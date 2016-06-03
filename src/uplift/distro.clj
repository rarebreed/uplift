(ns uplift.distro
  (:require [uplift.utils.file-sys :as file-sys]
            [commando.command :refer [launch]]
            [cheshire.core :as ches]
            [taoensso.timbre :as timbre]
            [uplift.core :as uc]
            [uplift.protos :as protos]
            [uplift.utils.algos :refer [keywordize]]
            [uplift.config.reader :as ucr])
  (:import (uplift.protos SystemSetup)))


(defrecord Distro
  [distributor-id
   release
   variant
   major
   minor
   arch])


(def yum-script
  ["import yum"
   "import json"
   "import platform"
   "import copy"
   "yb = yum.YumBase()"
   "d_id, release, _ = platform.linux_distribution()"
   "temp = copy.deepcopy(yb.conf.yumvar)"
   "temp['release'] = release"
   "temp['id'] = d_id"
   "yb_s = json.dumps(temp)"
   "print yb_s"])


(defn yum-base
  "Copies python script to remote host and executes it"
  [host]
  (spit "yum_base.py" (clojure.string/join "\n" yum-script))
  (file-sys/send-file-to host "yum_base.py")
  (when (file-sys/file-exists? "yum_base.py")
    (file-sys/delete-file "yum_base.py"))
  (let [result (launch "python yum_base.py" :host host)
        output (second (clojure.string/split (:output result) #"\n"))
        yum (ches/parse-string output true)]
    yum))


(defn yum->Distro
  [yum-output]
  (let [[_ major variant] (re-find #"^(\d+)(\w+)" (:releasever yum-output))
        [_ minor] (clojure.string/split (:release yum-output) #"\.")
        keys {:major          (Integer/parseInt major)
              :minor          (Integer/parseInt minor)
              :release        (:release yum-output)
              :variant        variant
              :distributor-id (-> (clojure.string/replace (:id yum-output) #"Linux" "")
                                  (clojure.string/replace #" " ""))
              :arch           (:basearch yum-output)}]
    (map->Distro keys)))

;; Ughhh, Java doesn't have a good way to get distro information.  So
;; we will scrape it from lsb_release -a
(defn distro-info
  "Runs lsb_release -a and gets the name, version, and variant type:
   {:distributor-id name of the distro (eg Fedora or RHEL)
    :release version number (eg 22 or 7.2)
    :variant type of OS (eg workstation or server)
    :major major release (as integer)
    :minor minor release (as integer)}"
  [host]
  (try
    (let [info (-> (launch "lsb_release -a" :host host :throws true :show-out? false) :output)
          pattern "^%s:\\s*(.*)$"
          variant-patt #"\w+(Server|Client|Workstation)\w*"
          ;; Create parsers to match NAME, VERSION_ID and VARIANT_ID
          parsers (for [x ["Distributor ID" "Release"]]
                    {(keywordize x) (re-pattern (format pattern x))})
          ;; Run each regex on each line, return ([:NAME match?])
          matches (for [parser parsers
                        line (clojure.string/split info #"\n")]
                    (let [keyname (first (keys parser))
                          val (first (vals parser))
                          _ (timbre/debug "Testing: " line " with " val)]
                      [keyname (re-find val line)]))
          ;; Only get the elements in the seq where the second element isn't nil
          filtered (filter (fn [%] (if (second %) true nil)) matches)
          ;; passed to reduce to return our final map
          finalfn (fn [coll entry]
                    (merge coll (let [f (first entry)
                                      [whole value] (second entry)]
                                  (hash-map f value))))
          m (reduce finalfn {} filtered)
          variant (second (re-find variant-patt (:distributor-id m)))
          [_ major minor] (re-find #"(\d+)\.?(\d*)" (:release m))
          arch (-> (launch "uname -m" :host host) :output (clojure.string/trim-newline))
          distro (map->Distro (merge m {:variant variant :major (Integer/parseInt major)
                                        :minor   (Integer/parseInt minor) :arch arch}))]
      distro)
    (catch Exception ex
      ;; We've got RHEL 6, so use yum-base instead
      (yum->Distro (yum-base host)))))


(defn configure-vncserver
  "Pulls down vncserver file"
  ([host cfg-file-path]
   (let [config (slurp cfg-file-path)
         tmp "/tmp/vncserver@:2.service"
         dest "/etc/systemd/system/"]
     (spit tmp config)
     (when-not (file-sys/file-exists? (str dest "vncserver@:2.service") :host host)
       (file-sys/send-file-to host tmp :dest dest))))
  ([host]
   (configure-vncserver host (-> (ucr/get-configuration) :config :vncserver-path))))

(defn vncpasswd
  [host]
  (when-not (file-sys/file-exists? "/root/.vnc/passwd" :host host)
    (let [cmd "vncpasswd << EOF\npassword\npassword\nEOF"]
      (launch cmd :host host))))

(defn xstartup
  [host]
  (when-not (file-sys/file-exists? "/root/.vnc/xstartup")
    (letfn [(launch+ [cmd] (launch cmd :host host))]
      (launch+ "vncserver :2")
      (Thread/sleep 10000)
      (launch+ "vncserver -kill :2")
      (Thread/sleep 10000))))

(defn autostart
  [host]
  (when-not (file-sys/file-exists? "/root/.config/autostart")
    (launch "mkdir -p /root/.config/autostart" :host host)))


(defn start-ldtpd
  [host]
  (let [root "/root/bin"
        script "/start-ldtpd.sh"
        exists? #(file-sys/file-exists? % :host host)]
    (when-not (exists? root)
      (launch (format "mkdir -p %s" root) :host host))
    (when-not (exists? (str root script))
      (let [ldtpd (-> (ucr/get-configuration) :config :start-ldtpd)
            dest (str root script)
            tmp (str "/tmp" script)]
        (spit tmp (slurp ldtpd))
        (file-sys/send-file-to host tmp :dest dest)
        (launch (format "chmod +x %s" dest) :host host)))))


(defn gnome-terminal-desktop
  [host]
  (let [path "/root/.config/autostart"
        f "/gnome-terminal.desktop"
        e? #(file-sys/file-exists? % :host host)]
    (when-not (e? path)
      (launch (format "mkdir -p %s" path) :host host))
    (when-not (e? (str path f))
      (let [desktop (-> (ucr/get-configuration) :config :gnome-desktop)
            tmp (str "/tmp" f)
            dest (str path f)]
        (spit tmp (slurp desktop))
        (file-sys/send-file-to host tmp :dest dest)))))

(defn gconftool-2
  [host]
  (let [cmd "gconftool-2 -s /apps/%s --type=%s %s"
        opts [["gnome-session/options/show_root_warning" "boolean" "false"]
              ["gnome-screensaver/idle_activation_enabled" "boolean" "false"]
              ["gnome-power-manager/ac_sleep_display" "int" "0"]]]
    (doseq [opt opts]
      (let [full-cmd (apply format cmd opt)]
        (launch full-cmd :host host)))))

(defn final-vnc-start-7
  [host]
  (letfn [(call [cmd] (launch cmd :host host :throws? false))]
    (call "killall -9 Xvnc")
    (call "rm -f /tmp/.X2-lock")
    (call "rm -f /tmp/.X11-unix/X2")
    (when (not= "gnome-classic" (:output (call "gsettings get org.gnome.desktop.session session-name")))
      (call "gsettings set org.gnome.desktop.session session-name gnome-classic"))
    (call "systemctl start vncserver@:2.service")))


(defn final-vnc-start-6
  [host]
  (let [servers (:output (launch "ls -A /etc/sysconfig/vncservers"))
        setting "VNCSERVERS=\"2:root\""
        matches (re-find (re-pattern setting) servers)]
    (when (empty? matches)
      (let [tmp "/tmp/vncservers"]
        (spit tmp setting)
        (file-sys/send-file-to host tmp :dest "/etc/sysconfig/vncservers"))
      (launch "chkconfig vncserver on" :host host)
      (launch "service vncserver restart" :host host))))


(defn gnome-configuration
  [host]
  (let [settings [["/apps/gnome-session/options/show_root_warning" "boolean" "false"]
                  ["/apps/gnome-screensaver/idle_activation_enabled" "boolean" "false"]
                  ["/apps/gnome-power-manager/ac_sleep_display" "int" 0]]]
    (vec (for [args settings]
           (apply uc/set-gconftool2 (flatten (conj args [:host host])))))))

(defn common-setup
  [host]
  (vec (for [fnc [vncpasswd xstartup autostart start-ldtpd gnome-terminal-desktop gconftool-2]]
         (fnc host))))

(defrecord RHEL6
  [distro host]

  SystemSetup
  (protos/disable-firewall [this]
    (let [host (:host this)]
      (launch "service iptables stop" :host host)
      (launch "chkconfig iptables off" :host host)))

  (protos/time-setup [this]
    (let [launch+ #(launch % :host host)
          cmds ["service ntpd stop"
                "ntpdate clock.redhat.com"
                "service ntpd start"
                "chkconfig ntpd enable"]]
      (list (for [cmd cmds]
              (launch+ cmd)))))

  (protos/start-vncserver [this]
    (common-setup (:host this))
    (final-vnc-start-6 (:host this))))

(defrecord RHEL7
  [distro host]

  SystemSetup
  (protos/disable-firewall [this]
    (let [host (:host this)]
      (launch "systemctl stop firewalld.service" :host host)
      (launch "systemctl disable firewalld.service" :host host)))

  (protos/time-setup [this]
    (let [ntpd? (fn []
                  (let [services (launch "systemctl list-unit-files" :host host)]
                    (re-find #"ntpd.service\s+enabled" (:output services))))
          launch+ #(launch % :host host)]
      (when-not (ntpd?)
        (let [cmds ["yum install -y ntp"
                    "rpm -q ntp"
                    "systemctl stop ntpd.service"
                    "ntpdate clock.redhat.com"
                    "systemctl start ntpd.service"
                    "systemctl enable ntpd.service"]]
          (list (for [cmd cmds]
                  (launch+ cmd)))))))

  (protos/start-vncserver [this]
    (let [host (:host this)
          launch+ #(launch % :host host)]
      (for [cmd ["systemctl daemon-reload"
                 "systemctl enable vncserver@:2.service"]]
        (launch+ cmd))
      (common-setup host)
      (final-vnc-start-7 host))))


(defn rhel-factory
  [distro-info host]
  (let [{:keys [major]} distro-info]
    (cond
      (= 7 major) (RHEL7. distro-info host)
      (= 6 major) (RHEL6. distro-info host))))
