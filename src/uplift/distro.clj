(ns uplift.distro
  (:require [uplift.utils.file-sys :as file-sys]
            [commando.command :refer [launch]]
            [cheshire.core :as ches]
            [taoensso.timbre :as timbre]
            [uplift.protos :as protos]
            [uplift.utils.algos :refer [keywordize]])
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
    (let [info (-> (launch "lsb_release -a" :host host :throws true) :output)
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
                "systemctl start ntpd.service"
                "systemctl enable ntpd.service"]]
      (list (for [cmd cmds]
              (launch+ cmd))))))

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
                  (launch+ cmd))))))))

(defn rhel-factory
  [distro-info host]
  (let [{:keys [major]} distro-info]
    (cond
      (= 7 major) (RHEL7. distro-info host)
      (= 7 major) (RHEL6. distro-info host))))
