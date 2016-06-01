;; Deployment should be a decentralized process
;; A DeploymentServer will be the focal point for users to request a machine to be provisioned
;; A user will log into the DeploymentServer and then provide the requested version of RHEL to install
;; The DS will provision a new VM and provide the IP address for the new machine.  Once the IP address
;; of the VM is known, the uplift agent will be installed to it.  This includes installing a JVM on the
;; new VM.  Once this is done, the DS can talk to the uplift agent.  The uplift agent will do any
;; further provisioning of the system it is installed to.
;;
;; DeploymentServer tasks
;; 1. Provision a new VM
;; 2. Provide IP address of the new VM
;; 3. Install a JVM and leiningen on the VM
;; 4. Create a local repo file for the remote machine
;; 5. Install uplift to the VM
;;
;; Uplift agent tasks
;; 1. Install the ddnsclient for remote
;; 2. Get distro information
;; 3.
;; 4. Setup system time
;; 5. Install needed dependencies
;; 6. Setup hostname and ddns name
;; 7. Poll until hostname resolves
;; 8. Copy the id_auto keys from central to machine
;; 9. Copy the candlepin-ca.crt to machine


(ns uplift.deploy
  (:require [clj-webdriver.taxi :as cwt]
            [taoensso.timbre :as timbre]
            [uplift.core :as uc]
            [uplift.distro :as udis]
            [commando.command :refer [launch ssh which]]
            [uplift.utils.file-sys :as file-sys]
            [uplift.config.reader :as ucr]
            [uplift.repos :as ur]
            [schema.core :as s]
            [clojure.core.match :refer [match]]
            [uplift.protos :as uprotos]
            [uplift.distro :as ud]))

(def config (ucr/get-configuration))
(def uplift-git "https://github.com/RedHatQE/uplift.git")


(defn install-uplift
  "Install uplift including any dependencies"
  [host]
  {:post [#()]}
  (let [uplift-dir (get-in config [:config :uplift-dir])
        uplift? (file-sys/file-exists? uplift-dir :host host)
        parent-dir (.getParent (java.io.File. uplift-dir))
        uplift-result (if uplift?
                        (ssh host (format "cd %s; git pull" uplift-dir))
                        (let [install-devtools (when-not (which "git" :host host)
                                                 (uc/install-devtools host))
                              clone-result (when-not (file-sys/file-exists? parent-dir)
                                             (ssh host (format "mkdir -p %s" parent-dir))
                                             (uc/git-clone host uplift-git)
                                             (ssh host (format "mv uplift %s" parent-dir)))]
                          (ssh host (format "cd %s; lein deps" uplift-dir))))]
    {:result uplift-result :parent-dir parent-dir :uplift-dir uplift-dir}))


(defn copy-products
  "Copies the generated certs from candlepin to the host"
  [candle]

  )


(defn copy-ca-cert
  "Copies the /etc/candlepin/certs/candlepin-ca.crt to test machine"
  [candle]
  (let [src "/etc/candlepin/certs/candlein-ca.crt"
        dest "/etc/rhsm/ca/candlepin-ca.pem"]
    (file-sys/get-remote-file candle src :dest dest)))


(defn validate-system-arch
  [host]
  (let [base ["x86_64" "i386"]
        {:keys [distributor-id major release arch] :as di} (udis/distro-info host)
        distro (if (re-find #"RedHatEnterprise" distributor-id)
                 (keyword (str "rhel" major))
                 (keyword (str distributor-id release)))
        valid (match distro
                     :rhel7 base
                     :rhel6 (conj base "ppc64"))]
    (if (some (set [arch]) valid)
      di
      (throw (Exception. (format "%s not a valid arch for %s %s" (:arch di) distributor-id release))))))


(defn set-aliases
  "Adds 2 default aliases"
  [host]
  (uc/add-alias "rhsm-version" "rpm -qa | egrep \"python-rhsm|subscription\"" :host host)
  (uc/add-alias "newrhsm" "yum -y update subscription-manager* python-rhsm" :host host))


(defn get-ldtp-repo
  [host]
  (let [ldtp-repo (-> (:config (ucr/get-configuration)) :ldtp-repo-path)
        repo-file (slurp ldtp-repo)]
    (spit "/etc/yum.repos.d/ldtp.repo" repo-file)
    (ur/set-repo-enable "/etc/yum.repos.d/ldtp.repo" "ldtp" true)))


(defn system-setup
  [distro-info host]
  (let [rhel-type (udis/rhel-factory distro-info host)]
    (uprotos/time-setup rhel-type)
    (uprotos/disable-firewall rhel-type)))


(defn disable-gnome-initial-setup
  "Not sure if this works.  Tries to eliminate the gnome3 splash screen"
  [host]
  (let [distro (udis/distro-info host)
        _ (launch "mkdir -p /etc/skel/.config" :throws false :host host)
        _ (launch "mkdir -p /root/.config" :throws false :host host)
        skel-path "/etc/skel/.config/gnome-initial-setup-done"
        root-path "/root/.config/gnome-initial-setup-done"
        check-n-send #(when-not (file-sys/file-exists? %1 :host host)
                       (spit "/tmp/gnome-initial-setup-done" "yes")
                       (file-sys/send-file-to host "/tmp/gnome-initial-setup-done" :dest %2))]
    (check-n-send skel-path "/etc/skel/.config")
    (check-n-send root-path "/root/.config")))


(defn configure-vncserver
  "Pulls down vncserver file"
  ([cfg-file-path]
   (let [config (slurp cfg-file-path)
         dest "/etc/systemd/system/vncserver@:2.service"]
     (spit dest config)))
  ([]
    (configure-vncserver (:vncserver-path config))))


(defn bootstrap
  "Sets up a new VM with the minimum to kick everything else off

  1. Install repo file
  2. Install JVM
  3. Install leiningen"
  [host & {:keys [key-path auto-key-path ddns-uuid ddns-name]
           :or {key-path (get-in config [:config :ssh-pub-key])
                auto-key-path (get-in config [:config :ssh-pub-key-auto])}}]
  (let [copy-key-res (uc/copy-ssh-key host :key-path key-path)
        copy-autokey-res (uc/copy-ssh-key host :key-path auto-key-path)
        distro-info (udis/distro-info host)

        ;; Create the nightly repo file so we can install dependencies
        latest-repo "/tmp/rhel-latest.repo"
        _ (if (file-sys/file-exists? latest-repo)
            (file-sys/delete-file latest-repo))
        opts-fmt (if (= 7 (:major distro-info))
                   "%s-optional"
                   "%s/optional")
        repo-install-res (ur/make-default-nightly-repo-file host :dest latest-repo :opt opts-fmt :di distro-info)
        _ (when-not repo-install-res
            (file-sys/send-file-to host latest-repo :dest "/etc/yum.repos.d/"))
        _ (ur/enable-repos distro-info host)
        _ (launch "yum -y install wget" :host host)

        ;; install and configure redhat ddns
        _ (when ddns-uuid
            (uc/install-redhat-ddns :host host)
            (uc/edit-ddns-hosts host ddns-name ddns-uuid)
            (uc/ddns-client-enable host))

        ;; Do system setup to control firewall, set system time, etc
        system-res (system-setup distro-info host)

        ;; No point in continuing if we're not a supported arch
        _ (validate-system-arch host)

        ;; Set aliases and install dependencies
        ; _ (launch "pip install python-pillow")
        deps ["subscription-manager-migration*" "expect" "python-pip" "python-devel" "dogtail" "git"
              "translate-toolkit" "net-tools"]
        _ (uc/install-deps deps :host host)
        _ (launch "pip install coverage" :host host)
        _ (set-aliases host)

        ;; disable the gnome3 splash screen
        _ (when (= 7 (:major distro-info))
            (disable-gnome-initial-setup host))

        ;; Install java dependencies
        [_ java-major java-minor] (uc/check-java :host host)
        install-jdk-res (cond
                          (= java-major "0") (uc/install-jdk host 8)
                          (= java-major "7") nil
                          :else (timbre/logf :info (format "Java 1.%s_%s already installed" java-major java-minor)))

        ;; Install leiningen and verify
        lein-install-res (do
                           (uc/install-lein host "/usr/local/bin/lein")
                           (let [lein-check (launch "lein version" :host host :env {"LEIN_ROOT" "1"})]
                             (if (-> lein-check :status (= 0))
                               true
                               (throw (RuntimeException. "Unable to install leiningen")))))

        ;; Install repos for all the vnc server related deps
        rhel-type (ud/rhel-factory distro-info host)
        _ (ur/setup-auto-server-deps rhel-type)

        ;; configure vncserver settings
        _ (ud/configure-vncserver host)

        ;; Start the vncserver
        _ (uprotos/start-vncserver rhel-type)

        ;; FIXME: we need to install uplift and pheidippides
        uplift-res (install-uplift host)
        final-res {:copy-key-res     copy-key-res :respo-install-res repo-install-res :install-jdk-res install-jdk-res
                   :copy-autokey-res copy-autokey-res :lein-install-res lein-install-res :uplift-res uplift-res
                   :system-res       system-res}]
    (timbre/info "=========== Setup is complete!! ==================")
    (timbre/info final-res)))
