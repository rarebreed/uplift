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
            [uplift.core :as core]
            [uplift.core :as uc]
            [commando.command :refer [launch ssh which]]
            [uplift.utils.file-sys :as file-sys]
            [uplift.config.reader :as ucr]
            [uplift.repos :as ur]
            [schema.core :as s]))

(def config (ucr/get-configuration))
(def uplift-git "https://github.com/RedHatQE/uplift.git")


(defrecord RHEL7 [distro]
  uplift.protos/SystemSetup
  (install-deps [this]
    nil)

  uplift.protos/SystemSetup
  (firewallctl [this])
  (timectl [this]))

(defrecord RHEL6 [distro]
  uplift.protos/SystemSetup
  (install-deps [this]
    nil)

  uplift.protos/SystemSetup
  (firewallctl [this])
  (timectl [this]))


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
                                             (core/git-clone host uplift-git)
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
        [{:keys [variant distributor-id release major minor]
          :as distro-info}] (uc/remote-distro-info host)
        repo-install-res (ur/make-default-nightly-repo-file host)
        _ (ur/enable-repos distro-info)
        _ (launch "yum -y install wget" :host host)
        ;; install and configure redhat ddns
        _ (when ddns-uuid
            (uc/install-redhat-ddns :host host)
            (uc/edit-ddns-hosts host ddns-name ddns-uuid)
            (uc/ddns-client-enable host))


        [_ java-major java-minor] (uc/check-java :host host)
        install-jdk-res (cond
                          (= java-major "0") (uc/install-jdk host 8)
                          (= java-major "7") nil
                          :else (timbre/logf :info (format "Java 1.%s_%s already installed" java-major java-minor)))
        ;; Install leiningen and verify
        lein-install-res (do
                           (uc/install-lein host "/usr/local/bin/lein")
                           (let [lein-check (ssh host "lein version")]
                             (if (-> lein-check :exit (= 0))
                               true
                               (throw (RuntimeException. "Unable to install leiningen")))))
        ;; Install uplift
        uplift-res (install-uplift host)]
    {:copy-key-res copy-key-res :respo-install-res repo-install-res :install-jdk-res install-jdk-res
     :copy-autokey-res copy-autokey-res :lein-install-res lein-install-res :uplift-res uplift-res}))
