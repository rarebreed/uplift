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
;; 3. Install a JVM and leiningen on the VM  --
;; 4. Install uplift to the VM
;;
;; Uplift agent tasks
;; 1. Create a local repo file for the remote machine
;; 2. Get distro information
;; 3. Install the ddnsclient for remote
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
            [uplift.command :refer [run ssh]]
            [uplift.utils.file-sys :as file-sys]
            [uplift.config.reader :as ucr]
            [uplift.repos :as ur])
  )

(def config (ucr/get-configuration))
(def uplift-git "https://github.com/RedHatQE/uplift.git")

(defn install-uplift
  [host]
  (let [uplift-dir (:uplift-dir config)
        uplift? (file-sys/file-exists? uplift-dir)]
    (if uplift?
      (ssh host (format "cd %s; git pull" uplift-dir))
      (do
        (core/git-clone host "https://github.com/RedHatQE/uplift.git")
        (ssh host "mkdir Projects")
        (ssh host "mv uplift Projects/")
        (ssh host "cd /root/Projects/uplift; lein deps")))))


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
  [host version]
  (ur/install-repos host version)
  ;; copy the public key

  (let [[_ major minor] (uc/check-java :host host)
        _ (cond
            (= major "0") (uc/install-jdk host 8)
            (= major "7") nil
            :else
            (timbre/logf :info (format "Java 1.%s_%s already installed" major minor)))
        uplift-dir (:uplift-dir (ucr/get-configuration))]
    ;; Install leiningen and verify
    (uc/install-lein host "/usr/local/bin/lein")
    (when-not (-> (ssh host "lein version") :exit (= 0))
      (throw (RuntimeException. "Unable to install leiningen")))
    ;; Install uplift
    (uc/install-devtools host)
    (uc/git-clone host uplift-git)
    ;; TODO: change that to lein run when it's ready
    (ssh host (format "cd %s; lein deps"))))