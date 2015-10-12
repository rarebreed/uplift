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
            [uplift.core :as core])
  )


(defn copy-products
  "Copies the generated certs from candlepin to the host"
  [candle]

  )

(defn copy-ca-cert
  "Copies the /etc/candlepin/certs/candlepin-ca.crt to test machine"
  [candle]
  (let [src "/etc/candlepin/certs/candlein-ca.crt"
        dest "/etc/rhsm/ca/candlepin-ca.pem"]
    (core/get-remote-file candle src :dest dest)))


(defn install-deps
  []
  (let [distro-info (core/distro-info)   ;; Figure out what version we are on
        repo-file ()]                    ;; Install repo file if it's not there

    )

  )