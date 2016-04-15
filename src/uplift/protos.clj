(ns uplift.protos)

(defprotocol RepoManager
  "Functionality to install all needed dependencies for GUI testing"
  (enable-repos [this]))


(defprotocol SystemSetup
  "Functionality to call system services or other functionality"
  (disable-firewall [this])
  (time-setup [this])
  (setup-automation-command-server [this]))
