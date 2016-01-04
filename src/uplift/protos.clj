(ns uplift.protos)

(defprotocol RepoManager
  "Functionality to install all needed dependencies for GUI testing"
  (enable-repos [this]))


(defprotocol SystemSetup
  "Functionality to call system services or other functionality"
  (timectl [this])
  (firewallctl [this]))
