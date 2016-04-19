(ns uplift.protos)

(defprotocol RepoManager
  "Functionality to install all needed dependencies for GUI testing"
  (enable-repos [this]))


(defprotocol SystemSetup
  "Functionality to call system services or other functionality"
  (disable-firewall [this] "Disables firewall on the system")
  (time-setup [this] "Sets up ntpd")
  (configure-vncserver [this] "Configures vnc-server for usage")
  (start-vncserver [this] "Starts the vnc-server"))
