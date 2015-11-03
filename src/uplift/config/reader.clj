(ns uplift.config.reader
  (:require [immuconf.config :as cfg]))


(defn get-configuration
  []
  (let [home (System/getProperty "user.home")
        usr-cfg (get (cfg/load "resources/dev.edn") :user-config)
        user-config (str home usr-cfg)]
    {:home home
     :user-config user-config
     :config (cfg/load "resources/properties.edn" user-config)}))