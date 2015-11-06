;; Contains clojure API calls to candlepin, and high level routines

(ns uplift.candlepin.candlepin
  (:require [org.httpkit.client :as http]
            [cheshire.core :as cheshire]
            [uplift.config.reader :as ucr]
            [uplift.utils.file-sys :as fs]))

(def headers {"Content-type" "application/json"
              "Accept" "application/json"})
(def user-agent {:user-agent "uplift"})
(def configuration (:config (ucr/get-configuration)))
(def candlepin (:candlepin configuration))
(def ^:dynamic insecure? true)

(def routes
  {:users "/users"
   :status "/status"})


(defrecord Request
  [method
   url
   headers
   insecure?
   user-agent
   basic-auth
   query-params
   ])


(let [request {:headers    {"Content-type" "application/json"
                            "Accept"       "application/json"}
               :basic-auth ["admin" "admin"]
               :user-agent "uplift"
               :insecure?  true
               :url "https://f22-candle.usersys.redhat.com:8443/candlepin/status"}]
  @(http/request request))
