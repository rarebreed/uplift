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
(def candlepin (get-in configuration [:candlepin :url]))
(def ^:dynamic insecure? true)
(def username (get-in configuration [:candlepin :user]))
(def password (get-in configuration [:candlepin :password]))

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


(defn make-request [url route user pw & {:keys [query-params]}]
  (let [request {:headers    {"Content-type" "application/json"
                              "Accept"       "application/json"}
                 :basic-auth [user pw]
                 :user-agent "uplift"
                 :insecure?  true
                 :query-params query-params
                 :url        (str url route)}]
    request))
