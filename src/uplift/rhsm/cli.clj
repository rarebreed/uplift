;; Mostly contains screen scraping functions for the CLI

(ns uplift.rhsm.cli
  (:require [commando.command :as cmdr]
            [commando.protos.protos :refer [output]]))

(defn- sm-list-consumed-
  "Retrieves all"
  [host]
  (let [result (cmdr/launch "subscription-manager list --consumed" :host host)]
    (:output result)))
