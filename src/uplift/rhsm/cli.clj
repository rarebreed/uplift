;; Mostly contains screen scraping functions for the CLI

(ns uplift.rhsm.cli
  (:require [uplift.command :as cmdr]))

(defn- sm-list-consumed-
  "Retrieves all"
  [host]
  (let [[cmd result] (cmdr/launch "subscription-manager list --consumed" :host host)
        ]))
