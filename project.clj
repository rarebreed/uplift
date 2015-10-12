

(defproject uplift "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [clj-ssh "0.5.11"]
                 [commons-net "3.3"]
                 [com.taoensso/timbre "3.3.1"]
                 [org.clojars.hozumi/clj-commons-exec "1.2.0"]
                 ;[org.clojure/tools.nrepl "0.2.10"]
                 [levand/immuconf "0.1.0"]
                 [clj-webdriver "0.7.2"]
                 [org.seleniumhq.selenium/selenium-server "2.47.1"]
                 [org.clojure/core.match "0.3.0-alpha4"]]

  :main uplift.repos)
