(defproject uplift "0.1.0-SNAPSHOT"
  :description (str "A clojure library to help setup development environments for "
                 "Red Hat Subscription Manager testing")
  :url "https://github.com/RedHatQE/uplift"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [clj-ssh "0.5.11"]
                 [commons-net "3.3"]
                 [com.taoensso/timbre "4.1.4"]
                 [org.clojars.hozumi/clj-commons-exec "1.2.0"]
                 [levand/immuconf "0.1.0"]
                 [clj-webdriver "0.7.2"]
                 [org.seleniumhq.selenium/selenium-server "2.47.1"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [prismatic/schema "1.0.1"]
                 [org.clojure/tools.cli "0.3.3"]
                 [org.clojure/core.async "0.2.371"]
                 [danlentz/clj-uuid "0.1.6"]
                 [http-kit "2.1.18"]
                 [cheshire "5.5.0"]
                 [commando "0.1.2-SNAPSHOT"]
                 [clj-time "0.11.0"]
                 [org.testng "6.9.10"]]

  :java-source-paths ["src"]
  :aot :all)
