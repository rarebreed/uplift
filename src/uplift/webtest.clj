(ns uplift.webtest
  (:import [org.openqa.selenium.support.ui ExpectedCondition ExpectedConditions WebDriverWait]
           [org.openqa.selenium By WebElement]
           [org.openqa.selenium.remote RemoteWebDriver DesiredCapabilities]
           [java.net URL]))


(defn make-webdriver
  [remote-host]
  (let [url (URL. remote-host)]
    (RemoteWebDriver. url (DesiredCapabilities/firefox))))


(defn get-webpage
  [wd url]
  (.get wd url))