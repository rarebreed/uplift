;; This namespaces contains functionality to parse through a repo file as well as generate new
;; repo files

(ns uplift.repos
  (:require [clojure.java.io :as cjio]
            [uplift.utils.file-sys :as file-sys]
            [uplift.config.reader :as ucr]
            [uplift.config :as ucfg]))

(def latest-rhel7-server "[latest-rhel7-server]") 
(def latest-rhel7-server-optional "[latest-rhel7-server-optional]")
(def latest-rhel7-server-debuginfo "[latest-rhel7-server-debuginfo]")
(def devconfig (ucr/get-configuration))
(def user-config (:user-config devconfig))
(def config (:config devconfig))
(def url-format (get config :url-format))


(defprotocol ToConfig
  (write-to-config [this filename] "Creates a config file representation"))


(defn- write-conf [obj fname]
  (letfn [(mkstr [key]
            (let [keyname (clojure.core/name key)
                  val (key obj)]
              (str keyname "=" val "\n")))]
    (with-open [newfile (cjio/writer fname :append true)]
      (.write newfile (:reponame obj))
      (.newLine newfile)
      (doseq [line (map mkstr [:name :baseurl :enabled :gpgcheck])]
        (.write newfile line))
      (.newLine newfile))))


(defrecord YumRepo
    [^String reponame  ;; The section eg [latest-rhel7-server
     ^String name      ;; description of repo
     ^String baseurl   ;; the baseurl to pull down content
     ^String enabled   ;; boolean (0 or 1 or True|False)
     ^String gpgcheck  ;; Boolean to decide to check gpg key
     ]
  ToConfig
  (write-to-config [this filename]
    (write-conf this filename)))


(defn build-url-rhel
  "Creates a url based on the url-format string"
  [url-fmt & {:keys [rtype version flavor arch debug]
               :as opts
               :or {rtype :rel-eng
                    version "7.2"
                    flavor "Server"
                    arch "x86_64"
                    debug false}}]
  (println "in build-url")
  (doseq [[k v] opts]
    (println k "=" v))
  (let [type (name rtype)
        repod (if debug "debug/tree" "os")]
    (format url-fmt type version flavor arch repod)))


(defn make-yum-repo
  "Creates a YumRepo record which can be used to create a repo file"
  [rtype version repo & {:keys [url url-fmt flavor arch enabled gpgcheck description debug]
                         :as opts
                         :or {url nil
                              url-fmt url-format
                              flavor "Server"
                              arch "x86_64"
                              enabled "1"
                              gpgcheck "0"
                              debug false}}]
  (println "in make-base-server")
  (doseq [[k v] opts]
    (println k "=" v))
  (let [url (if url
              url
              (build-url-rhel url-fmt :rtype rtype :version version :flavor flavor :arch arch :debug debug))]
    (println url)
    (-> {:reponame repo
         :name (if description
                 description
                 "latest-RHEL7 Server from download.devel.redhat.com")
         :baseurl url
         :enabled enabled
         :gpgcheck gpgcheck}
        map->YumRepo)))


;; TODO Make a macro to autogenerate 
(defn latest-rel-eng-server
  "Convenience function to make latest repo"
  [version]
  (make-yum-repo :rel-eng version latest-rhel7-server))


(defn latest-released-server
  [version]
  (make-yum-repo :released version latest-rhel7-server))


(defn latest-nightly-server
  [version]
  (make-yum-repo :nightly version latest-rhel7-server))


(defn make-default-repo-file
  "Creates a repo file in /etc/yum.repos.d/rhel-latest.repo"
  [version & {:keys [fpath clear]
              :or {fpath "/etc/yum.repos.d/rhel-latest.repo"
                   clear false}}]
  (if (file-sys/repo-file-exists? :repo-file fpath)
    "rhel-latest.repo already exists"
    (let [latest (latest-rel-eng-server version)
          latest-optional (make-yum-repo :rel-eng version latest-rhel7-server-optional :flavor "Server-optional")
          latest-debuginfo (make-yum-repo :rel-eng version latest-rhel7-server-debuginfo :enabled 0 :debug true)]
      (write-to-config latest fpath)
      (write-to-config latest-optional fpath)
      (write-to-config latest-debuginfo fpath)))
  (println (slurp fpath)))


(defn get-page [url]
  (slurp url))


(defn make-dotted-version-regex
  [version]
  (let [base (clojure.string/replace version #"\." "\\\\.")
        left ">([a-zA-Z0-9._-]*"
        right "[a-zA-Z0-9._/-]*)<"
        final (str left base right)]
    (re-pattern final)))


(defn scrape
  "Some sites dont have a REST API so here's a dumb regex to look for some version
   from the page retrieved from a mirror site

   Usage:
     (scrape (slurp \"http://some.site.com\") #\"RHEL-7.1\""
  [page-source pattern]
  ;; Just a dumb regex that scans an html page
  (let [matched (re-seq pattern page-source)]
    ;; we only want the second in each
    (map #(second %) matched)))


(defn find-all
  ""
  [url version]
  (let [patt (make-dotted-version-regex version)
        page (get-page url)]
    (scrape page patt)))


(defn make-links [url version]
  (let [sep (if (not= (last url) \/) "/" "")]
    (for [version (find-all url version)]
      (str url sep version))))


(defn install-repos
  [host version]
  ;; TODO: check if rhel-latest.repo already exists on remote host
  ;; If not, create one locally, then scp it to remote
  (if (file-sys/repo-file-exists? :host host)
    "rhel-latest.repo already exists"
    (let [_ (make-default-repo-file version :fpath "/tmp/rhel-latest.repo" :clear true)]
      (file-sys/send-file-to host "/tmp/rhel-latest.repo" :dest "/etc/yum.repos.d"))))


(defn validate-enabled
  [new-repo section]
  (let [cmap (ucfg/get-conf-file new-repo)
        matches (ucfg/get-conf-key cmap "enabled" section)
        ;; filter only matches where there are no comments as there should only be one
        filtered (filter (fn [entry]
                           (let [[index repo-section] entry]
                             (and (= "enabled" (:key repo-section))
                                  (nil? (:comment repo-section)))))
                         matches)]
    (if (not= (count filtered) 1)
      false
      (= "0" (-> (first filtered) second (:value))))))


(defn repo-enablement
  "Enables or disables a given repo file

  *Args*
  - host: IP address or hostname
  - repo: path to a repo file
  - enabled?: if true enable repo, if false, disable repo
  - dest-path: path where new file will be written (defaults to same as repo)"
  [repo section key value & {:keys [dest-path host]}]
  (let [repo (if host
               (file-sys/get-remote-file host repo)
               repo)
        dest-path (if dest-path dest-path repo)
        cmap (ucfg/set-conf-file repo key value :section section)
        edited-repo (ucfg/vec-to-file cmap dest-path)]
    (if host
      (file-sys/send-file-to repo dest-path)
      edited-repo)))


(defn repo-enable
  [repo section enabled? & args]
  (repo-enablement repo section "enabled" (if enabled? "1" "0")) args)


(defn gpgcheck-enable
  [repo section enabled? & args]
  (repo-enablement repo section "gpgcheck" (if enabled? "1" "0")) args)
