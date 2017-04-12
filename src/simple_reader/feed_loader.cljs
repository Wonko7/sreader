(ns simple-reader.feed-loader
  (:require
    [cljs.nodejs :as node]
    [cljs.core.async :refer [chan <! >!] :as a]
    [cognitect.transit :as json]
    [simple-reader.helpers :as h])
  (:require-macros [cljs.core.async.macros :as m :refer [go-loop go]]
                   [utils.macros :refer [<? <?? go? go-try dprint]]))

(def Path (node/require "path"))
(def FS   (node/require "fs"))
(def OS   (node/require "os"))

(def article-default-metadata {:metadata {:read false}})

(def config {:root ".sreader/"})

(defn mk-dir? [path]
  (loop [cur (first path) [p & ps] (rest path)]
    (when (not (.existsSync FS cur))
      (.mkdirSync FS cur))
    (if p
      (recur (.join Path cur p) ps)
      cur)))

(defn mk-root-path [& dirs]
  (let [dirs (concat [(.homedir OS) (:root config)] dirs)]
    (apply (partial Path.join) dirs)))

(defn mk-feed-dir [feed-md] ;; feed-md has :name, :url, :type. :tags
  (let [fname   (:name feed-md)
        fpath   (mk-root-path "feeds" (js/encodeURIComponent fname))
        f-md    (.join Path fpath "feed-metadata")]
        (println f-md)
    (if (.existsSync FS fpath)
      (println :mk-feed :error name "already exists")
      (do (.mkdirSync FS fpath)
          (.writeFileSync FS f-md (h/write-json feed-md))))))

(defn load-feed-md [dir]
  (let [path  (mk-root-path "feeds" dir "feed-metadata")
        md    (h/read-json (.readFileSync FS path))]
    {(:name md) (merge {:dir dir} md)}))

(defn load-feeds-md []
  (let [froot     (mk-root-path "feeds")
        feed-dirs (.readdirSync FS froot)]
    (into {} (map load-feed-md feed-dirs))))

(defn save-article [feed-id  ;; for now feed name
                    article
                    &
                    [override?]]
  (let [write-json-file #(.writeFileSync FS % (json/write (json/writer :json) %2))
        override?       (if (nil? override?) true override?) ;; if false isn't explicitly given, default to true
        art-id          (js/encodeURIComponent (:guid article))
        feed-dir        [(.homedir OS) (:root config) "feeds" feed-id art-id]
        feed-dir        (mk-dir? feed-dir)
        art-path        (.join Path feed-dir "entry")
        md-path         (.join Path feed-dir "metadata")
        exists?         (.existsSync FS art-path)]
    (if (and exists? (not override?))
      (println :feed feed-id :art (:title article) "already exists")
      (do (println :feed feed-id :art (:title article) "written")
          (write-json-file art-path (dissoc article :metadata))
          (when-let [md (:metadata article)]
            (write-json-file md-path md))
          ))))

(defn read-feed [feed-id]
  (let [read-json-file  #(json/read (json/reader :json) (.readFileSync FS %))
        feed-dir        (.join Path (.homedir OS) (:root config) "feeds" feed-id)
        exists?         (.existsSync FS feed-dir)
        load-art        (fn [art-dir]
                          (let [art-dir (.join Path feed-dir art-dir)
                                is-dir? (.isDirectory (.statSync FS art-dir))]
                            (when is-dir?
                              (let [entry-path (.join Path art-dir "entry")
                                    md-path    (.join Path art-dir "metadata")
                                    article    (read-json-file entry-path)]
                                (if (.existsSync FS md-path)
                                  (merge article {:metadata (read-json-file md-path)})
                                  article)))))]
    (when exists?
      (->> feed-dir
           (.readdirSync FS)
           (map load-art)
           (filter identity)))))

(defn read-article-md [feed article]
  (let [md-path (.join Path (.homedir OS) (:root config) "feeds" feed article "metadata")
        exists? (.existsSync FS md-path)]
    (if exists?
      (h/read-json (.readFileSync FS md-path))
      {})))

(defn write-article-md [feed article md]
  (let [md-path (.join Path (.homedir OS) (:root config) "feeds" feed article "metadata")]
    (.writeFileSync FS md-path (h/write-json md))))


(defn read-tag-md [tag]
  (println :tag tag)
  (let [md-path (mk-root-path "tags" tag "metadata")
        exists? (.existsSync FS md-path)]
    (if exists?
      (h/read-json (.readFileSync FS md-path))
      {})))

(defn read-tags-md []
  (let [tpath (mk-root-path "tags")
        tags (.readdirSync FS tpath)]
    (into {} (for [tag tags]
               {tag (read-tag-md tag)}))))

(defn write-tag-md [tag md]
  (let [tag-dir (mk-root-path "tags" tag)
        md-path (mk-root-path "tags" tag "metadata")
        exists? (.existsSync FS tag-dir)]
    (when-not exists?
      (.mkdirSync FS tag-dir))
    (.writeFileSync FS md-path (h/write-json md))))
