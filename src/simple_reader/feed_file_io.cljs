(ns simple-reader.feed-file-io
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


;;;;;;;;;; helpers:

(defn read-json-file [path]
  (h/read-json (.readFileSync FS path)))

(defn write-json-file [path data]
  (.writeFileSync FS path (h/write-json data)))

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

(defn mk-feed-dir [feed-md] ;; FIXME: unused.
  (let [fname   (:name feed-md)
        fpath   (mk-root-path "feeds" (js/encodeURIComponent fname))
        f-md    (.join Path fpath "feed-metadata")]
    (if (.existsSync FS fpath)
      (println "fio:" :mk-feed :error name "already exists")
      (do (.mkdirSync FS fpath)
          (write-json-file f-md feed-md)))))


;;;;;;;;;; article MD:

(defn- read-data-file [[& path]]
  (let [md-path (apply mk-root-path path)
        exists? (.existsSync FS md-path)]
    (if exists?
      (read-json-file md-path)
      {})))

(defn- write-data-file [[& path] md]
  (let [md-path (apply mk-root-path path)]
    (write-json-file md-path md)))

(defn- read-article-md [feed article]
  (read-data-file ["feeds" feed article "metadata"]))

(defn- write-article-md [feed article md]
  (write-data-file ["feeds" feed article "metadata"] md))

(defn- read-article-scraped [feed article]
  (read-data-file ["feeds" feed article "scraped"]))

(defn- write-article-scraped [feed article md]
  (write-data-file ["feeds" feed article "scraped"] md))

(defn load-article-md [feed article]
  (read-article-md (js/encodeURIComponent feed) article))

(defn save-article-md [feed article md]
  (write-article-md (js/encodeURIComponent feed) (js/encodeURIComponent article) md))

(defn load-article-scraped [feed article]
  (read-article-scraped (js/encodeURIComponent feed) article))

(defn save-article-scraped [feed article md] ;; unused for now
  (write-article-scraped (js/encodeURIComponent feed) (js/encodeURIComponent article) md))

(defn load-feed-md [feed]
  (read-data-file ["feeds" (js/encodeURIComponent feed) "metadata"]))

(defn save-feed-md [feed md]
  (write-data-file ["feeds" (js/encodeURIComponent feed) "metadata"] md))

(defn load-tag-md [tag]
  (read-data-file ["tags" tag "metadata"]))

(defn load-tags-md []
  (let [tpath (mk-root-path "tags")
        tags (.readdirSync FS tpath)]
    (into {} (for [tag tags]
               {tag (load-tag-md tag)}))))

(defn save-tag-md [tag md]
  (let [tag-dir (mk-root-path "tags" tag)
        md-path (mk-root-path "tags" tag "metadata")
        exists? (.existsSync FS tag-dir)]
    (when-not exists?
      (.mkdirSync FS tag-dir))
    (write-json-file md-path md)))


;;;;;;;;;; feeds:

(defn- count-unread [feed]
  "returns [count-unread count-saved] for given feed"
  (->> (mk-root-path "feeds" feed)
       (.readdirSync FS)
       (map #(read-article-md feed %))
       (reduce (fn [[u s] md]
                 (cond (= "unread" (:status md)) [(inc u) s]
                       (= "saved" (:status md)) [u (inc s)]
                       :else [u s]))
               [0 0])))

(defn load-feeds-md []
  (let [load-feed-md  (fn [dir]
                        (let [path           (mk-root-path "feeds" dir "feed-metadata")
                              md             (read-json-file path)
                              [unread saved] (count-unread dir)]
                          {(:name md) (merge {:dir dir :unread-count unread :saved-count saved} md)}))
        froot         (mk-root-path "feeds")
        feed-dirs     (.readdirSync FS froot)]
    (into {} (map load-feed-md feed-dirs))))


(defn save-article [feed article scraped]
  (let [feed-id         (js/encodeURIComponent feed)
        art-id          (js/encodeURIComponent (:guid article)) ;; FIXME all js/encodeURIComponent should be in here, not in core.
        feed-dir        [(.homedir OS) (:root config) "feeds" feed-id art-id]
        feed-dir        (mk-dir? feed-dir)
        art-path        (.join Path feed-dir "entry")
        md-path         (.join Path feed-dir "metadata")
        exists?         (.existsSync FS art-path)]
    (let [new-md (:metadata article)
          def-md {:status "unread"}  ;; fixme; read? false is default MD, shall be in config somewhere
          cur-md (read-article-md feed-id art-id)]
      (write-json-file art-path (dissoc article :metadata))
      (write-article-md feed-id art-id (merge def-md cur-md new-md))
      (when scraped
        (write-article-scraped feed-id art-id scraped)))))


(defn load-feed [feed]
  (let [feed-id         (js/encodeURIComponent feed)
        feed-dir        (.join Path (.homedir OS) (:root config) "feeds" feed-id)
        exists?         (.existsSync FS feed-dir)
        load-art        (fn [art-id]
                          (let [art-dir (.join Path feed-dir art-id)
                                is-dir? (.isDirectory (.statSync FS art-dir))]
                            (when is-dir?
                              (let [entry-path (.join Path art-dir "entry")
                                    md         (read-article-md feed-id art-id)
                                    scraped    (read-article-scraped feed-id art-id)
                                    article    (read-json-file entry-path)]
                                (merge article {:metadata md :scraped scraped})
                                ))))]
    (when exists?
      (->> feed-dir
           (.readdirSync FS)
           (map load-art)
           (filter identity)))))
