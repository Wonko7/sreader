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

(defn save-article [feed-id  ;; for now feed name
                    article
                    &
                    [override?]]
  (let [json-writer (json/writer :json)
        ;; my stuff:
        override?   (if (nil? override?) true override?) ;; if false isn't explicitly given, default to true
        art-id      (js/encodeURIComponent (:guid article))
        feed-dir    [(.homedir OS) (:root config) "feeds" feed-id]
        art-path    (.join Path (mk-dir? feed-dir) art-id)
        exists?     (.existsSync FS art-path)
        ]
    (if (and exists? (not override?))
      (println :feed feed-id :art (:title article) "already exists")
      (let [art-encoded (json/write json-writer (merge article-default-metadata article))]
        (println :feed feed-id :art (:title article) "written")
        (.writeFile FS art-path art-encoded #(when % (throw %))) ;; I don't see why we'd need  sync here.
        ))))

(defn read-feeds [feed-id]
  (let [json-reader (json/reader :json)
        feed-dir    (.join Path (.homedir OS) (:root config) "feeds" feed-id)
        exists?     (.existsSync FS feed-dir)]
    (when exists?
      (let [articles (->> feed-dir ;; transducers fixme
                          (.readdirSync FS)
                          (map #(.join Path feed-dir %))
                          (map #(.readFileSync FS %))
                          (map #(json/read json-reader %)))
            ]
        (doseq [a articles]
          (println (:title a) (:link a)))))))
