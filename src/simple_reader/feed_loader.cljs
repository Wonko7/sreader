(ns simple-reader.feed-loader
  (:require
    [cljs.nodejs :as node]
    [cljs.core.async :refer [chan <! >!] :as a]
    [cognitect.transit :as json]
    [simple-reader.helpers :as h])
  (:require-macros [cljs.core.async.macros :as m :refer [go-loop go]]
                   [utils.macros :refer [<? <?? go? go-try dprint]]))

(def  Path        (node/require "path"))
(def  FS          (node/require "fs"))
(def  OS          (node/require "os"))

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
        art-id      (js/encodeURIComponent (:guid article))
        feed-dir    [(.homedir OS) (:root config) "feeds" feed-id]
        art-path    (.join Path (mk-dir? feed-dir) art-id)
        exists?     (.existsSync FS art-path)
        ]
    (println art-path exists?)
    (if (and exists? (not override?))
      (println art-id "already exists")
      (let [art-encoded (json/write json-writer (merge article-default-metadata article))]
        (.writeFile FS art-path art-encoded #(when % (throw %))) ;; I don't see why we'd need  sync here.
        ))))
