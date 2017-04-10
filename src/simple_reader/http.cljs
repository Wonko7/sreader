(ns simple-reader.http
  (:require
    [cljs.nodejs :as nodejs]
    [simple-reader.feedreader :as fr]
    [cljs.core.async :refer [chan <! >!] :as a]
    [simple-reader.helpers :as h])
  (:require-macros [cljs.core.async.macros :as m :refer [go-loop go]]
                   [utils.macros :refer [<? <?? go? go-try dprint]]))

(defn init [request-feed-ch article-ans]
  (let [express (nodejs/require "express")
        app (new express)
        lol (atom "lol")
        request-feed (fn [req, res]
                       (let [params (merge {:nb 0} (-> req .-params h/to-clj))]
                         (go (println :received params)
                             (>! request-feed-ch params)
                             (println :requested params)
                             (.send res (h/to-js (<! article-ans))))))]

    (.use app "/" (.static express "resources/public/"))
    (.get app "/f/:feed/" request-feed)
    (.get app "/f/:feed/:nb" request-feed)

    ;app.use('/static', express.static('public'))

    ;; setup listen:
    (.listen app 3000 #(println "We're listening."))
    ))

	(comment (fn [req, res]
                     (println (-> req .-params h/to-clj))
                     (swap! lol #(str "lol " %))
                     (.send res (str (-> req .-params h/to-clj) "<br>" @lol))))
