(ns simple-reader.http
  (:require
    [cljs.nodejs :as nodejs]
    [simple-reader.feedreader :as fr]
    [cljs.core.async :refer [chan <! >!] :as a]
    [simple-reader.helpers :as h]
    [com.rpl.specter :as s :refer [setval select-one select transform filterer keypath pred ALL ATOM FIRST]])
  (:require-macros [cljs.core.async.macros :as m :refer [go-loop go]]
                   [utils.macros :refer [<? <?? go? go-try dprint]]))

(defn init [request-feed-ch request-feed-ans
            request-subs-ch request-subs-ans
            request-article-md-change-ch request-article-md-change-ans
            request-tag-md-change-ch request-tag-md-change-ans
            ]
  (let [express (nodejs/require "express")
        bodyparser (nodejs/require "body-parser")
        app (new express)
        request-feed (fn [req res]
                       (let [params (merge {:nb 0} (-> req .-params h/to-clj))]
                         (go (>! request-feed-ch params)
                             (.send res (h/to-js (<! request-feed-ans))))))]

    (.use app (.json bodyparser))
    (.use app (.urlencoded bodyparser (h/to-js {:extended true})))
    (.use app "/" (.static express "resources/public/"))
    (.get app "/f/:feed/" request-feed)
    (.get app "/f/:feed/:nb" request-feed)
    (.get app "/subs/" (fn [req res]
                         (go (>! request-subs-ch :req)
                             (.send res (h/to-js (<! request-subs-ans))))))
    (.post app "/md/:feed/:article" (fn [req res]
                                      (let [article-id (-> req .-params h/to-clj)
                                            article-id (transform [:article] js/encodeURIComponent article-id)
                                            metadata (-> req .-body h/to-clj)]
                                        (go (>! request-article-md-change-ch {:article-id article-id :metadata metadata})
                                            (.send res (h/to-js (<! request-article-md-change-ans))))))) ;; FIXME is h/to-js necessary?
    (.post app "/tag-md/:tag" (fn [req res]
                                (let [tag-id (:tag (-> req .-params h/to-clj))
                                      metadata (-> req .-body h/to-clj)]
                                  (go (>! request-tag-md-change-ch {:tag-id tag-id :metadata metadata})
                                      (.send res (h/to-js (<! request-tag-md-change-ans)))))))

    ;; setup listen:
    (.listen app 3000 #(println "We're listening."))))
