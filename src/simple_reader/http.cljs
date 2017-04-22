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
            request-feed-md-change-ch request-feed-md-change-ans
            request-tag-md-change-ch request-tag-md-change-ans
            ]
  (let [express       (nodejs/require "express")
        bodyparser    (nodejs/require "body-parser")
        app           (new express)
        request-feed  (fn [req res]
                       (let [params (merge {:nb 0} (-> req .-params h/to-clj))]
                         (go (>! request-feed-ch params)
                             (.send res (h/to-js (<! request-feed-ans))))))
        process-md    (fn [ans-ch req-ch req res]
                        (let [id       (-> req .-params h/to-clj)
                              metadata (-> req .-body h/to-clj)]
                          (go (>! req-ch {:id id :metadata metadata})
                              (.send res (h/to-js (<! ans-ch))))))]

    (.use app (.json bodyparser))
    (.use app (.urlencoded bodyparser (h/to-js {:extended true})))
    (.use app "/" (.static express "resources/public/"))
    (.get app "/f/:feed/" request-feed)
    (.get app "/f/:feed/:nb" request-feed)
    (.get app "/subs/" (fn [req res]
                         (go (>! request-subs-ch :req)
                             (.send res (h/to-js (<! request-subs-ans))))))
    (.post app "/a-md/:feed/:article" #(process-md request-article-md-change-ans request-article-md-change-ch %1 %2))
    (.post app "/f-md/:feed"          #(process-md request-feed-md-change-ans request-feed-md-change-ch %1 %2))
    (.post app "/t-md/:tag"           #(process-md request-tag-md-change-ans request-tag-md-change-ch %1 %2))

    ;; setup listen:
    (.listen app 3000 #(println "We're listening."))))
