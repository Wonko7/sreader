;(ns ^:figwheel-always simple-reader.core
(ns simple-reader.core
  (:require
    [cljs.nodejs :as nodejs]
    [simple-reader.feedreader :as fr]
    [cljs.core.async :refer [chan <! >!] :as a]
    [simple-reader.helpers :as h]
    [simple-reader.render :as html]
    [simple-reader.http :as http])
  (:require-macros [cljs.core.async.macros :as m :refer [go-loop go]]
                   [utils.macros :refer [<? <?? go? go-try dprint]]))

;; (enable-console-print!)
(nodejs/enable-util-print!)

(println "Starting stuff.")

(defn testing []
  (let [articles (chan)
        article-req (chan)
        article-ans (chan)
        hum-date (nodejs/require "human-date")
        ]
    (http/init article-req article-ans)
    (go-loop [{feed :feed nb :nb :as fixme} (<! article-req)]
             (println :getting fixme)
             ;was : 
             ;(fr/read "https://xkcd.com/atom.xml" articles)
             ;(>! article-ans (<! (html/render-articles articles)))
             (let [res (<! (html/render-articles articles))]
               (println :sending res)
               (>! article-ans res))

             )
    ;(fr/read "https://xkcd.com/atom.xml" articles)
    ;(go (println (<! (html/render-articles articles))))
    ;(go-loop [article articles]
    ;         (println)
    ;           (let [article (h/to-clj (<! article))]
    ;             (when (not= article :done)
    ;               (println (:title article))
    ;               (println (:link article))
    ;               (println (:description article))
    ;               (println (.prettyPrint hum-date (:date article)))
    ;               ;(println (:summary article))
    ;               ;(println (keys article))
    ;               (recur articles))))
    ))


(def -main testing)
(set! *main-cli-fn* -main)
