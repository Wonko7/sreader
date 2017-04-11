;(ns ^:figwheel-always simple-reader.core
(ns simple-reader.core
  (:require
    [cljs.nodejs :as nodejs]
    [simple-reader.feedreader :as fr]
    [cljs.core.async :refer [chan <! >!] :as a]
    [simple-reader.helpers :as h]
    [simple-reader.render :as html]
    [cognitect.transit :as json]
    [simple-reader.feed-loader :as io]
    [simple-reader.http :as http])
  (:require-macros [cljs.core.async.macros :as m :refer [go-loop go]]
                   [utils.macros :refer [<? <?? go? go-try dprint]]))

(nodejs/enable-util-print!)

(defn testing []
  (let [article-req (chan)
        article-ans (chan)
        ]
    (http/init article-req article-ans)
    ;; read feeds web client:
    (go-loop [{feed :feed nb :nb :as fixme} (<! article-req)]
             (println :getting fixme)
             (let [json-writer (json/writer :json) ;; fixme put these in helpers
                   f           (json/write json-writer {:feed-data {:title feed} :articles (sort-by :date (io/read-feed feed))})
                   ]
               (>! article-ans f)
               (recur (<! article-req))))

    ;(let [] (println (js/Date "22/11/1963")))

    ;; scrape subscriptions
    (let [subs []
          subss [{:link "https://xkcd.com/atom.xml" :name "xkcd"}
                {:link "http://rss.slashdot.org/Slashdot/slashdotMainatom" :name "SlashDot"}]]
      (doseq [{link :link name :name} subs]
             (let [articles (chan)]
               (println "fetching" name)
               (fr/read link articles)
               (go-loop [to-save (<! articles)]
                        (if (not= :done to-save)
                          (do 
                            (io/save-article name to-save)
                            (recur (<! articles)))
                          (println :godone :on name))
                        ))))

    ;(io/read-feeds "SlashDot")
    ;(io/read-feeds "xkcd")
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
