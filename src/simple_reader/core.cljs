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
        art-md-req  (chan)
        art-md-ans  (chan)
        ]

    ;; init http
    (http/init article-req article-ans art-md-req art-md-ans)

    ;; read feeds web client:
    (go-loop [{feed :feed nb :nb :as fixme} (<! article-req)]
             (println :getting fixme)
             (let [f (h/write-json {:feed-data {:title feed} :articles (sort-by :date (io/read-feed feed))})]
               (>! article-ans f)
               (recur (<! article-req))))

    ;; handle metadata changes:
    (go-loop [{{feed-id :feed article-id :article} :article-id new-md :metadata} (<! art-md-req)]
             (let [cur-md  (io/read-article-md feed-id article-id)
                   md       (merge cur-md new-md)]
               (println :cur cur-md)
               (println :new new-md)
               (println :merge md)
               (io/write-article-md feed-id article-id md)
               (>! art-md-ans md)
               (recur (<! art-md-req))))

    ;; scrape subscriptions once.
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
    ))


(def -main testing)
(set! *main-cli-fn* -main)
