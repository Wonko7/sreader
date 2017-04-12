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
    [simple-reader.http :as http]
    [com.rpl.specter :as s :refer [collect setval select-one select transform view filterer keypath pred ALL ATOM FIRST]])
  (:require-macros [cljs.core.async.macros :as m :refer [go-loop go]]
                   [utils.macros :refer [<? <?? go? go-try dprint]]))

(nodejs/enable-util-print!)


(defn testing []
  (let [article-req (chan)
        article-ans (chan)
        art-md-req  (chan)
        art-md-ans  (chan)
        subs-req    (chan)
        subs-ans    (chan)
        ;;
        feed-md  (io/load-feeds-md)
        get-fd-dir (fn [name]
                     (-> name feed-md :dir))
        ]
    ;; init http
    (http/init article-req article-ans
               art-md-req art-md-ans
               subs-req subs-ans)

    ;; read feeds web client:
    (go-loop [{feed :feed nb :nb :as fixme} (<! article-req)]
             (println :getting fixme)
             (let [f (h/write-json {:feed-data {:title feed} :articles (sort-by :date (io/read-feed (get-fd-dir feed)))})]
               (>! article-ans f)
               (recur (<! article-req))))

    ;; read subs:
    (go-loop [_ (<! subs-req)]
             (println :getting-subs)
             (>! subs-ans (h/write-json (sort-by :name (select [ALL FIRST (view #(hash-map :name %))] feed-md))))
             (recur (<! subs-req)))
    ;(println (sort-by :name (select [ALL FIRST (view #(hash-map :name %))] feed-md))) 

    ;; handle metadata changes:
    (go-loop [{{feed-id :feed article-id :article} :article-id new-md :metadata} (<! art-md-req)]
             (let [cur-md  (io/read-article-md (get-fd-dir feed-id) article-id)
                   md       (merge cur-md new-md)]
               (println :cur cur-md)
               (println :new new-md)
               (println :merge md)
               (io/write-article-md (get-fd-dir feed-id) article-id md)
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
                        (when (not= :done to-save)
                          (io/save-article (get-fd-dir name) to-save)
                          (recur (<! articles)))
                        ))))
    ))


(def -main testing)
(set! *main-cli-fn* -main)
