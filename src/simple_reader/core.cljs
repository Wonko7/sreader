;(ns ^:figwheel-always simple-reader.core
(ns simple-reader.core
  (:require
    [cljs.nodejs :as node]
    [cljs.core.async :refer [timeout chan <! >!] :as a]
    [clojure.set :as set]
    [simple-reader.feedreader :as fr]
    [simple-reader.helpers :as h]
    [simple-reader.render :as html]
    [cognitect.transit :as json]
    [simple-reader.feed-file-io :as io]
    [simple-reader.http :as http]
    [simple-reader.scrape :as scrape]
    [com.rpl.specter :as s :refer [collect setval select-one select transform view filterer keypath pred srange ALL ATOM FIRST MAP-VALS]])
  (:require-macros [cljs.core.async.macros :as m :refer [go-loop go]]
                   [utils.macros :refer [<? <?? go? go-try dprint try->empty]]))

(node/enable-util-print!)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; feed-md state :

(def feed-md (atom {}))

(defn get-subs-by-tags []
  (reset! feed-md (io/load-feeds-md))
  (let [tags-md   (io/load-tags-md)
        tag-list  (sort-by #(-> % tags-md :position) (keys tags-md))
        get-feeds-by-tag (fn [tag]
                           (sort-by first (select [ATOM MAP-VALS (fn [v] (some #(= tag %) (:tags v))) :name] feed-md)))]
    {:tag-order tag-list
     :tag-metadata tags-md
     :tag-content (into {} (map (fn [tag] {tag (get-feeds-by-tag tag)}) tag-list))
     :subscriptions @feed-md}))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Process client requests:

(defn get-subscriptions [_]
  "read subscriptions"
  (h/write-json (get-subs-by-tags)))

(defn get-feed [{feed :feed nb :nb :as fixme}]
  "read feeds web client"
  (try->empty (let [metadata (io/load-feed-md feed)
                    view     (or (:view-art-status metadata) "unread")
                    order    (or (:order metadata)  "oldest then saved")
                    articles (io/load-feed feed)
                    articles (condp = view
                               "unread"  (let [unread  (filter #(not= "read" (-> % :metadata :status)) articles)
                                               unsaved (filter #(not= "saved" (-> % :metadata :status)) unread)]
                                           (if (= 0 (count unsaved))
                                             articles
                                             unread))
                               "saved"   (filter #(= "saved" (-> % :metadata :status)) articles)
                               articles)
                    articles (condp = order
                               "oldest" (sort-by :date articles)
                               "newest" (reverse (sort-by :date articles))
                               (let [unsaved (sort-by :date (filter #(not= "saved" (-> % :metadata :status)) articles))
                                     saved   (sort-by :date (filter #(= "saved" (-> % :metadata :status)) articles))]
                                 (concat unsaved saved)))]
                (h/write-json {:feed-data {:title feed}
                               :metadata metadata
                               :articles articles}))))

(defn change-article-md [{{feed :feed article :article} :id new-md :metadata}]
  "handle article metadata changes"
  (try->empty (let [cur-md  (io/load-article-md feed article)
                    md      (merge cur-md new-md)]
                (io/save-article-md feed article md)
                md)))

(defn change-feed-md [{{feed :feed} :id new-md :metadata}]
  "handle feed metadata changes:"
  (try->empty (let [cur-md  (io/load-feed-md feed)
                    md      (merge cur-md new-md)]
                (io/save-feed-md feed md)
                md)))

(defn change-tag-md [{{tag :tag} :id new-md :metadata} ]
  "handle tag metadata changes:"
  (try->empty (let [cur-md  (io/load-tag-md tag)
                    md      (merge cur-md new-md)]
                (io/save-tag-md tag md)
                md)))

(defn update-feeds []
  "scrape subscriptions"
  (let [process-article (fn [feed {cnt :count kept-articles :kept} article]
                          (if (or (= article :done) (= article :error))
                            {:count cnt :status article :kept kept-articles}
                            (let [already-scraped (try->empty (io/load-article-scraped feed (:guid article))) ;; FIXME scrape-fn makes that decision
                                  scraped         (scrape/scrape feed article already-scraped)]
                              (go (try->empty (io/save-article feed article (<! scraped))))
                              {:count (inc cnt) :kept (conj kept-articles (:guid article))})))
        purge           (fn [feed {status :status to-keep :kept}]
                          (when (= status :done)
                            (let [all       (try->empty (io/load-feed feed))
                                  read      (into #{} (select [ALL #(= "read" (-> % :metadata :status)) :guid] all))
                                  out-dated (set/difference read to-keep)]
                              (map #(try->empty (io/rm-article feed %)) out-dated))))]
    (println "core:" (.toLocaleTimeString (new js/Date)) "starting update feeds")
    (doseq [[k {link :url feed :name www-link :www-link :as fmd}] @feed-md
            :let [[feed-meta articles] (fr/read link)]]
      (go (let [new-link (:link (<! feed-meta))]
            (when (not= new-link www-link)
              (try->empty (io/save-feed-fmd feed (merge (dissoc fmd :unread-count :saved-count) {:www-link new-link}))))))
      (go (let [res       (<! (a/reduce (partial process-article feed) {:kept #{} :count 0} articles))
                purged    (purge feed res)]
            (println "core:" (-> res :status name (str ":")) (:count res) "articles:" feed "-- purged:" (count purged)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; app:

(defn -main []
  (let [feed-req    (chan)
        feed-ans    (chan)
        art-md-req  (chan)
        art-md-ans  (chan)
        feed-md-req (chan)
        feed-md-ans (chan)
        tag-md-req  (chan)
        tag-md-ans  (chan)
        subs-req    (chan)
        subs-ans    (chan)]

    (get-subs-by-tags)

    ;; init http
    (http/init feed-req feed-ans
               subs-req subs-ans
               art-md-req art-md-ans
               feed-md-req feed-md-ans
               tag-md-req tag-md-ans)

    (a/pipeline 1 feed-ans (map get-feed) feed-req)
    (a/pipeline 1 subs-ans (map get-subscriptions) subs-req)
    (a/pipeline 1 art-md-ans (map change-article-md) art-md-req)
    (a/pipeline 1 feed-md-ans (map change-feed-md) feed-md-req)
    (a/pipeline 1 tag-md-ans (map change-tag-md) tag-md-req)

    (go (while true
          (update-feeds)
          (<! (timeout (* 1000 60 60)))))))

(set! *main-cli-fn* -main)
