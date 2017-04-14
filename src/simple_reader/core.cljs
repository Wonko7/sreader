;(ns ^:figwheel-always simple-reader.core
(ns simple-reader.core
  (:require
    [cljs.nodejs :as node]
    [simple-reader.feedreader :as fr]
    [cljs.core.async :refer [timeout chan <! >!] :as a]
    [simple-reader.helpers :as h]
    [simple-reader.render :as html]
    [cognitect.transit :as json]
    [simple-reader.feed-file-io :as io]
    [simple-reader.http :as http]
    [com.rpl.specter :as s :refer [collect setval select-one select transform view filterer keypath pred srange ALL ATOM FIRST MAP-VALS]])
  (:require-macros [cljs.core.async.macros :as m :refer [go-loop go]]
                   [utils.macros :refer [<? <?? go? go-try dprint]]))

(node/enable-util-print!)


(defn testing []
  (let [feed-req    (chan)
        feed-ans    (chan)
        art-md-req  (chan)
        art-md-ans  (chan)
        feed-md-req (chan)
        feed-md-ans (chan)
        tag-md-req  (chan)
        tag-md-ans  (chan)
        subs-req    (chan)
        subs-ans    (chan)
        ;;
        feed-md             (atom (io/load-feeds-md))
        get-fd-dir          (fn [name]
                              (-> name (@feed-md) :dir))
        get-feeds-by-tags   (fn [] ;; this is horrible.
                              (reset! feed-md (io/load-feeds-md))
                              (let [tags-md (io/read-tags-md)
                                    feed-md @feed-md
                                    tags  (->> (select [MAP-VALS :tags] feed-md)
                                               (reduce #(into %1 %2) #{})
                                               (sort-by #(-> % tags-md :position)))
                                    get-feeds-by-tag (fn [tag]
                                                       (sort-by :name (select [MAP-VALS (fn [v] (some #(= tag %) (:tags v)))] feed-md)))] ;; might be transformable instead
                                (map (fn [tag] {tag (get-feeds-by-tag tag)}) tags)))
        update-feeds        (fn []
                              (println "core:" (.toLocaleTimeString (new js/Date)) "starting update feeds")
                              (go (doseq [[k {link :url name :name dir :dir}] @feed-md
                                          ;:when (some #(= dir %) subs)
                                          ]
                                    (let [articles (chan)]
                                      (fr/read link articles)
                                      (go-loop [to-save (<! articles) cnt 0]
                                               (cond
                                                 (= :done to-save)  (do (println "core:" cnt "articles:" name)
                                                                        :done)
                                                 (= :error to-save) (do (println "core: error:" cnt "articles:" name)
                                                                        :error)
                                                 :else (do (io/save-article (get-fd-dir name) to-save)
                                                           (recur (<! articles) (inc cnt))))
                                               )))))
        ]
    ;; init http
    (http/init feed-req feed-ans
               subs-req subs-ans
               art-md-req art-md-ans
               feed-md-req feed-md-ans
               tag-md-req tag-md-ans)

    ;; read feeds web client:
    (go-loop [{feed :feed nb :nb :as fixme} (<! feed-req)]
             (println :getting fixme (@feed-md feed))
             (let [f-dir    (get-fd-dir feed)
                   metadata (io/read-feed-md f-dir)
                   view     (or (:view-art-status metadata) "unread")
                   order    (or (:order metadata)  "oldest then saved")
                   articles (io/read-feed f-dir)
                   ;view-values   ["unread" "saved" "all"]
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
                                (concat unsaved saved)))
                   f        (h/write-json {:feed-data {:title feed}
                                           :metadata metadata
                                           :articles articles})]
               (>! feed-ans f)
               (recur (<! feed-req))))

    ;; read subs:
    (go-loop [_ (<! subs-req)]
             (println :getting-subs)
             (>! subs-ans (h/write-json {:subscriptions (get-feeds-by-tags)
                                         :tags (io/read-tags-md)}))
             (recur (<! subs-req)))


    ;; handle article metadata changes:
    (go-loop [{{feed-id :feed article-id :article} :article-id new-md :metadata} (<! art-md-req)]
             (println :a-md-ch new-md)
             (let [cur-md  (io/read-article-md (get-fd-dir feed-id) article-id)
                   md       (merge cur-md new-md)]
               (io/write-article-md (get-fd-dir feed-id) article-id md)
               (>! art-md-ans md)
               (recur (<! art-md-req))))

    ;; handle feed metadata changes:
    (go-loop [{feed :feed new-md :metadata} (<! feed-md-req)]
             (println :f-md-ch feed new-md)
             (let [cur-md  (io/read-feed-md feed)
                   md      (merge cur-md new-md)]
               (io/write-feed-md feed md)
               (>! feed-md-ans md)
               (recur (<! feed-md-req))))

    ;; handle tag metadata changes:
    (go-loop [{tag :tag-id new-md :metadata} (<! tag-md-req)]
             (println :t-md-ch tag new-md)
             (let [cur-md  (io/read-tag-md tag)
                   md      (merge cur-md new-md)]
               (io/write-tag-md tag md)
               (>! tag-md-ans md)
               (recur (<! tag-md-req))))

    ;; scrape subscriptions
    (go (while true
          (update-feeds)
          (<! (timeout (* 1000 60 60)))))
    ))


(def -main testing)
(set! *main-cli-fn* -main)
