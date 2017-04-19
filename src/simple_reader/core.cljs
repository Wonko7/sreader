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
    [simple-reader.scrape :as scrape]
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
        feed-md             (atom {})
        get-subs-by-tags    (fn []
                              (reset! feed-md (io/load-feeds-md))
                              (let [tags-md   (io/load-tags-md)
                                    tag-list  (sort-by #(-> % tags-md :position) (keys tags-md))
                                    get-feeds-by-tag (fn [tag]
                                                       (sort-by :name (select [ATOM MAP-VALS (fn [v] (some #(= tag %) (:tags v))) :name] feed-md)))]
                                {:tag-order tag-list
                                 :tag-metadata tags-md
                                 :tag-content (into {} (map (fn [tag] {tag (get-feeds-by-tag tag)}) tag-list))
                                 :subscriptions @feed-md}))
        update-feeds        (fn []
                              (println "core:" (.toLocaleTimeString (new js/Date)) "starting update feeds")
                              (go (doseq [[k {link :url feed :name}] @feed-md]
                                    (let [articles (chan)]
                                      (fr/read link articles)
                                      (go-loop [to-save (<! articles) cnt 0]
                                               (cond
                                                 (= :done to-save)  (do (println "core:" cnt "articles:" feed)
                                                                        :done)
                                                 (= :error to-save) (do (println "core: error:" cnt "articles:" feed)
                                                                        :error)
                                                 :else (do (let [scraped  (io/read-article-scraped feed (:guid to-save))
                                                                 scraped  (if (empty? scraped)
                                                                            (scrape/scrape feed (:link to-save))
                                                                            (go scraped))]
                                                             (go (io/save-article feed to-save (<! scraped))))
                                                           (recur (<! articles) (inc cnt))))
                                               )))))
        ]
    (get-subs-by-tags)
    ;; init http
    (http/init feed-req feed-ans
               subs-req subs-ans
               art-md-req art-md-ans
               feed-md-req feed-md-ans
               tag-md-req tag-md-ans)

    ;; read feeds web client:
    (go-loop [{feed :feed nb :nb :as fixme} (<! feed-req)]
             (println :getting fixme (@feed-md feed))
             (let [metadata (io/load-feed-md feed)
                   view     (or (:view-art-status metadata) "unread")
                   order    (or (:order metadata)  "oldest then saved")
                   articles (io/load-feed feed)
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
             (>! subs-ans (h/write-json (get-subs-by-tags)))
             (recur (<! subs-req)))


    ;; handle article metadata changes:
    (go-loop [{{feed :feed article :article} :article-id new-md :metadata} (<! art-md-req)]
             (println :a-md-ch new-md)
             (let [cur-md  (io/read-article-md feed article)
                   md       (merge cur-md new-md)]
               (io/save-article-md feed article md)
               (>! art-md-ans md)
               (recur (<! art-md-req))))

    ;; handle feed metadata changes:
    (go-loop [{feed :feed new-md :metadata} (<! feed-md-req)]
             (println :f-md-ch feed new-md)
             (let [cur-md  (io/load-feed-md feed)
                   md      (merge cur-md new-md)]
               (io/save-feed-md feed md)
               (>! feed-md-ans md)
               (recur (<! feed-md-req))))

    ;; handle tag metadata changes:
    (go-loop [{tag :tag-id new-md :metadata} (<! tag-md-req)]
             (println :t-md-ch tag new-md)
             (let [cur-md  (io/load-tag-md tag)
                   md      (merge cur-md new-md)]
               (io/save-tag-md tag md)
               (>! tag-md-ans md)
               (recur (<! tag-md-req))))

    ;; scrape subscriptions
    (go (while true
          (update-feeds)
          (<! (timeout (* 1000 60 60)))))
    ))


(def -main testing)
(set! *main-cli-fn* -main)
