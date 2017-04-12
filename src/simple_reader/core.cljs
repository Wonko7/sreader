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
    [com.rpl.specter :as s :refer [collect setval select-one select transform view filterer keypath pred ALL ATOM FIRST MAP-VALS]])
  (:require-macros [cljs.core.async.macros :as m :refer [go-loop go]]
                   [utils.macros :refer [<? <?? go? go-try dprint]]))

(nodejs/enable-util-print!)


(defn testing []
  (let [feed-req    (chan)
        feed-ans    (chan)
        art-md-req  (chan)
        art-md-ans  (chan)
        tag-md-req  (chan)
        tag-md-ans  (chan)
        subs-req    (chan)
        subs-ans    (chan)
        ;;
        feed-md  (io/load-feeds-md)
        get-fd-dir (fn [name]
                     (-> name feed-md :dir))
        get-feeds-by-tags (fn []
                            (let [tags-md (io/read-tags-md)
                                  tags (sort-by #(-> % tags-md :position) (reduce #(into %1 %2) #{} (select [MAP-VALS :tags] feed-md)))
                                  get-feeds-by-tag (fn [tag]
                                                     (select [MAP-VALS (fn [v] (some #(= tag %) (:tags v)))] feed-md))]
                              (map (fn [tag] [tag (get-feeds-by-tag tag)]) tags)))  ;; sort-by
        ]
    ;; init http
    (http/init feed-req feed-ans
               subs-req subs-ans
               art-md-req art-md-ans
               tag-md-req tag-md-ans)

    ;; (println (io/count-unread (get-fd-dir "Slashdot")))
    ;; (let [tags [
    ;;             "lol"
    ;;             "Comics"
    ;;             "Blogs"
    ;;             "Authors"
    ;;             "Geek Stuff"
    ;;             "Tech Fun"
    ;;             "Gentoo"
    ;;             "Secu"
    ;;             "Locks"
    ;;             "Climbing"
    ;;             "Survivalism"
    ;;             "IE"
    ;;             "Lisp"
    ;;             "Math"
    ;;             "News"
    ;;             "Thinkers"


    ;;             ]
    ;;       ]
    ;;   (doseq [[tag md] (map (fn [t n] [t {:position n}]) tags (range))]
    ;;            (io/write-tag-md tag md)
    ;;            ))

    ;; read feeds web client:
    (go-loop [{feed :feed nb :nb :as fixme} (<! feed-req)]
             (println :getting fixme (feed-md feed))
             ;(println (interpose "\n" (keys feed-md)))
             (let [f (h/write-json {:feed-data {:title feed} :articles (sort-by :date (io/read-feed (get-fd-dir feed)))})]
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
             (println :a-md-ch)
             (let [cur-md  (io/read-article-md (get-fd-dir feed-id) article-id)
                   md       (merge cur-md new-md)]
               (io/write-article-md (get-fd-dir feed-id) article-id md)
               (>! art-md-ans md)
               (recur (<! art-md-req))))

    ;; handle tag metadata changes:
    (go-loop [{tag :tag-id new-md :metadata} (<! tag-md-req)]
             (println :t-md-ch tag new-md)
             (let [cur-md  (io/read-tag-md tag)
                   md      (merge cur-md new-md)]
               (io/write-tag-md tag md)
               (>! tag-md-ans md)
               (recur (<! tag-md-req))))

    ;; scrape subscriptions once.
    (comment (let [subs [ ]
                   one-by-one (chan)]
               ;(go (>! one-by-one :go))
               (go (doseq [[k {link :url name :name dir :dir}] feed-md
                           ;:when (some #(= dir %) subs)
                           ]
                     (let [articles (chan)]
                       ;(<! one-by-one)
                       (println "fetching" name)
                       (fr/read link articles)
                       (go-loop [to-save (<! articles)]
                                (cond
                                  (= :done to-save)  :done ;(>! one-by-one :go)
                                  (= :error to-save) :error ;(>! one-by-one :go) ;:error (comment (io/mv-bad-feed (get-fd-dir name)))
                                  :else (do (io/save-article (get-fd-dir name) to-save)
                                            (recur (<! articles))))

                                ))))))
    ))


(def -main testing)
(set! *main-cli-fn* -main)
