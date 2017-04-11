(ns simple-reader.core
  (:require [rum.core :as rum]
            [cljs-http.client :as http]
            [cognitect.transit :as json]
            [cljs.core.async :refer [chan <! >!] :as a]
            [com.rpl.specter :as s :refer [setval select-one select transform filterer keypath pred ALL ATOM FIRST]]
            ;; goog
            [goog.events :as events]
            )
  (:require-macros [cljs.core.async.macros :as m :refer [go-loop go]]
                   [utils.macros :refer [<? <?? go? go-try dprint]]))

(enable-console-print!)
(def host "http://localhost:3000")

;; define your app data so that it doesn't get over-written on reload


;; FIXME tmp:
(declare request-feed change-article-md)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; subscriptions!

(defonce subscriptions-state (atom {:feeds [{:title "xkcd"} {:title "SlashDot"}]}))

(rum/defcs mk-subscriptions < rum/reactive
                             (rum/local true ::visible)
                             [state]
  (let [subs (:feeds (rum/react subscriptions-state))
        visible (::visible state)]
    (println @visible)
    (into [:div.feeds {:style (if @visible {:visiblity "visible"} {:visiblity "hidden"}) ;{:flex "0 0 15%"} {:flex "0 100 0%"})
                       :on-click #(swap! visible not)}] ; xp, does not work.
	
          (for [{t :title} subs]
            [:div.subscription {:on-click #(request-feed t)}
             [:br]
             [:a {:href "javascript:void(0)"} t]]))))

(rum/mount (mk-subscriptions)
           (. js/document (getElementById "subscriptions")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; feeds!


(defonce feed-state (atom {:feed-data {:title "Loading..."}}))
(def article-metadata (atom {}))


;; :div.article {;:tab-index -1 ;; needed for focus
;;                    ;:on-focus #(do (reset! visible true)
;;                    ;               (let [f-div (.querySelector js/document "#feed .feed")
;;                    ;                     a-div (rum/dom-node state)]
;;                    ;                 (set! (.-scrollTop f-div) (.-offsetTop a-div))
;;                    ;                 (println :set (.-offsetTop a-div))
;;                    ;                 ))
;;                    ;:on-blur #(reset! visible false)
;;                    }

(rum/defcs mk-article < rum/reactive
  [state
   {title :title date :pretty-date desc :description link :link id :guid
    {read? :read? saved? :saved} :metadata}
   visible?]
  (let [a-md (@article-metadata id)
        {read? :read? saved? :saved?} (rum/react a-md)]
    [:div.article
     [:a.title {:href link} title]
     [:br]
     [:div.small date]
     [:div.small (str (if read? "read" "unread") " / " (if saved? "saved" "unsaved"))]
     [:div.content {:dangerouslySetInnerHTML {:__html desc}
                    :style {:display (if visible? "" "none")}}
      ]]))

(rum/defcs mk-feed < rum/reactive [state]
  (let [fstate      (rum/react feed-state)
        ftitle      (-> fstate :feed-data :title)
        articles    (:articles fstate)
        visible-id  (-> fstate :feed-data :selected :guid)
        visible-nb  (or (-> fstate :feed-data :selected :number) 0)
        articles    (drop visible-nb articles)]
    [:div.feed
      (when-not visible-id [:h1 ftitle])
      (for [a articles
            :let [rum-key (:guid a)]]
        (rum/with-key (mk-article a (= rum-key visible-id)) rum-key)
        )]))

(rum/mount (mk-feed)
           (. js/document (getElementById "feed")))

(defn change-article [nb]
  (let [cur-nb    (-> @feed-state :feed-data :selected :number)
        cur-nb    (or cur-nb -1)
        articles  (-> @feed-state :articles)
        total     (count articles)
        next-nb   (+ nb cur-nb)
        next-nb   (cond (< next-nb 0) 0
                        (< next-nb total) next-nb
                        :else (dec total))
       guid       (:guid (nth articles next-nb))]

    ;(setval [ATOM (keypath guid) ATOM :read?] true article-metadata)
    ;(ask to mark as read)
    (setval [ATOM :feed-data :selected] {:number next-nb :guid guid} feed-state)))

(defn toggle-article-md [key]
  (let [guid (-> @feed-state :feed-data :selected :guid)
        feed (-> @feed-state :feed-data :title)
        read-state (select-one [ATOM (keypath guid) ATOM (keypath key)] article-metadata)]
    (println :cur-read read-state)
    (change-article-md feed guid {key (not read-state)})
    ))

(defn init-feed-metadata [feed-state]
;;;;  (identity (let [feed-state {:articles [{:a 1} {:a 2 :c 1} {:a 3} {:a 2 :c 3}]}
  ;;                feed-state2 (atom feed-state)]
  ;;           (println :loooooooooooooooooooooool)
  ;;           ;(println (transform [:articles ALL (pred #(= (:a %) 2)) FIRST] feed-state))
  ;;           ;;(println (setval [ATOM :articles ALL :visible] false feed-state2))
  ;;           ;;(println @feed-state2)
  ;;           ;(println (select [:articles ALL :a]  feed-state))
  ;;           ;;))
  ;(setval [:articles ALL :visible] false feed-state)
  (let [feed-state (setval [:feed-data :selected] nil feed-state)
        feed-state (transform [:articles ALL :guid] js/encodeURIComponent feed-state)
        articles (:articles feed-state)
        md (into {} (map (fn [{id :guid md :metada}]
                           {id (atom (or md {:saved? false :read? false}))})
                         articles))
        ]
    
    (reset! article-metadata md)
    feed-state))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; get stuff:

(defn request-feed [title]
  (go (let [json-reader (json/reader :json)
            response (<! (http/get (str "/f/" title "/42")))
            response (json/read json-reader (:body response))]
        (reset! feed-state (init-feed-metadata response)))))

(defn change-article-md [feed art-id md]
  (go (let [json-writer (json/writer :json)
            json-reader (json/reader :json)
            new-md      (:body (<! (http/post (str "/md/" feed "/" art-id) {:json-params md})))]
        (transform [ATOM (keypath art-id) ATOM] #(merge % new-md) article-metadata))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; keyboard:

(defn listen [el type]
  (let [out (chan)]
    (events/listen el type
                   (fn [e] (go (>! out e))))
    out))

(let [keypresses (listen (.querySelector js/document "body") "keypress")]
  (go (while true
        (let [key-event (<! keypresses)
              character (.fromCharCode js/String (.-charCode key-event))]
          (condp = character
            "j" (change-article 1)
            "k" (change-article -1)
            "m" (toggle-article-md :read?)
            "s" (toggle-article-md :saved?)
            :else-nothing
            )
          ))))

;; init a page, fixme:
(request-feed "SlashDot")

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
