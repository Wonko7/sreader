(ns client.core
  (:require [rum.core :as rum]
            [cljs-http.client :as http]
            [cognitect.transit :as json]
            [cljs.core.async :refer [chan <! >!] :as a]
            [com.rpl.specter :as s :refer [setval select-one select transform filterer keypath pred ALL ATOM FIRST]]
            [simple-reader.helpers :as h]
            ;; goog
            [goog.events :as events]
            )
  (:require-macros [cljs.core.async.macros :as m :refer [go-loop go]]
                   [utils.macros :refer [<? <?? go? go-try dprint]]))

(enable-console-print!)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; def state:

(defonce subscriptions-state (atom {}))
(defonce tags-state (atom {}))
(defonce feed-state (atom {:feed-data {:title "Loading..."}}))
(defonce article-metadata (atom {}))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; get stuff:

(defn init-feed-metadata [feed-state]
  (let [feed-state (setval [:feed-data :selected] nil feed-state)
        feed-state (transform [:articles ALL :guid] js/encodeURIComponent feed-state)
        articles (:articles feed-state)
        md (into {} (map (fn [{id :guid md :metadata}]
                           {id (atom (or md {:status "unread"}))})
                         articles))]
    (reset! article-metadata md)
    feed-state))

(defn request-subscriptions []
  (go (let [response    (<! (http/get "/subs/"))
            response    (h/read-json (:body response))
            subs-state  (:subscriptions response)
            t-state     (:tags response)]
        (reset! subscriptions-state subs-state)
        (reset! tags-state t-state))))

(defn request-feed [title]
  (go (let [response    (<! (http/get (str "/f/" (js/encodeURIComponent title) "/42")))
            response    (h/read-json (:body response))]
        (reset! feed-state (init-feed-metadata response)))))

(defn change-feed-md [feed md]
  (go (let [new-md      (:body (<! (http/post (str "/f-md/" (js/encodeURIComponent feed)) {:json-params md})))]
        ;(transform [ATOM :metadata] #(merge % new-md) feed-state)
        (request-feed feed))))


(defn change-article-md [feed art-id md]
  (go (let [new-md      (:body (<! (http/post (str "/md/" (js/encodeURIComponent feed) "/" art-id) {:json-params md})))]
        (transform [ATOM (keypath art-id) ATOM] #(merge % new-md) article-metadata))))

(defn toggle-tag-md [tag key]
    (go (let [k-val       (@tags-state tag)
              k-val       (if k-val (not (k-val key)) true)
              new-md      (:body (<! (http/post (str "/tag-md/" (js/encodeURIComponent tag)) {:json-params {key k-val}})))]
          (transform [ATOM (keypath tag)] #(merge % new-md) tags-state))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; subscriptions!

(defn mk-subs [feeds] ;; not worth being a comp yet
   (for [{name :name unread :unread-count} feeds
         :when (> unread 0)] ;; FIXME will change that for a toggle in ui
     [:div.subscription {:on-click #(request-feed name)}
      [:a {:href "javascript:void(0)"} name [:span.small " " unread]]]))

(rum/defcs mk-subscriptions < rum/reactive
  [state]
  (let [subs (rum/react subscriptions-state)
        tag-md (rum/react tags-state)]
    [:div.feeds
     (for [[tag feeds] subs]
       [:div.tag [:a {:on-click #(toggle-tag-md tag :visible?)
                      :href "javascript:void(0)"} tag]
        (when (-> tag tag-md :visible?)
          (mk-subs feeds))])]
    ))

(rum/mount (mk-subscriptions)
           (. js/document (getElementById "subscriptions")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; feeds!



(rum/defcs mk-article < rum/reactive
  [state
   {title :title date :pretty-date desc :description link :link id :guid}
   visible?]
  (let [a-md (@article-metadata id)
        {read-status :status} (rum/react a-md)
        [art-div-style art-read-status] (condp = read-status
                                          "saved" [:div.article.saved "saved"]
                                          "read"  [:div.article.read ""]
                                          [:div.article "unread"])]
    [art-div-style
     [:a.title {:href link} title]
     [:br]
     [:div.small [:span date] [:span  {:dangerouslySetInnerHTML {:__html "&emsp;—&emsp;"}}] [:span art-read-status]]
     [:div.content {:dangerouslySetInnerHTML {:__html desc}
                    :style {:display (if visible? "" "none")}}
      ]]))


(rum/defcs mk-feed < rum/reactive
                     {:did-update (fn [state]
                                    (let [comp     (:rum/react-component state)
                                          dom-node (js/ReactDOM.findDOMNode comp)]
                                      (set! (.-scrollTop dom-node) 0) )
                                    state)}
  [state]
  (let [fstate      (rum/react feed-state)
        f-md        (-> fstate :metadata)
        ftitle      (-> fstate :feed-data :title)
        articles    (:articles fstate)
        visible-id  (-> fstate :feed-data :selected :guid)
        visible-nb  (or (-> fstate :feed-data :selected :number) 0)
        articles    (drop visible-nb articles)
        root-div    (rum/dom-node state)]
    [:div.feed
      (when-not visible-id
        (let [mk-select     (fn [value values callback]
                              [:select
                               {:on-change callback ;(fn [e] (reset! *ref (long (.. e -target -value))))
                                :value value }
                               (for [v values]
                                 [:option { :value v } v])])
              order         (or (:order f-md) "oldest then saved")
              order-values  ["oldest" "newest" "oldest then saved"]
              view          (or (:view-art-status f-md) "unread")
              view-values   ["unread" "saved" "all"]
              ]
          [:div [:h1.feed-title ftitle]
           [:span.feed-controls
            (mk-select order order-values #(change-feed-md ftitle {:order (-> % .-target .-value)}))
            (mk-select view view-values #(change-feed-md ftitle {:view-art-status (-> % .-target .-value)}))
            ]
         ]))
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

(defn change-article-status-md [new-state]
  (let [guid (-> @feed-state :feed-data :selected :guid)
        feed (-> @feed-state :feed-data :title)
        cur-state (select-one [ATOM (keypath guid) ATOM :status] article-metadata)
        cur-state (or cur-state "unread")
        new-state (cond
                    (and (not= cur-state "saved") (= new-state "saved"))  "saved"
                    (and (= cur-state "saved") (= new-state "read"))      nil
                    (and (= cur-state "saved") (= new-state "saved"))     "unread"
                    (and (= cur-state "unread") (= new-state "read"))     "read"
                    (and (= cur-state "read") (= new-state "read"))       "unread")]
    (when new-state
      (change-article-md feed guid {:status new-state}))))


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
            "m" (change-article-status-md "read")
            "s" (change-article-status-md "saved")
            :else-nothing
            )
          ))))

;; init a page, fixme:
(request-subscriptions)
(request-feed "FMyLife")

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
