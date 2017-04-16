(ns client.core
  (:require [rum.core :as rum]
            [cljs-http.client :as http]
            [cognitect.transit :as json]
            [cljs.core.async :refer [chan <! >!] :as a]
            [com.rpl.specter :as s :refer [setval select-one select transform must if-path cond-path multi-transform multi-path filterer keypath pred submap terminal-val terminal srange ALL ATOM FIRST MAP-VALS]]
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
(defonce tags-metadata (atom {}))
(defonce feed-state (atom {:feed-data {:title "Loading..."}}))
(defonce article-metadata (atom {}))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; get stuff:

(defn init-feed-metadata [feed-state]
  (let [feed-state (transform [:articles ALL :guid] js/encodeURIComponent feed-state)
        articles (merge (:articles feed-state) {:selected nil})
        md (into {} (map (fn [{id :guid md :metadata}]
                           {id (atom (merge {:status "unread" :visible? false} md))})
                         articles))]
    (reset! article-metadata md)
    feed-state))

(defn request-subscriptions []
  (go (let [response    (<! (http/get "/subs/"))
            response    (h/read-json (:body response))
            mk-atom-dic #(into {} (for [[k md] %]
                                    {k (atom md)}))
            subs-state  (mk-atom-dic (:subscriptions response))
            t-md        (mk-atom-dic (:tag-metadata response))
            t-state     (select-one [(submap [:tag-order :tag-content])] response)]
        (reset! subscriptions-state subs-state)
        (reset! tags-state t-state)
        (reset! tags-metadata t-md))))

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
        (transform [ATOM (keypath art-id) ATOM] #(merge % new-md) article-metadata)
        new-md)))

(defn toggle-tag-md [tag key]
  (go (let [k-val       (select-one [ATOM (keypath tag) ATOM (keypath key)] tags-metadata)
            k-val       (if k-val (not k-val) true)
            new-md      (:body (<! (http/post (str "/tag-md/" (js/encodeURIComponent tag)) {:json-params {key k-val}})))]
        (transform [ATOM (keypath tag) ATOM] #(merge % new-md) tags-metadata))))

(defn change-article-status-md [new-state & [gguid]]
  (let [guid (or gguid (-> @article-metadata :selected :guid))
        feed (-> @feed-state :feed-data :title)
        cur-state (select-one [ATOM (keypath guid) ATOM :status] article-metadata)
        cur-state (or cur-state "unread")
        new-state (cond
                    (and gguid (not= cur-state "saved"))                  "read" ;; hackish, this is for auto-mark-read on article change
                    (and (not= cur-state "saved") (= new-state "saved"))  "saved"
                    (and (= cur-state "saved") (= new-state "read"))      nil
                    (and (= cur-state "saved") (= new-state "saved"))     "unread"
                    (and (= cur-state "unread") (= new-state "read"))     "read"
                    (and (= cur-state "read") (= new-state "read"))       "unread")]
    (if (and new-state (not= new-state cur-state))
      (do (cond
            (or (= new-state "read") (and (= cur-state "unread") (= new-state "saved")))
            (transform [ATOM (keypath feed) ATOM :unread-count] dec subscriptions-state)
            (= new-state "unread")
            (transform [ATOM (keypath feed) ATOM :unread-count] inc subscriptions-state))
          (change-article-md feed guid {:status new-state}))
      (go :nothing-was-done))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; interaction:

(defn change-article [nb & [guid]]
  "If guid is given, nb is ignored, guid is selected.
  Otherwise next article is nb relative to current article."
  (let [cur-nb          (-> @article-metadata :selected :number)
        cur-guid        (-> @article-metadata :selected :guid)
        cur-nb          (or cur-nb -1)
        articles        (-> @feed-state :articles)
        total           (count articles)
        next-nb         (+ nb cur-nb)
        next-nb         (cond (< next-nb 0) 0
                              (< next-nb total) next-nb
                              :else (dec total))
        [guid next-nb]  (if guid
                          [guid (count (take-while #(not= guid (:guid %)) articles))]
                          [(:guid (nth articles next-nb)) next-nb])]
    (multi-transform [ATOM (multi-path [:selected (terminal-val {:number next-nb :guid guid})]
                                       [(if-path (keypath cur-guid) (keypath cur-guid)) ATOM :visible? (terminal-val false)]
                                       [(keypath guid) ATOM :visible? (terminal-val true)])]
                     article-metadata)
    (change-article-status-md "read" guid)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; subscriptions!

(rum/defc mk-sub < rum/reactive
  [feed show-all]
  (println :mk-sub)
  (let [selected-feed (-> feed-state rum/react :feed-data :title)
        unread        (:unread-count (rum/react (@subscriptions-state feed)))
        a             (if (zero? unread) :a.grey :a)
        div           (if (= selected-feed feed)
                        :div.subscription.selected
                        :div.subscription)]
    (when (or show-all (> unread 0))
      [div {:on-click #(request-feed feed)}
       [:div.sub-title [a {:href "javascript:void(0)"} feed]]
       [:div.sub-count.small unread]])))

(rum/defcs mk-tag < rum/reactive
                    (rum/local false ::show-all-read)
  [state tag feeds]
  (println :mk-tag)
  (let [tag-md    (rum/react (@tags-metadata tag))
        v?        (:visible? tag-md)
        show-all  (::show-all-read state)]
    [:div.tag
     [:a {:on-click #(toggle-tag-md tag :visible?)
          :href "javascript:void(0)"} (str (if v? "▾ " "▸ ") tag)]
     [:a.sub-show-all {:on-click #(swap! show-all not)
                       :href "javascript:void(0)"} (str (if @show-all " - " " + "))]
     (when v?
       (for [f feeds]
         (rum/with-key (mk-sub f @show-all) f)))]))

(rum/defcs mk-subscriptions < rum/reactive
  [state]
  (println :mk-subscriptions)
  (let [t-state   (rum/react tags-state)]
    [:div.feeds
     (for [tag (:tag-order t-state)
           :let [feeds ((:tag-content t-state) tag)]
           :when feeds] ;; weed out empty tags
       (rum/with-key (mk-tag tag feeds) tag))]))

(rum/mount (mk-subscriptions)
           (. js/document (getElementById "subscriptions")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; feeds!

(rum/defcs mk-article < rum/reactive
                        ;{:did-update (fn [state] ;; FIXME: we're doing this  on EVERY article (unselected too), bad. but h
                        ;               "focus on current article on article/feed change -> kb scrolling"
                        ;               (let [comp     (:rum/react-component state)
                        ;                     dom-node (js/ReactDOM.findDOMNode comp)]
                        ;                 (.focus dom-node))
                        ;               state)}
  [state
   {title :title date :pretty-date desc :description link :link id :guid}]
  (println :mk-article)
  (let [a-md     (@article-metadata id)
        {read-status :status visible? :visible?} (rum/react a-md)
        [art-div-style art-read-status] (condp = read-status
                                          "saved" [:div.article.saved "saved"]
                                          "read"  [:div.article.read ""]
                                          [:div.article "unread"])]
    [art-div-style {:tab-index -1 :style {:outline 0}
                    :on-click (when-not visible? #(change-article 0 id))}
     [:a.title {:href (if visible? link "javascript:void(0)")} title]
     [:br]
     [:div.article-info.small [:div.date date] [:div.artical-satus art-read-status]]
     [:div.content {:dangerouslySetInnerHTML {:__html desc}
                    :style {:display (if visible? "" "none")}}
      ]]))

(rum/defc mk-feed-title < rum/reactive
  [ftitle]
  (println :mk-feed-title)
  (let [f-md          (-> feed-state rum/react :metadata)
        astate        (rum/react article-metadata)
        visible-id    (:selected :guid astate)
        feed-exists?  (@subscriptions-state ftitle)
        sub-state     (if feed-exists?
                        (rum/react feed-exists?)
                        {:unread-count ""})
        ;; feed select controls:
        order         (or (:order f-md) "oldest then saved")
        order-values  ["oldest" "newest" "oldest then saved"]
        view          (or (:view-art-status f-md) "unread")
        view-values   ["unread" "saved" "all"]
        div-title     (if visible-id :div.feed-title-small :div.feed-title)
        mk-select     (fn [value values callback]
                        [:select {:on-change callback :value value}
                         (for [v values]
                           [:option {:value v} v])])]
    [:div.feed-title-wrapper
     [div-title
      [:div.title-only ftitle]
      [:div.feed-count.small (:unread-count sub-state)]]
     [:div.feed-controls
      (mk-select order order-values #(change-feed-md ftitle {:order (-> % .-target .-value)}))
      (mk-select view view-values #(change-feed-md ftitle {:view-art-status (-> % .-target .-value)}))]]
    ))

(rum/defcs mk-feed < rum/reactive
                     ;{:did-update (fn [state] ;; FIXME: testing
                     ;               "focus on current article on article/feed change -> kb scrolling"
                     ;               (let [comp     (:rum/react-component state)
                     ;                     dom-node (js/ReactDOM.findDOMNode comp)]
                     ;                 (.focus dom-node))
                     ;               state)}
                     {:did-update (fn [state]
                                    "Go to top of article on article change"
                                    (let [dom-node (. js/document (getElementById "feed"))]
                                      (set! (.-scrollTop dom-node) 0)
                                      (.focus dom-node)
                                      state))}
  [state]
  (println :mk-feed)
  (let [fstate      (rum/react feed-state)
        ;astate      (rum/react article-metadata)
        ftitle      (-> fstate :feed-data :title)
        articles    (:articles fstate)
        ;visible-id  (-> astate :selected :guid)
        ;visible-nb  (or (-> astate :selected :number) 0)
        ;articles    (drop visible-nb articles)
        ]
    [:div.feed
     (mk-feed-title ftitle)
     (for [a articles
           :let [rum-key (:guid a)]]
       (rum/with-key (mk-article a) rum-key)
       )]))

(rum/mount (mk-feed)
           (. js/document (getElementById "feed")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; search:

(defonce search-state (atom {:visible false}))

(defn select-search-feed []
  (when-let [feed (nth (:results @search-state) (:nth @search-state))]
    (request-feed feed))
  (reset! search-state {:visible false}))

(defn search [text]
  (let [subs  (sort (select [ATOM ALL FIRST] subscriptions-state)) ;; FIXME no we're not.
        re    (re-pattern (str "(?i)" (apply str (interpose ".*?" text))))
        res   (->> subs
                   (map #(vector (re-find re %) %))
                   (filter first)
                   (sort-by #(-> % first count))
                   (take 10)
                   (map second))]
    (multi-transform [ATOM (multi-path [:results (terminal-val res)]
                                       [:nth (terminal-val 0)])]
                     search-state)))

(rum/defcs mk-search < rum/reactive
                      {:did-update (fn [state]
                                     "Auto focus on input"
                                     (let [comp     (:rum/react-component state)
                                           dom-node (js/ReactDOM.findDOMNode comp)]
                                       (when dom-node ;; not present when search closes
                                         (-> dom-node .-firstChild .-firstChild .focus))
                                       state))}
  [state]
  (println :mk-search)
  (let [s-state   (rum/react search-state)
        v?        (:visible s-state)
        res       (:results s-state)
        nth       (:nth s-state)
        max       (dec (count (select-one [ATOM :results] search-state)))
        res       (map #(vector :div %) res)
        res       (when (> max -1) (setval [(srange nth (inc nth)) FIRST FIRST] :div.first-result res))]
    (when v?
      [:div#search-wrapper
       [:div#search
        [:input#search-input {:type :text
                              :on-key-down #(let [kcode     (.-keyCode %)
                                                  s-inc     (fn [i] (let [n (inc i)]
                                                                      (if (>= n max) max n)))
                                                  s-dec     (fn [i] (let [n (dec i)]
                                                                      (if (>= n 0) n 0)))]
                                              (cond (= 38 kcode) (transform [ATOM :nth] s-dec search-state)
                                                    (= 40 kcode) (transform [ATOM :nth] s-inc search-state)
                                                    (= 13 kcode) (select-search-feed))
                                              (when (:visible @search-state)
                                                (comment
                                                  (.preventDefault %)
                                                  (.stopPropagation %)))
                                              false)
                              :on-change #(search (-> % .-target .-value))}]
        [:div (vec res)]]])))

(rum/mount (mk-search)
           (. js/document (getElementById "search-anchor")))


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
          (when-not (:visible @search-state) ;; FIXME I hate myself.
            (condp = character
              "j" (<! (change-article 1))
              "k" (<! (change-article -1))
              "m" (<! (change-article-status-md "read"))
              "s" (<! (change-article-status-md "saved"))
              "b" (transform [ATOM :visible] not search-state)
              "r" (request-feed (select-one [ATOM :feed-data :title] feed-state))
              "R" (do (request-subscriptions)
                      (request-feed (select-one [ATOM :feed-data :title] feed-state)))
              "v" (do (let [guid (select-one [ATOM :selected :guid] article-metadata)
                            link (select-one [ATOM :articles ALL #(= guid (:guid %)) :link] feed-state)] ;; specter is awesome, my state structure isn't.
                        (.open js/window link))) ;; FIXME: for this to work in chrome (tab instead of pop up), we'd need to .open in the listen callback, which is annoying.
              "G" (let [dom-node (. js/document (getElementById "feed"))]
                    (set! (.-scrollTop dom-node) (.-scrollHeight dom-node)))
              "g" (let [dom-node (. js/document (getElementById "feed"))]
                    (set! (.-scrollTop dom-node) 0))
              :else-nothing
              ))))))

;; init a page, fixme:
(request-subscriptions)
(request-feed "FMyLife")

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
