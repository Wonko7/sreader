(ns client.core
  (:require [clojure.string :as str]
            [rum.core :as rum]
            [cljs-http.client :as http]
            [cognitect.transit :as json]
            [cljs.core.async :as a :refer [chan <! >!]]
            [com.rpl.specter :as s :refer [setval select-one select transform must if-path cond-path multi-transform multi-path filterer keypath pred submap terminal-val terminal srange ALL ATOM FIRST MAP-VALS]]
            [secretary.core :as secretary :refer-macros [defroute]]
            ;; me
            [simple-reader.helpers :as h]
            ;; goog
            [goog.events :as events]
            [goog.history.EventType :as EventType]
            [goog.events :as events])
            
  (:require-macros [cljs.core.async.macros :as m :refer [go-loop go]]
                   [utils.macros :refer [<? <?? go? go-try dprint]])
  (:import goog.History))

(enable-console-print!)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; def state:

(defonce subscriptions-state (atom {}))
(defonce tags-state (atom {}))
(defonce tags-metadata (atom {}))
(defonce feed-state (atom {:feed-data {:name "Loading..."}}))
(defonce article-metadata (atom {}))
(defonce search-state (atom {:visible false}))
(def HISTORY (History.))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; get stuff:

(defn init-feed-metadata [feed-state]
  (let [feed-state (transform [:articles ALL :guid] js/encodeURIComponent feed-state)
        articles   (merge (:articles feed-state) {:selected nil})
        md         (into {} (map (fn [{id :guid md :metadata}]
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
            t-state     (select-one [(submap [:tag-order :tag-content])] response)
            t-visible?  (:visible? @tags-state)]
        (reset! subscriptions-state subs-state)
        (reset! tags-state (merge {:visible? (if (nil? t-visible?) true t-visible?)} t-state))
        (reset! tags-metadata t-md))))

(defn request-feed [title]
  (set! (-> js/document (.getElementById "feed-page-wrapper") .-style .-opacity) 0) ;; fixme: it's sad this has to be here.
  (go (let [response    (<! (http/get (str "/f/" (js/encodeURIComponent title) "/42")))
            response    (h/read-json (:body response))]
        (reset! feed-state (init-feed-metadata response)))))

(defn change-feed-md [feed md]
  (go (let [new-md      (:body (<! (http/post (str "/f-md/" (js/encodeURIComponent feed)) {:json-params md})))]
        (request-feed feed))))


(defn change-article-md [feed art-id md]
  (go (let [new-md      (:body (<! (http/post (str "/a-md/" (js/encodeURIComponent feed) "/" art-id) {:json-params md})))]
        (transform [ATOM (keypath art-id) ATOM] #(merge % new-md) article-metadata)
        new-md)))

(defn toggle-tag-md [tag key]
  (go (let [k-val       (select-one [ATOM (keypath tag) ATOM (keypath key)] tags-metadata)
            k-val       (if k-val (not k-val) true)
            new-md      (:body (<! (http/post (str "/t-md/" (js/encodeURIComponent tag)) {:json-params {key k-val}})))]
        (transform [ATOM (keypath tag) ATOM] #(merge % new-md) tags-metadata))))

(defn change-article-status-md [new-state & [gguid]]
  (let [guid         (or gguid (-> @article-metadata :selected :guid))
        feed         (-> @feed-state :feed-data :name)
        cur-state    (select-one [ATOM (keypath guid) ATOM :status] article-metadata)
        cur-state    (or cur-state "unread")
        new-state    (cond (and gguid (not= cur-state "saved"))                  "read" ;; hackish, this is for auto-mark-read on article change
                           (and (= cur-state "unread") (= new-state "read"))     "read"
                           (= cur-state new-state)                               "unread"
                           :else                                                 "saved")
        update-count (fn [type cur-count]
                       (condp = type
                         new-state (inc cur-count)
                         cur-state (dec cur-count)
                         cur-count))]
    (if (and guid (not= new-state cur-state))
      (go (let [result-md (:status (<! (change-article-md feed guid {:status new-state})))]
            (when (= new-state result-md)
              (multi-transform [ATOM (keypath feed) ATOM (multi-path [:unread-count (terminal #(update-count "unread" %))]
                                                                     [:saved-count (terminal #(update-count "saved" %))])]
                               subscriptions-state))))
      (go :nothing-was-done))))

(defn change-feed-page [feed]
  (.setToken HISTORY (str "/feed/" (js/encodeURIComponent feed))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; interaction:

(defn change-article [nb & [guid]]
  "If guid is given, nb is ignored, guid is selected.
  Otherwise next article is nb relative to current article."
  (let [cur-nb         (-> @article-metadata :selected :number)
        cur-guid       (-> @article-metadata :selected :guid)
        cur-nb         (or cur-nb -1)
        articles       (-> @feed-state :articles)
        total          (count articles)
        next-nb        (+ nb cur-nb)
        next-nb        (cond (= next-nb -2)    (dec total)
                             (< next-nb 0)     0
                             (< next-nb total) next-nb
                             :else             (dec total))
        [guid next-nb] (if guid
                         [guid (count (take-while #(not= guid (:guid %)) articles))]
                         [(:guid (nth articles next-nb)) next-nb])]
    (if (not= next-nb cur-nb)
      (do (multi-transform [ATOM (multi-path [:selected (terminal-val {:number next-nb :guid guid})]
                                             [(if-path (keypath cur-guid) (keypath cur-guid)) ATOM :visible? (terminal-val false)]
                                             [(keypath guid) ATOM :visible? (terminal-val true)])]
                           article-metadata)
          (change-article-status-md "read" guid))
      (go :nothing-was-done))))

(defn change-feed [direction]
  (let [cur-f     (-> @feed-state :feed-data :name)
        tag       (-> @feed-state :feed-data :tags first) ;; fixme tags should become unique, that was a stupid idea.
        tc        ((-> @tags-state :tag-content) tag)
        sub-state @subscriptions-state
        unread?   #(let [md @(sub-state %)]
                     (or (= % cur-f) (-> md :saved-count pos?) (-> md :unread-count pos?)))
        search    (fn [p [f & fs]]
                    (if (= f cur-f)
                      (if (= direction 1) (first fs) p)
                      (if fs (recur f fs) nil)))
        res       (->> tc (filter unread?) (search nil))]
    (when res
      (change-feed-page res))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; subscriptions!

(rum/defc mk-sub < rum/reactive
  [feed show-all]
  (let [selected-feed (-> feed-state rum/react :feed-data :name)
        sub-md        (rum/react (@subscriptions-state feed))
        unread        (:unread-count sub-md)
        saved         (:saved-count sub-md)
        a             (if (zero? unread) :a.grey :a)
        div           (if (= selected-feed feed)
                        :div.subscription.selected
                        :div.subscription)]
    (when (or show-all (> unread 0) (> saved 0))
      [div {:on-click #(change-feed-page feed)}
       [:div.sub-title [a {:href (str "#/feed/" (js/encodeURIComponent feed))} feed]]
       [:div.sub-count.small unread]])))

(rum/defcs mk-tag < rum/reactive
                    (rum/local false ::show-all-read)
  [state tag feeds]
  (let [tag-md   (rum/react (@tags-metadata tag))
        v?       (:visible? tag-md)
        show-all (::show-all-read state)]
    [:div.tag
     [:a {:on-click #(toggle-tag-md tag :visible?)
          :href "javascript:void(0)"} (str (if v? "▾ " "▸ ") tag)]
     [:a.sub-show-all {:on-click #(swap! show-all not)
                       :href "javascript:void(0)"} (str (if @show-all " ▾" " ◂"))]
     (when v?
       (for [f feeds]
         (rum/with-key (mk-sub f @show-all) f)))]))

(rum/defcs mk-subscriptions < rum/reactive
                              {:did-update (fn [state]
                                             "hide/show"
                                             (let [comp       (:rum/react-component state)
                                                   feeds-node (js/ReactDOM.findDOMNode comp)
                                                   par-node   (.-parentElement feeds-node)]
                                               (set! (-> par-node .-style .-display) (if (:visible? @tags-state) "" "none"))
                                               state))}
  [state]
  (let [t-state   (rum/react tags-state)]
    [:div.feeds
     (for [tag (:tag-order t-state)
           :let [feeds ((:tag-content t-state) tag)]
           :when feeds] ;; weed out empty tags
       (rum/with-key (mk-tag tag feeds) tag))]))

(rum/mount (mk-subscriptions)
           (.getElementById js/document "subscriptions-anchor"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; feeds!

(rum/defcs mk-article < rum/reactive
                        {:did-update (fn [{[{guid :guid}] :rum/args :as state}]
                                       "focus on current article on article/feed change -> kb scrolling"
                                       (when (select-one [ATOM (keypath guid) ATOM :visible?] article-metadata)
                                         (let [comp       (:rum/react-component state)
                                               art-node   (js/ReactDOM.findDOMNode comp)
                                               feed-node  (.getElementById js/document "feed-content")]
                                           (.focus art-node)
                                           (set! (.-scrollTop feed-node) (- (.-offsetTop art-node) (.-offsetTop feed-node)))))
                                       state)}
  [state
   {media :media:content :keys [title pretty-date author description scraped link guid]}]
  (let [a-md (@article-metadata guid)
        {read-status :status visible? :visible?} (rum/react a-md)
        [a-style art-read-status] (condp = read-status
                                    "saved" [:a.title.saved "saved"]
                                    "read"  [:a.title.read ""]
                                    [:a.title "unread"])
        set-inner-html            (fn [html]
                                    {:dangerouslySetInnerHTML {:__html html}})]
    [:div.article {:tab-index -1 :style {:outline 0}
                   :on-click (when-not visible? #(change-article 0 guid))}
     [a-style {:href link :target "_blank"} title]
     [:br]
     [:div.article-info.small [:div.date (str pretty-date " — " author)] [:div.artical-satus art-read-status]]
     [:div.content {:style {:display (if visible? "" "none")}}
      [:div.scraped     (set-inner-html (:scraped-data scraped))]
      [:div.description (set-inner-html description)]
      [:div.scraped     (set-inner-html (:scraped-data-bottom scraped))]
      (when media
        (let [media-link (:url media)
              medium     (or (:medium media) (:type media) "")
              media-tag  (condp re-find medium
                           #"image" [:img {:src media-link}]
                           #"audio" [:audio {:controls true :width "100%"} [:source {:src media-link :type medium}]]
                           #"video" [:video {:controls true :width "100%"} [:source {:src media-link :type medium}]]
                           [])]
          [:div media-tag [:br] [:a {:href media-link} media-link]]))]]))

(rum/defc mk-feed-title < rum/reactive
  [ftitle]
  (let [f-md          (-> feed-state rum/react :metadata)
        visible?      (-> tags-state rum/react :visible?)
        astate        (rum/react article-metadata)
        visible-id    (-> astate :selected :guid)
        feed-exists?  (@subscriptions-state ftitle)
        sub-state     (if feed-exists?
                        (rum/react feed-exists?)
                        {:unread-count ""})
        feed-www      (:www-link sub-state)
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
    [:div.feed-title-wrapper {:style {:display (if visible? "" "none")}}
     [div-title
      [:div.title-only [:a.grey {:href feed-www :target "_blank"} ftitle]]
      [:div.feed-count.small (:unread-count sub-state)]]
     [:div.feed-controls
      (mk-select order order-values #(change-feed-md ftitle {:order (-> % .-target .-value)}))
      (mk-select view view-values #(change-feed-md ftitle {:view-art-status (-> % .-target .-value)}))]]))

(rum/defcs mk-feed < rum/reactive
                     {:did-update (fn [state]
                                    "focus on feed change"
                                    (let [feed-node (.getElementById js/document "feed-content")
                                          feed-wrapper (.getElementById js/document "feed-page-wrapper")]
                                      (.focus feed-node)
                                      (set! (.-scrollTop feed-node) 0)
                                      (set! (-> feed-wrapper .-style .-opacity) 1)
                                      state))}
  [state]
  (let [fstate      (rum/react feed-state)
        ftitle      (-> fstate :feed-data :name)
        articles    (:articles fstate)]
    [:div#feed-page-wrapper
     (mk-feed-title ftitle)
     [:div#feed-content
      (for [a articles
            :let [rum-key (:guid a)]]
        (rum/with-key (mk-article a) rum-key))]]))

(rum/mount (mk-feed)
           (.getElementById js/document "feed-anchor"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; search:

(defn select-search-feed []
  (when-let [n    (:nth @search-state)]
    (change-feed-page (nth (:results @search-state) n)))
  (reset! search-state {:visible false}))

(defn search [text]
  (let [subs  (select-one [ATOM :subscriptions] search-state)
        re    (re-pattern (str "(?i)" (apply str (interpose ".*?" text))))
        res   (when-not (empty? text)
                (->> subs
                     (map #(vector (re-find re %) %))
                     (filter first)
                     (sort-by #(-> % first count))
                     (take 10)
                     (map second)))]
    (multi-transform [ATOM (multi-path [:results (terminal-val res)]
                                       [:nth (terminal-val (if res 0 nil))])]
                     search-state)))

(rum/defcs mk-search < rum/reactive
                      {:did-update (fn [state]
                                     "Auto focus on input"
                                     (let [comp     (:rum/react-component state)
                                           dom-node (js/ReactDOM.findDOMNode comp)]
                                       (when dom-node ;; not present when search closes
                                         (-> dom-node .-firstChild .-firstChild .focus)
                                         (set! (-> dom-node .-style .-opacity) 1))
                                       state))}
  [state]
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
                                                    (= 13 kcode) (select-search-feed)))
                              :on-change #(search (-> % .-target .-value))}]
        [:div (vec res)]]])))

(rum/mount (mk-search)
           (.getElementById js/document "search-anchor"))


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
              "J" (change-feed 1)
              "K" (change-feed -1)
              "j" (<! (change-article 1))
              "k" (<! (change-article -1))
              "m" (<! (change-article-status-md "read"))
              "s" (<! (change-article-status-md "saved"))
              "b" (setval [ATOM] {:visible true :subscriptions (sort (select [ATOM ALL FIRST] subscriptions-state))} search-state)
              "r" (request-feed (select-one [ATOM :feed-data :name] feed-state))
              "R" (do (request-subscriptions)
                      (request-feed (select-one [ATOM :feed-data :name] feed-state)))
              "v" (do (let [guid (select-one [ATOM :selected :guid] article-metadata)
                            link (select-one [ATOM :articles ALL #(= guid (:guid %)) :link] feed-state)] ;; specter is awesome, my state structure isn't.
                        (.open js/window link))) ;; FIXME: for this to work in chrome (tab instead of pop up), we'd need to .open in the listen callback, which is annoying.
              "G" (let [dom-node (.getElementById js/document "feed-content")]
                    (set! (.-scrollTop dom-node) (.-scrollHeight dom-node)))
              "g" (let [dom-node (.getElementById js/document "feed-content")]
                    (set! (.-scrollTop dom-node) 0))
              "f" (transform [ATOM :visible?] not tags-state)
              :else-nothing))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; init, takes care of setting up history:

(defroute "/feed/:feed" {:as params}
  (request-feed (-> params :feed js/decodeURIComponent)))
;; FIXME add / route

(defn init []
  "init history, load subscriptions"
  (request-subscriptions)
  (goog.events/listen HISTORY EventType/NAVIGATE #(secretary/dispatch! (-> % .-token)))
  (doto HISTORY (.setEnabled true)))

(init)

(defn on-js-reload [])
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
