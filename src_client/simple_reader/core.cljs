(ns simple-reader.core
  (:require [rum.core :as rum]
            [cljs-http.client :as http]
            [cognitect.transit :as json]
            [cljs.core.async :refer [chan <! >!] :as a])
  (:require-macros [cljs.core.async.macros :as m :refer [go-loop go]]
                   [utils.macros :refer [<? <?? go? go-try dprint]]))

(enable-console-print!)
(def host "http://localhost:3000")

;; define your app data so that it doesn't get over-written on reload


;; FIXME tmp:
(declare request-feed)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; subscriptions!

(defonce subscriptions-state (atom {:feeds [{:title "xkcd"} {:title "slashdot"}]}))

(rum/defc mk-subscriptions < rum/reactive []
  (let [subs (:feeds (rum/react subscriptions-state))]
    (println subs)
    [:div.feeds
     (for [{t :title} subs]
       [:div#subscription {:on-click #(request-feed t)}
        ; [:br]
        [:a t]])]
    )) 

(rum/mount (mk-subscriptions)
           (. js/document (getElementById "subscriptions")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; feeds!

(defonce feed-state (atom {:feed-data {:title "Loading..."}}))

(defn mk-article [title date desc link]
  [[:br] [:br]
   [:div.article
    [:a.title {:href link} title]
    [:br]
    [:div.small date]
    [:br]
    [:div.content desc]]])

(rum/defc mk-feed < rum/reactive []
  (let [state (rum/react feed-state)
        ftitle (-> state :feed-data :title)
        articles (:articles state)]
    [:div.feed [:h1 ftitle]
     (for [{t :title d :date desc :description l :link} articles]
       (mk-article t d desc l)
       )]))

(rum/mount (mk-feed)
           (. js/document (getElementById "feed")))

(defn request-feed [title]
  (println :called :rf title)
  (go (let [json-reader (json/reader :json)
                             response (<! (http/get (str "/f/" title "/42")))
                             response (json/read json-reader (:body response))]
        (println :got response)
        (reset! feed-state response))))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
