(ns simple-reader.core
  (:require [rum.core :as rum]
            [cljs-http.client :as http]
            [cognitect.transit :as json]
            [cljs.core.async :refer [chan <! >!] :as a])
  (:require-macros [cljs.core.async.macros :as m :refer [go-loop go]]
                   [utils.macros :refer [<? <?? go? go-try dprint]]))

(enable-console-print!)
(def host "http://localhost:3000")

(println "This text is printed from src/simple-reader/core.cljs. Go ahead and edit it and see reloading in action.")
(println :lol)

;; define your app data so that it doesn't get over-written on reload

(defonce feed-state (atom {:feed-data {:title "ok"}}))

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
    (println "we were called:" ftitle)
    (println state)
    [:div.feed [:h1 ftitle]
     (for [{t :title d :date desc :description l :link} articles]
       (mk-article t d desc l)
       )]))

(rum/mount (mk-feed)
           (. js/document (getElementById "app")))

(go (let [json-reader (json/reader :json)
          response (<! (http/get "/f/xkcd/42"))
          response (json/read json-reader (:body response))]
      (println :got response)
      (reset! feed-state response)))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
