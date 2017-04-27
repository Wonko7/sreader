(ns client.style
  (:require [garden.def :refer [defstyles]]
            [com.rpl.specter :as s :refer [setval select-one select transform must if-path cond-path multi-transform multi-path filterer keypath pred submap terminal-val terminal srange ALL ATOM FIRST MAP-VALS]]
            [utils.macros :refer [dprint !]]
            ))

(defstyles style []
  (let [;; colors:
        sol-green       :green ;;"#859900"
        sol-orange      "#cb4b16"
        sol-red         "#dc323f"
        sol-dark-blue   "#073642"
        sol-ddark-blue  "#002b36"
        sol-grey        "#93a1a1"
        sol-grey2       "#888888"
        sol-grey3       "#AAAAAA"
        green           :green
        ;; flex-child/parent padding margin
        a-all-states    (fn [style] [:a:link :a:visited :a:hover :a:active style])
        mk-trbl         (fn [k values]
                          (if (or (string? values) (number? values))
                            {k values}
                            (let [{t :t b :b r :r l :l all :a} values
                                  vs (->> [t r b l]
                                          (map #(or % all 0))
                                          (interpose " ")
                                          (apply str))]
                              {k vs})))
        margin          (partial mk-trbl :margin)
        padding         (partial mk-trbl :padding)
        flex-parent     (fn [dir]
                          {:display :flex
                           :flex-direction dir})
        flex-child      (fn [f]
                          {:flex f})
        round-corners   (fn [radius]
                          {:border-width 0
                           :border-radius radius})
        m               merge
        ]
    (dprint (! :#page :lol [(flex-child :lol)
                            {:border 0}]))
    [:body (m (margin 0)
              (padding 0)
              {:background sol-dark-blue
               :height "100vh"})
     [:#page (m (flex-parent :row)
                {:height "100vh"})
      [:#subscriptions-anchor (m (flex-child "0 0 20%")
                                 {:overflow-y :scroll})
       [:.feeds (m (padding "1em")
                   {:background sol-ddark-blue
                    :min-height "100%"})
        [:.tag {:color sol-green}
         [:a {:color sol-orange}]
         [:a.sub-show-all {:color sol-grey
                           :float :right}]
         [:.selected (m (round-corners ".25em")
                        {:background sol-dark-blue})]
         [:.subscription (m (padding {:l ".5em" :a ".3em"})
                            (flex-parent :row)
                            {:align-items :center})
          [:.sub-title (flex-child "0 0 90%")
           [:a {:color sol-green}]
           [:a.grey {:color sol-grey2}]]
          [:.sub-count (m (flex-child 2)
                          {:padding-left ".9em"})]]]]]
      [:#feed-anchor (m (flex-child 2)
                        {:height "100vh"})
       [:#feed-page-wrapper (m (flex-parent :column)
                               {:height "100vh"
                                :overflow :hidden})
        [:.feed-title-wrapper (m (flex-child "0 1 auto")
                                 (flex-parent :row)
                                 (padding {:l "2em" :r "2em"})
                                 (margin {:t "1em" :b "1em"}))
         [:.feed-title-small {:font-size "1em"}]
         [:.feed-title {:font-size "2em"}]
         [:.feed-title-small
          :.feed-title (m (flex-child "0 0 80%")
                          (flex-parent :row)
                          {:font-weight :bold
                           :color sol-grey
                           :align-items :baseline})
          [:.title-only (flex-child "0 1 auto")]
          [:.feed-count (m (flex-child :auto)
                           (margin {:l "1em"}))]]
         [:.feed-controls (m (flex-child :auto)
                             {:text-align :right})
          [:select {:background sol-dark-blue
                    :color sol-grey
                    :border :none}]]]
        [:#feed-content (m (flex-child "0 1 auto")
                           (padding {:r "2em" :l "2em"})
                           {:overflow-y :scroll
                            :height "100%"})
         [:.article (m (round-corners ".5em")
                       (padding "1em")
                       (margin {:b "1em"})
                       {:font-size "1.2em"
                        :color sol-grey
                        :background sol-ddark-blue})
          [:a.title {:font-size "1.2em"}]
          [:a.title.saved {:color sol-green}]
          [:a.title.read {:color sol-grey}]
          [:.article-info (flex-parent :row)
           [:.date (flex-child :auto)]
           [:.article-status (m (flex-child :auto)
                                (margin {:l "1em"}))]]
          [:.scraped {:max-width "100%"}
           [:img {:max-width "100%"}]
           [:.video-wrapper (m (padding {:b "56%"})
                               {:position :relative
                                :width "100%"
                                :height 0})
            [:iframe {:position :absolute
                      :width "100%"
                      :height "100%"
                      :top 0
                      :left 0}]]]
          [:.content (m (padding {:t "1em"})
                        {:width "75%"
                         :text-align :justify})]]]]]]
     [:#search-wrapper {:position :fixed
                        :top 0
                        :left 0
                        :height "100%"
                        :width "100%"
                        :background "rgba(0, 0, 0, 0.5)"}
      [:#search (m (round-corners ".5em")
                   (padding "1em")
                   {:position :fixed
                    :top "10%"
                    :left "10%"
                    :width "30%"
                    :background sol-dark-blue
                    :color sol-green
                    :font-size "1.2em"})
       [:input (m (margin {:b "0.5em"})
                  {:width "100%"
                   :font-size "1.2em"
                   :background sol-grey3})]
       [:.first-result {:color sol-orange
                        :font-weight :bold}]]]
     ;; global:
     [(a-all-states {:text-decoration :none
                     :color sol-orange})]
     [:a.grey {:color sol-grey2}]
     [:.small {:font-size "0.75em"
               :color sol-grey2}]
     ]))