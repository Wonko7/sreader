(ns client.style
  (:require [garden.def :refer [defstyles]]))

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
        ]
    [:body {:background sol-dark-blue
            :padding 0
            :margin 0
            :height "100vh"}
     [:#page {:flex 1 ;; no?
              :display :flex
              :height "100vh"
              :flex-direction :row
              }
      [:#subscriptions-anchor {:flex "0 0 20%"
                               :overflow-y :scroll }
       [:.feeds {:padding "1em"
                 :background sol-ddark-blue
                 :min-height "100%"}
        [:.tag {:color sol-green} ;; FIXME was green, FIXME useless?
         [:a {:color sol-orange}]
         [:a.sub-show-all {:color sol-grey
                           :float :right
                           }]
         [:.selected {:background sol-dark-blue
                      :border-width 0
                      :border-radius ".25em"}]
         [:.subscription {:display :flex
                          :align-items :center
                          :padding-left ".5em"
                          :padding ".3em"}
          [:.sub-title {:flex "0 0 90%"}
           [:a {:color sol-green}]
           [:a.grey {:color sol-grey2}]
           ]
          [:.sub-count {:flex 2
                        :padding-left ".9em"}]]]]]
      [:#feed-anchor {:flex 2
                      :height "100vh"}
       [:#feed-page-wrapper {:height "100vh"
                             :overflow :hidden
                             :display :flex
                             :flex-direction :column}
        [:.feed-title-wrapper {:flex "0 1 auto"
                               :display :flex
                               :flex-direction :row
                               :padding-left "2em"
                               :padding-right "2em"
                               :margin-top "1em"
                               :margin-bottom "1em"
                               }
         [:.feed-title-small {:font-size "1em"}]
         [:.feed-title {:font-size "2em"}]
         [:.feed-title-small
          :.feed-title {:flex "0 0 80%"
                        :font-weight :bold
                        :color sol-grey
                        :display :flex
                        :align-items :baseline}
          [:.title-only {:flex "0 1 auto"}]
          [:.feed-count {:flex :auto
                         :margin-left "1em"}]]
         [:.feed-controls {:flex :auto
                           :text-align :right}
          [:select {:background sol-dark-blue
                    :color sol-grey
                    :border :none}]]]
        [:#feed-content {:flex "0 1 auto"
                         :overflow-y :scroll
                         :height "100%"
                         :padding-left "2em"
                         :padding-right "2em"}
         [:.article {:font-size "1.2em"
                     :border-width 0
                     :border-radius ".5em"
                     :color sol-grey
                     :background sol-ddark-blue
                     :padding "1em"
                     :margin-bottom "1em"}
          [:a.title {:font-size "1.2em"}]
          [:a.title.saved {:color sol-green}]
          [:a.title.read {:color sol-grey}]
          [:.article-info {:display :flex}
           [:.date {:flex :auto}]
           [:.article-status {:margin-left "1em"
                              :flex :auto}]]
          [:.scraped {:max-width "100%"}
           [:img {:max-width "100%"}]
           [:.video-wrapper {:position :relative
                             :width "100%"
                             :height 0
                             :padding-bottom "56%"}
            [:iframe {:position :absolute
                      :width "100%"
                      :height "100%"
                      :top 0
                      :left 0}]]]
          [:.content {:width "75%"
                      :text-align :justify
                      :padding-top "1em"}]]]]]
      [:#search-wrapper {:position :fixed
                         :top 0
                         :left 0
                         :height "100%"
                         :width "100%"
                         :background "rgba(0, 0, 0, 0.5)"}
       [:#search {:position :fixed
                  :top "10%"
                  :left "10%"
                  :width "30%"
                  :background sol-dark-blue
                  :color sol-green
                  :padding "1em"
                  :font-size "1.2em"
                  :border-width 0
                  :border-radius ".5em"}
        [:input {:width "100%"
                 :padding 0
                 :margin-bottom "0.5em"
                 :font-size "1.2em"
                 :background sol-grey3}]
        [:.first-result {:color sol-orange
                         :font-weight :bold}]]]
      [:.left {:float :right}]]
     ;; global:
     [(a-all-states {:text-decoration :none
                     :color sol-orange
                     })]
     [:a.grey {:color sol-grey2}]
     [:.small {:font-size "0.75em"
               :color sol-grey2}]
     ]))
