(ns darts-reframe.views
  (:require
    [re-frame.core :as rf]
    [darts-reframe.subs :as subs]
    [darts-reframe.cricket :refer [cricket]]
    [darts-reframe.countdown :refer [countdown]]))

;; ---------- Subscriptions ----------
(rf/reg-sub
  ::current-view
  (fn [db [_]]
    (or (get db ::current-view) :cricket)))

;; ---------- Events ----------
(rf/reg-event-db
  ::set-current-view
  (fn [db [_ view]]
    (assoc db ::current-view view)))

;; ---------- Views ----------
(defn top-bar []
  [:div.w-full.flex.justify-between.border-b.border-gray-500.bg-gray-700.text-white.text-lg.text-semibold
   [:div.flex
    [:button.inline-block.p-1.px-2.my-1.border-r.border-gray-500.hover:bg-gray-600
     {:on-click #(rf/dispatch [::set-current-view :cricket])}
     "Cricket"]
    [:button.inline-block.p-1.px-2.my-1.border-r.border-gray-500.hover:bg-gray-600
     {:on-click #(rf/dispatch [::set-current-view :501])}
     "501"]]
   [:button.inline-block.p-1.px-2.my-1.hover:bg-gray-600 "About"]])

(defn main-panel []
  (let [current-view @(rf/subscribe [::current-view])]
    [:div.bg-gray-200 {:style {:width "100vw" :height "100vh"}}
     [top-bar]
     (case current-view
       :cricket [cricket]
       :501 [countdown]
       :xox []
       [countdown])]))

