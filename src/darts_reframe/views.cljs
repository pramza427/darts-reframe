(ns darts-reframe.views
  (:require
    [re-frame.core :as rf]
    [darts-reframe.subs :as subs]
    [darts-reframe.cricket :refer [cricket]]
    [darts-reframe.countdown :refer [countdown]]
    [darts-reframe.ticktackdarts :refer [ticktackdarts]]))

;; ---------- Subscriptions ----------
(rf/reg-sub
  ::current-view
  (fn [db [_]]
    (or (get db :current-view) :cricket)))

;; ---------- Events ----------
(rf/reg-event-db
  ::set-current-view
  (fn [db [_ view]]
    (case view
      :cricket
      (assoc {} :current-view view)

      :501
      (-> {}
          (assoc :current-view view)
          (assoc :temp-options {:players 2 :teams 2 :score 501}))

      :ticktackdarts
      (assoc {} :current-view view)
      )))

;; ---------- Views ----------
(defn top-bar []
  [:div.w-full.flex.border-b.border-gray-500.bg-gray-700.text-white.text-lg.text-semibold
   [:button.p-1.px-2.my-1.border-r.border-gray-500.hover:bg-gray-600
    {:on-click #(rf/dispatch [::set-current-view :cricket])}
    "Cricket"]
   [:button.p-1.px-2.my-1.border-r.border-gray-500.hover:bg-gray-600
    {:on-click #(rf/dispatch [::set-current-view :501])}
    "501"]
   [:button.p-1.px-2.my-1.border-r.border-gray-500.hover:bg-gray-600
    {:on-click #(rf/dispatch [::set-current-view :ticktackdarts])}
    "Tick Tack Darts"]
   [:div.flex.flex-grow]
   [:button.py-1.px-2.m-1.hover:bg-gray-600
    {:on-click (fn [] (.classList.toggle (js/document.getElementById "top-elem") "dark"))}
    [:i.far.fa-moon]]
   [:button.py-1.px-2.m-1.hover:bg-gray-600 "About"]])

(defn main-panel []
  (let [current-view @(rf/subscribe [::current-view])]
    [:div.bg-gray-200.dark:bg-gray-900.dark:text-gray-300
     {:style {:width "100vw" :height "100vh"}}
     [top-bar]
     (case current-view
       :cricket [cricket]
       :501 [countdown]
       :ticktackdarts [ticktackdarts]
       [countdown])]))

