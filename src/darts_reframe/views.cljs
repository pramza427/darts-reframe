(ns darts-reframe.views
  (:require
    [re-frame.core :as rf]
    [reagent.core :as rc]
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

;; ---------- Helpers ----------

(defn set-local-storage [key val]
  (.setItem (.-localStorage js/window) key val))

(defn get-local-storage [key]
  (.getItem (.-localStorage js/window ) key))

(defn toggle-dark-mode []
  (let [dark? (or (get-local-storage "dark") "true")]
    (set-local-storage "dark" (if (= dark? "true") false true))
    (.classList.toggle (js/document.getElementById "top-elem") "dark")))

(defn init-dark-mode []
  (let [dark? (get-local-storage "dark")]
    (when (= dark? "false")
     (.classList.remove (js/document.getElementById "top-elem") "dark"))))

;; ---------- Views ----------
(defn top-bar []
  [:div.w-full.flex.border-b.border-gray-500.bg-gray-700.text-white.text-lg.text-semibold
   {:style {:height "3rem"}}
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
    {:on-click #(toggle-dark-mode)}
    [:i.far.fa-moon]]
   [:button.py-1.px-2.m-1.hover:bg-gray-600 "About"]])

(defn main-panel []
  (rc/create-class
    {
    :component-did-mount
     (fn []
       (init-dark-mode))
     :render
     (fn []
       (let [current-view @(rf/subscribe [::current-view])]
         [:div.bg-gray-200.dark:bg-gray-900.dark:text-gray-300.overflow-hidden
          {:style {:width "100vw" :height "100vh"}}
          [top-bar]
          [:div.overflow-auto.w-full.scrollbar-thin {:style {:max-height "calc(100% - 3rem)"}}
           (case current-view
             :cricket [cricket]
             :501 [countdown]
             :ticktackdarts [ticktackdarts]
             [countdown])]]))}))

