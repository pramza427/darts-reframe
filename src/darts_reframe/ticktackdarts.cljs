(ns darts-reframe.ticktackdarts
  (:require [clojure.string :as str]
            [re-frame.core :as rf]))

(def easy ["1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "11" "12" "13" "14" "15" "16" "17" "18" "19" "20"])
(def medium ["Inner 1" "Outer 1" "Inner 2" "Outer 2" "Inner 3" "Outer 3" "Inner 4" "Outer 4" "Inner 5" "Outer 5" "Inner 6" "Outer 6" "Inner 7" "Outer 7" "Inner 8" "Outer 8" "Inner 9" "Outer 9" "Inner 10" "Outer 10" "Inner 11" "Outer 11" "Inner 12" "Outer 12" "Inner 13" "Outer 13" "Inner 14" "Outer 14" "Inner 15" "Outer 15" "Inner 16" "Outer 16" "Inner 17" "Outer 17" "Inner 18" "Outer 18" "Inner 19" "Outer 19" "Inner 20" "Outer 20"])
(def hard ["Triple 1" "Double 1" "Triple 2" "Double 2" "Triple 3" "Double 3" "Triple 4" "Double 4" "Triple 5" "Double 5" "Triple 6" "Double 6" "Triple 7" "Double 7" "Triple 8" "Double 8" "Triple 9" "Double 9" "Triple 10" "Double 10" "Triple 11" "Double 11" "Triple 12" "Double 12" "Triple 13" "Double 13" "Triple 14" "Double 14" "Triple 15" "Double 15" "Triple 16" "Double 16" "Triple 17" "Double 17" "Triple 18" "Double 18" "Triple 19" "Double 19" "Triple 20" "Double 20" "Inner 1" "Outer 1" "Inner 2" "Outer 2" "Inner 3" "Outer 3" "Inner 4" "Outer 4" "Inner 5" "Outer 5" "Inner 6" "Outer 6" "Inner 7" "Outer 7" "Inner 8" "Outer 8" "Inner 9" "Outer 9" "Inner 10" "Outer 10" "Inner 11" "Outer 11" "Inner 12" "Outer 12" "Inner 13" "Outer 13" "Inner 14" "Outer 14" "Inner 15" "Outer 15" "Inner 16" "Outer 16" "Inner 17" "Outer 17" "Inner 18" "Outer 18" "Inner 19" "Outer 19" "Inner 20" "Outer 20"])

;; ---------- Subscriptions ----------

(rf/reg-sub
  ::difficulty
  (fn [db _]
    (get-in db [:game-state :difficulty])))

(rf/reg-sub
  ::tile-owned-by
  (fn [db [_ num]]
    (get-in db [:game-state :tiles num])))

;; ---------- Events ----------

(rf/reg-event-db
  ::difficulty
  (fn [db [_ val]]
    (assoc-in db [:game-state :difficulty] val)))

(rf/reg-event-db
  ::set-owner
  (fn [db [_ num val]]
    (assoc-in db [:game-state :tiles num] val)))
;; ---------- Helpers -----------

;; ---------- Views -----------

(defn setup []
  [:div.text-3xl
   [:div.flex-center.pt-10.pb-6 "Difficulty"]
   [:div.flex-center
    [:button.count-btn.w-40
     {:on-click (fn [] (rf/dispatch [::difficulty :easy]))}
     "Easy"]
    [:button.count-btn.w-40 {:on-click (fn [] (rf/dispatch [::difficulty :medium]))} "Medium"]
    [:button.count-btn.w-40 {:on-click (fn [] (rf/dispatch [::difficulty :hard]))} "Hard"]
    ]])

(defn tile [num val]
  (let [owner @(rf/subscribe [::tile-owned-by num])]
    (js/console.log owner)
    [:div.p-4.flex-center.border.rounded.relative.w-40.h-40
     {:class (case owner :red "bg-red-500 bg-opacity-50 border-red-600"
                         :blue "bg-blue-500 bg-opacity-50 border-blue-600"
                         "border-gray-600")}
     [:div val]
     (when (not owner)
       [:<>
        [:button.absolute.left-0.w-1_2.h-full.hover:bg-blue-500.hover:bg-opacity-30
         {:on-click #(rf/dispatch [::set-owner num :blue])}]
        [:button.absolute.right-0.w-1_2.h-full.hover:bg-red-500.hover:bg-opacity-30
         {:on-click #(rf/dispatch [::set-owner num :red])}]])]))

(defn ticktackdarts []
  (let [difficulty @(rf/subscribe [::difficulty])
        options (case difficulty :easy easy :medium medium :hard hard easy)
        vals (shuffle options)]
    (if (not difficulty)
      [setup]
      [:div.flex.justify-center.w-full.overflow-auto
       [:div.relative {:style {:max-width "60rem"}}
        [:div.text-center.font-semibold.text-4xl.p-4 "Tick Tack Darts"]
        [:div.grid.gap-4.text-2xl {:style {:grid-template-columns "1fr 1fr 1fr"}}
         (doall
           (for [num (range 9)]
             (if (= num 4)
               ^{:key num}
               [tile num "Bullseye"]
               ^{:key num}
               [tile num (get vals num)])))
         ]]])))