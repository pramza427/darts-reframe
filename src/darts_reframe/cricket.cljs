(ns darts-reframe.cricket
  (:require [clojure.string :as str]
            [re-frame.core :as rf]))


;; ---------- Subscriptions ----------
(rf/reg-sub
  ::player-1-name
  (fn [db [_]]
    (get-in db [:game-state :player-1 :name])))

(rf/reg-sub
  ::player-2-name
  (fn [db [_]]
    (get-in db [:game-state :player-2 :name])))

(rf/reg-sub
  ::player-score
  (fn [db [_ player score]]
    (get-in db [:game-state player :score score])))

(rf/reg-sub
  ::end-game?
  (fn [db [_]]
    (get-in db [:game-state :end-game?])))

(rf/reg-sub
  ::total-score
  (fn [db [_ player]]
    (->> (get-in db [:game-state player :score])
         (map (fn [[score amount]]
                (if (< 3 amount)
                  (* score (- amount 3))
                  0)))
         (reduce +))))

(rf/reg-sub
  ::number-open?
  (fn [db [_ score]]
    (let [score-1 (get-in db [:game-state :player-1 :score score])
          score-2 (get-in db [:game-state :player-2 :score score])]
     (cond
       (and (<= 3 score-1) (<= 3 score-2)) :closed
       (<= 3 score-1) :player-1
       (<= 3 score-2) :player-2
       :else :none))))

;; ---------- Events ----------
(rf/reg-event-db
  ::undo
  (fn [db [_]]
    (let [to-state (first (get db :old-states))]
      (-> db
          (assoc :old-states (rest (get db :old-states)))
          (assoc :game-state to-state)))))

(rf/reg-event-db
  ::set-player-1-name
  (fn [db [_ val]]
    (assoc-in db [:game-state :player-1 :name] val)))

(rf/reg-event-db
  ::set-player-2-name
  (fn [db [_ val]]
    (assoc-in db [:game-state :player-2 :name] val)))

(rf/reg-event-db
  ::cap-name
  (fn [db [_ player]]
    (let [name (get-in db [:game-state player :name])]
      (assoc-in db [:game-state player :name] (str/capitalize name)))))

(rf/reg-event-db
  ::add-player-score
  (fn [db [_ player score amount]]
    (let [score-1 (get-in db [:game-state :player-1 :score score])
          score-2 (get-in db [:game-state :player-2 :score score])
          current-amount (get-in db [:game-state player :score score])]
      (cond
        (and (<= 3 score-1) (<= 3 score-2)) db
        (and (= player :player-1) (<= 3 score-2)) (-> db
                                                      (update :old-states (fn [e] (take 20 (conj e (get db :game-state)))))
                                                      (assoc-in [:game-state player :score score] (min 3 (+ current-amount amount))))
        (and (= player :player-2) (<= 3 score-1)) (-> db
                                                      (update :old-states (fn [e] (take 20 (conj e (get db :game-state)))))
                                                      (assoc-in [:game-state player :score score] (min 3 (+ current-amount amount))))
        :else (-> db
                  (update :old-states (fn [e] (take 20 (conj e (get db :game-state)))))
                  (assoc-in [:game-state player :score score] (+ current-amount amount)))))))

;; ---------- Helpers -----------


;; ---------- Views -----------
(defn winner-screen []
  (let [end-game? @(rf/subscribe [::end-game?])]
    (when end-game?
     [:div.absolute.flex.justify-center.items-center.bg-opacity-80.bg-gray-500.w-full
      {:style {:height  "95%"
               :z-index 100}}
      [:div.flex.flex-col.items-center.bg-white.w-1_2.h-1_2
       [:div.flex.w-full.justify-end [:div [:i.fas.fa-times]]]
       [:div.font-bold.text-3xl "The Winner is:"]
       [:div.font-bold.text-3xl "FIX ME"]
       [:div.flex.w-1_2.justify-evenly [:button "Restart"] [:button "Close"]]]])))


(defn grid-column [score]
  (let [player-1-score @(rf/subscribe [::player-score :player-1 score])
        player-2-score @(rf/subscribe [::player-score :player-2 score])]
   [:<>
    [:div.flex-center.text-2xl (if (< 3 player-1-score)
                     (* score (- player-1-score 3))
                     "0")]
    [:div.flex.justify-center
     [:button.cricket-btn {:on-click #(rf/dispatch [::add-player-score :player-1 score 3])} "+3"]
     [:button.cricket-btn {:on-click #(rf/dispatch [::add-player-score :player-1 score 2])} "+2"]
     [:button.cricket-btn {:on-click #(rf/dispatch [::add-player-score :player-1 score 1])} "+1"]]
    [:div.flex-center
     [:div.h-full.w-2.border.border-black.m-1 {:class (when (>= player-1-score 1) "bg-red-500")}]
     [:div.h-full.w-2.border.border-black.m-1 {:class (when (>= player-1-score 2) "bg-red-500")}]
     [:div.h-full.w-2.border.border-black.m-1 {:class (when (>= player-1-score 3) "bg-red-500")}]]
    [:div.flex-center.text-4xl
     {:class (case @(rf/subscribe [::number-open? score])
               :player-1
               "border-8 border-red-700"
               :player-2
               "border-8 border-blue-700"
               :closed
               "line-through"
               ;else
               "")}
     (if (= score 25) "Bullseye" score)]
    [:div.flex-center
     [:div.h-full.w-2.border.border-black.m-1 {:class (when (>= player-2-score 1) "bg-blue-500")}]
     [:div.h-full.w-2.border.border-black.m-1 {:class (when (>= player-2-score 2) "bg-blue-500")}]
     [:div.h-full.w-2.border.border-black.m-1 {:class (when (>= player-2-score 3) "bg-blue-500")}]]
    [:div.flex-center
     [:button.cricket-btn {:on-click #(rf/dispatch [::add-player-score :player-2 score 1])} "+1"]
     [:button.cricket-btn {:on-click #(rf/dispatch [::add-player-score :player-2 score 2])} "+2"]
     [:button.cricket-btn {:on-click #(rf/dispatch [::add-player-score :player-2 score 3])} "+3"]]
    [:div.flex-center.text-2xl (if (< 3 player-2-score)
                     (* score (- player-2-score 3))
                     "0")]]))

(defn cricket []
  [:div.flex.justify-center.w-full.overflow-auto
   [winner-screen]
   [:div.relative {:style {:max-width "60rem"}}
    [:div.text-center.font-semibold.text-4xl.p-4 "Cricket"]
    [:div.absolute.top-3.right-2.flex-center.border.border-black.rounded-full.px-2.hover:bg-blue-200
     [:button.pr-2 {:on-click #(rf/dispatch [::undo])} "Undo"]
     [:i.fa-solid.fa-rotate-left.fa-sm]]
    [:div.grid.gap-4.text-2xl {:style {:grid-template-columns "1fr 2fr 1fr 2fr 1fr 2fr 1fr"}}
     [:div.flex.justify-center @(rf/subscribe [::total-score :player-1])]
     [:input.col-span-2.bg-transparent.text-center.border-b-2.border-red-500
      {:type        "text"
       :placeholder "Player 1"
       :value       @(rf/subscribe [::player-1-name])
       :on-change   #(rf/dispatch [::set-player-1-name (-> % .-target .-value)])
       :on-blur     #(rf/dispatch [::cap-name :player-1])}]
     [:div]
     [:input.col-span-2.bg-transparent.text-center.border-b-2.border-blue-500
      {:type        "text"
       :placeholder "Player 2"
       :value       @(rf/subscribe [::player-2-name])
       :on-change   #(rf/dispatch [::set-player-2-name (-> % .-target .-value)])
       :on-blur     #(rf/dispatch [::cap-name :player-2])}]
     [:div.flex.justify-center @(rf/subscribe [::total-score :player-2])]

     (doall
       (for [score (range 15 21)]
         ^{:key score}
         [grid-column score]))

     [grid-column 25]]]])