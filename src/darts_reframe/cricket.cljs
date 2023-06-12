(ns darts-reframe.cricket
  (:require [clojure.string :as str]
            [re-frame.core :as rf]))

(def NUMBERS [15 16 17 18 19 20 25])
(def EXTRA-NUMBERS [11 12 13 14 15 16 17 18 19 20 25])
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
  ::added-score
  (fn [db [_ player]]
    (get-in db [:game-state player :added-score])))

(rf/reg-sub
  ::end-game?
  (fn [db [_]]
    (->> NUMBERS
         (map (fn [number]
                (and (<= 3 (get-in db [:game-state :player-1 :score number]))
                     (<= 3 (get-in db [:game-state :player-2 :score number])))))
         (every? true?))))

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
  ::set-added-score
  (fn [db [_ player val]]
    (assoc-in db [:game-state player :added-score] val)))

(rf/reg-event-db
  ::cap-name
  (fn [db [_ player]]
    (let [name (get-in db [:game-state player :name])]
      (assoc-in db [:game-state player :name] (str/capitalize name)))))

(rf/reg-event-fx
  ::add-player-score
  (fn [{:keys [db]} [_ player score]]
    (let [score-1 (get-in db [:game-state :player-1 :score score])
          score-2 (get-in db [:game-state :player-2 :score score])
          current-amount (get-in db [:game-state player :score score])]
      (cond
        (and (<= 3 score-1) (<= 3 score-2)) db
        (and (= player :player-1) (<= 3 score-2)) {:db (-> db
                                                       (update :old-states (fn [e] (take 20 (conj e (get db :game-state)))))
                                                       (assoc-in [:game-state player :score score] (min 3 (+ current-amount 1))))}
        (and (= player :player-2) (<= 3 score-1)) {:db (-> db
                                                       (update :old-states (fn [e] (take 20 (conj e (get db :game-state)))))
                                                       (assoc-in [:game-state player :score score] (min 3 (+ current-amount 1))))}
        :else {:db (-> db
                   (update :old-states (fn [e] (take 20 (conj e (get db :game-state)))))
                   (assoc-in [:game-state player :score score] (+ current-amount 1))
                   (assoc-in [:game-state player :added-score] true)
                   (assoc-in [:game-state player :added-score] true))
               :dispatch-later {:ms 300 :dispatch [::set-added-score player false]}}))))

(rf/reg-event-db
  ::reset-game
  (fn [db [ one two three]]
    (js/console.log db one two three)
    (-> db
        (dissoc :old-states)
        (update-in [:game-state :player-1] dissoc :score)
        (update-in [:game-state :player-2] dissoc :score))))
;; ---------- Helpers -----------


;; ---------- Views -----------
(defn winner-screen []
  (let [end-game? @(rf/subscribe [::end-game?])
        player-1-name (or @(rf/subscribe [::player-1-name]) "Player 1")
        player-1-score @(rf/subscribe [::total-score :player-1])
        player-2-name (or @(rf/subscribe [::player-2-name]) "Player 2")
        player-2-score @(rf/subscribe [::total-score :player-2])]
    (when end-game?
      [:div.absolute.flex.justify-center.items-center.bg-opacity-80.bg-gray-500.w-full
       {:style {:height  "95%"
                :z-index 100}}
       [:div.flex.flex-col.items-center.bg-gray-200.w-1_2.h-1_2.p-4.rounded-lg.text-4xl.dark:bg-gray-900
        [:div.font-semibold "The Winner is:"]
        [:div.font-bold
         (cond
           (= player-1-score player-2-score)
           "Both Players!"
           (< player-1-score player-2-score)
           player-2-name
           :else
           player-1-name)]
        [:div.pt-10 "Final Scores:"]
        [:div.grid.gap-4 {:style {:grid-template-columns "1fr 1fr"}}
         [:div player-1-name] [:div.text-right player-1-score]
         [:div player-2-name] [:div.text-right player-2-score]]
        [:div.flex.flex-grow]
        [:div.flex.w-1_2.justify-center.items-end
         [:button.p-4.border.border-gray-400.rounded.hover:bg-blue-100.dark:border-gray-800.dark:hover:bg-gray-800
          {:on-click #(rf/dispatch [::reset-game])}
          "Restart"]]]])))

(defn total-line []
  [:<>
   [:div#score1.flex-center.text-4xl
    {:class (when @(rf/subscribe [::added-score :player-1]) "animate-bounce-once")}
    @(rf/subscribe [::total-score :player-1])]
   [:div.flex-center.text-4xl.col-span-2.h-20 "Total"]
   [:div#score2.flex-center.text-4xl
    {:class (when @(rf/subscribe [::added-score :player-2]) "animate-bounce-once")}
    @(rf/subscribe [::total-score :player-2])]])

(defn grid-column [score]
  (let [player-1-score @(rf/subscribe [::player-score :player-1 score])
        player-2-score @(rf/subscribe [::player-score :player-2 score])]
    [:<>
     [:div.flex.w-full.justify-end
      [:div.flex-center.w-full.h-20.rounded.hover:bg-blue-100.cursor-pointer.border.border-gray-300.dark:hover:bg-gray-800.dark:border-gray-800
       {:on-click #(rf/dispatch [::add-player-score :player-1 score 1])}
       (when (>= 0 player-1-score) [:div.absolute.text-3xl.text-opacity-20.text-black.dark:text-gray-300.dark:text-opacity-20.select-none "+1"])
       (when (>= player-1-score 1) [:i.absolute.fa.fa-slash])
       (when (>= player-1-score 2) [:i.absolute.fa.fa-slash.fa-flip-horizontal])
       (when (>= player-1-score 3) [:i.absolute.far.fa-circle])]]
     [:div.flex-center.text-4xl.col-span-2
      {:class (case @(rf/subscribe [::number-open? score])
                :player-1
                "border-l-8 border-red-700"
                :player-2
                "border-r-8 border-blue-700"
                :closed
                "line-through"
                ;else
                "")}
      (if (= score 25) "Bullseye" score)]
     [:div.flex-center.w-full.h-20.rounded.hover:bg-blue-100.cursor-pointer.border.border-gray-300.dark:hover:bg-gray-800.dark:border-gray-800
      {:on-click #(rf/dispatch [::add-player-score :player-2 score 1])}
      (when (>= 0 player-2-score) [:div.absolute.text-3xl.text-opacity-20.text-black.dark:text-gray-300.dark:text-opacity-20.select-none "+1"])
      (when (>= player-2-score 1) [:i.absolute.fa.fa-slash])
      (when (>= player-2-score 2) [:i.absolute.fa.fa-slash.fa-flip-horizontal])
      (when (>= player-2-score 3) [:i.absolute.far.fa-circle])]]))

(defn cricket []
  [:div.flex.justify-center.w-full.overflow-auto
   [winner-screen]
   [:div.relative {:style {:max-width "60rem"}}
    [:div.text-center.font-semibold.text-4xl.p-4 "Cricket"]
    [:div.absolute.top-3.right-2.flex-center.undo-btn
     {:on-click #(rf/dispatch [::undo])}
     [:div.pr-2 "Undo"]
     [:i.fa-solid.fa-rotate-left.fa-sm]]
    [:div.grid.gap-4.text-2xl {:style {:grid-template-columns "1fr 1fr 1fr 1fr"}}
     [:input.col-span-2.bg-transparent.text-center.border-b-2.border-red-500
      {:type        "text"
       :placeholder "Player 1"
       :value       @(rf/subscribe [::player-1-name])
       :on-change   #(rf/dispatch [::set-player-1-name (-> % .-target .-value)])
       :on-blur     #(rf/dispatch [::cap-name :player-1])}]
     [:input.col-span-2.bg-transparent.text-center.border-b-2.border-blue-500
      {:type        "text"
       :placeholder "Player 2"
       :value       @(rf/subscribe [::player-2-name])
       :on-change   #(rf/dispatch [::set-player-2-name (-> % .-target .-value)])
       :on-blur     #(rf/dispatch [::cap-name :player-2])}]

     (doall
       (for [score NUMBERS]
         ^{:key score}
         [grid-column score]))
     [total-line]]]])