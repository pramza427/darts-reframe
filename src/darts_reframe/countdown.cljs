(ns darts-reframe.countdown
  (:require [clojure.string :as str]
            [re-frame.core :as rf]))

;; ---------- Subscriptions ----------
(rf/reg-sub
  ::player-name
  (fn [db [_ player]]
    (get-in db [::state player :name])))

(rf/reg-sub
  ::player-score
  (fn [db [_ player score]]
    (get-in db [::state player :score score])))

(rf/reg-sub
  ::player-score
  (fn [db [_ player score]]
    (get-in db [::state player :score score])))

(rf/reg-sub
  ::setup?
  (fn [db [_]]
    (get db ::setup?)))

(rf/reg-sub
  ::total-score
  (fn [db [_ player]]
    (->> (get-in db [::state player :score])
         (map (fn [[score amount]]
                (if (< 3 amount)
                  (* score (- amount 3))
                  0)))
         (reduce +))))

(rf/reg-sub
  ::number-open?
  (fn [db [_ score]]
    (let [score-1 (get-in db [::state :player-1 :score score])
          score-2 (get-in db [::state :player-2 :score score])]
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
      (when to-state
        (-> db
            (assoc :old-states (rest (get db :old-states)))
            (assoc ::state to-state))))))

(rf/reg-event-db
  ::set-player-name
  (fn [db [_ player val]]
    (assoc-in db [::state player :name] val)))

(rf/reg-event-db
  ::cap-name
  (fn [db [_ player]]
    (let [name (get-in db [::state player :name])]
      (assoc-in db [::state player :name] (str/capitalize name)))))

(rf/reg-event-db
  ::add-player-score
  (fn [db [_ player score amount]]
    (let [score-1 (get-in db [::state :player-1 :score score])
          score-2 (get-in db [::state :player-2 :score score])
          current-amount (get-in db [::state player :score score])]
      (cond
        (and (<= 3 score-1) (<= 3 score-2)) db
        (and (= player :player-1) (<= 3 score-2)) (-> db
                                                      (update :old-states (fn [e] (take 20 (conj e (get db ::state)))))
                                                      (assoc-in [::state player :score score] (min 3 (+ current-amount amount))))
        (and (= player :player-2) (<= 3 score-1)) (-> db
                                                      (update :old-states (fn [e] (take 20 (conj e (get db ::state)))))
                                                      (assoc-in [::state player :score score] (min 3 (+ current-amount amount))))
        :else (-> db
                  (update :old-states (fn [e] (take 20 (conj e (get db ::state)))))
                  (assoc-in [::state player :score score] (+ current-amount amount)))))))

;; ---------- Helpers -----------


;; ---------- Views -----------
(defn content []
  (let [player-1-score @(rf/subscribe [::player-score :player-1 ])
        player-2-score @(rf/subscribe [::player-score :player-2 ])]
    ))

(defn start-up []
  [:div ])

(defn countdown []
  (let [setup? @(rf/subscribe [::setup?])]
    [:div.flex.justify-center.w-full.overflow-auto
     [:div.relative {:style {:max-width "60rem"}}
      [:div.text-center.font-semibold.text-4xl.p-4 "301, 501, 1001"]
      [:div.absolute.top-3.right-2.flex-center.border.border-black.rounded-full.px-2.hover:bg-blue-200
       [:button.pr-2 {:on-click #(rf/dispatch [::undo])} "Undo"]
       [:i.fa-solid.fa-rotate-left.fa-sm]]


      ]]))
