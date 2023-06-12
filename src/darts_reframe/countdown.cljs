(ns darts-reframe.countdown
  (:require [clojure.string :as str]
            [re-frame.core :as rf]))

;; ---------- Helpers -----------

(defn get-next-player [current-player options winners]
  (let [player-num (:players options)
        teams (:teams options)
        out-players (into #{} (mapcat (fn [winning-team]
                                        (if (> player-num teams)
                                          [winning-team (+ winning-team teams)]
                                          [winning-team]))
                                      winners))
        upcoming-player-order (into []
                                    (concat
                                      (range (+ 1 current-player) (+ 1 player-num))
                                      (range 1 (+ 1 current-player))))]
    (print out-players)
    (if (<= player-num (count out-players))
      0
      (first (filter #(not (contains? out-players %)) upcoming-player-order)))))

;; ---------- Subscriptions ----------
(rf/reg-sub
  ::game-state
  (fn [db [_]]
    (get-in db [:game-state])))

(rf/reg-sub
  ::team-score
  (fn [db [_ num]]
    (get-in db [:game-state :score num])))

(rf/reg-sub
  ::player-name
  (fn [db [_ num]]
    (get-in db [:game-state :players num :name])))

(rf/reg-sub
  ::options
  (fn [db [_]]
    (get-in db [:options])))

(rf/reg-sub
  ::temp-options
  (fn [db [_]]
    (get-in db [:temp-options])))

(rf/reg-sub
  ::winners
  (fn [db [_]]
    (get-in db [:game-state :winners])))

;; ---------- Events ----------
(rf/reg-event-db
  ::undo
  (fn [db [_]]
    (let [to-state (first (get db :old-states))]
      (when to-state
        (-> db
            (assoc :old-states (rest (get db :old-states)))
            (assoc :game-state to-state))))))

(rf/reg-event-db
  ::set-player-name
  (fn [db [_ player val]]
    (assoc-in db [:game-state :players player :name] val)))

(rf/reg-event-db
  ::cap-name
  (fn [db [_ player]]
    (let [name (get-in db [:game-state :players player :name])]
      (-> db
          (assoc-in [:game-state :players player :name] (str/capitalize name))
          (update :old-states (fn [e] (take 20 (conj e (get db :game-state)))))))))

(rf/reg-event-db
  ::continue
  (fn [db [_]]
    (assoc-in db [:game-state :continue] true)))

(rf/reg-event-db
  ::set-temp-options
  (fn [db [_ path val]]
    (assoc-in db [:temp-options path] val)))

(rf/reg-event-db
  ::set-options
  (fn [db [_ players teams score]]
    (-> db
        (dissoc :temp-options)
        (assoc :options {:players players :teams teams :score score})
        (assoc-in [:game-state :current-player] 1)
        (assoc-in [:game-state :teams] (->> (range 1 (+ 1 teams))
                                            (map (fn [num] [num {:score score}]))
                                            (into {})))
        (assoc-in [:game-state :players] (->> (range 1 (+ 1 players))
                                              (map (fn [num] [num {:score 0
                                                                   :name  (get-in db [:game-state :players num :name])}]))
                                              (into {})))
        (assoc-in [:game-state :winners] [])
        (assoc-in [:game-state :continue] false))))

(rf/reg-event-db
  ::add-temp-score
  (fn [db [_ val]]
    (if (= val 0)
      (assoc-in db [:game-state :temp-score] 0)
      (update-in db [:game-state :temp-score] + val))))

(rf/reg-event-db
  ::add-team-score
  (fn [db [_ team score]]
    (let [current-score (get-in db [:game-state :teams team :score])
          options (get db :options)
          current-player (get-in db [:game-state :current-player])
          next-player (get-next-player current-player options (get-in db [:game-state :winners]))]
      (cond
        (= 0 (- current-score score))
        (-> db
            (assoc-in [:game-state :temp-score] 0)
            (update :old-states (fn [e] (take 20 (conj e (get db :game-state)))))
            (assoc-in [:game-state :teams team :score] (- current-score score))
            (assoc-in [:game-state :current-player] next-player)
            (assoc-in [:game-state :players current-player :score]
                      (+ score (get-in db [:game-state :players current-player :score])))
            (update-in [:game-state :winners] conj team))

        (< 0 (- current-score score))
        (-> db
            (assoc-in [:game-state :temp-score] 0)
            (update :old-states (fn [e] (take 20 (conj e (get db :game-state)))))
            (assoc-in [:game-state :teams team :score] (- current-score score))
            (assoc-in [:game-state :current-player] next-player)
            (assoc-in [:game-state :players current-player :score]
                      (+ score (get-in db [:game-state :players current-player :score]))))

        :else
        (-> db
            (assoc-in [:game-state :temp-score] 0)
            (update :old-states (fn [e] (take 20 (conj e (get db :game-state)))))
            (assoc-in [:game-state :current-player] next-player))))))

;; ---------- Views -----------
(defn medal [team-num]
  (let [winners @(rf/subscribe [::winners])
        place (.indexOf winners team-num)]
    (js/console.log place)
    [:div.flex-center.h-14
     (when (not= -1 place)
       [:i.fas.fa-medal.fa-2xl
        {:class (case place
                  0 "text-yellow-600 dark:text-yellow-500"
                  1 "text-gray-400 dark:text-gray-300"
                  2 "text-amber-900 dark:text-amber-800"
                  "")}])]))

(defn winner-screen []
  (let [winners @(rf/subscribe [::winners])
        first-place (first winners)
        game-state @(rf/subscribe [::game-state])
        continue? (:continue game-state)
        players (:players game-state)
        teams (:teams game-state)
        options @(rf/subscribe [::options])
        team-count (:teams options)
        doubles? (not= team-count (:players options))
        winner1 (or (get-in players [first-place :name]) (str "Player " first-place))
        winner2 (or (get-in players [(+ first-place team-count) :name]) (str "Player " (+ first-place team-count)))]
    (when (and (not-empty winners) (or (not continue?) (= (count winners) team-count)))
      [:div.absolute.flex.justify-center.items-center.bg-opacity-80.bg-gray-500.w-full
       {:style {:height  "95%"
                :z-index 100}}
       [:div.flex.flex-col.items-center.bg-gray-200.p-4.rounded-lg.text-4xl.dark:bg-gray-900
        [:div.font-semibold (if doubles? "The Winners Are:" "The Winner Is:")]
        [:div.font-bold
         (if doubles?
           (str winner1 " and " winner2)
           winner1)]
        [:div.pt-10.pb-5 "Amount Scored:"]
        [:div.grid.gap-x-8.pb-10 {:style {:grid-template-columns (str "repeat(" team-count ",1fr)")}}
         (doall
           (for [[num {:keys [score]}] teams]
             ^{:key num}
             [:div.grid.gap-x-8.gap-y-4 {:style {:grid-template-columns "1fr 1fr"}}
              [:div (or (get-in players [num :name]) (str "Player " num))]
              [:div.flex-center (get-in players [num :score])]
              (when doubles? [:<>
                              [:div (or (get-in players [(+ team-count num) :name]) (str "Player " (+ team-count num)))]
                              [:div.flex-center (get-in players [(+ team-count num) :score])]])]))]
        [:div.flex.flex-grow]
        [:div.flex.justify-evenly
         [:button.p-4.border.border-gray-400.rounded.hover:bg-blue-200.dark:border-gray-800.dark:hover:bg-gray-800
          {:on-click #(rf/dispatch [::set-options (:players options) (:teams options) (:score options)])}
          "Restart"]
         (when (not= (count winners) team-count)
           [:button.p-4.ml-20.border.border-gray-400.rounded.hover:bg-blue-200.dark:border-gray-800.dark:hover:bg-gray-800
            {:on-click #(rf/dispatch [::continue])}
            "Continue"])]]])))

(defn content []
  (let [options @(rf/subscribe [::options])
        team-count (:teams options)
        player-count (:players options)
        doubles? (not= team-count player-count)
        game-state @(rf/subscribe [::game-state])
        current-player (:current-player game-state)
        current-team (+ 1 (mod (- current-player 1) team-count))
        teams (:teams game-state)
        temp-score (:temp-score game-state)
        winners @(rf/subscribe [::winners])]
    [:div
     [winner-screen]
     [:div.flex.justify-center.w-full.overflow-auto
      [:div.relative {:style {:max-width "60rem"}}
       [:div.text-center.font-semibold.text-4xl.p-4 "301, 501, 1001"]
       [:div.absolute.top-3.right-2.flex-center.undo-btn
        {:on-click #(rf/dispatch [::undo])}
        [:div.pr-2 "Undo"]
        [:i.fa-solid.fa-rotate-left.fa-sm]]

       ;; Number Inputs
       [:div {:style {:max-width "60rem"}}
        (for [num (range 1 21)]
          ^{:key num}
          [:button.count-btn
           {:on-click #(rf/dispatch [::add-temp-score num])}
           num])
        [:div.flex-center
         [:button.count-btn {:on-click #(rf/dispatch [::add-temp-score 25])} 25]
         [:button.count-btn {:on-click #(rf/dispatch [::add-temp-score 50])} 50]]]

       ;; Temp Score
       [:div.p-10
        [:div.flex-center.text-3xl (or (:temp-score game-state) 0)]
        [:div.flex-center
         [:button.count-btn.w-40
          {:on-click #(rf/dispatch [::add-temp-score 0])}
          "Clear"]
         [:button.count-btn.w-40
          {:on-click #(rf/dispatch [::add-team-score current-team temp-score])}
          "End Turn"]]]

       ;; Player Columns
       [:div.grid.gap-8
        {:style {:grid-template-columns (str "repeat(" team-count ",1fr)")}}
        (doall
          (for [team (range 1 (+ 1 team-count))]
            ^{:key team}
            [:div
             [medal team]
             [:input.py-2.w-full.bg-transparent.text-3xl.text-center.rounded-lg
              {:class       (if (= current-player team) "border-4 border-blue-500" "border-b-2 border-gray-500")
               :type        "text"
               :placeholder (str "Player " team)
               :value       @(rf/subscribe [::player-name team])
               :on-change   #(rf/dispatch [::set-player-name team (-> % .-target .-value)])
               :on-blur     #(rf/dispatch [::cap-name team])}]
             (when doubles?
               [:input.py-2.w-full.bg-transparent.text-3xl.text-center.rounded-lg
                {:class       (if (= current-player (+ team-count team)) "border-4 border-blue-500" "border-b-2 border-gray-500")
                 :type        "text"
                 :placeholder (str "Player " (+ team-count team))
                 :value       @(rf/subscribe [::player-name (+ team-count team)])
                 :on-change   #(rf/dispatch [::set-player-name (+ team-count team) (-> % .-target .-value)])
                 :on-blur     #(rf/dispatch [::cap-name (+ team-count team)])}])
             [:div.p-6.text-3xl.flex-center (get-in teams [team :score])]]))]]]]))

(defn setup []
  (let [temp-options @(rf/subscribe [::temp-options])
        players (or (:players temp-options) 2)
        teams (or (:teams temp-options) 1)
        score (or (:score temp-options) 501)]
    [:div.flex.justify-center.w-full.overflow-auto
     [:div.relative {:style {:max-width "60rem"
                             :min-width "30rem"}}
      [:div.text-center.font-semibold.text-5xl.p-4 "301, 501, 1001"]

      [:div.flex-center.text-4xl.mt-5 "How Many Players?"]
      [:div.flex-center.w-full
       (for [num (range 1 5)]
         ^{:key num}
         [:button.count-btn
          {:class    (when (and (= num players) (= num teams)) "bg-blue-300 dark:bg-blue-900")
           :on-click (fn []
                       (rf/dispatch [::set-temp-options :players num])
                       (rf/dispatch [::set-temp-options :teams num]))}
          num])]
      [:div.flex-center.w-full
       [:button.count-btn
        {:class    (when (and (= 4 players) (= 2 teams)) "bg-blue-300 dark:bg-blue-900")
         :on-click (fn []
                     (rf/dispatch [::set-temp-options :players 4])
                     (rf/dispatch [::set-temp-options :teams 2]))}
        "2 v 2"]
       [:button.count-btn
        {:class    (when (= 6 players) "bg-blue-300 dark:bg-blue-900")
         :on-click (fn []
                     (rf/dispatch [::set-temp-options :players 6])
                     (rf/dispatch [::set-temp-options :teams 3]))}
        "2 v 2 v 2"]]
      [:div.flex-center.text-4xl.mt-5 "Total Score?"]
      [:div.flex-center.w-full
       [:button.count-btn
        {:class    (when (= 301 score) "bg-blue-300 dark:bg-blue-900")
         :on-click #(rf/dispatch [::set-temp-options :score 301])}
        301]
       [:button.count-btn
        {:class    (when (= 501 score) "bg-blue-300 dark:bg-blue-900")
         :on-click #(rf/dispatch [::set-temp-options :score 501])}
        501]
       [:button.count-btn
        {:class    (when (= 1001 score) "bg-blue-300 dark:bg-blue-900")
         :on-click #(rf/dispatch [::set-temp-options :score 1001])}
        1001]]
      [:div.flex-center.text-4xl.mt-5 "Ready?"]
      [:button.count-btn.w-full.my-2
       {:on-click #(rf/dispatch [::set-options players teams score])}
       "Go"]
      ]]))

(defn countdown []
  (let [options @(rf/subscribe [::options])]
    (cond
      options
      [content options]
      :else
      [setup])))
