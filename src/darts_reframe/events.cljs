(ns darts-reframe.events
  (:require
   [re-frame.core :as rf]
   [darts-reframe.db :as db]
   [day8.re-frame.tracing :refer-macros [fn-traced]]
   ))

(rf/reg-event-db
 ::initialize-db
 (fn-traced [_ _]
            db/default-db))
