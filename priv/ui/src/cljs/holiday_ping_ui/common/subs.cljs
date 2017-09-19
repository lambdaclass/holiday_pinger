(ns holiday-ping-ui.common.subs
  (:require
   [clojure.string :as string]
   [re-frame.core :as re-frame]))

(defn db-subscription
  "Define a subscription handler that just gets a top level value from the db."
  ([db-kw] (db-subscription db-kw db-kw))
  ([sub-kw db-kw]
   (re-frame/reg-sub
    sub-kw
    (fn [db] (get db db-kw)))))

(db-subscription :error-message)
(db-subscription :success-message)
(db-subscription :current-view)
(db-subscription :current-view-args)
(db-subscription :loading-view?)

(re-frame/reg-sub
 :valid-required?
 (fn [db [_ value]]
   (if (string/blank? value)
     [false "This field is required."]
     [true])))
