(ns holiday-ping-ui.common.events
  (:require [re-frame.core :as re-frame]
            [holiday-ping-ui.routes :as routes]))

;;; EFFECTS/COEFFECTS

;; TODO make these do parse/stringify json and transform to clj
(re-frame/reg-fx
 :set-local-store
 (fn [[key value]]
   (.setItem js/localStorage key value)))

(re-frame/reg-fx
 :remove-local-store
 (fn [key]
   (.removeItem js/localStorage key)))

(re-frame/reg-cofx
 :local-store
 (fn [coeffects key]
   (assoc coeffects :local-store (.getItem js/localStorage key))))

;; FIXME remove
(re-frame/reg-fx
 :set-location
 (fn [value]
   (.replaceState js/history nil "" value)))

(re-frame/reg-fx
 :set-history
 (fn [view & args]
   (apply routes/set-history! view args)))

(re-frame/reg-cofx
 :location
 (fn [coeffects]
   (assoc coeffects :location {:href     (aget js/location "href")
                               :query    (aget js/location "search")
                               :host     (aget js/location "host")
                               :hostname (aget js/location "hostname")
                               :origin   (aget js/location "origin")
                               :path     (aget js/location "pathname")})))

;;; GENERAL EVENTS
(re-frame/reg-event-fx
 :switch-view
 (fn [{:keys [db]} [_ new-view & args]]
   {:dispatch (apply vector :load-view new-view args)
    :db       (-> db
                  (assoc :current-view new-view)
                  (assoc :current-view-args args)
                  (dissoc :error-message)
                  (dissoc :success-message))}))

(re-frame/reg-event-fx
 :navigate
 (fn [_ [_ new-view & args]]
   {:dispatch    (apply vector :switch-view new-view args)
    :set-history new-view}))

(defmulti load-view
  "Define an event handler to load data necessary for each specific view.
  This way each section can load its data without having a big handler that
  knows about all of the."
  (fn [cofx [view]]
    view))

(defmethod load-view :default
  [& args]
  {})

(re-frame/reg-event-fx
 :load-view
 (fn [cofx [_ new-view & args]]
   (load-view cofx (apply vector new-view args))))

(re-frame/reg-event-db
 :error-message
 (fn [db [_ message]]
   (assoc db :error-message message)))

(re-frame/reg-event-db
 :success-message
 (fn [db [_ message]]
   (assoc db :success-message message)))
