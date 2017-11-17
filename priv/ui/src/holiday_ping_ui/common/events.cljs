(ns holiday-ping-ui.common.events
  (:require [re-frame.core :as re-frame]
            [holiday-ping-ui.routes :as routes]
            [cemerick.url :as url]))

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

(re-frame/reg-fx
 :set-history
 (fn [view & args]
   (apply routes/set-history! view args)))

(re-frame/reg-cofx
 :location
 (fn [coeffects]
   (assoc coeffects :location (url/url (aget js/location "href")))))

;;; GENERAL EVENTS
(re-frame/reg-event-fx
 :switch-view
 (fn [{:keys [db]} [_ new-view & args]]
   (let [logged-in?  (:access-token db)
         auth-route? (routes/auth-route? new-view)]
     (cond
       (and auth-route? logged-in?)
       {:dispatch [:navigate :channel-list]}

       (and (not auth-route?) (not logged-in?))
       {:dispatch [:navigate :login]}

       :else
       {:dispatch (apply vector :load-view new-view args)
        :db       (-> db
                      (assoc :loading-view? true)
                      (assoc :current-view new-view)
                      (assoc :current-view-args args)
                      (dissoc :error-message)
                      (dissoc :success-message))}))))

(re-frame/reg-event-fx
 :navigate
 (fn [_ [_ new-view & args]]
   {:dispatch    (apply vector :switch-view new-view args)
    :set-history new-view}))

(defmulti load-view
  "Define an event handler to load data necessary for each specific view.
  This way each section can load its data without having a big handler that
  knows about all of the views. The handler needs to set :loading-view? to
  false when it finishes loading."
  (fn [cofx [view]]
    view))

(defmethod load-view :default
  [{:keys [db]} _]
  {:db (assoc db :loading-view? false)})

(re-frame/reg-event-fx
 :load-view
 (fn [cofx [_ new-view & args]]
   (load-view cofx (apply vector new-view args))))

(re-frame/reg-event-db
 :error-message
 (fn [db [_ message]]
   (-> db
       (assoc :loading-view? false)
       (assoc :error-message message))))

(re-frame/reg-event-db
 :success-message
 (fn [db [_ message]]
   (assoc db :success-message message)))
