(ns holiday-ping-ui.auth.events
  (:require [clojure.string :as string]
            [re-frame.core :as re-frame]
            [ajax.core :as ajax]
            [day8.re-frame.http-fx]
            [goog.crypt.base64 :as base64]
            [bouncer.core :as bouncer]
            [bouncer.validators :as validators]
            [holiday-ping-ui.db :as db]
            [holiday-ping-ui.common.events :as events]
            [holiday-ping-ui.auth.token :as token]))

;;; AUTH EVENTS
(re-frame/reg-event-fx
 :initialize-db
 [(re-frame/inject-cofx :local-store "access_token")
  (re-frame/inject-cofx :location)]
 (fn [cofx _]
   (let [stored-token (:local-store cofx)
         expired-db   (-> db/default-db
                          (dissoc :access-token)
                          (assoc :error-message "Your session has expired, please log in again."))]
     (cond
       (and stored-token (token/expired? stored-token))
       {:db                 expired-db
        :remove-local-store "access_token"
        :dispatch           [:navigate :login]}

       stored-token
       {:db (assoc db/default-db :access-token stored-token)}

       :else
       {:db db/default-db}))))

(defn- basic-auth-header
  [user password]
  (->> (str user ":" password)
       (base64/encodeString)
       (str "Basic ")))

(re-frame/reg-event-fx
 :auth-submit
 (fn [_ [_ {:keys [email password]}]]
   {:http-xhrio {:method          :get
                 :uri             "/api/auth/token"
                 :timeout         8000
                 :headers         {:authorization (basic-auth-header email password)}
                 :response-format (ajax/json-response-format {:keywords? true})
                 :on-success      [:login-success]
                 :on-failure      [:error-message "Authentication failed"]}}))

(re-frame/reg-event-fx
 :login-success
 (fn [{:keys [db]} [_ response]]
   (let [token (:access_token response)]
     {:dispatch        [:navigate :channel-list]
      :db              (assoc db :access-token token)
      :set-local-store ["access_token" token]})))

(re-frame/reg-event-fx
 :logout
 (fn [{:keys [db]} _]
   {:db                 (dissoc db :access-token)
    :dispatch           [:navigate :login]
    :remove-local-store "access_token"}))

(re-frame/reg-event-fx
 :register-submit
 (fn [_ [_ {:keys [email password password-repeat] :as data}]]
   ;; TODO handle validations generically
   (cond
     (some string/blank? (vals data))
     {:dispatch [:error-message "All fields are required."]}

     (not= password password-repeat)
     {:dispatch [:error-message "Passwords must match."]}

     (not (bouncer/valid? data :email validators/email))
     {:dispatch [:error-message "Email is invalid."]}

     :else {:http-xhrio {:method          :post
                         :uri             "/api/users"
                         :timeout         8000
                         :format          (ajax/json-request-format)
                         :params          data
                         :response-format (ajax/text-response-format)
                         :on-success      [:register-success email password]
                         :on-failure      [:error-message "Registration failed"]}})))

(re-frame/reg-event-fx
 :register-success
 (fn
   [_ [_ email password _]]
   {:dispatch [:auth-submit {:email    email
                             :password password}]}))

(defmethod events/load-view
  :github-callback [_ _]
  {:dispatch [:github-code-submit]})

(re-frame/reg-event-fx
 :github-code-submit
 [(re-frame/inject-cofx :location)]
 (fn [{:keys [location]} _]
   (let [code (-> location :query (string/split #"=") last)]
     {:http-xhrio {:method          :post
                   :uri             "/api/auth/github/code"
                   :timeout         8000
                   :format          (ajax/json-request-format)
                   :params          {:code code}
                   :response-format (ajax/json-response-format {:keywords? true})
                   :on-success      [:login-success]
                   :on-failure      [:error-message "GitHub authentication failed"]}})))
