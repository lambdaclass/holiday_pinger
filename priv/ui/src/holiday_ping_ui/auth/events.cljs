(ns holiday-ping-ui.auth.events
  (:require [re-frame.core :as re-frame]
            [ajax.core :as ajax]
            [day8.re-frame.http-fx]
            [goog.crypt.base64 :as base64]
            [holiday-ping-ui.db :as db]
            [holiday-ping-ui.common.events :as events]
            [holiday-ping-ui.auth.token :as token]))

;;; AUTH EVENTS
(re-frame/reg-event-fx
 :initialize-db
 [(re-frame/inject-cofx :local-store "access_token")]
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
                 :on-failure      [:login-failure]}}))

(re-frame/reg-event-fx
 :login-failure
 (fn [_ [_ response]]
   ;; not the best to branch based on an error message, maybe use some sort of error code?
   (if (= (get-in response [:response :message]) "Email not verified")
     {:dispatch [:navigate :not-verified]}
     {:dispatch [:error-message "Authentication failed"]})))

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
 (fn [{:keys [db]} [_ data]]
   {:db         (assoc db :loading-view? true)
    :http-xhrio {:method          :post
                 :uri             "/api/users"
                 :timeout         8000
                 :format          (ajax/json-request-format)
                 :params          data
                 :response-format (ajax/text-response-format)
                 :on-success      [:navigate :email-sent]
                 :on-failure      [:error-message "Registration failed"]}}))

(defmethod events/load-view
  :github-callback [_ _]
  {:dispatch [:github-code-submit]})

(re-frame/reg-event-fx
 :github-code-submit
 [(re-frame/inject-cofx :location)]
 (fn [{:keys [location]} _]
   (let [code (get-in location [:query "code"])]
     {:http-xhrio {:method          :post
                   :uri             "/api/auth/github/code"
                   :timeout         8000
                   :format          (ajax/json-request-format)
                   :params          {:code code}
                   :response-format (ajax/json-response-format {:keywords? true})
                   :on-success      [:login-success]
                   :on-failure      [:error-message "GitHub authentication failed"]}})))

(defmethod events/load-view
  :register-confirm [_ _]
  {:dispatch [:register-code-submit]})

(re-frame/reg-event-fx
 :register-code-submit
 [(re-frame/inject-cofx :location)]
 (fn [{:keys [location]} _]
   (let [{code "code" email "email"} (:query location)]
     {:http-xhrio {:method          :post
                   :uri             "/api/users/confirmation/code"
                   :timeout         8000
                   :format          (ajax/json-request-format)
                   :params          {:email email :code code}
                   :response-format (ajax/json-response-format {:keywords? true})
                   :on-success      [:register-code-success]
                   :on-failure      [:navigate :register-confirm-error]}})))

(re-frame/reg-event-db
 :register-code-success
 (fn [db _]
   (assoc db :loading-view? false)))

(re-frame/reg-event-fx
 :resend-confirmation-submit
 (fn [{:keys [db]} [_ params]]
   {:db         (assoc db :loading-view? true)
    :http-xhrio {:method          :post
                 :uri             "/api/users/confirmation"
                 :timeout         8000
                 :format          (ajax/json-request-format)
                 :params          params
                 :response-format (ajax/json-response-format {:keywords? true})
                 :on-success      [:navigate :email-sent]
                 :on-failure      [:resend-confirmation-error]}}))

(re-frame/reg-event-db
 :resend-confirmation-error
 (fn [db _]
   (assoc db
          :error-message "Mail confirmation failed."
          :loading-view? false)))

(re-frame/reg-event-fx
 :password-reset-request
 (fn [{:keys [db]} [_ params]]
   {:db         (assoc db :loading-view? true)
    :http-xhrio {:method          :post
                 :uri             "/api/users/password"
                 :timeout         8000
                 :format          (ajax/json-request-format)
                 :params          params
                 :response-format (ajax/json-response-format {:keywords? true})
                 :on-success      [:navigate :password-reset-sent]
                 :on-failure      [:password-reset-request-error]}}))

(re-frame/reg-event-db
 :password-reset-request-error
 (fn [db _]
   (assoc db
          :error-message "Password reset failed."
          :loading-view? false)))

(re-frame/reg-event-fx
 :password-reset-submit
 [(re-frame/inject-cofx :location)]
 (fn [{:keys [location db]} [_ form]]
   (let [{code "code" email "email"} (:query location)
         params                      {:email    email
                                      :code     code
                                      :password (:password form)}]
     {:db         (assoc db :loading-view? true)
      :http-xhrio {:method          :post
                   :uri             "/api/users/password/code"
                   :timeout         8000
                   :format          (ajax/json-request-format)
                   :params          params
                   :response-format (ajax/json-response-format {:keywords? true})
                   :on-success      [:auth-submit params]
                   :on-failure      [:navigate :password-reset-error]}})))
