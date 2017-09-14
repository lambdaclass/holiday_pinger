(ns holiday-ping-ui.auth.events
  (:require [clojure.string :as string]
            [re-frame.core :as re-frame]
            [ajax.core :as ajax]
            [day8.re-frame.http-fx]
            [goog.crypt.base64 :as base64]
            [bouncer.core :as bouncer]
            [bouncer.validators :as validators]
            [holiday-ping-ui.db :as db]
            [holiday-ping-ui.auth.token :as token]))

;;; AUTH EVENTS
(defn- github-callback?
  [{:keys [path]}]
  (string/includes? path "/oauth/github/callback"))

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
       (github-callback? (:location cofx)) ;; TODO when url routing is in place, stop doing it manually here
       {:db         db/default-db
        :dispatch-n [[:switch-view :github-loading]
                     [:github-code-submit]]}

       (and stored-token (token/expired? stored-token))
       {:db                 expired-db
        :remove-local-store "access_token"
        :dispatch           [:country-detect]}

       stored-token
       {:db         db/default-db
        :dispatch-n [[:country-detect]
                     [:auth-success {:access_token stored-token}]]}

       :else
       {:db       db/default-db
        :dispatch [:country-detect]}))))

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
                 :on-success      [:auth-success]
                 :on-failure      [:error-message "Authentication failed"]}}))

(re-frame/reg-event-fx
 :auth-success
 (fn
   [{:keys [db]} [_ response]]
   (let [token  (:access_token response)
         new-db (assoc db :access-token token)]
     {:db              new-db
      :set-local-store ["access_token" token]
      :dispatch-n      [[:channel-load]
                        [:switch-view :channel-list]]})))

(re-frame/reg-event-fx
 :logout
 (fn [{:keys [db]} _]
   {:db                 (-> db
                            (dissoc :access-token)
                            (assoc :current-view :login))
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

(re-frame/reg-event-fx
 :github-code-submit
 [(re-frame/inject-cofx :location)]
 (fn [{:keys [location]} _]
   (let [code (-> location :query (string/split #"=") last)]
     {:dispatch     [:country-detect]
      :set-location "/"
      :http-xhrio   {:method          :post
                     :uri             "/api/auth/github/code"
                     :timeout         8000
                     :format          (ajax/json-request-format)
                     :params          {:code code}
                     :response-format (ajax/json-response-format {:keywords? true})
                     :on-success      [:github-code-success]
                     :on-failure      [:error-message "GitHub authentication failed"]}})))

(re-frame/reg-event-fx
 :github-code-success
 (fn [{:keys [db]} [_ response]]
   (if (:new_user response)
     {:dispatch [:switch-view :github-register]
      :db       (assoc db :registration-token (:registration_token response))}
     {:dispatch [:auth-success response]})))

(re-frame/reg-event-fx
 :github-register-submit
 (fn [{:keys [db]} [_ form]]
   {:http-xhrio {:method          :post
                 :uri             "/api/auth/github/registration"
                 :timeout         8000
                 :format          (ajax/json-request-format)
                 :params          form
                 :response-format (ajax/json-response-format {:keywords? true})
                 :headers         {:authorization (str "Bearer " (:registration-token db))}
                 :on-success      [:auth-success]
                 :on-failure      [:error-message "GitHub registration failed"]}
    :db         (dissoc db :registration-token)}))

(re-frame/reg-event-fx
 :country-detect
 [(re-frame/inject-cofx :local-store "country")]
 (fn [{:keys [db local-store]} _]
   (if local-store
     {:db (assoc db :country local-store)}
     {:http-xhrio {:method          :get
                   :uri             "http://freegeoip.net/json/"
                   :timeout         8000
                   :response-format (ajax/json-response-format {:keywords? true})
                   :on-success      [:country-detect-success]}})))

(re-frame/reg-event-fx
 :country-detect-success
 (fn [{:keys [db]} [_ country-data]]
   (let [country (:country_name country-data)]
     {:db              (assoc db :country country)
      :set-local-store ["country" country]})))
