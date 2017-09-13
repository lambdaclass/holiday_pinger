(ns holiday-ping-ui.events
  (:require [clojure.string :as string]
            [re-frame.core :as re-frame]
            [ajax.core :as ajax]
            [day8.re-frame.http-fx]
            [goog.crypt.base64 :as base64]
            [cljs-time.core :as time]
            [holiday-ping-ui.db :as db]
            [holiday-ping-ui.helpers.time-format :as format]
            [holiday-ping-ui.helpers.token :as token]
            [bouncer.core :as bouncer]
            [bouncer.validators :as validators]))

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
 :set-location
 (fn [value]
   (.replaceState js/history nil "" value)))

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

(re-frame/reg-event-fx
 :switch-view
 (fn [{:keys [db]} [_ new-view & args]]
   {:dispatch (apply vector :load-view new-view args)
    :db       (-> db
                  (assoc :current-view new-view)
                  (assoc :current-view-args args)
                  (dissoc :error-message)
                  (dissoc :success-message))}))

(defmulti load-view
  "Define an event handler to load data necessary for each specific view."
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

;;; AUTH EVENTS
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

;;; CHANNEL EVENTS
(re-frame/reg-event-fx
 :channel-load
 (fn [{:keys [db]} _]
   {:http-xhrio {:method          :get
                 :uri             "/api/channels"
                 :timeout         8000
                 :headers         {:authorization (str "Bearer " (:access-token db))}
                 :response-format (ajax/json-response-format {:keywords? true})
                 :on-success      [:channel-load-success]
                 :on-failure      [:error-message "Channel loading failed."]}}))

(re-frame/reg-event-db
 :channel-load-success
 (fn [db [_ response]]
   (assoc db :channels response)))

(re-frame/reg-event-fx
 :channel-delete
 (fn [{db :db} [_ channel]]
   {:http-xhrio {:method          :delete
                 :uri             (str "/api/channels/" channel)
                 :timeout         8000
                 :headers         {:authorization (str "Bearer " (:access-token db))}
                 :format          (ajax/json-request-format)
                 :response-format (ajax/json-response-format {:keywords? true})
                 :on-success      [:channel-delete-success channel]
                 :on-failure      [:error-message "Channel deleting failed."]}}))

(re-frame/reg-event-db
 :channel-delete-success
 (fn [db [_ channel]]
   (update db :channels
           (fn [channels]
             (remove #(= (:name %) channel) channels)))))

(defn valid-slack-target?
  [value]
  (or (string/starts-with? value "#")
      (string/starts-with? value "@")))

(re-frame/reg-event-fx
 :channel-submit
 (fn [{db :db} [_ {:keys [name type url channels username emoji days-before
                          same-day]}]]
   (let [channels    (string/split channels #"\s+")
         days-before (js/parseInt days-before)
         params      {:name          name
                      :type          type
                      :same_day      same-day
                      :days_before   (if (zero? days-before) nil days-before)
                      :configuration {:channels channels
                                      :url      url
                                      :username username
                                      :emoji    emoji}}]
     (cond
       (some string/blank? [name type url])
       {:dispatch [:error-message "Please fill required fields."]}

       (not (every? valid-slack-target? channels))
       {:dispatch [:error-message "Slack targets must start with @ or #"]}

       (not (string/starts-with? url "https://hooks.slack.com/"))
       {:dispatch [:error-message "The url should be a valid slack hook url."]}

       :else {:http-xhrio {:method          :put
                           :uri             (str "/api/channels/" name)
                           :headers         {:authorization (str "Bearer " (:access-token db))}
                           :timeout         8000
                           :format          (ajax/json-request-format)
                           :params          params
                           :response-format (ajax/text-response-format)
                           :on-success      [:channel-submit-success]
                           :on-failure      [:error-message "Channel submission failed"]}}))))

(re-frame/reg-event-fx
 :channel-submit-success
 (fn [_ _]
   {:dispatch-n [[:channel-load]
                 [:switch-view :channel-list]]}))

(re-frame/reg-event-db
 :channel-test-start
 (fn [db [_ channel]]
   (assoc db :channel-to-test channel)))

(re-frame/reg-event-db
 :channel-test-cancel
 (fn [db _]
   (dissoc db :channel-to-test)))

(re-frame/reg-event-fx
 :channel-test-confirm
 (fn [{:keys [db]} [_ channel]]
   {:dispatch   [:channel-test-cancel]
    :http-xhrio {:method          :post
                 :uri             (str "/api/channels/" (:name channel) "/test")
                 :timeout         8000
                 :headers         {:authorization (str "Bearer " (:access-token db))}
                 :format          (ajax/json-request-format)
                 :params          {}
                 :response-format (ajax/text-response-format)
                 :on-success      [:success-message "Channel reminder sent."]
                 :on-failure      [:error-message "There was an error sending the reminder."]}}))

;;; HOLIDAY EVENTS
(defmethod load-view
  :holidays
  [{:keys [db]} [_ channel-name]]
  {:http-xhrio {:method          :get
                :uri             (str "/api/channels/" channel-name "/holidays")
                :timeout         8000
                :headers         {:authorization (str "Bearer " (:access-token db))}
                :response-format (ajax/json-response-format {:keywords? true})
                :on-success      [:holidays-load-success]
                :on-failure      [:error-message "Holidays loading failed."]}})

(re-frame/reg-event-db
 :holidays-load-success
 (fn [db [_ response]]
   (let [sorted   (sort-by :date response)
         holidays (map #(update % :date format/string-to-date) sorted)]
     (-> db
         (assoc :holidays-saved holidays)
         (assoc :holidays-edited holidays)))))

(re-frame/reg-event-fx
 :holidays-save
 (fn [{:keys [db]} [_ channel-name]]
   (let [edited       (:holidays-edited db)
         new-holidays (map #(update % :date format/date-to-string) edited)]
     {:http-xhrio {:method          :put
                   :uri             (str "/api/channels/" channel-name "/holidays")
                   :timeout         8000
                   :headers         {:authorization (str "Bearer " (:access-token db))}
                   :response-format (ajax/json-response-format {:keywords? true})
                   :format          (ajax/json-request-format)
                   :params          new-holidays
                   :on-success      [:holidays-load-success]
                   :on-failure      [:error-message "Holidays saving failed."]}})))

(re-frame/reg-event-db
 :holidays-reset
 (fn [db _]
   (assoc db :holidays-edited (:holidays-saved db))))

(re-frame/reg-event-db
 :holidays-clear
 (fn [db _]
   (let [past?         #(time/before? (:date %) (time/today))
         past-holidays (take-while past? (:holidays-saved db))]
     (assoc db :holidays-edited past-holidays))))

(re-frame/reg-event-db
 :calendar-select-year
 (fn [db [_ year]]
   (assoc db :calendar-selected-year year)))

(re-frame/reg-event-db
 :calendar-select-day
 (fn [db [_ day]]
   (let [holidays (:holidays-edited db)
         name     (:name (first (filter #(time/= day (:date %)) holidays)))]
     (-> db
         (assoc :calendar-selected-day day)
         (assoc :calendar-selected-day-name name)))))

(re-frame/reg-event-db
 :calendar-deselect-day
 (fn [db]
   (-> db
       (dissoc :calendar-selected-day)
       (dissoc :calendar-selected-day-name))))

(re-frame/reg-event-db
 :calendar-selected-name-change
 (fn [db [_ name]]
   (assoc db :calendar-selected-day-name name)))

(re-frame/reg-event-db
 :holidays-update
 (fn [db [_ date name]]
   (let [name (or name "")]
     (->> (:holidays-edited db)
          (remove #(time/= date (:date %)))
          (cons {:date date :name name})
          (sort-by :date time/before?)
          (assoc db :holidays-edited)))))

(re-frame/reg-event-db
 :holidays-remove
 (fn [db [_ date]]
   (let [edited  (:holidays-edited db)
         removed (remove #(time/= date (:date %)) edited)]
     (assoc db :holidays-edited removed))))
