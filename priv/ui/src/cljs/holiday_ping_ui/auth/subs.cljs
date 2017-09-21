(ns holiday-ping-ui.auth.subs
  (:require
   [clojure.string :as string]
   [re-frame.core :as re-frame]
   [goog.string :as gstring]
   [goog.crypt :as crypt]
   [goog.crypt.Md5]
   [bouncer.core :as bouncer]
   [bouncer.validators :as validators]
   [holiday-ping-ui.common.subs :as subs]
   [holiday-ping-ui.auth.token :as token]))

(subs/db-subscription :access-token)

(re-frame/reg-sub
 :user-info
 (fn [_ _] (re-frame/subscribe [:access-token]))
 (fn [token _] (token/decode token)))

(defn- md5 [s]
  (let [bytes  (crypt/stringToUtf8ByteArray s)
        hasher (doto (goog.crypt.Md5.) (.update bytes))]
    (-> hasher .digest crypt/byteArrayToHex)))

(defn- gravatar
  [email]
  (->> email
       string/lower-case
       string/trim
       md5
       (gstring/format "https://www.gravatar.com/avatar/%s?d=identicon&s=64")))

(re-frame/reg-sub
 :avatar
 (fn [_ _] (re-frame/subscribe [:user-info]))
 (fn [{:keys [email avatar]} _]
   (if avatar
     avatar
     (gravatar email))))

(re-frame/reg-sub
 :valid-email?
 (fn [db [_ email]]
   (when email
     (if-not (bouncer/valid? {:email email} :email validators/email)
       [false "Email is invalid."]
       [true]))))

(re-frame/reg-sub
 :matching-passwords?
 (fn [db [_ password-repeat {:keys [password]}]]
   (when password-repeat
     (if-not (= password password-repeat)
       [false "Passwords must match."]
       [true]))))
