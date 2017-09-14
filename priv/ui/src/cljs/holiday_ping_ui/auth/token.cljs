(ns holiday-ping-ui.auth.token
  (:require
   [clojure.string :as string]
   [goog.crypt.base64 :as base64]
   [goog.crypt :as crypt]))

(defn decode
  [token]
  (let [parse-json #(.parse js/JSON %)]
    (-> token
        (string/split #"\.")
        second
        base64/decodeStringToByteArray
        crypt/utf8ByteArrayToString
        parse-json
        (js->clj :keywordize-keys true))))

(defn expired?
  [token]
  (let [decoded    (decode token)
        current-ts (-> (js/Date.) .getTime (/ 1000))]
    (> current-ts (:exp decoded))))
