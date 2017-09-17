(ns holiday-ping-ui.channels.subs
  (:require
   [re-frame.core :as re-frame]
   [holiday-ping-ui.common.subs :as subs]))

(subs/db-subscription :channels)
(subs/db-subscription :channel-to-test)
(subs/db-subscription :channel-to-edit)
