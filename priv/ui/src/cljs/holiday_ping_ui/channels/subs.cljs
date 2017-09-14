(ns holiday-ping-ui.channels.subs
  (:require
   [holiday-ping-ui.common.subs :as subs]))

(subs/db-subscription :channels)
(subs/db-subscription :channel-to-test)
