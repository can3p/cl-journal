(in-package :cl-user)
(defpackage lj-parse-response.cl-journal-test
  (:use :cl
   :cl-journal
   :prove))
(in-package :lj-parse-response.cl-journal-test)

(plan nil)

;; An example of the beast to defeat:
;; (:ITEMID 57 :SUBJECT (:BASE64 "0YLQtdC60YHRgiDQv9C+LdC/0L7RgNGD0YHRgdC60Lg=")
;;  :EVENT (:BASE64 "0YLQtdC60YHRgiDQv9C+LdC/0L7RgNGD0YHRgdC60Lg=") :DITEMID
;;  14739 :EVENTTIME "2018-05-06 14:13:00" :PROPS
;;  (:CURRENT_MOOD (:BASE64 "0YLQtdC60YHRgiDQv9C+LdC/0L7RgNGD0YHRgdC60Lg=")
;;   :CURRENT_LOCATION (:BASE64 "0YLQtdC60YHRgiDQv9C+LdC/0L7RgNGD0YHRgdC60Lg=")
;;   :TAGLIST (:BASE64 "0YLQtdC60YHRgiDQv9C+LdC/0L7RgNGD0YHRgdC60Lg=")
;;   :CURRENT_MUSIC (:BASE64 "0YLQtdC60YHRgiDQv9C+LdC/0L7RgNGD0YHRgdC60Lg=")
;;   :GIVE_FEATURES 1 :INTERFACE "web" :PERSONIFI_TAGS "nterms:yes"
;;   :IMAGES_PROCESSED 1525608835 :LANGS
;;   "{\"languages\":[[\"rus\",0.445426723331829],[\"ukr\",0.322265592709001],[\"bel\",0.23230768395917]],\"detector\":\"Lingua-YALI:0.015\",\"updated\":1525608873}")
;;  :CAN_COMMENT 1 :LOGTIME "2018-05-06 12:13:55" :ANUM 147 :URL
;;  "https://can3p-test.livejournal.com/14739.html" :EVENT_TIMESTAMP 1525615980
;;  :REPLY_COUNT 0))

(subtest "testing multiple file solution"
  (is 1 2))

(finalize)
