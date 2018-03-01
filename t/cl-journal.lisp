(in-package :cl-user)
(defpackage cl-journal-test
  (:use :cl
   :cl-journal
        :prove))
(in-package :cl-journal-test)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-journal)' in your Lisp.

(plan nil)

;; example lj-synitems output
;; (:COUNT 22 :TOTAL 22 :SYNCITEMS
;;         ((:TIME "2016-05-22 20:11:26" :ACTION "create" :ITEM "L-1")
;;          (:TIME "2016-06-05 12:03:47" :ACTION "create" :ITEM "L-3")
;;          (:TIME "2016-06-19 21:59:35" :ACTION "create" :ITEM "L-4")
;;          (:TIME "2016-06-19 22:28:10" :ACTION "create" :ITEM "L-5")
;;          (:TIME "2016-06-20 22:20:42" :ACTION "create" :ITEM "L-6")
;;          (:TIME "2016-06-23 23:16:27" :ACTION "create" :ITEM "L-7")
;;          (:TIME "2016-06-24 07:23:43" :ACTION "create" :ITEM "L-8")
;;          (:TIME "2016-06-24 07:28:49" :ACTION "create" :ITEM "L-9")
;;          (:TIME "2016-06-24 07:30:25" :ACTION "create" :ITEM "L-10")
;;          (:TIME "2016-06-24 07:30:38" :ACTION "create" :ITEM "L-11")
;;          (:TIME "2016-06-25 07:30:54" :ACTION "create" :ITEM "L-12")
;;          (:TIME "2016-08-15 21:14:08" :ACTION "update" :ITEM "L-17")
;;          (:TIME "2016-10-17 21:31:43" :ACTION "update" :ITEM "L-15")
;;          (:TIME "2016-12-26 00:52:51" :ACTION "update" :ITEM "L-16")
;;          (:TIME "2017-04-19 21:59:49" :ACTION "update" :ITEM "L-46")
;;          (:TIME "2017-06-09 22:48:52" :ACTION "create" :ITEM "L-48")
;;          (:TIME "2018-02-21 23:44:12" :ACTION "create" :ITEM "L-51")
;;          (:TIME "2018-02-21 23:48:00" :ACTION "update" :ITEM "L-52")
;;          (:TIME "2018-02-22 00:07:09" :ACTION "create" :ITEM "L-53")
;;          (:TIME "2018-02-27 20:44:21" :ACTION "update" :ITEM "C-50")
;;          (:TIME "2018-02-27 20:45:36" :ACTION "update" :ITEM "L-50")
;;          (:TIME "2018-02-27 21:13:28" :ACTION "create" :ITEM "L-54")))

;; example lj-getevents output
;; (:SKIP 0 :EVENTS
;;        ((:ITEMID 1
;;          :SUBJECT (:BASE64 "0JTQvtCx0YDQviDQv9C+0LbQsNC70L7QstCw0YLRjCDQsiBMaXZlSm91cm5hbA==")
;;          :EVENT "<lj-replace name=\"first_post\">"
;;          :DITEMID 472
;;          :EVENTTIME "2016-05-21 00:00:00"
;;          :PROPS (:TAGLIST "livejournal, welcome"
;;                  :PERSONIFI_TAGS "nterms:yes"
;;                  :INTERFACE "flat"
;;                  :USERAGENT "lj_system_auto"
;;                  :LANGS "{\"languages\":[[\"rus\",0.373156936938249],[\"ukr\",0.362593474898788],[\"bel\",0.225839552480495]],\"detector\":\"Lingua-YALI:0.015\",\"updated\":1469029745}"
;;                  :GIVE_FEATURES 1)
;;          :CAN_COMMENT 1
;;          :LOGTIME "2016-05-22 20:11:26"
;;          :SECURITY "private"
;;          :ANUM 216
;;          :URL "https://can3p-test.livejournal.com/472.html"
;;          :EVENT_TIMESTAMP 1463788800
;;          :REPLY_COUNT 0)
;;         (:ITEMID 3
;;          :SUBJECT "second title"
;;          :EVENT "second post"
;;          :DITEMID 966
;;          :EVENTTIME "2016-06-05 14:03:00"
;;          :PROPS (:PERSONIFI_TAGS "nterms:yes"
;;                  :INTERFACE "xml-rpc"
;;                  :LANGS "{\"languages\":[[\"ita\",0.35541885545503],[\"eng\",0.311103738585191],[\"fra\",0.125067805176593]],\"detector\":\"Lingua-YALI:0.015\",\"updated\":1465128252}"
;;                  :GIVE_FEATURES 1)
;;          :CAN_COMMENT 1
;;          :LOGTIME "2016-06-05 12:03:47"
;;          :ANUM 198
;;          :URL "https://can3p-test.livejournal.com/966.html"
;;          :EVENT_TIMESTAMP 1465135380
;;          :REPLY_COUNT 0))
;;  :LASTSYNC "2018-02-27 21:26:21")

(subtest "test1234"
  (is 1 1))

(finalize)
