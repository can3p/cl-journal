(in-package :cl-user)
(defpackage lj-parse-response.cl-journal-test2
  (:use :cl
   :cl-journal
        :prove)
  (:import-from :cl-journal.file-api :parse-xml-response :post-file-list-to-string)
  )
(in-package :lj-parse-response.cl-journal-test2)

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


(subtest "parse-xml-response"

  (subtest "dead simple case"
    (let ((xml '(:ITEMID 56 :SUBJECT "test title" :EVENT
   "<p>I want to ahve it fetched back </p>" :DITEMID 14336 :EVENTTIME
   "2018-04-30 23:42:00" :PROPS
   (:PERSONIFI_TAGS "nterms:yes" :INTERFACE "xml-rpc" :IMAGES_PROCESSED
    1525124541 :GIVE_FEATURES 1)
   :CAN_COMMENT 1 :LOGTIME "2018-04-30 21:42:21" :ANUM 0 :URL
   "https://can3p-test.livejournal.com/14336.html" :EVENT_TIMESTAMP 1525131720
   :REPLY_COUNT 0)))

      (is (parse-xml-response xml)
          '(:post (
                   :itemid 56
                   :anum 0
                   :ditemid 14336
                   :url "https://can3p-test.livejournal.com/14336.html"
                   :log-ts "2018-04-30 21:42:21"
                   )
            :post-file (
                        :title "test title"
                        :body "<p>I want to ahve it fetched back </p>"
                        :body-raw "I want to ahve it fetched back"
                        )
            ))))
  )

  (subtest "complex case with russian text"
    (let* ((russian-text (list :BASE64
                               "0YLQtdC60YHRgiDQv9C+LdC/0L7RgNGD0YHRgdC60Lg="))
           (russian-text-decoded "текст по-порусски")
           (xml `(:ITEMID 57
                  :SUBJECT ,russian-text
                  :EVENT ,russian-text
                  :DITEMID 14739
                  :EVENTTIME "2018-05-06 14:13:00"
                  :PROPS
                  (:CURRENT_MOOD ,russian-text
                   :CURRENT_LOCATION ,russian-text
                   :TAGLIST ,russian-text
                   :CURRENT_MUSIC ,russian-text
                   :GIVE_FEATURES 1
                   :INTERFACE "web"
                   :PERSONIFI_TAGS "nterms:yes"
                   :IMAGES_PROCESSED 1525608835 :LANGS
                   "{\"languages\":[[\"rus\",0.445426723331829],[\"ukr\",0.322265592709001],[\"bel\",0.23230768395917]],\"detector\":\"Lingua-YALI:0.015\",\"updated\":1525608873}")
                  :CAN_COMMENT 1
                  :LOGTIME "2018-05-06 12:13:55"
                  :ANUM 147
                  :URL "https://can3p-test.livejournal.com/14739.html"
                  :EVENT_TIMESTAMP 1525615980
             :REPLY_COUNT 0)
            ))

      (is (parse-xml-response xml)
          `(:post (
                   :itemid 57
                   :anum 147
                   :ditemid 14739
                   :url "https://can3p-test.livejournal.com/14739.html"
                   :log-ts "2018-05-06 12:13:55"
                   )
            :post-file (
                        :title ,russian-text-decoded
                        :body ,russian-text-decoded
                        :body-raw ,russian-text-decoded
                        :fields (
                                 :music ,russian-text-decoded
                                 :mood ,russian-text-decoded
                                 :location ,russian-text-decoded
                                 :tags ,russian-text-decoded
                                 )
                        )
            )))

  (subtest "friends only"
    (let ((xml '(:ITEMID 56 :SUBJECT "test title" :EVENT
   "<p>I want to ahve it fetched back </p>" :DITEMID 14336 :EVENTTIME
   "2018-04-30 23:42:00" :PROPS
   (:PERSONIFI_TAGS "nterms:yes" :INTERFACE "xml-rpc" :IMAGES_PROCESSED
    1525124541 :GIVE_FEATURES 1)
                 :CAN_COMMENT 1 :LOGTIME "2018-04-30 21:42:21"
                 :SECURITY "usemask"
                 :ALLOWMASK 1
                 :ANUM 0 :URL
   "https://can3p-test.livejournal.com/14336.html" :EVENT_TIMESTAMP 1525131720
   :REPLY_COUNT 0)))

      (is (parse-xml-response xml)
          '(:post (
                   :itemid 56
                   :anum 0
                   :ditemid 14336
                   :url "https://can3p-test.livejournal.com/14336.html"
                   :log-ts "2018-04-30 21:42:21"
                   )
            :post-file (
                        :title "test title"
                        :body "<p>I want to ahve it fetched back </p>"
                        :body-raw "I want to ahve it fetched back"
                        :privacy "friends"
                        )
            ))))

  (subtest "private"
    (let ((xml '(:ITEMID 56 :SUBJECT "test title" :EVENT
   "<p>I want to ahve it fetched back </p>" :DITEMID 14336 :EVENTTIME
   "2018-04-30 23:42:00" :PROPS
   (:PERSONIFI_TAGS "nterms:yes" :INTERFACE "xml-rpc" :IMAGES_PROCESSED
    1525124541 :GIVE_FEATURES 1)
                 :CAN_COMMENT 1 :LOGTIME "2018-04-30 21:42:21"
                 :SECURITY "private"
                 :ANUM 0 :URL
   "https://can3p-test.livejournal.com/14336.html" :EVENT_TIMESTAMP 1525131720
   :REPLY_COUNT 0)))

      (is (parse-xml-response xml)
          '(:post (
                   :itemid 56
                   :anum 0
                   :ditemid 14336
                   :url "https://can3p-test.livejournal.com/14336.html"
                   :log-ts "2018-04-30 21:42:21"
                   )
            :post-file (
                        :title "test title"
                        :body "<p>I want to ahve it fetched back </p>"
                        :body-raw "I want to ahve it fetched back"
                        :privacy "private"
                        )
            ))))
  )

(subtest "post-file-list-to-string"

  (subtest "complex case with russian text"
    (let* ((russian-text (list :BASE64
                               "0YLQtdC60YHRgiDQv9C+LdC/0L7RgNGD0YHRgdC60Lg="))
           (russian-text-decoded "текст по-порусски")
           (xml `(:ITEMID 57
                  :SUBJECT ,russian-text
                  :EVENT ,russian-text
                  :DITEMID 14739
                  :EVENTTIME "2018-05-06 14:13:00"
                  :SECURITY "usemask"
                  :PROPS
                  (:CURRENT_MOOD ,russian-text
                   :CURRENT_LOCATION ,russian-text
                   :TAGLIST ,russian-text
                   :CURRENT_MUSIC ,russian-text
                   :GIVE_FEATURES 1
                   :INTERFACE "web"
                   :PERSONIFI_TAGS "nterms:yes"
                   :IMAGES_PROCESSED 1525608835 :LANGS
                   "{\"languages\":[[\"rus\",0.445426723331829],[\"ukr\",0.322265592709001],[\"bel\",0.23230768395917]],\"detector\":\"Lingua-YALI:0.015\",\"updated\":1525608873}")
                  :CAN_COMMENT 1
                  :LOGTIME "2018-05-06 12:13:55"
                  :ANUM 147
                  :URL "https://can3p-test.livejournal.com/14739.html"
                  :EVENT_TIMESTAMP 1525615980
             :REPLY_COUNT 0)
            ))

      (is (post-file-list-to-string
           (getf (parse-xml-response xml) :post-file))
          (format nil "title: ~a
privacy: friends
music: ~a
mood: ~a
location: ~a
tags: ~a

~a" russian-text-decoded
russian-text-decoded
russian-text-decoded
russian-text-decoded
russian-text-decoded
russian-text-decoded)))))

(finalize)
