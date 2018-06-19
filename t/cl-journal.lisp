(in-package :cl-user)
(defpackage cl-journal-test
  (:use :cl
   :cl-journal
   :cl-journal.db
   :mockingbird
   :cl-arrows
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

(defun create-stub-event (item-id sync-ts)
  `(
     :event (:itemid ,item-id)
     :sync-ts ,sync-ts))

(defun create-stub-store (&rest event-list)
  (let ((store (make-instance 'cl-journal.db::<store>)))
    (setf (cl-journal.db::events store) event-list)
    store))


(defun test-get-unfetched-item-ids (store-contents items date)
  (let* ((store (if (null store-contents)
                    (create-stub-store store-contents)
                    (apply #'create-stub-store store-contents)))
         (result (multiple-value-list
                  (cl-journal.lj-api::get-unfetched-item-ids store))))
    (is (subseq result 0 2) (list items date))))

(defmacro with-mocked-calls (func data &rest body)
  "This function is necessary to emulate the behavior of
   a function that has side effects. Every subsequent
   call to the function will return next item from the
   data list, except the last one which will be returned
   endlessly"
  `(with-dynamic-stubs
       ((,func
         (lambda (&rest rest)
           (declare (ignore rest))
           (cond
             ,@(loop for resp in data
                    for i from 1 to (length data)
                    collect
                    (if (equal i (length data))
                        `(t (quote ,resp))
                        `((equal (call-times-for (quote ,func)) ,i)
                          (quote ,resp))))))))
     ,@body))

(subtest "merge-fetched-posts"

  (subtest "to-hash-table"

    (let ((ht (to-hash-table
               (create-stub-store
                (create-stub-event 1 "2018-01-02 11:22:33")
                (create-stub-event 2 "2016-01-02 11:22:33")))))
      (is (gethash 1 ht) "2018-01-02 11:22:33")
      (is (gethash 2 ht) "2016-01-02 11:22:33")
      (is (gethash 3 ht) nil)))
  )


(subtest "generate-unique-filename"

  (subtest "empty db"
    (let ((db (cl-journal.db::create-db-from-list
               '(
                 :version 2
                 :posts (
                 )))))
      (is (cl-journal.db::generate-unique-filename
           db "2018-01-01 11:22:33" "post tile")
          "2018-01-01-post-tile.md"))
    )

  (subtest "non matching post"
    (let ((db (cl-journal.db::create-db-from-list
               '(
                 :version 2
                 :posts (
                         (:ITEMID 56 :ANUM 0 :DITEMID 14336 :URL
                          "http://1234.html" :CREATED-AT 3734113341
                          :UPDATED-AT 3734113341 :IGNORED-AT NIL :SERVER-CHANGED-AT NIL :FILENAME
                          "2018-04-30-test-sync.md" :JOURNAL NIL)
                         )
                 ))))
      (is (cl-journal.db::generate-unique-filename
           db "2018-01-01 11:22:33" "post tile")
          "2018-01-01-post-tile.md"))
    )

  (subtest "several matching posts"
    (let ((db (cl-journal.db::create-db-from-list
               '(
                 :version 2
                 :posts (
                         (:ITEMID 56 :ANUM 0 :DITEMID 14336 :URL
                          "http://1234.html" :CREATED-AT 3734113341
                          :UPDATED-AT 3734113341 :IGNORED-AT NIL :SERVER-CHANGED-AT NIL :FILENAME
                          "2018-01-01-post-tile.md" :JOURNAL NIL)
                         (:ITEMID 57 :ANUM 0 :DITEMID 14336 :URL
                          "http://1234.html" :CREATED-AT 3734113341
                          :UPDATED-AT 3734113341 :IGNORED-AT NIL :SERVER-CHANGED-AT NIL :FILENAME
                          "2018-01-01-post-tile-2.md" :JOURNAL NIL)
                         )
                 ))))
      (is (cl-journal.db::generate-unique-filename
           db "2018-01-01 11:22:33" "post tile")
          "2018-01-01-post-tile-3.md"))
    )

  )

(subtest "get-unfetched-item-ids"

  (subtest "syncitems-post-p"
    (is (cl-journal.lj-api::syncitems-post-p
         '(:TIME "2018-02-27 20:45:36" :ACTION "update" :ITEM "L-50")) T)

    (is (cl-journal.lj-api::syncitems-post-p
         '(:TIME "2018-02-27 20:45:36" :ACTION "update" :ITEM "C-50")) NIL))

  (subtest "syncitems-same-post-p"
    (is (cl-journal.lj-api::syncitems-same-post-p
         '(:TIME "2018-02-27 20:45:36" :ACTION "update" :ITEM "L-50")
         '(:TIME "2017-02-27 20:45:36" :ACTION "create" :ITEM "L-50")
         ) T)

    (is (cl-journal.lj-api::syncitems-same-post-p
         '(:TIME "2018-02-27 20:45:36" :ACTION "update" :ITEM "L-50")
         '(:TIME "2018-02-27 20:45:36" :ACTION "update" :ITEM "L-51")
         ) NIL))

  (subtest "syncitems-newer-post-in-store-p"
    (is (cl-journal.lj-api::syncitems-newer-post-in-store-p
         (create-stub-store (create-stub-event 51 "2018-02-27 20:45:36"))
         '(:TIME "2018-02-27 20:45:36" :ACTION "update" :ITEM "L-50")
         ) NIL)

    (is (cl-journal.lj-api::syncitems-newer-post-in-store-p
         (create-stub-store (create-stub-event 50 "2018-02-27 18:45:36"))
         '(:TIME "2018-02-27 20:45:36" :ACTION "update" :ITEM "L-50")
         ) NIL)

    (is (cl-journal.lj-api::syncitems-newer-post-in-store-p
         (create-stub-store (create-stub-event 50 "2017-02-27 20:45:36"))
         '(:TIME "2018-02-27 20:45:36" :ACTION "update" :ITEM "L-50")
         ) NIL)

    (is (cl-journal.lj-api::syncitems-newer-post-in-store-p
         (create-stub-store (create-stub-event 50 "2018-02-27 20:45:37"))
         '(:TIME "2018-02-27 20:45:36" :ACTION "update" :ITEM "L-50")
         ) T)

    (is (cl-journal.lj-api::syncitems-newer-post-in-store-p
         (create-stub-store (create-stub-event 50 "2018-02-28 20:45:36"))
         '(:TIME "2018-02-27 20:45:36" :ACTION "update" :ITEM "L-50")
         ) T)

    (is (cl-journal.lj-api::syncitems-newer-post-in-store-p
         (create-stub-store
          (create-stub-event 50 "2018-02-26 20:45:36")
          (create-stub-event 50 "2018-02-28 20:45:36")
          )
         '(:TIME "2018-02-27 20:45:36" :ACTION "update" :ITEM "L-50")
         ) T)
    )

  (subtest "syncitems-item-data"
    (let
        ((local-time::*default-timezone* local-time::+utc-zone+))
      (multiple-value-bind (itemid time)
          (cl-journal.lj-api::syncitems-item-data
           '(:TIME "2018-02-27 20:45:36" :ACTION "update" :ITEM "L-50"))
        (is itemid 50)
        (is time "2018-02-27 20:45:36")
        )))

  (subtest "get-unfetched-item-ids"

    (subtest "trivial calls"

      (with-mocked-calls
        cl-journal.lj-api::lj-syncitems
        (
         (:COUNT 0 :TOTAL 0 :SYNCITEMS ())
         )
        (test-get-unfetched-item-ids nil nil nil))

      (with-mocked-calls
        cl-journal.lj-api::lj-syncitems
        (
         (:COUNT 1 :TOTAL 1
          :SYNCITEMS
          (
           (:TIME "2018-02-27 20:45:36" :ACTION "create" :ITEM "L-50")
           ))
         nil
         )
        (test-get-unfetched-item-ids nil (list 50)  "2018-02-27 20:45:36"))

      (with-mocked-calls
        cl-journal.lj-api::lj-syncitems
        (
         (:COUNT 1 :TOTAL 1
          :SYNCITEMS
          (
           (:TIME "2018-02-27 20:45:36" :ACTION "create" :ITEM "L-50")
           ))
         nil
         )
        (test-get-unfetched-item-ids
         (list
          (create-stub-event 50 "2018-02-28 20:45:36")
          )
         nil
         nil))

      (with-mocked-calls
        cl-journal.lj-api::lj-syncitems
        (
         (:COUNT 1 :TOTAL 1
          :SYNCITEMS
          (
           (:TIME "2018-02-27 20:45:36" :ACTION "create" :ITEM "L-50")
           ))
         nil
         )
        (test-get-unfetched-item-ids
         (list
          (create-stub-event 50 "2018-02-26 20:45:36")
          )
         (list 50) "2018-02-27 20:45:36"))

      (with-mocked-calls
        cl-journal.lj-api::lj-syncitems
        (
         (:COUNT 1 :TOTAL 1
          :SYNCITEMS
          (
           (:TIME "2018-02-25 20:45:36" :ACTION "create" :ITEM "L-50")
           (:TIME "2018-02-27 20:45:36" :ACTION "update" :ITEM "L-50")
           ))
         nil
         )
        (test-get-unfetched-item-ids
         (list
          (create-stub-event 50 "2018-02-26 20:45:36")
         )
         (list 50) "2018-02-27 20:45:36"))

      (with-mocked-calls
        cl-journal.lj-api::lj-syncitems
        (
         (:COUNT 1 :TOTAL 1
          :SYNCITEMS
          (
           (:TIME "2018-02-25 20:45:36" :ACTION "create" :ITEM "L-51")
           (:TIME "2018-02-27 20:45:36" :ACTION "update" :ITEM "L-50")
           ))
         nil
         )
        (test-get-unfetched-item-ids
         (list
          (create-stub-event 50 "2018-02-26 20:45:36")
         )
         (list 51 50) "2018-02-27 20:45:36"))

      (with-mocked-calls
        cl-journal.lj-api::lj-syncitems
        (
         (:COUNT 1 :TOTAL 1
          :SYNCITEMS
          (
           (:TIME "2018-02-27 20:45:36" :ACTION "create" :ITEM "L-50")
           ))
         nil
         )
        (test-get-unfetched-item-ids
         (list
          (create-stub-event 51 "2018-02-26 20:45:36")
         )
         (list 50)  "2018-02-27 20:45:36"))

      (with-mocked-calls
        cl-journal.lj-api::lj-syncitems
        (
         (:COUNT 0 :TOTAL 0)
         nil
         )
        (test-get-unfetched-item-ids
         nil
         nil nil))
      )
    )

  )

(finalize)
