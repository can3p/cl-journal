(in-package :rpc4cl)

;; monkey patch encode string to send properly encoded strings
(defun encode-string (string)
  (with-output-to-string (s)
    (s-xml:print-string-xml string s)))

(in-package :cl-user)
(defpackage cl-journal.lj-api
  (:use :cl :cl-arrows)
  (:import-from :cl-journal.functions :get-date-struct)
  (:import-from :alexandria :compose)
  (:import-from :cl-journal.settings
                :get-password)
  (:import-from :local-time
   :timestamp>
   :timestamp-
   :parse-timestring)
  (:import-from
   :cl-journal.db
   :to-xmlrpc-struct
   :*raw-text*
   :raw-text
   :<post-file>
   :<post>
   :<db>
   :<store>
   :events
   :fetch-store
   :merge-events
   :login
   :service-url
   :service-endpoint
   :read-from-file
   :last-post-ts
   :create-post-from-xmlrpc-struct
   :filename
   :journal
   :updated-at
   :server-changed-at
   :synced-from-fetch
   :log-ts
   :posts)
  (:export :publish-post
           :update-post
           :fetch-posts
           :delete-post
))

(in-package :cl-journal.lj-api)

(defvar *service-login* nil)
(defvar *service-password* nil)
(defvar *service-endpoint* nil)

(defun rpc-call (method &rest method-parameters)
  (apply #'rpc4cl:rpc-call *service-endpoint*
         nil nil method method-parameters))

(defun hash (str)
  (string-downcase (format nil "~{~2,'0x~}"
                           (coerce (md5:md5sum-string str)
                                   'list))))


(defun getchallenge ()
  (->
   (rpc-call "LJ.XMLRPC.getchallenge")
   (getf :challenge)))

;; source of wisdom - https://github.com/apparentlymart/livejournal/blob/master/cgi-bin/ljprotocol.pl
(defun lj-getevents (itemids &key (encoding :unicode))
  (let* ((c (-<> (list
                  ;; ver 1 is required for api to work
                  :ver (if (equal encoding :unicode) 1 0)
                  ;; this gives us lj-embeds with video id, that we can later
                  ;; convert to something meaningful
                  :get_video_ids 1
                  :itemids (format nil "~{~s~^,~}" itemids)
                  :lineendings "unix"
                  :selecttype "multiple")
                 (add-challenge))))
    (rpc-call "LJ.XMLRPC.getevents" c)))

(defun lj-getevents-multimode (itemids &key (speed 100000)
                                         (encoding :unicode)
                                         (final nil)
                                         (num-calls 1))
  "Livejournal encoding conversion is broken and we
   don't want to use it. What we want is to get posts
   in nonunicode post if necessary because they seem
   to be in unicode anyway (WTF?). This sub will try
   to guess encoding and adjust speed to get events
   in not so terrible amount of queries"
  (labels ((toggle-encoding (encoding)
             (if (equal encoding :unicode) :plain :unicode))
           (handle-failure ()
             (cond
               (final (error "Both modes failed for fetch, cannot proceed with download"))
               ((equal speed 1)
                (lj-getevents-multimode itemids
                                        :encoding (toggle-encoding encoding)
                                        :speed 1
                                        :final t
                                        :num-calls (1+ num-calls)))
               (t
                (lj-getevents-multimode itemids
                                        :encoding encoding
                                        :speed 1
                                        :num-calls (1+ num-calls)))
             ))
           (merge-results (a b)
             (let ((lastsync (getf b :lastsync)))
               (list :skip 0
                     :lastsync lastsync
                     :events (concatenate 'list
                                          (getf a :events)
                                          (getf b :events)))))
           )
    (handler-case
        (let* ((num-to-fetch (min speed (length itemids)))
               (results (lj-getevents (subseq itemids 0 num-to-fetch)
                                      :encoding encoding)))
          (if (<= (length itemids) num-to-fetch)
              (progn
                (format t "~a calls have been made to fetch items~%" num-calls)
                results)
              (merge-results results
                             (lj-getevents-multimode (subseq itemids num-to-fetch)
                                                     :encoding encoding
                                                     :speed (* speed 2)
                                                     :num-calls (1+ num-calls)))))
      (RPC4CL:XML-RPC-FAULT (f)
                        (declare (ignore f))
                        (handle-failure)))))

(defun older-p (ts1 ts2 threshold)
  (labels ((parse (ts)
             (let
                 ((local-time::*default-timezone* local-time::+utc-zone+))
               (parse-timestring ts :date-time-separator #\Space))))
    (timestamp>
         (timestamp- (parse ts2) threshold :sec)
         (parse ts1))))


(defun lj-get-server-ts ()
  ;; scary hack to get server ts in a single timezone
  (labels ((r () (getf (lj-getevents '(1000000)) :lastsync))) ;; something big enough to have empty lookup
    (loop with a = (r)
          do
             (let ((b (r)))
               (format t "~a~%" b)
               (when (older-p a b 10) (return a))
               (when (older-p b a 10) (return b))))))

(defun lj-syncitems (&optional (lastsync nil))
  (let* ((c (-<> (list :ver 1)
                 (add-challenge)
                 (maybe-add-last-sync <> lastsync))))
    (rpc-call "LJ.XMLRPC.syncitems" c)))

(defun maybe-add-last-sync (l &optional (lastsync nil))
  (if lastsync
      (concatenate 'list l
                   (list :lastsync lastsync))
      l))

(defun add-challenge (&optional (req nil))
  (let* ((challenge (getchallenge))
         (auth-response (hash (concatenate 'string
                                           challenge
                                           (hash *service-password*)))))
    (concatenate 'list
                 req
                 (list :username *service-login*
                       :auth_method "challenge"
                       :auth_challenge challenge
                       :auth_response auth-response))))

;; (def post-structure :itemid "number"
;;   :subject "maybe-encoded-string"
;;   :event "maybe-encoded-string"
;;   :allowmask 1
;;   :security "usemask"
;;   :anum "number"

(defgeneric create-new-post (post))

(defmethod create-new-post ((post <post-file>))
  (let* ((response (rpc-call "LJ.XMLRPC.postevent"
                             (to-xmlrpc-struct post #'add-challenge))))
    (create-post-from-xmlrpc-struct response
                                    (filename post)
                                    (journal post))))


(defgeneric delete-old-post (post))

(defmethod delete-old-post ((post <post>))
  (rpc-call "LJ.XMLRPC.editevent"
            (to-xmlrpc-struct post #'add-challenge t)))

(defgeneric update-old-post (post))

(defmethod update-old-post ((post <post>))
  (rpc-call "LJ.XMLRPC.editevent"
            (to-xmlrpc-struct post #'add-challenge)))

(defun set-credentials (db)
  (when (or (not (string= (login db) *service-login*))
            (not (string= (service-endpoint db) *service-endpoint*))
          (string= "" *service-password*))
    (setf *service-endpoint* (service-endpoint db))
    (setf *service-login* (login db))
    (setf *service-password* (get-password (login db) (service-url db)))))

(defgeneric publish-post (db post))

(defmethod publish-post ((db <db>) (post-file <post-file>))
  (set-credentials db)
  (let ((*raw-text* (raw-text db)))
    (let ((post (create-new-post post-file))
          (server-ts (lj-get-server-ts)))

      ;; technically this is not true, but this
      ;; timestamp can only be later then a honest
      ;; one which is enough for the usecase
      (setf (server-changed-at post) server-ts)
      (setf (log-ts post) server-ts)
      (push post (posts db)))))

(defgeneric delete-post (db post))

(defmethod delete-post ((db <db>) (post <post>))
  (set-credentials db)
  (delete-old-post post)
  (setf (posts db)
        (remove-if #'(lambda (p) (string= (filename p) (filename post)))
                   (posts db))))

(defgeneric update-post (db post))

(defmethod update-post ((db <db>) (post <post>))
  (set-credentials db)
  (let ((*raw-text* (raw-text db)))
    (update-old-post post))
  (setf (updated-at post) (get-universal-time))
  ;; we reset synced flag because once we updated
  ;; post from current version of markdown we want
  ;; to keep it untouched
  (setf (synced-from-fetch post) nil)

  ;; technically this is not true, but this
  ;; timestamp can only be later then a honest
  ;; one which is enough for the usecase
  (setf (server-changed-at post) (lj-get-server-ts))
  )

(defgeneric fetch-posts (db))

(defmethod fetch-posts ((db <db>))
  (set-credentials db)
  (fetch-posts (fetch-store db)))

;; sync algorithm:
;; 1. Call syncitems with timestamp of last synced item or nothing
;; 2. Goto 5 if no entries were returned
;; 3. Filter out items that have not yet been downloaded, or their
;;    server timestamp is greater than timestamp of last sync
;; 4. Goto 1 if we have less than ~90 entries to download
;; 5. Finish if no entries were collected
;; 6. Download collected items via getevents
;; 7. Add last sync time to every downloaded entry.
;; 8. Append downloaded entries to saved ones and save timestamp
;;    of the last one from syncitems call as last sync timestamp
;; 9. Goto 1

(defconstant +number-items-to-fetch+ 100)

(defun syncitems-post-p (item)
  (equal #\L (-<> item
                  (getf <> :item)
                  (char <> 0))))

(defun syncitems-same-post-p (item1 item2)
  (string= (getf item1 :item)
           (getf item2 :item)))

(defun acc (l &rest args)
  "Access member in a nested plist.

   Usage (acc l :one :two :three)"
  (cond
    ((null args) l)
    ((not (listp l)) nil)
    (t (apply #'acc (getf l (car args)) (cdr args)))))

(defun partial (func &rest args)
  "Partially apply function. Another function
   is returned that will call passed function
   with all arguments from first call + arguments
   from the second one"
  (lambda (&rest args2)
    (apply func (concatenate 'list args args2))))

(defun syncitems-newer-post-in-store-p (store item)
  (let ((itemid (-<> item
                     (getf <> :item)
                     (subseq <> 2)
                     (parse-integer))))
    (labels ((item-p (event)
               (equal itemid (acc event :event :itemid)))
             (parse (ts)
               (let
                   ((local-time::*default-timezone* local-time::+utc-zone+))
                 (parse-timestring ts :date-time-separator #\Space))))
      (let ((event (find-if #'item-p (events store) :from-end t)))
        (and event
            (timestamp>
             (parse (acc event :sync-ts))
             (parse (acc item :time)))
            )))))

(defun syncitems-item-data (l)
  "Extract timestamp and itemid from the sync item record"
  (values (parse-integer (subseq (getf l :item) 2))
          (getf l :time)))

(defun print-and-return (s)
  "Prints value and returns it right away. Useful to
   get insights about internal of complext expressions."
  (print s)
  s)


(defun get-unfetched-item-ids (store
                               &key (num-to-fetch +number-items-to-fetch+))
  "Given current state of store fetch next num-to-fetch
   items that need to be synced. Returns this number
   or less if that's all that is available as well as timestamp
   of last seen event, so that we can start from it on next iteration"
  (labels
      (
       (to-values (item)
         (multiple-value-bind (itemid time)
             (syncitems-item-data item)
           (list itemid time)))

       (to-hash-table (l)
         (let ((ht (make-hash-table :test 'equal)))
           (dolist (item l ht)
             (setf (gethash (car item) ht) (cadr item)))))

       (get-return-values (l)
         (let* ((parsed-list (mapcar #'to-values l))
                (last-item (car (reverse parsed-list))))
           (values
            (mapcar #'car parsed-list)
            (cadr last-item)
            (to-hash-table parsed-list))))

       (sync-more (l ts)
         (cond
           ((> (length l) num-to-fetch)
            (get-return-values (subseq l 0 num-to-fetch)))
           ((= (length l) num-to-fetch) (get-return-values l))
           (t
            (let* ((orig-list (-<> ts
                                   (lj-syncitems)
                                   (getf <> :syncitems)))
                   (orig-list-len (length orig-list))
                   ;; we need to fetch last from the original
                   ;; list since items at the end might be removed
                   (new-ts (-<> orig-list
                                (reverse)
                                (car)
                                (getf <> :time)))
                   (new-l (-<> orig-list
                               (remove-if (partial
                                           #'syncitems-newer-post-in-store-p
                                           store) <>)
                               (remove-if-not #'syncitems-post-p <>)
                               (concatenate 'list l <>)
                               (remove-duplicates <>
                                                  :test
                                                  #'syncitems-same-post-p))))
              (if (= 0 orig-list-len)
                  (get-return-values l)
                  (-<> new-l
                       (concatenate 'list l <>)
                       (sync-more <> new-ts))))))))
    (sync-more nil (last-post-ts store))))

(defun enrich-with-ts (event ht)
  (list :event event
        :sync-ts (gethash (getf event :itemid) ht)))

;; take last-post-ts from the store
;; call sync posts
;; filter out comments and posts that are already in db
;; and have sync date later than last-post-ts
;; repeat the process till nothing is left or we have a
;; decent amount of items
;; @TODO: actually implement the code
(defmethod fetch-posts ((store <store>))
  "Fetch all new items from remote service since last-fetched-ts
   of the store"
  (multiple-value-bind
   (new-itemids last-item-ts ht) (get-unfetched-item-ids store)
   (cond
     ((null new-itemids) store)
     (t (let ((new-events (-<> new-itemids
                             (lj-getevents-multimode)
                             (getf <> :events)
                             (mapcar #'(lambda (x) (enrich-with-ts x ht)) <>))))
          (merge-events store new-events last-item-ts)
          (fetch-posts store))))))
