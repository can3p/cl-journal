(in-package :cl-user)
(defpackage cl-journal.file-api
  (:use :cl :cl-arrows)
  (:import-from :uiop/os :getcwd)
  (:import-from :cl-markdown :markdown)
  (:import-from :cl-journal.markdownify :markdownify)
  (:import-from :flexi-streams :octets-to-string)
  (:import-from :cl-base64 :base64-string-to-usb8-array)
  (:export
   :parse-post-file
   :read-file
   :read-parse-file
   :parse-xml-response
   :get-markdown-files))

(in-package :cl-journal.file-api)

(defun make-keyword (name) (values (intern (string-upcase name) "KEYWORD")))

;; (setf sb-impl::*default-external-format* :UTF-8)
(defun read-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun read-parse-file (file-path cb)
  (if (probe-file file-path)
      (with-open-file (in file-path)
        (with-standard-io-syntax
          (funcall cb (read in))))))

(defun parse-post-file (filename)
  (let ((l (read-file filename)))
    (process-by-line l ())))

(defun process-by-line (lines plist)
  (let ((cur-line (string-trim " " (car lines))))
    (if (find #\: cur-line)
        (let* ((p (position #\: cur-line))
               (key (make-keyword (string-trim " " (subseq cur-line 0 p))))
               (contents (string-trim " " (subseq cur-line (+ p 1))))
               )
          (setf (getf plist key) contents)
          (process-by-line (cdr lines) plist))
        (progn
          (setf (getf plist :body-raw) (format nil "~{~A~^~%~}" lines))
          (setf (getf plist :body) (prepare-body (format nil "~{~A~^~%~}" lines)))
          plist
          ))))

(defun prepare-body (text)
  (with-output-to-string (out)
    (markdown (string-trim '(#\Space #\Newline) text) :stream out)))

(defun get-markdown-files ()
  (mapcar #'(lambda (x) (enough-namestring x (getcwd)))
          (directory "./**/*.md")))


(defun b64getf (obj field)
  (let ((val (getf obj field)))
    (if (and (consp val) (equal (car val) :base64))
        (octets-to-string (base64-string-to-usb8-array
                           (cadr val))
                           :external-format :utf8)
        val)))

(defun parse-xml-response (xml)
  "Function takes output of the xml-rpc endpoint and turns it
   in a list that can become <post-file> and <post> objects.
   See test cases to get an understanding of cases covered"
  (labels ((append-fields (post-file props)
             (let* ((file-fields (list :music :mood :location :tags))
                    (lj-fields (list :current_music :current_mood :current_location :taglist))
                    (fields (mapcar #'(lambda (field lj-field)
                                        (if (b64getf props lj-field)
                                            (list field (b64getf props lj-field)) ()))
                                    file-fields lj-fields))
                    (fields-list (apply #'concatenate 'list fields)))

               (if (not (null fields-list))
                   (concatenate 'list
                                post-file
                                (list :fields fields-list))
                   post-file)))

           (add-privacy-field (post-file lj-security)
               (if (null lj-security) post-file
                   (cond
                     ((string= lj-security "private")
                      (concatenate 'list
                                   post-file
                                   (list :privacy "private")))
                     ;; note, that we do not import specific groups
                     ;; just say friends there. Why? Because I'm too
                     ;; lazy to implement it
                     ((string= lj-security "usemask")
                      (concatenate 'list
                                   post-file
                                   (list :privacy "friends")))
                     (t post-file))))
           )
    (list :post (list
                 :itemid (getf xml :itemid)
                 :anum (getf xml :anum)
                 :ditemid (getf xml :ditemid)
                 :url (getf xml :url)
                 )
          :post-file (add-privacy-field
                      (append-fields (list
                                      :title (b64getf xml :subject)
                                      :body (b64getf xml :event)
                                      :body-raw (markdownify (b64getf xml :event))
                                      )
                                     (getf xml :props))
                      (getf xml :security)))))

;;;; Merge fuctionality

;; What fact do we have available to make sync work?

;; 1) We can get update since some ts. Ts should be server time.
;;    We get notifications about new and updated posts.

;; 2) There is no nonhacky way of getting update ts from the
;;    post response by itself, however lj-getevents returns sync timestamp
;;    to us

;; Sounds like we have enough information to do a sync! We can split task
;; in three:

;; 1) Whenever we create/update post, we poke livejournal server to get
;;    a timestamp and store it along the post. [DONE]

;; 2) Whenever we fetch updated items, we store update timestamp of every
;;    one of them along with post response. [DONE]

;; Given these two last step is easy!

;; 3) Whenever we merge we take all raw posts that either do not exist as
;;    markdown or markdown has update ts ealier then raw server response
;;    and convert raw response to the markdown which is then save two the disk
;;    potentially replacing old file.

;; Sounds like a plan! The only two words that need clarification are `convert`
;; and `save` on a disk. Let's start with `save`.

;; How can we get the name? [DONE]

;; 1) If particular itemid is already in db use the name from there.

;; 2) If not, take date from event time as a base. If title exists,
;;    slagify it the best effort.

;; 3) Filename exists? Add -N to the end starting from 1 onwards till we
;; find an empty spot.

;; How do we convert?

;; 1) We walk over the structure. Atom is either a number or a string
;;    or a list with car :base64 and cdr containing some base64 encoded
;;    utf8 text.

;; 2) We save title as is, map fields to the supported flag.

;; 3) Flags can be marked as ignored, otherwise they cause a restart.

;; 4) Body text is markdownified, lj embeds are unwrapped.

;; That's it
