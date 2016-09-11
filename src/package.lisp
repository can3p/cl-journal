;;;; package.lisp

(defpackage #:cl-journal/functions
  (:use #:cl)
  (:export :get-date-struct))

(defpackage #:file-api
  (:use #:cl )
  (:import-from #:cl-markdown :markdown)
  (:export :parse-post-file :read-file))

(defpackage #:client
  (:use :cl :uiop/run-program)
  (:export :setup :get-login :get-password))

(defpackage #:lj-api
  (:use #:cl #:s-xml-rpc)
  (:import-from #:cl-journal/functions :get-date-struct)
  (:export :parse-post-answer :create-post :update-post :delete-post :*livejournal-login* :*livejournal-password*))

(defpackage #:cl-journal
  (:use #:cl #:lj-api #:file-api #:s-xml-rpc #:cl-markdown)
  (:import-from #:uiop/os :getcwd)
  (:import-from #:cl-journal/functions :get-date-struct)
  (:export #:*posts* :create-post-from-file :top-git-dir-p :publish-new-files :publish-modified-files :unpublish-deleted-files :restore-posts :lookup-file-url))

