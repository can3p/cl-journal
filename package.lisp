;;;; package.lisp

(defpackage #:file-api
  (:use #:cl )
  (:import-from #:cl-markdown :markdown)
  (:export :parse-post-file :read-file))

(defpackage #:lj-api
  (:use #:cl #:s-xml-rpc)
  (:export :create-post))

(defpackage #:cl-journal
  (:use #:cl #:lj-api #:file-api)
  (:export :create-post-from-file))

