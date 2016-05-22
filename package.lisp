;;;; package.lisp

(defpackage #:lj-api
  (:use #:cl #:s-xml-rpc)
  (:export :create-post))

(defpackage #:cl-journal
  (:use #:cl #:lj-api))

