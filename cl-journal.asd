#|
This file is a part of cl-journal project.
|#

(in-package :cl-user)
(defpackage cl-journal2-asd
  (:use :cl :asdf))
(in-package :cl-journal2-asd)

(defsystem cl-journal
  :description "Common lisp livejournal blog client"
  :author "Dmitry Petrov <dpetroff@gmail.com>"
  :version "0.1.6dev"
  :license "Public Domain"
  :homepage "https://github.com/can3p/cl-journal"
  :source-control "https://github.com/can3p/cl-journal"
  :depends-on (#:s-xml-rpc
               #:md5
               #:uiop
               #:alexandria
               #:cl-arrows
               #:cl-markdown)
  :components ((:module "src"
                :components
                (
                 (:file "markdown" :depends-on ("cl-journal"))
                 (:file "functions")
                 (:file "main" :depends-on ("settings" "lj-api" "cl-journal" "functions"))
                 (:file "db" :depends-on ("functions" "file-api"))
                 (:file "file-api")
                 (:file "settings")
                 (:file "lj-api" :depends-on ("functions" "db"))
                 (:file "cl-journal" :depends-on ("file-api" "lj-api"))
                 )))
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.md"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op cl-journal-test))))
