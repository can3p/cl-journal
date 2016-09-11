#|
This file is a part of cl-journal2 project.
|#

(in-package :cl-user)
(defpackage cl-journal2-asd
  (:use :cl :asdf))
(in-package :cl-journal2-asd)

(defsystem cl-journal
  :description "Common lisp livejournal blog client"
  :author "Dmitry Petrov <dpetroff@gmail.com>"
  :license "Public Domain"
  :depends-on (#:s-xml-rpc
               #:md5
               #:uiop
               #:cl-markdown)
  :components ((:module "src"
                :components
                (
                 (:file "markdown" :depends-on ("cl-journal"))
                 (:file "functions")
                 (:file "file-api")
                 (:file "client")
                 (:file "lj-api" :depends-on ("functions"))
                 (:file "cl-journal" :depends-on ("functions" "file-api" "lj-api"))
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
