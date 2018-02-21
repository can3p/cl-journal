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
  :version "0.5.2"
  :license "Public Domain"
  :homepage "https://github.com/can3p/cl-journal"
  :source-control "https://github.com/can3p/cl-journal"
  :depends-on (#:rpc4cl
               #:s-xml
               #:md5
               #:uiop
               #:alexandria
               #:cl-arrows
               #:cl-markdown
               #:split-sequence
               #+sbcl :sb-introspect
               )
  :components ((:module "src"
                :components
                (
                 (:file "magic-ed")
                 (:file "functions")
                 (:file "settings" :depends-on ("functions" "db"))
                 (:file "markdown" :depends-on ("cl-journal"))
                 (:file "main" :depends-on ("cl-journal" "functions"))
                 (:file "db" :depends-on ("functions" "file-api"))
                 (:file "file-api")
                 (:file "lj-api" :depends-on ("settings" "functions" "db"))
                 (:file "cl-journal" :depends-on ("settings" "file-api" "lj-api"))
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
