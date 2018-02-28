#|
This file is a part of cl-journal project.
|#

(in-package :cl-user)
(defpackage cl-journal-test-asd
  (:use :cl :asdf))
(in-package :cl-journal-test-asd)

(defsystem cl-journal-test
  :author "Dmitry Petrov <dpetroff@gmail.com>"
  :license "Public Domain"
  :depends-on (:cl-journal
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "cl-journal"))))
  :description "Test system for cl-journal"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove-asdf) c)
                    (asdf:clear-system c)))
