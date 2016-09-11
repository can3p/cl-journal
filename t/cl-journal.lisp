(in-package :cl-user)
(defpackage cl-journal-test
  (:use :cl
   :cl-journal2
        :prove))
(in-package :cl-journal2-test)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-journal2)' in your Lisp.

(plan nil)

(finalize)
