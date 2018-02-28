(in-package :cl-user)
(defpackage cl-journal-test
  (:use :cl
   :cl-journal
        :prove))
(in-package :cl-journal-test)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-journal)' in your Lisp.

(plan nil)

(subtest "test1234"
  (is 1 1))

(finalize)
