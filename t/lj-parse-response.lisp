(in-package :cl-user)
(defpackage lj-parse-response.cl-journal-test
  (:use :cl
   :cl-journal
   :prove))
(in-package :lj-parse-response.cl-journal-test)

(plan nil)

(subtest "testing multiple file solution"
  (is 1 2))

(finalize)
