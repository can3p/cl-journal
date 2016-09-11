(in-package :cl-user)
(defpackage cl-journal.functions
  (:use :cl)
  (:export :get-date-struct))

(in-package :cl-journal.functions)

(defun get-date-struct (&optional ts)
  (multiple-value-bind
        (second minute hour date month year day-of-week dst-p tz)
      (decode-universal-time (or ts (get-universal-time)))
    (list :year year
          :mon month
          :day date
          :hour hour
          :min minute)))
