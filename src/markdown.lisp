(in-package #:cl-markdown)

;; convert inline links like [inline-link](file.md) into real post urls
(defmethod render-span-to-html :before
    ((code (eql 'inline-link)) body encoding-method)
  (let ((record (cl-journal.db:get-by-fname cl-journal::*posts* (cadr body))))
    (if record
        (setf (cadr body) (cl-journal.db:url record)))))

;; convert reference links like [1]: file.md into real post urls
(defmethod url :around ((link link-info))
  (let* ((href (call-next-method))
         (record (cl-journal.db:get-by-fname cl-journal::*posts* href)))
    (if record
        (cl-journal.db:url record)
        href)))

(defextension (lj-user :arguments ((name :required)) :insertp t)
  (setf name (ensure-string name))
  (let ((safe-name (html-safe-name name)))
    (ecase phase
      (:parse)
      (:render
       (format nil "<lj user='~a'>" safe-name)))))

(push 'lj-user *render-active-functions*)
