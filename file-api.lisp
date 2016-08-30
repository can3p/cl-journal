(in-package #:file-api)

(defun make-keyword (name) (values (intern (string-upcase name) "KEYWORD")))

(defun read-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun parse-post-file (filename)
  (let ((l (read-file filename)))
    (process-by-line l ())))

(defun process-by-line (lines plist)
  (let ((cur-line (string-trim " " (car lines))))
    (if (find #\: cur-line)
        (let* ((p (position #\: cur-line))
               (key (make-keyword (string-trim " " (subseq cur-line 0 p))))
               (contents (string-trim " " (subseq cur-line (+ p 1))))
               )
          (setf (getf plist key) contents)
          (process-by-line (cdr lines) plist))
        (progn
          (setf (getf plist :body) (prepare-body (format nil "~{~A~^~%~}" lines)))
          plist
          ))))

(defun prepare-body (text)
  (with-output-to-string (out)
    (markdown (string-trim '(#\Space #\Newline) text) :stream out)))
