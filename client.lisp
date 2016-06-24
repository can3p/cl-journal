(in-package #:client)

(defun prompt-read (x)
  (format *query-io* "~a: " x)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-read-password (x)
  (format *query-io* "~a: " x)
  (force-output *query-io*)
  (let ((password (run-program "read -s val; echo $val" :output :string :input :interactive)))
    password
    ))

(defun get-login ()
  (let ((login (run-program "git config livejournal.login"
               :ignore-error-status t
               :output :string)))
    (string-trim `(#\Newline) login)))

(defun set-login ()
  (if (not (equal "" (get-login)))
      (progn
        (format t "Login is already set!~%")
        (get-login)
        )
      (progn
        (let ((login (prompt-read "Login name")))
          (run-program (format nil "git config livejournal.login \"~a\"" login)
                       :output :string)
          login
          ))))

(defun get-password (login)
  (let ((password (run-program (format nil "security find-internet-password -a ~a -s www.livejournal.com -w" login)
               :ignore-error-status t
               :output :string)))
    (string-trim `(#\Newline) password)))

(defun set-password (login)
  (if (not (equal "" (get-password login)))
      (format t "Password is already set!~%")
      (progn
        (let ((password (prompt-read-password "Password")))
          (run-program (format nil "security add-internet-password -a ~a -s www.livejournal.com -w ~a" login password)
                       :ignore-error-status t
                       :output :string)
          login
          ))))

(defun setup ()
  (let ((login (set-login)))
    (set-password login)
    ))
