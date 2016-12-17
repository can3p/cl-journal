;;;; magic-ed is an awesome library found in https://github.com/sanel/magic-ed
;;;; but unfortunately it does not exist on quick lisp. That's why I brought it
;;;; there
(defpackage :magic-ed
  (:use :cl #+sbcl :sb-alien)
  (:export :magic-ed
           #+(or sbcl ccl cmu scl) :ed-editor
           #+sbcl :system))

(in-package :magic-ed)

(defun slurp-file (path)
  "Slurp file from given path."
  (with-open-file (stream path)
    (let* ((len (file-length stream))
           (seq (make-string len)))
      (read-sequence seq stream)
      seq)))

;; SBCL specific thing, stolen from: http://random-state.net/log/3453226588.html

#+sbcl
(defun namestring-for-editor (editor thing)
  ;; a small function to check if we have editor which have 'editor +line <file>' support
  (defun known-editor (e)
    (member e '("vim" "emacs" "emacsclient") :test 'string=))

  (when thing
    (typecase thing
      (pathname (sb-ext:native-namestring (translate-logical-pathname thing) :as-file t))
      (string thing)
      (t
       (let* ((source   (sb-introspect:find-definition-source (fdefinition thing)))
              (pathname (sb-introspect:definition-source-pathname source))
              (offset   (or (sb-introspect:definition-source-character-offset source) 0)))
         (unless pathname
           (error "Don't know where the definition of ~S is, sorry." thing))
         (if (known-editor editor)
           (format nil "+~A ~A" offset (namestring-for-editor editor pathname))
           (format nil "~A" (namestring-for-editor editor pathname)) ))))))

#+sbcl
(unless sb-ext:*ed-functions*
  (defun ed-editor (thing)
    ;; simple FFI for system() call
    (define-alien-routine system int (command c-string))
    (let* ((editor (sb-ext:posix-getenv "EDITOR"))
           (editor (or editor "vi")))
      (system
        (format nil "~A~@[ ~A~]" editor (namestring-for-editor editor thing)))))
  ;; save it
  (push 'ed-editor sb-ext:*ed-functions*))

#+ccl
(unless ccl:*resident-editor-hook*
  (defun ed-editor (thing)
    (let* ((editor (ccl:getenv "EDITOR"))
           (editor (or editor "vi")))
      (ccl::os-command
        (format nil "~A ~A" editor thing))))

  ;; set it
  (setf ccl:*resident-editor-hook* 'ed-editor))


;; TODO: check if Hemlock was set
#+(or cmu scl)
(when t
  (defun ed-editor (&optional thing)
    (let* ((editor (cdr (assoc :EDITOR ext:*environment-list*)))
           (editor (or editor "vi"))
           ;; shamelessly stolen from ASDF
           (split-string
            (lambda (string &key (item #\space) (test #'char=))
              ;; Splits the string into substrings at spaces.
              (let ((len      (length string))
                    (index 0) result)
                (dotimes (i len
                            (progn (unless (= index len)
                                     (push (subseq string index) result))
                                   (reverse result)))
                  (when (funcall test (char string i) item)
                    (unless (= index i) ;; two spaces in a row
                      (push (subseq string index i) result))
                    (setf index (1+ i)))))))
           ;; CMUCL (run-program) is a bit sensitive and if running program has spaces
           ;; it will fail. We then must split the string, pick up the program name and
           ;; move everything else as arguments.
           (parts  (funcall split-string editor))
           (editor (first parts))
           (args   (append (rest parts) (when thing
                                          (list thing)))))
      (extensions:run-program editor args :input t :output t)))
  ;; overwrite (ed) with our definition
  (setf (symbol-function 'cl:ed) (symbol-function 'magic-ed:ed-editor)))

(defun magic-ed (&optional file &key (output :file) (eval t))
  "Call editor from REPL and depending on options, return to REPL or evaluate the file content in REPL.
This function will try to use implmentation specific (ed) function, which will in turn invoke editor set in
EDITOR environment variable. Some Common Lisp implementations uses different strategy for setting external
editor, so if unsure what they are, make sure to consult your implementation documentation first.

If called without file, this function will invoke editor without file argument.

Supported options are:

 :eval  (t or nil) - if :eval was set to nil, saved content will not be evaluated
 :output (:file or :string) - by default, content will be saved to file; if you want the content to
be returned as escaped string, set ':output :string'."

  ;; first assure we have valid parameters if we got them
  (unless (member output '(:file :string))
    (error "Received bad output type. For now only ':file' and ':string' are supported"))

  (let ((status (ed file)))
    (if (and file
             status
             (stringp file))
      (ecase output
        (:file
          ;; if user does not want to evaluate file
          (if eval (load file)))
        (:string
          (slurp-file file)))
      ;; invoked plain (ed), return it's status
      status)))
