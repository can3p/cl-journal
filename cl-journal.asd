;;;; cl-journal.asd

(asdf:defsystem #:cl-journal
  :description "Common lisp livejournal blog client"
  :author "Dmitry Petrov <dpetroff@gmail.com>"
  :license "Public Domain"
  :serial t
  :depends-on (#:s-xml-rpc
               #:md5
               #:cl-markdown)
  :components ((:file "package")
               (:file "file-api")
               (:file "lj-api")
               (:file "cl-journal")
               ))

