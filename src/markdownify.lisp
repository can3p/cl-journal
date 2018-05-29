(in-package :cl-user)
(defpackage cl-journal.markdownify
  (:use :cl :plump)
  (:import-from :cl-strings :replace-all)
  (:export
   :markdownify))

(in-package :cl-journal.markdownify)

(defun tag-name-p (node name)
  (string= (string-upcase (tag-name node))
           (string-upcase name)))

(defmethod plump-dom:serialize-object :around ((node element))
    (cond
      ((tag-name-p node "P")
         (loop for child across (children node)
               do (serialize-object child))
         (format *stream* "~%~%"))

      ((tag-name-p node "ul")
         (loop for child across (children node)
               do (serialize-object child))
         (format *stream* "~%~%"))

      ((tag-name-p node "img")
       (format *stream* "![~a](~a)" (or (attribute node "alt") "no alt")
               (attribute node "src")))

      ((tag-name-p node "lj-embed")
       (cond
         ((string= (attribute node "source") "youtube")
          (format *stream* "<iframe width=\"560\" height=\"315\" src=\"https://www.youtube.com/embed/~a\" frameborder=\"0\" allow=\"autoplay; encrypted-media\" allowfullscreen></iframe>" (attribute node "vid")))

         ((string= (attribute node "source") "vimeo")
          (format *stream* "<iframe src=\"https://player.vimeo.com/video/~a\" width=\"640\" height=\"360\" frameborder=\"0\" webkitallowfullscreen mozallowfullscreen allowfullscreen></iframe>" (attribute node "vid")))
         (t (call-next-method))))

      ((tag-name-p node "a")
       (let ((str (string-trim '(#\Space #\Newline)
                               (with-output-to-string (out)
                                 (let ((*stream* out))
                                   (loop for child across (children node)
                                         do (serialize-object child)))))))
         (format *stream* "[~a](~a)" (if (> (length str) 0)
                                         str (attribute node "href"))
               (attribute node "href"))))

      ((tag-name-p node "blockquote")
       (let ((str (string-trim '(#\Space #\Newline)
                               (with-output-to-string (out)
                                 (let ((*stream* out))
                                   (loop for child across (children node)
                                         do (serialize-object child)))))))
                  (format *stream* "> ~a~%~%"
                          (replace-all str "
" "
> "))))

      ((tag-name-p node "li")
         (format *stream* "* ")
         (loop for child across (children node)
               do (serialize-object child))
         )

      ((tag-name-p node "i")
         (format *stream* "__")
         (loop for child across (children node)
               do (serialize-object child))
         (format *stream* "__"))

      ((tag-name-p node "strong")
         (format *stream* "**")
         (loop for child across (children node)
               do (serialize-object child))
         (format *stream* "**"))

      ((tag-name-p node "code")
         (format *stream* "`")
         (loop for child across (children node)
               do (serialize-object child))
         (format *stream* "`"))

      ((tag-name-p node "h1")
         (format *stream* "~%# ")
         (loop for child across (children node)
               do (serialize-object child))
         (format *stream* "~%"))

      ((tag-name-p node "h2")
         (format *stream* "~%## ")
         (loop for child across (children node)
               do (serialize-object child))
         (format *stream* "~%"))

      ((tag-name-p node "h3")
         (format *stream* "~%### ")
         (loop for child across (children node)
               do (serialize-object child))
         (format *stream* "~%"))

      ((tag-name-p node "h4")
         (format *stream* "~%#### ")
         (loop for child across (children node)
               do (serialize-object child))
         (format *stream* "~%"))
      (t (call-next-method))
    ))

(defun markdownify (html)
  (let ((parsed (parse html)))
    (string-trim '(#\Space #\Newline)
                 (serialize parsed nil))))
