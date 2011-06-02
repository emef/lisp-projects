;;;; gre.asd

(asdf:defsystem #:gre
  :serial t
  :depends-on (#:hunchentoot #:sqlite #:cl-ppcre)
  :components ((:file "package")
               (:file "db")
               (:file "gre")))

