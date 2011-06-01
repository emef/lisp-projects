;;;; gre.asd

(asdf:defsystem #:gre
  :serial t
  :depends-on (#:hunchentoot :sqlite)
  :components ((:file "package")
               (:file "db")
               (:file "gre")))

