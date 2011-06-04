;;;; package.lisp

(defpackage #:gre
  (:use #:cl 
        #:cl-ppcre
        #:json
        #:sqlite
        #:hunchentoot)
  (:export #:start-server
           #:get-word))

