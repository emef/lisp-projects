;;;; gre.lisp

(in-package #:gre)

(defun start-server (&key (port 8080))
  (start (make-instance 'acceptor :port port)))

(setq *dispatch-table*
      `(,(create-prefix-dispatcher "/test" 'test-page)
         ,(create-prefix-dispatcher "/about" 'about-page)
         ,(create-prefix-dispatcher "/test/3" (handler-w-args "\/test\/(\\d+)" 'try-it))))
         
(defun handler-w-args (reg-uri fn)
  `(lambda ()
      (apply fn (scan-to-strings reg-uri (request-uri *request*)))))
    
(defmacro uri-let ())

(defun try-it (str)
  str)
  

(defun test-page ()
  (let ((name (parameter "name")))
    (if name
        (format nil "Hi, <b>~a</b>" name)
        "Name: <form action='' method='get'><input type='text' name='name' /><input type='submit' name='submit' /></form>")))

(defun about-page ()
  "<h1>Hunchentoot Demo</h1>This is a very simple demonstration of the Hunchentoot webserver.")

