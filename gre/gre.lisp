;;;; gre.lisp

(in-package #:gre)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defparameter *static-dir* #P"/Users/mattforbes/src/lisp/gre/static/")

(setq *message-log-pathname* #P"messages.log")
(setq *access-log-pathname* #P"access.log")


(defun start-server (&key (port 8080))
  (setf *dispatch-table* nil)
  (bind-urls
   (("\/mod_word" mod-word-hdl)
    ("\/del_word" del-word-hdl)
    ("\/get_words" get-words-hdl)
    ("\/" main-hdl)))
  (push (create-folder-dispatcher-and-handler "/static/" *static-dir*) *dispatch-table*)
  (start (make-instance 'acceptor :port port)))

;;
;; handlers
;;


;; only page to be served
(defun main-hdl ()
  (handle-static-file "main.html"))
  
;; API handlers (POST)
(defun mod-word-hdl ()
  (unwind-protect
       (progn
         (let ((word (parameter "word"))
               (definition (parameter "definition"))
               (sentence (parameter "sentence"))
               (hint (parameter "hint")))
           (cond ((and word definition)
                  (let ((existing-word (get-word word)))
                    (if existing-word
                        (update-word word 
                                     :definition definition
                                     :sentence sentence
                                     :hint hint)
                        (insert-word word
                                     definition
                                     :sentence sentence
                                     :hint hint)))
                  (json-response (word-json (get-word word))))
                 (t (null-response)))))
    (null-response)))

(defun del-word-hdl ()
  (unwind-protect
       (progn
         (let ((word (parameter "word")))
           (and word
                (delete-word word))
           (json-response "true")))
    (json-response "false")))

(defun get-words-hdl ()
  (unwind-protect 
       (json-response (word-list-json (get-words)))
    (null-response)))

;;
;; response utilities
;;

(defun simple-response (mime-type str)
  (setf content-type* mime-type)
  str)

(defun json-response (json-str)
  (simple-response "text/json" json-str))

(defun null-response ()
  (json-response "null"))
  



;;
;; word utilites
;;

(defun word-assoc (w)
  (list 
   (cons "word" (vword-word w))
   (cons "definition" (vword-definition w))
   (cons "sentence" (vword-sentence w))
   (cons "hint" (vword-hint w))))

(defun word-json (w)
  (with-output-to-string (str)
    (encode-json (word-assoc w) str)))

(defun word-list-json (words)
  (with-output-to-string (str)
    (encode-json (mapcar #'word-assoc words) str)))

;;
;; general utilities
;;

(defun handler-w-args (uri-reg fn)
  #'(lambda ()
      (multiple-value-bind (_ matches) (scan-to-strings uri-reg (request-uri* *request*))
        (apply (symbol-function fn) (coerce-array-to-list matches)))))

(defmacro bind-urls (binding-forms)
  (let ((binding-form (gensym))
        (uri-reg (gensym))
        (fn (gensym)))
    `(dolist (,binding-form (reverse ',binding-forms))
       (let* ((,uri-reg (car ,binding-form))
              (,fn (cadr ,binding-form)))
         (push (create-regex-dispatcher ,uri-reg (handler-w-args ,uri-reg ,fn)) 
               *dispatch-table*)))))

(defun coerce-array-to-list (in-array) 
   (loop for i below (array-total-size in-array) 
         collect (row-major-aref in-array i))) 





