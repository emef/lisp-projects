(in-package #:gre)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defconstant +db-file+ "/Users/mattforbes/src/lisp/gre/words.db")

(defstruct vword 
  id 
  word 
  definition 
  sentence 
  hint)

(defun get-db (&optional (dbfile +db-file+))
  (let ((db (connect dbfile)))
    (when (not (member '("words")
                       (get-tables db)
                       :test #'equal))
      (execute-non-query db "create table words (
id integer primary key,
word text not null,
definition text not null,
sentence text,
hint text)"))
    db))

(let ((names nil))
  (defun get-tables (db)
    (when (null names)
      (setf names (execute-to-list db "select name from sqlite_master")))
    names))

(defmacro with-words-db ((db) &body body)
  `(let ((,db (get-db)))
     (unwind-protect
          (progn ,@body)
       (disconnect ,db))))

(defun get-word (word)
  (with-words-db (db)
    (multiple-value-bind (id definition sentence hint)
        (execute-one-row-m-v db "select id, definition, sentence, hint 
from words 
where word=?" word)
      (when id
        (make-vword :id id
                    :word word
                    :definition definition
                    :sentence sentence
                    :hint hint)))))
      
      
(defun insert-word (word definition &key (sentence nil) (hint nil))
  (with-words-db (db)
    (execute-non-query db "insert into words (word, definition, sentence, hint) values(?, ?, ?, ?)" 
                       word definition sentence hint)))

(defun delete-word (word)
  (with-words-db (db)
    (execute-non-query db "delete from words where word=?" word)))

(defun update-word (word &key definition sentence hint)
  (let (setters)
    (when definition
      (push (list "definition" definition) setters))
    (when sentence
      (push (list "sentence" sentence) setters))
    (when hint
      (push (list "hint" hint) setters))
    (and setters
         (with-words-db (db)
           (let ((query 
                  (format nil 
                          "update words set ~{~{~a=\"~a\"~}~^, ~} where word=\"~a\"" setters word)))
             (execute-non-query db query))))))

(defun get-words ()
  (with-words-db (db)
    (mapcar #'(lambda (row)
                (destructuring-bind (i w d s h) row
                  (make-vword :id i
                             :word w
                             :definition d
                             :sentence s
                             :hint h)))
            (execute-to-list db "select * from words;"))))