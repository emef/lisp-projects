(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defstruct vword
  id word def sentence hint)

(defun get-db (&optional (dbfile "words.db"))
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
                             :def d
                             :sentence s
                             :hint h)))
            (execute-to-list db "select * from words;"))))