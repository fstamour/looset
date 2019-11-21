(ql:quickload 'looset)

(in-package #:looset)

(ql:quickload 'postmodern)
(ql:quickload 'city-hash)

#|
nix-shell -p postgresql --pure
mkdir db
initdb -D db
createuser -P looset
# with password "password"
createdb --owner looset looset
pg_ctl -D db -l logfile start

psql --user looset

pg_ctl -D db stop
|#

(let ((db "looset")
      (user "looset")
      (password "password")
      (host "localhost"))
  (defparameter *connection-spec* (list db user password host))
  (apply #'pomo:connect-toplevel *connection-spec*))

(defmacro with-db (&body body)
  `(pomo:with-connection ',*connection-spec*
     ,@body))

#+test
(with-db
    (pomo:query (:select (:count 1) :from 'artifact)))

(defclass artifact ()
  ((id
    :col-type serial
    :reader id)
   (path
    :col-type string
    :initarg :path
    :accessor path))
  (:keys id)
  ;; TODO Unique constraint on name?
  (:metaclass pomo:dao-class))

(pomo:deftable artifact
  (pomo:!dao-def)
  (pomo:!unique-index 'path))

#|
;; macro-expanded deftable then kept the "create unique index"
(pomo:query
 (let ((postmodern:*table-name* "artifact")
       (postmodern:*table-symbol* 'artifact))
   (postmodern:!unique-index 'path)))

(pomo:create-table 'artifact)
|#

(unless (pomo:table-exists-p 'artifact)
  (pomo:execute (pomo:dao-table-definition 'artifact)))

#|
;; adding all files one by one
(time
 (let* ((count 0))
     (map-files
      (merge-pathnames
       "dists/quicklisp/software/"
       ql:*quicklisp-home*)
      #'(lambda (file)
          (pomo:query
           (:insert-into 'artifact :set 'path (namestring file)))
          (incf count)))
   (print count)))
;; For 1000 entries
;; => 2.6 to 2.9 seconds with save-dao, insert-dao or insert-into
;; => 0.12 seconds without saving to db
;; => took 840 seconds for 117809 entries

;; accumulating all file path and adding them all in one sql query.
(time
 (pomo:query
  (s-sql:sql-compile
   `(:insert-rows-into 'artifact
     :columns 'path
     :values (,@(time
                 (while-collecting
                     (collect-file)
                   (map-files
                    (merge-pathnames
                     "dists/quicklisp/software/"
                     ql:*quicklisp-home*)
                    #'(lambda (file)
                        (collect-file (list (namestring file))))))))))))
;; => took 5 seconds
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; artifact <-> artifact-line <-> line

(defclass line-hash ()
  ((id
    :col-type serial
    :reader id)
   (hash
      :col-type bytea
      :initarg :hash
      :accessor hash))
  (:keys id)
  (:metaclass pomo:dao-class))

;; (setf pomo::*tables* (list (first pomo::*tables*)))

(pomo:deftable line-hash
  (pomo:!dao-def)
  (pomo:!unique-index 'hash))

#|
(pomo:create-table 'line-hash)
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; N-to-N reletion between artifacts and line-hashes
;; artifact <-> artifact-line-hash <-> line-hash

(defclass artifact-line-hash ()
  ((artifact-id
    :col-type integer
    :accessor artifact-id)
   (line-hash-id
    :col-type integer
    :accessor line-hash-id))
  (:keys artifact-id line-hash-id)
  (:metaclass pomo:dao-class))

(pomo:deftable artifact-line-hash
  (pomo:!dao-def)
  (pomo:!foreign 'line-hash 'line-hash-id 'id :on-delete :cascade)
  (pomo:!foreign 'artifact 'artifact-id 'id :on-delete :cascade))

#|
(pomo:create-table 'artifact-line-hash)
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+nil
(time
 (file-to-line-hash-list
  (with-db
    (pomo:query
     (:limit (:select 'path :from 'artifact) 1)
     :single))))

;; TODO handle line endings beter?
(defun file-to-line-hash-list (path)
  "Read a file, returns a list of hash (one per line, deduplicated)."
  (remove-duplicates
   (loop :for line :in (split-sequence-if
                        #'(lambda (byte)
                            (or (eq byte 10)
                                (eq byte 13)))
                        (alexandria:read-file-into-byte-vector path))
         :collect (city-hash:city-hash-64 line))))

;; RENDU

(defun db/put-line-hash-list (hash-list)
  "Get or create \"line-hashes\", returns their id."
  (pomo:query
   (format nil
           "with
  input as (select * from (values ~{('\\x~16,'0x'::bytea)~^,~}) as t (hash)),
  new_row as (insert into line_hash (hash)
      select hash from input on conflict do nothing returning id)
  select id from new_row union
  select id from line_hash where hash in (select hash from input);
" hash-list)
   :column))

#+nil
(db/put-line-hash-list '(10 11))

(pomo:defprepared-with-names db/put-artifact-line-hash
    (artifact-id line-hash-id)
    ("insert into artifact_line_hash
  (artifact_id, line_hash_id)
  values ($1, $2)
  on conflict do nothing;"
     artifact-id
     line-hash-id))

;; Map through files
#|
(time
 (pomo:doquery
     (:select 'id 'path :from 'artifact)
     (artifact-id path)
   (let ((hash-id-list (with-db
                         (db/put-line-hash-list
                          (file-to-line-hash-list path)))))
     (loop :for hash-id :in hash-id-list
           :do (with-db
                 (db/put-artifact-line-hash artifact-id hash-id))))))

It ran for 44.66 hours before I stopped it.

Evaluation took:
160768.765 seconds of real time
6512.900028 seconds of total run time (3040.509649 user, 3472.390379 system)
[ Run times consist of 13.645 seconds GC time, and 6499.256 seconds non-GC time. ]
4.05% CPU
384,954,960,468,280 processor cycles
15 page faults
295,018,528,544 bytes consed

before it was aborted by a non-local transfer of control.
|#

;; kill worker threads
(defun kill-worker-threads ()
  (mapcar 'bt:destroy-thread
          (remove-if-not
           #'(lambda (thread)
               (and
                (not (eq thread (bt:current-thread)))
                (string= "worker" (bt:thread-name thread))))
           (bt:all-threads))))

(defmacro do-artifacts (&body body)
  (with-gensyms
      (total count percent new-percent)
    `(let ((,total (pomo:query
                    (:select (:count 1) :from 'artifact)
                    :single))
           (,count 0)
           (,percent 0))
       (pomo:doquery
           (:select 'id 'path :from 'artifact)
           (artifact-id path)
         ,@body
         (incf ,count)
         (let ((,new-percent (floor
                              (* 100
                                 (/ ,count ,total)))))
           (when (> (- ,new-percent ,percent) 1)
             (setf ,percent ,new-percent)
             (format *debug-io* "~&~d (~d%) files processed~%"
                     ,count ,percent)))))))

#|
(time
 (do-artifacts))
;; => 0.372 seconds

(time
 (do-artifacts
   (file-to-line-hash-list path)))
;; 41.163 seconds to hash all the files

(time
 (let ((hashes (make-hash-table)))
   (do-artifacts
     (loop :for hash :in (file-to-line-hash-list path)
           :do (setf (gethash hash hashes) artifact-id)))))
;; 39.36 seconds

(time
 (let ((hashes '()))
   (do-artifacts
     (loop :for hash :in (file-to-line-hash-list path)
           :do (push hash hashes)))))
;; heap exhausted on first try
;; ran sbcl with --dynamic-space-size 4096
;; then it ran in 36 seconds
|#

(defvar *line-hashes-file* (merge-pathnames "prototypes/line-hashes"
                                       (asdf:component-pathname
                                        (asdf:find-system 'looset))))

(time
 (with-output-to-file (*standard-output*
                       *line-hashes-file*
                       :if-exists :overwrite
                       :if-does-not-exist :create)
   (do-artifacts
     (loop :for hash :in (file-to-line-hash-list path)
           :do (format t "~d ~d " hash artifact-id)))))
;; 97 seconds
;; the resulting file is 606M

(time
 (with-input-from-file (hashes *line-hashes-file*)
   (loop :for hash = (read hashes nil)
         :while hash
         :for artifact-id = (read hashes nil)
         :count hash)))
;; => 23 163 990
;; 32 seconds

(defun read-n-hashes (stream n)
  (loop
    :for i :below n
    :for hash = (read stream nil)
    :while hash
    :for artifact-id = (read stream nil)
    :collect (list hash artifact-id)))

(time
 (with-input-from-file (hashes *line-hashes-file*)
   (read-n-hashes hashes 1000)))
;; 0.001 seconds

(time
 (with-input-from-file (hashes *line-hashes-file*)
   (read-n-hashes hashes 100000)))
;; 0.140 seconds

;; (/ 23163990 100000.0) => 231

(time
 (with-input-from-file
     (hashes *line-hashes-file*)
   (loop :for line-hash-list = (read-n-hashes hashes 100000)
         :for i :from 0
         :while line-hash-list
         :do (let ((hash-id-list (db/put-line-hash-list
                                  (mapcar #'first line-hash-list))))
               (print i)
               #+nil (loop :for hash-id :in hash-id-list
                     :do (with-db
                           (db/put-artifact-line-hash artifact-id hash-id)))))))
;; => 460 seconds (a bit more than 7 minutes)
;; => 367 seconds the second time

(time
 (with-input-from-file
     (hashes *line-hashes-file*)
   (loop :for line-hash-list = (read-n-hashes hashes 100000)
         :for i :from 0
         :while line-hash-list
         :do (let ((hash-id-list (db/put-line-hash-list
                                  (mapcar #'first line-hash-list))))
               (print i)
               (time
                ;; RENDU
                (loop :for hash-id :in hash-id-list
                      :do (with-db
                            (db/put-artifact-line-hash artifact-id hash-id))))))))
(kill-worker-threads)


#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; sha1sum
(with-output-to-string (*standard-output*)
  (uiop/run-program:run-program
   "sha1sum ~/.bashrc" :ignore-error-status t
   :output t))
|#


