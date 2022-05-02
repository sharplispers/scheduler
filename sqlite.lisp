(in-package #:cl)

(defpackage #:scheduler-sqlite
  (:use #:cl #:alexandria #:sqlite #:scheduler-implementation)
  (:export #:sqlite-scheduler))

(in-package #:scheduler-sqlite)

(defclass sqlite-scheduler (scheduler)
  ((db-path :initarg :db-path :initform (error "DB-PATH is required.")
            :accessor sqlite-scheduler-db-path)))

(defmethod initialize-instance :after ((scheduler sqlite-scheduler) &key)
  (sqlite:with-open-database (db (slot-value scheduler 'db-path))
    (sqlite:execute-non-query
     db "create table if not exists tasks (name text primary key, task text not null)")))

(defclass sqlite-task (task)
  ((name :initarg :name :accessor task-name)))

(defmethod list-scheduler-tasks ((scheduler sqlite-scheduler))
  (sqlite:with-open-database (db (sqlite-scheduler-db-path scheduler))
    (loop
      with statement = (prepare-statement db "select task from tasks")
      while (step-statement statement)
      collect (deserialize-task (statement-column-value statement 0))
      finally (finalize-statement statement))))

(defmethod create-scheduler-task
    ((scheduler sqlite-scheduler) (cron-entry string)
     &key start-after name &allow-other-keys)
  (assert name)
  (if-let (task (read-scheduler-task scheduler name))
    task
    (multiple-value-bind (time-specs command) (parse-cron-entry cron-entry)
      (create-scheduler-task
       scheduler
       (make-instance 'sqlite-task :time-specs time-specs
                                   :command command
                                   :start-after start-after
                                   :source-entry cron-entry
                                   :name name)))))

(defmethod create-scheduler-task
    ((scheduler sqlite-scheduler) (task task)
     &key &allow-other-keys)
  (sqlite:with-open-database (db (sqlite-scheduler-db-path scheduler))
    (sqlite:execute-non-query db
                              "insert into tasks (name, task) values (?,? )"
                              (task-name task)
                              (serialize-task task)))
  task)

(defmethod read-scheduler-task
    ((scheduler sqlite-scheduler) (name string))
  (sqlite:with-open-database (db (sqlite-scheduler-db-path scheduler))
    (if-let (task (sqlite:execute-single
                   db "select task from tasks where name = ?" name))
      (deserialize-task task)
      nil)))

(defmethod update-scheduler-task
    ((scheduler sqlite-scheduler) (task task)
     &key (cron-entry nil ce-p) (last-run nil lr-p) (start-at nil at-p))
  (assert (read-scheduler-task scheduler (task-name task)))
  (when ce-p
    (multiple-value-bind (time-specs command) (parse-cron-entry cron-entry)
      (setf (task-time-specs task) time-specs
            (task-command task) command)))
  (when at-p (setf (task-next-execution task) start-at))
  (when lr-p (setf (task-last-execution task) last-run))
  (sqlite:with-open-database (db (sqlite-scheduler-db-path scheduler))
    (sqlite:execute-non-query db
                              "update tasks set task = ? where name = ?"
                              (serialize-task task)
                              (task-name task)))
  task)

(defmethod delete-scheduler-task
    ((scheduler sqlite-scheduler) (name string))
  (sqlite:with-open-database (db (sqlite-scheduler-db-path scheduler))
    (sqlite:execute-non-query db "delete from tasks where name = ?" name)))

(defun maybe-serialize-timestamp (maybe-timestamp)
  (typecase maybe-timestamp
    (local-time:timestamp (local-time:format-timestring
                           nil maybe-timestamp))
    (t maybe-timestamp)))

(defun serialize-task (task)
  (write-to-string
   (list
    (cons :name (task-name task))
    (cons :time-specs (task-time-specs task))
    (cons :command (task-command task))
    (cons :last-execution (maybe-serialize-timestamp (task-last-execution task)))
    (cons :next-execution (maybe-serialize-timestamp (task-next-execution task)))
    (cons :source-entry (task-source-entry task)))))

(defun maybe-deserialize-timestamp (maybe-timestamp-string)
  (handler-case
      (local-time:parse-timestring maybe-timestamp-string)
    (error ()
      maybe-timestamp-string)))

(defun deserialize-task (string)
  (let* ((task-slots (read-from-string string))
         (out (allocate-instance (find-class 'sqlite-task))))
    (setf (slot-value out 'name)
          (alexandria:assoc-value task-slots :name))
    (setf (slot-value out 'scheduler-implementation::time-specs)
          (alexandria:assoc-value task-slots :time-specs))
    (setf (slot-value out 'scheduler-implementation::command)
          (alexandria:assoc-value task-slots :command))
    (setf (slot-value out 'scheduler-implementation::last-execution)
          (maybe-deserialize-timestamp
           (alexandria:assoc-value task-slots :last-execution)))
    (setf (slot-value out 'scheduler-implementation::next-execution)
          (maybe-deserialize-timestamp
           (alexandria:assoc-value task-slots :next-execution)))
    (setf (slot-value out 'scheduler-implementation::source-entry)
          (alexandria:assoc-value task-slots :source-entry))
    (shared-initialize out t)))
