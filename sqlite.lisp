(in-package #:cl)

(defpackage #:scheduler-sqlite
  (:use #:cl #:alexandria #:sqlite #:scheduler-implementation)
  (:export #:sqlite-scheduler
           ;; task
           #:sqlite-task))

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
      collect (ms:unmarshal (read-from-string
                             (statement-column-value statement 0)))
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
                              (format nil "~S" (ms:marshal task))))
  task)

(defmethod read-scheduler-task
    ((scheduler sqlite-scheduler) (name string))
  (sqlite:with-open-database (db (sqlite-scheduler-db-path scheduler))
    (if-let (task (sqlite:execute-single
                   db "select task from tasks where name = ?" name))
      (ms:unmarshal (read-from-string task))
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
                              (format nil "~S" (ms:marshal task))
                              (task-name task)))
  task)

(defmethod delete-scheduler-task
    ((scheduler sqlite-scheduler) (name string))
  (sqlite:with-open-database (db (sqlite-scheduler-db-path scheduler))
    (sqlite:execute-non-query db "delete from tasks where name = ?" name)))

(defmethod ms:class-persistent-slots ((self sqlite-task))
  '(name
    scheduler-implementation::time-specs
    scheduler-implementation::command
    scheduler-implementation::last-execution
    scheduler-implementation::next-execution
    scheduler-implementation::source-entry))

(defmethod ms:class-persistent-slots ((self local-time:timestamp))
  '(local-time::day
    local-time::sec
    local-time::nsec))
