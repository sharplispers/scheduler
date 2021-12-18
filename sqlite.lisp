(in-package #:scheduler-implementation)

;;(use-package :sqlite)
;;(use-package :iter)

(defvar *db-file-path*
  "Path to the SQLite DB file.")

(defclass sqlite-scheduler (scheduler)
  ())

(defmethod initialize-instance :after ((scheduler sqlite-scheduler) &key db-path)
  (assert db-path)
  (setf *db-file-path* db-path)
  (sqlite:with-open-database (db *db-file-path*)
    (sqlite:execute-non-query
     db "create table if not exists tasks (name text primary key, task text not null)")))

(defclass sqlite-task (task)
  ((name :initarg :name :accessor task-name)))

(defmethod list-scheduler-tasks ((scheduler sqlite-scheduler))
  (sqlite:with-open-database (db *db-file-path*)
    (iterate:iter
      (iterate:for (task) in-sqlite-query "select task from tasks" on-database db)
      (iterate:collect (ms:unmarshal (read-from-string task))))))

(defmethod create-scheduler-task
    ((scheduler sqlite-scheduler) (cron-entry string)
     &key start-after name &allow-other-keys)
  (assert name)
  (if-let (task (read-scheduler-task scheduler name))
    task
    (mvb (time-specs command) (parse-cron-entry cron-entry)
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
  (sqlite:with-open-database (db *db-file-path*)
    (sqlite:execute-non-query db
                              "insert into tasks (name, task) values (?,? )"
                              (task-name task)
                              (format nil "~S" (ms:marshal task))))
  task)

(defmethod read-scheduler-task
    ((scheduler sqlite-scheduler) (name string))
  (sqlite:with-open-database (db *db-file-path*)
    (if-let (task (sqlite:execute-single
                   db "select task from tasks where name = ?" name))
      (ms:unmarshal (read-from-string task))
      nil)))

(defmethod update-scheduler-task
    ((scheduler sqlite-scheduler) (task task)
     &key (cron-entry nil ce-p) (last-run nil lr-p) (start-at nil at-p))
  (assert (read-scheduler-task scheduler (task-name task)))
  (when ce-p
    (mvb (time-specs command) (parse-cron-entry cron-entry)
      (setf (task-time-specs task) time-specs
            (task-command task) command)))
  (when at-p (setf (task-next-execution task) start-at))
  (when lr-p (setf (task-last-execution task) last-run))
  (sqlite:with-open-database (db *db-file-path*)
    (sqlite:execute-non-query db
                              "update tasks set task = ? where name = ?"
                              (format nil "~S" (ms:marshal task))
                              (task-name task)))
  task)

(defmethod delete-scheduler-task
    ((scheduler sqlite-scheduler) (name string))
  (sqlite:with-open-database (db *db-file-path*)
    (sqlite:execute-non-query db "delete from tasks where name = ?" name)))

(defmethod ms:class-persistent-slots ((self sqlite-task))
  '(scheduler-implementation::name
    scheduler-implementation::time-specs
    scheduler-implementation::command
    scheduler-implementation::last-execution
    scheduler-implementation::next-execution
    scheduler-implementation::source-entry))

(defmethod ms:class-persistent-slots ((self local-time:timestamp))
  '(local-time::day
    local-time::sec
    local-time::nsec))
