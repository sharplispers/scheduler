;;; Copyright (c) 2017, Emergent Network Defense <http://endsecurity.com/>

(defpackage #:scheduler
  (:use)
  (:export #:scheduler
           #:in-memory-scheduler
           #:start-scheduler
           #:stop-scheduler
           ;; CRUDL
           #:create-scheduler-task
           #:read-scheduler-task
           #:update-scheduler-task
           #:delete-scheduler-task
           #:list-scheduler-tasks))

(defpackage #:scheduler-implementation
  (:use #:cl #:alexandria)
  (:import-from #:scheduler
                #:scheduler
                #:in-memory-scheduler
                #:start-scheduler
                #:stop-scheduler
                #:create-scheduler-task
                #:read-scheduler-task
                #:update-scheduler-task
                #:delete-scheduler-task
                #:list-scheduler-tasks)
  (:export #:scheduler #:in-memory-scheduler
           #:start-scheduler #:stop-scheduler
           #:create-scheduler-task
           #:read-scheduler-task
           #:update-scheduler-task
           #:delete-scheduler-task
           #:list-scheduler-tasks
           ;; entry
           #:scheduler-entry
           #:cron-entry
           #:parse-entry
           ;; task
           #:task
           #:task-time-specs
           #:task-command
           #:task-last-execution
           #:task-next-execution
           #:task-source-entry
           #:execute-task))
(in-package #:scheduler-implementation)

;; utils
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro ss (&rest args)
    `(split-sequence:split-sequence #\space ,@args))
  (defmacro db (lambda-list expression &body body)
    `(destructuring-bind ,lambda-list ,expression ,@body))
  (defmacro mvb (lambda-list expression &body body)
    `(multiple-value-bind ,lambda-list ,expression ,@body))
  (defmacro ^if (test if-true if-false)
    `(let ((^it ,test)) (if ^it ,if-true ,if-false))))

(defun seed-random-state (seed)
  "Returns a new random state seeded with `object'."
  #-(or ecl sbcl) (declare (ignore seed))
  #+ecl(make-random-state seed)
  #+sbcl(sb-ext:seed-random-state seed)
  #-(or ecl sbcl) (make-random-state #.(make-random-state *random-state*)))

(defun parse-cron-spec (line)
  (flet ((split-at ()
           (multiple-value-bind (spec sp) (ss line :count 1)
             (values (alexandria:eswitch ((car spec) :test 'equal)
                       ("@reboot"   :reboot)
                       ("@shutdown" :shutdown)
                       ("@yearly"   '#.(ss "H H H H *"))
                       ("@annually" '#.(ss "H H H H *"))
                       ("@monthly"  '#.(ss "H H H * *"))
                       ("@weekly"   '#.(ss "H H * * H"))
                       ("@daily"    '#.(ss "H H * * *"))
                       ("@midnight" '#.(ss "H H * * *"))
                       ("@hourly"   '#.(ss "H * * * *")))
                     (subseq line sp))))
         (split ()
           (multiple-value-bind (specs sp) (ss line :count 5)
             (values specs (subseq line sp)))))
    (multiple-value-bind (specs command)
        (if (char/= #\@ (elt line 0))
            (split)
            (split-at))
      (assert (or (member specs '(:reboot :shutdown))
                  (alexandria:length= 5 specs)))
      (cons specs (if (emptyp command) "nil" command)))))

#+test
(funcall
 (defun test-parse-cron-spec ()
   (assert (equalp (parse-cron-spec "@reboot (lambda () (list 1 2 3))") `(:reboot . "(lambda () (list 1 2 3))")))
   (assert (equalp (parse-cron-spec "@shutdown (lambda () (list 1 2 3))") `(:shutdown . "(lambda () (list 1 2 3))")))
   (assert (equalp (parse-cron-spec "@yearly command") `(("H" "H" "H" "H" "*") . "command")))
   (assert (equalp (parse-cron-spec "@yearly command") (parse-cron-spec "@annually command")))
   (assert (equalp (parse-cron-spec "@daily command") (parse-cron-spec "@midnight command")))
   (assert (not (equalp (parse-cron-spec "@daily command!") (parse-cron-spec "@midnight command"))))
   (assert (not (equalp (parse-cron-spec "@weekly command bar") (parse-cron-spec "@weekly command"))))
   (assert (equalp (parse-cron-spec "1 0 1/2 * 1,2 command foo") `(("1" "0" "1/2" "*" "1,2") . "command foo")))
   (assert (null (ignore-errors (parse-cron-spec "1 0 1/2"))))
   (assert (null (ignore-errors (parse-cron-spec "@foo 3 4 "))))
   (assert (null (ignore-errors (parse-cron-spec "@foo 3 4 1 1 2"))))
   #+ (or) ;; let it slide as nil.
   (assert (null (ignore-errors (parse-cron-spec "@yearly "))))))

(defun %parse-cron-time-1/no-step (spec range)
  (optima:ematch spec
    ("*" :every)
    ("H" (list (alexandria:random-elt range)))
    ;; 1,2,4,8 | 1-4,5,6 etc
    ((or (optima.ppcre:ppcre "^[0-9]+,.*")
         (optima.ppcre:ppcre "^[0-9]+-[0-9]*,.*"))
     ;; (mapcar #'parse-integer (ppcre:split "," spec))
     (alexandria:mappend
      (lambda (spec-1)
        (%parse-cron-time-1/no-step spec-1 range))
      (ppcre:split "," spec)))
    ;; H(1-12)
    ((optima.ppcre:ppcre "H\\(([0-9]+)-([0-9]+)\\)" from to)
     (let* ((from (parse-integer from))
            (to (parse-integer to))
            (size (1+ (- to from))))
       (list (alexandria:random-elt (alexandria:iota size :start from)))))
    ;; 1-12
    ((optima.ppcre:ppcre "([0-9]+)-([0-9]+)" from to)
     (let* ((from (parse-integer from))
            (to (parse-integer to))
            (size (1+ (abs (- to from)))))
       (alexandria:iota size :start (min to from))))
    ((optima.ppcre:ppcre "(^[0-9]+)" number)
     (list (parse-integer number)))))

(defun %parse-cron-time-1/step (spec range step)
  (flet ((every-nth (list step)
           (if (= 1 step)
               list
               (loop for elt in list by #'(lambda (l) (nthcdr step l))
                  collect elt)))
         (every-nth* (from to step)
           (let ((set-size (1+ (floor (/ (- to from) step)))))
             (alexandria:iota set-size :start from :step step))))
    (optima:ematch spec
      ;; */2
      ("*" (every-nth range step))
      ;; H/2
      ("H" (every-nth (nthcdr (random step) range) step))
      ;; H(1-14)/3
      ((optima.ppcre:ppcre "H\\(([0-9]+)-([0-9]+)\\)" from to)
       (let* ((to (parse-integer to))
              (from (min to
                         (alexandria:random-elt
                          (alexandria:iota step
                                           :start (parse-integer from))))))
         (every-nth* from to step)))
      ;; 1-14/3
      ((optima.ppcre:ppcre "([0-9]+)-([0-9]+)" from to)
       (every-nth* (parse-integer from)
                   (parse-integer to)
                   step))
      #+invalid ; this is invalid, but we catch it with ematch anyway
      ((optima.ppcre:ppcre "[H]?.*,.*")
       (error "invalid clause")))))

(defun parse-cron-time-1 (spec range)
  (db (base . step) (ppcre:split "/" spec)
    (let ((parse-result
           (if (null step)
               (%parse-cron-time-1/no-step base range)
               (%parse-cron-time-1/step base range
                                        (parse-integer (car step))))))
      (assert (or (eql parse-result :every)
                  (every (lambda (n) (member n range)) parse-result))
              nil "PARSE-CRON-TIME-1: Each element of ~s must be a member of:~%~s."
              parse-result (cons :every range))
      parse-result)))

#+test
(funcall
 (defun test-parse-cron-time-1 (&aux (range (alexandria:iota 30)))
   (assert (equalp (parse-cron-time-1 "*" range) :every))
   (assert (equalp (parse-cron-time-1 "*/2" '(1 2 3 4 5 6 7)) '(1 3 5 7)))
   (assert (equalp (parse-cron-time-1 "*/3" '(1 2 3 4 5 6 7)) '(1 4 7)))
   (assert (equalp (parse-cron-time-1 "18" range) '(18)))
   (assert (equalp (parse-cron-time-1 "1-4,7-9,12" range) '(1 2 3 4 7 8 9 12)))
   (assert (equalp (parse-cron-time-1 "1,4-7" range) '(1 4 5 6 7)))
   (assert (equalp (parse-cron-time-1 "0" (alexandria:iota 7)) '(0)))
   (assert (equalp (parse-cron-time-1 "2-6" '#.(alexandria:iota 7)) '(2 3 4 5 6)))
   (let ((result (parse-cron-time-1 "H/2" '(1 2 3 4 5 6 7))))
     (assert (or (equalp result '(1 3 5 7))
                 (equalp result '(2 4 6)))))
   (let ((result (parse-cron-time-1 "H(8-12)/2" range)))
     (assert (or (equalp result '(8 10 12)) (equalp result '(9 11)))))
   (assert (member (first (parse-cron-time-1 "H(8-12)" range))
                   (alexandria:iota 5 :start 8)))
   (assert (null (ignore-errors (parse-cron-time-1 "H(1,2)" range))))
   (assert (null (ignore-errors (parse-cron-time-1 "1,2,4/2" '(1 2 3 4)))))
   (assert (null (ignore-errors (parse-cron-time-1 "18/2" range))))
   ;; we let that pass (due to being lazy) "1/" === "1"
   #+ (or) (assert (null (handler-case (parse-cron-time-1 "1/" range) (error () nil))))))


;;; XXX: backward compatibility (mainly for tests)
(defun parse-cron-entry (spec)
  (parse-entry (make-instance 'cron-entry :string spec)))

#+test
(funcall
 (defun test-parse-cron-entry ()
   (parse-cron-entry "@reboot foo")
   (parse-cron-entry "@shutdown foo")
   (parse-cron-entry "@yearly foo")
   (parse-cron-entry "@weekly foo")
   (parse-cron-entry "@daily foo")
   (parse-cron-entry "@midnight foo")
   (parse-cron-entry "@annually foo")
   (assert (null (ignore-errors (parse-cron-entry "0 0 0 0 0 foo"))))
   (assert (equal (getf (parse-cron-entry "* * * * 0 foo") :day-of-week) '(0)))
   (assert (equal #1=(parse-cron-entry "H H H H H foo") #1#))
   (assert (null (equal (parse-cron-entry "H H H H H bar") #1#)))))

(defclass scheduler-entry () ())
(defgeneric parse-entry (entry)
  (:documentation "Parses entry to return two values: the time spec and command."))

(defclass cron-entry (scheduler-entry)
  ((string :initarg :string :reader cron-entry-string)))

(defmethod parse-entry ((entry cron-entry) &aux (spec (parse-cron-spec (cron-entry-string entry))))
  (when (member (car spec) '(:reboot :shutdown))
    (return-from parse-entry (values `(:event ,(car spec)) (cdr spec))))
  (destructuring-bind ((minute hour day-of-month month day-of-week) . command) spec
    ;; XXX: seeding `*random-state*' with command ensures, that
    ;; randomness is stable. This is useful for parsing H entries.
    (let ((*random-state* (seed-random-state (sxhash command))))
      (values
       (list :minute (parse-cron-time-1 minute '#.(alexandria:iota 60))
             :hour (parse-cron-time-1 hour '#.(alexandria:iota 24))
             :day-of-month (if (char= (elt day-of-month 0) #\H)
                               (parse-cron-time-1 day-of-month '#.(alexandria:iota 28 :start 1))
                               (parse-cron-time-1 day-of-month '#.(alexandria:iota 31 :start 1)))
             :month (parse-cron-time-1 month '#.(alexandria:iota 12 :start 1))
             :day-of-week (if (string= day-of-week "7")
                              (parse-cron-time-1 "0" '#.(alexandria:iota 7))
                              (parse-cron-time-1 day-of-week '#.(alexandria:iota 7))))
       command))))


;;; we assume here that all `:random' entries are already picked and
;;; if `:step' present already coerced to sets.
(defun compute-next-occurance (spec &optional time)
  ;; algorithm is based on
  ;; https://stackoverflow.com/questions/321494/calculate-when-a-cron-job-will-be-executed-then-next-time#3453872
  (when (keywordp time) (return-from compute-next-occurance time))
  (when (null time) (setf time (local-time:now)))
  (local-time:adjust-timestamp! time
    (set :nsec 0) (set :sec (random 60)) (offset :minute 1))
  (flet ((match-spec (obj spec)
           (or (eql :every spec)
               (member obj spec)))
         (next-fit (n set) (find-if (lambda (s) (< n s)) set))
         (first* (set default)
           (if (eql set :every)
               default
               (first set)))
         (day-offset (now set default)
           (let ((day (local-time:timestamp-day now))
                 (mday (local-time:days-in-month
                        (local-time:timestamp-month now)
                        (local-time:timestamp-year now)))
                 (target (if (eql set :every)
                             default
                             (first set))))
             (+ (- mday day) target))))
    (symbol-macrolet ((next.minute (local-time:timestamp-minute time))
                      (next.hour (local-time:timestamp-hour time))
                      (next.weekday (local-time:timestamp-day-of-week time))
                      (next.day (local-time:timestamp-day time))
                      (next.month (local-time:timestamp-month time))
                      (next.year (local-time:timestamp-year time)))
      (db (&key minute hour day-of-month month day-of-week event) spec
          (when event (return-from compute-next-occurance event))
          (loop
             do (block nil
                  ;; nudge minute
                  (unless (match-spec next.minute minute)
                    (^if (next-fit next.minute minute)
                         (local-time:adjust-timestamp! time
                           (set :minute to ^it))
                         (local-time:adjust-timestamp! time
                           (offset :hour by 1)
                           (set :minute to (first* minute 0))))
                    (return))
                  ;; nudge hour
                  (unless (match-spec next.hour hour)
                    (^if (next-fit next.hour hour)
                         (local-time:adjust-timestamp! time
                           (set :hour to ^it)
                           (set :minute to (first* minute 0)))
                         (local-time:adjust-timestamp! time
                           (offset :day by 1)
                           (set :hour to (first* hour 0))
                           (set :minute to (first* minute 0))))
                    (return))
                  ;; nudge weekday
                  (unless (match-spec next.weekday day-of-week)
                    (let ((delta (- (first day-of-week) next.weekday)))
                      (when (minusp delta) (incf delta 7))
                      (local-time:adjust-timestamp! time
                        (offset :day by delta)
                        (set :hour to (first* hour 0))
                        (set :minute to (first* minute 0)))
                      (return)))
                  ;; nudge month day
                  (unless (match-spec next.day day-of-month)
                    (^if (next-fit next.day day-of-month)
                         (local-time:adjust-timestamp! time
                           (offset :day by (- ^it next.day))
                           (set :hour to (first* hour 0))
                           (set :minute to (first* minute 0)))
                         (local-time:adjust-timestamp! time
                           (offset :day by (day-offset time day-of-month 1))
                           (set :hour to (first* hour 0))
                           (set :minute to (first* minute 0))))
                    (return))
                  ;; nudge month
                  (unless (match-spec next.month month)
                    (^if (next-fit next.month month)
                         (local-time:adjust-timestamp! time
                           (set :month to ^it)
                           ;; always get back to 1 to prevent month bump!
                           (offset :day by (1+ (- next.day)))
                           (set :hour to (first* hour 0))
                           (set :minute to (first* minute 0)))
                         (local-time:adjust-timestamp! time
                           (offset :month by (+ (- 12 next.month)
                                                (first month)))
                           ;; same as above
                           (offset :day by (1+ (- next.day)))
                           (set :hour to (first* hour 0))
                           (set :minute to (first* minute 0))))
                    (return))
                  (return-from compute-next-occurance time)))))))

#+test
(funcall
 (defun test-compute-next-occurance ()
   (flet ((et (&rest args) (apply #'local-time:encode-timestamp 0 0 args))
          (st (time) (local-time:adjust-timestamp time (set :sec 0))))
     (assert (local-time:timestamp=
              (st (compute-next-occurance (parse-cron-entry "0 0 1 1 0 foo")))
              (et 0 0 1 1 2023)))
     (assert (local-time:timestamp=
              (st (compute-next-occurance
                   (parse-cron-entry "0 0 1 1 * foo") (et 1 0 1 1 2017)))
              (et 0 0 1 1 2018)))
     (assert (and (local-time:timestamp<=
                   (et 0 0 1 1 2018)
                   (st (compute-next-occurance
                        (parse-cron-entry "H H 1 1 * foo") (et 0 0 2 1 2017))))
                  (local-time:timestamp>=
                   (et 59 23 1 1 2018)
                   (st (compute-next-occurance
                        (parse-cron-entry "H H 1 1 * foo") (et 0 0 2 1 2017))))))
     (assert
      (null (ignore-errors (compute-next-occurance
                            (parse-cron-entry "0 0 0 0 0 foo"))))))))


(defclass scheduler ()
  ((%state :initform :stopped :accessor scheduler-state)
   (%lock :initform (bt:make-recursive-lock "scheduler lock") :accessor scheduler-lock))
  (:documentation "Thread-safe abstract scheduler class."))

(defclass task ()
  ((time-specs :initarg :time-specs :accessor task-time-specs)
   (command :initarg :command :accessor task-command)
   (last-execution :initform nil :initarg :last-execution :accessor task-last-execution)
   (next-execution :initform nil :initarg :next-execution :accessor task-next-execution)
   (source-entry :initform nil :initarg :source-entry :accessor task-source-entry)))

(defmethod initialize-instance :after ((task task) &key time-specs start-after)
  (setf (task-next-execution task) (compute-next-occurance time-specs start-after)))

(defmethod execute-task ((task task))
  (eval (read-from-string (task-command task))))

;;; This type of a task may be harder to serialize. The command is a function
;;; without arguments.
(defclass lambda-task (task) ())

(defmethod initialize-instance :after ((task lambda-task) &key)
  (check-type (task-command task) function))

(defmethod execute-task ((task lambda-task))
  (funcall (task-command task)))

(defgeneric create-scheduler-task (scheduler time-entry
                                   &key start-after &allow-other-keys)
  (:method :around ((scheduler scheduler) entry &key &allow-other-keys)
    (declare (ignore entry))
    (bt:with-recursive-lock-held ((scheduler-lock scheduler))
      (call-next-method)))
  (:method (scheduler (cron-entry string)
            &key start-after &allow-other-keys)
    (mvb (time-specs command) (parse-cron-entry cron-entry)
      (create-scheduler-task
       scheduler
       (make-instance 'task :time-specs time-specs
                            :command command
                            :start-after start-after
                            :source-entry cron-entry))))
  (:method (scheduler (cron-entry-and-lambda cons)
            &key start-after &allow-other-keys)
    (db (cron-entry . command) cron-entry-and-lambda
      (let ((time-specs (parse-cron-entry cron-entry)))
        (create-scheduler-task
         scheduler
         (make-instance 'lambda-task :time-specs time-specs
                                     :command command
                                     :start-after start-after
                                     :source-entry cron-entry-and-lambda))))))

(defgeneric read-scheduler-task (scheduler task)
  (:method :around ((scheduler scheduler) task)
    (declare (ignore task))
    (bt:with-recursive-lock-held ((scheduler-lock scheduler))
      (call-next-method))))

(defgeneric update-scheduler-task
    (scheduler task &key cron-entry start-at &allow-other-keys)
  (:method :around ((scheduler scheduler) task &key)
    (declare (ignore task))
    (bt:with-recursive-lock-held ((scheduler-lock scheduler))
      (call-next-method))))

(defgeneric delete-scheduler-task (scheduler task)
  (:method :around ((scheduler scheduler) task)
    (declare (ignore task))
    (bt:with-recursive-lock-held ((scheduler-lock scheduler))
      (call-next-method))))

(defgeneric list-scheduler-tasks (scheduler)
  (:method :around ((scheduler scheduler))
    (bt:with-recursive-lock-held ((scheduler-lock scheduler))
      (call-next-method)))
  (:documentation "Lists all tasks registered with the scheduler."))

(defun start-scheduler (scheduler)
  (format *debug-io* "~&Starting a scheduler.~%")
  (setf (scheduler-state scheduler) :running)
  (labels ((missed-task? (task)
             (and (typep (task-next-execution task) 'local-time:timestamp)
                  (local-time:timestamp< (task-next-execution task)
                                         (local-time:now))))

           (active-task? (task timespec)
             (if (and (typep timespec 'local-time:timestamp)
                      (typep (task-next-execution task) 'local-time:timestamp))
                 (local-time:timestamp= (task-next-execution task)
                                        (local-time:adjust-timestamp timespec (set :nsec 0)))
                 (eql (task-next-execution task) timespec)))

           (run-valid-tasks (time/event-spec)
             (dolist (task (list-scheduler-tasks scheduler))
               (unless (task-next-execution task)
                 (update-scheduler-task scheduler task
                                        :start-at (compute-next-occurance
                                                   (task-time-specs task)
                                                   (local-time:now))))
               (cond
                 ((missed-task? task)
                  (execute-task task)
                  (update-scheduler-task scheduler task
                                         :last-run time/event-spec
                                         :start-at (compute-next-occurance
                                                    (task-time-specs task)
                                                    (local-time:now))))
                 ((active-task? task time/event-spec)
                  (execute-task task)
                  (update-scheduler-task scheduler task
                                         :last-run time/event-spec
                                         :start-at (compute-next-occurance
                                                    (task-time-specs task)
                                                    (local-time:now))))))))

    (run-valid-tasks :reboot)
    (loop while (eql (scheduler-state scheduler) :running)
       do (progn (run-valid-tasks (local-time:now)) (sleep 1)))
    (run-valid-tasks :shutdown))
  (format *debug-io* "~&Exiting a scheduler.~%")
  (setf (scheduler-state scheduler) :stopped))

(defun stop-scheduler (scheduler)
  (setf (scheduler-state scheduler) :exit))


(defclass in-memory-scheduler (scheduler)
  ((tasks :initform nil :accessor list-scheduler-tasks)))

(defmethod create-scheduler-task
    ((scheduler in-memory-scheduler) (task task)
     &key &allow-other-keys)
  (car (push task (list-scheduler-tasks scheduler))))

(defmethod read-scheduler-task
    ((scheduler in-memory-scheduler) (task task))
  (find task (list-scheduler-tasks scheduler)))

(defmethod update-scheduler-task
    ((scheduler in-memory-scheduler) (task task)
     &key (cron-entry nil ce-p) (last-run nil lr-p) (start-at nil at-p))
  (assert (find task (list-scheduler-tasks scheduler)))
  (when ce-p
    (mvb (time-specs command) (parse-cron-entry cron-entry)
      (setf (task-time-specs task) time-specs
            (task-command task) command)))
  (when at-p (setf (task-next-execution task) start-at))
  (when lr-p (setf (task-last-execution task) last-run))
  task)

(defmethod delete-scheduler-task
    ((scheduler in-memory-scheduler) (task task))
  (setf (list-scheduler-tasks scheduler) (delete task (list-scheduler-tasks scheduler))))

#+test
(let (foobar xxx yyy)
  (defun %foobar () (incf foobar))
  (defun %xxx () (incf xxx))
  (defun %yyy () (incf yyy))
  (funcall
   (defun test-in-memory-scheduler-1 ()
     (setf foobar 0 xxx 0 yyy 0)
     (let* ((scheduler (make-instance 'in-memory-scheduler))
            (t1 (create-scheduler-task scheduler "@reboot (scheduler-implementation::%xxx)"))
            (t2 (create-scheduler-task scheduler "@shutdown (scheduler-implementation::%yyy)")))
       (create-scheduler-task scheduler "@reboot (scheduler-implementation::%xxx)")
       (create-scheduler-task scheduler "@reboot (scheduler-implementation::%xxx)")
       (create-scheduler-task scheduler "@shutdown (scheduler-implementation::%yyy)")
       (create-scheduler-task scheduler (cons "@shutdown" #'scheduler-implementation::%yyy))
       ;; we remove one reboot task *before* starting the scheduler
       (delete-scheduler-task scheduler t1)
       (let ((thread (bt:make-thread (lambda () (start-scheduler scheduler)))))
         (sleep 1)
         ;; and one shutdown task *before* stopping the scheduler
         (delete-scheduler-task scheduler t2)
         (sleep 1)
         (stop-scheduler scheduler)
         (bt:join-thread thread))
       (assert (= xxx yyy 2)))))
  (funcall
   (defun test-in-memory-scheduler-2 ()
     (setf foobar 0 xxx 0 yyy 0)
     (let ((scheduler (make-instance 'in-memory-scheduler)))
       (create-scheduler-task scheduler "@reboot (scheduler-implementation::%xxx)")
       (create-scheduler-task scheduler "@shutdown (scheduler-implementation::%yyy)")
       (create-scheduler-task scheduler "H/2 * * * * (scheduler-implementation::%foobar)")
       (create-scheduler-task scheduler "* * * * * (scheduler-implementation::%foobar)")
       (create-scheduler-task scheduler "H/3 * * * * (scheduler-implementation::%foobar)")
       (let ((thread (bt:make-thread (lambda () (start-scheduler scheduler)))))
         (sleep 180)
         (stop-scheduler scheduler)
         (bt:join-thread thread))
       (assert (= xxx 1))
       (assert (= yyy 1))
       (assert (>= foobar 3))))))


;; (list
;;  :every
;;  (:every :step 2)
;;  (:from 1 :to 4 :step 2)
;;  (1 2 3 4)
;;  ((1 2 3 4 5 6 7 9 12 23) :step 3)
 
;;  ;; :random is equivalent to (:random :every)
;;  :random
;;  ;; (:random :step 2) is equivalent to (:random :every :step 2)
;;  (:random :step 2)
;;  ;; note that step denotes set, from which we start, for instance we
;;  ;; pick one of (1 2 3 4) here and move by 4 beginning at picked
;;  ;; element. basically we start from (nth (random 4) full-set)
;;  (:random (1 2 3 4 5 7 8 10 12 43 55) :step 4)
;;  ;; likewise, we pick number from 0 to 14
;;  (:random :from 0 :to 59 :step 15))


;; (defun add-scheduler-task (&key
;;                              (minute :every)
;;                              (hour :every)
;;                              (day-of-month :every)
;;                              (month :every)
;;                              (day-of-week :every)))


;; (defun add-scheduler-task (schedule-specs function))

;; ;;; query database, plan whole day based on scheduled and not executed
;; ;;; yet tasks. Remove (or set as `retired' schedule tasks which has
;; ;;; ended.
;; (defun update-task-list ())

;; ;; Repeats: weekly
;; ;; Repeat every: x weeks
;; ;; Repeat on: [s] [w]
;; ;; Starts on: [date]
;; ;; Ends: [never], after [n] occurances, on [date]
;; ;; Summary: Weekly on Sunday
