
(eval-when (:execute :compile-toplevel :load-toplevel)
  (asdf:load-systems 'alexandria
                     'split-sequence
                     'local-time
                     'optima
                     'optima.ppcre))

(defpackage #:scheduler
  (:use)
  (:export #:start-scheduler
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
                #:start-scheduler
                #:stop-scheduler
                #:create-scheduler-task
                #:read-scheduler-task
                #:update-scheduler-task
                #:delete-scheduler-task
                #:list-scheduler-tasks))
(in-package #:scheduler-implementation)

(defstruct scheduler-entry
  schedule-specs
  last-occurance
  next-occurance
  trigger-function)

(defstruct scheduler-task
  (status :scheduled)
  date
  function)

(eval-when (:execute :compile-toplevel :load-toplevel)
  (defmacro ss (&rest args)
    `(split-sequence:split-sequence #\space ,@args))
  (defmacro db (lambda-list expression &body body)
    `(destructuring-bind ,lambda-list ,expression ,@body)))

(progn
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
        (assert (and (or (member specs '(:reboot :shutdown))
                         (alexandria:length= 5 specs))
                     (not (alexandria:emptyp command))))
        (cons specs command))))

  #+test
  (defun test-parse-cron-spec ()
    (flet ((parse (str) (parse-cron-spec str)))
      (assert (equalp (parse "@reboot (lambda () (list 1 2 3))")
                      `(:reboot . "(lambda () (list 1 2 3))")))
      (assert (equalp (parse "@shutdown (lambda () (list 1 2 3))")
                      `(:shutdown . "(lambda () (list 1 2 3))")))
      (assert (equalp (parse "@yearly command")
                      `(("0" "0" "1" "1" "*") . "command")))
      (assert (equalp (parse "@yearly command")
                      (parse "@annually command")))
      (assert (equalp (parse "@daily command")
                      (parse "@midnight command")))
      (assert (not (equalp (parse "@daily command!")
                           (parse "@midnight command"))))
      (assert (not (equalp (parse "@weekly command bar")
                           (parse "@weekly command"))))
      (assert (equalp (parse "1 0 1/2 * 1,2 command foo")
                      `(("1" "0" "1/2" "*" "1,2") . "command foo")))
      (assert (null (handler-case (parse "1 0 1/2")        (simple-error () nil))))
      (assert (null (handler-case (parse "@yearly ")       (simple-error () nil))))
      (assert (null (handler-case (parse "@foo 3 4 ")      (simple-error () nil))))
      (assert (null (handler-case (parse "@foo 3 4 1 1 2") (simple-error () nil)))))))

(progn
  (defun %parse-cron-time-1/no-step (spec range)
    (optima:ematch spec
      ("*" :every)
      ("H" (list (alexandria:random-elt range)))
      ((optima.ppcre:ppcre "(^[0-9]+)" number)
       (list (parse-integer number)))
      ;; 1,2,4,8
      ((optima.ppcre:ppcre "^[0-9]+,.*")
       (mapcar #'parse-integer (ppcre:split "," spec)))
      ;; H(1-12)
      ((optima.ppcre:ppcre "H\\(([0-9]+)-([0-9]+)\\)" from to)
       (let* ((from (parse-integer from))
              (to (parse-integer to))
              (size (1+ (- to from))))
         (alexandria:random-elt (alexandria:iota size :start from))))
      ;; 1-12
      ((optima.ppcre:ppcre "([0-9]+)-([0-9]+)" from to)
       (alexandria:iota (1+ (- to from)) :start from))))

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
                            (alexandria:iota step :start (parse-integer from))))))
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
                 (%parse-cron-time-1/step base range (parse-integer (car step))))))
        (assert (or (eql parse-result :every)
                    (every (lambda (n) (member n range)) parse-result))
                nil "PARSE-CRON-TIME-1: Each element of ~s must be a member of:~%~s."
                parse-result (cons :every range)))))

  #+test
  (defun test-parse-cron-time-1 (&aux (range (alexandria:iota 30)))
    (assert (equalp (parse-cron-time-1 "*" range) :every))
    (assert (equalp (parse-cron-time-1 "*/2" '(1 2 3 4 5 6 7)) '(1 3 5 7)))
    (assert (equalp (parse-cron-time-1 "*/3" '(1 2 3 4 5 6 7)) '(1 4 7)))
    (assert (equalp (parse-cron-time-1 "18" range) '(18)))
    (let ((result (parse-cron-time-1 "H/2" '(1 2 3 4 5 6 7))))
      (assert (or (equalp result '(1 3 5 7))
                  (equalp result '(2 4 6)))))
    (let ((result (parse-cron-time-1 "H(8-12)/2" range)))
      (assert (or (equalp result '(8 10 12)) (equalp result '(9 11)))))
    (assert (member (parse-cron-time-1 "H(8-12)" range) (alexandria:iota 5 :start 8)))
    (assert (null (handler-case (parse-cron-time-1 "H(1,2)" range) (error () nil))))
    (assert (null (handler-case (parse-cron-time-1 "1,2,4/2" '(1 2 3 4)) (error () nil))))
    (assert (null (handler-case (parse-cron-time-1 "18/2" range) (error () nil))))
    ;; we let that pass (due to being lazy) "1/" === "1"
    #+ (or) (assert (null (handler-case (parse-cron-time-1 "1/" range) (error () nil))))))

(progn
  (defun parse-cron-entry (spec &aux (spec (parse-cron-spec spec)))
    (if (member (car spec) '(:reboot :shutdown))
        (values (car spec) (cdr spec))
        (destructuring-bind ((minute hour day-of-month month day-of-week) . command)
            spec
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

  ;; for now we only check, if correct entries doesn't signal
  ;; condition, so no assertions here.
  #+test
  (defun test-parse-cron-entry ()
    (parse-cron-entry "@reboot foo")
    (parse-cron-entry "@shutdown foo")
    (parse-cron-entry "@yearly foo")
    (parse-cron-entry "@weekly foo")
    (parse-cron-entry "@daily foo")
    (parse-cron-entry "@midnight foo")
    (parse-cron-entry "@annually foo")
    (parse-cron-entry "0 0 0 0 0 foo")))


(progn
  (defun match-spec (obj spec)
    (or (eql :every spec)
        (member obj spec)))

  (defun is-it-now? (spec &optional (starting-of (local-time:now)))
    (destructuring-bind (&key minute hour day-of-month month day-of-week)
        spec
      (and
       (match-spec (local-time:timestamp-day-of-week starting-of) day-of-week)
       (match-spec (local-time:timestamp-month starting-of) month)
       (match-spec (local-time:timestamp-day starting-of) day-of-month)
       (match-spec (local-time:timestamp-hour starting-of) hour)
       (match-spec (local-time:timestamp-minute starting-of) minute))))

  (defun test-is-it-now? ()
    (assert (is-it-now? (parse-cron-entry "* * * * * foo") (local-time:now)))
    (assert (is-it-now? (parse-cron-entry "* * * 4 * foo")
                        (local-time:encode-timestamp 0 14 12 8 15 4 2017)))
    (assert (null (is-it-now? (parse-cron-entry "* * * 4 * foo")
                              (local-time:encode-timestamp 0 14 12 8 15 5 2017))))))

;;; we assume here that all `:random' entries are already picked and
;;; if `:step' present already coerced to sets.
(defun compute-next-occurance (spec &optional (starting-of (local-time:now)))
  )


(defun next-occurance (entry)
  (when (local-time:timestamp>= (local-time:now)
                                (scheduler-entry-next-occurance entry))
    (warn "Missed occurance, scheduling somewhere soon!")))

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


(defun add-scheduler-task (&key
                             (minute :every)
                             (hour :every)
                             (day-of-month :every)
                             (month :every)
                             (day-of-week :every)))


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

;; (defun scheduler-loop ()
;;   )


;; (defun schedule-task (time task)
;;   )

(defmacro ^if (test if-true if-false)
  `(let ((^it ,test))
     (if ^it
         ,if-true
         ,if-false)))
