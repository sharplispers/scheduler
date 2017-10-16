
(eval-when (:execute :compile-toplevel :load-toplevel)
  (asdf:load-systems 'alexandria 'split-sequence 'local-time))

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

(defun parse-cron-spec (line)
  (flet ((split-at ()
           (multiple-value-bind (spec sp) (ss line :count 1)
             (values (alexandria:eswitch ((car spec) :test 'equal)
                       ("@reboot"   :reboot)
                       ("@shutdown" :shutdown)
                       ("@yearly"   '#.(ss "0 0 1 1 *"))
                       ("@annually" '#.(ss "0 0 1 1 *"))
                       ("@monthly"  '#.(ss "0 0 1 * *"))
                       ("@weekly"   '#.(ss "0 0 * * 0"))
                       ("@daily"    '#.(ss "0 0 * * *"))
                       ("@midnight" '#.(ss "0 0 * * *"))
                       ("@hourly"   '#.(ss "0 * * * *")))
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

(defun parse-cron-time (spec range)
  (flet ((every-nth (list step)
           (if (= 1 step)
               list
               (loop for elt in list by #'(lambda (l) (nthcdr step l))
                  collect elt)))
         (every-nth* (from to step)
           (let ((set-size (1+ (floor (/ (- to from) step)))))
             (alexandria:iota set-size :start from :step step)))
         (fix-step (step)
           (if (null step)
               1
               (parse-integer (car step)))))
   (db (base . step) (ppcre:split "/" spec)
     (setf step (fix-step step))
     (optima:match base
       ("*" (every-nth range step))
       ("H" (alexandria:random-elt range))
       ((optima.ppcre:ppcre ".*,.*")
        (mapcar #'parse-integer (if (null step)
                                    #1=(ppcre:split "," base)
                                    (every-nth #1# step))))
       ((optima.ppcre:ppcre "H\\((.*)-(.*)\\)" from to)
        (setf to (parse-integer to)
              from (min to
                        (alexandria:random-elt
                         (alexandria:iota step :start (parse-integer from)))))
        (every-nth* from to step))
       ((optima.ppcre:ppcre "(.*)-(.*)" from to)
        (every-nth* (parse-integer from)
                    (parse-integer to)
                    step))))))


(defun match-spec (obj spec)
  (or (eql :every spec)
      (member obj spec)))

;;; we assume here that all `:random' entries are already picked and
;;; if `:step' present already coerced to sets.
(defun compute-next-occurance (spec &optional (starting-of (local-time:now)))
  )

(defun is-it-now? (spec &optional (starting-of (local-time:now)))
  (destructuring-bind (&key minute hour day-of-month month day-of-week)
      spec
    (and
     (match-spec (local-time:timestamp-day-of-week starting-of) day-of-week)
     (match-spec (local-time:timestamp-month starting-of) month)
     (match-spec (local-time:timestamp-day starting-of) day-of-month)
     (match-spec (local-time:timestamp-hour starting-of) hour)
     (match-spec (local-time:timestamp-minute starting-of) minute))))

(defun next-occurance (entry)
  (when (local-time:timestamp>= (local-time:now)
                                (scheduler-entry-next-occurance entry))
    (log:warn "Missed occurance, scheduling somewhere soon!")))

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


#+5am
(5am:test parse-cron-spec
  (flet ((parse (str) (parse-cron-spec str)))
    (5am:is (equalp `(:reboot . "(lambda () (list 1 2 3))")
                    (parse "@reboot (lambda () (list 1 2 3))")))
    (5am:is (equalp `(:shutdown . "(lambda () (list 1 2 3))")
                    (parse "@shutdown (lambda () (list 1 2 3))")))
    (5am:is (equalp `(("0" "0" "1" "1" "*") . "command")
                    (parse "@yearly command")))
    (5am:is (equalp (parse "@yearly command")
                    (parse "@annually command")))
    (5am:is (equalp (parse "@daily command")
                    (parse "@midnight command")))
    (5am:is (not (equalp (parse "@daily command!")
                         (parse "@midnight command"))))
    (5am:is (not (equalp (parse "@weekly command bar")
                         (parse "@weekly command"))))
    (5am:is (equalp `(("1" "0" "1/2" "*" "1,2") . "command foo")
                    (parse "1 0 1/2 * 1,2 command foo")))
    (5am:signals simple-error (parse "1 0 1/2"))
    (5am:signals simple-error (parse "@yearly "))
    (5am:signals simple-error (parse "@foo 3 4"))))
