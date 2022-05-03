
(defpackage #:scheduler-tests
  (:use #:cl #:5am)
  (:import-from #:scheduler-implementation
                #:parse-cron-spec
                #:parse-cron-time-1
                #:parse-cron-entry
                #:compute-next-occurance)
  (:export #:run-tests))
(in-package #:scheduler-tests)

(def-suite* scheduler
  :description "A test suite for the system 'scheduler'.")

(defun run-tests ()
  (5am:run! 'scheduler))

(def-test test-parse-cron-spec ()
  (macrolet ((is-spec (result spec)
               `(is (equalp ,result (parse-cron-spec ,spec)))))
    ;; usual specs
    (is-spec `(:reboot . "(lambda () (list 1 2 3))")      "@reboot (lambda () (list 1 2 3))")
    (is-spec `(:shutdown . "(lambda () (list 1 2 3))")    "@shutdown (lambda () (list 1 2 3))")
    (is-spec `(("H" "H" "H" "H" "*") . "command")         "@yearly command")
    (is-spec `(("1" "0" "1/2" "*" "1,2") . "command foo") "1 0 1/2 * 1,2 command foo"))
  ;; synonyms
  (is (equalp (parse-cron-spec "@yearly command") (parse-cron-spec "@annually command")))
  (is (equalp (parse-cron-spec "@daily command")  (parse-cron-spec "@midnight command")))
  ;; ensure that the full line reminder is parsed
  (is (not (equalp (parse-cron-spec "@daily command!") (parse-cron-spec "@midnight command"))))
  (is (not (equalp (parse-cron-spec "@weekly command bar") (parse-cron-spec "@weekly command"))))
  ;; errors
  (signals error (parse-cron-spec "1 0 1/2"))
  (signals error (parse-cron-spec "@foo 3 4 "))
  (signals error (parse-cron-spec "@foo 3 4 1 1 2"))
  #+ (or) ;; let it slide as nil.
  (signals (parse-cron-spec "@yearly ")))

(def-test test-parse-cron-time-1 ()
  (let ((range (alexandria:iota 30)))
    (is (equalp :every               (parse-cron-time-1 "*" range)))
    (is (equalp '(1 3 5 7)          (parse-cron-time-1 "*/2" '(1 2 3 4 5 6 7))))
    (is (equalp '(1 4 7)            (parse-cron-time-1 "*/3" '(1 2 3 4 5 6 7))))
    (is (equalp '(18)               (parse-cron-time-1 "18" range)))
    (is (equalp '(1 2 3 4 7 8 9 12) (parse-cron-time-1 "1-4,7-9,12" range)))
    (is (equalp '(1 4 5 6 7)        (parse-cron-time-1 "1,4-7" range)))
    (is (equalp '(0)                (parse-cron-time-1 "0" (alexandria:iota 7))))
    (is (equalp '(2 3 4 5 6)        (parse-cron-time-1 "2-6" '#.(alexandria:iota 7))))
    ;; Randomized
    (is (member (parse-cron-time-1 "H/2" '(1 2 3 4 5 6 7)) '((1 3 5 7) (2 4 6)) :test #'equalp))
    (is (member (parse-cron-time-1 "H(8-12)/2" range) '((8 10 12) (9 11)) :test #'equalp))
    (is (member (first (parse-cron-time-1 "H(8-12)" range)) (alexandria:iota 5 :start 8)))
    ;; errors
    (signals error (parse-cron-time-1 "H(1,2)" range))
    (signals error (parse-cron-time-1 "1,2,4/2" '(1 2 3 4)))
    (signals error (parse-cron-time-1 "18/2" range))
    ;; we let that pass (due to being lazy) "1/" === "1"
    #+ (or) (signals error (parse-cron-time-1 "1/" range))))

(def-test test-parse-cron-entry ()
  (finishes (parse-cron-entry "@reboot foo"))
  (finishes (parse-cron-entry "@shutdown foo"))
  (finishes (parse-cron-entry "@yearly foo"))
  (finishes (parse-cron-entry "@weekly foo"))
  (finishes (parse-cron-entry "@daily foo"))
  (finishes (parse-cron-entry "@midnight foo"))
  (finishes (parse-cron-entry "@annually foo"))
  (signals error (parse-cron-entry "0 0 0 0 0 foo"))
  (is (equalp '(0) (getf (parse-cron-entry "* * * * 0 foo") :day-of-week)))
  (multiple-value-bind (timespec command) (parse-cron-entry "1 1 2 2 3 foo")
    (multiple-value-bind (tsp cmd) (parse-cron-entry "1 1 2 2 3 foo")
      (is (and (equalp timespec tsp)
               (equalp command cmd))))
    (multiple-value-bind (tsp cmd) (parse-cron-entry "1 1 2 2 3 bar")
      (is (and (equalp timespec tsp)
               (not (equalp command cmd)))))))

(def-test test-compute-next-occurance ()
  (flet ((et (&rest args) (apply #'local-time:encode-timestamp 0 0 args))
         (st (time) (local-time:adjust-timestamp time (set :sec 0))))
    (is (local-time:timestamp= (st (compute-next-occurance (parse-cron-entry "0 0 1 1 0 foo")))
                               (et 0 0 1 1 2023)))
    (is (local-time:timestamp= (st (compute-next-occurance
                                    (parse-cron-entry "0 0 1 1 * foo") (et 1 0 1 1 2017)))
                               (et 0 0 1 1 2018)))
    (is (local-time:timestamp<= (et 0 0 1 1 2018)
                                (compute-next-occurance
                                 (parse-cron-entry "H H 1 1 * foo") (et 0 0 2 1 2017))
                                (et 59 23 1 1 2018)))
    (signals error (compute-next-occurance (parse-cron-entry "0 0 0 0 0 foo")))))

#+ (or) ;; this is a run-time test
(let (foobar xxx yyy)
  (defun %foobar () (incf foobar))
  (defun %xxx () (incf xxx))
  (defun %yyy () (incf yyy))
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
      (assert (= xxx yyy 2))))
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
      (assert (>= foobar 3))))

  (test-in-memory-scheduler-1)
  (test-in-memory-scheduler-2))
