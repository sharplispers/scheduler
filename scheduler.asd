(in-package #:asdf-user)

(defsystem "scheduler"
  :author "Daniel Kochmański <daniel@turtleware.eu>"
  :license "BSD-2-Clause"
  :description "Extensible task scheduler."
  :depends-on ("alexandria"
               "split-sequence"
               "local-time"
               "optima"
               "optima.ppcre")
  :components ((:file "scheduler")
               (:static-file "README.md")
               (:static-file "LICENSE"))
  :in-order-to ((test-op (test-op "scheduler/tests"))))

(defsystem "scheduler/tests"
  :depends-on ("scheduler" "fiveam" "local-time")
  :components ((:file "scheduler-tests"))
  :perform (test-op (operation component)
            (uiop:symbol-call '#:scheduler-tests '#:run-tests)))
