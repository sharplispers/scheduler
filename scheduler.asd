(in-package #:asdf-user)

(defsystem "scheduler"
  :author "Daniel Kochmański <daniel@turtleware.eu>"
  :license "BSD-2-Clause"
  :description "Extensible task scheduler."
  :depends-on ("alexandria"
               "bordeaux-threads"
               "split-sequence"
               "local-time"
               "optima"
               "optima.ppcre"
               "sqlite"
               "iterate")
  :components ((:file "scheduler")
               (:file "sqlite")
               (:static-file "README.md")
               (:static-file "LICENSE")))
