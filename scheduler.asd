;;; Copyright (c) 2017, Emergent Network Defense <http://endsecurity.com/>

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
               "optima.ppcre")
  :components ((:file "scheduler")
               (:static-file "README.md")
               (:static-file "LICENSE")))
