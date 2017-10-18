
(in-package #:asdf-user)

(defsystem #:scheduler
  :author "Daniel Kochmański <daniel@turtleware.eu>"
  :license "BSD-2-Clause"
  :description "Extensible task scheduler."
  :depends-on (#:alexandria #:split-sequence #:local-time #:optima #:optima.ppcre)
  :components ((:file "scheduler")
               (:static-file "README.md")))
