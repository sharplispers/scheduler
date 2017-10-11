
(defpackage #:scheduler
  (:use #:cl))
(in-package #:scheduler)

(defstruct scheduler-entry
  schedule-specs
  last-occurance
  next-occurance
  trigger-function)
