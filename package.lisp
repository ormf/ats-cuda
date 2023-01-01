;;;; package.lisp

(defpackage #:ats-cuda
  (:use :cl :incudine :cudere-clm :sb-loop :ieee-floats :alexandria)
  (:shadowing-import-from :incudine
   :play :scale-envelope :normalize-envelope))
