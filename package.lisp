;;;; package.lisp

(defpackage #:ats-cuda
  (:use :cl :incudine :cudere-clm :sb-loop :ieee-floats)
  (:shadowing-import-from :incudine
   :play :scale-envelope :normalize-envelope))
