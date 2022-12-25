;;;; package.lisp

(defpackage #:ats-cuda
  (:use :cl :incudine :cudere-clm :sb-loop :ieee-floats
   :de.finnendahl.binary-data
   :de.finnendahl.binary-data.common-datatypes)
  (:shadowing-import-from :de.finnendahl.binary-data
   :*debug*)
  (:shadowing-import-from :incudine
   :play :scale-envelope :normalize-envelope))
