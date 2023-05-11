;;;; package.lisp

(defpackage #:ats-cuda
  (:use :cl :incudine :cudere-clm :sb-loop :ieee-floats :alexandria
;;;        :de.finnendahl.binary-data :de.finnendahl.binary-data.common-datatypes
   )
  (:shadowing-import-from :incudine
   :play :scale-envelope :normalize-envelope)
  (:shadowing-import-from :incudine.util
   :sample)
  (:shadowing-import-from :cudere-clm
   :*debug*)
  (:export
   #:browser-play #:ats-load))
