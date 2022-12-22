;;;; package.lisp

(defpackage #:cl-ats
  (:use :cl :clm :sb-loop :ieee-floats
   :de.finnendahl.binary-data
   :de.finnendahl.binary-data.common-datatypes)
  (:shadowing-import-from :de.finnendahl.binary-data
   :*debug*))
