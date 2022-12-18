(in-package :cl-user)

(defpackage :de.finnendahl.binary-data
  (:use :common-lisp :de.finnendahl.macro-utilities)
  (:export :define-binary-body-class
           :define-binary-class
           :define-tagged-binary-class
           :define-binary-type
           :read-value
           :write-value
           :*in-progress-objects*
           :parent-of-type
           :current-binary-object
           :+null+
           :*debug*))

(defpackage #:de.finnendahl.binary-data.common-datatypes
  (:use :common-lisp :de.finnendahl.binary-data)
  (:export
   :u1
   :u2
   :u3 
   :u4 
   :generic-string 
   :generic-terminated-string 
   :iso-8859-1-char 
   :iso-8859-1-string 
   :iso-8859-1-terminated-string 
   :ucs-2-char 
   :ucs-2-char-big-endian 
   :ucs-2-char-little-endian 
   :ucs-2-char-type 
   :ucs-2-string 
   :ucs-2-terminated-string 
   :unsigned-integer))
