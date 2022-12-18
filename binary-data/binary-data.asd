(defpackage :de.finnendahl.binary-data-system (:use :asdf :cl))
(in-package :de.finnendahl.binary-data-system)

(asdf:defsystem #:binary-data
  :name "binary-data"
  :author "Peter Seibel <peter@gigamonkeys.com>"
  :version "1.0"
  :maintainer "Peter Seibel <peter@gigamonkeys.com>"
  :licence "BSD"
  :description "Parser for binary data files. "
  :long-description ""
  :components
  ((:file "packages")
   (:file "binary-data" :depends-on ("packages"))
   (:file "common-datatypes" :depends-on ("packages" "binary-data")))
  :depends-on (:alexandria :macro-utilities))

        
