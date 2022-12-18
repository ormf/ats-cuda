;;;; cl-ats.asd

(asdf:defsystem #:cl-ats
  :description "ATS file parser for Common Lisp. 

  The code makes extensive use of Peter Seibels Binary Data Parser
  from ch24/25 of \"Practical common Lisp\""
  :author "Orm Finnendahl"
  :license  "Public Domain"
  :version "0.0.1"
  :serial t
  :depends-on (#:ieee-floats)
  :components ((:module "macro-utilities"
                :serial t
                :components
                ((:file "packages")
                 (:file "macro-utilities" :depends-on ("packages"))))
               (:module "binary-data"
                :serial t
                :components
                ((:file "packages")
                 (:file "binary-data" :depends-on ("packages"))
                 (:file "common-datatypes" :depends-on ("packages" "binary-data"))))
               (:file "package")
               (:file "defs" :depends-on ("package"))
               (:file "cl-ats" :depends-on ("package" "defs"))))
