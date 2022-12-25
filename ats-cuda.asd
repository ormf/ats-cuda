;;;; cl-ats.asd

(asdf:defsystem #:ats-cuda
  :description "ATS file parser for Common Lisp
  and port of Juan Pampin's ats lisp code, incudine version.

  The parser code makes extensive use of Peter Seibels Binary Data Parser
  from ch24/25 of \"Practical common Lisp\""
  :author "Orm Finnendahl"
  :license  "Public Domain"
  :version "0.0.1"
  :serial t
  :depends-on (#:ieee-floats #:cudere-clm #:svg-import-export #:incudine)
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
               (:module "src"
                :serial t
                :components
                ((:file "read-file")
                 (:file "structure")
                 (:file "c-fun")
                 (:file "do-partials")
                 (:file "utilities")
                 (:file "get-value")
                 (:file "copy-sound")
                 (:file "formants")
                 (:file "shift-sound")
                 (:file "stretch-sound")
                 (:file "trans-sound")
                 (:file "ana-fun")
                 (:file "windows")
                 (:file "residual")
                 (:file "peak-detection")
                 (:file "critical-bands")
                 (:file "fft")
                 (:file "peak-tracking")
                 (:file "tracker")
                 (:file "residual-analysis")
                 (:file "save-load-sound")
                 ))
               (:module "synth"
                :serial t
                :components
                ((:file "sin-noi-synth")
                 (:file "sin-synth")))
               (:file "defs" :depends-on ("package"))
               (:file "ats-cuda" :depends-on ("package" "defs"))))
