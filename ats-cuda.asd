;;;; cl-ats.asd

(asdf:defsystem #:ats-cuda
  :description "ATS file parser for Common Lisp
  and port of Juan Pampin's ats lisp code, incudine version."
  :author "Orm Finnendahl"
  :license  "Public Domain"
  :version "0.0.1"
  :serial t
  :depends-on (#:ieee-floats #:alexandria #:cudere-clm
                             #:svg-import-export
                             #:incudine
;;;                             #:clack #:websocket-driver
                             #:clog-dsp-widgets
)
  :components (
               ;; (:module "macro-utilities"
               ;;  :serial t
               ;;  :components
               ;;  ((:file "packages")
               ;;   (:file "macro-utilities" :depends-on ("packages"))))
               ;; (:module "binary-data"
               ;;  :serial t
               ;;  :components
               ;;  ((:file "packages")
               ;;   (:file "binary-data" :depends-on ("packages"))
               ;;   (:file "common-datatypes" :depends-on ("packages" "binary-data"))))
               (:file "package")
;;;               (:file "defs" :depends-on ("package"))
               (:module "src"
                :serial t
                :components
                (
;;;                 (:file "read-file")
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
                 (:file "svg-export")
                 (:file "websocket-server")
;;;                 (:file "midi-handler")
                 (:file "browser-gui")
                 ))
               (:file "ats-cuda" :depends-on ("package"))
               (:module "synth"
                :serial t
                :components
                ((:file "array-ugens")
                 (:file "synth")
                 (:file "frame-synth")))))
#|
(asdf:defsystem #:ats-cuda-display
  :description "ATS file parser for Common Lisp
  and port of Juan Pampin's ats lisp code, incudine version."
  :author "Orm Finnendahl"
  :license  "Public Domain"
  :version "0.0.1"
  :serial t
  :depends-on (#:ats-cuda)
  :components ())
|#
