;;;
;;; ATS 
;;; by Juan Pampin
;;; juan@ccrma.stanford.edu
;;;
;;; File: structure.cl
;;; ==================
;;; this files contains the structure definitions
;;; for the main ATS data types

;;; Structure: ats-sound
;;; ====================
;;; main data abstraction
;;; amp, frq, and pha contain sinusoidal
;;; modeling information as arrays of
;;; arrays of data arranged by partial
;;; par-energy and band-energy hold
;;; noise modeling information (experimental format)

(in-package :cl-ats)

(defun dfloat (val)
  (float val 1.0d0))

(defmacro def-clm-struct (name &rest fields)
  `(eval-when #-(or clozure excl) (:compile-toplevel :load-toplevel)
	      #+(or clozure excl) (compile load eval)
     (progn
       (defstruct (,name (:type vector) :named)
	 ,@(loop for fld in fields collect 
	     (if (listp fld) 
		 (if (not (symbolp (second fld)))
		     fld
		   (if (not (member (second fld) (list nil 'array 'integer 'float 'double-float 'real 'short-float 'single-float 'rational 'number 'bignum 'fixnum)))
		       (error "~A is not a type def-clm-struct can handle" (second fld))
		     (first fld)))
	       fld)))
       ,@(loop for field in fields and i from 1 collect
	   (let ((fieldname (concatenate 'string
					 (symbol-name name)
					 "-"
					 (symbol-name (if (listp field) (first field) field)))))
	   `(progn (clm::def-clm-fun (intern ,fieldname)
		     #'(lambda (var x) (clm::package-op 'clm::<aref> var (list 'aref (cadr x) ,i))))
		   (push (list (intern ,fieldname) 'clm::<setf-aref> (list ,i)) clm::setf-functions)))))))


(def-clm-struct ats-sound  
  (name "new-sound")
  ;;; global sound info.
  (sampling-rate INTEGER)
  (frame-size INTEGER) 
  (window-size INTEGER)
  (partials INTEGER)
  (frames INTEGER)
  (bands array)
  ;;; Info. deduced from analysis
  (optimized nil)
  (ampmax 0.0)
  (frqmax 0.0)
  (frq-av array) 
  (amp-av array) 
  (dur 0.0)
  ;;; Sinusoidal Data
  (time array)
  (frq array)
  (amp array)
  (pha array)
  ;;; Noise Data
  (energy array) 
  (band-energy array))

;;; Structure: ats-fft
;;; ==================
;;; abstraction used to handle all 
;;; fft data in a single variable		
(def-clm-struct ats-fft
  (size INTEGER)
  (rate 0.0)
  (fdr array)
  (fdi array))

;;; Structure: ats-peak
;;; ===================
;;; abstraction used to keep peak data used
;;; for peak detection and tracking
(def-clm-struct ats-peak
  (amp 0.0)
  (frq 0.0)
  (pha 0.0)
  (smr 0.0)
  (track INTEGER))


;;; Structure: ats-sieve
;;; ====================
;;; abstraction used for peak fitting
;;; by the sieve algorithm
(def-clm-struct ats-sieve
  (ctrfrq array)
  (limits array)
  (tracks array))

