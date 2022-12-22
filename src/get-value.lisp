;;;
;;; ATS 
;;; by Juan Pampin
;;; juan@ccrma.stanford.edu
;;;
;;; get-value.cl
;;;
;;; Some useful extra macros
;;; for getting sound data

(in-package :cl-ats)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Getting a partial (array) by parameter:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; returns amp array of <par>
(defmacro get-par-amp (sound par)
  `(aref (ats-sound-amp ,sound) ,par))

;;; returns frq array of <par>
(defmacro get-par-frq (sound par)
  `(aref (ats-sound-frq ,sound) ,par))

;;; returns time array of <par>
(defmacro get-par-time (sound par)
  `(aref (ats-sound-time ,sound) ,par))

;;; returns frq array of <par>
(defmacro get-par-energy (sound par)
  `(aref (ats-sound-energy ,sound) ,par))

;;; a more general interface
;;; returns <parameter> array of <par>
(defmacro get-par (sound par parameter)
  `(cond ((equal 'amp ,parameter)
	  (get-par-amp ,sound ,par))
	 ((equal 'frq ,parameter)
	  (get-par-frq ,sound ,par))
	 ((equal 'time ,parameter)
	  (get-par-time ,sound ,par))
	 (t (error "Unknown parameter~%"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Getting data from a partial at a particular frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; returns time value of <partial> at <frame>
(defmacro get-time (sound partial frame)
  `(aref (aref (ats-sound-time ,sound) ,partial) ,frame))

;;; returns amplitude value of <partial> at <frame>
(defmacro get-amp (sound partial frame)
  `(aref (aref (ats-sound-amp ,sound) ,partial) ,frame))

;;; returns frequency value of <partial> at <frame>
(defmacro get-frq (sound partial frame)
  `(aref (aref (ats-sound-frq ,sound) ,partial) ,frame))

;;; returns energy value of <partial> at <frame>
(defmacro get-energy (sound partial frame)
  `(aref (aref (ats-sound-energy ,sound) ,partial) ,frame))

;;; returns energy value of <band> at <frame>
(defmacro get-band-energy (sound band frame)
  `(aref (aref (ats-band-energy ,sound) ,band) ,frame))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Getting data from a partial at a fractional frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; returns time value of <partial> at <frame> (can be fractional)
(defmacro get-time-f (sound partial frame)
  `(array-interp  (aref (ats-sound-time ,sound) ,partial) ,frame))

;;; returns amplitude value of <partial> at <frame> (can be fractional)
(defmacro get-amp-f (sound partial frame)
  `(array-interp  (aref (ats-sound-amp ,sound) ,partial) ,frame))
 
;;; returns frequency value of <partial> at <frame> (can be fractional)
(defmacro get-frq-f (sound partial frame)
  `(array-interp  (aref (ats-sound-frq ,sound) ,partial) ,frame))

;;; returns energy value of <partial> at <frame> (can be fractional)
(defmacro get-energy-f (sound partial frame)
  `(array-interp  (aref (ats-sound-energy ,sound) ,partial) ,frame))

;;; returns energy value of <band> at <frame> (can be fractional)
(defmacro get-band-energy-f (sound band frame)
  `(array-interp  (aref (ats-sound-band-energy ,sound) ,band) ,frame))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A general macro for data access, <frame> can be fractional
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro get-value (sound partial frame parameter)
  `(cond ((equal ,parameter 'time)
	  (if (floatp ,frame)
	      (get-time-f ,sound ,partial ,frame)
	    (get-time ,sound ,partial ,frame)))
	 ((equal ,parameter 'amp)
	  (if (floatp ,frame)
	      (get-amp-f ,sound ,partial ,frame)
	    (get-amp ,sound ,partial ,frame)))
	 ((equal ,parameter 'frq)
	  (if (floatp ,frame)
	      (get-frq-f ,sound ,partial ,frame)
	    (get-frq-fr ,sound ,partial ,frame)))
	 ((equal ,parameter 'energy)
	  (if (floatp ,frame)
	      (get-energy-f ,sound ,partial ,frame)
	    (get-energy-fr ,sound ,partial ,frame)))
	 ((equal ,parameter 'band-energy)
	  (if (floatp ,frame)
	      (get-band-energy-f ,sound ,partial ,frame)
	    (get-band-energy-fr ,sound ,partial ,frame)))
	 (T (error "Unknown parameter ~A~%" ,parameter))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Getting data from a partial at a given time 
;;;
;;; NOTE: This is somehow tricky because in theory partials
;;; can have any kind of time structure after transformation 
;;; (see stretch-sound for example)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; linear interpolalation
(defmacro lin-interp (x x0 y0 x1 y1)
  `(+ ,y0 (* (- ,y1 ,y0) (/ (- ,x ,x0)(- ,x1 ,x0)))))

;;; non-linear interpolation
(defmacro exp-interp (x x0 y0 x1 y1)
  `(* ,y0 (expt (/ ,y1 ,y0)
	       (/ (- ,x ,x0)(- ,x1 ,x0)))))

;;; seeks the frame number at or after <time>
(defmacro seek-frame (sound partial time)
  `(loop for i from 0 below (ats-sound-frames ,sound) do
     (if (>= (get-time ,sound ,partial i) ,time)
	 (loop-finish))
     finally
     (return i)))

;;; returns amplitude value of <partial> at <time> (slow)
(defmacro get-amp-t (sound partial time)
  `(let* ((f1 (seek-frame ,sound ,partial ,time))
	  (f0 (- f1 1))
	  (x0 (get-time ,sound ,partial f0))
	  (x1 (get-time ,sound ,partial f1))
	  (y0 (get-amp ,sound ,partial f0))
	  (y1 (get-amp ,sound ,partial f1)))
     (lin-interp ,time x0 y0 x1 y1)))

;;; returns frequency value of <partial> at <time> (slow)
(defmacro get-frq-t (sound partial time)
  `(let* ((f1 (seek-frame ,sound ,partial ,time))
	  (f0 (- f1 1))
	  (x0 (get-time ,sound ,partial f0))
	  (x1 (get-time ,sound ,partial f1))
	  (y0 (get-frq ,sound ,partial f0))
	  (y1 (get-frq ,sound ,partial f1)))
     (lin-interp ,time x0 y0 x1 y1)))

;;; returns energy value of <partial> at <time> (slow)
(defmacro get-energy-t (sound partial time)
  `(let* ((f1 (seek-frame ,sound ,partial ,time))
	  (f0 (- f1 1))
	  (x0 (get-time ,sound ,partial f0))
	  (x1 (get-time ,sound ,partial f1))
	  (y0 (get-energy ,sound ,partial f0))
	  (y1 (get-energy ,sound ,partial f1)))
     (lin-interp ,time x0 y0 x1 y1)))
