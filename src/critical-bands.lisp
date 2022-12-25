;;;
;;; ATS 
;;; by Juan Pampin
;;; juan@ccrma.stanford.edu
;;;
;;; File: critical-bands.cl
;;; =======================
;;; This file contains the implementation
;;; of a masking curve evaluation
;;; algorithm using a critical band based model

(in-package :ats-cuda)

(defparameter *ats-critical-band-edges* 
  '(0.0 100.0 200.0 300.0 400.0 510.0 630.0 770.0 920.0 1080.0 1270.0 
	1480.0 1720.0 2000.0 2320.0 2700.0 3150.0 3700.0 4400.0 
	5300.0 6400.0 7700.0 9500.0 12000.0 15500.0 20000.0))

(defun frq-to-bark (frq)
  "
frq-to-bark <frq>
converts <frq> into bark scale
"
  (cond ((<= frq 400.0)(* 0.01 frq))
	((>= frq 20000.0) NIL)
	( t (let* ((band (find-band frq))
		   (lo-frq (elt *ats-critical-band-edges* band))
		   (hi-frq (elt *ats-critical-band-edges* (1+ band))))
	      (+ 1 band (abs (/ (log (/ frq lo-frq) 10)
				(log (/ lo-frq hi-frq) 10))))))))


(defun find-band (frq &optional (l *ats-critical-band-edges*)(b 0))
"
find-band <frq>
finds the critical band for <frq>
"
 (if (or (not l) (> (first l) frq))
     (1- b)
   (find-band  frq (rest l) (1+ b))))


(defmacro band-edges (band)
  `(list  
    (nth ,band *ats-critical-band-edges*)
    (nth (1+ ,band) *ats-critical-band-edges*)))

(defmacro band-center (band)
  `(let ((edges (band-edges ,band)))
     (* (apply #'+ edges) 0.5)))

(defmacro band-partials (band sound frame)
  `(let ((edges (band-edges ,band)))
     (get-band-partials (first edges) (second edges) ,sound ,frame)))


;;; computes masker slope towards high frequencies
;;; depending on the level of the masker
(defmacro compute-slope-r (masker-amp-db)    
  `(+ -27.0 (* (max (- ,masker-amp-db 40.0) 0) 0.37)))
    
(defun circular (l &optional ll)   
  "
circular permutations of list <l>
"
  (cond ((equal (list-length l) 1) (nreverse ll))
	((null ll) (setq ll (cons l ll)) (circular l ll))
	(t (setq ll (cons (append (rest (first ll)) (list (first l))) ll))
	   (circular (rest l) ll))))


(defun clear-mask (peaks)
  "
sets mask values of all peaks in list <peaks> to 0.0
"
  (dolist (i peaks)
    (setf (ats-peak-smr i) 0.0)))

(defun evaluate-smr (peaks &key (slope-l -27.0)(delta-dB -50)(debug nil))
  "
evaluates masking values (SMR) for peaks in list <peaks>
[slope-r] and [slope-l] are the slopes of the mask
in dBs/bark, <delta-db> is the dB treshold for
the masking curves (must be <= 0dB) 
"
  (clear-mask peaks)
  (if (= (length peaks) 1)
      (setf (ats-peak-smr (first peaks)) (amp-db-spl (ats-peak-amp (first peaks))))
    (dolist (p (circular peaks))
      (let* ((maskee (first p))
	     (frq-maskee (frq-to-bark (ats-peak-frq maskee)))
	     (amp-maskee (amp-db-spl (ats-peak-amp maskee))))
	(if debug (format t "frq-maskee: ~f amp-maskee: ~f~%" frq-maskee amp-maskee))
	(dolist (pp (rest p))
	  (let* ((frq-masker (frq-to-bark (ats-peak-frq pp)))
		 (amp-masker (amp-db-spl (ats-peak-amp pp)))
		 (slope-r (compute-slope-r amp-masker))
		 (mask-term 0))
	    (if debug (format t "frq-masker: ~f amp-masker: ~f~%" frq-masker amp-masker))
	    (if debug (format t "slope-r: ~f~%" slope-r))
	    (setf mask-term (if (< frq-masker frq-maskee)
				(+ (+ amp-masker delta-dB) (* (- frq-maskee frq-masker) slope-r))
			      (+ (+ amp-masker delta-dB) (* (- frq-masker frq-maskee) slope-l))))
	    (if (> mask-term (ats-peak-smr maskee))
		(setf (ats-peak-smr maskee) mask-term))))
	(if debug (format t "Maskee SMR: ~f~%" (ats-peak-smr maskee)))
	;;; let's keep the SMR in dB-spl 
	;;; (setf (ats-peak-smr maskee) (db-amp-spl (- amp-maskee (ats-peak-smr maskee))))
	(setf (ats-peak-smr maskee) (- amp-maskee (ats-peak-smr maskee)))))))


(defmacro smr-frame (sound frame &key (slope-l -27.0)(delta-dB -50)(debug nil))
  `(let* ((partials (ats-sound-partials ,sound))
	  (peaks (loop for i from 0 below partials collect
		   (make-ats-peak :amp (aref (aref (ats-sound-amp ,sound) i) ,frame)
				  :frq (aref (aref (ats-sound-frq ,sound) i) ,frame)))))
     (evaluate-smr peaks :slope-l ,slope-l :delta-dB ,delta-dB :debug ,debug)
     (make-double-float-array partials 
			      :initial-contents (loop for p in peaks collect
						  (if (> (ats-peak-smr p) 0.0) (ats-peak-smr p) (dfloat 0.0))))))


(defmacro smr-average (sound &key (first-frame 0)(last-frame NIL)(slope-l -27.0)(delta-dB -50)(debug nil))
  `(let* ((last-frame (if ,last-frame ,last-frame (ats-sound-frames ,sound)))
	  (frames (- last-frame ,first-frame))
	  (partials (ats-sound-partials ,sound))
	  (smr-frames (make-array frames 
				  :initial-contents (loop for f from ,first-frame below last-frame collect
						      (smr-frame ,sound f :slope-l -27.0 :delta-dB -50 :debug nil)))))
     (make-double-float-array partials 
         :initial-contents (loop for p from 0 below partials collect
			     (/ (loop for n from 0 below frames sum (aref (aref smr-frames n) p)) (dfloat frames))))))
	  
	  
