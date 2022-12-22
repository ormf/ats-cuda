;;;
;;; ATS 
;;; by Juan Pampin
;;; juan@ccrma.stanford.edu
;;;
;;; File: residual.cl
;;; =================
;;; This file contains the implementation
;;; of ATS's residual computation

(in-package :cl-ats)

;;; Read one frame into a buffer
(defun read-frame (input-data samp-1 samp-2 in-buffer)
  "reads one input buffer"
  (clear-array in-buffer)
  (if (/= (- samp-2 samp-1) (length in-buffer))
      (error "read-frame: wrong number of samples~%"))
  (loop 
    for ptr from samp-1 below samp-2
    for i from 0 do
    (setf (aref in-buffer i)
	  (if (< ptr (length input-data))
              (aref input-data ptr)
              (double 0.0)))))

;;; Macros for phase interpolation
;;; All this comes from JOS/XJS article on PARSHL.
;;; Original phase interpolation eqns. by Qualtieri/McAulay.

(defmacro compute-M (pha-1 frq-1 pha frq buffer-size)
  `(truncate (+ (/ (+ (+ ,pha-1 (* ,frq-1 ,buffer-size) (- ,pha)) 
		      (* 0.5 ,buffer-size (- ,frq ,frq-1))) 
		   ,+two-pi+) 0.5)))

(defmacro compute-aux (pha-1 pha frq-1 buffer-size M)
  `(- (+ ,pha (* ,+two-pi+ ,M))
      (+ ,pha-1 (* ,frq-1 ,buffer-size)))) 


(defmacro compute-alpha (aux frq-1 frq buffer-size)
  `(- (* (/ 3.0 (* ,buffer-size ,buffer-size)) ,aux)
      (/ (- ,frq ,frq-1) ,buffer-size)))

(defmacro compute-beta (aux frq-1 frq buffer-size)
  `(+ (* (/ -2.0 (* ,buffer-size ,buffer-size ,buffer-size)) ,aux)
      (/ (- ,frq ,frq-1) (* ,buffer-size ,buffer-size))))

(defmacro interp-phase (pha-1 frq-1 alpha beta i)
  `(+ ,pha-1 (* ,frq-1 ,i)(* ,alpha ,i ,i)(* ,beta ,i ,i ,i)))

(defmacro wrap-phase (phase)
  `(- ,phase (* (floor ,phase +two-pi+) +two-pi+)))

;;; Table lookup sinewave oscillator
;;; we use 4096 length sine-table stored
;;; in extras.cl
;(defmacro osc-lookup (phase table size)
;  `(let* ((scale (/ ,two-pi (float (1- ,size))))
;	  (new-phase (mod ,phase ,two-pi)))
;     (aref ,table (ceiling new-phase scale))))
;;; try shadowing this macro with a direct sin lookup
(defmacro osc-lookup (phase table size)
  (declare (ignore table size))
  `(cos ,phase))


#|
(defmacro synth-buffer (a1 a2 f1 f2 p1 p2 buffer frame-samps)
  "Synthesizes a buffer using phase interpolation"
  `(let* ((a-inc (/ (- ,a2 ,a1) ,frame-samps))
	 (M (compute-M ,p1 ,f1 ,p2 ,f2 ,frame-samps))
	 (aux (compute-aux ,p1 ,p2 ,f1 ,frame-samps M))
	 (alpha (compute-alpha aux ,f1 ,f2 ,frame-samps))
	 (beta (compute-beta aux ,f1 f2 ,frame-samps)))
     (loop
      for k from 0 below ,frame-samps
      for amp from ,a1 by a-inc do
      (incf (aref ,buffer k)
	    (* amp (cos (interp-phase ,p1 ,f1 alpha beta k)))))))
|#

(defmacro synth-buffer (a1 a2 f1 f2 p1 p2 buffer frame-samps)
  "Synthesizes a buffer using phase interpolation"
  `(let* ((a-inc (/ (- ,a2 ,a1) ,frame-samps))
	  (M (compute-M ,p1 ,f1 ,p2 ,f2 ,frame-samps))
	  (aux (compute-aux ,p1 ,p2 ,f1 ,frame-samps M))
	  (alpha (compute-alpha aux ,f1 ,f2 ,frame-samps))
	  (beta (compute-beta aux ,f1 f2 ,frame-samps))
          (amp a1))
     (dotimes (k ,frame-samps)
       (incf (aref ,buffer k)
	     (* amp (cos (interp-phase ,p1 ,f1 alpha beta k))))
       (incf amp a-inc))))

#|

(defun compute-residual (fil output-file sound win-samps file-sampling-rate 
			     &optional (verbose nil)(equalize nil)
			     (first-frame 0)(last-frame nil)
			     (first-par 0) (last-par nil))
  "Computes the difference between the synthesis and the original sound. 
The function is passed an I/O struct (fil) pointing to the analyzed sound
the <win-samps> array contains the sample numbers in the input file corresponding to each frame"
  (let* ((out-fil (open-output output-file
                               :chans 2
                               :srate file-sampling-rate))
	 (frames (if last-frame last-frame (ats-sound-frames sound)))
	 (partials (if last-par last-par (ats-sound-partials sound)))
	 (frm-samps (- (aref win-samps 1)(aref win-samps 0)))
	 (in-buff (make-array frm-samps :initial-element 0.0))
	 (synth-buff (make-array frm-samps :initial-element 0.0))
	 (mag (/ +two-pi+ file-sampling-rate)))
    ;;; now we go over the whole sound computing the synthesis frame by frame
    ;;; we store the residual on channel A and the synthesis on channel B
;;;    (if (/= frames (length win-samps))
;;;	(error "compute-residual: sound and analysis have different number of frames~%"))
    (if verbose (format t "Computing residual...~%"))
    (loop for frm from (+ first-frame 1) below frames do
      (let* ((frm-1 (1- frm))
	     (frm-2 frm)
	     (samp-1 (aref win-samps frm-1))
	     (samp-2 (aref win-samps frm-2))
	     (max-in 0.0)
	     (max-synth 0.0)
	     (gain 1.0))
	(if verbose (format t "<Frame: ~d s1: ~d s2: ~d>" frm-1 samp-1 samp-2))
	  ;;; read samples from input
	(read-frame fil samp-1 samp-2 in-buff)
	  ;;; now we have to compute one synthesis frame
	  ;;; we clear the array first
	(clear-array synth-buff)
	(loop for par from first-par below partials do
	  (let ((a1 (aref (aref (ats-sound-amp sound) par) frm-1))
		(a2 (aref (aref (ats-sound-amp sound) par) frm-2))
		;;; have to convert the frequency into radians per sample!!!
		(f1 (* (aref (aref (ats-sound-frq sound) par) frm-1) mag))
		(f2 (* (aref (aref (ats-sound-frq sound) par) frm-2) mag))
		(p1 (aref (aref (ats-sound-pha sound) par) frm-1))
		(p2 (aref (aref (ats-sound-pha sound) par) frm-2)))
	    (when (not (and (<= a1 0.0)(<= a1 0.0))) ;;; probably a bug, second term should test for a2, not a1!
	     ;;; check form amp 0 in frame 1
	      (if (<= a1 0.0)
		  (let ((delta (- p2 (* f2 frm-samps))))
		    (setf f1 f2)
		    (setf p1 (wrap-phase delta)))
	     ;;; check form amp 0 in frame 2
		(if (<= a2 0.0)
		    (let ((delta (+ p1 (* f1 frm-samps))))
		      (setf f2 f1)
		      (setf p2 (wrap-phase delta)))))
	      ;;; synthesize partial and store in buffer
	      (synth-buffer a1 a2 f1 f2 p1 p2 synth-buff frm-samps))))
      (when equalize
	  ;;; synthesis buffer is filled up, see what's its maximum amplitude
	(setf max-synth (max_array synth-buff frm-samps))
	  ;;; see what the maximum of the input is
	(setf max-in (max_array in-buff frm-samps))
tr	  ;;; compute gain scaling factor
	(setf gain (if (> max-synth 0.0) 
		       (/ max-in max-synth) 1.0))
	  ;;; scale values in the synthesis array if needed
	(if (/= gain 1.0)
	    (un_norm_array synth-buff frm-samps gain)))
	  ;;; now we write the residual and synthesis to the output
      (loop for i from 0 below frm-samps do
	(outa out-smp (- (aref in-buff i)(aref synth-buff i)) out-fil)
	(outb out-smp (aref synth-buff i) out-fil)
	(incf out-smp))))
    (close-output out-fil)))

|#

(defun compute-residual (input-data output-file sound win-samps file-sampling-rate 
			 &key (verbose nil)(equalize nil)
			   (first-frame 0)(last-frame nil)
			   (first-par 0) (last-par nil)
                           (srate 44100))
  "Computes the difference between the synthesis and the original sound. 
The function is passed an I/O struct (fil) pointing to the analyzed sound
the <win-samps> array contains the sample numbers in the input file corresponding to each frame"
  (let* ((frames (if last-frame last-frame (ats-sound-frames sound)))
	 (partials (if last-par last-par (ats-sound-partials sound)))
	 (frm-samps (- (aref win-samps 1)(aref win-samps 0)))
	 (in-buff (make-array frm-samps :element-type 'double-float :initial-element (double 0.0)))
	 (synth-buff (make-array frm-samps :initial-element (double 0.0)))
	 (mag (/ +two-pi+ file-sampling-rate))
         (out-framples (* frm-samps frames))
         (out-a (make-array out-framples :element-type 'double-float :initial-element (double 0.0)))
         (out-b (make-array out-framples :element-type 'double-float :initial-element (double 0.0)))
         (out-smp 0))
;;; now we go over the whole sound computing the synthesis frame by frame
;;; we store the residual on channel A and the synthesis on channel B
;;;    (if (/= frames (length win-samps))
;;;	(error "compute-residual: sound and analysis have different number of frames~%"))
    (if verbose (format t "Computing residual...~%"))
;;;    (sfile->array )
    (loop for frm from (+ first-frame 1) below frames do
      (let* ((frm-1 (1- frm))
	     (frm-2 frm)
	     (samp-1 (aref win-samps frm-1))
	     (samp-2 (aref win-samps frm-2))
	     (max-in 0.0)
	     (max-synth 0.0)
	     (gain 1.0))
	(if verbose (format t "<Frame: ~d s1: ~d s2: ~d>" frm-1 samp-1 samp-2))
;;; read samples from input
	(read-frame input-data samp-1 samp-2 in-buff)
;;; now we have to compute one synthesis frame
;;; we clear the array first
	(clear-array synth-buff)
	(loop for par from first-par below partials do
	  (let ((a1 (aref (aref (ats-sound-amp sound) par) frm-1))
		(a2 (aref (aref (ats-sound-amp sound) par) frm-2))
;;; have to convert the frequency into radians per sample!!!
		(f1 (* (aref (aref (ats-sound-frq sound) par) frm-1) mag))
		(f2 (* (aref (aref (ats-sound-frq sound) par) frm-2) mag))
		(p1 (aref (aref (ats-sound-pha sound) par) frm-1))
		(p2 (aref (aref (ats-sound-pha sound) par) frm-2)))
	    (when (or (> a1 0.0)(> a2 0.0))
;;; check form amp 0 in frame 1
	      (cond ((<= a1 0.0)
		     (let ((delta (- p2 (* f2 frm-samps))))
		       (setf f1 f2)
		       (setf p1 (wrap-phase delta))))
;;; check form amp 0 in frame 2
		    ((<= a2 0.0)
		     (let ((delta (+ p1 (* f1 frm-samps))))
		       (setf f2 f1)
		       (setf p2 (wrap-phase delta)))))
;;; synthesize partial and store in buffer
	      (synth-buffer a1 a2 f1 f2 p1 p2 synth-buff frm-samps))))
        (when equalize
;;; synthesis buffer is filled up, see what's its maximum amplitude
	  (setf max-synth (max_array synth-buff frm-samps))
;;; see what the maximum of the input is
	  (setf max-in (max_array in-buff frm-samps))
;;; compute gain scaling factor
	  (setf gain (if (> max-synth 0.0) 
		         (/ max-in max-synth) 1.0))
;;; scale values in the synthesis array if needed
	  (if (/= gain 1.0)
	      (un_norm_array synth-buff frm-samps gain)))
;;; now we write the residual and synthesis to the output
;;;	(if verbose (format t "~&~a" frm-samps))
        (loop for i from 0 below frm-samps do
	  (setf (aref out-a out-smp) (double (- (aref in-buff i) (aref synth-buff i))))
	  (setf (aref out-b out-smp) (double (aref synth-buff i)))
	  (incf out-smp))))
    (array->sfile output-file (list out-a out-b) :rate srate)))
