;;;
;;; ATS 
;;; by Juan Pampin
;;; juan@ccrma.stanford.edu
;;;
;;; File: residual-analysis.cl
;;; ==========================
;;; This file contains the implementation
;;; of ATS's residual analysis

;;; we need to block DC out of the residual
;;; picked from CLM's prc-toolkit

(in-package :ats-cuda)

(defstruct dcb (input 0.0) (output 0.0))

(defmacro dcblock (b sample0) 
  `(let ((sample ,sample0))
     (prog1
	 (setf (dcb-output ,b) (+ sample (- (* 0.99 (dcb-output ,b)) (dcb-input ,b))))
       (setf (dcb-input ,b) sample))))

(defun residual-get-N (M min-fft-size &optional (factor 2))
  "returns the window size"
  (labels ((next-size (def-size)
		      (if (>= def-size min-fft-size) def-size
			(next-size (ppp2 (1+ def-size))))))
    (next-size (ppp2 (* factor M)))))


(defun residual-get-bands (fft-mag true-bands)
  "returns an array with the band limits for each band
limits are bin numbers in the fft"
  (let* ((len (length true-bands))
         (limits-arr (make-array len :initial-element 0)))
    (dotimes (k len)
      (setf (aref limits-arr k) (floor (nth k true-bands) fft-mag)))
    limits-arr))

      
(defun residual-compute-time-domain-energy (fft-struct)
  "computes the energy of the time domain residual in one block"
  (loop for n from 0 below (ats-fft-size fft-struct) sum
    ;;; Parseval's Theorem states:
    ;;; N-1                   N-1
    ;;; sum(|x(n)^2|) =  1/N* sum (|X(k)|^2)
    ;;; n=0                   k=0
    ;;; then we multiply the time domain energy by 1/2
    ;;; because we only compute frequency energy between 
    ;;; 0 Hz and Nyquist only (0 -> N/2)
    (abs (* (aref (ats-fft-fdr fft-struct) n)(abs (aref (ats-fft-fdr fft-struct) n))))))


(defmacro mag-squared (re im &optional (norm 1.0))
  `(* ,norm (+ (* ,re ,re) (* ,im ,im))))


(defun residual-get-band-energy (lo hi fft-struct norm)
  ;;; does 1/N * sum(re^2+im^2) within a band around <center>
  ;;; from <lo> lower bin to <hi> upper bin in <fft-struct>
  (if (< lo 0) (setf lo 0))
  (if (>= hi (floor (ats-fft-size fft-struct) 2.0))
      (setf hi (floor (ats-fft-size fft-struct) 2.0)))
  (* (/ (ats-fft-size fft-struct))
     (loop for k from lo below hi sum
       (mag-squared (aref (ats-fft-fdr fft-struct) k)
		    (aref (ats-fft-fdi fft-struct) k)
		    norm))))


(defun residual-compute-band-energy (fft-struct band-limits band-energy norm)
  "computes the energy at each sub-band and returns the values in <band-gains>"
;;; loop trough bands and evaluate energy
;;; we compute energy of one band as:
;;;     (N-1)/2
;;; 1/N * sum(|X(k)|^2)
;;;       k=0
;;; N=fft size, K=bins in band
    (loop for b from 0 below (1- (length band-limits)) do
      (setf (aref band-energy b)
	    (residual-get-band-energy
             (aref band-limits b)
             (aref band-limits (1+ b))
             fft-struct norm))))


(defun get-band-partials (lo hi sound frame)
  "returns a list of partial numbers that fall 
in frequency between lo and hi
"
  (let ((par nil))
    (loop for k from 0 below (ats-sound-partials sound) do 
      (if (<= lo (ats-aref (ats-sound-frq sound) k frame) hi)
	  (push k par)))
    (nreverse par)))

(defun get-load-band-partials (lo hi sound frame)
  "returns a list of partial numbers that fall 
in frequency between lo and hi
"
  (let ((par nil))
    (loop for k from 0 below (ats-sound-partials sound) do 
      (if (<= lo (ats-aref (ats-sound-frq sound) k frame) hi)
	  (push k par)))
    (nreverse par)))



(defun band-to-energy (sound &key (use-smr NIL)(debug NIL))
"
transfers band energy to partials
"
  (let* ((bands (if (ats-sound-bands sound) (length (ats-sound-bands sound)) *ats-critical-bands*))
	 (partials (ats-sound-partials sound))
	 (frames (ats-sound-frames sound))
	 (par-energy (make-ats-array partials frames)))
    ;;; create storage place for partial energy

    ;;; now compute par-energy frame by frame
    (loop for frame from 0 below frames do
      (let ((smr (if use-smr (smr-frame sound frame) nil)))
	(loop for b from 0 below (1- bands) do
          (progn
	    (let* ((lo-frq (nth b *ats-critical-band-edges*))
		   (hi-frq (nth (1+ b) *ats-critical-band-edges*))
		   (par (get-band-partials lo-frq hi-frq sound frame))
		   (band-energy (ats-aref (ats-sound-band-energy sound) b frame)))
	    ;;; if we found partials in this band evaluate the energy
	      (if (and (> band-energy 0.0) par)
		  (let* ((par-amp-sum (loop for p in par sum 
				                         (if smr (aref smr p)
					                     (ats-aref (ats-sound-amp sound) p frame))))
		         (n-pars (list-length par)))
		  ;;; check if we have active partials and store band-energy proportionally
		    (if (> par-amp-sum 0.0)
		        (loop for p in par do
			  (setf (ats-aref par-energy p frame) 
			        (/ (* (if smr (aref smr p)
				          (ats-aref (ats-sound-amp sound) p frame))
                                      band-energy)
				   par-amp-sum)))
		    ;;; inactive partials: split energy by partials
		        (loop 
		          for p in par 
		          with eng = (/ band-energy n-pars)
		          do
		             (setf (ats-aref par-energy p frame) eng)))
		  ;;; clear energy from band
		    (setf (ats-aref (ats-sound-band-energy sound) b frame) (dfloat 0.0)))
	          (if (and debug (> band-energy 0.0))
		      (format t "Frame: ~d Band: ~d Energy: ~a no partials~%" frame b band-energy))))))))
    (setf (ats-sound-energy sound) par-energy)))

(defun energy-to-band (sound band frame)
  "
transfers energy from partials to a band
"
  (let* ((lo-frq (nth band *ats-critical-band-edges*))
	 (hi-frq (nth (1+ band) *ats-critical-band-edges*))
	 (par (get-band-partials lo-frq hi-frq sound frame)))
    (loop for p in par sum
      (aref (array-slice (ats-sound-energy sound) p) frame))))


(defun residual-analysis (file sound &key 
			               (min-fft-size 4096)
			               (equalize nil)
			               (pad-factor 2)
			               (band-edges NIL)
			               (par-energy NIL)
			               (debug NIL)
			               (verbose NIL))
  (let* (;;; open input residual file 
;;;         (nn 0)
	 (fil (open-input* file))
         (input-data (sfile->array file))
;;; file sampling-rate
	 (file-sampling-rate (sound-srate fil))
;;; hop size
	 (hop (ats-sound-frame-size sound))
;;; for M we consider we hop by 1/hop-factor a window
;;; if this is too small we go up to the next power of 2
	 (M (ats-sound-window-size sound))
;;; we use fft-size equal to the zero-pad times M
;;; so by default M=N
	 (N (residual-get-N M min-fft-size pad-factor))
;;; fft structure
	 (fft-struct (make-ats-fft :size N
				   :rate file-sampling-rate
				   :fdr (make-double-float-array N :initial-element 0.0)
				   :fdi (make-double-float-array N :initial-element 0.0)))
;;; window norm (we use 1.0 beacause we are applying a rectangular window)
	 (norm 1.0)
;;; threshold for noise
	 (threshold (db-amp *ats-noise-threshold*))
;;; analysis frames
	 (frames (ats-sound-frames sound))
;;; magic number for fft frequencies (frquency resolution)
	 (fft-mag (dfloat (/ file-sampling-rate N)))	
;;; sub-band limits:
;;; array with lower and upper limits in bins for each sub-band
	 (band-limits (residual-get-bands fft-mag *ats-critical-band-edges*))
;;; number of bands
	 (bands (1- (length band-limits)))
;;; array to store band energy arrays
	 band-arr
;;; array for energy values
	 (band-energy (make-double-float-array bands :initial-element (double 0.0)))
;;; time domain energy 
	 (time-domain-energy 0.0)
;;; frequency domain energy 
	 (freq-domain-energy 0.0)
;;; DC blocker
	 (dc-block (make-dcb))
;;; sample counter
	 (smp 0)
;;; central point of the window
	 (M-over-2 (floor (- M 1) 2))
;;; first point in fft buffer where to write
	 (first-point (- N M-over-2))
;;; set file pointer half a window from the first sample
	 (filptr (- M-over-2)))
;;; first fill out band-arr with arrays 
    (setf band-arr (make-ats-array bands frames))
;;; Main loop
;;; tell user we start the analysis
    (format t "Analyzing residual~%")
    (loop for frame-n below frames
          with modulo = (floor frames 40)
          do
             (when (zerop (mod frame-n modulo)) (format t "."))
;;; clear fft arrays
             (clear-array (ats-fft-fdr fft-struct))
             (clear-array (ats-fft-fdi fft-struct))
;;; read samples from input and multiply by window
;;; multiply by window
             (loop for k from 0 below M do
	       (if (>= filptr 0) 
	           (setf (aref (ats-fft-fdr fft-struct)(mod (+ k first-point) N))
		         (double (dcblock dc-block (if (< filptr (length input-data)) (aref input-data filptr) (double 0.0))))))
	       (incf filptr))
;;; set sample counter
             (setf smp (- filptr M-over-2 1))
;;; take the enrgy of this input block
             (if equalize (setf time-domain-energy (residual-compute-time-domain-energy fft-struct)))
;;; take the dft 
             (fft  (ats-fft-fdr fft-struct) (ats-fft-fdi fft-struct) (ats-fft-size fft-struct) 1)
;;; now we compute energy in sub-bands 
             (residual-compute-band-energy fft-struct band-limits band-energy norm)
             (when equalize
;;; we have the energy in each sub-band into band-energy
;;; we should now try matching the energy of the spectrum with the 
;;; one of the time domain residual
	       (setf freq-domain-energy (* 2.0 (loop for k across band-energy sum k)))
	       (if debug (format t "TDE: ~s FDE: ~s~%" time-domain-energy  freq-domain-energy))
;;; scale band energy to match time domain energy
	       (loop 
	         with e-ratio = (if (> freq-domain-energy 0.0)
			            (/ time-domain-energy freq-domain-energy)
			            1.0)
	         for b from 0 below bands do
	           (setf (aref band-energy b)(* e-ratio (aref band-energy b)))))
;;; store this set of energy values
             (loop 
	       for b across band-energy 
	       for k from 0 
	       do
	          (if (< b  threshold) (setf b (dfloat 0.0)))
	          (setf (ats-aref band-arr k frame-n) b)
	          (if debug (format t "[Band: ~s Energy: ~s] " k (if (> b 0.0)(amp-db b) '-INF))))
;;; update file pointer
             (setf filptr (+ (- filptr M) hop))
;;; verbose mode
             (if verbose (format t "<Frame:~s Smp: ~s>  " frame-n smp)))
;;; store data into ats sound
    (setf (ats-sound-band-energy sound) band-arr)
;;; set band numbers
    (setf (ats-sound-bands sound) 
	  (make-array bands :initial-contents (loop for k from 0 below 25 collect k)))
    (when par-energy
      (band-to-energy sound :debug debug)
      (remove-bands sound))
    (close-input fil)))


#|

(residual-analysis "/zap/cel-tracker-res.snd" cel-tracker 
		   :verbose t :equalize t :debug t)

(residual-analysis "/zap/cbl-e4-res.snd" cbl-e4 
		   :debug t :verbose nil :equalize t :partials-energy t)

(residual-analysis "/zap/crt-cs6-res.snd" crt-cs6 :debug t :verbose t :equalize t)

(residual-analysis "/zap/cl-res.snd" cl :debug nil :verbose nil :equalize nil)

(residual-analysis "/zap/piazz-res.snd" piazz :debug Nil :verbose nil :equalize nil)

(residual-analysis "/zap/cel-res.snd" piazz :debug Nil :verbose nil :equalize nil)

|#
