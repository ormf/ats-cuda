;;;
;;; ATS 
;;; by Juan Pampin
;;; juan@ccrma.stanford.edu
;;;
;;; File: peak-detection.cl
;;; =======================
;;; This file contains the implementation
;;; of the ATS's peak detection algorithm

(in-package :cl-ats)

(defun peak-detection (ats-fft &key (lowest-bin nil) (highest-bin nil) (lowest-magnitude 0.0)(norm 1.0))
  "
peak-detection <ats-fft> &optional [highest-bin] [lowest-magnitude 0.0]
given an <ats-fft> this function returns a list of found peaks,
the optional <highest-bin> to search for data and the optional
<lowest-magnitude> treshold.
"
  (let* (;;; empty list of peaks 
	 (peaks nil)
	 ;;; last bin to search
	 (N (if highest-bin highest-bin 
	      (ats-fft-size ats-fft)))
	 ;;; first bin to start search
	 (first-bin (if lowest-bin (max lowest-bin 2) 2))
	 ;;; frequency scaling
	 (fft-mag (float (/ (ats-fft-rate ats-fft)(ats-fft-size ats-fft))))
	 ;;; array of magnitudes
	 (fftmags (make-array N :initial-element 0.0))	
	 ;;; array of phases
	 (fftphase (make-array N :initial-element 0.0 ))
         ;;; peak bins
	 (right-bin 0.0) ;;; first point
	 (left-bin 0.0) ;;; third point
	 (central-bin 0.0) ;;; central point
	 ;;; temp vars for data
	 (frq 0.0)
	 (pha 0.0))
    ;;; convert spectrum to polar coordinates
    (to-polar ats-fft fftmags fftphase N norm)
    (setf central-bin (aref fftmags (- first-bin 2)))
    (setf right-bin (aref fftmags (- first-bin 1)))
    ;;; peak detection:
    ;;; move by one bin and analyze by groups of 3		
    (loop for k from first-bin below N do
      (let ((ats-peak (make-ats-peak)))
	(setf left-bin central-bin)
	(setf central-bin right-bin)
	(setf right-bin (aref fftmags k))
	(if (and (> central-bin lowest-magnitude)
		 (> central-bin right-bin)
		 (> central-bin left-bin))
	    (multiple-value-bind (offset amp)
		(parabolic-interp left-bin central-bin right-bin)
	      (setf frq (* fft-mag (+ (- k 1) offset)))	;;; actual frq of peak
;;; old method for phase interpolation
;;;	      (setf pha (phase-interp-old (aref fftphase (- k 2)) left-bin
;;;				      (aref fftphase (- k 1)) central-bin  
;;;				      (aref fftphase k) right-bin))
	      (if (< offset 0)
		  (setf pha (phase-interp (aref fftphase (- k 2))(aref fftphase (- k 1)) (abs offset)))
		(setf pha (phase-interp (aref fftphase (- k 1))(aref fftphase k) offset)))
;;;	      (format t "offset: ~s amp: ~s left-phase: ~s peak-phase: ~s rightt-phase: ~s phase: ~s~%" 
;;;		      offset amp (aref fftphase (- k 2))(aref fftphase (- k 1))(aref fftphase k) pha)
	      ;;; store values in structure
	      (setf (ats-peak-amp ats-peak) amp)
	      (setf (ats-peak-frq ats-peak) frq)
	      (setf (ats-peak-pha ats-peak) pha)
	      ;;; push peak into peaks list
	      (push ats-peak peaks)))))
    ;;; return sorted list of peaks
    (sort peaks #'< :key #'ats-peak-frq)))
	      

;;; helper functions
(defun to-polar (ats-fft mags phase N &optional (norm 1.0))
  "
to-polar <ats-fft> <mags> <phase> <N>
returns the magnitude and phases of <ats-fft>
into the passed arrays <mags> and <phase>.
<N> is the highest bin where we look for spectral
data in the fft. We normalize mag values by [norm=1.0]
"
  (loop for k from 0 below N do
    (let ((x (aref (ats-fft-fdr ats-fft) k))
	  (y (aref (ats-fft-fdi ats-fft) k)))
      (setf (aref mags k) (* (sqrt (+ (* x x) (* y y))) norm))
      (setf (aref phase k) 
	    (if (= x y 0.0) 0.0  (atan  (- y) x))))))


(defun parabolic-interp (alpha beta gamma)
"
parabolic-interp <alpha> <beta> <gamma>
does parabolic interpolation of 3 points
returns the x offset and heigth
of the interpolated peak
"
  (let* ((dB-alpha (amp-db alpha))	;;; (alpha)
	 (dB-beta (amp-db beta))       ;;; (beta)
 	 (dB-gamma (amp-db gamma))     ;;; (gamma)
	 (offset 0)                     ;;; frq offset (p)
	 (height 0))                    ;;; parabola tip
	;;; parabolic interpolation
    (setf offset 
	  (* 0.5 (/ (- dB-alpha dB-gamma)
		    (+ dB-alpha (* -2 dB-beta) dB-gamma))))
    (setf height 
	  (db-amp (- dB-beta (* .25 (- dB-alpha dB-gamma) offset))))
    (values offset height)))


(defun phase-interp (peak-phase right-phase offset)
  (if (> (- peak-phase right-phase) (* 1.5 pi))
      (incf right-phase +two-pi+)
    (if (> (- right-phase peak-phase) (* 1.5 pi))
	(incf peak-phase +two-pi+)))
  (+ peak-phase (* offset (- right-phase peak-phase))))


;;; unused function  
(defun phase-interp-old (l-phase l-mag c-phase c-mag r-phase r-mag)
"
phase-interp <l-phase> <l-mag> <c-phase> <c-mag> <r-phase> <r-mag>
weighted mean of phase interpolation
"
  (/ (+ (* l-phase l-mag) (* c-phase c-mag) (* r-phase r-mag))
     (+ l-mag c-mag r-mag)))
