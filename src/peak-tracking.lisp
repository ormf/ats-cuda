;;;
;;; ATS 
;;; by Juan Pampin
;;; juan@ccrma.stanford.edu
;;;
;;; File: peak-tracking.cl
;;; =======================
;;; This file contains the implementation
;;; of the ATS's peak tracking algorithm

(in-package :cl-ats)

(defun peak-tracking (tracks peaks-b &optional (frq-deviation 0.45)(alpha 0.0)(unmatched nil))
  "
peak-tracking <tracks> <peaks-b>
given a list of tracks <tracks> and two lists of peaks 
<peaks-a> and <peaks-b> this function tracks stable 
sinusoidal trajectories between the two frames.
It returns  a list with two lists
of untracked peaks: first the untracked ones from
<tracks>, second the untracked ones from <peaks-b>.
The <frq-deviation> parameter is used to decide
which peaks in <peaks-b> are candidates for a 
particular peak and track combination, this value is multiplied 
by the frequency of the track that is used as a central 
axis for the candidate search in the <peaks-b> pool.
NOTE: this function assumes that the client passes
peaks in <peaks-a> sorted by masking value
"
  (if (or (not tracks) (not peaks-b))
      (list (append tracks unmatched) peaks-b)
    (let* ((peak (first tracks))
	   (peak-frq (ats-peak-frq peak))
	   (peak-smr (ats-peak-smr peak))
         ;;; find frq limits for candidates
	   (frq-limits (get-limits peak-frq frq-deviation))
	 ;;; get possible candidates
	   (peak-candidates (find-candidates (sort (copy-seq peaks-b) #'< :key #'ats-peak-frq) frq-limits))
	 ;;; find best candidate
	   (matched-peak (find-best-candidate peak-candidates peak-frq peak-smr alpha)))
      (if matched-peak 
	  (setf (ats-peak-track matched-peak) (ats-peak-track peak)
		peaks-b (remove matched-peak peaks-b))
	(push peak unmatched))
      (peak-tracking (rest tracks) peaks-b frq-deviation alpha unmatched))))
	 
;;; helper functions
(defun get-limits (peak-frq frq-dev)
  "
get-limits <peak-frq> <frq-dev>
returns a list with the high and low frq
limit for <peak-frq> given <ftq-dev>
"
  (let* ((half-band (* 0.5 peak-frq frq-dev))
	 (lo (- peak-frq half-band))
	 (hi (+ peak-frq half-band)))
    (list lo hi)))

(defun find-candidates (peaks frq-limits &optional l)
  "
find-candidates <peaks> <frq-limits>
returns a list with candidate peaks that 
fall within the frq-limits in <peaks>
NOTE: this function assumes that <peaks>
is sorted by frequency
"
  (let* ((lo (first frq-limits))
	 (hi (second frq-limits)))
    (if (or (not peaks)
	    (> (ats-peak-frq (first peaks)) hi))
	(nreverse l)
      (progn
	(if (<= lo (ats-peak-frq (first peaks)) hi)
	    (push (first peaks) l))
	(find-candidates (rest peaks) frq-limits l)))))


(defun find-best-candidate (peak-candidates peak-frq peak-smr &optional (alpha 0.0))
  "
find-best-candidate <peak-candidates> <peak-frq>
returns the peak from the <peak-candidates> list
that is closer in frequency to <peak-frq>
"
  (let* ((delta most-positive-double-float)
	 (best-peak nil)
	 (local-delta nil))
    (dolist (p peak-candidates)
      (if (< (setf local-delta 
		   (/ (+ (abs (- (ats-peak-frq p) peak-frq))
			 (* alpha (abs (- (ats-peak-smr p) peak-smr))))
		      (+ 1 alpha)))
	     delta)
	  (setf best-peak p
		delta local-delta)))
    best-peak))
	


(defun update-tracks (tracks track-length frame-n ana-frames &optional (beta 0.0))
  "
updates the list of current <tracks>
we use <track-length> frames of memory
the function returns a list of tracks
If <tracks> is nil peaks from the previous 
frame are returned in a list to init the tracks
"
  (if tracks 
      (let* ((frames (if (< frame-n track-length)
			 frame-n
		       track-length))
	     (first-frame (- frame-n frames)))
   ;;; average amp, frq and smr for all tracks
	(loop for g in tracks do
	  (let ((track (ats-peak-track g))
		(frq-acc 0)(f 0)
		(amp-acc 0)(a 0)
		(smr-acc 0)(s 0)
		(last-frq 0)
		(last-amp 0)
		(last-smr 0))
	    (loop for i from first-frame below frame-n do
	      (let* ((l-peaks (aref ana-frames i))
		     (peak (find track l-peaks :key #'ats-peak-track)))
		(when peak
		  (when (> (ats-peak-frq peak) 0.0)
		    (setf last-frq (ats-peak-frq peak))
		    (incf frq-acc (ats-peak-frq peak))
		    (incf f))
		  (when (> (ats-peak-amp peak) 0.0)
		    (setf last-amp (ats-peak-amp peak))
		    (incf amp-acc (ats-peak-amp peak))
		    (incf a))
		  (when (> (ats-peak-smr peak) 0.0)
		    (setf last-smr (ats-peak-smr peak))
		    (incf smr-acc (ats-peak-smr peak))
		    (incf s)))))
	    (if (> f 0)
		(setf (ats-peak-frq g) 
		      (+ (* (- 1 beta)(/ frq-acc f))(* beta last-frq))))
	    (if (> a 0)
		(setf (ats-peak-amp g) 
		      (+ (* (- 1 beta)(/ amp-acc a))(* beta last-amp))))
	    (if (> s 0)
		(setf (ats-peak-smr g) 
		      (+ (* (- 1 beta)(/ smr-acc s))(* beta last-smr))))))
	tracks)
    (copy-seq (aref ana-frames (1- frame-n)))))
   
	    
	
