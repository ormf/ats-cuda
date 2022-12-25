;;;
;;; ATS 
;;; by Juan Pampin
;;; juan@ccrma.stanford.edu
;;;
;;; formants.lisp
;;;

(in-package :ats-cuda)

;;; minimum amplitude value in dB
(defparameter *formant-amp-tresh* -60)

;;; power used to get new envelope
(defparameter *formant-env-pow* 2.0)

;;; gives the spectral enveloe for a frame
(defun get-spectral-env (sound frame &optional (tresh *formant-amp-tresh*)(pow *formant-env-pow*))
"
get-spectral-env <sound> <frame>
gives the spectral envelope as a break-point list at frame

"
(let*((par (ats-sound-partials sound))
      (env nil)
      (amp-tresh (db-amp tresh)))
  (loop for i from 0 below par do 
	(let ((amp-val (aref (aref (ats-sound-amp sound) i) frame)))
	  (if (>= amp-val amp-tresh)
	      (setf env (append (list (amp-db amp-val) (aref (aref (ats-sound-frq sound) i) frame))
				env))
	    (setf env (append (list tresh (aref (aref (ats-sound-frq sound) i) frame))
				env)))))
  (append '(0.0 0.0) 
	  (un-db-env 
	   (envelope-simplify (reverse env) tresh 
			 (expt (ats-sound-partials sound) pow)))
	  (list (/ (ats-sound-sampling-rate sound) 2.0) 0.0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; scale all partials by a spectral envelope
(defun scale-partials-by-env (sound frame env &optional (first-partial 0)(last-partial nil))
  "
scales partials of <sound> at <frame> by <env>
from [first-partial] to [last-partial]
"
  (let ((last-partial (if last-partial last-partial (ats-sound-partials sound))))
    (loop for i from first-partial below last-partial do
      (setf (aref (aref (ats-sound-amp sound) i) frame)
	    (dfloat
	    (envelope-interp (aref (aref (ats-sound-frq sound) i) frame) env))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun fit-formants (sound-1 sound-2)  
"
fit-formants <sound-1> <sound-2>
fits the amplitudes of <sound-1>
to the formant structure of <sound-2>
this function assumes that both sounds
have same number of partials and frames
"
(let ((frames (ats-sound-frames sound-1)))
  (format t "~%Fitting Formants of ~S to ~S...~%" 
	  (ats-sound-name sound-1)(ats-sound-name sound-2))
  (do ((i 0 (1+ i)))
      ((= i frames))
    (format t "~d " i) 
    (scale-partials-by-env sound-1 i (get-spectral-env sound-2 i)))))
	









