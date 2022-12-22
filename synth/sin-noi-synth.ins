;;; -*- syntax: common-lisp; package: clm; base: 10; mode:lisp -*-
;;;
;;; ATS 
;;; by Juan Pampin
;;; juan@ccrma.stanford.edu
;;;
;;; sin-noi-synth.ins
;;;

;;; General Purpose ATS Synthesizer
;;; ===============================
;;; This instrument sythesizes both sinusoids and noise.
;;; The noise part can contain the partials energy only
;;; (band-noise NIL), or both the partials energy and the 
;;; complementary critical-band energy (if they exist).
;;; Time information can be handled in two ways:
;;; using time information from partials (time-ptr NIL), 
;;; or using a time-pointer envelope. In time-pointer mode
;;; X values of the time-ptr envelope are proportional 
;;; time in the ATS sound (1.0=end) and Y values are proportional 
;;; times in the output sound (1.0=dur)
;;;
;;; Parameters:
;;; ===========
;;; sound ->  ATS sound to synthesize
;;; (amp-scale 1.0) -> global amplitude scalar
;;; (amp-env '(0 1 1 1)) -> global amplitude envelope
;;; (frq-scale 1.0) -> global frequency scalar
;;; (duration nil) -> duration, if nil sound's duration is used 
;;; (time-ptr nil) -> time pointer, if nil sound's time is used
;;; (par nil) -> list of partial numbers to sinthesize, if nil all partials
;;; (noise-env '(0 1 1 1)) -> global envelope for noise component
;;; (noise-only NIL) -> switch for noise-only synthesis
;;; (band-noise t) -> switch for band-noise synthesis

(in-package :cl-ats)

(definstrument sin-noi-synth
  (start-time sound &key 
	      (amp-scale 1.0)
	      (amp-env '(0 1 1 1))
	      (frq-scale 1.0)
	      (duration nil)
	      (time-ptr nil)
	      (par NIL)
	      (noise-env '(0 1 1 1))
	      (noise-only NIL)
	      (band-noise t))
  (if (not (ats-sound-energy sound)) (error "Sound has no noise energy!~%"))
  (let*((dur (if duration duration (ats-sound-dur sound)))
	(n-pars (if par (list-length par) (ats-sound-partials sound)))
	(par (make-array n-pars :initial-contents (if par par (loop for i from 0 below n-pars collect i))))
	(frames (ats-sound-frames sound))
	(band-noise (and band-noise (if (ats-sound-band-energy sound) T NIL)))
	(n-bands (if band-noise (length (ats-sound-bands sound))))
	(bands (if band-noise (coerce (ats-sound-bands sound) 'list)))
	(nyquist (* 0.5 (ats-sound-sampling-rate sound)))
	(window-size (ats-sound-window-size sound))
	(ampenv (make-env :envelope amp-env :duration dur :scaler amp-scale))
	(noienv (make-env :envelope noise-env :duration dur :scaler 1.0))
	(ptrenv (if time-ptr (make-env :envelope time-ptr :duration dur :scaler (1- frames))))
	(amp-arr (if (and (not time-ptr)(not noise-only)) (make-array n-pars)))
	(frq-arr (if (not time-ptr)(make-array n-pars)))
	(eng-arr (if (not time-ptr)(make-array n-pars)))
        (noi-arr (make-array n-pars))
        (sin-arr (make-array n-pars))
	(band-env-arr (if (and (not time-ptr) band-noise) (make-array n-bands)))
	(band-noi-arr (if band-noise (make-array n-bands)))
	(band-sin-arr (if band-noise (make-array n-bands)))
        (out-val 0.0)
	(noi-val 0.0)
	(frm 0.0))
    (format t "Initializing data...~%")
    ;;; initialize envelopes, noise generators, and oscils
    (loop for b from 0 below n-pars do
      (let ((n (aref par b)))
        ;;; store amp envelope
	(when (not time-ptr)
	  (if (not noise-only)
	      (setf (aref amp-arr b)
		    (make-env :envelope (ats-make-envelope sound n 'amp duration)
			      :duration dur)))
        ;;; store frq envelope
	  (setf (aref frq-arr b)
		(make-env :envelope (ats-make-envelope sound n 'frq duration)
			  :duration dur
			  :scaler frq-scale))
        ;;; store energy envelope
	  (setf (aref eng-arr b)
		(make-env :envelope (ats-make-envelope sound n 'energy duration)
			  :duration dur)))
	  ;;; store noise
	(setf (aref noi-arr b) (make-rand-interp 0.0 1.0))
	  ;;; store oscil
	(setf (aref sin-arr b) (make-oscil 0.0))))
    (if band-noise
	(loop 
	  for n in bands
	  for b from 0 
	  do
	  (let* ((f-low (nth n *ats-critical-band-edges*)) 
		 (f-up (nth (1+ n) *ats-critical-band-edges*))
		 (bw (- f-up f-low))
		 (fc (+ f-low (* 0.5 bw))))
	    (if (> (+ fc bw) nyquist)
		(setf bw (- nyquist fc)))
	  ;;; store energy envelope
	    (if (not time-ptr)
		(setf (aref band-env-arr b)
		      (make-env :envelope (ats-make-envelope sound b 'band-energy duration)
				:duration dur)))
	  ;;; store noise
	    (setf (aref band-noi-arr b) (make-rand-interp bw 1.0))
	  ;;; store oscil
	    (setf (aref band-sin-arr b) (make-oscil fc)))))
    (multiple-value-bind (beg end) (times->samples start-time dur)
	(format t "Synthesizing Sound: <~s>~%" (ats-sound-name sound))
	(run
	 (loop for i from beg to end do
	   (setf out-val 0.0)
	   (setf noi-val (env noienv))
	   (if time-ptr (setf frm (env ptrenv)))
	   (loop for j from 0 below n-pars do
	     (let* ((p (aref par j))
		    (amp-val (if (not noise-only) 
				 (if time-ptr (get-amp-f sound p frm) (env (aref amp-arr j)))
			       0.0))
		    (frq-val (if time-ptr (* frq-scale (get-frq-f sound p frm))(env (aref frq-arr j))))
		    (eng-val (if time-ptr (get-energy-f sound p frm)(env (aref eng-arr j))))
		    (bw (if (< frq-val 500.0) 50.0
			  (* frq-val 0.1)))
		    (sine (oscil (aref sin-arr j) (hz->radians frq-val)))
		    (noise (* noi-val (rand-interp (aref noi-arr j) (hz->radians bw)))))
	       (incf out-val 
		     (+ (* sine amp-val)
			(if (> eng-val 0.0)
			    (* sine noise (compute-noi-gain eng-val window-size))
			  0.0)))))
	   (if band-noise
	       (loop for j from 0 below n-bands do
		 (let* ((gain-val (if time-ptr (get-band-energy-f sound j frm)(env (aref band-env-arr j)))))
		   (if (> gain-val 0.0)
		       (progn
			 (setf gain-val (compute-noi-gain gain-val window-size))
			 (incf out-val (* gain-val noi-val
					  (oscil (aref band-sin-arr j))
					  (rand-interp (aref band-noi-arr j)))))))))
	   (outa i (* (env ampenv) out-val)))))))

#|

;;; cl
(with-sound (:play nil :output "/zap/cl-1.snd" :srate 44100
		   :statistics t :verbose t)
  (sin-noi-synth 0.0 cl-new))

(with-sound (:play nil :output "/zap/cl-2.snd" :srate 44100
		   :statistics t :verbose t)
  (sin-noi-synth 0.0 cl 
		     :par nil 
		     :time-ptr '(0.0 0.0 1.0 1.0)
		     :noise-env '(0 0 0.5 0.7 1 1)
		     :amp-scale 1.0  
		     :duration nil 
		     :noise-only nil))

;;; crt-cs6
(with-sound (:play nil :output "/zap/crt-cs6-1.snd" :srate 44100
		   :statistics t :verbose t)
  (sin-noi-synth 0.0 crt-cs6 
		 :par nil 
		 :amp-scale 1.0 
		 :frq-scale 0.25
		 :time-ptr '(0.0 0.0 0.025 0.1 0.5 0.5 1.0 1.0)
		 :duration (* (ats-sound-dur crt-cs6) 4)
		 :band-noise t))


|#
