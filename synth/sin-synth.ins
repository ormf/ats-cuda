;;; -*- syntax: common-lisp; package: clm; base: 10; mode:lisp -*-
;;;
;;; ATS 
;;; by Juan Pampin
;;; juan@ccrma.stanford.edu
;;;
;;; sin-synth.ins
;;;
;;; additive synthesis using oscillators

;;; Parameters:
;;; ===========
;;; sound ->  ATS sound to synthesize
;;; (amp-scale 1.0) -> global amplitude scalar
;;; (amp-env '(0 1 1 1)) -> global amplitude envelope
;;; (frq-scale 1.0) -> global frequency scalar
;;; (duration nil) -> duration, if nil sound's duration is used 
;;; (par nil) -> list of partial numbers to sinthesize, if nil all partials
(definstrument sin-synth
  (start-time sound &key 
	      (amp-scale 1.0)
	      (amp-env '(0 1 1 1))
	      (frq-scale 1.0)
	      (duration nil)
	      (par nil))
  (let*((dur (if duration duration (ats-sound-dur sound)))
	(partials (if par (list-length par) (ats-sound-partials sound)))
	(a-env (make-array partials))
	(f-env (make-array partials))
        (osc-arr (make-array partials))
	(ampenv (make-env :envelope amp-env 
			  :duration dur 
			  :scaler amp-scale))
        (out-val 0.0))
    (format t "Initializing data...~%")
    (multiple-value-bind (beg end) (times->samples start-time dur)
      ;;; initialize envelopes and oscils
      (dotimes (pa partials)
  	(let* ((par-n (if par (nth pa par) pa)))
	  ;;; create partial's amplitude and frequency envelopes
	  (setf (aref a-env pa) 
		(make-env :envelope (ats-make-envelope sound par-n 'amp duration)
			  :duration dur))
	  (setf (aref f-env pa) 
		(make-env :envelope (ats-make-envelope sound par-n 'frq duration)
			  :duration dur
			  :scaler frq-scale))
	  ;;; store oscil
          (setf (aref osc-arr pa) 
		(make-oscil :frequency 0.0))))
      (format t "Synthesizing sound <~s>~%" (ats-sound-name sound))
      (run 
       (loop for i from beg to end do
	 (setf out-val 0.0)
	 (loop for j from 0 below partials do
	   (let*((amp-val (env (aref a-env j)))
		 (frq-val (hz->radians (env (aref f-env j)))))
	     (incf out-val (* amp-val (oscil (aref osc-arr j) frq-val)))))
	 (outa i (* (env ampenv) out-val)))))))



#|

;;; cl
(with-sound (:play nil :output "/zap/cl-sin.snd" :srate 44100
		   :statistics t :verbose t)
  (sin-synth 0.0 cl-new))

;;; crt-cs6
(with-sound (:play nil :output "/zap/crt-cs6-sin.snd" :srate 44100
		   :statistics t :verbose t)
  (sin-synth 0.0 crt-cs6))

|#
