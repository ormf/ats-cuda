;;; 
;;; scratch.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2022 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
;;;
;;; Revision history: See git repository.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Gnu Public License, version 2 or
;;; later. See https://www.gnu.org/licenses/gpl-2.0.html for the text
;;; of this agreement.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU General Public License for more details.
;;;
;;; **********************************************************************

(in-package :incudine)

(define-vug pan2 (in pos)
  "Stereo equal power panpot with position POS between 0 (left)
and 1 (right)."
  (with-samples ((alpha (* +half-pi+ pos))
                 (left (cos alpha))
                 (right (sin alpha)))
    (cond ((= current-channel 0) (* left in))
          ((= current-channel 1) (* right in))
          (t +sample-zero+))))

(define-ugen oscili sample (amp cps (buf buffer))
  "A simple oscillator with linear interpolation."
  (osc buf cps amp 0 :linear))

(define-vug oscili2 (amp cps (buf buffer))
  "A simple oscillator with linear interpolation."
  (osc buf cps amp 0 :linear))

(compile-vug 'pan2 'sample)

(dsp! pan2-ugen-bug (freq amp pos)
  (foreach-channel
    (cout (pan2 (oscili2 amp freq *sine-table*) pos))))

(pan2-ugen-bug 440 0.3 0.5 :id 1)

(dsp! pan-test1 (freq amp pos)
  (declare (inline pan2))
  (foreach-channel
    (cout (pan2 (oscili amp freq *sine-table*) pos))))

(pan-test1 440 0.3 0.5 :id 1)


(rt-start)

(free 1)

(rt-stop)

(setq *rt-block-size* 1)
(macrolet ((sine-sum (n)
               `(+ ,@(mapcar (lambda (x)
                               `(sine (* freq ,x) ,(/ .3 n) 0))
                             (loop for i from 1 to n collect i)))))
  (sine-sum 10))

(defmacro sine-sum (n)
  `(+ ,@(mapcar (lambda (x)
                  `(sine (* freq ,x) ,(/ .3 n) 0))
                (loop for i from 1 to n collect i))))

(macroexpand-1 '(sine-sum 10))

(cout)

(dsp! swap-channels ((buf buffer) rate start-pos (loop-p boolean))
  (foreach-channel
    (cout (let ((current-channel (if (zerop current-channel) 1 0)))
            (buffer-play buf rate start-pos loop-p #'stop)))))



(define-vug buzz-hq (freq amp (num-harm fixnum) phase harm-change-lag)
  (with ((count 0)
         (cross 1.0d0)
         (nh-lag (sample->fixnum (* harm-change-lag *sample-rate*)))
         (inc-interp (/ (sample 1) nh-lag))
         (old-num-harm 0)
         (nh0 1)
         (nh1 1)
         (old-two-nh-plus-one 3)
         (two-nh-plus-one 3)
         (res0 +sample-zero+)
         (res1 +sample-zero+)
         (num0 +sample-zero+)
         (num1 +sample-zero+)
         (denom +sample-zero+)
         (amp0 (* amp 0.5))
         (mult0 (/ amp0 nh0))
         (mult1 (/ amp0 nh1))
         (fdiv2 (* 0.5 freq))
         (angle +sample-zero+))

    (declare (type sample cross res0 res1 num0 num1 denom amp0 mult0 mult1
                   fdiv2 angle inc-interp)
             (type fixnum count nh0 nh1 nh-lag old-num-harm
                   old-two-nh-plus-one two-nh-plus-one))
    (initialize
      (setf nh1 (max 1 (abs num-harm)))
      (setf two-nh-plus-one (1+ (* 2 nh1))))
    ;; Expand here and not in the next UNLESS form if the amplitude
    ;; is modulated
    (maybe-expand mult0 mult1)
    (unless (or (= old-num-harm num-harm) (plusp count))
        (setf old-num-harm num-harm
              count nh-lag
              cross +sample-zero+
              nh0 nh1
              nh1 (max 1 (abs num-harm))
              old-two-nh-plus-one two-nh-plus-one
              two-nh-plus-one (1+ (* 2 nh1))
              mult0 (/ amp0 nh0)
              mult1 (/ amp0 nh1)))
    (setf angle (+ (* +twopi+ (phasor fdiv2 0)) phase))
    (setf denom (sin angle))
    (cond ((or (> denom 1.d-5) (< denom -1.d-5))
           (setf num1 (sin (* angle two-nh-plus-one)))
           (setf res1 (* mult1 (- (/ num1 denom) 1.0)))
           (cond ((plusp count)
                  (decf count)
                  (setf num0 (sin (* angle old-two-nh-plus-one)))
                  (setf res0 (* mult0 (- (/ num0 denom) 1.0)))
                  (prog1 (linear-interp cross res0 res1)
                    (incf cross inc-interp)))
                 (t res1)))
          (t (when (plusp count)
               (decf count)
               (incf cross inc-interp))
             amp))))

(define-vug sin-noi-synth
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


(dsp! play-buffer* ((buffer buffer) amp rate start end)
  (:defaults (incudine:incudine-missing-arg "BUFFER") 0 1 0 0))

(loop for (name val) in '(   (amp-scale 1.0)
   (amp-env '(0 1 1 1))
   (frq-scale 1.0)
   (duration nil)
   (time-ptr nil)
   (par NIL)
   (noise-env '(0 1 1 1))
   (noise-only NIL)
                          (band-noise t))
      collect val)

(ats-cuda::make-ats-sound)

(typep nil null)



(test nil)
incudine::*rt-block-size*

(dsp! test ((blah (or sample boolean)))
  (initialize
   (format t "~&Hello World: ~a!" blah))
  (foreach-channel
    (cout (pan2 (sine 440 0.3) 0.2))))

(dsp! test ((blah (or sample boolean)))
  (initialize
   (format t "~&Hello World: ~a!" blah))
  (out (sine 440 0.3)(sine 443 0.3)))

(test nil)
(free 0)

(rt-start)


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
	   (outa i (* (env ampenv) out-val)))))



(destructuring-bind (bindings init update ergebnis)
    `(((x0s (make-array (length freqs) :element-type 'sample :initial-element 0.0d0))
       (x1s (make-array (length freqs) :element-type 'sample :initial-element 0.0d0)))
      (setf (aref x1s n) input)
      (setf (aref x0s n) (aref x1s n) (aref x1s n) (update input) (aref deltas n) (- (aref x0s n) (aref x1s n)))
      (+ (aref x1s n) (* (aref phases n) (aref deltas n))))
  `(initialize
    (setf (aref phases n) 0)
    (setf (aref incs n) (* (aref freqs n) *sample-duration*))
    ,init))



(with-gensyms (interp-n)
  `(vuglet ((,interp-n (n input freqs)
                       (with ((phases (make-array (length freqs) :element-type 'sample :initial-element 0.0d0))
                              (deltas (make-array (length freqs) :element-type 'sample :initial-element 0.0d0))
                              (incs (make-array (length freqs) :element-type 'sample :initial-element 0.0d0))
                              ,@bindings)
                         (declare (type (simple-array sample *)
                                        phases deltas incs ,@(mapcar #'first bindings)))
                         ,@(when init `((initialize
                                         (setf (aref phases n) 0)
                                         (setf (aref incs n) (* (aref freqs n) *sample-duration*))
                                         ,init)))
                         (decf (aref phases n) (aref incs n))
                         (when (minusp (aref phases n))
                           (setf (aref phases n) (wrap (aref phases n) 0 1))
                           ,update)
                         ,ergebnis)))
     (,interp-n ,idx ,generator-form ,freqs)))
