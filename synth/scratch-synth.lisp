;;; 
;;; scratch-synth.lisp
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


(define-vug ats-noise-bank (pos
                            (freqs (simple-array sample))
                            (bws (simple-array sample))
                            (amps (simple-array sample)))
  "a bank of band-limited noise with freqs, amps and bandwidths (in Hz)
provided as arrays of type sample. The length of the freqs array
determines the number of noise bands to generate in parallel and the
size of the other two arrays has to be >= the size of the freqs
array."
  (with-samples ((out 0))
    (with-sample-arrays
        ((sin-phase-array (sample-array (length freqs))))
      (setf out 0.0d0)
      (dotimes (n (length freqs))
        (incf out (* (sine-n n (aref freqs n) 1.0d0 sin-phase-array)
                     (i-aref-n amps n pos)
                     (randi-n n bws))))
      out)))

(define-vug ats-noise-bank (timeptr
                            (freqs (simple-array sample *))
                            (bws (simple-array sample *))
                            (amps (simple-array sample *)))
  "a bank of band-limited noise with freqs, amps and bandwidths (in Hz)
provided as arrays of type sample. The length of the freqs array
determines the number of noise bands to generate in parallel and the
size of the other two arrays has to be >= the size of the freqs
array."
  (with-samples ((out 0))
    (with-sample-arrays
        ((sin-phase-array (sample-array (length freqs))))
      (setf out 0.0d0)
      (dotimes (n (length freqs))
        (incf out (* (sine-n n (aref freqs n) (aref amps n) sin-phase-array)
                     (i-aref-n amps n timeptr)
                     (randi-n n bws))))
      out)))

(define-vug ats-noise-bank (timeptr
                            (freqs (simple-array sample *))
                            (bws (simple-array sample *))
                            (amps (simple-array sample *)))
  "a bank of band-limited noise with freqs, amps and bandwidths (in Hz)
provided as arrays of type sample. The length of the freqs array
determines the number of noise bands to generate in parallel and the
size of the other two arrays has to be >= the size of the freqs
array."
  (with-samples ((out 0))
    (with-sample-arrays
        ((sin-phase-array (sample-array (length freqs))))
      (setf out 0.0d0)
      (dotimes (n (length freqs))
        (incf out (* (sine-n n (aref freqs n) (aref amps n) sin-phase-array)
                     (i-aref-n amps n timeptr)
                     (randi-n n bws)
)))
      (* (i-aref-n amps 3 timeptr) 100 (sine 440 0.1)))))

(define-vug ats-noise-bank (timeptr
                            (freqs (simple-array sample *))
                            (bws (simple-array sample *))
                            (amps (simple-array sample *)))
  "a bank of band-limited noise with freqs, amps and bandwidths (in Hz)
provided as arrays of type sample. The length of the freqs array
determines the number of noise bands to generate in parallel and the
size of the other two arrays has to be >= the size of the freqs
array."
  (with-samples ((out 0))
    (with-sample-arrays
        ((sin-phase-array (sample-array (length freqs))))
      (setf out 0.0d0)
      (dotimes (n (length freqs))
        (incf out (* (sine-n n (aref freqs n) (aref amps n) sin-phase-array)
                     0.02
                     (randi-n n bws)
)))
      out)))

(dsp! nb01 (pos
            (noise-cfreqs (simple-array sample *))
            (noise-bws (simple-array sample *))
            (amps (simple-array sample *)))
  (stereo (ats-noise-bank pos noise-cfreqs noise-bws amps)))

(dsp! nb01 ((noise-cfreqs (simple-array sample *))
            (noise-bws (simple-array sample *))
            (amps (simple-array sample *)))
  (with-samples ((timeptr (line 0 253 2.5 #'free)))
    (stereo (* (i-aref-n amps 3 timeptr) 100 (sine 440 0.1)))))

(dsp! nb01 (timeptr
            (noise-cfreqs (simple-array sample *))
            (noise-bws (simple-array sample *))
            (noise-energy (simple-array sample *)))
  (stereo (sine 440 0.1)))

(nb01
 0.0d0
 (sample-array 2 :initial-contents '(300d0 700.0d0))
 (sample-array 2 :initial-contents '(30d0 20.0d0))
 (sample-array '(2 2) :initial-contents '((0.2d0 0.1d0)(0.2d0 0.1d0))))


(nb01
 9.0d0
 (get-noise-c-freqs (ats-cuda::ats-sound-bands ats-cuda::cl))
 (get-noise-bws (ats-cuda::ats-sound-bands ats-cuda::cl))
 (vec->array (ats-cuda::ats-sound-band-energy ats-cuda::cl)))

(dsp! nb01 ((noise-cfreqs (simple-array sample *))
            (noise-bws (simple-array sample *))
            (noise-energy (simple-array sample *)))
  (with-samples ((timeptr (line 0 253 2.5 #'free)))

    (stereo (*    (i-aref-n noise-energy 3 timeptr) 100 (sine 440 0.1)))))

(length (aref (ats-cuda::ats-sound-band-energy ats-cuda::cl) 0))


(rt-start)

(typep
 (vec->array (ats-cuda::ats-sound-energy ats-cuda::cl))
 '(simple-array double-float))

(typep
 (ats-cuda::ats-sound-energy ats-cuda::cl)
 '(simple-array double-float))

(typep (type-of (vec->array (ats-cuda::ats-sound-energy ats-cuda::cl)))
       'simple-array)

(i-aref-n (vec->array (ats-cuda::ats-sound-energy ats-cuda::cl)) 0 0.0d0)





(aref (sample-array '(25 2) :initial-contents (loop for x below 25 collect (list 0.1d0 0.2d0)))
      3 1)
(nb01
 0.9d0
 (get-ats) (ats-cuda::ats-sound-bands ats-cuda::cl)
 (sample-array '(2 2) :initial-contents '((0.01d0 0.02d0)(0.01d0 0.02d0))))

(vector 1 2 3 4)

(i-aref (aref (ats-cuda::ats-sound-energy ats-cuda::cl) 1) 1.0d0)



(i-aref-n (vec->array (ats-cuda::ats-sound-energy ats-cuda::cl)) 1 2.0d0)
(make-array (list))
(loop for x across 
      
      )

(vector)
(aref (aref  0) 3)
(i-aref-n
 (sample-array '(2 2) :initial-contents '((0.01d0 0.02d0)(0.01d0 0.02d0)))
 0 0.999d0)






(define-vug ats-master-vug (timeptr
                            (ats-data (simple-array sample *))
                            (partials list)
                            (fmod (simple-array sample *))
                            (amod (simple-array sample *))
                            res-bal)
  (:defaults 0 (incudine:incudine-missing-arg "ATS_DATA") nil (sample-array 1) (sample-array 1) 0.5d0)
  (with ((freqs (aref ats-data 0))
         (amps (aref ats-data 1))
         (pnoi (aref ats-data 2))
         (noise-energy (aref ats-data 3))
         (noise-bws (aref ats-data 4))
         (noise-cfreqs (aref ats-data 5)))
    (declare (type (simple-array sample (* *)) freqs amps pnoi noise-energy)
             (type (simple-array sample *) noise-bws noise-cfreqs))
    (ats-noise-bank timeptr noise-cfreqs noise-bws noise-energy)))

(sample )

(dsp! sin-noi-synth
    ((ats-sound ats-cuda::ats-sound)
     amp-scale
     (frq-scale (or null float))
     (duration (or null float))
     (time-ptr (or null list))
     (par (or null list))
     (noise-env (or null list))
     (noise-only boolean)
     (band-noise boolean))
  (:defaults (incudine:incudine-missing-arg "ATS_SOUND") 1.0d0 1.0 nil nil nil nil nil t)
  (with-sample-arrays
      ((noise-band-energy (alexandria:copy-array (ats-cuda::ats-sound-band-energy ats-sound))))
    (with-samples ((timeptr 0.0d0))
      (initialize
       (break "~a"
             (list
              (get-noise-bws (ats-cuda::ats-sound-bands ats-sound))
              (get-noise-c-freqs (ats-cuda::ats-sound-bands ats-sound))
              noise-band-energy))
       )
      (stereo
       (ats-noise-bank
        05.0d0
        (get-noise-bws (ats-cuda::ats-sound-bands ats-sound))
        (get-noise-c-freqs (ats-cuda::ats-sound-bands ats-sound))
        noise-band-energy
        )))))

(defparameter *testvals* nil)

(alexandria:copy-array (ats-cuda::ats-sound-band-energy ats-cuda::cl))

(sin-noi-synth 0.0 ats-cuda::cl)


(setf timeptr (line 0.0d0 (ats-cuda::ats-sound-frames ats-sound)
                    (ats-cuda::ats-sound-dur ats-sound)))

(dsp! gliss ()
  (with-samples ((freq 440.0))
    (setf freq (line 440 660 5 #'free))
    (stereo (sine freq 0.1))))

(defstruct blah
  (startfreq 220.0d0 :type double-float)
  (dur 3.0d0 :type double-float))

(defvar *test* (make-blah))

(dsp! gliss ((params blah) bal)
  (:defaults (incudine:incudine-missing-arg "ATS_SOUND") 0.5d0)
  (with ((start (blah-startfreq params))
         (dur (blah-dur params)))
    (declare (type sample start dur))
    (with-samples ((freq (line start 660 dur #'free)))
      (stereo (* (res-level bal) (sine freq 0.1))))))

(dsp! gliss ((ats-sound ats-cuda::ats-sound) bal)
  (:defaults (incudine:incudine-missing-arg "ATS_SOUND") 0.5d0)
  (with ((start 220)
         (dur 2.5))
    (declare (type sample start dur))
    (with-samples ((freq (line start 660 dur #'free)))
      (stereo (* (res-level bal) (sine freq 0.1))))))

ats-cuda::cl

(gliss *test* 0.5)

(rt-start)

(rt-start)

(ats-master-vug
                    timeptr
                    ats-data
                    partials
                    (sample-array len :initial-element curr-amp)
                    (sample-array len :initial-element (sample frq-scale))
                    0.5d0)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(dsp! test4 ((freqs (simple-array sample *))
             (amps (simple-array sample *)))
  (:defaults (make-array 2 :element-type 'sample :initial-contents '(440.0d0 880.0d0))
             (make-array 2 :element-type 'sample :initial-contents '(0.02d0 0.01d0)))
  (with ((phases (make-array 2 :element-type 'sample :initial-element 0.0d0)))
    (declare (type (simple-array sample *) phases))
    (stereo (sine-bank freqs amps phases))))

(dsp! test5 ((freqs (simple-array sample *))
             (amps (simple-array sample *))
             (bws (simple-array sample *)))
  (:defaults (sample-array 2 :initial-contents '(440.0d0 880.0d0))
             (sample-array 2 :initial-contents '(0.02d0 0.01d0))
             (sample-array 2 :initial-element 10.0d0))
  (stereo (noise-bank freqs amps bws)))

;;; (rt-start)

(test4
 (sample-array 3 :initial-contents '(440.0d0 660.0d0 1250.0d0))
 (sample-array 3 :initial-contents '(0.05d0 0.01d0 0.01d0)))

(test5
 (sample-array 3 :initial-contents '(440.0d0 660.0d0 1250.0d0))
 (sample-array 3 :initial-contents '(0.005d0 0.01d0 0.01d0))
 (sample-array 3 :initial-contents '(300.0d0 100.0d0 200.0d0)))


(dsp! test3 ((freqs (simple-array sample *))
             (amps (simple-array sample *)))
  (:defaults (make-array 2 :element-type 'sample :initial-contents '(440.0d0 880.0d0))
             (make-array 2 :element-type 'sample :initial-contents '(0.05d0 0.03d0)))
  (with ((out 0.0d0)
         (init-n (length freqs))
         (phase-array (make-array init-n :element-type 'sample :initial-element 0.0d0))
         (sine-phase-array (make-array init-n :element-type 'sample :initial-element 0.0d0)))
    (declare (type sample out)
             (type (simple-array sample *) phase-array sine-phase-array))
    (setf out 0.0d0)
    (loop
      for n below init-n
      do (incf out (sine-n n phase-array (aref freqs n) (aref amps n) sine-phase-array)))
    (stereo out)))

(dsp! bl-noise-test-n ((freqs (simple-array sample *)) (bws (simple-array sample *)) (amps (simple-array sample *)))
    (with ((out 0.0d0)
           (init-n (length freqs))
           (phase-array (make-array init-n :element-type 'sample :initial-element 0.0d0))
           (sine-phase-array (make-array init-n :element-type 'sample :initial-element 0.0d0)))
    (declare (type sample out)
             (type (simple-array sample *) phase-array sine-phase-array))
    (loop
      for n below init-n
      do (progn
;;           (format t "~&n: ~a" n)
           (incf out
                 (* (sine-n n phase-array (aref freqs n) (aref amps n) sine-phase-array)
                    (randi-n n bws)))))
    (stereo out)))

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

(ats-make-envelope cl 0 'amp)

(ats-sound-partials cl)

(in-package :incudine)

(dsp! sin-noi-cuda
    (start-time (sound ats-cuda::ats-sound) amp-scale (amp-env (or list boolean))
                frq-scale (duration (or sample boolean))
                (time-ptr (or list boolean)) (par boolean) (noise-env (or list boolean))
                (noise-only boolean) (band-noise boolean))
  (:defaults 0.0 (incudine:incudine-missing-arg "ATS-SOUND") 1.0 '(0 1 1 1) 1.0 nil nil nil '(0 1 1 1) nil t)
  
  (if (not (ats-sound-energy sound)) (error "Sound has no noise energy!~%"))
  (let*
      ((dur (if duration duration (ats-cuda::ats-sound-dur sound)))
       (n-pars (if par (list-length par) (ats-cuda::ats-sound-partials sound)))
       (par (make-array n-pars :initial-contents (if par par (loop for i from 0 below n-pars collect i))))
       (frames (ats-cuda::ats-sound-frames sound))
       (band-noise (and band-noise (if (ats-cuda::ats-sound-band-energy sound) T NIL)))
       (n-bands (if band-noise (length (ats-cuda::ats-sound-bands sound))))
       (bands (if band-noise (coerce (ats-cuda::ats-sound-bands sound) 'list)))
       (nyquist (* 0.5 (ats-cuda::ats-sound-sampling-rate sound)))
       (window-size (ats-cuda::ats-sound-window-size sound))
       (ampenv (make-env :envelope amp-env :duration dur :scaler amp-scale))
       (noienv (make-env :envelope noise-env :duration dur :scaler 1.0))
       (ptrenv (if time-ptr (make-env :envelope time-ptr :duration dur :scaler (1- frames))))
       (amp-arr (unless (or time-ptr noise-only) (make-array n-pars)))
       (frq-arr (unless time-ptr (make-array n-pars)))
       (eng-arr (unless time-ptr (make-array n-pars)))
       (noi-arr (make-array n-pars))
       (sin-arr (make-array n-pars))
       (band-env-arr (if (and (not time-ptr) band-noise) (make-array n-bands)))
       (band-noi-arr (if band-noise (make-array n-bands)))
       (band-sin-arr (if band-noise (make-array n-bands))))
    (with-samples
        ((out-val 0.0)
         (noi-val 0.0)
         (frm 0.0))
      (initialize
       (format t "Inititalizing...~%")
       (loop for b from 0 below n-pars do
         (let ((n (aref par b)))
;;; store amp envelope
           (when (not time-ptr)
	     (unless noise-only
	       (setf (aref amp-arr b)
		     (make-env :envelope (ats-cuda::ats-make-envelope sound n 'amp duration)
			       :duration dur)))
;;; store frq envelope
	     (setf (aref frq-arr b)
	           (make-env :envelope (ats-cuda::ats-make-envelope sound n 'frq duration)
			     :duration dur
			     :scaler frq-scale))
;;; store energy envelope
	     (setf (aref eng-arr b)
	           (make-env :envelope (ats-cuda::ats-make-envelope sound n 'energy duration)
			     :duration dur)))
;;; store noise
           (setf (aref noi-arr b) (cudere-clm:make-rand-interp 0.0 1.0))
;;; store oscil
           (setf (aref sin-arr b) (cudere-clm:make-oscil 0.0))))
       (if band-noise
	   (loop 
	     for n in bands
	     for b from 0 
	     do
	        (let* ((f-low (nth n ats-cuda::*ats-critical-band-edges*)) 
		       (f-up (nth (1+ n) ats-cuda::*ats-critical-band-edges*))
		       (bw (- f-up f-low))
		       (fc (+ f-low (* 0.5 bw))))
	          (if (> (+ fc bw) nyquist)
		      (setf bw (- nyquist fc)))
	  ;;; store energy envelope
	          (if (not time-ptr)
		      (setf (aref band-env-arr b)
		            (make-env :envelope (ats-cuda::ats-make-envelope sound b 'band-energy duration)
				      :duration dur)))
	  ;;; store noise
	          (setf (aref band-noi-arr b) (cudere-clm:make-rand-interp bw 1.0))
	  ;;; store oscil
	          (setf (aref band-sin-arr b) (cudere-clm:make-oscil fc))))))
      (out (sine 440 .3)))))

(let* (dur n-pars par frames band-noise n-bands bands nyquist window-size noienv ptrenv amp-arr frq-arr eng-arr noi-arr sin-arr band-env-arr band-noi-arr band-sin-arr out-val noi-val frm))

(dsp! sin-noi-cuda
    (start-time (sound ats-cuda::ats-sound) amp-scale (amp-env (or list boolean))
                frq-scale (duration (or sample boolean))
                (time-ptr (or list boolean)) (par boolean) (noise-env (or list boolean))
                (noise-only boolean) (band-noise boolean))
  (:defaults 0.0 (incudine:incudine-missing-arg "ATS-SOUND") 1.0 '(0 1 1 1) 1.0 nil nil nil '(0 1 1 1) nil t)
  
  (let* ((dur nil) (n-pars nil) (par nil)
         (frames nil) (band-noise nil)
         (n-bands nil)
         (bands nil)
         (nyquist nil)
         (window-size nil)
         (noienv nil)
         (ptrenv nil)
         (amp-arr nil)
         (frq-arr nil)
         (eng-arr nil)
         (noi-arr nil)
         (sin-arr nil)
         (band-env-arr nil)
         (band-noi-arr nil)
         (band-sin-arr nil)
         (out-val nil)
         (noi-val nil)
         (frm nil))
    (initialize
     (format t "Inititalizing...~%")
     (if (not (ats-cuda::ats-sound-energy sound)) (error "Sound has no noise energy!~%"))
     (let*
         ((dur (if duration duration (ats-cuda::ats-sound-dur sound)))
          (n-pars (if par (list-length par) (ats-cuda::ats-sound-partials sound)))
          (par (make-array n-pars :initial-contents (if par par (loop for i from 0 below n-pars collect i))))
          (frames (ats-cuda::ats-sound-frames sound))
          (band-noise (and band-noise (if (ats-cuda::ats-sound-band-energy sound) T NIL)))
          (n-bands (if band-noise (length (ats-cuda::ats-sound-bands sound))))
          (bands (if band-noise (coerce (ats-cuda::ats-sound-bands sound) 'list)))
          (nyquist (* 0.5 (ats-cuda::ats-sound-sampling-rate sound)))
          (window-size (ats-cuda::ats-sound-window-size sound))
          (ampenv (cudere-clm:make-env :envelope amp-env :duration dur :scaler amp-scale))
          (noienv (cudere-clm:make-env :envelope noise-env :duration dur :scaler 1.0))
          (ptrenv (if time-ptr (cudere-clm:make-env :envelope time-ptr :duration dur :scaler (1- frames))))
          (amp-arr (unless (or time-ptr noise-only) (make-array n-pars)))
          (frq-arr (unless time-ptr (make-array n-pars)))
          (eng-arr (unless time-ptr (make-array n-pars)))
          (noi-arr (make-array n-pars))
          (sin-arr (make-array n-pars))
          (band-env-arr (if (and (not time-ptr) band-noise) (make-array n-bands)))
          (band-noi-arr (if band-noise (make-array n-bands)))
          (band-sin-arr (if band-noise (make-array n-bands)))
          (out-val 0.0)
          (noi-val 0.0)
          (frm 0.0)))
     ))
  (out (sine 440 .3)))



(declaim (inline make-incudine-env))
(defun make-incudine-env (env &rest args)
  (loop
    for last = 0 then time
    for (time value) on env by #'cddr
    collect time into times
    collect value into values
    finally (return (apply #'make-envelope values (cdr (ou:differentiate times)) args))))


(declaim (inline make-clm-env))
in-noi(defun* make-clm-env (breakpoint-list (scaler 1.0) duration (offset 0.0)
                      base end length)
  (breakpoints->env breakpoint-list :scaler scaler :offset offset
                    :base base
                    :duration (or duration
                                  (and end (* end *sample-duration*))
                                  (and length (* length *sample-duration*)))))


(dsp! sin-noi-cuda
    (start-time (sound ats-cuda::ats-sound) amp-scale (amp-env (or list boolean))
                frq-scale (duration (or sample boolean))
                (time-ptr (or list boolean)) (par boolean) (noise-env (or list boolean))
                (noise-only boolean) (band-noise boolean))
  (:defaults 0.0 (incudine:incudine-missing-arg "ATS-SOUND") 1.0 '(0 1 1 1) 1.0 nil nil nil '(0 1 1 1) nil t)
  
  (let* ((dur nil) (n-pars nil) (par nil)
         (frames nil) (band-noise nil)
         (n-bands nil)
         (bands nil)
         (nyquist nil)
         (window-size nil)
         (ampenv nil)
         (noienv nil)
         (ptrenv nil)
         (amp-arr nil)
         (frq-arr nil)
         (eng-arr nil)
         (noi-arr nil)
         (sin-arr nil)
         (band-env-arr nil)
         (band-noi-arr nil)
         (band-sin-arr nil)
         (out-val nil)
         (noi-val nil)
         (frm nil))
    (initialize
     (format t "Inititalizing...~%")
     (if (not (ats-cuda::ats-sound-energy sound)) (error "Sound has no noise energy!~%"))
     (setf dur (if duration duration (ats-cuda::ats-sound-dur sound)))
     (setf n-pars (if par (list-length par) (ats-cuda::ats-sound-partials sound)))
     (setf par (make-array n-pars :initial-contents (if par par (loop for i from 0 below n-pars collect i))))
     (setf frames (ats-cuda::ats-sound-frames sound))
     (setf band-noise (and band-noise (if (ats-cuda::ats-sound-band-energy sound) T NIL)))
     (setf n-bands (if band-noise (length (ats-cuda::ats-sound-bands sound))))
     (setf bands (if band-noise (coerce (ats-cuda::ats-sound-bands sound) 'list)))
     (setf nyquist (* 0.5 (ats-cuda::ats-sound-sampling-rate sound)))
     (setf window-size (ats-cuda::ats-sound-window-size sound))
;;     (setf ampenv (make-clm-env amp-env :duration dur :scaler amp-scale))
     (setf noienv (make-clm-env noise-env :duration dur :scaler 1.0))
     (setf ptrenv (if time-ptr (make-clm-env time-ptr :duration dur :scaler (1- frames))))
     (setf amp-arr (unless (or time-ptr noise-only) (make-array n-pars)))
     (setf frq-arr (unless time-ptr (make-array n-pars)))
     (setf eng-arr (unless time-ptr (make-array n-pars)))
     (setf noi-arr (make-array n-pars))
     (setf sin-arr (make-array n-pars))
     (setf band-env-arr (if (and (not time-ptr) band-noise) (make-array n-bands)))
     (setf band-noi-arr (if band-noise (make-array n-bands)))
     (setf band-sin-arr (if band-noise (make-array n-bands)))
     (setf out-val 0.0)
     (setf noi-val 0.0)
     (setf frm 0.0)
     (format t "~a ~a" dur ampenv)
     )
    (stereo 
     (* (envelope amp-env 1 1 #'free) (sine 440 .3)))))


(incudine:make-buffer 200 :initial-contents (loop for x below 200 collect 0.0d0))

(defparameter *test-array* (foreign-alloc-sample 200))

(dotimes (i 200)
  (setf (smp-ref *test-array* i) 0.3d0))


(smp-ref *test-array* 10)


(setf (foreign-free *test-array*))


(with-foreign-array)

(incudine:make-foreign-)

(in-package :incudine)

(dsp! sin-noi-cuda
    (start-time (sound ats-cuda::ats-sound) amp-scale (amp-env (or list boolean))
                frq-scale (duration (or sample boolean))
                (time-ptr (or list boolean)) (par boolean) (noise-env (or list boolean))
                (noise-only boolean) (band-noise boolean))
  (:defaults 0.0 (incudine:incudine-missing-arg "ATS-SOUND") 1.0 '(0 1 1 1) 1.0 nil nil nil '(0 1 1 1) nil t)
  (with
      ((dur (if duration duration (ats-cuda::ats-sound-dur sound)))
       (n-pars (if par (list-length par) (ats-cuda::ats-sound-partials sound)))
       (par (make-array n-pars :initial-contents (if par par (loop for i from 0 below n-pars collect i))))
       (frames (ats-cuda::ats-sound-frames sound))
       (band-noise (and band-noise (if (ats-cuda::ats-sound-band-energy sound) T NIL)))
       (n-bands (if band-noise (length (ats-cuda::ats-sound-bands sound))))
       (bands (if band-noise (coerce (ats-cuda::ats-sound-bands sound) 'list)))
       (nyquist (* 0.5d0 (ats-cuda::ats-sound-sampling-rate sound)))
       (window-size (ats-cuda::ats-sound-window-size sound))
       (ampenv (make-clm-env amp-env :duration dur :scaler amp-scale))
       (noienv (make-clm-env noise-env :duration dur :scaler 1.0))
       (ptrenv (if time-ptr (make-clm-env time-ptr :duration dur :scaler (1- frames))))
       (amp-arr (unless (or time-ptr noise-only) (make-array n-pars)))
       (frq-arr (unless time-ptr (make-array n-pars)))
       (eng-arr (unless time-ptr (make-array n-pars)))
       (noi-arr (make-array n-pars))
       (sin-arr (make-array n-pars))
       (band-env-arr (if (and (not time-ptr) band-noise) (make-array n-bands)))
       (band-noi-arr (if band-noise (make-array n-bands)))
       (band-sin-arr (if band-noise (make-array n-bands)))
       (out-val 0.0d0)
       (noi-val 0.0d0)
       (frm 0.0d0))
    (declare
     (type sample dur nyquist out-val noi-val frm)
     (type fixnum n-pars n-bands window-size)
     (type list bands)
     (type function ampenv noienv ptrenv)
     (type simple-array amp-arr frq-arr eng-arr noi-arr sin-arr band-env-arr band-noi-arr band-sin-arr))
    (initialize
     (format t "Inititalizing...~%")
     (if (not (ats-cuda::ats-sound-energy sound)) (error "Sound has no noise energy!~%"))
;;;     (format t "~a ~a" dur ampenv)
     )
    (break "~a" ampenv)
    (stereo 
     (* 1.0 (sine 440 .3)))))

(sin-noi-cuda 0 ats-cuda::cl :amp-env '(0 1 1 1))



(rt-start)

(rt-stop)


(type-of '(1 2 3 4))

(dsp! sin-noi-cuda2 ((sound ats-cuda::ats-sound) (amp-scale sample)
                     (partials list)
                    (amp-env (or list boolean)) (duration (or sample boolean)))
  (with
      ((dur (if duration duration (ats-cuda::ats-sound-dur sound)))
       (ampenv nil)
       (pars (or partials (loop for x below (ats-cuda::ats-sound-partials sound) collect x)))
       (amp-arr (init-only
                 (format t "Inititalizing...~a~%" pars)
                 (make-array (length pars) :element-type 'buffer :initial-contents
                             (loop for p in pars
                                   collect (ats-cuda::array->buffer (aref (ats-cuda::ats-sound-amp sound) p)))))))
    (declare
     (type sample dur)
     (type cons pars)
     (type (or function boolean) ampenv)
     (type ((simple-array 'buffer '(*)) amp-arr))
     )
    (initialize
     (format t "Inititalizing...~a~%" pars)
     (if (not (ats-cuda::ats-sound-energy sound)) (error "Sound has no noise energy!~%"))
;;;     (format t "~a ~a" dur ampenv)
     )
    (break "~a" amp-arr)
    (stereo 
     (* 1.0 (sine 440 .3)))))

(declaim (inline calc-frame))
(defun calc-frame (timeptr duration hop-inc)
  (declare (type sample timeptr duration hop-inc))
  (/ (* timeptr duration) hop-inc))

(calc-frame 0.5d0 )

(type-of (make-buffer 1))

(defvar *empty-buffer* (make-buffer 1))

(free empty-buffer)

(make-buffer)

(defstruct ats-sound
  (float-frames 0.0d0 :type sample)
  (amp (make-array 1 :initial-contents (list empty-buffer)) :type (array buffer (*)))
  (freq (make-array 1 :initial-contents (list empty-buffer)) :type (array buffer (*)))
  (noi (make-array 1 :initial-contents (list empty-buffer)) :type (array buffer (*)))
  (noise-bands (make-array 1 :initial-contents (list empty-buffer)) :type (array buffer (*)))
  (noise-energy (make-array 1 :initial-contents (list empty-buffer)) :type (array buffer (*))))

(+ (* sine amp-val)
   (if (> eng-val 0.0)
       (* sine noise (compute-noi-gain eng-val window-size))
       0.0))


(define-vug sin-ats (time-ptr (ats-cuda::ats-sound sound) (partials list))
  (with-samples
      ((out 0.0d0)))
  (with-samples ((read-pos (* timeptr (float-frames sound)))))
  (dolist (idx partials)
    (incf out (* (buffer-read (aref (sound-amp sound)) idx)
                 (sine (aref (sound-freq sound) idx)))))
  out)

(type-of sine)

(phasor)





(sine)

(declaim (inline %phasor-n))
(define-vug %phasor-n ((phase-array (simple-array sample *))
                       (n integer) rate end)
  (prog1 (sample (aref phase-array n))
    (incf (aref phase-array n) rate)
    (cond ((>= (aref phase-array n) end) (decf (aref phase-array n) end))
          ((minusp (aref phase-array n)) (incf (aref phase-array n) end)))))

(declaim (inline phasor-n))
(define-vug phasor-n ((phase-array (simple-array sample *))
                         (n integer) freq)
     "Produce a normalized moving phase value with frequency FREQ and
initial value INIT (0 by default).
phase-array is an array of phase values, n indexes into it."
     (:defaults (make-array 1 :element-type 'sample :initial-contents '(0.0d0)) 1 0)
     (with-samples ((rate (* freq *sample-duration*)))
       (%phasor-n phase-array n rate 1)))

(declaim (inline sine-n))
(define-vug sine-n ((phase-array (simple-array sample *)) (n integer) freq amp
                    (sin-phase-array (simple-array sample *)))
  "High precision array of sine wave oscillators with frequency FREQ, amplitude
AMP and phase array PHASES."
  (:defaults (make-array 1 :element-type 'sample :initial-contents '(0.0d0))
             1 440 1
             (make-array 1 :element-type 'sample :initial-contents '(0.0d0)))
  (* amp (sin (+ (* +twopi+ (phasor-n (the (simple-array sample *) phase-array) n freq)) (aref sin-phase-array n)))))

(dsp! test3 ((freqs (simple-array sample *))
             (amps (simple-array sample *)))
  (:defaults (make-array 2 :element-type 'sample :initial-contents '(440.0d0 880.0d0))
             (make-array 2 :element-type 'sample :initial-contents '(0.05d0 0.03d0)))
  (with ((out 0.0d0)
         (phase-array (init-only (make-array 2 :element-type 'sample :initial-element 0.0d0)))
         (sine-phase-array (init-only (make-array 2 :element-type 'sample :initial-element 0.0d0))))
    (declare (type sample out)
             (type (array sample *) phase-array sine-phase-array))
    (setf out 0.0d0)
    (loop
      for n below (length freqs)
      do (incf out (sine-n phase-array n (aref freqs n) (aref amps n) sine-phase-array)))
    (stereo out)))

(test3 (make-array 3 :element-type 'sample :initial-contents '(440.0d0 660.0d0 1250.0d0))
       (make-array 3 :element-type 'sample :initial-contents '(0.05d0 0.02d0 0.01d0)))

(free 1)

(free 0)
(test3 )

(dsp! test3 ((freqs (simple-array sample *))
             (amps (simple-array sample *)))
  (:defaults (make-array 2 :element-type 'sample :initial-contents '(240.0d0 610.0d0))
             (make-array 2 :element-type 'sample :initial-contents '(0.1d0 0.1d0)))
  (with ((out 0.0d0)
         (phases (init-only (make-array 2 :element-type 'sample :initial-element 0.0d0)))
         (sine-phases (init-only (make-array 2 :element-type 'sample :initial-element 0.0d0))))
    (declare (type sample out)
             (type (array sample *) phases sine-phases))
    (setf out 0.0d0)
    (loop
      for n from 1 below 2
      do (incf out (sine-n phases n (aref freqs n) (aref amps n)
                           sine-phases)))
    (stereo out)))

(test3)
(sine-n phases n (aref freqs n) (aref amps n) sine-phases)

(let ((arr (make-array 2 :element-type 'sample :initial-contents '(440.0d0 610.0d0))))
  (aref arr 0))



(let* ((phases (make-array 2 :element-type 'double-float :initial-contents '(0.0d0 0.0d0)))
       (phase (aref phases 0)))
  (incf (aref phases 0) 0.1d0)
  phases)

(define-vug sin-test ((freqs (simple-array 'double-float *))
                      (amps (simple-array 'double-float *)))
  (with-samples
      ((out 0.0d0)
       (phases (init-only (make-array 2 :element-type 'sample :initial-element 0.0d0)))
       (sine-phases (init-only (make-array 2 :element-type 'sample :initial-element 0.0d0))))
    (setf out 0.0d0)
    (loop
      for n below 2
      do (incf out (sine-n phases n (aref freqs n) (aref amps n) sine-phases)))
    out))




(set-control)

(define-vug sin-noi-ats (timeptr (sound ats-sound) (partials list))
  (with-samples
      ((out 0.0d0)
       (sines (init-only (make-array (length partials)
                                     :initial-contents (loop repeat (length partial) collect (sine :freq 0)))))
       (read-pos (* timeptr (float-frames sound))))
    (loop for p-idx in partials
          for sinesig in sines
      (setf (sinesig) (sine (aref (sound-freq sound) idx)))
      (incf out (* (buffer-read (aref (sound-amp sound)) idx)
                   sinesig)))
    out))

(dsp! sin-noi-cuda2 ((sound ats-cuda::ats-sound) (amp-scale sample)
                     (partials list)
                    (amp-env (or list boolean)) (duration (or sample boolean)))
  (with
      ((dur (if duration duration (ats-cuda::ats-sound-dur sound)))
       (ampenv nil)
       (pars (or partials (loop for x below (ats-cuda::ats-sound-partials sound) collect x)))
       (amp-arr (init-only
                 (format t "Inititalizing...~a~%" pars)
                 (make-array (length pars) :element-type 'buffer :initial-contents
                             (loop for p in pars
                                   collect (ats-cuda::array->buffer (aref (ats-cuda::ats-sound-amp sound) p)))))))
    (declare
     (type sample dur)
     (type cons pars)
     (type (or function boolean) ampenv)
     (type ((simple-array 'buffer '(*)) amp-arr))
     )
    (initialize
     (format t "Inititalizing...~a~%" pars)
     (if (not (ats-cuda::ats-sound-energy sound)) (error "Sound has no noise energy!~%"))
;;;     (format t "~a ~a" dur ampenv)
     )
    (break "~a" amp-arr)
    (stereo 
     (* 1.0 (sine 440 .3)))))


(sin-noi-cuda2 ats-cuda::cl 1.0d0 nil '(0 1 1 1) nil)


(dsp! sin-noi-cuda ((sound ats-cuda::ats-sound) (amp-scale sample)
                    (amp-env (or list boolean)) (duration (or sample boolean)))
  (stereo (sine 440 .3)))

(sin-noi-cuda2 ats-cuda::cl 1.0d0 nil '(0 1 1 1) nil)


(type-of (make-array 10 :element-type 'sample))

(simple-array double-float (10))

(dsp! test2 ()
  (stereo (sine 440 .3)))

(test2)
(type-of (coerce 3 'fixnum))



(define-vug %phasor (rate init end)
  (with-samples ((phase init))
    (prog1 phase
      (incf phase rate)
      (cond ((>= phase end) (decf phase end))
            ((minusp phase) (incf phase end))))))

(define-vug phasor (freq init)
  "Produce a normalized moving phase value with frequency FREQ and
initial value INIT (0 by default)."
  (:defaults 1 0)
  (with-samples ((rate (* freq *sample-duration*)))
    (%phasor rate init 1)))

(define-vug sine (freq amp phase)
  "High precision sine wave oscillator with frequency FREQ, amplitude
AMP and PHASE."
  (:defaults 440 1 0)
  (* amp (sin (+ (* +twopi+ (phasor freq 0)) phase))))

(dsp! test2 ()
  (stereo (sine 440 0.1)))

(test2)


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
  (let* ((dur (double (if duration duration (ats-sound-dur sound))))
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
	 (band-sin-arr (if band-noise (make-array n-bands))))
    (declare (type incudine::sample dur)
             (type fixnum window-size n-bands frames n-pars))
    (incudine::with-samples
        ((out-val 0.0d0)
	 (noi-val 0.0d0)
	 (frm 0.0d0))        
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
	   (setf out-val 0.0d0)
	   (setf noi-val (double (env noienv)))
	   (if time-ptr (setf frm (double (env ptrenv))))
	   (loop for j from 0 below n-pars do
	     (let* ((p (aref par j))
                    (amp-val (double
                              (if (not noise-only) 
				  (if time-ptr (get-amp-f sound p frm) (env (aref amp-arr j)))
			          0.0d0)))
		    (frq-val (double
                              (if time-ptr
                                  (* frq-scale (get-frq-f sound p frm))
                                  (env (aref frq-arr j)))))
		    (eng-val (double (if time-ptr
                                         (get-energy-f sound p frm)
                                         (env (aref eng-arr j)))))
		    (bw (double (if (< frq-val 500.0) 50.0d0
			            (* frq-val 0.1d0))))
		    (sine (oscil (aref sin-arr j) (hz->radians frq-val)))
		    (noise (* noi-val (rand-interp (aref noi-arr j) (hz->radians bw)))))
               (declare (type incudine::sample amp-val frq-val eng-val bw sine noise))
               (incf out-val 
		     (+ (* sine amp-val)
			(if (> eng-val 0.0)
			    (* sine noise (compute-noi-gain eng-val window-size))
			    0.0)))))
	   (if band-noise
	       (loop for j from 0 below n-bands do
		 (let* ((gain-val (if time-ptr (get-band-energy-f sound j frm)
                                      (env (aref band-env-arr j)))))
                   (declare (type incudine::sample gain-val))
		   (if (> gain-val 0.0d0)
		       (progn
			 (setf gain-val (compute-noi-gain gain-val window-size))
			 (incf out-val (* gain-val noi-val
					  (oscil (aref band-sin-arr j))
					  (rand-interp (aref band-noi-arr j)))))))))
	   (outa i (* (env ampenv) out-val))))))))

;;;; working:

(define-vug ats-noise-bank (pos
                            (freqs (simple-array sample))
                            (bws (simple-array sample))
                            (amps (simple-array sample)))
  "a bank of band-limited noise with freqs, amps and bandwidths (in Hz)
provided as arrays of type sample. The length of the freqs array
determines the number of noise bands to generate in parallel and the
size of the other two arrays has to be >= the size of the freqs
array."
  (with-samples ((out 0) (idx 0))
    (with-sample-arrays
        ((sin-phase-array (sample-array (length freqs))))
      (setf out 0.0d0)
      (setf idx pos)
      (dotimes (n (length freqs))
        (incf out (* (sine-n n (aref freqs n) 1.0d0 sin-phase-array)
                     (i-aref-n amps n idx)
                     (randi-n n bws))))
      out)))

(dsp! nb01 (pos
            (noise-cfreqs (simple-array sample *))
            (noise-bws (simple-array sample *))
            (amps (simple-array sample *)))
  (stereo (ats-noise-bank pos noise-cfreqs noise-bws amps)))

(nb01
 9.0d0
 (get-noise-c-freqs (ats-cuda::ats-sound-bands ats-cuda::cl))
 (get-noise-bws (ats-cuda::ats-sound-bands ats-cuda::cl))
 (vec->array (ats-cuda::ats-sound-band-energy ats-cuda::cl)))

(dsp! nb02 ((noise-cfreqs (simple-array sample *))
            (noise-bws (simple-array sample *))
            (amps (simple-array sample *)))
  (with-samples ((timeline (line 0 253 2.5 #'free))
                 (pos 0.0d0))
    (setf pos timeline)
    (stereo (* 15 (ats-noise-bank pos noise-cfreqs noise-bws amps)))))

(nb02
 (get-noise-c-freqs (ats-cuda::ats-sound-bands ats-cuda::cl))
 (get-noise-bws (ats-cuda::ats-sound-bands ats-cuda::cl))
 (vec->array (ats-cuda::ats-sound-band-energy ats-cuda::cl))
)

;;; (typep (make-array '(2 2) :element-type 'double-float) '(simple-array sample))

(define-vug ats-noise-bank (timeptr
                            (noise-cfreqs (simple-array sample))
                            (noise-bws (simple-array sample))
                            (noise-energy (simple-array sample)))
  (with-samples ((out 0))
    (with ((num-bands (length noise-bws)))
      (declare (type integer num-bands))
      (with-sample-arrays
          ((sin-phase-array (sample-array (length noise-cfreqs))))
        (setf out 0.0d0)
        (dotimes (n num-bands)
          (incf out (* (sine-n n (aref noise-cfreqs n) 1.0d0 sin-phase-array)
                       (i-aref-n noise-energy n timeptr)
                       (randi-n n noise-bws))))))
    out))



(define-vug ats-master-vug
    (timeptr
     (freqs (simple-array sample))
     (amps (simple-array sample))
     (pnoi (simple-array sample))
     (noise-bws (simple-array sample))
     (noise-cfreqs (simple-array sample))
     (noise-energy (simple-array sample))
     (partials list)
     (fmod (simple-array sample))
     (amod (simple-array sample))
     res-bal)
  (:defaults 0
             (incudine:incudine-missing-arg "FREQS")
             (incudine:incudine-missing-arg "AMPS")
             (incudine:incudine-missing-arg "PNOI")
             (incudine:incudine-missing-arg "NOISE-BWS")
             (incudine:incudine-missing-arg "NOISE-CFREQS")
             (incudine:incudine-missing-arg "NOISE_ENERGY")
             nil (sample-array 1) (sample-array 1) 0.5)
  (initialize
;;;   (format t "~&~a~&~a~&~a~%"  noise-energy noise-bws noise-cfreqs)
   )
  (+
   (ats-sine-noi-bank timeptr freqs amps pnoi fmod amod partials res-bal)
     
   (* (res-level res-bal) (ats-noise-bank timeptr noise-cfreqs noise-bws noise-energy))))

(+
 (ats-sine-noi-bank timeptr freqs amps pnoi fmod amod partials res-bal)
       (* (res-level res-bal) (ats-noise-bank timeptr noise-cfreqs noise-bws noise-energy)))



(defparameter *array-test* (make-array '(3 3) :initial-contents
                                       '((1 2 3) (4 5 6) (7 8 9))))

(defun array-slice (arr row)
    (make-array (array-dimension arr 1) 
      :displaced-to arr 
      :displaced-index-offset (* row (array-dimension arr 1))))

(setf (aref (array-slice *array-test* 0) 1) 24)

(time
 (bounce-to-disk ("/tmp/test.wav" :channels 1 :duration 2.52)
   (sin-noi-synth ats-cuda::cl)))

(sin-noi-synth 0.0 ats-cuda::cl :amp-scale 0.1 :duration 25.2 :time-ptr '(0 0 0.2 0.02 1 1))

(sin-noi-synth 0.0 ats-cuda::cl :amp-scale 0.1 :duration 25.2 :time-ptr '(0 0 0.2 0.02 1 1))

(let ((ats-sound ats-cuda::cl))
  (vec->array (ats-cuda::ats-sound-frq ats-sound)))

(type-of (ats-cuda::ats-sound-frq ats-cuda::cl))

(declaim (inline i-aref-nv))
(defun i-aref-nv (array n idx)
  "linearly interpolated array indexing."
  (declare (type real idx)
           (type positive-fixnum n)
           (type simple-vector array))
  (multiple-value-bind (lo ratio) (floor idx)
    (+ (* (- 1 ratio) (aref (aref array n) lo))
       (* ratio (aref (aref array n) (1+ lo))))))

(i-aref-nv (ats-cuda::ats-sound-frq ats-cuda::cl) 10 3.4)

(define-vug ats-sine-noi-bank (timeptr
                               (freqs simple-vector)
                               (amps (simple-array sample))
                               (pnoi (simple-array sample))
                               (fmod (simple-array sample))
                               (amod (simple-array sample))
                               (partials list)
                               res-bal)
  (with-samples ((out 0)
                 (sine-sig 0.0)
                 (sin-level 1)
                 (res-level 1))
    (with-sample-arrays ((pbws (sample-array (array-dimension freqs 0)))
                         (sin-phase-array (sample-array (array-dimension freqs 0))))

      (initialize
       (break "~a" (i-aref-nv (ats-cuda::ats-sound-frq ats-cuda::cl)
           10 3.4)))
      (setf out 0.0d0)
      (setf sin-level (sin-level res-bal))
      (setf res-level (res-level res-bal))
      (dolist (partial partials)
        (let* (
               (freq (* (aref fmod partial)
;;                        (i-aref-nv freqs partial timeptr)
                        ))
               (amp (aref amod partial))
               (sine (sine-n partial freq amp sin-phase-array))
               )
          (setf sine-sig sine)
          (setf (aref pbws partial) (if (< freq 500.0) 50.0d0 (* freq 0.1d0)))
          (incf out (+ (* sin-level
                          (i-aref-n amps partial timeptr)
                          sine-sig)
                       (* res-level
                          (i-aref-n pnoi partial timeptr)
                          sine
                          (randi-n partial pbws))))))
      out)))

(define-vug ats-master-vug
    (timeptr
     (freqs simple-vector)
     (amps (simple-array sample))
     (pnoi (simple-array sample))
     (noise-bws (simple-array sample))
     (noise-cfreqs (simple-array sample))
     (noise-energy (simple-array sample))
     (partials list)
     (fmod (simple-array sample))
     (amod (simple-array sample))
     res-bal)
  (:defaults 0
             (incudine:incudine-missing-arg "FREQS")
             (incudine:incudine-missing-arg "AMPS")
             (incudine:incudine-missing-arg "PNOI")
             (incudine:incudine-missing-arg "NOISE-BWS")
             (incudine:incudine-missing-arg "NOISE-CFREQS")
             (incudine:incudine-missing-arg "NOISE_ENERGY")
             nil (sample-array 1) (sample-array 1) 0.5)
  (+ (ats-sine-noi-bank timeptr freqs amps pnoi fmod amod partials res-bal)
     (* (res-level res-bal) (ats-noise-bank timeptr noise-cfreqs noise-bws noise-energy))))

(dsp! sin-noi-synth
    ((start-time real)
     (ats-sound ats-cuda::ats-sound)
     (amp-scale (or null real))
     (frq-scale (or null real))
     (duration (or null real))
     (time-ptr (or null list))
     (par (or null list))
     (noise-env (or null list))
     (noise-only boolean)
     (band-noise boolean))
  (:defaults 0 (incudine:incudine-missing-arg "ATS_SOUND") 1 1 nil nil nil nil nil t)
  (with ((start-frm
          (round
           (* start-time
              (/ (ats-cuda::ats-sound-sampling-rate ats-sound)
                 (ats-cuda::ats-sound-frame-size ats-sound)))))
         (scale (- (1- (ats-cuda::ats-sound-frames ats-sound)) start-frm))
         (dur (or duration (- (ats-cuda::ats-sound-dur ats-sound) start-time))))
    (with-samples ((curr-amp (sample (or amp-scale 1.0d0)))
                   (curr-frq-scale (sample (or frq-scale 1.0d0)))
                   (timeptr (envelope
                             (make-clm-env
                              (or time-ptr '(0 0 1 1))
                              :scaler scale
                              :offset start-frm
                              :duration dur)
                             :done-action #'free))
                   (noise-amp (envelope
                               (make-clm-env
                                (or noise-env '(0 1 1 1))
                                :duration dur)
                               :done-action #'free))
                   idx)
      (with ((num-partials (length (ats-cuda::ats-sound-frq ats-sound)))
             (partials (or par (range num-partials))))
        (declare (type list partials)
                 (type integer num-partials))
        (setf idx timeptr)
        (stereo (ats-master-vug-compat
                 timeptr
                 (ats-cuda::ats-sound-frq ats-sound)
                 (vec->array (ats-cuda::ats-sound-amp ats-sound))
                 (vec->array (ats-cuda::ats-sound-energy ats-sound))
                 (get-noise-bws (ats-cuda::ats-sound-bands ats-sound))
                 (get-noise-c-freqs (ats-cuda::ats-sound-bands ats-sound))
                 (vec->array (ats-cuda::ats-sound-band-energy ats-sound))
                 partials
                 (sample-array num-partials :initial-element curr-amp)
                 (sample-array num-partials :initial-element curr-frq-scale)
                 noise-amp
                 noise-only
                 band-noise))))))





(let ((ats-sound ats-cuda::cl)
      duration)
  (* (/ (* (1- (ats-cuda::ats-sound-frames ats-sound))
           (/ (ats-cuda::ats-sound-frame-size ats-sound)
              (ats-cuda::ats-sound-sampling-rate ats-sound)))
              (ats-cuda::ats-sound-dur ats-sound))
           (or duration (ats-cuda::ats-sound-dur ats-sound)))
  )


(progn
  (define-vug ats-sine-noi-bank (timeptr
                               (freqs (simple-array sample))
                               (amps (simple-array sample))
                               (pnoi (simple-array sample))
                               (fmod (simple-array sample))
                               (amod (simple-array sample))
                               (partials list)
                               res-bal)
  (with-samples ((out 0)
                 (sine-sig 0.0)
                 (sin-level 1)
                 (res-level 1))
    (with-sample-arrays ((pbws (sample-array (array-dimension freqs 0)))
                         (sin-phase-array (sample-array (array-dimension freqs 0))))

      ;; (initialize
      ;;  (break "~&~a~&~a" pbws partials))
      (setf out 0.0d0)
      (setf sin-level (sin-level res-bal))
      (setf res-level (res-level res-bal))
      (dolist (partial partials)
        (let* (
               (freq (* (aref fmod partial)
                        (i-aref-n freqs partial timeptr)))
               (amp (aref amod partial))
               (sine (sine-n partial freq amp sin-phase-array))
               )
          (setf sine-sig sine)
          (setf (aref pbws partial) (if (< freq 500.0) 50.0d0 (* freq 0.1d0)))
          (incf out (+ (* sin-level
                          (i-aref-n amps partial timeptr)
                          sine-sig)
                       (* res-level
                          (i-aref-n pnoi partial timeptr)
                          sine
                          (randi-n partial pbws))))))
      out)))
  (dsp! sin-noi-synth
      ((ats-sound ats-cuda::ats-sound)
       (amp-scale (or null float))
       (frq-scale (or null float))
       (duration (or null float))
       (time-ptr (or null list))
       (par (or null list))
       (noise-env (or null list))
       (noise-only boolean)
       (band-noise boolean))
    (:defaults (incudine:incudine-missing-arg "ATS_SOUND") 1.0 1.0 nil nil nil nil nil t)
    (with-samples ((dur (sample (or duration (ats-cuda::ats-sound-dur ats-sound))))
                   (curr-amp 1.0d0)
                   (timeptr (line 0.0d0 (sample (ats-cuda::ats-sound-frames ats-sound)) dur #'free))
                   idx)
      (with ((num-partials (length (ats-cuda::ats-sound-frq ats-sound)))
             (partials (or par (range num-partials))))
        (declare (type list partials)
                 (type integer num-partials))
        (setf idx timeptr)
        (stereo (ats-master-vug
                 timeptr
                 (vec->array (ats-cuda::ats-sound-frq ats-sound))
                 (vec->array (ats-cuda::ats-sound-amp ats-sound))
                 (vec->array (ats-cuda::ats-sound-energy ats-sound))
                 (get-noise-bws (ats-cuda::ats-sound-bands ats-sound))
                 (get-noise-c-freqs (ats-cuda::ats-sound-bands ats-sound))
                 (vec->array (ats-cuda::ats-sound-band-energy ats-sound))
                 partials
                 (sample-array num-partials :initial-element curr-amp)
                 (sample-array num-partials :initial-element 1.0d0)))))))

(typep (make-array 16 :element-type 'double-float :initial-element 0.0d0) '(simple-array double-float))

(sin-noi-synth ats-cuda::cl)

;;; Berechnung der Dauer des line vugs:
(* (/ (* (1- frames (/ frame-size sampling-rate)))
      snd-duration)
   duration)

(sin-noi-synth ats-cuda::cl)

(coerce  '(simple-array sample))

(type-of (get-noise-bws (ats-cuda::ats-sound-bands ats-cuda::cl)))
(let ((ats-sound ats-cuda::cl))
  (type-of
   (vec->array (ats-cuda::ats-sound-band-energy ats-sound))))

(let ((elems ))
  (sample-array (length (ats-cuda::ats-sound-bands ats-cuda::cl))
                :initial-contents (ats-cuda::ats-sound-bands ats-cuda::cl)))

(let* ((ats-sound ats-cuda::cl)
       (arr (make-array
                      4
                      :element-type `(simple-array sample
                                      (* *))
                      :initial-contents
                      (list
                       (vec->array (ats-cuda::ats-sound-frq ats-sound))
                       (vec->array (ats-cuda::ats-sound-amp ats-sound))
                       (vec->array (ats-cuda::ats-sound-energy ats-sound))
                       (vec->array (ats-cuda::ats-sound-band-energy ats-sound)))))
       (ats-noise-band-data
            (make-array
             2
             :element-type `(simple-array sample ,(length (ats-cuda::ats-sound-bands ats-sound)))
             :initial-contents
             (list
              (get-noise-bws (ats-cuda::ats-sound-bands ats-sound))
              (get-noise-c-freqs (ats-cuda::ats-sound-bands ats-sound))))))
  (type-of arr))

  (loop for array across arr
        do (loop for m below (array-dimension array 0)
                 do (loop for n below (array-dimension array 1)
                          do (if (= (aref array m n) 3.4020381869193795d-313)
                                 (break "idx: ~a m: ~a n: ~a" idx m n)))))

(let* ((ats-sound ats-cuda::cl)
       (arr (make-array
                      4
                      :element-type `(simple-array sample
                                      (* *))
                      :initial-contents
                      (list
                       (vec->array (ats-cuda::ats-sound-frq ats-sound))
                       (vec->array (ats-cuda::ats-sound-amp ats-sound))
                       (vec->array (ats-cuda::ats-sound-energy ats-sound))
                       (vec->array (ats-cuda::ats-sound-band-energy ats-sound)))))
       (ats-noise-band-data
            (make-array
             2
             :element-type `(simple-array sample ,(length (ats-cuda::ats-sound-bands ats-sound)))
             :initial-contents
             (list
              (get-noise-bws (ats-cuda::ats-sound-bands ats-sound))
              (get-noise-c-freqs (ats-cuda::ats-sound-bands ats-sound))))))
  (loop for idx below 4
        collect (type-of (aref arr idx))))



(array-dimension (vec->array (ats-cuda::ats-sound-band-energy ats-cuda::cl)) 0)


(typep #((1.0d0)) '(simple-vector double-float *))

(type-of #(1 2 3))

(aref (vec->array (ats-cuda::ats-sound-band-energy ats-cuda::cl)) 0)

(array-rank (vec->array (ats-cuda::ats-sound-band-energy ats-cuda::cl)))

(dsp! sin-synth
    ((start-time real)
     (ats-sound ats-cuda::ats-sound)
     (amp-scale (or null real))
     (amp-env (or null list))
     (frq-scale (or null real))
     (duration (or null real))
     (par (or null list)))
  (:defaults 0 (incudine:incudine-missing-arg "ATS_SOUND") 1 nil 1 nil nil)
  (with ((start-frm
          (round
           (* start-time
              (/ (ats-cuda::ats-sound-sampling-rate ats-sound)
                 (ats-cuda::ats-sound-frame-size ats-sound)))))
         (scale (- (1- (ats-cuda::ats-sound-frames ats-sound)) start-frm))
         (dur (or duration (- (ats-cuda::ats-sound-dur ats-sound) start-time))))
    (with-samples ((curr-amp (sample (or amp-scale 1.0d0)))
                   (curr-frq-scale (sample (or frq-scale 1.0d0)))
                   (timeptr (envelope
                             (make-clm-env
                              '(0 0 1 1)
                              :scaler scale
                              :offset start-frm
                              :duration dur)
                             :done-action #'free))
                   (amp (envelope
                             (make-clm-env
                              (or amp-env '(0 1 1 1))
                              :duration dur)
                             :done-action #'free))
                   idx)
      (with ((num-partials (array-dimension (ats-cuda::ats-sound-frq ats-sound) 0))
             (partials (or par (range num-partials))))
        (declare (type list partials)
                 (type integer num-partials))
        (setf idx timeptr)
        (stereo (* amp
                   (ats-sine-bank
                    idx
                    (ats-cuda::ats-sound-frq ats-sound)
                    (ats-cuda::ats-sound-amp ats-sound)
                    (sample-array num-partials :initial-element 1.0d0)
                    (sample-array num-partials :initial-element 1.0d0)
                    partials)))))))

(dsp! sin-noi-rtc-synth
    ((timeptr real)
     (ats-sound ats-cuda::ats-sound)
     (amp-scale real)
     (par (or null list))
     (fmod (or null (array sample)))
     (amod (or null (array sample)))
     (res-bal real))
  (:defaults 0 (incudine:incudine-missing-arg "ATS_SOUND") 1 nil nil nil 0.5)
  "The synth definition with realtime control.

 <timeptr> indexes into the frames of the ATS synthesis

<fmod> Array of frequency modulation values. The arrayidx relates to
           the ATS partial with the same idx.

<amod> Array of amplitude modulation values. The arrayidx relates to
           the ATS partial with the same idx.

<res-bal> Crossfade between 0 (sine only) and 1 (residual only). Note
           that a pan value of 0.5 results in an amplitude of 1 for
           both, sine and residual component. Higher pan values will
           fade out the sine and lower pan values will fade out the
           residual component.

<par> List of indexes into the partials to synthesize. Can't be
           changed at performance time. Use the maximum number of
           needed residuals here and set the amod of the respective
           partial to 0.0d to mute it at performance time.
"
  (with-samples ((curr-amp (sample (or amp-scale 1.0d0)))
                 idx)
    (with ((num-partials (array-dimension (ats-cuda::ats-sound-frq ats-sound) 0))
           (partials (or par (ats-cuda::range num-partials))))
      (declare (type list partials)
               (type integer num-partials))
      (with-sample-arrays
          ((amp-mod (or amod (sample-array num-partials :initial-element 1.0d0)))
           (frq-mod (or fmod (sample-array num-partials :initial-element 1.0d0))))
        (initialize
         (break "~&~a ~&~a ~&~a" partials amp-mod frq-mod))
        (stereo (sine 440 0.1))))))



(defparameter *deb* nil)

(define-vug ats-master-vug
    (timeptr
     (freqs (simple-array sample))
     (amps (simple-array sample))
     (pnoi (simple-array sample))
     (noise-bws (simple-array sample))
     (noise-cfreqs (simple-array sample))
     (noise-energy (simple-array sample))
     (partials list)
     (fmod (simple-array sample))
     (amod (simple-array sample))
     (res-bal real))
  (:defaults 0.0d0
             (incudine:incudine-missing-arg "FREQS")
             (incudine:incudine-missing-arg "AMPS")
             (incudine:incudine-missing-arg "PNOI")
             (incudine:incudine-missing-arg "NOISE-BWS")
             (incudine:incudine-missing-arg "NOISE-CFREQS")
             (incudine:incudine-missing-arg "NOISE_ENERGY")
             nil (sample-array 1) (sample-array 1) 0.5)
  "Master VUG for the sin-noi-rtc-synth."
  (ats-sine-noi-bank timeptr freqs amps pnoi fmod amod partials res-bal)
)

(define-vug ats-sine-noi-bank (timeptr
                               (freqs (simple-array sample))
                               (amps (simple-array sample))
                               (pnoi (simple-array sample))
                               (fmod (simple-array sample))
                               (amod (simple-array sample))
                               (partials list)
                               (res-bal real))
  "A bank of sine wave plus residual noise oscillators.
   <freqs>, <amps> and <pnoi> are arrays, indicating the frequencies,
   sine-levels and noise-levels of the oscillators.

   <fmod> and <amod> are frequency and amplitude modulation
   arrays.

   <partials> is a list of indexes into the arrays indicating the
   partials to be generated.

   <res-bal> is a crossfade between 0 (sine only) and 1 (residual
   only). Note that a pan value of 0.5 results in an amplitude of 1
   for both, sine and residual component. Higher pan values will fade
   out the sine and lower pan values will fade out the residual
   component.

   All supplied array sizes have to be (>= (length freqs)).
"

  (with-samples ((out 0)
                 (sine-sig 0.0)
                 (sin-level 1)
                 (res-level 1)
                 )
    (setf out 0.0d0)
    ;; (setf sin-level (sin-level res-bal))
    ;; (setf res-level (res-level res-bal))
    ;; (dolist (partial partials)
    ;;   (let* ((freq (* (aref fmod partial)
    ;;                   (i-aref-n freqs partial timeptr)))
    ;;          (amp (aref amod partial))
    ;;          (sine (sine-n partial freq amp sin-phase-array)))
    ;;     (setf sine-sig sine)
    ;;     (setf (aref pbws partial) (if (< freq 500.0) 50.0d0 (* freq 0.1d0)))
    ;;     (incf out (+ (* sin-level
    ;;                     (i-aref-n amps partial timeptr)
    ;;                     sine-sig)
    ;;                  (* res-level
    ;;                     (i-aref-n pnoi partial timeptr)
    ;;                     sine-sig
    ;;                     (randi-n partial pbws))))))
    out))

(dsp! sin-noi-rtc-synth
    ((timeptr real)
     (ats-sound ats-cuda::ats-sound)
     (amp-scale real)
     (par (or null list))
     (fmod (or null (array sample)))
     (amod (or null (array sample)))
     (res-bal real))
  (:defaults 0 (incudine:incudine-missing-arg "ATS_SOUND") 1 nil nil nil 0.5)
  "The synth definition with realtime control.

 <timeptr> indexes into the frames of the ATS synthesis

<fmod> Array of frequency modulation values. The arrayidx relates to
           the ATS partial with the same idx.

<amod> Array of amplitude modulation values. The arrayidx relates to
           the ATS partial with the same idx.

<res-bal> Crossfade between 0 (sine only) and 1 (residual only). Note
           that a pan value of 0.5 results in an amplitude of 1 for
           both, sine and residual component. Higher pan values will
           fade out the sine and lower pan values will fade out the
           residual component.

<par> List of indexes into the partials to synthesize. Can't be
           changed at performance time. Use the maximum number of
           needed residuals here and set the amod of the respective
           partial to 0.0d to mute it at performance time.
"
  (with-samples ((curr-amp (sample (or amp-scale 1.0d0)))
                 idx)
    (with ((num-partials (array-dimension (ats-cuda::ats-sound-frq ats-sound) 0))
           (partials (or par (ats-cuda::range num-partials))))
      (declare (type list partials)
               (type integer num-partials))
      (with-sample-arrays
          ((amp-mod (or amod (sample-array num-partials :initial-element 1.0d0)))
           (frq-mod (or fmod (sample-array num-partials :initial-element 1.0d0))))
        (stereo
         (ats-master-vug
          timeptr
          (ats-cuda::ats-sound-frq ats-sound)
          (ats-cuda::ats-sound-amp ats-sound)
          (ats-cuda::ats-sound-energy ats-sound)
          (get-noise-bws (ats-cuda::ats-sound-bands ats-sound))
          (get-noise-c-freqs (ats-cuda::ats-sound-bands ats-sound))
          (ats-cuda::ats-sound-band-energy ats-sound)
          partials
          frq-mod
          amp-mod
          res-bal))))))

(setf *deb* nil)

(* 440 (expt (/ 880 440) 1.059))

(dsp! xy-snd-amp-ctl ((synth-id (or (unsigned-byte 62) node)))
  (set-control synth-id :soundpos (mouse-x))
  (set-control synth-id :amp-scale (mouse-y)))


(* (res-level res-bal) (ats-noise-bank timeptr noise-cfreqs noise-bws noise-energy))

(+ (ats-sine-noi-bank timeptr freqs amps pnoi fmod amod partials res-bal)
     (* (res-level res-bal)) (ats-noise-bank timeptr noise-cfreqs noise-bws noise-energy))

(defparameter *amod* (sample-array (ats-cuda::ats-sound-partials ats-cuda::cl)
                                   :initial-element 1.0d0))

(defparameter *fmod* (sample-array (ats-cuda::ats-sound-partials ats-cuda::cl)
                                   :initial-element 1.0d0))

(sin-noi-rtc-synth 0.2 ats-cuda::cl :fmod *fmod* :amod *amod* :amp-scale 0.05 :id 1)

(sin-noi-rtc-pstretch-synth 0.2 ats-cuda::cl :fmod *fmod* :pstretch 0 :base-partial 0 :amod *amod* :amp-scale 0.05 :id 2)

(xy-sndpos-amp-ctl 1 :id 3)

(free 3)



(dsp! xy-partial-ctl ((synth-id (or (unsigned-byte 62) node)))
  (set-control synth-id :soundpos (lag (mouse-x) 1))
  (set-control synth-id :amp-scale (lag (mouse-y) 1)))

;;; helper functions for the control of an array of
;;; amplitude-modulation vals for filter-like operations on the
;;; amplitudes of the partials.


(dsp! xy-sndpos-amp-ctl ((synth-id (or (unsigned-byte 62) node)))
  "A dsp to control the pos (mouse-x) and the amplitude (mouse-y) of a
sin-noi-rtc(-stretch)-synth."
  (set-control synth-id :soundpos (lag (mouse-x) 1))
  (set-control synth-id :amp-scale (lag (mouse-y) 1)))

(progn
  (defun n-lin (x min max)
    "normalized linear interpolation of x between min and max."
    (+ min (* (- max min) x)))

  (defun recalc-bw (bw num-partials)
    (n-lin bw 0.5 num-partials))

  (defun recalc-cfreq-pos (pos max)
    (n-lin pos 0 (1- max)))

  (defun bias-cos (cfreq-pos bw num-partials)
    "return a function which calculates the level for a partial with given
center frequency and bw. Center frequency and bw are normalized, both
in partial index units in relation to the total number of partials. bw
is the distance between the center freq pos and the -6 dB points
left/right of the cfreq index. It gets interpolated between 0.5
and (1- num-partials).

Example:

    num-partials = 11
    cfreq = 6th partial (index 7 => cfreq-pos = (float (/ 7 16))  = 0.4375
    bw = 0.25 (= 2.5 partials)

(let* ((num-partials 11)
       (fn (bias-cos 0.5 0.25 num-partials)))
  (loop for partial below num-partials collect (funcall fn partial)))

 -> (0.09549150281252633d0 0.28711035421746367d0 0.5313952597646567d0
     0.7679133974894983d0 0.9381533400219317d0 1.0d0 0.9381533400219317d0
     0.7679133974894983d0 0.5313952597646567d0 0.28711035421746367d0
     0.09549150281252633d0)

"
    (let* ((real-bw (recalc-bw bw num-partials))
           (fader-interp (- (clip real-bw (1- num-partials) num-partials) (1- num-partials))))
      (lambda (x) (+ fader-interp
                (* (- 1 fader-interp)
                   (+ 0.5 (* 0.5 (cos (clip (/ (* pi 1/2 (- x (* cfreq-pos (1- num-partials))))
                                 real-bw)
                              (* -1 pi) pi))))))))))

(dsp! xy-partial-ctl ((arr (array sample)) (synth-id (or (unsigned-byte 62) node)) (num-partials (unsigned-byte 62)))
  "A synth to control the center-freq (mouse-x) and the bw (mouse-y) of
the amplitude of partials in a sin-noi-rtc(-stretch)-synth."
  (with-samples ((ypos 0) (ypos-old 0)
                 xpos xpos-old)
    (setf xpos (lag (mouse-x) 1))
    (setf ypos (lag (mouse-y) 1))
    (when (or (/= ypos-old ypos) (/= xpos-old xpos))
      (let ((fn (bias-cos xpos (- 1 ypos) num-partials)))
        (dotimes (partial num-partials)
          (setf (aref arr partial) (funcall fn partial))))
      (setf xpos-old xpos)
      (setf ypos-old ypos))))

(sin-noi-rtc-synth 0.2 ats-cuda::cl :fmod *fmod* :amod *amod* :amp-scale 0.05 :id 2)

(sin-noi-rtc-pstretch-synth 0.2 ats-cuda::cl :fmod *fmod* :pstretch 0 :base-partial 0 :amod *amod* :amp-scale 0.05 :id 1)

(xy-snd-amp-ctl 2 :id 3)
(free 3)
(xy-partial-ctl *amod* 2 (ats-cuda::ats-sound-partials ats-cuda::cl) :id 4)
(free 4)

(set-control synth-id :amp-scale (lag (mouse-y) 1))



(set-control 1 :soundpos 0.2)

(set-control 1 :pstretch 0.5)

(set-control 1 :amp-scale 0.03)

(set-control 1 :base-partial 10.4)

(let ((base-freq 440))
  (* base-freq (expt (/ freq base-freq) pstretch))

  )

(set-control 1 :par nil)


(row-major-aref (ats-cuda::ats-sound-frq-av ats-cuda::cl) 0)

(loop for x below 42 do (setf (aref *amod* x) (sample 0.7)))

(loop for x below 42 do (setf (aref *fmod* x) (sample 1.1)))
(* freq (expt pstretch fac))
(set-control 1 :timeptr 200)


(sin-noi-synth 0.0d0 ats-cuda::cl)
*deb*

(sin-noi-rtc-synth 2.0 ats-cuda::cl)


(sin-noi-synth 0.0 ats-cuda::cl :amp-scale 0.1)


((timeptr real)
     (ats-sound ats-cuda::ats-sound)
     (amp-scale real)
     (par (or null list))
     (fmod (or null (array sample)))
     (amod (or null (array sample)))
     (res-bal real))

(sin-synth 0.0 ats-cuda::cl :amp-scale 0.3)

(let ((ats-sound ats-cuda::cl))
  (array-dimension (ats-cuda::ats-sound-frq ats-sound) 0))

(define-vug ats-sine-bank (timeptr
                           (freqs (simple-array sample))
                           (amps (simple-array sample))
                           (fmod (simple-array sample))
                           (amod (simple-array sample))
                           (partials list))
  "A bank of sine wave oscillators. <freqs> and <amps> of the
   oscillators have to be supplied as sample arrays.

   <fmod> and <amod> are frequency and amplitude modulation
   arrays.

   <partials> is a list of indexes into the arrays indicating the
   partials to be generated.

   All supplied array sizes have to be (>= (length freqs)).
"
  (with-samples ((out 0)
                 (sine-sig 0.0))
    (with-sample-arrays ((pbws (sample-array (array-dimension freqs 0)))
                         (sin-phase-array (sample-array (array-dimension freqs 0))))
      (setf out 0.0d0)
      (dolist (partial partials)
        (let* ((freq (* (aref fmod partial)
                        (i-aref-n freqs partial timeptr)))
               (amp (aref amod partial))
               (sine (sine-n partial freq 1.0d0 sin-phase-array)))
          (setf sine-sig sine)
          (incf out (* (i-aref-n amps partial timeptr) sine-sig))))
      out)))



(in-package :ats-cuda)
(make-ats-sound :name "hallo")
(tracker "clarinet.aif"
	   'cl
	   :start 0.0
	   :hop-size 1/4
	   :lowest-frequency 100.0
	   :highest-frequency 20000.0
	   :frequency-deviation 0.05
	   :lowest-magnitude (db-amp -70)
	   :SMR-continuity 0.7
	   :track-length 6
	   :min-segment-length 3
	   :residual "/tmp/cl-res.snd"
	   :verbose nil
	   :debug nil)



