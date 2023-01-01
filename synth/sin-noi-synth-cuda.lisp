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

(in-package :incudine)

(let* ((len (1- (length ats-cuda::*ats-critical-band-edges*)))
       (arr1 (sample-array len))
       (arr2 (sample-array len)))
  (loop
    for (lo hi) on ats-cuda::*ats-critical-band-edges*
    while hi
    for idx from 0
    do (setf (aref arr1 idx) (sample (- hi lo)))
       (setf (aref arr2 idx) (sample (/ (+ hi lo) 2))))
  (defparameter *ats-critical-band-bws* arr1) ;;; band-widths of bark scale
  (defparameter *ats-critical-band-c-freqs* arr2)) ;;; center-frequencies of bark scale

(declaim (inline i-aref))
(defun i-aref (array idx)
  "linearly interpolated array indexing."
  (declare (type sample idx)
           (type (simple-array sample) array))
  (multiple-value-bind (lo ratio) (floor idx)
    (+ (* (- 1 ratio) (aref array lo)) (* ratio (aref array (1+ lo))))))

(declaim (inline i-aref-n))
(defun i-aref-n (array n idx)
  "linearly interpolated array indexing."
  (declare (type sample idx)
           (type integer n)
           (type (simple-array sample) array))
  (multiple-value-bind (lo ratio) (floor idx)
    (+ (* (- 1 ratio) (aref array n lo)) (* ratio (aref array n (1+ lo))))))

(declaim (inline i-aref-n))
(defun i-aref-n (array n idx)
  "linearly interpolated array indexing."
  (declare (type sample idx)
           (type integer n)
           (type (simple-array sample) array))
  (multiple-value-bind (lo ratio) (floor idx)
    (sample (+ (* (- 1 ratio) (aref array n lo))
               (* ratio (aref array n (1+ lo)))))))

(declaim (inline sin-level))
(defun sin-level (pan)
  "calc sine level from pan:
fades out from 1 to 0 for pan = [0.5..1]
otherwise = 1
"
  (declare (type sample pan))
     (if (> pan 0.5) (sin (* pi pan)) 1.0d0))

(sin-level 1.0d0)

(declaim (inline res-level))
(defun res-level (pan)
  "calc residual level from pan:
fades in from 0 to 1 for pan = [0..0.5]
otherwise = 1
"
  (declare (type sample pan))
  (if (< pan 0.5) (sample (sin (* pi pan))) 1.0d0))

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

(defun range (num &optional end)
     (loop for n from (if end num 0) below (or end num)
           collect n))

(defun get-noise-bws (band-array)
  "return an array of the bark scale bandwidths of the idxs given in
<band-array>."
  (make-array (length band-array)
              :element-type 'sample
              :initial-contents (loop for band across band-array
                                      collect (aref *ats-critical-band-bws* band))))

(defun get-noise-c-freqs (band-array)
  "return an array of the bark scale center-frequencies of the idxs
given in <band-array>."
  (make-array (length band-array)
              :element-type 'sample
              :initial-contents (loop for band across band-array
                                      collect (aref *ats-critical-band-c-freqs* band))))
(declaim (inline ats-master-vug))
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
  (+ (ats-sine-noi-bank timeptr freqs amps pnoi fmod amod partials res-bal)
     (* (res-level res-bal) (ats-noise-bank timeptr noise-cfreqs noise-bws noise-energy))))

(declaim (inline ats-sine-noi-bank))
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

(declaim (inline ats-noise-bank))
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
                 (sample-array num-partials :initial-element 1.0d0))))))

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
    (with-samples ((dur 

                    (sample
                     (* (/ (* (1- (ats-cuda::ats-sound-frames ats-sound))
                              (/ (ats-cuda::ats-sound-frame-size ats-sound)
                                 (ats-cuda::ats-sound-sampling-rate ats-sound)))
                           (ats-cuda::ats-sound-dur ats-sound))
                        (or duration (ats-cuda::ats-sound-dur ats-sound)))))
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
                 (sample-array num-partials :initial-element 1.0d0))))))


(export 'sin-noi-synth 'incudine)
