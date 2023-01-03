;;; 
;;; synth.lisp
;;;
;;; 2023 port of the original ats synths by Juan Pampin to incudine
;;; plus additional synths. For the syntax of the original synths see
;;; "General Purpose ATS Synthesizer" below.
;;;
;;; Different to the original clm synths the incudine synths are
;;; working in realtime. Using (bounce-to-disk ...) the behaviour of
;;; the original synths can be achieved (see ../examples.lisp).
;;;
;;;
;;;
;;; **********************************************************************
;;; Copyright (c) 2023 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
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
;;;
;;; ATS 
;;; by Juan Pampin
;;; juan@ccrma.stanford.edu
;;;
;;; sin-noi-synth
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
;;;
;;; **********************************************************************
;;;
;;; sin-synth
;;;
;;; additive synthesis using oscillators
;;;
;;; Parameters:
;;; ===========
;;; sound ->  ATS sound to synthesize
;;; (amp-scale 1.0) -> global amplitude scalar
;;; (amp-env '(0 1 1 1)) -> global amplitude envelope
;;; (frq-scale 1.0) -> global frequency scalar
;;; (duration nil) -> duration, if nil sound's duration is used 
;;; (par nil) -> list of partial numbers to sinthesize, if nil all partials
;;;
;;;
;;;

(in-package :incudine)

(declaim (inline make-clm-env))
(defun* make-clm-env (breakpoint-list (scaler 1.0) duration (offset 0.0)
                      base end length)
  (breakpoints->env breakpoint-list :scaler scaler :offset offset
                    :base base
                    :duration (or duration
                                  (and end (* end *sample-duration*))
                                  (and length (* length *sample-duration*)))))

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

(declaim (inline i-aref-n))
(defun i-aref-n (array n idx)
  "linearly interpolated array indexing."
  (declare (type real idx)
           (type non-negative-fixnum n)
           (type (simple-array sample) array))
  (multiple-value-bind (lo ratio) (floor idx)
    (+ (* (- 1 ratio) (aref array n lo))
       (* ratio (aref array n (1+ lo))))))

(declaim (inline sin-level))
(defun sin-level (pan)
  "calc sine level from pan:
fades out from 1 to 0 for pan = [0.5..1]
otherwise = 1
"
  (declare (type real pan))
     (if (> pan 0.5) (sin (* pi pan)) 1.0d0))

(declaim (inline res-level))
(defun res-level (pan)
  "calc residual level from pan:
fades in from 0 to 1 for pan = [0..0.5]
otherwise = 1
"
  (declare (type real pan))
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

(declaim (inline ats-sine-bank))
(define-vug ats-sine-bank (timeptr
                               (freqs (simple-array sample))
                               (amps (simple-array sample))
                               (amod (simple-array sample))
                               (fmod (simple-array sample))
                               (partials list))
  (with-samples ((out 0)
                 (sine-sig 0.0))
    (with-sample-arrays ((pbws (sample-array (array-dimension freqs 0)))
                         (sin-phase-array (sample-array (array-dimension freqs 0))))
      (setf out 0.0d0)
      (dolist (partial partials)
        (let* (
               (freq (* (aref fmod partial)
                        (i-aref-n freqs partial timeptr)))
               (amp (aref amod partial))
               (sine (sine-n partial freq amp sin-phase-array))
               )
          (setf sine-sig sine)
          (setf (aref pbws partial) (if (< freq 500.0) 50.0d0 (* freq 0.1d0)))
          (incf out (+ (* (i-aref-n amps partial timeptr)
                          sine-sig)))))
      out)))

;;; 
(declaim (inline ats-sine-noi-bank))
(define-vug ats-sine-noi-bank (timeptr
                               (freqs (simple-array sample))
                               (amps (simple-array sample))
                               (pnoi (simple-array sample))
                               (fmod (simple-array sample))
                               (amod (simple-array sample))
                               (partials list)
                               res-bal)
  "A bank of sine wave plus residual noise oscillators.
   <freqs>, <amps> and <noise-level> of the oscillators have to be
   supplied as sample arrays. All supplied array sizes have to
   be (>= (length freqs)).

   <fmod> and <amod> are frequency and amplitude modulation
   arrays. Partials is a list of indexes into the arrays of the
   partials to be generated.

   <res-bal> is a crossfade between 0 (sine only) and 1 (residual
   only). Note that a pan value of 0.5 results in an amplitude of 1
   for both, sine and residual component. Higher pan values will fade
   out the sine and lower pan values will fade out the residual
   component.
"
  (with-samples ((out 0)
                 (sine-sig 0.0)
                 (sin-level 1)
                 (res-level 1))
    (with-sample-arrays ((pbws (sample-array (array-dimension freqs 0)))
                         (sin-phase-array (sample-array (array-dimension freqs 0))))
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
                          sine-sig
                          (randi-n partial pbws))))))
      out)))

(declaim (inline ats-noise-bank))
(define-vug ats-noise-bank (timeptr
                            (noise-cfreqs (simple-array sample))
                            (noise-bws (simple-array sample))
                            (noise-energy (simple-array sample)))
  "A bank of residual noise oscillators using a timeptr to index into the ats data structure.

   <noise-cfreqs> and <noise-bws> are the center frequencys and
   bandwidths of the 25 element bark scale bands used for the ats
   analysis. <noise-energy> is the two-dimensional array of the
   ats-data (first dimension = 25, second dimension = number of frames
   of the ats analysis data).

   timeptr is a floating point index of the frame to synthesize, the
   noise energy being linearly interpolated between neighboring
   frames.
"
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

(declaim (inline ats-master-vug-compat))
(define-vug ats-master-vug-compat
  (timeptr
   (freqs (simple-array sample))
   (amps (simple-array sample))
   (pnoi (simple-array sample))
   (noise-bws (simple-array sample))
   (noise-cfreqs (simple-array sample))
   (noise-energy (simple-array sample))
   (partials list)
   (amod (simple-array sample))
   (fmod (simple-array sample))
   (noise-amp real)
   (noise-only boolean)
   (band-noise boolean))
  (:defaults 0
             (incudine:incudine-missing-arg "FREQS")
             (incudine:incudine-missing-arg "AMPS")
             (incudine:incudine-missing-arg "PNOI")
             (incudine:incudine-missing-arg "NOISE-BWS")
             (incudine:incudine-missing-arg "NOISE-CFREQS")
             (incudine:incudine-missing-arg "NOISE_ENERGY")
             nil (sample-array 1) (sample-array 1) 0.5
             nil t)
  "The main synthesis vug compatible with the behaviour of the original clm instrument.
"
  (+
   (if noise-only
         (* noise-amp
            (ats-sine-noi-bank timeptr freqs amps pnoi fmod amod partials 1))
         (ats-sine-noi-bank timeptr freqs amps pnoi fmod amod partials (* 0.5 noise-amp)))
     (if band-noise
         (* noise-amp
            (ats-noise-bank timeptr noise-cfreqs noise-bws noise-energy))
         0.0d0)))

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
  "The synth definition compatible with the definstrument of the original
clm instrument."
  (with ((start-frm
          (round (* start-time
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
      (with ((num-partials (array-dimension (ats-cuda::ats-sound-frq ats-sound) 0))
             (partials (or par (range num-partials))))
        (declare (type list partials)
                 (type integer num-partials))
        (setf idx timeptr)
        (stereo (ats-master-vug-compat
                 timeptr
                 (ats-cuda::ats-sound-frq ats-sound)
                 (ats-cuda::ats-sound-amp ats-sound)
                 (ats-cuda::ats-sound-energy ats-sound)
                 (get-noise-bws (ats-cuda::ats-sound-bands ats-sound))
                 (get-noise-c-freqs (ats-cuda::ats-sound-bands ats-sound))
                 (ats-cuda::ats-sound-band-energy ats-sound)
                 partials
                 (sample-array num-partials :initial-element curr-amp)
                 (sample-array num-partials :initial-element curr-frq-scale)
                 noise-amp
                 noise-only
                 band-noise))))))

#|
  (start-time sound &key 
	      (amp-scale 1.0)
	      (amp-env '(0 1 1 1))
	      (frq-scale 1.0)
	      (duration nil)
	      (par nil))
|#

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
      (with ((num-partials (length (ats-cuda::ats-sound-frq ats-sound)))
             (partials (or par (range num-partials))))
        (declare (type list partials)
                 (type integer num-partials))
        (setf idx timeptr)
        (stereo (* amp
                   (ats-sine-bank
                    timeptr
                    (ats-cuda::ats-sound-frq ats-sound)
                    (ats-cuda::ats-sound-amp ats-sound)
                    (sample-array num-partials :initial-element curr-amp)
                    (sample-array num-partials :initial-element curr-frq-scale)
                    partials)))))))


(export '(sin-noi-synth sin-synth) 'incudine)
