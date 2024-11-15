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

(defmacro i-vls (val1 val2 pos)
  "linear interpolation."
  `(+ (* (- 1 ,pos) ,val1)
      (* ,pos ,val2)))

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
(define-vug ats-sine-bank (frameptr
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
                        (i-aref-n freqs partial frameptr)))
               (amp (aref amod partial))
               (sine (sine-n partial freq amp sin-phase-array))
               )
          (setf sine-sig sine)
          (setf (aref pbws partial) (if (< freq 500.0) 50.0d0 (* freq 0.1d0)))
          (incf out (+ (* (i-aref-n amps partial frameptr)
                          sine-sig)))))
      out)))

(declaim (inline ats-sine-bank))
(define-vug ats-sine-bank (frameptr
                           (freqs (simple-array sample))
                           (amps (simple-array sample))
                           (fmod (simple-array sample))
                           (amod (simple-array sample))
                           (partials list))
  "A bank of sine wave oscillators. <freqs> and <amps> of the
   oscillators have to be supplied as sample arrays.

   <frameptr> is a floating point index of the frame to synthesize,
   the frequencies being linearly interpolated between neighboring
   frames.

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
                        (i-aref-n freqs partial frameptr)))
               (amp (aref amod partial))
               (sine (sine-n partial freq amp sin-phase-array)))
          (setf sine-sig sine)
          (incf out (* (i-aref-n amps partial frameptr) sine-sig))))
      out)))


(declaim (inline ats-sine-noi-bank))
(define-vug ats-sine-noi-bank (frameptr
                               (freqs (simple-array sample))
                               (amps (simple-array sample))
                               (pnoi (simple-array sample))
                               (fmod (simple-array sample))
                               (amod (simple-array sample))
                               (partials list)
                               (res-bal real))
  "A bank of sine wave plus residual noise oscillators.

   <frameptr> is a floating point index of the frame to synthesize,
   the frequencies and noise-energies being linearly interpolated
   between neighboring frames.

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
                 (res-level 1))
    (with-sample-arrays ((pbws (sample-array (array-dimension freqs 0)))
                         (sin-phase-array (sample-array (array-dimension freqs 0)))
                         (lag-array (sample-array (array-dimension freqs 0))))
      (setf out 0.0d0)
      (setf sin-level (sin-level res-bal))
      (setf res-level (res-level res-bal))
      (dolist (partial partials)
        (let* ((freq (* (aref fmod partial)
                        (i-aref-n freqs partial frameptr)))
               (amp (lag-n partial (aref amod partial) 10 lag-array))
               (sine (sine-n partial freq amp sin-phase-array)))
          (setf sine-sig sine)
          (setf (aref pbws partial) (if (< freq 500.0) 50.0d0 (* freq 0.1d0)))
          (incf out (+ 0.0d0
                       (* sin-level
                          (i-aref-n amps partial frameptr)
                          sine-sig)
                       (* res-level
                          (i-aref-n pnoi partial frameptr)
                          sine-sig
                          (randi-n partial pbws))))))
      out)))

(define-vug ats-sine-noi-bank-pstretch (frameptr
                               (freqs (simple-array sample))
                               (amps (simple-array sample))
                               (pnoi (simple-array sample))
                               (fmod (simple-array sample))
                               (amod (simple-array sample))
                               (partials list)
                               (base-partial real)
                               (pstretch real)
                               (res-bal real))
  "A bank of sine wave plus residual noise oscillators with amplitude
   and frequency modulation and partial stretching.

   <frameptr> is a floating point index of the frame to synthesize,
   the frequencies and noise-energies being linearly interpolated
   between neighboring frames.

   <freqs>, <amps> and <pnoi> are arrays, indicating the frequencies,
   sine-levels and noise-levels of the oscillators.

   <fmod> and <amod> are frequency and amplitude modulation
   arrays.

   <partials> is a list of indexes into the arrays indicating the
   partials to be generated.

   <base-freq> is the freq which pstretch is related to.

   <pstretch> is the frequency stretch of the partials in
   semitones/12 per octave related to the base-freq.

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
                 (res-level 1))
    (with ((pstretch-frac (/ pstretch 12)))
      (declare (type real pstretch-frac))
      (with-sample-arrays ((pbws (sample-array (array-dimension freqs 0)))
                           (sin-phase-array (sample-array (array-dimension freqs 0)))
                           (lag-array (sample-array (array-dimension freqs 0))))
        (setf out 0.0d0)
        (setf sin-level (sin-level res-bal))
        (setf res-level (res-level res-bal))
        (dolist (partial partials)
          (let* ((base-frq (multiple-value-bind (partial pos) (floor base-partial)
                             (i-vls (i-aref-n freqs partial frameptr)
                                    (i-aref-n freqs (1+ partial) frameptr)
                                    pos)))
                 (frq (i-aref-n freqs partial frameptr))
                 (freq (* (aref fmod partial)
                          frq
                          (expt (/ frq base-frq) pstretch-frac)))
                 (amp (lag-n partial (aref amod partial) 0.5 lag-array))
                 (sine (sine-n partial freq amp sin-phase-array))
                 )
            (setf sine-sig sine)
            (setf (aref pbws partial) (if (< freq 500.0) 50.0d0 (* freq 0.1d0)))
            (incf out (+ (* 0
                            sin-level
                            (i-aref-n amps partial frameptr)
                            sine-sig)
                         (* 0
                            res-level
                            (i-aref-n pnoi partial frameptr)
                            sine-sig
                            (randi-n partial pbws))))))
        out))))

(declaim (inline ats-noise-bank))
(define-vug ats-noise-bank (frameptr
                            (noise-cfreqs (simple-array sample))
                            (noise-bws (simple-array sample))
                            (noise-energy (simple-array sample)))
  "A bank of residual noise oscillators using a frameptr to index into the ats data structure.

   <frameptr> is a floating point index of the frame to synthesize,
   the noise energy being linearly interpolated between neighboring
   frames.

   <noise-cfreqs> and <noise-bws> are the center frequencys and
   bandwidths of the 25 element bark scale bands used for the ats
   analysis. <noise-energy> is the two-dimensional array of the
   ats-data (first dimension = 25, second dimension = number of frames
   of the ats analysis data).

"
  (with-samples ((out 0))
    (with ((num-bands (length noise-bws)))
      (declare (type integer num-bands))
      (with-sample-arrays
          ((sin-phase-array (sample-array (length noise-cfreqs)))
           (lag-array (sample-array (length noise-cfreqs))))
        (setf out 0.0d0)
        (dotimes (n num-bands)
          (incf out (* (sine-n n (aref noise-cfreqs n) 1.0d0 sin-phase-array)
                       (lag-n n (i-aref-n noise-energy n frameptr) 1 lag-array)
                       (randi-n n noise-bws))))))
    out))

(declaim (inline ats-master-vug-compat))
(define-vug ats-master-vug-compat
  (frameptr
   (freqs (simple-array sample))
   (amps (simple-array sample))
   (pnoi (simple-array sample))
   (noise-bws (simple-array sample))
   (noise-cfreqs (simple-array sample))
   (noise-energy (simple-array sample))
   (partials list)
   (amod (simple-array sample))
   (fmod (simple-array sample))
   (res-bal real)
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
  (+ (ats-sine-noi-bank frameptr freqs amps pnoi fmod amod partials res-bal)
     (* (res-level res-bal) (ats-noise-bank frameptr noise-cfreqs noise-bws noise-energy))))

(declaim (inline ats-master-vug))
(define-vug ats-master-vug
    (frameptr
     (freqs (simple-array sample))
     (amps (simple-array sample))
     (pnoi (simple-array sample))
     (noise-bws (simple-array sample))
     (noise-cfreqs (simple-array sample))
     (noise-energy (simple-array sample))
     (partials list)
     (amod (simple-array sample))
     (fmod (simple-array sample))
     (res-bal real))
  (:defaults 0
             (incudine:incudine-missing-arg "FREQS")
             (incudine:incudine-missing-arg "AMPS")
             (incudine:incudine-missing-arg "PNOI")
             (incudine:incudine-missing-arg "NOISE-BWS")
             (incudine:incudine-missing-arg "NOISE-CFREQS")
             (incudine:incudine-missing-arg "NOISE_ENERGY")
             nil (sample-array 1) (sample-array 1) 0.5)
  "Master VUG for the sin-noi-rtc-synth."
  (+ (ats-sine-noi-bank frameptr freqs amps pnoi fmod amod partials res-bal)
     (* (res-level res-bal) (ats-noise-bank frameptr noise-cfreqs noise-bws noise-energy))))

(declaim (inline envelope*))
(define-ugen envelope* frame ((env incudine.vug:envelope) gate time-scale (done-action function))
  "Envelope Ugen working with any blocksize. The product of /time-scale/
and the total duration of /env/ is the total duration of the envelope
in seconds. /done-action/ is called when the total-duration has been
reached or when /gate/ is zero and the release phase of the envelope
has ended.

envelope* returns an array of block-size samples.

@Arguments
env - incudine.vug:envelope instance to use.
gate - Number functioning as a gate: If zero, start the release phase.
time-scale - Number scaling the envelope x-values.
done-action - Function to call on the dsp-node at end of release.

@See-also
buffer-loop-play*
buffer-play*
buffer-stretch-play*
envelope*
line*
phasor*
phasor-loop*
play-buffer*
play-buffer-loop*
play-buffer-stretch*
play-buffer-stretch-env-pan-out*
"
  (:defaults (incudine:incudine-missing-arg "Missing ENVELOPE struct.")
             1 1 #'identity)
  (with ((frm (make-frame (block-size))))
    (foreach-frame
      (setf (frame-ref frm current-frame)
            (envelope env gate time-scale done-action)))
    frm))

(declaim (inline ats-master-vug-pstretch))
(define-vug ats-master-vug-pstretch
    (frameptr
     (freqs (simple-array sample))
     (amps (simple-array sample))
     (pnoi (simple-array sample))
     (noise-bws (simple-array sample))
     (noise-cfreqs (simple-array sample))
     (noise-energy (simple-array sample))
     (partials list)
     (fmod (simple-array sample))
     (amod (simple-array sample))
     (base-partial real)
     (pstretch real)
     (res-bal real))
  (:defaults 0
             (incudine:incudine-missing-arg "FREQS")
             (incudine:incudine-missing-arg "AMPS")
             (incudine:incudine-missing-arg "PNOI")
             (incudine:incudine-missing-arg "NOISE-BWS")
             (incudine:incudine-missing-arg "NOISE-CFREQS")
             (incudine:incudine-missing-arg "NOISE_ENERGY")
             nil (sample-array 1) (sample-array 1)
             100 0 0.5)
  "Master VUG for the sin-noi-rtc-pstretch-synth."
  (+ (ats-sine-noi-bank-pstretch
      frameptr freqs amps pnoi fmod amod partials base-partial pstretch res-bal)
     (* (res-level res-bal) (ats-noise-bank frameptr noise-cfreqs noise-bws noise-energy))))

(dsp! sin-noi-synth ((start-time real) (ats-sound ats-cuda::ats-sound) (amp-scale (or null real))
                     (amp-env (or null list)) (frq-scale (or null real)) (duration (or null real))
                     (time-ptr (or null list)) (par (or null list)) (noise-env (or null list))
                     (noise-only boolean) (band-noise boolean))
  (:defaults 0 (incudine:incudine-missing-arg "ATS_SOUND") 1 nil 1 nil nil nil nil nil t)
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
                   idx)
      (with
          ((frameptr (envelope*
                      (make-clm-env
                       (or time-ptr '(0 0 1 1))
                       :scaler scale
                       :offset start-frm
                       :duration dur)
                      :done-action #'free))
           (amp (envelope*
                 (make-clm-env
                  (or amp-env '(0 1 1 1))
                  :duration dur)
                 :done-action #'free))
           (noise-amp (envelope*
                       (make-clm-env
                        (or noise-env '(0 0.5 1 0.5))
                        :duration dur)
                       :done-action #'free)))
        (maybe-expand frameptr)
        (maybe-expand amp)
        (maybe-expand noise-amp)
        (with ((num-partials (array-dimension (ats-cuda::ats-sound-frq ats-sound) 0))
               (partials (or par (ats-cuda::range num-partials))))
          (declare (type list partials)
                   (type integer num-partials))
          (foreach-frame
            (stereo (* (smp-ref amp current-frame)
                       (ats-master-vug
                        (smp-ref frameptr current-frame)
                        (ats-cuda::ats-sound-frq ats-sound)
                        (ats-cuda::ats-sound-amp ats-sound)
                        (ats-cuda::ats-sound-energy ats-sound)
                        (get-noise-bws (ats-cuda::ats-sound-bands ats-sound))
                        (get-noise-c-freqs (ats-cuda::ats-sound-bands ats-sound))
                        (ats-cuda::ats-sound-band-energy ats-sound)
                        partials
                        (sample-array num-partials :initial-element curr-amp)
                        (sample-array num-partials :initial-element curr-frq-scale)
                        (smp-ref noise-amp current-frame))))))))))

(dsp! sin-noi-synth ((start-time real) (ats-sound ats-cuda::ats-sound) (amp-scale (or null real))
                     (amp-env (or null list)) (frq-scale (or null real)) (duration (or null real))
                     (time-ptr (or null list)) (par (or null list)) (noise-env (or null list)))
  (:defaults 0 (incudine:incudine-missing-arg "ATS_SOUND") 1 nil 1 nil nil nil nil)
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
                   idx)
      (with
          ((frameptr (envelope*
                      (make-clm-env
                       (or time-ptr '(0 0 1 1))
                       :scaler scale
                       :offset start-frm
                       :duration dur)
                      :done-action #'free))
           (amp (envelope*
                 (make-clm-env
                  (or amp-env '(0 1 1 1))
                  :duration dur)
                 :done-action #'free))
           (noise-amp (envelope*
                       (make-clm-env
                        (or noise-env '(0 0.5 1 0.5))
                        :duration dur)
                       :done-action #'free)))
        (maybe-expand frameptr)
        (maybe-expand amp)
        (maybe-expand noise-amp)
        (with ((num-partials (array-dimension (ats-cuda::ats-sound-frq ats-sound) 0))
               (partials (or par (ats-cuda::range num-partials))))
          (declare (type list partials)
                   (type integer num-partials))
          (foreach-frame
            (stereo (* (smp-ref amp current-frame)
                       (ats-master-vug-compat
                        (smp-ref frameptr current-frame)
                        (ats-cuda::ats-sound-frq ats-sound)
                        (ats-cuda::ats-sound-amp ats-sound)
                        (ats-cuda::ats-sound-energy ats-sound)
                        (get-noise-bws (ats-cuda::ats-sound-bands ats-sound))
                        (get-noise-c-freqs (ats-cuda::ats-sound-bands ats-sound))
                        (ats-cuda::ats-sound-band-energy ats-sound)
                        partials
                        (sample-array num-partials :initial-element curr-amp)
                        (sample-array num-partials :initial-element curr-frq-scale)
                        (smp-ref noise-amp current-frame))))))))))

(dsp! sin-synth
    ((start-time real)
     (ats-sound ats-cuda::ats-sound)
     (amp-scale (or null real))
     (amp-env (or null list))
     (frq-scale (or null real))
     (duration (or null real))
     (par (or null list)))
  (:defaults 0 (incudine:incudine-missing-arg "ATS_SOUND") 1 nil 1 nil nil)
  (with ((max-frame (1- (ats-cuda::ats-sound-frames ats-sound)))
         (start-frm
             (round
              (* start-time
                 (/ (ats-cuda::ats-sound-sampling-rate ats-sound)
                    (ats-cuda::ats-sound-frame-size ats-sound)))))
         (scale (- max-frame start-frm))
         (dur (or duration (- (ats-cuda::ats-sound-dur ats-sound) start-time))))
    (with-samples ((curr-amp (sample (or amp-scale 1.0d0)))
                   (curr-frq-scale (sample (or frq-scale 1.0d0))))
      (with ((frameptr (envelope*
                        (make-clm-env
                         '(0 0 1 1)
                         :scaler scale
                         :offset start-frm
                         :duration dur)))
             (amp (envelope*
                   (make-clm-env
                    (or amp-env '(0 1 1 1))
                    :duration dur)
                   :done-action #'free)))
        (maybe-expand frameptr)
        (maybe-expand amp)
        (with ((num-partials (array-dimension (ats-cuda::ats-sound-frq ats-sound) 0))
               (partials (or par (ats-cuda::range num-partials))))
          (declare (type list partials)
                   (type integer num-partials))
          (foreach-frame
            (stereo (* (smp-ref amp current-frame)
                       (ats-sine-bank
                        (smp-ref frameptr current-frame)
                        (ats-cuda::ats-sound-frq ats-sound)
                        (ats-cuda::ats-sound-amp ats-sound)
                        (sample-array num-partials :initial-element curr-frq-scale)
                        (sample-array num-partials :initial-element curr-amp)
                        partials)))))))))

;; (sin-noi-synth 0.0 ats-cuda::cl)

(dsp! sin-noi-rtc-synth
    ((soundpos real)
     (ats-sound ats-cuda::ats-sound)
     (amp-scale real)
     (par (or null list))
     (fmod (or null (array sample)))
     (amod (or null (array sample)))
     (res-bal real))
  (:defaults 0 (incudine:incudine-missing-arg "ATS_SOUND") 1 nil nil nil 0.5)
  "The synth definition with realtime control.

<soundpos> normalized index into the ats sound (1 = end of sound).

<ats-sound> the sound to use.

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
                 (frameptr (lag (sample (* soundpos (ats-cuda:ats-sound-frames ats-sound))) 100)))
    (with ((num-partials (array-dimension (ats-cuda:ats-sound-frq ats-sound) 0))
           (partials (or par (ats-cuda::range num-partials))))
      (declare (type list partials)
               (type integer num-partials))
      (with ((frm (make-frame (block-size)))))
      (with-sample-arrays
          ((amp-mod (or amod (sample-array num-partials :initial-element 1.0d0)))
           (frq-mod (or fmod (sample-array num-partials :initial-element 1.0d0))))
        (foreach-frame
          (stereo (* curr-amp
                     (ats-master-vug
                      frameptr
                      (ats-cuda:ats-sound-frq ats-sound)
                      (ats-cuda:ats-sound-amp ats-sound)
                      (ats-cuda::ats-sound-energy ats-sound)
                      (get-noise-bws (ats-cuda:ats-sound-bands ats-sound))
                      (get-noise-c-freqs (ats-cuda:ats-sound-bands ats-sound))
                      (ats-cuda:ats-sound-band-energy ats-sound)
                      partials
                      frq-mod
                      amp-mod
                      res-bal))))))))

(dsp! sin-noi-rtc-pstretch-synth
    ((soundpos real)
     (ats-sound ats-cuda::ats-sound)
     (amp-scale real)
     (par (or null list))
     (fmod (or null (array sample)))
     (amod (or null (array sample)))
     (pstretch real)
     (base-partial real)
     (res-bal real))
  (:defaults 0 (incudine:incudine-missing-arg "ATS_SOUND") 1 nil nil nil
             0 0 0.5)
  "ATS synth definition with realtime control and partial stretching.

<soundpos> normalized index into the ats sound (1 = end of sound).

<fmod> Array of frequency modulation values. The arrayidx relates to
           the ATS partial with the same idx.

<amod> Array of amplitude modulation values. The arrayidx relates to
           the ATS partial with the same idx.

<pstretch> is the partial stretch in semitones per octave related to
           <base-freq>

<base-partial> the partial of the ats analysis pstretch is related
           to. Can be a float with linear interpolation between the
           partial frequencies.

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
                 (frameptr (sample (* soundpos (ats-cuda::ats-sound-frames ats-sound)))))
    (with ((num-partials (array-dimension (ats-cuda::ats-sound-frq ats-sound) 0))
           (partials (or par (ats-cuda::range num-partials))))
      (declare (type list partials)
               (type integer num-partials))
      (with-sample-arrays
          ((amp-mod (or amod (sample-array num-partials :initial-element 1.0d0)))
           (frq-mod (or fmod (sample-array num-partials :initial-element 1.0d0))))
        (stereo (* (lag curr-amp 0.3)
                   (ats-master-vug-pstretch
                    (lag frameptr 4)
                    (ats-cuda::ats-sound-frq ats-sound)
                    (ats-cuda::ats-sound-amp ats-sound)
                    (ats-cuda::ats-sound-energy ats-sound)
                    (get-noise-bws (ats-cuda::ats-sound-bands ats-sound))
                    (get-noise-c-freqs (ats-cuda::ats-sound-bands ats-sound))
                    (ats-cuda::ats-sound-band-energy ats-sound)
                    partials
                    frq-mod
                    amp-mod
                    base-partial
                    pstretch
                    res-bal)))))))


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
         (fader-interp (- (incudine::clip real-bw (1- num-partials) num-partials) (1- num-partials))))
    (lambda (x) (+ fader-interp
              (* (- 1 fader-interp)
                 (+ 0.5 (* 0.5 (cos (clip (/ (* pi 1/2 (- x (* cfreq-pos (1- num-partials))))
                                             real-bw)
                                          (* -1 pi) pi)))))))))

(dsp! xy-sndpos-partial-ctl ((arr (array sample)) (bw real)
                             (synth-id (or (unsigned-byte 62) node))
                             (num-partials (unsigned-byte 62))
                             (xlag-time real)
                             (ylag-time real))
  "A synth to control the center-freq (mouse-x) and the bw (mouse-y) of
the amplitude of partials in a sin-noi-rtc(-stretch)-synth."
  (:defaults (incudine::incudine-missing-arg "ARR") 1 0.5 1 1.5 1)
  (with-samples ((ypos 0) (ypos-old 0)
                 (bw-old 0) (bw-curr 0)
                 xpos xpos-old)
    (setf xpos (lag (mouse-x) (sample (/ xlag-time (block-size)))))
    (setf ypos (lag (mouse-y) (sample (/ ylag-time (block-size)))))
    (setf bw-curr (lag (sample bw) (sample (/ (block-size)))))
    (when (or (/= bw-curr bw-old) (/= ypos ypos-old) (/= xpos xpos-old))
      (set-control synth-id :soundpos xpos)
      (let ((fn (bias-cos ypos bw num-partials)))
        (dotimes (partial num-partials)
          (setf (aref arr partial) (funcall fn partial))))
      (setf xpos-old xpos)
      (setf ypos-old ypos)
      (setf bw-old (sample bw-curr)))))

(export '(sin-noi-synth sin-synth sin-noi-rtc-synth sin-noi-rtc-pstretch-synth
          xy-sndpos-partial-ctl)
        'incudine)



