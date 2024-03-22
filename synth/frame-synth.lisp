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

(define-ugen phasor-n* frame ((n fixnum) (phasors (array compiled-function)))
  (with ((frm (make-frame (block-size)))
         (phfrm (funcall (aref phasors n))))
    (maybe-expand phfrm)
    (foreach-frame
      (reduce-warnings
        (setf (frame-ref frm current-frame)
              (frame-ref phfrm current-frame))))
    frm))

#|
(make-array 3 :element-type 'compiled-function :initial-contents
            (loop for freq in '(440 660 880) collect (phasor* freq 0)))
|#

(define-ugen phasor* frame (freq init)
  (with ((frm (make-frame (block-size))))
    (foreach-frame
      (setf (frame-ref frm current-frame)
            (phasor freq init)))
    frm))

(dsp! ph-test (freq)
  (with ((frm1 (phasor* freq 0.0)))
    (maybe-expand frm1)
    (foreach-frame
      (out (* 0.1 (frame-ref frm1 current-frame))))))

;;; (ph-test 440)

(declaim (inline phasor-n*))
(define-ugen phasor-n* frame ((n fixnum) freq (phases (simple-array sample)))
  (with ((frm (make-frame (block-size))))
    (foreach-frame
      (setf (frame-ref frm current-frame)
            (phasor-n n phases freq)))
    frm))

(declaim (inline sine-n-norm*))
(define-ugen sine-n-norm* frame ((n fixnum) freq (sine-phases (simple-array sample)))
  (with ((frm (make-frame (block-size))))
    (foreach-frame
      (setf (frame-ref frm current-frame)
            (sine-n-norm n freq sine-phases)))
    frm))

(declaim (inline sine-n*))
(define-ugen sine-n* frame ((n fixnum) freq amp (sine-phases (simple-array sample)))
  (with ((frm (make-frame (block-size))))
    (foreach-frame
      (setf (frame-ref frm current-frame)
            (sine-n n freq amp sine-phases)))
    frm))

(declaim (inline randi-n*))
(define-ugen randi-n* frame
    ((n integer) (freqs (simple-array sample)))
  (:defaults 1 (sample-array 1 :initial-element 440.0d0))
  (with ((frm (make-frame (block-size))))
    (foreach-frame
      (setf (frame-ref frm current-frame)
            (interpolate-n n (white-noise) freqs)))
    frm))

(declaim (inline lag-n*))
(define-ugen lag-n* frame ((n integer) in time (arr (simple-array sample)))
  "Scaled one pole filter with the coefficient calculated from
a 60 dB lag TIME (array version)."
  (with ((frm (make-frame (block-size))))
    (foreach-frame
      (setf (frame-ref frm current-frame)
            (pole*-n n in (t60->pole time) arr)))
    frm))

(declaim (inline phasor-bank*))
(define-ugen phasor-bank* frame
    ((freqs (simple-array sample)) (idxs list))
  (with ((frm (make-frame (block-size)))
         (phases (sample-array (length freqs)) :initial-element 0.0d0))
    (foreach-frame
      (setf (frame-ref frm current-frame)
            0.0d0))
    (dolist (idx idxs)
      (with ((ph (phasor-n* idx (aref freqs idx) phases)))
        (maybe-expand ph)
        (foreach-frame
          (incf (frame-ref frm current-frame)
                (* 0.1 (frame-ref ph current-frame))))))
    frm))

(declaim (inline sine-bank*))
(define-ugen sine-bank* frame
    ((freqs (simple-array sample))
           (amps (simple-array sample))
           (idxs list))
  (with ((frm (make-frame (block-size)))
         (phases (sample-array (length freqs)) :initial-element 0.0d0))
    (foreach-frame
      (setf (frame-ref frm current-frame)
            0.0d0))
    (dolist (idx idxs)
      (with ((ph (sine-n* idx (aref freqs idx) (aref amps idx) phases)))
        (maybe-expand ph)
        (foreach-frame
          (incf (frame-ref frm current-frame)
                (frame-ref ph current-frame)))))
    frm))

(declaim (inline ats-sine-noi-bank*))
(define-ugen ats-sine-noi-bank* frame (frameptr
                                   (freqs (simple-array sample))
                                   (amps (simple-array sample))
                                   (pnoi (simple-array sample))
                                   (fmod (simple-array sample))
                                   (amod (simple-array sample))
                                   (partials list)
                                   (res-bal real))
  (with-samples ((sin-level (sin-level res-bal))
                 (res-level (res-level res-bal)))
    (with ((frm (make-frame (block-size)))
           (phases (sample-array (array-dimension freqs 0)) :initial-element 0.0d0)
           (pbws (sample-array (array-dimension freqs 0))))
      (foreach-frame (setf (frame-ref frm current-frame) 0.0d0))
      (dolist (partial partials)
        (let* ((freq (* (aref fmod partial) (i-aref-n freqs partial frameptr))))
          (setf (aref pbws partial) (if (< freq 500.0) 50.0d0 (* freq 0.1d0)))
          (with ((sine-sig (sine-n-norm* partial freq phases))
                 (noise (randi-n* partial pbws)))
            (maybe-expand sine-sig)
            (maybe-expand noise)
            (foreach-frame
              (incf (frame-ref frm current-frame)
                    (+ (* sin-level
                          (aref amod partial)
                          (i-aref-n amps partial frameptr)
                          (frame-ref sine-sig current-frame))
                       (* res-level
                          (aref amod partial)
                          (i-aref-n pnoi partial frameptr)
                          (frame-ref sine-sig current-frame)
                          (frame-ref noise current-frame))))))))
      frm)))

(declaim (inline ats-noise-bank*))
(define-ugen ats-noise-bank* frame
    (frameptr
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
  (with ((num-bands (init-only (length noise-bws))))
    (declare (type integer num-bands))
    (with ((frm (make-frame (block-size)))
           (sin-phase-array (sample-array num-bands :initial-element 0.0d0))
;;;           (lag-array (sample-array num-bands))
           )
;;;      (initialize (format t "~&~a" num-bands))

      (foreach-frame (setf (frame-ref frm current-frame) 0.0d0))
      (dotimes (n num-bands)
        (let ((freq (aref noise-cfreqs n)))
          (with ((sine-sig (sine-n-norm* n freq sin-phase-array))
                 (noise-sig (randi-n* n noise-bws)))
            (maybe-expand sine-sig)
            (maybe-expand noise-sig)
            (foreach-frame
              (incf (frame-ref frm current-frame)
                    (* (i-aref-n noise-energy n frameptr)
                       (frame-ref sine-sig current-frame)
                       (frame-ref noise-sig current-frame)
                       ))))))
      frm)))

(declaim (inline ats-master-vug*))
(define-ugen ats-master-vug* frame
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
  (with ((frm (make-frame (block-size)))
         (sin-noi-sig (ats-sine-noi-bank*
                       frameptr freqs amps pnoi fmod amod partials res-bal))
         (noi-band-sig (ats-noise-bank* frameptr noise-cfreqs noise-bws noise-energy)))
    (maybe-expand sin-noi-sig)
    (maybe-expand noi-band-sig)
    (foreach-frame
      (setf (frame-ref frm current-frame)
            (+
             (frame-ref sin-noi-sig current-frame)
             (* (res-level res-bal) (frame-ref noi-band-sig current-frame))
             )))
    frm))

(define-ugen lag* frame
    (in time)
  "Scaled one pole filter with the coefficient calculated from
a 60 dB lag TIME."
  (with ((frm (make-frame (block-size))))
    (setf (frame-ref frm current-frame)
          (pole* in (t60->pole time)))
    frm))

(define-ugen mouse-x* frame
    ()
  (with ((frm (make-frame (block-size))))
    (setf (frame-ref frm current-frame)
          (mouse-x))
    frm))

(define-ugen mouse-y* frame
    ()
  (with ((frm (make-frame (block-size))))
    (setf (frame-ref frm current-frame)
          (mouse-y))
    frm))


(dsp! xy-sndpos-partial-ctl* ((arr (array sample)) (bw real)
                             (synth-id (or (unsigned-byte 62) node))
                             (num-partials (unsigned-byte 62))
                             (xlag-time real)
                             (ylag-time real))
  "A synth to control the center-freq (mouse-x) and the bw (mouse-y) of
the amplitude of partials in a sin-noi-rtc(-stretch)-synth."
  (:defaults (incudine::incudine-missing-arg "ARR") 1 0.5 1 1.5 1)
  (with-samples ((xpos-old 0)
                 (ypos-old 0)
                 (bw-old 0))
    (with ((xpos (lag* (mouse-x) (sample xlag-time)))
           (ypos (lag* (mouse-y) (sample ylag-time)))
           (bw-curr (lag* (sample bw) 1)))
      (maybe-expand xpos)
      (maybe-expand ypos)
      (maybe-expand bw-curr)
      (foreach-frame
        (when (or (/= (frame-ref bw-curr current-frame) bw-old)
                  (/= (frame-ref ypos current-frame) ypos-old)
                  (/= (frame-ref xpos current-frame) xpos-old))
          (set-control synth-id :soundpos (frame-ref xpos current-frame))
          (let ((fn (bias-cos
                     (frame-ref ypos current-frame)
                     (frame-ref bw-curr current-frame)
                     num-partials)))
            (dotimes (partial num-partials)
              (setf (aref arr partial) (funcall fn partial))))
          (setf xpos-old (frame-ref xpos current-frame))
          (setf ypos-old (frame-ref ypos current-frame))
          (setf bw-old (frame-ref bw-curr current-frame))))
      )))

(dsp! sin-noi-rtc-synth*
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

<fmod> Array of frequency modulation values. The arrayidx relates to
           the ATS partial with the same idx.

<amod> Array of amplitude modulation values. The arrayidx relates to
           the ATS partial with the same idx.

<pstretch> is the partial stretch in semitones per octave related to
           <base-freq>

<base-freq> base frequency pstretch is related to. Defaults to the
           first partial of the frq-av in the ats sound.

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
        (with ((sig (ats-master-vug*
                     frameptr
                     (ats-cuda::ats-sound-frq ats-sound)
                     (ats-cuda::ats-sound-amp ats-sound)
                     (ats-cuda::ats-sound-energy ats-sound)
                     (get-noise-bws (ats-cuda::ats-sound-bands ats-sound))
                     (get-noise-c-freqs (ats-cuda::ats-sound-bands ats-sound))
                     (ats-cuda::ats-sound-band-energy ats-sound)
                     partials
                     frq-mod
                     amp-mod
                     res-bal)))
          (maybe-expand sig)
          (foreach-frame
            (stereo (* curr-amp (frame-ref sig current-frame)))))))))

(export '(sin-noi-rtc-synth*) 'incudine)

#|
(dsp! phasor-bank-test ((freqs (simple-array sample)) (idxs list))
  (with ((frm (phasor-bank freqs idxs)))
    (maybe-expand frm)
    (foreach-frame
      (stereo (frame-ref frm current-frame)))))

(dsp! sine-bank-test ((freqs (simple-array sample))(amps (simple-array sample)) (idxs list))
  (with ((frm (sine-bank* freqs amps idxs)))
    (maybe-expand frm)
    (foreach-frame
      (stereo (frame-ref frm current-frame)))))

(let ((num 64))
  (sine-bank-test
   (make-array num :element-type 'sample
                   :initial-contents (loop for n from 1 to num collect (sample (* n 100))))
   (make-array num :element-type 'sample
                   :initial-contents (loop for n from 1 to num collect (sample (* 0.1 (/ num) (/ n num)))))
   (ats-cuda::range num)))

(let ((rt-block-size 1))
  (rt-stop)
  (set-rt-block-size rt-block-size)
  (rt-start))

(let ((num 8))
  (phasor-bank-test
   (make-array num :element-type 'sample
                   :initial-contents (loop for n from 1 to num collect (sample (* (expt n 23/24) 100))))
   (ats-cuda::range num)))

(expt 3 23/24)

(setf *rt-block-size* 32)
(rt-start)

(let ((frm (make-frame 32)))
  (loop for x below 32 collect (frame-ref frm x)))
|#
