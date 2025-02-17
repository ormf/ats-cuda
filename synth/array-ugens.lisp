;;; 
;;; array-ugens.lisp
;;;
;;; ugens using arrays of values for maintaining the state of multiple
;;; ("n") ugens within a single instance. This enables to run multiple
;;; ugens simultaneously indexing/traversing them in an enclosing dsp
;;; or vug.
;;;
;;; example:
;;;
;;; A straight forward way to do additive synthesis:
;;;
;;; (dsp! 3-partials (freq1 freq2 freq2)
;;;   (out (+ (sine freq1 0.2) (sine freq2 0.2) (sine freq3 0.2))))
;;;
;;; Using sine-n this can be done like this (with a variable number of
;;; sine ugens):
;;;
;;; (dsp! n-partials ((freqs (simple-array sample)))
;;;   (with-samples ((sig 0.0.d0))
;;;     (dolist (n (length freqs))
;;;       (incf sig (sine-n n (aref freqs n) 0.2)))
;;;    (out sig))
;;;       
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

(defun vec->array (vec)
  (let ((m (length vec))
        (n (length (aref vec 0))))
    (make-array (list m n) :element-type 'sample
                :initial-contents vec)))

(defmacro sample-array (length &key initial-contents (initial-element +sample-zero+))
  "shortcut for #'make-array with :element-type 'sample"
  `(if ,initial-contents
       (make-array ,length :element-type 'sample
                           :initial-contents ,initial-contents)
       (make-array ,length :element-type 'sample
                           :initial-element ,initial-element)))

(defmacro with-sample-arrays (bindings &body body)
  `(with ,bindings
     (declare (type (simple-array sample) ,@(mapcar #'first bindings)))
     ,@body))

(define-vug %phasor-n ((n integer) (phase-array (simple-array sample)) rate end)
  (prog1 (sample (aref phase-array n))
    (incf (aref phase-array n) rate)
    (cond ((>= (aref phase-array n) end) (decf (aref phase-array n) end))
          ((minusp (aref phase-array n)) (incf (aref phase-array n) end)))))

(define-vug phasor-n ((n integer) (phase-array (simple-array sample)) freq)
     "Produce a normalized moving phase value with frequency FREQ and
initial value INIT (0 by default).
phase-array is an array of phase values, n indexes into it."
     (:defaults (sample-array 1) 1 0)
     (with-samples ((rate (* freq *sample-duration*)))
       (%phasor-n n phase-array rate 1)))

(define-vug sine-n ((n integer) freq amp (sin-phase-array (simple-array sample)))
  "High precision sine wave oscillator with frequency FREQ, amplitude
AMP being the nth element of an array of sines with phase array PHASE-ARRAY."
  (:defaults 1 440 1 (sample-array 1))
  (with-sample-arrays
      ((phase-array (sample-array (length sin-phase-array))))
    (* amp (sin (+ (* +twopi+ (phasor-n n (the (simple-array sample) phase-array) freq))
                   (aref sin-phase-array n))))))

(define-vug tab-sine-n ((n integer) freq amp (sin-phase-array (simple-array sample)))
  "High precision sine wave oscillator with frequency FREQ, amplitude
AMP being the nth element of an array of sines with phase array PHASE-ARRAY."
  (:defaults 1 440 1 (sample-array 1))
  (with-sample-arrays
      ((phase-array (sample-array (length sin-phase-array))))
    (* amp (buffer-read *sine-table* (* 65536 (+ (phasor-n n (the (simple-array sample) phase-array) freq)
                                                 (aref sin-phase-array n)))))))
(define-vug sine-n-norm ((n integer) freq (sin-phase-array (simple-array sample)))
  "High precision sine wave oscillator with frequency FREQ and amplitude
1 with its phase stored in the nth element of the array SIN-PHASE-ARRAY."
  (:defaults 1 440 (sample-array 1))
  (with-sample-arrays
      ((phase-array (sample-array (length sin-phase-array))))
    (sin (+ (* +twopi+ (phasor-n n (the (simple-array sample) phase-array) freq))
            (aref sin-phase-array n)))))

(define-vug tab-sine-n-norm ((n integer) freq (sin-phase-array (simple-array sample)))
  "High precision sine wave oscillator with frequency FREQ and amplitude
1 with its phase stored in the nth element of the array SIN-PHASE-ARRAY."
  (:defaults 1 440 (sample-array 1))
  (with-sample-arrays
      ((phase-array (sample-array (length sin-phase-array))))
    (buffer-read *sine-table* (* 65536 (+ (phasor-n n (the (simple-array sample) phase-array) freq)
                                          (aref sin-phase-array n))))))

(define-vug pole-n ((n integer) in coef (arr (simple-array sample)))
  "One pole filter. (array version)"
  (setf (aref arr n) (+ in (* coef (aref arr n)))))

(define-vug pole*-n ((n integer) in coef (arr (simple-array sample)))
  "Scaled one pole filter (array version)."
  (with-samples ((g (- 1 (abs coef))))
    (pole-n n (* g in) coef arr)))

(define-vug lag-n ((n integer) in time (arr (simple-array sample)))
  "Scaled one pole filter with the coefficient calculated from
a 60 dB lag TIME (array version)."
  (pole*-n n in (t60->pole time) arr))

(define-vug-macro interpolate-n (n generator-form freqs
                                   &optional (interpolation :linear) initial-value-p)
     "Interpolation of the values generated by a performance-time GENERATOR-FORM.

The values of the generator are calculated with a modulable frequency FREQ.

INTERPOLATION is one of :LINEAR, :COS, :CUBIC or NIL (default).

If INTERPOLATION is :CUBIC and INITIAL-VALUE-P is T, three adjacent
points are initialized with the same value.

"
     (destructuring-bind (bindings init update ergebnis)
         (case interpolation
           ((:lin :linear)
            `(((x0s (sample-array (length freqs)))
               (x1s (sample-array (length freqs))))
              (setf (aref x1s n) input)
              (setf (aref x0s n) (aref x1s n) (aref x1s n) (update input) (aref deltas n) (- (aref x0s n) (aref x1s n)))
              (+ (aref x1s n) (* (aref phases n) (aref deltas n)))))
           (:cos
            `(((x0s (sample-array (length freqs)))
               (x1s (sample-array (length freqs))))
              (setf (aref x1s n) input)
              (setf (aref x0s n) (aref x1s n) (aref x1s n) (update input))
              (cos-interp (aref phases n) (aref x1s n) (aref x0s n))))
           (:cubic
            `(((x0s (sample-array (length freqs)))
               (x1s (sample-array (length freqs)))
               (x2s (sample-array (length freqs)))
               (x3s (sample-array (length freqs))))
              (setf (aref x1s n) input
                    ;; Three adjacent points initialized with the same
                    ;; value when it is required an initial value.
                    (aref x2s n) ,(if initial-value-p 'input `(update input))
                    (aref x3s n) ,(if initial-value-p 'input `(update input)))
              (setf (aref x0s n) (aref x1s n) (aref x1s n) (aref x2s n) (aref x2s n) (aref x3s n) (aref x3s n) (update input))
              (cubic-interp phase (aref x3s n) (aref x2s n) (aref x1s n) (aref x0s n))))
           (otherwise
            `(((x0s (sample-array (length freqs))))
              nil (setf (aref x0s n) (update input)) (aref x0s n))))
       (with-gensyms (interp-n)
         `(vuglet ((,interp-n ((n integer) input (freqs (simple-array sample)))
                              (with-sample-arrays
                                  ((phases (sample-array (length freqs)))
                                   (deltas (sample-array (length freqs)))
                                   (incs (sample-array (length freqs)))
                                   ,@bindings)
                                (initialize
                                 (loop
                                   for n below (length freqs)
                                   do (progn
                                        (setf (aref phases n) 0.0d0)
                                        (setf (aref incs n) (* (aref freqs n) *sample-duration*))
                                        ,init)))
                                (decf (aref phases n) (aref incs n))
                                (when (minusp (aref phases n))
                                  (setf (aref phases n) (wrap (aref phases n) 0 1))
                                  ,update)
                                ,ergebnis)))
            (,interp-n ,n ,generator-form ,freqs)))))

(define-vug randi-n ((n integer) (freqs (simple-array sample)))
  (:defaults 1 (sample-array 1 :initial-element 440.0d0))
  (interpolate-n n (white-noise) freqs))

(define-vug sine-bank ((freqs (simple-array sample))
                       (amps (simple-array sample))
                       (phases (simple-array sample)))
  "a bank of sine oscillators with freqs, amps and initial phases
provided as arrays of type sample. The length of the freqs array
determines the number of sine waves to generate in parallel and the
size of the other two arrays has to be >= the size of the freqs
array."
  (with-samples ((out 0))
    (setf out 0.0d0)
    (dotimes (n (length freqs))
      (incf out (sine-n n (aref freqs n) (aref amps n) phases)))
    out))

(define-vug noise-bank ((freqs (simple-array sample))
                        (amps (simple-array sample))
                        (bws (simple-array sample)))
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
                     (randi-n n bws))))
      out)))

(export '(sample-array) 'incudine)

#|

Examples:

(dsp! test3 ((freq-array (simple-array sample))
             (amp-array (simple-array sample)))
  (:defaults (make-array 2 :element-type 'sample :initial-contents '(440.0d0 880.0d0))
             (make-array 2 :element-type 'sample :initial-contents '(0.05d0 0.03d0)))
  (with ((out 0.0d0)
         (init-n  (length freq-array))
         (phase-array (make-array init-n :element-type 'sample :initial-element 0.0d0))
         (sine-phase-array (make-array init-n :element-type 'sample :initial-element 0.0d0)))
    (declare (type sample out)
             (type (array sample) phase-array sine-phase-array))
    (setf out 0.0d0)
    (loop
      for n below init-n
      do (incf out (sine-n n phase-array (aref freq-array n) (aref amp-array n) sine-phase-array)))
    (stereo out)))

(dsp! bl-noise-test-n ((freqs (simple-array sample)) (bws (simple-array sample)) (amps (simple-array sample)))
    (with ((out 0.0d0)
           (init-n (init-only (format t "~%init-n: ~a~%" (length freqs)) (length freqs)))
           (phase-array (init-only (make-array init-n :element-type 'sample :initial-element 0.0d0)))
           (sine-phase-array (init-only (make-array init-n :element-type 'sample :initial-element 0.0d0))))
    (declare (type sample out)
             (type (array sample) phase-array sine-phase-array))
    (loop
      for n below init-n
      do (progn
;;           (format t "~&n: ~a" n)
           (incf out
                 (* (sine-n n phase-array (aref freqs n) (aref amps n) sine-phase-array)
                    (randi-n n bws)))))
    (stereo out)))

(defparameter *amp-test* (make-array 3 :element-type 'sample :initial-contents '(0.05d0 0.03d0 0.04d0)))

(bl-noise-test-n
 (make-array 3 :element-type 'sample :initial-contents '(440.0d0 790.0d0 1250.0d0))
 (make-array 3 :element-type 'sample :initial-contents '(20.0d0 20.0d0 100.0d0))
 *amp-test*
 :id 1)


(setf (aref *amp-test* 0) 0.03d0)


(test3
 :freq-array (make-array 3 :element-type 'sample :initial-contents '(440.0d0 790.0d0 1250.0d0))
 :amp-array *amp-test*
 :id 1)

(setf (aref *amp-test* 0) 0.03d0)

(set-control 1 :freq-array
             (make-array 3 :element-type 'sample :initial-contents '(450.0d0 890.0d0 1050.0d0)))

(node 1)

(free 1)

(test3
 :freq-array (make-array 3 :element-type 'sample :initial-contents '(440.0d0 790.0d0 1250.0d0))
 :amp-array (make-array 3 :element-type 'sample :initial-contents '(0.05d0 0.03d0 0.04d0)))

(test3 (make-array 3 :element-type 'sample :initial-contents '(440.0d0 660.0d0 1250.0d0))
       (make-array 3 :element-type 'sample :initial-contents '(0.05d0 0.02d0 0.01d0)))

|#

