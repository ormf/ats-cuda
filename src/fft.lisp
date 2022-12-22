;;; 
;;; fft.lisp
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

(in-package :cl-ats)

(in-package :incudine.gen)

(defwindow exact-blackman (foreign-array size)
  "Return a function called to fill a foreign array with an
exact Blackman (-51 dB) window.

The returned function takes two arguments, the foreign pointer to the
sample data and the data size, and returns the foreign array."
  (with-samples ((delta (/ +twopi+ (1- size)))
                 (phase +sample-zero+))
    (symmetric-loop (i j size foreign-array)
      (symmetric-set foreign-array i j
                     (+ (- 0.42659 (* 0.49656 (cos (the limited-sample phase))))
                        (* 0.07685 (cos (the limited-sample (* 2 phase))))))
      (incf phase delta))))

(defwindow blackman-harris-3-1 (foreign-array size)
  "Return a function called to fill a foreign array with a
3-term Blackman-Harris 1 (-67 dB) window.

The returned function takes two arguments, the foreign pointer to the
sample data and the data size, and returns the foreign array."
  (with-samples ((delta (/ +twopi+ (1- size)))
                 (phase +sample-zero+))
    (symmetric-loop (i j size foreign-array)
      (symmetric-set foreign-array i j
                     (+ (- 0.42323 (* 0.49755 (cos (the limited-sample phase))))
                        (* 0.07922 (cos (the limited-sample (* 2 phase))))))
      (incf phase delta))))

(defwindow blackman-harris-3-2 (foreign-array size)
  "Return a function called to fill a foreign array with a 3-term
Blackman-Harris 2 (-61 dB) window.

The returned function takes two arguments, the foreign pointer to the
sample data and the data size, and returns the foreign array."
  (with-samples ((delta (/ +twopi+ (1- size)))
                 (phase +sample-zero+))
    (symmetric-loop (i j size foreign-array)
      (symmetric-set foreign-array i j
                     (+ (- 0.44959 (* 0.49364 (cos (the limited-sample phase))))
                        (* 0.05677 (cos (the limited-sample (* 2 phase))))))
      (incf phase delta))))

(defwindow blackman-harris-4-1 (foreign-array size)
  "Return a function called to fill a foreign array with a 4-term
Blackman-Harris 1 (-92 dB) window.

The returned function takes two arguments, the foreign pointer to the
sample data and the data size, and returns the foreign array."
  (with-samples ((delta (/ +twopi+ (1- size)))
                 (phase +sample-zero+))
    (symmetric-loop (i j size foreign-array)
      (symmetric-set foreign-array i j
                     (+ (- 0.35875 (* 0.48829 (cos (the limited-sample phase))))
                        (* 0.14128 (cos (the limited-sample (* 2 phase))))
                        (* -0.01168 (cos (the limited-sample (* 4 phase))))))
      (incf phase delta))))

(defwindow blackman-harris-4-2 (foreign-array size)
  "Return a function called to fill a foreign array with a 4-term
Blackman-Harris 2 (-71 dB) window.

The returned function takes two arguments, the foreign pointer to the
sample data and the data size, and returns the foreign array."
  (with-samples ((delta (/ +twopi+ (1- size)))
                 (phase +sample-zero+))
    (symmetric-loop (i j size foreign-array)
      (symmetric-set foreign-array i j
                     (+ (- 0.40217 (* 0.49703 (cos (the limited-sample phase))))
                        (* 0.09392 (cos (the limited-sample (* 2 phase))))
                        (* -0.00183 (cos (the limited-sample (* 4 phase))))))
      (incf phase delta))))

(export '(blackman-harris-exact blackman-harris-3-1 blackman-harris-3-2
          blackman-harris-4-1 blackman-harris-4-2)
        'incudine.gen)

(in-package :incudine.analysis)

(defun fft->r-i-arrays (fft r-array i-array &optional (dir :input))
  "unzip complex values into left and right half."
  (let* ((vector-size (case dir
                 (:output (analysis-output-buffer-size fft))
                 (otherwise (analysis-input-buffer-size fft))))
         (buf (case dir
                (:output (analysis-output-buffer fft))
                (otherwise (analysis-input-buffer fft)))))
    (dotimes (n (ash vector-size -1))
      (setf (aref r-array n)
            (incudine:smp-ref buf (ash n 1)))
      (setf (aref i-array n)
            (incudine:smp-ref buf (1+ (ash n 1)))))
    (values r-array i-array)))

#|

(defparameter *fft* (make-fft 8 :window-function (incudine.gen:blackman-harris-3-1)))

(funcall (symbol-function (intern (symbol-name 'blackman-harris-3-1) :incudine.gen)))

(progn
  (dotimes (i (fft-size *fft*))
    (setf (fft-input *fft*) (if (zerop i) 1d0 0d0)))
  (compute-fft *fft* t)
  ;; 5 complex bins.
  (loop for i below (analysis-output-buffer-size *fft*)
        collect (smp-ref (analysis-output-buffer *fft*) i)))

(incudine.analysis:compute-fft *fft* t)


|#
