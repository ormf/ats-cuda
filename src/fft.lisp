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

(in-package :ats-cuda)

(in-package :incudine.analysis)

(defun fft->r-i-arrays (fft r-array i-array &key (direction :input))
  "unzip complex values into left and right half."
  (let* ((vector-size (analysis-input-buffer-size fft))
         (buf (case direction
                (:output (analysis-output-buffer fft))
                (otherwise (analysis-input-buffer fft)))))
    (dotimes (n (ash vector-size -1))
      (setf (aref r-array n)
            (incudine:smp-ref buf (ash n 1)))
      (setf (aref i-array n)
            (incudine:smp-ref buf (1+ (ash n 1)))))
    (values r-array i-array)))

(export 'fft->r-i-arrays 'incudine.analysis)

#|


(defparameter *fft* (make-fft 8 :window-function #'rectangular-window))

(progn
  (dotimes (i (fft-size *fft*))
    (setf (fft-input *fft*) (if (evenp i) 1d0 0d0)))
  
  (compute-fft *fft* t)
  ;; 5 complex bins.
  (list
   (loop for i below (analysis-input-buffer-size *fft*)
         collect (smp-ref (analysis-input-buffer *fft*) i))
   (loop for i below (analysis-output-buffer-size *fft*)
         collect (smp-ref (analysis-output-buffer *fft*) i))))

(defparameter *fft* (make-fft 8 :window-function #'rectangular-window))

(progn
  (setf (smp-ref (analysis-output-buffer *fft*) 8) 0.0d0)
  (setf (smp-ref (analysis-output-buffer *fft*) 9) 0.0d0)
  (loop for i from 0
        for rl in '(0.2d0 0.7d0 0.1d0 0.4d0)
        for img in '(0.4d0 0.3d0 0.9d0 0.1d0)
        do (setf (fft-input *fft*) rl)
           (setf (fft-input *fft*) img))
  (format t "~&~a~%"
          (list
           (loop for i below (ash (analysis-input-buffer-size *fft*) -1)
                 collect (smp-ref (analysis-input-buffer *fft*) (ash i 1)))
           (loop for i below (ash (analysis-input-buffer-size *fft*) -1)
                 collect (smp-ref (analysis-input-buffer *fft*) (1+ (ash i 1))))))
  (compute-fft *fft* t)
  ;; 5 complex bins, use only the first 4:
  (list
   (loop for i below (ash (analysis-output-buffer-size *fft*) -1)
              collect (smp-ref (analysis-output-buffer *fft*) (ash i 1)))
   (loop for i below (ash (analysis-output-buffer-size *fft*) -1)
         collect (smp-ref (analysis-output-buffer *fft*) (1+ (ash i 1))))))

((0.2d0 0.7d0 0.1d0 0.4d0) (0.4d0 0.3d0 0.9d0 0.1d0))



((0.2d0 0.7d0 0.1d0 0.4d0) (0.4d0 0.3d0 0.9d0 0.1d0))

(let ((fft (make-fft 8 :window-function #'rectangular-window))
      (res nil))
  (loop for i from 0
        for rl in '(0.2d0 0.7d0 0.1d0 0.4d0)
        for img in '(0.0d0 0.0d0 0.0d0 0.0d0)
        do
           (setf (fft-input fft) rl)
           (setf (fft-input fft) img))
  (setf res
        (loop for i below (analysis-input-buffer-size fft)
              collect (smp-ref (analysis-input-buffer fft) i)))
  (compute-fft fft t)
  (list
   res
   (loop for i below (ash (analysis-output-buffer-size fft) -1)
              collect (smp-ref (analysis-output-buffer fft) (ash i 1)))
   (loop for i below (ash (analysis-output-buffer-size fft) -1)
         collect (smp-ref (analysis-output-buffer fft) (1+ (ash i 1))))))

((0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0)
 (1.4000000000000001d0 0.1d0 -0.8d0 0.1d0 1.4000000000000001d0)
 (0.0d0 -0.29999999999999993d0 0.0d0 0.29999999999999993d0 0.0d0))

(let ((res nil)
      (fft (make-fft 8 :window-function #'rectangular-window)))
  (loop for i from 0 by 2
        for rl in '(0.2d0 0.7d0 0.1d0 0.4d0)
        for img in '(0.0d0 0.0d0 0.0d0 0.0d0)
        do
           (setf (smp-ref (analysis-input-buffer fft) i) rl)
           (setf (smp-ref (analysis-input-buffer fft) (1+ i)) img))
  (setf res
        (loop for i below (analysis-input-buffer-size fft)
              collect (smp-ref (analysis-input-buffer fft) i)))
  (compute-fft fft)
  (list
   res
   (loop for i below (analysis-input-buffer-size fft)
         collect (smp-ref (analysis-input-buffer fft) i))
   (loop for i below (analysis-output-buffer-size fft)
         collect (smp-ref (analysis-output-buffer fft) i))))


((0.2d0 0.0d0 0.7d0 0.0d0 0.1d0 0.0d0 0.4d0 0.0d0)
 (0.2d0 0.0d0 0.7d0 0.0d0 0.1d0 0.0d0 0.4d0 0.0d0)
 (0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0))

((0.2d0 0.0d0 0.7d0 0.0d0 0.1d0 0.0d0 0.4d0 0.0d0)
 (0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0)
 (0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0))

(in-package :ats-cuda)




(let ((fft-struct
        (make-ats-fft
         :size 4
         :rate 44100
         :fdr (make-double-float-array 4 :initial-contents '(0.2d0 0.7d0 0.1d0 0.4d0))
         :fdi (make-double-float-array 4 :initial-contents '(0.0d0 0.0d0 0.0d0 0.0d0)))))
  (clm:fft
   (ats-fft-fdr fft-struct)
   (ats-fft-fdi fft-struct)
   4
   -1)
  (list
   (ats-fft-fdr fft-struct)
   (ats-fft-fdi fft-struct)))

(#(1.4000000000000001d0 0.10000000000000002d0 -0.8d0 0.09999999999999999d0)
 #(0.0d0 -0.29999999999999993d0 0.0d0 0.29999999999999993d0))

(#(1.4000000000000001d0 0.30000000000000004d0 -0.8d0 -0.1d0)
 #(1.7000000000000002d0 -0.7999999999999999d0 0.9d0 -0.20000000000000007d0))

(#(1.4000000000000001d0 -0.09999999999999995d0 -0.8d0 0.29999999999999993d0)
 #(1.7000000000000002d0 -0.20000000000000007d0 0.9d0 -0.7999999999999999d0))




(incudine.analysis:compute-fft *fft* t)





         (fft-instance (incudine.analysis:make-fft
                        n :window-function nil))

(defparameter *fft* (make-fft 8 :window-function nil))

(progn
  (dotimes (i (fft-size *fft*))
    (setf (fft-input *fft*) (if (zerop i) 1d0 0d0)))
  (compute-fft *fft* t)
  ;; 5 complex bins.
  (loop for i below (analysis-output-buffer-size *fft*)
        collect (smp-ref (analysis-output-buffer *fft*) i)))

(incudine.analysis:compute-fft *fft* t)
|#


#|

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

|#
