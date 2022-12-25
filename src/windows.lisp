;;;
;;; ATS 
;;; by Juan Pampin
;;; juan@ccrma.stanford.edu
;;;
;;; File: windows.cl
;;; ================
;;; This file contains the implementation
;;; of ATS's analysis windows

(in-package :ats-cuda)

;;; All data coming form Harris' famous paper:
;;; "On the Use Of windows For Harmonic Analysis 
;;;  With The Discrete Fourier Transform"
;;; Proceedings of the IEEE, Vol. 66, No. 1 (pg. 51 to 84)
;;; January 1978

;;; Window coeffs. (a0, a1, a2, a3) 

#|
(defparameter ats-blackman-window-coeffs 
  (make-array 6 :initial-contents '((0.42659 -0.49656 0.07685) ;;; Exact Blackman (-51 dB)
				    (0.42 -0.5 0.08) ;;; Blackman (rounded coeffs) (-58 dB)
				    (0.42323 -0.49755 0.07922) ;;; 3-term Bkackman-Harris 1 (-67 dB)
				    (0.44959 -0.49364 0.05677) ;;; 3-term Bkackman-Harris 2 (-61 dB)
				    (0.35875 -0.48829 0.14128 -0.01168) ;;; 4-term Bkackman-Harris 1 (-92 dB)
				    (0.40217 -0.49703 0.09392 -0.00183)))) ;;; 4-term Bkackman-Harris 2 (-71 dB)
|#				    

(defparameter ats-blackman-window-coeffs 
  '(exact-blackman (0.42659 -0.49656 0.07685 nil) ;;; Exact Blackman (-51 dB)
    blackman (0.42 -0.5 0.08 nil) ;;; Blackman (rounded coeffs) (-58 dB)
    blackman-harris-3-1 (0.42323 -0.49755 0.07922 nil) ;;; 3-term Blackman-Harris 1 (-67 dB)
    blackman-harris-3-2 (0.44959 -0.49364 0.05677 nil) ;;; 3-term Blackman-Harris 2 (-61 dB)
    blackman-harris-4-1 (0.35875 -0.48829 0.14128 -0.01168) ;;; 4-term Blackman-Harris 1 (-92 dB)
    blackman-harris-4-2 (0.40217 -0.49703 0.09392 -0.00183)))

;;; Window creation:
;;; we generate a short-float array with the window values for each case.
(defun make-blackman-window (type M)
  (let* ((coeffs (getf ats-blackman-window-coeffs type))
	 (two-pi-over-M (/ +two-pi+ M))
	 (four-pi-over-M (/ (* 2 +two-pi+) M))
	 (six-pi-over-M (/ (* 3 +two-pi+) M))
	 (win (make-double-float-array M :initial-element 0.0)))
    (unless coeffs (error "Window type ~a not found" type))
    (destructuring-bind (a0 a1 a2 a3) coeffs
      (dotimes (i M)
        (setf (aref win i)
	      (dfloat 
	       (+ a0 
		  (* a1 (cos (* two-pi-over-M i))) 
		  (* a2 (cos (* four-pi-over-M i)))
		  (if a3 (* a3 (cos (* six-pi-over-M i)))
		      0.0))))))
    win))
	       
	 
;;; window tools
		      
(defun window-norm (window)
  "returns the norm of a window"
  (/ 2.0 (loop for amp across window sum (abs amp))))

#|
(defun window-norm (window)
"returns the norm of a window"
  (let ((acc 0))
    (dotimes (i (length window))
      (incf acc (abs (aref window i))))
    (/ 2.0 acc)))

(window-norm #(1 2 3 -4))
|#



