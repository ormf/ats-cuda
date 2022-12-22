;;;
;;; ATS 
;;; by Juan Pampin
;;; juan@ccrma.stanford.edu
;;;
;;; c-fun.cl
;;;
;;; all these functions were supported as C functions
;;; using Lisp's ffi. In this new version of ATS we
;;; use their Lisp version instead (speed is almost
;;; the same).

;;; array.c

(in-package :cl-ats)

(defun max_array (a n)
"
returns the maximum value in <a>,
an array of floats of length <n>
"
	(let ((max 0.0))
	(do ((i 0 (1+ i)))
	    ((= i n) max)

	(if (> (abs (aref a i)) max)
		(setf max (abs (aref a i)))))))

(defun norm_array (a n m)
"
divides all the values in the array <a>,
by <m>
"
(do ((i 0 (1+ i)))
      ((= i n) nil)
	(setf (aref a i) (/ (aref a i) m))))

(defun un_norm_array (a n m)
"
multiplies all the values in the array <a>,
by <m>.
"
(do ((i 0 (1+ i)))
      ((= i n) nil)
	(setf (aref a i) (* (aref a i) m))))

(defun multiply_array (a b n)
"
multiply array <a> by array <b>,
both of length <n>.
"
(do ((i 0 (1+ i)))
      ((= i n) nil)
	(setf (aref a i) (* (aref a i) (aref b i)))))


(defun prom_array (a n)
  "
returns the average value of the array <a>
"
  (let ((sum 0.0)
	(cont 0))
    (do ((i 0 (1+ i)))
	((= i n) (if (> cont 0)(/ sum cont) 0.0))
      (let ((tmp (aref a i)))
	(incf sum tmp)
	(if (> tmp 0.0) (incf cont))))))


(defun sum_array (a b n)
"
adds array <a> to array <b>.
"
	(do ((i 0 (1+ i)))
	      ((= i n) nil)
	(setf (aref a i) (+ (aref a i) (aref b i)))))


(defun add_array (a n m)
"
adds <m> to all values in array <a>.
"
	(do ((i 0 (1+ i)))
	      ((= i n) nil)
	(setf (aref a i) (+ (aref a i) m))))


;;; interp_arrays.c

(defun interp_arrays (a b c d n)
  "
interpolates between arrays <a> and <b>,
using factors in array <c> and putting the
results in array <d>
"
  (do ((i 0  (1+ i)))
      ((= i n))
  (let ((tmp1 (aref a i))
        (tmp2 (aref b i))
        (ex (aref c i)))
    (if (< tmp1 tmp2)
      (setf (aref d i) (+ tmp1 (* ex (- tmp2 tmp1))))
      (setf (aref d i) (- tmp2 (* (- ex 1) (- tmp1 tmp2))))))))

(defun int_array_lin (a b prop n)
"
interpolates between positions
of array <a> using proportion
<prop>, the results are stored
in array <b>
"
(do ((i 0 (1+ i)))
    ((= i n))
  (multiple-value-bind (ent rest)
                       (floor (* i prop))
    (let* ((up (ceiling (* i prop)))
           (tmp1 (aref a ent))
           (tmp2 (aref a up)))
      (setf (aref b i) (+ tmp1 (* rest (- tmp2 tmp1))))))))

;;; inter.c

(defun inter_lin (a b &optional (ex 0.5))
  "
linear interpolation between <a> and <b>
for absissa's exedent of <ex>
"
  (if (<= a b) (+ a (* ex (- b a)))
    (- b (* (- ex 1)(- a b)))))

(defun inter_exp (a b &optional (ex 0.5))
  "
exponential interpolation between <a> and <b>
for absissa's exedent of <ex>
"
  (if (<= a b) (* a (expt (/ b a) ex))
      (/ b (expt (/ a b) (- ex 1)))))

;;; interleaves two lists

(defun cierre-l (l ll &optional (lll nil))
  "
returns a list with interleaved values from list <l>
and list <ll>.
"
  (if (null l) (nreverse lll)
                (let*((x (first l))
                        (y (first ll)))
                        (push x lll)
                        (push y lll)
                        (cierre-l (rest l) (rest ll) lll))))

;;; cierre.c

(defun cierre (a b c n)
"
interleaves two arrays <a> and <b>
into <c>
"
(do ((i 0 (1+ i))(pos 0))
    ((= i n) nil)
  (setf (aref c pos) (aref a i))
  (incf pos)
  (setf (aref c pos) (aref b i))
  (incf pos)))

;;; chronau.c

(defun chronau_c(b-tiempos a-val c-res len)
  "
returns an array with the new values of time after stretching:
b-tiempos : array of time
a-val : array of stretch values
c-res : returned array of new times (optional)
(original version by Denis Lorrain)
"
  (let*(  (Ri 0.0)
          (Riplus1 0.0)
          (Ei 0.0)
          (Eiplus1 0.0)
          (si 0.0)
          (siplus1 0.0)
          (Sii 0.0)
          (cr (if c-res c-res (make-double-float-array len :initial-element 0.0))))
    (do     ((i  0 (1+ i)))
            ((= i len) cr)
	    (let ((ii (- i 1)))
        (if (= i 0) (progn
                      (setf Ei (aref a-val i))
                      (setf si (aref b-tiempos i)))
            (progn
              (setf Ei (aref a-val ii))
              (setf si (aref b-tiempos ii))))
        (setf   Eiplus1 (aref a-val i)
                siplus1 (aref b-tiempos i)
                Sii (- siplus1 si))
        (if (= Ei Eiplus1)(setf Ri (* Sii Ei))
            (if (= Ei 0)
              (setf   Ri (/ (* Sii (- Eiplus1 Ei ))
                            (log (/ Eiplus1 0.000001) 10)))
              (setf   Ri (/ (* Sii (- Eiplus1 Ei ))
                            (log (/ Eiplus1 Ei) 10)))))
        (incf Riplus1 Ri)
        (setf (aref cr i) Riplus1)))))
           
               
(defun fill_array (a n m)
  "
fills array <a> of length <n>
with <m>
"
  (do ((i 0 (1+ i)))
      ((= i n) nil)
    (let ((tmp (aref a i)))
      (if (= tmp 0.0) (setf (aref a i) (coerce m 'short-float))))))








