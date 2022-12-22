;;;
;;; ATS 
;;; by Juan Pampin
;;; juan@ccrma.stanford.edu
;;;
;;; do-partials.lisp
;;; general purpose partial iteration macros 

(in-package :cl-ats)

;;; applies single function through partials
(defmacro do-partials (sound function parameter value)
  `(loop for i from 0 below (ats-sound-partials ,sound) do
	 (format t "~S " i)
	 (,function (aref (,parameter ,sound) i) (ats-sound-frames ,sound) (float ,value))))

;;; applies a dynamic function through partials
(defmacro do-partials-by-single-list (sound function parameter val-list)
  `(let* ((n-frames (ats-sound-frames ,sound))
	  (a-val (make-array n-frames :initial-element 0.0))
	  (f-val (mapcar #'float ,val-list)))
    (loop for i from 0 below (ats-sound-partials ,sound) do
	  (format t "~S " i)
	  (int-points f-val (aref (ats-sound-time ,sound) i) a-val) 	; builds envelope array
	  (,function (aref (,parameter ,sound) i) a-val n-frames))))	; adds arrays

;;; applies a complex function trough partials
(defmacro do-partials-by-multiple-list (sound num-function list-function parameter val-list)
  `(let* ((n-partials (ats-sound-partials ,sound))
	  (n-frames (ats-sound-frames ,sound)))
     (loop for i from 0 below n-partials do
	   (format t "~S " i)
	   (let ((ele (elt ,val-list i)))
	     (cond ((numberp ele) ; number case
		    (,num-function (aref (,parameter ,sound) i) n-frames (float ele)))
		   ((listp ele) ; list case
		    (let ((a-val (make-array n-frames :initial-element 0.0))
			  (f-val (mapcar #'float ele)))
		      (int-points f-val (aref (ats-sound-time ,sound) i) a-val)
		      (,list-function (aref (,parameter ,sound) i) a-val n-frames)))
		   (t (error "bizarre type of element in transformation list.")))))))

