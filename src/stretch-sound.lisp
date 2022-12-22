;;;
;;; ATS 
;;; by Juan Pampin
;;; juan@ccrma.stanford.edu
;;;
;;; stretch-sound.cl
;;;

(in-package :cl-ats)

;;; stretch function
(defun stretch-sound (sound factor &key (name nil))
"
stretch-sound <sound> <factor> &key [name]
stretches <sound> by <factor>, if [name] exists 
a new sound is created
"
  (let*((snd (symbol-value sound))
	(new-name (if name (string name) 
		    (concatenate 'string (ats-sound-name snd) "-stretch")))
	(structure (if name
		       (set (read-from-string new-name)
			    (copy-ats-sound snd new-name)) 
		     snd)))
    (format t "~%Stretching <~S>...~%" (ats-sound-name snd))
    (cond ((numberp factor)
	   (if (<= factor 0) (error "stretch-sound : <factor> must be > 0"))
	   (do-partials structure un_norm_array ats-sound-time factor))
          ((and (listp factor)(= (list-length factor) 1))
	   (do-partials-by-single-list structure multiply_array ats-sound-time (first factor)))
	  ((listp factor)				
	   (if (/= (list-length factor) (ats-sound-partials structure))
	       (error "trans-sound : <factor> must have ~A elements." 
		      (ats-sound-partials structure)))
	   (do-partials-by-multiple-list structure un_norm_array multiply_array  ats-sound-time factor))
	   ;;; not a legal factor format
	  (t (error "what kind of stretch you want?")))
;;; update data
    (setf (ats-sound-dur structure)
	  (apply #'max (loop for i from 0 below (ats-sound-partials structure) collect
			 (max_array (aref (ats-sound-time structure) i)(ats-sound-frames structure)))))
    (format t "~%stretch-sound: ~S is done!" (ats-sound-name structure)))) 





