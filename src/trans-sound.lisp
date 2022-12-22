;;;
;;; ATS 
;;; by Juan Pampin
;;; juan@ccrma.stanford.edu
;;;
;;; trans-sound.cl
;;;

(in-package :cl-ats)

;;; transposition function
(defun trans-sound (sound factor &key (name nil)(simp nil)(formants nil))
"
trans-sound <sound> <factor> &key [name] [simp][formants]
transposes <sound> by <factor>, if [name] exists 
a new sound is created, if [simp] is T partials with
average frequencies over half sampling-rate are eliminated. 
If [formants] is T amplitudes are scaled to keep the formant
structure of <sound>
"
  (let*((snd (symbol-value sound))
	(new-name (if name (string name) 
		    (concatenate 'string (ats-sound-name snd) "-trans")))
	(structure (if (or name formants)
		       (set (read-from-string new-name)
			    (copy-ats-sound snd new-name)) 
		     snd)))
    (format t "~%Transposing <~S>...~%" (ats-sound-name snd))
    (cond ((numberp factor)
	   (if (<= factor 0) (error "trans-sound : <factor> must be > 0"))
	   (do-partials structure un_norm_array ats-sound-frq factor))
          ((and (listp factor)(= (list-length factor) 1))
	   (do-partials-by-single-list structure multiply_array ats-sound-frq (first factor)))
	  ((listp factor)				
	   (if (/= (list-length factor) (ats-sound-partials structure))
	       (error "trans-sound : <factor> must have ~A elements." 
		      (ats-sound-partials structure)))
	   (do-partials-by-multiple-list structure un_norm_array multiply_array ats-sound-frq factor))
	   ;;; not a legal factor format
	  (t (error "what kind of transposition you want?")))
;;; update data
    (set-frq-av structure)
;;; simplify sound
    (when simp 
      (simplify-sound structure (scan-sound-frq structure))
      (optimize-sound structure))
;;; scale by formants
    (when formants
      (fit-formants structure snd) 
      (set-amp-av structure))
    (format t "~%trans-sound: ~S is done!" (ats-sound-name structure))))
