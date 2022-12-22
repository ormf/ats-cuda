;;;
;;; ATS 
;;; by Juan Pampin
;;; juan@ccrma.stanford.edu
;;;
;;; copy-sound.lisp
;;;

(in-package :cl-ats)

;;; frequency-shift function
(defun shift-sound (sound shift &key (name nil) (simp nil)(formants nil))
"
shift-sound <sound> <shift> &optional [name] [simp][formants]
frequency shift of <sound> by <shift>, if [name] exists 
a new sound is created, if [simp] is T partials with
average frequencies over half sampling-rate are eliminated. 
If [formants] is T amplitudes are scaled to keep the formant
structure of <sound>
"
(let* ((snd (symbol-value sound))
       (new-name (if name (string name) 
		   (concatenate 'string (ats-sound-name snd) "-shift")))
       (structure (if (or name formants)
		      (set (read-from-string new-name) 
			   (copy-ats-sound snd new-name)) 
		    snd)))
  (format t "~%Shifting <~S>...~%" (ats-sound-name snd))
  (cond ((numberp shift)
	 (do-partials structure add_array ats-sound-frq shift))
        ((and (listp shift) (= (list-length shift) 1))
         (do-partials-by-single-list structure sum_array ats-sound-frq (first shift)))
	((listp shift)
	 (if (/= (list-length shift) (ats-sound-partials structure))
	     (error "shift-sound : <shift> must have ~A elements." (ats-sound-partials structure)))
	 (do-partials-by-multiple-list structure add_array sum_array ats-sound-frq shift))
	(t (error "what kind of shift you want?")))
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
  (format t "~%shift-sound: ~S is done!" (ats-sound-name structure))))
