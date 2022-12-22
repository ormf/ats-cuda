;;;
;;; ATS 
;;; by Juan Pampin
;;; juan@ccrma.stanford.edu
;;;
;;; copy-sound.lisp
;;;

(in-package :cl-ats)

;;; copy of arrays of arrays
(defun copy-ats-data-array (array)
"
copy-ats-data-array <array>
copy an array of arrays
"
(let* ((len (first (array-dimensions array)))
       (new (make-array len :element-type 'array)))
  (do ((i 0 (1+ i)))
      ((= i len) new)
    (setf (aref new i) (copy-seq (aref array i))))))	
	
;;; copy of a sound
(defun copy-ats-sound (sound &optional (new-sound nil))
  
"
copy-sound <sound> &optional [new-sound]
copies <sound> into [new-sound] (def. <sound>-copy)
"
  
(let ((name (if new-sound (string new-sound) 
		(concatenate 'string (ats-sound-name sound) "-copy"))))
  (pushnew name *ats-sounds* :test #'equal)
  (set (read-from-string name)
       (make-ats-sound 
	:name name
	:sampling-rate (ats-sound-sampling-rate sound)
	:frame-size (ats-sound-frame-size sound)
	:window-size (ats-sound-window-size sound)
	:partials (ats-sound-partials sound)
	:frames (ats-sound-frames sound)
	:bands (if (ats-sound-bands sound)(copy-seq (ats-sound-bands sound)))
	:optimized (ats-sound-optimized sound)
	:ampmax (ats-sound-ampmax sound)
	:frqmax (ats-sound-frqmax sound)
	:frq-av (copy-seq (ats-sound-frq-av sound))
	:amp-av (copy-seq (ats-sound-amp-av sound))
	:dur (ats-sound-dur sound)
	:time (copy-ats-data-array (ats-sound-time sound))
	:frq (copy-ats-data-array (ats-sound-frq sound))
	:amp (copy-ats-data-array (ats-sound-amp sound)) 
	:pha (if (ats-sound-pha sound) (copy-ats-data-array (ats-sound-pha sound)) nil)
	:energy (if (ats-sound-energy sound)(copy-ats-data-array (ats-sound-energy sound)) nil)
	:band-energy (if (ats-sound-band-energy sound)(copy-ats-data-array (ats-sound-band-energy sound)) nil)))))

