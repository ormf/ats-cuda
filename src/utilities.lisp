;;;
;;; ATS 
;;; by Juan Pampin
;;; juan@ccrma.stanford.edu
;;;
;;; utilities.cl
;;;

(in-package :ats-cuda)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Defaults and constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; some useful constants as double floats
(defconstant +pi-over-two+ (dfloat (/ pi 2.0)))
(defconstant +two-pi+ (dfloat (* pi 2.0)))

;;; some system defaults
(defparameter *ats-max-db-spl* 100.0)
(defparameter *ats-critical-bands* 25)
(defparameter *ats-amp-threshold* -60)
(defparameter *ats-min-segment-length* 3)
(defparameter *ats-noise-threshold* -120)

;;; variable to keep names of loaded sounds
(defparameter *ats-sounds* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; array utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Version with 2D-Array:

(if *2d-arrays*
;;;; Version with 2D-Arrays:
        (progn
      (defun make-ats-array (partials frames)
        (make-array (list partials frames) :element-type 'double-float))
      (defun ats-aref (array partial frame)
        (aref array partial frame))
      (defun ats-setf (array partial frame val)
        (setf (aref array partial frame) val))
      (defun (setf ats-aref) (val array partial frame)
        (setf (aref array partial frame) val))
      (defun array-slice (arr row)
        "get a row of a 2D Array as a 1D Array of the same type."
        (make-array (array-dimension arr 1)
                    :element-type (array-element-type arr)
                    :displaced-to arr 
                    :displaced-index-offset (* row (array-dimension arr 1))))
      (defun copy-row (src j dest i)
        "copy row j from src to row i in dest in 2D double-float arrays.
Arrays need to have equal dimension 1"
        (if (= (ats-array-frames src) (ats-array-frames dest))
            (loop for val across (array-slice src j)
                  for frm from 0
                  do (setf (ats-aref dest i frm) val))
            (error "num frames doesn't match: src dest")))
      (defun ats-array-partials (arr)
        (array-dimension arr 0))
      (defun ats-array-frames (arr)
        (array-dimension arr 1)))
;;;; Version with Array of Arrays:
    (progn
      (defun make-ats-array (partials frames)
        (make-array partials :element-type '(array (array double-float *))
                               :initial-contents
                               (loop for i below partials collect
                                          (make-array frames :element-type 'double-float :initial-element (double 0.0)))))

      (defun ats-aref (array partial frame)
        (aref (aref array partial) frame))
      (defun (setf ats-aref) (val array partial frame)
        (setf (aref (aref array partial) frame) val))

      (defun array-slice (arr row)
        "get a row of a 2D Array as a 1D Array of the same type."
        (aref arr row))

      (defun copy-row (src j dest i)
        "copy row j from src to row i in dest in 2D double-float arrays.
Arrays need to have equal dimension 1"
        (if (= (ats-array-frames src) (ats-array-frames dest))
            (loop for val across (array-slice src j)
                  for frm from 0
                  do (setf (ats-aref dest i frm) val))
            (error "num frames doesn't match: src dest")))

      (defun ats-array-partials (arr)
        (length arr))
      (defun ats-array-frames (arr)
        (length (aref arr 0)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; soundfile utils (loading and saving soundfiles into/from lisp
;;; arrays
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-input-data (file &optional (offs 0))
  (let* ((fil (open-input* file))
         (num (sound-framples fil))
         (arr (make-double-float-array num :initial-element (double 0.0))))
    (get-samples file arr :offs offs)))

(defun get-n-input-data (file num &optional (offs 0))
  (let* ((arr (make-double-float-array num :initial-element (double 0.0))))
    (get-samples file arr :offs offs)))

;;; (get-input-data (concatenate 'string *ats-snd-dir* "clarinet.aif"))

(definstrument get-samples (file arr &key (offs 0) (chan 0))
  "read samples of a soundfile into a supplied array. Returns the array"
  (let ((num (length arr)))
    (file->array file chan offs num arr)
    arr))

(defun sfile->array (file &key (offs 0) (chan 0) count)
  "read a soundfile into a freshly allocated array and return the
array."
  (let* ((fil (open-input* file))
         (num (if count
                  (min count (- (sound-framples fil) offs))
                  (- (sound-framples fil) offs)))
         (arr (make-double-float-array num :initial-element (double 0.0))))
    (get-samples file arr :offs offs :chan chan)))

(definstrument set-samples (fname arr frample-num rate chans)
  (array->file fname arr (* frample-num chans) rate chans)
  fname)

(defun array->sfile (fname arrays &key (offs 0) (rate 44100) count)
  "save <arrays> into a soundfile. <arrays> is a seq of arrays, each
containing the sound data of one channel. The arrays have to have
equal size."
  (let ((len (length (first arrays))))
    (unless (apply #'= (cons len (mapcar #'length (cdr arrays))))
      (error "array->sfile: array lengths don't match!"))
    (let* ((num (if count
                    (max 0 (min count (- len offs)))
                    (max 0 (- len offs))))
           (chans (length arrays))
           (array (make-array (* chans num) :element-type 'double-float :initial-element (double 0.0)))
           (arr-ptr 0))
      (dotimes (i num)
        (dotimes (o chans)
          (setf (aref array arr-ptr)
                (aref (elt arrays o) i))
          (incf arr-ptr)))
      (set-samples fname array num rate chans))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General sound init tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; add new sound to the list of sounds
(defmacro add-sound (sound)
  `(pushnew (ats-sound-name ,sound) *ats-sounds* :test #'equal))

;;; maximum amplitude
(defun get-ampmax (sound)
  "
get-ampmax <sound>
gets the maximum amplitude of <sound>
"
  (let ((ampmax 0)
	(tmp 0)
	(frames (ats-sound-frames sound)))
    (do ((h 0 (1+ h)))
        ((= h (ats-sound-partials sound)) 
	 ampmax)
      (if (> (setf tmp (max_array (array-slice (ats-sound-amp sound) h) frames)) ampmax)
	(setf ampmax tmp)))))

;;; maximum frequency
(defun get-frqmax (sound)
  "
get-frqmax <sound>
gets the maximum frequency of <sound>
"
  (let ((frqmax 0)
	(tmp 0)
	(frames (ats-sound-frames sound)))
    (do ((h 0 (1+ h)))
        ((= h (ats-sound-partials sound)) 
	 frqmax)
      (if (> (setf tmp (max_array (array-slice (ats-sound-frq sound) h) frames)) frqmax)
	  (setf frqmax tmp)))))

;;; normalize amplitudes
(defun norm-amp (sound)
  "
norm-amp <sound>
nomalizes amplitudes of <sound> to 1
"
  (let ((ampmax (if (ats-sound-ampmax sound)
		    (ats-sound-ampmax sound)
		  (get-ampmax sound)))
	(frames (ats-sound-frames sound)))
    (if (= ampmax 0.0)
	(error "oops! ~S has ampmax of 0.0" (ats-sound-name sound))
      (do ((h 0 (1+ h)))
	  ((= h (ats-sound-partials sound)) 'done)
	(norm_array (array-slice (ats-sound-amp sound) h) frames ampmax)))))

;;; set the average amplitude for each partial
(defun set-amp-av (sound)
  "
sets the average amplitude for each partial of <sound>
"
  (if (not (ats-sound-amp-av sound))
      (setf (ats-sound-amp-av sound)
	(make-double-float-array (ats-sound-partials sound) :initial-element 0.0)))
  (let ((frames (ats-sound-frames sound)))
    (loop for i from 0 below (ats-sound-partials sound) do
	  (if (> (max_array (array-slice (ats-sound-amp sound) i) frames) 0.0)
	      (setf (aref (ats-sound-amp-av sound) i)
		(prom_array (array-slice (ats-sound-amp sound) i) frames))))))


;;; set the average frequency for each partial
(defun set-frq-av (sound)
  "
sets the average frequency for each partial of <sound>
"
  (if (not (ats-sound-frq-av sound))
      (setf (ats-sound-frq-av sound)
	(make-double-float-array (ats-sound-partials sound) :initial-element 0.0)))
  (let ((frames (ats-sound-frames sound)))
    (loop for i from 0 below (ats-sound-partials sound) do
	  (if (> (max_array (array-slice (ats-sound-frq sound) i) frames) 0.0)
	      (setf (aref (ats-sound-frq-av sound) i)
		(prom_array (array-slice (ats-sound-frq sound) i) frames))))))

;;; returns a list with partials numbers over Fs/2
(defun scan-sound-frq (sound)
  "
 scan <sound> searching for partials  over Fs/2
 returns a list of valid partials
"
  (let ((limit (/ (ats-sound-sampling-rate sound) 2.0))
	(l nil))
    (do ((i 0 (1+ i)))
	((= i (ats-sound-partials sound)) (reverse l))
      (if (< (aref (ats-sound-frq-av sound) i) limit)
	  (push i l)))))

;;; get the valid partials of a sound 
;;; returns a list with the valid partial numbers
(defun get-valid-partials (sound &optional 
				 (min-frq 0.0)
				 (max-frq 20000.0)
				 (amp-threshold *ats-amp-threshold*))
  "
returns a list with the valid partial numbers
valid partials are those with frq-av >= *ats-amp-threshold*
and frq-av within min-frq and max-frq
"
  (let ((l))
    (do ((i 0 (1+ i)))
	((= i (ats-sound-partials sound)) (reverse l))
      (if (and (>= (aref (ats-sound-amp-av sound) i)
		   (db-amp amp-threshold))
	       (<= min-frq (aref (ats-sound-frq-av sound) i) max-frq))
	  (push i l)))))

;;; (type-of (make-array 25 :element-type '(simple-array double-float *)))

(defun init-sound (sound &key sampling-rate frame-size window-size frames duration partials 
			 (has-phase T)(has-noise NIL)(bands *ats-critical-bands*))
  "Initializes an ATS sound"
  (setf (ats-sound-sampling-rate sound) sampling-rate)
  (setf (ats-sound-frame-size sound) frame-size)
  (setf (ats-sound-window-size sound) window-size)
  (setf (ats-sound-partials sound) partials)
  (setf (ats-sound-frames sound) frames)
  (setf (ats-sound-dur sound) duration)
  (setf (ats-sound-frq-av sound) (make-double-float-array partials :initial-element 0.0))
  (setf (ats-sound-amp-av sound) (make-double-float-array partials :initial-element 0.0))
  (setf (ats-sound-time sound) (make-ats-array partials frames))
  (setf (ats-sound-frq sound) (make-ats-array partials frames))
  (setf (ats-sound-amp sound) (make-ats-array partials frames))
  (if has-phase
      (setf (ats-sound-pha sound) (make-ats-array partials frames)))
  (if has-noise
      (setf (ats-sound-band-energy sound) (make-ats-array bands frames))))

(defmacro n-collect (n form &key (initial-element '()))
  "return a seq of n elems prepended to initial-element by
evaluating form n times with the symbol n bound to the iteration
index in the lexical scope of form."
  `(labels
       ((fn (idx &optional seq)
          (if (< idx ,n)
              (cons
               (let ((n idx))
                 (declare (ignorable n))
                 ,form)
               (fn (+ idx 1) seq))
              seq)))
     (fn 0 ,initial-element)))



(defun simplify-sound (sound valid)
  "
eliminates unvalid partials from <sound>
valid partials in <valid> list
"
  (let* ((n-frames (ats-array-frames (ats-sound-frq sound)))
         (n-partials (list-length valid))
	 (n-time (make-ats-array n-partials n-frames))
	 (n-amp (make-ats-array n-partials n-frames))
	 (n-frq (make-ats-array n-partials n-frames))
	 (n-pha (if (ats-sound-pha sound)
		    (make-ats-array n-partials n-frames)))
	 (n-noi (if (ats-sound-energy sound)
		    (make-ats-array n-partials n-frames)))
	 (n-amp-av (make-double-float-array n-partials))
	 (n-frq-av (make-double-float-array n-partials)))
    (when valid
      ;;; we also sort partials by frequency here
      (let* ((sorted-valid (sort (loop for i in valid collect (list i (aref (ats-sound-frq-av sound) i)))
				 #'< :key #'second)))
	(loop 
	  for sv in sorted-valid
	  for i from 0 do
	    (let ((j (first sv)))
              (copy-row (ats-sound-time sound) j n-time i)
              (copy-row (ats-sound-amp sound) j n-amp i)
              (copy-row (ats-sound-frq sound) j n-frq i)              
	      (setf (aref n-amp-av i)(aref (ats-sound-amp-av sound) j)
		    (aref n-frq-av i)(aref (ats-sound-frq-av sound) j))
	      (if n-pha (copy-row (ats-sound-pha sound) j n-pha i))
	      (if n-noi  (copy-row (ats-sound-energy sound) j n-noi i)))))
    ;;; now set the slots
      (setf (ats-sound-time sound) n-time
	    (ats-sound-amp sound) n-amp
	    (ats-sound-frq sound) n-frq
	    (ats-sound-amp-av sound) n-amp-av
	    (ats-sound-frq-av sound) n-frq-av
	    (ats-sound-partials sound) n-partials)
      (if n-pha
	  (setf (ats-sound-pha sound) n-pha))
      (if n-noi
	  (setf (ats-sound-energy sound) n-noi))
      'done)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Auxiliary functions and macros to deal with short segments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fills zeroes interpolating values
(defun fill-gaps (arr)
  "
fills zeroes in <arr>
by interpolation
"
  (do ((i 0 (1+ i)))
      ((= i (length arr)) arr)
    (if (= (aref arr i) 0.0)
	(let* ((ax (if (= i 0) 0 (1- i))) 
	       (a (aref arr ax))
	       (bx (find-next-val-arr arr i))
	       (b (aref arr bx)))
	  (setf (aref arr i)(envelope-interp i (list ax a bx b)))))))

;;; auxiliary functions to fill-frq-gaps				    
(defun find-next-val-arr (arr j)
  (if (or (> (aref arr j) 0) (= j (1- (length arr))))
      j
    (find-next-val-arr arr (incf j))))

(defun find-next-zero-arr (arr j)
  (if (or (= (aref arr j) 0) (= j (1- (length arr))))
      j
    (find-next-zero-arr arr (incf j))))

(defun find-prev-val-arr (arr j)
  (if (or (> (aref arr j) 0) (= j 0))
      j
    (find-prev-val-arr arr (decf j))))

;;; count number of 0s in a an array
(defun zero-count-arr (arr)
  (loop for j from 0 below (length arr) sum
    (if (= (aref arr j) 0) 1.0 0.0)))

;;; returns a list of lists containing
;;; the index of the segment and its length
(defun segments-arr (arr &optional (k 0) (in-seg nil)(st 0)(l nil))
  (if (= k (length arr))
      (nreverse (if in-seg (push (list st (- k st)) l) l))
    (progn
      (if (and (= (aref arr k) 0.0) in-seg)
	  (progn
	    (push (list st (- k st)) l)
	    (setf in-seg nil))
	(if (and (/= (aref arr k) 0.0) (not in-seg))
	    (setf st k
		  in-seg t)))
      (segments-arr arr (incf k) in-seg st l))))

;;; get gap positions and lengths
(defun get-gaps-arr (arr)
  (let ((segs (segments-arr arr))
	(st 0)
	(l))
    (dolist (k segs)
      ;;; check begining
      (if (= (first k) 0)
	  (incf st (second k))
	(progn 
	  (push (list st (- (first k) st)) l)
	  (setf st (+ (first k)(second k))))))
    (nreverse l)))
	  
;;; Interpolates to fill zeroes
(defun fill-frq-gaps (sound)
  "
fills gaps in the frequencies of
<sound> using interpolation
should be done after finding out frq-av
"
  (let ((last-frame (1- (ats-sound-frames sound))))
    (do ((i 0 (1+ i)))
	((= i (ats-sound-partials sound)) 'done)
      ;;; check if first frq value is 0.0
      ;;; in this case set it to the first available frequency
      ;;; value in the partial
      (if (= (ats-aref (ats-sound-frq sound) i 0) 0.0)
	  (setf (ats-aref (ats-sound-frq sound) i 0)
		(ats-aref (ats-sound-frq sound) i 
		      (find-next-val-arr (aref (ats-sound-frq sound) i) 0))))
      ;;; do the same for the end of the partial
      (if (= (ats-aref (ats-sound-frq sound) i last-frame) 0.0)
	  (setf (ats-aref (ats-sound-frq sound) i last-frame)
		(ats-aref (ats-sound-frq sound) i 
		      (find-prev-val-arr (array-slice (ats-sound-frq sound) i) last-frame))))
      ;;; now we are ready to go!
      (fill-gaps (aref (ats-sound-frq sound) i)))))

;;; interpolates phase values (hummm... need to review this one!) JP
(defun fill-phase-gaps (sound)
  (loop for i from 0 below (ats-sound-partials sound) do
    (let ((segments (segments-arr (aref (ats-sound-amp sound) i))))
      (if (> (list-length segments) 1)
	  (let ((st 0))
	    (dolist (seg segments)
	      (if (= (first seg) 0)
		  (incf st (second seg))
		(let* ((nd (first seg))
		       (frq-1 (* (ats-aref (ats-sound-frq sound) i st) +two-pi+))
		       (frq-2 (* (ats-aref (ats-sound-frq sound) i nd) +two-pi+))
		       (dt (- (ats-aref (ats-sound-time sound) i nd)
			      (ats-aref (ats-sound-time sound) i st)))
		       (t-inc (/ dt (- nd st)))
		       (pha-2 (ats-aref (ats-sound-pha sound) i nd))
		       (pha-1 (mod (- pha-2 (* frq-2 dt)) +two-pi+))
		       (M (compute-M pha-1 frq-1 pha-2 frq-2 dt))
		       (aux (compute-aux pha-1 pha-2 frq-1 dt M))
		       (alpha (compute-alpha aux frq-1 frq-2 dt))
		       (beta (compute-beta aux frq-1 frq-2 dt)))
		  (loop 
		    for k from st below nd 
		    for time from 0 by t-inc
		    do
		    (setf (ats-aref (ats-sound-pha sound) i k)
			  (float (interp-phase pha-1 frq-1 alpha beta time) 1.0d0)))
		  (incf st (second seg))))))))))
	     
(defun fill-sound-gaps (sound &optional (min-length *ats-min-segment-length*))
  "fill up parameter for gaps shorter or equal to min-length"
  (let* ((srate (ats-sound-sampling-rate sound))
	 (mag (/ +two-pi+ srate))
	 (frame-size (ats-sound-frame-size sound)))
  (loop for par from 0 below (ats-sound-partials sound) do
    (let* ((amp-arr (array-slice (ats-sound-amp sound) par))
	   (frq-arr (array-slice (ats-sound-frq sound) par))
	   (next-val 0)
	   (gaps (get-gaps-arr amp-arr)))
      ;;; first we fix the freq gap before attack
      (if (> (setf next-val (find-next-val-arr frq-arr 0)) 0)
	  (loop for f from 0 below next-val do
	    (setf (aref frq-arr f) (aref frq-arr next-val))))
      ;;; now we fix inner gaps of frq, pha, and amp
      (loop for k in gaps do
	(if (<= (second k) min-length)
	    (let* ((left (if (zerop (first k)) 0 (1- (first k))))
		   (right (if (> (+ (first k)(second k))(ats-sound-frames sound))
			      (1- (ats-sound-frames sound))
			    (+ (first k)(second k)))))
	      ;;; we know the boundaries of the gap, now let's fill it out...
	;;; frq
	      (loop for j from (first k) below (+ (first k)(second k)) do
		(cond ((= (ats-aref (ats-sound-amp sound) par left) 0.0)
		       (setf (ats-aref (ats-sound-frq sound) par j) 
			     (ats-aref (ats-sound-frq sound) par right)))
		      ((= (ats-aref (ats-sound-amp sound) par right) 0.0)
		       (setf (ats-aref (ats-sound-frq sound) par j) 
			     (ats-aref (ats-sound-frq sound) par left)))
		      (t 
		       (setf (ats-aref (ats-sound-frq sound) par j)
			     (envelope-interp j (list left (ats-aref (ats-sound-frq sound) par left)
					 right (ats-aref (ats-sound-frq sound) par right))
				   )))))
	      ;;; pha
	      (if (= (ats-aref (ats-sound-amp sound) par left) 0.0)
		  (loop for j from (1- right) above left do
		    (setf (ats-aref (ats-sound-pha sound) par j)
			  (+ (ats-aref (ats-sound-pha sound) par (1+ j))
			     (* (ats-aref (ats-sound-frq sound) par j) 
				mag frame-size))))
		(loop for j from (1+ left) below right do
		  (setf (ats-aref (ats-sound-pha sound) par j)
			 (- (ats-aref (ats-sound-pha sound) par (1- j))
			    (* (ats-aref (ats-sound-frq sound) par j) 
			       mag frame-size)))))
	      ;;; and finally the amps
	      (loop for j from (first k) below (+ (first k)(second k)) do
		(setf (ats-aref (ats-sound-amp sound) par j)
		      (envelope-interp j (list left (ats-aref (ats-sound-amp sound) par left)
				  right (ats-aref (ats-sound-amp sound) par right))
				   ))))))))))
			
  


;;; removes short segments from a partial
(defun remove-short-segments (sound par &optional (min-length *ats-min-segment-length*))
  (let ((segments (segments-arr (array-slice (ats-sound-amp sound) par))))
    (dolist (seg segments)
      (if (< (second seg) min-length)
	  (loop for i from (first seg) below (+ (first seg) (second seg)) do
	    (setf (ats-aref (ats-sound-amp sound) par i) (float 0.0 1.0d0)))))))

;;; removes short segments from all partials
;;; (this could take account of SMR, coming soon!)
(defun trim-partials (sound &optional (min-length *ats-min-segment-length*))
  (loop for i from 0 below (ats-sound-partials sound) do
    (remove-short-segments sound i min-length)))

;;; interpolates to fill zeroes
(defun fill-amp-gaps (sound)
  "
fills gaps in the amplitudes of
<sound> using interpolation
"
  (do ((i 0 (1+ i)))
      ((= i (ats-sound-partials sound)) 'done)
    (fill-gaps (aref (ats-sound-amp sound) i))))


(defun get-valid-bands (sound threshold)
  (let ((band-l nil)
	(bands (if (ats-sound-bands sound)
                   (length (ats-sound-bands sound))
		   *ats-critical-bands*))
	(frames (ats-sound-frames sound)))
    (loop for i from 0 below bands do
      (if (>= (prom_array (array-slice (ats-sound-band-energy sound) i) frames)
	      threshold)
	  (push i band-l)))
    (nreverse band-l)))

;;; removes bands that have average energy below threshold
(defun remove-bands (sound &optional (threshold *ats-noise-threshold*))
  (let* ((frames (ats-sound-frames sound))
	 (threshold (db-amp threshold))
	 (band-l (get-valid-bands sound threshold))
	 (new-bands (make-ats-array (list-length band-l) frames)))
    ;;; now we only keep the bands we want 
    (loop 
      for i in band-l
      for k from 0
      do (loop for val across (array-slice (ats-sound-band-energy sound) i)
               for idx from 0
               do (setf (ats-aref new-bands k idx) val)))
    ;;; finally we store things in the sound
    (setf (ats-sound-band-energy sound) new-bands)
    (setf (ats-sound-bands sound)(coerce band-l 'vector))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sound Optimization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun optimize-sound (sound &key 
			     (verbose nil)
			     (get-max-values T)
			     (fill-gaps T)
			     (min-length *ats-min-segment-length*)
			     (trim T)
			     (amp-threshold *ats-amp-threshold*)
			     (simplify T)
			     (min-frq 0.0)
			     (max-frq 20000.0))
  "
optimize-sound <sound>
initializes several slots of <sound>
"
  (if (ats-sound-optimized sound)
      (warn "Sound already optimized!~%"))
  (format t "Optimizing sound...~%")
  (when get-max-values
  ;;; get max amplitude of sound
    (if verbose (format t "Getting Max. Amplitude...~%"))
    (setf (ats-sound-ampmax sound) (get-ampmax sound))
    (if verbose (format t "Max. Amplitude: ~s~%" (ats-sound-ampmax sound)))
  ;;; get max frequency of sound
    (if verbose (format t "Getting Max. Frequency...~%"))
    (setf (ats-sound-frqmax sound) (get-frqmax sound))
    (if verbose (format t "Max. Frequency: ~s~%" (ats-sound-frqmax sound))))
  ;;; (norm-amp sound)
  (when fill-gaps
    (if verbose (format t "Filling out sound gaps...~%"))
    (fill-sound-gaps sound min-length))
  ;;; trim out segments of partials shorter than min-length
  (when trim
    (if verbose (format t "Trimming short segments off...~%"))
    (trim-partials sound min-length))
;;;  (set-f0 sound)
  ;;; set average frequency and amplitude for each partial
  (if verbose (format t "Getting amplitude and frequency averages...~%"))
  (set-amp-av sound)
  (set-frq-av sound)
  ;;; remove partials that fall off limits and sort reminder by frequency
  ;;; we use average metrics for amp and frq
  (when simplify
    (if verbose (format t "Simplifying sound...~%"))
    (simplify-sound sound (get-valid-partials sound min-frq max-frq amp-threshold)))
  ;;; set optimized slot to True
  (setf (ats-sound-optimized sound) 't))

(defun optimize-load-sound (sound &key 
			     (verbose nil)
			     (get-max-values T)
			     (fill-gaps T)
			     (min-length *ats-min-segment-length*)
			     (trim T)
			     (amp-threshold *ats-amp-threshold*)
			     (simplify T)
			     (min-frq 0.0)
			     (max-frq 20000.0))
  "
optimize-sound <sound>
initializes several slots of <sound>
"
  (if (ats-sound-optimized sound)
      (warn "Sound already optimized!~%"))
  (format t "Optimizing load sound...~%")
  (when get-max-values
  ;;; get max amplitude of sound
    (if verbose (format t "Getting Max. Amplitude...~%"))
    (setf (ats-sound-ampmax sound) (get-ampmax sound))
    (if verbose (format t "Max. Amplitude: ~s~%" (ats-sound-ampmax sound)))
  ;;; get max frequency of sound
    (if verbose (format t "Getting Max. Frequency...~%"))
    (setf (ats-sound-frqmax sound) (get-frqmax sound))
    (if verbose (format t "Max. Frequency: ~s~%" (ats-sound-frqmax sound))))
  ;;; (norm-amp sound)
  (when fill-gaps
    (if verbose (format t "Filling out sound gaps...~%"))
    (fill-sound-gaps sound min-length))
  ;;; trim out segments of partials shorter than min-length
  (when trim
    (if verbose (format t "Trimming short segments off...~%"))
    (trim-partials sound min-length))
;;;  (set-f0 sound)
  ;;; set average frequency and amplitude for each partial
  (if verbose (format t "Getting amplitude and frequency averages...~%"))
  (set-amp-av sound)
  (set-frq-av sound)
  ;;; remove partials that fall off limits and sort reminder by frequency
  ;;; we use average metrics for amp and frq
  (when simplify
    (if verbose (format t "Simplifying sound...~%"))
    (simplify-sound sound (get-valid-partials sound min-frq max-frq amp-threshold)))
  ;;; set optimized slot to True
  (setf (ats-sound-optimized sound) 't))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Extra functions and macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun range (num &rest args)
  "similar to clozure's range function with all the bells and whistles."
  (let* ((start (if (numberp (first args)) num 0))
         (end (if (numberp (first args)) (first args) num))
         (proplist (if (oddp (length args)) (cdr args) args))
         (step (getf proplist :step 1))
         (offset (getf proplist :offset 0)))
    (cond
      ((zerop step) (error "range: step is zero"))
      ((< step 0) (reverse
                   (loop for n from start below end by (abs step)
                         collect (+ n offset))))
      (:else (loop for n from start below end by (abs step)
                         collect (+ n offset))))))

;;; (range 10) -> (0 1 2 3 4 5 6 7 8 9)
;;; (range 10 :step 2) -> (0 2 4 6 8)
;;; (range 10 :step 2 :offset 3) -> (3 5 7 9 11)
;;; (range 10 :step -1) -> (9 8 7 6 5 4 3 2 1 0)
;;; (range 2 10) -> (2 3 4 5 6 7 8 9)
;;; (range 2 10 :step -1) -> (9 8 7 6 5 4 3 2)



;;; the closest power of 2
(defun ppp2 (num &optional (dep 0))
  "ppp2 <num>
the power of 2 closest to  <num>"
  (let ((tmp (expt 2 dep)))
    (if (>= tmp num) tmp
      (ppp2 num (1+ dep)))))

;;; documentation macro
(defmacro doc (function)
  "
doc <function> 
gives the documentation of <funcion>
"
  `(documentation (quote ,function) 'function))


;;; returns an array with interpolated time values
;;; env-points is the desired envelope and
;;; a-temps are the desired time points to interpolate
(defun int-points (env-points a-temps &optional a-r)
  (let*((len (length a-temps))
	(f (elt a-temps 0))
	(d (elt a-temps (- len 1)))
	(tr (list f 0.0 d 1.0))
	(aa-r (if a-r a-r (make-double-float-array len :initial-element 0.0))))
	(do ((i 0 (1+ i)))
	    ((= i len) aa-r)
	  (setf (aref aa-r i) 
		(dfloat
		 (envelope-interp (envelope-interp (aref a-temps i) tr) env-points))))))

;;; just to avoid problems deleting elements from a list
(defmacro efface (terme liste)
        `(setq ,liste (delete ,terme ,liste :test #'string=)))

;;; deletes a sound from the ATS environment
(defmacro bye-sound (sound &optional (rg t))
"
bye-sound <sound> &optional [rg]
deletes <sound> from ATS, updating global *ats-sounds*
if [rg] is T forces a garbage collection
of memory
"
`(progn (efface (ats-sound-name ,sound) *sounds*)
        (makunbound (quote ,sound))
        (if ,rg
            #+sbcl (sb-kernel::gc)
            #-sbcl excl:gc
            )
        *ats-sounds*))

;;; functions for amplitude conversion
(defun amp-db (amp)
  (* 20 (log amp 10)))

(defun db-amp (db)
  (expt 10.0 (/ db  20.0)))


(defun amp-db-spl (amp)
  (+ *ats-max-db-spl* (amp-db amp)))

(defun db-amp-spl (db-spl)
  (db-amp (- db-spl *ats-max-db-spl*)))

;;; computes RMS value of noise
(defmacro compute-noi-gain (energy N &optional (variance 0.04))
  `(sqrt (/ ,energy (* ,N ,variance ))))

;;; extra envelope handlers
(defun x-env (env)
  "
returns the x values of env
"
  (loop for x in env by #'cddr collect x))


(defun y-env (env)
  "
returns the y values of env
"
  (loop for y in (cdr env) by #'cddr collect y))

(defun un-db-env (env) 
  "
transforms the values of an envelope from 
dB to linear scale
"
  (if (endp env) nil
      (append (list (car env) (db-amp (cadr env)))
	      (un-db-env (cddr env)))))



(defun normalize-amp-by-value (sound value)
  "
normalizes the amplitudes of <sound> 
to get an average amplitude of <value>
for all partials
"
  (let* ((par (ats-sound-partials sound))
	 (frames (ats-sound-frames sound))
	 (scaler-values (make-double-float-array par :initial-element 0.0)))
    ;;; scan scaler values first
    (loop for i from 0 below par do
      (setf (aref scaler-values i)
	    (/ value (aref (ats-sound-amp-av sound) i)))
      (print (aref scaler-values i)))
    ;;; do normalization
    (loop for i from 0 below par do
      (un_norm_array (aref (ats-sound-amp sound) i)
		     frames
		     (aref scaler-values i)))
    ;;; re-compute amplitude average of each partial
    (set-amp-av sound)))
    


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Envelope Handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro ats-make-envelope (sound n parameter &optional (duration NIL))
  `(let* ((dur (ats-sound-dur ,sound))
	  (frames (ats-sound-frames ,sound))
	  (rat (if ,duration (/ ,duration dur) 1.0)))
     (loop 
       for i from 0 below frames
       for time = (ats-aref (ats-sound-time ,sound) ,n i)
       for val = (ats-aref (cond ((equal 'amp ,parameter)
			          (ats-sound-amp ,sound))
			         ((equal 'frq ,parameter)
			          (ats-sound-frq ,sound))
			         ((equal 'band-energy ,parameter)
			          (ats-sound-band-energy ,sound))
			         ((equal 'energy ,parameter)
			          (ats-sound-energy ,sound))
			         (T (error "ats-make-env: bad parameter ~A" ,parameter)))
		           ,n
		           i)
       collect (if ,duration (* rat time) time)
       collect val)))

(defun array->buffer (arr)
  (make-buffer (length arr) :initial-contents arr))
