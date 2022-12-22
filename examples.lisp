;;; Analysis

(in-package :cl-ats)

(defparameter *ats-snd-dir* (namestring (truename (asdf:system-relative-pathname :cl-ats "snd"))))

;;; cl

(compute-frames 110750 441 0 110750)

(defparameter *debug* nil)

(* 4 1/20 44100)

(let ((lowest-frequency 100)
      (window-cycles 4)
      (file-sampling-rate 44100))
  (floor
   (* (/ 1 lowest-frequency)
      window-cycles file-sampling-rate)))

(double (/ 44100 4096))

(setf *debug* t)

(defun compute-frames (total-samps hop st nd)
  "computes the number of frames in the analysis.
We want to have an extra frame at the end to prevent
chopping the ending."
  (labels ((get-next-frm (coarse-frames)
		       (if (> (+ (- (* coarse-frames hop) hop) st) nd) 
			   coarse-frames
			 (get-next-frm (incf coarse-frames)))))
    (get-next-frm (floor total-samps hop))))

(compute-frames 110750 441 0 110750)

(floor (1+ (/ (- 110750 0) 441)))

(defun compute-frames (total-samps hop st nd)
  "computes the number of frames in the analysis.
We want to have an extra frame at the end to prevent
chopping the ending."
  (labels ((get-next-frm (coarse-frames)
	     (if (> (+ (* (1- coarse-frames) hop) st) nd) 
		 coarse-frames
		 (get-next-frm (incf coarse-frames)))))
    (get-next-frm (floor total-samps hop))))

(defun compute-frames (total-samps hop st nd)
  "computes the number of frames in the analysis.
We want to have an extra frame at the end to prevent
chopping the ending."
  (declare (ignore total-samps))
  (+ 2 (floor (- nd st) hop)))

(normalize-window)

252
59/441

(untrace)

(tracker "tokyo.wav"
	 'cl
	 :start 0.0
	 :hop-size 1/4
	 :lowest-frequency 100.0
	 :highest-frequency 20000.0
	 :frequency-deviation 0.05
	 :lowest-magnitude (db-amp -70)
	 :SMR-continuity 0.7
	 :track-length 6
	 :min-segment-length 3
	 :residual "/tmp/cl-res.snd"
	 :verbose nil
	 :debug nil)

(tracker "clarinet.aif"
	 'cl
	 :start 0.0
	 :hop-size 1/4
	 :lowest-frequency 100.0
	 :highest-frequency 20000.0
	 :frequency-deviation 0.05
	 :lowest-magnitude (db-amp -70)
	 :SMR-continuity 0.7
	 :track-length 6
	 :min-segment-length 3
	 :residual "/tmp/cl-res.snd"
	 :verbose nil
	 :debug nil)

(tracker "clarinet.aif"
	 'cl
	 :start 0.0
	 :hop-size 1/4
	 :lowest-frequency 100.0
	 :highest-frequency 20000.0
	 :frequency-deviation 0.05
	 :lowest-magnitude (db-amp -70)
	 :SMR-continuity 0.7
	 :track-length 6
	 :min-segment-length 3
	 :residual "/tmp/cl-res.snd"
	 :verbose nil
	 :debug nil)

;;; crt-cs6
(tracker "crt-cs6.snd" 
	 'crt-cs6
	 :start 0.1
	 :lowest-frequency 500.0
	 :highest-frequency 20000.0
	 :frequency-deviation 0.15
	 :window-cycles 4
	 :window-type 'blackman-harris-4-1
	 :hop-size 1/8
	 :lowest-magnitude (db-amp -90)
	 :amp-threshold -80
	 :track-length 6
	 :min-segment-length 3
	 :last-peak-contribution 0.5
	 :SMR-continuity 0.3
	 :residual "/tmp/crt-cs6-res.snd"
	 :verbose nil
	 :debug nil
	 :optimize t)

(defparameter *fil* (open-input* (concatenate 'string *ats-snd-dir* "crt-cs6.snd")))

(ina 10 *fil*)

(defparameter *test-fft* nil)

(make-ats-sound :name (string 'crt-cs6))
(let ((N 4092))
  (setf *test-fft*
        (make-ats-fft :size N :rate 44100
                      :fdr (make-double-float-array N :initial-element 0.0)
		      :fdi (make-double-float-array N :initial-element 0.0))))

(sound-srate *fil*)
(sound-framples *fil*)
(sound-samples *fil*)

(get-samples)

(definstrument get-samples (file arr &key (offs 0) (chan 0))
  (let ((num (length arr)))
    (file->array file chan offs num arr)
    arr))

(defun get-input-data (file &key (offs 0) (chan 0))
  "read a soundfile into an array and return the array."
  (let* ((fil (open-input* file))
         (num (- (sound-framples fil) offs))
         (arr (make-double-float-array num :initial-element (double 0.0))))
    (get-samples file arr :offs offs :chan chan)))

(defparameter arr1 (get-input-data "/tmp/trombone.wav"))


(defparameter arr2 (make-array (* (length arr1) 2) :element-type 'double-float :initial-contents
                               (loop for x in (coerce arr1 'list)
                                     append (list x x))))

(array->file "/tmp/test.snd" arr2 (length arr2) 44100 2)

(definstrument set-samples (fil arrs)
  (let ((num (length arr)))
    (file->array fil 0 0 num arr)
    arr))



(array->file fname array len srate chans)


(defparameter *window*
  (window-norm (make-blackman-window 'blackman-harris-4-1 (floor (* (/ 20) 4 (sound-srate *fil*))))))

(let* ((window-cycles 4)
       (file-sampling-rate (sound-srate *fil*))
       (lowest-frequency 20)
       (n 4096)
       (fil *fil*)
       (first-point 0)
       (filptr 0)
       (fft-struct
         (make-ats-fft :size N :rate 44100
                       :fdr (make-double-float-array N :initial-element 0.0)
		       :fdi (make-double-float-array N :initial-element 0.0)))
       (cycle-samps (floor (* (/ lowest-frequency) window-cycles file-sampling-rate)))
       (M (if (evenp cycle-samps)
	      (1+ cycle-samps)
	      cycle-samps))
       (window (make-blackman-window 'blackman-harris-4-1 M)))
  (get-fft *test*)
  fft-struct)

(defparameter *test2* (make-double-array 100 :initial-element (double 0.0)))



(get-samples *test2*)




(definstrument get-samples (array)
  (let* ((fil (open-input (concatenate 'string *ats-snd-dir* "clarinet.aif")))
         (num (length array)))
    (run
     (loop
       for k below num
       do (setf (aref array k) 
	        (float (ina (+ k 48000) fil) 1.0)))))
  array)

(get-samples *test*)


(definstrument get-samples (array)
  (let* ((fil (open-input (concatenate 'string *ats-snd-dir* "crt-cs6.snd")))
         (num 1000))
    (run
     (loop for k below num
           collect (ina (+ k 48000) fil)))))


(get-fft (make-array 1000))

(definstrument get-fft (fft-struct first-point window M n fil filptr)
  (run
   (dotimes (k M)
    (if (>= filptr 0) 
        (setf (aref (ats-fft-fdr fft-struct) (mod (+ k first-point) N)) 
	      (* 1 (ina filptr fil))))
     (incf filptr)))
  (print (format nil "done: ~a" (ats-fft-fdr fft-struct))))

;;; Synthesis


;;; cl
;;; plain resynthesis (sines only)
(with-sound (:play nil :output "/tmp/cl-1.snd" :srate 44100
		   :statistics t :verbose t)
  (sin-synth 0.0 cl))

;;; plain resynthesis (sines plus noise)
(with-sound (:play nil :output "/tmp/cl-2.snd" :srate 44100
		   :statistics t :verbose t)
  (sin-noi-synth 0.0 cl :time-ptr '(0 0 1 1)))

;;; plain resynthesis (noise only)
(with-sound (:play nil :output "/tmp/cl-3.snd" :srate 44100
		   :statistics t :verbose t)
  (sin-noi-synth 0.0 cl :time-ptr '(0 0 1 1) :noise-only t))

;;; using time pointer to modify the attack
(with-sound (:play nil :output "/tmp/cl-4.snd" :srate 44100
		   :statistics t :verbose t)
  (sin-noi-synth 0.0 cl :time-ptr '(0.0 0.0 0.5 0.1 0.7 0.7 1.0 1.0)))


;;; play backwards and gradually adding noise
(with-sound (:play nil :output "/tmp/cl-5.snd" :srate 44100
		   :statistics t :verbose t)
  (sin-noi-synth 0.0 cl 
		 :time-ptr '(0.0 1.0 0.9 0.3 1.0 0.0)
		 :noise-env '(0.0 0.0 0.9 1.0 1.0 1.0)
		 :amp-env '(0 0 0.1 0 0.9 1 1 1)))


;;; crt-cs6
;;; plain resynthesis (sines only)
(with-sound (:play nil :output "/tmp/crt-cs6-1.snd" :srate 44100
		   :statistics t :verbose t)
  (sin-synth 0.0 crt-cs6))

;;; plain resynthesis (sines plus noise)
(with-sound (:play nil :output "/tmp/crt-cs6-2.snd" :srate 44100
		   :statistics t :verbose t)
  (sin-noi-synth 0.0 crt-cs6 :time-ptr '(0 0 1 1)))

;;; plain resynthesis (noise only)
(with-sound (:play nil :output "/tmp/crt-cs6-3.snd" :srate 44100
		   :statistics t :verbose t)
  (sin-noi-synth 0.0 crt-cs6 :time-ptr '(0 0 1 1) :noise-only t))

;;; transpose up an octave and expand four times keeping attack
;;; use only partials' noise
(with-sound (:play nil :output "/tmp/crt-cs6-4.snd" :srate 44100
		   :statistics t :verbose t)
  (sin-noi-synth 0.0 crt-cs6 
		 :frq-scale 2
		 :time-ptr '(0.0 0.0 0.025 0.1 0.5 0.5 1.0 1.0)
		 :duration (* (ats-sound-dur crt-cs6) 4)))

;;; saving and loading

;;; saving sound 
(ats-save cl "/tmp/cl.ats")

;;; loading sound
(ats-load "/tmp/cl.ats" 'cl-new)

;;; saving sound without phase  
(ats-save crt-cs6 "/tmp/crt-cs6.ats" :save-phase nil)

;;; loading sound
(ats-load "/tmp/crt-cs6.ats" 'crt-cs6--new)
