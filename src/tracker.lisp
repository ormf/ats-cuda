;;; -*- syntax: common-lisp; package: clm; base: 10; mode:lisp -*-
;;;
;;; ATS 
;;; by Juan Pampin
;;; juan@ccrma.stanford.edu
;;;
;;; File: tracker.cl
;;; ======================
;;; This file contains the implementation
;;; of ATS's <tracker> analysis algorithm

(in-package :ats-cuda)

(defparameter *ats-snd-dir*
  (asdf:system-relative-pathname :ats-cuda "snd/"))

(defun get-fft-input-buffer (fft-struct)
  (let* ((buf (incudine.analysis:analysis-input-buffer fft-struct))
         (vector-size (incudine.analysis:fft-size fft-struct))
         (r-array (make-array (ash vector-size -1)))
         (i-array (make-array (ash vector-size -1))))
    (dotimes (n (ash vector-size -1))
      (setf (aref r-array n)
            (incudine:smp-ref buf (ash n 1)))
      (setf (aref i-array n)
            (incudine:smp-ref buf (1+ (ash n 1)))))
    (list r-array i-array)))

(defun get-fft-output-buffer (fft-struct)
  (let* ((buf (incudine.analysis:analysis-output-buffer fft-struct))
         (vector-size (incudine.analysis:fft-size fft-struct))
         (r-array (make-array (ash vector-size -1)))
         (i-array (make-array (ash vector-size -1))))
    (dotimes (n (ash vector-size -1))
      (setf (aref r-array n)
            (incudine:smp-ref buf (ash n 1)))
      (setf (aref i-array n)
            (incudine:smp-ref buf (1+ (ash n 1)))))
    (list r-array i-array)))

;;; Analysis function

(defun fix-freqs (ats-sound)
  "interpolate freqs in case of a freqs of 0.0d0."
  (dotimes (partial (ats-sound-partials ats-sound))
;;;    (break "partial: ~a" partial)
    (loop
      for last = 0 then freq
      with frame = 0
      with startfrq = 0
      while (< frame (ats-sound-frames ats-sound))
      for freq = (aref (ats-sound-frq ats-sound) partial frame)
      do (if (zerop freq)
             (progn
;;;               (break "")
               (setf startfrq last)
               (multiple-value-bind (frm2 endfrq)
                   (loop
                     for frame2 from frame below (ats-sound-frames ats-sound)
                     for freq2 = (aref (ats-sound-frq ats-sound) partial frame2)
                     while (zerop freq2)
                     finally (return (values frame2 (unless (zerop freq2) freq2))))
                 (progn
;;;                   (break "~a ~a ~a ~a" frame startfrq endfrq frm2)
                   (if endfrq
                       (loop for frm from frame below (min frm2 (ats-sound-frames ats-sound))
                             do (progn
;;;                                  (break "setting: ~a to ~a" frm (+ startfrq (* (- endfrq startfrq) (/ (- frm (1- frame)) (- frm2 (1- frame))))))
                                  (setf (aref (ats-sound-frq ats-sound) partial frm)
                                        (+ startfrq (* (- endfrq startfrq) (/ (- frm (1- frame)) (- frm2 (1- frame)))))))
                             finally (progn
                                       (setf frame frm2)
                                       (setf last endfrq)))
                       (loop for frm from frame below (ats-sound-frames ats-sound)
                             do (setf (aref (ats-sound-frq ats-sound) partial frm) startfrq)
                             finally (progn
                                       (setf frame frm2)))))))
             (incf frame)))))

(defun tracker (file snd &key  
                           (start 0.0)
                           (duration nil)
                           (lowest-frequency 20)
                           (highest-frequency 20000.0)
                           (frequency-deviation 0.1)
                           (window-cycles 4)
                           (window-type 'blackman-harris-4-1)
                           (hop-size 1/4)
                           (fft-size nil)
                           (lowest-magnitude (db-amp -60))
                           (track-length 3)
                           (min-segment-length 3)
                           (last-peak-contribution 0.0)
                           (SMR-continuity 0.0)
;;;             (SMR-threshold nil) ;;; unused!
                           (amp-threshold nil)
                           (residual nil)
                           (par-energy t)
                           (optimize t)
                           (debug nil)
                           (verbose nil)
                           (force-M NIL)
                           (force-window NIL)
                           )
  (let* (;;; input file 
         (fname (namestring (merge-pathnames file *ats-snd-dir*)))
         (fil (open-input* fname))
;;; ATS sound
         (sound (set snd (make-ats-sound :name (string snd))))
;;; file sampling-rate
         (file-sampling-rate (sound-srate fil))   
;;; index of first sample to read
         (st (floor (* start file-sampling-rate)))
;;; index of last sample to read
         (nd (if duration
                 (+ st (floor (* duration file-sampling-rate)))
                 (sound-framples fil)))
;;; number of samples to read
         (total-samps (- nd st))
         (input-data (sfile->array fname :count total-samps))
;;; file duration
         (file-duration (double (/ total-samps file-sampling-rate)))
;;; number of samples in a cycle
         (cycle-samps (floor
                       (* (/ 1 lowest-frequency)
                          window-cycles file-sampling-rate)))
;;; we want an odd lengthed window centered at time 0.0
         (M (if force-M force-M
                (if (evenp cycle-samps)
                    (1+ cycle-samps)
                    cycle-samps)))
;;; fft size is next power of 2 or forced by user
         (N (if fft-size
                fft-size
                (ppp2 (* 2 M))))
;;; fft structure
         (fft-struct
           (make-ats-fft
            :size N
            :rate file-sampling-rate
            :fdr (make-double-float-array N :initial-element (double 0.0))
            :fdi (make-double-float-array N :initial-element (double 0.0))))
;;; window array
         (window (if force-window force-window
                     (if (symbolp window-type)
                         (make-blackman-window window-type M)
                         (make-fft-window window-type M))))
;;; window normalization
         (norm (window-norm window))
;;; hop in samples
         (hop (floor (* M hop-size))) 
;;; number of analysis frames
         (frames (compute-frames total-samps hop st nd))
;;; we keep sample numbers of central points of the windows
         (win-samps (make-array frames :initial-element 0))
;;; magic number for fft frequencies (frquency resolution)
         (fft-mag (double (/ file-sampling-rate N)))
;;; lowest frequency to analyze
         (l-Frq (if (>= lowest-frequency 0.0)
                    lowest-frequency 0.0))
;;; highest frequency to analyze
         (h-Frq (if (<= highest-frequency (/ file-sampling-rate 2)) 
                    highest-frequency 
                    (floor file-sampling-rate 2)))
;;; lowest bin to read 
         (lowest-bin (floor l-Frq fft-mag))
;;; highest bin to read 
         (highest-bin (floor h-Frq fft-mag))
;;; Arrays for data
;;; array of lists for peaks
         (ana-frames (make-array frames :element-type 'list :initial-element nil))
;;; various vars
;;; timer
         (tmp 0.0)
         (smp 0)
;;; central point of the window
         (M-over-2 (floor (- M 1) 2))
;;; first point in fft buffer where to write
         (first-point (- N M-over-2))
;;; set file pointer half a window from the first sample
         (filptr (- st M-over-2))
;;; minimum SMR (unused?)
;;;  (min-smr (if SMR-threshold SMR-threshold 0.0))
         (n-partials 0)
         (tracks nil)
         (peaks nil)
         (unmatched-peaks nil))
;;;    (break "total-samps: ~a" total-samps)
;;; tell user we start tracking partials
    (format t "~&total-samps: ~a~%" total-samps)
    (format t "~&cycle-samps: ~a~%" cycle-samps)
    (format t "~&frames= ~D " frames)
    (format t "M = ~D N = ~D~%" M N)
    (format t "~&Tracking...~%")
;;; Main loop
    (loop
      for frame-n from 0 below frames
      with modulo = (floor frames 40)
      do
         (when (zerop (mod frame-n modulo))
           (format t "."))
;;; clear fft arrays
         (clear-array (ats-fft-fdr fft-struct))
         (clear-array (ats-fft-fdi fft-struct))
;;; multiply by window
         (loop for k from 0 below M do
           (if (>= filptr 0) 
               (setf (aref (ats-fft-fdr fft-struct) (mod (+ k first-point) N)) 
                     (* (aref window k)
                        (if (< filptr total-samps)
                            (aref input-data filptr)
                            (double 0.0)))))
           (incf filptr))
;;;      (format t "~a..." frame-n)
;;; note that after the loop filptr=M and not M-1
;;; the sample at the middle of the window is:
         (setf smp (- filptr M-over-2 1))
         (if debug (format t "smp=~d, frame-n=~d " smp frame-n))
;;; we keep sample numbers of window midpoints in an array
         (setf (aref win-samps frame-n) smp)
;;; set timer
         (setf tmp (double (/ (- smp st) file-sampling-rate)))
;;; get the dft 
         (fft
          (ats-fft-fdr fft-struct)
          (ats-fft-fdi fft-struct)
          (ats-fft-size fft-struct)
          1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Peak Detection:
;;; get peaks (amplitudes normalized by window norm)
;;; list of peaks is sorted by frequency
         (setf peaks (peak-detection fft-struct 
                                     :lowest-bin lowest-bin 
                                     :highest-bin highest-bin 
                                     :lowest-magnitude lowest-magnitude 
                                     :norm norm))
;;; process peaks
         (when peaks 
;;; evaluate masking values (SMR) of peaks
           (evaluate-smr peaks)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Peak Tracking:
;;; try to match peaks
;;; only if we have at least 2 frames
;;; and if we have active tracks 
           (if (and (> frame-n 0) 
                    (setf tracks (update-tracks tracks track-length frame-n ana-frames last-peak-contribution)))
               (let ((cpy-peak nil))
;;; track peaks and get leftover
                 (setf unmatched-peaks (peak-tracking (sort (copy-seq tracks) #'> :key #'ats-peak-smr) 
                                                      peaks frequency-deviation SMR-continuity))
;;; kill unmatched peaks from previous frame
                 (dolist (k (first unmatched-peaks))
;;; we copy the peak into this frame but with amp 0.0 
;;; this represents our death trajectory
                   (setf cpy-peak (copy-ats-peak k)
                         (ats-peak-amp cpy-peak) 0.0
                         (ats-peak-smr cpy-peak) 0.0)
                   (push cpy-peak peaks))
;;; give birth to peaks from new frame
                 (dolist (k (second unmatched-peaks))
;;; set track number of unmatched peaks
                   (setf (ats-peak-track k) n-partials)
                   (incf n-partials)
;;; we copy the peak into the previous frame but with amp 0.0 
;;; this represents our born trajectory
                   (setf cpy-peak (copy-ats-peak k)
                         (ats-peak-amp cpy-peak) 0.0
                         (ats-peak-smr cpy-peak) 0.0)
                   (push cpy-peak (aref ana-frames (1- frame-n)))
                   (push (copy-ats-peak k) tracks)))
;;; give number to all peaks
               (dolist (k (sort (copy-seq peaks) #'< :key #'ats-peak-frq))
                 (setf (ats-peak-track k) n-partials)
                 (incf n-partials)))
           (setf (aref ana-frames frame-n) peaks))
;;; update file pointer
         (setf filptr (+ (- filptr M) hop))
         (if verbose (format t "<Frame:~d Time:~4,3F Tracks:~4,3F> " frame-n tmp n-partials)))
    (format t "~%")
;;; Initialize ATS sound
    (init-sound sound 
                :sampling-rate file-sampling-rate
                :frame-size hop
                :window-size M
                :frames frames 
                :duration file-duration 
                :partials n-partials)
;;; and fill it up with data
    (loop for k from 0 below n-partials do
      (loop for frame from 0 below frames do
        (let ((pe (find k (aref ana-frames frame) :key #'ats-peak-track)))
          (if pe
              (setf (aref (ats-sound-amp sound) k frame)(double (ats-peak-amp pe))
                    (aref (ats-sound-frq sound) k frame)(double (ats-peak-frq pe))
                    (aref (ats-sound-pha sound) k frame)(double (ats-peak-pha pe))))
;;; set time anyways
          (setf (aref (ats-sound-time sound) k frame)
                (double (/ (- (aref win-samps frame) st) file-sampling-rate))))))
;;; finally optimize and declare new sound in ATS
    (if optimize 
        (optimize-sound sound
                        :min-frq lowest-frequency 
                        :max-frq highest-frequency
                        :min-length (if min-segment-length
                                        min-segment-length
                                        *ats-min-segment-length*)
                        :amp-threshold (if amp-threshold
                                           amp-threshold
                                           *ats-amp-threshold*)
                        :verbose verbose))
    (if verbose (format t "Partials: ~d Frames: ~d~%" (ats-sound-partials sound)(ats-sound-frames sound)))
;;; fix freqs of 0.0d:
    (fix-freqs sound)
;;; register sound in the system
    (add-sound sound)
    (format t "~&Partials: ~d~%" (ats-sound-partials sound))
;;; now get the residual
    (when residual
      (compute-residual
       input-data residual sound win-samps file-sampling-rate :verbose verbose :srate file-sampling-rate)
      (residual-analysis residual sound :par-energy par-energy :verbose verbose :debug debug :equalize t))
    (close-input fil)
;;;    (ats-vectors->arrays sound)
    (format t "~&Done!")))

(defun vec->array (vec)
  "transform a simple vector into a 2-dimensional simple-array of double-floats"
  (let ((m (length vec))
        (n (length (aref vec 0))))
    (make-array (list m n) :element-type 'double-float
                           :initial-contents vec)))

(defun ats-vectors->arrays (sound)
  (dolist (slot '(time frq amp pha band-energy energy))
    (setf (slot-value sound slot)
          (vec->array (slot-value sound slot))))
  )
#|

;;; cl
(tracker "~/Snd/Dsp/clarinet.aif" 'cl
  :start 0.0
  :hop-size 1/4
  :lowest-frequency 100.0
  :highest-frequency 20000.0
  :frequency-deviation 0.05
  :lowest-magnitude (db-amp -70)
  :SMR-continuity 0.7
  :track-length 6
  :min-segment-length 3
  :residual "/zap/cl-res.snd"
  :verbose t
  :debug nil)

;;; crt-cs6
(tracker "~/Snd/Tmp/crt-cs6.snd" 'crt-cs6
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
  :residual "/zap/crt-cs6-res.snd"
  :verbose nil
  :debug nil
  :optimize t)

|#
