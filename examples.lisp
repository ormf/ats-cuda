;;; Examples of the original ATS 1.0 clm package plus additional
;;; examples for the incudine realtime part.

;;; Analysis

(in-package :ats-cuda)

(defvar cl nil)
(defvar crt-cs6 nil)

;;; first analyse some sounds for the examples to work:

;;; cl
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
         :residual nil
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

;;; saving and loading

;;; saving sound 
(ats-save cl "/tmp/cl.ats")

;;; loading sound
(ats-load "/tmp/cl.ats" 'cl-new)

;;; saving sound without phase  
(ats-save crt-cs6 "/tmp/crt-cs6.ats" :save-phase nil)

;;; loading sound
(ats-load "/tmp/crt-cs6.ats" 'crt-cs6--new)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Synthesis
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Utility macro mimicking parts of clm's with-sound for exporting of
;;; sin-noi-synth and sin-synth to a soundfile

(defmacro with-ats-sound ((output &key sample-rate channels) synthcall)
  "export a sin-noi-synth or sin-synth call to a sound output."
  `(progn
     (unless (member ',(first synthcall) '(sin-noi-synth sin-synth))
       (error "synthcall must be a sin-noi-synth or a sin-synth"))
     (bounce-to-disk (,output
                      :duration ,(or (getf (nthcdr 3 synthcall) :duration) (ats-sound-dur (symbol-value (third synthcall))))
                      :channels (or ,channels 1)
                      :sample-rate (or ,sample-rate (ats-sound-sampling-rate ,(third synthcall))))
            ,synthcall)))

;;; start incudine's realtime processing:

(rt-start)

;;; plain resynthesis (sines only)

(with-ats-sound ("/tmp/cl-2.snd")
  (sin-synth 0.0 cl))

;;; sine resynthesis with softer attack:

(sin-synth 0.0 cl :amp-env '(0 0 0.2 0.1 1 1))

;;; plain resynthesis (sines plus noise)

(with-ats-sound ("/tmp/cl-2.snd")
  (sin-noi-synth 0.0 cl))

;;; realtime playing without saving to disk:

(sin-noi-synth 0.0 cl :amp-scale 0.2)

;;; plain resynthesis (sines plus noise) with selected partials:

(sin-noi-synth 0.0 cl :par (range 6))

(sin-noi-synth 0.0 cl :par (range 6 :step 2 :offset 1))

(sin-noi-synth 0.0 cl :par '(0 1 4))

;;; plain resynthesis (noise only)
(with-ats-sound ("/tmp/cl-3.snd" :channels 1 :sample-rate 44100)
  (sin-noi-synth 0.0 cl :noise-only t))

;;; using time pointer to modify the attack
(with-ats-sound ("/tmp/cl-4.snd")
  (sin-noi-synth 0.0 cl :time-ptr '(0.0 0.0 0.5 0.1 0.7 0.7 1.0 1.0)))

;;; play backwards and gradually adding noise
(with-ats-sound ("/tmp/cl-5.snd")
  (sin-noi-synth
   0.0 cl
   :time-ptr '(0.0 1.0 0.9 0.3 1.0 0.0)
   :noise-env '(0.0 0.0 0.9 1.0 1.0 1.0)
   :amp-env '(0 0 0.1 0 0.9 1 1 1)))

;;; crt-cs6
;;; plain resynthesis (sines only)
(with-ats-sound ("/tmp/crt-cs6-1.snd")
  (sin-synth 0.0 crt-cs6))

;;; plain resynthesis (sines plus noise)
(with-ats-sound ("/tmp/crt-cs6-2.snd")
  (sin-noi-synth 0.0 crt-cs6 :time-ptr '(0 0 1 1)))

;;; plain resynthesis (noise only)
(with-ats-sound ("/tmp/crt-cs6-3.snd")
  (sin-noi-synth 0.0 crt-cs6 :time-ptr '(0 0 1 1) :noise-only t))

;;; transpose up an octave and expand four times keeping attack
;;; use only partials' noise
(with-ats-sound ("/tmp/crt-cs6-4.snd")
  (sin-noi-synth
   0.0 crt-cs6 
   :frq-scale 2
   :time-ptr (list 0.0 0.0 (/ 0.1 (ats-sound-dur crt-cs6) 4) 0.1 0.5 0.5 1.0 1.0)
   :duration (* (ats-sound-dur crt-cs6) 4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Real time control synths (2023 incudine version)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; play sound at position (* 0.2 soundfilelength)

(sin-noi-rtc-synth 0.2 cl :amp-scale 0.1 :id 1)

;;; modify position:

(set-control 1 :soundpos 0.05)
(set-control 1 :soundpos 0.85)
(set-control 1 :soundpos 0.5)

;;; modify amplitude:

(set-control 1 :amp-scale 0.2)

;;; stop synth:

(free 1)

;;; play sound at position (* 0.2 soundfilelength) with
;;; the option of partial stretching:

(sin-noi-rtc-pstretch-synth 0.2 cl :amp-scale 0.1 :id 2)

;;; change the partial stretching (semitones per octave):

;;; quartertone per octave up:

(set-control 2 :pstretch 0.5) 

;;; quartertone per octave down:

(set-control 2 :pstretch -0.5)


;;; no stretching:

(set-control 2 :pstretch 0)

;;; stop synth:

(free 2)

;;; set up arrays for realtime amplitude and frequency modulation:

(defvar *amod* (sample-array (ats-sound-partials cl)
                             :initial-element 1.0d0))

(defvar *fmod* (sample-array (ats-sound-partials cl)
                             :initial-element 1.0d0))

;;; start the synth:

(sin-noi-rtc-pstretch-synth
 0.2 cl :fmod *fmod* :amod *amod*
        :amp-scale 0.1 :id 2)

;;; change amp of 1st partial:

(setf (aref *amod* 0) (sample 0.5))
(setf (aref *amod* 0) (sample 0.2))
(setf (aref *amod* 2) (sample 0.3))

;;; change amp of all partials:

(dotimes (partial (ats-sound-partials cl))
  (setf (aref *amod* partial) (sample 0.4)))

;;; stop synth

(free 2)

;;; helper synths for mouse control of a realtime synth. Evaluate the
;;; (progn ...) first.

(in-package :incudine)

(progn
  (dsp! xy-sndpos-amp-ctl ((synth-id (or (unsigned-byte 62) node)))
    "A dsp to control the pos (mouse-x) and the amplitude (mouse-y) of a
sin-noi-rtc(-stretch)-synth."
    (set-control synth-id :soundpos (lag (mouse-x) 1))
    (set-control synth-id :amp-scale (lag (mouse-y) 1)))

;;; helper synth for the control of an array of
;;; amplitude-modulation vals for filter-like operations on the
;;; amplitudes of the partials.

  (defun n-lin (x min max)
    "normalized linear interpolation of x between min and max."
    (+ min (* (- max min) x)))

  (defun recalc-bw (bw num-partials)
    (n-lin bw 0.5 num-partials))

  (defun recalc-cfreq-pos (pos max)
    (n-lin pos 0 (1- max)))

  (defun bias-cos (cfreq-pos bw num-partials)
    "return a function which calculates the level for a partial with given
center frequency and bw. Center frequency and bw are normalized, both
in partial index units in relation to the total number of partials. bw
is the distance between the center freq pos and the -6 dB points
left/right of the cfreq index. It gets interpolated between 0.5
and (1- num-partials).

Example:

    num-partials = 11
    cfreq = 6th partial (index 7 => cfreq-pos = (float (/ 7 16))  = 0.4375
    bw = 0.25 (= 2.5 partials)

(let* ((num-partials 11)
       (fn (bias-cos 0.5 0.25 num-partials)))
  (loop for partial below num-partials collect (funcall fn partial)))

 -> (0.09549150281252633d0 0.28711035421746367d0 0.5313952597646567d0
     0.7679133974894983d0 0.9381533400219317d0 1.0d0 0.9381533400219317d0
     0.7679133974894983d0 0.5313952597646567d0 0.28711035421746367d0
     0.09549150281252633d0)

"
    (let* ((real-bw (recalc-bw bw num-partials))
           (fader-interp (- (clip real-bw (1- num-partials) num-partials) (1- num-partials))))
      (lambda (x) (+ fader-interp
                (* (- 1 fader-interp)
                   (+ 0.5 (* 0.5 (cos (clip (/ (* pi 1/2 (- x (* cfreq-pos (1- num-partials))))
                                               real-bw)
                                            (* -1 pi) pi)))))))))

  (dsp! xy-partial-ctl ((arr (array sample)) (synth-id (or (unsigned-byte 62) node)) (num-partials (unsigned-byte 62)))
    "A synth to control the center-freq (mouse-x) and the bw (mouse-y) of
the amplitude of partials in a sin-noi-rtc(-stretch)-synth."
    (with-samples ((ypos 0) (ypos-old 0)
                   xpos xpos-old)
      (setf xpos (lag (mouse-x) 1))
      (setf ypos (lag (mouse-y) 1))
      (when (or (/= ypos-old ypos) (/= xpos-old xpos))
        (let ((fn (bias-cos xpos (- 1 ypos) num-partials)))
          (dotimes (partial num-partials)
            (setf (aref arr partial) (funcall fn partial))))
        (setf xpos-old xpos)
        (setf ypos-old ypos))))       

  (export '(xy-sndpos-amp-ctl xy-partial-ctl) 'incudine))

(in-package :ats-cuda)


;;; start the synth

(sin-noi-rtc-synth 0.2 cl :fmod *fmod* :amod *amod* :amp-scale 0.05 :id 2)

;;; start the mouse-ctl synth and then use the mouse to navigate
;;; through the sound, using mouse-x for time position and mouse-y as
;;; volume control.

(xy-sndpos-amp-ctl 2 :id 3)

;;; stop the mouse-ctl synth:

(free 3)

;;; start another mouse-ctl synth to modify the amplitudes of the
;;; partials. Mouse-x is the center partial and mouse-y is the
;;; "bandwith" of the amplitude control to simulate a bandpass filter
;;; with variable bandwidth on the partials.

(xy-partial-ctl *amod* 2 (ats-cuda::ats-sound-partials ats-cuda::cl) :id 4)

;;; stop the mouse-ctl synth

(free 4)

;;; stop the ats-sound synth

(free 2)
