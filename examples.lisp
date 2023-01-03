;;; Examples of the original ATS 1.0 clm package plus additional
;;; examples for the incudine realtime part.

;;; Analysis

(in-package :ats-cuda)

(defvar cl nil)
(defvar crt-cs6 nil)

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

;;; Synthesis

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

;;; play directly

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
(with-ats-sound ("/tmp/crt-cs6-4.snd" :sample-rate 44100)
  (sin-noi-synth
   0.0 crt-cs6 
   :frq-scale 2
   :time-ptr (list 0.0 0.0 (/ 0.1 (ats-sound-dur crt-cs6) 4) 0.1 0.5 0.5 1.0 1.0)
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
