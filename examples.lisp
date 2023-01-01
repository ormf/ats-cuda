;;; Analysis

(in-package :ats-cuda)

;;; cl
(defvar cl nil)

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
(defvar crt-cs6 nil)
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

(time
 (bounce-to-disk ("/tmp/test2.wav" :channels 2 :duration 2.52)
   (sin-noi-synth 0.0 cl)))

(sin-noi-synth 0.0 cl :duration 6 :time-ptr `(0 0 0.1 0.6 1 1)
               :frq-scale 2)

(sin-noi-synth 0.0 cl :duration 6.0 :time-ptr `(0 0 0.1 0.2 1 1))
(sin-noi-synth 0.0 cl :time-ptr `(0 1 1 0))
(sin-noi-synth 0.3 cl)

(sin-noi-synth 0.0 cl :duration 10.0 :time-ptr `(0 0.5 1 0.5))


(sin-noi-synth 0.0 crt-cs6 :noise-only t :band-noise nil :amp-scale 20)

(sin-noi-synth 0.0 crt-cs6 :noise-only t :band-noise t :noise-env '(0 0 1 1) :amp-scale 10)

(sin-noi-synth 0.0 cl :noise-only nil :band-noise t :noise-env '(0 1 1 0) :amp-scale 0.05)

;;; Synthesis
(ats-sound-sampling-rate cl)

;;; cl
;;; plain resynthesis (sines only)
(time
 (bounce-to-disk ("/tmp/cl-21.snd"
                  :duration 2.52
                  :sample-rate 44100)
   (cl-ats::sin-synth cl)))

;;; plain resynthesis (sines plus noise)
(time
 (with-sound (:play t :output "/tmp/cl-22.snd" :sample-rate 44100
	      :statistics t :verbose t)
   (sin-noi-synth 0.0 cl :time-ptr '(0 0 1 1) :par '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26))))

(bounce-to-disk ("/tmp/cl-22.snd" :duration 2.52 :sample-rate 44100)
  (sin-noi-synth 0.0 cl))

;;; plain resynthesis (noise only)
(bounce-to-disk ("/tmp/cl-3.snd" :channels 1 :sample-rate 44100)
  (sin-noi-synth 0.0 cl :time-ptr '(0 0 1 1) :noise-only t))

;;; using time pointer to modify the attack
(bounce-to-disk ("/tmp/cl-4.snd"
                 :channels 1 :sample-rate 44100)
  (sin-noi-synth 0.0 0.0 cl :time-ptr '(0.0 0.0 0.5 0.1 0.7 0.7 1.0 1.0)))


;;; play backwards and gradually adding noise
(bounce-to-disk ("/tmp/cl-5.snd" :sample-rate 44100)
  (sin-noi-synth 0.0 0.0 cl 
		 :time-ptr '(0.0 1.0 0.9 0.3 1.0 0.0)
		 :noise-env '(0.0 0.0 0.9 1.0 1.0 1.0)
		 :amp-env '(0 0 0.1 0 0.9 1 1 1)))

;;; crt-cs6
;;; plain resynthesis (sines only)
(bounce-to-disk ("/tmp/crt-cs6-1.snd" :sample-rate 44100)
  (sin-synth 0.0 crt-cs6))

;;; plain resynthesis (sines plus noise)
(bounce-to-disk ("/tmp/crt-cs6-2.snd" :sample-rate 44100)
  (sin-noi-synth 0.0 0.0 crt-cs6 :time-ptr '(0 0 1 1)))

;;; plain resynthesis (noise only)
(bounce-to-disk ("/tmp/crt-cs6-3.snd" :sample-rate 44100)
  (sin-noi-synth 0.0 0.0 crt-cs6 :time-ptr '(0 0 1 1) :noise-only t))

;;; transpose up an octave and expand four times keeping attack
;;; use only partials' noise
(bounce-to-disk ("/tmp/crt-cs6-4.snd" :sample-rate 44100)
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
