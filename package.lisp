;;;; package.lisp

(defpackage #:ats-cuda
  (:use :cl :incudine :cudere-clm :sb-loop :ieee-floats :alexandria
;;;        :de.finnendahl.binary-data :de.finnendahl.binary-data.common-datatypes
   )
  (:shadowing-import-from :incudine
   :play :scale-envelope :normalize-envelope)
  (:shadowing-import-from :incudine.util
   :sample)
  (:shadowing-import-from :cudere-clm
   :*debug*)
  (:export
   #:tracker #:browser-play #:ats-load
   #:ats->svg #:*ats-blackman-window-coeffs*
   #:blackman #:blackman-harris-3-1
   #:blackman-harris-3-2
   #:blackman-harris-4-1
   #:blackman-harris-4-2
   #:ats-sound-frames #:ats-sound-name #:ats-sound-sampling-rate #:ats-sound-frame-size
   #:ats-sound-window-size #:ats-sound-partials #:ats-sound-frames #:ats-sound-bands
   #:ats-sound-optimized #:ats-sound-ampmax #:ats-sound-frqmax #:ats-sound-frq-av
   #:ats-sound-amp-av #:ats-sound-dur #:ats-sound-time #:ats-sound-frq #:ats-sound-amp
   #:ats-sound-pha #:ats-sound-energy #:ats-sound-band-energy))

(defpackage #:ats-cuda-display
  (:use :cl :incudine :cudere-clm :sb-loop :ieee-floats :alexandria
;;;        :ats-cuda
   :clog-dsp-widgets :cl-refs :clog :clog-dsp-widgets
;;;        :de.finnendahl.binary-data :de.finnendahl.binary-data.common-datatypes
        )
  (:shadowing-import-from :clog
   :run :rotate)
  (:shadowing-import-from :ats-cuda
   #:ats-sound-frames #:ats-sound-name #:ats-sound-sampling-rate #:ats-sound-frame-size
   #:ats-sound-window-size #:ats-sound-partials #:ats-sound-frames #:ats-sound-bands
   #:ats-sound-optimized #:ats-sound-ampmax #:ats-sound-frqmax #:ats-sound-frq-av
   #:ats-sound-amp-av #:ats-sound-dur #:ats-sound-time #:ats-sound-frq #:ats-sound-amp
   #:ats-sound-pha #:ats-sound-energy #:ats-sound-band-energy
   #:ats->svg)
  (:shadowing-import-from :incudine
   :play :scale-envelope :normalize-envelope)
  (:shadowing-import-from :incudine.util
   :sample)
  (:shadowing-import-from :cudere-clm
   :*debug*)
  (:export
   #:ats-player-node-id #:ats-sound #:ats-fmod #:ats-amod #:ats-bw #:ats-x
   #:ats-shift-x #:ats-width #:ats-idx #:ats-data #:ats-crosshairs
   #:ats-mousepos #:ats-scale #:ats-play #:data-watch #:play-watch #:pos-watch
   #:ats->browser #:ats-display #:*ats-snd-directory*


   ))


