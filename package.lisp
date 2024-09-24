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
   #:tracker
   #:*ats-snd-dir*
   #:browser-play #:ats-load
;;;   #:ats->svg
   #:*ats-blackman-window-coeffs*
   #:blackman #:blackman-harris-3-1
   #:blackman-harris-3-2
   #:blackman-harris-4-1
   #:blackman-harris-4-2
   #:ats-sound-frames #:ats-sound-name #:ats-sound-sampling-rate #:ats-sound-frame-size
   #:ats-sound-window-size #:ats-sound-partials #:ats-sound-frames #:ats-sound-bands
   #:ats-sound-optimized #:ats-sound-ampmax #:ats-sound-frqmax #:ats-sound-frq-av
   #:ats-sound-amp-av #:ats-sound-dur #:ats-sound-time #:ats-sound-frq #:ats-sound-amp
   #:ats-sound-pha #:ats-sound-energy #:ats-sound-band-energy
   #:ats-save #:ats-load #:*ats-file-dir*))
