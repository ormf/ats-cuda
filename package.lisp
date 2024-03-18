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
   #:ats-sound-name
   #:ats-sound-partials
   #:ats-sound-bands

   ))

(defpackage #:ats-cuda-display
  (:use :cl :incudine :cudere-clm :sb-loop :ieee-floats :alexandria
;;;        :ats-cuda
   :clog-dsp-widgets :cl-refs :clog :clog-dsp-widgets
;;;        :de.finnendahl.binary-data :de.finnendahl.binary-data.common-datatypes
        )
  (:shadowing-import-from :clog
   :run :rotate)
  (:shadowing-import-from :ats-cuda
                #:ats->svg
                #:ats-sound-name
                #:ats-sound-partials
                #:ats-sound-bands
 )
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
   #:ats->browser #:ats-display #:*ats-snd-directory*))


