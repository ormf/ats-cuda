;;;
;;; ATS 
;;; by Juan Pampin
;;; juan@ccrma.stanford.edu
;;;
;;; File: structure.cl
;;; ==================
;;; this files contains the structure definitions
;;; for the main ATS data types

;;; Structure: ats-sound
;;; ====================
;;; main data abstraction
;;; amp, frq, and pha contain sinusoidal
;;; modeling information as arrays of
;;; arrays of data arranged by partial
;;; par-energy and band-energy hold
;;; noise modeling information (experimental format)

(in-package :ats-cuda)

(defun dfloat (val)
  (float val 1.0d0))

;;; (make-ats-sound)

(defparameter *sample-array* (make-array 1 :element-type 'double-float :initial-element 0.0d0))

(defparameter *sample-darray* (make-array '(1 1) :element-type 'double-float))

(defstruct ats-sound
  "Structure of an ATS sound. The access functions use the standard
scheme /ats-sound-<slot-name>/.

@Slots
name - String denoting the filename of the analyzed sound.
sampling-rate - Positive Number denoting the sample rate of the analyzed sound.
frame-size - Number denoting the Frame Size in samples.
window-size - Number denoting the Analysis Window Size in samples.
partials - Number of tracks in the analyzed sound.
frames - number of frames in the analyzed sound (duration/window-size).
bands - List containing the indexes of noise bands of the residual outside of the noise energy contained in the tracks.
optimized - Boolean indicating whether the sound was optimized in the analysis.
ampmax - Maximum overall amplitude.
frqmax - Maximum overall frequency.
frq-av - Array of size <partial> containing the average frequency for each track.
amp-av - Array of size <partial> containing the average amplitude for each track.
dur - Positive Number indicateing the duration of the analyzed sound in seconds.
time - 2-dimensional Array of size <partial> <frames> containing the time into the analyzed sound in seconds for each frame in each track.
frq - 2-dimensional Array of size <partial> <frames> containing the frequency for each frame in each track.
amp - 2-dimensional Array of size <partial> <frames> containing the amplitude for each frame in each track.
pha - 2-dimensional Array of size <partial> <frames> containing the phase for each frame in each track.
energy - Boolean indicating whether residual energy was analyzed and mapped to the tracks.
band-energy -  2-dimensional Array of size <partial> <frames> containing the phase for each frame in each track.
analysis-params -  Property list with the parameters given to the track-ats function.

@See-also
track-ats
load-ats
save-ats
"
  (name "new-sound")
  ;;; global sound info.
  (sampling-rate 0 :type integer)
  (frame-size 0 :type integer) 
  (window-size 0 :type integer)
  (partials 0 :type integer)
  (frames 0 :type alexandria::non-negative-integer)
  (bands nil)
  ;;; info. deduced from analysis
  (optimized nil)
  (ampmax 0.0 :type (or integer single-float double-float))
  (frqmax 0.0 :type (or integer single-float double-float))
  (frq-av #() :type array) 
  (amp-av #() :type array) 
  (dur 0.0 :type (or integer single-float double-float))
  ;;; sinusoidal data
  (time *sample-darray* :type (array double-float *))
  (frq *sample-darray* :type (array double-float *))
  (amp *sample-darray* :type (array double-float *))
  (pha *sample-darray* :type (array double-float *))
  ;;; noise data
  (energy nil) 
  (band-energy nil)
  (analysis-params nil))

;;; structure: ats-fft
;;; ==================
;;; abstraction used to handle all 
;;; fft data in a single variable		

(defstruct ats-fft
  (size 0 :type  integer)
  (rate 0.0 :type (or integer single-float double-float))
  (fdr #() :type array)
  (fdi #() :type array))

;;; structure: ats-peak
;;; ===================
;;; abstraction used to keep peak data used
;;; for peak detection and tracking

(defstruct ats-peak
  (amp 0.0 :type (or single-float double-float))
  (frq 0.0 :type (or single-float double-float))
  (pha 0.0 :type (or single-float double-float))
  (smr 0.0 :type (or single-float double-float))
  (track 0 :type integer))


;;; structure: ats-sieve
;;; ====================
;;; abstraction used for peak fitting
;;; by the sieve algorithm

(defstruct ats-sieve
  (ctrfrq nil :type array)
  (limits nil :type array)
  (tracks nil :type array))
