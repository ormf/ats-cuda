;;; 
;;; browser-gui.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2024 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
;;;
;;; Revision history: See git repository.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Gnu Public License, version 2 or
;;; later. See https://www.gnu.org/licenses/gpl-2.0.html for the text
;;; of this agreement.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU General Public License for more details.
;;;
;;; **********************************************************************

(in-package :ats-cuda-display)

(defparameter *ats-snd-directory* "/tmp/")

(defun ats->browser (ats-snd &key (reload t))
  (let ((svg-file (format nil "~a.svg"
                          (string-downcase (ats-sound-name ats-snd)))))
    (setf ats-sound ats-snd)
    (when reload (ats->svg ats-snd))
    (set-val ats-data svg-file :force t)
    (set-val ats-x 0)
    (set-val ats-shift-x (/ (get-val ats-width) 2))
    (let ((num-partials (ats-sound-partials ats-snd)))
      (setf ats-fmod (make-array num-partials
                                 :element-type 'incudine::sample
                                 :initial-element 1.0d0))
      (setf ats-amod (make-array num-partials
                                 :element-type 'incudine::sample
                                 :initial-element 1.0d0)))
    nil))

;;; (make-ref 1.0)
;;; (ats->browser ats-cuda::cl)

;;; (node-free-all)

;;; (incudine:free 1)

(defun start-browser-play ()
;;;  (format t "starting!~%")
  (when (and ats-amod ats-fmod)
    (setf ats-player-node-id (incudine:next-node-id))
    (if (ats-sound-bands ats-sound)
        (incudine::sin-noi-rtc-synth* (or (first (get-val ats-mousepos)) 0.0) ats-sound
                                     :amp-scale 0.1
                                     :id ats-player-node-id
                                     :amod ats-amod
                                     :fmod ats-fmod))))

(defun stop-browser-play ()
;;;  (format t "stopping!~%")
  (if ats-player-node-id
      (incudine::free ats-player-node-id))
  (setf ats-player-node-id nil))

#|
(defun browser-play (ats-sound &rest args)
  (let* ((ats-snd
           (if (or (stringp ats-sound) (typep ats-sound 'pathname))
               (symbol-value (ats-load
                              ats-sound
                              (intern
                               (string-upcase
                                (pathname-name (pathname ats-sound)))
                               :ats-cuda)))
               ats-sound))
         (num-partials (ats-cuda::ats-sound-partials ats-snd))
         (maxfreq (float (+ 100 (aref (ats-cuda::ats-sound-frq-av ats-snd) (1- num-partials))) 1.0))
         (browser-player
           (make-browser-player
            :ats-sound ats-snd
            :amp-scale (getf args :amp-scale 1.0)
            :num-partials num-partials
            :partials (getf args :partials (range num-partials))
            :res-bal (getf args :res-bal 0.5)
            :maxfreq maxfreq
            :amod (make-array num-partials :element-type 'incudine::sample :initial-element 1.0d0)
            :fmod (make-array num-partials :element-type 'incudine::sample :initial-element 1.0d0)
            :bw (getf args :bw 40000)
            :soundpos (getf args :soundpos 0)
            :mousefreq (* (max 0.0 (min (getf args :y 0) 1.0)) maxfreq))))
    (ats->svg ats-snd :brightness (getf args :brightness 20))
    (broadcast-message "reload")
    (if *curr-browser-player* (free (browser-player-id *curr-browser-player*)))
    (setf *curr-browser-player* browser-player)
    (recalc-amps)
    (apply #'incudine::sin-noi-rtc-synth 0.0 ats-snd
           :amod (browser-player-amod browser-player)
           :fmod (browser-player-fmod browser-player) :id (getf args :id 2) args)
    (setf (browser-player-id *curr-browser-player*) (getf args :id 2))
    browser-player))
|#

(progn
  (defparameter ats-player-node-id nil)
  (defparameter ats-sound nil)
  (defparameter ats-fmod nil)
  (defparameter ats-amod nil)
  (defparameter ats-bw nil)
  (defparameter ats-x nil)
  (defparameter ats-shift-x nil)
  (defparameter ats-width nil)
  (defparameter ats-idx nil)
  (defparameter ats-data nil)
  (defparameter ats-crosshairs nil)
  (defparameter ats-mousepos nil)
  (defparameter ats-scale nil)
  (defparameter ats-play nil)
  (defparameter data-watch nil)
  (defparameter play-watch nil)
  (defparameter pos-watch nil)
)

(progn
  (dolist (fn (list data-watch play-watch pos-watch))
    (if fn (funcall fn)))
  (clear-bindings)
  (setf ats-x (make-ref 0))
  (setf ats-shift-x (make-ref 0))
  (setf ats-width (make-ref 4))
  (setf ats-idx (make-ref 0))
  (setf ats-data (make-ref "ats-snd.svg"))
  (setf ats-crosshairs (make-ref 1))
  (setf ats-mousepos (make-ref '(0 0)))
  (setf ats-scale (make-ref 1))
  (setf ats-play (make-ref 0))
  (setf ats-bw (make-ref 0.5))
  (setf data-watch
        (watch (lambda ()
                 (set-val ats-shift-x (/ (get-val ats-width) 2))
                 )))
  (setf play-watch
        (watch (lambda () (if (zerop (get-val ats-play))
                         (stop-browser-play)
                         (start-browser-play)))))
  (setf pos-watch
        (watch (lambda () (destructuring-bind (x y) (get-val ats-mousepos)
                       (declare (ignorable y))
                       (if ats-player-node-id
                           (set-control ats-player-node-id :soundpos x))))))
  nil)

         ;; (num-partials (ats-cuda::ats-sound-partials ats-sound))
         ;; (maxfreq (float (+ 100 (aref (ats-cuda::ats-sound-frq-av ats-snd) (1- num-partials))) 1.0))


(defun ats-display (body)
  "On-new-window handler."
  (setf (title (clog::html-document body)) "ATS Cuda display")
  (create-o-svg
   body (bind-refs-to-attrs ats-width "width" ats-x "cursor-pos" ats-shift-x "shift-x" ats-data "data"
                            ats-scale "scale" ats-crosshairs "crosshairs" ats-mousepos "mousepos"
                            ats-bw "bandwidth"))
  ;; (create-o-radio body (bind-refs-to-attrs idx "value") :css '(:width "6em") :labels (list (loop for idx from 1 to 6 collect idx)) :num 6)
  (create-o-toggle body (bind-refs-to-attrs ats-play "value") :css '(:font-size "1.6em" :width "3em" :display "block") :label '("off" "on") :background '("transparent" "#8f8"))
  ;; (create-o-slider body (bind-refs-to-attrs shift-x "value" width "max") :min 0 :direction :right
  ;;                                                                        :css `(:display "inline-block" :height "1em" :width "100%"))
;;  (create-o-knob body (bind-refs-to-attrs x "value") 0 1 0.01 :precision 2)
  )

(defun on-new-window (body)
  (new-window body))

(set-on-new-window #'ats-display :path "/ats-display" :boot-file "/start.html")

#|
(funcall cursor-watch)
(defparameter cursor-watch
  (watch (lambda () (format t "~{~,2f~^, ~}~%" (get-val mousepos)))))
|#

(defun ats-display-start ()
  (clear-bindings) ;;; start from scratch
  (initialize #'ats-display
              :port 8080
              :static-root (merge-pathnames "www/" (asdf:system-source-directory :clog-dsp-widgets))
              :boot-file "/start.html")
  ;; Open a browser to http://127.0.0.1:8080 - the default for CLOG apps
  (open-browser))

;;; (ats-display-start)

#|
(set-val scale 0.5)
(get-val width)
|#
