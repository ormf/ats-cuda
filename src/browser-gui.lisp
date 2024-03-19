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
  (defparameter ats-contrast nil)
  (defparameter data-watch nil)
  (defparameter play-watch nil)
  (defparameter pos-watch nil))

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
  (setf ats-contrast (make-ref 0.1))
  (setf ats-mousepos (make-ref '(0 0)))
  (setf ats-scale (make-ref 1))
  (setf ats-play (make-ref 0))
  (setf ats-bw (make-ref 1))
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
                           (set-control ats-player-node-id :soundpos x))
                       (let* ((frames (ats-sound-frames ats-sound))
                              (soundpos x)
                              (num-partials (ats-sound-partials ats-sound))
                              (maxfreq (float (+ 100 (aref (ats-sound-frq-av ats-sound) (1- num-partials))) 1.0))
                              (mousefreq (* (max 0.0 (min y 1.0)) maxfreq)))
                         (if (<= num-partials (length ats-amod))
                             (loop for partial below num-partials
                                   for freq = (aref (ats-sound-frq ats-sound)
                                                    partial
                                                    (min (1- frames)
                                                         (max 0
                                                              (round (* soundpos
                                                                        (1- frames))))))
                                   do (setf (aref ats-amod partial)
                                            (ou:db->amp (* -18 (abs (/ (- freq mousefreq) (* 2 maxfreq (get-val ats-bw))))))))))))))
  nil)




         ;; (num-partials (ats-cuda::ats-sound-partials ats-sound))
         ;; (maxfreq (float (+ 100 (aref (ats-cuda::ats-sound-frq-av ats-snd) (1- num-partials))) 1.0))

;;; ats-sound
(defun ats-set-keyboard-mouse-shortcuts (container ats-svg ats-play ats-bw ats-contrast)
  (clog:js-execute
   container
   (format nil "document.onkeydown = function (event) {
  if (event.which == 32 || event.code == 'Space') {
    let transportToggle = document.getElementById('~a'); 
    let currValue = transportToggle.getAttribute('value');
    transportToggle.externalValueChange = false;
    if (currValue == 0) {
      transportToggle.setAttribute('value', 1);
    }
    else {
      transportToggle.setAttribute('value', 0);
    }
  }
  if (event.shiftKey) {
    let atsSvg = document.getElementById('~a'); 
    if (!atsSvg.shiftKey ) {
        atsSvg.shiftKey = true;
        console.log('shiftKey pressed');
    }
  }
};
"
           (clog:html-id ats-play)
           (clog:html-id ats-svg)))
  (clog:js-execute
   container
   (format nil "document.onkeyup = function (event) {
    let atsSvg = document.getElementById('~a'); 
    if (!event.shiftKey && atsSvg.shiftKey) {
      atsSvg.shiftKey = false;
      console.log('shiftKey released');
    }
};
"
                         (clog:html-id ats-svg)))

  (clog:js-execute
   container
   (format nil "document.onwheel = function (event) {
     let atsSvg = document.getElementById('~a');
     if (atsSvg.shiftKey) {
       let contrastSlider = document.getElementById('~a');
       let newValue = Math.min(1, Math.max (0, parseFloat(contrastSlider.getAttribute(\"value\")) + event.deltaY/3000));
//     console.log('contrast: ', newValue, 'slider: ', contrastSlider.getAttribute(\"value\"), 'dY: ', event.deltaY/1000);
       contrastSlider.setAttribute(\"value\", newValue);
       $(contrastSlider).trigger(\"data\", { value: parseFloat(newValue) });
     }
     else {
       let bwSlider = document.getElementById('~a');
       let newValue = Math.min(1, Math.max (0.01, parseFloat(bwSlider.getAttribute(\"value\")) + event.deltaY/-3000));
       bwSlider.setAttribute(\"value\", newValue);
       $(bwSlider).trigger(\"data\", { value: parseFloat(newValue) });

     }
};
"
           (clog:html-id ats-svg)
           (clog:html-id ats-contrast)
           (clog:html-id ats-bw)))

;;   (clog:js-execute
;;    container
;;    (format nil "document.onwheel = function (event) {
;;      let contrastSlider = document.getElementById('~a');
;;      let newValue = Math.min(1, Math.max (0, parseFloat(contrastSlider.getAttribute(\"value\")) + event.deltaY/10));
;;      contrastSlider.setAttribute(\"value\", newValue);
;; };
;; "
;;            (clog:html-id ats-contrast)))
  )

(defun ats-display (body)
  "On-new-window handler."
  (let (controls ats-svg ats-play-toggle ats-bw-slider ats-contrast-slider)
    (setf (title (clog::html-document body)) "ATS Cuda display")
    (setf ats-svg
          (create-o-svg
           body (bind-refs-to-attrs ats-width "width" ats-x "cursor-pos" ats-shift-x "shift-x" ats-data "data"
                                    ats-scale "scale" ats-crosshairs "crosshairs" ats-mousepos "mousepos"
                                    ats-bw "bandwidth" ats-contrast "ats-contrast")))
    ;; (create-o-radio body (bind-refs-to-attrs idx "value") :css '(:width "6em") :labels (list (loop for idx from 1 to 6 collect idx)) :num 6)
    (setf controls (create-div body :style "display: flex; height: 3em; margin-top: 0.5em"))

    (setf ats-play-toggle
          (create-o-toggle controls (bind-refs-to-attrs ats-play "value") :css '(:font-size "2em" :width "3em" :display "block") :label '("off" "on") :background '("transparent" "#8f8")))
    ;(create-o-slider body (bind-refs-to-attrs shift-x "value" width "max") :min 0 :direction :right
    ;;                                                                        :css `(:display "inline-block" :height "1em" :width "100%"))
    (setf ats-contrast-slider
          (create-o-slider controls (bind-refs-to-attrs ats-contrast "value") :width "4em" :css '(:margin-left "0.5em") :height "88%" :direction :right :min 0 :max 1))
    (setf ats-bw-slider
          (create-o-slider controls (bind-refs-to-attrs ats-bw "value") :width "4em" :css '(:margin-left "0.5em") :height "88%" :direction :right :min 0.01 :max 1))
    (ats-set-keyboard-mouse-shortcuts body ats-svg ats-play-toggle ats-bw-slider ats-contrast-slider)))

(defun on-new-ats-window (body)
  (ats-display body))

(set-on-new-window #'on-new-ats-window :path "/ats-display" :boot-file "/start.html")

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

(get-val ats-mousepos)

(defun coords (x y)
;;;  (break "ats-cuda::coords")
  (let ((id (browser-player-id *curr-browser-player*)))
    (set-control id :soundpos x)
    (set-control id :res-bal (browser-player-res-bal *curr-browser-player*)))
  (setf (browser-player-soundpos *curr-browser-player*) x)
  (setf (browser-player-mousefreq *curr-browser-player*)
        (* (max 0.0 (min y 1.0)) (browser-player-maxfreq *curr-browser-player*)))
  (recalc-amps))

(defun get-amp (freq mousefreq bw)
)

(defun recalc-amps ()
  
)

ats-amod
