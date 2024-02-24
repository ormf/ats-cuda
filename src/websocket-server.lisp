;;; 
;;; display-automation.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2022 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
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

(in-package :ats-cuda)

;; make a hash table to map connections to nicknames
(defparameter *connections* (make-hash-table))

;; and assign a random nickname to a user upon connection
(defun handle-new-connection (con)
  (setf (gethash con *connections*)
        (format nil "user-~a" (random 100000))))

(defun broadcast-to-room (connection message)
  (let ((message (format nil "~a: ~a"
                         (gethash connection *connections*)
                         message)))
    (loop :for con :being :the :hash-key :of *connections* :do
          (websocket-driver:send con message))))

(defun handle-close-connection (connection)
  (let ((message (format nil " .... ~a has left."
                         (gethash connection *connections*))))
    (remhash connection *connections*)
    (loop :for con :being :the :hash-key :of *connections* :do
          (websocket-driver:send con message))))

(defparameter *ws* nil)

(defun chat-server (env)
  (let ((ws (websocket-driver:make-server env)))
    (setf *ws* ws)
    (websocket-driver:on :open ws
                         (lambda () (handle-new-connection ws)))
    (websocket-driver:on :message ws
                         (lambda (msg)
                           (broadcast-to-room ws msg)
                           (let* ((form (read-from-string msg))
                                  (fn (if (listp form)
                                          (symbol-function
                                           (intern (string-upcase (format nil "~a" (first form))) 'ats-cuda)))))
                             (when fn (apply fn (rest form))))))
    (websocket-driver:on :close ws
                         (lambda (&key code reason)
                           (declare (ignore code reason))
                           (handle-close-connection ws)))
    (lambda (responder)
      (declare (ignore responder))
      (websocket-driver:start-connection ws)))) ; send the handshake

(defparameter *msg* nil)

;; keep the handler around so that you can stop your server later on

(defparameter *chat-handler* nil)

(defun start-ats-cuda-server ()
  (setf *chat-handler* (clack:clackup #'chat-server :port 14253)))

(defun stop-ats-cuda-server ()
  (clack:stop *chat-handler*))

;;; (hunchentoot::shutdown)

;;; (clack:stop *chat-handler*) 

(defstruct browser-player
  (ats-sound nil)
  (amp-scale 1.0)
  (amod nil)
  (fmod nil)
  (bw nil)
  (soundpos nil)
  (last-frame -1)
  (last-faders '())
  (maxfreq nil)
  (num-partials 0)
  (partials nil)
  (mousefreq nil)
  (res-bal 0.5)
  (id 2))

(defparameter *html* nil)
(defparameter *curr-browser-player* nil)

(defun broadcast-message (msg)
  (loop :for con :being :the :hash-key :of *connections* :do
    (websocket-driver:send con msg)))

(defun broadcast-message (msg)
  (maphash (lambda (con val) val
             (websocket-driver:send con msg))
           *connections*))

(defun dbtoamp (db)
  (expt 10 (/ db 20)))

(defun get-amp (freq mousefreq bw)
  (dbtoamp (* -6 (abs (/ (- freq mousefreq) bw)))))

(defun coords (x y)
;;;  (break "ats-cuda::coords")
  (let ((id (browser-player-id *curr-browser-player*)))
    (set-control id :soundpos x)
    (set-control id :res-bal (browser-player-res-bal *curr-browser-player*)))
  (setf (browser-player-soundpos *curr-browser-player*) x)
  (setf (browser-player-mousefreq *curr-browser-player*)
        (* (max 0.0 (min y 1.0)) (browser-player-maxfreq *curr-browser-player*)))
  (recalc-amps))

(defun recalc-amps ()
  (let ((curr-sound (browser-player-ats-sound *curr-browser-player*))
        (soundpos (browser-player-soundpos *curr-browser-player*))
        (amod (browser-player-amod *curr-browser-player*))
        (mousefreq (browser-player-mousefreq *curr-browser-player*))
        (bw (browser-player-bw *curr-browser-player*))
        (num-partials (browser-player-num-partials *curr-browser-player*))
        (partials (browser-player-partials *curr-browser-player*)))
    (if (<= num-partials (length amod))
        (loop for partial in partials
              for freq = (aref (ats-sound-frq curr-sound)
                               partial
                               (min (1- (ats-sound-frames curr-sound))
                                    (max 0
                                         (round (* soundpos
                                                   (1- (ats-sound-frames curr-sound)))))))
              do (setf (aref amod partial)
                       (get-amp freq mousefreq bw))))))


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
         (num-partials (ats-sound-partials ats-snd))
         (maxfreq (float (+ 100 (aref (ats-sound-frq-av ats-snd) (1- num-partials))) 1.0))
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

(defun bw (player)
  (browser-player-bw player))

(defsetf bw (player) (val)
    `(progn
       (setf (browser-player-bw ,player) ,val)
       (recalc-amps)))


;;; incudine::*responders*


#|
(defparameter *client-handler* nil)

(defun client-server (env)
    (declare (ignore env))
    `(200 (:content-type "text/html")
          (,*html*)))



(defun scroll (time xoffs)
  (unless (< xoffs -1920)
    (let ((next (+ time (* 100 (/ incudine::*sample-rate* 1000)))))
      (broadcast-message (format nil "~apx,0px" xoffs))    
      (incudine:at next #'scroll next (1- xoffs)))))

|#
;;; (broadcast-message (format nil "~apx,0px" -800))



;; (defparameter *from-pd* (fudi:open :port 3010))
;; (defparameter *fudi-responder* nil)
;; 
;; (scratch::recv-start *from-pd*)

#|
(setf *fudi-responder*
  (incudine::make-fudi-responder
   *from-pd*
   (lambda (msg)
     (format *debug-io* "~a~%" msg))))

(incudine::remove-responder *fudi-responder*)
|#

#|
(setf *fudi-responder*
      (incudine::make-fudi-responder
       *from-pd*
       (lambda (msg)
         (broadcast-message (format nil "~{~apx,~apx~}" msg)))))
|#
#|

(scroll (incudine:now) 0)
(* 49.62017 (/ 496994 497777)) 49.542118

(broadcast-message "-10px,00px")

(loop for xoffs from 0 downto -300)



(progn
  (setf *html*
        "<!doctype html>

<html lang=\"en\">
<head>
  <meta charset=\"utf-8\">
  <title>LISP-CHAT</title>
</head>
<body>
    <img src=\"./test.svg\"> </img>

</body>
</html>
")
  (when *client-handler* (clack:stop *client-handler*))
  (setf *client-handler* (clack:clackup #'client-server :port 8080
                                                        :document-root (truename "/tmp/"))))

;;; (clack::find-handler :hunchentoot)

;;; (clack.handler.hunchentoot::run)

(hunchentoot:define-easy-handler (say-yo :uri "/yo") (name)
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "Hey~@[ ~A~]!" name))

;;; (ql:where-is-system "hunchentoot")

(defvar *acceptor* (make-instance 'easy-acceptor
        :port 4242
        :document-root (truename "work/kompositionen/heidelberg/grafik/")))



(progn
  (setf *html*
      "<style>
:root{
  --height: 50px;
}

.cursor{
  position: absolute;
  top:0;
  left:50vw;
  height: 100vh;
  width: 2px;
  background-color: red;
}
#svg{
  position: absolute;
#  top:calc(50vh - var(--height)/2);
  top:0;
  height: 50vh;
  left:50vw;
  transition: 1ms linear;
}
</style>

<object id=\"svg\" data=\"https://upload.wikimedia.org/wikipedia/commons/6/62/Music_notation.svg\" type=\"image/svg+xml\"></object>
<div class=\"cursor\"></div>

<script>
let svg = document.getElementById(\"svg\") ;
let socket = new WebSocket(\"ws://localhost:12345\");

socket.onopen = function(e) {
  console.log(\"[open] Connection established\");
  console.log(\"Sending to server\");
  socket.send(\"Heartbeat\");
};

socket.onmessage = function(event) {
  console.log(`[message] Data received from server: ${event.data}`);
  // data should be string: \"x,y\" e.g. \"-100px,-50px\"
  svg.style.transform = `translate(${event.data})`;
};

socket.onclose = function(event) {
  if (event.wasClean) {
    console.log(`[close] Connection closed cleanly, code=${event.code} reason=${event.reason}`);
  }
  else {
    // e.g. server process killed or network down // event.code is usually 1006 in this case
    console.log('[close] Connection died');
  }
};

socket.onerror = function(error) {
  console.log(`[error] ${error.message}`);
};
</script>
")
  (when *client-handler* (clack:stop *client-handler*))
  (setf *client-handler* (clack:clackup #'client-server :port 8080
                                                        :document-root (truename "/tmp/"))))


(progn
  (setf *html*
      "<style>
:root{
  --height: 50px;
}

.cursor{
  position: absolute;
  top:0;
  left:50vw;
  height: 100vh;
  width: 2px;
  background-color: red;
}
#svg{
  position: absolute;
#  top:calc(50vh - var(--height)/2);
  top:0;
  height: 100vh;
  left:50vw;
  transition: 1ms linear;
}
</style>

<object id=\"svg\" data=\"http://localhost/hdbg04l-sfz.svg\" type=\"image/svg+xml\"></object>
<div class=\"cursor\"></div>

<script>
let svg = document.getElementById(\"svg\") ;
let socket = new WebSocket(\"ws://localhost:12345\");

socket.onopen = function(e) {
  console.log(\"[open] Connection established\");
  console.log(\"Sending to server\");
  socket.send(\"Heartbeat\");
};

socket.onmessage = function(event) {
  console.log(`[message] Data received from server: ${event.data}`);
  // data should be string: \"x,y\" e.g. \"-100px,-50px\"
  svg.style.transform = `translate(${event.data})`;
};

socket.onclose = function(event) {
  if (event.wasClean) {
    console.log(`[close] Connection closed cleanly, code=${event.code} reason=${event.reason}`);
  }
  else {
    // e.g. server process killed or network down // event.code is usually 1006 in this case
    console.log('[close] Connection died');
  }
};

socket.onerror = function(error) {
  console.log(`[error] ${error.message}`);
};
</script>
")
  (when *client-handler* (clack:stop *client-handler*))
  (setf *client-handler* (clack:clackup #'client-server :port 8080
                                                        :document-root (truename "/tmp/"))))


|#
