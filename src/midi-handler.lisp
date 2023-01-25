;;; 
;;; midi-handler.lisp
;;;
;;; framework to set midi cc responders
;;;
;;; **********************************************************************
;;; Copyright (c) 2023 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
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

(defparameter *global-midi-channel* 5)

(defparameter *midi-in1*
  (or (first (jackmidi:all-streams :input))
      (setf *midi-in1* (jackmidi:open
                        :direction :input
                        :port-name "midi_in-1"))))

(defun reconnect-midi ()
  (setf *midi-in1*
        (or (first (jackmidi:all-streams :input))
            (setf *midi-in1* (jackmidi:open
                              :direction :input
                              :port-name "midi_in-1"))))
  (incudine::remove-responder *ats-midi-responder*)
  (make-ats-responder *midi-in1*))

(defparameter *ml-opcodes*
  '((11 . :cc) (9 . :note-on) (8 . :note-off) (12 . :pgm-change)
    (14 . :pitch-bend) (10 . :key-pressure) (13 . :channel-pressure)))


(defun status->opcode (st)
  (cdr (assoc (ash (logand st #xf0) -4)
              *ml-opcodes*)))

(defun status->channel (st)
  (logand st #x0f))

(defparameter *midi-cc-state*
  (make-array 16 :element-type '(simple-array single-float)
                 :initial-contents
                 (loop repeat 16
                       collect (make-array
                                128
                                :element-type 'single-float))))

(defparameter *midi-cc-responders*
  (make-array 16 :element-type '(simple-array list)
                 :initial-contents (loop repeat 16
                                         collect
                                         (make-array 128 :element-type 'list
                                                         :initial-element nil))))

(defun make-ats-responder (&optional (midi-in *midi-in1*))
                             (incudine:make-responder
                              midi-in
                              (lambda (st d1 d2)
                                (case (status->opcode st)
                                  (:cc (let ((channel (status->channel st))
                                             (val (float (/ d2 127) 1.0)))
                                         (incudine::msg info "ats-midi-responder: ~d ~d ~,2f" channel d1 val)
                                         (setf (aref (aref ats-cuda::*midi-cc-state* channel) d1) val)
                                         (map nil (lambda (fn) (funcall fn val))
                                              (aref (aref ats-cuda::*midi-cc-responders* channel) d1))))))))

(defparameter *ats-midi-responder* (make-ats-responder))

;;; (incudine::remove-responder *ats-midi-responder*)

;;; (incudine::remove-all-responders *midi-in1*)

(defun add-ats-cc-responder (ccnum fn &key (channel *global-midi-channel*))
  "push fn to the the responders of <ccnum> at <channel>."
  (push fn (aref (aref ats-cuda::*midi-cc-responders* channel) ccnum)))

(defun remove-ats-cc-responders (ccnum &key (channel *global-midi-channel*))
  "clear all responders of <ccnum> at <channel>."
  (setf (aref (aref ats-cuda::*midi-cc-responders* channel) ccnum) nil))

(defun remove-ats-channel-cc-responders (channel)
  "clear all cc responders."
  (dotimes (ccnum 128)
    (setf (aref (aref ats-cuda::*midi-cc-responders* channel) ccnum) nil)))

(defun remove-all-ats-cc-responders ()
  "clear all cc responders."
  (dotimes (channel 16)
    (remove-ats-channel-cc-responders channel)))

#|
(add-ats-cc-responder 0 (lambda (val) (format t "~&midi-in channel: ~d cc: ~d ~a" 5 0 val)))
(add-ats-cc-responder 1 (lambda (val) (format t "~&midi-in channel: ~d cc: ~d ~a" 5 1 val)))

(remove-all-ats-cc-responders)

*midi-cc-responders*
incudine::*responders*
(funcall (first (aref (aref *midi-cc-responders* 5) 0)) 0.1)

|#

(incudine:recv-start *midi-in1*)
;;; (incudine:recv-stop *midi-in1*)

(defun ccin (ccnum &optional (channel *global-midi-channel*))
  (aref (aref *midi-cc-state* channel) ccnum))

(defsetf ccin (ccnum &optional (channel *global-midi-channel*)) (value)
  `(progn
     (setf (aref (aref *midi-cc-state* ,channel) ,ccnum) ,value)
     (map nil (lambda (fn) (funcall fn ,value))
          (aref (aref *midi-cc-responders* ,channel) ,ccnum))
     ,value))

;;; (setf (ccin 0) 64)

