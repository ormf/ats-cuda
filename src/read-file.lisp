;;; 
;;; read-file.lisp
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

(defun read-ats (file)
  "parse data of ats file into a freshly allocated ats-obj."
   (with-open-file (in file :element-type '(unsigned-byte 8))
    (read-value 'ats-obj in)))

(defun write-ats (ats-obj file)
  "write data of ats object to a binary file. "
  (with-open-file (out file :element-type '(unsigned-byte 8)
                            :direction :output
                            :if-exists :supersede)
    (write-value 'ats-obj out ats-obj)))

;;; class definitions of the parts of an ats-file

;;; top-level class

(define-tagged-binary-class ats-obj ()
  ((magic-number double-float-le)
   (sampling-rate double-float-le)
   (frame-size double-float-le)
   (window-size double-float-le)
   (partials double-float-le)
   (frames double-float-le)
   (ampmax double-float-le)
   (frqmax double-float-le)
   (dur double-float-le)
   (frame-type double-float-le))
  (:dispatch
   (ecase (round frame-type)
     (1 'ats-t1-obj)
     (2 'ats-t2-obj)
     (3 'ats-t3-obj)
     (4 'ats-t4-obj))))

(defun find-frame-class (frame-type)
     (ecase frame-type
     (1 'ats-t1-frame)
     (2 'ats-t2-frame)
     (3 'ats-t3-frame)
     (4 'ats-t4-frame)
     (otherwise "no ats frame type ~a!" frame-type)))

;;; objects contained in the ats-obj's "data" slot

(define-binary-class ats-t1-obj (ats-obj)
                     ((data (ats-frames
                             :num-frames (round frames)
                             :num-partials (round partials)
                             :frame-type 'ats-t1-frame))))

(define-binary-class ats-t2-obj (ats-obj)
                     ((data (ats-frames
                             :num-frames (round frames)
                             :num-partials (round partials)
                             :frame-type 'ats-t2-frame))))

(define-binary-class ats-t3-obj (ats-obj)
                     ((data (ats-frames
                             :num-frames (round frames)
                             :num-partials (round partials)
                             :frame-type 'ats-t3-frame))))

(define-binary-class ats-t4-obj (ats-obj)
                     ((data (ats-frames
                             :num-frames (round frames)
                             :num-partials (round partials)
                             :frame-type 'ats-t4-frame))))

#|
(defclass ats-t1-frame ()
  ((sinoids :accessor sinoids :initarg :sinoids :initform nil)))

(defclass ats-t2-frame ()
  ((sinoids :accessor sinoids :initarg :sinoids :initform nil)))
|#

(defclass ats-generic-frame ()
  ((time-tag :accessor time-tag :initarg :time-tag :initform nil)
   (sinoids :accessor sinoids :initarg :sinoids :initform nil)))

(defclass ats-sin-res-frame (ats-generic-frame)
  ((residuals :accessor residuals :initarg :residuals :initform nil)))

(define-binary-class ats-t1-sinoid ()
                     ((amp double-float-le)
                      (freq double-float-le)))

(define-binary-class ats-t2-sinoid ()
                     ((amp double-float-le)
                      (freq double-float-le)
                      (phs double-float-le)))

(defun read-ats-frame (frame-type stream &key (num-partials 0))
  (ecase frame-type
    (ats-t1-frame (make-instance 'ats-generic-frame
                                 :time-tag (read-value 'double-float-le stream)
                                 :sinoids (loop
                                            repeat num-partials
                                            collect (read-value 'ats-t1-sinoid stream))))
    (ats-t2-frame (make-instance 'ats-generic-frame
                                 :time-tag (read-value 'double-float-le stream)
                                 :sinoids (loop
                                            repeat num-partials
                                            collect (read-value 'ats-t2-sinoid stream))))
    (ats-t3-frame (make-instance 'ats-sin-res-frame
                                 :time-tag (read-value 'double-float-le stream)
                                 :sinoids (loop
                                            repeat num-partials
                                            collect (read-value 'ats-t1-sinoid stream))
                                 :residuals (loop repeat 25
                                                  collect (read-value 'double-float-le stream))))
    (ats-t4-frame (make-instance 'ats-sin-res-frame
                                 :time-tag (read-value 'double-float-le stream)
                                 :sinoids (loop
                                            repeat num-partials
                                            collect (read-value 'ats-t2-sinoid stream))
                                 :residuals (loop repeat 25
                                                  collect (read-value 'double-float-le stream))))
    (otherwise "no frame type ~a!" frame-type)))

(defun write-ats-t1-frame (stream frame)
  (write-value 'double-float-le stream (time-tag frame))
  (loop for sinoid in (sinoids frame)
        do (write-value 'ats-t1-sinoid stream sinoid)))

(defun write-ats-t2-frame (stream frame)
  (write-value 'double-float-le stream (time-tag frame))
  (loop for sinoid in (sinoids frame)
        do (write-value 'ats-t2-sinoid stream sinoid)))

(defun write-ats-t3-frame (stream frame)
    (write-value 'double-float-le stream (time-tag frame))
  (loop for sinoid in (sinoids frame)
        do (write-value 'ats-t1-sinoid stream sinoid))
  (loop for residual in (residuals frame)
        do (write-value 'double-float-le stream residual)))

(defun write-ats-t4-frame (stream frame)
    (write-value 'double-float-le stream (time-tag frame))
  (loop for sinoid in (sinoids frame)
        do (write-value 'ats-t2-sinoid stream sinoid))
  (loop for residual in (residuals frame)
        do (write-value 'double-float-le stream residual)))

(defun write-ats-frame (frame-type stream frame)
;;;  (declare (ignore frame-type stream frame))
  (ecase frame-type
    (ats-t1-frame (write-ats-t1-frame stream frame))
    (ats-t2-frame (write-ats-t2-frame stream frame))
    (ats-t3-frame (write-ats-t3-frame stream frame))
    (ats-t4-frame (write-ats-t4-frame stream frame))
    (otherwise "no frame type ~a!" frame-type)))

(define-binary-type ats-frames (frame-type num-partials num-frames)
  (:reader (in)
           (loop repeat num-frames
                 collect (read-ats-frame frame-type in :num-partials num-partials)))
  (:writer (out frames)
    (loop for frame in frames
          do (write-ats-frame frame-type out frame))))
