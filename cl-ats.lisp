;;; 
;;; cl-ats.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2022 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
;;;
;;; Revision history: See git repository.
;;;
;;; This program is free software in the public domain; you can
;;; redistribute it and/or modify it without any restrictions.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;;;
;;; **********************************************************************

(in-package #:cl-ats)

(defun read-ats (file)
   (with-open-file (in file :element-type '(unsigned-byte 8))
    (read-value 'ats-obj in)))

#|

(define-binary-class ats-obj ()
  ((magic-number double-float-le)
   (sampling-rate double-float-le)
   (frame-size double-float-le)
   (window-size double-float-le)
   (partials double-float-le)
   (frames double-float-le)
   (ampmax double-float-le)
   (frqmax double-float-le)
   (dur double-float-le)
   (frame-type double-float-le)))

(define-binary-class ats-header ()
  ((magic-number double-float-le)
   (sampling-rate double-float-le)
   (frame-size double-float-le)
   (window-size double-float-le)
   (partials double-float-le)
   (frames double-float-le)
   (ampmax double-float-le)
   (frqmax double-float-le)
   (dur double-float-le)
   (frame-type double-float-le)))

(define-binary-class ats-obj ()
  ((magic-number double-float-le)
   (sampling-rate double-float-le)
   (frame-size double-float-le)
   (window-size double-float-le)
   (partials double-float-le)
   (frames double-float-le)
   (ampmax double-float-le)
   (frqmax double-float-le)
   (dur double-float-le)
   (frame-type double-float-le)
   (data (ats-frames :class (find-frame-class (round frame-type))
                     :num-frames (round frames)
                     :num-partials (round partials)))))

|#

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

(defclass ats-t1-frame ()
  ((sinoids :accessor sinoids :initarg :sinoids :initform nil)))

(defclass ats-t2-frame ()
  ((sinoids :accessor sinoids :initarg :sinoids :initform nil)))

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


(defun read-frame (frame-type stream &key (num-partials 0))
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

(defun write-frame (type stream frame)
  (declare (ignore type stream frame)))

(define-binary-type ats-frames (frame-type num-partials num-frames)
  (:reader (in)
           (loop repeat num-frames
                 collect (read-frame frame-type in :num-partials num-partials)))
  (:writer (out frames)
    (loop for frame in frames
          do (write-frame frame-type out frame))))

#|

(let ((frame-type 1.0d0))
  (ecase frame-type
    (1.0d0 1)
    (2.0d0 2)
    (3.0d0 3)
    (4.0d0 4)
    (otherwise "no ats frame type ~a!" frame-type)))

(define-binary-class ats-obj ()
                     ((ats-header ats-header)
                      (ats-data ats-data-obj)))

(define-binary-class ats-data-obj ()
                     ((ats-data ats-data-list)))



(defun frame-class (type)
  (case type
    (1 'ats-ft1-data)
    (2 'ats-ft2-data)
    (3 'ats-ft3-data)
    (4 'ats-ft4-data)
    (otherwise (error "no ats frame type ~a!" type))))

;;; (frame-class 5)


(define-binary-type ats-frames (num-frames frame-type num-partials)
  (:reader (in)
           (loop repeat num-frames
                 with class = (frame-class frame-type)
                 collect (read-frame class in :num-partials num-partials)))
  (:writer (out frames)
           (loop for frame in frames
                 for class = (frame-class frame-type)
          do (write-value class out frame))))
q 

(define-binary-type ats-frame-type-1 ()
                     ((sin-data ats-ft1-sin-data-list)))

(define-binary-class ats-frame-type-1 ()
                     ((sin-data ats-ft1-sin-data-list)))

(define-binary-class ats-frame-type-2 ()
                     ((sin-data ats-ft2-sin-data-list)))

(define-binary-class ats-frame-type-3 ()
                     ((sin-data ats-ft1-sin-data-list)
                      (res-data ats-res-data)))

(define-binary-class ats-frame-type-4 ()
                     ((sin-data ats-ft2-sin-data-list)
                      (res-data ats-res-data)))

Hubertus Dreyer

info@sidneycorbett.de


12607040

01738425758


(define-binary-class info-list (riff-header)
  ((id2 (iso-8859-1-string :length 4)) (data (info-frames :tag-size (- size 4)))))

(define-binary-body-class ifil-ck (riff-header) ((major u2le) (minor u2le)))
(define-binary-body-class isng-ck (riff-header) ((isng (iso-8859-1-padded-string :length size))))
(define-binary-body-class irom-ck (riff-header) ((irom (iso-8859-1-padded-string :length size))))
(define-binary-body-class iver-ck (riff-header) ((major u2le) (minor u2le)))
(define-binary-body-class inam-ck (riff-header) ((inam (iso-8859-1-padded-string :length size))))
(define-binary-body-class icrd-ck (riff-header) ((icrd (iso-8859-1-padded-string :length size))))
(define-binary-body-class ieng-ck (riff-header) ((ieng (iso-8859-1-padded-string :length size))))
(define-binary-body-class iprd-ck (riff-header) ((iprd (iso-8859-1-padded-string :length size))))
(define-binary-body-class icop-ck (riff-header) ((icop (iso-8859-1-padded-string :length size))))
(define-binary-body-class icmt-ck (riff-header) ((icmt (iso-8859-1-padded-string :length size))))
(define-binary-body-class isft-ck (riff-header) ((isft (iso-8859-1-padded-string :length size))))

;;; set sort order of elems in INFO chunk for saving according to the ats spec:

(set-info-ck-file-precedence
 (ifil-ck isng-ck inam-ck irom-ck iver-ck icrd-ck ieng-ck iprd-ck icop-ck icmt-ck isft-ck))

(define-binary-class empty-ck (riff-header) ((data (skip-bytes :size size))))

(define-binary-class sdta-list (riff-header)
  ((id2 (iso-8859-1-string :length 4))
   (smpl smpl-ck)))

;;; (setf de.finnendahl.binary-data::*debug* t)
;;; (setf de.finnendahl.binary-data::*debug* nil)

(define-binary-class pdta-list (riff-header)
  ((id2 (iso-8859-1-string :length 4))
   (phdr (phdr-list)) (pbag (pbag-list)) (pmod pmod-list) (pgen pgen-list)
   (inst inst-list) (ibag ibag-list) (imod imod-list) (igen igen-list)
   (shdr shdr-list)))

(define-binary-class phdr-list (riff-header)
  ((phdr-frames (pdta-frames :tag-size size :class 'phdr-ck :frame-size 38))))

(define-binary-class pbag-list (riff-header)
  ((pbag-frames (pdta-frames :tag-size size :class 'pbag-ck :frame-size 4))))

(define-binary-class pmod-list (riff-header)
  ((pmod-frames (pdta-frames :tag-size size :class 'pmod-ck :frame-size 10))))

(define-binary-class pgen-list (riff-header)
  ((pgen-frames (pdta-frames :tag-size size :class 'pgen-ck :frame-size 4))))

(define-binary-class inst-list (riff-header)
  ((inst-frames (pdta-frames :tag-size size :class 'inst-ck :frame-size 22))))

(define-binary-class ibag-list (riff-header)
  ((ibag-frames (pdta-frames :tag-size size :class 'ibag-ck :frame-size 4))))

(define-binary-class imod-list (riff-header)
  ((imod-frames (pdta-frames :tag-size size :class 'imod-ck :frame-size 10))))

(define-binary-class igen-list (riff-header)
  ((igen-frames (pdta-frames :tag-size size :class 'igen-ck :frame-size 4))))

(define-binary-class shdr-list (riff-header)
  ((pgen-frames (pdta-frames :tag-size size :class 'shdr-ck :frame-size 46))))

(define-binary-class phdr-ck ()
  ((ach-preset-name (iso-8859-1-padded-string :length 20))
   (w-preset u2le)
   (w-bank u2le)
   (w-preset-bag-ndx u2le)
   (dw-library u4le)
   (dw-genre u4le)
   (dw-morpholgy u4le)))

(define-binary-class pbag-ck ()
  ((w-gen-ndx u2le)
   (w-mod-ndx u2le)))

(define-binary-class pmod-ck ()
  ((sf-mod-src-oper u2le)
   (sf-mod-dest-oper u2le)
   (sf-mod-amount u2le)
   (sf-mod-amt-src-oper u2le)
   (sf-mod-trans-oper u2le)))

(define-binary-class pgen-ck ()
  ((sf-gen-oper u2le)
   (gen-amount u2le)))

(define-binary-class inst-ck ()
  ((ach-inst-name (iso-8859-1-padded-string :length 20))
   (w-inst-bag-idx u2le)))

(define-binary-class ibag-ck ()
  ((w-inst-gen-ndx u2le)
   (w-inst-mod-ndx u2le)))

(define-binary-class imod-ck ()
  ((sf-mod-src-oper u2le)
   (sf-mod-dest-oper u2le)
   (sf-mod-amount u2le)
   (sf-mod-amt-src-oper u2le)
   (sf-mod-trans-oper u2le)))

(define-binary-class igen-ck ()
  ((sf-gen-oper u2le)
   (gen-amount u2le)))

(define-binary-class shdr-ck ()
  ((ach-sample-name (iso-8859-1-padded-string :length 20))
   (dw-start u4le)
   (dw-end u4le)
   (dw-start-loop u4le)
   (dw-end-loop u4le)
   (dw-sample-rate u4le)
   (by-original-pitch u1)
   (ch-pitch-correction u1)
   (w-sample-link u2le)
   (sf-sample-type u2le)))

(define-binary-type info-frames (tag-size)
  (:reader (in)
           (progn
             (loop with to-read = tag-size
                   while (plusp to-read)
                   for frame = (read-info-frame in)
                   while frame
                   do (decf to-read (+ 8 (size frame)))
                   collect frame into result
                   finally (progn
                             (dotimes (_ (1- to-read)) (read-byte in))
                             (return (sort result #'<
                                           :key (lambda (x) (get (type-of x) :file-precedence-idx))))))))
  (:writer (out frames)
    (loop with to-write = tag-size
          for frame in frames
          do (write-value 'info-frame out frame)
          (decf to-write (+ 8 (size frame)))
          finally (dotimes (_ to-write) (write-byte 0 out)))))

(define-binary-type pdta-frames (tag-size class frame-size)
  (:reader (in)
           (if (zerop (mod tag-size frame-size))
               (progn
                 (if *debug* (break "tag-size: ~a" tag-size))
                 (loop for preset below (/ tag-size frame-size)
                       for frame = (read-value class in)
                       do (if *debug* (break "frame: ~a" frame))
                       collect frame))))
  (:writer (out frames)
    (loop for frame in frames
          do (write-value 'phdr-ck out frame))))

(define-tagged-list-binary-class
    info-frame ()
  ((id (iso-8859-1-string :length 4))
   (size u4le))
  (:dispatch (find-frame-class id)))

(in-package :de.finnendahl.binary-data)

(progn
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (setf (get 'cl-ats::smpl-ck 'slots) '(file-pos))
    (setf (get 'cl-ats::smpl-ck 'superclasses) '(empty-ck)))
  
  (defclass cl-ats::smpl-ck (cl-ats::empty-ck)
    ((cl-ats::file-pos :initarg :file-pos :accessor file-pos)))

  (defmethod read-object progn ((obj cl-ats::smpl-ck) in)
    (declare (ignorable in))
    (break "read-object-binary: ~a, name: ~a" obj 'cl-ats::smpl-ck)
    (with-slots (cl-ats::file-pos cl-ats::size) obj
      (setf cl-ats::file-pos (- (file-position in) cl-ats::size))))
  
  (defmethod write-object progn ((obj cl-ats::smpl-ck) in)
    (declare (ignorable in))))

|#
