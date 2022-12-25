;;; 
;;; defs.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2021 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
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

(in-package :de.finnendahl.binary-data)

(defun extend-symbol (name extention)
  (intern (string-upcase (format nil "~a-~a" name extention))))

(defmacro define-binary-hydra-class (name size)
  `(define-binary-class ,(extend-symbol name 'list)
       (riff-header)
       ((,(extend-symbol name 'frames)
         (pdta-frames :tag-size 'size
                      :class ',(extend-symbol name 'ck)
                      :frame-size ,size)))))

(export 'extend-symbol 'de.finnendahl.binary-data)
(export 'define-binary-hydra-class 'de.finnendahl.binary-data)

(defmacro define-binary-body-class (name (&rest superclasses) slots)
  (with-gensyms (objectvar streamvar)
    `(define-generic-binary-class ,name ,superclasses ,slots
       (defmethod read-body ((,objectvar ,name) ,streamvar)
         (declare (ignorable ,streamvar))
         (if *debug* (break "read-body-binary-object: ~a, name: ~a" ,objectvar ',name))
         (with-slots ,(new-class-all-slots slots superclasses) ,objectvar
           ,@(mapcar #'(lambda (x) (slot->read-value x streamvar)) slots))))))

(defmacro define-tagged-list-binary-class (name (&rest superclasses) slots &rest options)
  (with-gensyms (typevar objectvar streamvar)
    `(define-generic-binary-class ,name ,superclasses ,slots
      (defmethod read-value ((,typevar (eql ',name)) ,streamvar &key)
        (let* ,(mapcar #'(lambda (x) (slot->binding x streamvar)) slots)
          (let ((,objectvar
                 (make-instance 
                  ,@(or (cdr (assoc :dispatch options))
                        (error "Must supply :dispatch form."))
                  ,@(mapcan #'slot->keyword-arg slots))))
            (if *debug* (break "tagged-list-read-object: ~a, name: ~a" ,objectvar ',name))
            (with-slots ,(new-class-all-slots slots superclasses) ,objectvar
              (read-body ,objectvar ,streamvar))
            ,objectvar))))))

(export 'define-tagged-list-binary-class 'de.finnendahl.binary-data)
(export 'define-binary-body-class 'de.finnendahl.binary-data)

(in-package :ats-cuda)

;;; little endian

(define-binary-type unsigned-integer-le (bytes bits-per-byte)
  (:reader (in)
    (loop with value = 0
       for low-bit below (* bits-per-byte bytes) by bits-per-byte do
         (setf (ldb (byte bits-per-byte low-bit) value) (read-byte in))
       finally (return value)))
  (:writer (out value)
    (loop for low-bit below (* bits-per-byte bytes) by bits-per-byte
       do (write-byte (ldb (byte bits-per-byte low-bit) value) out))))

;;; (define-binary-type u1le () (unsigned-integer-le :bytes 1 :bits-per-byte 8))

(define-binary-type u2le () (unsigned-integer-le :bytes 2 :bits-per-byte 8))
(define-binary-type u3le () (unsigned-integer-le :bytes 3 :bits-per-byte 8))
(define-binary-type u4le () (unsigned-integer-le :bytes 4 :bits-per-byte 8))
(define-binary-type u8le () (unsigned-integer-le :bytes 8 :bits-per-byte 8))

(define-binary-type raw-bytes (size)
  (:reader (in)
           (let ((buf (make-array size :element-type '(unsigned-byte 8))))
             (read-sequence buf in)
      buf))
  (:writer (out buf)
    (write-sequence buf out)))

(define-binary-type skip-bytes (size)
  (:reader (in)
           (let ((buf (make-array size :element-type '(unsigned-byte 8))))
             (declare (ignore buf))
             (file-position in (+ (file-position in) size))
             (values)))
  (:writer (out buf)
    (write-sequence buf out)))

(define-binary-type generic-terminated-padded-string (terminator character-type length)
  (:reader (in)
    (with-output-to-string (s)
      (loop for char = (read-value character-type in)
            for to-read = length then (decf to-read)
            until (char= char terminator) do (write-char char s)
            finally (progn
                      (loop repeat (1- to-read) do (read-value character-type in))
                      (return s)))))
  (:writer (out string)
    (loop for char across string
          for to-write = length then (decf to-write)
          do (write-value character-type out char)
          finally (loop repeat to-write do (write-value character-type out terminator)))))

(define-binary-type double-float-le ()
  (:reader (in)
           (let ((bits-per-byte 8) (bytes 8))
             (loop
               with value = 0
               for low-bit from 0 below (* bits-per-byte bytes) by bits-per-byte do
                 (setf (ldb (byte bits-per-byte low-bit) value) (read-byte in))
               finally (return (ieee-floats:decode-float64 value)))))
  (:writer (out float-value)
;;;           (declare (type (float 1.0d0) float-value))
           (let ((bits-per-byte 8) (bytes 8)
                 (value (ieee-floats:encode-float64 float-value)))
                (loop for low-bit below (* bits-per-byte bytes) by bits-per-byte
                      do (write-byte (ldb (byte bits-per-byte low-bit) value) out)))))

(defmacro set-info-ck-file-precedence (seq)
  `(progn
     ,@(loop for i from 0 for s in seq collect `(setf (get ',s :file-precedence-idx) ,i))))

(defparameter *sf-sample-link-enum*
  '((:mono-sample 1)
    (:right-sample 2)
    (:left-sample 4)
    (:linked-sample 8)
    (:rom-mono-sample #x8001)
    (:rom-right-sample #x8002)
    (:rom-left-sample #x8004)
    (:rom-linked-sample #x8008)))

(defparameter *generator-enumerator-table*
  (make-array
   '(60)
   :initial-contents
   '(nil
     :start-addrs-offset
     :end-addrs-offset
     :startloop-addrs-offset
     :endloop-addrs-offset
     :start-addrs-coarse-offset
     :mod-lfo-to-pitch
     :vib-lfo-to-pitch
     :mod-env-to-pitch
     :initial-filter-fc
     :initial-filter-q
     :mod-lfo-to-filter-fc
     :mod-env-to-filter-fc
     :end-addrs-coarse-offset
     :mod-lfo-to-volume
     :undefined ;; 14
     :chorus-effects-send
     :reverb-effects-send
     :pan
     :undefined
     :undefined
     :undefined ;; 18,19,20
     :delay-mod-lfo
     :freq-mod-lfo
     :delay-vib-lfo
     :freq-vib-lfo
     :delay-mod-env
     :attack-mod-env
     :hold-mod-env
     :decay-mod-env
     :sustain-mod-env
     :release-mod-env
     :keynum-to-mod-env-hold
     :keynum-to-mod-env-decay
     :delay-vol-env
     :attack-vol-env
     :hold-vol-env
     :decay-vol-env
     :sustain-vol-env
     :release-vol-env
     :keynum-to-vol-env-hold
     :keynum-to-vol-env-decay
     :instrument
     :undefined ;; 42
     :key-range
     :vel-range
     :startloop-addrs-coarse-offset
     :keynum
     :velocity
     :initial-attenuation
     :undefined ;; 49
     :end-loop-addrs-coarse-offset
     :coarse-tune
     :fine-tune
     :sample-id
     :sample-modes
     :undefined ;; 55
     :scale-tuning
     :exclusive-class
     :overriding-root-key)))

;;; (length *generator-enumerator-table*)

(defun find-frame-class (id)
  (intern (string-upcase (format nil "~a-ck" id)) 'cl-sf2))

(define-condition in-padding () ())

(defun read-info-frame (in)
  (handler-case (read-value 'info-frame in)
    (in-padding () nil)))
