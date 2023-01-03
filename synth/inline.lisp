;;; 
;;; inline.lisp
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

(in-package :cl-user)


(declaim (inline aref))
(defun aref (array &rest subscripts)
  "Return the element of the ARRAY specified by the SUBSCRIPTS."
  (declare (truly-dynamic-extent subscripts))
  (row-major-aref array (apply #'%array-row-major-index array subscripts)))

;;; (setf aref/bit/sbit) are implemented using setf-functions,
;;; because they have to work with (setf (apply #'aref array subscripts))
;;; All other setfs can be done using setf-functions too, but I
;;; haven't found technical advantages or disadvantages for either
;;; scheme.
(declaim (inline (setf aref)))
(defun (setf aref) (new-value array &rest subscripts)
  (declare (truly-dynamic-extent subscripts)
           (type array array))
  (setf (row-major-aref array (apply #'%array-row-major-index array subscripts))
        new-value))
