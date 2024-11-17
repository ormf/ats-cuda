;;; 
;;; globals.lisp
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

(in-package :ats-cuda)

(defvar *ats-file-dir* (pathname "/tmp/")
  "Default directory for ats files.

@See-also
load-ats
save-ats
")

(defvar *ats-snd-dir*
  (asdf:system-relative-pathname :ats-cuda "snd/")
  "Default directory for soundfiles to be analyzed by track-ats.

@See-also
track-ats
")
