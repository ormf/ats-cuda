
;;; 
;;; scratch.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2021 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
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

(in-package :cl-ats)

(read-ats "/home/orm/work/programmieren/lisp/cl-ats/doc/pat-waing.ats")
(read-ats "/usr/share/csoundqt/Examples/SourceMaterials/basoon-C4.ats")

(defun hexseq->integer (seq)
  (loop for x in seq
        for i = 1 then (* i 256)
        summing (* i x)))

(hexseq->integer '(#x00 #x00 #x00 #x00 #x00 #xC0 #x5E #x40))

(hexseq->integer (reverse '(#x00 #x00 #x00 #x00  #x00 #xC0 #x5E #x40)))

12607040

'(#x0000000000C05E40)

(ieee-floats:decode-float64
 (hexseq->integer '(#x00 #x00 #x00 #x00 #x00 #xC0 #x5E #x40)))



#x60

(read-sf2 "/home/orm/work/snd/sf2/WaTaShi.sf2")
(defparameter *sol-flute* (read-sf2 "/home/orm/work/snd/sf2/sol-flute.sf2"))

*sol-flute*


(/ 322 46)

(defparameter *in-test*
  (make-array '(184) :element-type '(unsigned-byte 8) :initial-contents
              (list #x69 #x66 #x69 #x6c #x04 #x00 #x00 #x00 ;;; ifil....
                    #x02 #x00 #x01 #x00 #x69 #x73 #x6e #x67 #x08 #x00 #x00 #x00 #x45 #x4d #x55 #x38 ;;; ....isng....EMU8
                    #x30 #x30 #x30 #x00 #x49 #x4e #x41 #x4d #x08 #x00 #x00 #x00 #x57 #x61 #x54 #x61 ;;; 000.INAM....WaTa
                    #x53 #x68 #x69 #x00 #x49 #x53 #x46 #x54 #x0a #x00 #x00 #x00 #x50 #x6f #x6c #x79 ;;; Shi.ISFT....Poly
                    #x70 #x68 #x6f #x6e #x65 #x00 #x4c #x49 #x53 #x54 #x1c #xb9 #x02 #x00 #x73 #x64 ;;; phone.LIST....sd
                    #x74 #x61 #x73 #x6d #x70 #x6c #x10 #xb9 #x02 #x00 #xff #xff #xfd #xff #xf7 #xff ;;; tasmpl..........
                    #xf2 #xff #xe6 #xff #xf4 #xff #xd0 #xff #xad #xff #x90 #xff #x4f #xff #x07 #xff ;;; ............O...
                    #xd6 #xfe #xd9 #xfe #xef #xfe #x05 #xff #xee #xfe #x00 #xff #xdc #xfe #x32 #xff ;;; ..............2.
                    #x6f #xff #x40 #xff #xcc #xfe #x83 #xfe #x59 #xfe #xb8 #xfd #xca #xfd #x6c #xfd ;;; o.@.....Y.....l.
                    #xd3 #xfc #xc6 #xfc #xef #xfc #xec #xfc #xcd #xfc #x92 #xfc #x92 #xfc #xab #xfb ;;; ................
                    #xc9 #xfb #x8c #xfb #x24 #xfb #xbf #xfa #x86 #xfa #x50 #xfa #xea #xf9 #xd3 #xf9 ;;; ....$.....P.....
                    #xfc #xf9 #xd0 #xf9 #x78 #xf9 #x8d #xf9 #x35 #xf9 #x16 #xf9 #xd2 #xf8 #xaa #xf8)))


(defparameter *in-test*
  (make-array '(196) :element-type '(unsigned-byte 8) :initial-contents
              (list #x4c #x49 #x53 #x54 ;;; LIST
                    #x42 #x00 #x00 #x00 #x49 #x4e #x46 #x4f ;;; B...INFO
                    #x69 #x66 #x69 #x6c #x04 #x00 #x00 #x00 ;;; ifil....
                    #x02 #x00 #x01 #x00 #x69 #x73 #x6e #x67 #x08 #x00 #x00 #x00 #x45 #x4d #x55 #x38 ;;; ....isng....EMU8
                    #x30 #x30 #x30 #x00 #x49 #x4e #x41 #x4d #x08 #x00 #x00 #x00 #x57 #x61 #x54 #x61 ;;; 000.INAM....WaTa
                    #x53 #x68 #x69 #x00 #x49 #x53 #x46 #x54 #x0a #x00 #x00 #x00 #x50 #x6f #x6c #x79 ;;; Shi.ISFT....Poly
                    #x70 #x68 #x6f #x6e #x65 #x00 #x4c #x49 #x53 #x54 #x1c #xb9 #x02 #x00 #x73 #x64 ;;; phone.LIST....sd
                    #x74 #x61 #x73 #x6d #x70 #x6c #x10 #xb9 #x02 #x00 #xff #xff #xfd #xff #xf7 #xff ;;; tasmpl..........
                    #xf2 #xff #xe6 #xff #xf4 #xff #xd0 #xff #xad #xff #x90 #xff #x4f #xff #x07 #xff ;;; ............O...
                    #xd6 #xfe #xd9 #xfe #xef #xfe #x05 #xff #xee #xfe #x00 #xff #xdc #xfe #x32 #xff ;;; ..............2.
                    #x6f #xff #x40 #xff #xcc #xfe #x83 #xfe #x59 #xfe #xb8 #xfd #xca #xfd #x6c #xfd ;;; o.@.....Y.....l.
                    #xd3 #xfc #xc6 #xfc #xef #xfc #xec #xfc #xcd #xfc #x92 #xfc #x92 #xfc #xab #xfb ;;; ................
                    #xc9 #xfb #x8c #xfb #x24 #xfb #xbf #xfa #x86 #xfa #x50 #xfa #xea #xf9 #xd3 #xf9 ;;; ....$.....P.....
                    #xfc #xf9 #xd0 #xf9 #x78 #xf9 #x8d #xf9 #x35 #xf9 #x16 #xf9 #xd2 #xf8 #xaa #xf8)))

(ironclad:with-octet-input-stream (in *in-test*)
  (loop repeat 10 collect (read-byte in)))

(ironclad:with-octet-input-stream (in *in-test*)
  (let ((obj1 (make-instance 'info-ck)))
    (de.finnendahl.binary-data::read-object obj1 in)
    obj1))


(ironclad:with-octet-input-stream (in (subseq *in-test* 12 nil))
  (let ((obj1 (make-instance 'info-ck)))
    (de.finnendahl.binary-data::read-value 'sfil-ck in)
    obj1))

(progn (untrace)
       (trace de.finnendahl.binary-data::read-value
              de.finnendahl.binary-data::read-object
              de.finnendahl.binary-data::read-body
              read-frame
              ))

(get 'ifil-ck 'de.finnendahl.binary-data::slots)



(ironclad:with-octet-input-stream (in *in-test*)
  (let ((obj1 (make-instance 'ifil-ck))
        (obj2 (make-instance 'isng-ck))
        (obj3 (make-instance 'inam-ck))
        (obj4 (make-instance 'isft-ck)))
    (de.finnendahl.binary-data::read-object obj1 in)
    (de.finnendahl.binary-data::read-object obj2 in)
    (de.finnendahl.binary-data::read-object obj3 in)
    (de.finnendahl.binary-data::read-object obj4 in)

;;;    (de.finnendahl.binary-data::read-object obj3 in)
    (list obj1 obj2 obj3 obj4)))


(with-input-from-string (in (make-array initial-contents)))

(octet-stream)


#|
id: RIFF sf2-obj
<size>
sfbk sfbk-obj
  id: LIST
  <size>
  INFO info-obj
    ifil ...

  id: LIST list-obj
  <size>
  sdta sdta-obj

  id: LIST list-obj
  <size>
  pdta pdta-obj
    phdr ...
    pbag ...
    pmod ...
    pgen ...
    inst ...
    ibag ...
    imod ...
    igen ...
    shdr ...
EOS
00000000000
|#





<iver-rec>
-> struct sfVersionTag
{
WORD wMajor;
WORD wMinor;
};
<phdr-rec>
-> struct sfPresetHeader
{
CHAR achPresetName[20];
WORD wPreset;
WORD wBank;
WORD wPresetBagNdx;
DWORD dwLibrary;
DWORD dwGenre;
DWORD dwMorphology;
};
<pbag-rec>
-> struct sfPresetBag
{
WORD wGenNdx;
WORD wModNdx;
};
<pmod-rec> -> struct sfModList
{
SFModulator sfModSrcOper;
SFGenerator sfModDestOper;
SHORT modAmount;
SFModulator sfModAmtSrcOper;
SFTransform sfModTransOper;
};
<pgen-rec>
-> struct sfGenList
{
SFGenerator sfGenOper;
genAmountType genAmount;
};
<inst-rec>
-> struct sfInst
{
CHAR achInstName[20];
WORD wInstBagNdx;
};

-> struct sfInstBag
{
WORD wInstGenNdx;
WORD wInstModNdx;
};
<imod-rec> -> struct sfInstModList
{
SFModulator sfModSrcOper;
SFGenerator sfModDestOper;
SHORT modAmount;
SFModulator sfModAmtSrcOper;
SFTransform sfModTransOper;
};
<igen-rec>
-> struct sfInstGenList
{
SFGenerator sfGenOper;
genAmountType genAmount;
};
<shdr-rec>
-> struct sfSample
{
CHAR achSampleName[20];
DWORD dwStart;
DWORD dwEnd;
DWORD dwStartloop;
DWORD dwEndloop;
DWORD dwSampleRate;
BYTE byOriginalKey;
CHAR chCorrection;
WORD wSampleLink;
SFSampleLink sfSampleType;


(defun fibonacci (n)
  (let ((memoize (make-hash-table)))
    (labels ((inner (n)
               (cond ((< n 3) n)
                     (t (or (gethash n memoize)
                            (setf (gethash n memoize)
                                  (+ (inner (- n 1))
                                     (inner (- n 2)))))))))
      (inner n))))

(time (fibonacci 100))

(loop
  repeat 10
  for last1 = 1 then last2
  for last2 = 1 then (+ last1 last2)
  while (< last1 4000000)
  if (evenp last1) collect last1)

(apply #'+
       (loop
         for last2 = 1 then last1
         for last1 = 1 then curr
         for curr = (+ last1 last2)
         while (<= curr 4000000)
         if (evenp curr) collect curr))

(defun even-fibonacci (n))
(class-name (type-of (make-instance 'ifil-ck)))

(get (class-name (find-class 'inam-ck)) :file-precedence-idx)

(export
 '(ifil-ck isng-ck inam-ck irom-ck iver-ck icrd-ck ieng-ck iprd-ck icop-ck icmt-ck isft-ck) 'de.finnendahl.binary-data)

(in-package :scratch)

(buffer-load)
