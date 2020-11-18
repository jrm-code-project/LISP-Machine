;;;  -*- Mode:LISP; Package:FED; Base:10; Readtable:ZL -*-

;;; Loading this file adds a new type of font file to read "BFD"
;;; which stands for "better (than what) font descriptor"
;;; the need to read such showing up mostly when porting programs
;;; from one lispmachine system to another.

;;; from a verbal description of what a BFD file contains
;;; and a bit of reverse engineering (hack it until it works);
;;; on a few BFD files we have this. The BFD format is
;;; extremely close to the FONT format internal to FED
;;; for many years. At least, under that assumption things
;;; seem to work.
;;; 16-Jun-86 10:30:02 -gjc
;;; It worked on the some files a user wanted converted,
;;; although some characters didnt print properly, presumably
;;; because of differences in kerning. Needless to say
;;; this is unsupported code.

#|

Copyright LISP Machine, Inc. 1986
   See filename "Copyright.Text" for
 licensing and release information.

*********************************************************
*********************************************************
*** NOTE: This is an EXAMPLE, not LMI supported code. ***
*** information contained in this example is subject  ***
*** to change without notice. The ways of doing       ***
*** the things contained in the example may change    ***
*** between system releases. Some techniques which    ***
*** are mere examples in one release may become built ***
*** in system features in the next release. Use good  ***
*** judgement when copying these techniques. Most     ***
*** examples have been motivated by specific customer ***
*** requests, and may not be the best engineered      ***
*** or most efficient solution for someone else.      ***
*********************************************************
*********************************************************

|#

(WHEN (NOT (ASS #'STRING-EQUAL "BFD" COM-READ-FILE-TYPES))
  (SETQ COM-READ-FILE-TYPES (APPEND COM-READ-FILE-TYPES '(("BFD" BFD-COM-READ-FILE)))))

(DEFUN BFD-COM-READ-FILE (FILENAME FONTNAME)
  (MULTIPLE-VALUE (FILENAME FONTNAME)
    (GET-INPUT-FILENAME-AND-FONTNAME FILENAME FONTNAME "BFD"))
  (LET* ((BFD (BFD-READ-FILE FILENAME))
         (FD (BFD-INTO-FONT-DESCRIPTOR BFD)))
    (PUTPROP FONTNAME FILENAME 'BFD-FILE)
    (OR (EQ (FD-NAME FD) FONTNAME)
        (SETF (FD-NAME FD) FONTNAME))
    (FONT-NAME-SET-FONT-AND-DESCRIPTOR FONTNAME FD)))


(DEFUN BFD-INTO-FONT-DESCRIPTOR (BFD
                                 &AUX FONT-DESCRIPTOR
                                 (FONT-LENGTH (GETF BFD :DIMENSION))
                                 (LINE-SPACING (GETF BFD :LINE-SPACING))
                                 (RASTER-HEIGHT NIL);(FONT-RASTER-HEIGHT FONT)
                                 (BASELINE (GETF BFD :BASELINE))
                                 (BLINKER-HEIGHT (GETF BFD :BLINKER-HEIGHT))
                                 (BLINKER-WIDTH (GETF BFD :BLINKER-WIDTH))
                                 (SPACE-WIDTH (GETF BFD :CHAR-WIDTH))
                                 TEMP RASTER-WIDTH CHARACTER-WIDTH LEFT-KERN RASTER)
  "Create and return a font-descriptor containing the data from BFD."
  (SETQ FONT-DESCRIPTOR (MAKE-FONT-DESCRIPTOR :MAKE-ARRAY (:LENGTH FONT-LENGTH)
                                              FD-FILL-POINTER FONT-LENGTH))
  (SETF (FD-NAME FONT-DESCRIPTOR) (INTERN (GETF BFD :NAME) 'FONTS))
  (SETF (FD-LINE-SPACING FONT-DESCRIPTOR) LINE-SPACING)
  (SETF (FD-BASELINE FONT-DESCRIPTOR)BASELINE)
  (SETF (FD-BLINKER-HEIGHT FONT-DESCRIPTOR) BLINKER-HEIGHT)
  (SETF (FD-BLINKER-WIDTH FONT-DESCRIPTOR) BLINKER-WIDTH)
  (SETF (FD-SPACE-WIDTH FONT-DESCRIPTOR) SPACE-WIDTH)
  (DOLIST (CHAR (GETF BFD :CHARACTERS))
    (SETQ CHARACTER-WIDTH (OR (GETF CHAR :SET-WIDTH)
                              (GETF BFD :CHAR-WIDTH)))
    (SETQ RASTER-WIDTH (GETF CHAR :RASTER-WIDTH))
    (SETQ LEFT-KERN (OR (GETF CHAR :LEFT-KERN) 0))
    (SETQ RASTER-HEIGHT (GETF CHAR :RASTER-HEIGHT))
    (SETQ TEMP (MAKE-CHAR-DESCRIPTOR
                 :MAKE-ARRAY (:TYPE 'ART-4B
                                   :LENGTH (LIST RASTER-HEIGHT RASTER-WIDTH))
                 CD-CHAR-WIDTH CHARACTER-WIDTH
                 CD-CHAR-LEFT-KERN LEFT-KERN))
    (ASET TEMP FONT-DESCRIPTOR (GETF CHAR :CHAR-CODE))
    (SETQ RASTER (GETF CHAR :RASTER))
    (DO ((ROW 0 (1+ ROW)))
        (( ROW RASTER-HEIGHT))
      (DO ((COL 0 (1+ COL)))
          (( COL RASTER-WIDTH))
        (ASET (AREF RASTER ROW COL)
              TEMP ROW COL))))
  FONT-DESCRIPTOR)


(defun bfd-read-file (filename &OPTIONAL &KEY (VERBOSE T))
  (with-open-file (stream filename
                          :direction :input
                          :characters nil
                          :byte-size 16)
    (let ((header (bfd-read-header stream))
          (characters nil))
      (WHEN VERBOSE
        (format t "~&Reading ~D characters for font ~S"
                (getf header :number-of-characters)
                (getf header :name)))
      (dotimes (j (getf header :number-of-characters))
        (push (bfd-read-character-info stream) characters))
      (setq characters (nreverse characters))
      (do ((l characters (cdr l)))
          ((null l))
        (setf (getf (car l) :raster) (bfd-read-raster-array stream (getf (car l) :hsize) (getf (car l) :vsize))))
      (append header (list :characters characters)))))

(defun bfd-read-16B (stream)
  (send stream :tyi))

(defun bfd-read-16b2c (stream)
  (let ((n (send stream :tyi)))
    (cond ((bit-test #.(expt 2 16) n)
           (- n #.(expt 2 15)))
          ('else
           n))))

(defun bfd-read-ascic (stream)
  (let ((length (bfd-read-16b2c stream)))
    (if (> length 250) (cerror "do it anyway" "Unreasonably long string. Probably bad BFD file."))
    (do ((s (make-string length))
         (j 0 (1+ j)))
        ((= j length) s)
      (setf (aref s j) (send stream :tyi)))))

(defun bfd-read-header (stream)
  (dolist (c '(#\B #\F #\D #o1000))
    (or (eq c (bfd-read-16b stream))
        (ferror "Not a bfd file: ~A" (send stream :truename))))
  (append (list :name (bfd-read-ascic stream)
                :line-spacing (bfd-read-16b stream)
                :baseline (bfd-read-16b stream)
                :char-width (bfd-read-16b stream))
          (bfd-read-plist stream)
          (list :dimension (bfd-read-16b stream)
                :number-of-characters (bfd-read-16b stream))))

(defun bfd-read-plist (stream)
  (do ((prop)
       (plist))
      ((zerop (length (setq prop (bfd-read-ascic stream))))
       plist)
    (let ((key (intern prop ""))
          (count (bfd-read-16b stream)))
      (cond ((memq key '(:blinker-width :point-size
                                        :blinker-height :horizontal-resolution
                                        :vertical-resolution))
             (do ((number 0 (dpb (bfd-read-16b stream) (byte 16 (* j 16)) number))
                  (j 0 (1+ j)))
                 ((= j count)
                  (setf (getf plist key) number))))
            ('else
             (format t "~&Unknown BFD property: ~S, value of ~D bytes being ignored~%"
                     prop count)
             (dotimes (j count)
               (send stream :tyi)))))))


(defun bfd-read-character-info (stream)
  (list* :char-code (bfd-read-16b stream)
         :set-width (bfd-read-16b2c stream)
         :left-kern (bfd-read-16b2c stream)
         :top-kern (bfd-read-16b2c stream)
         :raster-width (bfd-read-16b stream)
         :raster-height (bfd-read-16b stream)
         :hsize (bfd-read-16b stream)
         :vsize (bfd-read-16b stream)
         :hoff (bfd-read-16b2c stream)
         :voff (bfd-read-16b2c stream)
         :hinc (bfd-read-16b2c stream)
         :vinc (bfd-read-16b2c stream)
         (bfd-read-plist stream)))

(defun bfd-read-raster-array (stream x-hsize vsize &aux hsize)
  ;; kludge HSIZE must actually be even because of the way
  ;; that BITBLT works.
  (setq hsize (if  (zerop (mod x-hsize 2)) x-hsize (1+ x-hsize)))
  (let* ((array (make-array (list vsize (* hsize 16))
                            :type 'art-1b))
         (pointer (make-array (* hsize vsize) :displaced-to array :type 'art-16b)))
    #||
    (dotimes (j (* hsize vsize))
      (setf (aref pointer j) (bfd-read-16b stream)))
    ||#
    ;; el spedo
    (send stream :string-in t pointer)
    array))

(defun bfd-display-font (f &optional (wait t))
  (format t "~&Font named ~S has ~D characters, max code = #o~O~%"
          (getf f :name)
          (getf f :number-of-characters)
          (1- (getf f :dimension)))
  (send terminal-io :tyi)
  (dolist (char (getf f :characters))
    (cursorpos 'c)
    (format t "Character code = #o~O, ~:C~%" (getf char :char-code) (getf char :char-code))
    (send terminal-io :bitblt
          tv:alu-xor
          (getf char :raster-width)
          (getf char :raster-height)
          (getf char :raster)
          0 0
          100 100)
    (and wait (send terminal-io :tyi))))
