;;; -*- Syntax: Zetalisp; Mode: LISP; Package: Dvi; Base: 10 -*-

;;FOR THE DOVER
(defflavor press-stream
        ((part-list nil)                        ;Known parts.
         (current-record 0)                     ;Record being output.
         (entity-pending nil)                   ;There's an entity waiting to go.

         (data-list-length 0)                   ;DL length.
         (data-list-offset 0)                   ;DL begin-byte of current entity.

         (page-entity-buffer                    ;Current entity buffer being built.
           (make-array 4000. :type 'art-8b :fill-pointer 0 :area *temp-buffer*))
         (entity-list-length 0)                 ;EL length.
         (entity-list-offset 0)                 ;EL begin-byte of current entity.

         (entity-font-set 0)                    ;FontSet of current entity.
         (entity-x 0)                           ;Xe of current entity.
         (entity-y 0)                           ;Ye of current entity.
         (entity-left 0)                        ;Left of current entity.
         (entity-bottom 0)                      ;Bottom of current entity.
         (entity-width 0)                       ;Width of current entity.
         (entity-height 0)                      ;Height of current entity.

         (pending-characters 0)

         (part-directory-start 0)

         (copies 1)

         (graphic-x 0)                          ;Current X position.
         (graphic-y 0)                          ;Current Y position.

         (font-list ())                         ;Fonts that have been used.
         (current-font-set -1)                  ;Font set currently in use.
         (current-font-number -1)               ;Font currently in use.
         (highest-font-set 0)                   ;Highest font set number in use.
         (highest-font-number -1)               ;Highest font number in use.

         newvec-shape                           ;Current brush shape
         newvec-size                            ;Current brush thickness

         (x-offset 0)                           ;Same as Xe of current entity
         (y-offset 0)                           ;Same as Ye of current entity
         (width #.(fix (* 2540. 8.5)))          ;Same as entity-width
         (height #.(* 2540. 11.))               ;Same as entity-height

         press-pathname                         ;Output pathname for creating PRESS file
         output-stream)                         ;Output stream to opened PRESS file
        ()
  :settable-instance-variables
  :gettable-instance-variables)

;;; Define PRESS file variables.

;;; Entity List Commands.
(defconstant <Show-characters-short>             0)
(defconstant <Skip-characters-short>          #o40)
(defconstant <Show-characters-and-skip>      #o100)
(defconstant <Set-space-x-short>             #o140)
(defconstant <Set-space-y-short>             #o150)
(defconstant <Font>                          #o160)
(defconstant <Skip-control-bytes-immediate>  #o353)
(defconstant <Alternative>                   #o354)
(defconstant <Only-on-copy>                  #o355)
(defconstant <Set-x>                         #o356)
(defconstant <Set-y>                         #o357)
(defconstant <Show-characters>               #o360)
(defconstant <Skip-characters>               #o361)
(defconstant <Skip-control-bytes>            #o362)
(defconstant <Show-character-immediate>      #o363)
(defconstant <Set-space-x>                   #o364)
(defconstant <Set-space-y>                   #o365)
(defconstant <Reset-space>                   #o366)
(defconstant <Space>                         #o367)
(defconstant <Set-brightness>                #o370)
(defconstant <Set-hue>                       #o371)
(defconstant <Set-saturation>                #o372)
(defconstant <Show-object>                   #o373)
(defconstant <Show-dots>                     #o374)
(defconstant <Show-dots-opaque>              #o375)
(defconstant <Show-rectangle>                #o376)
(defconstant <Nop>                           #o377)

(defconstant entity-buffer-extension-size    4000.)

;;; Type registry.
(defconstant press-registry-type                 0)
(defconstant page-part-type                      0)
(defconstant font-part-type                      1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Press stream methods defined here are :
;;
;;  :reset-all-instance-variables
;;  :begin-entity
;;  :finish-entity
;;  :output-part
;;  :output-font-directory-part
;;  :output-a-font
;;  :output-part-directory
;;  :output-font-directory
;;  :finish-press-file
;;

(defmethod (press-stream :reset-all-instance-variables) ()
  (setf (fill-pointer page-entity-buffer) 0)
  (setq part-list nil
        current-record 0
        entity-pending nil
        data-list-length 0
        data-list-offset 0
        entity-list-length 0
        entity-list-offset 0
        entity-font-set 0
        entity-x 0
        entity-y 0
        entity-left 0
        entity-bottom 0
        entity-width 0
        entity-height 0
        pending-characters 0
        part-directory-start 0
        copies 1
        graphic-x 0
        graphic-y 0
        font-list nil
        current-font-set -1
        current-font-number -1
        highest-font-set 0
        highest-font-number -1
        x-offset 0
        y-offset 0
        width #.(fix (* 2540. 8.5))
        height #.(* 2540. 11.)))


;;; Begin a new entity.
(defmethod (press-stream :begin-entity) (font-set x y left bottom w h)
  (and entity-pending
       (send self :finish-entity))
  (setq entity-pending 'TRUE)
;  (or (zerop entity-list-offset)
;      (send self :finish-entity))
  (setq entity-font-set  font-set
        entity-x         x
        entity-y         y
        entity-left      left
        entity-bottom    bottom
        entity-width     w
        entity-height    h))

;;; Output entity trailer to entity list.
(defmethod (press-stream :finish-entity) ()
  (put-pending-characters)
  ;; Fill to end of word.
  (and (oddp entity-list-length)
       (el-byte <Nop>))
  ;; Calculate and output trailer to EL.
  (let ((total (// (+ (- entity-list-length entity-list-offset) 24.) 2))
        (dl-length (- data-list-length data-list-offset)))
    (el-byte    press-registry-type)
    (el-byte    entity-font-set)
    (el-32word  data-list-offset)
    (el-32word  dl-length)
    (el-word    entity-x)
    (el-word    entity-y)
    (el-word    entity-left)
    (el-word    entity-bottom)
    (el-word    entity-width)
    (el-word    entity-height)
    (el-word    total)
    (setq entity-list-offset entity-list-length
          data-list-offset   data-list-length)))

;;; Page part output functions.

;;; Outputs current page part being built (EL & DL).
(defmethod (press-stream :output-part) ()
  (and entity-pending
       (send self :finish-entity))
  (setq entity-pending nil)
  (cond ((oddp data-list-length)
         (incf data-list-length)
         (byte-out 0)))
  (word-out 0)
  (send output-stream :string-out page-entity-buffer)
  (let ((total (+ data-list-length 2 entity-list-length)))
    (let ((pad (- 512. (\ total 512.))))
      (pad-bytes-out pad)
      (push (list page-part-type                ;Printed page.
                  current-record                ;Start of page.
                  (1+ (// total 512.))          ;Number of records in page.
                  (// pad 2))                   ;Length of EL padding.
            part-list)
      (setq current-record (+ current-record (// total 512.) 1)
            data-list-length 0
            data-list-offset 0)
      (setf (fill-pointer page-entity-buffer) 0)
      (setq entity-list-length 0
            entity-list-offset 0))))

(defmethod (press-stream :add-fonts)(fontmap) ;array mapping fontnums to fontnames
  (dotimes (fontnum (fill-pointer fontmap))
    (push (list (// fontnum 16)(\ fontnum 16)(aref fontmap fontnum))
          font-list))
  (setq highest-font-set (// (1- (fill-pointer fontmap)) 16))
  (setq highest-font-number (\ (1- (fill-pointer fontmap)) 16)))

;;; Font directory part output functions.

;;; Output font directory part. Takes a list of fonts in format:
;;; ((FontSet FontNumber "Font") . . .)
(defmethod (press-stream :output-font-directory-part) ()
  (loop for font in font-list do (send self :output-a-font font))
  (word-out 0)                  ;End of font information.
  (let ((total (1+ (* 16. (length font-list)))))
    (let ((pad (- 256. (\ total 256.))))
      (pad-bytes-out (lsh pad 1))
      (push (list font-part-type        ;Font directory.
                  current-record        ;Start of directory.
                  (1+ (// total 256.))  ;Number of records.
                  0)            ;Undefined.
            part-list)
      (setq current-record (+ current-record 1 (// total 256.))))))

;;; Output one font directory entry.
(defmethod (press-stream :output-a-font) (font)
  (let ((font-set    (first  font))
        (font-number (second font)))
    (multiple-value-bind (family size face rotation)
        (decode-font (third font));font-name
      (word-out 16.)            ;Length.
      (byte-out font-set)       ;Font Set.
      (byte-out font-number)    ;Font Number.
      (byte-out 0)              ;M.
      (byte-out 127.)           ;N.
      (send output-stream :string-out family)   ;Font Family.
      (byte-out face)           ;Face.
      (byte-out 0)              ;Source.
      (word-out size)           ;Size.
      (word-out rotation))))    ;Rotation.

;;; Part directory and document directory output functions.

(defmethod (press-stream :output-part-directory) ()
  (setq part-directory-start current-record)
  (loop for part-record in (reverse part-list) do
        (loop for datum in part-record do
              (word-out datum)))
  (let ((total (* 4 (length part-list))))
    (let ((pad (- 256. (\ total 256.))))
      (pad-bytes-out (lsh pad 1))
      (setq current-record (+ current-record 1 (// total 256.))))))

(defmethod (press-stream :output-document-directory) ()
   (word-out 27183.)                            ;Password.
   (word-out (1+ current-record))               ;Total records in file.
   (word-out (length part-list))                ;Number of parts.
   (word-out part-directory-start)              ;Where part directory is.
   (word-out
     (- current-record part-directory-start))   ;Part directory length.
   (word-out 0)                                 ;Obsolete.
   (word-out 0)  (word-out 0)                   ;Date.
   (word-out 1)  (word-out copies)              ;Copies to print.
   (word-out -1) (word-out -1)                  ;Pages to print.
   (word-out -1)                                ;Printing mode.
   (loop for n from 13 to 127 do (word-out -1)) ;Unused.
   (bcpl-out (string press-pathname) 52.)       ;Filename.
   (bcpl-out (string user-id) 32.)              ;Creator.
   (bcpl-out (time:print-current-time nil) 40.) ;Creation date.
   (pad-bytes-out (lsh (- 256. #o276) 1)))

;;; Output part and document directories.
(defmethod (press-stream :finish-press-file) ()
  (send self :output-part-directory)
  (send self :output-document-directory))

;;; Higher-level entity commands.

(defmethod (press-stream :press-show-characters)(number)
  (incf pending-characters number))

(defmethod (press-stream :press-show-char)(char)
  (dl-byte char)
  (incf pending-characters))

(defmethod (press-stream :press-put-char)(char orig-posn)
  ;have to back up to compensate for automatic advancment of pen position
  (dl-byte char)
  (incf pending-characters)
  (setq graphic-x orig-posn)
  (send self :press-set-x graphic-x))

(defmethod (press-stream :press-set-x) (x)
  (put-pending-characters)
  (el-byte <Set-x>)
  (el-word (fixr x)))

(defmethod (press-stream :press-set-y) (y)
  (put-pending-characters)
  (el-byte <Set-y>)
  (el-word (fixr y)))

;;; Set X/Y position.
(defmethod (press-stream :press-set-position) (x y)
  (send self :press-set-x x)
  (send self :press-set-y y))

(defmethod (press-stream :press-set-rectangle)(w h)
  (send self :press-draw-rectangle w h)
  (incf graphic-x w)
  (send self :press-set-x graphic-x))

(defmethod (press-stream :press-draw-rectangle) (w h)
  (put-pending-characters)
  (el-byte <Show-rectangle>)
  (el-word (fixr w))
  (el-word (fixr h)))

;;; Change font.
(defmethod (press-stream :press-set-font) (number)
  (put-pending-characters)
  (el-byte (+ <Font> number)))

;;; Font name decoding.
(defvar digits-list '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
;;; Convert face code to internal representation.
(defmacro encode-face (face-code)
  `(loop for code being the array-elements of ,face-code
         sum (selectq code (#/B 2.) (#/L 4.) (#/I 1.) (#/C 6.) (#/E 12.) (otherwise 0.))))

;;; Decode a NEWVEC Press font in the form:
;;; <Family><Size><Face><Rotation> (Rotation in degrees.)
;;; Returns four values: family, size, face, rotation
(defun decode-newvec-font (string &aux index1 index2 index3)
  (let ((length (string-length string)))
    (or (setq index1 (string-search-set digits-list string))
        (progn (setq string (string-append string "10R")
                     index1 length)
               (incf length 3)))
    (or (setq index2 (string-search-not-set digits-list string (1+ index1)))
        (progn (setq string (string-append string "R")
                     index2 length)
               (incf length 1)))
    (or (setq index3 (string-search-set digits-list string (1+ index2)))
        (setq index3 length))
    (if (and (not (= index3 length))
             (string-search-not-set digits-list string (1+ index3)))
        (bad-dvi "Garbage after rotation in ~A" string))
    (let ((face (string-upcase (substring string index2 index3))))
      (values (convert-to-bcpl (string-upcase (substring string 0 index1)) 20)
              (let ((ibase 10.) (base 10.))
                (read-from-string (substring string index1 index2)))
              (encode-face face)
              (let ((rotation (substring string index3)))
                (if (zerop (string-length rotation))
                    0
                    (* 60. (let ((ibase 10.) (base 10.))
                             (read-from-string rotation)))))
              (if (string-equal face "R") "" face)))))

(defun convert-to-bcpl (string max-length)
  (let ((bcpl-string (make-array max-length :type art-string)))
    (aset (string-length string) bcpl-string 0)
    (copy-array-portion string 0 (string-length string)
                        bcpl-string 1 max-length)
    bcpl-string))

(defun decode-font (string)
  (if (string-equal (substring string 0 6) "NEWVEC")
      (decode-newvec-font string)
      (let ((tfm-fontdef (send *font-definitions* :get-hash string)))
        (values (tfd-family-name tfm-fontdef)
                (minus (fixr (* dvi2mica (tfd-scale tfm-fontdef))))
                (tfd-face-code tfm-fontdef)
                0))))

(defmethod (press-stream :set-font) (new-font-name)
  (let ((old-font-set current-font-set)
        (old-font-number current-font-number))
    (loop for font in font-list do
          (and (string-equal new-font-name (third font))
               (return (setq current-font-set (first font)
                             current-font-number (second font))))
          finally (incf highest-font-number)
          (and (> highest-font-number 15.)
               (setq highest-font-number 0
                     highest-font-set (1+ highest-font-set)))
          (setq current-font-set highest-font-set
                current-font-number highest-font-number)
          (push (list current-font-set current-font-number new-font-name)
                font-list))
    ;; Start new entity if necessary.
    (cond ((and (= old-font-set current-font-set)
                (= old-font-number current-font-number)))
          ((= old-font-set current-font-set)
           (send self :press-set-font current-font-number))
          (t
           (send self :begin-entity current-font-set x-offset y-offset 0 0 width height)
           (send self :press-set-position graphic-x graphic-y)
           (send self :press-set-font current-font-number)))))

;;; Basic graphic functions.
;;; These functions take arguments in PRESS units.(micas)

(defmethod (press-stream :graph-set-x) (x)
  (setq graphic-x x)
  (send self :press-set-x graphic-x))

(defmethod (press-stream :graph-set-y) (y)
  (setq graphic-y y)
  (send self :press-set-y graphic-y))

(defmethod (press-stream :graph-set-position) (x y)
  (send self :graph-set-x x)
  (send self :graph-set-y y))

;;; Start a new graphic entity.
(defmethod (press-stream :begin-graphic-entity) (font-set)
  (send self :begin-entity font-set x-offset y-offset 0 0 width height))

;;; Finish the current entity if the entity buffer is getting full.
;;; This will cause you to lose your cursor position and selected font.
;;; The problem is that if you have more than 32768 bytes in an entity,
;;; the Alto suffers from 16-bit brain-rot.
(defmethod (press-stream :maybe-new-entity) ()
  (cond ((> (- entity-list-length entity-list-offset) 25000.)
         (send self :begin-graphic-entity current-font-set)
         (send self :press-set-position (fix graphic-x) (fix graphic-y))
         (send self :press-set-font current-font-number))))

(defmethod (press-stream :begin-new-page)()
  (unless (minusp current-font-set)
    (send self :begin-entity current-font-set x-offset y-offset 0 0 width height))
  (unless (minusp current-font-number)
    (send self :press-set-font current-font-number)))

;;; Graphics using NEWVEC fonts for the Dover.
;;; Richard Mark Soley 4 April 1982.
;;; Rewritten for the LispM by Dinarte R. Morais
;;; 10 December 1983

(defvar *slope-table*)
(defvar *dx-table*)
(defvar *dy-table*)

;;; Pen (ahem, font) hacking.
;;;
;;; The variable font-list contains a list that looks like:
;;; ((FontSet FontNumber "FontName") . . .)
;;; which is compatible with output-font-directory-part.

;;; Choose another font (i.e., set the current font).  Note that
;;; this function changes font, remembers newly used fonts, and
;;; sometimes must start a new entity (if the font set wraps).
(defmethod (press-stream :set-pen) (size shape)
  (or (member size '(2. 4. 6. 8. 12. 16. 24. 32.))
      (bad-dvi "Can't use that pen size."))
  (setq shape (character (string-upcase shape)))
  (or (memq shape '(#/R #/S #/H #/V))
      (bad-dvi "Can't use that pen shape."))
  (send self :set-font
        (let ((base 10.) (ibase 10.))
          (format nil "~ANEWVEC~D" (if (= shape #/R) "" (ascii shape)) size))))


;;; Draw a line to new co-ordinates from present coordinates.
;;; Coordinates are in PRESS units.
(defmethod (press-stream :graph-move-to) (new-x new-y &optional (use-rectangles? t))
  (let ((old-x graphic-x) (old-y graphic-y))
    (cond
      ;; Is it a vertical line?
      ((= new-x old-x)
       (let ((h (abs (- new-y old-y))))
         (if (or (< h 500.) (not use-rectangles?))
             ;; Use NEWVEC font.
             (if (< new-y old-y)
                 ;; Going down.
                 (send self :graph-newvec-vertical-line '(64. 112. 136. 148. 154.) h -1)
                 ;; Going up.
                 (send self :graph-newvec-vertical-line '(0 80. 120. 140. 150.) h 1))
             ;; Otherwise use rectangles.
             ;; Show the first point.
             (dl-byte #o240)                    ;Isolated point in NEWVEC fonts
             (send self :press-show-characters 1)
             ;; Show the line.
             (send self :graph-vertical-line old-x old-y new-x new-y)
             ;; Show the end point.
             (send self :graph-set-position new-x new-y)
             (dl-byte #o240)
             (send self :press-show-characters 1))))
      ;; Is it a horizontal line?
      ((= new-y old-y)
       (let ((w (abs (- new-x old-x))))
         (if (or (< w 500.) (not use-rectangles?))
             ;; Use NEWVEC font.
             (if (> new-x old-x)
                 ;; Left to right.
                 (send self :graph-newvec-horizontal-line '(32. 96. 128. 144. 152.) w)
                 ;; Right to left.
                 (send self :graph-set-x new-x)
                 (send self :graph-newvec-horizontal-line '(32. 96. 128. 144. 152.) w)
                 (send self :graph-set-x new-x))
             ;; Otherwise use rectangles.
             (cond
               ;; Line going right to left.
               ((< new-x old-x)
                ;; Show the right point.
                (dl-byte #o240)                 ;Isolated point in NEWVEC fonts
                (send self :press-show-characters 1)
                ;; Put out the rectangle.
                (send self :graph-horizontal-line old-x old-y new-x new-y)
                ;; Move to and show the left point.
                (send self :graph-set-position new-x new-y)
                (dl-byte #o240)
                (send self :press-show-characters 1))
               ;; Line going left to right.
               (:else
                ;; Show the left point.
                (dl-byte #o240)                 ;Isolated point in NEWVEC fonts
                (send self :press-show-characters 1)
                ;; Put out the rectangle.
                (send self :graph-horizontal-line old-x old-y new-x new-y)
                ;; Move to the right point.
                (send self :graph-set-position new-x new-y)
                ;; Show the right point.
                (dl-byte #o240)
                (send self :press-show-characters 1))))))
      ;; The hard case; must use the NEWVEC fonts.
      (t
       (cond ((< new-x old-x)
              (send self :graph-set-position new-x new-y)
              (send self :graph-press-line new-x new-y old-x old-y)
              (send self :graph-set-position new-x new-y))
             (t
              (multiple-value (graphic-x graphic-y)
                (send self :graph-press-line old-x old-y new-x new-y))))))))

;;; Simple subroutines of graph-move-to.

(defvar *newvec-lengths* '(105.833336 52.916668 26.458334 13.229167 6.6145835))

;;; Put out a straight horizontal line using the NEWVEC font.
(defmethod (press-stream :graph-newvec-horizontal-line) (characters length)
  (loop for this-character in characters
        for this-length in *newvec-lengths* do
        (loop with count = 0
              until (< length this-length) do
              (decf length this-length)
              (dl-byte this-character)
              (incf count)
              finally (when (plusp count)
                        (send self :press-show-characters count)
                        (incf graphic-x (* count this-length)))))
  (setq graphic-x (fixr graphic-x)))

;;; Put out a straight vertical line using the NEWVEC font.
(defmethod (press-stream :graph-newvec-vertical-line) (characters length direction)
  (loop for this-character in characters
        for this-length in *newvec-lengths* do
        (loop with count = 0
              until (< length this-length) do
              (decf length this-length)
              (dl-byte this-character)
              (incf count)
              finally (when (plusp count)
                        (send self :press-show-characters count)
                        (incf graphic-y (* direction count this-length)))))
  (setq graphic-y (fixr graphic-y)))

;;; Pick characters from NEWVEC font surrounding given slope.
(defmacro pick-slope-chars (slope)
  `(loop for .char. from 1 to 63. do
         (and (< (aref *slope-table* .char.) ,slope)
              (return (values (1- .char.) .char.)))
         finally (return (values 63. 64.))))

(defvar *brush-shape->micas* '((2. 13.) (4. 26.) (6. 40.) (8. 53.)
                               (12. 79.) (16. 106.) (24. 159.) (32. 212.)))

(defun get-horizontal-brush-size (shape size)
  (selector shape string-equal
    (("" "R" "S" "V")
     (cadr (assq size *brush-shape->micas*)))
    ("H" 26.)))

;;; Put out a straight horizontal line.
(defmethod (press-stream :graph-horizontal-line) (from-x from-y to-x ignore)
  ;; Make line go left to right.
  (when (< to-x from-x) (psetq from-x to-x to-x from-x))
  ;; Get width and height of the rectangle.
  (let ((h (get-horizontal-brush-size newvec-shape newvec-size))
        (w (1+ (- to-x from-x))))
    ;; Position the point.
    (send self :graph-set-position from-x (- from-y (// h 2)))
    ;; Draw the rectangle.
    (send self :press-draw-rectangle (fixr w) (fixr h))))

(defun get-vertical-brush-size (shape size)
  (selector shape string-equal
    (("" "R" "S" "H")
     (cadr (assq size *brush-shape->micas*)))
    ("V" 26.)))

;;; Put out a straight vertical line.
(defmethod (press-stream :graph-vertical-line) (from-x from-y ignore to-y)
  ;; Make line go bottom to top.
  (and (< to-y from-y) (psetq from-y to-y to-y from-y))
  ;; Get width and height of the rectangle.
  (let ((w (get-vertical-brush-size newvec-shape newvec-size))
        (h (1+ (- to-y from-y))))
    ;; Position the point.
    (send self :graph-set-position (- from-x (// w 2)) from-y)
    ;; Draw the rectangle.
    (send self :press-draw-rectangle (fixr w) (fixr h))))

;;; Subroutine to put out a "non-straight" line.
;;; Algorithm derived from LispMachine Press package, my version.

;;; Put out a graphic NON-VERTICAL/HORIZONTAL line.  This is a subroutine,
;;; not intended for users, and takes PRESS units.
(defmethod (press-stream :graph-press-line) (from-x from-y to-x to-y)
  (let ((slope (// (float (- to-y from-y)) (- to-x from-x))))
    (multiple-value-bind (char1 char2) (pick-slope-chars slope)
      (do ((x from-x (+ x delta-x))
           (y from-y (+ y delta-y))
           (char 0)
           (delta-x 0.0)
           (delta-y 0.0)
           (x-delta1 (aref *dx-table* char1))
           (x-delta2 (aref *dx-table* char2))
           (y-delta1 (aref *dy-table* char1))
           (y-delta2 (aref *dy-table* char2))
           (length (+ (abs (- to-x from-x)) (abs (- to-y from-y))))
           (count 0)
           (new-graphic-x graphic-x)
           (new-graphic-y graphic-y)
           (done nil))
          (nil)
        ;; Pick char1 or char2 to match real slope.
        (let ((x-difference (- (+ x x-delta2) from-x)))
          (cond ((or (< x-difference 7.)
                     (< (// (- (+ y y-delta2) from-y) x-difference)
                        slope))
                 (setq char    char1
                       delta-x x-delta1
                       delta-y y-delta1))
                (t
                 (setq char    char2
                       delta-x x-delta2
                       delta-y y-delta2))))
        (do ((lengths '(0 80. 120. 140. 150.) (cdr lengths))
             (i char)
             (d 2 (* d 2)))
            ((or (<= (+ (abs (- (+ y delta-y) from-y))
                        (abs (- (+ x delta-x) from-x)))
                     length)
                 (setq done (null (cdr lengths)))))
          (setq char    (+ (// (- char (car lengths)) 2) (cadr lengths))
                i       (* (// i d) d)
                delta-x (// (aref *dx-table* i) (float d))
                delta-y (// (aref *dy-table* i) (float d))))
        (and done (return (values (fixr new-graphic-x)
                                  (fixr new-graphic-y)
                                  (send self :press-show-characters count))))
        (dl-byte char)
        (incf new-graphic-x delta-x)
        (incf new-graphic-y delta-y)
        (incf count)))))

;;;
;;; Setup functions.
;;;

;;; Set up tables used by line drawer.
(defun line-drawer-setup ()
  (setq *slope-table* (make-array 65.)
        *dx-table*    (make-array 65.)
        *dy-table*    (make-array 65.))
  (do ((i 0 (1+ i))
       (dx 0.0)
       (dy 16.0))
      ((= i #o101))
    (aset (cond ((zerop i) 1.0e18)              ;Positive infinity.
                ((= i #o100) -1.0e18)           ;Negative infinity.
                (t (// dy dx)))
          *slope-table* i)
    (aset (// (* dx 2540.0) 384.0) *dx-table* i)
    (aset (// (* dy 2540.0) 384.0) *dy-table* i)
    (cond ((< i 16.) (setq dx (1+ dx)))
          ((< i 48.) (setq dy (1- dy)))
          (t         (setq dx (1- dx))))))

(line-drawer-setup)

(defmethod (press-stream :graph-point) (x y)
  (send self :maybe-new-entity)
  (send self :graph-set-position x (- height y))
  (dl-byte #o240)                               ;Isolated point in the NEWVEC fonts.
  (send self :press-show-characters 1))

(defmethod (press-stream :graph-line) (xx1 yy1 xx2 yy2)
  (send self :maybe-new-entity)
  (if (and (= xx1 xx2) (= yy1 yy2)) (send self :graph-point xx1 yy1)
      ;; Make line go from left to right.
      (when (< xx2 xx1) (psetq xx1 xx2 xx2 xx1 yy1 yy2 yy2 yy1))
      (send self :graph-set-position xx1 (- height yy1))
      (send self :graph-move-to xx2 (- height yy2))))


(defvar *press-brush-thickness-alist* '((26. 4.) (53. 8.) (106. 16.) (159. 24.)))
(defvar *press-brush-type-alist* '((:circle "R") (:square "S")
                                   (:vertical "V") (:horizontal "H")))

;;; Set the current brush.
(defmethod (press-stream :set-brush) (brush-type brush-thickness)
  (send self :set-pen
        (setq newvec-size (cadr (assq brush-thickness *press-brush-thickness-alist*)))
        (setq newvec-shape (cadr (assq brush-type *press-brush-type-alist*)))))
