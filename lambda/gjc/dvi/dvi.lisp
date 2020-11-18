;;; -*- Mode: LISP; Syntax: Zetalisp; Package: DVI; Base: 10; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;0. System constants, variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Some of the DVI numerical opcodes of the commands
(defconstant set-char0 0)
(defconstant set1 128)
(defconstant set2 129)
(defconstant set3 130)
(defconstant set4 131)
(defconstant set-rule 132)
(defconstant put1 133)
(defconstant put2 134)
(defconstant put3 135)
(defconstant put4 136)
(defconstant put-rule 137)
(defconstant nop 138)
(defconstant bop 139)
(defconstant eop 140)
(defconstant dvi-push 141)
(defconstant dvi-pop 142)
(defconstant right1 143)
(defconstant right2 144)
(defconstant right3 145)
(defconstant right4 146)
(defconstant w0 147)
(defconstant w1 148)
(defconstant w2 149)
(defconstant w3 150)
(defconstant w4 151)
(defconstant x0 152)
(defconstant x1 153)
(defconstant x2 154)
(defconstant x3 155)
(defconstant x4 156)
(defconstant down1 157)
(defconstant down2 158)
(defconstant down3 159)
(defconstant down4 160)
(defconstant y0 161)
(defconstant y1 162)
(defconstant y2 163)
(defconstant y3 164)
(defconstant y4 165)
(defconstant z0 166)
(defconstant z1 167)
(defconstant z2 168)
(defconstant z3 169)
(defconstant z4 170)
(defconstant fntnum0 171)
(defconstant fnt1 235)
(defconstant fnt2 236)
(defconstant fnt3 237)
(defconstant fnt4 238)
(defconstant xxx1 239)
(defconstant xxx2 240)
(defconstant xxx3 241)
(defconstant xxx4 242)
(defconstant fntdef1 243)
(defconstant fntdef2 244)
(defconstant fntdef3 245)
(defconstant fntdef4 246)
(defconstant pre 247)
(defconstant post 248)
(defconstant post-post 249)


(defconstant begin-output 247)
(defconstant end-output 248)
(defconstant do-pages 249)

(defconstant id-byte 2) ;; this is actually the "version number" of
                           ;; the DVI format used.  This program
                           ;; implements the current version - 2

;;; Some conversion factors.
(defconstant chars-per-font 128)
(defconstant TwoToTheTwenty 1048576)
(defconstant rsus-per-inch 254000)
(defconstant points-per-inch 72.27)
(defconstant micas-per-dpix (// (dfloat 2540) 384))
(defconstant dpix-per-mica (// (dfloat 384)2540))
(defconstant rsus-per-mica 100)
(defconstant micas-per-rsu 0.01)

;;a file buffer for reading in 8-bit files. Overcomes net access delays in reading
;;PXL and TFM files where the file pointer has to be reset frequently.
(defresource file-buffer()
  :constructor (make-array 25000 :type 'art-8b :fill-pointer 0 :leader-length 2)
  :checker (progn object (not in-use-p)))

;; a temporary area for output buffers.
(defvar *temp-buffer* #+LMI WORKING-STORAGE-AREA
                      #+(OR TI SYMBOLICS) (make-area :name '*temp-buffer*
                                                     :gc #+SYMBOLICS :ephemeral #+TI :dynamic
                                                     :room t))
;; Some global variables

(defvar *font-definitions* (make-equal-hash-table :size 111 :area permanent-storage-area))

(defvar translation-array)

(defvar max-stack 0)
(defvar total-pages 0)
(defvar numerator)
(defvar denominator)
(defvar magnification)
(defvar pixels-per-page)
(defvar pixels-per-line)
(defvar center-text t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;1. Data structures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.1 The fontdef structure
;;         This structure holds information about a font : the character widths,
;; heights, bitmaps, etc.  read in from a PXL file.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (fontdef (:conc-name fd-)
                    (:type :named-array)
                    (:make-array (:area permanent-storage-area)))
  external-name  ;string
  raster-info    ;array[array[art-8b]]
  font-directory ; array[char-info]
  bitmaps ; array of char bitmaps for caching bitmaps used in screen displays.
  char-status ;array[bool]
                       )

(defstruct (char-info (:conc-name char-)
                      (:make-array (:area permanent-storage-area)))
  width height x-offset y-offset
  dvi-width
  pxl-width)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;1.1. a. read-pxlfile.  Reads the checksums, design
;;        sizes etc, of the file, and returns a font definition.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun read-pxlfile (pxlfilename checksum scale-factor design-size)
  (using-resource (fbuffer file-buffer)
    (let ((fdef (make-fontdef external-name pxlfilename)))
      (with-open-stream
        (pxlstr (open-8b-input (string-append *pxl-filename-prepend* pxlfilename)))
        (format t "~&    Reading font information for font ~S..." pxlfilename)
        (store-file-length fbuffer (file-stream-length pxlstr));store file-length
        (when (< (array-length fbuffer) (file-length fbuffer))
          (array-grow fbuffer (file-length fbuffer)))
        (send pxlstr :string-in nil fbuffer 0 (file-length fbuffer)))
    ;;the fill pointer points to the end of the file after the string in
    ;;read trailer info
      (decf (fill-pointer fbuffer) 20)
      (if ( checksum (bsigned-quad fbuffer))
          (format t  "  Warning - checksums don't match. "))
      (bsigned-quad fbuffer)
      (if (  design-size (fix (fix2dvi (bsigned-quad fbuffer))))
          (format t  "  Warning - design sizes don't match.  "))
      (setf (fill-pointer fbuffer)(* (bsigned-quad fbuffer) 4));;point to directory.
      (multiple-value-bind (font-dir raster-info)
          (read-fontdir fbuffer scale-factor)
        (setf (fd-raster-info fdef) raster-info)
        (setf (fd-font-directory fdef) font-dir))
      (format t "Done.")
      fdef)))

(defun read-glyph (file-buffer pointer cwidth cheight)
  ;;
  ;; pointer points to the position in the pxl file buffer for the glyph info
  ;;
  (let* ((min-bytes-per-row (ceiling (// (float cwidth) 8)))
         (glyph (make-array (* min-bytes-per-row cheight) :type art-8b)))
    (setf (fill-pointer file-buffer) pointer)
    (dotimes (i cheight)
      (dotimes (j (* (ceiling (// (float cwidth) 32)) 4));;bytes per row in pxlfile
        (cond (( j (1- min-bytes-per-row))
               (aset (bget-byte file-buffer) glyph (+ j (* i min-bytes-per-row))))
              (t (bget-byte file-buffer)))))
    glyph))


(defun read-fontdir (file-buffer scale-factor)
  ;
  ; read the font directory and raster info parts of
  ; a pxl file.  Requires that the file pointer points
  ; to the directory portion of the pxlfile
  ;
  (let ((fdir (make-array chars-per-font))
        (rast-info (make-array chars-per-font))
        (glyph-ptrs (make-array chars-per-font)))

    ;;read the font directory
    (dotimes (i chars-per-font) ; for all chars
      (let ((chr-info (make-char-info)))
        (setf (char-width chr-info) (bsigned-pair file-buffer))
        (setf (char-height chr-info) (bsigned-pair file-buffer))
        (setf (char-x-offset chr-info) (bsigned-pair file-buffer))
        (setf (char-y-offset chr-info) (bsigned-pair file-buffer))
        (aset (* (bsigned-quad file-buffer) 4) glyph-ptrs i)
        (setf (char-dvi-width chr-info)
              (// (* scale-factor (bsigned-quad file-buffer));;i.e. tfm width
                  (dfloat TwoToTheTwenty)))
        (setf (char-pxl-width chr-info) (pixel-round (char-dvi-width chr-info)))
        (aset chr-info fdir i)))

    ;;read the glyphs
    (dotimes (i chars-per-font) ;for all chars
      (aset
        (read-glyph
              file-buffer (aref glyph-ptrs i)
              (char-width (aref fdir i))(char-height (aref fdir i)))
        rast-info i))
    (values fdir rast-info)))

;;figure out the name of the pxl file
(defun get-pxl-fname (pxls-per-in fontname scale design)
  (let* ((realsize (* (// magnification (dfloat 1000))
                       (// scale (dfloat design))))
         (unmodsize (fix (* realsize 1000)))
         size);for the time being
    ;; This hack was taken from the file dvi-imagen.c on the vax.
    ;; Probably has to be modified for printers with different resolutions
    (selectq unmodsize
      (1095 (setq realsize 1.095445))
      (1199 (setq realsize 1.1997))
      (1315 (setq realsize 1.314534))
      (2074 (setq realsize 2.0736))
      (2488 (setq realsize 2.48832))
      (2986 (setq realsize 2.985984)))
    (setq size (fixr (* realsize pxls-per-in 5)))
    (if (= size 1383) (decf size))
    (if (= size 691) (incf size))
    #+SYMBOLICS
    (format nil "~A.~Dpxl" fontname size)
    #+(OR LMI TI)
    (get-best-pxl-fname fontname size)))



;;; The following function will first look if the ideal font file for FONTNAME
;;; at SIZE exists; if so, the filename is returned, otherwise the directory
;;; is searched to find then next smaller size for FONTNAME.  If none smaller
;;; is found, the function returns the filename for the (nonexistent) ideal
;;; font file.

#+(OR LMI TI)
(defun dvi-directory (x)
  (mapcar #'car (cdr (fs:directory-list x))))

#+(OR LMI TI)
(defun get-best-pxl-fname (fontname size)
  (cond ((probef (format nil "~A~A.~Dpxl" *pxl-filename-prepend* fontname size))
         (format nil "~A.~Dpxl" fontname size))
        (t (let ((pxl-files (dvi-directory (format nil "~A~A.*pxl" *pxl-filename-prepend* fontname))))
             (let ((sorted (sort (mapcar #'extract-pxl-pathname-size pxl-files) #'>)))
               (dolist (new-size sorted
                                 (cond ((null sorted)
                                        (format nil "~A.~Dpxl" fontname size))
                                       ('else
                                        (format t "~&Warning picking larger size pxl file: ~A.~Dpxl for ~A.~dpxl"
                                                fontname (car (last sorted))
                                                fontname size)
                                        (format nil "~a.~Dpxl" fontname (car (last sorted))))))
                 (if (< new-size size)
                     (return (format nil "~A.~Dpxl" fontname new-size)))))))))

#+(OR LMI TI)
;;; The following function takes a pixel pathname and extracts its size; e.g.,
;;; given the pathname for AMB10.1800pxl, the function returns 1800.

(defun extract-pxl-pathname-size (pxl-pathname)
  (let ((s (send pxl-pathname :type)))
    (parse-integer s :end (string-search "p" s))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; .1. b This is for reaing in font information from a TFM file. This is faster
;; than read-pxlfile since all it needs to get are the character widths.
;; This is used when producing press files.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (tfm-fontdef (:conc-name tfd-)
                        (:type :named-array))
  scale
  family-name  ; BCPL string of family name of font, e.g., CMR, CMB
  (face-code 0); coding of the face of the font (see Press File Format for details).
  dvi-widths)  ; the array containing the character widths in DVI units.

(defun read-tfmfile (fontname checksum scale-factor ignore)
  ;
  ; returns a tfm-fontdef
  ;
  (using-resource (fbuffer file-buffer)
    (let ((tfm-fdef (make-tfm-fontdef scale scale-factor))
          lf lh bc ec nw width-indices width-table)
      (with-open-stream
        (tfmstr (open-8b-input (string-append *tfm-filename-prepend* fontname ".tfm")))
        (format t
          "~&    Reading font information for font ~S scaled ~D ..."
          fontname scale-factor)
        (store-file-length fbuffer (file-stream-length tfmstr));store file-length
        (when (< (array-length fbuffer) (file-length fbuffer))
          (array-grow fbuffer (file-length fbuffer)))
        (send tfmstr :string-in nil fbuffer 0 (file-length fbuffer)))

      (setf (fill-pointer fbuffer) 0);make fill pointer point to beginning of file.
      (setq lf (bget-2-bytes fbuffer))
      (setq lh (bget-2-bytes fbuffer))
      (setq bc (bget-2-bytes fbuffer))
      (setq ec (bget-2-bytes fbuffer))
      (setq nw (bget-2-bytes fbuffer))
      (incf (fill-pointer fbuffer) 14) ;point to beginning of header data
      (if (< lh 17)(bad-dvi "Bad TFM file"))
      (if ( checksum (bsigned-quad fbuffer))
          (format t  "  Warning - checksums don't match. "))
      (incf (fill-pointer fbuffer) 44) ;ignore design size and coding information
      (let ((font-fam  (make-array 20 :type art-string)));read in BCPL font family name.
        (dotimes (i 20)(aset (bget-byte fbuffer) font-fam i))
        (setf (tfd-family-name tfm-fdef) font-fam))
      (cond ((> lh 17)
             (incf (fill-pointer fbuffer) 3)
             (let ((code (bget-byte fbuffer)))
;              (if (< code 18)
                   (setf (tfd-face-code tfm-fdef) code))))
      ;point to beginning of char info table
      (setf (fill-pointer fbuffer)(* 4 (+ 6 lh)))
      (let ((ec-bc+1 (1+ (- ec bc))))
        (setq width-indices (make-array ec-bc+1 :type 'art-8b))
        (dotimes (i ec-bc+1)
          (aset (bget-byte fbuffer) width-indices i)
          (incf (fill-pointer fbuffer) 3)))

      (setq width-table (make-array nw))
      (dotimes (i nw)
        (aset (bsigned-quad fbuffer) width-table i))
      (setf (tfd-dvi-widths tfm-fdef)
            (decode-widths bc ec width-indices width-table scale-factor))
      (format t "Done.")
      tfm-fdef)))

(defun decode-widths (bc ec width-indices width-table scale-factor)
  ;;
  ;;takes the char width indices and that char width table of
  ;;the internal fontdef and returns an array of 0..ec of character widths.
  ;;A width of -1 indicates a non-existent character.
  ;;
  (let ((dvi-widths (make-array (1+ ec) :initial-value -1)))
    (dotimes (i (array-length width-indices))
      (let ((tfm-width (aref width-table (aref width-indices i))));in units of fixes
        (aset (// (* scale-factor tfm-width)
                  (dfloat TwoToTheTwenty))
              dvi-widths
              (+ bc i))))
    dvi-widths))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.2 The document flavor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defflavor document
        ((pixel-h 0) ;h position of pen in pixels
         (pixel-v 0) ;v position of pen in pixels
         (h 0)       ;h position of pen in DVI units
         (v 0)       ;v position of pen in DVI units
         (w 0)       ;w, x, y, z parameters of a dvi file
         (x 0)
         (y 0)
         (z 0)
         page-length ;height plus depth of tallest page in DVI units
         page-width  ; width of widest page in DVI units
         (xpage-offset 0) ; init position of pen after bop
         (ypage-offset 0) ; init position of pen after bop
         file-buffer  ;buffer for contents of dvi file
         outfile      ;pathname for the output file
         buffer       ;buffer for output, holds press stream for press files
         hstack       ;the stack containing the h,v,w,x,y,z values
         vstack
         wstack
         xstack
         ystack
         zstack
         pxl-hstack
         pxl-vstack
         (current-font "Undefined");the current font used
         fontmap      ;maps printer font numbers to font hash table keys
         fontnums     ;maps printer font numbers to TeX font numbers.
         job-id        ;the id of the current job
         printer-name
         printer-resolution
         printer-max-fonts) ;the printing device
        ()
  :gettable-instance-variables
  :settable-instance-variables
  :initable-instance-variables)

(defsubst make-stack (size)
  (make-array size :fill-pointer 0))

(defsubst push-stack (stack value)
  (array-push stack value))

(defsubst pop-stack (stack)
  (array-pop stack))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.2 a .  Method :define-font.
;;          First, figure out the file name of the font, then
;;          look at the *font-definitions* table to see if the
;;          font definition already exists.  If not, define the
;;          font by reading the pxl file and then put the fontdef
;;          into the hash table.  Update the fontmap which maps
;;          between tex font nos. and their filenames and fontnums
;;          which maps between tex font nos. and printer font nos.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (document :define-font)(opcode)
  (let* ((texfntnum (select opcode
                     (fntdef1 (bget-byte file-buffer))
                     (fntdef2 (bget-2-bytes file-buffer))
                     (fntdef3 (bget-3-bytes file-buffer))
                     (fntdef4 (bsigned-quad file-buffer))
                     (t (bad-dvi "Not font def command ~S" opcode))))
         (checksum (bsigned-quad file-buffer))
         (scale (bsigned-quad file-buffer))
         (design (bsigned-quad file-buffer))
         (fname-len (+ (bget-byte file-buffer)(bget-byte file-buffer)));;length of filename
         (fname (make-array fname-len :type art-string)))
    (if ( printer-max-fonts (fill-pointer fontmap))
       (bad-dvi "Too many fonts. Maximum is ~S" printer-max-fonts))
    ;;read in the font external name
    (dotimes (i (array-length fname))(aset (bget-byte file-buffer) fname i))
    ;;set fname to name with proper XXXXpxl extension
    ;; fname also becomes the key with which to store the font def.
    (setq fname (get-pxl-fname printer-resolution fname scale design))
    (array-push-extend fontmap fname)
    (array-push-extend fontnums texfntnum)
    (cond ((null (send  *font-definitions* :get-hash fname))
           (send *font-definitions* :put-hash fname
                 (read-pxlfile fname checksum scale design))))
    (alter-fontdef (send *font-definitions* :get-hash fname) ;reset char statuses.(statii?)
          char-status (make-array chars-per-font :type art-1b))))

(defmethod (document :define-press-font)(opcode)
  (let* ((texfntnum (select opcode
                     (fntdef1 (bget-byte file-buffer))
                     (fntdef2 (bget-2-bytes file-buffer))
                     (fntdef3 (bget-3-bytes file-buffer))
                     (fntdef4 (bsigned-quad file-buffer))
                     (t (bad-dvi "Not font def command ~S" opcode))))
         (checksum (bsigned-quad file-buffer))
         (scale (bsigned-quad file-buffer))
         (design (bsigned-quad file-buffer))
         (fname-len (+ (bget-byte file-buffer)(bget-byte file-buffer)));;length of filename
         (fname (make-array fname-len :type art-string))
         hashname)
    (if ( printer-max-fonts (fill-pointer fontmap))
       (bad-dvi  "Too many fonts. Maximum is ~S" printer-max-fonts))
    ;;read in the font external name
    (dotimes (i (array-length fname))(aset (bget-byte file-buffer) fname i))
    (setq hashname (format nil "~Ascaled~D" fname scale))
    (array-push-extend fontmap hashname)
    (array-push-extend fontnums texfntnum)
    (cond ((null (send  *font-definitions* :get-hash hashname))
           (send *font-definitions* :put-hash hashname
                 (read-tfmfile fname checksum scale design))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1.2 b.  Manipulating the stacks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (document :push-stack)(&optional ignore)
  (push-stack hstack h)
  (push-stack vstack v)
  (push-stack wstack w)
  (push-stack xstack x)
  (push-stack ystack y)
  (push-stack zstack z)
  (push-stack pxl-hstack pixel-h)
  (push-stack pxl-vstack pixel-v))

(defmethod (document :pop-stack)(&optional ignore)
  (setq h (pop-stack hstack))
  (setq v (pop-stack vstack))
  (setq w (pop-stack wstack))
  (setq x (pop-stack xstack))
  (setq y (pop-stack ystack))
  (setq z (pop-stack zstack))
  (setq pixel-h (pop-stack pxl-hstack))
  (setq pixel-v (pop-stack pxl-vstack)))

(defmethod (document :empty-stack)()
  (setf (fill-pointer hstack) 0)
  (setf (fill-pointer vstack) 0)
  (setf (fill-pointer wstack) 0)
  (setf (fill-pointer xstack) 0)
  (setf (fill-pointer ystack) 0)
  (setf (fill-pointer zstack) 0)
  (setf (fill-pointer pxl-hstack) 0)
  (setf (fill-pointer pxl-vstack) 0))


(defmethod (document :xxx)(bytes)
  ;; by default skip over \special.
  (incf (fill-pointer file-buffer) bytes))

(defmethod (document :nop)(ignore)
  ;do nothing
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.  This is the portion that provides an interface for
;;       extension of this dvi-to-xx program for any printer.
;;
;;     Looking at the format of the dvi file, the main printer
;; dependent parts of the commands are the commands that appear
;; between bops and eops, and font definitions.  Font definitions are
;; handled while processing the postamble.  The remaining printer
;; dependent commands can be divided into these generic groups :
;; (1) set-char
;; (2) set-rule
;; (3) put-char
;; (4) put-rule
;; (5) nop
;; (6) push
;; (7) pop
;; (8) right
;; (9) w
;; (10)x
;; (11)down
;; (12)y
;; (13)z
;; (14)set-fnt
;; (15)xxx      - generated by \special comand in TeX
;; (16)fnt-def  -do nothing if already defined.
;; (17)bop
;; (18)eop


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is the procedure that gets the correct message name for sending to the
;; document, given the dvi opcode and the name of the printer.  Therefore, the
;; message name is :<printername>-<translation-array[opcode]>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defsubst get-message-name (opcode)
  ;; the table is an array
 (aref translation-array opcode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Given the opcode, this extracts the parameters from the
;;input stream.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro memaq (a &rest l)
  `(or ,@(mapcar #'(lambda (x) `(eq ,a ,x)) l)))


(defun get-parameters (opcode file-buffer)
  ;; assumes that the opcodes that appear here are those that can
  ;; appear betwen a bop and an eop
  (cond ((< opcode set1) opcode)
        ((memaq opcode nop dvi-push dvi-pop w0 x0 y0 z0 eop) ())
        ((memaq opcode right1 w1 x1 down1 y1 z1)
         (bget-signed-byte file-buffer))
        ((memaq opcode right2 w2 x2 down2 y2 z2)
         (bsigned-pair file-buffer))
        ((memaq opcode right3 w3 x3 down3 y3 z3)
         (bsigned-trio file-buffer))
        ((memaq opcode set4 put4 right4 w4 x4 down4 y4 z4 fnt4 xxx4)
         (bsigned-quad file-buffer))
        ((memaq opcode set1 put1 fnt1 xxx1)
         (bget-byte file-buffer))
        ((or (= opcode set-rule)(= opcode put-rule))
         (cons (bsigned-quad file-buffer)(bsigned-quad file-buffer)))
        ((< opcode fnt1) ;fntnum  opcode 171 to 238
         (- opcode fntnum0))
        (( opcode fntdef4) ;fnt def, just get rid of params,
         ;can be optimized.
         (incf (fill-pointer file-buffer)(+ (1+ (- opcode fntdef1)) 12))
         (let ((bytes (+ (bget-byte file-buffer)(bget-byte file-buffer))))
           (incf (fill-pointer file-buffer) bytes))
         nil)
        ((memaq opcode set2 put2 fnt2 xxx2)
         (bget-2-bytes file-buffer))
        ((memaq opcode set3 put3 fnt3 xxx3)
         (bget-3-bytes file-buffer))
        (t (bad-dvi "Undefined opcode between bop and eop"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Processing the preamble
; this function reads the information in the preamble and stores them
; for later use
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun process-preamble (doc file-buffer)
  (if (neq (bget-byte file-buffer) pre)
      (bad-dvi "DVI file doesn't start with preamble"))
  (if (neq (bget-byte file-buffer) 2)
      (bad-dvi "Wrong version of DVI for this program"))
  (setq numerator (bsigned-quad file-buffer))
  (setq denominator (bsigned-quad file-buffer))
  (setq magnification (bsigned-quad file-buffer))
  (setq dvi2mica  (// (dfloat (* magnification numerator))
                      (* 100000 denominator)))
  (setq conv  (// (dfloat (* numerator magnification (send doc :printer-resolution)))
                  (dfloat (* denominator 1000))
                  100000 2.54))
  (setq dvis-per-fix (// (dfloat (* denominator 2.54 100000))
                         (* numerator TwoToTheTwenty 72.27)))
  (setq pixels-per-page (* 11 (send doc :printer-resolution)))
  (setq pixels-per-line (* 8 (send doc :printer-resolution)))
  ;;read the job id
  (let* ((length (bget-byte file-buffer))
         (id (make-array length :type 'art-string)))
    (dotimes (i length)(aset (bget-byte file-buffer) id i))
    (send doc :set-job-id id)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Searching for the postamble byte
;;
;; input-stream is the stream connected to the DVI file
;; this function searches for the postamble from the back of the file
;; and leaves the pointer at the byte after the post command
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun find-the-postamble (file-buffer)
  ;;set the pointer to the end of the file
  (setf (fill-pointer file-buffer)(1- (file-length file-buffer)))
    (loop until (or (neq (bpeek-byte file-buffer) 223)
                     (zerop (fill-pointer file-buffer)))
          do  (decf (fill-pointer file-buffer)))
    (if (zerop (fill-pointer file-buffer))(bad-dvi "File is all 223s"))
    (if ( (bpeek-byte file-buffer) 2)(bad-dvi "Wrong version of dvi"))
    (decf (fill-pointer file-buffer) 4)
    (setf (fill-pointer file-buffer) (bsigned-quad file-buffer))
    (if (neq (bget-byte file-buffer) post)(bad-dvi "post byte is not in position"))
      ;;
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Processing the postamble
;; The pointer of the input stream must be pointing to the byte
;; FOLLOWING the post command.
;; The information in the postamble is read and stored into global
;; variables.
;; It also reads and processes the font definitions found in the
;; postamble.
;; It leaves the file pointer pointing to the beginning of the last page of
;; output.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun process-postamble (doc file-buffer)
  (let ((last-page (bsigned-quad file-buffer)))
    (if (or (not (= numerator (bsigned-quad file-buffer)))
            (not (= denominator (bsigned-quad file-buffer)))
            (not (= magnification (bsigned-quad file-buffer))))
       (bad-dvi "Postamble doesn't match preamble"))
    (send doc :set-page-length (bsigned-quad file-buffer))
    (send doc :set-page-width (bsigned-quad file-buffer))
    (and center-text
         (if (string= (send doc :printer-name) "DOVER")
             (progn (send doc :set-ypage-offset ;in dvi units
                          (// (- (// (send (send doc :buffer) :height) dvi2mica)
                                 (send doc :page-length)) 2))
                    (send doc :set-xpage-offset ;in dvi units
                          (// (- (// (send (send doc :buffer) :width) dvi2mica)
                                 (send doc :page-width)) 2)))
             (send doc :set-ypage-offset ;in pixels
                   (// (- pixels-per-page (pixel-round (send doc :page-length))) 2))
             (send doc :set-xpage-offset ;in pixels
                   (// (- pixels-per-line (pixel-round (send doc :page-width))) 2))))
    (setq max-stack (bget-2-bytes file-buffer))
    (send doc :set-hstack (make-stack max-stack))
    (send doc :set-vstack (make-stack max-stack))
    (send doc :set-wstack (make-stack max-stack))
    (send doc :set-xstack (make-stack max-stack))
    (send doc :set-ystack (make-stack max-stack))
    (send doc :set-zstack (make-stack max-stack))
    (send doc :set-pxl-hstack (make-stack max-stack))
    (send doc :set-pxl-vstack (make-stack max-stack))
    (setq total-pages (bget-2-bytes file-buffer))
      ;;define the fonts until postpost byte encountered
      (loop for command = (bget-byte file-buffer) do
            (select command
              ((fntdef1 fntdef2 fntdef3 fntdef4)
               (if (string= (send doc :printer-name) "DOVER")
                   (send doc :define-press-font command)
                   (send doc :define-font command)))
              (post-post (return))
              (otherwise (bad-dvi "No font definition found in postamble"))))
    (setf (fill-pointer file-buffer) last-page)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Starting the output, doing the pages, and finishing the output are all
;; printer dependent operations.  For example, in doing the pages, some printers
;; want the pages to be output backwards instead of forwards.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsubst start-output (doc)
  (send doc (get-message-name begin-output)))

(defsubst finish-output (doc)
  (send doc (get-message-name end-output)))

(defsubst do-pages (doc)
  (send doc (get-message-name do-pages)))

(defprop :imagen imagen-dvi-document dvi-flavor)
(defprop :screen screen-dvi-document dvi-flavor)
(defprop :dover dover-dvi-document dvi-flavor)

(defun run-dvi (inpathname outfilename device &key (margin 1))
  margin
  (let ((doc (make-instance (or (get (intern (string-upcase device) "") 'dvi-flavor)
                                (bad-dvi "unknown dvi device type: ~S" device))))
        (abortp t))
    (using-resource (fbuffer file-buffer)
      (with-open-stream (istr (open-8b-input inpathname))
        (store-file-length fbuffer (file-stream-length istr))
        (format t "~&Processing ~S, ~D bytes..."
                (send (send istr :truename) :string-for-printing)
                (file-length fbuffer))
        (if (not (zerop (\ (file-length fbuffer) 4)))
            (bad-dvi "File length not multiple of 4")
          (when (< (array-length fbuffer) (file-length fbuffer))
            (array-grow fbuffer (file-length fbuffer)))
          (send istr :string-in nil fbuffer 0 (file-length fbuffer))))
      (setf (fill-pointer fbuffer) 0)
      (unwind-protect
          (progn (send doc :set-file-buffer fbuffer)
                 (send doc :set-outfile outfilename)
                 (send doc :initialize)
                 (process-preamble doc fbuffer)
                 ;; position the pointer in the file stream
                 ;; to the first byte of the
                 ;; postamble.
                 (find-the-postamble fbuffer)
                 ;; process the postamble and leave the
                 (process-postamble doc fbuffer)
                 ;; file
                 ;; pointer at the beginning of the last page.
                 (start-output doc)             ; maybe output font definitions
                 (describe-settings doc)
                 (do-pages doc)
                 (finish-output doc)
                 (send doc :normal-ending)
                 (setq abortp nil)
                 (send doc :outfile))
        (if abortp
            (send doc :abnormal-ending))))))


(defun describe-settings (doc)
  (format t "~&Job ~S for printer ~S with ~D pixels per inch~%"
          (send doc :job-id) (send doc :printer-name)
          (send doc :printer-resolution))
  (format t "Available Page Width = ~D pixels, ~S inches~%"
          pixels-per-line (quotient pixels-per-line (float (send doc :printer-resolution))))

  (format t "Available Page Length = ~D pixels, ~S inches~%"
          pixels-per-page (quotient pixels-per-page (float (send doc :printer-resolution))))

  (format t "Page-Width = ~D dvi's ~D pixels ~S inches~%"
          (send doc :PAGE-WIDTH)
          (pixel-round (send doc :PAGE-WIDTH))
          (quotient (pixel-round (send doc :PAGE-WIDTH))
                    (float (send doc :PRINTER-RESOLUTION))))

  (format t "Page-Length = ~D dvi's ~D pixels ~S inches~%"
          (send doc :PAGE-LENGTH)
          (pixel-round (send doc :PAGE-LENGTH))
          (quotient (pixel-round (send doc :PAGE-LENGTH))
                    (float (send doc :PRINTER-RESOLUTION))))

  (format t "XPAGE-OFFSET = ~D pixels, ~S inches~%"
          (send doc :xpage-offset)
          (quotient (send doc :xpage-offset) (float (send doc :PRINTER-RESOLUTION))))
  (format t "YPAGE-OFFSET = ~D pixels, ~S inches~%"
          (send doc :Ypage-offset)
          (quotient (send doc :Ypage-offset) (float (send doc :PRINTER-RESOLUTION)))))


(defun initialize-translation-array ()
  (setq translation-array (make-array 250 :area permanent-storage-area))
  (dotimes (i (1+ set4))(aset :SET-CHAR translation-array i))
  (aset :SET-RULE translation-array set-rule)
  (loop for i from put1 to put4 do
    (aset :PUT-CHAR translation-array i))
  (aset :PUT-RULE translation-array put-rule)
  (aset :NOP translation-array nop)
  (aset :BOP translation-array bop)
  (aset :EOP translation-array eop)
  (aset :PUSH-STACK translation-array dvi-push)
  (aset :POP-STACK translation-array dvi-pop)
  (loop for i from right1 to right4 do
    (aset :RIGHT translation-array i))
  (loop for i from w0 to w4 do
    (aset :W translation-array i))
  (loop for i from x0 to x4 do
    (aset :X translation-array i))
  (loop for i from down1 to down4 do
    (aset :DOWN translation-array i))
  (loop for i from y0 to y4 do
    (aset :Y translation-array i))
  (loop for i from z0 to z4 do
    (aset :Z translation-array i))
  (loop for i from fntnum0 to fnt4 do
    (aset :SET-FNT translation-array i))
  (loop for i from xxx1 to xxx4 do
    (aset :XXX translation-array i))
  (loop for i from fntdef1 to fntdef4 do
    (aset :NOP translation-array i))
  (aset :START-OUTPUT translation-array begin-output)
  (aset :END-OUTPUT translation-array end-output)
  (aset :DO-PAGES translation-array do-pages)
  ())


(initialize-translation-array)
