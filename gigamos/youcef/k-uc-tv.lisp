;;;-*- Mode:LISP; Package:LISP-INTERNALS; Base:10; Readtable:CL -*-
;;;
;;; Written by Youcef Bennour.
;;;
;;;  Files contains the equivalents of contents of file UC-TV.
;;;  Select-sheet
;;;  Draw-char
;;;  Draw-rectangle
;;;  Draw-line
;;;  Bitblt
;;;
;;; Buffer stuff
;;;
(defparameter *A-TV-CURRENT-SHEET* nil)
(defparameter *A-TV-SCREEN-BUFFER-ADDRESS* nil)
(defparameter *A-TV-SCREEN-BUFFER-END-ADDRESS* nil)
(defparameter *A-TV-SCREEN-LOCATIONS-PER-LINE* nil)
(defparameter *A-TV-SCREEN-BUFFER-BIT-OFFSET* nil)
(defparameter *A-TV-SCREEN-WIDTH* nil)
(defparameter *A-TV-SCREEN-BUFFER-PIXEL-MROT* nil)
;;;
;;;  FONT STUFF
;;;
(defparameter *A-FONT-POINTER* nil)
(defparameter *A-FONT-ORIGIN* nil)
(defparameter *A-Font-raster-width* nil)
(defparameter *A-font-raster-height* nil)
(defparameter *a-font-raster-shift* nil)
(defparameter *a-font-rows-per-word* nil)
(defparameter *A-font-words-per-char* nil)

;;;
;;; Validate Sheet Cache
;;;
(defun Validate-Sheet-Cache (sheet)
  ;; Sequence Break is inhibited. Function checks to see if the sheet is the
  ;; same as *A-TV-CURRENT-SHEET*. If it is not, updates the sheet global variables.
  )

;;;
;;; Select-sheet
;;;
(defun select-sheet (sheet)
  (multiple-value-bind (bar array-length foo array-data)
      ;; foo and bar are dummy variables.
      (array:decode-array *screen-address*)
    (values 32.                                 ; number of words per line
            0                                   ; word offset
            array-data                          ; beginning of screen memory
            (hw:24+ array-length array-data)    ; ending address
            0)                                  ; weird pixel depth (log(pixel-size))
    )
  )

;;;
;;; Validate Font Cache
;;;

(defun Validate-Font-Cache (font-pointer)
  ;; Sequence Break is inhibited. If *A-FONT-POINTER* is different from font-pointer
  ;; Global font are then updated
  )

;;;
;;;    DISPATCH TABLE FOR DPB INTO 32 BIT WORDS USING AN ALU OPERATION.
;;;


(defafun dpb-alu-dispatch-table (value word)
  ;;; byte spec is already loaded into the status reg and ready for operation.
  (alu field-pass return gr:*all-zero* a1 pw-rr ch-return next-pc-return unboxed)       ; boole-clr=0
boole-and
  (alu field-AND return a0 a1 pw-rr ch-return next-pc-return unboxed)                   ; boole-and=1
  (unconditional-branch boole-and (alu field-not a0 a0 a0 pw-rr unboxed))               ; boole-andc1=2
  (alu field-pass return a1 a1 pw-rr ch-return next-pc-return unboxed)                  ; boole-2=3
  (unconditional-branch boole-and (alu field-not a1 a1 a1 pw-rr unboxed))               ; boole-andc2=4
  (alu field-pass return a0 a1 pw-rr ch-return next-pc-return unboxed)                  ; boole-1=5
  (alu field-XOR return a0 a1 pw-rr ch-return next-pc-return unboxed)                   ; boole-xor=6
Boole-OR
  (alu field-OR return a0 a1 pw-rr ch-return next-pc-return unboxed)                    ; boole-ior=7
  (unconditional-branch boole-not (alu field-OR a2 a0 a1 pw-rr unboxed))                ; boole-nor=8
  (unconditional-branch boole-not (alu field-XOR a2 a0 a1 pw-rr unboxed))               ; boole-EQV=9
  (alu field-not return a0 a0 pw-rr ch-return next-pc-return unboxed)                   ; boole-C1=10
  (unconditional-branch boole-or (alu field-not a0 a0 a0 pw-rr unboxed))                ; boole-ORC1=11
  (alu field-not return a1 a1 pw-rr ch-return next-pc-return unboxed)                   ; boole-C2=12
  (unconditional-branch boole-or (alu field-not a1 a1 a1 pw-rr unboxed))                ; boole-ORC2=13
  (unconditional-branch boole-not (alu field-AND a2 a0 a1 pw-rr unboxed))               ; boole-nor=14
  (alu field-pass return gr:*all-ones* a1 pw-rr ch-return next-pc-return unboxed)       ; boole-SET=15
boole-not
  (alu field-not return a2 a2 pw-rr ch-return next-pc-return unboxed)                   ; boole-not?
  )

(defafun dpb-unboxed-with-aluf (value byte-spec word aluf)
  (movea a4 (dpb-alu-dispatch-table 2) boxed)
  (move O0 a0 CH-TAIL-OPEN)
  (alu L+R nop a3 A4 boxed)
  (alu load-status-r nop a1 a1 bw-16 unboxed)
  (move o1 a2 ch-tail-call next-pc-dispatch)
  )

;;;************************************************************************************
;;;*                                                                                  *
;;;*               Char Drawing Routines.                                             *
;;;*                                                                                  *
;;;************************************************************************************

(defun draw-bit-patterns (tv-screen-buffer-address offset tv-bit-offset field-width bit-position word alu-function)
;  (error "In draw-bit-patterns")
  (Array:%VM-WRITE32
    tv-screen-buffer-address
    offset
    (dpb-unboxed-with-aluf
      (hw:ldb word (byte field-width bit-position) (hw:unboxed-constant 0))
      (byte field-width tv-bit-offset)
      (Array:%VM-READ32 tv-screen-buffer-address offset)
      alu-function))
  )


(defun draw-row-crossing-word-boundary (row-data
                                        raster-char-width
                                        tv-screen-buffer-address
                                        offset
                                        tv-bit-offset
                                        alu-function &aux (width-1 (- 32. tv-bit-offset)))
  "font-word fits in window but does not fit in a single word starting at tv-bit-offset. We need
two words of buffer to do it."
  (setq raster-char-width (- raster-char-width width-1))
  ;; first portion of current row
  (draw-bit-patterns tv-screen-buffer-address offset tv-bit-offset width-1 raster-char-width row-data alu-function)
  ;; second portion of current row
  (draw-bit-patterns tv-screen-buffer-address (1+ offset) 0 raster-char-width 0 row-data alu-function)
  )

(defun draw-char-no-check (raster-char-width
                           raster-char-height
                           tv-screen-buffer-address
                           tv-bit-offset
                           tv-screen-locations-per-line
                           font-word-address
                           alu-function)
    "Fast char drawing. At this point character has been found to fit in window to draw in,
row definition of character bit map have been found to fit in the word at tv-screen-buffer-address
starting at tv-bit-offset-position."
  (do ((row 0 (1+ row))
       (offset 0 (+ offset tv-screen-locations-per-line))
       (font-word-offset 0)
       (position 32. (+ position raster-char-width))
       ;; position is initialized to 32. to make it read the first word from the font array
       ;; in which case position is set to 0.
       font-word)
      ((= row raster-char-height))
    (when (> position 31.)
      (setq font-word (array:%VM-READ32 font-word-address font-word-offset))
      (setq position 0)
      (setq font-word-offset (1+ font-word-offset)))
    (draw-bit-patterns tv-screen-buffer-address offset tv-bit-offset raster-char-width position font-word alu-function)
    )
  )

(defun draw-char-in-two-words (raster-char-width
                               raster-char-height
                               tv-screen-buffer-address
                               tv-bit-offset
                               tv-screen-locations-per-line
                               font-word-address
                               alu-function)
  "Character fits in window but does not fit in a single word starting at tv-bit-offset. We need
two words of buffer to do it."
  (do ((row 0 (1+ row))
       (offset 0 (+ offset tv-screen-locations-per-line))
       (font-word-offset 0)
       (position 32. (+ position raster-char-width))
       font-word)
      ((= row raster-char-height))
    (and (> position 31.)
         (setq font-word (array:%VM-READ32 font-word-address font-word-offset)
               position 0
               font-word-offset (1+ font-word-offset)))
    (draw-row-crossing-word-boundary
      font-word
      raster-char-width
      tv-screen-buffer-address
      offset
      tv-bit-offset
      alu-function)
    )
  )


(defun draw-char-clipping (raster-char-width
                           remaining-height-to-draw
                           tv-screen-buffer-address
                           tv-bit-offset
                           tv-screen-locations-per-line
                           font-word-address
                           byte-spec
                           alu-function
                           &aux (size-to-draw (byte-size byte-spec)))
  "Character drawing with clipping relative to a given window. The clipping parameters must have been
already computed. remaining-height-to-draw is the number of rows that has to be drawn. font-word-address
is the first font word to draw (this take care of top and bottom clipping). Byte-spec specifies which bits
in a row word is getting drawn."
  (do ((row 0 (1+ row))
       (offset 0 (+ offset tv-screen-locations-per-line))
       (font-word-offset 0)
       (crosses-word-boundaryp (> (+ tv-bit-offset size-to-draw) 31.))
       (position 32. (+ position raster-char-width))
       font-word)
      ((= row remaining-height-to-draw))
    (and (> position 31.)
         (setq font-word (array:%VM-READ32 font-word-address font-word-offset)
               position 0
               font-word-offset (1+ font-word-offset)))
    (if crosses-word-boundaryp
        (draw-row-crossing-word-boundary
          (lisp:ldb byte-spec (lisp:ldb (byte raster-char-width position) font-word))
          size-to-draw
          tv-screen-buffer-address
          offset
          tv-bit-offset
          alu-function)
      (draw-bit-patterns
        tv-screen-buffer-address
        offset tv-bit-offset
        size-to-draw
        position
        font-word
        alu-function))
    )
  )

;;;
;;;        Top Level Routines to set char-drawing Parameters.
;;;

(defun clip-char-relative-to-window (window-x
                                     window-y
                                     window-width
                                     window-height
                                     char-x
                                     char-y
                                     raster-char-width
                                     raster-char-height)
  ;; if (or (char-x + raster-char-width < window-x)
  ;;        (char-x > window-width)
  ;;        (char-y + raster-char-height < window-y)
  ;;        (char-y > window-y + window-height)) then
  ;;       Completly-clipped = t
  ;;       return
  ;; else
  ;;   if char-x is negative then
  ;;         bit-position = 0
  ;;         width-to-draw = (+ char-width char-x)
  ;;         Char-x = 0
  ;;
  ;;   if char-x + width-to-draw > window-width then
  ;;         bit-position = (- width-to-draw (setq width-to-draw (- window-width char-x)))
  ;;
  ;;   if char-y is negative then
  ;;         height-to-draw = (+ char-height char-y)
  ;;         offset-row = char-y
  ;;         char-y = 0
  ;;
  ;;   if char-y + height-to-draw > window-height
  ;;        height-to-draw = (- window-height char-y)
  ;;
  (let ((completly-clipped nil)
        (not-clipped nil)
        (offset-row 0)
        (bit-position 0)
        (width-to-draw raster-char-width)
        (height-to-draw raster-char-height))
    (if (or (< (+ char-x raster-char-width) window-x)
            (> char-x window-width)
            (< (+ char-y raster-char-height) window-y)
            (> char-y (+ window-y window-height)))
        (setq completly-clipped T)
      ;; may be no clipped at all.
      (if (or (>= char-x 0)
              (>= char-y 0)
              (<= (+ char-x raster-char-width) window-width)
              (<= (+ char-y raster-char-height) window-height))
          (setq not-clipped t)
        ;; check for Left side clip
        (and (minusp char-x)
             (setq width-to-draw (+ char-x raster-char-width)
                   char-x 0))
        ;; check for right side clip
        (and (> (+ char-x width-to-draw) window-width)
             (setq bit-position (- width-to-draw (setq width-to-draw (- window-width char-x)))))
        ;; check for top side clip
        (and (minusp char-y)
             (setq offset-row (- char-y)
                   height-to-draw (+ raster-char-height char-y)
                   char-y 0))
        ;; check for bottom side clip
        (and (> (+ char-y height-to-draw) window-height)
             (setq height-to-draw (- window-height char-y)))))
    (values char-x char-y not-clipped completly-clipped offset-row bit-position width-to-draw height-to-draw)
    )
  )

;;;
;;;  Draw Rectangle routine
;;;

(defconstant *q-pointer-width* 31.)

(defun tv-xy-address (x-bitpos y-bitpos
                      tv-screen-locations-per-line
                      tv-screen-buffer-bit-offset
                      tv-screen-buffer-address
                      tv-screen-buffer-pixel-size-mrot)
;  (error "in tv-xy-address")
  (let ((line-address-offset (new-math:multiply-fixnum y-bitpos tv-screen-locations-per-line))
        (bit-offset-in-line (hw:24+ tv-screen-buffer-bit-offset
                                    (hw:dpb-boxed x-bitpos (byte (- 24. tv-screen-buffer-pixel-size-mrot)
                                                                 tv-screen-buffer-pixel-size-mrot) 0))))
       (values (hw:24+ (hw:24+ line-address-offset
                          (hw:ldb-boxed bit-offset-in-line (byte (- 24. 5) 5) 0))
                  tv-screen-buffer-address)
               (hw:ldb-boxed bit-offset-in-line (byte 5 0) 0)))
  )

(defun draw-column (width height word-adr bit-offset
                    tv-screen-buffer-address
                    tv-screen-buffer-end-address
                    tv-screen-locations-per-line
                    alu-function)
;  (error "In draw-column")
  (do* ((row 0 (hw:24+ 1 row))
        (offset 0 (hw:24+ offset tv-screen-locations-per-line))
        (word-address word-adr (hw:24+ offset word-adr)))
       ((or (hw:32< word-address tv-screen-buffer-address)
            (hw:32> word-address tv-screen-buffer-end-address)
            (= row height)))
    ;;
    ;; this will read a word from the screen buffer, no matter what the alu function is.
    ;; should improved it to discard the read if whole word is to be mugged up.
    ;; but will do for now.
    ;;
    (draw-bit-patterns word-address 0 bit-offset width bit-offset gr:*all-ones* alu-function)
    )
  )


(defun draw-rectangle-internal (width                           ; width in bits
                                height                          ; height
                                word-adr                        ; screen address where to start drawing
                                bit-offset                      ; bit offset in word
                                tv-screen-buffer-address        ; low address bound of screen
                                tv-screen-buffer-end-address    ; high address bound of screen
                                tv-screen-locations-per-line    ; number of word per scan line
                                alu-function)                   ; alu function to use
;  (error "In draw-rectangle-internal")
  (do* ((column 0 (1+ column))
        (remaining-width width (- remaining-width pattern-width))
        (pattern-width (min (hw:24- (hw:24+ 1 *q-pointer-width*) bit-offset) width)
                       (min (hw:24+ 1 *q-pointer-width*) remaining-width))
        (bit-off bit-offset 0)
        ;; width in word must be (div width 32) + 1 if the remainder is not zero.
        (width-in-word (hw:24+ (min (hw:ldb-boxed width (byte 5 0) 0) 1)
                               (hw:ldb-boxed width (byte (- 24. 5.) 5.) 0))))
       ((or (>= column width-in-word)
            (hw:32> word-adr tv-screen-buffer-end-address)))
    (draw-column pattern-width height word-adr bit-off
                 tv-screen-buffer-address
                 tv-screen-buffer-end-address
                 tv-screen-locations-per-line
                 alu-function)
    (setq word-adr (hw:24+ 1 word-adr))
    )
  )


(defun draw-rectangle (width height x-bitpos y-bitpos alu-function sheet)
  (li:%trap-if-not-both-fixnum width height)
  (li:%trap-if-not-both-fixnum x-bitpos y-bitpos)
  (li:%trap-if-not-both-fixnum alu-function alu-function)
  (let (word-adr
        bit-offset
        tv-screen-locations-per-line
        tv-screen-buffer-bit-offset
        tv-screen-buffer-address
        tv-screen-buffer-end-address
        tv-screen-buffer-pixel-size-mrot
        )
    ;; should turn off sequence breaks in select sheet.
    (setq gr:*allow-sequence-break* (1+ gr:*allow-sequence-break*))
    (multiple-value-setq (tv-screen-locations-per-line
                          tv-screen-buffer-bit-offset
                          tv-screen-buffer-address
                          tv-screen-buffer-end-address
                          tv-screen-buffer-pixel-size-mrot)
      (select-sheet sheet))
    (setq gr:*allow-sequence-break* (1- gr:*allow-sequence-break*))
    (multiple-value-setq (word-adr bit-offset)
      (tv-xy-address x-bitpos y-bitpos
                     tv-screen-locations-per-line
                     tv-screen-buffer-bit-offset
                     tv-screen-buffer-address
                     tv-screen-buffer-pixel-size-mrot))
    ;; compute width in bits. Bug tv-screen-buffer-pixel-size-mrot is the log of pixel size
    (setq width (new-math:multiply-fixnum width (ash 1 tv-screen-buffer-pixel-size-mrot)))
    (if (or (zerop width)
            (zerop height))
        nil
      (if (> (+ bit-offset width) (hw:24+ 1 *q-pointer-width*))
          (draw-rectangle-internal
            width
            height
            word-adr
            bit-offset
            tv-screen-buffer-address
            tv-screen-buffer-end-address
            tv-screen-locations-per-line
            alu-function)
        (draw-column width
                     height
                     word-adr
                     bit-offset
                     tv-screen-buffer-address
                     tv-screen-buffer-end-address
                     tv-screen-locations-per-line
                     alu-function)
        ))
    )
  )


;;;;
;;;;  DRAW LINE
;;;;


(defun increment-x (adr bit-offset pixel-size)
  (setq bit-offset (+ bit-offset pixel-size))
  (when (> bit-offset *q-pointer-width*)
    (setq adr (hw:24+ 1 adr))
    (setq bit-offset 0))
  (values adr bit-offset)
  )

(defun increment-cell-location (adr bit-offset pixel-size
                                direction long-side long-side/2
                                delta-x delta-y tv-screen-locations-per-line)
  (setq long-side/2 (- long-side/2 delta-y))
  (cond ((minusp long-side/2)
         (setq adr (hw:24+ tv-screen-locations-per-line adr)
               long-side/2 (+ long-side/2 delta-x))
         (multiple-value-setq (adr bit-offset) (increment-x adr bit-offset pixel-size)))
        ((minusp direction)
         (setq adr (hw:24+ tv-screen-locations-per-line adr)))
        (t (multiple-value-setq (adr bit-offset) (increment-x adr bit-offset pixel-size))))
  (values adr bit-offset long-side/2)
  )



(defun draw-line-internal (word-adr bit-offset
                           delta-x delta-y
                           draw-first-point draw-end-point
                           tv-screen-locations-per-line
                           tv-screen-buffer-address
                           tv-screen-buffer-end-address
                           tv-screen-buffer-pixel-size
                           alu-function)
  (let ((long-side delta-x)
        direction long-side/2)
    (setq direction (- delta-x delta-y))
    (when (minusp direction)
      (setq delta-x delta-y)
      (setq delta-y long-side)
      (setq long-side delta-x))
    (setq long-side/2 (ash long-side -1))
    (when (not draw-first-point)
      (setq long-side (1- long-side))
      (multiple-value (word-adr bit-offset long-side/2)
        (increment-cell-location
          word-adr bit-offset
          tv-screen-buffer-pixel-size
          direction long-side long-side/2
          delta-x delta-y tv-screen-locations-per-line)))

    (do ((count long-side (1- count))
         word)
        ((or (zerop count)
             (and (= count 1)
                  (not draw-end-point))))
      (when (and (hw:32>= word-adr tv-screen-buffer-address)
                 (hw:32<= word-adr tv-screen-buffer-end-address))
        (draw-bit-patterns word-adr 0 bit-offset tv-screen-buffer-pixel-size bit-offset gr:*all-ones* alu-function))
      (multiple-value-setq (word-adr bit-offset long-side/2)
        (increment-cell-location
          word-adr bit-offset
          tv-screen-buffer-pixel-size
          direction long-side long-side/2
          delta-x delta-y tv-screen-locations-per-line))
      )
    )
  )

(defun draw-line (x0 y0 x1 y1 alu-function draw-end-point sheet)
  (li:%trap-if-not-both-fixnum x0 y0)
  (li:%trap-if-not-both-fixnum x1 y1)
  (li:%trap-if-not-both-fixnum alu-function alu-function)
  (let ((delta-x (hw:24- x1 x0))
        (delta-y (hw:24- y1 y0))
        (draw-first-point t)
        tv-screen-locations-per-line
        tv-screen-bit-offset
        tv-screen-buffer-address
        tv-screen-buffer-end-address
        tv-screen-buffer-pixel-size-mrot
        )
    (when (minusp delta-x)
      (setq delta-x (hw:24- 0 delta-x))
      (setq delta-y (hw:24- 0 delta-y))
      (setq draw-first-point draw-end-point)
      (setq draw-end-point t)
      (setq tv-screen-locations-per-line x0)       ; Used as a temp location
      (setq x0 x1)
      (setq x1 tv-screen-locations-per-line)       ; Used as a temp location
      (setq tv-screen-locations-per-line y0)       ; Used as a temp location
      (setq y0 y1)
      (setq y1 tv-screen-locations-per-line))      ; Used as a temp location
    (setq gr:*allow-sequence-break* (1+ gr:*allow-sequence-break*))
    (multiple-value-setq (tv-screen-locations-per-line
                          tv-screen-bit-offset
                          tv-screen-buffer-address
                          tv-screen-buffer-end-address
                          tv-screen-buffer-pixel-size-mrot)
      (select-sheet sheet))
    (setq gr:*allow-sequence-break* (1- gr:*allow-sequence-break*))
    (multiple-value-setq (x0 y0)
      (tv-xy-address
        x0 y0
        tv-screen-locations-per-line
        tv-screen-bit-offset
        tv-screen-buffer-address
        tv-screen-buffer-pixel-size-mrot))
    (when (minusp delta-y)
      (setq tv-screen-locations-per-line (- tv-screen-locations-per-line))
      (setq delta-y (- delta-y)))
    (draw-line-internal
      x0 y0 delta-x delta-y draw-first-point
      draw-end-point tv-screen-locations-per-line
      tv-screen-buffer-address
      tv-screen-buffer-end-address
      (ash 1 tv-screen-buffer-pixel-size-mrot)
      alu-function)
    )
  )

;;;
;;;  BITBLT STUFF
;;;

(defun bitblt-column (dest-adr
                      src-adr
                      column-height
                      src-column-height
                      dest-adr-inc
                      src-adr-inc
                      dest-byte-spec
                      src-byte-spec
                      src-y-offset
                      src-offset
                      alu-function)
  (do ((i 0)
       (number-of-rows-until-wrap-around (hw:24- src-column-height src-offset) src-column-height)
       (src-address (hw:24+ src-y-offset src-adr) src-adr))
      ((= i column-height))
    (dotimes (j number-of-rows-until-wrap-around)
      (Array:%VM-WRITE32
        dest-adr
        0
        (dpb-unboxed-with-aluf
          (hw:ldb (Array:%VM-READ32 src-address 0) src-byte-spec 0)
          dest-byte-spec
          (Array:%VM-READ32 dest-adr 0)
          alu-function))
      (setq src-address (hw:24+ src-adr-inc src-address))
      (setq dest-adr (hw:24+ dest-adr-inc dest-adr))
      (setq i (1+ i)))
    )
  )

(defun max-2-args (x y)
  (if (> x y) x y)
  )

(defun min-2-args (x y)
  (if (> x y) y x)
  )

(defun bitblt-rectangle-internal (src-adr
                                  dest-adr
                                  src-bit-off
                                  dest-bit-off
                                  dest-height
                                  src-height
                                  dest-inc-adr
                                  src-inc-adr
                                  src-y-offset
                                  src-offset
                                  alu-function
                                  number-of-columns-until-wrap-around
                                  &aux size)
  (dotimes (i number-of-columns-until-wrap-around)
    (setq size (hw:24- 32. (max-2-args src-bit-off dest-bit-off)))
    (bitblt-column
        dest-adr
        src-adr
        dest-height
        src-height
        dest-inc-adr
        src-inc-adr
        (byte size dest-bit-off)
        (byte size src-bit-off)
        src-offset
        alu-function)
    (multiple-value-setq (src-adr src-bit-off)
      (increment-x src-adr src-bit-off size))
    (multiple-value-setq (dest-adr dest-bit-off)
      (increment-x dest-adr dest-bit-off size)))
  (values dest-adr dest-bit-off)
  )

(defun bitblt-rectangle (src-adr
                         dest-adr
                         src-bit-off
                         dest-bit-off
                         src-width
                         src-height
                         dest-width
                         dest-height
                         src-inc-adr
                         dest-inc-adr
                         src-offset-width
                         src-offset-height
                         alu-function)
  (do ((i 0 (+ i number-of-columns-until-wrap-around))
       (number-of-columns-until-wrap-around
         (area-data:poor-mans-ceiling
           (hw:24- src-offset-width src-width)
           32.)
         (area-data:poor-mans-ceiling (min-2-args (hw:24- dest-width i) src-width) 32.))
       )
      ((= i dest-width))
    (multiple-value-setq (dest-adr dest-bit-off)
      (bitblt-rectangle-internal
        (hw:24+ src-offset-width src-adr)
        dest-adr
        src-bit-off
        dest-bit-off
        dest-height
        src-height
        dest-inc-adr
        src-inc-adr
        src-offset-height
        alu-function
        number-of-columns-until-wrap-around))
    (setq src-offset-width 0)
    )
  )


(defun bitblt-decode-array (array x y)
  (let (dim-x dim-y offset array-origin array-type width-in-bits word-offset byte-spec)
    (multiple-value-setq (offset array-origin array-type dim-y dim-x)
      (array:decode-2d-array array y x))
    (multiple-value-setq (y x) (new-math:divide-fixnum offset dim-x))
    (prims:dispatch (byte 5 0) array-type
      (array:art-1b
        (setq byte-spec (byte 24. 0))
        (setq word-offset (hw:ldb offset (byte 19. 5) 0))
        (setq width-in-bits (hw:dpb dim-x byte-spec 0))
        (setq x (hw:dpb x byte-spec 0)))
      (array:art-2b
        (setq byte-spec (byte 23. 1))
        (setq word-offset (hw:ldb offset (byte 20. 4) 0))
        (setq width-in-bits (hw:dpb dim-x byte-spec 0))
        (setq x (hw:dpb x byte-spec 0)))
      (array:art-4b
        (setq byte-spec (byte 22. 2))
        (setq word-offset (hw:ldb offset (byte 21. 3) 0))
        (setq width-in-bits (hw:dpb dim-x byte-spec 0))
        (setq x (hw:dpb x byte-spec 0)))
      (array:art-8b
        (setq byte-spec (byte 21. 3))
        (setq word-offset (hw:ldb offset (byte 22. 2) 0))
        (setq width-in-bits (hw:dpb dim-x byte-spec 0))
        (setq x (hw:dpb x byte-spec 0)))
      (array:art-16b
        (setq byte-spec (byte 20. 4))
        (setq word-offset (hw:ldb offset (byte 23. 1) 0))
        (setq width-in-bits (hw:dpb dim-x byte-spec 0))
        (setq x (hw:dpb x byte-spec 0)))
      (array:art-32b
        (setq byte-spec (byte 19. 5))
        (setq word-offset (hw:ldb offset (byte 24. 0) 0))
        (setq width-in-bits (hw:dpb dim-x byte-spec 0))
        (setq x (hw:dpb x byte-spec 0)))
      (t (li:error "Not a bit array"))
      )
    (unless (zerop (hw:ldb width-in-bits (byte 5. 0) 0))
      (li:error "Width must be a multiple of 32."))
    (values (hw:24+ word-offset array-origin)
            byte-spec
            width-in-bits
            dim-y
            x
            y)
    )
  )

(defun bitblt-left-to-right (alu-function width height src-adr src-x src-y dest-adr dest-x dest-y)
  )

(defun bitblt-right-to-left (alu-function width height src-adr src-x src-y dest-adr dest-x dest-y)

  )

(defun bitblt-negative-height (height word-inc-src-rows word-inc-dest-rows
                               from-array To-array src-dim-y dest-dim-y from-y)
  (let (bitblt-src-y-offset
        temp1 temp2)
    (setq height (- height))
    ;; Change from top of column to bottom of columns for source and destination.
    (setq from-array (hw:24+ (new-math:multiply-fixnum
                               (hw:24+ -1 src-dim-y)
                               word-inc-src-rows)
                             from-array))
    (setq To-array (hw:24+ (new-math:multiply-fixnum
                             (hw:24+ -1 dest-dim-y)
                             word-inc-dest-rows)
                           To-array))
    ;; negate increment for source rows
    (setq word-inc-src-rows (- word-inc-src-rows))
    ;; negate word increment between destination rows.
    (setq word-inc-dest-rows (- word-inc-dest-rows))
    (multiple-value-setq (temp1 temp2)
      (new-math:divide-fixnum (hw:24+ height from-y)
                              src-dim-y))
    ;; number of rows is then offset from the bottom of column
    (setq from-y (hw:24- height temp1))
    (setq bitblt-src-y-offset (new-math:multiple-fixnum from-y (- word-inc-src-rows)))
    (values height from-array To-array word-inc-src-rows word-inc-dest-rows from-y bitblt-src-y-offset)
    )
  )


(defun bitblt (alu-function width height from-array from-x from-y to-array to-x to-y)
  (li:%trap-if-not-both-fixnum width height)
  (li:%trap-if-not-both-fixnum from-x from-y)
  (li:%trap-if-not-both-fixnum to-x to-y)
  (li:%trap-if-not-both-fixnum alu-function alu-function)
  (or (arrayp from-array)
      (li:error "~S is not an array" from-array))
  (or (arrayp to-array)
      (li:error "~S is not an array" to-array))
  (let (byte-spec
        bitblt-dest-width
        word-inc-dest-rows
        dest-dim-y
        bitblt-dest-width
        bitblt-src-width
        src-dim-y
        word-inc-src-rows
        bitblt-src-y-offset)
    ;; decode destination
    (multiple-value-setq (To-array byte-spec bitblt-dest-width dest-dim-y To-x To-y)
      (bitblt-decode-array To-array To-x To-y))
    ;; word increment between destination rows
    (setq word-inc-dest-rows (hw:ldb bitblt-dest-width (byte 21. 5.) 0))
    ;; decode source
    (multiple-value-setq (from-array byte-spec bitblt-src-width src-dim-y from-x from-y)
      (bitblt-decode-array from-array from-x from-y))
    ;; shifting down 5 bits to divide by 32.
    (setq word-inc-src-rows (hw:ldb bitblt-src-width (byte 21. 5.) 0))
    ;; compute offset from top of column.
    (setq bitblt-src-y-offset (new-math:multiply-fixnum from-y word-inc-src-rows))
    ;; get start address of first column assuming that height is positive.
    (setq from-array (hw:24+ (- bitblt-src-y-offset) from-array))
    (when (minusp height)
      (multiple-value-setq (height
                            from-array
                            To-array
                            word-inc-src-rows
                            word-inc-dest-rows
                            from-y
                            bitblt-src-y-offset
                            )
        (bitblt-negative-height height word-inc-src-rows word-inc-dest-rows
                                from-array To-array src-dim-y dest-dim-y from-y)
        )
      )
    (setq width (hw:dpb width byte-spec 0))
    )
  )
