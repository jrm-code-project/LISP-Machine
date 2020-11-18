;;;-*- Mode:LISP; Package:(LHACK :USE GLOBAL); Base:10; Readtable:CL -*-

;;; SI:COLD-LOAD-STREAM is implemented using: %DRAW-CHAR, %DRAW-RECTANGLE, and %DRAW-LINE.
;;; TV:FONT structures are accessed (FONT-CHAR-WIDTH-TABLE, FONT-INDEXING-TABLE, FONT-CHAR-WIDTH)
;;; TV:ALU-IOR

; Here's some documentation of the Lambda's primitives for character drawing.
;
; (TV:DRAW-CHAR FONT CHAR X Y ALU SHEET-OR-ARRAY)
;       Draws the character with code CHAR in FONT with its upper left corner at
;       position (X,Y) in outside coordinates.  ALU is used as the ALU function,
;       so you can either draw or erase.  There is no clipping or error checking.
;
; (SYS:%DRAW-CHAR FONT CHAR X Y ALU SHEET-OR-ARRAY)
;       This is the actual microcoded primitive.  It does not take into account the
;       indexing table of a wide font, so when used on a wide font CHAR is not the
;       character code that the user actually wants to output.
;       It is best to use TV:DRAW-CHAR.
;
; (SYS:%DRAW-LINE X0 Y0 X Y ALU DRAW-END-POINT-P SHEET-OR-ARRAY)
;       Draws a line from (X0,Y0) to (X,Y) all relative to the outside edges of the
;       sheet, or indices in the array.  The point at (X,Y) is not drawn if
;       DRAW-END-POINT-P is NIL.  No clipping or error checking is done.
;
; (SYS:%DRAW-RECTANGLE WIDTH HEIGHT X-BITPOS Y-BITPOS ALU-FUNCTION SHEET-OR-ARRAY)
;       Draws a rectangle of size WIDTH by HEIGHT with its upper left corner at
;       (X-BITPOS,Y-BITPOS). You can draw, erase, or complement the rectangle.
;       There is no clipping or error checking.




(defvar *little-screen*)

(defun make-little-screen (&optional (height 770))
  (setq *little-screen*
        (zl:make-array (list height (* 32 32))
                       :element-type 'bit
                       :displaced-to (send tv:main-screen :buffer))))



(defun foo (&optional (start 200) (size 100))
  (dotimes (i size)
    (draw-rectangle (+ i 10) (+ 10 i)
                       (+ i start) (+ i start)
                       tv:alu-ior lhack:*little-screen*)))

(defun draw-rectangle (width height x-bitpos y-bitpos alu-function sheet-or-array)
  ;(si:%draw-rectangle width height x-bitpos y-bitpos alu-function sheet-or-array)
  )
