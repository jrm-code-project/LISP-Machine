;;; -*-Mode:LISP; Package:FED; Base:10 -*-

;The elements of a FONT-DESCRIPTOR are either NIL or a CHAR-DESCRIPTOR.
;If NIL then this character is not defined for this font.

(DEFSTRUCT (FONT-DESCRIPTOR :ARRAY-LEADER :NAMED)
           (FD-FILL-POINTER 0)
           FD-NAME
           FD-LINE-SPACING              ;Vertical distance between baselines.
           FD-BASELINE                  ;Vertical distance from top of characters
                                        ;in this font.  The baseline is what is
                                        ;aligned for different fonts.
           FD-BLINKER-HEIGHT            ;Height of a "blinker" in this font.
           FD-BLINKER-WIDTH             ;Width of a "blinker" in this font.
           FD-SPACE-WIDTH               ;Width of a space.
           FD-DOUBLE-WIDTH-P            ;T means this font is intended for display
                                        ;with twice as many pixels per unit distance
                                        ;in the horizontal direction. (ESCAPE 7)

                                        ;The rest is for saving info that
                                        ;comes in Xerox fonts, so we don't lose it.
           (FD-VERT-RESOLUTION 3840)    ;Dots per inch, times ten.
           (FD-HORIZ-RESOLUTION 3840)   ;" "
                                        ;Default is right for the Dover.
           (FD-ROTATION 0)              ;Rotation in degrees.
           )

;A CHAR-DESCRIPTOR is a two dimensional array (with leader).
;The first dimension is the height of the character and the second is the width

(DEFSTRUCT (CHAR-DESCRIPTOR :ARRAY-LEADER :NAMED)
           CD-FILL-POINTER
           CD-NAME
           CD-CHAR-WIDTH                ;The horizontal distance taken by this character
           CD-CHAR-VERT-WIDTH           ;Vertical spacing caused by this character
                                        ;(always 0 for the usual horizontal font).
                                        ;For the sake of Xerox fonts.
           CD-CHAR-LEFT-KERN)           ;The distance to the left to move before placing the
                                        ;character.  A left kern of -5 means the array is to
                                        ;be placed 5 units to the right of the current position.
;NOTE: the CHAR-WIDTH is measured from the current position to the new position.
;the LEFT-KERN is not used in this computation therefore if the LEFT-KERN plus
;the width (second dimension) of the array is greater than the CHAR-WIDTH the
;character will overlap the next character on the line.
;A positive value of LEFT-KERN will cause the character to overlap the space of the
;last character.
