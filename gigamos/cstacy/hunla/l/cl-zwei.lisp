;;; -*- Mode:LISP; Package:USER; Readtable:CL; Base:10 -*-

(EVAL-WHEN (EVAL COMPILE LOAD)
  (defpackage "CL-ZWEI" :use ("LISP")))

(import
  `(ZWEI:DIS-ALL                                ;Macros for implementing commands
    ZWEI:DIS-BPS
    ZWEI:DIS-LINE
    ZWEI:DIS-MARK-GOES
    ZWEI:DIS-RETURN
    ZWEI:DIS-TEXT
    ZWEI:DIS-NONE
    ZWEI:KM
    ZWEI:NM
    ZWEI:R
    ZWEI:-R
    ZWEI:DEFCOM
    ZWEI:*NUMERIC-ARG-P*
    ZWEI:*NUMERIC-ARG*

    ZWEI:BP-LINE                                ;Basic data structures
    ZWEI:MOVE-BP
    ZWEI:COPY-BP

    ZWEI:*COMMENT-BEGIN*                        ;Assorted specials
    ZWEI:*COMMENT-END*

    ZWEI:*MAJOR-MODE*                           ;Mode-related things
    ZWEI:MODE
    ZWEI:MAJOR-MODE
    ZWEI:FUNDAMENTAL-MODE
    ZWEI:TEXT-MODE
    ZWEI:LISP-MODE
    ZWEI:LISP-SYNTAX-MIXIN

    ZWEI:POINT                                  ;Intervals, buffers, windows
    ZWEI:*WINDOW*
    ZWEI:*INTERVAL*
    ZWEI:INTERVAL
    ZWEI:INTERVAL-LAST-BP
    ZWEI:INTERVAL-FIRST-BP

    ZWEI:TYPEIN-LINE                            ;Subroutines
    ZWEI:DOWN-REAL-LINE
    ZWEI:FORWARD-LINE

    ZWEI:*READ-ONLY-PATHNAMES*                  ;Things we've added
    )
  (find-package "CL-ZWEI"))

(import
  `(ZL:SEND
    ZL:SELF
    SI:WITHOUT-INTERRUPTS
    )
  (find-package "CL-ZWEI"))

(import
  `(GF:DEFMETHOD
    GF:DEFGENERIC
    ZL:DEFFLAVOR
    GF:DEFINE-FLAVOR-ACCESS-METHODS
    )
  (find-package "CL-ZWEI"))
