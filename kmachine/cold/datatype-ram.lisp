;;; -*- Mode:LISP; Package:DATATYPE-RAM; Readtable:CL; Base:10. -*-

;(in-package 'dt-ram)

(export '(
          load-dt-ram-pattern
          load-initial-datatype-ram
          read-dt-ram
          write-dt-ram
          ))

(defafun write-dt-ram (lbox ldt rbox rdt dt-code value)
  (alu-field field-pass processor-control a5 processor-control (byte 1. 8.)) ;data bit
 left
  (alu-field extract-bit-right nop a0 a0 (byte 1. 0.))   ;left word
  (alu-field field-pass a6 a1 a1 (byte 6. 26.) unboxed br-zero)
  (branch right ())
  (alu-field field-pass a6 a1 a1 (byte 6. 26.) boxed)
 right
  (alu-field extract-bit-right nop a2 a2 (byte 1. 0.))   ;right word
  (alu-field field-pass a7 a3 a3 (byte 6. 26.) unboxed br-zero)
  (branch doit ())
  (alu-field field-pass a7 a3 a3 (byte 6. 26.) boxed)
 doit
  (alu-field field-extract-r a4 a4 a4 (byte 3. 0.) unboxed)
  (alu l+r a4 a4 a4 unboxed)
  (movea a8 write-dt-ram-dispatch)
  (alu l+r nop a8 a4)
  (nop)
  (nop next-pc-dispatch))


(defafun write-dt-ram-dispatch ()
  (alu setr datatype-ram-write-pulse a6 a7 dt-0)
  (unconditional-branch write-dt-ram-dispatch-exit (alu setr nop a6 a7 dt-0))
  (alu setr datatype-ram-write-pulse a6 a7 dt-1)
  (unconditional-branch write-dt-ram-dispatch-exit (alu setr nop a6 a7 dt-1))
  (alu setr datatype-ram-write-pulse a6 a7 dt-2)
  (unconditional-branch write-dt-ram-dispatch-exit (alu setr nop a6 a7 dt-2))
  (alu setr datatype-ram-write-pulse a6 a7 dt-3)
  (unconditional-branch write-dt-ram-dispatch-exit (alu setr nop a6 a7 dt-3))
  (alu setr datatype-ram-write-pulse a6 a7 dt-4)
  (unconditional-branch write-dt-ram-dispatch-exit (alu setr nop a6 a7 dt-4))
  (alu setr datatype-ram-write-pulse a6 a7 dt-5)
  (unconditional-branch write-dt-ram-dispatch-exit (alu setr nop a6 a7 dt-5))
  (alu setr datatype-ram-write-pulse a6 a7 dt-6)
  (unconditional-branch write-dt-ram-dispatch-exit (alu setr nop a6 a7 dt-6))
  (alu setr datatype-ram-write-pulse a6 a7 dt-7)
  (unconditional-branch write-dt-ram-dispatch-exit (alu setr nop a6 a7 dt-7))
 write-dt-ram-dispatch-exit
  (movei a6 '0) ;these two moves kill the illegal stuff in a6 and a7
  (movei a7 '0)
  (return a5))


(defafun read-dt-ram (lbox ldt rbox rdt dt-code)
 left
  (alu-field extract-bit-right nop a0 a0 (byte 1. 0.))   ;left word
  (alu-field field-pass a6 a1 a1 (byte 6. 26.) unboxed br-zero)
  (branch right ())
  (alu-field field-pass a6 a1 a1 (byte 6. 26.) boxed)
 right
  (alu-field extract-bit-right nop a2 a2 (byte 1. 0.))   ;right word
  (alu-field field-pass a7 a3 a3 (byte 6. 26.) unboxed br-zero)
  (branch doit ())
  (alu-field field-pass a7 a3 a3 (byte 6. 26.) boxed)
 doit
  (alu-field field-extract-r a4 a4 a4 (byte 3. 0.) unboxed)
  (alu l+r a4 a4 a4 unboxed)
  (movea a8 read-dt-ram-dispatch)
  (alu l+r nop a8 a4)
  (nop)
  (nop next-pc-dispatch))


(defafun read-dt-ram-dispatch ()
  (alu setr nop a6 a7 dt-0)
  (unconditional-branch read-dt (alu setr nop a6 a7 dt-0))
  (alu setr nop a6 a7 dt-1)
  (unconditional-branch read-dt (alu setr nop a6 a7 dt-1))
  (alu setr nop a6 a7 dt-2)
  (unconditional-branch read-dt (alu setr nop a6 a7 dt-2))
  (alu setr nop a6 a7 dt-3)
  (unconditional-branch read-dt (alu setr nop a6 a7 dt-3))
  (alu setr nop a6 a7 dt-4)
  (unconditional-branch read-dt (alu setr nop a6 a7 dt-4))
  (alu setr nop a6 a7 dt-5)
  (unconditional-branch read-dt (alu setr nop a6 a7 dt-5))
  (alu setr nop a6 a7 dt-6)
  (unconditional-branch read-dt (alu setr nop a6 a7 dt-6))
  (alu setr nop a6 a7 dt-7)
  (unconditional-branch read-dt (alu setr nop a6 a7 dt-7))
 read-dt
  (alu-field extract-bit-right a8 r0 processor-status (byte 1. 13.) unboxed)
  (movei a6 '0) ;these two moves kill the illegal stuff in a6 and a7
  (movei a7 '0)
  (alu l+r return a8 gr::*zero* ch-return next-pc-return bw-24)
 )



; load-dt-ram-pattern takes its first 5 arguments as 16 bit numbers (the top doesn't matter)
;   The bottom 8 bits are the pattern to match, and the high 8 bits are the mask to
;   be and'ed with the current test pattern. Only locations that match will be written.
;
;  Example: to set all locations with a left boxed datatype of #x1e in code 3:
;            (dt-ram-load #x0101 #x3f1e 0 0 #x0703 1)
;
(defafun load-dt-ram-pattern (lbox ldt rbox rdt dt-code value)
  (movei a10 '1)
 lbox-loop
  (alu-field field-and a9 a0 a10 (byte 8. -8.) unboxed)
  (alu xor nop a0 a9 bw-8)
  (test br-not-equal)
  (branch lbox-next ())

  (movei a11 '63)
 ldt-loop
  (alu-field field-and a9 a1 a11 (byte 8. -8.) unboxed)
  (alu xor nop a1 a9 bw-8)
  (test br-not-equal)
  (branch ldt-next ())

  (movei a12 '1)
 rbox-loop
  (alu-field field-and a9 a2 a12 (byte 8. -8.) unboxed)
  (alu xor nop a2 a9 bw-8)
  (test br-not-equal)
  (branch rbox-next ())

  (movei a13 '63)
 rdt-loop
  (alu-field field-and a9 a3 a13 (byte 8. -8.) unboxed)
  (alu xor nop a3 a9 bw-8)
  (test br-not-equal)
  (branch rdt-next ())

  (movei a14 '7)
 dt-code-loop
  (alu-field field-and a9 a4 a14 (byte 8. -8.) unboxed)
  (alu xor nop a4 a9 bw-8)
  (test br-not-equal)
  (branch dt-code-next ())

  (move o5 a5 ch-open)
  (move o4 a14)
  (move o3 a13)
  (move o2 a12)
  (move o1 a11)
  (call (write-dt-ram 6) a6 (o0 a10))

 dt-code-next
  (alu r-1 a14 a14 a14 bw-24)
  (test br-not-negative)
  (branch dt-code-loop ())

 rdt-next
  (alu r-1 a13 a13 a13 bw-24)
  (test br-not-negative)
  (branch rdt-loop ())

 rbox-next
  (alu r-1 a12 a12 a12 bw-24)
  (test br-not-negative)
  (branch rbox-loop ())

 ldt-next
  (alu r-1 a11 a11 a11 bw-24)
  (test br-not-negative)
  (branch ldt-loop ())

 lbox-next
  (alu r-1 a10 a10 a10 bw-24)
  (test br-not-negative)
  (branch lbox-loop ())

  (return a6))


;;; This function sets up the initial datatype ram
;;;
(defun load-initial-datatype-ram ()

;;; Init the whole datatype ram to ones

  (load-dt-ram-pattern  ; set all the bits
    #x0000   ;all left boxes
    #x0000   ;all left datatypes
    #x0000   ;all right boxes
    #x0000   ;all right datatypes
    #x0000   ;all codes
    1)

;;; Clear all the no-check code bits

  (load-dt-ram-pattern  ; clear the no-check bits
    #x0000   ;all left boxes
    #x0000   ;all left datatypes
    #x0000   ;all right boxes
    #x0000   ;all right datatypes
    (logior #x0700 vinc:$$dtc-none)     ;only the no-check code
    0)

;;; set up the check for both fixnum

  (load-dt-ram-pattern  ; clear the fixnum-codes, left-fixnum & right-fixnum
    #x0101                              ;left-boxed
    (logior #x3f00 vinc:$$dtp-fixnum)   ;left-fixnum
    #x0101                              ;right boxed
    (logior #x3f00 vinc:$$dtp-fixnum)   ;right-fixnum
    (logior #x0700 vinc:$$dtc-both-fixnum) ;fixnum without overflow
    0)

  (load-dt-ram-pattern
    #x0101                              ;left-boxed
    (logior #x3f00 vinc:$$dtp-fixnum)   ;left-fixnum
    #x0101                              ;right boxed
    (logior #x3f00 vinc:$$dtp-fixnum)   ;right-fixnum
    (logior #x0700 vinc:$$dtc-both-fixnum-with-overflow) ;fixnum with overflow
    0)

;;; set up check for list on right side

  (load-dt-ram-pattern  ; clear the list codes, left-any & right-list
    #x0000                              ;all left-box
    #x0000                              ;all left datatypes
    #x0101                              ;right-boxed
    (logior #x3f00 vinc:$$dtp-cons)             ;right-list
    (logior #x0700 vinc:$$dtc-right-list)       ;list code
    0)
  (load-dt-ram-pattern  ; clear the list codes, left-any & right-nil
    #x0000                              ;all left-box
    #x0000                              ;all left datatypes
    #x0101                              ;right-boxed
    (logior #x3f00 vinc:$$dtp-nil)              ;right-nil
    (logior #x0700 vinc:$$dtc-right-list)       ;list code
    0)

;;; set up check for array on right side and structure on left side

  (load-dt-ram-pattern  ; clear the list codes, left-structure & right-array
    #x0101                                 ;left-box
    (logior #x3f00 vinc:$$dtp-structure)   ;left-structure
    #x0101                                 ;right-boxed
    (logior #x3f00 vinc:$$dtp-array)       ;right-array
    (logior #x0700 vinc:$$dtc-right-array-and-left-structure) ;array code
    0)


;;; set up check for not both character

  (load-dt-ram-pattern  ; clear the character-codes, left-character & right-character
    #x0101                               ;left-boxed
    (logior #x3f00 vinc:$$dtp-character) ;left-character
    #x0101                               ;right boxed
    (logior #x3f00 vinc:$$dtp-character) ;right-character
    (logior #x0700 vinc:$$dtc-both-character) ;both character codes
    0)

;;; set up check for same type of hairy numbers (for EQL)

  (load-dt-ram-pattern  ; clear the entry
    #x0000   ;all left boxes
    #x0000   ;all left datatypes
    #x0000   ;all right boxes
    #x0000   ;all right datatypes
    (logior #x0700 vinc:$$dtc-hairy-number)     ;only the no-check code
    0)

  (load-dt-ram-pattern  ;trap if left unboxed
    #x0100                               ;left-unboxed
    #x0000                               ;left dt any
    #x0000                               ;right box any
    #x0000                               ;right dt any
    (logior #x0700 vinc:$$dtc-hairy-number) ;both fixnum codes
    1)

  (load-dt-ram-pattern  ;trap if right unboxed
    #x0000                               ;left box any
    #x0000                               ;left dt any
    #x0100                               ;right unbox
    #x0000                               ;right dt any
    (logior #x0700 vinc:$$dtc-hairy-number) ;both fixnum codes
    1)

  (load-dt-ram-pattern  ; trap if both bignum
    #x0101                                 ;left-boxed
    (logior #x3f00 vinc:$$dtp-bignum) ;left-short
    #x0101                                 ;right boxed
    (logior #x3f00 vinc:$$dtp-bignum) ;right-short
    (logior #x0700 vinc:$$dtc-hairy-number) ;both short codes
    1)

  (load-dt-ram-pattern  ; trap if both rational
    #x0101                                 ;left-boxed
    (logior #x3f00 vinc:$$dtp-rational) ;left-short
    #x0101                                 ;right boxed
    (logior #x3f00 vinc:$$dtp-rational) ;right-short
    (logior #x0700 vinc:$$dtc-hairy-number) ;both short codes
    1)

  (load-dt-ram-pattern  ; trap if both short-float
    #x0101                                 ;left-boxed
    (logior #x3f00 vinc:$$dtp-short-float) ;left-short
    #x0101                                 ;right boxed
    (logior #x3f00 vinc:$$dtp-short-float) ;right-short
    (logior #x0700 vinc:$$dtc-hairy-number) ;both short codes
    1)

  (load-dt-ram-pattern  ; trap if both float
    #x0101                               ;left-boxed
    (logior #x3f00 vinc:$$dtp-single-float)     ;left-float
    #x0101                               ;right boxed
    (logior #x3f00 vinc:$$dtp-single-float)     ;right-float
    (logior #x0700 vinc:$$dtc-hairy-number) ;both float codes
    1)

  (load-dt-ram-pattern  ; trap if both double-float
    #x0101                               ;left-boxed
    (logior #x3f00 vinc:$$dtp-double-float) ;left-double
    #x0101                               ;right boxed
    (logior #x3f00 vinc:$$dtp-double-float) ;right-double
    (logior #x0700 vinc:$$dtc-hairy-number) ;both double codes
    1)

  (load-dt-ram-pattern  ; trap if both complex
    #x0101                                 ;left-boxed
    (logior #x3f00 vinc:$$dtp-complex) ;left-short
    #x0101                                 ;right boxed
    (logior #x3f00 vinc:$$dtp-complex) ;right-short
    (logior #x0700 vinc:$$dtc-hairy-number) ;both short codes
    1)

 )
