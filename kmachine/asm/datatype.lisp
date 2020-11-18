;;; -*- Mode:LISP; Package:TEST; Base:10; Readtable:ZL -*-


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
  (alu-field field-extract-r a4 a4 a4 (byte 3. 0.))
  (alu l+r a4 a4 a4)
  (movea a7 write-dt-ram-dispatch)
  (alu l+r nop a7 a4)
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
  (alu-field field-extract-r a4 a4 a4 (byte 3. 0.))
  (alu l+r a4 a4 a4)
  (movea a7 read-dt-ram-dispatch)
  (alu l+r nop a7 a4)
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
  (alu-field extract-bit-right a8 r0 processor-status (byte 1. 13.))
  (return a8))



; load-dt-ram-pattern takes its first 5 arguments as 16 bit numbers (the top doesn't matter)
;   The bottom 8 bits are the pattern to match, and the high 8 bits are the mask to
;   be and'ed with the current test pattern. Only locations that match will be written.
;
;  Example: to set all locations with a left boxed datatype of #x1e in code 3:
;            (dt-ram-load #x0703 #x0101 #x3f1e 0 0 1)
;

(defun load-dt-ram-pattern (code-arg lbox-arg left-arg rbox-arg right-arg value)
  (dotimes (code 8.)
    (when (eq (logand code (hw:ldb code-arg (byte 8. 0.) 0))
              (logand code (hw:ldb code-arg (byte 8. 8.) 0)))
      (dotimes (lbox 2.)
        (when (eq (logand lbox (hw:ldb lbox-arg (byte 8. 0.) 0))
                  (logand lbox (hw:ldb lbox-arg (byte 8. 8.) 0)))
          (dotimes (left 64.)
            (when (eq (logand left (hw:ldb left-arg (byte 8. 0.) 0))
                      (logand left (hw:ldb left-arg (byte 8. 8.) 0)))
              (dotimes (rbox 2.)
                (when (eq (logand rbox (hw:ldb rbox-arg (byte 8. 0.) 0))
                          (logand rbox (hw:ldb rbox-arg (byte 8. 8.) 0)))
                  (dotimes (right 64.)
                    (when (eq (logand right (hw:ldb right-arg (byte 8. 0.) 0))
                              (logand right (hw:ldb right-arg (byte 8. 8.) 0)))
                      (write-dt-ram lbox left rbox right code value))))))))))))


(defconstant $$dtc-none                      0)
(defconstant $$dtc-spare1                    1)
(defconstant $$dtc-spare1                    2)
(defconstant $$dtc-spare1                    3)
(defconstant $$dtc-right-array               4)
(defconstant $$dtc-right-list                5)
(defconstant $$dtc-both-fixnum               6)
(defconstant $$dtc-both-fixnum-with-overflow 7)



;;; This function sets up the initial datatype ram
;;;
(defun load-initial-datatype-ram ()

;;; Init the whole datatype ram to ones

  (load-dt-ram-pattern  ; set all the bits
    #x0000   ;all codes
    #x0000   ;all left boxes
    #x0000   ;all left datatypes
    #x0000   ;all right boxes
    #x0000   ;all right datatypes
    1)

;;; Clear all the no-check code bits

  (load-dt-ram-pattern  ; clear the no-check bits
    (logior #x0700 $$dtc-none)  ;only the no-check code
    #x0000   ;all left boxes
    #x0000   ;all left datatypes
    #x0000   ;all right boxes
    #x0000   ;all right datatypes
    0)

;;; set up the check for both fixnum or unboxed codes

  (load-dt-ram-pattern  ; clear the fixnum-codes, left-fixnum & right-fixnum
    (logior #x0600 $$dtc-both-fixnum)   ;both fixnum codes
    #x0101                              ;left-boxed
    (logior #x3f00 $$dtp-fixnum)        ;left-fixnum
    #x0101                              ;right boxed
    (logior #x3f00 $$dtp-fixnum)        ;right-fixnum
    0)
  (load-dt-ram-pattern  ; clear the fixnum-codes, left-unboxed & right-unboxed
    (logior #x0600 $$dtc-both-fixnum)   ;both fixnum codes
    #x0100                              ;left-unboxed
    #x0000                              ;all left datatypes
    #x0100                              ;right unboxed
    #x0000                              ;all right datatypes
    0)
  (load-dt-ram-pattern  ; clear the fixnum-codes, left-fixnum & right-unboxed
    (logior #x0600 $$dtc-both-fixnum)   ;both fixnum codes
    #x0101                              ;left-boxed
    (logior #x3f00 $$dtp-fixnum)        ;left-fixnum
    #x0100                              ;right unboxed
    #x0000                              ;all right datatypes
    0)
  (load-dt-ram-pattern  ; clear the fixnum-codes, left-unboxed & right-fixnum
    (logior #x0600 $$dtc-both-fixnum)   ;both fixnum codes
    #x0100                              ;left-unboxed
    #x0000                              ;all left datatypes
    #x0101                              ;right boxed
    (logior #x3f00 $$dtp-fixnum)        ;right-fixnum
    0)

;;; set up check for list on right side

  (load-dt-ram-pattern  ; clear the list codes, left-any & right-list
    (logior #x0700 $$dtc-right-list)    ;list code
    #x0000                              ;all left-box
    #x0000                              ;all left datatypes
    #x0101                              ;right-boxed
    (logior #x3f00 $$dtp-list)          ;right-list
    0)
  (load-dt-ram-pattern  ; clear the list codes, left-any & right-nil
    (logior #x0700 $$dtc-right-list)    ;list code
    #x0000                              ;all left-box
    #x0000                              ;all left datatypes
    #x0101                              ;right-boxed
    (logior #x3f00 $$dtp-nil)           ;right-nil
    0)

;;; set up check for array on right side

  (load-dt-ram-pattern  ; clear the list codes, left-any & right-list
    (logior #x0700 $$dtc-right-array)   ;array code
    #x0000                              ;all left-box
    #x0000                              ;all left datatypes
    #x0101                              ;right-boxed
    (logior #x3f00 $$dtp-array)         ;right-array
    0)

 )
