;-*-Mode:Midas; base: 8; readtable: ZL-*-
;;;
;;; (c) Copyright 1984 - Lisp Machine, Inc.
;;;

(DEFCONST UC-STRING '(
;;; String processing

XCHAR-EQUAL (MISC-INST-ENTRY INTERNAL-CHAR-EQUAL)
        (CALL FXGTPP)
        ((M-1) (LISP-BYTE %%CH-CHAR) M-1)       ;Flush font or bucky bits
        ((M-2) (LISP-BYTE %%CH-CHAR) M-2)
        (JUMP-EQUAL M-1 A-2 XTRUE)              ;Equal if really equal
        (JUMP-XCT-NEXT XCHAR-EQUAL-IGNORE-CASE)
       ((M-T) A-V-NIL)

;Enter here with LDB'ed arguments in M-1, M-2
; to consider or ignore case according to ALPHABETIC-CASE-AFFECTS-STRING-COMPARISON.
;This is used by %STRING-SEARCH and %STRING-EQUAL
XCHAR-EQUAL-1-2
        (JUMP-EQUAL M-1 A-2 XTRUE)              ;Equal if really equal
        ((M-T) DPB M-ZERO Q-ALL-BUT-TYPED-POINTER A-ALPHABETIC-CASE-AFFECTS-STRING-COMPARISON)
        (JUMP-NOT-EQUAL M-T A-V-NIL XFALSE)     ;Certainly not equal if case matters
        ((M-1) (LISP-BYTE %%CH-CHAR) M-1)       ;Flush font or bucky bits
        ((M-2) (LISP-BYTE %%CH-CHAR) M-2)
        (JUMP-EQUAL M-1 A-2 XTRUE)              ;Aha, they match except for font.
XCHAR-EQUAL-IGNORE-CASE
        ((M-TEM) XOR M-1 A-2)                   ;Differ only in case bit?
        (POPJ-NOT-EQUAL M-TEM (A-CONSTANT 40))  ;If not, not equal
        (POPJ-LESS-THAN M-1 (A-CONSTANT 101))   ;And not equal if not a letter
        (POPJ-GREATER-THAN M-1 (A-CONSTANT 172))
        (JUMP-LESS-OR-EQUAL M-1 (A-CONSTANT 132) XTRUE)
        (JUMP-GREATER-OR-EQUAL M-1 (A-CONSTANT 141) XTRUE)
        (POPJ)

XBOTH-CASE-P (MISC-INST-ENTRY BOTH-CASE-P)
XALPHA-CHAR-P (MISC-INST-ENTRY ALPHA-CHAR-P)
        (DISPATCH (I-ARG DATA-TYPE-INVOKE-OP)
                        Q-DATA-TYPE C-PDL-BUFFER-POINTER TRAP-UNLESS-FIXNUM)
    (ERROR-TABLE ARGTYP FIXNUM PP 0)
        ((PDL-TOP) ANDCA PDL-TOP (A-CONSTANT 40))
        (JUMP XALPHA-CHAR-P-1)

XLOWER-CASE-P (MISC-INST-ENTRY LOWER-CASE-P)
        (DISPATCH (I-ARG DATA-TYPE-INVOKE-OP)
                        Q-DATA-TYPE C-PDL-BUFFER-POINTER TRAP-UNLESS-FIXNUM)
    (ERROR-TABLE ARGTYP FIXNUM PP 0)
        ((PDL-TOP) XOR PDL-TOP (A-CONSTANT 40))
        (JUMP XALPHA-CHAR-P-1)  ;DO NOT use JUMP-XCT-NEXT! watch out for pdl-buf pass around path problem.

XUPPER-CASE-P (MISC-INST-ENTRY UPPER-CASE-P)
        (DISPATCH (I-ARG DATA-TYPE-INVOKE-OP)
                        Q-DATA-TYPE C-PDL-BUFFER-POINTER TRAP-UNLESS-FIXNUM)
    (ERROR-TABLE ARGTYP FIXNUM PP 0)
XALPHA-CHAR-P-1
        ((M-2) (LISP-BYTE %%KBD-CONTROL-META) PDL-TOP)
        ((M-1) (LISP-BYTE %%CH-CHAR) PDL-POP)   ;Flush font or bucky bits
        ((M-T) A-V-NIL)
        (POPJ-NOT-EQUAL M-2 A-ZERO)
XALPHA-CHAR-P-2
        (POPJ-LESS-THAN M-1 (A-CONSTANT 101))   ;And not equal if not a letter
        (JUMP-LESS-OR-EQUAL M-1 (A-CONSTANT 132) XTRUE)
        (POPJ)

XALPHANUMERICP (MISC-INST-ENTRY ALPHANUMERICP)
        (DISPATCH (I-ARG DATA-TYPE-INVOKE-OP)
                        Q-DATA-TYPE C-PDL-BUFFER-POINTER TRAP-UNLESS-FIXNUM)
    (ERROR-TABLE ARGTYP FIXNUM PP 0)
        ((M-2) (LISP-BYTE %%KBD-CONTROL-META) PDL-TOP)
        ((M-1) (LISP-BYTE %%CH-CHAR) PDL-POP)   ;Flush font or bucky bits
        ((M-T) A-V-NIL)
        (POPJ-NOT-EQUAL M-2 A-ZERO)
        (POPJ-LESS-THAN M-1 (A-CONSTANT 60))
        (JUMP-LESS-OR-EQUAL M-1 (A-CONSTANT 71) XTRUE)
        (JUMP-XCT-NEXT XALPHA-CHAR-P-2)
       ((M-1) ANDCA M-1 (A-CONSTANT 40))

XCHAR-DOWNCASE (MISC-INST-ENTRY CHAR-DOWNCASE)
        (CALL-XCT-NEXT XUPPER-CASE-P)
       ((M-A) Q-TYPED-POINTER PDL-TOP)
        (JUMP XCHAR-UPCASE-1)

XCHAR-UPCASE (MISC-INST-ENTRY CHAR-UPCASE)
        (CALL-XCT-NEXT XLOWER-CASE-P)
       ((M-A) Q-TYPED-POINTER PDL-TOP)
XCHAR-UPCASE-1
        (POPJ-EQUAL-XCT-NEXT M-T A-V-NIL)
       ((M-T) M-A)
        (POPJ-XCT-NEXT)
       ((M-T) XOR M-T (A-CONSTANT 40))

XCHAR-INT (MISC-INST-ENTRY CHAR-INT)
        (DISPATCH (I-ARG DATA-TYPE-INVOKE-OP)
                        Q-DATA-TYPE C-PDL-BUFFER-POINTER TRAP-UNLESS-FIXNUM)
    (ERROR-TABLE ARGTYP FIXNUM PP 0)
        (POPJ-XCT-NEXT)
       ((M-T) DPB Q-POINTER PDL-POP (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))

XINT-CHAR (MISC-INST-ENTRY INT-CHAR)
        (DISPATCH (I-ARG DATA-TYPE-INVOKE-OP)
                        Q-DATA-TYPE C-PDL-BUFFER-POINTER TRAP-UNLESS-FIXNUM)
    (ERROR-TABLE ARGTYP FIXNUM PP 0)
        (POPJ-XCT-NEXT)
       ((M-T) DPB Q-POINTER PDL-POP (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-CHARACTER)))

;Args are width table, offset, string to scan, starting index, stop index, stop width.
;Width table must be an art-q array or else error.
;Take each character, subtract the offset,
;use difference as an index in the width table,
;and add the entry there into a cumulative value which starts at zero.
;Stop on reaching stop index, if element of "string" is not a number,
;width table entry is not a number, or if a character minus the offset
;is out of range for the width table.
;Stop before a character which would push the cumulative sum above the stop-width.
;(Stop width can be NIL to mean infinity, or no stop width).
;Returns with an extra value already on the stack: the last string index examined.
;The actual value returned is the cumulative sum.
XSTRING-WIDTH (MISC-INST-ENTRY %STRING-WIDTH)
        ((M-T) Q-TYPED-POINTER PDL-POP)   ;Pop stop-width.
        (JUMP-NOT-EQUAL M-T A-V-NIL XSTRING-WIDTH-NO-MAX)
        ((M-T) Q-TYPED-POINTER M-MINUS-ONE)
XSTRING-WIDTH-NO-MAX
        ((PDL-INDEX) SUB PDL-POINTER (A-CONSTANT 4))
        (call-xct-next gadptr)
       ((m-array-pointer) invalidate-array-cache c-pdl-buffer-index)
        (dispatch (lisp-byte %%array-type-field) m-array-header skip-if-numeric-array)
        (JUMP XSTRING-WIDTH-1)
        (call array-trap)
    (error-table number-array-not-allowed m-array-pointer)
XSTRING-WIDTH-1
        ((M-J) Q-POINTER M-T)           ;Save stop width.
        ((M-R) m-array-origin)          ;Save data origin of the width table.
        ((M-D) m-array-length)          ;Save length of it.
        (CALL XSTRING-SEARCH-DECODE)
    (ERROR-TABLE CALLS-SUB %STRING-WIDTH)
        ((M-1) A-ZERO)
        ((M-K) Q-POINTER PDL-POP)
        (JUMP-GREATER-OR-EQUAL M-I A-C XSTRING-WIDTH-9)
;M-R has data origin of width table.
;M-D has length of width table.
;M-J has stop width.  Return before any char that would go above this width.
;M-I has current index in string; M-C has stop index.
;M-1 has current cumulative value (unboxed).
;M-T has the current character.
;M-B, M-E, M-Q, M-S set up for accessing the string.
;M-K has offset to subtract from each char before looking in table.
XSTRING-WIDTH-LOOP
        (jump-data-type-not-equal m-t (a-constant (byte-value q-data-type dtp-fix))
                 xstring-width-9)       ;Exit if next element of "string" is not a number.
        ((M-T) Q-POINTER M-T)
;Subtract the offset, and exit if result is negative.
        ((M-2) SUB M-T A-K)
        (JUMP-LESS-THAN M-2 A-ZERO XSTRING-WIDTH-9)
;Exit if difference is bigger than length of width table.
        (JUMP-GREATER-OR-EQUAL M-2 A-D XSTRING-WIDTH-9)
        ((VMA-START-READ) ADD M-R A-2)  ;Access width table element.
        (CHECK-PAGE-READ)       ;No need to transport since all non-fixnums count as NIL.
        (jump-data-type-not-equal md (a-constant (byte-value q-data-type dtp-fix))
                xstring-width-9)        ;Not fixnum means stop at this character.
        ((M-2) Q-POINTER MD)
        ((M-1) ADD M-1 A-2)
        (JUMP-GREATER-THAN M-1 A-J XSTRING-WIDTH-TOO-WIDE)  ;Width exceeds max.
       ((M-I) ADD M-I (A-CONSTANT 1))           ;Advance subscripts
;       ((M-Q) ADD M-Q (A-CONSTANT 1))
        (JUMP-GREATER-OR-EQUAL M-I A-C XSTRING-WIDTH-9) ;Reached upper bound, return.
;       (CALL-GREATER-OR-EQUAL M-Q a-array-length array-trap)
    (error-table subscript-oob m-q m-array-length)
        (dispatch-call-xct-next (lisp-byte %%array-type-field) m-array-header array-type-ref-dispatch)
;      (no-op)
       ((m-q) add m-q (a-constant 1))
        (JUMP XSTRING-WIDTH-LOOP)

XSTRING-WIDTH-TOO-WIDE
        ((M-1) SUB M-1 A-2)  ;Don't count the char that pushed over the max.
XSTRING-WIDTH-9
        (POPJ-AFTER-NEXT (PDL-TOP) DPB M-I
          Q-POINTER (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))
       ((M-T) DPB M-1 Q-POINTER (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))

;;;??? LOSES LIKE AR-1.  What does that mean?

;; Decode last three args for %STRING-SEARCH and other things.
XSTRING-SEARCH-DECODE
        (trap-unless-fixnum c-pdl-buffer-pointer :argument 3)
        ((M-C) Q-TYPED-POINTER C-PDL-BUFFER-POINTER-POP)
        (trap-unless-fixnum c-pdl-buffer-pointer :argument 2)
        ((M-I) Q-TYPED-POINTER C-PDL-BUFFER-POINTER)
        (JUMP-GREATER-OR-EQUAL M-I A-C xstring-dec1)
        (CALL XAR-1)            ;pops index into M-Q, array into M-ARRAY-POINTER.
    (ERROR-TABLE ARG-POPPED 0 PP PP M-I M-C)
        (POPJ)

xstring-dec1  ;start past end, but try to keep stack straight anyway.
        (popj-after-next (m-q) q-pointer c-pdl-buffer-pointer-pop)
       ((m-garbage) c-pdl-buffer-pointer-pop)

XSTRING-SEARCH (MISC-INST-ENTRY %STRING-SEARCH-CHAR)
                ;Arguments are character, array, start index, end index
        (CALL XSTRING-SEARCH-DECODE)
    (ERROR-TABLE CALLS-SUB %STRING-SEARCH-CHAR)
        (JUMP-GREATER-OR-EQUAL M-I A-C XFALSE)  ;args already off stack.
        ((M-J) DPB M-ZERO Q-ALL-BUT-TYPED-POINTER A-ALPHABETIC-CASE-AFFECTS-STRING-COMPARISON)
        ((M-3) DPB M-MINUS-ONE (LISP-BYTE %%CH-CHAR) A-ZERO)
        (JUMP-EQUAL M-J A-V-NIL XSTRING-SEARCH-NOT-FONT)  ;Jump if ignore font.
        ((M-3) DPB M-MINUS-ONE Q-POINTER A-ZERO)
XSTRING-SEARCH-NOT-FONT
        ((M-1) AND C-PDL-BUFFER-POINTER-POP A-3)
        (JUMP-NOT-EQUAL M-J A-V-NIL XSTRING-SEARCH-3)
        (JUMP-GREATER-THAN M-1 (A-CONSTANT 172) XSTRING-SEARCH-3)
        (JUMP-LESS-THAN M-1 (A-CONSTANT 101) XSTRING-SEARCH-3)
;M-I initial subscript, M-C initial upper bound
;M-Q subscript, M-S upper bound after array-indirect
;M-1 character searching for, M-B array type, M-E array data base
;The loop is 27-34 cycles per character.  It could be bummed to be better
;but this is still much faster than macrocode.
XSTRING-SEARCH-1
        (CALL-XCT-NEXT XCHAR-EQUAL-1-2)
       ((M-2) Q-POINTER M-T)
        (JUMP-EQUAL-XCT-NEXT M-T A-V-TRUE XSTRING-SEARCH-4)     ;Return if found it
       ((M-I) ADD M-I (A-CONSTANT 1))           ;Advance subscripts
        ((M-Q) ADD M-Q (A-CONSTANT 1))
        (JUMP-GREATER-OR-EQUAL M-I A-C XFALSE)  ;Reached upper bound, return NIL
        (CALL-GREATER-OR-EQUAL M-Q a-array-length array-trap)
    (ERROR-TABLE SUBSCRIPT-OOB M-Q m-array-length)
        (dispatch-call-xct-next (lisp-byte %%array-type-field) m-array-header array-type-ref-dispatch)
       (NO-OP)
        (JUMP XSTRING-SEARCH-1)

;;; This loop is for when we are not searching for a letter.
;;; Time is reduced to 13 cycles per character.
XSTRING-SEARCH-2
        (JUMP-GREATER-OR-EQUAL M-I A-C XFALSE)  ;Reached upper bound, return NIL
        (CALL-GREATER-OR-EQUAL M-Q a-array-length array-trap)
    (ERROR-TABLE SUBSCRIPT-OOB M-Q m-array-length)
        (DISPATCH-CALL-XCT-NEXT (LISP-BYTE %%ARRAY-TYPE-FIELD) m-array-header ARRAY-TYPE-REF-DISPATCH)
XSTRING-SEARCH-3
       ((M-I) ADD M-I (A-CONSTANT 1))
        ((M-2) AND M-T A-3)
        (JUMP-NOT-EQUAL-XCT-NEXT M-1 A-2 XSTRING-SEARCH-2)
       ((M-Q) ADD M-Q (A-CONSTANT 1))
;Found it.  Return the index before array-indirect, which has been incremented past.
XSTRING-SEARCH-4
        (POPJ-AFTER-NEXT NO-OP)
       ((M-T) SUB M-I (A-CONSTANT 1))

XSTRING-EQUAL (MISC-INST-ENTRY %STRING-EQUAL)
        ;Arguments are the two strings (which must really be strings),
        ;the two starting indices (which must be fixnums), and the
        ;number of characters to compare.  If this count is a fixnum, it
        ;is the number of characters to compare; if this runs off the end
        ;of either string, they are not equal (no subscript-oob error occurs).
        ;However, it won't work to have the starting-index greater than the
        ;the length of the array (it is allowed to be equal).
        ;If this count is NIL, the string's lengths are gotten via array-active-length.
        ;Then if the lengths to be compared are not equal, the strings are not
        ;equal, otherwise they are compared.  This takes care of the most common
        ;cases, but is not the same as the STRING-EQUAL function.
        ;Only the %%CH-CHAR field is compared.  There are no "case shifts".
        ((M-J) Q-TYPED-POINTER C-PDL-BUFFER-POINTER-POP)        ;Get count argument (typed)
                (ERROR-TABLE RESTART XSTRING-EQUAL)
        (trap-unless-fixnum c-pdl-buffer-pointer :argument 3)
        (CALL-XCT-NEXT XAAIXL)                  ;Get second string's length and decode array
       ((M-Q) Q-POINTER C-PDL-BUFFER-POINTER-POP)       ;Index into second string
        (CALL-NOT-EQUAL M-array-rank (A-CONSTANT 1) array-trap)
            (ERROR-TABLE ARRAY-NUMBER-DIMENSIONS M-D 1 m-array-pointer)
        ((M-C) SUB M-T A-Q)                     ;First string's subrange length (typed)
        (CALL-IF-BIT-SET (LISP-BYTE %%ARRAY-DISPLACED-BIT) m-array-header decode-displaced-array)
        ((M-I) M-Q)                             ;Save parameters of second string
        ((M-K) m-array-origin)
        ((M-ZR) M-array-header)
        (trap-unless-fixnum c-pdl-buffer-pointer :argument 1)
        (CALL-XCT-NEXT XAAIXL)                  ;Get first string's length and decode array
       ((M-Q) Q-POINTER C-PDL-BUFFER-POINTER-POP)       ;Index into first string
        (CALL-NOT-EQUAL M-array-rank (A-CONSTANT 1) array-trap)
            (ERROR-TABLE ARRAY-NUMBER-DIMENSIONS M-D 1 m-array-pointer)
        ((M-T) SUB M-T A-Q)                     ;First string's subrange length (typed)
        (call-if-bit-set (lisp-byte %%array-displaced-bit) m-array-header decode-displaced-array)
        (JUMP-EQUAL M-J A-V-NIL XSTRING-EQUAL-2)        ;Jump if no count supplied
        (trap-unless-fixnum m-j :argument 4)
        (JUMP-GREATER-THAN M-J A-C XFALSE)      ;If count exceeds either array,
        (JUMP-GREATER-THAN M-J A-T XFALSE)      ; then the answer is NIL.
        ((M-C) Q-POINTER M-J)                   ;Number of chars to be compared
XSTRING-EQUAL-0 ;No bounds-checking required beyond this point
        (JUMP-EQUAL M-C A-ZERO XTRUE)           ;If no characters to compare, result is T
        ((M-C) ADD M-Q A-C)                     ;Highest location to reference in first str
XSTRING-EQUAL-1 ;This is the character-comparison loop (27-39 cycles/char)
        (dispatch-call-xct-next (lisp-byte %%array-type-field) m-array-header array-type-ref-dispatch)
       ((A-BIDIV-V1) M-Q)
        ((A-BIDIV-V2) m-array-origin)
        ((M-1) Q-POINTER M-T)   ;Character from first string
        ((M-Q) M-I)
        (dispatch-call-xct-next (lisp-byte %%array-type-field) m-zr array-type-ref-dispatch)
       ((m-array-origin) M-K)
        (CALL-XCT-NEXT XCHAR-EQUAL-1-2)
       ((M-2) Q-POINTER M-T)    ;Character from second string
        ((m-array-origin) A-BIDIV-V2)   ;Be sure to put M-ARRAY-ORIGIN back even (especially)
                ;if going to exit!!!  Failure to do this can cause the array caching stuff
                ;to lose grossly!!!.
        (POPJ-EQUAL M-T A-V-NIL)                ;Chars not equal => strings not equal
        ((M-Q) M+A+1 M-ZERO A-BIDIV-V1)
        (JUMP-LESS-THAN-XCT-NEXT M-Q A-C XSTRING-EQUAL-1)
       ((M-I) ADD M-I (A-CONSTANT 1))           ;All chars equal => strings equal
        (POPJ)                                  ;M-T already has A-V-TRUE in it

XSTRING-EQUAL-2
        (JUMP-EQUAL-XCT-NEXT M-T A-C XSTRING-EQUAL-0)   ;If lengths same,
       ((M-C) Q-POINTER M-C)                    ; compare that many,
        (JUMP XFALSE)                           ; else return NIL

;;; (%sxhash-substring string mask start end)
xsxhash-substring (misc-inst-entry %sxhash-substring)
        ((m-j) q-typed-pointer pdl-pop) ;END, NIL means active length
        ((m-q) q-pointer pdl-pop)       ;START
        ((m-2) q-pointer pdl-pop)       ;mask
        ((m-1) setz)                    ;HASH accumulator
        (call xaaixl)           ;pop array, decode, active length in M-T
        (jump-equal-xct-next m-j a-v-nil xsxhash-substring-1)
       ((m-i) q-pointer m-t)    ;LENGTH
        ((m-i) q-pointer m-j)
xsxhash-substring-1
        (call-if-bit-set (lisp-byte %%array-displaced-bit) m-array-header decode-displaced-array)
        (jump-greater-or-equal m-q a-array-length xsxhash-substring-done-1)
        (jump-greater-than m-i a-array-length xsxhash-substring-done-1)

;M-Q has index to fetch from.
;M-I has index of first element not included in hash.
;M-1 has hash code so far.
;M-B has array header.
;M-2 has mask to AND with each character.
xsxhash-substring-loop
        (dispatch-call-xct-next (lisp-byte %%array-type-field) m-array-header array-type-ref-dispatch)
    (error-table bad-array-type m-array-header)
       (no-op)
        ((m-tem) and m-t a-2)
        ((m-1) xor m-1 a-tem)
        ;; Rotate the pointer field of M-1 left 7 bits.
        ;; We don't care what happens to the all-but-pointer.
        ((m-1) dpb m-1 (byte-field 24. 7) a-zero)
        ((m-q) add m-q (a-constant 1))
        (jump-less-xct-next m-q a-i xsxhash-substring-loop)
        ;; Uses 24. rather than Q-POINTER-WIDTH so that hash codes
        ;; are independent of pointer width.
       ((m-1) ldb (byte-field 7 24.) m-1 a-1)
xsxhash-substring-done
        (jump-if-bit-clear (byte-field 1 23.) m-1 xsxhash-substring-done-1)
        ((m-1) xor m-1 (a-constant 40000001))
xsxhash-substring-done-1
        (popj-xct-next)
       ((m-t) dpb m-1 (byte-field 24. 0) (a-constant (byte-value q-data-type dtp-fix)))

XSXHASH-STRING (MISC-INST-ENTRY %SXHASH-STRING)
        ((M-2) Q-POINTER PDL-POP)
        (CALL XAAIXL)           ;Pop array, decode it, put active length in M-T.
        ((M-I) Q-POINTER M-T)
        ((M-Q) A-ZERO)
        (CALL-IF-BIT-SET (LISP-BYTE %%ARRAY-DISPLACED-BIT) M-array-header decode-displaced-array)
        ((M-1) A-ZERO)          ;M-1 accumulates the hash.
        (JUMP-GREATER-OR-EQUAL M-Q A-I XSXHASH-STRING-DONE)
;M-Q has index to fetch from.
;M-I has array length.
;M-1 has hash code so far.
;M-B has array header.
;M-2 has mask to AND with each character.
XSXHASH-STRING-LOOP
        (DISPATCH-CALL-XCT-NEXT (LISP-BYTE %%ARRAY-TYPE-FIELD) M-array-header ARRAY-TYPE-REF-DISPATCH)
    (ERROR-TABLE BAD-ARRAY-TYPE M-array-header)
       (NO-OP)
        ((M-TEM) AND M-T A-2)
        ((M-1) XOR M-1 A-TEM)
        ;; Rotate the pointer field of M-1 left 7 bits.
        ;; We don't care what happens to the all-but-pointer.
        ((M-1) DPB M-1 (BYTE-FIELD 24. 7) A-ZERO)
        ((M-Q) ADD M-Q (A-CONSTANT 1))
        (JUMP-LESS-XCT-NEXT M-Q A-I XSXHASH-STRING-LOOP)
        ;; Uses 24. rather than Q-POINTER-WIDTH so that hash codes
        ;; are independent of pointer width.
       ((M-1) LDB (BYTE-FIELD 7 24.) M-1 A-1)
XSXHASH-STRING-DONE
        (JUMP-IF-BIT-CLEAR (BYTE-FIELD 1 23.) M-1 XSXHASH-STRING-DONE-1)
        ((M-1) XOR M-1 (A-CONSTANT 40000001))
XSXHASH-STRING-DONE-1
        (POPJ-XCT-NEXT)
       ((M-T) DPB M-1 (BYTE-FIELD 24. 0) (A-CONSTANT (BYTE-VALUE Q-DATA-TYPE DTP-FIX)))

))
