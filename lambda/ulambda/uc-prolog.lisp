;-*- Mode:LISP; Base:8 -*-
;;; (C) Copyright 1983,1984,1985, Mats Carlsson, UPMAIL.

;;; Version for Lambda (release 2.1 or newer) and Explorer.

;;; Microcode for LM-Prolog.
;;; Potentially dangerous POPJs are marked with ***,
;;; since they could start a macro insn cycle.  However, the corresponding MISCs
;;; are pretty useless with D-IGNORE.

;;; Wired in assumptions on storage conventions:
;;; >> Fill-pointers are at offset -2 from the array pointer.
;;; >> Flavor instances look like <header word, instance vas...>

;;; Not using "carcdr-direct", since the calling conventions are slightly awkward.

(defconst uc-prolog '(
#+lambda (begin-pagable-ucode)
  (mc-linkage (;;the following are needed for A mem communication.
               a-unify-dispatch
               a-lmp-vector
               a-lmp-trail
               a-lmp-area
               a-lmp-mode
               ;;the following are needed because of LAST-ARG-IN-T properties
               lmp-cell
               lmp-reference
               lmp-dereference
               lmp-updl
               lmp-centry
               #-lexical lmp-invoke
               ;;the following are needed for our unify-dispatch-table
               lmp-pop-fail
               lmpu-x-x
               lmpu-instance-x
               lmpu-x-instance
               lmpu-var-x
               lmpu-x-var
               lmpu-var-var
               lmpu-var-list
               lmpu-list-var
               lmpu-list-list
               ))

;;Exit vector offsets.
    (assign lmpoff-unify 765)
    (assign lmpoff-occurs-in 766)
    (assign lmpoff-unify-term-with-template 767)
    (assign lmpoff-construct 770)
    (assign lmpoff-unify-term-with-term 771)
    (assign lmpoff-array-push-extend 772)
    (assign lmpoff-read-only-variable-flavor 773)
    (assign lmpoff-true 774)
    (assign lmpoff-false 775)
    (assign lmpoff-find-and-cache 776)
    (assign lmpoff-universe 777)

        (locality d-mem)
;;Tail-recursive calls.
        (start-dispatch 0 0)
d-lmp-mmjcall
        (mmjcall inhibit-xct-next-bit)          ;tail recursive call
        (end-dispatch)

;;%UNIFY-TERM-WITH-TEMPLATE
        (start-dispatch 3 0)                    ;PPSS 2503: what kind of occurrence.
d-lmp-unify
        (lmp-put-frame inhibit-xct-next-bit)    ;first occurrence
        (lmp-updl)                              ;subsequent occurrence
        (lmp-pop-succeed inhibit-xct-next-bit)  ;void occurrence
        (illop inhibit-xct-next-bit)
        (lmp-ro-unify-1 inhibit-xct-next-bit)   ; read-only first occurrence
        (lmp-ro-unify-2)                        ; read-only subsequent occurrence
        (lmp-ro-uvoid inhibit-xct-next-bit)     ; read-only void occurrence
        (illop inhibit-xct-next-bit)
        (end-dispatch)

;;%CONSTRUCT
        (start-dispatch 3 0)                    ;PPSS 2503: what kind of occurrence.
d-lmp-construct
        (lmp-put-cell-frame)                            ;first occurrence
        (lmp-get-frame inhibit-xct-next-bit)            ;subsequent occurrence
        (lmp-cell inhibit-xct-next-bit)                 ;void occurrence
        (illop inhibit-xct-next-bit)
        (lmp-ro-construct-1)                            ; read-only first occurrence
        (lmp-ro-construct-2 inhibit-xct-next-bit)       ; read-only subsequent occurrence
        (lmp-ro-construct-void inhibit-xct-next-bit)    ; read-only void occurrence
        (illop inhibit-xct-next-bit)
        (end-dispatch)

;;First occurrences.
        (start-dispatch 2 0)                    ;PPSS 2302: base reg ;PPSS 0023: offset
d-lmp-put-frame
        (lmp-put-vector)                        ;interpreter's vector
        (qstarg)        ;argument block
        (qstloc)        ;local block
        (#-lexical illop #+lexical lmp-xstore)  ;higher context.
        (end-dispatch)

;;Subsequent occurrences.
        (start-dispatch 2 0)                    ;PPSS 2302: base reg ;PPSS 0023: offset
d-lmp-get-frame
        (lmp-get-vector)                        ;interpreter's vector
        (qadarg1)       ;argument block
        (qadloc1)       ;local block
        (#-lexical illop #+lexical lmp-xload)   ;higher context.
        (end-dispatch)

;;Special va:s in A-MEM.
        (locality a-mem)
;a-unify-dispatch (0)           ;moved to UC-PARAMETERS, following A-FLOATING-ZERO.
;a-lmp-vector (0)
;a-lmp-trail (0)
a-lmp-area (0)
a-lmp-mode (0)

;;And now for the actual code.
        (locality i-mem)

         (misc-inst-entry occurs-in)
         ((m-a) q-typed-pointer pdl-pop)
         ((m-k) q-typed-pointer pdl-pop)

lmp-occurs-in
         (jump-data-type-not-equal m-a (a-constant (byte-value q-data-type dtp-list))
                                       lmp-occurs-in-atom)

lmp-occurs-in-list
        (call-xct-next lmp-carcdr)
       ((m-t) m-a)
        (jump-data-type-not-equal-xct-next m-a (a-constant (byte-value q-data-type dtp-list))
                                               lmp-occurs-in-simple-recurse)
        ((pdl-push) m-t)
#+lambda (jump-greater-than micro-stack-pntr-and-data (a-constant 20._24.)
                            lmp-occurs-in-slow-recurse) ;Ustack filling up?
#+exp    (jump-greater-than micro-stack-pntr (a-constant 10.)
                            lmp-occurs-in-slow-recurse)
         (jump-xct-next lmp-occurs-in-join)     ;No, proceed. (Should check stack frame too.)
        (call lmp-occurs-in-list)

lmp-occurs-in-atom
         ((m-t) a-v-nil)
         (popj-after-next popj-not-equal m-k a-a)
        ((m-t) a-v-true)

lmp-occurs-in-simple-recurse
         (call lmp-occurs-in-atom)

lmp-occurs-in-join                              ;Here with key in K and term on stack
         ((m-a) q-typed-pointer pdl-pop)
         (popj-not-equal m-t a-v-nil)
         (jump lmp-occurs-in)

lmp-occurs-in-slow-recurse
         ((pdl-push) m-k)
         (dispatch-call (i-arg lmpoff-occurs-in) d-call-exit-vector i-dont-chain)
         ((pdl-push) q-typed-pointer m-k (a-constant (byte-value q-cdr-code cdr-next)))
         ((pdl-push) q-typed-pointer m-a (a-constant (byte-value q-cdr-code cdr-nil)))
         (dispatch-call (i-arg 2) d-mmcall i-dont-chain)
         (jump-xct-next lmp-occurs-in-join)
        ((m-k) q-typed-pointer pdl-pop)

        (misc-inst-entry %reference)
        ((m-t) q-typed-pointer pdl-pop)

lmp-reference                                   ;Inviz. T if it is a variable.
       (popj-after-next
         popj-data-type-not-equal m-t (a-constant (byte-value q-data-type dtp-locative)))
       ((m-t) q-pointer m-t
        (a-constant (byte-value q-data-type dtp-external-value-cell-pointer)))

        (misc-inst-entry %dereference)
        ((m-t) q-typed-pointer pdl-pop)

lmp-dereference                                 ;Read T if it is a variable.
        (popj-data-type-not-equal m-t (a-constant (byte-value q-data-type dtp-locative)))
        ((vma-start-read) m-t)
        (check-page-read)
        (popj-after-next dispatch transport md)
       ((m-t) q-typed-pointer md)

(begin-comment) (end-comment)

lmp-cell
        (jump-not-equal m-t a-v-nil lmp-cell-named)
        (misc-inst-entry %cell0)                ;Make an unnamed value cell.

lmp-cell0
        ((m-b) (a-constant 1))                  ;Cons 1 word
        (call-xct-next allocate-list-storage)
       ((m-s) a-lmp-area)                       ;in *PROLOG-WORK-AREA*
        ((m-t vma) q-pointer m-t (a-constant (byte-value q-data-type dtp-locative)))
        ((md-start-write)
         q-typed-pointer m-t (a-constant (byte-value q-cdr-code cdr-nil)))
        (check-page-write)                      ;Store a self-reference.
        (popj-after-next)
       (gc-write-test)

lmp-cell-named
        ((pdl-push) m-t)
        (misc-inst-entry %cell)                 ;Make a named value cell.
        ((m-b) (a-constant 2))                  ;Cons 2 words
        (call-xct-next allocate-list-storage)
       ((m-s) a-lmp-area)                       ;in *PROLOG-WORK-AREA*
        ((m-t vma) q-pointer m-t (a-constant (byte-value q-data-type dtp-locative)))
        ((md-start-write) q-typed-pointer m-t (a-constant (byte-value q-cdr-code cdr-normal)))
        (check-page-write)                      ;Store a self-reference.
        (gc-write-test)
        ((vma) m+1 vma)
        ((md-start-write)
          q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-error)))
        (check-page-write)                      ;Store the name.
        (popj-after-next)
       (gc-write-test)

lmp-list
        ((m-b) m-r)                             ;Cons R words
        ((m-s) a-lmp-area)                      ;in *PROLOG-WORK-AREA*
        (jump-xct-next xlist0)
       (call allocate-list-storage)

lmp-list*
        ((m-b) m-r)                             ;Cons R words
        ((m-s) a-lmp-area)                      ;in *PROLOG-WORK-AREA*
        (jump-xct-next xlistr0)
       (call allocate-list-storage)

        (misc-inst-entry %unify-term-with-template)

lmp-utemplate                                   ;Here with template in K.
        ((m-k) q-typed-pointer pdl-pop)
        (call-data-type-equal-xct-next pdl-top (a-constant (byte-value q-data-type dtp-locative))
                                       qcar3)
       ((m-t) q-typed-pointer pdl-pop)
        ((pdl-push) m-t)

lmp-utemplate-nd                                ;Here if we don't need to dereference.
        (jump-equal m-k a-v-nil lmp-unil)
        (jump-data-type-equal m-k (a-constant (byte-value q-data-type dtp-list))
                                  lmp-uconstruct)
        (jump-data-type-equal m-k (a-constant (byte-value q-data-type dtp-locative))
                                  lmp-uconstant)
        (call-data-type-equal-xct-next m-k (a-constant (byte-value q-data-type dtp-symbol))
                                           lmp-symeval-a)
       ((m-a) m-k)
        (dispatch (byte-field 3 25) m-a d-lmp-unify)    ;XCT-NEXT only if subsequent occ.
       (call lmp-get-frame-d)

lmp-uconstant
;Possible bum:
;       (jump-xct-next lmp-updl)
;      (open-qcar m-k)
        ((m-t) m-k)
        (jump-xct-next lmp-updl)
       (call qcar3)

lmp-symeval-a
        ((vma-start-read) m+1 m-a)
        (check-page-read)
        (dispatch transport md)
        (popj-after-next (m-t) m-a)
       ((m-a) q-typed-pointer md)

lmp-uconstruct                                  ;X & construct
        ((m-t) m-k)
        (jump-data-type-not-equal pdl-top (a-constant (byte-value q-data-type dtp-list))
                                  lmp-uconstruct-1)
        (call lmp-carcdr)                       ;List & construct.
        ((m-k) m-a)                             ;Recurse, pushing cdrs.
        ((m-c) m-t)
        (call-xct-next lmp-carcdr)
       ((m-t) q-typed-pointer pdl-pop)
        ((pdl-push) m-t)
        ((pdl-push) m-c)
        (jump-equal-xct-next m-k a-v-nil lmp-unil-then-join)
       ((pdl-push) m-a)
#+lambda (call-greater-than micro-stack-pntr-and-data (a-constant 20._24.)      ;(Should check frame too.)
                            lmp-utemplate-slow-recurse)
#+lambda (call-less-or-equal micro-stack-pntr-and-data (a-constant 20._24.) lmp-utemplate-nd)
#+exp    (call-greater-than micro-stack-pntr (a-constant 10.) lmp-utemplate-slow-recurse)
#+exp    (call-less-or-equal micro-stack-pntr (a-constant 10.) lmp-utemplate-nd)
lmp-uconstruct-join
        (jump-not-equal m-t a-v-nil lmp-utemplate)
        ;drop thru and fail otherwise

lmp-pop2-fail                                   ;Pop two and fail.
        (popj-after-next (m-t) seta pdl-pop a-v-nil)
       (pdl-pop)

lmp-utemplate-slow-recurse                      ;MMcall %unify-term-with-template.
        (dispatch-call PDL-POP (i-arg lmpoff-unify-term-with-template) d-call-exit-vector i-dont-chain)
        ((pdl-push) q-typed-pointer m-a (a-constant (byte-value q-cdr-code cdr-next)))
        ((pdl-push) q-typed-pointer m-k (a-constant (byte-value q-cdr-code cdr-nil)))
        (dispatch (i-arg 2) d-lmp-mmjcall i-dont-chain)

lmp-uconstruct-1                                ;Atom & construct.
        (jump-data-type-not-equal pdl-top (a-constant (byte-value q-data-type dtp-locative))
                                  lmp-uconstruct-2)
        ;Variable & complex term.  Construct it and bind.
        (call lmp-construct-t)
        (jump-xct-next lmpu-var-list)
       ((m-i) a-lmp-mode)

lmp-uconstruct-2
        (jump-data-type-not-equal pdl-top (a-constant (byte-value q-data-type dtp-instance))
                                  lmp-pop-fail)
        ;Instance & complex term.  Construct it and send :unify msg.
        (jump-xct-next lmpu-instance-x)
       (call lmp-construct-t)

        (misc-inst-entry %construct)
        ((m-t) q-typed-pointer pdl-pop)
        (POPJ-EQUAL M-T A-V-NIL)
lmp-construct-t
        (jump-data-type-equal m-t (a-constant (byte-value q-data-type dtp-list))
                              lmp-construct-list)
lmp-construct-not-list
        (jump-data-type-equal m-t (a-constant (byte-value q-data-type dtp-locative))
                              qcar3)
        ((m-a) m-t)
        (call-data-type-equal-xct-next m-t (a-constant (byte-value q-data-type dtp-symbol))
                                       lmp-symeval-a)
       ((m-t) a-v-nil)
        (dispatch (byte-field 3 25) m-a d-lmp-construct)       ;XCT-NEXT if first occ.
       ((pdl-push) m-a)

lmp-construct-list                              ;Construct the cons sitting in T.
#+lambda (jump-greater-than micro-stack-pntr-and-data (a-constant 20._24.)
                            lmp-construct-slow-recurse) ;Chicken out?
#+exp    (jump-greater-than micro-stack-pntr (a-constant 10.)
                            lmp-construct-slow-recurse) ;Chicken out?
        (call-xct-next lmp-construct-list-rest)
       ((pdl-push) (a-constant (byte-value q-data-type dtp-fix)))       ;Push count first.
        (jump-equal-xct-next m-t a-v-nil lmp-list)      ;LIST* the elements.
       ((m-r) q-pointer pdl-pop)                ;R long.
        (jump-data-type-not-equal m-t (a-constant (byte-value q-data-type dtp-locative))
                                  lmp-construct-exit)
        ((m-t) q-pointer m-t
             (a-constant (byte-value q-data-type dtp-external-value-cell-pointer)))
lmp-construct-exit
        ((pdl-push) m-t)
        (jump-xct-next lmp-list*)
       ((m-r) m+1 m-r)

lmp-construct-list-rest
        (call lmp-carcdr)
        ((pdl-push) m-t)
        ((m-t) m-a)
        (jump-equal m-t a-v-nil lmp-construct-list-cdr)
        (call lmp-construct-t)
        ;Invz element if need be
        (jump-data-type-not-equal m-t (a-constant (byte-value q-data-type dtp-locative))
                                  lmp-construct-list-cdr)
        ((m-t) q-pointer m-t
             (a-constant (byte-value q-data-type dtp-external-value-cell-pointer)))

lmp-construct-list-cdr
        ((m-k) q-typed-pointer pdl-pop)         ;pop rest of template
        ((m-s) q-typed-pointer pdl-pop)         ;pop count
        ((pdl-push) m-t)                        ;push element
        ((pdl-push) m+1 m-s)                    ;push incf count
        (popj-equal-xct-next m-k a-v-nil)
       ((m-t) m-k)
        (jump-data-type-not-equal m-t (a-constant (byte-value q-data-type dtp-list))
                                  lmp-construct-not-list)
        ((pdl-buffer-index) sub pdl-buffer-pointer a-ap)        ;Stack frame filling up?
        (jump-less-or-equal pdl-buffer-index (a-constant 370) lmp-construct-list-rest)
        ;drop thru if frame is filling up

lmp-construct-slow-recurse
        ((m-k) m-t)                             ;CALL-EXIT-VECTOR clobbers M-T...
        (dispatch-call (i-arg lmpoff-construct) d-call-exit-vector i-dont-chain)
        ((pdl-push) q-typed-pointer m-k (a-constant (byte-value q-cdr-code cdr-nil)))
        (dispatch (i-arg 1) d-lmp-mmjcall i-dont-chain)

lmp-put-frame                                   ;Store PDL-POP in local frame and succeed.
        ((m-t) q-typed-pointer pdl-top)
        ((micro-stack-data-push) (a-constant (i-mem-loc lmp-pop-succeed)))
        (dispatch-xct-next (byte-field 2 23) m-a d-lmp-put-frame)
       ((m-1) (byte-field 23 0) m-a)

lmp-put-cell-frame                              ;Store a cell into local frame and return it.
        (call lmp-cell)
        ((m-a) pdl-pop)                                ;Was pushed at LMP-CONSTRUCT-LIST -1.
        (dispatch-xct-next (byte-field 2 23) m-a d-lmp-put-frame)
       ((m-1) (byte-field 23 0) m-a)

lmp-get-frame-d                                 ;Dereference after doing...
        ((micro-stack-data-push) (a-constant (i-mem-loc lmp-dereference)))
lmp-get-frame                                   ;Get from local frame into T
        (dispatch-xct-next (byte-field 2 23) m-a d-lmp-get-frame)
       ((m-1) (byte-field 23 0) m-a)

lmp-put-vector                                  ;Store in interpreter's vector.
        (call-xct-next qrar3)
       ((m-s) add m-1 a-lmp-vector)
        (popj-after-next (m-t) q-typed-pointer md)
       (no-op)

lmp-get-vector                                  ;Read from interpreter's vector.
        (jump-xct-next qcar3)
       ((m-t) add m-1 a-lmp-vector)

lmp-ro-construct-void                           ;Make read-only void.
        (jump-xct-next lmp-ro-allocate)
       (call lmp-cell)

lmp-ro-construct-1                              ;Make read-only 1st occ.
        (jump-xct-next lmp-ro-allocate)
       (call lmp-put-cell-frame)

lmp-ro-construct-2                              ;Make read-only 2nd occ.
        (call lmp-get-frame-d)
        (popj-data-type-not-equal m-t (a-constant (byte-value q-data-type dtp-locative)))
        ;drop thru if we are a variable

;;(make-instance-in-area *prolog-work-area* read-only-variable ':cell <T>)
lmp-ro-allocate
        ((pdl-push) m-t)
        ((m-b) (a-constant 2))                  ;Cons 2 words
        ((m-a) (a-constant 2))
        (call-xct-next allocate-structure-storage)
       ((m-s) a-lmp-area)                       ;in *PROLOG-WORK-AREA*
        (dispatch-call (i-arg lmpoff-read-only-variable-flavor) d-read-exit-vector i-dont-chain)
        ((write-memory-data)
         q-pointer md (a-constant (byte-value q-data-type dtp-instance-header)))
        ((vma-start-write m-t)
         q-pointer m-t (a-constant (byte-value q-data-type dtp-instance)))
        (check-page-write)
        (gc-write-test)
        ((md) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-nil)))
        ((vma-start-write) add m-t (a-constant 1))
        (check-page-write)
        (popj-after-next)
       (gc-write-test)

lmp-ro-uvoid                                    ;Unify with read-only void.
        (jump-data-type-not-equal pdl-top (a-constant (byte-value q-data-type dtp-locative))
                                  lmp-pop-fail)
        (jump-xct-next lmpu-var-x)
       (call lmp-ro-construct-void)

lmp-ro-unify-1                                  ;Unify with read-only 1st occ.
        (jump-data-type-not-equal pdl-top (a-constant (byte-value q-data-type dtp-locative))
                                  lmp-pop-fail)
        (jump-xct-next lmpu-var-x)
       (call lmp-ro-construct-1)

lmp-ro-unify-2                                  ;Unify with read-only 2nd occ.
        (jump-data-type-not-equal pdl-top (a-constant (byte-value q-data-type dtp-locative))
                                  lmp-ro-unify-nonvar)
        (jump-xct-next lmpu-var-x)
       (call-data-type-equal m-t (a-constant (byte-value q-data-type dtp-locative))
                                 lmp-ro-allocate)

lmp-ro-unify-nonvar                             ;NV&V  fail, NV&NV  unify
        (jump-data-type-not-equal m-t (a-constant (byte-value q-data-type dtp-locative))
                                  lmp-updl)
        (jump lmp-pop-fail)


lmp-unil-then-join
        ((micro-stack-data-push) (a-constant (i-mem-loc lmp-uconstruct-join)))
lmp-unil                                        ;Unify PDL with NIL.
        ((pdl-top m-tem) q-typed-pointer pdl-top)
        (jump-equal m-tem a-v-nil lmp-pop-succeed)
        (jump-data-type-equal-xct-next pdl-top
                                           (a-constant (byte-value q-data-type dtp-locative))
                                           lmpu-var-x)
       ((m-t) a-v-nil)
        (jump-data-type-equal pdl-top
                                  (a-constant (byte-value q-data-type dtp-instance))
                                  lmpu-instance-x)
        ;All other cases fail...

lmp-pop-fail                                    ;Pop one and fail.
        (popj-after-next (m-t) a-v-nil)
       (pdl-pop)

        (misc-inst-entry %unify-term-with-term)
        ((m-t) q-typed-pointer pdl-pop)

lmp-updl                                        ;Here to unify PDL with T.
        ((m-s pdl-top) q-typed-pointer pdl-top) ;Make S=PDL
        (jump-equal m-s a-t lmp-pop-succeed)
        ;Start of less strange code, as suggested by khs.
        ((m-i) a-lmp-mode output-selector-leftshift-1)

lmp-updl-1                                      ;Unify S=PDL with T and ST.
                                                ;M-I has mode bits when recursing,
                                                ;gets bit 1 in bit 2 the first time.
        ((m-1) q-data-type m-s)
        ((m-2) q-data-type m-t)            ;could save a cycle if table were on
        ((m-3) dpb m-1 (byte-field 5 5) a-2)    ; 0 modulo 2000...
        ((vma-start-read) add m-3 a-unify-dispatch)
        (check-page-read)                  ;no transport since it's static
        ((m-i) selective-deposit m-i (byte-field 1 2) a-lmp-mode)
        ((oa-reg-low) dpb md oal-jump a-zero)
        (jump 0)

lmpu-x-x                                        ;Chicken out to EQUAL.
        ((pdl-push) m-t)
        (jump xequal)

lmpu-var-var                                    ;Variable & variable.
        (jump-xct-next lmpu-var-x)
       ((m-t) q-pointer m-t
        (a-constant (byte-value q-data-type dtp-external-value-cell-pointer)))

lmpu-list-var                                   ;List & variable.
        ((pdl-top) m-t)
        ((m-t) m-s)
        ;drop thru

lmpu-var-list                                   ;Variable & list.
        (call-if-bit-set (byte-field 1 0) m-i lmpu-var-list-check)
        ;drop thru

lmpu-var-x
        (call-xct-next qrar3)
       ((m-s) q-typed-pointer pdl-pop)

        (call-not-equal-xct-next m-array-pointer a-lmp-trail decode-1d-array-uncached)
        ((m-array-pointer) dpb m-zero q-cdr-code a-lmp-trail)
        ((vma-start-read) sub m-array-pointer (a-constant 2))
        (check-page-read)
        ;;m-array-pointer is not generally transported
        (dispatch transport md)
        ((m-q) q-pointer md)
        (jump-greater-or-equal-xct-next m-q a-array-length lmpu-var-x-ov)       ;Check overflow.
       ((m-k) m-t)
        ((m-t md-start-write) m+1 md)           ;Bump fill-pointer.
        (check-page-write)
        ((vma) add m-array-origin a-q)
        ((md-start-write) m-k)                  ;Store trail-item.
        (check-page-write)
        (gc-write-test)
        (popj)          ;this can be D-IGNORE!

lmpu-var-x-ov                                   ;Overflow handling.
        (dispatch-call (i-arg lmpoff-array-push-extend) d-call-exit-vector i-dont-chain)
        ((pdl-push) a-lmp-trail)                ;Don't worry about cdr code...
        ((pdl-push) q-typed-pointer m-k (a-constant (byte-value q-cdr-code cdr-nil)))
        (dispatch (i-arg 2) d-lmp-mmjcall i-dont-chain)

lmpu-x-var                                      ;Non-variable & variable.
        ((pdl-top) m-t)
        (jump-xct-next lmpu-var-x)
       ((m-t) m-s)

lmpu-x-instance                                 ;Non-variable & instance.
        ((pdl-top) m-t)
        ((m-t) m-s)
        ;drop thru

lmpu-instance-x                                 ;Instance & non-variable.
        (call-xct-next p3zero)          ;Escape to a :UNIFY method.
       ((m-s) pdl-pop)
        ((pdl-push) m-s)
        (dispatch-call (i-arg lmpoff-unify) d-read-exit-vector i-dont-chain)
        ((pdl-push) q-typed-pointer md (a-constant (byte-value q-cdr-code cdr-next)))
        ((pdl-push) q-typed-pointer m-t (a-constant (byte-value q-cdr-code cdr-nil)))
        (dispatch (i-arg 2) d-lmp-mmjcall i-dont-chain)

lmpu-list-list                                  ;Here to unify two lists.
         (call-if-bit-set (byte-field 1 2) m-i lmpu-unbind-later)
         ((m-c) m-s)                            ;C gets first arg,
         (call-xct-next lmp-carcdr)             ;A and T get its parts.
        ((m-d) m-t)                             ;D gets second arg,
         ((m-b) m-a)                            ;B and S=TOP get its parts.
         ((m-s pdl-top) m-t)
         (call-xct-next lmp-carcdr)
        ((m-t) m-c)
         (jump-equal m-a a-b lmpu-equal-cars)
         (jump-equal m-s a-t lmpu-equal-cdrs)
         ((pdl-push) m-t)
         ((pdl-push m-s) m-a)                           ;Recurse on cars.
         (call-if-bit-set-xct-next (byte-field 1 1) m-i lmpu-llh-bind)
        ((m-t) m-b)
#+lambda (call-greater-than micro-stack-pntr-and-data (a-constant 20._24.)
                lmpu-slow-recurse)
#+lambda (call-less-or-equal micro-stack-pntr-and-data (a-constant 20._24.)
                lmp-updl-1)
#+exp    (call-greater-than micro-stack-pntr (a-constant 10.)
                lmpu-slow-recurse)
#+exp    (call-less-or-equal micro-stack-pntr (a-constant 10.)
                lmp-updl-1)
         (jump-equal m-t a-v-nil lmp-pop2-fail)
         ((pdl-buffer-index) sub pdl-buffer-pointer (a-constant 1))     ;Dereference cdrs.
         (call-data-type-equal-xct-next c-pdl-buffer-index
                                        (a-constant (byte-value q-data-type dtp-locative))
                                        qcar3)
        ((m-t) c-pdl-buffer-index)
         ((m-s c-pdl-buffer-index) m-t)
         (call-data-type-equal-xct-next pdl-top
                                        (a-constant (byte-value q-data-type dtp-locative))
                                        qcar3)
        ((m-t) q-typed-pointer pdl-pop)
         (jump-not-equal-xct-next m-s a-t lmp-updl-1)   ;Iterate on cdrs if .
        ((m-i) a-lmp-mode)
         ;drop thru if cdrs eq.

lmp-pop-succeed
         (popj-after-next (m-t) a-v-true)
        (pdl-pop)

lmpu-slow-recurse
         ((m-a) pdl-pop)                                ;First arg pushed already.
         ((m-b) m-t)
         (dispatch-call (i-arg lmpoff-unify-term-with-term) d-call-exit-vector i-dont-chain)
         ((pdl-push) q-typed-pointer m-a (a-constant (byte-value q-cdr-code cdr-next)))
         ((pdl-push) q-typed-pointer m-b (a-constant (byte-value q-cdr-code cdr-nil)))
         (dispatch (i-arg 2) d-lmp-mmjcall i-dont-chain)

lmpu-equal-cars                                         ;Just iterate.
         (jump-equal m-s a-t lmp-pop-succeed)
         (jump-xct-next lmp-updl-1)
        (call-if-bit-set (byte-field 1 1) m-i lmpu-llh-bind)

lmpu-equal-cdrs                                         ;Just iterate.
         ((m-t) m-a)
         ((pdl-top m-s) m-b)
         (jump-xct-next lmp-updl-1)
        (call-if-bit-set (byte-field 1 1) m-i lmpu-llh-bind)

lmpu-var-list-check
        ((m-k) pdl-top)
        (call-xct-next lmp-occurs-in-list)
       ((pdl-push m-a) m-t)
        (popj-equal-xct-next m-t a-v-nil)
       ((m-t) q-typed-pointer pdl-pop)
        (jump-xct-next lmp-pop-fail)       ;Occur check succeeded, cause failure.
       (micro-stack-data-pop)

lmpu-llh-bind   ;Bind the first cons to a RPLACD-FORWARD to the second cons.
         ((pdl-push) m-t)
         ((pdl-push) q-pointer m-c (a-constant (byte-value q-data-type dtp-locative)))
         ((m-t) q-pointer m-d (a-constant (byte-value q-data-type dtp-rplacd-forward)))
         (jump-xct-next poptj)
        (call xbind1)

lmpu-unbind-later                          ;Save specPDL index and unwind before returning.
         ((m-3) a-qlbndp)
         ((pdl-top) q-pointer m-3 (a-constant (byte-value q-data-type dtp-locative)))
         ((pdl-push) m-s)
         (popj-after-next (m-i) a-lmp-mode)     ;Turn off M-I<2>.
        ((micro-stack-data-push) (a-constant (i-mem-loc xunbind-to-index)))

lmp-carcdr                                      ;A := car(T), T := cdr(T)
          ((vma-start-read) m-t)
          (check-page-read)
          (dispatch transport md)
#+lambda (micro-fault-ok-here)
          (jump-not-equal-xct-next vma a-t qcdr3)       ;Full cdr if the car was forwarded
         ((m-a) q-typed-pointer md)
          (dispatch q-cdr-code md cdr-cdr-dispatch)     ;XCT-NEXT on CDR-NEXT
         ((m-t) add vma (a-constant 1))

        (misc-inst-entry %untrail)
;;Scratchpad registers used here:
;;PDL-TOP is MARK
;;M-I is transported array-pointer
;;M-K is *TRAIL*
;;M-E is 1+last trail item address
        ((pdl-top) q-typed-pointer pdl-top)

lmp-untrail-restart                             ;Initialize.
        (call-not-equal-xct-next m-array-pointer a-lmp-trail decode-1d-array-uncached)
        ((m-array-pointer) dpb m-zero q-cdr-code a-lmp-trail)
        ((vma-start-read) sub m-array-pointer (a-constant 2))
        (check-page-read)
        ;;m-array-pointer is not generally transported
        (dispatch transport md)
        ((m-i) add vma (a-constant 2))
        ((m-k) q-typed-pointer md)
        ((m-e) add m-array-origin a-k)

lmp-untrail-loop
        (jump-greater-or-equal pdl-top a-k lmp-pop-fail)
        ((m-k md) sub m-k (a-constant 1))       ;*TRAIL* := *TRAIL*-1
        ((vma-start-write) sub m-i (a-constant 2))
        (check-page-write)
        ((m-e vma-start-read) sub m-e (a-constant 1))   ;Get next item.
        (check-page-read)
        (dispatch transport md)                 ;Transport to newspace.
        ((m-t) q-typed-pointer md)
        ((md-start-write) a-v-nil)              ;Clobber item with NIL.
        (check-page-write)
        (jump-data-type-not-equal m-t (a-constant (byte-value q-data-type dtp-locative))
                                  lmp-untrail-invoke)
        ((vma-start-read) m-t)
        (check-page-read)                       ;Transporting done already, just want the cdr code.
        ((md-start-write) selective-deposit md q-cdr-code a-t)
        (check-page-write)
        (jump-xct-next lmp-untrail-loop)
       (gc-write-test)

lmp-untrail-invoke
        (jump-xct-next lmp-untrail-restart)     ;Re-initialize after invoking,
       (call lmp-invoke)                        ; since it can have side-effects.

#-lexical       (misc-inst-entry %invoke)
;; This is as #-LEXICAL (apply (car x) (cdr x)) #+LEXICAL (funcall x),
;; but checks for TRUE and FALSE first, which are frequent cases in continuations.
#-lexical       ((m-t) q-typed-pointer pdl-pop)

lmp-invoke

#-lexical (call-data-type-not-equal m-t (a-constant (byte-value q-data-type dtp-list))
                                    illop)
#-lexical       (call lmp-carcdr)               ;A := fctn, T := args.
#-lexical       (dispatch-call (i-arg lmpoff-true) d-read-exit-vector i-dont-chain)
#-lexical       ((md) q-typed-pointer md)
#-lexical       (jump-equal md a-a xtrue)               ;Succeed if fctn is TRUE.
#-lexical       (dispatch-call (i-arg lmpoff-false) d-read-exit-vector i-dont-chain)
#-lexical       ((md) q-typed-pointer md)
#-lexical       (popj-equal md a-a)                     ;Fail if fctn is FALSE.
#-lexical       ((pdl-push) m-a)
#-lexical       (jump-xct-next uaply)                   ;APPLY otherwise.
#-lexical      ((pdl-push) m-t)

#+LEXICAL       (call p3zero)
#+LEXICAL       ((pdl-push) m-t)
#+LEXICAL       (dispatch (i-arg 0) d-lmp-mmjcall i-dont-chain)


        (misc-inst-entry %current-entrypoint)
        ((m-t) q-typed-pointer pdl-pop)
lmp-centry
        ((vma-start-read) m-t)  ;K := definitions alist.
        (check-page-read)
        (dispatch transport md)
        ((m-k) q-typed-pointer md)
        (dispatch-call (i-arg lmpoff-universe) d-read-exit-vector i-dont-chain) ;B := *universe*.
        ((m-b) q-typed-pointer md)
        (jump-equal m-k a-v-nil lmp-nodef)      ;Any definitions at all?
        ((vma-start-read) m-k)                  ;K := cache item.
        (check-page-read)
        (dispatch transport md)
        (call-xct-next lmp-carcdr)              ;A := latest universe, T := defn.
       ((m-t) q-typed-pointer md)
        (jump-equal-xct-next m-b a-a qcar3)

lmp-nodef
       ((m-a) q-typed-pointer pdl-pop)          ;A := predicator
        (dispatch-call (i-arg lmpoff-find-and-cache) d-call-exit-vector i-dont-chain)
        ((pdl-push) q-typed-pointer m-k (a-constant (byte-value q-cdr-code cdr-next)))  ;defs
        ((pdl-push) q-typed-pointer m-b (a-constant (byte-value q-cdr-code cdr-next)))  ;worlds
        ((pdl-push) q-typed-pointer m-a (a-constant (byte-value q-cdr-code cdr-nil)))   ;name
        (dispatch-call (i-arg 3) d-mmcall i-dont-chain)
        (jump qcar)
; Micro-Paging crock doesn't like the following.  -- should be OK now.
;       (jump-xct-next qcar)                    ;Full CAR since arg may become NIL.
;       (dispatch-call (i-arg 3) d-mmcall i-dont-chain)

;;STORE IN LOCAL BLOCK
QSTLOC
        (POPJ-AFTER-NEXT (PDL-BUFFER-INDEX) ADD M-1 A-LOCALP)
       ((C-PDL-BUFFER-INDEX) M-T)
;;STORE IN ARGUMENT BLOCK
QSTARG
        (POPJ-AFTER-NEXT (PDL-BUFFER-INDEX) ADD M-AP A-1 ALU-CARRY-IN-ONE)
       ((C-PDL-BUFFER-INDEX) M-T)
;;STORE IN HIGHER CONTEXT
#+LEXICAL       lmp-xstore
#+LEXICAL        ((pdl-push) dpb m-zero (byte-field 5 23) a-a)
#+LEXICAL        (jump xstore-in-higher-context)

;;REF LOCAL BLOCK
QADLOC1
        (POPJ-AFTER-NEXT (PDL-BUFFER-INDEX) ADD M-1 A-LOCALP)
       ((M-T) Q-TYPED-POINTER C-PDL-BUFFER-INDEX)
;;REF ARGUMENT BLOCK.  CANNOT BE INVISIBLE POINTER.
QADARG1
        (POPJ-AFTER-NEXT (PDL-BUFFER-INDEX) ADD M-AP A-1 ALU-CARRY-IN-ONE)
       ((M-T) Q-TYPED-POINTER C-PDL-BUFFER-INDEX)
;;LOAD FROM HIGHER CONTEXT
#+LEXICAL       lmp-xload
#+LEXICAL        ((pdl-push) dpb m-zero (byte-field 5 23) a-a)
#+LEXICAL        (jump xload-from-higher-context)

#+lambda (end-pagable-ucode)
        ))
