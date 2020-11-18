;;;   -*- Mode:LISP; Package:COMPILER; Base:8; Readtable:ZL -*-

;-- for now, how to load the cross-compiler:
; 1. (make-system 'compiler :compile)
; 2. (load "sys:zwei;comc")    ;this loads the new version of zwei:compile-defun-internal, etc.
; 3. compile from editor buffer, dj:l.k;cross  and dj:l.k;cross-p2.
; 4. to turn on compiler debuging printout, do (nc:debug-on :post and/or :pre).

;-- new revision:
#|
NO WAY STATIONS..  code is to be generated the "right way" at the extreme bottom of the tree,
  as opposed to compiled into a temp, the futz with multiple-values or frame-flushing, then
  return the temp, etc.
the primary mechanism use to achieve this is "the closure", on *open-frames*, which is passed
  messages telling it what we want to do.

The Closure is required to preserve the hardware m.v. flag.

messages to the closure:
  :EXIST        sent when instruction which causes frame to exist at runtime is emitted.
  :DISCARD      used when thru with temporary-storage-frames (ie p2values-for-k)
                also when doing a GO out from under evaluated args.
  :RETURN       Used when passing a value through a frame, yielding a value to a destination.
                However, the frame is discarded, not completed as for the NIL message.
                Hairy; see below:
  NIL           normal completion. (open-frame operation dest)  Hairy, see below:

NIL and :RETURN receive the following arguments:

    Frame       The frame-entry on *open-frames*; like SELF.
    Operation   :RETURN, that is, this operation.
    Dest        Where to put the result.  Except for the last one to be removed, this will
                be a temporary global register.
    Source      Where to get the value.  A given frame can leave the value in the source,
                iff that source will be unaffected by discarding this frame.
    SType       A source-type flag.  Some frame types will look at this (i.e. THROW),
                and others will pass on to the recursive call to OUTI-FOR-K.

The value from the NIL or :RETURN operation is left either in Dest or
Source.  Which one is indicated by the return value -- it will be the
source for the next step.

Source-type flags passed to OUTI-FOR-K when the instruction yielding a final
value for some destination is output.  The Source-type flag will be one of
the following:


  NIL           -- Some unknown number of values is computed, and the hardware m.v. flag
                   is not set up.
  :SINGLE-VALUE -- A single value is computed, and the hardware m.v. flag is not set up.
  :SINGLE-VALUE-FLAG
                -- A single value is computed, and the hardware m.v. flag is cleared.
                   I'm not sure this is usefully distinct from :SINGLE-VALUE.
  :SUBR-VALUE   -- A function was called, which will set up the hardware m.v. flag to indicate
                   whether multiple values were returned.
  :MULTIPLE-VALUES
                -- A VALUES form (or other open-compiled m.v. producing form) produced values,
                   but the hardware m.v. flag is not set up.
  :MULTIPLE-VALUES-FLAG
                -- Like :MULTIPLE-VALUES, but the m.v. flag is set up.
  :LAST-VALUE   -- Multiple values were prepared, distributed by a higher level, and
                   Now this one (the first one) is passed along to activate the
                   open frames and other hair, and eventually be placed where it belongs.


OUTI-CLOSE-FOR-K takes optionally takes a Source and a Source-Type,
passes them to the closure in question.

OUTI-FOR-K optionally takes a Source-Type.  This will be supplied
when generating the instruction which produces the final result.  OUTI-FOR-K will
pass this on if it recurses, and will pass it on to CLEAN-UP-OPEN-FRAMES.  (The default
will be :SINGLE-VALUE).  When OUTI-FOR-K gets a compound destination, it simplifies
it:
  If it's a PROGDESC:
    If the PROGDEST-OPEN-FRAMES is the same as *OPEN-FRAMES*, the PROGDEST-IDEST is substituted
      in the instruction, and we're done.
    If the PROGDEST-OPEN-FRAMES is not in *OPEN-FRAMES*, we have an error situation.
    Otherwise:
      It substitutes the PROGDEST-IDEST, or K:R0 if the PROGDEST-IDEST is not
        an A register or D-IGNORE, and outputs the instruction.
      It calls CLEAN-UP-OPEN-FRAMES with the source being either K:R0 or the destination,
        depending on which was used above.  The Source-Type is passed on.
      CLEAN-UP-OPEN-FRAMES will have moved it into the destination.

  If it's a MULTIPLE-VALUES, it will:
    Case Source-type:
      (:SINGLE-VALUE :SINGLE-VALUE-FLAG):
        Output the instruction to store into the first destination.
        Output instructions to store NIL's into the additional destinations.
      (:SUBR-VALUE):
        Output the instruction, to store into the first destination.
        Output code to test the hardware bit, and store the values or NIL's into the additional
          destinations.
      (:VALUES):
        Output the instruction, to store into the first destination.
        The hardware bit is not set, but there are multiple values.  Copy from the
          global return registers into the additional destinations.

  If it's an OPEN-FRAME:
    Output the instruction, with a destination from the IDEST of the OPEN-FRAME.
    Call OUTI-CLOSE-FOR-K, with the IDEST as source as passing on the Source-type.

CLEAN-UP-OPEN-FRAME takes the Source given, and:

  For each open frame to be popped, except the last, it calls the closure with Source = SOURCE
  and Dest = the global return-temp register.  The return value becomes the new SOURCE.

  For the last open frame to be popped, the Dest = the Dest passed into CLEAN-UP-OPEN-FRAMES.
  If the return value is Dest, we're all done.  If it isn't, a move must is done.

(multiple-value-setq (a b c) (cond ((foo)))) would work as follows:
  * MULTIPLE-VALUE-SETQ's P2 would create a multiple-value destination, and call
    P2-FOR-K on (cond ((foo)))
  * (:PROPERTY COND P2-FOR-K) would call P2 on (FOO) and K:R0.
  * After braching, it would do a MOVE from K:R0 to the destination.  The Source-type would
    be :SINGLE-VALUE,

(throw 'foo (foo)) would work as follows:

* P2THROW-FOR-K would create an open frame with its own closure.
* 'FOO would be compiled to K:O0.
* The PDEST of the open frame would be set to K:O1.
* The IDEST of the open frame would be set to D-IGNORE, since we don't return.
  (Normally it would be the destination given P2THROW-FOR-K).
* P2THROW-FOR-K would call P2-FOR-K with (FOO) and a destination of the open frame.
* (Eventually, inside P2ARGC-FOR-K) OUTI-CLOSE-FOR-K will be called with
  Operation  = NIL, Destination = open-frame, Source-type = :SUBR-VALUES.
* OUTI-CLOSE-FOR-K will invoke P2ARGC-FOR-K's closure for FOO.  It will pass on Source-type.
* P2ARGC-FOR-K's closure will do OUTI-FOR-K of a call with a destination of the THROW open frame.
  It will pass on Source-type.
* OUTI-FOR-K will see that it has a compound, and simplify it.  In the case of an OPEN-FRAME,
  this means:
* OUTI-FOR-K will output the call instruction for FOO, with a destination of K:O1 (from the
  PDEST of the OPEN-FRAME).
* OUTI-FOR-K will call OUTI-CLOSE-FOR-K with a source of K:O1, a destination of D-IGNORE (from
  the IDEST of the open frame), and a Source-type passed on.
* OUTI-CLOSE-FOR-K will pass those on to P2THROW-FOR-K's closure.
* P2THROW-FOR-K's closure will output the appropriate code based on the Source-type.

|#


#|

Logical Destinations. Valid arguments to everything from P2-FOR-K to OUTI-FOR-K.
(OUTI-FOR-K also allows the use of all valid K: destinations).

Symbols:

D-IGNORE   -- Value is to be ignored.
D-RETURN   -- Value is to be returned from the function.

K:NOP      -- Value is discarded, but the indicators are set.

K:O0-K:O15 -- Place the result in this K register.  Used for compiler temporaries and arguments
              to functions being called.
K:A0-K:A15 -- Same.  Used for local variables and arguments to this function.
K:R0-K:R15 -- Same.  Also, K:R0 is used to set the indicators for conditionals, and
              as an intermediate.

GR:xxx     -- General Register, by name.

Otherwise  -- A special variable.  I would rather this were a structure, but P1 would have
              to create that.


Lists:

(K:REGISTER name block index)
           -- Same as GR:name.
(K:NEW-OPEN n)
           -- Put the result of this into a new OPEN frame, in (O-N n).
(K:NEW-TAIL-OPEN n)
           -- Put the result of this into a new TAIL-OPEN frame, in (O-N n).

Structures:

PROGDESC:  Has the following relevant slots:
  IDEST    -- Destination for this block.
  OPEN-FRAMES
           -- Tail of *OPEN-FRAMES*, indication how far to discard to.

MULTIPLE-VALUES:  Has the following slots:
  VALUES   -- A list of destinations.
  OPEN-FRAME
           -- NIL, or an open frame to be activated when this is output.

OPEN-FRAME: Has the following relevant slots:
  PDEST    -- Where to put the value this frame needs.  (New)
  IDEST    -- Where to put the value when this frame is done.  (New)
  CLEANUP-GENERATOR
           -- Function to generate the appropriate cleanup code.  This is the part
              that does the interesting work.  It will be called with an operation of
              NIL, and it can make decisions about what function to call depending on
              the Source-type argument.  This is useful for THROW, MULTIPLE-VALUE-LIST,
              MULTIPLE-VALUE-BIND, MULTIPLE-VALUE-CALL, MULTIPLE-VALUE-SETQ, and any other
              multiple value receivers.  (This open frame should also live on *OPEN-FRAMES*,
              if it involves a O-frame, as THROW does.  They may as well ALL live on
              *OPEN-FRAMES*).

VAR:       Has the following relevant slots:
  LAP-ADDRESS
           -- Where to store it.

NEW-VAR:   Has the following relevant slots:
  VAR      -- The VAR struct that this storing will create & initialize the home for.
  OPEN-FRAME
           -- NIL, or an open frame to be activated when this is output.

|#


;This file has modifications to P2 of the compiler for cross compiling on K.

;cross compilation switches and conditionalization.
;  *target-computer*    can be 'lambda or 'k
;    this is the main switch which indicates we are in fact cross compiling.
;    the macro COMPILER-TARGET-SWITCH makes conditional calls based on this switch.  It
;    is also looked at directly.
;  *fasd-interface*     can be 'lambda-fasd-interface or nlisp-fasd-interface
;    the macro COMPILER-FASD-SWITCH makes conditional calls based on this switch.

;the P2-FOR-K property.
;  when cross compiling, P2F will look for this before looking for P2 or QINTCMP properties.
;    When found, property is treated identically to how P2 property would be treated.

;general:
; MISC instructions have no meaning on the K.  Therefore, P2MISC-for-k just calls P2ARGC-for-k (which
;  compiles calls to ordinary functions) rather compiling a different sort of call.
; Since the K is (more or less) a register based machine, all references to D-PDL (etc) have to
;  go away, more or less.

; Destinations:
;  the destinations D-NEXT, D-LAST, and D-PDL basically do not apply to the K.  They
;may be OK if the code will be converted by the cross compiler, however.
; D-LAST is completely gone now.  --RWK
;  D-INDS is equivalent to K:R0, and therefore is flushed.


;optimizations for the future:
;  Introduce "h" pseudo-registers.  These really address the hardware "A" registers, but
;in addition, guarantee the peep-hole optimizer the value will be "used" exactly once.
;Thus,  (move h1 foo),   (move bar h1)  can be optimized, if possible, into (move bar foo)
;without worrying there might be another (move ble h1) comming along later.

(DEFUN testp1 (form)
  (LET ((*outer-context-vars* nil)
        (*outer-context-local-functions* nil)
        (*OUTER-CONTEXT-FUNCTION-ENVIRONMENT* nil)
        (*OUTER-CONTEXT-PROGDESC-ENVIRONMENT* nil)
        (*OUTER-CONTEXT-GOTAG-ENVIRONMENT* nil)

        (MAXPDLLVL 0)                           ;deepest lvl reached by local pdl
        (PDLLVL 0)                              ;Runtine local pdllvl
        ;; p2 things
        (*CALL-BLOCK-PDL-LEVELS*)               ;used only in lambda mode.
        (*open-frames* nil)                     ;used in cross compile mode.
        (*WITHIN-CATCH*)
        (*WITHIN-POSSIBLE-LOOP*)
        (*DROPTHRU* T)                          ;Can drop in if false, flush stuff till tag or
        (*TAGOUT*)

        (ALLGOTAGS)
        (*TLEVEL* T)
        (*P1VALUE* T)                           ;Compiling for all values
        (*BINDP* NIL)                           ;%BIND not yet used in this frame

        (*VARS* ())
        (*ALLVARS* ())
        (*FREEVARS* ())
        (*LOCAL-FUNCTIONS* nil)
        (*FUNCTION-ENVIRONMENT* nil)
        (*PROGDESC-ENVIRONMENT* nil)
        (*GOTAG-ENVIRONMENT* nil)
        (TLFUNINIT (not (eq *target-computer* 'lambda-interface)))  ;crosscompiling, use FEF-INI-COMP-C not fef initialization.
        (*SPECIALFLAG*)
        (*LOCAL-MAP* ())                        ;names of local variables
        (*ARG-MAP* ())                          ;names of arguments
        (*BASE-STACK-SLOTS* ())                 ;aux-slots in cross-compile mode.
        (*STACK-SLOTS* ())                      ;currently existing stack-slots in cross-compile mode.
        (*LOCAL-FUNCTION-MAP* ())               ;names of local functions
        (*FAST-ARGS-POSSIBLE* T)
        (*BREAKOFF-COUNT* 0)                    ;no internal functions yet
        (*LEXICAL-CLOSURE-COUNT* 0)
        (*lexical-ref-code-name-alist* ())
        (MACROS-EXPANDED)                       ;List of all macros found in this function,
                                                ; for the debugging info.
        (SELF-FLAVOR-DECLARATION (cdr (assq :self-flavor local-declarations)))
        (*SELF-REFERENCES-PRESENT* NIL)         ;Bound to T if any SELF-REFs are present
        (LOCAL-DECLARATIONS LOCAL-DECLARATIONS) ;Don't mung ouside value
        (INHIBIT-SPECIAL-WARNINGS INHIBIT-SPECIAL-WARNINGS)
        (barf-special-list nil)
        (this-function-barf-special-list nil)
        (*CLOBBER-NONSPECIAL-VARS-LISTS* ())
        (*placeholder-function-number* 0)
        (*placeholder-alist* nil)
        (compiler-queue nil)                    ;Compiler queue
        )
    (apply #'values (p1 form) compiler-queue)))

zwei:
(DEFCOM COM-CROSSCOMPILE-REGION "Crosscompile the current region or defun.
If there is a region, it is compiled.
Otherwise, the current or next defun is compiled." ()
  (COMPILE-DEFUN-INTERNAL (or (get-buffer-compiler *interval*) T)
                          (if *numeric-arg-p* "Cross-and-downloading" "Crosscompiling")
                          (if *numeric-arg-p* "Cross-and-downloaded" "Crosscompiled.")
                            NIL ;USE-TYPEOUT
                            NIL ;DEFVAR-HACK
                            '(:MODE COMPILER:MACRO-COMPILE)     ;generating-micro-compiler-input-p
                                        ;will wind up getting set however.
                            'COMPILER:K                         ;*target-computer*
                            (if *numeric-arg-p*
                                'COMPILER:NLISP-DOWNLOAD-FASD-INTERFACE
                              'COMPILER:K-DUMMY-FASD-INTERFACE))        ;*fasd-interface*
  DIS-NONE)

;k target computer interface
(defprop k fdefine-for-k fdefine)
(defprop k peep-for-k peep)
(defprop k qlapp-for-k qlapp)
(defprop k p2sbind-for-toplevel-for-k p2sbind-for-toplevel)
(defprop k assign-lap-addresses-for-k assign-lap-addresses)
(defprop k var-compute-init-for-k var-compute-init)
(defprop k p2-for-k p2)

(defun fdefine-for-k (function-spec definition &optional carefully-flag no-query-flag)
  (declare (ignore carefully-flag no-query-flag))
  (format t "~%Fdefine ~s ~s" function-spec definition)
  nil)

(defun peep-for-k (peep-code-array &optional function-name)
  (declare (ignore function-name))
  ;(grind-top-level (g-l-p peep-code-array))
  ;  (format t "~%PEEP ~s ~s" (g-l-p peep-code-array) function-name)
  nil)

(defun qlapp-for-k (fctn lap-mode)
  ;;(format t "~%QLAPP ~s ~s" fctn lap-mode)
  (multiple-value-bind (name instructions entry-points function-type)
      (cross-compile fctn 'store)               ;or 'print
    (let-if (not (variable-boundp nc:*debug-stream*))
            ((nc:*debug-stream* standard-output))
      (format nc:*debug-stream* "~%Entries ~s. ~%" entry-points)
      (setq instructions (nreverse instructions))
      (nc:debug :pre
        (format nc:*debug-stream* "~%Preprocessed instructions:~%")
        (nc:print-instructions instructions))
      (let ((code (nc:post-process (nreverse instructions))))
        (nc:debug :post
          (format nc:*debug-stream* "~%Postprocessed instructions:~%")
          (nc:print-instructions code nc:*debug-stream*)
          ;;(grind-top-level code)
          )
        (let ((nc-function-defstruct (nc:assemble-instruction-list name code entry-points)))
          (nc:debug :postx
            (format nc:*debug-stream* "~%Postprocessed instructions:~%")
            (nc:print-instructions code nc:*debug-stream* (nc:ncompiled-function-code nc-function-defstruct)))
          (cond ((eq lap-mode 'qfasl)
                 (fasd-k-compiled-function nc-function-defstruct))
                ((eq lap-mode 'compile-to-core)
                 (compiler-fasd-switch (fasd-function-defstruct nc-function-defstruct))))))
        )))

;k-dummy-fasd-interface
(defprop k-dummy-fasd-interface k-dummy-fasd-file-property-list fasd-file-property-list)
(defprop k-dummy-fasd-interface k-dummy-fasd-function-defstruct fasd-function-defstruct)

(defun k-dummy-fasd-file-property-list (plist)
  (declare (ignore plist))
  nil)

(defun k-dummy-fasd-function-defstruct (function-defstruct)
  (declare (ignore function-defstruct))
  nil)



;nlisp-download-fasd-interface
(defprop nlisp-download-fasd-interface nlisp-download-fasd-file-property-list fasd-file-property-list)
(defprop nlisp-download-fasd-interface nlisp-download-fasd-function-defstruct fasd-function-defstruct)

(defun nlisp-download-fasd-file-property-list (plist)
  (declare (ignore plist))
  nil)

(defun nlisp-download-fasd-function-defstruct (function-defstruct &optional (stream k-kbug:*kfasl-stream*))
  (let-globally ((k-kbug:*k-pausing* t))
    (let ((stopped? (k-kbug:kbug-stopped?)))
      (unless stopped?
        (k-kbug:kbug-stop)
        (k-kbug:kbug-wait-until-stopped))
      (send stream :reload-info)
      (k-kbug:kbug-cmd-raw k-k2:kbug-command-fasl-stream)
      (k-fasdump:fasd-compiled-function function-defstruct stream)
      (k-fasdump:fasd-eof stream)
      (send stream :force-output)
      (if (null stopped?)
          (k-kbug:kbug-proceed)))
    ;(process-sleep 1000)
    ))

;k system interfacing routines.

;internal that tell the compiler what it is trying to do.
(defconst *internal-return-destinations* '(d-return d-return-single d-return-multiple-value d-return-tail))
;these can actually appear in the destination of a K:MOVE instruction, etc.
(defconst *return-destinations* '(k:return k:return-tail k:return-mv
                                  k:return-i k:return-i-mv k:return-i-tail
                                  ))

(defconst *frame-registers-used-for-argument-passing* 16.)      ;will be less when lexical env problem addressed.

(defun o-n (n)
  (aref #(k:o0 k:o1 k:o2 k:o3 k:o4 k:o5 k:o6 k:o7 k:o8 k:o9 k:o10
           k:o11 k:o12 k:o13 k:o14 k:o15)
        n))

(defun a-n (n)
  (aref #(k:a0 k:a1 k:a2 k:a3 k:a4 k:a5 k:a6 k:a7 k:a8 k:a9 k:a10
               k:a11 k:a12 k:a13 k:a14 k:a15)
        n))

(defun a-register-p (x)
  (memq x '(k:a0 k:a1 k:a2 k:a3 k:a4 k:a5 k:a6 k:a7 k:a8 k:a9 k:a10
            k:a11 k:a12 k:a13 k:a14 k:a15)))

(defun r-n (n)
  (aref #(k:r0 k:r1 k:r2 k:r3 k:r4 k:r5 k:r6 k:r7 k:r8 k:r9 k:r10
               k:r11 k:r12 k:r13 k:r14 k:r15)
        n))

(defun k-destination-p (dest)
  (and (symbolp dest)
       (nc:register-number dest)))


;;; This is how you tell if a destination really means D-IGNORE.
(defun d-ignore-p (dest)
  (etypecase dest
    (symbol (or (eq dest 'd-ignore) (eq dest 'k:ignore)))
    (list nil)
    (multiple-values
     (or (null (multiple-values-values dest))
         (cl:every #'d-ignore-p (multiple-values-values dest))))
    (progdesc
     (d-ignore-p (progdesc-idest dest)))
    (open-frame
     (and (d-ignore-p (open-frame-pdest dest))
          (d-ignore-p (open-frame-idest dest))))))

;;; This is how you tell if a destination really means D-RETURN.
(defun d-return-p (dest)
  (etypecase dest
    (symbol (or (memq dest *return-destinations*)
                (memq dest *internal-return-destinations*)))
    (list nil)
    ;; MULTIPLE-VALUES are only used when we have more to do.
    (multiple-values nil)
    (progdesc
     (d-return-p (progdesc-idest dest)))
    ;; OPEN-FRAMEs are only used when we have more to do.
    (open-frame
     (d-return-p (open-frame-idest dest)))))



;;; Convert a destination to a source.  (This works on compiler-level destinations,
;;; not hardware-level ones).  Signals an error if given a D-RETURN, for obvious
;;; reasons
;;; This returns a new destination which must be used instead of the original one.
;;; The source is the second value.

(defun convert-dest-to-source (dest)
  (declare (values new-dest source))
  (etypecase dest
    (symbol
     (case dest
       (d-ignore (values 'k:r0 'k:r0))
       (d-return
        (error "Cannot convert D-RETURN into a source."))
       (otherwise (values dest dest))))
    (list
     (error "Cannot convert a NEW-OPEN-style destination into a source."))
    (multiple-values
     (let ((subdest (or (first (multiple-values-values dest)) 'd-ignore)))
       (multiple-value-bind (new-subdest source)
           (convert-dest-to-source subdest)
         (if (eq subdest new-subdest)
             (values dest source)
           (setq dest (copy-multiple-values dest))
           (setf (multiple-values-values dest)
                 (list* new-subdest (rest (multiple-values-values dest))))
           (values dest source)))))
    (progdesc
     (let ((subdest (progdesc-idest dest)))
       (multiple-value-bind (new-subdest source)
           (convert-dest-to-source subdest)
         (if (eq subdest new-subdest)
             (values dest source)
           (setq dest (copy-progdesc dest))
           (setf (progdesc-idest dest) new-subdest)
           (values dest source)))))
    (open-frame
     (let ((subdest (open-frame-idest dest)))
       (multiple-value-bind (new-subdest source)
           (convert-dest-to-source subdest)
         (setq dest (copy-open-frame dest))
         (setf (open-frame-idest dest) new-subdest)
         (values dest source))))))


;(defconst *k-constant-registers* '( (0 . gr:*zero) (1 . gr:*one*) (-1 . gr:*minus-one*)
;                                   (2 . gr:*two*) (nil . gr:*nil*) (t . gr:*t*)
;                                   (3 . gr:*three*) (4 . gr:*four*) (5 . gr:*five*)
;                                   (6 . gr:*six*) (7 . gr:*seven*) (8 . gr:*eight*)
;                                   (9 . gr:*nine*) (10. . gr:*ten*)))
;(defun k-ref-constant-frame (const)
;  (let ((tem (assq const *k-constant-registers*)))
;    (if tem `(register ))))

(defun k-find-constant-register (quan)
  (let ((probe (cl:assoc quan
                         nc:*global-constants*
                         :test #'equal)))
    (when probe
      (second (cdr probe)))))

;;; This finds the return register to use for value N.  (Value 0 is handled specially).
(defun k-find-return-register (index)
  ;; We really ought to rename the registers 1-30.  --RWK
  (when (< index 1)
    (error "Illegal return register ~D." index))
  (when (> index 30.)
    (error "Cannot return ~D values." (1+ index)))
  (svref #(gr:*return-0*  gr:*return-1*  gr:*return-2*  gr:*return-3*  gr:*return-4*
           gr:*return-5*  gr:*return-6*  gr:*return-7*  gr:*return-8*  gr:*return-9*
           gr:*return-10* gr:*return-11* gr:*return-12* gr:*return-13* gr:*return-14*
           gr:*return-15* gr:*return-16* gr:*return-17* gr:*return-18* gr:*return-19*
           gr:*return-20* gr:*return-21* gr:*return-22* gr:*return-23* gr:*return-24*
           gr:*return-25* gr:*return-26* gr:*return-27* gr:*return-28* gr:*return-29*)
         (1- index)))


(defvar *reg-error-enable* t)           ; Debugging switch.
(defun reg-error (&rest args)
  (declare (eh:error-reporter))
  (when *reg-error-enable*
    (apply #'fsignal args)))



;;; Some functions we just don't like to do tail calls on.  Provide a way to not
;;; do them, so people can debug.

(defvar *no-tail-call* nil)

(defun initialize-no-tail-call ()
  (unless *no-tail-call*
    (setq *no-tail-call* (make-hash-table :test #'eq))
    (setf (gethash 'li::error *no-tail-call*) t)
    (setf (gethash 'lisp:error *no-tail-call*) t)))

;;; I think this is how you arrange to be initialized after the system is loaded.
;;; In this case, we have to wait for the flavor system and hash tables to be loaded.
;;; If the once-only initialization list isn't run at the right point, we'll just
;;; have to find or create a list which is.
(add-initialization "Initialize no-tail-call table."
                    '(initialize-no-tail-call)
                    '(:once))

(defun tail-call-p (dest &optional fctn)
  (and (d-return-p dest)
       (null *specialflag*)
       (zerop *base-stack-slots*)
       (null *open-frames*)
       (or (null fctn)
           (null *no-tail-call*)                ;During system building
           (not (gethash fctn *no-tail-call*)))))

(defmacro tail-call-open (function tail-p cleanup-generator &optional source-type)
  `(outi-open-for-k (if ,tail-p `(k:tail-open) `(k:open)) ,tail-p ,cleanup-generator
                    ,source-type (if ,tail-p `(k:tail-open ,,function)
                                   `(k:open ,,function))))

(defun maybe-tail-call (function dest source-type tail-p)
  (if tail-p
      (outi-for-k `(k:tail-call ,function) source-type)
    (outi-for-k `(k:call ,function ,dest) source-type)))

(defun make-trivial-open-frame (tail-p)
  (labels ((error-to-call-or-discard (open-frame operation dest)
             (ignore open-frame dest)
             (case operation
               ((:exist)
                (cond ((open-frame-there-p open-frame)
                       (fsignal "Frame already there"))
                      (t (setf (open-frame-there-p open-frame) t))))
               (otherwise
                (error "Attempt to automatically ~A an open frame which should have been trivially handled."
                       (ecase operation
                         ((nil) "call")
                         ((:discard) "discard")
                         ((:return)
                          "return through")))))))
    (make-open-frame :open-instruction nil
                     :tail-p (not (null tail-p))
                     :there-p nil
                     :cleanup-generator #'error-to-call-or-discard)))

;;; NOTE-OPEN-FRAME is called from OUTI-FOR-K level.  If we haven't already been told
;;; what we're outputing (and hence, how it will be cleaned off the stack, or completed)
;;; then it's a trivial call, and our caller is generating a fixed code sequence.  Thus
;;; we just generate a frame that is cleaned off the stack by an explicit CALL.  If it
;;; is ever handled by the automatic mechanisms, an error will be signaled.

(defun note-open-frame (tail-p)
  (ignore tail-p)
  (cond ((null *open-frames*)
         (fsignal "No frame there"))
        (t (funcall (open-frame-cleanup-generator (car *open-frames*))
                    (car *open-frames*) :exist nil)))
  )

;;; A call was noted at the OUTI-FOR-K level.  This may or may not have been due to automatic
;;; mechanisms, but discard the now-vanished frame, verifying that what we did matched the
;;; OPEN for tailness.

(defun finish-open-frame (tail-p)
  (when (null *open-frames*)
    (error "Internal compiler error:  Over-pop of open frames."))
  (let ((old (pop *open-frames*)))
    (unless (eql tail-p (open-frame-tail-p old))
      (error "Internal compiler error:  Mismatch of tail-callness for OPEN and CALL."))))

(defun (:property list p1cross) (form)
  (let ((fn (case (length (cdr form))
              (0 'nil)
              (1 'li:ncons)
              (2 'li:list2)
              (3 'li:list3)
              (4 'li:list4)
              (t 'li:listn))))
    (p1 `(,fn . ,(cdr form)))))

(defun (:property list* p1cross) (form)
  (let ((fn (case (length (cdr form))
              (t 'li:list*))))
    (p1 `(,fn . ,(cdr form)))))

;pdl conventions:  never reference 0 or + index of stack pointer.
;  stack pointer points at an idle location.

;calling conventions on the K.
;args 1 to *frame-registers-used-for-argument-passing* in registers a0 to a15.
;Rest are on the stack.  The callee must pop these off prior to return.
;  -- there is this lossage where A15 is used to hold lexical environment, which will lose if
;   there are 16. or more args.  We inherit this from the Hardebeck environment, sigh.

;optional args:  must create entry for each number of supplied args.  This entry must initialize other
; optional args which are not supplied for this number of supplied args, etc.


;rest arg:  (presence of rest arg indicated as negative number of args on entry list.)
; NOTE: all rest arg entry points must start exactly with the two instruction standard sequence to
;       CONS-REST because APPLY-INTERNAL will enter with an already consed list 2 instructions past the normal entry point!
;  callee calls runtime routine CONS-REST (using special non-frame call).
;   a two instruction "link-code" loads the total number of args being passed in *arg-1*.
;   the callee loads the first one to cons in *arg-2*.
;  the value comes back in *VALUE-1*
;  if stack-args are consed, the pdl is actually popped.


;keyword args:
;  first treated like rest arg, including consing it up in *VALUE-1*
;  then call (GET-KEYWORD-ARG-VALUES <rest> <list of keywords> <allow-other-keys>)
;   this returns as multiple values the decoded values.
;      if a keyword is not present, it is returnned as 'keyword-garbage, and that is depended on to
;         decide whether to execute the initialization code for the variable, etc.
;  then (!) call MVBIND-n (which supplies NIL for returnned values expected but not supplied).
;       In this case this is a complete NO-OP!
;  load slots with values from *return-<n>* registers.

;special vars:  Bind each arg by (BIND <symbol> <value>).  Unbind by (UNBIND <number of slots>).
;  reference by (%symbol-value <symbol>). Set by (%%set <symbol> <value>).



;;; Bind a list of variables, computing initializations and binding sequentially.
;;; *VARS* are the *VARS* outside of this binding environment.
;;; NEWVARS are the *VARS* inside of it, starting with the variables in X in reverse order,
;;; except there may be additional entries for optional-specified-flags; each one
;;; will be on NEWVARS just before its corresponding main variable.
;;; We have to install these variables one at a time as we go, using successive tails.
;X is the lambda list.  The third arg to this call is always NIL.

;we compile a separate entry for each number of args, the entries for lessor numbers of args first.
; optional args:  general idea is code initializes unsupplied arg, then falls into code for arg supplied.
;  problem is: if code is run in generating arg, it must see special bindings involved in previous args.

;steps:  Compute entries required (# optional args + 1).  Then for each of them, test if individual code required.
;  individual code is required (for this entry) if:
;       (AND <there is an unsupplied optional arg which involves running code to initialize>
;            <any arg bound before that one is special>)

;"normally" the entries are "in order" to fall into each other.
;  i.e. (for 2) (0 sup) <init 1>, (1 sup) <init 2>, (2 sup).
; if the vars are special, each has to be bound and stored into.  The point is that we must do the b&s once and only once
;  for each var in each path.
;  i.e. (for 2) (0 sup) <init 1>, (1 sup) <b&s1> <init 2> <branch b&s2>, (2 sup) <b&s1> b&s2: <b&s2>
;  i.e. (for 3) (0 sup) <init 1>, (1 sup) <b&s1> <init 2> <branch b&s2>, (2 sup) <b&s1> b&s2: <b&s2> <init 3> <branch b&s3>
;               (3 sup) <b&s1> <b&s2> b&s3: <b&s3>
;  i.e. (for 4) (0 sup) <init 1>, (1 sup) <b&s1> <init 2> <branch b&s2>, (2 sup) <b&s1> b&s2: <b&s2> <init 3> <branch b&s3>
;               (3 sup) <b&s1> <b&s2> b&s3: <b&s3> <init 4> <branch b&s4>, (4 sup) <b&s1> <b&s2> <b&s3> b&s4: <b&s4>
;if the var in question is not special, things <b&s<n>> is null and things simplify:
;  i.e. (for 4) (0 sup) <init 1>, (1 sup) <init 2> <branch b&s2> (2 sup) b&s2: <init 3> <branch b&s3>
;               (3 sup) b&s3: <init 4> <branch b&s4>, (4 sup) b&s4:
;  then if branch .+1 is deleted, we get back to the "straightforward" case.
;if there are fef-arg-req args:
;  (for 2,2)    (2 sup) <b&s1> <b&s2> <init 3> <branch b&s3>
;               (3 sup) <b&s1> <b&s2> b&s3: <b&s3> <init 4> <branch b&s4>, (4 sup) <b&s1> <b&s2> <b&s3> b&s4: <b&s4>
;therefore:  we will depend on <branch *+1> being flushed elsewhere.

;-- for case where specified flags used things are more complicated--
;  i.e. (for 4) (0 sup) <init 1> <1 not-sup> <branch b&s1> (1 sup) <1 is-sup> b&s1: <b&s1S> <b&s1> <init 2> <2 not-sup>
;               <branch b&s2>, (2 sup) <b&s1> <1 is-sup> <b&s1S> <2 is-sup> b&s2: <b&s2S> <b&s2> <init 3> <3 not-sup>
;               <branch b&s3> (3 sup) <b&s1> <1 is-sup> <b&s1S> <2 is-sup> <b&s2S> <b&s2> <3 is-sup> b&s3: <b&s3S>
;               <b&s3> <init 4> <4 not-sup> <branch b&s4> (4 sup) <1 is-sup> <b&s1S> <b&s1> <2 is-sup> <b&s2S> <b&s2>
;               <3 is-sup> <b&s3S> <b&s3> <4 is-sup> b&s4: <b&s4S> <b&s4>

;if a SUPPLIED-P variable is special, we need a slot to hold the variable between when it is set and the B&S.
;we use R0 for this.  For this to win, the B&S for the supplied-p must immediately follow the setup, in particular,
;the B&S for the main variable must not intervene.

;entry prefix at entry:  if rest arg, REST-SEQUENCE
;  if <USES-PDL-SLOTS>,  <CREATE-STACK-SLOTS>

;the pre-slot of a variable:  normally the same as the slot.  but in the case of a special variable
; which doesnt have a slot (-aux or -internal-aux) use K:R0 to hold variable before bind occurs.

;only on call from QCOMPILE0 can there be FEF-ARG-REQ, FEF-ARG-OPT or FEF-ARG-REST.
(DEFUN p2sbind-for-toplevel-for-k (x NEWVARS *VARS*
                                &aux rest-varstruct n-specbinds nnewvars nreqargs noptargs nauxvars
                                     total-slots total-arg-slots slots-on-stack-p)
  (setq total-slots (setq n-specbinds (setq nnewvars (setq nreqargs (setq noptargs (setq nauxvars 0))))))
  (loop for l on newvars until (eq l *vars*)
        for v = (car l)
        as kind = (var-kind v)
        do (incf nnewvars)
        when (or (memq kind '(fef-arg-req fef-arg-opt fef-arg-rest))
                 (and (not (eq (var-type v) 'fef-special))
                      (memq kind '(fef-arg-aux fef-arg-internal-aux))))
        do (incf total-slots)
        when (memq 'variable-location (var-misc v))

        when (and (eq (var-type v) 'fef-special)
                  (memq kind '(fef-arg-req fef-arg-opt fef-arg-rest fef-arg-aux)))
        do (incf n-specbinds)
        when (memq kind '(fef-arg-rest fef-arg-key))
        do (setq rest-varstruct v)
        when (eq kind 'fef-arg-req)
        do (incf nreqargs)
        when (eq kind 'fef-arg-opt)
        do (incf noptargs)
        when (eq kind 'fef-arg-aux)
        do (incf nauxvars)
        )
  (setq total-arg-slots (+ nreqargs noptargs))
  (setq slots-on-stack-p (> total-slots *frame-registers-used-for-argument-passing*))
  (format t "~%Total-slots ~D, n-specbinds ~D, nreqargs ~D, noptargs ~D, nauxvars ~D, rest-varstruct ~S, slots-on-stack ~S"
          total-slots n-specbinds nreqargs noptargs nauxvars rest-varstruct slots-on-stack-p)
  (do ((entry-number nreqargs (1+ entry-number))
       (number-of-vars-supplied nreqargs (1+ number-of-vars-supplied))  ;also incremented below for supplied-p vars
       (b-and-s-tag))
      ((= entry-number (+ total-arg-slots 1)))
    (let* ((key-arg (if (>= entry-number total-arg-slots) nil (var-struct-for-slot entry-number x newvars))))
      ;(format t "~%KEY-ARG ~s " key-arg)
      (setq *dropthru* t)
      (outf-for-k `(entry ,(if rest-varstruct
                               (1- (minus entry-number))        ;rest arg itself counts!.
                             entry-number)))
                                                ;entry prefix
      (cond (rest-varstruct
        ;This is the rest header that all rest-arg entries must have.  Note that is necessary even if REST arg
        ; is to be ignored for possible stack-arg adjustment purposes.
             (outi-for-k `(k:movei gr:*arg-2* (quote ,entry-number) k:boxed))
             (outi-for-k `(k:move  gr:*return-pc-1* k:trap-pc+))
             (outj-for-k `(k:jump li:cons-rest))        ;rest arg returns  *VALUE-1*
             (setq *dropthru* t)        ;this is really a subroutine call, which the frob doesnt realize.
             ))
   ;create stack slots, if any.
      (let ((stack-slots-to-create
              (+ *base-stack-slots* (- (max 0 (- entry-number *frame-registers-used-for-argument-passing*))))))
        (if (> stack-slots-to-create 0)
            (let ((*stack-slots* *stack-slots*))
              ;; *** The *stack-slots* above is a conservative kludge, until we're sure that
              ;; *** the change to make generate-alloc-stack-slots increment *STACK-SLOTS* is safe here.
              (generate-alloc-stack-slots stack-slots-to-create)
              (outi-open-for-k-internal 'initial-bind nil t (make-dealloc-stack-slots stack-slots-to-create)))))


      ;; Create closure environment, if any.  Do it here so we can close over the REST argument
      ;; Don't do it before the call to LI:CONS-REST, since that would mess up the argument frame
      ;; for the REST hacking.
      ;; *** This brings up the issue of LI:CONS-REST being faked out by the environment, self, and
      ;; *** mapping-table arguments.  We either need 4 versions of LI:CONS-REST, or always limit
      ;; *** the number of args passed in registers to 12.  The latter seems not so unreasonable,
      ;; *** since I think nearly any function with that many arguments is going to turn them into
      ;; *** a &REST list, anyway.

      (create-closure-contour newvars)

      (cond (rest-varstruct     ;salt rest arg, if any.  Home exists now, even if on stack or closure.
             (let* ((lap-address (var-lap-address rest-varstruct)))
               (finish-store 'gr:*value-1* lap-address))))

                                                ;output <b&s> for args supplied getting here, if any.
      (dotimes (c entry-number)
        (let ((supplied-var-struct (var-struct-supplied-p-variable-for-slot c x newvars)))
          (if supplied-var-struct (p2-init-supplied-variable t supplied-var-struct)))
        (cond ((and b-and-s-tag (= (1+ c) entry-number))
               (outtag-for-k b-and-s-tag nil)
               (setq b-and-s-tag nil)))
        ;do the supplied-p first since its sitting in R0 if its special and would otherwise be clobbered.
        (let ((var-struct (var-struct-supplied-p-variable-for-slot c x newvars)))
          (if (and var-struct
                   (eq (var-type var-struct) 'fef-special))
              (p2-output-b-and-s var-struct 'k:r0)

;             (let ((lap-address (var-lap-address var-struct)))
;               (p2-output-b-and-s var-struct
;                                  (if (eq (car lap-address) 'special) 'k:r0
;                                    (k-source-from-lap-address lap-address))))
            ))
        (let ((var-struct (if (>= c total-arg-slots) nil (var-struct-for-slot c x newvars))))
          (if var-struct (p2-output-b-and-s var-struct c)))
        )
                                                ;<init n>
      (setq *vars* (nthcdr number-of-vars-supplied newvars))    ;note this includes SUPPLIED-P guys.
      (cond (key-arg
             (p2-init-var-struct-to-dest key-arg entry-number x)
             (let ((supplied-var-struct (var-struct-supplied-p-variable-for-slot entry-number x newvars)))
               (if supplied-var-struct (p2-init-supplied-variable nil supplied-var-struct)))))
      (cond ((not (= entry-number total-arg-slots))     ;if not the last time thru
                                                ;output init for arg (n+1)
             (setq b-and-s-tag (gensymbol "BIND&STORE"))
                                                ;output <branch b&s <n+1>
             (outj-for-k `(k:unconditional-branch ,b-and-s-tag))))
      (cond ((and key-arg (var-struct-supplied-p-variable-for-slot entry-number x newvars))
             (incf number-of-vars-supplied)))
      ))

  n-specbinds)

;;; These are copies of the constants in K;LISP-INTERNALS.  I'm not proud of this solution.

li:(progn
(defvar *make-contour-max* 5)
;;Unfortunately, making this a vector breaks the hardebeck compilation of this.
(defvar *make-contour* '(nil make-contour-1 make-contour-2 make-contour-3 make-contour-4))
(defvar *closure-ref-0-max* 4)
(defvar *closure-set-0-max* 4)
(defvar *closure-set-0* '(closure-set-0-1 closure-set-0-2 closure-set-0-3 closure-set-0-4))
(defvar *closure-ref-0* '(closure-ref-0-1 closure-ref-0-2 closure-ref-0-3 closure-ref-0-4))
)

(defun create-closure-contour (newvars)
  (let ((env-length (loop for var in *allvars*  ; *** Is *ALLVARS* the right one?
                          count (eq (car (var-lap-address var)) 'lexical)))
        (outer-contour (if *outer-context-vars* 'k:a15 'gr:*nil*))
        ;; look for an arg assigned to K:A15
        (a15-var (loop for var in newvars
                       when (equal (var-lap-address var) '(arg 15.))
                       return var)))
    (unless (= env-length 0)                    ;No closure environment.
      (labels ((make-our-contour ()
                (outi-open-for-k `(k:open) nil #'discard-temporary-frame nil
                                 `(k:open li:make-contour))
                (outi-for-k `(k:move k:o0 ,outer-contour k:boxed-right))
                (if (< env-length li:*make-contour-max*)
                    (outi-for-k `(k:call (,(elt li:*make-contour* env-length) 1) k:a15)
                                :single-value)
                  (outi-for-k `(k:movei k:o1 ',(1+ env-length)))        ;LI:MAKE-CONTOUR expexts 1+ length
                  (outi-for-k `(k:call (li:make-contour 2) k:a15) :single-value))))
        (if (not a15-var)
            ;; Simple case
            (make-our-contour)
          ;; We originally had an argument in A15.  Shuffle it to a new home.
          (let ((new-home env-length))
            (incf env-length)
            (setf (var-lap-address a15-var) `(lexical ,new-home))
            ;; We need a frame to save the original A15 in.
            (outi-open-for-k `(k:open) nil #'discard-temporary-frame nil
                             `(k:open initialize-A15))
            (outi-for-k `(k:move k:o3 k:a15 k:boxed-right))     ;Old var home
            (make-our-contour)
            ;; Now close up our frame, storing our var in its new home.
            (closure-set-call 0 new-home 'k:o3)))))))

(DEFUN P2SBIND-FOR-K (X NEWVARS *VARS*)
  (LET ((NBINDS 0)                              ;Number of (internal-aux) special bindings
        (NNEWVARS (LOOP FOR L ON NEWVARS UNTIL (EQ L *VARS*) COUNT T)))
    (DO ((X X (CDR X)) (HOME))
        ((NULL X))
      (SETQ HOME (NTH (1- NNEWVARS) NEWVARS))
      (WHEN (P2LMB-FOR-K (CAR X) HOME)
        (INCF NBINDS))
      ;; Set *VARS* to the tail of NEWVARS starting at the variable we just handled
      ;; or its optional-specified-flag.
      (SETQ NNEWVARS (1- NNEWVARS))
      (AND (CDDR (VAR-INIT HOME)) (SETQ NNEWVARS (1- NNEWVARS)))
      (SETQ *VARS* (NTHCDR NNEWVARS NEWVARS)))
    (OR (ZEROP NNEWVARS) (BARF X "VARS screwed up by this binding" 'BARF))
    (if (zerop nbinds)
        nil
      (make-unbind-open-frame nbinds))))

;;; Bind a list of variables "in parallel":  compute all values, then bind them all.
;;; Return the number of special bindings made.
;;; Note: an attempt to bind NIL is ignored at this level.
;;; This could have been done as a P1 rewrite to turn all the specials bindings into
;;; explicit calls to BIND, as in.
;;; (LET ((A 1) (*B* 2) (*C* 3) (D 4)) (BODY A D)) ==>
;;; (LET (A D)
;;;   (BIND '*B* (progn (BIND '*C* (progn (setq a 1 d 4) 3) 2)))
;;;   (BODY A D))
;;; (except that the init-forms for the variables would be done outside the ..)
;;;
(defun p2pbind-for-k (varnames newvars)
  (loop with nbinds = 0                         ;Number of specbinds performed.
        with intcode
        for varspec in varnames
        for varname = (if (atom varspec) varspec
                        (first varspec))
        for initform = (if (atom varspec)
                           ''nil
                         (second varspec))
        for home = (find varname newvars :key #'var-name)
        do
    (when (equal initform '(%pop))
      (error "%POP can't be used on the Falcon."))
    (cond ((null varname)
           (p2-for-k initform 'd-ignore))       ;Binding nil, just perform the side-effects.
          (t
           (unless home
             (barf varname 'not-on-vars 'barf))
           (setq intcode (var-init home))
           ;; If this variable's binding is fully taken care of by function entry,
           ;; we shouldn't have gotten it here.  (The LAMBDA compiler's version of this
           ;; does have to handle them.)
           (case (var-kind home)
             (fef-arg-internal-aux)             ;The good case
             (otherwise (error "P2PBIND-for-K got an argument variable.")))
           ;; Detect and handle internal special bound variables.
           (case (var-type home)
             ((fef-special fef-remote)          ;FEF-REMOTE??
              ;; Start a call to the BIND function.  We'll actually finish it off after we get everything else
              ;; done, so all the INITFORMS are done in the right dynamic environment.
              (incf nbinds)
              (outi-open-for-k '(k:open) nil #'discard-temporary-frame) ;let POST-PROCESS make OPEN-CALL,
                                                ; otherwise it gets faked out.
              (outi-for-k `(k:movei k:o0 (quote ,varname) k:boxed-right))
              (p2-for-k initform 'k:o1))
             (otherwise
              (unless (EQUAL INITFORM '(UNDEFINED-VALUE))       ;Er, What is this?  Can you dance to it?  --RWK
                (p2-to-variable initform home))))))
    finally
    (dotimes (i nbinds)
      (outi-for-k '(k:call (li:bind 2) k:ignore)))
    (unless (zerop nbinds)
      (return (make-unbind-open-frame nbinds)))))

(defun make-unbind-open-frame (nbinds)
  (labels ((do-unbinds (open-frame operation dest &optional (source dest) source-type)
             (ignore open-frame operation source-type)
             (finish-open-frame nil)
             (output-specpdl-pop nbinds dest)
             (values dest source)))
    (make-open-frame :open-instruction 'bind
                     :cleanup-generator #'do-unbinds)))

;;; Output code for binding the var VARNAME as specified in its HOME.
;;; Return T if a special variable was bound.
(defun p2lmb-for-k (varname home &aux intcode initform)
  (when (not (atom varname))
    (setq initform (cadr varname))
    (setq varname (car varname)))
  (unless (eq (var-name home) varname)
    (barf varname "Wrong home in P2LMB" 'barf))
  (setq intcode (var-init home))
  ;; If this variable's binding is fully taken care of by function entry,
  ;; we have nothing to do here.  In fact, these shouldn't happen on the K.
  (unless (memq (car intcode) '(fef-ini-opt-sa fef-ini-comp-c))
    (fsignal "Lambda init code ~A for variable ~S."
             (car intcode) (var-name home))
    (return-from p2lmb-for-k nil))
  (multiple-value-bind (ireg special-p)
      (intermediate-dest-for-store (var-lap-address home))
    (p2-for-k initform ireg)
    (bind-variable home ireg)
    (when (eq (first intcode) 'fef-ini-opt-sa)
      (outb-for-k `(branch always nil nil (second intcode))))
    special-p))


(defun bind-variable (var source)
  ;; Detect and handle internal special bound variables.
  (when (and (eq (var-kind var) 'fef-arg-internal-aux)
             (memq (var-type var) '(fef-special fef-remote)))
    (p2-output-b-and-s var source)
    (return-from bind-variable t))
  ;; Otherwise, it's an internal local variable,
  (finish-store source (var-lap-address var))
  nil)

(defun p2-output-b-and-s (var-struct source)    ;do "bind-and-store" for special variable.
                                                ;source must be addressible from right side OR
                                                ;  if a number, represents that arg-slot number.
  (let ((type (var-type var-struct))
        (symbol (var-name var-struct)))
    (labels ((canonicalize-source ()
              (if (numberp source)
                  (cond ((< source *frame-registers-used-for-argument-passing*)
                         (setq source (a-n source)))
                        (t
                         (read-stack-slot source 'k:r0 nil)
                         (setq source 'k:md))))))
      (ecase type
        (fef-local
         ;; do nothing if local, variable "lives" in final slot,
         ;; unless it's used in a closure.
         (when (memq 'fef-arg-used-in-lexical-closures (var-misc var-struct))
           (canonicalize-source)
           (finish-store source (var-lap-address var-struct))))
        (fef-special
         (canonicalize-source)
         ;; let POST-PROCESS make OPEN-CALL, otherwise it gets faked out.
         (outi-open-for-k '(k:open) nil #'discard-temporary-frame nil '(k:open li:bind))
         (outi-for-k `(k:movei k:o0 (quote ,symbol) k:boxed))
         (outi-for-k `(k:move k:o1 ,source k:boxed-right))
         (outi-for-k '(k:call (li:bind 2) k:ignore)))))))

(defun p2-init-supplied-variable (is-sup? var-struct)
  (cond ((nc:self-evaluating? is-sup?)
         (setq is-sup? `(quote ,is-sup?))))
  (p2-to-variable is-sup? var-struct))

(defun p2-to-variable (exp var-struct)
  ;compile exp, getting the value in the variable belonging to VAR-STRUCT.  Actually, the value really
  ; gets left in K:R0 if the variable in question is special.  This allows the caller to
  ; emit a call to bind or %%set as desired.
  (let* ((lap-address (var-lap-address var-struct))
         #+ignore
         (intermediate-dest (intermediate-dest-for-store lap-address)))
    (multiple-value-bind (new-dest new-source)
        (compute-temporary-destination var-struct)
      (p2-for-k exp new-dest)
      (cond ((eq (car lap-address) 'special)
             nil)                               ;special guys sit in k:R0 until B&S operation!
            (t
             (finish-store new-source lap-address))))))

(defun intermediate-dest-for-store (lap-address)
  (declare (values ireg special-p))
  (ecase (car lap-address)
    (special (values 'k:r0 t))
    (arg (a-n (cadr lap-address)))
    (local
     ;; can't use (k:md k:boxed-md) because it wouldn't work in Functional Dest.
     'k:r0)
    (lexical 'k:r0)))

(defun finish-store (intermediate-dest lap-address &optional (call-dest 'k:ignore))
  (ecase (first lap-address)
    (special
     (let ((tail-p (tail-call-p call-dest)))
       (tail-call-open 'symbol:%%set tail-p #'discard-temporary-frame :single-value)
       (outi-for-k `(k:movei k:o0 (quote ,(second lap-address)) k:boxed))
       (outi-for-k `(k:move k:o1 ,intermediate-dest k:boxed-right))
       (maybe-tail-call '(symbol:%%set 2) call-dest :single-value tail-p)))
    (arg
     (if (not (eq intermediate-dest (a-n (cadr lap-address))))
         (outi-for-k `(k:move ,(a-n (second lap-address)) ,intermediate-dest k:boxed-right))))
    (local
     (write-stack-slot intermediate-dest
                       (second lap-address)))
    (lexical
     (destructuring-bind (offset) (rest lap-address)
       (let ((tail-p (tail-call-p call-dest)))
         (tail-call-open 'li:closure-set tail-p #'discard-temporary-frame :single-value)
         (closure-set-call 0 offset intermediate-dest call-dest tail-p))))))

;;; INTERMEDIATE-DEST can be an O register  K:O3.
;;; See P2SBIND-FOR-TOPLEVEL-FOR-K
(defun closure-set-call (level offset intermediate-dest &optional (call-dest 'k:ignore) tail-p)
  (outi-for-k `(k:move k:o0 k:a15 k:boxed-right))       ;Environment
  (cond ((and (= level 0)
              (< offset li:*closure-set-0-max*))
         (outi-for-k `(k:move k:o1 ,intermediate-dest k:boxed-right))
         (maybe-tail-call `(,(elt li:*closure-set-0* offset) 2) call-dest :single-value tail-p))
        ((= level 0)
         (outi-for-k `(k:movei k:o1 ',(1+ offset) k:boxed))     ;1 origin!
         (outi-for-k `(k:move k:o2 ,intermediate-dest k:boxed-right))
         (maybe-tail-call `(li:closure-set-0 3) call-dest :single-value tail-p))
        (t (outi-for-k `(k:movei k:o1 ',level k:boxed))
           (outi-for-k `(k:movei k:o2 ',(1+ offset) k:boxed))   ;1 origin!
           (outi-for-k `(k:move k:o3 ,intermediate-dest k:boxed-right))
           (maybe-tail-call `(li:closure-set 4) call-dest :single-value tail-p))))

;(defun k-dest-from-lap-address (lap-address)
;  (let ((ans (cond ((eq (car lap-address) 'arg)
;                   (a-n (cadr lap-address))))))
;    (if (null ans)
;       (ferror nil "Cant convert address"))
;    ans))

;(defun k-source-from-lap-address (lap-address)
;  (let ((ans (cond ((eq (car lap-address) 'arg)
;                   (a-n (cadr lap-address))))))
;    (if (null ans)
;       (ferror nil "Cant convert address"))
;    ans))

(defun assq-safe (key list)
  (prog (ent)
    l  (cond ((null list) (return nil)))
       (setq ent (car list))
       (cond ((symbolp ent)
              (cond ((eq key ent)
                     (return nil))))
             ((eq (car ent) key)
              (return ent)))
       (setq list (cdr list))
       (go l)))

(defun p2-init-var-struct-to-dest (var-struct arg-slot-number ll)
  (let* ((var-symbol (var-name var-struct))
         (var-ll (assq-safe var-symbol ll))
         (var-init-from-ll (cadr var-ll))
         (var-kind (var-kind var-struct)))
    (cond ((eq var-kind 'fef-arg-opt)
           (if (nc:self-evaluating? var-init-from-ll)
               (setq var-init-from-ll `(quote ,var-init-from-ll)))
           (cond ((< arg-slot-number *frame-registers-used-for-argument-passing*)
                  (p2-for-k var-init-from-ll (a-n arg-slot-number)))
                 (t
                  (p2-for-k var-init-from-ll 'k:r0)     ;cant use (k:md k:boxed-md), see above.
                  (write-stack-slot 'k:r0
                                    (- arg-slot-number *frame-registers-used-for-argument-passing*))))))))

(defun var-struct-supplied-p-variable-for-slot (slot-number arglist varlist)
  (let* ((arg-var-struct (var-struct-for-slot slot-number arglist varlist))
         (arg-var-kind (var-kind arg-var-struct))
         (arg-var-init (var-init arg-var-struct)))
    (cond ((eq arg-var-kind 'fef-arg-req) nil)
          ((eq (car arg-var-init) 'fef-ini-opt-sa)
           (let* ((init-var-struct (cddr arg-var-init)))
             (cond ((null init-var-struct) nil)
                   ((memq 'fef-arg-specified-flag
                          (var-misc init-var-struct))
                    init-var-struct)
                   (t (fsignal "otherwise confused")))))
          (t (ferror nil "Confused")))))

(defun var-struct-for-slot (slot-number arglist varlist)
  (let* ((arg-spec (nth slot-number arglist))
         (arg-symbol (if (symbolp arg-spec) arg-spec (car arg-spec)))
         (var-struct (find arg-symbol varlist :key #'var-name)))
    (if (null var-struct) (ferror nil "Failed to find struct for slot ~d" slot-number))
    var-struct))

;;; New smh version

;;aux-stack slots:  aux-slots are needed for arguments beyond
;;*frame-registers-used-for-argument-passing* and also for other
;;purposes if the total number of slots exceeds that.  Variables whose
;;homes live in aux-slots have (LOCAL n) in their VAR-LAP-ADDRESS.  The
;;number *BASE-STACK-SLOTS* of aux-slots required for the function is
;;fixed for the entire duration of the function.  However, more
;;aux-slots can be generated if we call with more than
;;*FRAME-REGISTERS-USED-FOR-ARGUMENT-PASSING* args.  All entries
;;immediately allocate *BASE-STACK-SLOTS* of aux-slots (note: some may
;;already be present if this entry is for # args >
;;*frame-registers-used-for-argument-passing*.)  All exits flush
;;*BASE-STACK-SLOTS* aux-slots.

;;; After the end of pass 1, assign lap addresses to the variables.
;;; Returns the total number of aux stack slots needed.

#+never
(DEFUN ASSIGN-LAP-ADDRESSES-for-k ()
  (nc:debug :lap-adr-pre
    (format nc:*debug-stream* "~%Variables:")
    (dolist (v (reverse *allvars*))
      (format nc:*debug-stream*
              "~%  ~s:~30t[~a~{ ~a~}~@[ overlap-var ~s~] ~s] ~s"
              (var-name v) (var-type v) (var-misc v) (var-overlap-var v)
              (var-use-count v) (var-lap-address v))))
  (LET ((ARGN 0)                                ;Next arg number to allocate.
        (LVCNT 0)                               ;Next local block slot ditto.
                                                ;Counts rest, aux, and
                                                ; internal-aux if not special.
        (lexicals 0)                            ;Count of closed-over vars.
        (locatizeds 0)                          ;Count args and locals that must be on stack.
        ;;(instance-variable-seen nil)
        )
    (SETQ *ARG-MAP* ())                         ;Collects arg map and local map
    (SETQ *LOCAL-MAP* ())                       ; in reverse order.
    (DOLIST (V (REVERSE *ALLVARS*))
      ;; Cons up the expression for Lap to use to refer to this variable.
      (LET ((TYPE (VAR-TYPE V))
            (KIND (VAR-KIND V))
            (NAME (VAR-NAME V))
            (locatized (memq 'variable-location (var-misc v)))
            PERMANENT-NAME)
        (SETF (VAR-LAP-ADDRESS V)
              (COND ((EQ TYPE 'FEF-SPECIAL)
                     `(SPECIAL ,NAME))
                    ((memq 'fef-arg-used-in-lexical-closures (var-misc v))
                     ;; *** Where does the level come from?
                     (prog1 `(lexical ,lexicals)
                            (incf lexicals)))
                    (locatized (prog1 `(locatized ,locatizeds)
                                      (incf locatizeds)))
                    ((MEMQ KIND '(FEF-ARG-REQ FEF-ARG-OPT))
                     (prog1 (if locatized
                                (prog1 `(locatized ,locatizeds) ;This doesn't work yet!
                                       (incf locatizeds))       ; Might we someday reuse the argslot?
                              `(ARG ,ARGN))
                            (incf argn)))                       ; Should be inside IF ???
                    ((VAR-OVERLAP-VAR V)
                     (VAR-LAP-ADDRESS (VAR-OVERLAP-VAR V)))
                    (T (prog1 `(LOCAL ,LVCNT)
                              (incf lvcnt)))))
        ;; If the name is in the temporary area or is uninterned, don't put it in the
        ;; arg/local map.  This is partly to avoid putting all these stupid gensyms
        ;; into the qfasl file, but the real reason is to avoid the dreaded scourge
        ;; of temporary area lossage in the error handler.
        (SETQ PERMANENT-NAME (UNLESS (= (%AREA-NUMBER NAME) QCOMPILE-TEMPORARY-AREA)
                               (WHEN (SYMBOL-PACKAGE NAME)
                                 NAME)))
        ;; Now maybe make an entry on *LOCAL-MAP* or *ARG-MAP*
        (COND ((MEMQ KIND '(FEF-ARG-REQ FEF-ARG-OPT))
               (var-putprop v argn 'var-arg-slot)
               (PUSH (AND PERMANENT-NAME (LIST PERMANENT-NAME)) *ARG-MAP*))
              ((OR (EQ TYPE 'FEF-LOCAL)
                   (NOT (MEMQ KIND '(FEF-ARG-INTERNAL FEF-ARG-INTERNAL-AUX)))
                   locatized)
               (COND ((NOT (VAR-OVERLAP-VAR V))
                      (PUSH (AND PERMANENT-NAME (LIST PERMANENT-NAME)) *LOCAL-MAP*))
                     (T (LET ((L1 (NTHCDR (- (LENGTH *LOCAL-MAP*)
                                             (CADR (VAR-LAP-ADDRESS V))
                                             1)
                                          *LOCAL-MAP*)))
                          (OR (NULL PERMANENT-NAME)
                              (MEMQ NAME (CAR L1))
                              (PUSH NAME (CAR L1))))))))))
    (SETQ *LOCAL-MAP* (NREVERSE *LOCAL-MAP*)
          *ARG-MAP* (NREVERSE *ARG-MAP*))
    ;; Clobber all nonspecial varnames in elements of
    ;; *CLOBBER-NONSPECIAL-VARS-LISTS* with NIL.
    ;; Clobber away all-NIL tails of those lists with NIL.
    (DOLIST (L *CLOBBER-NONSPECIAL-VARS-LISTS*)
      (LET ((LAST-NON-NIL-PTR L))
        (DO ((L1 L (CDR L1)))
            ((NULL L1))
          (LET ((HOME (find (CAR L1) *ALLVARS* :key #'var-name)))
            (IF (AND HOME (EQ (VAR-TYPE HOME) 'FEF-LOCAL))
                (RPLACA L1 NIL)
              (SETQ LAST-NON-NIL-PTR L1))))
        (IF LAST-NON-NIL-PTR
            (RPLACD LAST-NON-NIL-PTR NIL))))
    (let* ((avail-non-arg-slots (max (- *frame-registers-used-for-argument-passing* argn) 0))
           (register-slots-for-locals (min lvcnt avail-non-arg-slots))
           (stack-slots-for-args (max 0 (- argn *frame-registers-used-for-argument-passing*)))
           (stack-slots-for-locals (+ (- lvcnt register-slots-for-locals) locatizeds))
           (total-stack-slots (+ stack-slots-for-args stack-slots-for-locals)))
      (nc:debug :lap-adr-pre
        (format nc:*debug-stream*
                "~%avail-non-arg-slots ~d register-slots-for-locals ~d stack-slots-for-args ~d stack-slots-for-locals ~d total-stack-slots ~d"
                avail-non-arg-slots register-slots-for-locals stack-slots-for-args stack-slots-for-locals total-stack-slots))
      ;; lobotomize locatized vars
      (unless (= 0 locatizeds)
        (dolist (v *allvars*)
          (let ((var-address (var-lap-address v)))
            (when (eq (car var-address) 'locatized)
              ))))
      ;;move locals to args if possible
      (if (not (zerop register-slots-for-locals))
          (dolist (v *allvars*)
            (let* ((lap-address (var-lap-address v)))
              (cond ((and (eq (car lap-address) 'local)
                          (>= (cadr lap-address) (- lvcnt register-slots-for-locals)))
                     (setf (var-lap-address v)
                           `(arg ,(+ argn (- (cadr lap-address) (- lvcnt register-slots-for-locals))))))
                    ((and (not (zerop stack-slots-for-args))
                          (eq (car lap-address) 'local))        ;make a gap in the locals for args.
                     (setf (var-lap-address v)
                           `(local ,(+ stack-slots-for-args (cadr lap-address)))))))))
      ;;move args to locals if necessary
      (cond ((not (zerop stack-slots-for-args))
             ;;relocate existing locals to make room.
             (dolist (v *allvars*)
               (let ((lap-address (var-lap-address v)))
                 (cond ((eq (car lap-address) 'local)
                        (setf (var-lap-address v)
                              `(local ,(+ stack-slots-for-args (cadr lap-address))))))))
             ;;now move excess args to vacated local slots.
             (dolist (v *allvars*)
               (let ((lap-address (var-lap-address v)))
                 (cond ((and (eq (car lap-address) 'arg)
                             (>= (cadr lap-address) *frame-registers-used-for-argument-passing*))
                        (setf (var-lap-address v)
                              `(local ,(- (cadr lap-address)
                                          *frame-registers-used-for-argument-passing*)))))))))
      (nc:debug :lap-adr
        (format nc:*debug-stream* "~%Variable lap addresses:")
        (dolist (v (reverse *allvars*))
          (format nc:*debug-stream*
                  "~%  ~s:~30t[~a~{ ~a~}~@[ overlap-var ~s~] ~s] ~s"
                  (var-name v) (var-type v) (var-misc v) (var-overlap-var v)
                  (var-use-count v) (var-lap-address v))))
      ;;locals slots 0..n are overflow args
      ;;real locals are n+1..m
      (setq *stack-slots* (setq *base-stack-slots* total-stack-slots))
      )))



;; Ye Olde Old Version
;aux-stack slots:
; aux-slots are needed for arguments beyond *frame-registers-used-for-argument-passing*
;and also for other purposes if the total number of slots exceeds that.
;Variables whose homes live in aux-slots have (LOCAL n) in their
;VAR-LAP-ADDRESS.  The number, *BASE-STACK-SLOTS*, of aux-slots required for the function is fixed for
;the entire duration of the function.  However, more aux-slots can be generated if we call
;with more than *frame-registers-used-for-argument-passing* args.  All entries immediately allocate *BASE-STACK-SLOTS* of
;aux-slots (note: some may already be present if this entry is for # args >
;*frame-registers-used-for-argument-passing*.)  All exits flush *BASE-STACK-SLOTS* aux-slots.

;;; After the end of pass 1, assign lap addresses to the variables.
;;; Returns the total number of aux stack slots needed.
#-never
(DEFUN ASSIGN-LAP-ADDRESSES-for-k ()
  (nc:debug :lap-adr-pre
    (format nc:*debug-stream* "~%Variable lap addresses:")
    (dolist (v *allvars*)
      (format nc:*debug-stream* "~%  ~s:~30t~a ~s ~s"
              (var-name v) (var-type v) (var-use-count v) (var-lap-address v))))
  (LET ((ARGN 0)   ;Next arg number to allocate.
        (LVCNT 0)  ;Next local block slot number to allocate.
                   ;Count rest arg, auxes, and internal-auxes if they are not special.
        (lexicals 0))
    (SETQ *ARG-MAP* ())                         ;We also build the arg map and local map,
    (SETQ *LOCAL-MAP* NIL)                      ;pushing things on in reverse order.
    (DOLIST (V (REVERSE *ALLVARS*))
      ;; Cons up the expression for Lap to use to refer to this variable.
      (LET ((TYPE (VAR-TYPE V))
            (KIND (VAR-KIND V))
            (NAME (VAR-NAME V))
            PERMANENT-NAME)
        (SETF (VAR-LAP-ADDRESS V)
              (COND ((EQ TYPE 'FEF-SPECIAL)
                     `(SPECIAL ,NAME))
                    ((memq 'fef-arg-used-in-lexical-closures (var-misc v))
                     ;; *** Where does the level come from?
                     (prog1 `(lexical ,lexicals)
                            (incf lexicals)))
                    ((memq 'variable-location (var-misc v))
                     `(local ,lvcnt))
                    ((MEMQ KIND '(FEF-ARG-REQ FEF-ARG-OPT))
                     `(ARG ,ARGN))
                    ((VAR-OVERLAP-VAR V)
                     (VAR-LAP-ADDRESS (VAR-OVERLAP-VAR V)))
                    (T `(LOCAL ,LVCNT))))
        (if (memq kind '(fef-arg-req fef-arg-opt))
            (var-putprop v argn 'var-arg-slot))
        ;; If the name is in the temporary area or is uninterned, don't put it in the
        ;; arg/local map.  This is partly to avoid putting all these stupid gensyms
        ;; into the qfasl file, but the real reason is to avoid the dreaded scourge
        ;; of temporary area lossage in the error handler.
        (SETQ PERMANENT-NAME (UNLESS (= (%AREA-NUMBER NAME) QCOMPILE-TEMPORARY-AREA)
                               (WHEN (SYMBOL-PACKAGE NAME)
                                 NAME)))
        ;; Now increment one or more of the counters of variables
        ;; and maybe make an entry on *LOCAL-MAP* or *ARG-MAP*
        (COND ((MEMQ KIND '(FEF-ARG-REQ FEF-ARG-OPT))
               (PUSH (AND PERMANENT-NAME (LIST PERMANENT-NAME)) *ARG-MAP*)
               (incf argn))
              ((OR (EQ TYPE 'FEF-LOCAL)
                   (NOT (MEMQ KIND '(FEF-ARG-INTERNAL FEF-ARG-INTERNAL-AUX)))
                   (memq 'variable-location (var-misc v)))
               (COND ((NOT (VAR-OVERLAP-VAR V))
                      (PUSH (AND PERMANENT-NAME (LIST PERMANENT-NAME)) *LOCAL-MAP*)
                      (incf lvcnt))
                     (T (LET ((L1 (NTHCDR (- (LENGTH *LOCAL-MAP*)
                                             (CADR (VAR-LAP-ADDRESS V))
                                             1)
                                          *LOCAL-MAP*)))
                          (OR (NULL PERMANENT-NAME)
                              (MEMQ NAME (CAR L1))
                              (PUSH NAME (CAR L1))))))))))
    (SETQ *LOCAL-MAP* (NREVERSE *LOCAL-MAP*)
          *ARG-MAP* (NREVERSE *ARG-MAP*))
    ;; Clobber all nonspecial varnames in elements of
    ;; *CLOBBER-NONSPECIAL-VARS-LISTS* with NIL.
    ;; Clobber away all-NIL tails of those lists with NIL.
    (DOLIST (L *CLOBBER-NONSPECIAL-VARS-LISTS*)
      (LET ((LAST-NON-NIL-PTR L))
        (DO ((L1 L (CDR L1)))
            ((NULL L1))
          (LET ((HOME (find (CAR L1) *ALLVARS* :key #'var-name)))
            (IF (AND HOME (EQ (VAR-TYPE HOME) 'FEF-LOCAL))
                (RPLACA L1 NIL)
                (SETQ LAST-NON-NIL-PTR L1))))
        (IF LAST-NON-NIL-PTR
            (RPLACD LAST-NON-NIL-PTR NIL))))
    (let* ((avail-non-arg-slots (max (- *frame-registers-used-for-argument-passing* argn) 0))
           (register-slots-for-locals (min lvcnt avail-non-arg-slots))
           (stack-slots-for-args (max 0 (- argn *frame-registers-used-for-argument-passing*)))
           (stack-slots-for-locals (- lvcnt register-slots-for-locals))
           (total-stack-slots (+ stack-slots-for-args stack-slots-for-locals)))
  ;move locals to args if possible
      (if (not (zerop register-slots-for-locals))
          (dolist (v *allvars*)
            (let* ((lap-address (var-lap-address v)))
              (cond ((memq 'variable-location (var-misc v)))    ;Can't move!
                    ((and (eq (car lap-address) 'local)
                          (>= (cadr lap-address) (- lvcnt register-slots-for-locals)))
                     (setf (var-lap-address v)
                           `(arg ,(+ argn (- (cadr lap-address) (- lvcnt register-slots-for-locals))))))
                    ((and (not (zerop stack-slots-for-args))
                          (eq (car lap-address) 'local)) ;make a gap in the locals for args.
                     (setf (var-lap-address v)
                           `(local ,(+ stack-slots-for-args (cadr lap-address)))))))))
  ;move args to locals if necessary
      (cond ((not (zerop stack-slots-for-args))
             ;relocate existing locals to make room.
             (dolist (v *allvars*)
               (let ((lap-address (var-lap-address v)))
                 (cond ((eq (car lap-address) 'local)
                        (setf (var-lap-address v)
                              `(local ,(+ stack-slots-for-args (cadr lap-address))))))))
             ;now move excess args to vacated local slots.
             (dolist (v *allvars*)
               (let ((lap-address (var-lap-address v)))
                 (cond ((and (eq (car lap-address) 'arg)
                             (>= (cadr lap-address) *frame-registers-used-for-argument-passing*))
                        (setf (var-lap-address v)
                              `(local ,(- (cadr lap-address)
                                          *frame-registers-used-for-argument-passing*)))))))))
      (nc:debug :lap-adr
        (format nc:*debug-stream* "~%Variable lap addresses:")
        (dolist (v *allvars*)
          (format nc:*debug-stream* "~%  ~s:~30t~a ~s ~s"
                  (var-name v) (var-type v) (var-use-count v) (var-lap-address v))))
  ;locals slots 0-n are overflow args
  ;real locals are n+1 .. m
      (setq *stack-slots* (setq *base-stack-slots* total-stack-slots))
      )))


(defun var-putprop (var-struct property flag)
  (setf (var-plist var-struct)
        (cons flag (cons property (var-plist var-struct)))))

;-- this is part of P1, which normally would not be changed by cross compiler mode.
;-- forked it so as to bypass hairy LAMBDA FEF initializing options.
;;; Given a variable home, compute its VAR-INIT and install it.
;;; When we are called, the VAR-INIT contains the data for us to work on
;;; which looks like (init-form arg-supplied-flag-name).
;;; Note that for a FEF-ARG-INTERNAL-AUX variable, the init-type will
;;; always be FEF-INI-COMP-C.
;;; At time of call, *VARS* should be bound to the environment for
;;; execution of the init form for this variable.
(DEFUN VAR-COMPUTE-INIT-for-k (HOME PARALLEL)
  (LET* ((NAME (VAR-NAME HOME))
         (KIND (VAR-KIND HOME))
         (TYPE (VAR-TYPE HOME))
         (INIT-SPECS (VAR-INIT HOME))
         (INIT-FORM (CAR INIT-SPECS))
         (SPECIFIED-FLAG-NAME (CADR INIT-SPECS))
         INIT-TYPE
         INIT-DATA)
    (COND ((NULL INIT-FORM))
          (T
           ;; Init is not NIL, constant or self => must P1 it, and maybe set TLFUNINIT.
           (LET ((*TLEVEL* NIL))
             (SETQ INIT-FORM (P1V INIT-FORM 1)))
           (SETQ TLFUNINIT T)))
    ;; Now that we have processed the init form, determine the ADL initialization field.
    ;; First, must we, or would we rather, use code to initialize the variable?
    ;; Note: specified-flags MUST be initted at entry time regardless of anything else.
    (WHEN (AND (NOT (MEMQ 'FEF-ARG-SPECIFIED-FLAG (VAR-MISC HOME)))
               (OR (EQ KIND 'FEF-ARG-INTERNAL-AUX) TLFUNINIT
                   ;; Don't spoil the fast arg option with nontrivial inits for aux's.
                   (AND (EQ KIND 'FEF-ARG-AUX)
                        *FAST-ARGS-POSSIBLE*
                        (NOT (SI:MEMBER-EQUAL INIT-FORM '(NIL 'NIL))))
                   (IF PARALLEL (NEQ TYPE 'FEF-LOCAL))))
      (SETQ INIT-TYPE 'FEF-INI-COMP-C)
      ;; Note: if we are initting by code, there is no advantage
      ;; in binding at function entry, and doing so would
      ;; make lap stupidly turn off the fast arg option!
      (AND (EQ KIND 'FEF-ARG-AUX)
           (SETF (VAR-KIND HOME) (SETQ KIND 'FEF-ARG-INTERNAL-AUX)))
      (SETQ TLFUNINIT T))
    ;; If we aren't forced already not to use an init, figure out
    ;; what type of init to use if there's no init-form: either "none" or "nil".
    (UNLESS INIT-TYPE
      (SETQ INIT-TYPE
            (IF (OR (EQ KIND 'FEF-ARG-OPT)
                    (AND (EQ KIND 'FEF-ARG-AUX)
                         (MEMQ TYPE '(FEF-SPECIAL FEF-REMOTE))))
                'FEF-INI-NIL
                'FEF-INI-NONE)))
    ;; Then, if there is an init form, gobble it.
    (WHEN (AND INIT-FORM (NEQ INIT-TYPE 'FEF-INI-COMP-C))
      (COND ((NOT (MEMQ KIND
                        '(FEF-ARG-OPT FEF-ARG-AUX FEF-ARG-INTERNAL-AUX)))
             (WARN 'BAD-ARGUMENT-LIST :IMPOSSIBLE
                   "The mandatory argument ~S was given a default value."
                   NAME))
            ;; There's a hack for binding a special var to itself.
            ((AND (EQ NAME INIT-FORM)
                  (NEQ TYPE 'FEF-LOCAL))
             (SETQ INIT-TYPE 'FEF-INI-SELF))
            ((MEMQ (CAR INIT-FORM) '(QUOTE FUNCTION BREAKOFF-FUNCTION SELF-REF))
             (SETQ INIT-TYPE 'FEF-INI-PNTR)
             (SETQ INIT-DATA INIT-FORM))
            (T (BARF INIT-FORM "Init-form calculation confused"))))
    (COND ((AND (EQ KIND 'FEF-ARG-OPT)
                (OR TLFUNINIT SPECIFIED-FLAG-NAME))
           ;; Once an opt arg gets an alternate starting address,
           ;; all following args must be similar or else FEF-INI-COMP-C.
           (SETQ TLFUNINIT T)
           (SETQ INIT-TYPE 'FEF-INI-OPT-SA)
           (SETQ INIT-DATA (gensymbol name "-init")))
          ;; If something not an optional arg was given a specified-flag,
          ;; discard that flag now.  There has already been an error message.
          (T (SETQ SPECIFIED-FLAG-NAME NIL)))
    (SETF (VAR-INIT HOME)
          (LIST* INIT-TYPE INIT-DATA
                 (AND SPECIFIED-FLAG-NAME
                      (DOLIST (V *ALLVARS*)
                        (AND (EQ (VAR-NAME V) SPECIFIED-FLAG-NAME)
                             (MEMQ 'FEF-ARG-SPECIFIED-FLAG (VAR-MISC V))
                             (RETURN V))))))
    (IF (NULL INIT-FORM)
        NAME
        (LIST NAME INIT-FORM))))

;mostly copied from ORSON:FLEABIT.GENERATE;GENERATE.  Try to keep things as much the same as possible.
(defun generate-incr (dest ptr n &optional (reg 'K:R1))
; this when alui-16 sign extends to 24 bits
;  (emit 'K:ALUI-16 'K:L+R dest ptr `',n)
  (case n
    (0 ;(generate-move ptr dest)
     (if (not (eq dest ptr))
         (emit `(K:MOVE ,dest ,ptr K:BOXED-RIGHT K:BW-24)))
     )
    (1 (emit-alu 'K:R+1 dest ptr ptr '(K:BOXED-RIGHT K:BW-24)))
    (2 (emit-alu 'K:R+2 dest ptr ptr '(K:BOXED-RIGHT K:BW-24)))
    (4 (emit-alu 'K:R+4 dest ptr ptr '(K:BOXED-RIGHT K:BW-24)))
    (t (emit `(K:MOVEI ,reg ',n K:BOXED))
       (emit-alu 'K:R+L dest reg ptr '(K:BOXED-RIGHT K:BW-24)))))

(defun generate-decr (dest ptr n &optional (reg 'K:R1))
  (case n
    (0 ;(generate-move ptr dest)
     (if (not (eq dest ptr))
         (emit `(K:MOVE ,dest ,ptr K:BOXED-RIGHT K:BW-24)))
     )
    (1 (emit-alu 'K:R-1 dest ptr ptr '(K:BOXED-RIGHT K:BW-24)))
    (2 (emit-alu 'K:R-2 dest ptr ptr '(K:BOXED-RIGHT K:BW-24)))
    (4 (emit-alu 'K:R-4 dest ptr ptr '(K:BOXED-RIGHT K:BW-24)))
    (t (emit `(K:MOVEI ,reg ',n K:BOXED))
       (emit-alu 'K:R-L dest reg ptr '(K:BOXED-RIGHT K:BW-24)))))


;;; but this shouldn't alloc for args, (interacts with optional args)
(defun generate-alloc-stack-slots (n)
  (check-type n (integer 0))
  (when (plusp n)
    (generate-incr 'GR:*STACK-POINTER* 'GR:*STACK-POINTER* n)
    (incf *stack-slots* n)))

(defun generate-dealloc-stack-slots (n)
  (check-type n (integer 0))
  (unless (zerop n)
    (generate-decr 'GR:*STACK-POINTER* 'GR:*STACK-POINTER* n)
    (decf *stack-slots* n)))

(defun stack-slot-address (dest slot &optional (reg 'k:R0))
  (generate-decr dest 'gr:*stack-pointer*
                 (- *stack-slots* slot)
                 reg))

(defun read-stack-slot (slot &optional (reg 'K:R0) (insert-wait t))
  (stack-slot-address '(K:VMA-START-READ K:BOXED-VMA K:BOXED-MD) slot reg)
  (if insert-wait (emit '(K:MEMORY-WAIT))))     ;result comes back in K:MD

(defun write-stack-slot (from slot &optional (temporary-register-that-gets-clobbered 'K:R1))
  ;FROM must be addressible from processor right source.
  ;if from is 'ALREADY-IN-MD, then assume it is.  (Can't use K:MD for this because K:MD copying
  ;is sometimes necessary due to multiple md "feature" of hardware.)
  ;--remember boxed-bit decoding is different if dest is VMA or MD. Instruction always specifies both directly.
  (if (not (eq from 'already-in-md))
      (emit `(K:MOVE (K:MD K:BOXED-MD) ,from)))
  (stack-slot-address '(K:VMA-START-WRITE K:BOXED-VMA K:BOXED-MD) slot
                      temporary-register-that-gets-clobbered)
  ;; We need to optimize this out somehow later.  Maybe the peephole optimizer already does it?
  (emit '(K:MEMORY-WAIT)))

(defun move-stack-slot-to-stack-slot (from-slot to-slot)
  (generate-decr '(K:VMA-START-READ K:BOXED-VMA K:BOXED-MD) 'GR:*STACK-POINTER* (- *stack-slots* from-slot))
  (emit '(K:MEMORY-WAIT))
  (generate-decr '(K:VMA-START-WRITE K:BOXED-VMA) 'GR:*STACK-POINTER* (- *stack-slots* to-slot))
  )

;--- interface

(defun emit (x)
  (outi-for-k x))

(defun emit-alu (op dest left right &optional options)
  (emit `(K:ALU ,op ,dest ,left ,right ,@options))
  dest)


(defun k-push-stack-arg (intermediate-home)
  (generate-incr 'GR:*STACK-POINTER* 'GR:*STACK-POINTER* 1)     ;incr stack pointer
  ;(outi-for-k `(K:MOVE (K:VMA K:BOXED-VMA) GR:*STACK-POINTER*))
  (generate-decr '(k:vma k:boxed-vma) 'gr:*stack-pointer* 1)
  (outi-for-k `(K:MOVE (K:MD-START-WRITE K:BOXED-VMA K:BOXED-MD) ,intermediate-home))
  (incf *stack-slots*)
  )

;---

;P2-FOR-K central functions.
;This is getting to look more and more like a whole new P2 (which is progress).

;;; Compile a form for multiple values (maybe).
;--- for lambda only.
;;; If our value is non-nil, it means that the code compiled
;;; failed to produce the multiple values as it was asked to.
;;; Normally, the destination should be D-PDL.
;;; If you use another destination, then, if the value returned is non-NIL
;;; then the single value has been compiled to the given destination,
;;; but if the value is NIL, then the destination has been ignored.
;;; This happens because forms that know how to generate the multiple
;;; values setq *M-V-TARGET* to NIL.

;;; Note: It is assumed that D-RETURN never has an *M-V-TARGET*,
;;; and that an *M-V-TARGET* of MULTIPLE-VALUE-LIST implies D-PDL.

;*M-V-TARGET* can be: a number, multiple-value-list, throw.

;--- for lambda only.



;Internal value: (for example, normal call to p2-for-k).  Value may be
;  generated by placing it in the desired destination without procedure call.
;Internal multiple values.  In some cases, compiler could figure things out
;  entirely at compile time.  We don't do this now, instead, everything must
;  use the external multiple-value mechanism.
;External multiple values.  Involve the mechanism of hw:return-code-mv-p,
; *number-of-return-values*, *return-0*, etc.  Always involves an actual
; procedure call, if for no other reason than destination return, etc.
; must be used to set up return-code-mv-p, etc.

(DEFUN P2MV-for-k (FORM DEST *M-V-TARGET*)
  (IF (NULL *M-V-TARGET*)
      (P2-for-k FORM DEST)
    (COND ((ADRREFP-for-k FORM)
           (P2-for-k FORM DEST))
          ((memq (car form) '(%POP %pop-for-with-stack-list))
           (ferror nil "%pop or %pop-for-with-stack-list can't be used on the Falcon."))
          (T
           (P2F-for-k FORM DEST))))
  *M-V-TARGET*)

(DEFUN ADRREFP-for-k (EXP)                              ;Predicate T if can be ref by adr only
  (OR (ATOM EXP)
      (MEMQ (CAR EXP)
            '(LOCAL-REF QUOTE FUNCTION BREAKOFF-FUNCTION SELF-REF
                        lexical-ref))))

;;; Compile code to compute FORM and put the result in destination DEST.
;;; If DEST is D-IGNORE, we may not actually bother to compute the value
;;; if we can tell that there would be no side-effects.
;DEST can be a symbol in the K package such as K:O0, K:A1, K:R2, etc.
;DEST can also be a list (k:new-open <n>) or (k:new-tail-open <n>).
;  in these cases, *open-frames* is hacked (mostly by OUTI-FOR-K).
;the value returned by P2-FOR-K is undefined.
(DEFUN P2-FOR-K (FORM DEST)
  (COND ((ADRREFP-for-k FORM)
         (OR (EQ DEST 'D-IGNORE)
             (p2-compute-move-for-k dest form)))
        ((EQ (CAR FORM) '%POP)
         (warn nil :very-obsolete "%POP is not supported on the Falcon."))
        ((EQ (CAR FORM) '%POP-for-with-stack-list)
         (warn nil :very-obsolete "%POP-FOR-WITH-STACK-LIST is not supported on the Falcon."))
        ((EQ (CAR FORM) 'CHANGE-PDLLVL)
         (warn nil :very-obsolete "Stack lists are not supported on the Falcon."))
        (T
         (LET ((*BDEST* ()))
           (P2F-FOR-K FORM DEST)))))

;; Stubs for now.
(defun microcoded-on-lambda (fun)
  (ignore fun)
  ())

(defun coded-for-k (fun)
  (ignore fun)
  t)

(DEFUN P2F-FOR-K (FORM DEST)
  (LET ((*P2FN* (CAR FORM))
        (ARGL (CDR FORM))
        TEM)
    (cond ((and (microcoded-on-lambda *p2fn*)
                (not (coded-for-k *p2fn*)))
           (format t "~&~S is not implemted on the K yet~%" *p2fn*)))
    (COND ((setq tem (get *p2fn* 'p2-for-k))
           (funcall tem (cdr form) dest))
          ((SETQ TEM (GET *P2FN* 'QINTCMP))
           (P2MISC-for-k *P2FN* ARGL DEST TEM))
          (T
           (P2ARGC-for-k *P2FN* ARGL (GETARGDESC *P2FN*) DEST *P2FN*)))))

(defun p2-compute-move-for-k (dest form &aux tem)
  (labels ((do-lexical (ref-index)
            ;; Lexical-ref's are encoded as 12 bits of offset in environment and
            ;; 12 bits of nesting level.
            (let* ((tail-p (tail-call-p dest))  ;Heh heh.  1-instruction closures!
                   (level (ldb (byte 12. 12.) ref-index))
                   (offset (ldb (byte 12. 0) ref-index)))
              (tail-call-open 'closure-ref tail-p #'discard-temporary-frame :single-value)
              (outi-for-k `(k:move k:o0 k:a15 k:boxed-right))   ;Lexical Environment
              (cond ((and (= level 0)
                          (< offset li:*closure-ref-0-max*))
                     (maybe-tail-call `(,(elt li:*closure-ref-0* offset) 1)
                                      dest :single-value tail-p))
                    ((= level 0)
                     (outi-for-k `(k:movei k:o1 ',(1+ offset) k:boxed)) ;1-origin!
                     (maybe-tail-call `(li:closure-ref-0 2) dest :single-value tail-p))
                    (t (outi-for-k `(k:movei k:o1 ',level k:boxed))
                       (outi-for-k `(k:movei k:o2 ',(1+ offset) k:boxed))       ;1-origin!
                       (maybe-tail-call `(li:closure-ref 3) dest :single-value tail-p))))))
    (cond ((and (symbolp form)
                ;;; @#$#@ this should be NC::REGISTER or COMPILER::REGISTER @#$#@$
                (get form :register))
           (outi-for-k `(k:move ,dest ,form k:boxed-right) :single-value))
          ((atom form)
           ;; let POST-PROCESS make OPEN-CALL, otherwise it gets faked out.
           (let ((tail-p (tail-call-p dest)))
             (tail-call-open 'symbol:%symbol-value tail-p #'discard-temporary-frame
                             :single-value)
             (outi-for-k `(k:movei k:o0 (quote ,form) k:boxed))
             (maybe-tail-call `(symbol:%symbol-value 1) dest :single-value tail-p)))
          ((eq (car form) 'local-ref)
           (let ((lap-address (var-lap-address (cadr form))))
             (ecase (first lap-address)
               (arg
                (outi-for-k `(k:move ,dest ,(a-n (cadr lap-address)) k:boxed-right)
                            :single-value))
               (local
                (read-stack-slot (cadr lap-address))
                (outi-for-k `(k:move ,dest k:md k:boxed-right) :single-value))
               (lexical
                (do-lexical (second lap-address))))))
          ;; LEXICAL
          ((eq (first form) 'lexical-ref)
           (let ((ref-index (second form)))
             (do-lexical ref-index)))
          ((memq (car form) '(function quote breakoff-function self-ref))
           (cond ((and (eq (car form) 'quote)
                       (setq tem (k-find-constant-register (cadr form))))       ;if possible, use move in hope
                  (outi-for-k `(k:move ,dest ,tem k:boxed-right) :single-value))        ; it can be combined into jump.
                 (t
                  (outi-for-k `(k:movei ,dest ,form k:boxed) :single-value))))
          (t (ferror nil "this is adrrefp-for-k?? ~s" form)))))


;;;; Compile functions which have their own special instructions.

;;; Here for a "miscellaneous" instruction (no source address field; args always on PDL).
;;; Such functions have no P2 properties.  We recognize them by their QINTCMP
;;; properties, which hold the number of args which the function takes.
(DEFUN P2MISC-for-k (INSN ARGL DEST NARGS)
  (WHEN ( NARGS (LENGTH ARGL))                 ;Too few args
    (warn nil nil "Wrong number of arguments to ~S." insn))
  (P2ARGC-for-k INSN ARGL (LIST (CONS NARGS '((FEF-ARG-REQ FEF-QT-EVAL))))
                DEST *P2FN*))


(DEFUN P2ARGC-for-k (FCTN ARGL DESC DEST TARGET &OPTIONAL MAPPING-TABLE)
  (LET (COUNT TOKEN-LIST AG1 DSC1 TM
        RESTART-PC
        (*stack-slots* *stack-slots*)           ;any stack-slots created for call are removed by callee.
        (previous-stack-slots *stack-slots*)    ;cant wait for unbinding to restore this
        (argn 0)                                ;  see below.
        (call-terms nil)                        ;total number of args being passed.
        (k-dest nil)                            ;destination for arg on K.
        (tail-call-switch
          (and (tail-call-p dest fctn)
               ( (length argl) *frame-registers-used-for-argument-passing*)))
        (our-open-frame))
    (labels ((generate-call-discard (open-frame operation dest &optional source source-type)
               (ignore open-frame)
               (ecase operation
                 ((nil)                         ;the NIL operation outputs the normal call.
                  (when (null (open-frame-there-p open-frame))
                    (fsignal "tried to output frame before it exists"))
                  (cond (tail-call-switch
                         (outi-for-k `(k:tail-call (,target ,argn)) source-type)
                         (values dest source))
                        (t (outi-for-k `(k:call (,target ,argn) ,dest)
                                       source-type)
                           (values dest dest))))
                 ((:exist)
                  (setf (open-frame-there-p open-frame) t))
                 ((:discard :return)
                  (cond ((open-frame-there-p open-frame)
                         (when (> *stack-slots* previous-stack-slots)
                           ;; Clean up any stack slots allocated to args for this call.
                           (generate-dealloc-stack-slots (- *stack-slots* previous-stack-slots)))
                         (cond ((not tail-call-switch)
                                (outi-for-k `(k:call (li::discard-open-frame 0) k:ignore)))
                               (t
                                (discard-tail-call-frame)
                                (pop *open-frames*))))
                        (t (pop *open-frames*)))
                  (values dest source)))))
      (SETQ AG1 ARGL)
      (setq call-terms (length ag1))
      (SETQ DSC1 DESC)
      (COND ((NOT (SYMBOLP FCTN))
             (fsignal "funcall")
             (SETQ TM FCTN))    ;Non-symbolic fctn; it's address for CALL
            (T (SETQ TM `(QUOTE-VECTOR (FUNCTION ,TARGET)))))
      (when (null ag1)          ;if zero args, open frame now.
        (tail-call-open fctn tail-call-switch #'generate-call-discard :subr-value)
        ; Remember which frame we opened, so we can be sure we close the same one.
        (setq our-open-frame *open-frames*))

        (PROG ()
           L4 (COND ((NULL DSC1) (GO X2)))
              (SETQ COUNT (CAAR DSC1))
              (SETQ TOKEN-LIST (CADAR DSC1))
              (COND ((MEMQ 'FEF-ARG-REST TOKEN-LIST)
                     (SETQ COUNT #o1005)))
           L3 (setq k-dest (cond ((null ag1))  ;no args, k-dest unused, frame opened above.
                                 ((zerop argn)
                                  ;; If our destination is to be a new open, we pass along
                                  ;; the additional information about the call to match that
                                  ;; open.  Thus, once it takes effect, *OPEN-FRAMES* will be
                                  ;; synchronously updated to reflect what needs to be done to
                                  ;; complete or discard the call.  But it is here that the information
                                  ;; is known, not when the destination finally becomes a call to
                                  ;; OUTI-FOR-K.
                                  (outi-open-for-k-internal (if tail-call-switch
                                                                `(k:tail-open ,fctn)
                                                              `(k:open ,fctn))
                                                            tail-call-switch nil #'generate-call-discard)
                                  (setq our-open-frame *open-frames*)
                                  (if tail-call-switch
                                      `(k:new-tail-open 0 ,target ,(first our-open-frame))
                                    `(k:new-open 0 ,target ,(first our-open-frame))))
                                 ((< argn *frame-registers-used-for-argument-passing*) (o-n argn))
                                 (t 'k:r2)))    ;then put it in stack-slot
              (COND ((= 0 COUNT) (SETQ DSC1 (CDR DSC1)) (GO L4))
                    ((AND (MEMQ 'FEF-ARG-REST TOKEN-LIST)
                          (MEMQ 'FEF-QT-QT TOKEN-LIST))
                     (fsignal "Call of FEXPR")
                     (GO RET))
                    ((NULL AG1) (GO RET))       ;OUT OF ARG LIST
                    ((MEMQ 'FEF-QT-QT TOKEN-LIST)
                     (if (not (zerop argn))
                         (outi-for-k `(k:move ,k-dest (quote ,(car ag1)) k:boxed-right))
                       (outi-for-k `(k:move ,k-dest (quote-vector ',(car ag1)) k:boxed-right))))
                    ((MEMQL '(FEF-QT-EVAL FEF-QT-DONTCARE) TOKEN-LIST)
                     (COND ((AND (NULL (CDR AG1))
                                 (MEMQ 'LEXPR-FUNCALL TOKEN-LIST))
                            (fsignal "%spread")
                            (P2-for-k (CAR AG1)
                                      (PROGN (FSIGNAL "D-PDL") 'K:R0))  ;Arg to %SPREAD
                            (OUTI-for-k (LIST 'MISC k-dest '%SPREAD)))
                           (T (P2-for-k (CAR AG1) k-dest))))
                    (T (BARF TOKEN-LIST
                             'TOKEN-LIST-LOSES-P2
                             'BARF)))
              (if (>= argn *frame-registers-used-for-argument-passing*)
                  (k-push-stack-arg k-dest))    ;this will increment *stack-slots*
              (incf argn)
              (SETQ AG1 (CDR AG1))
              (DECF COUNT)
              (GO L3)
           X2
              (COND (AG1 (SETQ DSC1 '((#o1005 (FEF-ARG-OPT FEF-QT-EVAL))))      ;Compile the rest
                         (GO L4)))              ;of them; he may possibly know what he's doing
           RET
              (setq *stack-slots* previous-stack-slots)
              (outi-close-for-k our-open-frame dest nil nil :subr-value)

              (COND (MAPPING-TABLE
                     (fsignal "Can't compile the mapping-table code yet.")))
              (COND (RESTART-PC
                     (SETQ *DROPTHRU* T)
                     (OUTF-for-k (LIST 'RESTART-TAG RESTART-PC))))
              (COND ((AND (EQ DEST 'D-RETURN)
                          (NULL RESTART-PC))
                     (TAKE-DELAYED-TRANSFER)))
              (RETURN NIL)

              ))))

;P2-FOR-K generators

(DEFUN (:PROPERTY COND P2-FOR-K) (ARGL DEST)
  (PROG (CLAUSE end-tag clause-tag CLAUSE-LENGTH TM PRED NOFALLTHRU
         LAST-CLAUSE-FLAG)
        (SETQ end-tag (gensymbol "COND-END"))   ;Tag to go to with value of COND in DEST

        ;; Compile next clause.
     L1
        (IF (NULL (CDR ARGL)) (SETQ LAST-CLAUSE-FLAG T))
        (SETQ CLAUSE (CAR ARGL)
              CLAUSE-LENGTH (LENGTH CLAUSE))
        (SETQ clause-tag (gensymbol "COND-CLAUSE"))
        (SETQ PRED (CAR CLAUSE))
        (WHEN (EQ (CAR-SAFE PRED) 'QUOTE)
          (and (NULL (CADR PRED))                               ;Is the null condition?
               (NOT LAST-CLAUSE-FLAG)
               (GO L5))                                         ;Can happen from DO expansion.
          ;;Condition is always true -- discard any remaining clauses.
          (SETQ NOFALLTHRU T)
          (when (CDR ARGL)
            (SETQ LAST-CLAUSE-FLAG T)
            ;;These can come from expanding DEFSUBSTs that contain CONDs with constant args.
            #+never
            (WARN 'UNREACHABLE-CODE ':IMPLAUSIBLE
                  "Some COND clauses are unreachable;
 the first starts with ~S."
                  (CAADR ARGL))
            (SETQ ARGL (LIST CLAUSE))))
        ;; Handle certain special cases of clauses.
        (COND ((AND (not (d-ignore-p dest)) (= 1 CLAUSE-LENGTH))
               ;; Clause containing only one element, compiled for value.
               ;; Value of condition is also value of clause.
               (cond ((constantp pred)                          ;Should be constant-in-compiler !! That's more inclusive. -smh
                      (p2-for-k (first clause) dest))
                     (t (P2-for-k PRED 'k:r0)
                        ;;The following instruction is necessary because the store in destination return
                        ;; might have been done with an ALU BW-24, in which case the indicators would test
                        ;; ZEROP not NULL.  We could have the convention outlawing that, in which case,
                        ;; this instruction could go.
                        (outi-for-k `(k:move k:nop k:r0 k:boxed-right))
                        (OUTB-for-k `(BRANCH NILIND true NIL ,clause-tag))
                        ;; OK, we're non-null, so move value to the destination.  Tell OUTI-FOR-K it's just
                        ;; got a single value to work with.
                        (outi-for-k `(k:move ,dest k:r0 k:boxed-right) :single-value)))
               (go L5))
              ;; Clause of one element, if value is not wanted.
              ((= 1 CLAUSE-LENGTH) (BOOL1-for-k PRED 'FALSE end-tag) (GO L5))
              ;; Clause is just condition followed by a GO.
              ((AND (= 2 CLAUSE-LENGTH)
                    (SIMPLEGOP-for-k (CADR CLAUSE)))
               (BOOL1-for-k PRED 'FALSE (GTAG (CADADR CLAUSE)))
               (GO L5))
              ;; Clause after this one is (T (GO ...)).
              ;; Can get special handling only if the GO
              ;; requires no pdl adjustment.
              ((AND (NOT NOFALLTHRU)            ;Isolate case of ((P1 A1) (T (GO X)))
                    (NOT LAST-CLAUSE-FLAG)
                    (NOT (ATOM (CAR (SETQ TM (CADR ARGL)))))
                    (EQ (CAAR TM) 'QUOTE)
                    (CADAR TM)
                    (= 2 (LENGTH TM))
                    (SIMPLEGOP-for-k (CADR TM)))
               ;; In effect, we turn this into (COND ((NOT P1) (GO X)) (T A1))
               (BOOL1-for-k PRED 'TRUE (GTAG (CADADR TM)))      ;Go X directly if P1 false
               (SETQ ARGL (CONS (CONS ''T (CDR CLAUSE)) (CDDR ARGL)))
               (GO L1))
              ((NOT NOFALLTHRU)                 ;Normal COND clause.
               (BOOL1-for-k PRED 'TRUE clause-tag)))    ;Jump around clause if predicate fails.

        ;; If the COND will have to return NIL if this clause's
        ;; condition is false, then generate a clause to return the nil.
        (WHEN (AND (not (d-ignore-p dest)) LAST-CLAUSE-FLAG (NOT NOFALLTHRU))
          (SETQ ARGL (LIST CLAUSE '('T 'NIL)))
          (SETQ LAST-CLAUSE-FLAG NIL))

        (let ((*open-frames* *open-frames*))
          ;; Compile each clause with its effects on *OPEN-FRAMES* being independent
          ;; of the others.
          ;; Compile the actions of the cond clause, except for the last.
          (DO ((ACTIONS (CDR CLAUSE) (CDR ACTIONS)))
              ((NULL (CDR ACTIONS))
               (SETQ CLAUSE ACTIONS))
            (P2-for-k (CAR ACTIONS) 'D-IGNORE))

          ;; Compile last action of cond clause (the value) to its final home.
          (p2-for-k (first clause) dest))

        (when (cdr argl)
          ;; Still more clauses to come, branch around them.
          (outb-for-k `(branch always nil nil ,end-tag)))

        ;; Here at end of cond-clause.
     L5
        (OUTTAG-FOR-K clause-tag NIL)                   ;Output tag for jumps from failing predicate.
        (IF (SETQ ARGL (CDR ARGL))              ;If there are more clauses, process them.
            (GO L1))

        ;; There are no more cond clauses!
        ;; No need to generate a NIL in this case, since we already produced a ('T 'NIL) clause
        ;; if one was needed.

        ;; In all of our branches which branch to this point, we reduced the open-frame
        ;; level by moving to DEST.  Simulate that effect once, now.
        (compute-new-level-for-destination dest)

        (OUTTAG-FOR-K end-tag NIL)
        (RETURN NIL)))


;;; Compile code to test CONDITION and jump to tag if it is NIL
;;; (for SENSE = TRUE) or if it is non-NIL (for SENSE = FALSE).
;(when getting to TAG, value will be in K:R0).
(DEFUN BOOL1-for-k (CONDITION SENSE TAG)
  (P2BRANCH-for-k CONDITION 'k:r0               ;'D-INDS
            `(BRANCH NILIND ,SENSE NIL ,TAG)))

;;; Like P2, but also supply a "branch destination".
;;; The branch destination (*BDEST*) is just a branch instruction which
;;; could simple-mindedly be compiled right after (P2-for-k FORM DEST),
;;; but some forms can optimize the code produced by incorporating
;;; the branch destination into their code.  Such forms can say that
;;; outputting the branch at the end is superfluous by setting *BDEST* to NIL.
;;; Forms which perform unconditional transfers need not worry about *BDEST*
;;; since it will be output and then discarded as unreachable.

;;; An unconditional branch destination can accompany any value of DEST.
;;; A conditional branch should only be used with DEST = K:R0 (was D-INDS).
;;; This is taken to imply that the indicators are used by the branch,
;;; not that the indicators will be correctly set up after the optimized
;;; code is finished branching or not.  If you wish to compile something
;;; and want the indicators correctly set up according to its value,
;;; you should use K:R0 (was D-INDS) with no *BDEST*, and do your branching yourself.

;;; Branches which pop the pdl may not be used as branch destinations.
;;; Most people who look at *BDEST* don't check for them,
;;; and the optimizations that *BDEST* is used for wouldn't work for them anyway.

;;; A funny kind of branch that can be used as a destination is
;;; (BRANCH ALWAYS NO-OP NIL tag).  It is a sort of unconditional branch,
;;; used when the tag to be branched to is known to be right after
;;; this expression, so that one might think that no branch is needed at all.
;;; When OUTB-for-k is called on such a branch, it does nothing.
;;; But some functions (such as AND and OR) can optimize these no-op branches
;;; like any other unconditional branches.

;;; An even funnier kind of branch destination is the return branch:
;;; (BRANCH ALWAYS RETURN NIL tag).  This is given as the branch destination
;;; to the last statement in a PROG, so that if the statement is a RETURN
;;; then the implicit (RETURN NIL) at the end of the PROG can be omitted
;;; and the RETURN at the end can just drop through to the PROG's rettag.
;;; Return branch destinations may not be passed along to subexpressions
;;; by AND, OR and COND.

(DEFUN P2BRANCH-for-k (FORM DEST *BDEST*)
  (COND ((AND *BDEST* (NEQ (CADR *BDEST*) 'ALWAYS)
              (NEQ DEST 'K:R0))         ;D-INDS
         (BARF `(,DEST . ,*BDEST*) "*BDEST* is conditional and DEST is not D-INDS" 'BARF))
        ;; We can optimize things like (AND 'T (GO FOO)) and (AND 'NIL (GO FOO))
        ;; into an unconditional jump or into nothing at all.
        ((AND (EQ (CADR *BDEST*) 'NILIND)
              (NULL (CADDDR *BDEST*))
              (NOT (ATOM FORM))
              (EQ (CAR FORM) 'QUOTE))
         (AND (EQ (NULL (CADR FORM))
                  (EQ (CADDR *BDEST*) 'TRUE))
              (OUTB-for-k `(BRANCH ALWAYS NIL . ,(COPY-LIST (CDDDR *BDEST*)))))
         (SETQ *BDEST* NIL))
        ((ADRREFP-for-k FORM)
         (p2-for-k form dest))
        (T (unless (symbolp dest)
             (fsignal "Complex destination ~S in P2BRANCH-FOR-K?" dest))
           (multiple-value-bind (new-dest source)
               (convert-dest-to-source dest)
             (P2F-for-k FORM new-dest)
             (if *BDEST* (outi-for-k `(k:move k:nop ,source k:boxed-right))))))
  (AND *BDEST* (OUTB-for-k (COPY-LIST *BDEST*))))

(defprop list-in-area p2list-cross p2-for-k)
(defprop list*-in-area p2list-cross p2-for-k)
(defun p2list-cross (argl dest &aux area)
  (declare (ignore dest))
  (when (memq *p2fn* '(list-in-area list*-in-area))
    (setq area (pop argl)))
  (fsignal "convert this"))

(DEFUN (:PROPERTY ATOM P2-FOR-K) (ARGL DEST) ;just treat ATOM normally.  This effectively overrides the normal P2.
  (P2MISC-FOR-K 'ATOM ARGL DEST 1))

;;; NOT compiles into a misc insn normally,
;;; but with a branch destination, it optimizes away by inverting the condition.
(DEFUN (:PROPERTY NOT P2-FOR-K) (ARGL DEST)
  (IF (OR (EQ (CADR *BDEST*) 'NILIND)
          (EQ (CADR *BDEST*) 'ATOMIND))
      (LET ((SENSE (OTHER (CADDR *BDEST*))))
        (P2BRANCH-for-k (CAR ARGL) DEST `(BRANCH ,(CADR *BDEST*) ,SENSE . ,(CDDDR *BDEST*)))
        (SETQ *BDEST* NIL))
    (P2MISC-for-k 'NOT ARGL DEST 1)))

(DEFUN gotag-frame-level (TAG)
  ;; Don't use GOTAGS-SEARCH because we want the warning
  ;; to appear only once, from OUTBRET or GTAG.
  (LET ((gotag (find TAG *GOTAG-ENVIRONMENT* :key #'gotag-prog-tag)))
    (IF GOTAG (progdesc-open-frames (GOTAG-progdesc GOTAG))
      nil)))

(DEFUN SIMPLEGOP-for-k (FORM)
  (AND (EQ (CAR-SAFE FORM) 'GO)
       (eql *open-frames*
            (gotag-frame-level (second form)))))

(DEFPROP AND P2ANDOR-for-k P2-FOR-K)
(DEFPROP OR P2ANDOR-for-k P2-FOR-K)

(DEFUN P2ANDOR-for-k (ARGL DEST)
  (LET ((SENSE (IF (EQ *P2FN* 'AND) 'TRUE 'FALSE)))
    (when (eq dest 'd-ignore)                   ;d-inds was in this list ??
      ;; compiling for predicate or effect
      (DO ()
          ((NOT (EQUAL (CAR (LAST ARGL))
                       (IF (EQ SENSE 'TRUE) ''T ''NIL))))
        (SETQ ARGL (BUTLAST ARGL))))
    ;; RETURN branches can't be passed in to the last thing in an AND.
    (AND (EQ (CADR *BDEST*) 'ALWAYS)
         (EQ (CADDR *BDEST*) 'RETURN)
         (SETQ *BDEST* NIL))
    ;; Any non-null constant as arg in an AND is ignorable unless it is last.
    ;; NIL as arg in an OR is always ignorable.
    (COND ((NULL ARGL))
          ((EQ SENSE 'FALSE)
           (SETQ ARGL (ZL:DELETE ''NIL ARGL)))
          (T
           (SETQ ARGL (NREVERSE (CONS (CAR (LAST ARGL))
                                      (DEL (LAMBDA (IGNORE X)
                                               (AND (EQ (CAR-SAFE X) 'QUOTE)
                                                    (CADR X)))
                                           NIL
                                           (CDR (NREVERSE ARGL))))))))
    (WHEN (NULL ARGL)
      (RETURN-FROM P2ANDOR-for-k (PROG1 (P2BRANCH-for-k `',(EQ SENSE 'TRUE) DEST *BDEST*)
                                        (SETQ *BDEST* NIL))))
    ;; If we are going to jump somewhere unconditionally after the AND,
    ;; things which are NIL might as well jump conditionally straight there.
    ;; But this only works if the value of the AND will be in the right place then.
    (MULTIPLE-VALUE-BIND (end-tag UNCONDITIONAL)
        (IF (AND (EQ (CADR *BDEST*) 'ALWAYS)
                 (MEMQ DEST '(k:r0 D-IGNORE)))  ;D-INDS probably should be register-p, D-PDL
            (VALUES (CAR (CDDDDR *BDEST*)) T)
            (VALUES (gensymbol "END-ANDOR") NIL))
      (let ((mv-tag nil))
        (COND ((EQ DEST 'D-IGNORE)
               ;; Compilation strategy for AND for effect:
               ;; compute each arg, using it only to jump to end if it's NIL.
               ;; The last one we just ignore, but we feed it our *BDEST* for
               ;; branch tensioning.  However, (AND form (GO tag)) can be optimized
               ;; by making it a conditional jump to tag rather than a jump around a jump.
               (DO ((ARGL ARGL (CDR ARGL)))
                   ((NULL (CDR ARGL))
                    (P2BRANCH-for-k (CAR ARGL) DEST *BDEST*))
                 (AND (SIMPLEGOP-for-k (CADR ARGL))
                      (RETURN (BOOL1-for-k (CAR ARGL) (OTHER SENSE) (GTAG (CADADR ARGL)))))
                 ;; If the next arg of this AND is NIL, this arg is effectively last.
                 ;; However, if AND has a branch destination, it must compute
                 ;; whether to branch based on the NIL, not on this arg.
                 (AND (EQ (CAR-SAFE (CADR ARGL)) 'QUOTE)
                      (EQ (NULL (CADADR ARGL))
                          (EQ SENSE 'TRUE))
                      (RETURN (P2BRANCH-for-k (CAR ARGL) DEST *BDEST*)))
                 (BOOL1-for-k (CAR ARGL) SENSE end-tag)))
              ((AND (symbolp dest) (EQ (CADR *BDEST*) 'NILIND))
               ;; Compilation strategy for AND followed by jump if NIL:
               ;; jump compute each value and jump THERE rather than to end if NIL.
               ;; Compilation strategy for AND followed by jump if not NIL:
               ;; put that jump if not NIL after the last thing in the AND
               ;; and go to after that if anything else fails to be non-NIL.
               (IF (EQ SENSE (CADDR *BDEST*))
                   (DO ((ARGL ARGL (CDR ARGL)))
                       ((NULL ARGL))
                     (P2BRANCH-for-k (CAR ARGL) DEST *BDEST*))
                 (DO ((ARGL ARGL (CDR ARGL)))
                     ((NULL (CDR ARGL))
                      (P2BRANCH-for-k (CAR ARGL) DEST *BDEST*))
                   ;; If the next arg of this AND is NIL, this arg is effectively last.
                   ;; Also, *BDEST* can be flushed since it says branch if
                   ;; not NIL and we now know the value of the AND is always NIL.
                   (AND (NOT (ATOM (CADR ARGL)))
                        (EQ (CAADR ARGL) 'QUOTE)
                        (EQ (NULL (CADADR ARGL))
                            (EQ SENSE 'TRUE))
                        (RETURN (P2-for-k (CAR ARGL) DEST)))
                   (BOOL1-for-k (CAR ARGL) SENSE end-tag)))
               (SETQ *BDEST* NIL))
              (T
               ;; Compilation strategy for AND for value
               ;; (correct indicators required counts as for value):
               ;; AND for multiple values is like AND for value.
               (DO ((ARGL ARGL (CDR ARGL))
                    (BRANCH `(BRANCH NILIND ,SENSE nil ,end-tag)))
                   ((NULL (CDR ARGL))
                    ;; Compile the last form.  If we want multiple values
                    ;; and it handles them, then say the AND is handling them.
                    (COND ((not (symbolp dest))
                           (p2-for-k (car argl) dest)
                           (setq mv-tag (gensymbol "ANDOR-MV")))
                          (UNCONDITIONAL
                           (P2BRANCH-for-k (CAR ARGL) DEST *BDEST*)
                           (SETQ *BDEST* NIL))
                          (T
                           (P2-for-k (CAR ARGL) dest))))
                 (P2-for-k (CAR ARGL) 'k:r0)
                 (outi-for-k `(k:move k:nop k:r0 k:boxed-right))
                 (when (SIMPLEGOP-for-k (CADR ARGL))
                   (RETURN (OUTB-for-k `(BRANCH NILIND ,(OTHER SENSE) NIL ,(GTAG (CADADR ARGL))))))
                 (OUTB-for-k (COPY-LIST BRANCH)))))
        (COND (mv-tag
               ;; If we want multiple values, and the last form provides them,
               ;; say that the AND provides them,
               ;; and arrange to produce some in every other path.
               (OUTB-for-k `(BRANCH ALWAYS NIL NIL ,mv-tag))    ;Last form jumps around.
               (OUTTAG-FOR-K end-tag NIL)               ;Other paths come here.
               ;; We compiled the individual clauses to K:R0.  If this OR and we terminated
               ;; early, we just fetch the value from there.  If it's AND, we just use the
               ;; NIL g.r.
               (let ((target (if (eq sense 'true)               ;true if AND.
                                 'gr:*nil*
                               'k:r0)))
                 (outi-for-k `(k:move ,dest ,target k:boxed-right) :single-value))
               (OUTTAG-FOR-K mv-tag NIL))               ;Last form jumps here.
              ((NOT UNCONDITIONAL)
               (OUTTAG-FOR-K end-tag NIL)
               (let ((target (if (eq sense 'true)               ;true if AND.
                                 (k-find-constant-register nil)
                               'k:r0)))
                 (cond ((neq dest 'd-ignore)
                        (outi-for-k `(k:move ,dest ,target k:boxed-right) :single-value))))
               )
              (t (OUTTAG-FOR-K end-tag NIL)
                 (ferror "look at this"))))
      NIL)))



(DEFUN (:PROPERTY LET* P2-for-k) (ARGL DEST)
  (LET ((*VARS* (CADR ARGL)))
    (opening-frames (dest (P2SBIND-for-k (CAR ARGL) (CADDR ARGL) *VARS*))
      (P2LET-INTERNAL-for-k *VARS* ARGL DEST))))

(DEFUN (:PROPERTY LET P2-for-k) (ARGL DEST)
  (LET ((*VARS* (CADR ARGL)))
    (opening-frames (dest (P2PBIND-for-k (CAR ARGL) (CADDR ARGL)))
      (P2LET-INTERNAL-for-k *VARS* ARGL DEST))))

(DEFUN (:PROPERTY LET-FOR-LAMBDA P2-for-k) (ARGL DEST)
    (LET ((OVARS *VARS*)
          (*VARS* *VARS*))
      (opening-frames (dest (P2PBIND-for-k (CAR ARGL) (CADDR ARGL)))
        (PROCESS-SPECIAL-DECLARATIONS (CADR ARGL))
        (P2LET-INTERNAL-for-k OVARS ARGL DEST))))

;;; Compile the body of a LET.  The variable binding has already been done
;;; by P1PBIND or P1SBIND, which substituted a new destination which will unbind
;;; the special bindings made.
(DEFUN P2LET-INTERNAL-for-k (OVARS ARGL DEST)
  (ignore ovars)
  (LET* ((*VARS* (THIRD ARGL))
         (BDY (NTHCDR 6 ARGL)))
    ;; If %BIND is used within this LET, and it's an internal LET,
    ;; we must push the specpdl index at entry so we can unbind to it later.
    ;; This is not needed for D-RETURN since function exit pops all bindings.
    (UNLESS BDY (SETQ BDY '('NIL)))
    (DO ((TAIL BDY (CDR TAIL)))
        ((NULL (CDR TAIL))
         (p2-for-k (car tail) dest))
      (P2-for-k (CAR TAIL) 'D-IGNORE))))

;;;; Compile a BLOCK.

;;; A BLOCK has no user-defined GOTAGS, but it does have one tag at this level: its rettag.
(DEFUN (:PROPERTY BLOCK P2-for-k) (ARGL DEST)
  (P2BLOCK-for-k ARGL DEST NIL))

(DEFUN (:PROPERTY BLOCK-FOR-PROG P2-for-k) (ARGL DEST)
  (P2BLOCK-for-k ARGL DEST T))

(DEFUN P2BLOCK-for-k (ARGL DEST &OPTIONAL ALSO-BLOCK-NAMED-NIL)
  (LET* ((OLDGOTAGS *GOTAG-ENVIRONMENT*)
         (*GOTAG-ENVIRONMENT* (CAR ARGL)) (MYPROGDESC (CADR ARGL)) (BDY (CDDR ARGL))
         (*PROGDESC-ENVIRONMENT* *progdesc-environment*)
         (PROGNAME (PROGDESC-NAME MYPROGDESC))
         (RETTAG (PROGDESC-RETTAG MYPROGDESC)))
    ;; Add this block to the stack of entered ones.
    (SETF (PROGDESC-IDEST MYPROGDESC) DEST)
    (SETF (PROGDESC-NBINDS MYPROGDESC) 0)
    (setf (progdesc-open-frames myprogdesc) *open-frames*)
    (PUSH MYPROGDESC *PROGDESC-ENVIRONMENT*)
    ;; For PROG, add a block named NIL also.
    (WHEN (AND ALSO-BLOCK-NAMED-NIL (NEQ PROGNAME 'T))
      (PUSH (COPY-PROGDESC MYPROGDESC) *PROGDESC-ENVIRONMENT*)
      (SETF (PROGDESC-NAME (CAR *PROGDESC-ENVIRONMENT*)) 'NIL))
    ;; Set the GOTAG-PDL-LEVEL of the rettag.
    ;; *GOTAG-ENVIRONMENT* at this moment contains the RETTAG and nothing else.
    (SETF (GOTAG-PROGDESC (CAR *GOTAG-ENVIRONMENT*))
          (CAR *PROGDESC-ENVIRONMENT*))
    (SETF *GOTAG-ENVIRONMENT*
          (APPEND *GOTAG-ENVIRONMENT* OLDGOTAGS))
    ;; Generate code for the body.
    (IF (NULL BDY)
        (P2RETURN1-for-k '('NIL) PROGNAME)
      (DO ((TAIL BDY (CDR TAIL)))
          ((NULL (CDR TAIL))
           (P2RETURN1-for-k (LIST (CAR TAIL)) PROGNAME))
        (P2-for-k (CAR TAIL) 'D-IGNORE)))
    ;; If this is a top-level BLOCK, we just went to D-RETURN,
    ;; and nobody will use the RETTAG, so we are done.
    (IF (EQ DEST 'D-RETURN)
        NIL
      ;; Otherwise, this is where RETURNs jump to.
      (OUTTAG-for-k RETTAG nil))))

;;;; Various types of RETURN.

;(DEFUN (:PROPERTY RETURN-LIST P2-for-k) (ARGL IGNORE)
;  (P2RETURN1-for-k `((VALUES-LIST ,(CAR ARGL))) NIL))

(DEFUN (:PROPERTY RETURN-FROM P2-for-k) (ARGL IGNORE)
  (P2RETURN1-for-k (CDR ARGL) (CAR ARGL)))

;;; (RETURN-FROM-T <value>) is like (RETURN-FROM T <value>).
(DEFUN (:PROPERTY RETURN-FROM-T P2-for-k) (ARGL IGNORE)
  (P2RETURN1-for-k ARGL T))

(defun p2return1-for-k (ARGL PROGNAME)
  (let ((rpdesc (find progname *progdesc-environment* :key #'progdesc-name)))
    (unless rpdesc
      (ferror "Internal compiler error: BLOCK environments randomized."))
    (let ((*open-frames* *open-frames*))
      (p2-for-k (if (= (length argl) 1)
                    (first argl)
                  `(values ,@argl))
                rpdesc))
    (unless (d-return-p rpdesc)
      (let ((gotag (gotags-search (progdesc-rettag rpdesc))))
        (when gotag
          (outb-for-k `(branch always nil nil ,(gotag-lap-tag gotag))))))))


(DEFUN (:PROPERTY TAGBODY P2-for-k) (ARGL PROGDEST)
  (LET* ((MYGOTAGS (CAR ARGL))
         (*GOTAG-ENVIRONMENT* *GOTAG-ENVIRONMENT*)
         (*WITHIN-POSSIBLE-LOOP* *WITHIN-POSSIBLE-LOOP*)
         (MYPROGDESC (cadr argl))
         (BODY (CDDR ARGL))
         (*PROGDESC-ENVIRONMENT* *PROGDESC-ENVIRONMENT*))
    (setf (progdesc-open-frames myprogdesc) *open-frames*)
    ;; Remember this TAGBODY's general environment.
    ;; We supply as the supposed block name
    ;; a list that will not appear as the block name in any RETURN-FROM.
    ;; So we can have an entry on the *PROGDESC-ENVIRONMENT* list to record our tags' pdllvl
    ;; without interfering with RETURN-FROM.
    (WHEN MYGOTAGS
      (PUSH MYPROGDESC *PROGDESC-ENVIRONMENT*)
      (SETQ *GOTAG-ENVIRONMENT* (APPEND MYGOTAGS *GOTAG-ENVIRONMENT*)))
    (DOLIST (STMT BODY)
      (COND ((ATOM STMT)
             (OR *DROPTHRU* (OUTF-for-k '(NO-DROP-THROUGH)))
             (SETQ *TAGOUT* (SETQ *DROPTHRU* T))
             (SETQ *WITHIN-POSSIBLE-LOOP* T)
             (OUTTAG-for-k (GTAG STMT) t))
            (T (P2-for-k STMT 'D-IGNORE))))
    (P2-for-k ''NIL PROGDEST)))

(DEFUN (:PROPERTY GO P2-for-k) (ARGL IGNORE)
  (COND ((NULL *PROGDESC-ENVIRONMENT*)
         (WARN 'BAD-PROG ':IMPOSSIBLE
               "There is a ~S to ~S not within any ~S."
               'GO (CAR ARGL) 'TAGBODY))
        ((OR (SYMBOLP (CAR ARGL))
             (NOT (%POINTERP (CAR ARGL))))
         (outbret-for-k (car argl)))
        (T
         (WARN 'BAD-PROG ':IMPOSSIBLE
               "The argument of ~S was ~S, not a symbol."
               'GO (CAR ARGL)))))

(DEFUN (:PROPERTY GO-HACK P2-for-k) (ARGL IGNORE)
  (OUTB-for-k `(BRANCH ALWAYS NIL NIL ,(GOTAG-LAP-TAG (CAR ARGL)))))

;;; Output an unconditional transfer to the specified prog tag,
;;; popping the pdl the appropriate number of times to adjust the
;;; pdl from its current level to the level required at that tag.

;;; For handling GO, PROGDESC should be NIL.
;;; When jumping to the return tag of a prog, PROGDESC should be
;;; the desc for the prog we are returning from.
(defun outbret-for-k (tag)
  (let ((gotag (gotags-search tag)))
    (when gotag
      ;; If this is GO, set EXITPROGDESC to the progdesc of its containing PROG
      (discarding-open-frames ((progdesc-open-frames (gotag-progdesc gotag)) 'k:r0)
        (outb-for-k `(branch always nil nil ,(gotag-lap-tag gotag))))
      )))

;;; Output a BRANCH instruction
;branch looks like (branch <condition> <sense> <pop-if-no-jump> adr)
; CONDITION can be always, NILIND.
; sense can be TRUE or FALSE.
; POP-IF-NO-JUMP should always be NIL.
(DEFUN OUTB-for-k (X)
  (COND ((not (null (fourth x))) (ferror nil "pop-if-no-jump not nil"))
        ((EQ (CADDR X) 'NO-OP))
        ((EQ (CADDR X) 'RETURN))
        ((NULL *DROPTHRU*))
        (T (PUTPROP (CAR (LAST X)) T 'USED)
           ;(OUTF-for-k X)
           (convert-branch-for-k x)
           )))

(defun outj-for-k (x)   ;output 2 element unconditional branch or jump
  (putprop (car (last x)) t 'used)
  (outf-for-k x)
  (setq *dropthru* nil))

(defun convert-branch-for-k (branch)
  (cond ((eq (cadr branch) 'always)
         (outj-for-k `(k:unconditional-branch ,(car (last branch)))))
        ((eq (cadr branch) 'nilind)
         (let ((inst (cond ((eq (third branch) 'true) 'k:br-zero)
                           ((eq (third branch) 'false) 'k:br-not-zero)
                           (t (ferror nil "bad sense")))))
           (outi-for-k `(k:test ,inst))
           (outi-for-k `(k:branch ,(car (last branch))))
           (putprop (car (last branch)) t 'used)))
        (t (ferror nil "bad condition"))))


(DEFUN (:PROPERTY SETQ P2-for-k) (ARGL DEST)
  (PROG ()
        (OR ARGL (RETURN (p2-for-k '(quote nil) dest)))
     LOOP
        (P2SETQ-1-for-k (CAR ARGL) (CADR ARGL)
                        (COND ((NULL (CDDR ARGL)) DEST)
                              (T 'D-IGNORE)))
        (SETQ ARGL (CDDR ARGL))
        (AND ARGL (GO LOOP))))

;;; Compile code to set VAR to the result of computing VALUE,
;;; and also move that value to DEST.
(DEFUN P2SETQ-1-for-k (VAR VALUE DEST)
  (COND ((MEMQ VAR '(NIL T))
         NIL)
        ((AND (CONSP VAR) (EQ (CAR VAR) 'LEXICAL-REF))
         (movem-and-move-to-dest-for-k value var dest))
        (T
         (movem-and-move-to-dest-for-k value var dest)))
  NIL)

(defun movem-and-move-to-dest-for-k (value var dest)
  ;compile VALUE, storing the result in VAR, and also in DEST.
  ; VAR is a REF type construct.
  ; DEST is any dest acceptable to P2-FOR-K. NOTE: that means its an INTERNAL dest!!
 ;** idea for special hack..  Change these k:r0 s to k:r15.  Then peephole optimizer
 ; could assume the contents on any k:r(n) is used at most once and do some optimizations
 ; (particularily wrt MD).  Cheap flow analysis.
  (let ((inter-var
          (when (and (listp var) (memq (car var) '(local-ref lexical-ref)))
            (intermediate-dest-for-store (var-lap-address (cadr var))))))
    (multiple-value-bind (inter-dest inter-reg)
        (compute-temporary-destination var inter-var)
      (p2-for-k value inter-dest)
      ;salt it in var, if necessary.
      (cond ((and (symbolp var)
                  ;; @#$@#$ This should be NC::REGISTER or COMPILER::REGISTER
                  (get var :register))
             (outi-for-k `(k:move ,var ,inter-reg k:boxed-right) :single-value))
            ((atom var)
             ;; Store from inter-reg into special variable
             ;; Let POST-PROCESS make OPEN-CALL, otherwise it gets faked out.
             (let ((tail-p (tail-call-p dest 'symbol:%%set)))
               (tail-call-open 'symbol:%%set tail-p #'discard-temporary-frame
                               :single-value)
               (outi-for-k `(k:movei k:o0 (quote ,var) k:boxed))
               (outi-for-k `(k:move k:o1 ,inter-reg k:boxed-right))
               (return-from movem-and-move-to-dest-for-k
                 (maybe-tail-call `(symbol:%%set 2) dest :single-value tail-p)))
             ;calling subroutine anyway, and %%set returns as value the right thing.
             ;so putting dest in subroutine call solves whole problem and avoids any
             ;possibility of K:R0 bashage.
             )
            ((eq (car var) 'local-ref)
             (let ((lap-address (var-lap-address (cadr var))))
               (finish-store inter-reg lap-address dest)))
            ((eq (car var) 'lexical-ref)
             (let* ((tail-p (tail-call-p dest))
                    (ref-spec (second var))
                    (level (ldb (byte 12. 12.) ref-spec))
                    (offset (ldb (byte 12. 0.) ref-spec)))
               (tail-call-open 'li:closure-set tail-p #'discard-temporary-frame
                               :single-value)
               (closure-set-call level offset inter-reg dest tail-p)))
            (t (ferror nil "Cant finish store")))
      ;salt it in dest, if necessary.
      (cond ((eq dest 'd-ignore) nil)
            ((eq dest inter-reg) nil)   ;already there.
            ((k-destination-p dest)
             (outi-for-k `(k:move ,dest ,inter-reg k:boxed-right)))
            (t (outi-for-k `(k:move ,dest ,inter-reg k:boxed-right)))))))


(DEFUN OUTTAG-for-k (X branch-back-possible)
  (WHEN (or branch-back-possible (GET X 'USED))
    (unless *DROPTHRU*
      ;; Always put this out, so use OUTF, not OUTF-FOR-K
      (OUTF '(NO-DROP-THROUGH)))
    (SETQ *DROPTHRU* T)
    (OUTF-for-k X)))


(defun clean-up-open-frames (level type k-dest)
  (when (eq k-dest 'd-return)
    ;;-- no more k-dest should never be an internal dest. --
    ;; This should only happen when called from OPENING-FRAMES, when the dest was D-RETURN.
    ;; In that case, the code that was output should have cleaned up the stack, no?
    (fsignal "lose"))
  (let ((new-frame-p))
    (case type
      ((:return)
       (unless (null level)
         (error "Trying to return, but not cleaning off the stack?"))
       (unless (or (d-return-p k-dest)
                   (eql k-dest 'k:o0))                        ;k:o0 is used for passing back values
                                                              ;during intermediate cleanups
         (error "Destination isn't a return destination, but we're trying to return?"))))
    (when (and (listp k-dest)
               (memq (car k-dest) '(k:new-open k:new-tail-open)))
      ;; If we're to create a new frame, note it, so we can not be suprised when it appears
      ;; on the stack!  If new-frame-p, we're being asked to create the frame.  So the frame
      ;; *UNDER* the top frame should be LEVEL, not the top frame.  In other words, we should
      ;; be cleaning everything off the stack except for the new frame.
      (setq new-frame-p t))
    (if (not (or (null level)
                 (loop for l on *open-frames*
                       thereis (eq l level))))
        (reg-error "Internal compiler error:  over-popped the open frames.")
      (if (eq level *open-frames*)
          (when new-frame-p
            (Reg-error "Internal compiler error:  failed to create new frame."))
        (loop for l = *open-frames*
              until (eq (cdr l) level)
              when (null l)
                do (return (reg-error "Internal compiler error:  ran off the end of the frame list."))
              do (outi-close-for-k l 'k:o0 type)
              finally
              (unless new-frame-p
                ;; The final one goes to the final destination, unless that's to create a new frame.
                ;; In that case, we leave this frame for our caller (P2ARGC-FOR-K), because this is
                ;; the frame he asked to be created.  He will close it when he's done with it.
                ;; We know it's the one he asked for, because the one just under it was the level
                ;; we were "cleaning up" down to.
                (outi-close-for-k l k-dest type)))))
    (unless (if new-frame-p
                (and *open-frames* (eql level (cdr *open-frames*)))
              (eql level *open-frames*))
      (reg-error "Internal compiler error:  failed to pop the right number of open frames."))))


(defun check-not-open-and-return (x)
  (cond ((memq (caddr x) *return-destinations*)
         (error "Internal compiler error -- simultaneously opening a frame and returning.")))  )

;;; This is the other piece of what WITH-OPEN-FRAME does.
;;; It returns the new OPEN-FRAME, so we can check for the right thing later.
(defun outi-open-for-k (open-instruction tail-p cleanup-generator &optional source-type (description open-instruction))
  (prog1 (outi-open-for-k-internal description tail-p nil       ;not there until outi-for-k sees it.
                                   cleanup-generator)
         (outi-for-k open-instruction source-type)))

;;; This is like the above, but doesn't do the output of the instruction.
;;; This is so it can be called from inside OUTI-FOR-K.
(defun outi-open-for-k-internal (open-instruction tail-p there-p cleanup-generator)
    (when tail-p
      (when *open-frames*
        (error "Internal compiler error:  An attempt was made to open for tail-call~@
                when there were already open frames.")))
    (let ((new-frame (make-open-frame :open-instruction open-instruction
                                      :tail-p (not (null tail-p))
                                      :there-p there-p
                                      :cleanup-generator cleanup-generator)))
      (push new-frame *open-frames*)
      new-frame))

;;; This is like OUTI-OPEN-FOR-K-INTERNAL, but doesn't actually involve an instruction
;;; being output.  It's for operations which create a "logical" frame without involving
;;; an actual physical frame, such as stack slots, or bindings, or stack-allocated arrays,
;;; or any other stack-discipline hair that isn't reflected in OUTI-FOR-K's monitoring of
;;; the instruction stream.

(defun outi-fake-open-for-k-internal (operation-name tail-p there-p cleanup-generator)
 ; (if *new-open-frame*
 ;     (error "The last open frame was never actually output.") )
    (when tail-p
      (when *open-frames*
        (error "Internal compiler error:  An attempt was made to open for tail-call~@
                when there were already open frames.")))
    (labels ((fake-cleanup (open-frame operation dest)
              (funcall cleanup-generator open-frame operation dest)
              ;; Since OUTI-FOR-K doesn't know anything about this, we have to do it.
              (ecase operation
                ((:return :discard)
                 (pop *open-frames*)))))
      (let ((new-frame (make-open-frame :open-instruction operation-name
                                        :tail-p (not (null tail-p))
                                        :there-p there-p
                                        :cleanup-generator #'fake-cleanup)))
        (push new-frame *open-frames*)
        new-frame)))


;;; This is what does the normal completion of an open frame.

(defun outi-close-for-k (frame-level k-dest action &rest others)
  (if (or (null *open-frames*) (not (eq frame-level *open-frames*)))
      (reg-error "Mismatch of frame levels; we're not finishing the frame we started.")
    (apply (open-frame-cleanup-generator (car *open-frames*))
           (car *open-frames*) action k-dest others)))

;;; This cleans up a tail-open.  To do this, we do:
;;; (TAIL-CALL (0 0) NIL NEXT-PC-PC+1)
;;; (TAIL-OPEN-CALL (0 0) NIL NEXT-PC-PC+1)
;;; The analysis is:
;;;                   O A R
;;; Initial state:    A A R  ; O had better have the same as A, on *ANY* tail-call.
;;; Tail open:        F A R  ; F is new from heap.
;;; Tail open:        F F A  ; R goes to heap.
;;; Tail open-call:   A A F  ; Once again in a valid state; with a different R reg.

(defun discard-tail-call-frame ()
  (outf-for-k '(k:tail-call (0 0) k:next-pc-pc+1))
  (outf-for-k '(k:tail-open-call (0 0) nil k:next-pc-pc+1)))

(defmacro compiling-to-destination ((dest source source-type) &body body)
  (let ((original-dest (gensymbol "ORIGINAL-DEST"))
        (source-var (gensymbol "SOURCE"))
        (source-type-var (gensymbol "SOURCE-TYPE")))
    `(let ((,original-dest ,dest)
           (,source-var ,source)
           (,source-type-var ,source-type))
       (multiple-value-bind (,dest ,source-var)
           (compute-temporary-destination ,original-dest ,source-var)
         ;; DEST is now the intermediate destination.
         ;; SOURCE-VAR is how you back get to that value.  (Usually
         ;; it's the same, but it can be a quoted constant or constant
         ;; register.)
         (multiple-value-prog1
           (progn ,@body)
           ;; Now move from the intermediate to the final value, if needed.
           (move-to-final-destination ,original-dest ,source-var ,source-type-var))))))

(deftype functional-dest ()
  `(satisfies functional-dest-p))

(defun functional-dest-p (dest)
  (case dest
    (k:nop t)                                   ;As far as we're concerned.
    (otherwise (nc::functional-dest-p dest))))

(deftype register-dest ()
  `(satisfies register-dest-p))

(defun register-dest-p (dest)
  (typecase dest
    (list (case (first dest)
            (k:register t)))
    (symbol
     (or (find dest
               #(k:a0 k:a1 k:a2 k:a3 k:a4 k:a5 k:a6 k:a7
                      k:a8 k:a9 k:a10 k:a11 k:a12 k:a13 k:a14 k:a15
                      k:o0 k:o1 k:o2 k:o3 k:o4 k:o5 k:o6 k:o7
                      k:o8 k:o9 k:o10 k:o11 k:o12 k:o13 k:o14 k:o15
                      k:r0 k:r1 k:r2 k:r3 k:r4 k:r5 k:r6 k:r7
                      k:r8 k:r9 k:r10 k:r11 k:r12 k:r13 k:r14 k:r15))
         ;; @#$@#$ This should be NC::REGISTER or COMPILER::REGISTER
         (get dest :register)))
    (otherwise nil)))

(deftype return-dest ()
  `(satisfies return-dest-p))

(defun return-dest-p (dest)
  (typecase dest
    (symbol (or (memq dest *return-destinations*)
                (memq dest *internal-return-destinations*)))
    (otherwise nil)))

(deftype constant-register ()
  `(satisfies constant-register))

(defun constant-register (reg)
  (typecase reg
    (list
     (third (find reg nc:*global-constants*
                  :key #'cdr :test #'equal)))
    (symbol
       (third (find reg nc:*global-constants* :key #'third)))
    (otherwise nil)))

(deftype quoted-object ()
  `(satisfies quoted-object-p))

(defun quoted-object-p (ref)
  (typecase ref
    (list (case (first ref)
            ((quote function breakoff-function) t)
            (otherwise nil)))
    (otherwise nil)))

(deftype var-reference ()
  `(satisfies var-reference-p))

(defun var-reference-p (ref)
  (typecase ref
    (list (case (first ref)
            ((local-ref special-ref lexical-ref) t)
            (otherwise nil)))
    (otherwise nil)))

(deftype new-frame-dest ()
  `(satisfies new-frame-dest-p))

(defun new-frame-dest-p (reg)
  (typecase reg
    (list (case (first reg)
            ((k:new-open k:new-tail-open) t)
            (otherwise nil)))
    (otherwise nil)))


;;; Answers the important question:  Is this a source that can be moved
;;; arbitrarily later in the computation?

(defun constant-source-p (source)
  (and source
       (etypecase source
         ((member k:trap-pc+) nil)              ;Can't move this!
         (functional-dest nil)
         (quoted-object t)
         (var-reference nil)
         (constant-register t)
         (register-dest nil))))

;;; This also works on sources (i.e. QUOTE frobs.)
(defun register-static-across-opens-p (source)
 (etypecase source
   (register-dest
    ;; Exclude O-frames.
    (not (find source #(k:o0 k:o1 k:o2 k:o3 k:o4 k:o5 k:o6 k:o7
                             k:o8 k:o9 k:o10 k:o11 k:o12 k:o13 k:o14 k:o15))))
   ;; Exclude k:new-open, etc.
   (new-frame-dest nil)
   (functional-dest nil)
   (quoted-object t)
   (var-reference t)
   (var-reference t)))

;;; Compute where to place the value initially, before any further processing of this
;;; destination is done.  This returns a new destination, and a new source; these
;;; will usually be the same.  However, if the source is a constant, use that.

(defun compute-temporary-destination (dest &optional source)
  (declare (values new-dest new-source))
  (if (constant-source-p source)
      ;; K:IGNORE is the same as K:R0, except it says you're not going to use it.
      ;; This lets us not move things there.
      (values 'k:ignore source)
    (etypecase dest
      (new-frame-dest
       (if (and source
                (register-static-across-opens-p source))
           (values 'k:ignore source)
         (values 'k:r0 'k:r0)))
      (functional-dest
       (values 'k:r0 'k:r0))
      (register-dest
       (values dest dest))
      (multiple-values
       (if (null (multiple-values-values dest))
           (values 'k:ignore (k-find-constant-register nil))
         (compute-temporary-destination (first (multiple-values-values dest)))))
      (progdesc
       (compute-temporary-destination (progdesc-idest dest)))
      (open-frame
       (compute-temporary-destination (or (open-frame-pdest dest) 'gr:*save-return-crap-0*)))
      (var
       (let ((idest (intermediate-dest-for-store (var-lap-address dest))))
         (values idest idest)))
      (new-var (compute-temporary-destination (new-var-var dest) source))
      ((member d-ignore k:ignore)
       (values 'k:ignore (or source (k-find-constant-register nil))))
      ((member d-return k:return k:return-mv k:return-tail
               k:return-i k:return-i-mv k:return-i-tail)
       (values 'gr:*save-return-crap-0* 'gr:*save-return-crap-0*))
      ((member k:nop)
       (values 'k:ignore source))
      (symbol
       ;; A Special variable
       (values 'k:r0 'k:r0))
      (list
       (ecase (first dest)
         ((local-ref special-ref self-ref)
          (compute-temporary-destination (second dest) source))
         (lexical-ref 'k:r0))))))

;;; Analyze a source to see if it's better handled as a K:MOVEI or
;;; K:MOVE.  DEST enters into this if it's a register, because if the
;;; dest is a global register and the source is a constant or a constant
;;; global register, we'll be better off with a MOVEI.
;;; If it's a reference to a function, use K:MOVEA.  (I think).

(defun analyze-source (source dest)
  (declare (values source movei-p))
  (etypecase source
    (quoted-object
     (let ((reg (k-find-constant-register (second source))))
       (cond (reg
              ;; It can come from a global register, so recurse and do
              ;; that analysis.
              (analyze-source reg dest))
             ((memq (first source) '(function breakoff-function))
              (values `(,(second source) 0) 'k:movea))
             (t ;; It can't come from a global register, so always do K:MOVEI.
              (values source 'k:movei)))))
    (functional-dest
     (values source 'k:move))
    (list (ecase (car source)
            (k:register
             ;; Recurse with it as a symbol, so we can do the analysis there.
             (analyze-source (second source) dest))))
    (symbol
     ;; @#$@#$ This should be NC::REGISTER or COMPILER::REGISTER
     (let* ((source-set (second (get source :register)))
            (constant-val (when source-set
                            (find source nc:*global-constants*
                                       :key #'third))))
       (if (not source-set)
           ;; OK, no more than one global register.
           (values source 'k:move)
         ;; One or more global registers.
         (etypecase dest
           (null (values source 'k:move))
           ((or symbol functional-dest)
            ;; We can K:MOVEI *to* functional destinations.
            ;; @#$@#$ This should be NC::REGISTER or COMPILER::REGISTER
            (let ((dest-set (second (get dest :register))))
              (cond ((not dest-set)
                     ;; OK, only one global register.
                     (values source 'k:move))
                    ((= source-set dest-set)
                     ;; OK, from one place to another in the same set.
                     (values source 'k:move))
                    (constant-val
                     ;; The register is for a constant; we can use K:MOVEI
                     ;; as an alternative to an intermediate register.
                     (values `',(car constant-val) 'k:movei))
                    (t
                     ;; Otherwise we have to use an intermediate register
                     (values source 'k:r0)))))
           (new-frame-dest
            ;; k:new-open and friends.
            (values source 'k:move))
           (list
            ;; Recurse with the symbolic name of the dest.
            (ecase (first dest)
              (k:register (analyze-source source (second dest)))))))))))

(defun convert-dest-for-k (internal-dest)
  ;if dest represents an internal return destination, convert it to K form.
  (selectq internal-dest
    ;** not complete **
    (d-ignore 'k:ignore)
    (d-return 'k:return)
    (d-return-multiple-value 'k:return-mv)
    (d-return-tail 'k:return-tail)
    (otherwise internal-dest)))

(defun output-move (dest source &optional (flags '(k:boxed)))
  (unless (equal dest source)
    (unless (eq dest 'd-ignore)
      (unless (eq dest 'k:ignore)
        (multiple-value-bind (source movei-p)
            (analyze-source source dest)
          (let* ((boxed-flags (intersection '(k:boxed k:boxed-right) flags))
                 (other-flags (set-difference flags boxed-flags))
                 (kdest (convert-dest-for-k dest))
                 (dest-flags (case kdest
                               ((k:return k:return-tail k:return-mv
                                 k:return-i k:return-i-tail k:return-i-mv)
                                '(k:ch-return k:next-pc-return))))
                 (kflags (union dest-flags other-flags)))
            (ecase movei-p
              ((k:movea)
               (outf-for-k `(k:movea ,kdest ,source
                                     ,@(union kflags (when boxed-flags '(k:boxed))))))
              ((k:movei)
               (outf-for-k `(k:movei ,kdest ,source
                                     ,@(union kflags (when boxed-flags '(k:boxed))))))
              ((k:move)
               (outf-for-k `(k:move ,kdest ,source
                                    ,@(union kflags (when boxed-flags '(k:boxed-right))))))
              ((k:r0)
               (outf-for-k `(k:move k:r0 ,source
                                    ,@(when boxed-flags '(k:boxed-right))))
               (outf-for-k `(k:move ,kdest k:r0
                                    ,@(union kflags (when boxed-flags '(k:boxed-right)))))))))))))

;;; Compute the adjustment to the *OPEN-FRAMES* level that will be performed
;;; by a particular destination.
;;; Parallels MOVE-TO-FINAL-DESTINATION.
(defun compute-new-level-for-destination (dest)
  (etypecase dest
    (return-dest
     (setq *open-frames* nil))
    ((member k:ignore d-ignore)
     ;; Ignore it.
     nil)
    (functional-dest)
    (register-dest)
    (new-frame-dest
     (ecase (first dest)
       (k:new-open
        (note-open-frame nil))
       (k:new-tail-open
        (note-open-frame t))))
    (progdesc
     (setq *open-frames*
           (progdesc-open-frames dest))
     (compute-new-level-for-destination (progdesc-idest dest)))
    (multiple-values
     (let ((open-frame (multiple-values-open-frame dest)))
       (when open-frame
         (push open-frame *open-frames*)
         (note-open-frame open-frame))))
    (open-frame
     (pop *open-frames*)
     ;; Recurse to handle any further movement.
     (compute-new-level-for-destination (open-frame-idest dest)))
    (var)
    (new-var
     (let ((open-frame (new-var-open-frame dest)))
       (when open-frame
         (push open-frame *open-frames*)
         (note-open-frame open-frame))))))


;;; Output a move to a final (complex) destination, doing any cleanup and
;;; processing implied by that destination.

(defun move-to-final-destination (dest source source-type)
  (etypecase dest
    (return-dest
     (move-to-return dest source source-type))
    ((member k:ignore d-ignore)
     ;; Ignore it.
     nil)
    (functional-dest
     ;; Sometime should look at all of the individual implications of these!
     ;; Right now we only have to worry about MD.
     (output-move dest source))
    (register-dest (output-move dest source))
    (new-frame-dest (move-to-new-frame dest source source-type))
    (progdesc (move-to-progdesc dest source source-type))
    (multiple-values (move-to-multiple-values dest source source-type))
    (open-frame (move-to-open-frame dest source source-type))
    (var (move-to-var dest source source-type))
    (new-var (move-to-new-var dest source source-type))))

(defun move-to-return (dest source source-type)
  (let ((new-dest dest))
    (loop while *open-frames*
          do
          (multiple-value-setq (new-dest source)
            (outi-close-for-k *open-frames*
                              (if (null (rest *open-frames*)) dest source)
                              nil source source-type))))
  (unless (null *open-frames*)
    (reg-error "Frame cleanup for destination RETURN failed to remove all open frames."))
  (output-move dest source)
  (setq *dropthru* nil))

(defun move-to-new-frame (dest source source-type)
  (ignore source-type)
  ;;Must be K:NEW-OPEN or K:NEW-OPEN-TAIL
  (ecase (first dest)
    (k:new-open
     (destructuring-bind (type index &rest ignore) dest
       (ignore type)
       (outi-for-k `(k:open))
       (output-move (o-n index) source)))
    (k:new-tail-open
     (destructuring-bind (type index &rest ignore) dest
       (ignore type)
       (outi-for-k `(k:tail-open))
       (output-move (o-n index) source)))))

(defun move-to-progdesc (dest source source-type)
  (loop with idest = (progdesc-idest dest)
        until (eq *open-frames* (progdesc-open-frames dest))
        do
    (when (and (null *open-frames*)
               (progdesc-open-frames dest))
      (error "Fell off the bottom searching for the frame to return from."))
        (multiple-value-setq (idest source)
          (outi-close-for-k *open-frames* idest
                            :return source source-type))
        finally
        ;; Recurse to handle where this one goes to.
        (outi-for-k `(k:move ,idest ,source k:boxed-right) source-type)))

(defun move-to-multiple-values (dest source source-type)
  (let* ((dests (multiple-values-values dest))
         (dest0 (first dests))
         (dest-rest (rest dests)))
    (ecase source-type
      ((:last-value)
       ;; All the others have been put in place by the higher level.
       ;; Just see to it that the last one makes it home OK.
       (outi-for-k `(k:move ,dest0 ,source k:boxed-right) :single-value))
      ((:single-value :single-value-flag)
       ;; Fill the other values with NIL
       (loop with nil-reg = (k-find-constant-register nil)
             for i from 1
             for v in dest-rest
             do (outi-for-k `(k:move ,v ,nil-reg k:boxed-right)))
       (outi-for-k `(k:move ,dest0 ,source k:boxed-right)
                   :single-value))
      ((:subr-value :multiple-values-flag)
       (let ((n (length dests)))
         (cond ((= n 0))                        ;No values wanted, we're done.
               ((= n 1)
                ;; Only one value?  We shouldn't have come here.
                (unless (eq source dest0)
                  (outi-for-k `(k:move ,dest0 ,source k:boxed-right) :single-value)))
               ((< 1 n 7)
                (let ((routine (aref #(li:mvbind-1 li:mvbind-2 li:mvbind-3
                                       li:mvbind-4 li:mvbind-5 li:mvbind-6)
                                     (1- n))))
                  (outi-open-for-k `(k:open) nil #'discard-temporary-frame
                                   :single-value `(k:open (,routine 1)))
                  (outi-for-k `(k:move k:o0 ,source k:boxed-right) :single-value)
                  (outi-for-k `(k:call (,routine 1) ,dest0) :single-value)))
               (t
                ;; Start out opening the frame with the second value, so we can use
                ;; K:MOVEI and avoid having to worry about the boundary case where
                ;; we don't have large enough constants.
                (outi-open-for-k `(k:open) nil #'discard-temporary-frame
                                 :single-value '(k:open li:mv-bind 2))
                (outi-for-k `(k:movei k:o1 ',n k:boxed) :single-value)
                (outi-for-k `(k:move  k:o0 ,source k:boxed-right) :single-value)
                (outi-for-k `(k:call (li:mvbind-n 2) ,dest0) :single-value))))
       ;; Transfer all of the the values.
       (loop for d in dest-rest
             for i from 1
             for r = (k-find-return-register i)
             do (outi-for-k `(k:move ,d ,r k:boxed-right) :single-value)))
      ((:multiple-values)
       ;; *** This needs a new routine like LI:MVBIND-N that doesn't check the
       ;; *** hardware flag.  Except I bet compiling VALUES will actually set the
       ;; *** flag, so this case is probably useless.
       (error "Can't hack :MULTIPLE-VALUES source type.  I didn't think it existed.")))
    (let ((open-frame (multiple-values-open-frame dest)))
      (when open-frame
        (push open-frame *open-frames*)
        (note-open-frame open-frame)))))

(defun move-to-open-frame (dest source source-type)
  (unless (or (null (open-frame-pdest dest))
              (eq (open-frame-pdest dest) source))
    (outi-for-k `(k:move ,(open-frame-pdest dest) ,source k:boxed-right) :single-value))
  (multiple-value-setq (dest source)
    (outi-close-for-k *open-frames*
                      (open-frame-idest dest)
                      nil source source-type))
  ;; Now recurse to get the value the rest of the way.
  (unless (equal source dest)
    (outi-for-k `(k:move ,dest ,source k:boxed-right) :single-value)))

(defun move-to-var (dest source source-type)
  (ignore source-type)
  (finish-store source (var-lap-address dest)))

(defun move-to-new-var (dest source source-type)
  (ignore source-type)
  (let* ((var (new-var-var dest))
         (lap-adr (var-lap-address var))
         (open-frame (new-var-open-frame dest)))
    (bind-variable var source)
    (when open-frame
      (push open-frame *open-frames*)
      (note-open-frame open-frame))
    nil))


(DEFUN OUTI-for-k (X &optional source-type)
 ;every instruction emitted for K goes thru here.
 ;general rules: all instructions should always be in "simplest form".  The peephole will
 ;  do a good job of combining things, and otherwise it can be faked out.
 ;  in particular:  do not use k:open-call, etc.  always use separate k:open instructions.
 ;                  similarily, never use k:ch-open-call, etc.
 ;also do some fixups on the instructions:
 ;  on dest in *return-destinations* (i.e. k:return, etc) insert appropriate exit sequence.
 ;  on dest (k:new-open <n>), leave it in a call.
 ;                            refrob it for k:move or k:movei
 ;  on dest for MOVE or MOVEI <other list>, pass it thru (typically VMA, MD etc)
  (IF (NOT *DROPTHRU*)
      NIL
    (cl:case (car x)
      ((k:open)
       (check-not-open-and-return x)
       (note-open-frame nil)
       (outf-for-k x))
      ((k:tail-open)
       (check-not-open-and-return x)
       ;; Not necessarily PROG1, this may just be a RETURN from inside a tail-called function.
       (note-open-frame t)
       (outf-for-k x))
  ;do not output K:OPEN-CALL directly, instead let POST-PROCESS make it.  Otherwise it gets confused.
      ((k:call)         ;take destination in 3rd element
       (finish-open-frame nil)
       (destructuring-bind (opcode fspec dest &rest flags) x
         (when (listp dest)
           ;; Some special cases.
           (case (first dest)
             (k:register
              (setq dest (second dest)))
             (k:new-open (note-open-frame nil))
             (k:new-tail-open (note-open-frame t))))
         (compiling-to-destination (dest nil source-type)
           (when (memq dest *internal-return-destinations*)     ;note that dest, if it is a return-type frob,
             (fsignal "destination not converted"))             ; must be one of *return-destinations* as opposed to
                                                                ; *internal-return-destinations*
           (outf-for-k `(,opcode ,fspec ,dest ,@flags)))))
      ((k:move k:movei k:movea)
       (destructuring-bind (opcode dest source &rest flags) x
         ;; We treat K:MOVE and K:MOVEI exactly the same at this level.  At a lower level,
         ;; we decide whether to use K:MOVE or K:MOVEI based on the source.  We use
         ;; K:MOVE wherever possible to allow maximal merging of moves and calls.
         (ignore opcode)
         (compiling-to-destination (dest source source-type)
           ;; A substitute destination may be supplied if needed.
           (when (memq dest *internal-return-destinations*)
             (fsignal "destination not converted"))
           (unless (eq dest 'k:ignore)
             ;; Don't bother if it says we're not going to use it.
             (output-move dest source flags)))))
      ((k:tail-call)
       (unless (= (length *open-frames*) 1)
         (reg-error "Tail-calling, but there are un-disposed-of open frames."))
       (finish-open-frame t)
       (outf-for-k x)
       (setq *dropthru* nil))
      ((k:nop)
       (destructuring-bind (opcode &rest flags) x
         (ignore opcode flags)
         (outf-for-k x)))
      ((k:alu)
       (outf-for-k x))  ;for now, these had better not have any funny destinations, etc.
      ((k:alu-field)
       (destructuring-bind (opcode op dest left-source right-source spec &rest flags) x
         (compiling-to-destination (dest nil source-type)
           (outf-for-k `(,opcode ,op ,dest ,left-source ,right-source ,spec ,@flags)))))
      ((k:memory-wait)
       (outf-for-k x))
      ((k:test)
       (outf-for-k x))
      ((k:branch)
       (outf-for-k x))
      ((k:move-pc)
       (outf-for-k x))
      (otherwise
       (ferror nil "unknown instruction")))))


(defun output-specpdl-pop (number dest)
  (let ((tail-p (tail-call-p dest)))
    ;; Let POST-PROCESS make OPEN-CALL, otherwise it gets faked out.
    (case number
      (0)
      (1 (tail-call-open 'li:unbind-1 tail-p #'discard-temporary-frame nil)
         (maybe-tail-call '(li:unbind-1 0) 'k:ignore nil tail-p))
      (t (tail-call-open 'li:unbind tail-p #'discard-temporary-frame nil)
         (outi-for-k `(k:movei k:o0 (quote ,number) k:boxed))
         (maybe-tail-call '(li:unbind 1) 'k:ignore nil tail-p)))))


(defun make-dealloc-stack-slots (slots)
  (labels ((generate (open-frame operation dest &optional source source-type)
             (ignore dest source-type)
             (ecase operation
               (:exist (setf (open-frame-there-p open-frame) t))
               ((nil :discard :return)
                (generate-dealloc-stack-slots slots)
                (pop *open-frames*)
                (values dest source)))))
    (when (> slots 0)
      #'generate)))

(DEFUN OUTI1-for-k (X)                          ;Use this for outputing instructions
  (IF *DROPTHRU* (OUTS X)))                     ;known to take delayed transfers

(defun outf-for-k (x)
  (when (listp x)
    (case (first x)
      ((k:move k:movei)
       (when (eq (second x) 'd-return)
         (error "Unconverted D-RETURN in OUTF-FOR-K."))))
    (dolist (p x)
      (etypecase p
        ((or symbol integer))
        (list (case (first p)
                ((k:new-open k:new-open-tail)
                 (error "~S in OUTF-FOR-K." (first p))))))))
  ;; Discard any instructions while we're dropping out.
  (unless (and (not *dropthru*) (listp x))
    (outf x)))

(DEFUN (:PROPERTY PROGN-WITH-DECLARATIONS P2-for-k) (ARGL DEST)
  (LET ((*VARS* (CAR ARGL)))
    (progn-for-k DEST (CDR ARGL))))

(defun (:property progn p2-for-k) (argl dest)
  (progn-for-k dest argl))

(defun progn-for-k (dest argl)
  (loop for argx on argl
        for arg = (first argx)
        while (rest argx)
        do (p2-for-k arg 'd-ignore)
        finally
        (p2-for-k arg dest)))

(defun (:property prog2 p2-for-k) (argl dest)
  (p2prog12mv-for-k 2 dest argl nil))


(defun make-mv-mv-dest (dest)
  (declare (values new-dest n-values))
  (let ((n (typecase dest
             (return-dest 31.)
             (functional-dest 1.)
             ((member d-ignore k:ignore) 0)
             (var 1.)
             (register-dest 1)
             (multiple-values (length (multiple-values-values dest)))
             (otherwise 31.))))
    (values
      (case n
        (0 'd-ignore)
        (1 'gr:*save-return-crap-0*)
        (otherwise
         (make-multiple-values
           :values (subseq '(gr:*save-return-crap-0*
                              gr:*return-0* gr:*return-1* gr:*return-2*
                              gr:*return-3* gr:*return-4* gr:*return-5*
                              gr:*return-6* gr:*return-7* gr:*return-8*
                              gr:*return-9* gr:*return-10* gr:*return-11*
                              gr:*return-12* gr:*return-13* gr:*return-14*
                              gr:*return-15* gr:*return-16* gr:*return-17*
                              gr:*return-18* gr:*return-19* gr:*return-20*
                              gr:*return-21* gr:*return-22* gr:*return-23*
                              gr:*return-24* gr:*return-25* gr:*return-26*
                              gr:*return-27* gr:*return-28* gr:*return-29*)
                           0 n))))
      n)))

(defun p2prog12mv-for-k (n dest argl &optional mv-p)
  (decf n)                              ;Convert to origin 0.
  ;; Compile the args before the one whose value we want.
  (dotimes (i n)
    (p2-for-k (or (pop argl) ''nil) 'd-ignore))
  ;; Compile the arg whose value we want.
  (let ((tail-p (tail-call-p dest 'multiple-value-prog1)))
    (cond ((and (null (cdr argl)) *bdest*)
           (p2branch-for-k (or (car argl) ''nil) dest *bdest*)
           (setq *bdest* nil))
          ;; Stash the value to be returned in an OPEN frame.
          (mv-p
           ;; First, put all the values in the return-value registers.
           (multiple-value-bind (inter-dest inter-values)
               (make-mv-mv-dest dest)
             (p2-for-k (or (first argl) `'nil) inter-dest)
             ;; Now, create the frames.
             (when (> inter-values 0)
               (tail-call-open 'multiple-value-prog1 tail-p
                               (if mv-p
                                   (compute-finish-values-frame nil tail-p)
                                 #'discard-temporary-frame)
                               :multiple-values)
               (outi-for-k `(k:move k:o0 gr:*save-return-crap-0* k:boxed-right))
               (loop for i from 1 below (min 16. inter-values)
                     do (outi-for-k `(k:move ,(o-n i) ,(k-find-return-register i) k:boxed-right)))
               ;; Gotta always output this, for the count.
               (outi-open-for-k `(k:open) nil (compute-finish-values-frame-2 nil))
               (loop for i from 16. below (min 31. inter-values)
                     do (outi-for-k `(k:move ,(o-n (- i 16.))
                                             ,(k-find-return-register i) k:boxed-right)))
               ;; Salt away the count, too.
               (outi-for-k `(k:move k:o15 gr:*number-of-return-values* k:boxed-right))))
           ;; Compile the rest of the arguments.
           (dolist (arg (cdr argl))
             (p2-for-k arg 'd-ignore))
           (outi-close-for-k *open-frames* dest nil 'k:o0 :multiple-values)
           (outi-close-for-k *open-frames* dest nil 'k:o0 :multiple-values))
          (t (tail-call-open 'li:prog1-internal tail-p #'discard-temporary-frame
                             :single-value)
             (p2-for-k (or (first argl) `'nil) 'k:o0)
             (dolist (arg (cdr argl))
               (p2-for-k arg 'd-ignore))
             (outi-close-for-k *open-frames* dest nil 'k:o0 :single-value)))))

(defun discard-temporary-frame (open-frame operation dest &optional source source-type)
  (ignore open-frame operation)
  (let ((tail-p (open-frame-tail-p open-frame)))
    (ecase operation
      ((nil)
       (cond ((or (and (eq dest source)
                       (not (find dest #(k:o0 k:o1 k:o2 k:o3 k:o4 k:o5 k:o6 k:o7
                                              k:o8 k:o9 k:o10 k:o11 k:o12 k:o13 k:o14 k:o15))))
                  (eq dest 'd-return))
              (maybe-tail-call '(ignore 0) dest source-type tail-p)
              (values dest source))
             (t (outi-for-k `(k:move k:o0 ,source k:boxed-right))
                (maybe-tail-call '(li::prog1-internal 1) dest source-type tail-p)
                (values dest dest))))
      ((:exist)
       (cond ((open-frame-there-p open-frame)
              (fsignal "Frame already there"))
             (t (setf (open-frame-there-p open-frame) t))))
      ((:return :discard)
       (if tail-p
           (discard-tail-call-frame)
         (outi-for-k `(k:call (ignore 0) k:ignore)))
       (values dest source)))))


(defun fast-multiple-value-dest (dest &optional (start 0) end open-frame-p)
  (labels ((register-dynamic-across-opens-p (reg)
            (typecase reg
              (var (not (var-ok-p reg)))
              (new-var (not (var-ok-p (new-var-var reg))))
              (otherwise (not (register-static-across-opens-p reg)))))
           (var-ok-p (var)
            (let ((lap-address (var-lap-address var)))
              (case (first lap-address)
                (special
                 ;; If there are any intervening open frames, we cannot,
                 ;; because they might be an UNBIND.
                 (not open-frame-p))
                (otherwise t)))))
    (typecase dest
      (symbol
       (unless (or (memq dest *return-destinations*)
                   (memq dest *internal-return-destinations*))
         dest))
      (functional-dest nil)
      (new-frame-dest nil)
      (list
       (ecase (first dest)
         (k:register (second dest))))
      (multiple-values
       (unless (find-if #'register-dynamic-across-opens-p
                        (multiple-values-values dest)
                        :start start :end end)
         dest))
      (progdesc
       (fast-multiple-value-dest (progdesc-idest dest) start end
                                 (neq (progdesc-open-frames dest)
                                      *open-frames*)))
      (open-frame
       (cond ((open-frame-idest dest)
              (fast-multiple-value-dest (open-frame-idest dest) start end t))
             ((equal (open-frame-open-instruction dest)
                     '(k:open *throw))
              ;; Throw is a special case.  It always accepts multiple values, trivially.
              dest)))
      (var
       (when (var-ok-p dest)
         dest))
      (new-var
       (when (var-ok-p (new-var-var dest))
         dest))
      (otherwise nil))))

(defun output-fast-multiple-values (n real-dest dest source source-type start end)
  (case n
    (0 (outi-for-k `(k:move k:o0 gr:*nil* k:boxed-right))
       (values dest source))
    (otherwise
     (if (eq source 'k:o0)
         (values dest dest)
       (outi-for-k `(k:move k:o0 ,source k:boxed-right) source-type)
       (values dest source))))
  (etypecase dest
    ((or var new-var symbol)
     (outi-for-k `(k:call (li::prog1-internal 1) ,real-dest) :last-value)
     (values dest source))
    (multiple-values
     (loop with begin = (max start 1)
           for i from begin below end
           for o from 1
           for vdest in (nthcdr begin (multiple-values-values dest))
           do (outi-for-k `(k:move ,vdest ,(o-n o) k:boxed-right) :single-value))
     (outi-for-k `(k:call (li::prog1-internal 1) ,real-dest) :last-value)
     (values dest source))))


(defun output-slow-multiple-values (n dest source source-type function &optional tail-p)
  (unless n
    (setq n 16.))
  (cond ((or (and (eq dest source)
                  (not (find dest #(k:o0 k:o1 k:o2 k:o3 k:o4 k:o5 k:o6 k:o7
                                         k:o8 k:o9 k:o10 k:o11 k:o12 k:o13 k:o14 k:o15))))
             (eq dest 'd-return))
         (maybe-tail-call `(,function ,n) dest source-type tail-p)
         (values dest source))
        ((eq source 'k:o0)
         (maybe-tail-call `(,function ,n) dest source-type tail-p)
         (values dest dest))
        (t (outi-for-k `(k:move k:o0 ,source k:boxed-right))
           (maybe-tail-call `(,function ,n) dest source-type tail-p)
           (values dest dest))))

;;; N = number of values computed, or NIL to mean the number computed is in
;;; K:O15 of the innermost frame.  In this latter case, there will be a
;;; full complement of values in the O-registers, waiting to be moved as needed.
;;; FINISH-VALUES-2N will move this count to  GR:*NUMBER-OF-RETURN-VALUES*,
;;; which is where FINISH-VALUES-N will find it.
;;; This hair is to allow saving multiple-value blocks for MULTIPLE-VALUE-PROG1.

#+K ;;Doesn't belong in this file, here for illustration
(defun li:finish-values-N (v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15)
  ;; This could be optimized with a dispatch to only move those that count.
  (setq gr:*return-0* v1
        gr:*return-1* v2
        gr:*return-2* v3
        gr:*return-3* v4
        gr:*return-4* v5
        gr:*return-5* v6
        gr:*return-6* v7
        gr:*return-7* v8
        gr:*return-8* v9
        gr:*return-9* v10
        gr:*return-10* v11
        gr:*return-11* v12
        gr:*return-12* v13
        gr:*return-13* v14
        gr:*return-14* v15)
  (case gr:*number-of-return-values*
    (1 v0)
    (otherwise (hw:return-mv v0))))

#+K ;;Doesn't belong in this file, here for illustration
(defun li:finish-values-2n (v16 v17 v18 v19 v20 v21 v22 v23 v24 v25 v26 v27 v28 v29 v30 vcount)
  (setq gr:*return-15* v16
        gr:*return-16* v17
        gr:*return-17* v18
        gr:*return-18* v19
        gr:*return-19* v20
        gr:*return-20* v21
        gr:*return-21* v22
        gr:*return-22* v23
        gr:*return-23* v24
        gr:*return-24* v25
        gr:*return-25* v26
        gr:*return-26* v27
        gr:*return-27* v28
        gr:*return-28* v29
        gr:*return-29* v30
        gr:*number-of-return-values* vcount)
  v16)


(defun compute-finish-values-frame (n tail-p)
  (labels ((finish-values-frame (open-frame operation dest &optional source source-type)
            (ignore open-frame)
            (ecase operation
              ((nil)
               (let ((fast-dest (fast-multiple-value-dest dest)))
                 (if fast-dest
                     ;; The fast case cannot be d-return anyway, no need to hack tail-p
                     (output-fast-multiple-values n dest fast-dest source source-type 0 16.)
                   ;; Gotta go through the return temporaries.  Start out by setting up
                   ;; the count.
                   (outi-for-k `(k:movei gr:*number-of-return-values* ',n k:boxed))
                   (output-slow-multiple-values (when n (min n 16.))
                                                dest source source-type
                                                (if n 'finish-values
                                                  'finish-values-n)
                                                tail-p))))
              ((:exist)
               (cond ((open-frame-there-p open-frame)
                      (fsignal "Frame already there"))
                     (t (setf (open-frame-there-p open-frame) t))))
              ((:return :discard)
               (if (not tail-p)
                   (outi-for-k `(k:call (ignore 0) k:ignore))
                 (discard-tail-call-frame)
                 (pop *open-frames*))
               (values dest source)))))
    #'finish-values-frame))

(defun compute-finish-values-frame-2 (n)
  (labels ((finish-values-frame-2 (open-frame operation dest &optional source source-type)
            (ignore open-frame)
            (ecase operation
              ((nil)
               (let ((fast-dest (fast-multiple-value-dest dest)))
                 (etypecase fast-dest
                   (null (output-slow-multiple-values (when n (- n 16.))
                                                      'd-ignore 'k:o0 :single-value
                                                      (if n 'finish-values-2
                                                        'finish-values-2n)))
                   ((or var new-var symbol)
                    ;; A destination that only takes one, just discard the frame.
                    (outi-for-k `(k:call (ignore 0) k:ignore)))
                   (multiple-values
                    (let ((dest-17 (or (nth 16. (multiple-values-values fast-dest)) 'k:ignore)))
                      (output-fast-multiple-values n dest-17 fast-dest source source-type 17. 31.)))))
               (values dest source))
              ((:exist)
               (cond ((open-frame-there-p open-frame)
                      (fsignal "Frame already there"))
                     (t (setf (open-frame-there-p open-frame) t))))
              ((:return :discard)
               (outi-for-k `(k:call (ignore 0) k:ignore))))))
    #'finish-values-frame-2))

;;; This sort of thing is probably better done as a source transformation,
;;; but this trivial case is easily enough done here.  If all the values
;;; are ADRREFP-FOR-K, we can shuffle the first to be last, and then move them
;;; all directly in, except for the first value, which we may need to
;;; use a temporary for, so we can trigger any unwinding needed on the
;;; real destination.

(defun trivial-p2values (argl dest fast)
  (typecase fast
    (multiple-values
     (destructuring-bind (first-arg &rest rest-arg) argl
       (destructuring-bind (first-dest &rest rest-dest)
                           (multiple-values-values fast)
         (when first-arg
           (loop for argp = rest-arg then (rest argp)
                 for argi = (first argp)
                 for desti in rest-dest
                 do
                 (if argp
                     (p2-for-k argi desti)
                   (outi-for-k `(k:move ,desti gr:*nil* k:boxed-right)
                               :single-value)))
           (multiple-value-bind (new-dest new-source)
               (compute-temporary-destination first-dest)
             (p2-for-k first-arg new-dest)
             (outi-for-k `(k:move ,dest ,new-source k:boxed-right) :last-value))))))
    (open-frame
     (typecase fast
       (open-frame
        (unless (equal (open-frame-open-instruction fast) '(k:open *throw))
          (error "Only THROW open frames are a suitable destination for fast multiple values."))))
     (destructuring-bind (first-arg &rest rest-arg) argl
       (loop for arg in rest-arg
             for i from 1
             for dest = (k-find-return-register i)
             do
             (p2-for-k arg dest))
       ;; Output the count before the last (first) arg so that the last/first arg can
       ;; be incorporated into the call.
       (outi-for-k `(k:movei gr:*number-of-return-values* ',(length argl) k:boxed))
       (p2-for-k (or first-arg `'nil) 'k:o1)
       (outi-close-for-k (member fast *open-frames*) 'k:ignore nil 'k:o1 :last-value)))
    (otherwise
     ;; Only one value wanted anyway.
     (p2-for-k (first argl) dest))))

(defprop values p2values-for-k p2-for-k)

;;; The basic strategy for the most general case is to compile everything
;;; into open frames (up to two), and then to call routines to store
;;; them into the return registers and set the h.w. m.v. flag.  At the same
;;; time we store the 0th value into its home as the result of the call.
;;;
;;; Two special cases are optimized:  If all the values are trivial (i.e.
;;; ADRREFP-FOR-K), we just move the values directly to their final homes, and
;;; if all the destinations are apparent and unaffected by things like
;;; unbinding special variables or unwinding open registers, we can
;;; move things directly to their homes inline, rather than via the
;;; return registers.

(defun p2values-for-k (argl dest)
  ;; Handle returning from the top level of a function.
  (let ((nargs (length argl))
        (nodropthru nil))
    (when (> nargs 30.)                 ;This wants to be return-values-limit or whatever, but
                                        ;this is a cross-compiler; we don't want the LAMBDA's limit.
      (barf NARGS "Too many return values"))
    (when (cl:every #'adrrefp-for-k argl)
      (let ((fast (fast-multiple-value-dest dest)))
        (when fast
          ;; All are ADDREFP; we can send the values directly.
          (return-from p2values-for-k
            (trivial-p2values argl dest fast)))))
    (labels ((p2-multiple (&optional (limit nargs))
              (let ((frame-levels)
                    (tail-p (tail-call-p dest 'values)))
                ;; Since we're going to call P2 to compute the values, we can't just
                ;; generate this as a fixed OPEN ... CALL sequence.  We have to allow
                ;; for the possibility of branches out of the main line, so we have to
                ;; use the hairier mechanism.
                (loop for i from 0 to (1- (max limit nargs))
                      for arg in argl
                      for reg = (o-n (mod i 16.))
                      do
                      (when (zerop (mod i 16.))
                        (if (zerop i)
                            (tail-call-open 'compute-finish-values-frame tail-p
                                            (compute-finish-values-frame limit tail-p)
                                            :multiple-values)
                          (outi-open-for-k `(k:open) nil (compute-finish-values-frame-2 limit)
                                           nil `(k:open compute-finish-values-frame-2)))
                        ;; Remember which open frames were in effect so we can check
                        ;; that we got the right ones when we close up.
                        (push *open-frames* frame-levels))
                      (cond (( i nargs)
                             (outi-for-k `(k:move ,reg ,(k-find-constant-register nil) k:boxed-right)))
                            (( i limit)
                             (p2-for-k arg 'd-ignore))
                            (t (p2-for-k arg reg))))
                (when (> limit 16.)
                  ;; We have a second frame to discard.
                  (outi-close-for-k (pop frame-levels) 'k:ignore nil 'k:o0 :single-value))
                ;; Call the last frame-closer.
                (outi-close-for-k (pop frame-levels) dest nil 'k:o0
                                  (if (= nargs 1)
                                      :single-value
                                    :multiple-values-flag)))
             (when nodropthru
               (setq *dropthru* nil))))
      (cond ((d-return-p dest)
             (setq nodropthru t)
             (COND ((= NARGS 1)
                    ;; DON'T change this to (P2 ... 'D-RETURN)
                    ;; because we want to make sure to pass only one value.
                    (p2-for-k (car argl) 'k:r0)
                    (outi-for-k `(k:move k:return k:r0 k:boxed-right) :single-value)
                    NIL)
                   (T (p2-multiple))))
            ((typep dest 'multiple-values)
             (p2-multiple (length (multiple-values-values dest))))
            (t (p2-multiple))))))

;;; Convert an mv-init list and vars to a destination.

(defun convert-mvlist-to-dest (vlist vars &optional new-p)
  (loop for vspec in vlist
        for (name init) = vspec
        for var = (find name vars :key #'var-name)
        for nvar = (if new-p
                       (make-new-var :var var)
                     var)
        collect var into dests
        count (and new-p
                   (memq (first (var-lap-address var))
                         '(special remote)))
          into unbinds
        finally
        (let ((open-frame (unless (zerop unbinds)
                            (make-unbind-open-frame unbinds))))
          (return (values (make-multiple-values
                            :values dests
                            :open-frame (when new-p open-frame))
                          open-frame)))))

(defun (:property multiple-value-bind p2-for-k) (mvb-p2form dest)
  ;; The first "argument" is the multiple-value producing form.
  ;; Remove that and what you have is the same as for a LET in
  ;; pass 2.
  (destructuring-bind (mvform &rest let-p2form) mvb-p2form
    (destructuring-bind (vlist *vars* nvars) let-p2form
      (multiple-value-bind (new-dest open-frame)
          (convert-mvlist-to-dest vlist nvars t)
        (let ((*vars* nvars))
          (opening-frames (dest (progn (p2-for-k mvform new-dest)
                                       open-frame))
            (p2let-internal-for-k *vars* let-p2form dest)))))))

(defun (:property nth-value P2-for-k) (p2form dest)
  (destructuring-bind (value-number form) p2form
    (typecase value-number
      ((integer 0 0)
       (p2 `(values ,form) dest))
      ((integer 0 30.)
       (let ((new-dest (make-multiple-values
                         :values (nconc (make-list value-number)
                                        (list dest)))))
         (p2-for-k form new-dest)))
      (otherwise
       ;; This wouldn't be hard to make work.  (P1 would have to be
       ;; changed).  A small run-time subroutine with a dispatch, and
       ;; an open-frame to receive the variables specially.
       (barf p2form "~S p2 lost" 'nth-value)))))

(defun (:property multiple-value p2-for-k) (p2form dest)
  (destructuring-bind (variables form) p2form
    (labels ((variable-dest (frob)
              (typecase frob
                (list (second frob))
                (otherwise frob))))
      (destructuring-bind (&optional (retval dest) &rest other-vals)
                          (cl:map 'list #'variable-dest variables)
          (multiple-value-bind (new-retval new-retsource)
              (compute-temporary-destination retval)
            (let ((new-dest (make-multiple-values
                              :values (list* new-retval other-vals))))
              (p2-for-k form new-dest)
              (outi-for-k `(k:move ,dest ,new-retsource k:boxed-right))))))))


;;; LI:MV-PROG1-INTERNAL is just like LI:PROG1-INTERNAL, except it
;;; does a K:TAIL-RETURN so it leaves the m.v. flag alone.

(defun (:property multiple-value-prog1 p2-for-k) (argl dest)
  (p2prog12mv-for-k 1 dest argl t))

(defun discard-mvprog1-frame (open-frame operation dest &optional source source-type)
  (ignore open-frame operation)
  (ecase operation
    ((nil)
     (cond ((or (and (eq dest source)
                     (not (find dest #(k:o0 k:o1 k:o2 k:o3 k:o4 k:o5 k:o6 k:o7
                                       k:o8 k:o9 k:o10 k:o11 k:o12 k:o13 k:o14 k:o15))))
                (eq dest 'd-return))
            (outi-for-k `(k:call (ignore 0) k:ignore))
            (values dest source))
           ((eq source 'k:o0)
            (outi-for-k `(k:call (li::mv-prog1-internal 1) ,dest) source-type)
            (values dest dest))
           (t (outi-for-k `(k:move k:o0 ,source k:boxed-right))
              (outi-for-k `(k:call (li::mv-prog1-internal 1) ,dest)
                          source-type)
              (values dest dest))))
    ((:exist)
     (cond ((open-frame-there-p open-frame)
            (fsignal "Frame already there"))
           (t (setf (open-frame-there-p open-frame) t))))
    ((:return :discard)
     (outi-for-k `(k:call (ignore 0) k:ignore)))))


(defun (:property multiple-value-list p2-for-k) (p2form dest)
  (destructuring-bind (mvform) p2form
    (let ((tail-p (tail-call-p dest 'multiple-value-list)))
      (labels ((construct-mv-list (open-frame operation dest &optional source source-type)
                (unless (eq open-frame (first *open-frames*))
                  (error "We're not the top frame on the stack."))
                (pop *open-frames*)
                (ecase operation
                  ((nil)
                   (ecase source-type
                     ((:multiple-values-flag :subr-value)
                      (tail-call-open 'li:mv-list tail-p #'discard-temporary-frame
                                      :single-value)
                      (maybe-tail-call '(li:mv-list 0) dest :single-value tail-p))
                     ((:single-value :single-value-flag)
                      (tail-call-open 'list tail-p #'discard-temporary-frame
                                      :single-value)
                      (outi-for-k `(k:move k:o0 ,source k:boxed-right) :single-value)
                      (maybe-tail-call '(list 1) dest :single-value tail-p)))
                   (values dest source))
                ((:exist)
                 (setf (open-frame-there-p open-frame) t))
                ((:discard :return)))))
        (opening-frames (dest (make-open-frame :open-instruction 'mv-list
                                               :there-p t
                                               :tail-p tail-p
                                               :cleanup-generator #'construct-mv-list))
          (p2-for-k mvform dest))))))

(defun (:property *throw p2-for-k) (p2form ignore)
  ;call one of LI:THROW-INTERNAL, LI:THROW-SV, LI:THROW-MV
  (destructuring-bind (tag mvform) p2form
    ;; We can always tail-call, since we never return.
    ;; But we don't, so if there is no such throw tag, the bug
    ;; is easier to debug.  The additional cost is trivial.
    (labels ((complete-throw (open-frame operation dest &optional source source-type)
              (ignore dest)
              (ecase operation
                ((nil)
                 (outi-for-k `(k:move k:o1 ,source k:boxed-right) :single-value)
                 (ecase source-type
                   ((:subr-value)
                    (outi-for-k `(k:call (li:throw-internal 2) k:ignore)))
                   ((:multiple-values :multiple-values-flag :last-value)
                    (outi-for-k `(k:call (li:throw-mv 2) k:ignore)))
                   ((:single-value :single-value-flag)
                    (outi-for-k `(k:call (li:throw-sv 2) k:ignore))))
                 (setq *dropthru* nil)
                 (values dest source))
                ((:exist)
                 (setf (open-frame-there-p open-frame) t))
                ((:discard :return)
                 (outi-for-k `(k:call (ignore 0) k:ignore))))))
      (let ((new-dest (outi-open-for-k `(k:open) nil #'complete-throw nil '(k:open *throw))))
        (p2-for-k tag 'k:o0)
        (p2-for-k mvform new-dest)))))

;;; CATCH becomes *CATCH in P1.
;;; We compile CATCH by opening a frame, and initializing it to hold the following:

;;; O0 -- 'LI:UNWIND-MARKER
;;; O1 -- catch-tag
;;; O2 -- GR:*SPECIAL-PDL-PTR*
;;; O3 -- GR:*STACK-POINTER*
;;; O4 -- Continuation PC
;;; O5 -- Value0 to return

;;; We call LI:CATCH-CONTINUE, LI:CATCH-CONTINUE-SV, or LI:CATCH-CONTINUE-MV
;;; to break up the frame and return the appropriate value, with the appropriate
;;; value for the hardware m.v. flag.

;;; There may be more than one non-THROW continuation, depending on how the values
;;; get into place.  There will be at least one LI:CATCH-CONTINUE, which will be
;;; the one which lives in the frame for THROW to use.


(defun (:property *catch p2-for-k) (argl dest)
  (destructuring-bind (tag &rest forms) argl
    (let ((tail-p  (tail-call-p dest 'catch)))
      ;; Catches are more common (in inner loops, etc.) than throws, and they never signal
      ;; errors unless things are completely messed up, so we do concern ourselves with
      ;; tail-calling.  You can turn off these if you want.
      (let ((catch-continue (gensymbol "CATCH-CONTINUE"))
            (catch-end (gensymbol "CATCH-END")))
        (labels ((finish-catch (open-frame operation dest &optional source source-type)
                  (ecase operation
                    ((nil :return :discard)
                     ;; Move the value into K:O5 if it's not already there.
                     (when source
                       (outi-for-k `(k:move k:o5 ,source k:boxed-right) :single-value))
                     ;; Break up the frame.
                     (let ((routine
                             (ecase source-type
                               ((:single-value :single-value-flag nil)
                                'li:catch-continue-sv)
                               ((:subr-value)
                                (when (null operation)
                                  (when catch-continue
                                    ;; This continuation will do as a stand-in for all,
                                    ;; if a throw happens.  No need to output another one
                                    ;; later.
                                    (outtag-for-k catch-continue t)
                                    (setq catch-continue nil)))
                                'li:catch-continue)
                               ((:multiple-values :multiple-values-flag :last-value)
                                'li:catch-continue-mv))))
                       (maybe-tail-call `(,routine 6) dest source-type tail-p)
                       (when (null operation)
                         (outb-for-k `(branch always nil nil ,catch-end))))
                     (values dest source))
                    (:exist (setf (open-frame-there-p open-frame) t)))))
          ;; Build the new catch frame.
          (let ((new-frame
                  (tail-call-open '*catch tail-p #'finish-catch :subr-value)))
            (setf (open-frame-idest new-frame) dest)
            (outi-for-k `(k:movei k:o0 'li:unwind-marker k:boxed))
            (p2-for-k tag 'k:o1)
            (outi-for-k `(k:move k:o2 gr:*special-pdl-ptr* k:boxed-right))
            (outi-for-k `(k:move k:o3 gr:*stack-pointer* k:boxed-right))
            (outi-for-k `(k:move-pc k:o4 ,catch-continue))
            (loop for fp on forms
                  for form = (first fp)
                  for cdest = (if (rest fp) 'd-ignore new-frame)
                  do (p2-for-k form cdest))

            ;; Output a continuation so it can go in the frame.
            (if catch-continue
                (outi-close-for-k *open-frames* dest nil 'k:o5 :subr-value)
              ;; No continuation needed.
              (pop *open-frames*))
            (outtag-for-k catch-end t)))))))



;;; We compile UNWIND-PROTECT by opening a frame, and initializing it to hold
;;; the following:

;;; O0 -- 'LI:UNWIND-MARKER
;;; O1 -- 'LI:UNWIND-PROTECT-TAG
;;; O2 -- GR:*SPECIAL-PDL-PTR*
;;; O3 -- GR:*STACK-POINTER*
;;; O4 -- Continuation PC
;;; O5 -- Value0 to return

;;; We call LI:UNWIND-PROTECT-CONTINUE to break up the frame and return
;;; the appropriate value, with the appropriate value for the hardware m.v. flag.

;;; Except that we don't do any of this right now, because THROW is all messed up.

(defun (:property unwind-protect p2-for-k) (argl dest)
  (p2prog12mv-for-k 1 dest argl t)
  #+ignore
  (destructuring-bind (tag &rest forms) argl
    (let ((tail-p  (tail-call-p dest 'catch)))
      (let ((catch-continue (gensymbol "UNWIND-PROTECT-CONTINUE"))
            (catch-end (gensymbol "UNWIND-PROTECT-END")))
        (labels ((finish-catch (open-frame operation dest &optional source source-type)
                  (ecase operation
                    ((nil :return :discard)
                     ;; Move the value into K:O5 if it's not already there.
                     (when source
                       (outi-for-k `(k:move k:o5 ,source k:boxed-right) :single-value))
                     ;; Break up the frame.
                     (let ((routine
                             (ecase source-type
                               ((:single-value :single-value-flag nil)
                                'li:catch-continue-sv)
                               ((:subr-value)
                                (when catch-continue
                                  ;; This continuation will do as a stand-in for all,
                                  ;; if a throw happens.  No need to output another one
                                  ;; later.
                                  (outtag-for-k catch-continue t)
                                  (setq catch-continue nil))
                                'li:catch-continue)
                               ((:multiple-values :multiple-values-flag :last-value)
                                'li:catch-continue-mv))))
                       (maybe-tail-call `(,routine 6) dest source-type tail-p)
                       (outb-for-k `(branch always nil nil ,catch-end)))
                     (values dest source))
                    (:exist (setf (open-frame-there-p open-frame) t)))))
          ;; Build the new catch frame.
          (let ((new-frame
                  (tail-call-open 'unwind-protect tail-p #'finish-catch :subr-value)))
            (setf (open-frame-idest new-frame) dest)
            (outi-for-k `(k:movei k:o0 'li:unwind-marker k:boxed))
            (p2-for-k tag 'k:o1)
            (outi-for-k `(k:move k:o2 gr:*special-pdl-ptr* k:boxed-right))
            (outi-for-k `(k:move k:o3 gr:*stack-pointer* k:boxed-right))
            (outi-for-k `(k:move-pc k:o4 ,catch-continue))
            (loop for fp on forms
                  for form = (first fp)
                  for cdest = (if (rest fp) 'd-ignore new-frame)
                  do (p2-for-k form cdest))
            ;; Output a continuation so it can go in the frame.
            (if catch-continue
                (outi-close-for-k *open-frames* dest nil 'k:o5 :subr-value)
              ;; No continuation needed.
              (pop *open-frames*))
            (outtag-for-k catch-end t)))))))


(defun (:property lexical-closure p2-for-k) (argl dest)
  (let ((tail-p (tail-call-p dest 'lexical-closure)))
    (tail-call-open 'li:make-closure-with-env tail-p #'discard-temporary-frame
                    :single-value)
    (p2-for-k (first argl) 'k:o0)
    (outi-for-k `(k:move k:o1 k:a15 k:boxed-right))
    (maybe-tail-call '(li:make-closure-with-env 2) dest :single-value tail-p)))

(defun test ()
  (setq gr:*stack-pointer* (hw:dpb gr:*stack-pointer* vinc:%%data-type vinc:$$dtp-locative)))


;Post Processed: entries ((0 . TAG::TEST_1))
;      TEST_1
;   0:  (MOVE R1 (REGISTER *TEN* 4 17) BOXED-RIGHT)
;   1:  (ALU-FIELD FIELD-PASS (REGISTER *STACK-POINTER* 6 0) (REGISTER *STACK-POINTER* 6 0) R1
;                                                                                        (QUOTE 3032)
;                                                                                        PW-II
;                                                                                        DT-NONE
;                                                                                        BOXED-RIGHT)
;   2:  (MOVE RETURN (REGISTER *STACK-POINTER* 6 0) BOXED-RIGHT CH-RETURN NEXT-PC-RETURN)

;; Note: variable needs to be moved to addressable space.
(defun (:property variable-location p2-for-k) (argl dest)
  (let ((var-ref (first argl)))
    (case (first var-ref)
      (local-ref
       (let ((lap-address (var-lap-address (second var-ref))))
         (ecase (first lap-address)
           (arg (error "Cannot have a locative to a local variable on the K."))
           (local
            (stack-slot-address 'k:r0 (second lap-address) 'k:r0)
            (outi-for-k `(k:movei k:r1 ',vinc:$$dtp-locative))
            (outi-for-k `(k:alu-field k:field-pass ,dest k:r0 k:r1 ',vinc:%%data-type k:pw-ii k:dt-none k:boxed)
                        :single-value)))))
      (self-ref
       (let ((tail-p (tail-call-p dest 'li:instance-location)))
         (tail-call-open '(li:instance-location 3) tail-p #'discard-temporary-frame :single-value)
         (outi-for-k `(k:movei k:o0 ',(second var-ref) k:boxed))
         (outi-for-k '(k:move k:o1 k:a14 k:boxed-right))
         (outi-for-k '(k:move k:o2 k:a0 k:boxed-right))
         (maybe-tail-call '(li:instance-location 3) dest :single-value tail-p)))
      (lexical-ref
       (let ((tail-p (tail-call-p dest 'li:closure-location)))
         (tail-call-open '(li:closure-location 2) tail-p #'discard-temporary-frame :single-valiue)
         (outi-for-k `(k:movei k:o0 ',(second var-ref) k:boxed))
         (outi-for-k '(k:move k:o1 k:a15 k:boxed-right))
         (maybe-tail-call '(li:closure-location 2) dest :single-value tail-p))))))

;;; This needs to be in QCP1, but that's frozen at the moment.

(DEFUN (:PROPERTY KLUDGEY-COMPILATION-VARIABLE-LOCATION P1) (FORM &AUX TEM TEM1)
  (SETQ FORM (CADR FORM))
  (SETQ TEM (COND ((SETQ TEM1 (find FORM *VARS* :key #'var-name))
                   (AND (EQ (VAR-KIND TEM1) 'FEF-ARG-FREE)
                        (ZEROP (VAR-USE-COUNT TEM1))
                        (PUSH (VAR-NAME TEM1) *FREEVARS*))
                   (VAR-LAP-ADDRESS TEM1))
                  ((SPECIALP FORM) FORM)
                  (T (BARF FORM "Lossage in keyed-lambda compilation"))))
  (cond ((SYMBOLP TEM)
         `(%EXTERNAL-VALUE-CELL ',TEM))
        (t (when (eq *target-computer* 'k)
                    (case (first tem)
                      (local-ref
                       (push 'variable-location (var-misc (second tem))))))
           `(VARIABLE-LOCATION ,TEM))))

;;; These need to be in QCP1, but that's frozen at the moment

;;; Given a lambda which uses &KEY, return an equivalent one
;;; which does not use &KEY.  It takes a &REST arg instead
;;; (though if the original one had a rest arg, it uses that one).
;;; If there is no ARGLIST declaration for this function, we make one
;;; so that the user is still told that the function wants keyword args.
(DEFUN EXPAND-KEYED-LAMBDA (LAMBDA-EXP)
  (LET (LAMBDA-LIST BODY
        MAYBE-REST-ARG KEYCHECKS
        PSEUDO-KEYNAMES)
    (IF (EQ (CAR LAMBDA-EXP) 'LAMBDA)
        (SETQ LAMBDA-LIST (CADR LAMBDA-EXP) BODY (CDDR LAMBDA-EXP))
        (SETQ LAMBDA-LIST (CADDR LAMBDA-EXP) BODY (CDDDR LAMBDA-EXP)))  ;named-lambda
    (MULTIPLE-VALUE-BIND (POSITIONAL-ARGS NIL AUXVARS
                          REST-ARG nil
                          KEYKEYS KEYNAMES KEYINITS KEYFLAGS ALLOW-OTHER-KEYS)
        (DECODE-KEYWORD-ARGLIST LAMBDA-LIST)
      (SETQ PSEUDO-KEYNAMES (COPY-LIST KEYNAMES))
      (MULTIPLE-VALUE-BIND (NIL DECLS)
          (WITH-LIST (ENV *FUNCTION-ENVIRONMENT*)
            (EXTRACT-DECLARATIONS BODY NIL NIL ENV))
        ;; For each keyword arg, decide whether we need to init it to KEYWORD-GARBAGE
        ;; and check explicitly whether that has been overridden.
        ;; If the arg is optional
        ;; and the initial value is a constant, we can really init it to that.
        ;; Otherwise we create a dummy variable initialized to KEYWORD-GARBAGE;
        ;; after all keywords are decoded, we bind the intended variable, in sequence.
        ;; However a var that can shadow something (including any special var)
        ;; must always be replaced with a dummy.
        (DO ((KIS KEYINITS (CDR KIS))
             (KNS KEYNAMES (CDR KNS))
             (PKNS PSEUDO-KEYNAMES (CDR PKNS))
             (KFS KEYFLAGS (CDR KFS)))
            ((NULL KNS))
          (LET ((KEYNAME (CAR KNS)) PSEUDO-KEYNAME
                (KEYFLAG (CAR KFS)) (KEYINIT (CAR KIS)))
            (OR (AND (NULL KEYFLAG)
                     (CONSTANTP KEYINIT)
                     (NOT (find KEYNAME *VARS* :key #'var-name))
                     (NOT (LEXICAL-VAR-P KEYNAME))
                     (NOT (SPECIALP KEYNAME)))
                (PROGN (SETF (CAR KIS) 'SI::KEYWORD-GARBAGE)
                       (SETQ PSEUDO-KEYNAME (gensymbol keyname))
                       (SETF (CAR PKNS) PSEUDO-KEYNAME)
                       (PUSH `(,KEYNAME
                               (COND ((EQ ,PSEUDO-KEYNAME SI::KEYWORD-GARBAGE)
                                      ,KEYINIT)
                                     (T ,(AND KEYFLAG `(SETQ ,KEYFLAG T))
                                        ,PSEUDO-KEYNAME)))
                             KEYCHECKS)))))
        (SETQ KEYFLAGS (REMQ NIL KEYFLAGS))
        (SETQ KEYCHECKS (NREVERSE KEYCHECKS))

        ;; If the user didn't ask for a rest arg, make one for the
        ;; outer function anyway.
        (OR REST-ARG (SETQ REST-ARG (gensymbol "REST")
                           MAYBE-REST-ARG (LIST '&REST REST-ARG)))
        `(LAMBDA (,@POSITIONAL-ARGS ,@MAYBE-REST-ARG)
           (DECLARE . ,DECLS)
           (LET* (,@(MAPCAR (LAMBDA (V INIT) `(,V ,INIT)) PSEUDO-KEYNAMES KEYINITS)
                  ,@KEYFLAGS)
             (DECLARE . ,DECLS)
;            (COND ((EQ (CAR ,REST-ARG) 'PERMUTATION-TABLE)
;                   (OR (%PERMUTE-ARGS)
;                       (PROGN (RECOMPUTE-KEYWORD-PERMUTATION-TABLE
;                                (CDR ,REST-ARG)
;                                (%P-CONTENTS-OFFSET (%STACK-FRAME-POINTER) %LP-FEF)
;                                ',KEYKEYS)
;                              (%PERMUTE-ARGS)))
;                   ;; If the function really wants the rest arg,
;                   ;; flush the permutation table and its keyword.
;                   ,(AND (NOT MAYBE-REST-ARG) `(SETQ ,REST-ARG (CDDR ,REST-ARG))))
;                  (T
             ,(case *target-computer*
                (k (generate-k-keyword-args-decode pseudo-keynames rest-arg keykeys allow-other-keys))
                (otherwise
                 `(WHEN ,REST-ARG
                    (SI::STORE-KEYWORD-ARG-VALUES ;; kludgey-compilation-variable-location is just like
                      ;; variable-location except that it doesn't increment
                      ;; the var-use-count of its arg
                      (KLUDGEY-COMPILATION-VARIABLE-LOCATION
                        ,(CAR PSEUDO-KEYNAMES))
                      ,REST-ARG ',KEYKEYS
                      ,ALLOW-OTHER-KEYS))))
             (LET* ,KEYCHECKS
               (DECLARE . ,DECLS)
               ((LAMBDA ,AUXVARS . ,BODY)))))))))

(defun generate-k-keyword-args-decode (pseudo-keynames rest-arg keykeys allow-other-keys)
  (labels ((mv-step (names keys)
                    (if (< (length names) 32.)  ;LISP:MULTIPLE-VALUES-LIMIT on the K
                        (values names keys)
                      (loop repeat 31.
                            for n on names
                            for k on keys
                            collect (first n) into nn
                            collect (first k) into kk
                            finally
                            (return (values nn kk n k))))))
    `(when ,rest-arg
       ,@(loop with (names keys)
               with more-names = pseudo-keynames
               with more-keys = keykeys
               do (multiple-value-setq (names keys more-names more-keys)
                    (mv-step more-names more-keys))
               while names
               collect
               `(multiple-value-setq ,names
                  (li:get-keyword-arg-values ,rest-arg ',keys ,allow-other-keys))))))


;;; Turn a call to an internal lambda into a LET, and return P1 of that LET.
;;; All &AUX variables in the lambda list are extracted by P1AUX.
;;; We generate a LET, since the lambda variables should all be computed and then bound.
;;; This means that &OPTIONALs don't work quite right;
;;; but they never used to work at all in internal lambdas anyway.
;;; No checking of number of args here, because it is done elsewhere.
;;; We just eval and ignore extra args and take missing ones to be NIL.
(DEFUN P1LAMBDA (LAMBDA ARGS)
  (LET (ARGLIST BODY ARGS1 OPTIONAL PROGVARS VAR QUOTEFLAG
         SPECIAL-FLAG SPECIAL-VARS UNSPECIAL-FLAG UNSPECIAL-VARS
         KEYCHECKS BORDER-VARIABLE PSEUDO-KEYNAMES)
    (SETQ LAMBDA (SI::LAMBDA-EXP-ARGS-AND-BODY (P1AUX LAMBDA)))
    (SETQ ARGLIST (CAR LAMBDA) BODY (CDR LAMBDA))
    (MULTIPLE-VALUE-BIND (NIL NIL NIL
                          REST-ARG NIL KEYKEYS KEYNAMES KEYINITS KEYFLAGS ALLOW-OTHER-KEYS)
        (DECODE-KEYWORD-ARGLIST ARGLIST)
      (WHEN (AND KEYNAMES (NOT REST-ARG))
        (SETQ REST-ARG (gensymbol "REST")))
      (SETQ ARGS1 ARGS)
      (DO ((ARGLIST1 ARGLIST (CDR ARGLIST1)))
          (NIL)
        (SETQ VAR (CAR ARGLIST1))
        (COND ((NULL ARGLIST1)
               (RETURN T))
              ((EQ VAR '&KEY)
               (PUSH (LIST REST-ARG `(LIST . ,ARGS1)) PROGVARS)
               (RETURN (SETQ ARGS1 NIL)))
              ((EQ VAR '&REST)
               (POP ARGLIST1)
               (PUSH (LIST (CAR ARGLIST1) `(LIST . ,ARGS1)) PROGVARS)
               (RETURN (SETQ ARGS1 NIL)))
              ((EQ VAR '&OPTIONAL)
               (SETQ OPTIONAL T))
              ;; soon to be obsolete
              ((EQ VAR '&QUOTE)
               (SETQ QUOTEFLAG T))
              ;; soon to be obsolete
              ((EQ VAR '&EVAL)
               (SETQ QUOTEFLAG NIL))
              ((EQ VAR '&SPECIAL)
               (warn 'obsolete-lambda-list-keyword :obsolete
                     "~~S in lambda-lists is obsolete and will not be supported in the future.~%  ~
                    Use the ~S declaration.~" '&special 'special)
               (SETQ SPECIAL-FLAG T UNSPECIAL-FLAG NIL))
              ((EQ VAR '&LOCAL)
               (warn 'obsolete-lambda-list-keyword :obsolete
                     "~~S in lambda-lists is obsolete and will not be supported in the future.~%  ~
                    Use the ~S declaration.~" '&special 'unspecial)
               (SETQ SPECIAL-FLAG NIL UNSPECIAL-FLAG T))
              ;; soon also to be obsolete
              ((EQ VAR '&FUNCTIONAL))
              ((MEMQ VAR LAMBDA-LIST-KEYWORDS)
               (WARN 'BAD-INTERNAL-LAMBDA-KEYWORD :IMPOSSIBLE
                     "~S is not supported in internal lambdas." VAR))
              (T (AND SPECIAL-FLAG (PUSH VAR SPECIAL-VARS))
                 (AND UNSPECIAL-FLAG (PUSH VAR UNSPECIAL-VARS))
                 (COND ((SYMBOLP VAR)
                        (PUSH (LIST VAR (IF QUOTEFLAG `',(CAR ARGS1)
                                          (CAR ARGS1)))
                              PROGVARS))
                       (T
                        (UNLESS (NOT OPTIONAL)
                          (WARN 'BAD-ARGUMENT-LIST :IMPOSSIBLE
                                "The mandatory argument ~S of an internal lambda ~
  was given a default value."
                                (CAR VAR)))
                        (PUSH (LIST (CAR VAR)
                                    (IF ARGS1 (IF QUOTEFLAG `',(CAR ARGS1) (CAR ARGS1))
                                      (CADR VAR)))
                              PROGVARS)))
                 (POP ARGS1))))
      (WHEN KEYNAMES
        (SETQ PSEUDO-KEYNAMES (COPY-LIST KEYNAMES))
        ;; For each keyword arg, decide whether we need to init it to KEYWORD-GARBAGE
        ;; and check explicitly whether that has been overridden.
        ;; If the initial value is a constant, we can really init it to that.
        ;; Otherwise we create a dummy variable initialized to KEYWORD-GARBAGE;
        ;; after all keywords are decoded, we bind the intended variable, in sequence.
        ;; However a var that can shadow something (including any special var)
        ;; must always be replaced with a dummy.
        (DO ((KIS KEYINITS (CDR KIS))
             (KNS KEYNAMES (CDR KNS))
             (PKNS PSEUDO-KEYNAMES (CDR PKNS))
             (KFS KEYFLAGS (CDR KFS)))
            ((NULL KNS))
          (LET ((KEYNAME (CAR KNS)) PSEUDO-KEYNAME
                (KEYFLAG (CAR KFS)) (KEYINIT (CAR KIS)))
            (OR (AND (NULL KEYFLAG)
                     (CONSTANTP KEYINIT)
                     (NOT (find KEYNAME *VARS* :key #'var-name))
                     (NOT (LEXICAL-VAR-P KEYNAME))
                     (NOT (SPECIALP KEYNAME)))
                (PROGN (SETF (CAR KIS) 'SI::KEYWORD-GARBAGE)
                       (SETQ PSEUDO-KEYNAME (gensymbol keyname))
                       (SETF (CAR PKNS) PSEUDO-KEYNAME)
                       (PUSH `(,KEYNAME
                               (COND ((EQ ,PSEUDO-KEYNAME SI::KEYWORD-GARBAGE)
                                      ,KEYINIT)
                                     (T ,(AND KEYFLAG `(SETQ ,KEYFLAG T))
                                        ,PSEUDO-KEYNAME)))
                             KEYCHECKS)))))
        (SETQ KEYFLAGS (REMQ NIL KEYFLAGS))
        (SETQ KEYCHECKS (NREVERSE KEYCHECKS))

        ;; BORDER-VARIABLE is a local we put in the binding list
        ;; as the easiest way of being able to get a locative to the
        ;; slot before the first of our keyword arg locals.
        (SETQ BORDER-VARIABLE (gensymbol "KEYWORDS-LOC"))
        (SETQ BODY
              `((LET* (,BORDER-VARIABLE
                       ,@(MAPCAR (LAMBDA (V INIT) `(,V ,INIT)) PSEUDO-KEYNAMES KEYINITS)
                       ,@KEYFLAGS)
                  (DECLARE (IGNORE ,BORDER-VARIABLE))
             ,(case *target-computer*
                (k (generate-k-keyword-args-decode pseudo-keynames rest-arg keykeys allow-other-keys))
                (otherwise
                 `(WHEN ,REST-ARG
                    (SI::STORE-KEYWORD-ARG-VALUES-INTERNAL-LAMBDA
                      (KLUDGEY-COMPILATION-VARIABLE-LOCATION ,BORDER-VARIABLE)
                      ,REST-ARG ',KEYKEYS
                      ,ALLOW-OTHER-KEYS))))
                  (LET* ,KEYCHECKS
                    . ,BODY)))))
      ;; Take all DECLAREs off the body and put them on DECLS.
      (MULTIPLE-VALUE-BIND (BODY DECLS)
          (with-list (env *function-environment*)
            (EXTRACT-DECLARATIONS-RECORD-MACROS BODY NIL NIL env))
        (WHEN SPECIAL-VARS
          (PUSH `(SPECIAL . ,SPECIAL-VARS) DECLS))
        (WHEN UNSPECIAL-VARS
          (PUSH `(UNSPECIAL . ,UNSPECIAL-VARS) DECLS))
        (WHEN DECLS
          (PUSH `(DECLARE . ,DECLS) BODY))
        (P1 `(LET-FOR-LAMBDA ,(NRECONC PROGVARS (IF ARGS1 `((IGNORE (PROGN . ,ARGS1)))))
                             . ,BODY))))))
