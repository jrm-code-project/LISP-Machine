;;; -*- Mode:LISP; Package:COMPILER; Base:8; Readtable:ZL -*-


;;; These don't belong here, but they don't belong anywhere else either...

;;; $$$ Cross-compiler / zwei interface rehacked <04-Nov-88 smh>

(defvar *download-p* nil)       ; $$$ New special var. <03-Nov-88 smh>

;;; Now sets up *falcon-environment* $$$ smh 28sep88
;;; $$$ Hacked for grand consistency scheme. <11-Nov-88 smh>
(zwei:DEFCOM COM-CROSSCOMPILE-REGION-for-falcon "Crosscompile the current region or defun.
If there is a region, it is compiled.
Otherwise, the current or next defun is compiled."
             ()
  (let ((nc:*debug-flag* (cons :post nc:*debug-flag*)))
    (let-if zwei:*numeric-arg-p* ((nc:*debug-stream* (open "ed-buffer:compiler-output" :direction :output)))
      (crosscompile-region-1 nil))))

;;; $$$ Hacked for grand consistency scheme. <11-Nov-88 smh>
(zwei:DEFCOM COM-CROSSCOMPILE-REGION-for-falcon-and-download "Crosscompile and download the current region or defun.
If there is a region, it is compiled.
Otherwise, the current or next defun is compiled."
             ()
  (let-if zwei:*numeric-arg-p* ((nc:*debug-flag* (cons :post nc:*debug-flag*)))
    (let-if (minusp zwei:*numeric-arg*) ((nc:*debug-stream* (open "ed-buffer:compiler-output" :direction :output)))
      (crosscompile-region-1 t))))

;;; $$$ Hacked for grand consistency scheme. <11-Nov-88 smh>
(zwei:DEFCOM com-crosscompile-region-for-falcon-disassemble "Crosscompile the current region or defun and disassemble to a buffer."
             ()
  (let ((nc:*debug-flag* (cons :post nc:*debug-flag*)))
    (let ((nc:*debug-stream* (open "ed-buffer:compiler-output" :direction :output)))
      (crosscompile-region-1 t))))

;;; $$$ New function. <04-Nov-88 smh>
;;; $$$ Further hacked towards grand consistency scheme. <07-Nov-88 smh> <11-Nov-88 smh>
(defun CROSSCOMPILE-REGION-1 (*download-p*)
  (let ((*compilation-environment* *falcon-environment*)
        (si::*target-features* si::*falcon-features*))  ; $$$ Binding added. <14-Nov-88 smh>
    (zwei:COMPILE-DEFUN-INTERNAL (or (zwei:get-buffer-compiler zwei:*interval*) T)
                                 (if *download-p* "Cross-and-downloading" "Crosscompiling")
                                 (if *download-p* "Cross-and-downloaded" "Crosscompiled.")
                                 NIL            ;USE-TYPEOUT
                                 NIL            ;DEFVAR-HACK
                                 '(:MODE MACRO-COMPILE) ;generating-micro-compiler-input-p
                                                ;will wind up getting set however.
                                 'K             ;*target-computer*
                                 *falcon-environment*)  ;$$$ smh 14oct88
    zwei:DIS-NONE))


;;; Ugly new commands: Macro Expand Cross Compilerly, Macro Expand All Cross Compilerly.
;;; $$$ smh 28sep88

(zwei:defcom com-macro-expand-cross-compilerly "Macroexpand the current region or defun
the way the cross compiler would see it."
  ()
  (let ((*compilation-environment* *falcon-environment*)
        (*target-computer* 'k))
    (zwei:com-macro-expand-expression)))

(zwei:defcom com-macro-expand-all-cross-compilerly "Macroexpand the current region or defun
the way the cross compiler would see it to all levels."
  ()
  (let ((*compilation-environment* *falcon-environment*)
        (*target-computer* 'k))
    (zwei:com-macro-expand-expression-all)))

;;; $$$ Rationalized to parallel the Fleabit shift-key panoply <11-Nov-88 smh>
(zwei::command-store 'com-crosscompile-region-for-falcon              #\super-c       zwei::*zmacs-comtab*)
(zwei::command-store 'com-crosscompile-region-for-falcon-and-download #\super-shift-C zwei::*zmacs-comtab*)
(zwei::command-store 'com-crosscompile-region-for-falcon-disassemble  #\super-meta-c  zwei::*zmacs-comtab*)
(zwei::command-store 'com-macro-expand-cross-compilerly               #\super-m       zwei::*zmacs-comtab*)
(zwei::command-store 'com-macro-expand-all-cross-compilerly           #\super-shift-M zwei::*zmacs-comtab*)


;k target computer interface
(defprop k fdefine-for-k fdefine)
(defprop k peep-for-k peep)
(defprop k qlapp-for-k qlapp)
(defprop k p2sbind-for-toplevel-for-k p2sbind-for-toplevel)
(defprop k assign-lap-addresses-for-k assign-lap-addresses)
(defprop k var-compute-init-for-k var-compute-init)
(defprop k p2-for-k p2)
(defprop k defafun-for-k defafun)

(defun fdefine-for-k (function-spec definition &optional carefully-flag no-query-flag)
  (declare (ignore carefully-flag no-query-flag))
  (when nc:*debug-flag*
    (format t "~%Fdefine ~s ~s" function-spec definition))
  nil)

(defun peep-for-k (peep-code-array &optional function-name)
  (declare (ignore function-name))
  ;(grind-top-level (g-l-p peep-code-array))
  ;  (format t "~%PEEP ~s ~s" (g-l-p peep-code-array) function-name)
  nil)

(defun qlapp-for-k (fctn lap-mode)
  ;;(format t "~%QLAPP ~s ~s" fctn lap-mode)
  (multiple-value-bind (name instructions entry-points function-type)
      (cross-compile fctn 'store)                               ;or 'print
    (let-if (not (variable-boundp nc:*debug-stream*))
            ((nc:*debug-stream* standard-output))
      (when (intersection '(:pre :post :postx) nc:*debug-flag*)
        (format nc:*debug-stream* "~%Entries ~s. ~%" entry-points))
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
                 (fasd-k-compiled-function nc-function-defstruct function-type))
                ((eq lap-mode 'compile-to-core)
                 (when *download-p*
                   (zwei:typein-line " ... downloading ~D instructions ... "    ; $$$ added <03-Nov-88 smh>
                                     (length (nc:ncompiled-function-code nc-function-defstruct)))
                   (nlisp-download-fasd-function-defstruct nc-function-defstruct))      ; $$$ added <03-Nov-88 smh>
                 #+never
                 (compiler-fasd-switch (fasd-function-defstruct nc-function-defstruct))))))
      )))

;;; $$$ new function <22-Nov-88 smh>
(defun defafun-for-k (fctn output-mode)
  (setq fctn (cdr fctn))
  (let* ((name (car-safe fctn))
         (args (cadr-safe fctn))
         (instructions (cddr-safe fctn))
         (entry-points `((,(length args) . ,name))))
    (unless (and name (symbolp name)
                 (cli:listp args)
                 (cli:listp instructions))
      (ferror nil "Malformed DEFAFUN: ~s" fctn))
    (let-if (not (variable-boundp nc:*debug-stream*))
            ((nc:*debug-stream* standard-output))
      (when (intersection '(:pre :post :postx) nc:*debug-flag*)
        (format nc:*debug-stream* "~%Entries ~s. ~%" entry-points)
        (format nc:*debug-stream* "~%Assembly instructions:~%")
        (nc:print-instructions instructions))
      (let ((nc-function-defstruct (nc:assemble-instruction-list name (cons name instructions) entry-points)))
        (nc:debug :postx
          (format nc:*debug-stream* "~%Postprocessed instructions:~%")
          (nc:print-instructions instructions nc:*debug-stream* (nc:ncompiled-function-code nc-function-defstruct)))
        (cond ((eq output-mode 'qfasl)
               (fasd-k-compiled-function nc-function-defstruct 'function))
              ((eq output-mode 'compile-to-core)
               (when *download-p*
                 (zwei:typein-line " ... downloading ~D instructions ... "
                                   (length (nc:ncompiled-function-code nc-function-defstruct)))
                 (nlisp-download-fasd-function-defstruct nc-function-defstruct))))))))

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
      (MEMQ (CAR EXP) '(LOCAL-REF QUOTE FUNCTION BREAKOFF-FUNCTION self-ref lexical-ref))))

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
          ((null form)
           (outi-for-k `(k:move ,dest gr:*nil* k:boxed-right) :single-value))
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
          ;; INSTANCE-VARIABLE ref
          ((eq (first form) 'self-ref)
           (iv-ref-for-k form 'li:instance-var-read dest))
          ((memq (car form) '(function quote breakoff-function))
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
  (LET (COUNT TOKEN-LIST AG1 DSC1 TM RESTART-PC
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
               (nc:debug :frames
                 (format nc:*debug-stream* "~%P2ARGC-int OP=~a DEST=~a SOURCE=~a SOURCE-TYPE=~a FRAME=~a" operation dest source source-type open-frame)
                 (format nc:*debug-stream* "~%OPEN-FRAMES: ~{~18t~a~^~%~}" *open-frames*)
                 #+ignore
                 (format nc:*debug-stream* "~% { ~{~a~^ <- ~} }" (cdr (limited-backtrace 50 :compiler))))
               (ecase operation
                 ((nil)                         ;the NIL operation outputs the normal call.
                  (when (null (open-frame-there-p open-frame))
                    (fsignal "tried to output frame before it exists"))
                  (cond (tail-call-switch
                         (outi-for-k `(k:tail-call (,target ,argn)) source-type)
                         (nc:debug :frames
                           (format nc:*debug-stream* "~%MV-P2ARGC-int returning [~a ~a]" dest source))
                         (values dest source))
                        (t (outi-for-k `(k:call (,target ,argn) ,dest) source-type)
                           (nc:debug :frames
                             (format nc:*debug-stream* "~%MV-P2ARGC-int returning [~a ~a]" dest dest))
                           (values dest dest))))                ;+++ Should be DEST SOURCE ?? -smh
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
                                (pop-frame))))
                        (t (pop-frame)))
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
                      ((NULL AG1) (GO RET))                     ;OUT OF ARG LIST
                      ((MEMQ 'FEF-QT-QT TOKEN-LIST)
                       (if (not (zerop argn))
                           (outi-for-k `(k:move ,k-dest (quote ,(car ag1)) k:boxed-right))
                         (outi-for-k `(k:move ,k-dest (quote-vector ',(car ag1)) k:boxed-right))))
                      ((MEMQL '(FEF-QT-EVAL FEF-QT-DONTCARE) TOKEN-LIST)
                       (with-frames                             ; added 8sep88 smh
                         (COND ((AND (NULL (CDR AG1))
                                     (MEMQ 'LEXPR-FUNCALL TOKEN-LIST))
                                (fsignal "%spread")
                                (P2-for-k (CAR AG1)
                                          (PROGN (FSIGNAL "D-PDL") 'K:R0))      ;Arg to %SPREAD
                                (OUTI-for-k (LIST 'MISC k-dest '%SPREAD)))
                               (T (P2-for-k (CAR AG1) k-dest)))))
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
              (outi-close-for-k our-open-frame
                                dest
                                (if (open-frame-there-p (car our-open-frame)) nil :discard)
                                nil :subr-value)
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
