;;;   -*- Mode:LISP; Package:COMPILER; Base:8; Readtable:ZL -*-

;this file has modifications to P2 of the compiler for cross compiling on K.

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
;  D-INDS is considered to be equivalent to K:R0.


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
  ;(format t "~%QLAPP ~s ~s" fctn lap-mode)
  (multiple-value-bind (name instructions entry-points)
      (cross-compile fctn 'store)               ;or 'print
    (let ((nc:*debug-stream* standard-output))
      (format nc:*debug-stream* "~%Entries ~s. ~%Preprocessed instructions:~%" entry-points)
      (setq instructions (reverse instructions))
      (nc:print-instructions instructions)
      ;(setq instructions (delete-branch-plus-one instructions))          ;these get generated quite a bit by optional args.
      ;(grind-top-level instructions)
      (let ((code (nc:post-process (reverse instructions))))
        (nc:debug :post
          (format nc:*debug-stream* "~%Postprocessed instructions:~%")
          (nc:print-instructions code nc:*debug-stream*)
          ;(grind-top-level code)
          )
      (let ((nc-function-defstruct (nc:assemble-instruction-list name code entry-points)))
        (compiler-fasd-switch (fasd-function-defstruct nc-function-defstruct)))
        )))
  nil)

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
       ;(string-equal (package-name (symbol-package dest)) "K")
       (nc:register-number dest)))

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
      (cdr probe))))

(defun k-find-return-register (index)
  (when ( index 0)
    (error "Return register ~D isn't used." index))
  (when (> index 29)
    (error "Cannot return ~D values." (1+ index)))
  (let* ((name (svref #(gr:*return-0*  gr:*return-1*  gr:*return-2*  gr:*return-3*  gr:*return-4*
                        gr:*return-5*  gr:*return-6*  gr:*return-7*  gr:*return-8*  gr:*return-9*
                        gr:*return-10* gr:*return-11* gr:*return-12* gr:*return-13* gr:*return-14*
                        gr:*return-15* gr:*return-16* gr:*return-17* gr:*return-18* gr:*return-19*
                        gr:*return-20* gr:*return-21* gr:*return-22* gr:*return-23* gr:*return-24*
                        gr:*return-25* gr:*return-26* gr:*return-27* gr:*return-28* gr:*return-29*)
                      index))
         (var (get name 'nc::global-register)))
    (nc::variable-loc var)))

(defun k-find-return-register-count ()
  (let ((var (get 'gr:*number-of-return-values* 'nc::global-register)))
    (nc::variable-loc var)))

;;; List of open frames.  The entries are structures of type OPEN-FRAME (see below).

(defvar-resettable *open-frames* ())

;; Debugging switch.  Don't enable it until *open-frames* gets scoped right.
(defvar *reg-error-enable* ())
(defun reg-error (&rest args)
  (when *reg-error-enable*
    (apply #'error args)))

(defun note-open-frame (tail-p &optional (cleanup-function '(li::discard-open-frame 0)))
  (push `(,(if tail-p 'k:tail-call 'k:call)
          ,cleanup-function)
        *open-frames*))

(defun finish-open-frame (tail-p)
  (when (null *open-frames*)
    (error "Internal compiler error:  Over-pop of open frames."))
  (let ((old (pop *open-frames*)))
    (unless (memq (car old)
                  (if tail-p '(k:tail-call k:tcall-dispatch) '(k:call k:call-dispatch)))
      (error "Internal compiler error:  Mismatch of tail-callness for OPEN and CALL."))))

(defvar *gensymbol-counter* 0)

(defun gensymbol (string)
  (make-symbol (string-append string "-"
                              (write-to-string (incf *gensymbol-counter*)
                                               :radix nil :base 10.))))

(defstruct (open-frame :conc-name)
  (open-instruction)                    ;For debugging.
  (tail-p)                              ;For error checking.
  (cleanup-generator)                   ;Function of three arguments.
                                        ;The first argument is the OPEN-FRAME object
                                        ;The second argument is one of:
                                        ;NIL -- Normal completion of the frame.
                                        ;:DISCARD -- Discard the frame, no value.
                                        ;:RETURN -- Discard the frame, return a value.
                                        ;The third argument is the destination, or where
                                        ;the return value may be found (in the case of :RETURN).
  )

;;; Use this macro when we do something which creates an open frame.
;;; The cleanup-body is queued up to be run when we're finished with
;;; the open frame.  It may be run many times, in the presence of
;;; conditional branching or returning.

(defmacro with-open-frame (open-instruction ((&optional open-frame discardp destination) &body cleanup-body) &body body)
  (let ((cleanup-fun (gensymbol "CLEANUP-FUN"))
        (open-i (gensymbol "OPEN-INSTRUCTION"))
        (nopen-frame open-frame)
        (ndestination destination)
        (ndiscardp discardp))
    (unless nopen-frame
      (setq nopen-frame (gensymbol "OPEN-FRAME")))
    (unless ndiscardp
      (setq ndiscardp (gensymbol "DISCARDP")))
    (unless ndestination
      (setq ndestination (gensymbol "DESTINATION")))
    `(flet ((,cleanup-fun (,nopen-frame ,ndiscardp ,ndestination)
             ,@(unless discardp
                 ;; Only burn up symbols we created.  We want to get the "unused" warning iff
                 ;; he supplied the arg.
                 `(,ndiscardp))
             ,@(unless open-frame
                 `(,nopen-frame))
             ,@(unless destination
                 `(,ndestination))
             ,@cleanup-body))
       (let* ((,open-i ,open-instruction))
         (opening-frames (:new-frame (make-open-frame :open-instruction ,open-i
                                                      :tail-p (tail-open-p ,open-i)
                                                      :cleanup-generator #'cleanup-fun))
           (outi-for-k ,open-i)
           ,@body)))))

;;; This is used both as a subroutine of the above, and for P2ARGC-for-K
;;; In the P2ARGC-for-K case, the caller wraps the following macro around
;;; the entire generation of the call, and P2ARGC-for-K does the missing pieces
;;; by calling OUTI-OPEN-FOR-K.

(defmacro opening-frames ((&key new-frame) &body body)
  (let ((original-open (gensymbol "ORIGINAL-OPEN-FRAMES")))
    `(let ((,original-open *open-frames*)
           (*open-frames* ,(if new-frame
                               `(cons ,new-frame *open-frames*)
                             *open-frames*)))
       (multiple-value-prog1
         (progn ,@body)
         (clean-up-open-frames ,original-open)))))

;;; This is the other piece of what WITH-OPEN-FRAME does.
;;; It returns the new OPEN-FRAME, so we can check for the right thing later.
(defun outi-open-for-k (open-instruction tail-p cleanup-generator)
  (let ((new-frame (make-open-frame :open-instruction open-instruction
                                    :tail-p tail-p
                                    :cleanup-generator cleanup-generator)))
    (push new-frame *open-frames*)
    (outi-for-k open-instruction)
    new-frame))

;;; This is what does the normal completion of an open frame.

(defun outi-close-for-k (frame-level dest)
  (unless (eq frame-level *open-frames*)
    (error "Mismatch of frame levels; we're not finishing the frame we started."))
  (funcall (open-frame-cleanup-generator (car *open-frames*))
           nil dest)
  (unless (eq (cdr frame-level) *open-frames*)
    (error "Frame cleanup failed to consume the open frame.")))

;;; Call this when doing a "temporary" discard of excess stack.
;;; For example, when generating a branch or return.

(defmacro discarding-open-frames (level &body body)
  `(let* ((*open-frames* *open-frames*))
     (clean-up-open-frames ,level)
     ,@body))

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
        as kind = (var-kind (car l))
        do (incf nnewvars)
        when (or (memq kind '(fef-arg-req fef-arg-opt fef-arg-rest))
                 (and (not (eq (var-type (car l)) 'fef-special))
                      (memq kind '(fef-arg-aux fef-arg-internal-aux))))
        do (incf total-slots)
        when (and (eq (var-type (car l)) 'fef-special)
                  (memq kind '(fef-arg-req fef-arg-opt fef-arg-rest fef-arg-aux)))
        do (incf n-specbinds)
        when (memq kind '(fef-arg-rest fef-arg-key))
        do (setq rest-varstruct (car l))
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
      (outf-for-k `(entry ,(if rest-varstruct
                               (1- (minus entry-number))        ;rest arg itself counts!.
                             entry-number)))
                                                ;entry prefix
      (setq *dropthru* t)
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
            (generate-alloc-stack-slots stack-slots-to-create)))

      (cond (rest-varstruct     ;salt rest arg, if any.  Home exists now, even if on stack.
             (let* ((lap-address (var-lap-address rest-varstruct))
                    (intermediate-dest (intermediate-dest-for-store lap-address)))
               (outi-for-k `(k:move ,intermediate-dest gr:*value-1* k:boxed-right))
               (finish-store intermediate-dest lap-address))))

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
             (setq b-and-s-tag (gensym))
                                                ;output <branch b&s <n+1>
             (outj-for-k `(k:unconditional-branch ,b-and-s-tag))))
      (cond ((and key-arg (var-struct-supplied-p-variable-for-slot entry-number x newvars))
             (incf number-of-vars-supplied)))
      ))
  ;(outf-for-k `(entry-sequence-specbinds ,n-specbinds))
  (setq *entry-sequence-specbinds* n-specbinds))

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
    NBINDS))

;;; Output code for binding the var VARNAME as specified in its HOME.
;;; Return T if a BINDPOP or BINDNIL instruction was output.
(DEFUN P2LMB-FOR-K (VARNAME HOME &AUX INTCODE INITFORM)
  (BLOCK NIL
    (WHEN (NOT (ATOM VARNAME))
      (SETQ INITFORM (CADR VARNAME))
      (SETQ VARNAME (CAR VARNAME)))
    (UNLESS (EQ (VAR-NAME HOME) VARNAME)
      (BARF VARNAME "wrong home in P2LMB" 'BARF))
    (SETQ INTCODE (VAR-INIT HOME))
    ;; If this variable's binding is fully taken care of by function entry,
    ;; we have nothing to do here.
    (COND ((NOT (MEMQ (CAR INTCODE) '(FEF-INI-OPT-SA FEF-INI-COMP-C)))
           (RETURN NIL)))
    ;; Detect and handle internal special bound variables.
    (COND ((AND (EQ (VAR-KIND HOME) 'FEF-ARG-INTERNAL-AUX)
                (MEMQ (VAR-TYPE HOME) '(FEF-SPECIAL FEF-REMOTE)))
           (p2-for-k initform 'k:r0)
           (p2-output-b-and-s home 'k:r0)
;          ;; Output BINDNIL, or push value and BINDPOP.
;          (COND ((SI:MEMBER-EQUAL INITFORM '(NIL 'NIL))
;                 (OUTIV 'BINDNIL HOME))
;                (T (P2PUSH INITFORM)
;                   (OUTIV 'BINDPOP HOME)))
           (RETURN T)))
    ;; Otherwise, it's an internal local variable,
    ;; or else a special variable already bound by entering the function.
    ;; Don't bind, just init.
    (COND
;          ((SI:MEMBER-EQUAL INITFORM '(NIL 'NIL))
;          ;; if initting to NIL, then if no tags output so far (*TAGOUT* is NIL)
;          ;; we can assume it is still NIL from function entry time.
;          (COND ((OR *TAGOUT*
;                     (MEMQ (VAR-TYPE HOME) '(FEF-SPECIAL FEF-REMOTE))
;                     (VAR-OVERLAP-VAR HOME))
;                 (OUTIV 'SETNIL HOME))))
          ;; If explicitly says value does not matter, do nothing to initialize.
          ((EQUAL INITFORM '(UNDEFINED-VALUE))
           NIL)
          ;; Initting var to itself;  do nothing.
          ((AND (EQ (VAR-TYPE HOME) 'FEF-REMOTE)
                (EQ INITFORM VARNAME)))
;         ((EQUAL INITFORM ''0)
;          (OUTIV 'SETZERO HOME))
          (T
           (p2-to-variable initform home)
;            (P2PUSH INITFORM)
;            ;;If &OPTIONAL and for micro-compiler, just leave variable on stack.
;            (COND ((AND GENERATING-MICRO-COMPILER-INPUT-P
;                        (EQ (CAR INTCODE) 'FEF-INI-OPT-SA)))
;                  (T (OUTIV 'POP HOME)))
             ))
    ;; If there is a specified-flag variable, it was bound to T at entry.
    ;; Set it to NIL here (ie, if the arg was NOT specified).
;   (COND ((CDDR INTCODE)
;          (OUTIV 'SETNIL (CDDR INTCODE))))
    (COND ((EQ (CAR INTCODE) 'FEF-INI-OPT-SA)
           (PUTPROP (CADR INTCODE) T 'PEEP-KEEP)
           (OUTF-for-k (CADR INTCODE))))
    (RETURN NIL)))

(defun p2-output-b-and-s (var-struct source)    ;do "bind-and-store" for special variable.
                                                ;source must be addressible from right side OR
                                                ;  if a number, represents that arg-slot number.
  (let ((type (var-type var-struct))
        (symbol (var-name var-struct))
        ;(lap-address (var-lap-address var-struct))
        )
    (cond ((eq type 'fef-local))        ;do nothing if local, variable "lives" in final slot
          ((eq type 'fef-special)
           (if (numberp source)
               (cond ((< source *frame-registers-used-for-argument-passing*)
                      (setq source (a-n source)))
                     (t
                      (read-stack-slot source 'k:r0 nil)
                      (setq source 'k:md))))
           (outi-for-k `(k:movei k:o0 (quote ,symbol) k:boxed k:ch-open))
           (outi-for-k `(k:move k:o1 ,source k:boxed-right))
           (outi-for-k '(k:call (li:bind 2) k:ignore)))
          (t
           (fsignal "foo")))))

(defun p2-init-supplied-variable (is-sup? var-struct)
  (cond ((nc:self-evaluating? is-sup?)
         (setq is-sup? `(quote ,is-sup?))))
  (p2-to-variable is-sup? var-struct))

(defun p2-to-variable (exp var-struct)
  ;compile exp, getting the value in the variable belonging to VAR-STRUCT.  Actually, the value really
  ; gets left in K:R0 if the variable in question is special.  This allows the called to
  ; emit a call to bind or %%set as desired.
  (let* ((lap-address (var-lap-address var-struct))
         (intermediate-dest (intermediate-dest-for-store lap-address)))
    (p2-for-k exp intermediate-dest)
    (cond ((eq (car lap-address) 'special)
           nil)         ;special guys sit in k:R0 until B&S operation!
          (t
           (finish-store intermediate-dest lap-address)))))

(defun intermediate-dest-for-store (lap-address)
  (cond ((eq (car lap-address) 'special) 'k:r0)
        ((eq (car lap-address) 'arg) (a-n (cadr lap-address)))
        ((eq (car lap-address) 'local) '(k:md k:boxed-md))
        (t (fsignal "cant figure out lap address"))))

(defun finish-store (intermediate-dest lap-address &optional (call-dest 'k:ignore))
  (cond ((eq (car lap-address) 'special)
         (outi-for-k `(k:movei k:o0 (quote ,(cadr lap-address)) k:boxed k:ch-open))
         (outi-for-k `(k:move k:o1 ,intermediate-dest k:boxed-right))
         (outi-for-k `(k:call (symbol:%%set 2) ,call-dest)))
        ((eq (car lap-address) 'arg)
         (if (not (eq intermediate-dest (a-n (cadr lap-address))))
             (outi-for-k `(k:move ,(a-n (cadr lap-address)) ,intermediate-dest k:boxed-right))))
        ((eq (car lap-address) 'local)
         (write-stack-slot (cond ((equal intermediate-dest '(k:md k:boxed-md)) 'already-in-md)
                                 (t intermediate-dest))
                           (cadr lap-address)))
        (t (fsignal "cant figure out lap address"))))

(defun k-dest-from-lap-address (lap-address)
  (let ((ans (cond ((eq (car lap-address) 'arg)
                    (a-n (cadr lap-address))))))
    (if (null ans)
        (ferror nil "Cant convert address"))
    ans))

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
         (var-kind (var-kind var-struct))
         ;(var-misc (var-misc var-struct))
         ;(var-init (var-init var-struct))
         )
    (cond ((eq var-kind 'fef-arg-opt)
           (if (nc:self-evaluating? var-init-from-ll)
               (setq var-init-from-ll `(quote ,var-init-from-ll)))
           (cond ((< arg-slot-number *frame-registers-used-for-argument-passing*)
                  (p2-for-k var-init-from-ll (a-n arg-slot-number)))
                 (t
                  (p2-for-k var-init-from-ll '(k:md k:boxed-md))
                  (write-stack-slot 'already-in-md (- arg-slot-number *frame-registers-used-for-argument-passing*))))
;          (cond ((eq (car var-init) 'fef-ini-opt-sa)
;                 (let* ((init-var-struct (cddr var-init))
;                        (init-var-misc (var-misc init-var-struct))
;                        (init-var-init (var-init init-var-struct)))
;                   (cond ((memq 'fef-arg-specified-flag init-var-misc)
;                          (fsignal "init-struct ~s" init-var-struct))
;                         (t (fsignal "otherwise confused")))))
;                (t (ferror nil "Confused")))
           ))))

(defun var-struct-supplied-p-variable-for-slot (slot-number arglist varlist)
  (let* ((arg-var-struct (var-struct-for-slot slot-number arglist varlist))
         (arg-var-kind (var-kind arg-var-struct))
         ;(arg-var-misc (var-misc arg-var-struct))
         (arg-var-init (var-init arg-var-struct)))
    (cond ((eq arg-var-kind 'fef-arg-req) nil)
          ((eq (car arg-var-init) 'fef-ini-opt-sa)
           (let* ((init-var-struct (cddr arg-var-init))
                  (init-var-misc (var-misc init-var-struct))
                  ;(init-var-init (var-init init-var-struct))
                  )
             (cond ((null init-var-struct) nil)
                   ((memq 'fef-arg-specified-flag init-var-misc)
                    init-var-struct)
                   (t (fsignal "otherwise confused")))))
          (t (ferror nil "Confused")))))

(defun var-struct-for-slot (slot-number arglist varlist)
  (let* ((arg-spec (nth slot-number arglist))
         (arg-symbol (if (symbolp arg-spec) arg-spec (car arg-spec)))
         (var-struct (assq arg-symbol varlist)))
    (if (null var-struct) (ferror nil "Failed to find struct for slot ~d" slot-number))
    var-struct))

;(DEFUN p2sbind-for-k-top-level (x NEWVARS *VARS* &aux rest-flag)
;  (LET ((NBINDS 0)                             ;Number of (internal-aux) special bindings
;       (NNEWVARS (LOOP FOR L ON NEWVARS UNTIL (EQ L *VARS*)
;                       when (memq (var-kind (car l))
;                                  '(fef-arg-rest fef-arg-key))
;                       do (setq rest-flag t)
;                       COUNT T))
;       (nreqargs (loop for l on newvars until (eq l *vars*)
;                       when (eq (var-kind (car l)) 'fef-arg-req)
;                       count t))
;       (noptargs (loop for l on newvars until (eq l *vars*)
;                       when (eq (var-kind (car l)) 'fef-arg-opt)
;                       count t)))
;    (DO ((X X (CDR X))
;        (argn 0 (1+ argn))
;        (HOME) (KIND))
;        ((NULL X)
;        (if (zerop argn) (outf-for-k `(entry ,argn))))
;      (SETQ HOME (NTH (1- NNEWVARS) NEWVARS)
;           kind (var-kind home))
;      (when (eq kind 'fef-arg-opt)
;       (outf-for-k `(entry ,argn)))
;      (WHEN (p2-output-var-initcode-for-k (CAR X) HOME)        ;was P2LMB
;       (INCF NBINDS))
;      ;; Set *VARS* to the tail of NEWVARS starting at the variable we just handled
;      ;; or its optional-specified-flag.
;      (SETQ NNEWVARS (1- NNEWVARS))
;      (AND (CDDR (VAR-INIT HOME)) (SETQ NNEWVARS (1- NNEWVARS)))
;      (SETQ *VARS* (NTHCDR NNEWVARS NEWVARS)))
;    (OR (ZEROP NNEWVARS) (BARF X "VARS screwed up by this binding" 'BARF))
;    NBINDS))


;;;; Output code for binding the var VARNAME as specified in its HOME.
;;;; Return T if a BINDPOP or BINDNIL instruction was output.
;;-- compile init-code for variable to its "calling home". No binds --
;(DEFUN p2-output-var-initcode-for-k (VARNAME HOME &AUX INTCODE INITFORM)
;  (BLOCK NIL
;    (WHEN (NOT (ATOM VARNAME))
;      (SETQ INITFORM (CADR VARNAME))
;      (SETQ VARNAME (CAR VARNAME)))
;    (UNLESS (EQ (VAR-NAME HOME) VARNAME)
;      (BARF VARNAME "wrong home in P2LMB" 'BARF))
;    (SETQ INTCODE (VAR-INIT HOME))
;    ;; If this variable's binding is fully taken care of by function entry,
;    ;; we have nothing to do here.
;    (COND ((NOT (MEMQ (CAR INTCODE) '(FEF-INI-OPT-SA FEF-INI-COMP-C)))
;          (RETURN NIL))
;         ((eq (var-kind home) 'fef-arg-req)
;          (cond ((eq (var-type home) 'fef-special)
;                 (outi-for-k `(K:MOVEI K:O0 (quote ,varname) k:boxed k:ch-open))
;                 (outi-for-k `(K:MOVE K:O1 ,(a-n (find-position-in-alist varname *arg-map*))))
;                 (outi-for-k `(K:CALL (LI:BIND 2) K:IGNORE NIL))
;                 (return t))
;                (t
;                 (return nil)))))
;    ;; Detect and handle internal special bound variables.
;    (COND ((AND (EQ (VAR-KIND HOME) 'FEF-ARG-INTERNAL-AUX)
;               (MEMQ (VAR-TYPE HOME) '(FEF-SPECIAL FEF-REMOTE)))
;          ;; Output BINDNIL, or push value and BINDPOP.
;          (COND ((SI:MEMBER-EQUAL INITFORM '(NIL 'NIL))
;                 (OUTIV 'BINDNIL HOME))
;                (T (P2PUSH INITFORM)
;                   (OUTIV 'BINDPOP HOME)))
;          (RETURN T)))
;    ;; Otherwise, it's an internal local variable,
;    ;; or else a special variable already bound by entering the function.
;    ;; Don't bind, just init.
;    (COND ((SI:MEMBER-EQUAL INITFORM '(NIL 'NIL))
;          ;; if initting to NIL, then if no tags output so far (*TAGOUT* is NIL)
;          ;; we can assume it is still NIL from function entry time.
;          (COND ((OR *TAGOUT*
;                     (MEMQ (VAR-TYPE HOME) '(FEF-SPECIAL FEF-REMOTE))
;                     (VAR-OVERLAP-VAR HOME))
;                 (OUTIV 'SETNIL HOME))))
;         ;; If explicitly says value does not matter, do nothing to initialize.
;         ((EQUAL INITFORM '(UNDEFINED-VALUE))
;          NIL)
;         ;; Initting var to itself;  do nothing.
;         ((AND (EQ (VAR-TYPE HOME) 'FEF-REMOTE)
;               (EQ INITFORM VARNAME)))
;         ((EQUAL INITFORM ''0)
;          (OUTIV 'SETZERO HOME))
;         (T (p2-for-k initform (a-n (cadr (var-lap-address home))))
;            ;(P2PUSH INITFORM)
;            ;;If &OPTIONAL and for micro-compiler, just leave variable on stack.
;            (COND ((AND GENERATING-MICRO-COMPILER-INPUT-P
;                        (EQ (CAR INTCODE) 'FEF-INI-OPT-SA)))
;                  (T (OUTIV 'POP HOME)))))
;    ;; If there is a specified-flag variable, it was bound to T at entry.
;    ;; Set it to NIL here (ie, if the arg was NOT specified).
;    (COND ((CDDR INTCODE)
;          (OUTIV 'SETNIL (CDDR INTCODE))))
;    (COND ((EQ (CAR INTCODE) 'FEF-INI-OPT-SA)
;          (PUTPROP (CADR INTCODE) T 'PEEP-KEEP)
;          (OUTF-for-k (CADR INTCODE))))
;    (RETURN NIL)))

(defun find-position-in-alist (item alist)
  (do ((ans 0 (1+ ans))
       (p alist (cdr p)))
      ((null p))
    (if (eq item (caar p))
        (return ans))))

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
(DEFUN ASSIGN-LAP-ADDRESSES-for-k ()
  (LET ((ARGN 0)   ;Next arg number to allocate.
        (LVCNT 0)) ;Next local block slot number to allocate.
                   ;Count rest arg, auxes, and internal-auxes if they are not special.
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
                    ((EQ TYPE 'FEF-REMOTE)
                     `(REMOTE ,NAME))
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
               (incf argn)
  ;            (AND (= (SETQ ARGN (1+ ARGN)) #o101)
  ;                 (WARN 'TOO-MANY-SLOTS :IMPLEMENTATION-LIMIT
  ;                       "More than 64. arguments accepted by one function."))
              )
              ((OR (EQ TYPE 'FEF-LOCAL)
                   (NOT (MEMQ KIND '(FEF-ARG-INTERNAL FEF-ARG-INTERNAL-AUX))))
               (COND ((NOT (VAR-OVERLAP-VAR V))
                      (PUSH (AND PERMANENT-NAME (LIST PERMANENT-NAME)) *LOCAL-MAP*)
                      (incf lvcnt)
  ;                   (AND (= (SETQ LVCNT (1+ LVCNT)) 65.)
  ;                        (WARN 'TOO-MANY-SLOTS :IMPLEMENTATION-LIMIT
  ;                              "More than 64. local variable slots required by one function."))
                      )
                     (T (LET ((L1 (NTHCDR (- (LENGTH *LOCAL-MAP*)
                                             (CADR (VAR-LAP-ADDRESS V))
                                             1)
                                          *LOCAL-MAP*)))
                          (OR (NULL PERMANENT-NAME)
                              (MEMQ NAME (CAR L1))
                              (PUSH NAME (CAR L1))))))))))
  ;** probably flush this.
    (DOLIST (V *ALLVARS*)                               ;Fix FIXE's put in by VAR-COMPUTE-INIT
      (AND (EQ (CAR (VAR-INIT V)) 'FEF-INI-EFF-ADR)
           (EQ (CAADR (VAR-INIT V)) 'FIXE)
           (SETF (CADADR (VAR-INIT V)) (VAR-LAP-ADDRESS (CADR (CADADR (VAR-INIT V)))))))
    (SETQ *LOCAL-MAP* (NREVERSE *LOCAL-MAP*)
          *ARG-MAP* (NREVERSE *ARG-MAP*))
    ;; Clobber all nonspecial varnames in elements of
    ;; *CLOBBER-NONSPECIAL-VARS-LISTS* with NIL.
    ;; Clobber away all-NIL tails of those lists with NIL.
    (DOLIST (L *CLOBBER-NONSPECIAL-VARS-LISTS*)
      (LET ((LAST-NON-NIL-PTR L))
        (DO ((L1 L (CDR L1)))
            ((NULL L1))
          (LET ((HOME (ASSQ (CAR L1) *ALLVARS*)))
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
              (cond ((and (eq (car lap-address) 'local)
                          (>= (cadr lap-address) (- lvcnt register-slots-for-locals)))
                     (setf (var-lap-address v)
                           `(arg ,(+ argn (- (cadr lap-address) (- lvcnt register-slots-for-locals))))))
                    ((and (not (zerop stack-slots-for-args))
                          (eq (car lap-address) 'local)) ;make a gap in the locals for args.
                     (setf (var-lap-address v)
                           `(local ,(+ stack-slots-for-args (cadr lap-address)))))))))
  ;move args to locals if necessary
      (if (not (zerop stack-slots-for-args))
          (dolist (v *allvars*)
            (let* ((lap-address (var-lap-address v)))
              (cond ((and (eq (car lap-address) 'arg)
                          (>= (cadr lap-address) *frame-registers-used-for-argument-passing*))
                     (setf (var-lap-address v)
                           `(local ,(- (cadr lap-address) *frame-registers-used-for-argument-passing*))))))))
  ;locals slots 0-n are overflow args
  ;real locals are n+1 .. m
      (setq *stack-slots* (setq *base-stack-slots* total-stack-slots)))))

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
   ;      ((EQ (CAR-SAFE INIT-FORM) 'QUOTE))
   ;      ((self-evaluating-p init-form)
   ;       (SETQ INIT-FORM `',INIT-FORM))
   ;      ((EQUAL INIT-FORM '(UNDEFINED-VALUE))
           ;;This is simplest thing that works.
           ;; More hair is not needed for the ways these are usually generated by SETF.
   ;       (SETQ TLFUNINIT T))
          (T
           ;; Init is not NIL, constant or self => must P1 it, and maybe set TLFUNINIT.
           (LET ((*TLEVEL* NIL))
             (SETQ INIT-FORM (P1V INIT-FORM 1)))
           (UNLESS nil ;(ADRREFP INIT-FORM)
             (SETQ TLFUNINIT T))))
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
 ;          ((ATOM INIT-FORM)
 ;           (SETQ INIT-TYPE 'FEF-INI-C-PNTR)
 ;           (SETQ INIT-DATA (LIST 'LOCATIVE-TO-S-V-CELL INIT-FORM)))
            ((MEMQ (CAR INIT-FORM) '(LOCAL-REF))
             (SETQ INIT-TYPE 'FEF-INI-EFF-ADR)  ;Initted to value of local var
             (SETQ INIT-DATA (LIST 'FIXE INIT-FORM)))
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
           (SETQ INIT-DATA (GENSYM)))
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
  (when (plusp n)
    (generate-incr 'GR:*STACK-POINTER* 'GR:*STACK-POINTER* n)))

(defun generate-dealloc-stack-slots (n)
  (unless (zerop n)
    (generate-decr 'GR:*STACK-POINTER* 'GR:*STACK-POINTER* n)))

(defun read-stack-slot (slot &optional (reg 'K:R0) (insert-wait t))
  (generate-decr '(K:VMA-START-READ K:BOXED-VMA K:BOXED-MD) 'gr:*stack-pointer*
                 (+ (+ *base-stack-slots* (minus slot))
                    (- *stack-slots* *base-stack-slots*))
                 reg)
  (if insert-wait (emit '(K:MEMORY-WAIT))))     ;result comes back in K:MD

(defun write-stack-slot (from slot &optional (temporary-register-that-gets-clobbered 'K:R1))
  ;FROM must be addressible from processor right source.
  ;if from is 'ALREADY-IN-MD, then assume it is.  (Can't use K:MD for this because K:MD copying
  ;is sometimes necessary due to multiple md "feature" of hardware.)
  ;--remember boxed-bit decoding is different if dest is VMA or MD. Instruction always specifies both directly.
  (if (not (eq from 'already-in-md))
      (emit `(K:MOVE (K:MD K:BOXED-MD) ,from)))
  (generate-decr '(K:VMA-START-WRITE K:BOXED-VMA K:BOXED-MD) 'GR:*STACK-POINTER*
                 (+ (+ *base-stack-slots* (minus slot))
                    (- *stack-slots* *base-stack-slots*))
                 temporary-register-that-gets-clobbered)
  )

(defun move-stack-slot-to-stack-slot (from-slot to-slot)
  (generate-decr '(K:VMA-START-READ K:BOXED-VMA K:BOXED-MD) 'GR:*STACK-POINTER* (- *stack-slots* from-slot))
  (emit 'K:MEMORY-WAIT)
  (generate-decr '(K:VMA-START-WRITE K:BOXED-VMA) 'GR:*STACK-POINTER* (- *stack-slots* to-slot))
  )

;--- interface

(defun emit (x)
  (outf-for-k x))

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

(DEFUN P2MV-for-k (FORM DEST *M-V-TARGET*)
  (IF (NULL *M-V-TARGET*)
      (P2-for-k FORM DEST)
    (COND ((ADRREFP FORM)
           (P2-for-k FORM DEST))
          ((EQ (CAR FORM) 'LEXICAL-REF)
           (P2-for-k FORM DEST))
          ((memq (car form) '(%POP %pop-for-with-stack-list))
           (ferror "%pop or %pop-for-with-stack-list can't be used on the Falcon.")
           (P2-for-k FORM DEST))
          (T
           (P2F-for-k FORM DEST))))
  *M-V-TARGET*)

;;; Compile code to compute FORM and put the result in destination DEST.
;;; If DEST is D-IGNORE, we may not actually bother to compute the value
;;; if we can tell that there would be no side-effects.
;DEST can be a symbol in the K package such as K:O0, K:A1, K:R2, etc.
;DEST can also be a list (k:new-open <n>) or (k:new-tail-open <n>)
(DEFUN P2-FOR-K (FORM DEST)
  (let ()
    (prog1
      (COND ((ADRREFP FORM)
             (OR (EQ DEST 'D-IGNORE)
                 ;; (OUTI-for-k `(MOVE ,DEST ,(P2-SOURCE-for-k FORM DEST)))
                 (p2-compute-move-for-k dest form)
                 ))
            ((EQ (CAR FORM) 'LEXICAL-REF)
             (OR (EQ DEST 'D-IGNORE)
                 (PROGN (P2PUSH-CONSTANT (CADR FORM))
                        (OUTI-for-k `(MISC ,DEST %LOAD-FROM-HIGHER-CONTEXT)))))
            ((EQ (CAR FORM) '%POP)
             (ferror nil "Decommitted")
;        (IF (ZEROP PDLLVL)
;            (FERROR "%POP with nothing on the stack."))
;        (DECF PDLLVL)
;        (MOVE-RESULT-FROM-PDL DEST)
             )
            ;; like %pop, but doesn't affect pdllvl, see with-stack-list in p1
            ((EQ (CAR FORM) '%POP-for-with-stack-list)
             (ferror nil "Decommitted")
;        (IF (< PDLLVL 0)
;            (FERROR "%POP with nothing on the stack."))
;        (MOVE-RESULT-FROM-PDL DEST)
             )
            ((EQ (CAR FORM) 'CHANGE-PDLLVL)
;        (ferror nil "Decommitted")
;        (LET ((*BDEST* ())
;              (*M-V-TARGET* ()))
;          (PROG1 (P2F-FOR-K (CADDR FORM) DEST)
;                 (MKPDLLVL (+ PDLLVL (CADR FORM)))))
             )
            (T
             (LET ((*BDEST* ())
                   (*M-V-TARGET* ()))
               (P2F-FOR-K FORM DEST))))
      #+ignore
      (when (and (listp dest)
                 (memq (car dest) '(k:new-tail-open k:new-open)))
        (pop *open-frames*))
      #+ignore
      (unless (eq *open-frames* old-open-frames)
        (reg-error "Internal compiler error:  mismatched open frames.")))))

(DEFUN P2F-FOR-K (FORM DEST)
  (LET ((PDLLVL PDLLVL)
        (*P2FN* (CAR FORM))
        (ARGL (CDR FORM))
        TEM)
    (COND ((setq tem (get *p2fn* 'p2-for-k))
           (funcall tem (cdr form) dest))
 ;        ((SETQ TEM (GET *P2FN* 'P2))
 ;         (FUNCALL TEM (CDR FORM) DEST))
          ((SETQ TEM (GET *P2FN* 'QINTCMP))
           (P2MISC-for-k *P2FN* ARGL DEST TEM))
          (T
           (P2ARGC-for-k *P2FN* ARGL (GETARGDESC *P2FN*) DEST *P2FN*)))))

(defun p2-compute-move-for-k (dest form &aux tem)
  (cond ((atom form)
         (outi-for-k `(k:movei k:o0 (quote ,form) k:boxed k:ch-open))
         (outi-for-k `(k:call (symbol:%symbol-value 1) ,dest)))
        ((eq (car form) 'local-ref)
         (let ((lap-address (var-lap-address (cadr form))))
           (cond ((eq (car lap-address) 'arg)
                  (outi-for-k `(k:move ,dest ,(a-n (cadr lap-address)) k:boxed-right)))
                 ((eq (car lap-address) 'local)
                  (read-stack-slot (cadr lap-address))
                  (outi-for-k `(k:move ,dest k:md k:boxed-right)))
                 (t (ferror "Cant deal with this lap address ~s " lap-address)))))
        ((eq (car form) 'lexical-ref)
         (fsignal "lexical-ref"))
        ((memq (car form) '(function quote break-off-function self-ref))
         (cond ((and (eq (car form) 'quote)
                     (setq tem (k-find-constant-register (cadr form))))
                (outi-for-k `(k:move ,dest ,tem k:boxed-right)))        ;use move in hope it can be combined into jump.
               (t
                (outi-for-k `(k:movei ,dest ,form k:boxed)))))
        (t (ferror nil "this is adrrefp?? ~s" form))))


;;; Compile something to be addressed by an instruction.
;;; Return the address which the instruction can address it by.
;;; Can push the value on the stack and return PDL-POP,
;;; or for a variable or constant can just return its address.
;;; DEST is significant only if it is D-IGNORE, in which case
;;; we compile code to compute and ignore the value.  What we return then is irrelevant.
(DEFUN P2-SOURCE-for-k (FORM DEST)
  (COND ((ATOM FORM)
         `(SPECIAL ,FORM))
        ((EQ (CAR FORM) 'LOCAL-REF)
         (VAR-LAP-ADDRESS (CADR FORM)))
        ((EQ (CAR FORM) 'LEXICAL-REF)
         (COND ((NEQ DEST 'D-IGNORE)
                (P2PUSH-CONSTANT (CADR FORM))
                (OUTI-for-k '(MISC D-PDL %LOAD-FROM-HIGHER-CONTEXT))))
         '(PDL-POP))
        ((MEMQ (CAR FORM) '(FUNCTION QUOTE BREAKOFF-FUNCTION SELF-REF))
         `(QUOTE-VECTOR ,FORM))
        (T (LET ((*BDEST* NIL)
                 (*M-V-TARGET* NIL))
             (P2F FORM (COND ((EQ DEST 'D-IGNORE) 'D-IGNORE) (T 'D-PDL)))
             '(PDL-POP)))))


;;;; Compile functions which have their own special instructions.

;;; Here for a "miscellaneous" instruction (no source address field; args always on PDL).
;;; Such functions have no P2 properties.  We recognize them by their QINTCMP
;;; properties, which hold the number of args which the function takes.
(DEFUN P2MISC-for-k (INSN ARGL DEST NARGS)
  (WHEN ( NARGS (LENGTH ARGL))                 ;Too few args
    (BARF INSN "P2MISC WNA"))
  (P2ARGC-for-k INSN ARGL (LIST (CONS NARGS '((FEF-ARG-REQ FEF-QT-EVAL))))
                DEST *P2FN*))

(DEFUN P2ARGC-for-k (FCTN ARGL DESC DEST TARGET &OPTIONAL MAPPING-TABLE)
  (opening-frames ()
    (LET (COUNT TOKEN-LIST AG1 DSC1 CALLI TM TDEST LDEST
          RESTART-PC ADI-LIST
          (MVTARGET *M-V-TARGET*)
          (*CALL-BLOCK-PDL-LEVELS* *CALL-BLOCK-PDL-LEVELS*)
          (*stack-slots* *stack-slots*) ;any stack-slots created for call are removed by callee.
          (previous-stack-slots *stack-slots*)  ;cant wait for unbinding to restore this
          (argn 0)                      ;  see below.
          (call-terms nil)              ;total number of args being passed.
          (k-dest nil)                  ;destination for arg on K.
          (tail-call-switch
            (and (eq dest 'd-return)
                 (null *specialflag*)
                 (zerop *base-stack-slots*)
                 (null *open-frames*)))
          (our-open-frame))
      (labels ((generate-call-discard (open-frame operation dest)
                 (ecase operation
                   ((nil)
                    (if tail-call-switch
                        (outi-for-k `(k:tail-call (,target ,argn)))
                      (outi-for-k `(k:call (,target ,argn) ,dest))))
                   ((:discard)
                    (if (not tail-call-switch)
                        (outi-for-k `(k:call (li::discard-open-frame 0) k:r0))
                      ;; Clean up a TAIL-OPEN.  The analysis is:
                      ;;                   O A R
                      ;; Initial state:    A A R  ; O had better have the same as A, on *ANY* tail-call.
                      ;; Tail open:        F A R  ; F is new from heap.
                      ;; ...
                      ;; Tail open:        F F A  ; R goes to heap.
                      ;; Tail open-call:   A A F  ; Once again in a valid state; with a different R reg.

                      (outi-for-k `(k:tail-call (0 0) nil next-pc-pc+1))
                      (outi-for-k `(k:tail-open-call (0 0) nil next-pc-pc+1))))
                   ((:return)
                    ;; Returning; just change our mind and call PROG1-INTERNAL instead.
                    (outi-for-k `(k:tail-call (li::prog1-internal 1) (o0 ,dest k:boxed-right)))))))
        (PROG ()
              (SETQ AG1 ARGL)
              (setq call-terms (length ag1))
              ;; Whatever our caller wants in the way of multiple values,
              ;; we will do it for him.  Say so.
              (SETQ *M-V-TARGET* NIL)
;;;       (SETQ CALLI (COND ((NULL ARGL) 'CALL0)
;;;                         (T 'CALL)))
              (SETQ LDEST (SETQ TDEST DEST))    ;May get changed to D-PDL below
              ;;TDEST is destination actually to be compiled into CALL instruction.
              ;;LDEST is "logical" destination.  This is usually the same except in case of multiple values.
              ;; Then TDEST is assembled D-IGNORE (it is actually ignored by the microcode, but doing
              ;; this confuses the micro-compiler least), while LDEST is D-PDL, reflecting the fact that the
              ;; values actually show up on the PDL.
              (COND ((NULL MVTARGET))
                    ((EQ MVTARGET 'MULTIPLE-VALUE-LIST)
                     (SETQ ADI-LIST
                           (CONS MVTARGET (CONS NIL ADI-LIST)))
                     (SETQ TDEST 'D-IGNORE LDEST 'D-PDL))
                    ((EQ MVTARGET 'THROW)
                     (SETQ ADI-LIST `(MULTIPLE-VALUE (QUOTE-VECTOR 'NIL) . ,ADI-LIST)
                           TDEST 'D-PDL LDEST 'D-PDL))
                    ((EQ MVTARGET 'RETURN)
                     (SETQ ADI-LIST `(MULTIPLE-VALUE (QUOTE-VECTOR 'T) . ,ADI-LIST)
                           TDEST 'D-PDL LDEST 'D-PDL))
                    ((NUMBERP MVTARGET)
                     ;; MVTARGET is a number => it is number of values,
                     ;; just leave them on the stack.
                     (SETQ ADI-LIST `(MULTIPLE-VALUE (QUOTE-VECTOR ',MVTARGET) . ,ADI-LIST)
                           TDEST 'D-IGNORE LDEST 'D-PDL)))
              (SETQ DSC1 DESC)
;         (COND ;; Use of FEXPR-CALL turned on 11/16/82.
;               ((AND (MEMQ 'FEF-ARG-REST (SETQ TM (CADAR (LAST DSC1))))
;                     (MEMQ 'FEF-QT-QT TM))
;                (SETQ CALLI 'CALL)
;                (SETQ ADI-LIST (CONS 'FEXPR-CALL
;                                     (CONS NIL ADI-LIST)))))
              (COND ((NOT (SYMBOLP FCTN))
                     (fsignal "funcall")
                     (SETQ TM FCTN))    ;Non-symbolic fctn; it's address for CALL
                    (T (SETQ TM `(QUOTE-VECTOR (FUNCTION ,TARGET)))))
              (COND ((NULL ADI-LIST)
                     (when (null ag1)
                       (outi-open-for-k (if tail-call-switch
                                            `(k:tail-open)
                                          `(k:open))
                                        tail-call-switch
                                        #'generate-call-discard)
                       (setq our-open-frame *open-frame*))
                     ;(OUTI-for-k (LIST CALLI TDEST TM))
                     )
                    (T (fsignal "lose ADI")
                       (OUTI1-for-k (LIST 'ADI-CALL CALLI TDEST TM ADI-LIST))
                       (MKPDLLVL (+ PDLLVL (LENGTH ADI-LIST)))))
          (COND ((NULL MVTARGET))
                ((EQ MVTARGET 'MULTIPLE-VALUE-LIST)
                 ;(INCPDLLVL)
                 )
                ((NUMBERP MVTARGET)
                 ;(MKPDLLVL (+ PDLLVL MVTARGET))
                 ))
          (PUSH PDLLVL *CALL-BLOCK-PDL-LEVELS*)
          ;(MKPDLLVL (+ 4 PDLLVL))
       L4
          (COND ((NULL DSC1) (GO X2)))
          (SETQ COUNT (CAAR DSC1))
          (SETQ TOKEN-LIST (CADAR DSC1))
          (COND ((MEMQ 'FEF-ARG-REST TOKEN-LIST)
                 (SETQ COUNT #o1005)))
       L3 (setq k-dest (cond ((zerop argn) (if tail-call-switch '(k:new-tail-open 0) '(k:new-open 0)))
                             ((< argn *frame-registers-used-for-argument-passing*) (o-n argn))
                             (t 'k:r2)))        ;then put it in stack-slot
          (COND ((= 0 COUNT) (SETQ DSC1 (CDR DSC1)) (GO L4))
                ;; The following clause works with the FEXPR-CALL ADI
                ;; and was turned on 11/16/82.
                ((AND (MEMQ 'FEF-ARG-REST TOKEN-LIST)
                      (MEMQ 'FEF-QT-QT TOKEN-LIST))
                 (GO OFEXPR))   ;DO THIS EVEN IF ARG LIST IS NULL
                ((NULL AG1) (GO RET))   ;OUT OF ARG LIST
                ((MEMQ 'FEF-QT-QT TOKEN-LIST)
                 (if (not (zerop argn))
                     (outi-for-k `(move ,k-dest (quote-vector ',(car ag1))))
                     ;; If this is the first one, it'll do an open.
                   (outi-open-for-k `(move ,k-dest (quote-vector ',(car ag1)))
                                    tail-call-switch #'generate-call-discard)))
                ((MEMQL '(FEF-QT-EVAL FEF-QT-DONTCARE) TOKEN-LIST)
                 (COND ((AND (NULL (CDR AG1))
                             (MEMQ 'LEXPR-FUNCALL TOKEN-LIST))
                        (fsignal "%spread")
                        (P2-for-k (CAR AG1) 'D-PDL)     ;Arg to %SPREAD
                        (OUTI-for-k (LIST 'MISC k-dest '%SPREAD)))
                       (T (P2-for-k (CAR AG1) k-dest))))
                (T (BARF TOKEN-LIST
                         'TOKEN-LIST-LOSES-P2
                         'BARF)))
          (if (>= argn *frame-registers-used-for-argument-passing*)
              (k-push-stack-arg k-dest))          ;this will increment *stack-slots*
          (incf argn)
          (INCPDLLVL)
          (SETQ AG1 (CDR AG1))
          (DECF COUNT)
          (GO L3)
       X2
          (COND (AG1 (SETQ DSC1 '((#o1005 (FEF-ARG-OPT FEF-QT-EVAL))))  ;Compile the rest
                     (GO L4)))                  ;of them; he may possibly know what he's doing
       RET
          ;(outf-for-k `(activate-open-call ,argn))     ;signal cross compiler.  Corresponds to D-LAST.
          (setq *stack-slots* previous-stack-slots)     ;on dest D-RETURN, outi-for-k will output
          (outi-for-k (if tail-call-switch              ; exit sequence.  Make sure *stack-slots*
                          `(k:tail-call (,target ,argn)); reflects that by then, called fctn
                        `(k:call (,target ,argn) ,dest))) ;will have returnned and gobbled its args.
          (COND (MAPPING-TABLE
                 (P2PUSH MAPPING-TABLE)
                 (OUTI-for-k '(MISC D-LAST %SET-SELF-MAPPING-TABLE))))
          (COND (RESTART-PC
                 (SETQ *DROPTHRU* T)
                 (OUTF-for-k (LIST 'RESTART-TAG RESTART-PC))))
          (COND ((NULL MVTARGET))
                ((EQ MVTARGET 'MULTIPLE-VALUE-LIST))
                ((EQ MVTARGET 'THROW)
                 (RETURN NIL))
                ((EQ MVTARGET 'RETURN)
                 (RETURN NIL))
                ((NUMBERP MVTARGET) (RETURN NIL)))
          (COND ((NOT (EQ LDEST DEST))          ;Interested in where value is, not what was
                 (MOVE-RESULT-FROM-PDL DEST)))  ;assembled into call
          (COND ((AND (EQ DEST 'D-RETURN)
                      (NULL RESTART-PC))
                 (TAKE-DELAYED-TRANSFER)))
          (RETURN NIL)

       OFEXPR
          (OUTI-for-k `(MOVE D-LAST (QUOTE-VECTOR (QUOTE ,AG1))))
          (GO RET)
          ))))


;P2-FOR-K generators

(DEFUN (:PROPERTY COND P2-FOR-K) (ARGL DEST)
  (PROG (CLAUSE TAG TAG1 TAG2 VALF CLAUSE-LENGTH TM IDEST PRED NOFALLTHRU
         LAST-CLAUSE-FLAG IDEST-USED)
        (SETQ TAG2 (GENSYM))                    ;Tag to go to with value of COND in DEST
        (SETQ TAG (GENSYM))                     ;Tag to go to with value of COND in IDEST
        ;; Choose an intermediate destination, depending on ultimate destination.
        ;; The intermediate destination can match the ultimate one
        ;; if they are D-IGNORE, D-INDS or D-PDL.
        ;; Each COND clause can compile its value to IDEST and go to TAG
        ;; or compile its value to DEST and go to TAG2.

        ;; Use of TAG and IDEST assumes that multiple values were NOT generated
        ;; whereas TAG2 and DEST assumes that they were if they are supposed to be.

        ;; For microcompiler input, we always use TAG and IDEST unless IDEST=DEST.
        ;; Otherwise, we usually use DEST except for clauses that are just predicates.

        ;; IDEST-USED is T if a clause has compiled its result to IDEST.
        ;; The code to move the value is only generated if IDEST/TAG has been used.
        (AND *M-V-TARGET* (SETQ DEST 'D-PDL))

        (setq idest dest)
;       (SETQ IDEST 'D-IGNORE)
;       (COND ((NOT (EQ DEST 'D-IGNORE))
;              (SETQ VALF T)
;              (SETQ IDEST 'D-PDL)))
;       (IF (EQ DEST 'D-INDS) (SETQ IDEST 'D-INDS))

        ;; Compile next clause.
     L1
        (IF (NULL (CDR ARGL)) (SETQ LAST-CLAUSE-FLAG T))
        (SETQ CLAUSE (CAR ARGL))
        (AND LAST-CLAUSE-FLAG (NULL (CDR CLAUSE))
             (SETQ CLAUSE (CONS ''T CLAUSE)))
        (SETQ TAG1 (GENSYM))
        (SETQ PRED (CAR CLAUSE))
        (WHEN (EQ (CAR-SAFE PRED) 'QUOTE)
          (COND ((NULL (CADR PRED))     ;Is the null condition?
                 (AND (NOT LAST-CLAUSE-FLAG)
                      (GO L5)))         ;Yep. Can happen as result of DO expansion.
                ((CDR ARGL)             ;condition always true?
                 (SETQ LAST-CLAUSE-FLAG T)      ;If so, discard any remaining clauses
                 (SETQ NOFALLTHRU T)    ;after a warning about them.
;These can come from expanding DEFSUBSTs that contain CONDs, with constant arguments.
;                (WARN 'UNREACHABLE-CODE ':IMPLAUSIBLE
;                      "Some COND clauses are unreachable;
; the first starts with ~S."
;                (CAADR ARGL))
                 (SETQ ARGL (LIST CLAUSE)))
                (T
                 (SETQ NOFALLTHRU T))))
        (SETQ CLAUSE-LENGTH (LENGTH CLAUSE))
        ;; Handle certain special cases of clauses.
        (COND ((AND VALF (= 1 CLAUSE-LENGTH))
               ;; Clause containing only one element, compiled for value.
               ;; value of condition is also value of clause.
               (P2-for-k PRED IDEST)
               (SETQ IDEST-USED T)
               ;; If something pushed, pop if the branch is not taken
               (OUTB-for-k `(BRANCH NILIND FALSE ,(EQ IDEST 'D-PDL) ,TAG))
               (GO L5))
              ;; Clause of one element, if value is not wanted.
              ((= 1 CLAUSE-LENGTH) (BOOL1-for-k PRED 'FALSE TAG) (GO L5))
              ;; Clause is just condition followed by a GO.
              ((AND (= 2 CLAUSE-LENGTH)
                    (SIMPLEGOP (CADR CLAUSE)))
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
                    (SIMPLEGOP (CADR TM)))
               ;; In effect, we turn this into (COND ((NOT P1) (GO X)) (T A1))
               (BOOL1-for-k PRED 'TRUE (GTAG (CADADR TM)))      ;Go X directly if P1 false
               (SETQ ARGL (CONS (CONS ''T (CDR CLAUSE)) (CDDR ARGL)))
               (GO L1))
              ((NOT NOFALLTHRU)                 ;Normal COND clause.
               (BOOL1-for-k PRED 'TRUE TAG1)))  ;Jump around clause if predicate fails.

        ;; If the COND will have to return NIL if this clause's
        ;; condition is false, then generate a clause to return the nil.
        (WHEN (AND VALF LAST-CLAUSE-FLAG (NOT NOFALLTHRU))
          (SETQ ARGL (LIST CLAUSE '('T 'NIL)))
          (SETQ LAST-CLAUSE-FLAG NIL))

        ;; Compile the actions of the cond clause, except for the last.
        (DO ((ACTIONS (CDR CLAUSE) (CDR ACTIONS)))
            ((NULL (CDR ACTIONS))
             (SETQ CLAUSE ACTIONS))
          (P2-for-k (CAR ACTIONS) 'D-IGNORE))

        ;; Compile last action of cond clause (the value).
        (LET ((TO-IDEST-P
                ;; Send value of last clause to IDEST rather than DEST
                ;; if that means we can avoid a branch to TAG2
                ;; that would otherwise be necessary.
                ;; Send values of all clauses to IDEST for microcompiler input.
                (OR (AND LAST-CLAUSE-FLAG
                         IDEST-USED
                         (NEQ DEST IDEST)
                         ;; Don't do this optimization if mult values wanted
                         ;; because only compilation to DEST can accept them.
                         (NULL *M-V-TARGET*)
                         ;; If D-RETURN, don't optimize, so it can propagate
                         ;; multiple values if there are any.
                         (NEQ DEST 'D-RETURN))
                    (AND GENERATING-MICRO-COMPILER-INPUT-P
                         (NOT (EQ DEST IDEST))))))
          (COND (TO-IDEST-P (P2-for-k (CAR CLAUSE) IDEST))
                ((EQUAL (CAR CLAUSE) ''NIL)
                 ;; Avoid "Doesn't really produce multiple values"
                 ;; for internally generated 'NIL.
                 (OUTI-for-k `(K:MOVE ,DEST ,(k-find-constant-register 'nil) k:boxed-right))
                 (AND *M-V-TARGET* (SETQ TO-IDEST-P T)))
                ((P2MV-for-k (CAR CLAUSE) DEST *M-V-TARGET*)
                 ;; If value fails to generate mult vals,
                 ;; we must make TAG generate them and go there.
                 (SETQ TO-IDEST-P T)))
          (IF (NULL TO-IDEST-P)
              (IF (OR (NULL LAST-CLAUSE-FLAG)
                      ;; If last clause, and TAG isn't the same as TAG2,
                      ;; we must still branch to TAG2.
                      (AND IDEST-USED (OR *M-V-TARGET* (NEQ DEST IDEST))))
                  (OUTB-for-k `(BRANCH ALWAYS NIL NIL ,TAG2)))
            (SETQ IDEST-USED T)
            (IF (NULL LAST-CLAUSE-FLAG)
                (OUTB-for-k `(BRANCH ALWAYS NIL NIL ,TAG)))))

        ;; Here at end of cond-clause.
     L5
        (OUTTAG TAG1)                           ;Output tag for jumps from failing predicate.
        (IF (SETQ ARGL (CDR ARGL))              ;If there are more clauses, process them.
            (GO L1))

        ;; There are no more cond clauses!
        (OUTTAG TAG)
        (AND IDEST-USED
             (COND ((NUMBERP *M-V-TARGET*)
                    (DOTIMES (I (1- *M-V-TARGET*))
                      (OUTI-for-k '(MOVE D-PDL (QUOTE-VECTOR 'NIL)))))
                   ((EQ *M-V-TARGET* 'MULTIPLE-VALUE-LIST)
                    (OUTI-for-k '(MISC D-PDL NCONS)))
                   ((NEQ DEST IDEST)
                    (MOVE-RESULT-FROM-PDL DEST))))
        ;; We have generated multiple values if necessary.
        (SETQ *M-V-TARGET* NIL)
        (OUTTAG TAG2)
        (RETURN NIL)))

;;; Compile code to test CONDITION and jump to tag if it is NIL
;;; (for SENSE = TRUE) or if it is non-NIL (for SENSE = FALSE).
(DEFUN BOOL1-for-k (CONDITION SENSE TAG)
  (P2BRANCH-for-k CONDITION 'D-INDS
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
;;; A conditional branch should only be used with DEST = D-INDS.
;;; This is taken to imply that the indicators are used by the branch,
;;; not that the indicators will be correctly set up after the optimized
;;; code is finished branching or not.  If you wish to compile something
;;; and want the indicators correctly set up according to its value,
;;; you should use D-INDS with no *BDEST*, and do your branching yourself.

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
  (AND (MEMQ DEST '(D-PDL D-NEXT))
       (NEEDPDL 1))
  (COND ((AND *BDEST* (NEQ (CADR *BDEST*) 'ALWAYS)
              (NEQ DEST 'D-INDS))
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
        ((ADRREFP FORM)
         (OR (EQ DEST 'D-IGNORE)
             ;(OUTI-for-k `(MOVE ,DEST ,(P2-SOURCE-for-k FORM DEST)))
             (p2-compute-move-for-k dest form)
             ))
        ((EQ (CAR FORM) 'LEXICAL-REF)
         (P2-for-k FORM DEST))
;       ((memq (CAR FORM) '(%POP %pop-for-with-stack-list))
;        (P2-for-k FORM DEST))
        (T (LET ((*M-V-TARGET* NIL))
             (P2F-for-k FORM DEST))))
  (AND *BDEST* (OUTB-for-k (COPY-LIST *BDEST*))))

(defprop list-in-area p2list-cross p2-for-k)
(defprop list*-in-area p2list-cross p2-for-k)
(defun p2list-cross (argl dest &aux area)
  (when (memq *p2fn* '(list-in-area list*-in-area))
    (setq area (pop argl)))
  (fsignal "convert this"))

(DEFUN (:PROPERTY ATOM P2-FOR-K) (ARGL DEST)    ;just treat ATOM normally.  This effectively overrides the normal P2.
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

(DEFPROP AND P2ANDOR-for-k P2-FOR-K)
(DEFPROP OR P2ANDOR-for-k P2-FOR-K)

(DEFUN P2ANDOR-for-k (ARGL DEST)
  (LET ((SENSE (IF (EQ *P2FN* 'AND) 'TRUE 'FALSE)))
    (when (and (null *m-v-target*)
               (memq dest '(0 d-inds d-ignore)))
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
    (MULTIPLE-VALUE-BIND (TAG UNCONDITIONAL)
        (IF (AND (EQ (CADR *BDEST*) 'ALWAYS)
                 (NULL *M-V-TARGET*)
                 (MEMQ DEST '(D-PDL D-INDS D-IGNORE 0)))
            (VALUES (CAR (CDDDDR *BDEST*)) T)
            (VALUES (GENSYM) NIL))
      (LET (TAG1)
        (COND ((AND (NULL *M-V-TARGET*) (EQ DEST 'D-IGNORE))
               ;; Compilation strategy for AND for effect:
               ;; compute each arg, using it only to jump to end if it's NIL.
               ;; The last one we just ignore, but we feed it our *BDEST* for
               ;; branch tensioning.  However, (AND form (GO tag)) can be optimized
               ;; by making it a conditional jump to tag rather than a jump around a jump.
               (DO ((ARGL ARGL (CDR ARGL)))
                   ((NULL (CDR ARGL))
                    (P2BRANCH-for-k (CAR ARGL) DEST *BDEST*))
                 (AND (SIMPLEGOP (CADR ARGL))
                      (RETURN (BOOL1-for-k (CAR ARGL) (OTHER SENSE) (GTAG (CADADR ARGL)))))
                 ;; If the next arg of this AND is NIL, this arg is effectively last.
                 ;; However, if AND has a branch destination, it must compute
                 ;; whether to branch based on the NIL, not on this arg.
                 (AND (EQ (CAR-SAFE (CADR ARGL)) 'QUOTE)
                      (EQ (NULL (CADADR ARGL))
                          (EQ SENSE 'TRUE))
                      (RETURN (P2BRANCH-for-k (CAR ARGL) DEST *BDEST*)))
                 (BOOL1-for-k (CAR ARGL) SENSE TAG)))
              ((AND (NULL *M-V-TARGET*) (EQ (CADR *BDEST*) 'NILIND))
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
                     (BOOL1-for-k (CAR ARGL) SENSE TAG)))
               (SETQ *BDEST* NIL))
              (T
               ;; Compilation strategy for AND for value
               ;; (correct indicators required counts as for value):
               ;; AND for multiple values is like AND for value on the stack,
               ;; except that we can pass the *M-V-TARGET* along to the last form.
               ;; Also, after the "end" where the failure branches branch to
               ;; we put code to push N-1 extra NILs, or whatever.
               ;; The code for the last form jumps around that, to the tag TAG1.
               (DO ((ARGL ARGL (CDR ARGL))
                    (BRANCH `(BRANCH NILIND ,SENSE nil ,TAG)))
                   ((NULL (CDR ARGL))
                    ;; Compile the last form.  If we want multiple values
                    ;; and it handles them, then say the AND is handling them.
                    (COND (*M-V-TARGET*
                           (IF (NULL (P2MV-for-k (CAR ARGL) dest *M-V-TARGET*))
                               (SETQ TAG1 (GENSYM))))
                          (UNCONDITIONAL
                           (P2BRANCH-for-k (CAR ARGL) DEST *BDEST*)
                           (SETQ *BDEST* NIL))
                          (T
                           (P2-for-k (CAR ARGL)
                                     ;;Ok to distribute down a D-RETURN, since
                                     ;; it is an implicit transfer
                                     (EQ DEST 'D-RETURN)))))
                 (P2-for-k (CAR ARGL) 'k:r0)
                 (when (SIMPLEGOP (CADR ARGL))
                   (RETURN (OUTB-for-k `(BRANCH NILIND ,(OTHER SENSE) NIL ,(GTAG (CADADR ARGL))))))
                 (OUTB-for-k (COPY-LIST BRANCH)))))
        (COND (TAG1
               ;; If we want multiple values, and the last form provides them,
               ;; say that the AND provides them,
               ;; and arrange to produce some in every other path.
               (OUTB-for-k `(BRANCH ALWAYS NIL NIL ,TAG1))      ;Last form jumps around.
               (OUTTAG TAG)                     ;Other paths come here.
               ;; We compiled the individual clauses to K:R0.  If this OR and we terminated
               ;; early, we just fetch the value from there.  If it's AND, we just use the
               ;; NIL g.r.
               (let ((target (if (eq *p2fn* 'and)
                                 (k-find-constant-register nil)
                               'k:r0)))
                 (COND ((NUMBERP *M-V-TARGET*)  ;Turn single value into N values,
                        (outi-for-k `(k:move (k-find-return-register-count) ',*m-v-target* k:boxed-right))
                        (loop with nilreg = (k-find-constant-register nil)
                              for i from 1 below *m-v-target*
                              do (outi-for-k `(k:move ,(k-find-return-register i) ,nilreg)))
                        (outi-for-k `(k:move ,dest ,target)))
                       ((EQ *M-V-TARGET* 'MULTIPLE-VALUE-LIST)  ;or into a list of values.
                        (outi-for-k `(k:call (ncons 1) ,dest
                                             (k:o0 ,target k:ch-open k:boxed-right))))
                       ((neq dest 'd-ignore)
                        (outi-for-k `(k:move ,dest ,target k:boxed-right)))))
               (SETQ *M-V-TARGET* NIL)
               (OUTTAG TAG1))                   ;Last form jumps here.
              ((NOT UNCONDITIONAL)
               (OUTTAG TAG))
              (t (outtag tag)))
        NIL))))





(DEFUN (:PROPERTY LET* P2-for-k) (ARGL DEST)
  (LET ((*VARS* (CADR ARGL)))
    (P2LET-INTERNAL-for-k *VARS*
                          (P2SBIND-for-k (CAR ARGL) (CADDR ARGL) *VARS*)
                          ARGL DEST)))

(DEFUN (:PROPERTY LET P2-for-k) (ARGL DEST)
  (LET ((*VARS* (CADR ARGL)))
    (P2LET-INTERNAL-for-k *VARS* (P2PBIND-for-k (CAR ARGL) (CADDR ARGL)) ARGL DEST)))

(DEFUN (:PROPERTY LET-FOR-LAMBDA P2-for-k) (ARGL DEST)
  (LET ((OVARS *VARS*)
        (*VARS* *VARS*)
        (NBINDS (P2PBIND-for-k (CAR ARGL) (CADDR ARGL))))
    (PROCESS-SPECIAL-DECLARATIONS (CADR ARGL))
    (P2LET-INTERNAL-for-k OVARS NBINDS ARGL DEST)))

;;; LET-HACK is generated by LET-INTERNAL in case of lexical closures and *WITHIN-CATCH*.
(DEFUN (:PROPERTY LET-HACK P2-for-k) (ARGL DEST)
  (LET ((*VARS* (CAR ARGL)))
    (P2LET-INTERNAL-for-k *VARS* (CADR ARGL) (CADDR ARGL) DEST T)))

;;; Compile the body of a LET.  The variable binding has already been done
;;; by P1PBIND or P1SBIND, which returned the number of special bindings made
;;; which is our argument NBINDS.
(DEFUN P2LET-INTERNAL-for-k (OVARS NBINDS ARGL DEST &OPTIONAL IGNORE-LEXICAL-CLOSURES)
  (IF (AND *WITHIN-CATCH*
           (NOT IGNORE-LEXICAL-CLOSURES)
           (NEQ (FIFTH ARGL) (SIXTH ARGL)))
      (P2F-for-k `(UNWIND-PROTECT
                      (LET-HACK-for-k ,OVARS ,NBINDS ,ARGL)
                    (DISCONNECT-CLOSURES-for-k ,(FIFTH ARGL) ,(SIXTH ARGL))
                    (UNSHARE-CLOSURE-VARS-for-k ,*VARS* ,OVARS))
           DEST)
    (LET* ((*VARS* (THIRD ARGL))
           (IBINDP (FOURTH ARGL))
           (ENTRY-LEXICAL-CLOSURE-COUNT (FIFTH ARGL))
           (EXIT-LEXICAL-CLOSURE-COUNT (SIXTH ARGL))
           (BDY (NTHCDR 6 ARGL))
           (IDEST 'D-PDL)
           NVALUES
           M-V-DONE
           (*PROGDESC-ENVIRONMENT* *PROGDESC-ENVIRONMENT*))
      ;; Determine the immediate destination of returns in this prog.
      (AND (MEMQ DEST '(D-IGNORE D-INDS D-RETURN))
           (NULL *M-V-TARGET*)
           (SETQ IDEST DEST))
      ;; If %BIND is used within this LET, and it's an internal LET,
      ;; we must push the specpdl index at entry so we can unbind to it later.
      ;; This is not needed for D-RETURN since function exit pops all bindings.
      (WHEN (AND IBINDP (NOT (EQ DEST 'D-RETURN)))
        (OUTI-for-k '(MISC D-PDL SPECIAL-PDL-INDEX))
        (INCPDLLVL))
      ;; Push a dummy progdesc so that GOs exiting this LET can unbind our specials.
      (PUSH (MAKE-PROGDESC :NAME '(LET)
                           :PDL-LEVEL PDLLVL
                           :NBINDS (IF IBINDP (LIST NBINDS) NBINDS))
            *PROGDESC-ENVIRONMENT*)
      (WHEN (AND (EQ *M-V-TARGET* 'THROW) IBINDP)
        (P2PUSH-CONSTANT 1)
        (OUTI '(MISC D-PDL PDL-WORD)))
      ;; How many words are we supposed to leave on the stack?
      (SETQ NVALUES
            (COND ((NUMBERP *M-V-TARGET*) *M-V-TARGET*)
                  ((EQ IDEST 'D-PDL) 1)
                  (T 0)))
      (UNLESS BDY (SETQ BDY '('NIL)))
      (DO ((TAIL BDY (CDR TAIL)))
          ((NULL (CDR TAIL))
           (UNLESS (P2MV-for-k (CAR TAIL) IDEST *M-V-TARGET*)
             (SETQ M-V-DONE T)))
        (P2-for-k (CAR TAIL) 'D-IGNORE))
      (UNLESS M-V-DONE
        (SETQ NVALUES 1))
      ;; If this is a top-level PROG, we just went to D-RETURN, so we are done.
      (UNLESS (EQ DEST 'D-RETURN)
        ;; Unbind any locals that need to be unbound.
        (WHEN (AND (NOT IGNORE-LEXICAL-CLOSURES)
                   ( ENTRY-LEXICAL-CLOSURE-COUNT EXIT-LEXICAL-CLOSURE-COUNT)
                   ;; If this code can only be executed once per function call
                   ;; and the variables we would consider unsharing
                   ;; are not overlapped with any other variables,
                   ;; then we do not need to unbind them explicitly.
                   (OR *WITHIN-POSSIBLE-LOOP*
                       (DO ((VS *VARS* (CDR VS)))
                           ((EQ VS OVARS))
                         (LET ((V (CAR VS)))
                           (WHEN (OR (MEMQ 'FEF-ARG-OVERLAPPED (VAR-MISC V))
                                     (VAR-OVERLAP-VAR V))
                             (RETURN T))))))
          (P2-for-k `(DISCONNECT-CLOSURES ,ENTRY-LEXICAL-CLOSURE-COUNT
                                          ,EXIT-LEXICAL-CLOSURE-COUNT)
                    'D-IGNORE)
          (P2-for-k `(UNSHARE-CLOSURE-VARS ,*VARS* ,OVARS) 'D-IGNORE))
        (WHEN (AND (EQ *M-V-TARGET* 'THROW) IBINDP)
          (POPPDL NVALUES 1))
        ;; Unbind any specials
        (WHEN IBINDP
          (OUTPUT-UNBIND-TO-INDEX-for-k NVALUES))
        (UNBIND IDEST NBINDS)
        ;; Dispose of our value.
        (AND (NEQ DEST IDEST)
             (NULL *M-V-TARGET*)
             (MOVE-RESULT-FROM-PDL DEST))
        ;; If we produced multiple values, say we did.
        (IF M-V-DONE (SETQ *M-V-TARGET* NIL))))))

;;; These two do not occur in code except as generated by P2PROG-INTERNAL.
;;; They are almost a kind of macro for use in pass 2.
(DEFUN (:PROPERTY DISCONNECT-CLOSURES P2-for-k) (ARGL IGNORE)
  (LET ((ENTRY-LEXICAL-CLOSURE-COUNT (CAR ARGL))
        (EXIT-LEXICAL-CLOSURE-COUNT (CADR ARGL)))
    (DO ((I (max ENTRY-LEXICAL-CLOSURE-COUNT *highest-lexical-closure-disconnected*) (1+ I)))
        ((= I EXIT-LEXICAL-CLOSURE-COUNT)
         (setq *highest-lexical-closure-disconnected* exit-lexical-closure-count))
      (OUTI-for-k `(,(IF (= I ENTRY-LEXICAL-CLOSURE-COUNT)
                         'CLOSURE-DISCONNECT-FIRST
                       'CLOSURE-DISCONNECT)
                    ,(+ (LENGTH *LOCAL-MAP*) I))))))

(DEFUN (:PROPERTY UNSHARE-CLOSURE-VARS P2-for-k) (ARGL IGNORE)
  (LET ((*VARS* (CAR ARGL)) (OVARS (CADR ARGL)))
    (DO ((VS *VARS* (CDR VS)))
        ((EQ VS OVARS))
      (LET ((V (CAR VS)))
        (WHEN (MEMQ 'FEF-ARG-USED-IN-LEXICAL-CLOSURES (VAR-MISC V))
          (OUTI-for-k `(CLOSURE-UNSHARE
                         ,(FIND-POSITION-IN-LIST V *VARIABLES-USED-IN-LEXICAL-CLOSURES*))))))))

;;;; Compile a BLOCK.

;;; A BLOCK has no user-defined GOTAGS, but it does have one tag at this level: its rettag.
(DEFUN (:PROPERTY BLOCK P2-for-k) (ARGL DEST)
  (P2BLOCK-for-k ARGL DEST NIL NIL))

(DEFUN (:PROPERTY BLOCK-FOR-PROG P2-for-k) (ARGL DEST)
  (P2BLOCK-for-k ARGL DEST T NIL))

;d-inds-loses is a weird thing associated with stack-lists, flush.
(DEFUN P2BLOCK-for-k (ARGL DEST &OPTIONAL ALSO-BLOCK-NAMED-NIL D-INDS-LOSES)
  (LET* ((OLDGOTAGS *GOTAG-ENVIRONMENT*)
         (*GOTAG-ENVIRONMENT* (CAR ARGL)) (MYPROGDESC (CADR ARGL)) (BDY (CDDR ARGL))
         (*PROGDESC-ENVIRONMENT* *progdesc-environment*)
         (PROGNAME (PROGDESC-NAME MYPROGDESC))
         (RETTAG (PROGDESC-RETTAG MYPROGDESC))
         (IDEST 'D-PDL)
         NVALUES)
    ;; Determine the immediate destination of returns in this prog.
    (AND (MEMQ DEST '(D-IGNORE D-INDS D-RETURN))
         (NOT (AND (EQ DEST 'D-INDS) D-INDS-LOSES))
         (NULL *M-V-TARGET*)
         (SETQ IDEST DEST))
    ;; Add this block to the stack of entered ones.
    (SETF (PROGDESC-IDEST MYPROGDESC) IDEST)
    (SETF (PROGDESC-M-V-TARGET MYPROGDESC) *M-V-TARGET*)
    (SETF (PROGDESC-PDL-LEVEL MYPROGDESC) PDLLVL)
    (SETF (PROGDESC-NBINDS MYPROGDESC) 0)
    (PUSH MYPROGDESC *PROGDESC-ENVIRONMENT*)
    ;; For PROG, add a block named NIL also.
    (WHEN (AND ALSO-BLOCK-NAMED-NIL (NEQ PROGNAME 'T))
      (PUSH (COPY-PROGDESC MYPROGDESC) *PROGDESC-ENVIRONMENT*)
      (SETF (PROGDESC-NAME (CAR *PROGDESC-ENVIRONMENT*)) 'NIL))
    ;; How many words are we supposed to leave on the stack?
    (SETQ NVALUES
          (COND ((NUMBERP *M-V-TARGET*) *M-V-TARGET*)
                ((EQ IDEST 'D-PDL) 1)
                (T 0)))
    ;; Set the GOTAG-PDL-LEVEL of the rettag.
    ;; *GOTAG-ENVIRONMENT* at this moment contains the RETTAG and nothing else.
    (SETF (GOTAG-PROGDESC (CAR *GOTAG-ENVIRONMENT*))
          (CAR *PROGDESC-ENVIRONMENT*))
    (SETF (GOTAG-PDL-LEVEL (CAR *GOTAG-ENVIRONMENT*))
          (+ PDLLVL NVALUES))
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
      (MKPDLLVL (GOTAG-PDL-LEVEL (CAR *GOTAG-ENVIRONMENT*)))
      (OUTTAG RETTAG)
      ;; Store away the value if
      ;; it is not supposed to be left on the stack.
      (AND (NEQ DEST IDEST)
           (NULL *M-V-TARGET*)
           (MOVE-RESULT-FROM-PDL DEST))
      ;; If we were supposed to produce multiple values, we did.
      (SETQ *M-V-TARGET* NIL))))

;;;; Various types of RETURN.

;(DEFUN (:PROPERTY RETURN-LIST P2-for-k) (ARGL IGNORE)
;  (P2RETURN1-for-k `((VALUES-LIST ,(CAR ARGL))) NIL))

(DEFUN (:PROPERTY RETURN-FROM P2-for-k) (ARGL IGNORE)
  (P2RETURN1-for-k (CDR ARGL) (CAR ARGL)))

;;; (RETURN-FROM-T <value>) is like (RETURN-FROM T <value>).
(DEFUN (:PROPERTY RETURN-FROM-T P2-for-k) (ARGL IGNORE)
  (P2RETURN1-for-k ARGL T))

(DEFUN P2RETURN1-for-k (ARGL PROGNAME)
  (LET ((RPDESC (ASSQ PROGNAME *PROGDESC-ENVIRONMENT*))
        IPROGDEST
        MVTARGET
        ARG
        LOSE
        NVALUES)
    (OR RPDESC
      (FERROR "Internal compiler error: BLOCK environments randomized."))
    (COND ((= (LENGTH ARGL) 1)
           (SETQ ARG (CAR ARGL)))
          (T (SETQ ARG `(VALUES . ,ARGL))))
    (SETQ IPROGDEST (PROGDESC-IDEST RPDESC))
    (SETQ MVTARGET (PROGDESC-M-V-TARGET RPDESC))
    ;; If going to throw values, things expect tag on top of stack.  So copy it to there.
    (WHEN (EQ MVTARGET 'THROW)
      (UNLESS (= PDLLVL (PROGDESC-PDL-LEVEL RPDESC))
        (P2PUSH-CONSTANT (- PDLLVL (PROGDESC-PDL-LEVEL RPDESC)))
        (OUTI-for-k '(MISC D-PDL PDL-WORD))
        (INCPDLLVL)))
    ;; Compile the arg with same destination and *m-v-target*
    ;; that the PROG we are returning from had.
    (SETQ LOSE (P2MV-for-k ARG IPROGDEST MVTARGET))
    ;; But, since a PROG has multiple returns, we can't simply
    ;; pass on to the PROG's caller whether this function did or did not
    ;; generate those multiple values if desired.
    ;; If the function failed to, we just have to compensate here.
    (AND LOSE
         (COND ((NUMBERP MVTARGET)
                ;; If we wanted N things on the stack, we have only 1, so push N-1 NILs.
                (DO ((I 1 (1+ I))) ((= I MVTARGET))
                  (OUTI-for-k '(MOVE D-PDL (QUOTE-VECTOR 'NIL)))))
               ((EQ MVTARGET 'MULTIPLE-VALUE-LIST)
                (OUTI-FOR-K '(MISC D-PDL NCONS)))))
    (SETQ NVALUES (COND ((NUMBERP MVTARGET) MVTARGET)
                        ((EQ IPROGDEST 'D-PDL) 1)
                        (T 0)))
    ;; Note how many things we have pushed.
    (AND (EQ IPROGDEST 'D-PDL)
         (MKPDLLVL (+ PDLLVL NVALUES)))
    ;; Jump to the prog's rettag, unless the prog is top-level (to d-return)
    ;; since in that case the code just compiled will not ever drop through.
    (OR (EQ IPROGDEST 'D-RETURN)
        (OUTBRET-for-k (PROGDESC-RETTAG RPDESC) RPDESC NVALUES))))

(DEFUN (:PROPERTY TAGBODY P2-for-k) (ARGL PROGDEST)
  (LET* ((MYGOTAGS (CAR ARGL))
         (*GOTAG-ENVIRONMENT* *GOTAG-ENVIRONMENT*)
         (*WITHIN-POSSIBLE-LOOP* *WITHIN-POSSIBLE-LOOP*)
         (BODY (CDR ARGL))
         (MYPROGDESC (GOTAG-PROGDESC (CAR MYGOTAGS)))
         (*PROGDESC-ENVIRONMENT* *PROGDESC-ENVIRONMENT*))
    ;; Remember this TAGBODY's general environment.
    ;; We supply as the supposed block name
    ;; a list that will not appear as the block name in any RETURN-FROM.
    ;; So we can have an entry on the *PROGDESC-ENVIRONMENT* list to record our tags' pdllvl
    ;; without interfering with RETURN-FROM.
    (WHEN MYGOTAGS
      (SETF (PROGDESC-PDL-LEVEL MYPROGDESC) PDLLVL)
      (PUSH MYPROGDESC *PROGDESC-ENVIRONMENT*)
      ;; Set the GOTAG-PDL-LEVEL of each of the tags.
      (DOLIST (GOTAG MYGOTAGS)
        (SETF (GOTAG-PDL-LEVEL GOTAG) PDLLVL))
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
         (OUTBRET-for-k (CAR ARGL) NIL 0))
        (T
         (WARN 'BAD-PROG ':IMPOSSIBLE
               "The argument of ~S was ~S, not a symbol."
               'GO (CAR ARGL)))))

(DEFUN (:PROPERTY GO-HACK P2-for-k) (ARGL IGNORE)
  (OUTB-for-k `(BRANCH ALWAYS NIL NIL ,(GOTAG-LAP-TAG (CAR ARGL)))))

;;; Output an unconditional transfer to the specified prog tag,
;;; popping the pdl the appropriate number of times to adjust the
;;; pdl from its current level to the level required at that tag.

;;; For handling GO, PROGDESC should be NIL and NVALUES should be 0.
;;; When jumping to the return tag of a prog, PROGDESC should be
;;; the desc for the prog we are returning from, and NVALUES should be
;;; the number of things on the top of the stack which are being left
;;; there as values to return from the prog.
(DEFUN OUTBRET-for-k (TAG PROGDESC NVALUES &AUX TEM (EXITPROGDESC PROGDESC))
  (SETQ TEM (GOTAGS-SEARCH TAG))
  (IF (NOT TEM)
      NIL
    (let ((*open-frames* *open-frames*))
      ;; If this is GO, set EXITPROGDESC to the progdesc of its containing PROG
      (OR PROGDESC (SETQ EXITPROGDESC (GOTAG-PROGDESC TEM)))
      (POP-FRAMES-for-k EXITPROGDESC NVALUES)
      ;; For a prog rettag, the pdl level should include
      ;; the number of values desired on the stack.
      (POPPDL NVALUES (- PDLLVL (GOTAG-PDL-LEVEL TEM)))
      (OUTB-for-k `(BRANCH ALWAYS NIL NIL ,(GOTAG-LAP-TAG TEM))))))

(DEFUN POP-FRAMES-for-k (EXITPROGDESC NVALUES)
  ;; If we are exiting any PROGs, unwind stacks to their levels.
  ;; Does not include the prog whose desc is EXITPROGDESC.
  (LET ((N-UNBINDS 0)
        (LAST-VARIABLE-UNBIND-PDL-LEVEL)                           ;Level of lowest
        (clean-up-open-frames
    (DO ((L *PROGDESC-ENVIRONMENT* (CDR L)))
        ((EQ (CAR L) EXITPROGDESC))
      (COND ((CONSP (PROGDESC-NBINDS (CAR L)))
             (SETQ N-UNBINDS (CAR (PROGDESC-NBINDS (CAR L))))
             (SETQ LAST-VARIABLE-UNBIND-PDL-LEVEL (PROGDESC-PDL-LEVEL (CAR L))))
            (T (SETQ N-UNBINDS (+ N-UNBINDS (PROGDESC-NBINDS (CAR L)))))))
    ;; LAST-VARIABLE-UNBIND-PDL-LEVEL is the level at start of PROG body,
    ;; and does not include the values we want to return.
    ;; PDLLVL at all times includes those values
    ;; since they are already on the stack.
    (COND (LAST-VARIABLE-UNBIND-PDL-LEVEL
           (POPPDL-for-k NVALUES (- PDLLVL NVALUES LAST-VARIABLE-UNBIND-PDL-LEVEL))
           (OUTPUT-UNBIND-TO-INDEX-for-k NVALUES)))
    (UNBIND 'D-IGNORE N-UNBINDS)))

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
        (T (COND ((EQ (CADR X) 'ALWAYS)
                  (SETQ *DROPTHRU* NIL)))
           (PUTPROP (CAR (LAST X)) T 'USED)
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
           (outf-for-k `(k:test ,inst))
           (outf-for-k `(k:branch ,(car (last branch))))
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
         (movem-and-move-to-dest-for-k value var dest)
;        (P2PUSH VALUE)
;        (MOVEM-AND-MOVE-TO-DEST VAR DEST)
         )
;       ((SI:MEMBER-EQUAL VALUE '('0 'NIL))
;        (OUTI `(,(CDR (ASSQ (CADR VALUE)
;                            '((0 . SETZERO) (NIL . SETNIL))))
;                0
;                ,(P2-SOURCE VAR 'D-PDL)))
;        (OR (MEMQ DEST '(D-IGNORE D-INDS))
;            (P2-for-k VALUE DEST)))
;       ((AND (NOT (ATOM VALUE))
;             (CDR VALUE)
;             (EQUAL (CADR VALUE) VAR)
;             (MEMQ (CAR VALUE) '(CDR CDDR 1+ 1-))
;             (MEMQ DEST '(D-IGNORE D-INDS)))
;        (OUTI `(SETE ,(CAR VALUE) ,(P2-SOURCE VAR 'D-PDL))))
        (T
         (movem-and-move-to-dest-for-k value var dest)
;        (P2PUSH VALUE)
;        (MOVEM-AND-MOVE-TO-DEST-for-k VAR DEST)
         ))
  NIL)

(defun movem-and-move-to-dest-for-k (value var dest)
  (let* ((inter-var (cond ((atom var) 'k:r0)    ;special, use r0 if necessary.
                          ((eq (car var) 'local-ref)
                           (let ((lap-address (var-lap-address (cadr var))))
                             (cond ((eq (car lap-address) 'arg)
                                    (a-n (cadr lap-address)))
                                   ((eq (car lap-address) 'local)
                                    'k:r0)      ;stack-slot, use r0 if necessary.
                                   (t (ferror nil "bad lap address")))))
                          ((eq (car var) 'lexical-ref)
                           (fsignal "lexical-ref")
                           'k:r0
                           )
                          (t (ferror nil "var = ~s?" var))))
         (inter-dest (cond ((eq dest 'd-ignore) nil)
                           ((eq dest 'd-return)
                            'k:a0)
                           ((and (listp dest)
                                 (memq (car dest) '(k:new-open k:new-tail-open)))
                            'k:r0)
                           ((and (symbolp dest)
                                 (string-equal (si:package-primary-name (symbol-package dest))
                                               "K"))
                            dest)
                           (t (ferror nil "dest = ~s?" dest))))
         (inter-reg (cond ((or (null inter-dest)
                               (eq inter-var inter-dest))
                           inter-var)
                          ((a-register-p inter-dest)
                           inter-dest)
                          (t inter-var))))
    (p2-for-k value inter-reg)
    ;salt it in var, if necessary.
    (cond ((atom var)
      ;store from inter-reg into special variable
           (outi-for-k `(k:movei k:o0 (quote ,var) k:boxed k:ch-open))
           (outi-for-k `(k:move k:o1 ,inter-reg k:boxed-right))
           (return-from movem-and-move-to-dest-for-k
             (outi-for-k `(k:call (symbol:%%set 2) ,dest)))
           ;calling subroutine anyway, and %%set returns as value the right thing.
           ;so putting dest in subroutine call solves whole problem and avoids any
           ;possibility of K:R0 bashage.
           )
          ((eq (car var) 'local-ref)
           (let ((lap-address (var-lap-address (cadr var))))
             (cond ((eq (car lap-address) 'arg))        ;if arg, its compiled directly there
                   ((eq (car lap-address) 'local)
                    (write-stack-slot inter-reg (cadr lap-address))
                    )
                   ))))

    ;salt it in dest, if necessary.
    ))

;;; Move the quantity on the top of the stack to the value of a variable
;;; and also move it to the specified destination.
;(DEFUN MOVEM-AND-MOVE-TO-DEST-for-k (VAR DEST)
;  (COND ((ATOM VAR)
;        (IF (MEMQ DEST '(D-IGNORE D-INDS))
;            (OUTI `(POP 0 (SPECIAL ,VAR)))
;          (OUTI `(MOVEM 0 (SPECIAL ,VAR)))
;          (MOVE-RESULT-FROM-PDL DEST)))
;       ((EQ (CAR VAR) 'LOCAL-REF)
;        (IF (MEMQ DEST '(D-IGNORE D-INDS))
;            (OUTI `(POP 0 ,(VAR-LAP-ADDRESS (CADR VAR))))
;          (OUTI `(MOVEM 0 ,(VAR-LAP-ADDRESS (CADR VAR))))
;          (MOVE-RESULT-FROM-PDL DEST)))
;       ((EQ (CAR VAR) 'SELF-REF)
;        (IF (MEMQ DEST '(D-IGNORE D-INDS))
;            (OUTI `(POP 0 (QUOTE-VECTOR ,VAR)))
;          (OUTI `(MOVEM 0 (QUOTE-VECTOR ,VAR)))
;          (MOVE-RESULT-FROM-PDL DEST)))
;       ((EQ (CAR VAR) 'LEXICAL-REF)
;        (P2PUSH-CONSTANT (CADR VAR))
;        (NEEDPDL 1)
;        (OUTI `(MISC ,DEST %STORE-IN-HIGHER-CONTEXT)))))



;;; Unbind NBINDS special variables, unless IDEST is D-RETURN.
(DEFUN UNBIND-for-k (IDEST NBINDS)
  (OR (EQ IDEST 'D-RETURN)                      ;returning unbinds for us
      (DO ((N 16. (+ N 16.)))
          ;; N is number of unbinds we would have done if we now
          ;; unbind another 16.  N-16 is number unbound so far.
          ;; Note that an UNBIND X instruction unbinds X+1 vars.
          ((> N NBINDS)
           (OR (= NBINDS (- N 16.))
               (OUTI-for-k `(MISC D-IGNORE UNBIND ,(- NBINDS (- N 16.) 1)))))
        (OUTI-for-k '(MISC D-IGNORE UNBIND 15.)))))

(DEFUN OUTTAG-for-k (X branch-back-possible)
  (WHEN (or branch-back-possible (GET X 'USED))
    (OR *DROPTHRU* (OUTF '(NO-DROP-THROUGH)))
    (SETQ *DROPTHRU* T)
    (OUTF-for-k X)))


(defun clean-up-open-frames (level)
  (unless (or (null level)
              (loop for l on *open-frames*
                    thereis (eq l level)))
    (reg-error "Internal compiler error:  over-popped the open frames."))
  (loop for l on *open-frames*
        for f = (car l)
        until (eq l level)
        do (outi-for-k (append f (copy-list '(k:r0)))))
  (unless (eql level *open-frames*)
    (reg-error "Internal compiler error:  failed to pop the right number of open frames.")))


(defun check-not-open-and-return (x)
  (case (caddr x)
    ((d-return)
     (error "Internal compiler error -- simultaneously opening a frame and returning.")))  )

(DEFUN OUTI-for-k (X)
 ;every instruction emitted for K goes thru here.
 ;also do some fixups on the instructions:
 ;  on dest D-RETURN, insert appropriate exit sequence.
 ;  on dest D-INDS or D-IGNORE, change it to k:r0
 ;  on dest (k:new-open <n>), leave it in a call.
 ;                            refrob it for k:move or k:movei
 ;  on dest for MOVE or MOVEI <other list>, pass it thru (typically VMA, MD etc)
  (IF (NOT *DROPTHRU*)
      NIL
    (cl:case (car x)
      ((k:open)
       (check-not-open-and-return x)
       (note-open-frame nil '(li::discard-open-frame 0))
       (outf-for-k x))
      ((k:open-tail)
       (check-not-open-and-return x)
       ;; Not necessarily PROG1; this may just be a RETURN from inside a tail-called function.
       (note-open-frame t '(li::prog1-internal 1)))
      ((k:call k:open-call)     ;take destination in 3rd element
       (unless (eq (car x) 'k:open-call)
         (finish-open-frame nil))
       (cond ((eq (caddr x) 'd-return)
                                                ;well, we evidently can't do a TAIL-CALL.
              (clean-up-open-frames nil)                ;Exiting, clean up everything.
              (outf-for-k `(,(car x) ,(cadr x) K:a0 . ,(cdddr x)))
              (output-full-exit-sequence t)     ;do return-tail on K:A0
              )
             ((memq (caddr x) '(d-inds d-ignore))
              (outi-for-k `(,(car x) ,(cadr x) K:r0 . ,(cdddr x))))
             (t (outf-for-k x))))
      ((k:move k:movei)
       (cond ((eq (cadr x) 'd-return)
              ;; We're about to return; better give back all of our open frames.
              ;; *** But first, if there's a TAIL-CALL, this has to move the return
              ;; *** value into O0 just before the final TAIL-CALL, and turn it into
              ;; *** a PROG1-INTERNAL instead of a DISCARD-OPEN-FRAME.  This needs more
              ;; *** careful design.  --RWK
              (clean-up-open-frames nil)
              (outf-for-k `(,(car x) K:return ,(caddr x) k:ch-return k:next-pc-return . ,(cdddr x)))
              (cond ((not (and (zerop *entry-sequence-specbinds*)
                               (zerop *stack-slots*)))
                     (ferror nil "stack needs adjusting")))     ;should have used K:CALL.
              (setq *dropthru* nil))
             ((memq (cadr x) '(d-inds d-ignore))
              ;; We don't count the open frame this time around; we wait for the recursive
              ;; call.  --RWK
              (outi-for-k `(,(car x) K:r0 . ,(cddr x))))
             ((and (listp (cadr x))
                   (memq (car (cadr x)) '(k:new-open k:new-tail-open)))
              ;; We don't count the open frame this time around; we wait for the recursive
              ;; call.  --RWK
              (ecase (car (second x))
                ((k:new-open)
                 (when (memql '(k:ch-open k:ch-tail-open) (cdddr x))
                   (ferror nil "Already an OPEN in ~s" x))
                 (outi-for-k `(,(car x) ,(o-n (cadr  (cadr x))) ,(caddr x) k:ch-open . ,(cdddr x))))
                ((k:new-tail-open)
                 (when (memql '(k:ch-open k:ch-tail-open) (cdddr x))
                   (ferror nil "Already an OPEN in ~s" x))
                 (outi-for-k `(,(car x) ,(o-n (cadr (cadr x)))  ,(caddr x) k:ch-tail-open . ,(cdddr x))))))
             (t
              (when (memq 'k:ch-tail-open (cdddr x))
                ;; If we're going to tail call, we'd better be sure we haven't left any
                ;; open frames around!  We can't just clean up.  For now, let's just error.
                ;; Later, we need a way to force ineligible things to not be tail-called.
                ;; Any non-discardable frames (CATCH or PROG1 or SPECBIND) would be reason to force normal
                ;; calling.
                (when *open-frames*
                  (error "Internal compiler error -- We're TAIL calling with stuff hanging around."))
                #+ignore
                (clean-up-open-frames nil))
              (when (memql '(k:ch-open k:ch-tail-open) (cdddr x))
                (note-open-frame (memq 'k:ch-tail-open (cdddr x))))
              (outf-for-k x))))
      ((k:tail-call)
       (unless (= (length *open-frames*) 1)
         (reg-error "Tail-calling, but there are un-disposed-of open frames."))
       (finish-open-frame t)
       (outf-for-k x))
      ((k:nop)
       (when (memql '(k:ch-open k:ch-tail-open) (cdr x))
         (note-open-frame (memq 'k:ch-tail-open (cdr x))))
       (outf-for-k x))
      (otherwise
       (ferror nil "unknown instruction")))))

(defun output-full-exit-sequence (return-tail-p)
  (case *entry-sequence-specbinds*
    (0)
    (1 (outi-for-k `(k:open-call (li:unbind-1 0) k:ignore nil)))
    (t (let ((reg-ref (k-find-constant-register *entry-sequence-specbinds*)))
         (cond (reg-ref
                (outi-for-k `(k:open-call (li:unbind 1) k:ignore (k:o0 ,reg-ref k:boxed-right))))
               (t (outi-for-k `(k:movei k:o0 (quote ,*entry-sequence-specbinds*) k:boxed k:ch-open))
                  (outi-for-k `(k:call (li:unbind 1) k:ignore nil)))))))
  (cond ((zerop *stack-slots*)                  ;flush *stack-slots*, if any.
         (outi-for-k '(k:nop)))
        (t (generate-decr 'gr:*stack-pointer* 'gr:*stack-pointer* *stack-slots*)))
  (outi-for-k `(k:move ,(if return-tail-p 'k:return-tail 'k:return)
                       k:a0 k:boxed-right k:ch-return k:next-pc-return)))

;    (IF (AND (EQ (CADR X) 'D-RETURN)
;            (NOT (EQ (CAR X) 'CALL)))
;       (SETQ *DROPTHRU* NIL))
;    (IF (AND (EQ (CAR X) 'MISC)
;            ( (GET (THIRD X) 'QLVAL) #o1000))
;       (SETQ X (CONS 'MISC1 (CDR X))))
;    (IF (MEMQ (CAR X) '(MISC MISC1))
;       (OUTF X)
;      (OUTS X))


(DEFUN OUTI1-for-k (X)                          ;Use this for outputing instructions
  (IF *DROPTHRU* (OUTS X)))                     ;known to take delayed transfers

(defun outf-for-k (x)
  (outf x))

;;; Bind a list of variables "in parallel":  compute all values, then bind them all.
;;; Return the number of special bindings made.
;;; Note: an attempt to bind NIL is ignored at this level.
;;; This could have been done as a P1 rewrite to turn all the specials bindings into
;;; explicit calls to BIND, as in.
;;; (LET ((A 1) (*B* 2) (*C* 3) (D 4)) (BODY A D)) ==>
;;; (LET (A D)
;;;   (BIND '*B* (progn (BIND '*C* (progn (setq a 1 d 4) 3) 2)))
;;;   (BODY A D))
;;; (except that the init-forms for the variables would be done outside the
;;;
(defun p2pbind-for-k (varnames newvars)
  (loop with nbinds = 0                         ;Number of specbinds performed.
        with pdllvl = pdllvl
        with intcode
        for varspec in varnames
        for varname = (if (atom varspec) varspec
                        (first varspec))
        for initform = (unless (atom varspec)
                         (second varspec))
        for home = (assq varname newvars)
        do
    (when (equal initform '(%pop))
      (error "%POP can't be used on the Falcon."))
    (if (null varname)
        (p2-for-k initform 'd-ignore)           ;Binding nil, just perform the side-effects.
      (unless home
        (barf varname 'not-on-vars 'barf))
      (setq intcode (var-init home))
      ;; If this variable's binding is fully taken care of by function entry,
      ;; we shouldn't have gotten it here.  (The LAMBDA compiler's version of this
      ;; does have to handle them.)
      (case (var-kind home)
        (fef-arg-internal-aux)                  ;The good case
        (otherwise (error "P2PBIND-for-K got an argument variable.")))
      ;; Detect and handle internal special bound variables.
      (case (var-type home)
        ((fef-special fef-remote)               ;FEF-REMOTE??
         ;; Start a call to the BIND function.  We'll actually finish it off after we get everything else
         ;; done, so all the INITFORMS are done in the right dynamic environment.
         (incf nbinds)
         (outi-for-k `(k:movei k:o0 (quote ,varname) k:ch-open k:boxed-right))
         (p2-for-k initform 'k:o1))
        (otherwise
         (unless (EQUAL INITFORM '(UNDEFINED-VALUE))    ;Er, What is this?  Can you dance to it?  --RWK
           (p2-for-k initform (k-dest-from-lap-address (var-lap-address home)))))))
    finally
    (dotimes (i nbinds)
      (outi-for-k '(k:call (bind 2) ignore)))
    (return nbinds)))

(DEFUN (:PROPERTY PROGN-WITH-DECLARATIONS P2-for-k) (ARGL DEST)
  (LET ((*VARS* (CAR ARGL)))
    (P2PROG12N-for-k (LENGTH (CDR ARGL)) DEST (CDR ARGL))))

(defun (:property prog2 p2-for-k) (argl dest)
  (p2prog12n-for-k 2 dest argl))

(defun p2prog12n-for-k (n dest argl)
  (decf n)                              ;Convert to origin 0.
  ;; Compile the args before the one whose value we want.
  (dotimes (i n)
    (p2-for-k (or (pop argl) ''nil) 'd-ignore))
  ;; Compile the arg whose value we want.
  ;; If it's the last arg (this is PROGN),
  ;; make sure to pass along any multiple value target that the PROGN has,
  ;; and to report back how many args were actually pushed.
  (cond ((and (null (cdr argl)) *m-v-target*)
         (p2mv-for-k (or (car argl) ''nil) dest *m-v-target*)
         (setq *m-v-target* nil))
        ((and (null (cdr argl)) *bdest*)
         (p2branch-for-k (or (car argl) ''nil) dest *bdest*)
         (setq *bdest* nil))
        (t ;; Stash the value to be returned in an OPEN frame.
         (outi-for-k `(k:open))
         (p2-for-k (or (car argl) ''nil) 'k:o0)))
  (unless (cdr argl) (return-from p2prog12n-for-k nil))
  (dolist (arg (cdr argl))
    (p2-for-k arg 'd-ignore))
  ;; Clean up our open frame, and get the value to its destination.
  (outi-for-k `(k:call (k-li::prog1-internal 1) ,dest)))
