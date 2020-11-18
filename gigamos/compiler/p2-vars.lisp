;;; -*- Mode:LISP; Package:COMPILER; Base:8; Readtable:ZL -*-

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
  (when (or (member ':pre nc:*debug-flag*)
            (member ':post nc:*debug-flag*)
            (member ':postx nc:*debug-flag*))
    (format t "~%Total-slots ~D, n-specbinds ~D, nreqargs ~D, noptargs ~D, nauxvars ~D, rest-varstruct ~S, slots-on-stack ~S"
            total-slots n-specbinds nreqargs noptargs nauxvars rest-varstruct slots-on-stack-p))
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

(DEFUN P2SBIND-FOR-K (X NEWVARS *VARS* dest)
  (LET ((NNEWVARS (LOOP FOR L ON NEWVARS UNTIL (EQ L *VARS*) COUNT T))
        frame)                                  ;Unbind frame iff any specials.
    (DO ((X X (CDR X)) (HOME))
        ((NULL X))
      (SETQ HOME (NTH (1- NNEWVARS) NEWVARS))
      (WHEN (P2LMB-FOR-K (CAR X) HOME)          ;Returns T iff special
        (if frame
            (funcall (open-frame-cleanup-generator frame) nil :another-special nil)
          (progn (setq frame (make-unbind-open-frame 1 dest))
                 (add-frame frame))))
      ;; Set *VARS* to the tail of NEWVARS starting at the variable we just handled
      ;; or its optional-specified-flag.
      (SETQ NNEWVARS (1- NNEWVARS))
      (AND (CDDR (VAR-INIT HOME)) (SETQ NNEWVARS (1- NNEWVARS)))
      (SETQ *VARS* (NTHCDR NNEWVARS NEWVARS)))
    (OR (ZEROP NNEWVARS) (BARF X "VARS screwed up by this binding" 'BARF))
    frame))

;;; Bind a list of variables "in parallel":  compute all values, then bind them all.
;;; Returns a binding frame if any special binds were done.
;;; Note: an attempt to bind NIL is ignored at this level.
;;; This could have been done as a P1 rewrite to turn all the specials bindings into
;;; explicit calls to BIND, as in.
;;; (LET ((A 1) (*B* 2) (*C* 3) (D 4)) (BODY A D)) ==>
;;; (LET (A D)
;;;   (BIND '*B* (progn (BIND '*C* (progn (setq a 1 d 4) 3) 2)))
;;;   (BODY A D))
;;; (except that the init-forms for the variables would be done outside the ..)

(defun p2pbind-for-k (varnames newvars dest)
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
             (otherwise (error "Internal compiler error: P2PBIND-for-K got an argument variable.")))
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
    (return (unless (zerop nbinds)
              (let ((frame (make-unbind-open-frame nbinds dest)))
                (add-frame frame)
                frame)))))

(defun make-unbind-open-frame (nbinds idest)
  (labels ((do-unbinds (open-frame operation dest &optional (source dest) source-type)
                       (declare (ignore open-frame source-type))
                       (ecase operation
                         (:exist (fsignal "Internal compiler error: trying to :EXIST an unbind-open-frame"))
                         (:another-special (incf nbinds))
                         ((nil :return :discard) (finish-open-frame nil)
                                                 (output-specpdl-pop nbinds dest)
                                                 (values dest source)))))
    (make-open-frame :open-instruction 'bind
                     :cleanup-generator #'do-unbinds
                     :idest idest)))

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
     (write-stack-slot intermediate-dest (second lap-address)))
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
            ;; ??? It is unclear why SELF-REF should be here. -smh
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
                     ;; Since we're moving it to the heap, de-overlap it.
                     (setf (var-overlap-var v) nil)
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
        (list* flag property (var-plist var-struct))))



;; Note: variable needs to be moved to addressable space.
(defun (:property variable-location p2-for-k) (argl dest)
  (labels ((do-lexical (lexical-spec)
            (let ((tail-p (tail-call-p dest 'li:closure-location)))
              (tail-call-open '(li:closure-location 2) tail-p #'discard-temporary-frame :single-valiue)
              (outi-for-k `(k:movei k:o0 ',lexical-spec k:boxed))
              (outi-for-k '(k:move k:o1 k:a15 k:boxed-right))
              (maybe-tail-call '(li:closure-location 2) dest :single-value tail-p))))
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
                          :single-value))
             (lexical (do-lexical (second lap-address))))))
        (self-ref
         (let ((tail-p (tail-call-p dest 'li:instance-location)))
           (tail-call-open '(li:instance-location 3) tail-p #'discard-temporary-frame :single-value)
           (outi-for-k `(k:movei k:o0 ',(second var-ref) k:boxed))
           (outi-for-k '(k:move k:o1 k:a14 k:boxed-right))
           (outi-for-k '(k:move k:o2 k:a0 k:boxed-right))
           (maybe-tail-call '(li:instance-location 3) dest :single-value tail-p)))
        (lexical-ref
         (do-lexical (second var-ref)))))))


(defun (:property lexical-closure p2-for-k) (argl dest)
  (let ((tail-p (tail-call-p dest 'lexical-closure)))
    (tail-call-open 'li:make-closure-with-env tail-p #'discard-temporary-frame
                    :single-value)
    (p2-for-k (first argl) 'k:o0)
    (outi-for-k `(k:move k:o1 k:a15 k:boxed-right))
    (maybe-tail-call '(li:make-closure-with-env 2) dest :single-value tail-p)))


;; This compiles calls to the internal runtime instance var accessor and setter routines.
(defun iv-ref-for-k (self-var runtime-fnc dest &optional (new-value nil new-value-p))
  (let ((nargs (if new-value-p 4 3))
        (tail-p (tail-call-p dest runtime-fnc)))
     (tail-call-open `(,runtime-fnc ,nargs) tail-p #'discard-temporary-frame :single-value)
    (outi-for-k                                                 ;index into mapping table
      `(k:movei-load-time k:o0 (SI:FLAVOR-VAR-SELF-REF-INDEX ',(cdr self-var)) k:boxed))
    (outi-for-k '(k:move k:o1 k:a14 k:boxed-right))             ;self
    (outi-for-k '(k:move k:o2 k:a0 k:boxed-right))              ;self-mapping-table
    (when new-value-p                                           ;If a setf operation,
      (p2-for-k new-value 'k:o3))                               ; compile new-value.
              (maybe-tail-call `(,runtime-fnc ,nargs) dest :single-value tail-p)))



(DEFUN (:PROPERTY LET* P2-for-k) (ARGL DEST)
  (LET ((*VARS* (CADR ARGL)))
    (opening-frames (dest (P2SBIND-for-k (CAR ARGL) (CADDR ARGL) *VARS* dest))
      (P2LET-INTERNAL-for-k *VARS* ARGL DEST))))

(DEFUN (:PROPERTY LET P2-for-k) (ARGL DEST)
  (LET ((*VARS* (CADR ARGL)))
    (opening-frames (dest (P2PBIND-for-k (CAR ARGL) (CADDR ARGL) dest))
      (P2LET-INTERNAL-for-k *VARS* ARGL DEST))))

(DEFUN (:PROPERTY LET-FOR-LAMBDA P2-for-k) (ARGL DEST)
  (LET ((OVARS *VARS*)
        (*VARS* *VARS*))
    (opening-frames (dest (P2PBIND-for-k (CAR ARGL) (CADDR ARGL) dest))
      (PROCESS-SPECIAL-DECLARATIONS (CADR ARGL))
      (P2LET-INTERNAL-for-k OVARS ARGL DEST))))

;;; Compile the body of a LET.  The variable binding has already been done
;;; by P1PBIND or P1SBIND, which substituted a new destination which will unbind
;;; the special bindings made.
(DEFUN P2LET-INTERNAL-for-k (OVARS ARGL DEST)
  (declare (ignore ovars))
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



(defun clean-up-open-frames (level type k-dest)
  (when (eq k-dest 'd-return)
    ;;-- no more k-dest should never be an internal dest. --
    ;; This should only happen when called from OPENING-FRAMES, when the dest was D-RETURN.
    ;; In that case, the code that was output should have cleaned up the stack, no?
    (error "Internal error cleaning up open frame."))
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
         (error "Internal compiler error -- simultaneously opening a frame and returning."))))



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
