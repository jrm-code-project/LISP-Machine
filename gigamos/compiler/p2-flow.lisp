;;; -*- Mode:LISP; Package:COMPILER; Base:8; Readtable:ZL -*-


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
               (with-frames                                     ;added 2sep88 -smh
                 (cond ((constantp pred)                        ;Should be constant-in-compiler !! That's more inclusive. -smh
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
                          (outi-for-k `(k:move ,dest k:r0 k:boxed-right) :single-value))))
               (go L5))
              ;; Clause of one element, if value is not wanted.
              ((= 1 CLAUSE-LENGTH)
               (BOOL1-for-k PRED 'FALSE end-tag) (GO L5))
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

        (with-frames
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
                    (with-frames
                      ;; Compile this value with its effect on *open-frames*
                      ;; being independent of the effect of the fall-through branch.
                      (COND ((not (symbolp dest))
                             (p2-for-k (car argl) dest)
                             (setq mv-tag (gensymbol "ANDOR-MV")))
                            (UNCONDITIONAL
                             (P2BRANCH-for-k (CAR ARGL) DEST *BDEST*)
                             (SETQ *BDEST* NIL))
                            (T
                             (P2-for-k (CAR ARGL) dest)))))
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
         ;;+++ This should be an error, not a warning! -smh
         (WARN 'BAD-PROG ':IMPOSSIBLE
               "There is a ~S to ~S not within any ~S."
               'GO (CAR ARGL) 'TAGBODY))
        ((OR (SYMBOLP (CAR ARGL))
             (NOT (%POINTERP (CAR ARGL))))                      ;What the hell is *this* used for? -smh
         (outbret-for-k (car argl)))
        (T
         (WARN 'BAD-PROG ':IMPOSSIBLE
               "The argument of ~S was ~S, not a symbol."
               'GO (CAR ARGL)))))

(DEFUN (:PROPERTY GO-HACK P2-for-k) (ARGL IGNORE)
  (OUTB-for-k `(BRANCH ALWAYS NIL NIL ,(GOTAG-LAP-TAG (CAR ARGL)))))

(DEFUN SIMPLEGOP-for-k (FORM)
  (AND (EQ (CAR-SAFE FORM) 'GO)
       (eql *open-frames*
            (gotag-frame-level (second form)))))

(DEFUN gotag-frame-level (TAG)
  ;; Don't use GOTAGS-SEARCH because we want the warning
  ;; to appear only once, from OUTBRET or GTAG.
  (LET ((gotag (find TAG *GOTAG-ENVIRONMENT* :key #'gotag-prog-tag)))
    (IF GOTAG (progdesc-open-frames (GOTAG-progdesc GOTAG))
      nil)))


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
    (with-frames
      (p2-for-k (if (= (length argl) 1)
                    (first argl)
                  `(values ,@argl))
                rpdesc))
    ;; The dropthru mechanism might be able to accomplish this just as well. -smh
    (unless (d-return-p rpdesc)
      (let ((gotag (gotags-search (progdesc-rettag rpdesc))))
        (when gotag
          (outb-for-k `(branch always nil nil ,(gotag-lap-tag gotag))))))))



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
                 (values source source))        ; $$$ was (VALUES DEST SOURCE) <15-Nov-88 smh>
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
      (let ((tail-p (tail-call-p dest 'catch))
            (catch-continue (gensymbol "CATCH-CONTINUE"))
            (catch-end (gensymbol "CATCH-END")))
        ;; Catches are more common (in inner loops, etc.) than throws, and they never signal
        ;; errors unless things are completely messed up, so we do concern ourselves with
        ;; tail-calling.  You can turn off these if you want.
        (labels ((finish-catch (open-frame operation dest &optional source source-type)
                   (ecase operation
                     ((nil :return :discard)
                      (nc:debug :frames
                        (format nc:*debug-stream* "~%*CATCH-int OPERATION=~a DEST=~a SOURCE=~a SOURCE-TYPE=~a" operation dest source source-type)
                        #+never (format nc:*debug-stream* "~% { ~{~a~^ <- ~} }" (cdr (limited-backtrace 50 :compiler))))
                      ;; Move the value into K:O5 if it's not already there.
                      (when source
                        (outi-for-k `(k:move k:o5 ,source k:boxed-right) :single-value))
                      ;; Break up the frame.
                      (let ((routine (ecase source-type
                                       ((:single-value :single-value-flag nil)
                                        'li:catch-continue-sv)
                                       ((:subr-value)
                                        (when (and (null operation) catch-continue)
                                          ;; This continuation will do as a stand-in for all, if a throw
                                          ;; happens.  No need to output another one later.
                                          (outtag-for-k catch-continue t)
                                          (setq catch-continue nil))
                                        'li:catch-continue)
                                       ((:multiple-values :multiple-values-flag :last-value)
                                        'li:catch-continue-mv))))
                        (let ((*dropthru* *dropthru*))          ;Don't let tail-call set dropthru! -smh 18aug88
                          (maybe-tail-call `(,routine 6) dest source-type tail-p))
                        (when (null operation)
                          (setq source dest)                    ;smh 14sep88
                          (outb-for-k `(branch always nil nil ,catch-end))))
                      (nc:debug :frames
                        (format nc:*debug-stream* "~%*CATCH-int returning [~a ~a]" dest source))
                      (values dest source))
                     (:exist (setf (open-frame-there-p open-frame) t)))))
          ;; Build the new catch frame.
          (let ((new-frame (tail-call-open '*catch tail-p #'finish-catch :subr-value)))
            (setf (open-frame-idest new-frame) dest)
            (outi-for-k `(k:movei k:o0 'li:unwind-marker k:boxed))
            ;; +++ Slightly wrong here.  The computation of the tag could cause a GO, THROW, or RETURN
            ;; outside the catch, in which case the frame needs to be cleaned off the stack *without*
            ;; treating the current frame as a catcher, and without doing any special continuation
            ;; processing.  Probably the tag should just be computed before the frame is made. -smh 9sep88
            (p2-for-k tag 'k:o1)
            (outi-for-k `(k:move k:o2 gr:*special-pdl-ptr* k:boxed-right))
            (outi-for-k `(k:move k:o3 gr:*stack-pointer* k:boxed-right))
            (outi-for-k `(k:move-pc k:o4 ,catch-continue))
            (loop for fp on forms
                  for form = (first fp)
                  for cdest = (if (rest fp) 'd-ignore new-frame)
                  do (with-frames (p2-for-k form cdest)))       ;WITH-FRAMES added -smh 18aug88
            ;; Output a continuation so it can go in the frame.
            (if catch-continue
                (outi-close-for-k *open-frames* dest nil 'k:o5 :subr-value)
              (pop-frame))                                      ; No continuation needed.
            (outtag-for-k catch-end t))))))



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
  (warn t :implausible "Trying to cross compile an UNWIND-PROTECT, which doesn't work yet.")
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
              (pop-frame))
            (outtag-for-k catch-end t)))))))
