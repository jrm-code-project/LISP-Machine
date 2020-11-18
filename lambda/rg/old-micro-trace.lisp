;;; This file is a somewhat hacked version of RG's original micro-tracer
;;; It is here for reference.


;state changes consist of register-dependance or register-modification.
;  The tree to be searched is defined as follows:
;    start at address, trace successors, and successors of successors, etc watching for loops.
;  Desired output is two lists:
;       register-modification list.
;       register-dependance list.
;  register-modification list is simply the IOR of all dests of all uinsts in entire tree.
;  register-dependance list is IOR of all sources of all uinsts, subtracting, however,
;    any that were modified in the particular patch prior to the point they were sourced.
;
; register <n> should be on the registers-sourced list if any path of control
;    possible starting at <ADDRESS> can reference the original contents of register <n>.
;    (a reference after it has been stored in would not count.)
; register <n> should be on the registers-clobbered list if it is modified
;    by any path starting at <ADDRESS>.

;the flow is traced by starting at ADDRESS and following successors.  Two conditions are
;  watched for.
;  (1)  A loop, ie the same location already exists in the recursive descent path.
;       Since then, more locations could have gotten clobbered, which would result in
;       fewer top level dependancies, so nothing needs to be done now.
;  (2)  A cross, ie the location has been visited before, although it is not on the current path.
;       if the same or more registers have been clobbered now as before, do nothing.
;       this is only a speedwise optimization, compared to following the path again.


(defvar *registers-sourced-result*)
(defvar *registers-clobbered-result*)
(defvar *break-on-register-sourced-list* '())

(defun source-register (register from-address)
  (when (member register *break-on-register-source-list*)
    (break "Sourcing register ~S at ~S." register from-address))
  (unless (constant? register)
    (pushnew register *registers-sourced-result*)))

(defun dest-register (register from-address)
  (when (constant? register)
    (break "Constant register ~S was smashed at ~S" register from-address))
  (pushnew register *registers-clobbered-result*))

(defun get-individual-state-changes (instruction regs-clobbered-so-far save-restore-info)
  (let ((my-sources (instruction-sources instruction))
        (my-dest (instruction-destination instruction))
        (my-declarations (instruction-declarations instruction))
        (new-regs-clobbered-so-far regs-clobbered-so-far)
        (new-save-restore-info save-restore-info))
  (dolist (source my-sources)
    (source-register source instruction))
  (dest-register my-dest instruction)
  (pushnew my-dest new-regs-clobbered-so-far)
  (dolist (declaration my-declarations)
    (case (first declaration)
      (saves (when (member declaration new-save-restore-info :test #'equal?)
               (break "~%Double save of ~S at ~S" declaration instruction))
             (push declaration new-save-restore-info))
      (restores (when (member declaration new-save-restore-info :test #'equal?)
                  (break "~%Double restore of ~S at ~S" declaration instruction))
                (setq new-save-restore-info
                      (remove-matching-restore declaration new-save-restore-info instruction)))))
  (values new-regs-clobbered-so-far new-save-restore-info)))

(defun remove-matching-restore (declaration-to-match new-save-restore-info instruction)
  (let ((corresponding-save (invert-declaration declaration-to-match)))
    (when (not (member corresponding-save new-save-restore-info :test #'equal))
      (break "~%Restore without save ~S at ~S" declaration-to-match instruction))
    (remove corresponding-save new-save-restore-info :test #'equal)))

;;; This is a candidate for the most obscure piece of code...
;(defun match-restore (decl save-restore-info address)
;  (let* ((HEAD (VARIABLE-LOCATION save-restore-info))
;        (TAIL (LOOP FOR L ON (CDR HEAD)                ;(MEMBER-EQUAL ITEM (CDR HEAD)) faster
;                    when (equal decl (car l))
;                    do (break "~%Double restore")
;                    WHEN (and (eq (caar l)
;                                  'saves)
;                              (equal (cdar l) (cdr decl)))
;                    RETURN L)))
;    (if (null tail)
;       (break"~%unable to match restore ~s, ~s at ~s (~s)"
;               decl save-restore-info (address->i-mem-symbol address) address)
;      (LOOP UNTIL (EQ (CDR HEAD) TAIL)
;           DO (RPLACD HEAD (SETQ HEAD (CONS (CADR HEAD) (CDDR HEAD)))))
;      (RPLACD HEAD (CDR TAIL))
;      ;(format t "~%Restoring ~s at ~s (~s)" decl (address->i-mem-symbol address) address)
;      )
;    save-restore-info))

;processing declarations:
;  (suspend-flow-tracing)  causes it to do that.
;  (local <var> ..)  causes vars to immediately become considered clobbered.
;      on subroutine return, they are considered to be randomized.  ** this nyi**
;  (args <var> ..)   error if they are randomized.  **nyi**
;  (values <var> ..) derandomized on return.    **nyi**

;*register-keys* during flow tracing.
; constant or assembly-constant
; (saved <reg>) or (saved-in <reg>)  These entries operate recursively, ie,
;      saving or restoring noted by trace-context for instruction that does it,
;      is in effect for deeper levels of recursion, and is restored when
;      that trace-context eventually exits.
;   detecting dual saving or restoring.
;   loops.  error if loop is not "balanced", ie, any unmatched saves or restores.
;   crosses. error if register saved state not exactly the same as previously.
; randomized.   registers clobbered in a subroutine, (or declared locals) which
;      are not values of the subroutine.


;this frob grubbles around in the binary micro-code stored in micro-code-paging area.
;  It gathers information about which registers are clobbered and/or depended on.

;We start grubbling at an arbitrary place.
;  we note DEPENDANCY for sources of this uinst.
;  we node CLOBBERS   for destination of this uinst.
;  if a ALU or BYTE
;     if popj-after-next
;     otherwise
;  if jump
;     if CALL
;     if POPJ
;     if JUMP
;  if dispatch
;     collect info on dispatch table

;identify entry-points
;  clobbers
;  depends-on

;given we start grubbling at an arbitrary place (called the head).  Trace the flow of control
; accumulating registers clobbered and registers sourced.  These are both monotonic
; functions, (except for knowing about saving and restoring).  So they can be "propagated back"
; at any time, adding to whats previously been propagated back.
; The following interesting things can happen:
;  (1)  we source a new register.
;  (2)  we source a register which has been previously clobbered.
;  (3)  we clobber a new register.
;  (4)  we clobber again a previously clobberred register
;  (5)  we reach a return.  Propagate registers clobbered and sourced back to the head.
;       registers sourced represent arguments or possible "dependancies"
;       registers clobbered represent results or possible side effects
;  (6)  we "close a loop".  Comparing registers-clobbered and registers-sourced now to then,
;       consider following cases:
;         a register was clobbered then, but isnt now.  This could be a new dependancy.
;         a register is clobbered now, but wasnt then.  This could be a new side effect.
;         a register was sourced then, but isnt now.
;         a register is sourced now, but wasnt then. This could be a new dependancy.

(defvar *trace-array* )

;the moby propagate.
;  to start with we have:
;    the successor array
;    the predecessors array
;    the source-array
;    the all-depends-array, initially nil
;    the propagated array, initially nil

(defvar *all-depends-array* )
(defvar *propagated-array* )

(defun print-all-depends-of-subroutines ()
  (dolist (address *subroutine-entries*)
    (format t "~%~s: ~s"
            (address->i-mem-symbol address)
            (mapcar (function address->a-mem-symbol) (aref *all-depends-array* address)))))

(defun moby-propagate nil
  (initialize-trace-array *all-depends-array*)
  (initialize-trace-array *propagated-array*)
  (dotimes (address *maximum-address*)
    (cond ((not (null (aref *i-mem-array* address)))
           (dolist (s (aref *i-mem-source-array* address))
             (let ((key (aref *register-keys* s)))
               (cond ((not (memq key '(constant assembly-constant)))
                      (push s (aref *all-depends-array* address)))))))))
 ;do guys who can be reduced to 0 unprop sucessors in this loop first.
  (tagbody
    top (format t "~%Starting 0 unprop pass")
        (do ((address 0 (1+ address))
             (done 0))
            ((= address *maximum-address*)
             (cond ((not (zerop done))
                    (go top))))
          (cond ((and (not (null (aref *i-mem-array* address)))
                      (null (aref *propagated-array* address)))
                 (dolist (succ (aref *successors-array* address))
                   (cond ((numberp succ)
                          (if (null (aref *propagated-array* succ))
                              (go x)))
                         ((eq (car succ) 'calls)
                          (if (null (aref *propagated-array* (second succ)))
                              (go x))
                          (if (and (fourth succ)
                                   (null (aref *propagated-array* (fourth succ))))
                              (go x)))
                         ((eq (car succ) 'jump-xct-next)
                          (if (null (aref *propagated-array* (second succ)))
                              (go x)))))
                 (propagate-sources-to-predecessors address)
                 (setq done (1+ done))
                 (setq address (max 0 (- address 200)))))
          x )
       )
 ;do guys who are less than or equal to low-count (which count from 1 to five)
  (do ((lowcount 1 (1+ lowcount)))
      ((= lowcount 5))
    (format t "~%Starting lowcount=~s passes" lowcount)
    (tagbody
     top (format t "~%Starting lowcount= ~s pass" lowcount)
         (do ((address *maximum-address* (1- address))
              (count-unprop)
              (done 0))
             ((< address 0)
              (cond ((not (zerop done))
                     (go top))))
           (cond ((and (not (null (aref *i-mem-array* address)))
                       (null (aref *propagated-array* address)))
                  (setq count-unprop 0)
                  (dolist (succ (aref *successors-array* address))
                    (cond ((numberp succ)
                           (if (null (aref *propagated-array* succ))
                               (incf count-unprop)))
                          ((eq (car succ) 'calls)
                           (if (null (aref *propagated-array* (second succ)))
                               (incf count-unprop))
                           (if (and (fourth succ)
                                    (null (aref *propagated-array* (fourth succ))))
                               (incf count-unprop)))
                          ((eq (car succ) 'jump-xct-next)
                           (if (null (aref *propagated-array* (second succ)))
                               (incf count-unprop)))))
                  (cond ((<= count-unprop lowcount)
                         (propagate-sources-to-predecessors address)
                         (setq done (1+ done)))))))))
  (tagbody
    top(format t "~%Making full pass")
       (do ((address 0 (1+ address))
             (best-address nil)
             (best-address-unprop)
             (count-unprop))
            ((= address *maximum-address*)
             (cond (best-address
                    (propagate-sources-to-predecessors best-address)
                    (go top))))
          (cond ((and (not (null (aref *i-mem-array* address)))
                      (null (aref *propagated-array* address)))
                 (setq count-unprop 0)
                 (dolist (succ (aref *successors-array* address))
                   (cond ((numberp succ)
                          (if (null (aref *propagated-array* succ))
                              (incf count-unprop)))
                         ((eq (car succ) 'calls)
                          (if (null (aref *propagated-array* (second succ)))
                              (incf count-unprop))
                          (if (and (fourth succ)
                                   (null (aref *propagated-array* (fourth succ))))
                              (incf count-unprop)))
                         ((eq (car succ) 'jump-xct-next)
                          (if (null (aref *propagated-array* (second succ)))
                              (incf count-unprop)))))
                 (cond ((or (null best-address)
                            (< count-unprop best-address-unprop))
                        (setq best-address address
                              best-address-unprop count-unprop)
                        (cond ((zerop count-unprop)
                               (setq address (1- *maximum-address*)))))))))
       ))

(defun propagate-sources-to-predecessors (address)
  (setf (aref *propagated-array* address) t)
  (let ((dep (aref *all-depends-array* address))
        (pred (aref *predecessors-array* address)))
    (dolist (p pred)
      (cond ((numberp p)
             (propagate-sources dep p nil))
            ((eq (car p) 'called)
             )
            ((eq (car p) 'jump-xct-nexted)
             (propagate-sources dep (second p) (third p)))))))

(defun propagate-sources (sources to-address address-xcted-next)
  (let ((to-dest (aref *i-mem-destination-array* to-address))
        (xct-next-dest (if address-xcted-next (aref *i-mem-destination-array* address-xcted-next))))
    (dolist (s sources)
      (cond ((and (not (memq s to-dest))
                  (not (memq s xct-next-dest))
                  (not (memq s (aref *all-depends-array* to-address))))
             (push s (aref *all-depends-array* to-address))
             (setf (aref *propagated-array* to-address) nil))))))


;returns t if reg not dested in any path, ie, still alive after subroutine return.
(defun find-source-of-dependance (starting-address reg &optional path &aux new-path)
  (cond ((memq starting-address path) nil)
        ((not (memq reg (aref *all-depends-array* starting-address))) nil)
        ((memq reg (aref *i-mem-source-array* starting-address))
         (format t "~% ~s is sourced by ~s, which can be reached by "
                 (address->a-mem-symbol reg)
                 (address->i-mem-symbol starting-address))
         (print-path path)
         nil)
        ((memq reg (aref *i-mem-destination-array* starting-address)) nil)
        (t (let ((val nil))
             (dolist (s *successors-array*)
               (cond ((numberp s)
                      (find-source-of-dependance s reg
                       (cond (new-path)
                             (t (setq new-path (cons starting-address path))))))
                     ((eq (car s) 'calls)
                      (cond ((or (null (third s))
                                 (null (find-source-check-xct-next (third s) reg path)))
                             (cond ((find-source-of-dependance (second s) reg
                                     (cond (new-path)
                                           (t (setq new-path (cons starting-address path)))))
                                    (let ((tem (find-source-of-dependance
                                                 (fourth s) reg new-path)))
                                      (if tem (setq val t))))))))
                     ((eq (car s) 'jump-xct-next)
                      (cond ((null (find-source-check-xct-next (second s) reg
                                    (cond (new-path)
                                          (t (setq new-path (cons starting-address path))))))
                             (let ((tem (find-source-of-dependance (second s) reg new-path)))
                               (if tem (setq val t))))))))
             val))))

(defun find-source-check-xct-next (xcted-next-address reg path)
  (cond ((memq reg (aref *i-mem-source-array* xcted-next-address))
         (format t "~% ~s is sourced by ~s on a xct-next inst, path "
                 (address->a-mem-symbol reg)
                 (address->i-mem-symbol xcted-next-address))
         (print-path path)
         t)
        ((memq reg (aref *i-mem-destination-array* xcted-next-address))
         t)
        (t nil)))

;    each instruction obviously depends on its source, so add that to
;    the all-depends array for that location.

;find an inst with propagated nil and with the minimum number of
; sucessors with propagated nil.

; set propagated flag for this inst. (do it now because it could get cleared below).

; the predecessors of this inst depend on its source(s), unless they dont because
; they mung it(them).  So add any such to all-depends array for predecessor inst.
; If anything was changed, clear propagated flag for that inst.

;repeat until fully propagated.

(defun make-bit-vector-for-i-mem ()
  (make-array *maximum-address* :type 'art-1b))

(defun bit-vector-reset (a)
  (dotimes (address *maximum-address*)
    (setf (aref a address) 0))
  a)

(defun bit-vector-logior (a b)
  (dotimes (address *maximum-address*)
    (setf (aref a address) (logior (aref a address) (aref b address))))
  a)

(defun micro-trace-sequence (start-address)
  (prog (op pjan seq p address)
        (setq p (variable-location seq)
              address start-address)
    l   (setq p (rplacd p (list address))
              op (ldb lam-ir-op (aref *i-mem-array* address))
              pjan (ldb lam-ir-popj-after-next (aref *i-mem-array* address)))
        (cond ((or (= op lam-op-alu)
                   (= op lam-op-byte))
               (setq address (1+ address))
               (go l))))
  )

(defun test-successors (&optional (first-address 0) (length 200000))
  (dotimes (c length)
    (let ((succ (get-inst-successors (aref *i-mem-array* (+ first-address c))
                                     (+ first-address c))))
      (format t "~%I-mem ~s, ~s"
              (+ first-address c)
              succ))))

(defun print-successors-compute (&optional (first-address 0) (length 200000))
  (dotimes (c length)
    (let ((succ (get-inst-successors (aref *i-mem-array* (+ first-address c))
                                     (+ first-address c))))
      (format t "~%I-mem ~s, ~s"
              (address->i-mem-symbol (+ first-address c))
              (print-pred-or-succ-symbolic succ)))))

(defun print-successors (&optional (first-address 0) (length 200000))
  (dotimes (c length)
    (let ((succ (aref *successors-array* (+ first-address c))))
      (format t "~%I-mem ~s, ~s"
              (address->i-mem-symbol (+ first-address c))
              (pred-or-succ-symbolic succ)))))

(defun print-predecessors (&optional (first-address 0) (length 200000))
  (dotimes (c length)
    (format t "~%I-mem ~s, ~s"
              (address->i-mem-symbol (+ first-address c))
              (pred-or-succ-symbolic (aref *predecessors-array* (+ first-address c))))))

(defun pred-or-succ-symbolic (list)
  (cond ((null list) nil)
        ((numberp (car list))
         (cons (address->i-mem-symbol (car list))
               (pred-or-succ-symbolic (cdr list))))
        ((memq (first (car list)) '(calls called))
         (cons `(,(first (car list)) ,(address->i-mem-symbol (second (car list)))
                 ,(address->i-mem-symbol (third (car list))) ,(address->i-mem-symbol (fourth (car list))))
               (pred-or-succ-symbolic (cdr list))))
        ((memq (first (car list)) '(jump-xct-nexted jump-xct-next))
         (cons `(,(first (car list)) ,(address->i-mem-symbol (second (car list)))
                 ,(address->i-mem-symbol (third (car list))))
               (pred-or-succ-symbolic (cdr list))))
        ((eq (first (car list)) 'returns)
         (cons `(,(first (car list)) nil ,(address->i-mem-symbol (third (car list))))
               (pred-or-succ-symbolic (cdr list))))
        (t (ferror nil "bad elem in pred or succ"))))

(comment
(defun get-all-successors-not-via-call-array ()
  (initialize-trace-array *all-successors-array-not-via-call*)
  (dotimes (address *maximum-address*)
    (if (aref *i-mem-array* address)
        (setf (aref *all-successors-array-not-via-call* address)
              (get-all-successors-not-via-call address))))
  *all-successors-array-not-via-call*)

(defun get-all-successors-not-via-call (address)
  (let ((ans nil))
    (labels
      ((trace-successors (address)
        (dolist (ent (aref *successors-array* address))
          (cond ((and (numberp ent)
                      (not (memq ent ans)))
                 (push ent ans)
                 (trace-successors ent))))))
      (trace-successors address)
      ans))) )

(defun sort-trace-array (array)
  (dotimes (address *maximum-address*)
    (setf (aref array address)
          (sort (aref array address) '<))))

(defun print-where-sourced (adr)
  (for-elements-in-trace-array
    *i-mem-source-array*
    (lambda (address element)
      (if (member adr element)
          (format t "~%Sourced at ~s, ~s" address (address->i-mem-symbol address))))))

(defun print-where-dested (adr)
  (for-elements-in-trace-array
    *i-mem-destination-array*
    (lambda (address element)
      (if (member adr element)
          (format t "~%Dested at ~s, ~s" address (address->i-mem-symbol address))))))

;;;;;Here we are
(defun get-state-changes (address)
  (initialize-trace-array *trace-array*)
   ;store list of (list of regs clobbered when entering here in *trace-array*)
  (let ((*registers-sourced-result* nil)
        (*registers-clobbered-result* nil))
    (labels
      ((trace-context (address path-to-address regs-clobbered-so-far save-restore-info)
        ;returns list of regs definitely clobbered in this path.  In some paths others as well
        ;may be, but for purposes of computing registers-sourced we want the minimal set.
        (let ((address-in-path (member address path-to-address))
              (trace-array-data (aref *trace-array* address))
              (new-regs-clobbered-so-far regs-clobbered-so-far)
              (new-save-restore-info save-restore-info)
              (regs-clobbered-in-all-paths '*null*)
              (save-restore-info-in-all-paths '*null*))
          (format t "~%Address ~s (~s), ~s" (address->i-mem-symbol address) address save-restore-info)
          (comment
            (cond ((and trace-array-data
                      (not (equal (cadr trace-array-data) save-restore-info)))
                 (format t "~%Save-restore info differs at ~s (~s), now ~s, then ~s"
                         (address->i-mem-symbol address) address save-restore-info (cadr trace-array-data)))) )
          (cond ((or (zerop address)
                     (suspend-flow-tracing-p address))
                 (values '*dead-end* '*dead-end*))      ;dont trace thru these
                (address-in-path
                 ;check save-restore balance since previous**
                 (values regs-clobbered-so-far save-restore-info))
                ((and trace-array-data
                      (not (fewer-regs-modified regs-clobbered-so-far
                                                (car trace-array-data)))
                      (equal (cadr trace-array-data) save-restore-info))
                 (values (car trace-array-data) (cadr trace-array-data)))
                (t
                 (multiple-value (new-regs-clobbered-so-far new-save-restore-info)
                   (get-state-changes-for-inst-at-address
                     address new-regs-clobbered-so-far new-save-restore-info))
                 (let ((new-path (cons address path-to-address)))
                   (dolist (succ (aref *successors-array* address))
                     (let ((succ-address (cond ((numberp succ) succ)
                                           ((and (listp succ)
                                                 (member (car succ) '(calls jump-xct-next)))
                                            (cadr succ))))
                           (xct-next-address (cond ((and (listp succ)
                                                     (member (car succ)
                                                           '(calls jump-xct-next returns)))
                                                (caddr succ))))
                           (resume-address (cond ((and (listp succ)
                                                   (eq (car succ) 'calls))
                                              (cadddr succ))))
                           (regs-after new-regs-clobbered-so-far)
                           (save-restore-after new-save-restore-info))
                       (cond (xct-next-address
                              (multiple-value (regs-after save-restore-after)
                                (get-state-changes-for-inst-at-address
                                  xct-next-address regs-after save-restore-after))))
                       (cond (succ-address
                              (multiple-value (regs-after save-restore-after)
                                (trace-context succ-address new-path regs-after save-restore-after))))
                       (cond ((and resume-address
                                   (not (eq regs-after '*dead-end*)))
                              (multiple-value (regs-after save-restore-after)
                                (trace-context resume-address new-path
                                               regs-after save-restore-after))))
                       (cond ((not (eq regs-after '*dead-end*))
                              (if (eq regs-clobbered-in-all-paths '*null*)
                                  (setq regs-clobbered-in-all-paths regs-after)
                                (dolist (e regs-clobbered-in-all-paths)
                                  (cond ((not (member e regs-after))
                                         (setq regs-clobbered-in-all-paths
                                               (delq e regs-clobbered-in-all-paths))))))
                              (if (eq save-restore-info-in-all-paths '*null*)
                                  (setq save-restore-info-in-all-paths save-restore-after)
                                (cond ((not (equal save-restore-info-in-all-paths
                                                   save-restore-after))
   (break "~%Save Restore fails to match between branches at ~s (~s), previous ~s, now in ~s, ~s"
           (address->i-mem-symbol address) address save-restore-info-in-all-paths succ save-restore-after))))))))

                   ;return registers clobbered on the way in plus by this inst plus by any
                   ;possible way out.  Thus, if this is a subroutine, these are the ones
                   ;which have been clobbered for sure.
                   (if (not (eq regs-clobbered-in-all-paths '*null*))
                       (dolist (e regs-clobbered-in-all-paths)
                         (pushnew e new-regs-clobbered-so-far)))
                   (if (neq save-restore-info-in-all-paths '*null*)
                       (setq new-save-restore-info
                             save-restore-info-in-all-paths))
                   (cond ((null trace-array-data)
                          (setf (aref *trace-array* address)
                                (list new-regs-clobbered-so-far new-save-restore-info)))
                         (t (rplaca trace-array-data
                                    (intersection new-regs-clobbered-so-far
                                                  (car trace-array-data)))))
                   (values new-regs-clobbered-so-far
                           new-save-restore-info)))))))
      (trace-context address nil nil nil)
      (values *registers-sourced-result* *registers-clobbered-result*))))

(defun fewer-regs-modified (regs-clobbered-now regs-clobbered-then)
  (dolist (reg-then regs-clobbered-then)
    (if (and (not (member reg-then regs-clobbered-now))
             (not (member reg-then *registers-sourced-result*)))
                ;this one potentially could be added
        (return t))))

(defun print-subroutines-symbolic ()
  (dolist (address *subroutine-entries*)
    (format t "~% ~s"
            (address->i-mem-symbol address))))

(defun print-subroutine-dependancies ()
  (dolist (address *subroutine-entries*)
    (print-address-dependancies address)))

(defun print-address-dependancies (address)
  (multiple-value-bind (sourced clobbered)
      (get-state-changes address)
    (format t "~% ~s: sourced: ~s, clobbered: ~s"
            (address->i-mem-symbol address)
            (mapcar 'address->a-mem-symbol sourced)
            (mapcar 'address->a-mem-symbol clobbered)
            )))

(defun print-path (path)
  (dolist (p path)
    (format t "~s " (address->i-mem-symbol p))))
