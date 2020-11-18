;-*- Mode:LISP; Package:COMPILER; Lowercase:T; Base:8; Readtable:ZL -*-
;This file contains the Lisp machine Lisp compiler peephole optimizer.

(defparameter peep-trace nil
  "T => print out each bunch of instructions that are going to be optimized.")

(defmacro peep-trace (counter-name &optional branch-target-insn)
  `(progn (incf ,counter-name)
          (incf peep-number-of-optimizations)
          (and peep-trace
               (peep-trace-1 ',counter-name x y z index ,branch-target-insn))))

(defun peep-trace-1 (counter-name x y z index branch-target-insn)
 (pkg-bind "COMPILER"
   (send *standard-output* ':fresh-line)
   (format t "~S: " counter-name)
   (if x (format t "~S " x))
   (format t "~D: ~S" index y)
   (if branch-target-insn
       (format t "[ -> ~S ]" branch-target-insn))
   (format t " ~S" z)))

(defvar set-inds-not-used 0)
(defvar two-branches-to-same-place 0)
(defvar dead-code 0)
(defvar two-branches-same-ind 0)
(defvar move-branch-move 0)
(defvar branch-testing-constant 0)
(defvar branch-or-pop-testing-constant 0)
(defvar push-nil-or-0-then-pop 0)
(defvar push-then-move-from-pdl 0)
(defvar set-inds-already-set 0)
(defvar pop-then-push 0)
(defvar branch-.+1 0)
(defvar branch-to-branch 0)
(defvar branch-across-branch 0)
(defvar branch-to-return 0)
(defvar branch-to-branch-not-taken 0)
(defvar insert-count 0)
(defvar two-pops 0)
(defvar branch-to-set-inds-already-set 0)
(defvar matching-code-precedes-branch-and-target 0)
(defvar two-push-nils 0)
(defvar branch-to-branch-back 0)
(defvar peep-one-spot 0)
(defvar peep-off-queue 0)

(defvar peep-total-time 0)

(defun peep-print-counters ()
  (format t "~&SET-INDS-NOT-USED = ~D." set-inds-not-used)
  (format t "~&TWO-BRANCHES-TO-SAME-PLACE = ~D." two-branches-to-same-place)
  (format t "~&DEAD-CODE = ~D." dead-code)
  (format t "~&TWO-BRANCHES-SAME-IND = ~D." two-branches-same-ind)
  (format t "~&MOVE-BRANCH-MOVE = ~D." move-branch-move)
  (format t "~&BRANCH-TESTING-CONSTANT = ~D." branch-testing-constant)
  (format t "~&BRANCH-OR-POP-TESTING-CONSTANT = ~D." branch-or-pop-testing-constant)
  (format t "~&PUSH-NIL-OR-0-THEN-POP = ~D." push-nil-or-0-then-pop)
  (format t "~&PUSH-THEN-MOVE-FROM-PDL = ~D." push-then-move-from-pdl)
  (format t "~&SET-INDS-ALREADY-SET = ~D." set-inds-already-set)
  (format t "~&POP-THEN-PUSH = ~D." pop-then-push)
  (format t "~&BRANCH-.+1 = ~D." branch-.+1)
  (format t "~&BRANCH-TO-BRANCH = ~D." branch-to-branch)
  (format t "~&BRANCH-ACROSS-BRANCH = ~D." branch-across-branch)
  (format t "~&BRANCH-TO-RETURN = ~D." branch-to-return)
  (format t "~&BRANCH-TO-BRANCH-NOT-TAKEN = ~D." branch-to-branch-not-taken)
  (format t "~&INSERT-COUNT = ~D." insert-count)
  (format t "~&TWO-POPS = ~D." two-pops)
  (format t "~&BRANCH-TO-SET-INDS-ALREADY-SET = ~D." branch-to-set-inds-already-set)
  (format t "~&MATCHING-CODE-PRECEDES-BRANCH-AND-TARGET = ~D." matching-code-precedes-branch-and-target)
  (format t "~&TWO-PUSH-NILS = ~D." two-push-nils)
  (format t "~&BRANCH-TO-BRANCH-BACK = ~D." branch-to-branch-back)
  (format t "~&PEEP-ONE-SPOT = ~D." peep-one-spot)
  (format t "~&PEEP-OFF-QUEUE = ~D." peep-off-queue)
  (format t "~&PEEP-TOTAL-TIME = ~D msec." peep-total-time))

(defun peep-clear-counters ()
  (setq set-inds-not-used 0)
  (setq two-branches-to-same-place 0)
  (setq dead-code 0)
  (setq two-branches-same-ind 0)
  (setq move-branch-move 0)
  (setq branch-testing-constant 0)
  (setq branch-or-pop-testing-constant 0)
  (setq push-nil-or-0-then-pop 0)
  (setq push-then-move-from-pdl 0)
  (setq set-inds-already-set 0)
  (setq pop-then-push 0)
  (setq branch-.+1 0)
  (setq branch-to-branch 0)
  (setq branch-across-branch 0)
  (setq branch-to-return 0)
  (setq branch-to-branch-not-taken 0)
  (setq insert-count 0)
  (setq two-pops 0)
  (setq branch-to-set-inds-already-set 0)
  (setq matching-code-precedes-branch-and-target 0)
  (setq two-push-nils 0)
  (setq branch-to-branch-back 0)
  (setq peep-one-spot 0)
  (setq peep-off-queue 0)
  (setq peep-total-time 0))

;;; The compiler can be run before this function is loaded.
(or (fboundp 'time:microsecond-time)
    (fset 'time:microsecond-time '(lambda () 0)))

(defparameter trace-peep-savings nil
  "T => print how many instructions are saved by PEEP in each function compiled.")

(defvar peep-insns-saved :unbound
  "Accumulates count of instructions saved by PEEP within one function.")

(defvar peep-number-of-optimizations :unbound
  "Accumulates number of optimizations made by PEEP within one function.")

(defvar peep-code-start-index :unbound
  "Within PEEP, the index in PEEP-CODE-ARRAY of the beginning of the actual code.")

(defvar peep-code-array :unbound
  "The array whose elements contain the code to be optimized by PEEP.
This is actually just QCMP-OUTPUT.")

(defvar peep-tag-alist :unbound
  "The alist which keeps track of all tags in the function PEEP is working on.
Each element looks like (TAG USE-COUNT INDEX).  INDEX is the index
in PEEP-CODE-ARRAY where the tag appears.")

(defparameter peep-twice-flag nil
  "T => run PEEP twice on each function, and print out if it finds anything the second time.
This mode is used to verify that the queueing mechanism does its job.")

(defun peep-instruction-p (code-element)
  (and (consp code-element)
       (not (memq (car code-element) '(comment param no-drop-through restart-tag)))))

(defun peep-list-at-index (index)
  (%make-pointer dtp-list (aloc peep-code-array index)))

;; nobody seems to use this
;(defun peep-code ()
;  (peep-list-at-index (1+ (find-position-in-list 'progsa (g-l-p qcmp-output)))))

(defvar peep-index-list nil
  "List of indices saved temporarily so they will be relocated if tags are inserted.")

(defmacro peep-updating-index (index &body body)
  "Execute BODY, relocating the index in the variable INDEX if any tags are inserted by BODY."
  `(let ((peep-index-list (cons ,index peep-index-list)))
     (prog1 (progn . ,body)
            (setq ,index (car peep-index-list)))))

(defun peep (peep-code-array &optional function-name)
  (let* ((peep-code-start-index
           (1+ (find-position-in-list 'progsa (g-l-p peep-code-array))))
         (alphabetic-case-affects-string-comparison t)
         peep-tag-alist
         peep-index-list
         (peep-number-of-optimizations 0)
         (peep-insns-saved 0) (insn-count 0))

    (when trace-peep-savings
      (dolist (elt (peep-list-at-index peep-code-start-index))
        (cond ((atom elt))
              ((memq (car elt) '(restart-tag no-drop-through param comment)))
              (t (incf insn-count)))))
    (let ((start-time (time:microsecond-time)))
      (setq peep-tag-alist (peep-tags))
      (peep-whole-function)
      (incf peep-total-time (ash (- (time:microsecond-time) start-time) -10.)))

    (when trace-peep-savings
      (format t "~&~D instructions saved out of ~D in ~S; ~D optimizations."
              peep-insns-saved insn-count
              function-name peep-number-of-optimizations))
    (when peep-twice-flag
      (let ((peep-insns-saved 0)
            (peep-number-of-optimizations 0))
        (peep-whole-function)
        (or (zerop peep-number-of-optimizations)
            (format t "~&~D instructions saved the second time in ~S; ~D optimizations."
                    peep-insns-saved
                    function-name peep-number-of-optimizations))))))

(defvar peep-scanning-index :unbound
  "Index at which PEEP-WHOLE-FUNCTION is scanning for optimizations.
There is no point in queueing any index greater than this
because PEEP-WHOLE-FUNCTION is going to get there anyway.")

(defvar peep-queue :unbound
  "List of indices which should be reconsidered by PEEP-ONE-SPOT.")

(defun peep-queue (index)
  (or (> index peep-scanning-index)
      (memq index peep-queue)
      (push index peep-queue)))

(defun peep-whole-function (&aux (peep-queue))
  (do ((peep-scanning-index peep-code-start-index))
      (( peep-scanning-index (array-active-length peep-code-array)))
    (cond ((consp (aref peep-code-array peep-scanning-index))
           (setq peep-scanning-index (peep-one-spot peep-scanning-index)))
          (t
           (incf peep-scanning-index))))
  (do ((peep-scanning-index (array-active-length peep-code-array)))
      ((null peep-queue))
    (incf peep-off-queue)
    (peep-one-spot (pop peep-queue))))

(defparameter set-inds-from-source-insns
              '(move movem pop sete setnil setzero bindnil bindpop))

(defparameter poppdl-max #o17 "Maximum number of pops in one POPPDL instruction.")

;;; Delete dead code and perform all optimizations on non-branch instructions.
(defun peep-one-spot (index)
   (prog function
         (x x-ptr y z z-ptr tag-skipped-flag
            y-just-sets-inds
            y-branch-target-index)
         (incf peep-one-spot)
         (or (consp (setq y (aref peep-code-array index))) (return (1+ index)))
         (when (eq (car y) 'comment)
           (setf (aref peep-code-array index) nil)
           (return (1+ index)))
      y-reloaded-reload-z
         (setq z-ptr (1+ index))

      reload-z
         ;; Find next non-deleted object (instruction or tag).
         (do ((length (array-active-length peep-code-array)))
             (())
           (if ( z-ptr length)
               (return (setq tag-skipped-flag t)))
           (if (setq z (aref peep-code-array z-ptr))
               (cond ((atom z) (setq tag-skipped-flag t))
                     ((memq (car z) '(restart-tag param))
                      (setq tag-skipped-flag t))
                     ((memq (car z) '(comment no-drop-through)))
                     (t (return nil))))
           (incf z-ptr))

      y-reloaded
         ;; There are a few optimizations that can go on even across a tag.
         ;; They are the ones that can delete the first insn, not the second.

         ;; Specifically, delete the first instruction
         ;; if all it does is set the indicators
         ;; but what follows does not use them.
         (setq y-just-sets-inds nil x nil)
         (and (or (and (memq (car y) '(move car cdr cadr cddr caar cdar))
                       (neq (caddr y) 'pdl-pop)
                       (memq (cadr y) '(d-inds 0 d-ignore)))
                  (member-equal y '((misc d-inds false) (misc d-ignore false)
                                    (misc d-inds true) (misc d-ignore true)
                                    (move d-pdl pdl-pop)
                                    (move d-pdl (lpdl #o77)))))
              (setq y-just-sets-inds t)
              (not (peep-indicators-used-p z-ptr))
              (progn (peep-trace set-inds-not-used)
                     (go delete-y)))

         (when (eq (car y) 'branch)
           ;; Conditional branch followed by unconditional branch to same place.
           (and (eq (car z) 'branch)
                (eq (cadr z) 'always)
                (not (fourth y))
                (not (fourth z))
                (eq (fifth y) (fifth z))
                (progn (peep-trace two-branches-to-same-place)
                       (go delete-y)))

           ;; Branch to .+1, and not popping.
           (when (and (eq index (setq y-branch-target-index
                                      (peep-previous-insn-index-skipping-tags
                                        (peep-tag-index (fifth y)))))
                      (null (fourth y)))
             (peep-trace branch-.+1)
             (go delete-y))

           (when (eq (cadr y) 'always)
             ;; See if the code before the branch target
             ;; matches the code before the branch.
             (do ((prev-ptr (peep-previous-insn-index index)

                            (peep-previous-insn-index prev-ptr))
                  (target-ptr y-branch-target-index
                              (peep-previous-insn-index target-ptr))
                  (prev-match index prev-ptr)
                  (target-match nil target-ptr))
                 ((or (null prev-ptr)
                      (null target-ptr)
                      (equal (aref peep-code-array prev-ptr) '(no-drop-through))
                      (not (equal (aref peep-code-array prev-ptr)
                                  (aref peep-code-array target-ptr))))
                  (unless (= index prev-match)
                    (setq x (aref peep-code-array prev-match))
                    (peep-trace matching-code-precedes-branch-and-target
                                (aref peep-code-array
                                      (peep-next-insn-index-skipping-tags
                                        (peep-tag-index (fifth y)))))
                    (let ((oinsn (aref peep-code-array prev-match)))
                      (if (eq (car oinsn) 'branch)
                          (peep-delete-tag-ref (fifth oinsn))))
                    (let ((ntag
                            (peep-updating-index prev-match
                              (peep-updating-index index
                                (peep-find-or-insert-tag target-match)))))
                      (setf (aref peep-code-array prev-match)
                            `(branch always nil nil ,ntag))
                      (peep-create-tag-ref ntag))
                    ;; Start deleting this branch
                    ;; so that peep-locally doesn't do this optimization again.
                    (setf (aref peep-code-array index) nil)
                    ;; Perhaps this new branch insn causes optimizations
                    ;; in branches branching to tags that precede it.
                    (peep-previous-insn-index-redo-tags prev-match
                                                        (aref peep-code-array prev-match))
                    (peep-locally (1+ prev-match))
                    (peep-locally prev-match)
                    ;; Finish deleting this branch.
                    (go delete-y)))))

           ;; Various other optimizations of branch insns
           ;; that involve looking at the insn following the target tag.
           (let (target target-ptr x-ptr)
             (setq target-ptr
                   (peep-next-insn-index-skipping-tags
                     (peep-tag-index (fifth y))))
             (when target-ptr
               (setq target (aref peep-code-array target-ptr))
               (cond ((and (eq (car target) 'branch)
                           (neq target y)       ;degenerate case of branch to self
                           (or (eq (cadr target) 'always)
                               (and (eq (cadr target) (cadr y))
                                    (eq (caddr target) (caddr y)))))
                      ;; Target of branch is an unconditional branch
                      ;; or a branch testing the same indicator with the same sense.
                      (peep-trace branch-to-branch target)
                      (let ((otag (fifth y)))
                        (setf (fifth y) (fifth target))
                        (peep-create-tag-ref (fifth y))
                        (peep-delete-tag-ref otag))
                      (go y-reloaded))
                     ((and (eq (car target) 'branch)
                           (eq (cadr target) (cadr y))
                           (neq target y)
                           (not (fourth target)))
                      ;;Branch to another branch on same indicator
                      ;;but opposite sense (and not popping).
                      ;;The other branch will never branch if reached from here,
                      ;;so branch to a new tag following it.
                      (peep-trace branch-to-branch-not-taken target)
                      (peep-updating-index index
                        (peep-change-branch-to-index y (1+ target-ptr)))
                      (go y-reloaded-reload-z))
                     ((and (eq (car target) 'branch)
                           (eq (cadr y) 'always)
                           (null (fourth target))
                           (eq index
                               (peep-previous-insn-index-skipping-tags
                                 (peep-tag-index (fifth target)))))
                      ;; branch [branch .+1]  turns into
                      ;; a branch on the opposite condition, to one after that branch.
                      (peep-trace branch-to-branch-back target)
                      (setf (cadr y) (cadr target))
                      (setf (caddr y) (other (caddr target)))
                      (peep-can-drop-through (1+ index))
                      (peep-updating-index index
                        (peep-change-branch-to-index y (1+ target-ptr)))
                      (go y-reloaded-reload-z))
                     ((and (eq (cadr y) 'always)
                           (eq (cadr target) 'd-return)
                           (memq (car target)
                                 '(move car cdr caar cadr cdar cddr misc false true call0)))
                      ;; Unconditional branch to a single instruction that returns.
                      (peep-trace branch-to-return target)
                      (setf (aref peep-code-array index) target)
                      (let ((oy y))
                        (setq y target)
                        (peep-delete-tag-ref (fifth oy)))
                      (go y-reloaded))
                     ((and (eq (cadr target) 'd-inds)
                           (setq x-ptr (peep-previous-insn-index index))
                           (setq x (aref peep-code-array x-ptr))
                           (equal (caddr target) (caddr x))
                           (not (memq (cadr x) '(d-last d-return)))
                           (or (and (memq (car x) set-inds-from-source-insns)
                                    (eq (car target) 'move))
                               (and (memq (car x) '(car cdr caar cadr cdar cddr))
                                    (eq (car x) (car target)))))
                      ;; Branch to an insn that just sets the inds
                      ;; to what they already were.
                      (peep-trace branch-to-set-inds-already-set target)
                      (peep-updating-index index
                        (peep-change-branch-to-index y (1+ target-ptr)))
                      (go y-reloaded-reload-z))))))

         ;; The rest can happen only if no tag intervenes.
         (if tag-skipped-flag (return z-ptr))

         ;; Ok, we have instructions in Y and Z.
         (cond ((equal y '(no-drop-through))
                ;; Delete dead code following a (NO-DROP-THROUGH)
                (if (peep-instruction-p z)
                    (peep-trace dead-code))
                (go delete-z))
               ((eq (car y) 'branch)
                (cond ((eq (cadr y) 'always)
                       (if (peep-instruction-p z)
                           (peep-trace dead-code))
                       (go delete-z))
                      ((and (eq y-branch-target-index z-ptr)
                            (not (fourth y))
                            (eq (car z) 'branch)
                            (eq (cadr z) 'always))
                       ;; branch .+2 followed by unconditional branch.
                       (peep-trace branch-across-branch)
                       (setf (caddr y) (other (caddr y)))
                       (let ((otag (fifth y)))
                         (setf (fifth y) (fifth z))
                         (setf (fifth z) otag)
                         (go delete-z))))
                (cond ((and (eq (car z) 'branch)
                            (not (fourth y))
                            (eq (cadr y) (cadr z)))
                       ;; Two branches in a row testing the same indicator.
                       (peep-trace two-branches-same-ind)
                       (if (eq (caddr y) (caddr z))
                           ;; same sense => just delete the second one.
                           (go delete-z)
                         ;; opposite sense => second one is really unconditional.
                         (setf (cadr z) 'always)
                         (setf (caddr z) nil)))
                      ;; Branch followed by move.  See if move is to d-inds and is superfluous
                      ((eq (car z) 'move)
                       (and (setq x-ptr (peep-previous-insn-index index))
                            (setq x (aref peep-code-array x-ptr))
                            (memq (car x) set-inds-from-source-insns)
                            (equal (caddr x) (caddr z))
                            (or (and (eq (cadr z) 'd-pdl)
                                     ;; Can optimize branch push
                                     ;; only if the branch is a branch-or-pop.
                                     ;; and the previous insn is also a push or a movem.
                                     (or (equal x z)
                                         (and (eq (car x) 'movem)
                                              (equal (caddr x) (caddr z))))
                                     (fourth y))
                                (and (memq (cadr z) '(d-ignore d-inds 0))
                                     (not (memq (cadr x) '(d-return d-last)))))
                            (progn
                              (peep-trace move-branch-move)
                              (if (eq (cadr z) 'd-pdl)
                                  (setf (fourth y) nil))
                              (go delete-z))))))
               ((eq (car z) 'branch)
                ;; check for MOVE D-INDS constant followed by branches.
                ;; Decide where the branching will stop
                ;; and make one branch straight there.
                ;; Then back to Y-RELOADED which may delete the MOVE.
                ;; Note: we know that the indicators will get used,
                ;; for otherwise the MOVE would already have been deleted, above.
                (let (const)
                  (when (or (and (eq (car y) 'move)
                                 (eq (cadr y) 'd-inds)
                                 (setq const (caddr y))
                                 (consp const)
                                 (eq (car const) 'quote-vector)
                                 (prog1 (eq (caadr const) 'quote)
                                        (setq const (cadadr const))))
                            (prog1 (member-equal y '((misc d-inds false)
                                                     (misc d-inds true)))
                                   (setq const (selectq (caddr y) (true t)))))
                    ;; CONST has the actual constant.
                    (peep-trace branch-testing-constant)
                    (let* ((inds-used (peep-indicators-used-p z-ptr))
                           (final-branch (aref peep-code-array inds-used))
                           (indicator (second final-branch))
                           (otag (fifth z))
                           (sense (third final-branch)))
                      (unless (and (eq (cadr z) 'always)
                                   (eq (fifth z) (fifth final-branch)))
                        (cond ((eq (selectq sense (true t) (false nil))
                                   (funcall (selectq indicator
                                              (nilind 'null)
                                              (atomind 'atom))
                                            const))
                               ;; The final branch will go.
                               (setf (fifth z) (fifth final-branch)))
                              (t ;; The final branchgh will not branch.
                               (if (fourth final-branch)
                                   ;; Can't skip over a branch that pops.
                                   (return z-ptr))
                               (peep-updating-index index
                                 (setf (fifth z) (peep-find-or-insert-tag (1+ inds-used))))))
                        (setf (cadr z) 'always)
                        (setf (caddr z) nil)
                        (peep-create-tag-ref (fifth z))
                        (peep-delete-tag-ref otag)
                        (go y-reloaded-reload-z)))))
                ;; Check for push followed by branch-or-pop that will never branch.
                (let (const)
                  (when (and (fourth z)   ;Must be branch-or-pop
                             (or (and (eq (car y) 'move)
                                      (eq (cadr y) 'd-pdl)
                                      (setq const (caddr y))
                                      (consp const)
                                      (eq (car const) 'quote-vector)
                                      (prog1 (eq (caadr const) 'quote)
                                             (setq const (cadadr const))))
                                 (prog1 (member-equal y '((misc d-pdl false)
                                                          (misc d-pdl true)))
                                        (setq const (eq (caddr y) 'true))))
                             ;; Test that branch will not occur.
                             (neq (selectq (third z) (true t) (false nil))
                                  (funcall (selectq (second z)
                                             (nilind 'null)
                                             (atomind 'atom))
                                           const)))
                    (peep-trace branch-or-pop-testing-constant)
                    ;; Change the push to a d-inds, and delete the branch.
                    (setf (aref peep-code-array index)
                          (setq y
                                (list* (car y) 'd-inds (cddr y))))
                    (go delete-z))))
               ((eq (car z) 'pop)
                ;; PUSH 0 or PUSH NIL followed by POP.
                (when (member-equal y '((move d-pdl (quote-vector (quote 0)))
                                        (move d-pdl (quote-vector (quote nil)))))
                  (peep-trace push-nil-or-0-then-pop)
                  (setf (aref peep-code-array index)
                        (setq y
                              (if (equal y '(move d-pdl (quote-vector (quote 0))))
                                  `(setzero 0 ,(caddr z))
                                `(setnil 0 ,(caddr z)))))
                  (go delete-z)))
               ((and (or (equal z '(move d-ignore pdl-pop))
                         (and (eq (car z) 'misc)
                              (eq (caddr z) 'poppdl)
                              ( (fourth z) poppdl-max)))
                     (or (equal y '(move d-ignore pdl-pop))
                         (and (eq (car y) 'misc)
                              (eq (caddr y) 'poppdl))))
                ;; Two insns in a row that just pop something(s) from the pdl.
                (peep-trace two-pops)
                (let ((total-pops (+ (or (fourth y) 1) (or (fourth z) 1))))
                  (cond (( total-pops poppdl-max)
                         (setf (aref peep-code-array index)
                               (setq y `(misc d-ignore poppdl
                                              ,total-pops)))
                         (go delete-z))
                        (t
                         (setf (aref peep-code-array index)
                               (setq y `(misc d-ignore poppdl ,poppdl-max)))
                         (setf (aref peep-code-array z-ptr)
                               (setq z `(misc d-ignore poppdl ,(- total-pops poppdl-max))))
                         (return z-ptr)))))
               ((eq (car z) 'move)
                (cond ((and (eq (car y) 'move)
                            (eq (cadr y) 'd-pdl)
                            (eq (caddr z) 'pdl-pop))
                       ;; Push followed by move from pdl.
                       ;; Transfer the destination into the push,
                       ;; then the pop is not needed.
                       (peep-trace push-then-move-from-pdl)
                       (setf (cadr y) (cadr z))
                       (go delete-z)))
                (and (equal z '(move d-pdl (quote-vector 'nil)))
                     (equal y '(move d-pdl (quote-vector 'nil)))
                     (progn (peep-trace two-push-nils)
                            (return z-ptr)))
                (cond ((equal (caddr y) (caddr z))
                       (cond ((and (eq (cadr z) 'd-inds)
                                   (not (memq (cadr y) '(d-last d-return)))
                                   (memq (car y) set-inds-from-source-insns))
                              ;; Delete MOVE D-INDS X after something that stores or fetches X.
                              (peep-trace set-inds-already-set)
                              (go delete-z))
                             ((and (eq (cadr z) 'd-pdl)
                                   (eq (car y) 'pop))
                              ;; Turn POP X ? MOVE D-PDL X into MOVEM X
                              (peep-trace pop-then-push)
                              (rplaca y 'movem)
                              (go delete-z)))))))
         (return z-ptr)

      delete-z
         (or (memq (car z) '(no-drop-through comment))
             (incf peep-insns-saved))
         (setf (aref peep-code-array z-ptr) nil)
         (when (eq (car z) 'branch)
           (peep-can-drop-through (1+ z-ptr))
           (peep-delete-tag-ref (fifth z)))
         (incf z-ptr)
         ;; If the previous instruction was a NO-DROP-THROUGH
         ;; we may have turned the branch before it into a branch to .+1.  Check for that.
         (if (eq (car y) 'no-drop-through)
             (peep-locally index))
         (go reload-z)

      delete-y
         (setf (aref peep-code-array index) nil)
         (when (eq (car y) 'branch)
           (peep-can-drop-through (1+ index))
           (peep-delete-tag-ref (fifth y)))
         (incf peep-insns-saved)
         (multiple-value-bind (i f)
             (peep-previous-insn-index-redo-tags index z)
           (setq index i)
           (setq tag-skipped-flag (or f tag-skipped-flag)))
         (unless index (return z-ptr))
         (setq y (aref peep-code-array index))
         ;; If we delete the insn following an unconditional branch,
         ;; we may have created a branch to .+2 before it,
         ;; so scan there.
         (when (and (eq (car y) 'branch)
                    (eq (cadr y) 'always))
           (peep-locally index))
         (setq peep-queue (delq y peep-queue))
         (go y-reloaded-reload-z)))

(defun peep-previous-insn-index-redo-tags (index next-insn &aux tag-skipped-flag)
  (do ((y)) (())
    (setq index (peep-previous-insn-or-tag-index index))
    (cond ((null index) (return nil)))
    (setq y (aref peep-code-array index))
    (cond ((symbolp y)
           (if (or (eq (car next-insn) 'branch)
                   (eq (cadr next-insn) 'd-inds))
               (peep-redo-branches (aref peep-code-array index))))
          ((memq (car y) '(restart-tag param comment no-drop-through)))
          (t (return nil)))
    (setq tag-skipped-flag t))
  (values index tag-skipped-flag))

;;; Reoptimize all branches to TAG.
;;; Called when the instruction that used to follow TAG is deleted.
(defun peep-redo-branches (tag)
  (do ((index peep-code-start-index (1+ index))
       y
       (length (array-active-length peep-code-array)))
      (( index length))
    (and (consp (setq y (aref peep-code-array index)))
         (eq (car y) 'branch)
         (eq (fifth y) tag)
         (peep-queue index))))

;;; Flush any (NO-DROP-THROUGH)s following a deleted branch.
(defun peep-can-drop-through (index)
  (do ((i index (1+ i))
       (length (array-active-length peep-code-array)))
      ((= i length))
    (let ((tem (aref peep-code-array i)))
      (cond ((atom tem))
            ((memq (car tem) '(restart-tag param comment)))
            ((eq (car tem) 'no-drop-through)
             (setf (aref peep-code-array i) nil))
            (t (return nil))))))

;;; Returns non-NIL if the indicators are used by the code starting at INDEX.
;;; The value is the index of the instruction that uses the indicators,
;;; which is a conditional branch instruction.
(defun peep-indicators-used-p (index)
  (do ((i index))
      (())
    (let ((insn (aref peep-code-array i)))
      (cond ((atom insn)
             (incf i))
            ((eq (car insn) 'branch)
             (if (eq (cadr insn) 'always)
                 (setq i (peep-tag-index (fifth insn)))
               (return i)))
            ((and (eq (car insn) 'misc)
                  (memq (caddr insn)
  ;** every misc instruction which doesnt affect M-T has to appear here!!
                        '(unbind poppdl pop-m-from-under-n unbind-to-index)))
             (incf i))
            (t (return nil))))))

(defun peep-tag-index (tag)
  (caddr (assq tag peep-tag-alist)))

;;; Return the index in code-array of the next instruction after INDEX,
;;; even if there are tags in between.  Returns NIL if there is no insn after INDEX.
(defun peep-next-insn-index-skipping-tags (index)
  (do ((i (1+ index) (1+ i))
       (length (array-active-length peep-code-array))
       tem)
      (())
    (cond (( i length) (return nil))
          ((and (consp (setq tem (aref peep-code-array i)))
                (not (memq (car tem) '(restart-tag param comment no-drop-through))))
           (return i)))))

;;; Return the index in code-array of the last instruction before INDEX,
;;; even if there are tags in between.  Returns NIL if there is no insn before INDEX.
(defun peep-previous-insn-index-skipping-tags (index)
  (do ((i (1- index) (1- i))
       tem)
      (())
    (cond ((< i peep-code-start-index) (return nil))
          ((and (consp (setq tem (aref peep-code-array i)))
                (not (memq (car tem) '(restart-tag param comment no-drop-through))))
           (return i)))))

;;; Return the index in code-array of the last instruction or tag before INDEX,
;;; Returns NIL if there is no insn before INDEX.
(defun peep-previous-insn-or-tag-index (index)
  (do ((i (1- index) (1- i)))
      (())
    (cond ((< i peep-code-start-index) (return nil))
          ((aref peep-code-array i)
           (return i)))))

;;; Return the index in code-array of the last instruction before INDEX,
;;; or NIL if there is none or if a tag intervenes.
(defun peep-previous-insn-index (index)
  (do ((i (1- index) (1- i))
       tem)
      (())
    (cond ((< i peep-code-start-index) (return nil))
          ((setq tem (aref peep-code-array i))
           (return (if (or (atom tem) (memq (car tem) '(restart-tag param))) nil i))))))

(defun peep-locally (index)
  (prog function ((prev-insn-index (1- index)))
        (if (> prev-insn-index peep-scanning-index)
            (return nil))
        (do () (())
          (cond ((aref peep-code-array prev-insn-index) (return nil))
                ((= prev-insn-index peep-code-start-index)
                 (return-from function nil)))
          (decf prev-insn-index))
        (if (atom (aref peep-code-array prev-insn-index))
            (return nil))
        (peep-queue prev-insn-index)))

;; PEEP keeps track of all tags in the code with PEEP-TAG-ALIST,
;; a list which contains an element for each tag.
;; The element looks like (TAG USE-COUNT INDEX).

;;; Replace all unreferenced tags with nil in code,
;;; and construct and return the initial value for PEEP-TAG-ALIST.
(defun peep-tags ()
  (let (tag-alist
        (code (peep-list-at-index peep-code-start-index)))
    (mapc #'(lambda (insn)
              (and (not (atom insn))
                   (eq (car insn) 'branch)
                   (let ((tem (assq (fifth insn) tag-alist)))
                     (if tem
                         (incf (cadr tem))
                       (push (list (fifth insn) 1 nil) tag-alist)))))
          code)
    (map #'(lambda (insn-ptr &aux tem)
             (and (atom (car insn-ptr))
                  (not (get (car insn-ptr) 'peep-keep))
                  (if (setq tem (assq (car insn-ptr) tag-alist))
                      (setf (caddr tem)
                            (%pointer-difference insn-ptr (aloc peep-code-array 0)))
                    (rplaca insn-ptr nil))))
         code)
    tag-alist))

;;; Call when a reference to a tag has been deleted.
;;; If the tag is now unreferenced, delete it
;;; and do any optimizations between the instructions before and after it.
(defun peep-delete-tag-ref (tag)
  (unless (get tag 'peep-keep)
    (let ((elt (assq tag peep-tag-alist)))
      (decf (cadr elt))
      (when (zerop (cadr elt))
        (setf (aref peep-code-array (caddr elt)) nil)
        (peep-locally (caddr elt))))))

(defun peep-create-tag-ref (tag)
  (let ((elt (assq tag peep-tag-alist)))
    (incf (cadr elt))))

;;; Insertion of new tags must be requested carefully, because it can invalidate
;;; the indices that you have saved in your local variables.
;;; Use PEEP-UPDATING-INDEX on each index you wish to have relocated
;;; across each call to any of these functions.

;;; Return a tag that is at INDEX (or the same pc),
;;; inserting one if necessary.
(defun peep-find-or-insert-tag (index)
  (let ((length (array-active-length peep-code-array))
        tem)
    (or (do ((i index (1+ i)))
            ((= i length))
          (cond ((setq tem (aref peep-code-array i))
                 (if (atom tem)
                     (return tem)
                   (return nil)))))
        (do ((i (1- index) (1- i)))
            ((< i peep-code-start-index))
          (cond ((setq tem (aref peep-code-array i))
                 (if (atom tem)
                     (return tem)
                   (return nil)))))
        (peep-insert-tag index))))

(defun peep-insert-tag (index)
  (if (= (array-length peep-code-array) (array-active-length peep-code-array))
      (adjust-array-size peep-code-array (+ (array-active-length peep-code-array) 100.)))
  (%blt-typed (aloc peep-code-array (1- (array-active-length peep-code-array)))
              (aloc peep-code-array (array-active-length peep-code-array))
              (- (array-active-length peep-code-array) index)
              -1)
  (dolist (elt peep-tag-alist)
    (if ( (caddr elt) index)
        (incf (caddr elt))))
  (do ((l peep-index-list (cdr l)))
      ((null l))
    (if ( (car l) index)
        (incf (car l))))
  (do ((l peep-queue (cdr l)))
      ((null l))
    (if ( (car l) index)
        (incf (car l))))
  (if ( peep-scanning-index index)
      (incf peep-scanning-index))
  (incf (fill-pointer peep-code-array))
  (incf insert-count)
  (let ((tag (gensym)))
    (push (list tag 0 index) peep-tag-alist)
    (setf (aref peep-code-array index) tag)
    tag))

;;; Change the instruction BRANCH-INSN to branch to a tag at index NEW-INDEX.
;;; A new tag is created if there is no suitable one.
(defun peep-change-branch-to-index (branch-insn new-index)
  (let ((otag (fifth branch-insn)))
    (setf (fifth branch-insn)
          (peep-find-or-insert-tag new-index))
    (peep-create-tag-ref (fifth branch-insn))
    (peep-delete-tag-ref otag)))
