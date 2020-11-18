;;; -*- Mode:LISP; Package:LAMBDA; Base:8; Readtable:CL -*-

;;; There are several kludges in this file among which are:
;;;
;;;    The microtracer is run in a different stack group so as to avoid overflowing
;;;    the pdl which cannot be extended because of a microcode bug.
;;;
;;;    Some internal procedures have been externalized because there are only
;;;    64 locals in any procedure.
;;;
;;; Kludges may be found by looking for flaming comments.

;;; The microtracer is very recursive and tends to overflow the pdl.
;;; Since the code that extends the pdl seems to be broken, we make a stack
;;; group with a large regular pdl explicitly for the microtracer.
;;; A kludge is born.

(defvar *microtracer-stack-group*
        (make-stack-group "Microtracer"
                          :regular-pdl-size 1000000
                          :special-pdl-size   10000))

(defvar *trace-m-mem-only?* nil
  "If T, only consider m-mem in tracing.  If NIL, consider a-mem, too.")
(defvar *interactive-tracing?* '()
  "If T, break on trace errors.  Overrides verbose tracing.")
(defvar *verbose-tracing?* '()
  "If T, print trace errors as they occur.  No effect if interactive tracing is on.")

(defvar *break-on-registers-sourced-set* (make-empty-set *register-set-universe*)
  "The micro-tracer will break upon finding an instruction that sources a register in this set.")

(defvar *current-subroutine* '()
  "Holds the name of the subroutine currently being traced.")

(defvar *subroutine-trace-chain* '()
  "Holds the list of subroutines that are being traced.")

(defvar *name-trace-chain* '()
  "Holds the list of labels seen while tracing.  Useful for debugging the tracer.")

;;; Stack group stuff.

;;; Experimental use of heap closures here.

(defun start-tracer (terminal-io thunk)
  ;; Note that the thunk here is from a different stack group and therefore
  ;; will not lose when we use heap closures.
  (using-heap-closures
    (pkg-goto 'lambda)
    (stack-group-return
      (catch-error-restart ((sys:abort error) "Exit the microtracer.")
        (funcall thunk)))))

(defun within-tracer (thunk)
  ;; (funcall thunk)
  (stack-group-preset *microtracer-stack-group*
                      #'start-tracer terminal-io thunk)
  (funcall *microtracer-stack-group*))


;;; A subroutine is essentially a single instruction as
;;; far as the tracer is concerned.  However, it is an error
;;; to depend on anything that the subroutine smashed
;;; except for values and if you are restoring something
;;; that was saved.  Therefore, when we trace a subroutine
;;; we wish to get back sources, clobbereds, and new save info.

(defun trace-the-microcode ()
  (labels (
    (trace-list (name list)
      (format t "~%~S" name)
      (dolist (routine list)
        (format t "~% ~S" (instruction-address routine))
        (within-tracer #'(lambda () (trace-subroutine routine)))))
    )
    (trace-list "Subroutines:" *subroutine-entries*)
    (trace-list "Miscellaneous:" *instructions-without-predecessors*)))

(defun i-info (symbol)
  "Returns the trace info for instruction at <symbol>"
  (let ((instruction (instruction-at (i-mem-symbol->address symbol))))
    (values (instruction-trace-warnings instruction)
            (instruction-trace-info instruction))))

(defun i-trace (symbol)
  "Starts tracing from instruction at <symbol>."
  (within-tracer
    #'(lambda ()
        (trace-subroutine (instruction-at (i-mem-symbol->address symbol))))))

(defun reinit-trace ()
  (for-each-instruction
    #'(lambda (instruction ignore-address)
        ignore-address
        (setf (instruction-trace-info instruction)         '()
              (instruction-trace-warnings instruction)     '()
              (instruction-calling-subroutine instruction) '()))))

(defun find-who-depends-on (&rest register-names)
  (find-subroutines-which-depend-on-non-args (list->set register-names)))

(defun find-subroutines-which-depend-on-non-args (registers-to-look-for)
  (dolist (subroutine *subroutine-entries*)
    (let ((subroutine-sources (elt (instruction-trace-info subroutine) 0)))
      (when (and subroutine-sources
                 (not (empty-set? (set-diff (set-intersection registers-to-look-for subroutine-sources)
                                            (spread-declarations subroutine 'args)))))
        (format t "~%~S" (instruction-address subroutine)))))
  (format t "~%Th-th-th-that's all folks!"))

;;; How trace warnings are made.

(defsubst make-trace-warning (format-string args)
  (cons format-string args))

(defun output-trace-info (filename)
  (let ((output-stream (open filename :direction :output)))
    (for-each-instruction
      #'(lambda (instruction ignore-address)
          ignore-address
          (display-trace-warnings instruction output-stream)))))

(defun display-trace-warnings (instruction &optional (stream terminal-io))
  (let ((warnings (instruction-trace-warnings instruction)))
    (when warnings
      (format stream "~&~S: " (instruction-address instruction))
      (dolist (warning warnings)
        (apply #'format stream warning)))))

(defun inform (instruction format-string &rest format-args)
  (setq format-args (copylist format-args)) ;; &rest lossage
  (let ((trace-warning (make-trace-warning format-string format-args)))
    (pushnew trace-warning (instruction-trace-warnings instruction) :test #'alike?)
    (if *interactive-tracing?*
        (apply #'break `(,format-string ,@format-args))
        (when *verbose-tracing?*
          (format t "~&~S: ~A" (instruction-address instruction)
                  (apply #'format `(nil ,format-string ,@format-args)))))))

(defun instruction-path-name (path)
  (cond ((symbol? path) path)
        ((instruction? path) (instruction-address path))
        ('else (list (first path) (instruction-address (instruction-xfered-to path))))))

;;; Things that are "interesting"

(defun add-register-breaks (&rest registers)
  (setq *break-on-registers-sourced-set*
        (set-union *break-on-registers-sourced-set*
                   (list->set registers))))

(defun remove-register-breaks (&rest registers)
  (setq *break-on-registers-sourced-set*
        (if (null? registers)
            (make-empty-set)
            (set-diff *break-on-registers-sourced-set* (list->set registers)))))

(defun maybe-break (registers)
  (let ((intersection (set-intersection registers *break-on-registers-sourced-set*)))
    (when (not (empty-set? intersection))
      (break "Souced registers ~S at ~S via ~S." intersection *current-subroutine* *name-trace-chain*))
    registers))

(defun interesting-registers (register-set)
  (if *trace-m-mem-only?*
      (set-intersection register-set *m-mem-registers-set*)
      register-set))

(defun excluding-register-sets (&rest register-sets)
  (let ((total-to-exclude-set (apply #'set-union register-sets)))
    #'(lambda (another-set) (set-diff another-set total-to-exclude-set))))

(defun interesting-sources (register-set)
  (funcall
    (compose
      #'interesting-registers
      (excluding-register-sets *global-registers-set* *constant-registers-set*)
      #'maybe-break
      #'instruction-sources)
    register-set))

(defun interesting-destination (register-set)
  (funcall
    (compose #'interesting-registers
             (excluding-register-sets *global-registers-set*)
             #'instruction-destination)
    register-set))

;;; Saves and restores

(u-neq *loop-tag*)
(u-neq *recur-tag*)
(u-neq *return-tag*)
(u-neq *suspend-tag*)

(defstruct (saves-record (:constructor make-saves-record (from into))
                         (:callable-accessors t))
  from
  into)

(defsynonym saves-record? saves-record-p)
(defalike #'saves-record?
          #'(lambda (saves-record1 saves-record2)
              (and (alike? (saves-record-from saves-record1) (saves-record-from saves-record2))
                   (alike? (saves-record-into saves-record1) (saves-record-into saves-record2)))))

(defstruct (restores-record (:constructor make-restores-record (saves))
                            (:callable-accessors t))
  saves
  (smashed-later? '()))

(defsynonym restores-record? restores-record-p)
(defalike #'restores-record?
          #'(lambda (restores-record1 restores-record2)
              (and (alike? (restores-record-saves restores-record1)
                           (restores-record-saves restores-record2))
                   (alike? (restores-record-smashed-later? restores-record1)
                           (restores-record-smashed-later? restores-record2)))))

(defsynonym exit-reason-identifier car)

(defsynonym entries cdr)
(defsetf entries rplacd)

(defun make-looping-record-maker (tag)
  #'(lambda (instruction)
      (cons (cons tag instruction) nil)))

(defun make-looping-record-predicate (tag)
  #'(lambda (exit-reason)
      (and (pair? exit-reason)
           (pair? (car exit-reason))
           (eq? (car (car exit-reason)) tag))))

(deff make-loop-record (make-looping-record-maker *loop-tag*))
(deff make-recur-record (make-looping-record-maker *recur-tag*))
(deff loop-exit-reason? (make-looping-record-predicate *loop-tag*))
(deff recur-exit-reason? (make-looping-record-predicate *recur-tag*))

(defun path-only-loops? (exit-reason)
  (and (null? (cdr exit-reason))
       (loop-exit-reason? (car exit-reason))))

(defun possible-new-trace-chain (instruction)
  (let ((possible-label (instruction-address instruction)))
    (if (symbol? possible-label)
        (cons possible-label *name-trace-chain*)
        *name-trace-chain*)))

;(defvar *things-to-avoid* '()
;  "Holds list of symbols that must not be reached.")

(defun path-suspends? (exit-reasons)
  (assoc *suspend-tag* exit-reasons))

(defun quit-tracing (instruction exit-reason)
  (values instruction (make-empty-set) (make-empty-set) (make-empty-set) '() '() (list exit-reason)))

(defun trace-subroutine (entry-point)
  "Values: arguments (non-local-sources), values, side-effects, saves, restores"
  (let ((*current-subroutine* entry-point)
        (*subroutine-trace-chain* (cons entry-point *subroutine-trace-chain*)))
    (multiple-value-bind
      (args values clobbers locals)
        (spread-declarations entry-point 'args 'values 'clobbers 'local)
      (multiple-value-bind
        (ignore-entry-point sources maybe-clobbered really-clobbered saves restores exit-reasons)
          (trace-path entry-point)
        ignore-entry-point
        (labels (
          (discrepancy-check (set-to-check report-format-string)
            (unless (empty-set? set-to-check)
              (inform entry-point report-format-string set-to-check)))

          (check-for-undeclared-sources ()
            (discrepancy-check (set-diff sources (set-union args locals))
                                "~&Sources ~S are not args or locals."))

          (check-for-undeclared-clobbers ()
            (discrepancy-check (set-diff maybe-clobbered (set-union values clobbers locals))
                               "~&Clobbers ~S are not values, clobbers or locals."))

          (check-for-unused-args ()
            (discrepancy-check (set-diff args sources) "~&Declares unused args ~S."))

          (check-for-unreturned-values ()
            (let ((never-clobbers      (set-diff values (set-union clobbers maybe-clobbered)))
                  (not-always-clobbers (set-diff values (set-union clobbers really-clobbered))))
              (discrepancy-check not-always-clobbers "~&May not clobber return value ~S.")
              (discrepancy-check never-clobbers "~&Never clobbers return value ~S.")))
          )
          (check-for-undeclared-sources)
          (check-for-undeclared-clobbers)
          (check-for-unused-args)
          (check-for-unreturned-values)
          (values (set-diff sources locals) ;; Anything but locals.
                  values
                  (set-diff maybe-clobbered (set-union locals values))
                  (set-diff really-clobbered (set-union locals values))
                  saves
                  restores
                  exit-reasons))))))

(defun trace-path (instruction)
  "Values: sources, maybe-clobbers, always-clobbers, saves, restores, exit-reasons"
  (cond ((null? instruction)         (ferror nil "Missing instruction"))
        ((already-seen? instruction) (values-list (cons instruction (instruction-trace-info instruction))))
        ((looping? instruction)      (quit-tracing instruction (make-loop-record instruction)))
        ((recurring? instruction)    (inform instruction "~&Recursively entered.")
                                     (quit-tracing instruction (make-recur-record instruction)))
        ('else                       (let ((*name-trace-chain* (possible-new-trace-chain instruction)))
                                       (append-instruction instruction #'(lambda () (trace-successors instruction)))))))

(defun already-seen? (instruction)
  ;;(not (null?
              (instruction-trace-info instruction))
      ;; ))

(defun looping? (instruction)
  (eq? (instruction-calling-subroutine instruction) *current-subroutine*))

(defun recurring? (instruction)
  (member (instruction-calling-subroutine instruction) *subroutine-trace-chain*))

(defun combine-sequences
       (beginning-of-first-sequence beginning-of-later-sequence
        first-sources first-values first-maybe-clobbers first-clobbers first-saves first-restores
        later-sources later-maybe-clobbers later-clobbers later-saves later-restores)
  (let ((path-name (if beginning-of-later-sequence
                       (instruction-path-name beginning-of-later-sequence)
                       'UNSPECIFIED)))
    (labels (
      (maybe-complain (string set)
        (when (not (empty-set? set))
          (inform beginning-of-first-sequence (string-append "~&Path ~S " string) path-name set)))

      (check-smashed-needed-value ()
        (let ((needed-but-smashed
                (set-intersection later-sources
                                  (apply #'set-union
                                         (map 'list #'set-later-may-restore
                                              (set->list-of-subsets (set-diff first-maybe-clobbers first-values)))))))
          (maybe-complain "depends on clobbered values ~S" needed-but-smashed)))

      (check-loses-result ()
        (let ((always-lost (set-intersection first-values (set-diff later-clobbers later-sources)))
              (maybe-lost (set-intersection first-values (set-diff later-maybe-clobbers later-clobbers later-sources))))
          (maybe-complain "may clobber unseen results ~S" maybe-lost)
          (maybe-complain "always clobbers unseen results ~S" always-lost)))

      (combine-sequence-sources ()
        (set-union first-sources (set-diff later-sources first-values)))

      (see-if-first-saves (register)
        (not (null? (car (member register first-saves
                                 :test #'alike?
                                 :key #'saves-record-from)))))

      (set-later-may-restore (register)
        (let ((restore-record (car (member register later-restores
                                           :test #'alike?
                                           :key (compose #'saves-record-into #'restores-record-saves)))))
          (if (or (null? restore-record)
                  (restores-record-smashed-later? restore-record))
              register
              (make-empty-set))))

      (combine-clobbers-set (first-set later-set)
        (let ((new-clobbers-set (make-empty-set)))
          (for-single-element-subsets first-set
            #'(lambda (clobber)
                (setq new-clobbers-set
                      (set-union new-clobbers-set (set-later-may-restore clobber)))))
          (for-single-element-subsets later-set
            #'(lambda (clobber)
                (when (not (see-if-first-saves clobber))
                  (setq new-clobbers-set (set-union new-clobbers-set clobber)))))
          new-clobbers-set))

      (combine-saves ()
        (union later-saves (set-difference first-saves later-restores
                                           :test #'(lambda (save-record restore-record)
                                                     (alike? save-record (restores-record-saves restore-record))))
               :test #'alike?))

      (combine-restores ()
        (dolist (first-restore first-restores)
          (when (not (null? (set-intersection later-maybe-clobbers
                                              (saves-record-from (restores-record-saves first-restore)))))
            (setf (restores-record-smashed-later? first-restore) t)))
        (union first-restores (set-difference later-restores first-saves
                                              :test #'(lambda (restore-record save-record)
                                                        (alike? save-record (restores-record-saves restore-record))))
               :test #'alike?
               :key  #'restores-record-saves))
      )
      (check-smashed-needed-value)
      (check-loses-result)
      (values (combine-sequence-sources)
              (combine-clobbers-set first-maybe-clobbers later-maybe-clobbers)
              (combine-clobbers-set first-clobbers later-clobbers)
              (combine-saves)
              (combine-restores)))))

(defun append-instruction (instruction delayed-tail)
  (if (suspend-flow-tracing? instruction)
      (quit-tracing instruction (list *suspend-tag*))
    (setf (instruction-calling-subroutine instruction) *current-subroutine*)
    (multiple-value-bind
      (tail-begin after-me-sources after-me-maybe-clobbers after-me-clobbers after-me-saves after-me-restores exit-reasons)
        (funcall delayed-tail)
      (if (path-suspends? exit-reasons)
          (quit-tracing instruction (list *suspend-tag*))
          (let* ((my-sources    (interesting-sources      instruction))
                 (my-clobbers   (interesting-destination  instruction))
                 (do-i-save?    (instruction-saves?       instruction))
                 (do-i-restore? (instruction-restores?    instruction))
                 (my-depends    (if do-i-save?    (make-empty-set) my-sources))
                 (my-values     (if do-i-restore? (make-empty-set) my-clobbers))
                 (saves-record  (make-saves-record my-sources my-clobbers))
                 (multiple-entry? (not (null? (cdr (instruction-predecessors instruction)))))
                 (new-exit-reasons '()))
            (multiple-value-bind
              (combined-sources combined-maybe-clobbers combined-clobbers combined-saves combined-restores)
                  (combine-sequences instruction tail-begin
                                     my-depends ;sources if not saving.
                                     my-values  ;values
                                     my-values  ;maybe-clobbers
                                     my-values  ;always-clobbers
                                     (when do-i-save? (list saves-record))
                                     (when do-i-restore? (list (make-restores-record saves-record)))
                                     after-me-sources after-me-maybe-clobbers after-me-clobbers
                                     after-me-saves after-me-restores)
              (labels (
                (close-loops ()
                  (let ((l-ident (exit-reason-identifier (make-loop-record instruction)))
                        (r-ident (exit-reason-identifier (make-recur-record instruction))))
                    (dolist (exit-reason exit-reasons)
                      (cond ((loop-exit-reason? exit-reason)
                             (close-test l-ident exit-reason #'merge-loop-entry))
                            ((recur-exit-reason? exit-reason)
                             (close-test r-ident exit-reason #'merge-loop-entry)) ;; like loop for now.
                            ('else (push exit-reason new-exit-reasons))))
                    new-exit-reasons))

                (close-test (ident exit-reason merger)
                  (if (not (alike? ident (exit-reason-identifier exit-reason)))
                      (progn (when multiple-entry?
                               (push instruction (entries exit-reason)))
                             (push exit-reason new-exit-reasons))
                      (dolist (entry (entries exit-reason))
                        (funcall merger entry))))

                (merge-loop-entry (loop-entry)
                  (multiple-value-bind
                    (entry-sources entry-maybe-clobbers entry-clobbers entry-saves entry-restores ignore-exit-reasons)
                      (values-list (instruction-trace-info loop-entry))
                    ignore-exit-reasons
                    (multiple-value-bind
                      (loop-sources loop-maybe-clobbers loop-clobbers loop-saves loop-restores)
                        (combine-sequences loop-entry instruction
                                           entry-sources entry-clobbers ;;values of the loop
                                           entry-maybe-clobbers entry-clobbers entry-saves entry-restores
                                           combined-sources combined-maybe-clobbers combined-clobbers
                                           combined-saves combined-restores)
                      (setf (instruction-trace-info loop-entry)
                            (list loop-sources loop-maybe-clobbers loop-clobbers loop-saves loop-restores)))))
                )
                (let ((trace-info (list combined-sources combined-maybe-clobbers combined-clobbers
                                        combined-saves combined-restores (close-loops))))
                  (setf (instruction-trace-info instruction) trace-info)
                  (values-list (cons instruction trace-info))))))))))

;;; Moby compiler lossage:  only 64 locals!
;;; We kludge by bringing internal procs outside.

(defun kludge-merge-sources (receiver sources new-sources)
  (funcall receiver (set-union sources new-sources)))

(defun kludge-merge-maybe-clobbers (receiver maybe-clobbers new-maybe-clobbers)
  (funcall receiver (set-union maybe-clobbers new-maybe-clobbers)))

(defun kludge-handle-xct-next (merge-parallel-path successor tail-computer)
  (let ((xct-next (instruction-xct-along-the-way successor)))
    (multiple-value-call
      merge-parallel-path
      (if (null? xct-next)
          (funcall tail-computer)
          (append-instruction xct-next tail-computer)))))


(defun trace-successors (instruction)
  (let ((successor 'none)
        (sources        (make-set))
        (maybe-clobbers (make-set))
        (clobbers       (make-set))
        (saves          '())
        (restores       '())
        (exit-reasons   '()))
    (labels (
;     (merge-sources (new-sources)
;       (setq sources (set-union new-sources sources)))

;     (merge-maybe-clobbers (new-maybe-clobbers)
;       (setq maybe-clobbers (set-union new-maybe-clobbers maybe-clobbers)))

      (merge-clobbers (new-clobbers)
        (let ((always-clobbers (set-intersection new-clobbers clobbers))
              (might-clobber   (set-xor new-clobbers clobbers)))
          (setq maybe-clobbers (set-union might-clobber maybe-clobbers))
          (setq clobbers always-clobbers)))

      (merge-saves (new-saves)
        (when (not (null? (set-exclusive-or new-saves saves :test #'alike?)))
          (inform instruction "~&Saves do not match on successor paths.")
          (setq saves (intersection saves new-saves))))

      (merge-restores (new-restores)
        (when (not (null? (set-exclusive-or new-restores restores :test #'alike?)))
          (inform instruction "~&Restores do not match on successor paths.")
          (setq restores (intersection restores new-restores))))

      (merge-parallel-path (a-successor more-sources more-maybe-clobbers more-clobbers more-saves more-restores exit-reason)
        (unless (path-suspends? exit-reason)
          (if (or (eq? successor 'none) (path-only-loops? exit-reasons))
              (setq successor      a-successor
                    sources        more-sources
                    maybe-clobbers more-maybe-clobbers
                    clobbers       more-clobbers
                    saves          more-saves
                    restores       more-restores
                    exit-reasons   exit-reason)
            (setq successor '())
            (unless (path-only-loops? exit-reason)
              (kludge-merge-sources #'(lambda (merged-sources) (setq sources merged-sources)) sources more-sources)
              (kludge-merge-maybe-clobbers
                #'(lambda (merged-maybe-clobbers) (setq maybe-clobbers merged-maybe-clobbers))
                maybe-clobbers more-maybe-clobbers)
;             (merge-sources more-sources)
;             (merge-maybe-clobbers more-maybe-clobbers)
              (merge-clobbers       more-clobbers)
              (merge-saves          more-saves)
              (merge-restores       more-restores)
              (setq exit-reasons (union exit-reason exit-reasons :test #'alike?))))))

;      (handle-xct-next (successor tail-computer)
;       (let ((xct-next (instruction-xct-along-the-way successor)))
;         (multiple-value-call
;           #'merge-parallel-path
;           (if (null? xct-next)
;               (funcall tail-computer)
;               (append-instruction xct-next tail-computer)))))
      )
      (let ((successors (instruction-successors instruction)))
        (dolist (successor successors)
          (if (instruction? successor)
              (multiple-value-call #'merge-parallel-path (trace-path successor))
              (kludge-handle-xct-next
                #'merge-parallel-path
                successor
                #'(lambda ()
                    (funcall
                      (case (first successor)
                        (calls         #'trace-call-successor)
                        (jump-xct-next #'trace-jump-xct-next-successor)
                        (returns       #'trace-returns-successor))
                      successor)))))))
    (if (null? exit-reasons)
        (quit-tracing instruction (list *suspend-tag*))
        (values successor sources maybe-clobbers clobbers saves restores exit-reasons))))

(defun trace-call-successor (successor)
  (let ((subroutine (instruction-xfered-to successor))
        (rest-of-path (instruction-returned-to successor)))
    (multiple-value-bind
      (sources values maybe-clobbers clobbers saves restores exit-reasons)
        (trace-subroutine subroutine)
      (if (path-suspends? exit-reasons)
          (quit-tracing successor (list *suspend-tag*))
          (multiple-value-bind
            (ignore-tail-head tail-sources tail-maybe-clobbers tail-clobbers tail-saves tail-restores tail-exit-reasons)
              (trace-path rest-of-path)
            ignore-tail-head
            (values-values
              successor
              (combine-sequences subroutine rest-of-path
                                 sources values maybe-clobbers clobbers saves restores
                                 tail-sources tail-maybe-clobbers tail-clobbers tail-saves tail-restores)
              tail-exit-reasons))))))

(defun trace-jump-xct-next-successor (successor)
  (trace-path (instruction-xfered-to successor)))

(defun trace-returns-successor (ignore-successor)
  ignore-successor
  (quit-tracing '() (list *return-tag*)))
