;;; -*- Mode:LISP; Package:EH; Lowercase:T; Base:10; Readtable:ZL -*-

;;;; Stepping commands.

;; Control-X: Control the trap-on-exit bits of frames.
(defun com-toggle-frame-trap-on-exit (sg ignore &optional ignore)
  "Toggles whether we trap on exit from this frame."
  (let ((trap-p (not (trap-on-exit-p sg *current-frame*))))
    (set-trap-on-exit sg *current-frame* trap-p)
    (terpri)
    (princ (if (trap-on-exit-p sg *current-frame*) "Break" "Do not break"))
    (princ " on exit from this frame.")))

(defun set-trap-on-exit (sg frame trap-p)
  "Set or clear trap on exit from FRAME in SG.  TRAP-P = T means set, else clear."
  (let ((rp (sg-regular-pdl sg)))
    (if (eq (rp-function-word rp frame) #'*catch)
        (setq trap-p nil))
    (setf (si::rp-attention rp frame) 1)
    (setf (rp-trap-on-exit rp frame) (if trap-p 1 0)))
  trap-p)

(defun trap-on-exit-p (sg frame)
  "T if FRAME in SG is set to trap on being exited."
  (not (zerop (rp-trap-on-exit (sg-regular-pdl sg) frame))))

;; Meta-X
(defun com-set-all-frames-trap-on-exit (sg ignore &optional ignore)
  "Makes all outer frames trap on exit."
  (do ((frame *current-frame* (sg-next-active sg frame)))
      ((null frame))
    (set-trap-on-exit sg frame t))
  (format t "~%Break on exit from this frame and all outer active frames."))

;; Control-Meta-X
(defun com-clear-all-frames-trap-on-exit (sg ignore &optional ignore)
  "Clears the trap-on-exit flag for all outer frames."
  (do ((frame *current-frame* (sg-next-open sg frame)))
      ((null frame))
    (set-trap-on-exit sg frame nil))
  (format t "~%Do not break on exit from this frame and all outer frames."))

;; Control-D: Proceed, and trap next function call.
(defun com-proceed-trap-on-call (sg error-object &optional ignore)
  "Proceeds from this error (if that is possible) and traps on the next function call."
  (setf (sg-flags-trap-on-call sg) 1)
  (format t "Trap on next function call. ")
  (com-proceed sg error-object))

;; Meta-D
(defun com-toggle-trap-on-call (sg ignore &optional ignore)
  "Toggle whether to trap on next function call."
  (setf (sg-flags-trap-on-call sg) (logxor 1 (sg-flags-trap-on-call sg)))
  (terpri)
  (princ (if (zerop (sg-flags-trap-on-call sg)) "Do not break" "Break"))
  (princ " on next function call."))

;;;; Commands for single-stepping and setting breakpoints

;; Meta-Shift-s
;>> SHOULD DO SOMETHING WITH ARG (like step n steps, not single-step called function, etc)
(defun com-macro-single-step (sg error-object &optional arg)
  "Single steps macroinstructions.
Should do something useful with an arg, but doesn't. Foo."
  (declare (special error-object) (ignore arg))
  (if (not *error-handler-running*)
      (throw 'exit t))
;>> Gak!
  (cond ((and (send error-object :debugging-condition-p)
              (send error-object :proceed-asking-user :operation-handled-p :no-action))
         (setf (getf (sg-plist sg) 'single-macro-dispatch) t)
         (format t "Single step...")
         (proceed-error-sg :no-action)
         nil)
        (t (format t "Cannot single-step from an error"))))

;; Control-Shift-s
(defun com-set-breakpoint (sg ignore &optional arg)
  "Sets a breakpoint in a compiled function.
With a numeric arg, sets a breakpoint at that pc withing the current function.
Otherwise prompts for a function and pc."
  (let ((function (rp-function-word (sg-regular-pdl sg) *current-frame*))
        pc)
    (if arg (setq pc arg)
      (multiple-value-setq (function pc)
        (get-compiled-function-and-pc function)))
    (let ((loss (multiple-value-list (check-for-bogus-pc function pc))))
      (if (null loss)
          (set-breakpoint function pc t)
        (apply #'format t loss)))))

;; Control-Shift-c
(defun com-clear-breakpoint (sg ignore &optional arg)
  "Clears a brekpoint from a compiled function.
With no argument, prompts for a function and pc.
With an arg of -1, clears all breakpoints from the current function
With any other argument, clears a breakpoint at that pc of the current function"
  (format t " Clear macrocode breakpoint")
  (let ((function (rp-function-word (sg-regular-pdl sg) *current-frame*))
        pc)
    (cond ((eq arg -1)
           (format t "~P from ~S" (length (function-breakpoints function)) function)
           (clear-breakpoint function nil t)
           (return-from com-clear-breakpoint nil))
          (arg
           (format t " at ~D" arg)
           (setq pc arg))
          (t
           (multiple-value-setq (function pc)
             (get-compiled-function-and-pc function))))
    (let ((loss (multiple-value-list (check-for-bogus-pc function pc))))
      (if (null loss)
          (clear-breakpoint function pc t)
        (apply #'format t loss)))))

;; Meta-Shift-c
(defun com-list-breakpoints (ignore ignore &optional arg)
  "Lists all functions which have breakpoints set in them
and the pc's at which they have breakpoints"
  (declare (ignore arg))
  (dolist (f *fefs-with-breakpoints*)
    (let ((breakpoints (function-breakpoints f)))
      (format t "~&Function ~S has ~:[a breakpoint~;breakpoints~] at pc~:*~[~;~'s~] ~{~D~^, ~}"
              (cdr breakpoints) breakpoints))))

;;>>
(defun com-clear-all-breakpoints (ignore ignore &optional arg)
  "Clears all breakpoints from all functions which have them"
  (declare (ignore arg))
  (dolist (f *fefs-with-breakpoints*)
    (clear-breakpoint f nil t)))


(defun check-for-bogus-pc (fef pc)
  (check-type fef compiled-function)
  (let ((name (fef-name fef))
        (lim (fef-limit-pc fef))
        (min (fef-initial-pc fef)))
    (if (not (fixnump pc))
        (values "The pc must be a fixnum!")
      (cond ((< pc min)
             (values
               "A pc of ~D is invalid for function ~S, whose instructions start at pc ~D"
               pc name min))
            ((> pc lim)
             (values
               "A pc of ~D is invalid for function ~S (which has ~D instructions)"
               pc name lim))
            (t
             (do (len
                  (n min))
                 ((> n lim) (values))
               (setq len (fef-instruction-length fef n))
               (cond ((= n pc) (return (values)))
                     ((< n pc (setq n (+ n len)))
                      (return-from check-for-bogus-pc
                        (values
                          "~The pc ~D lies in the middle of a multi-word instruction in ~S
  (the instruction starts at ~D)~"
                          pc name (- n len)))))))))))


(defun get-compiled-function-and-pc (&optional default-function)
  (declare (values function pc))
  (let* ((function-name (and default-function (function-name default-function)))
         (prompt #'(lambda (stream ignore)
                     (format stream "~&Function name~@[, or ~*~C for default (~S)~]: "
                             function-name #/end function-name)))
         function)
    (setq function (with-input-editing (*debug-io*
                                         `((:prompt ,prompt)
                                           (:activation char= #/end)
                                           ;; :full-rubout is too obnoxious
                                           ))
                     (prog ()
                        loop
                           (multiple-value-bind (fn flag)
                               (si:read-or-end *debug-io* nil nil)
                             (if (eq flag ':end)
                                 (setq fn default-function))
                             (unless (compiled-function-p fn)
                               (condition-case (error)
                                   (setq fn (fdefinition fn))
                                 (error (parse-ferror "~A" error)
                                        (go loop)))
                               (unless (compiled-function-p fn)
                                 (parse-ferror "~S is not a compiled function" fn)
                                 (go loop)))
                             (return fn))))
          function-name (function-name function))
    (let ((min (fef-initial-pc function))
          (lim (fef-limit-pc function)))
      (setq prompt #'(lambda (stream ignore)
                       (format stream "~&PC within function (between ~D and ~D): " min lim)))
      (with-input-editing (*debug-io*
                            `((:prompt ,prompt)
;character lossage
                              (:activation memq (#.(char-int #/end) #.(char-int #/newline)))
                              (:no-input-save t)))
        (tagbody
         loop
            (let* ((*read-base* 10.)
                   (pc (si:read-for-top-level *debug-io* nil nil))
                   (loss (multiple-value-list (check-for-bogus-pc function pc))))
              (if (null loss)
                  (return-from get-compiled-function-and-pc (values function pc))
                (apply #'parse-ferror loss)
                (go loop))))))))

;;;; The guts of breakpointing

(defvar *fefs-with-breakpoints* ()
  "List of fef's which contain a breakpoint")

(defun function-breakpoints (function)
  "Returns a list  ((pc original-instruction-code) ...)"
  (cdr (assq 'breakpoints (debugging-info function))))

(defun set-breakpoint (function pc &optional print)
  "Sets a breakpoint at PC in function FUNCTION (which must be a compiled function)
PRINT means to print a message on *DEBUG-IO* saying that the breakpoint has been set.
Returns T if successful."
  (check-type function compiled-function)
  (cond ((not ( (fef-initial-pc function) pc (fef-limit-pc function)))
         (ferror "~D is not a valid pc in ~S" pc function))
        ((not (fef-debugging-info-present-p function))
         (ferror "~S doesn't have a debugging-info slot. You lose" function)))
  (without-interrupts
    (let* ((debugging-info (fef-debugging-info function))
           (breakpoints (assq 'breakpoints debugging-info))
           (default-cons-area background-cons-area)
           (bpt compiler::(lap-word-eval '(misc bpt d-ignore)))
           (function-name (function-name function)))
      (if (and (assq pc (cdr breakpoints))
               (eq (fef-instruction function pc) bpt))
          (when print
            (format *debug-io* "~&Breakpoint already exists at pc ~D in ~S"
                    pc function-name))
        (let ((inst (fef-instruction function pc)))
          (let ((%inhibit-read-only t))
            (if breakpoints
                (push (list pc inst) (cdr breakpoints))
              (push `(breakpoints . ((,pc ,inst)))
                    (fef-debugging-info function)))
            (setf (fef-instruction function pc) bpt)))
        (pushnew function *fefs-with-breakpoints* :test #'eq)
        (when print
          (format *debug-io* "~&Breakpoint set at pc ~D in ~S" pc function-name))
        t))))

(defun clear-breakpoint (function &optional pc print)
  "Clears a breakpoint at PC in function FUNCTION (which must be a compiled function)
If PC is not supplied, then clears all breakpoints in FUNCTION
PRINT means to print a message in *DEBUG-IO* saying that the breakpoint has been cleared."
  (check-type function compiled-function)
  (without-interrupts
    (let* ((debugging-info (and (fef-debugging-info-present-p function)
                                (fef-debugging-info function)))
           (breakpoints (assq 'breakpoints debugging-info))
           (bpt compiler::(lap-word-eval '(misc bpt d-ignore)))
           (function-name (function-name function)))
      (flet ((do-it (pc &aux (bp (assq pc (cdr breakpoints))))
               (cond ((not ( (fef-initial-pc function) pc (fef-limit-pc function)))
                      (with-stack-list (args "~D is not a valid pc in ~S" pc function)
                        (if print (apply #'format *debug-io* args) (apply #'ferror args)))
                      nil)
                     ((not (eq (fef-instruction function pc) bpt))
                      (with-stack-list (args (if (cdr breakpoints)
                                                 "~&There is no breakpoint in ~S at pc ~D"
                                                 "~&There are no breakpoints set in ~S")
                                             function pc)
                        (if print (apply #'format *debug-io* args) (apply #'ferror args)))
                      nil)
                     ((null bp)
                      (ferror "~There is a breakpoint in ~S at pc ~D.
However, the information necessary to remove it has been lost!
You lose big!!"
                              function pc))
                     (t (let ((%inhibit-read-only t))
                          (setf (fef-instruction function pc) (cadr bp))
                          (setf (cdr breakpoints) (delq bp (cdr breakpoints))))
                        t))))
        (if pc
            (and (do-it pc)
                 print
                 (format *debug-io* "~&Breakpoint cleared at pc ~D in ~S" pc function-name))
          (let ((winners))
            (dolist (c (cdr breakpoints))
              (and (do-it (car c))
                   print
                   (push (car c) winners)))
            (when print
              (if (null winners)
                  (format *debug-io* "~&No breakpoints in ~S" function)
                (format *debug-io* "~&Breakpoint~@[s~] cleared from ~S at pc~@['s~] ~{~D~^ ~}"
                        (eq (length winners) 1) winners)))))
        (if (null breakpoints)
            (setq *fefs-with-breakpoints* (delq function *fefs-with-breakpoints*)))))))


;;;; the real guts of breakpointing

;; the real hard stuff

;; the stuff I haven't written

;>>
;(defmethod (breakpoint-error :case :proceed-asking-user :no-action) ()

;  )

;(defmethod (breakpoint-error :case :proced-asking-user :single-step) ()

;  )

;(defun proceed-breakpoint (sg fef pc single-step-p)
;  (let* ((bpt (assq pc (assq 'breakpoints (fef-debugging-info fef))))
;        (inst (cadr bpt)))
;    (if (null bpt)
;       (ferror "Foo! I don't know about a breakpoint in ~S at ~D. Lossage!!" fef pc)
;      (without-interrupts
;       (let ((%inhibit-read-only t))
;         (swapf (fef-instruction fef pc) inst)))
;      (setf (getf (sg-plist sg) 'single-macro-dispatch) t)
;      (proceed-error-sg :no-action)


;;;; BREAKON

(defvar *breakon-function-specs* ()
  "List of all function-specs that have BREAKONs.")

(defun breakon (&optional function-spec (condition t))
  "Break on entry to FUNCTION-SPEC, if CONDITION evaluates non-NIL.
If called repeatedly for one function-spec with different conditions,
a break will happen if any of the conditions evaluates non-NIL.

With no args, returns a list of function specs that have had
break-on-entry requested with BREAKON."
  (if (null function-spec)
      *breakon-function-specs*
    (setq function-spec (dwimify-arg-package function-spec 'function-spec))
    (breakon-init function-spec)
    (setq condition (si:rename-within-new-definition-maybe function-spec condition))
    (let* ((spec1 (si:unencapsulate-function-spec function-spec 'breakon)))
      (uncompile spec1 t)
      (let* ((def (fdefinition spec1))
             (default-cons-area background-cons-area)
             ;; Find our BREAKON-THIS-TIME.
             ;; def looks like:
             ;;   (named-lambda (foo debugging-info) arglist
             ;;     (si::encapsulation-let ((arglist (si::encapsulation-list* arglist)))
             ;;        (declare (special arglist))
             ;;        (breakon-this-time (or . conditions) unencapsulated-function arglist)))
             (defn-data (car (si::encapsulation-body def)))
             (slot-loc (cadr defn-data)))       ;Within that, find ptr to list of conditions.
        (pushnew condition (cdr slot-loc) :test #'equal)))
    (if compile-encapsulations-flag
        (compile-encapsulations function-spec 'breakon))
    function-spec))

(defun unbreakon (&optional function-spec (condition t))
  "Remove break on entry to FUNCTION-SPEC, or all functions if no arg.
If CONDITION is specified, we remove only that condition for breaking;
if other conditions have been specified with BREAKON on this function,
the other conditions remain in effect."
  (when function-spec
    (setq function-spec (dwimify-arg-package function-spec 'function-spec)))
  (let* ((spec1 (and function-spec (si:unencapsulate-function-spec function-spec 'breakon))))
    (cond ((null function-spec)
           (mapc #'unbreakon *breakon-function-specs*))
          ((eq condition t)
           (fdefine spec1 (fdefinition (si:unencapsulate-function-spec spec1 '(breakon))))
           (setq *breakon-function-specs*
                 (cl:delete function-spec *breakon-function-specs* :test #'equal))
           function-spec)
          ((neq spec1 (si:unencapsulate-function-spec spec1 '(breakon)))
           (uncompile spec1 t)
           (let* ((def (fdefinition spec1))
                  ;; Find our BREAKON-NEXT-TIME.
                  ;; def looks like:
                  ;;   (named-lambda (foo debugging-info) arglist
                  ;;      (si::encapsulation-let ((arglist (si::encapsulation-list* arglist)))
                  ;;        (declare (special arglist))
                  ;;        (breakon-this-time (or . conditions) unencapsulated-function arglist)))
                  (defn-data (car (si::encapsulation-body def)))
                  (slot-loc (cadr defn-data)))  ;Within that, find ptr to list of conditions.
             (setf (cdr slot-loc)
                   (cl:delete condition (cdr slot-loc) :test #'equal))
             (cond ((null (cdr slot-loc))
                    (fdefine spec1
                             (fdefinition (si:unencapsulate-function-spec spec1 '(breakon))))
                    (setq *breakon-function-specs*
                          (cl:delete function-spec *breakon-function-specs* :test #'equal)))
                   (compile-encapsulations-flag
                    (compile-encapsulations function-spec 'breakon))))
           function-spec))))

;;; Make a specifed function into an broken-on function
;;; (with no conditions yet) if it isn't one already.
(defun breakon-init (function-spec)
  (let ((default-cons-area background-cons-area)
        (spec1 (si:unencapsulate-function-spec function-spec 'breakon)))
    (when (eq spec1 (si:unencapsulate-function-spec spec1 '(breakon)))
      (si:encapsulate spec1 function-spec 'breakon
                      ;; Must cons the (OR) afresh -- it gets RPLAC'd.
                      `(breakon-this-time ,(list 'or)
                                          ,si::encapsulated-function
                                          arglist)
                      '((uninteresting-function debug)))
      (push function-spec *breakon-function-specs*))))

(defun breakon-this-time (break-condition function args)
  (declare (uninteresting-function debug))
  (when break-condition
    (select-processor
      (:cadr (setf (ldb %%m-flags-trap-on-call %mode-flags) 1))
      ((:lambda :explorer) (compiler::%trap-on-next-call))))
  ;; The next call ought to be the function the user is trying to call.
  ;; That will be so only if this function is compiled.
  (apply function args))
