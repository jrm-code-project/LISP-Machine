(defmacro %current-regular-pdl-pointer ()
  `(let (.reg.pdl.pointer.tem.)
     (%push (compiler::%make-explicit-stack-list nil))
     (setq .reg.pdl.pointer.tem. (%pop))
     (%pop)
     (%make-pointer-offset dtp-locative .reg.pdl.pointer.tem. -3)))

(defun eval-lambda (form fctn env)
  (let ((lambda-list (if (memq (car fctn) '(named-lambda named-subst))
                         (caddr fctn)
                         (cadr fctn)))
        (num-args 0)
        args
        pdl-before-args
        )
    ;; start of our manual list or list*
    (setq pdl-before-args (%current-regular-pdl-pointer))
    (do ((ll lambda-list (cdr ll))
         (quote-status '&eval)
         rest-flag)
        ((or (null ll)
             (memq (car ll) '(&aux &key)))
         (setq num-args (length (cdr form)))
         (%assure-pdl-room num-args))
      (cond ((memq (car ll) '(&eval &quote))
             (setq quote-status (car ll)))
            ((eq (car ll) '&rest)
             (setq rest-flag t))
            ((memq (car ll) lambda-list-keywords))
            (rest-flag
             ;; Here if we encounter a rest arg.
             (if ( (length (cdr form))
                    (if (eq quote-status '&quote)
                        num-args
                        ;; stack frames may be moby!
                        200.))
                 ;; If there aren't enough args supplied to actually
                 ;; reach it, arrange to exit via the DO's end-test.
                 (setq ll nil)
               ;; If the quoted rest arg is non-nil,
               ;; set NUM-ARGS to number of spread args,
               ;; and call with ADI.
               (%assure-pdl-room (1+ num-args))
               (return)))
            (t (incf num-args))))
    ;; Now push the args, evalling those that need it.
    (do ((ll lambda-list (cdr ll))
         (argl (cdr form) (cdr argl))
         (quote-status '&eval)
         (argnum 0 (1+ argnum))
         tem)
        (())
      (do () ((null ll))
        (cond ((memq (car ll) '(&eval &quote))
               (setq quote-status (car ll)))
              ((memq (car ll) '(&rest &aux &key))
               (setq ll nil))
              ((memq (car ll) lambda-list-keywords))
              (t (return)))
        (pop ll))
      (cond ((= argnum num-args)
             ;; Done with spread args => push the rest arg.
             (setq tem (%current-regular-pdl-pointer))
             (cond (argl
                    ;; push on either the quoted rest or the extra parameters beyond the
                    ;; stack-frame-size-limited number above
                    (let ((tem1  (if (eq quote-status '&eval)
                                     (mapcar #'eval1 argl)
                                     argl)))
                      (if (eq num-args 0)
                          (setq args tem1)
                        (%push tem1)
                        ;; list*-ify to terminate the list of args
                        (%p-dpb cdr-normal %%q-cdr-code tem)
                        (%p-dpb cdr-error %%q-cdr-code (%pointer-plus tem 1)))))
                   ((eq num-args 0)
                    (setq args ()))
                   (t
                    ;; terminate the list of args
                    (%p-dpb cdr-nil %%q-cdr-code tem)))
             (return))
            ((eq quote-status '&eval)
             (%push (eval1 (car argl))))
            (t
             (%push (car argl)))))
    (setq args (%make-pointer dtp-list (%pointer-plus pdl-before-args 1)))
    (if *applyhook*
        (let ((*evalhook* nil)
              (*applyhook* nil)
              (tem *applyhook*))
          (funcall tem fctn args env))
      (apply-lambda fctn args env))))
