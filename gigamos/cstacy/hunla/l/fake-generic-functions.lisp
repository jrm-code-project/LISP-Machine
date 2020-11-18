;;; -*- Mode:LISP; Package:(GF USE CL); Readtable:CL; Base:10 -*-
;;; This is a quick hack to get around the lack of generic functions
;;; on the Lambda.  It uses method-arg syntax similar to CLOS.
;;;   -- CStacy, Nov 88

(EVAL-WHEN (EVAL LOAD COMPILE)

(defun decode-lambda-list (lambda-list)
  (declare (values instance-arg positional-args keyword-args keyword-names
                   rest-arg allow-other-keys))
  (let* ((aux-vars (member '&AUX lambda-list))
         (most-args (ldiff lambda-list aux-vars))
         (instance-arg (first most-args)))
    (if (or (null instance-arg)
            (member instance-arg lambda-list-keywords))
        (error "No instance argument was given in the lambda list.")
      (setq most-args (rest most-args)))
    (let* ((rest-arg (member '&REST most-args))
           (keyword-args (member '&KEY most-args))
           (positional-args (ldiff most-args (or keyword-args rest-arg)))
           (allow-other-keys (member '&ALLOW-OTHER-KEYS lambda-list))
           keyword-names)
      (setq positional-args
            (remove '&OPTIONAL
                    (mapcar #'(lambda (arg) (if (consp arg) (car arg) arg))
                            positional-args)))
      (setq keyword-args (loop for k in (cdr keyword-args)
                               until (member k lambda-list-keywords)
                               when (consp k) do (setq k (car k))
                               appending (list (intern (symbol-name k) si:pkg-keyword-package) k)))

      (setq keyword-names (mapcar #'cdr keyword-args))
      (setq rest-arg (cadr rest-arg))
      (values instance-arg
              positional-args
              keyword-args
              keyword-names
              rest-arg
              allow-other-keys))))

(defun check-method-arglist (generic-function-name method-lambda-list)
  (multiple-value-bind (nil m-pos nil m-key m-rst m-aok)
      (decode-lambda-list method-lambda-list)
    (multiple-value-bind (nil g-pos nil g-key g-rst g-aok)
        (decode-lambda-list (si:arglist generic-function-name))
      (unless (and (equal m-pos g-pos)
                   (equal m-key g-key)
                   (equal m-rst g-rst)
                   (equal m-aok g-aok))
        (error "The arglist supplied, ~S, does not match the generic arglist for ~S"
               method-lambda-list generic-function-name)))))

);EVAL-WHEN

(defmacro defgeneric (function-name lambda-list &body declarations)
  (multiple-value-bind (instance-arg positional-args keyword-args nil rest-arg)
      (decode-lambda-list lambda-list)
    (assert (atom instance-arg) (lambda-list))
    (let ((message-sender (if rest-arg 'apply 'funcall))
          (message-arglist (append positional-args
                                   keyword-args
                                   (if rest-arg (list rest-arg)))))
      `(progn
         (eval-when (compile load eval)
           (setf (get ',function-name 'generic-function) t))
         (defun ,function-name ,lambda-list
           ,@declarations
           (,message-sender ,instance-arg ',function-name ,@message-arglist))))))

(defmacro defmethod (function-name lambda-list &body body)
  (let ((specialized-arg (first lambda-list))
        (ordinary-args (rest lambda-list))
        (maybe-generic-function-code nil))
    (when (atom specialized-arg)                ;(FOO...) means ((FOO FOO)....)
      (setq specialized-arg (list specialized-arg specialized-arg)))
    (let ((flavor (cadr specialized-arg))
          (self-var (car specialized-arg)))
      (cond ((get function-name 'generic-function)
             (check-method-arglist function-name lambda-list))
            (t
             (warn "There is no generic ~S function." function-name)
             (setq maybe-generic-function-code
                   `(defun ,function-name (,flavor ,@ordinary-args)
                      (global:send ,flavor ',function-name ,@ordinary-args)))))
      `(progn
         (zl:defmethod (,flavor ,function-name) (,@ordinary-args)
           (let ((,self-var global:self))
             ,self-var                          ;Reference happiness.
             ,@body))
         ,maybe-generic-function-code))))






;;; It's pretty hard to make our own DEFFLAVOR macro; I don't want to
;;; have to mess with COMPILE-AT-APPROPRIATE-TIME and all that other
;;; compiler hair.  CLOS will solve such problems when it arrives.
;;; A user who desires access functions must call an extra macro,
;;; DEFINE-FLAVOR-ACCESS-METHODS, after calling DEFFLAVOR.

(EVAL-WHEN (COMPILE LOAD EVAL)

(defun compose-access-functions (flavor)
  (setq flavor (find-flavor flavor))
  (let ((flavor-name (si:flavor-name flavor))
        (get-fns nil)
        (set-fns nil))
    (dolist (variable-name (union (si:flavor-gettable-instance-variables flavor)
                                  (get flavor ':outside-accessible-instance-variables)))
      (let ((accessor-fn-name (intern (format nil "~:@(~A-~A~)"
                                              flavor-name variable-name)))
            (get-method-name (intern variable-name
                                     si:pkg-keyword-package)))
        (if (fboundp accessor-fn-name)          ;Dont bash existing substs.
            (warn "~S is already defined." accessor-fn-name)
        (push `(defun ,accessor-fn-name (,flavor-name)
                 (send ,flavor-name ,get-method-name))
              get-fns))
        (when (and (not (fboundp accessor-fn-name))
                   (member variable-name (union (si:flavor-settable-instance-variables flavor)
                                                (get flavor ':outside-accessible-instance-variables))))
          (let ((set-method-name (intern (format nil "~:@(SET-~A~)"
                                                 flavor-name variable-name)
                                         si:pkg-keyword-package)))
            (push `(defsetf ,accessor-fn-name (,flavor-name) (store-variable)
                     `(send ,',flavor-name ,',set-method-name ,store-variable))
                  set-fns)))))
    (values get-fns
            set-fns)))

);EVAL-WHEN

(defmacro define-flavor-access-methods (flavor)
  (multiple-value-bind (get-fns set-fns)
      (compose-access-functions flavor)
    `(progn
       ,@get-fns
       ,@set-fns)))
