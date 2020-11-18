;;; -*- Mode:LISP; Package:prims; Readtable:CL; Base:10 -*-

;(prims::defsetf prims::progn (&rest forms) (new-value)
;  (let ((first-forms (butlast forms))
;       (last-form   (first (last forms))))
;    `(PRIMS::PROGN ,@first-forms (PRIMS::SETF ,last-form ,new-value))))

;          ((eq function-name 'PROGN)
;           (let ((first-forms (butlast function-args))
;                 (last-form   (first (last function-args))))
;           (multiple-value-bind (temps values stores store-form access-form)
;               (get-setf-method last-form)
;             (values temps
;                     values
;                     stores
;                     (subst `(PROGN ,@first-forms ,(car stores)) (car stores) store-form)
;                     `(PROGN ,@first-forms ,access-form)))))

;;; Modify macros used in primitive code.

;;; Youcef changed these to use GLOBAL:DEFINE-MODIFY-MACRO which I believe is the lossage
;;; change back to use prims:DEFINE-MODIFY-MACRO defined in CS-PRIMITIVES  --pfc 5/2/88

(define-modify-macro decf (&optional (user:delta 1.))
  -
  "Decrement place by delta.")

(define-modify-macro incf (&optional (user:delta 1.))
  +
  "Increment place by delta.")
