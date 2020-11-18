;;; -*- Mode:LISP; Package:VINCULUM; Readtable:CL; Base:10 -*-

(export '(null atom complexp arrayp compiled-function-p
          %fixnump ratiop %rationalp %bignump
          %short-float-p %single-float-p %double-float-p
          data-type
          integerp rationalp floatp numberp listp symbolp
          commonp)
        'vinculum)

;;;; Type Predicates
;;;; =====================================================

;;; the following are the function version of rewritten predicates

(defun null (x)
  (null x))

(defun atom (object)
  (atom object))

(defun complexp (object)
  (complexp object))

(defun arrayp (object)
  (arrayp object))

(defun compiled-function-p (object)
  (compiled-function-p object))

;; these are not Common Lisp

(defun %fixnump (ptr)
  (hw:field= ptr gr:*zero* vinc:%%data-type))

(defun ratiop (ptr)
  (vinc:type-test ptr vinc:$$dtp-rational))

(defun %rationalp (ptr)
  (vinc:type-test ptr vinc:$$dtp-rational))

(defun %bignump (ptr)
  (bignump ptr))

(defun %short-float-p (ptr)
  (short-float-p ptr))

(defun %single-float-p (ptr)
  (single-float-p ptr))

(defun %double-float-p (ptr)
  (double-float-p ptr))



;;;; Multiple type predicates

(defmacro data-type (ptr)
  `(HW:LDB ,ptr %%DATA-TYPE 0))

(eval-when (compile)

(defmacro dtp-test-or (ptr &rest types)
  `(HW:32LOGBITP (DATA-TYPE ,ptr)
                 (DPB-MULTIPLE-UNBOXED
                   ,@(lisp:mapcan
                       #'(lisp:lambda (type)
                           `(1 (BYTE 1 ,type)))
                       types)
                   (HW:UNBOXED-CONSTANT 0))))

)

;;; Should these be substs? (they come out to 4 instructions)


(defun integerp (ptr)
  (or (hw:field= ptr gr:*zero* %%data-type)
      (type-test ptr $$dtp-bignum)))

(defun rationalp (ptr)
  (dtp-test-or ptr
               vinc:$$dtp-fixnum
               vinc:$$dtp-bignum
               vinc:$$dtp-rational))

(defun floatp (ptr)
  (dtp-test-or ptr
               vinc:$$dtp-short-float
               vinc:$$dtp-single-float
               vinc:$$dtp-double-float))


(defun numberp (ptr)
  (dtp-test-or ptr
               vinc:$$dtp-fixnum
               vinc:$$dtp-bignum
               vinc:$$dtp-short-float
               vinc:$$dtp-single-float
               vinc:$$dtp-double-float
               vinc:$$dtp-rational
               vinc:$$dtp-complex))

(defun listp (ptr)
  (dtp-test-or ptr
               vinc:$$dtp-nil
               vinc:$$dtp-cons))

(defun symbolp (ptr)
  (dtp-test-or ptr
               vinc:$$dtp-nil
               vinc:$$dtp-symbol))

(defun commonp (ptr) ;;;; ****** This needs work *******
  t)
;  (dtp-test-or ptr
;              vinc:$$dtp-nil
;              etc))
