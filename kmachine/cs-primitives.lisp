;;; -*- Mode:LISP; Package:PRIMITIVES; Readtable:CL; Base:10 -*-

(global:export '(
                 defmacro
                 defsubst
                 define-modify-macro
                 byte
                 byte-size
                 byte-position
                 ))


(global#:defmacro define-modify-macro (name &rest stuff)
  `(global#:PROGN (SETF::DEFINE-MODIFY-MACRO ,name ,@stuff)
                  (global#:SETF (NLISP::MACRO-FUNCTION ',name)
                                (global#:MACRO-FUNCTION ',name))))

;;; Macros defined in K source files
;;; when compiled with the lambda compiler, will
;;; go both into the lambda environment and the
;;; new macro environment
;;; This macro definition is *not* seen by nlisp:compile-file
;;; -- It probably should be. -- JRM
;;; no, thats the point, this kluge is so that when
;;; something is compiled by the lambda compiler
;;; (or evaled in a buffer) it will be seen in both environments
;;; The eval-when is an even worse kluge, but is the only way I can
;;; think of to allow nlisp:macroexpand to work later in the file
;;; (it is called by setf)
(global#:defmacro defmacro (name lambda-list &body body)
  `(global#:PROGN
     (global#:EVAL-WHEN (global#:EVAL global#:COMPILE global#:LOAD)
       (global#:DEFMACRO ,name ,lambda-list
         ,@body)
       (global#:SETF (NLISP::MACRO-FUNCTION ',name) (global#:MACRO-FUNCTION ',name)))))

;;; this also is taken care of by a toplevel form handler
(global#:defmacro defsubst (name &rest body)
  `(global:PROGN
     (NC:DEF-DECLARATION ,name SUBST '(NAMED-SUBST ,name . ,body))
     (global#:DEFUN ,name . ,body)))

(eval-when (eval compile load)

(global#:defstruct byte-spec
  (size 0 :type global#:fixnum)
  (position 0 :type global#:fixnum))

(global#:defmacro lambda-byte (k-byte &environment env)
  (declare (ignore env))                        ;Clearly a bug! But this system doesn't provide
                                                ;any environment argument to constantp or eval!
                                                ;Lose, lose!!!
  (if (compiler:constantp k-byte)
      (global#:let ((byte (global#:eval k-byte)))
        (global#:byte (byte-size byte)
                      (byte-position byte)))
    k-byte))


(defun byte-size (byte-spec)
  (global#:etypecase byte-spec
    (global#:fixnum
      (let ((fsize (global#:ldb (global#:byte 5. 8.) #+really (lambda-byte vinc:%%byte-size)
                                byte-spec)))
        (if (global#:zerop fsize) 32. fsize)))
    (byte-spec (byte-spec-size byte-spec))))

(defun byte-position (byte-spec)
  (global#:etypecase byte-spec
   (global#:fixnum
     (global#:ldb (global#:byte 8. 0.) #+really
                  (lambda-byte vinc:%%byte-position) byte-spec))
   (byte-spec (byte-spec-position byte-spec))))

(defun byte (size position)
  (when (minusp size)
    (global#:error "BYTE can't handle negative field sizes: ~S" size))
  (cond ((and (<= size 32.)
              (global#:typep position '(global#:signed-byte 8)))
         (global#:dpb size (global#:byte 5. 8.) #+really (lambda-byte vinc:%%byte-size)
                      position))
        (t (global#:error "Complex byte specifiers not yet supported in bootstraping code." size position)
           (make-byte-spec :size size :position position))))

)
;;; End of EVAL-WHEN

(setf (nlisp:macro-function 'setf) (global#:macro-function 'setf:setf))
(setf (nlisp:macro-function 'defsetf) (global#:macro-function 'setf:defsetf))
(setf (nlisp:macro-function 'define-setf-method) (global#:macro-function 'setf:define-setf-method))

(setf (nlisp:macro-function 'setf:defsetf) (global#:macro-function 'setf:defsetf))
(setf (nlisp:macro-function 'setf:define-setf-method) (global#:macro-function 'setf:define-setf-method))
