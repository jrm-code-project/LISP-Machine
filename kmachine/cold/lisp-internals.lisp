;;; -*- Mode:LISP; Package:LISP-INTERNALS; Readtable:ZL; Base:10. -*-

;;; This file contains internal functions used by
;;; the compiler

;;;; Multiple value bind functions

(defconstant multiple-values-limit 30.
  "Upper bound on the number of values that may be returned from a function")

;;; T if the return code in the status register
;;; is multiple value
(defmacro hw:return-code-mv-p ()
  `(hw:32logbitp
     (byte-position hw:%%processor-status-return-code)
     (hw:read-processor-status)))

;;; this shouldn't ever get called
(defun MVBIND-1 (first-value)
  first-value)

(defafun MVBIND-2 (first-value)         ;test if expecting 2 values and only received one, if so, supply nil for the second.
  (alu-field extract-bit-right nop ignore processor-status
             hw:%%processor-status-return-code pw-ii)
  (alu r-2 nop ignore gr:*number-of-return-values* br-zero)
  (branch sv () br-greater-or-equal)
  (branch mv ())
 sv
  (movei gr:*return-0* 'NIL boxed)
 mv
  (return a0 boxed-right))

(defafun MVBIND-3 (first-value)
   (alu-field extract-bit-right nop ignore processor-status
              hw:%%processor-status-return-code pw-ii)
   (movei r0 '3 br-zero)
   (branch one-value (alu l-r nop gr:*number-of-return-values* r0))
   (move-pc r0 dispatch br-greater-or-equal)
   (branch done (alu l+r r0 gr:*number-of-return-values* r0 bw-24 boxed))
   (nop)
   (nop next-pc-dispatch)
 dispatch
   (movei a0 'nil boxed)
 one-value
   (movei gr:*return-0* 'nil boxed)
   (movei gr:*return-1* 'nil boxed)
 done
   (return a0 boxed-right))

(defafun MVBIND-4 (first-value)
   (alu-field extract-bit-right nop ignore processor-status
              hw:%%processor-status-return-code pw-ii)
   (movei r0 '4 br-zero)
   (branch one-value (alu l-r nop gr:*number-of-return-values* r0))
   (move-pc r0 dispatch br-greater-or-equal)
   (branch done (alu l+r r0 gr:*number-of-return-values* r0 bw-24 boxed))
   (nop)
   (nop next-pc-dispatch)
 dispatch
   (movei a0 'nil boxed)
 one-value
   (movei gr:*return-0* 'nil boxed)
   (movei gr:*return-1* 'nil boxed)
   (movei gr:*return-2* 'nil boxed)
 done
   (return a0 boxed-right))

(defafun MVBIND-5 (first-value)
   (alu-field extract-bit-right nop ignore processor-status
              hw:%%processor-status-return-code pw-ii)
   (movei r0 '5 br-zero)
   (branch one-value (alu l-r nop gr:*number-of-return-values* r0))
   (move-pc r0 dispatch br-greater-or-equal)
   (branch done (alu l+r r0 gr:*number-of-return-values* r0 bw-24 boxed))
   (nop)
   (nop next-pc-dispatch)
 dispatch
   (movei a0 'nil boxed)
 one-value
   (movei gr:*return-0* 'nil boxed)
   (movei gr:*return-1* 'nil boxed)
   (movei gr:*return-2* 'nil boxed)
   (movei gr:*return-3* 'nil boxed)
 done
   (return a0 boxed-right))

(defafun MVBIND-6 (first-value)
   (alu-field extract-bit-right nop ignore processor-status
              hw:%%processor-status-return-code pw-ii)
   (movei r0 '6 br-zero)
   (branch one-value (alu l-r nop gr:*number-of-return-values* r0))
   (move-pc r0 dispatch br-greater-or-equal)
   (branch done (alu l+r r0 gr:*number-of-return-values* r0 bw-24 boxed))
   (nop)
   (nop next-pc-dispatch)
 dispatch
   (movei a0 'nil boxed)
 one-value
   (movei gr:*return-0* 'nil boxed)
   (movei gr:*return-1* 'nil boxed)
   (movei gr:*return-2* 'nil boxed)
   (movei gr:*return-3* 'nil boxed)
   (movei gr:*return-4* 'nil boxed)
 done
   (return a0 boxed-right))

;;; this actually ignores n
;;; and sets all return values to nil
(defafun MVBIND-N (first-value n)
   (alu l+r r1 gr:*number-of-return-values* gr:*number-of-return-values* bw-24 boxed-right)
   (alu-field extract-bit-right nop ignore processor-status
              hw:%%processor-status-return-code pw-ii)
   (move-pc r0 dispatch br-zero)
   (branch one-value (alu l+r r0 r1 r0 bw-24 boxed))
   (alu r-1 a1 a1 a1 bw-24 boxed-right)
   (alu r-l a1 a1 gr:*number-of-return-values* bw-24 boxed-right next-pc-dispatch)
 dispatch
   (movei a0 'nil boxed br-negative)
   (branch done (alu r-1 a1 a1 a1 bw-24 boxed-right))
 one-value
   (movei gr:*return-0* 'nil boxed br-negative)
   (branch done (alu r-1 a1 a1 a1 bw-24 boxed-right))
   (movei gr:*return-1* 'nil boxed br-negative)
   (branch done (alu r-1 a1 a1 a1 bw-24 boxed-right))
   (movei gr:*return-2* 'nil boxed br-negative)
   (branch done (alu r-1 a1 a1 a1 bw-24 boxed-right))
   (movei gr:*return-3* 'nil boxed br-negative)
   (branch done (alu r-1 a1 a1 a1 bw-24 boxed-right))
   (movei gr:*return-4* 'nil boxed br-negative)
   (branch done (alu r-1 a1 a1 a1 bw-24 boxed-right))
   (movei gr:*return-5* 'nil boxed br-negative)
   (branch done (alu r-1 a1 a1 a1 bw-24 boxed-right))
   (movei gr:*return-6* 'nil boxed br-negative)
   (branch done (alu r-1 a1 a1 a1 bw-24 boxed-right))
   (movei gr:*return-7* 'nil boxed br-negative)
   (branch done (alu r-1 a1 a1 a1 bw-24 boxed-right))
   (movei gr:*return-8* 'nil boxed br-negative)
   (branch done (alu r-1 a1 a1 a1 bw-24 boxed-right))
   (movei gr:*return-9* 'nil boxed br-negative)
   (branch done (alu r-1 a1 a1 a1 bw-24 boxed-right))
   (movei gr:*return-10* 'nil boxed br-negative)
   (branch done (alu r-1 a1 a1 a1 bw-24 boxed-right))
   (movei gr:*return-11* 'nil boxed br-negative)
   (branch done (alu r-1 a1 a1 a1 bw-24 boxed-right))
   (movei gr:*return-12* 'nil boxed br-negative)
   (branch done (alu r-1 a1 a1 a1 bw-24 boxed-right))
   (movei gr:*return-13* 'nil boxed br-negative)
   (branch done (alu r-1 a1 a1 a1 bw-24 boxed-right))
   (movei gr:*return-14* 'nil boxed br-negative)
   (branch done (alu r-1 a1 a1 a1 bw-24 boxed-right))
   (movei gr:*return-15* 'nil boxed br-negative)
   (branch done (alu r-1 a1 a1 a1 bw-24 boxed-right))
   (movei gr:*return-16* 'nil boxed br-negative)
   (branch done (alu r-1 a1 a1 a1 bw-24 boxed-right))
   (movei gr:*return-17* 'nil boxed br-negative)
   (branch done (alu r-1 a1 a1 a1 bw-24 boxed-right))
   (movei gr:*return-18* 'nil boxed br-negative)
   (branch done (alu r-1 a1 a1 a1 bw-24 boxed-right))
   (movei gr:*return-19* 'nil boxed br-negative)
   (branch done (alu r-1 a1 a1 a1 bw-24 boxed-right))
   (movei gr:*return-20* 'nil boxed br-negative)
   (branch done (alu r-1 a1 a1 a1 bw-24 boxed-right))
   (movei gr:*return-21* 'nil boxed br-negative)
   (branch done (alu r-1 a1 a1 a1 bw-24 boxed-right))
   (movei gr:*return-22* 'nil boxed br-negative)
   (branch done (alu r-1 a1 a1 a1 bw-24 boxed-right))
   (movei gr:*return-23* 'nil boxed br-negative)
   (branch done (alu r-1 a1 a1 a1 bw-24 boxed-right))
   (movei gr:*return-24* 'nil boxed br-negative)
   (branch done (alu r-1 a1 a1 a1 bw-24 boxed-right))
   (movei gr:*return-25* 'nil boxed br-negative)
   (branch done (alu r-1 a1 a1 a1 bw-24 boxed-right))
   (movei gr:*return-26* 'nil boxed br-negative)
   (branch done (alu r-1 a1 a1 a1 bw-24 boxed-right))
   (movei gr:*return-27* 'nil boxed br-negative)
   (branch done (alu r-1 a1 a1 a1 bw-24 boxed-right))
   (movei gr:*return-28* 'nil boxed br-negative)
   (branch done (alu r-1 a1 a1 a1 bw-24 boxed-right))
   (movei gr:*return-29* 'nil boxed br-negative)
   done
   (return a0 boxed-right))



;(defun mvbind-n (first-value n)
;  (unless (hw:return-code-mv-p)
;    (setq gr:*number-of-return-values* 1))
;  (do ((i gr:*number-of-return-values* (1+ i)))
;      ((> i n))
;    (dispatch (byte 5. 0.) i
;      (0 (setq first-value nil))
;      (1 (setq gr:*return-0* nil))
;      ;...
;      (30 (setq gr:*return-29* nil))
;      (31 (error "too many return values expected"))))
;  first-value)


(defmacro multiple-value-list (form)
  `(MV-LIST ,form))

(defun MV-LIST (first-value)
  (if (not (hw:return-code-mv-p))
      (cons first-value '())
    (do ((i gr:*number-of-return-values* (1- i))
         (l '() (cons (dispatch (byte 5 0) i
                                (0 (error "MULTIPLE-VALUE-LIST is screwed up"))
                                (1 first-value)
                                (2 gr:*return-0*) (3 gr:*return-1*)
                                (4 gr:*return-2*) (5 gr:*return-3*)
                                (6 gr:*return-4*) (7 gr:*return-5*)
                                (8 gr:*return-6*) (9 gr:*return-7*)
                                (10 gr:*return-8*) (11 gr:*return-9*)
                                (12 gr:*return-10*) (13 gr:*return-11*)
                                (14 gr:*return-12*) (15 gr:*return-13*)
                                (16 gr:*return-14*) (17 gr:*return-15*)
                                (18 gr:*return-16*) (19 gr:*return-17*)
                                (20 gr:*return-18*) (21 gr:*return-19*)
                                (22 gr:*return-20*) (23 gr:*return-21*)
                                (24 gr:*return-22*) (25 gr:*return-23*)
                                (26 gr:*return-24*) (27 gr:*return-25*)
                                (28 gr:*return-26*) (29 gr:*return-27*)
                                (30 gr:*return-28*) (31 gr:*return-29*))
                      l)))
        ((zerop i) l))))


(defun VALUES-LIST (list &aux first-value)
  (do ((values list (cdr values))
       (i 0 (1+ i)))
      ((null values)
       (if (= gr:*number-of-return-values* 1)
           first-value
         (progn (setq gr:*number-of-return-values* i)
                (hw:return-mv first-value))))
    (let ((value (car values)))
      (dispatch (byte 5 0) i
        (0 (setq first-value value))
        (1 (setq gr:*return-0* value))
        (2 (setq gr:*return-1* value))
        (3 (setq gr:*return-2* value))
        (4 (setq gr:*return-3* value))
        (5 (setq gr:*return-4* value))
        (6 (setq gr:*return-5* value))
        (7 (setq gr:*return-6* value))
        (8 (setq gr:*return-7* value))
        (9 (setq gr:*return-8* value))
        (10 (setq gr:*return-9* value))
        (11 (setq gr:*return-10* value))
        (12 (setq gr:*return-11* value))
        (13 (setq gr:*return-12* value))
        (14 (setq gr:*return-13* value))
        (15 (setq gr:*return-14* value))
        (16 (setq gr:*return-15* value))
        (17 (setq gr:*return-16* value))
        (18 (setq gr:*return-17* value))
        (19 (setq gr:*return-18* value))
        (20 (setq gr:*return-19* value))
        (21 (setq gr:*return-20* value))
        (22 (setq gr:*return-21* value))
        (23 (setq gr:*return-22* value))
        (24 (setq gr:*return-23* value))
        (25 (setq gr:*return-24* value))
        (26 (setq gr:*return-25* value))
        (27 (setq gr:*return-26* value))
        (28 (setq gr:*return-27* value))
        (29 (setq gr:*return-28* value))
        (30 (setq gr:*return-29* value))
        (31 (error "More than 30 return values"))))))







;;;; Special Variable Binding

;;; This is a shallow binding mechanism
;;; Special pdls are arrays of type array:art-special-pdl.  they are consed in gr:*special-pdl-area*.
;;; The zeroth element (at offset 1) of the special pdl points to the stack group to which it belongs.
;;; The value of the special pdl pointer is an unboxed locative into the special pdl.
;;; It points to element 1 (offset 2) for an empty special pdl.
;;; The rest of the special pdl contains alternating SYMBOL-VALUE SYMBOL pairs.

;;; 9 instructions
;(defun BIND (symbol value)
;  ;; read old value
;  (hw:vma-start-read-visible-evcp-vma-unboxed-md-boxed (hw:32-1+ symbol))
;  ;; save it
;  (hw:write-md-boxed (hw:read-md))
;  (hw:vma-start-write-boxed gr:*special-pdl-ptr*)
;  (hw:memory-wait)
;  ;; save symbol
;  (hw:write-md-boxed symbol)
;  (hw:vma-start-write-boxed (hw:32-1+ gr:*special-pdl-ptr*))
;  ;; bump pointer
;  (setq gr:*special-pdl-ptr* (hw:32-2+ gr:*special-pdl-ptr*)))


(defafun BIND (symbol value)
   (alu r+1 gr:*allow-sequence-break* gr:*allow-sequence-break* gr:*allow-sequence-break* bw-24 boxed-right)
   (alu r+1 (vma-start-read-visible-evcp unboxed-vma boxed-md) ignore a0 bw-32)
   (alu l-r nop gr:*special-pdl-ptr* gr:*special-pdl-limit* bw-24)
   (alu r+2 gr:*special-pdl-ptr* ignore gr:*special-pdl-ptr* bw-32 br-not-greater-or-equal)
   (branch bind-1 (move (md boxed-md) md))      ;old symbol value
    (open-call (special-pdl-overflow 0) ignore ())
 bind-1
   (alu r-2 (vma-start-write boxed-vma) ignore gr:*special-pdl-ptr* bw-32)      ;save old symbol value
   (memory-wait)
   (move (md boxed-md) a0)                      ;symbol being bound
   (alu r-1 (vma-start-write boxed-vma) ignore gr:*special-pdl-ptr* bw-32)      ;record symbol being bound
   (memory-wait)
   (move (md boxed-md) a1)
   (alu r+1 (vma-start-write unboxed-vma) ignore a0 bw-32)      ;change symbol to have new value
   (alu r-1 gr:*allow-sequence-break* gr:*allow-sequence-break* gr:*allow-sequence-break* bw-24 boxed-right)
   (move return a1 boxed-right ch-return next-pc-return))

(defun special-pdl-overflow ()
  (trap:illop "Special PDL Overflow"))

;;; 8 instructions
(defun UNBIND-1 ()
  (hw:vma-start-read-vma-boxed-md-boxed (hw:32-1- gr:*special-pdl-ptr*) 0)
  (setq gr:*special-pdl-ptr* (hw:32-2- gr:*special-pdl-ptr*))
  (let ((sym (hw:read-md)))
    (hw:vma-start-read-visible-evcp-vma-boxed-md-boxed gr:*special-pdl-ptr*)
    (hw:write-md-boxed (hw:read-md))
    (hw:vma-start-write-unboxed (hw:32-1+ sym))
    ;(hw:return-tail nil)       ;new version of cross compiler will depend on this.
    ))

(defun unbind-1v (value)
  (unbind-1)
  value)

(defun UNBIND (n)
  (do () (())
    (hw:vma-start-read-vma-boxed-md-boxed (hw:32-1- gr:*special-pdl-ptr*) 0)
    (setq gr:*special-pdl-ptr* (hw:32-2- gr:*special-pdl-ptr*))
    (let ((sym (hw:read-md)))
      (hw:vma-start-read-visible-evcp-vma-boxed-md-boxed gr:*special-pdl-ptr*)
      (hw:write-md-boxed (hw:read-md))
      (hw:vma-start-write-unboxed (hw:32-1+ sym)))
    (if (zerop (setq n (1- n)))
        (return-from unbind))))

(defun unbindv (n value)
  (unbind n)
  value)

(defun UNBIND-TO (ptr)
  (do ()
      ((eq ptr gr:*special-pdl-ptr*))
    (hw:vma-start-read-vma-boxed-md-boxed (hw:32-1- gr:*special-pdl-ptr*) 0)
    (setq gr:*special-pdl-ptr* (hw:32-2- gr:*special-pdl-ptr*))
    (let ((sym (hw:read-md)))
      (hw:vma-start-read-visible-evcp-vma-boxed-md-boxed gr:*special-pdl-ptr*)
      (hw:write-md-boxed (hw:read-md))
      (hw:vma-start-write-unboxed (hw:32-1+ sym)))))

(defun unbind-tov (ptr value)
  (unbind-to ptr)
  value)

;;; This function does nothing but return; i.e. it uses up the OPEN (now active) frame.
;;; This is needed for the compilation of various operations which do OPEN but otherwise
;;; never call.  (Typically these involve a GO or RETURN from inside computing the args
;;; for a function).  --RWK

(defun discard-open-frame ()
  )

;;; This function does nothing but return its only argument.
;;; This is for the compilation of PROG1.

(defun prog1-internal (value)
  value)



;;;; Cons Rest Args

;;;
;;; (defun f (m n o p q r)
;;;   (g r q p o n m) ...)
;;;
;;; (defun g (a b c &rest z)
;;;   ...)
;;;
;;;
;;; f
;;;  (move O0 A5)
;;;  ...
;;;  (move O5 A0)
;;;  (move *arg-2* '6)             ;<n-args>
;;;  (call g)
;;;  ...
;;;
;;; g
;;;  (move *arg-1* '3)             ;<n-spread-args>
;;;  (jump cons-rest (*return-pc-1* trap-pc+))
;;;  (move A3 *rest*)
;;;  ...
;;;

;(defun cons-rest () ;(*arg-1* *arg-2*)
;  (tagbody
;      (setq gr:*value-1* nil)
;      (go next)
;   loop
;      (push (if (> gr:*arg-2* 15)
;               (get-stack-arg gr:*arg-2*)
;             (get-arg gr:*arg-2*))
;           gr:*value-1*)
;      (setq gr:*arg-2* (1+ gr:*arg-2*))
;   next
;      (if (< gr:*arg-2* gr:*arg-1*)
;        (go loop))
;      (hw:dispatch (1+ gr:*return-pc-1*))))


;;; CONS-REST
;;; called with
;;;     <return-address>-1 in *return-pc-1*
;;;     *arg-2* : <n-spread-args> = <first-consed-arg>
;;;     *arg-1* : <nargs> = <last-consed-arg>+1
;;; returns in gr:*value-1*
;;;
(defafun CONS-REST ()
   (unconditional-branch next (alu zero gr:*value-1* ignore ignore boxed))
 loop
   (move-pc r0 get-arg-dispatch br-greater-than boxed)
   (branch get-stack-arg (alu l+r r0 gr:*arg-1* r0 bw-24 boxed))
   (alu r-1 gr:*arg-1* gr:*arg-1* gr:*arg-1* dt-both-fixnum-with-overflow boxed-right)
 get-arg-dispatch
   (nop next-pc-dispatch ch-open)
   (unconditional-branch cons-it (move O0 A0))
   (unconditional-branch cons-it (move O0 A1))
   (unconditional-branch cons-it (move O0 A2))
   (unconditional-branch cons-it (move O0 A3))
   (unconditional-branch cons-it (move O0 A4))
   (unconditional-branch cons-it (move O0 A5))
   (unconditional-branch cons-it (move O0 A6))
   (unconditional-branch cons-it (move O0 A7))
   (unconditional-branch cons-it (move O0 A8))
   (unconditional-branch cons-it (move O0 A9))
   (unconditional-branch cons-it (move O0 A10))
   (unconditional-branch cons-it (move O0 A11))
   (unconditional-branch cons-it (move O0 A12))
   (unconditional-branch cons-it (move O0 A13))
   (unconditional-branch cons-it (move O0 A14))
   (unconditional-branch cons-it (move O0 A15))
 get-stack-arg
   (alu r-1 (vma-start-read boxed-vma boxed-md) gr:*stack-pointer* gr:*stack-pointer*)
   (alu r-1 gr:*stack-pointer* gr:*stack-pointer* gr:*stack-pointer* boxed-right)
   (alu r-1 gr:*arg-1* gr:*arg-1* gr:*arg-1* dt-both-fixnum-with-overflow boxed-right)
   (move O0 md boxed-right ch-open)
 cons-it
   (call (cons:cons 2) gr:*value-1* (O1 gr:*value-1*))
 next
   (alu l-r nop gr:*arg-2* gr:*arg-1* bw-24 dt-both-fixnum boxed-right)
   (movei r0 '16. br-less-than boxed)
   (branch loop (alu l-r nop gr:*arg-1* r0 bw-24 dt-both-fixnum boxed-right))
 done
   (alu l+r nop gr:*return-pc-1* gr:*dtp-code-1* bw-24 boxed)
   (nop)
   (nop next-pc-dispatch)
   )




;;;; Keyword args

;;; GET-KEYWORD-ARG-VALUES is called by compiled code
;;; to parse keywords from a rest arg.  It takes the
;;; the rest arg, a list of the keywords, and
;;; a flag saying whether to allow other keywords.
;;; It returns as multiple values the keyword values.
(defun get-keyword-arg-values (args keywords allow-other-keys-p)
  (unless allow-other-keys-p
    (do ((args args (cddr args)))
        ((null args))
      (when (eq (car args) :allow-other-keys)
        (setq allow-other-keys-p (cadr args))
        (return))))
  (let (first-value)
    (do ((keywords keywords (cdr keywords))
         (i 0 (1+ i)))
        ((null keywords) (setq gr:*number-of-return-values* (1+ i)))
      (let ((keyword (car keywords)))
        (let ((value (do ((args args (cddr args)))
                         ((null args) 'keyword-garbage)
                       (when (eq (car args) keyword)
                         (return (cadr args))))))
          (if (zerop i)
              (setq first-value value)
            (put-in-return-register i value)))))
    (hw:return-mv first-value)))

;;; Put value in the nth return value register.
;;; This is 1 based because the 0th arg in returned
;;; to the return-dest.
;;; *** do some error checking??? ***
(defafun put-in-return-register (n value)
   (move-pc r0 dispatch boxed)
   (alu l+r r0 a0 r0 bw-24 boxed)
   (nop)
 dispatch
   (nop next-pc-dispatch)
   (unconditional-branch done (move GR:*RETURN-0* a1))
   (unconditional-branch done (move GR:*RETURN-1* a1))
   (unconditional-branch done (move GR:*RETURN-2* a1))
   (unconditional-branch done (move GR:*RETURN-3* a1))
   (unconditional-branch done (move GR:*RETURN-4* a1))
   (unconditional-branch done (move GR:*RETURN-5* a1))
   (unconditional-branch done (move GR:*RETURN-6* a1))
   (unconditional-branch done (move GR:*RETURN-7* a1))
   (unconditional-branch done (move GR:*RETURN-8* a1))
   (unconditional-branch done (move GR:*RETURN-9* a1))
   (unconditional-branch done (move GR:*RETURN-10* a1))
   (unconditional-branch done (move GR:*RETURN-11* a1))
   (unconditional-branch done (move GR:*RETURN-12* a1))
   (unconditional-branch done (move GR:*RETURN-13* a1))
   (unconditional-branch done (move GR:*RETURN-14* a1))
   (unconditional-branch done (move GR:*RETURN-15* a1))
   (unconditional-branch done (move GR:*RETURN-16* a1))
   (unconditional-branch done (move GR:*RETURN-17* a1))
   (unconditional-branch done (move GR:*RETURN-18* a1))
   (unconditional-branch done (move GR:*RETURN-19* a1))
   (unconditional-branch done (move GR:*RETURN-20* a1))
   (unconditional-branch done (move GR:*RETURN-21* a1))
   (unconditional-branch done (move GR:*RETURN-22* a1))
   (unconditional-branch done (move GR:*RETURN-23* a1))
   (unconditional-branch done (move GR:*RETURN-24* a1))
   (unconditional-branch done (move GR:*RETURN-25* a1))
   (unconditional-branch done (move GR:*RETURN-26* a1))
   (unconditional-branch done (move GR:*RETURN-27* a1))
   (unconditional-branch done (move GR:*RETURN-28* a1))
   (unconditional-branch done (move GR:*RETURN-29* a1))
 done
   (return a1 boxed-right))



;;;; Interpreter closures



;;;; Closures

;;; A contour is one frame of an environment
;;; An environment is a chain of contours

(defmacro contour-link (contour)
  `(cons:contents-offset ,contour 1))

(defsetf contour-link (contour) (value)
  `(cons:store-contents-offset ,contour 1 ,value))

(defmacro contour-slot (contour offset)
  `(cons:contents-offset ,contour ,offset))

(defsetf contour-slot (contour offset) (value)
  `(cons:store-contents-offset ,contour ,offset ,value))

(defvar *make-contour-max* 5)
(defvar *make-contour* '(nil make-contour-1 make-contour-2  ;unfortunately, making this a vector
                         make-contour-3 make-contour-4))   ; breaks the hardebeck compilation of this.

(defun make-contour-1 (link v1)
  (let ((contour (array:make-vector 2)))
    (setf (contour-link contour) link)
    (setf (contour-slot contour 1) v1)
    contour))

(defun make-contour-2 (link v1 v2)
  (let ((contour (array:make-vector 3)))
    (setf (contour-link contour) link)
    (setf (contour-slot contour 1) v1)
    (setf (contour-slot contour 2) v2)
    contour))

(defun make-contour-3 (link v1 v2 v3)
  (let ((contour (array:make-vector 4)))
    (setf (contour-link contour) link)
    (setf (contour-slot contour 1) v1)
    (setf (contour-slot contour 2) v2)
    (setf (contour-slot contour 3) v3)
    contour))

(defun make-contour-4 (link v1 v2 v3 v4)
  (let ((contour (array:make-vector 5)))
    (setf (contour-link contour) link)
    (setf (contour-slot contour 1) v1)
    (setf (contour-slot contour 2) v2)
    (setf (contour-slot contour 3) v3)
    (setf (contour-slot contour 4) v4)
    contour))

;;; SIZE is number of slots + 1
(defun make-contour (link size)
  (let ((contour (array:make-vector size)))
    (setq gr:*value-1* contour)
    (setf (contour-link contour) link)
    contour))

(defun set-in-new-contour (offset value)
  (cons:store-contents-offset gr:*value-1* offset value)
  ;(setf (contour-slot gr:*value-1* offset) value)
  )

;;; A lexical closure is a pair of an environment and a function

(defun closure-environment (closure)
  (cons:make-pointer vinc:$$dtp-array
                     (cons:contents closure)))

(defmacro closure-function (closure)
  `(cons:contents-offset ,closure 1))

(defsetf closure-function (closure) (function)
  `(cons:store-contents-offset ,closure 1 ,function))

(defun make-closure-with-env (function env)
  (cons:make-pointer vinc:$$dtp-lexical-closure
                     (cons:cons env function)))

(defun make-closure-1 (function link v1)
  (let ((env (array:make-vector 2)))
    (setf (contour-link env) link)
    (setf (contour-slot env 1) v1)
    (make-closure-with-env function env)))

(defun make-closure-2 (function link v1 v2)
  (let ((env (array:make-vector 3)))
    (setf (contour-link env) link)
    (setf (contour-slot env 1) v1)
    (setf (contour-slot env 2) v2)
    (make-closure-with-env function env)))

(defun make-closure-3 (function link v1 v2 v3)
  (let ((env (array:make-vector 4)))
    (setf (contour-link env) link)
    (setf (contour-slot env 1) v1)
    (setf (contour-slot env 2) v2)
    (setf (contour-slot env 3) v3)
    (make-closure-with-env function env)))

(defun make-closure-4 (function link v1 v2 v3 v4)
  (let ((env (array:make-vector 5)))
    (setf (contour-link env) link)
    (setf (contour-slot env 1) v1)
    (setf (contour-slot env 2) v2)
    (setf (contour-slot env 3) v3)
    (setf (contour-slot env 3) v4)
    (make-closure-with-env function env)))

(defun make-closure (function link size)
  (let ((env (array:make-vector size)))
    (setf (contour-link env) link)
    (setq gr:*value-1* env)
    (make-closure-with-env function env)))

;;; I think there should be a few more of these... --RWK

(defun closure-ref-0-1 (env)
  (cons:contents-offset env 1))

(defun closure-ref-0-2 (env)
  (cons:contents-offset env 2))

(defun closure-ref-0-3 (env)
  (cons:contents-offset env 3))

(defun closure-ref-0-4 (env)
  (cons:contents-offset env 4))

(defvar *closure-ref-0-max* 4)

(defvar *closure-ref-0* '(closure-ref-0-1 closure-ref-0-2       ;must be list, see above
                          closure-ref-0-3 closure-ref-0-4))

(defun closure-ref-0 (env offset)
  (cons:contents-offset env offset))

(defun closure-ref (env nback offset)
  (dotimes (i nback)
    (setq env (contour-link env)))
  (cons:contents-offset env offset))

(defun closure-set-0-1 (env value)
  (cons:store-contents-offset env 1 value))

(defun closure-set-0-2 (env value)
  (cons:store-contents-offset env 2 value))

(defun closure-set-0-3 (env value)
  (cons:store-contents-offset env 3 value))

(defun closure-set-0-4 (env value)
  (cons:store-contents-offset env 4 value))

(defvar *closure-set-0-max* 4)

(defvar *closure-set-0* '(closure-set-0-1 closure-set-0-2               ;must be list, see above
                          closure-set-0-3 closure-set-0-4))

(defun closure-set-0 (env offset value)
  (cons:store-contents-offset env offset value))

(defun closure-set (env nback offset value)
  (dotimes (i nback)
    (setq env (contour-link env)))
  (cons:store-contents-offset env offset value))

(defsubst lexical-closure-p (p)
  (vinc:data-type= p (hw:dpb-unboxed vinc:$$dtp-lexical-closure vinc:%%data-type (hw:unboxed-constant 0))))

;;; FUNCALL - the compiler puts the function to be called in *ARG-2*
;;;                             the argument count is in     *ARG-1*
;;; all of the actual arguments are passed the usual way
;;;
(defun funcall-internal (&rest ignore)
 ;NOTE: cant use any temporaries in below code because it would clobber args.
 (do () (())
  (cond
    ((compiled-function-p gr:*arg-2*)
     (setq gr:*arg-2* (k2:get-entry-address-for-funcall gr:*arg-2* gr:*arg-1*))
     (if (or (= gr:*return-0* gr:*arg-1*)
             (and (minusp gr:*return-0*) (>= gr:*return-0* (- -1 gr:*return-0*))))
         (hw:dispatch gr:*arg-2*)
       (li:error "Bad number of arguments to function during FUNCALL")))
    ((lexical-closure-p gr:*arg-2*)
     (when (> gr:*arg-1* 15)
       (li:error "Can't funcall-internal lexical-clousure with 16 or more arguments"
                 gr:*arg-1*))
     (setf (hw:a15) (closure-environment gr:*arg-2*))
     (setq gr:*arg-2* (closure-function gr:*arg-2*)))
    ((symbolp gr:*arg-2*)
     (unless (fboundp gr:*arg-2*)
       (error "FUNCALL-INTERNAL: this symbol has no function definition !!" gr:*arg-2*))
     (setq gr:*arg-2* (symbol:symbol-function gr:*arg-2*)))
    ((interpreter-closure-p gr:*arg-2*)
     (setq gr:*return-0* gr:*arg-2*)
     (setq gr:*arg-2* gr:*arg-1*)
     (hw:jump-saving-pc 'cons-rest)
     (li:error "This code does not work properly, look at *value-1*. wkf")
     (return-from funcall-internal (apply-interpreter-closure gr:*return-0* gr:*value-1*)))
    ((consp gr:*arg-2*)
     (li:error "FUNCALL can't handle lambda lists or macros yet." gr:*arg-2*))
    ;;The array case added by wkf on 4/13/88.
    ((arrayp gr:*arg-2*)
     (cond ((array:named-structure-p gr:*arg-2*)
; would like to write it like this.  Unfortunately, that results in code that clobbers A0.
;           (setq gr:*arg-2*
;                 (cond ((get (array:named-structure-symbol gr:*arg-2*) 'global:named-structure-invoke))
;                       (t (li:error "Named structure symbol for ~S has no named-structure-invoke handler" gr:*arg-2*))))
    ;so write it like this instead.
            (cond ((get (array:named-structure-symbol gr:*arg-2*) 'global:named-structure-invoke)
                   (when (> gr:*arg-1* 14)
                     (li:error "Can't funcall-internal a named-structure with 15 or more args."
                               gr:*arg-1*))
                   (setf (hw:a14) (hw:a13))
                   (setf (hw:a13) (hw:a12))
                   (setf (hw:a12) (hw:a11))  ;; Here we are shifting the arguments to the
                   (setf (hw:a11) (hw:a10))  ;; funcall up one register and inserting self
                   (setf (hw:a10) (hw:a9 ))  ;; as the second argument.  The first argument
                   (setf (hw:a9 ) (hw:a8 ))  ;; is the message being sent to self.
                   (setf (hw:a8 ) (hw:a7 ))
                   (setf (hw:a7 ) (hw:a6 ))
                   (setf (hw:a6 ) (hw:a5 ))
                   (setf (hw:a5 ) (hw:a4 ))
                   (setf (hw:a4 ) (hw:a3 ))
                   (setf (hw:a3 ) (hw:a2 ))
                   (setf (hw:a2 ) (hw:a1 ))
                   (setf (hw:a1 ) gr:*arg-2*)
                   (setq gr:*arg-2*
                         (get (array:named-structure-symbol gr:*arg-2*) 'global:named-structure-invoke)
                         gr:*arg-1* (1+ gr:*arg-1*)))


                  (t (li:error "Named structure symbol for ~S has no named-structure-invoke handler" gr:*arg-2*)))
            )
           ((not (= (array:array-rank gr:*arg-2*) gr:*arg-1*))
            (li:error "Wrong number of dimensions" gr:*arg-2*))
           ((= 1 (array:array-rank gr:*arg-2*))
            (return-from funcall-internal (array:aref gr:*arg-2* (hw:a0))))
           ((= 2 (array:array-rank gr:*arg-2*))
            (return-from funcall-internal (array:aref gr:*arg-2* (hw:a0) (hw:a1))))
           ((= 3 (array:array-rank gr:*arg-2*))
            (return-from funcall-internal (array:aref gr:*arg-2* (hw:a0) (hw:a1) (hw:a2))))
           (t (li:error "4 or more dimensions not supported" gr:*arg-2*))))
    (t (li:error "You can't FUNCALL that." gr:*arg-2*)))))

(defun funcall (function &rest arglist)
  (setq gr:*value-1* arglist)
  (setq gr:*arg-1* 0)
  (setq gr:*arg-2* function)
  (apply-internal))

;;; APPLY-INTERNAL get its arguments passed in a funny way
;;;      *ARG-1*   contains the count of arguments to apply (excluding the last)
;;;      *ARG-2*   contains the function to be applied
;;;      *VALUE-1* contains the last argument to apply (The list)
(defun apply-internal (&rest ignore)
  (do () (())
    (cond
      ((compiled-function-p gr:*arg-2*)
       (setq gr:*arg-2* (apply-arg-scan gr:*arg-2* gr:*arg-1* gr:*value-1*))
       (if (minusp gr:*return-1*)
           (do () (()) ; spread some arguments
             (when (zerop gr:*return-1*) (hw:dispatch gr:*arg-2*))
             (setq gr:*return-1* (1+ gr:*return-1*))
             (setq gr:*value-2* (car gr:*value-1*))
             (setq gr:*value-1* (cdr gr:*value-1*))
             (if (>= gr:*return-0* 16.)
                 (progn
                   (setq gr:*stack-pointer* (hw:24+ (hw:unboxed-constant 1) gr:*stack-pointer*))
                   (hw:write-md-boxed gr:*value-2*)
                   (hw:vma-start-write-boxed (hw:24+ (hw:unboxed-constant #xffffff) gr:*stack-pointer*)))
               (dispatch (byte 4 0) gr:*return-0*
                 (0.  (setf (hw:a0)  gr:*value-2*)) (1.  (setf (hw:a1)  gr:*value-2*))
                 (2.  (setf (hw:a2)  gr:*value-2*)) (3.  (setf (hw:a3)  gr:*value-2*))
                 (4.  (setf (hw:a4)  gr:*value-2*)) (5.  (setf (hw:a5)  gr:*value-2*))
                 (6.  (setf (hw:a6)  gr:*value-2*)) (7.  (setf (hw:a7)  gr:*value-2*))
                 (8.  (setf (hw:a8)  gr:*value-2*)) (9.  (setf (hw:a9)  gr:*value-2*))
                 (10. (setf (hw:a10) gr:*value-2*)) (11. (setf (hw:a11) gr:*value-2*))
                 (12. (setf (hw:a12) gr:*value-2*)) (13. (setf (hw:a13) gr:*value-2*))
                 (14. (setf (hw:a14) gr:*value-2*)) (15. (setf (hw:a15) gr:*value-2*))))
             (setq gr:*return-0* (1+ gr:*return-0*)))
           (do () (()) ;cons some args
             (when (zerop gr:*return-1*) (hw:dispatch gr:*arg-2*))
             (setq gr:*return-0* (1- gr:*return-0*))
             (setq gr:*return-1* (1- gr:*return-1*))
             (if (>= gr:*return-0* 16.)
                 (progn
                   (hw:vma-start-read-vma-boxed-md-boxed (hw:24+ (hw:unboxed-constant #xffffff) gr:*stack-pointer*))
                   (setq gr:*value-1* (cons (hw:read-md) gr:*value-1*))
                   (setq gr:*stack-pointer* (hw:24+ (hw:unboxed-constant #xffffff) gr:*stack-pointer*)))
               (progn
                 (setq gr:*value-1*
                       (cons (dispatch (byte 4 0) gr:*return-0*
                               (0.  (hw:a0)) (1.  (hw:a1)) (2.  (hw:a2)) (3.  (hw:a3))
                               (4.  (hw:a4)) (5.  (hw:a5)) (6.  (hw:a6)) (7.  (hw:a7))
                               (8.  (hw:a8)) (9.  (hw:a9)) (10. (hw:a10)) (11. (hw:a11))
                               (12. (hw:a12)) (13. (hw:a13)) (14. (hw:a14)) (15. (hw:a15)))
                             gr:*value-1*)))))))
      ((lexical-closure-p gr:*arg-2*)
       (when (> gr:*arg-1* 15)
         (li:error "Can't apply-internal lexical-clousure with 16 or more arguments"
                   gr:*arg-1*))
       (setf (hw:a15) (closure-environment gr:*arg-2*))
       (setq gr:*arg-2* (closure-function gr:*arg-2*)))
      ((symbolp gr:*arg-2*)
       (setq gr:*arg-2* (symbol:symbol-function gr:*arg-2*)))  ;;wkf check for bound

;      ((interpreter-closure-p gr:*arg-2*)
;       (do () (()) ;cons some args
;        (when (minusp gr:*arg-1*) (return))
;        (setq gr:*arg-1* (1- gr:*arg-1*))
;        (if (>= gr:*arg-1* 16.)
;            (progn
;              (hw:vma-start-read-vma-boxed-md-boxed (hw:24+ (hw:unboxed-constant #xffffff) gr:*stack-pointer*))
;              (setq gr:*value-1* (cons (hw:read-md) gr:*value-1*))
;              (setq gr:*stack-pointer* (hw:24+ (hw:unboxed-constant #xffffff) gr:*stack-pointer*)))
;            (progn
;              (setq gr:*value-1*
;                    (cons (dispatch (byte 4 0) gr:*arg-1*
;                                    (0.  (hw:a0)) (1.  (hw:a1)) (2.  (hw:a2)) (3.  (hw:a3))
;                                    (4.  (hw:a4)) (5.  (hw:a5)) (6.  (hw:a6)) (7.  (hw:a7))
;                                    (8.  (hw:a8)) (9.  (hw:a9)) (10. (hw:a10)) (11. (hw:a11))
;                                    (12. (hw:a12)) (13. (hw:a13)) (14. (hw:a14)) (15. (hw:a15)))
;                          gr:*value-1*)))))
;       (return-from apply-internal (apply-interpreter-closure gr:*arg-2* gr:*value-1*)))

      ((interpreter-closure-p gr:*arg-2*)
       (if (= gr:*arg-1* 0)
           (return-from apply-internal (apply-interpreter-closure gr:*arg-2* gr:*value-1*))
           (error "Funky interpreter closure case not yet handled in APPLY")))

      ((consp gr:*arg-2*)
       (li:error "APPLY can't handle lambda lists or macros yet." gr:*arg-2*))
      ;;The array case added by wkf on 4/13/88.
      ((arrayp gr:*arg-2*)
       (if (array:named-structure-p gr:*arg-2*)
           (cond ((get (array:named-structure-symbol gr:*arg-2*) 'global:named-structure-invoke)
                  (cond ((= 0 gr:*arg-1*)
                         (setq gr:*value-1*
                               (cons (car gr:*value-1*)
                                     (cons gr:*arg-2* (cdr gr:*value-1*)))))
                        ((= 1 gr:*arg-1*)
                         (setq gr:*value-1* (cons gr:*arg-2* gr:*value-1*)))
                        ((< gr:*arg-1* 15)
                         (setf (hw:a14) (hw:a13))
                         (setf (hw:a13) (hw:a12))
                         (setf (hw:a12) (hw:a11))  ;; Here we are shifting the arguments to the
                         (setf (hw:a11) (hw:a10))  ;; funcall up one register and inserting self
                         (setf (hw:a10) (hw:a9 ))  ;; as the second argument.  The first argument
                         (setf (hw:a9 ) (hw:a8 ))  ;; is the message being sent to self.
                         (setf (hw:a8 ) (hw:a7 ))
                         (setf (hw:a7 ) (hw:a6 ))
                         (setf (hw:a6 ) (hw:a5 ))
                         (setf (hw:a5 ) (hw:a4 ))
                         (setf (hw:a4 ) (hw:a3 ))
                         (setf (hw:a3 ) (hw:a2 ))
                         (setf (hw:a2 ) (hw:a1 ))
                         (setf (hw:a1 ) gr:*arg-2*))
                        (t (li:error "Can not apply named-structure-symbol with 15 or more args"
                                     gr:*arg-1*)))
                  (setq gr:*arg-2*
                        (get (array:named-structure-symbol gr:*arg-2*) 'global:named-structure-invoke)
                        gr:*arg-1* (1+ gr:*arg-1*)))
                 (t (li:error "Named structure symbol for ~S has no named-structure-invoke handler" gr:*arg-2*)))
         (cond
           ((= 1 (array:array-rank gr:*arg-2*))
            (return-from apply-internal
              (array:aref gr:*arg-2*
                          (cond ((= 0 gr:*arg-1*)
                                 (car gr:*value-1*))
                                (t (hw:a0))))))
           (t (li:error "2 or more dimensions not supported in apply-internal" gr:*arg-2*)))
         ))
      (t (li:error "You can't APPLY that." gr:*arg-2*)))))

;;; APPLY-ARG-SCAN figures out the total number of arguments in the apply
;;;  and returns information about how to either spread or cons the arguments.
;;;  the total argument count is stored in gr:*ARG-1* for the dispatch later.
;;;      (values entry-address argument-index arg-adjust-count)
;;;   ENTRY-ADDRESS is the PC to dispatch to. If there are rest args, then it is two past the normal entry.
;;;   ARGUMENT-INDEX is the argument number to start adjusting from
;;;   ARG-ADJUST-INDEX is the number of arguments to be adjusted and the direction
;;;         Positive - CONS more args onto the &REST list, decrement the argument-index
;;;         Negative - spread more args into regs, shorten the &REST list, increment the argument-index

(defun apply-arg-scan (fcn pre-spread-nargs last-arg)
  (let* ((entry-points    (k2:%compiled-function-entry-points fcn))
         (start-pc        (k2:resurrect-function-if-dead fcn))
         (last-arg-length (cond
                            ((consp last-arg) (array:length last-arg))
                            ((null  last-arg) 0)
                            (t
                             (li:error "APPLY's last argument wasn't a list." last-arg))))
         (total-nargs     (+ last-arg-length pre-spread-nargs))
         (entry-length    (array:length entry-points)))
    (do ((i 0 (+ i 2)))
        ((>= i entry-length)
         (li:error "Bad number of arguments to function during APPLY" total-nargs fcn))
      (let ((entry-nargs (array:svref entry-points i)))
        (when (or (= entry-nargs total-nargs) (minusp entry-nargs))
          (let ((entry-offset (array:svref entry-points (1+ i))))
            (when (minusp entry-nargs)
              (setq entry-offset (+ entry-offset 2))
              (setq entry-nargs (- -1 entry-nargs)))
            (setq gr:*arg-1* total-nargs)
            (return (values
                      (hw:24+ entry-offset start-pc)
                      pre-spread-nargs
                      (- pre-spread-nargs entry-nargs)))))))))


;; this was losing... - JRR 12-Mar-88
;(defun apply (function arg &rest arg-list)
;  (when arg-list
;    (setq arg (cons arg nil))
;    (do ((args arg)
;        (arg-list arg-list (cdr arg-list)))
;       ((null (cdr arg-list)) (rplacd args (car arg-list)))
;      (rplacd args (cons (car arg-list) nil))))
;  (setq gr:*arg-1* 0)
;  (setq gr:*arg-2* function)
;  (setq gr:*value-1* arg)
;  (apply-internal))

(defun apply (function arg &rest more-args)
  (let ((arg-list (apply #'list* arg more-args)))
    (setq gr:*arg-1* 0)
    (setq gr:*arg-2* function)
    (setq gr:*value-1* arg-list)
    (apply-internal)))

;(defun apply-interpreter-closure (closure args)
;  (li:error "apply-interpreter-closure is not defined yet"))

;(defun interpreter-closure-p (thing)
;  nil)
