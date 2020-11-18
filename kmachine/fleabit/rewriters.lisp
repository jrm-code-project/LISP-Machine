;;; -*- Mode:Lisp; Package:(NC LISP); Readtable:CL; Base:10 -*-

;;;; Rewriters

(defrewrite not (x)
  `(if ,x nil t))


(defmacro def-n-arg-fcn (name rest 2-arg-fcn 0-arg-form 1-arg-form)
  `(defrewrite ,name (&rest ,rest)
     (cond ((null ,rest) ,0-arg-form)
           ((null (cdr ,rest)) ,1-arg-form)
           (t
            (do ((args (cdr ,rest) (cdr args))
                 (form (car ,rest)
                       `(,',2-arg-fcn ,form ,(car args))))
                ((null args) form))))))

(def-n-arg-fcn + numbers
  2-arg-+
  0 (car numbers))

(def-n-arg-fcn - numbers
  2-arg--
  `(error "0 arg minus")
  `(2-arg-- 0 ,(car numbers)))

(def-n-arg-fcn * numbers
  new-math::multiply-generic
  1
  (car numbers))

(def-n-arg-fcn / numbers
  new-math::divide-generic
  `(error "0 arg divide")
  `(new-math::divide-generic 1 (car numbers)))

(def-n-arg-fcn logior numbers
  2-arg-logior
  0
  (car numbers))

(def-n-arg-fcn logand numbers
  2-arg-logand
  -1
  (car numbers))

(def-n-arg-fcn logxor numbers
  2-arg-logxor
  0
  (car numbers))

(def-n-arg-fcn logeqv numbers
  2-arg-logxnor
  -1
  (car numbers))

(defrewrite lognot (x)
  `(logxor -1 ,x))

(defrewrite lognand (x y)
  `(lognot (logand ,x ,y)))

(defrewrite lognor (x y)
  `(lognot (logior ,x ,y)))

(defrewrite logandc1 (x y)
  `(logand (lognot ,x) ,y))

(defrewrite logandc2 (x y)
  `(logand ,x (lognot ,y)))

(defrewrite logorc1 (x y)
  `(logior (lognot ,x) ,y))

(defrewrite logorc2 (x y)
  `(logior ,x (lognot ,y)))



(defmacro def-n-arg-compare (name rest 2-arg)
  `(defrewrite ,name (&rest ,rest)
     `(and ,@(maplist #'(lambda (l)
                            (if (null (cdr l))
                                't
                              `(,',2-arg ,(car l) ,(cadr l))))
                      ,rest))))

;;; NOTE:  Because of Common LISP brain dammage and ambiguity the = functions
;;; might need to be full tree-wise instead of pair-wise.  This is because
;;; of complications involving rationals and different sizes of floats.
(def-n-arg-compare = numbers 2-arg-=)
(def-n-arg-compare < numbers 2-arg-<)
(def-n-arg-compare > numbers 2-arg->)
(def-n-arg-compare <= numbers 2-arg-<=)
(def-n-arg-compare >= numbers 2-arg->=)

;;; /= is "all different"
(defrewrite /= (&rest numbers)
  `(AND ,@(mapcon #'(lambda (args)
                        (if (null (cdr args))
                            (list 'T)
                          (let ((arg (car args)))
                            (mapcar #'(lambda (next-arg)
                                        `(NOT (= ,arg ,next-arg)))
                                    (cdr args)))))
                   numbers)))


; currently constant byte specs are lambda byte specs
; computed byte specs are K byte specs
;(defrewrite byte (width position)
;  `(hw:dpb ,width vinc:%%byte-size ,position))

;;; $$$ This byte-size rewriter was not being used (since (not (eq 'nc:byte-size 'li:byte-size)))
;;;     If it was being used I believe it is incorrect. <21-Nov-88 wkf>
#+Removed
(defrewrite byte-size (byte-spec)
  (setq byte-spec (nlisp:macroexpand byte-spec))
  (if (constantp byte-spec)
      (byte-size (eval byte-spec))
    `(hw:ldb ,byte-spec system:%%byte-specifier-position #|vinc:%%byte-position|# ,byte-spec)))  ;once only


;;; #'(lambda ... would be taken care of by simplify-funcall anyway
(defrewrite funcall (fn &rest args)
  (if (and (consp fn)
           (eq (car fn) 'FUNCTION))
      `(,(cadr fn) . ,args)
    `(nc:funcall-internal ,fn . ,args)))        ; $$$ Funcall-internal, a primop. <21-Nov-88 wkf>

(defrewrite li:apply (function arg &rest more-args)
  `(nc:apply-internal ,function ,arg . ,more-args))     ; $$$ Apply-internal, a primop. <21-Nov-88 wkf>


;(ndefmacro mapcar (fn &rest lists)
;  (let ((map-name (gensym 'map))
;       (call-fn (cond ((listp fn)
;                       (cond ((eq (car fn) 'lambda) (list fn)) ;????
;                             ((eq (car fn) 'function) (list (cadr fn)))
;                             (t (list 'funcall fn))))
;                      (t (list 'funcall fn))))
;       (vars '()))
;    (do ((l lists (cdr lists)))
;                 ((null l))
;               (push (gensym 'v) vars))
;  ;; Cons up result
;  (let ((map-result (gensym 'res))
;       (map-temp (gensym 'temp))
;       (map-val (gensym 'val)))
;    `(let ((,map-result)
;          (,map-temp))
;       (labels ((,map-name ,vars
;                 (if (not (or ,@(mapcar #'(lambda (var) `(null ,var))   ;atom?
;                                        vars)))
;                     (progn
;                       (let ((,map-val (ncons (,@call-fn . ,(mapcar #'(lambda (var)
;                                                                        `(car ,var))
;                                                                    vars)))))
;                         (if ,map-temp
;                             (rplacd ,map-temp ,map-val)
;                           (setq ,map-result ,map-val))
;                         (setq ,map-temp ,map-val))
;                       (,map-name ,@(mapcar #'(lambda (var) `(cdr ,var)) vars))))))
;        (,map-name . ,lists))
;       ,map-result))))


;;; $$$ Fixed K references (CAR and CDR ) <21-Nov-88 wkf>
(defrewrite li:mapc (fn &rest lists)
  (let ((vars (mapcar #'(lambda (l) (declare (ignore l))
                                (gensym 'v))
                      lists)))
    `(DO ,(mapcar #'(lambda (var list) `(,var ,list (li:CDR ,var)))
                  vars
                  lists)
         ((OR ,@(mapcar #'(lambda (var) `(NULL ,var))
                        vars)))
       (FUNCALL ,fn ,@(mapcar #'(lambda (var)
                                  `(li:CAR ,var))
                              vars)))))


(defrewrite li:mapcar (fn &rest lists)
  (let ((vars (mapcar #'(lambda (l) (declare (ignore l))
                                (gensym 'v))
                      lists))
        (map-result (gensym 'res))
        (map-temp (gensym 'temp))
        (map-val (gensym 'val)))
    `(DO (,@(mapcar #'(lambda (var list) `(,var ,list (LI:CDR ,var)))
                    vars
                    lists)
          ,map-result
          ,map-temp)
         ((OR ,@(mapcar #'(lambda (var) `(NULL ,var))
                        vars))
          ,map-result)
       (LET ((,map-val (LI:CONS (FUNCALL ,fn ,@(mapcar #'(lambda (var)
                                                         `(LI:CAR ,var))
                                                     vars))
                                NIL)))
         (IF ,map-temp
             (LI:RPLACD ,map-temp ,map-val)
           (SETQ ,map-result ,map-val))
         (SETQ ,map-temp ,map-val)))))


(defrewrite li:list (&rest elements)
  (case (length elements)
    (0 'nil)
    ;; @@@ Could be done as an ncons <21-Nov-88 wkf>
    (1 (cons 'li:cons `(,(car elements) nil)))
    (2 (cons 'li:list2 elements))
    (3 (cons 'li:list3 elements))
    (4 (cons 'li:list4 elements))
    (t (cons 'li:listn elements))))


(defrewrite li:char (string index)
  `(li:svref ,string ,index))

(setf:defsetf li:char array:svset)

(defrewrite li:typep (&whole form)
  (li:rewrite-typep form)) ;;+++ When this doesn't resolve to anything need to call nlisp:typep.  --wkf


(def-n-arg-compare li:char<  characters li:%char< )
(def-n-arg-compare li:char<= characters li:%char<=)
(def-n-arg-compare li:char=  characters li:%char= )
(def-n-arg-compare li:char>= characters li:%char>=)
(def-n-arg-compare li:char>  characters li:%char> )

(defrewrite li:CHAR/= (&rest characters)
  `(AND ,@(mapcon #'(lambda (args)
                      (if (null (cdr args))
                          (list 'T)
                        (let ((arg (car args)))
                          (mapcar #'(lambda (next-arg)
                                      `(li:%char= ,arg ,next-arg))
                                  (cdr args)))))
                  characters)))

;;; CHAR-EQUAL and its brothers are too hairy to open-code.  What we do here is just try
;;; to avoid the expense of &REST arguments for the common cases of these functions.

(defmacro define-char-????-predicate (name 2-arg-function 3-arg-function)
  `(defrewrite ,name (arg1 &optional (arg2 nil arg2-p) (arg3 nil arg3-p) &rest args)
     (cond (args                                ; are there more than three args?
            `(,',name ,arg1 ,arg2 ,arg3 ,@args))
           (arg3-p                              ; three args?
            `(,',3-arg-function ,arg1 ,arg2 ,arg3))
           (arg2-p                              ; two args?
            `(,',2-arg-function ,arg1 ,arg2))
           (t 't))))                            ; one arg?

(define-char-????-predicate li:char-equal        li:char-equal-2-args        li:char-equal-3-args)
(define-char-????-predicate li:char-not-equal    li:char-not-equal-2-args    li:char-not-equal-3-args)
(define-char-????-predicate li:char-lessp        li:char-lessp-2-args        li:char-lessp-3-args)
(define-char-????-predicate li:char-greaterp     li:char-greaterp-2-args     li:char-greaterp-3-args)
(define-char-????-predicate li:char-not-lessp    li:char-not-lessp-2-args    li:char-not-lessp-3-args)
(define-char-????-predicate li:char-not-greaterp li:char-not-greaterp-2-args li:char-not-greaterp-3-args)


;;; VINC:TYPE-TEST through LI:DOUBLE-FLOAT-P used to be in "K; TYPE-PREDICATES"

(ndefmacro vinc:type-test (object type)
  `(HW:FIELD= ,object
              (HW:DPB-UNBOXED ,type vinc:%%data-type 0)
              VINC:%%DATA-TYPE))

(defrewrite prims:null (x)
  `(if ,x nil t))

(defrewrite vinc:consp (object)
  `(VINC:TYPE-TEST ,object VINC:$$DTP-CONS))

(defrewrite vinc:atom (object)
  `(NOT (vinc:CONSP ,object)))

(defrewrite vinc:complexp (object)
  `(VINC:TYPE-TEST ,object VINC:$$DTP-COMPLEX))

(defrewrite vinc:characterp (object)
  `(VINC:TYPE-TEST ,object VINC:$$DTP-CHARACTER))

(defrewrite vinc:arrayp (object)
  `(HW:FIELD= GR:*RANDOM-ARRAY* ,object VINC:%%DATA-TYPE))

(defrewrite vinc:compiled-function-p (object)
  `(VINC:TYPE-TEST ,object VINC:$$DTP-COMPILED-FUNCTION))

(defrewrite vinc:make-fixnum (object) "Puts fixnum type code into object and set Zero and Negative status bits accordingly."
  `(hw:merge-24 gr:*zero* ,object)  ;;||| Chaned 10/13/88 --wkf
  #+Does-not-set-status-bits-for-negative`(HW:DPB-BOXED VINC:$$DTP-FIXNUM VINC::%%DATA-TYPE ,object)
  )

#+Not-currently-used
(defrewrite vinc:make-short-float (object) "Puts short type code on top of object word"
  `(HW:DPB-BOXED VINC:$$DTP-SHORT-FLOAT VINC::%%DATA-TYPE ,object)) ;;||| Added 10/13/88 --wkf

;;; These are not Common Lisp but we find them useful
;;; TYPEP expands into them

(defrewrite vinc:fixnump (object)
  `(HW:FIELD= GR:*ZERO* ,object VINC:%%DATA-TYPE))

(defrewrite vinc:bignump (object)
  `(VINC:TYPE-TEST ,object VINC:$$DTP-BIGNUM))

(defrewrite vinc:short-float-p (object)
  `(VINC:TYPE-TEST ,object VINC:$$DTP-SHORT-FLOAT))

(defrewrite vinc:single-float-p (object)
  `(VINC:TYPE-TEST ,object VINC:$$DTP-SINGLE-FLOAT))

(defrewrite vinc:long-float-p (object)
  `(VINC:TYPE-TEST ,object VINC:$$DTP-DOUBLE-FLOAT))

(defrewrite vinc:double-float-p (object)
  `(VINC:TYPE-TEST ,object VINC:$$DTP-DOUBLE-FLOAT))

(defrewrite vinc:structure-p (object)
  `(HW:FIELD= GR:*RANDOM-STRUCTURE* ,object VINC:%%DATA-TYPE))

(defrewrite vinc:instance-p (object)
  `(VINC:TYPE-TEST ,object VINC:$$DTP-INSTANCE))

(defrewrite li:make-string (&whole form length &key (initial-element nil init-p))
  (if (> (length form) 2)
      (if init-p
          `(li:make-string-with-init ,length ,initial-element)
        form)
    `(array::make-string-no-init ,length)))

;;; $$$Added defrewrites that were sprinkled through the system. <18-Nov-88 JIM>

;;; $$$ arithmetic.lisp <18-Nov-88 JIM>

li::
(defrewrite max (&whole form)
  (let ((length-form (length form)))
    (cond ((= length-form 2)
           (second form))
          ((= length-form 3)
           `(MAX-2 . ,(cdr form)))
          (t form)))
  )

li::
(defrewrite min (&whole form)
  (let ((length-form (length form)))
    (cond ((= length-form 2)
           (second form))
          ((= length-form 3)
           `(MIN-2 . ,(cdr form)))
          (t form))))  ;;@@@ Is it worth saving consing with a min/max-3+ ???  --wkf

;;; $$$ array2.lisp <18-Nov-88 JIM>

array::
(defrewrite aref (&whole form array &rest subscripts)
  (lisp:case (lisp:length subscripts)
    (1 `(AREF-1  ,array . ,subscripts))
    (2 `(AREF-2  ,array . ,subscripts))
    (t form)))

array::
(defrewrite aset (&whole form value array &rest subscripts)
  (lisp:case (lisp:length subscripts)
    (1 `(ASET-1 ,value ,array . ,subscripts))
    (2 `(ASET-2 ,value ,array . ,subscripts))
    (t form)))

array::
(defrewrite make-array (&whole form dimensions &rest options &key (element-type t element-type-p))
  (cond
    ((null options)
     (cond ((numberp dimensions)
            `(MAKE-VECTOR ,dimensions))
           (t
            `(MAKE-EASY-ARRAY ,dimensions))))
    ((and element-type-p
          (null (lisp:cddr options)))
     (let ((type ;; do something clever with constantp here
             (or (eq element-type t)
                 (and (lisp:consp element-type)
                      (eq (lisp:car element-type) 'QUOTE)
                      (lisp:cadr element-type)))))
       (if type
           (let ((array-type (array-type-from-element-type type)))
             (if (numberp dimensions)
                 (cond ((= array-type art-q)
                        `(MAKE-VECTOR ,dimensions))
                       ((= array-type art-string)
                        `(MAKE-STRING ,dimensions))
                       (t
                        `(MAKE-1D-ARRAY ,dimensions ,array-type)))
               `(MAKE-EASY-ARRAY ,dimensions ,array-type)))
         `(MAKE-EASY-ARRAY-WITH-ELEMENT-TYPE ,dimensions ,element-type))))
    (t form)))
