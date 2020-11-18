

;; TYPEP of one arg

((not type-specified-p)
           (setq dtp (%data-type object))
           ;; Cannot use TYPE-OF, since we must
           ;; for back-compatibility return keywords.
           (cond ((eq dtp dtp-instance)
                  (%p-contents-offset
                    (instance-flavor object)
                    %instance-descriptor-typename))
                 ((eq dtp dtp-array-pointer)
                  (cond ((named-structure-p object))
                        ((stringp object) :string)
                        (t :array)))
;                ((eq dtp dtp-entity)
;                 (class-symbol object))
                 ((eq dtp dtp-extended-number)
                  (select (%p-ldb-offset %%header-type-field object 0)
                    (%header-type-flonum :flonum)
                    (%header-type-bignum :bignum)
                    (%header-type-rational :rational)
                    (%header-type-complex :complex)
                    (otherwise :random)))
                 ((cdr (assq dtp typep-one-arg-alist)))
                 (t :random)))





(define-type structure
  (:predicate (object) (not (null (named-structure-p object))))
  (:optimizer (expression)
             `(not (null (named-structure-p ,(cadr expression))))))
(define-type named-structure structure)

(define-type character
  (:predicate characterp)
  (:subtypes standard-char string-char fat-char))

(define-type fat-char
  (:predicate (object)
             (and (characterp object)
                  ( object 0)
                  (< object (lsh 1 #o20))))
  (:optimizer (expression)
             (setq expression (cadr expression))
             (once-only (expression)
               `(and ( ,expression 0)
                     (< ,expression (lsh 1 #o20)))))
  (:subtypes string-char standard-char))




(define-type non-complex-number
  (:predicate (object &optional (low '*) (high '*))
             (and (non-complex-number-p object)
                  (non-complex-number-in-range-p object low high 'non-complex-number)))
  (:optimizer (expression &optional (low '*) (high '*))
             (optimize-numeric-type-test 'non-complex-number-p expression low high))
  (:subtypep-predicate dense-arithmetic-subtypep)
  (:disjoint-typep-predicate real-disjoint-typep)
  (:canonicalizer canonicalize-real-type-specifier)
  (:subtypes rational integer fixnum ratio bignum float short-float single-float))


(define-type real
  (:predicate (object &optional (low '*) (high '*))
     (and (realp object)
          (setq object (realpart object))
          (non-complex-number-in-range-p object low high 'real)))
  (:optimizer (expression &optional (low '*) (high '*))
             (if (and (eq low '*) (eq high '*))
                 `(realp ,(cadr expression))
               (let ((object (cadr expression))
                     (o (gensym)))
                 `(let ((,o ,object))
                    (block real
                      (and (setq ,o (typecase ,o
                                      (complex (%complex-real-part ,o))
                                      (number ,o)
                                      (t (return-from real nil))))
                           ,(cond ((eq low '*)
                                   t)
                                  ((numberp low)
                                   `( ,o ,low))
                                  ((consp low)
                                   `(> ,o ,(car low))))
                           ,(cond ((eq high '*)
                                   t)
                                  ((numberp high)
                                   `( ,o ,high))
                                  ((consp high)
                                   `(< ,o ,(car high))))))))))
  (:subtypep-predicate dense-arithmetic-subtypep)
  (:disjoint-typep-predicate real-disjoint-typep)
  (:canonicalizer canonicalize-real-type-specifier)
  (:subtypes rational integer fixnum ratio bignum
            float short-float single-float non-complex-number))



;>>??
(define-type fix integer)

(define-type small-flonum short-float)
(define-type small-float short-float)
(define-type flonum single-float)


(define-type :array array)
(define-type :atom atom)
(define-type :bignum bignum)
(define-type :character cl:character)
(define-type :closure closure)
(define-type :compiled-function compiled-function)
(define-type :complex complex)
(define-type :cons cons)
(define-type :double-float double-float)
(define-type :entity entity)
(define-type :fix integer)
(define-type :fixnum fixnum)
(define-type :flonum single-float)
(define-type :float float)
(define-type :instance instance)
(define-type :integer integer)
(define-type :locative locative)
(define-type :long-float long-float)
(define-type :microcode-function microcode-function)
(define-type :named-structure structure)
(define-type :null null)
(define-type :number number)
(define-type :ratio ratio)
(define-type :rational rational)
(define-type :select select)
(define-type :select-method select)
(define-type :short-float short-float)
(define-type :single-float single-float)
(define-type :small-flonum short-float)
(define-type :string string)
(define-type :symbol symbol)
(define-type :list cons)
