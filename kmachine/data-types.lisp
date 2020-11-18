;;; -*- Mode:LISP; Package:VINCULUM; Base:10; Readtable:CL -*-


user::
(defmacro vinc::def-data-types (&rest types)
  `(PROGN
     (defconstant vinc:k-data-type-names-alist ',(user:copylist types))
     (EXPORT ',(mapcar #'(lambda (type)
                            (global#:car type))
                        types))
     ,@(mapcar #'(lambda (type)
                   `(DEFCONSTANT ,(global#:car type) ,(global#:cadr type)))
               types)))

;;;; Master list of all the datatypes.

(def-data-types

;;;; Codes 0 through 31 are the visible datatypes.
  ($$dtp-nil                  0.)
  ($$dtp-fixnum               1.)
  ($$dtp-cons                 2.)
  ($$dtp-symbol               3.)
  ($$dtp-bignum               4.)
  ($$dtp-short-float          5.)
  ($$dtp-single-float         6.)
  ($$dtp-double-float         7.)
  ($$dtp-rational             8.)
  ($$dtp-complex              9.)
  ($$dtp-locative            10.)
  ($$dtp-unboxed-locative    11.)
  ($$dtp-compiled-function   12.)
  ($$dtp-code                13.)
  ($$dtp-array               14.)
  ($$dtp-stack-group         15.)
  ($$dtp-instance            16.)
  ($$dtp-lexical-closure     17.)
  ($$dtp-interpreter-closure 18.)
  ($$dtp-lexical-environment 19.)       ;not used apparently
  ($$dtp-structure           20.)
  ($$dtp-character           21.)
  ($$dtp-extend              22.)       ;not used apparently
  ($$dtp-encapsulation       23.)       ;not used apparently
  ($$dtp-hash-table          24.)
  ($$dtp-stream              25.) ;; I'm borrowing this dtp a while for streams... --Jim
  ($$dtp-dynamic-closure     26.) ;; analogous to DTP-CLOSURE on lambda.
  ($$dtp-select-method       27.)
  ; 28 spare
  ; 29 spare
  ; 30 spare
  ; 31 spare

;;;; Codes 32 through 63 are the magic datatypes. (invisible, headers, unbound, etc)
  ($$dtp-unboxed-header                 32.)
  ($$dtp-symbol-header                  33.)
  ($$dtp-array-header-single            34.)
  ($$dtp-array-header-multiple          35.)
  ($$dtp-array-header-extension         36.)
  ($$dtp-external-value-cell-pointer    37.)
  ($$dtp-gc-forward                     38.)
  ($$dtp-one-q-forward                  39.)
  ($$dtp-indexed-forward                40.)
  ($$dtp-instance-header                41.)
  ($$dtp-array-leader-header            42.)
  ($$dtp-unbound                        43.)
  ($$dtp-header-forward                 44.)
  ($$dtp-body-forward                   45.)
  ($$dtp-compiled-function-header       46.)
  ($$dtp-structure-header               47.)
  ($$dtp-hash-table-header              48.)
  ($$dtp-unreconciled                   49.)
  ($$dtp-self-ref-pointer               50.)
  ; 51 spare
  ; 52 spare
  ; 53 spare
  ; 54 spare
  ; 55 spare
  ; 56 spare
  ; 57 spare
  ; 58 spare
  ; 59 spare
  ; 60 spare
  ; 61 spare
  ; 62 spare
  ; 63 spare
 )
