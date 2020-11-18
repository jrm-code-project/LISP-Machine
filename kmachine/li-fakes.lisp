;;; -*- Mode:LISP; Package:LISP-INTERNALS; Base:10; Readtable:CL -*-


(lisp:defmacro setup-li-fakes (&rest fakes)
  (do ((fakes fakes (lisp:cddr fakes))
       (forms '() (lisp:cons `(zl:deff ,(lisp:car fakes) #',(lisp:cadr fakes))
                             forms)))
      ((lisp:null fakes) `(lisp:progn . ,forms))))

(setup-li-fakes
  intern      lisp:intern
  apply       lisp:apply
;  macroexpand nlisp::macroexpand
  defstruct   lisp:defstruct
  symbol-name lisp:symbol-name
  symbol-value lisp:symbol-value
  concatenate lisp:concatenate
  mapcar      lisp:mapcar
  dolist      lisp:dolist
  reverse     lisp:reverse
  car         lisp:car
  cdr         lisp:cdr
  first       lisp:first
  second      lisp:second
  third       lisp:third
  caar        lisp:caar
  cadr        lisp:cadr
  cdar        lisp:cdar
  cddr        lisp:cddr
  cdddr       lisp:cdddr
  caddr       lisp:caddr
  nthcdr      lisp:nthcdr

  array:make-vector global:make-array
  array:svset       global:aset
  svref       lisp:svref

  string      lisp:string
  make-list   lisp:make-list
  cons        lisp:cons
  list        lisp:list
  list*       lisp:list*
  vector      lisp:vector
  make-symbol lisp:make-symbol
  make-hash-table lisp:make-hash-table
  gethash     lisp:gethash
  member      lisp:member
  assoc       lisp:assoc
  subst       lisp:subst
  length      lisp:length
  rassoc      lisp:rassoc
  mapcan      lisp:mapcan
  every       lisp:every
  nreverse    lisp:nreverse
  pushnew     lisp:pushnew

  atom        lisp:atom
  listp       lisp:listp
  consp       lisp:consp
  symbolp     lisp:symbolp
  integerp    lisp:integerp
  stringp     lisp:stringp
  keywordp    lisp:keywordp
  numberp     lisp:numberp
  complexp    lisp:complexp
  constantp   lisp:constantp

  equal       lisp:equal
  eql         lisp:eql

  substitute-if lisp:substitute-if
  getf        lisp:getf

  error       lisp:error
  cerror      lisp:cerror
  check-type  lisp:check-type

  subtypep    lisp:subtypep

  gentemp     lisp:gentemp
  gensym      lisp:gensym

  log         lisp:log

  )

(global:defvar lambda-list-keywords lisp:lambda-list-keywords)
(global:defvar most-positive-fixnum lisp:most-positive-fixnum)
(global:defvar most-negative-fixnum lisp:most-negative-fixnum)

;;;; until new-setf is loaded
;(zl:deff push   #'lisp:push)
;(zl:deff pop    #'lisp:pop)
;(zl:deff incf   #'lisp:incf)


(lisp:deftype symbol () 'lisp:symbol)
(lisp:deftype string () 'lisp:string)
(lisp:deftype cons   () 'lisp:cons)
(lisp:deftype string-char () 'lisp:string-char)
(lisp:deftype list  () 'lisp:list)
(lisp:deftype vector () 'lisp:vector)

(lisp:defsetf gethash (k ht) (v)
  `(lisp:setf (lisp:gethash ,k ,ht) ,v))

(lisp:defsetf car (x) (v)
  `(lisp:setf (lisp:car ,x) ,v))

(lisp:defsetf cdr (x) (v)
  `(lisp:setf (lisp:cdr ,x) ,v))

(lisp:defsetf svref (x i) (v)
  `(lisp:setf (lisp:svref ,x ,i) ,v))
