;;; -*- Mode:LISP; Package:MICRO; Base:10 -*-

;;; Lisp will see SCHEME functions as


;(defun j-random-scheme-function
;       (return-destination return-frame-location
;       &optional
;       act0  act1  act2  act3  act4  act5  act6  act7
;       act8  act9  act10 act11 act12 act13 act14 act15
;       n-args
;       &aux
;       opn0  opn1  opn2  opn3  opn4  opn5  opn6  opn7
;       opn8  opn9  opn10 opn11 opn12 opn13 opn14 opn15
;       ret0  ret1  ret2  ret3  ret4  ret5  ret6  ret7
;       ret8  ret9  ret10 ret11 ret12 ret13 ret14 ret15)
;   (block top-of-scheme-function
;  code)

(defun check-arg-and-local-order (function)
  (let ((arg-map (cadr (assq 'compiler:arg-map (debugging-info function))))
        (local-map (cadr (assq 'compiler:local-map (debugging-info function)))))
    (let ((act-start (cl:member 'act0 arg-map :test #'(lambda (x y) (eq x (car y))))))
      (do ((i 0 (1+ i))
           (act-list act-start (cdr act-list)))
          ((= i 16.)
           (when (not (string-equal (caar act-list) "N-ARGS"))
             (ferror nil "No N-ARGS arg")))
        (when (not (string-equal (caar act-list) (format nil "ACT~d" i)))
          (ferror nil "args out of order"))))
    (let ((opn-start (cl:member 'opn0 local-map :test #'(lambda (x y) (eq x (car y))))))
      (do ((i 0 (1+ i))
           (opn-list opn-start (cdr opn-list)))
          ((= i 16.))
        (when (not (string-equal (caar opn-list) (format nil "OPN~d" i)))
          (ferror nil "opens out of order"))))
    (let ((ret-start (cl:member 'ret0 local-map :test #'(lambda (x y) (eq x (car y))))))
      (do ((i 0 (1+ i))
           (ret-list ret-start (cdr ret-list)))
          ((= i 16.))
        (when (not (string-equal (caar ret-list) (format nil "RET~d" i)))
          (ferror nil "returns out of order"))))
    ))

(defconst %%scheme-min-args (byte 8 0))
(defconst %%scheme-max-args (byte 8 8))
(defconst %%scheme-rest-p (byte 1 16.))

(defmacro make-argspec (required-args optional-args rest-p)
  (cond ((and (cl:listp required-args)
              (cl:listp optional-args)
              (memq rest-p '(t nil)))
         (dpb (if (eval rest-p) 1 0)
              %%scheme-rest-p
              (dpb (+ (length (eval required-args)) (length (eval optional-args)))
                    %%scheme-max-args
                    (dpb (length (eval required-args))
                         %%scheme-min-args
                         0))))
        (t
         `(dpb (if ,rest-p 1 0)
               %%scheme-rest-p
               (dpb (+ (length ,required-args) (length ,optional-args))
                    %%scheme-max-args
                    (dpb (length ,required-args)
                         %%scheme-min-args
                         0))))))

(defmacro define-orbit-output (name arglist &body body)
  `(progn
     (defun ,name (return-destination return-frame-location
                   act0 act1 act2 act3 act4 act5 act6 act7
                   act8 act9 act10 act11 act12 act13 act14 act15
                   n-args
                   &aux
                   opn0 opn1 opn2 opn3 opn4 opn5 opn6 opn7
                   opn8 opn9 opn10 opn11 opn12 opn13 opn14 opn15
                   ret0 ret1 ret2 ret3 ret4 ret5 ret6 ret7
                   ret8 ret9 ret10 ret11 ret12 ret13 ret14 ret15)
       return-destination return-frame-location
       act0 act1 act2 act3 act4 act5 act6 act7
       act8 act9 act10 act11 act12 act13 act14 act15
       n-args
       opn0 opn1 opn2 opn3 opn4 opn5 opn6 opn7
       opn8 opn9 opn10 opn11 opn12 opn13 opn14 opn15
       ret0 ret1 ret2 ret3 ret4 ret5 ret6 ret7
       ret8 ret9 ret10 ret11 ret12 ret13 ret14 ret15

       (block top-of-scheme-function
         (%scheme-check-args (make-argspec ',arglist nil nil))
         ,@body
         (ferror nil "trying to do lisp return from scheme function")
         ))
     (check-arg-and-local-order ',name)
     ))

(defvar *new-k-frames*)
(defvar *k-frame-stack-pointer*)

(defun initialize-new-k-frames ()
  (setq *new-k-frames* (make-array 256.))
  (dotimes (i (array-length *new-k-frames*))
    (aset (make-list 16.) *new-k-frames* i))
  (setq *k-frame-stack-pointer* 0))
(initialize-new-k-frames)

;return frame must be at end of local block so it is more
;likely to be in the pdl buffer when the scheme function returns
(defun lisp-calls-scheme (scheme-function &rest arguments)
  (let (return-value i args
        ret0  ret1  ret2  ret3  ret4  ret5  ret6  ret7
        ret8  ret9  ret10 ret11 ret12 ret13 ret14 ret15)
    ret0  ret1  ret2  ret3  ret4  ret5  ret6  ret7
    ret8  ret9  ret10 ret11 ret12 ret13 ret14 ret15
    (when (> (length arguments) 16.)
      (ferror nil "too many arguments"))
    (%open-call-block scheme-function 0 0)
    (%push (locf return-value))
    (%push (locf ret0))
    (setq i 0)
    (setq args arguments)
    (do ()
        ((= i 16.))
      (%push (car args))
      (setq i (+ i 1))
      (setq args (cdr args)))
    (%push (length arguments))
    (%activate-open-call-block)
    return-value))

(defun lisp-calls-scheme-mv (scheme-function &rest arguments)
  (let (return-value i args
        ret0  ret1  ret2  ret3  ret4  ret5  ret6  ret7
        ret8  ret9  ret10 ret11 ret12 ret13 ret14 ret15)
    ret0  ret1  ret2  ret3  ret4  ret5  ret6  ret7
    ret8  ret9  ret10 ret11 ret12 ret13 ret14 ret15
    (when (> (length arguments) 16.)
      (ferror nil "too many arguments"))
    (%open-call-block scheme-function 0 0)
    (%push (locf return-value))
    (%push (locf ret0))
    (setq i 0)
    (setq args arguments)
    (do ()
        ((= i 16.))
      (%push (car args))
      (setq i (+ i 1))
      (setq args (cdr args)))
    (%push (length arguments))
    (%activate-open-call-block)
    (values ret0  ret1  ret2  ret3  ret4  ret5  ret6  ret7
            ret8  ret9  ret10 ret11 ret12 ret13 ret14 ret15)))

eh:
(eh:def-ucode-format-error something-was-wrong
  "Something was wrong ~s"
  (and (second ete) (sg-contents sg (second ete)))
  )

(defmacro %scheme-open ()
  `(%u-scheme-open (locf opn0)))

(define-micro-function %u-scheme-open (locf-opn0)
  (declare (:call-as-misc-instruction t))
  ;;(locf opn0)
  ((m-a) pdl-pop)

  ;;put base of free frame into m-b

  ((vma) a-v-support-entry-vector)
  ((vma-start-read) add vma (a-constant (eval '(eval (get-support-entry-vector-slot '*k-frame-stack-pointer*)))))
  (check-page-read)
  (dispatch transport md)
  ((vma-start-read m-c) add md (a-constant 1))
  (check-page-read)
  (dispatch transport md)
  ((m-1) q-pointer md)

  ((vma) a-v-support-entry-vector)
  ((vma-start-read) add vma (a-constant (eval '(eval (get-support-entry-vector-slot '*new-k-frames*)))))
  (check-page-read)
  (dispatch transport md)
  ((vma-start-read) add md (a-constant 1))
  (check-page-read)
  (dispatch transport md)
  (call-data-type-not-equal md (a-constant (byte-value q-data-type dtp-array-pointer)) trap)
  (error-table something-was-wrong nil *new-k-frames* is not an array)

  ((vma-start-read) md)
  (check-page-read)
  (dispatch transport md)
  (call-if-bit-set (lisp-byte si:%%array-long-length-flag) md trap)
  (error-table something-was-wrong nil new-k-frames is a long array)
  ((m-tem) (lisp-byte si:%%array-index-length-if-short) md)
  (call-greater-or-equal m-1 a-tem trap)
  (error-table something-was-wrong nil k-frame overflow)
  ((vma-start-read) m+a+1 vma a-1)
  (check-page-read)
  (dispatch transport md)
  ((m-b) md)
  ((md) add m-1 (a-constant 1))
  ((md) q-pointer md (a-constant (byte-value q-data-type dtp-fix)))
  ((vma-start-write) m-c)
  (check-page-write)
  (gc-write-test)

  ((m-k) m-a)
  (call get-pdl-buffer-index)
  ((pdl-index) m-k)

  ((vma) sub m-b (a-constant 1))
  ((m-1) (a-constant 16.))
  ((md) pdl-index-indirect)
copy-loop
  ((vma-start-write) add vma (a-constant 1))
  (check-page-write)
  ((pdl-index) add pdl-index (a-constant 1))
  ((m-1) sub m-1 (a-constant 1))
  (gc-write-test)
  (jump-not-equal-xct-next m-1 a-zero copy-loop)
 ((md) ldb q-typed-pointer pdl-index-indirect (a-constant (byte-value q-cdr-code cdr-next)))

  ((pdl-index) sub pdl-index (a-constant 1))
  ((md-start-write) ldb pdl-index-indirect q-typed-pointer (a-constant (byte-value q-cdr-code cdr-nil)))
  (check-page-write)
  (gc-write-test)

  ((m-t) a-v-nil)
  (popj)
  )

(defmacro %scheme-call (function n-args return-dest)
  (when (not (numberp n-args))
    (ferror nil "n-args must be a constant number"))
  `(%u-scheme-call ,function ,n-args ,return-dest (locf opn0) (locf ret0)))

(define-micro-function %u-scheme-call (function n-args return-dest locf-opn0 locf-ret0)
  (declare (:call-as-misc-instruction t))
  ((m-d) q-typed-pointer pdl-pop)
  ((m-c) q-pointer pdl-pop)
  ((m-b) q-typed-pointer pdl-pop)
  ((m-r) q-typed-pointer pdl-pop)
  ((m-a) q-typed-pointer pdl-pop)

  (call p3zero)

  ((pdl-push) m-a) ;function
  ((pdl-push) m-b) ;return destination
  ((pdl-push) m-d) ;locf-ret0

  ((m-1) add pdl-pointer (a-constant 16.))
  ((pdl-index) add m-1 (a-constant 1))
  ((pdl-pointer) sub m-c a-pdl-buffer-virtual-address)
  ((pdl-pointer) add pdl-pointer a-pdl-buffer-head)
  ((pdl-pointer) add pdl-pointer (a-constant 15.))

  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))

  ((pdl-pointer) m-1)
  ((pdl-push) dpb m-r q-typed-pointer (a-constant (byte-value q-cdr-code cdr-nil)))

  ;;get saved open frame into m-a

  ((vma) a-v-support-entry-vector)
  ((vma-start-read) add vma (a-constant (eval '(eval (get-support-entry-vector-slot '*k-frame-stack-pointer*)))))
  (check-page-read)
  (dispatch transport md)
  ((vma-start-read) add md (a-constant 1))
  (check-page-read)
  (dispatch transport md)
  ((m-1) q-pointer md)
  (call-less-or-equal m-1 a-zero trap)
  (error-table something-was-wrong nil k-frame underflow)
  ((m-1) sub m-1 (a-constant 1))
  ((md-start-write) q-pointer m-1 (a-constant (byte-value q-data-type dtp-fix)))
  (check-page-write)
  (gc-write-test)

  ((vma) a-v-support-entry-vector)
  ((vma-start-read) add vma (a-constant (eval '(eval (get-support-entry-vector-slot '*new-k-frames*)))))
  (check-page-read)
  (dispatch transport md)
  ((vma-start-read) add md (a-constant 1))
  (check-page-read)
  (dispatch transport md)
  (call-data-type-not-equal md (a-constant (byte-value q-data-type dtp-array-pointer)) trap)
  (error-table something-was-wrong m-t *new-k-frames* is not an array)

  ((vma-start-read) m+a+1 md a-1)
  (check-page-read)
  (dispatch transport md)
  ((vma) sub md (a-constant 1))

  ((pdl-index) sub m-c a-pdl-buffer-virtual-address)
  ((pdl-index) add pdl-index a-pdl-buffer-head)

  ((m-1) (a-constant 16.))
  ((pdl-index) sub pdl-index (a-constant 1))
copy-loop
  ((vma-start-read) add vma (a-constant 1))
  (check-page-read)
  ((pdl-index) add pdl-index (a-constant 1))
  ((m-1) sub m-1 (a-constant 1))
  (dispatch transport md)
  (jump-not-equal-xct-next m-1 a-zero copy-loop)
 ((pdl-index-indirect) md)

  ((arg-call mmcall) (i-arg 20.))               ;i-arg is not really used

  (popj-after-next (m-t) a-v-nil)
 (no-op)
  )

(defmacro %scheme-return (value)
  `(return-from top-of-scheme-function
     (%u-scheme-return ,value (locf act0) return-frame-location return-destination)))

(define-micro-function %u-scheme-return (value act-frame-location return-frame-location return-destination)
  (declare (:call-as-misc-instruction t))
  ;;return-destination
  ((m-e) q-pointer pdl-pop)
  ;;return-frame-location
  ((m-d) q-pointer pdl-pop)
  ;;act-frame-location
  ((m-b) q-typed-pointer pdl-pop)
  ;;value
  ((m-c) q-typed-pointer pdl-pop)

  (call pdl-buffer-refill)                      ;clobbers m-1, m-2
  (jump-less-than m-d a-pdl-buffer-virtual-address not-in-pdl-buffer)

  ;;return-frame-location
  ((m-tem) sub m-d a-pdl-buffer-virtual-address)
  ((m-k) add m-tem a-pdl-buffer-head)
  ((pdl-index) add m-k (a-constant 16.))

  ((m-1) pdl-pointer)


  ;;act-frame-location
  ((m-k) m-b)
  (call get-pdl-buffer-index)
  ((pdl-pointer) add m-k (a-constant 15.))

  ((c-pdl-buffer-index-pre-dec) ldb q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) ldb q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) ldb q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) ldb q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) ldb q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) ldb q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) ldb q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) ldb q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) ldb q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) ldb q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) ldb q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) ldb q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) ldb q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) ldb q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) ldb q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) ldb q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))

  ((pdl-pointer) m-1)
  (jump write-return-dest)

not-in-pdl-buffer
;;copy m-b to m-d, m-b is in pdl, m-d must use vma path
  ((pdl-index) sub m-b a-pdl-buffer-virtual-address)
  ((pdl-index) add pdl-index a-pdl-buffer-head)
  ((vma) m-d)
  ((m-1) (a-constant 16.))
loop
  ((md-start-write) pdl-index-indirect)
  (check-page-write)
  (gc-write-test)
  ((vma) add vma (a-constant 1))
  ((pdl-index) add pdl-index (a-constant 1))
  ((m-1) sub m-1 (a-constant 1))
  (jump-not-equal m-1 a-zero loop)

write-return-dest
  ((md) m-c)
  (JUMP-LESS-THAN M-e A-PDL-BUFFER-VIRTUAL-ADDRESS real-write)
  ((M-TEM) SUB M-e A-PDL-BUFFER-VIRTUAL-ADDRESS)
  ((PDL-INDEX) ADD M-TEM A-PDL-BUFFER-HEAD)
  (popj-after-next (PDL-INDEX-INDIRECT) MD)
 ((m-t) a-v-nil)

real-write
   ((VMA-START-WRITE) M-e)
   (CHECK-PAGE-WRITE)
   (gc-write-test)
   (popj-after-next (m-t) a-v-nil)
  (no-op)
  )

(defmacro %scheme-t-open ()
  nil)

(defmacro %scheme-t-call (function n-args)
  (when (not (numberp n-args))
    (ferror nil "n-args must be a number"))
  `(ignore
     (%u-scheme-t-call ,function ,n-args (locf act0) (locf opn0))))

(define-micro-function %u-scheme-t-call (function n-args locf-act0 locf-opn0)
  (declare (:call-as-misc-instruction t))
  ((m-t) a-v-nil)
  (call-data-type-equal m-fef (a-constant (byte-value q-data-type dtp-u-entry)) trap)
 (error-table something-was-wrong m-t not-dtp-u-entry)
  (call-if-bit-set (byte 1 0) m-flags trap)
 (error-table something-was-wrong m-t you have special bindings)

  ((m-tem) ldb (byte 8 24.) micro-stack-pntr-and-data)
  (call-not-equal m-tem a-zero trap)
 (error-table something-was-wrong m-t i must be called d-ignore)

  ((m-c) q-typed-pointer pdl-pop) ;locf-opn0
  ((m-b) q-typed-pointer pdl-pop) ;locf-act0
  ((m-r) q-typed-pointer pdl-pop) ;n-args
  ((m-a) q-typed-pointer pdl-pop) ;function


  ((pdl-index) add m-ap (a-constant (eval si:%lp-call-state)))
  ((m-t) pdl-index-indirect)
  ((m-tem) and pdl-index-indirect
   (a-constant (eval (logior (dpb -1 si:%%lp-cls-self-map-provided 0)
                             (dpb -1 si:%%lp-cls-adi-present 0)))))
  (call-not-equal m-tem a-zero trap)
 (error-table something-was-wrong m-t bad call state word)

  ((pdl-index) add m-ap (a-constant (eval si:%lp-exit-state)))
  ((m-t) pdl-index-indirect)
  ((m-tem) and pdl-index-indirect
   (a-constant (eval (logior (dpb -1 si:%%lp-exs-micro-stack-saved 0)))))
  (call-not-equal m-tem a-zero trap)
 (error-table something-was-wrong m-t bad exit state word)

  ((pdl-index) add m-ap (a-constant (eval si:%lp-entry-state)))
  ((m-t) pdl-index-indirect)
  ((m-tem) and pdl-index-indirect
   (a-constant (eval (logior (dpb -1 si:%%lp-ens-lctyp 0)
                             ;;this is automatically set when you do (locf arg0) ...
                             ;;(dpb -1 si:%%lp-ens-unsafe-rest-arg 0)
                             (dpb -1 si:%%lp-ens-unsafe-rest-arg-1 0)
                             (dpb -1 si:%%lp-ens-environment-pointer-points-here 0)
                             ))))
  (call-not-equal m-tem a-zero trap)
 (error-table something-was-wrong m-t bad entry state word)

  ((m-tem) ldb (lisp-byte si:%%lp-ens-num-args-supplied) pdl-index-indirect)
  ((m-t) pdl-index-indirect)
  (call-less-than m-tem (a-constant 19.) trap)
 (error-table something-was-wrong m-t wrong number of args)

  ((pdl-index) add m-ap (a-constant si:%lp-call-state))
  ((pdl-index-indirect) and pdl-index-indirect
   (a-constant (eval (logior (dpb -1 si:%%lp-cls-delta-to-open-block 0)
                             (dpb -1 si:%%lp-cls-destination 0)
                             (dpb -1 si:%%lp-cls-delta-to-active-block 0)
                             (dpb -1 si:%%lp-cls-attention 0)
                             (dpb -1 si:%%lp-cls-trap-on-exit 0)
                             (dpb -1 %%q-data-type 0)
                             ))))
  ((pdl-index) add m-ap (a-constant si:%lp-exit-state))
  ((pdl-index-indirect) and pdl-index-indirect
   (a-constant (eval (logior (dpb -1 si:%%lp-exs-pc-status 0)
                             (dpb -1 si:%%lp-exs-exit-pc 0)
                             (dpb -1 %%q-data-type 0)))))
  ((pdl-index) add m-ap (a-constant si:%lp-entry-state))
  ((pdl-index-indirect) and pdl-index-indirect
   (a-constant (eval (logior (dpb -1 si:%%lp-ens-num-args-supplied 0)
                             (dpb -1 %%q-data-type 0)))))

  ((pdl-index) m-ap)
  ((pdl-index-indirect) m-a)
  ((m-fef) m-a)

  ;;locf-opn0
  ((m-k) m-c)
  (call get-pdl-buffer-index)
  ((m-c) m-k)
  ;;locf-act0
  ((m-k) m-b)
  (call get-pdl-buffer-index)
  ((m-b) m-k)

  ((pdl-pointer) add m-c (a-constant 15.))
  ((pdl-index) add m-b (a-constant 16.))

  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))
  ((c-pdl-buffer-index-pre-dec) q-typed-pointer pdl-pop (a-constant (byte-value q-cdr-code cdr-next)))

  ((pdl-top) ldb q-typed-pointer m-r (a-constant (byte-value q-cdr-code cdr-nil)))

  ((pdl-index) sub pdl-pointer a-ap)                    ;number of args
  ((m-r) pdl-index)
  (dispatch-xct-next dispatch-write-vma q-data-type d-qmrcl m-a)
  (no-op)

  (call trap)
 (error-table something-was-wrong m-t how did you get here))

(defconst %%scheme-min-args (byte 8 0))
(defconst %%scheme-max-args (byte 8 8))
(defconst %%scheme-rest-p (byte 1 16.))


(defmacro %scheme-check-args (argspec)
  `(%u-scheme-check-args ,argspec))

(define-micro-function %u-scheme-check-args (argspec)
  (declare (:call-as-misc-instruction t))
  ((m-t) a-v-nil)
  ;;get n-args
  ((pdl-index) add m-minus-one a-localp)
  ((m-1) q-pointer pdl-index-indirect)
  ((m-2) q-pointer pdl-pop)
  ((m-3) ldb (lisp-byte %%scheme-rest-p) m-1)
  ((m-4) ldb (lisp-byte %%scheme-rest-p) m-2)
  (call-not-equal m-3 a-4 trap)
  (error-table something-was-wrong m-t rest-arg-p disagreees)
  ((m-1) dpb m-zero (lisp-byte %%scheme-rest-p) a-1)
  ((m-4) ldb (lisp-byte %%scheme-max-args) m-2)
  (call-greater-than m-1 a-4 trap)
  (error-table something-was-wrong m-t too many arguments)
  ((m-4) ldb (lisp-byte %%scheme-min-args) m-2)
  (call-less-than m-1 a-4 trap)
  (error-table something-was-wrong m-t too few arguments)
  (popj)
  )


(define-orbit-output test1 (x)
  (setq act1 (+ act0 1))
  (%scheme-return act1))

(define-orbit-output utest4 (x)
  (%scheme-open)
  (setq opn0 act0)
  (%scheme-call 'test1 1 (locf act1))
  (%scheme-return act1))

(define-orbit-output t-test (x)
  (%scheme-t-open)
  (setq opn0 act0)
  (%scheme-t-call 'test1 1))

(define-orbit-output t-test-2 ()
  (when (or (not (numberp act0))
            (not (numberp act1))
            (not (numberp act2)))
    (ferror nil "bad"))
  (%scheme-t-open)
  (cond ((> act0 0)
         (%scheme-open)
         (setq opn0 (1- act0))
         (setq opn1 act1)
         (setq opn2 act2)
         (%scheme-call #'t-test-2 3 (locf act3))))
  (setq opn0 act0)
  (setq opn1 act1)
  (setq opn2 act2)
  (%scheme-t-call #'t-test-2 3))

(define-orbit-output no-op ()
  (%scheme-return nil))

(define-orbit-output foo1 ()
  (%scheme-open)
  (%scheme-call #'no-op 0 (locf act0))
  (%scheme-return act0))



(define-orbit-output foo ()
  (%scheme-open)
  (%scheme-open)
  (%scheme-call #'no-op 0 (locf act0))
  (%scheme-call #'no-op 0 (locf act0))
  (%scheme-return nil))
