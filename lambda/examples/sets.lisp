;;; -*- Mode:LISP; Package:USER; Readtable:CL; Base:10 -*-

#||

*********************************************************
*********************************************************
*** NOTE: This is an EXAMPLE, not LMI supported code. ***
*** information contained in this example is subject  ***
*** to change without notice. The ways of doing       ***
*** the things contained in the example may change    ***
*** between system releases. Some techniques which    ***
*** are mere examples in one release may become built ***
*** in system features in the next release. Use good  ***
*** judgement when copying these techniques. Most     ***
*** examples have been motivated by specific customer ***
*** requests, and may not be the best engineered      ***
*** or most efficient solution for someone else.      ***
*********************************************************
*********************************************************

||#

;;; fast set operations using bignum arithmetic: 11/23/85 17:32:23 -George Carrette
;;; There are two sets of functions, those that manipulate bignums and refer to a global
;;; variable as *UNIVERSE* and those that manipulate a defstruct of type SET.
;;; The numeric forms of set algebra operations open compile into machine instructions.

;;; unfortunately common-lisp already has a function named SET-DIFFERENCE, so
;;; we call ours SET-DIFF.

;;; Set-Object-Form,           Numeric-Form
;;;
;;;
;;; MAKE-SET &rest elements,   MAKE-SET-NUMERIC &rest elements, MAKE-SET-FROM-NUMERIC number.
;;; SET-ELEMENTS set,          SET-ELEMENTS-NUMERIC number.
;;; SET-UNION &rest sets,      SET-UNION-NUMERIC &rest numbers.
;;; SET-INTERSECTION
;;; SET-DIFF

(defstruct (set (:callable-constructors nil) (:constructor make-set-1)
                (:print-function set-printing-function))
  universe
  contents)


(defun significant-size (set)
  ;; the set-contents will only be negative if we are using indefinite sets
  ;; which is not yet implemented.
  (if (minusp (set-contents set))
      (length (set-contents (set-universe set)))
    (integer-length (set-contents set))))

(defun set-printing-function (set stream ignore)
  (cond ((not (numberp (set-contents set)))
         (format stream "#<UNIVERSE with ~D element~p>"
                 (length (set-contents set))
                 (length (set-contents set))))
        ((zerop (set-contents set))
         (princ "#<An Empty Set>" stream))
        ('else
         (princ "#<SET containing:" stream)
         (do ((v (set-contents (set-universe set)))
              (c (set-contents set))
              (j 0 (1+ j))
              (n (significant-size set)))
             ((= j n))
           (when (logbitp j c)
             (princ " " stream)
             (prin1 (aref v j) stream)))
         (princ ">" stream))))

(defun make-universe (&optional (size 100))
  (make-set-1 universe (make-hash-table :test 'equal :size size)
              contents (make-array size :adjustable t :fill-pointer 0)))

(defvar *universe* (make-universe))

(defun get-ordinal (element &optional (universe *universe*))
  (or (gethash element (set-universe universe))
      (prog1 (puthash element
                      (length (set-contents universe))
                      (set-universe universe))
             (vector-push-extend element (set-contents universe)))))

(defun make-set-numeric (&rest elements)
  (let ((b 0))
    (dolist (e elements)
      (setf (ldb (byte 1 (get-ordinal e)) b) 1))
    b))

(defun make-set (&rest elements)
  (make-set-1 universe *universe*
              contents (apply #'make-set-numeric elements)))

(defun make-set-from-numeric (number &optional (universe *universe*))
  (make-set-1 universe universe
              contents number))

(defun make-empty-set (&optional (universe *universe*))
  (make-set-1 universe universe contents 0))

(defun set-elements (set)
  (check-type set set)
  (set-elements-1 (set-contents set) (set-contents (set-universe set))))

(defun set-elements-numeric (number)
  (set-elements-1 number (set-contents *universe*)))

(defun set-elements-1 (number vector)
  (do ((j 0 (1+ j))
       (l nil (if (logbitp j number)
                  (cons (aref vector j) l)
                l))
       (n (if (minusp number) (length vector) (integer-length number))))
       ((= j n)
        l)))

(defun meaningless-universe-error (set1 set2)
  (cerror "do it anyway(meaningless result)"
          "operation on sets from different universes: ~S and ~S"
          set1 set2))

(defun set-adjoin-elements (set &rest elements)
  (let ((contents (set-contents set))
        (universe (set-universe set)))
    (dolist (e elements)
      (setf (ldb (byte 1 (get-ordinal e)) contents) 1))
    (make-set-1 universe universe
                contents contents)))

(defmacro define-set-operation (name contents-operation)
  `(progn (defsubst ,(intern (string-append name '-numeric)) (&rest sets)
            (apply #',contents-operation sets))
          (defun ,name (set1 &rest sets)
            (check-type set1 set)
            (do ((result (set-contents set1))
                 (universe (set-universe set1))
                 (l sets (cdr l))
                 (x))
                ((null l)
                 (make-set-1 universe universe contents result))
              (setq x (car l))
              (check-type x set)
              (or (eq universe (set-universe x)) (meaningless-universe-error set1 x))
              (setq result (,contents-operation result (set-contents x)))))))

(define-set-operation set-union logior)

(define-set-operation set-intersection logand)

;; fix bug in system. Documentation of boole is wrong and so are some of the constants.
(setq boole-andc2 4)

(defsubst set-diff-numberic (number1 number2)
  (logandc2 number1 number2))

(defun set-diff (set1 set2)
  (check-type set1 set)
  (check-type set2 set)
  (let ((universe (set-universe set1)))
    (or (eq universe (set-universe set2)) (meaningless-universe-error set1 set2))
    (make-set-1 universe universe
                contents (logandc2 (set-contents set1)
                                   (set-contents set2)))))

(defsubst set-cardinality-numeric (number)
  (logcount number))

(defun set-cardinality (set)
  (check-type set set)
  (logcount (set-contents set)))

;; cant use LOGNOT here because that would cause a set to GROW as items were added to
;; the universe. Experiment with that (indefinite sets) later.

(defun set-complement (set)
  (check-type set set)
  (make-set-1 universe (set-universe set)
              contents (logandc2 (1- (^ 2 (length (set-contents (set-universe set)))))
                                 (set-contents set))))

(defsubst set-complement-numeric (number)
  (logandc2 (1- (^ 2 (length (set-contents *universe*))))
            number))

(defun set-subset (set predicate &rest args)
  (check-type set set)
  (do ((v (set-contents (set-universe set)))
       (c (set-contents set))
       (j 0 (1+ j))
       (n (significant-size set)))
      ((= j n)
       (make-set-1 universe (set-universe set)
                   contents c))
    (when (and (logbitp j c)
               (not (apply predicate (aref v j) args)))
      (setf (ldb (byte 1 j) c) 0))))


(defun map-set (set predicate &rest args)
  (check-type set set)
  (do ((v (set-contents (set-universe set)))
       (c (set-contents set))
       (j 0 (1+ j))
       (n (significant-size set)))
      ((= j n))
    (when (logbitp j c)
      (apply predicate (aref v j) args))))

(defun map-subset-1 (set predicate &rest args)
  "Map over subsets of cardinality one"
  (check-type set set)
  (do ((c (set-contents set))
       (j 0 (1+ j))
       (n (significant-size set)))
      ((= j n))
    (when (logbitp j c)
      (apply predicate
             (make-set-1 universe (set-universe set)
                         contents (^ 2 j))
             args))))
