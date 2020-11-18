;;; -*- Mode:LISP; Base:10; Readtable:CL -*-

;;; fast set operations using bignum arithmetic: 11/23/85 17:32:23 -George Carrette
;;; Rewritten for use in micro-tracer by JRM.

;;; unfortunately common-lisp already has a function named SET-DIFFERENCE, so
;;; we call ours SET-DIFF. ditto for SET-EXCLUSIVE-OR.

;;; Universes (universes?)

(defvar *default-universe-size* 100. "Default size for all universes.")

(defstruct (universe (:callable-constructors nil)
                     (:constructor internal-make-universe)
                     (:print-function print-universe))
  element->number-map
  number->element-map)

(defun print-universe (universe stream ignore)
  (format-unreadable-object stream
    #'(lambda ()
        (format stream "Universe ~D with ~D element~:P"
                (%pointer universe)
                (length (universe-number->element-map universe))))))

(defun make-universe (&optional (size *default-universe-size*))
  (internal-make-universe
    :element->number-map (make-hash-table :test 'equal :size size)
    :number->element-map (make-array size :adjustable t :fill-pointer 0)))

(defun universe-size (universe)
  (length (universe-number->element-map universe)))

(defun element->number (element universe)
  (gethash element (universe-element->number-map universe)))

(defun number->element (number universe)
  (aref (universe-number->element-map universe) number))

(defun element->number-inserting (element universe)
  (or (element->number element universe)
      (prog1 (puthash element
                      (length (universe-number->element-map universe))
                      (universe-element->number-map universe))
             (vector-push-extend element (universe-number->element-map universe)))))

(defvar *default-universe* (make-universe))

;;; Sets

(defstruct (set (:callable-constructors nil)
                (:constructor internal-make-set)
                (:print-function set-printing-function))
  universe
  contents)

(defsynonym set? set-p)

(defsubst element-bit-is-on? (index contents)
  (logbitp index contents))

(defun spread-set (set receiver)
  "(FUNCALL <receiver> (SET-UNIVERSE <set>) (SET-CONTENTS <set>))"
  (funcall receiver (set-universe set) (set-contents set)))

(defun spread-set-operation->set-operation (function)
  #'(lambda (set)
      (check-type set set)
      (spread-set set function)))

(defun spread-set-significant-size (universe contents)
  ;; the set-contents will only be negative if we are using indefinite sets
  ;; which is not yet implemented.
  (if (minus? contents)
      (universe-size universe)
      (integer-length contents)))

(defun spread-set-for-set-elements (universe contents function)
  (dotimes (index (spread-set-significant-size universe contents))
    (when (element-bit-is-on? index contents)
      (funcall function (number->element index universe)))))

(defun set-printing-function (set stream ignore)
  (format-unreadable-object stream
    #'(lambda ()
        (spread-set set
          #'(lambda (universe contents)
              (if (zero? contents)
                  (format stream "An Empty Set")
                (format stream "SET containing:")
                (spread-set-for-set-elements universe contents
                  #'(lambda (element)
                      (format stream " ~S" element)))))))))

(defun set-alike? (set1 set2)
  (spread-set set1
    #'(lambda (universe1 contents1)
        (spread-set set2
        #'(lambda (universe2 contents2)
            (and (eq? universe1 universe2)
                 (= contents1 contents2)))))))

(defalike #'set? #'set-alike?)

(defun set-adjoin-element! (set element)
  (spread-set set
    #'(lambda (universe contents)
        (setf (set-contents set) (dpb 1 (byte 1 (element->number-inserting element universe)) contents))))
  set)

(deff set-adjoin-elements! (binary-function->left-associating-reducer #'set-adjoin-element! #'identity))

(defsubst set-push! (element set)
  (set-adjoin-element! set element))

(defun make-empty-set (&optional (universe *default-universe*))
  (internal-make-set :universe universe
                     :contents 0))

(defun empty-set? (object)
  (and (set? object)
       (zero? (set-contents object))))

(defun elements->set-contents (element-list universe)
  (let ((contents-bignum 0))
    (dolist (element element-list)
      (setf (ldb (byte 1 (element->number-inserting element universe)) contents-bignum) 1))
    contents-bignum))

(defun list->set (element-list &optional (universe *default-universe*))
  (internal-make-set :universe universe
                     :contents (elements->set-contents element-list universe)))

(defun make-set (&rest elements)
  (list->set (copylist elements)))

(defun for-set-elements (set function)
  (spread-set set
    #'(lambda (universe contents)
        (spread-set-for-set-elements universe contents function))))

(defun for-single-element-subsets (set function)
  (spread-set set
    #'(lambda (universe contents)
        (dotimes (index (spread-set-significant-size universe contents))
          (when (element-bit-is-on? index contents)
            (funcall function (internal-make-set :universe universe
                                                 :contents (^ 2 index))))))))

(defun set-filter (set predicate)
  (let ((result-set (make-empty-set)))
    (for-single-element-subsets set
      #'(lambda (subset)
          (when (funcall predicate subset)
            (setq result-set (set-union result-set subset)))))
    result-set))

(defun set->list-of-subsets (set)
  (let ((subset-list '()))
    (for-single-element-subsets set
      #'(lambda (subset)
          (push subset subset-list)))
    subset-list))

(defun set-element? (element set)
  (check-type set set)
  (spread-set set
    #'(lambda (universe contents)
        (let ((index (element->number element universe)))
          (and index (element-bit-is-on? index contents))))))

(defun spread-set-set->list (universe contents)
  (let ((elements '()))
    (spread-set-for-set-elements universe contents
      #'(lambda (element)
          (push element elements)))
    elements))

(deff set->list (spread-set-operation->set-operation #'spread-set-set->list))

;; cant use LOGNOT here because that would cause a set to GROW as items were added to
;; the universe. Experiment with that (indefinite sets) later.

(defun spread-set-complement (universe contents)
  (internal-make-set :universe universe
                     :contents (logandc2 (1- (^ 2 (universe-size universe)))
                                         contents)))

(deff set-complement (spread-set-operation->set-operation #'spread-set-complement))

(defun spread-set-cardinality (ignore-universe contents)
  ignore-universe
  (logcount contents))

(deff set-cardinality (spread-set-operation->set-operation #'spread-set-cardinality))

(defun check-set-universes (universe1 universe2)
  (when (not (eq? universe1 universe2))
    (cerror "Do it anyway (result is meaningless)."
            "Operation on sets from different universes: ~S and ~S"
            universe1 universe2)))

(defun logical-operation->set-operation (logop)
  #'(lambda (set1 set2)
      (check-type set1 set)
      (check-type set2 set)
      (spread-set set1
        #'(lambda (universe1 contents1)
            (spread-set set2
              #'(lambda (universe2 contents2)
                  (check-set-universes universe1 universe2)
                  (internal-make-set :universe universe1 ;or universe2, they are eq
                                     :contents (funcall logop contents1 contents2))))))))

(defmacro define-set-operation (name contents-operation)
  `(defun ,name (&rest sets)
     (left-associating-reduce
       (logical-operation->set-operation (function ,contents-operation))
       (make-empty-set)
       sets)))

(define-set-operation set-union logior)

(define-set-operation set-xor logxor)

(deff set-intersection
      (binary-function->left-associating-reducer
        (logical-operation->set-operation #'logand)
        #'identity))

;; fix bug in system. Documentation of boole is wrong and so are some of the constants.
(setq boole-andc2 4)

(deff set-diff (binary-function->left-associating-reducer
                 (logical-operation->set-operation #'logandc2)))
