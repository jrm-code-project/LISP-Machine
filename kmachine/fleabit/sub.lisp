
  Substitution of Setqed Variables

is possible when the setq(s) are outside the extent
of the variable substituted for.

this is important for macros especially setf

  var not live at setqs
    requires live variable analysis which does not seem to work
    for thunked labels (Y) fixed! removed fixup! dehaired simplify-y!

    but it still doesn't work very well because strategy analysis
    (known variable) is not too reliable before simplification
    bogus STRATEGY/HEAP lambdas cause bogus closed properties

---

what about putting variables in same pref class?
criterion for coextant-p is now if one variable is live
in the extent of the other.  But if v1 bound to v2 and
v1 is not setqed and v2 is not setqed within extent

**************************************************
maybe this is over restrictive??
if y is bound to x then x and y are in same pref class
if z is also bound to x the we try to add z to pref class
 but z is not bound to y so can-be-same returns NIL

   must v1 be bound to v2??

(defun bletch (i)
  (labels ((foo (x))
             (bar i)
             (foo 5))
    (foo i)))

i and x must be distinct here

coextant-p:

v1 cannot be live and binding of v2 if v2-binder is STRATEGY/LABEL??

what we mean is: v1 cant be live at places where v2 can change.
v2 gets initialized at let binding but we wouldn't be trying to put
it in same class if that binding wasn't to v1 or equivalent

**************************************************

;;; CAN-BE-SAME is true
;;;   if V1 is bound to V2
;;;      and there are no setqs of V1
;;;      and there are no setqs of V2 when V1 is live
;;;
;;; This would be much better done by the simplification
;;; phase if SUBSTITUTE? could return T for V1 and V2.
;;; However, we need live variable analysis to determine this
;;; and analysis is difficult to make work before simplification
;;;
(defun can-be-same (v1 v2)
  (and (null (variable-setqs v1))
       (let ((binder (variable-binder v1)))
         (and (eq (node-role binder) call-proc)
              (let ((val (call-arg-n (1- (variable-number v1))
                              (node-parent binder))))
                (and (reference-node? val)
                     (eq v2 (reference-variable val))
                     (every #'(lambda (setq-call)
                                (not (member v1 (lambda-live (node-parent setq-call)))))
                            (variable-setqs v2))))))))



(defun var-target-by-binding (var binder)
  (and (null (variable-setqs var))
       (eq (node-role binder) call-proc)
       (let ((value (call-arg-n (1- (variable-number var))
                                (node-parent binder))))
         (and (reference-node? value)
              (let ((value-var  (reference-variable value)))
                (and (every #'(lambda (setq-call)
                                (not (member var (lambda-live (node-parent setq-call)))))
                            (variable-setqs value-var))
                     value-var))))))


this seems to work, but target-var-to-var cant handle it because
the two vars are coextant even though that is ok.

;;; variable substitution test

(defun subst-var (x)
  (let ((y x))
    (bar y)
    (baz y))
  (foo x)))

;;; actually x and y can be the same here
;;; because x is not used again!
(defun no-subst-for-setqed-var (x)
  (let ((y x))
    (bar y)
    (setq y 3)
    (bar y)))

(defun no-subst-for-setqed-var (x)
  (let ((y x))
    (bar y)
    (setq y 3)
    (bar y))
  (bar x))

(defun no-subst-setqed-var (x)
  (let ((y x))
    (bar y)
    (setq x 3)
    (bar y)))

(defun no-subst-setqed-var (x)
  (labels ((foo ()
             (setq x 3)))
    (let ((y x))
      (bar y)
      (foo)
      (bar y))))


;;; this doesn't work, but it would be nice if it did
;;; now works in reg-alloc
(defun subst-setqed-ok (x)
  (let ((y x))
    (bar y)
    (baz y))
  (setq x 3))


(defun subst-setqed-ok (x)
  (dotimes (i 3)
    (let ((y x))
      (bar y)
      (baz y))
    (setq x i)))


;;; this could also work
(defun subst-setqed-ok (x)
  (let ((y x))
    (bar y x)
    (baz y))
  (setq x 3))

(prims:defsubst sss (i v)
  (ttt i v))

(defun foo ()
  (let ((i 0))
    (let ((t10 i))
      (let ((val (vvv i)))
        (sss t10 val)))
    (setq i 10)))

(defun copy (a b)
  (dotimes (i 100)
    (setf:setf (svref b i) (svref a i))))

(prims:defsubst %VM-WRITE (pointer data)
  (hw:write-md-boxed data)
  (hw:vma-start-write-boxed pointer))


(defun foo (a d)
  (setq a (bar))
  (%vm-write a d))

;;; this is not a problem with substitution
;;; but with the fact that an a reg gets allocated
;;; for POINTER (the result of the 24+)

(defun foo (a d)
  (%vm-write (hw:24+ 3 a) d))


(defun build-array-headers (number-of-dimensions
                            type
                            displaced-to
                            2d-displaced
                            indirect
                            fill-pointer
                            adjustable
                            leader-length
                            index-length-in-q-s
                            header-block-length
                            main-array-header
                            array-header-extension
                            array-leader-header
                            array
                            )
  ;; Compute what each header has to be and store it.
  (when array-header-extension
    (let ((temp number-of-dimensions))
      (setq main-array-header (hw:dpb-boxed vinc:$$dtp-array-header-multiple vinc:%%data-type main-array-header))
      (setq main-array-header (hw:dpb-boxed array:art-hard array:%%sv-art main-array-header))
      (when displaced-to
        (if 2d-displaced
            (setq temp (+ temp 3))
          (setq temp (+ temp 2))))
      (when fill-pointer (setq temp (1+ temp)))
      (setq array-header-extension (hw:dpb-boxed temp array:%%leader-offset 0))
;                                    (+ number-of-dimensions
;                                       (if displaced-to (if 2d-displaced 3 2) 0)
;                                       (if fill-pointer 1 0))
;                                                array:%%leader-offset 0))
      (when fill-pointer (setq array-header-extension (hw:dpb-boxed 1 array:%%fill-pointer-p array-header-extension)))
      (when (not (zerop leader-length))
        (setq array-header-extension (hw:dpb-boxed 1 array:%%leader-p array-header-extension)))
      (when 2d-displaced (setq array-header-extension (hw:dpb-boxed 1 array:%%displaced-2d-p array-header-extension)))
      (when indirect (setq array-header-extension (hw:dpb-boxed 1 array:%%indirect-p array-header-extension)))
      (setq array-header-extension (hw:dpb-boxed type array:%%array-type array-header-extension))
      (setq array-header-extension (hw:dpb-boxed number-of-dimensions array:%%dimensions array-header-extension))
      (when adjustable (setq array-header-extension (hw:dpb-boxed 1 array:%%adjustable-p array-header-extension)))
      ;; now store it in right spot.
      (setq temp (1- header-block-length))
      (%vm-write (hw:24+ temp array) array-header-extension)
      (when array-leader-header
        (setq array-leader-header (hw:dpb-boxed leader-length array:%%leader-length 0))
        (setq temp (1+ leader-length))
        (hw:write-md-boxed array-leader-header)
        (hw:vma-start-write-boxed (hw:24+ temp array)))))
  (setq array (hw:24+ header-block-length array))
  (values array main-array-header)
  )
