;-*- Mode:LISP; Package:SI; Readtable:ZL; Base:8; Lowercase:T -*-


;K compatability package.   Sexpression definitions of things microcoded on LAMBDA or
;  for some other reason not suitable for K in their LAMBDA form.

;microcoded functions on lambda.  In order of defmic code (see "sys:cold;defmic")
(defun (:property %data-type k-function) (item)
  ****)

;%pointer

;%make-pointer

(defun (:property %logldb k-function) (ppss word) )

(defun (:property %logdpb k-function) (value ppss word)
  )

;ldb    -not compatible. hw:ldb takes different args, uses DT-NONE.  mumble ldb-generic incomplete

;dpb

(defun (:property getl k-function) (from list-of-properties)
  (let ((plist (plist from)))
    (do ((p plist (cddr p)))
        ((null p))
      (cond ((memq (car p) list-of-properties)
             (return p))))))

(defun (:property assq k-function) (item list)
  (do ((l list (cdr l)))
      ((null l))
    (cond ((eq item (caar l))
           (return (car l))))))

;last  compatible

;length compabible

;1+ compatible

;1- compatible

;rplaca compatible

;rplacd compatible

;zerop  ;compatble?  must detect all forms of 0.

;set    does not seem to exist on K.

;integerp,fixp

;floatp

;equal compatible?

;%set-self-mapping-table  - not user level

;pdl-word       - not compatible

(defun (:property false k-function) () nil)

(defun (:property true k-function) () t)

;not,null compatible?

;atom

;oddp

;evenp

;%halt  - !!

;get-pname,symbol-name

;lsh

;rot

;*boole

;numberp

;plusp

;minusp

;\

;minus

;%sxhash-string

;value-cell-location            -- LOCATIVE DATA TYPE

;function-cell-location

;property-cell-location

(defun (:property ncons k-function) (car) (cons car nil))

(defun (:property ncons-in-area k-function) (car area) (cons-in-area car nil area))

;cons

;cons-in-area


(defun (:property xcons k-function) (cdr car) (cons car cdr))

(defun (:property xcons-in-area k-function) (cdr car area) (cons-in-area car cdr area))

;%spread-n              --not user level

;symeval,symbol-value

;pop-m-from-under-n     --not user level

;return-next-value      --decommitted

;return-list

;unbind-to-index-under-n        --not user level

;%bind bind    ***

;%nway-branch  ***




(defun (:property memq k-function) (item list)
  (do ((l list (cdr l)))
      ((null l))
    (cond ((eq (car l) item) (return l)))))

;internal-char-equal

;%string-search-char

;%string-equal

;nth

;nthcdr

;*plus

;*dif

;*times

;*quo

;*logand

;*logxor

;*logior

;array-leader           --array
;store-array-leader     --array

;get-list-pointer-into-array            ** decommit?

;array-push

;internal-apply           not-user-level

;%make-list

;locate-in-instance

;%p-cdr-code            ** decommit

;%p-data-type

;%p-pointer

;%page-trace            -random

;throw-n                not-user-level

;%p-store-cdr-code      **decommit

;%p-store-data-type     ** should not use this.

;%p-store-pointer       ** should not use this.

;float-exponent

;float-fraction

;scale-float

;internal-floor-1       **decommit

;%div

;%blt

;%p-ldb

;%p-dpb

;mask-field

;%p-mask-field

;deposit-field

;%p-deposit-field

;copy-array-contents

;copy-array-contents-and-leader

;%function-inside-self          **?

;array-has-leader-p

;copy-array-portion

;find-position-in-list

;%get-self-mapping-table        -- not user level

;g-l-p          decommit

;internal-floor-2               -- decommit

;eql

;ar-1

;ar-2

;ar-3

;as-1

;as-2

;as-3

;%instance-ref

;%instance-loc

;%binding-instances             ;??
;%external-value-cell           ;??
;%using-binding-instances       ;??

;%gc-cons-work

;%p-contents-offset             ;probably should not use.

;%args-info

;%push                          ;not compatible

;%activate-open-call-block      ;not compatible

;%assure-pdl-room               ;not compatible (noop anyway)

;%stack-group-return

;as-2-reverse

;%make-stack-list               ;not compatible

;stack-group-resume

;%%p-store-contents-offset      ;probably should not use.

;array-length

;array-total-size

;array-active-length

;%area-number

;*max

;*min

;closure

;ar-2-reverse

;listp

;nlistp

;symbolp

;nsymbolp

;arrayp

;fboundp

;stringp

;boundp

;internal-\\

;fsymeval,symbol-function

;ap-1

;ap-2

;ap-3

;ap-leader

;%p-ldb-offset

;%p-dpb-offset

;%p-mask-field-offset

;%p-deposit-field-offset

;%multiply-fractions

;%divde-double

;%remainder-double

;haulong

;%make-pointer-offset

;^

;%24-bit-plus

;%24-bit-difference

;%24-bit-times

;abs

;%pointer-difference

;%p-contents-as-locative

;%p-contents-as-locative-offset

;eq             ;compatible!

;%store-conditional

;%stack-frame-pointer   -- not compatible
;*unwind-stack          -- not compatible

;elt

;move-pdl-top           -- not compatible
;shrink-pdl-save-top    -- not compatible
;special-pdl-index      -- not compatible
;unbind-to-index        -- not compatible
;unbind-to-index-move   -- not compatible

;fix

;small-float
;%float-double

;bignum-to-array         may decommit
;array-to-bignum         may decommit

;%unwind-protect-continue       -- not compatible
;%write-internal-processor-memories  -- not compatible
;%page-status           -- not compatible

;%region-number

;%find-structure-header
;%structure-boxed-size
;%structure-total-size

;%make-region

;bitblt         -- youcef

;%physical-address

;pop-open-call          not compatible

;%beep

;%find-structure-leader

;bpt

;%findcore              not compatible

;%page-in               no-op for now

;ash

;%make-explicit-stack-list      ;not compatible

;%draw-char             -- youcef
;%draw-rectangle        -- youcef
;%draw-line             -- youcef
;%draw-triangle         -- youcef       (semi-optional)
;%color-transform       decommit

;%record-event          decommit

;%aos-triangle          -- youcef       (semi-optional)

;%set-mouse-screen
;%open-mouse-cursor

;setelt

;%blt-typed

;%draw-patterned-line   --youcef        (semi optional)

;ar-1-force
;as-1-force
;ap-1-force

;aref aset aloc  (not really ucoded)

;equalp

;%make-explicit-stack-list*     --decommit

;setcar
;setcdr
;get-location-or-nil

;%string-width

;ar-1-cached-1          decommit
;ar-1-cached-2          decommit

;set-ar-1
;set-ar-2
;set-ar-3
;set-ar-1-force
;set-aref               --not really ucoded
;set-array-leader
;set-%indstance-ref

;vector-push
;array-has-fill-pointer-p
;array-leader-length
;array-rank
;array-dimension

;return-n-keep-control    decommit
;return-spread-keep-control  decommit

;common-lisp-listp

;%microsecond-time
;%fixnum-microsecond-time

;vectorp
;simple-vector-p
;simple-array-p
;simple-string-p
;bit-vector-p
;simple-bit-vector-p

;named-structure-p
;named-structure-symbol

;typep-structure-or-flavor

;fixnump
;small-floatp
;charachterp

;car-safe
;cdr-safe
;cadr-safe
;cddr-safe
;cddddr-safe
;nthcdr-safe
;nth-safe

;carcdr

;endp

;consp-or-pop           ;?

;indicators-value

;%pointer-times

;common-lisp-aref
;common-lisp-ar-1
;common-lisp-ar-1-force

;char-int
;int-char
;alpha-char-p
;upper-case-p
;alphanumericp
;package-cell-location
;member-eql
;rationalp
;ratiop
;complexp
;%ratio-cons
;%complex-cons
;both-case-p
;char-upcase
;char-downcase
;lower-case-p

;member, member-equal

;assoc, assoc-equal

;%blt-boole
;%sxhash-substring

;%pointer-info          decommit

;%pointer-lessp
;%pointer-greaterp

;%string-translate

;%store-conditional-double
