;;; -*- Mode:LISP; Package:COMPILER; Base:8; Readtable:ZL -*-


;;; Convert a destination to a source.  (This works on compiler-level destinations,
;;; not hardware-level ones).  Signals an error if given a D-RETURN, for obvious
;;; reasons
;;; This returns a new destination which must be used instead of the original one.
;;; The source is the second value.

(defun convert-dest-to-source (dest)
  (declare (values new-dest source))
  (etypecase dest
    (symbol
     (case dest
       (d-ignore (values 'k:r0 'k:r0))
       (d-return
        (error "Cannot convert D-RETURN into a source."))
       (otherwise (values dest dest))))
    (list
     (error "Cannot convert a NEW-OPEN-style destination into a source."))
    (multiple-values
     (let ((subdest (or (first (multiple-values-values dest)) 'd-ignore)))
       (multiple-value-bind (new-subdest source)
           (convert-dest-to-source subdest)
         (if (eq subdest new-subdest)
             (values dest source)
           (setq dest (copy-multiple-values dest))
           (setf (multiple-values-values dest)
                 (list* new-subdest (rest (multiple-values-values dest))))
           (values dest source)))))
    (progdesc
     (let ((subdest (progdesc-idest dest)))
       (multiple-value-bind (new-subdest source)
           (convert-dest-to-source subdest)
         (if (eq subdest new-subdest)
             (values dest source)
           (setq dest (copy-progdesc dest))
           (setf (progdesc-idest dest) new-subdest)
           (values dest source)))))
    (open-frame
     (let ((subdest (open-frame-idest dest)))
       (multiple-value-bind (new-subdest source)
           (convert-dest-to-source subdest)
         (setq dest (copy-open-frame dest))
         (setf (open-frame-idest dest) new-subdest)
         (values dest source))))))




(defun emit (x)
  (outi-for-k x))

(defun emit-alu (op dest left right &optional options)
  (emit `(K:ALU ,op ,dest ,left ,right ,@options))
  dest)


(defun k-push-stack-arg (intermediate-home)
  (generate-incr 'GR:*STACK-POINTER* 'GR:*STACK-POINTER* 1)     ;incr stack pointer
  ;(outi-for-k `(K:MOVE (K:VMA K:BOXED-VMA) GR:*STACK-POINTER*))
  (generate-decr '(k:vma k:boxed-vma) 'gr:*stack-pointer* 1)
  (outi-for-k `(K:MOVE (K:MD-START-WRITE K:BOXED-VMA K:BOXED-MD) ,intermediate-home))
  (incf *stack-slots*)
  )



(DEFUN OUTI-for-k (X &optional source-type)
 ;every instruction emitted for K goes thru here.
 ;general rules: all instructions should always be in "simplest form".  The peephole will
 ;  do a good job of combining things, and otherwise it can be faked out.
 ;  in particular:  do not use k:open-call, etc.  always use separate k:open instructions.
 ;                  similarily, never use k:ch-open-call, etc.
 ;also do some fixups on the instructions:
 ;  on dest in *return-destinations* (i.e. k:return, etc) insert appropriate exit sequence.
 ;  on dest (k:new-open <n>), leave it in a call.
 ;                            refrob it for k:move or k:movei
 ;  on dest for MOVE or MOVEI <other list>, pass it thru (typically VMA, MD etc)
;;
;; No! Let lower level outf-for-k do dropthru checking.  If we bypass *all* of outi-for-k
;; then open but unused frames don't get cleaned properly. -smh 7sep88
;;  (IF (NOT *DROPTHRU*)
;;      NIL
    (case (car x)
      ((k:open)
       (check-not-open-and-return x)
       (note-open-frame nil)
       (outf-for-k x))
      ((k:tail-open)
       (check-not-open-and-return x)
       ;; Not necessarily PROG1, this may just be a RETURN from inside a tail-called function.
       (note-open-frame t)
       (outf-for-k x))
  ;do not output K:OPEN-CALL directly, instead let POST-PROCESS make it.  Otherwise it gets confused.
      ((k:call)         ;take destination in 3rd element
       (finish-open-frame nil)
       (destructuring-bind (opcode fspec dest &rest flags) x
         (when (listp dest)
           ;; Some special cases.
           (case (first dest)
             (k:register
              (setq dest (second dest)))
             (k:new-open (note-open-frame nil))
             (k:new-tail-open (note-open-frame t))))
         (compiling-to-destination (dest nil source-type)
           (nc:debug :frames
             (format nc:*debug-stream* "~%OUTI-FOR-K K:CALL ~a ~a ~a" opcode fspec dest))
           (when (memq dest *internal-return-destinations*)     ;note that dest, if it is a return-type frob,
             (fsignal "destination not converted"))             ; must be one of *return-destinations* as opposed to
                                                                ; *internal-return-destinations*
           (outf-for-k `(,opcode ,fspec ,dest ,@flags)))))
      ((k:move k:movei k:movea)
       (destructuring-bind (opcode dest source &rest flags) x
         ;; We treat K:MOVE and K:MOVEI exactly the same at this level.  At a lower level,
         ;; we decide whether to use K:MOVE or K:MOVEI based on the source.  We use
         ;; K:MOVE wherever possible to allow maximal merging of moves and calls.
         (declare (ignore opcode))
         (compiling-to-destination (dest source source-type)
           ;; A substitute destination may be supplied if needed.
           (when (memq dest *internal-return-destinations*)
             (fsignal "destination not converted"))
           (unless (eq dest 'k:ignore)
             ;; Don't bother if it says we're not going to use it.
             (output-move dest source flags)))))
      ;;||| load-time case added - smh 18oct88
      ((k:movei-load-time)
       (destructuring-bind (opcode dest source &rest flags) x
           (declare (ignore opcode))
           ;; The source is implicitly always a quoted boxed value.
           (outf-for-k `(k:movei-load-time ,dest ,source ,@(union flags '(k:boxed))))))
      ((k:tail-call)
       (unless (= (length *open-frames*) 1)
         (reg-error "Tail-calling, but there are un-disposed-of open frames."))
       (finish-open-frame t)
       (outf-for-k x)
       (setq *dropthru* nil))
      ((k:nop)
       (outf-for-k x))
      ((k:alu)
       (outf-for-k x))  ;for now, these had better not have any funny destinations, etc.
      ((k:alu-field)
       (destructuring-bind (opcode op dest left-source right-source spec &rest flags) x
         (compiling-to-destination (dest nil source-type)
           (outf-for-k `(,opcode ,op ,dest ,left-source ,right-source ,spec ,@flags)))))
      ((k:memory-wait)
       (outf-for-k x))
      ((k:test)
       (outf-for-k x))
      ((k:branch)
       (outf-for-k x))
      ((k:move-pc)
       (outf-for-k x))
      (otherwise
       (ferror nil "unknown instruction")))) ;;)


(defun output-specpdl-pop (number dest)
  (let ((tail-p (tail-call-p dest)))
    ;; Let POST-PROCESS make OPEN-CALL, otherwise it gets faked out.
    (case number
      (0)
      (1 (tail-call-open 'li:unbind-1 tail-p #'discard-temporary-frame nil)
         (maybe-tail-call '(li:unbind-1 0) 'k:ignore nil tail-p))
      (t (tail-call-open 'li:unbind tail-p #'discard-temporary-frame nil)
         (outi-for-k `(k:movei k:o0 (quote ,number) k:boxed))
         (maybe-tail-call '(li:unbind 1) 'k:ignore nil tail-p)))))


(defun make-dealloc-stack-slots (slots)
  (labels ((generate (open-frame operation dest &optional source source-type)
             (declare (ignore source-type))
             (ecase operation
               (:exist
                (setf (open-frame-there-p open-frame) t))
               ((nil :discard :return)
                (generate-dealloc-stack-slots slots)
                (pop-frame)
                (values dest source)))))
    (when (> slots 0)
      #'generate)))

(DEFUN OUTI1-for-k (X)                          ;Use this for outputing instructions
  (IF *DROPTHRU* (OUTS X)))                     ;known to take delayed transfers

(defun outf-for-k (x)
  (when (listp x)
    (case (first x)
      ((k:move k:movei)
       (when (eq (second x) 'd-return)
         (error "Unconverted D-RETURN in OUTF-FOR-K."))))
    (dolist (p x)
      (etypecase p
        ((or symbol integer))
        (list (case (first p)
                ((k:new-open k:new-open-tail)
                 (error "~S in OUTF-FOR-K." (first p))))))))
  ;; Discard any instructions while we're dropping out.
  (unless (and (not *dropthru*) (listp x))
    (outf x)))

(DEFUN OUTTAG-for-k (X branch-back-possible)
  (WHEN (or branch-back-possible (GET X 'USED))
    (unless *DROPTHRU*
      ;; Always put this out, so use OUTF, not OUTF-FOR-K
      (OUTF '(NO-DROP-THROUGH)))
    (SETQ *DROPTHRU* T)
    (OUTF-for-k X)))

;;; Output an unconditional transfer to the specified prog tag,
;;; popping the pdl the appropriate number of times to adjust the
;;; pdl from its current level to the level required at that tag.

;;; For handling GO, PROGDESC should be NIL.
;;; When jumping to the return tag of a prog, PROGDESC should be
;;; the desc for the prog we are returning from.
(defun outbret-for-k (tag)
  (let ((gotag (gotags-search tag)))
    (when gotag
      ;; If this is GO, set EXITPROGDESC to the progdesc of its containing PROG
      (discarding-open-frames ((progdesc-open-frames (gotag-progdesc gotag)) 'k:r0)
        (outb-for-k `(branch always nil nil ,(gotag-lap-tag gotag))))
      )))

;;; Output a BRANCH instruction
;branch looks like (branch <condition> <sense> <pop-if-no-jump> adr)
; CONDITION can be always, NILIND.
; sense can be TRUE or FALSE.
; POP-IF-NO-JUMP should always be NIL.
(DEFUN OUTB-for-k (X)
  (COND ((not (null (fourth x))) (ferror nil "pop-if-no-jump not nil"))
        ((EQ (CADDR X) 'NO-OP))
        ((EQ (CADDR X) 'RETURN))
        ((NULL *DROPTHRU*))
        (T (PUTPROP (CAR (LAST X)) T 'USED)
           ;(OUTF-for-k X)
           (convert-branch-for-k x)
           )))

(defun outj-for-k (x)   ;output 2 element unconditional branch or jump
  (putprop (car (last x)) t 'used)
  (outf-for-k x)
  (setq *dropthru* nil))

(defun convert-branch-for-k (branch)
  (cond ((eq (cadr branch) 'always)
         (outj-for-k `(k:unconditional-branch ,(car (last branch)))))
        ((eq (cadr branch) 'nilind)
         (let ((inst (cond ((eq (third branch) 'true) 'k:br-zero)
                           ((eq (third branch) 'false) 'k:br-not-zero)
                           (t (ferror nil "bad sense")))))
           (outi-for-k `(k:test ,inst))
           (outi-for-k `(k:branch ,(car (last branch))))
           (putprop (car (last branch)) t 'used)))
        (t (ferror nil "bad condition"))))

;;; This is the other piece of what WITH-OPEN-FRAME does.
;;; It returns the new OPEN-FRAME, so we can check for the right thing later.
(defun outi-open-for-k (open-instruction tail-p cleanup-generator &optional source-type (description open-instruction))
  (prog1 (outi-open-for-k-internal description tail-p nil       ;not there until outi-for-k sees it.
                                   cleanup-generator)
         (outi-for-k open-instruction source-type)))

;;; This is like the above, but doesn't do the output of the instruction.
;;; This is so it can be called from inside OUTI-FOR-K.
(defun outi-open-for-k-internal (open-instruction tail-p there-p cleanup-generator)
    (when tail-p
      (when *open-frames*
        (error "Internal compiler error:  An attempt was made to open for tail-call~@
                when there were already open frames.")))
    (let ((new-frame (make-open-frame :open-instruction open-instruction
                                      :tail-p (not (null tail-p))
                                      :there-p there-p
                                      :cleanup-generator cleanup-generator)))
      (add-frame new-frame)
      new-frame))

;;; This is like OUTI-OPEN-FOR-K-INTERNAL, but doesn't actually involve an instruction
;;; being output.  It's for operations which create a "logical" frame without involving
;;; an actual physical frame, such as stack slots, or bindings, or stack-allocated arrays,
;;; or any other stack-discipline hair that isn't reflected in OUTI-FOR-K's monitoring of
;;; the instruction stream.

(defun outi-fake-open-for-k-internal (operation-name tail-p there-p cleanup-generator)
 ; (if *new-open-frame*
 ;     (error "The last open frame was never actually output.") )
    (when tail-p
      (when *open-frames*
        (error "Internal compiler error:  An attempt was made to open for tail-call~@
                when there were already open frames.")))
    (labels ((fake-cleanup (open-frame operation dest)
              (funcall cleanup-generator open-frame operation dest)
              ;; Since OUTI-FOR-K doesn't know anything about this, we have to do it.
              (ecase operation
                ((:return :discard)
                 (pop-frame)))))
      (let ((new-frame (make-open-frame :open-instruction operation-name
                                        :tail-p (not (null tail-p))
                                        :there-p there-p
                                        :cleanup-generator #'fake-cleanup)))
        (add-frame new-frame)
        new-frame)))


;;; This is what does the normal completion of an open frame.

(defun outi-close-for-k (frame-level k-dest action &rest others)
  (when (or (null *open-frames*) (not (eq frame-level *open-frames*)))
    (reg-error "Mismatch of frame levels; we're not closing the frame we started."))
  (apply (open-frame-cleanup-generator (car *open-frames*))
         (car *open-frames*) action k-dest others))

;;; This cleans up a tail-open.  To do this, we do:
;;; (TAIL-CALL (0 0) NIL NEXT-PC-PC+1)
;;; (TAIL-OPEN-CALL (0 0) NIL NEXT-PC-PC+1)
;;; The analysis is:
;;;                   O A R
;;; Initial state:    A A R  ; O had better have the same as A, on *ANY* tail-call.
;;; Tail open:        F A R  ; F is new from heap.
;;; Tail open:        F F A  ; R goes to heap.
;;; Tail open-call:   A A F  ; Once again in a valid state; with a different R reg.

(defun discard-tail-call-frame ()
  (outf-for-k '(k:tail-call (0 0) k:next-pc-pc+1))
  (outf-for-k '(k:tail-open-call (0 0) nil k:next-pc-pc+1)))



;mostly copied from ORSON:FLEABIT.GENERATE;GENERATE.  Try to keep things as much the same as possible.
(defun generate-incr (dest ptr n &optional (reg 'K:R1))
; this when alui-16 sign extends to 24 bits
;  (emit 'K:ALUI-16 'K:L+R dest ptr `',n)
  (case n
    (0 ;(generate-move ptr dest)
     (if (not (eq dest ptr))
         (emit `(K:MOVE ,dest ,ptr K:BOXED-RIGHT K:BW-24)))
     )
    (1 (emit-alu 'K:R+1 dest ptr ptr '(K:BOXED-RIGHT K:BW-24)))
    (2 (emit-alu 'K:R+2 dest ptr ptr '(K:BOXED-RIGHT K:BW-24)))
    (4 (emit-alu 'K:R+4 dest ptr ptr '(K:BOXED-RIGHT K:BW-24)))
    (t (emit `(K:MOVEI ,reg ',n K:BOXED))
       (emit-alu 'K:R+L dest reg ptr '(K:BOXED-RIGHT K:BW-24)))))

(defun generate-decr (dest ptr n &optional (reg 'K:R1))
  (case n
    (0 ;(generate-move ptr dest)
     (if (not (eq dest ptr))
         (emit `(K:MOVE ,dest ,ptr K:BOXED-RIGHT K:BW-24)))
     )
    (1 (emit-alu 'K:R-1 dest ptr ptr '(K:BOXED-RIGHT K:BW-24)))
    (2 (emit-alu 'K:R-2 dest ptr ptr '(K:BOXED-RIGHT K:BW-24)))
    (4 (emit-alu 'K:R-4 dest ptr ptr '(K:BOXED-RIGHT K:BW-24)))
    (t (emit `(K:MOVEI ,reg ',n K:BOXED))
       (emit-alu 'K:R-L dest reg ptr '(K:BOXED-RIGHT K:BW-24)))))


;;; but this shouldn't alloc for args, (interacts with optional args)
(defun generate-alloc-stack-slots (n)
  (check-type n (integer 0))
  (when (plusp n)
    (generate-incr 'GR:*STACK-POINTER* 'GR:*STACK-POINTER* n)
    (incf *stack-slots* n)))

(defun generate-dealloc-stack-slots (n)
  (check-type n (integer 0))
  (unless (zerop n)
    (generate-decr 'GR:*STACK-POINTER* 'GR:*STACK-POINTER* n)
    (decf *stack-slots* n)))

(defun stack-slot-address (dest slot &optional (reg 'k:R0))
  (generate-decr dest 'gr:*stack-pointer*
                 (- *stack-slots* slot)
                 reg))

(defun read-stack-slot (slot &optional (reg 'K:R0) (insert-wait t))
  (stack-slot-address '(K:VMA-START-READ K:BOXED-VMA K:BOXED-MD) slot reg)
  (if insert-wait (emit '(K:MEMORY-WAIT))))     ;result comes back in K:MD

(defun write-stack-slot (from slot &optional (temporary-register-that-gets-clobbered 'K:R1))
  ;FROM must be addressible from processor right source.
  ;if from is 'ALREADY-IN-MD, then assume it is.  (Can't use K:MD for this because K:MD copying
  ;is sometimes necessary due to multiple md "feature" of hardware.)
  ;--remember boxed-bit decoding is different if dest is VMA or MD. Instruction always specifies both directly.
  (if (not (eq from 'already-in-md))
      (emit `(K:MOVE (K:MD K:BOXED-MD) ,from)))
  (stack-slot-address '(K:VMA-START-WRITE K:BOXED-VMA K:BOXED-MD) slot
                      temporary-register-that-gets-clobbered)
  ;; We need to optimize this out somehow later.  Maybe the peephole optimizer already does it?
  (emit '(K:MEMORY-WAIT)))

(defun move-stack-slot-to-stack-slot (from-slot to-slot)
  (generate-decr '(K:VMA-START-READ K:BOXED-VMA K:BOXED-MD) 'GR:*STACK-POINTER* (- *stack-slots* from-slot))
  (emit '(K:MEMORY-WAIT))
  (generate-decr '(K:VMA-START-WRITE K:BOXED-VMA) 'GR:*STACK-POINTER* (- *stack-slots* to-slot))
  )



(defun o-n (n)
  (aref #(k:o0 k:o1 k:o2 k:o3 k:o4 k:o5 k:o6 k:o7 k:o8 k:o9 k:o10
           k:o11 k:o12 k:o13 k:o14 k:o15)
        n))

(defun a-n (n)
  (aref #(k:a0 k:a1 k:a2 k:a3 k:a4 k:a5 k:a6 k:a7 k:a8 k:a9 k:a10
               k:a11 k:a12 k:a13 k:a14 k:a15)
        n))

(defun a-register-p (x)
  (memq x '(k:a0 k:a1 k:a2 k:a3 k:a4 k:a5 k:a6 k:a7 k:a8 k:a9 k:a10
            k:a11 k:a12 k:a13 k:a14 k:a15)))

(defun r-n (n)
  (aref #(k:r0 k:r1 k:r2 k:r3 k:r4 k:r5 k:r6 k:r7 k:r8 k:r9 k:r10
               k:r11 k:r12 k:r13 k:r14 k:r15)
        n))


(defun k-find-constant-register (quan)
  (let ((probe (cl:assoc quan
                         nc:*global-constants*
                         :test #'equal)))
    (when probe
      (second (cdr probe)))))

;;; This finds the return register to use for value N.  (Value 0 is handled specially).
(defun k-find-return-register (index)
  ;; We really ought to rename the registers 1-30.  --RWK
  (when (< index 1)
    (error "Illegal return register ~D." index))
  (when (> index 30.)
    (error "Cannot return ~D values." (1+ index)))
  (svref #(gr:*return-0*  gr:*return-1*  gr:*return-2*  gr:*return-3*  gr:*return-4*
           gr:*return-5*  gr:*return-6*  gr:*return-7*  gr:*return-8*  gr:*return-9*
           gr:*return-10* gr:*return-11* gr:*return-12* gr:*return-13* gr:*return-14*
           gr:*return-15* gr:*return-16* gr:*return-17* gr:*return-18* gr:*return-19*
           gr:*return-20* gr:*return-21* gr:*return-22* gr:*return-23* gr:*return-24*
           gr:*return-25* gr:*return-26* gr:*return-27* gr:*return-28* gr:*return-29*)
         (1- index)))


(defun k-destination-p (dest)
  (and (symbolp dest)
       (nc:register-number dest)))


;;; This is how you tell if a destination really means D-IGNORE.
(defun d-ignore-p (dest)
  (etypecase dest
    (symbol (or (eq dest 'd-ignore) (eq dest 'k:ignore)))
    (list nil)
    (multiple-values
     (or (null (multiple-values-values dest))
         (cl:every #'d-ignore-p (multiple-values-values dest))))
    (progdesc
     (d-ignore-p (progdesc-idest dest)))
    (open-frame
     (and (d-ignore-p (open-frame-pdest dest))
          (d-ignore-p (open-frame-idest dest))))
    ((or var new-var) nil)))

;;; This is how you tell if a destination really means D-RETURN.
(defun d-return-p (dest)
  (etypecase dest
    (symbol (or (memq dest *return-destinations*)
                (memq dest *internal-return-destinations*)))
    (list nil)
    ;; MULTIPLE-VALUES are only used when we have more to do.
    (multiple-values nil)
    (progdesc
     (d-return-p (progdesc-idest dest)))
    ;; OPEN-FRAMEs are only used when we have more to do.
    (open-frame
     (d-return-p (open-frame-idest dest)))
    ((or var new-var) nil)))



;;; Compute where to place the value initially, before any further processing of this
;;; destination is done.  This returns a new destination, and a new source; these
;;; will usually be the same.  However, if the source is a constant, use that.

(defun compute-temporary-destination (dest &optional source)
  (declare (values new-dest new-source))
  (if (constant-source-p source)
      ;; K:IGNORE is the same as K:R0, except it says you're not going to use it.
      ;; This lets us not move things there.
      (values 'k:ignore source)
    (etypecase dest
      (new-frame-dest
       (if (and source
                (register-static-across-opens-p source))
           (values 'k:ignore source)
         (values 'k:r0 'k:r0)))
      (functional-dest
       (values 'k:r0 'k:r0))
      (register-dest
       (values dest dest))
      (multiple-values
       (if (null (multiple-values-values dest))
           (values 'k:ignore (k-find-constant-register nil))
         (compute-temporary-destination (first (multiple-values-values dest)))))
      (progdesc
       (compute-temporary-destination (progdesc-idest dest)))
      (open-frame
       (compute-temporary-destination (or (open-frame-pdest dest) 'gr:*save-return-crap-0*)))
      (var
       (let ((idest (intermediate-dest-for-store (var-lap-address dest))))
         (values idest idest)))
      (new-var (compute-temporary-destination (new-var-var dest) source))
      ((member d-ignore k:ignore)
       (values 'k:ignore (or source (k-find-constant-register nil))))
      ((member d-return k:return k:return-mv k:return-tail
               k:return-i k:return-i-mv k:return-i-tail)
       (values 'gr:*save-return-crap-0* 'gr:*save-return-crap-0*))
      ((member k:nop)
       (values 'k:ignore source))
      (symbol
       ;; A Special variable
       (values 'k:r0 'k:r0))
      (list
       (ecase (first dest)
         ((local-ref special-ref)
          (compute-temporary-destination (second dest) source))
         (self-ref (values 'k:r0 'k:r0))
         (lexical-ref (values 'k:r0 'k:r0)))))))

;;; Analyze a source to see if it's better handled as a K:MOVEI or
;;; K:MOVE.  DEST enters into this if it's a register, because if the
;;; dest is a global register and the source is a constant or a constant
;;; global register, we'll be better off with a MOVEI.
;;; If it's a reference to a function, use K:MOVEA.  (I think).

(defun analyze-source (source dest)
  (declare (values source movei-p))
  (etypecase source
    (quoted-object
     (let ((reg (k-find-constant-register (second source))))
       (cond (reg
              ;; It can come from a global register, so recurse and do
              ;; that analysis.
              (analyze-source reg dest))
             ((memq (first source) '(function breakoff-function))
              (values `(,(second source) 0) 'k:movea))
             (t ;; It can't come from a global register, so always do K:MOVEI.
              (values source 'k:movei)))))
    (functional-dest
     (values source 'k:move))
    (list (ecase (car source)
            (k:register
             ;; Recurse with it as a symbol, so we can do the analysis there.
             (analyze-source (second source) dest))))
    (symbol
     ;; @#$@#$ This should be NC::REGISTER or COMPILER::REGISTER
     (let* ((source-set (second (get source :register)))
            (constant-val (when source-set
                            (find source nc:*global-constants*
                                       :key #'third))))
       (if (not source-set)
           ;; OK, no more than one global register.
           (values source 'k:move)
         ;; One or more global registers.
         (etypecase dest
           (null (values source 'k:move))
           ((or symbol functional-dest)
            ;; We can K:MOVEI *to* functional destinations.
            ;; @#$@#$ This should be NC::REGISTER or COMPILER::REGISTER
            (let ((dest-set (second (get dest :register))))
              (cond ((not dest-set)
                     ;; OK, only one global register.
                     (values source 'k:move))
                    ((= source-set dest-set)
                     ;; OK, from one place to another in the same set.
                     (values source 'k:move))
                    (constant-val
                     ;; The register is for a constant; we can use K:MOVEI
                     ;; as an alternative to an intermediate register.
                     (values `',(car constant-val) 'k:movei))
                    (t
                     ;; Otherwise we have to use an intermediate register
                     (values source 'k:r0)))))
           (new-frame-dest
            ;; k:new-open and friends.
            (values source 'k:move))
           (list
            ;; Recurse with the symbolic name of the dest.
            (ecase (first dest)
              (k:register (analyze-source source (second dest)))))))))))

(defun convert-dest-for-k (internal-dest)
  ;if dest represents an internal return destination, convert it to K form.
  (selectq internal-dest
    ;** not complete **
    (d-ignore 'k:ignore)
    (d-return 'k:return)
    (d-return-multiple-value 'k:return-mv)
    (d-return-tail 'k:return-tail)
    (otherwise internal-dest)))

(defun output-move (dest source &optional (flags '(k:boxed)))
  ;;||| cosmetic cleanup - smh 18oct88
  (unless (or (eq dest 'd-ignore)
              (eq dest 'k:ignore)
              (equal dest source))
    (multiple-value-bind (source movei-p)
        (analyze-source source dest)
      (let* ((boxed-flags (intersection '(k:boxed k:boxed-right) flags))
             (other-flags (set-difference flags boxed-flags))
             (kdest (convert-dest-for-k dest))
             (dest-flags (case kdest
                           ((k:return k:return-tail k:return-mv
                                      k:return-i k:return-i-tail k:return-i-mv)
                            '(k:ch-return k:next-pc-return))))
             (kflags (union dest-flags other-flags)))
        (ecase movei-p
          ((k:movea)
           (outf-for-k `(k:movea ,kdest ,source
                                 ,@(union kflags (when boxed-flags '(k:boxed))))))
          ((k:movei)
           (outf-for-k `(k:movei ,kdest ,source
                                 ,@(union kflags (when boxed-flags '(k:boxed))))))
          ((k:move)
           (outf-for-k `(k:move ,kdest ,source
                                ,@(union kflags (when boxed-flags '(k:boxed-right))))))
          ((k:r0)
           (outf-for-k `(k:move k:r0 ,source
                                ,@(when boxed-flags '(k:boxed-right))))
           (outf-for-k `(k:move ,kdest k:r0
                                ,@(union kflags (when boxed-flags '(k:boxed-right)))))))))))

;;; Compute the adjustment to the *OPEN-FRAMES* level that will be performed
;;; by a particular destination.
;;; Parallels MOVE-TO-FINAL-DESTINATION.
(defun compute-new-level-for-destination (dest)
  (etypecase dest
    (return-dest
     (restore-frame nil))
    ((member k:ignore d-ignore)
     ;; Ignore it.
     nil)
    (functional-dest)
    (register-dest)
    (new-frame-dest
     (ecase (first dest)
       (k:new-open
        (note-open-frame nil))
       (k:new-tail-open
        (note-open-frame t))))
    (progdesc
     (restore-frame (progdesc-open-frames dest))
     (compute-new-level-for-destination (progdesc-idest dest)))
    (multiple-values
     (let ((open-frame (multiple-values-open-frame dest)))
       (when open-frame
         (add-frame open-frame)
         (note-open-frame open-frame))))
    (open-frame
     (pop-frame)
     ;; Recurse to handle any further movement.
     (compute-new-level-for-destination (open-frame-idest dest)))
    (var)
    (new-var
     (let ((open-frame (new-var-open-frame dest)))
       (when open-frame
         (add-frame open-frame)
         (note-open-frame open-frame))))))


;;; Output a move to a final (complex) destination, doing any cleanup and
;;; processing implied by that destination.

(defun move-to-final-destination (dest source source-type)
  (etypecase dest
    (return-dest
     (move-to-return dest source source-type))
    ((member k:ignore d-ignore)
     ;; Ignore it.
     nil)
    (functional-dest
     ;; +++ Sometime should look at all of the individual implications of these!
     ;; Right now we only have to worry about MD.
     (output-move dest source))
    (register-dest (output-move dest source))
    (new-frame-dest (move-to-new-frame dest source source-type))
    (progdesc (move-to-progdesc dest source source-type))
    (multiple-values (move-to-multiple-values dest source source-type))
    (open-frame (move-to-open-frame dest source source-type))
    (var (move-to-var dest source source-type))
    (new-var (move-to-new-var dest source source-type))))

(defun move-to-return (dest source source-type)
  (let ((new-dest dest))
    (loop while *open-frames*
          do
          (multiple-value-setq (new-dest source)
            (outi-close-for-k *open-frames*
                              (if (null (rest *open-frames*)) dest source)
                              nil source source-type))))
  (output-move dest source)
  (setq *dropthru* nil))

(defun move-to-new-frame (dest source source-type)
  (declare (ignore source-type))
  ;;Must be K:NEW-OPEN or K:NEW-OPEN-TAIL
  (ecase (first dest)
    (k:new-open
     (destructuring-bind (type index &rest ignore) dest
         (declare (ignore type ignore))
         (outi-for-k `(k:open))
         (output-move (o-n index) source)))
    (k:new-tail-open
     (destructuring-bind (type index &rest ignore) dest
         (declare (ignore type ignore))
         (outi-for-k `(k:tail-open))
         (output-move (o-n index) source)))))

(defun move-to-progdesc (dest source source-type)
  (loop with idest = (progdesc-idest dest)
        until (eq *open-frames* (progdesc-open-frames dest))
        do
        (when (and (null *open-frames*)
                   (progdesc-open-frames dest))
          (error "Fell off the bottom searching for the frame to return from."))
        (multiple-value-setq (idest source)
          (outi-close-for-k *open-frames* idest
                            :return source source-type))
        finally                                 ; Recurse to handle where this one goes to.
        (outi-for-k `(k:move ,idest ,source k:boxed-right) source-type)))

(defun move-to-multiple-values (dest source source-type)
  (let* ((dests (multiple-values-values dest))
         (dest0 (first dests))
         (dest-rest (rest dests)))
    (ecase source-type
      ((:last-value)
       ;; All the others have been put in place by the higher level.
       ;; Just see to it that the last one makes it home OK.
       (outi-for-k `(k:move ,dest0 ,source k:boxed-right) :single-value))
      ((:single-value :single-value-flag)
       ;; Fill the other values with NIL
       (loop with nil-reg = (k-find-constant-register nil)
             for i from 1
             for v in dest-rest
             do (outi-for-k `(k:move ,v ,nil-reg k:boxed-right)))
       (outi-for-k `(k:move ,dest0 ,source k:boxed-right)
                   :single-value))
      ((:subr-value :multiple-values-flag)
       (let ((n (length dests)))
         (cond ((= n 0))                        ;No values wanted, we're done.
               ((= n 1)
                ;; Only one value?  We shouldn't have come here.
                (unless (eq source dest0)
                  (outi-for-k `(k:move ,dest0 ,source k:boxed-right) :single-value)))
               ((< 1 n 7)
                (let ((routine (aref #(li:mvbind-1 li:mvbind-2 li:mvbind-3
                                       li:mvbind-4 li:mvbind-5 li:mvbind-6)
                                     (1- n))))
                  (outi-open-for-k `(k:open) nil #'discard-temporary-frame
                                   :single-value `(k:open (,routine 1)))
                  (outi-for-k `(k:move k:o0 ,source k:boxed-right) :single-value)
                  (outi-for-k `(k:call (,routine 1) ,dest0) :single-value)))
               (t
                ;; Start out opening the frame with the second value, so we can use
                ;; K:MOVEI and avoid having to worry about the boundary case where
                ;; we don't have large enough constants.
                (outi-open-for-k `(k:open) nil #'discard-temporary-frame
                                 :single-value '(k:open li:mv-bind 2))
                (outi-for-k `(k:movei k:o1 ',n k:boxed) :single-value)
                (outi-for-k `(k:move  k:o0 ,source k:boxed-right) :single-value)
                (outi-for-k `(k:call (li:mvbind-n 2) ,dest0) :single-value))))
       ;; Transfer all of the the values.
       (loop for d in dest-rest
             for i from 1
             for r = (k-find-return-register i)
             do (if (and (symbolp d)                            ;||| Might be a special var - smh 20oct88
                         (not (get d :register)))
                    (progn (tail-call-open 'symbol:%%set nil #'discard-temporary-frame :single-value)
                           (outi-for-k `(k:movei k:o0 ',d k:boxed))
                           (outi-for-k `(k:move k:o1 ,r k:boxed-right))
                           (maybe-tail-call `(symbol:%%set 2) 'd-ignore :single-value nil))
                  (outi-for-k `(k:move ,d ,r k:boxed-right) :single-value))))
      ((:multiple-values)
       ;; *** This needs a new routine like LI:MVBIND-N that doesn't check the
       ;; *** hardware flag.  Except I bet compiling VALUES will actually set the
       ;; *** flag, so this case is probably useless.
       (error "Can't hack :MULTIPLE-VALUES source type.  I didn't think it existed.")))
    (let ((open-frame (multiple-values-open-frame dest)))
      (when open-frame
        (add-frame open-frame)
        (note-open-frame open-frame)))))

(defun move-to-open-frame (dest source source-type)
  (unless (or (null (open-frame-pdest dest))
              (eq (open-frame-pdest dest) source))
    (outi-for-k `(k:move ,(open-frame-pdest dest) ,source k:boxed-right) :single-value))
  (multiple-value-setq (dest source)
    (outi-close-for-k *open-frames*
                      (open-frame-idest dest)
                      nil source source-type))
  ;; Now recurse to get the value the rest of the way.
  (unless (equal source dest)
    (outi-for-k `(k:move ,dest ,source k:boxed-right) :single-value)))

(defun move-to-var (dest source source-type)
  (ignore source-type)
  (finish-store source (var-lap-address dest)))

(defun move-to-new-var (dest source source-type)
  (ignore source-type)
  (let* ((var (new-var-var dest))
         (lap-adr (var-lap-address var))
         (open-frame (new-var-open-frame dest)))
    (bind-variable var source)
    (when open-frame
      (add-frame open-frame)
      (note-open-frame open-frame))
    nil))

(defun make-mv-mv-dest (dest)
  (declare (values new-dest n-values))
  (let ((n (typecase dest
             (return-dest 31.)
             (functional-dest 1.)
             ((member d-ignore k:ignore) 0)
             (var 1.)
             (register-dest 1)
             (multiple-values (length (multiple-values-values dest)))
             (otherwise 31.))))
    (values
      (case n
        (0 'd-ignore)
        (1 'gr:*save-return-crap-0*)
        (otherwise
         (make-multiple-values
           :values (subseq '(gr:*save-return-crap-0*
                              gr:*return-0* gr:*return-1* gr:*return-2*
                              gr:*return-3* gr:*return-4* gr:*return-5*
                              gr:*return-6* gr:*return-7* gr:*return-8*
                              gr:*return-9* gr:*return-10* gr:*return-11*
                              gr:*return-12* gr:*return-13* gr:*return-14*
                              gr:*return-15* gr:*return-16* gr:*return-17*
                              gr:*return-18* gr:*return-19* gr:*return-20*
                              gr:*return-21* gr:*return-22* gr:*return-23*
                              gr:*return-24* gr:*return-25* gr:*return-26*
                              gr:*return-27* gr:*return-28* gr:*return-29*)
                           0 n))))
      n)))
