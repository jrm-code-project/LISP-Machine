;;; -*- Mode:LISP; Package:COMPILER; Base:8; Readtable:ZL -*-


(defun (:property list p1cross) (form)
  (let ((fn (case (length (cdr form))
              (0 'nil)
              (1 'li:ncons)
              (2 'li:list2)
              (3 'li:list3)
              (4 'li:list4)
              (t 'li:listn))))
    (p1 `(,fn . ,(cdr form)))))

(defun (:property list* p1cross) (form)
  (let ((fn (case (length (cdr form))
              (t 'li:list*))))
    (p1 `(,fn . ,(cdr form)))))

(defun (:property list-in-area p1cross) (form)
  (cl:warn "Treating a LIST-IN-AREA as if LIST")
  (p1 `(LIST . ,(cddr form))))

(defun (:property list*-in-area p1cross) (form)
  (cl:warn "Treating a LIST*-IN-AREA as if LIST*")
  (p1 `(LIST* . ,(cddr form))))

(defprop list-in-area p2list-cross p2-for-k)
(defprop list*-in-area p2list-cross p2-for-k)
(defun p2list-cross (argl dest &aux area)
  (declare (ignore dest))
  (when (memq *p2fn* '(list-in-area list*-in-area))
    (setq area (pop argl)))
  (fsignal "convert this"))

(DEFUN (:PROPERTY ATOM P2-FOR-K) (ARGL DEST) ;just treat ATOM normally.  This effectively overrides the normal P2.
  (P2MISC-FOR-K 'ATOM ARGL DEST 1))

;;; NOT compiles into a misc insn normally,
;;; but with a branch destination, it optimizes away by inverting the condition.
(DEFUN (:PROPERTY NOT P2-FOR-K) (ARGL DEST)
  (IF (OR (EQ (CADR *BDEST*) 'NILIND)
          (EQ (CADR *BDEST*) 'ATOMIND))
      (LET ((SENSE (OTHER (CADDR *BDEST*))))
        (P2BRANCH-for-k (CAR ARGL) DEST `(BRANCH ,(CADR *BDEST*) ,SENSE . ,(CDDDR *BDEST*)))
        (SETQ *BDEST* NIL))
    (P2MISC-for-k 'NOT ARGL DEST 1)))

(DEFUN (:PROPERTY SETQ P2-for-k) (ARGL DEST)
  (PROG ()
        (OR ARGL (RETURN (p2-for-k '(quote nil) dest)))
     LOOP
        (P2SETQ-1-for-k (CAR ARGL) (CADR ARGL)
                        (COND ((NULL (CDDR ARGL)) DEST)
                              (T 'D-IGNORE)))
        (SETQ ARGL (CDDR ARGL))
        (AND ARGL (GO LOOP))))

;;; Compile code to set VAR to the result of computing VALUE,
;;; and also move that value to DEST.
(DEFUN P2SETQ-1-for-k (VAR VALUE DEST)
  (COND ((MEMQ VAR '(NIL T))
         NIL)
        ;; The next case appear identical to the default.
        ;;((AND (CONSP VAR) (EQ (CAR VAR) 'LEXICAL-REF))
        ;; (movem-and-move-to-dest-for-k value var dest))
        (T
         (movem-and-move-to-dest-for-k value var dest)))
  NIL)

(defun movem-and-move-to-dest-for-k (value var dest)
  ;;compile VALUE, storing the result in VAR, and also in DEST.
  ;; VAR is a REF type construct.
  ;; DEST is any dest acceptable to P2-FOR-K. NOTE: that means its an INTERNAL dest!!
  ;;** idea for special hack..  Change these k:r0 s to k:r15.  Then peephole optimizer
  ;; could assume the contents on any k:r(n) is used at most once and do some optimizations
  ;; (particularily wrt MD).  Cheap flow analysis.
  (let ((inter-var
          (when (listp var)
            (case (car var)
              ((local-ref)
               (intermediate-dest-for-store (var-lap-address (cadr var))))))))
    (multiple-value-bind (inter-dest inter-reg)
        (compute-temporary-destination var inter-var)
      ;;salt it in var, if necessary.
      (cond ((and (symbolp var)
                  (get var :register))                          ; @#$@#$ This should be NC::REGISTER or COMPILER::REGISTER
             (p2-for-k value inter-dest)
             (outi-for-k `(k:move ,var ,inter-reg k:boxed-right) :single-value))
            ((atom var)
             ;; Store from inter-reg into special variable
             ;; Let POST-PROCESS make OPEN-CALL, otherwise it gets faked out.
             (p2-for-k value inter-dest)
             (let ((tail-p (tail-call-p dest 'symbol:%%set)))
               (tail-call-open 'symbol:%%set tail-p #'discard-temporary-frame
                               :single-value)
               (outi-for-k `(k:movei k:o0 (quote ,var) k:boxed))
               (outi-for-k `(k:move k:o1 ,inter-reg k:boxed-right))
               (return-from movem-and-move-to-dest-for-k
                 (maybe-tail-call `(symbol:%%set 2) dest :single-value tail-p)))
             ;;calling subroutine anyway, and %%set returns as value the right thing.
             ;;so putting dest in subroutine call solves whole problem and avoids any
             ;;possibility of K:R0 bashage.
             )
            ((eq (car var) 'local-ref)
             (p2-for-k value inter-dest)
             (let ((lap-address (var-lap-address (cadr var))))
               (finish-store inter-reg lap-address dest)))
            ((eq (car var) 'self-ref)
             ;; We delay computing value until after new frame gets opened.
             (iv-ref-for-k var 'li:instance-var-write inter-reg))
            ((eq (car var) 'lexical-ref)
             (p2-for-k value inter-dest)
             (let* ((tail-p (tail-call-p dest))
                    (ref-spec (second var))
                    (level (ldb (byte 12. 12.) ref-spec))
                    (offset (ldb (byte 12. 0.) ref-spec)))
               (tail-call-open 'li:closure-set tail-p #'discard-temporary-frame
                               :single-value)
               (closure-set-call level offset inter-reg dest tail-p)))
            (t (ferror nil "Compiler internal error -- can't finish store")))
      ;;salt it in dest, if necessary.
      (cond ((eq dest 'd-ignore) nil)
            ((eq dest inter-reg) nil)           ;already in place
            #+never ((k-destination-p dest)     ; $$$ This clause effectively same as the last. <17-Nov-88 smh>
                     (outi-for-k `(k:move ,dest ,inter-reg k:boxed-right)))
            (t (outi-for-k `(k:move ,dest ,inter-reg k:boxed-right)
                           :single-value))))))  ; $$$ added :SINGLE-VALUE <17-Nov-88 smh>

(DEFUN (:PROPERTY PROGN-WITH-DECLARATIONS P2-for-k) (ARGL DEST)
  (LET ((*VARS* (CAR ARGL)))
    (progn-for-k DEST (CDR ARGL))))

(defun (:property progn p2-for-k) (argl dest)
  (progn-for-k dest argl))

(defun progn-for-k (dest argl)
  (loop for argx on argl
        for arg = (first argx)
        while (rest argx)
        do (with-frames                                         ;added 8sep88 - smh
             (p2-for-k arg 'd-ignore))
        finally (p2-for-k arg dest)))

(defun (:property prog2 p2-for-k) (argl dest)
  (p2prog12mv-for-k 2 dest argl nil))
