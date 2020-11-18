;;; -*- Mode:LISP; Package:COMPILER; Base:8; Readtable:ZL -*-



(defun tail-call-p (dest &optional fctn)
  (and (d-return-p dest)
       (null *specialflag*)
       (zerop *base-stack-slots*)
       (null *open-frames*)
       (or (null fctn)
           (null *no-tail-call*)                ;During system building
           (not (gethash fctn *no-tail-call*)))))

(defun maybe-tail-call (function dest source-type tail-p)
  (if tail-p
      (outi-for-k `(k:tail-call ,function) source-type)
    (outi-for-k `(k:call ,function ,dest) source-type)))

(defun make-trivial-open-frame (tail-p)
  (labels ((error-to-call-or-discard (open-frame operation dest)
             (declare (ignore dest))
             (case operation
               ((:exist)
                (setf (open-frame-there-p open-frame) t))
               (otherwise
                (error "Attempt to automatically ~A an open frame which should have been trivially handled."
                       (ecase operation
                         ((nil) "call")
                         ((:discard) "discard")
                         ((:return) "return through")))))))
    (make-open-frame :open-instruction nil
                     :tail-p (not (null tail-p))
                     :there-p nil
                     :cleanup-generator #'error-to-call-or-discard)))

;;; NOTE-OPEN-FRAME is called from OUTI-FOR-K level.  If we haven't already been told
;;; what we're outputing (and hence, how it will be cleaned off the stack, or completed)
;;; then it's a trivial call, and our caller is generating a fixed code sequence.  Thus
;;; we just generate a frame that is cleaned off the stack by an explicit CALL.  If it
;;; is ever handled by the automatic mechanisms, an error will be signaled.

(defun note-open-frame (tail-p)
  (declare (ignore tail-p))
  (nc:debug :frames
    (format nc:*debug-stream* "~%NOTE-OPEN-FRAME:~{~18t~a~^~%~}" *open-frames*)
    #+never
    (format nc:*debug-stream* "~% { ~{~a~^ <- ~} }" (cdr (limited-backtrace 12 :compiler))))
  (when (null *open-frames*)
    (error "No frames outstanding"))
  (nc:debug :frames
    (when (open-frame-there-p (car *open-frames*))
      (format t "~%Top frame already there-p: ~a" *open-frames*)
      #+ignore
      (fsignal "Top frame already there-p: ~a" *open-frames*)))
  (funcall (open-frame-cleanup-generator (car *open-frames*))
           (car *open-frames*) :exist nil))

;;; POP-FRAME and ADD-FRAME exist primarily to make it easier to debug.
;;; They can be traced, breakpointed, or otherwise instrumented to follow what happens with
;;; the frames.

(defun pop-frame ()
  (nc:debug :frames
    (format nc:*debug-stream* "~%POPing-FRAME: ~{~18t~a~^~%~}" *open-frames*))
  (when (null *open-frames*)
    (fsignal "Internal compiler error:  Over-pop of open frames."))
  (pop *open-frames*))

(defun add-frame (new-frame)
  (unless (typep new-frame 'open-frame)
    (error "Botch arg to NEW-FRAME: ~s" new-frame))
  (push new-frame *open-frames*)
  (nc:debug :frames
    (format nc:*debug-stream* "~%ADDed-FRAME: ~{~18t~a~^~%~}" *open-frames*))
  *open-frames*)

(defun restore-frame (new-frame-list)
  (nc:debug :frames
    (format nc:*debug-stream* "~%RESTORing-FRAME: ~{~18t~a~^~%~}" *open-frames*))
  (setq *open-frames* new-frame-list)
  (nc:debug :frames
    (format nc:*debug-stream* "~%RESTOREd-FRAME: ~{~18t~a~^~%~}" *open-frames*))
  new-frame-list)

;;; A call was noted at the OUTI-FOR-K level.  This may or may not have been due to automatic
;;; mechanisms, but discard the now-vanished frame, verifying that what we did matched the
;;; OPEN for tailness.

(defun finish-open-frame (tail-p)
  (let ((old (pop-frame)))
    (unless (eql tail-p (open-frame-tail-p old))
      (fsignal "Internal compiler error:  Mismatch of tail-callness for OPEN and CALL."))))

(defun discard-temporary-frame (open-frame operation dest &optional source source-type)
  (let ((tail-p (open-frame-tail-p open-frame)))
    (ecase operation
      ((nil)
       (cond ((or (and (eq dest source)
                       (not (find dest #(k:o0 k:o1 k:o2 k:o3 k:o4 k:o5 k:o6 k:o7
                                              k:o8 k:o9 k:o10 k:o11 k:o12 k:o13 k:o14 k:o15))))
                  (eq dest 'd-return))
              (maybe-tail-call '(ignore 0) dest source-type tail-p)
              (values dest source))
             (t (outi-for-k `(k:move k:o0 ,source k:boxed-right))
                (maybe-tail-call '(li::prog1-internal 1) dest source-type tail-p)
                (values dest dest))))
      ((:exist)
       (setf (open-frame-there-p open-frame) t))
      ((:return :discard)
       (if tail-p
           (discard-tail-call-frame)
         (outi-for-k `(k:call (ignore 0) k:ignore)))
       (values dest source)))))



;;; N = number of values computed, or NIL to mean the number computed is in
;;; K:O15 of the innermost frame.  In this latter case, there will be a
;;; full complement of values in the O-registers, waiting to be moved as needed.
;;; FINISH-VALUES-2N will move this count to  GR:*NUMBER-OF-RETURN-VALUES*,
;;; which is where FINISH-VALUES-N will find it.
;;; This hair is to allow saving multiple-value blocks for MULTIPLE-VALUE-PROG1.

#+K ;;Doesn't belong in this file, here for illustration
(defun li:finish-values-N (v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15)
  ;; This could be optimized with a dispatch to only move those that count.
  (setq gr:*return-0* v1
        gr:*return-1* v2
        gr:*return-2* v3
        gr:*return-3* v4
        gr:*return-4* v5
        gr:*return-5* v6
        gr:*return-6* v7
        gr:*return-7* v8
        gr:*return-8* v9
        gr:*return-9* v10
        gr:*return-10* v11
        gr:*return-11* v12
        gr:*return-12* v13
        gr:*return-13* v14
        gr:*return-14* v15)
  (case gr:*number-of-return-values*
    (1 v0)
    (otherwise (hw:return-mv v0))))

#+K ;;Doesn't belong in this file, here for illustration
(defun li:finish-values-2n (v16 v17 v18 v19 v20 v21 v22 v23 v24 v25 v26 v27 v28 v29 v30 vcount)
  (setq gr:*return-15* v16
        gr:*return-16* v17
        gr:*return-17* v18
        gr:*return-18* v19
        gr:*return-19* v20
        gr:*return-20* v21
        gr:*return-21* v22
        gr:*return-22* v23
        gr:*return-23* v24
        gr:*return-24* v25
        gr:*return-25* v26
        gr:*return-26* v27
        gr:*return-27* v28
        gr:*return-28* v29
        gr:*return-29* v30
        gr:*number-of-return-values* vcount)
  v16)


(defun p2prog12mv-for-k (n dest argl &optional mv-p)
  (decf n)                              ;Convert to origin 0.
  ;; Compile the args before the one whose value we want.
  (dotimes (i n)
    (p2-for-k (or (pop argl) ''nil) 'd-ignore))
  ;; Compile the arg whose value we want.
  (let ((tail-p (tail-call-p dest 'multiple-value-prog1)))
    (cond ((and (null (cdr argl)) *bdest*)
           (p2branch-for-k (or (car argl) ''nil) dest *bdest*)
           (setq *bdest* nil))
          ;; Stash the value to be returned in an OPEN frame.
          (mv-p
           ;; First, put all the values in the return-value registers.
           (multiple-value-bind (inter-dest inter-values)
               (make-mv-mv-dest dest)
             (p2-for-k (or (first argl) `'nil) inter-dest)
             ;; Now, create the frames.
             (when (> inter-values 0)
               ;; There is some inefficiency here.  It shouldn't be necessary always to use
               ;; two frames, or even one frame if only a single value is needed.
               ;; I think the reason the mv frame gets built anyway is that it is important
               ;; that someone sets the return values count, and this wouldn't always happen
               ;; without some mechanism to do it. - smh 15oct88
               (tail-call-open 'multiple-value-prog1 tail-p
                               (if mv-p
                                   (compute-finish-values-frame nil tail-p)
                                 #'discard-temporary-frame)
                               :multiple-values)
               (outi-for-k `(k:move k:o0 gr:*save-return-crap-0* k:boxed-right))
               (loop for i from 1 below (min 16. inter-values)
                     do (outi-for-k `(k:move ,(o-n i) ,(k-find-return-register i) k:boxed-right)))
               ;; Gotta always output this, for the count.
               (outi-open-for-k `(k:open) nil (compute-finish-values-frame-2 nil))
               (loop for i from 16. below (min 31. inter-values)
                     do (outi-for-k `(k:move ,(o-n (- i 16.))
                                             ,(k-find-return-register i) k:boxed-right)))
               ;; Salt away the count, too.
               (outi-for-k `(k:move k:o15 gr:*number-of-return-values* k:boxed-right)))
             ;; Compile the rest of the arguments.
             (dolist (arg (cdr argl))
               (p2-for-k arg 'd-ignore))
             ;;||| Frames weren't created for D-IGNORE - smh 15oct88
             (unless (= inter-values 0)
               (outi-close-for-k *open-frames* dest nil 'k:o0 :multiple-values)
               (outi-close-for-k *open-frames* dest nil 'k:o0 :multiple-values))))
          (t (tail-call-open 'li:prog1-internal tail-p #'discard-temporary-frame
                             :single-value)
             (p2-for-k (or (first argl) `'nil) 'k:o0)
             (dolist (arg (cdr argl))
               (p2-for-k arg 'd-ignore))
             (outi-close-for-k *open-frames* dest nil 'k:o0 :single-value)))))


(defun fast-multiple-value-dest (dest &optional (start 0) end open-frame-p)
  (labels ((register-dynamic-across-opens-p (reg)
            (typecase reg
              (var (not (var-ok-p reg)))
              (new-var (not (var-ok-p (new-var-var reg))))
              (otherwise (not (register-static-across-opens-p reg)))))
           (var-ok-p (var)
            (let ((lap-address (var-lap-address var)))
              (case (first lap-address)
                (special
                 ;; If there are any intervening open frames, we cannot,
                 ;; because they might be an UNBIND.
                 (not open-frame-p))
                (otherwise t)))))
    (typecase dest
      (symbol
       (unless (or (memq dest *return-destinations*)
                   (memq dest *internal-return-destinations*))
         dest))
      (functional-dest nil)
      (new-frame-dest nil)
      (list
       (ecase (first dest)
         (k:register (second dest))))
      (multiple-values
       (unless (find-if #'register-dynamic-across-opens-p
                        (multiple-values-values dest)
                        :start start :end end)
         dest))
      (progdesc
       (fast-multiple-value-dest (progdesc-idest dest) start end
                                 (neq (progdesc-open-frames dest)
                                      *open-frames*)))
      (open-frame
       (cond ((open-frame-idest dest)
              (fast-multiple-value-dest (open-frame-idest dest) start end t))
             ((equal (open-frame-open-instruction dest)
                     '(k:open *throw))
              ;; Throw is a special case.  It always accepts multiple values, trivially.
              dest)))
      (var
       (when (var-ok-p dest)
         dest))
      (new-var
       (when (var-ok-p (new-var-var dest))
         dest))
      (otherwise nil))))

(defun output-fast-multiple-values (n real-dest dest source source-type start end)
  (case n
    (0 (outi-for-k `(k:move k:o0 gr:*nil* k:boxed-right))
       (values dest source))
    (otherwise
     (if (eq source 'k:o0)
         (values dest dest)
       (outi-for-k `(k:move k:o0 ,source k:boxed-right) source-type)
       (values dest source))))
  (etypecase dest
    ((or var new-var symbol)
     (outi-for-k `(k:call (li::prog1-internal 1) ,real-dest) :last-value)
     (values dest source))
    (multiple-values
     (loop with begin = (max start 1)
           for i from begin below end
           for o from 1
           for vdest in (nthcdr begin (multiple-values-values dest))
           do (outi-for-k `(k:move ,vdest ,(o-n o) k:boxed-right) :single-value))
     (outi-for-k `(k:call (li::prog1-internal 1) ,real-dest) :last-value)
     (values dest source))))


(defun output-slow-multiple-values (n dest source source-type function &optional tail-p)
  (unless n
    (setq n 16.))
  (cond ((or (and (eq dest source)
                  (not (find dest #(k:o0 k:o1 k:o2 k:o3 k:o4 k:o5 k:o6 k:o7
                                         k:o8 k:o9 k:o10 k:o11 k:o12 k:o13 k:o14 k:o15))))
             (eq dest 'd-return))
         (maybe-tail-call `(,function ,n) dest source-type tail-p)
         (values dest source))
        ((eq source 'k:o0)
         (maybe-tail-call `(,function ,n) dest source-type tail-p)
         (values dest dest))
        (t (outi-for-k `(k:move k:o0 ,source k:boxed-right))
           (maybe-tail-call `(,function ,n) dest source-type tail-p)
           (values dest dest))))



(defun compute-finish-values-frame (n tail-p)
  (labels ((finish-values-frame (open-frame operation dest &optional source source-type)
             (ecase operation
               ((nil)
                (let ((fast-dest (fast-multiple-value-dest dest)))
                  (if fast-dest
                      ;; The fast case cannot be d-return anyway, no need to hack tail-p
                      (output-fast-multiple-values n dest fast-dest source source-type 0 16.)
                    ;; Gotta go through the return temporaries.  Start out by setting up
                    ;; the count.
                    (outi-for-k `(k:movei gr:*number-of-return-values* ',n k:boxed))
                    (output-slow-multiple-values (when n (min n 16.))
                                                 dest source source-type
                                                 (if n 'finish-values
                                                   'finish-values-n)
                                                 tail-p))))
               ((:exist)
                (setf (open-frame-there-p open-frame) t))
               ((:return :discard)
                (if (not tail-p)
                    (outi-for-k `(k:call (ignore 0) k:ignore))
                  (discard-tail-call-frame)
                  (pop-frame))
                (values dest source)))))
    #'finish-values-frame))

(defun compute-finish-values-frame-2 (n)
  (labels ((finish-values-frame-2 (open-frame operation dest &optional source source-type)
             (ecase operation
               ((nil)
                (let ((fast-dest (fast-multiple-value-dest dest)))
                  (etypecase fast-dest
                    (null (output-slow-multiple-values (when n (- n 16.))
                                                       'd-ignore 'k:o0 :single-value
                                                       (if n 'finish-values-2
                                                         'finish-values-2n)))
                    ((or var new-var symbol)
                     ;; A destination that only takes one, just discard the frame.
                     (outi-for-k `(k:call (ignore 0) k:ignore)))
                    (multiple-values
                     (let ((dest-17 (or (nth 16. (multiple-values-values fast-dest)) 'k:ignore)))
                       (output-fast-multiple-values n dest-17 fast-dest source source-type 17. 31.)))))
                (values dest source))
               ((:exist)
                (setf (open-frame-there-p open-frame) t))
               ((:return :discard)
                (outi-for-k `(k:call (ignore 0) k:ignore))))))
    #'finish-values-frame-2))



;;; This sort of thing is probably better done as a source transformation,
;;; but this trivial case is easily enough done here.  If all the values
;;; are ADRREFP-FOR-K, we can shuffle the first to be last, and then move them
;;; all directly in, except for the first value, which we may need to
;;; use a temporary for, so we can trigger any unwinding needed on the
;;; real destination.

(defun trivial-p2values (argl dest fast)
  (typecase fast
    (multiple-values
     (destructuring-bind (first-arg &rest rest-arg) argl
       (destructuring-bind (first-dest &rest rest-dest)
                           (multiple-values-values fast)
         (when first-arg
           (loop for argp = rest-arg then (rest argp)
                 for argi = (first argp)
                 for desti in rest-dest
                 do
                 (if argp
                     (p2-for-k argi desti)
                   (outi-for-k `(k:move ,desti gr:*nil* k:boxed-right)
                               :single-value)))
           (multiple-value-bind (new-dest new-source)
               (compute-temporary-destination first-dest)
             (p2-for-k first-arg new-dest)
             (outi-for-k `(k:move ,dest ,new-source k:boxed-right) :last-value))))))
    (open-frame
     (typecase fast
       (open-frame
        (unless (equal (open-frame-open-instruction fast) '(k:open *throw))
          (error "Internal error: Only a THROW open frame is a suitable destination for fast MULTIPLE-VALUES."))))
     (destructuring-bind (first-arg &rest rest-arg) argl
       (loop for arg in rest-arg
             for i from 1
             for dest = (k-find-return-register i)
             do
             (p2-for-k arg dest))
       ;; Output the count before the last (first) arg so that the last/first arg can
       ;; be incorporated into the call.
       (outi-for-k `(k:movei gr:*number-of-return-values* ',(length argl) k:boxed))
       (p2-for-k (or first-arg `'nil) 'k:o1)
       (outi-close-for-k (member fast *open-frames*) 'k:ignore nil 'k:o1 :last-value)))
    (otherwise
     ;; Only one value wanted anyway.
     (p2-for-k (first argl) dest))))

(defprop values p2values-for-k p2-for-k)

;;; The basic strategy for the most general case is to compile everything
;;; into open frames (up to two), and then to call routines to store
;;; them into the return registers and set the h.w. m.v. flag.  At the same
;;; time we store the 0th value into its home as the result of the call.
;;;
;;; Two special cases are optimized:  If all the values are trivial (i.e.
;;; ADRREFP-FOR-K), we just move the values directly to their final homes, and
;;; if all the destinations are apparent and unaffected by things like
;;; unbinding special variables or unwinding open registers, we can
;;; move things directly to their homes inline, rather than via the
;;; return registers.

(defun p2values-for-k (argl dest)
  ;; Handle returning from the top level of a function.
  (let ((nargs (length argl))
        (nodropthru nil))
    (when (> nargs 30.)                 ;This wants to be return-values-limit or whatever, but
                                        ;this is a cross-compiler; we don't want the LAMBDA's limit.
      (barf NARGS "Too many return values"))
    (when (cl:every #'adrrefp-for-k argl)
      (let ((fast (fast-multiple-value-dest dest)))
        (when fast
          ;; All are ADDREFP; we can send the values directly.
          (return-from p2values-for-k
            (trivial-p2values argl dest fast)))))
    (labels ((p2-multiple (&optional (limit nargs))
              (if argl
                  (let ((frame-levels)
                        (tail-p (tail-call-p dest 'values)))
                    ;; Since we're going to call P2 to compute the values, we can't just
                    ;; generate this as a fixed OPEN ... CALL sequence.  We have to allow
                    ;; for the possibility of branches out of the main line, so we have to
                    ;; use the hairier mechanism.
                    (loop for i from 0 to (1- (max limit nargs))
                          for arg in argl
                          for reg = (o-n (mod i 16.))
                          do
                          (when (zerop (mod i 16.))
                            (if (zerop i)
                                (tail-call-open 'compute-finish-values-frame tail-p
                                                (compute-finish-values-frame limit tail-p)
                                                :multiple-values)
                              (outi-open-for-k `(k:open) nil (compute-finish-values-frame-2 limit)
                                               nil `(k:open compute-finish-values-frame-2)))
                            ;; Remember which open frames were in effect so we can check
                            ;; that we got the right ones when we close up.
                            (push *open-frames* frame-levels))
                          (cond (( i nargs)
                                 (outi-for-k `(k:move ,reg ,(k-find-constant-register nil) k:boxed-right)))
                                (( i limit)
                                 (p2-for-k arg 'd-ignore))
                                (t (p2-for-k arg reg))))
                    (when (> limit 16.)
                      ;; We have a second frame to discard.
                      (outi-close-for-k (pop frame-levels) 'k:ignore nil 'k:o0 :single-value))
                    ;; Call the last frame-closer.
                    (outi-close-for-k (pop frame-levels) dest nil 'k:o0
                                      (if (= nargs 1)
                                          :single-value
                                        :multiple-values-flag)))
                ;; Already filtered out the cases which care.
                (outi-for-k `(k:movei ,dest 'nil k:boxed) :single-value))
             (when nodropthru
               (setq *dropthru* nil))))
      (cond ((d-return-p dest)
             (setq nodropthru t)
             (COND ((= nargs 0)
                    (outi-for-k `(k:movei gr:*number-of-return-values* '0 k:boxed))
                    (outi-for-k `(k:movei k:return-mv 'nil k:boxed) :multiple-values))
                   ((= NARGS 1)
                    ;; DON'T change this to (P2 ... 'D-RETURN)
                    ;; because we want to make sure to pass only one value.
                    (p2-for-k (car argl) 'k:r0)
                    (outi-for-k `(k:move k:return k:r0 k:boxed-right) :single-value)
                    NIL)
                   (T (p2-multiple))))
            ((and (typep dest 'open-frame)
                  (= nargs 0))
             (outi-for-k `(k:movei gr:*number-of-return-values* '0 k:boxed))
             (outi-for-k `(k:movei ,dest 'nil k:boxed) :multiple-values))
            ((typep dest 'multiple-values)
             (p2-multiple (length (multiple-values-values dest))))
            (t (p2-multiple))))))

;;; Convert an mv-init list and vars to a destination.

(defun convert-mvlist-to-dest (vlist vars dest &optional new-p)
  (loop for vspec in vlist
        for (name init) = vspec
        for var = (find name vars :key #'var-name)
        for nvar = (if new-p
                       (make-new-var :var var)
                     var)
        collect var into dests
        count (and new-p
                   (memq (first (var-lap-address var))
                         '(special remote)))
          into unbinds
        finally
        (let ((open-frame (unless (zerop unbinds)
                            (make-unbind-open-frame unbinds dest))))
          (return (values (make-multiple-values
                            :values dests
                            :open-frame (when new-p open-frame))
                          open-frame)))))

(defun (:property multiple-value-bind p2-for-k) (mvb-p2form dest)
  ;; The first "argument" is the multiple-value producing form.
  ;; Remove that and what you have is the same as for a LET in pass 2.
  (destructuring-bind (mvform &rest let-p2form) mvb-p2form
    (destructuring-bind (vlist *vars* nvars) let-p2form
      (multiple-value-bind (new-dest open-frame)
          (convert-mvlist-to-dest vlist nvars dest t)
        (let ((*vars* nvars))
          (opening-frames (dest (progn (p2-for-k mvform new-dest)
                                       (when open-frame (add-frame open-frame))
                                       open-frame))
            (p2let-internal-for-k *vars* let-p2form dest)))))))

(defun (:property nth-value P2-for-k) (p2form dest)
  (destructuring-bind (value-number form) p2form
    (typecase value-number
      ((integer 0 0)
       (p2 `(values ,form) dest))
      ((integer 0 30.)
       (let ((new-dest (make-multiple-values
                         ;; ||| Did this ever work?  Added :INITIAL-ELEMENT - smh 15oct88
                         :values (nconc (make-list value-number :initial-element 'd-ignore)
                                        (list dest)))))
         (p2-for-k form new-dest)))
      (otherwise
       ;; This wouldn't be hard to make work.  (P1 would have to be
       ;; changed).  A small run-time subroutine with a dispatch, and
       ;; an open-frame to receive the variables specially.
       (barf p2form "~S p2 lost" 'nth-value)))))

(defun (:property multiple-value p2-for-k) (p2form dest)
  (destructuring-bind (variables form) p2form
    (labels ((variable-dest (frob)
              (typecase frob
                (list (second frob))
                (otherwise frob))))
      (destructuring-bind (&optional (retval dest) &rest other-vals)
                          (cl:map 'list #'variable-dest variables)
          (multiple-value-bind (new-retval new-retsource)
              (compute-temporary-destination retval)
            (let ((new-dest (make-multiple-values
                              :values (list* new-retval other-vals))))
              (p2-for-k form new-dest)
              (outi-for-k `(k:move ,dest ,new-retsource k:boxed-right))))))))


;;; LI:MV-PROG1-INTERNAL is just like LI:PROG1-INTERNAL, except it
;;; does a K:TAIL-RETURN so it leaves the m.v. flag alone.

(defun (:property multiple-value-prog1 p2-for-k) (argl dest)
  (p2prog12mv-for-k 1 dest argl t))

(defun discard-mvprog1-frame (open-frame operation dest &optional source source-type)
  (ecase operation
    ((nil)
     (cond ((or (and (eq dest source)
                     (not (find dest #(k:o0 k:o1 k:o2 k:o3 k:o4 k:o5 k:o6 k:o7
                                            k:o8 k:o9 k:o10 k:o11 k:o12 k:o13 k:o14 k:o15))))
                (eq dest 'd-return))
            (outi-for-k `(k:call (ignore 0) k:ignore))
            (values dest source))
           ((eq source 'k:o0)
            (outi-for-k `(k:call (li::mv-prog1-internal 1) ,dest) source-type)
            (values dest dest))
           (t (outi-for-k `(k:move k:o0 ,source k:boxed-right))
              (outi-for-k `(k:call (li::mv-prog1-internal 1) ,dest)
                          source-type)
              (values dest dest))))
    ((:exist)
     (setf (open-frame-there-p open-frame) t))
    ((:return :discard)
     (outi-for-k `(k:call (ignore 0) k:ignore)))))


(defun (:property multiple-value-list p2-for-k) (p2form dest)
  (destructuring-bind (mvform) p2form
      (if (d-ignore-p dest)
          (p2-for-k mvform dest)
        (let ((tail-p (tail-call-p dest 'multiple-value-list)))
          (labels ((construct-mv-list (open-frame operation dest &optional source source-type)
                     (nc:debug :frames
                       (format nc:*debug-stream* "~%MV-LIST-int OPERATION=~a DEST=~a SOURCE=~a SOURCE-TYPE=~a"
                               operation dest source source-type)
                       #+never(format nc:*debug-stream* "~% { ~{~a~^ <- ~} }" (cdr (limited-backtrace 50 :compiler))))
                     (unless (eq open-frame (first *open-frames*))
                       (error "MULTIPLE-VALUE-LIST frame not the top frame on the stack."))
                     (pop-frame)                                        ;<<+++ This can't be right here!!! -smh
                     (ecase operation
                       ((nil)
                        (ecase source-type
                          ((:multiple-values-flag :subr-value)
                           (tail-call-open 'li:mv-list tail-p #'discard-temporary-frame
                                           :single-value)
                           (maybe-tail-call '(li:mv-list 0) dest :single-value tail-p))
                          ((:single-value :single-value-flag)
                           (tail-call-open 'list tail-p #'discard-temporary-frame
                                           :single-value)
                           (outi-for-k `(k:move k:o0 ,source k:boxed-right) :single-value)
                           (maybe-tail-call '(list 1) dest :single-value tail-p))
                          ((nil)
                           ;; This happens when the body has already returned its result.
                           ;; Check out: (multiple-value-list (catch 'bar (foo)))
                           ;; So try doing nothing. -smh 1Sep88
                           )
                          )
                        (nc:debug :frames
                          (format nc:*debug-stream* "~%MV-LIST-int returning [~a ~a]" dest source))
                        ;; (values dest source) ;changed 14sep88 smh
                        (values dest dest))
                       ;; I think this is never supposed to happen, so let ecase tell
                       ;; us it if does. -smh 1sep88
                       ;;#+never Yes, it can happen. <15-Nov-88 smh>
                       ((:exist)
                        (setf (open-frame-there-p open-frame) t))
                       ((:discard :return)
                        (values dest source)))))                ;|||Return values added - 19oct88 smh
            (let ((new-frame (make-open-frame :open-instruction 'mv-list
                                              :there-p t
                                              :tail-p tail-p
                                              :cleanup-generator #'construct-mv-list
                                              :idest dest)))
              (opening-frames (dest new-frame)
                (add-frame new-frame)
                (p2-for-k mvform dest))))))))
