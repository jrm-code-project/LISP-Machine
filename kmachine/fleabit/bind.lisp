
(bind n)
...
(call f a0)
(movei a1 n)
(jump unbind-exit)  ;linker makes this mv-unbind-exit if f is mv

(defun unbind-exit (return-value n)
  (li:unbind n)
  return-value)

(defafun mv-unbind-exit (return-value n)
  ;; pass through returned values
  (move a15 r15 r15 bw-24)
  (move a14 r14 br-zero)
  (branch done (alu r-1 r15 r15 r15 bw-24))
  (move a13 r13 br-zero)
  (branch done (alu r-1 r15 r15 r15))
  (move a12 r12 br-zero)
  (branch done (alu r-1 r15 r15 r15))
  (move a11 r11 br-zero)
  (branch done (alu r-1 r15 r15 r15))
  (move a10 r10 br-zero)
  (branch done (alu r-1 r15 r15 r15))
  (move a9 r9 br-zero)
  (branch done (alu r-1 r15 r15 r15))
  (move a8 r8 br-zero)
  (branch done (alu r-1 r15 r15 r15))
  (move a7 r7 br-zero)
  (branch done (alu r-1 r15 r15 r15))
  (move a6 r6 br-zero)
  (branch done (alu r-1 r15 r15 r15))
  (move a5 r5 br-zero)
  (branch done (alu r-1 r15 r15 r15))
  (move a4 r4 br-zero)
  (branch done (alu r-1 r15 r15 r15))
  (move a3 r3 br-zero)
  (branch done (alu r-1 r15 r15 r15))
  (move a2 r2)
 done
  (move o0 a1 ch-open)
  (call li:unbind (a1 r1))
  (nop)
  (return a0))


;;; 9 instructions
(defun li:bind (symbol value)
  ;; read old value
  (hw:vma-start-read-visible-evcp-vma-unboxed-md-boxed (hw:32-1+ symbol))
  ;; save it
  (hw:write-md-boxed (hw:read-md))
  (hw:vma-start-write-boxed gr:*special-pdl*)
  (hw:memory-wait)
  ;; save symbol
  (hw:write-md-boxed symbol)
  (hw:vma-start-write-boxed (hw:32-1+ gr:*special-pdl*))
  ;; bump pointer
  (setq gr:*special-pdl* (hw:32-2+ gr:*special-pdl*)))

;;; 8 instructions
(defun li:unbind-1 ()
  (hw:vma-start-read-vma-boxed-md-boxed (hw:32-1- gr:*special-pdl*) 0)
  (setq gr:*special-pdl* (hw:32-2- gr:*special-pdl*))
  (let ((sym (hw:read-md)))
    (hw:vma-start-read-vma-boxed-md-unboxed gr:*special-pdl*)
    (hw:write-md-boxed (hw:read-md))
    (hw:vma-start-write-unboxed (hw:32-1+ sym))))

(defun li:unbind (n)
  (do () (())
    (hw:vma-start-read-vma-boxed-md-boxed (hw:32-1- gr:*special-pdl*) 0)
    (setq gr:*special-pdl* (hw:32-2- gr:*special-pdl*))
    (let ((sym (hw:read-md)))
      (hw:vma-start-read-vma-boxed-md-unboxed gr:*special-pdl*)
      (hw:write-md-boxed (hw:read-md))
      (hw:vma-start-write-unboxed (hw:32-1+ sym)))
    (if (zerop (setq n (1- n)))
        (return-from li:unbind))))

(defun li:unbind-to (ptr)
  (do ()
      ((eq ptr gr:*special-pdl*))
    (hw:vma-start-read-vma-boxed-md-boxed (hw:32-1- gr:*special-pdl*) 0)
    (setq gr:*special-pdl* (hw:32-2- gr:*special-pdl*))
    (let ((sym (hw:read-md)))
      (hw:vma-start-read-vma-boxed-md-unboxed gr:*special-pdl*)
      (hw:write-md-boxed (hw:read-md))
      (hw:vma-start-write-unboxed (hw:32-1+ sym)))))


(defvar *foo* 3)

(defun foo (a *foo* b)
  (+ *foo* a b))


(lambda (a f_1 b)
  (bind (*foo* f_1)
    (+ f_1 a b)))

($BIND '*foo* f_1)
(+ f_1 a b)
($UNBIND 1)




(defun foo (a *foo* &optional (x (bar)))
  (list x))

(defun bar ()
  *foo*)




(defun li:bind (var value)
  (push (symbol-value var) *special-pdl-ptr*)
  (push var *special-pdl-ptr*)
  (set var value))

(defun li:bind-nil (var)
  (push (symbol-value var) *special-pdl-ptr*)
  (push var *special-pdl-ptr*)
  (set var nil))

(defun li:unbind (n)
  (dotimes (i n)
    (set (pop *special-pdl-ptr*)
         (pop *special-pdl-ptr*))))

(defun li:unbind-to (ptr)
  (do ()
      ((eq *special-pdl-ptr* ptr))
    (set (pop *special-pdl-ptr*)
         (pop *special-pdl-ptr*))))



(defun foo (x)
  (let ((*foo* x))
    (bar)
    (baz)))

(defun foo (x)
 (li:bind '*foo* x)
 (multiple-value-prog1
   (progn
     (bar)
     (baz))
   (li:unbind 1)))

foo
 (movei o0 '*foo* ch-open)
 (call li:bind (o1 a0))
 (open-call bar a1)
 copy r to a ???
 (movei o0 '1 ch-open)
 (tail-call li:unbind)



(values v1 v2 ... v19 v20)
(values v1 v2 ... v15 (list v16 ... v20))

(multiple-value-bind (x1 x2 x3 x4 ... x19 x20)


    ...
  (setq x2 R1)
  (setq x3 R2)
  ...
  (setq x15 R14)
  (setq x16 (pop R15))
  ...
  (setq x20 (pop R15))
