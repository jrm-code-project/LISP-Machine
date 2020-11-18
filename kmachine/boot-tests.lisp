
;;;; This stuff used to be in BOOT

(defun test-tak ()
  (when (not (= (tak 18. 12. 6.) 7.))
    (trap::illop "Tak returned the wrong result.")))

(defun tak (x y z)
 ; (ensure-interrupts-are-on)
  (if (not (< y x))
      z
      (tak (tak (1- x) y z)
           (tak (1- y) z x)
           (tak (1- z) x y))))

(defun test-tak-with-lights ()
    (labels ((loop (n)
               (modify-leds (hw:ldb-not n (byte 3. 0.) 0))
               (let ((region (make-test-consing-region)))
                 (tak 18. 12. 6.)
                 ;; Primitive Garbage Collection
                 ;; Reset the free pointer.
                 (region-data:free-region region))
        ;       (trap::illop "Region freed.")
               (loop (1+ n))))
      (loop 0)))

(defun ensure-interrupts-are-on ()
  (when (= hw:$$trap-disable (hw:ldb (hw:read-memory-control) hw:%%memory-control-master-trap-enable 0))
    (trap::illop "Traps are off.")))

(defun test-fib ()
  (when (not (= (fib 4.) 3))
    (trap::illop "Fib returned the wrong result.")))

(defun fib (n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(defun fib (x)
  (fib-iter x 1 0))

(defun fib-iter (x ans prev)
  (if (= x 0)
      ans
      (fib-iter (1- x) (+ ans prev) ans)))

(defun hanoi (from to other n)
  (prims:when (= n 0)
      '()
      (progn
        (hanoi from other to (1- n))
        (hanoi other to from (1- n)))))

(defafun test-branch ()
  (movei a0 0)

 test-branch-foo
  (move nop a0)
  (test br-zero)
  (branch test-branch-was-not-zero ())

  (movei a0 0)
  (unconditional-branch test-branch-foo ())

 test-branch-was-not-zero
  (movei a0 1)
  (unconditional-branch test-branch-foo ()))

(defun make-consing-region-for-initial-list ()
  (let ((region
          (region-data:make-region 1.
                       (region-bits:encode-region-bits
                         region-bits:$$region-fixed
                         region-bits:$$region-new-space
                         region-bits:$$region-space-cons
                         region-bits:$$region-read-write
                         region-bits:$$scavenge-enabled
                               region-bits:$$region-internal-memory
                         0.)
                       7.)))
    (setq gr::*cons-cache-area* 5.)
    (setq gr::*cons-cache-region* region)
    (setq gr::*cons-cache-free* (region-data:unsafe-region-free-pointer region))
    (setq gr::*cons-cache-limit* (region-data:region-end region)))
  (trap::illop "Made consing region for first list."))

(defun make-test-consing-region ()
  (let ((fake-consing-region
          (region-data:make-region 256.                 ;1 megaQ
                       (region-bits::encode-region-bits
                         region-bits:$$region-fixed
                         region-bits:$$region-new-space
                         region-bits:$$region-space-cons
                         region-bits:$$region-read-write
                         region-bits:$$scavenge-enabled
                               region-bits:$$region-internal-memory
                         0.)
                       7.)))
    (setq gr::*cons-cache-area*   5.)           ;random number
    (setq gr::*cons-cache-region* fake-consing-region)
    (setq gr::*cons-cache-free* (region-data:unsafe-region-free-pointer fake-consing-region))
    (setq gr::*cons-cache-limit* (region-data:region-end         fake-consing-region))
    fake-consing-region))


(defun create-n (n)
  (do ((n n (1- n))
       (a () (li:push () a)))
      ((= n 0) a)
  ;  (trap::illop "Create N loop.")
    )
  )


(defun iterative-div2 (l)
  (do ((l l (cons::cddr l))
       (a () (li:push (cons::car l) a)))
      ((null l) a)))

(defun recursive-div2 (l)
  (cond ((null l) ())
        (t (cons::cons (cons::car l) (recursive-div2 (cons::cddr l))))))

(defun test-1 (l)
  (do ((i 300. (1- i)))
      ((= i 0))
    (iterative-div2 l)
    (iterative-div2 l)
    (iterative-div2 l)
    (iterative-div2 l)
    ))

(defun test-2 (l)
  (do ((i 300. (1- i)))
      ((= i 0))
    (recursive-div2 l)
    (recursive-div2 l)
    (recursive-div2 l)
    (recursive-div2 l)))

(defun test-div2-iterative ()
  (make-consing-region-for-initial-list)
  (let ((l (create-n 200.)))
    (trap::illop "I think I made a 200 element list.")
    (labels ((loop (n)
               (modify-leds (hw:ldb-not n (byte 3. 0.) 0))
               (let ((region (make-test-consing-region)))
                 (test-1 l)
                 ;; Primitive Garbage Collection
                 ;; Reset the free pointer.
                 (region-data:free-region region))
        ;       (trap::illop "Region freed.")
               (loop (1+ n))))
      (loop 0))))

(defun test-div2-recursive ()
  (labels ((loop (n l)
             (modify-leds (hw:ldb-not n (byte 3. 0.) 0.))
             (test-2 l)
             (maybe-reset-area n l))

           (maybe-reset-area (n l)
             (if (= n 4.)
                 (progn (area-data::reset-temporary-area gr:*cons-cache-area*)
                        (loop 0 (create-n 200.)))
                 (loop (1+ n) l))))
      (loop 0 (create-n 200.))))

(defun listn (n)
  (if (not (= 0 n))
      (cons:cons n (listn (1- n)))))

(defun mas (x y z)
  (if (not (shorterp y x))
      z
      (mas (mas (cons::%cdr x) y z)
           (mas (cons::%cdr y) z x)
           (mas (cons::%cdr z) x y))))

(defun shorterp (x y)
  (and y (or (null x)
             (shorterp (cons::%cdr x) (cons::%cdr y)))))

(defun test-mas ()
  (make-consing-region-for-initial-list)
  (let ((list18 (listn 18.))
        (list12 (listn 12.))
        (list6  (listn 6.))
        (region (make-test-consing-region)))
    (labels ((loop (n)
               (modify-leds (hw:ldb-not n (byte 3. 0.) 0.))
               (mas list18 list12 list6)
               ;; Primitive Garbage Collection
               ;; Reset the free pointer.
               (setf (region-data:region-free-pointer region)
                     (quantum->address region))
               (loop (1+ n))))
      (loop 0))))

(defun length (l)
  (labels ((scan (l count)
             (if (null l)
                 count
                 (scan (cons::cdr l) (1+ count)))))
    (scan l 0)))

(defun nconc (l1 l2)
  (labels ((bash-last-pair (l)
             (let ((c (cons::cdr l)))
               (if (null c)
                   (cons:rplacd l l2)
                   (bash-last-pair c)))))
    (if (null l1)
        l2
        (progn (bash-last-pair l1)
               l1))
    ))

(defafun floor (x y)
;;; a0 - fixnum dividend
;;; a1 - fixnum divisor
;;;
;;; a2 - remainder
;;; a3 - quotient
;;; a4 - temp
;;; a5 - bignum result pointer

  (move nop a1 bw-24)                           ;check for zero-divide
  (alu load-q-r a2 a0 a0 bw-24 br-zero)         ;q <- dividend
  (branch zdiv (alu sign a2 a0 a0 bw-32))       ;sign extend initial remainder
  (alu sdiv-first a2 a1 a2 bw-24)               ;step 1
  (alu sdiv-step  a2 a1 a2 bw-24)
  (alu sdiv-step  a2 a1 a2 bw-24)
  (alu sdiv-step  a2 a1 a2 bw-24)

  (alu sdiv-step  a2 a1 a2 bw-24)
  (alu sdiv-step  a2 a1 a2 bw-24)
  (alu sdiv-step  a2 a1 a2 bw-24)
  (alu sdiv-step  a2 a1 a2 bw-24)

  (alu sdiv-step  a2 a1 a2 bw-24)
  (alu sdiv-step  a2 a1 a2 bw-24)
  (alu sdiv-step  a2 a1 a2 bw-24)
  (alu sdiv-step  a2 a1 a2 bw-24)

  (alu sdiv-step  a2 a1 a2 bw-24)
  (alu sdiv-step  a2 a1 a2 bw-24)
  (alu sdiv-step  a2 a1 a2 bw-24)
  (alu sdiv-step  a2 a1 a2 bw-24)

  (alu sdiv-step  a2 a1 a2 bw-24)
  (alu sdiv-step  a2 a1 a2 bw-24)
  (alu sdiv-step  a2 a1 a2 bw-24)
  (alu sdiv-step  a2 a1 a2 bw-24)

  (alu sdiv-step  a2 a1 a2 bw-24)
  (alu sdiv-step  a2 a1 a2 bw-24)
  (alu sdiv-step  a2 a1 a2 bw-24)
  (alu sdiv-step  a2 a1 a2 bw-24)

  (alu sdiv-last1 a2 a1 a2 bw-24)               ;first fixup
  (alu pass-q a3 a1 a1 br-equal bw-24)          ;no fixup2 if zero, save quotient maybe
  (branch done (alu setr nop ignore a3 bw-24))

  (alu sdiv-last2 nop a1 a2 bw-24)              ;second fixup
  (alu pass-q a3 a1 a1 bw-24)                   ;save fixed quotient
  (alu quo-corr a3 a3 a3 bw-24)                 ;final fixup
done
  (alu pass-status a14 gr::*zero* gr::*zero* bw-24 boxed)
  (move a15 gr::*two*)
  (return a3)
zdiv
  (open-call (trap::illop-function 0) ignore ())
  (unconditional-branch done (alu setr a3 ignore a1)))

;(defun rplaca (list value)
;  (cons::%set-car list value list))

;(defun cons:rplacd (list value)
;  (cons::%set-cdr list value list))

(defun destructive (n m)
  (let ((l (do ((i 10. (1- i))
                (a () (li::push () a)))
               ((= i 0) a))))
    (do ((i n (1- i)))
        ((= i 0))
      (cond ((null (cons::car l))
             (do ((l l (cons::cdr l)))
                 ((null l))
               (or (cons::car l)
                   (cons:rplaca l (cons::cons () ())))
               (nconc (cons::car l)
                      (do ((j m (1- j))
                           (a () (li::push () a)))
                          ((= j 0) a)))))
            (t
             (do ((l1 l (cons::cdr l1))
                  (l2 (cons::cdr l) (cons::cdr l2)))
                 ((null l2))
               (cons:rplacd (do ((j (ash (length (cons::car l2)) -1)
                               (1- j))
                            (a (cons::car l2) (cons::cdr a)))
                           ((zerop j) a)
                         (cons:rplaca a i))
                       (let ((n (ash (length (cons::car l1)) -1)))
                         (cond ((= n 0) (cons:rplaca l1 ())
                                (cons::car l1))
                               (t
                                (do ((j n (1- j))
                                     (a (cons::car l1) (cons::cdr a)))
                                    ((= j 1)
                                     (prog1 (cons::cdr a)
                                            (cons:rplacd a ())))
                                  (cons:rplaca a i))))))))))))

(defun n-element-list (n)
  (create-n n))

(defun new-destructive (n m)
  (let ((l (n-element-list 10.)))
    (dotimes (pass n)
;            (trap::illop "Beginning pass")
      (destroy pass m l))))

(defun destroy (pass m l)
  (let ((first-element (cons::car l)))
    (if (null first-element)
        (replenish-lists m l)
        (bash-lists l pass))))

;(defmacro dolist ((element list) &body body)
;  `(DO ((,element ,list (cons::cdr ,element)))
;       ((null ,element))
;     ,@body))

(defun replenish-lists (m l)
;  (trap::illop "Replenishing.")
  (do ((cdrs l (cons::cdr cdrs)))
      ((null cdrs))
;    (trap::illop "Replenish-loop.")
    (when (null (cons::car cdrs))
      (cons:rplaca cdrs (cons::cons () ())))
    (nconc (cons::car cdrs)
           (n-element-list m)))
  (trap::illop "Finished replenishing."))

(defun bash-lists (l pass)
  (do ((forward (cons::cdr l) (cons::cdr forward))
       (behind  l             (cons::cdr behind)))
      ((null forward))
    (let* ((forward-sublist      (cons::car forward))
           (half-forward (ash (length forward-sublist) -1))
           (rest-forward
             ;; Get second holf, and bash the first.
             (do ((count half-forward (1- count))
                  (a     forward-sublist (cons::cdr a)))
                 ((zerop count) a)
               (cons:rplaca a pass)))
           (behind-sublist     (cons::car behind))
           (half-behind (ash (length behind-sublist) -1))
           (rest-behind
             ;; Get the second half, bash the first, differently
             (if (zerop half-behind)
                 (progn (cons:rplaca behind '()) nil)
                 (do ((count half-behind (1- count))
                      (a     behind-sublist (cons::cdr a)))
                     ((= count 1)
                      (prog1 (cons::cdr a)
                             (cons:rplacd a ())))
                   (cons:rplaca a pass)))))
      (cons:rplaca rest-forward rest-behind))))

(defun test-destructive ()
  (labels ((loop (n)
             (modify-leds (hw:ldb-not n (byte 3. 0.) 0.))
             ;         (trap::illop "About to destruct.")
             (let ((region (make-test-consing-region)))
;              (trap::illop "About to destruct.")
               (destructive 600. 50.)
;              (trap::illop "Did destructive.")
               (region-data:free-region region))
             (loop (1+ n))))
    (loop 0)))

(defun test-string ()
  (let ((test-string (array::make-string 3.)))
    (array::aset-1 #\F test-string 0)
    (array::aset-1 #\o test-string 1)
    (array::aset-1 #\o test-string 2))
  (trap::illop "Made string."))

(defun test-symbol ()
  (let ((test-string (array::make-string 3.)))
    (array::aset-1 #\F test-string 0)
    (array::aset-1 #\o test-string 1)
    (array::aset-1 #\o test-string 2)
    (let ((sym (symbol::%make-symbol test-string)))
      (symbol::%set sym sym)
      (trap::illop "Made a symbol."))))
