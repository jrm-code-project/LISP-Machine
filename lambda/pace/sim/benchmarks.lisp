;;; -*- Mode:LISP; Package:SIM; Base:10; Readtable:ZL -*-

(zwei:DEFCOM com-compile-region-for-new-processor "" ()
  (let ((*features* (cons :new *features*)))
    (zwei:COMPILE-DEFUN-INTERNAL #'compile-region-for-new-processor "Hacking" "hacked."))
  zwei:DIS-NONE)

(zwei:COMMAND-STORE 'com-compile-region-for-new-processor #/c-m-h-s-c zwei:*mode-COMTAB*)

(defun compile-region-for-new-processor (form)
  (when form
    (when (or (not (eq (car form) 'defun))
              (not (symbolp (cadr form))))
      (ferror nil "must be simple defun"))
    (let ((output (compile-defun form)))
      (putprop (cadr form) output 'hack-compiler-output)
      (zwei:kill-string (new-processor-compiler-pp output)))))

(defun new-processor-compiler-pp (form)
  (with-output-to-string (s)
    (format s "(define-asm ~s ~s" (cadr form) (caddr form))
    (dolist (inst (cdddr form))
      (cond ((consp inst)
             (format s "~&  ~a" inst))
            (t
             (format s "~&~a" inst))))
    (format s "~&  )")))



(defun tak (x y z)
  (cond ((< y x)
         (tak (tak (1- x) y z)
              (tak (1- y) z x)
              (tak (1- z) x y))
         )
        (t       z )))

(define-asm TAK (X Y Z)
  (ALU (GARBAGE) <- (ACTIVE 1) SUB (ACTIVE 0))
  (JUMP LESS-THAN TRUE-BRANCH-0390)
  (RETURN-XCT-NEXT)
  (ALU (FUNC RETURN) <- (ACTIVE 2) SETM (GARBAGE))
TRUE-BRANCH-0390
  (TAIL-RECURSIVE-OPEN)
  (OPEN (OPEN 0))
  (ALU (OPEN 0) <- (ACTIVE 0) L-R-1 (CONSTANT (QUOTE 0)))
  (ALU (OPEN 1) <- (ACTIVE 1) SETM (GARBAGE))
  (ALU (OPEN 2) <- (ACTIVE 2) SETM (GARBAGE))
  (CALL-XCT-NEXT TAK)
  (NOOP)
  (OPEN (OPEN 1))
  (ALU (OPEN 0) <- (ACTIVE 1) L-R-1 (CONSTANT (QUOTE 0)))
  (ALU (OPEN 1) <- (ACTIVE 2) SETM (GARBAGE))
  (ALU (OPEN 2) <- (ACTIVE 0) SETM (GARBAGE))
  (CALL-XCT-NEXT TAK)
  (NOOP)
  (OPEN (OPEN 2))
  (ALU (OPEN 0) <- (ACTIVE 2) L-R-1 (CONSTANT (QUOTE 0)))
  (ALU (OPEN 1) <- (ACTIVE 0) SETM (GARBAGE))
  (ALU (OPEN 2) <- (ACTIVE 1) SETM (GARBAGE))
  (CALL-XCT-NEXT TAK)
  (NOOP)
  (TAIL-RECURSIVE-CALL-XCT-NEXT TAK)
  (NOOP)
  )

(define-asm tak-driver ()
  (open (garbage))
  (open (open 0))
  (alu (open 0) <- (active 0) set-source-1 (garbage))
  (alu (open 1) <- (active 1) set-source-1 (garbage))
  (alu (open 2) <- (active 2) set-source-1 (garbage))
  (call-xct-next tak)
 (no-op)
  (call-xct-next %halt)
  (noop))
#|
(sim-test 'tak-driver '(8 7 6))
(sim-test 'tak-driver '(18. 12. 6))
|#

(defun create-n (n)
  (do ((n n (1- n))
       (a () (push () a)))
      ((= n 0) a)))
(define-asm CREATE-N (N)
  (STORE-IMMEDIATE BOXED (ACTIVE 1))
  (IMMEDIATE-DATA (VALUE-CELL NIL))
  (ALU (ACTIVE 2) <- (ACTIVE 0) SETM (GARBAGE))
  (STORE-IMMEDIATE BOXED (ACTIVE 3))
  (IMMEDIATE-DATA (VALUE-CELL NIL))
G0391
  (alu (garbage) <- (active 2) sub (constant 0))
  (JUMP EQUAL TRUE-BRANCH-0397)
  (JUMP ALWAYS MERGE-0398)
TRUE-BRANCH-0397
  (JUMP ALWAYS G0392)
MERGE-0398
  (OPEN (ACTIVE 1))
  (STORE-IMMEDIATE BOXED (OPEN 0))
  (IMMEDIATE-DATA (VALUE-CELL NIL))
  (ALU (OPEN 1) <- (ACTIVE 1) SETM (GARBAGE))
  (CALL-XCT-NEXT CONS)
  (NOOP)
  (ALU (ACTIVE 4) <- (ACTIVE 1) SETM (GARBAGE))
  (ALU (ACTIVE 5) <- (ACTIVE 2) L-R-1 (CONSTANT (QUOTE 0)))
  (ALU (ACTIVE 2) <- (ACTIVE 5) SETM (GARBAGE))
  (ALU (ACTIVE 1) <- (ACTIVE 4) SETM (GARBAGE))
  (JUMP ALWAYS G0391)
G0392
  (ALU (ACTIVE 3) <- (ACTIVE 1) SETM (GARBAGE))
  (RETURN-XCT-NEXT)
  (ALU (FUNC RETURN) <- (ACTIVE 3) SETM (GARBAGE))
  )

(defun iterative-div2 (l)
  (do ((l l (cddr l))
       (a () (push (car l) a)))
      ((null l) a)))
(define-asm ITERATIVE-DIV2 (L)
  ;;active 0 is L
  ;;active 1 is A
  (alu (active 1) <- (constant 'nil) setl (garbage))
G0650
  ;;(null l)
  (alu (garbage) <- (active 0) sub (constant nil))
  (JUMP EQUAL G0651)
  ;;get ready to call CONS, result to A
  (OPEN (ACTIVE 1))
  ;;CAR L
  (VMA-START-READ (ACTIVE 0))
  (NOOP)
  (ALU (OPEN 0) <- (FUNC MD) SETM (GARBAGE))
  (CALL-XCT-NEXT CONS)
  (ALU (OPEN 1) <- (ACTIVE 1) SETM (GARBAGE))

  ;;CDR L
  (alu (func vma-start-read) <- or (active 0) (constant 1))
  (noop)
  ;;CDR L
  (alu (func vma-start-read) <- or (func md) (constant 1))
  (noop)
  (alu (active 0) <- (func md) setl (garbage))
  (JUMP ALWAYS G0650)

G0651
  (RETURN-XCT-NEXT)
  (ALU (FUNC RETURN) <- (ACTIVE 1) SETM (GARBAGE))
  )

(defun test-iterative-div2 (n)
  (let ((l (create-n n)))
    (%halt (iterative-div2 l))
    nil))
(define-asm TEST-ITERATIVE-DIV2 (N)
  (OPEN (ACTIVE 1))
  (ALU (OPEN 0) <- (ACTIVE 0) SETM (GARBAGE))
  (CALL-XCT-NEXT CREATE-N)
  (NOOP)
  (OPEN (GARBAGE))
  (OPEN (OPEN 0))
  (ALU (OPEN 0) <- (ACTIVE 1) SETM (GARBAGE))
  (CALL-XCT-NEXT ITERATIVE-DIV2)
  (NOOP)
  (CALL-XCT-NEXT %HALT)
  (NOOP)
  (STORE-IMMEDIATE BOXED (ACTIVE 2))
  (IMMEDIATE-DATA (VALUE-CELL NIL))
  (RETURN-XCT-NEXT)
  (ALU (FUNC RETURN) <- (ACTIVE 2) SETM (GARBAGE))
  )

#|
(sim-test 'test-iterative-div2 '(30.))
|#

(defun run-iterative-div2 (n)
  (do ((l (create-n n))
       (i 300. (1- i)))
      ((= i 0))
    (iterative-div2 l)
    (iterative-div2 l)
    (iterative-div2 l)
    (iterative-div2 l))
  (%halt)
  nil)

(define-asm RUN-ITERATIVE-DIV2 (N)
  (STORE-IMMEDIATE BOXED (ACTIVE 1))            ;I
  (IMMEDIATE-DATA 300.)
  (OPEN (ACTIVE 2))                             ;L
  (ALU (OPEN 0) <- (ACTIVE 0) SETM (GARBAGE))   ;N
  (CALL-XCT-NEXT CREATE-N)
  (NOOP)
G2717
  (alu (garbage) <- (active 1) sub (constant 0))        ;I
  (jump equal G2718)

  (OPEN (GARBAGE))
  (CALL-XCT-NEXT ITERATIVE-DIV2)
  (ALU (OPEN 0) <- (ACTIVE 2) SETM (GARBAGE))   ;L

  (OPEN (GARBAGE))
  (CALL-XCT-NEXT ITERATIVE-DIV2)
  (ALU (OPEN 0) <- (ACTIVE 2) SETM (GARBAGE))

  (OPEN (GARBAGE))
  (CALL-XCT-NEXT ITERATIVE-DIV2)
  (ALU (OPEN 0) <- (ACTIVE 2) SETM (GARBAGE))

  (OPEN (GARBAGE))
  (CALL-XCT-NEXT ITERATIVE-DIV2)
  (ALU (OPEN 0) <- (ACTIVE 2) SETM (GARBAGE))

  (JUMP-xct-next ALWAYS G2717)
 (ALU (ACTIVE 1) <- (ACTIVE 1) L-R-1 (CONSTANT (QUOTE 0)))
G2718
  (ALU (ACTIVE 3) <- (CONSTANT (QUOTE NIL)) SETM (GARBAGE))
  (OPEN (garbage))
  (CALL-XCT-NEXT %HALT)
  (NOOP)
  )


#|
(sim-test 'run-iterative-div2 '(200.))
|#


(DEFUN DER1 (A)
  (LIST 'QUOTIENT (DERIV A) A))
(DEFINE-ASM DER1 (A)
  (TAIL-RECURSIVE-OPEN)
  (STORE-IMMEDIATE BOXED (OPEN 0))
  (IMMEDIATE-DATA 'QUOTIENT)
  (OPEN (OPEN 1))
  (CALL-XCT-NEXT DERIV)
  (ALU (OPEN 0) <- (ACTIVE 0) SETM (GARBAGE))
  (TAIL-RECURSIVE-CALL-XCT-NEXT %LIST-3)
  (ALU (OPEN 2) <- (ACTIVE 0) SETM (GARBAGE))
  )


(DEFUN DERIV (A)
  (COND ((ATOM A)
         (COND ((EQ A 'X) 1)
               (T 0)))
        ((EQ (CAR A) 'PLUS)
         (CONS 'PLUS (MAPCAR (FUNCTION DERIV) (CDR A))))
        ((EQ (CAR A) 'DIFFERENCE)
         (CONS 'DIFFERENCE (MAPCAR (FUNCTION DERIV) (CDR A))))
        ((EQ (CAR A) 'TIMES)
         (LIST 'TIMES A (CONS 'PLUS (MAPCAR (FUNCTION DER1) (CDR A)))))
        ((EQ (CAR A) 'QUOTIENT)
         (LIST 'DIFFERENCE
               (LIST 'QUOTIENT (DERIV (CADR A)) (CADDR A))
               (LIST 'QUOTIENT (CADR A) (LIST 'TIMES (CADDR A) (CADDR A) (DERIV (CADDR A))))))
        (T 'ERROR)))
(DEFINE-ASM DERIV (A)
  ;;(atom a)
  (alu (garbage) <- (active 0) setz (constant #.(dpb dtp-list %%q-data-type 0)))
  (jump data-type-equal true-branch-7278)

  (VMA-START-READ (ACTIVE 0))
  (NOOP)
  (ALU (ACTIVE 2) <- (FUNC MD) SETM (GARBAGE))

  (STORE-IMMEDIATE BOXED (ACTIVE 3))
  (IMMEDIATE-DATA 'PLUS)

  (alu (garbage) <- (active 2) sub (active 3))
  (jump equal true-branch-7276)

  (VMA-START-READ (ACTIVE 0))
  (NOOP)
  (ALU (ACTIVE 2) <- (FUNC MD) SETM (GARBAGE))
  (STORE-IMMEDIATE BOXED (ACTIVE 3))
  (IMMEDIATE-DATA 'DIFFERENCE)

  (alu (garbage) <- (active 2) sub (active 3))
  (jump equal true-branch-7267)

  (VMA-START-READ (ACTIVE 0))
  (NOOP)
  (ALU (ACTIVE 2) <- (FUNC MD) SETM (GARBAGE))
  (STORE-IMMEDIATE BOXED (ACTIVE 3))
  (IMMEDIATE-DATA 'TIMES)

  (alu (garbage) <- (active 2) sub (active 3))
  (jump equal true-branch-7258)

  (VMA-START-READ (ACTIVE 0))
  (NOOP)
  (ALU (ACTIVE 2) <- (FUNC MD) SETM (GARBAGE))
  (STORE-IMMEDIATE BOXED (ACTIVE 3))
  (IMMEDIATE-DATA 'QUOTIENT)

  (alu (garbage) <- (active 2) sub (active 3))
  (jump equal true-branch-7249)

  (STORE-IMMEDIATE BOXED (ACTIVE 1))
  (IMMEDIATE-DATA 'ERROR)
  (RETURN-XCT-NEXT)
  (ALU (FUNC RETURN) <- (ACTIVE 1) SETM (GARBAGE))

TRUE-BRANCH-7249
;;; (car a) is 'quotient
;(LIST 'DIFFERENCE
;      (LIST 'QUOTIENT (DERIV (CADR A)) (CADDR A))
;      (LIST 'QUOTIENT
;           (CADR A)
;           (LIST 'TIMES (CADDR A) (CADDR A) (DERIV (CADDR A)))))
  (TAIL-RECURSIVE-OPEN)
  (STORE-IMMEDIATE BOXED (OPEN 0))
  (IMMEDIATE-DATA 'DIFFERENCE)
  (OPEN (OPEN 1))
  (STORE-IMMEDIATE BOXED (OPEN 0))
  (IMMEDIATE-DATA 'QUOTIENT)
  (OPEN (OPEN 1))

  ;(cdr a)
  (alu (func vma-start-read) <- or (active 0) (constant 1))
  (noop)

  ;(car *)
  (alu (func vma-start-read) <- (func md) setl (garbage))
  (noop)

  (CALL-XCT-NEXT DERIV)
  (ALU (OPEN 0) <- (FUNC MD) SETM (GARBAGE))

  ;(cdr a)
  (alu (func vma-start-read) <- or (active 0) (constant 1))
  (noop)
  ;(cdr *)
  (alu (func vma-start-read) <- or (active 0) (constant 1))
  (noop)
  ;(car *)
  (alu (func vma-start-read) <- (func md) setl (garbage))
  (noop)
  (CALL-XCT-NEXT %LIST-3)
  (ALU (OPEN 2) <- (FUNC MD) SETM (GARBAGE))

  (OPEN (OPEN 2))
  (STORE-IMMEDIATE BOXED (OPEN 0))
  (IMMEDIATE-DATA 'QUOTIENT)
  ;(cdr a)
  (alu (func vma-start-read) <- or (active 0) (constant 1))
  (noop)

  ;(car *)
  (alu (func vma-start-read) <- (func md) setl (garbage))
  (noop)
  (ALU (OPEN 1) <- (FUNC MD) SETM (GARBAGE))

  (OPEN (OPEN 2))
  (STORE-IMMEDIATE BOXED (OPEN 0))
  (IMMEDIATE-DATA 'TIMES)
  ;(caddr a)
  (alu (func vma-start-read) <- or (active 0) (constant 1))
  (noop)
  (alu (func vma-start-read) <- or (func md) (constant 1))
  (noop)
  (alu (func vma-start-read) <- setl (func md) (garbage))
  (noop)
  (ALU (OPEN 1) <- (FUNC MD) SETM (GARBAGE))

  ;(caddr a)
  (alu (func vma-start-read) <- or (active 0) (constant 1))
  (noop)
  (alu (func vma-start-read) <- or (func md) (constant 1))
  (noop)
  (alu (func vma-start-read) <- setl (func md) (garbage))
  (noop)
  (ALU (OPEN 2) <- (FUNC MD) SETM (GARBAGE))

  (OPEN (OPEN 3))
  ;(caddr a)
  (alu (func vma-start-read) <- or (active 0) (constant 1))
  (noop)
  (alu (func vma-start-read) <- or (func md) (constant 1))
  (noop)
  (alu (func vma-start-read) <- setl (func md) (garbage))
  (noop)
  (CALL-XCT-NEXT DERIV)
  (ALU (OPEN 0) <- (FUNC MD) SETM (GARBAGE))

  (CALL-XCT-NEXT LIST)
  (NOOP)
  (CALL-XCT-NEXT LIST)
  (NOOP)
  (TAIL-RECURSIVE-CALL-XCT-NEXT LIST)
  (NOOP)

TRUE-BRANCH-7258
  ;open for call to LIST
  (TAIL-RECURSIVE-OPEN)
  (STORE-IMMEDIATE BOXED (OPEN 0))
  (IMMEDIATE-DATA 'TIMES)
  (ALU (OPEN 1) <- (ACTIVE 0) SETM (GARBAGE))

  ;open for call to CONS
  (OPEN (OPEN 2))
  (STORE-IMMEDIATE BOXED (OPEN 0))
  (IMMEDIATE-DATA 'PLUS)

  ;open for call to MAPCAR
  (OPEN (OPEN 1))

  (STORE-IMMEDIATE BOXED (OPEN 0))
  (IMMEDIATE-DATA (FUNCTION DER1))

  ;(cdr a)
  (alu (func vma-start-read) <- or (active 0) (constant 1))
  (noop)

  (CALL-XCT-NEXT MAPCAR)
  (ALU (OPEN 1) <- (FUNC MD) SETM (GARBAGE))

  (CALL-XCT-NEXT CONS)
  (NOOP)
  (TAIL-RECURSIVE-CALL-XCT-NEXT LIST)
  (NOOP)

TRUE-BRANCH-7267
  ;;open for CONS
  (TAIL-RECURSIVE-OPEN)
  (STORE-IMMEDIATE BOXED (OPEN 0))
  (IMMEDIATE-DATA 'DIFFERENCE)
  ;;open for MAPCAR
  (OPEN (OPEN 1))
  (store-immediate boxed (open 0))
  (immediate-data (function deriv))

  ;(cdr a)
  (alu (func vma-start-read) <- or (active 0) (constant 1))
  (noop)
  (CALL-XCT-NEXT MAPCAR)
  (ALU (OPEN 1) <- (FUNC MD) SETM (GARBAGE))
  (TAIL-RECURSIVE-CALL-XCT-NEXT CONS)
  (NOOP)

TRUE-BRANCH-7276
  ;;open for call to CONS
  (TAIL-RECURSIVE-OPEN)
  (STORE-IMMEDIATE BOXED (OPEN 0))
  (IMMEDIATE-DATA 'PLUS)
  ;;open for call to MAPCAR
  (OPEN (OPEN 1))
  (STORE-IMMEDIATE BOXED (OPEN 0))
  (IMMEDIATE-DATA (FUNCTION DERIV))
  ;(cdr a)
  (alu (func vma-start-read) <- or (active 0) (constant 1))
  (noop)
  (CALL-XCT-NEXT MAPCAR)
  (ALU (OPEN 1) <- (FUNC MD) SETM (GARBAGE))
  (TAIL-RECURSIVE-CALL-XCT-NEXT CONS)
  (NOOP)

TRUE-BRANCH-7278
  (STORE-IMMEDIATE BOXED (ACTIVE 2))
  (IMMEDIATE-DATA 'X)
  (alu (garbage) <- (active 0) sub (active 2))
  (jump equal true-branch-7167)

  (RETURN-XCT-NEXT)
  (ALU (FUNC RETURN) <- (CONSTANT '0) SETM (GARBAGE))

TRUE-BRANCH-7167
  (RETURN-XCT-NEXT)
  (ALU (FUNC RETURN) <- (CONSTANT '1) SETM (GARBAGE)))

(define-asm test-deriv (exp)
  (open (garbage))
  (open (open 0))
  (alu (open 0) <- (active 1) setl (garbage))
  (call-xct-next deriv)
  (no-op)
  (call-xct-next %halt)
  (no-op))

(DEFUN RUN ()
  (DECLARE (FIXNUM I))
  (DO ((I 0 (1+ I)))
      ((= I 1000))
    (DERIV '(PLUS (TIMES 3 X X) (TIMES A X X) (TIMES B X) 5))
    (DERIV '(PLUS (TIMES 3 X X) (TIMES A X X) (TIMES B X) 5))
    (DERIV '(PLUS (TIMES 3 X X) (TIMES A X X) (TIMES B X) 5))
    (DERIV '(PLUS (TIMES 3 X X) (TIMES A X X) (TIMES B X) 5))
    (DERIV '(PLUS (TIMES 3 X X) (TIMES A X X) (TIMES B X) 5))))
(DEFINE-ASM RUN
            NIL
            (DECLARE (PROGN (DECLARE (FIXNUM I))
                            (DO ((I 0 (1+ I)))
                                ((= I 1000))
                              (DERIV '(PLUS (TIMES 3 X X) (TIMES A X X) (TIMES B X) 5))
                              (DERIV '(PLUS (TIMES 3 X X) (TIMES A X X) (TIMES B X) 5))
                              (DERIV '(PLUS (TIMES 3 X X) (TIMES A X X) (TIMES B X) 5))
                              (DERIV '(PLUS (TIMES 3 X X) (TIMES A X X) (TIMES B X) 5))
                              (DERIV '(PLUS (TIMES 3 X X) (TIMES A X X) (TIMES B X) 5)))))
            (DECLARE (DECLARE (FIXNUM I)))
            (DECLARE (DO ((I 0 (1+ I)))
                         ((= I 1000))
                       (DERIV '(PLUS (TIMES 3 X X) (TIMES A X X) (TIMES B X) 5))
                       (DERIV '(PLUS (TIMES 3 X X) (TIMES A X X) (TIMES B X) 5))
                       (DERIV '(PLUS (TIMES 3 X X) (TIMES A X X) (TIMES B X) 5))
                       (DERIV '(PLUS (TIMES 3 X X) (TIMES A X X) (TIMES B X) 5))
                       (DERIV '(PLUS (TIMES 3 X X) (TIMES A X X) (TIMES B X) 5))))
            (DECLARE (LET (#:G7281
                           (I 0))
                       (TAGBODY #:G7279
                                (IF (= I 1000) (GO #:G7280))
                                (DERIV '(PLUS (TIMES 3 X X) (TIMES A X X) (TIMES B X) 5))
                                (DERIV '(PLUS (TIMES 3 X X) (TIMES A X X) (TIMES B X) 5))
                                (DERIV '(PLUS (TIMES 3 X X) (TIMES A X X) (TIMES B X) 5))
                                (DERIV '(PLUS (TIMES 3 X X) (TIMES A X X) (TIMES B X) 5))
                                (DERIV '(PLUS (TIMES 3 X X) (TIMES A X X) (TIMES B X) 5))
                                (PSETQ I (1+ I))
                                (GO #:G7279)
                                #:G7280
                                (SETQ #:G7281 (PROGN)))
                       #:G7281
                       ))
            (DECLARE 0)
            (ALU (ACTIVE 0) <- (CONSTANT '0) SETM (GARBAGE))
            (DECLARE NIL)
            (STORE-IMMEDIATE BOXED (ACTIVE 1))
            (IMMEDIATE-DATA (VALUE-CELL NIL))
            (DECLARE (TAGBODY #:G7279
                              (IF (= I 1000) (GO #:G7280))
                              (DERIV '(PLUS (TIMES 3 X X) (TIMES A X X) (TIMES B X) 5))
                              (DERIV '(PLUS (TIMES 3 X X) (TIMES A X X) (TIMES B X) 5))
                              (DERIV '(PLUS (TIMES 3 X X) (TIMES A X X) (TIMES B X) 5))
                              (DERIV '(PLUS (TIMES 3 X X) (TIMES A X X) (TIMES B X) 5))
                              (DERIV '(PLUS (TIMES 3 X X) (TIMES A X X) (TIMES B X) 5))
                              (PSETQ I (1+ I))
                              (GO #:G7279)
                              #:G7280
                              (SETQ #:G7281 (PROGN))))
            #:G7279
            (DECLARE (IF (= I 1000) (GO #:G7280)))
            (DECLARE (= I 1000))
            (OPEN (GARBAGE))
            (DECLARE I)
            (ALU (OPEN 0) <- (ACTIVE 0) SETM (GARBAGE))
            (DECLARE 1000)
            (STORE-IMMEDIATE BOXED (OPEN 1))
            (IMMEDIATE-DATA 1000)
            (CALL-XCT-NEXT =)
            (NOOP)
            (JUMP EQUAL #:TRUE-BRANCH-7284)
            (JUMP ALWAYS #:MERGE-7285)
            #:TRUE-BRANCH-7284
            (DECLARE (GO #:G7280))
            (JUMP ALWAYS #:G7280)
            #:MERGE-7285
            (DECLARE (DERIV '(PLUS (TIMES 3 X X) (TIMES A X X) (TIMES B X) 5)))
            (OPEN (GARBAGE))
            (DECLARE '(PLUS (TIMES 3 X X) (TIMES A X X) (TIMES B X) 5))
            (STORE-IMMEDIATE BOXED (OPEN 0))
            (IMMEDIATE-DATA (PLUS (TIMES 3 X X) (TIMES A X X) (TIMES B X) 5))
            (CALL-XCT-NEXT DERIV)
            (NOOP)
            (DECLARE (DERIV '(PLUS (TIMES 3 X X) (TIMES A X X) (TIMES B X) 5)))
            (OPEN (GARBAGE))
            (DECLARE '(PLUS (TIMES 3 X X) (TIMES A X X) (TIMES B X) 5))
            (STORE-IMMEDIATE BOXED (OPEN 0))
            (IMMEDIATE-DATA (PLUS (TIMES 3 X X) (TIMES A X X) (TIMES B X) 5))
            (CALL-XCT-NEXT DERIV)
            (NOOP)
            (DECLARE (DERIV '(PLUS (TIMES 3 X X) (TIMES A X X) (TIMES B X) 5)))
            (OPEN (GARBAGE))
            (DECLARE '(PLUS (TIMES 3 X X) (TIMES A X X) (TIMES B X) 5))
            (STORE-IMMEDIATE BOXED (OPEN 0))
            (IMMEDIATE-DATA (PLUS (TIMES 3 X X) (TIMES A X X) (TIMES B X) 5))
            (CALL-XCT-NEXT DERIV)
            (NOOP)
            (DECLARE (DERIV '(PLUS (TIMES 3 X X) (TIMES A X X) (TIMES B X) 5)))
            (OPEN (GARBAGE))
            (DECLARE '(PLUS (TIMES 3 X X) (TIMES A X X) (TIMES B X) 5))
            (STORE-IMMEDIATE BOXED (OPEN 0))
            (IMMEDIATE-DATA (PLUS (TIMES 3 X X) (TIMES A X X) (TIMES B X) 5))
            (CALL-XCT-NEXT DERIV)
            (NOOP)
            (DECLARE (DERIV '(PLUS (TIMES 3 X X) (TIMES A X X) (TIMES B X) 5)))
            (OPEN (GARBAGE))
            (DECLARE '(PLUS (TIMES 3 X X) (TIMES A X X) (TIMES B X) 5))
            (STORE-IMMEDIATE BOXED (OPEN 0))
            (IMMEDIATE-DATA (PLUS (TIMES 3 X X) (TIMES A X X) (TIMES B X) 5))
            (CALL-XCT-NEXT DERIV)
            (NOOP)
            (DECLARE (PSETQ I (1+ I)))
            (DECLARE (SETQ I (1+ I)))
            (DECLARE (PROGN (INTERNAL-SETQ I (1+ I))))
            (DECLARE (INTERNAL-SETQ I (1+ I)))
            (DECLARE (1+ I))
            (OPEN (ACTIVE 0))
            (DECLARE I)
            (ALU (OPEN 0) <- (ACTIVE 0) SETM (GARBAGE))
            (CALL-XCT-NEXT 1+)
            (NOOP)
            (JUMP ALWAYS #:G7279)
            #:G7280
            (DECLARE (SETQ #:G7281 (PROGN)))
            (DECLARE (PROGN (INTERNAL-SETQ #:G7281 (PROGN))))
            (DECLARE (INTERNAL-SETQ #:G7281 (PROGN)))
            (DECLARE (PROGN))
            (ALU (ACTIVE 1) <- (CONSTANT 'NIL) SETM (GARBAGE))
            (DECLARE #:G7281)
            (RETURN-XCT-NEXT)
            (ALU (FUNC RETURN) <- (ACTIVE 1) SETM (GARBAGE)))
