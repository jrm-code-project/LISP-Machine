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

  (VMA-START-READ (ACTIVE 0))
....

again
  (no-op)
  (CALL CONS (OPEN 0) <- (FUNC MD) SETM (GARBAGE))

  ;;CDR L
  (alu (func vma-start-read) <- or (active 0) (constant 1))
 (open (active 1) (new-open 1) <- (active 1))
  ;;CDR L
  (alu (func vma-start-read) <- or (func md) (constant 1))
  (noop)
  (alu (active 0) <- (func md) setl (garbage))
  (JUMP-xct-next not-equal again)                       ;NIL is 0
 (vma-start-read (active 0))



again
  (vma-start-read (active 0))
  (no-op)
  (CALL CONS (OPEN 0) <- (FUNC MD) SETM (GARBAGE))

  ;;CDR L
  (alu (func vma-start-read) <- or (active 0) (constant 1))
 (open (active 1) (new-open 1) <- (active 1))
  ;;CDR L
  (alu (func vma-start-read) <- or (func md) (constant 1))
  (JUMP-xct-next again)                 ;NIL is 0
  (alu (active 0) <- (func md) setl (garbage))

;;;;CONS

  (no-op)

  (alu (func md) <- (active 0) )
  (alu (func vma-start-write) <- (free-pointer))

  (alu (garbage) <- add-32 (free-pointer) (free-pointer-limit) trap-if-over)

  (no-op)
  (no-op)

  (alu (func md) <- (active 1) setl (garbage))
  (alu (func vma-start-write) <- (free-pointer) or (constant 1))

  (alu (free-pointer) <- (free-poitner) add (constant 2))

  (alu (func return) <- (free-pointer) sub (constant 2))
