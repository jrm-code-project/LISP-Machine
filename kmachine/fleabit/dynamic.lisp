;;; -*- Mode:LISP; Readtable:CL; Base:10 -*-


(defun dynamic-rf ()
  (block foo
    (bar #'(lambda (x) (return-from foo x))
         3)))

Generating:
50465414  ((DYNAMIC-RF_5 NIL K_0) ($OPEN-FRAME 1 ^C_10 '#{CALL-NODE BAR_1 50465516}))   STRATEGY/HEAP
50466246   ((C_10)   (BAR_1 1 K_0 ^P_7 '3))   STRATEGY/OPEN
50465627    ((P_7 NIL K_2 X_3) (K_0 0 X_3))   STRATEGY/HEAP


(defun dynamic-rf ()
  (block foo
    (bar #'(lambda (x) (return-from foo x))
         3))
  (print 'done))

Generating:
50500366  ((DYNAMIC-RF_6 NIL K_0) (^BLOCK_7 1 ^B_14))   STRATEGY/HEAP
50500441   ((BLOCK_7 NIL FOO_4) ($OPEN-FRAME 1 ^C_11 '#{CALL-NODE BAR_1 50500470}))   STRATEGY/OPEN
50501220    ((C_11)   (BAR_1 1 FOO_4 ^P_8 '3))   STRATEGY/OPEN
50500601     ((P_8 NIL K_2 X_3) (FOO_4 0 X_3))   STRATEGY/HEAP
50501655   ((B_14 IGNORE_13) ($OPEN-FRAME 1 ^C_12 '#{CALL-NODE PRINT_5 50501342}))   STRATEGY/HEAP
50501524    ((C_12)   (PRINT_5 1 K_0 'DONE))   STRATEGY/OPEN


(defun unwind-go ()
  (tagbody
   foo
      (bar #'(lambda () (go foo)) 3)
      (print 'yow))
  (print 'done))

Generating:
50437331  ((UNWIND-GO_7 NIL K_0) (^TAGBODY_8 1 ^B_26))   STRATEGY/HEAP
50437404   ((TAGBODY_8 NIL K_1) ($Y 1 ^Y_11))   STRATEGY/OPEN
50437621    ((Y_11 NIL C_9 FOO_2) (C_9 0 ^C_10 ^FOO_14))   STRATEGY/HEAP
50437544     ((C_10 NIL NIL) (FOO_2 1 K_1))   STRATEGY/OPEN
50440022     ((FOO_14 NIL K_3) ($OPEN-FRAME 1 ^C_18 '#{CALL-NODE BAR_4 50440051}))   STRATEGY/HEAP
50440606      ((C_18)   (BAR_4 1 ^B_21 ^P_15 '3))   STRATEGY/OPEN
50441203       ((B_21 IGNORE_20) ($OPEN-FRAME 1 ^C_19 '#{CALL-NODE PRINT_6 50440670}))   STRATEGY/OPEN
50441052        ((C_19)   (PRINT_6 1 K_3 'YOW))   STRATEGY/OPEN
50440161       ((P_15 NIL K_5) (FOO_2 1 K_1))   STRATEGY/HEAP
50442216   ((B_26 IGNORE_25) ($OPEN-FRAME 1 ^C_24 '#{CALL-NODE PRINT_6 50441703}))   STRATEGY/LABEL
50442065    ((C_24)   (PRINT_6 1 K_0 'DONE))   STRATEGY/OPEN


dynamic unwinding


if a continuation var is used free
in a heap lambda, need to throw?


if continuation has variable-closed...
then a catch is needed

if you are binding something that is called
and is closed over... (what about procs?).. (with no exits or closed exits???)
then you need to establish a catch
(if you are binding several such things
the catch needs to dispatch off the returned val...)



do we need to flag go/rf/tagbody/block?

50440161       ((P_15 NIL K_5) (FOO_2 1 K_1))   STRATEGY/HEAP

($GO 1 FOO_2)     ?
($GO 2 FOO_2 K_1) ?

(defun generate-go (label tagbody-cont)
  (unwind-dynamic-state tagbody-cont)
  (generate-known-call label))


(tagbody
    (bar #'(lambda () (go tag1))
         #'(lambda () (go tag2)))
    (print 'bar-returns)
 tag1
    (print 'yow)
 tag2
    (print 'yikes))

(tagbody
    (case
      (catch <tag>
        (bar #'(lambda () (throw <tag> 'tag1))
             #'(lambda () (throw <tag> 'tag2)))
        (go <skip>))
      (tag1 (go tag1))
      (tag2 (go tag2)))
 skip
    (print 'bar-returns)
 tag1
    (print 'yow)
 tag2
    (print 'yikes))

<tag> is actually:

(let ((tag (stack-addr-of-tag)))
  (catch tag
    (bar #'(lambda () (throw tag 'tag1))
         ...

with tag is closed over!


go doesn't pass values
  the throw value can be the name of the tag

return-from passes values
  but can only return to one place


the difference between go and return-from
is that there are multiple continuation bindings in the go binder


generate catch
lambda-queue:
(lambda (tag)
  (case tag
    (FOO (go foo))
    (BAR (go bar))))



need to reestablish catch

(defun twotag ()
  (tagbody
      (xxx #'(lambda () (go tag1))
           #'(lambda () (go tag2)))
      (print 'bar-returns)
   tag1
      (print 'endtag2)
      (yyy #'(lambda () (go tag1))
           #'(lambda () (go tag2)))
      (print 'endtag1)
   tag2
      (print 'yikes)))







;;; The following code doesn't work

(defun stupidloop ()
  (tagbody
   tag
      (print 'start)
      (xxx #'(lambda () (go tag)))
      (print 'end)))


(defun xxx (f)
  (print 'in-xxx)
  (funcall f))




;;; needs to do something like this

(block .block.
  (do ((.op. '.start.
             (catch .tag.
               (tagbody
                   (case .op.
                     (.start.)
                     (foo   (go foo))
                     (bar   (go bar))
                     (t     (return-from .block.)))
                   <body>
                   (return-from .block.)))))
      (())))






v











(ndefmacro catch (tag &body body)
  `(%catch ',tag . ,body))

(defun tagtest ()
  (block .block.
    (labels ((.loop. (.op.)
               (.loop. (catch .tag.
                         (tagbody
                             (case .op.
                               (.start.)
                               (foo   (go foo))
                               (bar   (go bar))
                               (t     (return-from .block.)))
                             (print 'yow)
                          foo
                             (print 'foo)
                          bar
                             (print 'bar)
                             (return-from .block.))))))
      (.loop. '.start.))))

Generating:
56375205  ((TAGTEST_13 NIL K_0) ($Y 1 ^Y_17))   STRATEGY/HEAP
56375453   ((Y_17 NIL C_15 .LOOP._1) (C_15 0 ^C_16 ^.LOOP._20))   STRATEGY/LABEL
56375376    ((C_16 NIL NIL) ($OPEN-FRAME 1 ^C_81 '#{CALL-NODE .LOOP._1 56407354}))   STRATEGY/OPEN
56407534     ((C_81)   (.LOOP._1 1 K_0 '.START.))   STRATEGY/OPEN
56375655    ((.LOOP._20 NIL K_2 .OP._3) ($OPEN-FRAME 1 ^C_80 '#{CALL-NODE .LOOP._1 56375703}))   STRATEGY/LABEL
56407116     ((C_80)   ($OPEN-FRAME 1 ^C_77 '#{CALL-NODE %CATCH_4 56375766}))   STRATEGY/OPEN
56406636      ((C_77)   (^TAGBODY_21 1 ^C_76))   STRATEGY/OPEN
56376130       ((TAGBODY_21 NIL K_5) ($Y 1 ^Y_24))   STRATEGY/OPEN
56376346        ((Y_24 NIL C_22 FOO_6 BAR_7) (C_22 0 ^C_23 ^FOO_27 ^BAR_35))   STRATEGY/LABEL
56376270         ((C_23 NIL NIL) ($OPEN-FRAME 1 ^C_65 '#{CALL-NODE EQL_12 56405022}))   STRATEGY/OPEN
56405240          ((C_65)   (EQL_12 1 ^C_67 .OP._3 '.START.))   STRATEGY/OPEN
56405407           ((C_67 NIL V_66) ($CONDITIONAL 2 ^C_43 ^C_44 $TEST $TRUE? V_66))   STRATEGY/OPEN
56401013            ((C_43 NIL) ($OPEN-FRAME 1 ^C_68 '#{CALL-NODE PRINT_9 56405455}))   STRATEGY/LABEL
56405635             ((C_68)   (PRINT_9 1 ^B_74 'YOW))   STRATEGY/OPEN
56406353              ((B_74 IGNORE_73) ($%GO 2 K_5 FOO_6))   STRATEGY/OPEN
56401145            ((C_44 NIL) ($OPEN-FRAME 1 ^C_62 '#{CALL-NODE EQL_12 56404141}))   STRATEGY/LABEL
56404357             ((C_62)   (EQL_12 1 ^C_64 .OP._3 'FOO))   STRATEGY/OPEN
56404526              ((C_64 NIL V_63) ($CONDITIONAL 2 ^C_47 ^C_50 $TEST $TRUE? V_63))   STRATEGY/OPEN
56401337               ((C_47 NIL) ($%GO 2 K_5 FOO_6))   STRATEGY/LABEL
56401741               ((C_50 NIL) ($OPEN-FRAME 1 ^C_59 '#{CALL-NODE EQL_12 56403260}))   STRATEGY/LABEL
56403476                ((C_59)   (EQL_12 1 ^C_61 .OP._3 'BAR))   STRATEGY/OPEN
56403645                 ((C_61 NIL V_60) ($CONDITIONAL 2 ^C_53 ^C_56 $TEST $TRUE? V_60))   STRATEGY/OPEN
56402133                  ((C_53 NIL) ($%GO 2 K_5 BAR_7))   STRATEGY/LABEL
56402535                  ((C_56 NIL) (K_0 0 'NIL))   STRATEGY/LABEL
56376547         ((FOO_27 NIL K_8) ($OPEN-FRAME 1 ^C_28 '#{CALL-NODE PRINT_9 56376575}))   STRATEGY/LABEL
56376755          ((C_28)   (PRINT_9 1 ^B_32 'FOO))   STRATEGY/OPEN
56377376           ((B_32 IGNORE_31) ($%GO 2 K_5 BAR_7))   STRATEGY/OPEN
56377641         ((BAR_35 NIL K_10) ($OPEN-FRAME 1 ^C_36 '#{CALL-NODE PRINT_9 56377667}))   STRATEGY/LABEL
56400047          ((C_36)   (PRINT_9 1 ^B_40 'BAR))   STRATEGY/OPEN
56400430           ((B_40 IGNORE_39) (K_0 0 'NIL))   STRATEGY/OPEN
56406525       ((C_76 NIL V_75) (%CATCH_4 1 ^C_79 '.TAG. V_75))   STRATEGY/LABEL
56407005        ((C_79 NIL V_78) (.LOOP._1 1 K_2 V_78))   STRATEGY/OPEN

Post Processed:
TAGTEST_13
   (MOVEI A0 (QUOTE .START.) BOXED)
.LOOP._20
   (OPEN)
TAGBODY_21
   (MOVE O0 A0 CH-OPEN)
   (MOVEI O1 (QUOTE .START.) BOXED)
   (CALL (EQL 2) R2 NIL)
   (MOVE NOP R2)
   (TEST BR-ZERO)
   (BRANCH C_44 NIL)
C_43
   (MOVEI O0 (QUOTE YOW) BOXED CH-OPEN)
   (CALL (PRINT 1) IGNORE NIL)
FOO_27
   (MOVEI O0 (QUOTE FOO) BOXED CH-OPEN)
   (CALL (PRINT 1) IGNORE NIL)
BAR_35
   (MOVEI O0 (QUOTE BAR) BOXED CH-OPEN)
   (CALL (PRINT 1) IGNORE NIL)
   (MOVEI A0 (QUOTE NIL) BOXED)
   (CALL (FLUSH-OPEN-FRAME 0) IGNORE NIL)
   (NOP)
   (RETURN A0)
C_44
   (MOVE O0 A0 CH-OPEN)
   (MOVEI O1 (QUOTE FOO) BOXED)
   (CALL (EQL 2) R2 NIL)
   (MOVE NOP R2)
   (TEST BR-ZERO)
   (BRANCH C_50 NIL)
C_47
   (UNCONDITIONAL-BRANCH FOO_27 NIL)
C_50
   (MOVE O0 A0 CH-OPEN)
   (MOVEI O1 (QUOTE BAR) BOXED)
   (CALL (EQL 2) R2 NIL)
   (MOVE NOP R2)
   (TEST BR-ZERO)
   (BRANCH C_56 NIL)
C_53
   (UNCONDITIONAL-BRANCH BAR_35 NIL)
C_56
   (MOVEI A0 (QUOTE NIL) BOXED)
   (CALL (FLUSH-OPEN-FRAME 0) IGNORE NIL)
   (NOP)
   (RETURN A0)



   (movei r0 '#\0 boxed)
loop
   (open)           ;catch
   (move-pc o? catch-continuation)
   ...catch stuff...
   (movei r1 '#\0 boxed)
   (alu l-r r0 r1)
   (test br-equal)
   (branch start)
   (movei r1 'foo boxed)
   (alu l-r r0 r1)
   (test br-equal)
   (branch foo_27)
   (movei r1 'bar boxed)
   (alu l-r r0 r1)
   (test br-equal)
   (branch bar_35)
;; shouldn't fall in
catch-continuation
   (move r0 <catch-value>)
   (unconditional-branch loop)
start
   <generate tagbody>


(defun generate-tagbody-catch ()
  (let ((loop-tag       (gen-tag 'loop))
        (catch-cont-tag (gen-tag 'catch-cont))
        (start-tag      (gen-tag 'start)))
    (generate-move ''LI:.TAGBODY-START. R0)
   (emit-tag loop-tag)
    (generate-catch-1 '.TAG. catch-cont-tag) ;this is not good enough
    (generate-tagtest 'LI:.TAGBODY-START. start-tag)
    (dolist (? ??)
      (generate-tagtest . ..))
   (emit-tag catch-cont-tag)
    (generate-move <catch-value> R0)
    (emit-unconditional-branch loop-tag)
   (emit-tag start-tag)
   (generate...)))

(defun generate-tagtest (symbolic-tag real-tag)
  (generate-move `',symbolic-tag R1)
  (emit 'K:L-R R0 R1)
  (emit 'K:TEST 'K:BR-EQUAL)    ;bignums?
  (emit-branch real-tag))


(defun generate-catch-1 (tag cont)
  (push (cons 'CATCH tag) *dynamic-state*)
  (emit 'K:OPEN)
  (generate-move ''LI:.UNWIND-MARKER. O0)
  (generate-move `',tag O1)
  (generate-move 'GR:*SPECIAL-PDL* O2)
  (generate-move 'GR:*STACK-POINTER* 'K:O3)
  (emit 'K:MOVE-PC 'K:O4 cont))





(defun unwind-go ()
  (tagbody
   foo
      (bar #'(lambda () (go foo)) 3)
      (print 'yow))
  (print 'done))



Generating:
51554237  ((UNWIND-GO_7 NIL K_0) (^TAGBODY_8 1 ^B_26))   STRATEGY/HEAP
51554312   ((TAGBODY_8 NIL K_1) ($Y 1 ^Y_11))   STRATEGY/OPEN
51554527    ((Y_11 NIL C_9 FOO_2) (C_9 0 ^C_10 ^FOO_14))   STRATEGY/LABEL
51554452     ((C_10 NIL NIL) ($%GO 2 K_1 FOO_2))   STRATEGY/OPEN
51554730     ((FOO_14 NIL K_3) ($OPEN-FRAME 1 ^C_18 '#{CALL-NODE BAR_4 51554757}))   STRATEGY/LABEL
51555537      ((C_18)   (BAR_4 1 ^B_21 ^P_15 '3))   STRATEGY/OPEN
51556132       ((B_21 IGNORE_20) ($OPEN-FRAME 1 ^C_19 '#{CALL-NODE PRINT_6 51555621}))   STRATEGY/OPEN
51556001        ((C_19)   (PRINT_6 1 K_3 'YOW))   STRATEGY/OPEN
51555066       ((P_15 NIL K_5) ($%GO 2 K_1 FOO_2))   STRATEGY/HEAP
51557173   ((B_26 IGNORE_25) ($OPEN-FRAME 1 ^C_24 '#{CALL-NODE PRINT_6 51556662}))   STRATEGY/LABEL
51557042    ((C_24)   (PRINT_6 1 K_0 'DONE))   STRATEGY/OPEN

(1 ENTER GENERATE-LAMBDA: #{LAMBDA-NODE UNWIND-GO_7 51554237})
UNWIND-GO_7
Dynamic State UNWIND-GO_7: ((ARBITRARY-STUFF))
  (2 ENTER GENERATE-LAMBDA: #{LAMBDA-NODE TAGBODY_8 51554312})
TAGBODY_8
Dynamic State TAGBODY_8: ((ARBITRARY-STUFF))
  (MOVEI R0 (QUOTE .TAGBODY-START.) BOXED)
LOOP0554
  (OPEN)
  (MOVEI O0 (QUOTE .UNWIND-MARKER.) BOXED)
  (MOVEI O1 (QUOTE .TAG.) BOXED)
  (MOVE O2 *SPECIAL-PDL*)
  (MOVE O3 *STACK-POINTER*)
  (MOVE-PC O4 CATCH-CONT0555)
  (MOVEI R1 (QUOTE .TAGBODY-START.) BOXED)
  (L-R R0 R1)
  (TEST BR-EQUAL)
  (BRANCH C_10)
  (MOVEI R1 (QUOTE FOO) BOXED)
  (L-R R0 R1)
  (TEST BR-EQUAL)
  (BRANCH FOO_14)
CATCH-CONT0555
  (UNCONDITIONAL-BRANCH LOOP0554)
C_10
    (1 ENTER LAMBDA-QUEUE: #{LAMBDA-NODE FOO_14 51554730})
    (1 EXIT LAMBDA-QUEUE: ((#{LAMBDA-NODE FOO_14 51554730} (CATCH . .TAG.) (ARBITRARY-STUFF))))
  (2 EXIT GENERATE-LAMBDA: NIL)
(1 EXIT GENERATE-LAMBDA: NIL)
(1 ENTER GENERATE-LAMBDA: #{LAMBDA-NODE FOO_14 51554730})
FOO_14
Dynamic State FOO_14: ((CATCH . .TAG.) (ARBITRARY-STUFF))
  (OPEN)
  (1 ENTER GENERATE-CONTINUATION: NIL #{LAMBDA-NODE C_18 51555537} 1)
    (1 ENTER LAMBDA-QUEUE: #{LAMBDA-NODE P_15 51555066})
    (1 EXIT LAMBDA-QUEUE: ((#{LAMBDA-NODE P_15 51555066} (OPEN) (CATCH . .TAG.) (ARBITRARY-STUFF))))
  (OPEN)
  (MOVE O0 (FUNCTION (INTERNAL P_15)))
  (CALL (MAKE-CLOSURE-WITH-CURRENT-ENV 1) O0)
  (MOVEI O1 (QUOTE 3) BOXED)
  (CALL (BAR 2) IGNORE)
    (2 ENTER GENERATE-CONTINUATION: K:IGNORE #{LAMBDA-NODE B_21 51556132})
  (OPEN)
      (3 ENTER GENERATE-CONTINUATION: NIL #{LAMBDA-NODE C_19 51556001} 1)
  (MOVEI O0 (QUOTE YOW) BOXED)
  (CALL (PRINT 1) IGNORE)
        (4 ENTER GENERATE-CONTINUATION: K:IGNORE #{REFERENCE #{Variable K_3} 51556154})
          (1 ENTER LAMBDA-QUEUE: #{LAMBDA-NODE B_26 51557173})
          (1 EXIT LAMBDA-QUEUE: ((#{LAMBDA-NODE B_26 51557173} (CATCH . .TAG.) (ARBITRARY-STUFF)) (#{LAMBDA-NODE P_15 51555066} (OPEN) (CATCH . .TAG.) (ARBITRARY-STUFF))))
        (4 EXIT GENERATE-CONTINUATION: NIL)
      (3 EXIT GENERATE-CONTINUATION: NIL)
    (2 EXIT GENERATE-CONTINUATION: NIL)
  (1 EXIT GENERATE-CONTINUATION: NIL)
(1 EXIT GENERATE-LAMBDA: NIL)
(1 ENTER GENERATE-LAMBDA: #{LAMBDA-NODE B_26 51557173})
B_26
Dynamic State B_26: ((CATCH . .TAG.) (ARBITRARY-STUFF))
  (TAIL-OPEN)
  (1 ENTER GENERATE-CONTINUATION: NIL #{LAMBDA-NODE C_24 51557042} 1)
  (MOVEI O0 (QUOTE DONE) BOXED)
  (TAIL-CALL (PRINT 1))
  (1 EXIT GENERATE-CONTINUATION: NIL)
(1 EXIT GENERATE-LAMBDA: NIL)
(1 ENTER GENERATE-LAMBDA: #{LAMBDA-NODE P_15 51555066})
P_15
Dynamic State P_15: ((ARBITRARY-STUFF) (OPEN) (CATCH . .TAG.) (ARBITRARY-STUFF))
  (TAIL-OPEN)
  (MOVEI O0 (QUOTE K) BOXED)
  (MOVEI O1 (QUOTE FOO) BOXED)
  (TAIL-CALL (THROW 2))
(1 EXIT GENERATE-LAMBDA: NIL)

Post Processed:
UNWIND-GO_7
TAGBODY_8
   (MOVEI R0 (QUOTE .TAGBODY-START.) BOXED)
LOOP0554
   (MOVEI O0 (QUOTE .UNWIND-MARKER.) BOXED CH-OPEN)
   (MOVEI O1 (QUOTE .TAG.) BOXED)
   (MOVE O2 *SPECIAL-PDL*)
   (MOVE O3 *STACK-POINTER*)
   (MOVE-PC O4 CATCH-CONT0555)
   (MOVEI R1 (QUOTE .TAGBODY-START.) BOXED)
   (L-R R0 R1)
   (TEST BR-EQUAL)
   (BRANCH C_10 NIL)
   (MOVEI R1 (QUOTE FOO) BOXED)
   (L-R R0 R1)
   (TEST BR-EQUAL)
   (BRANCH FOO_14 NIL)
CATCH-CONT0555
   (UNCONDITIONAL-BRANCH LOOP0554 NIL)
C_10
FOO_14
   (OPEN)
   (OPEN-CALL (MAKE-CLOSURE-WITH-CURRENT-ENV 1) O0 (O0 (FUNCTION (INTERNAL P_15))))
   (MOVEI O1 (QUOTE 3) BOXED)
   (CALL (BAR 2) IGNORE NIL)
   (MOVEI O0 (QUOTE YOW) BOXED CH-OPEN)
   (CALL (PRINT 1) IGNORE NIL)
B_26
   (MOVEI O0 (QUOTE DONE) BOXED CH-TAIL-OPEN)
   (TAIL-CALL (PRINT 1) NIL)
P_15
   (MOVEI O0 (QUOTE K) BOXED CH-TAIL-OPEN)
   (MOVEI O1 (QUOTE FOO) BOXED)
   (TAIL-CALL (THROW 2) NIL)


;;; this doesn't work
;;; because tail call only
;;; unwinds specials
(defun unwind-go ()
  (tagbody
   foo
      (bar #'(lambda () (go foo)) 3)
      (print 'yow)))




(defun mvthrow-maybe (x y z)
  (block foo
    (cc1 #'(lambda () (return-from foo (values x y z)))
         #'(lambda () (return-from foo y)))
    (print 'done)))

(defun cc1 (c1 c2)
  (funcall c1))


 28 PUSH-ADDRESS LOCAL|0
 29 POP LOCAL|0
 30 PUSH-NUMBER 41
 31 (MISC) %CATCH-OPEN D-RETURN
 32 PUSH LOCAL|0
 33 CALL D-IGNORE FEF|7       ;#'CC1
 34 PUSH FEF|8                ;'#<GLOBAL:DTP-FEF-POINTER (:INTERNAL MVTHROW-MAYBE 0) 47530053>
 35 MAKE-LEXICAL-CLOSURE-TOP-LEVEL local slot 1
 36 PUSH FEF|9                ;'#<GLOBAL:DTP-FEF-POINTER (:INTERNAL MVTHROW-MAYBE 1) 47530127>
 37 MAKE-LEXICAL-CLOSURE-TOP-LEVEL local slot 2
 38 MOVE D-LAST PDL-POP
 39 CALL D-RETURN FEF|10      ;#'PRINT
 40 MOVE D-LAST FEF|11        ;'DONE


 14 PUSH-NUMBER 0
 15 (MISC) %LOAD-FROM-HIGHER-CONTEXT D-PDL
 16 PUSH (lexical ref 0,3)    ;X
 17 (MISC) %LOAD-FROM-HIGHER-CONTEXT D-PDL
 18 PUSH (lexical ref 0,2)    ;Y
 19 (MISC) %LOAD-FROM-HIGHER-CONTEXT D-PDL
 20 PUSH (lexical ref 0,1)    ;Z
 21 (MISC) %LOAD-FROM-HIGHER-CONTEXT D-PDL
 22 PUSH-NUMBER 3
 23 (MISC) THROW-N D-IGNORE
 24 (MISC) *THROW D-IGNORE

 14 PUSH-NUMBER 0
 15 (MISC) %LOAD-FROM-HIGHER-CONTEXT D-PDL
 16 PUSH (lexical ref 0,2)    ;Y
 17 (MISC) %LOAD-FROM-HIGHER-CONTEXT D-PDL
 18 (MISC) *THROW D-IGNORE





(defun catchval (x)
  (setq x (catch 'foo (bar x)))
  (foo x))




catch could always return in R0
throw could be %throw and %throw-mv

;; was open or topen, check it
(when (eq (hw:O0) 'si:unwind-marker)
  (setq *stack-pointer* (hw:O3))
  (unbind-to (hw:O3))
  (if (eq (hw:O1) *tag*)
      (progn
        (hw:call 'flush-catch 0)
        (hw:dispatch (hw:O4)))
    (when (eq (hw:O1) 'si:unwind-protect-tag)


;; was open or topen, check it
(when (eq (hw:O0) 'si:unwind-marker)
  (setq *stack-pointer* (hw:O3))
  (unbind-to (hw:O3))
  (if (eq (hw:O1) *tag*)
      (progn
        (hw:call 'flush-catch-mv 0)
        (hw:dispatch (hw:O4)))
    (when (eq (hw:O1) 'si:unwind-protect-tag)




(defun flush-catch ()
  gr:*throw-value*)

(defafun flush-catch-mv ()
  (return-mv gr:*throw-value*))
