;;;-*- Mode:LISP; Package:USER; Base:8; Readtable:CL -*-
;;;
;;; (c) Copyright 1984 - Lisp Machine, Inc.
;;;


;;      This file contains descriptions of the proms used in the lambda processor.
;;      it also contains the following functions to aid in their programming

;;we generate a prom array with MAKE-PROM-ARRAY
;;check for conflicts in logical requirements
;;the function is passed a list of lists.  each list is a specification, except the first
;;list which allows the naming of inputs. other arguments are n and m.
;; the input specifications are of the form (* * * * * * *) where * is matched with
;; an address bit (low bit left) and may be a 1,0,X (dont-care),a keyword,or a variable.
;; A variable
;; allows the state of an input bit to be passed to an output bit...optionally negated.
;; The only keyword is "default" which allows the programmer to specify the behavior in
;; all cases which match a particular predicate and which have not already been matched.
;; the output specification is of the same form

(defun make-prom-array (length width &optional (name 'pa))
  (set name (fillarray (make-array `(,length)) (list (make-list width ':initial-value 'x)))))

(defun print-prom-array (array)
  (format t "~%~% adr     data~%")
  (do ((length (first (array-dimensions array)))
       (address 0 (1+ address)))
      (( address length))
    (let ((contents  (aref array address)))
      (format t "~%~T~7o  ~T~A " address
              (cond ((numberp contents) contents)
                    (T (reverse contents))))))
  (terpri))

;;at each address, we go down a list of pairs of lists : an input predicate list
;;and an output specification list.  The input predicate is compared to the address
;; and if true, the associated output spec is combined with the existing output
;;(which starts out as (x x x x...)).  The rules of combination are : x going to anything
;; - no op; 1 going to x rplaca the 1 in, 1 going to 0 signal conflict, 1 going to 1
;;mark as multiply specified, potential source of trouble.  Perhaps the right
;;way to do that is to store a list of the conflicting predicates and output specs ...
;;do that later

(defun inputs-match-predicate (address number-of-address-bits predicate-list)
  (DO* ((VARIABLE-A-LIST '())
        (pred-list predicate-list (cdr pred-list))
        (bit-predicate (first pred-list) (first pred-list))
        (bit 0 (1+ bit))
        (BIT-VALUE (LOGAND 1 ADDRESS)
                   (LOGAND 1 (ASH ADDRESS (- BIT)))))
       ((or (null pred-list)
            ( bit number-of-address-bits))     ;return the alist of symbols and values
        (COND ((NULL VARIABLE-A-LIST) T)        ;unless its null, in which case return t
              (T VARIABLE-A-LIST)))
    (COND ((and  (numberp bit-predicate)
                                                ;if the predicate is a number,
                                                ;and it doesn't match that bit of
                                                ;the address then return nil
                 (not (equal bit-predicate BIT-VALUE)))
           (RETURN NIL))
          ((AND (SYMBOLP BIT-PREDICATE)(NOT (EQUAL BIT-PREDICATE 'X)))
           (SETQ VARIABLE-A-LIST (CONS (CONS BIT-PREDICATE BIT-VALUE) VARIABLE-A-LIST)))
          ((AND (LISTP BIT-PREDICATE)
                (EQUAL 'NOT (CAR BIT-PREDICATE)))
           (SETQ VARIABLE-A-LIST (CONS
                                   (CONS (CADR BIT-PREDICATE) (logxor 1 BIT-VALUE))
                                       VARIABLE-A-LIST))))
    ))

;;this function is given the spec list after each input spec and output spec has been
;;reversed  -- this is so the user can type in specs in high bit to low bit format,
;;and allow high bits to default to don't-care

(defun set-prom-output-specs-at-address (address reversed-spec-list
                                         number-of-address-bits
                                         prom-array prom-width
                                         )
  (aset
    (DO* ((variable-a-list nil nil)             ;reset the "pass-through" variable a-list
                                                ;for each predicate/spec pair
          (no-matchp t)
          (reversed-spec-list reversed-spec-list (cdr reversed-spec-list))
          (IN-spec-rlist (first (first reversed-spec-list))
                         (first (first reversed-spec-list)))
          (out-spec-rlist (second (first reversed-spec-list))
                          (second (first reversed-spec-list)))
          (output-list (make-list
                         prom-width ':initial-value 'x)))
         ((null reversed-spec-list)
          output-list)
      (cond  ((and (equal (first in-spec-rlist) "default")
                   no-matchp)
;             (format t "~%default at address ~O with in-spec-rlist ~A" address in-spec-rlist)
              (cond ((null (cdr in-spec-rlist))
                     (setq output-list (add-specs out-spec-rlist output-list
                                                  in-spec-rlist variable-a-list)))
                                                ;if we default and have no input predicates,
                                                ;then set the output list

                    ((setq variable-a-list (inputs-match-predicate address
                                             number-of-address-bits
                                             (cdr in-spec-rlist)))

                     (setq no-matchp nil)       ;indicates a succesful match at this address

                     (setq output-list (add-specs out-spec-rlist output-list
                                                  in-spec-rlist variable-a-list)))))

             ((stringp (first in-spec-rlist)))  ;ignore other strings than "default"

             ((setq variable-a-list (inputs-match-predicate address
                                      number-of-address-bits
                                      in-spec-rlist))

              (setq no-matchp nil)              ;indicates a succesful match at this address

              (setq output-list (add-specs out-spec-rlist output-list
                                           in-spec-rlist variable-a-list)))))
    prom-array address))

(defun add-specs (out-spec-list output-list in-spec-rlist VARIABLE-A-LIST)
  (cond ((null out-spec-list) output-list)
        ((null output-list) NIL)
        (T (let* ((ospec (first out-spec-list))
                  (out-spec (cond ((and (not (equal ospec 'x))
                                        (symbolp ospec))
                                   (if (null (cdr (assoc ospec variable-a-list)))
                                       (ferror t "no varible named ~A"ospec)
                                     (cdr (assoc ospec variable-a-list))))
                                                ;out-spec is replaced with the new
                                                ;value from the a-list if it is a
                                                ;symbol,though if the assoc returns
                                                ;nil we error out
                                  (t ospec)))
                  (output (first output-list)))
             (cond ((or (equal output out-spec)
                        (equal out-spec 'x))    ;if they already agree, or if the
                                                ;out-spec is a "don't care", then
                                                ;output is unchanged

                    (cons output (add-specs (cdr out-spec-list)
                                            (cdr output-list)
                                            in-spec-rlist
                                            variable-a-list)))

                   ((equal output 'x)(cons out-spec
                                           (add-specs (cdr out-spec-list)
                                                      (cdr output-list)
                                                      in-spec-rlist
                                                      variable-a-list)))
                   (T (format t "~%~A~%"in-spec-rlist)
                      (ferror T "conflict")))))))

;;how do we want to pass information of conflicts back up to the top level?

(defun make-prom-array-to-specs (spec-list
                                 prom-name
                                 prom-length
                                 prom-width
                                 &OPTIONAL (REVERSED-IO-P NIL))
  (make-prom-array prom-length prom-width prom-name)
  (DO* ((address 0 (1+ address))
        (reversed-spec-list (COND (REVERSED-IO-P SPEC-LIST)
                                  (T (spec-list-reversal spec-list))))
        (number-of-address-bits (haulong (sub1 prom-length))))
       (( address prom-length))
    (set-prom-output-specs-at-address address
                                      reversed-spec-list
                                      number-of-address-bits
                                      (eval prom-name)
                                      prom-width))
;  (print-prom-array (eval prom-name))
)

(defun spec-pair-reversal (spec-pair)
  (list (reverse (first spec-pair))
        (reverse (second spec-pair))))

(defun spec-list-reversal (spec-list)
  (cond ((null spec-list) nil)
        (T (cons (spec-pair-reversal (first spec-list))(spec-list-reversal (cdr spec-list))))
        ))

;;now we must convert the bit representation into octal numbers, compatible with
;;the functions to write prom files (found in fs:lmio1;promp)
;;we're going to need to increase the size of the array that programmer-read-prom-file
;;uses in the beginning
;;
;;programmer-write-prom-file wants to write octal numbers in the file -trim to 8 bits first
;;

(defun make-output-list-into-number (output-list &optional (default-state 0))
  (DO* ((output-list output-list (cdr output-list))
        (output-number 0)
        (bit-loc 0001 (+ bit-loc 0100))
        (output-bit (first output-list)(first output-list)))
       ((or (null output-list)
            ( bit-loc 1001))
        output-number)
    (setq output-number
          (cond ((numberp output-bit)
                 (dpb output-bit bit-loc output-number))
                (T (dpb default-state bit-loc output-number))))))

(defun convert-prom-array-to-numbers (prom-array &OPTIONAL (default-state 0))
  (DO ((prom-length (array-dimension prom-array 0))
       (address 0 (1+ address)))
      (( address prom-length))
    (aset (make-output-list-into-number (aref prom-array address) default-state)
          prom-array
          address)))

(defvar tsp '((x 0 1)(1 0)))

(defvar tsl '(((x 0 1)(1))))
;;            ((1 1 1)(0 1 1))))

(defvar atsl '(
               ((1 0 0) (1 0 0 0))
               ((0 1 0) (0 1 0 0))
               ((0 1 1) (0 0 1 0))))


(defun make-prom (prom-list &OPTIONAL (REVERSED-IO-P NIL))
  (let ((prom-name (first prom-list))
        (length (second prom-list))
        (width (third prom-list))
        (spec-list (cdddr prom-list)))
  (make-prom-array-to-specs spec-list prom-name length width REVERSED-IO-P)
  (convert-prom-array-to-numbers (symeval prom-name))
  (print-prom-array (symeval prom-name))))


;; RG board:
;;
;;1.slave address proms
;;      these three 2Kx4 proms are found on the RG board, nubus-slave.
;;      with associated logic, they decode the nu-bus address for the slave port

;;Due to a bug in the draw data base, the address bus and data bus of the prom
;;got reflected .. that is, address bit zero got wired to address input 11 on the
;;chip.  so we need to turn off spec-list reversal for the 2k x 4 proms.
;; this has the side effect that all "dont care" specs must be explicitly marked
;;as an X (basically a good idea) rather than letting high bits of the spec default
;; to "dont care". IMPORTANT:turning off spec-list-reversal is done when you make the
;;prom, not contained in the prom spec

(defvar hi-adr-spec
        '(hi-adr 2048. 4.
          ((1 1 1 1 1 1 1 1 1 1 1)      (1 0 0 0))      ;poss con reg or con prom
          ((0 0 0 0 0 0 0 0 0 0 0)      (0 0 1 0))      ;poss spy or interrupt
          ( ("default")                 (0 0 0 1))))    ;unused

(defvar low-adr-a-spec
        '(low-adr-a 2048. 4.
          ((1 1 x x x x x x x x x)      (1 0 0 0))      ;poss con prom
          ((0 0 0 0 0 0 1 0 0 0 1)      (0 0 1 0))      ;spy read parity
          ((0 0 0 0 0 0 1 0 0 0 0)      (0 0 0 1))))    ;processor mode reg

(defvar low-adr-b-spec
        '(low-adr-b 2048. 4.
          ((0 0 1 x x x x x x x x)      (1 0 0 0))      ;poss interrrupt
          ((0 0 0 0 0 0 0 x x x x)      (0 1 0 0))      ;poss spy
          ((1 0 1 1 1 1 1 1 1 1 1)      (0 0 1 0))      ;poss con reg
          ((0 0 0 0 0 0 1 0 0 0 x)      (0 0 0 0))      ;mode reg or parity read
          ( ("default")                 (0 0 0 1))))    ;unused unless con prom


;;2.configuration prom
;;      512x8 contains the nubus configuration information for the processor
;;      -- not required for simple operation, but is a nice easy thing to read back
;;      and see if you're winning

;;3.MFO zeros prom
;;      512x8 decodes source address to drive 4-bit blocks of zeroes onto mfo bus,
;;      -- insures that all bits of mfo are driven during source cycle
;;
;;      this has a bug for the first ww prototype.. the low two blocks
;;      of zero drive are enabled by outputs 6,7, others are shifted down two bits

;;      "wait" bit ignored in all cases (bit 5)

(defvar mfo-zeroes-spec
        '(mfo-zeroes 512. 8.
          ((x 0 x x x x x x x)          (1 1 1 1 1 1 1 1))      ;not source cycle

          ((x 1 1 x 0 0 0 0 0)          (1 1 0 0 0 0 0 0))      ;interrupt pointer, 8 bits
          ((x 1 1 x 0 0 0 0 1)          (1 1 0 0 0 0 0 0))      ;mac.ir.displacement 8 b
          ((x 1 1 x 0 0 0 1 0)          (1 1 1 1 1 1 1 1))      ;stat.counter 32 bits
          ((x 1 1 x 0 0 0 1 1)          (1 1 0 0 0 0 1 1))      ;macro.ir 16 bits
          ((x 1 1 x 0 0 1 0 0)          (1 1 0 0 0 0 1 1))      ;m.i.d. ram 16 bits
          ((x 1 1 x 0 0 1 0 1)          (1 1 1 1 1 1 1 1))      ;spy.reg 32 bits
          ((x 1 1 x 0 1 0 0 0)          (1 1 0 0 0 0 0 1))      ;disp. const 12 bits
          ((x 1 1 x 0 1 0 0 1)          (1 1 1 1 0 1 1 1))      ;usp,us 28 bits
          ((x 1 1 x 0 1 0 1 0)          (1 1 1 1 0 1 1 1))      ;usp,us, pop usp 28 bits
                                                                ;usp:31-24,us: 19-0

          ((x 1 1 x 1 0 0 0 0)          (1 1 0 0 0 1 1 1))      ;cache address 18 bits
          ((x 1 1 x 1 0 0 0 1)          (1 1 1 1 1 1 1 1))      ;md 32 bits
          ((x 1 1 x 1 0 0 1 0)          (1 1 1 1 1 1 1 1))      ;vma 32 bits
          ((x 1 1 x 1 0 0 1 1)          (1 1 0 0 0 0 0 0))      ;level-1-map 8 bits
          ((x 1 1 x 1 0 1 0 0)          (1 1 0 0 0 0 1 1))      ;level-2-map-control 16 b
          ((x 1 1 x 1 0 1 0 1)          (1 1 0 0 1 1 1 1))      ;level-2-map-page 24 bits
          ((x 1 1 x 1 0 1 1 0)          (1 1 0 1 1 1 1 1))      ;lc 28 bits

          ((x 1 1 x 1 1 0 0 0)          (1 1 0 0 0 0 0 1))      ;pi 12 bits
          ((x 1 1 x 1 1 0 0 1)          (1 1 1 1 1 1 1 1))      ;q 32 bits
          ((x 1 1 x 1 1 0 1 0)          (1 1 0 0 0 0 0 1))      ;pp 12 bits
          ((x 1 1 0 1 1 1 1 0)          (1 1 1 1 1 1 1 1))      ;c-pp 32 bits
          ((x 1 1 1 1 1 1 1 0)          (1 1 1 1 1 1 1 1))      ;c-pp-pop 32 bits
          ((x 1 1 x 1 1 1 1 1)          (1 1 1 1 1 1 1 1))      ;c-pi 32 bits

          ((x 1 x x x x x x x
              "default")                (0 0 0 0 0 0 0 0))      ;null source cycle,
                                                                ;or source m-mem
                                                                ;drive all zeroes

          (("default")                  (1 1 1 1 1 1 1 1))      ;just in case
        ))

;;4.Interrupt prom
;;      512x8 used as part of interrupt state machine



;;******************************************************************************************


;;  CM board:
;;
;;1.destination prom
;;      four 512x8s decode 9 bits into destination control signals
;;
;;inputs  /8 noop or no dest/ 7 gnd/ 6 m.adr=0 L/ dest.a /(4-0) dest.funct 4-0/
;;
;;outputs /(7-5) next.dest.seq / 4 interrupt.control/ 3 memory.cycle / 2 start.write
;;              / 1 write.A / 0 write.M /

(defvar dest-prom-0-spec
        '(dest-prom-0 512. 8.
          ((1 x x x x x x x x)          (0 0 0 0 0 0 0 0))      ;no.op.or.no.dest
          ((0 x x 1 x x x x x)          (0 0 1 0 0 0 1 0))    ;dest.a  write.a and
                                                                ;uinst.next.dest.seq.1


;;functional destinations, each with and without a simultaneous m write
;;pdl writes do not need the write.m bit; the write.a bit should be on for
;;any m write

          ((0 x 1 0 0 0 0 0 0)          (0 1 0 0 0 0 1 1))      ;0 = nothing
          ((0 x 0 0 0 0 0 0 0)          (0 0 0 0 0 0 0 0))      ;0 = nothing
          ((0 x 1 0 0 0 0 0 1)          (0 1 0 0 0 0 1 1))      ;1 = uinst.dest.lc
          ((0 x 0 0 0 0 0 0 1)          (0 0 0 0 0 0 0 0))      ;1 = uinst.dest.lc
          ((0 x 1 0 0 0 0 1 0)          (0 1 0 1 0 0 1 1))    ;2 = interrupt control
          ((0 x 0 0 0 0 0 1 0)          (0 0 0 1 0 0 0 0))    ;2 = interrupt control
          ((0 x 1 0 0 0 0 1 1)          (0 1 0 0 0 0 1 1))      ;3 = clear interrupt
          ((0 x 0 0 0 0 0 1 1)          (0 0 0 0 0 0 0 0))      ;3 = clear interrupt
          ((0 x 1 0 0 0 1 0 0)          (0 1 0 0 0 0 1 1))      ;4 = statistics counter
          ((0 x 0 0 0 0 1 0 0)          (0 0 0 0 0 0 0 0))      ;4 = statistics counter
          ((0 x 1 0 0 0 1 0 1)          (0 1 0 0 0 0 1 1))      ;5 = m.i.d.ram
          ((0 x 0 0 0 0 1 0 1)          (0 0 0 0 0 0 0 0))      ;5 = m.i.d.ram
          ((0 x 1 0 0 1 0 0 0)          (1 0 0 0 0 0 1 1))      ;10 = c.pdl.p & m.write
          ((0 x 0 0 0 1 0 0 0)          (0 1 1 0 0 0 0 0))      ;10 = c.pdl.p & no m.write
          ((0 x 1 0 0 1 0 0 1)          (1 0 0 0 0 0 1 1))      ;11 = c.pdl.p.count, m.wr
          ((0 x 0 0 0 1 0 0 1)          (0 1 1 0 0 0 0 0))      ;11 = c.pdl.p.count, no m.wr
          ((0 x 1 0 0 1 0 1 0)          (1 1 0 0 0 0 1 1))      ;12 = c.pdl.index, m.wr
          ((0 x 0 0 0 1 0 1 0)          (1 0 1 0 0 0 0 0))      ;12 = c.pdl.index, no m.wr
          ((0 x 1 0 0 1 0 1 1)          (0 1 0 0 0 0 1 1))      ;13 = pi
          ((0 x 0 0 0 1 0 1 1)          (0 0 0 0 0 0 0 0))      ;13 = pi
          ((0 x 1 0 0 1 1 0 0)          (0 1 0 0 0 0 1 1))      ;14 = pp
          ((0 x 0 0 0 1 1 0 0)          (0 0 0 0 0 0 0 0))      ;14 = pp
          ((0 x 1 0 0 1 1 0 1)          (0 1 0 0 0 0 1 1))      ;15 = us.data, push
          ((0 x 0 0 0 1 1 0 1)          (0 0 0 0 0 0 0 0))      ;15 = us.data, push
          ((0 x 1 0 0 1 1 1 0)          (0 1 0 0 0 0 1 1))      ;16 =uinst.dest.low.imod
          ((0 x 0 0 0 1 1 1 0)          (0 0 0 0 0 0 0 0))      ;16 =uinst.dest.low.imod
          ((0 x 1 0 0 1 1 1 1)          (0 1 0 0 0 0 1 1))      ;17 = uinst.dest.high.imod
          ((0 x 0 0 0 1 1 1 1)          (0 0 0 0 0 0 0 0))      ;17 = uinst.dest.high.imod
          ((0 x 1 0 1 0 0 0 0)          (0 1 0 0 0 0 1 1))      ;20 = vma
          ((0 x 0 0 1 0 0 0 0)          (0 0 0 0 0 0 0 0))      ;20 = vma
          ((0 x 1 0 1 0 0 0 1)          (0 1 0 0 1 0 1 1))    ;21 = vma start read
          ((0 x 0 0 1 0 0 0 1)          (0 0 0 0 1 0 0 0))    ;21 = vma start read
          ((0 x 1 0 1 0 0 1 0)          (0 1 0 0 1 1 1 1))    ;22 = vma start write
          ((0 x 0 0 1 0 0 1 0)          (0 0 0 0 1 1 0 0))    ;22 = vma start write
          ((0 x 1 0 1 0 0 1 1)          (0 1 0 0 0 0 1 1))      ;23 = l1.map,adr by md
          ((0 x 0 0 1 0 0 1 1)          (0 0 0 0 0 0 0 0))      ;23 = l1.map,adr by md
          ((0 x 1 0 1 0 1 0 0)          (0 1 0 0 0 0 1 1))      ;24 = l2 map -control bits
          ((0 x 0 0 1 0 1 0 0)          (0 0 0 0 0 0 0 0))      ;24 = l2 map -control bits
          ((0 x 1 0 1 0 1 0 1)          (0 1 0 0 0 0 1 1))      ;25 = l2 map -phys page
          ((0 x 0 0 1 0 1 0 1)          (0 0 0 0 0 0 0 0))      ;25 = l2 map -phys page
          ((0 x 1 0 1 1 0 0 0)          (0 1 0 0 0 0 1 1))      ;30 = md
          ((0 x 0 0 1 1 0 0 0)          (0 0 0 0 0 0 0 0))      ;30 = md
          ((0 x 1 0 1 1 0 0 1)          (0 1 0 0 1 0 1 1))    ;31 = md start read
          ((0 x 0 0 1 1 0 0 1)          (0 0 0 0 1 0 0 0))    ;31 = md start read
          ((0 x 1 0 1 1 0 1 0)          (0 1 0 0 1 1 1 1))    ;32 = md start write
          ((0 x 0 0 1 1 0 1 0)          (0 0 0 0 1 1 0 0))    ;32 = md start write

          ((0 x 1 0 1 1 0 1 1)          (1 1 0 0 0 0 1 1))      ;33 = c.pdl.index wr.m, inc
          ((0 x 0 0 1 1 0 1 1)          (1 0 1 0 0 0 0 0))      ;33 = c.pdl.index  no wr.m,inc

          ((0 x 1 0 1 1 1 0 0)          (0 1 0 0 0 0 1 1))      ;34 = uinst.dest.us
          ((0 x 0 0 1 1 1 0 0)          (0 0 0 0 0 0 0 0))      ;34 = uinst.dest.us
          ((0 x 1 0 1 1 1 0 1)          (0 1 0 0 0 0 1 1))      ;35 = uinst.dest.usp
          ((0 x 0 0 1 1 1 0 1)          (0 0 0 0 0 0 0 0))      ;35 = uinst.dest.usp

          ((0 x 1 0 1 1 1 1 0)          (1 1 0 0 0 0 1 1))      ;36 = c.pdl.index wr.m, decr.
          ((0 x 0 0 1 1 1 1 0)          (1 0 1 0 0 0 0 0))      ;36 = c.pdl.index no wr.m,decr
          ((0 x 1 0 x x x x x
              "default")                (0 1 0 0 0 0 1 1))    ;write m mem and nothing else
          (("default")                  (0 0 0 0 0 0 0 0))
          ))

;;
;;inputs  /8 noop or no dest/ 7 gnd/ 6 m.adr=0 L/ dest.a /(4-0) dest.funct 4-0/
;;
;;outputs /7 d.lc L/ 6 d.low.imod/ 5 d.high.imod/ 4 d.us/ (3-2) slow.dest.seq/
;;              /1 inc.pi/ 0 inc.pp L/

(defvar dest-prom-1-spec
        '(dest-prom-1 512. 8.
          ((1 x x x x x x x x)          (1 0 0 0 0 0 0 1))      ;no.op.or.no.dest

;;functional destinations

          ((0 x x 0 0 0 0 0 0)          (1 0 0 0 0 0 0 1))      ;0 = nothing
          ((0 x x 0 0 0 0 0 1)          (0 0 0 0 0 0 0 1))    ;1 = uinst.dest.lc
          ((0 x x 0 0 0 0 1 0)          (1 0 0 0 0 0 0 1))      ;2 = interrupt control
          ((0 x x 0 0 0 0 1 1)          (1 0 0 0 0 0 0 1))      ;3 = clear interrupt
          ((0 x x 0 0 0 1 0 0)          (1 0 0 0 0 0 0 1))      ;4 = statistics counter
          ((0 x x 0 0 0 1 0 1)          (1 0 0 0 0 0 0 1))      ;5 = m.i.d.ram
          ((0 x x 0 0 1 0 0 0)          (1 0 0 0 0 0 0 1))      ;10 = c.pdl.p
          ((0 x x 0 0 1 0 0 1)          (1 0 0 0 0 0 0 0))    ;11 = c.pdl.p.count
          ((0 x x 0 0 1 0 1 0)          (1 0 0 0 0 0 0 1))      ;12 = c.pdl.index, m.wr
          ((0 x x 0 0 1 0 1 1)          (1 0 0 0 0 0 0 1))      ;13 = pi
          ((0 x x 0 0 1 1 0 0)          (1 0 0 0 0 0 0 1))      ;14 = pp
          ((0 x x 0 0 1 1 0 1)          (1 0 0 1 0 0 0 1))    ;15 = us.data, push
          ((0 x x 0 0 1 1 1 0)          (1 1 0 0 0 0 0 1))    ;16 = uinst.dest.low.imod
          ((0 x x 0 0 1 1 1 1)          (1 0 1 0 0 0 0 1))    ;17 = uinst.dest.high.imod

;;slow dest seq = 1
          ((0 x x 0 1 0 0 0 0)          (1 0 0 0 0 1 0 1))    ;20 = vma
          ((0 x x 0 1 0 0 0 1)          (1 0 0 0 0 1 0 1))    ;21 = vma start read
          ((0 x x 0 1 0 0 1 0)          (1 0 0 0 0 1 0 1))    ;22 = vma start write

          ((0 x x 0 1 0 0 1 1)          (1 0 0 0 0 0 0 1))      ;23 = l1.map,adr by md
          ((0 x x 0 1 0 1 0 0)          (1 0 0 0 0 0 0 1))      ;24 = l2 map -control bits
          ((0 x x 0 1 0 1 0 1)          (1 0 0 0 0 0 0 1))      ;25 = l2 map -phys page
;;slow dest seq = 1
          ((0 x x 0 1 1 0 0 0)          (1 0 0 0 0 1 0 1))    ;30 = md
          ((0 x x 0 1 1 0 0 1)          (1 0 0 0 0 1 0 1))    ;31 = md start read
          ((0 x x 0 1 1 0 1 0)          (1 0 0 0 0 1 0 1))    ;32 = md start write

          ((0 x x 0 1 1 0 1 1)          (1 0 0 0 0 0 1 1))    ;33 = c.pdl.index ,increment
          ((0 x x 0 1 1 1 0 0)          (1 0 0 1 0 0 0 1))    ;34 = uinst.dest.us
          ((0 x x 0 1 1 1 0 1)          (1 0 0 0 0 0 0 1))      ;35 = uinst.dest.usp
          ((0 x x 0 1 1 1 1 0)          (1 0 0 0 0 0 0 1))      ;36 = c.pdl.index ,decrement
          (("default")                  (1 0 0 0 0 0 0 1))
          ))

;;
;;inputs  /8 noop or no dest/ 7 gnd/ 6 m.adr=0 L/ dest.a /(4-0) dest.funct 4-0/
;;
;;outputs /7 d.stat.counter/ 6 count.pi L/ 5 nc/4 nc/ 3 d.vma/ 2 d.md/ 1 d.pi L/ 0 d.pp L/

(defvar dest-prom-2-spec
        '(dest-prom-2 512. 8.
          ((1 x x x x x x x x)          (0 1 0 0 0 0 1 1))      ;no.op.or.no.dest

;;functional destinations

          ((0 x x 0 0 0 0 0 0)          (0 1 0 0 0 0 1 1))      ;0 = nothing
          ((0 x x 0 0 0 0 0 1)          (0 1 0 0 0 0 1 1))      ;1 = uinst.dest.lc
          ((0 x x 0 0 0 0 1 0)          (0 1 0 0 0 0 1 1))      ;2 = interrupt control
          ((0 x x 0 0 0 0 1 1)          (0 1 0 0 0 0 1 1))      ;3 = clear interrupt
          ((0 x x 0 0 0 1 0 0)          (1 1 0 0 0 0 1 1))    ;4 = statistics counter
          ((0 x x 0 0 0 1 0 1)          (0 1 0 0 0 0 1 1))      ;5 = m.i.d.ram
          ((0 x x 0 0 1 0 0 0)          (0 1 0 0 0 0 1 1))      ;10 = c.pdl.p
          ((0 x x 0 0 1 0 0 1)          (0 1 0 0 0 0 1 1))      ;11 = c.pdl.p.count
          ((0 x x 0 0 1 0 1 0)          (0 1 0 0 0 0 1 1))      ;12 = c.pdl.index
          ((0 x x 0 0 1 0 1 1)          (0 1 0 0 0 0 0 1))    ;13 = pi
          ((0 x x 0 0 1 1 0 0)          (0 1 0 0 0 0 1 0))    ;14 = pp
          ((0 x x 0 0 1 1 0 1)          (0 1 0 0 0 0 1 1))      ;15 = us.data, push
          ((0 x x 0 0 1 1 1 0)          (0 1 0 0 0 0 1 1))      ;16 = uinst.dest.low.imod
          ((0 x x 0 0 1 1 1 1)          (0 1 0 0 0 0 1 1))      ;17 = uinst.dest.high.imod
          ((0 x x 0 1 0 0 0 0)          (0 1 0 0 1 0 1 1))    ;20 = vma
          ((0 x x 0 1 0 0 0 1)          (0 1 0 0 1 0 1 1))    ;21 = vma start read
          ((0 x x 0 1 0 0 1 0)          (0 1 0 0 1 0 1 1))    ;22 = vma start write
          ((0 x x 0 1 0 0 1 1)          (0 1 0 0 0 0 1 1))      ;23 = l1.map,adr by md
          ((0 x x 0 1 0 1 0 0)          (0 1 0 0 0 0 1 1))      ;24 = l2 map -control bits
          ((0 x x 0 1 0 1 0 1)          (0 1 0 0 0 0 1 1))      ;25 = l2 map -phys page
          ((0 x x 0 1 1 0 0 0)          (0 1 0 0 0 1 1 1))    ;30 = md
          ((0 x x 0 1 1 0 0 1)          (0 1 0 0 0 1 1 1))    ;31 = md start read
          ((0 x x 0 1 1 0 1 0)          (0 1 0 0 0 1 1 1))    ;32 = md start write
          ((0 x x 0 1 1 0 1 1)          (0 0 0 0 0 0 1 1))    ;33 = c.pdl.index, increment
          ((0 x x 0 1 1 1 0 0)          (0 1 0 0 0 0 1 1))      ;34 = uinst.dest us
          ((0 x x 0 1 1 1 0 1)          (0 1 0 0 0 0 1 1))      ;35 = null
          ((0 x x 0 1 1 1 1 0)          (0 0 0 0 0 0 1 1))    ;36 = c.pdl.index ,decrement
          (("default")                  (0 1 0 0 0 0 1 1))
          ))

;;
;;inputs  /8 noop or no dest/ 7 t.slow.dest.write/ 6 m.adr=0 L/ dest.a /(4-0) dest.funct 4-0/
;;
;;outputs /7 d.clear.interrupt/ 6 d.cram.adr.map / 5 d.high.cram L / 4 d.low.cram L
;;        / 3 d.write.mid/ 2 d.wr.phys.pg/ 1 d.wr.L2/ 0 d.wr.L1/
;;

(defvar dest-prom-3-spec
        '(dest-prom-3 512. 8.
          ((1 x x x x x x x x)          (0 0 1 1 0 0 0 0))      ;no.op.or.no.dest
          ((x 0 x x x x x x x)          (0 0 1 1 0 0 0 0))      ;t.new.uinst

;;functional destinations

          ((0 1 x 0 0 0 0 0 0)          (0 0 1 1 0 0 0 0))      ;0 = nothing
          ((0 1 x 0 0 0 0 0 1)          (0 0 1 1 0 0 0 0))      ;1 = uinst.dest.lc
          ((0 1 x 0 0 0 0 1 0)          (0 0 1 1 0 0 0 0))      ;2 = interrupt control
          ((0 1 x 0 0 0 0 1 1)          (1 0 1 1 0 0 0 0))    ;3 = clear interrupt
          ((0 1 x 0 0 0 1 0 0)          (0 0 1 1 0 0 0 0))      ;4 = statistics counter
          ((0 1 x 0 0 0 1 0 1)          (0 0 1 1 1 0 0 0))    ;5 = m.i.d.ram
          ((0 1 x 0 0 0 1 1 0)          (0 0 0 1 0 0 0 0))    ;6 = high cram
          ((0 1 x 0 0 0 1 1 1)          (0 0 1 0 0 0 0 0))    ;7 = low cram
          ((0 1 x 0 0 1 0 0 0)          (0 0 1 1 0 0 0 0))      ;10 = c.pdl.p
          ((0 1 x 0 0 1 0 0 1)          (0 0 1 1 0 0 0 0))      ;11 = c.pdl.p.count
          ((0 1 x 0 0 1 0 1 0)          (0 0 1 1 0 0 0 0))      ;12 = c.pdl.index
          ((0 1 x 0 0 1 0 1 1)          (0 0 1 1 0 0 0 0))      ;13 = pi
          ((0 1 x 0 0 1 1 0 0)          (0 0 1 1 0 0 0 0))      ;14 = pp
          ((0 1 x 0 0 1 1 0 1)          (0 0 1 1 0 0 0 0))      ;15 = us.data, push
          ((0 1 x 0 0 1 1 1 0)          (0 0 1 1 0 0 0 0))      ;16 = uinst.dest.low.imod
          ((0 1 x 0 0 1 1 1 1)          (0 0 1 1 0 0 0 0))      ;17 = uinst.dest.high.imod
          ((0 1 x 0 1 0 0 0 0)          (0 0 1 1 0 0 0 0))      ;20 = vma
          ((0 1 x 0 1 0 0 0 1)          (0 0 1 1 0 0 0 0))      ;21 = vma start read
          ((0 1 x 0 1 0 0 1 0)          (0 0 1 1 0 0 0 0))      ;22 = vma start write
          ((0 1 x 0 1 0 0 1 1)          (0 0 1 1 0 0 0 1))    ;23 = l1.map,adr by md
          ((0 1 x 0 1 0 1 0 0)          (0 0 1 1 0 0 1 0))    ;24 = l2 map :control bits
          ((0 1 x 0 1 0 1 0 1)          (0 0 1 1 0 1 0 0))    ;25 = l2 map :phys page
          ((0 1 x 0 1 0 1 1 0)          (0 1 1 1 0 0 0 0))    ;26 = cram adr map
          ((0 1 x 0 1 1 0 0 0)          (0 0 1 1 0 0 0 0))      ;30 = md
          ((0 1 x 0 1 1 0 0 1)          (0 0 1 1 0 0 0 0))      ;31 = md start read
          ((0 1 x 0 1 1 0 1 0)          (0 0 1 1 0 0 0 0))      ;32 = md start write
          ((0 1 x 0 1 1 0 1 1)          (0 0 1 1 0 0 0 0))      ;33 = c.pdl.index ,increment
          ((0 1 x 0 1 1 1 0 0)          (0 0 1 1 0 0 0 0))      ;34 = uinst.dest.us
          ((0 1 x 0 1 1 1 0 1)          (0 0 1 1 0 0 0 0))      ;35 = uinst.dest.usp
          ((0 1 x 0 1 1 1 1 0)          (0 0 1 1 0 0 0 0))      ;36 = c.pdl.index ,decrement
          (("default")                  (0 0 1 1 0 0 0 0))
          ))

;;******************************************************************************************


;;  MI board
;;
;;1.nu-control prom
;;      2 512x8s : decodes low bits of address to find transaction type
;;
(defvar nu-control-0-spec                       ;goes in f11 on version 1
        '(nu-control-0 512. 8.
           ((x 1 x x x x x x x)         (0 1 0 0 0 1 0 0))      ;disable if nu.ack.at.start
           ((x 0 0 x x x x x x)         (0 1 0 0 1 0 0 0))      ;byte operation
           ((x 0 1 x x x x 0 0)         (0 1 0 1 0 0 0 0))      ;halfword 1
           ((x 0 1 x x x x 0 1)         (1 0 0 0 0 0 0 0))      ;block operation
           ((x 0 1 x x x x 1 0)         (0 1 0 1 0 0 0 0))      ;halfword 0
           ((x 0 1 x x x x 1 1)         (0 1 1 0 0 0 0 0))))    ;word operation

(defvar nu-control-1-spec                       ;goes in f10 on version 1
        '(nu-control-1 512. 8.
           ((x 1 x a b c d x x)         (1 1 1 1 a b c d))      ;disable is nu.ack.at.start
           ((x 0 0 a b c d x x)         (1 1 1 1 a b c d))      ;byte operation
           ((x 0 1 a b c d 0 0)         (1 1 1 1 a b c d))      ;halfword 1
           ((x 0 1 a b c 1 0 1)         (1 1 1 0 a b c 0))      ;block operation 2 long
           ((x 0 1 a b 1 0 0 1)         (1 1 0 0 a b 0 0))      ;block operation 4 long
           ((x 0 1 a 1 0 0 0 1)         (1 0 0 0 a 0 0 0))      ;block operation 8 long
           ((x 0 1 1 0 0 0 0 1)         (0 0 0 0 0 0 0 0))      ;block operation 16 long
           ((x 0 1 a b c d 1 0)         (1 1 1 1 a b c d))      ;halfword 0
           ((x 0 1 a b c d 1 1)         (1 1 1 1 a b c d))))    ;word operation


;;2 nu-encoding prom
;;      512 x 8 : encodes low bits of address during write from lambda

(defvar nu-encode-spec                          ;goes in e5 on version 1
        '(nu-encode 512. 8.
  ;inputs: packetize pkt.size.1 pkt.size.0 vma.3 vma.2 vma.1 vma.0 phys.1 phys.0
  ;  encoding here is pkt.size 0 -> word op, 1 -> byte ops, 2 -> block 2, 3 -> block 16.
  ;
  ;REMEMBER, the outputs are inverted before reaching the nubus

          ((x 0 0 a b c d 0 0)          (0 0 a b c d 0 0))      ;word op, no block
          ((x 0 0 a b c d 0 1)          (1 0 a b c d 0 0))      ;word op,  encoding error
          ((x 0 0 a b c d 1 0)          (1 0 a b c d 0 0))      ;word op,  encoding error
          ((x 0 0 a b c d 1 1)          (1 0 a b c d 0 0))      ;word op,  encoding error
          ((x 0 1 a b c d e f)          (0 0 a b c d e f))      ;byte op
          ((1 1 0 a b c d 0 0)          (0 1 a b c 1 1 0))      ;block op 2 long
          ((1 1 0 a b c d 0 1)          (0 1 a b c 1 1 0))      ;block op 2 long
          ((1 1 0 a b c d 1 0)          (0 1 a b c 1 1 0))      ;block op 2 long
          ((1 1 0 a b c d 1 1)          (0 1 a b c 1 1 0))      ;block op 2 long
          ((0 1 0 a b c d 0 0)          (0 0 a b c d 0 0))      ;use word instead of block
          ((0 1 0 a b c d 0 1)          (1 0 a b c d 0 0))      ;word op,  encoding error
          ((0 1 0 a b c d 1 0)          (1 0 a b c d 0 0))      ;word op,  encoding error
          ((0 1 0 a b c d 1 1)          (1 0 a b c d 0 0))      ;word op,  encoding error
          ((1 1 1 a b c d 0 0)          (0 1 1 0 0 0 1 0))      ;block op 16 long
          ((1 1 1 a b c d 0 1)          (1 1 1 0 0 0 1 0))      ;block op, encoding error
          ((1 1 1 a b c d 1 0)          (1 1 1 0 0 0 1 0))      ;block op, encoding error
          ((1 1 1 a b c d 1 1)          (1 1 1 0 0 0 1 0))      ;block op, encoding error
          ((0 1 1 a b c d 0 0)          (0 0 a b c d 0 0))      ;use word instead of block
          ((0 1 1 a b c d 0 1)          (1 0 a b c d 0 0))      ;word op,  encoding error
          ((0 1 1 a b c d 1 0)          (1 0 a b c d 0 0))      ;word op,  encoding error
          ((0 1 1 a b c d 1 1)          (1 0 a b c d 0 0))))    ;word op,  encoding error

;;******************************************************************************************


;;  DP board
;;
;;1.dispatch masker prom
;;      512x8 : creates DMASK from dispatch length

(defvar dmask-spec
        '(dmask 512. 8.
          ((0 0 0 0 0 0 0 0 0)          (0 0 0 0 0 0 0 0))      ;0
          ((0 0 0 0 0 0 0 0 1)          (0 0 0 0 0 0 0 1))      ;1
          ((0 0 0 0 0 0 0 1 0)          (0 0 0 0 0 0 1 1))      ;2
          ((0 0 0 0 0 0 0 1 1)          (0 0 0 0 0 1 1 1))      ;3
          ((0 0 0 0 0 0 1 0 0)          (0 0 0 0 1 1 1 1))      ;4
          ((0 0 0 0 0 0 1 0 1)          (0 0 0 1 1 1 1 1))      ;5
          ((0 0 0 0 0 0 1 1 0)          (0 0 1 1 1 1 1 1))      ;6
          ((0 0 0 0 0 0 1 1 1)          (0 1 1 1 1 1 1 1))      ;7
          ((0 0 0 0 0 1 0 0 0)          (1 1 1 1 1 1 1 1))      ;10
          (("default")                  (0 0 0 0 0 0 0 0))))    ;otherwise,byte width is zero

;;2.masker proms
;;      8 2Kx4s : used as 1Kx32, outputs masking bits based on input of
;;      byte length and offset
;;
;;      special function for making masker proms rather than tryiing to
;;      adapt the general function
;;
;;      !! the address and data inputs are still reversed in the wiring!!
;;      we fill a masker prom by taking each address, extracting the byte fields,
;;      deposit ones in a field of zeros (specify the mask), and then extract
;;      the bits corresponding to that particular prom.  We then store that
;;      value, bit reversed, at the bit reversed address (again,
;;      the proms are wired
;;      wrong on version 1 of the lambda)

(defun bit-reverse (word word-length)           ;this is just to cope with reversed
  (let ((new-word 0))                           ;data busses. yuck!
    (dotimes (bit word-length)
      (setq new-word
            (dpb (ldb (byte 1 bit) word)        ;take bit from old word
                 (byte 1 (1- (- word-length bit)))      ;specify field in new word
                 new-word)))
    new-word))

;; the LOW address bit is tied LOW on the proms to use them as 1k
;; this is the REAL address bit zero. The right thing to do is to
;; write the same thing in even and odd halves of the  proms
;; the other way to look at it is that the HIGH bit of the address
;; paths to the proms is zero.

(defun make-masker-set (&optional (reverse-bits nil))
  (do* ((prom-number 0 (1+ prom-number))
        (starting-bit 0 (+ 4. starting-bit)))
       ((= prom-number 8.))
      (make-masker-prom (intern (format nil "MASK-~d" prom-number))
                        starting-bit
                        4.
                        reverse-bits)))

(defun make-masker-prom (name starting-bit &optional (prom-width 4.)(reverse-bits nil))
  (set name (make-array 2048.))             ;high half of ARRAY duplicates low
  (dotimes (address 2048.)                  ;if BIT REVERSED, odd duplicates even in prom.
    (let* ((bits-wide (1+ (ldb 0505 address)))  ;possible widths are from 1 to 32.
           (bits-over (ldb 0005 address))
           (mc1 (ash 37777777777 bits-over))
           (mc2 (ash 37777777777 (+ bits-over bits-wide)))
           (mask (logand 37777777777 (logxor mc1 mc2))))
      (cond (reverse-bits (aset
                            (bit-reverse
                              (ldb (byte prom-width starting-bit) mask) 4.)
                             (symeval name)
                            (bit-reverse address 11.)))
            (t (aset (ldb (byte prom-width starting-bit) mask)
                      (symeval name)
                     address))))))

(defun print-masker-prom-array (array)
  (format t "~%~% bits-wide  bits-over     data~%")
  (do ((length (first (array-dimensions array)))
       (address 0 (1+ address)))
      (( address length))
    (let ((contents  (aref array address)))
      (format t "~%~T~7o  ~T~7o  ~T~12o "
              (ldb 0005 address)
              (1+ (ldb 0505 address))           ;widths are between 1 and 32.
              contents)))
  (terpri))

(defun print-masker-set ()
  (format t "~%~% bits-wide  bits-over     data~%")
  (do ((length 2048.)
       (address 0 (1+ address)))
      (( address length))
    (let ((contents  (LIST (aref MASK-7 address)
                             (aref MASK-6 address)
                             (aref MASK-5 address)
                             (aref MASK-4 address)
                             (aref MASK-3 address)
                             (aref MASK-2 address)
                             (aref MASK-1 address)
                             (aref MASK-0 address))))
      (format t "~%~T~7o  ~T~7o  ~{ ~T~2,4r~} "
              (1+ (ldb 0505 address))           ;widths are between 1 and 32.
              (ldb 0005 address)
              contents)))
  (terpri))
