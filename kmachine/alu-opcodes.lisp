;;; -*- Mode:LISP; Package:HARDWARE; Base:10; Readtable:CL -*-

;;;     A --> left
;;;     B --> right
;;;  Ybus --> out-reg

;;; group 1: byte boundary aligned operand - data movement
(defconstant $$i-alu-op-zero-ext-left           0 "zero extend left")
(defconstant $$i-alu-op-zero-ext-right          1 "zero extend right")
(defconstant $$i-alu-op-sign-ext-left           2 "sign extend left")
(defconstant $$i-alu-op-sign-ext-right          3 "sign extend right")
(defconstant $$i-alu-op-pass-stat               4 "move status to out-reg")
(defconstant $$i-alu-op-pass-q                  5 "move q register to out-reg")
(defconstant $$i-alu-op-load-q-left             6 "load Q register from left")
(defconstant $$i-alu-op-load-q-right            7 "load Q register from right")
(defconstant $$i-alu-op-merge-right-left
             #x0e "merge byte left into right")
   ;byte width specifies the number of least significant bytes of right that
   ; pass to out-reg.  The unselected most significant bytes contain the corresponding bytes
   ; of left.   BW=0 causes all bytes of right to pass.  Status test selected bytes of
   ; right only.
(defconstant $$i-alu-op-merge-left-right
             #x0f "merge byte right into left")
   ;byte width specifies the number of least significant bytes of left that
   ; pass to out-reg.  The unselected most significant bytes contain the corresponding bytes
   ; of right.   BW=0 causes all bytes of left to pass.  Status test on selected bytes of
   ; left only.

(defconstant $$i-alu-op-ld-stat-left            #x1c "load status register from left")
(defconstant $$i-alu-op-ld-stat-right           #x1d "load status register from right")

;;; group 2: byte boundary aligned operand - logical
(defconstant $$i-alu-op-not-left                #x08 "one's compilment left")
(defconstant $$i-alu-op-not-right               #x09 "one's compilment right")
(defconstant $$i-alu-op-or                      #x3e "or of left and right")
(defconstant $$i-alu-op-xor                     #x3f "exclusive or of left and right")
(defconstant $$i-alu-op-and                     #x40 "and of left and right")
(defconstant $$i-alu-op-xnor                    #x41 "not exclusive or of left and right")
(defconstant $$i-alu-op-zero                    #x3c "output zero to out-reg")
(defconstant $$i-alu-op-sign                    #x3d "output sign to out-reg")

;;; group 3: byte boundary aligned operand - single bit shifts
(defconstant $$i-alu-op-dnl-0f-left             #x20 "right-shift left  with zero")
(defconstant $$i-alu-op-dnl-0f-right            #x21 "right-shift right with zero")
(defconstant $$i-alu-op-dnl-1f-left             #x24 "right-shift left  with one")
(defconstant $$i-alu-op-dnl-1f-right            #x25 "right-shift right with one")
(defconstant $$i-alu-op-dnl-lf-left             #x28 "right-shift left  with link")
(defconstant $$i-alu-op-dnl-lf-right            #x29 "right-shift right with link")
(defconstant $$i-alu-op-dnl-ar-left             #x2c "right-shift left  with sign")
(defconstant $$i-alu-op-dnl-ar-right            #x2d "right-shift right with sign")

;;; single bit downshift (double precision)
(defconstant $$i-alu-op-dnl-0f-left-q   #x22 "right-shift left  and q-reg with zero")
(defconstant $$i-alu-op-dnl-0f-right-q  #x23 "right-shift right and q-reg with zero")
(defconstant $$i-alu-op-dnl-1f-left-q   #x26 "right-shift left  and q-reg with one")
(defconstant $$i-alu-op-dnl-1f-right-q  #x27 "right-shift right and q-reg with one")
(defconstant $$i-alu-op-dnl-lf-left-q   #x2a "right-shift left  and q-reg with link")
(defconstant $$i-alu-op-dnl-lf-right-q  #x2b "right-shift right and q-reg with link")
(defconstant $$i-alu-op-dnl-ar-left-q   #x2e "right-shift left  and q-reg with sign")
(defconstant $$i-alu-op-dnl-ar-right-q  #x2f "right-shift right and q-reg with sign")

;;; single bit upshift (single-precision)
(defconstant $$i-alu-op-upl-0f-left             #x30 "left-shift left  with zero")
(defconstant $$i-alu-op-upl-0f-right            #x31 "left-shift right with zero")
(defconstant $$i-alu-op-upl-1f-left             #x34 "left-shift left  with one")
(defconstant $$i-alu-op-upl-1f-right            #x35 "left-shift right with one")
(defconstant $$i-alu-op-upl-lf-left             #x38 "left-shift left  with link")
(defconstant $$i-alu-op-upl-lf-right            #x39 "left-shift right with link")

;;; single bit upshift (double precision)
(defconstant $$i-alu-op-upl-0f-left-q   #x32 "left-shift left  and q-reg with zero")
(defconstant $$i-alu-op-upl-0f-right-q  #x33 "left-shift right and q-reg with zero")
(defconstant $$i-alu-op-upl-1f-left-q   #x36 "left-shift left  and q-reg with one")
(defconstant $$i-alu-op-upl-1f-right-q  #x37 "left-shift right and q-reg with one")
(defconstant $$i-alu-op-upl-lf-left-q   #x3a "left-shift left  and q-reg with link")
(defconstant $$i-alu-op-upl-lf-right-q  #x3b "left-shift right and q-reg with link")

;;; group 4: byte boundary aligned operand - arithmetic
(defconstant $$i-alu-op-prior-left              #x0c "prioritize left  operand")
(defconstant $$i-alu-op-prior-right             #x0d "prioritize right operand")

;;; group 5: byte boundary aligned operand - arithmetic
(defconstant $$i-alu-op-neg-left                #x0a "two's complement of left  operand")
(defconstant $$i-alu-op-neg-right               #x0b "two's complement of right operand")

(defconstant $$i-alu-op-decr1-left              #x10 "decrement left  operand by one")
(defconstant $$i-alu-op-decr1-right             #x11 "decrement right operand by one")  ;
(defconstant $$i-alu-op-decr2-left              #x14 "decrement left  operand by two")
(defconstant $$i-alu-op-decr2-right             #x15 "decrement right operand by two")
(defconstant $$i-alu-op-decr4-left              #x18 "decrement left  operand by four")
(defconstant $$i-alu-op-decr4-right             #x19 "decrement right operand by four")

(defconstant $$i-alu-op-incr1-left              #x12 "increment left  operand by one")
(defconstant $$i-alu-op-incr1-right             #x13 "increment right operand by one")  ;
(defconstant $$i-alu-op-incr2-left              #x16 "increment left  operand by two")
(defconstant $$i-alu-op-incr2-right             #x17 "increment right operand by two")
(defconstant $$i-alu-op-incr4-left              #x1a "increment left  operand by four")
(defconstant $$i-alu-op-incr4-right             #x1b "increment right operand by four")

(defconstant $$i-alu-op-add                     #x42 "add")
(defconstant $$i-alu-op-addc                    #x43 "add with carry")
(defconstant $$i-alu-op-sub                     #x44 "subtract right from left")
(defconstant $$i-alu-op-subr                    #x46 "subtract left  from right")
(defconstant $$i-alu-op-subc                    #x45 "subtract right from left with carry")
(defconstant $$i-alu-op-subrc           #x47 "subtract left  from right with carry")
(defconstant $$i-alu-op-sum-corr-left   #x48 "BCD correct left  for partial sum")
(defconstant $$i-alu-op-sum-corr-right  #x49 "BCD correct right for partial sum")
(defconstant $$i-alu-op-diff-corr-left  #x4a "BCD correct left  for partial difference")
(defconstant $$i-alu-op-diff-corr-right #x4b "BCD correct right for partial difference")

;;; group 6: byte boundary aligned operands - division steps
(defconstant $$i-alu-op-sdiv-first              #x4e "signed divide, first step")
(defconstant $$i-alu-op-sdiv-step               #x50 "signed divide, intermediate step")
(defconstant $$i-alu-op-sdiv-last1              #x51 "signed divide, last step 1")
(defconstant $$i-alu-op-sdiv-last2              #x5a "signed divide, last step 2")
(defconstant $$i-alu-op-udiv-first              #x4f "unsigned divide, first step")
(defconstant $$i-alu-op-udiv-step               #x54 "unsigned divide, intermediate step")
(defconstant $$i-alu-op-udiv-last1              #x55 "unsigned divide, last step 1")
(defconstant $$i-alu-op-remcorr                 #x58 "signed and unsigned remainder correct")
(defconstant $$i-alu-op-quocorr                 #x59 "signed quotient correct")
(defconstant $$i-alu-op-mp-div-step1            #x52 "inner loop first step for multiprecision division")
(defconstant $$i-alu-op-mp-div-step2            #x56 "inner loop intermediate step for multiprecision division")
(defconstant $$i-alu-op-mp-s-div-step3          #x53 "inner loop last step for signed multiprecision division")
(defconstant $$i-alu-op-mp-u-div-step3          #x57 "inner loop last step for unsigned multiprecision division")
(defconstant $$i-alu-op-umul-first              #x5b "unsigned multiply, first step")
(defconstant $$i-alu-op-umul-step               #x5c "unsigned multiply, intermediate step")
(defconstant $$i-alu-op-umul-last               #x5d "unsigned multiply, last step")
        ;;; there might be something funny here, these have switched opcode order from the unsigned case
(defconstant $$i-alu-op-smul-first              #x5f "signed multiply, first step")
(defconstant $$i-alu-op-smul-step               #x5e "signed multiply, intermediate step")

;;; group 8: n bit shifts and rotates
(defconstant $$i-alu-op-nb-sn-sh-left           #x60 "n bit shift left  operand with sign fill")
(defconstant $$i-alu-op-nb-sn-sh-right          #x61 "n bit shift right operand with sign fill")
(defconstant $$i-alu-op-nb-0f-sh-left           #x62 "n bit shift left  operand with zero fill")
(defconstant $$i-alu-op-nb-0f-sh-right          #x63 "n bit shift right operand with zero fill")
(defconstant $$i-alu-op-nb-rot-left             #x64 "n bit rotate left  operand")
(defconstant $$i-alu-op-nb-rot-right            #x65 "n bit rotate right operand")

;;; group 9: variable length bit field, single bit
(defconstant $$i-alu-op-ext-bit-from-left       #x66 "extract bit from left operand")
(defconstant $$i-alu-op-ext-bit-from-right      #x67 "extract bit from right operand")
(defconstant $$i-alu-op-set-bit-left            #x68 "set bit in left operand")
(defconstant $$i-alu-op-set-bit-right           #x69 "set bit in right operand")
(defconstant $$i-alu-op-rst-bit-left            #x6a "reset bit in left operand")
(defconstant $$i-alu-op-rst-bit-right           #x6b "reset bit in left operand")
(defconstant $$i-alu-op-set-bit-stat            #x6c "set bit in status register")
(defconstant $$i-alu-op-rst-bit-stat            #x6d "reset bit in status register")
(defconstant $$i-alu-op-ext-bit-stat            #x7e "extract bit from status-register")

;;; group 10: variable length bit field - aligned
(defconstant $$i-alu-op-not-f-al-left   #x71 "insert aligned not left into right")
(defconstant $$i-alu-op-pass-f-al-left  #x73 "insert aligned left into right")
(defconstant $$i-alu-op-or-f-al-left    #x75 "logical or of aligned field of left into right")
(defconstant $$i-alu-op-xor-f-al-left   #x77 "logical xor of aligned field of left into right")
(defconstant $$i-alu-op-and-f-al-left   #x79 "logical and of aligned field of left into right")
(defconstant $$i-alu-op-not-f-al-right  #x6e "invert field of right operand")
(defconstant $$i-alu-op-pass-f-al-right #x6f "test a field in the right operand")

;;; field logical - non-aligned
(defconstant $$i-alu-op-not-f-left              #x70 "insert non-aligned not left into right")
(defconstant $$i-alu-op-pass-f-left             #x72 "insert non-aligned left into right")      ;this is DPB
(defconstant $$i-alu-op-or-f-left               #x74 "logical or of non-aligned field of left into right")
(defconstant $$i-alu-op-xor-f-left              #x76 "logical xor of non-aligned field of left into right")
(defconstant $$i-alu-op-and-f-left              #x78 "logical and of non-aligned field of left into right")

;;; field logical - extract
(defconstant $$i-alu-op-ext-f-left              #x7a "extract field from left")
(defconstant $$i-alu-op-ext-f-right             #x7b "extract field from right")
(defconstant $$i-alu-op-ext-f-left-right        #x7c "extract field from 64 bit number, left is most significant")
(defconstant $$i-alu-op-ext-f-right-left        #x7d "extract field from 64 bit number, right is most significant")

;;; group 11: variable length bit field - mask generation
(defconstant $$i-alu-op-pass-mask               #x7f "move mask to out-reg")

;;; unused opcodes
(defconstant $$i-alu-reserved-1e                #x1e "reserved")
(defconstant $$i-alu-reserved-1f                #x1f "reserved")
(defconstant $$i-alu-reserved-4c                #x4c "reserved")
(defconstant $$i-alu-reserved-4d                #x4d "reserved")
