;;; -*- Mode:Lisp; Package:(TI68 GLOBAL); Base:8 -*-

; 68k disassembler
; bobp

(defvar pc)
(defvar virtual-offset #xf00000)                ;temp hack for reading v7 kernel
(defvar dis-read-func 'read-from-bin)           ;'read-from-ram
(defvar verbose-p t)

(defun disam (&optional (start-pc #xf00400.) (n-ops 1))
  (setq pc start-pc)
  (do ((i 0 (1+ i)))
      ((eq i n-ops))
    (format t "~&~8x  " pc)
    (pr-as-nearest pc)
    (let ((word (get-word)))
      (format t "~28T")
      (format t "~4,'0x   " word)

      (crack-op word))))

(defun bad-op (word)
  (format t "*** UNKNOWN OPCODE ~4,'0x ***" word))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; crack hi-nibble of 16-bit opcode

(defvar first-nib-funcs '(nib-0 nib-1 nib-2 nib-3 nib-4 nib-5 nib-6 nib-7
                          nib-8 nib-9 bad-op nib-b nib-c nib-d nib-e bad-op))

(defun crack-op (word)
  (let ((nib (ldb (byte 4 12.) word)))
    (funcall (nth nib first-nib-funcs) word)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; crack instructions that start with #x0---

(defvar nib-0-list '("orb" "orw" "orl" nil t t t t "andb" "andw" "andl" nil t t t t
                     "subb" "subw" "subl" nil t t t t "addb" "addw" "addl" nil t t t t
                     "btst" "bchg" "bclr" "bset" t t t t "eorb" "eorw" "eorl" nil t t t t
                     "cmpb" "cmpw" "cmpl" nil t t t t))

(defvar movep-list '("movepw" "movepl"))
(defvar nib-0-1-list '("btst" "bchg" "bclr" "bset"))

(defun nib-0 (word)
  (cond
    ((= 1 (ldb (byte 1 8) word))                ;btst etc.
     (cond
       ((= 1 (ldb (byte 3 3) word))             ;movepw etc. instead of addr-reg-direct
        (if (= 0 (ldb (byte 1 7) word))
           (format t "~a #x~x(a~d),d~d"         ;foo(a#),d#
                   (nth (ldb (byte 1 6) word) movep-list)
                   (get-word)
                   (ldb (byte 3 0) word)        ;address reg
                   (ldb (byte 3 9) word))       ;data reg
           (format t "~a d~d,#x~x(a~d)"         ;d#,foo(a#)
                   (nth (ldb (byte 1 6) word) movep-list)
                   (ldb (byte 3 9) word)        ;data reg
                   (get-word)
                   (ldb (byte 3 0) word))))     ;address reg
       (t
        (format t "~a d~d,"
                (nth (ldb (byte 2 6) word) nib-0-1-list)
                (ldb (byte 3 9) word))          ;data reg
        (adr-mode word))))
    (t
     (format t "~a #x~x,"
             (nth (ldb (byte 6 6) word) nib-0-list)
             (if (and (not (= 8 (ldb (byte 4 8) word))) ;not bset
                      (= 1 (ldb (byte 1 7) word)))      ;long
                 (get-long)
               (get-word)))
     (adr-mode word))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; crack instructions that start with #x1---, #x2---, #x3---

(defun nib-1 (word)
  (format t "movb ")
  (adr-mode word 1)
  (format t ",")
  (adr-mode (adr-field-2 word) 1))

(defun nib-2 (word)
  (format t "movl ")
  (adr-mode word 4)
  (format t ",")
  (adr-mode (adr-field-2 word) 4))

(defun nib-3 (word)
  (format t "movw ")
  (adr-mode word 2)
  (format t ",")
  (adr-mode (adr-field-2 word) 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; crack instructions that start with #x4---

(defvar nib-4-funcs '(crack-40 crack-41 crack-42 crack-41
                      crack-44 crack-41 crack-46 crack-41
                      crack-48 crack-41 crack-4a crack-41
                      crack-4c crack-41 crack-4e crack-41))

(defun nib-4 (word)
  (let ((nib (ldb (byte 4 8) word)))
    (funcall (nth nib nib-4-funcs) word)))

(defun crack-4-common (word l)
  (let ((size-field (ldb (byte 2 6) word)))
    (format t "~a " (nth size-field l))
    (adr-mode word (if (= 3 size-field)
                       nil
                     (ash 1 size-field)))))

(defun crack-40 (word)
  (if (not (= 3 (ldb (byte 2 6) word)))         ;not move sr,
      (crack-4-common word '("negxb" "negxw" "negxl"))
    (format t "move sr,")
    (adr-mode word)))

(defun crack-41 (word)
  (cond
    ((= 3 (ldb (byte 2 6) word))                ;lea
     (format t "lea ")
     (adr-mode word)
     (format t ",a~d" (ldb (byte 3 9) word)))
    (t                                          ;chk
     (format t "chk ")
     (adr-mode word 2)
     (format t ",d~d" (ldb (byte 3 9) word)))))

(defun crack-42 (word)
  (if (not (= 3 (ldb (byte 2 6) word)))         ;not move ccr,
      (crack-4-common word '("clrb" "clrw" "clrl"))
    (format t "move ccr,")
    (adr-mode word)))

(defun crack-44 (word)
  (if (not (= 3 (ldb (byte 2 6) word)))         ;not move ,ccr
      (crack-4-common word '("negb" "negw" "negl"))
    (format t "move ")
    (adr-mode word 1)
    (format t ",ccr")))

(defun crack-46 (word)
  (if (not (= 3 (ldb (byte 2 6) word)))         ;not move ,sr
      (crack-4-common word '("notb" "notw" "notl"))
    (format t "move ")
    (adr-mode word 2)
    (format t ",sr")))

(defun crack-48 (word)                          ;ignores immediate-mode size
  (selectq (ldb (byte 2 6) word)
    (0
     (format t "nbcd "))
    (1
     (if (= 0 (ldb (byte 3 3) word))
         (format t "swap ")
       (format t "pea ")))
    (2
     (if (= 0 (ldb (byte 3 3) word))
         (format t "extw ")
       (format t "movemw #x~x," (get-word))))
    (3
     (if (= 0 (ldb (byte 3 3) word))
         (format t "extl ")
       (format t "moveml #x~x," (get-word)))))
  (adr-mode word))

(defun crack-4a (word)
  (crack-4-common word '("tstb" "tstw" "tstl" "tas")))

(defvar list-4c '("movemw" "moveml"))

(defun crack-4c (word)
  (cond
    ((= 1 (ldb (byte 1 7) word))                ;movemw, moveml
     (format t "~a " (nth (ldb (byte 1 7) word) list-4c))
     (adr-mode word)
     (format t ",#x~x" (get-word)))
    (t
     (bad-op word))))

(defvar list-4e70 '("reset" "nop" "stop" "rte" nil "rts" "trapv" "rtr"))

(defun crack-4e (word)
  (cond
    ((= #x4e40 (logand #xfff0 word))            ;trap vecnum
     (format t "trap #x~x" (ldb (byte 4 0) word)))
    ((= #x4e50 (logand #xfff8 word))            ;link
     (format t "link a~d,#x~x" (ldb (byte 3 0) word) (get-word)))
    ((= #x4e58 (logand #xfff8 word))            ;unlink
     (format t "unlk a~d" (ldb (byte 3 0) word)))
    ((= #x4e60 (logand #xfff8 word))            ;move a#,usp
     (format t "mov a~d,USP" (ldb (byte 3 0) word)))
    ((= #x4e68 (logand #xfff8 word))            ;move usp,a#
     (format t "mov USP,a~d" (ldb (byte 3 0) word)))
    ((= #x4e70 (logand #xfff8 word))            ;reset, nop etc.
     (format t "~a" (nth (ldb (byte 3 0) word) list-4e70))
     (if (= #x4e72 word)                        ;stop has SR arg
         (format t " #x~x" (get-word))))
    ((= #x4e80 (logand #xffc0 word))            ;jsr jadr
     (format t "jsr ")
     (adr-mode word))
    ((= #x4ec0 (logand #xffc0 word))            ;jmp jadr
     (format t "jmp ")
     (adr-mode word))
    (t
     (bad-op word))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; crack instructions that start with #x5---

(defvar addq-subq '("addqb" "addqw" "addql" nil "subqb" "subqw" "subql" nil))

(defvar db-list '("dbt" "dbf" "dbhi" "dbls" "dbcc" "dbcs" "dbne" "dbeq"
                  "dvc" "dvs" "dbpl" "dbmi" "dbge" "dblt" "dbgt" "dble"))

(defvar set-list '("st" "sf" "shi" "sls" "scc" "scs" "sne" "seq"
                   "svc" "svs" "spl" "smi" "sge" "slt" "sgt" "sle"))

(defun nib-5 (word)
  (cond
    ((= 3 (ldb (byte 2 6) word))                ;branch
     (cond
       ((= 1 (ldb (byte 3 3) word))             ;decrement-branch
        (format t "~a d~d,"
                (nth (ldb (byte 4 8) word) db-list)
                (ldb (byte 3 0) word))          ;register number
        (pr-as-nearest (branch-addr 0)))
       (t
        (format t "~a " (nth (ldb (byte 4 8) word) set-list))   ;conditional set
        (adr-mode word))))
    (t                                          ;addq or subq
     (let ((d (ldb (byte 3 9) word)))           ;numeric arg 1..8
       (if (= 0 d)
           (setq d 8))
       (format t "~a ~d," (nth (ldb (byte 3 6) word) addq-subq) d))
     (adr-mode word))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; crack instructions that start with #x6---

(defvar nib-6-list '("bra" "bsr" "bhi" "bls" "bcc" "bcs" "bne" "beq"
                     "bvc" "vbs" "bpl" "bmi" "bge" "blt" "bgt" "ble"))

(defun nib-6 (word)
  (format t "~a " (nth (ldb (byte 4 8) word) nib-6-list))
  (pr-as-nearest (branch-addr (ldb (byte 8 0) word))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; crack instructions that start with #x6---

(defun nib-7 (word)
  (if (= 0 (ldb (byte 1 8) word))
      (format t "moveq ~x,d~d" (ext-byte (ldb (byte 8 0) word)) (ldb (byte 3 9) word))
    (bad-op word)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; crack instructions that start with #x8---

(defun nib-8 (word)
  (cond
    ((= #x8100 (logand #xf1f8 word))            ;sbcd d#,d#
     (format t "sbcd d~d,d~d" (ldb (byte 3 0) word) (ldb (byte 3 9) word)))
    ((= #x8108 (logand #xf1f8 word))            ;sbcd -(a#),-(a#)
     (dec-xadr-special word "sbcd"))
    ((= #x8000 (logand #xf100 word))            ;8000...
     (sadr-datareg word '("orb" "orw" "orl" "divu") '(1 2 4 2)))
    ((= #x81c0 (logand #xf1c0 word))            ;divs
     (sadr-datareg word '(nil nil nil "divs") '(nil nil nil 2)))
    ((= #x8100 (logand #xf100 word))            ;8100...
     (datareg-dadr word '("orb" "orw" "orl") '(1 2 4)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; crack instructions that start with #x9---

(defun nib-9 (word)
  (cond
    ((= #x90c0 (logand #xf1c0 word))            ;subw addr-reg
     (sadr-addrreg word "subw" 2))
    ((= #x91c0 (logand #xf1c0 word))            ;subl addr-reg
     (sadr-addrreg word "subl" 4))
    ((= #x9108 (logand #xf138 word))            ;subxb etc. -(a#),-(a#)
     (dec-xadr-special word '("subxb" "subxw" "subxl")))
    ((= #x9000 (logand #xf100 word))            ;9000...
     (sadr-datareg word '("subb" "subw" "subl") '(1 2 4)))
    ((= #x9100 (logand #xf100 word))            ;9100...
     (datareg-dadr word '("subxb" "subxw" "subxl") '(1 2 4)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; crack instructions that start with #xb---

(defun nib-b (word)
  (cond
    ((= #xb0c0 (logand #xf1c0 word))            ;cmpw addr-reg
     (sadr-addrreg word "cmpw" 2))
    ((= #xb1c0 (logand #xf1c0 word))            ;cmpl addr-reg
     (sadr-addrreg word "cmpl" 4))
    ((= #xb108 (logand #xf138 word))            ;cmpmb etc. (a#)+,(a#)+
     (inc-xadr-special word '("cmpmb" "cmpmw" "cmpml")))
    ((= #xb000 (logand #xf100 word))            ;b000...
     (sadr-datareg word '("cmpb" "cmpw" "cmpl") '(1 2 4)))
    ((= #xb100 (logand #xf100 word))            ;b100...
     (datareg-dadr word '("eorb" "eorw" "eorl") '(1 2 4)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; crack instructions that start with #xc---

(defun nib-c (word)
  (cond
    ((= #xc1c0 (logand #xf1c0 word))            ;muls
     (sadr-datareg word "muls" 2))
    ((= #xc100 (logand #xf1f8 word))            ;abcd d#,d#
     (sadr-datareg word "abcd"))
    ((= #xc108 (logand #xf1f8 word))            ;abcd -(a#),-(a#)
     (dec-xadr-special word "abcd"))
    ((= #xc140 (logand #xf1f8 word))            ;exg d#,d#
     (format t "exg d~d,d~d" (ldb (byte 3 0) word) (ldb (byte 3 9) word)))
    ((= #xc148 (logand #xf1f8 word))            ;exg a#,a#
     (format t "exg a~d,a~d" (ldb (byte 3 0) word) (ldb (byte 3 9) word)))
    ((= #xc188 (logand #xf1f8 word))            ;exg d#,a#
     (format t "exg d~d,a~d" (ldb (byte 3 0) word) (ldb (byte 3 9) word)))
    ((= #xc000 (logand #xf100 word))            ;c000...
     (sadr-datareg word '("andb" "andw" "andl" "mulu") '(1 2 4 2)))
    ((= #xc1c0 (logand #xf1c0 word))            ;muls
     (sadr-datareg word "muls" 2))
    ((= #xc100 (logand #xf100 word))            ;c100...
     (datareg-dadr word '("andb" "andw" "andl") '(1 2 4)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; crack instructions that start with #xd---

(defun nib-d (word)
  (cond
    ((= #xd0c0 (logand #xf1c0 word))            ;addw addrreg
     (sadr-addrreg word "addw" 4))
    ((= #xd1c0 (logand #xf1c0 word))            ;addl addrreg
     (sadr-addrreg word "addl" 4))
    ((= #xd100 (logand #xf138 word))            ;addxb etc. d#,d#
     (sadr-datareg word '("addxb" "addxw" "addxl")))
    ((= #xd108 (logand #xf138 word))            ;addxb etc. -(a#),-(a#)
     (dec-xadr-special word '("addxb" "addxw" "addxl")))
    ((= #xd000 (logand #xf100 word))            ;d000...
     (datareg-dadr word '("addb" "addw" "addl") '(1 2 4)))
    ((= #xd100 (logand #xf100 word))            ;d100...
     (sadr-datareg word '("addb" "addw" "addl") '(1 2 4)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; crack instructions that start with #xe---

(defvar list-e '("asrb" "lsrb" "roxrb" "rorb"
                 "asrw" "lsrw" "roxrw" "rorw"
                 "asrl" "lsrl" "roxrl" "rorl"
                 nil nil nil nil
                 "aslb" "lslb" "roxlb" "rolb"
                 "aslw" "lslw" "roxlw" "rolw"
                 "asll" "lsll" "roxll" "roll"))

(defun nib-e (word)
  (let ((op-field (dpb (ldb (byte 3 6) word)    ;convert split fields into 5-bit number
                       (byte 3 2)
                       (ldb (byte 2 3) word))))
    (cond
      ((= #xe0c0 (logand #xf8c0 word))          ;op dadr
       (format t "~a " (nth (ldb (byte 3 4) word)
                            '("asr" "asl" "lsr" "lsl" "roxr" "roxl" "ror" "rol")))
       (adr-mode word))
      ((= #xe000 (logand #xf020 word))          ;op count,d#
       (format t "~a ~d,d~d"
               (nth op-field list-e)
               (if (= 0 (ldb (byte 3 9) word))  ;shift count
                   8
                 (ldb (byte 3 9) word))
               (ldb (byte 3 0) word)))          ;dest data reg
      ((= #xe020 (logand #xf020 word))          ;op d#,d#
       (format t "~a d~d,d~d"
               (nth op-field list-e)
               (ldb (byte 3 9) word)            ;data reg with count
               (ldb (byte 3 0) word))))))       ;dest data reg

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; functions to crack common stuff

; The low six bits of word are the addressing-mode field.
;;;; If is-status-reg is t, "immediate" mode means the status register.
; if immediate-mode is nil, immediate mode means the status register,
; if it is a number, it is the size in bytes of immediate data.
; pc has already scanned past immediate data.
(defun adr-mode (word &optional immediate-mode)
  (selectq (ldb (byte 3 3) word)
    (0                                          ;data register direct
     (format t "d~d" (ldb (byte 3 0) word)))
    (1                                          ;address register direct
     (format t "a~d" (ldb (byte 3 0) word)))
    (2                                          ;address register indirect
     (format t "(a~d)" (ldb (byte 3 0) word)))
    (3                                          ;address register indirect with post-increment
     (format t "(a~d)+" (ldb (byte 3 0) word)))
    (4                                          ;address register indirect with pre-decrement
     (format t "-(a~d)" (ldb (byte 3 0) word)))
    (5                                          ;address register indirect with displacement
     (format t "#x~x(a~d)" (get-word) (ldb (byte 3 0) word)))
    (6                                          ;addr reg indir with index and displacement
     (let ((ext-word (get-word)))
        (format t "#x~x(a~d,~a)"
               (ext-disp ext-word)              ;8-bit displacement
               (ldb (byte 3 0) word)            ;address reg
               (ext-reg ext-word))))            ;index reg
    (7
     (selectq (ldb (byte 3 0) word)
       (0                                       ;absolute short
        ; (format t "#x~x" (get-word))
        (pr-as-nearest (ext-short (get-word))))
       (1                                       ;absolute long
        ; (format t "#x~x" (get-long)))
        (pr-as-nearest (get-long)))
       (2                                       ;program counter relative with displacement
        ; (format t "#x~x(pc)" (+ 2 (get-word))))
        (pr-as-nearest (+ pc (ext-short (get-word)))))
       (3                                       ;pc relative with index and displacement
        (let ((ext-word (get-word)))
          (format t "#x~x(~a)" (ext-disp ext-word) (ext-reg ext-word))))
       (4                                       ;immediate or status register
        (if (null immediate-mode)
            (format t "SR")
          (pr-as-nearest (if (= 4 immediate-mode)
                             (get-long)
                           (get-word)))))
       ))))

(defun sadr-datareg (word op-list &optional len-list)
  (sadr-register word op-list len-list "d"))

(defun sadr-addrreg (word op-list &optional len-list)
  (sadr-register word op-list len-list "a"))

(defun sadr-register (word op-list len-list reg-name)
  (let ((size-field (ldb (byte 2 6) word)))
    (format t "~a " (nth-or-atom size-field op-list))
    (adr-mode word (nth-or-atom size-field len-list))
    (format t ",~a~d" reg-name (ldb (byte 3 9) word))))

(defun datareg-dadr (word op-list &optional len-list)
  (register-dadr word op-list len-list "d"))

(defun datareg-dadr (word op-list &optional len-list)
  (register-dadr word op-list len-list "a"))

(defun register-dadr (word op-list len-list reg-name)
  (let ((size-field (ldb (byte 2 6) word)))
    (format t "~a ~a~d," (nth-or-atom size-field op-list) reg-name (ldb (byte 3 9) word))
    (adr-mode word (nth-or-atom size-field len-list))))

(defun dec-xadr-special (word op-list)
  (inc-dec-xadr-special word op-list "~a -(a~d),-(a~d)"))

(defun inc-xadr-special (word op-list)
  (inc-dec-xadr-special word op-list "~a (a~d)+,(a~d)+"))

(defun inc-dec-xadr-special (word op-list format-str)
  (let ((size-field (ldb (byte 2 6) word)))
    (format t format-str
            (nth-or-atom size-field op-list)
            (ldb (byte 3 0) word)
            (ldb (byte 3 9) word))))

(defun inc-xadr-special (word op)
  (format t
          op
          (ldb (byte 3 0) word)
          (ldb (byte 3 9) word)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ext-byte (b)
  "sign-extend byte into short"
  (if (= 1 (ldb (byte 1 7) b))
      (- (- #x100 b))
    b))

(defun ext-short (w)
  "sign-extend short into long"
  (if (= 1 (ldb (byte 1 15.) w))
      (- (- #x10000 w))
    w))

(defun branch-addr (byte)
  "if byte is non-zero, return pc + byte, else return pc + (get-word)"
  (+ pc (if (= 0 byte)
              (ext-short (get-word))
            (ext-byte byte))))

(defun get-long ()
  (dpb (get-word) (byte 16. 16.) (get-word)))

(defun adr-field-2 (word)
  (dpb (ldb (byte 3 6) word) (byte 3 3) (ldb (byte 3 9) word)))

(defun ext-disp (ext-word)
  "return extension-word displacement field"
  (ldb (byte 8 0) ext-word))

(defun ext-reg (ext-word)
  "return extension-word string a3w, d0l, a4 etc."
  (format nil "~a~d~a"
          (if (= 0 (ldb (byte 1 15.) ext-word)) "d" "a")        ;index is data or addr reg
          (ldb (byte 3 12.) ext-word)                           ;index reg
          (if (= 0 (ldb (byte 1 11.) ext-word)) "w" "l")))      ;index is short or long

(defun nth-or-atom (n l)
  (if (listp l)
      (nth n l)
    l))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; functions to read from file, memory, etc.

(defun get-word ()
  (funcall dis-read-func))

(defun read-from-bin ()
  (setq pc (+ 2 pc))
  (let ((xpc (- pc 2 virtual-offset)))
    (dpb (aref bin-array xpc) (byte 8 8) (aref bin-array (1+ xpc)))))

; just for looking at kernel text; onlys works for #xf00000... ==> phys 0
(defun read-from-ram ()
; add virtual-offset ...
  (setq pc (+ 2 pc))
  (dpb (read-ti68-phys-memory-8 (- pc 2)) (byte 8 8) (read-ti68-phys-memory-8 (- pc 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; command processor

; commands:

(defvar command-list '("cmds" 'pr-cmd-list "print command list"
                       "byte" 'pr-as-byte "print as byte"
                       "long" 'pr-as-long "print as long"
                       "short" 'pr-as-short "print as short"
                       "char" 'pr-as-char "print as char"
                       "string" 'pr-as-string "print as string"
                       "sym" 'pr-as-sym "print as symbol"
                       "l1" 'pr-as-l1-map "print as level one map entry"
                       "l2" 'pr-as-l2-map "print as level two map entry"
                       "set-pc" 'set-pc "set pc"
                       "ibase" 'set-input-radix "set input radix"
                       "obase" 'set-output-radix "set output radix"
                       "state" 'pr-proc-state "print processor state"
                       "trace" 'pr-stack-trace "trace stack"
                       "header" 'pr-exec-header "print exec header"
                       "swabs" 'cmd-swab-short "swab short"
                       "swab" 'cmd-swab-long "swab long"
                       ))

; operations

"select address"
"select contents at address"

; (dis)
; dis-> main+4,100?dis
; dis-> main+4,100?swab dis

; main+4                      stack top gets address main+4
;       ,100                  iter count gets 100
;           ?                 indir at address: pop addr; push contents
;            swab             pop n; push swab(n)
;                 dis         dissassemble opcode n; inc pc; read as req.

; main+4,100 dis              dissassemble at addr etc.; no "?"

; dis-> main 4 <tab>       read mem at main+4
;                   _l     print as long; pc += 4

; files:
;  lm1:pace.disas;   disas, macros
;  dj:l.lambda-diag; lam


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
