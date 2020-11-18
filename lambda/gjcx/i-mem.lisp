
(DefConst %%MInst-Write-Control-Store (Byte 1. 9.))
(DefConst %%MInst-Read-Control-Store (Byte 1. 8.))

;;; Special case jump instruction: Read/Write with Instruction memory
(PreDef Access-I-Mem '(Plus Call-Xct-Next Always))
(PreDef Write-I-Mem
        `(Plus Popj Always ,(Byte-Value 1 %%MInst-Write-Control-Store)))
(PreDef Read-I-Mem
        `(Plus Popj Always ,(Byte-Value 1 %%MInst-Read-Control-Store)))



PROCESS-I-MEM-SECTION
        (jump if-equal m-d a-zero PROCESS-SECTION)

        ((m-a) (byte-field 18. 14.) m-c)
        (jump if-not-equal m-a a-zero BAD-MICROLOAD-FORMAT)     ;bad section address

        (call-xct-next GET-NEXT-WORD)
       ((m-d) sub m-d a-hex-1)
        (call-xct-next GET-NEXT-WORD)
       ((m-b) m-a)

        ;; M-B has first word of instruction, M-A has second word
        ;; M-C has i-memory address

        ((imod-low) dpb m-c %imod-low-new-micro-pc a-zero)
        (ACCESS-I-MEM)
        (WRITE-I-MEM m-a a-b)
        (jump-xct-next PROCESS-I-MEM-SECTION)
       ((m-c) add m-c a-hex-1)
