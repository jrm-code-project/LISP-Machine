;;; -*- Mode:LISP; Package:MICRO; Base:8; Readtable:ZL -*-

(define-micro-function call-for-2-values (macro-function)
  ((m-a) pdl-pop)                               ;get function to call

  ((m-d) (a-constant 2))                        ;number of values

        ((M-K) DPB M-D          ;ADI for return values info
          (LISP-BYTE si:%%ADI-RET-NUM-VALS-EXPECTING)
             (A-CONSTANT (PLUS (BYTE-VALUE Q-DATA-TYPE DTP-FIX)
                               (BYTE-VALUE si:%%ADI-PREVIOUS-ADI-FLAG 1)
                               (BYTE-VALUE si:%%ADI-TYPE si:ADI-RETURN-INFO)
                               (BYTE-VALUE si:%%ADI-RET-STORING-OPTION si:ADI-ST-BLOCK))))
        ((M-D) DPB M-D
          (LISP-BYTE si:%%ADI-RET-NUM-VALS-TOTAL) A-K)

        ((M-K) ADD PDL-POINTER (A-CONSTANT 1))  ;block will start on next pdl word
        (call CONVERT-PDL-BUFFER-ADDRESS)       ;convert M-K (a pdl index) to locative
        ((pdl-push) a-v-nil)
        ((pdl-push) a-v-nil)

        ;;this is the ADI itself
        ((PDL-PUSH) M-K)        ;RETURN VALUES BLOCK POINTER
        ((PDL-PUSH) M-D)
        ;;push the 3 other fixnums in the frame
        (call p3adi)
        ;;push the function
        ((pdl-push) m-a)
        ;;push any args here
        ((arg-call mmcall) (i-arg 0))           ;i-arg is number of args pushed above

        ((m-c) pdl-pop)                         ;last value, also avail in M-T
        ((m-d) pdl-pop)                         ;second to last value
                                                ;...

        ((pdl-push) m-t)
        ((pdl-push) a-v-nil)
        (call xcons)
        ((pdl-push) m-c)
        ((pdl-push) m-t)
        (call xcons)
        ((pdl-push) m-d)
        ((pdl-push) m-t)
        (call xcons)
        (popj))
