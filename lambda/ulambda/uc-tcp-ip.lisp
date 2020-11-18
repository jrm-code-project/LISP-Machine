;-*- MODE: LISP; BASE: 8.; READTABLE: ZL -*-

(DEFCONST UC-TCP-IP '(
;;Support for TCP/IP

checksum (misc-inst-entry %ip-checksum)
;call: (%ip-checksum array sum count odd-p)

 ;Register usage:
 ;  (vma)    current address in array
 ;  (m-a)    bytes remaining (as a fixnum)
 ;  (m-b)    0,1,2,3 -- byte within word
 ;  (m-c)    0,1 -- have-word flag
 ;  (m-d)    0,1 -- odd-p (0 -> add to high-sum, 1 -> add to low-sum)
 ;  (m-1)    high-sum
 ;  (m-2)    low-sum
 ;  (m-t)    current byte
 ;  (md)     current word from array

 ;Get odd-p into m-t
        ((m-t) c-pdl-buffer-pointer-pop)
        (jump-equal-xct-next m-t a-v-nil checksum-not-odd-p)
       ((m-d) a-zero)
        ((m-d) a-minus-one)
checksum-not-odd-p

 ;Get count into m-a
        ((m-a) q-pointer c-pdl-buffer-pointer-pop)

 ;Get sum into m-t, high-sum into m-1, and low-sum into m-2
        ((m-t) c-pdl-buffer-pointer-pop)

 ;Get array into m-array-pointer
        ((m-array-pointer) invalidate-array-cache c-pdl-buffer-pointer-pop)
        ((pdl-push) m-t)                        ;Save sum
        ((pdl-push) m-d)                        ;Save odd-p
        (call-xct-next GADPTR)                  ;Get array header, following indirection
       ((pdl-push) m-a)                         ;Save count
        ((m-a) pdl-pop)                         ;Restore count
        ((m-d) pdl-pop)                         ;Restore odd-p
        ((m-1) ldb (byte-field 8. 8.) pdl-top)  ;Get high-sum into m-1
        ((m-2) ldb (byte-field 8. 0) pdl-pop)   ;Get low-sum into m-2
        ((m-3) add m-a a-q)                     ;count + initial index

        (jump-greater-xct-next m-3 a-array-length checksum-done)        ;bounds check
       ((m-b) m-q)                              ;Initial index into m-b
        ((m-3) ldb (byte-field 23. 2) m-q)      ;High-bytes of initial index
        ((m-3) m-a-1 m-3 a-zero)                ; ... -1
        ((vma) add m-array-origin a-3)          ;Now vma points one before first actual word of array

        (jump-equal-xct-next m-a a-zero checksum-done)  ;If count is zero, we are done
       ((m-c) a-zero)                           ;We don't have a word yet

checksum-loop
        (jump-equal-xct-next m-c a-minus-one checksum-have-word)        ;branch if we already have word
       ((m-c) a-minus-one)                      ;Indicate we have word
        ((vma-start-read) m+1 vma)              ;Read next word
        (check-page-read)

checksum-have-word
         (dispatch-xct-next (byte-field 2 0) m-b d-checksum-byte)
        ((m-a) m-a-1 m-a a-zero)

(locality d-mem)
(start-dispatch 2 0)
d-checksum-byte (checksum-byte-0)
                (checksum-byte-1)
                (checksum-byte-2)
                (checksum-byte-3)
(end-dispatch)
(locality i-mem)

checksum-byte-0
         (jump-xct-next checksum-have-byte)
        ((m-t) ldb (byte-field 8. 0) md)

checksum-byte-1
         (jump-xct-next checksum-have-byte)
        ((m-t) ldb (byte-field 8. 8.) md)

checksum-byte-2
         (jump-xct-next checksum-have-byte)
        ((m-t) ldb (byte-field 8. 16.) md)

checksum-byte-3
         ((m-t) ldb (byte-field 8. 24.) md)
         ((m-c) a-zero)                         ;Need a new word next time

checksum-have-byte
         (jump-not-equal-xct-next m-d a-zero checksum-add-to-low)
        ((m-b) m+1 m-b)                         ;Point to next byte within word
         ((m-1) add m-1 a-t)                    ;Add to high-sum
         (jump-not-equal-xct-next m-a a-zero checksum-loop)     ;Go get next byte
        ((m-d) a-minus-one)                     ;Add to low-sum next time
         (jump checksum-done)                   ;We are done -- go assemble the checksum

checksum-add-to-low
         ((m-2) add m-2 a-t)                    ;Add to low-sum
         (jump-not-equal-xct-next m-a a-zero checksum-loop)     ;Go get next byte
        ((m-d) a-zero)                          ;Add to high-sum next time

checksum-done
         ((m-3) ldb (byte-field 24. 8.) m-2)    ;Get carry from low-sum
         ((m-1) add m-1 a-3)                    ;Add into high-sum
         ((m-2) dpb m-1 (byte-field 24. 8.) a-2)        ;Stick high-sum above low-sum
checksum-done-loop
         ((m-3) ldb (byte-field 16. 16.) m-2)   ;Get carry from concatenated bytes
         (jump-equal-xct-next m-3 a-zero checksum-return)       ;If no carry, we are done
        ((m-1) ldb (byte-field 16. 0.) m-2)     ;Put low 16 bits of sum into m-1
         (jump-xct-next checksum-done-loop)
        ((m-2) add m-1 a-3)                     ;Add carry into sum

checksum-return
         (popj-after-next                       ;Move checksum to m-t, box it, and return
           (m-t) dpb m-2 q-pointer (a-constant (byte-value q-data-type dtp-fix)))
        (no-op)

))
