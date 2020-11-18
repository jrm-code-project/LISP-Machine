;;; -*- Mode:LISP; Package:KBUG; Base:10; Readtable:CL; compile-in-roots:("K-GLOBAL") -*-



;;;**************************************************************************************************
;;;
;;;       For window oriented debugger.
;;;


(defconstant *array-types* '(
                             (0 . ART-Q)
                             (1 . ART-1B)
                             (2 . ART-2BS)
                             (3 . ART-2B)
                             (4 . ART-4BS)
                             (5 . ART-4B)
                             (6 . ART-8BS)
                             (7 . ART-8B)
                             (8 . ART-16BS)
                             (9 . ART-16B)
                             (10. . ART-32BS)
                             (11. . ART-32B)
                             (12. . ART-STRING)
                             (13. . ART-FAT-STRING)
                             (14. . ART-SINGLE-FLOAT)
                             (15. . ART-DOUBLE-FLOAT)

                             (28. . ART-CONTROL-PDL)
                             (29. . ART-EXTRANEOUS-PDL)
                             (30. . ART-SPECIAL-PDL)
                             (31. . ART-ERROR)
                             ))


(defun decode-array-object (addr)
  (let* ((header (kbug-generic-read-memory addr))
         (array-type (ldb (byte 5. 21.) header))        ; sv-art field
         (dims (list (ldb (byte 21. 0) header)))
         (number-of-dims 1)
         (offset 0)
         (fill-pointer nil)
         (displaced-to nil)
         (displaced-index-offset nil)
         (named-structure-symbol nil)
         (leader-length nil)
         (index-offset nil)
         )
    (if (= array-type 31.)                              ; art-hard
      (let* ((header2 (ldb (byte 24. 0) (kbug-generic-read-memory (1- addr))))
             (leader-offset (ldb (byte 4 0.) header2))  ; offset to either leader header or main array header.
             )
        (setq array-type (cdr (assoc (ldb (byte 5. 9.) header2) *array-types*))) ;; real array-type
        (setq offset (1+ leader-offset))
        (setq number-of-dims (ldb (byte 3. 14.) header2)) ;; number of dimensions.
        (when (logbitp 19. header2) ;; displaced bit.
          (setq displaced-to (kbug-generic-read-memory (- addr (1+ number-of-dims))))
          (setq displaced-to
                (case (k-data-type-name displaced-to)
                  (vinc:$$dtp-array
                   (let* ((header (kbug-generic-read-memory displaced-to))
                          (array-type (ldb (byte 5. 21.) header)))
                     (format nil "#<~A Array, at ~7,0x>"
                             (cdr (if (= array-type 31.)
                                      (assoc (ldb (byte 5. 9.) (kbug-generic-read-memory (1- displaced-to))) *array-types*)
                                    (assoc array-type *array-types*)))
                             displaced-to)))
                  (otherwise
                   (format nil "#<Unboxed Locative ~x>" (ldb (byte 26. 0) displaced-to))))))
        (setq index-offset (kbug-generic-read-memory (- addr (+ 2 number-of-dims))))
        (when (logbitp 21. header2) ;; fill-pointer
          (setq fill-pointer (kbug-generic-read-memory (- addr (1+ leader-offset)))))
        (when (logbitp 20. header2)
          (setq named-structure-symbol (read-symbol-name (kbug-generic-read-memory (- addr (+ 2 leader-offset))) t))
          )
        (when (logbitp 17. header2)
          (setq leader-length (ldb (byte 24. 0) (kbug-generic-read-memory (- addr leader-offset)))
                offset (+ offset leader-length)))
        ;; last collect the dimensions.
        (dotimes (i (1- number-of-dims))
          (setq dims (nconc dims (list (ldb (byte 24. 0) (kbug-generic-read-memory (- addr 2 i))))))
          )
        )
      (setq array-type (cdr (assoc array-type *array-types*))))
    (list (- addr offset) (if displaced-to (+ 1 offset) 32)
          (format nil "#<~a Array ~7,0x>" array-type addr)
          `(:array-type ,array-type :dimensions ,dims :displaced-to ,displaced-to
          :displaced-index-offset ,displaced-index-offset :fill-pointer ,fill-pointer
          :named-structure-symbol ,named-structure-symbol :leader-length ,leader-length)
          t)
    )
  )

(defun decode-symbol-object (addr)
  (let* ((symbol (read-symbol-name addr t)))
    (list addr 0
          (format nil "~A" symbol)
          (list :Value-cell (third (kbug2-get-object-description (+ symbol:*symbol-value* addr)))
                :function-cell (third (kbug2-get-object-description (+ symbol:*symbol-function* addr)))
                :package-cell (third (kbug2-get-object-description (+ symbol:*symbol-package* addr)))
                :plist-cell (third (kbug2-get-object-description (+ symbol:*symbol-plist* addr))))
          t)
    )
  )

(defun get-cons-value (addr)
  (case (k-data-type-name addr)
    (vinc:$$dtp-nil nil)
    (vinc:$$dtp-cons
     (cons (get-cons-value (kbug-generic-read-memory addr))
           (get-cons-value (kbug-generic-read-memory (1+ addr)))))
    (otherwise
     (third (kbug2-get-object-description addr))
     )
    )
  )

(defun decode-cons-object (addr)
  (list addr 2 (format nil "~a" (get-cons-value addr))
        nil nil)
  )

(defun kbug2-get-object-description (addr)
  (let ((data-type (k-data-type-name addr)))
    (case data-type
      (vinc:$$dtp-nil (list addr 0 "NIL" nil nil))
      (vinc:$$dtp-fixnum
       (list addr 0
             (format nil "~a"
                     (cond ((zerop (ldb vinc:%%fixnum-sign-bit addr))
                            (ldb vinc:%%fixnum-field addr))
                           (t
                            (global:minus (1+ (logxor #o37777777 (logand #o37777777 addr)))))))
             nil nil))
      (vinc:$$dtp-symbol (decode-symbol-object addr))
      (vinc:$$dtp-locative
       (list addr 0 (format nil "#<Boxed Locative Pointer ~7,0x>" (logand #xfffffff addr)) nil nil))
      (vinc:$$dtp-unboxed-locative
       (list addr 0 (format nil "#<Unboxed Locative Pointer ~7,0x>" (logand #xfffffff addr)) nil nil))
      (vinc:$$dtp-compiled-function
                                                ;(show-compiled-function addr)
       )
      (vinc:$$dtp-array (decode-array-object addr))
      (vinc:$$dtp-structure
                                                ;(decode-structure-object addr)
       )
      (vinc:$$dtp-code
                                                ;(get-warm-symbolic-address addr)
       )
      (vinc:$$dtp-unbound
                                                ;(format stream "Unbound: ")
                                                ;(show-symbol addr)
       )
      (vinc:$$dtp-cons
       (decode-cons-object addr)
       )
      (otherwise
       (list addr 32. :unknown-object))
      )
    )
  )
