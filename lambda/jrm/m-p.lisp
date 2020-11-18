;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:8; Readtable:ZL -*-

(defun blt-micro-paging-area-into-i-mem ()
  (let ((paging-area-origin (%region-origin micro-code-paging-area)))
    (do ((adr 0 (+ adr 2)))
        ((>= adr (* 2 (* 16. (read-meter '%highest-handcode-ucode-page)))))
      (let ((halfword-0 (%p-ldb (byte 16. 0) (%pointer-plus paging-area-origin adr)))
            (halfword-1 (%p-ldb (byte 16. 16.) (%pointer-plus paging-area-origin adr)))
            (halfword-2 (%p-ldb (byte 16. 0) (%pointer-plus paging-area-origin (1+ adr))))
            (halfword-3 (%p-ldb (byte 16. 16.) (%pointer-plus paging-area-origin (1+ adr)))))
        (%write-internal-processor-memories
          1 (floor adr 2)
          (dpb halfword-3 (byte 16. 16.) halfword-2)
          (dpb halfword-1 (byte 16. 16.) halfword-0)))))
  (%micro-paging 1))

(defvar *i-mem-symbol-tables* nil)

(defun i-mem-symeval (sym &optional (vn %microcode-version-number))
  (gethash (intern (string-upcase sym) "LAMBDA")
           (cadr (assq vn *i-mem-symbol-tables*))))

(defun i-mem-fset (symbol value &optional (vn %microcode-version-number))
  (puthash symbol value (cadr (assq vn *i-mem-symbol-tables*))))

(defun i-mem-find-closest-symbol (value &optional (vn %microcode-version-number))
  (let ((ht (cadr (assq vn *i-mem-symbol-tables*)))
        (bsf-sym nil)
        (bsf-value nil))
    (maphash (function (lambda (sym sym-val)
                         (cond ((and (<= sym-val value)
                                     (or (null bsf-value)
                                         (> sym-val bsf-value)))
                                (setq bsf-sym sym
                                      bsf-value sym-val)))))
             ht)
    (cond ((null bsf-value)
           value)
          ((= bsf-value value)
           bsf-sym)
          (t (list bsf-sym (- value bsf-value))))))

(defun register-symeval (sym &optional (vn %microcode-version-number))
  (setq sym (intern (string-upcase sym) "LAMBDA"))
  (dolist (e (caddr (assq vn *i-mem-symbol-tables*)))
    (cond ((eq (cadr e) sym)
           (return (car e))))))

(defun register-find-symbol (value &optional (vn %microcode-version-number))
  (cadr (assq value (caddr (assq vn *i-mem-symbol-tables*)))))

(defun load-i-mem-symbols (&optional vn filename &aux register-symbols info-list)
  (cond ((null vn)
         (setq vn %microcode-version-number)))
  (cond ((null filename)
         (setq filename  (format nil "SYS:UBIN;ULAMBDA.LMC-SYM.~d" vn)))
        (t (setq filename (funcall filename :new-type-and-version "LMC-SYM" vn))))
  (with-open-file (f filename)
    (let* ((line nil)
           (vn (funcall (funcall f :truename) :version))
           (ht (cadr (assq vn *i-mem-symbol-tables*))))
      (cond ((null ht)
             (setq *i-mem-symbol-tables*
                   (cons (list vn
                               (setq ht (make-hash-table :test 'eq :size 5000.))
                               nil      ;spot for register-symbols
                               nil)     ;spot for info-list
                         *i-mem-symbol-tables*))))
    ;search for a line beginning with '-4'
      (do ((this-line (send f :line-in) (send f :line-in)))
          ((null this-line)
           (ferror nil "unexpected EOF"))
        (cond ((string-equal this-line "-4 " :end1 3)
               (setq line (substring this-line 3))
               (return nil))))
      (setq info-list
            (let ((base 8)
                  (ibase 8))
              (read f)))
    ;search for a line beginning with '-2'
      (do ((this-line (send f :line-in) (send f :line-in)))
          ((null this-line)
           (ferror nil "unexpected EOF"))
        (cond ((string-equal this-line "-2 " :end1 3)
               (setq line (substring this-line 3))
               (return nil))))
      (do ()
          ((or (null line)
               (string-equal line "-1" :end1 2)))
        (let ((first-space (string-search #/space line)))
          (when first-space
            (let ((second-space (string-search #/space line (1+ first-space))))
              (cond ((and second-space
                          (string-equal line "I-MEM"
                                        :start1 (1+ first-space) :end1 second-space))
                     (let ((symbol (intern (substring line 0 first-space) "LAMBDA"))
                           (value (parse-number line (1+ second-space) nil 8)))
                       (puthash symbol value ht)))
                    ((and second-space
                          (string-equal line "A-MEM"
                                        :start1 (1+ first-space) :end1 second-space))
                     (let ((symbol (intern (substring line 0 first-space) "LAMBDA"))
                           (value (parse-number line (1+ second-space) nil 8)))
                       (push (list value symbol) register-symbols)))))))
        (setq line (send f :line-in)))))
  (setf (caddr (assq vn *i-mem-symbol-tables*)) register-symbols)
  (setf (cadddr (assq vn *i-mem-symbol-tables*)) info-list))

(defun load-i-mem-symbols-if-necessary (&optional (vn %microcode-version-number)
                                                   fn)
  (cond ((null (assq vn *i-mem-symbol-tables*))
         (load-i-mem-symbols vn fn))))

(defun read-c-mem (adr)
  (let ((origin (%region-origin micro-code-paging-area))
        (offset (* adr 2)))
    (dpb (%p-ldb (byte 16. 16.) (+ origin offset 1))
         (byte 16. 48.)
         (dpb (%p-ldb (byte 16. 0) (+ origin offset 1))
              (byte 16. 32.)
              (dpb (%p-ldb (byte 16. 16.) (+ origin offset))
                   (byte 16. 16.)
                   (%p-ldb (byte 16. 0) (+ origin offset)))))))

(defun read-field-from-c-mem (field adr)
  "Equivalent to (ldb field (read-c-mem adr)) but may avoid consing"
  (let ((bits (ldb (byte 6 0) field))
        (bits-over (ldb (byte 12. 6) field))
        (origin (%region-origin micro-code-paging-area))
        (offset (* adr 2)))
    (cond ((< (+ bits bits-over) 32.)
           (%p-ldb field (+ origin offset)))
          ((>= bits-over 32.)
           (%p-ldb (byte bits (- bits-over 32.)) (+ origin offset 1)))
          (t (ldb field (read-c-mem adr))))))

(defun print-last-uinst-on-upages ()
  (pkg-bind 'lambda
    (do ((adr 40017 (+ adr 20)))
        ((>= adr 200000))
      (format t "~&")
      (lam:lam-print-uinst (read-c-mem adr))
      )))

;stat bit is (byte 1 57.) = bit 25. in high word
(defun set-stat-bit-in-micro-code-paging-area (adr)
  (check-type adr (fixnum 0 #o177777))
  (let ((paging-area-origin (%region-origin micro-code-paging-area)))
    (%p-dpb 1 (byte 1 25.) (%pointer-plus paging-area-origin (1+ (* adr 2))))))

(defun clear-stat-bit-in-micro-code-paging-area (adr)
  (check-type adr (fixnum 0 #o177777))
  (let ((paging-area-origin (%region-origin micro-code-paging-area)))
    (%p-dpb 0 (byte 1 25.) (%pointer-plus paging-area-origin (1+ (* adr 2))))))

(defun clear-all-stat-bits ()
  (dotimes (adr #o200000)
    (clear-stat-bit-in-micro-code-paging-area adr))
  (blt-micro-paging-area-into-i-mem))

(defun set-stat-bits-in-range (from to &optional (copy-to-hardware t))
  (let (base-symbol
        (base-adr 0)
        end-symbol
        (end-adr 0))
    (cond ((consp from)
           (setq base-symbol (car from))
           (setq base-adr (cadr from)))
          (t
           (setq base-symbol from)))
    (let ((base-symbol-adr (i-mem-symeval base-symbol)))
      (cond ((null base-symbol-adr)
             (format t "~&can't find symbol ~s" from)
             (return-from set-stat-bits-in-range nil))
            (t
             (incf base-adr base-symbol-adr))))

    (cond ((consp to)
           (setq end-symbol (car to))
           (setq end-adr (cadr to)))
          ((numberp to)
           (setq end-adr (+ base-adr to)))
          (t
           (setq end-symbol to)))
    (cond ((not (null end-symbol))
           (let ((end-symbol-adr (i-mem-symeval end-symbol)))
             (cond ((null end-symbol-adr)
                    (format t "~&can't find symbol ~s" to)
                    (return-from set-stat-bits-in-range nil))
                   (t
                    (incf end-adr end-symbol-adr))))))

    (cond ((< end-adr base-adr)
           (format t "Addresses out of order"))
          (t
           (do ((adr base-adr (1+ adr)))
               ((>= adr end-adr))
             (set-stat-bit-in-micro-code-paging-area adr))
           (if copy-to-hardware
               (blt-micro-paging-area-into-i-mem))))))

(defun set-stat-bit (adr)
  (set-stat-bit-in-micro-code-paging-area adr)
  (cond ((< adr (* 16. (read-meter '%highest-handcode-ucode-page)))
         (let ((inst (read-c-mem adr)))
           (%write-internal-processor-memories
             1 adr
             (ash inst -32.)
             inst)))
        (t
         (%micro-paging 1))))

(defun update-range (adr end-adr)
  (do ((block-number (floor adr 16.) (1+ block-number))
       (last-block (ceiling end-adr 16.)))
      ((> block-number last-block))
    (update-block block-number)))

(defun update-block (block-number)
  (let ((adr (* block-number 16.)))
    (cond ((< adr (* 16. (read-meter '%highest-handcode-ucode-page)))
           (dotimes (x 16.)
             (let ((inst (read-c-mem (+ adr x))))
               (%write-internal-processor-memories
                 1 (+ adr x)
                 (ash inst -32.)
                 inst))))
          (t
           (%micro-paging 1)))))


(defun read-stat-bit (sym)
  (let ((adr (if (numberp sym)
                 sym
               (i-mem-symeval sym))))
    (ldb (byte 1 57.) (read-c-mem adr))))

;some interesting ranges:

; qimove-ignore-fef-0  qicxr-ignore-fef      all the MOVE D-PDL LOCAL|0 type instructions
; '(lmp-occurs-in -2)  '(lmp-xload 2)           all of UC-PROLOG
; 'P3ADI '(PN-1  1)                             all of UC-CALL-RETURN
; SGLV  '(sg-alt-main-return  6)                all of UC-STACK-GROUPS
; 'QMLP '(set-simple-q-vector  7)               all of UC-LAMBDA-MACROCODE
; 'xlist '(fsh-list-header-forward 4)           all of UC-STORAGE-ALLOCATION
; 'pgf-r-sb '(p-r-pf 7)                         all of UC-LAMBDA-PAGE-FAULT
; 'xcar '(delq-rplacd 7)                        all of UC-FCTNS
; 'fxgtpp '(BCLEANUP-X 8)                       all of UC-ARITH
; 'trans-old '(p-b-mr0-hack 2)                  all of UC-TRANSPORTER
; 'MC-READ-EXIT-VECTOR '(MC-POP-SPECPDL-AND-SUB-PP 4)  all of UC-MC
; 'array-subscript-error '(qdacm6 6)            all of UC-LAMBDA-ARRAY
; 'zero '(kbd-boot-char-xct-now 4)              all of UC-LAMBDA
; 'set-mouse-screen '(not-buttons-next 2)       all of UC-TRACK-MOUSE

(defvar inst-per-chunk 128.)
(defvar ratio-array (make-array (// #o200000 inst-per-chunk)))

(defun print-stats ()
  (pkg-bind 'lambda
    (dotimes (x (array-length ratio-array))
      (let ((adr (* x inst-per-chunk)))
        (format t "~&~7o ~30s ~8,4f"
                adr
                (lam:lam-find-closest-sym (+ lam:racmo adr))
                (aref ratio-array x))))))

(defun make-stats (pred)
  (fillarray ratio-array nil)
  (si:clear-all-stat-bits)
  (let* ((highest-virtual-cram-adr (i-mem-symeval 'lam:lmp-xload))
         (highest-test-number (+ 3 (// highest-virtual-cram-adr inst-per-chunk))))
    (format t "~&highest test will be ~s" highest-test-number)
    (do ((test-number 1 (1+ test-number)))
        ((>= test-number highest-test-number))
      (format t "~&test-number ~s; adr #o~o; " test-number (* inst-per-chunk test-number))
      (format t "~40s " (lam:lam-find-closest-sym (+ lam:racmo (* inst-per-chunk test-number))))
      (let ((last-block-adr (* inst-per-chunk (1- test-number)))
            (block-adr (* inst-per-chunk test-number)))
  (format t "c")
        (dotimes (x inst-per-chunk)
          (clear-stat-bit-in-micro-code-paging-area (+ last-block-adr x)))
  (format t "s")
        (dotimes (x inst-per-chunk)
          (set-stat-bit-in-micro-code-paging-area (+ block-adr x)))
  (format t "u")
        (update-range last-block-adr (+ block-adr inst-per-chunk))
        (prolog:test-from-lisp pred)
        (format t "ratio ~8,4f"
                (aset (prolog:test-from-lisp pred)
                      ratio-array
                      test-number))))))

(defun stat-on-function-calls ()
  (set-stat-bits-in-range 'qical0-fef 'qimove-ignore-fef-0 nil) ;in UC-LAMBDA-MACROCODE
  (set-stat-bits-in-range 'qmrcl 'qmrcl-trap nil)
  (set-stat-bits-in-range 'P3ADI '(PN-1  1))    ;all of UC-CALL-RETURN
  )

(defun stat-on-macrocode ()
  (set-stat-bits-in-range 'QMLP '(set-simple-q-vector 7))
  )
