;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:8 -*-

(defvar *i-mem-symbol-tables* nil)
(defstruct (symbol-table (:type list))
  symbol-table-version
  symbol-table-i-mem-hash-table
  symbol-table-register-symbols
  symbol-table-info-list
  symbol-table-d-mem-hash-table
  )

(defun i-mem-symeval (sym &optional (vn %microcode-version-number) &aux (offset 0))
  (cond ((numberp sym)
         sym)
        (t
         (when (consp sym)
           (setq offset (cadr sym))
           (setq sym (car sym)))
         (when (or (stringp sym)
                   (not (eq (symbol-package sym) lam:*lambda-package*)))
           (setq sym (intern (string-upcase sym) lam:*lambda-package*)))
         (let ((base-adr (gethash sym (cadr (assq vn *i-mem-symbol-tables*)))))
           (if (null base-adr)
               (ferror nil "can't find ~s" sym))
           (+ base-adr offset)))))

(defun d-mem-symeval (sym &optional (vn %microcode-version-number) &aux (offset 0))
  (cond ((numberp sym)
         sym)
        (t
         (when (consp sym)
           (setq offset (cadr sym))
           (setq sym (car sym)))
         (when (or (stringp sym)
                   (not (eq (symbol-package sym) lam:*lambda-package*)))
           (setq sym (intern (string-upcase sym) lam:*lambda-package*)))
         (let ((base-adr (gethash sym (symbol-table-d-mem-hash-table (assq vn *i-mem-symbol-tables*)))))
           (if (null base-adr)
               (ferror nil "can't find ~s" sym))
           (+ base-adr offset)))))

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
  (let ((answer (dolist (e (caddr (assq vn *i-mem-symbol-tables*)))
                  (cond ((eq (cadr e) sym)
                         (return (car e)))))))
    (cond ((and (null answer)
                (char-equal (aref (string sym) 0) #/M))
           (let ((new-answer (register-symeval (intern (string-append "A"
                                                                      (substring sym 1))
                                                       "LAMBDA"))))
             (cond ((and (integerp new-answer)
                         (< new-answer 64.))
                    new-answer)
                   (t
                    nil))))
          (t
           answer))))


(defun register-find-symbol (value &optional (vn %microcode-version-number))
  (cadr (assq value (caddr (assq vn *i-mem-symbol-tables*)))))

(defun a-constant-address (a-constant &optional (vn %microcode-version-number))
  "Return the location in a memory containing the constant A-CONSTANT, or NIL if none."
  (let ((a-constant-list (getf (symbol-table-info-list (assq vn *i-mem-symbol-tables*))
                               'a-constant-list)))
    (cadr (cli:assoc a-constant a-constant-list))))

(defun info-list-items (&optional (vn %microcode-version-number))
  (loop for tag in (symbol-table-info-list (assq vn *i-mem-symbol-tables*)) by 'cddr
        collect tag))

(defun print-info-list (&optional (vn %microcode-version-number))
  (let ((*print-level* 1))
    (print (symbol-table-info-list (assq vn *i-mem-symbol-tables*)))
    nil))

(defun get-info-list-item (item &optional (vn %microcode-version-number))
  (getf (symbol-table-info-list (assq vn *i-mem-symbol-tables*))
        item))

(defun highest-i-mem-location ()
  (get-info-list-item 'pagable-i-mem-loc))

(defun load-i-mem-symbols (&optional vn filename &aux register-symbols info-list)
  (cond ((null vn)
         (setq vn %microcode-version-number)))
  (cond ((null filename)
         (setq filename  (format nil "SYS:UBIN;ULAMBDA.LMC-SYM.~d" vn)))
        (t (setq filename (funcall filename :new-type-and-version "LMC-SYM" vn))))
  (setq *i-mem-symbol-tables*
        (delq (assq vn *i-mem-symbol-tables*)
              *i-mem-symbol-tables*))
  (with-open-file (f filename)
    (let* ((line nil)
           ht d-hash)
      (setq *i-mem-symbol-tables*
            (cons (list vn
                        (setq ht (make-hash-table :test 'eq :size 7000.))
                        nil     ;spot for register-symbols
                        nil     ;spot for info-list
                        (setq d-hash (make-hash-table :test 'eq :size 500.))
                        )
                  *i-mem-symbol-tables*))
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
                       (push (list value symbol) register-symbols)))
                    ((and second-space
                          (string-equal line "D-MEM"
                                        :start1 (1+ first-space) :end1 second-space))
                     (let ((symbol (intern (substring line 0 first-space) "LAMBDA"))
                           (value (parse-number line (1+ second-space) nil 8)))
                       (puthash symbol value d-hash)))))))
        (setq line (send f :line-in)))))
  (setf (caddr (assq vn *i-mem-symbol-tables*)) register-symbols)
  (setf (cadddr (assq vn *i-mem-symbol-tables*)) info-list)
  nil)



(defun load-i-mem-symbols-if-necessary (&optional (vn %microcode-version-number)
                                                   fn)
  (cond ((null (assq vn *i-mem-symbol-tables*))
         (load-i-mem-symbols vn fn))))

(defun read-c-mem (adr)
  (setq adr (i-mem-symeval adr))
  (let ((origin (%region-origin micro-code-paging-area))
        (offset (* adr 2)))
    (dpb (%p-ldb (byte 16. 16.) (+ origin offset 1))
         (byte 16. 48.)
         (dpb (%p-ldb (byte 16. 0) (+ origin offset 1))
              (byte 16. 32.)
              (dpb (%p-ldb (byte 16. 16.) (+ origin offset))
                   (byte 16. 16.)
                   (%p-ldb (byte 16. 0) (+ origin offset)))))))

(defun write-c-mem (adr inst)
  (setq adr (i-mem-symeval adr))
  (let ((origin (+ (* adr 2) (%region-origin micro-code-paging-area))))
    (without-interrupts
      (%p-dpb (ldb (byte 16. 0) inst) (byte 16. 0) origin)
      (%p-dpb (ldb (byte 16. 16.) inst) (byte 16. 16.) origin)
      (%p-dpb (ldb (byte 16. 32.) inst) (byte 16. 0) (+ origin 1))
      (%p-dpb (ldb (byte 16. 48.) inst) (byte 16. 16.) (+ origin 1)))
    (cond ((< adr (* 16. (read-meter 'si:%highest-handcode-ucode-page)))
           (%write-internal-processor-memories
             1 adr
             (ash inst -32.)
             inst))
          (t
           (%micro-paging 1)))
    inst))

;stat bit is (byte 1 57.) = bit 25. in high word
(defun write-stat-bit (adr val)
  (setq adr (i-mem-symeval adr))
  (write-c-mem adr
               (dpb val
                    (byte 1 57.)
                    (read-c-mem adr))))

(defun read-stat-bit (sym)
  (setq sym (i-mem-symeval sym))
  (ldb (byte 1 57.) (read-c-mem sym)))

(defun write-stat-bits-in-range (from to val)
  (setq from (i-mem-symeval from))
  (setq to (i-mem-symeval to))
  (if (not (< 0 from to 65536.))
      (ferror nil "addresses out of range"))
  (do ((adr from (1+ adr)))
      ((>= adr to))
    (write-stat-bit adr val)))

#|
(write-stat-bits-in-range 'qmlp '(qmlp 3) 0)
(write-stat-bits-in-range 'p3adi '(LOAD-PDL-BUFFER-INDEX 3) 1)
(write-stat-bits-in-range 'qical0-fef 'qimove-ignore-fef-0 1)
(write-stat-bits-in-range 'qmddl 'fetch-fef-offset 1)
|#

(defun set-macro-single-step (flag)
  "FLAG means:
 T   - go to QMLP between every macro instruction
 NIL - transfer directly from the end of one macro instruction to the beginning
       of the next, if it is already present in the high bits of the macro-ir"
  (let* ((adr (i-mem-symeval '(lam:sg-alt-main-x -1)))
         (inst (read-c-mem adr))
         (a-adr (a-constant-address (dpb 1 (byte 1 #o37) 0))))
    (if (or (null a-adr)
            (not (= (ldb lam:lam-ir-op inst) lam:lam-op-alu))
            (not (= (ldb lam:lam-ir-ob inst) lam:lam-ob-alu))
            (not (= (ldb lam:lam-ir-func-dest inst) lam:lam-func-dest-rg-mode))
            (not (= (ldb lam:lam-ir-m-src inst) lam:lam-m-src-rg-mode)))
        (ferror nil "unexpected instruction"))
    (setq inst (dpb (if flag
                        lam:lam-alu-ior
                      4 ;lam:lam-alu-andca
                      )
                    lam:lam-ir-aluf
                    inst))
    (write-c-mem adr inst)))
