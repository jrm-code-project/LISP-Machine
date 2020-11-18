;;; -*- Mode:LISP; Package:MICRO; Base:8; Readtable:ZL -*-

(defstruct (symbol-table (:type :named-array)
                         (:print "#<~s Version ~d. ~s>"
                                 (type-of symbol-table)
                                 (getf (symbol-table-plist symbol-table) 'lam:version-number)
                                 (%pointer symbol-table)))
  symbol-table-hash-table
  symbol-table-plist
  )

(defvar *symbol-tables* nil)

(defun find-symbol-table (vn)
  (let ((st (cdr (assq vn *symbol-tables*))))
    (when (null st)
      (load-symbol-table vn)
      (setq st (cdr (assq vn *symbol-tables*)))
      (if (null st)
          (ferror nil "can't find symbol table for version ~d." vn)))
    st))

(defun symbol-table-properties (&optional (vn %microcode-version-number))
  (loop for prop in (symbol-table-plist (find-symbol-table vn)) by 'cddr
        collect prop))

(defun symbol-table-get (property &optional default (vn %microcode-version-number))
  (getf (symbol-table-plist (find-symbol-table vn)) property default))

(defun symbol-table-lookup (symbolic-address expected-type version-number)
  "expected-type = NIL means return all info"
  (cond ((integerp symbolic-address)
         symbolic-address)
        (t
         (let ((base-sym symbolic-address)
               (offset 0))
           (when (consp symbolic-address)
             (setq base-sym (car symbolic-address))
             (setq offset (cadr symbolic-address)))
           (if (or (stringp base-sym)
                   (not (eq (symbol-package base-sym) lam:*lambda-package*)))
               (setq base-sym (intern (string-upcase base-sym) lam:*lambda-package*)))
           (let ((info (gethash base-sym (symbol-table-hash-table (find-symbol-table version-number)))))
             (cond ((null info)
                    nil)
                   ((null expected-type)
                    info)
                   ((eq (car info) expected-type)
                    (+ (cadr info) offset))
                   (t
                    nil)))))))

(defun symbol-info (symbolic-address &optional (vn %microcode-version-number))
  (symbol-table-lookup symbolic-address nil vn))

(defun i-mem-lookup (symbolic-address &optional (vn %microcode-version-number))
  (symbol-table-lookup symbolic-address 'i-mem-adr vn))

(defun i-mem-find-closest-symbol (address &optional (vn %microcode-version-number))
  address vn
  )

(defun d-mem-lookup (symbolic-address &optional (vn %microcode-version-number))
  (symbol-table-lookup symbolic-address 'd-mem-adr vn))

(defun d-mem-find-closest-symbol (address &optional (vn %microcode-version-number))
  address vn
  )

(defun a-mem-lookup (symbolic-address &optional (vn %microcode-version-number))
  (symbol-table-lookup symbolic-address 'a-mem-adr vn))

(defun a-mem-find-closest-symbol (address &optional (vn %microcode-version-number))
  address vn
  )

(defun m-mem-lookup (symbolic-address &optional (vn %microcode-version-number))
  (symbol-table-lookup symbolic-address 'm-mem-adr vn))

(defun m-mem-find-closest-symbol (address &optional (vn %microcode-version-number))
  address vn
  )

(defun load-symbol-table (version-number &aux filename st)
  (setq filename (format nil "SYS:UBIN;ULAMBDA.LMC-SYM.~d" version-number))
  (setq *symbol-tables* (delq (assq version-number *symbol-tables*) *symbol-tables*))
  (with-open-file (f filename)
    (let (line)
      (setq st (make-symbol-table))
      (setf (symbol-table-hash-table st) (make-hash-table :test 'eq :size 7000.))
      (setf (symbol-table-plist st) nil)

      ;;search for a line beginning with '-4'
      (do ((this-line (send f :line-in) (send f :line-in)))
          ((null this-line)
           (ferror nil "unexpected EOF"))
        (cond ((string-equal this-line "-4" :end1 2)
               (setq line (substring this-line 2))
               (return nil))))
      (setf (symbol-table-plist st)
            (let ((*read-base* 8)
                  (*print-base* 8)
                  (*package* (find-package "LAMBDA"))
                  (*readtable* si:standard-readtable))
              (read f)))
      ;;search for a line beginning with '-2'
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
                       (puthash symbol (list 'i-mem-adr value) (symbol-table-hash-table st))))
                    ((and second-space
                          (string-equal line "A-MEM"
                                        :start1 (1+ first-space) :end1 second-space))
                     (let ((symbol (intern (substring line 0 first-space) "LAMBDA"))
                           (value (parse-number line (1+ second-space) nil 8)))
                       (puthash symbol (list 'a-mem-adr value) (symbol-table-hash-table st))
                       (when (< value 64.)
                         (puthash (intern (string-append "M" (substring symbol 1)) "LAMBDA")
                                  (list 'm-mem-adr value)
                                  (symbol-table-hash-table st)))))
                    ((and second-space
                          (string-equal line "D-MEM"
                                        :start1 (1+ first-space) :end1 second-space))
                     (let ((symbol (intern (substring line 0 first-space) "LAMBDA"))
                           (value (parse-number line (1+ second-space) nil 8)))
                       (puthash symbol (list 'd-mem-adr value) (symbol-table-hash-table st))))))))
        (setq line (send f :line-in)))))
  (push (cons version-number st) *symbol-tables*)
  nil)
