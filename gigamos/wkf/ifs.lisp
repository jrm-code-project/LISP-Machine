;;; -*- Mode:LISP; Package:LISP-INTERNALS; Base:10; Readtable:ZL -*-

(defun find-lisp-ifs ()
  (dotimes (i (pkg-number-of-slots pkg-lisp-package))
    (when (and (numberp (pkg-slot-code pkg-lisp-package i))
               (string= (symbol-name (pkg-slot-symbol pkg-lisp-package i)) "IF"))
      (print i))))

(defun find-dups (&optional (pkg pkg-lisp-package))
  (let ((ans nil))
    (dotimes (i (pkg-number-of-slots pkg))
      (when (numberp (pkg-slot-code pkg i))
        (dotimes (j (pkg-number-of-slots pkg))
          (when (and (numberp (pkg-slot-code pkg j))
                     (not (= i j))
                     (string= (symbol-name (pkg-slot-symbol pkg i))
                              (symbol-name (pkg-slot-symbol pkg j))))
            (push i ans)
            (format t "~%~4d ~8x ~8x ~8x ~s"
                    i
                    (pkg-slot-code pkg i)
                    (pkg-code-hash-code (pkg-slot-code pkg i))
                    (pkg-string-hash-code (symbol-name (pkg-slot-symbol pkg i)))
                    (pkg-slot-symbol pkg i))))))
    ans))

(defun find-lisp-rehashes ()
  (dolist (ent pkg-rehash-symbols)
    (when (eq (cdr ent) pkg-lisp-package)
      (print (car ent)))))

(defun find-warm-ifs (&aux syms)
  (dolist (s *saved-warm-symbols*)
    (when (string= (symbol-name s) "IF")
      (push s syms)))
  syms)

(defun find-warm-dups ()
  (do ((s *saved-warm-symbols* (cdr s)))
      ((null s))
    (let ((n (symbol-name (car s))))
      (dolist (x (cdr s))
        (when (string= n (symbol-name x))
          (format t "~s ~s~%" (car s) x))))))

(defun find-lisp-symbols-dups ()
  (do ((s lisp-symbols (cdr s)))
      ((null s))
    (let ((n (symbol-name (car s))))
      (dolist (x (cdr s))
        (when (string= n (symbol-name x))
          (format t "~s ~s~%" (car s) x))))))

(defun smh (pkg)
  (dotimes (i (pkg-number-of-slots pkg))
    (let ((x (pkg-slot-code pkg i)))
      (when (numberp x)
        (format t "~%~4d ~8x  ~s" i x (pkg-slot-symbol pkg i))))))

(defun find-ifs (pkg)
  (dotimes (i (pkg-number-of-slots pkg))
    (when (and (numberp (pkg-slot-code pkg i))
               (equal "IF" (symbol-name (pkg-slot-symbol pkg i))))
        (format t "~%~4d ~s" i (pkg-slot-symbol pkg i)))))

(defun intern-mush (sym &optional pkg &aux hash str)
  "Like INTERN but returns NIL for all three values if no suitable symbol found.
Does not ever put a new symbol into the package."
  (let ((default-package (and (boundp '*package*) *package*)))
    (cond ((null pkg) (setq pkg default-package))
          ((not (package-p pkg)) (setq pkg (pkg-find-package pkg nil )))))
  (if (stringp sym) (setq str sym)
    (if (symbolp sym) (setq str (symbol-name sym))
      (setq str (string sym))))
  (setq hash (pkg-string-hash-code str))
    (block intern
      ;; Search this package.
      (let ((len (pkg-number-of-slots pkg))
            x y)
        (do ((i (mod hash len)))
            ((null (setq x (pkg-slot-code pkg i)))
             nil)
          (print :SEARCHING) (prin1 i) (prin1 :x) (prin1 x) (prin1 (pkg-slot-symbol pkg i))
          (and (pkg-code-valid-p x)
               (= hash (pkg-code-hash-code x))
               (equal str (symbol-name (setq y (pkg-slot-symbol pkg i))))
               (return-from intern
                 (values y
                         (if (pkg-code-external-p x)
                             :external :internal)
                         pkg)))
          (if (= (incf i) len) (setq i 0))))
      ;; Search USEd packages.
      (dolist (pkg (pkg-use-list pkg))
        (let ((len (pkg-number-of-slots pkg))
              x y)
          (do ((i (rem hash len)))
              ((null (setq x (pkg-slot-code pkg i)))
               nil)
            (and (pkg-code-valid-p x)
                 (= hash (pkg-code-hash-code x))
                 (equal str (symbol-name (setq y (pkg-slot-symbol pkg i))))
                 (if (pkg-code-external-p x)
                     (return-from intern
                       (values y
                               :inherited
                               pkg))
                   (return)))                   ;Not inheritable from this package.
            (if (= (incf i) len) (setq i 0)))))))



(defun find-duplicate-symbols (sym-name pkg-name)
  (let ((pkg (find-package pkg-name))
        ans sym)
    (dotimes (i (pkg-number-of-slots pkg))
      (when (and (numberp (pkg-slot-code pkg i))
                 (string= (symbol-name (setq sym (pkg-slot-symbol pkg i))) sym-name))
        (push (list i (symbol-name sym) (pkg-name (symbol-package sym))) ans)))
    (li:break ans)))
