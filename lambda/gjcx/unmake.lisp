;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:10 -*-


;;; to remove a system.
;;; umake it.
;;; then if it has a package that it alone deals with use KILL-PACKAGE on that package.

(defun unmake-system (name)
  (let ((sys (si:find-system-named name nil t)))
    (let ((files (reverse (system-files-loaded name))))
      (format t "~&~D files to unload~%" (length files))
      (mapcar #'unload-file files)
      (setq *SYSTEMS-LIST* (delq sys *SYSTEMS-LIST*))
      (let ((psys (ASS #'STRING-EQUAL (SYSTEM-NAME SYS) PATCH-SYSTEMS-LIST)))
        (when psys
          (setq PATCH-SYSTEMS-LIST (delq psys PATCH-SYSTEMS-LIST))
          (setq frozen-PATCH-SYSTEMS-LIST (delq psys frozen-PATCH-SYSTEMS-LIST))))
      sys)))




(defun system-files-loaded (name)
  (apply #'append
         (mapcar #'transformation-loaded-files
                 (make-system name :print-only :silent :reload :no-reload-system-declaration))))


(defun transformation-loaded-files (z)
  (let ((type (file-transformation-transformation-type z)))
    (funcall (or (get (transformation-type-name type) 'transformation-loaded-files)
                 #'(lambda (type system args)
                     system args
                     (format t "~&Warning: unhandled transformation type ~S~%"
                             (transformation-type-name type))
                     ()))
             type
             (file-transformation-system z)
             (file-transformation-args z))))


(defun (:fasload transformation-loaded-files) (type system args)
  type system
  args)

(defun (increment-loaded-version transformation-loaded-files) (type system args)
  type args
  (multiple-value-bind (major minor)
        (get-system-version system)
    (do ((j minor (1- j))
         (l nil (cons (patch-system-pathname (system-name system) :patch-file major j :qfasl) l)))
        ((zerop j) l))))


(defvar *unload-doit* nil)

(defun unload-file (name)
  (let ((pathname (send (fs:parse-pathname name) :generic-pathname)))
    (format t "~&Unloading ~A~%" pathname)
    (dolist (dp (get pathname :definitions))
      (format t " ~d definition~:*~p in package ~A~%"
              (length (cdr dp))
              (car dp))
      (dolist (d (cdr dp))
        (unload-definition (cdr d) (car d))))
    (if *unload-doit*
        (setf (get pathname :definitions) nil))
    (let ((forms (get pathname :random-forms)))
      (when forms
        (format t " ~d random form~:*~p~%" (length forms))
        (dolist (form forms)
          (unload-form form))))
    (if *unload-doit*
        (setf (get pathname :random-forms) nil))))

(defun unload-definition (type def)
  (let ((handler (get type 'unload-definition)))
    (cond (handler
           (and *unload-doit* (funcall handler def)))
          ('else
           (format t "  Unhandled definition type ~S: ~S~%" type def)))))


(defun (defun unload-definition) (def)
  (when (fdefinedp def)
    ;; it is possible for a patch file to define something
    ;; this works for functions, properties, methods.
    (fundefine def)
    (or (and (consp def) (eq :property (car def)))
        (remove-source-file-name def 'defun))
    ))


(defun (defvar unload-definition) (def)
  (makunbound def)
  (remprop def 'special)
  (remove-documentation def 'VARIABLE)
  (remove-source-file-name def 'defvar))


(defun (defstruct unload-definition) (def)
  (remprop def 'defstruct-description)
  (remove-source-file-name def 'defstruct))


(defun (defflavor unload-definition) (def)
  (catch-error (undefflavor def))
  (remprop def 'si:flavor)
  (remove-source-file-name def 'defflavor))


(defun (defsignal unload-definition) (def)
  (remove-source-file-name def 'defsignal)
  (remove-documentation def 'signal)
  (remprop def 'make-condition-function))

(defun (defresource unload-definition) (def)
  (remove-source-file-name def 'DEFRESOURCE)
  (remove-documentation def 'resource)
  (when (GET def 'DEFRESOURCE)
    (clear-resource def)
    (remprop def 'defresource))
  (setq *all-resources* (delq def *all-resources*)))

(defun unload-form (f &aux temp)
  (cond ((atom f))
        ((setq temp (find-unload-form f))
         (and *unload-doit* (funcall temp f)))
        ('else
         (format t "  Unhandled form: ~S~%" f))))


(defvar *unload-form-handlers*
        '(((putprop (? constantp) * (? constantp))
           unload-putprop)
          ((defprop * * *)
           unload-defprop)
          ((deff-macro * *) unload-defmacro)
          ((setq patch-source-file-namestring *)
           unload-ignored)
          ((setq * *)
           unload-setq)
          ((fasl-record-file-macros-expanded *)
           unload-ignored)
          ((set-documentation (? constantp) (? constantp) *) unload-documentation)
          ((add-initialization (? constantp) *) unload-initialization)
          ((add-initialization (? constantp) * (? constantp)) unload-initialization)
          ((add-initialization (? constantp) * (? constantp) (? constantp)) unload-initialization)
          ((TV:ADD-SYSTEM-KEY (? constantp) . *) unload-system-key)
          ))

(defun unload-putprop (form)
  (remprop (eval (nth 1 form))
           (eval (nth 3 form))))

(defun unload-defprop (form)
  (remprop (nth 1 form)
           (nth 3 form)))

(defun unload-ignored (form)
  form)


(defun unload-defmacro (form)
  (let ((f (cadr form)))
    (remove-source-file-name f 'defun)
    (fmakunbound f)))

(defun unload-setq (form)
  (makunbound (cadr form)))

(defun find-unload-form (f)
  (do ((l *unload-form-handlers* (cdr l)))
      ((null l) nil)
    (when (form-match (caar l) f)
      (return (cadar l)))))


(defun form-match (pattern form)
  (cond ((atom pattern)
         (or (eq pattern '*)
             (eq pattern form)))
        ((eq (car pattern) '?)
         (funcall (cadr pattern) form))
        ((atom form)
         nil)
        ('else
         (do ((l1 pattern (cdr l1))
              (l2 form (cdr l2)))
             ((atom l1)
              (form-match l1 l2))
           (if (atom l2) (return nil))
           (or (form-match (car l1) (car l2))
               (return nil))))))




(defun remove-documentation (spec doc-type)
  (LET* ((S (SYMBOL-NAME DOC-TYPE))
         (l (function-spec-GET spec 'DOCUMENTATION-PROPERTY))
         (C (ASSOC-EQUAL S l)))
    (when c
      (setq l (delq c l))
      (cond (l
             (setf (function-spec-get spec 'DOCUMENTATION-PROPERTY) l))
            ('else
             (function-spec-remprop spec 'DOCUMENTATION-PROPERTY))))))

(defun remove-source-file-name (spec type)
  (LET* ((S (SYMBOL-NAME TYPE))
         (l (function-spec-GET spec ':SOURCE-FILE-NAME)))
    (cond ((not l))
          ((atom l)
           (function-spec-remprop spec ':SOURCE-FILE-NAME))
          ('else
           (let ((C (ASSOC-EQUAL S l)))
             (when c
               (setq l (delq c l))
               (cond (l
                      (setf (function-spec-get spec ':SOURCE-FILE-NAME) l))
                     ('else
                      (function-spec-remprop spec ':SOURCE-FILE-NAME)))))))))



(defun unload-initialization (form)
  ;;(add-initialization name form keywords list-name)
  (format t "~&   Initialization: ~S~%" form)
  (ecase (length form)
    (3
     (delete-initialization (eval (nth 1 form))))
    (4
     (delete-initialization (eval (nth 1 form)) (eval (nth 3 form))))
    (5
     (delete-initialization (eval (nth 1 form)) (eval (nth 3 form)) (eval (nth 4 form))))))


(defun unload-system-key (form)
  (let ((char (eval (nth 1 form))))
    (SETQ *SYSTEM-KEYS*
          (DELETE-IF #'(LAMBDA (ELT) (CHAR-EQUAL (CAR ELT) CHAR))
                   *SYSTEM-KEYS*))))
