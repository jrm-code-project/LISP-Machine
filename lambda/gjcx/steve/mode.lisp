; -*- Mode:LISP; Package:STEVE; Readtable:CL; Base:10 -*-

;Copyright (c) May 1983 by Christopher Eliot
; and Massachusetts Institute of Technology.
;Permission to copy all or part of this material is granted, provided
; that the copies are not made or distributed for resale, the MIT
; copyright notice and reference to the source file and the software
; distribution version appear, and that notice is given that copying
; is by permission of Massachusetts Institute of Technology.
;
;Major and minor mode functions for steve.
;

;;; A major mode must have a variable *MODE-bindings* containing the
;;; *editor-bindings* for that mode. Also *MODE-syntax* is local-bound
;;; to the variable syntax-table.

(defstruct (mode (:conc-name mode-))
  name
  superior
  ;:inferiors ;Would this be useful?
  keys
  syntax
  variables)

(defun (:mode fs:file-attribute-bindings) (pathname kwd mode)
  ;; Note: mode can be a symbol or string.
  ;; (setq mode (lookup-mode mode))
  (loop for (var value . crap) on (major-mode-variables mode) by #'cddr
        collect var into vars
        collect value into values
        finally (return (values vars values () ()))))

(defun mode-lookup-symbol (name)
  (intern (string-append (string-upcase name) "-MODE") 'steve))

(defun lookup-mode (name)
  (let ((sym (mode-lookup-symbol name)))
    (and (boundp sym) (symeval sym))))

;Notice that this does not allow for upward propegation of changes.
;Actually that may be reasonable, But the issue should be kept alive
;until decided.
(defsubst derive-syntax (table)
  (copy-syntax-table table))

(defmacro define-mode (name &optional superior keys syntax &restl variables)
  (when (null superior)
    (when (null keys) (setq keys '(make-empty-key-binding-table)))
    (when (null syntax) (setq syntax '(make-empty-syntax-table))))
  `(progn (defunmetax ,(intern (string-append name "-MODE") 'steve) ()
           (major-mode ',name))
          (defparameter
           ,(mode-lookup-symbol name)
           (make-mode
            :name ',name
            :superior ',superior
            ;; :inferiors nil
            :keys ,(if (null keys)
                       `(derive-bindings (mode-keys (lookup-mode ',superior)))
                       `,keys)
            :syntax ,(if (null syntax)
                         `(derive-syntax (mode-syntax
                                          (lookup-mode ',superior)))
                         `,syntax)
            :variables ',(copy-list variables)))))

(defun init-buffer-locals (&optional (buffer *editor-buffer*))
  ;;This creates an environment without a major mode.
  (let ((filename (probe-file (buffer-file-name buffer)))
        (plist))
    (cond ((not (null filename))
           (setq plist (send filename :file-plist))
           (remprop plist :mode)                ;This differs from below.
           (setf (buffer-environment buffer)
                 (calculate-buffer-environment (cdr plist) filename)))
          (t (setf (buffer-environment buffer) (null-load-environment))))
    (setq *context-change-flag* t)))

;;This is really about the same as init-buffer-locals.
;;The only difference is that this sets the major mode too.
(defun initialize-buffer-environment (buffer)
  (let ((filename (probe-file (buffer-file-name buffer)))
        (plist))
    (cond ((not (null filename))
           (setq plist (send filename :file-plist))
           (when (null (get plist :mode))
             (putprop plist (default-major-mode (send filename :type)) :mode))
           (setf (buffer-environment buffer)
                 (calculate-buffer-environment (cdr plist) filename)))
          (t (setf (buffer-environment buffer) (null-load-environment))
             (major-mode (default-major-mode (send (buffer-file-name buffer)
                                                   :type)))))
    (setq *context-change-flag* t)))

(defvar default-major-mode-alist nil)

(defun default-major-mode (file-type)
  (loop for (type mode) in default-major-mode-alist
        if (string-equal type file-type) return mode
        finally (return "Fundamental")))

(defun major-mode-variables (mode-name &aux (mode (lookup-mode mode-name)))
  (cond ((null mode) (ed-warning "Invalid Mode ~a" mode-name))
        (t (nconc (major-mode-superior-variables mode)
                  (list 'syntax-table (or (mode-syntax mode) syntax-table)
                        '*editor-bindings* (mode-keys mode)
                        '*major-mode* mode-name)))))

(defun major-mode-superior-variables (mode)
  (cond ((null mode) nil)
        (t (let ((vars (cons '*alist* (mode-variables mode)))
                 (shadowable-vars
                  (major-mode-superior-variables
                   (lookup-mode (mode-superior mode)))))
             (nconc (loop for (var value . shit) on shadowable-vars by #'cddr
                          if (not (get vars var))
                          nconc (list var value))
                    (copy-list (cdr vars)))))))

(defun major-mode (&optional (mode-name (mx-prompter #'read-symbol "Mode: "))
                    (buffer *editor-buffer*))
  (init-buffer-locals buffer)
  (loop for (var value . creap) on (major-mode-variables mode-name) by #'cddr
        do (bind-in-buffer buffer var value)))

(defun set-mode-variable (mode variable value)
  (setf (mode-variables mode)
        (list* variable value
               (loop for (var value . shit) on (mode-variables mode) by #'cddr
                     unless (eq var variable)
                     nconc (list var value))))
  (setq *context-change-flag* t)
  variable)
