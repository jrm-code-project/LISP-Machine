;;; -*- Mode:LISP; Package:NC; Base:10; Readtable:CL -*-


(defvar *instructions* '())

(defconstant *tag-package* (or (find-package 'tag)
                               (make-package 'tag)))

(defun get-tag (node)
  (let ((var (cond ((lambda-node? node)
                    (lambda-self-var node))
                   ((reference-node? node)
                    (reference-variable node))
                   (t (bug "Bad tag")))))
    (intern (variable-unique-name var) *tag-package*)))

(defun emit-template (n1 n2)
  (let ((tag (get-tag n1)))
    (debug :emit (format t "~&~a" tag))
    (push tag *instructions*)))

(defun emit-tag (tag)
  (setq tag (get-tag tag))
  (debug :emit (format t "~&~a" tag))
  (push tag *instructions*))

(defun emit-comment (string)
  (push `(COMMENT ,(format nil "~&;~a" string)) *instructions*))

(defun emit-jump (node)
  (emit 'K:JUMP (get-tag node)))


(defconstant *inverse-conditions*
             `((K:BR-EQUAL . K:BR-NOT-EQUAL)
               (K:BR-NOT-EQUAL . K:BR-EQUAL)))

(defun inverse-cond (cond)
  (cdr (assoc cond *inverse-conditions*)))

(defun emit-inverse-test (cond)
  (emit 'K:TEST (inverse-cond cond)))

(defun emit-test (cond)
  (emit 'K:TEST cond))

(defun emit-branch (node)
  (emit 'K:BRANCH (get-tag node)))

(defun emit-alu (op dest left right)
  (emit 'K:ALU op dest left right))

;;; should be K:
(defconstant active-regs '(K:A0 K:A1 K:A2 K:A3 K:A4 K:A5 K:A6 K:A7 K:A8
                           K:A9 K:A10 K:A11 K:A12 K:A13 K:A14 K:A15))
(defconstant open-regs '(K:O0 K:O1 K:O2 K:O3 K:O4 K:O5 K:O6 K:O7 K:O8
                         K:O9 K:O10 K:O11 K:O12 K:O13 K:O14 K:O15))
(defconstant return-regs '(K:R0 K:R1 K:R2 K:R3 K:R4 K:R5 K:R6 K:R7 K:R8
                           K:R9 K:R10 K:R11 K:R12 K:R13 K:R14 K:R15))
(defconstant *all-regs* (append active-regs open-regs return-regs '(K:RETURN K:P)))

(defun hack-operands (ops)
  (let ((new-ops '()))
    (dolist (op ops)
      (push
        (cond ((integerp op)
               (nth op *all-regs*))
              ((and (consp op) (integerp (car op)) (integerp (cdr op)))
               (cons (nth (car op) *all-regs*) (cdr op)))
              ((literal-node? op)
               (list 'quote (leaf-value op)))
              ((reference-node? op)
               (variable-name (leaf-value op)))
              ((lambda-node? op)
               (variable-unique-name (lambda-self-var op)))
              (t op))
        new-ops))
    (nreverse new-ops)))

;(defun emit (op &rest operands)
;  (format t "~&  ~a" (cons op (hack-operands operands))))


(defun ncp (form)
  (setq *instructions* '())
  (nc form)
  (let ((insts (post-process *instructions*)))
    (debug :post (print-instructions insts))
    insts))

(defun comp (form)
  (case (car form)
    (DEFUN `(sim:defkfun ,(cadr form) ,(caddr form)
              . ,(cddr (ncp form))))
    (t (error "KCOMP doesn't grok ~a" (car form)))))

(defun kcomp (form)
  (eval (comp form))
  (compile (cadr form)))

(defun emit (op &rest operands)
  (let ((i (cons op (hack-operands operands))))
    (debug :emit (format t "~&  ~a" i))
    (push  i *instructions*)))

(defun i-type (i)
  (if (consp i)
      (car i)
    'LABEL))

(defun post-process (instructions)
  (do ((instructions (cdr instructions) (cdr instructions))
       (i (car instructions) (car instructions))
       (new-instructions '()))
      ((null instructions) (push i new-instructions))
    (let* ((i-type (i-type i))
           (prev-i (car instructions))
           (prev-i-type (i-type prev-i)))
      (push (case i-type
              (K:KCALL (case prev-i-type
                      (K:MOVE (pop instructions)
                            (case (i-type (car instructions))
                              (K:KOPEN
                               (pop instructions)
                               (case (i-type (car instructions))
;                                (K:KOPEN
;                                 (pop instructions)
;                                 `(K:OPEN-CALL ,(second i) ,(third i) ,(list 'K:NEW-OPEN (fourth i)) ,(cdr prev-i)
;                                               . ,(cddddr i)))
                                 (t `(K:OPEN-CALL ,(second i) ,(third i) ,(fourth i) ,(cdr prev-i) . ,(cddddr i)))))
                              (t
                               `(K:KCALL ,(second i) ,(third i) ,(fourth i) ,(cdr prev-i) . ,(cddddr i)))))
                      (K:KOPEN (pop instructions)
                            `(K:OPEN-CALL ,(second i) ,(third i) ,(fourth i) () . ,(cddddr i)))
                      (t `(K:KCALL ,(second i) ,(third i) ,(fourth i) () . ,(cddddr i)))))
              ;; gack
              (K:TAIL-CALL (case prev-i-type
                      (K:MOVE (pop instructions)
                            (case (i-type (car instructions))
                              (K:TAIL-OPEN
                               (pop instructions)
                               (case (i-type (car instructions))
;                                (K:KOPEN
;                                 (pop instructions)
;                                 `(K:OPEN-TAIL-CALL ,prev-i (K:NEW-OPEN ,(cadr i)) . ,(cddr i)))
                                 (t `(K:OPEN-TAIL-CALL ,(second i) ,(third i) ,(cdr prev-i) . ,(cddddr i)))))
                              (t
                               `(K:TAIL-CALL ,(second i) ,(third i) ,(cdr prev-i) . ,(cddddr i)))))
                      (K:TAIL-OPEN (pop instructions)
                            `(K:OPEN-TAIL-CALL () ,(cdr i)))
                      (t `(K:TAIL-CALL . ,(cdr i)))))
              (K:RETURN (case prev-i-type
                        (K:MOVE (pop instructions)
                              (append prev-i (list 'K:CH-RETURN)))))
              ((K:MOVE K:ALU)
               (case prev-i-type
                 (K:KOPEN (pop instructions)
                       (append i (list 'K:CH-OPEN)))
                 (K:TAIL-OPEN (pop instructions)
                       (append i (list 'K:CH-TAIL-OPEN)))
                 (t i)))
              (K:KOPEN (case prev-i-type
;                     (K:KOPEN (pop instructions)
;                           ;; find the call for the second open
;                           (let ((call (car (member-if #'(lambda (i-type) (or (eq i-type 'K:KCALL)
;                                                                              (eq i-type 'K:OPEN-CALL)))
;                                                       new-instructions :key #'car))))
;                             ;; munge it to do a new-open
;                             (rplaca (cddr call) (list 'K:NEW-OPEN (caddr call)))
;                             i))
;                     (K:TAIL-OPEN (pop instructions)
;                           ;; find the call for the second open
;                           (let ((call (car (member-if #'(lambda (i-type) (or (eq i-type 'K:KCALL)
;                                                                              (eq i-type 'K:OPEN-CALL)))
;                                                       new-instructions :key #'car))))
;                             ;; munge it to do a new-open
;                             (rplaca (cddr call) (list 'K:NEW-TAIL-OPEN (caddr call)))
;                             i))
                      (t i)))
              (t i))
            new-instructions))))


(defun print-instructions (insts)
  (dolist (i insts)
    (cond ((consp i)
           (format t "~&   ~a" i))
          (t (format t "~&~a" i)))))
