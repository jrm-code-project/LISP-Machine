;;; -*- Mode:LISP; Package:PASCAL; Base:10; Fonts:(CPTFONTB); Readtable:CL -*-

(defvar *proc-fn-list* nil)

(defun translate-tree (tree)
  (setq *proc-fn-list* nil)
  (translate-node tree))

(defun translate-node (node)
  (let ((translator-fn (get (node-category node) :translator))
        (results       nil))
    (if (null (node-daughters node))
        (or (and translator-fn
                 (funcall translator-fn (node-translation node)))
            (node-translation node))
        (dolist (d (node-daughters node))
          (push (translate-node d) results))
        (or (and translator-fn
                 (funcall translator-fn results))
            (cons (node-category node) results)))))


(defun statement-translator (args)
  (cond ((eq (first args) 'if)     (if (null (nthcdr 4 args))
                                       `(if ,(second args)
                                            ,(fourth args))
                                       `(if ,(second args)
                                            ,(fourth args)
                                            ,(if (eq (fifth args) '\;)
                                                 (seventh args)
                                                 (sixth args)))))
        ((eq (first args) 'begin)  `(progn ,@(second args)))
        ((eq (first args) 'case)   `(case ,(second args) ,@(fourth args)))
        ((eq (first args) 'goto)   `(go ,(second args)))
        ((eq (first args) 'while)  `(do-while ,(second args) ,(fourth args)))
        ((eq (first args) 'repeat) `(do-until ,(fourth args) ,@(second args)))
        ((eq (first args) 'for)    `(do-to (,(second args) ,(fourth args) ,(sixth args)) ,(eighth args)))
        ((eq (second args) '\:=)   `(setf ,(first args) ,(third args)))
        ((= (length args) 1)       args)
        ((eq (second args) '\()    (cons (first args) (third args)))
        (t nil)))
(putprop '<statement> 'statement-translator :translator)


(defun begin-translator (args)
  `(progn ,@(second args)))
(putprop '<block-begin> 'begin-translator :translator)


(putprop '<logical-op> 'car :translator)
(putprop '<plus-minus-or> 'car :translator)
(putprop '<plus-minus> 'car :translator)
(putprop '<star-slash-div-mod-and> 'car :translator)
(putprop '<unsigned-number> 'car :translator)
(putprop '<unsigned-integer> 'car :translator)
(putprop '<unsigned-constant> 'car :translator)



(defun case-clause-list-translator (args)
  (do ((arglist args (nthcdr 4 arglist))
       (results nil))
      ((null arglist) (nreverse results))
    (push (list (first arglist) (third arglist)) results)))
(putprop '<case-clause-list> 'case-clause-list-translator :translator)

(defun variable-translator (args)
  (cond ((= (length args) 1) (car args))
        (t (funcall (second args) (first args)))))
(putprop '<variable> 'variable-translator :translator)

(defun constant-list-translator (args)
  (let ((results (list (car args))))
    (do ((arglist (cddr args) (cddr arglist)))
        ((null arglist) (nreverse results))
      (push (car arglist) results))))
(putprop '<constant-list> 'constant-list-translator :translator)

(defun constant-translator (args)
  (cond ((= (length args) 1) (car args))
        (t args)))
(putprop '<constant> 'constant-translator :translator)

(defun factor-translator (args)
  (cond ((= (length args) 1) (car args))
        ((eq (car args) '\() (second args))
        ((eq (second args) '\() (cons (first args) (third args)))
        ((eq (first args) '[) (list 'quote (second args)))
        (t args)))
(putprop '<factor> 'factor-translator :translator)

(defun factor-sequence-translator (args)
  (polishize args))
(putprop '<factor-sequence> 'factor-sequence-translator :translator)

(defun factor-expression-list-translator (args)
  (do ((arglist args (cddr arglist))
       (results nil))
      ((null arglist) (nreverse results))
    (cond ((eq (second arglist) '\.\.)
           (push (list 'DOT-DOT (first arglist) (third arglist)) results)
           (setq arglist (cddr arglist)))
          (t (push (first arglist) results)))))
(putprop '<factor-expression-list> 'factor-expression-list-translator :translator)

(defun term-translator (args)
  (cond ((= (length args) 1) (car args))
        (t args)))
(putprop '<term> 'term-translator :translator)

(defun term-sequence-translator (args)
  (let ((sign nil))
    (if (or (eq (car args) '+)
            (eq (car args) '-))
        (setq sign (car args)
              args (cdr args)))
    (if (eq sign '-)
        (list '- (polishize args))
        (polishize args))))

(defun polishize (args)
  (cond ((= (length args) 1) (car args))
        (t (list (second args)
                 (first args)
                 (if (nthcdr 3 args)
                     (polishize (cddr args))
                     (third args))))))

(putprop '<term-sequence> 'term-sequence-translator :translator)

(defun simple-expression-translator (args)
  (cond ((= (length args) 1) (car args))
        (t args)))
(putprop '<simple-expression> 'simple-expression-translator :translator)

(defun expression-translator (args)
  (cond ((= (length args) 1) (car args))
        (t (list (second args) (first args) (third args)))))
(putprop '<expression> 'expression-translator :translator)

(defun l-statement-translator (args)
  (cond ((= (length args) 1) (car args))
        (t (list (second args) (first args) (third args)))))
(putprop '<l-statement> 'l-statement-translator :translator)





(defun id-list-translator (args)
  (let ((results (list (car args))))
    (do ((arglist (cddr args) (cddr arglist)))
        ((null arglist) (nreverse results))
      (push (car arglist) results))))
(putprop '<id-list> 'id-list-translator :translator)

(defun expression-list-translator (args)
  (let ((results (list (car args))))
    (do ((arglist (cddr args) (cddr arglist)))
        ((null arglist) (nreverse results))
      (push (car arglist) results))))
(putprop '<expression-list> 'expression-list-translator :translator)


(defun statement-list-translator (args)
  (let ((results    (list (car args)))
        (label-flag nil))
    (do ((arglist (cddr args) (cddr arglist)))
        ((null arglist) (if (not label-flag)
                            (nreverse results)
                            `((prog nil ,@(nreverse results)))))
      (cond ((eq (first (car arglist)) '\:)
             (setq label-flag t)
             (push (second (car arglist)) results)
             (push (third  (car arglist)) results))
            (t (push (car arglist) results))))))
(putprop '<statement-list> 'statement-list-translator :translator)



(defun variable-suffix-translator (args)
  (cond ((eq (first args) '[)
         `(lambda (x) `(aref ,x ,',@(second args))))
        ((eq (first args) '\.)
         `(lambda (x) `(,',(second args) ,x)))
        ((eq (first args) '^)
         '(lambda (x) `(%caret ,x)))))

(putprop '<variable-suffix> 'variable-suffix-translator :translator)




(defun idlist-colon-type-list-translator (args)
  (do ((arglist args (nthcdr 4 arglist))
       (results nil))
      ((null arglist) results)
    (setq results (append results (first arglist)))))
(putprop '<idlist-colon-type-list> 'idlist-colon-type-list-translator :translator)




(defun function-translator (args)
  (push `(defun ,(second args) ,(if (= (length args) 9)
                                    (fourth args)
                                    nil)
           (let (,(second args))
             ,(car (last args))
             ,(second args)))
        *proc-fn-list*)
  nil)
(putprop '<function> 'function-translator :translator)


(defun procedure-translator (args)
  (push `(defun ,(second args) ,(if (= (length args) 7)
                              (fourth args)
                              nil)
           ,(car (last args)))
        *proc-fn-list*)
  nil)
(putprop '<procedure> 'procedure-translator :translator)


(defun program-translator (args)
  `(defun ,(second args) ,(if (= (length args) 7)
                              (fourth args)
                              nil)
     ,(or (seventh args) (fourth args))))
(putprop '<program> 'program-translator :translator)


(defun param-list-translator (args)
  (do ((arglist args (cddr arglist))
       (result  nil))
      ((null arglist) result)
    (setq result (append result (first args)))))
(putprop '<param-list> 'param-list-translator :translator)


(defun param-translator (args)
  (if (eq (first args) 'var)
      (second args)
      (first args)))
(putprop '<param> 'param-translator :translator)


(defvar *reserved-identifier-alist*
        '((true  . t)
          (false . nil)
          (read  . pread)
          (write . pwrite)
          (t     . *t*)))

(defun identifier-translator (arg)
  (if (eq arg 'false)
      nil
      (or (cdr (assq arg *reserved-identifier-alist*))
          arg)))
(putprop '<identifier> 'identifier-translator :translator)


(defun block-translator (args)
  (let ((block-vars   (assq '<block-var> args))
        (block-consts (assq '<block-const> args)))
    `(let ,(append (third block-vars) (third block-consts))
       ,(car (last args)))))
(putprop '<block> 'block-translator :translator)




(defun id-equals-const-list-translator (args)
  (do ((arglist args (nthcdr 4 arglist))
       (results nil))
      ((null arglist) (nreverse results))
    (push (list (first arglist) (third arglist)) results)))
(putprop '<id-equals-const-list> 'id-equals-const-list-translator :translator)
