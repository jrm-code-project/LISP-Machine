;;; -*- Mode:LISP; Package:READ; Readtable:CL; Base:10 -*-

(defmacro backquote (argument)


(backquote (foo bar)) --> (quote (foo bar))
(backquote foo) -> (quote foo)
(backquote (unquote foo)) -> foo

(backquote (foo bar)) -> (quote (foo bar))
(backquote ((unquote foo) bar)) -> (foo (quote bar))

`(,foo bar)

;;; (backquote (e1 e2 ... en . (unquote form)))



(defmacro backquote (expression)
  (list
    'QUOTE
    (eval
      (cond ((vectorp expression)
             `(VECTOR ,@(nbutlast
                          (expand-backquoted-list (coerce expression 'list)))))
            ((not (listp expression))
             `(QUOTE ,expression))
            ((eq (car expression) 'UNQUOTE)
             `,(cadr expression))
            ((or (eq (car expression) 'UNQUOTE-SPLICING)
                 (eq (car expression) 'DESTRUCTIVE-UNQUOTE-SPLICING))
             (error "Can't unquote-splicing immediaely after backquote."))
            ((eq (car expression) 'BACKQUOTE)
             `(BACKQUOTE ,(macroexpand expression)))
            (t
             `(LIST* ,@(expand-backquoted-list expression)))))))


;;;(apply #'list* (expand-backquoted-list expression)))))








;`(quote foo)
;(append [quote] [foo])
;(append (list 'quote) (list 'foo))
