;;; -*- Mode:LISP; Package:HACKS; Base:8; Lowercase:YES; readtable: ZL -*-
;;; Doctor program, from MIT-MULTICS:>nbdd>m>bsg>doctor::doctor.lisp
;;; Obarray hacking removed and readtable hacking updated by DLW.
;;; Changed putprops at end of this file not to do (rplacd (gensym) ...)
;;; Flushed linefeed.
;;; Improved readin. (IN PROGRESS)

(comment doctor part one /; the basic functions)

(array spargs t 20)
;(declare (*lexpr ceval))
(declare (special *sentence*  newlinechar flipflop keystack *word*
                  *flag* *letter* terminal *line* s d rules lev docrt))

(defconst blank '| |)
(defconst newlinechar '|
|)
(defconst horizontaltab '|      |)

(putprop blank t 'break)
(putprop newlinechar t 'break)
(putprop horizontaltab t 'break)

(defun doctor nil
 (let ((readtable docrt)
       (package (pkg-find-package "HACKS")))
  (prog (*sentence* keystack)
        (terpri)
        (princ "Tell me about your problem.")
        (terpri)
      a   (setq *sentence* (setq keystack nil))
           (readin)
           (cond ((null *sentence*)(go a)))
           (analyze)
           (terpri)
           (terpri)
           (go a))))

(defun readin nil
 (prog (*word* *letter* *flag* terminal *line*)
       (setq *line* (explodec (string-append (string-upcase (readline)) #\newline)))
    a  (cond ((null (readword)) (go b)))
       (makesentence)
       (setkeystack)
    b  (breakanalyze)
       (cond ((not *flag*) (go a)))
       (setq *sentence* (reverse *sentence*))))

(defun readword nil
 (prog nil
       (setq *word* nil)
  a    (cond ((setq *flag* (get (nextch) (quote break)))
              (return (cond (*word* (setq *word*
                                          (intern (maknam (reverse *word*)))))))))
       (setq *word* (cons *letter* *word*))
       (go a)))

(defun makesentence nil
 (setq *sentence*
       (cons (cond ((setq *flag* (get *word* (quote translation)))
                            *flag*)
                   (*word*))
             *sentence*)))

(defun setkeystack nil
 (cond ((and (setq *flag* (get *word* (quote priority)))
             keystack
             (greaterp *flag*
                       (get (car keystack) (quote priority))))
        (setq keystack (cons *word* keystack)))
       (*flag* (setq keystack (append keystack (list *word*))))))

(defun breakanalyze nil
 (cond ((eq *letter* newlinechar) (setq *flag* t))
       ((and (setq *flag* (get *letter* (quote punctuation)))
             keystack)
        (gobble))
       (*flag* (setq *sentence* (setq *flag* nil)))
       (t (setq terminal nil))))

(defun test (d s)
  (setq lev 0)
  (prog nil
     g    (cond ((null d)
                 (return (not s)))
                ((not (cond ((numberp (car d))
                             (cond ((zerop (car d)) (test5))
                                   ((test3 (car d) nil))))
                            ((test4 (car d)) (test2))))
                 (return nil)))
        (setq d (cdr d))
        (go g)))

(defun advance nil
     (rplaca (cdar rules)
             (cond ((null (cdar(cdar rules))) (cddar rules))
                   ((cdar(cdar rules))))))


(defun reconstruct (r)
     (cond ((null r) nil)
           ((numberp (car r)) (append (spargs (car r))
                                      (reconstruct (cdr r))))
           ((cons (car r) (reconstruct (cdr r))))))

(defun gobble nil
 (prog nil
      a    (nextch)
           (breakanalyze)
           (cond ((not *flag*) (go a)))))

(defun nextch nil
        (setq *letter* (car *line*))
        (setq *line* (cdr *line*))
        *letter*)

(defun test1 (propl x)
   (cond ((null propl) nil)
         ((get x (car propl)) t)
                 ((test1 (cdr propl) x))))

(defun test2 nil
   (stra (list (car s)))
   (setq s (cdr s))
       t)


(defun test3 (x l)
         (cond ((zerop x) (stra (reverse l)))
               (s (test3 (sub1 x)
                         (cons (car s) (prog2 (setq s (cdr s)) l))))))

(defun test4 (d)
     (cond ((null s) nil)
           ((atom d) (eq d (car s)))
           ((car d) (member (car s) d))
           ((test1 (cdr d) (car s)))))

(defun test5 nil
 (prog (l)
       (cond ((null (cdr d)) (stra s)
                             (return (not (setq s nil)))))
  a    (cond ((test4 (cadr d))
               (stra (reverse l))
               (return t))
             ((and (setq l (cons (car s) l)) (setq s (cdr s)))
              (go a)))))



(defun stra (element)
        (store (spargs (setq lev (1+ lev))) element))

(defun analyze nil
       (prog (rules decomp)
           (setq
            keystack
            (nconc
             keystack
             (list
              (get
               'none
               (cond ((zerop (setq flipflop
                               (plus 2. (minus flipflop))))
                    'mem)
                   ('lastresort))))))
        a    (setq rules (get (car keystack) 'rules))
        b    (setq decomp (caar (cond ((atom (car rules))
                                       (cond ((eq (car rules)
                                                  'zap)
                                              (return-from analyze t)))
                                       (setq rules (get (car rules)
                                                        'rules)))
                                      (t rules))))
           (cond
            ((not (test decomp *sentence*)) (setq rules (cdr rules)))
            ((and (not (atom (car (setq rules (car (advance))))))
                (not (memq (caar rules)
                         '(pre eval))))
             (sentprint (reconstruct (car rules)))
        (return (memory)))
            ((not (atom (car rules)))
             (cond ((eq (caar rules) 'pre)
                  (setq *sentence* (reconstruct (cadar rules)))
                  (setq rules (cddar rules)))
                 ((eq (caar rules) 'eval)
                  (cond ((cadar rules)
                         (sentprint (reconstruct (cadar rules)))))
                  (return (eval (cons 'progn
                                  (cddar rules)))))
                 (t (cond ((cadar rules)
                         (sentprint (reconstruct (cadar rules)))))
                    (return (eval (cons 'prog
                                     (cddar rules)))))))
             ((eq (car rules) 'newkey)
              (setq keystack (cdr keystack))
              (go a)))
            (go b)))


(defun memory nil
  (prog (x)
        (cond ((and (setq rules (get (car keystack) (quote memr)))
                    (test (caar rules) *sentence*))
               (rplaca (setq x (cdar (get (get (quote none) (quote mem))
                                          (quote rules))))
                       (append (car x)
                               (list (reconstruct (caar (advance))))))))))

(setq flipflop 0)

(mapc   (quote (lambda (x)
                   (putprop x t (quote break))
                   (putprop x t (quote punctuation))))
        (quote (/. /, /( /) /! ? /: /;)))

(putprop (quote none)
         (let ((sym (gensym)))
           (setplist sym (list
               (quote rules)
               (quote (((0)
                        (nil)
                        (I am not sure I understand you fully)
                        (please go on)
                        (what does that suggest to you)
                        (do you feel strongly about discussing such things))))))
           sym)
         (quote lastresort))

(putprop (quote none)
         (let ((sym (gensym)))
           (setplist sym (list (quote rules) (list (list (list 0)
                                                   (list nil)
                                                   (get (quote none)
                                                        (quote lastresort))))))
           sym)
         (quote mem))

(defun sentprint (ans)
        (cond ((null ans))
              ((atom ans)(princ ans)(princ '/ ))
              (t (sentprint (car ans))(sentprint (cdr ans)))))

(setq docrt (si:copy-readtable si:initial-readtable))
(let ((readtable docrt))
  (setsyntax #/' ':single nil)
  (setsyntax #/; ':single nil)
  (setsyntax #/: ':single nil))
