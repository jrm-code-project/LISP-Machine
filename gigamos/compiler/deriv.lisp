;;; -*- Mode:LISP; Package:user; Readtable:CL; Base:10. -*-


;;needed mapcar, mapc, & funcall

;;;;;; DERIV ;;;;;;;;

(defun deriv-aux (deriv-a)
  (list '/ (deriv deriv-a) deriv-a))

(defun deriv (deriv-a)
  (cond
    ((atom deriv-a)
     (cond ((eq deriv-a 'x) 1) (t 0)))
    ((eq (car deriv-a) '+)
     (cons '+ (mapcar #'deriv (cdr deriv-a))))
    ((eq (car deriv-a) '-)
     (cons '- (mapcar #'deriv
                      (cdr deriv-a))))
    ((eq (car deriv-a) '*)
     (list '*
           deriv-a
           (cons '+ (mapcar #'deriv-aux (cdr deriv-a)))))
    ((eq (car deriv-a) '/)
     (list '-
           (list '/
                 (deriv (cadr deriv-a))
                 (caddr deriv-a))
           (list '/
                 (cadr deriv-a)
                 (list '*
                       (caddr deriv-a)
                       (caddr deriv-a)
                       (deriv (caddr deriv-a))))))
     (t 'error)))

(defun run-deriv ()
 (declare (fixnum i))
 (do ((i 0 (1+ i)))
     ((= i 1000.))
   (deriv '(+ (* 3 x x) (* a x x) (* b x) 5))
   (deriv '(+ (* 3 x x) (* a x x) (* b x) 5))
   (deriv '(+ (* 3 x x) (* a x x) (* b x) 5))
   (deriv '(+ (* 3 x x) (* a x x) (* b x) 5))
   (deriv '(+ (* 3 x x) (* a x x) (* b x) 5))))

;;;;;; DDERIV ;;;;;;;;


(DEFUN DER1 (DERIV-A) (LIST '/ (DDERIV DERIV-A) DERIV-A))

(DEFUN +DDERIV (DERIV-A)
       (CONS '+ (MAPCAR #'DDERIV DERIV-A)))

(DEFUN -DDERIV (DERIV-A)
       (CONS '- (MAPCAR #'DDERIV
                                 DERIV-A)))

(DEFUN *DDERIV (DERIV-A)
        (LIST '* (CONS '* DERIV-A)
                (CONS '+ (MAPCAR #'DER1 DERIV-A))))

(DEFUN /DDERIV (DERIV-A)
       (LIST '-
             (LIST '/
                   (DDERIV (CAR DERIV-A))
                   (CADR DERIV-A))
             (LIST '/
                   (CAR DERIV-A)
                   (LIST '*
                         (CADR DERIV-A)
                         (CADR DERIV-A)
                         (DDERIV (CADR DERIV-A))))))

 (DEFUN DDERIV (DERIV-A)
        (COND
         ((ATOM DERIV-A)
          (COND ((EQ DERIV-A 'X) 1) (T 0)))
         (T (LET ((DDERIV (GET (CAR DERIV-A) 'DDERIV)))
                 (COND (DDERIV (FUNCALL DDERIV (CDR DERIV-A)))
                       (T 'ERROR))))))

(DEFUN SETUP-DDERIV ()
  (MAPC
    #'(LAMBDA (OP FUN)
        (SETF (GET OP 'DDERIV)
              (SYMBOL-FUNCTION FUN)))
    '(+       -       *       /)
    '(+DDERIV -DDERIV *DDERIV /DDERIV)))


(DEFUN RUN-DDERIV ()
 (DO ((I 0 (1+ I)))
     ((= I 1000.))
     (DDERIV '(+ (* 3 X X) (* A X X) (* B X) 5))
     (DDERIV '(+ (* 3 X X) (* A X X) (* B X) 5))
     (DDERIV '(+ (* 3 X X) (* A X X) (* B X) 5))
     (DDERIV '(+ (* 3 X X) (* A X X) (* B X) 5))
     (DDERIV '(+ (* 3 X X) (* A X X) (* B X) 5))))




;;;;;;;;;; BENCHMARK the machine ;;;;;;;;;;;;;;

;;;;THIS MUST BE COMPILED WITH HARDEBECK COMPILER!!!!!
(defun test-deriv ()
  (hw:write-microsecond-clock (hw:unboxed-constant 0))
  (li:error "DERIV complete." (run-deriv) (hw:read-microsecond-clock))
  (loop))

;;;;THIS MUST BE COMPILED WITH HARDEBECK COMPILER!!!!!
(defun test-dderiv ()
  (setup-dderiv)
  (hw:write-microsecond-clock (hw:unboxed-constant 0))
  (li:error "DDERIV complete." (run-dderiv) (hw:read-microsecond-clock))
  (loop))
