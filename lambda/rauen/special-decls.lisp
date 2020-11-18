


;; Binding
;;   Sx  n means that the dynamic (special) value of x is bound to n
;;   x  n  means that x is lexically bound to n
;;
;; Reference
;;   x  is a reference to the lexical value of x
;;   Sx is a reference to the dynamic (special) value of x

(let ((x 3))                           ;x  3
  (let ((x 4) (y x))                   ;(x  4, y  x = 3)
    ..))                               ;x = 4, y = 3

(let ((x 3))                           ;x  3
  (let* ((x 4) (y x))                  ;x  4, y  x = 4
    ..))                               ;x = 4, y = 4

(let ((x 3))                           ;x  3
  (let* ((y x) (x 4))                  ;y  x = 3, x  4
    ..))                               ;x = 4, y = 3

(let ((x 3))                           ;Sx  3
  (declare (special x))                ;-
  (let ((x 4) (y x))                   ;(x  4, y  Sx = 3)
    ..))                               ;x = 4, y = 3, [Sx = 3]

(let ((x 3))                           ;Sx  3
  (declare (special x))                ;-
  (let* ((x 4) (y x))                  ;x  4, y  x = 4
    ..))                               ;x = 4, y = 4, [Sx = 3]

(let ((x 3))                           ;Sx  3
  (declare (special x))                ;-
  (let* ((y x) (x 4))                  ;y  Sx = 3, x  4
    ..))                               ;x = 4, y = 3, [Sx = 3]

(let ((x 3))                           ;x  3
  (let ((x 4) (y x))                   ;(Sx  4, y  x = 3)
    (declare (special x))              ;-
    ..))                               ;Sx = 4, y = 3, [x = 3]

(let ((x 3))                           ;x  3
  (let* ((x 4) (y x))                  ;Sx  4, y  Sx = 4
    (declare (special x))              ;-
    ..))                               ;Sx = 4, y = 4, [x = 3]

(let ((x 3))                           ;x  3
  (let* ((y x) (x 4))                  ;y  x = 3, Sx  4
    (declare (special x))              ;-
    ..))                               ;Sx = 4, y = 3, [x = 3]

(let ((x 3))                           ;Sx  3
  (declare (special x))                ;-
  (let ((x 4) (y x))                   ;(Sx  4, y  Sx = 3)
    (declare (special x))              ;-
    ..))                               ;Sx = 4, y = 3

(let ((x 3))                           ;Sx  3
  (declare (special x))                ;-
  (let* ((x 4) (y x))                  ;Sx 
    (declare (special x))
    ..))

(let ((x 3))
  (declare (special x))
  (let* ((y x) (x 4))
    (declare (special x))
    ..))


(let ((x 3))                           ;The dynamic value of x is bound to 3
  (declare (special x))                ;This declaration affects the binding of x to 3
                                       ;  and the reference in the first print statement.
  (print x)                            ;3 is printed
  (let ((x 4))                         ;x is lexically bound to 4
    (print x)                          ;4 is printed
    (let ()                            ;-
      (declare (special x))            ;This declaration affects the reference in the
                                       ;  third print statement.
      (print x))))                     ;3 is printed

(progn                                 ;Be real sure that w doesn't have a dynamic value
  (makunbound 'w))                     ;before proceeding

(let ((w 5))                           ;w is lexically bound to 3
  (let ()                              ;-
    (declare (special w))              ;Further references to w are to its dynamic value
    w)))                               ;dynamic value is unbound, should give an error.

(let ((w 3))                           ;The dynamic value of w is bound to 3
  (declare (special w))                ;This declaration affects the above binding
  (let ()                              ;-
    w))                                ;= 3

(progv '(a) '(4)                       ;The dynamic value of a is bound to 4
  a)                                   ;free variable reference

(let ((a 3))                           ;a is lexically bound to 3
  (progv '(a) '(4)                     ;and dynamically bound to 4
    a))                                ;lexical value = 3

(let ((a 3))                           ;a is dynamically bound to 3
  (declare (special a))                ;-
  (progv '(a) '(4)                     ;a is dynamically bound to 4
    a))                                ;dynamic value = 4

(let ((a 3))                           ;a is lexically bound to 3
  (let ()                              ;-
    (declare (special a))              ;Further references to a are to its dynamic value
    (progv '(a) '(4)                   ;a is dynamically bound to 4
      a)))                             ;dynamic value = 4
