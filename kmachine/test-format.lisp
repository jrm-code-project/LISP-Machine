;-*- Mode:LISP; Package:LISP-INTERNALS; Base:10; Lowercase:T; Readtable:CL -*-

(defun clear-ap (x)
  (setq x (setq *all-packages* ()))
  (loop))



(defun test-format (x)
  (boot-stack-groups)
  (setq x (format nil "foo ~D ~S" 7 '(x 99 3)))
  (loop))


(defun test-format (x)
  (setq x (format nil "foo ~D" 7))
  (loop))

(defun test-format (x)
  (setq x (format nil "foo"))
  (loop))

(defun streamp (x)
  ())

(defun vector-push-extend (thing vector)
  (vector-push thing vector))


(defun test-getf (x y)
  (setq x nil)
  (setf (getf x 'foo) 1)

  (setf (getf x 'bar) 2)
  (setq y (getf x 'foo))
  (loop))

(defun test-ma (x)
  (boot-stack-groups)
  (setq x (array:make-array 128. :element-type 'string-char :adjustable T :fill-pointer T))
  (loop))

(defun ts ()
  (setf (getf (car (foo)) 'bar) 4))
