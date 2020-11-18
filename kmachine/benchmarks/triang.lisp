;;;-*- Mode:LISP; Package:USER; Base:10.; Readtable:ZL -*-
;;; From the "Dick Gabriel" Benchmark Series.
;;; Enhancements (C) Copyright 1983, Lisp Machine, Inc.

;;;BEGIN
;;;TRIANG
(declare (special answer final))

;(eval-when (compile load eval)
;          (setq base 10. ibase 10.))

(defvar board nil)
(defvar sequence nil)
(defvar triang-a nil)
(defvar triang-b nil)
(defvar triang-c nil)

;(defarray board fixnum 16.)
;(defarray sequence fixnum 14.)
;(defarray triang-a fixnum 37.)
;(defarray triang-b fixnum 37.)
;(defarray triang-c fixnum 37.)

(defun setup-triang ()
  (setq board (make-array 16.))
  (setq sequence (make-array 14.))
  (setq triang-a (make-array 37.))
  (setq triang-b (make-array 37.))
  (setq triang-c (make-array 37.))

  (fillarray board '(1))
  (setf (aref board 5) 0)

  (fillarray triang-a '(1 2 4 3 5 6 1 3 6 2 5 4 11 12 13 7 8 4
                   4 7 11 8 12 13 6 10 15 9 14 13 13 14 15 9 10 6))

  (fillarray triang-b '(2 4 7 5 8 9 3 6 10 5 9 8 12 13 14 8 9 5
                   2 4 7 5 8 9 3 6 10 5 9 8 12 13 14 8 9 5))

  (fillarray triang-c '(4 7 11 8 12 13 6 10 15 9 14 13 13 14 15 9 10 6
                   1 2 4 3 5 6 1 3 6 2 5 4 11 12 13 7 8 4)) )

(defun last-position ()
  (do ((i 1 (1+ i)))
      ((= i 16.) 0)
    (cond ((= 1 (aref board i)) (return i)))))

(defun try (i depth)
  (cond ((= depth 14.)
         (let ((lp (last-position)))
           (cond ((member lp final))
                 (t (push lp final))))
         (push (cdr (listarray sequence)) answer) t)
        ((and (= 1 (aref board (aref triang-a i)))
              (= 1 (aref board (aref triang-b i)))
              (= 0 (aref board (aref triang-c i))))
         (setf (aref board (aref triang-a i)) 0)
         (setf (aref board (aref triang-b i)) 0)
         (setf (aref board (aref triang-c i)) 1)
         (setf (aref sequence depth) i)
         (do ((j 0 (1+ j))
              (depth (1+ depth)))
             ((or (= j 36.)
                  (try j depth))
              ()))
         (setf (aref board (aref triang-a i)) 1)
         (setf (aref board (aref triang-b i)) 1)
         (setf (aref board (aref triang-c i)) 0)
         ())))

;(defun gogogo (i)
;       (let ((answer ())
;            (final ()))
;           (try i 1)))

;(include "timer.lsp")

;(timer timit
;       (gogogo 22.))

(defun test ()
  (let ((answer ())
        (final ()))
    (try 22. 1)
    (= (length answer) 775.)))

;;;END




;;;;THIS MUST BE COMPILED WITH HARDEBECK COMPILER!!!!!
(defun triang ()
  (setup-triang)
  (progn
    (hw:write-microsecond-clock (hw:unboxed-constant 0))
    (li:error "TRIANG complete." (test) (hw:read-microsecond-clock)))
  ;; Look in  This error: A0  Result: A1 Time in Microseconds: A2
  (loop))
