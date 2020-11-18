;;; -*- Mode:LISP; Package:LISP-INTERNALS; Base:10; Readtable:CL -*-


;;; FUNCALL - the compiler puts the function to be called in *ARG-2*
;;;                             the argument count is in     *ARG-1*
;;; all of the actual arguments are passed the usual way
;;;
(defun funcall-internal (&rest ignore)
  (do () (())
  (cond
    ((k2:%compiled-function-p gr:*arg-2*)
     (setq gr:*arg-2* (k2:get-entry-adress-for-funcall gr:*arg-2* gr:*arg-1*))
     (if (or (= gr:*return-1* gr:*arg-1*)
             (and (minusp gr:*return-0*) (>= gr:*return-0* (- 1 gr:*return-0*))))
         (hw:dispatch gr:*arg-2*)
       (li:error "Bad number of arguments to function during FUNCALL")))
    ((symbolp gr:*arg-2*)
     (setq gr:*arg-2* (symbol:symbol-function gr:*arg-2*)))
    ((consp gr:*arg-2*)
     (li:error "FUNCALL can't handle lambda lists or macros yet." gr:*arg-2*))
    (t (li:error "You can't FUNCALL that." gr:*arg-2*)))))

(defun funcall (function &rest arglist)
  (setq gr:*value-1* arglist)
  (setq gr:*arg-1* 0)
  (setq gr:*arg-2* function)
  (apply-internal))

;;; APPLY-INTERNAL get its arguments passed in a funny way
;;;      *ARG-1*   contains the count of arguments to apply (excluding the last)
;;;      *ARG-2*   contains the function to be applied
;;;      *VALUE-1* contains the last argument to apply (The list)
(defun apply-internal ()
  (do () (())
    (cond
      ((k2:%compiled-function-p gr:*arg-2*)
       (setq gr:*arg-2* (apply-arg-scan gr:*arg-2* gr:*arg-2* gr:*value-1*))
       (if (minusp gr:*return-1*)
           (do () (()) ; spread some arguments
             (when (zerop gr:*return-1*) (hw:dispatch gr:*arg-2*))
             (setq gr:*return-1* (1+ gr:*return-1*))
             (setq gr:*value-2* (car gr:*value-1*))
             (setq gr:*value-1* (cdr gr:*value-1*))
             (if (>= gr:*return-0* 16.)
                 (progn
                   (setq gr:*stack-pointer* (hw:32-1+ gr:*stack-pointer*))
                   (hw:write-md-boxed gr:*value-2*)
                   (hw:vma-start-write-boxed (hw:32-1- gr:*stack-pointer*)))
               (dispatch (byte 4 0) gr:*return-0*
                 (0.  (setf (hw:a0)  gr:*value-2*)) (1.  (setf (hw:a1)  gr:*value-2*))
                 (2.  (setf (hw:a2)  gr:*value-2*)) (3.  (setf (hw:a3)  gr:*value-2*))
                 (4.  (setf (hw:a4)  gr:*value-2*)) (5.  (setf (hw:a5)  gr:*value-2*))
                 (6.  (setf (hw:a6)  gr:*value-2*)) (7.  (setf (hw:a7)  gr:*value-2*))
                 (8.  (setf (hw:a8)  gr:*value-2*)) (9.  (setf (hw:a9)  gr:*value-2*))
                 (10. (setf (hw:a10) gr:*value-2*)) (11. (setf (hw:a11) gr:*value-2*))
                 (12. (setf (hw:a12) gr:*value-2*)) (13. (setf (hw:a13) gr:*value-2*))
                 (14. (setf (hw:a14) gr:*value-2*)) (15. (setf (hw:a15) gr:*value-2*))))
             (setq gr:*return-0* (1+ gr:*return-0*)))
           (do () (()) ;cons some args
             (when (zerop gr:*return-1*) (hw:dispatch gr:*arg-2*))
             (setq gr:*return-1* (1- gr:*return-1*))
             (if (>= gr:*return-0* 16.)
                 (progn
                   (hw:vma-start-read-vma-boxed-md-boxed (hw:32-1- gr:*stack-pointer*))
                   (setq gr:*value-1* (cons (hw:read-md) gr:*value-1*))
                   (setq gr:*stack-pointer* (hw:32-1- gr:*stack-pointer*)))
               (progn
                 (setq gr:*value-1*
                       (cons (dispatch (byte 4 0) gr:*return-0*
                               (0.  (hw:a0)) (1.  (hw:a1)) (2.  (hw:a2)) (3.  (hw:a3))
                               (4.  (hw:a4)) (5.  (hw:a5)) (6.  (hw:a6)) (7.  (hw:a7))
                               (8.  (hw:a8)) (9.  (hw:a9)) (10. (hw:a10)) (11. (hw:a11))
                               (12. (hw:a12)) (13. (hw:a13)) (14. (hw:a14)) (15. (hw:a15)))
                             gr:*value-1*))))
             (setq gr:*return-0* (1- gr:*return-0*)))))
      ((symbolp gr:*arg-2*)
       (setq gr:*arg-2* (symbol:symbol-function gr:*arg-2*)))
      ((consp gr:*arg-2*)
       (li:error "APPLY can't handle lambda lists or macros yet." gr:*arg-2*))
      (t (li:error "You can't APPLY that." gr:*arg-2*)))))

;;; APPLY-ARG-SCAN figures out the total number of arguments in the apply
;;;  and returns information about how to either spread or cons the arguments.
;;;  the total argument count is stored in gr:*ARG-1* for the dispatch later.
;;;      (values entry-address argument-index arg-adjust-count)
;;;   ENTRY-ADDRESS is the PC to dispatch to. If there are rest args, then it is two past the normal entry.
;;;   ARGUMENT-INDEX is the argument number to start adjusting from
;;;   ARG-ADJUST-INDEX is the number of arguments to be adjusted and the direction
;;;         Positive - CONS more args onto the &REST list, decrement the argument-index
;;;         Negative - spread more args into regs, shorten the &REST list, increment the agument-index

(defun apply-arg-scan (fcn pre-spread-nargs last-arg)
  (let* ((entry-points    (k2:%compiled-function-entry-points fcn))
         (start-pc        (k2:%compiled-function-code fcn))
         (last-arg-length (cond
                            ((consp last-arg) (array:length last-arg))
                            ((null  last-arg) 0)
                            (t
                             (li:error "APPLY's last argument wasn't a list." last-arg))))
         (total-nargs     (+ last-arg-length pre-spread-nargs))
         (entry-length    (array:length entry-points)))
    (do ((i 0 (+ i 2)))
        ((>= i entry-length)
         (li:error "Bad number of arguments to function during APPLY" total-nargs fcn))
      (let ((entry-nargs (array:svref entry-points i)))
        (when (or (= entry-nargs total-nargs) (minusp entry-nargs))
          (let ((entry-offset (array:svref entry-points (1+ i))))
            (when (minusp entry-nargs)
              (setq entry-offset (+ entry-offset 2))
              (setq entry-nargs (- 1 entry-nargs)))
            (setq gr:*arg-1* total-nargs)
            (return (values
                      (hw:24+ entry-offset start-pc)
                      (if (> pre-spread-nargs entry-nargs) pre-spread-nargs entry-nargs)
                      (- pre-spread-nargs entry-nargs)))))))))


(defun apply (function arg &rest arg-list)
  (when arg-list
    (setq arg (cons arg nil))
    (do ((args arg)
         (arg-list arg-list (cdr arg-list)))
        ((null (cdr arg-list)) (rplacd args (car arg-list)))
      (rplacd args (cons (car arg-list) nil))))
  (setq gr:*arg-1* 0)
  (setq gr:*arg-2* function)
  (setq gr:*value-1* arg)
  (apply-internal))
