;;; -*- Mode:LISP; Package:SIM; Base:10; Readtable:CL -*-

(defconst *active-registers* '(a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15))
(defconst *open-registers* '(o0 o1 o2 o3 o4 o5 o6 o7 o8 o9 o10 o11 o12 o13 o14 o15))
(defconst *return-registers* '(r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15))


(defun check-arg-and-local-order (function)
  (let ((arg-map (cadr (assq 'compiler:arg-map (debugging-info function))))
        (local-map (cadr (assq 'compiler:local-map (debugging-info function)))))
    (let ((act-start (cl:member (car *active-registers*) arg-map :test #'(lambda (x y)
                                                                           (string-equal x (car y))))))
      (do ((i 0 (1+ i))
           (act-list act-start (cdr act-list)))
          ((= i 16.)
           (when (not (string-equal (caar act-list) "TEMP"))
             (ferror nil "No N-ARGS arg (named TEMP)")))
        (when (not (string-equal (caar act-list) (format nil "~c~d" (aref (string (car *active-registers*)) 0) i)))
          (ferror nil "args out of order"))))
    (let ((opn-start (cl:member (car *open-registers*) local-map :test #'(lambda (x y) (string-equal x (car y))))))
      (do ((i 0 (1+ i))
           (opn-list opn-start (cdr opn-list)))
          ((= i 16.))
        (when (not (string-equal (caar opn-list) (format nil "~c~d" (aref (string (car *open-registers*)) 0) i)))
          (ferror nil "opens out of order"))))
    (let ((ret-start (cl:member (car *return-registers*) local-map :test #'(lambda (x y) (string-equal x (car y))))))
      (do ((i 0 (1+ i))
           (ret-list ret-start (cdr ret-list)))
          ((= i 16.))
        (when (not (string-equal (caar ret-list) (format nil "~c~d" (aref (string (car *return-registers*)) 0) i)))
          (ferror nil "returns out of order"))))
    ))


(defmacro make-argspec (required-args optional-args rest-p)
  (cond ((and (cl:listp required-args)
              (cl:listp optional-args)
              (memq rest-p '(t nil)))
         (dpb (if (eval rest-p) 1 0)
              %%k-rest-p
              (dpb (+ (length (eval required-args)) (length (eval optional-args)))
                    %%k-max-args
                    (dpb (length (eval required-args))
                         %%k-min-args
                         0))))
        (t
         `(dpb (if ,rest-p 1 0)
               %%k-rest-p
               (dpb (+ (length ,required-args) (length ,optional-args))
                    %%k-max-args
                    (dpb (length ,required-args)
                         %%k-min-args
                         0))))))

(defmacro defkfun (name arglist &body body)
  `(progn
     (defun ,name (return-destination return-frame-location
                   ,@*active-registers*
                   temp
                   &aux
                   ,@*open-registers*
                   ,@*return-registers*)
       (declare (arglist ,@arglist))
       return-destination return-frame-location
       ,@*active-registers*
       temp
       ,@*open-registers*
       ,@*return-registers*
       (prog top-of-k-function ()
         (%k-check-args (make-argspec ',arglist nil nil))
         ,@body
         (ferror nil "trying to do lisp return from k function")
         ))
     (check-arg-and-local-order ',name)
     ))

(defconst *number-of-k-frames* 256.)
(defvar *k-frames*)
(defvar *k-frame-stack-pointer*)

(defresource k-frame ()
  :constructor (let ((k-frame (make-array *number-of-k-frames*)))
                 (dotimes (i (array-length k-frame))
                   (aset (make-list 16.) k-frame i))
                 k-frame))

(defvar *current-catch-tag* 0)

(defun get-catch-tag ()
  (without-interrupts
    (incf *current-catch-tag*)))

(defvar *state-stack*)

(defun unwind-state-stack-to-level (level)
  (do ()
      (())
    (cond ((eq level *state-stack*)
           (return nil))
          ((null *state-stack*)
           (ferror nil "ran out of state stack"))
          (t
           (let ((item (pop *state-stack*)))
             (cond ((consp item)
                    (setf (contents (car item)) (cdr item)))
                   ((fixnump item)
                    (setq *k-frame-stack-pointer* item))
                   ((compiled-function-p item)
                    (funcall item))
                   (t
                    (ferror nil "unknown item on *state-stack* ~s" item))))))))



;return frame must be at end of local block so it is more
;likely to be in the pdl buffer when the k function returns
(defun call-k (k-function &rest arguments)
  (let (return-value i args
        r0  r1  r2  r3  r4  r5  r6  r7
        r8  r9  r10 r11 r12 r13 r14 r15)
    r0  r1  r2  r3  r4  r5  r6  r7
    r8  r9  r10 r11 r12 r13 r14 r15
    (when (> (length arguments) 16.)
      (ferror nil "too many arguments"))
    (let ((*state-stack* nil)
          (*k-frame-stack-pointer* 0)
          (*k-frames* (allocate-resource 'k-frame)))
      (unwind-protect
          (progn
            (%open-call-block k-function 0 0)
            (%push (locf return-value))
            (%push (locf r0))
            (setq i 0)
            (setq args arguments)
            (do ()
                ((= i 16.))
              (%push (car args))
              (setq i (+ i 1))
              (setq args (cdr args)))
            (%push (length arguments))
            (%activate-open-call-block)
            return-value)
        (deallocate-resource 'k-frame *k-frames*)
        (unwind-state-stack-to-level nil)))))

(defmacro %k-open ()
  `(micro:%u-k-open (locf o0)))

(defmacro %k-call (function n-args return-dest)
  (when (not (numberp n-args))
    (ferror nil "n-args must be a constant number"))
  `(micro:%u-k-call ,function ,n-args ,return-dest (locf o0) (locf r0)))

(defmacro %k-return (value)
  `(return-from top-of-k-function
     (micro:%u-k-return ,value (locf a0) return-frame-location return-destination)))

(defmacro %k-t-open ()
  nil)

(defmacro %k-t-call (function n-args)
  (when (not (numberp n-args))
    (ferror nil "n-args must be a number"))
  `(ignore
     (micro:%u-k-t-call ,function ,n-args (locf a0) (locf o0))))

(defmacro %k-check-args (argspec)
  `(micro:%u-k-check-args ,argspec))
