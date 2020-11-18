;;; -*- Mode:LISP; Package:STEVE; Readtable:CL; Base:10 -*-

;;; First some missing bp hacking functions

(defun line>= (x line)
  (do ((forward line (line-next forward))
       (backward (line-previous line) (line-previous backward)))
      (nil)
    (cond ((eq x forward) (return t))
          ((eq x backward) (return nil))
          ((null forward) (return nil))
          ((null backward) (return t)))))

(defmethod (bp :equal) (other)
  (and (eq line (bp-line other))
       (=& position (bp-position other))))

(defmethod (bp :print-self) (&optional (stream standard-output) ignore ignore)
  (format stream "#<~s ~z " 'bp (si:address-of self))
  (unless (or (null line) (null position))
    (print-bp-internal line position stream))
  (write-char #\> stream))

(defun print-bp-internal (line position stream)
  (let ((line-chars (line-chars line)))
    (princ #\" stream)
    (oustr line-chars stream 0 position)
    (write-char #\^ stream)
    (oustr line-chars stream position (-& (line-char-count line) position))
    (write-char #\" stream)))

(defmethod (line :print-self) (&optional (stream standard-output) ignore ignore)
  (format stream "#<~s ~s ~z>"
          'line (substring chars 0 char-count) (si:address-of self)))

(defun bp-< (bp1 bp2)
  (let ((line1 (bp-line bp1))
        (line2 (bp-line bp2)))
    (cond ((eq line1 line2)
           (< (bp-position bp1) (bp-position bp2)))
          (t (not (line>= line1 line2))))))

(defmacro move-bp (bp to-bp-arg)
  (let ((to-bp (gentemp "BP")))
    `(let ((,to-bp ,to-bp-arg))
       (send ,bp :move (bp-line ,to-bp) (bp-position ,to-bp)))))

(defmacro mark ()
  '(get-mark))

;;; Same as MAKE-BP but doesn't tell LINE about it

(defun make-temp-bp (bp)
  (make-instance 'bp :buffer :temp-bp
                     :line (bp-line bp)
                     :position (bp-position bp)))

(defmacro flush-bp (bp)
  `(send ,bp :expire))

(defmacro with-bp ((var bp) &body corpus)
  `(let ((,var (copy-bp ,bp)))
     (unwind-protect
         (progn
           ,@corpus)
       (flush-bp ,var))))

(defmacro with-bp* (bindings &body corpus)
  (let ((x-bindings (loop for (var bp) in bindings
                          collecting `(,var (copy-bp ,bp))))
        (flushings (loop for (var . ignore) in bindings
                         collecting `(flush-bp ,var))))
    `(let* ,x-bindings
       (unwind-protect
           (progn
             ,@corpus)
         ,@flushings))))

(defmacro preserving-point (&body corpus)
  `(with-bp (*editor-cursor* *editor-cursor*)
     ,@corpus))

;;; This exists but in a slightly different form

(defmacro region ((bp1 bp2) &body corpus)
  `(with-bp* ((,bp1 *editor-cursor*)
              (,bp2 (or (mark) (ed-warn :no-mark))))
     (cond ((bp-< ,bp2 ,bp1)
            (psetq ,bp1 ,bp2
                   ,bp2 ,bp1)))
     ,@corpus))

;;; This is to make up for the lack of motion functions
;;; which don't hack the point

(defmacro with-point-at ((bp) &body corpus)
  `(with-bp (*editor-cursor* *editor-cursor*)
     (move-bp *editor-cursor* ,bp)
     ,@corpus
     (make-temp-bp *editor-cursor*)))

(defmacro funcall-for-bp (motion-fn n start-bp)
  `(with-point-at (,start-bp)
     (let ((*argument* ,n))
       (funcall ,motion-fn))))

;;; An optimized version of UN-KILL

(defun insert-interval (bp interval)
  (with-point-at (bp)
    (loop with max-index = (1-& (vector-length interval))
          for i from 0 to max-index
          do (unless (null (aref interval i))
               (send (edit-cursor-line *editor-cursor*)
                     :insert-string (edit-cursor-position *editor-cursor*)
                     (aref interval i))
               (when (<& i max-index)
                 (send (edit-cursor-line *editor-cursor*)
                       :break (edit-cursor-position *editor-cursor*)))))))

(defmacro delete-interval (bp1 bp2 ignore)
  `(delete-between-marks ,bp1 ,bp2))

(defmacro copy-interval (bp1 bp2 ignore)
  `(progn (setq *last-kill-command-count* -1000) ;ickypoo hack.
          (kill-between-marks ,bp1 ,bp2)
          (pop-kill)))


;;; Another missing function

(defun forward-line ()
  (cond ((zerop& *argument*)
         (send *editor-cursor* :set-pos 0))
        ((minusp& *argument*)
         (do ((line (bp-line *editor-cursor*)
                    (line-previous line))
              (n (-& *argument*) (1-& n)))
             ((if (null line)
                  (ed-warn :at-start)
                (<=& n 0))
              (send *editor-cursor* :move line 0))))
        (:plusp&
         (do ((line (bp-line *editor-cursor*)
                    (line-next line))
              (n *argument* (1-& n)))
             ((if (null line)
                  (ed-warn :at-end)
                (<=& n 0))
              (send *editor-cursor* :move line 0)))))
  nil)

;;; Let's put this here for no good reason

(defun steve-beep ()    ;but dont call it beep. It takes a different number of args.
  (write-char #\bell))

(defun type-ahead-p ()
  (or (send terminal-io :listen)
      executing-keyboard-macro))
