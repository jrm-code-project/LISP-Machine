;;; -*- Mode:LISP; Package:USER; Base:10; Readtable:CL -*-

(defun re-parse (pat)
  (let ((result nil)
        (p 0)
        (pend (string-length pat))
        last-start zero-times-ok many-times-ok c
        )
    (labels ((pat-fetch ()
                        (when (= p pend)
                          (ferror nil "end of pattern"))
                        (setq c (char pat p))
                        (incf p)
                        c)
             (pat-unfetch ()
                          (decf p))
             (pat-push (item)
                       (cond ((null result)
                              (setq result (cons item nil)))
                             (t
                              (setf (cdr (last result)) (cons item nil)))))
             (pat-insert (place item)
                         (let ((new-cell (cons nil nil)))
                           (setf (cdr new-cell) (cdr place))
                           (setf (cdr place) new-cell)
                           (setf (car new-cell) (car place))
                           (setf (car place) item)))
             (normal-char ()
                          (let ((l (last result)))
                            (cond ((and (consp (car l))
                                        (eq (caar l) :exact)
                                        (or (= p pend)
                                            (not (memq (char pat p) '(#\* #\^ #\+ #\?)))))
                                   (setf (cadar l) (string-append (cadar l) c)))
                                  (t
                                   (pat-push (list :exact (string c)))
                                   (setq last-start (last result))))))
             )
      (do-forever
        (cond ((= p pend)
               (dolist (x result)
                 (when (and (consp x)
                            (memq (car x) '(:on-failure-jump :dummy-failure-jump)))
                   (when (null (cdr (cdr x)))
                     (ferror nil "on-failure-jump to end of pattern"))
                   (setf (cdr x) (cdr (cdr x)))))
               (return result))
              (t
               (case (pat-fetch)
                 (#\$
                  (cond ((or (eq p pend)
                             (and (eq (char pat p) #\\)
                                  (memq (char pat (1+ p)) '(#\) #\|))))
                         (pat-push :endline))
                        (t
                         (normal-char))))
                 (#\^
                  (cond ((null last-start)
                         (pat-push :begline))
                        (t
                         (normal-char))))
                 ((#\* #\+ #\?)
                  (setq zero-times-ok nil)
                  (setq many-times-ok nil)
                  (do-forever
                    (if (not (eq c #\+))
                        (setq zero-times-ok t))
                    (if (not (eq c #\?))
                        (setq many-times-ok t))
                    (pat-fetch)
                    (when (not (memq c '(#\* #\+ #\?)))
                      (pat-unfetch)
                      (return nil)))
                  (when many-times-ok
                    (pat-push (cons :maybe-finalize-jump last-start)))
                  (pat-insert last-start (cons :on-failure-jump (last result)))
                  (when (not zero-times-ok)
                    (pat-insert last-start (cons :dummy-failure-jump (last result)))))
                 (#\.
                  (pat-push :anychar)
                  (setq last-start (last result)))
                 (t
                  (normal-char)))))))))

(defun re-exec (pat string)
  (let ((p pat)
        op exp
        (d 0)
        (dend (string-length string))
        fstack
        )
    (labels ((next ()
                   (setq exp (car p))
                   (cond ((consp exp)
                          (setq op (car exp)))
                         (t
                          (setq op exp)))
                   (setq p (cdr p))
                   op)
             (dnext ()
                    (cond ((= d dend)
                           (fail))
                          (t
                           (prog1 (char string d) (incf d)))))
             (fail ()
                   (cond ((null fstack)
                          (return-from re-exec nil))
                         (t
                          (let ((x (pop fstack)))
                            (setq p (car x))
                            (setq d (cadr x)))
                          :fail)))
             )
      (do-forever
        (cond ((null p)
               (return d))
              (t
               (ecase (next)
                 (:anychar
                  (let ((c (dnext)))
                    (cond ((eq c :fail))
                          ((eq c #\return)
                           (fail))
                          (t nil))))
                 (:begline
                  (when (and (not (= d 0))
                             (not (= (char string (1- d)) #\return)))
                    (fail)))
                 (:endline
                  (when (and (not (= d dend))
                             (not (= (char string d) #\return)))

                    (fail)))
                 (:on-failure-jump
                  (push (list (cdr exp) d) fstack))
                 (:maybe-finalize-jump
                  (setq p (cdr exp)))
                 (:exact
                  (let ((ex (cadr exp)))
                    (cond ((< (- dend d) (string-length ex))
                           (fail))
                          (t
                           (dotimes (i (string-length ex)
                                       (incf d (string-length ex)))
                             (when (not (eq (char string (+ d i)) (char ex i)))
                               (return (fail))))))))
                 )))))))
