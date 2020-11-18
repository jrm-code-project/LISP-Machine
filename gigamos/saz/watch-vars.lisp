
(defmacro watch-vars (vars form)
  ;; indicates any mutations of variables specified in vars which arose
  ;; as a result of evaluating form...
  ;; E.g., (watch-vars (foo bar) (setq foo 3 bar 4))
  ;;   ==> foo: NIL --> 3
  ;;       bar: NIL --> 4
  ;;       4
  ;; ...assuming of course that the two variables were unbound before
  ;; calling watchvars...
  `(let* ((prev-value-list
            (list
               ,@(mapcar #'(lambda (f) `',(symeval f))
                         (if (listp vars)
                             vars
                           (list vars)))))
          (return-me (si:eval-special-ok ,form)))
     (print 'got-here)
     (loop for var in ',vars
           and old-val in prev-value-list
           do (format t "~%~A: ~S --> ~S" var old-val (symeval var)))
     return-me))
