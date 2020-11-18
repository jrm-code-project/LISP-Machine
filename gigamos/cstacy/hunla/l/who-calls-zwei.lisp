;;;-*- Mode:LISP; Package:ZWEI; Base:10; Readtable:CL -*-

(defcom com-who-calls
        "Show the callers of an object."
        ()
  (let* ((symbol (read-function-name "Who Calls"
                                     (relevant-function-name (point) nil nil t)
                                     'aarray-ok))
         (callers (hl:who-calls-internal symbol)))
    (hl:who-calls-check-for-magic symbol)
    (cond ((null callers)
           (hl:printing-package-names
             (format t "~&No callers of ~S found." symbol)))
          (t
           (hl:printing-package-names
             (format t "~&There ~:[is~;are~] ~D caller~P of ~S:"
                     (> (length callers) 1) (length callers) (length callers) symbol))
           (let ((items nil))
             (hl:who-calls-1 callers
                             #'(lambda (caller-info tab-column &rest ignore)
                                 (push (cons (with-output-to-string (*standard-output*)
                                               (write-string "  ")
                                               (hl:print-who-calls-item caller-info tab-column))
                                             (car caller-info))
                                       items)))
             (setq items (nreverse items))
             (send *standard-output* :item-list 'function-name items)
             (command-store 'com-go-to-next-top-level-possibility #\c-. *zmacs-comtab*)
             (format t "~2&Type ~A to ~:[start editing these~;edit this~].~%"
                     (key-for-command-set-c-. 'com-go-to-next-possibility #\c-sh-P)
                     (= (length items) 1))
             (insert-definitions-possibilities
               (hl:printing-package-names
                 (format nil "~&Callers of ~S:" symbol))
               (mapcar #'cdr items))))))
  dis-none)

(SET-COMTAB *ZMACS-COMTAB* NIL (MAKE-COMMAND-ALIST '(COM-WHO-CALLS)))
