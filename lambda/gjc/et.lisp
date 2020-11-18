;;; -*- Mode:LISP; Package:USER; Readtable:ZL; Base:10; Fonts:(METS) -*-


(defun et ()
  (format t "~&~8<HOST~>~19<CHAOS~>~10<ETHERNET~>~%")
  (dolist (e ethernet:ether-address-translations)
     (format t "~20a #o~o ~x~%"
                 (si:get-host-from-address (first e) :chaos)
                 (first e)
                 (second e))))

(defun etc ()
   (let ((new nil))
      (dolist (e ethernet:ether-address-translations)
         (push (or (si:get-host-from-address (first e) (fourth e))
                        (list (first e) (fourth e)))
                   (get (or (assoc (second e) new)
                               (car (push (list (second e))
                                               new)))
                          (fourth e))))
      (let ((hosts (remove-duplicates
                          (mapcan #'(lambda (l)
                                             (mapcan #'(lambda (x)
                                                                (if (atom x) nil
                                                                   (copylist* x)))
                                                           (cdr l)))
                                        new))))
         (format t "~&~D rack~:*~p ~D host~:*~p"
                     (length new)
                     (length hosts))
         (pprint new))))
