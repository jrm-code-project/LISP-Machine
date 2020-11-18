;;; -*- Mode:LISP; Package:USER; Base:10; Readtable:ZL -*-

(defun rand ()
  (ash (random (ash 2 32)) (- (random 32))))

;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;

(defun cf-hack-switch-1 (ar accessor)
  (let ((w (funcall accessor ar)))
    (multiple-value-bind (bits-wide bits-set bits-reset)
        (loop for b in (get 'lam-opt-processor-switches :bits)
              for bit = (ldb (symeval b) w)
              for elt = `(,(format nil "~@(~a~)" (get b :name)) :eval (symeval b))
              when (> (ldb (byte 6 0) (symeval b)) 1)   ;field is wider than one bit
                collect elt into wide-list
              else when (zerop bit)
                collect elt into reset-list
              else
                collect elt into set-list
              finally (return (values wide-list set-list reset-list)))
      (let ((switch
            (tv:menu-choose (append '(("NOW SET; SELECT TO RESET:" :no-select nil))
                                    bits-set
                                    '(("NOW RESET; SELECT TO SET:" :no-select nil))
                                    bits-reset
                                    '(("WIDER THAN ONE BIT; SELECT TO EDIT:" :no-select nil))
                                    bits-wide)
                            "Choose bit to change")))
        (when switch
          (format t "~&~s" switch))))))

;;(tv:multiple-menu-choose (get 'lam-opt-processor-switches :bits) "Foo!" '(:mouse)
;;  (nthcdr 6 (get 'lam-opt-processor-switches :bits)))

;;(tv:multiple-choose
;;   "Foo!"
;;   '((foo "Foo" ((:set t) :reset))
;;     (bar "Bar" (:set (:reset t)))
;;     (baz "Baz" (:set :reset)))
;;   '((:set "Set" nil (:reset) (:reset) nil)
;;     (:reset "Reset" nil (:set) (:set) nil)))

(defconst set-choice '((:set t) :reset))
(defconst reset-choice '(:set (:reset t)))

(defun cf-hack-switch-2 (ar accessor)
  (let* ((w (funcall accessor ar))
         (x (tv:multiple-choose
              "Select bits to set:"
              (loop for b in (get 'lam-opt-processor-switches :bits)
                    for bit = (ldb (symeval b) w)
                    collect `(,b
                              ,(format nil "~@(~a~)" (get b :name))
                              ,(if (zerop bit) reset-choice set-choice)))
              '((:set "Set" nil (:reset) (:reset) nil)
                (:reset "Reset" nil (:set) (:set) nil)))))
    (format t "~{~&~s~}" x)
    (loop for b in x
          do (setq w (dpb (if (eq (cadr b) :set) 1 0) (symeval (car b)) w)))))

;;(defun cf-set-switch (ar accessor)
;;  (let ((switch (tv:menu-choose
;;                (loop for l in (get 'lam-opt-processor-switches :bits)
;;                      collect `(,(get l :name) :eval ,l)))))

;;;;;;;;;;;;;;;;

(defun reff (sym ar &optional new)
  (let* ((size (or (get sym :size) 4))
         (mult (or (get sym :repeat) 4))
         (offset (or (get sym :offset) (* (symeval sym) mult))))
    (cond
      (new
       (set-bytes ar offset size new))
      (t
       (get-bytes ar offset size)))
    ))

(defsetf reff reff)

;;;;;;;;;;;;;;;;

(defun undef (x)
  (cond
    ((listp x)
     (dolist (q x)
       (undef q)))
    (t
     (makunbound x)
     (fmakunbound x)
     (do ((p (plist x) (cddr p)))
         ((null p))
       (remprop x (car p))))))

;;;;;;;;;;;;;;;;
