;;; -*- Mode:LISP; Package:ZWEI; Base:10 -*-


(defvar *CALLER-LIST* (list '*CALLER-LIST*))

(defcom COM-INITIALIZE-CALLER-LIST "" ()
  (initialize-caller-list)
  dis-none)

(defun INITIALIZE-CALLER-LIST ()
  (setq *caller-list* (list '*CALLER-LIST*)))

(defcom COM-MARK-CALLER-LIST "" ()
  (mark-caller-list)
  dis-none)

(defun MARK-CALLER-LIST ()
  (nconc *caller-list* (list '*MARK*)))

(defcom COM-ADD-TO-CALLER-LIST "" ()
  (add-to-caller-list)
  dis-none)

(defun ADD-TO-CALLER-LIST ()
  (nconc *caller-list* (list '*TRACE* (first-caller))))

(defcom COM-PRINT-CALLER-LIST "" ()
  (print-caller-list)
  dis-none)

(defun PRINT-CALLER-LIST () (pprint *caller-list*))

(SET-COMTAB *ZMACS-COMTAB*
            '(#/super-meta-i com-initialize-caller-list
              #/super-meta-m com-mark-caller-list
              #/super-meta-a com-add-to-caller-list
              #/super-meta-p com-print-caller-list))

(defun PRINT-CALLERS () (pprint (my-callers-names)))

(defun MY-CALLERS (&optional (n-frames 1000))
  (let* ((rp (si:sg-regular-pdl current-stack-group))
         (index (- (%pointer-difference (si:%stack-frame-pointer) rp)
                   (si:array-data-offset rp))))
    (do ((i 0 (1+ i))
         (frame index (eh:sg-next-active current-stack-group frame))
         parents)
        ((or (null frame)
             (= i n-frames))
         (reverse parents))
      (push (aref rp frame) parents)
      )))


(defun MY-CALLERS-NAMES (&optional (n-frames 1000))
  (let* ((rp (si:sg-regular-pdl current-stack-group))
         (index (- (%pointer-difference (si:%stack-frame-pointer) rp)
                   (si:array-data-offset rp))))
    (do ((i 0 (1+ i))
         (frame index (eh:sg-next-active current-stack-group frame))
         parents)
        ((or (null frame)
             (= i n-frames))
         (reverse parents))
      (push (function-name (eh:rp-function-word rp frame)) parents)
      )))


(defun FIRST-CALLER (&aux (n-frames 1000))
  (let* ((rp (si:sg-regular-pdl current-stack-group))
         (index (- (%pointer-difference (si:%stack-frame-pointer) rp)
                   (si:array-data-offset rp))))
    (do ((i 0 (1+ i))
         (frame index (eh:sg-next-active current-stack-group frame))
         caller)
        ((or (null frame)
             (= i n-frames))
         caller)
      (setq caller (function-name (eh:rp-function-word rp frame)))
      )))


(defun IN-CALLER-LIST (target-caller &aux (n-frames 1000) caller)
  (let* ((rp (si:sg-regular-pdl current-stack-group))
         (index (- (%pointer-difference (si:%stack-frame-pointer) rp)
                   (si:array-data-offset rp))))
    (do ((i 0 (1+ i))
         (frame index (eh:sg-next-active current-stack-group frame)))
        ((or (null frame)
             (= i n-frames)
             (eq caller target-caller)))
      (setq caller (function-name (eh:rp-function-word rp frame))))
    (eq caller target-caller)))


(defun GET-FRAME-FUNCTION (sg frame &aux function (rp (eh:sg-regular-pdl sg)))
  (setq function (function-name (eh:rp-function-word rp frame))))
