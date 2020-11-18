;;; -*- Mode:LISP; Package:SIM; Readtable:CL; Base:10 -*-

;(defstruct (s-node (:type :named-array)
;                  (:print "#<~s ~s ~s>"
;                          (type-of s-node)
;                          (s-node-sym s-node)
;                          (%pointer s-node)))
;  adr
;  size
;  sym
;  left
;  right)

;(defvar *address-to-symbol-table* nil)

;(defun adr-to-symbol (adr)
;  (labels ((adr-to-symbol-1 (adr node)
;            (cond ((null node)
;                   nil)
;                  ((< adr (s-node-adr node))
;                   (adr-to-symbol-1 adr (s-node-left node)))
;                  ((< adr (+ (s-node-adr node) (s-node-size node)))
;                   node)
;                  (t
;                   (adr-to-symbol-1 adr (s-node-right node))))))
;    (let ((node (adr-to-symbol-1 adr *address-to-symbol-table*)))
;      (cond ((null node)
;            nil)
;           (t
;            (s-node-sym node))))))


(defvar *address-to-symbol-list* nil)

(defun add-symbol-to-reverse-list (sym adr size type)
  (cond ((null *address-to-symbol-list*)
         (setq *address-to-symbol-list* (list (list sym adr size type))))
        ((< adr (cadr (car *address-to-symbol-list*)))
         (setq *address-to-symbol-list* (cons (list sym adr size type)
                                              *address-to-symbol-list*)))
        (t
         (do ((before *address-to-symbol-list* (cdr before))
              (after (cdr *address-to-symbol-list*) (cdr after)))
             ((null after)
              (setf (cdr before)
                    (list (list sym adr size type))))
           (cond ((> (cadr (car after)) adr)
                  (setf (cdr before)
                        (cons (list sym adr size type)
                              after))
                  (return nil))))))
  nil)

(defun main-mem-symbol-info (adr)
  (labels ((in-range-p (adr sym-list)
                       (and (>= adr (cadr sym-list))
                            (< adr (+ (cadr sym-list) (caddr sym-list))))))
    (do ((before *address-to-symbol-list* (cdr before))
         (after (cdr *address-to-symbol-list*) (cdr after)))
        ((null after)
         (cond ((in-range-p adr (car before))
                (list (cadr (car before)) (car (car before)) nil nil (cadddr (car before))))
               (t
                nil)))
      (cond ((> (cadr (car after)) adr)
             (cond ((in-range-p adr (car before))
                    (return (list (cadr (car before)) (car (car before)) nil nil (cadddr (car before)))))
                   (t
                    (return nil))))))))

#|
(defun test-reverse-list ()
  (setq *address-to-symbol-list* nil)
  (mapcar #'(lambda (x)
              (apply #'add-symbol-to-reverse-list (car x))
              (when (not (equal *address-to-symbol-list* (cadr x)))
                (ferror nil "bad add")))
          '(((five 5 2 t) ((five 5 2 t)))
            ((ten 10. 2 t) ((five 5 2 t) (ten 10. 2 t)))
            ((eight 8 2 t) ((five 5 2 t) (eight 8 2 t) (ten 10. 2 t)))
            ((eighteen 18. 2 t) ((five 5 2 t) (eight 8 2 t) (ten 10. 2 t) (eighteen 18. 2 t)))
            ((two 2 2 t) ((two 2 2 t) (five 5 2 t) (eight 8 2 t) (ten 10. 2 t) (eighteen 18. 2 t)))))
  t)
|#

(defvar *register-table* (make-hash-table :test 'equal))
(defvar *symbol-table* (make-hash-table :test 'equal))

(defun clear-register-table ()
  (clrhash *register-table*))

(defun add-register (register-name val)
  (puthash (string-upcase register-name) val *register-table*))

(defun add-symbol (symbol val size type)
  (let ((name (string-upcase symbol)))
  (when (or (get symbol 'sim-asm)
            (gethash name *register-table*))
    (ferror nil "name conflict"))
  (puthash name val *symbol-table*)
  (add-symbol-to-reverse-list symbol val size type)
  ))

(defun clear-symbols ()
  (clrhash *symbol-table*)
  (setq *address-to-symbol-list* nil))

(defun symbol-lookup (symbol)
  (or (gethash (string-upcase symbol) *register-table*)
      (gethash (string-upcase symbol) *symbol-table*)))

(defun sim-eval (exp &aux val)
  (cond ((numberp exp) exp)
        ((setq val (symbol-lookup exp))
         val)
        (t nil)))

(defun ra-command-to-address (command)
  (let ((info (assq command ra-commands-to-addresses)))
    (when (null info)
      (ferror 'bad-reg-adr "unknown reg-adr name ~s" command))
    (cdr info)))

(defun ra-address-info (adr)
  (cond ((< adr 0)
         (do ((pair ra-addresses-to-commands (cdr pair))
              (old-pair nil pair))
             ((null pair))
           (cond ((> (caar pair) adr)
                  (return (car old-pair))))))
        ((main-mem-symbol-info adr))
        (t
         nil)))

(defun ra-read (adr)
  (when (>= adr 0)
    (ferror nil "can't use this for main memory"))
  (let ((info (ra-address-info adr)))
    (when (null info)
      (ferror 'bad-reg-adr "unknown register base ~o" adr))
    (when (null (send *proc* :operation-handled-p (caddr info)))
      (ferror 'bad-reg-adr "~s is not implemented" (caddr info)))
    (send *proc* (caddr info) (- adr (car info)))))

(defun ra-write (adr val)
  (when (>= adr 0)
    (ferror nil "can't use this for main memory"))
  (let ((info (ra-address-info adr)))
    (when (null info)
      (ferror 'bad-reg-adr "unknown register base ~o" adr))
    (when (null (send *proc* :operation-handled-p (cadddr info)))
      (ferror 'bad-reg-adr "~s is not implemented" (cadddr info)))
    (send *proc* (cadddr info) (- adr (car info)) val)))
