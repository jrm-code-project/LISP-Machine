;;; -*- Mode:LISP; Package:(EXHIBIT GLOBAL); Base:10; Readtable:CL -*-


;;; We need a structure examiner that can be called from any program
;;; and that can examine circular structures effectively.
;;; 11/06/85 19:14:36 -George Carrette.

(defvar *exhibit-help* "~%~
H ....... history~%~
L ....... last object~%~
0...9 ... get that slot~%~
G ....... get slot, read index number~%~
R ....... replace a slot, reading index and value~%~
D ....... describe current object
E ....... evaluate something~%")

(defun exhibit (object)
  "A simple structure examiner/editor"
  (do ((command #\CONTROL-L (progn (format t "~&->") (read-char)))
       (object-history nil)
       (current-object object)
       (nslots nil)
       (selection-command nil nil)
       (replace-command nil nil)
       (punting-stream (make-punting-stream)))
      (nil)
    (selector (char-upcase command) char=
      ((#\? #\HELP)
       (format t *exhibit-help*))
      ((#\E)
       (let ((* current-object))
         (eval (prompt-and-read :read "~&evaluate>"))))
      ((#\END #\Q) (RETURN CURRENT-OBJECT))
      ((#\control-L #\PAGE)
       (setq nslots (display-object current-object punting-stream)))
      (#\H
       (setq current-object (prog1 object-history
                                   (push current-object object-history)))
       (setq nslots (display-object current-object punting-stream)))
      ((#\P #\L)
       (setq current-object (prog1 (pop object-history)
                                   (setq object-history (append object-history
                                                                (list current-object)))))
       (setq nslots (display-object current-object punting-stream)))
      ((#\0 #\1 #\2 #\3 #\4 #\% #\6 #\7 #\8 #\9)
       (setq selection-command (digit-char-p command)))
      (#\G
       (format t "Get Index->")
       (setq selection-command (read-index)))
      (#\D
       (DESCRIBE CURRENT-OBJECT))
      (#\R
       (format t "Replace Index->")
       (setq replace-command (read-index))))
    (cond (selection-command
           (cond ((< selection-command nslots)
                  (push current-object object-history)
                  (multiple-value-bind (new errorp)
                      (select-element current-object selection-command)
                    (cond (errorp
                           (format t "~&Cant select because ~A~%" new))
                          ('else
                           (setq current-object new)
                           (setq nslots (display-object current-object punting-stream))))))
                 ('else
                  (format t "~&Index must be < ~D: ~D" nslots selection-command))))
          (replace-command
           (cond ((< replace-command nslots)
                  (let ((errorp
                          (replace-element current-object
                                           replace-command
                                           (let ((* current-object))
                                             (prompt-and-read :eval-read
                                                              "~&New Value for slot ~D: "
                                                              replace-command)))))
                    (cond (errorp
                           (format t "~&Cant replace because ~A~%" errorp))
                          ('else
                           (setq nslots (display-object current-object punting-stream))))))
                 ('else
                  (format t "~&Index must be < ~D: ~D" nslots replace-command)))))))


;; An object display routine should print elements of the object and index numbers.
;; The number of slots in the object is returned.

(defun display-object (x s)
  (send s :clear-display)
  (cond ((instancep x)
         (cond ((send x :get-handler-for :display-slots)
                (send x :display-slots))
               ('else
                (display-instance-slots x s))))
        ((get (type-of x) 'display-object)
         (funcall (get (type-of x) 'display-object) x s))
        ((and (named-structure-p x)
              (get (named-structure-p x) 'si:defstruct-description))
         (display-defstruct x s))
        ('else
         (display-random x s))))

(defun display-random (x s)
  (format s "Object of type ~S~%~S" (type-of x) x)
  0)


(defun display-instance-slots (x s)
  (FORMAT S "Instance of type ~S~%" (type-of x))
  (format s "[0] ~S~%" (si:INSTANCE-FLAVOR x))
  (DO ((IVARS (SI:FLAVOR-ALL-INSTANCE-VARIABLES (SI:INSTANCE-FLAVOR X))
              (CDR IVARS))
       (I 1 (1+ I)))
      ((NULL IVARS)
       I)
    (FORMAT s "[~D] ~S:~27T " I (CAR IVARS))
    (IF (= (SI:%P-LDB-OFFSET SI:%%Q-DATA-TYPE X I) DTP-NULL)
        (FORMAT s "void~%")
      (FORMAT s "~S~%" (SI:%INSTANCE-REF X I)))))


(defun display-defstruct (x s)
  ;; this is not very carefull, since the defstruct description can many times
  ;; be out of whack with objects.
  (let ((DESCRIPTION (GET (IF (CONSP X) (CAR X)
                            (NAMED-STRUCTURE-P X))
                          'SI:DEFSTRUCT-DESCRIPTION)))
    (format s "[0] Slots of a ~S~%" (SI:DEFSTRUCT-DESCRIPTION-NAME DESCRIPTION))
    (DO ((I 1 (1+ I))
         (L (SI:DEFSTRUCT-DESCRIPTION-SLOT-ALIST DESCRIPTION) (CDR L)))
        ((NULL L) I)
      (FORMAT S "[~D] ~30A ~S~%"
              I
              (CAAR L)
              (EVAL `(,(SI:DEFSTRUCT-SLOT-DESCRIPTION-REF-MACRO-NAME (CDAR L)) ',X))))))


;; a selection routine receives the object and slot number. it should select
;; that element if possible, return it. If not then return two values,
;; and error message and T.

(defun select-element (x slot)
  (cond ((instancep x)
         (cond ((send x :get-handler-for :select-element)
                (send x :select-element slot))
               ('else
                (select-instance-element x slot))))
        ((get (type-of x) 'select-element)
         (funcall (get (type-of x) 'select-element) x slot))
        ((and (named-structure-p x)
              (get (named-structure-p x) 'si:defstruct-description))
         (select-element-defstruct x slot))
        ('else
         (values "dont know how to select an element from this" t))))

(defun select-instance-element (x slot)
  (cond ((zerop slot)
         (si:instance-flavor x))
        ((= (SI:%P-LDB-OFFSET SI:%%Q-DATA-TYPE X SLOT) DTP-NULL)
         (values "instance cell is unbound" t))
        ('else
         (SI:%INSTANCE-REF X SLOT))))


(defun select-element-defstruct (x slot)
  (let ((DESCRIPTION (GET (IF (CONSP X) (CAR X)
                            (NAMED-STRUCTURE-P X))
                          'SI:DEFSTRUCT-DESCRIPTION)))
    (cond ((= slot 0)
           (SI:DEFSTRUCT-DESCRIPTION-NAME DESCRIPTION))
          ('else
           (DO ((I 1 (1+ I))
                (L (SI:DEFSTRUCT-DESCRIPTION-SLOT-ALIST DESCRIPTION) (CDR L)))
               ((OR (= I SLOT) (NULL L))
                (COND ((NULL L)
                       (values "no such slot?" t))
                      ('else
                       (EVAL `(,(SI:DEFSTRUCT-SLOT-DESCRIPTION-REF-MACRO-NAME (CDAR L))
                               ',X))))))))))

;; a replacement routine receives three arguments, object, slot, value.
;; It should do the replacement, return NIL if ok, otherwise an error message.

(defun replace-element (x slot value)
  (cond ((instancep x)
         (cond ((send x :get-handler-for :replace-element)
                (send x :replace-element slot value))
               ('else
                (replace-instance-element x slot value))))
        ((get (type-of x) 'replace-element)
         (funcall (get (type-of x) 'replace-element) x slot value))
        ((and (named-structure-p x)
              (get (named-structure-p x) 'si:defstruct-description))
         (replace-element-defstruct x slot value))
        ('else
         "no routine for replacing parts of this structure")))

(defun replace-instance-element (x slot value)
  (cond ((zerop slot)
         "Dangerous! Unimplemented dangerous mode")
        ('else
         (setf (si:%instance-ref x slot) value)
         nil)))


(defun replace-element-defstruct (x slot value)
  (let ((DESCRIPTION (GET (IF (CONSP X) (CAR X)
                            (NAMED-STRUCTURE-P X))
                          'SI:DEFSTRUCT-DESCRIPTION)))
    (cond ((= slot 0)
           "can't change name of struct")
          ('else
           (DO ((I 1 (1+ I))
                (L (SI:DEFSTRUCT-DESCRIPTION-SLOT-ALIST DESCRIPTION) (CDR L)))
               ((OR (= I SLOT) (NULL L))
                (COND ((NULL L)
                       "no such slot?")
                      ('else
                       (EVAL `(SETF (,(SI:DEFSTRUCT-SLOT-DESCRIPTION-REF-MACRO-NAME (CDAR L))
                                     ',X)
                                    ',VALUE))
                       nil))))))))

;; handlers for our favorite objects.

(defun (symbol display-object) (x s)
  (format s "This is a symbol~%")
  (format s "[0] SYMBOL-NAME: ~S~%" (symbol-name x))
  (format s "[1] ~:[unbound~;Value: ~S~]~%"
          (boundp x)
          (and (boundp x) (symbol-value x)))
  (format s "[2] ~:[no function~;function: ~S~]~%"
          (fboundp x)
          (and (fboundp x) (symbol-function x)))
  (do ((l (plist x) (cddr l))
       (i 3 (1+ i)))
      ((null l)
       i)
    (format s "[~D] ~S: ~S~%" i (car l) (cadr l))))


(defun (symbol select-element) (x i)
  (cond ((= i 0)
         (symbol-name x))
        ((= i 1)
         (if (boundp x)
             (symbol-value x)
           (values "symbol is unbound" t)))
        ((= i 2)
         (if (fboundp x)
             (symbol-function x)
           (values "symbol has no function" t)))
        ('else
         (do ((j 3 (1+ j))
              (l (plist x) (cddr l)))
             ((= i j)
              (cadr l))))))


(defun (symbol replace-element) (x i v)
  (cond ((= i 0)
         "you can't change the name of a symbol")
        ((= i 1)
         (setf (symbol-value x) v)
         nil)
        ((= i 2)
         (setf (symbol-function x) v)
         nil)
        ('else
         (do ((j 3 (1+ j))
              (l (plist x) (cddr l)))
             ((= i j)
              (cond ((null l)
                     "plist changed suddenly?")
                    ('else
                     (setf (cadr l) v)
                     nil)))))))

(defun (cons display-object) (x s)
  (cond ((null (cdr (last x)))
         (format s "Elements of a proper list ~D long~%" (length x))
         (do ((l x (cdr l))
              (i 0 (1+ i)))
             ((null l)
              i)
           (format s "[~D] ~S~%" i (car l))))
        ((atom (cdr x))
         (format s "A cons cell~%[0] CAR: ~S~%[1] CDR: ~S~%" (car x) (cdr x)))
        ('else
         (format s "~D Elements of an improper list (non-null cdr)~%:"
                 (1+ (length x)))
         (do ((l x (cdr l))
              (i 0 (1+ i)))
             ((atom l)
              (format s "[~D] ~S~%" i l)
              (1+ i))
           (format s "[~D] ~S~%" i (car l))))))


(defun (cons select-element) (x i)
  (do ((l x (cdr l))
       (j 0 (1+ j)))
      ((or (= i j) (atom l))
       (cond ((and (= i j) (atom l))
              l)
             ((= i j)
              (car l))))))

(defun (cons replace-element) (x i v)
  (do ((l x (cdr l))
       (j 0 (1+ j)))
      ((or (= i j) (atom (cdr l)))
       (cond ((= j i)
              (setf (car l) v))
             ((= (1+ j) i)
              (setf (cdr l) v)))
       nil)))

;;

(DEFVAR *SAFE-PUNT* NIL)

(defun make-punting-stream (&optional (forward-to standard-output))
  (let ((stream)
        (limit (or (send-if-handles forward-to :size-in-characters) 70))
        (index 0))
    (setq stream #'(lambda (operation &optional arg1 &rest args)
                   (si:selectq-with-which-operations operation
                     (:tyo
                       (cond ((char= arg1 #\return)
                              (send forward-to :tyo #\return)
                              (setq index 0))
                             ((not (graphic-char-p arg1))
                              (princ "#\\" stream)
                              (princ (or (char-name arg1) (char-code arg1)) stream))
                             ((< index limit)
                              (send forward-to :tyo arg1)
                              (incf index))
                             (*safe-punt*
                              (throw 'punt t))))
                     (:clear-display
                       (send forward-to :clear-screen)
                       (setq index 0))
                     (:print
                       (let ((*safe-punt* t))
                         (*catch 'punt
                           (cond ((= (length (arglist'si:print-object)) 3)
                                  ;; system version > 102
                                  (si:print-object arg1 (car args) stream))
                                 ('else
                                  ;; release 2.0
                                  (si:print-object arg1 (car args) stream '(:STRING-OUT))))
                           t)))
                     (:string-out
                       (let ((*safe-punt* t))
                         (*catch 'punt
                           (do ((j (or (car args) 0) (1+ j))
                                (end (or (cadr args) (length arg1))))
                               ((= j end))
                             (send stream :tyo (aref arg1 j))))))
                     (:fresh-line
                       (send forward-to :fresh-line)
                       (setq index 0))
                     (:operation-handled-p
                       (memq arg1 (send stream :which-operations))))))))

(defun read-index ()
  (do ((c)
       (index 0 (+ (* index 10) (digit-char-p c))))
      ((not (digit-char-p (setq c (read-char))))
       index)
    (write-char c)))
