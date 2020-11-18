
;;;  This is an attemp to generate sequence functions along with optimised
;;; versions and rewriters using some hairy macros.  It doesn't seem to be
;;; especially winning.
;;;
;;; EFH 10/6/87



;--------------------------------------------------------------------------------
;;; Sequence Search Macros

(defmacro do-vector-seq (seq from-end start end value-thunk body-thunk)
  `(LET ((SEQ ,seq)
         (FROM-END ,from-end)
         (VALUE-THUNK ,value-thunk)
         (BODY-THUNK ,body-thunk))
     (LET ((START ,(if start start 0))
           (END ,(if end
                     `(MIN ,end (LENGTH SEQ))
                   `(LENGTH SEQ)))
           (INC (IF FROM-END -1 1)))
       ,@(if from-end `((IF FROM-END (PSETQ START END END START))))
       (DO ((I START (+ I INC)))
           ((= I END) (FUNCALL VALUE-THUNK))
         (FUNCALL BODY-THUNK (AREF SEQ I))))))

(defmacro do-list-seq-fd (seq from-end start end value-thunk body-thunk)
  `(LET ((SEQ ,seq)
         (VALUE-THUNK ,value-thunk)
         (BODY-THUNK ,body-thunk))
     (LET (,@(if end `((END ,end))))
       (DO ((L ,(if start `(NTHCDR ,start SEQ) 'SEQ) (CDR L))
            ,@(if end `((N ,(if start `(- END START) 'END)
                           (1- N)))))
           ((OR (NULL L) ,@(if end '((<= N 0))))
            (FUNCALL VALUE-THUNK))
         (FUNCALL BODY-THUNK (CAR L))))))


;;; *SEQ-TEST*, *SEQ-TEST-NOT* and *SEQ-KEY* contain knowledge at macro expand time about the
;;; state of the :TEST, :TEST-NOT and :KEY keyword arguments.  They are used to optimise
;;; the expansion of SEQ-TEST and SEQ-KEY.
(defvar *seq-test*)
(defvar *seq-test-not*)
(defvar *seq-key*)


(eval-when (compile load)

;;; Return T if F specifies a constant function
(defun constant-function-p (f)
  (and (consp f)
       (or (eq (car f) 'FUNCTION)
           ;; Common Lisp may change to make this illegal
           (eq (car f) 'QUOTE))))
)

;;; SEQ-TEST
;;; This macro generates a test of a sequence element against another object.
;;; Sequence functions which test elements will use this and the default test
;;; is EQL.  A test other than EQL can be specified by the :TEST or :TEST-NOT
;;; keyword.  It is an error to use both of these keywords in the same call.
;;; The knowledge we have about the :TEST and :TEST-NOT arguments are in the
;;; special variables *SEQ-TEST* and *SETQ-TEST-NOT* at macroexpand time.
;;; There are several possibilities:
;;; * If both :TEST and :TEST-NOT are known to be NIL or unspecified
;;;   then EQL is used as the test.
;;; * If one of :TEST or :TEST not is known to be nil or unspecified
;;;   then if the other is non-nil it is FUNCALLed
;;;        else EQL is used
;;; * If both :TEST and :TEST-NOT are unknown then test both
;;;   if one is non-null FUNCALL it
;;;   or use EQL if both are null
(defmacro seq-test (x y)
  (if (null *seq-test-not*)
      (if *seq-test*
          (if (constant-function-p *seq-test*)
              `(FUNCALL ,*seq-test* ,x ,y)
            `(LET ((X ,x) (Y ,y))
               (IF ,*seq-test*
                   (FUNCALL ,*seq-test* X Y)
                 (EQL X Y))))
        `(EQL ,x ,y))
    (if (null *seq-test*)
        (if (constant-function-p *seq-test-not*)
            `(NOT (FUNCALL ,*seq-test-not* ,x ,y))
          `(LET ((X ,x) (Y ,y))
             (IF ,*seq-test-not*
                 (NOT (FUNCALL ,*seq-test-not* X Y))
               (EQL X Y))))
      `(LET ((X ,x) (Y ,y))
         (COND (,*seq-test*
                (FUNCALL ,*seq-test* X Y))
               (,*seq-test-not*
                (NOT (FUNCALL ,*seq-test-not* X Y)))
               (T (EQL X Y)))))))


;;; SEQ-KEY
;;; This macro generates the acces to a sequence element.
;;; If the keyword argument :KEY is supplied to a sequence function, it should
;;; be a function of one argument which is applied to the element before it is tested.
;;; The knowledge we have about the :KEY argument is in the special variable
;;; *SEQ-KEY* at macroexpand time.
;;; (SEQ-KEY <elt>) can expand into three things:
;;; 1. If :KEY is known to be a constant function ==> (FUNCALL <key> <elt>)
;;; 2. If :KEY is unknown                         ==> (IF <key> (FUNCALL <key> <elt>))
;;; 3. If :key is known to be nil or unspecified  ==> <elt>
(defmacro seq-key (elt)
  (if *seq-key*
      (if (constant-function-p *seq-key*)
          `(FUNCALL ,*seq-key* ,elt)
        `(IF ,*seq-key* (FUNCALL ,*seq-key* ,elt) ,elt))
    elt))

(defmacro do-sequence (seq type from-end test test-not start end key value-thunk body-thunk)
  `(COMPILER-LET ((*SEQ-TEST* ',test)
                  (*SEQ-TEST-NOT* ',test-not)
                  (*SEQ-KEY* ',key))
     (LET ((.SEQ. ,seq))
       ,(case type
          (VECTOR `(DO-VECTOR-SEQ .SEQ. ,from-end ,start ,end ,value-thunk ,body-thunk))
          (LIST `(DO-LIST-SEQ-FD .SEQ. ,from-end ,start ,end ,value-thunk ,body-thunk))
          (t `(COND ((ARRAYP .SEQ.)
                     (DO-VECTOR-SEQ .SEQ. ,from-end ,start ,end ,value-thunk ,body-thunk))
                    ((CONSP .SEQ.)
                     (DO-LIST-SEQ-FD .SEQ. ,from-end ,start ,end ,value-thunk ,body-thunk))
                    ((NULL .SEQ.) (FUNCALL ,value-thunk))
                    (T (error "~s is not a sequence" .SEQ.))))))))

;(defun count-eql (item seq)
;  (let ((n 0))
;    (do-sequence seq t
;                 nil
;                 #'eql nil
;                 nil nil
;                 nil
;                 #'(lambda () n)
;                 #'(lambda (elt)
;                     (when (seq-test item (seq-key elt))
;                       (incf n))))))

;(defun count (item seq &key from-end (test #'eql) test-not start end key)
;  (let ((n 0))
;    (do-sequence seq t
;                 from-end
;                 test test-not
;                 start end
;                 key
;                 #'(lambda () n)
;                 #'(lambda (elt)
;                     (when (seq-test item (seq-key elt))
;                       (incf n))))))

(defun seq-predicate-1 (pred seq test-value stop-value end-value)
  (do-sequence seq t
               nil nil
               nil nil
               nil
               #'(lambda () end-value)
               #'(lambda (elt)
                   (let ((result (funcall pred elt)))
                     (when (if test-value result (not result))
                       (return-from seq-predicate-1 stop-value))))))


(defvar *seq-options*)
(defvar *seq-optimiser-options*)

(defun arg-match (args lambda-list key-options)
  ;; hack &-keywords in lambda-list?
  (do ((args (nthcdr (length lambda-list) args) (cddr args)))
      ((null args) t)
    (let ((key (first args))
          (value (second args)))
      (unless (and (keywordp key)
                   (let ((opl (member key key-options)))
                     (and opl
                          (equal value (second opl)))))
        (return nil)))))

(defun try-to-optimise-sequence-function-call (form optimisations)
  (let ((args (cdr form)))
    (dolist (op optimisations form)
      (when (arg-match args (second op) (cddr op))
        (return (cons (first op) args))))))

(defmacro define-sequence-fcn (name lambda-list optimisations &body body)
  (let ((options (mapcar #'(lambda (key)
                             (cond ((symbolp key)
                                    `(,(intern (symbol-name key) 'keyword)
                                      ,key
                                      NIL))
                                   ((consp key)
                                    `(,(intern (symbol-name (first key)) 'keyword)
                                      ,(first key)
                                      ,(second key)))
                                   (t (error "lose"))))
                             (cdr (member '&key lambda-list)))))
    `(COMPILER-LET ((*SEQ-OPTIONS* ',options))
       (DEFREWRITE ,name (&WHOLE FORM)
         (TRY-TO-OPTIMISE-SEQUENCE-FUNCTION-CALL FORM ',optimisations))
       (DEFUN ,name ,lambda-list
         (COMPILER-LET ((*SEQ-OPTIMISER-OPTIONS* nil))
           ,@body))
       ,@(mapcar #'(lambda (optimisation)
                     (let ((op-name        (first optimisation))
                           (op-lambda-list (second optimisation))
                           (op-options     (cddr optimisation)))
                       `(DEFUN ,op-name ,op-lambda-list
                          (COMPILER-LET ((*SEQ-OPTIMISER-OPTIONS* ',op-options))
                            (LET ()
                              ,@body)))))
                 optimisations))))


(defmacro seq-body (sequence value-thunk body-thunk)
  (labels ((get-op (op)
             (let ((opl (member op *seq-optimiser-options*)))
               (if opl
                   (second opl)
                 (second (assoc op *seq-options*))))))
    `(DO-SEQUENCE ,sequence t ,(get-op :from-end)
                  ,(get-op :test) ,(get-op :test-not)
                  ,(get-op :start) ,(get-op :end)
                  ,(get-op :key)
                  ,value-thunk ,body-thunk)))


;--------------------------------------------------------------------------------

;;; ****** from-end with lists doesn't work yet *****

(defun count-1 (item list &optional (start 0) end test invertp key one-arg-predicate)
  (let ((n 0))
    (do ((i start (1+ i))
         (l (nthcdr start list)
            (cdr l)))
        ((or (null l) (and end (>= i end))))
      (let ((elt (if key (funcall key (car l)) (car l))))
        (when (eq invertp (not (cond (one-arg-predicate
                                      (funcall one-arg-predicate elt))
                                     (test
                                      (funcall test elt item))
                                     (t
                                      (eql elt item)))))
          (incf n))))
    n))

(define-sequence-fcn count (item sequence &key from-end (test #'eql) test-not start end key)
         ((count-eql (item sequence) :from-end nil :test #'eql :test-not nil :start nil :end nil :key nil)
          )
  (let ((n 0))
    (seq-body sequence
              #'(lambda () n)
              #'(lambda (elt)
                  (when (seq-test item (seq-key elt))
                    (incf n))))))

(define-sequence-fcn find (item sequence &key from-end (test #'eql) test-not start end key)
         ((find-eql (item sequence) :from-end nil :test #'eql :test-not nil :start nil :end nil :key nil)
          )
  (block found
    (seq-body sequence
              #'(lambda () NIL)
              #'(lambda (elt)
                  (when (seq-test item (seq-key elt))
                    (return-from found elt))))))

(define-sequence-fcn find-if (test sequence &key from-end start end key)
         ((find-if-nokey (test sequence) :from-end nil :start nil :end nil :key nil)
          )
  (block found
    (seq-body sequence
              #'(lambda () NIL)
              #'(lambda (elt)
                  (when (funcall test (seq-key elt))
                    (return-from found elt))))))

(define-sequence-fcn find-if-not (test sequence &key from-end start end key)
         ((find-if-not-nokey (test sequence) :from-end nil :start nil :end nil :key nil)
          )
  (block found
    (seq-body sequence
              #'(lambda () NIL)
              #'(lambda (elt)
                  (when (not (funcall test (seq-key elt)))
                    (return-from found elt))))))


(define-sequence-fcn position (item sequence &key from-end (test #'eql) test-not start end key)
         ((position-eql (item sequence) :from-end nil :test #'eql :test-not nil :start nil :end nil :key nil)
          )
  (let ((pos (if from-end (length sequence) 0))
        (inc (if from-end -1 1)))
    (block found
      (seq-body sequence
                #'(lambda () NIL)
                #'(lambda (elt)
                    (if (seq-test item (seq-key elt))
                      (return-from found pos)
                      (setq pos (+ pos inc))))))))

(define-sequence-fcn position-if (test sequence &key from-end start end key)
         ((position-if-nokey (test sequence) :from-end nil :start nil :end nil :key nil)
          )
  (let ((pos (if from-end (length sequence) 0))
        (inc (if from-end -1 1)))
    (block found
      (seq-body sequence
                #'(lambda () NIL)
                #'(lambda (elt)
                    (if (funcall test (seq-key elt))
                      (return-from found pos)
                      (setq pos (+ pos inc))))))))

(define-sequence-fcn position-if-not (test sequence &key from-end start end key)
         ((position-if-not-nokey (test sequence) :from-end nil :start nil :end nil :key nil)
          )
  (let ((pos (if from-end (length sequence) 0))
        (inc (if from-end -1 1)))
    (block found
      (seq-body sequence
                #'(lambda () NIL)
                #'(lambda (elt)
                    (if (not (funcall test (seq-key elt)))
                      (return-from found pos)
                      (setq pos (+ pos inc))))))))
