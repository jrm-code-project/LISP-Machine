;;; -*- Mode:LISP; Package:ZWEI; Readtable:CL; Base:10 -*-


;;;Byte Spec

(DEFCOM COM-EVALUATE-byte-spec-AND-REPLACE-INTO-BUFFER
        "Evaluate the current s-expression as a byte specifier and replace
with the proper (BYTE x y) form into the buffer.
The original expression is deleted and the BYTE spec, printed out, replaces it."
        ()
  (com-forward-sexp-no-up)
  (com-backward-sexp-no-up)
  (LET* ((POINT (point))
         (MARK (MARK))
         (STREAM (REST-OF-INTERVAL-STREAM POINT))
         (FORM (READ-OR-BARF STREAM)))
    (MOVE-BP MARK (SEND STREAM :READ-BP))
    (typecase form
      (number
       (let* ((width     (logand form #o77))
              (position  (lsh    form -6.))
              (result (format nil "(BYTE ~D. ~D.)" width position)))
                (if (= (eval (read-from-string result)) form)
                    (progn
                      (WITH-UNDO-SAVE ("Replacement" POINT MARK T)
                        (princ result STREAM)
                        (WITH-BP (END (SEND STREAM :READ-BP) :NORMAL)
                          (DELETE-INTERVAL POINT MARK T)
                          (MOVE-BP POINT END))))
                  (error "UNBYTE error: (UNBYTE ~S) -> ~A" form result))))
      (symbol
       (when (y-or-n-p "~S is a symbol -- edit its definition?" form)
         (edit-definition form T T)))
      (list
       (if (fboundp (setq form (first form)))
           (when (y-or-n-p "~S is a function -- edit its definition?" form)
             (edit-definition form T T))
         (barf "~S is not a function." form)))
      (t (barf "Cannot create a byte specifier from ~S" form))))
  DIS-TEXT)

#| Tests:

;;;These should work:

(ldb #o101 1765.)            ; #o101 -> (BYTE 1. 1.)
(ldb 16. 1765)               ; 16    -> (BYTE 16. 0.)
(ldb #o331 #o7777)           ; #o331 -> (BYTE 25. 3.)

;;;These should offer to edit definition:

(ldb (print-herald) 3.)
(ldb *print-base* 77.)

;;;These should barf:

(ldb 'fucked-up 99.)

(ldb #(1 2 3) 99.)

|#

;; stuff to get list of functions calling specified functions

(defvar *my-callers-list* () "Place to keep callers list for Multiple List Callers Into Buffer.")

(defun my-get-package-to-search ()
  (let* ((x (typein-line-readline "Package to search (default GLOBAL):"))
         (y (if (equal x "") "GLOBAL" (string-upcase x)))
         (pkg (pkg-find-package y)))
    (values pkg y)))

(defcom com-multiple-list-callers-into-buffer
        "Reads the next lisp object as a list of functions and inserts
a table containing names of caller functions."
        ()
  (com-forward-sexp-no-up)                      ;be sure to put point at front of object (list)
  (com-backward-sexp-no-up)
  (let* ((point (point))
         (mark (mark))
         (stream (rest-of-interval-stream point))
         (form (read-or-barf stream))
         (functions form)                       ;functions is form at point
         (callers)
         (pkg)
         (pkg-name)
         )
    (move-bp mark (send stream :read-bp))
    (multiple-value-setq (pkg pkg-name)
      (my-get-package-to-search))               ;get package and name from argument scenerio
    (setq callers
          (setup-zmacs-callers-to-be-edited
            (list-callers
              functions pkg)))
    (setq *my-callers-list*
          (stable-sortcar (mapcar (lambda (x)
                                    (cons (format nil "~S" x) x))
                                  callers)
                          'string-lessp))
    (com-forward-sexp-no-up)
    (format stream "~2%;;Callers in package ~a of functions in the above list are:~%" pkg-name)
    (do ((i 0 (1+ i))
         (len (length *my-callers-list*))
         )
        ((= i len))
      (format stream "  ~a~%" (string-downcase (car (elt *my-callers-list* i)))))
    (format stream "~%")
    )
  dis-text)

#|
;Sitting cursor in front of this list should produce the following output
; when executing the above command (C-M-X Multiple List Callers Into Buffer)
; assuming the necesssary files have been compiled (e.g. ddb;globe)

(user:moveto user:drawto)

;;Callers in package USER of functions in the above list are:
  user::drawto
  user::list-plot-with-types
  user::putdot


;and yet another example...

(si:string-downcase si:string-flipcase)

;;Callers in package ZWEI of functions in the above list are:
  (:method unix-mail-file-mixin :inbox-buffer)
  com-multiple-list-callers-into-buffer
  com-redo
  com-undo
  describe-flavor-1
  get-keywords-from-msg-by-filtering
  write-qwabl


|#



(DEFCOM com-edit-definition-next-sexp "Go to the definition of a specified function.
The name of the function is read from the mini-buffer." ()
  (com-forward-sexp-no-up)                      ;be sure to put point at front of object (list)
  (com-backward-sexp-no-up)
  (let* ((point (point))
         (mark (mark))
         (stream (rest-of-interval-stream point))
         (form (read-or-barf stream))
         )
    (move-bp mark (send stream :read-bp))
    (LET (SPEC STRING EXPLICIT-PACKAGE-P)
      (SETF (VALUES SPEC STRING EXPLICIT-PACKAGE-P)
            (READ-FUNCTION-NAME "Edit definition"
                                form
                                'AARRAY-OK))
      (SETQ SPEC (LIST SPEC))
      (com-forward-sexp-no-up)
      (IF (AND (NOT EXPLICIT-PACKAGE-P) (SYMBOLP (CAR SPEC)))
          (MULTIPLE-VALUE-BIND (THIS-PKG-SYMBOL FOUNDP)
              (INTERN-SOFT (STRING-UPCASE (STRING (CAR SPEC))))
            (IF (AND FOUNDP
                     (NEQ THIS-PKG-SYMBOL (CAR SPEC))
                     (GET THIS-PKG-SYMBOL :SOURCE-FILE-NAME))
                (PUSH THIS-PKG-SYMBOL SPEC))))
      (EDIT-DEFINITION-1 (CAR SPEC) (IF EXPLICIT-PACKAGE-P T SPEC) STRING)))
  DIS-TEXT)


;;functions from SAZ

(defcom com-bury-buffer "" ()
  (WITHOUT-INTERRUPTS
    (DOLIST (W *ALL-ZMACS-WINDOWS*)
      (LET ((HISTORY (SEND W :BUFFER-HISTORY)))
        (APPEND-REMOVE-ON-HISTORY *interval* HISTORY)))
    (SETQ *ZMACS-BUFFER-LIST* (APPEND (REMQ *interval* *ZMACS-BUFFER-LIST*)
                                      (LIST *interval*)))
    (MAKE-BUFFER-CURRENT (first *zmacs-buffer-list*)))
  dis-text)


(defcom com-rotate-buffer-stack "" ()
  (let ((*numeric-arg* (- (length *zmacs-buffer-list*) 3)))     ;weirdo fencepost error!
    (com-select-previous-buffer))
  dis-text)

;;


(defcom com-find-buffer "Find next buffer having name matching first
letters specified in the mini buffer."
        ()
  (let* ((search-name (string-upcase
                        (typein-line-readline
                          "First letters of buffer name to search for:")))
         (slen (length search-name))
         (zlen (length *zmacs-buffer-list*))
         (found nil)
         )
    (do ((i 1 (1+ i))) ((= i zlen))
      (let* ((buff-pnt (elt *zmacs-buffer-list* i))
             (buff-name (string-upcase (format nil "~a" buff-pnt)))
             )
        (when (string-equal buff-name search-name :end2 slen :end1 slen)
          (setq *zmacs-buffer-list* (append
                                      (list buff-pnt)
                                      (remq buff-pnt *zmacs-buffer-list*)))
          (make-buffer-current (car *zmacs-buffer-list*))
          (setq found t)
          (return))))
    (if (not found)
        (barf "No buffer found matching ~S as first letters." search-name)))
  dis-text)


(defun mention-to-user (CTL-STRING &REST ARGS)
  (WHEN CTL-STRING
    (SEND *QUERY-IO* :FRESH-LINE)
    (APPLY #'FORMAT *QUERY-IO* CTL-STRING ARGS)))


(defcom com-electric-shock "?" ()
  (mention-to-user "Bzzzzttttt!!!!!")
  (do-forever
    (beep)
    (if (read-char-no-hang) (return)))
  dis-none)


(defcom com-other-macro-expand-expression "Print macroexpansion of next s-expression.
The result is printed on the screen with GRIND-TOP-LEVEL." ()
  (let ((stream (rest-of-interval-stream (point))))
    (let ((form (read-or-barf stream)))
      (grind-top-level (cl:macroexpand form))))
  dis-none)




(defcom com-kill-all-unmodified-buffers "Flushes all unmodified buffers from *zmacs-buffer-list*
after counting which have not been saved since last edit."
        ()
  (let ((*query-io *standard-output*)
        (zlen (length *zmacs-buffer-list*))
        (list-of-buffers-to-be-killed)
        )
    (dolist (buffer *zmacs-buffer-list*)
      (setq list-of-buffers-to-be-killed
            (append list-of-buffers-to-be-killed
                    (if (send buffer :modified-p)
                        ()
                      (list buffer)))))
    (and (fquery () "There are currently ~d. buffers modified out of ~d. total.
Start deleting the unmodified buffers? "
                 (- zlen (length list-of-buffers-to-be-killed))
                 zlen)
         (dolist (buffer list-of-buffers-to-be-killed)
           (setq *zmacs-buffer-list* (remq buffer *zmacs-buffer-list*))
           (send buffer :kill)))
    (make-buffer-current (car *zmacs-buffer-list*)))
  dis-text)



(defcom com-kill-all-buffers-in-system "Flushes buffers which are part of a specified system."
        ()
  (let* ((system-name (read-system-name "System whose associated buffers need removal:"))
         (system-source-files-list (si:system-source-files system-name si:*source-file-types* nil t))
         (list-of-buffers-to-be-killed)
         )
    (dolist (buffer *zmacs-buffer-list*)
      (let ((buffer-name (format () "~a" (cdar (pathname-defaults *pathname-defaults* buffer)))))
        (dolist (file system-source-files-list)
          (let ((file-name (format () "~a" file)))
            (when (string-equal buffer-name file-name)
              (setq list-of-buffers-to-be-killed
                    (append list-of-buffers-to-be-killed
                            (progn
                              (and (send buffer :modified-p)
                                   (y-or-n-p "Buffer ~a was modified, save it first? " buffer-name)
                                   (save-buffer buffer))
                              (list buffer)))))))))
    (and (fquery () "There are ~d buffers associated with system ~a out of ~d Zmacs buffers.
Begin removing associated buffers? "
                 (length list-of-buffers-to-be-killed)
                 system-name
                 (length *zmacs-buffer-list*))
         (dolist (buffer list-of-buffers-to-be-killed)
           (setq *zmacs-buffer-list* (remq buffer *zmacs-buffer-list*))
           (send buffer :kill)))
    (make-buffer-current (car *zmacs-buffer-list*))
    (format t "Buffers associated with system ~a flushed." system-name))
  dis-text)



(defcom com-kill-all-buffers-in-system-related-directories
        "Flushes buffers which are part of a specified system's related directories."
        ()
  (let* ((system-name (read-system-name "System whose associated buffers need removal:"))
         (system-source-files-list (si:system-source-files system-name si:*source-file-types* nil t))
         (len (length system-source-files-list))
         (dir-list)
         (list-of-buffers-to-be-killed)
         )
    (do ((i 0 (1+ i)) (dir-name)) ((= i len))
      (setq dir-name (format () "~a" (elt system-source-files-list i))
            dir-name (subseq dir-name 0 (position #\; dir-name)))
      (and (not (member dir-name dir-list :test #'equal))
           (y-or-n-p "Release buffers of directory ~a? " dir-name)
           (setq dir-list (append dir-list (list dir-name)))))
    (dolist (buffer *zmacs-buffer-list*)
      (let ((buffer-name (format () "~a" (cdar (pathname-defaults *pathname-defaults* buffer)))))
        (dolist (dir-name dir-list)
          (when (string-equal buffer-name dir-name
                              :end1 (position #\; buffer-name)
                              :end2 (position #\; dir-name))
            (setq list-of-buffers-to-be-killed
                  (append list-of-buffers-to-be-killed
                          (progn
                            (and (send buffer :modified-p)
                                 (y-or-n-p "Buffer ~a was modified, save it first? " buffer-name)
                                 (save-buffer buffer))
                            (list buffer))))
            (return)))))
    (and (fquery () "There are ~d buffers associated with system ~a out of ~d Zmacs buffers.
Begin removing associated buffers? "
                 (length list-of-buffers-to-be-killed)
                 system-name
                 (length *zmacs-buffer-list*))
         (progn
           (dolist (buffer list-of-buffers-to-be-killed)
             (setq *zmacs-buffer-list* (remq buffer *zmacs-buffer-list*))
             (send buffer :kill))
           (mention-to-user "~d buffers associated with system ~a flushed."
                            (length list-of-buffers-to-be-killed)
                            system-name)))
    (make-buffer-current (car *zmacs-buffer-list*)))
  dis-text)
