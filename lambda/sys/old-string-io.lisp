;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:8; Readtable:ZL -*-
;;; STRING-IO stream handler.

;;;>>> I removed this from QMISC.LISP; it was, and still is, all
;;;>>> commented out anyway.  I suppose it's being saved for posterity.
;;;>>> --Keith 23-oct-88

;;; Note that DEFSELECT doesn't work in the cold load.
;;; WITH-INPUT-FROM-STRING and WITH-OUTPUT-FROM-STRING used to compile into calls to this.
;;; It is now obsolete, but present for the sake of old compiled code.

;;; Supported operations:
;;; :READ-CHAR, :WRITE-CHAR, :STRING-OUT, :LINE-OUT, :FRESH-LINE, :READ-POINTER
;;;    -- these are normal
;;; :SET-POINTER
;;;    -- This works to any location in the string.  If done to an output string,
;;;     and it hasn't gotten there yet, the string will be extended.  (The elements
;;;     in between will contain garbage.)
;;; :UNTYI
;;;    -- you can UNTYI as many characters as you like.  The argument is ignored.
;;; :READ-CURSORPOS, :INCREMENT-CURSORPOS
;;;    -- These work on the X axis only; they ignore Y.
;;;     They are defined only for :CHARACTER units; :PIXEL will give an error.
;;; :UNTYO, :UNTYO-MARK
;;;    -- These exist to keep the grinder happy.
;;; :CONSTRUCTED-STRING
;;;    -- This is a special operation required by the operation of the WITH-OPEN-STRING macro.
;;;     This is how the string is extracted from the stream closure.
;;;     You shouldn't need to use this.

;(defvar *string-io-string*)
;(defvar *string-io-index*)
;(defvar *string-io-limit*)
;(defvar *string-io-direction*)
;(defvar *string-io-stream*)

;(defmacro maybe-grow-io-string (index)
;  `(if ( ,index *string-io-limit*)
;       (adjust-array-size *string-io-string*
;                         (setq *string-io-limit* (fix (* (1+ ,index) 1.5s0))))))

;(defmacro string-io-add-character (ch)
;  `(progn (maybe-grow-io-string *string-io-index*)
;         (setf (char *string-io-string* *string-io-index*) ,ch)
;         (incf *string-io-index*)))

;(defmacro string-io-add-line (string start end)
;  `(let* ((string-io-length (- ,end ,start))
;         (string-io-finish-index (+ *string-io-index* string-io-length)))
;     (maybe-grow-io-string string-io-finish-index)
;     (copy-array-portion ,string ,start ,end
;                        *string-io-string* *string-io-index* string-io-finish-index)
;     (setq *string-io-index* string-io-finish-index)))

;(defselect (string-io string-io-default-handler)
;  (:tyi (&optional eof)
;       (if (< *string-io-index* *string-io-limit*)
;           (prog1 (zl:aref *string-io-string* *string-io-index*)
;                  (incf *string-io-index*))
;         (and eof (ferror 'sys:end-of-file-1 "End of file on ~S." *string-io-stream*))))
;  (:read-char ()
;    (if (< *string-io-index* *string-io-limit*)
;       (prog1 (char *string-io-string* *string-io-index*)
;              (incf *string-io-index*))
;      nil))
;  ((:untyi :unread-char) (ignore)
;   (if (minusp (decf *string-io-index*))
;       (error "Attempt ~S past beginning -- ~S" :unread-char 'string-io)))
;  ((:write-char :tyo) (ch)
;   (string-io-add-character ch))
;  (:string-out (string &optional (start 0) end)
;    (or end (setq end (length string)))
;    (string-io-add-line string start end))
;  (:line-out (string &optional (start 0) end)
;    (or end (setq end (length string)))
;    (string-io-add-line string start end)
;    (string-io-add-character #/Newline))
;  (:fresh-line ()
;    (and (plusp *string-io-index*)
;        ( (char *string-io-string* *string-io-index*) #/Newline)
;        (string-io-add-character #/Newline)))
;  (:read-pointer ()
;    *string-io-index*)
;  (:set-pointer (ptr)
;    (and (neq *string-io-direction* :in)
;        (< ptr *string-io-limit*)
;        (error "Attempt to ~S beyond end of string -- ~S" :set-pointer 'string-io))
;    (setq *string-io-index* ptr))
;  (:untyo-mark ()
;    *string-io-index*)
;  (:untyo (mark)
;    (setq *string-io-index* mark))
;  (:read-cursorpos (&optional (units :pixel))
;    (string-io-confirm-movement-units units)
;    (let ((string-io-return-index
;           (string-reverse-search-char #/Newline *string-io-string* *string-io-index*)))
;      (if string-io-return-index
;         (- *string-io-index* string-io-return-index)
;       *string-io-index*)))
;  (:increment-cursorpos (x ignore &optional (units :pixel))
;    (string-io-confirm-movement-units units)
;    (dotimes (i x) (string-io-add-character #/Space)))
;  (:constructed-string ()
;    ;; Don't change allocated size if we have a fill pointer!
;    (if (array-has-fill-pointer-p *string-io-string*)
;       (setf (fill-pointer *string-io-string*) *string-io-index*)
;      (setq *string-io-string*
;           (adjust-array-size *string-io-string* *string-io-index*)))))

;(defun string-io-default-handler (op &optional arg1 &rest rest)
;  (stream-default-handler 'string-io op arg1 rest))

;(defun string-io-confirm-movement-units (units)
;  (if (neq units :character)
;      (ferror "Unimplemented cursor-movement unit ~A -- STRING-IO." units)))
