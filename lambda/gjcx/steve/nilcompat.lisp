;;-*- Mode:LISP; Package:STEVE-SI; Base:10; Readtable:CL -*-

;; in doing compat package for first-time ports it is
;; important to not directly translate %mumble things
;; into super efficient versions until the port is complete.
;; %versions used here only if they are also known to be safe.

;; first thing to fix

(defsubst 0p (x)
  (zerop x))

(defsubst -p (x)
  (minusp x))

(defsubst +p (x)
  (plusp x))

(defsubst <& (&rest args)
  (apply #'< args))

(defsubst >& (&rest args)
  (apply #'> args))

(defsubst =& (&rest args)
  (apply #'= args))


(defsubst >=& (&rest args)
  (apply #'>= args))

(defsubst <=& (&rest args)
  (apply #'<= args))


(defsubst +& (&rest args)
  (apply #'+ args))

(defsubst -& (&rest args)
  (apply #'- args))

(defsubst *& (&rest args)
  (apply #'* args))

(defsubst /& (&rest args)
  (apply #'QUOTIENT args))

(defsubst 1+& (x)
  (1+ x))

(defsubst 1-& (x)
  (1- x))

(defsubst \\& (x y)
  (\\ x y))

(defsubst make-vector (dims &rest l)
  (apply #'make-array dims l))

(eval-when (eval compile load)

(defpackage *
  (:use))

)


(defconst *:min-fixnum most-negative-fixnum)

(defconst *:character-to-string-table
          (do ((a (make-array 256))
               (j 0 (1+ j)))
              ((= j 256)
               a)
            (setf (aref a j) (string (code-char j)))))

(defconst *:character-upcase-table (do ((a (make-string 256))
                                        (j 0 (1+ j)))
                                       ((= j 256)
                                        a)
                                     (setf (aref a j) (char-upcase (code-char j)))))



(defsubst min& (&rest l)
  (apply #'min l))

(defsubst max& (&rest l)
  (apply #'max l))

(defsubst logand& (&rest l)
  (apply #'logand l))


;;; these were in newmacros.lisp
;;; which didnt have correct order of compilation,
;;; so i stuck 'em here instead

;;; Some faster predicates (should definitely go elsewhere)

(defmacro zerop& (n)
  `(=& ,n 0))

(defmacro plusp& (n)
  `(>& ,n 0))

(defmacro minusp& (n)
  `(<& ,n 0))


(or (assq :bell si:xr-special-character-names)
    (push (cons :bell 7) si:xr-special-character-names))



(defsubst logior& (&rest l)
  (apply #'logior l))

(defsubst logandc2& (&rest l)
  (apply #'logandc2 l))

(defsubst logandc1& (&rest l)
  (apply #'logandc1 l))

(defsubst logbitp& (index integer)
  (logbitp index integer))


(defsubst ^& (x y)
  (^ x y))

(defun address-of (x)
  ;; used only in print self messages anyway.
  (%pointer x))

(defun make-extend (size flavor)
  ;; used instead of make-instance for efficient consing
  (OR (SI:FLAVOR-DEPENDS-ON-ALL FLAVOR)
      (SI:COMPOSE-FLAVOR-COMBINATION FLAVOR))
  (OR (SI:FLAVOR-METHOD-HASH-ARRAY FLAVOR)
      (SI:COMPOSE-METHOD-COMBINATION FLAVOR))
  (check-arg size (= (1+ size) (si:flavor-instance-size flavor)) "size of this flavor")
  ;; this is actually safe given the above. otherwise extremely dangerous
  (si:%make-structure dtp-instance dtp-instance-header
                      flavor nil default-cons-area
                      (1+ size)
                      (1+ size)))

(defsubst graphic-charp (x)
  (graphic-char-p x))

(defun to-string (x)
  (coerce x 'string))


(defsubst logbitp& (x y)
  (logbitp x y))

(defun make-bits (n)
  (make-array n :type 'art-1b))


(defsubst string-charp (x)
  (zerop (logand x #o377)))

(defun oustr (string stream &optional start count)
  (send stream :string-out string
        (or start 0)
        (if count
            (+ start count)
          (length string))))

(defun %string-replace (dest source dest-offset source-offset count)
  ;; could call REPLACE here. but wont improve code any to do so.
  (dotimes (j count)
    (setf (aref dest (+ dest-offset j)) (aref source (+ source-offset j))))
  dest)


(defsubst %string-posq (char string offset count)
  (%string-search-char char string offset (+ offset count)))


(defsubst extendp (x)
  (instancep x))

(defsubst vector-length (x)
  (length x))

(defsubst bits-length (x)
  (length x))


(defun bitsp (x)
  (and (typep x 'array)
       (= 1 (array-rank x))
       (eq 'art-1b (array-type x))))

; in lmmac
;(defsubst bit (x j)
;  (aref x j))


(defsubst TO-CHARACTER (x)
  (coerce x 'character))

(defsubst pairp (x)
  (consp x))

; in lmmac
;(defsubst char (string j)
;  (aref string j))



(defsubst RPLACHAR (string j c)
  (si:set-aref string j c))


(defsubst bits-replace (dest source dest-offset source-offset count)
  (%string-replace dest source dest-offset source-offset count))


(defsubst RESET-FILL-POINTER (object k)
  (setf (fill-pointer object) k))





(defflavor output-stream
         ()
         (si:output-stream))

(defmethod (output-stream :tyo) (c)
  (send self :write-char (code-char c)))

(DEFFLAVOR INPUT-STREAM
         ()
         (SI:INPUT-STREAM))


(DEFMETHOD (INPUT-STREAM :TYI) ()
  (CHAR-CODE (SEND SELF :READ-CHAR)))


(DEFMETHOD (INPUT-STREAM :UNTYI) (C)
  (SEND SELF :UNREAD-CHAR (CODE-CHAR C)))

(defsubst vref (v j)
  (aref v j))


(defun %string-eqv (string1 string2 offset1 offset2 count)
  (%string-equal string1 offset1 string2 offset2 count))


(pushnew (cons (intern "&RESTV" "STEVE")
               '&REST)
         (getf (si:rdtbl-plist readtable) 'si:symbol-substitutions)
         :test 'equal)

(pushnew (cons (intern "&RESTL" "STEVE")
               '&REST)
         (getf (si:rdtbl-plist readtable) 'si:symbol-substitutions)
         :test 'equal)


(defsubst sgvref (x j)
  (aref x j))


(defsubst lowercasep (char)
  (<= #\a char #\z))


(defsubst simple-string-length (x)
  (length x))


(defsubst of-type (obj type)
  (typep obj type))


(DEFUN PATHNAME (X)
  (GLOBAL:PATHNAME (COND ((AND (STRINGP X)
                               (STRING-EQUAL "NIL$DISK:[NIL.STEVE]"
                                             X
                                             :END2 (LENGTH "NIL$DISK:[NIL.STEVE]")))
                          (STRING-APPEND "DJ:GJCX.STEVE;"
                                         (SUBSTRING X (LENGTH "NIL$DISK:[NIL.STEVE]"))))
                         ('ELSE
                          X))))

(DEFPROP PATHNAME GLOBAL:PATHNAME SI:TYPE-ALIAS-FOR)


(DEFPROP NIL-TERMINAL T SI:IO-STREAM-P)

(DEFUN NIL-TERMINAL (OP &REST ARGS)
  (LET ((F (GET OP 'NIL-TERMINAL)))
    (IF F
        (APPLY F ARGS)
      (APPLY *TERMINAL-IO* OP ARGS))))

(DEFUN (:READ-CURSORPOS NIL-TERMINAL) ()
  (SEND *TERMINAL-IO* :READ-CURSORPOS :CHARACTER))

(DEFUN (:SET-CURSORPOS NIL-TERMINAL) (X Y)
  (SEND *TERMINAL-IO* :SET-CURSORPOS X Y :CHARACTER))


(defun (:pagel nil-terminal) ()
  (nth-value 1 (send *TERMINAL-IO* :size-in-characters)))


(defun (:LINEL nil-terminal) ()
  (nth-value 0 (send *TERMINAL-IO* :size-in-characters)))

(defun (:GET-DEVICE-MODE nil-terminal) (key)
  (caseq key
    (:passall nil)))


(defun (:SET-DEVICE-MODE nil-terminal) (key value)
  value
  (caseq key
    (:passall
     nil)))


(defun (:oustr nil-terminal) (string &optional (start 0) (count (- (length string) start)))
  (send *terminal-io*
        :string-out string start (+ start count)))


(defun (:RAW-oustr nil-terminal) (string &optional (start 0) (count (- (length string) start)))
  (send *terminal-io*
        :string-out string start (+ start count)))


(DEFUN (:SET-CURSOR-FRIED-FLAG NIL-TERMINAL) (X)
  X)


(defun (:hpos nil-terminal) ()
  (MULTIPLE-VALUE-BIND (X Y) (SEND *terminal-io* :READ-CURSORPOS ':CHARACTER)
    y
    x))


(defun (:width nil-terminal) ()
  (nth-value 0 (send *TERMINAL-IO* :size-in-characters)))


(defun (:tyi nil-terminal) ()
  (let ((c (send *terminal-io* :tyi)))
    (cond ((char-bit c :control)
           (logxor #o100 (char-upcase (set-char-bit c :control nil))))
          ('else
           (logand #o377 c)))))

(DEFUN (:UNTYI NIL-TERMINAL) (C)
  (COND ((AND (< C #\SPACE) (NOT (= C #\ALT)))
         (SEND *TERMINAL-IO* :UNTYI (SET-CHAR-BIT (LOGXOR #o100 C) :CONTROL T)))
        ('ELSE
         (SEND *TERMINAL-IO* :UNTYI C))))



(defun (:read-char nil-terminal) ()
  (code-char (FUNCALL (GET ':TYI 'NIL-TERMINAL))))


(DEFUN (:UNREAD-CHAR NIL-TERMINAL) (C)
  (FUNCALL (GET :UNTYI 'NIL-TERMINAL) (CHAR-CODE C)))


(DEFUN (:LINENUM NIL-TERMINAL) ()
  (1+ (nth-value 1 (send *TERMINAL-IO* :READ-CURSORPOS :CHARACTER))))



(SPECIAL TERMINAL-IO standard-output standard-input)

(SETQ TERMINAL-IO 'NIL-TERMINAL)

(setq standard-input (make-synonym-stream 'terminal-io))

(setq standard-output (make-synonym-stream 'terminal-io))



;; lisp cursorpos, maclisp compatibility function (should get nil one)
;; hacked to send nil like message.

(DEFUN CURSORPOS (&REST ARGS)
  (LET ((NARGS (LENGTH ARGS))
        (STREAM STANDARD-OUTPUT)
        ARG1 WO)
    (COND ((NULL ARGS))                 ;If any args, look for stream as 1st arg
          ((EQ (SETQ ARG1 (CAR (LAST ARGS))) T) (SETQ STREAM TERMINAL-IO NARGS (1- NARGS)))
          ((OR (NUMBERP ARG1) (NULL ARG1)))
          ((OR (NOT (SYMBOLP ARG1)) (> (LENGTH (SYMBOL-NAME ARG1)) 1))
           (SETQ STREAM ARG1 NARGS (1- NARGS))))
    (SETQ ARG1 (CAR ARGS)
          WO (SEND STREAM :WHICH-OPERATIONS))
    (COND ((ZEROP NARGS)
           (IF (MEMQ ':READ-CURSORPOS WO)
               (MULTIPLE-VALUE-BIND (X Y) (SEND STREAM :READ-CURSORPOS)
                 (CONS Y X))
               (FERROR NIL "~S stream does not support cursor positioning" STREAM)))
          ((> NARGS 2)
           (FERROR NIL "Too many arguments"))   ;Why bother signalling the correct condition?
          ((OR (> NARGS 1) (NUMBERP ARG1))      ;2 arguments or one numeric argument
           (IF (MEMQ ':SET-CURSORPOS WO)
               (MULTIPLE-VALUE-BIND (X Y)
                   (SEND STREAM :READ-CURSORPOS)
                 (SEND STREAM :SET-CURSORPOS
                              (OR (SECOND ARGS) X) (OR (FIRST ARGS) Y)))
             (FERROR "~S stream does not support cursor positioning" STREAM)))
          ((= (SETQ ARG1 (CHAR-INT (CHAR-UPCASE (CHAR (SYMBOL-NAME ARG1) 0)))) #\F)
           (CURSORPOS-INTERNAL STREAM +1 0 #\SP WO))    ;F forward space
          ((= ARG1 #\B)                         ;B backspace
           (CURSORPOS-INTERNAL STREAM -1 0 #\BS WO))
          ((= ARG1 #\D)                         ;D down a line
           (CURSORPOS-INTERNAL STREAM 0 +1 NIL WO))
          ((= ARG1 #\U)                         ;U up a line
           (CURSORPOS-INTERNAL STREAM 0 -1 NIL WO))
          ((= ARG1 #\C)                         ;C clear screen
           (IF (MEMQ :CLEAR-SCREEN WO)
               (SEND STREAM :CLEAR-SCREEN)
             (SEND STREAM :TYO #\FORM))
           T)
          ((= ARG1 #\T)                         ;T top of screen
           (IF (MEMQ ':HOME-CURSOR WO)
               (SEND STREAM :HOME-CURSOR)
             (SEND STREAM :TYO #\FORM))
           T)
          ((= ARG1 #\E)                         ;E erase to end of screen
           (WHEN (MEMQ :CLEAR-EOF WO)
             (SEND STREAM :CLEAR-EOF)
             T))
          ((= ARG1 #\L)                         ;L erase to end of line
           (WHEN (MEMQ :CLEAR-EOL WO)
             (SEND STREAM :CLEAR-EOL)
             T))
          ((= ARG1 #\K)                         ;K erase character
           (WHEN (MEMQ ':CLEAR-CHAR WO)
             (SEND STREAM :CLEAR-CHAR)
             T))
          ((= ARG1 #\X)                         ;X erase character backward
           (CURSORPOS 'B STREAM)
           (CURSORPOS 'K STREAM))
          ((= ARG1 #\Z)                         ;Z home down
           (IF (MEMQ ':HOME-DOWN WO)
               (SEND STREAM :HOME-DOWN)
             (SEND STREAM :FRESH-LINE))
           T)
          ((= ARG1 #\A)                         ;A fresh line
           (SEND STREAM :FRESH-LINE)
           T)
          ((FERROR "~C not a recognized CURSORPOS option" ARG1)))))

(DEFUN CURSORPOS-INTERNAL (STREAM DX DY ALTERNATE-CHARACTER WO)
  (COND ((MEMQ :SET-CURSORPOS WO)
         (MULTIPLE-VALUE-BIND (X Y)
             (SEND STREAM :READ-CURSORPOS)
           (SEND STREAM :SET-CURSORPOS (+ X DX) (+ Y DY))
           T))
        ((NOT (NULL ALTERNATE-CHARACTER))
         (SEND STREAM :TYO ALTERNATE-CHARACTER)
         T)
        (T NIL)))       ;Or should it give an error?



;;~Z IS HEX I THINK
(FORMAT:DEFFORMAT FORMAT:Z (:ONE-ARG) (ARG PARAMS)
  (FORMAT:FORMAT-CTL-ASCII ARG PARAMS T))


(DEFUN MERGE-PATHNAME-DEFAULTS (PATHNAME &REST ARGS)
  (APPLY #'FS:MERGE-PATHNAME-DEFAULTS PATHNAME ARGS))

(DEFUN USER-WORKINGDIR-PATHNAME ()
  (CDAR FS:LOAD-PATHNAME-DEFAULTS))



(DEFMACRO LEXPR-FUNCALL (F &REST ARGS)
  (IF ARGS
      `(GLOBAL:LEXPR-FUNCALL ,F
                             ,@(BUTLAST ARGS)
                             (VECTOR-TO-LIST ,(CAR (LAST ARGS))))
    `(FUNCALL ,F)))


(DEFUN VECTOR-TO-LIST (X)
  (COND ((NOT X) ())
        ((CONSP X) X)
        ((VECTORP X)
         (LISTARRAY X))
        ('ELSE
         (FERROR NIL "WRONG TYPE LAST ARG TO LEXPR-FUNCALL"))))


(defun depends-on-a-macro (&rest list)
  "Return a list of modules that depend on a given macro"
  (mapcar #'(lambda (p) (send p :name))
          (subset #'(lambda (p)
                      (mem #'(lambda (item elem)
                               (memq (if (atom elem) elem (car elem))
                                     item))
                           list
                           (get (send p :generic-pathname) :macros-expanded)))
                  (si:system-source-files 'STEVE))))

(DEFSUBST BIT1P (VECTOR INDEX)
  (NOT (ZEROP (AREF VECTOR INDEX))))


(DEFUN STRING-EQUAL (STRING1 STRING2 &OPTIONAL IDX1 IDX2 LIM1 LIM2)
  (OR IDX1 (SETQ IDX1 0))
  (OR IDX2 (SETQ IDX2 0))
  (SI:COERCE-STRING-ARG STRING1)
  (SI:COERCE-STRING-ARG STRING2)
  (COND ((OR LIM1 LIM2)
         (OR LIM1 (SETQ LIM1 (LENGTH STRING1)))
         (OR LIM2 (SETQ LIM2 (LENGTH STRING2)))
         (AND (= (SETQ LIM1 (- LIM1 IDX1)) (- LIM2 IDX2))
              (%STRING-EQUAL STRING1 IDX1 STRING2 IDX2 LIM1)))
        (T (%STRING-EQUAL STRING1 IDX1 STRING2 IDX2 NIL))))

(DEFUN GET-A-BYTE (BITVECTOR INDEX)
  (DO ((J 0 (1+ J))
       (N 0 (DPB (AREF BITVECTOR (+ J (* INDEX 8))) (BYTE 1 J) N)))
      ((= J 8)
       N)))


(DEFUN STRING-SUBSEQ (STRING &OPTIONAL (START 0) (COUNT (- (LENGTH STRING) START)))
  (SUBSTRING STRING START (+ START COUNT)))
