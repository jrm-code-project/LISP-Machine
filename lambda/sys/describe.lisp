;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:10; Readtable:CL -*-

;This stuff used to be largely in sys;qmisc

; (defun describe (frob) (send frob :describe 0)) Sigh...

(defvar *describe-print-level* 2
  "Value of *PRINT-LEVEL* to use in DESCRIBE")
(defvar *describe-print-length* 3
  "Value of *PRINT-LENGTH* to use in DESCRIBE")
;(defvar *describe-print-pretty* nil
;  "Value of *PRINT-PRETTY* to use in DESCRIBE")

;; plebian-lisp says that this should return (values).  Tough.
;;; Describe anything
(defun describe (anything &optional no-complaints)
  "Describe the value or components of any Lisp object.
This is a good way to find out more than the printed representation says."
  (unless (and (named-structure-p anything)
               (cond ((and (get (named-structure-p anything) 'named-structure-invoke)
                           (memq ':describe
                                 (named-structure-invoke anything :which-operations)))
                      (named-structure-invoke anything :describe)
                      anything)
                     ((get (named-structure-p anything) 'defstruct-description)
                      (describe-defstruct anything)
                      anything)
                     (t nil)))
    (typecase anything
      ((or entity instance)
       (send anything :describe))
      (array
       (describe-array anything))
      (closure
       (describe-closure anything))
      (compiled-function
       (describe-fef anything))
      (symbol
       (describe-symbol anything))
      (cons
       (describe-cons anything))
      (stack-group
       (describe-stack-group anything))
      (short-float
       (describe-small-flonum anything))
      (single-float
       (describe-flonum anything))
      (select
       (describe-select-method anything))
      (character
       (describe-character anything))
      (bignum
       (describe-bignum anything))
      (fixnum
       (format t "~%~R is ~:[even~;odd~]~&It is ~B in binary"
               anything (oddp anything) anything))
      (ratio
       (describe-rational-number anything))
      (complex
       (describe-complex-number anything))
      (locative
       (describe-locative anything))
      (microcode-function
       (describe-microcode-function anything))
      (t (unless no-complaints
           (format t "~%I don't know how to describe ~S" anything)))))
  (send *standard-output* :fresh-line)
  anything)

(defun describe-1 (thing)                       ;An internal subroutine
  (unless (or (null thing)                      ;Don't recursively describe boring things
              (numberp thing) (symbolp thing) (stringp thing)
              (consp thing))
    (send *standard-output* :fresh-line)
    (let ((*standard-output*                    ;Arrange for indentation by 5 spaces
            ;;>> this loses. (loses recursively)
            (closure '(*standard-output*)
                     #'(lambda (&rest args)
                         (and (eq (send *standard-output* :send-if-handles :read-cursorpos)
                                  0)
                              (send *standard-output* :string-out "     "))
                         (lexpr-send *standard-output* args)))))
      (describe thing t))))


(DEFUN DESCRIBE-FEF-ADL (FEF &AUX (ADL (GET-MACRO-ARG-DESC-POINTER FEF)))
  (PROG (OPT-Q INIT-OPTION (ARGNUM 0) (LOCALNUM 0) ARGP
         ARG-SYNTAX)
     L  (IF (NULL ADL) (RETURN NIL))
        (SETQ OPT-Q (CAR ADL) ADL (CDR ADL))
        (SETQ ARG-SYNTAX (NTH (LDB %%FEF-ARG-SYNTAX OPT-Q)
                              FEF-ARG-SYNTAX))
        (SETQ ARGP (MEMQ ARG-SYNTAX
                         '(FEF-ARG-REQ FEF-ARG-OPT FEF-ARG-REST)))
        (COND ((NOT (ZEROP (LOGAND OPT-Q %FEF-NAME-PRESENT)))
               (SETQ ADL (CDR ADL))))
        (COND ((EQ (NTH (LDB %%FEF-ARG-SYNTAX OPT-Q) FEF-ARG-SYNTAX) 'FEF-ARG-REST)
               (FORMAT T "~&Rest arg (~A) is " (EH:REST-ARG-NAME FEF))
               (INCF LOCALNUM))
              (ARGP
               (FORMAT T "~&Arg ~D (~A) is " ARGNUM (EH:ARG-NAME FEF ARGNUM))
               (INCF ARGNUM))
              ((EQ ARG-SYNTAX 'FEF-ARG-FREE)
               (GO L))
              ((EQ ARG-SYNTAX 'FEF-ARG-INTERNAL-AUX)
               (RETURN NIL))
              (T
               (FORMAT T "~&Local ~D (~A) is " LOCALNUM (EH:LOCAL-NAME FEF LOCALNUM))
               (INCF LOCALNUM)))
        (PRINC (OR (NTH (LDB %%FEF-ARG-SYNTAX OPT-Q)
                        '("required, " "optional, "))
                   ""))
        (IF (EQ (NTH (LDB %%FEF-QUOTE-STATUS OPT-Q)
                     FEF-QUOTE-STATUS)
                'FEF-QT-QT)
            (PRINC "quoted, "))
        (PRINC (NTH (LDB %%FEF-SPECIALNESS OPT-Q)
                    '("local, " "special, " "" "remote, ")))
;       (PRINC (NTH (LDB %%FEF-DES-DT OPT-Q)
;                              FEF-DES-DT))
        (SETQ INIT-OPTION (NTH (LDB %%FEF-INIT-OPTION OPT-Q)
                               FEF-INIT-OPTION))
        (CASE INIT-OPTION
          (FEF-INI-NIL (FORMAT T "initialized to NIL."))
          (FEF-INI-NONE (FORMAT T "not initialized."))
          (FEF-INI-SELF (FORMAT T "initialized by binding it to itself."))
          (FEF-INI-COMP-C (FORMAT T "initialized by execution of the function."))
          (FEF-INI-PNTR
           (PRINC "initialized to ")
           (LET ((COMPILER::DISASSEMBLE-OBJECT-OUTPUT-FUN NIL))
             (COMPILER::DISASSEMBLE-POINTER FEF (%POINTER-DIFFERENCE ADL FEF) 0))
           (PRINC ".")
           (POP ADL))
          (FEF-INI-C-PNTR
           (LET ((LOC (CAR ADL))
                 (STR (%FIND-STRUCTURE-HEADER (CAR ADL))))
             (COND ((SYMBOLP STR)
                    (FORMAT T "initialized to the ~A of ~S."
                            (CASE (%POINTER-DIFFERENCE LOC STR)
                              (1 "value")
                              (2 "function definition")
                              (3 "property list")
                              (4 "package"))
                            STR))
                   ((CONSP STR)
                    (FORMAT T "initialized to the function definition of ~S."
                            (CAR STR)))
                   (T (FORMAT T "initialized to the contents of ~S." (CAR ADL)))))
           (POP ADL))
          (FEF-INI-EFF-ADR
           (FORMAT T "initialized to the value of ")
           (LET ((SLOT (LOGAND #o77 (CAR ADL))))
             (IF (= (LOGAND #o700 (CAR ADL)) (GET 'COMPILER::local 'COMPILER::QLVAL))
                 (FORMAT T "local ~D (~S)." SLOT (EH:LOCAL-NAME FEF SLOT))
               (FORMAT T "arg ~D (~S)." SLOT (EH:ARG-NAME FEF SLOT))))
           (POP ADL))
          (FEF-INI-OPT-SA (FORMAT T "initialized by the code up to pc ~D." (CAR ADL))
                          (POP ADL)))
        (GO L)
))

(DEFUN DESCRIBE-STACK-GROUP (SG &AUX TEM)
  (FORMAT T "~%Stack Group; name is ~S, current state ~S"
          (SG-NAME SG)
          (NTH (SG-CURRENT-STATE SG) SG-STATES))
  (COND ((NOT (ZEROP (SG-IN-SWAPPED-STATE SG)))
         (FORMAT T "~%  Variables currently swapped out")))
  (COND ((NOT (ZEROP (SG-FOOTHOLD-EXECUTING-FLAG SG)))
         (FORMAT T "~%  Foothold currently executing")))
  (COND ((NOT (ZEROP (SG-PROCESSING-ERROR-FLAG SG)))
         (FORMAT T "~% Currently processing an error")))
  (COND ((NOT (ZEROP (SG-PROCESSING-INTERRUPT-FLAG SG)))
         (FORMAT T "~% Currently processing an interrupt")))
  (FORMAT T "~%ERROR-MODE:")
     (PRINT-ERROR-MODE (SG-SAVED-M-FLAGS SG))
  (FORMAT T "~%SG-SAFE ~D, SG-SWAP-SV-ON-CALL-OUT ~D, SG-SWAP-SV-OF-SG-THAT-CALLS-ME ~D"
          (SG-SAFE SG)
          (SG-SWAP-SV-ON-CALL-OUT SG)
          (SG-SWAP-SV-OF-SG-THAT-CALLS-ME SG))
  (FORMAT T "~%SG-INST-DISP: ~D (~:*~[Normal~;Debug~;Single-step~;Single-step done~])"
            (SG-INST-DISP SG))
  (FORMAT T "~%SG-PREVIOUS-STACK-GROUP ~S, SG-CALLING-ARGS-NUMBER ~S, SG-CALLING-ARGS-POINTER ~S"
          (SG-PREVIOUS-STACK-GROUP SG)
          (SG-CALLING-ARGS-NUMBER SG)
          (SG-CALLING-ARGS-POINTER SG))
  (FORMAT T "~%Regular PDL pointer ~D, ~D available, ~D limit"
          (SG-REGULAR-PDL-POINTER SG)
          (ARRAY-LENGTH (SG-REGULAR-PDL SG))
          (SG-REGULAR-PDL-LIMIT SG))
  (FORMAT T "~%Special PDL pointer ~D, ~D available, ~D limit"
          (SG-SPECIAL-PDL-POINTER SG)
          (ARRAY-LENGTH (SG-SPECIAL-PDL SG))
          (SG-SPECIAL-PDL-LIMIT SG))
  (COND ((SETQ TEM (SG-RECOVERY-HISTORY SG))
         (FORMAT T "~%Recovery history ~S" TEM)))
  (COND ((SETQ TEM (SG-PLIST SG))
         (FORMAT T "~%SG-PLIST ~S" TEM))))

(DEFUN PRINT-ERROR-MODE (&OPTIONAL (EM %MODE-FLAGS) (STREAM *STANDARD-OUTPUT*))
  "Prints the current error mode."
  (FORMAT STREAM "~&CAR of a number is ~A.
CDR of a number is ~A.
CAR of a symbol is ~A.
CDR of a symbol is a ~A.
Trapping is ~A.~%"
          (CASE (LDB %%M-FLAGS-CAR-NUM-MODE EM)
            (0 "an error")
            (1 "NIL")
            (OTHERWISE "in an unknown state"))
          (CASE (LDB %%M-FLAGS-CDR-NUM-MODE EM)
            (0 "an error")
            (1 "NIL")
            (OTHERWISE "in an unknown state"))
          (CASE (LDB %%M-FLAGS-CAR-SYM-MODE EM)
            (0 "an error")
            (1 "NIL if the symbol is NIL, otherwise an error")
            (2 "NIL")
            (3 "its print-name"))
          (CASE (LDB %%M-FLAGS-CDR-SYM-MODE EM)
            (0 "an error")
            (1 "NIL if the symbol is NIL, otherwise an error")
            (2 "NIL")
            (3 "its property list"))
          (CASE (LDB %%M-FLAGS-TRAP-ENABLE EM)
            (0 "disabled")
            (1 "enabled"))
          ))

(DEFUN DESCRIBE-FEF (FEF &OPTIONAL DONT-MENTION-DEBUGGING-INFO
                     &AUX HEADER HEADER-TYPE NAME FAST-ARG SV MISC LENGTH
                          NO-ADL-EXISTS FAST-ARG-ACTIVE)
   (COND ((SYMBOLP FEF)
          (DESCRIBE-FEF (SYMBOL-FUNCTION FEF)))
         ((NOT (COMPILED-FUNCTION-P FEF))
          (FERROR "~S is not a FEF (a compiled function)" FEF))
         (T
          (SETQ HEADER (%P-LDB-OFFSET %%HEADER-REST-FIELD FEF %FEFHI-IPC))
          (SETQ HEADER-TYPE (%P-LDB-OFFSET %%HEADER-TYPE-FIELD FEF %FEFHI-IPC))
          (SETQ LENGTH (%P-CONTENTS-OFFSET FEF %FEFHI-STORAGE-LENGTH))
          (SETQ NAME (%P-CONTENTS-OFFSET FEF %FEFHI-FCTN-NAME))
          (SETQ FAST-ARG (%P-CONTENTS-OFFSET FEF %FEFHI-FAST-ARG-OPT))
          (SETQ SV (%P-CONTENTS-OFFSET FEF %FEFHI-SV-BITMAP))
          (SETQ MISC (%P-CONTENTS-OFFSET FEF %FEFHI-MISC))

          (FORMAT T "~%FEF for function ~S~%" NAME)
          (FORMAT T "Initial relative PC: ~S halfwords.~%" (LDB %%FEFH-PC HEADER))
          (cond ((= header-type %header-type-fef)
                 (SETQ NO-ADL-EXISTS (LDB %%FEFH-NO-ADL HEADER)
                       FAST-ARG-ACTIVE (LDB %%FEFH-FAST-ARG HEADER))
                 (UNLESS (ZEROP (%P-LDB %%FEFH-GET-SELF-MAPPING-TABLE FEF))
                   (FORMAT T "This is a method of flavor ~S.~%"
                           (%P-CONTENTS-OFFSET FEF (1- (%P-LDB-OFFSET %%FEFHI-MS-ARG-DESC-ORG
                                                                      FEF %FEFHI-MISC)))))
; -- Special variables
                 (COND ((ZEROP (LDB %%FEFH-SV-BIND HEADER))
                        (PRINC "There are no special variables present."))
                       (T (PRINC "There are special variables, ")
                          (TERPRI)
                          (COND ((ZEROP (LDB %%FEFHI-SVM-ACTIVE SV))
                                 (PRINC "but the S-V bit map is not active. "))
                                (T (FORMAT T "and the S-V bit map is active and contains: ~O"
                                           (LDB %%FEFHI-SVM-BITS SV))))))
                 (TERPRI))
                ((= header-type %HEADER-TYPE-FAST-FEF-FIXED-ARGS-NO-LOCALS)
                 (setq no-adl-exists (%p-ldb-offset %%fefsl-no-adl fef %fefhi-storage-length))
                 (FORMAT T "The fast-fef option FIXED-ARGS-NO-LOCALS is selected.~%")
                 (FORMAT T "It says there are ~s args.~%" (ldb %%fefh-args-for-fanl header)))
                ((= header-type %HEADER-TYPE-FAST-FEF-VAR-ARGS-NO-LOCALS)
                 (setq no-adl-exists (%p-ldb-offset %%fefsl-no-adl fef %fefhi-storage-length))
                 (FORMAT T "The fast-fef option VAR-ARGS-NO-LOCALS is selected.~%")
                 (format t "It says there are between ~s and ~s args.~%"
                         (%p-ldb-offset %%fefh-min-args-for-vanl fef %fefhi-ipc)  ;cdr-code
                         (ldb %%fefh-max-args-for-vanl header)))
                ((= header-type %HEADER-TYPE-FAST-FEF-FIXED-ARGS-WITH-LOCALS)
                 (setq no-adl-exists (%p-ldb-offset %%fefsl-no-adl fef %fefhi-storage-length))
                 (format t "The fast-fef option FIXED-ARGS-WITH-LOCALS is selected.~%")
                 (format t "It says there are ~s args and ~s locals.~%"
                         (%p-ldb-offset %%fefh-args-for-fawl fef %fefhi-ipc)       ;cdr-code
                         (ldb %%fefh-locals-for-fawl header)))
                ((= header-type %HEADER-TYPE-FAST-FEF-VAR-ARGS-WITH-LOCALS)
                 (setq no-adl-exists (%p-ldb-offset %%fefsl-no-adl fef %fefhi-storage-length))
                 (format t "The fast-fef option VAR-ARGS-WITH-LOCALS is selected.~%")
                 (format t "It says there are between ~s and ~s args and ~s locals.~%"
                         (%p-ldb-offset %%fefh-min-args-for-vawl fef %fefhi-ipc)  ;cdr-code
                         (ldb %%fefh-max-args-for-vawl header)
                         (ldb %%fefh-locals-for-vawl header)))
                (t (format t "The header type field (~s) is not a known code.~%"
                           header-type)
                   (setq no-adl-exists (%p-ldb-offset %%fefsl-no-adl fef %fefhi-storage-length))))

          ;; -- Print out the fast arg option
          (FORMAT T "The Fast Argument Option is ~A"
                    (IF (ZEROP (LDB %%FEFH-FAST-ARG HEADER))
                        "not active, but here it is anyway:"
                      "active:"))
          (DESCRIBE-NUMERIC-DESCRIPTOR-WORD FAST-ARG)
; -- ADL.
          (COND ((ZEROP NO-ADL-EXISTS)
                 (FORMAT T "There is an ADL:  It is ~S long, and starts at ~S"
                         (LDB %%FEFHI-MS-BIND-DESC-LENGTH MISC)
                         (LDB %%FEFHI-MS-ARG-DESC-ORG MISC))
                 (DESCRIBE-FEF-ADL FEF)
                 )
                (T (PRINC "There is no ADL.")))
          (TERPRI)
          ;; -- Randomness.
          (FORMAT T "~%The length of the local block is ~S~%"
                    (LDB %%FEFHI-MS-LOCAL-BLOCK-LENGTH MISC))
          (FORMAT T "The total storage length of the FEF is ~S~%"
                    LENGTH)
          (UNLESS DONT-MENTION-DEBUGGING-INFO
            (LET ((DBI (FEF-DEBUGGING-INFO FEF)))
              (WHEN DBI
                (FORMAT T "Debugging info:~%")
                (DOLIST (ITEM DBI)
                  (FORMAT T "  ~S~%" ITEM)))))
          )))


(DEFUN DESCRIBE-NUMERIC-DESCRIPTOR-WORD (N &AUX (MIN (LDB %%ARG-DESC-MIN-ARGS N))
                                                (MAX (LDB %%ARG-DESC-MAX-ARGS N)))
  (FORMAT T "~&   ")
  (IF (BIT-TEST %ARG-DESC-QUOTED-REST N)
      (PRINC "Quoted rest arg, "))
  (IF (BIT-TEST %ARG-DESC-EVALED-REST N)
      (PRINC "Evaluated rest arg, "))
  (IF (BIT-TEST %ARG-DESC-FEF-QUOTE-HAIR N)
      (PRINC "Some args quoted, "))
  (IF (BIT-TEST %ARG-DESC-INTERPRETED N)
      (PRINC "Interpreted function, "))
  (IF (BIT-TEST %ARG-DESC-FEF-BIND-HAIR N)
      (PRINC "Linear enter must check ADL, "))
  (FORMAT T "Takes ~:[between ~D and ~D~;~D~] args.~%"
          (= MAX MIN) MIN MAX))


(defun describe-microcode-function (u-entry)
  (if (not (= (%data-type u-entry) dtp-u-entry))
      (ferror nil "~s is not a dtp-u-entry" u-entry))
  (format t "~&~s:~&entry-area index = ~s" u-entry (%pointer u-entry))
  (let ((symbol-area-index (aref #'micro-code-entry-area (%pointer u-entry))))
    (format t "~&entry-area contains ~s" symbol-area-index)
    (cond ((fixp symbol-area-index)
           (format t "~&symbol-area contains ~s" (aref #'micro-code-symbol-area symbol-area-index))))))

(defun describe-array (array)
  (let ((rank (array-rank array))
        (long-length-flag (%p-ldb-offset %%array-long-length-flag array 0)))
    (format t "~%This is an ~S type array. (element-type ~S)~%"
            (array-type array) (array-element-type array))
    (case rank
      (0
       (format T "It is of zero rank."))
      (1
       (format t "It is a vector, with a total size of ~S elements" (array-total-size array)))
      (t
       (format T "It is ~S-dimensional, with dimensions " rank)
       (dotimes (d rank) (format t "~S " (array-dimension array d)))
       (format t ". Total size ~S elements" (array-total-size array))))
    (when (array-has-leader-p array)
      (let ((length (array-leader-length array)))
        (cond ((and (eq rank 1)
                    (eq length 1)
                    (fixnump (array-leader array 0)))
               (format t "~%It has a fill-pointer: ~S" (fill-pointer array)))
              (t
               (format t "~%It has a leader, of length ~S. Contents:" length)
               (format t "~%  Leader 0~:[~; (fill-pointer)~]: ~S"
                       (and (eq rank 1) (fixnump (array-leader array 0))) (array-leader array 0))
               (dotimes (i (1- length))
                 (format t "~%  Leader ~S: ~S" (1+ i) (array-leader array (1+ i))))))))
    (when (array-displaced-p array)
      (cond ((array-indirect-p array)
             (format t "~%The array is indirected to ~S"
                     (%p-contents-offset array (+ rank long-length-flag)))
             (and (array-indexed-p array)
                  (format T ", with index-offset ~S"
                          (%p-contents-offset array (+ rank long-length-flag 2))))
             (format t "~%Description of that array:")
             (describe-1 (%p-contents-offset array (+ rank long-length-flag))))
            (t (format t "~%The array is displaced to ~S"
                       (%p-contents-offset array (+ rank long-length-flag))))))))

(defun describe-symbol (sym)
  (let ((symbol-package (symbol-package sym)))
    (format t "~%Symbol ~S is in ~:[no~;the ~:*~A~] package." sym (symbol-package sym))
    (let ((tem nil))
      (dolist (p *all-packages*)
        (unless (eq p symbol-package)
          (multiple-value-bind (s flag) (find-symbol sym p)
            (when (and flag
                       (eq s sym) ;; are we talking about the same symbol ?
                       (not (eq flag :inherited)))
              (push p tem)))))
      (when tem (format t "~% It is ~:[strangely~;also~] interned in package~P ~{~A~^, ~}"
                        symbol-package (length tem) tem))))
  (when (and (boundp sym) (not (keywordp sym)))
    (let ((*print-level* *describe-print-level*)
          (*print-length* *describe-print-length*))
      (format t "~%The value of ~S is ~S" sym (symbol-value sym)))
    (describe-1 (symbol-value sym)))
  (when (fboundp sym)
    (let ((*print-level* *describe-print-level*)
          (*print-length* *describe-print-length*))
      (ignore-errors
        (format t "~%The function definition of ~S is ~S: ~S"
                sym (symbol-function sym) (arglist sym))))
         (describe-1 (symbol-function sym)))
  (do ((pl (symbol-plist sym) (cddr pl))
       (*print-level* *describe-print-level*)
       (*print-length* *describe-print-length*))
      ((null pl))
    (format t "~%~S has property ~S: ~S"
            sym (car pl) (cadr pl))
    (describe-1 (cadr pl)))
  (if (not (or (boundp sym) (fboundp sym) (symbol-plist sym)))
      (format t "~%It has no value, definition or properties"))
  nil)

(defun describe-cons (l &aux (*print-circle* t))
  (format t "~%~S is a cons" l))

(DEFUN DESCRIBE-LOCATIVE (X)
  (LET ((AREA (%AREA-NUMBER X)))
    (cond ((NULL AREA)
           (FORMAT T "~%~S is a locative pointer not into any area." X))
          (t
           (FORMAT T "~%~S is a locative pointer into area ~S"
                   X (AREA-NAME AREA))
           (when (>= area (min si:working-storage-area si:nr-sym)) ;the first area which is not fixed.
                                ; don't go through this trouble if the area might be dangerous
                                ; to do %find-structure-header in.
             (LET* ((STRUC (%FIND-STRUCTURE-HEADER X))
                    (BASEP (%MAKE-POINTER DTP-LOCATIVE (%FIND-STRUCTURE-LEADER STRUC)))
                    (BOUND (%MAKE-POINTER-OFFSET DTP-LOCATIVE BASEP (%STRUCTURE-TOTAL-SIZE STRUC))))
               (IF (AND (OR (EQ BASEP X) (%POINTER-LESSP BASEP X))
                        (%POINTER-LESSP X BOUND))
                   (FORMAT T "~%It points to word ~D. of ~S~%" (%POINTER-DIFFERENCE X STRUC) STRUC)
                 (FORMAT T "at some sort of forwarded version of ~S~%" STRUC))
               (DESCRIBE-1 STRUC)))))))

(DEFUN DESCRIBE-DEFSTRUCT (X &OPTIONAL DEFSTRUCT-TYPE)
  "Prints out a description of X, including the contents of each of its
slots.  DEFSTRUCT-TYPE should be the name of the structure so
DESCRIBE-DEFSTRUCT can figure out the names of the slots of X.  If X is
a named structure, you don't have to provide DEFSTRUCT-TYPE.  Normally
the DESCRIBE function will call DESCRIBE-DEFSTRUCT if asked to describe
a named structure; however, some named structures have their own way of
describing themselves."
  (LET ((DESCRIPTION (GET (OR DEFSTRUCT-TYPE
                              (IF (CONSP X) (CAR X) (NAMED-STRUCTURE-P X)))
                          'DEFSTRUCT-DESCRIPTION)))
    (FORMAT T "~%~S is a ~S~%" X (DEFSTRUCT-DESCRIPTION-NAME DESCRIPTION))
    (DOLIST (L (DEFSTRUCT-DESCRIPTION-SLOT-ALIST DESCRIPTION))
      (FORMAT T "   ~S:~30T~S~%"
              (CAR L)
              (EVAL `(,(DEFSTRUCT-SLOT-DESCRIPTION-REF-MACRO-NAME (CDR L)) ',X))))
    X))
; also si::describe-defstruct-description in sys2;struct

(defun describe-instance (instance)
  "Prints out each of the slots of INSTANCE.
This bypasses the :DESCRIBE method of the instance."
  (let* ((flavor (%instance-flavor instance))
         (ivars (flavor-all-instance-variables flavor)))
    (format t "~%~S is an instance of flavour ~S" instance flavor)
    (loop for v in ivars
          for n from 1
       do (format t "   ~S:~30T~S~%" v (%instance-ref instance n))))
  instance)


(defun describe-closure (cl)
  (if (interpreter-environment-closure-p cl)
      (describe-interpreter-closure cl)         ;defined in sys; eval
    (let ((bindings (closure-bindings cl))
          (sym nil) (offset nil))
      (format t "~%~S is a closure of ~S~%" cl (closure-function cl))
      (case (length bindings)
        (0 (format t "(No bindings)"))
        (1 (describe-lexical-closure-bindings bindings (closure-function cl) *standard-output*))
        (t
         (do ((bindings bindings (cddr bindings)))
             ((null bindings))
           (setq sym (%find-structure-header (car bindings))
                 offset (%pointer-difference (car bindings) sym))
           (if (not (symbolp sym))
               (format t "    ~S" (car bindings))
             (format t "    ~[Print name~;Value~;Function~;Property list~;Package~] cell of ~S"
                     offset sym))
           (format t ":~40T~:[void~;~S~]~%"
                   (location-boundp (cadr bindings))
                   (and (location-boundp (cadr bindings))
                        (contents (cadr bindings)))))))
      (unless (consp (closure-function cl))             ;don't describe interpreted functions.
        (describe-1 (closure-function cl))))))

(defun describe-lexical-closure-bindings (bindings function stream)
  (format t "Lexical environment:~%")
  (let ((map ()))
    (labels ((frob (fn depth)
               (let* ((di (debugging-info fn))
                      (m (cdr (assq 'compiler::lexical-ref-map di)))
                      (o (cdr (assq ':internal-fef-offsets di))))
                 (dolist (x m)
                   (let ((n (dpb (- (ldb (byte 12. 12.) (car x)) depth)
                                 (byte 12. 12.)
                                 (ldb (byte 12. 0) (car x)))))
                     (unless (assq n map)
                       (push (cons n (cadr x)) map))))
                 (dolist (x o)
                   (frob (%p-contents-offset fn x) (1+ depth))))))
      (frob function 0))
    (loop for frame in (car bindings)
          for f from 0
       when (eq frame 't) do
         (format stream "  (Context ~D empty.)~%" f)
       else do
         (format stream "  Context ~D~%" f)
         (loop for x in frame
               for i from 0
               as code = (dpb f (byte 12. 12.) i)
               as name = (cdr (assq code map))
           do (format stream "    Slot ~D~:[ :~8@T~; (~:*~S)~]~1,8@T~S~%" i name x)))))

(DEFUN DESCRIBE-SELECT-METHOD (M)
  (FORMAT T "~%~S handles:" M)
  (DO ((ML (%MAKE-POINTER DTP-LIST M) (CDR ML)))
      ((ATOM ML)
       (UNLESS (NULL ML)
         (FORMAT T "~%   anything else to ~S" ML)
         (IF (AND (SYMBOLP ML) (BOUNDP ML))
           (FORMAT T "  -> ~S" (SYMBOL-VALUE ML)))))
    (cond ((ATOM (CAR ML))
           (FORMAT T "~%   subroutine ~S" (CAR ML)))
          (t
           (FORMAT T "~%   ~S: ~34T" (CAAR ML))
           (OR (EQ (FUNCTION-NAME (CDAR ML)) (CDAR ML))
               (PRINC "#'"))
           (PRIN1 (FUNCTION-NAME (CDAR ML)))))))

(defun describe-all-areas ()
  (dolist (a (current-area-list))
    (describe-area a)))

(defun describe-area (area)
  "Describe area AREA and all of its regions."
  (ctypecase area
    (area-name (setq area (symbol-value area)))
    (area-number))
  (multiple-value-bind (length used regions)
      (room-get-area-length-used area)
    (declare (ignore length used))
    (format t "~&Area #~A: ~S has:~
               ~%                 volatility  ~A~
               ~%                 region-size  #o~O~
               ~%                 map-status   #o~O ~:[(WRITE-READ)~;(READ-ONLY)~]~
               ~%                 swap quantum ~D.~%"
            area
            (area-name area)
            (%area-volatility area)
            (%area-region-size area)
            (%area-map-status area)
            (= (%area-map-status area)  %pht-map-status-read-only)
            (%area-swap-recommendations area))
    (when (area-temporary-p area) (format t "It is a temporary area.~%"))
    (format t "It has ~D. region~P:~%" regions regions)
    (for-every-region-in-area (region area)
      (describe-region region))
    t))

(DEFUN DESCRIBE-ALL-REGIONS NIL
  "Tell all about all regions."
  (DO ((REGION (1- NUMBER-OF-REGIONS) (1- REGION)))
      ((MINUSP REGION))
    (DESCRIBE-REGION REGION)))

(defun describe-region (region)
  "Tell all about the region number REGION."
  (let ((h (ldb si:%%virtual-page-structure-handle
                (aref #'system:virtual-page-data (lsh (%region-origin region) -8))))
        (area-from-region-area-map (aref #'system:region-area-map region)))
    (format t "  Region #~A: Origin ~O, Length ~O, Used ~O, region-area-map ~O "
          region
          (%pointer-unsigned (%region-origin region))
          (%region-length region)
          (%region-free-pointer region)
          area-from-region-area-map)
    (cond ((not (zerop (logand #o377 (ldb si:%%virtual-page-first-header h))))
           (format t "~2% The initial header begins on word ~o, not ZERO!!!"
                   (ldb si:%%virtual-page-first-header h))
           (if (not (minusp area-from-region-area-map))  ;dont bomb on inactive regions.
               (let ((w0-dt (%p-data-type (%region-origin region))))
                 (format t "~%  The data type of word 0 is ~s. ~2%"
                         (nth w0-dt q-data-types))
                 (if (= w0-dt dtp-header)
                     (format t "Header type is ~s. ~2%"
                             (nth (%p-ldb %%header-type-field (%region-origin region))
                                  q-header-types)))))))
    (cond ((not (zerop (ldb si:%%virtual-page-initial-qs h)))
           (format t "~2% Initial-boxed-qs for the first page is ~o, not ZERO!!! ~2%"
                   (ldb si:%%virtual-page-initial-qs h)))))
  (let ((used (%region-free-pointer region))
        (scavenged (%region-gc-pointer region)))
    (if (zerop (%region-scavenge-enable region))
        (format t "Scavenger off, ")
      (cond ((= used scavenged)
             (format t "Scavenge done, "))
            ((= used 0)
             (format t "Scavenged 0%, "))
            (t
             (format t "Scavenged ~D%, "
                     (truncate (* 100. (cl:/ (float scavenged) (float used)))))))))
  (format t "~A space, Vol=~D.~%"
          (nth (%region-type region)
               '(free old new new1 new2 new3 new4 new5 new6
                      static fixed extra-pdl copy moby-fixed moby-new))
          (%region-volatility region))
  (format t "    Scavenge-enable ~s, Scavenge-carefully ~s, Flip enable ~s, Swapin quantum ~D~%"
          (%region-scavenge-enable region)
          (%region-scavenge-carefully region)
          (%region-flip-enable region)
          (%region-swap-recommendations region))
  )

(defun describe-rational-number (number)
  (format t "~&~S is a rational number with numerator ~S and denominator ~S"
          number (numerator number) (denominator number)))

(defun describe-complex-number (number)
  (format t "~&~S is a complex number with real part ~S and imaginary part ~S."
          number (realpart number) (imagpart number)))

(defun describe-character (character)
  (setq character (cl:character character))
  (format t "~&~S is a character with integer representation ~D, and code ~D
Its control-bits are ~D~:[~4*~; (~@[Control~*~]~@[Meta~*~]~@[Super~*~]~@[Hyper~*~])~]. Its font is ~D"
          character (char-int character)
          (char-code character) (char-bits character) ( 0 (char-bits character))
          (char-bit character :control) (char-bit character :meta)
          (char-bit character :super) (char-bit character :hyper)
          (char-font character))
  character)

;;;; Room

(DEFVAR ROOM '(WORKING-STORAGE-AREA MACRO-COMPILED-PROGRAM)
  "Areas to mention when ROOM is called with no args.")

(defun room-get-area-length-used (area &aux (regions 0) (length 0) (used 0))
  (for-every-region-in-area (region area)
    (incf regions)
    (incf length (%region-length region))
    (incf used (%region-free-pointer region)))
  (values length used regions))

;>> *print-package*
(defun room-print-area (area)
  (let ((*package* (pkg-find-package "SYSTEM")))
    (unless (null (area-name area))
      (multiple-value-bind (length used regions)
          (room-get-area-length-used area)
        (if (= (%area-type area) %region-space-fixed)
            (format t "~51,1,1,'.<~S~;(~D region~:P)~> ~O/~O used.  ~D% free.~%"
                    (area-name area)
                    regions
                    used
                    length
                    (cond ((zerop length)
                           0)
                          ((< length #o40000)
                           (truncate (* 100. (- length used)) length))
                          (t
                           (truncate (- length used) (truncate length 100.)))))
          (format t "~51,1,1,'.<~S~;(~D region~:P)~> ~DK allocated, ~DK used.~%"
                  (area-name area)
                  regions
                  (ceiling length #o2000)
                  (ceiling used #o2000))))))
  t)

;;; (ROOM) tells about the default areas
;;; (ROOM area1 area2...) tells about those areas
;;; (ROOM T) tells about all areas
;;; (ROOM NIL) prints only the header, does not do any areas
(DEFUN ROOM (&REST ARGS)
  "Print size and free space of some areas.
ARGS can be areas, or T as arg means all areas.
No args means use the areas whose names are members of the value of ROOM.
NIL as arg means print a header but mention no areas."
  (LET ((FREE-SIZE (GET-FREE-SPACE-SIZE))
        (PHYS-SIZE (SYSTEM-COMMUNICATION-AREA %SYS-COM-MEMORY-SIZE)))
    (FORMAT T "~&Physical memory: #o~O (~DK), Free space: #o~O (~DK)"
              PHYS-SIZE (TRUNCATE PHYS-SIZE #o2000) FREE-SIZE (TRUNCATE FREE-SIZE #o2000)))
  (MULTIPLE-VALUE-BIND (N-WIRED-PAGES N-FIXED-WIRED-PAGES)
      (COUNT-WIRED-PAGES)
    (FORMAT T ", Wired pages ~D+~D (~D~[~;.25~;.5~;.75~]K)~%"
              N-FIXED-WIRED-PAGES (- N-WIRED-PAGES N-FIXED-WIRED-PAGES)
              (TRUNCATE N-WIRED-PAGES (TRUNCATE #o2000 PAGE-SIZE))
              (CL:REM N-WIRED-PAGES (TRUNCATE #o2000 PAGE-SIZE))))
  (COND ((NULL ARGS)
         (SETQ ARGS ROOM))
        ((EQUAL ARGS '(T))
         (FORMAT T "Unless otherwise noted, area names are in the SYSTEM package~%")
         (SETQ ARGS (CURRENT-AREA-LIST))))
  (COND ((NOT (EQUAL ARGS '(NIL)))
         (DOLIST (AREA ARGS)
           (ROOM-PRINT-AREA (IF (SYMBOLP AREA) (SYMEVAL AREA) AREA))))))


(DEFUN PRINT-AREAS-OF-WIRED-PAGES ()
  (DO ((ADR (%REGION-ORIGIN PAGE-TABLE-AREA) (+ ADR 2))
       (N (TRUNCATE (SYSTEM-COMMUNICATION-AREA %SYS-COM-PAGE-TABLE-SIZE) 2) (1- N)))
      ((ZEROP N))
    (WHEN (AND (NOT (ZEROP (%P-LDB %%PHT1-VALID-BIT ADR)))
               (= (%P-LDB %%PHT1-SWAP-STATUS-CODE ADR) %PHT-SWAP-STATUS-WIRED))
      (FORMAT T "~S " (AREF (SYMBOL-FUNCTION 'SYS:AREA-NAME)
                            (%AREA-NUMBER
                              (ASH (%P-LDB %%PHT1-VIRTUAL-PAGE-NUMBER ADR) 8)))))))



(DEFUN COUNT-PAGES-IN-POSITION-FOR-FAST-CACHE ()
  (DO ((ADR (%REGION-ORIGIN PAGE-TABLE-AREA) (+ ADR 2))
       (N (TRUNCATE (SYSTEM-COMMUNICATION-AREA %SYS-COM-PAGE-TABLE-SIZE) 2) (1- N))
       (IN-POSITION 0)
       (TOTAL 0))
      ((ZEROP N)
       (FORMAT T "~%~D pages in position out of ~D total" IN-POSITION TOTAL))
    (COND ((NOT (ZEROP (%P-LDB %%PHT1-VALID-BIT ADR)))
           (SETQ TOTAL (1+ TOTAL))
           (COND ((= (LDB (BYTE 4 0) (%P-LDB %%PHT1-VIRTUAL-PAGE-NUMBER ADR))
                     (LDB (BYTE 4 0) (%P-LDB %%PHT2-PHYSICAL-PAGE-NUMBER (1+ ADR))))
                  (SETQ IN-POSITION (1+ IN-POSITION))))))))

(DEFUN count-unused-physical-pages ()
  (DO ((ADR (%REGION-ORIGIN PAGE-TABLE-AREA) (+ ADR 2))
       (N (TRUNCATE (SYSTEM-COMMUNICATION-AREA %SYS-COM-PAGE-TABLE-SIZE) 2) (1- N))
       (total 0)
       (unused 0))
      ((ZEROP N)
       (FORMAT T "~%~D pages out of ~D have never been used (~d%)" unused total
               (round (* 100. (cl:/ (float unused) total)))))
    (COND ((NOT (ZEROP (%P-LDB %%PHT1-VALID-BIT ADR)))
           (incf total)
           (if (= (%p-ldb %%pht1-virtual-page-number adr) %pht-dummy-virtual-address)
               (incf unused))))))

(defun show-virtual-memory-fragmentation (&optional (window tv:selected-window))
  (unless (send window :operation-handled-p :draw-point)
    (ferror nil "Window does not support graphics operations"))
  (let* ((blinkers (tv:sheet-blinker-list window))
         (visibility-list (mapcar #'(lambda (blinker) (send blinker :visibility)) blinkers)))
    (unwind-protect
        (progn (dolist (blinker blinkers)
                 (send blinker :set-visibility nil))
               (send window :clear-screen)
               (do* ((region 0 (add1 region))
                     (current-y 0)
                     (current-x 0)
                     (inside-width (send window :inside-width))
                     (vpage 0)
                     region-fill
                     region-size)
                    ((= region number-of-regions)
                     (multiple-value-bind (final-y final-x)
                         (floor vpage inside-width)
                       (send window :draw-triangle
                             final-x final-y
                             (- final-x 10) (+ final-y 10)
                             (+ final-x 10) (+ final-y 10))
                       (cursorpos (+ (ceiling final-y (send window :line-height)) 2) 0)))
                 (setq region-fill (ash (%region-free-pointer region) -8)
                       region-size (ash (%region-length region) -8))
                 (cond ((<= region-fill (- inside-width current-x))
                        (send window :draw-line current-x current-y (+ current-x region-fill) current-y))
                       (t
                        (send window :draw-line current-x current-y inside-width current-y)
                        (send window :draw-line
                              0
                              (add1 current-y)
                              (- region-fill (- inside-width current-x))
                              (add1 current-y))))
                 (if (<= region-size (- inside-width current-x))
                     (incf current-x region-size)
                   (setq current-x (- region-size (- inside-width current-x))
                         current-y (add1 current-y)))
                 (incf vpage region-size)))
      (mapcar #'(lambda (blinker vis) (send blinker :set-visibility vis))
              blinkers visibility-list))))


;(DEFCONST NUMERIC-ARG-DESC-INFO '(
;;  %%ARG-DESC-QUOTED-REST 2501
;;  %%ARG-DESC-EVALED-REST 2401
;;  %%ARG-DESC-ANY-REST 2402                    ;NON-ZERO IF HAS EITHER KIND OF REST ARG
;;  %%ARG-DESC-FEF-QUOTE-HAIR 2301              ; CALLER MUST CHECK A-D-L FOR FULL INFO
;;  %%ARG-DESC-INTERPRETED 2201                 ; NO INFORMATION AVAILABLE (VAL=1000077)
;;  %%ARG-DESC-FEF-BIND-HAIR 2101                       ; LINEAR ENTER MUST CHECK A-D-L
;  %%ARG-DESC-MIN-ARGS 0606                     ;MINIMUM NUMBER OF REQUIRED ARGS
;  %%ARG-DESC-MAX-ARGS 0006                     ;MAXIMUM NUMBER OF REQUIRED+OPTIONAL
;                                               ; ARGS.  REST ARGS NOT COUNTED.
;  ))

(defun describe-args-info (args-info)
  (unless (fixnump args-info) (setq args-info (args-info args-info)))
  (dolist (x '((%%arg-desc-interpreted . "~%~:[C~;Non-c~]ompiled function")
               (%%arg-desc-any-rest . "~@[~%Has &rest argument~]")
               (%%arg-desc-quoted-rest . "~@[~% Has quoted &rest arg~]")
               (%%arg-desc-evaled-rest . "~@[~% Has evalled &rest arg~]")
               (%%arg-desc-fef-quote-hair . "~@[~%Hairy FEF arg quoting (caller must check FEF ADL)~]")
               (%%arg-desc-fef-bind-hair . "~@[~%Hairy FEF binding~]")
               (%%arg-desc-min-args . "~*~%Minimum ~D. positional args")
               (%%arg-desc-max-args . "~*~%Maximum ~D. positional args")))
    (let ((tem (ldb (symbol-value (car x)) args-info)))
      (format t (cdr x) (not (zerop tem)) tem)))
  (fresh-line))

(defun describe-small-flonum (x)
  (format t "~%~S is a small flonum.~%  " x)
  (format t "Excess-~O exponent #o~O, ~D-bit mantissa #o~O (~:[including sign bit~;with sign bit deleted~])"
          short-float-exponent-offset
          (%short-float-exponent x)
          short-float-mantissa-length
          (%short-float-mantissa x)
          short-float-implicit-sign-bit-p))

(defun describe-flonum (x)
  (format t "~%~S is a flonum.~%  " x)
  (format t "Excess-~O exponent #o~O, ~D-bit mantissa #o~O (~:[including sign bit~;with sign bit deleted~])"
          single-float-exponent-offset
          (%single-float-exponent x)
          single-float-mantissa-length
          (%single-float-mantissa x)
          single-float-implicit-sign-bit-p))

(defun describe-bignum (x)
  (let ((len (%p-ldb-offset #o0022 x 0))
        (barf nil))
    (format t "~&~S is a bignum.~&It is ~R word~:P long.  It is ~[positive~;negative~].  ~
                 It is stored starting at location: #o~O~&Its contents:~2%"
            x len (%p-ldb-offset #o2201 x 0) (%pointer x))
    (do ((i 1 (1+ i)))
        ((> i len))
      (unless (zerop (%p-ldb-offset #o3701 x i))
        (setq barf t))
      (format t "~&~3O: ~[ ~;*~]"
              i (%p-ldb-offset #o3701 x i))
      (do ((ppss #o3601 (- ppss #o0100)))
          ((< ppss #o0001))
        (write-char (digit-char (%p-ldb-offset ppss x i))))
      (format t "  ~O," (%p-ldb-offset #o3601 x i))
      (do ((ppss #o3303 (- ppss #o0300)))
          ((< ppss #o0003))
        (write-char (digit-char (%p-ldb-offset ppss x i))))
      (princ "  ")
      (do ((ppss #o3403 (- ppss #o0300)))
          ((< ppss #o0103))
        (write-char (digit-char (%p-ldb-offset ppss x i))))
      (format t ",~O  ~O," (%p-ldb-offset #o0001 x i) (%p-ldb-offset #o3502 x i))
      (do ((ppss #o3203 (- ppss #o0300)))
          ((< ppss #o0203))
        (write-char (digit-char (%p-ldb-offset ppss x i))))
      (format t ",~O" (%p-ldb-offset #o0002 x i)))
    (if barf
        (format t "~2&* = high order bit illegally 1, bug in bignum microcode?"))
    (terpri))
  x)
