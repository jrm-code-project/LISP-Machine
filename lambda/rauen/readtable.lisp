;;; -*- Mode:LISP; Package:LISP-IO; Base:10; Readtable:CL -*-
;;;
;;; READTABLE.LISP
;;;
;;; Things to do.
;;;   Replace REEDTABLE with READTABLE when this is ready to run on the K.
;;;   size in readtable defstruct should be parametrized.
;;;   set-syntax-from-char
;;;
;;; Contents:
;;;   [1] Readtables
;;;   [2] Escaping predicates


(shadow '(readtable-macros readtable-array))



;;;----------------------------------------------------------------------------
;;; {1} READTABLES
;;;----------------------------------------------------------------------------
;;;
;;; A readtable controls the behavior of the reader.  It contains an entry
;;; for each character that the reader can read.  The entry describes the
;;; syntax of that character.  There are four slots in a readtable entry.
;;;
;;; SYNTAX-TYPE is a keyword, one of :ILLEGAL, :WHITESPACE, :CONSTITUENT,
;;; :SINGLE-ESCAPE, :MULTIPLE-ESCAPE, :TERMINATING-MACRO, or :NONTERMINATING-
;;; MACRO.
;;;
;;; DISPATCHING-MACRO-P is meaningful only if SYNTAX-TYPE is :TERMINATING-
;;; MACRO or :NONTERMINATING-MACRO.  It indicates whether or not that macro
;;; is a dispatching macro.  If SYNTAX-TYPE is not a macro type, then this is
;;; NIL.
;;;
;;; MACRO-FUNCTION is meaningful only if SYNTAX-TYPE is a macro type {and
;;; DISPATCHING-MACRO-P is NIL}.  It is a function of two arguments, a stream
;;; and the macro character, which is called when the reader macro is
;;; invoked.  If the macro is a dispatching macro, this will always be
;;; 'DISPATCH-CHAR-READER.
;;;
;;; DISPATCH-TABLE is meaningful only if SYNTAX-TYPE is a macro type and
;;; DISPATCHING-MACRO-P is T.  It is a vector of 256 elements, each
;;; corresponding to the dispatching macro's second character.  Each of these
;;; elements is a function of three arguments: a stream, the second character,
;;; and the numeric argument, which is called when the reader submacro is
;;; invoked.  Calls into the dispatch table are made by the MACRO-FUNCTION.
;;;
;;; The entries in a readtable may be manipulated with SET-SYNTAX-TYPE (not
;;; CL), SET-SYNTAX-FROM-CHAR, SET-MACRO-CHARACTER, MAKE-DISPATCH-MACRO-
;;; CHARACTER, and SET-DISPATCH-MACRO-CHARACTER.  They may be examined using
;;; SYNTAX-TYPE (not CL), GET-MACRO-CHARACTER, and GET-DISPATCH-MACRO-
;;; CHARACTER.
;;;
;;; The readtable ignores bucky bits and font info.
;;;----------------------------------------------------------------------------

(defvar *readtable*)
(defvar *initial-common-lisp-readtable*)

(defstruct (readtable
             (:constructor make-readtable-rep ())
             (:predicate   readtablep))
  (entries (make-array 256)))

(defstruct (readtable-entry
             (:constructor make-readtable-entry))
  (syntax-type         :ILLEGAL)
  (dispatching-macro-p NIL)
  (macro-function      #'(lambda (char stream) char stream (error "foo")))
  (dispatch-table      NIL))

(defun make-readtable ()
  (let* ((reedtable (make-readtable-rep))
         (entries   (readtable-entries reedtable)))
    (dotimes (i 256)
      (setf (aref entries i) (make-readtable-entry)))
    reedtable))

(defun get-readtable-entry (char reedtable)
  "Return the entry in REEDTABLE for CHAR."
  (aref (readtable-entries reedtable) (char-code char)))

(defun set-readtable-entry (char reedtable entry)
  "Set the entry for CHAR in REEDTABLE to ENTRY."
  (setf (aref (readtable-entries reedtable) (char-code char))
        entry))

(defun copy-readtable (&optional (from-readtable *readtable*) to-readtable)
  "Make a copy of FROM-READTABLE.  If TO-READTABLE is supplied, destructively
copy FROM-READTABLE into it."
  (let* ((new-readtable (if to-readtable to-readtable (make-readtable)))
         (old-entries   (readtable-entries from-readtable))
         (new-entries   (readtable-entries new-readtable)))
    (dotimes (i 256)
      (setf (aref new-entries i)
            (copy-readtable-entry (aref old-entries i))))))

(defun copy-readtable-entry (entry)
  (make-readtable-entry
    :syntax-type          (readtable-entry-syntax-type entry)
    :dispatching-macro-p  (readtable-entry-dispatching-macro-p entry)
    :macro-function       (readtable-entry-macro-function entry)
    :dispatch-table       (copy-seq (readtable-entry-dispatch-table entry))))



(defun syntax-type (char &optional (reedtable *readtable*))
  "Return a keyword describing CHAR's syntax type in REEDTABLE."
  (readtable-entry-syntax-type (get-readtable-entry char reedtable)))

(defun set-syntax-type (char type &optional (reedtable *readtable*))
  "Set CHAR's syntax type in REEDTABLE to TYPE."
  (if (member type '(:ILLEGAL :WHITESPACE :CONSTITUENT
                     :SINGLE-ESCAPE :MULTIPLE-ESCAPE))
      (set-readtable-entry char reedtable
                           (make-readtable-entry :syntax-type type))
      (error "Other syntax types can't just be set.")))

(defun set-syntax-from-char (to-char from-char
                             &optional
                             (to-readtable *readtable*)
                             (from-readtable *initial-common-lisp-readtable*))
  (let ((entry (get-readtable-entry from-char from-readtable)))
    (set-readtable-entry to-char to-readtable (copy-readtable-entry entry))))

(defun set-macro-character
       (char function &optional non-terminating-p (reedtable *readtable*))
  "Make CHAR be a nondispatching macro character in REEDTABLE.  Whenever the
reader encounters CHAR while using REEDTABLE, it will call FUNCTION with two
arguments:  the input stream and CHAR.  If NON-TERMINATING-P is not NIL, then
CHAR can be imbedded in tokens without invoking FUNCTION."
  (set-readtable-entry char
                       reedtable
                       (make-readtable-entry
                         :syntax-type         (if non-terminating-p
                                                  :NONTERMINATING-MACRO
                                                  :TERMINATING-MACRO)
                         :dispatching-macro-p NIL
                         :macro-function      function)))

(defun get-macro-character (char &optional (reedtable *readtable*))
  "Return two values:  the macro function associated with CHAR in REEDTABLE,
and nil or a non-nil value to indicate if the macro is nonterminating."
  (let ((entry (get-readtable-entry char reedtable)))
    (values (readtable-entry-macro-function entry)
            (case (readtable-entry-syntax-type entry)
              (:NONTERMINATING-MACRO T)
              (:TERMINATING-MACRO    NIL)
              (OTHERWISE
               (error "~C is not a macro character." char))))))

(defun make-dispatch-macro-character
       (char &optional non-terminating-p (reedtable *readtable*))
  "Make CHAR be a dispatching macro character in REEDTABLE.  Whenever the
reader encounters CHAR while using REEDTABLE, it will read in an optional
numeric argument and a second character.  Then it will invoke the appropriate
function defined by SET-DISPATCH-MACRO-CHARACTER."
  (set-readtable-entry
    char
    reedtable
    (make-readtable-entry
      :syntax-type         (if non-terminating-p
                               :NONTERMINATING-MACRO
                               :TERMINATING-MACRO)
      :dispatching-macro-p T
      :macro-function      'dispatch-char-reader
      :dispatch-table      (make-array 256 :initial-element NIL))))

(defun set-dispatch-macro-character
       (disp-char sub-char function &optional (reedtable *readtable*))
  (let* ((readtable-entry (get-readtable-entry disp-char reedtable))
         (dispatch-table  (readtable-entry-dispatch-table readtable-entry)))
    (cond
      ((not (readtable-entry-dispatching-macro-p readtable-entry))
       (error "~C is not a dispatch macro character." disp-char))
      ((char<= #\0 sub-char #\9)
       (error "Dispatch macro subcharacter can't be a digit."))
      (t
       (setf (aref dispatch-table (char-code (char-upcase sub-char)))
             function)))))

(defun get-dispatch-macro-character
       (disp-char sub-char &optional (reedtable *readtable*))
  (let* ((readtable-entry (get-readtable-entry disp-char reedtable))
         (dispatch-table  (readtable-entry-dispatch-table readtable-entry)))
    (cond
      ((not (readtable-entry-dispatching-macro-p readtable-entry))
       (error "~C is not a dispatch macro character." disp-char))
      ((char<= #\0 sub-char #\9)
       (error "Dispatch macro subcharacter can't be a digit."))
      (t
       (aref dispatch-table (char-code (char-upcase sub-char)))))))


;;; ambiguity in CLtL p. 364 ...nil if there is no function associated with...
;;; p. 363 ...initially every character has a character-macro function that
;;; signals an error...

(defun make-common-lisp-readtable ()
  "Construct and return a readtable for the standard Common Lisp syntax."
  (let ((reedtable (make-readtable)))
    (dolist (char '(#\! #\$ #\% #\& #\* #\+ #\- #\. #\/ #\0 #\1 #\2 #\3 #\4
                    #\5 #\6 #\7 #\8 #\9 #\: #\< #\= #\> #\? #\@ #\A #\B #\C
                    #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q
                    #\R #\S #\T #\U #\V #\W #\X #\Y #\Z #\[ #\] #\^ #\_ #\a
                    #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o
                    #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\{ #\} #\~
                    #\Rubout #\Backspace))
      (set-syntax-type char :CONSTITUENT reedtable))
    (dolist (char '(#\Tab #\Space #\Page #\Newline #\Return #\Linefeed))
      (set-syntax-type char :WHITESPACE reedtable))
    (set-syntax-type #\\ :SINGLE-ESCAPE reedtable)
    (set-syntax-type #\| :MULTIPLE-ESCAPE reedtable)
    (set-macro-character #\" 'double-quote-reader nil reedtable)
    (set-macro-character #\' 'quote-reader        nil reedtable)
    (set-macro-character #\( 'open-paren-reader   nil reedtable)
    (set-macro-character #\) 'close-paren-reader  nil reedtable)
    (set-macro-character #\, 'comma-reader        nil reedtable)
    (set-macro-character #\; 'semicolon-reader    nil reedtable)
    (set-macro-character #\` 'backquote-reader    nil reedtable)
    (make-dispatch-macro-character #\# T reedtable)
    (set-dispatch-macro-character #\# #\# 'read-|##| reedtable)
    (set-dispatch-macro-character #\# #\' 'read-|#'| reedtable)
    (set-dispatch-macro-character #\# #\( 'read-|#(| reedtable)
    (set-dispatch-macro-character #\# #\* 'read-|#*| reedtable)
    (set-dispatch-macro-character #\# #\, 'read-|#,| reedtable)
    (set-dispatch-macro-character #\# #\: 'read-|#:| reedtable)
    (set-dispatch-macro-character #\# #\= 'read-|#=| reedtable)
    (set-dispatch-macro-character #\# #\\ 'read-|#\\| reedtable)
    (set-dispatch-macro-character #\# #\| 'read-|#\|| reedtable)
    (set-dispatch-macro-character #\# #\+ 'read-#+/#- reedtable)
    (set-dispatch-macro-character #\# #\- 'read-#+/#- reedtable)
    (set-dispatch-macro-character #\# #\. 'read-|#.| reedtable)
    (set-dispatch-macro-character #\# #\A 'read-|#A| reedtable)
    (set-dispatch-macro-character #\# #\B 'read-|#B| reedtable)
    (set-dispatch-macro-character #\# #\C 'read-|#C| reedtable)
    (set-dispatch-macro-character #\# #\O 'read-|#O| reedtable)
    (set-dispatch-macro-character #\# #\R 'read-|#R| reedtable)
    (set-dispatch-macro-character #\# #\S 'read-|#S| reedtable)
    (set-dispatch-macro-character #\# #\X 'read-|#X| reedtable)
    (set-dispatch-macro-character #\# #\)         'signal-sharp-error reedtable)
    (set-dispatch-macro-character #\# #\<         'signal-sharp-error reedtable)
    (set-dispatch-macro-character #\# #\Backspace 'signal-sharp-error reedtable)
    (set-dispatch-macro-character #\# #\Tab       'signal-sharp-error reedtable)
    (set-dispatch-macro-character #\# #\Newline   'signal-sharp-error reedtable)
    (set-dispatch-macro-character #\# #\LineFeed  'signal-sharp-error reedtable)
    (set-dispatch-macro-character #\# #\Page      'signal-sharp-error reedtable)
    (set-dispatch-macro-character #\# #\Return    'signal-sharp-error reedtable)
    (set-dispatch-macro-character #\# #\Space     'signal-sharp-error reedtable)
    reedtable))

(eval-when (load eval)
  (setq *initial-common-lisp-readtable* (make-common-lisp-readtable))
  (setq *readtable* *initial-common-lisp-readtable*))



;;;----------------------------------------------------------------------------
;;; {2} ESCAPING PREDICATES
;;;----------------------------------------------------------------------------
;;;
;;; Both the reader and the printer need to know, on occasion, whether or not a
;;; character or a symbol's print name must be escaped.  This depends on, among
;;; other things, the current readtable.
;;;----------------------------------------------------------------------------

(defun must-escape-character-p (char)
  "Return true if CHAR will have to be escaped."
  (or (lower-case-p char)
      (member (syntax-type char)
              '(:TERMINATING-MACRO :SINGLE-ESCAPE :MULTIPLE-ESCAPE))))

(defun must-escape-print-name-p (print-name)
  "Return true if PRINT-NAME will have to be escaped."
  (cond
    ((zerop (length print-name))
     T)
    ((eq (syntax-type (char print-name 0)) :NONTERMINATING-MACRO)
     T)
    (t
     (some #'(lambda (char)
               (or (lower-case-p char)
                   (member (syntax-type char)
                           '(:TERMINATING-MACRO :SINGLE-ESCAPE
                             :MULTIPLE-ESCAPE :WHITESPACE))
                   (char= char #\:)))
           print-name))))
