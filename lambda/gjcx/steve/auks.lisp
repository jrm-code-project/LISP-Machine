;-*- Mode:LISP; Package:STEVE; Readtable:CL; Base:10 -*-

;Copyright (c) May 1983 by Christopher Eliot
; and Massachusetts Institute of Technology.
;Permission to copy all or part of this material is granted, provided
; that the copies are not made or distributed for resale, the MIT
; copyright notice and reference to the source file and the software
; distribution version appear, and that notice is given that copying
; is by permission of Massachusetts Institute of Technology.

;
;This file contains random functions used by the editor, which
;do not depend heavily on other functions in the editor. Some of
;them may depend upon its data formats however. This is certainly not
;a guarantee that anything here will work in another context.
;
;*****************************************************************************
;Compilation note.
;Before installation every file in the editor should be compiled in
;an environment where the whole editor is loaded. This ensures that DEFSUBSTs
;are expanded, and that macros work correctly.
;******************************************************************************
;

(defvar *query-line*)
(defvar *notify-line*)
(defvar *more-line*)
(defvar *feedback-line*)
(defvar *double-line*)
(defvar *error-line*)
(defvar *prefix-echo-line*)

;Now 135 spaces long for bigger TTYs.
(defconstant 80spaces "                                                                                                                                       ")
(defconstant 80dashes "---------------------------------------------------------------------------------------------------------------------------------------")


;
;Ascii dependant cruft which helps fix the read stream.
;

(eval-when (compile load)

(defconstant *first-non-control-character-code* (char-int #\space))
(defconstant *first-meta-character-code* 128)
(defconstant *tab-distance* 8)

);eval-when

(defconstant n-spaces
  #.(loop with vector = (make-vector 9)    ;Initial value inconsequential
          for i from 0 to 8
          do (setf (svref vector i) (make-string i :initial-element #\sp))
          finally (return vector)))

(defconstant control-character-translation-table
  #.(loop with vect = (make-vector (char-int #\space))
                      ;Initial value inconsequential
          for i from 0 below (char-int #\space)
          for char = (int-char i)
          if (memq char '(#\bell #\backspace #\tab #\line
                                 #\page #\return #\altmode))
          do (setf (aref vect i) char)
          else do (setf (aref vect i)
                        (code-char (logior& (char-code char) #o100)
                                   char-control-bit))
          finally (return vect)))


(defconstant character-translation-table
 #.(loop with vect = (make-vector 256)     ;Initial value inconsequential
         for i from 0 to 255
         for chr = (int-char i)
         do (cond ((memq chr  '(#\bell #\backspace #\tab #\line
                                 #\page #\return #\altmode))
                   (setf (aref vect i) chr))
                  ((>=& i 128)
                   (setf (aref vect i) (code-char (logandc2& i #o0200)
                                                  char-meta-bit)))
                  (t (setf (aref vect i) chr)))
         do (cond ((memq (make-char (setq chr (aref vect i)))
                         '(#\bell #\backspace #\tab #\line
                                 #\page #\return #\altmode)))
                  ((<& (char-code chr) (char-int #\space))
                   (setf (aref vect i)
                         (code-char (logior& (char-code chr) #o100)
                                    (logior& (char-bits chr)
                                             char-control-bit)))))
         finally (return vect)))

;This is very much dependant upon the ASCII character set.
(defsubst canonicalize-control-characters (char)
  (svref character-translation-table (char-int char)))

(defun coerse-to-string-charp (char)
  (cond ((string-charp char) char)
        ((not (0p (char-font char))) nil)
        ((not (0p (logandc2 (char-bits char) #.(logior& char-control-bit
                                                        char-meta-bit))))
         nil)
        (t (let ((code (char-code char)))
             (when (not (0p (logand& (char-bits char) char-control-bit)))
               (setq code (logandc2& code #o100)))
             (when (not (0p (logand& (char-bits char) char-meta-bit)))
               (setq code (logior& code #o200)))
             (int-char code)))))

(defparameter preknown-character-printed-representations
  #.(loop with v = (make-vector char-code-limit :initial-element nil)
          for i from 0 below char-code-limit
          as c = (code-char i)
          do (unless (graphic-charp c)
               (setf (svref v i)
                     (format nil (if (and (<& i 32) (not (char-name c)))
                                     "~:C"
                                     "~@C")
                             c)))
          finally (return v)))



(defsubst stringify-tab (col)
  (let ((col1 col))
    (svref n-spaces (-& (logandc1& 7 (+& col1 8)) col1))))


(defsubst stringify-non-tab (chr)
  (svref preknown-character-printed-representations (char-code chr)))

(defsubst stringify-char (chr col)
  ;Normally a graphic-charp character will not be an argument.
  (let ((chrctr chr)
        (col1 col))
    (if (eq chrctr #\tab)
        (svref n-spaces
                ;This depends upon tabs being 8 columns apart.
                (-& (logandc1& 7 (+& col1 8)) col1))
        (svref preknown-character-printed-representations
                (char-code chrctr)))))

;
;Edit-cursors bind a buffer, a cursor and a window together.
;

(defflavor edit-cursor (buffer line position window home-line home-pos) (bp)
  :ordered-instance-variables
  :initable-instance-variables
  ;;:gettable-instance-variables
  :outside-accessible-instance-variables)

(defmethod (edit-cursor :print-self) (&optional (stream standard-output)
                                      ignore ignore)
  (format stream "#<~s ~a " 'edit-cursor (buffer-name buffer))
  (print-bp-internal line position stream)
  (write-char #\> stream))

(defun create-edit-cursor (buffer &optional (line (buffer-content buffer))
                                            (pos 0)
                                            (window nil))
  (let ((new-bp (make-instance 'edit-cursor
                               :line line
                               :position pos
                               :buffer buffer
                               :window window
                               :home-line line
                               :home-pos 0)))
    (send line :add-bp new-bp)
    (push new-bp *all-edit-cursors*)
    new-bp))

;This is how to make a new buffer. If the name is not unique, the
;existing buffer may be used however.
(defun make-buffer (name)
  (let ((buffer (buffer name :create nil)))
    (when (not (null buffer))
      (with-double-second-line
       (with-double-line
        (format terminal-io "A buffer named ~a (file ~a) already exists."
                name (send (buffer-file-name buffer) :string-for-editor))
        (format terminal-io
                "~&Type buffer name to use, or CR to reuse ~a (or <DEL> to select it): "
                name)
        (let ((new-name (prescan #'read-buffer-name nil)))
          (cond ((null new-name)
                 )
                ((string= new-name "")
                 (when (buffer-modified? buffer)
                   (when (with-query-line
                          (oustr "Save changes to buffer? " terminal-io)
                          (ed-y-or-n-p "Save changes to ~a "
                                       (buffer-name buffer)))
                     (save-file buffer)))
                 (%kill-buffer-primitive buffer)
                 (setq buffer nil))
                (t (setq name new-name)
                   (setq buffer (buffer name :create t))))))))
    (when (null buffer)
      ;; This delays calculation of the environment until
      ;; it is actually needed.
      (setf buffer (make-instance 'buffer
                                  :name name
                                  :access buffer-access-any
                                  :environment nil
                                  :mark-ring (make-vector mark-ring-size
                                                          :initial-element nil)
                                  :mark-ring-index 0
                                  :narrow-info nil))
      (setf (buffer-content buffer) (make-line buffer nil nil))
      (setf (buffer-modified? buffer) nil)
      (push buffer *all-buffers*)
      (send buffer :set-file-name (editor-default name))
      (when *buffer-creation-hook*
        (funcall *buffer-creation-hook* buffer)))
    buffer))

(defun make-point (name &optional (window nil))
  (let ((buff (make-buffer name)))
    (create-edit-cursor buff (buffer-content buff) 0 window)))

(defun select-point (point &aux (buffer (edit-cursor-buffer point)))
  (unless (eq *editor-buffer* buffer)
    (setq *last-buffer-selected* *editor-buffer*))
  (unless (eq *editor-cursor* point)
    (setq *editor-cursor* point
          *.* point
          *editor-buffer* buffer
          *b* *editor-buffer*
          *context-change-flag* t))
  (when (null (edit-cursor-window point))
    (with-no-passall
     (oustr "Selected point has no associated window." echo-area-window)
     (oustr "I will try to fix it" echo-area-window)
     (one-window))))

(defun select-point-in-current-window (point)
  (unless (eq point *editor-cursor*)
    (setf (edit-cursor-window point) (edit-cursor-window *editor-cursor*)
          (edit-cursor-window *editor-cursor*) nil)
    (select-point point)))



;
;Character syntax for STEVE.
;
;What we need is a way to set/read the syntax for any STRING-CHAR
;For now we will assume there are at most 8 bits of information
;about any character.
;This will be stored in a 256 byte table.
;

(defvar syntax-bit-map nil)

(defvar *all-modes* nil)

;Type must be an symbol.
;When using the syntax base option, the base must be declared before it
;is used.
(defmacro declare-syntax-type (type &optional (base nil))
  `(progn (defvar ,type ,(if (null base)
                             '(make-empty-syntax-table)
                             `(copy-syntax-table ,base)))
          (defprop ,type t syntax-type)
          (or (memq ,type *all-modes*)
              (push ,type *all-modes*))))

(defun make-empty-syntax-table ()
 (make-bits (* 256 8)))

(defun copy-syntax-table (table)
  (bits-replace (make-empty-syntax-table) table 0 0 (* 256 8)))

(defvar next-unused-syntax-bit 0)
(defvar max-syntax-bit 7)
(defmacro declare-syntax-bit (name)
 ;This allocates a bit automatically, and sets the name to reference it.
 `(progn (defvar ,name)
         (when (null (get ',name 'syntax-bit))
          (when (>& next-unused-syntax-bit max-syntax-bit)
           (ed-lose "Too Many Syntax Bits Allocated"))
          (putprop ',name next-unused-syntax-bit 'syntax-bit)
          (setq next-unused-syntax-bit (1+& next-unused-syntax-bit))
          (setq ,name (get ',name 'syntax-bit))
          (defvar ,(intern (string-append name "-MASK"))
                  (^& 2 ,name))
          (push (list ,name ',name (^& 2 ,name)) syntax-bit-map))))

(defun syntax-description (table char &aux (byte (get-char-syntax table char)))
 (loop for (bit-number name) in syntax-bit-map
       if (logbitp& bit-number byte)
       collect name))

;This can be expanded by SETF.
(defmacro get-char-syntax (table char)
 `(get-a-byte ,table (char-code ,char)))

;This can be expanded by SETF.
(defmacro get-char-syntax-bit (table char bit)
 `(bit ,table (+& (*& 8 (char-code ,char)) ,bit)))

(defmacro of-syntax (char bit)
  `(=& 1 (get-char-syntax-bit syntax-table ,char ,bit)))


;
;Syntax table generation.
;
;Paren matching.
;Paren matching is more wired in than in emacs.
;We assume that:
;       Open paren For  Close paren
;       (               )
;       [               ]
;       {               }
;       <               >
;And all others will either be illegal as parens, or match themselves.
;Any complaints?
;

(defvar paren-matches
        '((#\) . #\()
          (#\] . #\[)
          (#\} . #\{)
          (#\> . #\<)
          (#\' . #\`)

          (#\( . #\))
          (#\[ . #\])
          (#\{ . #\})
          (#\< . #\>)
          (#\` . #\')))

(defmacro get-paren-match (paren)
  `(cdr (assq ,paren paren-matches)))

;Syntax bit declarations.

(declare-syntax-bit word-alphanumeric)
(declare-syntax-bit lisp-alphanumeric)
(declare-syntax-bit white-space)
(declare-syntax-bit paren-open)
(declare-syntax-bit paren-close)
(declare-syntax-bit string-quote)
(declare-syntax-bit character-quote)
(declare-syntax-bit prefix)

(defparameter lisp-word-chars ".")

(defparameter text-word-chars "'")

(defparameter lisp-atom-chars "!#&*+/<=>?@^`-_:\\[]")

(defparameter extra-alphanumerics "$%")

(defparameter white-space-chars #.(to-string '(#\space #\tab #\return)))

(defparameter prefix-chars "':`,#;\\")

(defun set-to-syntax (table string syntax)
  (loop for i from 0 below (string-length string)
        do (setf (get-char-syntax-bit table (char string i) syntax) 1)))

(defun set-default-syntax (table)
  (loop for i from 0 below 256
        for chr = (int-char i)
        if (alphanumericp chr)
        do (setf (get-char-syntax-bit table chr word-alphanumeric) 1
                 (get-char-syntax-bit table chr lisp-alphanumeric) 1))

  (set-to-syntax table extra-alphanumerics word-alphanumeric)
  (set-to-syntax table extra-alphanumerics lisp-alphanumeric)

  (set-to-syntax table lisp-word-chars lisp-alphanumeric)
  (set-to-syntax table lisp-atom-chars lisp-alphanumeric)

  (set-to-syntax table white-space-chars white-space)

  (set-to-syntax table prefix-chars prefix)

  (setf (get-char-syntax-bit table #\( paren-open) 1)
  (setf (get-char-syntax-bit table #\) paren-close) 1)
  (setf (get-char-syntax-bit table #\" string-quote) 1)
  (setf (get-char-syntax-bit table #\| string-quote) 1)
  (setf (get-char-syntax-bit table #\\ character-quote) 1))

;
;
;Syntax types.
;

(declare-syntax-type *fundamental-syntax*)
(set-default-syntax *fundamental-syntax*)

(declare-syntax-type *text-syntax*)
(set-default-syntax *text-syntax*)
(set-to-syntax *text-syntax* text-word-chars word-alphanumeric)

(declare-syntax-type *lisp-syntax*)
(set-default-syntax *lisp-syntax*)
(set-to-syntax *lisp-syntax* lisp-word-chars word-alphanumeric)

;;;The current value of SYNTAX-TABLE is the current syntax.
(defvar syntax-table *lisp-syntax*)



;
;Syntax table usage.
;

(defsubst atom-char? (chr)
 (=& 1 (get-char-syntax-bit syntax-table chr lisp-alphanumeric)))

(defsubst word-char? (chr)
 (=& 1 (get-char-syntax-bit syntax-table chr word-alphanumeric)))

(defsubst white-space? (chr)
 (=& 1 (get-char-syntax-bit syntax-table chr white-space)))

(defsubst horizontal-white-space? (chr)
 (and (white-space? chr) (not (char= chr #\return))))


;
;Macros to do some fancy line control stuff.
;

(defmacro with-line (line &body forms)
 `(unwind-protect
   (progn (cursorpos ,line 0) (send terminal-io :clear-eol) ,@forms)
   (cursorpos ,line 0)
   (send terminal-io :clear-eol)))

(defmacro with-line-remaining (line &body forms)
 `(progn (cursorpos ,line 0) (send terminal-io :clear-eol) ,@forms))

(defmacro declare-line (name line &aux (arg (gensym)))
 `(progn (defmacro ,(intern (string-append "WITH-" name "-LINE")) (&body ,arg)
          (list* 'with-line ',line ,arg))
         (defmacro ,(intern (string-append "WITH-" name "-LINE-REMAINING"))
                     (&body ,arg)
           (list* 'with-line-remaining ',line ,arg))))

(declare-line query *query-line*)
(declare-line notify *notify-line*)
(declare-line more *more-line*)
(declare-line feedback *feedback-line*)
(declare-line double *double-line*)
(declare-line double-second (1+& *double-line*))
(declare-line error *error-line*)
(declare-line prefix-echo *prefix-echo-line*)

;
;Functions to do overwritten displays.
;This has to be retro-fitted to the editor.
;

(defvar *overwrite-line* 0)

(defun overwrite-open-line (line)
 (cursorpos line 0 terminal-io)
  (send terminal-io :clear-eol)
 (setq creamed-tty-lines-to
       (max& (1+& line) creamed-tty-lines-to))
 (setq *overwrite-line* line)
 t)

(defun overwrite-home ()
 (overwrite-open-line 0))

(defun overwrite-start ()
 (overwrite-open-line *overwrite-line*))

(defun overwrite-done ()
 (setq *overwrite-line* (1+& *overwrite-line*)))

(defun overwrite-terpri ()
  (setq *overwrite-line* (1+& *overwrite-line*))
  (when (>& *overwrite-line* *last-overwrite-line*)
    (setq *overwrite-line* 0)
    (with-more-line
      (setq creamed-tty-lines-to (max& (send terminal-io :linenum)
                                       creamed-tty-lines-to))
      (oustr "*more*" terminal-io)
      (cond ((char= (peek-char&save) #\space)
             (read-char&save))
            (t (when (char= (peek-char&save) #\rubout)
                 (read-char&save))
               (ed-abort)))))
  (overwrite-open-line *overwrite-line*))

(defun editor-notify (string)
 (with-notify-line-remaining
  (princ string terminal-io)))


;
;Special purpose IO functions.
;

;The editor needs to save the last 60 chars for the help command.
;It also must be able to do keyboard macros.
;Any character read from the keyboard must be read by this function,
;which will take care of all such considerations.
;
;However, there is one problem with these functions.
;If the last character read is pushed back, it will still be in the buffer.
;If this is a problem, check the flag (pushback-flag).
;

;I wonder if this would be better done by defining a special purpose stream?

(defvar 60char-buffer (make-string 60 :initial-element #\space))
(defvar 60char-index 0)
(defvar 1char-buffer #\null)
(defvar save-chars-for-keyboard-macros nil)
(defvar pushback-flag nil)
(defvar keyboard-macro-char-list nil)
(defvar executing-keyboard-macro nil)

(defun read-char&save (&optional (stream terminal-io))
  (cond ((not (null pushback-flag))
         (unless (if (not (null executing-keyboard-macro))
                     (char= (pop executing-keyboard-macro) 1char-buffer)
                     (char= (send stream :read-char) 1char-buffer))
           ;;(ed-warning "Character reader out of sync")
           nil))
        ((not (null executing-keyboard-macro))
         ;;Note that this ignores the stream.
         ;;This might screw somthing terribly, but what else
         ;;is there to do?
         (setq 1char-buffer (pop executing-keyboard-macro))
         (when (null executing-keyboard-macro)
           (send terminal-io :set-device-mode :passall *editor-device-mode*)))
        (t (setq 1char-buffer (send stream :read-char))
           (setf (char 60char-buffer 60char-index) 1char-buffer)
           (setq 60char-index (\\& (1+& 60char-index) 60))
           (when save-chars-for-keyboard-macros
             (push 1char-buffer keyboard-macro-char-list))))
  (setq pushback-flag nil)
  1char-buffer)

(defun string-read&save (char-list)
  (loop for char in char-list
        do (setf (char 60char-buffer 60char-index) char)
        do (setq 60char-index (\\& (1+& 60char-index) 60)))
  (setq pushback-flag nil)
  (setq 1char-buffer (car (last char-list))))

(defun unread-char&save (char &optional (stream terminal-io))
  (cond ((not (null pushback-flag)) (ed-lose "Double Unread&save"))
        (executing-keyboard-macro
         (push char executing-keyboard-macro))
        (t (send stream :unread-char char)))
  (setq pushback-flag t)
  char)

(defun peek-char&save (&optional (stream terminal-io))
  (unread-char&save (read-char&save stream) stream))

;
;Functions to interact with the user.
;These should be used when possible, so that the interaction is
;as ritualized as possible. This does a great deal to make the editor
;(or any program) easy to use.
;

(defresource reading-buffer ()
  :constructor (make-array 100 :element-type 'string-char :fill-pointer 0
                           :adjustable t)
  :deinitializer (reset-fill-pointer object 0))


(defun readline-with-exit (stream)
  (using-resource (reading-buffer b)
    (loop for char = (read-char&save stream)
          do (case char
               (#\bell (ed-abort :echo terminal-io))
               ((#\altmode #\return) (return (copy-seq b)))
               (t (vector-push-extend char b))))))


(defun read-symbol (stream)
  (using-resource (reading-buffer b)
    (loop for chr = (read-char&save stream)
          do (if (or (null chr) (white-space? chr))
                 (return (intern (copy-seq b)))
                 (vector-push-extend (char-upcase chr) b)))))

;These should do completion etc.

(defun read-file-name (&optional (stream terminal-io stream?) &aux name)
 (cond ((null stream?)
        (setq name (si:ttyscanner #'readline-with-exit terminal-io terminal-io
                                  #'(lambda (strm reason)
                                     (oustr "File: " strm))
                                  T 0))
        (if (not (stringp name))
            (ed-warn :aborted)
            name))
       (t (readline-with-exit stream))))

;
;Completion of buffer names.
;This does not really belong in this file because it depends heavily upon
;being part of the editor. It is included here because it belongs with the
;previous functions.
;

;Is common lisp really so damned brain dead as to think that anyone
;will ever want STRING to return "NIL" for the empty list. I would
;like to see someone come up with three examples of natural uses
;for that type of mis-behavior. I can come up with 95 natural
;examples where that behavior has fucked me over.
(defun steve-to-string (seq)
  (cond ((stringp seq) seq)
        ((null seq) (make-string 0))
        ((symbolp seq) (get-pname seq))
        ((characterp seq)
           ;Note we know the implication of string-charp here.
           (if (string-charp seq)
               (aref *:character-to-string-table (char-code seq))
               (cerror t nil ':implementation-lossage
                       "The character ~S can't go into a string"
                       seq)))
        ((pairp seq)
         (let* ((n (list-length seq))
                (s (make-string n :initial-element #\space)))
           (loop for c in seq for i from 0
                 do (rplachar s i (to-character c)))
           s))
        ((bitsp seq)
         (let* ((n (bits-length seq))
                (s (make-string n :initial-element #\space)))
               (dotimes (i n) (rplachar s i (char "01" (bit seq i))))
               s))
        ((vectorp seq)
         (let* ((n (vector-length seq))
                (s (make-string n :initial-element #\space)))
           (dotimes (i n)
             (rplachar s i (to-character (aref seq i))))
           s))
        ((and (si:extendp seq) (send seq ':send-if-handles ':to-string)))
        (t (steve-to-string (cerror t () ':wrong-type-argument
                               "~*~S not coercible to a string"
                               'string seq)))))

(defun complete-buffer-name (name)
 (loop with completion = nil
       with length = (string-length name)
       for buffer in *all-buffers*
       for bname = (buffer-name buffer)
       if (string-equal name bname 0 0
                        length (min& (string-length bname) length))
       do (cond ((null completion) (setq completion bname))
                (t (setq completion (max-prefix (list completion bname)))))
       finally (return (if (or (null completion)
                               (<=& (string-length completion) length))
                           name
                           completion))))

(defun print-buffer-name-completions (name)
 (overwrite-start)
 (loop with length = (string-length name)
       for buffer in *all-buffers*
       for bname = (buffer-name buffer)
       if (string-equal name bname 0 0
                        length (min& (string-length bname) length))
       do (progn (princ bname terminal-io) (overwrite-terpri)))
 (oustr "Done" terminal-io)
 (overwrite-terpri)
 (overwrite-done))

(defun read-buffer-name (stream &aux name)
 (loop for chr = (read-char&save stream)
       do (cond ((char= chr #\return)
                 (return
                  ;;If the only buffers are named FOO and FROB
                  ;;then completing "" will => "F" and so ^X ^B <CR>
                  ;;will incorectly create a buffer named "F" if we are not
                  ;;careful here.
                  (cond ((not (null *buffer-name-completion-on-return*))
                         (setq name (complete-buffer-name
                                     (steve-to-string chrs)))
                         (if (buffer name :create nil)
                             name
                             (steve-to-string chrs)))
                        (t (steve-to-string chrs)))))
                ((char= chr #\altmode)
                 (send stream :completion-string
                       (setq name (complete-buffer-name
                                   (steve-to-string chrs)))
                       (1+& (length chrs)))
                 (when (buffer name :create nil)
                  (send stream :send-if-handles :echo-false-char #\$))
                 (throw 'rubout-tag nil))
                ((char= chr #\page)
                 (make-screen-image)
                 (send stream :rubout)
                 (throw 'rubout-tag nil))
                ((char= chr #\?)
                 (print-buffer-name-completions
                  (steve-to-string chrs))
                 (send stream :rubout)
                 (throw 'rubout-tag nil)))
       collect chr into chrs))

;(defun beep () (write-char #\bell))    ;this in newmacros and renamed steve-beep to avoid lossage.

(defun ed-y-or-n-p (&optional (help "Type Y or N")
                    &RESTV format-args)
  (do ((char)
       (c-pos (cursorpos terminal-io)))
      (nil) ;Forever.
      (cursorpos (car c-pos) (cdr c-pos) terminal-io)
      (setq char (read-char&save))
      (send terminal-io :write-char char)
      (case char
        ((#\Y #\y) (oustr "es" terminal-io)
                   (return t))
        ((#\N #\n) (write-char #\o terminal-io)
                   (return nil))
        (#\? (with-notify-line-remaining
              (lexpr-funcall #'format terminal-io help format-args)))
        (#\bell (ed-abort :echo terminal-io))
        (t (with-notify-line-remaining
            (format terminal-io "Type Y or N. ? for help"))))))



;Function to "bind" passall mode off.

(defmacro with-no-passall (&body forms &aux (var (gentemp)))
  `(let ((,var (send terminal-io :get-device-mode :passall)))
     (unwind-protect
       (progn (send terminal-io :set-device-mode :passall nil)
              ,@forms)
       (send terminal-io :set-device-mode :passall ,var))))

;
;Emacs rings.
;

;First some primatives.
(defvar mark-ring-size 8)
(defvar mark-ring (make-vector mark-ring-size :initial-element nil))
(defvar mark-ring-index 0)

(defun push-mark (bp)
 (let ((mark-ring-index (buffer-mark-ring-index *editor-buffer*)))
  (setq mark-ring-index (1+& mark-ring-index))
  (if (>=& mark-ring-index mark-ring-size)
      (setq mark-ring-index 0))
  (setf (svref (buffer-mark-ring *editor-buffer*) mark-ring-index) (copy-bp bp)
        (buffer-mark-ring-index *editor-buffer*) mark-ring-index)))

;Use this when programing a function that sometimes pushes a mark.
;This will print a mark-set message when called, if appropriate.
(defun auto-push-mark (bp)
  (unless (string= "" auto-push-point-notification)
    (with-error-line-remaining
     (oustr auto-push-point-notification terminal-io)))
  (push-mark bp))

(defun push-mark-no-copy (bp)
  (let ((mark-ring-index (buffer-mark-ring-index *editor-buffer*)))
    (setq mark-ring-index (1+& mark-ring-index))
    (if (>=& mark-ring-index mark-ring-size)
        (setq mark-ring-index 0))
    (setf (aref (buffer-mark-ring *editor-buffer*) mark-ring-index) bp
          (buffer-mark-ring-index *editor-buffer*) mark-ring-index)))

(defun pop-mark ()
  (let ((ring (buffer-mark-ring *editor-buffer*))
        (index (buffer-mark-ring-index *editor-buffer*)))
    (prog1 (or (svref ring index) (ed-warn :no-mark))
           ;;This code is only to keep narrowing from losing.
           (when (buffer-narrow-info (bp-buffer (svref ring index)))
             (when (loop with mark = (svref ring index)
                         with target = (bp-line mark)
                         for line first (buffer-content (bp-buffer mark))
                         then (line-next line)
                         never (eq line target))
               (ed-lose "Mark outside region")))
           ;;
           (loop do (setq index (1-& index))
                 do (when (-p index) (setq index (1-& mark-ring-size)))
                 until (svref ring index))
           (setf (buffer-mark-ring-index *editor-buffer*) index))))

(defun get-mark (&aux ring index)
  (prog1 (or (aref (setq ring (buffer-mark-ring *editor-buffer*) )
                   (setq index (buffer-mark-ring-index *editor-buffer*)))
             (ed-warn :no-mark))
         ;;This code is only to keep narrowing from losing.
         (when (buffer-narrow-info (bp-buffer (svref ring index)))
           (when (loop with mark = (svref ring index)
                       with target = (bp-line mark)
                       for line first (buffer-content (bp-buffer mark))
                       then (line-next line)
                       never (eq line target))
             (ed-lose "Mark outside region")))
         ;;
         ))

(defun edit-cursor-goto (buffer line position)
  (when (not (eq buffer (bp-buffer *editor-cursor*)))
    (point-selected buffer :create nil))
  (send *editor-cursor* :move line position))

(defun goto-mark (mark)
  (unless (null mark)
    (edit-cursor-goto (bp-buffer mark) (bp-line mark) (bp-position mark))))

;;; Some kill primitives have rightly been moved to KILLS.LSP

;
;String comparison.
;

(defun string-search-for-substring (chars string
                                          &optional (start 0)
                                          (chars-length (string-length chars)))
  (or (string= string "")
      (let ((c1 (char string 0))
            (c2)
            (len (string-length string))
            (last))
        (setq c2 (char-upcase c1)
              c1 (char-downcase c1)
              last (-& chars-length len))
        (or (loop for pos first (%string-posq c1 chars start chars-length)
                  then (%string-posq c1 chars pos (-& chars-length pos))
                  never (or (null pos) (>& pos last))
                  if (loop for j from 1 below len
                           always (char-equal (char string j)
                                              (char chars (+& pos j))))
                  return (if (<& pos chars-length) pos nil)
                  do (setq pos (1+& pos)))
            (loop for pos first (%string-posq c2 chars start chars-length)
                  then (%string-posq c2 chars pos (-& chars-length pos))
                  never (or (null pos) (>& pos last))
                  if (loop for j from 1 below len
                           always (char-equal (char string j)
                                              (char chars (+& pos j))))
                  return (if (<& pos chars-length) pos nil)
                  do (setq pos (1+& pos)))))))

(defun delimited-substring-search (chars string
                                         &optional (start 0)
                                         (chars-length (string-length chars)))
  (or (string= string "")
      (let ((c1 (char string 0))
            (c2)
            (len (string-length string))
            (last))
        (setq c2 (char-upcase c1)
              c1 (char-downcase c1)
              last (-& chars-length len))
        (or (loop for pos first (%string-posq c1 chars start chars-length)
                  then (%string-posq c1 chars pos (-& chars-length pos))
                  never (or (null pos) (>& pos last))
                  if (and (or (0p pos)
                              (white-space? (char chars (1-& pos))))
                          (or (=& pos last)
                              (white-space? (char chars (+& pos len))))
                          (loop for j from 1 below len
                                always (char-equal (char string j)
                                                   (char chars (+& pos j)))))
                  return (if (<& pos chars-length) pos nil)
                  do (setq pos (1+& pos)))
            (loop for pos first (%string-posq c2 chars start chars-length)
                  then (%string-posq c2 chars pos (-& chars-length pos))
                  never (or (null pos) (>& pos last))
                  if (and (or (0p pos)
                              (white-space? (char chars (1-& pos))))
                          (or (=& pos last)
                              (white-space? (char chars (+& pos len))))
                          (loop for j from 1 below len
                                always (char-equal (char string j)
                                                   (char chars (+& pos j)))))
                  return (if (<& pos chars-length) pos nil)
                  do (setq pos (1+& pos)))))))


;More randomness.


;The argument is not the default.
(defmacro argument? ()
 '(or (not (=& *argument* 1)) argument-supplied?))

;The argument was C-U with no number.
(defmacro c-u-only? ()
  `(and (=& *argument* 4) (null argument-supplied?)))

;An argument is supplied, not just C-U.
(defmacro real-arg-sup? ()
  `(or argument-supplied?
       (and (not (=& *argument* 1))
            (not (=& *argument* 4)))))

(defmacro home-line ()
 '(edit-cursor-home-line *editor-cursor*))

(defmacro current-line ()
 '(edit-cursor-line *editor-cursor*))

(defmacro current-position ()
 '(edit-cursor-position *editor-cursor*))

(defmacro current-window ()
 '(edit-cursor-window *editor-cursor*))

(defmacro advance1 ()
 '(send *editor-cursor* :advance-pos 1))

(defmacro -advance1 ()
 '(send *editor-cursor* :advance-pos -1))

(defun buffer-begin? (&optional (bp *editor-cursor*))
  (and (null (line-previous (bp-line bp)))
       (0p (bp-position bp))))

(defun buffer-end? (&optional (bp *editor-cursor*))
  (and (null (line-next (bp-line bp)))
       (=& (bp-position bp)
           (line-char-count (bp-line bp)))))

(defun last-line? (&optional (bp *editor-cursor*))
  (null (line-next (bp-line bp))))

(defun first-line? (&optional (bp *editor-cursor*))
  (null (line-previous (bp-line bp))))

(defun not-buffer-begin (&optional (bp *editor-cursor*))
  (when (and (null (line-previous (bp-line bp)))
             (0p (bp-position bp)))
    (ed-warn :at-start)))

(defun not-buffer-end (&optional (bp *editor-cursor*))
  (when (and (null (line-next (bp-line bp)))
             (=& (bp-position bp)
                 (line-char-count (bp-line bp))))
    (ed-warn :at-end)))

(defun not-first-line (&optional (bp *editor-cursor*))
  (when (null (line-previous (bp-line bp)))
    (ed-warn :at-first-line)))

(defun not-last-line (&optional (bp *editor-cursor*))
  (when (null (line-next (bp-line bp)))
    (ed-warn :at-last-line)))

(defun check-line-map-is-current (point)
  (loop with win = (edit-cursor-window point)
        for i from (window-y-pos win) below (+& (window-y-pos win)
                                                (window-y-size win))
        with line = (edit-cursor-home-line point)
        for old-map-line first (aref *line-map* i) then map-line
        for map-line = (aref *line-map* i)
        always (or (eq map-line line) (eq map-line old-map-line))
        if (eq map-line line) do (and line (setq line (line-next line)))))

(defun buffer-end-on-screen? (&optional (point *editor-cursor*))
  (let ((win (edit-cursor-window point)))
    (and (not (null win))
         (if (not (check-line-map-is-current point))
             (buffer-end? point)
             (let ((last (+& (window-y-pos win) (window-y-size win) -1)))
               (or (null (aref *line-map* last))
                   (null (line-next (aref *line-map* last)))))))))


;;
;;Some usefull functions.
;;

(defun last-line (&optional (start-line (bp-line *editor-cursor*)))
  (loop for line first start-line then next
        for next = (line-next line)
        while (not (null next))
        finally (return line)))

(defun nth-next-line (start-line n)
  (if (-p n)
      (nth-previous-line start-line (-& n))
      (loop for i from 0 to n
            for line first start-line then next
            for next = (line-next line)
            while next
            finally (return line))))

(defun nth-previous-line (start-line n)
  (if (-p n)
      (nth-next-line start-line (-& n))
      (loop for i from 0 to n
            for line first start-line then prev
            for prev = (line-previous line)
            while prev
            finally (return line))))

;;; PROCESS-WORDS-AS-STRINGS and PROCESS-REGION-AS-CHARACTERS
;;; moved to KILLS.LSP for no better reason than to put them
;;; closer to their loved ones

(defun process-region-as-lines (function &RESTV args)
  (multiple-value-bind (bp1 bp2) (order-bps *editor-cursor* (get-mark))
    (loop with limit-line = (line-next (bp-line bp2))
          for line first (bp-line bp1) then (line-next line)
          until (or (null line) (eq line limit-line))
          do (lexpr-funcall function line args))))

(defun process-region-as-lines-with-point (function &RESTV args)
  (multiple-value-bind (bp1 bp2) (order-bps *editor-cursor* (get-mark))
    (loop with temp-bp = (copy-bp *editor-cursor*)
          with limit-line = (line-next (bp-line bp2))
          for line first (bp-line bp1) then (line-next line)
          until (or (null line) (eq line limit-line))
          do (send *editor-cursor* :move line 0)
          do (lexpr-funcall function line args)
          finally (send *editor-cursor*
                        :move (bp-line temp-bp) (bp-position temp-bp))
          finally (send temp-bp :send-if-handles :expire))))

;Should BUFFER (below) call this on a list? (Using what name?)
(defun list-to-buffer (name list-of-strings)
  (if (null list-of-strings)
      (buffer name :create t) ;Fucking STRING interpretation of NIL.
      (loop with buffer = (buffer name :create t)
            with line-1 = (make-line buffer nil nil
                                     (string (car list-of-strings)))
            for string in list-of-strings
            for prev first line-1
            then (make-line buffer prev nil (string string))
            finally (setf (buffer-content buffer) line-1)
            finally (return buffer))))

;Coerse any reasonable thing to be a buffer.
;Value is NIL if object cannot be interpreted as a buffer.
(defun buffer (x &key (create t))
  (cond ((null x) nil)                  ;Makes certain code easier.
        ((of-type x 'buffer) x)
        ((stringp x)
         (loop for buffer in *all-buffers*
               if (string-equal (buffer-name buffer) x)
               return buffer
               finally (unless (null create)
                         (let ((buffer (make-buffer x)))
                           (send buffer :set-file-name
                                 (editor-default x))
                           (return buffer)))))
        ((of-type x 'pathname)
         (setq x (editor-default x))
         (let ((true (probe-file x)))
           (or (if (null true)
                   (loop for buffer in *all-buffers*
                         if (and (buffer-file-name buffer)
                                 (send (buffer-file-name buffer) :equal x))
                         return buffer)
                   (loop for buffer in *all-buffers*
                         if (and (buffer-file-name buffer)
                                 (send true :equal
                                       (probe-file (buffer-file-name buffer))))
                         return buffer))
               (unless (null create)
                 (let ((buffer (make-buffer (send x :name))))
                   (send buffer :set-file-name x)
                   (if (not (null true))
                       (read-file-into-buffer x buffer)
                       (editor-notify "New File"))
                   buffer)))))
        ;;Note the funny coding. This could work for BPs, but I don't
        ;;want to encourage that unless it seems to be needed.
        ;;(I am thinking of eliminating the BUFFER field from BPs & LINEs.)
        ((of-type x 'edit-cursor) (bp-buffer x))
        (t nil)))

;Ditto for edit-cursors.
(defun point (x &key (create t))
  (cond ((null x) nil)                  ;Makes certain code easier.
        ((of-type x 'edit-cursor) x)
        ((or (stringp x)
             (of-type x 'pathname)
             (of-type x 'buffer))
         (when (setq x (buffer x :create create))
           (or (loop with p1 = nil
                     for p in *all-edit-cursors*
                     if (eq x (edit-cursor-buffer p))
                     do (progn (setq p1 p)
                               (when (null (edit-cursor-window p))
                                 (return p)))
                     finally (when (not (null p1))
                               (return p1)))
               (and (not (null create))
                    (create-edit-cursor x)))))
        (t nil)))

(defun point-selected (x &key (create t))
  (let ((point (point x :create create)))
    (unless (null point)
      (if (edit-cursor-window point)
          (select-point point)
          (select-point-in-current-window point)))
    point))

