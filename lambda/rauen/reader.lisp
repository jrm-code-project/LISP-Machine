;;; -*- Mode:LISP; Package:LISP-IO; Base:10; Readtable:CL -*-
;;;
;;; READ.LISP
;;;
;;; Lisp reader.
;;;
;;; Still to do:
;;;   #. #, #s
;;;   Reading flonums.  1E4 doesn't work right.
;;;   how should recursive-p affect EOF errors?
;;;   T for stream argument
;;;
;;; Contents:
;;;   [1] Reader top level
;;;   [2] Guts of the reader
;;;   [3] Tokens
;;;   [4] Parsing tokens
;;;   [5] Reader macros
;;;   [6] Backquote
;;;   [7] # dispatch macros


(defvar *read-default-float-format* 'single-float
  "A floating-point number read or printed with the E marker has this type.")

(defvar *allow-dot-tokens* NIL
  "When T, if the reader encounters a token that is just a period, read that
token in as the value of *reader-dot-token*.  When NIL, raise an error if such
a token is read.")

(defconstant *reader-dot-token* (cons 'the-dot-token NIL)
  "This is the object read in when a single-dot token is read.")

;; here follows an array iteration hack...

(defun number-list (number)
  "Return the list (0 1 2 ... number - 1)"
  (nreverse (reversed-number-list number)))

(defun reversed-number-list (number)
  "Return the list (number - 1 ... 2 1 0)"
  (when (> number 0)
    (cons (1- number) (reversed-number-list (1- number)))))

(defun cartesian-product (list1 list2)
  (apply #'append
         (mapcar #'(lambda (list1-element)
                     (mapcar #'(lambda (list2-element)
                                 (cons list1-element list2-element))
                             list2))
                 list1)))

(defun generate-permutations (list)
  (if list
      (cartesian-product (number-list (car list))
                         (if (cdr list)
                             (generate-permutations (cdr list))
                             (list nil)))))

(defmacro do-array-elements ((element indices array) &body body)
  "Iterate over each element of ARRAY, with ELEMENT bound to that element and
INDICES bound to a list of the indices of that element."
  `(DOLIST (,indices (GENERATE-PERMUTATIONS (ARRAY-DIMENSIONS ,array)))
     (LET ((,element (APPLY #'AREF ,array ,indices)))
       ,@body)))



;;;----------------------------------------------------------------------------
;;; {1} READER TOP LEVEL
;;;----------------------------------------------------------------------------
;;;
;;; The functions READ, READ-PRESERVING-WHITESPACE, and READ-DELIMITED-LIST
;;; are used for both top-level and nested calls into the reader.  Their
;;; RECURSIVE-P argument indicates which type of call they are being used for.
;;;
;;; At the top level, these functions do additional work besides calling the
;;; reader.  They bind the *READER-CIRCULARITIES* association list to nil.
;;; And they generate a unique object which is used to hold the place of
;;; #n# expressions.  After an object has been read in at the top level,
;;; the #n# expressions are replaced with the #n= forms in the circularities
;;; list.
;;;----------------------------------------------------------------------------

(defvar *reader-circularities* NIL
  "An association list that keeps track of #n= and #n# business.  The car of
each element is a unique positive integer n; the cdr is the object that it
refers to.  There is also an entry for UNIQUE-IDENTIFIER.")

(defun read (&optional (input-stream *standard-input*)
                       (eof-error-p  t)
                       eof-value
                       recursive-p)
  "Read an expression from INPUT-STREAM and return it."
  (when (eq input-stream t)
    (setq input-stream *terminal-io*))
  (if recursive-p
      (read-frob input-stream eof-error-p eof-value recursive-p nil)
      (let ((*reader-circularities* NIL))
        (fill-in-the-blanks
          (read-frob input-stream eof-error-p eof-value recursive-p nil)))))


(defun read-preserving-whitespace (&optional (input-stream *standard-input*)
                                             (eof-error-p t)
                                             eof-value
                                             recursive-p)
  "Read an expression from INPUT-STREAM, leaving behind a character of
whitespace if it follows the expression, and return the expression."
  (when (eq input-stream t)
    (setq input-stream *terminal-io*))
  (if recursive-p
      (read-frob input-stream eof-error-p eof-value recursive-p T)
      (let ((*reader-circularities* NIL))
        (fill-in-the-blanks
          (read-frob input-stream eof-error-p eof-value recursive-p T)))))


(defun read-delimited-list (char &optional (input-stream *standard-input*)
                            recursive-p)
  (when (eq input-stream t)
    (setq input-stream *terminal-io*))
  (if recursive-p
      (read-delimited-frobs char input-stream recursive-p)
      (let ((*reader-circularities* NIL))
        (error "You shouldn't be doing this recursively.")
        (read-delimited-frobs char input-stream t))))


(defun read-delimited-frobs (char input-stream recursive-p)
  (let ((list NIL))
    (loop
      (let* ((next-char   (read-char input-stream t nil recursive-p))
             (syntax-type (syntax-type next-char)))
        (if (char= char next-char)
            (return)
            (cond
              ((member syntax-type '(:CONSTITUENT :SINGLE-ESCAPE
                                     :MULTIPLE-ESCAPE))
               (unread-char next-char input-stream)
               (push (read input-stream t nil t) list))
              ((member syntax-type '(:TERMINATING-MACRO :NONTERMINATING-MACRO))
               (let ((values (multiple-value-list
                               (funcall (get-macro-character next-char)
                                        input-stream
                                        next-char))))
                 (when (= (length values) 1)
                   (push (car values) list))))
              ((eq syntax-type :WHITESPACE)
               NIL)
              ((eq syntax-type :ILLEGAL)
               (error "Illegal character ~C encountered by READ." next-char))
              (t
               (error "Unrecognized syntax type ~S" syntax-type))
              ))))
    (nreverse list)))


(defconstant *placeholder-for-#n#-identifier* (cons 'placeholder nil))

(defun make-placeholder-for-#n# (n)
  (cons *placeholder-for-#n#-identifier* n))

(defun placeholder-for-#n#-p (thing)
  (and (consp thing) (eq (car thing) *placeholder-for-#n#-identifier*)))

(defun placeholder-for-#n#-n (placeholder)
  (cdr placeholder))

(defun lookup-placeholder (placeholder)
  (let* ((n   (placeholder-for-#n#-n placeholder))
         (foo (assoc n *reader-circularities*)))
    (if foo
        (cdr foo)
        (error "Placeholder #~D# not found in circularities table." n))))

(defun fill-in-the-blanks (thing)
  "Replace any #n# placeholders with what belongs in their places."
  (typecase thing
    (cons (if (placeholder-for-#n#-p (car thing))
              (rplaca thing (lookup-placeholder (car thing)))
              (fill-in-the-blanks (car thing)))
          (if (placeholder-for-#n#-p (cdr thing))
              (rplacd thing (lookup-placeholder (cdr thing)))
              (fill-in-the-blanks (cdr thing))))
    (vector (dotimes (i (length thing))
              (if (placeholder-for-#n#-p (aref thing i))
                  (setf (aref thing i) (lookup-placeholder (aref thing i)))
                  (fill-in-the-blanks (aref thing i)))))
    (array (do-array-elements (element indices thing)
             (if (placeholder-for-#n#-p element)
                 (setf (apply #'aref thing indices)
                       (lookup-placeholder element)))))
    (t nil))
  thing)



;;;----------------------------------------------------------------------------
;;; {2} GUTS OF THE READER
;;;----------------------------------------------------------------------------
;;;
;;; Shamelessly lifted from CLtL, section 22.1.1
;;;----------------------------------------------------------------------------

(defun read-frob (stream eof-error eof-value recursive preserve)
  "Step 1 of the reader.  Look at the next character on the stream and dispatch
according to what kind of character it is.  Also deal with what happens when
there aren't any more characters left."
  (let ((char (read-char stream t nil t)))
    (funcall (case (syntax-type char)
               (:ILLEGAL              'read-from-illegal-char)
               (:WHITESPACE           'read-from-whitespace-char)
               (:CONSTITUENT          'read-from-constituent-char)
               (:SINGLE-ESCAPE        'read-from-single-escape-char)
               (:MULTIPLE-ESCAPE      'read-from-multiple-escape-char)
               (:TERMINATING-MACRO    'read-from-macro-char)
               (:NONTERMINATING-MACRO 'read-from-macro-char))
             char
             stream
             eof-error
             eof-value
             recursive
             preserve)))

(defun read-from-illegal-char
       (char stream eof-error eof-value recursive preserve)
  "Step 2 of the reader.  CHAR is an illegal character."
  (declare (ignore char stream eof-error eof-value recursive preserve))
  (error "Illegal character encountered in READ."))

(defun read-from-whitespace-char
       (char stream eof-error eof-value recursive preserve)
  "Step 3 of the reader.  CHAR is a whitespace character."
  (declare (ignore char))
  (read-frob stream eof-error eof-value recursive preserve))

(defun read-from-macro-char
       (char stream eof-error eof-value recursive preserve)
  "Step 4 of the reader.  CHAR is a macro character."
  (let ((macro-values (multiple-value-list
                        (funcall (get-macro-character char)
                                 stream
                                 char))))
    (if (= (length macro-values) 1)
        (first macro-values)
        (read-frob stream eof-error eof-value recursive preserve))))

(defun read-from-single-escape-char
       (char stream eof-error eof-value recursive preserve)
  "Step 5 of the reader.  CHAR is a single escape character."
  (declare (ignore char eof-error eof-value))
  (let ((char (read-char stream nil nil recursive)))
    (if char
        (accumulate-token (augment-token (start-token) char :ALPHABETIC)
                          stream
                          recursive
                          preserve
                          'finish-accumulation)
        (error "End of file encountered after single escape character in READ.")
        )))

(defun read-from-multiple-escape-char
       (char stream eof-error eof-value recursive preserve)
  "Step 6 of the reader.  CHAR is a multiple escape character."
  (declare (ignore char eof-error eof-value))
  (accumulate-token-in-bars (start-token)
                            stream
                            recursive
                            preserve
                            'finish-accumulation))

(defun read-from-constituent-char
       (char stream eof-error eof-value recursive preserve)
  "Step 7 of the reader.  CHAR is a constituent character."
  (declare (ignore eof-error eof-value))
  (accumulate-token (augment-token (start-token) (char-upcase char))
                    stream
                    recursive
                    preserve
                    'finish-accumulation))

(defun accumulate-token (token stream recursive preserve continue)
  "Step 8 of the reader.  TOKEN is being accumulated, and an even number of
multiple escape characters have been encountered.  Funcall CONTINUE with one
argument, the token, when it has been accumulated."
  (let ((char (read-char stream nil nil recursive)))
    (if char
        (case (syntax-type char)
          (:CONSTITUENT
           (augment-token token (char-upcase char))
           (accumulate-token token stream recursive preserve continue))
          (:NONTERMINATING-MACRO
           (augment-token token (char-upcase char))
           (accumulate-token token stream recursive preserve continue))
          (:SINGLE-ESCAPE
           (let ((char (read-char stream))) ;perhaps a more explicit error?
             (augment-token token char :ALPHABETIC)
             (accumulate-token token stream recursive preserve continue)))
          (:MULTIPLE-ESCAPE
           (accumulate-token-in-bars token stream recursive preserve continue))
          (:ILLEGAL
           (error "Illegal character encountered in READ."))
          (:TERMINATING-MACRO
           (unread-char char stream)
           (funcall continue token))
          (:WHITESPACE
           (when preserve (unread-char char stream))
           (funcall continue token)))
        (funcall continue token))))

(defun accumulate-token-in-bars (token stream recursive preserve continue)
  "Step 9 of the reader.  TOKEN is being accumulated, and an odd number of
multiple escape characters have been encountered.  Funcall CONTINUE with one
argument, the token, when it has been accumulated."
  (let ((char (read-char stream nil nil recursive)))
    (if char
        (case (syntax-type char)
          (:CONSTITUENT
           (augment-token token char :ALPHABETIC)
           (accumulate-token-in-bars token stream recursive preserve continue))
          (:NONTERMINATING-MACRO
           (augment-token token char :ALPHABETIC)
           (accumulate-token-in-bars token stream recursive preserve continue))
          (:TERMINATING-MACRO
           (augment-token token char :ALPHABETIC)
           (accumulate-token-in-bars token stream recursive preserve continue))
          (:WHITESPACE
           (augment-token token char :ALPHABETIC)
           (accumulate-token-in-bars token stream recursive preserve continue))
          (:SINGLE-ESCAPE
           (let ((char (read-char stream))) ;prehaps a more explicit error?
             (augment-token token char :ALPHABETIC)
             (accumulate-token-in-bars
               token stream recursive preserve continue)))
          (:MULTIPLE-ESCAPE
           (accumulate-token token stream recursive preserve continue))
          (:ILLEGAL
           (error "Illegal character encountered in READ.")))
        (error "Unexpected end of file in READ."))))

(defun finish-accumulation (token)
  "Step 10 of the reader.  TOKEN has been accumulated."
  (identify-alphadigits token)
  (parse-token token))



;;;----------------------------------------------------------------------------
;;; {3} TOKENS
;;;----------------------------------------------------------------------------
;;;
;;; The possible attributes are
;;;     :ALPHABETIC
;;;     :DIGIT
;;;     :ALPHADIGIT     (A-Z or a-z, if *read-base* says it _could_ be a digit)
;;;     :SIGN           (+ or -)
;;;     :RATIO-MARKER   (/)
;;;     :DECIMAL        (.)
;;;     :EXTENSION      (^ or _)
;;;     :PACKAGE-MARKER (:)
;;;     :NUMBER-MARKER  (D, E, F, L, S, d, e, f, l, s)
;;;----------------------------------------------------------------------------

(defstruct (token
             (:constructor start-token ()))
  (characters (make-array 0 :adjustable T :fill-pointer T))
  (attributes (make-array 0 :adjustable T :fill-pointer T))
  contains-decimal-p
  contains-number-marker-p
  (scratch-pointer 0))

(defun augment-token (token character &optional attribute)
  (let ((char-attribute
          (cond (attribute attribute)
                ((find character "0123456789") :DIGIT)
                ((and (token-contains-decimal-p token)
                      (find character "DEFLSdefls")
                      (not (token-contains-number-marker-p token)))
                 (setf (token-contains-number-marker-p token) T)
                 :NUMBER-MARKER)
                ((digit-char-p character *read-base*) :ALPHADIGIT)
                ((find character "+-") :SIGN)
                ((char= character #\/) :RATIO-MARKER)
                ((char= character #\.)
                 (setf (token-contains-decimal-p token) T)
                 :DECIMAL)
                ((find character "^_") :EXTENSION)
                ((char= character #\:) :PACKAGE-MARKER)
                (t :ALPHABETIC))))
    (vector-push-extend character (token-characters token))
    (vector-push-extend char-attribute (token-attributes token)))
  token)

(defun identify-alphadigits (token)
  "If the token looks like it's a floating point number, then the unidentified
alphadigits must be alphabetic.  Otherwise, they must be digits."
  (let ((token-attributes (token-attributes token))
        (attribute        (if (or (token-contains-decimal-p token)
                                  (token-contains-number-marker-p token))
                              :ALPHABETIC
                              :DIGIT)))
    (dotimes (i (length token-attributes))
      (when (eq (aref token-attributes i) :ALPHADIGIT)
        (setf (aref token-attributes i) attribute)))))

(defun potential-number-p (token)
  "True if TOKEN is a potential number, according to the rules in CLtL, section
22.1.2"
  (let ((attributes (token-attributes token)))
    (and (every #'(lambda (attrib)
                    (member attrib '(:DIGIT :SIGN :RATIO-MARKER :DECIMAL
                                     :EXTENSION :NUMBER-MARKER)))
                attributes)
         (some  #'(lambda (attrib)
                    (eq attrib :DIGIT))
                attributes)
         (member (aref attributes 0) '(:DIGIT :SIGN :DECIMAL :EXTENSION))
         (not (eq (aref attributes (1- (length attributes))) :SIGN))
         T)))

(defun all-dots-token-p (token)
  "True if TOKEN consists entirely of periods."
  (every #'(lambda (char) (char= char #\.)) (token-characters token)))

(defun token-name (token)
  (coerce (token-characters token) 'string))



;;;----------------------------------------------------------------------------
;;; {4} PARSING TOKENS
;;;----------------------------------------------------------------------------

(defun parse-token (token)
  (cond (*read-suppress*
         NIL)
        ((potential-number-p token)
         (let ((number (number-token-p token)))
           (if number
               number
               (progn
                 (warn "Potential number; treated as symbol.")
                 (parse-symbol-string
                   (coerce (token-characters token) 'string))))))
        ((all-dots-token-p token)
         (if (and (= (length (token-characters token)) 1) *allow-dot-tokens*)
             *reader-dot-token*
             (error "Illegal dot token encountered by reader.")))
        (t
         (parse-symbol-string (coerce (token-characters token) 'string)))))


(defun parse-digit-list (digit-list radix)
  (let ((value 0))
    (dolist (digit digit-list)
      (let ((digit-value (digit-char-p digit radix)))
        (if digit-value
            (setq value (+ digit-value (* radix value)))
            (let ((*print-base* 10.))
              (error "~C is not a valid digit in base ~S." digit radix)))))
    value))


(defun number-token-p (token)
  (catch 'quit
    (setf (token-scratch-pointer token) 0)
    (let ((mantissa-sign       (get-characters token :SIGN          0 1))
          (digits-before-break (get-characters token :DIGIT         0 NIL))
          (decimal-point       (get-characters token :DECIMAL       0 1))
          (ratio-bar           (get-characters token :RATIO-MARKER  0 1))
          (digits-after-break  (get-characters token :DIGIT         0 NIL))
          (exponent-marker     (get-characters token :NUMBER-MARKER 0 1))
          (exponent-sign       (get-characters token :SIGN          0 1))
          (exponent-digits     (get-characters token :DIGIT         0 NIL)))
      (no-more-characters token)
      (let ((sign      (if (string= mantissa-sign "-") -1 1))
            (exponentp (cond ((and exponent-marker exponent-digits) T)
                             ((or exponent-marker exponent-sign exponent-digits)
                              (quit-parsing-token))
                             (t nil))))
        (cond
          ((and digits-before-break (not ratio-bar) (not digits-after-break)
                (not exponentp))
           #| Integer |#
           (* sign (parse-digit-list digits-before-break *read-base*)))
          ((and digits-before-break (not decimal-point) ratio-bar
                digits-after-break (not exponentp))
           #| Ratio |#
           (* sign (/ (parse-digit-list digits-before-break *read-base*)
                      (parse-digit-list digits-after-break  *read-base*))))
          ((or (and decimal-point (not ratio-bar) digits-after-break)
               (and digits-before-break
                    (not ratio-bar)
                    (or (not digits-after-break) decimal-point)
                    exponentp))
           #| Floating point |#
           (let* ((before-decimal (parse-digit-list digits-before-break 10.))
                  (after-decimal  (parse-digit-list digits-after-break  10.))
                  (exponent       (* (if (string= exponent-sign "-") -1 1)
                                     (parse-digit-list exponent-digits 10.)))
                  (the-bignum     (+ (* before-decimal
                                        (expt 10. (length digits-after-break)))
                                     after-decimal))
                  (type           (cond
                                    ((find exponent-marker "Ss") 'short-float)
                                    ((find exponent-marker "Ff") 'single-float)
                                    ((find exponent-marker "Dd") 'double-float)
                                    ((find exponent-marker "Ll") 'long-float)
                                    (t *read-default-float-format*)))
                  (the-flonum     (coerce the-bignum type))
                  (scale          (- exponent (length digits-after-break))))
             (* the-flonum (expt 10. scale))
             )))))))

(defun quit-parsing-token ()
  (throw 'quit NIL))

(defun get-characters (token attribute minimum maximum)
  (let* ((attributes (token-attributes token))
         (start      (token-scratch-pointer token))
         (end        (position-if-not
                       #'(lambda (attrib) (eq attrib attribute))
                       attributes
                       :start start)))
    (unless end (setq end (length attributes)))
    (setf (token-scratch-pointer token) end)
    (let* ((characters (subseq (coerce (token-characters token) 'list) start end))
           (length     (length characters)))
      (cond ((< length minimum)
             (throw 'quit NIL))
            ((and maximum (> length maximum))
             (throw 'quit NIL))
            (t
             characters)))))

(defun no-more-characters (token)
  (unless (= (length (token-attributes token))
             (token-scratch-pointer token))
    (throw 'quit NIL)))


(defun parse-symbol-string (string)
  (let ((number-of-colons (count #\: string))
        (length           (length string))
        (package-name     NIL)
        (symbol-name      NIL))
    (cond ((= number-of-colons 0)
           (setq symbol-name string))
          ((<= 1 number-of-colons 2)
           (let ((left-colon  (position #\: string))
                 (right-colon (position #\: string :from-end T)))
             (cond ((or (= right-colon (1- length))
                        (> (- right-colon left-colon) 1)
                        (and (= left-colon 0) (= right-colon 1)))
                    (parse-symbol-error string))
                   ((= right-colon 0)
                    (setq package-name "KEYWORD")
                    (setq symbol-name (subseq string 1)))
                   (t
                    (setq package-name (subseq string 0 left-colon))
                    (setq symbol-name  (subseq string (1+ right-colon)))))))
          (t
           (parse-symbol-error string)))
    (let ((pkg (if package-name (find-package package-name) *package*)))
      (if pkg
          (multiple-value-bind (symbol how-interned)
              (intern symbol-name pkg)
            (if (and (= number-of-colons 1) (not (eq how-interned :EXTERNAL)))
                (error "Reference to non-external symbol ~S in package ~A ~
                        using single colon prefix."
                       symbol package-name)
                symbol))
          (error "No package named ~A exists." package-name)))))

(defun parse-symbol-error (string)
  (error "The pattern of package markers in ~A is meaningless." string))

(defun parse-integer (string &key (start 0) end (radix 10) junk-allowed)
  (setq end (length string))
  (let ((parse-begun-p NIL)
        (parse-done-p  NIL)
        (sign          1)
        (value         NIL)
        (final-index   NIL))
    (do ((index start (1+ index)))
        ((>= index end)
         (setq final-index end))
      (let* ((char        (char string index))
             (whitespacep (eq (syntax-type char) :WHITESPACE))
             (digitp      (digit-char-p char radix))
             (signp       (and (find char "+-") (not parse-begun-p))))
        (cond (whitespacep
               (when parse-begun-p (setq parse-done-p T)))
              (signp
               (setq parse-begun-p T)
               (when (char= char #\-) (setq sign -1)))
              (digitp
               (cond ((not parse-done-p)
                      (setq parse-begun-p T)
                      (unless value (setq value 0))
                      (setq value (+ (* value 10) digitp)))
                     (junk-allowed
                      (setq final-index index)
                      (return))
                     (t (error "Junk found instead of integer."))))
              (t
               (cond (junk-allowed
                      (setq final-index index)
                      (return))
                     (t (error "Junk found instead of integer.")))))))
    (values (if value (* sign value) NIL)
            final-index)))



;;;----------------------------------------------------------------------------
;;; {5} READER MACROS
;;;----------------------------------------------------------------------------

(defun dispatch-char-reader (stream char)
  (let* ((dispatch-table   (readtable-entry-dispatch-table
                             (get-readtable-entry char *readtable*)))
         (digits           (make-array 0 :adjustable T :fill-pointer T))
         (sub-char         (loop (let ((char (read-char stream t nil t)))
                                   (if (char<= #\0 char #\9)
                                       (vector-push-extend char digits)
                                       (return (char-upcase char))))))
         (function         (aref dispatch-table (char-code sub-char)))
         (numeric-argument (if (= (length digits) 0)
                               NIL
                               (parse-integer (coerce digits 'string)))))
    (if function
        (funcall function stream sub-char numeric-argument)
        (error "Undefined dispatch macro ~C~C encountered by READ."
               char sub-char))))

(defun quote-reader (stream char)
  (declare (ignore char))
  (list 'quote (read stream t nil t)))

(defun double-quote-reader (stream char)
  (declare (ignore char))
  (let ((characters (make-array 0 :adjustable T :fill-pointer T)))
    (loop (let ((char (read-char stream t nil t)))
            (cond ((char= char #\")
                   (return (coerce characters 'string)))
                  ((eq (syntax-type char) :SINGLE-ESCAPE)
                   (vector-push-extend (read-char stream t nil t) characters))
                  (t
                   (vector-push-extend char characters)))))))

(defun open-paren-reader (stream char)
  (declare (ignore char))
  (let ((*allow-dot-tokens* T))
    (let ((list (read-delimited-list #\) stream t)))
      (let ((first-dot-in-list (position *reader-dot-token* list))
            (last-dot-in-list  (position *reader-dot-token* list :from-end T)))
        (cond
          ((null last-dot-in-list)
           list)
          ((not (eql first-dot-in-list last-dot-in-list))
           (error "Bad dotted list syntax; more than one dot in list."))
          ((= last-dot-in-list 0)
           (error "Bad dotted list syntax; dot begins list."))
          ((not (= last-dot-in-list (- (length list) 2)))
           (error "Bad dotted list syntax; dot not in penultimate position."))
          (t
           (let* ((last-cons    (nthcdr (- (length list) 3) list))
                  (last-element (caddr last-cons)))
             (rplacd last-cons last-element)
             list)))))))


(defun close-paren-reader (stream char)
  (declare (ignore stream char))
  (error "Unexpected close-parenthesis in READ."))

(defun semicolon-reader (stream char)
  (declare (ignore char))
  (do ()
      ((char= (read-char stream nil #\Newline t) #\Newline)))
  (values))

(defun comma-reader (stream char)
  (declare (ignore char))
  (let ((next-char (read-char stream t nil t)))
    (cond ((char= next-char #\@)
           (list 'unquote-splicing (read stream t nil t)))
          ((char= next-char #\.)
           (list 'destructive-unquote-splicing (read stream t nil t)))
          (t
           (unread-char next-char stream)
           (list 'unquote (read stream t nil t))))))

(defun backquote-reader (stream char)
  (declare (ignore char))
  (list 'backquote (read stream t nil t)))



;;;----------------------------------------------------------------------------
;;; {6} BACKQUOTE
;;;----------------------------------------------------------------------------

(defmacro backquote (expression &environment env)
  (cond ((vectorp expression)
         `(VECTOR ,@(nbutlast
                      (expand-backquoted-list (coerce expression 'list)))))
        ((not (listp expression))
         `(QUOTE ,expression))
        ((eq (car expression) 'UNQUOTE)
         `,(cadr expression))
        ((or (eq (car expression) 'UNQUOTE-SPLICING)
             (eq (car expression) 'DESTRUCTIVE-UNQUOTE-SPLICING))
         (error "Can't unquote-splicing immediately after backquote."))
        ((eq (car expression) 'BACKQUOTE)
         `(BACKQUOTE ,(macroexpand expression env)))
        (t
         `(LIST* ,@(expand-backquoted-list expression)))))

(defun expand-backquoted-list (expr)
  "EXPR is a (possibly dotted) list.  Return a list suitable for LIST* to be applied to."
  (cond
    ((not (consp expr))
     (list `(QUOTE ,expr)))
    ((eq (car expr) 'UNQUOTE)
     (cadr expr))
    ((or (eq (car expr) 'UNQUOTE-SPLICING)
         (eq (car expr) 'DESTRUCTIVE-UNQUOTE-SPLICING))
     (error "foo"))
    (t
     (let ((element (car expr)))
       (cond ((not (listp element))
              (cons `(BACKQUOTE ,element)
                    (expand-backquoted-list (cdr expr))))
             ((eq (car element) 'UNQUOTE)
              (cons (cadr element)
                    (expand-backquoted-list (cdr expr))))
             ((or (eq (car element) 'UNQUOTE-SPLICING)
                  (eq (car element) 'DESTRUCTIVE-UNQUOTE-SPLICING))
              (if (listp (cadr element))
                  (append (cadr element)
                          (expand-backquoted-list (cdr expr)))
                  (error "bar")))
             (t
              (cons `(BACKQUOTE ,element)
                    (expand-backquoted-list (cdr expr)))))))))

(defun unquote (expression)
  (declare (ignore expression))
  (error "Comma not within backquote."))

(defun unquote-splicing (expression)
  (declare (ignore expression))
  (Error "Can't unquote splicing"))

(defun destructive-unquote-splicing (expression)
  (declare (ignore expression))
  (error "Can't unquote splicing"))



;;;----------------------------------------------------------------------------
;;; {7} # DISPATCH MACROS
;;;----------------------------------------------------------------------------

(defun signal-sharp-error (stream char argument)
  (declare (ignore stream argument))
  (error "Illegal syntax #~C encountered by READ." char))

(defun read-#\\ (stream char argument)
  (declare (ignore char))
  (let ((token (start-token)))
    (augment-token token (read-char stream t nil t) :ALPHABETIC)
    (accumulate-token token stream t nil 'identity)
    (parse-character-name (token-name token) argument)))

(defun parse-character-name (character-name &optional (font-number 0))
  (when (null font-number) (setq font-number 0))
  (let ((bits 0))
    (labels
      ((extract-char (name)
         (cond ((= (length name) 1)
                (aref name 0))
               ((find #\- name)
                (let* ((hyphen-pos   (position #\- name))
                       (bucky-name   (string-upcase
                                       (subseq name 0 hyphen-pos)))
                       (rest-of-name (subseq name (1+ hyphen-pos))))
                  (cond ((or (string= bucky-name "CONTROL")
                             (string= bucky-name "C"))
                         (setq bits (logior bits char-control-bit)))
                        ((or (string= bucky-name "META")
                             (string= bucky-name "M"))
                         (setq bits (logior bits char-meta-bit)))
                        ((or (string= bucky-name "SUPER")
                             (string= bucky-name "S"))
                         (setq bits (logior bits char-super-bit)))
                        ((or (string= bucky-name "HYPER")
                             (string= bucky-name "H"))
                         (setq bits (logior bits char-hyper-bit)))
                        (t
                         (error "~S is not a legal bucky-bit name."
                                bucky-name)))
                  (extract-char rest-of-name)))
               (t
                (let ((char (name-char name)))
                  (if char
                      char
                      (error "~A is not a defined character name."
                             character-name)))))))
      (let ((char (extract-char character-name)))
        (make-char char bits font-number)))))

(defun |READ-#'| (stream char argument)
  (error-if-argument char argument)
  (list 'function (read stream t nil t)))

(defun |READ-#(| (stream char argument)
  (declare (ignore char))
  (let* ((elements (read-delimited-list #\) stream t))
         (length   (length elements)))
    (cond ((not argument)
           (apply #'vector elements))
          ((> length argument)
           (error "Too many objects in vector; only ~S allowed." argument))
          ((< length argument)
           (if (zerop length)
               (error "At least one element is required in vector.")
               (apply #'vector
                      (append elements (make-list (- argument length)
                                                  :initial-element
                                                  (car (last elements)))))))
          (t
           (apply #'vector elements)))))

(defun read-#* (stream char argument)
  (declare (ignore char))
  (let ((bits NIL))
    (loop
      (let ((char (read-char stream nil nil t)))
        (if (char<= #\0 char #\1)
            (push char bits)
            (progn (unread-char char stream)
                   (return)))))
    (setq bits (nreverse bits))
    (let ((length (length bits)))
      (cond ((not argument)
             (coerce bits 'bit-vector))
            ((> length argument)
             (error "Too many bits in bit-vector; only ~S allowed." argument))
            ((< length argument)
             (if (zerop length)
                 (error "At least one bit is required in bit-vector.")
                 (coerce (append bits (make-list (- argument length)
                                                 :initial-element
                                                 (car (last bits))))
                         'bit-vector)))
            (t
             (coerce bits 'bit-vector))))))

(defun read-|#:| (stream char argument)
  (error-if-argument char argument)
  (let ((token (start-token)))
    (accumulate-token token stream t nil 'identity)
    (identify-alphadigits token)
    (if (or (potential-number-p token)
            (find :PACKAGE-MARKER (token-attributes token)))
        (error "~A cannot name an uninterned symbol." (token-name token))
        (make-symbol (token-name token)))))

(defun read-#b (stream char argument)
  (error-if-argument char argument)
  (let ((*read-base* 2.))
    (read stream t nil t)))

(defun read-#o (stream char argument)
  (error-if-argument char argument)
  (let ((*read-base* 8.))
    (read stream t nil t)))

(defun read-#x (stream char argument)
  (error-if-argument char argument)
  (let ((*read-base* 16.))
    (read stream t nil t)))

(defun read-#r (stream char argument)
  (error-unless-argument char argument)
  (if (<= 2. argument 36.)
      (let ((*read-base* argument))
        (let ((foo
                (read stream t nil t))
              )
          foo))
      (let ((*print-base* 10.))
        (error "Illegal radix ~S. in #r." argument))))

(defun read-#a (stream char argument)
  (error-unless-argument char argument)
  (labels ((dimensions (contents dim)
            (if (zerop dim)
                NIL
                (cons (length contents)
                      (dimensions (elt contents 0) (1- dim))))))
    (let* ((contents-arg (read stream t nil t))
           (dimensions (dimensions contents-arg argument)))
      (make-array dimensions :initial-contents contents-arg))))

(defun read-#s (stream char argument)
  (error-if-argument char argument)
  (let ((args (read stream t nil t)))
    (declare (ignore args))
    (error "I don't know how to determine a standard constructor macro yet.")))


(defun read-#= (stream char argument)
  (error-unless-argument char argument)
  (cond (*read-suppress*
         (values))
        ((assoc argument *reader-circularities*)
         (error "More than one object labeled by #~S=." argument))
        (t
         (push (cons argument (make-placeholder-for-#n# argument))
               *reader-circularities*)
         (let ((thing (read stream t nil t)))
           (cond
             ((and (placeholder-for-#n#-p thing)
                   (= (placeholder-for-#n#-n thing) argument))
              (error "#~S= #~:*~S# is ambiguous reader syntax." argument))
             (t
              (rplacd (assoc argument *reader-circularities*) thing)
              thing))))))

(defun read-## (stream char argument)
  (declare (ignore stream))
  (error-unless-argument char argument)
  (if *read-suppress*
      NIL
      (let ((pair (assoc argument *reader-circularities*)))
        (if pair
            (cdr pair)
            (error "No object labeled by #~S= before #~:*~S#" argument)))))

(defun read-#+/#- (stream char argument)
  (error-if-argument char argument)
  (let ((feature (read stream t nil t)))
    (when (char= char #\-)
      (setq feature (list 'NOT feature)))
    (labels ((features-true-p (feature-expr)
               (cond ((symbolp feature-expr)
                      (member feature-expr *features*))
                     ((not (listp feature-expr))
                      (feature-error feature))
                     ((eq (car feature-expr) 'AND)
                      (every #'features-true-p (cdr feature-expr)))
                     ((eq (car feature-expr) 'OR)
                      (some #'features-true-p (cdr feature-expr)))
                     ((eq (car feature-expr) 'NOT)
                      (if (= (length feature-expr) 2)
                          (not (features-true-p (cadr feature-expr)))
                          (feature-error feature)))
                     (t (feature-error feature)))))
      (if (features-true-p feature)
          (read stream t nil t)
          (let ((*read-suppress* T))
            (read stream t nil t)
            (values))))))

(defun read-#\| (stream char argument)
  (error-if-argument char argument)
  (let ((depth 1))
    (loop
      (when (= depth 0) (return))
      (let ((char-1 (read-char stream nil #\x t))
            (char-2 (peek-char nil stream nil #\x t)))
        (cond ((and (char= char-1 #\#) (char= char-2 #\|))
               (read-char stream t nil t)
               (incf depth))
              ((and (char= char-1 #\|) (char= char-2 #\#))
               (read-char stream t nil t)
               (decf depth))))))
  (values))


(defun feature-error (feature)
  (error "~S is not a valid feature expression." feature))

(defun error-if-argument (char argument)
  (when (and argument (not *read-suppress*))
    (error "An argument, ~S, was given to #~C." argument char)))

(defun error-unless-argument (char argument)
  (unless (or argument *read-suppress*)
    (error "An argument is required by #~C." char)))







;;  Hi. This is Jim Rauen.  I'm too busy working right now to pick up the phone,...
