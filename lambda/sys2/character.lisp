;-*- Mode:LISP; Package:SI; Cold-Load:T; Readtable:CL; Base:10; Lowercase:T -*-

;;; Character functions and variables.

; character lossage of the most complete kind

(defconstant char-code-limit #o400
  "Character code values must be less than this.")

(defconstant char-font-limit #o400
  "Font codes in characters must be less than this.")

(defconstant char-bits-limit #o20
  "All the special bits in a character must be less than this.
They are Control, Meta, Super and Hyper.")

(defconstant char-control-bit 1
  "This bit within the bits of a character is the Control bit.")

(defconstant char-meta-bit 2
  "This bit, within the bits of a character, is the Meta bit.")

(defconstant char-super-bit 4
  "This bit, within the bits of a character, is the Super bit.")

(defconstant char-hyper-bit 8.
  "This bit, within the bits of a character, is the Hyper bit.")

(defsubst char-code (char)
  "Returns the character code of the character CHAR.
This is sans the font number and meta bits."
  (ldb %%ch-char char))

(defsubst char-font (char)
  "Returns the font number of character CHAR."
  (ldb %%ch-font char))

(defsubst char-bits (char)
  "Returns the special bits of the character CHAR."
  (%logldb %%kbd-control-meta char))

;These are now microcoded
;(defsubst alpha-char-p (char)
;  "T if CHAR is alphabetic with no meta bits."
;  (and (zerop (ldb %%kbd-control-meta char))
;       (or ( #/A (ldb %%ch-char char) #/Z)
;          ( #/a (ldb %%ch-char char) #/z))))

;(defsubst upper-case-p (char)
;  "T if CHAR is an upper case letter with no meta bits."
;  (and (zerop (ldb %%kbd-control-meta char))
;       ( #/A (ldb %%ch-char char) #/Z)))

;(defsubst lower-case-p (char)
;  "T if CHAR is an upper case letter with no meta bits."
;  (and (zerop (ldb %%kbd-control-meta char))
;       ( #/a (ldb %%ch-char char) #/z)))

;(defsubst both-case-p (char)
;  "T if CHAR is a character which has upper and lower case forms, with no meta bits.
;This is just letters."
;  (and (zerop (ldb %%kbd-control-meta char))
;       (or ( #/A (ldb %%ch-char char) #/Z)
;          ( #/a (ldb %%ch-char char) #/z))))

;(defsubst alphanumericp (char)
;  "T if CHAR is a letter or digit, with no meta bits."
;  (and (zerop (ldb %%kbd-control-meta char))
;       (or ( #/0 (ldb %%ch-char char) #/9)
;          ( #/A (ldb %%ch-char char) #/Z)
;          ( #/a (ldb %%ch-char char) #/z))))

(defsubst char< (&rest chars)
  "T if all the characters are monotonically increasing, considering bits, font and case."
  (apply #'< chars))

(defsubst char> (&rest chars)
  "T if all the characters are monotonically decreasing, considering bits, font and case."
  (apply #'> chars))

(defsubst char<= (&rest chars)
  "T if all the characters are monotonically nondecreasing, considering bits, font and case."
  (apply #' chars))

(defsubst char>= (&rest chars)
  "T if all the characters are monotonically nonincreasing, considering bits, font and case."
  (apply #' chars))

(defsubst char (&rest chars)
  "T if all the characters are monotonically nondecreasing, considering bits, font and case."
  (apply #' chars))

(defsubst char (&rest chars)
  "T if all the characters are monotonically nonincreasing, considering bits, font and case."
  (apply #' chars))

(defsubst char/= (&rest chars)
  "T if all the characters are distinct (no two equal), considering bits, font and case."
  (apply #' chars))

(defsubst char= (&rest chars)
  "T if all the characters are equal, considering bits, font and case."
  (apply #'= chars))

(defsubst char (&rest chars)
  "T if all the characters are distinct (no two equal), considering bits, font and case."
  (apply #' chars))

(defun standard-char-p (char)
  "T if CHAR is one of the ASCII printing characters or the Newline character."
  (or (char= char #\Newline)
      ( (char-int #\space) (char-int char) #o176)))

(defsubst graphic-char-p (char)
  "T if CHAR is a graphic character, one which prints as a single glyph.
Things like #\NEWLINE and #\RESUME and #\CONTROL-A are not graphic."
  ( 0 (char-int char) #o177))

(defsubst string-char-p (char)
  "T if CHAR is a character which ordinary strings can contain.
Note that ART-FAT-STRING arrays can contain additional characters,
for which this function nevertheless returns NIL."
  ( 0 (char-int char) #o377))

;>> flush
(defsubst fat-string-char-p (char)
  "T if CHAR is a charater which a fat string can contain."
  ( 0 (char-int char) #o177777))

(defun digit-char-p (char &optional (radix 10.))
  "Weight of CHAR as a digit, if it is a digit in radix RADIX; else NIL.
The weights of #\0 through #\9 are 0 through 9;
the weights of letters start at ten for A.
RADIX does not affect the weight of any digit,
but it affects whether NIL is returned."
  (and (zerop (char-bits char))
       (let ((basic (char-code char)))
         (and (if ( radix 10.)
                  ( (char-int #\0) basic (+ (char-int #\0) radix -1))
                  (or ( (char-int #\0) basic (char-int #\9))
                      ( (char-int #\A)
                         (setq basic (char-code (char-upcase char)))
                         (+ (char-int #\A) radix -11.))))
              (if ( basic (char-int #\9))
                  (- basic (char-int #\0))
                  (+ 10. (- basic (char-int #\A))))))))

;;; This is symbol*cs braindeath. Darn if I know what it's for.
;;; It's apparently something to do with their way of making
;;; standard characters.  It is not a common lisp thing.
(defun char-standard (char)
  (declare (ignore char))
  t)


(defun char-not-equal (&rest chars)
  "T if all the characters are distinct, ignoring bits, font and case."
  (do ((tail chars (cdr tail)))
      ((null (cdr tail)) t)
    (let ((char1 (car tail)))
      (dolist (char2 (cdr tail))
        (if (char-equal char1 char2)
            (return-from char-not-equal nil))))))

;; compiled code usually calls the char-equal microinstruction
(defun char-equal (&rest chars)
  "T if all the characters are equal, ignoring bits, font and case."
  (do ((tail chars (cdr tail)))
      ((null (cdr tail)) t)
    (unless (char-equal (car tail) (cadr tail))
      (return nil))))

(defun char-lessp (&rest chars)
  "T if all the characters are monotonically increasing, ignoring bits, font and case."
  (do ((tail chars (cdr tail)))
      ((null (cdr tail)) t)
    (let ((ch1 (char-code (car tail)))
          (ch2 (char-code (cadr tail))))
      (setq ch1 (char-upcase ch1))
      (setq ch2 (char-upcase ch2))
      (unless (< ch1 ch2) (return nil)))))

(defun char-greaterp (&rest chars)
  "T if all the characters are monotonically decreasing, ignoring bits, font and case."
  (do ((tail chars (cdr tail)))
      ((null (cdr tail)) t)
    (let ((ch1 (char-code (car tail)))
          (ch2 (char-code (cadr tail))))
      (setq ch1 (char-upcase ch1))
      (setq ch2 (char-upcase ch2))
      (unless (> ch1 ch2) (return nil)))))

(defun char-not-lessp (&rest chars)
  "T if all the characters are monotonically nonincreasing, ignoring bits, font and case."
  (do ((tail chars (cdr tail)))
      ((null (cdr tail)) t)
    (let ((ch1 (char-code (car tail)))
          (ch2 (char-code (cadr tail))))
      (setq ch1 (char-upcase ch1))
      (setq ch2 (char-upcase ch2))
      (unless ( ch1 ch2) (return nil)))))

(defun char-not-greaterp (&rest chars)
  "T if all the characters are monotonically nondecreasing, ignoring bits, font and case."
  (do ((tail chars (cdr tail)))
      ((null (cdr tail)) t)
    (let ((ch1 (char-code (car tail)))
          (ch2 (char-code (cadr tail))))
      (setq ch1 (char-upcase ch1))
      (setq ch2 (char-upcase ch2))
      (unless ( ch1 ch2) (return nil)))))

;; now microcoded
;(defun char-upcase (char &aux subchar)
;  "Return the uppercase version of CHAR.
;If CHAR does not have a uppercase version, it is returned unchanged."
;  (setq subchar (char-code char))
;  (if ( #/a subchar #/z)
;      (if (fixnump char)
;         (logxor #o40 char)
;         (int-char (logxor #o40 char)))
;    char))

;(defun char-downcase (char &aux subchar)
;  "Return the lowercase version of CHAR.
;If CHAR does not have a lowercase version, it is returned unchanged."
;  (setq subchar (ldb %%ch-char char))
;  (if ( #/A subchar #/Z)
;      (if (fixnump char)
;         (logxor #o40 char)
;         (int-char (logxor #o40 char)))
;    char))

(defun char-flipcase (char)
  "If CHAR is an uppercase character, return it's lowercase conterpart, and vice-versa.
Returns CHAR unchanged if CHAR is neither upper now lower case."
  (cond ((upper-case-p char) (char-downcase char))
        ((lower-case-p char) (char-upcase char))
        (t char)))

(defun code-char (code &optional (bits 0) (font 0))
  "Returns a character whose code comes from CODE, bits from BITS and font from FONT.
CODE can be a number or a character.
NIL is returned if it is not possible to have a character object
with the specified FONT and BITS."
  (if (and ( 0 bits (1- char-bits-limit))
           ( 0 font (1- char-font-limit)))
      (%make-pointer dtp-character
                     (%logdpb bits %%kbd-control-meta
                              (dpb font %%ch-font code)))
    nil))
(deff make-char 'code-char)

(defun digit-char (weight &optional (radix 10.) (font 0))
  "Return a character which signifies WEIGHT in radix RADIX, with FONT as specified.
This is always NIL if WEIGHT is  RADIX.
Otherwise, for WEIGHT between 0 and 9, you get characters 0 through 9;
for higher weights, you get letters."
  (if (not ( 0 weight (1- radix))) nil
    (if (not ( 0 font char-font-limit)) nil
      (%make-pointer dtp-character
                     (dpb font %%ch-font (if (< weight 10.)
                                             (+ (char-code #\0) weight)
                                             (+ (char-code #\A) weight -10.)))))))

;Now microcoded
;(defun char-int (char)
;  "Returns an integer whose value corresponds to CHAR.
;On the Lisp machine, this conversion will happen automatically
;in most places that an integer can be used."
;  (dont-optimize (%pointer char)))

(defun char-name (char)
  "Returns the standard name of CHAR, as a string; or NIL if there is none.
For example, \"RETURN\" for the character Return.
Only works for characters which are not GRAPHIC-CHAR-P (unlike \"a\", for example.)"
;character lossage
  (let ((elt (rassq (char-int char) xr-special-character-names)))
    (if elt (symbol-name (car elt)))))

(defun name-char (name)
  "Returns a character object which is the meaning of NAME as a character name,
or NIL if NAME has none."
  (let ((found (cdr (ass 'string-equal name xr-special-character-names))))
    (and found (int-char found))))

(defparameter *char-bit-alist*
              `((:control . ,%%kbd-control)
                (:meta . ,%%kbd-meta)
                (:super . ,%%kbd-super)
                (:hyper . ,%%kbd-hyper))
  "Alist of bit names for CHAR-BIT vs byte specifiers to extract those bits from a character.")

(defun char-bit (char bit-name)
  "T if the bit spec'd by BIT-NAME (a keyword) is on in CHAR.
BIT-NAME can be :CONTROL, :META, :SUPER or :HYPER."
  (let ((byte (cdr (assq bit-name *char-bit-alist*))))
    (if byte
        (%logldb-test byte char)
      (ferror "~S is not a valid character-bit specifier" bit-name))))

(defun set-char-bit (char bit-name new-value)
  "Returns a character like CHAR except that the bit BIT-NAME has value NEW-VALUE in it.
BIT-NAME can be :CONTROL, :META, :SUPER or :HYPER.
NEW-VALUE should be T or NIL."
  (let ((byte (cdr (assq bit-name *char-bit-alist*))))
    (if byte
        (let* ((new-char (%logdpb (if new-value 1 0) byte char)))
          (if (typep char 'character)
              (int-char new-char)
            new-char))
      (ferror "~S is not a valid character-bit specifier" bit-name))))
