;;;-*- Mode:LISP; Package:SI; Lowercase:T; Base:8; Cold-Load:T; Readtable:ZL -*-

;       ** (c) Copyright 1980 Massachusetts Institute of Technology **

; character lossage of the most severe and pervasive type

;;many of these should be move to microcode!!!! (hah!!)

;NOTES: THINK ABOUT 16-BIT STRINGS.

;The string functions:

#|
(STRING-LENGTH string) returns the number of characters in a given string.
(SUBSTRING string from to) returns an arbitrary substring of a given string, copied.
 Omitting <to> means go all the way to the end of the string.
(NSUBSTRING string from to) is like SUBSTRING, but returns a shared substring, not copied.
(STRING-APPEND string ...) concatenates strings.
(STRING-SUBST-CHAR new old string) replaces all occurences of a <old> with <new> in <string>
(STRING-SEARCH-CHAR char string from to) searches a string for a given character.
  Returns index if found, else NIL.
(STRING-REVERSE-SEARCH-CHAR char string from to) searches backwards, as above.
(STRING-SEARCH-NOT-CHAR char string from to) searches a string for anything other
   than a given char.  Returns index if found, else NIL.
(STRING-REVERSE-SEARCH-NOT-CHAR char string from to) searches backwards, as above.
(STRING-SEARCH key string from to) searches for <key> in <string>.
  Returns index if found, else NIL.
(STRING-REVERSE-SEARCH key string from to) searches backwards for <key> in <string>.
(STRING-SEARCH-SET charlist string from to) searches in <string> from <from>
 for a char in <charlist>.
(STRING-SEARCH-NOT-SET charlist string from to) searches in <string> from <from>
 for a char not in <charlist>.
(STRING-REVERSE-SEARCH-SET charlist string from to) searches backwards in <string>
 from <from> for a char in <charlist>.
(STRING-REVERSE-SEARCH-NOT-SET charlist string from to) searches backwards in <string>
 from <from> for a char not in <charlist>.
(STRING-TRIM charlist string) returns a copy of <string> with all leading and
 trailing members of <charlist> truncated.
(STRING-LEFT-TRIM charlist string) is like STRING-TRIM but only hacks leading characters.
(STRING-RIGHT-TRIM charlist string) is analogous.
(STRING-NREVERSE string) reverses the elements of <string>, in place.
(STRING-REVERSE string) returns a copy of <string> with the characters reversed.
(STRING-UPCASE string) returns string copied and converted to all upper case.
(STRING-DOWNCASE string) returns string copied and converted to all lower case.
(string-flipcase string)
(CHAR-UPCASE char) returns the character converted to upper case.
(CHAR-DOWNCASE char) returns the character converted to lower case.
(STRING-REMOVE-FONTS string) returns string without font info (chars truncated to 8 bits)
(STRING-COMPARE s1 s2 &optional (from1 0) (from2 0) to1 to2)
(STRING-LESSP s1 s2) says whether s1 is less than s2, in dictionary ordering.
(ARRAY-TYPE array) returns the type of an array, as a symbol (eg, ART-STRING).
(SUBSTRING-AFTER-CHAR char string) "" if char not in string.
(STRING-PLURALIZE string) returns plural of word in string.
(STRING-EQUAL string1 string2 &optional start1 start2 end1 end2)
 returns T if specified portions match.
(STRING something) returns the argument, converted to a string.
(CL:CHARACTER something) returns the argument, converted to a character.
(ZL:CHARACTER something) returns the argument, converted to a fixnum.
(ALPHALESSP something1 something2) is nearly the same as comparing the
 two objects' printed representations, as strings.
 However, numbers are compared with =.
(ALPHAEQUAL something1 something2) is nearly the same as comparing the
 two objects' printed representations, as strings.
 However, numbers are compared with =.

SUBSTRING and NSUBSTRING take an optional area argument.

Note that most of the functions in this package will consider a number
to be a string one character long.  However, they will never return
a number instead of a string one character long.
Symbols given as arguments will be converted into their pnames.
|#

(DEFSUBST FIXNUM-ARRAYP (OBJECT)
  "T if OBJECT is an array whose elements cannot be of arbitrary type."
  (AND (ARRAYP OBJECT)
       (ARRAY-BITS-PER-ELEMENT (%P-LDB %%ARRAY-TYPE-FIELD OBJECT))))     ;;**********************

;;; This macro is used by string-searching functions to coerce the string args.
(DEFMACRO COERCE-STRING-SEARCH-ARG (ARG-NAME)
  `(OR (FIXNUM-ARRAYP ,ARG-NAME)
       (SETQ ,ARG-NAME (STRING ,ARG-NAME))))

(DEFMACRO COERCE-STRING-ARG (ARG-NAME)
  "Convert ARG-NAME to a string if it isn't one already.
Sets the value of ARG-NAME."
  `(OR (STRINGP ,ARG-NAME)
       (SETQ ,ARG-NAME (STRING ,ARG-NAME))))

(defmacro require-character (variable)
  "Checks the type of VARIABLE to be either a character or an integer.
If the value is initially an integer, it is coerced into a character with
INT-CHAR."
  `(progn
     (when (typep ,variable 'zl:fixnum)
       (setq ,variable (int-char ,variable)))
     (check-type ,variable character)))

;This is now microcoded
;(DEFUN INT-CHAR (INTEGER)
;  "Returns a character whose value corresponds to INTEGER."
;  (%MAKE-POINTER DTP-CHARACTER INTEGER))

(defun array-int->array-char (int-array)
  "Mutates an array of integers into an array of characters."
  (dotimes (index (array-active-length int-array))
    (setf (aref int-array index)
          (int-char (aref int-array index)))))

(defun array-int->string (int-array)
  "Coerces an array of fixnums into a string.  This is a crock."
  (let ((ans (make-string (array-active-length int-array))))
    (dotimes (index (array-active-length int-array))
      (setf (aref ans index)
            (int-char (aref int-array index))))
    ans))

(DEFUN STRING-APPEND (&REST STRINGS
                      &AUX (LENGTH 0) (BITS 0) B TY (TYPE 'ART-STRING) FROB)
  "Append any number of strings (or vectors).  The value is always a newly constructed array.
The value will have be of an array type which can contain the elements of all the STRINGS.
Symbols, characters and numbers are coerced into strings."
  (DOLIST (S STRINGS)
    (IF (CHARACTERP S) (SETQ S (CHAR-INT S)))
    (TYPECASE S
      (FIXNUM
       (INCF LENGTH 1)
       (COND ((< S (^ 2 8)) (SETQ B 8 TY 'ART-STRING))
             ((< S (^ 2 16.)) (SETQ B 16. TY 'ART-FAT-STRING))
             (T (SETQ B %%Q-POINTER TY 'ART-Q))))                     ;;**********************
      (VECTOR
       (INCF LENGTH (LENGTH S))
       (SETQ B (ARRAY-ELEMENT-SIZE S) TY (ARRAY-TYPE S)))
      (SYMBOL
       (INCF LENGTH (LENGTH (SYMBOL-NAME S)))
       (SETQ B 8 TY ART-STRING))
      ((AND INSTANCE (SATISFIES (LAMBDA (STRING)
                                (SEND STRING :OPERATION-HANDLED-P :STRING-FOR-PRINTING))))
       (PUSH (SETQ S (SEND S :STRING-FOR-PRINTING)) FROB)
       (INCF LENGTH (LENGTH S))
       (SETQ B 8 TY 'ART-STRING))
      (T
       (FERROR "Cannot convert ~S into a string." S)))
    (WHEN (> B BITS)
      (SETQ BITS B TYPE TY)))
  (SETQ FROB (NREVERSE FROB))
  (LET ((STRING (MAKE-ARRAY LENGTH :TYPE TYPE))
        (I 0)
        COERCED)
    (DOLIST (S STRINGS)
      (TYPECASE S
        (CHARACTER
         (SETF (CHAR STRING I) S)
         (INCF I 1))
        (FIXNUM
         (SETF (CHAR STRING I) (INT-CHAR S))
         (INCF I 1))
        (T (SETQ COERCED (TYPECASE S
                           (VECTOR S)
                           (SYMBOL (SYMBOL-NAME S))
                           (T (POP FROB))))
           (COPY-ARRAY-PORTION COERCED 0 (SETQ LENGTH (LENGTH COERCED))
                               STRING I (INCF I LENGTH)))))
    STRING))

(DEFUN STRING-NCONC (MUNG &REST STRINGS &AUX LEN FINAL-LEN S2LEN)
  "STRING-NCONC extends the first string and tacks on any number of additional strings.
The first argument must be a string with a fill-pointer.
Returns the first argument, which may have been moved and forwarded,
just like ADJUST-ARRAY-SIZE."
  (SETQ FINAL-LEN (SETQ LEN (FILL-POINTER MUNG)))
  (DOLIST (STR2 STRINGS)
    (SETQ FINAL-LEN (+ FINAL-LEN (STRING-LENGTH STR2))))
  (AND (> FINAL-LEN (ARRAY-LENGTH MUNG))
       (ADJUST-ARRAY-SIZE MUNG FINAL-LEN))
  (DOLIST (STR2 STRINGS)
    (TYPECASE STR2
      (CHARACTER
       (VECTOR-PUSH STR2 MUNG)
       (INCF LEN 1))
      (FIXNUM
       (VECTOR-PUSH (INT-CHAR STR2) MUNG)
       (INCF LEN 1))
      (T (SETQ STR2 (IF (STRINGP STR2) STR2 (STRING STR2)) S2LEN (LENGTH STR2))
         (COPY-ARRAY-PORTION STR2 0 S2LEN MUNG LEN (INCF LEN S2LEN))
         (SETF (FILL-POINTER MUNG) LEN))))
  MUNG)

(DEFUN NSUBSTRING (STRING FROM &OPTIONAL TO (AREA NIL)
                   &AUX LENGTH ARRAYTYPE)
  "Return a displaced array whose data is part of STRING, from FROM to TO.
If you modify the contents of the displaced array, the original string changes.
If TO is omitted or NIL, the substring runs up to the end of the string.
If AREA is specified, the displaced array is made in that area."
  (COERCE-STRING-ARG STRING)
  (OR TO (SETQ TO (LENGTH STRING)))
  (ASSERT ( 0 FROM TO (LENGTH STRING))
          (FROM TO STRING)
          "Args ~S and ~S out of range for ~S."
          FROM TO STRING)
  (SETQ LENGTH (- TO FROM))
  (SETQ ARRAYTYPE (ARRAY-TYPE STRING))
  (COND ((NOT (ARRAY-INDEXED-P STRING))
         (MAKE-ARRAY LENGTH :TYPE ARRAYTYPE
                            :AREA AREA
                            :DISPLACED-TO STRING
                            :DISPLACED-INDEX-OFFSET FROM))
        ;; Otherwise, probably a substring of a substring
        (T
         (MAKE-ARRAY LENGTH  :TYPE ARRAYTYPE
                             :AREA AREA
                             :DISPLACED-TO (ARRAY-INDIRECT-TO STRING)
                             ;; Point to array pointed to originally
                             :DISPLACED-INDEX-OFFSET
                             (+ FROM (ARRAY-INDEX-OFFSET STRING))))))

(DEFUN SUBSTRING (STRING FROM &OPTIONAL TO (AREA NIL))
  "Return a copy of part of STRING, from FROM to TO.
If TO is omitted, the copied part is up to the end of the string.
If AREA is specified, the new string is made in that area."
  ;; Nice and modular but conses up the wazoo
  ;; (STRING-APPEND (NSUBSTRING STRING FROM TO))
  ;; What's wrong with consing up wazoos?  Do they take up lots of space?
  ;; No, but they make a lot of noise.
  (COERCE-STRING-ARG STRING)
  (OR TO (SETQ TO (LENGTH STRING)))
  (ASSERT ( 0 FROM TO (LENGTH STRING))
          (FROM TO STRING)
          "Args ~S and ~S out of range for ~S."
          FROM TO STRING)
  (LET ((RES (MAKE-ARRAY (- TO FROM) :TYPE (%P-LDB-OFFSET %%ARRAY-TYPE-FIELD STRING 0) ;;*********
                         :AREA AREA)))
    (COPY-ARRAY-PORTION STRING FROM TO
                        RES 0 (ARRAY-LENGTH RES))
    RES))

(DEFUN SUBSTRING-AFTER-CHAR (CHAR STRING &OPTIONAL START END AREA)
  "Return the part of STRING that follows the first occurrence of CHAR after START.
Only the part of STRING up to END is searched, and the substring stops there too.
The value is a newly created string, in area AREA (or the default area)."
  (OR START (SETQ START 0))
  (OR END (SETQ END (STRING-LENGTH STRING)))
  (LET ((IDX (STRING-SEARCH-CHAR CHAR STRING START END)))
    (IF (NULL IDX) ""
      (SUBSTRING STRING (1+ IDX) END AREA))))

(DEFUN STRING-LENGTH (STRING)
  "Return the length of STRING, in characters."
  (TYPECASE STRING
    (STRING
     (LENGTH STRING))
    ((OR INTEGER CHARACTER)
     1)
    (SYMBOL
     (LENGTH (SYMBOL-NAME STRING)))
    ((AND INSTANCE (SATISFIES (LAMBDA (STRING)
                                (SEND STRING :OPERATION-HANDLED-P :STRING-FOR-PRINTING))))
     (STRING-LENGTH (SEND STRING :STRING-FOR-PRINTING)))
    (T
     (FERROR "Cannot convert ~S into a string." STRING))))

(DEFUN STRING-EQUAL (STRING1 STRING2 &REST ARGS) ;CL compatible
  "T if STRING1 and STRING2's contents are the same.
Case is ignored in comparing characters.
The keyword arguments allow you to compare only part of a string.
The range of STRING1 to be compared runs from START1 to END1
and the range of STRING2 runs from START2 to END2.
If END1 or END2 omitted or NIL, the end of that string is used."
  (DECLARE (ARGLIST STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2))
  (LET (IDX1 IDX2 LIM1 LIM2
        (ALPHABETIC-CASE-AFFECTS-STRING-COMPARISON NIL))
    (IF (KEYWORDP (CAR ARGS))
        (SETQ IDX1 (GETF ARGS ':START1)
              IDX2 (GETF ARGS ':START2)
              LIM1 (GETF ARGS ':END1)
              LIM2 (GETF ARGS ':END2))
      (LIST-MATCH-P ARGS `(,IDX1 ,IDX2 ,LIM1 ,LIM2)))
    (OR IDX1 (SETQ IDX1 0))
    (OR IDX2 (SETQ IDX2 0))
    (COERCE-STRING-ARG STRING1)
    (COERCE-STRING-ARG STRING2)
    (COND ((OR LIM1 LIM2)
           (OR LIM1 (SETQ LIM1 (LENGTH STRING1)))
           (OR LIM2 (SETQ LIM2 (LENGTH STRING2)))
           (AND (= (SETQ LIM1 (- LIM1 IDX1)) (- LIM2 IDX2))
                (%STRING-EQUAL STRING1 IDX1 STRING2 IDX2 LIM1)))     ;;**********************
          (T (%STRING-EQUAL STRING1 IDX1 STRING2 IDX2 NIL)))))       ;;**********************

(DEFUN STRING= (STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2) ;CL compatible
  "T if STRING1 and STRING2's contents are the same, case being significant.
The keyword arguments allow you to compare only part of a string.
The range of STRING1 to be compared runs from START1 to END1
and the range of STRING2 runs from START2 to END2.
If END1 or END2 omitted or NIL, the end of that string is used."
  (LET ((ALPHABETIC-CASE-AFFECTS-STRING-COMPARISON T))
    (COERCE-STRING-ARG STRING1)
    (COERCE-STRING-ARG STRING2)
    (COND ((OR END1 END2)
           (OR END1 (SETQ END1 (LENGTH STRING1)))
           (OR END2 (SETQ END2 (LENGTH STRING2)))
           (AND (= (SETQ END1 (- END1 START1)) (- END2 START2))
                (%STRING-EQUAL STRING1 START1 STRING2 START2 END1)))   ;;**********************
          (T (%STRING-EQUAL STRING1 START1 STRING2 START2 NIL)))))     ;;**********************

(DEFUN STRING-NOT-EQUAL (STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2) ;CL compatible
  "True if STRING1 (or substring) is different from STRING2 (or substring).
The value is actually the index of the first difference between the strings.
Keyword arg :START1 is where to start comparing, in STRING1, and :END1 where to stop.
Similar for :START2 and :END2.  Case is ignored when comparing letters."
  (LET ((V (STRING-COMPARE STRING1 STRING2 START1 START2 END1 END2)))
    (UNLESS (ZEROP V) (1- (ABS V)))))

(defun string-matchp (string1 string2)
  "Like STRING-EQUAL but STRING1 can have wildchar indicators:
`%' = match one character, `*' = match any number of characters from STRING2"
  ;; 17-Mar-86 17:16:08 -gjc
  (IF (or (string-search #\* string1)
          (string-search #\% string1))
      (string-matchp-1 string1 0 (string-length string1)
                       string2 0 (string-length string2))
    (string-equal string1 string2)))

(defun string-matchp-1 (string1 i1 n1 string2 i2 n2)
  (prog (temp)
        loop
        (if (and (= i1 n1) (= i2 n2)) (return t))
        (if (= i1 n1) (return nil))
        (if (= i2 n2) (go check-star))
        (when (or (char-equal (setq temp (aref string1 i1)) #\%)
                  (char-equal temp (aref string2 i2)))
          (setq i1 (1+ i1) i2 (1+ i2))
          (go loop))
        check-star
        (if (char-equal (aref string1 i1) #\*)
            (cond ((= (1+ i1) n1) (return t))
                  ((= i2 n2) (return nil))
                  ((string-matchp-1 string1 (1+ i1) n1 string2 (1+ i2) n2)
                   (return t))
                  ((string-matchp-1 string1 i1 n1 string2 (1+ i2) n2)
                   (return t))
                  ('else
                   (setq i1 (1+ i1))
                   (go loop)))
            (return nil))))


(DEFSUBST MAKE-STRING (LENGTH &REST KEYWORD-ARGS)
  "Creates and returns a string of LENGTH elements, all set to INITIAL-ELEMENT.
If INITIAL-VALUE is not supplied, the elements contain the character with code 0."
  (DECLARE (ARGLIST LENGTH &KEY INITIAL-ELEMENT &ALLOW-OTHER-KEYS))
  (APPLY #'MAKE-ARRAY LENGTH :TYPE ART-STRING KEYWORD-ARGS))

(DEFUN ARRAY-TYPE (ARRAY)
  "Return the name of the array-type of ARRAY.
The value is a symbol such as ART-Q."
  (CHECK-TYPE ARRAY ARRAY)
  (NTH (%P-LDB-OFFSET %%ARRAY-TYPE-FIELD ARRAY 0) ARRAY-TYPES))   ;;**********************

(DEFUN STRING-SEARCH-CHAR (CHAR STRING &OPTIONAL (FROM 0) TO (CONSIDER-CASE alphabetic-case-affects-string-comparison)
                           &AUX (ALPHABETIC-CASE-AFFECTS-STRING-COMPARISON CONSIDER-CASE))
  "Returns the index in STRING of the first occurrence of CHAR past FROM, or NIL if none.
If TO is non-NIL, the search stops there, and the value is NIL
if CHAR is not found before there.
Case matters during character comparison if CONSIDER-CASE is non-NIL."
  (CHECK-TYPE CHAR (OR CHARACTER FIXNUM) "a character")
  (COERCE-STRING-SEARCH-ARG STRING)
  (OR TO (SETQ TO (LENGTH STRING)))
  (%STRING-SEARCH-CHAR CHAR STRING FROM TO))                     ;;**********************

(DEFUN STRING-REVERSE-SEARCH-CHAR (CHAR STRING &OPTIONAL FROM (TO 0) CONSIDER-CASE)
  "Returns the index in STRING of the last occurrence of CHAR before FROM, or NIL if none.
If TO is non-zero, the search stops there, and the value is NIL
if CHAR does not appear after there.  TO should normally be less than FROM.
If FROM is omitted or NIL, the default is the end of the string.
Case matters during character comparison if CONSIDER-CASE is non-NIL."
  (IF (FIXNUMP CHAR) (SETQ CHAR (INT-CHAR CHAR)))
  (CHECK-TYPE CHAR CHARACTER)
  (COERCE-STRING-SEARCH-ARG STRING)
  (OR FROM (SETQ FROM (LENGTH STRING)))
  (IF CONSIDER-CASE
      (DO ((I (1- FROM) (1- I)))
          ((< I TO) NIL)
        (AND (CHAR= CHAR (CHAR STRING I))
             (RETURN I)))
      (DO ((I (1- FROM) (1- I)))
          ((< I TO) NIL)
        (AND (CHAR-EQUAL CHAR (CHAR STRING I))
             (RETURN I)))))

(DEFUN STRING-SEARCH-NOT-CHAR (CHAR STRING &OPTIONAL (FROM 0) TO CONSIDER-CASE)
  "Returns the index in STRING of the first character past FROM not equal to CHAR, or NIL.
If TO is non-NIL, the search stops there, and the value is NIL
if a character different from CHAR is not found before there.
Case matters during character comparison if CONSIDER-CASE is non-NIL."
  (IF (FIXNUMP CHAR) (SETQ CHAR (INT-CHAR CHAR)))
  (CHECK-TYPE CHAR CHARACTER)
  (COERCE-STRING-SEARCH-ARG STRING)
  (OR TO (SETQ TO (LENGTH STRING)))
  (IF CONSIDER-CASE
      (DO ((I FROM (1+ I)))
          (( I TO) NIL)
        (OR (CHAR= CHAR (CHAR STRING I))
            (RETURN I)))
      (DO ((I FROM (1+ I)))
          (( I TO) NIL)
        (OR (CHAR-EQUAL CHAR (CHAR STRING I))
            (RETURN I)))))

(DEFUN STRING-REVERSE-SEARCH-NOT-CHAR (CHAR STRING &OPTIONAL FROM (TO 0) CONSIDER-CASE)
  "Returns the index in STRING of the last character before FROM not equal to CHAR, or NIL.
If TO is non-zero, the search stops there, and the value is NIL
if no character different from CHAR appears after there.
TO should normally be less than FROM.
If FROM is omitted or NIL, the default is the end of the string.
Case matters during character comparison if CONSIDER-CASE is non-NIL."
  (IF (FIXNUMP CHAR) (SETQ CHAR (INT-CHAR CHAR)))
  (CHECK-TYPE CHAR CHARACTER)
  (COERCE-STRING-SEARCH-ARG STRING)
  (OR FROM (SETQ FROM (LENGTH STRING)))
  (IF CONSIDER-CASE
      (DO ((I (1- FROM) (1- I)))
          ((< I TO) NIL)
        (OR (CHAR= CHAR (CHAR STRING I))
            (RETURN I)))
      (DO ((I (1- FROM) (1- I)))
          ((< I TO) NIL)
        (OR (CHAR-EQUAL CHAR (CHAR STRING I))
            (RETURN I)))))

(DEFUN STRING-SEARCH (KEY STRING &OPTIONAL (FROM 0) TO (KEY-FROM 0) KEY-TO
                      (CONSIDER-CASE alphabetic-case-affects-string-comparison)
                      &AUX (ALPHABETIC-CASE-AFFECTS-STRING-COMPARISON CONSIDER-CASE)
                      KEY-LEN)
  "Returns the index in STRING of the first occurrence of KEY past FROM, or NIL.
If TO is non-NIL, the search stops there, and the value is NIL
if no occurrence of KEY is found before there.
KEY-FROM and KEY-TO may be used to specify searching for just a substring of KEY.
CONSIDER-CASE if non-NIL means we distinguish letters by case."
  (COERCE-STRING-SEARCH-ARG STRING)
  (COERCE-STRING-ARG KEY)                       ;??
  (UNLESS KEY-TO
    (SETQ KEY-TO (LENGTH KEY)))
  (SETQ KEY-LEN (- KEY-TO KEY-FROM))
  (OR TO (SETQ TO (LENGTH STRING)))
  (COND ((= KEY-FROM KEY-TO)
         (AND ( FROM TO) FROM))
        (T
         (SETQ TO (1+ (- TO KEY-LEN)))          ;Last position at which key may start + 1
         (PROG (CH1)
               (WHEN (MINUSP TO) (RETURN NIL))
               (SETQ CH1 (CHAR KEY KEY-FROM))
            LOOP                                ;Find next place key might start
               (OR (SETQ FROM (%STRING-SEARCH-CHAR CH1 STRING FROM TO))     ;;********************
                   (RETURN NIL))
               (AND (%STRING-EQUAL KEY KEY-FROM STRING FROM KEY-LEN)        ;;********************
                    (RETURN FROM))
               (INCF FROM)                      ;Avoid infinite loop.  %STRING-SEARCH-CHAR
               (GO LOOP)))))                    ;  does right thing if from  to.

(DEFUN STRING-REVERSE-SEARCH (KEY STRING &OPTIONAL FROM (TO 0) (KEY-FROM 0) KEY-TO
                              (CONSIDER-CASE alphabetic-case-affects-string-comparison)
                              &AUX (ALPHABETIC-CASE-AFFECTS-STRING-COMPARISON CONSIDER-CASE)
                              KEY-LEN)
  "Returns the index in STRING of the last occurrence before FROM of KEY, or NIL.
If TO is non-zero, the search stops there, and the value is NIL
if no occurrence of KEY is found after there.
TO should normally be less than FROM.
If FROM is omitted or NIL, the default is the end of the string.
KEY-FROM and KEY-TO may be used to specify searching for just a substring of KEY.
CONSIDER-CASE if non-NIL means we distinguish letters by case."
  (COERCE-STRING-SEARCH-ARG STRING)
  (COERCE-STRING-ARG KEY)               ;??
  (UNLESS KEY-TO (SETQ KEY-TO (LENGTH KEY)))
  (SETQ KEY-LEN (- KEY-TO KEY-FROM))
  (OR FROM (SETQ FROM (LENGTH STRING)))
  (SETQ TO (+ TO (1- KEY-LEN)))         ;First position at which last char of key may be
  (IF (ZEROP KEY-LEN)
      FROM
    (DO ((N (1- FROM) (1- N))
         (CH1 (CHAR KEY (1- KEY-TO))))
        ((< N TO) NIL)
      (AND (CHAR-EQUAL (CHAR STRING N) CH1)
           (%STRING-EQUAL KEY KEY-FROM STRING (1+ (- N KEY-LEN)) KEY-LEN) ;;**********************
           (RETURN (1+ (- N KEY-LEN)))))))

(DEFUN STRING-UPCASE (STRING &KEY (START 0) END)  ;CL compatible
  "Return a copy of STRING with all characters converted to upper case.
START and END can be used to control which part of STRING is
converted, but the entire string will be copied.
Fonts in the characters are not changed."
  (SETQ STRING (STRING-APPEND STRING))          ;Copy so we don't mung original string
                                                ;Note COPY-SEQ is incorrect here.
  (DO ((LEN (OR END (LENGTH STRING)))
       (CHAR)
       (I START (1+ I)))
      ((= I LEN))
    (SETQ CHAR (CHAR STRING I))
    (WHEN (LOWER-CASE-P CHAR)
      (SETF (CHAR STRING I) (CHAR-UPCASE CHAR))))
  STRING)

(DEFUN STRING-DOWNCASE (STRING &KEY (START 0) END)  ;CL compatible
  "Return a copy of STRING with all letters converted to lower case.
START and END can be used to control which part of STRING is
converted, but the entire string will be copied.
Fonts in the characters are not changed."
  (SETQ STRING (STRING-APPEND STRING))          ;Copy so we don't mung original string
                                                ;Note COPY-SEQ is incorrect here.
  (DO ((LEN (OR END (LENGTH STRING)))
       (CHAR)
       (I START (1+ I)))
      ((= I LEN))
    (SETQ CHAR (CHAR STRING I))
    (WHEN (UPPER-CASE-P CHAR)
      (SETF (CHAR STRING I) (CHAR-DOWNCASE CHAR))))
  STRING)

(defun string-flipcase (string &optional (start 0) end (copy-p t))
  "Invert the case (upperlower) of characters in STRING.
Does not affect characters which are not alphabetic.
Symbol*cs braindamage means that this does not take the same argument pattern as
STRING-UP//DOWNCASE."
  (if copy-p (setq string (string-append string)))
  (do ((len (or end (length string)))
       (char)
       (i start (1+ i)))
      ((= i len))
    (setq char (char string i))
    (cond ((upper-case-p char) (setf (char string i) (char-downcase char)))
          ((lower-case-p char) (setf (char string i) (char-upcase char)))))
  string)

(DEFUN STRING-CAPITALIZE (STRING &OPTIONAL &KEY (START 0) END SPACES)  ;CL compatible
  "In STRING, turn hyphens to spaces and make each word be capitalized.
START and END limit the portion of the string converted,
but in any case the entire string is copied.
If SPACES is T, hyphens are changed to spaces."
  (SETQ STRING (STRING-APPEND STRING))          ;Copy so we don't mung original string
                                                ;Note COPY-SEQ is incorrect here.
  (DO ((I START (1+ I))
       (LEN (OR END (LENGTH STRING)))
       PREV-LETTER CH)
      ((= I LEN))
    (SETQ CH (CHAR STRING I))
    (COND ((AND SPACES (CHAR= CH #/-))
           (SETF (CHAR STRING I) #/SPACE)
           (SETQ PREV-LETTER NIL))
          ((UPPER-CASE-P CH)
           (WHEN PREV-LETTER
             (SETF (CHAR STRING I) (CHAR-DOWNCASE CH)))
           (SETQ PREV-LETTER T))
          ((LOWER-CASE-P CH)
           (UNLESS PREV-LETTER
             (SETF (CHAR STRING I) (CHAR-UPCASE CH)))
           (SETQ PREV-LETTER T))
          ((DIGIT-CHAR-P CH)
           (SETQ PREV-LETTER T))
          (T (SETQ PREV-LETTER NIL))))
  STRING)

(DEFUN NSTRING-UPCASE (STRING &KEY (START 0) END) ;CL compatible
  "Return a copy of STRING with all characters converted to upper case.
START and END can be used to control which part of STRING is
converted, but the entire string will be copied.
Fonts in the characters are not changed."
  (DO ((LEN (OR END (LENGTH STRING)))
       (CHAR)
       (I START (1+ I)))
      ((= I LEN))
    (SETQ CHAR (CHAR STRING I))
    (WHEN (LOWER-CASE-P CHAR)
      (SETF (CHAR STRING I) (CHAR-UPCASE CHAR))))
  STRING)

(DEFUN NSTRING-DOWNCASE (STRING &KEY (START 0) END) ;CL compatible
  "Return a copy of STRING with all letters converted to lower case.
START and END can be used to control which part of STRING is
converted, but the entire string will be copied.
Fonts in the characters are not changed."
  (DO ((LEN (OR END (LENGTH STRING)))
       (CHAR)
       (I START (1+ I)))
      ((= I LEN))
    (SETQ CHAR (CHAR STRING I))
    (WHEN (UPPER-CASE-P CHAR)
      (SETF (CHAR STRING I) (CHAR-DOWNCASE CHAR))))
  STRING)

(DEFUN NSTRING-CAPITALIZE (STRING &OPTIONAL &KEY (START 0) END SPACES) ;CL compatible
  "In STRING, turn hyphens to spaces and make each word be capitalized.
START and END limit the portion of the string converted,
but in any case the entire string is copied.
If SPACES is T, hyphens are changed to spaces."
  (DO ((I START (1+ I))
       (LEN (OR END (LENGTH STRING)))
       PREV-LETTER CH)
      ((= I LEN))
    (SETQ CH (CHAR STRING I))
    (COND ((AND SPACES (= CH #/-))
           (SETF (CHAR STRING I) #/SPACE)
           (SETQ PREV-LETTER NIL))
          ((UPPER-CASE-P CH)
           (WHEN PREV-LETTER
             (SETF (CHAR STRING I) (CHAR-DOWNCASE CH)))
           (SETQ PREV-LETTER T))
          ((LOWER-CASE-P CH)
           (UNLESS PREV-LETTER
             (SETF (CHAR STRING I) (CHAR-UPCASE CH)))
           (SETQ PREV-LETTER T))
          ((DIGIT-CHAR-P CH)
           (SETQ PREV-LETTER T))
          (T (SETQ PREV-LETTER NIL))))
  STRING)

(DEFUN STRING-CAPITALIZE-WORDS (STRING &OPTIONAL (COPY-P T) (SPACES T))
  "In STRING, turn hyphens to spaces and make each word be capitalized.
If SPACES is NIL, hyphens are not changed.
Copies the original string unless COPY-P is NIL, meaning mung the original."
  (OR (AND (NOT COPY-P) (STRINGP STRING))
      (SETQ STRING (STRING-APPEND STRING)))
  (NSTRING-CAPITALIZE STRING :SPACES SPACES))

(DEFUN STRING-REMOVE-FONTS (STRING)
  "Return a copy of STRING, with all characters changed to font 0.
If STRING already has all characters in font 0, it may not be copied."
  (IF (AND (VECTORP STRING)
           (EQ (ARRAY-TYPE STRING) 'ART-STRING))
      STRING
    (LET ((NEWSTRING (MAKE-STRING (LENGTH STRING))))
      ;; this ignores high bits
      (COPY-ARRAY-CONTENTS STRING NEWSTRING)
      NEWSTRING)))

(DEFUN STRING-NREVERSE (STRING &AUX LEN)
  "Destructively modify string by reversing the order of its elements.
Actually, this will work on any one-dimensional array."
  (TYPECASE STRING
    ((OR FIXNUM CHARACTER))
    (T (TYPECASE STRING
         (VECTOR)
         (SYMBOL
; no longer needed since pnames are now in a read-only area
;         ;; Special treatment to avoid munging symbols
;         (WHEN (SYMBOL-PACKAGE STRING)
;           (FERROR "Illegal to mung the PNAME of an interned symbol."))
          (SETQ STRING (SYMBOL-NAME STRING)))
         (T (COERCE-STRING-ARG STRING)))
       (SETQ LEN (LENGTH STRING))
       (DO ((I 0 (1+ I))
            (J (1- LEN) (1- J)))
           ((< J I))
         (SWAPF (CHAR STRING I) (CHAR STRING J)))))
  STRING)

(DEFUN STRING-REVERSE (STRING)
  "Return a string whose elements are those of STRING, in reverse order.
Actually, this will work on any one-dimensional array."
  (STRING-NREVERSE (IF (STRINGP STRING) (COPY-SEQ STRING) (STRING STRING))))

;;; Internal function.
(DEFUN ARRAY-MEM (FUNCTION ITEM ARRAY)
  (DOTIMES (I (LENGTH ARRAY))
    (IF (FUNCALL FUNCTION ITEM (CL:AREF ARRAY I))
        (RETURN T))))

(DEFUN STRING-SEARCH-SET (CHAR-SET STRING &OPTIONAL (FROM 0) TO CONSIDER-CASE)
  "Returns the index in STRING of the first char past FROM that's in CHAR-SET, or NIL.
CHAR-SET can be a list of characters or a string.
If TO is non-NIL, the search stops there, and the value is NIL
if no occurrence of a char in CHAR-SET is found before there.
Case matters during character comparison if CONSIDER-CASE is non-NIL."
  (CTYPECASE CHAR-SET
    ((OR CHARACTER FIXNUM)
     (STRING-SEARCH-CHAR CHAR-SET STRING FROM TO CONSIDER-CASE))
    (SEQUENCE
     (IF (NULL CHAR-SET)
         NIL
       (COERCE-STRING-SEARCH-ARG STRING)
       (OR TO (SETQ TO (LENGTH STRING)))
       (DO ((I FROM (1+ I))
            (FUN (IF (CL:LISTP CHAR-SET) #'MEM #'ARRAY-MEM)))
           (( I TO) NIL)
         (AND (IF CONSIDER-CASE
                  (FUNCALL FUN #'CHAR= (CHAR STRING I) CHAR-SET)
                  (FUNCALL FUN #'CHAR-EQUAL (CHAR STRING I) CHAR-SET))
              (RETURN I)))))))

(DEFUN STRING-REVERSE-SEARCH-SET (CHAR-SET STRING &OPTIONAL FROM (TO 0) CONSIDER-CASE)
  "Returns the index in STRING of the last char before FROM that's in CHAR-SET, or NIL.
CHAR-SET can be a list of characters or a string.
If TO is non-NIL, the search stops there, and the value is NIL
if no occurrence of a char in CHAR-SET is found after there.
TO is normally less than FROM.
Case matters during character comparison if CONSIDER-CASE is non-NIL."
  (CTYPECASE CHAR-SET
    ((OR CHARACTER FIXNUM)
     (STRING-REVERSE-SEARCH-CHAR CHAR-SET STRING FROM TO CONSIDER-CASE))
    (SEQUENCE
     (IF (NULL CHAR-SET)
         NIL
       (COERCE-STRING-SEARCH-ARG STRING)
       (OR FROM (SETQ FROM (LENGTH STRING)))
       (DO ((I (1- FROM) (1- I))
            (FUN (IF (CL:LISTP CHAR-SET) #'MEM #'ARRAY-MEM)))
           ((< I TO) NIL)
         (AND (IF CONSIDER-CASE
                  (FUNCALL FUN #'CHAR= (CHAR STRING I) CHAR-SET)
                  (FUNCALL FUN #'CHAR-EQUAL (CHAR STRING I) CHAR-SET))
              (RETURN I)))))))

(DEFUN STRING-SEARCH-NOT-SET (CHAR-SET STRING &OPTIONAL (FROM 0) TO CONSIDER-CASE)
  "Returns the index in STRING of the first char past FROM that's NOT in CHAR-SET, or NIL.
CHAR-SET can be a list of characters or a string.
If TO is non-NIL, the search stops there, and the value is NIL
if no occurrence of a char not in CHAR-SET is found before there.
Case matters during character comparison if CONSIDER-CASE is non-NIL."
  (CTYPECASE CHAR-SET
    ((OR CHARACTER FIXNUM)
     (STRING-SEARCH-NOT-CHAR CHAR-SET STRING FROM TO CONSIDER-CASE))
    (SEQUENCE
     (IF (NULL CHAR-SET)
         NIL
       (COERCE-STRING-SEARCH-ARG STRING)
       (OR TO (SETQ TO (LENGTH STRING)))
       (DO ((I FROM (1+ I))
            (FUN (IF (CL:LISTP CHAR-SET) #'MEM #'ARRAY-MEM)))
           (( I TO) NIL)
         (OR (IF CONSIDER-CASE
                 (FUNCALL FUN #'CHAR= (CHAR STRING I) CHAR-SET)
                 (FUNCALL FUN #'CHAR-EQUAL (CHAR STRING I) CHAR-SET))
             (RETURN I)))))))

(DEFUN STRING-REVERSE-SEARCH-NOT-SET (CHAR-SET STRING &OPTIONAL FROM (TO 0) CONSIDER-CASE)
  "Returns the index in STRING of the last char before FROM that's NOT in CHAR-SET, or NIL.
CHAR-SET can be a list of characters or a string.
If TO is non-NIL, the search stops there, and the value is NIL
if no occurrence of a char not in CHAR-SET is found after there.
TO is normally less than FROM.
Case matters during character comparison if CONSIDER-CASE is non-NIL."
  (CTYPECASE CHAR-SET
    ((OR CHARACTER FIXNUM)
     (STRING-REVERSE-SEARCH-NOT-CHAR CHAR-SET STRING FROM TO CONSIDER-CASE))
    (SEQUENCE
     (IF (NULL CHAR-SET)
         NIL
       (COERCE-STRING-SEARCH-ARG STRING)
       (OR FROM (SETQ FROM (LENGTH STRING)))
       (DO ((I (1- FROM) (1- I))
            (FUN (IF (CL:LISTP CHAR-SET) #'MEM #'ARRAY-MEM)))
           ((< I TO) NIL)
         (OR (IF CONSIDER-CASE
                 (FUNCALL FUN #'CHAR= (CHAR STRING I) CHAR-SET)
                 (FUNCALL FUN #'CHAR-EQUAL (CHAR STRING I) CHAR-SET))
             (RETURN I)))))))

(DEFUN STRING-TRIM (CHAR-SET STRING &AUX I J)       ;CL compatible
  "Return a copy of STRING with all characters in CHAR-SET removed at both ends.
CHAR-SET can be a list of characters or a string.
As of now, case is ignored in comparisons."
  (COERCE-STRING-ARG STRING)
  (SETQ I (STRING-SEARCH-NOT-SET CHAR-SET STRING 0 NIL T))
  (IF (NULL I) ""
    (SETQ J (STRING-REVERSE-SEARCH-NOT-SET CHAR-SET STRING NIL 0 T))
    (SUBSTRING STRING I (1+ J))))

(DEFUN STRING-LEFT-TRIM (CHAR-SET STRING &AUX I)     ;CL compatible
  "Return a copy of STRING with all characters in CHAR-SET removed at the beginning.
CHAR-SET can be a list of characters or a string.
As of now, case is ignored in comparisons."
  (COERCE-STRING-ARG STRING)
  (SETQ I (STRING-SEARCH-NOT-SET CHAR-SET STRING 0 NIL T))
  (IF I
      (SUBSTRING STRING I (STRING-LENGTH STRING))
    ""))

(DEFUN STRING-RIGHT-TRIM (CHAR-SET STRING &AUX I)    ;CL compatible
  "Return a copy of STRING with all characters in CHAR-SET removed at the end.
CHAR-SET can be a list of characters or a string.
As of now, case is ignored in comparisons."
  (COERCE-STRING-ARG STRING)
  (SETQ I (STRING-REVERSE-SEARCH-NOT-SET CHAR-SET STRING NIL 0 T))
  (IF I
      (SUBSTRING STRING 0 (1+ I))
    ""))

(DEFUN STRING-SUBST-CHAR (NEW OLD STRING &OPTIONAL (COPY-P T) (RETAIN-FONT-P T))
  "Substitute the NEW character at every occurence of OLD in STRING.
Copies the original string unless COPY-P is NIL, meaning mung the original.
If RETAIN-FONT-P is T, then the font of each repective OLD character is retained.
As of now, case is ignored in comparisons."
  (OR (AND (NOT COPY-P) (STRINGP STRING))
      (SETQ STRING (STRING-APPEND STRING)))
  (LET ((END (STRING-LENGTH STRING)))
    (DO ((NEW (CHARACTER NEW))
         (OLD (CHARACTER OLD))
         (I (%STRING-SEARCH-CHAR OLD STRING 0 END) (%STRING-SEARCH-CHAR OLD STRING I END)) ;******
         TEM)
        ((NULL I))
      (SETQ TEM (CHAR STRING I))
      (SETF (CHAR STRING I)
            (IF RETAIN-FONT-P
                (MAKE-CHAR NEW (CHAR-BITS TEM) (CHAR-FONT TEM))
              NEW))))
  STRING)

;;; T means case matters in string comparisons, NIL means it is ignored.
;;; This is bound to T by certain routines, such as INTERN, but I do not
;;; recommend changing its global value to T rather than NIL; many system
;;; functions, or at least their user interfaces, assume that string
;;; comparison is case-insensitive.
;;>> What a crock
(DEFVAR-RESETTABLE ALPHABETIC-CASE-AFFECTS-STRING-COMPARISON () ()
  "Microcode flag which controls whether %STRING-EQUAL and %STRING-SEARCH consider case.")

(DEFUN STRING-COMPARE (STR1 STR2 &OPTIONAL (IDX1 0) (IDX2 0) LIM1 LIM2
                       (CONSIDER-CASE ALPHABETIC-CASE-AFFECTS-STRING-COMPARISON))
  "Compares the two substrings in dictionary order.
Returns a positive number if STR1>STR2.
Returns zero if STR1=STR2.
Returns a negative number if STR1<STR2.
If the strings are not equal, the absolute value of the number returned is
one more than the index (in STR1) at which the difference occured.
It is possible to compare only part of a string.
Only the part of STR1 from IDX1 to LIM1 is compared;
only the part of STR2 from IDX2 to LIM2 is compared."
  (COERCE-STRING-ARG STR1)
  (COERCE-STRING-ARG STR2)
  (OR LIM1 (SETQ LIM1 (LENGTH STR1)))
  (OR LIM2 (SETQ LIM2 (LENGTH STR2)))
  (PROG ()
     L  (AND ( IDX1 LIM1)
             (RETURN (IF (< IDX2 LIM2) (MINUS (1+ IDX1)) 0)))
        (AND ( IDX2 LIM2)
             (RETURN (1+ IDX1)))
        (WHEN (IF CONSIDER-CASE
                  (CHAR= (CHAR STR1 IDX1) (CHAR STR2 IDX2))
                  (CHAR-EQUAL (CHAR STR1 IDX1) (CHAR STR2 IDX2)))
          (INCF IDX1) (INCF IDX2)
          (GO L))
        (IF (IF CONSIDER-CASE
                (CHAR< (CHAR STR1 IDX1) (CHAR STR2 IDX2))
                (< (CHAR-UPCASE (CHAR STR1 IDX1)) (CHAR-UPCASE (CHAR STR2 IDX2))))
            (RETURN (MINUS (1+ IDX1)))
            (RETURN (1+ IDX1)))))

(DEFUN STRING< (STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2) ;CL compatible
  "True if STRING1 (or substring) is less than STRING2 (or substring) in dictionary order.
The value is actually the index of the first difference between the strings.
Keyword arg :START1 is where to start comparing, in STRING1, and :END1 where to stop.
Similar for :START2 and :END2.  Case is significant in the comparison."
  (LET ((V (STRING-COMPARE STRING1 STRING2 START1 START2 END1 END2 T)))
    (IF (MINUSP V) (1- (ABS V)))))

(DEFUN STRING> (STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2) ;CL compatible
  "True if STRING1 (or substring) is greater than STRING2 (or substring) in dictionary order.
The value is actually the index of the first difference between the strings.
Keyword arg :START1 is where to start comparing, in STRING1, and :END1 where to stop.
Similar for :START2 and :END2.  Case is significant in the comparison."
  (LET ((V (STRING-COMPARE STRING1 STRING2 START1 START2 END1 END2 T)))
    (IF (PLUSP V) (1- (ABS V)))))

;; Copied from LAD: RELEASE-3.SYS2; STRING.LISP#161 on 2-Oct-86 04:36:54
(DEFUN STRING<= (STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2) ;CL compatible
  "True if STRING1 (or substring) is  STRING2 (or substring) in dictionary order.
The value is actually the index of the first difference between the strings,
or their length if they match.
Keyword arg :START1 is where to start comparing, in STRING1, and :END1 where to stop.
Similar for :START2 and :END2.  Case is significant in the comparison."
  (LET ((V (STRING-COMPARE STRING1 STRING2 START1 START2 END1 END2 T)))
    (COND ((MINUSP V) (- (ABS V) 1))
          ((ZEROP V) (- (OR END1 (LENGTH STRING1)) START1)))))


(DEFUN STRING>= (STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2) ;CL compatible
  "True if STRING1 (or substring) is  STRING2 (or substring) in dictionary order.
The value is actually the index of the first difference between the strings,
or their length if they match.
Keyword arg :START1 is where to start comparing, in STRING1, and :END1 where to stop.
Similar for :START2 and :END2.  Case is significant in the comparison."
  (LET ((V (STRING-COMPARE STRING1 STRING2 START1 START2 END1 END2 T)))
    (COND ((PLUSP V) (1- V))
          ((ZEROP V) (- (OR END1 (LENGTH STRING1)) START1)))))
(DEFF STRING 'STRING>=)

;can't use (not (string= ...)) since need value returned
(DEFUN STRING//= (STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2) ;CL STRING/=
  "True if STRING1 (or substring) and STRING2 (or substring) are different.
The value is actually the index of the first difference between the strings.
Keyword arg :START1 is where to start comparing, in STRING1, and :END1 where to stop.
Similar for :START2 and :END2.  Case is significant in the comparison."
  (LET ((V (STRING-COMPARE STRING1 STRING2 START1 START2 END1 END2 T)))
    (UNLESS (ZEROP V) (1- (ABS V)))))
(DEFF STRING 'STRING//=)

(DEFUN STRING-LESSP (STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2) ;CL compatible
  "True if STRING1 (or substring) is less than STRING2 (or substring) in dictionary order.
The value is actually the index of the first difference between the strings.
Keyword arg :START1 is where to start comparing, in STRING1, and :END1 where to stop.
Similar for :START2 and :END2.  Case is ignored when comparing letters."
  (LET ((V (STRING-COMPARE STRING1 STRING2 START1 START2 END1 END2 NIL)))
    (IF (MINUSP V) (1- (ABS V)))))

(DEFUN STRING-GREATERP (STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2) ;CL compatible
  "True if STRING1 (or substring) is greater than STRING2 (or substring) in dictionary order.
The value is actually the index of the first difference between the strings.
Keyword arg :START1 is where to start comparing, in STRING1, and :END1 where to stop.
Similar for :START2 and :END2.  Case is ignored when comparing letters."
  (LET ((V (STRING-COMPARE STRING1 STRING2 START1 START2 END1 END2 NIL)))
    (IF (PLUSP V) (1- (ABS V)))))

;; Copied from LAD: RELEASE-3.SYS2; STRING.LISP#161 on 2-Oct-86 04:36:56
(DEFUN STRING-NOT-GREATERP (STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2) ;CL compatible
  "True if STRING1 (or substring) is  to STRING2 (or substring) in dictionary order.
The value is actually the index of the first difference between the strings,
or their length if they match.
Keyword arg :START1 is where to start comparing, in STRING1, and :END1 where to stop.
Similar for :START2 and :END2.  Case is ignored when comparing letters."
  (LET ((V (STRING-COMPARE STRING1 STRING2 START1 START2 END1 END2 NIL)))
    (COND ((MINUSP V) (- (ABS V) 1))
          ((ZEROP V) (- (OR END1 (LENGTH STRING1)) START1)))))

(DEFUN STRING-NOT-LESSP (STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2) ;CL compatible
  "True if STRING1 (or substring) is  to STRING2 (or substring) in dictionary order.
The value is actually the index of the first difference between the strings,
or their length if they match.
Keyword arg :START1 is where to start comparing, in STRING1, and :END1 where to stop.
Similar for :START2 and :END2.  Case is ignored when comparing letters."
  (LET ((V (STRING-COMPARE STRING1 STRING2 START1 START2 END1 END2 NIL)))
    (COND ((PLUSP V) (1- V))
          ((ZEROP V) (- (OR END1 (LENGTH STRING1)) START1)))))

(DEFUN ALPHALESSP (X Y)
  "T if printed representation of X is less than that of Y.
Characters and numbers come before symbols//strings, before random objects, before lists.
Characters and numbers are compared using CHAR<; symbols//strings with STRING-LESSP;
random objecs by printing them(!); lists are compared recursively."
  (IF (fixnump X) (SETQ X (INT-CHAR X)))
  (IF (fixnump Y) (SETQ Y (INT-CHAR Y)))
  (COND ((CHARACTERP X)
         (OR (NOT (CHARACTERP Y))
             (CHAR< X Y)))
        ((CHARACTERP Y) NIL)
        ((OR (SYMBOLP X) (STRINGP X))
         (OR (NOT (OR (SYMBOLP Y) (STRINGP Y)))
             (STRING-LESSP X Y)))
        ((OR (SYMBOLP Y) (STRINGP Y)) NIL)
        ((ATOM X) (OR (CONSP Y)
                      (STRING-LESSP (FORMAT NIL "~S" X) (FORMAT NIL "~S" Y))))
        ((ATOM Y) NIL)
        (T (DO ((X1 X (CDR X1)) (Y1 Y (CDR Y1)))
               ((NULL Y1))
             (OR X1 (RETURN T))
             (AND (ALPHALESSP (CAR X1) (CAR Y1)) (RETURN T))
             (AND (ALPHALESSP (CAR Y1) (CAR X1)) (RETURN NIL))))))

(DEFUN ALPHAEQUAL (X Y)
  "T if X and Y print the same, or nearly so.
Exceptions: numbers and characters are compared using =
and a symbol and its pname compare as equal."
  (IF (NUMBERP X) (SETQ X (INT-CHAR X)))
  (IF (NUMBERP Y) (SETQ Y (INT-CHAR Y)))
  (TYPECASE X
    (CHARACTER
     (AND (CHARACTERP Y)
          (= X Y)))
    ((OR SYMBOL STRING)
     (AND (OR (SYMBOLP Y) (STRINGP Y))
          (STRING-EQUAL X Y)))
    (ATOM
     (AND (ATOM Y)
          (STRING-EQUAL (FORMAT NIL "~S" X) (FORMAT NIL "~S" Y))))
    (T (DO ((X1 X (CDR X1)) (Y1 Y (CDR Y1)))
           ((NULL X1) (NULL Y1))
         (OR Y1 (RETURN NIL))
         (OR (ALPHAEQUAL (CAR X1) (CAR Y1)) (RETURN NIL))))))

(DEFUN STRING (X) ;CL compatible
  "Convert X to a string if possible."
  (IF (fixnump X) (SETQ X (INT-CHAR X)))
  (TYPECASE X
    (STRING X)
    (SYMBOL (SYMBOL-NAME X))
    (STRING-CHAR
     (VALUES (MAKE-STRING 1 :INITIAL-ELEMENT X)))
    (INSTANCE
     (SEND X :SEND-IF-HANDLES :STRING-FOR-PRINTING))
    (T
     (if (fixnum-arrayp x)
         (let ((ans (make-string (array-active-length x))))
           (dotimes (index (array-active-length x))
             (setf (aref ans index)
                   (int-char (aref x index))))
           ans)
         (FERROR "Cannot convert ~S into a string." X)))))

(DEFUN ZL:CHARACTER (X)
  "Convert X to a fixnum representing character if possible.
This is the same as (CHAR-INT (CL:CHARACTER X))"
  (COND ((NUMBERP X)
         X)
        ((CHARACTERP X)
         (CHAR-INT X))
        ((AND (STRINGP X) (= (LENGTH X) 1))
         (CHAR-INT (CHAR X 0)))
        ((AND (SYMBOLP X) (= (LENGTH (GET-PNAME X)) 1))
         (CHAR-INT (CHAR (SYMBOL-NAME X) 0)))
        (T (FERROR "Cannot convert ~S into a character." X))))

(defun string-pluralize (string)
  "Return a plural form of STRING.
Attempts to preserve the case-pattern in STRING."
  (coerce-string-arg string)
  (if (equal string "")
      ""
    (let* (flush add
           (last-char-raw (char string (1- (string-length string))))
           (last-char (char-upcase last-char-raw))
           (last-char-lc-flag (char last-char last-char-raw))
           (penult-char (char-upcase (if (> (string-length string) 1)
                                         (char string (- (string-length string) 2))
                                         0)))
           (last-3 (substring string (max 0 (- (string-length string) 3)))))
      (cond ((and (char-equal last-char #/Y)
;character lossage assumes font=0
                  (not (memq penult-char '(#/A #/E #/I #/O #/U))))
             (setq flush 1 add "ies"))
            ((or (string-equal string "ox") (string-equal string "vax"))
             (setq add "en"))
            ((or (and (eq last-char #/H)
                      (memq penult-char '(#/C #/S)))
                 (memq last-char '(#/S #/Z #/X)))
             (setq add "es"))
            ((string-equal last-3 "man")
             (setq flush 2 add "en"))
            ((string-equal last-3 "fan")
             (setq flush 2 add "en"))
            ((string-equal last-3 "ife")
             (setq flush 2 add "ves"))
            (t (setq add "s")))
      (and flush (setq string (substring string 0 (- (string-length string) flush))))
      (cond (add (string-append string
                                (cond (last-char-lc-flag add)
                                      (t (string-upcase add)))))
            (t string)))))


;;; STRING-APPEND-A-OR-AN sophistication taken to a new high by Dan Hoey
;;; of NRL, July 1985, and graciously contributed to the LMI system.

(defun string-append-a-or-an (noun-string &rest more-strings)
  "Appends the strings, with /"a /" or /"an /" added in front."
  (apply #'string-append
         (string-select-a-or-an noun-string)
         " "
         noun-string
         more-strings))

;;; Leave this macro here for now, the idea being that LMI definitions should
;;; go right here for easy recompilation, and user definitions shouldn't be
;;; a problem since STRING is in the cold load.
(defmacro def-a-or-an (&rest a-or-an-pairs)
  "Arguments are alternating article keywords (:A or :AN) and prefixes of
  words to which the articles apply.  Returns the inverse operation for
  LOGIN-EVAL.  (DEF-A-OR-AN :A /"/") sets all words to use A."
  (loop with a-or-an = 'none
        with outform = nil
        for arg in a-or-an-pairs
    do (case arg
         (:a  (setq a-or-an nil))
         (:an (setq a-or-an t))
         (t (if (eq a-or-an 'none)
                (ferror ":A or :AN expected, not ~S" arg))
            (setq outform
                  `(hack-a-or-an-table ',arg ,a-or-an ,outform))))
    finally (return ``(def-a-or-an ,@,outform))))

(defvar
  *a-or-an-patterns*
  '(""
    ;; Everything is a consonant (except
    ("A" "E" ("EU")
     ;; an apple, an exception (except a euphemism), an F (except a foo),
     "F" ("FA" "FE" "FI" "FJ" "FL" "FN" "FO" "FR" "FU" "FW" "FY") "H"
     ;; an H (except a ha (except an habanera), a he (except an heiress or
     ("HA" ("HABAN") "HE"
      ("HEIR" "HERB" ("HERBAC" "HERBAR" "HERBE" "HERBI")) "HI" "HO"
      ;; an herbalist (except a herbaceous, a herbarium, a herbert, or
      ;; a herbivore)), a hi, a ho (except an homage, an hombre,
      ("HOMA" "HOMBR" "HONEST" "HONOR" "HORS" ("HORSE" "HORST" "HORSY")
       ;; an honest, an honorarium, an hors d'oeuvre (except a horse,
       ;; a horst, or a horsy),  or an hour (except a houri)), a hug,
       "HOUR" ("HOURI")) "HU" "HY")
     ;; or a hype), an iota, an L (except a lot),
     "I" "L" ("LA" "LE" "LF" "LH" "LI" "LL" "LO" "LU" "LY")
     "M" ("MA" "MC" "ME" "MI" "ML" "MN" "MO" "MR" "MS" "MU" "MW" "MY")
     ;; an M (except a multitude), an N (except a number),
     "N" ("NA" "NB" "NE" "NG" "NI" "NO" "NU" "NY")
     ;; an other (except a once and future or a one (except an Onega
     "O" ("ONCE" "ONE" ("ONEG" "ONEI"))
     ;; or an oneiromancer), an R (except a riot),
     "R" ("RA" "RE" "RH" "RI" "RM" "RO"  "RU" "RY")
     "S" ("SA" "SC" "SE" "SH" "SI" "SJ" "SK" "SL" "SM" "SN" "SO" "SP"
          "SQ" "SR" "ST" "SU" "SV" "SW" "SY" "SZ") "UD" "UG" "UH" "UL" "UM"
     ;; an S (except a superfluity), an udder, an ugh, an uh, an ulcer,
     ;; an um, an unlikelihood, (except a unanimity, a unanimous decision,
     "UN" ("UNANIMI" "UNANIMO"
           ;; a universal botch (except an unidentified case (except
           ;; a unidimensional one), an unignorable,  an unilluminated,
           "UNI" ("UNID" ("UNIDI") "UNIGN" "UNILL" "UNIM" ("UNIMO")
                  ;; an unimpressive (except a unimodal), an uninteresting,
                  ;; an uniodized, an unironed, an unissued, an unitalicized,
                  ;; an unitemized uneogh!!)),
                  "UNIN" "UNIOD" "UNIR" "UNISS" "UNITAL" "UNITEM"))
     "UP" "UR" ("URA" "URE" "URI" "URO") "USH" "UTM" "UTT" "UX" "UZ"
     ;; an upper at last, an urge, (except a uranous, a ureous, a uriniferous,
     ;; a urologist), an usher, an utmost or an utter one, an uxoricide by
     ;; an Uzi, an X (xcept a xoo), an yclept, an Yggdrasil, an ylang-ylang,
     ;; an yngvi, or an yttride)
     "X" ("XA" "XE" "XI" "XU" "XY") "YC" "YG" "YL" "YNG" "YT"))
  "Exceptions list for the article ``AN''.  Each exception is a
   prefix, possibly followed by its exception list")

(defvar *alphabet* "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
  "A string containing all the upper case and all the lower case letters.")

;; Copied from LAD: RELEASE-3.SYS2; STRING.LISP#161 on 2-Oct-86 04:37:00
(defun hack-a-or-an-table (arg insertp undo-list
                           &optional (pattern
                                       (cond ((get '*a-or-an-patterns* 'original-value)
                                              *a-or-an-patterns*)
                                             ('else
                                              (putprop '*a-or-an-patterns*
                                                       *a-or-an-patterns*
                                                       'original-value)
                                              (setq *a-or-an-patterns* (copy-tree *a-or-an-patterns*)))))
                           (a-or-an ':a) (na-ro-a ':an))
  "Search for ARG in PATTERN, a list of A-OR-AN (not NA-RO-A) types.
  If INSERTP, we are inserting ARG.  Cons the inverse operations onto
  UNDO-LIST and return it."
  (when (string-search-not-set *alphabet* (setq arg (string-upcase arg)))
    (ferror "String of letters required, not ~S" arg))

  (loop with holder = (cons nil (cadr pattern))
        with patptr = holder
        as compare = (and (cdr patptr) (string-compare (cadr patptr) arg))

        ;; until ARG precedes and and is disjoint from PATPTR.
        while (and compare
                   (or ( compare 0)
                       (> compare (string-length arg))))

     do (cond ((< compare (- (string-length (cadr patptr))))
               ;; ARG follows and is included in PATPTR, go down
               (and (or (null (cddr patptr)) (stringp (caddr patptr)))
                    (push nil (cddr patptr)))
               (setf (cadr pattern) (cdr holder)
                     pattern        (cdr patptr)
                     (cdr holder)   (cadr pattern)
                     patptr         holder
                     insertp        (not insertp))
               (rotatef a-or-an na-ro-a))

              ((< compare 0)
               ;; ARG follows and is disjoint from PATPTR, keep looking
               (pop patptr)
               (or (null (cdr patptr)) (stringp (cadr patptr))
                   (pop patptr)))

              ;; ARG includes PATPTR, delete PATPTR
              ((loop until (or (null (cddr patptr)) (stringp (caddr patptr)))
                  do (setq undo-list
                           (hack-a-or-an-table (caaddr patptr) NIL undo-list
                                               (cdr patptr) na-ro-a a-or-an))
                  finally (push (cadr patptr) undo-list)
                  finally (push na-ro-a undo-list)
                  finally (pop (cdr patptr)))))

     finally (when insertp
               (unless (and undo-list (equal arg (cadr undo-list)))
                 (push arg undo-list)
                 (push a-or-an undo-list))
               (push arg (cdr patptr)))
     finally (or (setf (cadr pattern) (cdr holder))
                 (eq pattern *a-or-an-patterns*)
                 (pop (cdr pattern)))
     finally (return undo-list)))

(defun string-select-a-or-an (string)
  "Returns /"a/" or /"an/", lowercase, according to contents of STRING."
  (setq string (string string))
  (loop with alphalen = (or (string-search-not-set *alphabet* string)
                            (string-length string))
        with pattern = (cadr *a-or-an-patterns*)
        with ans = "a"
        with sna = "an"
        as compare = (and pattern (string-compare (car pattern) string
                                                  0 0 nil alphalen nil))

    ;; return when STRING precedes PATTERN
    unless (and compare ( compare 0))
      return ans

    ;; STRING is included in PATTERN, go down if possible
    if (or (= compare 0) (< compare (- (string-length (car pattern)))))
      if (or (null (cdr pattern)) (stringp (cadr pattern)))
        return sna
      else do
        (psetq ans sna sna ans pattern (cadr pattern))
    ;; STRING follows PATTERN, keep looking
    else do
      (setq pattern (if (or (null (cdr pattern)) (stringp (cadr pattern)))
                        (cdr pattern)
                      (cddr pattern)))))

(DEFUN PARSE-INTEGER (STRING &KEY (START 0) END (RADIX 10.) JUNK-ALLOWED)
  "Return a number parsed from the contents of STRING, or a part of it.
START and END are indices specify the part of the string; END = NIL means the end of it.
RADIX is a number from 2 to 36, which defaults to ten.

If JUNK-ALLOWED is NIL, the string must contain simply a number
surrounded by whitespace.  The number must be just digits (including suitable
letters if RADIX is > 10) and an optional sign.  Otherwise it is an error.

If JUNK-ALLOWED is non-NIL, parsing stops when anything is encountered that
doesn't fit that description, or at non-leading whitespace.
If no number has been found by then, the value is NIL.

The second value is the index in STRING of where parsing stopped."
  (DECLARE (VALUES NUMBER-OR-MAYBE-NIL STOP-INDEX))
  (SETQ STRING (STRING STRING))
  (LET ((INDEX START)
        (STOP (OR END (LENGTH STRING)))
        NUM SIGN TEM)
    ;; Skip initial whitespace.
    (DO () ((= INDEX STOP))
      (LET ((CH (CHAR STRING INDEX)))
;character lossage font
        (UNLESS (MEMQ CH '(#/SP #/TAB))
          (RETURN))
        (INCF INDEX)))
    (DO ()
        ((= INDEX STOP))
      (LET ((CH (CHAR-UPCASE (CHAR STRING INDEX))))
        (COND ((SETQ TEM (DIGIT-CHAR-P CH))
               (SETQ NUM (+ (* (OR NUM 0) RADIX) TEM)))
;character lossage font
              ((AND (NULL SIGN) (NULL NUM) (EQ CH #/+)) (SETQ SIGN 1))
              ((AND (NULL SIGN) (NULL NUM) (EQ CH #/-)) (SETQ SIGN -1))
              (T
               (UNLESS JUNK-ALLOWED
                 ;; No junk allowed.  Skip any trailing whitespace.
                 (DO () ((= INDEX STOP))
;character lossage font
                   (LET ((CH (CHAR STRING INDEX)))
                     (UNLESS (MEMQ CH '(#/SP #/TAB))
                       (RETURN))
                     (INCF INDEX)))
                 ;; If this did not get us to the end, barf.
                 (UNLESS (= INDEX STOP)
                   (FERROR "~S does not contain simply a number surrounded by whitespace."
                           STRING)))
               (RETURN)))
        (INCF INDEX)))
    (VALUES (AND NUM (IF (EQ SIGN -1) (- NUM) NUM))
            INDEX)))

(DEFUN PARSE-NUMBER (STRING &OPTIONAL (FROM 0) TO (RADIX 10.) FAIL-IF-NOT-WHOLE-STRING)
  "Return a number parsed from the contents of STRING, or a part of it.
FROM and TO specify the part of the string; TO = NIL means the end of it.
RADIX defaults to decimal.

If the string or part doesn't start with a number, NIL is returned.
The second value is the index in STRING of the first non-digit, or NIL if none.
FAIL-IF-NOT-WHOLE-STRING means return NIL and 0 unless the whole string or
specified part can be parsed as a number."
  (LOOP WITH SIGN = NIL
        WITH NUM = NIL
        WITH RADIX = (OR RADIX 10.)
        WITH TEM
        FOR I FROM FROM BELOW (OR TO (STRING-LENGTH STRING))
        FOR CH = (CHAR STRING I)
     DO (COND ((SETQ TEM (DIGIT-CHAR-P CH))
               (SETQ NUM (+ (* (OR NUM 0) RADIX) TEM)))
;character lossage font
              ((AND (NULL SIGN) (NULL NUM) (EQ CH #/+)) (SETQ SIGN 1))
;character lossage font
              ((AND (NULL SIGN) (NULL NUM) (EQ CH #/-)) (SETQ SIGN -1))
              (T
               (IF FAIL-IF-NOT-WHOLE-STRING
                   (RETURN (VALUES NIL 0))
                 (LOOP-FINISH))))
     FINALLY (RETURN (VALUES (AND NUM (IF (EQ SIGN -1) (- NUM) NUM))
                             (IF NUM I 0)))))

(DEFUN NUMBER-INTO-ARRAY (ARRAY N &OPTIONAL (RADIX *PRINT-BASE*) (AT-INDEX 0) (MIN-COLUMNS 0))
  "Store a printed representation of the fixnum N into ARRAY starting at AT-INDEX.
The index of the first element of ARRAY not stored into and
the new value of ARRAY are returned. (ARRAY is munged)
RADIX, which defaults to BASE, is used for conversion.
Leading spaces are used to fill up at least MIN-COLUMNS columns."
  (DECLARE (VALUES NEW-INDEX ARRAY))
  (MULTIPLE-VALUE-BIND (QUOT DIGIT)
      (TRUNCATE N RADIX)
    (IF (ZEROP QUOT)
        (DOTIMES (I (1- MIN-COLUMNS))
          (SETF (CHAR ARRAY AT-INDEX) #/SPACE)
          (INCF AT-INDEX))
      (SETQ AT-INDEX (NUMBER-INTO-ARRAY ARRAY QUOT RADIX AT-INDEX (1- MIN-COLUMNS))))
    (SETF (CHAR ARRAY AT-INDEX) (DIGIT-CHAR DIGIT))
    (VALUES (1+ AT-INDEX) ARRAY)))

;;; Add an array to the end of another
(DEFUN APPEND-TO-ARRAY (TO-ARRAY FROM-ARRAY &OPTIONAL (FROM-START 0) FROM-END)
  "Append the contents of FROM-ARRAY to TO-ARRAY, modifying the latter.
FROM-START and FROM-END specify the part of FROM-ARRAY to use."
  (OR FROM-END (SETQ FROM-END (LENGTH FROM-ARRAY)))
  (LET* ((OLD-LENGTH (FILL-POINTER TO-ARRAY))
         (NEW-LENGTH (+ OLD-LENGTH (- FROM-END FROM-START))))
    (AND (< (ARRAY-LENGTH TO-ARRAY) NEW-LENGTH) (ADJUST-ARRAY-SIZE TO-ARRAY NEW-LENGTH))
    (COPY-ARRAY-PORTION FROM-ARRAY FROM-START FROM-END TO-ARRAY OLD-LENGTH NEW-LENGTH)
    (SETF (FILL-POINTER TO-ARRAY) NEW-LENGTH)
    TO-ARRAY))
