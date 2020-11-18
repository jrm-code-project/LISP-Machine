;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Readtable:CL; Base:10 -*-

;;; This file contains documentation for microcoded functions

;(SETF (DOCUMENTATION 'CAR 'FUNCTION)
;  "")
;;arglist = (X)

;(SETF (DOCUMENTATION 'CDR 'FUNCTION)
;  "")
;;arglist = (X)

;(SETF (DOCUMENTATION 'CAAR 'FUNCTION)
;  "")
;;arglist = (X)

;(SETF (DOCUMENTATION 'CADR 'FUNCTION)
;  "")
;;arglist = (X)

;(SETF (DOCUMENTATION 'CDAR 'FUNCTION)
;  "")
;;arglist = (X)

;(SETF (DOCUMENTATION 'CDDR 'FUNCTION)
;  "")
;;arglist = (X)

;(SETF (DOCUMENTATION 'CAAAR 'FUNCTION)
;  "")
;;arglist = (X)

;(SETF (DOCUMENTATION 'CAADR 'FUNCTION)
;  "")
;;arglist = (X)

;(SETF (DOCUMENTATION 'CADAR 'FUNCTION)
;  "")
;;arglist = (X)

;(SETF (DOCUMENTATION 'CADDR 'FUNCTION)
;  "")
;;arglist = (X)

;(SETF (DOCUMENTATION 'CDAAR 'FUNCTION)
;  "")
;;arglist = (X)

;(SETF (DOCUMENTATION 'CDADR 'FUNCTION)
;  "")
;;arglist = (X)

;(SETF (DOCUMENTATION 'CDDAR 'FUNCTION)
;  "")
;;arglist = (X)

;(SETF (DOCUMENTATION 'CDDDR 'FUNCTION)
;  "")
;;arglist = (X)

;(SETF (DOCUMENTATION 'CAAAAR 'FUNCTION)
;  "")
;;arglist = (X)

;(SETF (DOCUMENTATION 'CAAADR 'FUNCTION)
;  "")
;;arglist = (X)

;(SETF (DOCUMENTATION 'CAADAR 'FUNCTION)
;  "")
;;arglist = (X)

;(SETF (DOCUMENTATION 'CAADDR 'FUNCTION)
;  "")
;;arglist = (X)

;(SETF (DOCUMENTATION 'CADAAR 'FUNCTION)
;  "")
;;arglist = (X)

;(SETF (DOCUMENTATION 'CADADR 'FUNCTION)
;  "")
;;arglist = (X)

;(SETF (DOCUMENTATION 'CADDAR 'FUNCTION)
;  "")
;;arglist = (X)

;(SETF (DOCUMENTATION 'CADDDR 'FUNCTION)
;  "")
;;arglist = (X)

;(SETF (DOCUMENTATION 'CDAAAR 'FUNCTION)
;  "")
;;arglist = (X)

;(SETF (DOCUMENTATION 'CDAADR 'FUNCTION)
;  "")
;;arglist = (X)

;(SETF (DOCUMENTATION 'CDADAR 'FUNCTION)
;  "")
;;arglist = (X)

;(SETF (DOCUMENTATION 'CDADDR 'FUNCTION)
;  "")
;;arglist = (X)

;(SETF (DOCUMENTATION 'CDDAAR 'FUNCTION)
;  "")
;;arglist = (X)

;(SETF (DOCUMENTATION 'CDDADR 'FUNCTION)
;  "")
;;arglist = (X)

;(SETF (DOCUMENTATION 'CDDDAR 'FUNCTION)
;  "")
;;arglist = (X)

;(SETF (DOCUMENTATION 'CDDDDR 'FUNCTION)
;  "")
;;arglist = (X)

;(SETF (DOCUMENTATION '%LOAD-FROM-HIGHER-CONTEXT 'FUNCTION)
;  "")
;;arglist = (ENVPTR)

;(SETF (DOCUMENTATION '%LOCATE-IN-HIGHER-CONTEXT 'FUNCTION)
;  "")
;;arglist = (ENVPTR)

;(SETF (DOCUMENTATION '%STORE-IN-HIGHER-CONTEXT 'FUNCTION)
;  "")
;;arglist = (VALUE ENVPTR)

(SETF (DOCUMENTATION '%DATA-TYPE 'FUNCTION)
  "Return the data-type field of X.
The value is a number less than 32.  The data types are all standard
and have names on the list Q-DATA-TYPES, such as DTP-SYMBOL for symbols.")
;arglist = (X)

(SETF (DOCUMENTATION '%POINTER 'FUNCTION)
  "Return the address or pointer-field of X.")
;arglist = (X)

;(SETF (DOCUMENTATION '%MAKE-REST-ARG-SAFE 'FUNCTION)
;  "")
;;arglist = NIL

;(SETF (DOCUMENTATION '%PERMIT-TAIL-RECURSION 'FUNCTION)
;  "")
;;arglist = NIL

;(SETF (DOCUMENTATION 'INTERNAL-FLOAT 'FUNCTION)
;  "")
;;arglist = (NUMBER)

(SETF (DOCUMENTATION '%MAKE-POINTER 'FUNCTION)
  "Create a lisp datum given a data type code and an address.")
;arglist = (DTP ADDRESS)

(SETF (DOCUMENTATION '%SPREAD 'FUNCTION)
  "Takes a list and pushes its elements on the stack")
;arglist = (LIST)

(SETF (DOCUMENTATION '%P-STORE-CONTENTS 'FUNCTION)
  "Store X into the memory location addressed by POINTER.
Only the data type and pointer fields of the word are changed
 (the fields which are part of the \"contents\" of the word).
POINTER's data type is ignored -- it can even be a fixnum -- so this
is dangerous if not used with care.")
;arglist = (POINTER VALUE)

(SETF (DOCUMENTATION '%LOGLDB 'FUNCTION)
  "Fixnums-only form of LDB.
No complaint about loading/clobbering the sign bit")
;arglist = (PPSS WORD)

(SETF (DOCUMENTATION '%LOGDPB 'FUNCTION)
  "Fixnums-only form of DPB.
No complaint about loading/clobbering the sign bit")
;arglist = (VALUE PPSS WORD)

(SETF (DOCUMENTATION 'LDB 'FUNCTION)
  "Load a byte specified by PPSS out of the number WORD.
PPSS is a number whose printed representation in octal, as four digits,
 contains a two-digit position-within-word and a two-digit size.
The position is the number of low bits of WORD after the desired byte.
The size must be less than 25. so that the value fits in a fixnum.")
;arglist = (PPSS WORD)

(SETF (DOCUMENTATION 'DPB 'FUNCTION)
  "Deposit VALUE into the byte PPSS of the number WORD, returning a new number.
PPSS is a number whose printed representation in octal, as four digits,
 contains a two-digit position-within-word and a two-digit size.
The position is the number of low bits of WORD after the desired byte.
The size must be less than 25. so that the value fits in a fixnum.")
;arglist = (VALUE PPSS WORD)

(SETF (DOCUMENTATION '%P-STORE-TAG-AND-POINTER 'FUNCTION)
  "Store the entire word addressed by POINTER from two numbers.
MISC-FIELDS is stored into the top 7 bits (the cdr-code and data-type)
 and POINTER-FIELD into the bottom 25.
 (These are the byte fields %%Q-ALL-BUT-POINTER and %%Q-POINTER respectively)
POINTER's data type is ignored -- it can even be a fixnum -- so this
is dangerous if not used with care.")
;arglist = (POINTER MISC-FIELDS POINTER-FIELD)

;(SETF (DOCUMENTATION 'GET 'FUNCTION)
;  "")
;;arglist = (SYMBOL PROPERTY &OPTIONAL DEFAULT)

(SETF (DOCUMENTATION 'GETL 'FUNCTION)
  "Find any of the properties in INDICATOR-LIST, on SYMBOL.
Whichever of those properties occurs first in the property list is used.
The value is a pointer to the cell in the property list that points
to the indicator.  The CADR of the value is the property's value.")
;arglist = (SYMBOL PROPERTY-NAME-LIST)

(SETF (DOCUMENTATION 'ASSQ 'FUNCTION)
  "Search association list ALIST for X.
An association list is a list of lists.  The keys are the CARs of the elements.
ALIST is searched for an element whose CAR is X.
That element is returned.  If there is none, NIL is returned.")
;arglist = (X ALIST)

(SETF (DOCUMENTATION 'LAST 'FUNCTION)
  "Return the last cons-cell in LIST.
Works by CDR'ing down LIST until it finds a cell whose CDR is not a cons.
That cell is the value.  The last element of the list is the cell's CAR.")
;arglist = (LIST)

(SETF (DOCUMENTATION 'LENGTH 'FUNCTION)
  "If LIST-OR-ARRAY is a list, returns the number of elements in LIST-OR-ARRAY
If LIST-OR-ARRAY is an array, returns the active length of the array,
which is the value of the fill-pointer, if any, or else the number of elements
in the array.")
;arglist = (LIST-OR-ARRAY)

(SETF (DOCUMENTATION '1+ 'FUNCTION)
  "Returns N plus one.  N can be any type of number.")
;arglist = (N)

(SETF (DOCUMENTATION '1- 'FUNCTION)
  "Returns N minus one.  N can be any type of number.")
;arglist = (N)

(SETF (DOCUMENTATION 'RPLACA 'FUNCTION)
  "Modifies the CAR of CONS to contain X.  Returns CONS.")
;arglist = (CONS NEW-CAR)

(SETF (DOCUMENTATION 'RPLACD 'FUNCTION)
  "Modifies the CDR of CONS to contain X.  Returns CONS.")
;arglist = (CONS NEW-CDR)

(SETF (DOCUMENTATION 'ZEROP 'FUNCTION)
  "T if NUMBER is zero.  Error if NUMBER is not a number.")
;arglist = (NUMBER)

(SETF (DOCUMENTATION 'SET 'FUNCTION)
  "Modifies the value of SYMBOL to be X.
\(SET 'FOO 'BAR) changes the value of the symbol FOO.
May not be used to change local variables in compiled code.")
;arglist = (SYMBOL VALUE)

(SETF (DOCUMENTATION 'INTEGERP 'FUNCTION)
  "T if X is an integer; NIL for other numbers and non-numbers.")
;arglist = (X)

(SETF (DOCUMENTATION 'FIXP 'FUNCTION)
  "T if X is an integer; NIL for other numbers and non-numbers.
INTEGERP is a more modern name for this function.")
;arglist = (X)

(SETF (DOCUMENTATION 'FLOATP 'FUNCTION)
  "T if X is a floating point number of any size.  Never gets an error.")
;arglist = (X)

(SETF (DOCUMENTATION 'EQUAL 'FUNCTION)
  "T if X and Y are EQ, or if they are lists whose elements are EQUAL.
Numbers are compared with EQL, so the answer is T if they have the same type and value.
Strings are compared by their contents, using STRING=.
Other kinds of arrays, however, are compared with EQ.")
;arglist = (X Y)

;(SETF (DOCUMENTATION '%SET-SELF-MAPPING-TABLE 'FUNCTION)
;  "")
;;arglist = (MAPPING-TABLE)

;(SETF (DOCUMENTATION 'PDL-WORD 'FUNCTION)
;  "")
;;arglist = (N)

(SETF (DOCUMENTATION 'FALSE 'FUNCTION)
  "Returns NIL.  Allows no arguments.")
;arglist = NIL

(SETF (DOCUMENTATION 'TRUE 'FUNCTION)
  "Returns T.  Allows no arguments.")
;arglist = NIL

(SETF (DOCUMENTATION 'NOT 'FUNCTION)
  "Returns T if X is NIL.")
;arglist = (X)

(SETF (DOCUMENTATION 'NULL 'FUNCTION)
  "Returns T if X is NIL.")
;arglist = (X)

(SETF (DOCUMENTATION 'ATOM 'FUNCTION)
  "Returns T if X is not a CONS.")
;arglist = (X)

(SETF (DOCUMENTATION 'ODDP 'FUNCTION)
  "T is NUMBER is odd.")
;arglist = (NUMBER)

(SETF (DOCUMENTATION 'EVENP 'FUNCTION)
  "T if NUMBER is even.")
;arglist = (NUMBER)

(SETF (DOCUMENTATION '%HALT 'FUNCTION)
  "Halts the processor.")
;arglist = NIL

(SETF (DOCUMENTATION 'GET-PNAME 'FUNCTION)
  "Returns the pname-string of SYMBOL. Older name for SYMBOL-NAME.")
;arglist = (SYMBOL)

(SETF (DOCUMENTATION 'SYMBOL-NAME 'FUNCTION)
  "Returns the pname-string of SYMBOL.")
;arglist = (SYMBOL)

(SETF (DOCUMENTATION 'LSH 'FUNCTION)
  "Logical shift N by NBITS.  Sign controls direction of shift.  N must be a fixnum.")
;arglist = (N NBITS)

(SETF (DOCUMENTATION 'ROT 'FUNCTION)
  "Rotate the 25. bits of the fixnum N by NBITS.")
;arglist = (N NBITS)

(SETF (DOCUMENTATION '*BOOLE 'FUNCTION)
  "Internal primitive version of BOOLE for the case of exactly two arguments and an alu (FN).
You should never need to use this, as the compiler optimizes into this as appropriate")
;arglist = (FN ARG1 ARG2)

(SETF (DOCUMENTATION 'NUMBERP 'FUNCTION)
  "Returns T if X is a number.")
;arglist = (X)

(SETF (DOCUMENTATION 'PLUSP 'FUNCTION)
  "Returns T if NUMBER is greater than zero.
Error if NUMBER is not a real number.")
;arglist = (NUMBER)

(SETF (DOCUMENTATION 'MINUSP 'FUNCTION)
  "Returns T if NUMBER is less than zero.
Error if NUMBER is not a real number.")
;arglist = (NUMBER)

(SETF (DOCUMENTATION '\\ 'FUNCTION)
  "Return the remainder of X divided by Y.")
;arglist = (X Y)

(SETF (DOCUMENTATION 'MINUS 'FUNCTION)
  "Returns zero minus NUMBER.")
;arglist = (NUMBER)

;(SETF (DOCUMENTATION '%SXHASH-STRING 'FUNCTION)
;  "")
;;arglist = (STRING CHARACTER-MASK)

(SETF (DOCUMENTATION 'VALUE-CELL-LOCATION 'FUNCTION)
  "Returns a locative to the cell which holds SYMBOL's value.
This ignores such things as local variables of compiled code;
it always returns a pointer to the value-cell word inside SYMBOL.")
;arglist = (SYMBOL)

(SETF (DOCUMENTATION 'FUNCTION-CELL-LOCATION 'FUNCTION)
  "Returns a locative to the cell inside SYMBOL that holds its function definition.")
;arglist = (SYMBOL)

(SETF (DOCUMENTATION 'PROPERTY-CELL-LOCATION 'FUNCTION)
  "Returns a locative to the cell inside SYMBOL that holds its property list.")
;arglist = (SYMBOL)

(SETF (DOCUMENTATION 'NCONS 'FUNCTION)
  "Returns (CONS X NIL).")
;arglist = (CAR)

(SETF (DOCUMENTATION 'NCONS-IN-AREA 'FUNCTION)
  "Returns (CONS-IN-AREA X NIL AREA).")
;arglist = (CAR AREA)

(SETF (DOCUMENTATION 'CONS 'FUNCTION)
  "Returns a newly allocated CONS whose CAR is X and CDR is Y.")
;arglist = (CAR CDR)

(SETF (DOCUMENTATION 'CONS-IN-AREA 'FUNCTION)
  "Returns a newly allocated CONS in area AREA whose CAR is X and CDR is Y.
AREA is an area number, such as WORKING-STORAGE-AREA.")
;arglist = (CAR CDR AREA)

(SETF (DOCUMENTATION 'XCONS 'FUNCTION)
  "Returns (CONS Y X).")
;arglist = (CDR CAR)

(SETF (DOCUMENTATION 'XCONS-IN-AREA 'FUNCTION)
  "Returns (CONS-IN-AREA Y X AREA).")
;arglist = (CDR CAR AREA)

;(SETF (DOCUMENTATION '%SPREAD-N 'FUNCTION)
;  "")
;;arglist = (LIST N)

(SETF (DOCUMENTATION 'SYMEVAL 'FUNCTION)
  "Returns the contents of the value cell of SYMBOL.
This is the value that would be obtained by use of SYMBOL as a special variable.")
;arglist = (SYMBOL)

(SETF (DOCUMENTATION 'SYMBOL-VALUE 'FUNCTION)
  "Returns the contents of the value cell of SYMBOL.
This is the value that would be obtained by use of SYMBOL as a special variable.")
;arglist = (SYMBOL)

;(SETF (DOCUMENTATION 'POP-M-FROM-UNDER-N 'FUNCTION)
;  "")
;;arglist = (NUM-POPS NUM-TO-KEEP)

;(SETF (DOCUMENTATION 'GET-LEXICAL-VALUE-CELL 'FUNCTION)
;  "")
;;arglist = (ENV-LIST SYMBOL-CELL-LOCATION)

;(SETF (DOCUMENTATION '%CALL-MULT-VALUE 'FUNCTION)
;  "")
;;arglist = NIL

;(SETF (DOCUMENTATION '%CALL0-MULT-VALUE 'FUNCTION)
;  "")
;;arglist = NIL

;(SETF (DOCUMENTATION '%RETURN-2 'FUNCTION)
;  "")
;;arglist = NIL

;(SETF (DOCUMENTATION '%RETURN-3 'FUNCTION)
;  "")
;;arglist = NIL

;(SETF (DOCUMENTATION '%RETURN-N 'FUNCTION)
;  "")
;;arglist = NIL

(SETF (DOCUMENTATION 'RETURN-NEXT-VALUE 'FUNCTION)
  "In a PROG, provide X one additional value for the caller.
If the caller has not asked for multiple values, returns from the PROG.
If the caller has as many values as he has asked for, with X together
with any values you have returned in this fashion so far, this returns
to the caller.  Otherwise, it continues with the PROG.

Works only in compiled code.")
;arglist = (VALUE)

(SETF (DOCUMENTATION 'RETURN-LIST 'FUNCTION)
  "Return the elements of VALUES, as multiple values, from a PROG.")
;arglist = (VALUES)

;(SETF (DOCUMENTATION 'UNBIND-TO-INDEX-UNDER-N 'FUNCTION)
;  "")
;;arglist = (N)

(SETF (DOCUMENTATION '%BIND 'FUNCTION)
  "Bind any location to a specified value.
Adds the binding to the current stack-frame.  Only works in compiled code.
This allows you to bind cells other than value cells and to do conditional
binding.")
;arglist = (POINTER VALUE)

(SETF (DOCUMENTATION 'MEMQ 'FUNCTION)
  "Returns the first link in LIST whose CAR is EQ to X, or else NIL.")
;arglist = (X LIST)

(SETF (DOCUMENTATION 'INTERNAL-< 'FUNCTION)
  "Internal primitive two argument form of <.
You should never need to use this, as the compiler optimizes into this as appropriate")
;arglist = (NUM1 NUM2)

(SETF (DOCUMENTATION 'INTERNAL-> 'FUNCTION)
  "Internal primitive two argument form of >.
You should never need to use this, as the compiler optimizes into this as appropriate")
;arglist = (NUM1 NUM2)

(SETF (DOCUMENTATION 'INTERNAL-= 'FUNCTION)
  "Internal primitive two argument form of -=.
You should never need to use this, as the compiler optimizes into this as appropriate")
;arglist = (NUM1 NUM2)

(SETF (DOCUMENTATION 'INTERNAL-CHAR-EQUAL 'FUNCTION)
  "Internal primitive two argument form of CHAR-EQUAL.
You should never need to use this, as the compiler optimizes into this as appropriate")
;arglist = (CH1 CH2)

(SETF (DOCUMENTATION '%STRING-SEARCH-CHAR 'FUNCTION)
  "The same as STRING-SEARCH-CHAR, but without coercion and error checking.
Also, all the args are required.  And it's faster.")
;arglist = (CHAR STRING START END)

(SETF (DOCUMENTATION '%STRING-EQUAL 'FUNCTION)
  "T if COUNT characters of STRING1 at INDEX1 match those of STRING2 at INDEX2.
Similar to STRING-EQUAL, but args are slightly different and all required -- and it's faster.
The comparison ignores case unless ALPHABETIC-CASE-AFFECTS-STRING-COMPARISON is non-NIL.")
;arglist = (STRING1 INDEX1 STRING2 INDEX2 COUNT)

(SETF (DOCUMENTATION 'NTH 'FUNCTION)
  "Returns the N'th element of LIST.
Counting starts from 0, so element 1 is the CADR.")
;arglist = (N LIST)

(SETF (DOCUMENTATION 'NTHCDR 'FUNCTION)
  "Discards N elements from LIST; performs CDR N times.")
;arglist = (N LIST)

(SETF (DOCUMENTATION '*PLUS 'FUNCTION)
  "Internal primitive two argument form of +.
You should never need to use this, as the compiler optimizes into this as appropriate")
;arglist = (NUM1 NUM2)

(SETF (DOCUMENTATION '*DIF 'FUNCTION)
  "Internal primitive two argument form of DIFFERENCE.
You should never need to use this, as the compiler optimizes into this as appropriate")
;arglist = (NUM1 NUM2)

(SETF (DOCUMENTATION '*TIMES 'FUNCTION)
  "Internal primitive two argument form of *.
You should never need to use this, as the compiler optimizes into this as appropriate")
;;arglist = (NUM1 NUM2)

(SETF (DOCUMENTATION '*QUO 'FUNCTION)
  "Internal primitive two argument form of ZL:/.
You should never need to use this, as the compiler optimizes into this as appropriate")
;;arglist = (NUM1 NUM2)

(SETF (DOCUMENTATION '*LOGAND 'FUNCTION)
  "Internal primitive two argument form of LOGAND.
You should never need to use this, as the compiler optimizes into this as appropriate")
;arglist = (NUM1 NUM2)

(SETF (DOCUMENTATION '*LOGXOR 'FUNCTION)
  "Internal primitive two argument form of LOGXOR.
You should never need to use this, as the compiler optimizes into this as appropriate")
;arglist = (NUM1 NUM2)

(SETF (DOCUMENTATION '*LOGIOR 'FUNCTION)
  "Internal primitive two argument form of LOGIOR.
You should never need to use this, as the compiler optimizes into this as appropriate")
;arglist = (NUM1 NUM2)

(SETF (DOCUMENTATION 'ARRAY-LEADER 'FUNCTION)
  "Returns the contents of leader slot INDEX of ARRAY.")
;arglist = (ARRAY INDEX)

(SETF (DOCUMENTATION 'STORE-ARRAY-LEADER 'FUNCTION)
  "Stores X into leader slot INDEX of ARRAY.
This function is semi-obsolete: Use SETF of ARRAY-LEADER instead.")
;arglist = (VALUE ARRAY INDEX)

;(SETF (DOCUMENTATION 'GET-LIST-POINTER-INTO-ARRAY 'FUNCTION)
;  "")
;;arglist = (ARRAY)

(SETF (DOCUMENTATION 'ARRAY-PUSH 'FUNCTION)
  "Add X as an element at the end of ARRAY.
The fill pointer (leader element 0) is the index of the next element to
be added.  Returns NIL and does no add the element if the array is full;
use VECTOR-PUSH-EXTEND instead if you want the array to grow automatically.")
;arglist = (ARRAY VALUE)

;(SETF (DOCUMENTATION 'APPLY 'FUNCTION)
;  "")
;;arglist = (FN ARGS)

(SETF (DOCUMENTATION '%MAKE-LIST 'FUNCTION)
  "Construct a cdr-coded list of objects of the specified length")
;arglist = (INITIAL-VALUE AREA LENGTH)

;; now in macrocode
;(SETF (DOCUMENTATION 'LIST 'FUNCTION)
;  "Return a list whose elements are the arguments.")
;;arglist = (&REST ELEMENTS)

;(SETF (DOCUMENTATION 'LIST* 'FUNCTION)
;  "Return a list whose elements are the arguments, and whose tail is the last argument.
;\(LIST* 'A 'B '(C D)) returns a list (A B C D).")
;;arglist = (FIRST &REST ELEMENTS)

;(SETF (DOCUMENTATION 'LIST-IN-AREA 'FUNCTION)
;  "Returns a list of ELEMENTS, constructed in area AREA.")
;;arglist = (AREA &REST ELEMENTS)

;(SETF (DOCUMENTATION 'LIST*-IN-AREA 'FUNCTION)
;  "Returns a LIST* of ELEMENTS, constructed in area AREA.")
;;arglist = (AREA FIRST &REST ELEMENTS)

(SETF (DOCUMENTATION 'LOCATE-IN-INSTANCE 'FUNCTION)
  "Returns a locative to the slot in INSTANCE for instance variable SYMBOL.")
;arglist = (INSTANCE SYMBOL)

(SETF (DOCUMENTATION '%P-CDR-CODE 'FUNCTION)
  "Returns the cdr-code value of the word addressed by POINTER.
This is a number from 0 to 3.  The values have standard names
 which are CDR-NEXT, CDR-NIL, CDR-NORMAL and CDR-ERROR.
POINTER's data type is ignored -- it can even be a fixnum.")
;arglist = (POINTER)

(SETF (DOCUMENTATION '%P-DATA-TYPE 'FUNCTION)
  "Returns the data type field of the word addressed by POINTER.
This is similar to (%DATA-TYPE (CAR (%MAKE-POINTER DTP-LIST POINTER))),
except that if the word contains an illegal data type or a forwarding pointer,
this function returns the illegal data type or the data type of the forwarding pointer.
POINTER's data type is ignored -- it can even be a fixnum.")
;arglist = (POINTER)

(SETF (DOCUMENTATION '%P-POINTER 'FUNCTION)
  "Returns the pointer field of the word addressed by POINTER.
This is similar to (%POINTER (CAR (%MAKE-POINTER DTP-LIST POINTER))),
except that if the word contains an illegal data type or a forwarding pointer,
this function returns address field actually in the memory location
rather than getting an error or forwarding.
POINTER's data type is ignored -- it can even be a fixnum.")
;arglist = (POINTER)

;(SETF (DOCUMENTATION '%PAGE-TRACE 'FUNCTION)
;  "")
;;arglist = (TABLE)

;(SETF (DOCUMENTATION 'THROW-N 'FUNCTION)
;  "")
;;arglist = (TAG &REST VALUES-AND-COUNT)

(SETF (DOCUMENTATION '%P-STORE-CDR-CODE 'FUNCTION)
  "Store CDR-CODE into the cdr-code field of the word addressed by POINTER.
CDR-CODE is a number from 0 to 3.
POINTER's data type is ignored -- it can even be a fixnum -- so this
can be dangerous unless used with care.")
;arglist = (POINTER CDR-CODE)

(SETF (DOCUMENTATION '%P-STORE-DATA-TYPE 'FUNCTION)
  "Store DATA-TYPE into the data-type field of the word addressed by POINTER.
DATA-TYPE is a value such as %DATA-TYPE might return.
POINTER's data type is ignored -- it can even be a fixnum -- so this
can be dangerous unless used with care.")
;arglist = (POINTER DATA-TYPE)

(SETF (DOCUMENTATION '%P-STORE-POINTER 'FUNCTION)
  "Store POINTER-TO-STORE into the data-type field of the word addressed by POINTER.
POINTER's data type is ignored -- it can even be a fixnum -- so this
can be dangerous unless used with care.")
;arglist = (POINTER POINTER-TO-STORE)

(SETF (DOCUMENTATION 'FLOAT-EXPONENT 'FUNCTION)
  "Return as an integer the exponent of a floating point number.")
;arglist = (FLONUM)

(SETF (DOCUMENTATION 'FLOAT-FRACTION 'FUNCTION)
  "Return FLONUM modified to contain 0 as its exponent.
The result is either zero or has absolute value 1/2  abs value < 1.")
;arglist = (FLONUM)

(SETF (DOCUMENTATION 'SCALE-FLOAT 'FUNCTION)
  "Return a flonum like FLONUM but with INTEGER added to its exponent.")
;arglist = (FLONUM INTEGER)

;(SETF (DOCUMENTATION '%CATCH-OPEN 'FUNCTION)
;  "")
;;arglist = NIL

;(SETF (DOCUMENTATION '%CATCH-OPEN-MV 'FUNCTION)
;  "")
;;arglist = NIL

;(SETF (DOCUMENTATION 'INTERNAL-FLOOR-1 'FUNCTION)
;  "")
;;arglist = (DIVIDEND DIVISOR)

(SETF (DOCUMENTATION '%DIV 'FUNCTION)
  "Divide X by Y, returning a rational number if args are integers.")
;arglist = (DIVIDEND DIVISOR)

;(SETF (DOCUMENTATION '%FEXPR-CALL 'FUNCTION)
;  "")
;;arglist = NIL

;(SETF (DOCUMENTATION '%FEXPR-CALL-MV 'FUNCTION)
;  "")
;;arglist = NIL

;(SETF (DOCUMENTATION '%FEXPR-CALL-MV-LIST 'FUNCTION)
;  "")
;;arglist = NIL

;(SETF (DOCUMENTATION '%CATCH-OPEN-MV-LIST 'FUNCTION)
;  "")
;;arglist = NIL

(SETF (DOCUMENTATION '*CATCH 'FUNCTION)
  "Set up a tag TAG that a *THROW can throw to.
If a *THROW with argument EQ to TAG is executed dynamically within FORMS,
it returns immediately from the *CATCH, skipping the rest of the execution of FORMS.
The second argument of *THROW is returned from the *CATCH.")
;arglist = (TAG &REST FORMS)

(SETF (DOCUMENTATION '%BLT 'FUNCTION)
  "Copy a block of memory, a word at a time, with no decoding, for untyped data.
Use %BLT-TYPED for words which contain Lisp data types.
The first word is copied from FROM-ADDRESS to TO-ADDRESS.
INCREMENT is added to each address and then another word is copied, and so on.
COUNT is the number of words to copy.")
;arglist = (FROM-ADDRESS TO-ADDRESS COUNT INCREMENT)

(SETF (DOCUMENTATION '*THROW 'FUNCTION)
  "Return VALUE immediately from the innermost *CATCH that handles TAG.")
;arglist = (TAG VALUE)

;(SETF (DOCUMENTATION '%XBUS-WRITE-SYNC 'FUNCTION)
;  "")
;;arglist = (IO-ADDR WORD DELAY SYNC-LOC SYNC-MASK SYNC-VAL)

(SETF (DOCUMENTATION '%P-LDB 'FUNCTION)
  "Return the contents of byte PPSS in the word addressed by POINTER.
This byte can include any of the bits in the word, and can overlap
between the various fields normally used by Lisp.
But it may not be more than 23. bits long.
POINTER's data type is ignored -- it can even be a fixnum.")
;arglist = (PPSS POINTER)

(SETF (DOCUMENTATION '%P-DPB 'FUNCTION)
  "Store VALUE into byte PPSS in the word addressed by POINTER.
This byte can include any of the bits in the word, and can overlap
between the various fields normally used by Lisp.
But it may not be more than 23. bits long.
POINTER's data type is ignored -- it can even be a fixnum -- so this
can be dangerous unless used with care.")
;arglist = (VALUE PPSS POINTER)

(SETF (DOCUMENTATION 'MASK-FIELD 'FUNCTION)
  "Return a number which is FIXNUM with all but the byte PPSS replaced by zero.")
;arglist = (PPSS FIXNUM)

(SETF (DOCUMENTATION '%P-MASK-FIELD 'FUNCTION)
  "Return (MASK-FIELD PPSS (%P-POINTER POINTER)).")
;arglist = (PPSS POINTER)

(SETF (DOCUMENTATION 'DEPOSIT-FIELD 'FUNCTION)
  "Return a number which in the byte PPSS matches VALUE, and the rest matches FIXNUM.")
;arglist = (VALUE PPSS FIXNUM)

(SETF (DOCUMENTATION '%P-DEPOSIT-FIELD 'FUNCTION)
  "Stores into the byte PPSS of the word addressed by POINTER from teh same byte of VALUE.
This byte can include any of the bits in the word, and can overlap
between the various fields normally used by Lisp.
For example, part of VALUE's data type field may be included.
POINTER's data type is ignored -- it can even be a fixnum -- so this
can be dangerous unless used with care.")
;arglist = (VALUE PPSS POINTER)

(SETF (DOCUMENTATION 'COPY-ARRAY-CONTENTS 'FUNCTION)
  "Copy all the elements of the array FROM into TO.
If TO is longer than FROM, it is filled out with zeros (if numeric array) or NILs.
If either array is multidimensional, its elements are used in the order
they are stored in memory.")
;arglist = (FROM TO)

(SETF (DOCUMENTATION 'COPY-ARRAY-CONTENTS-AND-LEADER 'FUNCTION)
  "Copy all the elements and leader slots of the array FROM into TO.
If TO is longer than FROM, it is filled out with zeros (if numeric array) or NILs.
If either array is multidimensional, its elements are used in the order
they are stored in memory.")
;arglist = (FROM TO)

;(SETF (DOCUMENTATION '%FUNCTION-INSIDE-SELF 'FUNCTION)
;  "")
;;arglist = NIL

(SETF (DOCUMENTATION 'ARRAY-HAS-LEADER-P 'FUNCTION)
  "Returns T if ARRAY has a leader.")
;arglist = (ARRAY)

(SETF (DOCUMENTATION 'COPY-ARRAY-PORTION 'FUNCTION)
  "Copies specified elements of FROM-ARRAY into TO-ARRAY.
FROM-START and FROM-END are indices in FROM-ARRAY indicating the portion to copy.
TO-START and TO-END are indices in TO-ARRAY.  If the specified portion of
TO-ARRAY is longer, it is filled out with zeros (if TO-ARRAY is a numeric array) or NILs.
If either array is multidimensional, its elements are used in the order
they are stored in memory.")
;arglist = (FROM-ARRAY FROM-START FROM-END TO-ARRAY TO-START TO-END)

(SETF (DOCUMENTATION 'FIND-POSITION-IN-LIST 'FUNCTION)
  "Returns a number N such that (NTH N LIST) is EQ to X.
The value is NIL if X is not an element of LIST.")
;arglist = (ELEMENT LIST)

;(SETF (DOCUMENTATION '%GET-SELF-MAPPING-TABLE 'FUNCTION)
;  "")
;;arglist = (METHOD-FLAVOR-NAME)

(SETF (DOCUMENTATION 'G-L-P 'FUNCTION)
  "Return a list overlayed with the contents of ARRAY.
ARRAY must be an array of type ART-Q-LIST.")
;arglist = (ARRAY)

;(SETF (DOCUMENTATION 'INTERNAL-FLOOR-2 'FUNCTION)
;  "")
;;arglist = (DIVIDEND DIVISOR)

(SETF (DOCUMENTATION 'EQL 'FUNCTION)
  "Like = when both arguments are numbers; like EQ otherwise.")
;arglist = (X Y)

(SETF (DOCUMENTATION 'AR-1 'FUNCTION)
  "Internal primitive version of AREF for 1-dimensional arrays.
You should never need to use this, as the compiler optimizes into this as appropriate")
;arglist = (ARRAY SUB)

(SETF (DOCUMENTATION 'AR-2 'FUNCTION)
  "Internal primitive version of AREF for 2-dimensional arrays.
You should never need to use this, as the compiler optimizes into this as appropriate")
;arglist = (ARRAY SUB1 SUB2)

(SETF (DOCUMENTATION 'AR-3 'FUNCTION)
  "Internal primitive version of AREF for 3-dimensional arrays.
You should never need to use this, as the compiler optimizes into this as appropriate")
;arglist = (ARRAY SUB1 SUB2 SUB3)

(SETF (DOCUMENTATION 'AS-1 'FUNCTION)
  "Internal primitive version of ASET for 1-dimensional arrays.
You should never need to use this, as the compiler optimizes into this as appropriate")
;arglist = (VALUE ARRAY SUB)

(SETF (DOCUMENTATION 'AS-2 'FUNCTION)
  "Internal primitive version of AREF for 2-dimensional arrays.
You should never need to use this, as the compiler optimizes into this as appropriate")
;arglist = (VALUE ARRAY SUB1 SUB2)

(SETF (DOCUMENTATION 'AS-3 'FUNCTION)
  "Internal primitive version of AREF for 3-dimensional arrays.
You should never need to use this, as the compiler optimizes into this as appropriate")
;arglist = (VALUE ARRAY SUB1 SUB2 SUB3)

(SETF (DOCUMENTATION '%INSTANCE-REF 'FUNCTION)
  "Return contents of slot INDEX in INSTANCE.  1 is the lowest valid index.")
;arglist = (INSTANCE INDEX)

(SETF (DOCUMENTATION '%INSTANCE-LOC 'FUNCTION)
  "Return location of slot INDEX in INSTANCE.  1 is the lowest valid index.")
;arglist = (INSTANCE INDEX)

(SETF (DOCUMENTATION '%INSTANCE-SET 'FUNCTION)
  "Set contents of slot INDEX in INSTANCE.  1 is the lowest valid index.")
;arglist = (VAL INSTANCE INDEX)

;(SETF (DOCUMENTATION '%BINDING-INSTANCES 'FUNCTION)
;  "")
;;arglist = (LIST-OF-SYMBOLS)

;(SETF (DOCUMENTATION '%EXTERNAL-VALUE-CELL 'FUNCTION)
;  "")
;;arglist = (SYMBOL)

;(SETF (DOCUMENTATION '%USING-BINDING-INSTANCES 'FUNCTION)
;  "")
;;arglist = (BINDING-INSTANCES)

;(SETF (DOCUMENTATION '%GC-CONS-WORK 'FUNCTION)
;  "")
;;arglist = (NQS)

(SETF (DOCUMENTATION '%P-CONTENTS-OFFSET 'FUNCTION)
  "Returns the contents of the word OFFSET beyond that addressed by POINTER.
This is not the same as (CAR (%MAKE-POINTER-OFFSET ... POINTER OFFSET))
because it checks for a forwarding pointer in the word addressed by POINTER.
The idea is that POINTER points at the beginning of a structure
and OFFSET is an offset within it.")
;arglist = (POINTER OFFSET)

(SETF (DOCUMENTATION '%DISK-RESTORE 'FUNCTION)
  "Restore a world load partition.
The two args make a 32-bit number that is interpreted as 4 characters, the partition name.
Zero means the current band is used.")
;arglist = (PARTITION-HIGH-16-BITS LOW-16-BITS)

(SETF (DOCUMENTATION '%DISK-SAVE 'FUNCTION)
  "Save current memory contents in a world load partition.
The last two args make a 32-bit number that is interpreted as 4 characters,
 the partition name.  Zero means the current band is used.")
;arglist = (MAIN-MEMORY-SIZE PARTITION-HIGH-16-BITS LOW-16-BITS)

;(SETF (DOCUMENTATION '%ARGS-INFO 'FUNCTION)
;  "")
;;arglist = (FUNCTION)

(SETF (DOCUMENTATION '%OPEN-CALL-BLOCK 'FUNCTION)
  "Push a call-block on the stack, for function FUNCTION.
ADI-PAIRS is the number of two-word adi units you have already pushed.
DESTINATION is a numeric destination code, 0 through 7,
which stands for either D-RETURN or D-NEXT or D-LAST or D-IGNORE.
This works only in compiled code.")
;arglist = (FUNCTION ADI-PAIRS DESTINATION)

(SETF (DOCUMENTATION '%PUSH 'FUNCTION)
  "Push X onto the stack.  Useful with %OPEN-CALL-BLOCK.
You must make sure you have room on the stack, with %ASSURE-PDL-ROOM,
before you push words with %PUSH.")
;arglist = (X)

(SETF (DOCUMENTATION '%ACTIVATE-OPEN-CALL-BLOCK 'FUNCTION)
  "Actually call the function in a call block made you with %OPEN-CALL-BLOCK.
This is done after pushing the arguments with %PUSH.")
;arglist = NIL

;(SETF (DOCUMENTATION '%ASSURE-PDL-ROOM 'FUNCTION)
;  "")
;;arglist = (ROOM)

(SETF (DOCUMENTATION 'STACK-GROUP-RETURN 'FUNCTION)
  "Resume the stack group which invoked this one, with X as argument.
Does not change which stack group is recorded as that one's resumer.")
;arglist = (X)

(SETF (DOCUMENTATION 'AS-2-REVERSE 'FUNCTION)
  "Store VALUE in ARRAY, optionally reversing the indices.
While arrays are stored with first index varying fastest,
this is the same as ASET.  When arrays are stored with last index varying fastest,
this uses INDEX1 as the first index even though it is the last argument.")
;arglist = (VALUE ARRAY INDEX2 INDEX1)

;(SETF (DOCUMENTATION '%MAKE-STACK-LIST 'FUNCTION)
;  "")
;;arglist = (N)

(SETF (DOCUMENTATION 'STACK-GROUP-RESUME 'FUNCTION)
  "Resume stack group SG with X as argument.")
;arglist = (SG X)

;(SETF (DOCUMENTATION '%CALL-MULT-VALUE-LIST 'FUNCTION)
;  "")
;;arglist = NIL

;(SETF (DOCUMENTATION '%CALL0-MULT-VALUE-LIST 'FUNCTION)
;  "")
;;arglist = NIL

;(SETF (DOCUMENTATION '%GC-SCAV-RESET 'FUNCTION)
;  "")
;;arglist = (REGION)

(SETF (DOCUMENTATION '%P-STORE-CONTENTS-OFFSET 'FUNCTION)
  "Store X in contents of word OFFSET beyond that addressed by POINTER.
This is not the same as (%P-STORE-CONTENTS (%MAKE-POINTER-OFFSET ... POINTER OFFSET))
because it checks for a forwarding pointer in the word addressed by POINTER.
The idea is that POINTER points at the beginning of a structure
and OFFSET is an offset within it.")
;arglist = (VALUE POINTER OFFSET)

;(SETF (DOCUMENTATION '%GC-FREE-REGION 'FUNCTION)
;  "")
;;arglist = (REGION)

;(SETF (DOCUMENTATION '%GC-FLIP 'FUNCTION)
;  "")
;;arglist = (REGION)

(SETF (DOCUMENTATION 'ARRAY-LENGTH 'FUNCTION)
  "Returns the number of elements in ARRAY.
This does not take account of the fill pointer.")
;arglist = (ARRAY)

(SETF (DOCUMENTATION 'ARRAY-TOTAL-SIZE 'FUNCTION)
  "Returns the number of elements in ARRAY, or the fill pointer if there is one.")
;arglist = (ARRAY)

(SETF (DOCUMENTATION 'ARRAY-ACTIVE-LENGTH 'FUNCTION)
  "Returns the number of elements in ARRAY, or the fill pointer if there is one.")
;arglist = (ARRAY)

;(SETF (DOCUMENTATION '%COMPUTE-PAGE-HASH 'FUNCTION)
;  "")
;;arglist = (ADDR)

;(SETF (DOCUMENTATION 'THROW-SPREAD 'FUNCTION)
;  "")
;;arglist = (TAG VALUE-LIST)

(SETF (DOCUMENTATION '%UNIBUS-READ 'FUNCTION)
  "Returns the 16-bit value read from the unibus at UNIBUS-ADDR.")
;arglist = (UNIBUS-ADDR)

(SETF (DOCUMENTATION '%UNIBUS-WRITE 'FUNCTION)
  "Writes WORD into the 16-bit unibus location at UNIBUS-ADDR.")
;arglist = (UNIBUS-ADDR WORD)

;(SETF (DOCUMENTATION '%GC-SCAVENGE 'FUNCTION)
;  "")
;;arglist = (WORK)

;(SETF (DOCUMENTATION '%CHAOS-WAKEUP 'FUNCTION)
;  "")
;;arglist = NIL

(SETF (DOCUMENTATION '%AREA-NUMBER 'FUNCTION)
  "Returns the area number of the area the pointer X points into.")
;arglist = (X)

;(SETF (DOCUMENTATION '*MAX 'FUNCTION)
;  "")
;;arglist = (NUM1 NUM2)

;(SETF (DOCUMENTATION '*MIN 'FUNCTION)
;  "")
;;arglist = (NUM1 NUM2)

(SETF (DOCUMENTATION 'CLOSURE 'FUNCTION)
  "Returns a closure, closing FUNCTION over the variables in SYMBOL-LIST.
The closure is a function which when called will perform FUNCTION
in an environment in which those variables have the same bindings they have now.
Only special variables may be closed over.")
;arglist = (SYMBOL-LIST FUNCTION)

(SETF (DOCUMENTATION 'AR-2-REVERSE 'FUNCTION)
  "Return an element of ARRAY, optionally reversing the indices.
While arrays are stored with first index varying fastest,
this is the same as AREF.  When arrays are stored with last index varying fastest,
this uses INDEX1 as the first index even though it is the last argument.")
;arglist = (ARRAY INDEX2 INDEX1)

(setf (documentation 'cli:listp 'function)
      "Returns T if OBJECT is a list, including NIL. Compare with ZL:LISTP.")
;arglist = (OBJECT)

(setf (documentation 'zl:listp 'function)
      "Returns T if X is a cons (a non-null list).  Compare with CL:LISTP.
Most uses of this function should be changed to CONSP.")
;arglist = (X)

(SETF (DOCUMENTATION 'NLISTP 'FUNCTION)
  "T if X is not a list.  Currently (NLISTP NIL) is T, but it will change.")
;arglist = (X)

(SETF (DOCUMENTATION 'SYMBOLP 'FUNCTION)
  "T if X is a symbol.")
;arglist = (X)

(SETF (DOCUMENTATION 'NSYMBOLP 'FUNCTION)
  "T if X is not a symbol.")
;arglist = (X)

(SETF (DOCUMENTATION 'ARRAYP 'FUNCTION)
  "T if X is an array.")
;arglist = (X)

(SETF (DOCUMENTATION 'FBOUNDP 'FUNCTION)
  "T if SYMBOL has a function definition.")
;arglist = (SYMBOL)

(SETF (DOCUMENTATION 'STRINGP 'FUNCTION)
  "T if X is a string.")
;arglist = (X)

(SETF (DOCUMENTATION 'BOUNDP 'FUNCTION)
  "T if SYMBOL has a value, as a special variable.")
;arglist = (SYMBOL)

(SETF (DOCUMENTATION 'INTERNAL-\\\\ 'FUNCTION)
  "Internal two-argument function used by GCD (\\\\).
You should never need to use this, as the compiler optimizes into this as appropriate")
;arglist = (NUM1 NUM2)

(SETF (DOCUMENTATION 'FSYMEVAL 'FUNCTION)
  "Returns the function definition of SYMBOL.")
;arglist = (SYMBOL)

(SETF (DOCUMENTATION 'SYMBOL-FUNCTION 'FUNCTION)
  "Returns the function definition of SYMBOL.")
;arglist = (SYMBOL)

(SETF (DOCUMENTATION 'AP-1 'FUNCTION)
  "Internal primitive version of ALOC for 1-dimensional arrays.
You should never need to use this, as the compiler optimizes into this as appropriate")
;arglist = (ARRAY SUB)

(SETF (DOCUMENTATION 'AP-2 'FUNCTION)
  "Internal primitive version of ALOC for 2-dimensional arrays.
You should never need to use this, as the compiler optimizes into this as appropriate")
;arglist = (ARRAY SUB1 SUB2)

(SETF (DOCUMENTATION 'AP-3 'FUNCTION)
  "Internal primitive version of ALOC for 3-dimensional arrays.
You should never need to use this, as the compiler optimizes into this as appropriate")
;arglist = (ARRAY SUB1 SUB2 SUB3)

(SETF (DOCUMENTATION 'AP-LEADER 'FUNCTION)
  "Returns a locative to leader slot INDEX of ARRAY.")
;arglist = (ARRAY INDEX)

(SETF (DOCUMENTATION '%P-LDB-OFFSET 'FUNCTION)
  "Returns the contents of byte PPSS in the word OFFSET beyond POINTER.
This is not the same as (%P-LDB PPSS (%MAKE-POINTER-OFFSET ... POINTER OFFSET))
because it checks for a forwarding pointer in the word addressed by POINTER.
The idea is that POINTER points at the beginning of a structure
and OFFSET is an offset within it.")
;arglist = (PPSS POINTER OFFSET)

(SETF (DOCUMENTATION '%P-DPB-OFFSET 'FUNCTION)
  "Stores VALUE into the byte PPSS in the word OFFSET beyond POINTER.
This is not the same as (%P-DPB VALUE PPSS (%MAKE-POINTER-OFFSET ... POINTER OFFSET))
because it checks for a forwarding pointer in the word addressed by POINTER.
The idea is that POINTER points at the beginning of a structure
and OFFSET is an offset within it.")
;arglist = (VALUE PPSS POINTER OFFSET)

(SETF (DOCUMENTATION '%P-MASK-FIELD-OFFSET 'FUNCTION)
  "MASK-FIELD of PPSS from the contents of the word OFFSET beyond POINTER.
This is not the same as (%P-MASK-FIELD PPSS (%MAKE-POINTER-OFFSET ... POINTER OFFSET))
because it checks for a forwarding pointer in the word addressed by POINTER.
The idea is that POINTER points at the beginning of a structure
and OFFSET is an offset within it.")
;arglist = (PPSS POINTER OFFSET)

(SETF (DOCUMENTATION '%P-DEPOSIT-FIELD-OFFSET 'FUNCTION)
  "Copy byte PPSS from VALUE into the word OFFSET beyond POINTER.
This is not the same what you could simulate using %P-DPB
because it checks for a forwarding pointer in the word addressed by POINTER.
The idea is that POINTER points at the beginning of a structure
and OFFSET is an offset within it.")
;arglist = (VALUE PPSS POINTER OFFSET)

;(SETF (DOCUMENTATION '%MULTIPLY-FRACTIONS 'FUNCTION)
;  "")
;;arglist = (NUM1 NUM2)

;(SETF (DOCUMENTATION '%DIVIDE-DOUBLE 'FUNCTION)
;  "")
;;arglist = (HIGH-DIVIDEND LOW-DIVIDEND DIVISOR)

;(SETF (DOCUMENTATION '%REMAINDER-DOUBLE 'FUNCTION)
;  "")
;;arglist = (HIGH-DIVIDEND LOW-DIVIDEND DIVISOR)

(SETF (DOCUMENTATION 'HAULONG 'FUNCTION)
  "Returns the \"size\" of INTEGER in bits.  The size of 777 is nine bits.")
;arglist = (INTEGER)

;(SETF (DOCUMENTATION '%BETTER-GC-SCAVENGE 'FUNCTION)
; "")
;;arglist = (IDLE-P WORK)

;(SETF (DOCUMENTATION '%ALLOCATE-AND-INITIALIZE-ARRAY 'FUNCTION)
;  "")
;;arglist = (HEADER INDEX-LENGTH LEADER-LENGTH AREA NQS)

(SETF (DOCUMENTATION '%MAKE-POINTER-OFFSET 'FUNCTION)
  "Given data-type and address as pointer+offset, fake up a Lisp object.")
;arglist = (NEW-DTP POINTER OFFSET)

(SETF (DOCUMENTATION '^ 'FUNCTION)
  "Exponentiate NUM to the EXPT power.")
;arglist = (NUM EXPT)

;(SETF (DOCUMENTATION '%CHANGE-PAGE-STATUS 'FUNCTION)
;  "")
;;arglist = (VIRT-ADDR SWAP-STATUS ACCESS-AND-META)

;(SETF (DOCUMENTATION '%CREATE-PHYSICAL-PAGE 'FUNCTION)
;  "")
;;arglist = (PHYS-ADDR)

;(SETF (DOCUMENTATION '%DELETE-PHYSICAL-PAGE 'FUNCTION)
;  "")
;;arglist = (PHYS-ADDR)

;(SETF (DOCUMENTATION '%24-BIT-PLUS 'FUNCTION)
;  "")
;;arglist = (NUM1 NUM2)

;(SETF (DOCUMENTATION '%24-BIT-DIFFERENCE 'FUNCTION)
;  "")
;;arglist = (NUM1 NUM2)

;(SETF (DOCUMENTATION '%24-BIT-TIMES 'FUNCTION)
;  "")
;;arglist = (NUM1 NUM2)

(SETF (DOCUMENTATION 'ABS 'FUNCTION)
  "Returns the absolute value of NUM, which can be any type of number, even complex.")
;arglist = (NUM)

(SETF (DOCUMENTATION '%POINTER-DIFFERENCE 'FUNCTION)
  "Return the number of words difference between two pointers.
They had better be locatives into the same object for this operation to be meaningful;
otherwise, their relative position will be changed by GC.")
;arglist = (PTR1 PTR2)

(SETF (DOCUMENTATION '%P-CONTENTS-AS-LOCATIVE 'FUNCTION)
  "Returns a locative whose pointer field is copied from the word POINTER points to.
If you have determined that that word contains a datum that points at memory,
this is a good way to find the object it points to without getting things
confused by forwarding or by DTP-NULL or by header data types.")
;arglist = (POINTER)

(SETF (DOCUMENTATION '%P-CONTENTS-AS-LOCATIVE-OFFSET 'FUNCTION)
  "Like %P-CONTENTS-AS-LOCATIVE but fetches the word OFFSET beyond where POINTER points.
This is not the same as (%P-CONTENTS-AS-LOCATIVE (%MAKE-POINTER-OFFSET ... POINTER OFFSET))
because it checks for a forwarding pointer in the word addressed by POINTER.
The idea is that POINTER points at the beginning of a structure
and OFFSET is an offset within it.")
;arglist = (POINTER OFFSET)

(SETF (DOCUMENTATION 'EQ 'FUNCTION)
  "T if X and Y are precisely the same Lisp object.")
;arglist = (X Y)

(SETF (DOCUMENTATION '%STORE-CONDITIONAL 'FUNCTION)
  "Store NEW into POINTER if the old contents of POINTER match OLD.
Returns T if the store was done, otherwise NIL.
This is a basic interlocking primitive, which can be used to simulate
any sort of atomic test-and-modify operation.")
;arglist = (POINTER OLD NEW)

(SETF (DOCUMENTATION '%STACK-FRAME-POINTER 'FUNCTION)
  "Returns a locative pointing at the first slot in the current stack frame.
This is the slot that contains a pointer to the function that is executing.
While this will execute in interpreted code, it is not likely to give anything
useful therein.")
;arglist = NIL

(SETF (DOCUMENTATION '*UNWIND-STACK 'FUNCTION)
  "Throw VALUE to TAG, or at most FRAME-COUNT frames; then call ACTION if it's not NIL.
ACTION receives VALUE as its only arg.
If FRAME-COUNT is non-NIL, it should be a fixnum;
if a catch for TAG is not found in that many frames out,
the frame that many frames out, whatever it is, is returned from.")
;arglist = (TAG VALUE FRAME-COUNT ACTION)

(SETF (DOCUMENTATION '%XBUS-READ 'FUNCTION)
  "Returns the contents of the word at address IO-ADDR in XBUS io space.
It returns a 32 bit value as either a fixnum or a bignum")
;arglist = (IO-ADDR)

(SETF (DOCUMENTATION '%XBUS-WRITE 'FUNCTION)
  "Store WORD into xbus io location IO-ADDR.
25. bits of data are usefully stored.")
;arglist = (IO-ADDR WORD)

(SETF (DOCUMENTATION 'ELT 'FUNCTION)
  "Return element INDEX of SEQUENCE.  SEQUENCE is a vector or a list.")
;arglist = (SEQUENCE INDEX)

;(SETF (DOCUMENTATION 'MOVE-PDL-TOP 'FUNCTION)
;  "")
;;arglist = NIL

;(SETF (DOCUMENTATION 'SHRINK-PDL-SAVE-TOP 'FUNCTION)
;  "")
;;arglist = (VALUE-TO-MOVE N-SLOTS)

;(SETF (DOCUMENTATION 'SPECIAL-PDL-INDEX 'FUNCTION)
;  "")
;;arglist = NIL

;(SETF (DOCUMENTATION 'UNBIND-TO-INDEX 'FUNCTION)
;  "")
;;arglist = (SPECIAL-PDL-INDEX)

;(SETF (DOCUMENTATION 'UNBIND-TO-INDEX-MOVE 'FUNCTION)
;  "")
;;arglist = (SPECIAL-PDL-INDEX VALUE-TO-MOVE)

(SETF (DOCUMENTATION 'FIX 'FUNCTION)
  "Convert NUMBER to an integer, which is less than or equal to NUMBER.")
;arglist = (NUMBER)

;(SETF (DOCUMENTATION 'FLOAT 'FUNCTION)
;  "")
;;arglist = (NUMBER OTHER)

(SETF (DOCUMENTATION 'SMALL-FLOAT 'FUNCTION)
  "Convert NUMBER to a small flonum.")
;arglist = (NUMBER)

(SETF (DOCUMENTATION '%FLOAT-DOUBLE 'FUNCTION)
  "The fixnums HIGH and LOW are concatenated together to produce an
unsigned positive integer with twice as many bits as are used to represent pointers.
A flonum containing an approximation to that value is constructed and returned.")
;arglist = (LOW HIGH)

(SETF (DOCUMENTATION 'BIGNUM-TO-ARRAY 'FUNCTION)
  "Converts a bignum into an array.
BIGNUM is expressed in base *PRINT-BASE* and stuffed into an appropriate ART-Q
array.  The sign of the bignum is ignored.")
;arglist = (BIGNUM BASE)

(SETF (DOCUMENTATION 'ARRAY-TO-BIGNUM 'FUNCTION)
  "Converts an array into a bignum.
ARRAY is an ART-Q array, *PRINT-BASE* is a fixnum and SIGN the sign bit (0 or 1).
ARRAY is interpreted as a bignum expressed in base *PRINT-BASE is and converted
into that bignum with sign SIGN.  This is the inverse of BIGNUM-TO-ARRAY.")
;arglist = (ARRAY BASE SIGN)

;(SETF (DOCUMENTATION '%UNWIND-PROTECT-CONTINUE 'FUNCTION)
;  "")
;;arglist = (VALUE TAG COUNT ACTION)

;(SETF (DOCUMENTATION '%WRITE-INTERNAL-PROCESSOR-MEMORIES 'FUNCTION)
;  "")
;;arglist = (CODE ADR D-HI D-LOW)

;(SETF (DOCUMENTATION '%PAGE-STATUS 'FUNCTION)
;  "")
;;arglist = (PTR)

(SETF (DOCUMENTATION '%REGION-NUMBER 'FUNCTION)
  "Return the number of the region PTR points into.
Only the %POINTER field of PTR is significant.")
;arglist = (PTR)

(SETF (DOCUMENTATION '%FIND-STRUCTURE-HEADER 'FUNCTION)
  "Given a locative return the object containing it.
Finds the overall structure containing the cell addressed by the locative pointer.")
;arglist = (PTR)

(SETF (DOCUMENTATION '%STRUCTURE-BOXED-SIZE 'FUNCTION)
  "Returns the number of normal Lisp pointers in an object.
This many words at the beginning of the object contain normal Lisp data.
The remaining words contain just numbers (such as, the instructions of a FEF,
or the data in a numeric array).")
;arglist = (PTR)

(SETF (DOCUMENTATION '%STRUCTURE-TOTAL-SIZE 'FUNCTION)
  "Returns the number of words in an object.")
;arglist = (PTR)

;(SETF (DOCUMENTATION '%MAKE-REGION 'FUNCTION)
;  "")
;;arglist = (BITS SIZE)

(SETF (DOCUMENTATION 'BITBLT 'FUNCTION)
  "")
;arglist = (ALU WIDTH HEIGHT FROM-ARRAY FROM-X FROM-Y TO-ARRAY TO-X TO-Y)

;(SETF (DOCUMENTATION '%DISK-OP 'FUNCTION)
;  "")
;;arglist = (RQB)

(SETF (DOCUMENTATION '%PHYSICAL-ADDRESS 'FUNCTION)
  "Return the address in core of the page which contains PTR.
The value is a fixnum which may be negative.
Only the pointer field of PTR is significant.")
;arglist = (PTR)

;(SETF (DOCUMENTATION 'POP-OPEN-CALL 'FUNCTION)
;  "")
;;arglist = NIL

(SETF (DOCUMENTATION '%BEEP 'FUNCTION)
  "")
;arglist = (HALF-WAVELENGTH DURATION)

(SETF (DOCUMENTATION '%FIND-STRUCTURE-LEADER 'FUNCTION)
  "Given a locative return the object containing it.
This is like %FIND-STRUCTURE-HEADER except that it always returns the base of the
structure; thus for an array with a leader it gives a locative to the base instead
of giving the array.")
;arglist = (PTR)

;(SETF (DOCUMENTATION 'BPT 'FUNCTION)
;  "")
;;arglist = NIL

;(SETF (DOCUMENTATION '%FINDCORE 'FUNCTION)
;  "")
;;arglist = NIL

;(SETF (DOCUMENTATION '%PAGE-IN 'FUNCTION)
;  "")
;;arglist = (PFN VPN)

(SETF (DOCUMENTATION 'ASH 'FUNCTION)
  "Shift N arithmetically by NBITS. N must be an integer.")
;arglist = (N NBITS)

;(SETF (DOCUMENTATION '%MAKE-EXPLICIT-STACK-LIST 'FUNCTION)
;  "")
;;arglist = (LENGTH)

(SETF (DOCUMENTATION '%DRAW-CHAR 'FUNCTION)
  "Draw character CHAR-CODE of font FONT-ARRAY on SHEET using ALU-FUNCTION.
ALU-FUNCTION is typically TV:ALU-IOR, TV:ALU-ANDCA or TV:ALU-XOR.
X-BITPOS and Y-BITPOS are the position in SHEET for the upper left corner.")
;arglist = (FONT-ARRAY CHAR-CODE X-BITPOS Y-BITPOS ALU-FUNCTION SHEET)

(SETF (DOCUMENTATION '%DRAW-RECTANGLE 'FUNCTION)
  "Draw a solid rectangle on SHEET using ALU-FUNCTION.
ALU-FUNCTION is typically TV:ALU-IOR, TV:ALU-ANDCA or TV:ALU-XOR.
HEIGHT and WIDTH are the size, and X-BITPOS and Y-BITPOS are the upper left corner.")
;arglist = (WIDTH HEIGHT X-BITPOS Y-BITPOS ALU-FUNCTION SHEET)

(SETF (DOCUMENTATION '%DRAW-LINE 'FUNCTION)
  "Draw a straight line from X0, Y0 to X, Y on SHEET using ALU-FUNCTION.
ALU-FUNCTION is typically TV:ALU-IOR, TV:ALU-ANDCA or TV:ALU-XOR.
If DRAW-END-POINT is NIL, the point X, Y is omitted.")
;arglist = (X0 Y0 X Y ALU DRAW-END-POINT SHEET)

(SETF (DOCUMENTATION '%DRAW-TRIANGLE 'FUNCTION)
  "Draw a triangle with corners at (X1, Y1), (X2, Y2) and (X3, Y3) on SHEET.
ALU-FUNCTION is typically TV:ALU-IOR, TV:ALU-ANDCA or TV:ALU-XOR.
Pixels are regarded as being between coordinate points, and the top
left pixel is considered between X values 0 and 1, and between Y values 0 and 1.")
;arglist = (X1 Y1 X2 Y2 X3 Y3 ALU SHEET)

(SETF (DOCUMENTATION '%COLOR-TRANSFORM 'FUNCTION)
  "Modify all pixels in a rectangle within ARRAY, an ART-4B array.
If the pixel contains 0, it is replaced by N0, and so on.
WIDTH and HEIGHT are the size of rectangle; START-X and START-Y the top left corner.")
;arglist = (N17 N16 N15 N14 N13 N12 N11 N10 N7 N6 N5 N4 N3 N2 N1 N0 WIDTH HEIGHT ARRAY START-X START-Y)

;(SETF (DOCUMENTATION '%RECORD-EVENT 'FUNCTION)
;  "")
;;arglist = (DATA-4 DATA-3 DATA-2 DATA-1 STACK-LEVEL EVENT MUST-BE-4)

(SETF (DOCUMENTATION '%AOS-TRIANGLE 'FUNCTION)
  "Add INCREMENT to each pixel in a triangle on SHEET.
The triangle's corners are at (X1, Y1), (X2, Y2) and (X3, Y3).
Pixels are regarded as being between coordinate points, and the top
left pixel is considered between X values 0 and 1, and between Y values 0 and 1.")
;arglist = (X1 Y1 X2 Y2 X3 Y3 INCREMENT SHEET)

;(SETF (DOCUMENTATION '%SET-MOUSE-SCREEN 'FUNCTION)
;  "")
;;arglist = (SHEET)

;(SETF (DOCUMENTATION '%OPEN-MOUSE-CURSOR 'FUNCTION)
;  "")
;;arglist = NIL

(SETF (DOCUMENTATION 'SETELT 'FUNCTION)
  "SETF of ELT expands into this.")
;arglist = (SEQUENCE INDEX VALUE)

(SETF (DOCUMENTATION '%BLT-TYPED 'FUNCTION)
  "Copy a block of memory, a word at a time, with no decoding, for typed data.
Use %BLT for words of raw bits which do not contain Lisp data types.
The first word is copied from FROM-ADDRESS to TO-ADDRESS.
INCREMENT is added to each address and then another word is copied, and so on.
COUNT is the number of words to copy.")
;arglist = (FROM-ADDRESS TO-ADDRESS COUNT INCREMENT)

;(SETF (DOCUMENTATION '%DRAW-PATTERNED-LINE 'FUNCTION)
;  "")
;arglist = (PATTERN-ARRAY FROM-X FROM-Y TO-X TO-Y ALU-FUNCTION DRAW-LAST-POINT-P CURRENT-SHEET)

(SETF (DOCUMENTATION 'AR-1-FORCE 'FUNCTION)
  "Return contents of element INDEX of ARRAY, treated as one-dimensional.
ARRAY is treated as one-dimensional in that it is indexed with
a single subscript regardless of its rank.")
;arglist = (ARRAY INDEX)

(SETF (DOCUMENTATION 'AS-1-FORCE 'FUNCTION)
  "Store VALUE into element INDEX of ARRAY, treated as one-dimensional.
ARRAY is treated as one-dimensional in that it is indexed with
a single subscript regardless of its rank.")
;arglist = (VALUE ARRAY INDEX)

(SETF (DOCUMENTATION 'AP-1-FORCE 'FUNCTION)
  "Return a locative to element INDEX of ARRAY, treated as one-dimensional.
ARRAY is treated as one-dimensional in that it is indexed with
a single subscript regardless of its rank.")
;arglist = (ARRAY INDEX)

(SETF (DOCUMENTATION 'AREF 'FUNCTION)
  "Return the contents of the element of ARRAY specified by SUBSCRIPTS.")
;arglist = (ARRAY &REST SUBSCRIPTS)

(SETF (DOCUMENTATION 'ASET 'FUNCTION)
  "Store VALUE in the element of ARRAY specified by SUBSCRIPTS.")
;arglist = (VALUE ARRAY &REST SUBSCRIPTS)

(SETF (DOCUMENTATION 'ALOC 'FUNCTION)
  "Return a locative to the element of ARRAY specified by SUBSCRIPTS.")
;arglist = (ARRAY &REST SUBSCRIPTS)

(SETF (DOCUMENTATION 'EQUALP 'FUNCTION)
  "Like EQUAL but ignores case and numeric type, and looks at elts of arrays.
Strings are compared by their contents, using STRING-EQUAL.
Other arrays are just compared elementwise.
All numbers are passed to =, so that 0 and 0.0 give T.")
;arglist = (X Y)

;(SETF (DOCUMENTATION '%MAKE-EXPLICIT-STACK-LIST* 'FUNCTION)
;  "")
;;arglist = (LENGTH)

(SETF (DOCUMENTATION 'SETCAR 'FUNCTION)
  "SETF of CAR expands into this. Like RPLACA, but returen NEWCAR.")
;arglist = (CONS NEWCAR)

(SETF (DOCUMENTATION 'SETCDR 'FUNCTION)
  "SETF of CDR expands into this. Like RPLACD, but returns NEWCDR.")
;arglist = (CONS NEWCDR)

(SETF (DOCUMENTATION 'GET-LOCATION-OR-NIL 'FUNCTION)
  "Return the location of property PROPERTY in  plist of SYMBOL, or NIL if no property.
SYMBOL can actually be anything you can GET from.")
;arglist = (SYMBOL PROPERTY)

;(SETF (DOCUMENTATION '%STRING-WIDTH 'FUNCTION)
;  "")
;;arglist = (TABLE OFFSET STRING START END STOP-WIDTH)

;(SETF (DOCUMENTATION 'AR-1-CACHED-1 'FUNCTION)
;  "")
;;arglist = (ARRAY SUBSCRIPT)

;(SETF (DOCUMENTATION 'AR-1-CACHED-2 'FUNCTION)
;  "")
;;arglist = (ARRAY SUBSCRIPT)

;(SETF (DOCUMENTATION '%MULTIBUS-READ-16 'FUNCTION)
;  "")
;;arglist = (MULTIBUS-BYTE-ADR)

;(SETF (DOCUMENTATION '%MULTIBUS-WRITE-16 'FUNCTION)
;  "")
;;arglist = (MULTIBUS-BYTE-ADR WORD)

;(SETF (DOCUMENTATION '%MULTIBUS-READ-8 'FUNCTION)
;  "")
;;arglist = (MULTIBUS-BYTE-ADR)

;(SETF (DOCUMENTATION '%MULTIBUS-WRITE-8 'FUNCTION)
;  "")
;;arglist = (MULTIBUS-BYTE-ADR WORD)

;(SETF (DOCUMENTATION '%MULTIBUS-READ-32 'FUNCTION)
;  "")
;;arglist = (MULTIBUS-BYTE-ADR)

;(SETF (DOCUMENTATION '%MULTIBUS-WRITE-32 'FUNCTION)
;  "")
;;arglist = (MULTIBUS-BYTE-ADR WORD)

(SETF (DOCUMENTATION 'SET-AR-1 'FUNCTION)
  "Internal primitive version of SETF of AREF for 1-dimensional arrays.
You should never need to use this, as the compiler optimizes into this as appropriate")
;arglist = (ARRAY SUBSCRIPT VALUE)

(SETF (DOCUMENTATION 'SET-AR-2 'FUNCTION)
  "Internal primitive version of SETF of AREF for 2-dimensional arrays.
You should never need to use this, as the compiler optimizes into this as appropriate")
;arglist = (ARRAY SUBSCRIPT1 SUBSCRIPT2 VALUE)

(SETF (DOCUMENTATION 'SET-AR-3 'FUNCTION)
  "Internal primitive version of SETF of AREF for 3-dimensional arrays.
You should never need to use this, as the compiler optimizes into this as appropriate")
;arglist = (ARRAY SUBSCRIPT1 SUBSCRIPT2 SUBSCRIPT3 VALUE)

(SETF (DOCUMENTATION 'SET-AR-1-FORCE 'FUNCTION)
  "Internal primitive version of SET of AR-1-FORCE.
You should never need to use this, as the compiler optimizes into this as appropriate")
;arglist = (ARRAY SUBSCRIPT VALUE)

(SETF (DOCUMENTATION 'SET-AREF 'FUNCTION)
  "SETF of AREF expands into this.")
;arglist = (ARRAY &REST SUBSCRIPTS-AND-VALUE)

(SETF (DOCUMENTATION 'SET-ARRAY-LEADER 'FUNCTION)
  "SETF of ARRAY-LEADER expands into this.")
;arglist = (ARRAY INDEX VALUE)

(SETF (DOCUMENTATION 'SET-%INSTANCE-REF 'FUNCTION)
  "SETF of %INSTANCE-REF expands into this.")
;arglist = (INSTANCE INDEX VALUE)

(SETF (DOCUMENTATION 'VECTOR-PUSH 'FUNCTION)
  "Add NEW-ELEMENT as an element at the end of VECTOR.
The fill pointer (leader element 0) is the index of the next element to
be added.  Returns NIL and does no add the element if the array is full;
use VECTOR-PUSH-EXTEND instead if you want the array to grow automatically.")
;arglist = (NEW-ELEMENT VECTOR)

(SETF (DOCUMENTATION 'ARRAY-HAS-FILL-POINTER-P 'FUNCTION)
  "T if ARRAY has a fill pointer.")
;arglist = (ARRAY)

(SETF (DOCUMENTATION 'ARRAY-LEADER-LENGTH 'FUNCTION)
  "Return the number of elements in ARRAY's leader, or NIL if no leader.")
;arglist = (ARRAY)

(SETF (DOCUMENTATION 'ARRAY-RANK 'FUNCTION)
  "Return the number of dimensions of ARRAY.")
;arglist = (ARRAY)

(SETF (DOCUMENTATION 'ARRAY-DIMENSION 'FUNCTION)
  "Return the length of dimension DIMENSION-NUMBER of ARRAY.  The first dimension is number 0.")
;arglist = (ARRAY DIMENSION)

;(SETF (DOCUMENTATION 'ARRAY-IN-BOUNDS-P 'FUNCTION)
;  "T if the indices are in bounds for the dimensions of ARRAY.")
;;arglist = (ARRAY &REST SUBSCRIPTS)

;(SETF (DOCUMENTATION 'ARRAY-ROW-MAJOR-INDEX 'FUNCTION)
;  "Return the combined index in ARRAY of the element identified by SUBSCRIPTS.
;This value could be used as the second argument of AR-1-FORCE to access that element.")
;arglist = (ARRAY &REST SUBSCRIPTS)

;(SETF (DOCUMENTATION 'RETURN-N-KEEP-CONTROL 'FUNCTION)
;  "")
;;arglist = (&REST VALUES N)

;(SETF (DOCUMENTATION 'RETURN-SPREAD-KEEP-CONTROL 'FUNCTION)
;  "")
;;arglist = (VALUE-LIST)

(SETF (DOCUMENTATION 'COMMON-LISP-LISTP 'FUNCTION)
  "T if OBJECT is a cons or NIL")
;arglist = (OBJECT)

(SETF (DOCUMENTATION '%NUBUS-READ 'FUNCTION)
  "NUBUS-SLOT is top 8 bits of physical address (usually the top 4 bits are 1's).
SLOT-BYTE-ADR is the lower 24 bits of physical address.")
;;arglist = (NUBUS-SLOT SLOT-BYTE-ADR)

(SETF (DOCUMENTATION '%NUBUS-WRITE 'FUNCTION)
  "NUBUS-SLOT is top 8 bits of physical address (usually the top 4 bits are 1's).
SLOT-BYTE-ADR is the lower 24 bits of physical address.
WORD is a 32 bit integer.")
;;arglist = (NUBUS-SLOT SLOT-BYTE-ADR WORD)

;(SETF (DOCUMENTATION '%MICROSECOND-TIME 'FUNCTION)
;  "")
;;arglist = NIL

;(SETF (DOCUMENTATION '%FIXNUM-MICROSECOND-TIME 'FUNCTION)
;  "")
;;arglist = NIL

;(SETF (DOCUMENTATION '%IO-SPACE-READ 'FUNCTION)
;  "")
;;arglist = (IO-ADDR)

;(SETF (DOCUMENTATION '%IO-SPACE-WRITE 'FUNCTION)
;  "")
;;arglist = (IO-ADDR WORD)

;(SETF (DOCUMENTATION '%NUBUS-PHYSICAL-ADDRESS 'FUNCTION)
;  "")
;;arglist = (APPARENT-PHYSICAL-PAGE)

(SETF (DOCUMENTATION 'VECTORP 'FUNCTION)
  "T if OBJECT is a vector: an array of rank 1.")
;arglist = (OBJECT)

(SETF (DOCUMENTATION 'SIMPLE-VECTOR-P 'FUNCTION)
  "T if OBJECT is a simple general vector.
This is a simple array of rank 1 whose elements are unrestricted.
A simple array is one which is not displaced and has no fill pointer.")
;arglist = (OBJECT)

(SETF (DOCUMENTATION 'SIMPLE-ARRAY-P 'FUNCTION)
  "T if OBJECT is a simple array, an array which is not displaced and has no fill pointer.")
;arglist = (OBJECT)

(SETF (DOCUMENTATION 'SIMPLE-STRING-P 'FUNCTION)
  "T if OBJECT is a simple string, a string which is not displaced and has no fill pointer.")
;arglist = (OBJECT)

(SETF (DOCUMENTATION 'BIT-VECTOR-P 'FUNCTION)
  "T if OBJECT is a bit vector, an array of rank 1 whose elements are restricted to 0 and 1.")
;arglist = (OBJECT)

(SETF (DOCUMENTATION 'SIMPLE-BIT-VECTOR-P 'FUNCTION)
  "T if OBJECT is a simple bit vector, one which is not displaced and has no fill pointer.")
;arglist = (OBJECT)

(SETF (DOCUMENTATION 'NAMED-STRUCTURE-P 'FUNCTION)
  "If OBJECT is a named-structure, return its structure type keyword, otherwise NIL")
;arglist = (OBJECT)

(SETF (DOCUMENTATION 'NAMED-STRUCTURE-SYMBOL 'FUNCTION)
  "This is an obsolete name for the function NAMED-STRUCTURE-P")
;arglist = (OBJECT)

;(SETF (DOCUMENTATION 'TYPEP-STRUCTURE-OR-FLAVOR 'FUNCTION)
;  "")
;;arglist = (OBJECT TYPE)

(SETF (DOCUMENTATION 'FIXNUMP 'FUNCTION)
  "T if OBJECT is a fixnum; an integer close enough to zero to require no storage.")
;arglist = (OBJECT)

(SETF (DOCUMENTATION 'SMALL-FLOATP 'FUNCTION)
  "T if OBJECT is of type SHORT-FLOAT (a small flonum).")
;arglist = (OBJECT)

(SETF (DOCUMENTATION 'CHARACTERP 'FUNCTION)
  "T if OBJECT is a character.")
;arglist = (OBJECT)

(SETF (DOCUMENTATION 'CAR-SAFE 'FUNCTION)
  "Returns (CAR OBJECT) if OBJECT is a cons, else NIL.")
;arglist = (OBJECT)

(SETF (DOCUMENTATION 'CDR-SAFE 'FUNCTION)
  "Returns (CDR OBJECT) is OBJECT is a cons, else NIL.")
;arglist = (OBJECT)

(SETF (DOCUMENTATION 'CADR-SAFE 'FUNCTION)
  "Returns (CADR OBJECT) if that can be done without error on OBJECT, else NIL.
Never gets an error.")
;;arglist = (OBJECT)

(SETF (DOCUMENTATION 'CDDR-SAFE 'FUNCTION)
  "Returns (CDDR OBJECT) if that can be done without error on OBJECT, else NIL.
Never gets an error.")
;arglist = (OBJECT)

(SETF (DOCUMENTATION 'CDDDDR-SAFE 'FUNCTION)
  "Returns (CDDDR OBJECT) if that can be done without error on OBJECT, else NIL.
Never gets an error.")
;arglist = (OBJECT)

(SETF (DOCUMENTATION 'NTHCDR-SAFE 'FUNCTION)
  "Returns (CADR OBJECT) if that can be done without error on OBJECT, else NIL.
Never gets an error.")
;arglist = (N OBJECT)

(SETF (DOCUMENTATION 'NTH-SAFE 'FUNCTION)
  "Returns (NTH N OBJECT) if that can be done without error on OBJECT, else NIL
Never gets an error.")
;arglist = (N OBJECT)

;(SETF (DOCUMENTATION 'CARCDR 'FUNCTION)
;  "")
;;arglist = (LIST)

(SETF (DOCUMENTATION 'ENDP 'FUNCTION)
  "T if X as the cdr of a list would terminate that list.
\(In fact, this function is the same as ATOM)")
;arglist = (X)

;(SETF (DOCUMENTATION 'CONSP-OR-POP 'FUNCTION)
;  "")
;;arglist = (OBJECT)

;(SETF (DOCUMENTATION 'INDICATORS-VALUE 'FUNCTION)
;  "")
;;arglist = (OBJECT)

;(SETF (DOCUMENTATION '%POINTER-TIMES 'FUNCTION)
;  "")
;;arglist = (POINTER1 POINTER2)

;(SETF (DOCUMENTATION 'COMMON-LISP-AREF 'FUNCTION)
;  "")
;;arglist = (ARRAY &REST INDICES)

;(SETF (DOCUMENTATION 'COMMON-LISP-AR-1 'FUNCTION)
;  "")
;;arglist = (ARRAY INDEX)

;(SETF (DOCUMENTATION 'COMMON-LISP-AR-1-FORCE 'FUNCTION)
;  "")
;;arglist = (ARRAY INDEX)

(SETF (DOCUMENTATION 'INTERNAL-GET-3 'FUNCTION)
  "Internal primitive version of GET when three arguments are supplied.
You should never need to use this, as the compiler optimizes into this as appropriate")
;arglist = (SYMBOL PROPERTY DEFAULT)

(SETF (DOCUMENTATION 'CHAR-INT 'FUNCTION)
  "Returns an integer whose value corresponds to CHAR.
On the Lisp machine, this conversion will happen automatically
in most places that an integer can be used.")
;;arglist = (CHAR)

(SETF (DOCUMENTATION 'INT-CHAR 'FUNCTION)
  "Returns a character whose value corresponds to INTEGER.")
;;arglist = (INTEGER)

(SETF (DOCUMENTATION 'ALPHA-CHAR-P 'FUNCTION)
  "T if CHAR is alphabetic with no modifier bits.")
;;arglist = (CHAR)

(SETF (DOCUMENTATION 'BOTH-CASE-P 'FUNCTION)
  "T if CHAR is alphabetic with no modifier bits.")
;;arglist = (CHAR)

(SETF (DOCUMENTATION 'UPPER-CASE-P 'FUNCTION)
  "T if CHAR is upper case alphabetic with no modifier bits.")
;;arglist = (CHAR)

(SETF (DOCUMENTATION 'LOWER-CASE-P 'FUNCTION)
  "T if CHAR is lower case alphabetic with no modifier bits.")
;;arglist = (CHAR)

(SETF (DOCUMENTATION 'ALPHANUMERICP 'FUNCTION)
  "T if CHAR is a digit or alphabetic, with no modifier bits.")
;;arglist = (CHAR)

(SETF (DOCUMENTATION 'PACKAGE-CELL-LOCATION 'FUNCTION)
  "Returns a locative pointing to the cell in which SYMBOL's package is stored.")
;;arglist = (SYMBOL)

(SETF (DOCUMENTATION 'MEMBER-EQL 'FUNCTION)
  "T if ELT is EQL some element of LIST.")
;;arglist = (ELT LIST)

(SETF (DOCUMENTATION 'RATIONALP 'FUNCTION)`
  "T if OBJECT is a rational number, that is either an integer or a ratio.")
;;arglist = (OBJECT)

(SETF (DOCUMENTATION 'RATIOP 'FUNCTION)
  "T if OBJECT is a ratio.
Note that this does not include integer. To test for either integers or ratios, use RATIONALP")
;;arglist = (OBJECT)

(SETF (DOCUMENTATION 'COMPLEXP 'FUNCTION)
  "T if OBJECT is a complex number.
Note that this may include complex numbers with an imaginary part of 0.0!
To avoid this problem, use you may wany to use (NOT (REALP X)) instead.")
;;arglist = (OBJECT)

(SETF (DOCUMENTATION '%COMPLEX-CONS 'FUNCTION)
  "Internal function to return a rational number with specified numerator and denominator.
This can be used to construct rationals not in lowest terms, and so should not normally
be so used. Use CLI:/, RATIONAL or RATIONALIZE instead.")
;;arglist = (REALPART IMAGPART)

(SETF (DOCUMENTATION '%RATIO-CONS 'FUNCTION)
  "Return a rational number with specified numerator and denominator.
This can be used to construct non-canonicalized complex numbers, and so should not normally
be used. Use COMPLEX instead.")
;;arglist = (NUMERATOR DENOMINATOR)

;(SETF (DOCUMENTATION '%MICRO-PAGING 'FUNCTION)
;  "")
;;arglist = (ARG)

;(SETF (DOCUMENTATION '%PROCESSOR-SWITCHES 'FUNCTION)
;  "")
;;arglist = (ARG)

;(SETF (DOCUMENTATION '%COLD-BOOT 'FUNCTION)
;  "")
;;arglist = NIL

(SETF (DOCUMENTATION '%NUBUS-READ-8 'FUNCTION)
  "NUBUS-SLOT is top 8 bits of physical address (usually the top 4 bits are 1's).
SLOT-BYTE-ADR is the lower 24 bits of physical address.")
;;arglist = (NUBUS-SLOT SLOT-BYTE-ADR)

(SETF (DOCUMENTATION '%NUBUS-WRITE-8 'FUNCTION)
  "NUBUS-SLOT is top 8 bits of physical address (usually the top 4 bits are 1's).
SLOT-BYTE-ADR is the lower 24 bits of physical address.
WORD is a 8 bit integer.")
;;arglist = (NUBUS-SLOT SLOT-BYTE-ADR WORD)

;(SETF (DOCUMENTATION '%LAMBDA-RG-QUAD-SLOT 'FUNCTION)
;  "")
;;arglist = NIL

;(SETF (DOCUMENTATION '%LAMBDA-TV-QUAD-SLOT 'FUNCTION)
;  "")
;;arglist = NIL

;(SETF (DOCUMENTATION '%LAMBDA-SDU-QUAD-SLOT 'FUNCTION)
;  "")
;;arglist = NIL

;(SETF (DOCUMENTATION '%LAMBDA-SYS-CONF-VIRTUAL-TO-PHYS 'FUNCTION)
;  "")
;;arglist = VIRTUAL-ADDRESS

;(SETF (DOCUMENTATION '%LAMBDA-SYS-CONF-PHYS-TO-VIRTUAL 'FUNCTION)
;  "")
;;arglist = PHYSICAL-ADDRESS

;(SETF (DOCUMENTATION '%LAMBDA-SYS-CONF-VIRTUAL-ADR 'FUNCTION)
;  "")
;;arglist = NIL

;(SETF (DOCUMENTATION '%LAMBDA-MOUSE-BUTTONS 'FUNCTION)
;  "")
;;arglist = NIL

(SETF (DOCUMENTATION 'ZL:MEMBER 'FUNCTION)
  "MEMBER is like MEMQ except that the comparison uses EQUAL instead of EQ")
;;arglist = (TARGET LIST)

(SETF (DOCUMENTATION 'MEMBER-EQUAL 'FUNCTION)
  "A synonym for ZL:MEMBER")
;;arglist = (TARGET LIST)

(SETF (DOCUMENTATION 'ZL:ASSOC 'FUNCTION)
  "ASSOC is like ASSQ except that the comparison uses EQUAL instead of EQ")
;;arglist = (TARGET LIST)

(SETF (DOCUMENTATION 'ASSOC-EQUAL 'FUNCTION)
  "A synonym for ZL:ASSOC")
;;arglist = (TARGET LIST)

;(SETF (DOCUMENTATION '%SET-METER-ENABLES 'FUNCTION)
;  "")
;;arglist = (N)

;(SETF (DOCUMENTATION '%BLT-BOOLE 'FUNCTION)
;  "")
;;arglist = (ALU FROM-ADDRESS TO-ADDRESS COUNT INCREMENT)

;(SETF (DOCUMENTATION '%STAT-COUNTER 'FUNCTION)
;  "")
;;arglist = (OP-CODE VALUE)

;(SETF (DOCUMENTATION '%MAKE-STRUCTURE 'FUNCTION)
;  "")
;;arglist = (POINTER-TAG HEADER-TAG HEADER SECOND-WORD AREA TOTAL BOXED)

;(SETF (DOCUMENTATION '%MAKE-ARRAY 'FUNCTION)
;  "")
;;arglist = (HEADER-WORD INDEX-LENGTH LEADER-LENGTH AREA TOTAL-SIZE BOXED-SIZE)

;(SETF (DOCUMENTATION '%POINTER-LESSP 'FUNCTION)
;  "")
;;arglist = (P1 P2)

;(SETF (DOCUMENTATION '%POINTER-GREATERP 'FUNCTION)
;  "")
;;arglist = (P1 P2)

;(SETF (DOCUMENTATION '%MULTIBUS-BLT-16 'FUNCTION)
;  "")
;;arglist = (ALU STARTING-VADR STARTING-PHASE WIDTH-IN-16S ROWS SKIP-IN-16S MULTIBUS-ADR)

;(SETF (DOCUMENTATION ' %STORE-CONDITIONAL-DOUBLE 'FUNCTION)
;  "")
;;arglist = (POINTER OLD NEW-POINTER NEW)

;(SETF (DOCUMENTATION '%P-STORE-DATA-TYPE-AND-POINTER 'FUNCTION)
;  "")
;;arglist = (ADDRESS DATA-TYPE POINTER-TO-STORE)

(SETF (DOCUMENTATION '%NUBUS-READ-SAFE 'FUNCTION)
  "NUBUS-SLOT is top 8 bits of physical address (usually the top 4 bits are 1's).
SLOT-BYTE-ADR is the lower 24 bits of physical address.
Returns NIL if the read fails. Since this can't be implemented on the lambda
processor it is the same as %NUBUS-READ")
;;arglist = (NUBUS-SLOT SLOT-BYTE-ADR)

(SETF (DOCUMENTATION '%NUBUS-READ-8-SAFE 'FUNCTION)
  "NUBUS-SLOT is top 8 bits of physical address (usually the top 4 bits are 1's).
SLOT-BYTE-ADR is the lower 24 bits of physical address.
Returns NIL if the read fails. Since this can't be implemented on the lambda
processor it is the same as %NUBUS-READ-8")
;;arglist = (NUBUS-SLOT SLOT-BYTE-ADR)

(SETF (DOCUMENTATION '%NUBUS-WRITE-SAFE 'FUNCTION)
  "NUBUS-SLOT is top 8 bits of physical address (usually the top 4 bits are 1's).
SLOT-BYTE-ADR is the lower 24 bits of physical address.
WORD is a 32 bit integer.
Returns NIL if the write fails. Since this can't be implemented on the lambda
processor it is the same as %NUBUS-WRITE")
;;arglist = (NUBUS-SLOT SLOT-BYTE-ADR WORD)

(SETF (DOCUMENTATION '%NUBUS-WRITE-8-SAFE 'FUNCTION)
  "NUBUS-SLOT is top 8 bits of physical address (usually the top 4 bits are 1's).
SLOT-BYTE-ADR is the lower 24 bits of physical address.
WORD is a 8 bit integer.
Returns NIL if the write fails. Since this can't be implemented on the lambda
processor it is the same as %NUBUS-WRITE-8")
;;arglist = (NUBUS-SLOT SLOT-BYTE-ADR WORD)

;(SETF (DOCUMENTATION '%SET-MOUSE-ARRAYS 'FUNCTION)
;  "")
;;arglist = (CURSOR-PATTERN BUTTONS-BUFFER X-SCALE Y-SCALE)

;(SETF (DOCUMENTATION '%MAP-DEVICE-QUANTUM 'FUNCTION)
;  "")
;;arglist = (QUANTUM-NUMBER NUBUS-PAGE NUBUS-WORDS L2-CONTROL)

(SETF (DOCUMENTATION '%IP-CHECKSUM 'FUNCTION)
  "The primitive TCP/IP checksum function.
ARRAY is an ART-8B array to be checksummed.
SUM is the initial sum to add into.
COUNT is the number of bytes to consider.
ODD-P is T, start adding into low byte of checksum.")
;;arglist = (ARRAY SUM COUNT ODD-P)
