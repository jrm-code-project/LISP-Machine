;;; Hey, Editors, this file should be -*- Package:hacks;Base:10. -*-

;; ** (uc) Not copyrightten 1983 by the Massachusetts Institute of Technology
;;     Everyone is entitled to use the software free of charge, as long as
;;     MIT is acknowledged as the source of this software.                  **

(DECLARE (SPECIAL WHAT-PARSE-STRING-ALIST))

(DEFUN WHAT-CAN-I-DO ()
  "Returns a string of all of the commands that the WHAT program can hack."
  (WITH-OUTPUT-TO-STRING (S)
    (DOLIST (ENTRY WHAT-PARSE-STRING-ALIST)
         (FUNCALL S ':LINE-OUT (CAR ENTRY)))))

(DEFUN WHAT-RANDOMNESS ()
  "Pick a random WHAT to do and do it."
  (WHAT (CAR (NTH (RANDOM (LENGTH WHAT-PARSE-STRING-ALIST)) WHAT-PARSE-STRING-ALIST))))

(DEFVAR *AGE-OF-UNIVERSE-IN-BILLIONS-OF-YEARS-BIGNUM*
        233425256326591223212781264. "Age of Universe.")


(DEFUN WHAT-AGE-UNIVERSE ()
  "Compute the age of the Universe."
  (PROCESS-SLEEP 300. "Computing")
  (FORMAT NIL
          "Using Big Bang Cosmology,  ~D billion years,
plus or minus 14 fermtoseconds."  ;its used the line feed right there
  ;;don't trust this algorithm !!  Ask alan@mc for a reference and real time.
          (+ (// *AGE-OF-UNIVERSE-IN-BILLIONS-OF-YEARS-BIGNUM* 1E25)
             (QUOTIENT (+ (RANDOM 14) (TIME:GET-UNIVERSAL-TIME))
                       (* 3600. 24. 365. 1000000000.))))) ;.lose 365++



(DEFUN ADD-WHAT (STRING FIND-MODE WHAT-TO-DO)
  "Adds a new thing for the WHAT program to understand.
See also the documentation of WHAT-PARSE-STRING-ALIST."
  (PUSH (LIST STRING FIND-MODE WHAT-TO-DO) WHAT-PARSE-STRING-ALIST)) ;lossage potential

   ;;this wins better than sys:io1;conver
(DEFUN WHAT-SENDS (&OPTIONAL (NUMBER 0) (STREAM STANDARD-OUTPUT) &AUX SENDS S (POS 0))
  "Print the last NUMBER sends on standard-output.  If number is 0, print them all."
  (COND ((OR (= 0 (STRING-LENGTH ZWEI:*SAVED-SENDS*))
             (NULL ZWEI:*SAVED-SENDS*))  ;older systems only -hdt
         (FORMAT NIL "You have no interactive messages."))
        ((= NUMBER 0)
         (FORMAT STREAM ZWEI:*SAVED-SENDS*))
        (T
         (SETQ SENDS (STRING-APPEND ZWEI:*SAVED-SENDS*))
         (SETQ S (STRING-APPEND #\RETURN #\RETURN))
         (DOTIMES (I NUMBER)
           (SEND STANDARD-OUTPUT ':FRESH-LINE)
           (SETQ POS (STRING-SEARCH S SENDS POS))
           (IF (NULL POS) (RETURN))
           (FORMAT STREAM
                   (SUBSTRING SENDS (INCF POS)
                              (STRING-SEARCH S SENDS POS)))))))


(DEFUN PERSONS-BIRTHDAY (WHO &OPTIONAL (STREAM STANDARD-OUTPUT))
  "Return the birthday of person WHO, in English form on stream STREAM."
  (CHECK-ARG WHO #'STRINGP "a string")
  (SETQ WHO (STRING-UPCASE WHO))
  (AND (> (STRING-LENGTH WHO) 2)
       (STRING-EQUAL "'S" (SUBSTRING WHO (- (LENGTH WHO) 2)))
       (SETQ WHO (SUBSTRING WHO 0 (- (LENGTH WHO) 2))))
  (CONDITION-CASE (TIME)
      (MULTIPLE-VALUE-BIND
       (S M H D MON Y I1 I2)
          (TIME:PARSE (STRING-APPEND WHO "'S BIRTHDAY"))
        (IGNORE I1 I2)
        (TIME:ENCODE-UNIVERSAL-TIME S M H D MON Y))
    (:NO-ERROR (TIME:PRINT-UNIVERSAL-TIME TIME STREAM))
    (TIME:PARSE-ERROR (FORMAT STREAM "Sorry, cannot determine ~A's birthday." WHO))))

(DEFVAR WHAT-PUNCTUATION "?.!"
  "A string of characters to be ignored if they terminate the input to WHAT")

(DEFMACRO EMPTY? (STRING)
  `(OR (NULL ,STRING) (= (STRING-LENGTH ,STRING) 0)))

(DEFUN WHAT (&OPTIONAL STRING (STREAM STANDARD-OUTPUT)
             &AUX FOUND-IT FIND-MODE SEARCH-STRING HOW ARGS L1 L2)
  "Simulate the old ITS what program.  For more info, type (what /"CAN YOU DO/")"
  ;;special case check.
  (IF (NUMBERP STRING)
      (SETQ STRING (FORMAT NIL "~D" STRING))) ;make parsable
  (LET ((STANDARD-OUTPUT STREAM))
    (FUNCALL STREAM ':FRESH-LINE)
    (IF (EMPTY? STRING) (SETQ STRING (READLINE-TRIM STREAM)))
    ;;special hacks for special cases.
    (COND ((EMPTY? STRING) ;if its still empty...
           (WHAT-SENDS))
          (T  ;normal case
           (SETQ STRING (STRING-UPCASE (WHAT-CANONICAL-QUESTION STRING)))
           ;hack spelling of unknown words someday
           (*CATCH 'STRING  ;this is what we return
             (PROG ()
                REPEAT
                   (DOLIST (S WHAT-PARSE-STRING-ALIST)
                     (SETQ SEARCH-STRING (STRING-UPCASE (FIRST S)))
                     (SETQ FIND-MODE (SECOND S))
                     (COND ((NULL FIND-MODE) ;find-mode is exactly the same
                            (SETQ FOUND-IT (STRING-EQUAL STRING SEARCH-STRING)))
                           ((EQ FIND-MODE T)  ;if text appears anywhere
                            (SETQ FOUND-IT (STRING-SEARCH SEARCH-STRING STRING)))
                           ((EQ FIND-MODE ':BEGINNING)  ;if text is at the beginning
                            (SETQ L1 (STRING-LENGTH STRING)
                                  L2 (STRING-LENGTH SEARCH-STRING))
                            (COND ((AND ( L1 L2)
                                        (STRING-EQUAL SEARCH-STRING (SUBSTRING STRING 0 L2)))
                                   (SETQ FOUND-IT T)
                                   (IF (= L1 L2)
                                       (SETQ ARGS NIL)  ;got it verbatim, no args
                                     ;;more there, set args to the rest of the string
                                     (SETQ ARGS (SUBSTRING STRING (1+ L2)))))))
                           ((EQ FIND-MODE ':END)  ;if text is at the end.
                            (SETQ L1 (STRING-LENGTH STRING)
                                  L2 (STRING-LENGTH SEARCH-STRING))
                            (COND ((AND ( L1 L2)
                                        (STRING-EQUAL SEARCH-STRING (SUBSTRING STRING (1+ (- L1 L2)))))
                                   (SETQ FOUND-IT T)
                                   (IF (= L1 L2)
                                       (SETQ ARGS NIL) ;its initialize to this, but...
                                     (SETQ ARGS (SUBSTRING STRING 0 (-  L1 L2)))))))
                           ((EQ FIND-MODE ':MATCH)
                            (LET* (($-POS (STRING-SEARCH-CHAR #/$ SEARCH-STRING))
                                   (BEGIN-STRING (SUBSTRING SEARCH-STRING 0 (1- $-POS)))
                                   (END-STRING (SUBSTRING SEARCH-STRING (1+ $-POS)))
                                   (L1 (STRING-LENGTH STRING))
;                                  (L2 (STRING-LENGTH SEARCH-STRING))
                                   (MIDDLE-POS (- L1 (LENGTH END-STRING))))
                              (COND ((AND ( L1 (1+ $-POS)) ;don't blow up
                                                ;;string matches at beginning
                                          (STRING-EQUAL
                                            (SUBSTRING STRING 0 (1- $-POS)) BEGIN-STRING)
                                          ( MIDDLE-POS 0)  ;string is big enough
                                          ;;to match at end
                                          (STRING-EQUAL
                                            (SUBSTRING STRING MIDDLE-POS) END-STRING))
                                     (SETQ FOUND-IT T)))))
                           (T
                            (FORMAT STREAM "~%Sorry, WHAT database broken.  Token is ~A.   ~
Carrying on as best I can." SEARCH-STRING)))
                     (COND ((NULL FOUND-IT)     ;;if this particular match fails
                            (IF (STRING-EQUAL (FIRST S) "*LOSE*") ;and we are all done
                                (*THROW 'STRING (WHAT-LOSER STRING STREAM))) ;finish
                            ) ;otherwise, finish the dolist
                           ;;We have found it.
                           (T
                            (SETQ HOW (THIRD S))
                            (COND ((AND (CONSP HOW) (STRING-EQUAL (CAR HOW) "MAP"))
                                   (LET ((BEGIN-POS (STRING-SEARCH SEARCH-STRING STRING)))
                                     (SETQ STRING
                                           (STRING-APPEND
                                             (SUBSTRING STRING 0 (MAX 0 (1- BEGIN-POS)));.lose
                                             (CDR HOW) ;string to splice in
                                             (SUBSTRING STRING
                                                        (+ BEGIN-POS
                                                           (STRING-LENGTH SEARCH-STRING)))))
                                     (SETQ FOUND-IT NIL)
                                     (GO REPEAT)))) ;;start all over
                            ;;No special case: We have a winner
                            (*THROW 'STRING (WHAT-APPLY STREAM STRING HOW ARGS)))))))))))


  (DEFUN WHAT-APPLY (STREAM STRING HOW ARGS &AUX RESULT DONE)
    "Print the results of what onto stream STREAM based on the method describe in HOW.
See the WHAT-PARSE-STRING-ALIST for further details."
    (IGNORE STRING ARGS STREAM)
    (WITH-OUTPUT-TO-STRING (S)
      (COND ((STRINGP HOW)
             (FORMAT S HOW))
            ((FUNCTIONP HOW)  ;this must come before the listp
             (LET ((STANDARD-OUTPUT S))
               (SETQ RESULT (FUNCALL HOW)) ;forget about args
               (IF (NOT (NULL RESULT))  ;if nil, assume stuff already printed.
                   (FORMAT S "~A" RESULT))))
            ((LISTP HOW)
             (DOLIST (FILE HOW)
               (COND ((PROBEF FILE)
                      (VIEWF FILE S)
                      (SETQ DONE T)))) ;gross
             (IF (NULL DONE)
                 (FORMAT S "I can't answer that right now.")))
          (T
           (FERROR NIL "The what database is broken, token is ~A.") HOW)) ;be more helpful
    S))

(DEFUN WHAT-LOSER (STRING STREAM &AUX N)
  "WHAT lost in parsing the lusers input.  Blow up in his face."
  (IGNORE STREAM)
    ;;special case hacks that I was too lazy to code.
  (COND ((STRING-SEARCH "birthday" STRING)
         (LET ((STRING (STRING-LEFT-TRIM #\SP (STRING-APPEND STRING #\SP)))) ;don't lose
           (PERSONS-BIRTHDAY (SUBSTRING STRING 0 (STRING-SEARCH-CHAR #\SP STRING)) STREAM)))
        ((NOT (NULL (SETQ N (PARSE-NUMBER STRING)))) ;this doesn't catch when spelled out.
         (WHAT-SENDS N))
        (T
         "You tell me.")))  ;if we lose, be clever, and maybe learn (except in ai lab)


(DEFUN WHAT-CANONICAL-QUESTION (STRING)
  "Canonicalize string into a question without the question mark."
  (STRING-RIGHT-TRIM WHAT-PUNCTUATION STRING))

(DEFUN WHAT-PRINT-DEMONS ()
  (DOLIST (P SI:ALL-PROCESSES)
    (FORMAT T "~&Process ~A has arrest reasons: ~A, ~%   run reasons ~A, ~
idle-time ~A, and a whostate of ~A.~%"
            P (SEND P ':ARREST-REASONS) (SEND P ':RUN-REASONS) (SEND P ':IDLE-TIME)
            (SEND P ':WHOSTATE))))


(DEFVAR *WHAT-TO-TEST* '("is this" "is up" "temperature" "temp" "lusers"
                         "time" "hair" "fucking losers" 2.  "randomness")
  "A list of forms that the TEST-WHAT function uses to test the WHAT parser.")

(DEFUN TEST-WHAT (&OPTIONAL ALL-P (STREAM STANDARD-OUTPUT))
  "Test the WHAT parser, by trying out all of the forms on *what-to-test*."
  (DOLIST (TEST (IF ALL-P WHAT-PARSE-STRING-ALIST *WHAT-TO-TEST*))
    (IF ALL-P (SETQ TEST (CAR TEST)))
    (FORMAT STREAM "~2&Testing /"~A/": " TEST)
    (FORMAT STREAM "~A" (WHAT TEST STREAM))))

;;also test gjs's birthay

(DEFVAR WHAT-PARSE-STRING-ALIST
        `(("is this" NIL "It's an all purpose utility program, dummy!")
          ("is what" NIL (:MAP . "is this"))
          ("are you" NIL "I am an omnisceint utility program that can't spell, idiot!")
          ("am i" NIL "You are a luser.")
          ("can you do" NIL ,#'WHAT-CAN-I-DO)
          ("do you think you are" T (:MAP . "are you"))
          ("can i do" NIL "Nothing.  I am all powerful.")
          ("is your name" NIL "Fred Derf.")
          ("is your quest" NIL "I seek the Holy Grail.")
          ("tide" NIL (:MAP . "is the time"))
          ("is at the end of the universe" NIL "A restaurant.")
          ("is the meaning of zen" NIL " ")
          ("time is it" NIL "It's Howdy Doody time!")
          ("is the time" NIL ,#'(LAMBDA () (FORMAT NIL "The time is ~A."
                                                   (TIME:PRINT-UNIVERSAL-TIME
                                                     (TIME:GET-UNIVERSAL-TIME) NIL))))
          ("time" NIL (:MAP . "is the time"))
          ("id" NIL ,#'(LAMBDA () (FORMAT NIL "~A" USER-ID)))
          ("what" NIL "What?")
          ("is the shape of the earth" NIL "Flat.")
          ("happens when you get to the edge" NIL "You fall off.")
          ("for" NIL "It turns me on.")
          ("is new" NIL ("mc:common;_news_ nyt" "ml:common;_news_ nyt")) ;also on oz
          ("is up" NIL "I am up.")
          ("disk errors" NIL ,#'SI:PRINT-DISK-ERROR-LOG)
          ("do you know" nil "Everything.")
          ("version" NIL ,#'(LAMBDA () (LET ((BASE 10.)) (SI:GET-SYSTEM-VERSION))))
          ("brand" NIL ,#'(LAMBDA () (FORMAT STANDARD-OUTPUT
                                       #+MIT "Brand M" #+symbolics "Brand S")))
          ("randomness" NIL ,#'WHAT-RANDOMNESS)
          ("demons" NIL ,#'WHAT-PRINT-DEMONS)
          ("demon" NIL (:MAP . "demons"))
          ("daemon" NIL (:MAP . "demons"))
          ("daemons" NIL (:MAP . "demons"))
          ("bugs" NIL ("oz:src:<l.doc>bugs.txt" "mc:lisp;bug mail"))
          ("losers" NIL ,#'(LAMBDA () (FORMAT NIL "~A" (FINGER)))) ;ass. machine
          ("lusers" NIL (:MAP . "losers"))  ;;there should be a winners list
          ("users" NIL (:MAP . "losers"))
          ("a crock" NIL ,#'(LAMBDA () (HACKS:CROCK)))
          ("rough beast" NIL "Its hour come round at last.
Slouches towards Bethlehem to be born.")
          ("me worry" NIL "Start worrying.") ;;tv:notify ...
          ("is your favorite color" NIL "Yellow... no!  Blue! AIIIIIIEEEEEEE......")
          ("is teco" NIL "It is the only systems programming language more obscure than RPG.")
          ("times" NIL ,#'(LAMBDA () (PROCESS-SLEEP 200. "Connect: MC")                           "MIT-MC not responding.")) ;lazy
          ("day" NIL ,#'(LAMBDA () (TIME:PRINT-UNIVERSAL-DATE (TIME:GET-UNIVERSAL-TIME) T)))
          ("date" NIL (:MAP . "day"))
          ("has four wheels and flies" NIL "A garbage truck.")
          ("hacks" NIL ,#'HACKS:DEMO)
          ("has four legs and files" NIL "Two guys moving an RK02?")
          ("seminar" :BEGINNING ("ml:common;calend log" "oz:src:<hdt.mail>seminars.txt"))
          ("has four wheels and files" NIL "A filing cabinet on casters.")
          ("does it cost to ride the unibus" NIL "Two bits.")
          ("temperature" NIL "Mild spring-type weather under the Fuller dome.
Ditto on the General Technics plaza.")
          ("temp" NIL (:MAP . "temperature"))
          ("a dog" NIL ("mc:.info.;fido order" "ml:.info.;fido order"))
          ("hair" NIL ("mc:.info.;lisp recent" "ml:.info.;lisp recent"))
          ("a kludge" NIL ("sys:demo;what lisp"))
          ("kluge" NIL (:MAP . "kludge")) ;can't they spell only "a kluge" should trap
          ("jargon" NIL ("mc:gls;jargon >"))
          ("is that" NIL "How the heck should I know?")
          ("is the answer" T "42.")
          ("is the question" NIL "If I knew that, then the answer would make sense.")
          ("do you get if you multiply six by nine" NIL "42.")
          ("do you call a seven foot$with a bicycle chain" :MATCH "Sir.")
          ("is the number of the beast" NIL "#o1232")
          ("is the name of this book" NIL "What is the name of this book?")
          ("is the question that is its own answer" NIL "What is the question that is its own answer?")
          ("system" NIL ,#'PRINT-HERALD) ;;loses here
          ("notifications" NIL ,#'PRINT-NOTIFICATIONS)
          ("modifications" NIL ,#'PRINT-SYSTEM-MODIFICATIONS)
          ("patches" NIL (:MAP . "modifications"))
          ("say" NIL (:MAP . "0")) ;hack: print-sends
          ("are you doing" NIL "I'm CONS'ing.
I'll give the garbage collector a fatal error before long.")
          ("me load patches" NIL ,#'LOAD-PATCHES)
          ("sys" T (:MAP . "system"))
          ("newio" T (:MAP . "lisp"))
          ("features" NIL ,#'(LAMBDA () (FORMAT:PRINT-LIST NIL "~S" (STATUS FEATURE))))
          ("is the capital of Outer Mongolia" NIL "Ulan Bator.")
          ("the$are you" :MATCH "You shouldn't insult an omniscent super utility program!")
          ("is the age of the universe" NIL ,#'WHAT-AGE-UNIVERSE)
 ;;put things have a second arg of T after here to avoid conflicts
      ;;parse out (some) swear words   ;;this really doesn't fully work yet. (spaces)
          ("goddam" T (:MAP . ""))
          ("goddamn" T (:MAP . ""))
          ("fucking" T (:MAP . ""))
          ("cretinous" T (:MAP . "")) ;gumby cretinous losers
          ("does ddt type when a job" T ":KILL
")
          ("in the world" T (:MAP . "is this")) ;;wins here
          ("dcrock" T ,#'HACKS:DC-DEMO)
          ("digital crock" T ,#'HACKS:DC-DEMO)
          ("crocks" T "I have a crock and a dcrock.")
          ("is on second" T "I don't know is on third.")
          ("happen" T "Beats me! All I did was let my mask down for six microseconds and
this moby interrupt came and knocked the crap out of me!!")
          ("lisp" T ,#'(LAMBDA () (MULTIPLE-VALUE (A B) (SI:GET-SYSTEM-VERSION))
                               (LET ((BASE 10.) (IBASE 10.) (*NOPOINT NIL))
                                 (FORMAT STANDARD-OUTPUT "This is System ~D.~D" A B))))
          ("the" :BEGINNING "That's not nice.")  ;hmm.
          ("is" T (:MAP . "")) ;try parsing out if we lose.
          ;;This is the last element, changing it will break the parser
          ("*LOSE*" NIL "You tell me.") ;just in case.
          )
  "An alist used by what-parse to determine what response the what program should
give to something.

Each entry should be in the following form: STRING FIND-MODE WHAT-TO-DO
STRING is the string to look for in the stuff that what tries to parse.

FIND-MODE if NIL says that we must get this string verbatim, if T says
 accept it if it can be found anywhere in the string we are parsing.
 Similarly, ':beginning and ':end mean consider this a match if the
 verbatim STRING exists at the, respectively, beginning or end of what what is parsing.

WHAT-TO-DO can be a string to print out as a result, or a function to be called
instead (if the find-mode was not NIL, then try carefully to send this function
whatever we can figure out that its arguments should be.  If what-to-do is a list,
then try to view the first one of those files that we can detect exists, else
claim that we don't know now.  Actually, the car of the list is checked to see if
it is the token ':map, if so we try reparsing again, by replacing the string
found with the cdr of the list.  You must use T as the searching mode is
':MAP, otherwise it will very rarely trigger. If the token is ':match, then look for the
$ in the string we are searching for.  Match if the string is the same string
as the string with the $ in it, except allow *anything* in place of the $.
You probably don't want to have spaces surrounding the $.
The way to get something to be ignored is to use the :MAP option to turn it into the
empty string.  [This doesn't completely work, because of the extra space.]

Instead of munging this list, you may prefer the ADD-WHAT function.")


(COMMENT ;;; These old carry-overs from ITS could not be implemented:
;; console, tty, its, load, busses, bus.

;These have not yet been implemented:

movie  ;connect xx lsc
movies
BINARY
dir
dirs
)
 ;;end
