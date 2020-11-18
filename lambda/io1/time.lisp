;;; Date and time routines -*- Mode:LISP; Package:TIME; Readtable:ZL; Base:10 -*-
;;;     ** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; Note: days and months are kept one-based throughout, as much as possible.
;;; Days of the week are zero-based on Monday.

;;; [Maybe this should have a global variable which causes it to use AM/PM in place
;;;  of 24-hour time, in all relevant functions?]
;; this should probably have variable which is the initial-year,
;; in case we want more precision.


(DEFINE-SITE-VARIABLE *TIMEZONE* :TIMEZONE "The timezone.")

;; these should probably be site variables too
(DEFVAR *DAYLIGHT-SAVINGS-TIME-P-FUNCTION* 'daylight-savings-time-in-usa-p
  "A function, which when applied to arguments of seconds minutes hours day month year,
will return T if daylight savings time is in effect in the local timezone at that time.")

;>> Changed from :mm//dd//yy 18-Jul-85 Mly.  I can't stand the backwards confusing notation!
(DEFVAR *DEFAULT-DATE-PRINT-MODE* :DD-MMM-YY
  "Defines the default way to print the date. Possible values include:
:DD//MM//YY :MM//DD//YY :DD-MM-YY :DD-MMM-YY :|DD MMM YY| :DDMMMYY :YYMMDD :YYMMMDD
 and similar keywords with YYYY instead of YY.")

(DEFUN MICROSECOND-TIME (&AUX (INHIBIT-SCHEDULING-FLAG T))
  "Return the current value of the microsecond clock (a bignum).
Only differences in clock values are meaningful.
There are 32. bits of data, so the value wraps around every few hours."
  (SELECT-PROCESSOR
    (:CADR
     (LET ((LOW (%UNIBUS-READ #o764120))  ;Hardware synchronizes if you read this one first
           (HIGH (%UNIBUS-READ #o764122)))
       (DPB HIGH (BYTE 16. 16.) LOW)))
    (:LAMBDA
     (%MICROSECOND-TIME))
    (:explorer
      (%microsecond-time))
    ))

(DEFUN FIXNUM-MICROSECOND-TIME (&AUX (INHIBIT-SCHEDULING-FLAG T))
  "Return the current value of the microsecond clock as two fixnums."
  (DECLARE (VALUES LOW-23-BITS TOP-9-BITS))
  (SELECT-PROCESSOR
    (:CADR
     (LET ((LOW (%UNIBUS-READ #o764120))
           (HIGH (%UNIBUS-READ #o764122)))
       (VALUES (DPB HIGH (BYTE 7 16.) LOW) (LDB (BYTE 9. 7) HIGH))))
    (:LAMBDA
     (LET ((TIME (%MICROSECOND-TIME)))
       (VALUES (LDB (BYTE 23. 0) TIME) (LDB (BYTE 9. 23.) TIME))))
    (:explorer
     (LET ((TIME (%MICROSECOND-TIME)))
       (VALUES (LDB (BYTE 23. 0) TIME) (LDB (BYTE 9. 23.) TIME))))
    ))

(DEFCONST INTERNAL-TIME-UNITS-PER-SECOND 60.
  "60 60ths of a second in a second.")

(DEFVAR HIGH-TIME-BITS 0
  "Number of times (TIME) has wrapped around since booting.")

(DEFVAR WAS-NEGATIVE NIL
  "T if (TIME) was TIME-LESSP than LAST-BOOT-TIME when last checked.
Each this changes from T to NIL, (TIME) has wrapped around once.")

(DEFVAR LAST-BOOT-TIME 0
  "Value of (TIME) when machine was booted.")

(DEFF GET-INTERNAL-REAL-TIME 'GET-INTERNAL-RUN-TIME)
(DEFUN GET-INTERNAL-RUN-TIME ()
  "Returns time in 60'ths since last boot. May be a bignum."
  (LET ((TIME-DIFF (%POINTER-DIFFERENCE (TIME) LAST-BOOT-TIME)))
    (WHEN (AND (PROG1 WAS-NEGATIVE
                      (SETQ WAS-NEGATIVE (LDB-TEST (BYTE 1 22.) TIME-DIFF)))
               (NOT WAS-NEGATIVE))
      (INCF HIGH-TIME-BITS))
    (DPB HIGH-TIME-BITS (BYTE 23. 23.) (LDB (BYTE 23. 0) TIME-DIFF))))


;;;; Conversion routines, universal time is seconds since 1-jan-00 00:00-GMT

(DEFINE-SITE-VARIABLE *TIMEZONE* :TIMEZONE "The timezone.")

(DEFVAR *CUMULATIVE-MONTH-DAYS-TABLE*
        (MAKE-ARRAY 13. :TYPE 'ART-16B
                        :INITIAL-CONTENTS '#10r(0   0   31  59  90  120 151
                                                    181 212 243 273 304 334))
  "One-based array of cumulative days per month.")

;;; Takes Univeral Time (seconds since 1/1/1900) as a 32-bit number
;;; Algorithm from KLH's TIMRTS.
(DEFUN DECODE-UNIVERSAL-TIME (UNIVERSAL-TIME &OPTIONAL TIMEZONE
                                             &AUX SECS MINUTES HOURS DAY MONTH YEAR
                                                  DAY-OF-THE-WEEK DAYLIGHT-SAVINGS-TIME-P)
  "Given a UNIVERSAL-TIME, decode it into year, month number, day of month, etc.
TIMEZONE is hours before GMT (5, for EST).
DAY and MONTH are origin-1.  DAY-OF-THE-WEEK = 0 for Monday."
  (DECLARE (VALUES SECS MINUTES HOURS DAY MONTH YEAR
                   DAY-OF-THE-WEEK DAYLIGHT-SAVINGS-TIME-P TIMEZONE))
  (IF TIMEZONE                                  ;explicit timezone means no-dst
      (MULTIPLE-VALUE (SECS MINUTES HOURS DAY MONTH YEAR DAY-OF-THE-WEEK)
         (DECODE-UNIVERSAL-TIME-WITHOUT-DST UNIVERSAL-TIME TIMEZONE))
    ;;Otherwise, decode the time and THEN daylight-adjust it.
    (MULTIPLE-VALUE (SECS MINUTES HOURS DAY MONTH YEAR DAY-OF-THE-WEEK)
      (DECODE-UNIVERSAL-TIME-WITHOUT-DST UNIVERSAL-TIME *TIMEZONE*))
    (AND (SETQ DAYLIGHT-SAVINGS-TIME-P
               (FUNCALL *DAYLIGHT-SAVINGS-TIME-P-FUNCTION*
                        SECS MINUTES HOURS DAY MONTH YEAR))
         ;; See if it's daylight savings time, time-zone number gets smaller if so.
         (MULTIPLE-VALUE (SECS MINUTES HOURS DAY MONTH YEAR DAY-OF-THE-WEEK)
           (DECODE-UNIVERSAL-TIME-WITHOUT-DST UNIVERSAL-TIME (1- *TIMEZONE*)))))
  (VALUES SECS MINUTES HOURS DAY MONTH YEAR DAY-OF-THE-WEEK DAYLIGHT-SAVINGS-TIME-P
          (OR TIMEZONE *TIMEZONE*)))

(DEFUN DECODE-UNIVERSAL-TIME-WITHOUT-DST (UNIVERSAL-TIME &OPTIONAL (TIMEZONE *TIMEZONE*)
                                          &AUX X SECS MINUTES HOURS DAY MONTH YEAR)
  "Like DECODE-UNIVERSAL-TIME, but always uses standard time.
Even if the time is one at which daylight savings time would be in effect,
the hour and date are computed as for standard time."
  (DECLARE (VALUES SECS MINUTES HOURS DAY MONTH YEAR DAY-OF-THE-WEEK TIMEZONE))
  (SETQ UNIVERSAL-TIME (- UNIVERSAL-TIME (* TIMEZONE 3600.)))
  (SETQ SECS (CL:REM UNIVERSAL-TIME (* 24. 60. 60.))
        X (TRUNCATE UNIVERSAL-TIME (* 24. 60. 60.)))    ;Days since genesis.
  (MULTIPLE-VALUE-BIND (A B) (FLOOR X 365.)
    (UNLESS (ZEROP A)
      (DECF B (LSH (1- A) -2))
      (WHEN (< B 0)
        (SETQ A (+ A -1 (TRUNCATE B 365.)))     ;We must allow for times so far in the future
        (SETQ B (CL:REM B 365.))                        ;as to produce >> 365. Feb 29's.
        (SETQ B (+ B 365.))                     ;(Of course, this doesn't allow for
                                                ;the year 2100 not being a leap-year.)
        (AND (NOT (BIT-TEST A 3))
             (INCF B))))
    (DO ((C 12. (1- C)))
        (( B (AREF *CUMULATIVE-MONTH-DAYS-TABLE* C))
         (WHEN (AND (NOT (BIT-TEST A 3))
                    (> C 2))
           (DECF B)
           (IF (< B (AREF *CUMULATIVE-MONTH-DAYS-TABLE* C)) (DECF C))
           (IF (= C 2) (INCF B)))
         (DECF B (AREF *CUMULATIVE-MONTH-DAYS-TABLE* C))
         (SETQ YEAR (+ 1900. A))
         (SETQ MONTH C)
         (SETQ DAY (1+ B)))))
  (SETQ HOURS (FLOOR SECS 3600.)
        MINUTES (FLOOR (CL:REM SECS 3600.) 60.)
        SECS (CL:REM SECS 60.))
  (VALUES SECS MINUTES HOURS DAY MONTH YEAR (CL:REM X 7) TIMEZONE))

(DEFUN DAYLIGHT-SAVINGS-TIME-P (&REST ARGS)
  "T if daylight savings time would be in effect at specified time in the local timezone."
  (DECLARE (ARGLIST HOURS DAY MONTH YEAR))
  (APPLY *DAYLIGHT-SAVINGS-TIME-P-FUNCTION* 0 0 ARGS))

(DEFUN YYYY-YY (YEAR CURRENT-YEAR)
  (IF (AND CURRENT-YEAR
          (= (TRUNCATE (+ YEAR 50.) 100.) (TRUNCATE (+ CURRENT-YEAR 50.) 100.)))
      (MOD YEAR 100.)
    YEAR))

(DEFUN ENCODE-UNIVERSAL-TIME (SECONDS MINUTES HOURS DAY MONTH YEAR
                              &OPTIONAL TIMEZONE &AUX TEM goodyear)
  "Given a time, return a universal-time encoding of it.
A universal-time is the number of seconds since 1-Jan-1900 00:00-GMT (a bignum)."
  (IF (< YEAR 100.)
      (LET ((CURRENT-YEAR (or (NTH-VALUE 5 (GET-DECODED-TIME))
                              2000))) ;In case called during startup or during DISK-SAVE.
        (SETQ YEAR
              (+ CURRENT-YEAR
                 (- (MOD (+ 50. (- YEAR (CL:REM CURRENT-YEAR 100.))) 100.) 50.)))))
  (setq goodyear year)
  (SETQ YEAR (- YEAR 1900.))
  (OR TIMEZONE
      (SETQ TIMEZONE (IF (DAYLIGHT-SAVINGS-TIME-P HOURS DAY MONTH goodyear)
                         (1- *TIMEZONE*) *TIMEZONE*)))
  (SETQ TEM (+ (1- DAY) (AREF *CUMULATIVE-MONTH-DAYS-TABLE* MONTH)
               (FLOOR (1- YEAR) 4) (* YEAR 365.)))      ;Number of days since 1-Jan-1900.
  (AND (> MONTH 2) (LEAP-YEAR-P goodyear)
       (SETQ TEM (1+ TEM)))                             ;After 29-Feb in a leap year.
  (+ SECONDS (* 60. MINUTES) (* 3600. HOURS) (* TEM (* 60. 60. 24.)) (* TIMEZONE 3600.)))


;;;; domain-dependent knowledge
(DEFUN DAYLIGHT-SAVINGS-TIME-IN-NORTH-AMERICA-P (SECONDS MINUTES HOURS DAY MONTH YEAR)
  "T if daylight savings time would be in effect at specified time in North America."
  (DECLARE (IGNORE SECONDS MINUTES))
  (COND ((OR (< MONTH 4)                ;Standard time if before 2 am last Sunday in April
             (AND (= MONTH 4)
                  (LET ((LSA (LAST-SUNDAY-IN-APRIL YEAR)))
                    (OR (< DAY LSA)
                        (AND (= DAY LSA) (< HOURS 2))))))
         NIL)
        ((OR (> MONTH 10.)              ;Standard time if after 1 am last Sunday in October
             (AND (= MONTH 10.)
                  (LET ((LSO (LAST-SUNDAY-IN-OCTOBER YEAR)))
                    (OR (> DAY LSO)
                        (AND (= DAY LSO) ( HOURS 1))))))
         NIL)
        (T T)))

(defun daylight-savings-time-in-usa-p (seconds minutes hours day month year)
  (declare (ignore seconds minutes))
  ;;; remember, HOURS here is always in standard time.
  (cond ((< month 4) nil)                       ; before April
        ((> month 10.) nil)                     ; after October
        ((= month 4)
         (let ((magic-day (cond ((< year 1987.) (last-sunday-in-april year))
                                ((>= year 1987.) (first-sunday-in-april year)))))
           (cond ((< day magic-day) nil)
                 ((> day magic-day) t)
                 ((>= hours 2) t)
                 (t nil))))
        ((= month 10.)
         (let ((magic-day (last-sunday-in-october year)))
           (cond ((< day magic-day) t)
                 ((> day magic-day) nil)
                 ((>= hours 1) nil)
                 (t t))))
        (t t)))

(defun day-of-week-first-day-of-year (year)
  ;;; Sunday is 0
  (flet ((n-leap-days (year)
           (setq year (1- year))                        ; don't count this year yet
           (let ((year-div-4 (floor year 4))            ; extra day every 4,
                 (year-div-100 (- (floor year 100.)))   ;   except every 100,
                 (year-div-400 (floor year 400.)))      ;     except every 400.
             (+ year-div-4 year-div-100 year-div-400))))
    (let* ((base-year 1900.)
           (base-day 1)                         ; 1900 began on a Monday
           (leap-days-since-base (- (n-leap-days year) (n-leap-days base-year)))
           (non-leap-days-since-base (* 365. (- year base-year))))
      (cl:rem (+ base-day non-leap-days-since-base leap-days-since-base)
              7))))

;;; incomprehensible ITS implementation
;(defun day-of-week-first-day-of-year (year)
;  ;;; Friday is zero
;  (let ((b (cl:rem (+ year 1899.) 400.)))
;     (cl:rem (- (+ (1+ b) (setq b (floor b 4))) (floor b 25.)) 7)))

(defun first-sunday-in-april (year)
  (let* ((day-year-started-on (day-of-week-first-day-of-year year))
         (feb29 (if (leap-year-p year) 1 0))
         (days-before-april (+ 31 28 feb29 31))
         (day-of-week-april-first (cl:rem (+ day-year-started-on days-before-april) 7)) )
    (cl:rem (- 7 day-of-week-april-first) 7)))

(DEFUN LAST-SUNDAY-IN-OCTOBER (YEAR)
  (LET ((LSA (LAST-SUNDAY-IN-APRIL YEAR)))
    ;; Days between April and October = 31+30+31+31+30 = 153  6 mod 7
    ;; Therefore the last Sunday in October is one less than the last Sunday in April
    ;; unless that gives 24. or 23. in which case it is six greater.
    (IF ( LSA 25.) (+ LSA 6) (1- LSA))))

(DEFUN LAST-SUNDAY-IN-APRIL (YEAR)
  (IF (> YEAR 100.)
      (SETQ YEAR (- YEAR 1900.)))
  ;; This copied from GDWOBY routine in ITS
  (LET ((DOW-BEG-YEAR
          (LET ((B (CL:REM (+ YEAR 1899.) 400.)))
            (CL:REM (- (+ (1+ B) (SETQ B (FLOOR B 4))) (FLOOR B 25.)) 7)))
        (FEB29 (IF (LEAP-YEAR-P YEAR) 1 0)))
    (LET ((DOW-APRIL-30 (CL:REM (+ DOW-BEG-YEAR 119. FEB29) 7)))
      (- 30. DOW-APRIL-30))))

;;;; Maintenance functions

(DEFVAR *LAST-TIME-UPDATE-TIME* NIL
  "A number representing the universal time of the last update time.")
(DEFVAR PREVIOUS-TOP-9-TIME-BITS NIL)
(DEFVAR *LAST-TIME-SECONDS* NIL "A number representing the seconds of the last update time.")
(DEFVAR *LAST-TIME-MINUTES* NIL "A number representing the minutes of the last update time.")
(DEFVAR *LAST-TIME-HOURS* NIL "A number representing the hours of the last update time.")
(DEFVAR *LAST-TIME-DAY* NIL "A number representing the day of the last update time.")
(DEFVAR *LAST-TIME-MONTH* NIL "A number representing the month of the last update time.")
(DEFVAR *LAST-TIME-YEAR* NIL "A number representing the year of the last update time.")
(DEFVAR *LAST-TIME-DAY-OF-THE-WEEK* NIL
  "A number representing the day of the week of the last update time.")
(DEFVAR *LAST-TIME-DAYLIGHT-SAVINGS-P* NIL "Whether it was DST the last update time.")
(DEFVAR *NETWORK-TIME-FUNCTION* NIL)
(DEFVAR *UT-AT-BOOT-TIME* NIL "Used for UPTIME protocol, do not random SETQ.")

(defun unix-processor-present ()
  (dolist (op si:*other-processors*)
    (cond ((= (si:%processor-conf-processor-type (si:op-proc-conf op)) 2)
           (return t)))))

;This does notifies because world isnt really initialized sufficiently to win at
; warm-initialization-list time.  We'll have to see if people like this.
(DEFUN INITIALIZE-TIMEBASE (&OPTIONAL UT always-ask suppress-notify &AUX SOURCE-HOST)
  "Set the clock.
Possible sources of the time include the network, the Lambda SDU clock, SIB Clock (Explorer),
and the local user.  Will notify where time gotten from unless suppressed."
  (TAGBODY
      (if always-ask (go string))
      (COND ((AND (NULL UT) (NOT (SI:GET-SITE-OPTION :STANDALONE)) *NETWORK-TIME-FUNCTION*)
             (MULTIPLE-VALUE (UT SOURCE-HOST)
               (FUNCALL *NETWORK-TIME-FUNCTION*))
             (COND ((AND (NULL SUPPRESS-NOTIFY) (FBOUNDP 'TV:NOTIFY) (NUMBERP UT) SOURCE-HOST)
                    (TV:NOTIFY NIL "Time from host ~A is ~VQ." SOURCE-HOST UT 'PRINT-UNIVERSAL-TIME)))))
      (AND (NUMBERP UT) (GO DO-IT))
      (select-processor
        (:lambda
          (SETQ UT (AND (NULL SI:*IN-COLD-LOAD-P*) ; don't deal with SDU clock until cold load is done ...
;                       (NULL (UNIX-PROCESSOR-PRESENT))  ;It sets up SDU clock differently, let it have it.
                        (NULL (when (fboundp 'unix:version-7-unix-p)
                                (UNIX:VERSION-7-UNIX-P)))
                        (RTC-GET-UNIVERSAL-TIME)))
          (COND ((AND (NULL SUPPRESS-NOTIFY)
                      (FBOUNDP 'TV:NOTIFY)
                      UT)
                 (TV:NOTIFY NIL "Time from SDU clock is ~VQ." UT 'PRINT-UNIVERSAL-TIME)))
          (COND (UT (GO DO-IT))))    ; change to GIVE-IT-A-SHOT if the user should be asked
        (:explorer
          (when (explorer-initial-date-valid-p)
            (setq ut (explorer-get-universal-time))
            (When (And (Not suppress-notify) (FBoundP 'TV:NOTIFY) (Not (Null ut)))
              (tv:Notify nil "Time from SIB clock is ~VQ." ut 'print-universal-time))
            (go do-it)))
        (:cadr))
   STRING
      (FORMAT *QUERY-IO* "~&Please type the date and time: ")
      (SETQ UT (READLINE *QUERY-IO*))
      (WHEN (STRING-EQUAL UT "")
        (IF (Y-OR-N-P "Do you want to specify the time or not? ")
            (GO STRING)
          (SETQ *LAST-TIME-UPDATE-TIME* NIL)
          (RETURN-FROM INITIALIZE-TIMEBASE NIL)))
      (CONDITION-CASE (ERROR)
          (SETQ UT (PARSE-UNIVERSAL-TIME UT 0 NIL T 0))
        (ERROR (SEND ERROR :REPORT *QUERY-IO*)
               (GO STRING)))
   GIVE-IT-A-SHOT
      (UNLESS (Y-OR-N-P (FORMAT NIL "Time is ~A, OK? " (PRINT-UNIVERSAL-DATE UT NIL)))
        (GO STRING))
   DO-IT
      (WITHOUT-INTERRUPTS
        (IF (NOT (NULL *UT-AT-BOOT-TIME*))
            ;;if we are randomly changing the time while up, mung uptime
            (SETQ *UT-AT-BOOT-TIME*
                  (+ *UT-AT-BOOT-TIME* (- UT (GET-UNIVERSAL-TIME))))
          ;;no real surprise: changing at boot time
          (SETQ *UT-AT-BOOT-TIME* UT))
        (SETQ *LAST-TIME-UPDATE-TIME* (TIME))
        (MULTIPLE-VALUE (*LAST-TIME-SECONDS* *LAST-TIME-MINUTES* *LAST-TIME-HOURS*
                         *LAST-TIME-DAY* *LAST-TIME-MONTH* *LAST-TIME-YEAR*
                         *LAST-TIME-DAY-OF-THE-WEEK* *LAST-TIME-DAYLIGHT-SAVINGS-P*)
          (DECODE-UNIVERSAL-TIME UT))
        (select-processor
          (:lambda
            (Unless (unix-processor-present)
              (RTC-SET-UNIVERSAL-TIME UT)))
          (:explorer
            (explorer-set-universal-time UT))
          (:cadr))
        (RETURN-FROM INITIALIZE-TIMEBASE T))))


(defun set-local-time (&optional new-time)
  (when (stringp new-time)
    (condition-case (error)
        (setq new-time (parse-universal-time new-time 0 nil t 0))
      (error (send error :report *query-io*)
             (setq new-time nil))))
  (if new-time
      (let ((*network-time-function* nil))
        (initialize-timebase new-time))
    (initialize-timebase nil t)))

;; This is so freshly booted machines don't give out an incorrect time or uptime until
;; they've found out for themselves what the time *really* is.
(ADD-INITIALIZATION "Forget time" '(SETQ TIME:*LAST-TIME-UPDATE-TIME* NIL) '(BEFORE-COLD))
(ADD-INITIALIZATION "Forget uptime" '(SETQ TIME:*UT-AT-BOOT-TIME* NIL) '(BEFORE-COLD))

(DEFUN UPDATE-TIMEBASE ()
  "Update our information on the current time."
  (WHEN (NOT (NULL *LAST-TIME-UPDATE-TIME*))
    ;; The scheduler calls this indirectly through the status-line updater.
    (WITHOUT-INTERRUPTS
      (LET* ((SIXTY 60.)
             (TIME (TIME))
             (TICKS (TRUNCATE (TIME-DIFFERENCE TIME *LAST-TIME-UPDATE-TIME*) SIXTY))
             (PREVIOUS-HOUR *LAST-TIME-HOURS*))
        (SETQ *LAST-TIME-UPDATE-TIME* (TIME-INCREMENT *LAST-TIME-UPDATE-TIME*
                                                      (* TICKS SIXTY)))
        (OR (< (INCF *LAST-TIME-SECONDS* TICKS) SIXTY)
            (< (MULTIPLE-VALUE-BIND (QUOTIENT REMAINDER)
                   (TRUNCATE *LAST-TIME-SECONDS* SIXTY)
                 (SETQ *LAST-TIME-SECONDS* REMAINDER)
                 (INCF *LAST-TIME-MINUTES* QUOTIENT))
               SIXTY)
            (< (MULTIPLE-VALUE-BIND (QUOTIENT REMAINDER)
                   (TRUNCATE *LAST-TIME-MINUTES* SIXTY)
                 (SETQ *LAST-TIME-MINUTES* REMAINDER)
                 (INCF *LAST-TIME-HOURS* QUOTIENT))
               24.)
            ( (PROG1 (INCF *LAST-TIME-DAY*)
                      (SETQ *LAST-TIME-DAY-OF-THE-WEEK*
                            (CL:REM (1+ *LAST-TIME-DAY-OF-THE-WEEK*) 7))
                      (SETQ *LAST-TIME-HOURS* 0))
               (MONTH-LENGTH *LAST-TIME-MONTH* *LAST-TIME-YEAR*))
            ( (SETQ *LAST-TIME-DAY* 1
                     *LAST-TIME-MONTH* (1+ *LAST-TIME-MONTH*))
               12.)
            (SETQ *LAST-TIME-MONTH* 1
                  *LAST-TIME-YEAR* (1+ *LAST-TIME-YEAR*)))
        (WHEN ( PREVIOUS-HOUR *LAST-TIME-HOURS*)
          ;; If hour has incremented, turn decoded time into a UT
          ;; using the timezone we were using up to now,
          ;; use that to decide if we have turned DST on or off,
          ;; and then re-decode the time.
          (MULTIPLE-VALUE (*LAST-TIME-SECONDS* *LAST-TIME-MINUTES* *LAST-TIME-HOURS*
                           *LAST-TIME-DAY* *LAST-TIME-MONTH* *LAST-TIME-YEAR*
                           *LAST-TIME-DAY-OF-THE-WEEK* *LAST-TIME-DAYLIGHT-SAVINGS-P*)
            (DECODE-UNIVERSAL-TIME
              (ENCODE-UNIVERSAL-TIME
                *LAST-TIME-SECONDS* *LAST-TIME-MINUTES* *LAST-TIME-HOURS*
                *LAST-TIME-DAY* *LAST-TIME-MONTH* *LAST-TIME-YEAR*
                (IF *LAST-TIME-DAYLIGHT-SAVINGS-P*
                    (1- *TIMEZONE*)
                  *TIMEZONE*))))
          ;; Update things for GET-INTERNAL-RUN-TIME at least once an hour.
          (GET-INTERNAL-RUN-TIME)))
      T)))

(DEFVAR *MONTH-LENGTHS* '#10r(0 31 28 31 30 31 30 31 31 30 31 30 31)
  "One-based list of lengths of months.")

(DEFUN MONTH-LENGTH (MONTH YEAR)
  "Return the number of days in month MONTH in year YEAR.
Knows about leap years.  January is month 1."
  (IF (= MONTH 2)
      (IF (LEAP-YEAR-P YEAR) 29. 28.)
    (NTH MONTH *MONTH-LENGTHS*)))

(DEFUN LEAP-YEAR-P (YEAR)                       ;2000 is a leap year.  2100 is not.
  "T if YEAR is a leap year."
  (IF (< YEAR 100.)
      (SETQ YEAR (+ 1900. YEAR)))
  (AND (ZEROP (CL:REM YEAR 4))
       (OR (NOT (ZEROP (CL:REM YEAR 100.)))
           (ZEROP (CL:REM YEAR 400.)))))

(DEFUN DAYLIGHT-SAVINGS-P ()
  "T if we are now in daylight savings time."
  (UPDATE-TIMEBASE)
  *LAST-TIME-DAYLIGHT-SAVINGS-P*)

(DEFUN DEFAULT-YEAR ()
  "Return the current year, minus 1900."
  (UPDATE-TIMEBASE)
  *LAST-TIME-YEAR*)

;;; These are the functions the user should call
;;; If they can't find out what time it is, they return NIL
(DEFF GET-DECODED-TIME 'GET-TIME)
(DEFUN GET-TIME ()
  "Return the current time, decoded into second, hour, day, etc.
Returns NIL if the time is not known (during startup or DISK-SAVE)."
  (DECLARE (VALUES SECONDS MINUTES HOURS DAY MONTH YEAR DAY-OF-THE-WEEK
                   DAYLIGHT-SAVINGS-P TIMEZONE))
  (AND (UPDATE-TIMEBASE)
       (VALUES *LAST-TIME-SECONDS* *LAST-TIME-MINUTES* *LAST-TIME-HOURS*
               *LAST-TIME-DAY* *LAST-TIME-MONTH*
               *LAST-TIME-YEAR*
               *LAST-TIME-DAY-OF-THE-WEEK* *LAST-TIME-DAYLIGHT-SAVINGS-P*
               *TIMEZONE*)))

(DEFUN GET-UNIVERSAL-TIME ()
  "Return the current time as a universal-time.
A universal-time is the number of seconds since 01-Jan-1900 00:00-GMT (a bignum)"
  (UPDATE-TIMEBASE)
  (ENCODE-UNIVERSAL-TIME *LAST-TIME-SECONDS* *LAST-TIME-MINUTES* *LAST-TIME-HOURS*
                         *LAST-TIME-DAY* *LAST-TIME-MONTH* *LAST-TIME-YEAR*
                         (IF *LAST-TIME-DAYLIGHT-SAVINGS-P*
                             (1- *TIMEZONE*) *TIMEZONE*)))


;;;args to format: DAY MONTH MONTH-STRING DONT-PRINT-YEAR-P YEAR2 YEAR4
;;;                0   1     2            3                 4     5
(DEFPROP :DD//MM//YY "~D//~2,'0D~*~:[//~2,'0D~]" DATE-FORMAT)           ;27/10{/66}
(DEFPROP :DD//MM//YYYY "~D//~2,'0D~*~:[//~*~D~]" DATE-FORMAT)           ;27/10{/1966}
(DEFPROP :MM//DD//YY "~*~D//~0@*~2,'0D~2*~:[//~2,'0D~]" DATE-FORMAT)    ;10/27{/66}
(DEFPROP :MM//DD//YYYY "~*~D//~0@*~2,'0D~2*~:[//~*~D~]" DATE-FORMAT)    ;10/27{/1966}
(DEFPROP :DD-MM-YY "~D-~2,'0D~*~:[-~2,'0D~]" DATE-FORMAT)               ;27-10{-66}
(DEFPROP :DD-MM-YYYY "~D-~2,'0D~*~:[-~*~D~]" DATE-FORMAT)               ;27-10{-1966}
(DEFPROP :DD-MMM-YY "~D-~*~A~:[-~2,'0D~]" DATE-FORMAT)                  ;27-Oct{-66}
(DEFPROP :DD-MMM-YYYY "~D-~*~A~:[-~*~D~]" DATE-FORMAT)                  ;27-Oct{-1966}
(DEFPROP :DD/ MMM/ YY "~D ~*~A~:[ ~2,'0D~]" DATE-FORMAT)                ;27 Oct{ 66}
(DEFPROP :DD/ MMM/ YYYY "~D ~*~A~:[ ~*~D~]" DATE-FORMAT)                ;27 Oct{ 1966}
(DEFPROP :DDMMMYY "~D~*~A~:[~2,'0D~]" DATE-FORMAT)                      ;27Oct{66}
(DEFPROP :DDMMMYYYY "~D~*~A~:[~*~D~]" DATE-FORMAT)                      ;27Oct{1966}
(DEFPROP :YYMMDD "~4*~2,'0D~1@*~2,'0D~0@*~2,'0D" DATE-FORMAT)           ;661027
(DEFPROP :YYYYMMDD "~5*~2,'0D~1@*~2,'0D~0@*~2,'0D" DATE-FORMAT)         ;19661027
(DEFPROP :YYMMMDD "~3*~:[~2,'0D~]~2@*~A~0@*~2,'0D" DATE-FORMAT)         ;{66}Oct27
(DEFPROP :YYYYMMMDD "~3*~:[~*~D~]~2@*~A~0@*~2,'0D" DATE-FORMAT)         ;{1966}Oct27
(DEFPROP :YY-MMM-DD "~3*~:[~2,'0D-~]~2@*~A-~0@*~2,'0D" DATE-FORMAT)     ;{66-}Oct-27
(DEFPROP :YYYY-MMM-DD "~3*~:[~*~D-~]~2@*~A-~0@*~2,'0D" DATE-FORMAT)     ;{1966-}Oct-27
(DEFPROP :YY-MM-DD "~3*~:[~2,'0D-~]~1@*~A-~0@*~2,'0D" DATE-FORMAT)      ;{66-}10-27
(DEFPROP :YYYY-MM-DD "~3*~:[~*~D-~]~1@*~A-~0@*~2,'0D" DATE-FORMAT)      ;{1966-}10-27

(deftype date-format ()
  `(satisfies valid-date-print-mode-p))

(defun valid-date-print-mode-p (symbol)
  (and (keywordp symbol)
       (stringp (get symbol 'date-format))))

(defun get-date-print-mode (mode)
  (check-type mode date-format
              "a valid date-print-mode
   (a keyword with a TIME::DATE-FORMAT property, such as :DD-MMM-YY)")
  (get mode 'date-format))

(DEFUN PRINT-CURRENT-TIME (&OPTIONAL (STREAM *STANDARD-OUTPUT*)
                                     (DATE-PRINT-MODE *DEFAULT-DATE-PRINT-MODE*))
  "Print the current time on STREAM."
  (AND (UPDATE-TIMEBASE)
       (MULTIPLE-VALUE-BIND (SECONDS MINUTES HOURS DAY MONTH YEAR)
           (GET-TIME)
         (PRINT-TIME SECONDS MINUTES HOURS DAY MONTH YEAR STREAM DATE-PRINT-MODE))))

(DEFUN PRINT-UNIVERSAL-TIME (UT &OPTIONAL (STREAM *STANDARD-OUTPUT*)
                                          TIMEZONE
                                          (DATE-PRINT-MODE *DEFAULT-DATE-PRINT-MODE*))
  "Print the universal-time UT on STREAM, interpreting for time zone TIMEZONE.
TIMEZONE is the number of hours earlier than GMT."
  ;;Let DECODE-UNIVERSAL-TIME default the timezone if wanted, as that fcn
  ;;must know to suppress DST iff TIMEZONE is supplied.
  (MULTIPLE-VALUE-BIND (SECONDS MINUTES HOURS DAY MONTH YEAR)
      (DECODE-UNIVERSAL-TIME UT TIMEZONE)
    (PRINT-TIME SECONDS MINUTES HOURS DAY MONTH YEAR STREAM DATE-PRINT-MODE)))

(DEFUN PRINT-TIME (SECONDS MINUTES HOURS DAY MONTH YEAR
                   &OPTIONAL (STREAM *STANDARD-OUTPUT*)
                             (DATE-PRINT-MODE *DEFAULT-DATE-PRINT-MODE*))
  "Print time specified on STREAM using date format DATE-PRINT-MODE.
If STREAM is NIL, construct and return a string."
  (WITH-STACK-LIST (DATE-MODE-ARGS DAY MONTH (MONTH-STRING MONTH :SHORT)
                                   NIL (YYYY-YY YEAR (NTH-VALUE 5 (GET-DECODED-TIME))) YEAR)
    (FORMAT STREAM "~? ~2,'0D:~2,'0D:~2,'0D"
            (get-date-print-mode date-print-mode)
            DATE-MODE-ARGS
            HOURS MINUTES SECONDS)))

(DEFUN PRINT-CURRENT-DATE (&OPTIONAL (STREAM *STANDARD-OUTPUT*))
  "Print the current date in a verbose form on STREAM.
If STREAM is NIL, construct and return a string."
  (AND (UPDATE-TIMEBASE)
       (MULTIPLE-VALUE-BIND (SECONDS MINUTES HOURS DAY MONTH YEAR DAY-OF-THE-WEEK)
           (GET-TIME)
         (PRINT-DATE SECONDS MINUTES HOURS DAY MONTH YEAR DAY-OF-THE-WEEK STREAM))))

(DEFUN PRINT-UNIVERSAL-DATE (UT &OPTIONAL (STREAM *STANDARD-OUTPUT*) TIMEZONE)
  "Print the universal-time UT in verbose form on STREAM, decoding for TIMEZONE.
If STREAM is NIL, construct and return a string."
  (MULTIPLE-VALUE-BIND (SECONDS MINUTES HOURS DAY MONTH YEAR DAY-OF-THE-WEEK)
      (DECODE-UNIVERSAL-TIME UT TIMEZONE)
    (PRINT-DATE SECONDS MINUTES HOURS DAY MONTH YEAR DAY-OF-THE-WEEK STREAM)))

(DEFUN PRINT-DATE (SECONDS MINUTES HOURS DAY MONTH YEAR DAY-OF-THE-WEEK
                   &OPTIONAL (STREAM *STANDARD-OUTPUT*))
  "Print the date and time in verbose form on STREAM.
If STREAM is NIL, construct and return a string."
  (SETQ MONTH (MONTH-STRING MONTH)
        DAY-OF-THE-WEEK (DAY-OF-THE-WEEK-STRING DAY-OF-THE-WEEK))
  (FORMAT STREAM
          "~A the ~:R of ~A, ~D; ~D:~2,'0D:~2,'0D ~A"
          DAY-OF-THE-WEEK DAY MONTH YEAR (1+ (CL:REM (+ HOURS 11.) 12.)) MINUTES SECONDS
          (COND ((AND (ZEROP SECONDS)
                      (ZEROP MINUTES)
                      (MEMQ HOURS '(0 12.)))
                 (IF (= HOURS 0) "midnight" "noon"))
                (( HOURS 12.) "pm")
                (T "am"))))

(DEFUN PRINT-DATE-ONLY (&OPTIONAL (STREAM *STANDARD-OUTPUT*))
  "Prints only those aspects of the time, UT, that differ from the current time.
Also never prints seconds.  Used by notifications, for example.
If STREAM is NIL, construct and return a string."
  (MULTIPLE-VALUE-BIND (IGNORE IGNORE IGNORE DAY MONTH YEAR)
      (DECODE-UNIVERSAL-TIME (get-universal-time))
    (FORMAT STREAM "~2,'0D-~2,'0D-~2,'0D"
            day
            (month-string month ':short)
            (remainder year 100.))))

(DEFUN PRINT-BRIEF-UNIVERSAL-TIME (UT &OPTIONAL (STREAM *STANDARD-OUTPUT*)
                                                (REF-UT (GET-UNIVERSAL-TIME))
                                                (DATE-PRINT-MODE *DEFAULT-DATE-PRINT-MODE*))
  "Prints only those aspects of the time, UT, that differ from the current time.
Also never prints seconds.  Used by notifications, for example.
If STREAM is NIL, construct and return a string."
  (MULTIPLE-VALUE-BIND (IGNORE MINUTES HOURS DAY MONTH YEAR)
      (DECODE-UNIVERSAL-TIME UT)
    (MULTIPLE-VALUE-BIND (IGNORE IGNORE IGNORE REF-DAY REF-MONTH REF-YEAR)
        (DECODE-UNIVERSAL-TIME REF-UT)
      ;; If not same day, print month and day numerically
      (IF (OR ( DAY REF-DAY) ( MONTH REF-MONTH) ( YEAR REF-YEAR))
          (WITH-STACK-LIST (DATE-MODE-ARGS DAY MONTH (MONTH-STRING MONTH :SHORT)
                                           (= YEAR REF-YEAR) (YYYY-YY YEAR REF-YEAR) YEAR)
            (FORMAT STREAM "~? ~2,'0D:~2,'0D"
                    (GET-DATE-PRINT-MODE DATE-PRINT-MODE)
                    DATE-MODE-ARGS
                    HOURS MINUTES))
        ;; Always print hours colon minutes, even if same as now
        (FORMAT STREAM "~2,'0D:~2,'0D" HOURS MINUTES)))))

(DEFUN PRINT-UPTIME (&OPTIONAL (STREAM *STANDARD-OUTPUT*))
  "Print how long this machine has been up since last cold boot."
  (FORMAT STREAM "~&This machine has been up ~\time-interval\."
          (- (TIME:GET-UNIVERSAL-TIME) TIME:*UT-AT-BOOT-TIME*)))


#|
;;;; Essential stuff
(defun moonphase (&optional (ut (get-universal-time)))
;  (multiple-value-bind (seconds minutes hours date month year nil dstp)
;                      (decode-universal-time ut)
;    (let* ((secs (+ seconds (* minutes 60.) (* hours 3600.)
;                    (if dstp -3600. 0)
;                    (* (+ date -1
;                          (aref cumulative-month-days-table month))
;                        86400.)))
;          (year-1 (1- year))
;          (d (+ (- (+ (* year 365.) (ash year-1 -2)) (truncate year-1 100.))
;                (truncate year-1 400.)
;                1)))
;      ;If one wanted to waste time, it would seem to be possible to just
;      ; change the constants which follow, and just use the universal
;      ; time in place of all the code above and the quantity
;      ; (+ (* d 86400.) secs) below.  Possibly even reducing it such that
;      ; it was all fixnum arithmetic.
  (let* ((d (ash (+ ut 690882.)
                 2))
         (r (ash (remainder d 2551443.) -2)))
    (values (ldb (byte 2 0) (truncate d 2551443.))
            (truncate r (* 60. 60. 24.))
            (cl:rem (truncate r 3600.) 24.)
            (cl:rem (truncate r 60.) 60.)
            (cl:rem r 60.))))

;(defun test (&optional (ut (get-universal-time)))
;  (let* ((year (nth-value 5 (decode-universal-time ut)))
;        (d (* (+ (- (+ (* year 365.)
;                       (ash (1- year) -2))
;                    (truncate (1- year) 100.))
;                 (truncate (1- year) 400.)
;                 1)
;              (* 60. 60. 24.))))
;    (format t "~D ~D" ut d)
;    (values ut d)))

(defun print-moonphase (quarter day hour minute second
                        &optional (destination t))
  (format destination
          "~A~@[+~:[~*~;~DD.~]~:[~*~;~DH.~]~:[~*~;~DM.~]~:[~*~;~DS.~]~]"
          (nth quarter '("NM" "FQ" "FM" "LQ"))
          (= (+ day hour minute second) 0)
          (= day 0) day (= hour 0) hour
          (= minute 0) minute (= second 0) second))

(defun print-universal-moonphase (ut &optional (destination t))
  (multiple-value-bind (quarter day hour minute second) (moonphase ut)
    (print-moonphase quarter day hour minute second destination)))

(defun print-current-moonphase (&optional (destination t))
  (print-universal-moonphase (get-universal-time) destination))
|#


;;;; Some useful strings and accessing functions.

;;; Days of the week.  Elements must be (in order):
;;; (1) Three-letter form.
;;; (2) Full spelling.
;;; (3) Middle-length form if any, else NIL.
;;; (4) Francais.
;;; (5) Deutsch.
;;; (6) Italian.  ; How do you say that in Italian ?

(DEFVAR *DAYS-OF-THE-WEEK* '(("Mon" "Monday" NIL "Lundi" "Montag" "Lunedi")
                             ("Tue" "Tuesday" "Tues" "Mardi" "Dienstag" "Martedi")
                             ("Wed" "Wednesday" NIL "Mercredi" "Mittwoch" "Mercoledi")
                             ("Thu" "Thursday" "Thurs" "Jeudi" "Donnerstag" "Giovedi")
                             ("Fri" "Friday" NIL "Vendredi" "Freitag" "Venerdi")
                             ("Sat" "Saturday" NIL "Samedi" "Samstag" "Sabato")
                             ("Sun" "Sunday" NIL "Dimanche" "Sonntag" "Domenica"))
        "The list of the days of the week in short, long, medium, French, German, Italian." )

(DEFUN DAY-OF-THE-WEEK-STRING (DAY-OF-THE-WEEK &OPTIONAL (MODE :LONG) &AUX STRINGS)
  (SETQ STRINGS (NTH DAY-OF-THE-WEEK *DAYS-OF-THE-WEEK*))
  (CASE MODE
    (:SHORT (FIRST STRINGS))
    (:LONG (SECOND STRINGS))
    (:MEDIUM (OR (THIRD STRINGS) (FIRST STRINGS)))
    (:FRENCH (FOURTH STRINGS))
    (:GERMAN (FIFTH STRINGS))
    (:ITALIAN (SIXTH STRINGS))                  ; After this, perhaps NDOWSS ?
    (OTHERWISE (FERROR NIL "~S is not a known day-of-the-week mode" MODE))))


;;; Months of the year:  Elements must be (in order):
;;; (1) Three-letter form.
;;; (2) Full spelling.
;;; (3) Middle-length form if any, else NIL.
;;; (4) Francais.
;;; (5) Roman numerals (used in Europe).
;;; (6) Deutsch.
;;; (7) Italian.

(DEFVAR *MONTHS* '(("Jan" "January" NIL "Janvier" "I" "Januar" "Genniao")
                   ("Feb" "February" NIL "Fevrier" "II" "Februar" "Febbraio")
                   ("Mar" "March" NIL "Mars" "III" "Maerz" "Marzo")
                   ("Apr" "April" NIL "Avril" "IV" "April" "Aprile")
                   ("May" "May" NIL "Mai" "V" "Mai" "Maggio")
                   ("Jun" "June" NIL "Juin" "VI" "Juni" "Giugno")
                   ("Jul" "July" NIL "Juillet" "VII" "Juli" "Luglio")
                   ("Aug" "August" NIL "Aout" "VIII" "August" "Agosto")
                   ("Sep" "September" "Sept" "Septembre" "IX" "September" "Settembre")
                   ("Oct" "October" NIL "Octobre" "X" "Oktober" "Ottobre")
                   ("Nov" "November" "Novem" "Novembre" "XI" "November" "Novembre")
                   ("Dec" "December" "Decem" "Decembre" "XII" "Dezember" "Dicembre"))
  "List of names lists of names of months: short, long, medium, French, Roman, German, Italian")

(DEFUN MONTH-STRING (MONTH &OPTIONAL (MODE :LONG) &AUX STRINGS)
  (SETQ STRINGS (NTH (1- MONTH) *MONTHS*))
  (CASE MODE
    (:SHORT (FIRST STRINGS))
    (:LONG (SECOND STRINGS))
    (:MEDIUM (OR (THIRD STRINGS) (FIRST STRINGS)))
    (:FRENCH (FOURTH STRINGS))
    (:ROMAN (FIFTH STRINGS))
    (:GERMAN (SIXTH STRINGS))
    (:ITALIAN (SEVENTH STRINGS))
    (OTHERWISE (FERROR NIL "~S is not a known month mode" MODE))))

(DEFVAR *TIMEZONES* '((0 "GMT" NIL #/Z)                        ;Greenwich
                      (0 "UT" NIL #/Z)
                      (1 NIL NIL #/A)
                      (2 NIL NIL #/B)
                      (3 NIL "ADT" #/C)
                      (4 "AST" "EDT" #/D)              ;Atlantic
                      (5 "EST" "CDT" #/E)              ;Eastern
                      (6 "CST" "MDT" #/F)              ;Central
                      (7 "MST" "PDT" #/G)              ;Mountain
                      (8 "PST" "YDT" #/H)              ;Pacific
                      (9 "YST" "HDT" #/I)              ;Yukon
                      (10. "HST" "BDT" #/K)            ;Hawaiian
                      (11. "BST" NIL #/L)              ;Bering
                      (12. NIL NIL #/M)
                      (-1 NIL NIL #/N)
                      (-2 NIL NIL #/O)
                      (-3 NIL NIL #/P)
                      (-4 NIL NIL #/Q)
                      (-5 NIL NIL #/R)
                      (-6 NIL NIL #/S)
                      (-7 NIL NIL #/T)
                      (-8 NIL NIL #/U)
                      (-9 NIL NIL #/V)
                      (-10. NIL NIL #/W)
                      (-11. NIL NIL #/X)
                      (-12. NIL NIL #/Y)
                      (3.5 "NST" NIL -1)                ;Newfoundland
                      )
  "List of timezones: offset from gmt, name, daylight-savings-name, military character.")

(DEFUN TIMEZONE-STRING (&OPTIONAL (TIMEZONE *TIMEZONE*)
                                  (DAYLIGHT-SAVINGS-P (DAYLIGHT-SAVINGS-P)))
  "Return a string describing timezone TIMEZONE, optionally for daylight savings time.
Defaults are our own timezone, and DST if it is now in effect."
  (IF DAYLIGHT-SAVINGS-P
      (THIRD (ASSQ (1- TIMEZONE) *TIMEZONES*))
      (SECOND (ASSQ TIMEZONE *TIMEZONES*))))

;;;; Date and time parsing

(DEFUN VERIFY-DATE (DAY MONTH YEAR DAY-OF-THE-WEEK)
  "If the day of the week of the date specified by DATE, MONTH, and YEAR
is the same as DAY-OF-THE-WEEK, return NIL; otherwise, return a string that
contains a suitable error message. If YEAR is less than 100, it is shifted
by centuries until it is within 50 years of the present."
  (COND ((> DAY (MONTH-LENGTH MONTH YEAR))
         (FORMAT NIL "~A only has ~D day~:P" (MONTH-STRING MONTH) (MONTH-LENGTH MONTH YEAR)))
        (DAY-OF-THE-WEEK
         (LET ((UT (ENCODE-UNIVERSAL-TIME 0 0 0 DAY MONTH YEAR)))
           (MULTIPLE-VALUE-BIND (NIL NIL NIL NIL NIL NIL CORRECT-DAY-OF-THE-WEEK)
               (DECODE-UNIVERSAL-TIME UT)
             (AND ( DAY-OF-THE-WEEK CORRECT-DAY-OF-THE-WEEK)
                  (FORMAT NIL "The ~:R of ~A, ~D is a ~A, not a ~A"
                          (MONTH-STRING MONTH) DAY YEAR
                          (DAY-OF-THE-WEEK-STRING CORRECT-DAY-OF-THE-WEEK)
                          (DAY-OF-THE-WEEK-STRING DAY-OF-THE-WEEK))))))
        (T
         NIL)))


;;;; This is code to read and initialize the LAMBDA's battery clock.

(defmacro define-rtc-register-field (name byte register)
  (setq register (case register (A #o12) (B #o13) (C #o14) (D #o15)))
  `(defun ,name (&optional (value :none))
     (cond
       ((eq value :none)
        (ldb ,byte (read-rtc-register ,register)))
       ((eq value nil)
        (write-rtc-register ,register (dpb 0 ,byte (read-rtc-register ,register))))
       ((eq value t)
        (write-rtc-register ,register (dpb 1 ,byte (read-rtc-register ,register))))
       (t
        (write-rtc-register ,register (dpb value ,byte (read-rtc-register ,register)))))))

(define-rtc-register-field rtc-update-bit #o0701 A)
(define-rtc-register-field rtc-divider-select #o0403 A)
(define-rtc-register-field rtc-rate-select #o0004 A)
(define-rtc-register-field rtc-set-mode #o0701 B)
(define-rtc-register-field rtc-periodic-interrupt-enable #o0601 B)
(define-rtc-register-field rtc-alarm-interrupt-enable #o0501 B)
(define-rtc-register-field rtc-update-interrupt-enable #o0401 B)
(define-rtc-register-field rtc-square-wave-enable #o0301 B)
(define-rtc-register-field rtc-binary-mode #o0201 B)
(define-rtc-register-field rtc-24-hour-mode #o0101 B)
(define-rtc-register-field rtc-daylight-savings-mode #o0001 B)
(define-rtc-register-field rtc-valid-bit #o0701 D)

;;; 0-15 used by clock chip.  16,17 time zone.  20..34 validation cookie.

(defvar *rtc-array* :unbound)
(select-processor
  (:lambda (setq *rtc-array* (make-array 64.)))
  ((:cadr :explorer) nil))

(defconstant rtc-seconds 0)
(defconstant rtc-seconds-alarm 1)
(defconstant rtc-minutes 2)
(defconstant rtc-minutes-alarm 3)
(defconstant rtc-hours 4)
(defconstant rtc-hours-alarm 5)
(defconstant rtc-day-of-week 6)
(defconstant rtc-date 7)
(defconstant rtc-month #o10)
(defconstant rtc-year #o11)
(defconstant rtc-time-zone-low #o16)            ;not maintained by chip
(defconstant rtc-time-zone-hi #o17)             ;not maintained by chip
(defconstant rtc-cookie-start #o20)
(defconstant rtc-cookie "C'est vrai.")

(defmacro read-rtc-array (index)
  `(setf (aref *rtc-array* ,index) (read-rtc-register ,index)))

(defmacro write-rtc-array (index)
  `(write-rtc-register ,index (aref *rtc-array* ,index)))

(defsubst rtc-update-in-progress? ()
  (not (zerop (rtc-update-bit))))

(defun rtc-update-test ()
  (when (rtc-update-in-progress?)
    (process-sleep 1)
    (if (rtc-update-in-progress?)
        (format *error-output* "~&Warning: update bit on clock chip seems to be stuck."))))

(defmacro with-rtc-update-suspended (&body body)
  `(progn (rtc-update-test)
          (rtc-set-mode t)
          ,@body
          (rtc-set-mode nil)))

(defconstant rtc-address-register-multibus-address #x1c124)
(defconstant rtc-data-register-multibus-address #x1c120)

(defun read-rtc-register (adr)
  (si:%nubus-write-8 si:sdu-quad-slot rtc-address-register-multibus-address adr)
  (si:%nubus-read-8 si:sdu-quad-slot rtc-data-register-multibus-address))

(defun write-rtc-register (adr data)
  (si:%nubus-write-8 si:sdu-quad-slot rtc-address-register-multibus-address adr)
  (si:%nubus-write-8 si:sdu-quad-slot rtc-data-register-multibus-address data))

(defun print-rtc-status ()
  (flet ((print (name value)
           (format t "~&~A:~30,5T~O" name value)))
    (print "Update bit" (rtc-update-bit))
    (print "Divider select" (rtc-divider-select))
    (print "Rate select" (rtc-rate-select))
    (print "Set mode" (rtc-set-mode))
    (print "Periodic interrupt enable" (rtc-periodic-interrupt-enable))
    (print "Update interrupt enable" (rtc-update-interrupt-enable))
    (print "Square wave enable" (rtc-square-wave-enable))
    (print "Binary mode" (rtc-binary-mode))
    (print "24-hour mode" (rtc-24-hour-mode))
    (print "Daylight savings" (rtc-daylight-savings-mode))
    (print "Valid bit" (rtc-valid-bit))))

(defun read-rtc-chip ()
  (with-rtc-update-suspended
    (dotimes (i 64.) (read-rtc-array i))))

(defun write-rtc-chip ()
  (with-rtc-update-suspended
    (dotimes (i 64.) (write-rtc-array i))))

(defun rtc-valid? ()
  (read-rtc-chip)
  (dotimes (i (length rtc-cookie) t)
    (unless (= (aref *rtc-array* (+ rtc-cookie-start i)) (char-int (char rtc-cookie i)))
      (return nil))))

(defun rtc-get-universal-time ()
  (read-rtc-chip)
  (cond ((rtc-valid?)
         (let ((tz (truncate (dpb (aref *rtc-array* rtc-time-zone-hi)
                                  (byte 8 8)
                                  (aref *rtc-array* rtc-time-zone-low))
                             60.)))
           (unless (= tz *timezone*)
             (format *error-output* "~&Warning: timezone in clock chip is wrong")))
         (time:encode-universal-time
           (aref *rtc-array* rtc-seconds)
           (aref *rtc-array* rtc-minutes)
           (aref *rtc-array* rtc-hours)
           (aref *rtc-array* rtc-date)
           (aref *rtc-array* rtc-month)
           (+ (aref *rtc-array* rtc-year) 1900.)
           ;; supplying timezone arg makes encode-universal-time ignore daylight savings.
           *timezone*))
        (t nil)))

(defun set-correct-rtc-modes ()
  (rtc-binary-mode t)
  (rtc-24-hour-mode t)
  (rtc-daylight-savings-mode t)
  (rtc-divider-select 2)
  (rtc-periodic-interrupt-enable nil)
  (rtc-alarm-interrupt-enable nil)
  (rtc-update-interrupt-enable nil)
  (rtc-square-wave-enable nil))

(defun rtc-set-universal-time (ut)
  (read-rtc-chip)
  (set-correct-rtc-modes)
  (multiple-value-bind (seconds minutes hours date month year day-of-week)
      (time:decode-universal-time-without-dst ut)
    (setf (aref *rtc-array* rtc-seconds) seconds)
    (setf (aref *rtc-array* rtc-minutes) minutes)
    (setf (aref *rtc-array* rtc-hours) hours)
    (setf (aref *rtc-array* rtc-date) date)
    (setf (aref *rtc-array* rtc-month) month)
    (setf (aref *rtc-array* rtc-year) (- year 1900.))
    (setf (aref *rtc-array* rtc-day-of-week) (if (= day-of-week 6)
                                                 1
                                               (+ day-of-week 2))))
  (setf (aref *rtc-array* rtc-time-zone-low) (ldb (byte 8. 0) (* *timezone* 60.)))
  (setf (aref *rtc-array* rtc-time-zone-hi) (ldb (byte 8. 8.) (* *timezone* 60.)))
  (dotimes (i (string-length rtc-cookie))
    (setf (aref *rtc-array* (+ i rtc-cookie-start)) (char-int (char rtc-cookie i))))
  (write-rtc-chip)
  t)


;;; Explorer RTC code.

(DefConst Real-Time-Clock-Base #xF80000)
(DefConst RTC-Seconds-Counter                            8.)
(DefConst RTC-Minutes-Counter                           12.)
(DefConst RTC-Hours-Counter                             16.)
(DefConst RTC-Day-Of-Month-Counter                      24.)
(DefConst RTC-Month-Counter                             28.)
(DefConst RTC-RAM-100-Nanoseconds-Counter               32.)
(DefConst RTC-RAM-10-And-100-Millisecond-Counter        36.)
(DefConst RTC-Read-Status-Bit                           80.)


(DefConst RTC-Counter-Registers
          `(,RTC-Seconds-Counter
            ,RTC-Minutes-Counter
            ,RTC-Hours-Counter
            ,RTC-Day-Of-Month-Counter
            ,RTC-Month-Counter))

(DefConst *maximum-year* 2399.)

(Defun BCD-to-Fixnum (bcd-number)
  (+ (* (truncate bcd-number #x10) 10.)
     (mod bcd-number #x10)))

(Defun Fixnum-to-BCD (fixnum)
  (+ (* (truncate fixnum 10.) #x10)
     (mod fixnum 10.)))

(Defun Read-Explorer-RTC-Chip (offset)
  (si:%nubus-read-8 tv:tv-quad-slot (+ real-time-clock-base offset)))

(Defun Write-Explorer-RTC-chip (offset value)
  (si:%nubus-write-8 tv:tv-quad-slot (+ real-time-clock-base offset) value))

(Defun Explorer-RTC-Read-Status-OK-P ()
  (Evenp (Read-Explorer-RTC-Chip RTC-read-status-bit)))

(Defun Read-Explorer-RTC (&aux clock-values (try-again t))
  (loop WHILE try-again
        DO
        (progn
          (without-interrupts
            (setq clock-values
                  (loop FOR register IN RTC-counter-registers
                        ALWAYS (Explorer-RTC-Read-Status-Ok-P)
                        FINALLY (Return (Progn (Setq try-again nil) clock-collector))
                        COLLECT (BCD-to-Fixnum (Read-Explorer-RTC-Chip register))
                        INTO clock-collector)))))
  clock-values)

(Defun Write-Explorer-RTC (seconds minutes hours date month
                           &aux clock-values (try-again t))
  (Setq clock-values `(,seconds ,minutes ,hours ,date ,month))
  (Loop WHILE try-again
        DO (without-interrupts
             (loop FOR time-index FROM 0 BY 1
                   FOR register IN RTC-counter-registers
                   ALWAYS (Explorer-RTC-read-status-ok-p)
                   FINALLY (setq try-again nil)
                   DO
                     (Write-Explorer-RTC-chip
                       register (fixnum-to-bcd (nth time-index clock-values))))))
  )

(Defun Explorer-Set-Universal-Time (universal-time)
  (Multiple-Value-Bind (seconds minutes hours day-of-month month year)
      (time:decode-universal-time universal-time)
    (Let ((february-29 (and (= day-of-month 29) (= month 2))))
      (Write-Explorer-RTC
        seconds minutes hours (if february-29 (1- day-of-month) day-of-month) month)
      (Write-Day-is-February-29 february-29)
      (Write-Explorer-Year year)))
  )


(Defun Explorer-Initial-Date-Valid-P ()
  (Let ((year (Read-Explorer-Year)))
    (Unless (or (< year 1984.) (> year *maximum-year*))
      (Let ((clock-value (read-explorer-RTC)))
        (Unless (or (> (nth 0 clock-value) 59.)         ; Seconds
                    (> (nth 1 clock-value) 59.)         ; Minutes
                    (> (nth 2 clock-value) 23.))        ; Hours
          (Let ((month (nth 4 clock-value)))
            (Unless (or (> month 12.)
                        (> (nth 3 clock-value)
                           (month-length month year)))  ; Day of month
              t))))))
  )

(Defun Explorer-Get-Universal-Time ()
  ;; Special note: when February 29th comes around, we have
  ;; backed up the clock to February 28th and set a flag
  ;; that indicates that today is really February 29th.
  (Let ((clock-value (Read-Explorer-RTC)))
    (time:encode-universal-time
      (nth 0 clock-value)               ; Seconds
      (nth 1 clock-value)               ; Minutes
      (nth 2 clock-value)               ; Hours
      (+ (nth 3 clock-value)            ; Day of month
         (if (day-is-february-29-p) 1 0))
      (nth 4 clock-value)               ; Month
      (Read-Explorer-Year)))
  )

(Defun Read-Explorer-Year ()
  ;; The clock chip doesn't have a year counter so we will store that
  ;; information into the RAM part of the clock which we will not be
  ;; using.  The low order 3 decimal digits of the time will contain
  ;; the year data in the following format:
  ;;   10 and 100 millisecond counters - year within century
  ;;   100 nanosecond counter          - formula
  ;;
  ;; where the formula is calculated as follows:
  ;;   (century - 19) * 2 + day-is-february-29
  ;; Note that the 100 nanosecond counter only has the 10's digits
  ;; being valid.  The units digits are all zeros.
  ;;      7 6 5 4 3 2 1 0  bit position
  ;;      D D D D 0 0 0 0  data present or 0
  ;;      x x x            century
  ;;            x          day-is-february-29
  ;;
  ;; This will get us up to the year 2399, which should be enough.
  ;;
  ;; day-is-february-29 is 0 on every day which is not February 29 and
  ;;   is  1 on that day.
  (+ (bcd-to-fixnum
       (Read-Explorer-RTC-chip RTC-RAM-10-and-100-millisecond-counter))
     (* 100. (+ 19. (ldb #o0503 (read-explorer-RTC-chip RTC-RAM-100-nanoseconds-counter)))))
  )

(Defun Write-Explorer-Year (year)
  (Multiple-Value-Bind (century year-within-century)
      (Truncate year 100.)
    (Write-Explorer-RTC-chip
      RTC-RAM-10-and-100-millisecond-counter (fixnum-to-bcd year-within-century))
    ;; Write in the century information, being careful not to
    ;; touch the day-is-february-29 bit (bit 4)
    (Write-Explorer-RTC-chip
      RTC-RAM-100-nanoseconds-counter
      (dpb (- century 19.) #o0503
           (read-explorer-RTC-chip RTC-RAM-100-nanoseconds-counter))))
  )


(Defun day-is-february-29-p ()
  (ldb-test #o0401 (read-explorer-RTC-chip RTC-RAM-100-nanoseconds-counter)))

(Defun write-day-is-february-29 (day-indicator)
  (Unless (numberp day-indicator)
    (Setq day-indicator (if day-indicator 1 0)))
  (Write-Explorer-RTC-Chip
    RTC-RAM-100-nanoseconds-counter
    (dpb day-indicator #o0401
         (Read-Explorer-RTC-Chip
           RTC-RAM-100-nanoseconds-counter)))
  )

(defun time-initialization ()
  (setq last-boot-time (time)
        was-negative nil
        high-time-bits 0)
  (if si:*in-cold-load-p*
      (initialize-timebase)
    ;;Wait until scheduler turned on, etc.  Directly on WARM-INITIALIZATION-LIST
    ;; doesnt win on the secondary proc of a 2X2.  We'll have to see if people like this.
    (process-run-function "Initialize Timebase" 'initialize-timebase)))

(ADD-INITIALIZATION "Initialize Timebase" '(time-initialization) '(:warm :normal))
