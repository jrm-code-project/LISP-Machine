;;; -*- Mode:LISP; Package:TV; Readtable:ZL; Base:10; Patch-file: T -*-

(DEFMACRO WITH-WHOSTATE (WHOSTATE &BODY BODY)
  (LET ((^OLD-WHOSTATE (COMPILER:GENSYMBOL "OLD-WHOSTATE"))
        (^WHOSTATE (COMPILER:GENSYMBOL "WHOSTATE")))
    `(LET ((,^OLD-WHOSTATE (SI:PROCESS-RUN-WHOSTATE SI:CURRENT-PROCESS))
           (,^WHOSTATE ,WHOSTATE))
       (UNWIND-PROTECT
           (PROGN
             (SETF (SI:PROCESS-RUN-WHOSTATE SI:CURRENT-PROCESS) ,WHOSTATE)
             ,@BODY)
         (SETF (SI:PROCESS-RUN-WHOSTATE SI:CURRENT-PROCESS) ,^OLD-WHOSTATE)))))



(DEFVAR *SHOW-READTABLE-IN-WHO-LINE* T)

(DEFUN WHO-LINE-PACKAGE (WHO-SHEET)
  (DECLARE (:SELF-FLAVOR WHO-LINE-SHEET))
  (SETQ LAST-WHO-LINE-PROCESS (OR WHO-LINE-PROCESS
                                  (AND SELECTED-IO-BUFFER
                                       (IO-BUFFER-LAST-OUTPUT-PROCESS
                                         SELECTED-IO-BUFFER))))
  (WHEN LAST-WHO-LINE-PROCESS                   ;--- SOMETIMES THIS GETS BASHED, MESSING UP SG BELOW?
    (LET* ((SG (PROCESS-STACK-GROUP LAST-WHO-LINE-PROCESS))
           (PKG (COND ((EQ SG %CURRENT-STACK-GROUP) *PACKAGE*)
                      ((TYPEP SG 'STACK-GROUP) (SYMEVAL-IN-STACK-GROUP '*PACKAGE* SG))
                      (T PACKAGE)))
           (RDTBL (COND ((EQ SG %CURRENT-STACK-GROUP) *READTABLE*)
                        ((TYPEP SG 'STACK-GROUP) (SYMEVAL-IN-STACK-GROUP '*READTABLE* SG))
                        (T READTABLE))))
      (WHEN (OR (AND PKG (PACKAGEP PKG) (NEQ WHO-LINE-ITEM-STATE PKG))
                (AND RDTBL (NEQ WHO-LINE-EXTRA-STATE RDTBL)))
        (LET ((RDTBL-PART (HL:RDTBL-SHORTEST-NAME RDTBL))
              (PKG-PART (SI:PKG-SHORTEST-NAME PKG))
              (CHARS (TRUNCATE (SHEET-INSIDE-WIDTH WHO-SHEET)
                               (SHEET-CHAR-WIDTH WHO-SHEET))))
          (PREPARE-SHEET (WHO-SHEET)
                         (SHEET-CLEAR WHO-SHEET)
                         (WHEN *SHOW-READTABLE-IN-WHO-LINE*
                           (SHEET-STRING-OUT WHO-SHEET RDTBL-PART
                                             0 (MIN (STRING-LENGTH RDTBL-PART) 3 (- CHARS 4)))
                           (SHEET-STRING-OUT WHO-SHEET " "))
                         (SHEET-STRING-OUT WHO-SHEET PKG-PART
                                           0 (MIN (STRING-LENGTH PKG-PART)
                                                  (- CHARS (IF SI::*READ-SINGLE-COLON-ALLOW-INTERNAL-SYMBOL* 1 2))))
                         (SHEET-TYO WHO-SHEET #\:)
                         (UNLESS SI::*READ-SINGLE-COLON-ALLOW-INTERNAL-SYMBOL*
                           (SHEET-TYO WHO-SHEET #\:)))
          (SETQ WHO-LINE-ITEM-STATE PKG
                WHO-LINE-EXTRA-STATE RDTBL))))))


(DEFUN OLD-NWATCH-WHO-FUNCTION (WHO-SHEET &AUX LEFTX)
  (DECLARE (:SELF-FLAVOR WHO-LINE-SHEET))
  (OR WHO-LINE-EXTRA-STATE
      (LET ((DEFAULT-CONS-AREA WHO-LINE-AREA))
        (SETQ WHO-LINE-EXTRA-STATE (STRING-APPEND "MM/DD/YY HH:MM:SS"))))
                                                  ; Errgghhh! Krazy Backwards Amerikan dates.
  (MULTIPLE-VALUE-BIND (SECONDS MINUTES HOURS DAY MONTH YEAR)
      (TIME:GET-TIME)
    (COND ((NULL SECONDS)
           (SHEET-CLEAR WHO-SHEET)
           (COPY-ARRAY-CONTENTS "MM//DD//YY HH:MM:SS" WHO-LINE-EXTRA-STATE))
          (T
           (SETQ YEAR (MOD YEAR 100.))
           (SETQ LEFTX (MIN (NWATCH-N MONTH WHO-LINE-EXTRA-STATE 0)
                            (NWATCH-N DAY WHO-LINE-EXTRA-STATE 3)
                            (NWATCH-N YEAR WHO-LINE-EXTRA-STATE 6)
                            (NWATCH-N HOURS WHO-LINE-EXTRA-STATE 9)
                            (NWATCH-N MINUTES WHO-LINE-EXTRA-STATE 12.)
                            (NWATCH-N SECONDS WHO-LINE-EXTRA-STATE 15.)))
           (UNLESS WHO-LINE-ITEM-STATE (SETQ LEFTX 0))  ;was clobbered, redisplay all
           (SHEET-SET-CURSORPOS WHO-SHEET (* LEFTX CHAR-WIDTH) 0)
           (SHEET-CLEAR-EOL WHO-SHEET)
           (SHEET-STRING-OUT WHO-SHEET WHO-LINE-EXTRA-STATE LEFTX)
           (SETQ WHO-LINE-ITEM-STATE T)))))


(defun new-nwatch-who-function (who-sheet)
  (declare (:self-flavor who-line-sheet))
  (or who-line-extra-state
      (let ((si:default-cons-area who-line-area))
        (setq who-line-extra-state (si:string-append "mon dd hh:mm:ssAM"))))
  (multiple-value-bind (seconds minutes hours day month)
      (time:get-time)
    (cond ((null seconds)
           (sheet-clear who-sheet)
           (zl:copy-array-contents "mon dd hh:mm:ssAM" who-line-extra-state))
          (t
           (let ((leftx (min (nwatch-delta (time:month-string month ':short)
                                              who-line-extra-state 0)
                             (nwatch-delta day who-line-extra-state 4)
                             (nwatch-delta (hl:hours-mod-12 hours)
                                              who-line-extra-state 7)
                             (nwatch-delta minutes who-line-extra-state 10.)
                             (nwatch-delta seconds who-line-extra-state 13.)
                             (nwatch-delta (hl:am-or-pm hours minutes seconds)
                                              who-line-extra-state 15.))))
             (unless who-line-item-state
               (setq leftx 0))
             (sheet-set-cursorpos who-sheet (* leftx char-width) 0)
             (sheet-clear-eol who-sheet)
             (sheet-string-out who-sheet who-line-extra-state leftx)
             (setq who-line-item-state t))))))

(defun nwatch-delta (new str idx)
  (cond ((numberp new)
         (let ((dig1 (int-char (+ (zl:truncate new 10.) (char-int #\0))))
               (dig2 (int-char (+ (lisp:rem new 10.) (char-int #\0)))))
           (prog1 (cond ((not (equal (char str idx) dig1))
                         idx)
                        ((not (equal (char str (1+ idx)) dig2))
                         (1+ idx))
                        (t (si:array-length str)))
                  (setf (char str idx) dig1)
                  (setf (char str (1+ idx)) dig2))))
        (t
         (loop with delta
               finally (return (or delta 0))
               for i from 0 below (length new)
               do
               (when (not (char= (char str (+ idx i))
                                 (char new i)))
                 (setf (char str (+ idx i)) (char new i)))))))


(defvar *who-line-clock-style* ':old)
(defvar *reset-who-line-clock-style* nil)


(defun nwatch-who-function (who-sheet)
  (declare (:self-flavor who-line-sheet))
  (funcall (case *who-line-clock-style*
             (:old 'old-nwatch-who-function)
             (:new 'new-nwatch-who-function)
             (otherwise 'bash-nwatch))
           who-sheet))

(defun set-who-line-clock-style (style)
  (assert (member style '(:old :new)))
    (setq *who-line-clock-style* nil)
    (sleep 1)
    (setq *who-line-clock-style* style)
    (zl:send nwatch-who-line-sheet ':clobbered))


(defun bash-nwatch (who-sheet)
  (declare (:self-flavor who-line-sheet))
  (setq who-line-item-state nil
        who-line-extra-state nil)
  (sheet-set-cursorpos who-sheet 0 0)
  (sheet-clear-eol who-sheet)
  (sheet-string-out who-sheet who-line-extra-state 0))
