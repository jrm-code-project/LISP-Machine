;;;-*- Mode:LISP; Package:ZWEI; Base:10; Readtable:CL; Patch-file: T -*-

;;; Make it possible to get into Incremental-Search from Char-Search.
;;; Modified from SYS:ZWEI;COMS.LISP.94


(DEFUN DOC-CHAR-SEARCH (COMMAND CHAR TYPE)
  (DECLARE (IGNORE CHAR))
  (CASE TYPE
    (:NAME (GET COMMAND 'COMMAND-NAME))
    ((:FULL :SHORT)
     (SEND *STANDARD-OUTPUT* :STRING-OUT "Search for a single character.")
     (COND ((EQ TYPE ':FULL)
            (SEND *STANDARD-OUTPUT* :STRING-OUT "
Special characters:
C-A     Do string search (see below).
C-B     Search forward from the beginning of the buffer.
C-E     Search backwards from the end of the buffer.
C-F     Leave the point at the top of the window, if the window must be recentered.
C-I     Incremetal Search.
C-R     Search backwards.
C-S     Repeat the last search.

String search, which you get into from C-A, reads in a string and searches for it.
")
            (SEND *STANDARD-OUTPUT* :STRING-OUT *STRING-SEARCH-OPTION-DOCUMENTATION*))))))


(DEFUN CHAR-SEARCH-INTERNAL (REVERSEP)
  (UNWIND-PROTECT
    (PROG (XCHAR CHAR UCHAR BJP ZJP TOP-P STRING BP FAILED-P QUOTE-P
           (ORIG-PT (COPY-BP (POINT))) (ARG *NUMERIC-ARG*)
           (FCN 'ZWEI-SEARCH))
        (AND (MINUSP ARG) (SETQ REVERSEP (NOT REVERSEP) ARG (- ARG)))
     LOOP (COND ((OR FAILED-P                   ;Force redisplay on failing search
                     (NULL (SETQ XCHAR (SEND *STANDARD-INPUT* :TYI-NO-HANG))))
                 (TYPEIN-LINE-WITH-REDISPLAY "~:|")
                 (AND BJP (FORMAT *QUERY-IO* "Begin "))
                 (AND ZJP (FORMAT *QUERY-IO* "End "))
                 (AND TOP-P
                      (FORMAT *QUERY-IO* "Top Line "))
                 (AND REVERSEP (FORMAT *QUERY-IO* "Reverse "))
                 (AND QUOTE-P (FORMAT *QUERY-IO* "Quoted-ascii "))
                 (FORMAT *QUERY-IO* "Search: ")))
          (COND ((NOT FAILED-P)
                 (SETQ CHAR (OR XCHAR
                                (TYPEIN-LINE-ACTIVATE
                                  (SEND *STANDARD-INPUT* :TYI))))
                 (SETQ UCHAR (CHAR-UPCASE CHAR))
                 (COND (QUOTE-P
                        (IF ( (CHAR-BITS CHAR))
                            (SETQ CHAR (LOGAND CHAR 37)))       ;ascii control
                        (SETQ STRING CHAR)
                        (SEARCH-RING-PUSH CHAR FCN))
                       ((= UCHAR #\C-A)
                        (RETURN (COM-STRING-SEARCH-INTERNAL REVERSEP BJP ZJP TOP-P)))
                       ((AND (= UCHAR #\C-R) (NOT REVERSEP))
                        (SETQ REVERSEP (NOT REVERSEP))
                        (GO LOOP))
                       ((= UCHAR #\C-I)
                        (RETURN (INCREMENTAL-SEARCH NIL)))
                       ((= UCHAR #\C-B)
                        (SETQ BJP T ZJP NIL REVERSEP NIL)
                        (GO LOOP))
                       ((= UCHAR #\C-E)
                        (SETQ ZJP T BJP NIL REVERSEP T)
                        (GO LOOP))
                       ((= UCHAR #\C-F)
                        (SETQ *CENTERING-FRACTION* 0.0s0 TOP-P T)
                        (GO LOOP))
                       ((= UCHAR #\C-G)
                        (BEEP)
                        (SEND *QUERY-IO* :MAKE-COMPLETE)
                        (GO QUIT))
                       ((OR (= UCHAR #\C-S)
                            (AND REVERSEP (= UCHAR #\C-R)))
                        (OR *SEARCH-RING* (BARF))
                        (SETQ STRING (CAAR *SEARCH-RING*)
                              FCN (CADAR *SEARCH-RING*)))
                       ((= UCHAR #\C-Q)         ;Funny ascii compatibility
                        (SETQ QUOTE-P T)
                        (GO LOOP))
                       ((> CHAR 220)            ;Random control character
                        (BEEP)
                        (GO LOOP))
                       (T
                        (SETQ STRING CHAR)
                        (SEARCH-RING-PUSH CHAR FCN)))))
          (AND (OR (NULL XCHAR) FAILED-P)
               (IF (NUMBERP STRING)
                   (FORMAT *QUERY-IO* "~C" STRING)
                   (FORMAT *QUERY-IO* "~A" STRING)))
          (SETQ BP (AND (NOT FAILED-P)
                        (DO ((I 0 (1+ I))
                             (BP (COND (BJP (INTERVAL-FIRST-BP *INTERVAL*))
                                       (ZJP (INTERVAL-LAST-BP *INTERVAL*))
                                       (T (POINT)))
                                 (FUNCALL FCN BP STRING REVERSEP)))
                            ((OR ( I ARG) (NULL BP))
                             BP))))
          (COND (BP
                 (MOVE-BP (POINT) BP))
                ((OR FAILED-P (NULL XCHAR))
                 (FORMAT *QUERY-IO* " Search failed.")
                 (BARF))
                (T
                 (SETQ FAILED-P T)
                 (GO LOOP)))                    ;Failed search typed ahead
    QUIT (MAYBE-PUSH-POINT ORIG-PT)
         (RETURN DIS-BPS))
    (SEND *MODE-LINE-WINDOW* :DONE-WITH-MODE-LINE-WINDOW)))
