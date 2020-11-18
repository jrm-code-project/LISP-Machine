
(DEFCOM COM-WHAT-LOSSAGE "What commands did I type to cause this lossage?
Prints out descriptions of the last sixty characters typed on the keyboard." (KM)
  (COND ((NOT (MEMQ ':PLAYBACK (SEND *STANDARD-INPUT* ':WHICH-OPERATIONS)))
         (BARF "The current value of *STANDARD-INPUT*, ~S, does not support recording of input."
               *STANDARD-INPUT*))
        (T (LET ((A (SEND *STANDARD-INPUT* ':PLAYBACK))
                 (WIDTH (OR (SEND *STANDARD-OUTPUT* ':SEND-IF-HANDLES ':SIZE-IN-CHARACTERS)
                            95.)))
             (LET ((P (ARRAY-LEADER A 1))
                   (L (ARRAY-LEADER A 0)))
               (DO ((I (\ (1+ P) L) (\ (1+ I) L))
                    (J 0 (1+ J)))
                   (( J L))
                 (LET ((CH (AREF A I)))
                   (AND CH (NOT (LISTP CH))
                        (FORMAT:BREAKLINE WIDTH NIL
                          (FORMAT:OCHAR CH ':EDITOR)
                          " "))))))
           (SEND *STANDARD-OUTPUT* ':FRESH-LINE)))
  DIS-NONE)
