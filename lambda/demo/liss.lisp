;;;-*- Mode:LISP; Package:HACKS; Base:8 -*-

;;; Slope-indexes vary from 0 to 7.  This table is indexed into by the index
;;; and gives you the slope value.
(DEFVAR *SLOPE-ARRAY* (MAKE-ARRAY 11 ':TYPE 'ART-Q))
(FILLARRAY *SLOPE-ARRAY* '(0 1 2 1 0 -1 -2 -1))

;;; Take two 8-lists representing X durations and Y durations.  Convert
;;; to a list of three-lists, being duration, X slope, and Y slope, by
;;; merging.
(DEFUN MERGE-DURATIONS (X-LIST Y-LIST)
  (DO ((X-SLOPE-INDEX 0)
       (Y-SLOPE-INDEX 0)
       (X-DUR (FIRST X-LIST))
       (Y-DUR (FIRST Y-LIST))
       (ANSWER))
      (NIL)
    (LET ((X-SLOPE (AREF *SLOPE-ARRAY* X-SLOPE-INDEX))
          (Y-SLOPE (AREF *SLOPE-ARRAY* Y-SLOPE-INDEX)))
      (COND ((AND (NULL X-DUR) (NULL Y-DUR))
             (RETURN (NREVERSE ANSWER)))
            ((NULL Y-DUR)
             (PUSH (LIST X-DUR X-SLOPE Y-SLOPE) ANSWER)
             (INCF X-SLOPE-INDEX)
             (POP X-LIST)
             (SETQ X-DUR (FIRST X-LIST)))
            ((NULL X-DUR)
             (PUSH (LIST Y-DUR X-SLOPE Y-SLOPE) ANSWER)
             (INCF Y-SLOPE-INDEX)
             (POP Y-LIST)
             (SETQ Y-DUR (FIRST Y-LIST)))
            ((= X-DUR Y-DUR)
             (PUSH (LIST X-DUR X-SLOPE Y-SLOPE) ANSWER)
             (INCF X-SLOPE-INDEX)
             (POP X-LIST)
             (SETQ X-DUR (FIRST X-LIST))
             (INCF Y-SLOPE-INDEX)
             (POP Y-LIST)
             (SETQ Y-DUR (FIRST Y-LIST)))
            ((< X-DUR Y-DUR)
             (PUSH (LIST X-DUR X-SLOPE Y-SLOPE) ANSWER)
             (INCF X-SLOPE-INDEX)
             (POP X-LIST)
             (SETQ Y-DUR (- Y-DUR X-DUR))
             (SETQ X-DUR (FIRST X-LIST)))
            (T
             (PUSH (LIST Y-DUR X-SLOPE Y-SLOPE) ANSWER)
             (INCF Y-SLOPE-INDEX)
             (POP Y-LIST)
             (SETQ X-DUR (- X-DUR Y-DUR))
             (SETQ Y-DUR (FIRST Y-LIST)))))))

;;; Takes a list of three-lists, being duration, X-slope, and Y-slope,
;;; plust an initial point.  Displays using these until kbd input stops it.
(DEFUN DISPLAY-LISS (LIST X Y &OPTIONAL (SLOWNESS 1000) (WINDOW TERMINAL-IO))
  (DO ((PREV-X X)
       (PREV-Y Y)
       (CUR-X X)
       (CUR-Y Y))
      (NIL)
    (IF (FUNCALL WINDOW ':TYI-NO-HANG)
        (RETURN NIL))
    (DOLIST (TL LIST)
      (DOTIMES (I SLOWNESS))
      (SETQ PREV-X CUR-X
            PREV-Y CUR-Y
            CUR-X (+ PREV-X (* (FIRST TL) (SECOND TL)))
            CUR-Y (+ PREV-Y (* (FIRST TL) (THIRD TL))))
      (FUNCALL WINDOW ':DRAW-TRIANGLE X Y PREV-X PREV-Y CUR-X CUR-Y TV:ALU-XOR))))
