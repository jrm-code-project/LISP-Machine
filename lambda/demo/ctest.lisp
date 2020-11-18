;;-*- Mode:LISP; Package:USER; Base:10. -*-

;work rotation xfrm

(DEFCONST CTEST-BOARD-TYPE 'MPG216)  ;OR LG684

;Continuity Tester Interface

(DEFUN BYTE-FIELD (BITS OVER)
  (+ BITS (LSH OVER 6)))

(defconst ctest-%%pin  (byte-field 7 00))       ;pin number (all type boards)

;The below code basically copied from NEWWL;MPG216 >.  The fields are the
; same altho the names have been changed to protect the innocent.

;SOME BYTE POINTERS FOR EXTRACTING FIELDS

(defconst mpg216-%conn   (byte-field 1 25))     ;connector bit. (sign on 10).
(defconst mpg216-%DIPPNL (byte-field 3 22))     ;PANEL NUMBER
(defconst mpg216-%DIPG   (byte-field 3 19))     ;DIP GROUP
(defconst mpg216-%DIPS   (byte-field 6 13))     ;DIP SLOT
(defconst mpg216-%DIPOF  (byte-field 6 07))     ;DIP OFFSET

(defconst mpg216-%CNPPNL (byte-field 3 22))     ;PANEL NUMBER (MUST BE SAME AS DIP)
(defconst mpg216-%CNPG   (byte-field 3 19))     ;CONN GROUP  ( "        "       )
(defconst mpg216-%CNPJK  (byte-field 1 07))     ;CONNECTOR JACK BIT
;(defconst mpg216-%CONP   (byte-field 9 00))    ;CONNECTOR PIN #
                                                ;  (AND JACK NUMBER <PINS 1-2*26>)

(defconst mpg216-GPINS 400)             ;NOMEN FOR "G" PINS ON BOARD STARTS AT 400
                                        ; 401 IS "G1", ETC.
(defconst mpg216-MXPNL 5)               ;max number of panels wrappable at once
(defconst mpg216-NGRPS 6)               ;PG216-180 HAS 6 GROUPS
(defconst mpg216-GRPCOL 5)      ;# COLS IN GROUP
(defconst mpg216-GRPROW 6)      ;# ROWS IN GROUP
(defconst mpg216-GRPDIP (* mpg216-GRPROW mpg216-GRPCOL))        ;# DIPS IN GROUP
(defconst mpg216-MXCNP1 40)     ;MAX # CONNECTOR PINS FOR J1
(defconst mpg216-MXCNP2 50)     ;MAX # CONNECTOR PINS FOR J2
(defconst mpg216-J1TO2 2)               ;J1 IS OFFSET 2 PINS TO RIGHT OF J2

;JACKSZ:        MXCNP1
;               MXCNP2


;The 8136-PG216 consists of 180 dip slots, organized into
;6 groups of 30 dips each.
;The groups are labeled A-F, with group A to left
;Withhin a group, slots are numbered:
;       5  4  3  2  1
;       10 9 ....   6
;       15   ....   11
;       20   ....   16
;       25   ....   21
;       30   ....   26

;(All coordinates are from DIP side, assuming Scotchflex conns
; are at the top)


;AUGAT-X8136-PG216 CONNECTOR PIN FORMAT PRINTS AS #LJ#-#
;WHERE L IS THE GROUP.  THE J IS LITERAL.  THE FIRST DIGIT IS THE PANEL
;AND THE SECOND IS THE JACK.  PIN IS LAST

;     15    12     9                 0     LM bit numbering
;______|_____|_____|_____|_____|_____|
;|    20    23    26                35     PDP-10 bit numbering
;|  3  |  3  |   | |        9        |
;|_____|_____|___|_|_________________|
;     |    |     |      |
;     |    |     |      |------------>PIN
;     |    |     |
;     |    |     |-------------------JACK 01, 12
;     |    |
;     |    |--------------------------># GROUP
;     |
;     |-------------------------------># PANEL

(DEFUN CTEST-LOC (LOC)
  (SELECTQ CTEST-BOARD-TYPE
    (MPG216 (MPG216-PRNLOC LOC))
    (LG684 (LG684-PRNLOC LOC))
    (OTHERWISE (FERROR NIL ""))))

(DEFUN CTEST-GETLOC (STR &OPTIONAL BEG LIM)
  (SELECTQ CTEST-BOARD-TYPE
    (MPG216 (MPG216-GETLOC STR BEG LIM))
    (LG684 (LG684-GETLOC STR BEG LIM))
    (OTHERWISE (FERROR NIL ""))))

(DEFUN CTEST-MAPLOC (LOC)
  (SELECTQ CTEST-BOARD-TYPE
    (MPG216 (MPG216-MAPLOC LOC))
    (LG684 (LG684-MAPLOC LOC))
    (OTHERWISE (FERROR NIL ""))))


(defun mpg216-prnloc (loc &aux ans)
  (let ((panel (ldb mpg216-%DIPPNL loc))
        (group (ldb mpg216-%DIPG loc))
        (conn (ldb mpg216-%CONN loc))
        (pin (ldb ctest-%%pin loc)))
    (cond ((zerop conn)
           (let ((slot (ldb mpg216-%DIPS loc))
                 (offset (ldb mpg216-%DIPOF loc)))
             (setq ans (format nil "~D~C~2,48D" panel (+ #/@ group) slot))
             (cond ((not (zerop offset))
                    (setq ans (string-append ans (format nil "@~2,48D" offset)))))))
          (t (setq ans (format nil "~D~CJ~D"
                               panel (+ #/@ group) (1+ (ldb mpg216-%CNPJK loc))))))
    (cond ((and (zerop conn)
                (>= pin mpg216-GPINS))
           (SETQ ANS (format nil "~A-~1D" ANS (- pin mpg216-GPINS))))
          (t (SETQ ANS (format nil "~A-~2,48D" ANS pin)))))
  ANS)

;All calculations are done from the dip side.

;(0,0) at lower left hand corner in left handed coordinate system.
;Dip sockets are arranged in 5x6 groups.  These 30 dip groups come
;in pairs.  Each group comes with a pair of Scotch Flex(R) connectors
;labeled "J1" and "J2".  There can be up to 6 30 dip groups on one
;board.  The horizontal spacing between groups is 2.700".

;There are 5 panels, vertically arranged.  The vertical spacing
;is 7.500".

;5A30(8) is at (0,0).  5AJ2-26 is at (500,6100).  5AJ1-26 is at (500, 6400).
;--5A30(10)
;It follows that 5B30(8) is at (2700,0)

;UMLCOL__GRPROW         ;UML INTERCHANGES ROWS AND COLUMNS
;UMLROW__GRPCOL
(defconst mpg216-PNLOFT 8100)   ;8.100" VERTICAL SPACING BETWEEN PANELS
(defconst mpg216-GXOFST 2700)   ;2.700" GROUP HORIZONTAL SPACING
(defconst mpg216-XDIPSP  500)   ; .500" DIP HORIZONTAL SPACING
(defconst mpg216-YDIPSP 1100)   ;1.100" DIP VERTICAL SPACING
(defconst mpg216-GNDXOF  100)   ; .100" X OFFSET FOR TWP GROUND PINS
(defconst mpg216-BRDGND   10)   ; PIN 10 IS DEDICATED GROUND
(defconst mpg216-BRDPWR   20)   ; PIN 20 IS DEDICATED POWER
;CONNECTORS
(defconst mpg216-JXOFST    0)   ; .000" X OFFSET FOR JACKS
(defconst mpg216-JYOFST 6700)   ;6.700" Y OFFSET FOR J2
(defconst mpg216-JYOFS1  300)   ; .300" Y OFFSET FROM J2 TO J1

(defvar twenty-pin-socket-xoff nil)
(defvar twenty-pin-socket-yoff nil)
(defconst twenty-pin-dipsoc-xpinsp  300)        ; .300" DIP PIN HORIZONTAL SPACING
(defconst twenty-pin-dipsoc-ypinsp  100)        ; .100" DIP PIN VERTICAL SPACING
(defvar flat-cable-conn-xoffs nil)
(defvar flat-cable-conn-yoffs nil)
(defconst flat-cable-pxpnsp  100)       ; .100" CONNECTOR PIN VERTICAL SPACING
(defconst flat-cable-pypnsp  100)       ; .100" CONNECTOR PIN HORIZONTAL SPACING

(defun ctest-conn-and-socket-init nil
  (setq twenty-pin-socket-xoff (make-array 20)
        twenty-pin-socket-yoff (make-array 20)
        flat-cable-conn-xoffs (make-array 51)
        flat-cable-conn-yoffs (make-array 51))
  (dotimes (p 20.)
    (as-1 (* (cond ((<= p 9) 0) ;First col of pins is on the left (DIP side)
                   (t 1))
             twenty-pin-dipsoc-xpinsp)
          twenty-pin-socket-xoff
          p)
    (as-1 (* (cond ((<= p 9)
                    (- 9 p))
                   (t (- p 10.)))
             twenty-pin-dipsoc-ypinsp)
          twenty-pin-socket-yoff
          p))
  (dolist (maxp '(50 40 26))
    (let ((xoff-arr (make-array maxp))
          (yoff-arr (make-array maxp))
          (hmaxp (truncate maxp 2)))
      (dotimes (p maxp)
        (as-1 (- (* (- (1- hmaxp) (\ p hmaxp)) flat-cable-pxpnsp)
                 flat-cable-pxpnsp)     ;Pin 25 (which doesnt really exist) is one click
              xoff-arr                  ; to the right of the 1a5-1 to 10 line of pins
            p)
      (as-1 (* (cond ((< p hmaxp) 1)
                     (t 0))
               flat-cable-pypnsp)
            yoff-arr
            p)
      (as-1 xoff-arr flat-cable-conn-xoffs maxp)
      (as-1 yoff-arr flat-cable-conn-yoffs maxp)))))

(defun mpg216-getloc (STR BEG LIM &AUX C VAL-LIST IDX)
  (IF (NULL BEG) (SETQ BEG 0))
  (IF (NULL LIM) (SETQ LIM (ARRAY-ACTIVE-LENGTH STR)))
  (MULTIPLE-VALUE (C VAL-LIST IDX)
    (LNPARSE '("#L#-#(#L#-#)" "#L#-#(#)" "#L#-#"
               "#L#@#-#(#)" "#L#@#-#" "#LJ#-#") STR BEG LIM))
  (SELECTQ C (0 (SETQ VAL-LIST (CDDDDR VAL-LIST))  ;ADAPTOR FROB, FLUSH THAT ONE
                (DPB (CAR VAL-LIST) MPG216-%CNPPNL
                  (DPB (CADR VAL-LIST) MPG216-%CNPG
                     (DPB (CADDR VAL-LIST) MPG216-%DIPS
                          (DPB (CADDDR VAL-LIST) CTEST-%%PIN 0)))))
             (1 (DPB (CAR VAL-LIST) MPG216-%CNPPNL
                  (DPB (CADR VAL-LIST) MPG216-%CNPG
                     (DPB (CADDR VAL-LIST) MPG216-%DIPS
                          (DPB (CAR (CDDDDR VAL-LIST)) CTEST-%%PIN 0)))))
             (2 (DPB (CAR VAL-LIST) MPG216-%CNPPNL
                  (DPB (CADR VAL-LIST) MPG216-%CNPG
                     (DPB (CADDR VAL-LIST) MPG216-%DIPS
                          (DPB (CADDDR VAL-LIST) CTEST-%%PIN 0)))))
             (3 (DPB (CAR VAL-LIST) MPG216-%CNPPNL
                  (DPB (CADR VAL-LIST) MPG216-%CNPG
                     (DPB (CADDR VAL-LIST) MPG216-%DIPS
                          (DPB (CADDDR (CDDR VAL-LIST)) CTEST-%%PIN 0)))))
             (4 (DPB (CAR VAL-LIST) MPG216-%CNPPNL
                  (DPB (CADR VAL-LIST) MPG216-%CNPG
                     (DPB (CADDR VAL-LIST) MPG216-%DIPS
                         (DPB (CADDDR VAL-LIST) MPG216-%DIPOF
                            (DPB (CAR (CDDDDR VAL-LIST)) CTEST-%%PIN 0))))))
             (5 (DPB (CAR VAL-LIST) MPG216-%CNPPNL
                  (DPB (CADR VAL-LIST) MPG216-%CNPG
                     (DPB 1 MPG216-%CONN
                        (DPB (1- (CADDR VAL-LIST)) MPG216-%CNPJK
                           (DPB (CADDDR VAL-LIST) CTEST-%%PIN 0)))))))
)

(defun mpg216-maploc (loc)  ;returns x and y, but swap these to get tester coords
  (prog (x y panel group conn pin slot offset)
        (setq panel (ldb mpg216-%DIPPNL loc)
              group (ldb mpg216-%DIPG loc)
              conn  (ldb mpg216-%CONN loc)
              pin   (ldb ctest-%%pin loc))
        (cond ((> panel mpg216-MXPNL)
               (ferror nil "panel number too high ~s" panel))
              ((> group mpg216-NGRPS)
               (ferror nil "group number too high ~s" group)))
        (setq y (* mpg216-PNLOFT (- mpg216-MXPNL panel))
              x (* (1- group) mpg216-GXOFST))
        (cond ((zerop conn)
               (setq slot (1- (ldb mpg216-%DIPS loc))
                     offset (ldb mpg216-%DIPOF loc))
               (let ((xpos (\ slot mpg216-grpcol))
                     (ypos (truncate slot mpg216-grpcol)))
                 (setq x (+ x (* (1- (- mpg216-grpcol xpos)) mpg216-XDIPSP)
                              (ar-1 TWENTY-PIN-SOCKET-xoff (1- pin)))
                       y (+ y (* (1- (- mpg216-grprow ypos)) mpg216-YDIPSP)
                              (ar-1 TWENTY-PIN-SOCKET-yoff (1- pin))))))
              (t (let ((jack (ldb mpg216-%CNPJK loc)))
                   (setq y (+ y mpg216-jyofst))  ;vert offset to j2
                   (cond ((= jack 0)
                          (setq y (+ y mpg216-jyofs1))   ;additional offset to j1
                          (cond ((> pin (truncate mpg216-mxcnp1 2))  ;in bottom row?
                                 (setq pin (+ pin 5))))        ;j2's bottom row starts
                                                               ; +5 pins w.r.t j1
                          (setq pin (+ pin mpg216-j1to2))))  ;and j1 is displaced 2 pins right
                   (setq x (+ x (ar-1 (ar-1 flat-cable-conn-xoffs 50.) (1- pin)))
                         y (+ y (ar-1 (ar-1 flat-cable-conn-yoffs 50.) (1- pin)))))))
        (return x y)))


(DEFUN LNPARSE (PATTERN-LIST STR BEG LIM)
  (PROG (PL MATCHP ANS IDX C)
        (SETQ PL PATTERN-LIST C 0)
    L   (COND ((NULL PL) (RETURN NIL)))
        (MULTIPLE-VALUE (MATCHP ANS IDX)
           (LN-PMATCH (CAR PL) STR BEG LIM))
        (COND (MATCHP (RETURN C ANS IDX)))
        (SETQ PL (CDR PL) C (1+ C))
        (GO L)))

(DEFUN LN-PMATCH (PAT STR BEG LIM)
   (PROG (PIDX PLIM IDX ANS MATCHP VAL)
         (SETQ PIDX 0 PLIM (ARRAY-ACTIVE-LENGTH PAT)
               IDX BEG)
    L    (COND ((NOT (< IDX LIM)) (RETURN NIL)))
         (MULTIPLE-VALUE (MATCHP VAL IDX)
             (LN-PMATCH-CH (AR-1 PAT PIDX) STR IDX LIM))
         (COND ((NULL MATCHP) (RETURN NIL)))
         (COND (VAL (SETQ ANS (NCONC ANS (LIST VAL)))))
         (COND ((= (SETQ PIDX (1+ PIDX)) PLIM)
                (RETURN T ANS IDX)))
         (GO L)))

(DEFUN LN-PMATCH-CH (CH STR IDX LIM)
   (COND ((EQ CH #/#)
          (LN-PMATCH-NUM STR IDX LIM))
         ((EQ CH #/L)           ;any DEC letter
          (LN-PMATCH-LET STR IDX LIM))
         ((EQ CH #/P)           ;"paddle" letter ABCDEF
          (LN-PMATCH-LET STR IDX LIM 6))
         ((= CH (CHAR-UPCASE (AR-1 STR IDX)))
          (PROG NIL (RETURN T NIL (1+ IDX))))
))


(DEFUN LN-PMATCH-NUM (STR IDX LIM)
   (PROG (ANS CH)
         (COND ((OR (NOT (< IDX LIM))
                    (NOT (AND (>= (SETQ CH (AR-1 STR IDX)) #/0)
                              (<= CH #/9))))
                (RETURN NIL)))
         (SETQ ANS (- CH #/0))
      L  (COND ((OR (NOT (< (SETQ IDX (1+ IDX)) LIM))
                    (NOT (AND (>= (SETQ CH (AR-1 STR IDX)) #/0)
                              (<= CH #/9))))
                (RETURN T ANS IDX)))
         (SETQ ANS (+ (* ANS 10.) (- CH #/0)))
         (GO L)))

(DEFUN LN-PMATCH-LET (STR IDX LIM &OPTIONAL (MAX 22.))
  (PROG (CH)
        (COND ((AND (< IDX LIM)
                    (>= (SETQ CH (CHAR-UPCASE (AR-1 STR IDX))) #/A)
                    (<= CH (+ #/A MAX)))
               (RETURN T (CTEST-DEC-LETTER-TO-NUMBER (- CH #/@)) (1+ IDX))))))

(DEFUN MATCH-TEST NIL
  (DO ((x) (y) (loc)) (())
    (PRINT (CTEST-LOC (setq loc (CTEST-GETLOC (READLINE)))))
    (multiple-value (x y)
      (ctest-maploc loc))
    (format t "x=~d,y=~d~%" x y)
    ))

(DECLARE (SPECIAL LEFT-PROBE RIGHT-PROBE))

(DEFUN LOC-TEST (&optional (probe-ob left-probe))
  (DO ((x) (y) (loc) (XE) (YE)) (())
    (CTEST-LOC (setq loc (CTEST-GETLOC (READLINE))))
    (multiple-value (y x)       ;Y and X swapped
      (ctest-maploc loc))
    (format t "x=~d,y=~d" x y)
    (<- PROBE-OB ':GOTO-LOC LOC)
    (<- PROBE-OB ':PROBE-DOWN)
    (MULTIPLE-VALUE (XE YE)
      (<- PROBE-OB ':CLIMB-ON-PIN))
    (FORMAT T "~%XERR ~S, YERR ~S" XE YE))
    )

(defvar ctest-retry-count 0)

;SIGNAL-NAME is for printouts, plus it does different things for power and ground
(DEFUN TEST-WIRE (SIGNAL-NAME LOC1 LOC2)
  (MULTIPLE-VALUE-BIND (Y1 X1)
      (CTEST-MAPLOC LOC1)
    (MULTIPLE-VALUE-BIND (Y2 X2)
        (CTEST-MAPLOC LOC2)
      (COND ((< X1 X2)          ;left probe takes wire with greater X
             (SETQ X1 (PROG1 X2 (SETQ X2 X1)))
             (SETQ Y1 (PROG1 Y2 (SETQ Y2 Y1)))))
  (setq ctest-retry-count 0)
  (error-restart
      (prog (lx rx lxdiff rxdiff cxdiff lxe lye rxe rye)
            (cond ((equal signal-name "NC") (return t)))  ;that one isnt there!
        L0 (setq lx (<- left-probe ':xpos)
                 rx (<- right-probe ':xpos))
           (setq lxdiff (- x1 lx)
                 rxdiff (- x2 rx)
                 cxdiff (- lx rx))       ;current dist between probes
;In code below, we assume if probes are 250. mills apart in x direction, they cannot
; collide even if y's cross.
           (cond ((and (> lxdiff 0)                ;left probe moving away,
                       (> (+ lxdiff cxdiff) 250.))  ;and far enuf away
                  (<- left-probe ':goto x1 y1)     ;safe to move it.
                  (<- right-probe ':goto x2 y2))
                 ((and (> (minus rxdiff) 0)        ;same stuff, right probe
                       (> (+ (minus rxdiff) cxdiff) 250.))
                  (<- right-probe ':goto x2 y2)
                  (<- left-probe ':goto x1 y1))
                 ((> (min cxdiff (- x1 rx) (- lx x2) (- x1 x2)) 250.)
                  (<- right-probe ':goto x2 y2)    ;they are far apart and will stay far apart
                  (<- left-probe ':goto x1 y1))
;Try to arrange the second motion to involve a x-motion of at least 250 mills toward
; the other arm.
                 ((> (minus lxdiff) 250.)
                  (<- right-probe ':goto x2 y2)
                  (<- left-probe ':goto x1 y1))
                 ((> rxdiff 250.)
                  (<- left-probe ':goto x1 y1)
                  (<- right-probe ':goto x2 y2))
;cause one arm to step away from the other by 20 steps (about .2 inch), then try again.
                 ((zerop lxdiff)
                  (<- right-probe ':step-x -20.)
                  (go L0))
                 (t (<- left-probe ':step-x 20.)
                    (go L0)))
           (<- left-probe ':probe-down)
           (multiple-value (lxe lye) (<- left-probe ':climb-on-pin))
           (<- right-probe ':probe-down)
           (multiple-value (rxe rye) (<- right-probe ':climb-on-pin))
           (cond ((eq (ctest-test-cont signal-name t) 'win)
                  (<- LEFT-PROBE ':APPARENT-ERROR LXE LYE)  ;WON, SO PROBES MUST BE IN RIGHT
                  (<- RIGHT-PROBE ':APPARENT-ERROR RXE RYE)  ;PLACE
                  (return t))))))))

;definitions below basically copied from MC;WL;LG684 517

(DEFCONST LG684-%DIPG (BYTE-FIELD 4 19))        ;DIP GROUP (The letter)
(DEFCONST LG684-%DIPS (BYTE-FIELD 6 13))        ;DIP SLOT  (The number)
(DEFCONST LG684-%DIPOF (BYTE-FIELD 6 07))       ;DIP OFFSET

(DEFCONST LG684-%CONT (BYTE-FIELD 1 07))        ;CONNECTOR TYPE 0  DEC  1  FLAT CABLE "J"
(DEFCONST LG684-%CONN (BYTE-FIELD 4 23))        ;CONNECTOR NUMBER (LETTER OR JACK #)
;(DEFCONST LG684-%CONP (BYTE-FIELD 9 00))       ;CONNECTOR PIN #

(DEFCONST LG684-GPINS 400)              ;NOMEN FOR "G" PINS ON BOARD STARTS AT 400
                                        ; 401 IS "G1", ETC.
;BRDGND__=10                            ; PIN 10 IS DEDICATED GROUND
;BRDPWR__=20                            ; PIN 20 IS DEDICATED POWER
(DEFCONST LG684-MXDPIN 36)              ;MAXIMUM NUMBER OF DEC PINS/CONNECTOR
(DEFCONST LG684-NGRPS 6)                        ;max for group (the letter)
(DEFCONST LG684-MAXCON 12)              ;max for the jack (Jnn)

;The L8X30 board has an area of 16 pin DIP's, in 7 rows (A-H), columns 5-30
; and an 2 areas of 20 pin dips: rows A-F cols 1-4, and row J cols 1-30
; (There is no H row for columns 1-4)

;The LG684 board consists of 6 rows of 30 dips
;Row A, slot1 is the upper left (with connector paddles up,
; and from DIP side)

;AUGAT-LG684 CONNECTOR PIN FORMAT PRINTS AS J#-# OR LL#
;THE J IS LITERAL.

;     15    12     9                 0   LM bit numbering
;______|_____|_____|_____|_____|_____|
;|        22      26                35   PDP-10 bit numbering
;|       |1|   4   |        9        |
;|_______|_|_______|_________________|
;         |    |        |
;         |    |        |------------>PIN
;         |    |
;         |    |----------------------CONNECTOR NUMBER (JACK OR ROW LETTER)
;         |
;         |--------------------------->CONNECTOR TYPE 0  DEC, 1  FLAT CABLE





;All calculations are done from the DIP side.

;(0,0) at LOWER left hand corner in left handed coordinate system.
;X+ to right, Y+ is up
;0,0 corresponds to FV1 of DEC connectors.

(DEFCONST LG684-XDIPSP  500)    ; .500" DIP HORIZONTAL SPACING
(DEFCONST LG684-YDIPSP 1100)    ;1.100" DIP VERTICAL SPACING
(DEFCONST LG684-XGRPOF 200)     ;HORIZONTAL DISTANCE FROM ORIGIN TO DIP PIN10 OF F1
(DEFCONST LG684-YGRPOF 600)     ;VERTICAL DISTANCE FROM ORIGIN TO DIP PIN10 OF F1

(DEFCONST LG684-XPINSP  300)    ; .300" DIP PIN HORIZONTAL SPACING
(DEFCONST LG684-YPINSP  100)    ; .100" DIP PIN VERTICAL SPACING
(DEFCONST LG684-XGNDOFF -100)   ;OFFSET FROM DIP SLOT ORIGIN (ON EXTRA GROUND ROW) TO
                                ; FIRST DIP PIN ROW
;CONNECTORS, SCOTCHFLEX
(DEFCONST LG684-FCXOFF 100)     ;.1" RIGHT FOR FIRST PIN OF FLAT CABLE CONNECTORS
(DEFCONST LG684-FCYOFF 7400)    ;7.4" up to first pin of J2 relative to origin
(DEFCONST LG684-FCYOF2 300)     ;.3" up for first pin of J1 relative to J2
(DEFCONST LG684-FCJ3OF 2800)    ;2.8" RIGHT FOR FIRST PIN OF J3
(DEFCONST LG684-FCGOF 5100)     ;5.1" RIGHT FOR FIRST PIN OF J5

;CONNECTORS, DEC
(DEFCONST LG684-DECCNY 0)       ;0.0" UP TO BOTTOM ROW OF DEC CONNECTORS
(DEFCONST LG684-DCPINO 200)     ;.2" UP FROM BOTTOM TO SECOND ROW
(DEFCONST LG684-DECX1 2700)     ;2.7" LEFT FOR LARGE CONNECTOR SPACES
(DEFCONST LG684-DECX2 2600)     ;2.6" LEFT FOR SMALL CONNECTOR SPACES
(DEFCONST LG684-GAP1T2 (- LG684-DECX2 1900))    ; THE SMALLER GAP BETWEEN DEC PADDLES
(DEFCONST LG684-DCPINS 1900)    ;1.9" RIGHT FOR PIN A1 RELATIVE TO START OF CONNECTOR
(DEFCONST LG684-DCPNSP 100)     ;.1" BETWEEN PINS HORIZONTALLY
(DEFCONST LG684-DCGRSP 200)     ;.2" BETWEEN GROUPS OF PINS

(DEFVAR LG684-PADDLE-XOFFS NIL)
(DEFVAR LG684-PADDLE-YOFFS NIL)

(DEFVAR LG684-JACKSZ NIL)               ;array, for each J, max # pins. J0 illegal.
(DEFVAR LG684-JACK-XOFFS NIL)
(DEFVAR LG684-JACK-YOFFS NIL)

(DEFVAR DEC-EDGE-XOFFS NIL)    ;offset within dec edge connector.

(DEFUN LG684-PRNLOC (LOC &AUX ANS)
  (let ((group (ldb LG684-%DIPG loc))
        (conn (ldb LG684-%CONN loc))
        (pin (ldb ctest-%%pin loc)))
    (cond ((zerop conn)
           (let ((slot (ldb LG684-%DIPS loc))
                 (offset (ldb LG684-%DIPOF loc)))
             (setq ans (format nil "~C~2,48D" (+ #/@ group) slot))
             (cond ((not (zerop offset))
                    (setq ans (string-append ans (format nil "@~2,48D" offset)))))))
          ((ZEROP (LDB LG684-%CONT LOC))        ;DEC edge connector
           (SETQ ANS (FORMAT NIL "~C" (+ #/@ CONN))))   ;paddle letter just ABCDEF
          (t (setq ans (format nil "J~D" CONN))))
    (cond ((and (zerop conn)
                (>= pin LG684-GPINS))
           (SETQ ANS (format nil "~A-~1D" ANS (- pin LG684-GPINS))))    ;ground pin
          ((AND (NOT (ZEROP CONN))
                (ZEROP (LDB LG684-%CONT LOC)))
           (SETQ ANS (FORMAT NIL "~A~C~D"
                             ANS
                             (CTEST-NUMBER-TO-DEC-LETTER (LSH PIN -1))
                             (1+ (LOGAND PIN 1)))))
          (t (SETQ ANS (format nil "~A-~2,48D" ANS pin)))))
  ANS)

(DEFUN LG684-INIT (&AUX JN DX PN)
  (SETQ LG684-JACKSZ (MAKE-ARRAY 13.))  ;J0 illegal
  (DOTIMES (C 12.)
    (AS-1 (IF (BIT-TEST 1 C) 40 50)
          LG684-JACKSZ
          (1+ C)))
  (SETQ LG684-JACK-XOFFS (MAKE-ARRAY 13)
        LG684-JACK-YOFFS (MAKE-ARRAY 13))
  (SETQ JN 1)
  (DOTIMES (JGROUP 3)   ;3 groups, 4 connectors each
    (AS-1  (+ LG684-FCXOFF (* LG684-FCGOF JGROUP)) LG684-JACK-XOFFS JN) ;eg J1
    (AS-1  (+ LG684-FCYOFF LG684-FCYOF2) LG684-JACK-YOFFS JN)
    (SETQ JN (1+ JN))
    (AS-1  (+ LG684-FCXOFF (* LG684-FCGOF JGROUP)) LG684-JACK-XOFFS JN) ;eg J2
    (AS-1  LG684-FCYOFF LG684-JACK-YOFFS JN)
    (SETQ JN (1+ JN))
    (AS-1  (+ LG684-FCXOFF LG684-FCJ3OF (* LG684-FCGOF JGROUP)) LG684-JACK-XOFFS JN)    ;eg J3
    (AS-1  (+ LG684-FCYOFF LG684-FCYOF2) LG684-JACK-YOFFS JN)
    (SETQ JN (1+ JN))
    (AS-1  (+ LG684-FCXOFF LG684-FCJ3OF (* LG684-FCGOF JGROUP)) LG684-JACK-XOFFS JN)    ;eg J4
    (AS-1  LG684-FCYOFF LG684-JACK-YOFFS JN)
    (SETQ JN (1+ JN)))
  (SETQ LG684-PADDLE-XOFFS (MAKE-ARRAY 6)
        LG684-PADDLE-YOFFS (MAKE-ARRAY 6))
  (SETQ JN 5
        DX 0)
  (DOTIMES (JGROUP 3)
    (AS-1 DX LG684-PADDLE-XOFFS JN)
    (AS-1 LG684-DECCNY LG684-PADDLE-YOFFS JN)
    (SETQ JN (1- JN)
          DX (+ DX LG684-DECX1))
    (AS-1 DX LG684-PADDLE-XOFFS JN)
    (AS-1 LG684-DECCNY LG684-PADDLE-YOFFS JN)
    (SETQ JN (1- JN)
          DX (+ DX LG684-DECX2)))
  (SETQ DEC-EDGE-XOFFS (MAKE-ARRAY 18.))
  (SETQ DX 0
        PN 17.)
  (DOTIMES (PGROUP 3)
    (AS-1 DX DEC-EDGE-XOFFS PN)
    (SETQ PN (1- PN))
    (DOTIMES (C 5)
      (SETQ DX (+ DX LG684-DCPNSP))
      (AS-1 DX DEC-EDGE-XOFFS PN)
      (SETQ PN (1- PN)))
    (SETQ DX (+ DX LG684-DCGRSP))))

(DEFUN LG684-GETLOC (STR BEG LIM &AUX C VAL-LIST IDX)
  (IF (NULL BEG) (SETQ BEG 0))
  (IF (NULL LIM) (SETQ LIM (ARRAY-ACTIVE-LENGTH STR)))
  (MULTIPLE-VALUE (C VAL-LIST IDX)
    (LNPARSE '("P#-#(P#-#)" "P#-#(#)" "P#-#" "P#@#-#(#)" "P#@#-#" "PL#" "J#-#")
             STR BEG LIM))
  (SELECTQ C
    (0 (SETQ VAL-LIST (CDDDR VAL-LIST)) ;flush adaptor cruft
       (SI:DESTRUCTURING-BIND (LET NUM PIN) VAL-LIST
                              (DPB LET LG684-%DIPG
                                   (DPB NUM LG684-%DIPS
                                        (DPB PIN CTEST-%%PIN 0)))))
    (1 (SI:DESTRUCTURING-BIND (LET NUM IGNORE PIN) VAL-LIST
                              (DPB LET LG684-%DIPG
                                   (DPB NUM LG684-%DIPS
                                        (DPB PIN CTEST-%%PIN 0)))))
    (2 (SI:DESTRUCTURING-BIND (LET NUM PIN) VAL-LIST
                              (DPB LET LG684-%DIPG
                                   (DPB NUM LG684-%DIPS
                                        (DPB PIN CTEST-%%PIN 0)))))
    (3 (SI:DESTRUCTURING-BIND (LET NUM IGNORE IGNORE PIN) VAL-LIST
                              (DPB LET LG684-%DIPG
                                   (DPB NUM LG684-%DIPS
                                        (DPB PIN CTEST-%%PIN 0)))))
    (4 (SI:DESTRUCTURING-BIND (LET NUM IGNORE PIN) VAL-LIST
                              (DPB LET LG684-%DIPG
                                   (DPB NUM LG684-%DIPS
                                        (DPB PIN CTEST-%%PIN 0)))))
    (5 (SI:DESTRUCTURING-BIND (LET PADDLE-LET PADDLE-SIDE) VAL-LIST
                              (DPB LET LG684-%CONN
                                   (DPB (+ (LSH PADDLE-LET 1)  ;already dec-letter hacked
                                           (1- PADDLE-SIDE))
                                        CTEST-%%PIN
                                        0))))   ;LG684-%CONT => 0
    (6 (SI:DESTRUCTURING-BIND (J-NUM PIN) VAL-LIST
                              (DPB J-NUM LG684-%CONN
                                   (DPB PIN CTEST-%%PIN
                                        (DPB 1 LG684-%CONT 0))))))
)

(DEFUN LG684-MAPLOC (LOC)   ;returns x and y, but swap these to get tester coords
  (prog (x y group conn pin slot offset)
        (setq group (ldb LG684-%DIPG loc)
              conn  (ldb LG684-%CONN loc)
              pin   (ldb ctest-%%pin loc))
        (setq y 0 x 0)
        (cond ((zerop conn)
               (if (> group LG684-NGRPS)
                   (ferror nil "group number too high ~s" group))
               (setq slot (ldb LG684-%DIPS loc)
                     offset (ldb LG684-%DIPOF loc))
               (setq x LG684-XGRPOF y LG684-YGRPOF)
               (setq x (+ x (* (1- slot) LG684-XDIPSP)
                          (ar-1 twenty-pin-socket-xoff (1- pin)))
                     y (+ y (* (- 6 group) LG684-YDIPSP)
                              (ar-1 twenty-pin-socket-yoff (1- pin)))))
              ((ZEROP (LDB LG684-%CONT LOC))    ;DEC edge connector
               (IF (OR (ZEROP CONN)
                       (NOT (<= CONN LG684-MAXCON)))
                   (FERROR NIL "bad conn number"))
               (SETQ X (+ X (AR-1 LG684-PADDLE-XOFFS (1- CONN))
                          (AR-1 DEC-EDGE-XOFFS (1- (LSH PIN -1))))
                     Y (+ Y (AR-1 LG684-PADDLE-YOFFS (1- CONN))
                          (* LG684-DCPINO (LOGAND PIN 1)))))
              (t (let ((maxp (ar-1 lg684-jacksz conn)))
                   (if (> pin maxp) (ferror nil "PIN NUMBER TOO HIGH"))
                   (setq x (+ x (ar-1 (ar-1 flat-cable-conn-xoffs maxp) (1- pin))
                              (AR-1 LG684-JACK-XOFFS CONN))
                         y (+ y (ar-1 (ar-1 flat-cable-conn-yoffs maxp) (1- pin))
                              (AR-1 LG684-JACK-YOFFS CONN)))
                   )))
        (return (- 9000 x) (- 17000 y)
)))  ;board fits in tester backwards ..


(DEFUN CTEST-NUMBER-TO-DEC-LETTER (NUM)
  (IF (>= NUM #/G) (SETQ NUM (1+ NUM)))
  (IF (>= NUM #/I) (SETQ NUM (1+ NUM)))
  (IF (>= NUM #/O) (SETQ NUM (1+ NUM)))
  (IF (>= NUM #/Q) (SETQ NUM (1+ NUM)))
  (+ #/@ NUM))

(DEFUN CTEST-DEC-LETTER-TO-NUMBER (NUM)
  (SETQ NUM (+ NUM #/@))
  (COND ((MEMQ NUM '(#/G #/I #/O #/Q))
         (FERROR NIL "~C invalid DEC letter" NUM)))
  (IF (> NUM #/Q) (SETQ NUM (1- NUM)))
  (IF (> NUM #/O) (SETQ NUM (1- NUM)))
  (IF (> NUM #/I) (SETQ NUM (1- NUM)))
  (IF (> NUM #/G) (SETQ NUM (1- NUM)))
  (SETQ NUM (- NUM #/@))
  NUM)

;---

(DEFVAR CTEST-LINES NIL)
(DEFVAR CTEST-RUNS NIL)
(DEFVAR CTEST-BAD-RUNS NIL)
(DEFVAR CTEST-FILE NIL)

(DEFUN CTEST-READIN (&OPTIONAL (FILE "CADRWD;ICMEM3 WLR")
                               (SKIP-COUNT 0)
                               (READ-COUNT 2000000))
  (LET ((STREAM (OPEN FILE '(IN))))
     (SETQ CTEST-LINES (CTEST-READLINES STREAM SKIP-COUNT READ-COUNT))
     (CLOSE STREAM))
 (FORMAT T "~%Readin completed, parsing runs")
 (SETQ CTEST-RUNS (CTEST-PARSE-RUNS CTEST-LINES))
 (SETQ CTEST-RUNS (APPEND CTEST-RUNS NIL))
 (FORMAT T "~%Parse completed, starting phase 1 sort. Total travel =~D"
         (CTEST-TOTAL-TRAVEL CTEST-RUNS))
 (SETQ CTEST-RUNS (SORT CTEST-RUNS (FUNCTION RUN-LEFT-PROBE-X-LOCN-<)))
 (FORMAT T "~%Starting phase 2 sort. Total travel =~D"
         (CTEST-TOTAL-TRAVEL CTEST-RUNS))
 (SETQ CTEST-RUNS (CTEST-SORT CTEST-RUNS))
 (FORMAT T "~%Final total travel ~D" (CTEST-TOTAL-TRAVEL CTEST-RUNS)))

(DEFUN CTEST-PROCESS-WLR (FILE &OPTIONAL (SKIP-COUNT 0) (ALREADY-IN NIL))
  (SETQ CTEST-FILE FILE)
  (COND ((NULL ALREADY-IN)
         (CTEST-READIN FILE)))
  (CTEST-TRY-TESTING SKIP-COUNT)
  (CTEST-WRITE-BAD-RUNS "RG;LOSERS >" CTEST-BAD-RUNS 'END)
)

(DEFUN CTEST-WRITE-BAD-RUNS (FILE BAD PLACE)
  (LET ((STREAM (OPEN FILE '(OUT))))
    (FORMAT STREAM "~%PROCESSING ~A RN ~D~%" CTEST-FILE PLACE)
    (GRIND-TOP-LEVEL BAD 80. STREAM)
    (CLOSE STREAM)))

(DEFUN CTEST-READ-BAD-RUNS (FILE)
  (LET ((STREAM (OPEN FILE)))
    (READLINE STREAM)
    (READLINE STREAM)
    (SETQ CTEST-BAD-RUNS (READ STREAM))
    (CLOSE STREAM)))

;RETEST BAD RUNS.  NOTE THEY ARE IN TEXT FORM
(DEFUN CTEST-RETEST-BAD-RUNS (&OPTIONAL (BAD-RUNS CTEST-BAD-RUNS))
  (SETQ CTEST-OLD-BAD-RUNS CTEST-BAD-RUNS)
  (SETQ CTEST-BAD-RUNS NIL)
  (CTEST-TRY-TESTING 0 (MAPCAR (FUNCTION CTEST-UNCONVERT-RUN)
                               (MAPCAR (FUNCTION CAR) BAD-RUNS))))

(DEFUN CTEST-FIND-RUN-ENDING (STRING-LOC &OPTIONAL (RUNS CTEST-RUNS))
  (PROG (LOC L RN)
        (SETQ LOC (CTEST-GETLOC STRING-LOC)
              L RUNS
              RN 0)
     L  (COND ((NULL L) (RETURN NIL))
              ((OR (= LOC (CAR (CADR (CAR L))))
                   (= LOC (CAR (LAST (CADR (CAR L))))))
               (RETURN RN L)))
        (SETQ L (CDR L) RN (1+ RN))
        (GO L)))

(DEFUN CTEST-FIND-RUN-CONTAINING (STRING-LOC &OPTIONAL (RUNS CTEST-RUNS))
  (PROG TOP (LOC L RN)
        (SETQ LOC (CTEST-GETLOC STRING-LOC)
              L RUNS
              RN 0)
     L  (COND ((NULL L) (RETURN NIL)))
        (DOLIST (LK (CADR (CAR L)))
          (COND ((= LOC LK)
                 (RETURN-FROM TOP RN L))))
        (SETQ L (CDR L) RN (1+ RN))
        (GO L)))

(DEFUN CTEST-FIND-RUN (RUN-NAME &OPTIONAL (RUNS CTEST-RUNS))
  (PROG (L RN)
        (SETQ RN 0 L RUNS)
     L  (COND ((NULL L) (RETURN NIL))
              ((EQUAL RUN-NAME (CAAR L))
               (RETURN RN L)))
        (SETQ RN (1+ RN) L (CDR L))
        (GO L)))

(defun ctest-try-testing (&optional (skip-count 0) (runs ctest-runs))
  (prog (l rn wins losses)
        (setq runs (nthcdr skip-count runs))
        (setq l runs rn skip-count wins 0 losses nil)
     l  (cond ((null l)
               (return wins losses)))
        (let ((run (car l)))
          (cond ((> (length (cadr run)) 1)
                 (cond ((test-wire (car run) (car (cadr run)) (car (last (cadr run))))
                        (setq wins (1+ wins))
                        (tv:beep))
                       (t (setq losses (cons run losses))
                          (SETQ CTEST-BAD-RUNS (CONS (CTEST-TEST-SEGMENTS RUN)
                                                     CTEST-BAD-RUNS))
                          (CTEST-WRITE-BAD-RUNS "RG; CTEST PLACE" CTEST-BAD-RUNS RN))))))
        (setq l (cdr l))
        (setq rn (1+ rn))
        (go l)))

(DEFUN CTEST-TEST-SEGMENTS (RUN)
  (PROG (LOC-LIST ANS SEG-NO)
        (SETQ LOC-LIST (CADR RUN) SEG-NO 0)
   L    (COND ((NULL (CDR LOC-LIST))
               (RETURN (LIST (CTEST-CONVERT-RUN RUN) ANS))))
        (SETQ SEG-NO (1+ SEG-NO))
        (COND ((TEST-WIRE (CAR RUN)
                          (CAR LOC-LIST)
                          (CADR LOC-LIST))
               (TV:BEEP))
              (T (SETQ ANS (CONS (LIST (CTEST-LOC (CAR LOC-LIST))
                                       (CTEST-LOC (CADR LOC-LIST)))
                                 ANS))))
        (SETQ LOC-LIST (CDR LOC-LIST))
        (GO L)))

(DEFUN RUN-LEFT-PROBE-X-LOCN-< (R1 R2 &AUX R1X1 R1Y1 R1X2 R1Y2 R2X1 R2Y1 R2X2 R2Y2)
  (MULTIPLE-VALUE (R1Y1 R1X1) (CTEST-MAPLOC (CAR (CADR R1))))
  (MULTIPLE-VALUE (R1Y2 R1X2) (CTEST-MAPLOC (CAR (LAST (CADR R1)))))
  (COND ((< R1X1 R1X2)          ;left probe takes wire with greater X
         (SETQ R1X1 (PROG1 R1X2 (SETQ R1X2 R1X1)))
         (SETQ R1Y1 (PROG1 R1Y2 (SETQ R1Y2 R1Y1)))))
  (MULTIPLE-VALUE (R2Y1 R2X1) (CTEST-MAPLOC (CAR (CADR R2))))
  (MULTIPLE-VALUE (R2Y2 R2X2) (CTEST-MAPLOC (CAR (LAST (CADR R2)))))
  (COND ((< R2X1 R2X2)          ;left probe takes wire with greater X
         (SETQ R2X1 (PROG1 R2X2 (SETQ R2X2 R2X1)))
         (SETQ R2Y1 (PROG1 R2Y2 (SETQ R2Y2 R2Y1)))))
  (< R1X1 R2X1))

(DEFUN RUN-LEFT-PROBE-Y-LOCN-< (R1 R2 &AUX R1X1 R1Y1 R1X2 R1Y2 R2X1 R2Y1 R2X2 R2Y2)
  (MULTIPLE-VALUE (R1Y1 R1X1) (CTEST-MAPLOC (CAR (CADR R1))))
  (MULTIPLE-VALUE (R1Y2 R1X2) (CTEST-MAPLOC (CAR (LAST (CADR R1)))))
  (COND ((< R1X1 R1X2)          ;left probe takes wire with greater X
         (SETQ R1X1 (PROG1 R1X2 (SETQ R1X2 R1X1)))
         (SETQ R1Y1 (PROG1 R1Y2 (SETQ R1Y2 R1Y1)))))
  (MULTIPLE-VALUE (R2Y1 R2X1) (CTEST-MAPLOC (CAR (CADR R2))))
  (MULTIPLE-VALUE (R2Y2 R2X2) (CTEST-MAPLOC (CAR (LAST (CADR R2)))))
  (COND ((< R2X1 R2X2)          ;left probe takes wire with greater X
         (SETQ R2X1 (PROG1 R2X2 (SETQ R2X2 R2X1)))
         (SETQ R2Y1 (PROG1 R2Y2 (SETQ R2Y2 R2Y1)))))
  (< R1Y1 R2Y1))

(DEFUN CTEST-TOTAL-TRAVEL (&OPTIONAL (RUNS CTEST-RUNS) (SEGMENTS 10000000))
  (PROG (X1 X2 Y1 Y2 CR ANS P RX1 RY1 RX2 RY2 DIST)
        (SETQ ANS 0 P RUNS)
        (COND ((NULL P) (RETURN ANS)))
        (SETQ CR (CAR P))
        (MULTIPLE-VALUE (Y1 X1) (CTEST-MAPLOC (CAR (CADR CR))))
        (MULTIPLE-VALUE (Y2 X2) (CTEST-MAPLOC (CAR (LAST (CADR CR)))))
        (COND ((< X1 X2)                ;left probe takes wire with greater X
               (SETQ X1 (PROG1 X2 (SETQ X2 X1)))
               (SETQ Y1 (PROG1 Y2 (SETQ Y2 Y1)))))
        (SETQ P (CDR P))
    L   (COND ((OR (< (SETQ SEGMENTS (1- SEGMENTS)) 0)
                   (NULL P))
               (RETURN ANS)))
        (SETQ CR (CAR P))
        (MULTIPLE-VALUE (RY1 RX1) (CTEST-MAPLOC (CAR (CADR CR))))
        (MULTIPLE-VALUE (RY2 RX2) (CTEST-MAPLOC (CAR (LAST (CADR CR)))))
        (COND ((< RX1 RX2)
               (SETQ RX1 (PROG1 RX2 (SETQ RX2 RX1)))
               (SETQ RY1 (PROG1 RY2 (SETQ RY2 RY1)))))
        (SETQ DIST (+ (ABS (- RX1 X1)) (ABS (- RY1 Y1))
                      (ABS (- RX2 X2)) (ABS (- RY2 Y2))))
        (SETQ X1 RX1 Y1 RY1 X2 RX2 Y2 RY2)
        (SETQ ANS (+ ANS DIST))
        (SETQ P (CDR P))
        (GO L)))

(DEFUN CTEST-SEGMENT-TRAVEL (R1 R2)
  (PROG (X1 X2 Y1 Y2 RX1 RY1 RX2 RY2)
        (MULTIPLE-VALUE (Y1 X1) (CTEST-MAPLOC (CAR (CADR R1))))
        (MULTIPLE-VALUE (Y2 X2) (CTEST-MAPLOC (CAR (LAST (CADR R1)))))
        (COND ((< X1 X2)                ;left probe takes wire with greater X
               (SETQ X1 (PROG1 X2 (SETQ X2 X1)))
               (SETQ Y1 (PROG1 Y2 (SETQ Y2 Y1)))))
        (MULTIPLE-VALUE (RY1 RX1) (CTEST-MAPLOC (CAR (CADR R2))))
        (MULTIPLE-VALUE (RY2 RX2) (CTEST-MAPLOC (CAR (LAST (CADR R2)))))
        (COND ((< RX1 RX2)
               (SETQ RX1 (PROG1 RX2 (SETQ RX2 RX1)))
               (SETQ RY1 (PROG1 RY2 (SETQ RY2 RY1)))))
        (RETURN (+ (ABS (- RX1 X1)) (ABS (- RY1 Y1))
                   (ABS (- RX2 X2)) (ABS (- RY2 Y2))))))

(DEFUN CTEST-N-OPTIMIZE (N &OPTIONAL (RUNS CTEST-RUNS))
  (PROG (LP L)
        (SETQ LP (LOCF RUNS) L (LENGTH RUNS))
   L    (COND ((NULL (CDR LP)) (RETURN RUNS)))
        (RPLACD LP (COND ((<= L N)
                          (CTEST-PERMUTE (CDR LP)))
                         (T
                           (NCONC (CTEST-PERMUTE (FIRSTN N (CDR LP)))
                                  (NTHCDR N (CDR LP))))))
        (SETQ LP (CDR LP) L (1- L))
        (GO L)
))

(DEFUN CTEST-DIF (RL1 RL2)
  (PROG (C)
        (SETQ C 0)
    L   (COND ((OR (NULL RL1) (NULL RL2)
                   (NOT (EQUAL (CAR RL1) (CAR RL2))))
               (RETURN C)))
        (SETQ C (1+ C) RL1 (CDR RL1) RL2 (CDR RL2))
        (GO L)))

;FIRST FROB IS STARTING POINT, LAST ENDING POINT, SO ACTUAL PERMUTATION IS
; ON N-2 FROBS.
(DEFUN CTEST-PERMUTE (SEG)
   (PROG (BSF BSF-DIST P1 P2 DIST P2-DIST TRY TRY-RESULT TRY-DIST)
         (COND ((NULL (CDR SEG)) (RETURN SEG 0))
               ((NULL (CDDR SEG)) (RETURN SEG (CTEST-SEGMENT-TRAVEL (CAR SEG) (CADR SEG))))
               ((NULL (CDDDR SEG)) (RETURN SEG (+ (CTEST-SEGMENT-TRAVEL (CAR SEG)
                                                                        (CADR SEG))
                                                  (CTEST-SEGMENT-TRAVEL (CADR SEG)
                                                                        (CADDR SEG))))))
         (SETQ P1 (CAR SEG) SEG (CDR SEG))
         (DOTIMES (C (1- (LENGTH SEG)))
           (SETQ P2 (NTH C SEG))
           (SETQ P2-DIST (CTEST-SEGMENT-TRAVEL P1 P2))
           (COND ((OR (NULL BSF-DIST)
                      (< P2-DIST BSF-DIST))
                  (MULTIPLE-VALUE (TRY-RESULT TRY-DIST)
                    (CTEST-PERMUTE (SETQ TRY (CONS P2 (BUTNTH C SEG)))))
                  (SETQ DIST (+ TRY-DIST P2-DIST))
                  (COND ((OR (NULL BSF-DIST)
                             (< DIST BSF-DIST))
                         (SETQ BSF TRY-RESULT BSF-DIST DIST))))))
         (RETURN (CONS P1 BSF) BSF-DIST)))

;RANDOMIZE, LEAVING FIRST AND LAST THE SAME.
(DEFUN CTEST-RANDOMIZE (RUNS &AUX N C FIRST LAST MIDDLE)
  (SETQ FIRST (CAR RUNS) LAST (CAR (LAST RUNS)) MIDDLE (BUTLAST (CDR RUNS)))
  (SETQ N (LENGTH MIDDLE))
  (DOTIMES (X N)
    (SETQ C (RANDOM N))
    (SETQ MIDDLE (CONS (NTH C MIDDLE) (BUTNTH C MIDDLE))))
  (NCONC (LIST FIRST) MIDDLE (LIST LAST)))

(DEFUN BUTNTH (N LST)
  (NCONC (FIRSTN N LST) (NTHCDR (1+ N) LST)))

;CHOOSE A WIRE AT RANDOM (THE FIRST ONE ACTUALLY).  THEN FIND THE WIRE THAT
; CAN BE TESTED WITH THE LEAST PROBE MOVEMENT, ETC.
(DEFUN CTEST-SORT (RUNS)
  (PROG (ANS P CR X1 Y1 X2 Y2 OR RX1 RY1 RX2 RY2 DIST PP LPP BSF BSF-LPP BSF-DIST TEM
         WC)
        (SETQ ANS (APPEND RUNS NIL))
        (SETQ P ANS)            ;FIRST ELEM P POINTS AT CONSIDERED SORTED. REST NOT.
    L0  (COND ((NULL P)
               (RETURN ANS)))
        (SETQ CR (CAR P))
        (MULTIPLE-VALUE (Y1 X1) (CTEST-MAPLOC (CAR (CADR CR))))
        (MULTIPLE-VALUE (Y2 X2) (CTEST-MAPLOC (CAR (LAST (CADR CR)))))
        (COND ((< X1 X2)                ;left probe takes wire with greater X
               (SETQ X1 (PROG1 X2 (SETQ X2 X1)))
               (SETQ Y1 (PROG1 Y2 (SETQ Y2 Y1)))))
        (SETQ LPP P PP (CDR P) BSF NIL BSF-LPP NIL)
        (SETQ WC 0)
    L   (COND ((NULL PP)
               (GO GOBBLE)))
        (SETQ OR (CAR PP))
        (MULTIPLE-VALUE (RY1 RX1) (CTEST-MAPLOC (CAR (CADR OR))))
        (MULTIPLE-VALUE (RY2 RX2) (CTEST-MAPLOC (CAR (LAST (CADR OR)))))
        (COND ((< RX1 RX2)
               (SETQ RX1 (PROG1 RX2 (SETQ RX2 RX1)))
               (SETQ RY1 (PROG1 RY2 (SETQ RY2 RY1)))))
        (SETQ DIST (+ (ABS (- RX1 X1)) (ABS (- RY1 Y1))
                      (ABS (- RX2 X2)) (ABS (- RY2 Y2))))
        (COND ((OR (NULL BSF)
                   (< DIST BSF-DIST))
               (SETQ BSF OR BSF-LPP LPP BSF-DIST DIST)))
        (COND ((AND BSF (OR (> (SETQ WC (1+ WC)) 50.)
                            (> (ABS (- RX1 X1))
                               BSF-DIST)))  ;CANT FIND A BETTER ONE
               (GO GOBBLE)))
        (SETQ LPP PP PP (CDR PP))
        (GO L)
 GOBBLE (COND (BSF-LPP
                (SETQ TEM (CDR BSF-LPP))        ;SAVE PNTR TO THIS GUY
                (RPLACD BSF-LPP (CDDR BSF-LPP)) ;SPLICE HIM OUT
                (RPLACD TEM (CDR P))            ;UNSORTED TAIL
                (COND ((EQ P TEM) (BREAK FOO T)))
                (RPLACD P TEM)))                ;ADD TO SORTED PART
        (SETQ P (CDR P))
        (GO L0)
))


(DEFUN CTEST-READLINES (STREAM &OPTIONAL (SKIP-COUNT 0) (READ-COUNT 100000))
  (PROG (P V TEM)
        (DOTIMES (C SKIP-COUNT)         ;FLUSH THOSE
          (COND ((EQ (SETQ TEM (READLINE STREAM 'EOF)) 'EOF)
                 (RETURN NIL)))
          (RETURN-ARRAY TEM))
        (SETQ P (LOCF V))
    L   (COND ((OR (< (SETQ READ-COUNT (1- READ-COUNT)) 0)
                   (EQ (SETQ TEM (READLINE STREAM 'EOF)) 'EOF))
               (RETURN V)))
        (RPLACD P (SETQ P (LIST TEM)))
        (GO L)))

(DEFUN CTEST-UNCONVERT-RUN (RUN)
  (LIST (CAR RUN) (MAPCAR (FUNCTION CTEST-GETLOC) (CADR RUN))))

(DEFUN CTEST-CONVERT-RUN (RUN)
  (LIST (CAR RUN) (MAPCAR (FUNCTION CTEST-LOC) (CADR RUN))))

(DEFUN CTEST-PRINT-RUNS (&OPTIONAL (RUNS CTEST-RUNS))
  (DOLIST (RUN RUNS)
    (PRINT (CTEST-CONVERT-RUN RUN))))

(DEFUN CTEST-PARSE-RUNS (LINES)
  (PROG (PTR RUN-NAME TYPE SYM LOC CURRENT-RUN RUNS RUNP)
        (SETQ PTR LINES
              RUNP (LOCF RUNS))
    L0  (IF (NULL PTR) (FERROR NIL "unable to find runs part of wlr file"))
        (MULTIPLE-VALUE (TYPE SYM LOC) (CTEST-LINE-TYPE (CAR PTR)))
        (SETQ PTR (CDR PTR))
        (IF (NEQ TYPE 'PAGE-HEADER) (GO L0))
        (IF (NOT (STRING-EQUAL (CAR PTR) "SIGNAL NAME" 0 0 11. 11.))
            (GO L0))
    L   (COND ((NULL PTR) (RETURN RUNS)))
        (MULTIPLE-VALUE (TYPE SYM LOC) (CTEST-LINE-TYPE (CAR PTR)))
        (COND ((EQ TYPE 'RUN-HEADER)
               (SETQ RUN-NAME SYM)
               (SETQ CURRENT-RUN (NCONC CURRENT-RUN (LIST LOC))))
              ((EQ TYPE 'RUN-NAME)
               (SETQ RUN-NAME SYM))
              ((EQ TYPE 'RUN-ITEM)
               (SETQ CURRENT-RUN (NCONC CURRENT-RUN (LIST LOC))))
              ((EQ TYPE 'BLANK)
               (COND (CURRENT-RUN
                       (RPLACD RUNP (SETQ RUNP (LIST (LIST RUN-NAME CURRENT-RUN))))))
               (SETQ CURRENT-RUN NIL)))
        (SETQ PTR (CDR PTR))
        (GO L)))
;Returns line type:
; RUN-HEADER starts with signal name, followed by pin number
; RUN-NAME  starts with signal name, then nothing else at all.
;           Wirelister puts this out when signal name is too long.
; RUN-ITEM   blank first field, followed by pin number
; RANDOM     non-null, but unable to make sense out of it
; BLANK  (completely blank, or starts with 2 tabs)

(DEFUN CTEST-LINE-TYPE (LINE)
  (PROG (LEN CH SYM IDX LOC)
        (SETQ IDX 0)
        (COND ((OR (ZEROP (SETQ LEN (ARRAY-ACTIVE-LENGTH LINE)))
                   (AND (> LEN 1)
                        (= (AR-1 LINE 0) #\TAB)
                        (= (AR-1 LINE 1) #\TAB)))
               (RETURN 'BLANK) )
              ((= (SETQ CH (AR-1 LINE IDX)) #\FORM)
               (RETURN 'PAGE-HEADER))
              ((AND (< CH 128.)
                    (NOT (= CH #\SPACE)))
               (MULTIPLE-VALUE (SYM IDX)
                 (CTEST-GETSYL LINE IDX '(#\TAB)))  ;SPACE CAN BE PART OF SIG NAME
               (COND ((OR (= IDX LEN)
                          (AND (< (+ IDX 4) LEN)        ;"NC" IS FOLLOWED BY A BUNCH OF TABS!
                               (= (AR-1 LINE IDX) #\TAB)
                               (= (AR-1 LINE (1+ IDX)) #\TAB)
                               (= (AR-1 LINE (+ 2 IDX)) #\TAB)
                               (= (AR-1 LINE (+ 3 IDX)) #\TAB)))
                      (RETURN 'RUN-NAME SYM NIL)))))
        (COND ((AND (= (AR-1 LINE IDX) #\TAB)
                    (SETQ LOC (CTEST-GETLOC LINE (1+ IDX))))
               (RETURN (COND (SYM 'RUN-HEADER)
                             (T 'RUN-ITEM))
                       SYM
                       LOC)))
        (RETURN 'RANDOM)
 ))

(DEFUN CTEST-GETSYL (STRING IDX &OPTIONAL (TERMS '(#\SPACE #\TAB)))
  (PROG (CH LIM FIN)
        (SETQ LIM (ARRAY-ACTIVE-LENGTH STRING))
    L0  (COND ((NOT (< IDX LIM))
               (RETURN NIL IDX))
              ((MEMQ (SETQ CH (AR-1 STRING IDX)) TERMS)
               (SETQ IDX (1+ IDX))
               (GO L0)))
        (SETQ FIN (1+ IDX))
    L1  (COND ((OR (NOT (< FIN LIM))
                   (MEMQ (SETQ CH (AR-1 STRING FIN)) TERMS))
               (RETURN (NSUBSTRING STRING IDX FIN) FIN)))
        (SETQ FIN (1+ FIN))
        (GO L1)))

(ctest-conn-and-socket-init)
(lg684-init)
