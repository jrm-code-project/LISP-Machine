;            -*-     Mode:Lisp;     Package:Hacks;    Base:10.    -*-
;****************************************************************************************
;  treedv . module   12-29-82

(defun treedv ()

  " This routine is a  second pass at a three dimensional segmented vector display system
    for the lisp machines.

      The routines in this module depend directly on the modules:
         COLORHACK.LISP   - routines for drawing on color screen.
                     - can't use COLOR.lisp routines because need to write to array,
                       not to a screen.

      Interesting routines are:
         GO1  - draws a rotating airplane
         GO2  - draws airplane with mouse control over orientation
         GO3  - draws row of boxes
         GO4  - draws sin(x)/x hilly plot
         GO5  - 3D viewer with colored glasses on 4-bit displays or crossed eyes on
                1-bit displays.

         SWAP-SCREENS - swaps between 4-bit and one-bit displays.
         T-OPRINT     - lists current display segments
         T-SPRINT <seg-name> - lists numbers in a segment
         T-TOP-PRINT  - lists top of transformation stack

   The improvements over the old version are as follows:

     1) Data is stored in arrays instead of lists to avoid having
        to run the garbage collector.

     2) Most of the graphics calls are to the microcode routines instead of through the
        window system.

     3) Transformation and drawing segments are in the same format.

     4) Bitblitting between two screen arrays to smooth the transition between rapidly
        drawn elements.

     5) Able to draw on the color screen, in a variety of colors.
        Needs the module COLOR.LISP loaded in for this to work.
        Use the routine (SWAP-WINDOWS) to use 4-bit color screen
    Improvments still needed to be implemented:

     1) Converting most of the routine to 16 (or 24 ?) bit integer arithmitic
        Still doesn't work well - need to work on scaling for windp/mast problems
         so that don't end up with 16+ bit magnitude difference in transform ?
        Note that fix-mode almost works, but transformation arrays need alot of
        scaling.  For general purposes however, any reference to fix-mode is
        useless.

        Work on display/draw to make simpler

    Notes: should set three first defvars to defconst and recomp program when I get
           the fixed point stuff working. [which I never will]
    J.Bradstreet  6-6-83
: - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  - - - - "
  (3d-document))

;****************************************************************************************

(defun 3d-document ()
 (format t "Treedv.module

      This module contains routines to do three dimensional segmented vector
   graphics on lisp machines in 1 bit or 4 bit displays.  Calculations are
   resonably fast, so updates can be made every 1 - 10 seconds depending on the
   complexity of what is being drawn and the type of screen being used.

      These routines are the a direct outgrowth of Newman & Sproull's book
   on computer graphics, and an Evans & Sutherland vector graphics system.
   Each particular object is stored as it is created into a data `segment`.
   The most primitive data types are points and lines.  The advantage of
   these segments is that it is possible to consturct more complicated
   objects out of them, no matter how complicated they happen to be.  For
   example, a square can be constructed from four primitive line segments.
   Once this square is consturcted, it is then possible to construct
   checkerboards, cubes, and similar sorts of things out of just these
   squares and the various transformation routines.  Any level of nesting
   (within reason) of segments within segments can be accomidated, squares
   out of lines, cubes out of squares, houses out of cubes, cities out of
   houses, and so on.

     Interesting features of this module are:
   1) Color graphics capability on the 4-bit color displays.
   2) Swaping between two display frames, so that one is displayed while the other
      is being updated.
   3) A three-D line clipping routine lifted from Newman & Sproull.
   4) Stereo viewing capability using colored (red/cyan) glasses on a 4-bit color display,
      or crossed eyes on the one bit display.
   5) Stack for transformation arrays.
   6) Ease of input of data sturctures.
   7) ... Possible simple adaptablility to Array Processors ??? ...
   8) ...

     There are several demos at the end of this module to demonstate some of its
   capabilities.  Although the routines all run slower than desirable, they do
   run.  Need an array processor and a faster way to generate lines in order to make them
   run faster.

    See the routines GO-TEST, GO0, GO1, ... ,GO5 for resonably documented programs,
   in particular the routine GO-TEST.

  John Bradstreet  6-8-83
----------------------------------------------------------------------------------------"))

(setq base 10. ibase 10.)

(defvar tdv*array-type 'art-float "Array type used in most arrays in this routine")
(defvar tdv*fix-mode    nil       "If true, uses half-fix math in transform arrays")
(defvar tdv*basic-w       1.0     "Basic w.  Should be 1 for float cals, 32767 for half-fix")

(defconst %pi-float  3.1415926536 "Pi to 10 or so places -- floating point")

(defvar tdv*window            nil "window used for graphic calls. (Set in t-window-init)")
(defvar tdv*color-mode        nil "true if drawing to color windows")
(defvar tdv*current-color     nil "Color drawing lines with when in color mode")

(defvar tdv*window-half-xdim  nil "half width of graphics display window in pixels")
(defvar tdv*window-half-ydim  nil "half height of graphics display window in pixels")
(defvar tdv*window-xcenter    nil "center of graphics display window in pixels from origin")
(defvar tdv*window-ycenter    nil "center of graphics display window in pixels from origin")
(defvar tdv*gen-window-size 16384 "Size of window in generalized coordinates")

(defvar tdv*stack-max-length  20  "Maximum length of transform stack")
(defvar tdv*stack-array       nil "stack for transform arrays used in this routine")
(defvar tdv*stack-length      0   "stack for transform arrays used in this routine")
(defvar tdv*stack-top         nil "Top array in transform stack")
(defvar tdv*stack-document    nil "Contains english information that describes tarray")

(defvar tdv*temp-array        nil "Temp array used in all transform routines")
(defvar tdv*temp-array1       nil "Temp array used wind and windp routines")
(defvar tdv*temp-array2       nil "Temp array used wind and windp routines")
(defvar tdv*temp-out-vect     nil "Temp array used in most vector routines")
(defvar tdv*temp-float-vect   nil "Temp floating point array used in some vector routines")
(defvar tdv*tfun-array        nil "Temp array used only inside of routine t-fun")
(defvar tdv*temp-float-array  nil "Temp array used only in T-Array-multiply")

(defvar s*max-segs   nil     " maximum number of segemts allowable - set in S-ROUTINE-INIT")
(defvar s*latest-seg 0       " last segment created")
(defvar s*open-seg nil       " number of the top level open segment")
(defvar s*open-seg-stack nil " Stack containing list of open segments")
(defvar s*open-seg-array nil " Array containing info about open segments")

(defvar s*seg-arrays     nil " Array containing all segment lists")
(defvar s*seg-lengths    nil " Array containing current lengths of all segments")

(defvar s*hash-table   nil   " Hash Table to go from seg names to numbers")
(defvar s*seg-names   nil    " Names of segments in hash table")

(setq tdv*temp-float-vect (make-array 5 ':type 'art-float))

(defun swap-fix-mode ()
  (ferror t "~% Fix Point Calcs don't work yet")
  (Setq tdv*fix-mode (not tdv*fix-mode))
  (COND (tdv*fix-mode
         (setq tdv*array-type 'art-half-fix
               tdv*basic-w 32767.))
        (T
         (setq tdv*array-type 'art-float
               tdv*basic-w 1.0))))

;****************************************************************************************


;****************************************************************************************
;                                                                           Macros
;****************************************************************************************

(defmacro WITH-T-PUSH (&body body)
  `(progn 'compile
     (T-PUSH)
     ,@body
     (T-POP)))

(defmacro WITH-A-NEW-SCREEN (&body body)
  `(progn 'compile
     (S-CLEAR-SCREEN)
     ,@body
     (S-DISPLAY-SCREEN)))

(Defmacro WITH-NEW-COLOR (color &body body)
  `(progn 'compile
     (let ((old-color tdv*current-color))
       (SET-CURRENT-COLOR ,color)
       ,@body
       (SET-CURRENT-COLOR old-color))))


(defmacro fill-4by4-array (out-array
                           a00 a10 a20 a30
                           a01 a11 a21 a31
                           a02 a12 a22 a32
                           a03 a13 a23 a33)

  `(progn
     (ASET ,a00 ,out-array 0 0)
     (ASET ,a10 ,out-array 1 0)
     (ASET ,a20 ,out-array 2 0)
     (ASET ,a30 ,out-array 3 0)

     (ASET ,a01 ,out-array 0 1)
     (ASET ,a11 ,out-array 1 1)
     (ASET ,a21 ,out-array 2 1)
     (ASET ,a31 ,out-array 3 1)

     (ASET ,a02 ,out-array 0 2)
     (ASET ,a12 ,out-array 1 2)
     (ASET ,a22 ,out-array 2 2)
     (ASET ,a32 ,out-array 3 2)

     (ASET ,a03 ,out-array 0 3)
     (ASET ,a13 ,out-array 1 3)
     (ASET ,a23 ,out-array 2 3)
     (ASET ,a33 ,out-array 3 3)
     out-array))

;****************************************************************************************

;****************************************************************************************
;                                                                  Color window hacks
;****************************************************************************************

(defun USE-COLOR-SCREEN-IF-POSSIBLE ()
  (IF (color:color-exists-p)
      (USE-COLOR-SCREEN)))

(defun SWAP-SCREENS ()
  (COND ((AND tdv*color-mode (color:color-exists-p))
         (setq tdv*color-mode t)
         (format t "~% Drawing on color window")
         (USE-COLOR-SCREEN-IF-POSSIBLE))
        (T
         (setq tdv*color-mode nil)
         (format t "~% Drawing on lisp machine window"))))

(Defun SET-NEW-COLOR (number color)
  (COND ((color:color-exists-p)
         (SET-COLOR number color))))


;----------------------------------------------------------------------------------------

;****************************************************************************************
;                                                           Transform stack routines
;****************************************************************************************

(defmacro T-COPY-ARRAY (from-array to-array)           ;copy transform stack contents upward
  `(copy-array-contents ,from-array ,to-array))

;----------------------

(defun T-POP ()
;  '(progn 'compile                             ; for defmacro
          (setq tdv*stack-length (1- tdv*stack-length))
          (COND ((MINUSP tdv*stack-length)
                 (format t
                         "~% You dummy, you popped the transform array stack once too often")
                 (setq tdv*stack-length 0))
                (T
                 (setq tdv*stack-top (AREF tdv*stack-array tdv*stack-length)))))
;    )

;----------------------

(defun T-PUSH (&optional (old-stack-top tdv*stack-top))
  (setq tdv*stack-length (1+ tdv*stack-length))
  (COND (( tdv*stack-length tdv*stack-max-length)
         (ferror t "~% You dummy, you just exceeded the maximum transform stack length"))
        (T
         (setq tdv*stack-top (AREF tdv*stack-array tdv*stack-length))
         (t-copy-array old-stack-top tdv*stack-top)     ;copy stack content upward
         (ASET '(I) tdv*stack-document tdv*stack-length)))      ;clear documentation list
  tdv*stack-top)

;----------------------

(defun T-FUN (t-function &rest fun-vars &aux new-array)
  (IF (null tdv*tfun-array) (setq tdv*tfun-array (Make-Array '(4 4) ':type tdv*array-type)))
  (setq new-array (apply t-function (copylist fun-vars)))
  (setq tdv*tfun-array (T-ARRAY-MULTIPLY new-array tdv*stack-top tdv*tfun-array))
  (T-COPY-ARRAY tdv*tfun-array tdv*stack-top)
  (ASET (APPEND (AREF tdv*stack-document tdv*stack-length)
                (ncons t-function))
        tdv*stack-document tdv*stack-length)
  tdv*stack-top)

;----------------------

(defun T-INIT ()
  (COND ((null tdv*stack-array)
         (setq tdv*stack-array    (make-array tdv*stack-max-length ':type 'art-q))
         (setq tdv*stack-document (make-array tdv*stack-max-length ':type 'art-q))
         (DO ((i 0 (1+ i)))
             ((= i tdv*stack-max-length))
           (ASET (MAKE-ARRAY '(4 4) ':type tdv*array-type) tdv*stack-array i)
           (ASET '(Empty) tdv*stack-document i))))

  (setq tdv*stack-length 0)
  (T-IDENT))                                    ;set stack top to idenity matrix

;----------------------

(defun T-IDENT ()
  (ASET '(I) tdv*stack-document tdv*stack-length)
  (setq tdv*stack-top (IDENT (AREF tdv*stack-array tdv*stack-length))))

;----------------------


;****************************************************************************************
;                                                                      Math routines
;****************************************************************************************

(defun T-DOT-PRODUCT (a-vect b-vect &aux (sum 0))
  (DO ((j 0 (1+ j)))
      ((= j 4))
    (setq sum (+ sum (* (AREF a-vect j) (AREF b-vect j)))))
  sum)

;----------------------

(defun T-VECT-PRODUCT (vect array &optional out-vect)

  (if (eq vect out-vect) (ferror t "~% Oops, In and out vectors equal"))

  (IF (NULL out-vect)
      (setq out-vect (MAKE-ARRAY 5 ':type tdv*array-type)))
  (IF tdv*fix-mode
      (DO ((j 0 (1+ j))
           (sum 0 0))
          ((= j 4))
        (DO ((i 0 (1+ i)))
            ((= i 4))
          (setq sum (+ sum (* (AREF vect i) (AREF array i j)))))
        (ASET (FIX sum) out-vect j))

    (DO ((j 0 (1+ j))
         (sum 0 0))
        ((= j 4))
      (DO ((i 0 (1+ i)))
          ((= i 4))
        (setq sum (+ sum (* (AREF vect i) (AREF array i j)))))
      (ASET (FIX sum) out-vect j)))

  (ASET (AREF vect 4) out-vect 4)               ;copy drawing info too

  out-vect)

;----------------------

(defun T-TRANS-VECT (vect
                     &optional
                     out-vect
                     (array tdv*stack-top)
                     (wb tdv*basic-w)
                     (ww 0.0)
                     max-sum
                     (float-vect tdv*temp-float-vect)
                     &aux line-type ws)

  "This preforms a array-vector multiplication for homogeneous vectors/arrays.
   A lot of auto scaling occurs during fixed point multiplications to keep a
   maximum number of significant bits."

  (IF (eq vect out-vect) (ferror t "~% Oops, In and out vectors equal"))

  (IF (NULL out-vect)
      (setq out-vect (MAKE-ARRAY 5 ':type tdv*array-type)))

  (DO ((i 0 (1+ i)))
      ((= i 4))
    (setq ww (+ ww (* (AREF vect i) (AREF array i 3)))))


  (COND (tdv*fix-mode                           ;Are we in fix point mode ?

         (COND ((zerop ww)                      ;oops, w is much too small.
                (DO ((j 0 (1+ j))
                     (v))                       ;set numbers as close to infinity as possible
                    ((= j 3))
                  (setq v (AREF vect j))
                  (ASET (If (zerop v) 0
                          (IF (plusp (* v ww)) wb (- wb)))
                        out-vect j))
                (ASET 1 out-vect 3))

               (T                               ;normal case

                (setq max-sum (ABS ww))
                (DO ((j 0 (1+ j))
                     (sum 0 0))
                    ((= j 3))
                  (DO ((i 0 (1+ i)))
                      ((= i 4))
                    (setq sum (+ sum (* (AREF vect i) (AREF array i j)))))
                  (setq max-sum (MAX max-sum (ABS sum)))        ;get max value in vector
                  (ASET sum float-vect j))      ;store sum in floating point vector

                (setq ws (// (FLOAT wb) max-sum))       ;scaling factor
                (DO ((j 0 (1+ j)))              ;convert back to integer format
                    ((= j 3))
                  (ASET (FIX (* ws (AREF float-vect j))) out-vect j))
                (ASET (FIX (* ws ww)) out-vect 3)))

         (setq line-type (AREF vect 4))
         (IF (AND tdv*color-mode (NOT (null tdv*current-color)) (NOT (zerop line-type)))
             (setq line-type (IF (minusp line-type) (- tdv*current-color) tdv*current-color)))
         (ASET (FIXR line-type) out-vect 4))            ;copy drawing info too


        (T                                      ;Nope - We're in floating point mode

         (COND ((< (ABS ww) 0.00001)            ;oops, w is much too small.
                (DO ((j 0 (1+ j))               ;set numbers as close to infinity as possible
                     (v))
                    ((= j 3))
                  (setq v (AREF vect j))
                  (ASET (If (< (ABS v) 0.000001) 0
                          (IF (plusp (* v ww)) 1 -1))
                        out-vect j))
                (ASET .000001 out-vect 3))

               (T                               ;normal case

                (DO ((j 0 (1+ j))
                     (sum 0 0))
                    ((= j 3))
                  (DO ((i 0 (1+ i)))
                      ((= i 4))
                    (setq sum (+ sum (* (AREF vect i) (AREF array i j)))))
                  (ASET sum out-vect j))
                (ASET ww out-vect 3)))

;                 (ASET (// sum ww) out-vect j))     ; this is for auto scaling too -
;               (ASET 1 out-vect 3)))))              ;  not yet though

         (setq line-type (AREF vect 4))
         (IF (AND tdv*color-mode (NOT (null tdv*current-color)) (NOT (zerop line-type)))
             (setq line-type (IF (minusp line-type) (- tdv*current-color) tdv*current-color)))
         (ASET line-type out-vect 4))           ;copy drawing info too

        )
 out-vect)


;----------------------

(defun T-ARRAY-SCALAR-MULTIPLY (array1 scalar &optional (outarray array1))

 "This routine multiplies all the elements in a 4 by 4 array by a scalar"

  (IF tdv*fix-mode
      (DO ((j 0 (1+ j)))
          ((= j 4))
        (DO ((i 0 (1+ i)))
            ((= i 4))
          (ASET (FIX (* (AREF array1 i j) scalar)) outarray i j)))
    (DO ((j 0 (1+ j)))
        ((= j 4))
      (DO ((i 0 (1+ i)))
          ((= i 4))
        (ASET (* (AREF array1 i j) scalar) outarray i j))))
  outarray)

;----------------------

(defun T-ARRAY-MULTIPLY (array1 array2 &optional out-array
                        &aux (W tdv*basic-w) ww (float-array tdv*temp-float-array))

  " This routine multiplies two arrays together, scaling the results if need be"

  (if (OR (eq array1 out-array) (eq array2 out-array) (eq array1 array2))
      (ferror t "~% Oops, in and out arrays equal"))

  (IF (NULL out-array)
      (setq out-array (MAKE-ARRAY '(4 4) ':type 'tdv*array-type)))

  (COND (tdv*fix-mode
         (IF (NULL float-array)
             (setq float-array
                   (setq tdv*temp-float-array (MAKE-ARRAY '(4 4) ':type 'art-float))))
         (setq float-array (MATH:MULTIPLY-MATRICES array1 array2 float-array))
         (setq ww (apply 'max (listarray float-array)))
         (IF (zerop ww) (setq ww 1))
         (setq out-array (T-ARRAY-SCALAR-MULTIPLY float-array (// w ww) out-array)))
        (T
         (setq out-array (MATH:MULTIPLY-MATRICES array1 array2 float-array))))
  out-array)



;****************************************************************************************
;                                                                 Display Segment Stuff
;****************************************************************************************

;. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;________________________________________________________________________________________
;        Segment name and number hashing routines.

(defun S-GET-HASH-NUMBER (seg-name &aux hash-number name-existance)
  " This routine reads the number of a segment with the inputed name out of a hash table.
    That number is then returned.  If the name doesn't exist in the table, nil is returned."
  (multiple-value (hash-number name-existance) (GETHASH seg-name s*hash-table))
  hash-number)

;----------------------

(defun S-ADD-HASH-NAME (seg-name seg-number &aux hash-number name-existance)
  " This routine adds a new segment name and number to the hash table
    It returns true if this works ok, and nil if another segment exists with that name"
  (multiple-value (hash-number name-existance) (GETHASH seg-name s*hash-table))
  (COND ((NULL hash-number)
         (PUTHASH seg-name seg-number s*hash-table)
         (ASET seg-name s*seg-names seg-number))        ;remember it's name (for debugging)
        (t
         (format t "~% CAUTION: Another segment exists with the name : ~A" seg-name)))
  (NOT name-existance))

;----------------------

(defun S-DELETE-HASH-NAME (seg-name &aux hash-number name-existance)
  (multiple-value (hash-number name-existance) (GETHASH seg-name s*hash-table))
  (COND ((NOT (NULL name-existance))
         (ASET "*" s*seg-names hash-number)
         (REMHASH seg-name s*hash-table ))))

;----------------------

(defun S-CLEAR-HASH-TABLE ()
  (CLRHASH s*hash-table))

;. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
;. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

(defun S-ROUTINE-INIT (&optional (max-number-segments 20) )
  "This routine must be used to initialize the initial number of segments used to N

   The segments that are initialized to hold drawing information are:

      s*seg-arrays   -  Array containing line info in full 5 vector, full scale detail.
                        Is appended to by the various line input routines, used to write
                        other segments, and to create draw lists.  The data may be
                        transformed by the current transform array in any of these three
                        steps. (First 4 vector locations are homogeneous coordinate, fifth
                        one is drawing info.)"

  (COND ((OR (Null s*seg-arrays) (Null s*seg-lengths) (Null s*open-seg-array)
             (Null s*max-segs) (< (Array-dimension s*seg-arrays 0) max-number-segments))
         (setq s*latest-seg 0
               s*seg-arrays (Make-Array max-number-segments ':type 'art-q)
               s*seg-lengths (Make-Array max-number-segments ':type 'art-half-fix)
               s*open-seg-array (Make-Array max-number-segments ':type 'art-half-fix)
               s*seg-names (Make-Array max-number-segments ':type 'art-q)
               s*hash-table (MAKE-HASH-TABLE ':size (* 2 max-number-segments)))))

  (DO ((N 0 (1+ N)))
      ((= N (Array-Length s*seg-arrays)))
    (ASET 0 S*open-seg-array N)
    (ASET 0 S*seg-lengths N)
    (ASET "." S*seg-names N))
  (S-CLEAR-HASH-TABLE)

  (t-init)
  (t-window-init .8 .8 0 0)
  (setq s*max-segs max-number-segments)
  (setq s*open-seg nil)
  (setq s*open-seg-stack nil)

  (S-INIT "default segment"))                   ;hack to avoid doing it in initailization sec.


;. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
(defun S-INIT (seg-name &optional (open-seg-too t) &AUX seg-number)

 " This routine is used to initialize a segment for input if desired.  It will create the
   segment if not already created.  It returns T is the segment aleady exits and NIL if
   it dosen't.  This may be simply used as follows to avoid rewriting the array:

     (defun blah ()                             ;routine used to make a segment 'blah   ;
       (setq segment-name 'BLAH)
       (COND ((S-INIT segment-name)
              ( draw draw and draw again ...)   ;Draw something. This can be S-Line commands,
              (       ....     )                ;   to calling other routines.
              (S-CLOSE)))                       ;Close that segment.
       (S-TRANSFORM segment-name))              ;Get information out of segment stack, and
                                                ;   write it into top open segment

   In the default mode, the segment is also opened for input.  This is usually what you
   want to do.  In the rare case where you just want to initailize a few segment names,
   set the open-seg-too flag to nil
                      ----------------------------------------------"


 (setq seg-number (S-GET-HASH-NUMBER seg-name)) ;see if segment exists with this name
 (COND ((null seg-number)
        (setq s*latest-seg (LOWEST-UNOPEN-SEG)) ;find out a place to keep segment
        (S-ADD-HASH-NAME seg-name s*latest-seg) ;remember it's name

        (COND (open-seg-too
               (setq s*open-seg s*latest-seg)
               (PUSH s*open-seg s*open-seg-stack)
               (ASET (Length s*open-seg-stack)
                     s*open-seg-array s*latest-seg))    ;positive number means segment is open
              (T
               (ASET -1 s*open-seg-array s*latest-seg)))))      ;-1 means segment is closed

 (NOT seg-number))                                      ;return nil if segment already exists

;. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

(defun LOWEST-UNOPEN-SEG (&aux (empty-seg -1) )                 ;kludge !!!!!
  (do ((s 0 (1+ s)))
      ((= 0 (AREF s*open-seg-array s)))
    (setq empty-seg s))
  (1+ empty-seg))

(defun HIGHEST-OPEN-SEG (&aux
                         (max-segs (MIN s*max-segs (Array-length s*open-seg-array)))
                         open-seg)
  (setq open-seg max-segs)
  (do ((s (1- max-segs) (1- s)))
      ((= 1 (AREF s*open-seg-array s)))
    (setq open-seg s))
  (1- open-seg))

;. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

(defun S-CLOSE  ( &aux temp)
  "Closes closes open segment, and then reopens segment closest to the top of stack"

; take segment off of stack
  (IF (NOT (> (Length s*open-seg-stack) 1))
      (ferror t "~% You closing too many segments. At least one must remain open for input"))
  (setq temp (POP s*open-seg-stack))

;indicate that segment is now closed
  (ASET -1 s*open-seg-array temp)   ;segment is still there, but only for reading

;find open segment                   I used highest-open-seg here before, Why ?
  (setq s*open-seg (POP s*open-seg-stack))
  (PUSH s*open-seg s*open-seg-stack))
;. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
(defun S-CLEAR  (seg-name &aux seg-number)
  "Clears everything in segment called seg-name without deleting it.
   Maybe this should automatically go in a call to s-open, though I sort of doubt it.
   I't can't go in S-Close, so where can it go ?"

  (setq seg-number (S-GET-HASH-NUMBER seg-name))
  (COND ((NULL seg-number)
         (ferror t "~% You can't clear segment ~A, because it does't exist" seg-name)))

; zero it's length and it's name if it exists
  (ASET 0 s*seg-lengths seg-number))

;. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .


(defun S-DELETE (seg-name &aux seg-number)
  "Deletes segment seg-name from segment list "

  (setq seg-number (S-GET-HASH-NUMBER seg-name))
  (COND ((NULL seg-number)
         (ferror t "~% You can't delete segment ~A, because it does't exist" seg-name)))
  (COND ((= 1 (AREF s*open-seg-array seg-number))
         (ferror t "~% You can't delete segment ~A without closeing it first" seg-name)))

  (ASET 0 s*open-seg-array seg-number ) ;free up segment area

; zero it's length and it's name if it exists
  (COND ((NOT (NULL seg-number))
         (ASET 0 s*seg-lengths seg-number)
         (S-DELETE-HASH-NAME seg-name))))

;. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

(defun S-OPEN  (seg-name &aux seg-number)
  "This opens a segment for input by dumping it onto the stack.
   NOTE: This routine does not check first to see that the segment is already open.  Although
         this allows for recursive calls, I am not to sure I want to to this.
     **  I should check, or fiqure out some way to figure out what segment to open after
         I close this segment once.  Besides, what does this stupid sort of recursion give
         me."

  (setq seg-number (S-GET-HASH-NUMBER seg-name))
  (COND ((NOT (NULL seg-number))
         (setq s*open-seg seg-number)           ;rember that this segments now open
         (push s*open-seg s*open-seg-stack)
         (ASET (length s*open-seg-stack)
               s*open-seg-array seg-number))  ;positive number means segment is open
        (t
         (ferror t "~% Tried to open a non-existant segment ~A.  Oops1" seg-name))))

;. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

(defun S-SWAP-NAMES (seg1-name seg2-name
                 &aux seg1-number seg2-number)

  "This routine swaps the names which two segments are accessed by.
   It can also be used  to rename a segment if a dummy segment with the new name
   is first inited, and then this routine is called.

   Caution.  This routine does not alter the open and other status of the segments it
   it renameing.  If seg1 is open for input before this routine is called, then seg2
   will be written in after this routine finishes.  It is a good practice to make sure
   that both segments are closed before this routine is called."

  (setq seg1-number (S-GET-HASH-NUMBER seg1-name))
  (setq seg2-number (S-GET-HASH-NUMBER seg2-name))

  (COND ((NOT (OR (NULL seg1-number) (NULL seg2-number)))
         (S-DELETE-HASH-NAME seg1-name)         ;swap numbers associated with names
         (S-DELETE-HASH-NAME seg2-name)         ;by removing names from hash table
         (S-ADD-HASH-NAME seg1-name seg2-number)        ;then putting them back in with
         (S-ADD-HASH-NAME seg2-name seg1-number))       ;each others numbers
        (T
         (ferror t "~% Can't swap names of non-existant segments.
                    ~% One or both of these two segments doesn't exist
                    ~%     ~A , ~A  .  " seg1-name seg2-name))))

;. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

(defun S-TRANSFORM (seg-name &aux seg-number vect seg-length segment)
  "This routine runs the segment named seg-name through the transformtion array,
   and then writes it into the current open segment.  Note that this routine is
   not used to create draw lists, (i.e. the final transformation into a 2 vect that
   the screen can handle), but is used to create instances and copies of other segments."

  (IF (NULL s*open-seg)
         (ferror t "~% Trying to write ~A into a non-existant segment " seg-name))

  (SETQ seg-number (S-GET-HASH-NUMBER seg-name))

  (COND ((NOT (NULL seg-number))

         (SETQ segment (AREF s*seg-arrays seg-number)
               seg-length (AREF s*seg-lengths seg-number))

         (DO ((i 0 (1+ i)))
             ((= i seg-length))
           (setq vect (AREF segment i))
           (T-TRANS-VECT vect (S-END-OF-OPEN-SEGMENT))))
        (t
         (ferror t "~% Tried to write from a non-existant segment ~A. " seg-name))))

;. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

(defun S-END-OF-OPEN-SEGMENT (&optional (seg-number s*open-seg)
                              &aux
                              seg-length max-length open-segment
                              (initial-segment-length 40))
  "This routine returns the pointer to the last point vector in the open segment "

  (setq open-segment (AREF s*seg-arrays seg-number))

    ; does the segment exist yet ?

  (COND ((null open-segment)
         (setq open-segment (MAKE-ARRAY initial-segment-length ':type 'art-q))
         (DO ((i 0 (1+ i)))
             ((= i (Array-length open-segment)))
           (ASET (MAKE-ARRAY 5 ':type tdv*array-type) open-segment i))
         (ASET 0 s*seg-lengths seg-number)
         (ASET open-segment s*seg-arrays seg-number)))


    ; is it long enough ?  -  If not, make it longer -

  (setq seg-length (AREF s*seg-lengths seg-number))
  (setq max-length (Array-length open-segment))

  (COND (( seg-length (- max-length 4))
         (Adjust-Array-Size open-segment (FIX (* seg-length 1.7)))
         (DO ((i max-length (1+ i)))
             ((= i (Array-length open-segment)))
           (ASET (MAKE-ARRAY 5 ':type tdv*array-type) open-segment i))
         (ASET open-segment s*seg-arrays seg-number)))

    ; Now, get the last element in the list

  (ASET (1+ seg-length) s*seg-lengths seg-number)       ;increment number
  (AREF open-segment seg-length))                       ;get point and return it.



;****************************************************************************************
;                                                             Segment Drawing routines
;****************************************************************************************
(defvar tdv*window-array1       nil "Displayed screen array")
(defvar tdv*window-array2       nil "Working screen array")
(defvar tdv*window-xdim         0   "X dimension of screen array")
(defvar tdv*window-ydim         0   "Y dimension of screen array")
(defvar tdv*window-lispm-array2 nil "Pointer to keep track of working screen array")
(defvar tdv*window-color-array2 nil "Pointer to keep track of working screen array")
(defvar tdv*clearing-array nil "Small array filled with zeros for clearing big arrays")

;----------------------------------------------------------------------------------------
(defun S-CLEAR-SCREEN (&optional (array tdv*window-array2))
  "This routine clears the array passed to it. In the default mode it clears the array
   in which all drawing is done in."

  (IF (OR (NULL tdv*clearing-array)
          (NOT (eq (Array-type array) (Array-type tdv*clearing-array))))
      (setq tdv*clearing-array
            (MAKE-PIXEL-ARRAY 32. 1 ':type (array-type array))))
  (BITBLT tv:alu-seta tdv*window-xdim tdv*window-ydim tdv*clearing-array 0 0 array 0 0))

;. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
(defun S-DISPLAY-SCREEN (&optional
                         (from-array tdv*window-array2)
                         (to-array tdv*window-array1))
  (BITBLT tv:alu-seta tdv*window-xdim (- tdv*window-ydim 32) from-array 0 0 to-array 0 0))

;----------------------------------------------------------------------------------------
(defun make-unique-symbol (&rest sss)
  (INTERN (apply 'string-append (mapcar #'string sss))))

;. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
(defun S-DRAW (seg-name
               &optional
               (draw-alu tv:alu-ior)
               (color tdv*current-color)
               (to-array tdv*window-array2)
               &aux N d1-seg-name)

  "This routine draws seg-name onto one of the output frames.
   By default this is the one that is usually hidden.
   This is done using the current transformation matrix. "

  (SETQ N (S-GET-HASH-NUMBER seg-name))

  (COND ((NOT (NULL N))

;         (t-sprint seg-name)

         (Setq d1-seg-name (make-unique-symbol 'd1- seg-name))

         (S-INIT d1-seg-name nil)
         (S-CLEAR d1-seg-name)                  ;get rid of it

         (S-OPEN d1-seg-name)
          (S-CLEAR d1-seg-name)
          (S-CLIP-SEGMENT seg-name)  ;generate new draw list

         (S-CLOSE)

;         (t-sprint d1-seg-name)

         (S-DISPLAY d1-seg-name draw-alu color to-array))       ;draw new version of segment

        (t
         (format t "~% Warning: The segment ~A doesn't exist in routine S-DRAW" seg-name))))

;. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .


(defun S-DISPLAY (seg-name
                    &optional
                    (alu-type tv:alu-ior)
                    (color tdv*current-color)
                    (to-array tdv*window-array1)
                    &aux
                    seg-number seg-array seg-array-length
                    line-info xs ys xe ye type)


  " This routine dumps the draw list passed to it into one of the display arrays.
     By default this is the one that is usually displayed, but
     when called by s-draw, it is usually the array that is hidden.

    This list is assumed to be in the following format:
      display-list     <--  ((line-info) (line-info) ... (line-info))

        line-info      <--  (x y z ? line-code)
         line-code     <-- < 0 : draw a dot
                             0 : begining of a line segment
                           > 0 : connect this point and the previous point with a line

               When drawing to color screens, the absolute value of the line code
               is used as the color of the line segment if tdv*current-color is nil

    Note that x an y are assumed to be in a generalized display coordinate system.
    (That is they range between -1 and 1 in floating point mode, and -32k to +32k
     when in fixed point mode).

    This routine will work for connected line lists, that is groups of lines where the
    end of one line is the begining of the next.  The begining of the line should have
    a line code of 0. The rest of the endpoints in the line should have line codes
    of one or larger.

               ............................................"

  (SETQ seg-number (S-GET-HASH-NUMBER seg-name))
  (IF (null seg-number)
      (ferror t "~% Tried to draw a non-existant-segment : ~A. " seg-name))

  (setq seg-array        (AREF s*seg-arrays seg-number)
        seg-array-length (AREF s*seg-lengths seg-number))

 ; draw the stupid thing

  (DO ((l 0 (1+ l)))
      (( l seg-array-length))

    (setq line-info (AREF seg-array l))
    (setq type (FIX (AREF line-info 4)))


    (IF (AND tdv*color-mode (NULL tdv*current-color))
        (setq color (REMAINDER (ABS type) *number-of-colors*)))

    (COND
          ((plusp type)
           (setq xe (T-ADJUST-TO-WINDOW 0 (AREF line-info 0))   ;end of line
                 ye (T-ADJUST-TO-WINDOW 1 (AREF line-info 1)))
;          (format t "~% Drawing line using ( ~5D ~5D ) -> (~5D ~5D) " xs ys xe ye)
           (IF tdv*color-mode
                  (color-draw-line xs ys xe ye color alu-type t to-array)
                  (sys:%draw-line xs ys xe ye alu-type t to-array))
           (setq xs xe   ys ye))                ;save end point to be new begining point

          ((zerop type)
           (setq xs (T-ADJUST-TO-WINDOW 0 (AREF line-info 0))   ;begining of line
                 ys (T-ADJUST-TO-WINDOW 1 (AREF line-info 1))))

          (T                                    ; must be negative type then
           (setq xs (T-ADJUST-TO-WINDOW 0 (AREF line-info 0))   ;point
                 ys (T-ADJUST-TO-WINDOW 1 (AREF line-info 1)))
           (IF tdv*color-mode
               (color-draw-point xs ys color alu-type to-array)
               (ASET (Boole alu-type -1 (AREF to-array xs ys))
                     to-array xs ys)))
          )))


;----------------------------------------------------------------------------------------

;****************************************************************************************
;                                                             Positioning Routines
;****************************************************************************************
;----------------------------------------------------------------------------------------

(defun T-WINDOW-INIT (&optional
                      (xwscale 1.0)
                      (ywscale 1.0)
                      (xcshift 0.0)
                      (ycshift 0.0)
                      &aux xdim ydim)
  "This routine sets the display window to the full size LISPM window"

  (setq tdv*window (IF tdv*color-mode (USE-COLOR-SCREEN-IF-POSSIBLE) terminal-io))

  (setq tdv*window-array1 (Send tdv*window ':screen-array))
  (setq xdim (pixel-array-width tdv*window-array1)
        ydim (pixel-array-height tdv*window-array1))

  (IF tdv*color-mode
      (setq tdv*window-array2 tdv*window-color-array2)
      (setq tdv*window-array2 tdv*window-lispm-array2))

  (IF (OR (Null tdv*window-array2)
          (NOT (= (pixel-array-width tdv*window-array2) xdim))
          (< (pixel-array-height tdv*window-array2) ydim))
      (setq tdv*window-array2
            (tv:Make-sheet-bit-array tdv*window xdim ydim)))
;           (MAKE-ARRAY (list xdim ydim) ':type (ARRAY-TYPE tdv*window-array1))))

  (IF tdv*color-mode
      (setq tdv*window-color-array2 tdv*window-array2)
      (setq tdv*window-lispm-array2 tdv*window-array2))

  (setq tdv*current-color    nil
        tdv*window-xdim      xdim
        tdv*window-ydim      ydim)

  (T-SIZE-WINDOW xwscale ywscale xcshift ycshift))

;----------------------------------------------------------------------------------------

(defun T-SIZE-WINDOW (&optional
                      (xwscale 1.0)
                      (ywscale 1.0)
                      (xcshift 0.0)
                      (ycshift 0.0)
                      (xdim tdv*window-xdim)
                      (ydim tdv*window-ydim)
                      &aux dim)
  "This routine sets the extent of the drawing area in the window system window
   The input parameters are as follows:
       xwscale, ywscale - extent of the drawing area inside the largest possible square
                          drawing area.  One means full size, 0 means no size.
       xcshift,ycshift  - Shift of the center of the drawing from the center of the
                          screen.  0 means centerd, 1 means shift the center all the
                          way to one of the sides of the viewscreen.
       xdim,ydim        - Size of viewing area in pixels. "

  (setq dim (MIN xdim ydim))
  (setq tdv*window-half-xdim (FIXR (* xwscale (// dim 2.0 )))
        tdv*window-half-ydim (FIXR (* ywscale (// dim 2.0)))
        tdv*window-xcenter   (FIXR (* (+ 1.0 xcshift) (// xdim 2.0 )))

        tdv*window-ycenter   (FIXR (* (+ 1.0 ycshift) (// ydim 2.0 )))))

;----------------------------------------------------------------------------------------

(defun T-ADJUST-TO-WINDOW (code value &optional (gw-size tdv*gen-window-size))
  "This routine converts an x or y coordinate from +- wb scale to full size window scale.
   Hopefully, this is one of the only places where floating point calculations will have
   to be done if we are in fixed point mode. "  ; pull out fix's eventually ?
  (IF (zerop code)
      (+ tdv*window-xcenter
         (// (* tdv*window-half-xdim (FIX value)) gw-size))     ;x coor
      (+ tdv*window-ycenter
         (// (* tdv*window-half-ydim (FIX value)) gw-size)))) ;y coor

;. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

(defunp T-GET-DISPLAY-COORDS (vect
                               &optional
                               (draw-code 0)    ;this is line end type and color info
                               (out-vect nil)
                               (toll 0.001)
                               (gw-size tdv*gen-window-size)
                               &aux ww scale-factor)

  " This routine transforms vect from homogenious coodinates to normalized display
  coordinates (+ or - 32K), and returns an array containing the x y and z values,
  and the draw code.
    It assumes, (God! I hope I can make this assumption), that all the points have been
  clipped to a plus or minus w sized viewport.  If this is not true, Help ! "

  (IF (null out-vect) (setq out-vect (MAKE-ARRAY 5 ':type tdv*array-type)))

  (setq ww (AREF vect 3))
  (IF (< (ABS ww) toll) (setq ww toll))

  (setq scale-factor (// gw-size (FLOAT ww))); is there a way to avoid the float?
  (ASET (FIX (* (AREF vect 0) scale-factor)) out-vect 0)
  (ASET (FIX (* (AREF vect 1) scale-factor)) out-vect 1)
  (ASET (FIX (* (AREF vect 2) scale-factor)) out-vect 2) ;comment out if no depth info needed

  (ASET gw-size out-vect 3)             ;this line can probally be commented out
  (ASET draw-code out-vect 4)
  out-vect)

;----------------------------------------------------------------------------------------


;****************************************************************************************
;                                                             Line Clipping routines
;****************************************************************************************

(defvar s*clip-array1 nil " Temp. storage location for one clipping vector")
(defvar s*clip-array2 nil " Temp. storage location for one clipping vector")
(defvar s*clip-vect1  nil " Temp. storage for one clipped vector")
(defvar s*clip-vect2  nil " Temp. storage for one clipped vector")

(defun S-CLIP-SEGMENT (seg-name
                         &optional
                         (s-clip-array s*clip-array1)
                         (e-clip-array s*clip-array2)
                         (vect-cs s*clip-vect1)
                         (vect-ce s*clip-vect2)
                         &aux
                         seg-number inarray inarray-length
                         vect-s type-s
                         vect-e type-e
                         s-clip-code e-clip-code
                         clipped-line-exists)

  " This routine reads in a segment, and converts it to a form suitable for drawing.

  It does this by reading in the transform list point by point, transforming it one
  last time, clipping out the points or lines if needed, and scaleing them up to
  the display window coordinates, and finally appending them to the draw list.

    This input segment is assumed to be in the following format:
      inarray             <--  ((vect-info) (vect-info) ... (vect-info))
         vect-info        <--  (x y z w line-type)
            line-type     <-- < 0 : draw a dot
                                0 : begining of a line segment
                              > 0 : connect this point and the previous point with a line
                  When working with a color windows, the Absolute value of the line type
                  determines the color of the line (from 1 to MAX-COLORs)

    The output segment is in the same form.  Non visible lines and points are clipped out,
    and the data is normalized to + or - tdv*basic-w

    Note the funny calls to T-GET-DISPLAY-COORDS, they don't seem to return anything.
    In reality, the internal call to S-END-OF-OPEN-SEGMENT returns a pointer to the
    5-vect at the end of the open segment.  T-GET-ect. stuffs the output data into this
    vector, and hence there is no need to return anything to this routine.  I realize that
    this is not a good programing proceedure, but it allows me to explicitly control where
    the data is going to end up so I don't have to turn the garbage collector on.

                      ............................................ "

; get segment arrays and lengths

  (IF (NULL s*open-seg)
      (ferror t "~% No open drawing segment to write ~A into " seg-name))

  (setq seg-number (S-GET-HASH-NUMBER seg-name))

  (IF (NULL seg-number)
      (ferror t "~% Tried to draw a non-existant segment ~A " seg-name))

  (setq inarray        (AREF s*seg-arrays seg-number)
        inarray-length (AREF s*seg-lengths seg-number))

; get first point

  (setq vect-s (T-TRANS-VECT (AREF inarray 0) vect-s))  ;get first point, transform it
  (setq type-s (AREF vect-s 4))

  (multiple-value (s-clip-array s-clip-code)    ;get it's clipping parameters
    (T-MAKE-CLIP-ARRAY vect-s s-clip-array))

  (multiple-value (e-clip-array e-clip-code)    ;do it twice in order to generate another
    (T-MAKE-CLIP-ARRAY vect-s e-clip-array))    ;clipping array

; If it is a point and not clipped, draw it

  (IF (AND (minusp type-s) (zerop s-clip-code)) ;negative # means draw point
      (T-GET-DISPLAY-COORDS vect-s type-s (S-END-OF-OPEN-SEGMENT)))

;          ---------------------- MAIN PROGRAM LOOP ----------------------
  (DO ((iv 1 (1+ iv)))
      (( iv inarray-length))

    (setq vect-e (T-TRANS-VECT (AREF inarray iv) vect-e))       ;transform next point
    (setq type-e (AREF vect-e 4))

    (multiple-value (e-clip-array e-clip-code)  ;get it's clipping parameters
      (T-MAKE-CLIP-ARRAY vect-e e-clip-array))

  ;depending on if it's a point or a line, draw or process it

    (COND ((MINUSP type-e)                      ;it's a point
           (COND ((zerop e-clip-code)           ;and it can be displayed
                  (T-GET-DISPLAY-COORDS vect-e type-e (S-END-OF-OPEN-SEGMENT)))))

       ; not a point, so it must be a line

          ((PLUSP type-e)                       ;and it's the end of a line too

            ; Is the whole line inside the viewbox ?

           (COND ((AND (zerop e-clip-code)
                       (zerop s-clip-code))     ; whole thing is inside of viewbox

                  (T-GET-DISPLAY-COORDS vect-s 0 (S-END-OF-OPEN-SEGMENT))
                  (T-GET-DISPLAY-COORDS vect-e type-e (S-END-OF-OPEN-SEGMENT)))

            ; Is any part of the line inside of viewbox ?

                 ((ZEROP (LOGAND e-clip-code
                                 s-clip-code))  ; something may be inside of viewbox

                  (multiple-value (vect-cs vect-ce clipped-line-exists)
                    (T-CLIP-LINE vect-s vect-e
                                 s-clip-array e-clip-array
                                 vect-cs vect-ce))      ;clip-line

                  (COND (clipped-line-exists    ;something left after clip
                         (T-GET-DISPLAY-COORDS vect-cs 0 (S-END-OF-OPEN-SEGMENT))
                         (T-GET-DISPLAY-COORDS vect-ce type-e (S-END-OF-OPEN-SEGMENT))))))))


  ;swap the pointers from the new end point to the start point, and continue do loop
    (psetq vect-s vect-e
           vect-e vect-s
           s-clip-array e-clip-array
           e-clip-array s-clip-array
           s-clip-code e-clip-code
           e-clip-code s-clip-code))

                      ;............................................

  (IF (Null s*clip-array1)                      ;remember pointers into these arrays
      (setq s*clip-array1 s-clip-array          ;so don't have to create them again
            s*clip-array2 e-clip-array
            s*clip-vect1  vect-cs
            s*clip-vect2  vect-ce))

  s*open-seg)

;----------------------------------------------------------------------------------------
; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

(defunp T-CLIP-LINE (vect-s vect-e clip-array-s clip-array-e
                            &optional
                            (out-vect-s nil)
                            (out-vect-e nil)
                            (toll 0.0001)
                            &aux
                            (ts 0) (te 1) tt dem
                            xs ys zs ws
                            xe ye ze we
                            cs ce
                            dx dy dz dw
                            (things-went-ok t))

 " This routine clips lines to the display viewbox.

 It requires a pre-processing step to create the clip lists, and to determine that the
 line need clipping at all. (Both Clip-codes are not zero, and the bit wize logical or
 of the two codes are zero).  The preprocessing step may be done in the  T-MAKE-CLIP-LIST,
 followed by codes to treat the above two cases.  This routine was adapted from Newman
 and Sproull, 2'nd edition, page 360."

; make sure we haven't screwed up and made clip-arrays eq.
  (IF (eq clip-array-s clip-array-e)
      (ferror t "~% Oops, Clip arrays are EQ.
                    Better set s*clip-array1,2 to nil and start again"))

; create output vectors if have to
 (IF (OR (null out-vect-s) (null out-vect-e))
     (setq out-vect-s (MAKE-ARRAY 5 ':type tdv*array-type)
           out-vect-e (MAKE-ARRAY 5 ':type tdv*array-type)))

; get components of vectors
  (setq xs (AREF vect-s 0)
        ys (AREF vect-s 1)
        zs (AREF vect-s 2)
        ws (AREF vect-s 3)

        xe (AREF vect-e 0)
        ye (AREF vect-e 1)
        ze (AREF vect-e 2)
        we (AREF vect-e 3))

;calculate t's for each end of the clipped line.
  (DO ((ic 0 (1+ ic)))
      ((= ic 6))

    (setq cs (AREF clip-array-s ic)
          ce (AREF clip-array-e ic))

    (COND ((OR (minusp cs) (minusp ce))         ;if code is negative, line is clipped
           (setq dem (FLOAT (- cs ce)))
           (IF (< (ABS dem) toll) (setq dem toll))
           (setq tt (// cs dem))
           ;(format t "~% i cs ce tt : ~d ~3F ~3F   ~3f" ic cs ce tt)
           (IF (minusp cs) (setq ts (MAX tt ts));begining of line was clipped
             (setq te (MIN tt te))))))          ;otherwise end was clipped


; knowing t's, clip line
  (COND (( te ts)                              ;the line is there
         ;(format t "~% ts te : ~3F ~3F " te ts)
         (setq dx (- xe xs)
               dy (- ye ys)
               dz (- ze zs)
               dw (- we ws))

         (COND ((NOT (= ts 0))                  ;begining of line was clipped
                (COND (tdv*fix-mode
                       (ASET (+ xs (FIX (* dx ts))) out-vect-s 0)
                       (ASET (+ ys (FIX (* dy ts))) out-vect-s 1)
                       (ASET (+ zs (FIX (* dz ts))) out-vect-s 2)
                       (ASET (+ ws (FIX (* dw ts))) out-vect-s 3))
                      (T
                       (ASET (+ xs (* dx ts)) out-vect-s 0)
                       (ASET (+ ys (* dy ts)) out-vect-s 1)
                       (ASET (+ zs (* dz ts)) out-vect-s 2)
                       (ASET (+ ws (* dw ts)) out-vect-s 3))))

               (T                               ;begining was not clipped
                (ASET xs out-vect-s 0)
                (ASET ys out-vect-s 1)
                (ASET zs out-vect-s 2)
                (ASET ws out-vect-s 3)))

         (COND ((NOT (= te 1))                  ;end of line was clipped
                (COND (tdv*fix-mode
                       (ASET (+ xs (FIX (* dx te))) out-vect-e 0)
                       (ASET (+ ys (FIX (* dy te))) out-vect-e 1)
                       (ASET (+ zs (FIX (* dz te))) out-vect-e 2)
                       (ASET (+ ws (FIX (* dw te))) out-vect-e 3))
                      (T
                       (ASET (+ xs (* dx te)) out-vect-e 0)
                       (ASET (+ ys (* dy te)) out-vect-e 1)
                       (ASET (+ zs (* dz te)) out-vect-e 2)
                       (ASET (+ ws (* dw te)) out-vect-e 3))))

               (T                               ;end was not clipped
                (ASET xe  out-vect-e 0)
                (ASET ye  out-vect-e 1)
                (ASET ze  out-vect-e 2)
                (ASET we  out-vect-e 3))))

        (t                                      ;line isn't really there
         (setq things-went-ok nil)))                    ;flag for t-write-list

; copy line drawing codes
  (ASET (AREF vect-s 4) out-vect-s 4)
  (ASET (AREF vect-e 4) out-vect-e 4)

  (RETURN out-vect-s out-vect-e things-went-ok))

;. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

(defunp T-MAKE-CLIP-ARRAY (vect
                            &optional
                           (clip-vect nil)
                           &aux
                           x y z w
                           (clip-code 0))


  "  This routine determines wether or not a point (vect) is within the display viewbox.

 In addition, it also generates preliminary data for the actual clipping routine,
 which I've called T-CLIP-LINE.  This algorithm was adapted from Newman & Sproull,
 2'nd edition, page 360.

 The position of the point is coded in the number clip-code. It is entirerly within
 the veiwbox iff clip-code is zero.

 Note:  This rotine assumes that w is greater than zero.  I'm not sure how well it will
        work if w < 0.

 I am not sure the array type for clip-vect is correct.  I'd like to keep it of
 type tdv*array-type, but think I will end up with overflows when w and x are both large.
 This could be avoided by scaling the vector so this does not happen, yet think that that
 would be too much work (4 divisions vs 6 floating point calc's)  Oh well !

                      ............................................"


  (IF (null clip-vect) (setq clip-vect (MAKE-ARRAY 6 ':type tdv*array-type)))

  (setq x (AREF vect 0)
        y (AREF vect 1)
        z (AREF vect 2)
        w (AREF vect 3))

  (AND (minusp (ASET (+ w x) clip-vect 0)) (setq clip-code 1))
  (AND (minusp (ASET (- w x) clip-vect 1)) (setq clip-code (logior clip-code 2.)))
  (AND (minusp (ASET (+ w y) clip-vect 2)) (setq clip-code (logior clip-code 4.)))
  (AND (minusp (ASET (- w y) clip-vect 3)) (setq clip-code (logior clip-code 8.)))
  (AND (minusp (ASET    z    clip-vect 4)) (setq clip-code (logior clip-code 16.)))
  (AND (minusp (ASET (- w z) clip-vect 5)) (setq clip-code (logior clip-code 32.)))

  (Return Clip-Vect clip-code))


;****************************************************************************************
;                                                                   Line Stuff
;****************************************************************************************

(defvar l*basez      0      "default value for Z coordinate in s-line routines")
(defvar l*basew tdv*basic-w "default value for W in l-line routines")
(defvar l*basec      1      "default color for line or point in s-line routines")

;. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

(defun SET-BASEW ( W )
  (setq l*basew W))

;. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

(defun SET-BASEZ ( Z )
  (setq l*basez Z))

;  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

(defun SET-CURRENT-COLOR ( c )
  (setq tdv*current-color
        (setq l*basec (AND c (IF tdv*color-mode (REMAINDER c *number-of-colors*) 1)))))

;  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

(defun GET-POINT (invect-list line-code &optional (out-vect nil)
                  &aux x y z w ws (toll .001))
  " This routine gets a point out of a vector, altering it to make it a 4-vect if needed.
    Hopefully, this is only called in the input routines.`
    Note that in fix point mode, all the values are scaled in order to make good homogeneous
    coordinates."

  (IF (null out-vect) (setq out-vect (MAKE-ARRAY 5 ':type tdv*array-type)))

  (COND (tdv*fix-mode
         (setq x (first invect-list)
               y (second invect-list)
               z (OR (Third invect-list)  l*basez)
               w (OR (Fourth invect-list) l*basew))
         (setq ws (// tdv*basic-w (MAX toll (abs x) (abs y) (abs z) (abs w))))
         (Aset (FIXR (* x ws)) out-vect 0)
         (Aset (FIXR (* y ws)) out-vect 1)
         (Aset (FIXR (* z ws)) out-vect 2)
         (Aset (FIXR (* w ws)) out-vect 3))

        (T
         (Aset (first invect-list)   out-vect 0)
         (Aset (second invect-list)  out-vect 1)
         (Aset  (OR (Third invect-list)  l*basez ) out-vect 2)
         (Aset (OR (Fourth invect-list) l*basew ) out-vect 3)))

  (Aset (IF (zerop line-code) line-code (OR (FIFTH invect-list) line-code)) out-vect 4)
  out-vect)

;  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

(defun S-LINE (&rest line-list
               &aux  first-point)

  "This routine dumps a connected series of lines into the open segment list.
   It should be called like (s-line (list '(1 2 3) '(1 2 2) '(1 3 4) .... ))."

  (IF (Null s*open-seg) (S-OPEN 'default-seg-from-S-LINE))

; transform first point, and write out with a 0 code to indicate begining of line
  (setq first-point (FIRST (FIRST line-list)))
  (T-TRANS-VECT (GET-POINT first-point 0) (S-END-OF-OPEN-SEGMENT))

;transform rest of lines, using a 1 code to indicate they are connected to previous point
  (dolist (a-point (REST1 (FIRST line-list)))
    (T-TRANS-VECT (GET-POINT a-point (OR l*basec 1)) (S-END-OF-OPEN-SEGMENT)))
  (Aref s*seg-arrays s*open-seg))

;  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

(defun S-POINT (&rest point-list)
  "This routine dumps a  of points into the open segment list.
   It should be called like (s-point (list '(1 2) '(1 2 2 1 6) '(1 3 4) .... ))."
  (IF (Null s*open-seg) (S-OPEN 'default-seg-from-S-POINT))

;transform points, using a minus code to indicate they are points

  (dolist (a-point (first point-list))
    (T-TRANS-VECT (GET-POINT a-point (- (OR l*basec 1))) (S-END-OF-OPEN-SEGMENT)))
  (Aref s*seg-arrays s*open-seg))


;****************************************************************************************
;                                                Transform Array Generation Routines
;****************************************************************************************

(defun IDENT (&optional (out-array tdv*temp-array) &aux (w tdv*basic-w))

  (IF (NULL out-array)
      (setq out-array (setq tdv*temp-array (MAKE-ARRAY '(4 4) ':type tdv*array-type))))


  (fill-4by4-array out-array
                   w   0   0   0
                   0   w   0   0
                   0   0   w   0
                   0   0   0   w))

;----------------------

(defun ROTX (theta
             &optional (out-array tdv*temp-array)
             &aux c s (w tdv*basic-w))

  (IF (NULL out-array)
      (setq out-array (setq tdv*temp-array (MAKE-ARRAY '(4 4) ':type tdv*array-type))))

  (setq s (SIN theta)
        c (COS theta))

  (IF (NOT (NULL tdv*fix-mode))
      (setq s (FIX (* s w))
            c (FIX (* c w))))

  (IF (NULL out-array)
      (setq out-array (setq tdv*temp-array (MAKE-ARRAY '(4 4) ':type tdv*array-type))))

  (fill-4by4-array out-array
                   w   0   0   0
                   0   c (- s) 0
                   0   s   c   0
                   0   0   0   w))

;----------------------
(defun ROTY (theta
             &optional (out-array tdv*temp-array)
             &aux c s (w tdv*basic-w))

  (IF (NULL out-array)
      (setq out-array (setq tdv*temp-array (MAKE-ARRAY '(4 4) ':type tdv*array-type))))

  (setq s (SIN theta)
        c (COS theta))

  (IF (NOT (NULL tdv*fix-mode))
      (setq s (FIX (* s w))
            c (FIX (* c w))))

  (IF (NULL out-array)
      (setq out-array (setq tdv*temp-array (MAKE-ARRAY '(4 4) ':type tdv*array-type))))

  (fill-4by4-array out-array
                   c   0   s   0
                   0   w   0   0
                 (- s) 0   c   0
                   0   0   0   w))

;----------------------

(defun ROTZ (theta
             &optional (out-array tdv*temp-array)
             &aux c s (w tdv*basic-w))

  (IF (NULL out-array)
      (setq out-array (setq tdv*temp-array (MAKE-ARRAY '(4 4) ':type tdv*array-type))))

  (setq s (SIN theta)
        c (COS theta))

  (IF (NOT (NULL tdv*fix-mode))
      (setq s (FIX (* s w))
            c (FIX (* c w))))

  (IF (NULL out-array)
      (setq out-array (setq tdv*temp-array (MAKE-ARRAY '(4 4) ':type tdv*array-type))))

  (fill-4by4-array out-array
                   c (- s) 0   0
                   s   c   0   0
                   0   0   w   0
                   0   0   0   w))

;----------------------

(defun SCALE (sx sy sz
             &optional (out-array tdv*temp-array)
             &aux (w tdv*basic-w))

  (IF (NULL out-array)
      (setq out-array (setq tdv*temp-array (MAKE-ARRAY '(4 4) ':type tdv*array-type))))

  (IF tdv*fix-mode
      (setq w (// w (FLOAT (MAX 1 w (ABS sx) (ABS sy) (ABS sz))))
            sx (FIX (* sx w))
            sy (FIX (* sy w))
            sz (FIX (* sz w))
            w  (FIX w)))

  (fill-4by4-array out-array
                   sx   0    0   0
                   0    sy   0   0
                   0    0    sz  0
                   0    0    0   w))


;----------------------
(defun TRANS (tx ty tz
             &optional (out-array tdv*temp-array)  &aux (w tdv*basic-w))

  (IF (NULL out-array)
      (setq out-array (setq tdv*temp-array (MAKE-ARRAY '(4 4) ':type tdv*array-type))))

  (IF tdv*fix-mode
      (setq w (// w (FLOAT (MAX 1 w (ABS tx) (ABS ty) (ABS tz))))
            tx (FIX (* tx w))
            ty (FIX (* ty w))
            tz (FIX (* tz w))
            w (FIX w)))

  (fill-4by4-array out-array
                   w   0   0   tx
                   0   w   0   ty
                   0   0   w   tz
                   0   0   0   w))

;----------------------
(defun WIND (left right bottom top hither yon
             &optional  (ww 1.0) (out-array tdv*temp-array)
                        (temp1 tdv*temp-array1) (temp2 tdv*temp-array2)
             &aux  (wb tdv*basic-w) w a b c d e)

  (setq w (* ww wb))

  (IF (NULL out-array)
      (setq out-array (setq tdv*temp-array (MAKE-ARRAY '(4 4) ':type tdv*array-type))))

  (IF (NULL temp1)
      (setq temp1 (setq tdv*temp-array1 (MAKE-ARRAY '(4 4) ':type 'art-float))))

  (IF (NULL temp2)
      (setq temp2 (setq tdv*temp-array2 (MAKE-ARRAY '(4 4) ':type 'art-float))))

  (setq a (// (- right left) 2.0)
        b (// (+ right left) 2.0)
        c (// (- top bottom) 2.0)
        d (// (+ top bottom) 2.0)
        e     (- yon hither))

  (fill-4by4-array temp1
                   (// wb a) 0 0 0
                   0 (// wb c) 0 0
                   0 0 (// wb e) 0
                   0 0 0 (// wb ww))

  (fill-4by4-array temp2
                   ww 0 0 (- b)
                   0 ww 0 (- d)
                   0 0 ww (- hither)
                   0 0 0 ww)

  (T-ARRAY-MULTIPLY temp2 temp1 out-array))

;----------------------

(defun WINDP (left right bottom top hither yon eye
             &optional  (ww 1.0) (out-array tdv*temp-array)
                        (temp1 tdv*temp-array1) (temp2 tdv*temp-array2)
             &aux (wb tdv*basic-w) w a b c d e f w22)
  "Does perspective windowing.
   Note that coordinates are not in fix-point scale when in fix point mode.
   This scaling is done automatically"

  (setq w (* ww wb))

  (IF (NULL out-array)
      (setq out-array (setq tdv*temp-array (MAKE-ARRAY '(4 4) ':type tdv*array-type))))

  (IF (NULL temp1)
      (setq temp1 (setq tdv*temp-array1 (MAKE-ARRAY '(4 4) ':type 'art-float))))

  (IF (NULL temp2)
      (setq temp2 (setq tdv*temp-array2 (MAKE-ARRAY '(4 4) ':type 'art-float))))

  (setq a (// (- right left) 2.0)
        b (// (+ right left) 2.0)
        c (// (- top bottom) 2.0)
        d (// (+ top bottom) 2.0)
        e     (- yon hither)
        f     (- hither eye))

  (setq w22 (+ (// wb e) (// wb f)))

  (fill-4by4-array temp1
                   (// wb a) 0 0 0
                   0 (// wb c) 0 0
                   0 0 w22 0
                   0 0 (// wb f) (// wb ww))

  (fill-4by4-array temp2
                   ww 0 0 (- b)
                   0 ww 0 (- d)
                   0 0 ww (- hither)
                   0 0 0 ww)

  (T-ARRAY-MULTIPLY temp2 temp1 out-array))

;----------------------

(defun MAST (left right bottom top near far
             &optional (w 1.0) (out-array tdv*temp-array)
              &aux a b c d e f ws)

  (setq a (// (- right left) 2.0)
        b (// (+ right left) 2.0)
        c (// (- top bottom) 2.0)
        d (// (+ top bottom) 2.0)
        e (// (- far near  ) 2.0)
        f (// (+ far near  ) 2.0))

  (IF (NULL out-array)
      (setq out-array (setq tdv*temp-array (MAKE-ARRAY '(4 4) ':type tdv*array-type))))

  (COND (tdv*fix-mode
         (setq ws (// tdv*basic-w
                      (MAX 1 (ABS a) (ABS b) (ABS c) (ABS d) (ABS e) (ABS f) (ABS w))))
         (fill-4by4-array out-array
                          (FIX (* a ws)) 0 0 (FIX (* b ws))
                          0 (FIX (* c ws)) 0 (FIX (* d ws))
                          0 0 (FIX (* e ws)) (FIX (* f ws))
                          0 0 0 (FIX (* w ws))))
        (T
         (fill-4by4-array out-array
                          a 0 0 b
                          0 c 0 d
                          0 0 e f
                          0 0 0 w))))


;****************************************************************************************
;                                                                          Instances
;****************************************************************************************
(defun TETRA ( &optional (seg-name 'tetra))
  (COND ((S-INIT seg-name)
         (t-push)
         (t-fun 'scale 100 100 100)
         (s-line '((-1 -1 -1) (-1 1 -1) (1 1 -1) (1 -1 -1) (-1 -1 -1) (0 0 1) (1 1 -1)))
         (s-line '((-1 1 -1) (0 0 1) (1 -1 -1)))
         (t-pop)
         (S-CLOSE)))
  (S-TRANSFORM seg-name))

(defun ARROW-THING (&optional (seg-name 'arrow-thing))
  (COND ((S-INIT seg-name)
         (t-push)
         (t-fun 'scale 100 100 100)
         (t-fun 'rotz (* -1.25 %pi-float))
         (s-line '((-1 -1 0) (1 1 0)))
         (s-line '((1 -1 0) (-1 1 0)))
         (s-line '((0 0 1) (0 0 -1)))
         (s-line '((-.75 .75 .25) (-1 1 0) (-.75 .75 -.25) (-.75 .75 .25)))
         (s-line '((-.5 1 .0) (-1 1 0) (-1 .5 0) (-.5 1 .0)))
         (t-pop)
         (s-close)))
  (S-TRANSFORM seg-name))

(defun CUBE (&optional (seg-name 'cube))
  (COND ((S-INIT seg-name)
         (t-push)
         (t-fun 'scale 100 100 100)
         (s-line '((-1 -1 -1) (-1 1 -1) (1 1 -1) (1 -1 -1) (-1 -1 -1)
                   (-1 -1 1 ) (-1 1  1) (1 1 1) (1 -1  1) (-1 -1  1)))
         (s-line '((-1 1 -1) (-1 1 1)))
         (s-line '((1 1 -1) (1 1 1)))
         (s-line '((1 -1 -1) (1 -1 1)))
         (t-pop)
         (s-close)))
  (S-TRANSFORM seg-name))

(defun SQUARE (&optional (seg-name 'square))
  (COND ((S-INIT seg-name)
          (t-push)
           (t-fun 'scale 100 100 100)
           (s-line '((-1 -1) (-1 1) (1 1) (1 -1) (-1 -1)))
          (t-pop)
         (s-close)))
  (S-TRANSFORM seg-name))

(defun OCTAGON (&optional (seg-name 'octagon))
  (COND ((S-INIT seg-name)
         (WITH-T-PUSH
           (t-fun 'scale 100 100 100)
           (s-line '((-1 0 0) (0 -1 0) (1 0 0) (0 1 0) (-1 0 0)
                              (0 0 -1) (1 0 0) (0 0 1) (-1 0 0)))
           (s-line '((0 -1 0) (0 0 -1) (0 1 0) (0 0 1) (0 -1 0))))
         (s-close)))
  (S-TRANSFORM seg-name))

(defun COLOR-WHEEL (&optional (seg-name 'color-wheel) &aux theta c s)
  (COND ((S-INIT seg-name)
         (WITH-T-PUSH
           (t-fun 'scale 100 100 100)
           (DO ((i 0 (1+ i)))
               ((= i *number-of-colors*))
             (setq theta (* 2.0 %pi-float (// (FLOAT i) *number-of-colors*)))
             (setq c (COS theta)
                   s (SIN theta))
             (WITH-NEW-COLOR i
               (s-line `((0 0 -.2) (,c ,s 0) (0 0 .2))))))
         (S-CLOSE)))
  (S-TRANSFORM seg-name))

(defvar prop-angle 0)

(defun AIRPLANE (&optional (seg-name 'airplane))
  (COND ((S-INIT seg-name)
         (s-close)
           (square)                             ;create square and cube if need to
           (cube)
         (s-open seg-name)
         (WITH-T-PUSH
           (t-fun 'rotz  %pi-float)
           (WITH-T-PUSH
             (t-fun 'mast -100 100 -21 -20 -80 -40 )    ;bottom wing
             (t-fun 'scale .01 .01 .01)
             (t-fun 'rotx (// %pi-float 2))
             (square))


           (WITH-T-PUSH
             (t-fun 'mast -100 100 30 31 -90 -50 )      ;top wing
             (t-fun 'scale .01 .01 .01)
             (t-fun 'rotx (// %pi-float 2))
             (square))

           (WITH-T-PUSH
             (SET-CURRENT-COLOR 3)
             (t-fun 'mast -6 6 -25 20 -100 80)  ;body of plane
             (t-fun 'scale .01 .01 .01)
             (t-fun 'roty (// %pi-float 2))
             (cube)
             (SET-CURRENT-COLOR nil))


           (WITH-T-PUSH
             (t-fun 'mast -50 50 5 6 60 95 )    ;elevator
             (t-fun 'scale .01 .01 .01)
             (t-fun 'rotx (// %pi-float 2))
             (square))

           (WITH-T-PUSH
             (t-fun 'mast -5 5 00 60 80 100 )   ;rudder
             (t-fun 'scale .01 .01 .01)
             (t-fun 'roty (// %pi-float 2))
             (square))

           (s-close)                            ;close airplane

           (COND ((S-INIT 'prop)
                  (WITH-T-PUSH
                    (SET-CURRENT-COLOR 2)
                    (t-fun 'mast -1 1 -20 20 -101 -100)
                    (t-fun 'scale .01 .01 .01)
                    (square)
                    (SET-CURRENT-COLOR nil)
                    (s-close)))))))

  (WITH-T-PUSH                                  ;include prop
    (setq prop-angle (+ prop-angle .3))
    (t-fun 'rotz prop-angle)
    (s-transform 'prop))

  (S-TRANSFORM seg-name))                       ;include airplane

(defun funct (x y &optional (s 12) &aux (toll .001) r)
  (setq r (* s (SQRT (+ (* x x) (* y y)))))
  (COND ((< r toll) 1)
        (T
         (// (SIN r) r))))

(defun WAVES (&optional (seg-name 'waves) (del-x .1) (del-y .1) &aux zmn zmx zmy)
  (COND ((S-INIT seg-name)
         (WITH-T-PUSH
           (t-fun 'scale 100 100 100)

           (DO ((y (+ -1 del-y) (+ y del-y))
                (ym -1 (+ ym del-y)))
               ((> y 1))
             (DO ((x (+ -1 del-x) (+ x del-x))
                  (xm -1 (+ xm del-x)))
                 ((> x 1))
               (setq
                  zmn (funct x y)
                  zmx (funct xm y)
                  zmy (funct x ym))
               (s-line (list (list xm y zmx) (list x y zmn) (list x ym zmy)))))
           )
         (s-close)))
  (S-TRANSFORM seg-name))


(defun WAVESP (&optional (seg-name 'wavesp) (del-x .05) (del-y .05) )
  (COND ((S-INIT seg-name)
         (WITH-T-PUSH
           (t-fun 'scale 100 100 100)

           (DO ((y -1 (+ y del-y)))
               ((> y 1))
             (DO ((x -1 (+ x del-x)))
                 ((> x 1))
               (s-point (list (list x y (funct x y))))))
           )
         (s-close)))
  (S-TRANSFORM seg-name))




;****************************************************************************************
;                                                                   Debugging routines
;****************************************************************************************

(defun t-aprint (array)
  (do ((j 0 (1+ j)))
      ((= j 4))
    (format t "~%")
    (do ((i 0 (1+ i)))
        ((= i 4))
      (IF (FIXP (AREF array i j))
          (format t "~12<~5D~>" (AREF array i j))
          (format t "~12<~5F~>" (AREF array i j)))))
  (format t "~%")
  array)

(defvar the-list nil "dummy list used for grindef in debugging routines")

(defun t-top-print ()
  " Prints out whatever is in the stack top transformation array"
   (t-aprint tdv*stack-top))

(defun t-doc-print ()
  "Prints out documentation strings for the transformation array stack"
  (setq the-list (listarray tdv*stack-document (1+ tdv*stack-length)))
  (grindef the-list))

(defun t-oprint ()
  "This prints out a list of all open, closed, and empty segments"
  (setq the-list s*open-seg-stack)
  (grindef the-list)
  (DO ((x 0 (1+ x)))
      ((= x (ARRAY-LENGTH s*open-seg-array)))
    (format t "~% ~4D:  ~5D  -- ~A " x (Aref s*open-seg-array x) (Aref s*seg-names x))))

(defun t-sprint (seg-name
                 &aux
                 seg-number seg-array seg-array-length
                 line-info)

  " This routine prints out a draw list"

  (SETQ seg-number (S-GET-HASH-NUMBER seg-name))
  (IF (null seg-number)
      (ferror t  "~% Tried to print out a non-existant-segment : ~A. " seg-name))

  (setq seg-array        (AREF s*seg-arrays seg-number)
        seg-array-length (AREF s*seg-lengths seg-number))

 ; draw the stupid thing

  (DO ((l 0 (1+ l)))
      (( l seg-array-length))

    (setq line-info (AREF seg-array l))
    (Format t "~% ~D : " l)

    (DO ((j 0 (1+ j)))
        ((= j 5))
      (COND ((FIXP (aref line-info j))
             (format t " ~8<~3D~> " (aref line-info j)))
            (T
             (format t " ~8<~3F~> " (aref line-info j)))))))


;****************************************************************************************
;                                                                    Test routines
;****************************************************************************************
(defunp get-scaled-mouse-pos ()
  (RETURN (// (- tv:mouse-x tdv*window-xcenter) (FLOAT tdv*window-half-xdim))
          (// (- tv:mouse-y tdv*window-ycenter) (FLOAT tdv*window-half-ydim))))

(defun GO-TEST (&aux mx my)
  " This is a very well documented demo of the 3-D vector graphics package TREEDV.
    Almost everything that I can think of that can be done is being done in this routine.

    Notes:

     A)    Most of the instances (objects) in this package are created in a plus or minus
         100 sized box.  The square in 'thing, for instance will acutally be stored at the
         location '((-100 -100) (100 -100) (100 100) (-100 100) (-100 -100)) for instance.
         (This is done by this call to 'scale.)  Doing things this way makes all the
         previous instances compatable and switching things aroung becomes fairly easy.
         For example, if you wanted to use the already defined square instead of using the
         S-LINE command here, all that would be necessay would be to use the already
         defined program (SQUARE) outside of the T-PUSH region here.  Simiallarly, the
         Cube, octahedron, ect. programs could go here too.  They would then all appear in
         the segment 'thing being defined.

     B)    Probably the most confusing call in this whole package is the transformation
         call to WIND or WINDP to set up the viewing box size.  Both these routines are
         of the form (T-FUN 'wind<p> left right bottom top hither yon <eye> <w 1>), where
         <eye> only appears in the 'windp call, and w is a scale factor defaulted at 1.
         Left, right, top, and bottom define the dimensions and position of the front
         face of the viewbox along the x an y axis.  Hither and yon define the position
         of the front and back face along the z axis respectivly.  The scaling factor
         w is used to inversly scale the coefficents such that the real coefficents
         are treated as the inputed ones divided by w.  The call to
         (T-FUN 'windp -1 1 -1 1 -1 1 -4 .01) is the same as the call to
         (T-FUN 'windp -100 100 -100 100 -100 100 -400 1), and is alot easier to type.
           A non-perspective viewbox that displays only what is inside the boundarys of
         + or - 99 could be created by the call to (T-FUN 'wind -99 99 -99 99 -99 99)
         Note that points outside this viewbox are clipped, even if they are just in front
         of the hither plane or behind the you plane.  The point (0 0 100) would the
         be clipped in the above example.
           The routine WINDP setups a perspective viewport.  This is done by adding the
         additional parameter eye, which is the position of the viewer along the Z axis.
         The viewbox is then a trucated right pyramid with apex at (0 0 eye) and the
         dimensions of right < x < left, bottom < y < top at (0 0 hither).  The angle of
         view is therefore (ATAN  (- left right) (- hither eye)) along the x axis and
         (ATAN (- top bottom) (- hither eye)) along the y axis.  By moving the eye's
         position closer to the hither plane, a more wide angle field of view is obtained,
         similarly, by moving it futher away, a narrower field of view occurs. By moving
         the eye's position back and forth, a zoom lens sort of effect will occur. Note
         Note that regardless of the eyes position, objects right on the hither plane will
         always be the same size.  The eye's position by itself therefore can not be used
         to scale the size of the objects in the viewbox.  For achieve the effect of moving
         towards or away from the objects, the whole viewbox should be moved (or at least
         the postion of the eye and the hither plane should be moved together. )
           A good normal viewing angle occurs when the distance between the eye
         and the hither plane is about twice the size of that of the width and/or height
         of the viewbox.  The calls to (T-FUN 'windp -1 1 -1 1 -1 1000 -4 <w>) all over my
         routines result in a slightly wide angle veiwport (about the same as a 35mm
         camera lens) with a very distant yon plane.

    C)    The transformations any particullar object are done in a sort of last T-FUN call
        first tansformation that occurs.  Here, the objects are first rotated about the
        Z-axis, then the Y-axis and X axis, and finally go through the 'windp transformation.
        Any T-FUN call after this 'rotz call therefore occur before the object is rotated
        about the Z axis.  In general, think of the points undergoing transformations as
        column vectors V.  Given a set of transformations T0 T1 T2 and T3 in that order,
        the transformed vector V' = T0 T1 T2 T3 V.  Additional transformations are added
        after T3.
          Pushing the transformation stack copies the current transformation into a new array,
        wile popping returns the transformation array to the state that is was before the
        push.

  John Bradstreet  6-9-83
  ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ..."

  (S-ROUTINE-INIT 10)                           ; initalize package for max of ten segments

  (S-INIT 'thing1)                              ; initialize segment of name 'thing1
  (WITH-T-PUSH                                  ; macro to push t-stack in this region
    (T-FUN 'scale 100 100 100)                  ; multiply all xyz coords by 100. (See note A)
    (S-LINE '((-1 -1) (1 -1) (1 1) (-1 1) (-1 -1)))  ; draw a small square for example
    (S-LINE '((0 0 1) (0 0 -1))))               ; add a line on the z-axis
  (S-CLOSE)                                     ; close input to segment 'thing1 and pop stack

  (SET-NEW-COLOR 1 red)                         ; setup colors for color screen if have one
  (SET-NEW-COLOR 2 blue)                        ; note: red,blue,green ect. are initalized
  (SET-NEW-COLOR 3 green)                       ; in the module COLOR.LISP
  (SET-NEW-COLOR 4 cyan)                        ; they are list structures '(r g b) where
  (SET-NEW-COLOR 5 violet)                      ; r g and b are intensities from 0 to 255

  (S-INIT 'thing2)                              ; start a new object with name 'thing2
  (DO ((i 1 (1+ i)))                            ; stupid little loop
      ((= i 6))                                 ;
    (WITH-T-PUSH                                ; push T-stack again - important ! -
      (T-FUN 'scale (// i 6.0) (// i 6.0) (// i 6.0)) ; Shrink object to different sizes
      (T-FUN 'rotz (* i .2))                    ; rotate about the z axis too
      (T-FUN 'roty (* (- i 3) .2))              ; rotate about y axis some
      (WITH-NEW-COLOR i                         ; setup default colors for this object
        (S-TRANSFORM 'thing1)                   ; transform thing1 and add it to thing2
        )))                                     ; close off loops
  (S-CLOSE)                                     ; Don't forget to close off segment too

  (WITH-T-PUSH                                  ; push transformation (T-) stack again
    (T-FUN 'windp -100 100 -100 100 -100 10000 -400 1) ;setup viewing window (see note B)
    (T-FUN 'scale .8 .8 .8)

    (HACKS:WITH-REAL-TIME                       ; Make routine run much smoother

      (DO ((i 0 (1+ i)))                        ; set up display loop
          ((> i 10000))

        (WITH-A-NEW-SCREEN                      ; set up for frame swapping - important ! -

          (WITH-T-PUSH
            (T-FUN 'rotz (// i 40.0))           ; rotate about z-axis
            (WITH-NEW-COLOR (1+ (REMAINDER  i 5)) ; about xyz-axis too.
              (S-DRAW 'thing1)))                ; draw object 'thing1 above

          (WITH-T-PUSH                          ; push  T-stack inside loop  - important ! -
            (MULTIPLE-VALUE (mx my)             ; depending on location of mouse on viewport
              (GET-SCALED-MOUSE-POS))           ; return numbers from -1 to 1
            (T-FUN 'roty (- (* .1 mx %pi-float)))  ; make so moving mouse up/down rotates
            (T-FUN 'rotx    (* .1 my %pi-float))   ; things up/down, left/right similaryly
            (T-FUN 'rotz (// i -40.0))          ; rotate about z-axis (See Note C)
            (S-DRAW 'thing2))                   ; Draw 'thing2 in default colors

          (WITH-T-PUSH
            (T-FUN 'trans (* mx 100) (* my 100) 0)      ; use mouse to shift this thing around
            (T-FUN 'scale .1 .1 .1)             ; shrink object to tenth size
            (WITH-NEW-COLOR 2                   ; draw it in color 2 (blue)
              (S-DRAW 'thing1)))

          )))))                         ; close off all loops

;---------------------- ---------------------- ---------------------- --------------------

(defun go0 (&optional (del .05) &aux mx my)
  (s-routine-init)

  (s-init 'dummy)
    (cube)
  (s-close)

  (t-fun 'windp -1 1 -1 1 -1 1 -2 (// 1.0 150))

  (hacks:with-real-time
    (do ((i 0 (+ i del)))
        ((> i 1999))
      (multiple-value (mx my) (get-scaled-mouse-pos))

      (WITH-A-NEW-SCREEN
        (WITH-T-PUSH
           (s-draw 'dummy))))))


; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .


(defun go1 (&optional (del .05) &aux mx my)
  (s-routine-init)

  (airplane)                                    ;init airplane

  (s-init 'dummy)                               ;init segment 'dummy
  (s-close)

  (t-fun 'windp -1 1 -1 1 -1 1 -2 (// 1.0 150))

  (hacks:with-real-time
    (do ((i 0 (+ i del)))
        ((> i 1999))
      (multiple-value (mx my) (get-scaled-mouse-pos))

      (WITH-A-NEW-SCREEN
        (WITH-T-PUSH
          (t-fun 'roty (- (* .1 mx %pi-float)))
          (t-fun 'rotx    (* .1 my %pi-float))
          (s-open 'dummy)
            (WITH-T-PUSH
              (t-ident)
              (airplane))                       ;calling airplane this way makes prop rotate
            (s-draw 'dummy)
          (s-close)
          (s-clear 'dummy))))))

; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

(defun go2 ()
  (s-routine-init 10)

  (s-init 'dummy)                               ;create segement
    (airplane)                                  ;create all the instances in airplane
  (s-close)                                     ;close the dumb thing
  (s-clear 'dummy)                              ;and then blow it away

    (WITH-T-PUSH
    (t-fun 'windp -1 1 -1 1 -1 1 -4 (// 1.0 150.0))

    (do ((r 0 (+ r 0.03))
         (rr 1 (1+ rr)))
        (nil)

      (WITH-A-NEW-SCREEN
        (WITH-T-PUSH
          (t-fun 'rotx (* 2 r))
          (t-fun 'roty r)
          (s-open 'dummy)
            (WITH-T-PUSH
              (t-ident)
              (airplane))
            (s-draw 'dummy)
          (s-close)
          (s-clear 'dummy))))))

;. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
(defun go2.5 ()
  (s-routine-init 10)

    (waves)                                     ;create the hill

    (WITH-T-PUSH
    (t-fun 'windp -1 1 -1 1 -1 1 -4 (// 1.0 150.0))

    (do ((r 0 (+ r 0.03))
         (rr 1 (1+ rr)))
        (nil)

      (WITH-A-NEW-SCREEN
        (WITH-T-PUSH
          (t-fun 'rotx (* 2 r))
          (t-fun 'roty r)
            (s-draw 'waves))))))

;. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

(defun go3 ()
  (s-routine-init 10)

  (s-init 'junk)
  (arrow-thing)
  (tetra)
  (cube)

  (s-init 'zcubes)
  (do ((z 0 (1+ z)))
      ((= z 6))
    (WITH-T-PUSH
      (WITH-NEW-COLOR z
        (t-fun 'trans 0 0 (* (- z 2.5) 250))
        (cube))))
  (s-close)

  (s-close)
  (s-delete 'junk)

  (WITH-T-PUSH
    (t-fun 'windp -1 1 -1 1 -600 10000 -604 .8) ;not much scaling (w) needed since
                                                ;looking from far away

    (do ((r 0 (+ r 0.02))
         (rr 1 (1+ rr)))
        ((> r %pi-float))

      (WITH-A-NEW-SCREEN
        (WITH-T-PUSH
;           (t-fun 'trans 0 0 -500 )
          (t-fun 'roty r)
          (t-fun 'rotz r)
;           (t-fun 'trans 0 0 500)
          (s-draw 'zcubes))))))

;----------------------------------------------------------------------------------------

(defun go4 (&optional (del .05) &aux mx my theta (small-size .2))
  (s-routine-init)

  (color-wheel)
  (square)

  (t-fun 'windp -1 1 -1 1 -1 1 -4 (// 1.0 120))

  (hacks:with-real-time
    (do ((i 0 (+ i del))
         (ii 0 (1+ ii)))
        ((> i 1999))
      (multiple-value (mx my) (get-scaled-mouse-pos))

      (WITH-A-NEW-SCREEN
        (WITH-T-PUSH
          (t-fun 'roty i)
          (t-fun 'rotx i)
          (t-fun 'rotz i)
          (s-draw 'color-wheel)
          (DO ((j 0 (1+ j)))
              ((= j *number-of-colors*))
            (setq theta (* 2 %pi-float (// (FLOAT j) *number-of-colors*)))
            (WITH-T-PUSH
              (WITH-NEW-COLOR (+ j ii)
                (t-fun 'roty (- (* .1 mx %pi-float)))
                (t-fun 'rotx    (* .1 my %pi-float))

                (t-fun 'rotz theta)
                (t-fun 'trans (* 100 (1+ (* small-size .414))) 0 0)
                (t-fun 'rotx (+ theta (* ii .03)))
                (t-fun 'rotz (// %pi-float 4))
                (t-fun 'scale .1 .1 .1)
                (s-draw 'square)))))))))

;. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
(defmacro tan (x)
  `(// (SIN ,x) (COS ,x)))

(defun go5 (&optional (idd 1)                   ;3D view if have stereo glasses
                      (del .05)
            &aux
            mx my
            (main-w 100)
            (hither-minus-eye   2)              ;for perspective calcs (scaled by main-w))
            (object-from-hither 2)              ;dist: hither --> (0,0,0) (scaled by main-w)
            (distance-to-screen 18.0)           ;distance from eye to screen in inches
            (distance-between-eye-and-nose 1.5) ;also in inches
            (eye-scale (// distance-between-eye-and-nose distance-to-screen))
            eye-distance
            eye-angle object-distance)

  (setq eye-distance (* eye-scale main-w))      ;half width between eyes
  (setq object-distance (* Main-w (+ hither-minus-eye object-from-hither)))
                                                ;from nose to center of object
  (setq eye-angle (ATAN2  eye-distance object-distance))

  (s-routine-init 20)
  (square)                                      ;init segments
  (arrow-thing)
  (cube)
  (IF (= idd 1) (airplane))
  (IF (= idd 2) (color-wheel))
  (IF (= idd 4) (waves))
  (IF (= idd 5) (wavesp))

  (IF tdv*color-mode (shaded-color-map blue green))
  (set-new-color 0 black)
  (set-new-color 1 red)
  (set-new-color 2 cyan)                                ;note (ior 1 2) = 3
  (set-new-color 3 white)

  (s-init 'thing)
  (WITH-NEW-COLOR 3
    (WITH-T-PUSH
      (COND ((= idd 1)
             (airplane))
            ((= idd 2)
             (t-fun 'scale  1.4 1.4 1.4)
             (color-wheel))
            ((= idd 3)
             (cube))
            ((= idd 4)
             (t-fun 'scale 1.5 1.5 -1.8)
             (waves))
            ((= idd 5)
             (t-fun 'scale 1.5 1.5 -1.8)
             (wavesp))
            (t
             (WITH-T-PUSH
               (t-fun 'scale  .8 .8 .8)
               (DO ((i -400 (+ 50 i)))
                   ((> i 400))
                 (WITH-T-PUSH
                   (t-fun 'trans 0 0 i)
                   (t-fun 'rotz (// i 200.0))
                   (square)))))
            )))
  (s-close)

  (s-init 'thing-to-draw nil)                   ;create but don't open

  (hacks:with-real-time
    (do ((i 0 (+ i del))
         (ii 0 (1+ ii)))
        ((> i 1999))
      (multiple-value (mx my) (get-scaled-mouse-pos))

;  Transform Object

      (COND (t
             (S-open 'thing-to-draw)
             (WITH-T-PUSH
               (t-ident)
               (t-fun 'roty (- (* .1 mx %pi-float)))
               (t-fun 'rotx    (* .1 my %pi-float))
               (t-fun 'rotz    i)
               (COND ((= idd 1)
                      (airplane))               ;this is to get spinning prop
                     (t
                      (s-transform 'thing))))
             (s-close)
             ))

;   Draw object with left and right eye views

      (WITH-A-NEW-SCREEN
        (WITH-T-PUSH
            ; perspective transformation
          (t-fun 'windp -1 1 -1 1 hither-minus-eye 1000 0 (// 1.0 main-w))

          (DRAW-SEGMENT-IN-3D 'thing-to-draw eye-angle eye-distance object-distance)

          ))

      (s-clear 'thing-to-draw)
      )))



(defun DRAW-SEGMENT-IN-3D (segment-name eye-angle eye-distance object-distance)

  "This routine is to draw 3-D objects in 3-D on a 4-bit display using red/cyan glasses.
   Note that color 1 is assumed to be red, color 2 cyan, and color three white"

            ;right eye view (in red)
  (WITH-T-PUSH
    (WITH-NEW-COLOR 1
      (IF (NOT tdv*color-mode) (t-size-window .5 .5 .4 0))  ;this is for B&W screens
      (t-fun 'roty eye-angle)
      (t-fun 'trans (- eye-distance) 0 object-distance)
      (s-draw segment-name)))

            ;left eye view (in cyan)
  (WITH-T-PUSH
    (WITH-NEW-COLOR 2
      (IF (NOT tdv*color-mode) (t-size-window .5 .5 -.4 0))
      (t-fun 'roty  (- eye-angle))
      (t-fun 'trans eye-distance 0 object-distance)
      (s-draw segment-name tv:alu-ior))))


;. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

(defun bounce (&optional (vmax 10) (eye -2)
               &aux (edge 1000) px py pz (vx 0) (vy 0) (vz 0) edge-plus)


  (s-routine-init)

  (s-init 'dummy)
    (cube)
    (square)
  (s-close)

  (s-init 'stuff)
    (tetra)
  (s-close)

  (s-init 'junk nil)                            ;init but don't open it

  (setq edge-plus (+ edge 100))

  (setq px (- (RANDOM (* 2 edge)) edge)
        py (- (RANDOM (* 2 edge)) edge)
        pz (- (RANDOM (* 2 edge)) edge))

  (T-FUN 'windp -1 1 -1 1 -1 1 eye (// 1.0 edge-plus))

  (hacks:with-real-time
    (DO ((i 0 (1+ i))
         (angle 0)
         (dr .07))
        ((= i 10000))

      (WITH-A-NEW-SCREEN
        (WITH-T-PUSH

          (WITH-T-PUSH                          ;draw boarder
            (t-fun 'scale (// edge 100.0) (// edge 100.0) (// edge 100.0))
            (s-draw 'cube))

          (setq vx (+ vx (- (RANDOM (* 2 vmax)) vmax))
                vy (+ vy (- (RANDOM (* 2 vmax)) vmax))
                vz (+ vz (- (RANDOM (* 2 vmax)) vmax)))
          (setq px (+ px vx)
                py (+ py vy)
                pz (+ pz vz))

          (IF (> px edge) (setq vx (- vx)       ;bounce off right side
                                px (+ edge (- edge px))
                                dr (- dr)))
          (IF (> py edge) (setq vy (- vy)       ;bounce off bottom side
                                py (+ edge (- edge py))
                                dr (- dr)))
          (IF (> pz edge) (setq vz (- vz)       ;bounce off back side
                                pz (+ edge (- edge pz))
                                dr (- dr)))

          (IF (< px (- edge)) (setq vx (- vx)   ;bounce off left side
                                    px (- (+ px edge edge))
                                    dr (- dr)))
          (IF (< py (- edge)) (setq vy (- vy)   ;bounce off top side
                                    py (- (+ py edge edge))
                                    dr (- dr)))
          (IF (< pz (- edge)) (setq vz (- vz)   ;bounce off front side
                                    pz (- (+ pz edge edge))
                                    dr (- dr)))

          (setq angle (+ angle dr))

          (WITH-T-PUSH
            (WITH-NEW-COLOR 2
              (T-fun 'trans px edge pz)
              (T-fun 'rotx (// %pi-float 2))
              (s-draw 'square)))

          (WITH-T-PUSH
            (WITH-NEW-COLOR 3
              (T-fun 'trans edge py pz)
              (T-fun 'roty (// %pi-float 2))
              (s-draw 'square)))

          (WITH-T-PUSH
            (WITH-NEW-COLOR 4
              (T-fun 'trans px py edge)
              (T-fun 'rotz (// %pi-float 2))
              (s-draw 'square)))


          (WITH-T-PUSH
            (WITH-NEW-COLOR i
              (T-fun 'trans px py pz)
              (T-fun 'rotz angle)
              (T-fun 'roty angle)
              (S-draw 'stuff)))

          )))))
;----------------------------------------------------------------------------------------

(hacks:defdemo "Vector Graphic Hacks"
               "Demos to do 3-D vector graphics on 1 and 4-bit color machines"
  "Vector Graphic Hacks"
  ("Documentation" "Some documentation on this routine" (3d-document))
  ("Airplane" "Airplane with spinning Prop" (Go1))
  ("Boxes" "Over flight of a sequence of boxes" (Go3))
  ("Color Wheel" "Bizzar Wheel - looks neat on Color Window" (Go4))
  ("Hill" "Sin(r)/r Hill drawn with Grid" (Go2.5))
  ("Squares and Things" "Random Demo that does all sorts of things" (GO-test))
  ("stereo - Airplane" "Airplane in Stereo" (Go5 1))
  ("stereo - Helix" "Squares in a helix for stereo " (Go5 7))
  ("stereo - Hill" "Sin(r)/r function in stereo" (Go5 4))
  ("stereo - Hill - dotted" "Sin(r)/r with dots only in stereo" (Go5 5))
  ("Swap Screens" "Alternate between Color and B&W Screen" (Swap-Screens))
  ("List Current Segments" "Lists the Current Segments and Their open Status" (t-oprint)))
