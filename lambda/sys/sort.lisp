;SORT PACKAGE   -*- Mode:LISP; Package:SI; Base:8; readtable: ZL -*-
;       ** (c) Copyright 1980 Massachusetts Institute of Technology **

;ENTRIES
;  SORT <list or array> <lessp predicate>
;  MERGE-SORT <list> <lessp predicate> <key>
;     A faster non-destructive sort.  Make sure the
;     garbage collector is on.
;  SORTCAR  ..  ..
;  SORT-SHORT-LIST <list> <lessp predicate>
;     A simple exchange sort, good for short lists.  Need not be contiguous.
;  SORTCAR-SHORT-LIST .. ..
;  STABLE-SORT -- slower version of SORT which is guaranteed to be stable
;     Note that SORT itself is stable on non-cdr-coded lists.
;  SORT-GROUPED-ARRAY <array-only> <group-size> <lessp predicate>
;     Assumes logical records come in groups of <group-size> entries.
;             the key is the first entry of the group.
;     Uses ARRAY-ACTIVE-LENGTH to determine portion of array to be sorted.
;  SORT-GROUPED-ARRAY-GROUP-KEY <array-only> <group-size> <lessp predicate>
;     Similar to SORT-GROUPED-ARRAY, but <lessp predicate> should be a function
;     of four arguments, <array1> <idx1> <array2> <idx2>.  Thus, it can
;     reference the entire group, if desired, not just the first element.

;INTERNAL FUNCTIONS
;  SORT-ARRAY-QUICK <array> <left index> <right index>
;       Uses insertion sort if small, otherwise Quicksort
;       Indices are inclusive.
;  SORT-GROUPED-ARRAY-QUICK <array> <left index> <right index> <group-size>
;       Analogous to above for use by SORT-GROUPED-ARRAY.
;  SORT-GROUPED-ARRAY-GROUP-KEY-QUICK <array> <left index> <right index> <group-size>
;       Analogous for SORT-GROUPED-ARRAY-GROUP-KEY.
;  SORT-CONTIG-LIST-QUICK <list> <length>
;  SORT-LIST <list>
;       Hacks contiguous lists, does combined merge and quick sort
;  SORT-LIST-PREFIX <height>
;  SORT-LIST-MERGE <list1> <list2>
;  SORT-LIST-MERGE-CONTIG <list1> <length1> <list2> <length2>

;SUBROUTINE (SHOULD BE PUT INTO THE NUCLEUS IN SOME FORM)
;  CONTIGUOUS-LIST-INFO <list>
;     Returns 2 values:
;       Number of contiguous CDR-NEXTs in <list> (1- the number of contiguous CAR cells)
;       "last" of the contiguous part.  CAR of this is last CAR cell, CDR is link
;       the non-contiguous part of the list.
;     If you call this with an argument of NIL, it will either loop or err out.

(DEFVAR SORT-LESSP-PREDICATE)
(DEFVAR SORT-KEY-FUNCTION)
(DEFVAR SORT-INPUT-LIST)
(DEFVAR SORT-DUMMY-ARRAY-HEADER)
(DEFVAR SORT-QS-BREAKEVEN 10)
(DEFVAR SORT-ARRAY-TEMP-V)

(DEFMACRO APPLY-PREDICATE-AND-KEY (LPRED KEYFUN ARG1 ARG2)
  `(COND ((NULL ,KEYFUN) (FUNCALL ,LPRED ,ARG1 ,ARG2))
         ((EQ ,KEYFUN #'CAR) (FUNCALL ,LPRED (CAR ,ARG1) (CAR ,ARG2)))
         (T (FUNCALL ,LPRED (FUNCALL ,KEYFUN ,ARG1)
                     (FUNCALL ,KEYFUN ,ARG2)))))

;Special Considerations
;
; GC must never de-linearize lists.
;  The hairy version of NRECONC (NREVERSE) depends on this too.
; Note that a list can get de-linearized by the GC finding a pointer
;  to the middle and copying from there.  One way around this is to
;  set up an arrangement to be interrupted, signalled, thrown-through,
;  or whatever when a flip happens, then at the time the size of a
;  contiguous segment of list is counted, ensure that everything is
;  in newspace (already copied).  Great care is required.
;;; The above is not true.  The GC will not delinearize lists even if
;;; it does find a pointer into the middle of a list.  If you don't
;;; want the storage to move, a simple (WITHOUT-FLIPPING ...) should
;;; suffice.

(DEFUN STABLE-SORT (X LESSP-PREDICATE &KEY &OPTIONAL KEY
                    &AUX TEM (SORT-KEY-FUNCTION KEY))
  "Sort the list or array X by comparing the elements, not rearranging /"equal/" elements.
SORT-LESSP-PREDICATE is applied to a pair of elements
and should return non-NIL if the first element is /"less/" than the second.
Two elements are /"equal/" if the predicate returns NIL
when given those two elements in either order.
If KEY is non-NIL, it is a function to apply to each element
to get the thing to pass to the predicate; using CAR for KEY
gets the effect of STABLE-SORTCAR.
The list or array is modified destructively."
  (COND ((NULL X) NIL)
        ((CONSP X)
         (COND ((< (LENGTH X) 20.) (SORT-SHORT-LIST X LESSP-PREDICATE KEY))
               (T (SORT-LIST-STABLE X LESSP-PREDICATE KEY))))
        ((ARRAYP X)
         (SORT-ARRAY-STABLE X LESSP-PREDICATE KEY)
         X)
        ((AND (SYMBOLP X)
              (ARRAYP (SETQ TEM (FSYMEVAL X))))
         (SORT-ARRAY-STABLE TEM LESSP-PREDICATE KEY)
         X)
        ((ERROR "ARG MUST BE A LIST OR AN ARRAY - STABLE-SORT" X))))

(DEFUN STABLE-SORTCAR (X PREDICATE)
  "Sort the list or array X by comparing the cars of the elements, not rearranging /"equal/" elements.
PREDICATE is applied to two elements' cars.
Two elements are /"equal/" if the predicate returns NIL
when given those two elements in either order.
The list or array is modified destructively."
  (STABLE-SORT X PREDICATE ':KEY #'CAR))

(DEFUN SORT-LIST-STABLE (L LESSP-PREDICATE KEY)
  (LET ((TEMP (MAKE-ARRAY (* 2 (LENGTH L)) ':TYPE ART-Q-LIST))
        SORTED)
    ;; Make the contents of TEMP be a totally non-cdr-coded list
    ;; whose elements are those of A.
    (DO ((TAIL L (CDR TAIL))
         (I 0 (+ I 2)))
        ((NULL TAIL))
      (without-interrupts
        (SETF (AREF TEMP I) (CAR TAIL))
        (%P-STORE-CDR-CODE (ALOC TEMP I) CDR-NORMAL)
        (IF (NULL (CDR TAIL))
            (RETURN))
        (SETF (AREF TEMP (1+ I)) (%MAKE-POINTER DTP-LIST (ALOC TEMP (+ 2 I))))))
    ;; SORT is stable on totally non-cdr-coded lists.
    (SETQ SORTED (SORT (G-L-P TEMP) LESSP-PREDICATE ':KEY KEY))
    ;; Copy the data back into L.
    (DO ((TAIL L (CDR TAIL))
         (STAIL SORTED (CDR STAIL)))
        ((NULL TAIL))
      (SETF (CAR TAIL) (CAR STAIL)))
    L))

;Barbarian cunning, not so slow, and works.
(DEFUN SORT-ARRAY-STABLE (A LESSP-PREDICATE KEY)
  (UNLESS ( (LENGTH A) 1)
    (LET ((TEMP (MAKE-ARRAY (* 2 (LENGTH A)) ':TYPE ART-Q-LIST)))
      ;; Make the contents of TEMP be a totally non-cdr-coded list
      ;; whose elements are those of A.
      (DO ((J 0 (1+ J))
           (I 0 (+ I 2))
           (LEN (LENGTH A)))
          ((= J LEN))
        (without-interrupts
          (SETF (AREF TEMP I) (AREF A J))
          (%P-STORE-CDR-CODE (ALOC TEMP I) CDR-NORMAL)
          (IF (= J (1- LEN))
              (RETURN))
          (SETF (AREF TEMP (1+ I)) (%MAKE-POINTER DTP-LIST (ALOC TEMP (+ 2 I))))))
      ;; SORT is stable on totally non-cdr-coded lists.
      (FILLARRAY A (SORT (G-L-P TEMP) LESSP-PREDICATE ':KEY KEY)))))

(DEFUN SORTCAR (X PREDICATE)
  "Sort the list or array X, whose elements should be lists, by comparing their cars.
PREDICATE is applied to two elements' cars.
The list or array is modified destructively."
  (SORT X PREDICATE ':KEY #'CAR))

(DEFUN SORT (X SORT-LESSP-PREDICATE &KEY &OPTIONAL KEY
             &AUX TEM (SORT-KEY-FUNCTION KEY))
  "Sort the list or array X by comparing the elements.
SORT-LESSP-PREDICATE is applied to a pair of elements
and should return non-NIL if the first element is /"less/" than the second.
If KEY is non-NIL, it is a function to apply to each element
to get the thing to pass to the predicate; using CAR for KEY
gets the effect of SORTCAR.
The list or array is modified destructively."
  (COND ((CONSP X)
         (COND ((< (LENGTH X) 12.) (SORT-SHORT-LIST X SORT-LESSP-PREDICATE KEY))
               (T (SORT-LIST X))))
        ((NULL X)                                       ;NIL IS A LIST, SORT OF
         X)
        ((ARRAYP X)
         (SORT-ARRAY-QUICK X 0 (1- (ARRAY-ACTIVE-LENGTH X)))
         X)
        ((AND (SYMBOLP X)
              (ARRAYP (SETQ TEM (FSYMEVAL X))))
         (SORT-ARRAY-QUICK TEM 0 (1- (ARRAY-LENGTH TEM)))
         X)
        ((ERROR "ARG MUST BE A LIST OR AN ARRAY - SORT" X))))

;; Just a bubble sort.
(DEFUN SORT-SHORT-LIST (L LPRED KEYFUN)
  (COND ((CDR L)
         (DO ((I (1- (LENGTH L)) (1- I))
              (SWITCH NIL))
             ((OR (ZEROP I) SWITCH))
            (SETQ SWITCH T)
            (DO LP L (CDR LP) (NULL (CDR LP))
              (COND ((APPLY-PREDICATE-AND-KEY LPRED KEYFUN (CADR LP) (CAR LP))
                     (RPLACA LP (PROG1 (CADR LP) (RPLACA (CDR LP) (CAR LP))))
                     (SETQ SWITCH NIL)))))))
  L)

(DEFUN SORTCAR-SHORT-LIST (L LPRED)
 (PROG (LP SWITCH)
       (COND ((NULL (CDR L))
              (RETURN L)))
   L0  (SETQ LP L)
   L1  (COND ((FUNCALL LPRED (CAADR LP) (CAAR LP))
              (RPLACA LP (PROG1 (CADR LP) (RPLACA (CDR LP) (CAR LP))))
              (SETQ SWITCH T)))
        (SETQ LP (CDR LP))
        (COND ((CDR LP) (GO L1))
              (SWITCH (SETQ SWITCH NIL)
                      (GO L0)))
        (RETURN L)))

(DEFUN CONTIGUOUS-LIST-INFO (LIST)
  "The first value is the number of CDR-NEXT cells in LIST.
The second is the non-cdr-next tail of LIST, whose cdr
is either NIL or not contiguous with that cell.
It is not very meaningful to supply NIL as the argument."
  (DECLARE (RETURN-LIST NUMBER-OF-CDR-NEXT NON-CDR-NEXT-CELL))
  (PROG ((N 0))
     (IF (NULL LIST) (RETURN (values 0 NIL)))
LOOP (OR (AND (= (%P-CDR-CODE LIST) CDR-NEXT)
              (NEQ (%P-DATA-TYPE LIST) DTP-RPLACD-FORWARD))
         (RETURN (values N LIST)))
     (SETQ N (1+ N) LIST (CDR LIST))
     (GO LOOP)))

;;; Note, LENGTH is 1- real length
(DEFUN SORT-CONTIG-LIST-QUICK (LIST LENGTH &AUX LLOC FLOC)
  (SETQ LLOC (%MAKE-POINTER DTP-LOCATIVE LIST))
  (IF SORT-DUMMY-ARRAY-HEADER
      (CHANGE-INDIRECT-ARRAY SORT-DUMMY-ARRAY-HEADER 'ART-Q-LIST (1+ LENGTH)
                             LLOC NIL)
      (SETQ SORT-DUMMY-ARRAY-HEADER (MAKE-ARRAY (1+ LENGTH)
                                                ':TYPE 'ART-Q-LIST
                                                ':DISPLACED-TO LLOC)))
  (SETQ LLOC (%MAKE-POINTER-OFFSET DTP-LIST LIST LENGTH))
  (COND ((= DTP-RPLACD-FORWARD (%P-DATA-TYPE LLOC))
         (without-interrupts
           (SETQ FLOC (%P-CONTENTS-AS-LOCATIVE LLOC))
           ;; Replace the RPLACD forwarding pointer with the CAR it points to
           (%P-STORE-TAG-AND-POINTER LLOC (%P-DATA-TYPE FLOC) (%P-POINTER FLOC))
           (%P-DPB CDR-NIL %%Q-CDR-CODE LLOC))))
  (SORT-ARRAY-QUICK SORT-DUMMY-ARRAY-HEADER 0 LENGTH)   ;Call array quicksort on it
  (COND (FLOC
         (without-interrupts
           ;; Update the CAR pointed to with the correct element of the sorted partial list
           (RPLACA FLOC (CAR LLOC))
           (%P-STORE-TAG-AND-POINTER LLOC DTP-RPLACD-FORWARD FLOC)
           (%P-DPB CDR-ERROR %%Q-CDR-CODE LLOC)))))

; List sorting algorithm
;
; Due to MJF and GLS.
;
; The basic idea is to do a merge sort, which gets the list into
; order by doing RPLACDs.  (This is the same algorithm as is
; used for sorting lists in Maclisp.)  It operates by considering
; the given list to be the frontier of a binary tree (which may be
; incomplete if the length of the list is not a power of two).
; At each node, the two nodes below it are merged.  The frontier
; nodes are one-element lists, these are then merged into bigger lists.
; Instead of the usual method of merging all pairs, then all pairs
; of pairs, etc., this implementation effectively does a suffix walk
; over the binary tree (thus it can grab items sequentially off the given list.)
; Warning: like DELQ and others, the safe way to use this
; function is (SETQ FOO (ALPHASORT FOO)) or whatever.
;
; On the lisp machine, the above algorithm does not work well, because
; cdr-coded (contiguous) lists cannot be RPLACD'ed without implicit CONSing.
; Instead, contiguous chunks of the list are sorted in place.
; The idea is to use a merge sort on the list of contiguous chunks
; and to be a little hairy when comparing two chunks
; in the merge.  First, on encountering each chunk it is sorted
; (using quicksort).  Then, when two chunks meet during a merge,
; they are merged together in place, one getting all the low elements
; and one all thee high elements.  Deciding which one to use for the
; high chunk is a little tricky; note the code carefully.
; The two chunks are combined by a straight insertion technique; there may be
; better ways to combine two already sorted chunks.  Another approach
; not used here would be not to sort each chunk using quicksort except
; the first, and then to be hairier about the insertion technique.

(DEFUN SORT-LIST (SORT-INPUT-LIST &AUX SORT-DUMMY-ARRAY-HEADER)
  (DO ((HEIGHT -1 (1+ HEIGHT))
       (SOFAR NIL))
      ((NULL SORT-INPUT-LIST) SOFAR)
    (SETQ SOFAR (SORT-LIST-MERGE SOFAR (SORT-LIST-PREFIX HEIGHT)))))

(DEFUN SORT-LIST-PREFIX (HEIGHT &AUX LENGTH LAST)       ;GET MERGED BINARY TREE, SPECD HEIGHT
  (COND ((NULL SORT-INPUT-LIST) NIL)                    ;INPUT EXHAUSTED, INCOMPLETE TREE
        ((< HEIGHT 1)
         (MULTIPLE-VALUE (LENGTH LAST)                  ;PULL OFF A CONTIGUOUS SEGMENT OF LIST
               (CONTIGUOUS-LIST-INFO SORT-INPUT-LIST))
         (AND (> LENGTH 0)                              ;IF MORE THAN A SINGLE CELL, SORT IT.
              (SORT-CONTIG-LIST-QUICK SORT-INPUT-LIST LENGTH))
         (PROG1 SORT-INPUT-LIST                         ;RETURN THAT SEGMENT
                (AND (SETQ SORT-INPUT-LIST (CDR LAST))  ;ADVANCE TO NEXT
                     (RPLACD LAST NIL))))               ;MAKE SURE RETURNED SEGMENT ENDS
        ((SORT-LIST-MERGE (SORT-LIST-PREFIX (1- HEIGHT))
                          (SORT-LIST-PREFIX (1- HEIGHT))))))

(DEFUN SORT-LIST-MERGE (L1 L2 &AUX R)                   ;MERGE TWO SORTED LISTS, HACKING CONTIG
  (DO ((P (LOCF R))                     ;R ACCUMULATES RESULT, P POINTS TO TAIL
       (LAST1) (LENGTH1) (LAST2) (LENGTH2) (HIGH1) (HIGH2))
      ((COND ((NULL L1)                                 ;IF AN INPUT IS EXHAUSTED, DONE
              (RPLACD P L2)
              (RETURN R))
             ((NULL L2)
              (RPLACD P L1)
              (RETURN R))))
    (MULTIPLE-VALUE (LENGTH1 LAST1) (CONTIGUOUS-LIST-INFO L1))  ;PULL OFF A CONTIGUOUS CHUNK
    (MULTIPLE-VALUE (LENGTH2 LAST2) (CONTIGUOUS-LIST-INFO L2))  ;OF EACH LIST
    (SETQ HIGH1 (CAR LAST1) HIGH2 (CAR LAST2))
    (COND ((APPLY-PREDICATE-AND-KEY SORT-LESSP-PREDICATE SORT-KEY-FUNCTION
                                    HIGH2 (CAR L1))     ;SEE IF CHUNK2 ALL < CHUNK1
           (RPLACD P L2)
           (SETQ P LAST2 L2 (CDR LAST2)))
          ((OR (AND (= LENGTH1 0) (= LENGTH2 0))                ;SMALL CHUNKS, BYPASS HAIR
               (APPLY-PREDICATE-AND-KEY SORT-LESSP-PREDICATE SORT-KEY-FUNCTION
                                        HIGH1 (CAR L2)))        ;SEE IF CHUNK1 ALL < CHUNK2
           (RPLACD P L1)
           (SETQ P LAST1 L1 (CDR LAST1)))
          ;; GOT TO MERGE CHUNKS, CHOOSE HIGHER.  BUT CORRECT THE LENGTHS FIRST.
          ((APPLY-PREDICATE-AND-KEY SORT-LESSP-PREDICATE SORT-KEY-FUNCTION
                                    HIGH1 HIGH2)
           (SORT-LIST-MERGE-CONTIG L1 LENGTH1 L2 LENGTH2)
           (RPLACD P L1)
           (SETQ P LAST1 L1 (CDR LAST1)))
          (T
           (SORT-LIST-MERGE-CONTIG L2 LENGTH2 L1 LENGTH1)
           (RPLACD P L2)
           (SETQ P LAST2 L2 (CDR LAST2))))))

;MACROS FOR NEXT FUNCTION, ALLOW HACKING OF THE TWO LISTS AS ONE ARRAY.
;ALSO NOTE THE EVALUATION OF THE SUBSCRIPT SHOULD NOT HAVE SIDE-EFFECTS.

(DEFMACRO SORT-LIST-AREF (I)
  `(COND ((< ,I N1) (%P-CONTENTS-OFFSET L1 ,I))
         ((= ,I N1) (IF (ZEROP N1) (CAR L1)
                        (CADR (%MAKE-POINTER-OFFSET DTP-LIST L1 (1- N1)))))
         ((= ,I N1+N2+1) (IF (ZEROP N2) (CAR L2)
                             (CADR (%MAKE-POINTER-OFFSET DTP-LIST L2 (1- N2)))))
         (T (%P-CONTENTS-OFFSET L2 (- ,I (1+ N1))))))

(DEFMACRO SORT-LIST-ASET (X I)
  `(COND ((< ,I N1) (%P-STORE-CONTENTS-OFFSET ,X L1 ,I))
         ((= ,I N1) (IF (ZEROP N1) (RPLACA L1 ,X)
                        (RPLACA (CDR (%MAKE-POINTER-OFFSET DTP-LIST L1 (1- N1))) ,X)))
         ((= ,I N1+N2+1) (IF (ZEROP N2) (RPLACA L2 ,X)
                             (RPLACA (CDR (%MAKE-POINTER-OFFSET DTP-LIST L2 (1- N2))) ,X)))
         (T (%P-STORE-CONTENTS-OFFSET ,X L2 (- ,I (1+ N1))))))

;SIMPLE-MINDED INSERTION-SORT TAIL-END TO MERGE TWO SORTED ARRAYS
(DEFUN SORT-LIST-MERGE-CONTIG (L1 N1 L2 N2 &AUX (N1+N2+1 (+ N1 N2 1)))
  (DO ((I (1+ N1) (1+ I)))
      ((> I N1+N2+1))
    (DO ((J (1- I) (1- J))
         (X (SORT-LIST-AREF I)))
        ((OR (< J 0)
             (NOT (APPLY-PREDICATE-AND-KEY SORT-LESSP-PREDICATE SORT-KEY-FUNCTION
                                           X (SORT-LIST-AREF J))))
         (SORT-LIST-ASET X (1+ J)))
      (SORT-LIST-ASET (SORT-LIST-AREF J) (1+ J)))))

(defun merge-sort (list comparison key)
  "Non destructive merge sort that conses like mad.
It also runs slightly faster than the old sort of sort."
  (labels
    ((split-list (l)
       (do ((l-parts l (cdr l-parts))
            (half '() other-half)
            (other-half '() (cons (first l-parts) half)))
           ((null l-parts) (values half other-half))))

     (merge-lists (comparison key l1 l2)
       (let* ((head-pointer (cons '() '()))
              (tail-pointer head-pointer)
              (l1-left l1)
              (l2-left l2))
         (do-forever
           (cond ((null l2-left)
                  (setf (cdr tail-pointer) l1-left)
                  (return-from merge-lists (cdr head-pointer)))
                 ((null l1-left)
                  (setf (cdr tail-pointer) l2-left)
                  (return-from merge-lists (cdr head-pointer)))
                 ('else
                  (let ((l1c (first l1-left))
                        (l2c (first l2-left)))
                    (if (funcall comparison (funcall key l1c) (funcall key l2c))
                        (progn (setf (cdr tail-pointer) l1-left)
                               (setq l1-left (cdr l1-left)))
                        (progn (setf (cdr tail-pointer) l2-left)
                               (setq l2-left (cdr l2-left)))))
                  (setq tail-pointer (cdr tail-pointer)))))))
     )
    (cond ((null list) nil)
          ((null (cdr list)) list)
          ('else
           (multiple-value-bind
             (half other-half)
               (split-list list)
             (merge-lists comparison key
                          (merge-sort half comparison key)
                          (merge-sort other-half comparison key)))))))


;Quicksort for arrays.  If the array is small, does an insertion sort instead.

(DEFUN SORT-ARRAY-QUICK (A L R)
       (COND ((> L (- R SORT-QS-BREAKEVEN))             ;SEE IF SHOULD DO AN INSERTION SORT
              (DO ((I (1+ L) (1+ I)))                   ;THIS CLAUSE ALSO APPLIES WHEN L>R
                  ((> I R))
                (DO ((J (1- I) (1- J))
                     (X (AR-1 A I)))
                    ((OR (< J L)
                         (NOT (APPLY-PREDICATE-AND-KEY
                                SORT-LESSP-PREDICATE SORT-KEY-FUNCTION
                                X (AR-1 A J))))
                     (AS-1 X A (1+ J)))
                  (AS-1 (AR-1 A J) A (1+ J)))))
             (T ((LAMBDA (N)                            ;RANDOMLY CHOSEN POINT BETWEEN L AND R
                         ((LAMBDA (M)                   ;BREAK-POINT BETWEEN LOW AND HIGH
                                  (SORT-ARRAY-QUICK A L (1- M))         ;SORT THE LOW ELEMENTS
                                  (SORT-ARRAY-QUICK A (1+ M) R))        ;SORT THE HIGH ELEMENTS
                          (DO ((K (PROG1 (AR-1 A N)     ;K WILL BE M'TH ELEMENT
                                         (AS-1 (AR-1 A L) A N)))
                               (I L)                    ;A[...I-1] < K
                               (J R))                   ;K < A[J+1...]
                              (NIL)
                           DECRJ                        ;DECREASE J UNTIL K NOT LT A[J]
                            (COND ((= J I)
                                   (AS-1 K A I)
                                   (RETURN I))
                                  ((APPLY-PREDICATE-AND-KEY
                                     SORT-LESSP-PREDICATE SORT-KEY-FUNCTION
                                     K (AR-1 A J))
                                   (SETQ J (1- J))
                                   (GO DECRJ)))
                            (AS-1 (AR-1 A J) A I)
                            (SETQ I (1+ I))
                           INCRI                        ;INCREASE I UNTIL K NOT GT A[I]
                            (COND ((= I J)
                                   (AS-1 K A J)
                                   (RETURN J))
                                  ((APPLY-PREDICATE-AND-KEY
                                     SORT-LESSP-PREDICATE SORT-KEY-FUNCTION
                                     (AR-1 A I) K)
                                   (SETQ I (1+ I))
                                   (GO INCRI)))
                            (AS-1 (AR-1 A I) A J)
                            (SETQ J (1- J)))))
                 ;(+ L (RANDOM (+ (- R L) 1)))
                 (+ L (TRUNCATE (- R L) 2))     ;USE THIS UNTIL HAVE RANDOM FUNCTION
))))


(DEFUN SORT-GROUPED-ARRAY (ARRAY GROUP-SIZE SORT-LESSP-PREDICATE)
  "Sort ARRAY, grouping elements into records of size GROUP-SIZE.
The first GROUP-SIZE elements are the first record, etc.
The first elements of two records are compared using the predicate,
and entire records are reshuffled."
  (PROG (SORT-ARRAY-TEMP-V)
        (SETQ SORT-ARRAY-TEMP-V (MAKE-ARRAY GROUP-SIZE))
        (SORT-GROUPED-ARRAY-QUICK ARRAY 0 (- (ARRAY-ACTIVE-LENGTH ARRAY) GROUP-SIZE) GROUP-SIZE)))

(DEFUN SORT-GROUPED-ARRAY-QUICK (A L R GS)
       (COND ((> L (- R (* GS SORT-QS-BREAKEVEN)))      ;SEE IF SHOULD DO AN INSERTION SORT
              (DO ((I (+ L GS) (+ I GS)))               ;THIS CLAUSE ALSO APPLIES WHEN L>R
                  ((> I R))
                (DO C 0 (1+ C) (= C GS) ;COPY GUY OUT
                    (AS-1 (AR-1 A (+ I C))
                          SORT-ARRAY-TEMP-V
                          C))
                (DO ((J (- I GS) (- J GS))
                     (X (AR-1 A I)))
                    ((OR (< J L) (NOT (FUNCALL SORT-LESSP-PREDICATE X (AR-1 A J))))
                     (DO C 0 (1+ C) (= C GS)            ;ON EXIT, STICK THAT ENTRY
                         (AS-1 (AR-1 SORT-ARRAY-TEMP-V C)  ;BACK IN
                               A
                               (+ J GS C))))
                    (DO C 0 (1+ C) (= C GS)
                        (AS-1 (AR-1 A (+ C J)) A (+ J GS C))))))
             (T ((LAMBDA (N)                            ;RANDOMLY CHOSEN POINT BETWEEN L AND R
                         ((LAMBDA (M)                   ;BREAK-POINT BETWEEN LOW AND HIGH
                                  (SORT-GROUPED-ARRAY-QUICK A L (- M GS) GS)
                                                        ;SORT THE LOW ELEMENTS
                                  (SORT-GROUPED-ARRAY-QUICK A (+ M GS) R GS))
                                                        ;SORT THE HIGH ELEMENTS
                          (DO ((K (PROG1 (AR-1 A N)     ;K WILL BE M'TH ELEMENT
                                         (DO C 0 (1+ C) (= C GS)
                                             (AS-1 (AR-1 A (+ N C))   ;SAVE N IN TEMP
                                                   SORT-ARRAY-TEMP-V
                                                   C)
                                             (AS-1 (AR-1 A (+ L C)) A (+ N C))))) ;PUT
                                                                ;L WHERE N WAS
                               (I L)                    ;A[...I-1] < K
                               (J R))                   ;K < A[J+1...]
                              (NIL)
                           DECRJ                        ;DECREASE J UNTIL K NOT LT A[J]
                            (COND ((= J I)
                                   (DO C 0 (1+ C) (= C GS)
                                       (AS-1 (AR-1 SORT-ARRAY-TEMP-V C)
                                             A
                                             (+ I C)))
                                   (RETURN I))
                                  ((FUNCALL SORT-LESSP-PREDICATE K (AR-1 A J))
                                   (SETQ J (- J GS))
                                   (GO DECRJ)))
                            (DO C 0 (1+ C) (= C GS)
                                (AS-1 (AR-1 A (+ J C)) A (+ I C)))
                            (SETQ I (+ I GS))
                           INCRI                        ;INCREASE I UNTIL K NOT GT A[I]
                            (COND ((= I J)
                                   (DO C 0 (1+ C) (= C GS)
                                       (AS-1 (AR-1 SORT-ARRAY-TEMP-V C)
                                             A
                                             (+ J C)))
                                   (RETURN J))
                                  ((FUNCALL SORT-LESSP-PREDICATE (AR-1 A I) K)
                                   (SETQ I (+ I GS))
                                   (GO INCRI)))
                            (DO C 0 (1+ C) (= C GS)
                                (AS-1 (AR-1 A (+ I C)) A (+ J C)))
                            (SETQ J (- J GS)))))
                 ;(+ L (RANDOM (+ (- R L) 1)))
                 (+ L (* GS (FLOOR (FLOOR (- R L) 2) GS)));USE THIS UNTIL HAVE RANDOM FUNCTION
                                                ;MAKE SURE RESULT IS A MULTIPLE OF GS
))))

;SORT-LESSP-PREDICATE HERE MUST BE A FUNCTION OF FOUR ARGS,
; <ARRAY1> <IDX1> <ARRAY2> <IDX2>.
(DEFUN SORT-GROUPED-ARRAY-GROUP-KEY (ARRAY GROUP-SIZE SORT-LESSP-PREDICATE)
  "Sort ARRAY, grouping elements into records of size GROUP-SIZE.
The first GROUP-SIZE elements are the first record, etc.
The predicate can compare entire records, since it is passed
four arguments, two array//index pairs each pointing at one records.
Ultimately, entire records are reshuffled."
  (PROG (SORT-ARRAY-TEMP-V)
        (SETQ SORT-ARRAY-TEMP-V (MAKE-ARRAY GROUP-SIZE))
        (SORT-GROUPED-ARRAY-GROUP-KEY-QUICK ARRAY 0 (- (ARRAY-ACTIVE-LENGTH ARRAY) GROUP-SIZE) GROUP-SIZE)
        ;(RETURN-STORAGE (PROG1 SORT-ARRAY-TEMP-V (SETQ SORT-ARRAY-TEMP-V NIL)))
        (RETURN ARRAY)))

(DEFUN SORT-GROUPED-ARRAY-GROUP-KEY-QUICK (ARRAY L R GROUP-SIZE)
       (COND ((> L (- R (* GROUP-SIZE SORT-QS-BREAKEVEN))) ;SEE IF SHOULD DO AN INSERTION SORT
              (DO ((I (+ L GROUP-SIZE) (+ I GROUP-SIZE)))  ;THIS CLAUSE ALSO APPLIES WHEN L>R
                  ((> I R))
                (DO C 0 (1+ C) (= C GROUP-SIZE) ;COPY GUY OUT
                    (AS-1 (AR-1 ARRAY (+ I C))
                          SORT-ARRAY-TEMP-V
                          C))
                (DO ((J (- I GROUP-SIZE) (- J GROUP-SIZE)))
                    ((OR (< J L) (NOT (FUNCALL SORT-LESSP-PREDICATE
                                               SORT-ARRAY-TEMP-V 0 ARRAY J)))
                     (DO C 0 (1+ C) (= C GROUP-SIZE)    ;On exit, stick that entry back in.
                         (AS-1 (AR-1 SORT-ARRAY-TEMP-V C)
                               ARRAY
                               (+ J GROUP-SIZE C))))
                    (DO C 0 (1+ C) (= C GROUP-SIZE)
                        (AS-1 (AR-1 ARRAY (+ C J)) ARRAY (+ J GROUP-SIZE C))))))
             ;; Divide the array into two halves, by exchanging elements, and sort them.
             (T (LET ((N (+ L (* GROUP-SIZE (TRUNCATE (TRUNCATE (- R L) 2) GROUP-SIZE)))))
                  ;;N is a randomly chosen point between L and R (actually not random)
                  ;;Make sure result is a multiple of group-size
                  (LET ((M
                          (DO ((K ;K WILL BE M'TH ELEMENT - K NOT USED IN THIS VERSION OF CODE
                                  ; INSTEAD USE ARRAY SORT-ARRAY-TEMP-V, STARTING AT ELEMENT 0
                                 (DO C 0 (1+ C) (= C GROUP-SIZE)
                                     (AS-1 (AR-1 ARRAY (+ N C)) ;SAVE N IN TEMP
                                           SORT-ARRAY-TEMP-V
                                           C)
                                     (AS-1 (AR-1 ARRAY (+ L C)) ARRAY (+ N C)))) ;PUT L WHERE N WAS
                               (I L)                    ;ARRAY[...I-1] < K
                               (J R))                   ;K < ARRAY[J+1...]
                              (NIL)
                            ;; Prevent warning about K.
                            (PROGN K)
                           DECRJ                        ;DECREASE J UNTIL K NOT LT ARRAY[J]
                            (COND ((= J I)
                                   (DO C 0 (1+ C) (= C GROUP-SIZE)
                                       (AS-1 (AR-1 SORT-ARRAY-TEMP-V C)
                                             ARRAY
                                             (+ I C)))
                                   (RETURN I))
                                  ((FUNCALL SORT-LESSP-PREDICATE SORT-ARRAY-TEMP-V 0 ARRAY J)
                                   (SETQ J (- J GROUP-SIZE))
                                   (GO DECRJ)))
                            (DO C 0 (1+ C) (= C GROUP-SIZE)
                                (AS-1 (AR-1 ARRAY (+ J C)) ARRAY (+ I C)))
                            (SETQ I (+ I GROUP-SIZE))
                           INCRI                        ;INCREASE I UNTIL K NOT GT ARRAY[I]
                            (COND ((= I J)
                                   (DO C 0 (1+ C) (= C GROUP-SIZE)
                                       (AS-1 (AR-1 SORT-ARRAY-TEMP-V C)
                                             ARRAY
                                             (+ J C)))
                                   (RETURN J))
                                  ((FUNCALL SORT-LESSP-PREDICATE ARRAY I SORT-ARRAY-TEMP-V 0)
                                   (SETQ I (+ I GROUP-SIZE))
                                   (GO INCRI)))
                            (DO C 0 (1+ C) (= C GROUP-SIZE)
                                (AS-1 (AR-1 ARRAY (+ I C)) ARRAY (+ J C)))
                            (SETQ J (- J GROUP-SIZE)))))
                    ;; M is break-point between low and high
                    ;; Sort the low elements
                    (SORT-GROUPED-ARRAY-GROUP-KEY-QUICK
                      ARRAY L (- M GROUP-SIZE) GROUP-SIZE)
                    ;; Sort the high elements.
                    (SORT-GROUPED-ARRAY-GROUP-KEY-QUICK
                      ARRAY (+ M GROUP-SIZE) R GROUP-SIZE))))))
