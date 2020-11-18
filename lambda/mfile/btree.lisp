;;; -*- Mode: Lisp; Base: 8; Package: moby-file-system; Readtable: ZL -*-

;experimental BTREE hacker


;A BTREE is a (hopefully) efficient associative mechanism.
;  On a retreival operation, the input is a KEY to be found.  They
;key may be a number or a string.  Any node of the
;BTREE to be referenced will serve as a starting point for the retreival.
;The reply is a single associated Q.

;  To help keep terminology straight, data obtained from a BTREE structure
;is prefixed by B- in the below discussion.

;  A BTREE consists of layers of nodes.  Each node is capable of containing a moderate
;number of node elements, typically 10. to 100.  The bottom layer contains FINAL-ASSOCIATIONS,
;while the upper layers, (if required) are DIVISORs containing SUBNODE-POINTERs.
;(We require all FINAL-ASSOCIATIONs to be on the bottom layer since otherwise
;excessive complications are introduced in node balancing.  Also the range mapped by
;the bottom layer becomes discontinuous if FINAL-ASSOCIATIONs are stored in higher layers).
;The topmost BTREE layer is always a single node, called TOP-NODE.  If the amount
;of information stored in the BTREE is quite small, TOP-NODE can be an FINAL-ASSOCIATION
;node, in which case the entire scheme degenerates into a sorted table within TOP-NODE.
;Once the information to be stored fails to fit in a single node, TOP-NODE will always
;be a DIVISOR.
;  Each layer, taken as a whole (and the BTREE itself), "MAPs" (or contains information
;relavant to) the same total KEY range.   The nodes within a particular layer
;are sorted monotonically, left to right ("least" to "greatest"),
;and contain pointers to their left and right neighboring BTREE nodes.  The node elements within
;a node are also sorted left to right.  Each node MAPs a continuous interval of KEYs.
;A node specifies its mapped range "inclusively", ie, N+1 keys specify N divisions.
;(Making them inclusive allows realization of the need to step up the tree
;if the search has started at a subnode but the key being searched for is out of range
;of that subnode.)  However, the "odd" (N+1st) key is stored in a different way from the
;others (in KEY-OF-RIGHT-MARGIN).  The range of a node is considered to
;be a half-open interval, that is, the least key is within the range, but the greatest key
;(called KEY-OF-RIGHT-MARGIN) is just outside the range.  Since all layers are continuous,
;KEY-OF-RIGHT-MARGIN of a node is equal to the first element of its right layer-level neighbor
;(except at the right edge of the BTREE when KEY-OF-RIGHT-MARGIN is NIL).
;  When an entry is to be added to the BTREE, the first step is to locate the lowest
;"containing" DIVISOR node, called the DOMINATING node (excepting the simple case
;that TOP-NODE is a FINAL-ASSOCIATION node).  No node in the BTREE
;higher than the DOMINATING node will need to be modified or examined unless the
;DOMINATING node overflows.
;  Next, a ROOM-TEST is made to insure that sufficient room exists in all nodes in which
;it will be required.  If not, SPLITs or SHUFFLEs are performed as necessary (note that during
;this process the logical content of the BTREE has not as yet been changed).
;Finally, the desired new entry is made, which simply involves inserting it in a
;FINAL-ASSOCIATION node which, it has been determined, already has sufficient room.
;  During the ROOM-TEST, it may initially be the case that the FINAL-ASSOCIATION
;node which MAPs the given key is full.  There are two methods of dealing with the
;situation, called SPLITs and SHUFFLEs.
;  A SHUFFLE may be used where either the LEFT or RIGHT neighboring BTREE node
;to the overflowing node has sufficient room and also shares a common parent node
;with the overflowing node.  Data is simply moved over, and the dividing pointer
;in the common parent node is adjusted.  (If there is no common parent node,
;the hair required is too much to be worth it.  Also note that if the layer being shuffled
;is a DIVISOR layer, the parent pointers of the data shuffled must be adjusted).
;  Otherwise a SPLIT is necessary, involving dividing the data of the overflowing node into
;two nodes and adding a new dividing pointer to the parent node.  The condition that
;this will cause the parent node to overflow is tested and dealt with before
;any other modifications are undertaken, (which could involve a SHUFFLE or SPLIT at that
;level, etc).  The left-right linkages must also be adjusted appropriately.
;  If in this process TOP-NODE overflows, a TOP-SPLIT occurs which generates a
;new layer of nodes.  (Other SPLITs make more nodes in given layer, not more layers).

;  Evidently, each node is always at least half full since that is the case immediately
;after a SPLIT. (Exceptions concern TOP-NODE, either if it is a FINAL-ASSOCIATION node
;or immediately after a TOP-SPLIT).

;  BTREE items can be deleted as desired.  In doing this, we preserve the at-least-half-full
;property of BTREE nodes.   So doing not only guarentees a minimal level of storage
;efficiency, but also simplifies the code that would otherwise have to exist
;to deal with NULL BTREE nodes, if they were permitted to arise.
;  If, as a result of a deletion, a node becomes less that half full, rebalancing action
;is initiated.  The first possibility is to SHUFFLE data from the LEFT or RIGHT nodes
;which share a common parent.  (At least one of them must share a parent.)  If such a
;node is more than half full, the situation can be relieved by shuffling over data,
;leaving each at least half full.  If such a node is exactly half-full, a REVERSE-SPLIT
;can occur, concentrating the data in a single node.  This involves deletion of a
;divisor pointer, which node may then need to be rebalanced, etc.  Note that the
;process proceeds by steps each of which represents a valid state for the entire BTREE
;(excepting the half-full property).  If a reverse split happen at the level immediately
;below TOP-NODE which would reduce TOP-NODE to a single item, then a REVERSE-TOP-SPLIT occurs.
;This destroys a BTREE layer.  Note if a layer were to collapse so as to be only one node
;wide, this would insure a rebalance at the next higher level since that level would only
;have a single pointer which would make it less than half full.

;Deleting the leftmost or rightmost element of a node:
; If the leftmost element of a node is deleted, the range mapped by that node
;is reduced.  To maintain the continuity of the layer involved, the right-range of
;the left-adjacent node must be adjusted (recall that for nodes not at the right
;edge of the BTREE, the right-range is equal to the leftmost entry of the right
;neighbor).  Furthermore, the divisor pointer in the parent needs to be adjusted.
;If that pointer is the leftmost element in its divisor node, the processs must
;be continued, etc.
; Deleting the rightmost element in a node does not cause special problems.

;Containing invervals:
;  If a BTREE associates numeric keys, data in the BTREE can be associated not only with a single key,
;but also with a RANGE of them.  These RANGEs are stored in the
;BTREE in non-overlapping form.  B-RANGE intervals can only be associated
;with FINAL-ASSOCIATION type BTREE node elements, not with DIVISORs.
;  B-RANGEs are "relative", ie, if the Nth item of the B-RANGE is referenced,
;the association is the FINAL-ASSOCIATION of the B-RANGE plus N.  B-RANGEs are
;also stored as half-open intervals, the B-RANGE-LIMIT being the highest element of the
;range, plus one.
;  The lowest member of the B-RANGE is stored as the B-KEY in the normal BTREE
;fashion, and B-RANGE-LIMIT is stored as subsidary data under this key.
;No other entry can "break" the B-RANGE; that is, next key in the BTREE node
;must be at least B-RANGE-LIMIT.

;--no now it splits it.. --   However, under a B-RANGE entry can be stored
;an EXCEPTION pointer.  This points to a separate BTREE node, which can list
;EXCEPTIONs to the range, as appropriate.

;Storing intervals
;  Complexities can be involved if the RANGE being stored LAPs a preexisting RANGE
;within the system.  (Two intervals are said to LAP if neither is contained within
;the other, but they have a non-null intersection).  Either stored key-ranges or
;mapped ranges can cause problems.  The general approach to avoiding the problem
;is to split the store into one or more segments so as to avoid any LAPping.
;  A few cases are distinguished either because they are simple,
;or because of necessity.  In particular, if the edge of the BTREE is involved,
;it must be extended rather than attempting to segment the RANGE.
;  A STORE-RANGE-TEST operation is performed to determine how much of the range
;can be stored without creating any LAPping segments.  (One optimization
;made during the STORE-RANGE-TEST is that if a previous element is being completely
;overstored, it is deleted rather than allowing it to cause segmentation.)
;  Then, if any of the orginal range remains to be stored, it
;is stored as a separate segment.

;The format of the "array-leader part" of a distributed BTREE node is always the same.
;Notable is the KEY-OF-RIGHT-MARGIN, which specifies the key just out of range of
;this node to the right or NIL if on RIGHT-MARGIN.

;The format of the "array part" of a distributed BTREE node depends on whether the
;node is a FINAL-ASSOCIATION node (1) or DIVISOR (2).

;DIVISORs have alternating KEYs and SUBNODE-POINTERs.  The comparison used
;is key-<, thus, the leftmost key in each node is in the range mapped by the node.
;However, the rightmost key, contained in KEY-OF-RIGHT-MARGIN is just outside
;the high end of the range of the node.

;FINAL-ASSOCIATIONs have alternating KEYs and FINAL-ASSOCIATIONs.  However, if the
;data type of a Q in the KEY position is DTP-CHARACTER (randomly), it is really
;a range delta specifier for the previous key.
;  the paired Q used to be an exception pointer but now is unused.
;    Following it is the associated EXCEPTION pointer.

;entering new data which extends the range of the BTREE.
;  If an attempt is made to store an item outside the current MAPPED range of the
;entire BTREE, the process will ascend to
;TOP-NODE, when situation must be recognized and handled.  Then the limiting
;entries must be modified down the entire edge of the tree.  To avoid embarresment,
;first assure there is room in the bottommost node where a new entry must be made.

;entering ranges in the BTREE.
;  In most cases, a range type BTREE entry can be made as a single element in the BTREE.
;Sometimes, however, this may not be true (without excessive hair, anyway).
;Generally, the range structure of existing entries is preserved unless it is easy
;to see that an entire range is being overwritten.  (This may mean spitting the
;entry in two or more pieces).  If a subrange is being stored of a previous range,
;the previous range must be split. it is stored as an exception.

;numeric key BTREEs versus string-key BTREEs.
;  could use only one set of routines for both.  However, it leads to cleaner
;  code to have top level routines duplicated (routines with * below).  Most of the
;  hair can be shared anyway.  The only below-top-level routine which has to test
;  is btree-insert-item.  (one reason for the duplication is ranges only apply
;  to the numeric-key case, but mess up the code for the string case quite a bit.)

;subroutine hierarchy:  * indicates has key-< type ops.
;  btree-lookup *
;  btree-delete *
;    btree-find *
;    btree-store *
;  btree-delete-element-from-node ~*
;    btree-adjust-divisor ~*
;    btree-rebalance ~*
;     btree-shuffle-left ~*
;     btree-shuffle-right ~*
;     btree-reverse-split ~*
;      btree-find-node-idx ~*
;      btree-change-parents ~*
;      btree-delete-element-from-node ~*
;      btree-reverse-top-split ~*
;  btree-store *
;    btree-make-room ~*
;     btree-shuffle-left ~*
;       btree-find-node-idx ~*
;       btree-change-parents
;     btree-shuffle-right ~*
;     btree-split-node ~*
;       btree-assure-room ~*
;       btree-insert-item*    ---
;       btree-change-parents ~*
;    btree-assure-room-left-edge ~*
;       btree-shuffle-right ~*
;       btree-split-node ~*
;    btree-store-left-edge ~*
;      btree-make-room ~* but gets to btree-insert-item *
;  btree-store-range *  -anything -range is numeric only anyway.
;    btree-find-segment *
;    btree-store-range-segment *
;     btree-make-room ~* but gets to btree-insert-item * (see above)
;    btree-assure-room-left-edge (see above)
;    btree-store-left-edge-range
;     btree-make-room ~* but gets to btree-insert-item * (see above)
;    btree-delete-range-segment *

(defconst *btree-check-flag* nil)

(defstruct (btree (:type :array-leader)
                  (:conc-name "BTREE-")
                  :named)
  (fill-pointer 0)
  lock
  ((type-codes)
   (type-number (byte 3 0))     ;1 final association, 2 divisor.
   (key-code    (byte 3 3))     ;1 integer, 2 %pointer, 3 string, 4 list pointer.
   (res-code    (byte 3 6)))    ;1 integer, 2 %pointer, 3 string, 4 list pointer.
                                ;5 integer if numeric, else lisp pointer.
  key-of-right-margin   ;first key OUTSIDE range of this node to the right or NIL if
                        ; on right margin.
  top-node              ;topmost node of entire BTREE.  Note if this is an exception tree,
                        ;  this points to the topmost node  of the entire BTREE.
  parent-node           ;NIL if this is TOP-NODE.
  left-node             ;adjacent at same level in BTREE
  right-node
  )


(defselect ((:property btree named-structure-invoke) ignore)
  (:print-self (self *standard-output* ignore &optional ignore)
    (si:printing-random-object (self *standard-output*)
       (format t "BTREE ~O, D~D, ~D//~D, ~A, [~s,~s]"
               (%POINTER (BTREE-TOP-NODE SELF))
               (prog ( (d 0) n)
                     (setq n self)
                  l  (cond ((null (setq n (btree-parent-node n)))
                            (return d)))
                     (setq d (1+ d))
                     (go l))
               (btree-fill-pointer self)
               (array-total-size self)
               (SELECT (BTREE-TYPE-NUMBER SELF)
                 (1 "FINAL-ASSOCIATION")
                 (2 "DIVISOR")
                 (OTHERWISE "BAD TYPE NUMBER"))
               (AREF SELF 0)
               (BTREE-KEY-OF-RIGHT-MARGIN SELF)))))


;******* begin random ramblings ********

; is stored has four kinds of data:
; (these are decoded in the CDR-CODE bits of the ART-Q array.  Because of this, it does not
; win to store into a BTREE node with SETF, for now you must use
; (%p-dpb %%q-typed-pointer (locf xx))).

;0  B-KEY data.  Divides array data into BTREE elements.
;(first 3)  range delta.   Preceeding key really applies to a range, this many (KEY itself
;       is the bottom of the range.)
;(second 3) subrange pointer.  Obtain trial "final association" from 2 item in this group.
;       This 3 data is a pointer to a BTREE structure which can map exceptions; if the
;       data searched for is not associated in the subrange, then return the TRIAL guy.
;1  lower btree node pointers.  Follow this if searched for data was LESS  or EQUAL
;       than preceeding key.  If present, this signals that this BTREE element is a
;       SUBNODE-POINTER.  Otherwise, a FINAL-ASSOCIATION, below, must be present.
;2  FINAL-ASSOCIATION.  If searched for data is EQUAL to (or within range of) the preceeding
;       B-KEY, this is it!

;For efficiency, we impose a few restrictions on the ordering of the various frobs.
;  The B-KEY comes first.  other entries until next key apply to this key.
;  range delta (3), if present, must come next.
;  subrange exception pointer (second 3), if present, must come next.
;  final association (2), if present, must come next.
;  last comes subtree pointers (1)

;btree root:
; identification
; tick data
; parent root
; number levels
; logical division

;Btree node:

;is an ART-Q array with an array leader.  May sometimes be consed by a
; special frob which allocates strings, etc, on same page.  Data
; structure is standard LISPM, tho.

;"standard string" form.
;  file names have various components (name type version), etc.  In some sense
; we want to have a separate BTREE to deal with each level.  this would be
; quite inefficient to.  So we define a single string, constructed from the
; components in a standard way, which is the key under which all info for the
; file is stored.  Glitches in said string separate the components.

; "repeating" components.  "directory" can appear one or more times, and is supposed
; to be decoded relative to a "current directory".  Does each file have a copy of
; the directory tree string? (clearly undesirable).


;moby BTREE nodes are "self contained", meaning that having just a pointer to
;a NODE you can crawl around and find other nodes, etc, as necessary.  No auxilliary
;root pointers, data structures or stacks are necessary.

;moby BTREE nodes are "plastic" in a certain sense.  Each node has its own size,
;  etc.  We do not insist that the tree necessarily be organized into nodes of a certain
;  size (referred to as "degree" in KNUTH), or even a certain size as a function of
;  level.  Instead, each node specifies for itself its size, whether it is a leaf node, etc.

;*************************************

(defun key-< (code a b)
  (selectq code
    (1 (< a b))
    (2 (compiler:%pointer-lessp a b))
    (3 (string-lessp a b))
    (4 (< (sxhash a) (sxhash b)))
    (otherwise (ferror nil ""))))

(defun key-= (code a b)
  (selectq code
    (1 (= a b))
    (2 (eq a b))
    (3 (string-equal a b))
    (4 (equal a b))
    (otherwise (ferror nil ""))))

(defun key-<= (code a b)
  (selectq code
    (1 (<= a b))
    (2 (not (compiler:%pointer-greaterp a b)))
    (3 (not (string-greaterp a b)))
    (4 (<= (sxhash a) (sxhash b)))
    (otherwise (ferror nil ""))))

(defun key-- (code a b)
  (selectq code
    (1 (- a b))
    (2 (%pointer-difference a b))
    (3 (ferror nil ""))
    (4 (ferror nil ""))
    (otherwise (ferror nil ""))))

(defun key-+ (code a b)
  (selectq code
    (1 (+ a b))
    (2 (%pointer-plus a b))
    (3 (ferror nil ""))
    (4 (ferror nil ""))
    (otherwise (ferror nil ""))))

;these used to compare values returnned by key--, etc.
; they have prefixed names just to keep things straight.
(defmacro key-relative-< (a b)
  `(< ,a ,b))

(defmacro key-relative-<= (a b)
  `(<= ,a ,b))

(defmacro key-relative-= (a b)
  `(= ,a ,b))

(defmacro key-relative-- (a b)
  `(- ,a ,b))

(defun res-+ (code delta base)
  (selectq code
    (1 (+ delta base))
    (2 (%pointer-plus delta base))
    (4 base)    ;ignore delta
    (5 (if (numberp base)
           (+ delta base)  ;page-number, apply delta.
         base))         ;lisp pointer, return that.
    (otherwise (ferror nil ""))))

(defun btree-minimum-key (node)
  ;return the lowest key in the entire tree represented by node
  (prog ()
     l  (cond ((null (btree-parent-node node))
               (return (aref node 0)))
              (t (setq node (btree-parent-node node))
                 (go l)))))

;enter here to get association for ITEM in BTREE of which NODE is a member.  Node can be
; ANY node of the tree, not necessarily the top or not necessarily the node which "covers"
; the range of ITEM.
(defun btree-lookup (node key)
  (prog top (idx idx-lim b-key b-subnode b-range-limit)
        (setq idx 0 idx-lim (btree-fill-pointer node))
        (cond ((or (zerop idx-lim)
                   (key-< (btree-key-code node) key (aref node 0))      ;left-most b-key
                   (and (btree-key-of-right-margin node)
                        (not (key-< (btree-key-code node) key (btree-key-of-right-margin node)))))
               (go up))                         ;key not within range of this node.
              ((= (btree-type-number node) 1)
               (go fa-1))
              ((not (= (btree-type-number node) 2))
               (ferror nil "bad type number for node" node)))
  dv-1  (setq b-key (aref node idx))    ;divisor node
        (cond ((key-< (btree-key-code node) key b-key)
               (return (btree-lookup b-subnode key))))
        (setq b-subnode (aref node (1+ idx))
              idx (+ idx 2))
        (cond ((= idx idx-lim)   ;if reach end, follow rightmost subnode pointer since
               (return (btree-lookup b-subnode key))))  ;its known to be within right margin.
        (go dv-1)
  fa-1  (setq b-key (aref node idx))    ;final-association node
        (cond ((key-< (btree-key-code node) key b-key)
               (return nil)))     ;not found and won't since keys sorted.
        (setq b-range-limit
              (if (and (< (+ idx 3) idx-lim)
                       (= (%data-type (aref node (+ idx 2))) dtp-character))
                  (%make-pointer dtp-fix (aref node (+ idx 2)))
                nil))
        (cond ((or (key-= (btree-key-code node) key b-key)
                   (and b-range-limit
                        (key-relative-< (key-- (btree-key-code node) key b-key)
                                        b-range-limit)))
               (cond ((null b-range-limit)
                      (return (aref node (1+ idx))))
;                    ((aref node (+ idx 3))     ;b-exception
;                                               ;return exception if any, else relative result
;                     (return (cond ((btree-lookup (aref node (+ idx 3)) key))
;                                   (t (res-+ (key-- (btree-key-code node) key b-key)
;                                             (aref node (+ idx 1)))))))
;                                               ;no exception, return relative result.
                     (t (return (res-+ (btree-res-code node)
                                       (key-- (btree-key-code node) key b-key)
                                       (aref node (+ idx 1))))))))
        (setq idx (+ idx (if b-range-limit 4 2)))
        (cond ((< idx idx-lim)
               (go fa-1))
              (t (return nil)))
     up (cond ((null (btree-parent-node node))
               (return nil))                    ;range not covered by TOP-NODE node.
              (t (return (btree-lookup (btree-parent-node node) key))))
         ))

;modified version of BTREE-LOOKUP.  Returns:
;   nil         if out of BTREE mapped range.
;   node, nil   if key not found (NODE being final-association node where it would go).
;   node, idx   if found.
(defun btree-find (node key)
  (prog top (idx idx-lim b-key b-subnode b-range-limit)
  top   (setq idx 0 idx-lim (btree-fill-pointer node))
        (cond ((or (zerop idx-lim)
                   (key-< (btree-key-code node) key (aref node 0))      ;left-most b-key
                   (and (btree-key-of-right-margin node)
                        (not (key-< (btree-key-code node) key (btree-key-of-right-margin node)))))
               (go up))                         ;key not within range of this node.
              ((= (btree-type-number node) 1)
               (go fa-1))
              ((not (= (btree-type-number node) 2))
               (ferror nil "bad type number for node" node)))
  dv-1  (setq b-key (aref node idx))    ;divisor node
        (cond ((key-< (btree-key-code node) key b-key)
               (setq node b-subnode)
               (go top)))
        (setq b-subnode (aref node (1+ idx))
              idx (+ idx 2))
        (cond ((= idx idx-lim)   ;if reach end, follow rightmost subnode pointer since
               (setq node b-subnode)
               (go top)))  ;its known to be within right margin.
        (go dv-1)
  fa-1  (setq b-key (aref node idx))    ;final-association node
        (cond ((key-< (btree-key-code node) key b-key)
               (return (values node nil))))       ;not found and won't since keys sorted.
        (setq b-range-limit
              (if (and (< (+ idx 3) idx-lim)
                       (= (%data-type (aref node (+ idx 2))) dtp-character))
                  (%make-pointer dtp-fix (aref node (+ idx 2)))
                nil))
        (cond ((or (key-= (btree-key-code node) key b-key)
                   (and b-range-limit
                        (key-relative-< (key-- (btree-key-code node) key b-key)
                                        b-range-limit)))
               (return (values node idx))))
        (setq idx (+ idx (if b-range-limit 4 2)))
        (cond ((< idx idx-lim)
               (go fa-1))
              (t (return (values node nil))))
     up (cond ((null (btree-parent-node node))
               (return nil))                    ;range not covered by TOP-NODE node.
              (t (setq node (btree-parent-node node))
                 (go top)))
         ))

;Lookup KEY, then determine what is next step in storing [key,key-range-limit] without too much
; "complexity".  (actually, the interval is open but using a close paren would unbalance things!)
; We restrict range so as to avoid need to change any DIVISOR's range.
; Previously stored ranges are split, if necessary, to avoid partial overlaps.
;Returns:
; first, second values are segment being returned for "action".
; third value says what action is appropriate:
;    T says store this much.  Necessary deletes for storing yea much have been done.
;  DELETE says this segment is overlapped.  Delete it, and try again.
;      (can either be completely overlapped, or aligned at one edge of B-ENTRY, in which
;      case deletion can be effected by suitable storing on top of B-ENTRY edge, restricting
;      it.)
;  -- not used -- LEFT-EDGE says use left edge store for this.
;  NODE says limited by NODE mapping range.
;  If desired range is contained within previously stored range, not touching either edge,
;     This function will split it, and try again.
(defun btree-find-segment (node key key-range-limit)
  (prog top (idx idx-lim b-key b-key-range-top new-node)
   top  (multiple-value (new-node idx) (btree-find node key))
        (cond ((null new-node)
               ;completely outside mapped range.
               (cond ((key-< (btree-key-code node) key (aref node 0))
                      (return
                        (values key
                                (cond ((key-< (btree-key-code node)
                                              key-range-limit
                                              (btree-minimum-key node))
                                       key-range-limit)
                                      (t (btree-minimum-key node)))
                                t)))
                     (t (return (values key key-range-limit t))))))     ;on high side, store whole thing.
        (setq node new-node)
        (setq idx-lim (btree-fill-pointer node))
        (cond ((null idx)
               ;key not found, NODE is FINAL-ASSOCIATION where it would be stored.
               ;look to see if any B-RANGEs getting overlapped, if so, signal them for deletion.
               (prog ()
                     (setq idx 0)
                l0   (cond ((not (< idx idx-lim))
                            (return nil)))      ;from PROG.  No interference
                     (setq b-key (aref node idx))
                     (cond ((key-<= (btree-key-code node) key-range-limit b-key)
                            (return nil)))      ;also no interference.
                     (setq b-key-range-top
                           (if (and (< (+ idx 3) idx-lim)
                                    (= (%data-type (aref node (+ idx 2))) dtp-character))
                               (key-+ (btree-key-code node)
                                      b-key
                                      (%make-pointer dtp-fix (aref node (+ idx 2))))
                             nil))
                     (cond ((null b-key-range-top)
                            (cond ((and (key-<= (btree-key-code node) key b-key)
                                        (key-< (btree-key-code node) b-key key-range-limit))
                                   (return-from top
                                     (values b-key
                                             (key-+ (btree-key-code node)
                                                    b-key
                                                    1)
                                             'delete)))))  ;single guy getting
                                                                 ; stored over, flush.
                     ;we know b-key will not be within range.  Is stored entry completely
                     ; or partially overlapped?
                           ((and (key-< (btree-key-code node) key b-key)
                                 (key-<= (btree-key-code node) b-key-range-top key-range-limit))
                            (return-from top
                              (values b-key b-key-range-top 'delete))) ;range getting
                                                                ;completely stored over, flush.
                           ((and (key-< (btree-key-code node) b-key key-range-limit)
                                 (key-<= (btree-key-code node) key-range-limit b-key-range-top))
                            ;b-range partially overlaps key-range, delete offending part.
                            (return-from top
                              (values b-key key-range-limit 'delete))))
                     (setq idx (+ idx (if b-key-range-top 4 2)))
                     (go l0))
               ;drop thru on existing b-entry does not overlap, thus, not a factor.
               ;signal store of range limited by min (desired, existing b-entry, right-margin)
               (cond ((or (null (btree-key-of-right-margin node))
                          (key-<= (btree-key-code node)
                                  key-range-limit (btree-key-of-right-margin node)))
                      (return (values key key-range-limit t)))
                     (t (return (values key (btree-key-of-right-margin node) 'node))))))
    ;key found within b-interval.
        (setq b-key (aref node idx))
        (setq b-key-range-top
              (if (and (< (+ idx 3) idx-lim)
                       (= (%data-type (aref node (+ idx 2))) dtp-character))
                  (key-+ (btree-key-code node)
                         b-key (%make-pointer dtp-fix (aref node (+ idx 2))))
                nil))
        ;if storing range touches either edge of b-range, delete the b-range section.
        ;otherwise, split stored range and try again.
        (cond ((key-= (btree-key-code node) key b-key)
               (cond ((or (null b-key-range-top)
                          (key-<= (btree-key-code node) b-key-range-top key-range-limit))
                      (return (values b-key
                                      (cond (b-key-range-top)
                                            (t (key-+ (btree-key-code node) b-key 1)))
                                      'delete)))  ;whole thing overstored.
                     (t (return (values b-key key-range-limit 'delete))))) ;delete lower section
              ((key-= (btree-key-code node) key-range-limit b-key-range-top)
               (return (values key b-key-range-top 'delete)))    ;delete upper section.
              (t (btree-split-stored-range node idx key)
                 (go top))) ;split stored range at key, then try again.
  ))

(defconst *btree-default-node-size* 20)   ;must hold at least 4 entries to work for deletion.
      ;with range entries (4 wds), 20 is minimum that works.
(defconst *btree-max-entry-size* 4)    ;max size of an entry.  All nodes (except possibly TOP-NODE)
      ;must always hold at least 2 entries, (so as to have one in case one gets deleted).
(defvar *btree-node-being-filled* nil)  ;During CHECK-BTREE, dont complain about this one not
      ;having at least 2 entries.

;return NODE and idx of room if successful, otherwise node and NIL to say
; look it up again and try again (data has been split or shuffled, so desired data
; may have moved).   **combine this with btree-assure-room, below**
(defun btree-make-room (node base-idx words-of-room)
  (prog (idx-lim)
        (setq idx-lim (btree-fill-pointer node))
        (cond ((<= (+ idx-lim words-of-room) (array-total-size node))
               (dotimes (c words-of-room)
                 (array-push node nil))
               (do ((t-idx (1- (btree-fill-pointer node)) (1- t-idx))
                    (f-idx (1- idx-lim) (1- f-idx)))
                   ((< f-idx base-idx))
                 (aset (aref node f-idx) node t-idx))
               (return (values node base-idx)))
              ((and (btree-left-node node)
                    (eq (btree-parent-node node) (btree-parent-node (btree-left-node node)))
                    (>= (btree-room (btree-left-node node))
                        (* 2 words-of-room)))  ;* 2 to avoid possible looping when
                          ;it shuffles back and forth between two nodes in an infinite loop
                          ;because the data it wants to insert before is shuffled into
                          ;the other node.
               (btree-shuffle-left (btree-left-node node) node words-of-room)
               (return (values node nil)))
              ((and (btree-right-node node)
                    (eq (btree-parent-node node) (btree-parent-node (btree-right-node node)))
                    (>= (btree-room (btree-right-node node))
                        (* 2 words-of-room)))
               (btree-shuffle-right node (btree-right-node node) words-of-room)
               (return (values node nil)))
              ;doesnt fit, split then tell guy to try again.
              (t
               (btree-split-node node)
               (return (values node nil))))))


(defun btree-insert-item (node association key)
  (prog (idx-lim idx)
        (setq idx-lim (btree-fill-pointer node)
              idx 0)
     l  (cond ((or (not (< idx idx-lim))
                   (key-< (btree-key-code node) key (aref node idx)))  ;can be either.
               (go x)))
        (setq idx (+ idx 2))
        (go l)
     x  (array-push node nil)
        (array-push node nil)
        (do ((f-idx (1- idx-lim) (1- f-idx))
             (t-idx (1- (btree-fill-pointer node)) (1- t-idx)))
            ((< f-idx idx))
          (aset (aref node f-idx) node t-idx))
        (aset key node idx)
        (aset association node (1+ idx))
        (return t)))

;returns NIL if there was room, T if fancy stuff had to be done.
(defun btree-assure-room (node words-of-room &optional inhibit-shuffle)
  (cond ((>= (btree-room node) words-of-room)
         nil)
        ((and (null inhibit-shuffle)
              (btree-left-node node)
              (eq (btree-parent-node node) (btree-parent-node (btree-left-node node)))
              (>= (btree-room (btree-left-node node))
                  (* 2 words-of-room)))
         (btree-shuffle-left (btree-left-node node) node words-of-room)
         t)
        ((and (null inhibit-shuffle)
              (btree-right-node node)
              (eq (btree-parent-node node) (btree-parent-node (btree-right-node node)))
              (>= (btree-room (btree-right-node node))
                  (* 2 words-of-room)))
         (btree-shuffle-right node (btree-right-node node) words-of-room)
         t)
       ;doesnt fit, split then tell guy to try again.
        (t
         (btree-split-node node)
         t)))

(defun btree-shuffle-left (left-node right-node min-words-of-room &optional inhibit-check)
  (prog (parent idx words-to-shuffle)
        (if (and (null inhibit-check) *btree-check-flag*)
            (let ((*btree-node-being-filled* left-node))
              (check-btree left-node)))
        (setq parent (btree-parent-node left-node))
        (cond ((not (eq parent (btree-parent-node  right-node)))
               (ferror "cant shuffle unless common parent."))
              ((not (> (btree-fill-pointer right-node)
                       (* *btree-max-entry-size* 2)))
               (ferror "right not big enuf to shuffle from")))
        (setq idx (btree-find-node-idx parent right-node))   ;find right-node pointer in parent.
        (setq words-to-shuffle
              (max min-words-of-room
                   (min (logand -2 (// (btree-room left-node) 2))
                        (logand -2 (// (btree-fill-pointer right-node) 2))
                        (- (btree-fill-pointer right-node)  ;leave at least 2 entries in RIGHT.
                           (* 2 *btree-max-entry-size*)))))
        (cond ((= (%data-type (aref right-node words-to-shuffle))
                  dtp-character)
               (setq words-to-shuffle (+ words-to-shuffle 2))))
    ;move data from bottom of right to top of left.
        (do ((f-idx 0 (1+ f-idx)))
            ((= f-idx words-to-shuffle))
          (array-push left-node (aref right-node f-idx)))
    ;move remaining data in right guy down.
        (do ((f-idx words-to-shuffle (1+ f-idx))
             (t-idx 0 (1+ t-idx)))
            ((= f-idx (btree-fill-pointer right-node))
    ;store NILs over garbage in right guy.
             (do ((ff-idx (1+ t-idx) (1+ ff-idx)))
                 ((= ff-idx f-idx))
               (aset nil right-node ff-idx))
             (setf (btree-fill-pointer right-node) t-idx))
          (aset (aref right-node f-idx) right-node t-idx))
        (setf (aref parent idx) (aref right-node 0))    ;new division point
        (setf (btree-key-of-right-margin left-node) (aref right-node 0))
        (btree-change-parents left-node)
        (btree-change-parents right-node)
        (if (and (null inhibit-check) *btree-check-flag*)
            (check-btree left-node))
        (return t)))

(defun btree-shuffle-right (left-node right-node min-words-of-room &optional inhibit-check)
   inhibit-check
  (prog (parent idx words-to-shuffle)
        (if (and (null inhibit-check) *btree-check-flag*)
            (let ((*btree-node-being-filled* right-node))
              (check-btree left-node)))
        (setq parent (btree-parent-node left-node))
        (cond ((not (eq parent (btree-parent-node  right-node)))
               (ferror "cant shuffle unless common parent.")))
   ;find right-node pointer in parent.
        (setq idx (btree-find-node-idx parent right-node))   ;find right-node pointer in parent.
        (setq words-to-shuffle
              (max min-words-of-room
                   (min (logand -2 (// (btree-room right-node) 2))
                        (logand -2 (// (btree-fill-pointer left-node) 2))
                        (- (btree-fill-pointer left-node)       ;leave at least 2 entries in LEFT.
                           (* 2 *btree-max-entry-size*)))))
        (cond ((= (%data-type (aref left-node (- (btree-fill-pointer left-node)
                                                 words-to-shuffle)))
                  dtp-character)
               (setq words-to-shuffle (+ words-to-shuffle 2))))
     ;shove right guy up
        (let ((old-right-fill-pointer (btree-fill-pointer right-node))
              (new-left-fill-pointer (- (btree-fill-pointer left-node) words-to-shuffle)))
          (dotimes (c words-to-shuffle)
            (array-push right-node nil))  ;increment fill pointer and avoid garbage even
                                          ; momentarily.
    ;move data in right guy up to make room.
          (do ((t-idx (1- (btree-fill-pointer right-node)) (1- t-idx))
               (f-idx (1- old-right-fill-pointer) (1- f-idx)))
              ((< f-idx 0))
            (aset (aref right-node f-idx) right-node t-idx))
    ;move data from top of left guy to right guy.
          (do ((t-idx 0 (1+ t-idx))
               (f-idx new-left-fill-pointer (1+ f-idx)))
              ((= t-idx words-to-shuffle))
            (aset (aref left-node f-idx) right-node t-idx))
    ;store NILs over garbage at top of left guy.
          (do ((ff-idx new-left-fill-pointer (1+ ff-idx)))
              ((= ff-idx (btree-fill-pointer left-node)))
            (aset nil left-node ff-idx))
          (setf (btree-fill-pointer left-node) new-left-fill-pointer)
          (setf (aref parent idx) (aref right-node 0))  ;new division point
          (setf (btree-key-of-right-margin left-node) (aref right-node 0))
          (btree-change-parents left-node)
          (btree-change-parents right-node)
          (if (and (null inhibit-check) *btree-check-flag*)
              (check-btree left-node))
          (return t))))

(defun btree-split-node (node)
  (prog (idx idx-lim parent)
        (if *btree-check-flag* (check-btree node))
        (setq idx-lim (btree-fill-pointer node))
   ;locate split point
        (setq idx (logand -2 (// idx-lim 2)))
        (cond ((= (%data-type (aref node idx)) dtp-character)
               (setq idx (- idx 2))))
        (cond ((null (setq parent (btree-parent-node node)))
               (go top-split))
              ((not (>= (btree-room parent) 2))
               (btree-assure-room parent 2)  ;this should do something.
               (return nil)))
   ;locate new-node to the "right" of old one.
        (let ((new-node
                (make-btree :make-array  (:length (max (* (- idx-lim idx) 2)
                                                       *btree-default-node-size*))
                            :type-codes (btree-type-codes node))))
          (do ((f-idx idx (1+ f-idx)))
              ((not (< f-idx idx-lim)))
            (array-push new-node (aref node f-idx)))
          (setf (btree-key-of-right-margin new-node) (btree-key-of-right-margin node))
          (setf (btree-top-node new-node) (btree-top-node node))
          (setf (btree-parent-node new-node) parent)
          (setf (btree-left-node new-node) node)
          (setf (btree-right-node new-node) (btree-right-node node))
          (cond ((btree-right-node node)
                 (setf (btree-left-node (btree-right-node node)) new-node)))
          (btree-insert-item parent new-node (aref new-node 0))
          (setf (btree-key-of-right-margin node) (aref new-node 0))
     ;store NILs over garbage in NODE.
          (do ((ff-idx idx (1+ ff-idx)))
              ((not (< ff-idx idx-lim)))
            (aset nil node ff-idx))
          (setf (btree-fill-pointer node) idx)
          (setf (btree-right-node node) new-node)
     ;    (print (list node new-node))
          (btree-change-parents node)
          (btree-change-parents new-node)
          (if *btree-check-flag* (check-btree node))
          (return t))
   top-split    ;create two new nodes in layer below TOP-NODE.  Same node continues to
        ;serve as top-node, which may mean changing its type from FINAL-ASSOCIATION to
        ;DIVISOR.
        (let ((new-left (make-btree :make-array  (:length (max (* idx 2)
                                                               *btree-default-node-size*))
                            :type-codes (btree-type-codes node)))
              (new-right (make-btree :make-array  (:length (max (* (- idx-lim idx) 2)
                                                                *btree-default-node-size*))
                            :type-codes (btree-type-codes node))))
  ;copy data to left.
          (do ((f-idx 0 (1+ f-idx)))
              ((not (< f-idx idx)))
            (array-push new-left (aref node f-idx)))
          (setf (btree-key-of-right-margin new-left) (aref node idx))
          (setf (btree-top-node new-left) (btree-top-node node))
          (setf (btree-parent-node new-left) node)
          (setf (btree-left-node new-left) nil)
          (setf (btree-right-node new-left) new-right)
  ;copy data to right.
          (do ((f-idx idx (1+ f-idx)))
              ((not (< f-idx idx-lim)))
            (array-push new-right (aref node f-idx)))
          (setf (btree-key-of-right-margin new-right) (btree-key-of-right-margin node))
          (setf (btree-top-node new-right) (btree-top-node node))
          (setf (btree-parent-node new-right) node)
          (setf (btree-left-node new-right) new-left)
          (setf (btree-right-node new-right) nil)
          (btree-change-parents new-right)
          (btree-change-parents new-left)
  ;(break "about to top split ~s" node)
  ;reconsititute top node.
          (setf (btree-type-number node) 2)     ;divisor, if it wasnt before.
        ;wipe old data for node.
          (do ((ff-idx 0 (1+ ff-idx)))
              ((not (< ff-idx idx-lim)))
            (aset nil node ff-idx))
          (setf (btree-fill-pointer node) 0)    ;entirely new data for node.
          (array-push node (aref new-left 0))
          (array-push node new-left)
          (array-push node (aref new-right 0))
          (array-push node new-right)
        ; (print node)
        ; (print (list new-left new-right))
          (if *btree-check-flag* (check-btree node))
          (return t))
))



;delete node element at base-idx.  Adjusts fill-pointer.  Initiates rebalancing if necessary.
; If data at index 0 is being deleted, the key-of-right-margin of my left node is adjusted.
;And divisor pointer readjusted.
(defun btree-delete-element-from-node (node base-idx &optional inhibit-check)
  (prog (idx-lim words-to-flush new-idx-lim)
        (setq idx-lim (btree-fill-pointer node)
              words-to-flush (if (and (>= idx-lim (+ base-idx 4))
                                      (= (%data-type (aref node (+ base-idx 2)))
                                         dtp-character))
                                     4
                                   2)
              new-idx-lim (- idx-lim words-to-flush))
        (cond ((and (zerop new-idx-lim)
                    (btree-parent-node node))  ;OK for top to be empty.
               (ferror nil "Illegal delete, would leave empty node")))
        (do ((t-idx base-idx (1+ t-idx))
             (f-idx (+ base-idx words-to-flush) (1+ f-idx)))
            ((not (< f-idx idx-lim)))
          (aset (aref node f-idx) node t-idx))
    ;Store NILs over garbage.
        (do ((ff-idx new-idx-lim (1+ ff-idx)))
            ((= ff-idx idx-lim))
          (aset nil node ff-idx))
        (setf (btree-fill-pointer node) new-idx-lim)
        (cond ((zerop new-idx-lim)
               ;deleting everything from top node, reset all ranges.
               (setf (btree-key-of-right-margin node) nil)
               (return node))
              (t
               (cond ((zerop base-idx)
                      (btree-propagate-deletion node)))
               (cond ((< (btree-fill-pointer node) (// (array-total-size node) 2))
                      (return (btree-rebalance node inhibit-check)))
                     (t (return node)))))))

;Have just hacked entry at index 0 of node.  Adjust left node and/or parent node if necessary.
(defun btree-propagate-deletion (node)
  (cond ((btree-left-node node)
         (setf (btree-key-of-right-margin (btree-left-node node))
               (aref node 0))))
  (cond ((btree-parent-node node)
         (btree-adjust-divisor (btree-parent-node node) node))))

(defun btree-adjust-divisor (parent-node node)
  (let ((idx (btree-find-node-idx parent-node node)))
    (setf (aref parent-node idx) (aref node 0))
    (cond ((zerop idx)
           (cond ((btree-left-node parent-node)
                  (setf (btree-key-of-right-margin (btree-left-node parent-node))
                        (aref node 0))))
           (cond ((btree-parent-node parent-node)
                  (btree-adjust-divisor (btree-parent-node parent-node) parent-node)))))))

(defun btree-rebalance (node &optional inhibit-check)
  (prog (idx-lim left right)
        (setq idx-lim (btree-fill-pointer node)
              left (btree-left-node node)
              right (btree-right-node node))
        (cond ((null (btree-parent-node node))
               (return node))   ;TOP-NODE always in balance.
              ((<= (btree-room node)
                   (// (array-total-size node) 2))
               (return node))           ;in balance.
              ((and left
                    (eq (btree-parent-node node) (btree-parent-node left))
                    (< (btree-room left)
                       (// (array-total-size left) 2)))
               (btree-shuffle-right left node 0 inhibit-check)
               (return node))
              ((and right
                    (eq (btree-parent-node node) (btree-parent-node right))
                    (< (btree-room right)
                       (// (array-total-size right) 2)))
               (btree-shuffle-left node right 0 inhibit-check)
               (return node))
              ((and left
                    (eq (btree-parent-node node) (btree-parent-node left))
                    (>= (btree-room left)       ;should always be =, if at all.
                        (// (array-total-size left) 2)))
               (return (btree-reverse-split left node inhibit-check)))
              ((and right
                    (eq (btree-parent-node node) (btree-parent-node right))
                    (>= (btree-room right)      ;should always be =, if at all.
                        (// (array-total-size right) 2)))
               (return (btree-reverse-split node right inhibit-check)))
              (t (ferror nil "this is impossible")))))

(defun btree-reverse-split (left-node right-node &optional inhibit-check)
  (prog (parent right-idx words-to-shuffle)
        (if (and (null inhibit-check) *btree-check-flag*)
            (let ((*btree-node-being-filled*
                    (if (< (btree-fill-pointer right-node)
                           (btree-fill-pointer left-node))
                        right-node
                      left-node)))
              (check-btree left-node)))
        (setq parent (btree-parent-node left-node))
        (cond ((not (eq parent (btree-parent-node right-node)))
               (ferror nil "reverse split when no common parent")))
    ;flush right node, shuffle all data to left.
        (setq right-idx (btree-find-node-idx parent right-node))
        (setq words-to-shuffle (btree-fill-pointer right-node))
    ;move data from bottom of right to top of left.
        (do ((f-idx 0 (1+ f-idx)))
            ((= f-idx words-to-shuffle))
          (array-push left-node (aref right-node f-idx)))
        (setf (btree-key-of-right-margin left-node) (btree-key-of-right-margin right-node))
        (btree-change-parents left-node)
        (btree-delete-element-from-node parent right-idx t)
        (let ((right-of-right (btree-right-node right-node)))
          (cond (right-of-right
                 (setf (btree-left-node right-of-right) left-node)))
          (setf (btree-right-node left-node) right-of-right))
        (if (and (null inhibit-check) *btree-check-flag*)
            (check-btree left-node))
        (cond ((and (null (btree-left-node left-node))
                    (null (btree-right-node left-node)))
               (return (btree-reverse-top-split left-node)))
              (t
               (return left-node))) ))

;node is the only node at the layer below top.  Flush it, transferring data
; to top node.  Update parent pointers.
(defun btree-reverse-top-split (node)
  (let ((parent (btree-parent-node node)))
    (if (or (null parent)
            (not (null (btree-parent-node parent)))
            (not (= (btree-fill-pointer parent) 2)))
        (ferror nil "lossage")
      (setf (btree-fill-pointer parent) 0)
      (setf (btree-type-number parent) (btree-type-number node))
      (do ((f-idx 0 (+ f-idx 2)))
          ((= f-idx (btree-fill-pointer node)))
        (array-push parent (aref node f-idx))
        (array-push parent (aref node (1+ f-idx)))
        (cond ((= (btree-type-number node) 2)
               (setf (btree-parent-node (aref node (1+ f-idx))) parent))))
      parent)))


(defun btree-delete (node key)
  "Delete association stored with key.  If its part of a range and not at edge of
the range, split the range then try again."
  (prog top (idx idx-lim b-key b-range-limit)
   top  (multiple-value (node idx) (btree-find node key))
        (cond ((or (null node)
                   (null idx))
               (return nil)))
        (setq idx-lim (btree-fill-pointer node))
        ;node is now final association node where it goes.
        (setq b-key (aref node idx))
        (cond ((and (< (+ idx 3) idx-lim)
                    (= (%data-type (aref node (+ idx 2))) dtp-character))
           ;found item is range.
               (setq b-range-limit (%make-pointer dtp-fix (aref node (+ idx 2))))
               (cond ((key-= (btree-key-code node) key b-key)
                      (cond ((key-relative-= b-range-limit 1)
                             (return (btree-delete-element-from-node node idx)))
                            (t
                ;delete by bumping lower limit up one and reducing range one.
                             (aset (key-+ (btree-key-code node) key 1) node idx)
                             (aset (%make-pointer dtp-character (1- b-range-limit))
                                   node
                                   (+ idx 2))
                ;maybe propagate to left node or parent.
                             (cond ((zerop idx)
                                    (btree-propagate-deletion node)))
                             (return node))))
                     ((key-relative-= (key-- (btree-key-code node)
                                             key
                                             b-key)
                                      (1- b-range-limit)) ;delete by bumping upper limit down.
                ;delete from upper end by reducing range one.
                      (aset (%make-pointer dtp-character (1- b-range-limit))
                            node
                            (+ idx 2))
                ;no propagate necessary since q 0 has not been changed.
                      (return node))
                     (t (btree-split-stored-range node idx key)
                        ;no propagate necessary since q0 not changed.
                        (go top))))
              ((key-= (btree-key-code node) key b-key)
          ;found item is simple entry, not range.
               (return (btree-delete-element-from-node node idx)))
              (t (ferror nil "")))  ;shouldnt have "found" it.
  ))

;At idx in node should be a range entry that encompasses key.  Split it into two entries,
;  the lower bound of the upper one being KEY.  Then rebalance the btree.
;
(defun btree-split-stored-range (node idx key)
  (cond ((btree-assure-room node 4 t)  ;must inhibit shuffle since item of interest
   ;could get moved into a node without sufficient room, causing an infinite loop.
   ;Now it will split, if necessary, which must improve situation and eventually result
   ;in enuf room.
         nil)
        (t
         (let* ((idx-lim (btree-fill-pointer node))
                (b-key (aref node idx))
                (b-assoc (aref node (1+ idx)))
                (b-range-limit (%make-pointer dtp-fix (aref node (+ 2 idx))))
                (key-delta (key-- (btree-key-code node) key b-key)))
           (array-push node nil)
           (array-push node nil)
           (array-push node nil)
           (array-push node nil)
                                                ;move stuff, including old entry, up.
           (do ((f-idx (1- idx-lim) (1- f-idx))
                (t-idx (1- (btree-fill-pointer node)) (1- t-idx)))
               ((< f-idx idx))
             (aset (aref node f-idx) node t-idx))
          ;(aset b-key node idx)          ;base of low entry is original base  --already there
          ;(aset b-assoc node (1+ idx))   ;assoc is original association       --already there
           (aset (%make-pointer dtp-character key-delta)
                 node (+ 2 idx))
           (aset nil node (+ 3 idx))            ;unused
           (aset key node (+ 4 idx))            ;bottom of range, as requested
           (aset (res-+ (btree-res-code node) key-delta b-assoc) node (+ 5 idx))
           (aset (%make-pointer dtp-character (key-relative-- b-range-limit key-delta))
                 node (+ 6 idx))
           (aset nil node (+ 7 idx))
           t))))                                ;no need to rebalance when adding things.

; KEY must exactly correspond to a BTREE entry.
;  if that is a single entry, flush it.
;  if it is a range and the range is less than or equal to key-range-limit, flush it.
;  if it is a range and the range is > that key-range-limit, flush this part of it.
;    That means smash the stored key to be KEY-RANGE-LIMIT and adjust the range.
;    Also, do a (btree-propagate-deletion node) if idx 0 has been changed.

(defun btree-delete-range-segment (node key key-range-limit)
    (prog top (idx idx-lim b-key b-key-range-top b-size)
        (multiple-value (node idx) (btree-find node key))
        (cond ((or (null node)
                   (null idx))
               (ferror "range to delete not found")))
        (setq idx-lim (btree-fill-pointer node))
        ;node is now final association node where it goes.
        (setq b-key (aref node idx))
        (cond ((and (< (+ idx 3) idx-lim)
                    (= (%data-type (aref node (+ idx 2))) dtp-character))
               (setq b-size 4
                     b-key-range-top
                      (key-+ (btree-key-code node)
                             b-key (%make-pointer dtp-fix (aref node (+ idx 2))))))
              (t (setq b-size 2
                       b-key-range-top (key-+ (btree-key-code node) b-key 1))))
        (cond ((key-= (btree-key-code node) key b-key)
               (cond ((not (key-< (btree-key-code node) key-range-limit b-key-range-top))
                      (return (btree-delete-element-from-node node idx)))
                     (t
                      (if (not (= b-size 4)) (ferror ""))
                      (aset key-range-limit node idx)
                        ;adjust association so it is unchanged for region not deleted.
                      (aset (res-+ (btree-res-code node)
                                   (key-- (btree-key-code node) key-range-limit key)
                                   (aref node (1+ idx)))
                            node
                            (1+ idx))
                      (aset (%make-pointer dtp-character
                                           (key-relative-- (aref node (+ idx 2))
                                                           (key-- (btree-key-code node)
                                                                  key-range-limit
                                                                  b-key)))
                            node (+ idx 2))
                      (cond ((zerop idx)
                             (btree-propagate-deletion node)))
                      (return node)))) ;delete lower section
              ((not (key-< (btree-key-code node) key-range-limit b-key-range-top))
               (if (not (= b-size 4)) (ferror ""))     ;delete upper section.
               (aset (%make-pointer dtp-character (key-- (btree-key-code node) key b-key))
                     node
                     (+ idx 2))
               (return node))
              (t (ferror nil "cant delete this"))) ;should have split it appropriately.
  ))

(defun btree-find-node-idx (node node-to-find)
  (prog (idx idx-lim)
        (setq idx 0
              idx-lim (btree-fill-pointer node))
   l    (cond ((not (< idx idx-lim))
               (ferror nil "unable to find node ~s in node ~s" node-to-find node)))
        (cond ((eq node-to-find (aref node (1+ idx)))
               (return idx)))
        (setq idx (+ idx 2))
        (go l)))

;(defun btree-delete-node (node)
;  (let ((parent (btree-parent-node node))
;       (left (btree-left-node node))
;       (right (btree-right-node node)))
;    (cond ((null parent)
;          nil)         ;top node can be just NULL.
;         (t
;          (let ((p-idx (btree-find-node-idx parent node)))
;      ;parent made null by this?
;      ;parent btree level continuous?
;            (btree-delete-element-from-node parent node)
;            (cond ((null left)
;                   (ferror nil "cant delete on left edge"))
;                  (t
;                   (setf (btree-right-node left) right)))
;            (cond ((null right)
;                   )
;                  (t
;                   (setf (btree-left-node right) left))))))
;    ))

(defun btree-room (node)
  (- (array-total-size node) (btree-fill-pointer node)))

(defun btree-assure-room-left-edge (node words-of-room)
  (prog (idx-lim)
   l0   (setq idx-lim (btree-fill-pointer node))
        (cond ((= (btree-type-number node) 1)
               (go fa-1))
              ((not (= (btree-type-number node) 2))
               (ferror nil "bad type number for node" node)))
   dv-1 (cond ((zerop idx-lim)
               (ferror nil "Empty subnode pointer" node)))
        (setq node (aref node 1))       ;follow left edge
        (go l0)
   fa-1 ;reached left final-association-node.  is there room?
        (cond ((>= (btree-room node) words-of-room)
               (return t))
              ((and (btree-right-node node)
                    (eq (btree-parent-node node) (btree-parent-node (btree-right-node node)))
                    (>= (btree-room (btree-right-node node))
                        (* 2 words-of-room)))
               (btree-shuffle-right node (btree-right-node node) words-of-room)
               (return t))
              (t
               (btree-split-node node)
               (return t)))))

(defun btree-store-left-edge (node association key)
  ;get here when data to be stored extends left edge of entire tree.  We know there is
  ; room in the left hand final association node and that the right-edge of the BTREE has
  ; been extended if necessary.
  (prog (idx-lim tem)
   l0   (setq idx-lim (btree-fill-pointer node))
        (cond ((= (btree-type-number node) 1)
               (go fa-1))
              ((not (= (btree-type-number node) 2))
               (ferror nil "bad type number for node ~s" node)))
   dv-1 (cond ((zerop idx-lim)
               (ferror nil "Empty subnode pointer" node)))
        (setf (aref node 0) key)        ;extend left edge.
        (setq node (aref node 1))       ;follow left edge
        (go l0)
   fa-1 ;reached left final-association-node.  Make entry (there is room).
        ;is a split or deletion necessary?  This can only happen if we are storing a range.
        (multiple-value (node tem) (btree-make-room node 0 2))
        (cond ((null tem) (ferror nil "there was supposed to be room!")))
        (setf (aref node 0) key)
        (setf (aref node 1) association)
        (return node)))


(defun btree-store-left-edge-range (node association key key-range-limit)
  ;get here when data to be stored extends left edge of entire tree.  We know there is
  ; room in the left hand final association node and that the right-edge of the BTREE has
  ; been extended if necessary.
  (prog (idx-lim tem)
   l0   (setq idx-lim (btree-fill-pointer node))
        (cond ((= (btree-type-number node) 1)
               (go fa-1))
              ((not (= (btree-type-number node) 2))
               (ferror nil "bad type number for node ~s" node)))
   dv-1 (cond ((zerop idx-lim)
               (ferror nil "Empty subnode pointer" node)))
        (setf (aref node 0) key)        ;extend left edge.
        (setq node (aref node 1))       ;follow left edge
        (go l0)
   fa-1 ;reached left final-association-node.  Make entry (there is room).
        ;is a split or deletion necessary?  This can only happen if we are storing a range.
        (multiple-value (node tem) (btree-make-room node 0 (if key-range-limit 4 2)))
        (cond ((null tem) (ferror nil "there was supposed to be room!")))
        (setf (aref node 0) key)
        (setf (aref node 1) association)
        (cond (key-range-limit
               (aset (%make-pointer dtp-character (key-- (btree-key-code node)
                                                         key-range-limit key))
                     node
                     2)
               (aset nil node 3)))  ;not used.
        (return node)))

(defun btree-change-parents (node)
  (cond ((= (btree-type-number node) 2)
         (do ((idx 0 (+ idx 2)))
             ((= idx (btree-fill-pointer node)))
           (setf (btree-parent-node (aref node (+ 1 idx)))
                 node)))))


(defun btree-store (node association key)
  (prog top (idx idx-lim b-key b-range-limit b-subnode)
   top  (setq idx 0 idx-lim (btree-fill-pointer node)
              b-subnode nil)
        (cond ((zerop idx-lim)
               (go null-btree))
              ((or (key-< (btree-key-code node) key (aref node 0))      ;left-most b-key
                   (and (btree-key-of-right-margin node)
                        (not (key-< (btree-key-code node) key (btree-key-of-right-margin node)))))
               (go up)))        ;key not within range of this node.
    ;DOMINATING node found
        (cond ((= (btree-type-number node) 1)
               (go fa-1))
              ((not (= (btree-type-number node) 2))
               (ferror nil "bad type number for node")))
  dv-1  (setq b-key (aref node idx))    ;divisor node
        (cond ((key-< (btree-key-code node) key b-key)
               (cond ((null b-subnode)
                      (ferror nil "BTREE inconsistant"))
                     (t
                      (setq node b-subnode)
                      (go top)))))
        (setq b-subnode (aref node (1+ idx))
              idx (+ idx 2))
        (cond ((= idx idx-lim)   ;if reach end, follow rightmost subnode pointer since
                                 ;its known to be within right margin.
               (setq node b-subnode)
               (go top)))
        (go dv-1)
  fa-1  (setq b-key (aref node idx))    ;final-association node
        (cond ((and (< (+ idx 3) idx-lim)
                    (= (%data-type (aref node (+ idx 2))) dtp-character))
               (setq b-range-limit (%make-pointer dtp-fix (aref node (+ idx 2))))
     ;key within [b-key, (b-key "+" b-range-limit)] ?
               (cond ((key-< (btree-key-code node) key b-key)
                      (go simple-store))   ;item not found, so no overlap.
                 ;if it is a one-wide range, just store over it.
                     ((and (key-relative-= b-range-limit 1)
                           (key-= (btree-key-code node) b-key key))
                      (aset association node (+ idx 1))
                      (return node))
                     ((and (key-<= (btree-key-code node) b-key key)
                           (key-relative-< (key-- (btree-key-code node) key b-key)
                                           b-range-limit))
                 ;within previously stored range... split previous frob, and try again.
                 ; eventually, it will be reduced to a one-wide frob and test above will win.
                      (cond ((key-= (btree-key-code node) b-key key)
                 ;bottom range is right, split one off so it can be overstored.
                             (btree-split-stored-range node idx
                                                       (key-+ (btree-key-code node)
                                                              key
                                                              1))
                             (go top))
                 ;split for bottom range
                            (t (btree-split-stored-range node idx key)
                               (go top)))))
               (setq idx (+ idx 4))
               (cond ((< idx idx-lim)
                      (go fa-1)))       ;drop thru if > than last key, since within right margin.
               )
              (t
               (cond ((key-< (btree-key-code node)
                             key b-key)         ;item not found, no overlap with previous stuff,
                      (go simple-store))        ; store it.
                     ((key-= (btree-key-code node) b-key key)
                                                ;no range, simple match, clobber away.
                      (aset association node (1+ idx))
                      (return node)))
               (setq idx (+ idx 2))
               (cond ((< idx idx-lim)
                      (go fa-1)))  ;drop thru if > than last key, since within right margin.
                 ))
   simple-store
        (cond ((zerop idx)
               (ferror nil "this cant happen")))  ;if it could, the right-margin pointer
                        ;of our left guy would have to be adjusted.
        (multiple-value (node idx)
          (btree-make-room node idx 2))
        (if (null idx)
            ;stuff had to happen, try again.
            (go top))
          ;was able to simply make room, store new entry.
        (aset key node idx)
        (aset association node (1+ idx))
        (return node)

     up (cond ((btree-parent-node node)
               (setq node (btree-parent-node node))
               (go top))
          ;range covered to right, but not to left.
              (t
          ;make sure room exists where entry will ultimately be made.
               (btree-assure-room-left-edge node 2)
               (btree-store-left-edge node association key)
               (return node)))
 null-btree
        (cond ((btree-parent-node node)
               (ferror nil "Null BTREE node which is not top-node")))
        (setf (btree-type-number node) 1)       ;make this a final-association node
     ;  (setf (btree-top-node node) node)
        (setf (btree-left-node node) nil)
        (setf (btree-right-node node) nil)
     ;  (setf (btree-key-of-right-margin node) (key-+ key 1))
        (setf (btree-key-of-right-margin node) nil)
        (array-push node key)
        (array-push node association)
        (return node)
         ))

;key-range-limit is NOT inclusive.
(defun btree-store-range (node association key key-range-limit)
  (prog top (idx-lim segment-key segment-range-limit segment-action)
        (cond ((not (key-< (btree-key-code node) key key-range-limit))
               (ferror nil "Arguments invalid")))
   top  (setq idx-lim (btree-fill-pointer node))
        (cond ((zerop idx-lim)
               (go null-btree))
              ((or (key-< (btree-key-code node) key (aref node 0))      ;left-most b-key
                   (and (btree-key-of-right-margin node)
                        (not (key-<= (btree-key-code node)
                                     key-range-limit (btree-key-of-right-margin node)))))
               (go up)))        ;full key range not within range of this node.
   store-loop
        (multiple-value (segment-key segment-range-limit segment-action)
          (btree-find-segment node key key-range-limit))
     ;(break "~%Segment found is ~s to ~s, action ~s"
     ;      segment-key segment-range-limit segment-action)
        (cond ((eq segment-action 'delete)
               (go delete)))
        (btree-store-range-section node association key segment-range-limit)
   store-loop-1
        (cond ((= segment-range-limit key-range-limit)
               (return node)))
        (setq association (res-+ (btree-res-code node)
                                 (key-- (btree-key-code node)
                                        segment-range-limit
                                        key)
                                  association)
              key segment-range-limit)
        (go store-loop)         ;store next section

     up (cond ((btree-parent-node node)
               (setq node (btree-parent-node node))
               (go top))
          ;range covered to right, but not to left.
              (t
          ;make sure room exists where entry will ultimately be made.
               (btree-assure-room-left-edge node 4)
               (multiple-value (segment-key segment-range-limit segment-action)
                 (btree-find-segment node key key-range-limit))
               (cond ((eq segment-action 'delete)
                      (go delete))
                     (t
                      (btree-store-left-edge-range node association key segment-range-limit)
                      (go store-loop-1)))))
 delete (btree-delete-range-segment node segment-key segment-range-limit)
        (go top)
 null-btree
        (cond ((btree-parent-node node)
               (ferror nil "Null BTREE node which is not top-node")))
        (setf (btree-type-number node) 1)       ;make this a final-association node
     ;  (setf (btree-top-node node) node)
        (setf (btree-left-node node) nil)
        (setf (btree-right-node node) nil)
        (setf (btree-key-of-right-margin node) nil)
        (array-push node key)
        (array-push node association)
        (array-push node (%make-pointer dtp-character (key-- (btree-key-code node)
                                                             key-range-limit key)))
        (array-push node nil)  ;no exceptions to start
        (return node)
        ))

;store a section-range.  Node is the desired dominating node, and the range is chosen
; so as to avoid LAPping problems and all necessary deletes have been done.
; No left edge problems here either.
(defun btree-store-range-section (node association key key-range-limit)
  (prog top (idx idx-lim b-key b-key-range-top b-size b-subnode)
  top   (setq idx 0 idx-lim (btree-fill-pointer node)
              b-subnode nil)
    ;DOMINATING node found
        (cond ((= (btree-type-number node) 1)
               (go fa-1))
              ((not (= (btree-type-number node) 2))
               (ferror nil "bad type number for node")))
  dv-1  (setq b-key (aref node idx))    ;divisor node
        (cond ((key-< (btree-key-code node) key b-key)
               (cond ((null b-subnode)
                      (ferror nil "BTREE inconsistant"))
                     (t
                      (setq node b-subnode)
                      (go top)))))
        (setq b-subnode (aref node (1+ idx))
              idx (+ idx 2))
        (cond ((= idx idx-lim)   ;if reach end, follow rightmost subnode pointer since
                                 ;its known to be within right margin.
               (setq node b-subnode)
               (go top)))
        (go dv-1)
  fa-1  (setq b-key (aref node idx))    ;final-association node
        (cond ((and (< (+ idx 3) idx-lim)
                    (= (%data-type (aref node (+ idx 2))) dtp-character))
               (setq b-key-range-top (key-+ (btree-key-code node)
                                          b-key (%make-pointer dtp-fix (aref node (+ idx 2))))
                     b-size 4))
              (t (setq b-key-range-top (key-+ (btree-key-code node) b-key 1)
                       b-size 2)))
      ;any overlap between [key key-high] and [b-key, b-key-high] ?
        (cond ((or (and (not (key-< (btree-key-code node) key b-key))
                        (key-< (btree-key-code node) key b-key-range-top))
                   (and (not (key-<= (btree-key-code node) key-range-limit b-key))
                        (key-<= (btree-key-code node) key-range-limit b-key-range-top)))
               (ferror nil "this stuff supposedly was deleted already!")

;              (cond ((and (key-= (btree-key-code node) key b-key)
;                          (key-= (btree-key-code node) key-range-limit b-key-range-top))
;                     ;ranges match, store frob
;                     (aset association node (1+ idx))
;                     (aset nil node (+ idx 3))
;                     (return node))

;                    ((and (key-<= (btree-key-code node) b-key key)
;                          (key-< (btree-key-code node) key-range-limit b-key-range-top))
;                     ;store this as exception under previous frob
;                     (aset (btree-store-range (aref node (+ idx 3))
;                                              association key key-range-limit)
;                           node
;                           (+ idx 3))
;                     (return node))

;                    (t
;                     (ferror nil "range breaking necessary")))
               )
              ((key-< (btree-key-code node)
                      key b-key)     ;entire range not found, no overlap with previous stuff,
               (go simple-store)))   ; store it.
        (setq idx (+ idx b-size))
        (cond ((< idx idx-lim)
               (go fa-1)))      ;drop thru if > than last key, since within right margin.
   simple-store
        (cond ((zerop idx)
               (ferror nil "this cant happen")))  ;if it could, the right margin pointer of our
                                        ;left guy would need to be adjusted.
        (multiple-value (node idx)
          (btree-make-room node idx 4))
        (if (null idx)
            ;stuff had to happen, try again.
            (return (btree-store-range node association key key-range-limit)))
          ;was able to simply make room, store new entry.
        (aset key node idx)
        (aset association node (1+ idx))
        (aset (%make-pointer dtp-character (key-- (btree-key-code node) key-range-limit key))
              node
              (+ idx 2))
        (aset nil node (+ idx 3))       ;not used.
        (return node)))


(defun test-n (node n &optional (interleave 1) (base-n 0))
  (cond ((null node)
         (setq node (make-btree :make-array  (:length *btree-default-node-size*)
                                :type-number 1
                                :key-code 1
                                :res-code 1
                                :key-of-right-margin nil))))
  (dotimes (i interleave)
    (dotimes (c n)
      (let ((x (key-+ (btree-key-code node) c base-n)))
        (cond ((= (\ x interleave) i)
               (setq node (btree-store node (list x) x))
               (check-btree node)
               )))))
  node)

(defun test-n-string (node n &optional (interleave 1) (base-n 0))
  (cond ((null node)
         (setq node (make-btree :make-array  (:length *btree-default-node-size*)
                                :type-number 1
                                :key-code 3
                                :res-code 1
                                :key-of-right-margin nil))))
  (dotimes (i interleave)
    (dotimes (c n)
      (let ((x (+ c base-n)))
        (cond ((= (\ x interleave) i)
               (let ((s (format nil "A~D" x)))
                 (setq node (btree-store node (list x) s))
                 (check-btree node)))))))
  node)


(defun test-n-delete (node n &optional (interleave 1) (base-n 0))
  (dotimes (i interleave)
    (dotimes (c n)
      (let ((x (key-+ (btree-key-code node) c base-n)))
        (cond ((= (\ x interleave) i)
               (setq node (btree-delete node x))
               (check-btree node))))))
  node)

(defun test-n-delete-string (node n &optional (interleave 1) (base-n 0))
  (dotimes (i interleave)
    (dotimes (c n)
      (let ((x (+ c base-n)))
        (cond ((= (\ x interleave) i)
               (let ((s (format nil "A~D" x)))
                 (setq node (btree-delete node s))
                 (check-btree node)))))))
  node)

(defun test-range (node)
  (cond ((null node)
         (setq node (make-btree :make-array  (:length *btree-default-node-size*)
                                :type-number 1
                                :key-code 1
                                :res-code 1
                                :key-of-right-margin nil))))
  (btree-store-range node 10100. 100. 200.)
  (btree-store node 6699 100.)
  (btree-store-range node 0. 125. 130.)
  node)

(defun check-btree (node)
  (prog (tem)
   top (cond ((setq tem (btree-parent-node node))
              (setq node tem)
              (go top)))
       (check-btree-step node)))

(defun check-btree-step (node)
  (check-node-contents node)
  (cond ((= (btree-type-number node) 2)
         (do ((idx 0 (+ idx 2)))
             ((= idx (btree-fill-pointer node)))
           (check-btree-step (aref node (1+ idx))))))
  )

(defun check-node-contents (node)
  (cond ((and (btree-parent-node node)  ;if top node, OK.
              (not (eq node *btree-node-being-filled*))  ;dont complain about that one.
              (not (> (btree-fill-pointer node) 4)))
         (ferror nil "Node fill pointer only 4")))
  (cond ((and (btree-right-node node)
              (not (eq node (btree-left-node (btree-right-node node)))))
         (break "Hes my right node, but Im not his left node")))
  (cond ((and (btree-left-node node)
              (not (eq node (btree-right-node (btree-left-node node)))))
         (break "Hes my left node, but Im not his right node")))
  (cond ((and (btree-right-node node)
              (not (key-= (btree-key-code node)
                          (btree-key-of-right-margin node)
                          (aref (btree-right-node node) 0))))
         (break "Layer discontinuous ~s " node)))
  (cond ((= (btree-type-number node) 2)
         (let ((divisor-type (btree-type-number (aref node 1))))
           (do ((idx 0 (+ idx 2)))
               ((= idx (btree-fill-pointer node)))
             (cond ((not (= divisor-type (btree-type-number (aref node (1+ idx)))))
                    (break "Divisor not uniform ~s" node))))
           (prog (idx idx-lim key)
                 (setq idx-lim (btree-fill-pointer node)
                       idx 0)
          l0   (setq key (aref node idx))
               (cond ((not (key-= (btree-key-code node) key (aref (aref node (1+ idx)) 0)))
                      (ferror nil "Divisor not accurate ~s" node)))
               (setq idx (+ idx 2))
               (cond ((not (< idx idx-lim))
                      (if (and (btree-key-of-right-margin node)
                               (not (key-< (btree-key-code node)
                                           key (btree-key-of-right-margin node))))
                          (break "key of right margin incorrect ~s" node))
                      (return t))
                     ((not (key-< (btree-key-code node) key (aref node idx)))
                      (break "node not sorted ~s" node)))
               (go l0))))
        ((= (btree-type-number node) 1)
         (prog (idx idx-lim key)
               (setq idx-lim (btree-fill-pointer node)
                     idx 0)
          l0   (setq key (aref node idx))
               (cond ((and (< (+ idx 3) idx-lim)
                           (= (%data-type (aref node (+ idx 2))) dtp-character))
                      (setq key (key-+ (btree-key-code node)
                                       key (1- (%make-pointer dtp-fix (aref node (+ idx 2))))))
                      (setq idx (+ idx 2))))
               (setq idx (+ idx 2))
               (cond ((not (< idx idx-lim))
                      (if (and (btree-key-of-right-margin node)
                               (not (key-< (btree-key-code node)
                                           key (btree-key-of-right-margin node))))
                          (break "key of right margin incorrect ~s" node))
                      (return t))
                     ((not (key-< (btree-key-code node) key (aref node idx)))
                      (break "node not sorted ~s" node)))
               (go l0)))))
