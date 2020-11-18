;;; -*- Mode: Lisp; Base: 8; Package: moby-file-system; Readtable: ZL -*-

;  COPYRIGHT LISP MACHINE INC 1985
;    CONFIDENTIAL
;  This file contains trade secret information of Lisp Machine Inc.
;   (However, I (RG) and LMI (to some extent) state their intention of making
;   this technique "OPEN" at an appropriate time.)

;moby-to-do:
;  activate double mapped stuff.
;  hack microcode to allow trapping to macrocode on moby-page faults.

;"timing" errors.
; altho the moby lock mechanism avoids most kinds of timing errors,
;lossage could occur if a random process references a paritially existant
;structure in moby space (note it needs no locks at all to do this.)
; to avoid this, the actual installation/deinstallation is done by microcode.
;there are potentially four cases:
;  local reconcilation
;  local dereconcilation.  not a problem currently since doesnt really deinstall
;      local space, just writes it out.
;  remote reconcilation
;  remote dereconcilation


;keeper data structures and transformations:
; local-page-to-dataspace-page:  ->  ref region-map at region-relative-page.
; moby-page-to-local-page-correspondance: -> btree *moby-page-association*
;            fixnum -> local-page number (and page is mapped)
;            section-defstruct  -> (page is not mapped)

; local-to-moby-correspondance:
;     -> (#'sys:region-namespace-origin region ) + region-relative-address

;checked out moby data can potentially be mapped or unmapped.
;  for now we always map it before letting anyone else have it.

;if remote machine has checked out data: (potentially need a list of hosts that have it)
;  if mapped and uniquely represented, pointer of local data q available
;        (with unreconciled data type).
;  *moby-keeper-association*

;moby-partition-sections.

;the moby data structure on the disk.
;  the moby root structure is formatted so as to be legitimate moby data, altho
;    to bootstrap, it must be handled directly.

(defconst *moby-partition-header-magic-number* (+ (lsh 231 16.) 15324)
  "Magic constant to help find lost moby partition headers.  Should not be a bignum.")

;data types in moby-space.  For the most part, the same as in local space.
;  however, dtp-gc-forward, definitely can not appear.
;     dtp-unreconciled, dtp-header-forward, dtp-body-forward probably are out too.
;     dtp-symbol-header probaby is unnecessary.
;  dtp-stack-group is for some time in the future!
;  dtp-entity needs to be overhauled.
;  dtp-self-ref-pointer is unclear.

;For now, we are going to use 50. moby addresses with 25. bits in each Q.
;  This minimizes bignum hacking and leaves extra bits...
;moby Q format:
;word 0:  cdr-code, data-type, 25 address within section.
;word 1:
(defconstant %%moby-format-bits (byte 7 9))     ;all the below bits.
(defconstant %%moby-external-bits (byte 2 14.)) ;stash other goodies here.  has no relation
                ;               to the other bits.
(defconstant %%moby-internal-bits (byte 5 9))   ;moby-format-bits minus external bits.
(defconstant %%%moby-internal-bits (byte 5 0))

(defconstant %%moby-starts-object (byte 1 13.))  ;defined relative to ART-16B buffer array.
(defconstant %%%moby-starts-object (byte 1 4))
;            0 this Q within object
;            1 this Q begins new MOBY object
;               Object boundaries are defined by %find-structure-leader and %structure-total-size.
;             List headers are considered self-contained single Q objects.
;             A block of CDR-NEXT list Qs are considered one object.  If RPLACD-FORWARDing
;             occurs, this object can suddenly subdivide itself, which is an exception
;             to the rule that object boundaries never change.  For the moment, we dont
;             allow DTP-RPLACD-FORWARD in moby-space at all.
(defconstant %%moby-boxed  (byte 2 11.))              ;defined relative to ART-16B buffer array.
(defconstant %%%moby-boxed (byte 2 2))
;            0 unboxed low 16 bits of unboxed data in wd0, 16 high bits here.
;            1 normal boxed 50. bit pointer.  Reconcile to convert to LISPM Q
;               (if dt a pointer, etc)
;            2 LOCAL cell.  Do not attempt to dereconcile, always reconciles as NIL.
(defconstant %%moby-writable (byte 2 9.))
(defconstant %%%moby-writable (byte 2 0))
;               0  read-only
;               1  uniquely-represented
;               2  automatically-interlocked
;               3  manually-interlocked

;moby data storage conventions:
;  fixnums:  usual
;  bignums:  usual (but note also "medimum-nums")
;  symbols:  same data type as usual, but points to the moby-symbol-header in a moby package section.
;     The moby-symbol-header is a one-dimensional array, 3 elements long.
;       The elements are the symbol, the print-name, and the package.
;       See package stuff below for further details, including some cute tricks.
;     Moby symbols do not "have" function cell, property cells or plists..
;  arrays:
;   dtp-array-header  25 bits in first Q, directly copied.
;  lists:    usual

;dereconciling pointers to local space.
;  Ordinarily, you lose, but:
;    symbols win.
;    dtp-instance-header s (ie flavor instances) win.
;    dtp-instance (ie pointers to flavor instances).
;      get send a :dereconcile <area-where-found> message.  Value is
;        first value is mode:
;           error.   bombs out.
;           function.  Return sexp consed in <area>.  Evaluated to reconcile.
;   works by turning into a DTP-MOBY-EVAL pointer to the list.
;    dtp-moby-eval is chosen to overlay local data type DTP-SYMBOL-HEADER.

;initialize moby data structure.
;  Can section directory be bootstrapped or is it necessary to deal with it directly in
;   moby form?
;  Answer: its all bootstrapped except for about a dozen Qs in the partition header.
;   See vmem-activate-partition, etc.

;The section directory
;  Is a "fake" file which is pointed to by the partition header and serves as the root for all.
;  Remaining directory structure is essentially identically to how things were in LMFS.
;

;section options:
(defconst %%section-option-update-on-request-only (byte 1 0))
;  Causes update to disk to occur only when explicitly requested.
;    good idea for stable structures.

(defconst %%section-option-reconcile-on-swapin    (byte 1 1))
;  Causes all data on pages swapped in to be reconciled.
;    Eliminates need for compares on swapout.  (Note: this means LOCAL address space for
;    what page points to must be assigned.  However, LOCAL address space for what that,
;    in turn, points to need not be assigned.  Good idea for when write-modifies must be
;    detected.)

(defconst %%section-option-self-relative  (byte 1 2))
;  All moby pointers within section which point to within the section
;    are to be self relative.  This allows the section to be multiply instantiated.
;    Used for root-dataspace-map.

(defconst %%section-permit-inward-pointers (byte 1 3))
;  Permit pointers from other moby-sections directly in to here.

;reincarnate-on-write (map)
;  reincarnate on write is a key tool for dealing with various problems of consistancy..
;    (such as avoid loss of data if a crash occurs in the middle of a directory operation..)
;  In the past, similar operations have sometimes been called "copy-on-write".  However,
;  in the case of data containing pointers, this name is somewhat misleading.  While the
;  data itself is indeed copied (if it has been modified),  there is also a claim on namespace
;  which inheritly cannot be copied.  Thus, ONLY ONE INCARNATION CAN BE "ACTIVE" AT A TIME.
;  The old "frozen" data may become accessible again in the course of a salvage and recover
;  operation, but cannot be "first class instantiated" as long as the newer incarnation is
;  "instantiated".  "Second class instantiated" may be possible by means of self relative pointers, etc.

;MOBY-MAPs
; A MOBY-MAP is a two level structure, the top level of which is a SECTION-MAP defstruct.
;  Entries in the array-portion of the SECTION-MAP are REGION-MAP defstructs, plus the
;  SECTION-MAP has a short array leader.  The SECTION-MAP occupies a place in the storage heirarchy
;  analogous to a LISP machine AREA.
; The REGION-MAP is analogous to a LISP machine region, and the REGION-MAP defstruct contains
;  exactly the information necessary to specify a region and associate it with both moby-namespace
;  and moby-dataspace.
;    To specify the region itself, you need its SIZE-IN-QS and its FREE-POINTER.
;    To associate with moby-namespace, you need its base MOBY-NAMESPACE-PAGE-NUMBER (42 bits).
;    Entries in the array section associate each page with its dataspace page number.
;     These numbers need only be large enuf to specify a page within the particular moby-partition in use.
;    Pages may be be assigned dataspace only as needed.  So there is a fill pointer which says how
;     may dataspace pages have been assigned.
;    Since several moby partitions may be active at once on a particular machine, the dataspace addresses
;     associated with each partition are offset by some amount so that dataspace addresses are unique
;     within a particular machine.  These kind of numbers are called DATASPACE-WITH-OFFSET.  They are
;     VALID ONLY WITHIN A PARTICULAR BOOT ON A PARTICULAR MACHINE.  THEY ARE NEVER STORED IN MOBY SPACE.
;     (ie all quantities in moby space are dataspace addresses relative to the containing moby partition).

; A DATASPACE-MAP logically "guarantees" the underlying data.  There is no way that
;  data can be modified except by operations on the dataspace-map.  However, the
;  DATASPACE-MAP itself does not concern itself with problems having to do with
;  data that might be pointed at by the contents of the underlying data.

; Can be modified through DECOUPLE operations, see below.

; Because regions are allocated in 32. page quanta, moby-namespace is also allocated in
;       32. page quanta.

;  LOCAL-CELLs
;   Certain moby Qs are permitted to contain pointers to unmapped moby space.  (Notably, LOCAL-CELLS
;   which contain a pointer to the process which has the lock when the lock is seized).  Evidently,
;   such a cell could not be dereconciled in the ordinary fashion.  Fortunately, this is not what is
;   required.  When you reboot, all locks should be reinitalized to NIL.  Thus, once a cell has been
;   declared as a LOCAL-CELL (a fact recorded in the moby external bits), DERECONCILATION becomes a NO-OP,
;   and RECONCILATION always generates NIL.  The RM-CONSING-HOST Q in the region-map is another example
;   of a LOCAL-CELL.

;The MSA structure:  (MSA-ELEMENT defstruct).
;  Is a transient structure, valid for ONE BOOT.  IS NOT in moby space.
;  Can be considered an extension of system arrays dealing with an area. (AREA-NAME, AREA-REGION-BITS, etc.)
;    Eventually, might actually be implemented this way.
;  exists for every MOBY-MAPPED area.
;  Two classes:  primary-host or not.
;   primary-host = T, (signified by PRIMARY-HOST-OBJECT defstruct component = NIL).
;     This means section is resident in one of my moby-partitions.
;   primary-host = NIL (host structure for guy who is primary in PRIMARY-HOST-OBJECT defstruct component).
;     This means we are accessing section remotely via a MOBY-SERVER.

;  The primary host must always know about all the regions in a section.  His SECTION-MAP and REGION-MAPs
;    are stored in moby-space  (in the ROOT-AREA of the moby-partition holding the section).
;    Even withing the primary host, each region can be locally mapped (ie assigned a LISPM REGION)
;    or not, independantly.
;  A non-primary host is basically accessing the section over the network.  Its SECTION-MAP and REGION-MAPs
;    are stored in unmapped local space.  If a non-primary host wants to CONS in a section, an interaction
;    is necessary with the primary host to specify (and possibly allocate) a region for this.
;    A region can have CONSABLE status on at most one host (recorded in the RM-CONSING-HOST Q of the region-map).
;    A non-primary host may know about some regions, but not necessarily all.

;-- begin random discussion --
;  Note that consecutive MOBY pages are not necessarily consecutive LOCAL pages and
;vice versa.  A moby section having several disjoint pieces can be mapped into a single
;consecutive range of local pages, for example.

;DATASPACE-USAGE-COUNT
; Specifies, for each moby-page, how many times that page appears in a DATASPACE-MAP
;  somewhere.  Additionaly, the limiting value (for the array-size used) indicates unusable.
;  The next lower value is "sticky" in case the count tries to overflow, and is also
;  used to reserve certain system root pages.
; leader: 0 -> moby page number which corresponds to element 0 in array.
;         1 -> ascending count
;         2 -> tick-count

;  To write a ordinary "file" we need dataspace pages, plus freshly allocated namespace pages.
;  ** there are no longer any namespace counts at all **
;  A reincarnate-on-write increments namespace count of page copied, but dataspace count
;   of some new page.   For this, we need only find page with dataspace count=0.
;  If the original version of a file gets deleted, this could create  pages with
;   namespace count .ne. 0, but dataspace count=0.

;INSTANTIATIONs
;More precisely the operations involved in "reincarnate-on-write" are:
;  REINCARNATE.  this results in the logical appearance of a "new" file,
;    while also "freezing" the old one.  It is a "directory" only operation, involving
;    no movement of physical data.
; New dataspace and namespace maps are created.
; Thus, namespace and dataspace counts are incremented.

;decouple.
;  if it is desired to modify a page, and the DATASPACE-USAGE-COUNT of that page is
; not 1, then simply writing the page would not win since that would change the data
; seen by some other DATASPACE-MAP.  A "DECOUPLE" is necessary.  This consists of:
;   allocate a new dataspace page (it doesnt matter if namespace count for this page .ne. 0)
;   write desired data there.
;   increment dataspace count for new page, decrement it for original page.
;   change map to point to new page instead of original one.

;As noted above, only one INCARNATION of a given section can be active at once.
;  One has to be careful when relying on incarnations for conventional backup.
;  What they do provide, is ability to checkpoint in case of a crash and recovery sequence.

;external-namespace-reference.
;  If a dataspace contains pointers to outside the section, sections containing those
;    pointers must be "frozen" to assure the "integrity" of this section.
;  There are two primary aspects to this problem:
;     determining if it is safe/possible to activate the incarnation
;     problems having to do with deletion.  Ie, warning the user if he is about to delete
;     something which will make an activation he has unusable.

;"file" block
;  tick
;  map
;  external-reference list
;  "file"s which reference me.

;timestamps
;  eventually will contain time and also clock (ie machine) number to avoid lossage caused
; comparing different clocks that may not be exactly in sync.
;  for now, just an integer.

;recovery.
;  If a crash or glitch occurs, there are serious problems involved in
;"making everything consistant again".  Our basic strategy is to revert
;everything to its status as of a "checkpoint" time.

;A section name (or almost equivantly, the section namespace) may be bound
;to different incarnations at different times. To implement the above, we need
;to determine which incarnation was active at the checkpoint time, and reactivate that one.

;The minimum region size is 64 pages, thus, the minimum size for a section's namespace
; is also 64 pages.  However, there is no requirement the 64 pages be consectutive in dataspace.

;moby page update operations are inheritly compare operations since
; there may still be unreconciled data in core which should not overwrite
; the corresponding data on disk.  Eventually, the compare will be bypassable
; if the corresponding local page has not been written at all (which fact
; will be recorded in VIRTUAL-PAGE-DATA.)

;read-only hacking for moby pages, etc..
;  two more or less separate hacks.
;  (1) detection of non-modified pages, avoiding unnecessary moby updates.
;      for this, the page needs to be r-w-f until it gets flagged as dirty.
;  (2) implementation of r-w control at individual Q level.  This works by
;      maintain the whole page in r-w-f, taking ucode traps on every write, etc.
;-- all moby pages are always RWF now, partly for above reasons but also to
;  immediately detect illegal writes of local pointers into moby space.

;;below stuff temporary until next cold load.  normally in QCOM.
;si:(DEFCONST Q-VIRTUAL-PAGE-DATA-VALUES '(
;  %%VIRTUAL-PAGE-STRUCTURE-HANDLE 0022
;  ;; Number of boxed words at the beginning of the page, 0..256.
;  %%VIRTUAL-PAGE-INITIAL-QS 0011
;  ;; Index of first object consed on this page, 256 means no header on this page.
;  %%VIRTUAL-PAGE-FIRST-HEADER 1111
;  ;; High bit of above field is 1 if there is no header on this page.
;  %%VIRTUAL-PAGE-NO-HEADER? 2101

;  %%virtual-page-moby-bits-from-moby 2701      ;if 1, moby bits valid.  May have freshly
;                                               ;       consed stuff too.
;                                               ;   0, freshly consed, moby bits irrelevant.
;  %%virtual-page-moby-format-handle-and-bits-processed 2601
;                                               ;if 1, handle and moby-format bits processed.
;  %%virtual-page-moby-bits-complete  2501      ;if 1, moby bits for whole page valid and stable.
;                                               ;   0  worry about things, bits may have to come
;                                               ;      from moby space or freshly consed.
;  %%virtual-page-moby-status-bits    2503      ;above three bits.


;  %%virtual-page-clean 3001  ;Set by moby-writeout-page.
;                            ; Cleared by ucode on write trap.

;;--old stuff.
;;  ;; The following two have significance only for local pages associated with moby ones.
;;  ;; See si:resolve-unreconciled-data for further discussion.
;;  %%VIRTUAL-PAGE-MOBY-PAGE-SOURCE  2701  ;if 1, moby page is source; 0 local page is source.
;;  %%VIRTUAL-PAGE-COPY-SKELETONIZED 2601 ;if 1, the copy page (i.e. opposite of source above),
;;                                       ; has been skeletonized.
;;  %%VIRTUAL-PAGE-SKELETONIZED-CODE 2602        ;above two bits.
;;    ;note that in any case, there can be freshly consed stuff too.
;  ))

(defvar *moby-lock*
        (make-lock :name "Moby Lock"))
(defvar *fake-moby-lock* nil)

(defvar *moby-pages-written* 0) ;count of dirty moby pages actually written.


;(defvar *moby-to-local*)       ;btree:  input 50 bit moby-namespace, 25 bit local address.
                                ;   key compares use <, output-arithmetic, %pointer-plus.

 ;use indexing based on local region instead of this
;(defvar *local-to-moby*)       ;btree:  input 25 bit local address. output, 50 bit moby
                                ;     namespace adr.
                                ;   key compares use %pointer-lessp, output arith, +.
                                ;output of this is in MOBY-NAMESPACE.

 ;used to "find" section pointed to by previously unmapped moby address.
(defvar *moby-page-association*)        ;btree:  input 42 bit moby-namespace page,
                                ; output
                                ;    if fixnum, local-page number (ie page mapped)
                                ;     otherwise coded defstruct (ie page not mapped).
                                ; codes identical in function to named-structure-symbols,
                                ;  simplify bootstrapping.
   ;possibilities for code:
   ; if page part of "bootstrapping" section, (partition-header-area or root-mapping-area)
   ;     pointer will be to msa for area. --- no will always be associated.
   ; if page associated, will be local page number.
   ; if page not associated but is occuppied by a known moby-section, a pointer to
   ;   the section defstruct (located, more than likely, in root-area).

(defvar *moby-keeper-association*) ;btree: input is local pointer
                        ;  rather than 50 bit moby-namespace address.
                        ;or would dataspace address be more convenient?
                        ;output: list of hosts.


;(defvar #'sys:region-namespace-origin (make-array si:number-of-regions))
  (fix-region-arrays)

(defvar *region-to-msa* (make-array si:number-of-regions))
(defvar *region-to-region-map* (make-array si:number-of-regions))   ;if local region.
(defvar *region-fresh-cons-boundary* (make-array si:number-of-regions))
  ;This used to keep track of where to get moby bits from.
  ; Set to free-pointer when region mapped in.  Never changed, even when moby space update.
  ; For Qs less than this, get moby bits from moby space, otherwise from local structures.

;(defvar #'sys:region-moby-bits-array (make-array si:number-of-regions))
   ;if non-NIL, a ART-8B array which contains moby bits for each Q.
   ;coding of #'sys:region-moby-bits-array

(defconstant %%%moby-region-valid (byte 1 7))
(defconstant %moby-region-valid 200)
(defconstant %%%moby-region-reconciled-p (byte 1 6))   ;if 1, Q is reconciled and valid in local space.
   ;  Not maintained for boxed data since equivalent to data-type being DTP-UNRECONCILED.
(defconstant %moby-region-reconciled-p 100)
;;;;(defconstant %%%moby-region-live  (byte 1 6))    ;not yet used
(defconstant %%%moby-region-dirty (byte 1 5))    ;not yet used
   ; low 5 bits same as %%%moby-format bits
   ;   %%%moby-starts-object
   ;   %%%moby-boxed
   ;   %%%moby-writable

  ;notes:
  ; 0 is OK as an initial value either for freshly consed or mapped stuff, since
  ;   valid bits is off.
  ; once moby-bits become valid, they remain so forever (unless region purged, etc)

   ;  "check out" structure...   list of hosts.


(defvar *area-to-msa* (make-array si:number-of-areas))  ;msa defstruct for this area.  Each area
                                ;has its own.

(defvar *moby-section-area-and-region-associations* nil)  ;elements msa-elements below.

(defvar *moby-local-package-host* nil)  ;host where we can cons new moby symbols.
                ;corresponds with *moby-package-root*.  Should be an a-list.
                ;This host also given to remote guys who ask for it.
(defvar *moby-package-root* nil)        ;root of moby-package system, or nil.
(defvar *symbol-to-moby-address*)       ;hash table associating local symbols with their
                                        ; moby addresses.  Eventually, symbols will have a
                                        ; moby-address component instead of this.


(defun moby-make-fake-map (name-of-area namespace-page-origin initial-section-map-size
                           region-size-in-pages moby-options)
  (let ((map (make-section-map
               :make-array (:area working-storage-area :length initial-section-map-size)
               :fill-pointer 0
               :area-name-string (string-append "fake-" (string name-of-area))
               :area-region-bits 0
               :area-region-size (ash region-size-in-pages 8)
               :area-moby-options moby-options))
        (region-map
          (moby-make-region-map nil
                                working-storage-area
                                namespace-page-origin
                                region-size-in-pages)))
    (array-push map region-map)
    map))

(defun moby-make-section-map
       (area-to-cons-in map-moby-handle section associated-area initial-section-map-size
  ;map-moby-handle nil for local section, supplied for remote section.
  ;  note section maps for remote sections are consed in ordinary storage and
  ;  do not necessarily have all existant regions.
  ;section is the mfile- guy for this map, if there is one.
        moby-options)
  (let ((map (make-section-map
               :make-array (:area area-to-cons-in :length initial-section-map-size)
               :fill-pointer 0
               :area-name-string
                 (let ((default-cons-area area-to-cons-in)
                       (s (string (aref #'si:area-name associated-area))))
                   (if (null map-moby-handle) (string-append s) s))
               :area-region-bits (aref #'si:area-region-bits associated-area)
               :area-region-size (aref #'si:area-region-size associated-area)
               :area-moby-options moby-options
               :locally-mapped-area associated-area
               :section-map-section section))
        (msa (aref *area-to-msa* associated-area)))
    (setf (msa-map msa) map)
    (setf (msa-map-moby-handle msa)
          (if map-moby-handle map-moby-handle (local-to-moby-correspondance map)))
    (cond ((and (null map-moby-handle)
                (not (memq (msa-kind msa) '(partition-header root-mapping))))
           (moby-declare-local-cells-for-section-map map)))
    map))

(defun moby-make-region-map (msa
                             area-to-cons-in
                             namespace-page-origin
                             region-size-in-pages)
 ;just makes region-map structure.  Does not actually make any regions.
 ;MSA used only to test if data-space map needed.  If MSA supplied nil,
 ; MOBY-MAKE-REGION-MAP will assume it is needed.
  (let* ((region-map (make-region-map
               :make-array (:area area-to-cons-in
                                ;on remote host, no data per page.
                            :length (if (and msa
                                             (msa-primary-host-object msa))
                                        0
                                      region-size-in-pages))
               :namespace-page-origin namespace-page-origin
               :free-pointer 0
               :fill-pointer 0
               :size (lsh region-size-in-pages 8))))
    (moby-declare-local-cells-for-region-map region-map)
    region-map))

(defun moby-declare-local-cells-for-area (area)
  (let ((msa (aref *area-to-msa* area)))
    (moby-declare-local-cells-for-section-map (msa-map msa))))

(defun moby-declare-local-cells-for-section-map (section-map)
  (moby-declare-local-cell (locf (sm-locally-mapped-area section-map)))
  (dotimes (rmi (array-active-length section-map))
    (let ((rm (aref section-map rmi)))
      (moby-declare-local-cells-for-region-map rm))))

(defun moby-declare-local-cells-for-region-map (region-map)
  (moby-declare-local-cell (locf (rm-consing-host region-map)))
  (moby-declare-local-cell (locf (rm-locally-mapped-region region-map))))

(defun moby-add-region-to-map-and-allocate-namespace
       (map msa region-size-in-pages partition-header)
  "Returns map for region."
  (let* ((na-allocation-structure (ph-namespace-allocation-structure partition-header))
         (namespace-page-origin (moby-allocate-namespace
                                  na-allocation-structure region-size-in-pages)))
    (moby-add-region-to-map map msa namespace-page-origin
                            region-size-in-pages)))

(defun moby-add-region-to-map (map msa namespace-page-origin region-size-in-pages)
 "Returns map for region.  Does not actually map in the region."
  (let* ((region-map (moby-make-region-map msa
                                           (%area-number map)
                                           namespace-page-origin
                                           region-size-in-pages)))
    (cond ((null (array-push-extend map region-map))
           (break "map filled")))
    (let ((section (sm-section-map-section map)))
      (if section
          (moby-store-moby-to-section-for-region-map section region-map)))
    region-map))

(defun moby-allocate-namespace (allocation-structure number-pages)
 "Return moby-namespace-page-number."
  (let ((limit (na-allocation-limit allocation-structure))
        (np (logand -40 (+ number-pages 37)))   ;always allocate on 40 boundaries.
                                                ;should be a no-op
        (free-pointer nil))
    (without-interrupts
      (setq free-pointer (na-allocation-pointer allocation-structure))
      (cond ((>= (+ free-pointer np)
                 limit)
             (break "namespace allocation exhausted")))
      (setf (na-allocation-pointer allocation-structure)
            (+ free-pointer np)))
    free-pointer))

(defun msa-association-for-map-moby-handle (map-moby-handle)
  (dolist (e *moby-section-area-and-region-associations*)
    (cond ((= map-moby-handle (msa-map-moby-handle e))
           (return e)))))

(defun partition-header-of-area (area-number)
  (let ((msa (aref *area-to-msa* area-number)))
    (msa-partition-header msa)))

;debugging only.
(defun msa-association-for-section (section)
  (dolist (e *moby-section-area-and-region-associations*)
    (cond ((eq section (msa-section e))
           (return e)))))

;(defun moby-area-number-regions (area-number)
;  (let ((msa (msa-association-for-area area-number)))
;    (length (msa-region-list msa))))

;(defun moby-area-size-in-pages (area-number)
;  (let ((msa (msa-association-for-area area-number)))
;    (do ((l (msa-region-list msa) (cdr l))
;        (size 0))
;       ((null l) size)
;      (setq size (+ size (// (aref #'si:region-length (car l)) si:page-size))))))

(defun moby-writeout-area (area-number &optional even-if-clean)
  (prog (msa map iterations)
        (setq msa (aref *area-to-msa* area-number)
              map (msa-map msa)
              iterations 0)
 ;update free pointer in dataspace map
        (cond ((null map)
               (ferror nil "Attempt to write out area without map")))
   again
        (moby-update-map-free-pointers-for-area area-number (not (zerop iterations)))
        (cond ((null (msa-primary-host-object msa))
               (cond ((or (not (eq (msa-kind msa) 'section))
                          (not (mfile-deleted? (msa-section msa))))
                      (moby-assign-dataspace-to-area area-number nil)
                      (si:for-every-region-in-area (region (msa-area msa))
                        (moby-writeout-region region (msa-partition-header msa)
                                              (sm-area-moby-options map)
                                              even-if-clean))
        ;if consing has occurred in attempt to write, try again.
                      (si:for-every-region-in-area (region (msa-area msa))
                        (cond ((and (moby-region-consable? region)
                                    (not (= (si:%region-free-pointer region)
                                            (rm-free-pointer (aref *region-to-region-map* region)))))
                               (cond ((>= (setq iterations (1+ iterations))
                                          2)
                                      (ferror nil "Attempt to writeout-area ~s does not terminate" area-number))
                                     (t (go again)))))))))
              (t (let ((response-pkt (moby-send-pkt-command-for-msa
                                       msa %moby-pkt-writeout-area (msa-map-moby-handle msa))))
                   (chaos:return-pkt response-pkt))))))

;given an area, make sure all free pointers of regions in the area are updated into
; the map of the area.  Locally apparent means don't do anything which would cause network
; traffic such as getting free pointers from remote machines.  This is not necessary after
; the first dereconcilation pass, since any consing or modification done during the
; dereconcilation will be locally apparent (ie must have happened on this machine.)
(defun moby-update-map-free-pointers-for-area (area locally-apparent)
  (let* ((msa (aref *area-to-msa* area)))
    (si:for-every-region-in-area (region area)
      (cond ((or (null locally-apparent)
                 (moby-region-consable? region))
             (let* ((namespace-page-origin (aref #'sys:region-namespace-origin region))
                    (free-pointer (moby-region-free-pointer region t))
                    (region-map (aref *region-to-region-map* region)))
               (cond ((= namespace-page-origin (rm-namespace-page-origin region-map))
                      (cond ((not (= (rm-size region-map)
                                     (si:%region-length region)))
                             (ferror nil "region sizes mismatch")))
                      (setf (rm-free-pointer region-map) free-pointer)
                      (return t))
                     (t (ferror nil "Region mapping errror")))))))
        ;if this area a partition-header-area, update free pointer in partition-header.
    (cond ((eq (msa-kind msa) 'partition-header)
           (let ((partition-header (msa-partition-header msa)))
             (if partition-header       ;might not be set up during initialization.
                 (setf (ph-partition-header-free-pointer partition-header)
                       (rm-free-pointer
                         (aref (ph-partition-header-section-map partition-header) 0)))))))
    ))

(defun moby-area-clean-p (area)
 ;** should worry about data checked out **
  (block top
    (si:for-every-region-in-area (region area)
      (let* ((fp (si:%region-free-pointer region))      ;***
             (pages (ceiling fp si:page-size)))
        (do ((p (lsh (si:%region-origin region) -8) (1+ p))
             (c 0 (1+ c)))
            ((= c pages))
          (cond ((zerop (ldb si:%%virtual-page-clean
                             (aref #'system:virtual-page-data p)))
                 (return-from top nil))))))
    t))

(defun moby-writeout-region (region-number partition-header moby-options even-if-clean)
  (let ((region-map (aref *region-to-region-map* region-number))
        (fp (moby-region-free-pointer region-number)))
    (do ((lfp 0 (+ lfp si:page-size))
         (ladr (aref #'system:region-origin region-number)
               (%pointer-plus ladr si:page-size))
         (rmi 0 (1+ rmi)))
        ((>= lfp fp))
      (cond ((not (<= rmi (rm-fill-pointer region-map)))
             (array-push region-map (moby-find-dataspace-page partition-header moby-options))))
      (moby-writeout-page (lsh ladr -8)
                          (min (- fp lfp) si:page-size)
                          even-if-clean))
    ))

(defun moby-mapped-area-for-section (section)
  (let ((section-map (mfile-map section)))
    (if section-map (sm-locally-mapped-area section-map))))

;old way.  (let ((prev (msa-association-for-section section)))
;           (if prev (msa-area prev)))

;debugging only.
(defun moby-section-mapped-to-area (area-number)
  (msa-section (aref *area-to-msa* area-number)))

;;error if area has more than one region.
(defun one-and-only-region-of-area (area)
  (let ((ans nil))
    (si:for-every-region-in-area (region area)
     (if (null ans)
         (setq ans region)
       (ferror nil "Area ~s has more than one region" area)))
    ans))

(defun moby-area-name-for-section (section)
  (intern (moby-list-string-append "MAPPED-DATA-AREA-FOR"
                                   (partition-host-name-of-section section)
                                   (mfile-full-name section)
                                   (mfile-type section)
                                   (format nil "~D" (mfile-version section)))
          "MFS"))

(defun moby-area-name-for-pathname (pathname partition-host-name)
  (intern (moby-list-string-append "MAPPED-DATA-AREA-FOR"
                                   partition-host-name
                                   (append (directory-list-of-pathname pathname)
                                           (list (send pathname :name)))
                                   (send pathname :type)
                                   (format nil "~D" (send pathname :version)))
          "MFS"))

(defun directory-list-of-pathname (pathname)
  (let ((directory (send pathname :directory)))
    (if (listp directory) directory (list directory))))

(defun moby-partition-header-area-name-for-partition-host (partition-host-name)
  (intern (string-append "MOBY-PARTITION-HEADER-AREA-FOR-" partition-host-name)
          "MFS"))
(defun moby-root-mapping-area-name-for-partition-host (partition-host-name)
  (intern (string-append "MOBY-ROOT-MAPPING-AREA-FOR-" partition-host-name)
          "MFS"))
(defun moby-root-area-name-for-partition-host (partition-host-name)
  (intern (string-append "MOBY-ROOT-AREA-FOR-" partition-host-name)
          "MFS"))

(defun moby-list-string-append (&rest args)
  (%open-call-block 'string-append 0 4)
  (dolist (e args)
    (cond ((stringp e)
           (%push e)
           (%push "-"))
          ((listp e)
           (dolist (e1 e)
             (%push e1)
             (%push "-")))
          (t (ferror nil "Cant handle this"))))
  (%pop)
  (%activate-open-call-block))

(defun moby-initialize-transforms ()
  ;static so ucode doesnt have to worry about transporting, etc.
  (make-area :name 'moby-bits-array-area
             :gc :static
             :region-size #o400000)
;  (setq *moby-to-local* (make-btree :make-array  (:length *btree-default-node-size*)
;                                   :type-number 1
;                                   :key-code 1 ;integer
;                                   :res-code 2 ;%pointer
;                                   ))
;  (setq *local-to-moby*  (make-btree :make-array  (:length *btree-default-node-size*)
;                               :type-number 1
;                               :key-code 2     ;%pointer
;                               :res-code 1     ;integer
;                               ))
;  (setq *moby-namespace-page-to-dataspace-page*
;       (make-btree :make-array (:length *btree-default-node-size*)
;                   :type-number 1
;                   :key-code 1 ;integer
;                   :res-code 1 ;integer
;                   ))
  (setq *moby-page-association*
        (make-btree :make-array (:length *btree-default-node-size*)
                    :type-number 1
                    :key-code 1                 ;integer
                    :res-code 5                 ;integer or lisp-pointer
                    ))
  (moby-keeper-reset)
  (setq *moby-package-root* nil)
  (setq *symbol-to-moby-address* (make-hash-table :test 'eq :size 1000.))
  (moby-rewipe-array #'sys:region-namespace-origin)
  (moby-rewipe-array *region-to-msa*)
  (moby-rewipe-array *region-to-region-map*)
  (moby-rewipe-array *region-fresh-cons-boundary*)
  (moby-rewipe-array #'sys:region-moby-bits-array)
  (moby-rewipe-array *area-to-msa*)
  (setq *moby-section-area-and-region-associations* nil))


(defun moby-rewipe-array (array)
  (dotimes (i (array-length array))
    (aset nil array i)))

(defun moby-remote-find-msa (map-moby-handle region-base-namespace-page)
  (let ((local-page (moby-page-to-local-page-correspondance region-base-namespace-page)))
    (cond (local-page
           (let* ((localp (lsh local-page 8))
                  (region (%region-number localp)))
             (aref *region-to-msa* region)))
          (t (msa-association-for-map-moby-handle map-moby-handle)))))

;section may be nil if partition-header, root-mapping, or root.
;dataspace-map may be nil in above cases. For root-mapping area or root it will get filled in.
;does not deal with section-map, call moby-store-map-for-area after this, if desired
(defun moby-setup-area (name-for-area kind section partition-header host-index)
  (let* ((mpa (mpa-association-of-partition-header partition-header))
         (area-number
           (make-area :name name-for-area
                      :gc :moby-fixed  ;or :moby-consable doesnt matter since regions individually hacked.
                      ))
         (msa (make-msa-element :section section
                                :area area-number
                                :kind kind
                                :partition-header partition-header
                                :mpa mpa
                                :host-index host-index)))
    (push msa *moby-section-area-and-region-associations*)
    (setf (aref *area-to-msa* area-number) msa)
    area-number))

;Create area for remote section.  Differences are: pathname instead of section (just for identification).
;  No partition-header or MPA involved.
(defun moby-remote-setup-area (name-for-area kind pathname primary-host-object)
  (let* ((area-number
           (make-area :name name-for-area
                      :gc :moby-fixed))
         (msa (make-msa-element :section pathname       ;stored here for identification
                                :area area-number
                                :kind kind
                                :primary-host-object primary-host-object
                                :host-index (send primary-host-object :host-index))))
    (push msa *moby-section-area-and-region-associations*)
    (setf (aref *area-to-msa* area-number) msa)
    area-number))

(defun moby-store-partition-header-for-area (area-number partition-header)
  (let* ((msa (aref *area-to-msa* area-number))
         (mpa (mpa-association-of-partition-header partition-header)))
    (setf (msa-partition-header msa) partition-header)
    (if mpa (setf (msa-mpa msa) mpa))))

(defun moby-store-map-for-area
       (area-number map option namespace-page consing-host-index)       ;consable?
  (let ((msa (aref *area-to-msa* area-number)))
    (setf (sm-locally-mapped-area map) area-number)
    (setf (msa-map msa) map)
    (cond ((null (msa-primary-host-object msa))
        ;for remote sections, dont clobber map-moby-handle since it has been
        ; set by primary host.
           (setf (msa-map-moby-handle msa) (local-to-moby-correspondance map))))
    (moby-add-mapped-regions-to-area area-number map
     option namespace-page consing-host-index)))

(defun moby-add-mapped-regions-to-area
       (area-number section-map option namespace-page consing-host-index)       ;consable?
  ;NIL -> all
  ;selected  -> section containing that namespace page.
  (selectq option
    (all
     (dotimes (rmi (array-active-length section-map))
       (let ((region-map (aref section-map rmi)))
         (moby-allocate-region-at-namespace area-number region-map consing-host-index))))
    (selected
     (dotimes (rmi (array-active-length section-map))
       (let ((region-map (aref section-map rmi)))
         (cond ((and (>= namespace-page (rm-namespace-page-origin region-map))
                     (<  namespace-page (+ (rm-namespace-page-origin region-map)
                                           (ash (rm-size region-map) -8))))
                (moby-allocate-region-at-namespace area-number region-map consing-host-index))))))
    (otherwise
     (ferror nil "Bad option"))))

(defun moby-allocate-region-at-namespace (area-number region-map consing-host-index)    ;consable?
  (let* ((msa (aref *area-to-msa* area-number))
         (namespace-page-origin (rm-namespace-page-origin region-map)))
    (block allocate
      (si:for-every-region-in-area (region (msa-area msa))
        (cond ((= namespace-page-origin
                  (aref #'sys:region-namespace-origin region))
               (let* ((size-in-qs (rm-size region-map))
                      (remote-host (msa-primary-host-object msa))
                      (locally-consable (moby-host-can-cons consing-host-index remote-host))
                     ;(rm-free-pointer (rm-free-pointer region-map))
                      )
                 (cond ((not (= size-in-qs
                                (si:%region-length region)))
                         (ferror nil "Region already beings at ~s with a different size"
                                namespace-page-origin)))
                 (cond ((not (= (if locally-consable #o16 #o15)
                                (ldb sys:%%region-space-type (si:%region-bits region))))
                        (ferror nil "Consability not right")))
;                (setf (aref *region-to-msa* region) msa)
;                (setf (aref *region-to-region-map* region) region-map)
;                (cond ((null (aref #'sys:region-moby-bits-array region))
;                       (setf (aref #'sys:region-moby-bits-array region)
;                             (make-array size-in-qs :type art-8b))
;                       ;(check-array-initialization (aref #'sys:region-moby-bits-array region))
;                       ))
;                (setf (rm-locally-mapped-region region-map) region)
;                (setf (aref *region-fresh-cons-boundary* region)
;                      (cond ((null consable?) nil)
;                            (t rm-free-pointer)))
;                (cond ((< (si:%region-free-pointer region)
;                          rm-free-pointer)
;                       (moby-wipe-region region (- rm-free-pointer
;                                                   (si:%region-free-pointer region)) 0)
;       ;;;get structure handle for last page so future consing will merge properly.
;                       (cond ((and consable? (not (zerop (logand rm-free-pointer #o377))))
;                              (moby-validate-moby-bits
;                                (%pointer-plus (si:%region-origin region)
;                                               rm-free-pointer)
;                                (logand rm-free-pointer #o377))))
;                       ))
                 (return-from allocate nil)))))
      (allocate-region-to-moby-area area-number nil region-map consing-host-index))))

;(defun check-array-initialization (array)
;  (let ((l (array-length array)))
;    (dotimes (c l)
;      (cond ((not (zerop (aref array c)))
;            (ferror nil "Array not initialized"))))))

(defun moby-region-consable? (region)
  (let* ((region-bits (aref #'sys:region-bits region))
         (region-type (ldb si:%%region-space-type region-bits)))
    (= region-type si:%region-space-moby-new)))

(defun moby-local-physical-host (host-index)
  (cond ((null host-index) nil)
        (t
         (let ((host (aref *moby-host-index* host-index)))
           (break "foo")))))

(defun moby-host-can-cons (host-index rhost)
 ;rhost NIL iff local section.
  (cond ((null host-index) nil)
        (t
         (let ((host (aref *moby-host-index* host-index)))
           (cond ((consp host))                 ;true during bootstrapping.
                 ((null rhost) (typep host 'moby-file-host))
                 (t (typep host 'moby-remote-host)))))))

(defun allocate-region-to-moby-area (area-number region region-map consing-host-index) ;consable?
  ;size-in-qs must be exactly a size the system will agree to make.
  ;region may be new region if already created, or nil if not yet made.
  ;  (it is necessary to make region first sometimes to avoid a infinite loop.
  ;   i.e. when dealing with ROOT-AREA.)
  ;consing-host-index may be NIL (for uncommitted, but definitely not me)
  ; if this a local section, allow consing if a moby-file-host.
  ; if this a remote section, allow consing if host a remote host.
  ;    or a fixnum to indicate a host in *moby-host-index*.
  ;    if the host in *moby-host-index* is a local host, I can CONS.  As a special kludge,
  ;    if the entry in *moby-host-index* is a cons, it counts as the local
  ;    host (this can happen during initialization).
  (let* ((namespace-page-origin (rm-namespace-page-origin region-map))
         (size-in-qs (rm-size region-map))
         (npages (ash size-in-qs -8))
         (rb (aref #'sys:area-region-bits area-number))
         (msa (aref *area-to-msa* area-number))
         (remote-host (msa-primary-host-object msa))
         (locally-consable (moby-host-can-cons consing-host-index remote-host))
         (rm-free-pointer (rm-free-pointer region-map)))
    (cond ((null region)
           (setq region
                 (compiler:%make-region area-number
                                        (%logdpb (if locally-consable
                                                     si:%region-space-moby-new
                                                   si:%region-space-moby-fixed)
                                                 sys:%%region-space-type rb)
                                        size-in-qs))
           (cond ((not (zerop rm-free-pointer))  ;if anything previously
                  (aset -1 #'sys:region-allocation-status region))))  ;insure that a list header
                ;gets put in since last thing might have been a structure.
          (t
           (if (not (eq locally-consable
                        (moby-region-consable? region)))
               (ferror nil "Region created wrong"))))
    (cond ((not (= size-in-qs
                   (si:%region-length region)))
           (ferror nil "Region did not turn out the right length")))
 ;add region to list for area.
    (setf (msa-region-list msa) (nconc (msa-region-list msa) (list region)))
    (setf (aref *region-to-msa* region) msa)
    (setf (aref #'sys:region-namespace-origin region) namespace-page-origin)
    (setf (aref *region-to-region-map* region) region-map)
    (setf (aref #'sys:region-moby-bits-array region)
          (make-array size-in-qs :type art-8b :area moby-bits-array-area))
      ;(check-array-initialization (aref #'sys:region-moby-bits-array region))
    (let ((rm-origin (si:%region-origin region)))
      (setf (rm-locally-mapped-region region-map) region)
      (btree-store-range *moby-page-association*
                         (lsh rm-origin -8)
                         namespace-page-origin
                         (+ namespace-page-origin npages))
      (setf (aref *region-fresh-cons-boundary* region)
            rm-free-pointer)
  ;;;wipe used part with 0
      (moby-wipe-region region
                        rm-free-pointer
                        0)
  ;;;if not consable, wipe rest with host index.
      (cond ((null locally-consable)
             (moby-wipe-region region
                               (- size-in-qs rm-free-pointer)
                               consing-host-index)))
      (setf (rm-consing-host region-map) consing-host-index)
  ;;;get structure handle for last page so future consing will merge properly.
      (cond ((and locally-consable (not (zerop (logand rm-free-pointer #o377))))
             (cond ((null remote-host)
                    (moby-validate-moby-bits
                      (%pointer-plus rm-origin rm-free-pointer)
                      (logand rm-free-pointer #o377)))
                   (t
                ;reconcile highest object to assure handle valid.
                    (moby-remote-validate-handle
                      (%pointer-plus rm-origin
                                     (1- rm-free-pointer)))))))
      region)))

;*** this could lose.. should be a special request..
(defun moby-remote-validate-handle (local-address)
  (moby-reconcile-object local-address))

(defun moby-wipe-region (region nwds pointer)
  ;eventually, avoid taking "clean" trap every write.
    (compiler:%advance-free-pointer-and-wipe region dtp-unreconciled pointer
                                             nwds)
    (moby-wipe-region-pages region nwds))

(defun moby-wipe-region-pages (region nwds)
  (let ((p (lsh (%pointer-plus (aref #'system:region-origin region)
                               (si:%region-free-pointer region))
                -8)))
    (do ((pp p (1+ pp))
         (wds (- nwds (logand 377 (si:%region-free-pointer region)))
              (- wds 400)))
        ((<= wds 0))
      ;;;mark pages as moby-bits-from-moby valid, not processed, not complete.
      (aset (dpb 4 si:%%virtual-page-moby-status-bits
                 (aref #'system:virtual-page-data pp))
            #'system:virtual-page-data pp)
      ;;;pages are initially "clean".
      (setf (ldb si:%%virtual-page-clean
                 (aref #'system:virtual-page-data pp))
            1))))

(defun moby-rewipe-region (region &optional no-msg)
  (if (or no-msg
          (fquery nil "Do you really want to wipe region ~s which is part of area ~s"
                  region (aref #'sys:area-name (aref #'sys:region-area-map region))))
      (let ((orig (si:%region-origin region))
            (moby-bits-array (aref #'sys:region-moby-bits-array region))
            (region-map (aref *region-to-region-map* region)))
        (setf (aref *region-fresh-cons-boundary* region)
              (rm-free-pointer region-map))   ;stuff below here is valid in moby representation.
        (dotimes (c (moby-region-free-pointer region))
          (let ((off (%pointer-plus orig c)))
            (%p-dpb-offset dtp-unreconciled %%q-all-but-pointer 0 off)
            (%p-dpb-offset 0 %%q-pointer 0 off)
            (cond ((zerop (logand c #o377))
        ;probably should wipe structure handle completely..
                   (setf (ldb si:%%virtual-page-moby-status-bits
                              (aref #'system:virtual-page-data
                                    (lsh off -8)))
                         4))))
          (aset 0 moby-bits-array c)))))

;wipe most everything, but so much as to need re-bootstrap.
(defun moby-rewipe-all (&optional wipe-root (write-out-first t))
  ;update moby representation, also, make consing boundaries current.
  (if write-out-first
      (dolist (msa *moby-section-area-and-region-associations*)
        (let ((kind (msa-kind msa)))
          (cond ((or (eq wipe-root 'really)
                     (and wipe-root (eq kind 'root))
                     (not (memq kind '(partition-header root-mapping root))))
                 (let ((area (msa-area msa)))
                   (moby-writeout-area area)))))))
  (dolist (msa *moby-section-area-and-region-associations*)
    (let ((kind (msa-kind msa)))
      (cond ((or (eq wipe-root 'really)
                 (and wipe-root (eq kind 'root))
                 (not (memq kind '(partition-header root-mapping root))))
             (let ((area (msa-area msa)))
               (si:for-every-region-in-area (region area)
                (moby-rewipe-region region t))))))))

(defun moby-initialize-root-on-disk (&optional
                                     (partition-host-name
                                       (moby-select-partition-host-name))
                                     part-name
                                     (moby-base-namespace-page
                                       (moby-select-base-namespace-page))
                                     (moby-namespace-pages 1_24.)
                                     (mapping-code 1))
  (if (null part-name) (setq part-name (moby-find-part-name)))
  (vmem-initialize)
  (if (not (boundp '*moby-page-association*)) (moby-initialize-transforms))
        ;will confirm before clobbering.
  (vmem-initialize-partition part-name partition-host-name
                             moby-base-namespace-page
                             moby-namespace-pages
                             mapping-code
                             (select-processor ((:lambda :cadr) 0)
                                               (:explorer 1)))
  )

(defun moby-to-local-correspondance (moby-address)
  (let* ((moby-page (ash moby-address -8))
         (page-association (btree-lookup *moby-page-association* moby-page)))
    (cond ((numberp page-association)
           (%pointer-plus (lsh page-association 8) (logand moby-address 377)))
          ((zerop moby-address)
           0))))

;note: this does not map NIL.
(defun moby-page-to-local-page-correspondance (moby-page)
  (let* ((page-association (btree-lookup *moby-page-association* moby-page)))
    (cond ((numberp page-association)
           page-association)
          (t nil))))

(defun local-to-moby-correspondance (local-address)
  (let* ((region (si:%region-number local-address))
         (region-namespace-page-origin (aref #'sys:region-namespace-origin region)))
    (cond (region-namespace-page-origin
           (+ (ash region-namespace-page-origin 8)
              (%pointer-difference local-address (si:%region-origin region))))
          ((zerop (%pointer local-address)) 0))))

(defun local-to-moby-correspondance-or-error (local-address)
  (cond ((local-to-moby-correspondance local-address))
        (t (ferror nil "No moby correspondance for ~S" local-address))))

(defun moby-local-to-moby-transform (localp)
  (declare (values moby-data-type moby-pointer))
  (let* ((local-data (%p-contents-offset 0 localp))
         (local-data-type (%data-type local-data))
         (local-address (%pointer local-data))
         tem)
    (cond ((setq tem (local-to-moby-correspondance local-address))
           (values local-data-type tem))
          ((or (= local-data-type dtp-instance)
               (and (named-structure-p local-data)
                    (memq :dereconcile (send local-data :which-operations))))
           (send local-data :dereconcile (%area-number localp)))
          (t (ferror nil "No moby transform for ~s (from ~s)" local-address localp)))))

(defvar *all-objects-with-cached-plists* nil)
(defvar *remember-objects-with-cached-plists* t)

;this called from si:package-named-structure-invoke, which is in SYS;CLPACK.
(defun si:moby-dereconcile-package (pkg area)
  (values dtp-moby-eval
          (let ((map-moby-handle
                  (msa-map-moby-handle (aref *area-to-msa* area))))
            (cond ((get-equal pkg map-moby-handle))
                  (t (let ((ans
                             (local-to-moby-correspondance-or-error
                               (list-in-area area
                                             'si:pkg-find-package
                                             (copy-to-area-if-necessary
                                               area
                                               (si:pkg-name pkg))))))
                       (putprop pkg ans map-moby-handle)
                       (if *remember-objects-with-cached-plists*
                           (pushnew pkg *all-objects-with-cached-plists* :test #'eq))
                       ans))))))

;like get, but uses equal so key may be a bignum.
(defun get-equal (obj key)
  (let ((plist (plist obj)))
    (do ((p plist (cddr p)))
        ((null p))
      (cond ((equal (car p) key)
             (return (cadr p)))))))

(defun moby-boot-root (&optional part-name (unit (select-processor ((:cadr :lambda) 0)
                                                                   (:explorer 1))))
  (if (null part-name) (setq part-name (moby-find-part-name)))
  (if (not (boundp '*vmem-page-reuse-pointer*))
      (vmem-initialize))
  (if (not (boundp '*moby-page-association*)) (moby-initialize-transforms))
  (vmem-activate-partition part-name unit)
)

(DEFCONST *MOBY-PREFERRED-PARTITIONS* '("0MB0" "0MB1" "0MB2"))

(defun moby-find-part-name (&optional (unit 0))
  (select-processor
    (:explorer "MOBY")
    ((:CADR :LAMBDA)
     (let* ((op si:*my-op*)
            (op-index (si:op-proc-number op))
            (preferred-part (nth op-index *MOBY-PREFERRED-PARTITIONS*))
            (partitions (si:partition-list nil unit)))
       (cond ((assoc preferred-part partitions)
              preferred-part)
             (t "MOBY"))))
     ))


;returns dataspace-page-with-offset
(defun moby-namespace-page-to-dataspace-page (moby-namespace-page)
  (let ((local-page (moby-page-to-local-page-correspondance moby-namespace-page)))
    (moby-local-page-to-dataspace-page local-page)))

(defun moby-local-page-to-dataspace-page (local-page &optional null-ok)
  (let* ((region (si:%region-number (lsh local-page 8)))
         (msa (aref *region-to-msa* region))
         (relative-page (- local-page (lsh (si:%region-origin region) -8)))
         (region-map (aref *region-to-region-map* region)))
    (cond ((null msa)
           (ferror nil "No msa for page ~s" local-page))
          ((null region-map)
           (ferror nil "No region-map for region ~s" region))
          ((or (> relative-page (rm-fill-pointer region-map))
               (null (aref region-map relative-page)))
           (if null-ok nil
             (ferror nil "No dataspace assigned to this page")))
          (t
           (let ((mpa (msa-mpa msa)))
             (if (null mpa)
                 (ferror nil "MPA of MSA not filled in")
               (+ (aref region-map relative-page)
                  (mpa-dataspace-origin mpa))))))))


(defun moby-writeout-page (local-page active-qs-in-page even-if-clean)
  (let ((vp (aref #'system:virtual-page-data local-page)))
    (when (or even-if-clean
              (zerop (ldb si:%%virtual-page-clean vp)))
    ;;;set clean bit first, so it will be cleared if writes occur during dereconcile.
      (setf (ldb si:%%virtual-page-clean
                 (aref #'system:virtual-page-data local-page))
            1)
      (let* ((local-address (lsh local-page 8))
             (region (%region-number local-address)))
        (incf *moby-pages-written*)
        (cond ((zerop (ldb si:%%virtual-page-moby-bits-complete vp))
    ;;;do the whole thing, both from moby space and freshly consed.
               (moby-validate-moby-bits (lsh local-page 8) active-qs-in-page)
    ;;;structure handle may have gotten filled in, so gobble it again.
               (setq vp (aref #'system:virtual-page-data local-page))
               (cond ((zerop (ldb si:%%virtual-page-moby-format-handle-and-bits-processed
                                  vp))
    ;;;structure handle still not filled in.  Error unless remotely consable.
                      (cond ((moby-region-consable? region)
                             (ferror nil "Handle not valid"))
                            (t
                             (let ((lp (%p-ldb-offset %%q-pointer 0 local-address))
                                   (ldt (%p-ldb-offset %%q-data-type 0 local-address)))
                             (cond ((and (= ldt dtp-unreconciled)
                                         (not (zerop lp)))
                                     ;he may have consed it, get it.
                                    (moby-reconcile-remote-object local-address
                                     (moby-get-conn-for-host
                                       (aref *moby-host-index* lp))
                                     nil)
                                    (setq vp (aref #'system:virtual-page-data local-page))
                                    (if (zerop (ldb si:%%virtual-page-moby-format-handle-and-bits-processed
                                                    vp))
                                        (ferror nil "Structure handle did not become valid.")))
                                   (t
                                    (ferror nil "Moby bits not valid in non-consable region"))
                                    ))))))))
        (let ((dataspace-page (moby-local-page-to-dataspace-page local-page)))
          (cond ((not (= dataspace-page *moby-cached-dataspace-page*))
                 (setq *moby-cached-vmem-idx*
                       (vmem-find-page dataspace-page)
                       *moby-cached-dataspace-page* dataspace-page)))
          (let* ((vmem-idx *moby-cached-vmem-idx*)
                 (buf (aref *vmem-pages* vmem-idx 5))
                 (local-structure-handle
                   (ldb si:%%virtual-page-structure-handle vp))
                 (region-relative-address
                   (%pointer-difference local-address (si:%region-origin region)))
                 (region-moby-bits-array (aref #'sys:region-moby-bits-array region))
                 (moby-bits 0))
            (incf (aref *vmem-pages* vmem-idx 4))       ;increment lock.
            (cond ((null region-moby-bits-array)
                   (ferror nil "No moby bits for region ~s" region))
;             ((not (bit-test (aref region-moby-bits-array region-relative-address)
;                             %moby-region-valid))
;              (moby-validate-moby-bits-of-freshly-consed-local-page
;                local-address))
                  )
  ;;;make sure a supposedly fixed bug is really gone.  It could cause first header of the
  ;;; first page in a region to get set to 1, when 0 contained a list-header.  This could
  ;;; eventually cause the reconciler to try scanning below the region origin.
            (if (zerop region-relative-address)
                (if (not (zerop (ldb si:%%virtual-page-first-header local-structure-handle)))
   ;                (setf (ldb si:%%virtual-page-first-header vp) 0)
   ;                (setq local-structure-handle
   ;                      (ldb si:%%virtual-page-structure-handle vp))
   ;                (setf (aref #'system:virtual-page-data local-page) vp)
                    (ferror nil "lose")
                  ))

            (if (or (> (ldb si:%%virtual-page-initial-qs local-structure-handle) 400)
                    (> (ldb si:%%virtual-page-first-header local-structure-handle) 400))
                (ferror nil "Bad structure handle"))
            (do ((moby-idx 0 (+ moby-idx 4))
                 (local-offset 0 (1+ local-offset))
                 (localp))
                ((>= local-offset active-qs-in-page)
                 (moby-store-structure-handle-into-external-bits
                   local-structure-handle
                   buf)
                 (aset t *vmem-pages* vmem-idx 3)       ;set modified flag
                 (decf (aref *vmem-pages* vmem-idx 4))) ;release lock.
              (setq localp (%pointer-plus local-address local-offset))
       retry-moby-bits
              (setq moby-bits (aref region-moby-bits-array
                                    (+ region-relative-address local-offset)))
              (cond ((zerop (logand moby-bits %moby-region-valid))
                     (cond ((moby-region-consable? region)
                            (ferror nil "Moby bits not valid"))
                           (t (let ((lp (%p-ldb-offset %%q-pointer 0 localp))
                                    (ldt (%p-ldb-offset %%q-data-type 0 localp)))
                                (cond ((and (= ldt dtp-unreconciled)
                                            (not (zerop lp)))
                                       ;he may have consed it, get it.
                                       (moby-reconcile-remote-object localp
                                        (moby-get-conn-for-host
                                          (aref *moby-host-index* lp))
                                        nil)
                                       (go retry-moby-bits))
                                      (t
                                       (ferror nil "Moby bits not valid in non-consable region"))
                                      ))))))
        retry-body
             (cond ((= 2 (ldb %%%moby-boxed moby-bits))
      ;            (cond ((not (= 2 (ldb %%moby-boxed (aref buf (+ moby-idx 3)))))
      ;                   (ferror nil "Moby space moby-bits of local cell not 2")))
      ;;;LOCAL-CELL, do nothing except update cdr code.
                    (let ((lcc (%p-ldb-offset %%q-cdr-code 0 localp)))
                      (aset 0 buf moby-idx)
                      (aset (ash (dpb lcc (byte 2 5) dtp-fix) 9)
                            buf (1+ moby-idx))
                      (aset 0 buf (+ moby-idx 2))
                      (aset (dpb moby-bits %%moby-internal-bits 0) buf (+ moby-idx 3))))
                   ((zerop (ldb %%%moby-boxed moby-bits))
                ;leave moby data alone if not valid in local space.
                    (cond ((not (zerop (logand %moby-region-reconciled-p moby-bits)))
                           (aset (%p-ldb-offset (byte 16. 0) 0 localp)
                                 buf moby-idx)
                           (aset (%p-ldb-offset (byte 16. 16.) 0 localp)
                                 buf (+ 2 moby-idx))
                           (aset 0 buf (+ 1 moby-idx))
                           (aset (dpb moby-bits %%moby-internal-bits 0) buf (+ 3 moby-idx)))))
                   (t
                    (let ((lp (%p-ldb-offset %%q-pointer 0 localp))
                          (ldt (%p-ldb-offset %%q-data-type 0 localp))
                          (lccdt (%p-ldb-offset %%q-all-but-pointer 0 localp)))
                      (select ldt
                        ((dtp-fix dtp-character dtp-small-flonum)
                         (aset lp buf moby-idx) ;bottom 16 bits of "address"
                         (aset (dpb lccdt (byte 7 9) (ldb (byte 9 16.) lp))     ;cc, dt, plus next 9 bits.
                               buf (1+ moby-idx))
                         (aset 0 buf (+ moby-idx 2))
                         (aset (dpb moby-bits %%moby-internal-bits 0)   ;rep field (normal, 50. bits)
                               buf (+ moby-idx 3)))
                        ((dtp-array-pointer dtp-list dtp-instance)
                         (multiple-value-bind (moby-data-type moby-adr)
                             (moby-local-to-moby-transform localp)
                           (aset (ldb 0020 moby-adr) buf moby-idx)
                           (aset (dpb (dpb moby-data-type (byte 5 0) lccdt)
                                      (byte 7 9)
                                      (ldb (byte 9 16.) moby-adr))
                                 buf (1+ moby-idx))
                           (aset (ldb (byte 16. 25.) moby-adr) buf (+ moby-idx 2))
                           (aset (dpb moby-bits %%moby-internal-bits
                                      (ldb (byte 9 41.) moby-adr))
                                 buf (+ moby-idx 3))))
                        ((dtp-symbol)
                         (let* ((sym (%p-contents-offset 0 localp))
                                (moby-symbol-defstruct
                                  (moby-intern-memoed
                                    (moby-local-partition-host localp)
                                    sym))       ;returns 0 for NIL.
                                (moby-adr (local-to-moby-correspondance-or-error
                                            (%pointer moby-symbol-defstruct))))
                           (cond ((not (or (null sym)   ;special case.
                                           (arrayp moby-symbol-defstruct)))
                                  (ferror nil "Moby symbol defstruct for ~s screwwed"
                                          sym))
                                 (t
                                  (aset (ldb (byte 16. 0) moby-adr) buf moby-idx)
                                  (aset (dpb lccdt (byte 7 9) (ldb (byte 9 16.) moby-adr))
                                        buf (1+ moby-idx))
                                  (aset (ldb (byte 16. 25.) moby-adr) buf (+ moby-idx 2))
                                  (aset (dpb moby-bits %%moby-internal-bits
                                             (ldb (byte 9 41.) moby-adr))
                                        buf (+ moby-idx 3))))))
                       ((dtp-array-header dtp-header)
                        (aset lp buf moby-idx)
                        (aset (dpb lccdt (byte 7 9) (ldb (byte 9 16.) lp)) buf (1+ moby-idx))
                        (aset 0 buf (+ moby-idx 2))
                        (aset (dpb moby-bits %%moby-internal-bits 0)
                              buf (+ moby-idx 3)))
                       (dtp-extended-number
                        (let ((ex-num (%p-contents-offset 0 localp)))
                          (cond ((and (fixp ex-num)
                                      (<= (haulong ex-num) 50.))
                                 (aset (ldb (byte 16. 0) ex-num)
                                       buf moby-idx)
                                                ;change data type to fix since it fits in a moby fixnum.
                                 (aset (dpb (dpb dtp-fix (byte 5 0) lccdt) (byte 7 9)
                                            (ldb (byte 9 16.) ex-num))
                                       buf (1+ moby-idx))
                                 (aset (ldb (byte 16. 25.) ex-num) buf
                                       (+ 2 moby-idx))
                                 (aset (dpb moby-bits %%moby-internal-bits
                                            (ldb (byte 9 41.) ex-num))
                                       buf (+ 3 moby-idx)))
                                (t
                                 (ferror nil "This form of extended number cannot be~
 handled within a moby object" ex-num)))))
                       (dtp-unreconciled
                        (cond ((zerop lp) nil)                  ;leave moby data alone.
                              (t        ;gave it to somebody, get it back.
                               (moby-reconcile-remote-object localp
                                (moby-get-conn-for-host
                                  (aref *moby-host-index* lp))
                                nil)
                               (go retry-body))))
                       (dtp-instance-header
                        (let* ((instance (%make-pointer dtp-instance localp))
                               (moby-instance-description-defstruct
                                 (moby-find-flavor-instance-description
                                   (moby-local-partition-host localp)
                                   instance))
                               (moby-adr (local-to-moby-correspondance-or-error
                                           (%pointer moby-instance-description-defstruct))))
                          (aset (ldb (byte 16. 0) moby-adr) buf moby-idx)
                          (aset (dpb lccdt (byte 7 9) (ldb (byte 9 16.) moby-adr))
                                buf (1+ moby-idx))
                          (aset (ldb (byte 16. 25.) moby-adr) buf (+ moby-idx 2))
                          (aset (dpb moby-bits %%moby-internal-bits
                                     (ldb (byte 9 41.) moby-adr))
                                buf (+ moby-idx 3))))
                       ((dtp-header-forward dtp-body-forward)
                        (let ((moby-adr (local-to-moby-correspondance-or-error lp)))
                          (aset (ldb 0020 moby-adr) buf moby-idx)
                          (aset (dpb lccdt (byte 7 9) (ldb (byte 9 16.) moby-adr))
                                buf (1+ moby-idx))
                          (aset (ldb (byte 16. 25.) moby-adr) buf (+ moby-idx 2))
                          (aset (dpb moby-bits %%moby-internal-bits
                                     (ldb (byte 9 41.) moby-adr))
                                buf (+ moby-idx 3))))
                       (otherwise
                        (ferror nil
                                "Data type ~s(~s) cannot be handled within moby object, proceed to smash it to NIL"
                                (nth ldt q-data-types) ldt)
                        (%p-dpb-offset dtp-symbol %%q-data-type 0 localp)
                        (%p-dpb-offset 0 %%q-pointer 0 localp)
                        (go retry-body))))))) ))
        ))))


(defun moby-keeper-reset ()
  (setq *moby-keeper-association*
        (make-btree :make-array (:length *btree-default-node-size*)
                    :type-number 1
                    :key-code 2                 ;local-pointer
                    :res-code 5                 ;integer or lisp pointer
                    ))
  )

(defun moby-keeper-update (host local-base size)
  ;(format t "~%host: ~s, local-base ~s, size ~s" host local-base size)
  (btree-store-range *moby-keeper-association* host local-base (%pointer-plus local-base size))
  )

;This returns the chaos host for now.
(defun lispm-host-of-conn (conn)
  ;gets something from SI:HOST-ALIST.
  (SI:GET-HOST-FROM-ADDRESS (CHAOS:FOREIGN-ADDRESS CONN) :CHAOS))

(defun moby-host-of-conn (conn)
  (cond ((getf (chaos:conn-plist conn) 'moby-host))
        (t (let* ((lispm-host (lispm-host-of-conn conn))
                  (moby-host (moby-host-of-lispm-host lispm-host)))
             (setf (getf (chaos:conn-plist conn) 'moby-host) moby-host)
             moby-host))))

(defvar *dereconcile-on-send-object-to-net* t)

(defun moby-dereconcile-object-to-net
       (conn seq options base-of-object total-size boxed-size pkt-comm-code) options
  (let* ((moby-host (moby-host-of-conn conn))
         (unreconciled-index-for-host (send moby-host :host-index))
         (dereconcile-p *dereconcile-on-send-object-to-net*)
         pkt moby-q-idx moby-q-lim loop-count localp pkt-base-local
         region region-origin region-moby-bits-array msa section-map moby-bits sub-seq)
    (moby-keeper-update moby-host base-of-object total-size)
    (labels
      ((send-pkt (arg)
        (moby-send-pkt-response-data conn pkt seq
         (dpb arg (byte 8 8) sub-seq)
         moby-q-idx
         (ldb si:%%virtual-page-structure-handle
              (aref #'system:virtual-page-data (lsh pkt-base-local -8)))
         (local-to-moby-correspondance-or-error pkt-base-local)
         pkt-comm-code)
        (init-pkt)
        (incf sub-seq))
       (init-pkt ()
        (setq pkt (chaos:get-pkt))
        (setq moby-q-idx 0
              pkt-base-local (%pointer localp)
              moby-q-lim (min *max-moby-qs-in-pkt*
                              (- #o400 (logand (%pointer pkt-base-local) #o377))))))
      (tagbody
          (setq loop-count boxed-size
                localp base-of-object
                sub-seq 0)
          (setq region (%region-number localp)
                region-origin (si:%region-origin region)
                region-moby-bits-array (aref #'sys:region-moby-bits-array region)
                msa (aref *region-to-msa* region)
                section-map (msa-map msa))
          (cond ((and section-map
                      (not (zerop (ldb %%area-moby-option-write-once
                                       (sm-area-moby-options section-map)))))
                 (setq dereconcile-p nil)))
          (cond ((null region-moby-bits-array)
                 (ferror nil "No moby bits for region ~s" region)))
          (init-pkt)
  ;if moby-format bits not valid,
  ;  if frob unreconciled, send it over net directly.
  ;     otherwise frob must be freshly consed, call moby-validate-moby-bits.
     l1   (cond ((not (< moby-q-idx moby-q-lim))
                 (send-pkt 0)
                 (go l1))
                ((zerop loop-count)
                 (go x1)))
       r1 (setq moby-bits (aref region-moby-bits-array
                                (%pointer-difference localp region-origin)))
          (let ((lp (%p-ldb-offset %%q-pointer 0 localp))
                (ldt (%p-ldb-offset %%q-data-type 0 localp))
                (lccdt (%p-ldb-offset %%q-all-but-pointer 0 localp)))
            (cond ((zerop (logand %moby-region-valid moby-bits))
                   (ferror nil "Moby bits not valid -dereconcile-to-net")
                   ;check beyond fresh-cons boundary.  **
                   (moby-validate-moby-bits localp nil))
                  ((= 2 (ldb %%moby-boxed moby-bits))
                   ;local-cell. dont dereconcile anything and send nil.
                   (store-moby-address-in-pkt pkt
                    0 moby-q-idx (dpb dtp-symbol (byte 5 0) lccdt) moby-bits)
                   (go e1)))
            (select ldt
              ((dtp-fix dtp-character dtp-small-flonum)
               (store-pointer-bits-in-pkt pkt lp moby-q-idx lccdt moby-bits))
              ((dtp-array-pointer dtp-list dtp-instance)
               (multiple-value-bind (moby-data-type moby-adr)
                   (moby-local-to-moby-transform localp)
               (store-moby-address-in-pkt pkt
                moby-adr moby-q-idx (dpb moby-data-type (byte 5 0) lccdt) moby-bits)))
              ((dtp-header-forward dtp-body-forward)
               (store-moby-address-in-pkt pkt
                (local-to-moby-correspondance-or-error lp) moby-q-idx lccdt moby-bits))
              ((dtp-symbol)
               (let* ((sym (%p-contents-offset 0 localp))
                      (moby-symbol-defstruct
                        (moby-intern-memoed
                          (moby-local-partition-host localp)
                          sym)) ;returns 0 for NIL.
                      (moby-adr (local-to-moby-correspondance-or-error
                                  (%pointer moby-symbol-defstruct))))
                 (cond ((not (or (null sym)     ;special case.
                                 (arrayp moby-symbol-defstruct)))
                        (ferror nil "Moby symbol defstruct for ~s screwwed"
                                sym))
                       (t
                        (store-moby-address-in-pkt pkt moby-adr moby-q-idx lccdt moby-bits)))))
              ((dtp-array-header dtp-header)
               (store-pointer-bits-in-pkt pkt
                 lp moby-q-idx lccdt moby-bits))
              (dtp-extended-number
               (let ((ex-num (%p-contents-offset 0 localp)))
                 (cond ((and (fixp ex-num)
                             (<= (haulong ex-num) 50.))
                         ;change data type to fix since it fits in a moby fixnum.
                        (store-moby-address-in-pkt
                          pkt ex-num moby-q-idx
                          (dpb dtp-fix (byte 5 0) lccdt) moby-bits))
                       (t
                        (ferror nil "This form of extended number cannot be~
 handled within a moby object" ex-num)))))
              (dtp-unreconciled
               (cond ((and (null (msa-primary-host-object msa))
                           (zerop lp))
                      (multiple-value-bind (q0 q1 q2 q3)
                          (moby-fetch-quarters localp)
                        (store-moby-quarters-in-pkt pkt
                         q0 q1 q2 q3 moby-q-idx)))              ;send moby format data
                     (t
                      (store-moby-address-in-pkt pkt 0 moby-q-idx lccdt moby-bits))))
              (dtp-instance-header
               (let* ((instance (%make-pointer dtp-instance localp))
                      (moby-instance-description-defstruct
                        (moby-find-flavor-instance-description
                          (moby-local-partition-host localp)
                          instance))
                      (moby-adr (local-to-moby-correspondance-or-error
                                  (%pointer moby-instance-description-defstruct))))
                 (store-moby-address-in-pkt pkt moby-adr moby-q-idx lccdt moby-bits)))
              (otherwise
               (ferror nil
                       "Data type ~s(~s) cannot be handled within moby object, proceed to smash it to NIL"
                       (nth ldt q-data-types) ldt)
               (%p-dpb-offset dtp-symbol %%q-data-type 0 localp)
               (%p-dpb-offset 0 %%q-pointer 0 localp)
               (go r1))))
          (cond (dereconcile-p
                 (%p-dpb-offset dtp-unreconciled %%q-data-type 0 localp)
                 (%p-dpb-offset unreconciled-index-for-host %%q-pointer 0 localp)))
       e1 (setq localp (%pointer-plus localp 1)
                loop-count (1- loop-count)
                moby-q-idx (1+ moby-q-idx))
          (go l1)
       x1 (setq loop-count (- total-size boxed-size))
       l2 (cond ((not (< moby-q-idx moby-q-lim))
                 (send-pkt 0)
                 (go l2))
                ((zerop loop-count)
                 (go x2)))
          (setq moby-bits (aref region-moby-bits-array
                                (%pointer-difference localp region-origin)))
          (cond ((zerop (logand %moby-region-valid moby-bits))
                 (ferror nil "moby-bits not valid-unboxed"))
                ((zerop (logand %moby-region-reconciled-p moby-bits))
                 (cond ((not (= (%p-ldb-offset %%q-data-type 0 localp)
                                dtp-unreconciled))
                        (ferror nil "Wrong data type when unboxed and not reconciled")))
                 (multiple-value-bind (q0 q1 q2 q3)
                     (moby-fetch-quarters localp)
                   (store-moby-quarters-in-pkt pkt
                                               q0 q1 q2 q3 moby-q-idx)))
                (t
                 (store-moby-halves-in-pkt pkt
                                           (%p-ldb-offset (byte 16. 0) 0 localp)
                                           (%p-ldb-offset (byte 16. 16.) 0 localp)
                                           moby-q-idx moby-bits)))
   ;smash unreconciled data type even in unboxed qs so scanning wins.
          (cond (dereconcile-p
                 (%p-dpb-offset dtp-unreconciled %%q-data-type 0 localp)
                 (%p-dpb-offset unreconciled-index-for-host %%q-pointer 0 localp)
                 (setf (aref region-moby-bits-array
                             (%pointer-difference localp region-origin))
                       (dpb 0 %%%moby-region-reconciled-p moby-bits))))
          (setq localp (%pointer-plus localp 1)
                loop-count (1- loop-count)
                moby-q-idx (1+ moby-q-idx))
          (go l2)
   x2   ;send pkt
          (send-pkt 1_7.)       ;end flag.
          ))))

(defun moby-count-occurances (occurance-count-array usage-count-array)
  (dotimes (i (array-length occurance-count-array))
    (aset 0 occurance-count-array i))
  (dotimes (c (array-length usage-count-array))
    (incf (aref occurance-count-array (aref usage-count-array c)))))

;given an area, assign dataspace (if not already assigned) to the regions of the area.
;  Assign only up to the current free pointer unless whole-thing-p is t, in which case
;  assign dataspace to the entire region.
;use single or double mapped space as selected by the area-moby-options in the section map.
(defun moby-assign-dataspace-to-area (area whole-thing-p)
  (let* ((msa (aref *area-to-msa* area))
         (map (msa-map msa))
         (partition-header (msa-partition-header msa))
         (moby-options (sm-area-moby-options map)))
    (si:for-every-region-in-area (region area)
      (let ((namespace-page-origin (aref #'sys:region-namespace-origin region))
            (free-pointer (moby-region-free-pointer region)))
        (dotimes (i (array-active-length map)
                    (ferror nil "Unable to find region ~s in map ~s" region map))
          (let ((region-map (aref map i)))
            (cond ((= namespace-page-origin (rm-namespace-page-origin region-map))
                   (cond ((not (= (rm-size region-map)
                                  (si:%region-length region)))
                          (ferror nil "region sizes mismatch")))
                   (let ((pages (lsh (if whole-thing-p
                                         (si:%region-length region)
                                       (%pointer-plus free-pointer #o377))
                                     -8)))
                     (dotimes (p pages)
                       (when (null (aref region-map p))
                         (cond ((not (= p (rm-fill-pointer region-map)))
                                (ferror nil "rm-fill-pointer out of phase")))
                         (array-push region-map
                                     (moby-find-dataspace-page partition-header moby-options)))))
                   (return t)))))))))

(defun moby-find-dataspace-page (partition-header moby-options)
  (let* ((single-mapped (zerop (ldb %%area-moby-option-double-mapped moby-options)))
         (uc (if single-mapped
                 (ph-single-mapped-usage-count-array partition-header)
               (ph-double-mapped-usage-count-array partition-header)))
         (scan-pointer (uc-scan-pointer uc))
         (scan-limit (array-length uc))
         (mpa (mpa-of-partition-header partition-header))
         (uoc (if single-mapped
                  (mpa-single-mapped-dataspace-usage-occurance-counts mpa)
                (mpa-double-mapped-dataspace-usage-occurance-counts mpa))))
    (do ((c 0 (1+ c))
         (track scan-pointer (cond ((< (1+ track) scan-limit) (1+ track))
                                   (t 0))))
        ((= c scan-limit)
         (ferror nil "dataspace full"))
      (cond ((zerop (aref uc track))
             (setf (aref uc track) 1)
             (setf (uc-scan-pointer uc) track)
             (incf (aref uoc 0) -1)
             (incf (aref uoc 1) 1)
             (return (+ track (uc-base-dataspace-page uc))))))
    ))

(defun moby-delete-map-dataspace (map partition-header)
  (let* ((moby-options (sm-area-moby-options map))
         (single-mapped (zerop (ldb %%area-moby-option-double-mapped moby-options)))
         (uc (if single-mapped
                 (ph-single-mapped-usage-count-array partition-header)
               (ph-double-mapped-usage-count-array partition-header)))
         (mpa (mpa-of-partition-header partition-header))
         (uoc (if single-mapped
                  (mpa-single-mapped-dataspace-usage-occurance-counts mpa)
                (mpa-double-mapped-dataspace-usage-occurance-counts mpa))))
    (dotimes (i (array-active-length map))
      (let ((region-map (aref map i)))
        (dotimes (p (array-active-length region-map))
          (moby-delete-dataspace-page (- (aref region-map p) (uc-base-dataspace-page uc)) uc uoc)
          (setf (aref region-map p) nil))
        (setf (rm-fill-pointer region-map) -1))))
  )

(defun moby-delete-dataspace-page (dataspace-page-relative-to-uc uc uoc)
  (incf (aref uoc (aref uc dataspace-page-relative-to-uc)) -1)
  (incf (aref uc dataspace-page-relative-to-uc) -1)
  (incf (aref uoc (aref uc dataspace-page-relative-to-uc)) 1)
  )

(defun moby-map-data-length-in-pages (map)
  (let ((size 0))
    (dotimes (i (array-active-length map))
      (let ((region-map (aref map i)))
        (incf size (rm-fill-pointer region-map))))
    size))

(DEFUN MOBY-MAP-DATA-LENGTH-IN-BITS (MAP)
  (* 32. 256. (moby-map-data-length-in-pages map)))     ;could look at free pointer(s) ...


;Returns usage-array index. This is equivalent to a moby page number relative to
; the moby-section-origin (which is stored in (array-leader usage-array 0)).
;(defun moby-find-block-in-usage-array (usage-array max-pages-desired)
;  (let ((scanning-index (array-leader usage-array 3))
;       free-index
;       (max-index (array-length usage-array)))
;    (setq free-index (or (%string-search-char 0 usage-array scanning-index max-index)
;                        (%string-search-char 0 usage-array 1 scanning-index)
;                        (ferror nil "Completely out of space")))
;    (do ((i free-index (1+ i))
;        (p 0 (1+ p)))
;       (())
;      (cond ((or (>= p max-pages-desired)
;                (>= i max-index)
;                (not (zerop (aref usage-array i))))
;            (setf (array-leader usage-array 3) i)
;            (return-from moby-find-block-in-usage-array
;                free-index p))
;           (t (aset 1 usage-array i))))))

(defun moby-typed-pointer (dtp moby-address)
  (let ((local (moby-to-local-correspondance moby-address)))
    (cond ((null local)
           (ferror nil "No local correspondance for ~s" moby-address))
          (t
           (%make-pointer dtp local)))))

;debugging only.
(defun moby-rewalk (&optional (partition-host-name "MOBY"))
  (let ((sdh (lookup-section-directory-header partition-host-name)))
    (setq *moby-page-association*
          (make-btree :make-array (:length *btree-default-node-size*)
                      :type-number 1
                      :key-code 1               ;integer
                      :res-code 5               ;integer or lisp-pointer
                      ))
    (moby-walk-hierarchy-storing-moby-to-section sdh)
    (dolist (e *moby-section-area-and-region-associations*)
      (si:for-every-region-in-area (region (msa-area e))
        (let ((namespace-page-origin (aref #'sys:region-namespace-origin region)))
          (btree-store-range *moby-page-association*
                             (lsh (si:%region-origin region) -8)
                             namespace-page-origin
                             (+ namespace-page-origin (lsh (si:%region-length region)
                                                           -8))))))))

(defun moby-walk-hierarchy-storing-moby-to-section (section)
  (let ((section-map (mfile-map section)))
    (if section-map
        (moby-store-moby-to-section-for-section-map section section-map))
    (dolist (e (mfile-files section))
      (moby-walk-hierarchy-storing-moby-to-section e))))

(defun moby-store-moby-to-section-for-section-map (section section-map)
  (dotimes (idx (array-active-length section-map))
    (moby-store-moby-to-section-for-region-map section (aref section-map idx))))

(defun moby-store-moby-to-section-for-region-map (section region-map)
  (let ((first-moby-page (rm-namespace-page-origin region-map)))
    (btree-store-range *moby-page-association*
                       section
                       first-moby-page
                       (+ first-moby-page (ash (rm-size region-map) -8)))))

;(cond ((not (memq 'si:resolve-unreconciled-data (g-l-p #'system:support-entry-vector)))
;       (array-push #'system:support-entry-vector 'si:resolve-unreconciled-data)))

;will be in cold-load eventually
;(let ((si:%inhibit-read-only t))
;  (aset 'si:resolve-unreconciled-data #'system:support-entry-vector 16) )
;(let ((si:%inhibit-read-only t))
;  (aset 'si:allocate-new-region-to-moby-area #'system:support-entry-vector 17) )

;structure-handles


;Boxed data can either be reconciled, or left with a DTP-UNRECONCILED datatype
; as desired.  Most of the time, one will probably choose to do the reconcile
; if it can be done simply, otherwise, leave it
; unreconciled if any hair would be involved.

;Unboxed data must be transferred with its header information.  Fortunately,
;transferring it is quite easy and doesnt involve reference to hairy data
;structures, etc.  Note, however, that the entire array involved must be
;copied.

(defun moby-reconcile-object (localp)
 ;reconcile in range delimited by starts-object markers.
 ;<moby-address> is definitely boxed.  If it doesnt start an object,
 ; reconcile downwards until you find something that does.
 ;refinements:
 ; if frob is an dtp-array-header:
 ;   if has array leader, reconcile only leader-length Q and leader-header.
 ;   if is an art-q array, dont reconcile array contents.
 ; if Q is NOT a dtp-array-header or array leader dtp-header
 ;   just reconcile it, not all stuff to object boundaries.
  (prog (msa local-page
         moby-dt region host-index)  ;region-origin moby-bits region-moby-bits-array
      (setq region (%region-number localp)
               ;region-origin (si:%region-origin region)
            local-page (lsh localp -8)
               ;region-moby-bits-array (aref #'sys:region-moby-bits-array region)
            )
      (setq msa (aref *region-to-msa* region))
      (cond ((not (= (%p-ldb-offset %%q-data-type 0 localp)
                     dtp-unreconciled))
             (ferror nil "Data type not unreconciled")))
      (setq host-index (%p-ldb-offset %%q-pointer 0 localp))
      (cond ((null msa)
             (ferror nil "No msa for page ~s" local-page))
            ((not (zerop host-index))   ;we have given this to somebody, get it back.
             (return (moby-reconcile-remote-object localp
                      (moby-get-conn-for-host
                        (aref *moby-host-index* host-index))
                      nil)))
            ((msa-primary-host-object msa)
             (return (moby-reconcile-remote-object localp
                      (moby-get-conn-for-msa msa)
                      nil))))
      (moby-reconcile-q localp nil)
 ;      (setq moby-bits (aref region-moby-bits-array (%pointer-difference localp region-origin)))
      (setq moby-dt (%p-ldb-offset %%q-data-type 0 localp))
      (cond ((= moby-dt dtp-array-header)
             (return (moby-reconcile-object-array-header localp
                                                         ;region-moby-bits-array
                                                         ;region-origin
                                                         )))
            ((and (= moby-dt dtp-header)
                  (= (%p-ldb-offset si:%%header-type-field 0 localp)
                     si:%header-type-array-leader))
             (let* ((delta (%p-ldb-offset si:%%header-rest-field 0 localp))
                    (array-header-localp (%pointer-plus localp delta)))
               (moby-reconcile-q array-header-localp nil)
               (cond ((not (= (%p-ldb-offset %%q-data-type 0 array-header-localp)
                              dtp-array-header))
                      (ferror nil "Array header did not appear where expected")))
               (return (moby-reconcile-object-array-header
                         array-header-localp
                         ;region-moby-bits-array
                         ;region-origin
                         ))))
            (t (return nil))) ))

(defun moby-reconcile-object-via-buffer (localp)
 ;reconcile in range delimited by starts-object markers.
 ;<moby-address> is definitely boxed.  If it doesnt start an object,
 ; reconcile downwards until you find something that does.
 ;refinements:
 ; if frob is an dtp-array-header:
 ;   if has array leader, reconcile only leader-length Q and leader-header.
 ;   if is an art-q array, dont reconcile array contents.
 ; if Q is NOT a dtp-array-header or array leader dtp-header
 ;   just reconcile it, not all stuff to object boundaries.
  (prog (msa local-page vp
         moby-dt region host-index)  ;region-origin moby-bits region-moby-bits-array
      (setq region (%region-number localp)
               ;region-origin (si:%region-origin region)
            local-page (lsh localp -8)
               ;region-moby-bits-array (aref #'sys:region-moby-bits-array region)
            vp (aref #'system:virtual-page-data local-page))
      (if (zerop (ldb si:%%virtual-page-moby-format-handle-and-bits-processed
                    vp))
          (moby-validate-moby-bits localp nil))
  ;is skeleton of object in?  (test if lowest q reconciled).
  ;  yes  ->  reconcile just this q and flush.
  ;  no   ->  fetch lowest q
  ;     this frob an array?
  ;
      (setq msa (aref *region-to-msa* region))
      (cond ((not (= (%p-ldb-offset %%q-data-type 0 localp)
                     dtp-unreconciled))
             (ferror nil "Data type not unreconciled")))
      (setq host-index (%p-ldb-offset %%q-pointer 0 localp))
      (cond ((null msa)
             (ferror nil "No msa for page ~s" local-page))
            ((not (zerop host-index))   ;we have given this to somebody, get it back.
             (return (moby-reconcile-remote-object localp
                      (moby-get-conn-for-host
                        (aref *moby-host-index* host-index))
                      nil)))
            ((msa-primary-host-object msa)
             (return (moby-reconcile-remote-object localp
                      (moby-get-conn-for-msa msa)
                      nil))))
      (moby-reconcile-q localp nil)
 ;      (setq moby-bits (aref region-moby-bits-array (%pointer-difference localp region-origin)))
      (setq moby-dt (%p-ldb-offset %%q-data-type 0 localp))
      (cond ((= moby-dt dtp-array-header)
             (return (moby-reconcile-object-array-header localp
                                                         ;region-moby-bits-array
                                                         ;region-origin
                                                         )))
            ((and (= moby-dt dtp-header)
                  (= (%p-ldb-offset si:%%header-type-field 0 localp)
                     si:%header-type-array-leader))
             (let* ((delta (%p-ldb-offset si:%%header-rest-field 0 localp))
                    (array-header-localp (%pointer-plus localp delta)))
               (moby-reconcile-q array-header-localp nil)
               (cond ((not (= (%p-ldb-offset %%q-data-type 0 array-header-localp)
                              dtp-array-header))
                      (ferror nil "Array header did not appear where expected")))
               (return (moby-reconcile-object-array-header
                         array-header-localp
                         ;region-moby-bits-array
                         ;region-origin
                         ))))
            (t (return nil))) ))

;call this if complete bit not set on local page.
; On return, all moby bits on page below <active-qs-in-page> will be valid.
; The complete bit will be set unless <active-qs-in-page> .ne. #o400 .

;method:
;  validate structure handle.
;  determine number of valid moby-format-moby-bits.
;    hack initial boxed qs
;  scan thru rest using %find-structure-leader, %structure-total-size.

(defun moby-validate-moby-bits-of-localp (localp)
  (let* ((local-page (lsh localp -8))
         (vp (aref #'system:virtual-page-data local-page)))
    (if (not (zerop (ldb si:%%virtual-page-moby-format-handle-and-bits-processed
                         vp)))
        (break "Foo, moby-bits should be valid"))
    (moby-validate-moby-bits localp nil)))


(defun moby-validate-moby-bits (local-address active-qs-in-page)
  ;if moby-format bits not processed, process them.
  ;if local-moby, moby-bit validation works page-wise, if remote-moby,
  ; lisp-object wise.
  (prog (local-page vp-bits region region-origin region-moby-bits-array
         msa remote-host region-free-pointer
         region-relative-local-address page-based-relative-local-address
         region-fresh-cons-boundary page-fresh-conses)
        (setq local-page (lsh local-address -8)
              vp-bits (aref #'system:virtual-page-data local-page)
              region (%region-number local-address)
              region-moby-bits-array (aref #'sys:region-moby-bits-array region)
              region-origin (si:%region-origin region)
              region-free-pointer (si:%region-free-pointer region)      ;***
              region-relative-local-address (%pointer-difference local-address region-origin)
              page-based-relative-local-address (logand region-relative-local-address #o-400)
              msa (aref *region-to-msa* region)
              remote-host (msa-primary-host-object msa))
        (cond ((null active-qs-in-page)
               (setq active-qs-in-page
                     (min #o400 (- region-free-pointer page-based-relative-local-address)))))
        (if (= active-qs-in-page #o400)
            (setf (ldb si:%%virtual-page-moby-bits-complete
                       (aref #'system:virtual-page-data local-page))
                  1))
        (setq region-fresh-cons-boundary (aref *region-fresh-cons-boundary* region)
              page-fresh-conses (cond ((or (null region-fresh-cons-boundary)
                                           (>= (- region-fresh-cons-boundary
                                                  page-based-relative-local-address)
                                               #o400))
                                       0)
                                      ((>= page-based-relative-local-address
                                           region-fresh-cons-boundary)
                                       active-qs-in-page)
                                      (t (max 0  ;during bootstrapping, free pointer not advanced
                                              (min (- region-free-pointer
                                                      region-fresh-cons-boundary)
                                                   active-qs-in-page)))))
    ;;get appropriate bits from moby representation.
        (if (and (zerop (ldb si:%%virtual-page-moby-format-handle-and-bits-processed
                             vp-bits))
                 (> (- active-qs-in-page
                       page-fresh-conses)
                    0))
            (cond ((null remote-host)
                   (moby-transfer-moby-bits-local-moby-to-local
                     local-page (- active-qs-in-page
                                   page-fresh-conses)))
                  (t
                   (cond ((not (zerop page-fresh-conses))
                          (ferror nil "Conses done in page without structure handle")))
                   (break "verify moby bits on remote host")
                   (moby-reconcile-remote-object
                     local-address
                     (moby-get-conn-for-msa msa)
                     nil))))
        (if (> page-fresh-conses 0)
            (if (moby-region-consable? region)
                (moby-validate-moby-bits-freshly-consed local-address active-qs-in-page)))
  ))

(defun moby-transfer-moby-bits-local-moby-to-local (local-page local-limit)
  ;handling of virtual memory buffer is OK since no pointers are ever followwed.
  ;fills in %%virtual-page-initial-qs and %%virtual-page-first-header, and
  ;transfers moby-bits for entire page (or active part thereof).
  ;if page complete, (ie free pointer is past it), set %%virtual-page-moby-bits-complete.
  (let* ((local-base (lsh local-page 8))
         (region (si:%region-number local-base))
         (region-relative-base (%pointer-difference local-base (si:%region-origin region)))
         (region-moby-bits-array (aref #'sys:region-moby-bits-array region)))
    (cond ((null region-moby-bits-array)
           (ferror nil "No moby bits for region ~s" region))
          (t
           (let ((dataspace-page (moby-local-page-to-dataspace-page local-page)))
             (cond ((not (= dataspace-page *moby-cached-dataspace-page*))
                    (setq *moby-cached-vmem-idx*
                          (vmem-find-page dataspace-page)
                          *moby-cached-dataspace-page* dataspace-page)))
             (let* ((buf (aref *vmem-pages* *moby-cached-vmem-idx* 5))
                    (structure-handle (moby-read-structure-handle-from-external-bits buf)))
               (setf (ldb si:%%virtual-page-structure-handle
                          (aref #'system:virtual-page-data local-page))
                     structure-handle)
        ;transfer moby bits for entire page (or until free pointer)
               (do ((moby-idx-of-q3 3 (+ moby-idx-of-q3 4))
                    (local-offset 0 (1+ local-offset)))
                   ((= local-offset local-limit)
                    (setf (ldb si:%%virtual-page-moby-format-handle-and-bits-processed
                              (aref #'system:virtual-page-data local-page))
                         1))
                 (cond ((not (bit-test %moby-region-valid
                                       (aref region-moby-bits-array
                                             (+ region-relative-base local-offset))))
                        (setf (aref region-moby-bits-array
                                    (+ region-relative-base local-offset))
                              (logior %moby-region-valid
                                      (ldb %%moby-internal-bits
                                           (aref buf moby-idx-of-q3)))))))

               ))))))

;Validate a given local-page.
;Moby-format Qs, if any, already validated.
(defun moby-validate-moby-bits-freshly-consed (local-address limit)
  (prog (local-page vp-bits
         region region-origin moby-bits region-moby-bits-array
         region-relative-base offset
         boxed-count unboxed-count)
        (setq local-page (lsh local-address -8)
              local-address (lsh local-page 8)
              vp-bits (aref #'system:virtual-page-data local-page)
              offset 0)
  ;;Handle had better be valid by now.  Presumably was filled in by system.
  ;;would be nice to error check this, but unfortunately initial value can also
  ;;be final value.
        (setf (aref #'system:virtual-page-data local-page)
              (dpb 1 si:%%virtual-page-moby-format-handle-and-bits-processed
                   vp-bits))
        (let ((initial-qs (ldb si:%%virtual-page-initial-qs vp-bits))
              (first-header (ldb si:%%virtual-page-first-header vp-bits)))
          (setq boxed-count initial-qs
                unboxed-count (- first-header initial-qs)))  ;this does the right
                        ;thing if there is or is not a first-header.
        (setq region (%region-number local-address)
              region-moby-bits-array (aref #'sys:region-moby-bits-array region)
              region-origin (si:%region-origin region)
              region-relative-base (%pointer-difference local-address region-origin))
        (if (null region-moby-bits-array)
            (ferror nil "No moby bits for region ~s" region))
  ;we use a separate loop to handle the overflow object, if any.
 o-boxed(cond ((>= offset limit)
               (return t))
              ((zerop boxed-count)
               (go o-unboxed)))
        (setq moby-bits (aref region-moby-bits-array (+ region-relative-base offset)))
        (cond ((not (bit-test %moby-region-valid moby-bits))
               (setf (aref region-moby-bits-array (+ region-relative-base offset))
                     (dpb 0 %%%moby-starts-object
                          (dpb 1 %%%moby-boxed %moby-region-valid)))))
        (setq offset (1+ offset)
              boxed-count (1- boxed-count))
        (go o-boxed)
 o-unboxed
        (cond ((>= offset limit)
               (return t))
              ((zerop unboxed-count)
               (go l-header)))
        (setq moby-bits (aref region-moby-bits-array (+ region-relative-base offset)))
        (cond ((not (bit-test %moby-region-valid moby-bits))
               (setf (aref region-moby-bits-array (+ region-relative-base offset))
                     (dpb 0 %%%moby-starts-object
                          (dpb 0 %%%moby-boxed (logior %moby-region-valid
                                                       %moby-region-reconciled-p))))))
        (setq offset (1+ offset)
              unboxed-count (1- unboxed-count))
        (go o-unboxed)
 l-header
        (cond ((>= offset limit)
               (return t)))
        (setq moby-bits (aref region-moby-bits-array (+ region-relative-base offset)))
        (cond ((bit-test %moby-region-valid moby-bits)   ;bits valid, this guy may not
                ;be freshly consed, and could contain dtp-unreconciled.  So dont touch him
                ;with %structure-total-size, etc, which could bomb and would call for the
                ;data unnecessarily anyway.
               (setq offset (1+ offset))
                ;scan until encounter moby-bits needing to be set up.  This can only
                ;occur at a header boundary.
               (go l-header))
              (t
               (let* ((localp (%pointer-plus local-address offset))
                      (total-size (%structure-total-size localp))
                      (boxed-size (%structure-boxed-size localp)))
                 (setq boxed-count boxed-size
                       unboxed-count (- total-size boxed-size)))
               (setf (aref region-moby-bits-array (+ region-relative-base offset))
                     (dpb 1 %%%moby-starts-object
                          (dpb 1 %%%moby-boxed %moby-region-valid)))))
        (setq offset (1+ offset)
              boxed-count (1- boxed-count))
 l-boxed(cond ((>= offset limit)
               (return t))
              ((zerop boxed-count)
               (go l-unboxed)))
        (setq moby-bits (aref region-moby-bits-array (+ region-relative-base offset)))
        (cond ((not (bit-test %moby-region-valid moby-bits))
               (setf (aref region-moby-bits-array (+ region-relative-base offset))
                     (dpb 0 %%%moby-starts-object
                          (dpb 1 %%%moby-boxed %moby-region-valid)))))
        (setq offset (1+ offset)
              boxed-count (1- boxed-count))
        (go l-boxed)
 l-unboxed
        (cond ((>= offset limit)
               (return t))
              ((zerop unboxed-count)
               (go l-header)))
        (setq moby-bits (aref region-moby-bits-array (+ region-relative-base offset)))
        (cond ((not (bit-test %moby-region-valid moby-bits))
               (setf (aref region-moby-bits-array (+ region-relative-base offset))
                     (dpb 0 %%%moby-starts-object
                          (dpb 0 %%%moby-boxed (logior %moby-region-valid
                                                       %moby-region-reconciled-p))))))
        (setq offset (1+ offset)
              unboxed-count (1- unboxed-count))
        (go l-unboxed)  ))

;assure moby-bits valid for all qs of object including local-address.
; possibilities:
;       freshly consed.
;  if storage handle is present on page, moby-bits have been swapped in
;
;       unreconciled - local
;       unreconciled - remote

;validation of moby bits:
; local moby pages:
;   all valid moby bits from moby page are tranferred when skeleton transferred.
;   after skeleton has been xferred, all moby bits must be valid except for
;      freshly consed stuff (which could be done by the local machine or
;      remote ones).
;   breaks in moby bit validity can occur at page boundaries, or
;   object boundaries (in the case of newly consed stuff).
;  - general plan is to touch moby page (if necessary), then test bits.
;    then if bits are not valid, stuff must be newly consed, so fill in bits.
; remote moby pages:
;   moby-bits are transferred with qs across net.  All validations are
;   object-wise, so any break must be at an object boundary.

;(defun moby-validate-moby-bits-of-local-object (local-address)
;  (prog (localp local-page vp-bits
;        region region-origin moby-bits region-moby-bits-array
;        remote-host region-free-pointer region-relative-localp
;        total-size boxed-size boxed-count unboxed-count)
;       (setq localp (%find-structure-leader local-address)
;             total-size (%structure-total-size localp)
;             boxed-size (%structure-boxed-size localp)
;             region (%region-number localp)
;             region-moby-bits-array (aref #'sys:region-moby-bits-array region)
;             region-origin (si:%region-origin region)
;             region-free-pointer (si:%region-free-pointer region)
;             region-relative-localp (%pointer-difference localp region-origin))
;       (setq boxed-count boxed-size
;             unboxed-count (= total-size boxed-size))
;       (if (null region-moby-bits-array)
;           (ferror nil "No moby bits for region ~s" region))
;        (setq local-page (lsh localp -8)
;             vp-bits (aref #'system:virtual-page-data local-page))
;       (if (zerop (ldb si:%%virtual-page-moby-format-handle-and-bits-processed
;                       vp-bits))
;           (moby-transfer-skeleton-local-moby-to-local local-page))
;       (setq moby-bits (aref region-moby-bits-array region-relative-localp))
;       (cond ((bit-test %moby-region-valid moby-bits)
;              (go old-stuff))) ;not newly consed
;   ;fill in bits for newly consed object.
;       (setf (aref region-moby-bits-array region-relative-localp)
;             (dpb 1 %%%moby-starts-object
;                  (dpb 1 %%%moby-boxed %moby-region-valid)))
;  b-l  (setq localp (%pointer-plus localp 1)
;             region-relative-localp (1+ region-relative-localp))
;       (cond ((zerop (setq boxed-count (1- boxed-count)))
;              (go ub-l)))
;       (setf (aref region-moby-bits-array region-relative-localp)
;             (dpb 0 %%%moby-starts-object
;                  (dpb 1 %%%moby-boxed %moby-region-valid)))
;       (go b-l)
;  ub-l  (cond ((zerop (setq unboxed-count (1- unboxed-count)))
;              (go exit)))
;       (setf (aref region-moby-bits-array region-relative-localp)
;             (dpb 0 %%%moby-starts-object
;                  (dpb 0 %%%moby-boxed %moby-region-valid)))
;       (setq localp (%pointer-plus localp 1)
;             region-relative-localp (1+ region-relative-localp))
;       (go ub-l)

;  old-stuff
;       (cond ((zerop (ldb %%%moby-starts-object moby-bits))
;              (ferror nil "Moby starts object not set when expected")))

; scan-up(setq localp (%pointer-plus localp 1)
;             region-relative-localp (1+ region-relative-localp))
;  l-up  (cond ((zerop (logand 377 localp))
;              (setq local-page (lsh localp -8)
;                    vp-bits (aref #'system:virtual-page-data local-page))
;              (if (zerop (ldb si:%%virtual-page-moby-format-handle-and-bits-processed
;                              vp-bits))
;                  (moby-transfer-skeleton-local-moby-to-local local-page))))
;       (setq moby-bits (aref region-moby-bits-array region-relative-localp))
;       (cond ((not (bit-test %moby-region-valid moby-bits))
;              (ferror nil "moby-bits not valid"))
;             ((not (zerop boxed-count))
;              (cond ((zerop (ldb %%%moby-boxed moby-bits))
;                     (ferror nil "Moby-bits say unboxed where boxed expected")))
;              (setq boxed-count (1- boxed-count)))
;             ((not (zerop unboxed-count))
;              (cond ((not (zerop (ldb %%%moby-boxed moby-bits)))
;                     (ferror nil "Moby-bits say boxed where unboxed expected")))
;              (setq unboxed-count (1- unboxed-count)))
;             (t (ferror nil "Have not encountered moby object boundary at end of object")))
;       (cond ((zerop (ldb %%%moby-starts-object moby-bits))
;              (setq localp (%pointer-plus localp 1)
;                    region-relative-localp (+ region-relative-localp 1))
;              (cond ((< region-relative-localp region-free-pointer)
;                     (go l-up)))))
;  exit  (return t)
;       ))




;  dn1 (setq localp (%pointer-difference localp 1))
;      (cond ((compiler:%pointer-lessp localp region-origin)
;            (ferror nil "Attempted to scan below region boundary")))
;      (setq moby-bits (moby-reconcile-q localp))
;      (cond ((not (= 1 (ldb %%%moby-starts-object moby-bits)))
;            (go dn1)))
;  up0 (setq localp (%pointer-plus local-address 1))
;  up  (cond ((not (compiler:%pointer-lessp localp region-hi))
;            (return nil)))
;      (setq moby-bits (moby-reconcile-q localp))
;      (cond ((not (= 1 (ldb %%%moby-starts-object moby-bits)))
;            (setq localp (%pointer-plus localp 1))
;            (go up))
;           (t (return nil)))


;call here with local pointer to array header.  This reconciles additional stuff as
; necessary.  Basically, do enuf stuff to assure %structure-total-size and array-active-length
; will work, no unreconciled traps will be taken out of the guts of array referencing
; (ie dimension multiliers), plus the whole contents of the array if it is unboxed.
(defun moby-reconcile-object-array-header (array-header-localp)
  (prog (localp localp-1 region region-hi region-relative-offset local-moby-bits
         region-moby-bits-array region-origin)
        (setq region (%region-number array-header-localp)
              region-origin (si:%region-origin region)
              region-moby-bits-array (aref #'sys:region-moby-bits-array region))
        (setq localp array-header-localp)
        (cond ((not (zerop (%p-ldb-offset si:%%array-long-length-flag 0 localp)))
               (setq localp (%pointer-plus localp 1))
               (moby-reconcile-q localp nil)))
                ;reconcile array multipliers, if any.
        (dotimes (c (1- (%p-ldb-offset si:%%array-number-dimensions 0 array-header-localp)))
          (setq localp (%pointer-plus localp 1))
          (moby-reconcile-q localp nil))
        (cond ((not (zerop (%p-ldb-offset si:%%array-leader-bit 0 array-header-localp)))
               (if (= 1 (ldb %%%moby-starts-object
                             (aref region-moby-bits-array
                                   (%pointer-difference array-header-localp region-origin))))
                   (ferror nil "bad moby bits, array with leader starts object."))
             ;reconcile array-leader length Q
               (setq localp-1 (%pointer-difference array-header-localp 1))
               (moby-reconcile-q localp-1 nil)
               (let* ((leader-length (%p-ldb-offset %%q-pointer 0 localp-1))
                      (leader-delta (1+ leader-length)))
             ;reconcile array-leader 0 so ARRAY-ACTIVE-LENGTH works.
                 (moby-reconcile-q (%pointer-difference localp-1 1) nil)
             ;reconcile array-leader-header.
                 (moby-reconcile-q (%pointer-difference localp-1 leader-delta) nil))))
             ;for an unboxed array, we must reconcile the whole thing now.
        (cond ((memq (%p-ldb-offset si:%%array-type-field 0 array-header-localp)
                     '(#.(find-position-in-list 'art-q array-types)
                       #.(find-position-in-list 'art-q-list array-types)))
               (return nil)))    ;array boxed, OK.
   ;reconcile all unboxed stuff.
        (setq region-hi (%pointer-plus region-origin (si:%region-free-pointer region)))
    up  (setq localp (%pointer-plus localp 1))
        (cond ((not (compiler:%pointer-lessp localp region-hi))
               (return nil)))
        (setq region-relative-offset (%pointer-difference localp region-origin))
    up0 (setq local-moby-bits (aref region-moby-bits-array region-relative-offset))
        (cond ((zerop (logand %moby-region-valid local-moby-bits))
               (moby-validate-moby-bits-of-localp localp)
               (go up0)))
        (cond ((not (= 1 (ldb %%%moby-starts-object local-moby-bits)))
               (setf (aref region-moby-bits-array region-relative-offset)
                     (logior %moby-region-reconciled-p local-moby-bits))
               (moby-reconcile-q localp t)
               (go up))
              (t (return nil))) ))

;local-address non-nil, pkt nil means a (user) data-request.
;local-address nil, pkt supplied means a (server) data-give.
(defun moby-reconcile-remote-object (local-address conn pkt)
  (prog (sub-seq moby-p localp local-page
         region region-origin region-moby-bits-array seq-num)
        (cond (local-address
               (setq region (%region-number local-address)
                     region-origin (si:%region-origin region)
                     region-moby-bits-array (aref #'sys:region-moby-bits-array region)
                     seq-num (moby-send-pkt-request-data conn
                              %moby-pkt-request-data 0
                              (local-to-moby-correspondance-or-error local-address))))
              (t (setq seq-num (mpkt-seq pkt))))
  ;the request must be entirely in a single region. However, each packet
  ; may be based on a different page, the first of which is not necessarily
  ; the same page as local-address.  A packet may not straddle a page boundary.
        (setq sub-seq 0)
        (cond (pkt (go l0)))
    l   (setq pkt (chaos:get-next-pkt conn))
    l0  (cond ((not (= seq-num (mpkt-seq pkt)))
               (ferror nil "Response has wrong seq number (~s), should be ~s"
                       (mpkt-seq pkt) seq-num))
              ((not (= sub-seq (mpkt-sub-seq pkt)))
               (ferror nil "Response has wrong sub-seq number (~s), should be ~s"
                       (mpkt-sub-seq pkt) sub-seq)))
        (setq moby-p (moby-base-from-data-response pkt)
              localp (moby-to-local-correspondance moby-p)
              local-page (lsh localp -8))
        (cond ((null region)
               (setq region (%region-number localp)
                     region-origin (si:%region-origin region)
                     region-moby-bits-array (aref #'sys:region-moby-bits-array region))))
        (let ((structure-handle
                (dpb (mpkt-structure-handle-hi pkt)
                     (byte 16. 16.)
                     (mpkt-structure-handle-lo pkt))))
          (cond ((zerop (ldb si:%%virtual-page-moby-format-handle-and-bits-processed
                             (aref #'system:virtual-page-data local-page)))
                 (setf (ldb si:%%virtual-page-structure-handle
                            (aref #'system:virtual-page-data local-page))
                       structure-handle)
                 (setf (ldb si:%%virtual-page-moby-format-handle-and-bits-processed
                            (aref #'system:virtual-page-data local-page))
                       1))
                (t
                 (cond ((not (= (ldb si:%%virtual-page-structure-handle
                                     (aref #'system:virtual-page-data local-page))
                                structure-handle))
                        (ferror nil "Structure handle differs for page ~s" local-page))))))
        (dotimes (c (mpkt-q-count pkt))
          (multiple-value-bind (q0 q1 q2 q3)
              (moby-fetch-quarters-from-pkt pkt c)
            (let ((moby-bits (logior %moby-region-valid
                                     %moby-region-reconciled-p
                                     (ldb %%moby-internal-bits q3)))
                  (region-relative-adr (%pointer-difference localp region-origin)))
              (multiple-value-bind (format-symbol v0 v1)
                  (moby-parse-components q0 q1 q2 q3)
                (let ((ombits (aref region-moby-bits-array region-relative-adr)))
                  (cond ((zerop (logand ombits %moby-region-valid))
                         (setf (aref region-moby-bits-array region-relative-adr)
                               moby-bits))
                        (t (cond ((not (= (logand #o37 moby-bits)
                                          (logand #o37 ombits)))
                                  (ferror "moby bits differ"))))))
                (moby-reconcile-q-internal localp format-symbol v0 v1 'unknown)
                (setq localp (%pointer-plus localp 1))))))
        (cond ((not (zerop (mpkt-last-flag pkt)))
               (chaos:return-pkt pkt)
               (return nil))
              (t
               (chaos:return-pkt pkt)
               (setq sub-seq (1+ sub-seq))
               (go l)))
    ))

(defun moby-reconcile-q (localp unboxed)
  (let* ((local-page (lsh localp -8))
         (vp (aref #'system:virtual-page-data local-page)))
    (if (zerop (ldb si:%%virtual-page-moby-format-handle-and-bits-processed
                    vp))
        (moby-validate-moby-bits localp nil))
    (multiple-value-bind (q0 q1 q2 q3)
        (moby-fetch-quarters localp)
      (multiple-value-bind (format-symbol v0 v1)
          (moby-parse-components q0 q1 q2 q3)
        (moby-reconcile-q-internal localp format-symbol v0 v1 unboxed)))
  ;reconcile does not count as a write, so restore state of clean bit.
  ;*** this could lose if incidental writes happen during reconcile (ie flavor hacking, etc.)
    (setf (ldb si:%%virtual-page-clean (aref #'system:virtual-page-data local-page))
          (ldb si:%%virtual-page-clean vp))))

(defun moby-reconcile-q-internal
       (localp format-symbol v0 v1 unboxed &aux local-dt)
  (cond ((eq format-symbol 'unboxed)
  ;if unboxed, clobber it.
         (cond ((null unboxed)
                (ferror nil "Unboxed in moby space where boxed expected")))
         (%p-dpb-offset v0 (byte 16. 0) 0 localp)
         (%p-dpb-offset v1 (byte 16. 16.) 0 localp))
        ((and unboxed (not (eq unboxed 'unknown)))
         (ferror nil "Boxed in moby space where unboxed expected"))
        (t
         (setq local-dt (%p-ldb-offset %%q-data-type 0 localp))
  ;clobber only unreconciled data.
         (cond ((= local-dt dtp-unreconciled)
                (%p-dpb-offset dtp-fix %%q-data-type 0 localp)  ;smash the unreconciled
                        ;guy to a fixnum.  This avoids problems which trying to store
                        ;over it.  Must use a pointer type store below so volatilities
                        ;get updated correctly.
                (selectq format-symbol
                  (pointer
                   (let ((moby-dt (ldb (byte 5 0) v1)))
                     (cond ((not (memq moby-dt '(#.dtp-array-pointer #.dtp-symbol #.dtp-list
                                                 #.dtp-instance #.dtp-instance-header
                                                 #.dtp-header-forward #.dtp-body-forward
                                                 #.dtp-moby-eval #.dtp-unreconciled)))
                            (ferror nil "The data type ~s should not appear in moby space"
                                    moby-dt)))
                     (prog (local-pointer)
                        l  (setq local-pointer (moby-to-local-correspondance v0))
                           (cond ((null local-pointer)
                                  (moby-map-section-of-moby-pointer v0 localp)
                                  (go l))
                                 ((and (= moby-dt dtp-symbol)
                                       (not (zerop local-pointer))) ;NIL falls into case below,
                                                                    ;which does the right thing.
                                  ;(format t "~%symbol ~s" v0)
                                  (let ((local-dt
                                          (%p-ldb-offset %%q-data-type 0 local-pointer)))
                                    (cond ((or (= local-dt dtp-array-header)
                                               (= local-dt dtp-unreconciled))
                                           ;it had better turn into the right thing ...
                                           (let* ((sym-defstruct
                                                    (%make-pointer dtp-array-pointer
                                                                   local-pointer))
                                                  (sym
                                                    (cond ((eq local-pointer
                                                               (%pointer-difference localp 3))
                                                           (intern (aref sym-defstruct 0)
                                                                   (pkg-find-package
                                                                     (moby-package-name
                                                                       (aref sym-defstruct 1)))))
                                                          (t
                                                           (aref sym-defstruct 2)))))
                                             (cond ((not (symbolp sym))
                                                    (break "lose sym should be symbol")))
                                             (%p-dpb-offset (ldb (byte 2 5) v1) ;store cdr-code.
                                                            (byte 2 30.) 0 localp)
                                                ;used typed store so volatilities updated.
                                             (%p-store-contents-offset sym 0 localp)))
                                          (t (ferror nil "Bad moby symbol pointer ~s"
                                                     local-dt)))))
                                 ((= moby-dt dtp-instance-header)
                                  (let* ((moby-instance-description-defstruct
                                           (%make-pointer dtp-array-pointer local-pointer))
                                         (moby-all-instance-variables
                                           (moby-all-instance-variables
                                             moby-instance-description-defstruct))
                                         (instance-symbol
                                           (moby-instance-symbol
                                             moby-instance-description-defstruct))
                                         (instance-defstruct (get instance-symbol 'si:flavor))
                                         local-all-instance-variables)
                                    (or (si:flavor-depends-on-all instance-defstruct)
                                        ;**  depends on defstruct ordering.
                                        (si:compose-flavor-combination instance-defstruct))
                                    (or (si:flavor-method-hash-array instance-defstruct) ;**
                                        (si:compose-method-combination instance-defstruct))
                                      ;below might not be set up before now.
                                    (setq local-all-instance-variables
                                          (si:flavor-all-instance-variables-slow
                                            instance-defstruct))
                                    (cond ((not (equal moby-all-instance-variables
                                                       local-all-instance-variables))
                                           (ferror nil "Instance variables for flavor ~s not compatible with moby version"
                                                   instance-symbol)))
                                    (%p-dpb-offset (ldb (byte 2 5) v1)  ;store cdr-code.
                                                   (byte 2 30.) 0 localp)
                                           ;used typed store so volatilities updated.
                                    (%p-store-contents-offset instance-defstruct 0 localp)
                                    (%p-dpb-offset dtp-instance-header
                                                   (byte 5 25.)
                                                   0
                                                   localp)))
                                 ((memq moby-dt
                                        '(dtp-instance-header dtp-header-forward
                                                              dtp-body-forward))
                        ;not safe to "pick up" these, so make locative first.
                                  (%p-dpb-offset (ldb (byte 2 5) v1)  ;store cdr-code.
                                                 (byte 2 30.) 0 localp)
                                  ;used typed store so volatilities updated.
                                  (%p-store-contents-offset
                                    (%make-pointer dtp-locative local-pointer) 0 localp)
                                  ;then clobber correct data type.
                                  (%p-dpb-offset moby-dt (byte 5 25.) 0 localp))
                                 ((= moby-dt dtp-moby-eval)
                                  (%p-dpb-offset (ldb (byte 2 5) v1)  ;store cdr-code.
                                                 (byte 2 30.) 0 localp)
                                  (let* ((reconcile-expression
                                           (%make-pointer dtp-list local-pointer))
                                         (ans (eval reconcile-expression))
                                         (area (%area-number localp))
                                         (msa (aref *area-to-msa* area))
                                         (map-moby-handle (msa-map-moby-handle msa)))
                ;it is possible for this frob to have already been dereconciled, in which
                ; case we might as well leave it alone.  This could result in multiple copies
                ;of the reconcile-expression being stored, but so what.  Usually one will get
                ;touched before any get dereconciled.
                                    (cond ((get-equal ans map-moby-handle))
                ;use this same reconcile-expression if we later want to dereconcile this.
                ; (v0 is moby correspondance of local-pointer)
                                          (t (putprop ans v0 map-moby-handle)))
                                    (%p-store-contents-offset ans 0 localp)))
                                 ((= moby-dt dtp-unreconciled)
                ;smash back to unreconciled.  Can happen with data recvd from foreign
                ; machine when stuff has not been gotten from local disk.
                                  (%p-dpb-offset dtp-unreconciled
                                                 %%q-data-type
                                                 0
                                                 localp))
                                 (t
                                  (%p-dpb-offset (ldb (byte 2 5) v1)  ;store cdr-code.
                                                 (byte 2 30.) 0 localp)
                                  ;used typed store so volatilities updated.
                                  ;OK to pickup and store final thing.  This avoids
                                  ;faking out error checking in ucode, for one thing.
                                  (%p-store-contents-offset
                                    (%make-pointer moby-dt local-pointer) 0 localp))))))
                  (direct
                   (let ((moby-dt (ldb (byte 5 0) v1)))
                                                ;just for checking...
                     (cond ((not (memq moby-dt '(#.dtp-small-flonum #.dtp-character
                                                 #.dtp-array-header #.dtp-header)))
                            (ferror nil "The data type ~s should not appear in moby space"
                                    moby-dt)))
                     (%p-dpb-offset (%logldb (byte 9 16.) v0) (byte 9 16.) 0 localp)
                     (%p-dpb-offset v0 (byte 16. 0) 0 localp)
                     (%p-dpb-offset v1 (byte 7 25.) 0 localp)))
                  (fixnum
                   (%p-dpb-offset (ldb (byte 2 5) v1)  ;store cdr-code.
                                  (byte 2 30.) 0 localp)
                   ;used typed store so volatilities updated (if bignum).
                   (%p-store-contents-offset v0 0 localp))
                  (local-cell
                   (%p-dpb-offset (ldb (byte 2 5) v1)  ;store cdr-code.
                                  (byte 2 30.) 0 localp)
                   (%p-store-contents-offset nil 0 localp))
                  (otherwise
                   (ferror nil "??")))))))
  (cond ((and (not (eq format-symbol 'unboxed))
              (zerop (%p-ldb-offset (byte 5 25.) 0 localp)))
         (si:%halt))))

(defun moby-map-section-of-moby-pointer (moby-pointer localp-where-found)
  (prog (section region msa)
        (setq section (btree-lookup *moby-page-association*
                                    (ash moby-pointer -8)))
        (cond (section
               (tv:notify tv:selected-window
                          "Mapping section ~S for input"
                          (mfile-full-name section))
               (moby-map-section
                 (partition-host-of-section section)
                 section
                 :input
                 nil)                           ;options doesnt matter on input.
               (return t))
              (localp-where-found
           ;if this a remote section, ask that host for mapping
               (setq region (%region-number localp-where-found)
                     msa (aref *region-to-msa* region))
               (cond ((null (msa-primary-host-object msa)))     ;drop thru to error
                     (t
                      (let ((response-pkt
                              (moby-send-pkt-command-for-msa
                                msa %moby-pkt-request-region-mapping moby-pointer)))
                        (cond ((null (moby-allocate-region-to-area-from-response-region-mapping
                                       response-pkt nil))
                               (ferror nil "Negative response to allocate region to area"))
                              (t (return t))))))))
        (ferror nil "No section correspondance for ~s"
                moby-pointer)))

(defun si:allocate-new-region-to-moby-area (area-number size-being-consed volatility)
  size-being-consed volatility
  (let* ((default-cons-area working-storage-area)  ;make sure we dont add our cruft
                ;to his moby region.
         (msa (aref *area-to-msa* area-number))
         (map (msa-map msa)))
    (cond ((null (msa-primary-host-object msa))
           (let* ((host-index (msa-host-index msa))
                  (rb (aref #'sys:area-region-bits area-number))
                  (region-size-in-pages #o100)
  ;need to create region first and pass it to ALLOCATE-REGION-TO-MOBY-AREA
  ; to avoid infinite loop when dealing with ROOT-AREA, for example.
  ; --no should be OK, region map table for root-area is in root-mapping-area, etc.
;                 (region (compiler:%make-region area-number
;                                       ;always consable.
;                                                (%logdpb si:%region-space-moby-new
;                                                         sys:%%region-space-type rb)
;                                                (ash region-size-in-pages 8)))
                  (region-map
                   (moby-add-region-to-map-and-allocate-namespace
                     map msa region-size-in-pages (msa-partition-header msa))))
             (allocate-region-to-moby-area area-number nil region-map  ;NIL was region.
                                           host-index)))
          (t (let ((response-pkt
                     (moby-send-pkt-command-for-msa
                       msa %moby-pkt-request-consing-region (msa-map-moby-handle msa))))
               (cond ((null (moby-allocate-region-to-area-from-response-region-mapping
                              response-pkt area-number))
                      (ferror nil "Negative response to request for consing region")))
               (chaos:return-pkt response-pkt)
               nil)))))

(defun moby-allocate-region-to-area-from-response-region-mapping (pkt area-number)
  ;value T if positive response and work done, NIL if negative response and nothing done.
  ;if area nil, consider all known moby mapped areas, use the one, if any, for which
  ;map-moby-handle matches.
  (prog top (consable? map-moby-handle namespace-page-origin size-in-qs free-pointer)
        (setq consable? (if (zerop (logand 1_14. (mpkt-arg pkt))) nil t)
              map-moby-handle (get-moby-address-from-pkt pkt 0)
              namespace-page-origin (get-moby-address-from-pkt pkt 1)
              size-in-qs  (get-moby-address-from-pkt pkt 2)
              free-pointer (get-moby-address-from-pkt pkt 3))
        (cond ((zerop (logand (mpkt-arg pkt) 1_13.))
               (return nil))                            ;negative response
              ((null area-number)                       ;area unknown
               (dolist (msa *moby-section-area-and-region-associations*
                            (ferror "Cant find handle ~s." map-moby-handle)
                            ;(return-from top nil)
                            )
                 (cond ((= map-moby-handle (msa-map-moby-handle msa))
                        (return (setq area-number (msa-area msa))))))))
        (let* ((msa (aref *area-to-msa* area-number))
               (map (msa-map msa)))
          (cond ((not (= map-moby-handle (msa-map-moby-handle msa)))
                 (ferror nil "Moby-handle of map mismatch"))
                (t
                 (let ((region-map (moby-add-region-to-map map msa
                                    namespace-page-origin (ash size-in-qs -8))))
                   (setf (rm-free-pointer region-map) free-pointer)
                   (allocate-region-to-moby-area area-number
                                                 nil
                                                 region-map
                                                 (if consable?
                                                     (msa-host-index msa)))
                   (return t)))))))

(defun moby-region-free-pointer (region &optional update-page-clean-bits)
  (cond ((moby-region-consable? region)
         (si:%region-free-pointer region))
        (t
         (let* ((region-map (aref *region-to-region-map* region))
                (consing-host-index (rm-consing-host region-map)))
           (cond ((null consing-host-index)
                  (rm-free-pointer region-map))
                 (update-page-clean-bits
                  (moby-remote-free-pointer-and-update-page-clean
                    region consing-host-index))
                 (t
                  (let* ((response-pkt (moby-send-pkt-command
                                         (moby-get-conn-for-host-index consing-host-index)
                                         %moby-pkt-request-free-pointer
                                         (aref #'sys:region-namespace-origin region)))
                         (response (moby-base-from-data-response response-pkt)))
                    (cond ((> response 1_25.)
                           (ferror nil "Negative response to request free pointer"))
                          (t (chaos:return-pkt response-pkt)
                             ;(setf (rm-free-pointer region-map) response)
                             response)))))))))

(defun moby-remote-free-pointer-and-update-page-clean (region host-index)
  ;get free pointer from remote host and merge in clean bits.
  (prog (region-map local-free-pointer conn response-pkt seq-num sub-seq
         remote-free-pointer region-first-page relative-page-base relative-page-limit)
        (setq sub-seq 0
              region-map (aref *region-to-region-map* region)
              local-free-pointer (si:%region-free-pointer region)
              relative-page-base 0
              relative-page-limit (lsh local-free-pointer -8)
              region-first-page (lsh (si:%region-origin region) -8))
        (setq seq-num
              (moby-send-pkt-request-clean-bits
                (setq conn
                      (moby-get-conn-for-host-index host-index))
                %moby-pkt-request-clean-bits
                1               ;request bits.
                (aref #'sys:region-namespace-origin region)
                local-free-pointer))
   next-pkt
        (setq response-pkt (chaos:get-next-pkt conn))
        (cond ((not (= seq-num (mpkt-seq response-pkt)))
               (ferror nil "Response has wrong seq number (~s), should be ~s"
                       (mpkt-seq response-pkt) seq-num))
              ((not (= sub-seq (mpkt-sub-seq response-pkt)))
               (ferror nil "Response has wrong sub-seq number (~s), should be ~s"
                       (mpkt-sub-seq response-pkt) sub-seq)))
        (if (null remote-free-pointer)
            (setq remote-free-pointer (dpb (mpkt-structure-handle-hi response-pkt)
                                           (byte 16. 16.)
                                           (mpkt-structure-handle-lo response-pkt))
                  relative-page-limit (min relative-page-limit
                                           (ash remote-free-pointer -8))))
        (let ((limit-this-pkt (min relative-page-limit
                                   (+ relative-page-base *max-clean-bits-in-pkt*))))
          (do ((c 0 (1+ c)))
              ((= c limit-this-pkt))
            (if (not (zerop (ldb si:%%virtual-page-clean
                                 (aref #'system:virtual-page-data
                                       (+ relative-page-base
                                          region-first-page)))))
                (if (zerop (moby-get-clean-bit-from-pkt response-pkt c))
                    (setf (ldb si:%%virtual-page-clean
                               (aref #'system:virtual-page-data (+ relative-page-base
                                                                   region-first-page)))
                          0)))
            (incf relative-page-base)))
        (cond ((zerop (mpkt-last-flag response-pkt))
               (chaos:return-pkt response-pkt)
               (incf sub-seq)
               (go next-pkt)))
        (chaos:return-pkt response-pkt)
        (return remote-free-pointer)
  ))

;SAFEing moby structures, particularily section data maps.

;Safeing a structure is the operation of assuring that structure has physically been
; written on the disk in a consistant form.   "Checkpointing" is the operation of
; snapshotting a structure which has been SAFEd, such that it is possible to
; return the structure to that state in case of a subsequent system crash.

;Retention and Integrity are related but distinct concepts.
; Retention retainning user data itself, plus information (usually contained in system
;  directories) as to where the user data is stored on disk.
; Integrity is a more global property.  This means, when a crash and system restore
;  is done, the entirity of the data base is restored to SOME checkpointed state,
;  BUT NOT to any intermediate between-checkpoints state.  In conventional file systems,
;  integrity is sometimes a fuzzy issue since files cannot contain pointers, etc.
;  However, in the MOBY file system, the issue is brought into sharp focus since
;  lack of integrity could result pointers to invalid data structure, etc.

;The moby file system is intended to raise the reliablity of computer data storage
; to a new high.  To this end, various data structures are designed with a high
; degree of built in redundancy and robustness.  In particular, we assure retention
; of SAFEd data despite any combination of:
;  (1)  A crash of the system at any moment.
;  (2)  The loss of any single disk sector, not part of the data itself.
;       (In other words, the loss of any single directory page or other page
;        containg system data of any sort).  Such a loss could occur, for example,
;       if a power failure were to occur just as the section in question was being
;       written to the disk.
;     (-- actually, a very recently written "single safed" file could be lost
;     if it was necessary to revert its directory to the backup version.  After a short
;     period, another checkpoint would be taken and the file would be "double-safed"
;     since it would appear in the backup version as well as the prime version.)

;To meet this goal, we need at least the following subgoals.
;   (1) The system data structure must exhibit sufficient static robustness and
;       redundancy.
;   (2) There must be adequate proceedures to assure things get written out
;       in an consistant enough state so they can be reconstructed, if necessary,
;       by a salvager.

;Meeting these goals, combined with the multisystem-level redundancy possible with
; the moby-address system, raises reliability and availability levels to a new high.

;The dataspace map is the primary system data structure of concern will dealing with
;issue of retention and integrity.  All other system structures can be reconstructed
;if a consistant set to dataspace maps can be recovered.

;Checkpointing large structures.
;  Checkpointing a structure inheritly involves some sort of locking or denial
;of access to that structure for a period of time.  Since inheritly, checkpointing
;is a structure-wide operation, we may expect the time involved to be some function
;of the size of the structure.  There is great concern, that, as structures grow
;larger there is a pointer where the denial of access associated with checkpointing
;will become unacceptable.

;A checkpoint in the MOBY FILE system is a dataspace-map.  A dataspace map
;"guarantees" its contents, in the sense that none of its disk pages can be written
;as long as the dataspace map itself exists (because their dataspace usage count
;must be at least 2, one due to the dataspace map in question and one due to any
;other map which would overwrite it.)

eh:(def-ucode-error illegal-moby-write illegal-moby-write-error
     :location-modified (second ete)
     :data-stored (third ete)
     :format-string "An illegal object was stored into moby space.")

eh:(defflavor illegal-moby-write-error (location-modified data-stored)
              (error)
     :gettable-instance-variables
     :initable-instance-variables)

eh:(defmethod (illegal-moby-write-error :ucode-proceed-types) ()
     '())

eh:(def-ucode-error new-region-needed-for-moby-area new-region-needed-for-moby-area-error
     :area (sg-ac-s sg)
     :restart-tag (third ete))

eh:(defflavor new-region-needed-for-moby-area-error (area restart-tag)
              (error)
     :gettable-instance-variables
     :initable-instance-variables)

eh:(defmethod (new-region-needed-for-moby-area-error :ucode-proceed-types) ()
     '(:allocate-new-region))

eh:(defmethod (new-region-needed-for-moby-area-error
                :case :proceed-asking-user :allocate-new-region)
              (continuation ignore)
     "Allocate a region and proceed"
     (funcall continuation :allocate-new-region))

eh:(defmethod (new-region-needed-for-moby-area-error
                :case :proceed-ucode-with-args :allocate-new-region) (sg &rest ignore)
     ;(mfs:allocate-region-to-moby-area area ...)
     (break "foo")
;   assign moby namespace and dataspace to file.
     (format t "~%Continuing after allocating new moby region")
     (sg-proceed-micro-pc sg restart-tag))



;moby packages and symbols
; -- for now, a simple implementation to prove concepts... --
; The root of the moby-package-system is the root of the file "moby:root;package-root.mby".
; The CAR of the root is the moby-symbol moby-address allocating pointer, as a bignum.
; The CADR of the root is an alist (<package-string> . <moby-package>)
;  <moby-package> is a defstruct defined below.  The SYMBOL-LIST component is a list
; of the relavent symbols.
;
;A 3 element ART-Q array serves as a moby symbol header.
; 0 is the print name (as a string).
; 1 is the moby-package-cell
; 2 in moby form, is a dtp-symbol pointer to the symbol header itself.
;   This gets reconciled into the symbol pointer for the corresponding local symbol,
;   via a cute trick.  Essentially, what would otherwise be an infinite recursive loop
;   trying to reconcile the dtp-symbol pointer is broken when the reconciler notices
;   a pointer to *-3.  It then knows this is an internal symbol pointer, and should be
;   reconciled by looking up the print-name in *-2 in the local package in *-1.

;Moby symbols do not have value-cells, function cells, or property cells.  If needed
; these should be implemented using MOBY-CELLs, when they are available, see discussion below.

(defun moby-package-root-string-for-partition-host (partition-host-name)
  (string-append partition-host-name ":root;package-root.mby"))

(defun moby-package-root (partition-host)
  (cond (*moby-package-root*)
        (t
         (setq *moby-package-root*
               (open (moby-package-root-string-for-partition-host
                       (send partition-host
                             :moby-partition-host-name))
                     :direction :output :moby-mapped t :write-once t
                     :if-exists :append :if-does-not-exist :create
                     :create-directories t)))))

(defun moby-package-root-of-lispm-host (host)
  (cond ((get host 'moby-package-root-host-name))
        (t (let ((prn (moby-package-root-host-name host)))
             (cond ((null prn)
                    (ferror nil "Package root name for host ~s not available" host))
                   (t (putprop host prn 'moby-package-root-host-name)
                      prn))))))

(defun moby-package-root-host-name-server ()
  (LET ((CONN (CHAOS:LISTEN "MOBY-PACKAGE-ROOT-HOST-NAME")))
    (IF (CHAOS:UNWANTED-CONNECTION-REJECTED-P CONN "none of your bee's wax")
        (RETURN-FROM MOBY-PACKAGE-ROOT-HOST-NAME-SERVER NIL))
    (COND (*MOBY-LOCAL-PACKAGE-HOST*
           (CHAOS:ANSWER-STRING CONN (FORMAT NIL "~A" *MOBY-LOCAL-PACKAGE-HOST*)))
          (T (CHAOS:REJECT CONN "MOBY-LOCAL-PACKAGE-HOST not set up.")))))

(ADD-INITIALIZATION "MOBY-PACKAGE-ROOT-HOST-NAME"
                    '(MOBY-PACKAGE-ROOT-HOST-NAME-SERVER)
                    NIL
                    'CHAOS:SERVER-ALIST)

(DEFUN moby-package-root-host-name (HOST &OPTIONAL (TIMEOUT 240.) &AUX PKT)
  "Returns string if host up, NIL if host down."
  (SETQ HOST (SI:PARSE-HOST HOST))
  (UNWIND-PROTECT
      (CONDITION-CASE ()
          (SETQ PKT (CHAOS:SIMPLE HOST "MOBY-PACKAGE-ROOT-HOST-NAME" TIMEOUT))
        (SYS:REMOTE-NETWORK-ERROR
         NIL)
        (:NO-ERROR
         (string-append (chaos:pkt-string pkt))))
    (AND PKT (CHAOS:RETURN-PKT PKT))))

(defun writeout-moby-package-root (partition-host)
  (let ((p (moby-package-root partition-host)))
    (moby-writeout-area (%area-number p))))

;;; This defines the format of a MOBY-PACKAGE.
;note that MOBY-PACKAGES dont have much information stored in them.  Instead, a MOBY-PACKAGE
; is really pretty much just a name and an association list of strings and their moby addresses.
; The name is associated with the local-package of the same name, and all the hairy properties
; come from there.  Doing that way minimizes depandance on information which could vary
; across all the systems of the world.
(defstruct (moby-package (:type :array ) (:conc-name mpkg-))
                       ;; (:constructor moby-pkg-make-package)
                       ;; (constructor klapaucius)
;  (refname-alist nil :documentation
;    "Alist of local nicknames, available in this package, for other packages.
;Each element is (STRING . PACKAGE)")
  (name nil :documentation
    "Official name for this package (a string)")
  (symbol-list nil :documentation
    "An list of symbols")
;  (nicknames nil :documentation
;    "List of nicknames for this package (strings)")
;  (use-list nil :documentation
;    "List of packages this one has done USE-PACKAGE to")
;  (all-packages-pointer '*all-packages* :documentation
;    "Pointer to the symbol *ALL-PACKAGES*.
;Here only for the sake of QF running on another machine.")

;  ;; Slots beyond here not known about by QF.

;  (used-by-list nil :documentation
;    "List of packages that have done USE-PACKAGE to this package")
;  (shadowing-symbols nil :documentation
;    "List of symbols explicitly shadowed in this package")
  (number-of-symbols nil :documentation
    "Current number of symbols in this package")
;  (max-number-of-symbols nil :documentation
;    "Threshold for rehashing. This is the specified size of the package array")
;  (prefix-print-name nil :documentation
;    "Name to print in package prefixes.  NIL means use PKG-NAME")
  (plist nil :documentation
     "Be careful putting stuff here, see comments near MOBY-PACKAGE defstruct"
;    "Random properties associated with this package.
;Properties used include:
;SI:READ-LOCK (non-NIL means that READ will not attempt to intern new symbols in this package)
;:SOURCE-FILE-NAME
;SI:SUPER-PACKAGE (value is the superpackage, if there is one)"
    )
;  (new-symbol-function nil :documentation
;    "Function called to store a new symbol in this package.
;NIL means PKG-INTERN-STORE is used.")
;  (auto-export-p nil :documentation
;    "Non-NIL means this package EXPORTs all symbols put in it.")
  (partition-host-name nil :documentation
   "Name of host this package structure primarily resident on.  Visible to hosts that moby-map this .")
  )

(defun moby-package-name (pkg)
  (mpkg-name pkg))

(defstruct (moby-symbol (:type :array) (:conc-name msym-))
  ;print-name package and symbol must be in exactly that order.
  print-name
  package
  symbol        ;winds up being a pointer to local symbol, see discussion above.
  flavor-instance-descriptions
  )

(defstruct (moby-flavor-instance-description (:type :array) (:conc-name mflav-))
  symbol
  all-instance-variables)

(defun moby-instance-symbol (inst-desc)
  (mflav-symbol inst-desc))

(defun moby-all-instance-variables (inst-desc)
  (mflav-all-instance-variables inst-desc))

;the following complete lossage seems necessary for the error handler menu to
; document itself properly.  Flush this (hopefully) as soon as common-lisp error system
; installed.
(defflavor moby-package-not-found () (si:ferror))
(defmethod (moby-package-not-found :document-proceed-type)
           (proceed-keyword stream &rest resume-handlers) resume-handlers
  (cond ((listp proceed-keyword)
         (apply 'format stream (car proceed-keyword) (cdr proceed-keyword))
         nil)
        (t
         (format stream proceed-keyword)
         nil)))

(defsignal moby-package-not-found (moby-package-not-found) (package-name)
  "There is no moby package.")

;returns package-specifier, which is a 2 list, (<package-properties> <symbol-string-alist>)
(defun moby-pkg-find-package (partition-host thing &optional create-p use-local-names-package)
  "Find or possibly create a moby package named THING.
If MOBY-FIND-PACKAGE can find a package from the name THING, we return that package.
Otherwise, we may create such a package, depending on CREATE-P.
This should only happen if THING is a string or symbol.
Possible values of CREATE-P:
 NIL means get an error,
 :FIND means return NIL,
 :ASK means ask whether to create the package, and returns it if so.
 T means create the package (with default characteristics if they are unspecified)
   and return it."
  (or ;(and (packagep thing) thing)
      (moby-find-package partition-host thing use-local-names-package)
      (case create-p
        (:find nil)
        ((:error)
         (signal-proceed-case ((new-name)
                               'moby-package-not-found
                               "A MOBY Package corresponding to ~s not found"
                               (moby-pkg-string-of thing))
           ("Create the package"
            (format t "~%Creating the package")
            (or (moby-find-package partition-host thing)
                (moby-make-package partition-host thing)))
           ("Supply a new package name to use instead"
            (let* ((*package* si:pkg-user-package)
                   (string1 (string (cli:read-from-string new-name))))
              (moby-pkg-find-package partition-host string1 create-p nil)))
           ("Try again to find the same package"
            (moby-pkg-find-package partition-host thing create-p use-local-names-package))))
        (:ask
         (if (fquery format:yes-or-no-p-options
                     "~&Package ~A not found.  Create it? "
                     thing)
             (moby-make-package partition-host thing)
           (cerror :no-action nil nil
                   "Please load package ~A declaration then continue." thing)
           (moby-pkg-find-package partition-host thing create-p)))
        ((nil t)
         (moby-make-package partition-host thing)))))

(defun moby-find-package (partition-host thing &optional use-local-names-package)
  use-local-names-package
  (cond ((moby-packagep thing)
         thing)
        (t
         (let* ((string-of-thing (moby-pkg-string-of thing))
                (root (moby-package-root partition-host))
                (package-alist (cadr root)))
           (cdr (assoc string-of-thing package-alist))))))

(defun moby-pkg-string-of (thing)
  (cond ((stringp thing) thing)
        ((symbolp thing) (get-pname thing))
        ((packagep thing) (si:pkg-name thing))
        (t (ferror nil "Cant get a string from arg ~S" thing))))

(defun moby-packagep (thing)
  (and (arrayp thing)
       (not (stringp thing))
       (not (packagep thing))))         ;assume its a moby package **

(defun moby-make-package (partition-host thing)
  (let* ((string-of-thing (cond ((stringp thing) thing)
                                ((symbolp thing) (get-pname thing))
                                ((packagep thing) (si:pkg-name thing))
                                (t (ferror nil "Cant get a string from arg ~S" thing))))
         (root (moby-package-root partition-host))
         (root-area (%area-number root))
         (package-name (copy-to-area-if-necessary root-area string-of-thing))
         (p-h-n (copy-to-area-if-necessary root-area
                 (send partition-host :moby-partition-host-name)))
         (package (make-moby-package
                    :partition-host-name p-h-n
                    :make-array (:area root-area)
                    :name package-name
                    :symbol-list nil)))
    (cond ((null (cdr root))
           (rplacd root (cons-in-area nil nil root-area))))             ;first time.
    (si:push-in-area root-area
  ;this is an alist so we can see if its the one we want without touching the whole
  ; package when it eventually becomes some sort of array or hash table.
                     (cons-in-area package-name package root-area)
                     (cadr root))
    package))

;;; Value 1 is the moby symbol defstruct of the symbol.
;;; Value 2 is T if the symbol was already interned.
;;; Value 3 is the moby-package that the symbol is actually present in.
(defun moby-intern (partition-host sym &optional pkg &aux moby-package str)
  "Returns the moby-symbol defstruct for a symbol.
Interns the string or symbol SYM in moby package PKG.  If PKG is a local
package, the moby package with the same name is used (or, if PKG is NIL,
the moby package with the same name as *PACKAGE*).
If the package has a symbol whose pname matches SYM, that symbol is returned.
The USEd packages are also searched.
Otherwise, if SYM is a symbol, it is put in the package and returned.
Otherwise (SYM is a string), a new symbol is constructed, put in the package and returned.

The second value is non-NIL if a preexisting symbol was found.
 More specifically, it can be :INTERNAL, :EXTERNAL or :INHERITED.

The third value is the package the symbol was actually found or inserted in.
This may be PKG or one of the packages USEd by PKG."
  (declare (values symbol already-interned-flag actual-package))
  (cond ((null pkg) (setq pkg *package*)))
  (cond ((symbolp sym) (setq pkg (car (package-cell-location sym)))))
  (setq moby-package (cond ((moby-packagep pkg)
                            pkg)
                           (t (moby-pkg-find-package partition-host pkg))))
  (cond ((not (symbolp sym))
          ;need to actually get local symbol to install in moby-symbol defstruct.
         (setq sym (intern sym (cond ((packagep pkg) pkg)
                                     (t (pkg-find-package (mpkg-name moby-package))))))))
  (setq str (symbol-name sym))
  (without-interrupts
    ;; Search this package.
    (dolist (e (mpkg-symbol-list moby-package))
      (when (equal str (msym-print-name e))
          (return-from moby-intern (values e
                                           t
                                           moby-package))))
    ;; Must install a new symbol.
    ;; Make a moby-symbol defstruct.
    (let* ((root-area (%area-number moby-package))
           (moby-symbol (make-moby-symbol
                         :make-array (:area root-area)
                         :print-name (copy-to-area-if-necessary root-area str)
                         :package moby-package
                         :symbol sym)))
      (si:push-in-area root-area moby-symbol (mpkg-symbol-list moby-package))
      (moby-writeout-area root-area)
      (values moby-symbol nil moby-package)
      )))

(defun moby-intern-memoed (partition-host sym)
 ;** should be per partition-host
  (cond ((null sym) 0)          ;special hack.
        ((gethash sym *symbol-to-moby-address*))
        (t (let ((moby-sym (moby-intern partition-host sym)))
             (puthash sym moby-sym *symbol-to-moby-address*)
             moby-sym))))

(defun moby-find-flavor-instance-description (partition-host instance)
  (let ((flavor-defstruct (si:instance-flavor instance)))
    (cond ((get flavor-defstruct (send partition-host :host-index)))
          (t
           (let* ((flavor-symbol (%p-contents-offset flavor-defstruct
                                                    si:%instance-descriptor-typename))
                  (moby-flavor-symbol (moby-intern-memoed partition-host flavor-symbol))
                  (local-instance-variables (si:flavor-all-instance-variables flavor-defstruct)))
             (do ((p (msym-flavor-instance-descriptions moby-flavor-symbol)
                     (cdr p)))
                 ((null p)
                  (let* ((root (moby-package-root partition-host))
                         (root-area (%area-number root))
                         (moby-fid (make-moby-flavor-instance-description
                                    :make-array (:area root-area)
                                    :symbol flavor-symbol
                                    :all-instance-variables
                                      (copy-to-area-if-necessary root-area
                                                                 local-instance-variables))))
                    (si:push-in-area root-area
                                     moby-fid
                                     (msym-flavor-instance-descriptions moby-flavor-symbol))
                    (moby-writeout-area root-area)
                    moby-fid))
               (cond ((equal local-instance-variables
                             (mflav-all-instance-variables (car p)))
                      (putprop flavor-defstruct (car p)
                               (send partition-host :host-index))
                      (if *remember-objects-with-cached-plists*
                          (pushnew flavor-defstruct *all-objects-with-cached-plists* :test #'eq))
                      (return (car p))))))))))

;(defun flavor-putprop (flavor-defstruct value flag)
;  (setf (si:flavor-plist flavor-defstruct)
;       (list* flag value (si:flavor-plist flavor-defstruct))))

;MOBY-CELLs
; MOBY-CELLS are an important tool for building data structures in moby space
;  exhibit a high degree of multipoint availability.
; For example, MOBY-CELLs may be used effectively to "substitute" for value cells of symbols,
;property-list-cells of symbols and instances, and "association" cells of property lists
;or alists.

; The critical active component of a moby-cell is the cell itself, which is a single location
;in local-address space capable of holding a typed-pointer.  There exists independant control
;of the read-write properties of this single cell, which can be changed from time to time.
;(In the implementation with the current hardware,
;this means the page almost always has to be read-only, with special microcode to
;allow writes to happen reasonably efficiently, if they are to be allowwed).
; The cell has a "permanently" associated moby-address.
; The cell may have a "namechain".  This is a way to locate the cell from external
;  information, and might correspond roughly to:  "The value cell of the symbol FOO",
;  or "The association with the symbol BAR off FOO's property list".
;In addition, there exists for each MOBY-CELL a database which lists which machines
;that cell is established on at a given time.  This data base can be kept by a single
;machine or several.  In case of need, it can be reconstructed by a broadcast message
;asking all machine on which the cell is established to respond.

;MOBY-CELLs can be established or disestablished on a particular machine.
;  If established it means that that particular machine is present in the above mentioned
;data base and that a local cell has been allocated.  Said cell may either contain a
;lisp value or may contain DTP-UNRECONCILED.

;If disestablished, the local cell MUST contain DTP-UNRECONCILED if it exists at all.

;All instances of a particular MOBY-CELL, taken globally, function as a "serializer".
;That is, they can be modelled as containing sequentally a single series of values.
;At any moment, a conceptual global snapshot of the system would either show all instances of a
;cell containing the "current" value (the quiesent state), or it might show some containing
;the "previous" value.  (Some, in addition, might contain DTP-UNRECONCILED which
;always counts as the "current value")
;If any contained the previous value, they must either get updated or made unreconciled
;before there can be a new "current value", and this process would be actively in progress
;in the form of network messages in transit, etc.

;usually, when the value of the cell is called for, it simply accessed directly by CAR or CDR,
;etc.  However, there is available a primitive (moby-current-value <cell>) which actually
;contracts to return then current current-value.  (ie if any update cycle was in progress,
;MOBY-CURRENT-VALUE is guarenteed to return the value about to be updated to.)

;To implement this somewhat efficiently, each moby-cell has a single bit of "LIFE" associated
;with it. Only one machine can posses this LIFE, so MOBY-CURRENT-VALUE can return immediately
;on that machine without network traffic, etc.  Anyone wishing to modify the cell must call
;for the LIFE, then send out an update message to all machines on which the cell is established.
;An acknowlegment must be received from all these machines before the update is complete and
;the LIFE can be released. (However, once LIFE is obtained, the modification can be made
;immediately on the local machine and the local computation proceeded.)  The update message
;can either send out the updated value, so specify the value is to be made DTP-UNRECONCILED,
;at the option of the modifying host.  If an DTP-UNRECONCILED update is sent out and acknowledged,
;that host is said to have exclusive control of the variable and may modify it as it pleases
;without further network traffic until a request is received to export the cell.


;Openning remote moby-mapped files.
;  If the :moby-mapped keyword is used in an open to a network connected host,
;everything proceeds normally until we get to fs:open-chaos.
;converted into a probe
;probe has info necessary to moby-map-section.
;  for :input direction this currently is:
;    moby-setup-area:   (adds local-area, area, dataspace-map to msa)
;    moby-assure-area-hash-size:   free-pointer
;    moby-associate-moby-local-area-with-map:

;multiple machines consing in the same section at the same time:
; It would clearly be undesirable to require them to use the same free pointer,
;since that would generate considerable network traffic and be awkward, etc.
;Thus, we allow each to set up its own active region and cons away.


;-----
;namespace and dataspace..

;A moby section requires both namespace and dataspace to exist.
; Namespace is basically something we have a lot of.  In fact, the new scheme is NEVER
;to recycle it!.  It is allocated in big globs (maybe 2**32) from a central server to
;the particular MOBY partition.  A range of namespace is assigned to each region
;of a moby-mapped area.  Since regions are allocated in quanta of 64. pages,
;namespace is also effectively allocated in 64 page quanta.

;a namespace address is 50 bits, although, using relative techniques, it may sometimes be
; stored in less.  (One idea, not implemented now, is that any address stored in the section
; itself must always point either within the section or to an exit vector.  Thus internal
; pointers could easily be held in 25 bits, without provision for overflow.  The exit vector
; must hold full moby addresses).


;A dataspace page number is always relative its own moby-partition.  24 bits of page number is plenty.
; dataspace page numbers are found mainly in dataspace-maps.

;data structures:
;  moby-partition.
;       root-glob
;       globs which have been assigned
;       current-glob

;  root-section
;       root-glob
;       root-dataspace-map

;  section
;      namespace-map
;       ea region:  base moby-address size.
;      dataspace-map
;       dataspace blocks.  provision for "gaps" if necessary.


;  namespace-map
;   leader:
;     fill-pointer
;   each entry corresponds to a local region.
;    [moby-address, n-pages].

;  dataspace-map
;   leader:
;     fill-pointer
;   each entry corresponds to a local region.
;     [free-pointer, local-pages].  note that each region is not necessarily fully populated
;         with pages (ie there can be fewer in the dataspace-map than in the namespace-map).

;the root section is mapped into moby-namespace, primarily for convenience in bootstrapping.
; Pointers from elsewhere are not allowwed directly into the root-section.  (--exit vector maybe).

;the root section is somewhat special for the followwing reasons:
  ; bootstraping considerations
  ;-- "integrity"  old frob had dataspace map in one block so there was no possibility of
  ;   a partial write.. with new external-bits frob, this is not necessary.


;fundamental problem ... in bootstrapping the system.

;  It is extremely convenient to use the moby-address-space system to bootstrap itself.
;(this allows ordinary LISP arrays, etc, plus other usual advantages.)
;  Problem is, MOBY data needs a namespace-map and a dataspace-map to function.
;Since the system itself is not bootstrapped, direct namespace=dataspace mapping
; is the only way to accomplish this.  This direct mapped region is called the ROOT-SECTION.
;We want the minimum amount of data possible to be resident there.
;Even more important is the question whether the
;data stored direct-mapped is fixed, or grows in some way.  Unfortunately, it seems
;that it must grow if limitations on certain parameters set at the time the root is
; is to be avoided.

; The critical data which must be stored in the ROOT-SECTION is namespace and dataspace
;maps which enable us to bootstrap the rest of the system.

; Namespace maps are relatively non-critical.  They change only when a new section is added.
;(Conceivably, this could be never if sufficient namespace is allocated to begin with..)

; Dataspace maps present considerably more problems.  Dataspace maps change much more frequently.
;Also, we need the reincarnate-on-write feature to work.  It would be desireable for the
;root-dataspace-map to be identical to other dataspace-maps in the system, although this
;is not absolutely necessary.

; For the reincarnate-on-write feature to work, we need to be able to "find" the various
;incarnations, and, by examining them, determine which is the last complete incarnation.
;Each of these possible incarnations implies the existance of a dataspace map which says
;where it is.

;.. another possibility is to not have the reincarnate-on-write feature work on
;root-dataspace-maps, and, instead, have N root-dataspace-maps actually visible at the
;LISP level.  This may be a good idea..

;A STRICT-TREE.  A data structure, rooted at a single node, where each pointer
; may point only at structures lower than itself in the tree is called a STRICT-TREE.
; A strict tree corresponds to the GREY-CODE of data-structures.  Any possible transformation
; may be made in a sequence of individual steps, before and after which the data structure
; is a STRICT-TREE.  The point is, the resulting datastructure can be effectively
; checkpointed in an incremental way, especially if timestamps are employed as well.

;-idea-- a two-copy section ...
; you have two places to swap each page out to.  One policy is to maintain the whole section
;read-only except one page.  If some other page gets written, you swap out the previous
;read-write page to disk and make that one read-only before allowing the other write to
;happen.


;size of dataspace-maps:
;   section-dataspace-map
;       leader-header
;       fill-pointer
;       leader-length
;       header
;       <n, typically 3>  region-dataspace-map  <single-pointer>

;   region-dataspace-map   <pointed to in a single place>
;       leader-header
;       region-free-pointer
;       fill-pointer
;       leader-length
;       header
;       <n, >  partition block number

;multiple region (usually double).  A certain region of each MOBY partition is double stored.
;  On each swapout of a multiply stored page, it is actually written out in one of the
;possible places on a rotating basis  (which one is kept track of in the %%swapout-store-option
;of the virtual-page-data).  When a salvage is done (or the first time the page is touched),
;the most recent one of the possibilities is (normally) selected by reference to the
;external-bits timestamp.



;dataspace numbering conventions:
; dataspace pages are numbered beginning at 0 in every MOBY partition.  As MOBY partitions
;are activated, each is assigned a DATASPACE-OFFSET (which is effective for this "boot" only).
;It thus takes its place within the DATASPACE-WITH-OFFSET naming space of the host.
;*MOBY-NAMESPACE-PAGE-TO-DATASPACE-PAGE*
;holds such names, and *VMEM-MOBY-PARTITION-ALIST* is indexed by such.
; MOBY-PARTITIONs can be "band-transferred" around with no problems..

;--next paragraph is obsolete.
; dataspace is a much more local thing than namespace.  It need only be consistant within
;a particular physical machine.  If a remapping should become necessary, it is a relatively
;simple matter.  A few Qs in the leader of the MOBY partition and the dataspace maps in the
;partition need be changed.  These are easily accessible.
;  If there are several moby-partitions being supported by a single host, there dataspace
;mappings must be compatible.  It is OK to "band-transfer" moby paritions around, although
;dataspace remapping may become necessary.

;namespace numbering conventions:
; moby-base-page
;  partition-header
;   partition-header-low-namespace-page
;   partition-header-high-namespace-page (d = constant partition-header-namespace-length-in-pages)
;  root-mapping-section
;    size= root-mapping-section-namespace-allocation
;  root-section
;    size= *root-section-region-size-in-pages*  (size of initial region.  Can expand
;               as provided for in root-mapping-section)
;  ...  user sections


;namespace-allocation-limit  is limit of namespace allocated to this partition.
;  if exhausted, more must be requested, presumably from central server.
;namespace-allocation-pointer
;  actual allocation pointer for allocating namespace to new sections.


;bootstrapping a moby-partition, etc, involves three sections:   All are always stored
;    on double mapped tracks.
;  partition-header
;  root-mapping-section
;  root-section.

;  The partition header section is fixed allocated and never expands.
;    it contains the partition-header defstruct itself, followed by the
;       root-mapping-section namespace and dataspace maps.  Once initialized
;       it essentially never changes.

;  The root-mapping-section contains:
;       namespace-allocation-structure.
;       track usage tables (separate for single and double mapped),
;        and the root-section namespace and dataspace maps.
;     The track usage tables frequently change, and need to be written.
;     Also, if root-section expands, the mapping info for the additional page(s)
;     needs to be stored away.

;  root-section contains the section-directory-header and all other directory info.

;new map format:
;  section-map
;   leader
;    fill-pointer
;   each-entry -> region-map

;  region map
;   leader
;     3 namespace origin  (page)
;     2 size       (qs)
;     1 free-pointer (qs)
;     0 fill-pointer
;   each-entry -> dataspace address (w/o offset) for page.
