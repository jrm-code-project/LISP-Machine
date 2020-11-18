;;; -*- Mode: LISP; Package: UA; Base:8 -*-
;       ** (c) Copyright 1980 Massachusetts Institute of Technology **

;;;Managing microcode entries and stuff:
;;;  All actual microcode entry address are stored in MICRO-CODE-SYMBOL-AREA.
;;;This area is 1000 locations long.  The first 600 are accessible via
;;;misc macroinstruction (values 200-777).
;;;  How DTP-U-ENTRY works:  DTP-U-ENTRY is sort of an indirect pointer relative
;;;to the origin of MICRO-CODE-ENTRY-AREA.  The Q referenced is to be interpreted
;;;in functional context in the normal fashion, with one exception: If the
;;;data type is DTP-FIX,  this is a "real" ucode entry.
;;;In that case, various data (the number of args, etc), can be obtained
;;;by referencing various other specified areas with the same offset as was used
;;;to reference MICRO-CODE-ENTRY-AREA.  The address to transfer to in microcode
;;;is gotten by referencing MICRO-CODE-SYMBOL-AREA at the relative address
;;;that was obtained from MICRO-CODE-ENTRY-AREA.  The reason for the indirecting
;;;step from MICRO-CODE-ENTRY-AREA to MICRO-CODE-SYMBOL-AREA is to separate
;;;the world into two independant pieces.  (The microcode and MICRO-CODE-SYMBOL-AREA
;;;separate from the rest of the load).

;;;Making new microcoded functions:  Two "degrees of commitment" are available,
;;;ie, the newly added function can be made available as a misc instruction or not.
;;;If it is available as a misc instruction, the system becomes completely committed
;;;to this function remaining microcoded forever.  If not, it is possible in the future to
;;;decommit this function from microcode, reinstating the macrocoded definition.
;;;  Decommiting can be done either by restoring the DTP-FEF-POINTER to the function cell,
;;;or by putting it in the MICRO-CODE-ENTRY-AREA position.  This latter option allows
;;;the microcoded definition to be quickly reinstalled.
;;;  One problem with decomitting concerns activation-records for the microcoded
;;;which may be lying around on various stack-groups.  If later, an attempt is made
;;;to return through these, randomness will occur.  To avoid this, on a
;;;macro-to-micro return, the microcode can check that the function being returnned
;;;to is still in fact microcoded.

;;;Re a-mem and m-mem:  Separate register addresses have been retained since
;;; there really are two registers in the hardware and we want to be
;;; able to examine both in CC. however, in the following, there is
;;; really only one array, 0-37 of which is considered to be m-mem, the rest, a-mem.

(DECLARE SI:(SPECIAL RAPC RASIR RAOBS RANOOPF RASTS
           RACMO RACME RADME RAPBE RAM1E RAM2E RAAME RAUSE RAMME RAFSE RAFDE
           RARGE RACSWE RARDRE RACIBE RAGO RASTOP RARDRO RAFDO RAOPCE
           RARS RASTEP RASA RAAMO RAMMO RARCON RAPBO RAUSO RADMO RADME))

;;;A "total" snapshot of the processor consists of a ucode-image and a ucode-state.
;;;The ucode-image contains quantities which are unchanged once they are loaded,
;;;while all "dynamic" quantities are contained in the ucode-state.  The assignment
;;;as to which one is made on a memory by memory basis except for a-memory, which
;;;is assigned on a location by location basis.  As well as the contents of all
;;;hardware memories, the combined ucode-image and ucode-state also contain copies
;;;of micro-code-related main memory areas such as MICRO-CODE-SYMBOL-AREA and
;;;PAGE-TABLE-AREA.  The intention is that all data which changes "magically" from
;;;the point of view of lisp be included in ucode-state. Thus the inclusion of
;;;PAGE-TABLE-AREA.  One motivation for having such an inclusive ucode-state is to
;;;be able to find possible bugs by checking the page-tables etc, for consistency.
;;;Also, it may be possible in the future to single step microcode via this mechanism
;;;(either via hardware or via a simulator).

(DECLARE (SPECIAL CURRENT-UCODE-IMAGE CURRENT-ASSEMBLY-DEFMICS CURRENT-ASSEMBLY-TABLE
                  CURRENT-ASSEMBLY-HIGHEST-MISC-ENTRY))

(DEFVAR NUMBER-MICRO-ENTRIES NIL)  ;Should have same value as SYSTEM:%NUMBER-OF-MICRO-ENTRIES
                                   ;Point is, that one is stored in A-MEM and is reloaded
                                   ;if machine gets warm-booted.

;;;A ucode-image and associated stuff describe the complete state of a micro-load.
;;; Note that this is not necessarily the micro-load actually loaded into the machine
;;; at a given time.
(DEFSTRUCT (UCODE-IMAGE :ARRAY :NAMED (:ALTERANT NIL))
   UCODE-IMAGE-VERSION                  ;version # of this microcode
   UCODE-IMAGE-MODULE-POINTS            ;List of ucode-module structures, "most recent" first
                                        ; these give modules that were loaded and the state
                                        ; of load after each so that it is possible to unload
                                        ; a module, etc. (In push-down fashion. All modules
                                        ; loaded since that module must also be unloaded, etc)
   UCODE-IMAGE-MODULE-LOADED            ;A tail of ucode-image-module-points, which is
                                        ;  the list of modules actually loaded now.
   UCODE-IMAGE-TABLE-LOADED             ;The concentationation of the ucode-tables for
                                        ;  the modules loaded.
   UCODE-IMAGE-ASSEMBLER-STATE          ;Assembler state after main ASSEMBLY
   (UCODE-IMAGE-CONTROL-MEMORY-ARRAY    ;Data as loaded into control memory
    (MAKE-ARRAY SI:SIZE-OF-HARDWARE-CONTROL-MEMORY));size-of-hardware-control-memory
   (UCODE-IMAGE-DISPATCH-MEMORY-ARRAY   ;Data as loaded into dispatch memory
    (MAKE-ARRAY SI:SIZE-OF-HARDWARE-DISPATCH-MEMORY))
   (UCODE-IMAGE-A-MEMORY-LOCATION-IN-IMAGE ;1 -> this a-mem location part of ucode-image
    (MAKE-ARRAY SI:SIZE-OF-HARDWARE-A-MEMORY ':TYPE 'ART-1B))
   (UCODE-IMAGE-A-MEMORY-ARRAY          ;data as loaded into a-memory
    (MAKE-ARRAY SI:SIZE-OF-HARDWARE-A-MEMORY))
   (UCODE-IMAGE-ENTRY-POINTS-ARRAY      ;Image of the stuff that normally gets main memory
    (MAKE-ARRAY 1000 ':LEADER-LIST '(577)));with fill-pointer
                                        ; First 600 locs are entries for misc
                                        ;  insts 200-777.
                                        ; Next 200 are for micro-code-entries
                                        ;  (specified via micro-code-entry pseudo in CONSLP)
                                        ; Rest are entry points to microcompiled fctns.
   (UCODE-IMAGE-SYMBOL-ARRAY            ;CONSLP symbols. Alternating symbol, type, value
    (MAKE-ARRAY 3000 ':FILL-POINTER 0))
   )

(DEFUN (UCODE-IMAGE NAMED-STRUCTURE-INVOKE) (OP &OPTIONAL UCODE-IMAGE &REST ARGS)
  (SELECTQ OP
    (:WHICH-OPERATIONS '(:PRINT-SELF))
    ((:PRINT-SELF)
     (SI:PRINTING-RANDOM-OBJECT (UCODE-IMAGE (CAR ARGS) :NO-POINTER)
       (FORMAT (CAR ARGS) "UCODE-IMAGE version ~d, modules ~s"
               (UCODE-IMAGE-VERSION UCODE-IMAGE)
               (UCODE-IMAGE-MODULE-POINTS UCODE-IMAGE))))
    (OTHERWISE (FERROR NIL "~S Bad operation for a named-structure" op))))

;;;A ucode-module is the unit in which ucode is loaded.  The ucode-module
;;;containts enough information to completely hold the logical state of the ucode-loader
;;;just after the module was loaded.  Thus, modules may be off-loaded in reverse
;;;order from that in which they were loaded.  The active ucode-modules are
;;;contained in a list off of ucode-image-module-points, the last element in which
;;;list refers to the initial microcode load.
;;;A-memory is allocated in two regions: an ascending constants block, and a
;;;variable block descending from the top.  If the two collide, a-memory is exhausted.
(DEFSTRUCT (UCODE-MODULE :ARRAY :NAMED (:ALTERANT NIL))
   UCODE-MODULE-IMAGE                   ;IMAGE THIS MODULE PART OF
   UCODE-MODULE-SOURCE                  ;WHERE CAME FROM: A pathname
   UCODE-MODULE-GENERIC-PATHNAME        ;of the source
   UCODE-MODULE-ASSEMBLER-STATE         ;assembler state after module assembly
   UCODE-MODULE-TABLE                   ;as output by assembler.
   UCODE-MODULE-ENTRY-POINTS-INDEX      ;fill-pointer of UCODE-IMAGE-ENTRY-POINTS-ARRAY
   UCODE-MODULE-DEFMICS
   UCODE-MODULE-SYM-ADR                 ;final fill pointer for UCODE-IMAGE-SYMBOL-ARRAY
   UCODE-MODULE-I-MEM-ALIST             ;Alist of I-MEM array resulting from assembly.
   UCODE-MODULE-D-MEM-ALIST             ;Alist of D-MEM array resulting from assembly.
   UCODE-MODULE-A-MEM-ALIST             ;Alist of A-MEM array resulting from assembly.
)

(DEFUN (UCODE-MODULE NAMED-STRUCTURE-INVOKE) (OP &OPTIONAL UCODE-MODULE &REST ARGS)
  (SELECTQ OP
    (:WHICH-OPERATIONS '(:PRINT-SELF))
    ((:PRINT-SELF)
     (SI:PRINTING-RANDOM-OBJECT (UCODE-MODULE (CAR ARGS) :NO-POINTER)
       (FORMAT (CAR ARGS) "UCODE-MODULE ~s" (UCODE-MODULE-SOURCE UCODE-MODULE))))
    (OTHERWISE (FERROR NIL "~S Bad operation for a named-structure" op))))

(DEFSTRUCT (UCODE-STATE (:ALTERANT NIL))

;The following registers "should" be in the ucode-state.  However, they are
;commented out for the time being because (1) they are not needed for present
;purposes. (2) they are awkward to do without bignums, etc.  They are in the
;same order they are in (almost) the register address space

; (UCODE-STATE-PC 0)                    ;PC (PC)
;  (UCODE-STATE-USP 0)                  ;U stack pointer (USP)
;;RAIR==62562                           ;.IR (Put in diag inst reg, then load into IR, then
;;                                      ; update obus display. diagnostic only)
;  (UCODE-STATE-IR 0)                   ;Saved IR (The one saved on full state save
;                                       ; and restored on full restore)
;                                       ; This is normally the uinst about to get executed.
;  (UCODE-STATE-Q 0)                    ;Q register (Q)
;  (UCODE-STATE-DISPATCH-CONSTANT 0)    ;Dispatch constant register (DC)
;;RARSET==62566                         ;Reset register!  depositing here
;                                       ;  clears entire C, D, P, M1, M2, A, U and M memories!
;;RASTS==62567                          ;Status register (32 bit, as read by ERERWS)
;  (UCODE-STATE-OUTPUT-BUS 0)           ;Output bus status (32 bits)
;
;;Due to lossage, the following 4 are in the register address space at a random place
;  (UCODE-STATE-MEM-WRITE-REG 0)        ;Main mem write data register
;  (UCODE-STATE-VMA 0)                  ;VMA (virtual memory address)
;  (UCODE-STATE-PDL-POINTER 0)          ;PDL pointer (to PDL buffer)
;  (UCODE-STATE-PDL-INDEX 0)            ;PDL index (to PDL buffer)

   (UCODE-STATE-A-MEMORY-ARRAY          ;Data as loaded into a-memory
    (MAKE-ARRAY SI:SIZE-OF-HARDWARE-A-MEMORY ':TYPE 'ART-16B))
   (UCODE-STATE-PDL-BUFFER-ARRAY        ;Data as loaded into PDL buffer
    (MAKE-ARRAY SI:SIZE-OF-HARDWARE-PDL-BUFFER ':TYPE 'ART-16B))
   (UCODE-STATE-MICRO-STACK-ARRAY       ;Data as loaded into ustack
    (MAKE-ARRAY SI:SIZE-OF-HARDWARE-MICRO-STACK))
   (UCODE-STATE-LEVEL-1-MAP             ;Data as loaded into level 1 map.
    (MAKE-ARRAY SI:SIZE-OF-HARDWARE-LEVEL-1-MAP ':TYPE 'ART-8B))
   (UCODE-STATE-LEVEL-2-MAP             ;Data as loaded into level 2 map
    (MAKE-ARRAY SI:SIZE-OF-HARDWARE-LEVEL-2-MAP ':TYPE 'ART-16B))
   (UCODE-STATE-UNIBUS-MAP              ;Data as loaded into unibus map.
    (MAKE-ARRAY SI:SIZE-OF-HARDWARE-UNIBUS-MAP ':TYPE 'ART-16B))
   (UCODE-STATE-PAGE-TABLE              ;Copy of PAGE-TABLE-AREA
    (MAKE-ARRAY (SI:ROOM-GET-AREA-LENGTH-USED PAGE-TABLE-AREA)))
   (UCODE-STATE-PHYSICAL-PAGE-AREA-NUMBER;Copy of like named area
    (MAKE-ARRAY (SI:ROOM-GET-AREA-LENGTH-USED PHYSICAL-PAGE-AREA-NUMBER)))
)

(DEFVAR CURRENT-UCODE-IMAGE (MAKE-UCODE-IMAGE))
(DEFVAR CURRENT-ASSEMBLY-DEFMICS NIL)
(DEFVAR CURRENT-ASSEMBLY-TABLE NIL)

(DEFVAR CC-UCODE-IMAGE (MAKE-UCODE-IMAGE)) ;Use this for frobbing other machine with CC.

;This is really useful only for wired areas, but may as well work for all.
(DEFUN LOWEST-ADDRESS-IN-AREA (AREA)
  (DO ((REGION (SYSTEM:AREA-REGION-LIST AREA) (SYSTEM:REGION-LIST-THREAD REGION))
       (BSF (%LOGDPB 0 %%Q-BOXED-SIGN-BIT -1)
            (MIN BSF (SI:REGION-ORIGIN-TRUE-VALUE REGION))))
      ((LDB-TEST %%Q-BOXED-SIGN-BIT REGION)
       BSF)))

(DEFUN UCODE-IMAGE-STORE-ASSEMBLER-STATE (STATE UCODE-IMAGE)
   (SETF (UCODE-IMAGE-ASSEMBLER-STATE UCODE-IMAGE) STATE)
)

(DEFUN UCODE-IMAGE-INITIALIZE (UCODE-IMAGE &AUX TEM)
  (IF (NULL UCODE-IMAGE)
      (MAKE-UCODE-IMAGE)
    (SETF (UCODE-IMAGE-MODULE-POINTS UCODE-IMAGE) NIL)  ;Reset pointers, etc
    (SETQ TEM (UCODE-IMAGE-A-MEMORY-LOCATION-IN-IMAGE UCODE-IMAGE))
    (DOTIMES (I SI:SIZE-OF-HARDWARE-A-MEMORY)
      (SETF (AREF TEM I) 0))
    (SETF (FILL-POINTER (UCODE-IMAGE-ENTRY-POINTS-ARRAY UCODE-IMAGE)) 577)
    (SETF (FILL-POINTER (UCODE-IMAGE-SYMBOL-ARRAY UCODE-IMAGE)) 0)
    UCODE-IMAGE))

(DEFUN READ-SIGNED-OCTAL-FIXNUM (&OPTIONAL (STREAM *STANDARD-INPUT*) &AUX (NUM 0) (SIGN 1))
  (DO ((CH (DO ((CH (SEND STREAM ':TYI) (SEND STREAM ':TYI)))
               (( #\0 CH #\9) CH)
             (IF (= CH #/-)
                 (SETQ SIGN (- SIGN))))         ;otherwise flush random cruft (usually crlf)
           (SEND STREAM ':TYI)))
      ((NOT ( #\0 CH #\9))
       (IF (= CH #\_)
           (* SIGN (LSH NUM (READ-SIGNED-OCTAL-FIXNUM STREAM)))
         (* NUM SIGN)))
    (SETQ NUM (+ (* NUM 10) (- CH #\0)))))

(DEFUN ADD-ASSEMBLY (&OPTIONAL FILE-NAME (IMAGE CURRENT-UCODE-IMAGE)
                     &AUX ASSEMBLER-STATE-AFTER PATHNAME GENERIC-PATHNAME)
  (UNLESS (BOUNDP 'SI:RACMO)
    (READFILE "SYS: CC; CADREG LISP >"))
  (UNLESS (EQ %MICROCODE-VERSION-NUMBER
              (UCODE-IMAGE-VERSION IMAGE))
    (READ-UCODE-VERSION %MICROCODE-VERSION-NUMBER IMAGE))
  (SETQ PATHNAME (FS:MERGE-PATHNAME-DEFAULTS FILE-NAME)
        GENERIC-PATHNAME (SEND PATHNAME ':GENERIC-PATHNAME))
  (WHEN (EQ (UCODE-MODULE-GENERIC-PATHNAME (CAR (UCODE-IMAGE-MODULE-POINTS IMAGE)))
            GENERIC-PATHNAME)
       (FLUSH-MODULE NIL IMAGE))        ;Evidently a new version, flush the old
; (UA-DEFINE-SYMS IMAGE)
  (ASSEMBLE PATHNAME (UCODE-MODULE-ASSEMBLER-STATE
                       (CAR (UCODE-IMAGE-MODULE-POINTS IMAGE))))
  (SETQ ASSEMBLER-STATE-AFTER (MAKE-ASSEMBLER-STATE-LIST))
  ;;Merge results and form new module
  (MERGE-MEM-ARRAY I-MEM SI:RACMO IMAGE)
  (MERGE-MEM-ARRAY D-MEM SI:RADMO IMAGE)
  (MERGE-MEM-ARRAY A-MEM SI:RAAMO IMAGE)
  (LET ((MODULE (MAKE-UCODE-MODULE)))
    (SETF (UCODE-MODULE-I-MEM-ALIST MODULE)
          (ALIST-DESCRIBING-ARRAY I-MEM))
    (SETF (UCODE-MODULE-D-MEM-ALIST MODULE)
          (ALIST-DESCRIBING-ARRAY D-MEM))
    (SETF (UCODE-MODULE-A-MEM-ALIST MODULE)
          (ALIST-DESCRIBING-ARRAY A-MEM))
    (SETF (UCODE-MODULE-IMAGE MODULE) IMAGE)
    (SETF (UCODE-MODULE-SOURCE MODULE) PATHNAME)
    (SETF (UCODE-MODULE-GENERIC-PATHNAME MODULE) GENERIC-PATHNAME)
    (SETF (UCODE-MODULE-ASSEMBLER-STATE MODULE) ASSEMBLER-STATE-AFTER)
    (SETF (UCODE-MODULE-ENTRY-POINTS-INDEX MODULE)
          (FILL-POINTER (UCODE-IMAGE-ENTRY-POINTS-ARRAY IMAGE)))
    (SETF (UCODE-MODULE-DEFMICS MODULE)
          CURRENT-ASSEMBLY-DEFMICS)
    (SETF (UCODE-MODULE-TABLE MODULE)
          CURRENT-ASSEMBLY-TABLE)
    (SETF (UCODE-MODULE-SYM-ADR MODULE)
          (FILL-POINTER (UCODE-IMAGE-SYMBOL-ARRAY IMAGE)))
    (SETF (UCODE-IMAGE-MODULE-POINTS IMAGE)
          (CONS MODULE
                (UCODE-IMAGE-MODULE-POINTS IMAGE))))
)

(DEFUN ALIST-DESCRIBING-ARRAY (ARRAY)
  (LET ((ALIST))
    (DOTIMES (I (LENGTH ARRAY))
      (LET ((ELT (AREF ARRAY I)))
        (WHEN ELT (PUSH (CONS I ELT) ALIST))))
    ALIST))

(DEFCONST I-MEM-LENGTH SI:SIZE-OF-HARDWARE-CONTROL-MEMORY)
(DEFCONST A-MEM-LENGTH 2000)
(DEFCONST D-MEM-LENGTH 4000)

(DEFCONST NEEDED-CADREG-SYMBOLS
          'SI:(RAPC RASIR RAOBS RASTS
                    RACMO RACME RADME RAPBE RAM1E RAM2E RAAME RAUSE RAMME RAFSE RAFDE
                    RARGE RACSWE RARDRE RACIBE RAGO RASTOP RARDRO RAFDO RAOPCE
                    RARS RASTEP RASA RAAMO RAMMO RARCON RAPBO RAUSO RADMO RADME)
  "Symbols from CADREG that must be dumped in dumped modules.
This saves the user loading a dumped module from having to load CADREG.")

(DEFUN DUMP-MODULE (&OPTIONAL OUTPUT-FILE MODULE
                    &AUX IMAGE FIRST-USER-MODULE-P)
  "Write a QFASL file containing the assembly of in ucode module MODULE.
Loading this file will be equivalent to repeating the ADD-ASSEMBLY that made MODULE.
MODULE defaults to the last one assembled (loaded?).
OUTPUT-FILE defaults based on MODULE's source file."
  (UNLESS MODULE
    (SETQ MODULE (CAR (UCODE-IMAGE-MODULE-POINTS CURRENT-UCODE-IMAGE))))
  (SETQ IMAGE (UCODE-MODULE-IMAGE MODULE))
  (SETQ FIRST-USER-MODULE-P
        (NULL (CDDR (MEMQ MODULE
                          (UCODE-IMAGE-MODULE-POINTS IMAGE)))))
  (SETQ OUTPUT-FILE (FS:MERGE-PATHNAME-DEFAULTS (OR OUTPUT-FILE "")
                                                (SEND (UCODE-MODULE-SOURCE MODULE)
                                                      ':NEW-CANONICAL-TYPE ':QFASL)
                                                ':QFASL))
  (SI:DUMP-FORMS-TO-FILE OUTPUT-FILE
                         `(,@(WHEN FIRST-USER-MODULE-P
                               ;; (If not first user module, CADREG must already be there).
                               (MAPCAR #'(LAMBDA (SYM)
                                           `(SETQ ,SYM ,(SYMEVAL SYM)))
                                       NEEDED-CADREG-SYMBOLS))
                           (RELOAD-MODULE
                             ',(UCODE-MODULE-I-MEM-ALIST MODULE)
                             ',(UCODE-MODULE-D-MEM-ALIST MODULE)
                             ',(UCODE-MODULE-A-MEM-ALIST MODULE)
                             ',(UCODE-MODULE-SOURCE MODULE)
                             ',(UCODE-MODULE-ASSEMBLER-STATE MODULE)
                             ',(UCODE-IMAGE-ENTRY-POINTS-ARRAY IMAGE)
                             ',(UCODE-MODULE-DEFMICS MODULE)
                             ',(UCODE-MODULE-TABLE MODULE)
                             ',NIL
                             ',(MAPCAR 'UCODE-MODULE-GENERIC-PATHNAME
                                       (CDR (MEMQ MODULE
                                                  (UCODE-IMAGE-MODULE-POINTS IMAGE))))
                             ',%MICROCODE-VERSION-NUMBER
                             ',(WHEN FIRST-USER-MODULE-P
                                 ;; Only one previous module => it is the ucode,
                                 ;; so save data on it to don't need to read UCADR.SYM
                                 ;; to get the data.
                                 (UCODE-MODULE-ASSEMBLER-STATE
                                   (CADR (MEMQ MODULE
                                               (UCODE-IMAGE-MODULE-POINTS IMAGE)))))))
                         '(:PACKAGE :USER)))

;;; QFASL files containing saved out incremental assemblies
;;; contain calls to this function, which serves to recreate the module
;;; as if it had just been assembled.
(DEFUN RELOAD-MODULE (I-MEM D-MEM A-MEM PATHNAME ASSEMBLER-STATE-AFTER
                      ENTRY-POINTS-ARRAY DEFMICS TABLE IGNORE
                      PREV-MODULES
                      &OPTIONAL ASSEMBLED-UCODE-VERSION UCODE-ASSEMBLER-STATE
                      &AUX (IMAGE CURRENT-UCODE-IMAGE)
                      (GENERIC-PATHNAME (SEND PATHNAME ':GENERIC-PATHNAME)))
  (UNLESS (EQ ASSEMBLED-UCODE-VERSION %MICROCODE-VERSION-NUMBER)
    (FERROR NIL "File was dumped under a different microcode version."))
  (UNLESS (EQ %MICROCODE-VERSION-NUMBER (UCODE-IMAGE-VERSION IMAGE))
    (IF UCODE-ASSEMBLER-STATE
        (RELOAD-UCODE-VERSION %MICROCODE-VERSION-NUMBER IMAGE UCODE-ASSEMBLER-STATE)
      (READ-UCODE-VERSION %MICROCODE-VERSION-NUMBER IMAGE)))
  (LET ((ACTUAL-PREV-MODULES
          (MAPCAR 'UCODE-MODULE-GENERIC-PATHNAME (UCODE-IMAGE-MODULE-POINTS IMAGE))))
    (UNLESS (EQUAL ACTUAL-PREV-MODULES PREV-MODULES)
      (FERROR "Current microcode state does not match that of this file,~%which is ~A."
              PREV-MODULES)))
  (WHEN (EQ (UCODE-MODULE-GENERIC-PATHNAME (CAR (UCODE-IMAGE-MODULE-POINTS IMAGE)))
            GENERIC-PATHNAME)
    (FLUSH-MODULE NIL IMAGE))                   ;Evidently a new version, flush the old
  (DOLIST (D DEFMICS)
    (APPLY 'UA-DO-DEFMIC D))
  (LET ((MODULE (MAKE-UCODE-MODULE)))
    (MERGE-MEM-ALIST I-MEM SI:RACMO IMAGE)
    (MERGE-MEM-ALIST D-MEM SI:RADMO IMAGE)
    (MERGE-MEM-ALIST A-MEM SI:RAAMO IMAGE)
    (SETF (UCODE-MODULE-I-MEM-ALIST MODULE) I-MEM)
    (SETF (UCODE-MODULE-D-MEM-ALIST MODULE) D-MEM)
    (SETF (UCODE-MODULE-A-MEM-ALIST MODULE) A-MEM)
    (SETF (UCODE-MODULE-IMAGE MODULE) IMAGE)
    (SETF (UCODE-MODULE-SOURCE MODULE) PATHNAME)
    (SETF (UCODE-MODULE-GENERIC-PATHNAME MODULE) GENERIC-PATHNAME)
    (SETF (UCODE-MODULE-ASSEMBLER-STATE MODULE) ASSEMBLER-STATE-AFTER)
    (SETF (UCODE-IMAGE-ENTRY-POINTS-ARRAY IMAGE) ENTRY-POINTS-ARRAY)
    (SETF (UCODE-MODULE-ENTRY-POINTS-INDEX MODULE)
          (FILL-POINTER (UCODE-IMAGE-ENTRY-POINTS-ARRAY IMAGE)))
    (SETF (UCODE-MODULE-DEFMICS MODULE)
          DEFMICS)
    (SETF (UCODE-MODULE-TABLE MODULE)
          TABLE)
;   (SETF (UCODE-IMAGE-SYMBOL-ARRAY IMAGE) SYMBOL-ARRAY)
    (SETF (UCODE-MODULE-SYM-ADR MODULE)
          (FILL-POINTER (UCODE-IMAGE-SYMBOL-ARRAY IMAGE)))
    (SETF (UCODE-IMAGE-MODULE-POINTS IMAGE)
          (CONS MODULE
                (UCODE-IMAGE-MODULE-POINTS IMAGE)))))

(DEFUN UNLOAD-MODULE (&OPTIONAL MOD (IMAGE CURRENT-UCODE-IMAGE))
  (UNLESS MOD
    (SETQ MOD (CAR (UCODE-IMAGE-MODULE-POINTS IMAGE))))
  (UNLESS (EQ MOD (CAR (UCODE-IMAGE-MODULE-POINTS IMAGE)))
    (FERROR NIL "Must unload modules in reverse order loaded"))
  (WHEN (EQ (UCODE-IMAGE-MODULE-POINTS IMAGE)
            (UCODE-IMAGE-MODULE-LOADED IMAGE))
    (SETF (UCODE-IMAGE-MODULE-LOADED IMAGE)
          (CDR (UCODE-IMAGE-MODULE-POINTS IMAGE)))))

(DEFUN FLUSH-MODULE (&OPTIONAL MOD (IMAGE CURRENT-UCODE-IMAGE))
  (UNLESS MOD
    (SETQ MOD (CAR (UCODE-IMAGE-MODULE-POINTS IMAGE))))
  (UNLESS (EQ MOD (CAR (UCODE-IMAGE-MODULE-POINTS IMAGE)))
    (FERROR NIL "Must flush modules in reverse order loaded"))
  (WHEN (EQ (UCODE-IMAGE-MODULE-POINTS IMAGE)
            (UCODE-IMAGE-MODULE-LOADED IMAGE))
    (SETF (UCODE-IMAGE-MODULE-LOADED IMAGE)
          (CDR (UCODE-IMAGE-MODULE-POINTS IMAGE))))
  (SETF (UCODE-IMAGE-MODULE-POINTS IMAGE)
        (CDR (UCODE-IMAGE-MODULE-POINTS IMAGE))))

;;;UA-DEFMIC is called during readin phase for incremental assemblies.
;;;Dont do anything immediately, since the world might bomb
;;;  out before you really win. Just buffers it up for later processing.
;;;OPCODE is value to appear in MISC instructions.  The entry point is stored in
;;;  MICRO-CODE-SYMBOL-AREA at this location less 200.  The OPCODE can also be
;;;  NIL, in which case the system will assign the next available one.
;;;  Note, however, that there is a possible screw in using NIL in conjunction
;;;  with a QINTCMP property and compiling QFASL files to disk: the compiled file
;;;  might be loaded at a later time when the actual OPCODE was different and lose.
(DEFUN UA-DEFMIC (&QUOTE NAME OPCODE ARGLIST LISP-FUNCTION-P &OPTIONAL (NO-QINTCMP NIL))
  (SETQ CURRENT-ASSEMBLY-DEFMICS
        (CONS (LIST NAME OPCODE ARGLIST LISP-FUNCTION-P NO-QINTCMP)
              CURRENT-ASSEMBLY-DEFMICS)))

;;;This called on buffered stuff from UA:ASSEMBLE just before assembly actually done.
;;; ASSEMBLER-STATE environment has been established.
(DEFUN UA-DO-DEFMIC (NAME OPCODE ARGLIST LISP-FUNCTION-P NO-QINTCMP
                  &AUX FUNCTION-NAME INSTRUCTION-NAME MICRO-CODE-ENTRY-INDEX NARGS)
  (IF (ATOM NAME)
      (SETQ FUNCTION-NAME NAME INSTRUCTION-NAME NAME)
    (SETQ FUNCTION-NAME (CAR NAME) INSTRUCTION-NAME (CDR NAME)))
  (IF (NULL OPCODE)
      (SETQ OPCODE (OR (GET INSTRUCTION-NAME 'QLVAL) (UA-ASSIGN-MICRO-ENTRY NAME))))
  (PUTPROP INSTRUCTION-NAME OPCODE 'QLVAL)
  (SETQ NARGS (SI:ARGS-INFO-FROM-LAMBDA-LIST ARGLIST))
  (WHEN (OR (BIT-TEST NARGS %ARG-DESC-QUOTED-REST)
            (BIT-TEST NARGS %ARG-DESC-EVALED-REST)
            (BIT-TEST NARGS %ARG-DESC-INTERPRETED)
            (BIT-TEST NARGS %ARG-DESC-FEF-QUOTE-HAIR)
            (AND (NOT NO-QINTCMP)
                 (NOT (= (LDB %%ARG-DESC-MAX-ARGS NARGS)
                         (LDB %%ARG-DESC-MIN-ARGS NARGS)))))
    (FERROR NIL "~%The arglist of the function ~s, ~s, is too hairy to microcompile.
ARGS-INFO = ~O~%"
            NAME ARGLIST NARGS))
  (WHEN LISP-FUNCTION-P
    (SETQ MICRO-CODE-ENTRY-INDEX (ALLOCATE-MICRO-CODE-ENTRY-SLOT FUNCTION-NAME))
    (STORE (SYSTEM:MICRO-CODE-ENTRY-ARGLIST-AREA MICRO-CODE-ENTRY-INDEX) ARGLIST)
    (STORE (SYSTEM:MICRO-CODE-ENTRY-ARGS-INFO-AREA MICRO-CODE-ENTRY-INDEX) NARGS))
  (UNLESS NO-QINTCMP
    (PUTPROP INSTRUCTION-NAME (LDB %%ARG-DESC-MAX-ARGS NARGS) 'QINTCMP)
    (UNLESS (EQ FUNCTION-NAME INSTRUCTION-NAME)
      (PUTPROP FUNCTION-NAME (LDB %%ARG-DESC-MAX-ARGS NARGS) 'QINTCMP))))

(DEFUN UA-ASSIGN-MICRO-ENTRY (NAME)
  NAME
  (UNLESS (EQ CURRENT-ASSEMBLY-HIGHEST-MISC-ENTRY 0)
    (FERROR NIL "lossage assigning micro-entries"))
  (INCF CURRENT-ASSEMBLY-HIGHEST-MISC-ENTRY))

;;;Do this when module containing DEFMIC is actually loaded
(DEFUN UA-LOAD-DEFMIC (NAME OPCODE ARGLIST LISP-FUNCTION-P NO-QINTCMP
                  &AUX FUNCTION-NAME INSTRUCTION-NAME MICRO-CODE-ENTRY-INDEX
                       MICRO-CODE-SYMBOL-INDEX)  NO-QINTCMP ARGLIST
  (IF (ATOM NAME)
      (SETQ FUNCTION-NAME NAME INSTRUCTION-NAME NAME)
    (SETQ FUNCTION-NAME (CAR NAME) INSTRUCTION-NAME (CDR NAME)))
  (UNLESS (SETQ OPCODE (GET INSTRUCTION-NAME 'QLVAL))
    (FERROR NIL "OPCODE not assigned ~s" NAME))
  (SETQ MICRO-CODE-SYMBOL-INDEX (- OPCODE 200))
  (WHEN LISP-FUNCTION-P
    (LET ((FS (FSYMEVAL FUNCTION-NAME)))
      (IF ( (%DATA-TYPE FS) DTP-U-ENTRY)
          (FERROR NIL "Function cell of ~s not DTP-U-ENTRY" FUNCTION-NAME)
        (SETQ MICRO-CODE-ENTRY-INDEX (%POINTER FS)))))
  (LET ((PREV (AREF (FUNCTION SYSTEM:MICRO-CODE-ENTRY-AREA) MICRO-CODE-ENTRY-INDEX)))
    (WHEN (AND PREV (NOT (FIXP PREV)))
      (PUTPROP FUNCTION-NAME PREV 'DEFINITION-BEFORE-MICROCODED)))
  (SETF (AREF #'SYSTEM:MICRO-CODE-ENTRY-AREA MICRO-CODE-ENTRY-INDEX) MICRO-CODE-SYMBOL-INDEX))

;;;Call this to repair the damage if a reboot (either warm or cold) is done.
(DEFUN UA-REBOOT (&OPTIONAL (IMAGE CURRENT-UCODE-IMAGE))
  (DO () ((NULL (CDR (UCODE-IMAGE-MODULE-LOADED IMAGE))))
    (UNLOAD-MODULE (CAR (UCODE-IMAGE-MODULE-LOADED IMAGE))))
  (LOAD-MODULE NIL IMAGE))

(DEFF ALLOCATE-MICRO-CODE-ENTRY-SLOT 'COMPILER:ALLOCATE-MICRO-CODE-ENTRY-SLOT)

;;;NIL as module means load all
(DEFUN LOAD-MODULE (&OPTIONAL MODULE (IMAGE CURRENT-UCODE-IMAGE) &AUX AS TEM)
  (IF (NULL MODULE)
      (DOLIST (M (REVERSE (LDIFF (UCODE-IMAGE-MODULE-POINTS IMAGE)
                                 (UCODE-IMAGE-MODULE-LOADED IMAGE)))
                 T)
        (LOAD-MODULE M IMAGE))
    (UNLESS (EQ %MICROCODE-VERSION-NUMBER
                (UCODE-IMAGE-VERSION IMAGE))
      (FERROR NIL "Wrong Ucode Version: Machine ~S, Image ~S"
              %MICROCODE-VERSION-NUMBER (UCODE-IMAGE-VERSION IMAGE)))
    (SETQ AS (UCODE-MODULE-ASSEMBLER-STATE MODULE))
    (LET ((ARRAY (UCODE-IMAGE-CONTROL-MEMORY-ARRAY IMAGE))
          (RANGE-LIST (GETF AS 'I-MEMORY-RANGE-LIST)))
      (DOLIST (R RANGE-LIST)
        (DO ((ADR (CAR R) (1+ ADR))
             (CNT (CADR R) (1- CNT)))
            (( CNT 0))
          (WHEN (SETQ TEM (AREF ARRAY ADR))
            (SI:%WRITE-INTERNAL-PROCESSOR-MEMORIES
                        1 ADR
                        (%LOGDPB (LDB 4020 TEM) 1020 (LDB 3010 TEM))    ;Assure no bignums or
                        (%LOGDPB (LDB 1020 TEM) 1020 (LDB 0010 TEM)))))));sign bit lossage
    (LET ((ARRAY (UCODE-IMAGE-DISPATCH-MEMORY-ARRAY IMAGE))
          (RANGE-LIST (GETF AS 'D-MEMORY-RANGE-LIST)))
      (DOLIST (R RANGE-LIST)
        (DO ((ADR (CAR R) (1+ ADR))
             (CNT (CADR R) (1- CNT)))
            (( CNT 0))
          (WHEN (SETQ TEM (AREF ARRAY ADR))
            ;; Must write correct parity
            (SI:%WRITE-INTERNAL-PROCESSOR-MEMORIES
                        2 ADR                   ;D
                        0                       ;No high bits
                        (DPB (DO ((COUNT 17. (1- COUNT))
                                  (X TEM (LOGXOR TEM (LSH X -1))))
                                 ((= COUNT 0)
                                  (LOGXOR 1 X)))        ;Odd parity
                             2101
                             TEM))))))
    (LET ((ARRAY (UCODE-IMAGE-A-MEMORY-ARRAY IMAGE))
          (RANGE-LIST (GETF AS 'A-MEMORY-RANGE-LIST)))
      (DOLIST (R RANGE-LIST)
        (DO ((ADR (CAR R) (1+ ADR))
             (CNT (CADR R) (1- CNT))
             (IN-IMAGE (UCODE-IMAGE-A-MEMORY-LOCATION-IN-IMAGE IMAGE)))
            (( CNT 0))
          (WHEN (SETQ TEM (AREF ARRAY ADR))
            (SETF (AREF IN-IMAGE ADR) 1)
            (SI:%WRITE-INTERNAL-PROCESSOR-MEMORIES
                        4 ADR                   ;A/M
                        (%LOGDPB (LDB 4020 TEM) 1020 (LDB 3010 TEM))
                        (%LOGDPB (LDB 1020 TEM) 1020 (LDB 0010 TEM)))))))
    (DOLIST (E (GETF AS 'MICRO-ENTRIES))
      (LET ((IDX (IF (EQ (CAR E) 'MISC-INST-ENTRY)
                     (- (GET (CADR E) 'QLVAL)
                        200)
                   (FERROR NIL "Unknown micro-entry ~s" E))))
        (SETF (AREF #'SYSTEM:MICRO-CODE-SYMBOL-AREA IDX) (CADDR E))
        ;; Don't mark any of micro-code-symbol-area as used!
        ;; It is "free" as far as saving a LOD band is concerned;
        ;; the data comes from the MCR band.
;       (SI:MARK-NOT-FREE (AP-1 #'SYS:MICRO-CODE-SYMBOL-AREA IDX))
        (SETF (AREF (UCODE-IMAGE-ENTRY-POINTS-ARRAY IMAGE) IDX) (CADDR E))))
    (COND (NUMBER-MICRO-ENTRIES         ;in case machine has been warm booted.
           (SETQ SYSTEM:%NUMBER-OF-MICRO-ENTRIES NUMBER-MICRO-ENTRIES)))
    (DOLIST (X (UCODE-MODULE-DEFMICS MODULE))
      (APPLY (FUNCTION UA-LOAD-DEFMIC) X))
    (DO ((L (UCODE-IMAGE-MODULE-POINTS IMAGE) (CDR L))
         (C (UCODE-IMAGE-MODULE-LOADED IMAGE)))
        ((OR (NULL L) (EQ (CDR L) C))
         (COND ((AND L (EQ (CAR L) MODULE))
                (SETF (UCODE-IMAGE-MODULE-LOADED IMAGE) L)))))))

;;;Load into the other machine with CC.
(DEFUN CC-LOAD-MODULE (&OPTIONAL MODULE (IMAGE CC-UCODE-IMAGE) &AUX TEM AS)
  (IF (NULL MODULE)
      (DOLIST (M (REVERSE (LDIFF (UCODE-IMAGE-MODULE-POINTS IMAGE)
                                 (UCODE-IMAGE-MODULE-LOADED IMAGE)))
                 T)
        (CC-LOAD-MODULE M IMAGE))
    (UNLESS (EQ %MICROCODE-VERSION-NUMBER
                (UCODE-IMAGE-VERSION IMAGE))
      (FERROR NIL "Wrong ucode version, Machine ~S, Image ~S"
              %MICROCODE-VERSION-NUMBER (UCODE-IMAGE-VERSION IMAGE)))
    (SETQ AS (UCODE-MODULE-ASSEMBLER-STATE MODULE))
    (LET ((ARRAY (UCODE-IMAGE-CONTROL-MEMORY-ARRAY IMAGE))
          (RANGE-LIST (GETF AS 'I-MEMORY-RANGE-LIST)))
      (DOLIST (R RANGE-LIST)
        (DO ((ADR (CAR R) (1+ ADR))
             (CNT (CADR R) (1- CNT)))
            (( CNT 0))
          (WHEN (SETQ TEM (AREF ARRAY ADR))
            (CADR:CC-R-D (+ ADR RACMO) TEM))))) ;Sign bit lossage
    (LET ((ARRAY (UCODE-IMAGE-DISPATCH-MEMORY-ARRAY IMAGE))
          (RANGE-LIST (GETF AS 'D-MEMORY-RANGE-LIST)))
      (DOLIST (R RANGE-LIST)
        (DO ((ADR (CAR R) (1+ ADR))
             (CNT (CADR R) (1- CNT)))
            (( CNT 0))
          (WHEN (SETQ TEM (AREF ARRAY ADR))
            (CADR:CC-R-D (+ ADR RADMO) TEM)))))
    (LET ((ARRAY (UCODE-IMAGE-A-MEMORY-ARRAY IMAGE))
          (RANGE-LIST (GETF AS 'A-MEMORY-RANGE-LIST)))
      (DOLIST (R RANGE-LIST)
        (DO ((ADR (CAR R) (1+ ADR))
             (CNT (CADR R) (1- CNT))
             (IN-IMAGE (UCODE-IMAGE-A-MEMORY-LOCATION-IN-IMAGE IMAGE)))
            (( CNT 0))
          (WHEN (SETQ TEM (AREF ARRAY ADR))
            (SETF (AREF IN-IMAGE ADR) 1)
            (CADR:CC-R-D (+ ADR RAAMO) TEM)))))
    (DOLIST (E (GETF AS 'MICRO-ENTRIES))
      (LET ((IDX (IF (EQ (CAR E) 'MISC-INST-ENTRY)
                     (- (GET (CADR E) 'QLVAL)
                        200)
                   (FERROR NIL "Unknown micro-entry ~s" E))))
;       (ASET (CADDR E) (FUNCTION SYSTEM:MICRO-CODE-SYMBOL-AREA) IDX)
        (SETF (AREF (UCODE-IMAGE-ENTRY-POINTS-ARRAY IMAGE) IDX) (CADDR E))))
;    (DOLIST (X (UCODE-MODULE-DEFMICS MODULE))
;      (APPLY (FUNCTION UA-LOAD-DEFMIC) X))
    ))

(DEFUN BLAST-WITH-IMAGE (&OPTIONAL (IMAGE CURRENT-UCODE-IMAGE) &AUX TEM)
  (UNLESS (EQ %MICROCODE-VERSION-NUMBER
              (UCODE-IMAGE-VERSION IMAGE))
    (FERROR NIL "Wrong ucode version, Machine ~S, Image ~S"
            %MICROCODE-VERSION-NUMBER (UCODE-IMAGE-VERSION IMAGE)))
  (LET ((ARRAY (UCODE-IMAGE-CONTROL-MEMORY-ARRAY IMAGE)))
    (DOTIMES (ADR (ARRAY-LENGTH ARRAY))
      (WHEN (SETQ TEM (AREF ARRAY ADR))
      (SI:%WRITE-INTERNAL-PROCESSOR-MEMORIES 1 ADR
                (%LOGDPB (LDB 4020 TEM) 1020 (LDB 3010 TEM))  ;Assure no bignums or
                (%LOGDPB (LDB 1020 TEM) 1020 (LDB 0010 TEM)))))) ;sign bit lossage
  ;doesn't load RPN bits properly
  (LET ((ARRAY (UCODE-IMAGE-DISPATCH-MEMORY-ARRAY IMAGE)))
    (DO ((ADR 0 (1+ ADR))
         (LIM (ARRAY-LENGTH ARRAY)))
        (( ADR LIM))
      (WHEN (SETQ TEM (AREF ARRAY ADR))
        (SI:%WRITE-INTERNAL-PROCESSOR-MEMORIES 2 ADR    ;D
                (%LOGDPB (LDB 4020 TEM) 1020 (LDB 3010 TEM))
                (%LOGDPB (LDB 1020 TEM) 1020 (LDB 0010 TEM))))))
;  (LET ((ARRAY (UCODE-IMAGE-A-MEMORY-ARRAY IMAGE)))
;    (DO ((ADR 0 (1+ ADR))
;        (LIM (ARRAY-LENGTH ARRAY))
;        (IN-IMAGE (UCODE-IMAGE-A-MEMORY-LOCATION-IN-IMAGE IMAGE)))  ;Hmm really loses.
;       (( ADR LIM))
;      (COND ((AND (NOT (ZEROP (AREF IN-IMAGE ADR)))
;                 (NOT (NULL (SETQ TEM (AREF ARRAY ADR)))))
;            (SI:%WRITE-INTERNAL-PROCESSOR-MEMORIES 4 ADR       ;A/M
;                   (%LOGDPB (LDB 4020 TEM) 1020 (LDB 3010 TEM))
;                   (%LOGDPB (LDB 1020 TEM) 1020 (LDB 0010 TEM)))))))
  )

(DEFUN MERGE-MEM-ALIST (ALIST RA-ORG IMAGE)
  "Store the contents of ALIST into registers in IMAGE starting at address RA-ORG.
Each element of ALIST looks like (OFFSET . VALUE), and VALUE
is stored in the register at address (+ RA-ORG OFFSET)."
  (DOLIST (ELT ALIST)
    (WHEN (CDR ELT)
      (CC-IMAGE-REGISTER-DEPOSIT IMAGE NIL (+ RA-ORG (CAR ELT)) (CDR ELT) T))))

(DEFUN MERGE-MEM-ARRAY (ARRAY RA-ORG IMAGE)
  "Store the contents of ARRAY into registers in IMAGE starting at address RA-ORG.
One element of ARRAY goes into each register."
  (DOTIMES (IDX (LENGTH ARRAY))
    (CC-IMAGE-REGISTER-DEPOSIT IMAGE NIL (+ RA-ORG IDX) (AREF ARRAY IDX) T)))

(DEFUN EXTRACT-MEM-ARRAY (RA-ORG LENGTH IMAGE &AUX (ARRAY (MAKE-ARRAY LENGTH)))
  "Return an array holding the contents of LENGTH registers of IMAGE starting at addr RA-ORG."
  (DOTIMES (I LENGTH ARRAY)
    (SETF (AREF ARRAY I)
          (CC-IMAGE-REGISTER-EXAMINE IMAGE NIL (+ RA-ORG I)))))

;;;Like READ-UCODE-VERSION, but works based on information
;;; passed as args (presumably stored in a QFASL file with a user module).
(DEFUN RELOAD-UCODE-VERSION (&OPTIONAL (VERSION %MICROCODE-VERSION-NUMBER)
                             (IMAGE CURRENT-UCODE-IMAGE)
                             ASSEMBLER-STATE)
  (PKG-BIND "UA"
;   (READ-SYM-FILE VERSION IMAGE)
    (SETF (UCODE-IMAGE-ASSEMBLER-STATE IMAGE)
          ASSEMBLER-STATE)
;    (READ-MCR-FILE VERSION IMAGE)
;    (READ-TABLE-FILE VERSION IMAGE)
    (SETF (UCODE-IMAGE-VERSION IMAGE) VERSION)
    (LET ((MODULE (MAKE-UCODE-MODULE)))
      (SETF (UCODE-MODULE-IMAGE MODULE) IMAGE)
      (SETF (UCODE-MODULE-SOURCE MODULE)
            ;; Fake what directory this came from
            (SEND (FS:PARSE-PATHNAME "SYS: UBIN; UCADR LISP >") ':NEW-VERSION VERSION))
      (SETF (UCODE-MODULE-ASSEMBLER-STATE MODULE)
            (UCODE-IMAGE-ASSEMBLER-STATE IMAGE))
      (SETF (UCODE-MODULE-ENTRY-POINTS-INDEX MODULE)
            (FILL-POINTER (UCODE-IMAGE-ENTRY-POINTS-ARRAY IMAGE)))
      (SETF (UCODE-MODULE-TABLE MODULE)
            (UCODE-IMAGE-TABLE-LOADED IMAGE))
      (SETF (UCODE-MODULE-SYM-ADR MODULE)
            (FILL-POINTER (UCODE-IMAGE-SYMBOL-ARRAY IMAGE)))
      (SETF (UCODE-IMAGE-MODULE-LOADED IMAGE)
            (SETF (UCODE-IMAGE-MODULE-POINTS IMAGE) (LIST MODULE))))
    ))

(DEFUN READ-UCODE-VERSION (&OPTIONAL (VERSION %MICROCODE-VERSION-NUMBER)
                           (IMAGE CURRENT-UCODE-IMAGE))
  (PKG-BIND "UA"
    (UNLESS (BOUNDP 'SI:RACMO)
      (READFILE "SYS: CC; CADREG LISP >"))
;   (READ-SYM-FILE VERSION IMAGE)
    (UCODE-IMAGE-STORE-ASSEMBLER-STATE (GET-UCADR-STATE-LIST VERSION) IMAGE)
;    (READ-MCR-FILE VERSION IMAGE)
;    (READ-TABLE-FILE VERSION IMAGE)
    (SETF (UCODE-IMAGE-VERSION IMAGE) VERSION)
    (LET ((MODULE (MAKE-UCODE-MODULE)))
      (SETF (UCODE-MODULE-IMAGE MODULE) IMAGE)
      (SETF (UCODE-MODULE-SOURCE MODULE)
            ;; Fake what directory this came from
            (SEND (FS:PARSE-PATHNAME "SYS: UBIN; UCADR LISP >") ':NEW-VERSION VERSION))
      (SETF (UCODE-MODULE-ASSEMBLER-STATE MODULE)
            (UCODE-IMAGE-ASSEMBLER-STATE IMAGE))
      (SETF (UCODE-MODULE-ENTRY-POINTS-INDEX MODULE)
            (FILL-POINTER (UCODE-IMAGE-ENTRY-POINTS-ARRAY IMAGE)))
      (SETF (UCODE-MODULE-TABLE MODULE)
            (UCODE-IMAGE-TABLE-LOADED IMAGE))
      (SETF (UCODE-MODULE-SYM-ADR MODULE)
            (FILL-POINTER (UCODE-IMAGE-SYMBOL-ARRAY IMAGE)))
      (SETF (UCODE-IMAGE-MODULE-LOADED IMAGE)
            (SETF (UCODE-IMAGE-MODULE-POINTS IMAGE) (LIST MODULE))))
    ))

(DEFUN READ-TABLE-FILE (VERSION &OPTIONAL (IMAGE CURRENT-UCODE-IMAGE))
  (WITH-OPEN-FILE (STREAM (IF (NUMBERP VERSION)
                              (SEND (FS:PARSE-PATHNAME "SYS: UBIN; UCADR")
                                    ':NEW-TYPE-AND-VERSION "TBL" VERSION)
                            VERSION)
                          ':DIRECTION ':INPUT)
    (READ STREAM)                       ;Flush (SETQ MICROCODE-ERROR-TABLE-VERSION-NUMBER ..)
    (LET ((TABLE (READ STREAM)))        ;Gobble (SETQ MICROCODE-ERROR-TABLE '(...))
      (SETF (UCODE-IMAGE-TABLE-LOADED IMAGE)
            (CADR (CADDR TABLE))))))    ;Flush SETQ, QUOTE, etc.

(DEFUN GET-UCADR-STATE-LIST (&OPTIONAL (VERSION %MICROCODE-VERSION-NUMBER) &AUX ITEM)
  (WITH-OPEN-FILE (STREAM (SEND (FS:PARSE-PATHNAME "SYS: UBIN; UCADR")
                                ':NEW-TYPE-AND-VERSION "SYM" VERSION)
                          ':DIRECTION ':INPUT)
    (DO-FOREVER
      (SETQ ITEM (READ STREAM))
      (AND (< ITEM 0)
           (SELECTQ ITEM
             ((-1 -2) (RETURN NIL))
             (-4 (RETURN (READ STREAM)))
             (OTHERWISE (FERROR NIL "~O is not a valid block header" ITEM)))))))

;;;Don't do this by default any more
(DEFUN READ-SYM-FILE (VERSION &OPTIONAL (IMAGE CURRENT-UCODE-IMAGE))
  (LET ((FILENAME (IF (NUMBERP VERSION)
                      (SEND (FS:PARSE-PATHNAME "SYS: UBIN; UCADR")
                            ':NEW-TYPE-AND-VERSION "SYM" VERSION)
                    VERSION)))
    (WITH-OPEN-FILE (STREAM FILENAME ':DIRECTION ':INPUT)
      (LET (ITEM SYM TYPE VAL SYM-ARRAY)
        (DO-FOREVER
          (DO () ((> (SETQ ITEM (READ-SIGNED-OCTAL-FIXNUM STREAM)) 0)))
          (DO-FOREVER
            (SELECTQ ITEM
              (-1 (RETURN-FROM READ-SYM-FILE NIL))
              (-2 (SETQ SYM-ARRAY (UCODE-IMAGE-SYMBOL-ARRAY IMAGE))
                  (SETF (FILL-POINTER SYM-ARRAY) 0)
                  (DO ((ITEM (READ STREAM) (READ STREAM)))
                      ((AND (NUMBERP ITEM)
                            (MINUSP SYM)))
                    (SETQ TYPE (READ STREAM) VAL (READ STREAM))
                    (VECTOR-PUSH-EXTEND ITEM SYM-ARRAY 1000)
                    (VECTOR-PUSH-EXTEND TYPE SYM-ARRAY 1000)
                    (VECTOR-PUSH-EXTEND VAL SYM-ARRAY 1000)))
              (-4 (UCODE-IMAGE-STORE-ASSEMBLER-STATE (READ STREAM) IMAGE)
                  (RETURN))
              (OTHERWISE (FERROR NIL "~O is not a valid block header" ITEM)))))))))

(DEFUN UA-DEFINE-SYMS (&OPTIONAL (IMAGE CURRENT-UCODE-IMAGE))
  ;;Cause symbols to exist. Temporarily CONS-LAP-SYM.
  (LET ((SYM-ARRAY (UCODE-IMAGE-SYMBOL-ARRAY IMAGE)))
    (COND (T
         ; (NULL (GET (AREF SYM-ARRAY 0) 'CONS-LAP-SYM)) ;Save time if looks like it's there
           (DO ((ADR 0 (+ ADR 3))
                (LIM (ARRAY-ACTIVE-LENGTH SYM-ARRAY)))
               (( ADR LIM))
             (LET ((SYM (AREF SYM-ARRAY ADR))
                   (TYPE (AREF SYM-ARRAY (1+ ADR)))
                   (VAL (AREF SYM-ARRAY (+ 2 ADR))))
               (PUTPROP SYM
                        (COND ((EQ TYPE 'NUMBER)
                               VAL)
                              (T
                               (LIST TYPE
                                (CONS 'FIELD
                                  (COND ((EQ TYPE 'I-MEM)
                                         (LIST 'JUMP-ADDRESS-MULTIPLIER VAL))
                                        ((EQ TYPE 'A-MEM)
                                         (LIST 'A-SOURCE-MULTIPLIER VAL))
                                        ((EQ TYPE 'M-MEM)
                                         (LIST 'M-SOURCE-MULTIPLIER VAL))
                                        ((EQ TYPE 'D-MEM)
                                         (LIST 'DISPATCH-ADDRESS-MULTIPLIER VAL))
                                        (T (FERROR NIL
"~%The symbol ~S has bad type ~S. Its value is ~S" SYM TYPE VAL)) )))))
                        'CONS-LAP-SYM)))))))

(DEFUN READ-MCR-FILE (VERSION &OPTIONAL (IMAGE CURRENT-UCODE-IMAGE))
  (UNLESS (NUMBERP VERSION)
    (FORMAT T "~& Please type microcode version number (decimal): ")
    (SETQ VERSION (LET ((*READ-BASE* 10.)) (READ))))
  (LET ((VERSION-NUMBER VERSION)
        (FILENAME (SEND (FS:PARSE-PATHNAME "SYS: UBIN; UCADR")
                             ':NEW-TYPE-AND-VERSION "MCR" VERSION)))
    (SETF (UCODE-IMAGE-VERSION IMAGE) VERSION-NUMBER)
    (WITH-OPEN-FILE (STREAM FILENAME ':DIRECTION ':INPUT ':CHARACTERS NIL ':BYTE-SIZE 16.)
      (DO (HCODE LCODE HADR LADR HCOUNT LCOUNT HD LD UDSP-NBLKS UDSP-RELBLK)
          (())
        (SETQ HCODE (SEND STREAM ':TYI) LCODE (SEND STREAM ':TYI))
        (UNLESS (AND (ZEROP HCODE) ( 0 LCODE 5))
          (FERROR NIL "Bad Code: HCODE=~O LCODE=~O" HCODE LCODE))
        (SETQ HADR (SEND STREAM ':TYI) LADR (SEND STREAM ':TYI))
        (SETQ HCOUNT (SEND STREAM ':TYI) LCOUNT (SEND STREAM ':TYI))
        (UNLESS (AND (ZEROP HADR)
                     (ZEROP HCOUNT))
          (FERROR NIL "Bad header SA ~O,~O Count ~O,~O"
                  HADR LADR HCOUNT LCOUNT))
        (COND ((ZEROP LCODE)
               (WHEN UDSP-NBLKS
                 (SEND STREAM ':SET-POINTER (* 2 UDSP-RELBLK SI:PAGE-SIZE))
                 (DO ((UE-ARRAY (UCODE-IMAGE-ENTRY-POINTS-ARRAY IMAGE))
                      (ADR 0 (1+ ADR))
                      (FIN (* UDSP-NBLKS SI:PAGE-SIZE)))
                     ((= ADR FIN))
                   (SETF (AREF UE-ARRAY ADR)
                         (DPB (SEND STREAM ':TYI)
                              2020
                              (DPB (SEND STREAM ':TYI)
                                   0020
                                   0)))))
               (RETURN-FROM READ-MCR-FILE IMAGE))
              ((= LCODE 1)                      ;I-MEM
               (DO () ((MINUSP (DECF LCOUNT)))
                 (SETF (AREF (UCODE-IMAGE-CONTROL-MEMORY-ARRAY IMAGE) LADR)
                       (DPB (SEND STREAM ':TYI) 6020
                            (DPB (SEND STREAM ':TYI) 4020
                                 (DPB (SEND STREAM ':TYI) 2020
                                      (DPB (SEND STREAM ':TYI) 0020 0)))))
                 (INCF LADR)))
              ((= LCODE 2)                      ;D-MEM
               (DO () ((MINUSP (DECF LCOUNT)))
                 (SETF (AREF (UCODE-IMAGE-DISPATCH-MEMORY-ARRAY IMAGE) LADR)
                       (DPB (SEND STREAM ':TYI) 1020
                            (DPB (SEND STREAM ':TYI) 0020 0)))
                 (INCF LADR)))
              ((= LCODE 3)              ;Ignore main memory load
               (SETQ UDSP-NBLKS LADR)
               (SETQ UDSP-RELBLK LCOUNT)
               (SETQ HD (SEND STREAM ':TYI) LD (SEND STREAM ':TYI))) ;Phys mem adr
              ((= LCODE 4)                      ;A-MEM
               (DO () ((MINUSP (DECF LCOUNT)))
                 (SETF (AREF (UCODE-IMAGE-A-MEMORY-ARRAY IMAGE) LADR)
                       (DPB (SEND STREAM ':TYI) 2020
                            (DPB (SEND STREAM ':TYI) 0020 0)))
                 (SETF (AREF (UCODE-IMAGE-A-MEMORY-LOCATION-IN-IMAGE IMAGE) LADR) 1)
                 (INCF LADR))))))))

;;; Following code adopted from CC.  Eventually, it would be nice for CC
;;;to be able to operate interchangably on either a ucode-image, ucode-state
;;;in the home machine, or on a remote machine via the debugging interface.
;;;Due to lack of bignums and lots of other reasons, we are not really trying to
;;;accomplish this now.  However, we are trying to keep the structure of things
;;;as much CC like as possible to simplify doing this in the future.

(DEFUN CC-IMAGE-PRINT-REG-ADR-CONTENTS (IMAGE STATE ADR)
 (PROG (DATA)
;       (SETQ RANGE (CC-IMAGE-FIND-REG-ADR-RANGE ADR))
        (SETQ DATA (CC-IMAGE-REGISTER-EXAMINE IMAGE STATE ADR))
;       (COND ((MEMQ RANGE '(C CIB))
;               (CC-TYPE-OUT DATA CC-UINST-DESC T))
;             ((MEMQ RANGE '(U OPC))
;               (CC-IMAGE-PRINT-ADDRESS (+ DATA SI:RACMO))
;               (PRINC '/ ))
;             ((EQ RANGE 'RAIDR)
;               (CC-IMAGE-PRINT-ADDRESS DATA) (PRINC '/ ))
;             (T (PRIN1-THEN-SPACE DATA)))
        (PRIN1-THEN-SPACE DATA)
        (PRINC '/ / )))

(DEFUN CC-IMAGE-REGISTER-EXAMINE (IMAGE STATE ADR)
  (MULTIPLE-VALUE-BIND (RANGE IDX) (CC-IMAGE-FIND-REG-ADR-RANGE ADR)
    (COND ((EQ RANGE 'C)
           (AREF (UCODE-IMAGE-CONTROL-MEMORY-ARRAY IMAGE)
                 IDX))
          ((EQ RANGE 'D)
           (AREF (UCODE-IMAGE-DISPATCH-MEMORY-ARRAY IMAGE)
                 IDX))
          ((EQ RANGE 'P)
           (AREF (UCODE-STATE-PDL-BUFFER-ARRAY STATE)
                 IDX))
          ((EQ RANGE '/1)
           (AREF (UCODE-STATE-LEVEL-1-MAP STATE)
                 IDX))
          ((EQ RANGE '/2)
           (AREF (UCODE-STATE-LEVEL-2-MAP STATE)
                 IDX))
          ((EQ RANGE 'A)
           (COND ((ZEROP (AREF (UCODE-IMAGE-A-MEMORY-LOCATION-IN-IMAGE
                                 IMAGE)
                               IDX))
                  (AREF (UCODE-IMAGE-A-MEMORY-ARRAY IMAGE) IDX))
                 (STATE (AREF (UCODE-STATE-A-MEMORY-ARRAY STATE) IDX))))
          ((EQ RANGE 'U)
           (AREF (UCODE-STATE-MICRO-STACK-ARRAY STATE)
                 IDX))
          (T (FERROR NIL "~S is not a valid range for ~O" RANGE ADR))) ))

(DEFUN CC-IMAGE-REGISTER-DEPOSIT (IMAGE STATE ADR DATA &OPTIONAL IMAGE-FLAG)
  (MULTIPLE-VALUE-BIND (RANGE IDX) (CC-IMAGE-FIND-REG-ADR-RANGE ADR)
    (COND ((EQ RANGE 'C)
           (SETF (AREF (UCODE-IMAGE-CONTROL-MEMORY-ARRAY IMAGE) IDX) DATA))
          ((EQ RANGE 'D)
           (SETF (AREF (UCODE-IMAGE-DISPATCH-MEMORY-ARRAY IMAGE) IDX) DATA))
          ((EQ RANGE 'P)
           (SETF (AREF (UCODE-STATE-PDL-BUFFER-ARRAY STATE) IDX) DATA))
          ((EQ RANGE '/1)
           (SETF (AREF (UCODE-STATE-LEVEL-1-MAP STATE) IDX) DATA))
          ((EQ RANGE '/2)
           (SETF (AREF (UCODE-STATE-LEVEL-2-MAP STATE) IDX) DATA))
          ((EQ RANGE 'A)
           (SETF (AREF (UCODE-IMAGE-A-MEMORY-LOCATION-IN-IMAGE IMAGE) IDX)
                 (IF IMAGE-FLAG 0 1))
           (SETF (AREF (IF IMAGE-FLAG
                           (UCODE-IMAGE-A-MEMORY-ARRAY IMAGE)
                         (UCODE-STATE-A-MEMORY-ARRAY STATE))
                       IDX)
                 DATA))
          ((EQ RANGE 'U)
           (SETF (AREF (UCODE-STATE-MICRO-STACK-ARRAY STATE) IDX) DATA))
          (T (FERROR NIL "~S is not a valid range for ~O" RANGE ADR)))))

;;;Returns symbol type and value or NIL, not assq list element as in CC.
(DEFUN CC-IMAGE-EVAL-SYM (IMAGE SYM &AUX (SYMTAB (UCODE-IMAGE-SYMBOL-ARRAY IMAGE)))
  (DO ((IDX 0 (+ 3 IDX)))
      ((OR ( IDX (FILL-POINTER SYMTAB))
          (EQ SYM (AREF SYMTAB IDX)))
       (IF ( IDX (FILL-POINTER SYMTAB)) NIL
         (VALUES (AREF SYMTAB (1+ IDX))
                 (AREF SYMTAB (+ IDX 2)))))))

;;;Returns: NIL if none found closer than 20 to desired reg adr
;;;         SYMBOL if exact match found
;;;         (LIST SYMBOL DIFFERENCE) if one found closer than 20

;****
(DEFUN CC-IMAGE-FIND-CLOSEST-SYM (IMAGE REG-ADR)
  (PROG (BSF BSF-VAL VAL SYMTAB IDX LIM)
        (SETQ BSF-VAL 0)
        (SETQ SYMTAB (UCODE-IMAGE-SYMBOL-ARRAY IMAGE))
        (SETQ IDX 0 LIM (FILL-POINTER SYMTAB))
   L    (COND ((NOT (< IDX LIM)) (GO X))
              ((= REG-ADR (SETQ VAL (AREF SYMTAB (1+ IDX))))
                (RETURN (AREF SYMTAB IDX)))
              ((AND (> VAL BSF-VAL)
                    (< VAL REG-ADR))
                (SETQ BSF (AREF SYMTAB IDX))
                (SETQ BSF-VAL VAL)))
        (SETQ IDX (+ IDX 3))
        (GO L)
  X     (COND ((OR (NULL BSF)
                   (> (- REG-ADR BSF-VAL) 20))
                 (RETURN NIL))
              (T (RETURN (LIST BSF (- REG-ADR BSF-VAL)))))
))

(DEFUN CC-IMAGE-FIND-REG-ADR-RANGE (REG-ADR)
  (COND ((< REG-ADR SI:RACMO) (VALUES 'TOO-LOW 0))
        ((< REG-ADR SI:RACME) (VALUES 'C (- REG-ADR SI:RACMO)))
        ((< REG-ADR SI:RADME) (VALUES 'D (- REG-ADR SI:RACME)))
        ((< REG-ADR SI:RAPBE) (VALUES 'P (- REG-ADR SI:RADME)))
        ((< REG-ADR SI:RAM1E) (VALUES '/1 (- REG-ADR SI:RAPBE)))
        ((< REG-ADR SI:RAM2E) (VALUES '/2 (- REG-ADR SI:RAM1E)))
        ((< REG-ADR SI:RAAME) (VALUES 'A (- REG-ADR SI:RAM2E)))
        ((< REG-ADR SI:RAUSE) (VALUES 'U (- REG-ADR SI:RAAME)))
        ((< REG-ADR SI:RAMME) (VALUES 'A (- REG-ADR SI:RAUSE))) ;M-MEM
        (T (VALUES 'TOO-HIGH 0))
;       ((< REG-ADR SI:RAFSE) 'FS)
;       ((< REG-ADR SI:RAFDE) 'FD)
;       ((< REG-ADR SI:RARGE) 'CC)
;       ((< REG-ADR SI:RACSWE) 'CSW)
;       ((< REG-ADR SI:RARDRE) 'RAIDR)
;       ((< REG-ADR SI:RACIBE) 'CIB)
;       ((< REG-ADR SI:RAOPCE) 'OPC)
;       ((< REG-ADR CC-REG-ADR-PHYS-MEM-OFFSET) 'TOO-HIGH)
;       ((< REG-ADR CC-REG-ADR-VIRT-MEM-OFFSET) 'PHYSICAL)
;       (T 'VIRTUAL)
        ))

(DEFPROP C SI:RACMO CC-LOWEST-ADR)
(DEFPROP D SI:RADMO CC-LOWEST-ADR)
(DEFPROP P SI:RAPBO CC-LOWEST-ADR)
(DEFPROP /1 SI:RAM1O CC-LOWEST-ADR)
(DEFPROP /2 SI:RAM2O CC-LOWEST-ADR)
(DEFPROP A SI:RAAMO CC-LOWEST-ADR)
(DEFPROP U SI:RAUSO CC-LOWEST-ADR)
(DEFPROP M SI:RAMMO CC-LOWEST-ADR)
;(DEFPROP FS SI:RAFSO CC-LOWEST-ADR)
;(DEFPROP FD SI:RAFDO CC-LOWEST-ADR)
;(DEFPROP CC SI:RARGO CC-LOWEST-ADR)
;(DEFPROP CSW SI:RACSWO CC-LOWEST-ADR)
;(DEFPROP RAIDR SI:RARDRO CC-LOWEST-ADR)
;(DEFPROP CIB SI:RACIBO CC-LOWEST-ADR)
;(DEFPROP OPC SI:RAOPCO CC-LOWEST-ADR)

(DEFPROP C C CC-@-NAME)
(DEFPROP D D CC-@-NAME)
(DEFPROP P P CC-@-NAME)
(DEFPROP /1 1 CC-@-NAME)
(DEFPROP /2 2 CC-@-NAME)
(DEFPROP A A CC-@-NAME)
(DEFPROP U U CC-@-NAME)

(DEFUN CC-IMAGE-PRINT-ADDRESS (IMAGE REG-ADR &AUX RANGE-NAME RANGE-BASE @-NAME TEM)
  (SETQ RANGE-NAME (CC-IMAGE-FIND-REG-ADR-RANGE REG-ADR))
  (COND ((AND (SETQ TEM (CC-IMAGE-FIND-CLOSEST-SYM IMAGE REG-ADR))
              (OR (ATOM TEM)
                  (EQ RANGE-NAME 'C)
                  (EQ RANGE-NAME 'D)))
         (PRIN1 TEM))
        ((SETQ RANGE-BASE (GET RANGE-NAME 'CC-LOWEST-ADR))
         (COND ((SETQ @-NAME (GET RANGE-NAME 'CC-@-NAME))
                (PRIN1 (- REG-ADR (SYMEVAL RANGE-BASE)))
                (TYO #\@)
                (PRIN1 @-NAME))
               (T (PRIN1 RANGE-NAME)
                  (TYO #\ )
                  (PRIN1 (- REG-ADR (SYMEVAL RANGE-BASE))))))
        (T (PRIN1 REG-ADR)))
  T)

(DEFUN PREPARE-FOR-UINST-COUNTING NIL
  (READ-MCR-FILE %MICROCODE-VERSION-NUMBER)
  (READ-SYM-FILE %MICROCODE-VERSION-NUMBER))

;;;Set statistics bit for uinsts in given ranges.  A range is a list (<start> <end>).
;;;  Each of these can be
;;;    a number which is a C-MEM address,
;;;    a symbol which is defined in UCADR,
;;;    a list of a symbol and a number, which in N instructions after SYMBOL.
;;;  Also, in <end> the special symbol * has the value of <start>.

(DEFUN MARK-UINST-RANGES (RANGES &OPTIONAL (IMAGE CURRENT-UCODE-IMAGE))
  (LET* ((ARRAY (UCODE-IMAGE-CONTROL-MEMORY-ARRAY IMAGE))
         (LIM (ARRAY-LENGTH ARRAY)))
    (DOTIMES (ADR LIM)
      (LET ((VAL (AREF ARRAY ADR)))
        (IF VAL (SETF (AREF ARRAY ADR) (BOOLE 2 1_46. VAL)))))  ;clear bits
    (DOLIST (RANGE RANGES)
      (LET* ((START (MARK-UINST-EVAL IMAGE (CAR RANGE)))
             (END (MARK-UINST-EVAL IMAGE (CADR RANGE) START)))
        (DO ((ADR START (1+ ADR)))
            (( ADR END))
          (LET ((VAL (AREF ARRAY ADR)))
            (IF VAL (SETF (AREF ARRAY ADR) (LOGIOR 1_46. VAL)))))))
    (BLAST-WITH-IMAGE)
    NIL))

(DEFCONST PAGE-FAULT-RANGES '( (PGF-R XCPGS)))

  ; 21% (process-sleep 60.)
  ; 7.7% (who-uses 'foobarbletch "si")
  ; 2.9% (apropos "foobarbletch" "si")
  ; 36.2 (worst-case-test)
(DEFCONST FIRST-LEVEL-MAP-RELOAD-RANGES
          '((LEVEL-1-MAP-MISS ADVANCE-SECOND-LEVEL-MAP-REUSE-POINTER)))

  ; 42% (process-sleep 60.)
  ; 17.2% (who-uses 'foobarbletch "si")
  ; 7.9% (apropos "foobarbletch" "si")
  ; 57.5 (worst-case-test)
  ; 17.2 (compile 'add-assembly)
(DEFCONST ALL-MAP-FAULT-EXCEPT-DISK-WAIT-RANGES '(
  ;attempts to measure map reloads for stuff in core
  (PGF-R-SB SBSER)
  (PGF-R-I PGF-R-PDL)                   ;do not include PDL-BUFFER-FAULTS
  (PGF-SAVE LEVEL-1-MAP-MISS)
  (LEVEL-1-MAP-MISS PGF-MAP-MISS)
  (PGF-MAP-MISS PGF-MAR)                ;not MAR, A-MEM faults, MPV, WR-RDONLY, PGF-RWF
  (PGF-RL SEARCH-PAGE-HASH-TABLE)
  (SEARCH-PAGE-HASH-TABLE XCPH)         ;not %COMPUTE-PAGE-HASH
  (COMPUTE-PAGE-HASH SWAPIN)))



  ; 4.3% (process-sleep 60.)
  ; 2.1% (who-uses 'foobarbletch" "si")
  ; 1.4% (apropos "foobarbletch" "si")
  ; 7.7% (worst-case-test)
  ; 2.4% (compile 'add-assembly)
(DEFCONST SEARCH-PAGE-HASH-RANGE '( (SEARCH-PAGE-HASH-TABLE XCPH)
                                   (COMPUTE-PAGE-HASH SWAPIN)))

(DEFCONST ALL-MICROCODE-RANGE `( (0 ,SI:SIZE-OF-HARDWARE-CONTROL-MEMORY)))


(DEFUN MARK-UINST-EVAL (IMAGE SPEC &OPTIONAL START-VALUE)
  (COND ((NUMBERP SPEC) SPEC)
        ((SYMBOLP SPEC)
         (IF (EQ SPEC '*)
             START-VALUE
             (MULTIPLE-VALUE-BIND (TYPE VAL)
                 (CC-IMAGE-EVAL-SYM IMAGE SPEC)
               (IF (NOT (EQ TYPE 'I-MEM))
                   (FERROR NIL "wrong type")
                   VAL))))
        (T (+ (MARK-UINST-EVAL (CAR SPEC) START-VALUE) (CADR SPEC)))))

(DEFUN READ-STATISTICS-COUNTER ()
  (DPB (%UNIBUS-READ 766036) 2020 (%UNIBUS-READ 766034)))

(DEFUN USTAT (SECS)
  (LET ((TEM (READ-STATISTICS-COUNTER)))
    (PROCESS-SLEEP SECS)
    (FORMAT T "~%~D" (- (READ-STATISTICS-COUNTER) TEM))))

(DEFMACRO USTAT-MACRO (&BODY BODY)
  `(PROGN
     'COMPILE
     (PRINT ',BODY)
     (WRITE-METER 'SYS:%DISK-WAIT-TIME 0)
     (WRITE-METER 'SYS:%COUNT-SECOND-LEVEL-MAP-RELOADS 0)
     (WRITE-METER 'SYS:%COUNT-FIRST-LEVEL-MAP-RELOADS 0)
     (LET ((TEMP2 (READ-STATISTICS-COUNTER))
           (TEMP1 (TIME:MICROSECOND-TIME))
           (TIME-DIFF 0)
           (STAT-DIFF 0)
           (DISK-DIFF 0))
       ,@BODY
       (SETQ STAT-DIFF (- (READ-STATISTICS-COUNTER) TEMP2))
       (SETQ TIME-DIFF (- (TIME:MICROSECOND-TIME) TEMP1))
       (SETQ DISK-DIFF (READ-METER 'SYS:%DISK-WAIT-TIME))
       (FORMAT T "~%Map faults, first ~D, second ~D"
               (READ-METER 'SYS:%COUNT-FIRST-LEVEL-MAP-RELOADS)
               (READ-METER 'SYS:%COUNT-SECOND-LEVEL-MAP-RELOADS))
       (FORMAT T "~% elapsed time: ~D-~D microseconds~%ticks: ~D~%guesstimate (4MuIPS): ~F%~%"
               TIME-DIFF
               DISK-DIFF
               STAT-DIFF
               (* 100 (// (FLOAT STAT-DIFF)
                          (* 4 (- (FLOAT TIME-DIFF) (FLOAT DISK-DIFF)))))))))


(DEFUN WORST-CASE-TEST (LIM)
  (DECLARE (SPECIAL WC-SECOND-LEVEL))
  (UNLESS (BOUNDP 'WC-SECOND-LEVEL)
    (SETQ WC-SECOND-LEVEL (MAKE-ARRAY 32.))
    (DOTIMES (J 32.)
      (SETF (AREF WC-SECOND-LEVEL J) (MAKE-ARRAY 8192. ':INITIAL-ELEMENT 0))))
  (USTAT-MACRO
    (SETQ LIM (TRUNCATE LIM 32.))
    (DOTIMES (I (1+ LIM))
      (DOTIMES (J 32.)
        ;; cause a second level map fault
        (AREF (AREF WC-SECOND-LEVEL J) 0)))))

(DEFUN UINST-TESTS NIL
  (USTAT-MACRO (PROCESS-SLEEP 60.))
  (USTAT-MACRO (WHO-USES 'FOOBARBLETCH "SI"))
  (USTAT-MACRO (APROPOS "foobarbletch" ':PACKAGE "SI"))
  (WORST-CASE-TEST 100000.))

(DEFUN UINST-OTHER-TEST NIL
  (USTAT-MACRO (COMPILE 'ADD-ASSEMBLY)))
