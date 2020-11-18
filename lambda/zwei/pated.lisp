;;; -*- Mode:LISP; Package:ZWEI; Base:10; Readtable:CL -*-
;;; More winning Lisp Machine software patch facility.   DLW & BEE 10/24/80
;;; The functions in this file manage the patch files
;;; Depends on defs in SYS: SYS2; PATCH

(DEFVAR *PATCH-BUFFER* NIL
  "The buffer holding the patch file being accumulated.")

(DEFVAR *PATCH-SYSTEM* NIL
  "The system we are trying to patch.
An object of type SI::PATCH-SYSTEM, or NIL if we are making a private patch.")

(DEFVAR *PATCH-NUMBER* NIL
  "Minor version number of patch being made.")

(DEFUN PATCH-VERSION-DESCRIPTION ()
  (IF *PATCH-SYSTEM*
      (FORMAT NIL "~D.~D of ~A"
              (SI::PATCH-VERSION *PATCH-SYSTEM*) *PATCH-NUMBER*
              (SI::PATCH-NAME *PATCH-SYSTEM*))
    (BUFFER-NAME *PATCH-BUFFER*)))

(DEFCOM COM-ADD-PATCH "Add the current defun or the region (if any) to the patch buffer.
If there is no patch buffer, ask the user for the system to patch. Then reserve a new
version number, and create a buffer whose pathname is the source file name for the
patch creating that version number.  If there is a region, append it to the end of the
patch buffer; otherwise append the current defun to the end of the patch buffer." ()
  (LET (BP1 BP2 DEFUN-NAME)
    (COND ((WINDOW-MARK-P *WINDOW*)
           ;; there is a region, use it.
           (SETQ BP1 (MARK) BP2 (POINT))
           (OR (BP-< BP1 BP2) (PSETQ BP1 BP2 BP2 BP1))
           (CHECK-INTERVAL-SECTIONS BP1 BP2 T)
           (SETQ DEFUN-NAME "the region"))
          ((SETQ BP1 (DEFUN-INTERVAL (BEG-LINE (POINT)) 1 NIL NIL))
           ;; No region, try to get containing defun.
           (SETQ BP2 (INTERVAL-LAST-BP BP1) BP1 (INTERVAL-FIRST-BP BP1))
           (CHECK-INTERVAL-SECTIONS BP1 BP2 T)
           (SETQ DEFUN-NAME (SECTION-NODE-NAME (LINE-NODE (BP-LINE BP1)))))
          (T
           (BARF "Unbalanced parentheses or no defuns.")))
    (ADD-PATCH-INTERVAL BP1 BP2 T DEFUN-NAME *INTERVAL*))
  DIS-MARK-GOES)

(DEFUN VALIDATE-PATCH-BUFFER ()
  "Set *PATCH-BUFFER* to NIL if it is a killed buffer."
  (UNLESS (MEMQ *PATCH-BUFFER* *ZMACS-BUFFER-LIST*)
    (SETQ *PATCH-BUFFER* NIL
          *PATCH-SYSTEM* NIL
          *PATCH-NUMBER* NIL)))


(DEFUN ADD-PATCH-INTERVAL (BP1 BP2 IN-ORDER-P DEFUN-NAME BUFFER)
  (GET-INTERVAL BP1 BP2 IN-ORDER-P)
  (VALIDATE-PATCH-BUFFER)
  (LET ((NEW-PATCH-BUFFER-P (NULL *PATCH-BUFFER*)))
    (IF NEW-PATCH-BUFFER-P (CREATE-NEW-PATCH (READ-PATCH-SYSTEM-NAME)))
    (FORMAT *QUERY-IO*
            "~&Adding ~:[~S~;~A~] to patch file ~A~@[~%(New patch file)~]"
            (STRINGP DEFUN-NAME) DEFUN-NAME (PATCH-VERSION-DESCRIPTION) NEW-PATCH-BUFFER-P))
  ;; Warn the luser if the readtable in BUFFER differs from the one in the patch buffer
  (let* ((readtable-name (or (get buffer ':readtable) (get buffer ':syntax)))
         (readtable-binder "")
         (reason (cond ((null readtable-name) " has no readtable attribute")
                       ((neq (si:find-readtable-named readtable-name)
                             (si:find-readtable-named (get *patch-buffer* :readtable)))
                        (setq readtable-binder (format nil "#!~S" readtable-name))
                        ())))
         (bp (interval-last-bp *patch-buffer*)))
    (when reason
      (setq reason (string-append "You may lose because the buffer" reason #\.))
      (beep nil *query-io*)
      (format *query-io* "~&Warning: ~A" reason)
      (insert bp (format nil ";; *** Note: ***~%;;   ~A~%;; *************~%" reason)))
    ;; Put into the patch buffer, making sure the right package and base will be used.
    (MULTIPLE-VALUE-BIND (VARS VALS)
        (SEND BUFFER :ATTRIBUTE-BINDINGS)
      (PROGV VARS VALS
        (LET ((TIME:*DEFAULT-DATE-PRINT-MODE* :DD-MMM-YY))
          (INSERT BP (FORMAT NIL "~%; From ~@[~*modified ~]~:[buffer ~A~;file ~:*~A~*~] at ~\\DATIME\\~%"
                             (BUFFER-MODIFIED-P BUFFER)
                             (CAR-SAFE (SEND BUFFER :FILE-ID)) BUFFER (GET-UNIVERSAL-TIME))))
        (INSERT BP (FORMAT NIL "#~DR ~A#: ~A
\(~S ((~S (~S ~S)))
\  (~S ~S~2%"
                           *READ-BASE*
                           ;;>> Note that this loses since we are not quoting the package-name
                           ;;>>  if necessary!
                           (PACKAGE-NAME *PACKAGE*)
                           READTABLE-BINDER
                           'COMPILER-LET '*PACKAGE* 'PKG-FIND-PACKAGE (PACKAGE-NAME *PACKAGE*)
                           'COMPILER::PATCH-SOURCE-FILE
                           (WHEN (BUFFER-GENERIC-PATHNAME BUFFER)
                             (SEND (BUFFER-GENERIC-PATHNAME BUFFER) :STRING-FOR-PRINTING))))))
    (INSERT-INTERVAL BP BP1 BP2 T)
    (INSERT BP #\NEWLINE)
    (INSERT BP "))")
    (INSERT BP #\NEWLINE))
  ;; Mark all sections that the region contains part of
  ;; as having been patched.
  (INTERVAL-LINES (BP1 BP2) (START-LINE END-LINE)
    (DO ((LINE START-LINE (LINE-NEXT LINE))
         (LAST-SECTION))
        ((EQ LINE END-LINE))
      (LET ((SECTION (LINE-NODE LINE)))
        (UNLESS (EQ LAST-SECTION SECTION)
          (SETF (GET SECTION 'PATCH-TICK) *TICK*))
        (SETQ LAST-SECTION SECTION)))))

(DEFCOM COM-START-PATCH "Begin a patch but do not put any code into it yet.
Like the Add Patch command but leaves the patch buffer empty except for its
initial comments." ()
  (VALIDATE-PATCH-BUFFER)
  (WHEN *PATCH-BUFFER*
    (UNLESS (FQUERY () "You are already editing patch ~A.
Do you want to switch to editing some other patch? "
                    (PATCH-VERSION-DESCRIPTION))
      (BARF "You cannot edit two patches at once."))
    (FORMAT *QUERY-IO* "~&Use M-X Resume Patch to go back to editing the previous patch.")
    (SETQ *PATCH-BUFFER* NIL))
  (CREATE-NEW-PATCH (READ-PATCH-SYSTEM-NAME))
  (FORMAT *QUERY-IO* "~&Beginning patch ~A." (PATCH-VERSION-DESCRIPTION))
  DIS-NONE)

(DEFCOM COM-START-PRIVATE-PATCH "Begin a private patch, assign no version number.
Like Start Patch but you specify a filename for the patch file.
LOAD-PATCHES does not know about private patches; you must
load them explicitly if you want them." ()
  (VALIDATE-PATCH-BUFFER)
  (WHEN *PATCH-BUFFER*
    (UNLESS (FQUERY () "You are already editing patch ~A.
Do you want to switch to editing some other patch? "
                    (PATCH-VERSION-DESCRIPTION))
      (BARF "You can only edit one patch at a time."))
    (FORMAT *QUERY-IO* "~&Use M-X Resume Patch to go back to editing the previous patch.")
    (SETQ *PATCH-BUFFER* NIL))
  (CREATE-PRIVATE-PATCH-BUFFER (READ-DEFAULTED-AUX-PATHNAME "Patch file name: " :LISP))
  (FORMAT *QUERY-IO* "~&Beginning patch ~A."
          (PATCH-VERSION-DESCRIPTION))
  DIS-NONE)

(DEFCOM COM-CANCEL-PATCH "Cancel the patch being edited.
If this patch is still the most recent one for its system,
the patch's version number will be reused for the next patch
made in the same system." ()
  (VALIDATE-PATCH-BUFFER)
  (cond ((NULL *PATCH-BUFFER*)
         (BARF "You are not now editing a patch file."))
        (t
         (WHEN *PATCH-SYSTEM*
           (SI::ABORT-PATCH *PATCH-SYSTEM* *PATCH-NUMBER*))
         (FORMAT *QUERY-IO* "~&Patch ~A aborted."
                 (PATCH-VERSION-DESCRIPTION))
         (SETQ *PATCH-BUFFER* NIL *PATCH-NUMBER* NIL *PATCH-SYSTEM* NIL)))
  DIS-NONE)

(DEFCOM COM-RESUME-PATCH "Resume editing a patch previously started but never finished.
If the patch file source was saved during the previous session,
that source will be read back in to initialize the editing in this session.
You can then proceed to Abort Patch or to add to the patch and finish it." ()
  (VALIDATE-PATCH-BUFFER)
  (WHEN *PATCH-BUFFER*
    (UNLESS (FQUERY () "You are already editing patch ~A.
Do you want to switch to editing some other patch? "
                    (PATCH-VERSION-DESCRIPTION))
      (BARF "You can only edit one patch at a time."))
    (SETQ *PATCH-BUFFER* NIL))
  (LET* ((PATCH-SYSTEM
           (READ-PATCH-SYSTEM-NAME))
         (PATCH-NUMBER
           (LET ((*READ-BASE* 10.) (*PRINT-BASE* 10.))
             (TYPEIN-LINE-READ "Minor version of ~A to resume constructing: "
                               PATCH-SYSTEM))))
    (CREATE-OLD-PATCH-BUFFER (SI::GET-PATCH-SYSTEM-NAMED PATCH-SYSTEM) PATCH-NUMBER))
  DIS-NONE)

;;; Try to find a patchable system that this file lives in.  If it isn't
;;; patchable, try to find a patchable supersystem.  If there is none, use the
;;; system named SYSTEM.
(DEFUN PATCHABLE-SYSTEM-OF-PATHNAME (PATHNAME)
  ;;if there wasn't a pathname, return the SYSTEM system
  (IF (NULL PATHNAME)
      (SI:FIND-SYSTEM-NAMED "System")
    (LET* ((GENERIC-PATHNAME (SEND PATHNAME :GENERIC-PATHNAME))
           (SYSTEMS (SEND GENERIC-PATHNAME :GET ':SYSTEMS)))
      ;;if it wasn't defined as part of a system, use the SYSTEM system
      (IF (NULL SYSTEMS)
          (SI:FIND-SYSTEM-NAMED "System")
        (SI::SYSTEM-PATCHABLE-SUPERSYSTEM (CAR SYSTEMS))))))

(DEFUN READ-PATCH-SYSTEM-NAME ()
  "Read and return the name of a system to patch, using the minibuffer.
We provide completion and a default based on the selected buffer."
  (LET* ((DEFAULT (PATCHABLE-SYSTEM-OF-PATHNAME (SEND *INTERVAL* :PATHNAME)))
         (PATCH-SYSTEM (COMPLETING-READ-FROM-MINI-BUFFER  ;a SYSTEM defstruct.
                         (IF DEFAULT
                             (FORMAT NIL "System to patch (default ~A)"
                                     (SI::SYSTEM-NAME DEFAULT))
                           "System to patch")
                         (MAPCAR (LAMBDA (X) (CONS (SI::PATCH-NAME X) X))
                                 SI::PATCH-SYSTEMS-LIST))))
    (IF (STRINGP PATCH-SYSTEM)
        (IF DEFAULT (SETQ PATCH-SYSTEM (SI::SYSTEM-NAME DEFAULT)) (BARF))
      ;; Some day maybe allow creating of a patch here
      (SETQ PATCH-SYSTEM (CAR PATCH-SYSTEM)))     ;If completed on alist, flush alist dotted pair
    PATCH-SYSTEM))

(DEFUN CREATE-NEW-PATCH (PATCH-SYSTEM)
  (LET ((PATCH-STRUCTURE (SI::GET-PATCH-SYSTEM-NAMED PATCH-SYSTEM)))
    (CREATE-PATCH-BUFFER PATCH-STRUCTURE (SI::RESERVE-PATCH PATCH-STRUCTURE *QUERY-IO*))))

(DEFUN CREATE-PATCH-BUFFER (PATCH-STRUCTURE NUMBER)
  (SETQ *PATCH-NUMBER* NUMBER)
  (LET* ((FILENAME (SI::PATCH-SYSTEM-PATHNAME (SI:PATCH-NAME PATCH-STRUCTURE) :PATCH-FILE
                                              (SI:PATCH-VERSION PATCH-STRUCTURE)
                                              *PATCH-NUMBER*
                                              :LISP))
         (*FIND-FILE-NOT-FOUND-IS-AN-ERROR* NIL))
    (SETQ *PATCH-BUFFER*
          (FIND-FILE FILENAME NIL)))    ; The NIL means "don't select this buffer".
  (LET ((EMPTY-P (BP-= (INTERVAL-FIRST-BP *PATCH-BUFFER*)
                       (INTERVAL-LAST-BP *PATCH-BUFFER*))))
    (INITIALIZE-PATCH-BUFFER *PATCH-BUFFER* PATCH-STRUCTURE)
    (SETQ *PATCH-SYSTEM* PATCH-STRUCTURE)
    (UNLESS EMPTY-P
      (MAKE-BUFFER-CURRENT *PATCH-BUFFER*)
      (BARF "~A was supposed to be a new file.  It isn't.  Look at it!"
            (BUFFER-PATHNAME *PATCH-BUFFER*)))))

(DEFUN CREATE-PRIVATE-PATCH-BUFFER (FILENAME)
  (LET* ((*FIND-FILE-NOT-FOUND-IS-AN-ERROR* NIL))
    (SETQ *PATCH-BUFFER* (FIND-FILE FILENAME NIL)))     ;NIL means "don't select this buffer".
  (SETQ *PATCH-SYSTEM* NIL *PATCH-NUMBER* NIL)
  (IF (BP-= (INTERVAL-FIRST-BP *PATCH-BUFFER*)
            (INTERVAL-LAST-BP *PATCH-BUFFER*))
      (INITIALIZE-PATCH-BUFFER *PATCH-BUFFER* NIL)))

;;; Create a patch buffer for a patch that has already been reserved.
;;; Read in the existing source file for the patch, if any.
(DEFUN CREATE-OLD-PATCH-BUFFER (PATCH-STRUCTURE NUMBER)
  (SETQ *PATCH-NUMBER* NUMBER)
  (LET* ((FILENAME (SI::PATCH-SYSTEM-PATHNAME (SI:PATCH-NAME PATCH-STRUCTURE) :PATCH-FILE
                                              (SI:PATCH-VERSION PATCH-STRUCTURE)
                                              *PATCH-NUMBER*
                                              :LISP)))
    (SETQ *PATCH-BUFFER*
          (FIND-FILE FILENAME NIL)))            ; The NIL means "don't select this buffer".
  (INITIALIZE-PATCH-BUFFER *PATCH-BUFFER* PATCH-STRUCTURE)
  (SETQ *PATCH-SYSTEM* PATCH-STRUCTURE))

(DEFUN INITIALIZE-PATCH-BUFFER (BUFFER PATCH-STRUCTURE)
  ;; If buffer is empty, initialize it.
  (IF (BP-= (INTERVAL-FIRST-BP BUFFER) (INTERVAL-LAST-BP BUFFER))
      (LET ((STREAM (INTERVAL-STREAM BUFFER)))
        (FORMAT STREAM
;>> this readtable:zl is somewhat bogus -- perhaps we should have a readtable default as
;>> part of the defsystem?
;>>
;>> In any case, we lose trying to do a m-x add patch from a buffer with one readtable
;>> into a patch file with a another.
;>> m-x add patch should check for a variety of incompatibilty conditions such as
;>>  that between the patch buffer and the buffer from which the patch is being taken.
                ";;; -*- Mode:Lisp; Readtable:~A; Package:~A; Base:~D; Patch-File:T -*-~%"
                (send *interval* :get-attribute :readtable (si::rdtbl-short-name *readtable*))
                (OR (AND PATCH-STRUCTURE
                         (SI::SYSTEM-PACKAGE-DEFAULT
                           (SI:FIND-SYSTEM-NAMED
                             (SI::PATCH-NAME PATCH-STRUCTURE))))
                    "USER")
                (SEND *INTERVAL* :GET-ATTRIBUTE :BASE *READ-BASE*))
        (IF (NOT PATCH-STRUCTURE)
            (FORMAT STREAM ";;; Private patches made by ~A" USER-ID)
          (FORMAT STREAM ";;; Patch file for ~A version ~D.~D"
                  (SI::PATCH-NAME PATCH-STRUCTURE) (SI::PATCH-VERSION PATCH-STRUCTURE)
                  *PATCH-NUMBER*))
        (SI::WRITE-RESPONSIBILITY-COMMENT STREAM)
        (TERPRI STREAM)))
  (REPARSE-BUFFER-attribute-list BUFFER))

(DEFCOM COM-FINISH-PATCH "Finish off the current patch file.
Writes out the patch buffer, compiles it, and marks it as finished and released
so that LOAD-PATCHES will load it.  See also Finish Patch Unreleased." ()
  (IF *NUMERIC-ARG-P*
      (FINISH-PATCH T)
    (FINISH-PATCH))
  DIS-NONE)

(DEFCOM COM-FINISH-PATCH-UNRELEASED "Finish off the current patch file, but don't release it.
Writes out the patch buffer, compiles it, and marks it as finished.
Does not release the patch, so that LOAD-PATCHES will normally NOT load it.
To load it, answer YES to the special query in LOAD-PATCHES;
after testing, use the M-X Release Patch command
to mark it released so that LOAD-PATCHES will normally load it." ()
  (FINISH-PATCH NIL)
  DIS-NONE)

(DEFUN FINISH-PATCH (&OPTIONAL (RELEASE-FLAG NIL SPECIFIEDP))
  (VALIDATE-PATCH-BUFFER)
  (COND ((NULL *PATCH-BUFFER*)
         (BARF "There is no current patch buffer"))
        ((NOT SPECIFIEDP)
         (SETQ RELEASE-FLAG
               (FQUERY ()
                       "Release this patch? (answer N if you are not completely sure that it works) "))))
  (LET ((DESCRIPTION (TYPEIN-LINE-MULTI-LINE-READLINE
                       "Description of changes (end with ~C)" #\END)))
    (SETQ DESCRIPTION (STRING-TRIM '(#\NEWLINE) DESCRIPTION))
    (LET ((BP (FORWARD-LINE (INTERVAL-FIRST-BP *PATCH-BUFFER*) 2)))
      (INSERT-MOVING BP ";;; Reason:")
      (INSERT-MOVING BP #\NEWLINE)
      (DO ((START 0 (1+ NEXT-LINE))
           NEXT-LINE)
          (())
        (SETQ NEXT-LINE (STRING-SEARCH-CHAR #\NEWLINE DESCRIPTION START))
        (INSERT-MOVING BP ";;;  ")
        (INSERT-MOVING BP DESCRIPTION START NEXT-LINE)
        (INSERT-MOVING BP #\NEWLINE)
        (OR NEXT-LINE (RETURN))))
    (SAVE-BUFFER-IF-NECESSARY *PATCH-BUFFER*)
    (WHEN (EQ *PATCH-BUFFER* *INTERVAL*)
      (MUST-REDISPLAY *WINDOW* DIS-TEXT))
    (LET ((ERROR-MESSAGE (IF *PATCH-SYSTEM*
                             (SI::CONSUMMATE-PATCH *PATCH-SYSTEM* *PATCH-NUMBER*
                                                   DESCRIPTION RELEASE-FLAG)
                           (progn
                             (si::compile-patch-file *patch-system* (buffer-pathname *patch-buffer*))
                             NIL))))
      (cond ((ERRORP ERROR-MESSAGE)
             (BARF "~A" ERROR-MESSAGE))
            (t
             (FORMAT *QUERY-IO* "~&~:[Patch completed.~;Don't forget to save your files!~]"
                     (LOOP FOR BUFFER IN *ZMACS-BUFFER-LIST*
                           THEREIS (BUFFER-NEEDS-SAVING-P BUFFER)))
             (SETQ *PATCH-BUFFER* NIL
                   *PATCH-SYSTEM* NIL))))))

(DEFCOM COM-RELEASE-PATCH "Mark an already finished patch as \"released\".
If you finish a patch with Finish Unreleased Patch, it will not
be loaded by LOAD-PATCHES unless you say you want to test it.
After testing it that way, use this command to mark the patch
as released so users will load it." ()
  (LET* ((PATCH-SYSTEM
           (READ-PATCH-SYSTEM-NAME))
         (PATCH-NUMBER
           (LET ((*READ-BASE* 10.) (*PRINT-BASE* 10.))
             (TYPEIN-LINE-READ "Minor version of ~A to release: "
                               PATCH-SYSTEM))))
    (SI::CONSUMMATE-PATCH (SI::GET-PATCH-SYSTEM-NAMED PATCH-SYSTEM) PATCH-NUMBER NIL T T))
  DIS-NONE)

(DEFCOM COM-ADD-PATCH-CHANGED-SECTIONS "Offer to Add Patch for each changed section.
Does not ask about sections that haven't been changed
or that have been patched since they were last changed.
Type P to patch all the rest of one buffer's changed sections
with no more questions.  Questions will resume in the next buffer." ()
  (DOLIST (BUFFER *ZMACS-BUFFER-LIST*)
    (AND (EQ (SEND BUFFER :MAJOR-MODE) 'LISP-MODE)
         (NOT (SEND BUFFER :GET-ATTRIBUTE :PATCH-FILE))
         (IF (EQ (ADD-PATCH-BUFFER-CHANGED-FUNCTIONS BUFFER) ':QUIT) (RETURN))))
  DIS-NONE)

(DEFCOM COM-ADD-PATCH-BUFFER-CHANGED-SECTIONS
  "Offer to Add Patch this buffer's changed sections.
Does not ask about sections that haven't been changed
or that have been patched since they were last changed.
Type P to patch all the rest of the changed sections with no more questions." ()
  (ADD-PATCH-BUFFER-CHANGED-FUNCTIONS *INTERVAL*)
  DIS-NONE)

(DEFUN ADD-PATCH-BUFFER-CHANGED-FUNCTIONS (BUFFER)
  (VALIDATE-PATCH-BUFFER)
  (IF (NULL *PATCH-BUFFER*) (CREATE-NEW-PATCH (READ-PATCH-SYSTEM-NAME)))
  (LET (PROCEED-FLAG)
    (RESECTIONIZE-BUFFER BUFFER)
    (MAKE-BUFFER-CURRENT BUFFER)
    (DOLIST (SECTION (NODE-INFERIORS BUFFER))
      (AND (TYPEP SECTION 'SECTION-NODE)
           (SECTION-NODE-DEFUN-LINE SECTION)
           (LET ((PATCH-TICK (GET SECTION 'PATCH-TICK)))
             (> (NODE-TICK SECTION) (OR PATCH-TICK (BUFFER-FILE-READ-TICK BUFFER))))
           (LET ((NAME (SECTION-NODE-NAME SECTION)))
             (DISPLAY-SECTION SECTION NIL T)
             (WHEN (OR PROCEED-FLAG
                       (CASE (FQUERY '(:CHOICES
                                        (((:PROCEED "Proceed.") #\P)
                                         ((:QUIT "Quit") #\Q #\c-G #\Abort)
                                         . #,FORMAT:Y-OR-N-P-CHOICES))
                                     "Add ~S to patch? " NAME)
                         (:PROCEED (SETQ PROCEED-FLAG T))
                         (:QUIT (RETURN-FROM ADD-PATCH-BUFFER-CHANGED-FUNCTIONS ':QUIT))
                         ((T) T)))
               (ADD-PATCH-INTERVAL SECTION NIL T NAME BUFFER))))))
  NIL)
