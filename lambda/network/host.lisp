;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:8; Readtable:ZL -*-
;;;  ** (c) Copyright 1981, 1984 Massachusetts Institute of Technology **
;;;
;;; This is SYS: NETWORK; HOST

;;; Host objects:

;;; Host objects have two major uses.  One, they identify a "file
;;; computer" to use inside a pathname.  Two, they serve to canonicalize
;;; name and address mapping on networks.  A comment at the start of
;;; SYS: IO; FILE; PATHNM describes in more detail the pathname parsing
;;; interaction with host objects.

;;; The flavor SI:BASIC-HOST is included in host objects of all types.
;;; The flavor SI:HOST is for hosts which actually correspond to a
;;; particular machine on some network (as opposed to a logical host in
;;; the pathname sense).

;;; Interesting messages on all hosts
;;; :NAME - returns the whole name of the host
;;; :NAME-AS-FILE-COMPUTER - returns what should be printed before the : in pathnames
;;; :PATHNAME-HOST-NAMEP name - is name the name of this host before the : in a pathname?

;;; Interesting messages on network hosts
;;; :SHORT-NAME - returns the shortest nickname from the host table
;;; :SYSTEM-TYPE - returns the operating system type, a symbol in the keyword package, e.g.
;;;                :ITS, :LISPM
;;; :NETWORK-TYPEP network - is this host connected to network, a keyword, e.g. :CHAOS?
;;; :NETWORK-TYPE - returns the major network this host is connected to (i.e. prefers
;;;                :CHAOS)

;;; Relevant functions
;;; SI:PARSE-HOST thing - takes a string and returns a host object corresponding to the
;;;                       network host with that name.  NOT the function used with pathnames.
;;; SI:DEFINE-HOST name &rest options - the form written into host table files.  Defines
;;;                                     a new network host.  Special options include:
;;;     :SYSTEM-TYPE keyword
;;;     :MACHINE-TYPE keyword
;;;     :HOST-NAMES list-of-nicknames
;;;                                     All other options are taken as a network type and
;;;                                     the value as a list of addresses.
;;;                                     The function CHAOS:GENERATE-HOST-TABLE-1 knows how
;;;                                     to make a set of these forms from the HOSTS2 CHAOS
;;;                                     host table.
;;;   Other keywords are for network addresses.
;;; SI:GET-HOST-FROM-ADDRESS address network - returns a host object for address on network.
;;;     E.g. (SI:GET-HOST-FROM-ADDRESS 2026 :CHAOS) => #FS:ITS-HOST "MIT-AI"

;;; Host flavor association
;;; For a particular operating system type (computed from a host table), there should be a
;;; HOST-FLAVOR property.  :DEFAULT is the property for other hosts.
;;; Usually, the HOST-FLAVOR mixes in FS:FILE-HOST-MIXIN (see SYS: IO; FILE; ACCESS) so that
;;; file access can be done to it.

;;; Base flavor, for network and special hosts.
(DEFFLAVOR BASIC-HOST () (SI:PROPERTY-LIST-MIXIN)
  (:REQUIRED-METHODS :NAME)
  :ABSTRACT-FLAVOR
  (:METHOD-COMBINATION (:OR :BASE-FLAVOR-LAST :PATHNAME-HOST-NAMEP)
                       (:LIST :BASE-FLAVOR-FIRST :PEEK-FILE-SYSTEM-HEADER
                                                 :PEEK-FILE-SYSTEM)))

(DEFMETHOD (BASIC-HOST :INIT) (IGNORE) NIL)

;;; STRING of a host is its name
(DEFMETHOD (BASIC-HOST :STRING-FOR-PRINTING) () (SEND SELF :NAME))

(DEFMETHOD (BASIC-HOST :PRINT-SELF) (STREAM IGNORE SLASHIFY-P)
  (IF SLASHIFY-P
      (FORMAT:OUTPUT STREAM
        "#" (PRIN1 (TYPE-OF SELF))
        " " (PRIN1 (SEND SELF :NAME))
        "")
    (SEND STREAM :STRING-OUT (SEND SELF :NAME))))

;;; For sake of random types of hosts, look on pathname host list.
;;; For sake of host-table hosts, look on host table.
(DEFMETHOD (BASIC-HOST :READ-INSTANCE) (IGNORE STREAM)
  (LET ((NAM (READ STREAM)))
    (OR (FS:GET-PATHNAME-HOST NAM)
        (PARSE-HOST NAM T))))

(DEFMETHOD (BASIC-HOST :SHORT-NAME) () (SEND SELF :NAME))
(DEFMETHOD (BASIC-HOST :NAME-AS-FILE-COMPUTER) ()(SEND SELF :NAME))

(DEFMETHOD (BASIC-HOST :DEFAULT :PATHNAME-HOST-NAMEP) (NAME)
  (STRING-EQUAL NAME (SEND SELF :NAME)))

(DEFMETHOD (BASIC-HOST :SAMPLE-PATHNAME) ()
  (FS:MAKE-PATHNAME-INTERNAL SELF NIL NIL NIL NIL NIL))

(DEFMETHOD (BASIC-HOST :OPEN-STREAMS) () NIL)

(DEFMETHOD (BASIC-HOST :NETWORK-TYPE) () NIL)
(DEFMETHOD (BASIC-HOST :NETWORK-TYPEP) (IGNORE) NIL)

(DEFMETHOD (BASIC-HOST :CLOSE-ALL-FILES) (&OPTIONAL (MODE :ABORT))
  (LOOP FOR STREAM IN (SEND SELF :OPEN-STREAMS)
        DO (FORMAT *ERROR-OUTPUT* "~%Closing ~S" STREAM)
           (SEND STREAM :CLOSE MODE)
        COLLECT STREAM))

(DEFMETHOD (BASIC-HOST :DEFAULT :PEEK-FILE-SYSTEM-HEADER) ()  NIL)

(DEFMETHOD (BASIC-HOST :DEFAULT :PEEK-FILE-SYSTEM) () NIL)

(DEFVAR FS:*GENERIC-BASE-TYPE-ALIST*
        '((:TEXT . :TEXT) (:PRESS . :TEXT) ("XGP" . :TEXT) ("DOC" . :TEXT)
          (:LISP . :UNSPECIFIC) (:QFASL . :UNSPECIFIC)
          (NIL . :UNSPECIFIC))
;The association of :UNSPECIFIC with LISP is a concession to ITS..
; After all, this is the LISP machine.
  "Associate pathname types, with the corresponding types for the generic pathname.
Each element is (file-type . generic-pathname-type).
Each type may be a string (in standard case) or a canonical type keyword.
The first entry for a given generic pathname type
is the one for the /"source file/" of that type.")

(DEFMETHOD (BASIC-HOST :DEFAULT :GENERIC-BASE-TYPE) (FILE-TYPE)
  (LET ((AE (ASSOC FILE-TYPE FS:*GENERIC-BASE-TYPE-ALIST*)))
    (IF (NULL AE) FILE-TYPE (CDR AE))))

(DEFMETHOD (BASIC-HOST :DEFAULT :GENERIC-SOURCE-TYPE) (FILE-TYPE)
  (OR (CAR (RASSOC FILE-TYPE FS:*GENERIC-BASE-TYPE-ALIST*))
      FILE-TYPE))

(DEFMETHOD (BASIC-HOST :LOGICALLY-BACKTRANSLATE-HOST-DEV-DIR)
           (PHYSICAL-HOST PHYSICAL-DEVICE PHYSICAL-DIRECTORY)
  PHYSICAL-HOST PHYSICAL-DEVICE PHYSICAL-DIRECTORY
  NIL)

(DEFMETHOD (BASIC-HOST :DEFAULT :DEFAULT-DEVICE) () :UNSPECIFIC)
(DEFMETHOD (BASIC-HOST :DEFAULT :PRIMARY-DEVICE) () :UNSPECIFIC)
(DEFMETHOD (BASIC-HOST :ENABLE-CAPABILITIES) (&REST IGNORE) NIL)
(DEFMETHOD (BASIC-HOST :DISABLE-CAPABILITIES) (&REST IGNORE) NIL)

(COMPILE-FLAVOR-METHODS BASIC-HOST)

;;; General (multi-network) host table.
(DEFVAR HOST-ALIST NIL "Alist of host names and host data.
Each element is a HOST-ALIST-ELEM structure, and looks like
/(primary-name host-instance-or-nil list-of-all-names
  system-type machine-type site-name . addresses/)")

(DEFSTRUCT (HOST-ALIST-ELEM :LIST* (:CONC-NAME HOST-) (:ALTERANT NIL))
  "Items of this format appear on SI:HOST-ALIST"
  (NAME NIL :DOCUMENTATION "Primary, official name of this host. A string")
  (INSTANCE NIL :DOCUMENTATION "The actual object representing this host. An instance.")
  (NAME-LIST NIL :DOCUMENTATION
    "List of all nicknames for this host. Must include primary name in this list also.")
  (SYSTEM-TYPE-INTERNAL NIL :DOCUMENTATION
    "Keyword describing machine type, such as :PDP10")
  (MACHINE-TYPE-INTERNAL NIL :DOCUMENTATION
    "Keyword describing the operating system the machine is running, such as :ITS")
  SITE-NAME
  ADDRESSES)

(DEFUN DEFINE-HOST (NAME &REST OPTIONS)
  "Define a host.  Should be used only in SYS:SITE;HSTTBL LISP."
  (let (name-list system-type-internal machine-type-internal addresses elem old-p)
    (loop for (option value) on options by 'cddr
          do (case option
               (:host-names (setq name-list value))
               (:system-type (setq system-type-internal value))
               (:machine-type (setq machine-type-internal value))
               (otherwise (setq addresses (append (list option value) addresses)))))
    (setq elem (or (block found
                     (dolist (host host-alist)
                       (dolist (name name-list)
                         (when (member-equalp name (host-name-list host))
                           (setq old-p t)
                           (return-from found host)))))
                   (make-host-alist-elem)))
    (setf (host-name elem) name)
    (setf (host-name-list elem) name-list)
    (setf (host-system-type-internal elem) system-type-internal)
    (setf (host-machine-type-internal elem) machine-type-internal)
    (setf (host-addresses elem) addresses)
    (cond ((consp (host-site-name elem))        ;Site is a list -- add this site to list
           (pushnew site-name (host-site-name elem)))
          ((null (host-site-name elem))         ;Site is unspecified -- set it to this site
           (setf (host-site-name elem) site-name))
          ((neq (host-site-name elem) site-name);Site is specified but different -- create list
           (setf (host-site-name elem) (list site-name (host-site-name elem)))))
    (IF (NOT OLD-P)
        (PUSH ELEM HOST-ALIST)
      ;; It changed flavors, due to network or OS change.  Just ignore it for now.
      (LET ((OLD-INSTANCE (HOST-INSTANCE ELEM)))
        (WHEN OLD-INSTANCE
          (LET ((FLAVOR (COMPUTE-HOST-FLAVOR ELEM)))
            (WHEN (NEQ FLAVOR (TYPE-OF OLD-INSTANCE))
              (FORMAT *ERROR-OUTPUT* "~&[Sorry, can't change ~A's flavor.]~%" (HOST-NAME ELEM))
              ;;Invalidate host. (Thats nasty. -gjc)
              (SETF (HOST-ADDRESSES ELEM) NIL)))
          ;; needed because this may depend on :special-file-hosts which may change.
          (SEND OLD-INSTANCE :SEND-IF-HANDLES :RESET-SAMPLE-PATHNAME))))))


(DEFPROP DEFINE-HOST T QFASL-DONT-RECORD)


(DEFUN MAKE-DUMMY-HOST (TYPE)
  "A dummy host is useful for parsing foreign pathnames from magtapes, etc"
  (LET* ((ALE (MAKE-HOST-ALIST-ELEM NAME (FORMAT NIL "DUMMY_~A_HOST" TYPE)
                                      SYSTEM-TYPE-INTERNAL TYPE))
         (I (MAKE-INSTANCE (GET TYPE 'HOST-FLAVOR) :ALIST-ELEM ALE)))
    (SETF (HOST-INSTANCE ALE) I)
    (SETF (HOST-NAME-LIST ALE) (LIST (HOST-NAME ALE)))
    I))

;;; After new host table has been loaded, take any old hosts and forget about their
;;; addresses.  They stay around that way, if pointed to by pathnames, but aren't
;;; usable.  Also flush them from FS:*PATHNAME-HOST-LIST*, and put new ones on.
(DEFUN RESET-NON-SITE-HOSTS (&AUX (OLD-HOST-ALIST HOST-ALIST))
  (declare (special FS:LOCAL-HOST-ADDED))
  (MAYBE-MINI-LOAD-FILE-ALIST HOST-TABLE-FILE-ALIST)
  (LOOP FOR ELEM IN OLD-HOST-ALIST
        WHEN (NEQ (HOST-SITE-NAME ELEM) SITE-NAME)
        DO (COND ((ATOM (HOST-SITE-NAME ELEM))
                  (SETF (HOST-ADDRESSES ELEM) NIL)
                  (SETQ HOST-ALIST (DELQ ELEM HOST-ALIST)))
                 (T
                  (SETF (HOST-SITE-NAME ELEM) (CAR (HOST-SITE-NAME ELEM))))))
  (IF (VARIABLE-BOUNDP ZWEI:*PATHNAME-DEFAULTS*)
      (SETQ ZWEI:*PATHNAME-DEFAULTS* (LIST (ASSQ NIL ZWEI:*PATHNAME-DEFAULTS*))))
  (SETQ FS:*DEFAULT-PATHNAME-DEFAULTS* (LIST (ASSQ NIL FS:*DEFAULT-PATHNAME-DEFAULTS*)))
  (AND LOCAL-HOST
       (SETQ FS:*PATHNAME-HOST-LIST* (DELQ LOCAL-HOST FS:*PATHNAME-HOST-LIST*)
             FS:LOCAL-HOST-ADDED NIL
             SI:LOCAL-HOST (CATCH-ERROR
                             (GET-HOST-FROM-ADDRESS CHAOS:MY-ADDRESS :CHAOS)
                             NIL)))
  ;; Flush all the old *chaos-file-hosts* and *pathnamehost-list* elements
  ;; and make new ones from the new host table.
  (AND (FBOUNDP 'FS:SITE-PATHNAME-INITIALIZE)
       (FS:SITE-PATHNAME-INITIALIZE))
  (AND (FBOUNDP 'FS:DEFINE-LOCAL-FILE-SYSTEM-HOST)
       (FS:DEFINE-LOCAL-FILE-SYSTEM-HOST)))             ;For local file, if loaded.
; SYS.TRANSLATIONS is now wired into HOST-TABLE-FILE-ALIST
; (AND (FBOUNDP 'FS:DEFINE-SYS-LOGICAL-DEVICE)
;      (FS:DEFINE-SYS-LOGICAL-DEVICE)))

(ADD-INITIALIZATION "Reset non-site host addresses" '(RESET-NON-SITE-HOSTS) '(SITE NORMAL))

;;; These are hosts of the host table sort
(DEFFLAVOR HOST
        (ALIST-ELEM (protocols nil))
        (BASIC-HOST)
  (:INITABLE-INSTANCE-VARIABLES))

(DEFMETHOD (HOST :NAME) () (HOST-NAME ALIST-ELEM))
(DEFMETHOD (HOST :HOST-NAMES) () (HOST-NAME-LIST ALIST-ELEM))
(DEFMETHOD (HOST :SHORT-NAME) ()
  (OR (FIRST (HOST-NAME-LIST ALIST-ELEM)) (HOST-NAME ALIST-ELEM)))

(DEFMETHOD (HOST :NAME-AS-FILE-COMPUTER) () (SEND SELF :SHORT-NAME))
(DEFMETHOD (HOST :PATHNAME-HOST-NAMEP) (NAME)
  (OR (AND (EQ SELF SI:LOCAL-HOST)
           (OR (STRING-EQUAL NAME "LOCAL") (STRING-EQUAL NAME "LM"))
           self)
      (CAR (MEM #'STRING-EQUAL NAME (HOST-NAME-LIST ALIST-ELEM)))))

(defmethod (host :file-host-p) () nil)

(DEFMETHOD (HOST :SYSTEM-TYPE) () (HOST-SYSTEM-TYPE-INTERNAL ALIST-ELEM))
(DEFMETHOD (HOST :MACHINE-TYPE) () (HOST-MACHINE-TYPE-INTERNAL ALIST-ELEM))

(DEFMETHOD (HOST :NETWORK-TYPE) () (CAR (HOST-ADDRESSES ALIST-ELEM)))
(DEFMETHOD (HOST :NETWORK-TYPEP) (TYPE) (GET (LOCF (HOST-ADDRESSES ALIST-ELEM)) TYPE))
(DEFMETHOD (HOST :NETWORK-ADDRESSES) () (HOST-ADDRESSES ALIST-ELEM))

(defmethod (host :network-address) (network &optional smart-p)
  (let ((addresses (getf (host-addresses alist-elem) network)))
    (when addresses
      (if (or (null (second addresses)) (not smart-p)) (first addresses)
        (let ((smart-function (get network 'smart-address-chooser)))
          (if smart-function (funcall smart-function addresses)
            (first addresses)))))))

(defmethod (host :add-network-address) (network address)
  (pushnew address (getf (host-addresses alist-elem) network)))

(defun (:property :chaos smart-address-chooser) (addresses)
  (or (first (mem #'(lambda (S a) (= (ldb (BYTE 8. 8.) a) S))
                  CHAOS:MY-SUBNET
                  addresses))
      (first addresses)))

;;; These were written by looking at SYS: NETWORK;CHAOS; CHSAUX
(DEFUN (:PROPERTY :ARPA ADDRESS-UNPARSER) (A)
  (FORMAT () "~D//~D" (LDB (BYTE 8. 9.) A) (LDB (BYTE 8. 0.) A)))

(DEFUN (:PROPERTY :RCC ADDRESS-UNPARSER) (A)
  (FORMAT () "~D//~D" (LDB (BYTE 8. 9.) A) (LDB (BYTE 8. 0.) A)))

(DEFUN (:PROPERTY :LCS ADDRESS-UNPARSER) (A)
  (FORMAT () "~O//~O" (LDB (BYTE 8. 8.) A) (LDB (BYTE 8. 0.) A)))

(DEFUN (:PROPERTY :SU ADDRESS-UNPARSER) (A)
  (FORMAT () "~O#~O" (LDB (BYTE 8. 8.) A) (LDB (BYTE 8. 0.) A)))

;;;???This gets DEFF'd later, somewhere, to be IP:CANONICAL-IP... -Keith 21-oct-88

(defun (:property :internet address-unparser) (a)
  (format () "~d.~d.~d.~d"
          (load-byte a #o30 #o10)
          (load-byte a #o20 #o10)
          (load-byte a #o10 #o10)
          (load-byte a #o00 #o10)))

;;; Dial and Chaos nets are handled here
(defun default-address-unparser (a)
  (typecase a
    (number (format () "~O" A))
    (string a)
    (t (format () "~A" a))))

(defmethod (host :unparsed-network-address) (network &optional smart-p)
  (let ((a (send self :network-address network smart-p)))
    (and a
         (funcall (or (get network 'address-unparser) 'default-address-unparser) a))))

(defmethod (host :unparsed-network-addressES) (network)
  (MAPCAR (or (get network 'address-unparser) 'default-address-unparser)
          (GETF (HOST-ADDRESSES ALIST-ELEM) NETWORK)))

(defmethod (host :internet-addresses) ()
  (let ((addresses (send self :network-addresses)))
    (getf addresses :internet)))

(defmethod (host :internet-address) ()
  (let ((addresses (send self :network-addresses)))
    (car (getf addresses :internet))))

(DEFMETHOD (HOST :CHAOS-ADDRESSES) ()
  (LET ((ADDRESSES (SEND SELF :NETWORK-ADDRESSES)))
    (getf ADDRESSES :CHAOS)))

(DEFMETHOD (HOST :CHAOS-ADDRESS) ()
  (LET ((ADDRESSES (SEND SELF :NETWORK-ADDRESSES)))
    (CAR (getf ADDRESSES :CHAOS))))

(DEFMETHOD (HOST :SET-SITE) (NEW-SITE) (SETF (HOST-SITE-NAME ALIST-ELEM) NEW-SITE))

(DEFMETHOD (HOST :CONNECT) (CONTACT-NAME) (SEND SELF :CHAOS-CONNECT CONTACT-NAME))
(DEFMETHOD (HOST :CHAOS-CONNECT) (CONTACT-NAME) (CHAOS:CONNECT SELF CONTACT-NAME))

(DEFMETHOD (HOST :FILE-SYSTEM-TYPE) () (SEND SELF :SYSTEM-TYPE))

(DEFUN COMPUTE-HOST-FLAVOR (ELEM)
  "Compute the flavor to be used for a host object, from the host alist element.
Creates and composes a new flavor if necessary."
  (OR (GET (HOST-SYSTEM-TYPE-INTERNAL ELEM) 'HOST-FLAVOR)
      (GET :DEFAULT 'HOST-FLAVOR)))

;;;If non-NIL, this should be a function that accepts a host name
;;;and, if possible, defines and returns host object.
;;;Set in CHUSE.LISP to the CHAOS-UNKNOWN-HOST-FUNCTION.

(DEFVAR UNKNOWN-HOST-FUNCTION NIL)

(DEFINE-SITE-VARIABLE LOCAL-INTERNET-DOMAINS :LOCAL-INTERNET-DOMAINS
  "List of domains to which our site belongs.
If a host is specified with one of these domains, we ignore the domain.
If a host is specified with an unrecognized domain,
 we do not look in our own host table, always ask a host table server,
 and keep the domain in the host name.")

(DEFUN PARSE-HOST (HOST &OPTIONAL NO-ERROR-P (UNKNOWN-OK T)
                   &AUX ELEMENT)
  "Return a host object for name HOST, taken from the HOST-ALIST.
This is the right function to use for making network connections,
but is not right for parsing pathnames.  Use FS:GET-PATHNAME-HOST for that.
HOST can also be a host object already; then it's simply returned.
NO-ERROR-P says just return NIL if there is no such host known.
UNKNOWN-OK says call the UNKNOWN-HOST-FUNCTION (if that's not NIL)
to give it a chance to create a host and add it to the host table."
  (cond ((TYPEP HOST 'HOST) HOST)
        (t
         (LET ((IDX (STRING-SEARCH-CHAR #/. HOST)))
           (AND IDX
                (MEMBER-EQUALP (SUBSTRING HOST (1+ IDX)) LOCAL-INTERNET-DOMAINS)
                (SETQ HOST (SUBSTRING HOST 0 IDX))))
         (COND ((AND (SETQ ELEMENT (LOOP FOR ELEMENT IN HOST-ALIST
                                         WHEN (or (MEM #'STRING-EQUAL HOST
                                                       (HOST-NAME-LIST ELEMENT))
                                                  (string-equal host (host-name element)))
                                         RETURN ELEMENT))
                     (NOT (NULL (HOST-ADDRESSES ELEMENT))))
                (GET-ALIST-ELEM-HOST ELEMENT))
               ((STRING-EQUAL HOST "CHAOS|" :END1 6 :END2 6)
                (LET ((ADDRESS (PARSE-NUMBER HOST 6 NIL 8)))
                  (OR (GET-HOST-FROM-ADDRESS ADDRESS :CHAOS)
                      (MAKE-UNNAMED-HOST :DEFAULT `(:CHAOS (,ADDRESS))))))
               ((AND UNKNOWN-OK UNKNOWN-HOST-FUNCTION)
                (FUNCALL UNKNOWN-HOST-FUNCTION HOST)
                (PARSE-HOST HOST NO-ERROR-P NIL))
               (NO-ERROR-P
                NIL)
               (T
                (FERROR 'SYS:UNKNOWN-HOST-NAME "~S is not a known host." HOST))))))

(DEFUN GET-ALIST-ELEM-HOST (ELEM)
  "Given an element ELEM of SI:HOST-ALIST, return the host object for it."
  (OR (HOST-INSTANCE ELEM)
      (LET ((INSTANCE (MAKE-INSTANCE (COMPUTE-HOST-FLAVOR ELEM) :ALIST-ELEM ELEM)))
        (SETF (HOST-INSTANCE ELEM) INSTANCE)
        INSTANCE)))

(DEFUN GET-HOST-FROM-ADDRESS (ADDRESS NETWORK &AUX H)
  "Return the host object which has the address ADDRESS on NETWORK, or NIL if none."
  (DOLIST (ELEM SI:HOST-ALIST)
    (WHEN (MEMber-equal ADDRESS (GETF (SI:HOST-ADDRESSES ELEM) NETWORK))
      (COND (H ; this is ANONYMOUS, and therefore SUPERFLUOUS.  It can be flushed
             (SETQ SI:HOST-ALIST (DELQ (ASSOC (SEND H :NAME) SI:HOST-ALIST) SI:HOST-ALIST))
             ;;; Space here for dealing with pathname/host frobbage (hard !)
             (RETURN (GET-ALIST-ELEM-HOST ELEM)))
            ; Save the host if it is anonymous, so that we can return it later if a
            ; real host is not around.
            ((NOT (GET (SETQ H (GET-ALIST-ELEM-HOST ELEM)) 'ANONYMOUS))
             (RETURN H)))))
  H)

(DEFUN MAKE-UNNAMED-HOST (SYSTEM-TYPE ADDRESSES &AUX ELEM)
  (SETQ ELEM (MAKE-HOST-ALIST-ELEM :NAME (FORMAT NIL "UNKNOWN-~A-~O"
                                                 (CAR ADDRESSES) (CAADR ADDRESSES))
                                   :SYSTEM-TYPE-INTERNAL SYSTEM-TYPE
                                   :SITE-NAME SITE-NAME
                                   :ADDRESSES ADDRESSES))
  (PUSH ELEM HOST-ALIST)
  (LET ((HOST (GET-ALIST-ELEM-HOST ELEM)))
    (SETF (GET HOST 'ANONYMOUS) T)
    HOST))

(DEFMACRO DO-ALL-HOSTS ((HOST) &BODY BODY)
  (LET ((ELEM-VAR (GENSYM)))
    `(LET ((,HOST NIL))
       (DOLIST (,ELEM-VAR SI:HOST-ALIST)
         (SETQ ,HOST (GET-ALIST-ELEM-HOST ,ELEM-VAR))
         ,@BODY))))

;;; Mixins for various host operating systems.
;;; These contain methods that relate to the kind of host
;;; but not to the protocol or network used to access it.

(DEFPROP :DEFAULT DEFAULT-SYSTEM-TYPE-MIXIN SYSTEM-TYPE-FLAVOR)
(DEFFLAVOR DEFAULT-SYSTEM-TYPE-MIXIN () () (:REQUIRED-FLAVORS HOST))
(DEFFLAVOR DEFAULT-HOST ()
           (DEFAULT-SYSTEM-TYPE-MIXIN HOST))
(DEFPROP :DEFAULT DEFAULT-HOST HOST-FLAVOR)

(DEFPROP :ITS HOST-ITS-MIXIN SYSTEM-TYPE-FLAVOR)
(DEFFLAVOR HOST-ITS-MIXIN () () (:REQUIRED-FLAVORS HOST))

(DEFMETHOD (HOST-ITS-MIXIN :PRIMARY-DEVICE) () "DSK")

(DEFPROP :TOPS-20 HOST-TOPS20-MIXIN SYSTEM-TYPE-FLAVOR)
(DEFFLAVOR HOST-TOPS20-MIXIN ((PRIMARY-DEVICE "PS")) ()
  (:REQUIRED-FLAVORS HOST)
  :SETTABLE-INSTANCE-VARIABLES)

(defmethod (host-tops20-mixin :reset-primary-device) ()
  (setq primary-device "PS"))

(DEFMETHOD (HOST-TOPS20-MIXIN :HSNAME-PATHNAME) (STRING HOST)
  (LET ((PN (FS:PARSE-PATHNAME (FS:TOPS20-LOCAL-PATHSTRING STRING HOST) HOST)))
    (IF (NULL (SEND PN :DEVICE))
        (SEND PN :NEW-DEVICE (SEND HOST :PRIMARY-DEVICE))
      PN)))

(DEFPROP :TENEX HOST-TENEX-MIXIN SYSTEM-TYPE-FLAVOR)
(DEFFLAVOR HOST-TENEX-MIXIN () () (:REQUIRED-FLAVORS HOST))

(DEFMETHOD (HOST-TENEX-MIXIN :PRIMARY-DEVICE) () "DSK")

(DEFMETHOD (HOST-TENEX-MIXIN :HSNAME-PATHNAME) (STRING HOST)
  (FS:MAKE-PATHNAME :HOST HOST :DEVICE "DSK" :DIRECTORY STRING))

(DEFPROP :UNIX HOST-UNIX-MIXIN SYSTEM-TYPE-FLAVOR)
(DEFFLAVOR HOST-UNIX-MIXIN () () (:REQUIRED-FLAVORS HOST))

(DEFMETHOD (HOST-UNIX-MIXIN :PRIMARY-DEVICE) () NIL)

(DEFMETHOD (HOST-UNIX-MIXIN :HSNAME-PATHNAME) (STRING HOST)
  (FS:PARSE-PATHNAME STRING HOST))

(DEFPROP :MULTICS HOST-MULTICS-MIXIN SYSTEM-TYPE-FLAVOR)
(DEFFLAVOR HOST-MULTICS-MIXIN () () (:REQUIRED-FLAVORS HOST))

(DEFMETHOD (HOST-MULTICS-MIXIN :PRIMARY-DEVICE) () NIL)

(DEFMETHOD (HOST-MULTICS-MIXIN :HSNAME-PATHNAME) (STRING HOST)
  (SEND (FS:PARSE-PATHNAME STRING HOST)
        :NEW-PATHNAME :NAME NIL :TYPE NIL :VERSION NIL))

(DEFPROP :LMFS HOST-LMFS-MIXIN SYSTEM-TYPE-FLAVOR)
(DEFFLAVOR HOST-LMFS-MIXIN () () (:REQUIRED-FLAVORS HOST))

(DEFMETHOD (HOST-LMFS-MIXIN :PRIMARY-DEVICE) () ())

(DEFMETHOD (HOST-LMFS-MIXIN :HSNAME-PATHNAME) (STRING HOST)
  (FS:MAKE-PATHNAME :HOST HOST :DIRECTORY (LIST STRING)))

(DEFPROP :VMS HOST-VMS-MIXIN SYSTEM-TYPE-FLAVOR)
(DEFFLAVOR HOST-VMS-MIXIN () () (:REQUIRED-FLAVORS HOST))

(DEFMETHOD (HOST-VMS-MIXIN :PRIMARY-DEVICE) ()
  (OR (GETF SI:PROPERTY-LIST :PRIMARY-DEVICE)
      "USRD$"))

(DEFMETHOD (HOST-VMS-MIXIN :SET-PRIMARY-DEVICE) (DEVICE)
  (SETF (GETF SI:PROPERTY-LIST :PRIMARY-DEVICE) DEVICE))

(defmethod (host-vms-mixin :reset-primary-device) ()
  (remf si:property-list :primary-device))

(DEFMETHOD (HOST-VMS-MIXIN :HSNAME-PATHNAME) (STRING HOST)
  (FS:PARSE-PATHNAME STRING HOST))

(DEFPROP :LISPM HOST-LISPM-MIXIN SYSTEM-TYPE-FLAVOR)
(DEFFLAVOR HOST-LISPM-MIXIN () () (:REQUIRED-FLAVORS HOST))

(DEFUN DESCRIBE-LOGICAL-HOST (HOST &OPTIONAL (STREAM *STANDARD-OUTPUT*) &AUX OLD-HOST)
  "Print out useful things about the logical HOST onto STREAM."
  (SETQ OLD-HOST HOST)
  (SETQ HOST (FS:GET-PATHNAME-HOST HOST))
  (IF (OR (NULL HOST)
          (NOT (SEND HOST :OPERATION-HANDLED-P :PHYSICAL-HOST)))
      (FERROR NIL "~A does not appear to be a logical host." OLD-HOST))
  (LET* ((REAL-HOST (SEND HOST :PHYSICAL-HOST))
         (TRANSLATIONS (SEND HOST :TRANSLATIONS))
         (INFO (SEND HOST :GET 'FS:MAKE-LOGICAL-PATHNAME-HOST)))
    (COND ((NULL TRANSLATIONS)
           (FORMAT STREAM "~&The logical host ~A has ~A as its physical host.
There don't appear to be any translations."
                   (SEND HOST :NAME) REAL-HOST))
          (T
           (FORMAT STREAM
                   "~&The logical host ~A translates the following pathnames on the physical host ~A:~%"
                   (SEND HOST :NAME) REAL-HOST)
           (DOLIST (TRANS TRANSLATIONS)
             (FORMAT STREAM "~& ~30A  ~A" (CAR TRANS) (CADR TRANS)))))
    (if info (format stream "~%These translations were defined by the file ~A"
                     (send (send (fs:get-pathname-host "SYS") :sample-pathname)
                           :back-translated-pathname (car info)))))
  HOST)

(DEFUN DESCRIBE-HOST (HOST-ARG &OPTIONAL (STREAM *STANDARD-OUTPUT*)
                      &AUX HOST)
  "Prints information about HOST-ARG onto STREAM."
  (SETQ HOST (PARSE-HOST HOST-ARG T T))
  (AND (NULL HOST)
       (SETQ HOST (FS:GET-PATHNAME-HOST HOST-ARG T)))
  (FRESH-LINE)
  (TYPECASE HOST
    (NULL
     (FORMAT STREAM "There seems to be no host named ~A." HOST-ARG))
    (FS:LOGICAL-HOST
     (FORMAT STREAM "The host ~A is a logical host.~2%" HOST)
     (DESCRIBE-LOGICAL-HOST HOST STREAM))
    (HOST
     (FORMAT STREAM "Host ~A has the following names: ~{ ~S~}."
             (SEND HOST :NAME)
             (SEND HOST :HOST-NAMES))
     (FORMAT STREAM "~&It is a ~A running the ~A system."
             (SEND HOST :MACHINE-TYPE) (SEND HOST :SYSTEM-TYPE))
     (LET ((INFO (SEND HOST :NETWORK-ADDRESSES)))
       (DO ((REST INFO (CDDR REST)))
           ((NULL REST))
         (LET* ((NETWORK (CAR REST))
                (ONLY-ONE-P (= (LENGTH (CADR REST)) 1)))
           ;;Would be nice show these addresses in both decimal and octal
           (FORMAT STREAM
                   "~&Its address~:[es~] on the ~A network ~:[are~;is~]:~{ ~O~}"
                   ONLY-ONE-P NETWORK ONLY-ONE-P
                   (SEND HOST :UNPARSED-NETWORK-ADDRESSES NETWORK))))
       (IF (NULL INFO)
           (FORMAT STREAM "~&WARNING: This host does not seem to be on any network."))))
    (BASIC-HOST
     (LET ((*STANDARD-OUTPUT* STREAM)) (DESCRIBE HOST)))
    (T
     (FORMAT STREAM "~&This is supposedly a host, but does not incorporate the usual flavors.~%")
     (LET ((*STANDARD-OUTPUT* STREAM)) (DESCRIBE HOST))))
  (FRESH-LINE))

(DEFUN CHECK-THIS-SITE-INTEGRITY (&AUX (TIMES-LOST 0))
  "Check to see that this host is in the host table properly."
  (WHEN (VARIABLE-BOUNDP CHAOS:MY-ADDRESS)
    (LET ((THIS-HOST (SI:GET-HOST-FROM-ADDRESS CHAOS:MY-ADDRESS :CHAOS)))
      (COND-EVERY ((NULL THIS-HOST)
                   (INCF TIMES-LOST)
                   (FORMAT T "~&This Lisp Machine has a chaosnet address of ~O (octal).
However, it doesn't appear to be in the host table.
This means that you must fix the inconsistent site files that are in SYS: SITE;"
                           CHAOS:MY-ADDRESS)) ;;change this!!
                  ((AND THIS-HOST (NOT (EQ (SEND THIS-HOST :SYSTEM-TYPE) :LISPM)))
                   (INCF TIMES-LOST)
                   (FORMAT T "~&This host is ~A and it is a Lisp Machine.
However, according to the host table, its system type is ~S.
This means that you must fix the inconsistent site files that are in SYS: SITE;"
                           THIS-HOST (SEND THIS-HOST :SYSTEM-TYPE)))
                  ((AND THIS-HOST
                        (NOT (ASSOC-EQUALP (SEND THIS-HOST :NAME) SI:MACHINE-LOCATION-ALIST))
                   (INCF TIMES-LOST)
                   (FORMAT T "~&This host, ~A, doesn't appear to have a terminal location.
Please add an entry for this host in the file SYS: SITE;LMLOCS LISP"
                           THIS-HOST))))
;I think this will be a real pain when moving to a new site.
;      (IF (> TIMES-LOST 0)
;         (FERROR 'SITE-FILES-BROKEN  ;this could be simpler, but...
;                 "Please fix the ~:[problem~;problems~] described above."
;                 (> TIMES-LOST 1)))
      )))

(ADD-INITIALIZATION "Verify this site exists" '(check-this-site-integrity) '(SITE))

;;;; LISPM Location stuff
(DEFVAR LOCAL-HOST NIL "The host object for this Lisp machine.")
(DEFVAR LOCAL-HOST-NAME NIL "The full name of this Lisp machine as a host.")
(DEFVAR LOCAL-PRETTY-HOST-NAME NIL "A pretty form of the name of this Lisp machine.")
(DEFVAR LOCAL-FINGER-LOCATION NIL "A string saying where this Lisp machine is located.
Locations are stored in the file SYS: SITE; LMLOCS LISP.")
(DEFVAR LOCAL-FLOOR-LOCATION NIL "List of building-name and floor number")
(DEFVAR ASSOCIATED-MACHINE NIL "Default machine to log in on from this Lisp machine.")

(DEFUN SET-LOCAL-HOST-VARIABLES (&AUX ELEM)
  (cond ((boundp 'fs:*pathname-host-list*)  ;avoid lossage in cold-load.
         (IF LOCAL-HOST
             (SETQ LOCAL-HOST-NAME (SEND LOCAL-HOST :NAME)))
         (COND ((SETQ ELEM (ASSOC-EQUALP LOCAL-HOST-NAME MACHINE-LOCATION-ALIST))
                (LET (TEM)
                  (SETF `(LOCAL-HOST-NAME ,LOCAL-PRETTY-HOST-NAME ,LOCAL-FINGER-LOCATION
                                          ,LOCAL-FLOOR-LOCATION ,ASSOCIATED-MACHINE
                                          ,TEM)
                        ELEM)
                  (SETQ ASSOCIATED-MACHINE (FS:GET-PATHNAME-HOST ASSOCIATED-MACHINE))
                  (SETQ HOST-OVERRIDDEN-SITE-OPTION-ALIST NIL)  ;In case error in EVAL.
                  (SETQ HOST-OVERRIDDEN-SITE-OPTION-ALIST
                        (LOOP FOR (KEY EXP) IN TEM
                              COLLECT `(,KEY . ,(EVAL EXP))))))
               (T
                (SETQ LOCAL-PRETTY-HOST-NAME "Unknown"
                      LOCAL-FINGER-LOCATION "(Unknown)"
                      LOCAL-FLOOR-LOCATION '(UNKNOWN 0)
                      ASSOCIATED-MACHINE
                      (IF (GET-SITE-OPTION :DEFAULT-ASSOCIATED-MACHINE)
                          (FS:GET-PATHNAME-HOST (GET-SITE-OPTION :DEFAULT-ASSOCIATED-MACHINE))
                          (CAR FS:*PATHNAME-HOST-LIST*))
                      HOST-OVERRIDDEN-SITE-OPTION-ALIST NIL)))
;  (unless (null local-host)
;    (setq fs:*pathname-host-list* (del-if #'(lambda (x)
;                                             (condition-case ()
;                                                 (typep x 'fs:local-file-host)
;                                               (error nil)))
;                                         fs:*pathname-host-list*)))
         (INITIALIZATIONS 'SITE-OPTION-INITIALIZATION-LIST T))))

(ADD-INITIALIZATION "SET-LOCAL-HOST-VARIABLES" '(SET-LOCAL-HOST-VARIABLES) '(:SITE))
(ADD-INITIALIZATION "SET-LOCAL-HOST-VARIABLES" '(SET-LOCAL-HOST-VARIABLES) '(:COLD))


;;; This has to go somewhere....
(DEFUN PARSE-USER-NAME
       (STRING &OPTIONAL &KEY DEFAULT-HOST (RETURN-HOST :PARSE) (USE-NET-P T))
  "Returns the user name and the host (if given) of a UNAME@HOST string.
The host returned will either be (), a string, or a host object.  If a host
name is not found in the string, default-host will be returned (it could be NIL)
-- and it is not parsed, just returned.
If a host is found, the value depends on the :RETURN-HOST argument:
  :PARSE an object or an error during parse (could not find the host object).
  :NO-ERROR an object or a string
  :STRING don't try parsing the host.
If :USE-NET-P is T (the default), then an effort is made to go over the host-table
servers if the host can't be found in the local host table.
All strings are returned trimmed of whitespace."
  (SETQ STRING (STRING-TRIM '(#/TAB #/SPACE #/RETURN) STRING))
  (LET ((@-IDX (STRING-SEARCH-CHAR #/@ STRING)))
    (IF (NOT @-IDX)
        (VALUES (STRING-RIGHT-TRIM '(#/TAB #/SPACE #/RETURN) STRING)
                DEFAULT-HOST)
      (LET ((HOST (STRING-LEFT-TRIM '(#/TAB #/SPACE #/RETURN)
                                    (SUBSTRING STRING (1+ @-IDX)))))
        (VALUES (STRING-RIGHT-TRIM '(#/TAB #/SPACE #/RETURN)
                                   (SUBSTRING STRING 0 @-IDX))
                (SELECTQ RETURN-HOST
                  (:STRING HOST)
                  (:PARSE
                   (CONDITION-CASE (RESULT) (SI:PARSE-HOST HOST () USE-NET-P)
                     (SYS:LOCAL-NETWORK-ERROR RESULT) ; return instance
                     (:NO-ERROR RESULT)))  ; return host object
                  (:NO-ERROR (OR (SI:PARSE-HOST HOST T USE-NET-P) HOST))))))))

;;; Interim Internet stuff
(defvar *chaos-use-host-name-for-tcp* t)
(defvar *default-tcp-connect-timeout* (* 20. 60.))

(defun tcp-connect-via-server (host net socket error characters
                               ascii-translation timeout &aux tcp-gws)
  (setq tcp-gws (get-site-option :arpa-gateways))
  (if (null tcp-gws)
    (let ((e (make-instance 'sys:network-error :format-string "No Internet available")))
      (if error e (signal e)))
    (do* ((gws tcp-gws (cdr gws))
          (gw (car gws) (car gws))
          (contact (format () "TCP ~A ~O"
                           (cond ((stringp host) host)
                                 ((or (eq net :arpa) *chaos-use-host-name-for-tcp*)
                                  (send host :name))
                                 (t (send host :unparsed-network-address net)))
                           socket))
          (stream))
         ((null gws)) ; should not exit this way
      (setq stream
            (chaos:open-stream gw contact :error nil :characters characters
                               :ascii-translation ascii-translation
                               :timeout (or timeout *default-tcp-connect-timeout*)))
      (if (not (errorp stream)) (return stream))
      (when (null (second gws)) ; No more gateways to try
        (let ((e (make-condition 'sys:connection-error :host host
                                 :format-string "Cannot connect to ~A"
                                 :format-args (list host))))
          (if error (signal e) e))))))

(defvar *tcp-implementation* 'tcp-connect-via-server)

(defmethod (host :internet-connect)
           (protocol socket &key (ascii-translation t) (characters t) (error t) timeout
                            &rest options &allow-other-keys &aux net)
  options
  (if (neq protocol :tcp)
      (let ((e (make-instance 'sys:network-error
                              :format-string "~S is not an implemented Internet protocol"
                              :FORMAT-ARGS (LIST PROTOCOL))))
        (if error e (signal e)))
    (if (or (and (send self :network-typep :internet) (setq net :internet))
            (and (send self :network-typep :arpa) (setq net :arpa)))
        (funcall *tcp-implementation* self net socket error characters ascii-translation timeout)
      (let ((e (make-instance 'sys:network-error ; :host self  -- don't think one can supply this
                              :format-string "~A is not reachable via Internet"
                              :format-args (list self))))
        (if error (signal e) e)))))

(defmethod (host :fasd-form) ()
  ;;;A method for giving the compiler a form that when eval'd will reconstruct SELF.
  `(prog(host hostname oldtype)
        (setq oldtype ',(type-of self))
        (setq hostname ,(send self :name))
        (setq host (si:parse-host hostname))
        (unless (typep host oldtype)
          (warn "When host ~a was compiled, it's type was ~s, but now it is type ~s"
                hostname oldtype (type-of host)))
        (return host)))
