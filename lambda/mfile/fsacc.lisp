;;; -*- Mode: Lisp; Package: Moby-File-System; Base: 10.; Readtable: T -*-

(defflavor moby-host-mixin
         (moby-partition-host-name
          mpa-defstruct
          host-index)   ;index into *moby-host-index*
         (SI:HOST-LISPM-MIXIN FS:FILE-HOST-LISPM-MIXIN SI:HOST)
  (:initable-instance-variables moby-partition-host-name mpa-defstruct host-index)
  (:gettable-instance-variables moby-partition-host-name mpa-defstruct host-index)
  )

(DEFMETHOD (MOBY-HOST-MIXIN :FILE-SYSTEM-TYPE) () :LISPM)

(DEFMETHOD (MOBY-HOST-MIXIN :NAME-AS-FILE-COMPUTER) ()
  MOBY-PARTITION-HOST-NAME)

(DEFMETHOD (MOBY-HOST-MIXIN :GET-ACCESS) ()
  (OR fs:ACCESS (SEND SELF :DETERMINE-ACCESS)))

(defmethod (moby-host-mixin :after :init) (&rest ignore)
  (cond ((or (not (variable-boundp host-index))
             (null host-index))
         (setq host-index (moby-assign-host-index self)))
        (t (setf (aref *moby-host-index* host-index) self))))  ;if prevously reserved..

;; This file contains the access interface to the local file system.

(DEFFLAVOR MOBY-FILE-ACCESS () (FS:DIRECTORY-LIST-MIXIN FS:BASIC-ACCESS))

(fs:DEFINE-FILE-ACCESS MOBY-FILE-ACCESS .95S0
  :moby)

(defun (:moby fs:file-access-condition) (host ignore)
  (typep host 'moby-file-host))

(DEFMETHOD (MOBY-FILE-ACCESS :RESET) () (SEND SELF :CLOSE-ALL-FILES))

(DEFMETHOD (MOBY-FILE-ACCESS :OPEN-STREAMS) ()
  MLM-FILE-STREAMS-LIST)

(DEFMETHOD (MOBY-FILE-ACCESS :ACCESS-DESCRIPTION) ()
  "Direct")

(DEFMETHOD (MOBY-FILE-ACCESS :CLOSE-ALL-FILES) (&OPTIONAL (MODE :ABORT))
  (DOLIST (S MLM-FILE-STREAMS-LIST)
    (CLOSE S MODE)))

(DEFMETHOD (MOBY-FILE-ACCESS :HOMEDIR) (&OPTIONAL (USER USER-ID))
  (MAKE-PATHNAME :HOST ;MOBY-LOCAL-HOST
                   (funcall fs:host :moby-partition-host-name)
                   :DIRECTORY USER))

(DEFMETHOD (MOBY-FILE-ACCESS :CHANGE-PROPERTIES) (PATHNAME ERROR-P &REST PLIST)
  (IDENTIFY-FILE-OPERATION :CHANGE-PROPERTIES
    (HANDLING-ERRORS ERROR-P
      (LET ((ph (FUNCALL FS:HOST :MOBY-PARTITION-HOST-NAME)))
        (OPEN-INPUT-FILE (PH FILE PATHNAME)
          (MBFS-CHANGE-FILE-PROPERTIES PH FILE PLIST))))))

(DEFMETHOD (MOBY-FILE-ACCESS :PROPERTIES) (PATHNAME &OPTIONAL ERROR-P)
  (IDENTIFY-FILE-OPERATION :PROPERTIES
    (HANDLING-ERRORS ERROR-P
      (LET ((PH (FUNCALL FS:HOST :MOBY-PARTITION-HOST-NAME)))
        (OPEN-INPUT-FILE (PH F PATHNAME)
          (VALUES (CONS (FILE-TRUENAME F) (MBFS-FILE-PROPERTIES F))
                  MLM-UNSETTABLE-PROPERTIES))))))

(DEFMETHOD (MOBY-FILE-ACCESS :COMPLETE-STRING) (PATHNAME STRING OPTIONS)
  (MULTIPLE-VALUE-BIND (DEV DIR NAM TYP VER)
      (SEND PATHNAME :PARSE-NAMESTRING fs:HOST STRING)
    (MULTIPLE-VALUE-BIND (NEW-DIRECTORY NEW-NAME NEW-TYPE COMPLETION)
        (MBFS-COMPLETE-PATH (FUNCALL FS:HOST :MOBY-PARTITION-HOST-NAME)
                            (OR DIR (FS:PATHNAME-RAW-DIRECTORY PATHNAME) "")
                            (OR NAM "") (OR TYP "")
                            (FS:PATHNAME-RAW-NAME PATHNAME)
                            (FS:PATHNAME-RAW-TYPE PATHNAME)
                            OPTIONS)
      (VALUES (MLM-NAMESTRING fs:HOST (OR DEV (FS:PATHNAME-RAW-DEVICE PATHNAME))
                             NEW-DIRECTORY NEW-NAME NEW-TYPE VER)
              COMPLETION))))

(DEFMETHOD (MOBY-FILE-ACCESS :CREATE-DIRECTORY) (PATHNAME &OPTIONAL (ERROR T))
  (IDENTIFY-FILE-OPERATION :CREATE-DIRECTORY
    (HANDLING-ERRORS ERROR
      (MBFS-CREATE-DIRECTORY (FUNCALL FS:HOST :MOBY-PARTITION-HOST-NAME)
                             (FS:PATHNAME-RAW-DIRECTORY PATHNAME))
      T)))

(DEFMETHOD (MOBY-FILE-ACCESS :CREATE-LINK) (PATHNAME TARGET &OPTIONAL (ERROR T))
  PATHNAME TARGET
  (HANDLING-ERRORS ERROR
    (MLM-SIGNAL-ERROR 'LINKS-NOT-SUPPORTED NIL NIL :CREATE-LINK)))

(DEFMETHOD (MOBY-FILE-ACCESS :REMOTE-CONNECT) (PATHNAME &OPTIONAL (ERROR T) IGNORE)
  PATHNAME
  (HANDLING-ERRORS ERROR
    (MLM-SIGNAL-ERROR 'FS:UNKNOWN-OPERATION NIL NIL :REMOTE-CONNECT)))

(DEFMETHOD (MOBY-FILE-ACCESS :OPEN) (FILE PATHNAME &REST OPTIONS)
  (APPLY 'MBFS-OPEN-FILE FS:HOST PATHNAME
         (FS:PATHNAME-RAW-DIRECTORY FILE)
         (FS:PATHNAME-RAW-NAME FILE)
         (FS:PATHNAME-RAW-TYPE FILE)
         (FS:PATHNAME-RAW-VERSION FILE)
         OPTIONS))

(DEFMETHOD (MOBY-FILE-ACCESS :DIRECTORY-LIST) (PATHNAME OPTIONS)
  (MBFS-DIRECTORY-LIST FS:HOST PATHNAME fs:HOST
         (FS:PATHNAME-RAW-DIRECTORY PATHNAME)
         (FS:PATHNAME-RAW-NAME PATHNAME)
         (FS:PATHNAME-RAW-TYPE PATHNAME)
         (FS:PATHNAME-RAW-VERSION PATHNAME)
         OPTIONS))

(DEFMETHOD (MOBY-FILE-ACCESS :RENAME) (PATHNAME NEW-NAME &OPTIONAL (ERROR-P T))
  (IDENTIFY-FILE-OPERATION :RENAME
    (HANDLING-ERRORS ERROR-P
      (LET ((PH (FUNCALL FS:HOST :MOBY-PARTITION-HOST-NAME)))
        (OPEN-INPUT-FILE (PH FILE PATHNAME)
          (MBFS-RENAME-FILE PH FILE
                          (FS:PATHNAME-DIRECTORY NEW-NAME)
                          (OR (FS:PATHNAME-NAME NEW-NAME) "FOO")
                          (OR (FS:PATHNAME-TYPE NEW-NAME) :UNSPECIFIC)
                          (FS:PATHNAME-VERSION NEW-NAME)))))))

(DEFMETHOD (MOBY-FILE-ACCESS :DELETE) (PATHNAME &OPTIONAL (ERROR-P T))
  (IDENTIFY-FILE-OPERATION :DELETE
    (HANDLING-ERRORS ERROR-P
      (OPEN-INPUT-FILE-OR-DIRECTORY (FS:HOST FILE PATHNAME)
        (MBFS-DELETE-FILE FS:HOST FILE)))))

(DEFMETHOD (MOBY-FILE-ACCESS :EXPUNGE) (PATHNAME &OPTIONAL (ERROR T))
  (IDENTIFY-FILE-OPERATION :EXPUNGE
    (HANDLING-ERRORS ERROR
      (MBFS-EXPUNGE-DIRECTORY FS:HOST
        (FS:PATHNAME-RAW-DIRECTORY PATHNAME)
        (FS:PATHNAME-RAW-NAME PATHNAME)
        (FS:PATHNAME-RAW-TYPE PATHNAME)
        (FS:PATHNAME-RAW-VERSION PATHNAME)))))

(DEFMETHOD (MOBY-FILE-ACCESS :DELETE-MULTIPLE-FILES) (ERROR-P PATHNAMES)
  (IDENTIFY-FILE-OPERATION :DELETE
    (HANDLING-ERRORS ERROR-P
      (LOOP FOR PATHNAME IN PATHNAMES
            WITH FILES-OF-DIRECTORY-TO-WRITE = NIL
            DO (OPEN-INPUT-FILE-OR-DIRECTORY (fs:host FILE PATHNAME)
                 (MBFS-DELETE-FILE fs:host FILE NIL)
                 (LOOP FOR ENTRY IN FILES-OF-DIRECTORY-TO-WRITE
                       WHEN (EQUAL (MFILE-DIRECTORY FILE) (MFILE-DIRECTORY ENTRY))
                       RETURN NIL
                       FINALLY (PUSH FILE FILES-OF-DIRECTORY-TO-WRITE)))
;             FINALLY
;             (DOLIST (FILE FILES-OF-DIRECTORY-TO-WRITE)
;               (WRITE-DIRECTORY-OF-FILE FILE))
              finally
              (moby-writeout-root fs:host)
              ))))

(DEFMETHOD (MOBY-FILE-ACCESS :ALL-DIRECTORIES) (OPTIONS)
  (MBFS-ALL-DIRECTORIES FS:HOST FS:HOST (NOT (MEMQ :NOERROR OPTIONS))))

(DEFMETHOD (MOBY-FILE-ACCESS :MULTIPLE-FILE-PLISTS) (PATHNAMES OPTIONS)
  "This is a hack to speed up DIRED.
There are no currently meaningful options." OPTIONS
  (IDENTIFY-FILE-OPERATION :PROPERTIES
    (MAPCAR #'(LAMBDA (PATHNAME)
                (LET ((TPATHNAME (SEND PATHNAME :TRANSLATED-PATHNAME)))
                  (OPEN-INPUT-FILE (fs:host FILE TPATHNAME)
                    (IF (NULL FILE)
                        (LIST PATHNAME)
                      (LIST* PATHNAME
                             :TRUENAME (FILE-TRUENAME FILE)
                             (MBFS-FILE-PROPERTIES FILE))))))
              PATHNAMES)))

(COMPILE-FLAVOR-METHODS MOBY-FILE-ACCESS)

;(DEFVAR MOBY-LOCAL-HOST NIL)
;(defvar *previous-local-host* (IF (VARIABLE-BOUNDP MOBY-LOCAL-HOST) MOBY-LOCAL-HOST))

;(defun add-local-pathname-host ()
;  (setq FS:*pathname-host-list* (delq *previous-local-host* FS:*pathname-host-list*))
;  (cond ((null moby-local-host)
;        (si:define-host "MLM" :system-type :moby-lispm)
;        (setq moby-local-host (fs:get-pathname-host "MLM"))))
;  (setq *previous-local-host* MOBY-LOCAL-HOST)
;  (fs:add-file-computer MOBY-LOCAL-HOST))

;(add-initialization "Add Moby Local Pathname Host" '(add-local-pathname-host) '(:warm :now))
;(add-initialization "Add Moby Local Pathname Host" '(add-local-pathname-host) :site)

;;; A special host flavor for the local file system.  The local host used to be
;;; treated specially by the the :PATHNAME-HOST-P method, which compared SELF
;;; against SI:LOCAL-HOST to determine if "MLM" was a valid host.  This causes
;;; problems when SI:LOCAL-HOST is not defined, as when changing the name of
;;; a machine and updating the site files.  DEFINE-MOBY-FILE-SYSTEM-HOST defines
;;; a valid host, connected to the local file system, without concern for any
;;; of the site definitions.  KHS 1/9/85.
;;;
;;; This thing should be added only when there are site information problems;
;;; it causes lot of pain when things are otherwise OK.  We should advertise
;;; the function DEFINE-MOBY-FILE-SYSTEM-HOST just in case the site information
;;; requires it.

;REFERENCES TO SI:LOCAL-HOST CHANGED TO MOBY-LOCAL-HOST FOR NOW.

(DEFFLAVOR MOBY-FILE-HOST
  ()
  (MOBY-HOST-MIXIN)
  )

(DEFMETHOD (MOBY-FILE-HOST :AFTER :INIT) (&REST IGNORE)
  (SETQ fs:APPROPRIATE-ACCESS-FLAVORS '(MOBY-FILE-ACCESS)))

(DEFMETHOD (MOBY-FILE-HOST :DETERMINE-ACCESS) ()
  (SETQ fs:ACCESS (MAKE-INSTANCE 'MOBY-FILE-ACCESS :HOST SELF)))

(DEFUN UNDEFINE-MOBY-FILE-SYSTEM-HOST (PARTITION-HOST)
  (SETQ FS:*PATHNAME-HOST-LIST*
        (DEL-IF #'(LAMBDA (X) (AND (OR (TYPEP X 'MOBY-FILE-HOST)
                                       (TYPEP X 'MOBY-REMOTE-HOST))
                                   (OR (EQ PARTITION-HOST :ALL)
                                       (EQ PARTITION-HOST
                                           x
                                           ;(SEND X :MOBY-PARTITION-HOST-NAME)
                                           ))))
                FS:*PATHNAME-HOST-LIST*)))

;;; Remove the kludge before disk-save time
(add-initialization "Remove MOBY host" '(undefine-moby-file-system-host :ALL)
                    '(:before-cold))

;(defvar moby-local-host)

(DEFUN DEFINE-MOBY-FILE-SYSTEM-HOST (partition-host-name mpa-defstruct moby-host-index)
  ;moby-host-index can be nil if not committed, or an index previously reserved
  ; with a placeholder.

  (let ((oph (partition-host-of-partition-host-name partition-host-name t)))
    (if oph (undefine-moby-file-system-host oph)))
  (LET ((HOST-STRUCTURE (SI:MAKE-HOST-ALIST-ELEM :NAME partition-host-name
                                                 :NAME-LIST (list partition-host-name) ;'("MLM" "MOBY-LOCAL")
                                                 :SYSTEM-TYPE-INTERNAL :LISPM
                                                 :MACHINE-TYPE-INTERNAL :LISPM
                                                 :SITE-NAME SITE-NAME
                                                 :ADDRESSES NIL)))
    (LET ((HOST-OBJECT (MAKE-INSTANCE 'MOBY-FILE-HOST
                                      :ALIST-ELEM HOST-STRUCTURE
                                      :moby-partition-host-name partition-host-name
                                      :mpa-defstruct mpa-defstruct
                                      :host-index moby-host-index)))
      (SETF (SI:HOST-INSTANCE HOST-STRUCTURE) HOST-OBJECT)
      (SETQ FS:*PATHNAME-HOST-LIST* (NCONC FS:*PATHNAME-HOST-LIST* (NCONS HOST-OBJECT)))
      ;(setq moby-local-host HOST-OBJECT)
      host-object
      )))


(DEFFLAVOR MOBY-REMOTE-HOST
  (REMOTE-PARTITION-HOST-NAME
   REMOTE-LISPM-HOST)
  (MOBY-HOST-MIXIN)
  (:initable-instance-variables remote-partition-host-name
                                remote-lispm-host)
  (:gettable-instance-variables remote-partition-host-name
                                remote-lispm-host))

(DEFMETHOD (MOBY-REMOTE-HOST :AFTER :INIT) (&REST IGNORE)
  (SETQ fs:APPROPRIATE-ACCESS-FLAVORS '(MOBY-REMOTE-ACCESS)))

(DEFMETHOD (MOBY-REMOTE-HOST :DETERMINE-ACCESS) ()
  (SETQ fs:ACCESS (MAKE-INSTANCE 'MOBY-REMOTE-ACCESS :HOST SELF)))

(defmethod (moby-remote-host :remote-host-name) ()  ;FILE-PRINT-PATHNAME looks for this method.
  remote-partition-host-name)

(defflavor moby-remote-access () (fs:lispm-qfile-access))

;seems to be ok to inherit below method from LISPM-QFILE-ACCESS.
;(defmethod (moby-remote-access :open-command) (hu handle file characters byte-size direction
;                                              if-exists if-exists-p if-does-not-exist
;                                              &rest options)
;  (declare (ignore characters byte-size))
;  (SEND hu :COMMAND NIL handle nil "LispM Open" "OPEN-FOR-LISPM "
;       #/NEWLINE
;       (fs:FILE-PRINT-PATHNAME FILE) #/NEWLINE
;       (fs:QFILE-LISPM-OPEN-OPTIONS-STRING
;         DIRECTION OPTIONS IF-EXISTS IF-EXISTS-P IF-DOES-NOT-EXIST)))

(defun define-remote-moby-host (partition-host-name remote-lispm-host
                  &optional (remote-partition-host-name partition-host-name))
  (if (not (boundp '*moby-page-association*)) (moby-initialize-transforms))
  (setq partition-host-name (string-upcase partition-host-name))
  (let ((oph (partition-host-of-partition-host-name partition-host-name t)))
    (if oph (undefine-moby-file-system-host oph)))
  (cond ((stringp remote-lispm-host)
         (setq remote-lispm-host (si:parse-host (string-upcase remote-lispm-host)))))
  (LET* ((HOST-STRUCTURE (SI:MAKE-HOST-ALIST-ELEM
                           :NAME partition-host-name
                           :NAME-LIST (list partition-host-name)
                           :SYSTEM-TYPE-INTERNAL :LISPM
                           :MACHINE-TYPE-INTERNAL :LISPM
                           :SITE-NAME SITE-NAME
                           :ADDRESSES (send remote-lispm-host :network-addresses)))
         (HOST-OBJECT (MAKE-INSTANCE 'MOBY-REMOTE-HOST
                                     :ALIST-ELEM HOST-STRUCTURE
                                     :moby-partition-host-name partition-host-name
                                     :remote-partition-host-name remote-partition-host-name
                                     :remote-lispm-host remote-lispm-host)))
      (SETF (SI:HOST-INSTANCE HOST-STRUCTURE) HOST-OBJECT)
      (SETQ FS:*PATHNAME-HOST-LIST* (NCONC FS:*PATHNAME-HOST-LIST* (NCONS HOST-OBJECT)))

  ;open to assure section is mapped.  We wont need to actually follow package list structure,
  ; but symbols we receive will contain pointers to symbol defstructs in this section.
      (process-run-function "Open package root"
         'define-remote-assure-package-root partition-host-name)
      host-object))

(defun define-remote-assure-package-root (partition-host-name)
  (open (moby-package-root-string-for-partition-host partition-host-name)
        :direction :output :moby-mapped t :write-once t
        :if-exists :append :if-does-not-exist :create))


;;; Get a MOBY-CONNECTION.   Analogous to :GET-DATA-CONNECTION.
;;; Make two passes over existing units, first trying open ones.
fs:(DEFMETHOD (DC-ACCESS-MIXIN :GET-MOBY-CONNECTION) (&OPTIONAL NOERROR-P)
  (DO-NAMED TOP ((ERROR-P NIL T)) (NIL)
    (DO ((UNITS HOST-UNITS (CDR UNITS))
         (UNIT) (DATA-CONN))
        ((NULL UNITS))
      (SETQ UNIT (CAR UNITS))
      (AND (SEND UNIT :VALIDATE-CONTROL-CONNECTION (OR NOERROR-P (NOT ERROR-P)))
           (SETQ DATA-CONN (SEND UNIT :GET-MOBY-CONNECTION))
           (RETURN-FROM TOP (values DATA-CONN UNIT))))
    (AND NOERROR-P
         (NOT (SEND SELF :GET-HOST-UNIT T))     ;If you can't get a valid connection,
         (RETURN-FROM TOP (values NIL NIL)))            ;then you have a loosing host.
    (AND ERROR-P
         (LET* ((UNIT (SEND SELF :NEW-HOST-UNIT))
                (DATA-CONN (SEND UNIT :GET-MOBY-CONNECTION)))
           (OR DATA-CONN
               (FERROR NIL "New unit failed to allocate moby connection"))
           (RETURN-FROM TOP (values DATA-CONN UNIT))))))

fs:(DEFMETHOD (QFILE-HOST-UNIT :NEW-MOBY-CONNECTION) ()
  (LET* ((DEFAULT-CONS-AREA SYS:BACKGROUND-CONS-AREA)
         (INPUT-HANDLE (FILE-GENSYM "I"))
         (OUTPUT-HANDLE (FILE-GENSYM "O"))
         CONNECTION
         SUCCESS)                               ;T => don't remove-conn the connection.
    (UNWIND-PROTECT
      (PROG ()
         RETRY
           (SETQ CONNECTION
                 (CHAOS:LISTEN OUTPUT-HANDLE *QFILE-DATA-WINDOW-SIZE* NIL))
           (LET ((PKT (CHAOS:GET-PKT))
                 (ID (FILE-MAKE-TRANSACTION-ID))
                 (DATA-CONN))
             (CHAOS:SET-PKT-STRING PKT ID "  MOBY-CONNECTION " INPUT-HANDLE " " OUTPUT-HANDLE)
             (CHAOS:SEND-PKT CONTROL-CONNECTION PKT)
             (UNLESS (CHAOS:WAIT CONNECTION 'CHAOS:LISTENING-STATE (* 60. 30.) "File Data Connection")
               ;; Attempt to establish connection timed out -- give reasonable error
               (CERROR :RETRY-FILE-OPERATION NIL 'NETWORK-LOSSAGE
                       "Attempt to establish file data connection timed out.")
               ;; It lost; tell the connection we gave up,
               (CHAOS:CLOSE-CONN CONNECTION)
               ;; wait for the server to report the failure on our side,
               ;; or say why it failed, or something.
               (CHAOS:RETURN-PKT
                 (FILE-WAIT-FOR-TRANSACTION ID CONTROL-CONNECTION "File Data Connection"))
               ;; then try again
               (GO RETRY))
             (CHAOS:ACCEPT CONNECTION)
             (SETQ PKT (FILE-WAIT-FOR-TRANSACTION ID CONTROL-CONNECTION "File Data Connection"))
             (UNWIND-PROTECT
               (LET ((STRING (CHAOS:PKT-STRING PKT)))
                 (SETQ STRING (NSUBSTRING STRING (1+ (STRING-SEARCH-CHAR #/SPACE STRING))))
                 (COND ((QFILE-CHECK-COMMAND "MOBY-CONNECTION" STRING T)
                        (SETQ DATA-CONN (MAKE-DATA-CONNECTION CONNECTION INPUT-HANDLE OUTPUT-HANDLE))
                        (setf (data-stream data-conn :input) 'moby)     ;mark as allocated.
                        (setf (data-stream data-conn :output) 'moby)
                        (PUSH DATA-CONN DATA-CONNECTIONS))
                       (T (QFILE-PROCESS-ERROR-NEW STRING))))   ;not proceedable
               (CHAOS:RETURN-PKT PKT))
             (SETQ SUCCESS T)
             (RETURN DATA-CONN)))
      ;; If we are not putting CONNECTION into the host unit, free it.
      (UNLESS SUCCESS
        (WHEN CONNECTION
          (SEND SELF :COMMAND NIL INPUT-HANDLE T "Undata" "UNDATA-CONNECTION")
          (CHAOS:CLOSE-CONN CONNECTION "Aborted")
          (CHAOS:REMOVE-CONN CONNECTION))))))

;;; Get a data connection for this unit.  Makes a new one if there is room in within the
;;; maximum number.  We are assumed to have recently been checked for validity.
fs:(DEFMETHOD (BIDIRECTIONAL-DATA-CONNECTION-MIXIN :GET-MOBY-CONNECTION) ()
  (LOCK-HOST-UNIT (SELF)
     (DO ((DATA-CONNS DATA-CONNECTIONS (CDR DATA-CONNS))
         (DATA-CONN))
        (NIL)
      (SETQ DATA-CONN (COND (DATA-CONNS (CAR DATA-CONNS))
  ;                         ((= (LENGTH DATA-CONNECTIONS)
  ;                             (MIN MAX-DATA-CONNECTIONS
  ;                                  (SEND ACCESS :ACCESS-SPECIFIC-DATA-CONN-LIMIT)))
  ;                          (RETURN NIL))   ;no limit!!
                            (T (SEND SELF :NEW-MOBY-CONNECTION))))
 ;for now, there can effectively be only one moby-connection to a given host, so return it if found.
      (COND ((AND (EQ (DATA-STREAM DATA-CONN :INPUT) 'moby)
                  (EQ (DATA-STREAM DATA-CONN :OUTPUT) 'moby))
  ;          (SETF (DATA-STREAM DATA-CONN DIRECTION) T) ;Mark as allocated
             (RETURN DATA-CONN))))))

;This function called from FS:OPEN-CHAOS.  Properties is full property list for file as received from
;file server.  String is the actual response string.   Format is:
;    1st line:  transaction code, standard properties
;    2nd line:  file truename
;    3rd line:  which-operations
;    4th line:  file-property-list
;    5th line:  <root datatype> <root-pointer-region-offset> <area-moby-options>
;       <map-moby-handle> <region base-namespace page> <region-size-in-qs> <region-free-pointer> <consability-granted>
; properties has TRUENAME (as pathname instance) on :truename key.

;see fs:open-chaos-moby-mapped
(defun open-chaos-moby-mapped-internal (host-unit properties string)
  (let* ((line1 (string-search-char #\return string))
         (line2 (string-search-char #\return string (1+ line1)))
         (line3 (string-search-char #\return string (1+ line2)))
         (line4 (string-search-char #\return string (1+ line3))))
    (let ((*read-base* 8) (*print-base* 8) (truename (get (locf properties) :truename))
          (map-added nil))
      (multiple-value-bind (root-datatype idx) (read-from-string string nil (1+ line4))
        (multiple-value-bind (root-pointer-region-offset idx) (read-from-string string nil idx)
          (multiple-value-bind (area-moby-options idx) (read-from-string string nil idx)
            (multiple-value-bind (map-moby-handle idx) (read-from-string string nil idx)
              (multiple-value-bind (region-base-namespace-page idx)
                  (read-from-string string nil idx)
                (multiple-value-bind (region-size-in-qs idx) (read-from-string string nil idx)
                  (multiple-value-bind (region-free-pointer idx) (read-from-string string nil idx)
                    (multiple-value-bind (consable? idx) (read-from-string string nil idx)
                      idx
                      (prog top () top
                            (let ((local-page (moby-page-to-local-page-correspondance
                                                region-base-namespace-page)))
                              (cond (local-page
                                     (return-from top
                                       (%make-pointer root-datatype
                                                      (%pointer-plus (lsh local-page 8)
                                                                     root-pointer-region-offset))))
                                    (t
                                     (let* ((msa (msa-association-for-map-moby-handle map-moby-handle))
                                            (section-map nil)
                                            (area-number nil)
                                            (host (send host-unit :host))
                                            (host-index (send host :host-index)))
                                       (cond ((null msa)
                                              (let ((name-for-area
                                                      (moby-area-name-for-pathname
                                                        truename
                                                        (send host :name-as-file-computer))))
   ;;should get AREA-MOBY-OPTIONS.
   ;;generate AREA-REGION-BITS from AREA-MOBY-OPTIONS,
   ;;AREA-REGION-SIZE not important since will have to ask
   ;;   before any regions are created anyway.
                                                (setq area-number
                                                      (moby-remote-setup-area
                                                        name-for-area
                                                        'remote-section
                        ;use truename instead of section. It servers only as an identifier.
                                                        truename
                                                        host))
                                                (setq msa (aref *area-to-msa* area-number))))
                                             (t
                                              (setq area-number (msa-area msa))))
                                       (if (null (msa-map msa))
                                           (moby-make-section-map       ;this stores map in MSA.
                                             working-storage-area
                                             map-moby-handle
                                             nil        ;remote section, doesnt matter
                                             area-number 10. area-moby-options))
                                       (setq section-map (msa-map msa))
                                                ;make region map and add to section map.
                                       (let ((region-map
                                               (moby-make-region-map
                                                 msa
                                                 working-storage-area
                                                 region-base-namespace-page
                                                 (ash region-size-in-qs -8)
                                                 )))
                                         (setf (rm-free-pointer region-map) region-free-pointer)
                                         (array-push-extend section-map region-map)
                                         ;if consable?, its by me.
                                         (allocate-region-to-moby-area
                                           area-number
                                           nil
                                           region-map
                                           (if consable? host-index)) ;consable?
                                         (setq map-added t)
                                         ;(break "open-chaos-moby-mapped")
                                         (go top))))))
                            )))))))))
    )))

;Called by file-server.  Read packets and respond as long as (car cell) contains fs:moby-server.
;  If that fails to be the case, return.
(defun fs:moby-server-toplevel (cell conn)
  (process-assign-lock-info)
  (do ((celloc (locf (car cell))))
      ((not (eq (car cell) 'fs:moby-server)))
    (let ((pkt (if (fs:server-window-write-check cell conn 'fs:moby-server)
                   (chaos:get-next-pkt conn)
                 (return nil))))
      (select (chaos:pkt-opcode pkt)
        (chaos:eof-op
         (chaos:return-pkt pkt)
         (setq pkt (if (fs:server-window-write-check cell conn 'fs:moby-server)
                       (chaos:get-next-pkt conn)
                     (return nil)))
         (or (= (chaos:pkt-opcode pkt) fs:%qfile-synchronous-mark-opcode)
             (ferror "Unrecognized Opcode in data server: ~O"
                     (chaos:pkt-opcode pkt)))
         (chaos:return-pkt pkt)
         (%store-conditional celloc 'fs:moby-server nil)
         (return nil))
        (fs:%qfile-synchronous-mark-opcode
         (chaos:return-pkt pkt)
         (%store-conditional celloc 'fs:moby-server nil)
         (return nil))
        (fs:%qfile-binary-opcode
  ;"process" pkt
         (moby-process-pkt-request conn pkt))
        (otherwise (ferror nil "Unknown pkt opcode: ~O" (chaos:pkt-opcode pkt))))))
  )

(defun moby-process-pkt-dispatch (conn pkt fctn)
  (let* ((seq (mpkt-seq pkt))
         (arg (mpkt-arg pkt))
         (ma-1 (moby-base-from-data-response pkt)))
    (funcall fctn conn seq arg ma-1)
    (chaos:return-pkt pkt)))

(defun moby-process-pkt-request-data-single-object-local (conn seq arg ma-1)
  (prog (local-address)
    l  (setq local-address (moby-to-local-correspondance ma-1))
       (cond ((null local-address)
              (moby-map-section-of-moby-pointer ma-1 nil)
              (go l)))
;    l1 (let ((ldt (%p-data-type local-address)))
;        (select ldt
;          (dtp-unreconciled
;           (moby-reconcile-object local-address)
;           (go l1))
;          ))
       (multiple-value-bind (leader total-size boxed-size)
           (moby-find-structure-info local-address)
         (moby-dereconcile-object-to-net conn seq arg
          leader total-size boxed-size %moby-pkt-response-data))
       ))

(defun moby-process-pkt-give-data (conn pkt)
  (let ((seq (mpkt-seq pkt)))
    (moby-reconcile-remote-object nil conn pkt)
    (let ((response-pkt (chaos:get-pkt)))
      (moby-send-pkt-response conn response-pkt seq 1)
      )))

(defun moby-process-pkt-writeout-area-local (conn seq arg ma-1)
  (prog (local-address)
    l  (setq local-address (moby-to-local-correspondance ma-1))
       (cond ((null local-address)
              (moby-map-section-of-moby-pointer ma-1 nil)
              (go l)))
    l1 (let ((ldt (%p-data-type local-address)))
         (select ldt
           (dtp-unreconciled
            (moby-reconcile-object local-address)
            (go l1))
           (dtp-array-header
            (let* ((section-map (%make-pointer dtp-array-pointer local-address))
                   (area-to-write (sm-locally-mapped-area section-map)))
              (cond ((not (fixp area-to-write))
                     (ferror nil "Bad request to write area"))
                    (t (moby-writeout-area area-to-write)
                       (let ((pkt (chaos:get-pkt)))
                         (moby-send-pkt-response conn pkt seq 0))))))
           (otherwise
            (ferror nil "Section map not found"))))
       ))

(defun moby-process-pkt-request-consing-region-local (conn seq arg ma-1)
  ;ma-1 is the map-moby-handle of section he wants to cons in.
  (prog top (local-address map moby-host host-index area msa region-map)
        (setq local-address (moby-to-local-correspondance ma-1))
        (setq moby-host (moby-host-of-conn conn)
              host-index (send moby-host :host-index))
        (cond ((null local-address)
               (ferror nil "Bad map-moby-handle")))
        (let ((local-dt
                (%p-ldb-offset %%q-data-type 0 local-address)))
          (cond ((not (or (= local-dt dtp-array-header)
                          (= local-dt dtp-unreconciled)))
                 (ferror nil "Map moby handle points to garbage"))))
        (setq map (%make-pointer dtp-array-pointer local-address)
              area (sm-locally-mapped-area map)
              msa (aref *area-to-msa* area))
        (cond ((null msa)
               (ferror nil "Section map data structure screwwed")))
        ;see if an existing section is unassigned as to consing.
        (dotimes (i (sm-fill-pointer map)
                    (setq region-map
                          (moby-add-region-to-map-and-allocate-namespace
                            map msa #o400 (msa-partition-header msa))))
          (setq region-map (aref map i))
          (cond ((and (null (rm-consing-host region-map))  ;found available region map, use it.
                      (>= (- (rm-size region-map)
                             (rm-free-pointer region-map))
                          si:page-size))
                 (return))))
        ;Actually cause local region to appear, with consable part checked out to conser.
        ;This is necessary because (1) if we write the area, we have to know
        ; to get stuff from him. (2) if have to know not to try to ref local dataspace for stuff.
        (moby-allocate-region-at-namespace area region-map host-index)  ;consable? to requestor
        (moby-send-pkt-response-region-mapping conn seq ma-1
         (rm-namespace-page-origin region-map) (rm-size region-map) (rm-free-pointer region-map) t)
  ))


(defun moby-process-pkt-request-region-mapping-local (conn seq arg ma-1)
  ;ma-1 is the moby address we need to know the region of.
  (prog top (local-address region moby-host host-index area msa region-map)
        (setq local-address (moby-to-local-correspondance ma-1))
        (setq moby-host (moby-host-of-conn conn)
              host-index (send moby-host :host-index))
        (cond ((null local-address)
               (ferror nil "region-mapping-for-unmapped region")))   ;*** this could happen ***
        (setq region (%region-number local-address)
              area (%area-number local-address))
        (setq region-map (aref *region-to-region-map* region)
              msa (aref *area-to-msa* area))
        (cond ((or (null msa) (null region-map))
               (ferror nil "Data structure screwwed")))

        ;Actually cause local region to appear, with consable part checked out to conser.
        ;This is necessary because (1) if we write the area, we have to know
        ; to get stuff from him. (2) if have to know not to try to ref local dataspace for stuff.
 ;      (moby-allocate-region-at-namespace area region-map host-index)  ;consable? to requestor
        (moby-send-pkt-response-region-mapping conn seq
         (msa-map-moby-handle msa)
         (rm-namespace-page-origin region-map) (rm-size region-map) (rm-free-pointer region-map) nil)
  ))


(defun moby-process-pkt-request-free-pointer-local (conn seq arg ma-1)
  (prog (local-page ans local-address region pkt)
        (cond ((null (setq local-page (moby-page-to-local-page-correspondance ma-1)))
               (setq ans -1)
               (go x)))
        (setq local-address (lsh local-page 8)
              region (%region-number local-address)
              ans (si:%region-free-pointer region))
    x   (setq pkt (chaos:get-pkt))
        (moby-send-pkt-response conn pkt seq ans)
  ))

(defun moby-process-pkt-request-clean-bits (conn command-pkt)
  (prog (seq-num sub-seq arg ma-1
         local-page local-free-pointer local-address region response-pkt
         remote-free-pointer
         region-first-page relative-page-base relative-page-limit)
        (setq seq-num (mpkt-seq command-pkt)
              arg (mpkt-arg command-pkt)
              sub-seq 0
              relative-page-base 0
              remote-free-pointer (dpb (mpkt-structure-handle-hi command-pkt)
                                       (byte 16. 16.)
                                       (mpkt-structure-handle-lo command-pkt))
              ma-1 (moby-base-from-data-response command-pkt)
              )

        (cond ((null (setq local-page (moby-page-to-local-page-correspondance ma-1)))
               (ferror nil "No local correspondance -- rmb")))
        (setq local-address (lsh local-page 8)
              region (%region-number local-address)
              region-first-page (lsh (si:%region-origin region) -8)
              local-free-pointer (si:%region-free-pointer region)
              relative-page-limit (min (lsh local-free-pointer -8)
                                       (lsh remote-free-pointer -8)))
   pkt-loop
        (let ((limit-this-pkt (min relative-page-limit
                                   (+ relative-page-base *max-clean-bits-in-pkt*))))
          (setq response-pkt (chaos:get-pkt))
          (do ((c 0 (1+ c)))
              ((= c limit-this-pkt)
               (moby-send-pkt-response-clean-bits
                 conn response-pkt seq-num (dpb (if (= relative-page-base relative-page-limit)
                                                1_7
                                              0)
                                            (byte 8 8)
                                            sub-seq)
                 c local-free-pointer))
            (moby-set-clean-bit-in-pkt
              (ldb si:%%virtual-page-clean
                   (aref #'system:virtual-page-data
                         (+ relative-page-base
                            region-first-page)))
              response-pkt
              c)
            (incf relative-page-base)))
        (cond ((not (= relative-page-base relative-page-limit))
               (incf sub-seq)
               (go pkt-loop)))
        (chaos:return-pkt command-pkt)
  ))

(defun moby-find-structure-info (local-address)
  ;works entirely by refernce to moby-bits.
  (declare (values leader total-size boxed-size))
  (prog (region region-origin region-relative-address free-pointer
         moby-bits-array moby-bits-from-array
         leader total-size boxed-size msa remote-host)
        (setq region (%region-number local-address)
              region-origin (si:%region-origin region)
              region-relative-address (%pointer-difference local-address region-origin)
              free-pointer (si:%region-free-pointer region)             ;***
              moby-bits-array (aref #'sys:region-moby-bits-array region)
              msa (aref *region-to-msa* region)
              remote-host (msa-primary-host-object msa))

  l-up  (setq moby-bits-from-array (aref moby-bits-array region-relative-address))
        (cond ((not (bit-test %moby-region-valid moby-bits-from-array))
               (cond ((null (moby-region-consable? region))
                      (ferror nil "moby-bits not valid -- up-remote"))
                     (t (moby-validate-moby-bits
                          (%pointer-plus region-origin region-relative-address)
                          nil)
                        (go l-up))))
              ((not (zerop (ldb %%%moby-starts-object moby-bits-from-array)))
               (go scan-down)))
        (cond ((>= (setq region-relative-address (1- region-relative-address))
                   0)
               (go l-up))
              (t (ferror nil "Starts object not set at region origin")))
  scan-down
        (setq leader (%pointer-plus region-origin region-relative-address)
              total-size 1 boxed-size 1)
  l-down(cond ((>= (setq region-relative-address (1+ region-relative-address))
                   free-pointer)
               (go exit)))
  l-dn0 (setq moby-bits-from-array (aref moby-bits-array region-relative-address))
        (cond ((not (bit-test %moby-region-valid moby-bits-from-array))
               (cond ((null (moby-region-consable? region))
                      ;assume this must be an object boundary since moby bits should become
                      ; valid object-wise...  ****
                      ;(ferror nil "all moby-bits not valid in object where some were.")
                      (go exit))
                     (t
                      (moby-validate-moby-bits
                          (%pointer-plus region-origin region-relative-address)
                          nil)
                      (go l-dn0))))
              ((not (zerop (ldb %%%moby-starts-object moby-bits-from-array)))
               (go exit)))
        (setq total-size (1+ total-size))
        (cond ((not (zerop (ldb %%%moby-boxed moby-bits-from-array)))
               (setq boxed-size (1+ boxed-size))))
        (go l-down)
  exit  (return (values leader total-size boxed-size))
        ))

;server packet protocol:
;  goals:  simple low level packet oriented protocol
;          suitable for "snapping" into microcode implementation of main cases.

;  requests:
;    request-data:  arg is moby-pointer.
;       structure-handle needed or not.  (assume always needed)
;       page(s) or lisp object.
;       send-along mode.  <just requested> <requested+easy> <whole-page>
;    data-response:
;       moby-page-number
;       structure-handle.
;        <skip>  <moby-data>
;      data-responses are page-oriented.  However, skip counts are provided so
;       only one Q can be sent, if desired.

;  lisp objects split across page boundaries...

;    request-mapping:  arg is moby-page-number.
;       single region.
;       complete section
;     response:  ea region: moby-base-page, length-in-qs.
;     response:  section:
;
;    request-application:  <nargs>,  supply n+1 moby-pointers.
;     response:  values

; other conventions regarding replies:
;   <end-of-response> signal.

;formatting of actual packet:  (16 bit data-words)
;  first-data-word.     <seq. number,, command>
;  second-data-word.   short arg.
;   q-count
;   subsequence count
;     last flag.
;   structure-handle
;  base-moby-address
;  following are moby quantities, 4 of these short words per.

;even numbers are commands
; odd are associated replies.

;400 bit of command says continuation packets also apply to this command (or response).

;consability ...
;  each region-map can be consable in a maximum of one machine.
;    if the region is "consed-out" there may be no benefit to making it consable in any.
;
;page-structure-handles

(defun fs:moby-mapped-open-response-5th-line (conn-stream root)
  (let* ((region (%region-number root))
         (msa (aref *region-to-msa* region)))
    (format conn-stream "~%~O ~O ~O ~O ~O ~O ~O ~O"
            (%data-type root)
            (%pointer-difference root
                                 (si:%region-origin (si:%region-number root)))
            (sm-area-moby-options (msa-map msa))     ;area-moby-options
            (msa-map-moby-handle msa)
            (aref #'sys:region-namespace-origin region)
            (si:%region-length region)
    ;region-free-pointer of region is not maximally winning thing to send.
    ; doesnt matter unless consablility being granted, then get it from region-map
            (rm-free-pointer (aref *region-to-region-map* region))
            nil         ;no consability-granted  **for now**
            )))

(defun moby-get-conn-for-msa (msa)
  (let ((host (msa-primary-host-object msa)))
    (cond ((null host)
           (ferror nil "Attempt to get conn for local MSA"))
          (t (moby-get-conn-for-host host)))))

(defun moby-get-conn-for-host-index (host-index)
  (moby-get-conn-for-host (aref *moby-host-index* host-index)))

(defun moby-get-conn-for-host (host)
  (car (send (send host :get-access) :get-moby-connection)))

(defun moby-conn-echo-test (conn n)
  (let ((pkt (chaos:get-pkt)))
    (setf (chaos:pkt-first-data-word pkt) n)
    (setf (chaos:pkt-nbytes pkt) 2)
    (chaos:send-pkt conn pkt fs:%qfile-binary-opcode)
    (let ((in-pkt (chaos:get-next-pkt conn)))
      (prog1 (chaos:pkt-first-data-word pkt)
             (chaos:return-pkt in-pkt)))))
