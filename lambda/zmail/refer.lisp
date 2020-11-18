;;; -*-Mode:LISP; Package:ZWEI; Base:10 ; Readtable:ZL -*-

;;; Keep track of inter-message references.

;;; Each mail-file-buffer has a hash table that is used to hash
;;; each reference (an object that describes what is referenced)
;;; against an example of this data structure.
;;; Only messages in the mail file are mentioned.

(DEFSTRUCT (REFERENCE-ENTRY :LIST)
  REFERENCE-ENTRY-REFERENCED-MSGS       ;List of msgs this ref refers TO.
  REFERENCE-ENTRY-REFERENCING-MSGS)     ;List of msgs that refer using this ref.

(DEFUN MAKE-REFERENCE-HASH-TABLE ()
  (MAKE-HASH-TABLE :COMPARE-FUNCTION 'REFERENCE-COMPARE
                   :HASH-FUNCTION 'REFERENCE-HASH
                   :area *zmail-msg-area*))

(defun reference-identifier (reference)
  (let ((loc (locf reference)))
    (or (get loc :message-id)
        (truncate (or (get loc :date) 0) 60.))))

(defun reference-hash (reference)
  (let ((ref-id (reference-identifier reference)))
    (values (sxhash ref-id) (si:%pointer-volatility ref-id))))

(defun reference-compare (ref1 ref2)
  (equal (reference-identifier ref1)
         (reference-identifier ref2)))

(DEFUN HASH-RECORD-MSG-REFERENCES (MSG HT)
  "Record MSG in HT, a hash table of inter-message references.
Updates the data on MSG if MSG has changed, adding and removing entries as appropriate."
  (LET* ((STATUS (ASSURE-MSG-PARSED MSG))
         (OREFS (GET STATUS 'RECORDED-REFERENCES))
         (OIDS (GET STATUS 'RECORDED-MSG-IDS))
         (NREFS (MSG-REFERENCES MSG))
         (NIDS (MSG-IN-REFERENCES MSG)))
    (UNLESS (AND (EQUAL OIDS (OR NIDS T)) (EQUAL OREFS (OR NREFS T)))
      (IF OREFS (HASH-UNRECORD-MSG MSG HT))
      (DOLIST (ID NIDS)
        (LET ((HASHENTRY (GETHASH ID HT)))
          (UNLESS HASHENTRY
            (SETQ HASHENTRY (LIST NIL NIL))
            (PUTHASH ID HASHENTRY HT))
          (UNLESS (MEMQ MSG (REFERENCE-ENTRY-REFERENCED-MSGS HASHENTRY))
            (PUSH MSG (REFERENCE-ENTRY-REFERENCED-MSGS HASHENTRY)))))
      (DOLIST (REF NREFS)
        (LET ((HASHENTRY (GETHASH REF HT)))
          (UNLESS HASHENTRY
            (SETQ HASHENTRY (LIST NIL NIL))
            (PUTHASH REF HASHENTRY HT))
          (UNLESS (MEMQ MSG (REFERENCE-ENTRY-REFERENCING-MSGS HASHENTRY))
            (PUSH MSG (REFERENCE-ENTRY-REFERENCING-MSGS HASHENTRY)))))
      (PUTPROP STATUS (OR NIDS T) 'RECORDED-MSG-IDS)
      (PUTPROP STATUS (OR NREFS T) 'RECORDED-REFERENCES))))

(DEFUN HASH-UNRECORD-MSG (MSG HT)
  "Remove MSG from HT, a hash table of inter-message references.
You can use this when about to kill MSG, or if MSG must be re-recorded
because its contents have changed."
  (LET* ((STATUS (LOCF (MSG-STATUS MSG)))
         (OREFS (GET STATUS 'RECORDED-REFERENCES))
         (OIDS (GET STATUS 'RECORDED-MSG-IDS)))
    (UNLESS (EQ OIDS T)
      (DOLIST (ID OIDS)
        (LET ((HASHENTRY (GETHASH ID HT)))
          (WHEN HASHENTRY
            (SETF (REFERENCE-ENTRY-REFERENCED-MSGS HASHENTRY)
                  (DELQ MSG (REFERENCE-ENTRY-REFERENCED-MSGS HASHENTRY)))))))
    (UNLESS (EQ OREFS T)
      (DOLIST (REF OREFS)
        (LET ((HASHENTRY (GETHASH REF HT)))
          (WHEN HASHENTRY
            (SETF (REFERENCE-ENTRY-REFERENCING-MSGS HASHENTRY)
                  (DELQ MSG (REFERENCE-ENTRY-REFERENCING-MSGS HASHENTRY)))))))
    (PUTPROP STATUS NIL 'RECORDED-MSG-IDS)
    (PUTPROP STATUS NIL 'RECORDED-MSG-REFERENCES)))

;;; External interface.

;Use :UPDATE-REFERENCES to cause the hash table to be made up-to-date.
;Use :UNCACHE-REFERENCES to say that you have made changes to many existing messages
;so all should be reparsed and rerecorded in the hash table.
;Use :UNCACHE-MSG to say you have changed one message.
;Use :REFERENCED-MSGS to get the messages referred to by a list of references.
;Use :REFERENCING-MSGS to get the messages referring to any of a list of msg-ids.
;(A msg-id is the same thing as a reference).

;These operations work on all buffers, but use a hash table only for mail-file-buffers.

(DEFUN UNCACHE-MSG-REFERENCES (MSG)
  (SEND (MSG-MAIL-FILE-BUFFER MSG) :UNCACHE-MSG MSG))

;; Enter in the hash table any messages that are not in it.
;; Assumes that HASH-UNRECORD-MSG has been used to unhash
;; any messages that have changed; also, any moved-in messages will not be in it.
(DEFMETHOD (MAIL-FILE-BUFFER :UPDATE-REFERENCES) ()
  (WITHOUT-INTERRUPTS
    (UNLESS REFERENCE-HASH-TABLE
      (SETQ REFERENCE-HASH-TABLE (MAKE-REFERENCE-HASH-TABLE))))
  (LOOP FOR MSG BEING THE MSGS IN SELF
        DO
        (UNLESS (GET (LOCF (MSG-STATUS MSG)) 'RECORDED-REFERENCES)
          (HASH-RECORD-MSG-REFERENCES MSG REFERENCE-HASH-TABLE))))

(DEFMETHOD (MAIL-FILE-BUFFER :UNCACHE-REFERENCES) ()
  (DOMSGS (MSG SELF)
    (LET ((-STATUS- (ASSURE-MSG-PARSED MSG)))
      (REMPROP -STATUS- 'RECORDED-REFERENCES)
      (REMPROP -STATUS- 'RECORDED-MSG-IDS)))
  (CLRHASH REFERENCE-HASH-TABLE))

(DEFMETHOD (MAIL-FILE-BUFFER :UNCACHE-MSG) (MSG)
  (HASH-UNRECORD-MSG MSG REFERENCE-HASH-TABLE))

(DEFMETHOD (MAIL-FILE-BUFFER :REFERENCED-MSGS) (REFERENCES)
  (SEND SELF :UPDATE-REFERENCES)
  (MAPCAN #'(LAMBDA (REFERENCE)
              (SUBSET 'MSG-REFERENCE-EQUAL
                      (REFERENCE-ENTRY-REFERENCED-MSGS
                        (GETHASH REFERENCE REFERENCE-HASH-TABLE))
                      (CIRCULAR-LIST REFERENCE)))
          REFERENCES))

(DEFMETHOD (MAIL-FILE-BUFFER :REFERENCING-MSGS) (MSG-IDS)
  (SEND SELF :UPDATE-REFERENCES)
  (MAPCAN #'(LAMBDA (ID)
              (SUBSET #'(LAMBDA (MSG)
                          (LOOP FOR REF IN (MSG-REFERENCES MSG)
                            THEREIS (REFERENCE-ALL-MATCH REF ID)))
                      (REFERENCE-ENTRY-REFERENCING-MSGS
                        (GETHASH ID REFERENCE-HASH-TABLE))))
          MSG-IDS))

(DEFMETHOD (ZMAIL-BUFFER :UNCACHE-REFERENCES) () NIL)

(DEFMETHOD (ZMAIL-BUFFER :UNCACHE-MSG) (IGNORE) NIL)

(DEFMETHOD (ZMAIL-BUFFER :UPDATE-REFERENCES) () NIL)

(DEFMETHOD (ZMAIL-BUFFER :REFERENCED-MSGS) (REFERENCES)
  (MAPCAN #'(LAMBDA (REFERENCE)
              (LOOP FOR MSG BEING THE MSGS IN SELF
                    WHEN (MSG-REFERENCE-EQUAL MSG REFERENCE)
                    COLLECT MSG))
          REFERENCES))

(DEFMETHOD (ZMAIL-BUFFER :REFERENCING-MSGS) (MSG-IDS)
  (MAPCAN #'(LAMBDA (ID)
              (LOOP FOR MSG BEING THE MSGS IN SELF
                    WHEN
                    (LOOP FOR REF IN (MSG-REFERENCES MSG)
                          THEREIS (REFERENCE-COMPARE REF ID))
                    COLLECT MSG))
          MSG-IDS))

;; Interface between references and messages.

(DEFUN MSG-REFERENCE-EQUAL (MSG REF)
  "T if MSG is one of the messages referred to by references REF."
  (LOOP FOR (IND PROP) ON REF BY 'CDDR
        WITH STATUS = (ASSURE-MSG-PARSED MSG)
        ALWAYS (REFERENCE-EQUAL PROP
                                (IF (EQ IND :FROM)
                                    (CAR (GET STATUS IND))
                                  (GET STATUS IND))
                                IND)))

(DEFUN REFERENCE-ALL-MATCH (REF MSG-ID)
  (LOOP FOR (IND PROP) ON REF BY 'CDDR
        WITH STATUS = (LOCF MSG-ID)
        ALWAYS (REFERENCE-EQUAL PROP (GET STATUS IND) IND)))

(DEFUN REFERENCE-EQUAL (REF-PROP MSG-PROP IND)
  (AND MSG-PROP
       (CASE IND
         (:DATE
          ;; Don't compare the seconds.
          (= (TRUNCATE (IF (CONSP REF-PROP) (CAR REF-PROP) REF-PROP) 60.)
             (TRUNCATE (IF (CONSP MSG-PROP) (CAR MSG-PROP) MSG-PROP) 60.)))
         (:FROM (LOOP FOR (IND PROP) ON REF-PROP BY 'CDDR
                      ALWAYS
                      (OR (EQ IND :HOST)
                          (EQUAL PROP (GET (LOCF MSG-PROP) IND)))))
         (OTHERWISE (EQUAL REF-PROP MSG-PROP)))))

(DEFUN MSG-IN-REFERENCES (MSG &AUX STATUS)
  "Return a list of references whereby this message may be referenced."
  (SETQ STATUS (ASSURE-MSG-PARSED MSG))
  (OR (GET STATUS 'IN-REFERENCES)
      (LET* ((MSG-ID (GET STATUS :MESSAGE-ID))
             (DATE (GET STATUS :DATE))
             (FROM (SOME-PLIST (CAR (GET STATUS :FROM)) '(:NAME)))
             (IN-REFERENCES
               (IF MSG-ID
                   (IF (AND DATE FROM)
                       `((:MESSAGE-ID ,MSG-ID)
                         (:FROM ,FROM :DATE ,DATE))
                     `((:MESSAGE-ID ,MSG-ID)))
                 (IF (AND DATE FROM)
                     `((:FROM ,FROM :DATE ,DATE))
                   NIL))))
        (PUTPROP STATUS IN-REFERENCES 'IN-REFERENCES))))

(DEFUN MSG-REFERENCES (MSG &AUX STATUS TEM)
  "Return a list of the references of MSG.
Each element is a reference that may match another MSG (the one referred to).
Use MSG-REFERENCE-EQUAL to match a message against a reference."
  (SETQ STATUS (ASSURE-MSG-PARSED MSG))
  (IF (SETQ TEM (GETL STATUS '(REFERENCES)))
      (CADR TEM)
      (SETQ TEM (GET-MSG-REFERENCES MSG STATUS))
      (PUTPROP STATUS TEM 'REFERENCES)
      TEM))

(DEFUN GET-MSG-REFERENCES (MSG STATUS)
  ;; If there is an IN-REPLY-TO field, use what is says, else get from text
  (IF (GETL STATUS *REFERENCE-TYPE-HEADERS*)
      (LOOP FOR IND IN *REFERENCE-TYPE-HEADERS*
            APPEND (GET STATUS IND))
      (GET-MSG-TEXT-REFERENCES MSG)))

(DEFUN SOME-MSG-FITS-REFERENCE (MSGS REFERENCE)
  (DOLIST (MSG MSGS)
    (IF (MSG-REFERENCE-EQUAL MSG REFERENCE) (RETURN T))))

(DEFUN TRACE-REFERENCES-RECURSIVELY (MSG BUFFER FORWARD-FLAG BACKWARD-FLAG)
"First value is a list of all references traced through.
Second value is a list of all messages thus reached."
  (UNLESS BUFFER
    (IF *NUMERIC-ARG-P*
        (MULTIPLE-VALUE-BIND (MAP-FUNCTION MAP-ARG)
            (GET-UNIVERSE-FUNCTION '(:MOUSE) "Universe to search")
          (SETQ BUFFER (EXPAND-UNIVERSE-FUNCTION MAP-FUNCTION MAP-ARG)))
      (SETQ BUFFER *ZMAIL-BUFFER*)))
  (LET* ((REFERENCES (APPEND (IF FORWARD-FLAG (MSG-REFERENCES MSG))
                             (COPYLIST (IF BACKWARD-FLAG (MSG-IN-REFERENCES MSG)))))
         (REFS-LIST-TAIL (LAST REFERENCES))
         (MSGS (LIST MSG))
         (MSGS-TAIL MSGS)
         (REFS-SCANNING-POINTER REFERENCES))
    (DO ()
        ((NULL REFS-SCANNING-POINTER))
      (LET ((REF (CAR REFS-SCANNING-POINTER)))
        (WHEN FORWARD-FLAG
          (LET ((MSGS-REFD (SEND BUFFER :REFERENCED-MSGS (LIST REF))))
            (DOLIST (M MSGS-REFD)
              (UNLESS (MEMQ M MSGS)
                (SETF (VALUES REFS-LIST-TAIL MSGS-TAIL)
                      (TRACE-REFERENCES-RECURSIVELY-ADD-MSG M REFS-LIST-TAIL MSGS-TAIL
                                                            FORWARD-FLAG BACKWARD-FLAG))))))
        (WHEN BACKWARD-FLAG
          (LET ((MSGS-REFFING (SEND BUFFER :REFERENCING-MSGS (LIST REF))))
            (DOLIST (M MSGS-REFFING)
              (UNLESS (MEMQ M MSGS)
                (SETF (VALUES REFS-LIST-TAIL MSGS-TAIL)
                      (TRACE-REFERENCES-RECURSIVELY-ADD-MSG M REFS-LIST-TAIL MSGS-TAIL
                                                            FORWARD-FLAG BACKWARD-FLAG)))))))
      (POP REFS-SCANNING-POINTER))
    (LOOP FOR REF IN REFERENCES
          WITH FLAG = NIL
          UNLESS (SOME-MSG-FITS-REFERENCE MSGS REF)
          DO (UNLESS FLAG (FORMAT QUERY-IO "~&Some referents cannot be found.")
                     (SETQ FLAG T))
;            (TERPRI)
;            (PRINT-REFERENCE *STANDARD-OUTPUT* REF NIL)
             )
    (VALUES MSGS REFERENCES)))

(DEFUN EXPAND-UNIVERSE-FUNCTION (MAP-FUNCTION MAP-ARG)
  (CASE MAP-FUNCTION
    (MAP-OVER-SINGLE-ZMAIL-BUFFER MAP-ARG)
    ;; The next two are not really implemented right, but who cares?
    (MAP-OVER-REST-OF-ZMAIL-BUFFER MAP-ARG)
    (MAP-OVER-BEGINNING-OF-ZMAIL-BUFFER MAP-ARG)
    (MAP-OVER-DEFINED-UNIVERSE (EXPAND-UNIVERSE MAP-ARG))
    ((MAP-OVER-ALL-ZMAIL-BUFFERS MAP-OVER-LOADED-ZMAIL-BUFFERS)
     #'(LAMBDA (OP &OPTIONAL ARG)
         (CASE OP
           ((:UNCACHE-REFERENCES :UPDATE-REFERENCES)
            (DOLIST (B *ZMAIL-BUFFER-LIST*)
              (SEND B OP)))
           (:UNCACHE-MSG
            (DOLIST (B *ZMAIL-BUFFER-LIST*)
              (SEND B OP ARG)))
           ((:REFERENCED-MSGS :REFERENCING-MSGS)
            (LOOP FOR B IN *ZMAIL-BUFFER-LIST*
                  NCONC (SEND B OP ARG))))))))

(DEFUN TRACE-REFERENCES-RECURSIVELY-ADD-MSG (MSG REFS-TAIL MSG-TAIL
                                             FORWARD-FLAG BACKWARD-FLAG)
  (SETF (CDR MSG-TAIL) (NCONS MSG))
  (IF FORWARD-FLAG
      (SETF (CDR REFS-TAIL)
            (COPYLIST* (MSG-REFERENCES MSG))))
  (IF BACKWARD-FLAG
      (SETF (CDR REFS-TAIL)
            (COPYLIST* (MSG-IN-REFERENCES MSG))))
  (VALUES (IF (CDR REFS-TAIL) (LAST REFS-TAIL) REFS-TAIL)
          (LAST MSG-TAIL)))

(DEFUN MAKE-ZMAIL-BUFFER-OF-REFERENCES (MSG FORWARD-FLAG BACKWARD-FLAG &AUX MF)
  (SETQ MF (GET-RECYCLED-TEMP-ZMAIL-BUFFER "<References>"))
  (LET ((MSGS (TRACE-REFERENCES-RECURSIVELY
                MSG NIL FORWARD-FLAG BACKWARD-FLAG))
        (ARRAY (ZMAIL-BUFFER-ARRAY MF)))
    (DOLIST (M (SORT MSGS 'MSG-DATE-SORT-LESSP))
      (VECTOR-PUSH-EXTEND M ARRAY)))
  MF)
