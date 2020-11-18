;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Cold-Load:T; Readtable:ZL; Base:10 -*-

;;; Cold load side of a simple bi-directional stream called COLD-BI-STREAM.
;;;

(DEFVAR *COLD-SHARED-MEMORY-SIZE*)
(DEFVAR *COLD-SHARED-MEMORY-8*)
(DEFVAR *COLD-SHARED-MEMORY-16*)

(DEFUN %COLD-SYS-CONF (INDEX)
  (DPB (%P-LDB (BYTE 16 16) (+ (%LAMBDA-SYS-CONF-VIRTUAL-ADR) INDEX))
       (BYTE 16 16)
       (%P-LDB (BYTE 16 0) (+ (%LAMBDA-SYS-CONF-VIRTUAL-ADR) INDEX))))

(DEFUN COLD-SDU-PHYS-TO-VIRTUAL (SDU-PHYS)
  (%lambda-sys-conf-phys-to-virtual
    (if (= (%lambda-sdu-quad-slot) #xff)
        sdu-phys
      (logxor #x10000000 sdu-phys))))

(DEFUN SETUP-COLD-SHARED-MEMORY (&OPTIONAL RESET)
  (WHEN (OR RESET
            (NOT (BOUNDP '*COLD-SHARED-MEMORY-SIZE*))
            (NOT *COLD-SHARED-MEMORY-SIZE*))
    (LET ((SIZE (%COLD-SYS-CONF %SYSTEM-CONFIGURATION-GLOBAL-SHARED-SIZE))
          (START (COLD-SDU-PHYS-TO-VIRTUAL
                   (%COLD-SYS-CONF %SYSTEM-CONFIGURATION-GLOBAL-SHARED-BASE))))
      (SETQ *COLD-SHARED-MEMORY-SIZE* SIZE)
      (setq *COLD-shared-memory-8*
            (make-array SIZE
                        :type :art-8b
                        :displaced-to START))
      (setq *COLD-shared-memory-16*
            (make-array (// SIZE 2)
                        :type :art-16b
                        :displaced-to START)))))


(DEFVAR *COLD-BI-STREAM-INPUT-SECTION* 0)
(DEFVAR *COLD-BI-STREAM-OUTPUT-SECTION* 2048)

;; FORMAT OF A SECTION IS [OWNER][TYPE][OFFSET][LIMIT]{DATA....}
;; OWNER = 0, owned by COLD side, 1 = owned by regular side.
;; TYPE = 0: CHARACTER DATA.
;; TYPE = 1: 16-BIT BINARY DATA.
;; OFFSET is index into data section of first available datum.
;; LIMIT is index into data section of END of data.
;;  When OFFSET=LIMIT then there is no data.


(DEFUN COLD-BI-STREAM-INPUT-WAIT ()
  (LET ((A *COLD-SHARED-MEMORY-16*)
        (B *COLD-BI-STREAM-INPUT-SECTION*))
    (COND ((ZEROP (AREF A B)))
          ('ELSE
           (PROCESS-WAIT "shared input"
                         #'(LAMBDA (A B)
                             (ZEROP (AREF A B)))
                         A
                         B)))))


(DEFUN COLD-BI-STREAM-OUTPUT-WAIT ()
  (LET ((A *COLD-SHARED-MEMORY-16*)
        (B *COLD-BI-STREAM-OUTPUT-SECTION*))
    (COND ((ZEROP (AREF A B)))
          ('ELSE
           (PROCESS-WAIT "shared output"
                         #'(LAMBDA (A B)
                             (ZEROP (AREF A B)))
                         A
                         B)))))

(DEFPROP COLD-BI-STREAM T IO-STREAM-P)

(DEFVAR *COLD-BI-STREAM-TYIPEEK-P* NIL)
(DEFVAR *COLD-BI-STREAM-TYIPEEK-CHAR* NIL)

(DEFUN COLD-BI-STREAM (OP &OPTIONAL ARG1 &REST ARGS)
  (SELECTQ-WITH-WHICH-OPERATIONS OP
    (:LISTEN
      (OR *COLD-BI-STREAM-TYIPEEK-P*
          (ZEROP (AREF *COLD-SHARED-MEMORY-16* *COLD-BI-STREAM-INPUT-SECTION*))))
    (:TYI
      (COND (*COLD-BI-STREAM-TYIPEEK-P*
             (PROG1 *COLD-BI-STREAM-TYIPEEK-CHAR*
                    (SETQ *COLD-BI-STREAM-TYIPEEK-P* NIL)))
            ('ELSE
             (COLD-BI-STREAM-TYI))))
    (:UNTYI
      (SETQ *COLD-BI-STREAM-TYIPEEK-P* T)
      (SETQ *COLD-BI-STREAM-TYIPEEK-CHAR* ARG1))
    (:TYIPEEK
      (COND (*COLD-BI-STREAM-TYIPEEK-P*
             *COLD-BI-STREAM-TYIPEEK-CHAR*)
            ('ELSE
             (SETQ *COLD-BI-STREAM-TYIPEEK-P* T)
             (SETQ *COLD-BI-STREAM-TYIPEEK-CHAR* (COLD-BI-STREAM-TYI)))))
    (:READ-INPUT-BUFFER
      (COLD-BI-STREAM-READ-INPUT-BUFFER))
    (:GET-INPUT-BUFFER
      (COLD-BI-STREAM-GET-INPUT-BUFFER))
    (:ADVANCE-INPUT-BUFFER
      (COLD-BI-STREAM-ADVANCE-INPUT-BUFFER))
    (:TYO
      (COLD-BI-STREAM-TYO ARG1))
    (:EOF
      (COLD-BI-STREAM-EOF))
    (:STRING-OUT
      (COLD-BI-STREAM-STRING-OUT ARG1 (CAR ARGS) (CADR ARGS)))
    (:BYTE-SIZE
      (cold-bi-stream-input-wait)       ;shared byte is set by :send-output-buffer method
                ;on server side, so is not valid until data is in buffer.
      (ECASE (AREF *COLD-SHARED-MEMORY-16* (+ *COLD-BI-STREAM-INPUT-SECTION* 1))
        (0 8)
        (1 16)))
    (:CHARACTERS
      (cold-bi-stream-input-wait)       ;shared byte is set by :send-output-buffer method
                ;on server side, so is not valid until data is in buffer.
      (ECASE (AREF *COLD-SHARED-MEMORY-16* (+ *COLD-BI-STREAM-INPUT-SECTION* 1))
        (0 T)
        (1 NIL)))
    (:DIRECTION :BIDIRECTIONAL)
    (:PATHNAME
      (IF (BOUNDP 'MINI-FASLOAD-FILENAME) (SYMEVAL 'MINI-FASLOAD-FILENAME)))
    (:GENERIC-PATHNAME 'MINI-PLIST-RECEIVER)
    (:INFO
      (IF (BOUNDP 'MINI-FILE-ID) (SYMEVAL 'MINI-FILE-ID)))
    (:CLOSE
      (SETF (AREF *COLD-SHARED-MEMORY-16* *COLD-BI-STREAM-INPUT-SECTION*) 1))
    (T
      (stream-default-handler 'cold-bi-stream op arg1 args))))

(DEFUN COLD-BI-STREAM-TYI ()
  (COLD-BI-STREAM-INPUT-WAIT)
  (LET ((OFFSET (AREF *COLD-SHARED-MEMORY-16* (+ *COLD-BI-STREAM-INPUT-SECTION* 2)))
        (LIMIT (AREF *COLD-SHARED-MEMORY-16* (+ *COLD-BI-STREAM-INPUT-SECTION* 3))))
    (COND ((= OFFSET LIMIT)
           (SETF (AREF *COLD-SHARED-MEMORY-16* *COLD-BI-STREAM-INPUT-SECTION*) 1)
           ())
          ('ELSE
           (PROG1 (ECASE (AREF *COLD-SHARED-MEMORY-16* (+ *COLD-BI-STREAM-INPUT-SECTION* 1))
                    (0
                     (AREF *COLD-SHARED-MEMORY-8* (+ (* *COLD-BI-STREAM-INPUT-SECTION* 2)
                                                     OFFSET 8)))
                    (1
                     (AREF *COLD-SHARED-MEMORY-16* (+ *COLD-BI-STREAM-INPUT-SECTION* OFFSET 4))))
                  (INCF OFFSET)
                  (SETF (AREF *COLD-SHARED-MEMORY-16* (+ *COLD-BI-STREAM-INPUT-SECTION* 2))
                        OFFSET)
                  (WHEN (= OFFSET LIMIT)
                    (SETF (AREF *COLD-SHARED-MEMORY-16* *COLD-BI-STREAM-INPUT-SECTION*) 1)))))))



(DEFUN COLD-BI-STREAM-READ-INPUT-BUFFER ()
  (COLD-BI-STREAM-INPUT-WAIT)
  (LET ((OFFSET (AREF *COLD-SHARED-MEMORY-16* (+ *COLD-BI-STREAM-INPUT-SECTION* 2)))
        (LIMIT (AREF *COLD-SHARED-MEMORY-16* (+ *COLD-BI-STREAM-INPUT-SECTION* 3))))
    (COND ((= OFFSET LIMIT)
           (SETF (AREF *COLD-SHARED-MEMORY-16* *COLD-BI-STREAM-INPUT-SECTION*) 1)
           ())
          ('ELSE
           (ECASE (AREF *COLD-SHARED-MEMORY-16* (+ *COLD-BI-STREAM-INPUT-SECTION* 1))
             (0
              (VALUES *COLD-SHARED-MEMORY-8*
                      (+ (* 2 *COLD-BI-STREAM-INPUT-SECTION*) OFFSET 8)
                      (+ (* 2 *COLD-BI-STREAM-INPUT-SECTION*) LIMIT 8)))
             (1
              (VALUES *COLD-SHARED-MEMORY-16*
                      (+ *COLD-BI-STREAM-INPUT-SECTION* OFFSET 4)
                      (+ *COLD-BI-STREAM-INPUT-SECTION* LIMIT 4))))))))


(DEFUN COLD-BI-STREAM-GET-INPUT-BUFFER ()
  (COLD-BI-STREAM-INPUT-WAIT)
  (LET ((OFFSET (AREF *COLD-SHARED-MEMORY-16* (+ *COLD-BI-STREAM-INPUT-SECTION* 2)))
        (LIMIT (AREF *COLD-SHARED-MEMORY-16* (+ *COLD-BI-STREAM-INPUT-SECTION* 3))))
    (COND ((= OFFSET LIMIT)
           (SETF (AREF *COLD-SHARED-MEMORY-16* *COLD-BI-STREAM-INPUT-SECTION*) 1)
           ())
          ('ELSE
           (ECASE (AREF *COLD-SHARED-MEMORY-16* (+ *COLD-BI-STREAM-INPUT-SECTION* 1))
             (0
              (VALUES *COLD-SHARED-MEMORY-8*
                      (+ (* *COLD-BI-STREAM-INPUT-SECTION* 2) OFFSET 8)
                      (- LIMIT OFFSET)))

             (1
              (VALUES *COLD-SHARED-MEMORY-16*
                      (+ *COLD-BI-STREAM-INPUT-SECTION* OFFSET 4)
                      (- LIMIT OFFSET))))))))

(DEFUN COLD-BI-STREAM-ADVANCE-INPUT-BUFFER ()
  (SETF (AREF *COLD-SHARED-MEMORY-16* *COLD-BI-STREAM-INPUT-SECTION*) 1))

(DEFUN COLD-BI-STREAM-TYO (CHAR)
  (COLD-BI-STREAM-OUTPUT-WAIT)
  (SETF (AREF *COLD-SHARED-MEMORY-16* (+ *COLD-BI-STREAM-OUTPUT-SECTION* 2)) 0)
  (SETF (AREF *COLD-SHARED-MEMORY-16* (+ *COLD-BI-STREAM-OUTPUT-SECTION* 3)) 1)
  (SETF (AREF *COLD-SHARED-MEMORY-8* (+ (* 2 *COLD-BI-STREAM-OUTPUT-SECTION*) 8)) CHAR)
  (SETF (AREF *COLD-SHARED-MEMORY-16* *COLD-BI-STREAM-OUTPUT-SECTION*) 1))

(DEFUN COLD-BI-STREAM-EOF ()
  (COLD-BI-STREAM-OUTPUT-WAIT)
  (SETF (AREF *COLD-SHARED-MEMORY-16* (+ *COLD-BI-STREAM-OUTPUT-SECTION* 2)) 0)
  (SETF (AREF *COLD-SHARED-MEMORY-16* (+ *COLD-BI-STREAM-OUTPUT-SECTION* 3)) 0)
  (SETF (AREF *COLD-SHARED-MEMORY-16* *COLD-BI-STREAM-OUTPUT-SECTION*) 1))

(DEFUN COLD-BI-STREAM-STRING-OUT (STRING START END)
  (OR START (SETQ START 0))
  (OR END (SETQ END (LENGTH STRING)))
  (UNLESS (= START END)
    (COLD-BI-STREAM-OUTPUT-WAIT)
    (SETF (AREF *COLD-SHARED-MEMORY-16* (+ *COLD-BI-STREAM-OUTPUT-SECTION* 2)) 0)
    (SETF (AREF *COLD-SHARED-MEMORY-16* (+ *COLD-BI-STREAM-OUTPUT-SECTION* 3)) (- END START))
    (COPY-ARRAY-PORTION STRING START END
                        *COLD-SHARED-MEMORY-8*
                        (+ (* 2 *COLD-BI-STREAM-OUTPUT-SECTION*) 8)
                        (+ (* 2 *COLD-BI-STREAM-OUTPUT-SECTION*) 8 (- END START)))
    (SETF (AREF *COLD-SHARED-MEMORY-16* *COLD-BI-STREAM-OUTPUT-SECTION*) 1)))


(DEFUN COLD-BI-STREAM-DESCRIBE ()
  (DOLIST (S '(*COLD-BI-STREAM-INPUT-SECTION* *COLD-BI-STREAM-OUTPUT-SECTION*))
    (FORMAT T "~&~S~%" S)
    (LET ((OWNER (AREF *GLOBAL-SHARED-MEMORY-16* (SYMEVAL S)))
          (TYPE (AREF *GLOBAL-SHARED-MEMORY-16* (+ 1 (SYMEVAL S))))
          (OFFSET (AREF *GLOBAL-SHARED-MEMORY-16* (+ 2 (SYMEVAL S))))
          (LIMIT (AREF *GLOBAL-SHARED-MEMORY-16* (+ 3 (SYMEVAL S)))))
      (FORMAT t "  OWNER  = ~S~%  TYPE   = ~S~%  OFFSET = ~S~%  LIMIT  = ~D~%"
              (or (cadr (assq owner '((0 "cold side") (1 "server side")))) owner)
              (or (cadr (assq type '((0 "character data") (1 "binary data")))) type)
              offset limit))))
