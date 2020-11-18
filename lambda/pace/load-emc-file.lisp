;;; -*- Mode:Zetalisp; Package:SYSTEM-INTERNALS; Base:8 -*-
(DEFUN LOAD-EMC-FILE (FILENAME PART &OPTIONAL (UNIT 0))
  "Load microcode from file FILENAME into partition PART on unit UNIT.
UNIT can be a disk unit number, the name of a machine on the chaosnet
or /"CC/" which refers to the machine being debugged by this one."
  (SETQ FILENAME (COND ((NUMBERP FILENAME)
                        (SEND (PATHNAME "SYS:UBIN;ULAMBDA EMC >") :NEW-VERSION
                              FILENAME))
                       ((EQ FILENAME T)
                        (PATHNAME "SYS:UBIN;ULAMBDA EMC >"))
                       (T
                        (MERGE-PATHNAMES FILENAME "SYS: UBIN; ULAMBDA EMC >"))))
  ;;  Do string-equal, not equal, on the canonical-type, not the type
  (if (not (or (STRING-EQUAL (SEND FILENAME :CANONICAL-TYPE) :MCR)
               (STRING-EQUAL (SEND FILENAME :CANONICAL-TYPE) :EMC)))
      (FERROR "~A is not a MCR file." FILENAME))
  (let  ((UNIT (decode-unit-argument UNIT
                                (FORMAT NIL "Loading ~A into ~A partiton"
                                        FILENAME PART)
                                NIL
                                T)))
    (let ((rqb (get-disk-rqb)))
      (MULTIPLE-VALUE-BIND (PART-BASE PART-SIZE NIL PART)
          (FIND-DISK-PARTITION-FOR-WRITE PART NIL UNIT NIL "MCR")
        (WITH-OPEN-FILE (FILE FILENAME :DIRECTION :INPUT :CHARACTERS NIL :BYTE-SIZE 16.)
          (BLOCK DONE
            (DO ((BUF16 (ARRAY-LEADER RQB %DISK-RQ-LEADER-BUFFER))
                 (BLOCK PART-BASE (1+ BLOCK))
                 (N PART-SIZE (1- N)))
                ((ZEROP N) (FERROR "Failed to fit in partition"))
              (DO ((LH) (RH)
                   (I 0 (+ I 2)))
                  ((= I #o1000)
                   (DISK-WRITE RQB UNIT BLOCK))
                (SETQ LH (SEND FILE :TYI)
                      RH (SEND FILE :TYI))
                (COND ((OR (NULL LH) (NULL RH))
                       (UPDATE-PARTITION-COMMENT
                         PART
                         (format nil "~A ~D"
                                 (send (truename file) :name)
                                 (send (truename file) :version))
                         UNIT)
                       (RETURN-FROM DONE NIL)))
                (ASET RH BUF16 I)
                (ASET LH BUF16 (1+ I))))))))))
