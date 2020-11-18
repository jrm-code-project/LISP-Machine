;;; -*- Mode:LISP; Package:(FOO GLOBAL); Base:10; Readtable:ZL -*-

(DEFUN PROCESS-FILE (F INFILE &OPTIONAL &KEY OUTFILE
                     (OUTPUT-VERSION :NEWEST)
                     (OUTPUT-TYPE "OUTPUT")
                     (OUTPUT-BYTE-SIZE 8)
                     (OUTPUT-CHARACTERS-P T))
  (FLET ((MAYBE-COMPUTE (F &REST L) (IF (FUNCTIONP F) (APPLY F L) F)))
    (LET ((INPATH (FS:MERGE-PATHNAME-DEFAULTS INFILE FS:LOAD-PATHNAME-DEFAULTS NIL)))
      (WITH-OPEN-STREAM (INPUT-STREAM
                          (FILE-RETRY-NEW-PATHNAME (INPATH FS:FILE-ERROR)
                            (SEND INPATH :OPEN-CANONICAL-DEFAULT-TYPE :LISP)))
        ;; The input pathname might have been changed by the user in response to an error.
        ;; Also, find out what type field was actually found.
        (SETQ INPATH (SEND INPUT-STREAM :PATHNAME))
        (LET ((GENERIC-PATHNAME (SEND INPATH :GENERIC-PATHNAME)))
          (LET ((OUTPATH (COND ((TYPEP OUTFILE 'PATHNAME)
                                (IF (SEND OUTFILE :VERSION)
                                    OUTFILE
                                  (SEND OUTFILE :NEW-PATHNAME
                                        :VERSION (MAYBE-COMPUTE OUTPUT-VERSION (SEND INPUT-STREAM :TRUENAME)))))
                               (OUTFILE
                                (FS:MERGE-PATHNAME-DEFAULTS
                                  OUTFILE INPATH
                                  (MAYBE-COMPUTE OUTPUT-TYPE GENERIC-PATHNAME)
                                  (MAYBE-COMPUTE OUTPUT-VERSION (SEND INPUT-STREAM :TRUENAME))))
                               (T
                                (SEND INPATH :NEW-PATHNAME
                                      :TYPE (MAYBE-COMPUTE OUTPUT-TYPE GENERIC-PATHNAME)
                                      :VERSION (MAYBE-COMPUTE OUTPUT-VERSION (SEND INPUT-STREAM :TRUENAME)))))))
            ;; Get the file property list again, in case we don't have it already or it changed
            (FS:READ-ATTRIBUTE-LIST GENERIC-PATHNAME INPUT-STREAM)
            ;; Bind all the variables required by the file property list.
            (MULTIPLE-VALUE-BIND (VARIABLES VALS)
                (FS:FILE-ATTRIBUTE-BINDINGS GENERIC-PATHNAME)
              (PROGV VARIABLES VALS
                (WITH-OPEN-FILE (OUTPUT-STREAM OUTPATH
                                               :DIRECTION :OUTPUT
                                               :CHARACTERS OUTPUT-CHARACTERS-P
                                               :BYTE-SIZE OUTPUT-BYTE-SIZE)
                  (SETQ OUTPATH (SEND OUTPUT-STREAM :PATHNAME))
                  (FUNCALL F INPUT-STREAM OUTPUT-STREAM))))))))))

(DEFUN GRIND-FILE (FILENAME)
  (PROCESS-FILE #'(LAMBDA (IN OUT)
                    (FORMAT T "~&Pretty printing ~A => ~A~%"
                            (send in :truename) (send out :truename))
                    (PPRINT (PLIST (SEND (SEND IN :TRUENAME) :GENERIC-PATHNAME)) OUT)
                    (DO ((FORM)
                         (EOF (LIST NIL)))
                        ((EQ EOF (SETQ FORM (READ IN EOF)))
                         (close out)
                         (send out :truename))
                      (PPRINT FORM OUT)))
                FILENAME))
