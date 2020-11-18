;;; -*- Mode:LISP; Package:CLC; Readtable:CL; Base:10 -*-


(defun cl-file (filename &key output-filename (dribble-filename t))
  (let ((path (fs:parse-pathname filename)))
    (with-open-file (input-stream path)
      (let* ((truename (send path :back-translated-pathname (send input-stream :truename)))
             (output-pathname (cond ((memq output-filename '(t nil))
                                     (send truename :new-pathname :type "FASL" :version :newest))
                                    ('else
                                     (fs:merge-pathname-components output-filename
                                                                   (send truename :new-pathname
                                                                         :version nil
                                                                         :type nil)
                                                                   :default-version :newest
                                                                   :default-type "FASL"))))
             (dribble-pathname (cond ((not dribble-filename)
                                      ())
                                     ((eq dribble-filename t)
                                      (send output-pathname :NEW-PATHNAME :type "TEXT" :version :newest))
                                     ('else
                                      (fs:merge-pathname-components dribble-filename
                                                                    (send output-pathname :new-pathname
                                                                          :version nil
                                                                          :type nil)
                                                                    :default-version :newest
                                                                    :default-type "TEXT")))))
        (cond ((not dribble-pathname)
               (cl-file-stream input-stream output-pathname))
              ('else
               (with-open-file (dribble-file dribble-pathname :direction :output :characters t)
                 (FORMAT DRIBBLE-FILE
                         "Compilation log started at ~\time\ by ~S for~% INPUT: ~S~% OUTPUT: ~S~2%"
                         (TIME:GET-UNIVERSAL-TIME) SI:USER-ID
                         (SEND INPUT-STREAM :TRUENAME)
                         OUTPUT-PATHNAME)
                 (LET ((DRIBBLE-STREAM (SI:MAKE-DRIBBLE-STREAM *TERMINAL-IO* DRIBBLE-FILE)))
                   (LET ((*STANDARD-INPUT* DRIBBLE-STREAM)
                         (*STANDARD-OUTPUT* DRIBBLE-STREAM)
                         (*QUERY-IO* DRIBBLE-STREAM)
                         (*ERROR-OUTPUT* DRIBBLE-STREAM)
                         (*TRACE-OUTPUT* DRIBBLE-STREAM)
                         (TIME (TIME))
                         (DW (SI:READ-METER 'SI:%DISK-WAIT-TIME)))
                     (cl-file-stream input-stream output-pathname)
                     (FORMAT DRIBBLE-FILE
                             "~&~3%Compilation complete at ~\time\~
                              ~%~\scientific\seconds realtime ~\scientific\seconds disk wait~%"
                             (TIME:GET-UNIVERSAL-TIME)
                             (QUOTIENT (TIME-DIFFERENCE (TIME) TIME) 60.0)
                             (QUOTIENT (- (SI:READ-METER 'SI:%DISK-WAIT-TIME) DW) 1.0E6)))))))))))


(DEFUN CALL-WRT-ERRORS (F &REST L)
  (COND (*WARN-ON-ERRORS*
         (CATCH-ERROR (APPLY F L)))
        ('ELSE
         (APPLY F L))))


(DEFUN CL-FILE-STREAM-READ (INPUT-STREAM EOF)
  (CALL-WRT-ERRORS (OR SI:*READFILE-READ-FUNCTION*
                       'READ-CHECK-INDENTATION)
                   INPUT-STREAM EOF))

(defun cl-file-stream (INPUT-STREAM OUTPUT-PATHNAME)
  (LET* ((PATHNAME (SEND INPUT-STREAM :PATHNAME))
         (GENERIC-PATHNAME (SEND PATHNAME :GENERIC-PATHNAME))
         (*PACKAGE* *PACKAGE*)
         (*COMPILER-ENV* (MAKE-COMPILER-ENV)))
    (FS:READ-ATTRIBUTE-LIST GENERIC-PATHNAME INPUT-STREAM)
    (MULTIPLE-VALUE-BIND (VARS VALS)
        (FS:FILE-ATTRIBUTE-BINDINGS GENERIC-PATHNAME)
      (PROGV VARS VALS
        (FORMAT *QUERY-IO* "~&Compiling ~A in package ~A~%" PATHNAME *PACKAGE*)
        (DO ((EOF (LIST 'EOF))
             (FORM))
            ((EQ EOF (SETQ FORM (CL-FILE-STREAM-READ INPUT-STREAM EOF)))
             (CALL-WRT-ERRORS #'CL-FILE-OUTPUT OUTPUT-PATHNAME))
          (CALL-WRT-ERRORS #'CL-TOPLEVEL-FORM FORM))))))


(DEFUN CL-FILE-OUTPUT (OUTPUT-PATHNAME)
  ;; OUTPUT THE RESULTING CODE IN *COMPILER-ENV*
  ;; TO THE FILE.
  OUTPUT-PATHNAME)
