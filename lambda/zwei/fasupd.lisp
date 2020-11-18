;; -*-Mode:LISP; Package:ZWEI; Base:8 -*-

(DEFCOM COM-FASL-UPDATE
             "Update the fasl file of the file you are visiting.
Uses the function definitions present in the environment,
offering to compile them if they have changed.
Note that DECLAREs and EVAL-WHEN (COMPILE)s will be ignored!" ()
    (LET ((BUFFER (READ-BUFFER-NAME "Update fasl file of buffer:"
                       *INTERVAL*               ;Default is current buffer.
                       NIL)))
      (OR (BUFFER-FILE-ID BUFFER)
          (BARF "This buffer is not associated with a file"))
      (SI:FILE-OPERATION-WITH-WARNINGS
        ((AND (BUFFER-FILE-ID BUFFER)
              (FUNCALL (SEND BUFFER ':GENERIC-PATHNAME) ':GENERIC-PATHNAME))
         ':COMPILE NIL)
        (COMPILER:COMPILER-WARNINGS-CONTEXT-BIND
          (COMPILE-BUFFER-CHANGED-FUNCTIONS BUFFER T)))
      (FASL-UPDATE BUFFER))
    DIS-NONE)

;; Write out the compilations of the functions whose sources are in BUFFER.
;; We assume that the user has compiled all the functions he has changed.
;; The QFASL file name is formed from the name of the buffer.
;; We don't actually do any compilation or evaluation of the buffer,
;; though we do expand the macros.

;; Normally, we read each form from the buffer and process it.
;; For forms starting with DEFUN and DEFMETHOD, we read only the
;; function name, which is enough to use to dump the function,
;; and then we skip the rest of the form and cons up a dummy DEFUN or DEFMETHOD
;; with no body or arglist to use in doing the dumping.

(DEFUN FASL-UPDATE (BUFFER &OPTIONAL OUTFILE &AUX INFILE)
  (SETQ INFILE (BUFFER-PATHNAME BUFFER))
  (SETQ OUTFILE
        (IF OUTFILE
            (FS:MERGE-PATHNAME-DEFAULTS OUTFILE INFILE ':QFASL)
          (FUNCALL INFILE ':NEW-TYPE ':QFASL)))
  (COMPILER#:FASL-UPDATE-STREAM INFILE OUTFILE (INTERVAL-STREAM BUFFER)
                                'FASL-UPDATE-BUFFER-READ-FUNCTION))

;;; This function acts like READ, but it doesn't always really do a READ.
;;; It can examine the form coming up and skip it, returning a dummy form.
(DEFUN FASL-UPDATE-BUFFER-READ-FUNCTION (INPUT-STREAM EOF-OPTION)
  ;; Find next interesting object in buffer.
  (LET ((BP (SKIP-OVER-BLANK-LINES-AND-COMMENTS
              (FUNCALL INPUT-STREAM ':READ-BP))))
    (IF (NULL BP) EOF-OPTION
      ;; This is intended to look at the form that follows,
      ;; decide whether it is a defun, and if so
      ;; just create a dummy, since we will not look at the body anyway.
      (MULTIPLE-VALUE-BIND (DEFTYPE FNNAME)
          (FASL-UPDATE-CHECK-DEFUN BP)
        (COND ((AND DEFTYPE
                    (FDEFINEDP (IF (EQ DEFTYPE 'DEFMETHOD)
                                   (CONS ':METHOD FNNAME)
                                 FNNAME)))
               (FUNCALL INPUT-STREAM ':SET-BP
                        ;; The memo-izing lisp parser can cons permanent information
                        (LET ((DEFAULT-CONS-AREA SYS:BACKGROUND-CONS-AREA))
                          (FORWARD-SEXP BP)))
               `(,DEFTYPE ,FNNAME NIL NIL))
              (T
               (FUNCALL INPUT-STREAM ':SET-BP BP)
               (READ INPUT-STREAM EOF-OPTION)))))))

;; This is the list of types of form that we don't even need to read.
(DECLARE (SPECIAL FASL-UPDATE-DEFTYPES-ALIST))
(SETQ FASL-UPDATE-DEFTYPES-ALIST
      '(("DEFUN" DEFUN) ("DEFMETHOD" DEFMETHOD)))

(DEFUN FASL-UPDATE-CHECK-DEFUN (BP &AUX BP1 DEFTYPE FNNAME)
  ;; Now get the second word after BP.
  (AND (= (BP-CH-CHAR BP) #/()
       (SETQ BP (FORWARD-CHAR BP))
       (SETQ BP1 (FORWARD-ATOM BP))
       (SETQ DEFTYPE (CADR (ASS 'EQUALP (STRING-INTERVAL BP BP1)
                                FASL-UPDATE-DEFTYPES-ALIST)))
       (SETQ BP (FORWARD-OVER *BLANKS* BP1))
       (SETQ BP1 (FORWARD-SEXP BP))
       (SETQ FNNAME (STRING-REMOVE-FONTS (STRING-INTERVAL BP BP1)))
       (VALUES DEFTYPE (READ-FROM-STRING FNNAME))))
