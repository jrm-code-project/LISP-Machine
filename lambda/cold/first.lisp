;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Readtable:ZL; Base:10; COLD-LOAD: T -*-


;;; this is the first file in the cold load, so it has the first thing on the
;;; lisp-crash-list (to be evaluated) by lisp-reinitialize.

;;; When I changed how special forms worked I didnt know how much it was going
;;; to screw me in the cold load. Hence these kludges.
;;; Instead, if a file is COLD-LOAD:T all "eval-mungables" should be
;;; consed up into a single function to be called. This would make things
;;; initialize faster and with less bootstrapping hair. Like in VAX-NIL.

(DEFUN (:PROPERTY SPECIAL SPECIAL-FORM-BOOTSTRAP) (FORM)
  (MAPC (LAMBDA (X) (SETF (GET X 'SPECIAL) T))
        (CDR FORM))
  T)

(DEFUN (:PROPERTY UNSPECIAL SPECIAL-FORM-BOOTSTRAP) (FORM)
  (MAPC (LAMBDA (X) (REMPROP X 'SPECIAL))
        (CDR FORM))
  T)

(EVAL-WHEN (EVAL COMPILE)
  (DEFVAR *BOOTSTRAP-SPECIAL-FORMS*))

(SETQ *BOOTSTRAP-SPECIAL-FORMS*
      '(
         AND                                    ;EVAL
         BLOCK                                  ;EVAL
         CATCH                                  ;EVAL
         COMMENT                                ;EVAL
         COMPILER-LET                           ;EVAL
         COMPILER::CASEN                        ;EVAL
         COND                                   ;EVAL
         DECLARE                                ;EVAL
         DEFCONST-1                             ;LTOP
         DEF                                    ;QFCTNS
         DEFF                                   ;QFCTNS
         DEFF-MACRO                             ;QFCTNS
         DEFSUBST                               ;QFCTNS
         DEFUN                                  ;QFCTNS
         DEFPROP                                ;QRAND
         DEFVAR-1                               ;LTOP
         DO                                     ;EVAL
         DO-NAMED                               ;EVAL
         DO*                                    ;EVAL
         DO*-NAMED                              ;EVAL
         DONT-OPTIMIZE                          ;EVAL
         EVAL-WHEN                              ;EVAL
         FLET                                   ;EVAL
         FUNCTION                               ;EVAL
         GO                                     ;EVAL
         IF                                     ;EVAL
         LABELS                                 ;EVAL
         LAMBDA                                 ;EVAL
         LET                                    ;EVAL
         LET*                                   ;EVAL
         LET-IF                                 ;EVAL
         LETF                                   ;EVAL
         LETF*                                  ;EVAL
         LETF-IF                                ;EVAL
         LOCALLY                                ;EVAL
         MACRO                                  ;QFCTNS
         MACROLET                               ;EVAL
         MULTIPLE-VALUE-BIND                    ;EVAL
         MULTIPLE-VALUE-CALL                    ;EVAL
         MULTIPLE-VALUE-LIST                    ;EVAL
         MULTIPLE-VALUE-PROG1                   ;EVAL
         MULTIPLE-VALUE-SETQ                    ;EVAL
         NTH-VALUE                              ;EVAL
         OR                                     ;EVAL
         PROG                                   ;EVAL
         PROG*                                  ;EVAL
         PROGN                                  ;EVAL
         PROGV                                  ;EVAL
         PROGW                                  ;EVAL
         QUOTE                                  ;EVAL
         RETURN                                 ;EVAL
         RETURN-FROM                            ;EVAL
         SETQ                                   ;EVAL
         SIGNP                                  ;QFCTNS
         SPECIAL                                ;FIRST
         TAGBODY                                ;EVAL
         THE                                    ;EVAL
         THROW                                  ;EVAL
         UNSPECIAL                              ;FIRST
         UNWIND-PROTECT                         ;EVAL
         VARIABLE-BOUNDP                        ;EVAL
         VARIABLE-LOCATION                      ;EVAL
         VARIABLE-MAKUNBOUND                    ;EVAL
         WITH-STACK-LIST                        ;EVAL
         WITH-STACK-LIST*                       ;EVAL
         ))

(DEFUN INSTALL-BOOTSTRAP-SPECIAL-FORMS ()
  (PUTPROP ':SPECIAL-FORM 'SPECIAL-FORM-FUNCTION-SPEC-HANDLER 'FUNCTION-SPEC-HANDLER)
  (DOLIST (X *BOOTSTRAP-SPECIAL-FORMS*)
    (function-spec-putprop `(:special-form ,x)
                           (function-spec-get `(:property ,x special-form-bootstrap) :source-file-name)
                           :source-file-name)
    (BOOTSTRAP-SPECIAL-FORM X)))

(INSTALL-BOOTSTRAP-SPECIAL-FORMS)

(DEFVAR *BOOTSTRAP-SPECIAL-FORMS*)
