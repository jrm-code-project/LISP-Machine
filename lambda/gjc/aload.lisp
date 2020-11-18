;-*- Mode:LISP; Package:MACSYMA; Readtable:CL; Base:10 -*-
; This work was produced under the sponsorship of the
;  U.S. Department of Energy.  The Government retains
;  certain rights therein.

; autoload handler for LMI LISPMACHINE ENVIRONMENT for use with DOE-MACSYMA.
; 8/27/85 17:38:33 George Carrette, Paradigm Associates Inc.

(DEFUN SET-UP-AUTOLOAD-CONDITION-HANDLER ()
  ;; this is LISPM specific
  (or (assq 'si:undefined-function eh:condition-default-handlers)
      (push (cons 'si:undefined-function 'autoload-undefined-function-handler)
            eh:condition-default-handlers)))

(DEFVAR *NOW-AUTOLOADING* ())

(DEFUN AUTOLOAD-UNDEFINED-FUNCTION-HANDLER (condition)
  (let ((f (send condition ':containing-structure)))
    (cond ((not (symbolp f))
           (values))
          ('ELSE
           (LET ((FILE-NAME (GET F 'AUTOLOAD)))
             (COND ((OR (NULL FILE-NAME)
                        (GET F 'ALREADY-TRIED-TO-AUTOLOAD))
                    (VALUES))
                   ((ASSQ F *NOW-AUTOLOADING*)
                    (MERROR "Recursive autoload attempt on ~M" F))
                   ('ELSE
                    (LET ((OLD *NOW-AUTOLOADING*))
                      (unwind-protect
                          (progn (setq *NOW-AUTOLOADING* (cons (cons f file-name)
                                                               *NOW-AUTOLOADING*))
                                 (LET (MEXPRP) (LOAD-FILE FILE-NAME)))
                        (setq *now-autoloading* old)))
                    (PUTPROP F FILE-NAME 'ALREADY-TRIED-TO-AUTOLOAD)
                    ;; now some code taken from JPG's FIND0 in SUPRV...
                    (COND ((AND MEXPRP (GET F 'MACRO))
                           (MERROR "LISP MACROs may not be called from MACSYMA level."))
                          ((OR (FBOUNDP F) (AND MEXPRP (MFBOUNDP F)))
                           ;; Win.  Lisp-defined, or called from macsyma level and
                           ;; is macsyma-defined.
                           (VALUES :NEW-VALUE F))
                          ((AND (NOT MEXPRP) (MFBOUNDP F))
                           ;; Sort of a loss, called from lisp level, but defined
                           ;; only at macsyma level.  We want the undefined function
                           ;; handler to take over now, and make sure we never get called
                           ;; on this loser again.
                           (REMPROP F 'AUTOLOAD)
                           (VALUES))
                          (T
                           (MERROR "~A not found" F))))))))))


(set-up-autoload-condition-handler)
