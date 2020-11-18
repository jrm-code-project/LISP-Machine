;;;   -*- Mode:LISP; Package:ZWEI; Readtable:ZL; Base:8 -*-

(DEFCOM COM-CRassCOMPILE-REGION "Crosscompile the current region or defun.
 If there is a region, it is compiled.
 Otherwise, the current or next defun is compiled." ()
  (with-open-stream (*standard-output*
                      (open-editor-stream :buffer-name "Crass Crumple"
                                          :create-p t
                                          :start (if *numeric-arg-p* :beginning :end)
                                          ;; :start :beginning :end :end
                                          :kill *numeric-arg-p*
                                          ))
    (let ((*trace-output* *standard-output*)
          (nc:*debug-flag* (or nc:*debug-flag* (list :post))))
      (COMPILE-DEFUN-INTERNAL
        (or (get-buffer-compiler *interval*) T)
        "Crass Crumpling"
        "Crumpled"
        :query-io                               ;USE-TYPEOUT
        NIL                                     ;DEFVAR-HACK
        '(:MODE COMPILER:MACRO-COMPILE)         ;generating-micro-compiler-input-p
                                                ;will wind up getting set however.
        'COMPILER:K                             ;*target-computer*
        compiler:*falcon-environment*           ;compilation-environment
        ))
    (let ((name (send *interval* :name)))
      (if (and (stringp name) (string= name "Crass Crumple"))
          DIS-ALL
        DIS-NONE))))
