;;; -*- Mode:LISP; Package:(ANALYZE GLOBAL); Readtable:CL; Base:10 -*-
;;;
;;; Functions for analysis of the Lisp Machine Environment
;;;
;;;

(defun count-stuff (&optional (top-pkg pkg-global-package))
  (let ((vars 0)
        (functions 0)
        (plists 0))
    (declare (special vars functions))
    (mapatoms-all #'(lambda (symbol)
                      (cond-every ((fboundp symbol) (incf functions))
                                  ((boundp symbol) (incf vars))
                                  ((plist symbol) (incf plists))))
                  top-pkg)
    (list (time:get-universal-time) :vars vars :functions functions :plists plists)))

(defun count-symbols (&optional (top-pkg pkg-global-package))
  (let ((symbols 0)
        (compiled-functions 0)
        (ucode-functions 0)
        (closures 0)
        (named-lambda-functions 0)
        (deffs 0)
        (macros 0)
        (flavors 0)
        (variables 0)
        (defstructs 0)
        (previously-defined 0)
        (sourced-symbols 0)
        (editor-buffers 0))
    (special symbols compiled-functions ucode-functions
             closures named-lambda-functions deffs macros
             flavors variables defstructs previously-defined
             sourced-symbols editor-buffers)
    (mapatoms-all #'(lambda (symbol)
                      (incf symbols)
                      (cond-every ((fboundp symbol)
                                   (typecase (fsymeval symbol)
                                     (:compiled-function (incf compiled-functions))
                                     (:microcode-function (incf ucode-functions))
                                     (closure (incf closures))
                                     (:cons (case (car (fsymeval symbol))
                                              (macro (incf macros))
                                              (named-lambda (incf named-lambda-functions 0))
                                              (t (ferror nil "Unknown thing in function cell: ~A"
                                                         (fsymeval symbol)))))
                                     (:symbol (incf deffs))
                                     (t (ferror nil "Unknown thing in function cell: ~A"
                                                (fsymeval symbol)))))
                                  ((boundp symbol)
                                   (incf variables))
                                  ((get symbol 'si:flavor) (incf flavors))
                                  ((get symbol 'si:defstruct-description) (incf defstructs))
                                  ((get symbol :previous-definition) (incf previously-defined))
                                  ((get symbol ':source-file-name) (incf sourced-symbols))
                                  ((get symbol 'zwei:zmacs-buffers) (incf editor-buffers))))
                  top-pkg)
    (list (time:get-universal-time)
          :symbols symbols
          :compiled-functions compiled-functions
          :ucode-functions ucode-functions
          :closures closures
          :named-lambda-functions named-lambda-functions
          :macros macros
          :flavors flavors
          :variables variables
          :defstructs defstructs
          :previously-defined previously-defined
          :sourced-symbols sourced-symbols
          :editor-buffers editor-buffers)))
