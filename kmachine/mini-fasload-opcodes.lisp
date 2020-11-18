;;; -*- Mode:LISP; Package:K2; Readtable:CL; Base:10 -*-

;;;**************************************************************************************************************
;;;*****   This file should be identical to mini-fasload-opcodes(-for-k-debugger) at all times WKF 5/5/88   *****
;;;**************************************************************************************************************

;;; These duplicate versions are needed to fool make-system since COMPILER-FOR-K and K-DEBUGGER
;;;  both use this file and k-debugger loads it in the k-user hierarchy.    WKF

(defconstant fasl-opcode-names
             '($$fasl-op-bignum
                $$fasl-op-compiled-function
                $$fasl-op-cons
                $$fasl-op-create-table
                $$fasl-op-defafun
                $$fasl-op-defconstant
                $$fasl-op-defmacro
                $$fasl-op-defsubst
                $$fasl-op-defun
                $$fasl-op-end-of-object
                $$fasl-op-end-of-file
                $$fasl-op-escape
                $$fasl-op-eval
                $$fasl-op-fixnum
                $$fasl-op-in-package
                $$fasl-op-nil
                $$fasl-op-reference-table-index
                $$fasl-op-store-table-index
                $$fasl-op-string
                $$fasl-op-string-character
                $$fasl-op-symbol
                $$fasl-op-short-float
                $$fasl-op-single-float
                $$fasl-op-double-float
                $$fasl-op-defvar
                $$fasl-op-defparameter
                $$fasl-op-unbound
                $$fasl-op-simple-vector
                ))

;(export fasl-opcode-names)


;;; Opcodes for the MINI-FASLOADER

;;; You get 1 byte of opcode.

(defconstant $$FASL-OP-END-OF-FILE #X00)        ; handled by COLD-FASLOAD
; handled by COLD-FASLOAD; handled by COLD-FASLOAD Followed by nothing, exits the mini-fasloader

(defconstant $$FASL-OP-STRING      #x01)        ; handled by COLD-FASLOAD
; handled by COLD-FASLOAD; handled by COLD-FASLOAD Followed by a fixnum size, and then the characters

(defconstant $$fasl-op-fixnum           #x02)   ; handled by COLD-FASLOAD
(defconstant $$fasl-op-string-character #x03)   ; handled by COLD-FASLOAD
(defconstant $$fasl-op-nil              #x04)   ; handled by COLD-FASLOAD
(defconstant $$fasl-op-cons             #x05)   ; handled by COLD-FASLOAD
(defconstant $$fasl-op-end-of-object    #x06)
(defconstant $$fasl-op-create-table     #x07)
(defconstant $$fasl-op-reference-table-index    #x08)
(defconstant $$fasl-op-store-table-index        #x09)
(defconstant $$fasl-op-symbol           #x0a)   ; handled by COLD-FASLOAD
(defconstant $$fasl-op-compiled-function #x0b)  ; handled by COLD-FASLOAD
(defconstant $$fasl-op-defun            #x0c)   ; handled by COLD-FASLOAD
(defconstant $$fasl-op-defmacro         #x0d)   ; handled by COLD-FASLOAD
(defconstant $$fasl-op-defconstant      #x0e)   ; handled by COLD-FASLOAD
(defconstant $$fasl-op-defsubst         #x0f)   ; handled by COLD-FASLOAD
(defconstant $$fasl-op-defafun          #x10)   ; handled by COLD-FASLOAD
(defconstant $$fasl-op-eval             #x11)   ; handled by COLD-FASLOAD
(defconstant $$fasl-op-in-package       #x12)   ; handled by COLD-FASLOAD
(defconstant $$fasl-op-bignum           #x13)   ; handled by COLD-FASLOAD
(defconstant $$fasl-op-short-float      #x14)   ; handled by COLD-FASLOAD
(defconstant $$fasl-op-single-float     #x15)   ; handled by COLD-FASLOAD
(defconstant $$fasl-op-double-float     #x16)   ; handled by COLD-FASLOAD
(defconstant $$fasl-op-defvar           #x17)   ; handled by COLD-FASLOAD
(defconstant $$fasl-op-defparameter     #x18)   ; handled by COLD-FASLOAD
(defconstant $$fasl-op-unbound          #x19)   ; handled by COLD-FASLOAD
(defconstant $$fasl-op-list             #x1A)   ; handled by COLD-FASLOAD
(defconstant $$fasl-op-simple-vector    #x1f)   ; handled by COLD-FASLOAD
;; Followed by a fixnum size, and then the objects

;; For future expansion
(defconstant $$FASL-OP-ESCAPE #xFF)
