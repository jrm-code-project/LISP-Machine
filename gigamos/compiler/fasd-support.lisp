;; -*- Mode:LISP; Package:compiler; Base:8.; Readtable:ZL -*-

;;; $$$ New file. <07-Nov-88 smh>
;;; This file supports fasd generation for the cross compiler.
;;; These functions used to be in SYS:SYS;QCFASD but were moved here to
;;; keep the NC: package from appearing in the cold load.

(defun fasd-k-compiled-function (function &optional (function-type 'function))
  ;; On the Lambda, of course, FUNCTION can never be anything but a symbol
  ;; or an NCOMPILED-FUNCTION. On the K, however, we'd like FASD-CONSTANT to dump
  ;; them, so we could make NC::GET-NCOMPILED-FUNCTION construct an NCOMPILED-FUNCTION
  ;; from the actual function. This *IS* possible, I *THINK*. --RWK
  (let* ((fcn (nc::get-ncompiled-function function))
         (fcn-name (nc::ncompiled-function-name fcn)))
    (fasd-k-function-info
      fcn-name
      (nc::ncompiled-function-local-refs fcn)
      (nc::ncompiled-function-refs fcn)
      (nc::ncompiled-function-immediates fcn)
      (nc::ncompiled-function-entry-points fcn)
      (nc::ncompiled-function-code fcn)
      (nc::ncompiled-function-load-time-evals fcn))
    ;; Now that we have the function put together, put it into the FASD table.
    (let ((fcn-index (fasd-table-add function)))
      (ecase function-type
        ((function))
        ((macro)
         ;; It's a macro; macroify the cell before storing.
         (FASD-START-GROUP T 1 FASL-OP-LIST)
         (FASD-NIBBLE 2)
         (FASD-CONSTANT 'MACRO)
         (FASD-START-GROUP NIL 1 FASL-OP-INDEX)
         (FASD-NIBBLE fcn-index)
         ;; OK, now arrange to store THIS entry in the function cell,
         ;; instead of the function itself.
         (SETQ fcn-index (FASD-TABLE-ADD `(macro ,function)))))
       (if (null fcn-name)
           (fasd-constant nil)                  ;Placeholder
      ;; Store the function we allocated a fasl-table entry for
      ;; earlier, into its proper home.
         (FASD-STORE-FUNCTION-CELL FCN-NAME fcn-index)))))

(defun fasd-k-function-info (name local-refs refs immediates entry-points code load-time-evals)
  (let ((length (length code)))
    (fasd-start-group nil (* length 4) si::fasl-op-k-compiled-function)
    (fasd-k-instructions code)
    (fasd-constant name)
    ;; This has to construct a permanent table, so may as well dump it now.
    (fasd-k-link-info local-refs refs entry-points)
    (fasd-k-immediates immediates)
    (fasd-k-load-time-evals load-time-evals)))

;; Not a group, just a piece of a function.
(defun fasd-k-instructions (code)
  (mapc #'(lambda (instruction)
            (write-k-instruction instruction))
          code))

(eval-when (compile load eval)
  (deftype k-instruction () '(unsigned-byte 64))
  )

(defun write-k-instruction (inst)
;  (check-type inst k-instruction)
  (fasd-nibble (ldb (byte 16.  0.) inst))
  (fasd-nibble (ldb (byte 16.  16.) inst))
  (fasd-nibble (ldb (byte 16. 32.) inst))
  (fasd-nibble (ldb (byte 16. 48.) inst)))

(defun fasd-k-link-info (local-refs refs entry-points)
  (let ((len (length local-refs)))
    (fasd-start-group nil 0 si::fasl-op-k-local-refs)
    (fasd-constant (// len 2))
    (do ((i 0 (+ i 2)))
        ((>= i len))
      (fasd-constant (svref local-refs i))              ;ref offset
      (fasd-constant (svref local-refs (1+ i))))        ;target offset
    (fasd-table-add local-refs))
  (let ((len (length refs)))
    (fasd-start-group nil 0 si::fasl-op-k-refs)
    (fasd-constant (// len 3))
    (do ((i 0 (+ i 3)))
        ((>= i len))
      (fasd-constant (svref refs i))                    ;ref offset
      (fasd-constant (svref refs (1+ i)))               ;referenced function name
      (fasd-constant (svref refs (+ i 2))))             ;number of args
    (fasd-table-add refs))
  (let ((len (length entry-points)))
    (fasd-start-group nil 0 si::fasl-op-k-entry-points)
    (fasd-constant (// len 2))
    (do ((i 0 (+ i 2)))
        ((>= i len))
      (fasd-constant (svref entry-points i))            ;number of args
      (fasd-constant (svref entry-points (1+ i))))      ;entry offset
    (fasd-table-add entry-points)))

(defun fasd-k-immediates (immediates)
  (let ((len (length immediates)))
    (fasd-constant (// len 2))
    (do ((i 0 (+ i 2)))
        ((>= i len))
      (fasd-constant (svref immediates i))              ;ref offset
      (fasd-constant (svref immediates (1+ i))))))      ;immediate object

(defun fasd-k-load-time-evals (items)
  (let ((len (length items)))
    (fasd-constant len)
    (loop for (offset form) in items
          do
          (fasd-constant offset)                        ;ref offset
          (fasd-constant form))))
