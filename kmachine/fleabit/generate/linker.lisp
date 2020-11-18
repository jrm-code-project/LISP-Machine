;;; -*- Mode:LISP; Package:(NC LISP); Base:10.; Readtable:CL -*-

;;;; Compiled Function Objects and Linker

(export '(DEFAFUN NDISASSEMBLE PRINS LINK))

(zl:defsubst nsymbol-function (symbol)
  (get symbol 'ncompiled-function))

(defun create-ncompiled-function (name code local-refs refs immediates entry-points)
  (let ((cfun (setf (nsymbol-function name)
                    (make-ncompiled-function :name name))))
    (setf (ncompiled-function-starting-address cfun) nil)
    (setf (ncompiled-function-code         cfun) (concatenate 'vector code))
    (setf (ncompiled-function-length       cfun) (length code))
    (setf (ncompiled-function-local-refs   cfun) local-refs)
    (setf (ncompiled-function-refs         cfun) refs)
    (setf (ncompiled-function-immediates   cfun) immediates)
    (setf (ncompiled-function-entry-points cfun) entry-points)
    cfun))


(defmacro defafun (name arglist &body instructions)
  "Define an assembly function, takes a body of assembly code.
installs a compiled-function structure on the symbol name"
  `(setf (nsymbol-function ',name)
         (assemble-instruction-list ',name '(,name . ,instructions) '((,(length arglist) . ,name)))))

(defun cc (defun)
  (let ((f (c defun)))
    (setf (nsymbol-function (ncompiled-function-name f))
          f)
    f))

(defun get-ncompiled-function (fcn)
  (cond
    ((ncompiled-function-p fcn) fcn)
    ((symbolp fcn)
     (nsymbol-function fcn))
    (t (cerror "foo" "unknown function thing ~s" fcn))))

(defun ndisassemble (fcn)
  "Print disassembled code of FCN"
  (let ((f (get-ncompiled-function fcn)))
    (let ((code (ncompiled-function-code f)))
      (case code
;       (:in-cold-load (cold::cold-disassemble f))
;       (:in-memory
;        (kbug::disassemble-fcn-from-memory f))
        (t (if (typep code 'sequence)
               (pr-dis (ncompiled-function-code f))
             (error "don't know where this code is: ~a" code))))))
  fcn)

(defun prins (fcn)
  "Print instructions of FCN in hex"
  (let ((f (get-ncompiled-function fcn)))
    (format t "~&Start: ~x" (ncompiled-function-starting-address f))
    (map nil #'(lambda (i)
                 (format t "~&~16,'0x" i))
         (ncompiled-function-code f)))
  fcn)

(defun convert-to-ntuples (vector elements-in-ntuple)
  (let ((list (coerce vector 'list))
        (ans nil)
        (ntuple-so-far nil)
        (count elements-in-ntuple))
    (dolist (e list)
      (setq ntuple-so-far (nconc ntuple-so-far (list e)))
      (cond ((zerop (setq count (1- count)))
             (setq count elements-in-ntuple)
             (setq ans (nconc ans (list ntuple-so-far)))
             (setq ntuple-so-far nil))))
    ans))

(defun convert-to-dotted-pairs (vector)
  (let ((list (coerce vector 'list)))
    (do ((l list (cddr l))
         (ans nil))
        ((null l) ans)
      (setq ans (nconc ans (list (cons (car l) (cadr l))))))))

(defun link (function starting-address &optional (do-callees t))
  "Relocate and resolve references of FUNCTION given STARTING-ADDRESS"
  (let* ((cfun (get-ncompiled-function function))
         (code (ncompiled-function-code cfun)))
    ;; do this first to get recursive calls
    (setf (ncompiled-function-starting-address cfun) starting-address)
    ;; Link local ref addresses
    (dolist (local-ref (convert-to-dotted-pairs (ncompiled-function-local-refs cfun)))
      (let ((iaddr (car local-ref)))
        ;avoid confusion by using all byte stuff below in lambda form.
        ;;  nlisp:dpb, nlisp:ldb, nlisp:byte, nlisp:byte-size, nlisp:byte-position handle this properly
        ;;  please do not change these  --pfc
        (setf (nth iaddr code)
              (nlisp:dpb (nlisp:ldb (nlisp:byte (nlisp:byte-size hw:%%i-branch-address)
                              0)
                        (+ starting-address (cdr local-ref)))
                   hw:%%i-branch-address
                   (nth iaddr code)))))
    ;; Link Refs
    (dolist (ref (convert-to-ntuples (ncompiled-function-refs cfun) 3))
      (let ((reffun (get-ncompiled-function (if (symbolp (cdr ref))
                                                (cdr ref)
                                              (second ref))))
;           (nargs (third ref))
            )
        (if reffun
            (let ((refstart (ncompiled-function-starting-address reffun)))
              (if refstart
                  (setf (nth (car ref) code)
                        (logior (logand #xFFFFFFFFFF000000
                                        (nth (car ref) code))
                                refstart))
                (format t "~&~S is not yet linked" (cdr ref)))
              (pushnew (cons cfun (car ref)) (ncompiled-function-callees reffun) :test #'equal))
          (format t "~&~S is undefined" (cdr ref)))))
    ;; Do callees
    ;; do entry points too
    (when do-callees
      (dolist (callee (coerce (nc::ncompiled-function-callees cfun) 'list))
        (let ((callee-code (nc::ncompiled-function-code (car callee)))
              (addr (+ (nc::ncompiled-function-starting-address (car callee))
                       (cdr callee))))
          (case callee-code
            (:in-memory
             (global:fsignal "This probably does not win")      ;wrong form of address?? --rg
             (lam:write-inst addr (logior (logand #xFFFFFFFFFF000000 (lam:read-inst addr))
                                            starting-address)))))))

    ))
