;;; -*- Mode:LISP; Package:COMPILER; Base:8; Readtable:ZL -*-

;;;    ** (c) Copyright 1980 Massachusetts Institute of Technology **
;;;    ** (c) Copyright 1984 Lisp Machine Inc **
;;;    ** (c) Copyright 1987, GigaMOS Systems Inc. **

;k assembly code is sketched in ORSON:FLEABIT.GENERATE;ASSEM

;transplanted micro-compiler...

(DECLARE (SPECIAL CC-MODE QCMP-OUTPUT))

(defvar *cc-instructions* nil)                  ;in reversed order
(defvar *entry-points* nil)                     ;in form suitable for NC:ASSEMBLE-INSTRUCTION-LIST.
                                                ; a list (<number args> . <entry-tag>).

(defvar *inst-number* nil)                      ;# times to CC-OUT.  For debugging
(defvar *inst-stop-number* nil)                 ;If non-NIL, bkpt on NTH call to CC-OUT


(defun cross-compile (fctn cc-mode)
 ;this function essentially does no work.  (finding the entry points hardly qualifies..)
  (let ((fun-name nil)                          ;The name of the function
        (fun-type nil)                          ;FUNCTION or MACRO
        (word-pointer fctn)                     ;Scan pointer through function
        (*inst-number* 0)                       ;Counter
        (*cc-instructions* nil)                 ;Instructions output
        (*entry-points* nil)                    ;Entry-points found.
        )
    (multiple-value-setq (fun-name fun-type word-pointer) (cc-1 word-pointer))
    (multiple-value-setq (word-pointer) (cc-2 word-pointer))
    (pop word-pointer)                          ;flush random ENDLIST
    (multiple-value-setq (nil word-pointer)
      (cc-process-code nil fun-name word-pointer))      ;Process code
    (cc-out nil)                                ;Force-output
    (cc-out nil)
    (values fun-name *cc-instructions* *entry-points* fun-type)))

(defun cc-process-code (end-tag fun-name word-pointer)  ;Process body of code, but stop if reach end-tag
  (loop for wd = (pop word-pointer)
        while word-pointer
        do (cc-3 wd fun-name)
        (when (and end-tag (eq end-tag wd))
          (pop word-pointer)
          (return (values t word-pointer)))))

(defun unknown-word (phase wd)
  (declare (eh:error-reporter))
  (error "Unknown word in ~A: ~S" phase wd))

(defun cc-1 (word-pointer)
  (loop with name
        with type = 'function
        for wd = (first word-pointer)
        do (unless word-pointer
             (error "Truncated input before QUOTE-BASE"))
        (when (atom wd)
          (unknown-word "CC-1" wd))
        (case (car wd)
          ((qtag)
           (case (second wd)
             ((quote-base) (return (values name type word-pointer)))))
          ((param)
           (set (car wd) (cadr wd)))
          ((comment endlist s-v-block a-d-l quote self-flavor))
          ((mfef)
           (setq name (second wd)))
          ((macro-function)
           (setq type 'macro))
          ((construct-macro)
           (setq type 'macro))
          (otherwise (unknown-word "CC-1" wd)))
        (pop word-pointer)))

(defun cc-2 (word-pointer)                      ;Process QUOTE-LIST
  (loop for wd = (pop word-pointer)
        do (when (atom wd)
             (error "Unknown word in CC-2: ~S" wd))
        (case (car wd)
          ((endlist) (return word-pointer)))))

(defun cc-3 (wd fun-name)                       ;Translate code
  (cond ((null wd))
        ((atom wd)
         (cc-out wd))
        ((and (symbolp (first wd))
              (or (memq (first wd) '(k:open))   ;packages win again!
                  (string-equal (si:package-primary-name (symbol-package (first wd)))
                                "K")))
         (cc-out wd))
        (t (ecase (first wd)
             ((entry)
              (let ((tag (gensymbol fun-name "-entry-" (write-to-string (second wd) :base 10.))))
                (push `(,(cadr wd) . ,tag) *entry-points*)
                (cc-out tag)))
             ((debug-info no-drop-through variables-used-in-lexical-closures breakoffs comment))
             ((param)
              (set (first wd) (second wd)))
             ((restart-tag) (fsignal "RESTART-TAG"))))))

;;see also K-DEST-FROM-LAP-ADDRESS in CROSS-P2.
;;use move or movei
;(defun k-compute-move (dest adr)
;  (cond ((eq (car adr) 'arg)
;        (cc-out `(k:move ,dest ,(a-n (cadr adr)) k:boxed-right)))
;       ((member adr '( (quote nil) (quote t) ))
;        (cc-out
;          `(k:move ,dest ,(make-register-list (cdr (assq (cadr adr)
;                                                         '( (nil . gr:*nil*)
;                                                           (t   . gr:*t*)))))
;                   k:boxed-right)))
;       ((eq (car adr) 'quote)
;        (cc-out `(k:movei ,dest ,adr k:boxed)))
;       ((eq (car adr) 'special)
;        (cc-out `(k:movei k:o0 (quote ,(cadr adr)) k:boxed k:ch-open))
;        (cc-out `(k:call (symbol:%symbol-value 1) ,dest)))
;       ((eq (car adr) 'quote-vector)
;        (k-compute-move dest (cadr adr)))
;       (t (ferror nil "Cant compute move ~s ~s" dest adr))))

;(defun make-register-list (k-register-name)
;  (let ((prop (get k-register-name :register)))
;    `(k:register ,k-register-name ., (cdr prop))))



(defun cc-out (X)
  (when (eq (incf *inst-number*) *inst-stop-number*)
    (break 'inst-stop))
  (cc-final-out x *inst-number*))

;Debugging function
(DEFUN TC (&OPTIONAL (MODE 'PRINT))
  (COND ((EQ MODE 'INPUT)
         (DOLIST (I (G-L-P QCMP-OUTPUT))
           (PRINT I)))
        (T
         (CROSS-COMPILE (G-L-P QCMP-OUTPUT) MODE))))

(defun cc-final-out (x inst-number)
  (ecase cc-mode
    ((print)
     (format t "~%~O:~S" inst-number x))
    ((store)
     (push x *cc-instructions*))))
