;;; -*- Mode:LISP; Package:ZWEI; Readtable:CL; Base:10 -*-

;;Originally in FALCON:K;FLEABIT;TOP.LISP -- moved <28-Oct-88 wkf+keith>
;;These functions rehacked by --wkf 9/30/88 |||

(defun Kompile-defun-for-new-processor (form &optional (stream k-kbug::*kfasl-stream*))
  (send stream :reload-info)
  (k-kbug::kbug-cmd-raw k2:kbug-command-fasl-stream)
  (let ((code (nc:cc form)))
    (typein-line " ... downloading ~D instructions ... "
                      (length (nc:ncompiled-function-code code)))
    (fasdump:fasd-compiled-function code stream))
  (fasdump:fasd-eof stream)
  (send stream :force-output))

(defsubst compile-region-for-new-processor (form)
  (nc:cc form))

(defmacro with-debug-mode (mode &body body) "Adds a debug mode for body."
  `(let ((nc:*debug-flag* (cons ,mode nc:*debug-flag*)))
     ,@body))

(DEFCOM com-fleabit-Kompile-defun-download
        "With numeric arg turns on debug :post with negative arg opens buffer COMPILER-OUTPUT" ()
  (let ((si:*target-features* si:*falcon-features*))
    (with-debug-mode (when *numeric-arg-p* :post)
      (global:let-if (minusp *NUMERIC-ARG*) ((nc:*debug-stream* (open "ed-buffer:compiler-output" :direction :output)))
        (compile-defun-internal #'Kompile-defun-for-new-processor "Fleabit Kompiling" "Fleabit kompiled and downloaded."))))
  dis-none)

(defcom com-fleabit-kompile-disassemble "Opens buffer COMPILER-OUTPUT for debug :post result" ()
  (let ((si:*target-features* si:*falcon-features*))
    (with-debug-mode :post
      (let ((nc:*debug-stream* (open "ed-buffer:compiler-output" :direction :output)))
        (compile-defun-internal #'compile-region-for-new-processor
                                "Outputing to Buffer" "Output in Compiler-output buffer."))))
  dis-none)

(defcom com-fleabit-Kompile-defun
        "Always uses :post debug mode.  With numeric arg opens buffer COMPILER-OUTPUT." ()
  (let ((si:*target-features* si:*falcon-features*))
    (with-debug-mode :post
      ;; $$$ Made uniform with com-fleabit-Kompile-defun-download. <21-Nov-88 wkf>
      (let-if (minusp *numeric-arg*) ((nc:*debug-stream* (open "ed-buffer:compiler-output" :direction :output)))
        (compile-defun-internal #'compile-region-for-new-processor "Outputing fleabit disassemblely" "Disassembled."))))
  dis-none)

;;; +++ The next function when given a numeric arg needs to send output to the buffer MACRO-EXPAND-OUTPUT <4-Nov-88 wkf>
(defcom com-fleabit-macro-expand-expression "Fleabit macroexpand of next s-expression.
The result is printed on the screen with GRIND-TOP-LEVEL." ()
  (LET ((STREAM (REST-OF-INTERVAL-STREAM (POINT)))
        (si:*target-features* si:*falcon-features*))
    (LET ((FORM (READ-OR-BARF STREAM)))
      (global:GRIND-TOP-LEVEL (nlisp:MACROEXPAND FORM))))
  DIS-NONE)

(command-store 'com-fleabit-Kompile-defun-download  #\hyper-shift-C *zmacs-comtab*)
(command-store 'com-fleabit-kompile-disassemble     #\hyper-meta-c  *zmacs-comtab*);;$$$ Changed to hyper-meta <09-Nov-88 wkf>
(command-store 'com-fleabit-Kompile-defun           #\hyper-c       *zmacs-comtab*)
(command-store 'com-fleabit-macro-expand-expression #\hyper-m       *zmacs-comtab*)
;;; +++ We need a recursive macro-expand for #\hyper-shift-m <4-nov-88 wkf>

;;; $$$ This got left out of ZWEI, accidentally correctly. <18-Nov-88 keith>

(set-comtab *zmacs-comtab* nil (make-command-alist '(com-falcon-compile-file)))

#|
(defvar target-computer-CHOICES '(((:lambda "Lambda.") #\L #\space)
                                  ((:falcon "Falcon (K)") #\K #\RUBOUT #\HAND-DOWN)))

(defvar *default-target-computer* nil)

(defcom com-xmacro-expand-expression "Print target-computer macroexpansion of next S-expression.
  With a numeric argument, you select appropriate default target-computer
  for this and next invocation." ()

  (when (or *numeric-arg-p* (null *default-target-computer*))
    (setq *default-target-computer*
          (fquery :tyi
                  :choices '((
  (let ((compi*target- )))))))))
|#
