;;; -*- Mode:LISP; Package:K-KBUG; Base:10; Readtable:CL -*-
;;;
;;; WARM-FILES.LISP
;;;
;;;
;;; This file provides some user functions to boot the K processor and
;;; download system files.
;;;
;;; The function (MEGA-BOOT) builds a new cold load, writes the cold load
;;; to the K processor, and boots the K processor.  Then it downloads the
;;; files on the user#:*warm-loaded-files* list.  Next, it runs the function
;;; (LISP-INTERNALS::WARM-BOOT) on the K processor, which initializes the
;;; package system and evaluator.  Finally, it downloads the files on the
;;; *hot-loaded-files* list.
;;;
;;; When supplied with a non-NIL optional argument, MEGA-BOOT does not
;;; build a new cold load.  Instead, it reuses the last one created.
;;;
;;; Each entry of user#:*warm-loaded-files* and user#:*hot-loaded-files* is either a string
;;; (the filename) or a list of two elements, the filename and a second value.
;;; This second value controls whether or not the corresponding KENV file should
;;; be loaded.  If it is true, or the entry is just a filename, then the KENV
;;; file is loaded on the lambda at the same time the KFASL file is loaded on
;;; the K processor.  If it is NIL, then the KENV file is suppressed.
;;;
;;; Note:  When the lambda is freshly booted, it needs to know what slot the K
;;; processor is in before MEGA-BOOT can be run.  The function which does this
;;; is (LAM:K-LOCAL-SETUP <slot>), where <slot> is the slot number of the K
;;; memory board (the one with the LED's)
;;;
;;; (James Rauen 4-Mar-88 20:25:13)

;;; to see how the K side of the warm-load streams work
;;; look in these files
;;;             "jb:kbug2;streams"          loaded both on K and lambda
;;;             "jb:kbug2;k2"
;;;             "jb:k;warm-loader"

;;; &&& Reorganized file in a more logical manner <08-Nov-88 wkf>

(defvar *downloaded-files*  nil "a list of warm and hot files downloaded by latest MEGA-BOOT")
(defvar *mega-boot-history* nil "a list of lists of files downloaded")

(defun mega-boot (&key fast (compile-type :compile)(load-type :load)(no-warn :just-warn) (warning-stream nil)
                  (info-stream t))              ; $$$  <07-Nov-88 pace> added info stream
  ;; $$$ Added :no-load to support recompilation without a FALCON <09-Nov-88 JIM>
  ;; $$$ Added coupler setup to message and resume capability.    <16-Nov-88 wkf>
  (do () ((or (eq load-type :no-load)
              lam:*local-k-slot*
              (not lam:*local-debugging*)))
    (cerror "Simply proceed." "You did not call (lam:k-local-setup n)
or (lam:k-setup) for non local debugging
or (lam:k-coupler-setup n) for the nubus coupler.
Type ~c to continue after you have done this." #\resume))

  ;; $$$ Added function to remove the file lengths of *cold-files-loaded* when saving the *mega-boot-history*. <16-Nov-88 wkf>
  (push (cons (user#:remove-kenv-flags cold:*cold-files-loaded*) *downloaded-files*)
        *mega-boot-history*)

  ;; WARM-DOWNLOAD pushes pathnames on this list
  (setq *downloaded-files*  nil
        *history-ram-saved* nil)

  (telnet:without-more-processing *terminal-io*
    (let ((si#:inhibit-fdefine-warnings no-warn)
          (*package* (find-package 'k-kbug)))
      (k-boot :inhibit-cold-load? fast :compile-type compile-type :load-type load-type :warning-stream warning-stream
              :info-stream info-stream)
      (format t "~%~% Begining Hot files.")
      ;; $$$ Added :no-load to support recompilation without a FALCON <09-Nov-88 JIM>
      (hot-load user#:*hot-loaded-files* compile-type (if (eq load-type :no-load) () t)))))

(defun k-boot (&key inhibit-cold-load? (compile-type :compile) (load-type :load) (warning-stream t) (info-stream t))
  (let ((loadp (not (eq load-type :no-load))))
    (setq *falcon-error-clobbered* nil)
    (unless inhibit-cold-load?
      (k-cold:make-cold-load :load-type load-type :compile-type compile-type :format-stream warning-stream))
    ;; $$$ Added :no-load to support recompilation without a FALCON <09-Nov-88 JIM>
    (when loadp
      (clear-breakpoint)
      (setq *breakpoints-installed* nil)
      (download-cold-load)

      ;; PSEUDO-BOOT reaches into the cold-load and jumps to the address of the boot function
      ;; which is at **INITIAL-CODE-ENTRY-POINT-BV-OFFSET**
      ;; MAKE-COLD-LOAD has initialized this to BOOT:COLD-BOOT-FUNCTION

      (pseudo-boot info-stream)

      ;; this call to K-RUN fires up  BOOT:COLD-BOOT-FUNCTION,BOOT:EVENT-HORIZON, and BOOT:SYNTHESIZE-COLD-LOAD
      ;; which stops with a call to TRAP::ILLOP to say "Cold load finished!"
      ;; which is picked up by the call to WHY below.

      (let ((msg (kbug-run-loop-then-why)))
        (cond ((not (string-equal (cadr msg) "Cold load finished!"))
               (global:fsignal "First phase of cold load failed to finish ~A" msg))))

      ;;  BOOT:SYNTHESIZE-COLD-LOAD continues with a call to
      ;;   BOOT:WARM-START which initializes the state of all traps and calls
      ;;     K2:INIT-KBUG which resets the lambda/k communications areas and
      ;;       clears the trace-trap (single-step) flag
      ;;     LI:FLUSH-CALL-STACK
      ;;      (never returns, which is why you see that at the bottom of your stack in KBUG2...)
      ;;      calls
      ;;       BOOT:COLD-INITIALIZE-CALL-HARDWARE
      ;;       NUBUS-STUFF:CAUSE-DEBUGGER-TRAP to pause the machine in the debugger
      ;;       BOOT:WAIT-FOR-DEBUGGER which simply loops calling itself
      ;; comments by Peter Cerrato  4/27/88

      (kbug2-run-loop)
      (kbug-load-cold-info :warning-stream warning-stream))     ;download linkage information for cold-load functions.
    (unless (eq inhibit-cold-load? :no-warm)
      ;; $$$ Added :no-load to support recompilation without a FALCON <09-Nov-88 JIM>
      (when loadp
        (let* ((flags (k-mem-read (ash k2:kbug-flag-addr 2)))
               (error (ldb k2:%%kbug-error-flag              flags)))
          (when (= error 1)
            (kbug-stop) (error "The k got an error during cold-boot."))))
      ;;(warm-fix-symbols (car user#:*warm-loaded-files*) compile-type loadp)   ; $$$ Removed <22-Nov-88 wkf>
      ;; $$$ Added :no-load to support recompilation without a FALCON <09-Nov-88 JIM>
      (warm-load user#:*warm-loaded-files* compile-type loadp))))

(defun run-function-on-k (function-symbol)
  "Runs a function on the K.  The function must set the k2:%%kbug-warm-boot-complete-flag when done and then loop."
  (wait-for-kbug-cmd-idle)
  (kbug-goto function-symbol)
  (wait-for-kbug-cmd-idle)
  (kbug-proceed)
  ;; $$$ Upped wait time to 15000 <30-Nov-88 wkf>
  (do* ((wait-time 15000)
        (count 0 (1+ count))
        (flags 0 (k-mem-read (ash k2:kbug-flag-addr 2)))
        (error 0 (ldb k2:%%kbug-error-flag              flags))
        (done  0 (ldb k2:%%kbug-warm-boot-complete-flag flags)))
       ((cond ((= done  1))
              ((= error 1)
               (kbug-stop)
               (error "The k got an error running ~s -- try (KBUG2)." function-symbol))
              ((= count wait-time)
               (cerror "Simply proceed." "Timeout: The K appears to have crashed in ~s.
Type ~c to keep trying with a longer time out." function-symbol #\resume)
               (setq wait-time (* 2 wait-time))
               nil))                            ; $$$ Keep looping. <14-Nov-88 wkf>
        ;; $$$ Added sleep to see if it would affect the "It wouldn't stop messages" <29-Nov-88 JIM>
        (sleep .5 "Waiting for K")
        (kbug-stop)
        (k-mem-write (ash k2:kbug-flag-addr 2) (dpb 0 k2:%%kbug-warm-boot-complete-flag flags)))))

;;; $$$ Removed new warm-symbols method. <22-Nov-88 wkf>
;(defun warm-fix-symbols (&optional (file "falcon:warm;warm-symbols") (compile-type :compile) (download t))
;  "Run the K2:WARM-SYMBOLS function on the K to fix possible duplicate symbols."
;  (when download
;    (compile-and-load-file-for-falcon file :kfasl compile-type download)
;    (run-function-on-k 'k2:warm-symbols)))

(defun hot-load (&optional (file-list user#:*hot-loaded-files*) (compile-type :compile) (download t))
  "Warm-boot the K and load the files in FILE-LIST"
  (when download
    (run-function-on-k 'li:warm-boot))
  (warm-load file-list compile-type download)
  (when download
    (wait-for-kbug-cmd-idle)
    (kbug-proceed)  ;;||| Added last three lines 9/28/88 --wkf
    (sleep 5)                                   ; changed from .5 <29-Nov-88 rg>
    (kbug-stop)
    (kbug2)))

(defun warm-load (&optional (file-list user#:*warm-loaded-files*) (compile-type :compile) (download t))
  (dolist (f file-list)
    (compile-and-load-file-for-falcon f :kfasl compile-type download)))

(defun compile-and-load-file-for-falcon (file &optional (binary-type :kfasl) (compile-type :compile) (download t))
  (let* ((load-environment-file (if (consp file)
                                    (prog1 (cadr file)
                                           (setq file (car file)))
                                  t))
         (binary-pathname       (zl:probef (fs:merge-pathname-defaults file nil binary-type :newest)))
         (file-needs-compiling  (cold:file-needs-compiling-p file binary-pathname)))
    (when (or (and file-needs-compiling
                   (eq compile-type :compile))
              (eq compile-type :recompile))     ; $$$ Simplified. <18-Nov-88 wkf>
      (let ((*package* (zl:pkg-find-package 'user)))
        (ecase binary-type
          (:kfasl (compiler:::nlisp:compile-file     file))
          (:fbin  (compiler::compile-file-for-falcon file)))))
    (when download
      (kbug-download-binary  file binary-type))
    (when load-environment-file
      (load-environment-file file binary-type))))

;;; $$$ Added generic function do the work for fleabit and cross compile and loading. <08-Nov-88 wkf>

(defun warm-compile-and-load-file  (file &optional (compile-type :compile) (load-kenv-file t) (download t))
  (compile-and-load-file-for-falcon (list file load-kenv-file) :kfasl compile-type download))

(defun cross-compile-and-load-file (file &optional (compile-type :compile) (load-fdef-file t) (download t))
  (compile-and-load-file-for-falcon (list file load-fdef-file) :fbin  compile-type download))

(defun kbug-run-loop ()         ;run k, waiting for real processor halt
  (lam:k-run)
  (sleep 3)
  (do ((n 0 (1+ n))                             ; $$$ Added time out. <07-Nov-88 wkf>
       (wait-time 1000))
      ((= 1 (lam:k-read-spy-program-halt)) (k-read-spy-pc))
    (when (= n wait-time)
      (cerror "Simply proceed." "The K appears to have crashed.
Type ~c to keep trying with a longer time out." #\resume)
      (setq wait-time (* 2 wait-time)))))

(defun kbug-run-loop-then-why ()
  (let ((pc (kbug-run-loop))
        (msg nil))
    (format t "~%Falcon stopped, PC=#x~x~%" pc)
    (when (not (= pc #x4c))
      ;; $$$ As of this time #x4c is a good halt pc. Look into why we get others. <19-Nov-88 wkf>
      (cerror "Simply proceed." "The falcon stopped at a location other than #x4c    --wkf
Type ~c to continue anyways." #\resume))
    (setq msg (why))
    (dotimes (i 5) (lam:k-step))
    (list pc msg)))

(defun kbug2-run-loop ()        ;run k, wait for signal from wedge that "main program" has halted. (i.e. command done)
  (lam:k-run)
  (sleep 3)
  (wait-for-kbug-cmd-idle))

(defun wait-for-kbug-cmd-idle ()
  (do () ((kbug-stopped?))))

;;; $$$ Removed LOAD-KENV-FILES defintion which was not used. <08-Nov-88 wkf>
