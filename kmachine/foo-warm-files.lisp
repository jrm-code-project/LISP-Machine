;;; -*- Mode:LISP; Package:K-KBUG; Base:10; Readtable:CL -*-

(defparameter *warm-loaded-files*
   ;These are the files which are needed to get the package system and
   ;interpreter running.
   ;** if you add files here, remember to add them to the k-warm system in SIMULATION-SYSDEF.
        '(
          "jb:k.math;generic"
          "jb:k.math;arithmetic"
          "jb:k.math;convert"
          "jb:k.math;fixnum"

          "jb:k.array;array2"
          "jb:k.array;character"
          "jb:k.array;string"

          "jb:k.list;lists"
          "jb:k.list;bald"
          "jb:k;nseq"

          "jb:k;equal"
          "jb:k;hash"

          "jb:k;throw"
          "jb:k;stack-groups"
          "jb:k;control-pdl"
          "jb:k;boot-stack-groups"
          "jb:k;package"
          "jb:k;foo-warm-boot"

          "jb:k.interpreter;vanilla-interpreter"
          "jb:k;defmacro"
          "jb:k;top-level-forms"

          "jb:k;miscellaneous-functions"
          ))

(defparameter *hot-loaded-files*
  ;These are all the other K source files we want to load.
  '(
    "jb:k.lisp-io;readtable"
    "jb:k.lisp-io;reader"
    "jb:k.lisp-io;high-level-streams"
    "jb:k.lisp-io;printer"
    "jb:k.lisp-io;format"

    "jb:k.interpreter;mini-lisp-listener"

;    "jb:k;vcmem-driver"
;    "jb:k;k-uc-tv"

;    "jb:k.math;bignum"
;    "jb:k.math;float"
;    "jb:k.math;rational"
;    "jb:k.math;complex"

;    "jb:k;hot-boot"

    "jb:k.math;cross-support"   ;functions in here exist mostly for cross compiler.
                ))

(defun warm-compile-and-load-file (file &optional (compile-type :compile))
  (let ((kfasl-real-pathname (zl:probef (fs:merge-pathname-defaults file nil "KFASL" :newest))))
    (when (or (and (eq compile-type :compile)
                   (cold:cold-file-needs-compiling? file kfasl-real-pathname))
              (eq compile-type :recompile))
      (global:pkg-goto 'user)
      (compiler:::nlisp:compile-file file)
      (global:pkg-goto 'k-kbug))
    (kbug-fasl file)))

(defun hot-load (&optional (file-list *hot-loaded-files*))
  "Warm-boot the K and load the files in FILE-LIST"
  (kbug-goto 'li:warm-boot)
  (sleep 0.1)
  (kbug-proceed)
  (sleep 10)
  (kbug-stop)
  (warm-load file-list))

(defun mega-boot (&optional fast)
  (k-boot fast)
  (hot-load))

(defun warm-load (&optional (file-list *warm-loaded-files*))
  (dolist (f file-list)
    (warm-compile-and-load-file f)))

(defun k-boot (&optional inhibit-cold-load?)
  (si:pkg-goto 'k-kbug)
  (unless inhibit-cold-load?
    (k-cold:make-cold-load))
  (si:pkg-goto 'k-kbug)
  (clear-breakpoint)
  (setq *breakpoints-installed* nil)
  (download-cold-load)
  (pseudo-boot)
  (lam:k-run)
  (sleep 3)
  (do () ((= 1 (lam:k-read-spy-halt))))
  (why)
  (dotimes (i 5) (lam:k-step))
  (lam:k-run)
  (sleep 3)
  (do () ((kbug-stopped?)))
  (kbug-load-cold-info)
  (unless (eq inhibit-cold-load? :no-warm)
    (warm-load))
  )


(defun load-kenv-files ()
  (let ((*package* (zl:pkg-find-package "USER")))
    (flet ((load-kenv (filename)
             (load (lisp:merge-pathnames filename ".kenv#>"))))
      (dolist (f k-cold:*cold-files*)
        (load-kenv f))
      (dolist (f k-kbug:*warm-loaded-files*)
        (load-kenv f)))))
