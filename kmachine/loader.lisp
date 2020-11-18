;;; -*- Mode:LISP; Package:USER; Readtable:ZL; Base:10 -*-

(defvar *k-system-loaded* nil "T if load-k-system function has run.")

;;; $$$ These tell the qfasl loader that .lisp  .qfasl  .kenv  .fdef
;;;      are equivalent as far as not warning the user of function redefinitions. <17-Nov-88 wkf>
;;; $$$ These need to be at the end of the list so as to not confuse the system. <21-Nov-88 wkf>
;;; $$$ Put the new :unspecific types at the end of the list. <22-Nov-88 JIM>
;;; $$$ otherwise they screw up meta-. <22-Nov-88 JIM>
;;; $$$ Removed extra ) at end of this form
(setq fs:*generic-base-type-alist*
      (append (delete '(:fdef . :unspecific)
                      (delete '(:kenv . :unspecific)
                              fs:*generic-base-type-alist*))
              '((:kenv . :unspecific) (:fdef . :unspecific))))

(defun make-cold-band (&optional warnings &aux (warn (if warnings :just-warn t)))
  "This builds a K world including cold download.
To then boot the K:  (lam:k-local-setup xx) (mega-boot :fast t)"
  (load-patches :noselective)
  (make-the-k-system warn)
  (make-the-cold-system warnings warn))

(defun make-the-cold-system (&optional warnings (warn (if warnings :just-warn t)))
  (telnet:without-more-processing *terminal-io*
    (let ((*package* (find-package 'k-kbug))
          (inhibit-fdefine-warnings warn))
      (format t "~%~%Making the Cold Load.")
      (k-cold:make-cold-load :format-stream warnings))))

;;; Don't add functionality to these two functions.
;;; They serve as the driver for loading the entire K system.
;;; Any functions called by update or load will be reloaded by the
;;; (make-system 'falcon)

(defun load-k-system (&rest make-system-keywords)
  "Load all the K software needed for a mega-boot, without compiling or warnings.
Later, software can be reloaded via UPDATE-K-SYSTEM."
  (load-patches :noselective)
  (apply #'make-system 'falcon make-system-keywords)
  (apply #'make-the-k-system t make-system-keywords))


;; Fixed losage: MAKE-THE-K-SYSTEM takes an optional first arg which was eating the :COMPILE keyword
;; This means all those UPDATE-K-SYSTEMs we've been doing have had no effect.
;; Please, please test this code carefully when you hack it !!!
;; ||| 27sept88 pfc

(defun update-k-system (&optional (compile-p t) (load-patches t))
  "Reload K software with warnings and the current system changes, compiling as needed."
  (let ((*package* (find-package :user)))
    (when load-patches (load-patches :noselective))
    (if compile-p
        ; $$$ Removed :defaulted-batch <17-Nov-88 wkf>
        ;; $$$ Added :noconfirm <22-Nov-88 JIM>
        (progn (make-system 'falcon :compile :noconfirm)
               (make-the-k-system t :compile))  ; $$$ Removed :defaulted-batch <17-Nov-88 wkf>
      (progn
        ;; $$$ Added :noconfirm option <22-Nov-88 JIM>
        (make-system 'falcon :noconfirm)
        ;;||| Changed (make-system 'k) to 'falcon so old bands will get new system. (after two makes)  --wkf 9/29/88
        (make-the-k-system)))))

(defun make-the-k-system (&optional inhibit-warnings &rest make-system-keywords)
  (telnet:without-more-processing *terminal-io*
    (let ((inhibit-fdefine-warnings inhibit-warnings)
          (*package* (find-package 'user)))
      (setq si::inhibit-displacing-flag t)  ;; prevent moby lossage with fucking si::displaced macros.
      (make-k-system-internal make-system-keywords)
      (setq *k-system-loaded* t)
      ;;;||| Allow use of #+falcon-system-loaded reader macro to read
      ;;;    only if FALCON tools (loader, etc.) have been loaded. --Keith 21-oct-88
      (pushnew :falcon-system-loaded *features*))))

;;; $$$ Added the bus-coupler system <14-Nov-88 JIM>
(defun make-k-system-internal (make-system-keywords &aux (sys-keywords (cons :noconfirm (cons :no-reload-system-declaration
                                                                                              make-system-keywords))))
  (make-fleabit-system        sys-keywords)
  (make-bus-coupler-system    sys-keywords)
  (make-compiler-system       sys-keywords)
  (make-debugger-system       sys-keywords)
  (make-cross-compiler-system sys-keywords))

(defun make-k-system (make-system-keywords)
  (format t "~%~%Make 'K system")
  (let ((*package* (find-package 'global)))
    (apply 'make-system 'k make-system-keywords)))

;;; $$$ Added make-bus-copuler-system <14-Nov-88 JIM>
(defun make-bus-coupler-system (make-system-keywords)
  (format t "~%~%Make 'bus-coupler system")
  (let ((*package* (find-package 'global)))
    (apply 'make-system 'bus-coupler make-system-keywords)))

(defun make-fleabit-system (make-system-keywords)
  (format t "~%~%Make 'Fleabit system")
  (let ((*package* (find-package 'global)))
    (apply 'make-system 'fleabit make-system-keywords)))

(defun make-debugger-system (make-system-keywords)
  (format t "~%~%Make 'K-Debugger system")
  (let ((*package* (find-package 'k-user)))
    (apply 'make-system 'k-debugger make-system-keywords)))

(defun make-compiler-system (make-system-keywords)
  (format t "~%~%Make 'Compiler-For-K system")
  (apply 'make-system 'compiler-for-k make-system-keywords))

(defun make-cross-compiler-system (make-system-keywords)
  (format t "~%~%Make 'cross-compiler-for-k system")
  (apply 'make-system 'cross-compiler-for-k make-system-keywords))
