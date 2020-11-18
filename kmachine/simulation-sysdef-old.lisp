;;; -*- Mode:LISP; Package:USER; Readtable:ZL; Base:10 -*-

(multiple-value-bind(major minor)
    (si:get-system-version)
  (when (or (< major 124)
            (and (= major 124)
                 (< minor 16)))
    (load "dj:jim;logical-pathname")))

(fs:set-logical-pathname-host
  "K-SYS"
  :physical-host "JB"
  :translations
  '(("k;" "<k>")
    ("k.*;" "<k.*>")
    ("k.*.*;" "<k.*.*>")
    ("kb;" "<kb>")
    ("kb.*;" "<kb.*>")
    ("kbug;" "<kbug>")
    ("kbug2;" "<kbug2>")
    ("kdoc;" "<kdoc>")
    ("kdoc.*;" "<kdoc.*>")
    ("kdoc.*.*;" "<kdoc.*.*>")
    ("rg;" "<rg>")
    ("fleabit;" "<fleabit>")
    ("fleabit.*;" "<fleabit.*>")
    ("fleabit.*.*;" "<fleabit.*.*>")))

(when (= 123 (si:get-system-version))
  (load "k-sys:rg;multi-root")
  (load "sys:sys;clpack")
  (load "k-sys:rg;multi-root1")
  (load "k-sys:rg;multi-root-2"))

(defsystem cross-compiler-for-k
  (:name             "K-cross-compiler")  ;;Use the Lambda compiler with new p2.
  (:pathname-default "sys:k;")
  (:component-systems compiler)
  (:module zwei      "zwei;comc")
  (:module cross     "cross")
  (:module cross-np2 "cross-np2")
  (:compile-load zwei)
  (:compile-load cross     (:fasload zwei)       (:fasload zwei))
  (:compile-load cross-np2 (:fasload zwei cross) (:fasload zwei cross)))

(defsystem compiler-for-k
  (:name "K-Compiler")  ;unfortunately conflicts with regular system compiler. rg 12/18/87
  (:pathname-default "k-sys:k;")

  ;;list of modules in the compiler
  (:module k-macros                  "k-macros")
  (:module k-macros-for-k-debugger   "k-macros-for-k-debugger")  ;;;Hack WKF 5/5/88
  (:module primitive-setf            "primitive-setf.lisp")
  (:module conversions               "conversions.lisp")
  (:module global-registers          "global-registers.lisp")
  (:module mini-fasload-opcodes      "mini-fasload-opcodes.lisp")
  (:module new-fasdump               "new-fasdump")
  (:module defstruct                 "defstruct")
  (:module li-imports-for-k-debugger "li-imports")

  (:compile-load                   k-macros)
  (:compile                        k-macros-for-k-debugger)
  (:readfile primitive-setf       ((:fasload k-macros)))
  (:readfile conversions          ((:fasload k-macros)
                                   (:readfile primitive-setf)))
  (:readfile global-registers     ((:fasload k-macros)
                                   (:readfile primitive-setf conversions)))
  (:readfile mini-fasload-opcodes ((:fasload k-macros)
                                   (:readfile primitive-setf conversions global-registers)))
  (:compile-load new-fasdump      ((:fasload k-macros)
                                   (:readfile primitive-setf conversions global-registers
                                              mini-fasload-opcodes)))
  (:compile-load defstruct        ((:fasload k-macros)
                                   (:readfile primitive-setf conversions global-registers
                                              mini-fasload-opcodes)
                                   (:fasload new-fasdump)))
  (:compile li-imports-for-k-debugger))

;;Files which are in both COMPILER-FOR-K and K-DEBUGGER systems.
;;k;k-macros                compiled           compiled
;;k;conversions             read               read
;;k;global-registers        read               read
;;k;mini-fasload-opcodes    read               read
;;k;new-fasdump             compiled           compiled
;;
;;To fix problem created duplicate versions of these files with file-name-FOR-K-DEBUGGER.  WKF 5/5/88

(defsystem k-debugger
  ;; This system is loaded into the K-xxx package hierarchy!!!
  (:name "K-DEBUGGER")
  (:pathname-default "k-sys:kbug;")

  ;; List of modules in the debugger.
  (:module debug-board            "debug-board")
  (:module li-imports             "k;li-imports")
  (:module debug-crock            "debug-crock0")
  (:module debug-macros           "k;k-macros-for-k-debugger")  ;;wkf this is compiled in COMPILER-FOR-K make-system.
  (:module hw-constants           "k;hardware-constants")
  (:module spy-utilities          "new-spy-utilities")
  (:module debug-support          "debug-support")
  (:module constants              ("k;instructions"  ;this is readfile'd
                                   "k;data-types" "k;firm-definitions"
                                   "k;alu-opcodes"
                                   "k;conversions-for-k-debugger"))
  (:module global-registers       "k;global-registers-for-k-debugger")

    ;following module contains code for both machines.  Compile it for the lambda as well as K.
  (:module both-stuff-1           ("kbug2;common-definitions"))
  (:module both-stuff-2           ("kbug2;streams"                      ;has compile-in-roots.
                                   "k;new-fasdump-for-k-debugger"))

  (:module k-stuff                ("k;trap"              "k;trap-handlers"  "k;gc-ram"           "k;datatype-ram"
                                   "k;nuclear-control"   "k;memory-map"     "k;vmem"             "k;quantum-map"
                                   "k;transporter-ram"   "k;timers"         "k;pcd-table"
                                   "k;memory-management" "k;region-bits"    "k;region-data"
                                   "k;gc-fault.lisp"     "k;area-data.lisp" "k;nubus-interrupts"
                                   "k;memory-interface"  "k;cons"           "k;symbols"          "k.array;array"
                                   "k;boot"   ;was after streams, may have to be in own module
                                   "k;mini-fasload-opcodes-for-k-debugger"
                                   ))
  (:module kold-loader            "k;kold-loader")
  (:module lambda-stuff-2         ("kbug;kbug"          "kbug2;kbug2"
                                   "Kbug2;kbug-generic" "k;warm-files"))

  (:compile-load  debug-board)
  (:fasload       li-imports    (:fasload debug-board))             ;;This should be compiled in COMPILER-FOR-K make-system.
  (:compile-load  debug-crock   (:fasload debug-board li-imports)
                                (:fasload debug-board li-imports))
  (:fasload       debug-macros  (:fasload debug-board debug-crock)) ;;This should be compiled in COMPILER-FOR-K make-system
  (:readfile      hw-constants  (:fasload debug-crock debug-board debug-macros))
  (:compile-load  spy-utilities ((:fasload debug-crock debug-board debug-macros)
                                 (:readfile hw-constants))
                                ((:fasload debug-crock debug-board debug-macros)
                                 (:readfile hw-constants)))
  (:compile-load  debug-support ((:fasload debug-crock debug-board debug-macros)
                                 (:readfile hw-constants)
                                 (:fasload spy-utilities))
                                ((:fasload debug-crock debug-board debug-macros)
                                 (:readfile hw-constants)
                                 (:fasload spy-utilities)))
  (:readfile-init constants     (debug-macros)
                                ((:fasload debug-crock debug-support)
                                 (:readfile hw-constants)
                                 (:fasload spy-utilities debug-support)))
  (:readfile   global-registers ((:fasload debug-crock debug-support)
                                 (:readfile hw-constants)
                                 (:fasload spy-utilities debug-support)
                                 (:readfile constants)))
  (:compile-load  both-stuff-1  ((:fasload debug-crock debug-support)
                                 (:readfile hw-constants)
                                 (:fasload spy-utilities debug-support)
                                 (:readfile constants global-registers))
                                ((:fasload debug-crock debug-support)
                                 (:readfile hw-constants)
                                 (:fasload spy-utilities debug-support)
                                 (:readfile constants global-registers)))
  (:compile-load  both-stuff-2  ((:fasload debug-crock debug-support)
                                 (:readfile hw-constants)
                                 (:fasload spy-utilities debug-support)
                                 (:readfile constants global-registers)
                                 (:fasload both-stuff-1))
                                ((:fasload debug-crock debug-support)
                                 (:readfile hw-constants)
                                 (:fasload spy-utilities debug-support)
                                 (:readfile constants global-registers)
                                 (:fasload both-stuff-1)))
  (:readfile-init k-stuff       (debug-macros)
                                ((:fasload debug-crock debug-support)
                                 (:readfile hw-constants)
                                 (:fasload spy-utilities debug-support)
                                 (:readfile constants global-registers)
                                 (:fasload both-stuff-1 both-stuff-2)))
  (:compile-load-init
     kold-loader (debug-macros) ((:fasload debug-crock debug-support)
                                 (:readfile hw-constants)
                                 (:fasload spy-utilities debug-support)
                                 (:readfile constants global-registers)
                                 (:fasload both-stuff-1 both-stuff-2)
                                 (:readfile k-stuff))
                                ((:fasload debug-crock debug-support)
                                 (:readfile hw-constants)
                                 (:fasload spy-utilities debug-support)
                                 (:readfile constants global-registers)
                                 (:fasload both-stuff-1 both-stuff-2)
                                 (:readfile k-stuff)))
  (:compile-load lambda-stuff-2 ((:fasload debug-crock debug-support)
                                 (:readfile hw-constants)
                                 (:fasload spy-utilities debug-support)
                                 (:readfile constants global-registers)
                                 (:fasload both-stuff-1 both-stuff-2)
                                 (:readfile k-stuff)
                                 (:fasload kold-loader))
                                ((:fasload debug-crock debug-support)
                                 (:readfile hw-constants)
                                 (:fasload spy-utilities debug-support)
                                 (:readfile constants global-registers)
                                 (:fasload both-stuff-1 both-stuff-2)
                                 (:readfile k-stuff)
                                 (:fasload kold-loader))))

(defun load-k-system (&rest make-system-keywords)
  "This function will load all the K software needed for a mega-boot"
  (telnet:without-more-processing *terminal-io*
    (let ((inhibit-fdefine-warnings t)
          (package *package*))
      (apply 'make-k-system nil :noconfirm make-system-keywords)
      (setq *package* package))))

(defun update-k-system (&rest &optional (make-system-keywords '(:compile)))
  "This function will update a K world with the current system changes.  (requires that you load-k-system first)"
  (telnet:without-more-processing *terminal-io*
    (let ((inhibit-fdefine-warnings t)
          (package *package*))
      (apply 'make-k-system t :noconfirm make-system-keywords)
      (setq *package* package))))

(defun make-k-system (updatep &rest make-system-keywords
                      &aux (no-reload-and-sys-keywords (cons :no-reload-system-declaration make-system-keywords)))

  (setq si::inhibit-displacing-flag t)  ;; prevent moby lossage with fucking si::displaced macros.
  (unless updatep
    (load "k-sys:fleabit;sysdef")
    (load "k-sys:kbug;def-corr-pkg"))
  (make-fleabit-system        no-reload-and-sys-keywords updatep)
  (make-compiler-system       no-reload-and-sys-keywords)
  (make-debugger-system       no-reload-and-sys-keywords)
  (make-cross-compiler-system no-reload-and-sys-keywords))

(defun make-fleabit-system (make-system-keywords updatep)
  (pkg-goto 'user)
  (unless updatep
    (load "k-sys:k;illop.qfasl"))
  (setq *package* (find-package 'global))
  (apply 'make-system 'fleabit make-system-keywords)
  (unless updatep
    (load "k-sys:k;compiler-crock0.lisp")))


(defun make-debugger-system (make-system-keywords)
  (format t "~%Load-files-for-debugging")
  (setq *package* (find-package 'k-user))
  (apply 'make-system 'k-debugger make-system-keywords))

(defun make-compiler-system (make-system-keywords)
  (format t "~%Load-files-for-compiling")
  (setq *package* (find-package 'user))
  (apply 'make-system 'compiler-for-k make-system-keywords))

(defun make-cross-compiler-system (make-system-keywords)
  (apply 'make-system 'cross-compiler-for-k make-system-keywords))


;;These two are decomitted.
(defun load-k-system-unattended (&rest make-system-keywords)
  "This function will load all the K software needed for a mega-boot"
  (apply 'load-k-system-unattended make-system-keywords))

(defun load-k-system-on-lambda (&rest make-system-keywords)
  (apply 'make-k-system nil :noconfirm make-system-keywords))

;;;**********************************************************************

(defun load-k-system-on-lambda-from-sources (&rest make-system-keywords)
  ;; prevent moby lossage with fucking si::displaced
  (setq si::inhibit-displacing-flag t)
  (qc-file-load "k-sys:kbug;def-corr-pkg.lisp")
  (make-fleabit-system-from-sources make-system-keywords)
  (load-files-for-compiling-from-sources)
  (load-files-for-debugging-from-sources t)
  )

(defun make-fleabit-system-from-sources (make-system-keywords)
  (pkg-goto 'user)
;  (si::goto-package-environment "USER")
  (load "k-sys:k;illop.qfasl")
  (setq *package* (find-package 'global))
;  (si::goto-package-environment "COMPILER")
;  (load "k-sys:k;simulation-packages.lisp")
;  (load "k-sys:k;lambda-list.qfasl")
;  (load "k-sys:k;new-setf.qfasl")
;  (load "k-sys:K;new-setf-macros.qfasl")
  (apply #'make-system 'fleabit make-system-keywords)   ; make-system-keywords
  (load "k-sys:k;compiler-crock0.lisp#>"))

(defun load-files-for-debugging (all-p)
  (format t "~%Load-files-for-debugging")
  (setq *package* (find-package 'k-user))
;  (si::goto-package-environment "DEBUGGER")
;  (load "k-sys:k;simulation-packages.lisp")    ;packages already created.
  (load "k-sys:K;li-imports.lisp")
  (load "k-sys:kbug;debug-crock0.lisp")
  (load "k-sys:k;hardware-constants.lisp")
  (load "k-sys:kbug;debug-board.qfasl")
  (load "k-sys:kbug;new-spy-utilities.qfasl")   ;LAM package
  (load "k-sys:kbug;debug-support.lisp")
  (when all-p
    (load "k-sys:k;firm-definitions.lisp")
    (load "k-sys:k;data-types.lisp")
    (load "k-sys:k;alu-opcodes.lisp")
    (load "k-sys:k;instructions.lisp")
    (load "k-sys:k;k-macros")
    (load "k-sys:k;conversions.lisp")
    (load "k-sys:k;global-registers.lisp")
    (load "k-sys:k;trap.lisp")
    (load "k-sys:k;trap-handlers.lisp")
    (load "k-sys:k;gc-ram.lisp")
    (load "k-sys:k;datatype-ram.lisp")
    (load "k-sys:k;nuclear-control.lisp")
    (load "k-sys:k;memory-map.lisp")
    (load "k-sys:k;vmem.lisp")
    (load "k-sys:k;transporter-ram.lisp")
    (load "k-sys:k;pcd-table.lisp")
    (load "k-sys:k;quantum-map.lisp")
    (load "k-sys:k;memory-management.lisp")
    (load "k-sys:k;region-bits.lisp")
    (load "k-sys:k;region-data.lisp")
    (load "k-sys:k;gc-fault.lisp")
    (load "k-sys:k;area-data.lisp")
    (load "k-sys:k;nubus-interrupts.lisp")
    (load "k-sys:k;memory-interface.lisp")
    (load "k-sys:k;cons.lisp")
    (load "k-sys:k;symbols.lisp")
    (load "k-sys:k.array;array.lisp"))
  (load "k-sys:kbug2;common-definitions.lisp")
  (load "k-sys:kbug2;streams.lisp")
  (load "k-sys:k;boot.lisp")
  (load "k-sys:k;mini-fasload-opcodes.lisp")
  (load "k-sys:K;new-fasdump.qfasl")
  (load "k-sys:k;kold-loader.qfasl")
  (load "k-sys:kbug;kbug.qfasl")
  (load "k-sys:kbug2;kbug2.qfasl")
  (load "k-sys:Kbug2;kbug-generic.qfasl"))

(defun load-files-for-debugging-from-sources (all-p)
  (format t "~%Load-files-for-debugging")
  (setq *package* (find-package 'k-user))
;  (si::goto-package-environment "DEBUGGER")
;  (load "k-sys:k;simulation-packages.lisp")    ;packages already created.
  (load "k-sys:K;li-imports.lisp")
  (load "k-sys:kbug;debug-crock0.lisp")
  (load "k-sys:k;hardware-constants.lisp")
  (load "k-sys:kbug;debug-board.qfasl")
  (load "k-sys:kbug;new-spy-utilities.qfasl")   ;LAM package.
  (load "k-sys:kbug;debug-support.lisp")
  (when all-p
    (load "k-sys:k;firm-definitions.lisp")
    (load "k-sys:k;data-types.lisp")
    (load "k-sys:k;alu-opcodes.lisp")
    (load "k-sys:k;instructions.lisp")
    (load "k-sys:k;k-macros")
    (load "k-sys:k;conversions.lisp")
    (load "k-sys:k;global-registers.lisp")
    (load "k-sys:k;trap.lisp")
    (load "k-sys:k;trap-handlers.lisp")
    (load "k-sys:k;gc-ram.lisp")
    (load "k-sys:k;datatype-ram.lisp")
    (load "k-sys:k;nuclear-control.lisp")
    (load "k-sys:k;memory-map.lisp")
    (load "k-sys:k;vmem.lisp")
    (load "k-sys:k;transporter-ram.lisp")
    (load "k-sys:k;pcd-table.lisp")
    (load "k-sys:k;quantum-map.lisp")
    (load "k-sys:k;memory-management.lisp")
    (load "k-sys:k;region-bits.lisp")
    (load "k-sys:k;region-data.lisp")
    (load "k-sys:k;gc-fault.lisp")
    (load "k-sys:k;area-data.lisp")
    (load "k-sys:k;nubus-interrupts.lisp")
    (load "k-sys:k;memory-interface.lisp")
    (load "k-sys:k;cons.lisp")
    (load "k-sys:k;symbols.lisp")
    (load "k-sys:k.array;array.lisp"))
  (load "k-sys:kbug2;common-definitions.lisp")
  (load "k-sys:kbug2;streams.lisp")
  (load "k-sys:k;boot.lisp")
  (load "k-sys:k;mini-fasload-opcodes.lisp")
  (load "k-sys:K;new-fasdump.qfasl")
  (qc-file-load "k-sys:k;kold-loader")
  (qc-file-load "k-sys:kbug;kbug")
  (qc-file-load "k-sys:kbug2;kbug2")
  (qc-file-load "k-sys:Kbug2;kbug-generic"))



(defun load-files-for-compiling ()
  (format t "~%Load-files-for-compiling")
  (pkg-goto 'user)
;  (si::goto-package-environment "COMPILER")
;;  (load "k-sys:k;illop.qfasl")
;;  (load "k-sys:k;simulation-packages.lisp")
;;  (load "k-sys:k;hardware-constants.lisp")
;;  (load "k-sys:k;firm-definitions.lisp")
;;  (load "k-sys:k;data-types.lisp")
;;  (load "k-sys:k;alu-opcodes.lisp")
;;  (load "k-sys:k;instructions.lisp")
  (load "k-sys:k;k-macros")
  (load "k-sys:k;primitive-setf.lisp")
  (load "k-sys:k;conversions.lisp")
  (load "k-sys:k;global-registers.lisp")

;;  (load "k-sys:k;trap.lisp")
;;  (load "k-sys:k;gc-ram.lisp")
;;  (load "k-sys:k;datatype-ram.lisp")
;;  (load "k-sys:k;nuclear-control.lisp")
;;  (load "k-sys:k;memory-map.lisp")
;;  (load "k-sys:k;vmem.lisp")
;;  (load "k-sys:k;transporter-ram.lisp")
;;  (load "k-sys:k;pcd-table.lisp")
;;  (load "k-sys:k;quantum-map.lisp")
;;  (load "k-sys:k;memory-management.lisp")
;;  (load "k-sys:k;region-bits.lisp")
;;  (load "k-sys:k;region-data.lisp")
;;  (load "k-sys:k;map-fault.lisp")
;;  (load "k-sys:k;gc-fault.lisp")
;;  (load "k-sys:k;timers.lisp")
;;  (load "k-sys:k;area-data.lisp")
;;  (load "k-sys:k;nubus-interrupts.lisp")
;;  (load "k-sys:k;memory-interface.lisp")
;;  (load "k-sys:k;cons.lisp")
;;  (load "k-sys:k;symbols.lisp")
;;  (load "k-sys:k.array;array.lisp")

;;  (load "k-sys:kbug;new-spy-utilities.lisp")
;;  (load "k-sys:kbug2;common-definitions.lisp")
;;  (load "k-sys:kbug2;streams.lisp")
;;  (load "k-sys:kbug2;k2.lisp")
  (load "k-sys:k;mini-fasload-opcodes.lisp")
  (load "k-sys:K;new-fasdump.qfasl")
;;  (load "k-sys:k;boot.lisp")
  (load "k-sys:k;defstruct")
  )

(defun load-files-for-compiling-from-sources ()
  (format t "~%Load-files-for-compiling")
  (pkg-goto 'user)
;  (si::goto-package-environment "COMPILER")
;;  (load "k-sys:k;illop.qfasl")
;;  (load "k-sys:k;simulation-packages.lisp")
;;  (load "k-sys:k;hardware-constants.lisp")
;;  (load "k-sys:k;firm-definitions.lisp")
;;  (load "k-sys:k;data-types.lisp")
;;  (load "k-sys:k;alu-opcodes.lisp")
;;  (load "k-sys:k;instructions.lisp")
  (qc-file-load "k-sys:k;k-macros")
  (load "k-sys:k;primitive-setf.lisp")
  (load "k-sys:k;conversions.lisp")
  (load "k-sys:k;global-registers.lisp")

;;  (load "k-sys:k;trap.lisp")
;;  (load "k-sys:k;gc-ram.lisp")
;;  (load "k-sys:k;datatype-ram.lisp")
;;  (load "k-sys:k;nuclear-control.lisp")
;;  (load "k-sys:k;memory-map.lisp")
;;  (load "k-sys:k;vmem.lisp")
;;  (load "k-sys:k;transporter-ram.lisp")
;;  (load "k-sys:k;pcd-table.lisp")
;;  (load "k-sys:k;quantum-map.lisp")
;;  (load "k-sys:k;memory-management.lisp")
;;  (load "k-sys:k;region-bits.lisp")
;;  (load "k-sys:k;region-data.lisp")
;;  (load "k-sys:k;map-fault.lisp")
;;  (load "k-sys:k;gc-fault.lisp")
;;  (load "k-sys:k;timers.lisp")
;;  (load "k-sys:k;area-data.lisp")
;;  (load "k-sys:k;nubus-interrupts.lisp")
;;  (load "k-sys:k;memory-interface.lisp")
;;  (load "k-sys:k;cons.lisp")
;;  (load "k-sys:k;symbols.lisp")
;;  (load "k-sys:k.array;array.lisp")

;;  (load "k-sys:kbug;new-spy-utilities.lisp")
;;  (load "k-sys:kbug2;common-definitions.lisp")
;;  (load "k-sys:kbug2;streams.lisp")
;;  (load "k-sys:kbug2;k2.lisp")
  (load "k-sys:k;mini-fasload-opcodes.lisp")
  (qc-file-load "k-sys:K;new-fasdump")
;;  (load "k-sys:k;boot.lisp")
  (qc-file-load "k-sys:k;defstruct")
  )


;this defsystem is currently useful only for tags search.
;should be the same as *cold-files* in k;kold-loader
(defsystem k-cold-load
  (:name "K-COLD-LOAD")
  (:pathname-default "k-sys:k;")
  (:module cold-load ("trap" "trap-handlers" "nuclear-control" "gc-ram" "datatype-ram"
                      "memory-map" "vmem" "transporter-ram" "timers" "pcd-table"
                      "quantum-map" "map-fault" "memory-management"
                      "region-bits" "region-data" "gc-fault" "area-data"
                      "nubus-interrupts" "memory-interface" "type-predicates" "cons"
                      "k.array;array" "symbols" "structure" "kbug2;common-definitions"
                      "kbug2;streams" "kbug2;k2" "warm-loader" "lisp-internals"
                      "error" "dt-ovf-trap" "boot" "control-pdl"))
  (:module temp-cold-files
;****************************** Temps for debug - not permanently in cold load *********
           ("k.math;fixnum" "k.math;generic" "k.math;convert" "k.math;bignum"
            "k.math;rational" "k.math;float" "k.math;complex"))
  (:compile-load cold-load)
  (:compile-load temp-cold-files))

(defsystem k-diag
  (:name "K-DIAG")
  (:pathname-default "k-sys:kb;")
  (:module stuff ("k-regadr" "k-config" "k-regint" "kbug;spy-diags"
                  "k-sys:kbug;test-vectors" "k-sys:kbug;test-vectors-support"))
  (:compile-load stuff)
  )

(defsystem k-warm
 ;useful only for tags search.
  (:name "K-WARM")
  (:pathname-default "k-sys:k;")
 ;this should be the same as *warm-loaded-files* in K;WARM-FILES.
  (:module stuff ("k.math;generic" "k.math;arithmetic" "k.math;convert" "k.math;fixnum"
                  "k.array;array2" "k.array;character" "k.array;string"
                  "k.list;lists" "k-sys:k.list;bald" "k;nseq"

                  "k;equal" "k;hash"

                  "k;throw" "k;stack-groups"
                  "k;control-pdl" "k;boot-stack-groups"
                  "k;package" "k;warm-boot"

                  "k.interpreter;vanilla-interpreter"
                  "k;defmacro"
                  "k;top-level-forms"

                  "k;miscellaneous-functions"
                  ))
  (:compile-load stuff)
  )

(defsystem k-hot
 ;useful only for tags search
  (:name "K-HOT")
  (:pathname-default "k-sys:k;")
 ;this should be the same as *hot-loaded-files* in K;WARM-FILES.
  (:module stuff (
                  "k.lisp-io;readtable"
                  "k.lisp-io;reader"
                  "k.lisp-io;high-level-streams"

                  "k.lisp-io;printer"
                  "k.lisp-io;format"

                  "k.interpreter;mini-lisp-listener"

                  "k;vcmem-driver"
                  "k;k-uc-tv"

                  "k.math;bignum"
                  "k.math;float"
                  "k.math;rational"
                  "k.math;complex"

                  "k.math;cross-support"))
  (:compile-load stuff)
  )

(defsystem k-misc
 ;useful only for tagsearch.
  (:name "K-MISC")
  (:module stuff ("k-sys:kbug;def-corr-pkg"
                  "k-sys:k;compiler-crock0"
;                 "k-sys:rg;multi-root"
;                 "sys:sys;clpack"
;                 "k-sys:rg;multi-root1"
;                 "k-sys:rg;multi-root-2"
                  "k-sys:k;imported-syms"
                  "k-sys:k;daisy-prom"
                  "k-sys:k;daisy-sim"
                  ))
  (:compile-load stuff)
  )

;Likewise currently useful only for tags search.
(defsystem k-runtime
  (:name "K-RUNTIME")
  (:component-systems k-cold-load k-warm k-hot k-debugger k-diag)
  )


(setq si:*source-file-types*
      (pushnew :botex si:*source-file-types*))

(si:define-simple-transformation :botex ignore nil (:botex) NIL)

(defsystem k-documentation
  (:name "k-DOCUMENTATION")
  (:pathname-default "k-sys:kdoc.falcon;")
  (:module k-technical-manual
           ; contains Falcon manual
           ("title-page.botex"
            "copyright-page.botex"
            "introduction.botex"
            "architecture-overview.botex"
            "timing.botex"
            "instruction-set.botex"
            "program-counter.botex"
            "call-hardware.botex"
            "instruction-cache.botex"
            "functional-io.botex"
            "alu-opcodes.botex"
            "transporter-ram.botex"
            "gc-ram.botex"
            "storage-conventions.botex"
            ))
  (:botex k-technical-manual))

(defsystem k-everything
  ;this should include every single file in any way connected with K.
  ;useful for tags search.
  (:name "K-EVERYTHING")
  (:component-systems k-runtime compiler-for-k fleabit k-misc k-documentation))

;; There is just no excuse for having a feature in the language
;; that destroys the ability to use forward references.

;(defun ktest (&optional inhibit-cold-load?)
;   (pkg-goto 'k-user)
; ;  (si::goto-package-environment "DEBUGGER")
;  (unless inhibit-cold-load?
;    (funcall (intern "MAKE-COLD-LOAD" "K-COLD")
;            (symeval (intern "*COLD-FILES*"   "K-COLD"))
;            (intern "COLD-BOOT-FUNCTION" "BOOT")))
;  (funcall (intern "DOWNLOAD-COLD-LOAD" "K-KBUG"))
;  (funcall (intern "PSEUDO-BOOT" "K-KBUG"))
;  (funcall (intern "KBUG" "K-KBUG") nil (symeval (intern "*CODE-START*" "K-KBUG"))))
