;;; -*- Mode:LISP; Package:USER; Readtable:ZL; Base:10 -*-

;these patch files necessary for system 121, should not be necessary for 123.
;except somehow they aren't there (erRic)
(load "dj:rg;multi-root")
(load "sys:sys;clpack")
(load "dj:rg;multi-root1")
(load "dj:rg;multi-root-2")

(eval-when (load eval)
  (load "orson:fleabit;sysdef.lisp")
;  (load "dj:rauen;new-interpreter.system")
  )

(defsystem compiler-for-k
  ;; Pretty name of the system
  (:name "Compiler")
  ;; Where to find the files.
  (:pathname-default "jb:k;")

  ;;list of modules in the compiler
  (:module k-macros             "k-macros")
  (:module primitive-setf       "primitive-setf.lisp")
  (:module conversions          "conversions.lisp")
  (:module global-registers     "global-registers.lisp")
  (:module mini-fasload-opcodes "mini-fasload-opcodes.lisp")
  (:module new-fasdump          "new-fasdump")
  (:module defstruct            "defstruct")
  (:compile-load k-macros)
  (:readfile primitive-setf    ((:fasload k-macros)) )
  (:readfile conversions      ((:fasload k-macros)
                               (:readfile primitive-setf)) )
  (:readfile global-registers ((:fasload k-macros)
                               (:readfile primitive-setf conversions)) )
  (:readfile mini-fasload-opcodes ((:fasload k-macros)
                                   (:readfile primitive-setf conversions global-registers))
                                   )
  (:compile-load new-fasdump ((:fasload k-macros)
                         (:readfile primitive-setf conversions global-registers
                                    mini-fasload-opcodes))
                         )
  (:compile-load defstruct ((:fasload k-macros)
                       (:readfile primitive-setf conversions global-registers
                                  mini-fasload-opcodes)
                       (:fasload new-fasdump))
                       ))


(defsystem k-debugger
  ;; Pretty name of the system
  (:name "K-DEBUGGER")

  ;; Where to find the files.
  (:pathname-default "jb:kbug;")

  ;; List of modules in the debugger.
  (:module debug-board            "debug-board")
  (:module spy-utilities          "new-spy-utilities")
  (:module debug-crock            ("k;li-imports" "debug-crock0"
                                   "k;k-macros"))
  (:module debug-support          "debug-support")
  (:module hw-constants           "k;hardware-constants")
  (:module constants              ( "k;instructions"  ;this is readfile'd
                                   "k;data-types" "k;firm-definitions"
                                   "k;alu-opcodes"
                                   "k;conversions"))
 ;  (:module illop-stuff            "k;illop")              ;;;;not found
  (:module global-registers       "k;global-registers")
    ;following module contains code for both machines.  Compile it for the lambda.
  (:module both-stuff             ("kbug2;common-definitions"   ;has compile-in-roots.
                                   "kbug2;streams"
                                   "k;new-fasdump"))
  (:module k-stuff                ("k;trap" "k;trap-handlers" "k;gc-ram"  "k;datatype-ram"
                                   "k;nuclear-control" "k;memory-map" "k;vmem" "k;quantum-map"
                                   "k;transporter-ram" "k;timers" "k;pcd-table"
                                   "k;memory-management" "k;region-bits" "k;region-data"
                                   "k;gc-fault.lisp" "k;area-data.lisp" "k;nubus-interrupts"
                                   "k;memory-interface" "k;cons" "k;symbols" "k.array;array"
                                   "k;boot"   ;was after streams, may have to be in own module
                                   "k;mini-fasload-opcodes"
                                   ))
  (:module lambda-stuff           ("k;kold-loader" "kbug;kbug" "kbug2;kbug2"
                                   "Kbug2;kbug-generic" "k;warm-files"))
;  (:module cold-loader            "cold-loader")           ;;;;;; not found
;  (:Module initialize-cold-loader "initialize-cold-loader");;;;;;; not found
;  (:module cold-files             ("k;trap.kbin"
;                                  "k;gc-ram.kbin"
;                                  "k;nuclear-control.kbin"
;                                  "k;memory-map.kbin"
;                                  "k;vmem.kbin"
;                                  "k;transporter-ram.kbin"
;                                  "k;timers.kbin"
;                                  "k;pcd-table.kbin"
;                                  "k;boot.kbin"))

  (:compile-load debug-board )
  ;(:readfile debug-packages  )
  (:compile-load debug-crock)
  (:readfile hw-constants (:fasload debug-crock))
  (:compile-load spy-utilities ((:fasload debug-crock) (:fasload debug-board)
                                (:readfile hw-constants)))
  (:compile-load debug-support (:fasload spy-utilities))
  (:readfile constants ((:fasload debug-crock debug-support) (:readfile hw-constants)))
  ;(:compile-load illop-stuff)
  (:readfile global-registers (:readfile constants hw-constants))
  (:compile-load both-stuff ((:fasload debug-support)
                             (:readfile global-registers constants)))
  (:readfile k-stuff ((:fasload debug-support) (:fasload both-stuff)
                      (:readfile global-registers constants)))
  (:compile-load lambda-stuff ((:fasload debug-support)
                               (:readfile k-stuff global-registers constants)
                               (:fasload both-stuff)))
  )


(defun load-k-system-on-lambda (&rest make-system-keywords)
  (apply #'load-k-system-on-lambda-with-defsystem make-system-keywords))

(defun load-k-system-on-lambda-with-defsystem (&rest make-system-keywords)
  ;; prevent moby lossage with fucking si::displaced
  (setq si::inhibit-displacing-flag t)
  (load "jb:Kbug;package-hierarchies")
  (make-hack-compiler make-system-keywords)
  (load-files-for-compiling-with-defsystem (cons :no-reload-system-declaration
                                                 make-system-keywords))
  (load-files-for-debugging-with-defsystem (cons :no-reload-system-declaration
                                                 make-system-keywords))
  )

(defun make-hack-compiler (make-system-keywords)
  (in-hierarchy 'COMP)
  (load "jb:k;k-system-packages.lisp")
  (load "jb:k;illop.qfasl")
  (apply #'make-system 'fleabit make-system-keywords)
  (load "jb:k;compiler-crock0.lisp#>"))


(defun load-files-for-compiling-with-defsystem (make-system-keywords)
  (format t "~%Load-files-for-compiling")
  (in-hierarchy 'COMP)
  (apply 'make-system 'compiler-for-k make-system-keywords))

(defun load-files-for-debugging-with-defsystem (make-system-keywords)
  (format t "~%Load-files-for-debugging")
  (in-hierarchy 'SIM)
  (make-package 'user :use 'global)
  (load "jb:k;k-system-packages")
  (load "jb:k;debugger-packages")
  (apply 'make-system 'k-debugger make-system-keywords))







;this defsystem is currently useful only for tags search.
(defsystem k-cold-load
  (:name "K-COLD-LOAD")
  (:pathname-default "jb:k;")
  (:module cold-load ("trap" "trap-handlers" "nuclear-control" "gc-ram" "datatype-ram"
                      "memory-map" "vmem" "transporter-ram" "timers" "pcd-table"
                      "quantum-map" "map-fault" "memory-management"
                      "region-bits" "region-data" "gc-fault" "area-data"
                      "nubus-interrupts" "memory-interface" "cons"
                      "k.array;array" "symbols" "kbug2;common-definitions"
                      "kbug2;streams" "kbug2;k2" "warm-loader" "lisp-internals"
                      "error" "boot"))
  (:module temp-cold-files
;****************************** Temps for debug - not permanently in cold load *********
           ("k.math;fixnum" "k.math;generic" "k.math;convert" "k.math;bignum"
            "k.math;rational" "k.math;float" "k.math;complex"))
  (:compile-load cold-load)
  (:compile-load temp-cold-files))

(defsystem k-diag
  (:name "K-DIAG")
  (:pathname-default "jb:kb;")
  (:module stuff ("k-regadr" "k-config" "k-regint"))
  (:compile-load stuff)
  )

(defsystem k-warm
 ;useful only for tags search.
  (:name "K-WARM")
  (:pathname-default "jb:k;")
 ;this should be the same as *warm-loaded-files* in this file.
  (:module stuff ("k.math;generic" "k.math;arithmetic" "k.math;convert" "k.math;fixnum"
                  "k.math;bignum" "k.math;float" "k.math;rational" "k.math;complex"
                  "k.array;array2" "k.array;character" "k.array;string"
                  "k.list;lists" "jb:k.list;bald" "k;nseq"

                  "k;equal" "k;hash"

                  "k;throw" "k;stack-groups"

                  "k;package" "k;warm-boot"

                  "k.lisp-io;crutches-for-cold-load"

                  "k.lisp-io;readtable"
                  "k.lisp-io;reader"

                  "k.lisp-io;printer"
                  ))
  (:compile-load stuff)
  )

;Likewise currently useful only for tags search.
(defsystem k-runtime
  (:name "K-RUNTIME")
  (:component-systems k-cold-load k-warm k-debugger k-diag)
  )

(defsystem k-everything
  ;this should include every single file in any way connected with K.
  ;useful for tags search.
  (:name "K-EVERYTHING")
  (:component-systems k-runtime compiler-for-k fleabit))

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
