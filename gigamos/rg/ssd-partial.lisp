;;; -*- Mode:LISP; Package:USER; Readtable:ZL; Base:10 -*-

(load "dj:rg;multi-root")
(load "sys:sys;clpack")
(load "dj:rg;multi-root1")
(load "dj:rg;multi-root-2")

(eval-when (load eval)
  (load "orson:fleabit;sysdef")
;  (load "dj:rauen;new-interpreter.system")
  )

;;; Make system sucks so bad!  I have never seen anything bite
;;; the bag so thoroughly!


;;; This was used to load the simulator on the lambda.
;(defsystem %k-sim
;  (:module imicro-crock        ("jb:k;imicro-crock"))
;  (:module packages            ("jb:k;simulation-packages"))
;  (:module lambda-macros       ("jb:k;lambda-macros"))
;  (:module sim-constants       ("jb:k;simulation-constants"))
;  (:module sim-area            ("jb:k;simulation-areas"))
;  (:module sim-ucode           ("jb:k;simulation-microcode"))
;  (:module prom-maker          ("jb:k;prom-maker"))
;  (:module simulator           ("jb:k;hardware-simulation"))
;  (:module hardware-constants  ("jb:k;hardware-constants" "jb:k.opcodes;opcode-fields"
;                               "jb:k;instructions"
;                               "jb:k.opcodes;am29332-alu-opcodes"))
;  (:module hardware-defs       ("jb:k;primitives"))
;  (:module primitives          ("jb:k;cs-primitives"))
;  (:module randomness          ("jb:k;k-macros" "jb:k;k-orphans"))
;  (:module vinculum            ("jb:k;firm-definitions" "jb:k;conversions" "jb:k;data-types"))
;  (:module vinc-exporter       ("jb:k;vinc-exporter"))
;  (:module hw-exporter         ("jb:k;hw-exporter"))
;  (:compile-load imicro-crock)
;  (:compile-load packages)
;  (:compile-load sim-constants (:fasload packages))
;  (:compile-load sim-area      (:fasload packages sim-constants))
;  (:compile-load sim-ucode     (:fasload imicro-crock packages sim-constants sim-area))
;  (:compile-load prom-maker    (:fasload packages))
;  (:compile-load lambda-macros (:fasload packages))
;  (:compile-load simulator     (:fasload packages lambda-macros imicro-crock
;                                        sim-constants sim-area sim-ucode prom-maker))
;  (:compile-load hardware-constants (:fasload packages lambda-macros imicro-crock
;                                        sim-constants sim-area sim-ucode prom-maker
;                                        simulator))
;  (:compile-load hardware-defs (:fasload packages lambda-macros imicro-crock
;                                        sim-constants sim-area sim-ucode prom-maker
;                                        simulator hardware-constants))
;  (:compile-load primitifes     (:fasload packages lambda-macros imicro-crock
;                                        sim-constants sim-area sim-ucode prom-maker
;                                        simulator hardware-constants hardware-defs))
;  (:compile-load randomness      (:fasload packages lambda-macros imicro-crock
;                                        sim-constants sim-area sim-ucode prom-maker
;                                        simulator hardware-constants hardware-defs primitives))
;  (:compile-load hw-exporter  (:fasload packages lambda-macros imicro-crock
;                                        sim-constants sim-area sim-ucode prom-maker
;                                        simulator hardware-constants hardware-defs primitives randomness))
;  (:compile-load vinculum    (:fasload packages lambda-macros imicro-crock
;                                        sim-constants sim-area sim-ucode prom-maker
;                                        simulator hardware-constants hardware-defs primitives randomness
;                                        hw-exporter))
;  (:compile-load vinc-exporter    (:fasload packages lambda-macros imicro-crock
;                                        sim-constants sim-area sim-ucode prom-maker
;                                        simulator hardware-constants hardware-defs primitives randomness
;                                        hw-exporter vinculum)))

;;; This loaded the microcode part of the simulator on the lambda
;(defsystem isim
;  (:pathname-default "jb:k.isim;")
;  (:module defs ("k-defs"))
;  (:module microcode ("k-microcode") :package micro)
;  (:module main ("k-control"))
;  (:module i-expand "i-expand")
;  (:module k-funs "k-funs")
;  (:compile-load defs)
;  (:compile-load microcode (:fasload defs))
;  (:compile-load main (:fasload defs microcode))
;  (:compile-load i-expand (:fasload main))
;  (:compile-load k-funs (:fasload i-expand) (:fasload i-expand)))


;;; This defined the files for the virtual memory system on the lambda
;;; it is obsolete.
;(defsystem k-virtual-memory
;  (:name "K-VIRTUAL-MEMORY")
;  (:module map         ("jb:k;memory-map"))
;  (:module gc-ram      ("jb:k;gc-ram"))
;  (:module vmem-common ("jb:k;vmem"))
;  (:module paging-devices ("jb:k;paging-devices"))
;  (:module pcd         ("jb:k;pcd-table"))
;  (:module quantum-map ("jb:k;quantum-map"))
;  (:compile-load map)
;  (:compile-load gc-ram)
;  (:compile-load vmem-common (:fasload map))
;  (:compile-load paging-devices)
;  (:compile-load quantum-map (:fasload map vmem-common gc-ram paging-devices))
;  (:compile-load pcd (:fasload map vmem-common)))

;;; This defined the files for the memory management and allocation for the k,
;;; it is obsolete.
;(defsystem k-memory-management
;  (:name "K-MEMORY-MANAGEMENT")
;  (:module low-mem     ("jb:k;memory-management"))
;  (:module region-bits ("jb:k;region-bits"))
;  (:module region-data ("jb:k;region-data"))
;  (:module area-data   ("jb:k;area-data"))
;  (:module map-fault   ("jb:k;map-fault"))
;  (:module interface   ("jb:k;memory-interface"))
;  (:compile-load low-mem)
;  (:compile-load region-bits (:fasload low-mem))
;  (:compile-load region-data (:fasload low-mem region-bits))
;  (:compile-load area-data   (:fasload low-mem region-bits region-data))
;  (:compile-load map-fault   (:fasload low-mem region-bits region-data area-data))
;  (:compile-load interface   (:fasload low-mem region-bits region-data area-data map-fault)))

;;; This loaded the debugger for the simulator
;(defsystem simulation-debugger
;  (:name   "SIMULATION-DEBUGGER")
;  (:module trap            ("jb:k;trap"))
;  (:module transporter-ram ("jb:k;transporter-ram"))
;  (:module sim-debug       ("jb:k;k-debug"))
;  (:module boot            ("jb:k;boot"))
;  (:compile-load trap)
;  (:compile-load transporter-ram (:fasload trap))
;  (:compile-load sim-debug (:fasload trap transporter-ram))
;  (:compile-load boot (:fasload trap transporter-ram sim-debug)))

;;; This loads up the package environments sets them up for hacking on the K
(defsystem k-hacking
  (:name "K-HACKING")
  (:pathname-default "jb:kbug;")
  (:module package-environments "package-environments")
  (:module k-package-environments "k-hacking-pe")
  (:compile-load package-environments)
  (:compile-load k-package-environments (:fasload package-environments)))

;;; The following was a failed experiment to make MAKE-SYSTEM deal with
;;; making cold loads
;(defun always-load (&rest ignore)
;  t)

;(si::define-simple-transformation :cold-load build-cold-load
;  always-load (:kbin) nil)

;(defun build-cold-load (file)
;  (funcall (intern "COLD-FASLOAD" "COLD")
;          file))

;(defsystem k-debugger
;  ;; Pretty name of the system
;  (:name "K-DEBUGGER")

;  ;; Where to find the files.
;  (:pathname-default "jb:kbug;")

;  ;; List of modules in the debugger.
;  (:module debug-board            "debug-board")
;  (:module spy-utilities          "new-spy-utilities")
;  (:module goto-debugger-pe       "goto-debugger-pe")
;  (:module debug-packages         "k;simulation-packages")
;  (:module debug-support          ("debug-support" "k;k-macros"))
;  (:module constants              ("k;hardware-constants" "k;instructions"
;                                  "k;data-types" "k;firm-definitions"
;                                  "k;conversions"))
;  (:module illop-stuff            "k;illop")
;  (:module global-registers       "k;global-registers")
;  (:module trap                   "k;trap")
;  (:module gc-ram                 "k;gc-ram")
;  (:module nuclear-control        "k;nuclear-control")
;  (:module memory-map             "k;memory-map")
;  (:module vmem                   "k;vmem")
;  (:module quantum-map            "k;quantum-map")
;  (:module transporter-ram        "k;transporter-ram")
;  (:module timers                 "k;timers")
;  (:Module pcd                    "k;pcd-table")
;  (:module memory-management      "k;memory-management")
;  (:module region-bits            "k;region-bits")
;  (:module region-data            "k;region-data")
;  (:module boot                   "k;boot")
;  (:module cold-loader            "cold-loader")
;  (:Module initialize-cold-loader "initialize-cold-loader")
;  (:module cold-files             ("k;trap.kbin"
;                                  "k;gc-ram.kbin"
;                                  "k;nuclear-control.kbin"
;                                  "k;memory-map.kbin"
;                                  "k;vmem.kbin"
;                                  "k;transporter-ram.kbin"
;                                  "k;timers.kbin"
;                                  "k;pcd-table.kbin"
;                                  "k;boot.kbin"))

;  (:compile-load debug-board)
;  (:readfile goto-debugger-pe (:fasload (k-hacking k-package-environments package-environments)) always-load)
;  (:readfile debug-packages   ((:fasload (k-hacking k-package-environments package-environments))
;                              (:readfile goto-debugger-pe)) always-load)
;  (:compile-load debug-support
;                ((:fasload (k-hacking k-package-environments package-environments))
;                           (:readfile goto-debugger-pe debug-packages))
;                ((:fasload (k-hacking k-package-environments package-environments))
;                           (:readfile goto-debugger-pe debug-packages))
;                always-load always-load)
;  (:readfile constants ((:fasload (k-hacking k-package-environments package-environments))
;                                (:readfile goto-debugger-pe debug-packages)
;                                (:fasload debug-support)) always-load)
;  (:compile-load spy-utilities ((:fasload (k-hacking k-package-environments package-environments))
;                               (:readfile goto-debugger-pe debug-packages)
;                               (:fasload debug-support)
;                               (:readfile constants)
;                               (:fasload debug-board)))
;  (:compile-load illop-stuff)
;  (:readfile global-registers ((:fasload (k-hacking k-package-environments package-environments))
;                              (:readfile goto-debugger-pe debug-packages)
;                              (:fasload debug-support)
;                              (:readfile constants)) always-load)
;  (:readfile trap ((:fasload (k-hacking k-package-environments package-environments))
;                  (:readfile goto-debugger-pe debug-packages)
;                  (:fasload debug-support)
;                  (:readfile constants global-registers)) always-load)
;  (:readfile gc-ram ((:fasload (k-hacking k-package-environments package-environments))
;                  (:readfile goto-debugger-pe debug-packages)
;                  (:fasload debug-support)
;                  (:readfile constants global-registers trap)) always-load)
;  (:readfile nuclear-control ((:fasload (k-hacking k-package-environments package-environments))
;                             (:readfile goto-debugger-pe debug-packages)
;                             (:fasload debug-support)
;                             (:readfile constants global-registers trap)) always-load)
;  (:readfile memory-map ((:fasload (k-hacking k-package-environments package-environments))
;                             (:readfile goto-debugger-pe debug-packages)
;                             (:fasload debug-support)
;                             (:readfile constants global-registers trap nuclear-control)) always-load)
;  (:readfile vmem ((:fasload (k-hacking k-package-environments package-environments))
;                             (:readfile goto-debugger-pe debug-packages)
;                             (:fasload debug-support)
;                             (:readfile constants global-registers trap nuclear-control memory-map))
;            always-load)
;  (:readfile quantum-map ((:fasload (k-hacking k-package-environments package-environments))
;                             (:readfile goto-debugger-pe debug-packages)
;                             (:fasload debug-support)
;                             (:readfile constants global-registers trap nuclear-control memory-map))
;            always-load)
;  (:readfile transporter-ram ((:fasload (k-hacking k-package-environments package-environments))
;                             (:readfile goto-debugger-pe debug-packages)
;                             (:fasload debug-support)
;                             (:readfile constants global-registers trap nuclear-control memory-map vmem))
;            always-load)
;  (:readfile timers ((:fasload (k-hacking k-package-environments package-environments))
;                             (:readfile goto-debugger-pe debug-packages)
;                             (:fasload debug-support)
;                             (:readfile constants global-registers trap nuclear-control memory-map vmem))
;            always-load)
;  (:readfile pcd ((:fasload (k-hacking k-package-environments package-environments))
;                             (:readfile goto-debugger-pe debug-packages)
;                             (:fasload debug-support)
;                             (:readfile constants global-registers trap nuclear-control
;                                        memory-map vmem transporter-ram timers))
;            always-load)
;  (:readfile memory-management ((:fasload (k-hacking k-package-environments package-environments))
;                               (:readfile goto-debugger-pe debug-packages)
;                               (:fasload debug-support)
;                               (:readfile constants global-registers trap nuclear-control
;                                          memory-map vmem transporter-ram timers))
;            always-load)
;  (:readfile region-bits ((:fasload (k-hacking k-package-environments package-environments))
;                             (:readfile goto-debugger-pe debug-packages)
;                             (:fasload debug-support)
;                             (:readfile constants global-registers trap nuclear-control
;                                        memory-map vmem quantum-map memory-management
;                                        transporter-ram timers))
;            always-load)
;  (:readfile region-data ((:fasload (k-hacking k-package-environments package-environments))
;                             (:readfile goto-debugger-pe debug-packages)
;                             (:fasload debug-support)
;                             (:readfile constants global-registers trap nuclear-control
;                                        memory-map vmem quantum-map memory-management
;                                        transporter-ram timers region-bits))
;            always-load)
;  (:readfile boot ((:fasload (k-hacking k-package-environments package-environments))
;                             (:readfile goto-debugger-pe debug-packages)
;                             (:fasload debug-support)
;                             (:readfile constants global-registers trap nuclear-control
;                                        memory-map vmem quantum-map memory-managment
;                                        transporter-ram timers pcd region-bits region-data))
;            always-load)
;  )


;  (:cold-load cold-files ((:fasload (k-hacking k-package-environments package-environments))
;                             (:readfile goto-debugger-pe debug-packages)
;                             (:fasload debug-support)
;                             (:readfile constants global-registers trap nuclear-control
;                                        memory-map vmem transporter-ram timers pcd boot
;                                        initialize-cold-loader)))

;(defun

;  (:readfile hardware-constants (:fasload goto-debugger-pe simulation-packages))
;  (:compile-load debug-board)
;  (:compile-load-init spy-utilities (debug-board)
;                     ((:fasload debug-board package-environments crock1 debug-packages
;                               debug-hardware-support) (:readfile hardware-constants)))
;  (:compile-load boot-support
;                (:fasload package-environments crock1 debug-packages))
;  (:readfile boot-code (:fasload package-environments crock1 debug-packages boot-support)))



;;;  ME ME ME oh pleease load me! yes
;;; This is what you want to load.
(defun load-k-system-on-lambda (&rest make-system-keywords)
  ;; prevent moby lossage with fucking si::displaced
  (setq si::inhibit-displacing-flag t)
  (load "alex:kbug;def-corr-pkg")
  (make-hack-compiler make-system-keywords)
  (load-files-for-compiling)
  (load-files-for-debugging t)
  )

(defun load-k-system-on-lambda-from-sources (&rest make-system-keywords)
  ;; prevent moby lossage with fucking si::displaced
  (setq si::inhibit-displacing-flag t)
  (qc-file-load "jb:kbug;def-corr-pkg.lisp")
  (make-hack-compiler-from-sources make-system-keywords)
  (load-files-for-compiling-from-sources)
  (load-files-for-debugging-from-sources t)
  )

(defun load-k-system-on-lambda-with-defsystem (&rest make-system-keywords)
  ;; prevent moby lossage with fucking si::displaced
 (fsignal "--** broken **--")
  (setq si::inhibit-displacing-flag t)
  (load "jb:kbug;def-corr-pkg")
  (make-hack-compiler-from-sources make-system-keywords)
  (load-files-for-compiling-new)
;  (load-files-for-compiling-with-defsystem)
  (load-files-for-debugging-with-defsystem t)
  )

(defun make-hack-compiler (make-system-keywords)
  (make-system 'k-hacking)
  (pkg-goto 'user)
;  (si::goto-package-environment "USER")
;  (defpackage NC :use lisp)
  (defpackage K  :use () (:import-from lisp "OPEN" "RETURN" "OR" "AND"))
  (defpackage KBUG :use '(lisp k lam))
  (defpackage SETF :use '(lisp))
  (load "jb:k;illop.qfasl")
  (setq *package* (find-package 'global))
;  (si::goto-package-environment "COMPILER")
  (load "jb:k;simulation-packages.lisp")
;  (load "jb:k;lambda-list.qfasl")
;  (load "jb:k;new-setf.qfasl")
;  (load "jb:K;new-setf-macros.qfasl")
  (apply #'make-system 'fleabit make-system-keywords)
  (load "jb:k;compiler-crock0.lisp#>"))

(defun make-hack-compiler-from-sources (make-system-keywords)
  (make-system 'k-hacking)
  (pkg-goto 'user)
;  (si::goto-package-environment "USER")
;  (defpackage NC :use lisp)
  (defpackage K  :use () (:import-from lisp "OPEN" "RETURN" "OR" "AND"))
  (defpackage KBUG :use '(lisp k lam))
  (defpackage SETF :use '(lisp))
  (load "jb:k;illop.qfasl")
  (setq *package* (find-package 'global))
;  (si::goto-package-environment "COMPILER")
  (load "jb:k;simulation-packages.lisp")
;  (load "jb:k;lambda-list.qfasl")
;  (load "jb:k;new-setf.qfasl")
;  (load "jb:K;new-setf-macros.qfasl")
  (apply #'make-system 'fleabit make-system-keywords)
  (load "jb:k;compiler-crock0.lisp#>"))

;;; Don't use this.
;(defun make-hack-debugger ()
;  (make-system 'k-debugger))

;  (:module debug-board "debug-board")
;  (:module package-environments ("jb:k;clpack"
;  (:module spy-utilities ("jb:k;spy-utilities"))
;  (:module spy-diags   ("jb:k;spy-diags"))
;  (:module cold-loader ("jb:k;cold-loader"))
;  (:module k-debug-stuff ("jb:k;kbug"))
;  (:module illop         ("jb:k;illop"))
;  (:compile-load debug-board)
;  (:compile-load spy-utilities (:fasload debug-board))
;  (:compile-load spy-diags (:fasload debug-board spy-utilities))
;  (:compile-load illop      (:fasload debug-board spy-utilities spy-diags))
;  (:compile-load cold-loader (:fasload debug-board spy-utilities spy-diags illop))
;  (:compile-load k-debug-stuff (:fasload debug-board spy-utilities spy-diags illop cold-loader)))

;(defsystem k-simulator
;  (:name "K-SIMULATOR")
;  (:component-systems %k-sim isim))


;(defsystem assembler
;  (:module pack "jb:k;simulation-packages")
;  (:module hw ("jb:k;hardware-constants" "jb:k;instructions" "jb:k;alu-opcodes"))
;  (:module hwx "jb:k;hw-exporter")
;  (:module global-frames "jb:k;global-frames")
;  (:module assem ("orson:fleabit.generate;assem"))
;  (:module dis ("orson:fleabit.generate;dis"))
;  (:module linker ("orson:fleabit.generate;linker"))
;  (:compile-load pack)
;  (:compile-load hw (:fasload pack) (:fasload pack))
;  (:compile-load hwx (:fasload hw) (:fasload hw))
;  (:compile-load global-frames)
;  (:compile-load assem (:fasload hw hwx global-frames) (:fasload hw hwx global-frames))
;  (:compile-load dis (:fasload hw hwx global-frames assem) (:fasload hw hwx global-frames assem))
;  (:compile-load linker (:fasload hw hwx global-frames assem) (:fasload hw hwx global-frames assem)))

;(si::define-simple-transformation :k-compile kompile-file
;  si::file-newer-than-file-p (:lisp) (:kbin))

;(defun kompile-file (&rest args)
;  (apply (intern "COMPILE-FILE" "NLISP") args))

(defun load-files-for-debugging (all-p)
  (format t "~%Load-files-for-debugging")
  (setq *package* (find-package 'k-user))
;  (si::goto-package-environment "DEBUGGER")
;  (load "jb:k;simulation-packages.lisp")       ;packages already created.
  (load "jb:K;li-imports.lisp")
  (load "jb:kbug;debug-crock0.lisp")
  (load "jb:k;hardware-constants.lisp")
  (load "jb:kbug;debug-board.qfasl")
  (load "jb:kbug;new-spy-utilities.qfasl")   ;LAM package
  (load "jb:kbug;debug-support.lisp")
  (when all-p
    (load "jb:k;firm-definitions.lisp")
    (load "jb:k;data-types.lisp")
    (load "jb:k;alu-opcodes.lisp")
    (load "jb:k;instructions.lisp")
    (load "jb:k;k-macros")
    (load "jb:k;conversions.lisp")
    (load "jb:k;global-registers.lisp")
    (load "jb:k;trap.lisp")
    (load "jb:k;trap-handlers.lisp")
    (load "jb:k;gc-ram.lisp")
    (load "jb:k;datatype-ram.lisp")
    (load "jb:k;nuclear-control.lisp")
    (load "jb:k;memory-map.lisp")
    (load "jb:k;vmem.lisp")
    (load "jb:k;transporter-ram.lisp")
    (load "jb:k;pcd-table.lisp")
    (load "jb:k;quantum-map.lisp")
    (load "jb:k;memory-management.lisp")
    (load "jb:k;region-bits.lisp")
    (load "jb:k;region-data.lisp")
    (load "jb:k;gc-fault.lisp")
    (load "jb:k;area-data.lisp")
    (load "jb:k;nubus-interrupts.lisp")
    (load "jb:k;memory-interface.lisp")
    (load "jb:k;cons.lisp")
    (load "jb:k;symbols.lisp")
    (load "jb:k.array;array.lisp"))
  (load "jb:kbug2;common-definitions.lisp")
  (load "jb:kbug2;streams.lisp")
  (load "jb:k;boot.lisp")
  (load "jb:k;mini-fasload-opcodes.lisp")
  (load "jb:K;new-fasdump.qfasl")
  (load "jb:k;kold-loader.qfasl")
  (load "jb:kbug;kbug.qfasl")
  (load "jb:kbug2;kbug2.qfasl")
  (load "jb:Kbug2;kbug-generic.qfasl"))

(defun load-files-for-debugging-from-sources (all-p)
  (format t "~%Load-files-for-debugging")
  (setq *package* (find-package 'k-user))
;  (si::goto-package-environment "DEBUGGER")
;  (load "jb:k;simulation-packages.lisp")       ;packages already created.
  (load "jb:K;li-imports.lisp")
  (load "jb:kbug;debug-crock0.lisp")
  (load "jb:k;hardware-constants.lisp")
  (load "jb:kbug;debug-board.qfasl")
  (load "jb:kbug;new-spy-utilities.qfasl")      ;LAM package.
  (load "jb:kbug;debug-support.lisp")
  (when all-p
    (load "jb:k;firm-definitions.lisp")
    (load "jb:k;data-types.lisp")
    (load "jb:k;alu-opcodes.lisp")
    (load "jb:k;instructions.lisp")
    (load "jb:k;k-macros")
    (load "jb:k;conversions.lisp")
    (load "jb:k;global-registers.lisp")
    (load "jb:k;trap.lisp")
    (load "jb:k;trap-handlers.lisp")
    (load "jb:k;gc-ram.lisp")
    (load "jb:k;datatype-ram.lisp")
    (load "jb:k;nuclear-control.lisp")
    (load "jb:k;memory-map.lisp")
    (load "jb:k;vmem.lisp")
    (load "jb:k;transporter-ram.lisp")
    (load "jb:k;pcd-table.lisp")
    (load "jb:k;quantum-map.lisp")
    (load "jb:k;memory-management.lisp")
    (load "jb:k;region-bits.lisp")
    (load "jb:k;region-data.lisp")
    (load "jb:k;gc-fault.lisp")
    (load "jb:k;area-data.lisp")
    (load "jb:k;nubus-interrupts.lisp")
    (load "jb:k;memory-interface.lisp")
    (load "jb:k;cons.lisp")
    (load "jb:k;symbols.lisp")
    (load "jb:k.array;array.lisp"))
  (load "jb:kbug2;common-definitions.lisp")
  (load "jb:kbug2;streams.lisp")
  (load "jb:k;boot.lisp")
  (load "jb:k;mini-fasload-opcodes.lisp")
  (load "jb:K;new-fasdump.qfasl")
  (qc-file-load "jb:k;kold-loader")
  (qc-file-load "jb:kbug;kbug")
  (qc-file-load "jb:kbug2;kbug2")
  (qc-file-load "jb:Kbug2;kbug-generic"))



(defun load-files-for-compiling ()
  (format t "~%Load-files-for-compiling")
  (pkg-goto 'user)
;  (si::goto-package-environment "COMPILER")
;;  (load "jb:k;illop.qfasl")
;;  (load "jb:k;simulation-packages.lisp")
;;  (load "jb:k;hardware-constants.lisp")
;;  (load "jb:k;firm-definitions.lisp")
;;  (load "jb:k;data-types.lisp")
;;  (load "jb:k;alu-opcodes.lisp")
;;  (load "jb:k;instructions.lisp")
  (load "jb:k;k-macros")
  (load "jb:k;primitive-setf.lisp")
  (load "jb:k;conversions.lisp")
  (load "jb:k;global-registers.lisp")

;;  (load "jb:k;trap.lisp")
;;  (load "jb:k;gc-ram.lisp")
;;  (load "jb:k;datatype-ram.lisp")
;;  (load "jb:k;nuclear-control.lisp")
;;  (load "jb:k;memory-map.lisp")
;;  (load "jb:k;vmem.lisp")
;;  (load "jb:k;transporter-ram.lisp")
;;  (load "jb:k;pcd-table.lisp")
;;  (load "jb:k;quantum-map.lisp")
;;  (load "jb:k;memory-management.lisp")
;;  (load "jb:k;region-bits.lisp")
;;  (load "jb:k;region-data.lisp")
;;  (load "jb:k;map-fault.lisp")
;;  (load "jb:k;gc-fault.lisp")
;;  (load "jb:k;timers.lisp")
;;  (load "jb:k;area-data.lisp")
;;  (load "jb:k;nubus-interrupts.lisp")
;;  (load "jb:k;memory-interface.lisp")
;;  (load "jb:k;cons.lisp")
;;  (load "jb:k;symbols.lisp")
;;  (load "jb:k.array;array.lisp")

;;  (load "jb:kbug;new-spy-utilities.lisp")
;;  (load "jb:kbug2;common-definitions.lisp")
;;  (load "jb:kbug2;streams.lisp")
;;  (load "jb:kbug2;k2.lisp")
  (load "jb:k;mini-fasload-opcodes.lisp")
  (load "jb:K;new-fasdump.qfasl")
;;  (load "jb:k;boot.lisp")
  (load "jb:k;defstruct")
  )

(defun load-files-for-compiling-from-sources ()
  (format t "~%Load-files-for-compiling")
  (pkg-goto 'user)
;  (si::goto-package-environment "COMPILER")
;;  (load "jb:k;illop.qfasl")
;;  (load "jb:k;simulation-packages.lisp")
;;  (load "jb:k;hardware-constants.lisp")
;;  (load "jb:k;firm-definitions.lisp")
;;  (load "jb:k;data-types.lisp")
;;  (load "jb:k;alu-opcodes.lisp")
;;  (load "jb:k;instructions.lisp")
  (qc-file-load "jb:k;k-macros")
  (load "jb:k;primitive-setf.lisp")
  (load "jb:k;conversions.lisp")
  (load "jb:k;global-registers.lisp")

;;  (load "jb:k;trap.lisp")
;;  (load "jb:k;gc-ram.lisp")
;;  (load "jb:k;datatype-ram.lisp")
;;  (load "jb:k;nuclear-control.lisp")
;;  (load "jb:k;memory-map.lisp")
;;  (load "jb:k;vmem.lisp")
;;  (load "jb:k;transporter-ram.lisp")
;;  (load "jb:k;pcd-table.lisp")
;;  (load "jb:k;quantum-map.lisp")
;;  (load "jb:k;memory-management.lisp")
;;  (load "jb:k;region-bits.lisp")
;;  (load "jb:k;region-data.lisp")
;;  (load "jb:k;map-fault.lisp")
;;  (load "jb:k;gc-fault.lisp")
;;  (load "jb:k;timers.lisp")
;;  (load "jb:k;area-data.lisp")
;;  (load "jb:k;nubus-interrupts.lisp")
;;  (load "jb:k;memory-interface.lisp")
;;  (load "jb:k;cons.lisp")
;;  (load "jb:k;symbols.lisp")
;;  (load "jb:k.array;array.lisp")

;;  (load "jb:kbug;new-spy-utilities.lisp")
;;  (load "jb:kbug2;common-definitions.lisp")
;;  (load "jb:kbug2;streams.lisp")
;;  (load "jb:kbug2;k2.lisp")
  (load "jb:k;mini-fasload-opcodes.lisp")
  (qc-file-load "jb:K;new-fasdump")
;;  (load "jb:k;boot.lisp")
  (qc-file-load "jb:k;defstruct")
  )


;(defsystem k-cold-load
;  (:name "K-COLD-LOAD")
;  (:pathname-default "jb:k;")
;  (:module goto-compiler-pe ("kbug;goto-compiler-pe"))
;  (:module global-registers ("global-registers"))
;  (:module illop            ("illop"))
;  (:module macros           ("primitive-setf" "k-macros"))
;  (:module trap             (("trap"            "trap.kbin")))
;  (:module conversions      (("conversions"     "conversions.kbin")))
;  (:module gc-ram           (("gc-ram"          "gc-ram.kbin")))
;  (:module nuclear-control  (("nuclear-control" "nuclear-control.kbin")))
;  (:module memory-map       (("memory-map"      "memory-map.kbin")))
;  (:module vmem             (("vmem"            "vmem.kbin")))
;  (:module quantum-map      (("quantum-map"     "quantum-map.kbin")))
;  (:module transporter-ram  (("transporter-ram" "transporter-ram.kbin")))
;  (:module timers           (("timers"          "timers.kbin")))
;  (:module pcd              (("pcd-table"       "pcd-table.kbin")))
;  (:module boot             (("boot"            "boot.kbin")))
;  (:readfile  goto-compiler-pe () always-load)
;  (:readfile  global-registers (:readfile goto-compiler-pe))
;  (:compile-load illop)
;  (:compile-load macros (:readfile goto-compiler-pe))
;  (:k-compile trap ((:readfile goto-compiler-pe global-registers)
;                   (:fasload illop macros)))
;  (:readfile trap (:readfile goto-compiler-pe))
;  (:k-compile conversions ((:readfile goto-compiler-pe)
;                          (:fasload macros)))
;  (:readfile conversions)
;  (:k-compile gc-ram ((:readfile goto-compiler-pe global-registers)
;                     (:fasload illop macros)
;                     (:readfile trap conversions)))
;  (:k-compile nuclear-control ((:readfile goto-compiler-pe global-registers)
;                              (:fasload illop macros)
;                              (:readfile trap conversions)))
;  (:readfile nuclear-control (:readfile goto-compiler-pe))
;  (:k-compile memory-map      ((:readfile goto-compiler-pe global-registers)
;                              (:fasload illop macros)
;                              (:readfile trap conversions nuclear-control)))
;  (:readfile memory-map)
;  (:k-compile vmem            ((:readfile goto-compiler-pe global-registers)
;                              (:fasload illop macros)
;                              (:readfile trap conversions nuclear-control memory-map)))
;  (:readfile vmem (:readfile goto-compiler-pe))
;  (:k-compile transporter-ram ((:readfile goto-compiler-pe global-registers)
;                              (:fasload illop macros)
;                              (:readfile trap conversions nuclear-control memory-map vmem)))
;  (:readfile transporter-ram)
;  (:k-compile timers ((:readfile goto-compiler-pe global-registers)
;                     (:fasload illop macros)
;                     (:readfile trap conversions nuclear-control memory-map vmem transporter-ram)))
;  (:readfile timers)
;  (:k-compile pcd       ((:readfile goto-compiler-pe global-registers)
;                              (:fasload illop macros)
;                              (:readfile trap conversions nuclear-control memory-map vmem
;                                         transporter-ram timers)))
;  (:readfile pcd)
;  (:k-compile boot            ((:readfile goto-compiler-pe global-registers)
;                              (:fasload illop macros)
;                              (:readfile trap conversions nuclear-control memory-map vmem
;                                         transporter-ram timers pcd))))

;; There is just no excuse for having a feature in the language
;; that destroys the ability to use forward references.

(defun ktest (&optional inhibit-cold-load?)
   (pkg-goto 'k-user)
 ;  (si::goto-package-environment "DEBUGGER")
  (unless inhibit-cold-load?
    (funcall (intern "MAKE-COLD-LOAD" "K-COLD")
             (symeval (intern "*COLD-FILES*"   "K-COLD"))
             (intern "COLD-BOOT-FUNCTION" "K-BOOT")))
  (funcall (intern "DOWNLOAD-COLD-LOAD" "K-KBUG"))
  (funcall (intern "PSEUDO-BOOT" "K-KBUG"))
  (funcall (intern "KBUG" "K-KBUG") nil (symeval (intern "*CODE-START*" "K-KBUG"))))
