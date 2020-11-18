;;; -*- Mode:LISP; Package:USER; Readtable:ZL; Base:10 -*-

(fs:set-logical-pathname-host
  "K-SYS"
  :physical-host "JB"
  :translations
  '(("k;"           "<k>")
    ("k.*;"         "<k.*>")
    ("k.*.*;"       "<k.*.*>")
    ("kbug;"        "<k.kbug>")
    ("kbug2;"       "<k.kbug2>")
    ("kdoc;"        "<k.kdoc>")
    ("kdoc.*;"      "<k.kdoc.*>")
    ("kdoc.*.*;"    "<k.kdoc.*.*>")
    ("fleabit;"     "<k.fleabit>")
    ("fleabit.*;"   "<k.fleabit.*>")
    ("fleabit.*.*;" "<k.fleabit.*.*>")
    ("compiler"     "<k.compiler>")
    ("bus-coupler"  "<k.bus-coupler>")
    ("cold"         "<k.cold>")
    ("warm"         "<k.warm>")
    ("hot"          "<k.hot>")))

(defsystem k   ;;Used for getting the most recent version of this file.
  (:name "K")
  (:pathname-default "k-sys:k;")
  ;; List of modules in the debugger.
  (:module packages  "package-definitions")
  (:module k-imports "imported-syms")
  (:module illop     "illop")
  (:module wimp      "kbug2;wimp-terminal")
  (:module loader    "loader")
  (:readfile packages   ())
  (:readfile k-imports  ((:readfile packages)))
  (:compile-load illop  ((:readfile packages) (:readfile k-imports))
                        ((:readfile packages) (:readfile k-imports)))
  (:compile-load wimp   ((:readfile packages) (:readfile k-imports) (:fasload illop))
                        ((:readfile packages) (:readfile k-imports) (:fasload illop)))
  (:compile-load loader ((:readfile packages) (:readfile k-imports) (:fasload illop) (:fasload wimp))
                        ((:readfile packages) (:readfile k-imports) (:fasload illop) (:fasload wimp))))

(defun make-k-system-release (host version &optional verbose)
  "To make this work, you will need to edit the logical pathname definitions above, in the copied sysdef file."
  (let* ((dir-name-for-creation (format nil "~A:K-~A;*.*"          host version))
         (dir-name-for-probe    (format nil "~A:~~;K-~A.directory" host version)))
    (when (probe-file dir-name-for-probe)
      (cerror t t t "Version ~S of the K system already exists" version))
    (fs:create-directory dir-name-for-creation)
    (fs:copy-directory "jb:k;" dir-name-for-creation :copy-only :newest :report-stream standard-output
                       :function #'(lambda (FROM TO &REST OPTIONS)
                                     (setq options (copy-list options))
                                     (apply #'fs:FS-COPY-FILE FROM TO
                                            (nconc OPTIONS '(:create-directory t :verbose verbose)))))
    t))



(import '(si:atom si:signed-byte si:rplaca si:rplacd si:unspecial) 'lisp)
(export 'lisp:unspecial 'lisp)

(defsystem temp-hw
  (:pathname-default "k-sys:k;")
  ;(:module pack "k-system-packages")
  ;; Setf needs to be loaded before CS primitives.
  (:module nmacros "nmacros")
  (:module new-setf ("lambda-list" "new-setf"))
  (:module prims "cs-primitives")
  (:module imports "li-imports")
  (:module fakes "li-fakes")
  (:module hs "compiler-support")
  (:module hw ("hardware-constants" "instructions" "alu-opcodes" "floating-point-opcodes"))
  (:module vinc ("firm-definitions" "data-types"))
  ;(:readfile pack)
  (:compile-load nmacros)
  (:compile-load new-setf )             ;(:readfile pack)
  (:compile-load prims (:fasload new-setf nmacros) (:fasload new-setf nmacros)) ;(:readfile pack)
  (:readfile imports ((:fasload prims)))
  (:readfile fakes ((:fasload new-setf) (:fasload prims)))      ;(:readfile pack)
  (:readfile hs ((:fasload new-setf) (:fasload prims))) ;(:readfile pack)
  (:readfile hw ((:fasload new-setf) (:fasload prims) (:readfile hs)))  ;(:readfile pack)
  (:readfile vinc ((:fasload  new-setf) (:fasload prims) (:readfile hs hw))))   ;(:readfile pack)


(defsystem fleabit-top
  (:pathname-default "k-sys:fleabit;")
  (:module extra "extra-stuff")
  (:module vect "expanding-vector")
  (:module util "top-util")
  (:module defs "top-defs")
  (:module processor-defs "processor-defs")
  (:module primop-defs "primop-defs")
  (:module primitives ("primitives" "hw-primitives"))
  (:module env "compiler-env")
  (:module decl "decl")
  (:module new-macros "new-macros")
  (:module setf-macros "k-sys:k;new-setf-macros")
  (:module rewrite "rewrite")
  (:module rewriters "rewriters")
;  (:module type-pred "k-sys:K;type-predicates")
  (:module types "k-sys:K;types")
  (:module hw-macros "hw-macros")
  (:module top "top")
  (:compile-load extra)
  (:compile-load vect (:fasload extra))
  (:compile-load util (:fasload extra) (:fasload extra))
  (:compile-load defs (:fasload util vect extra) (:fasload util vect extra))
  (:compile-load processor-defs (:readfile (temp-hw hw vinc))           ;pack
                                (:readfile (temp-hw hw vinc)))          ;pack
  (:compile-load primop-defs (:fasload defs util) (:fasload defs util))
  (:compile-load primitives (:fasload primop-defs processor-defs)
                            (:fasload primop-defs processor-defs))
  (:compile-load env (:fasload extra util) (:fasload extra util))
  (:compile-load decl)
  (:compile-load new-macros ((:readfile (temp-hw hw)) (:fasload (temp-hw nmacros) defs env decl))
                         ((:readfile (temp-hw hw)) (:fasload  (temp-hw nmacros) defs env decl)))
  (:compile-load setf-macros (:fasload (temp-hw new-setf))
                             ((:fasload (temp-hw prims)) (:fasload (temp-hw new-setf))))
  (:compile-load rewrite (:fasload defs util new-macros)
                         (:fasload defs util new-macros))
  (:compile-load rewriters (:fasload defs util new-macros rewrite)
                           (:fasload defs util new-macros rewrite))
;  (:readfile type-pred (:readfile (temp-hw fakes)))
  (:compile-load types (:readfile (temp-hw fakes))
                       (:readfile (temp-hw fakes)))
  (:compile-load hw-macros ((:readfile (temp-hw hw)) (:fasload defs new-macros))
                 ((:readfile (temp-hw hw))(:fasload defs new-macros)))
  (:compile-load top (:fasload extra env decl) (:fasload extra env decl)))

(defsystem fleabit-front-end
  (:pathname-default "k-sys:fleabit.front-end;")
  (:module type "type")
;  (:module envs "envs")
  (:module nodestuff "nodestuff")
  (:module front "nfront")
  (:module alpha "alpha")
  (:module compilators "compilators")
  (:module node "node")
  (:module simplify ("simplify" "simplify-call" "simplify-let" "nsimplify-y" "simplifiers" "param"))
;  (:module gen-interface "gen-interface")
;  (:module fixup "fixup")
  (:module module "module")
;  (:module analyze "analyze")
  (:compile-load type  (:fasload (fleabit-top defs)) (:fasload (fleabit-top defs)))
;  (:compile-load envs  (:fasload (fleabit-top defs)) (:fasload (fleabit-top defs)))
                          ;(:fasload envs) (:fasload envs))
  (:compile-load nodestuff (:fasload (fleabit-top defs util)) (:fasload (fleabit-top defs)))
                          ;(:fasload envs) (:fasload envs))
  (:compile-load front   (:fasload (fleabit-top defs util)) (:fasload (fleabit-top defs)))
  (:compile-load alpha (:fasload (fleabit-top defs env rewrite new-macros))
                       (:fasload (fleabit-top defs env rewrite new-macros)))
  (:compile-load compilators (:fasload alpha) (:fasload alpha))
  (:compile-load node (:fasload compilators front))
  (:compile-load simplify  (:fasload (fleabit-top defs)) (:fasload (fleabit-top defs)))
;  (:compile-load gen-interface (:fasload (fleabit-top defs) envs) (:fasload (fleabit-top defs) envs))
;  (:compile-load fixup  (:fasload (fleabit-top defs)) (:fasload (fleabit-top defs)))
;  (:compile-load module (:fasload alpha front) (:fasload alpha))
;  (:compile-load analyze (:fasload (fleabit-top defs) front) (:fasload (fleabit-top defs)))
)

(defsystem fleabit-back-end
  (:pathname-default "k-sys:FLEABIT.BACK-END;")
  (:module strategy  "STRATEGY")
  (:module live      "LIVE")
  (:module trace     "TRACE")
; (:module close     "CLOSE-ANALYZE")
  (:module env       "ENV-ANALYZE")
  (:compile-load strategy (:fasload (fleabit-top defs primitives)) (:fasload (fleabit-top defs primitives)))
  (:compile-load live (:fasload (fleabit-top defs) strategy) (:fasload (fleabit-top defs) strategy))
  (:compile-load trace (:fasload (fleabit-top defs primitives)) (:fasload (fleabit-top defs primitives)))
; (:compile-load close (:fasload (fleabit-top defs) strategy) (:fasload (fleabit-top defs) strategy))
  (:compile-load env (:fasload (fleabit-top defs) strategy) (:fasload (fleabit-top defs) strategy)))


(defsystem fleabit-generate
  (:pathname-default    "k-sys:FLEABIT.GENERATE;")
  (:module emit         "EMIT")
  (:module reg          "REG-ALLOC")
  (:module generate     "GENERATE")
  (:module assemble     "ASSEM")
  (:module disassemble ("DIS" "LINKER")) ;"LOADER"
  (:compile-load emit ((:fasload (fleabit-top defs util processor-defs))
                       (:readfile (temp-hw hw)))                ;pack
                      ((:fasload (fleabit-top defs processor-defs))
                       (:readfile (temp-hw hw))))               ;pack
  (:compile-load reg
                 (:fasload (fleabit-top defs util processor-defs) (fleabit-back-end strategy) emit)
                 (:fasload (fleabit-top defs processor-defs)      (fleabit-back-end strategy) emit))
  (:compile-load generate
                 (:fasload (fleabit-top util processor-defs) emit reg)
                 (:fasload (fleabit-top processor-defs)      emit reg))
  (:compile-load assemble
                 ((:fasload (fleabit-top defs processor-defs))
                  (:readfile (temp-hw hw)))
                 ((:fasload (fleabit-top defs processor-defs))
                  (:readfile (temp-hw hw))))
  (:compile-load-init disassemble (assemble)
                      ((:fasload (fleabit-top defs processor-defs))
                       (:fasload assemble)
                       (:readfile (temp-hw hw)))
                      ((:fasload (fleabit-top defs processor-defs))
                       (:fasload assemble)
                       (:readfile (temp-hw hw)))))

(defsystem compiler-crock            ;;Could we merge this into TEMP-HW?
  (:pathname-default  "k-sys:k;")
  (:module crock      "compiler-crock0")
  (:readfile crock    ((:fasload (fleabit-top defs env rewrite new-macros)
                                 (fleabit-front-end node)
                                 (fleabit-back-end env)
                                 (fleabit-generate disassemble))
                       (:readfile (temp-hw vinc hw)))))

(defsystem fleabit
  (:component-systems temp-hw fleabit-top fleabit-front-end fleabit-back-end fleabit-generate compiler-crock))


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
  )

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
  (:module debug-crock            "debug-crock0")
  (:module debug-macros           "k;k-macros-for-k-debugger")  ;;wkf this is compiled in COMPILER-FOR-K make-system.
  (:module hw-constants           "k;hardware-constants")
  (:module spy-utilities          ("new-spy-utilities" "spy-diags"))
  (:module debug-support          "debug-support")
  (:module constants              ("k;instructions"  ;this is readfile'd
                                   "k;data-types" "k;firm-definitions"
                                   "k;alu-opcodes"
                                   "k;conversions-for-k-debugger"))
  (:module global-registers       "k;global-registers-for-k-debugger")

    ;following module contains code for both machines.  Compile it for the lambda as well as K.
  (:module both-stuff-1           ("cold;common-definitions"))
  (:module both-stuff-2           ("cold;streams"                       ;has compile-in-roots.
                                   "k;new-fasdump-for-k-debugger"))

;;We are only loading the cold files here to get defconstants into k-xxx pkgs on lambda for kold-loader.
;;Fix this by moving the defconstants to hardware-constants.  If we want these constants on K put constants onto
;;*warm-eval-list* and make hot-boot work.   --wkf  @@@ +++

  (:module k-stuff                ("cold;trap"              "cold;trap-handlers"  "cold;gc-ram"           "cold;datatype-ram"
                                   "cold;nuclear-control"   "cold;memory-map"     "cold;vmem"             "cold;quantum-map"
                                   "cold;transporter-ram"   "cold;timers"         "cold;pcd-table"
                                   "cold;memory-management" "cold;region-bits"    "cold;region-data"
                                   "cold;gc-fault.lisp"     "cold;area-data.lisp" "cold;nubus-interrupts"
                                   "cold;memory-interface"  "cold;cons"           "cold;symbols"          "cold;array"
                                   "cold;boot"
                                   "k;mini-fasload-opcodes-for-k-debugger"
                                   ))
  (:module kold-loader            "k;kold-loader")
  (:module lambda-stuff-2         ("kbug;kbug"          "kbug2;kbug2"
                                   "Kbug2;kbug-generic" "k;warm-files"))

  (:compile-load  debug-board)
  (:compile-load  debug-crock   (:fasload debug-board )
                                (:fasload debug-board ))
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
  (:readfile      constants     ((:fasload debug-crock debug-support)  ;;might be screwed by changes to (debug-macros)
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
  (:readfile      k-stuff       ((:fasload debug-crock debug-support)  ;;might be screwed by changes to (debug-macros)
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


(defsystem cross-compiler-for-k
  (:name             "K-cross-compiler")  ;;Use the Lambda compiler with new p2.
  (:pathname-default "k-sys:compiler;")
  (:component-systems compiler)
  (:module cross     "cross")
  (:module p2-defs   "p2-defs")
  (:module p2-stuff  ("p2-top" "p2-handlers" "p2-vars" "p2-frames" "p2-flow" "p2-support"))
  (:compile-load cross)
  (:compile-load p2-defs  (:fasload cross) (:fasload cross))
  (:compile-load p2-stuff (:fasload cross p2-defs) (:fasload cross p2-defs)))



;;;****************************************************************

;;; LOAD-K-SYSTEM and friends moved to LOADER.LISP

;;***************************************************************************************************

(defparameter *cold-files*
              '("k-sys:cold;trap"
                "k-sys:cold;trap-handlers"
                "k-sys:cold;nuclear-control"
                "k-sys:cold;gc-ram"
                "k-sys:cold;datatype-ram"
                "k-sys:cold;memory-map"
                "k-sys:cold;vmem"
                "k-sys:cold;transporter-ram"
                "k-sys:cold;timers"
                "k-sys:cold;pcd-table"
                "k-sys:cold;quantum-map"
                "k-sys:cold;memory-management"
                "k-sys:cold;region-bits"
                "k-sys:cold;map-fault"          ;;Not loaded into k-xxx for lambda
                "k-sys:cold;region-data"
                "k-sys:cold;gc-fault"
                "k-sys:cold;area-data"
                "k-sys:cold;nubus-interrupts"
                "k-sys:cold;memory-interface"
                "k-sys:cold;type-predicates"    ;;Not loaded into k-xxx for lambda
                "k-sys:cold;cons"
                "k-sys:cold;array"
                "k-sys:cold;symbols"
                "k-sys:cold;structure"          ;;Not loaded into k-xxx for lambda
                "k-sys:cold;common-definitions" ;;Compiled for both machines
                "k-sys:cold;streams"            ;;Compiled for both machines
                "k-sys:cold;k2"                 ;;Not loaded into k-xxx for lambda
                "k-sys:cold;warm-loader"        ;;Not loaded into k-xxx for lambda
                "k-sys:cold;lisp-internals"     ;;Not loaded into k-xxx for lambda
                "k-sys:cold;error"              ;;Not loaded into k-xxx for lambda
                "k-sys:cold;dt-ovf-trap"        ;;Not loaded into k-xxx for lambda
                "k-sys:cold;boot"
                ))

(defparameter *warm-loaded-files*
              ;; These are the files which are needed to get the package system and interpreter running.
              '("k-sys:warm;generic"
                "k-sys:warm;arithmetic"
                "k-sys:warm;convert"
                "k-sys:warm;fixnum"

                "k-sys:warm;array2"
                "k-sys:warm;character"
                "k-sys:warm;string"

                "k-sys:warm;lists"
                "k-sys:warm;bald"
                "k-sys:warm;nseq"

                "k-sys:warm;equal"
                "k-sys:warm;hash"

                "k-sys:warm;throw"
                "k-sys:warm;stack-groups"
                "k-sys:warm;control-pdl"
                "k-sys:warm;boot-stack-groups"
                "k-sys:warm;package"
                "k-sys:warm;warm-boot"

                "k-sys:warm;vanilla-interpreter"
                "k-sys:warm;defmacro"
                "k-sys:warm;top-level-forms"

                "k-sys:warm;miscellaneous-functions"

                ("k-sys:warm;do" NIL)           ; don't load KENV file

                ))

(defparameter *hot-loaded-files*
              ;;These are all the other K source files we want to load.
              '(
               ("k-sys:hot;readtable" NIL)           ;;Dont't load KENV file
                "k-sys:hot;reader"
               ("k-sys:hot;high-level-streams" NIL)  ;;Dont't load KENV file
                "k-sys:hot;printer"
                "k-sys:hot;format"

                "k-sys:hot;mini-lisp-listener"

;;              "k-sys:hot;vcmem-driver"
;;              "k-sys:hot;k-uc-tv"

                "k-sys:hot;bignum"
                "k-sys:hot;float"
                "k-sys:hot;rational"
                "k-sys:hot;complex"

;;              "k-sys:hot;hot-boot"


                ;; With the way the SI: package and the LI: package interact in NEW-FASDUMP
                ;; the SI: functions in CROSS-SUPPORT are overwritting the LI: that already exist ..
                ;; Until this is straightened out we can't download CROSS-SUPPORT  --pfc
;;              "k-sys:hot;cross-support"       ;functions in here exist mostly for cross compiler.

                "k-sys:hot;fbin"
               ("k-sys:hot;fbin-evals" NIL)     ;;Don't load KENV file
                ))

(defun remove-kenv-flags (file-list &aux answer)
  (dolist (file file-list (nreverse answer))
    (setq answer (cons (if (consp file) (car file) file)
                       answer))))


;;***************************************************************************************************
;;***         the following defsystems are currently useful only for tags search.                 ***
;;***************************************************************************************************

(defsystem k-diag
  (:name "K-DIAG")
  (:pathname-default "k-sys:k;")
  (:module stuff ("kbug;spy-diags"
                  "k-sys:k;test-strategy"               ;a document, not a program.
                  "k-sys:kbug;test-vectors"     ;must be compiled in compiler hierarchy
                  "k-sys:kbug;test-vectors-support"     ;requires manual compilation from editor buffer.
                  ))
  (:compile-load stuff)
  )

(defsystem k-cold-load
   (:name "K-COLD-LOAD")
   (:pathname-default "k-sys:k;")
   (:module cold-load #.*cold-files*)
   (:compile-load cold-load))

(defsystem k-warm-load
 ;useful only for tags search.
  (:name "K-WARM-LOAD")
  (:pathname-default "k-sys:k;")
  (:module stuff #.*warm-loaded-files*)
  (:compile-load stuff)
  )

(defsystem k-hot-load
 ;useful only for tags search
  (:name "K-HOT-LOAD")
  (:pathname-default "k-sys:k;")
  (:module stuff #.(remove-kenv-flags *hot-loaded-files*))
  (:compile-load stuff)
  )

(defsystem k-code
  ;useful only for tags search
  (:name "K-CODE")
  (:component-systems k-cold-load k-warm-load k-hot-load))


(defsystem k-misc
 ;useful only for tagsearch.
  (:name "K-MISC")
  (:module stuff ("k-sys:k;imported-syms"
                  "k-sys:k;daisy-prom"
                  "k-sys:k;daisy-sim"
                  "k-sys:kbug2;wimp-terminal"
                  "k-sys:k;lambda-to-k-streams"
                  ))
  (:compile-load stuff)
  )

(defsystem k-runtime
  (:name "K-RUNTIME")
  (:component-systems k-cold-load k-warm-load k-hot-load k-debugger k-diag k)
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

(defsystem cross-compiler-for-tags-search
  (:name             "K-cross-compiler-for-tag-search")  ;;Use the Lambda compiler with new p2.
  (:pathname-default "k-sys:compiler;")
  ;; (:component-systems compiler)
  (:module cross     "cross")
  (:module p2-defs   "p2-defs")
  (:module p2-stuff  ("p2-top" "p2-handlers" "p2-vars" "p2-frames" "p2-flow" "p2-support"))
  (:module shared-stuff ("SYS: SYS; QCDEFS"
                         "SYS: SYS; QCFILE"
                         "SYS: SYS; QCP1"
                         "SYS: SYS; QCOPT"
                         "SYS: SYS; QCLUKE"))
  (:compile-load cross)
  (:compile-load p2-defs  (:fasload cross) (:fasload cross))
  (:compile-load p2-stuff (:fasload cross p2-defs) (:fasload cross p2-defs))
  (:compile-load shared-stuff))

(defsystem k-everything
  ;this should include every single file in any way connected with K.
  ;useful for tags search.
  (:name "K-EVERYTHING")
  (:component-systems k k-runtime compiler-for-k fleabit cross-compiler-for-tags-search k-misc k-documentation))
