;;; -*- Mode:LISP; Package:USER; Readtable:ZL; Base:10 -*-

;;;Pointers to Falcon system files are in SYS:SITE;FALCON.SYSTEM
;;;Translations for FALCON: host are in SYS:SITE;FALCON.TRANSLATIONS

;;The FALCON system is used for getting the most recent version of the
;;other systems.

(defsystem falcon
  (:name "FALCON")
  (:short-name "K")
  (:cts-controlled)                             ; $$$ Enforce CTS! <17-Nov-88 smh>
  (:pathname-default "falcon:k;")
  ;; List of modules in the debugger.
  (:module packages  "package-definitions")
  (:module k-imports "imported-syms")
  (:module illop     "illop")
  ;; $$$ added wimp-patch to remove make-system redef warning - smh&saz 24oct88
  (:module wimp      ("kbug2;wimp-terminal" "kbug2;wimp-patch"))        ;wimp-patch after wimp-terminal - smh 03nov88
  (:module loader    "loader")
  (:module zwei-commands "zwei-coms")

  (:readfile packages   ())
  (:readfile k-imports  ((:readfile packages)))
  (:compile-load zwei-commands ((:readfile packages) (:readfile k-imports)))
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
      (cerror t t t "Version ~S of the K (Falcon) system already exists" version))
    (fs:create-directory dir-name-for-creation)
    (fs:copy-directory "jb:k;" dir-name-for-creation :copy-only :newest :report-stream standard-output
                       :function #'(lambda (FROM TO &REST OPTIONS)
                                     (setq options (copy-list options))
                                     (apply #'fs:FS-COPY-FILE FROM TO
                                            (nconc OPTIONS '(:create-directory t :verbose verbose)))))
    t))

;;; $$$ Added this function <14-Nov-88 JIM>
;;; This function removes a K release from the file system.
(defun delete-k-system-release (host version &optional verbose)
  "To make this work, you will need to edit the logical pathname definitions above, in the copied sysdef file."
  (let* ((dir-name-for-deletion (format nil "~A:K-~A;*.*"          host version))
         (dir-name-for-probe    (format nil "~A:~~;K-~A.directory" host version)))
    (when (not (probe-file dir-name-for-probe))
      (cerror t t t "Version ~S of the K (Falcon) system does not exist" version))
    (delete-directory-recursive dir-name-for-deletion verbose)
    (if verbose
        (format t "~&Expunging ~A:~~;*.*#*." host))
    (fs:expunge-directory (format nil "~A:~~;*.*#*." host)))
  t)

;;; $$$ Added this function <14-Nov-88 JIM>
(defun delete-directory-recursive (dir-name verbose)
  (let* ((dir-list (fs:directory-list (send (send (send (fs:parse-pathname dir-name)
                                                       :new-name :wild)
                                                 :new-type :wild)
                                           :new-version :wild)))
         (dir-name (get (car dir-list) :pathname)))
    (do ((files (cdr dir-list) (cdr files)))
        ((null files) t)
      (delete-one-recursive (caar files) verbose))
    (if verbose
        (format t "~&Expunging ~A." dir-name))
    (fs:expunge-directory dir-name)
    (if verbose
        (format t "~&Deleting ~A." (send (send dir-name :directory-pathname-as-file)
                                         :new-version :newest)))
    (send (send (send dir-name :directory-pathname-as-file)
                :new-version :newest)
          :delete)
    ))

;;; $$$ Added this function <15-Nov-88 JIM>
(defun delete-one-recursive (file verbose)
  (if (let ((type (send file :type)))
        (and (stringp type)
             (string-equal type "DIRECTORY")))
      (let ((old-dir (send file :directory)))
        (if (nlistp old-dir)
            (setq old-dir `(,old-dir)))
        (delete-directory-recursive (send file :new-directory
                                          `(,@old-dir ,(send file :name)))
                                    verbose))
    (progn (if verbose
               (format t "~&Deleting ~A." file))
           (fs:delete-file file))))

(import '(si:atom si:signed-byte si:rplaca si:rplacd si:unspecial) 'lisp)
(export 'lisp:unspecial 'lisp)

(defsystem temp-hw
  (:pathname-default "falcon:k;")
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
  (:pathname-default "falcon:fleabit;")
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
  (:module setf-macros "falcon:k;new-setf-macros")
  (:module rewrite "rewrite")
  (:module rewriters "rewriters")
;  (:module type-pred "falcon:K;type-predicates")
  (:module types "falcon:K;types")
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
  (:pathname-default "falcon:fleabit.front-end;")
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
  (:pathname-default "falcon:FLEABIT.BACK-END;")
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
  (:pathname-default    "falcon:FLEABIT.GENERATE;")
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
  (:pathname-default  "falcon:k;")
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
  (:pathname-default "falcon:k;")

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
  (:pathname-default "falcon:kbug;")

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
  (:pathname-default "falcon:compiler;")
  ;;(:component-systems compiler)    ; let's not screw keith
  (:module cross     "cross")
  (:module p2-defs   "p2-defs")
  ;; $$$ added fasd-support <07-Nov-88 smh>
  (:module p2-stuff  ("p2-top" "p2-handlers" "p2-vars" "p2-frames" "p2-flow" "p2-support" "fasd-support"))
  (:module cross-definitions ("setf-methods" "basic-macros"))
  ;; ||| Added the unfasl module JIM 10/24/88
  (:module unfasl                    "falcon-unfasl")
  (:compile-load cross)
  ;; ||| Added transformation to load unfasl module JIM 10/24/88
  (:compile-load unfasl)
  (:compile-load p2-defs  (:fasload cross) (:fasload cross))
  (:compile-load p2-stuff (:fasload cross p2-defs) (:fasload cross p2-defs))
  (:cross-compile-load-falcon-environment cross-definitions))

;;; $$$ Added the bus-coupler system to falcon. <14-Nov-88 JIM>
(defsystem bus-coupler
  (:name "Bus Coupler")
  (:pathname-default "falcon:bus-coupler;")
  (:module bc  ("cd" "md" "mbc"))
  (:compile-load bc))

;;***************************************************************************************************

;;; LOAD-K-SYSTEM and friends moved to LOADER.LISP

;;***************************************************************************************************

(defparameter *cold-files-saved*                ; $$$ Changed so reset works better <22-Nov-88 wkf>
              '("falcon:cold;trap"
                "falcon:cold;trap-handlers"     ;;Used by both machines via "K-SYS:KBUG;DEBUG-SUPPORT" file
                "falcon:cold;nuclear-control"
                "falcon:cold;gc-ram"
                "falcon:cold;datatype-ram"      ; $$$  <22-Nov-88 wkf>
                "falcon:cold;memory-map"        ;; "falcon:cold;memory-map-use-nubus-memory-slot-a"
                "falcon:cold;vmem"
                "falcon:cold;transporter-ram"
                "falcon:cold;timers"
                "falcon:cold;pcd-table"
                "falcon:cold;quantum-map"
                "falcon:cold;memory-management"
                "falcon:cold;region-bits"
                "falcon:cold;map-fault"         ;;Not loaded into k-xxx for lambda
                "falcon:cold;region-data"
                "falcon:cold;gc-fault"
                "falcon:cold;area-data"
                "falcon:cold;nubus-interrupts"
                "falcon:cold;memory-interface"
                "falcon:cold;type-predicates"   ;;Not loaded into k-xxx for lambda
                "falcon:cold;cons"
                "falcon:cold;array"
                "falcon:cold;symbols"
                "falcon:cold;structure"         ;;Not loaded into k-xxx for lambda
                "falcon:cold;common-definitions";;Compiled for both machines
                "falcon:cold;streams"           ;;Compiled for both machines
                "falcon:cold;k2"                ;;Not loaded into k-xxx for lambda
                "falcon:cold;warm-loader"       ;;Not loaded into k-xxx for lambda
                "falcon:cold;lisp-internals"    ;;Not loaded into k-xxx for lambda
                "falcon:cold;error"             ;;Not loaded into k-xxx for lambda
                "falcon:cold;dt-ovf-trap"       ;;Not loaded into k-xxx for lambda
                "falcon:cold;boot"
                ))

(defparameter *cold-files* *cold-files-saved*)  ; $$$ Made resetable. <21-Nov-88 wkf>

(defun reset-cold-files ()
  (setq *cold-files* *cold-files-saved*))

(defparameter *warm-loaded-files-saved*
              ;; These are the files which are needed to get the package system and interpreter running.
              '("falcon:warm;generic"           ; $$$ Removed WARM-SYMBOLS <22-Nov-88 wkf>
                "falcon:warm;arithmetic"
                "falcon:warm;convert"
                "falcon:warm;fixnum"

                "falcon:warm;array2"
                "falcon:warm;character"
                "falcon:warm;string"

                "falcon:warm;lists"
                "falcon:warm;bald"
                "falcon:warm;nseq"

                "falcon:warm;equal"
                "falcon:warm;hash"

                "falcon:warm;throw"
                "falcon:warm;stack-groups"
                "falcon:warm;control-pdl"
                "falcon:warm;boot-stack-groups"
                "falcon:warm;package"
                "falcon:warm;warm-boot"

                "falcon:warm;vanilla-interpreter"
                "falcon:warm;defmacro"
                "falcon:warm;top-level-forms"

                "falcon:warm;miscellaneous-functions"

                ("falcon:warm;do" NIL)          ; don't load KENV file

                ))

(defparameter *warm-loaded-files* *warm-loaded-files-saved*)

(defun reset-warm-loaded-files ()
  (setq *warm-loaded-files* *warm-loaded-files-saved*))

(defparameter *hot-loaded-files-saved*
              ;;These are all the other K source files we want to load.
              '(
               ("falcon:hot;readtable" NIL)           ;;Dont't load KENV file
                "falcon:hot;reader"
               ("falcon:hot;high-level-streams" NIL)  ;;Dont't load KENV file
                "falcon:hot;printer"
                "falcon:hot;format"

                "falcon:hot;mini-lisp-listener"

;;              "falcon:hot;vcmem-driver"
;;              "falcon:hot;k-uc-tv"

                "falcon:hot;bignum"
                "falcon:hot;float"
                "falcon:hot;rational"
                "falcon:hot;complex"

;;              "falcon:hot;hot-boot"


                ;; With the way the SI: package and the LI: package interact in NEW-FASDUMP
                ;; the SI: functions in CROSS-SUPPORT are overwritting the LI: that already exist ..
                ;; Until this is straightened out we can't download CROSS-SUPPORT  --pfc

                ;; I've put back in a few like *PLUS which do not conflict.
                ;; The rest are commented out.  |||26sept88 pfc
                ("falcon:hot;cross-support" NIL)        ;functions in here exist mostly for cross compiler.

                "falcon:hot;fbin"
               ("falcon:hot;fbin-evals" NIL)     ;;Don't load KENV file
                ))

(defparameter *hot-loaded-files* *hot-loaded-files-saved*)

(defun reset-hot-loaded-files ()
  (setq *hot-loaded-files* *hot-loaded-files-saved*))

(defun reset-cold-warm-hot-files ()
  (reset-cold-files)
  (reset-warm-loaded-files)
  (reset-hot-loaded-files)
  t)

(defun remove-kenv-flags (file-list &aux answer)
  (dolist (file file-list (nreverse answer))
    (setq answer (cons (if (consp file) (car file) file)
                       answer))))


;;***************************************************************************************************
;;***         the following defsystems are currently useful only for tags search.                 ***
;;***************************************************************************************************

(defsystem k-diag
  (:name "K-DIAG")
  (:pathname-default "falcon:k;")
  (:module stuff ("kbug;spy-diags"
                  "falcon:k;test-strategy"              ;a document, not a program.
                  "falcon:kbug;test-vectors"    ;must be compiled in compiler hierarchy
                  "falcon:kbug;test-vectors-support"    ;requires manual compilation from editor buffer.
                  ))
  (:compile-load stuff)
  )

(defsystem k-cold-load
   (:name "K-COLD-LOAD")
   (:cts-controlled)
   (:pathname-default "falcon:k;")
   (:module cold-load #.*cold-files*)
   (:compile-load cold-load))

(defsystem k-warm-load
 ;useful only for tags search.
  (:name "K-WARM-LOAD")
  (:cts-controlled)
  (:pathname-default "falcon:k;")
  (:module stuff #.*warm-loaded-files*)
  (:compile-load stuff)
  )

(defsystem k-hot-load
 ;useful only for tags search
  (:name "K-HOT-LOAD")
  (:cts-controlled)
  (:pathname-default "falcon:k;")
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
  (:module stuff ("falcon:k;imported-syms"
                  "falcon:k;daisy-prom"
                  "falcon:k;daisy-sim"
                  "falcon:kbug2;wimp-terminal"
                  "falcon:k;lambda-to-k-streams"
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
  (:pathname-default "falcon:kdoc.falcon;")
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
  (:pathname-default "falcon:compiler;")
  ;; (:component-systems compiler)    ; let's not screw keith
  (:module cross     "cross")
  (:module p2-defs   "p2-defs")
  ;; $$$ added fasd-support <07-Nov-88 smh>
  (:module p2-stuff  ("p2-top" "p2-handlers" "p2-vars" "p2-frames" "p2-flow" "p2-support" "fasd-support"))
  (:module shared-stuff ("SYS: SYS; QCDEFS"
                         "SYS: SYS; QCFILE"
                         "SYS: SYS; QCP1"
                         "SYS: SYS; QCOPT"
                         "SYS: SYS; QCLUKE"))
  (:module cross-definitions ("setf-methods" "basic-macros"))
  (:compile-load cross)
  (:compile-load p2-defs  (:fasload cross) (:fasload cross))
  (:compile-load p2-stuff (:fasload cross p2-defs) (:fasload cross p2-defs))
  (:compile-load shared-stuff)
  (:compile-load cross-definitions))

;;; $$$ Added the bus-coupler system <14-Nov-88 JIM>
(defsystem k-everything
  ;this should include every single file in any way connected with K.
  ;useful for tags search.
  (:name "K-EVERYTHING")
  (:cts-controlled)                             ; $$$ Added cts-controlled throughout. <22-Nov-88 wkf>
  (:component-systems k k-runtime bus-coupler compiler-for-k
                      fleabit cross-compiler-for-tags-search k-misc k-documentation))
