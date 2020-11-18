;;; -*- Mode:LISP; Package:USER; Base:10 -*-

(defsystem steve
  (:pathname-default "dj:gjcx.steve;")
  (:MODULE PATCHES "LISPM-PATCHES")
  (:module nilcompat "nilcompat")
  (:module vars "vars")
  (:module em "em")
  (:module newmacros "newmacros")
  (:module aux "aux")
  (:module over "over")
  (:module ed "ed")
  (:module prll "prll")
  (:module redis "redis")
  (:module hashish "hashish")
  (:module mode "mode")
  (:module hash "hash")
  (:module initbinds "initbinds")
  (:module scan "scan")
  (:module metax "metax")
  (:module modes "modes")
  (:module echo "echo")
  (:module edio "edio")
  (:module recurse "recurse")
  (:module sexp "sexp")
  (:module args "args")
  (:module edkeys "edkeys")
  (:module keymac "keymac")
  (:module comment "comment")
  (:module kills "kills")
  (:module indent "indent")
  (:module mxcom "mxcom")
  (:module editopt "editopt")
  (:module hashini "hashini")
  (:module syn "syn")
  (:module autofill "autofill")
  (:module dow "dow")
  (:module search "search")
  (:module xsearch "xsearch")
  (:module defsrch "defsrch")
  (:module help "help")
  (:module dired "dired")
  (:module sm "sm")

  (:COMPILE-LOAD PATCHES)

  (:compile-load nilcompat (:FASLOAD PATCHES) (:FASLOAD PATCHES))
  (:compile-load vars (:fasload nilcompat) (:fasload nilcompat))
  (:compile-load em (:fasload vars) (:fasload vars))
  (:compile-load newmacros (:fasload em) (:fasload em))
  (:compile-load aux (:fasload newmacros) (:fasload newmacros))
  (:compile-load over (:fasload aux) (:fasload aux))
  (:compile-load ed (:fasload over) (:fasload over))
  (:compile-load prll (:fasload ed) (:fasload ed))
  (:compile-load redis (:fasload prll) (:fasload prll))
  (:compile-load hashish (:fasload redis) (:fasload redis))
  (:compile-load mode (:fasload hashish) (:fasload hashish))
  (:compile-load hash (:fasload mode) (:fasload mode))
  (:compile-load initbinds (:fasload hash) (:fasload hash))
  (:compile-load scan (:fasload initbinds) (:fasload initbinds))
  (:compile-load metax (:fasload scan) (:fasload scan))
  (:compile-load modes (:fasload metax) (:fasload metax))
  (:compile-load echo (:fasload modes) (:fasload modes))
  (:compile-load edio (:fasload echo) (:fasload echo))
  (:compile-load recurse (:fasload edio) (:fasload edio))
  (:compile-load sexp (:fasload recurse) (:fasload recurse))
  (:compile-load args (:fasload sexp) (:fasload sexp))
  (:compile-load edkeys (:fasload args) (:fasload args))
  (:compile-load keymac (:fasload edkeys) (:fasload edkeys))
  (:compile-load comment (:fasload keymac) (:fasload keymac))
  (:compile-load kills (:fasload comment) (:fasload comment))
  (:compile-load indent (:fasload kills) (:fasload kills))
  (:compile-load mxcom (:fasload indent) (:fasload indent))
  (:compile-load editopt (:fasload mxcom) (:fasload mxcom))
  (:compile-load hashini (:fasload editopt) (:fasload editopt))
  (:compile-load syn (:fasload hashini) (:fasload hashini))
  (:compile-load autofill (:fasload syn) (:fasload syn))
  (:compile-load dow (:fasload autofill) (:fasload autofill))
  (:compile-load search (:fasload dow) (:fasload dow))
  (:compile-load xsearch (:fasload search) (:fasload search))
  (:compile-load defsrch (:fasload xsearch) (:fasload xsearch))
  (:compile-load help (:fasload defsrch) (:fasload defsrch))
  (:compile-load dired (:fasload help) (:fasload help))
  (:compile-load sm (:fasload dired) (:fasload dired))

  )

#||

nil$disk:[nil.steve]steve
nil$disk:[nil.steve]edboot
nil$disk:[nil.steve]vars
nil$disk:[nil.steve]em
nil$disk:[nil.steve]newmacros
nil$disk:[nil.steve]aux
nil$disk:[nil.steve]over
nil$disk:[nil.steve]ed
nil$disk:[nil.steve]prll
nil$disk:[nil.steve]redis
nil$disk:[nil.steve]hashish
nil$disk:[nil.steve]mode
nil$disk:[nil.steve]hash
nil$disk:[nil.steve]initbinds
nil$disk:[nil.steve]scan
nil$disk:[nil.steve]metax
nil$disk:[nil.steve]modes
nil$disk:[nil.steve]echo
nil$disk:[nil.steve]edio
nil$disk:[nil.steve]recurse
nil$disk:[nil.steve]sexp
nil$disk:[nil.steve]args
nil$disk:[nil.steve]edkeys
nil$disk:[nil.steve]keymac
nil$disk:[nil.steve]comment
nil$disk:[nil.steve]kills
nil$disk:[nil.steve]indent
nil$disk:[nil.steve]mxcom
nil$disk:[nil.steve]editopt
nil$disk:[nil.steve]hashini
nil$disk:[nil.steve]syn
nil$disk:[nil.steve]autofill
nil$disk:[nil.steve]dow
nil$disk:[nil.steve]search
nil$disk:[nil.steve]xsearch
nil$disk:[nil.steve]defsrch
nil$disk:[nil.steve]help
nil$disk:[nil.steve]dired
nil$disk:[nil.steve]sm
nil$disk:[nil.vas]pkgusr

||#
