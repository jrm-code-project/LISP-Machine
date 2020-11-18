
;System FED made by pld at 9-Jul-88 16:51:14  -*-Mode: Lisp; Package: User; Base: 10-*-
;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: WINDOW; IMAGE-TOOLS LISP >"
  '((:COMPILE NIL
     (FED::ROLLTST NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." COLOR:COLOR-SCREEN))
     (FED::TST NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." COLOR:COLOR-SCREEN)))))

;-*-Mode: Lisp; Package: User; Base: 10. -*-
(SI:RELOAD-FILE-WARNINGS
  '#FS::LOGICAL-PATHNAME "SYS: WINDOW; FED LISP >"
  '((:COMPILE NIL
     ((:METHOD FED::GRID-MIXIN :MOUSE-BOOLE-SQUARES) NIL NIL
      (COMPILER::INSTANCE-VARIABLE-USED-IN-INTERNAL-LAMBDA :UNIMPLEMENTED NIL "~The ~:[~;special ~]instance variable ~S of flavor ~S~@
                                is being referenced by a lexically closed-over function.~@
                                This will not work outside of the dynamic scope of ~S.~" NIL FED::BOX-Y-SIZE FED::GRID-MIXIN SELF)
      (COMPILER::INSTANCE-VARIABLE-USED-IN-INTERNAL-LAMBDA :UNIMPLEMENTED NIL "~The ~:[~;special ~]instance variable ~S of flavor ~S~@
                                is being referenced by a lexically closed-over function.~@
                                This will not work outside of the dynamic scope of ~S.~" NIL TV:TOP-MARGIN-SIZE FED::GRID-MIXIN SELF)
      (COMPILER::INSTANCE-VARIABLE-USED-IN-INTERNAL-LAMBDA :UNIMPLEMENTED NIL "~The ~:[~;special ~]instance variable ~S of flavor ~S~@
                                is being referenced by a lexically closed-over function.~@
                                This will not work outside of the dynamic scope of ~S.~" NIL FED::BOX-X-SIZE FED::GRID-MIXIN SELF)
      (COMPILER::INSTANCE-VARIABLE-USED-IN-INTERNAL-LAMBDA :UNIMPLEMENTED NIL "~The ~:[~;special ~]instance variable ~S of flavor ~S~@
                                is being referenced by a lexically closed-over function.~@
                                This will not work outside of the dynamic scope of ~S.~" NIL TV:LEFT-MARGIN-SIZE FED::GRID-MIXIN SELF)
      (COMPILER::INSTANCE-VARIABLE-USED-IN-INTERNAL-LAMBDA :UNIMPLEMENTED NIL "~The ~:[~;special ~]instance variable ~S of flavor ~S~@
                                is being referenced by a lexically closed-over function.~@
                                This will not work outside of the dynamic scope of ~S.~" NIL FED::WINDOW-Y-SIZE FED::GRID-MIXIN SELF)
      (COMPILER::INSTANCE-VARIABLE-USED-IN-INTERNAL-LAMBDA :UNIMPLEMENTED NIL "~The ~:[~;special ~]instance variable ~S of flavor ~S~@
                                is being referenced by a lexically closed-over function.~@
                                This will not work outside of the dynamic scope of ~S.~" NIL FED::WINDOW-X-SIZE FED::GRID-MIXIN SELF)
      (COMPILER::INSTANCE-VARIABLE-USED-IN-INTERNAL-LAMBDA :UNIMPLEMENTED NIL "~The ~:[~;special ~]instance variable ~S of flavor ~S~@
                                is being referenced by a lexically closed-over function.~@
                                This will not work outside of the dynamic scope of ~S.~" NIL FED::WINDOW-ARRAY FED::GRID-MIXIN SELF)
      (COMPILER::SELF-USED-IN-INTERNAL-LAMBDA :UNIMPLEMENTED NIL "~~S is being referenced by a lexically closed-over function.~@
                        This will not, of course, work outside of the dynamic scope of ~S.~" SELF SELF))
     (FED::REDISPLAY-LABELS NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." FED::FED-EDITED-CHARS))
     ((:METHOD FED:FED-FRAME :AFTER :INIT) NIL NIL
      (COMPILER:UNDEFINED-FUNCTION-USED :PROBABLE-ERROR NIL "The undefined function ~S was called" COLOR:COLOR-EXISTS-P)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." FED::*COLOR-CHAR-BOX-AND-LINE*))
     ((:METHOD FED :COMMAND-LOOP) NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." FED::COMMAND-TABLE))
     ((:METHOD FED::BASIC-COLOR-CHAR-BOX-AND-LINE :CHECK-P) NIL NIL
      (COMPILER::INSTANCE-VARIABLE-BOUND :IMPLAUSIBLE NIL "Binding ~S, which has the same name as an instance variable of flavor ~S" FED::CENTER-X FED::BASIC-COLOR-CHAR-BOX-AND-LINE)
      (COMPILER::INSTANCE-VARIABLE-BOUND :IMPLAUSIBLE NIL "Binding ~S, which has the same name as an instance variable of flavor ~S" FED::CENTER-Y FED::BASIC-COLOR-CHAR-BOX-AND-LINE)
      (COMPILER::INSTANCE-VARIABLE-BOUND :IMPLAUSIBLE NIL "Binding ~S, which has the same name as an instance variable of flavor ~S" FED::ANGLE FED::BASIC-COLOR-CHAR-BOX-AND-LINE)
      (COMPILER::INSTANCE-VARIABLE-BOUND :IMPLAUSIBLE NIL "Binding ~S, which has the same name as an instance variable of flavor ~S" FED::SCALE FED::BASIC-COLOR-CHAR-BOX-AND-LINE)
      (COMPILER::INSTANCE-VARIABLE-BOUND :IMPLAUSIBLE NIL "Binding ~S, which has the same name as an instance variable of flavor ~S" FED::BOX-TL-X FED::BASIC-COLOR-CHAR-BOX-AND-LINE)
      (COMPILER::INSTANCE-VARIABLE-BOUND :IMPLAUSIBLE NIL "Binding ~S, which has the same name as an instance variable of flavor ~S" FED::BOX-TL-Y FED::BASIC-COLOR-CHAR-BOX-AND-LINE)
      (COMPILER::INSTANCE-VARIABLE-BOUND :IMPLAUSIBLE NIL "Binding ~S, which has the same name as an instance variable of flavor ~S" FED::BOX-TR-X FED::BASIC-COLOR-CHAR-BOX-AND-LINE)
      (COMPILER::INSTANCE-VARIABLE-BOUND :IMPLAUSIBLE NIL "Binding ~S, which has the same name as an instance variable of flavor ~S" FED::BOX-TR-Y FED::BASIC-COLOR-CHAR-BOX-AND-LINE)
      (COMPILER::INSTANCE-VARIABLE-BOUND :IMPLAUSIBLE NIL "Binding ~S, which has the same name as an instance variable of flavor ~S" FED::BOX-BL-X FED::BASIC-COLOR-CHAR-BOX-AND-LINE)
      (COMPILER::INSTANCE-VARIABLE-BOUND :IMPLAUSIBLE NIL "Binding ~S, which has the same name as an instance variable of flavor ~S" FED::BOX-BL-Y FED::BASIC-COLOR-CHAR-BOX-AND-LINE)
      (COMPILER::INSTANCE-VARIABLE-BOUND :IMPLAUSIBLE NIL "Binding ~S, which has the same name as an instance variable of flavor ~S" FED::BOX-BR-X FED::BASIC-COLOR-CHAR-BOX-AND-LINE)
      (COMPILER::INSTANCE-VARIABLE-BOUND :IMPLAUSIBLE NIL "Binding ~S, which has the same name as an instance variable of flavor ~S" FED::BOX-BR-Y FED::BASIC-COLOR-CHAR-BOX-AND-LINE)
      (COMPILER::INSTANCE-VARIABLE-BOUND :IMPLAUSIBLE NIL "Binding ~S, which has the same name as an instance variable of flavor ~S" FED::LINE-L-X FED::BASIC-COLOR-CHAR-BOX-AND-LINE)
      (COMPILER::INSTANCE-VARIABLE-BOUND :IMPLAUSIBLE NIL "Binding ~S, which has the same name as an instance variable of flavor ~S" FED::LINE-L-Y FED::BASIC-COLOR-CHAR-BOX-AND-LINE)
      (COMPILER::INSTANCE-VARIABLE-BOUND :IMPLAUSIBLE NIL "Binding ~S, which has the same name as an instance variable of flavor ~S" FED::LINE-R-X FED::BASIC-COLOR-CHAR-BOX-AND-LINE)
      (COMPILER::INSTANCE-VARIABLE-BOUND :IMPLAUSIBLE NIL "Binding ~S, which has the same name as an instance variable of flavor ~S" FED::LINE-R-Y FED::BASIC-COLOR-CHAR-BOX-AND-LINE)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." COLOR:COLOR-SCREEN)
      (COMPILER::NOT-USED :IMPLAUSIBLE NIL "The variable ~S is bound but never used." FED::BOX-BR-Y)
      (COMPILER::NOT-USED :IMPLAUSIBLE NIL "The variable ~S is bound but never used." FED::BOX-BR-X))
     ((:METHOD FED::BASIC-FED-POT :MOUSE-MOVES) NIL NIL
      (COMPILER:UNDEFINED-FUNCTION-USED :PROBABLE-ERROR NIL "The undefined function ~S was called" COLOR:WRITE-COLOR-MAP)
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." COLOR:COLOR-SCREEN))
     (FED::COM-FG-CAMERA NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." COLOR:COLOR-SCREEN))
     (FED::COM-FG-CAMERA-TIL-SPACE NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." COLOR:COLOR-SCREEN))
     (FED::COM-FG-CROP NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." COLOR:COLOR-SCREEN))
     (FED::COM-FG-SNAP-TO-GRAY NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." COLOR:COLOR-SCREEN))
     (FED::COM-FG-SNAP-TO-BLACK NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." COLOR:COLOR-SCREEN))
     (FED::COM-LOAD-COLOR NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." COLOR:COLOR-SCREEN))
     (FED::COM-SAVE-COLOR NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." COLOR:COLOR-SCREEN))
     (FED::PREPARE-COLOR-SCREEN NIL NIL
      (COMPILER::FREE-VARIABLE :MISSING-DECLARATION NIL "The variable ~S is used free; assumed special." COLOR:COLOR-SCREEN))
     ((:METHOD FED::BASIC-COLOR-CHAR-BOX-AND-LINE :DRAW) NIL NIL
      (COMPILER:UNDEFINED-FUNCTION-USED :PROBABLE-ERROR NIL "The undefined function ~S was called" COLOR:COLOR-DRAW-LINE))
     ((:METHOD FED::BASIC-COLOR-CHAR-BOX-AND-LINE :UNDRAW) NIL NIL
      (COMPILER:UNDEFINED-FUNCTION-USED :PROBABLE-ERROR NIL "The undefined function ~S was called" COLOR:COLOR-DRAW-LINE)))))
