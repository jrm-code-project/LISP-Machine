


2/12/87 SUBSTITUTE? SIMPLIFY-LET
  No longer returns NIL if var has no variable-binder.
  This allows globals to be substituted

2/23/87 ... PRIMITIVES,NMACROS
  added new primitives, added macro for multi-arg primops

2/23/87 HW:UNBOXED-CONSTANT HW-MACROS,ALPHA,EMIT
  changed HW:UNBOXED-CONSTANT from primop to macro also
  allowing constants to be defined to be unboxed numbers
  and to get integrated

2/20/87 - 2/25/87  GENERATE-CONDITIONAL GENERATE
  added TENSION-BRANCH completely change conditional generation
  added lambda-queue to generate-unconditional-branch
  rewrote generate-let

2/25/87 GENERATE-CONTINUATION-CALL GENERATE
  made it do branch tensioning right and hack primops lambda conts

2/25/87 GENERATE-LABEL-CALL GENERATE
  removed spurious old assignment to NARGS for rest arg
  instead pass it to parallel-assign (which does nothing with it yet)

2/26/87 PARALLEL-ASSIGN GENERATE
  changed parallel-assign to call LIST for assignment to known
  rest args.  Fixed GENERATE-LAMBDA which was calling cons-rest even
  for STRATEGY/LABEL lambdas.

2/26/87 STRATEGY-ANALYZE-LET NEW-STRATEGY
  fixed this which was not getting lambda-bound-vars right
  and so not setting STRATEGY/HEAP right.

2/26/87 GENERATE-LABELS GENERATE
  Fix this up to start trying to generate heap labels.
  Still losing because lambda-live is not the right thing
  for finding out which vars are closed over.

2/26/87 ... PROCESSOR-DEFS
  Created this file from stuff in EMIT (used in primitives)

2/26/87 ... NMACROS
  Stopped using macros defined in the lambda environment

2/27/87 hardware-constants, cs-primitives
  added unboxed-constant to hardware-constants cuz it uses it
  made prims:defmacro so that loading k files on lambda
  gets you macros for new compiler

2/27/87 GENERATE-LET GENERATE, TRACE-ANALYZE-LET TRACE
  Made it call parallel-assign to get rest args consed
  Took out bug message for let with rest arg in trace
  (allowed for lambdas, syntax of let disallows)


...

3/4/87 GENERATE, ASSEM, EMIT
  GET-CALL-DESTINATION, do NEW-OPEN and NEW-TAIL-OPEN
  broke off very processor specific stuff to PROCESSOR-DEFS

3/6/87 ALPHA
  Put in DECLARE, added VARIABLE-SPECIAL-P to TOP-DEFS
  improved special variable compilation

3/6/87 GENERATE
  Fixed MV return. Added GENERATE-ASSIGN-RETURN-VALUES.
  Added GET-DESTINATION, GENERATE-ASSIGN-CONT-VALUE return values may need
  to go to special/closure/stack

3/9/87 GENERATE
  Generalized GENERATE-CONTINUATION, use it in gen-general-call

3/10/87 DISPATCH PRIMITIVES, ALPHA, COMPILATORS
  Make dispatch really compile into dispatch

3/10/87 ALPHA-VARIABLE SETQ CREATE-FREE-REFERENCE ALPHA
  Fixed up special variable detection and free var warnings
  set special-p in variable and give only one warning

3/10/87 BIND ALPHA-LAMBDA ALPHA; GENERATE-RETURN TAIL-CONTINUATION-P ... GENERATE
  Moved special variable binding to generation phase
  The theory is to check args at lambda generation time and bind any specials
  and to check again when returning (generate-return for heap return,
  generate-continuation sometimes ... and tail calls in gen-general-call)
  and generate unbind code..  the hard part of this is the return values
  which need to be saved... (not done yet...)

3/11/87  GENERATE-RETURN, GENERATE-CONTINUATION, GEN-GENERAL-CALL
  Completely remodeled generation of return/continuations
  to handle undoing dynamic state. Includes removal
  of tail calls when dynamic state is present, and managing
  return values with undoing.  Added DYNAMIC-DEPTH-TO-RETURN

3/12/87 REG-ALLOC
  dynamic-depth-to-return was adding in stack-depth for label
  lambdas (which is ok) so reg-alloc sets it to 0

3/12/87
  Fixed up read/write stack slots to not clobber registers
  with their temps in some cases (read two slots, write a
  slot which is dest of something)

3/12/87 VARIABLE-OPTIONAL-P TOP-DEFS, Alpha-LAMBDA ALPHA
  note which variables are optional args.  Could use this
  to bind rather than setq special optionals in optional-setup
  and not bind when binding other special args
  (but where does that happen???)


3/12/87 SIMPLIFY-EXIT-ARGS SIMPLIFY-CALL
  Made this simplify all exits (for dispatch which takes lots)
  and made it check proc for conditional rather than just
  assume something was a conditional if it had two exits

3/12/87 SETQ ALPHA
  Set variable-setqs to T.  (does it need it for wierd vars?)

