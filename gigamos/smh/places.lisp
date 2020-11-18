k.fleabit; top
        Add FDEF file creation to nlisp:compile-file.
        Fix all the top-level handlers that commit EVAL-WHEN COMPILE lossage.
e.g., DEFCONSTANT evals the form in the compiling world.
We also need to surround lots of the K hardware definitions with
EVAL-WHEN (COMPILE LOAD EVAL).  This is OK since these definitions will stay
in certain isolated packages.
        Make top-level handlers side effect the compilation environment.

k.fleabit; new-macros
        Make NLISP:MACROEXPAND-1 look for a compilation environment like
macroexpand now does.

k.fleabit; extra-stuff
        Make NC:CONSTANT-P know about compilation environments.

k.fleabit; front-end; alpha
        Make NC:SPECIAL-P know about compilation environments.

k; new-setf
        Change the two alists *SIMPLE-SETF-MACRO-TABLE* and
*GRODY-SETF-MACRO-TABLE* to use properties in the compilation
environment.

Check out the K TYPE system.
