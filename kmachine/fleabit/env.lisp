support  *primitive-primop-table*
            a primop-table
            defined in ENVS
            filled in PRIMITIVE associates name with primop object

         *primitive-support-table*
           a support-table defined in ENVS
           contains *primitive-primop-table*
           containts support structure
           filled in PRIMITIVE associates name with support object
              (which contains support variable and primop object)


         *support*
           bound by do-exps to an argument, arg to make-code-tree
                     arg to really-compile, *primitive-support-table* passed by nc
           used in LOCALE-VARIABLE-REFERENCE in NODE


         *new-support*
           was bound to (make-empty-support-env) by FRONT-INIT
           gets support-entries (created from shape-defined after alphatization
                (definitions from lset set-variable-value define... define-constant))
                added to it by create-env (called from do-exps)
           this will contain new support things defined in a module
           used in LOCALE-VARIABLE-REFERENCE in NODE




        locale-variable-reference creates a reference node to the support-variable
        (who makes variable-refs nil?) integrate-support in the simplification pass
        looks at variable-support (in get-variable-support) and replaces the
        support-variable with the support-value

        define-constant was done by adding the constant to the shape (and shape-defined)
        after alphatization things on shape-defined are added to *new-support* (in create-env)
        then in node/cps conversion locale-variable-reference...


           defconstant
           defsubst
           primops
       at the moment these are substituted at simplification time
       can/should they be done during alphatization?

       (constants are now)


syntax   primitive-syntax-table
          defined in ALPHA, associates entry with "syntax descriptor" (now just symbol)
          (by define-special-form)

         primitive-handler-table
           syntax descriptors looked up in here by alpha-special-form


         *syntax*
           bound by do-exps to a new syntax table derived from arg
             from make-code-tree,really-compile,nc, primitive-syntax-table
           seems to have been used globally only by subexpression->code-tree (removed)
                 called by simplify-*primop (removed)
           passed to alphatize-module which passes it all over and only used in alpha
             which looks up symbol gets "syntax descriptor" and passes that to
             alpha-special-form which looks up syntax/foo in primitive-syntax-table
             (let-syntax used to extend syntax)
