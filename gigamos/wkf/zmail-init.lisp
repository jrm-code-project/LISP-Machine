;;-*-Mode:LISP;Package:ZWEI;Base:10;ReadTable:CL-*-

(pushnew '("FALCON"
            :value "FALCON"
            :documentation "Report an issue for the Falcon development group")
         *zmail-bug-list*
         :key #'car
         :test #'string-equal)

(pushnew '("FALCON-COMPILER"
            :value "FALCON-COMPILER"
            :documentation "Report an issue for the Falcon Compiler development group")
         *zmail-bug-list*
         :key #'car
         :test #'string-equal)

(pushnew '("FLEABIT-COMPILER"
            :value "FLEABIT-COMPILER"
            :documentation "Report an issue for the Fleabit Compiler development group")
         *zmail-bug-list*
         :key #'car
         :test #'string-equal)

