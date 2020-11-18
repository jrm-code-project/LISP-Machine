-*- Mode:Text -*-


The purpose of this document is to present an architectual overview of the
K-Processor, outline the K's functionality, and present a strategy for testing
this functionality.  The test strategy will be based upon an identification of
functional dependancies found in the K-Processor design.  Data paths used by
more than one function of the K-Processor will be tested first thereby providing
the basis for diagnosing the source of failure in a K-Processor that is not
fully functional.



Documents to draw upon (existing):

K-Processor Technical Manual
K-Processor Instruction Set Tables
K-Processor Board Block Diagrams
K-Processor Board Wiring Diagrams


Documents to draw upon (planned):

K-Processor Functional Specification
K-Processor Detailed Design Document




Existing Tests and Diagnostics

spy-diags.lisp
test-vectors.lisp





Memory-Diagnostics
        main data path memories
                left and right versions for each register
                        (due to simulated two read one write port memory)
                frame and location (4K locations)
                three ways of accessing
                        global registers
                        OAR frame
                        function return destination
        call stack memory
                return pc and destination stack  (heap pointer/call stack pointer)
                saved OAR frames
        datatype ram
        gc ram
        tranporter ram
        memory map
                external NuBus memory
                        Fsxx xxxx where s is slot on NuBus



Trap Diagnostics
        modifying exit


Instruction Cache
        run first with cache off and statistics on
        run with each cache turned on seperately
                Page Zero cache
                A Cache
                B Cache
        compare statistics with those run on known good machine


ALU Diagnostics
