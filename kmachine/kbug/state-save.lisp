;;;-*- Mode:LISP; Package:K2; Base:10.; Readtable:CL -*-

;stack group stuff:
;  debug full save..
;    save all that can be saved.  we must assume a trap has saved the "ultra-volatile" state.
;  debug full restore ..

;the debug image
;  every bit of dynamic state in the whole thing

;the registers:

;random registers
;    oar
;    Q
;  4kx32 plus boxed bits
;    reg 0 is global trap frame

