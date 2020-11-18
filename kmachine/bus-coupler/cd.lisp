;;;-*- Mode:LISP; Package:LAMBDA; Base:10; Readtable:ZL -*-
;;; Bus coupler diagnostic tests        5-10-88       Mike M.




;local configuration PROM fsffffff-fsfffe00

(defconst memory-size #x80)
(defconst local-config-prom #xfffe00)
(defconst local-config-register #xfffd00)
(defconst local-control-register #xfffc00)
(defconst local-rba-map-ram #xfffa00)
(defconst busc-slot #xfc)
(defconst remote-falcon-slot #x0)       ;in K-MAC with memory board in far edge slot
                                        ;(ie. as far as possible from coupler) .
(defconst local-mem-slot #x81)                  ;Was #x91
(defconst remote-mem-slot #xfb)                 ;Was #xf0


;remote MBC configuration PROM fsfff7ff-fsfff600


(defconst remote-config-prom #xfff600)
(defconst remote-config-register #xfff500)
(defconst remote-control-register #xfff400)
(defconst remote-rba-map-ram #xfff200)


(defvar current-address 0)
(defvar offset 0)
(defvar addr 0)
(defvar contents 0)
(defvar new-contents 0)
(defvar reg-contents 0)
(defvar rbc 0)
(defvar rsy 0)
(defvar rba 0)
(defvar tal 0)
(defvar paren 0)
(defvar parer 0)
(defvar tim 0)
(defvar bit 0)
(defvar map-offset 0)
(defvar map-data 0)
(defvar ram-error-count 0)
(defvar data 0)
(defvar temp 0)





(defsubst read-control-reg ()
  (sys:%nubus-read-8 busc-slot local-control-register))

(defsubst read-remote-control-reg ()
  (sys:%nubus-read-8 busc-slot remote-control-register))

(defsubst read-config-reg ()
  (sys:%nubus-read-8 busc-slot local-config-register))

(defsubst read-remote-config-reg ()
  (sys:%nubus-read-8 busc-slot remote-config-register))

(defsubst coupler-read-config-prom (offset)
  (sys:%nubus-read-8 busc-slot (+ offset local-config-prom)))

(defsubst coupler-read-remote-config-prom (offset)
  (sys:%nubus-read-8 busc-slot (+ offset remote-config-prom)))

(defsubst read-map (offset)
  (sys:%nubus-read-8 busc-slot (+ offset local-rba-map-ram)))

(defsubst read-remote-map (offset)
  (sys:%nubus-read-8 busc-slot (+ offset remote-rba-map-ram)))


(defsubst write-control-reg (data)
  (sys:%nubus-write-8 busc-slot local-control-register data))

(defsubst write-remote-control-reg (data)
  (sys:%nubus-write-8 busc-slot remote-control-register data))

(defsubst write-map (offset data)
  (sys:%nubus-write-8 busc-slot (+ offset local-rba-map-ram) data))

(defsubst write-remote-map (offset data)
  (sys:%nubus-write-8 busc-slot  (+ offset remote-rba-map-ram) data))

(defsubst write-config-reg (data)
  (sys:%nubus-write-8 busc-slot local-config-register data))

(defsubst write-remote-config-reg (data)
  (sys:%nubus-write-8 busc-slot remote-config-register data))

(defsubst write-config-prom (data)
  (sys:%nubus-write-8 busc-slot local-config-register data))

(defsubst write-remote-config-prom (data)
  (sys:%nubus-write-8 busc-slot remote-config-register data))



(defsubst read-bus-coupler (addr)
  (sys:%nubus-read-8 busc-slot addr))


(defsubst write-bus-coupler (addr data)
  (sys:%nubus-write-8 busc-slot addr data))



;;;;;;;;;;MBC TEST LOOPS;;;;;;;;;




(defun read-bus-coupler-loop ()
  (do-forever
    (read-bus-coupler local-rba-map-ram)))


(defun read-control-reg-loop ()
  (do-forever
    (read-control-reg )))


(defun read-remote-control-reg-loop ()
  (do-forever
    (read-remote-control-reg )))


(defun read-config-reg-loop ()
  (do-forever
    (read-config-reg )))


(defun read-remote-config-reg-loop ()
  (do-forever
    (read-remote-config-reg )))


(defun write-map-loop (data)
  (do-forever
    (write-bus-coupler local-rba-map-ram data)))


(defun read-map-loop ()
  (do-forever
    (read-bus-coupler local-rba-map-ram )))


(defun write-remote-map-loop (data)
  (do-forever
    (write-bus-coupler remote-rba-map-ram data)))


(defun read-remote-map-loop ()
  (do-forever
    (read-bus-coupler remote-rba-map-ram )))




;;;;;;;;;;MBC RBA MAP PURGE;;;;;;;;;;





(defun clear-map ()             ;Clears local map and verifies that it is clear!!!
  (prog ()
       (setq offset 0)
       (setq ram-error-count 0)
    :write
       (write-map offset 0)
       (setq offset (+ 4 offset))
       (if (> #xfffbfd (+ local-rba-map-ram offset) ) (go :write))
    :read
     (setq addr #xfffbfc data 0)
     (dotimes (i memory-size)
       (setq temp (read-bus-coupler addr))
       (cond (( data temp)
         (ram-error "Clear Map" addr data temp)))
       (setq addr (- addr 4)))
     (cond ((= ram-error-count 0)
     (format t "~% Map Cleared successfully~%"))))
     (values))



(defun clear-remote-map ()        ;Clears remote map and verifies that it is clear!!!
  (prog ()                        ;Clears remote map via local MBC!!!
       (setq offset 0)
       (setq ram-error-count 0)
    :write
       (write-remote-map offset 0)
       (setq offset (+ 4 offset))
       (if (> #xfff3fd (+ remote-rba-map-ram offset) ) (go :write))
    :read
     (setq addr #xfff3fc data 0)
     (dotimes (i memory-size)
       (setq temp (read-bus-coupler addr))
       (cond (( data temp)
         (ram-error "Clear Remote Map" addr data temp)))
       (setq addr (- addr 4)))
     (cond ((= ram-error-count 0)
     (format t "~% Remote Map Cleared successfully~%"))))
     (values))



;;;;;;;;MBC CONTROL REGISTER FUNCTIONS;;;;;;;;




(defun clear-control-reg ()
  (write-control-reg 0)
  (setq reg-contents (read-control-reg))
  (format t "~%")
  (cond ((= reg-contents #x20)
  (format t " Control Register Cleared - Control register contains ~16R ~%"
          reg-contents)))
  (cond (( reg-contents #x20)
  (format t " Control Register *ERROR* - Control register contains ~16R ~%"
          reg-contents)))
  (cond (( reg-contents #x20)
  (format t " Control Register failed to clear - should be #x20")))
  (values))



(defun clear-remote-control-reg ()
  (write-remote-control-reg 0)
  (setq reg-contents (read-remote-control-reg))
  (format t "~%")
  (cond ((= reg-contents #x20)
  (format t " Remote Control Register Cleared - Remote register contains ~16R ~%"
          reg-contents)))
  (cond (( reg-contents #x20)
  (format t " Remote Control Register *ERROR* - Remote register contains ~16R ~%"
          reg-contents)))
  (cond (( reg-contents #x20)
  (format t " Control Register failed to clear - should be #x20")))
  (values))



(defun reset-remote-mbc ()   ;*WARNING* This function resets the remote MBC!!!
  (setq reg-contents (read-control-reg))   ;This function is now obsolete.
  (setq bit (logand reg-contents #x1))
  (cond
    ((= #x0 bit)
     (setq new-contents (+ reg-contents #x1))
     (write-control-reg new-contents)
     (setq reg-contents (read-control-reg))
     (format t "~%")
     (format t " Remote MBC reset - Control register contains ~16R ~%" reg-contents))
     (t (format t " Remote MBC already reset - Control register contains - ~16R ~%"
                reg-contents)))(values))



(defun set-remote-mbc ()
  (setq reg-contents (read-control-reg))  ;This function is now obsolete.
  (setq bit (logand reg-contents #x1))
  (cond
    ((= #x1 bit)
     (setq new-contents (- reg-contents #x1))
     (write-control-reg new-contents)
     (setq reg-contents (read-control-reg))
     (format t "~%")
     (format t " Remote MBC set - Control register contains ~16R ~%" reg-contents))
     (t (format t " Remote MBC already set - Control register contains - ~16R ~%"
                reg-contents)))(values))



(defun reset-remote-system ()   ;*WARNING* This function resets the remote system!!!
  (setq reg-contents (read-control-reg))  ;Presently causes a timeout!!!!
  (setq bit (logand reg-contents #x80))
  (cond
    ((= #x0 bit)
     (setq new-contents (+ reg-contents #x80))
     (write-control-reg new-contents)
     (write-remote-config-reg #x1)
     (setq reg-contents (read-control-reg))
     (format t "~%")
     (format t " Remote System reset - Control register contains ~16R ~%" reg-contents))
     (t (format t " Remote system already reset - Control register contains - ~16R ~%"
                reg-contents)))(values))



(defun set-remote-system ()
  (setq reg-contents (read-control-reg))
  (setq bit (logand reg-contents #x80))
  (cond
    ((= #x80 bit)
     (setq new-contents (- reg-contents #x80))
     (write-control-reg new-contents)
     (write-remote-config-reg #x0)
     (setq reg-contents (read-control-reg))
     (format t "~%")
     (format t " Remote System set - Control register contains ~16R ~%" reg-contents))
     (t (format t " Remote system already set - Control register contains - ~16R ~%"
                reg-contents)))(values))



(defun enable-remote-bus-address-space ()   ;*WARNING* You must clear the local map first!!!
  (setq reg-contents (read-control-reg))
  (setq bit (logand reg-contents #x4))
  (cond
    ((= #x0 bit)
     (setq new-contents (+ reg-contents #x4))
     (write-control-reg new-contents)
     (setq reg-contents (read-control-reg))
     (format t "~%")
     (format t " Remote Bus Address Space enabled - Control register contains ~16R ~%"
             reg-contents))
     (t
      (format t " Remote bus address space already enabled - Control register contains - ~16R ~%"
                reg-contents)))(values))



(defun disable-remote-bus-address-space ()
  (setq reg-contents (read-control-reg))
  (setq bit (logand reg-contents #x4))
  (cond
    ((= #x4 bit)
     (setq new-contents (- reg-contents #x4))
     (write-control-reg new-contents)
     (setq reg-contents (read-control-reg))
     (format t "~%")
     (format t " Remote Bus Address Space disabled - Control register contains ~16R ~%"
             reg-contents))
     (t
      (format t " Remote bus address space already disabled - Control register contains - ~16R ~%"
                reg-contents)))(values))



(defun enable-try-again-later ()            ;*WARNING* This must not be used on both
  (setq reg-contents (read-control-reg))    ; bus couplers at the same time!!!
  (setq bit (logand reg-contents #x8))
  (cond
    ((= #x0 bit)
     (setq new-contents (+ reg-contents #x8))
     (write-control-reg new-contents)
     (setq reg-contents (read-control-reg))
     (format t "~%")
     (format t " Try Again Later enabled - Control register contains ~16R ~%"
             reg-contents))
     (t (format t " Try again later is already enabled - Control register contains - ~16R ~%"
                reg-contents)))(values))



(defun disable-try-again-later ()
  (setq reg-contents (read-control-reg))
  (setq bit (logand reg-contents #x8))
  (cond
    ((= #x8 bit)
     (setq new-contents (- reg-contents #x8))
     (write-control-reg new-contents)
     (setq reg-contents (read-control-reg))
     (format t "~%")
     (format t " Try Again Later disabled - Control register contains ~16R ~%"
             reg-contents))
     (t (format t " Try again later is already disabled - Control register contains - ~16R ~%"
                reg-contents)))(values))



(defun enable-serial-parity ()
  (setq reg-contents (read-control-reg))
  (setq bit (logand reg-contents #x10))
  (cond
    ((= #x0 bit)
     (setq new-contents (+ reg-contents #x10))
     (write-control-reg new-contents)
     (setq reg-contents (read-control-reg))
     (format t "~%")
     (format t " Serial Parity enabled - Control register contains ~16R ~%"
             reg-contents))
     (t (format t " Serial parity is already enabled - Control register contains - ~16R ~%"
                reg-contents)))(values))



(defun disable-serial-parity ()
  (setq reg-contents (read-control-reg))
  (setq bit (logand reg-contents #x10))
  (cond
    ((= #x10 bit)
     (setq new-contents (- reg-contents #x10))
     (write-control-reg new-contents)
     (setq reg-contents (read-control-reg))
     (format t "~%")
     (format t " Serial Parity disabled - Control register contains ~16R ~%"
             reg-contents))
     (t (format t " Serial parity is already disabled - Control register contains - ~16R ~%"
                reg-contents)))(values))



(defun disable-timeout ()
  (setq reg-contents (read-control-reg))
  (setq bit (logand reg-contents #x40))
  (cond
    ((= #x0 bit)
     (setq new-contents (+ reg-contents #x40))
     (write-control-reg new-contents)
     (setq reg-contents (read-control-reg))
     (format t "~%")
     (format t " Timeout disabled - Control register contains ~16R ~%"
             reg-contents))
     (t (format t " Timeout is already disabled - Control register contains - ~16R ~%"
                reg-contents)))(values))



(defun enable-timeout ()
  (setq reg-contents (read-control-reg))
  (setq bit (logand reg-contents #x40))
  (cond
    ((= #x40 bit)
     (setq new-contents (- reg-contents #x40))
     (write-control-reg new-contents)
     (setq reg-contents (read-control-reg))
     (format t "~%")
     (format t " Timeout enabled - Control register contains ~16R ~%"
             reg-contents))
     (t (format t " Timeout is already enabled - Control register contains - ~16R ~%"
                reg-contents)))(values))



;;;;;;;;;VIEW MBC CONTROL REGISTER;;;;;;;;;;




(defun view-control-reg ()   ;Use this function to view the control register!!!
  (setq rbc 0)
  (setq rsy 0)
  (setq rba 0)
  (setq tal 0)
  (setq paren 0)
  (setq parer 0)
  (setq tim 0)
  (setq reg-contents (read-control-reg))
  (setq bit (logand reg-contents #x1))
  (cond
    ((= #x1 bit)
     (setq rbc 1)))
  (setq bit (logand reg-contents #x2))
  (cond
    ((= #x2 bit)
     (setq rsy 1)))
  (setq bit (logand reg-contents #x4))
  (cond
    ((= #x4 bit)
     (setq rba 1)))
  (setq bit (logand reg-contents #x8))
  (cond
    ((= #x8 bit)
     (setq tal 1)))
  (setq bit (logand reg-contents #x10))
  (cond
    ((= #x10 bit)
     (setq paren 1)))
  (setq bit (logand reg-contents #x20))
  (cond
    ((= #x20 bit)
     (setq parer 1)))
  (setq bit (logand reg-contents #x40))
  (cond
    ((= #x40 bit)
     (setq tim 1)))
  (format t  "~% -to_ena  -par_er  par_ena  tal_ena  rba_ena  rsy_res  rbc_res")
  (format t  "~%    ~D        ~D        ~D        ~D        ~D        ~D        ~D"
          tim parer paren tal rba rsy rbc)(values))




(defun view-remote-control-reg ()   ;Function to view the remote control register
  (setq rbc 0)                      ;via the local bus coupler!!!
  (setq rsy 0)
  (setq rba 0)
  (setq tal 0)
  (setq paren 0)
  (setq parer 0)
  (setq tim 0)
  (setq reg-contents (read-remote-control-reg))
  (setq bit (logand reg-contents #x1))
  (cond
    ((= #x1 bit)
     (setq rbc 1)))
  (setq bit (logand reg-contents #x2))
  (cond
    ((= #x2 bit)
     (setq rsy 1)))
  (setq bit (logand reg-contents #x4))
  (cond
    ((= #x4 bit)
     (setq rba 1)))
  (setq bit (logand reg-contents #x8))
  (cond
    ((= #x8 bit)
     (setq tal 1)))
  (setq bit (logand reg-contents #x10))
  (cond
    ((= #x10 bit)
     (setq paren 1)))
  (setq bit (logand reg-contents #x20))
  (cond
    ((= #x20 bit)
     (setq parer 1)))
  (setq bit (logand reg-contents #x40))
  (cond
    ((= #x40 bit)
     (setq tim 1)))
  (format t  "~% -to_ena  -par_er  par_ena  tal_ena  rba_ena  rsy_res  rbc_res")
  (format t  "~%    ~D        ~D        ~D        ~D        ~D        ~D        ~D"
          tim parer paren tal rba rsy rbc)(values))



;;;;;;;;;;;MBC SETUP FOR MEMORY DIAGNOSTICS;;;;;;;;;;




(defun setup-remote-slot ()                                 ; local-mem-slot = #x91  [default]
  (clear-control-reg)                                       ; remote-mem-slot = #xF9 [default]
  (set-remote-mbc)
  (clear-map)
  (setq map-offset (times #x2 local-mem-slot))
  (setq map-offset (logand map-offset #x1fc))
  (setq map-data remote-mem-slot)
  (cond ((= 0 (remainder map-data 2))(setq map-data (+ 1 map-data))))     ;Make sure valid bit
  (write-map map-offset map-data)                                         ;is set!!!!
  (setq contents (read-map map-offset))
  (setq current-address (+ #xfffa00 map-offset))
  (format t "~% Address map contains ~16R at address #x~16R~16R ~%"
          contents busc-slot current-address)
  (format t "~% Remote slot #x~16R can now be accessed using #x~16R as a local slot address ~%"
          remote-mem-slot local-mem-slot)
  (enable-remote-bus-address-space)
  (cond ((and (oddp remote-mem-slot)(evenp local-mem-slot))
         (setq 4meg-slot (+ 4meg-slot 1))))
  (cond ((and (oddp remote-mem-slot)(evenp local-mem-slot))
         (format t "~% *WARNING* We are really using #x~16R as a local slot address ~%"
                 4meg-slot)))  ; This must be done because of the present address scheme!!!
  (cond ((and (oddp local-mem-slot)(evenp remote-mem-slot))
         (setq 4meg-slot (- 4meg-slot 1))))
  (cond ((and (oddp local-mem-slot)(evenp remote-mem-slot))
         (format t "~% *WARNING* We are really using #x~16R as a local slot address ~%"
                 4meg-slot))))



;;;;;;;;;;MBC RBA MAP RAM TESTS;;;;;;;;;;




(defun ram-error (test addr expect got)
  (format t " Error in ~A test at address ~X  expected ~X got ~X ~%"
          test addr expect got)
  (setq ram-error-count (1+ ram-error-count)))



(defun rba-map-ram-test ()
  (setq ram-error-count 0)
  (write-control-reg 0)
  (format t "~%")
  (format t " Map ram address test ~%")
  (setq addr local-rba-map-ram data (- local-rba-map-ram #xfffa00))
  (dotimes (i memory-size)
    (cond ((= data #x100)(setq data (- data #x100))))
    (write-bus-coupler addr data)
    (setq addr (+ addr 4))
    (setq data (+ data 4)))
  (setq addr local-rba-map-ram data (- local-rba-map-ram #xfffa00))
  (dotimes (i memory-size)
    (cond ((= data #x100)(setq data (- data #x100))))
    (and
      ( (setq temp (read-bus-coupler addr)) data)
      (ram-error "Map ram address" addr data temp))
    (setq addr (+ addr 4)
          data (+ data 4)))
  (cond ((= ram-error-count 0)(format t "......passed ~%")))
  (values))



(defun inverted-rba-map-ram-test ()
  (setq ram-error-count 0)
  (format t "~%")
  (format t " Inverted Map ram address test ~%")
  (setq addr #xfffbfc)
  (setq data (logxor #xff #xfc))
  (dotimes (i memory-size)
    (cond ((> data #xFF)(setq data (- data #x100))))
    (write-bus-coupler addr data)
    (setq addr (- addr 4))
    (setq data (+ data 4)))
  (setq addr #xfffbfc data (logxor #xff #xfc))
  (dotimes (i memory-size)
    (cond ((> data #xFF)(setq data (- data #x100))))
    (and
      ( (setq temp (read-bus-coupler addr)) data)
      (ram-error "Inverted Map ram address" addr data temp))
    (setq addr (- addr 4) data (+ data 4)))
  (cond ((= ram-error-count 0)(format t "......passed ~%")))
  (values))




(defun remote-rba-map-ram-test ()    ;Tests the remote map via the local MBC!!!
  (setq ram-error-count 0)
  (write-control-reg 0)
  (format t "~%")
  (format t " Remote Map ram address test ~%")
  (setq addr remote-rba-map-ram data (- remote-rba-map-ram #xfff200))
  (dotimes (i memory-size)
    (cond ((= data #x100)(setq data (- data #x100))))
    (write-bus-coupler addr data)
    (setq addr (+ addr 4))
    (setq data (+ data 4)))
  (setq addr remote-rba-map-ram data (- remote-rba-map-ram #xfff200))
  (dotimes (i memory-size)
    (cond ((= data #x100)(setq data (- data #x100))))
    (and
      ( (setq temp (read-bus-coupler addr)) data)
      (ram-error "Remote Map ram address" addr data temp))
    (setq addr (+ addr 4)
          data (+ data 4)))
  (cond ((= ram-error-count 0)(format t "......passed ~%")))
  (values))



(defun remote-inverted-rba-map-ram-test ()  ;Tests the remote map via the local MBC!!!
  (setq ram-error-count 0)
  (format t "~%")
  (format t " Remote Inverted Map ram address test ~%")
  (setq addr #xfff3fc)
  (setq data (logxor #xff #xfc))
  (dotimes (i memory-size)
    (cond ((> data #xFF)(setq data (- data #x100))))
    (write-bus-coupler addr data)
    (setq addr (- addr 4))
    (setq data (+ data 4)))
  (setq addr #xfff3fc data (logxor #xff #xfc))
  (dotimes (i memory-size)
    (cond ((> data #xFF)(setq data (- data #x100))))
    (and
      ( (setq temp (read-bus-coupler addr)) data)
      (ram-error "Remote Inverted Map ram address" addr data temp))
    (setq addr (- addr 4) data (+ data 4)))
  (cond ((= ram-error-count 0)(format t "......passed ~%")))
  (values))



;;;;;;;;;;MBC REGISTER TESTS;;;;;;;;;;;



(defun control-register-test ()
  (clear-map)
  (format t "~% Control register test ~%")
  (write-control-reg #x20)
  (setq reg-contents (read-control-reg))
  (cond (( reg-contents #x20)(ram-error "Control register initialize"
                                         #xfffc00 #x20 reg-contents )))
  (cond ((= reg-contents #x20)(format t "Initialization ......passed ~%")))
  (write-control-reg #x24)
  (setq reg-contents (read-control-reg))
  (cond (( reg-contents #x24)(ram-error "Control register rba_ena"
                                         #xfffc00 #x24 reg-contents )))
  (cond ((= reg-contents #x24)(format t "Rba_enable     ......passed ~%")))
  (write-control-reg #x30)
  (setq reg-contents (read-control-reg))
  (cond (( reg-contents #x30)(ram-error "Control register par_ena"
                                         #xfffc00 #x30 reg-contents )))
  (cond ((= reg-contents #x30)(format t "Par_ena        ......passed ~%")))
  (write-control-reg #x60)
  (setq reg-contents (read-control-reg))
  (cond (( reg-contents #x60)(ram-error "Control register timeout_disable"
                                         #xfffc00 #x60 reg-contents )))
  (cond ((= reg-contents #x60)(format t "Timeout_disable......passed ~%")))
  (write-control-reg #x2C)
  (setq reg-contents (read-control-reg))
  (cond (( reg-contents #x2C)(ram-error "Control register timeout_disable"
                                         #xfffc00 #x2C reg-contents )))
  (cond ((= reg-contents #x2C)(format t "Tal_enable     ......passed ~%")))
  (write-control-reg #x20)
  (setq reg-contents (read-control-reg))
  (cond ((= reg-contents #x20)(format t "Control register cleared successfully~%")))
  (values))



(defun remote-control-register-test () ;Tests the remote control register via the local MBC!!!
  (clear-remote-map)
  (format t "~% Remote Control register test ~%")
  (write-remote-control-reg #x20)
  (setq reg-contents (read-remote-control-reg))
  (cond (( reg-contents #x20)(ram-error "Remote Control register initialize"
                                         #xfff400 #x20 reg-contents )))
  (cond ((= reg-contents #x20)(format t "Initialization ......passed ~%")))
  (write-remote-control-reg #x24)
  (setq reg-contents (read-remote-control-reg))
  (cond (( reg-contents #x24)(ram-error "Remote Control register rba_ena"
                                         #xfff400 #x24 reg-contents )))
  (cond ((= reg-contents #x24)(format t "Rba_enable     ......passed ~%")))
  (write-remote-control-reg #x30)
  (setq reg-contents (read-remote-control-reg))
  (cond (( reg-contents #x30)(ram-error "Remote Control register par_ena"
                                         #xfff400 #x30 reg-contents )))
  (cond ((= reg-contents #x30)(format t "Par_enable     ......passed ~%")))
  (write-remote-control-reg #x60)
  (setq reg-contents (read-remote-control-reg))
  (cond (( reg-contents #x60)(ram-error "Remote Control register timout_disable"
                                         #xfff400 #x60 reg-contents )))
  (cond ((= reg-contents #x60)(format t "Timeout_disable......passed ~%")))
  (write-remote-control-reg #x2C)
  (setq reg-contents (read-remote-control-reg))
  (cond (( reg-contents #x2C)(ram-error "Remote Control register tal_enable"
                                         #xfff400 #x2C reg-contents )))
  (cond ((= reg-contents #x2C)(format t "Tal_enable     ......passed ~%")))
  (write-remote-control-reg #x20)
  (setq reg-contents (read-remote-control-reg))
  (cond ((= reg-contents #x20)(format t "Remote Control register cleared successfully~%")))
  (values))



(defun config-register-test ()
  (format t "~%")
  (format t " Config register test ~%")
  (write-config-reg #x0)
  (setq reg-contents (read-config-reg))
  (cond (( reg-contents #x2)(ram-error "Config register write 0000"
                                        #xfffd00 #x2 reg-contents )))
  (cond ((= reg-contents #x2)(format t "Pattern 0000 should return 0010 ......passed ~%")))
  (write-config-reg #xf)
  (setq reg-contents (read-config-reg))
  (cond (( reg-contents #x6)(ram-error "Config register write 1111"
                                        #xfffd00 #x6 reg-contents )))
  (cond ((= reg-contents #x6)(format t "Pattern 1111 should return 0110 ......passed ~%")))
  (write-config-reg #xA)
  (setq reg-contents (read-config-reg))
  (cond (( reg-contents #xA)(ram-error "Config register Special pattern 1010"
                                        #xfffd00 #xA reg-contents )))
  (cond ((= reg-contents #xA)(format t "Pattern 1010 should return 1010 ......passed ~%")))
  (write-config-reg #x5)
  (setq reg-contents (read-config-reg))
  (cond (( reg-contents #x6)(ram-error "Config register Special pattern 0101"
                                        #xfffd00 #x6 reg-contents )))
  (cond ((= reg-contents #x6)(format t "Pattern 0101 should return 0110 ......passed ~%")))
  (format t "Flash Fault LED Awhile ~%")
  (beep)
  (dotimes (i 7)
    (write-config-reg #x0)(ua:sleep .5)
    (write-config-reg #xf)(ua:sleep .5))
  (beep)
  (write-config-reg #x0)
  (setq reg-contents (read-config-reg))
  (cond ((= reg-contents #x2)(format t "Config register initialized successfully~%")))
  (cond (( reg-contents #x2)(format t "Config register DID NOT initialize successfully~%")))
  (values))



(defun remote-config-register-test ()   ;Tests the remote config register via the local MBC!!!
  (format t "~%")
  (format t " Remote Config register test ~%")
  (write-remote-config-reg #x0)
  (setq reg-contents (read-remote-config-reg))
  (cond (( reg-contents #x2)(ram-error "Remote Config register write 0000"
                                        #xfff500 #x2 reg-contents )))
  (cond ((= reg-contents #x2)(format t "Pattern 0000 should return 0010 ......passed ~%")))
  (write-remote-config-reg #xE)         ;Writing a #x1 to the remote reg gets timeout!
  (setq reg-contents (read-remote-config-reg))
  (cond (( reg-contents #xE)(ram-error "Remote Config register write 1110"
                                        #xfff500 #xE reg-contents )))
  (cond ((= reg-contents #xE)(format t "Pattern 1110 should return 1110 ......passed ~%")))
  (write-remote-config-reg #xA)
  (setq reg-contents (read-remote-config-reg))
  (cond (( reg-contents #xA)(ram-error "Remote Config register Special pattern 1010"
                                        #xfff500 #xA reg-contents )))
  (cond ((= reg-contents #xA)(format t "Pattern 1010 should return 1010 ......passed ~%")))
  (write-remote-config-reg #x4)
  (setq reg-contents (read-remote-config-reg))
  (cond (( reg-contents #x6)(ram-error "Remote Config register Special pattern 0100"
                                        #xfff500 #x6 reg-contents )))
  (cond ((= reg-contents #x6)(format t "Pattern 0100 should return 0110 ......passed ~%")))
  (format t "Flash Remote Fault LED Awhile ~%")
  (beep)
  (dotimes (i 7)
    (write-remote-config-reg #x0)(ua:sleep .5)
    (write-remote-config-reg #xE)(ua:sleep .5))
  (beep)
  (write-remote-config-reg #x0)
  (setq reg-contents (read-remote-config-reg))
  (cond ((= reg-contents #x2)
         (format t "Remote Config register initialized successfully~%")))
  (cond (( reg-contents #x2)
         (format t "Remote Config register DID NOT initialize successfully~%")))
  (values))



;;;;;;;;;MBC DIAGNOSTIC TESTS;;;;;;;;;



(defun test-local-mbc ()
  (config-register-test)
  (control-register-test)
  (rba-map-ram-test)
  (inverted-rba-map-ram-test)(values))



(defun test-remote-mbc ()   ;Tests the remote bus coupler via the local one!!!
  (remote-config-register-test)
  (remote-control-register-test)
  (remote-rba-map-ram-test)
  (remote-inverted-rba-map-ram-test)(values))



;;;;;;;;;MBC INITIALIZATIONS;;;;;;;;;




(defun init-bc-slot (slot)                    ;Use this function to tell the diags
  (setq busc-slot (+ #xF0 slot))              ;which slot the local bus coupler is in!!!
  (format t "~% Bus coupler is in slot ~R ~%" slot)
  (values))                                   ;For slot use a decimal number from 0 to 15!!!



(defun init-bc ()                             ;Use this function to setup for a remote
  (setup-remote-slot))                        ;memory diagnostic!!!
                                              ;With TRY AGAIN LATER disabled!!!


(defun init-bc-tal ()                         ;Use this function to setup for a remote
  (setup-remote-slot)                         ;memory diagnostic!!!
  (enable-try-again-later)                    ;With TRY AGAIN LATER enabled!!!
  (view-control-reg))



(defun init-local-mem-slot (slot)             ;Use this function to define a local
  (setq local-mem-slot (+ #x00 slot))         ;bus slot for the remote memory!!!
  (format t "~% Remote memory is referenced to slot #x~16R on the local bus~%"
          local-mem-slot)
  (setq 4meg-slot local-mem-slot)             ;For slot use a hex number from 10 to FF!!!
  (values))                                   ;Example -   (init-local-mem-slot #x91)
                                              ;The value for slot should be a slot presently
                                              ;unused by any other board on the local bus!!!



(defun init-remote-mem-slot (slot)            ;Use this function to define a remote
  (setq remote-mem-slot (+ #x00 slot))        ;bus slot for the remote memory!!!
  (format t "~% Remote memory is referenced to slot #x~16R on the remote bus~%"
          remote-mem-slot)                    ;For slot use a hex number from F0 to FF!!!
  (values))                                   ;Example -   (init-remote-mem-slot #xF9)
                                              ;The value for slot should be the present
                                              ;remote memory board slot location!!!
