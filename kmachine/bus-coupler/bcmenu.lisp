;;;-*- Mode:LISP; Package:LAMBDA; Base:16 -*-
;;; Bus coupler diagnostic tests        5-10-88       Mike M.
;;;                                     5-13-88       Rich R.
;;; Derived from 4meg memory diagnostic






;;;;;put coupler-diagnostic here


;;;;;
(defun big-letters-L ()
  (send terminal-io ':set-font-map '(METS)))

(defun small-letters-L ()
  (send terminal-io ':set-font-map '(cptfont)))
(DEFVAR DEVICE-L NIL)

(defun DEVICE?-L ()
  (SETQ DEVICE-L (prompt-and-read ':read "~%What is the DEVICE?  KEYBOARD, MONITOR, OR MOUSE."))
    (format terminal-io "THE DEVICE IS  ~a." DEVICE-L))

(DEFVAR TEST-L NIL)
(DEFVAR TEST NIL)

(DEFVAR addr-value NIL)

(defun addr-value? ()
  (SETQ addr-value (prompt-and-read ':read "~%At address :"))
    (format terminal-io "THE ADDRESS IS:  ~a." addr-value))

(DEFVAR data-value NIL)

(defun data-value? ()
  (SETQ data-value (prompt-and-read ':read "~% Use data :"))
    (format terminal-io "USING DATA :  ~a." data-value))

(defun BUS-COUPLER-DIAGNOSTIC ()
  "diagnostic of Bus Coupler Board"
  (SEND TERMINAL-IO ':CLEAR-SCREEN)
  (big-letters-L)
  (format t "~%                               GIGAMOS BUS COUPLER DIAGNOSTIC.~10%")
  (format t "~%                             CLICK MOUSE RIGHT ONCE TO SELECT.~%")
  (do-forever
  (selectq (tv:menu-choose '(("SELECT ONE:"          :NO-SELECT NIL)
                             ("MANUAL DEBUG        " :VALUE 1)
                             ("AUTOMATIC DIAGNOSTIC" :VALUE 2)
                             ()
                             ("RETURN  " :value 3))
                           "   Diagnostic Tests   ")
    (1 (SEND TERMINAL-IO ':CLEAR-SCREEN)(setq device-L "manual debug")(small-letters-l)(manual-debug))
    (2 (SEND TERMINAL-IO ':CLEAR-SCREEN)(setq device-L "automatic diagnostic")(small-letters-l)(automatic-diagnostic))
    (3 (return))))
  (small-letters-L))


(DEFUN beeper-L ()
  (DOTIMES (TRACK 5 'beep beep) (beep)))

(defun manual-debug ()
  "test routines for manual debug "
  (do-forever
  (selectq (tv:menu-choose ' (("SELECT ONE :"no-select nil)
                              ("INITIALIZE LOCAL BUS-COUPLER" :value 1)
                              ("VIEW CONTROL REG" :value 2)
                              ("CLEAR CONTROL REG" :value 3)
                              ("READ CONTROL REG LOOP" :value 4)
                              ()
                              ("READ BUS COUPLER LOOP" :value 5)
                              ("WRITE BUS COUPLER B LOOP" :value 6)
                              ("WRITE MAP LOOP (DATA-VALUE)" :value 7)
                              ()
                              ("SET REMOTE SYSTEM" :value 8)
                              ("RESET REMOTE SYSTEM" :value 9)
                              ("SET REMOTE MBC" :value 10)
                              ("RESET REMOTE MBC" :value 11)
                              ("CLEAR MAP" :value 12)
                              ("CLEAR REMOTE MAP" :value 13)
                              ()
                              ("ENABLE TIMEOUT" :value 14)
                              ("DISABLE TIMEOUT" :value 15)
                              ("DISABLE SERIAL PARITY" :value 16)
                              ("ENABLE SERIAL PARITY" :value 17)
                              ("DISABLE TRY AGAIN LATER" :value 18)
                              ("ENABLE TRY AGAIN LATER" :value 19)
                              ("DISABLE REMOTE BUS ADDRESS SPACE" :value 20)
                              ("ENABLE REMOTE BUS ADDRESS SPACE" :value 21)
                              ("RBA MAP RAM TEST" :value 22)
                              ()
                              ("RETURN   " :VALUE 23))
                            " MANUAL DIAGNOSTICS ")
           (1 (send terminal-io ':clear-screen)(setq test "initialize local bus-coupler"))
           (2 (send terminal-io ':clear-screen)(setq test "view-control-reg")(view-control-reg))
           (3 (send terminal-io ':clear-screen)(setq test "clear-control-reg")(clear-control-reg))
           (4 (send terminal-io ':clear-screen)(setq test "read-control-reg-loop")(read-control-reg-loop))
           (5 (send terminal-io ':clear-screen)(setq test "read-bus-coupler-loop")(read-bus-coupler-loop))
           (6 (send terminal-io ':clear-screen)(setq test "write-bus-coupler-b-loop")(write-bus-coupler-b-loop))
           (7 (send terminal-io ':clear-screen)(setq test "write-map-loop (data-value)")(write-map-loop data-value))
           (8 (send terminal-io ':clear-screen)(setq test "set-remote-system")(set-remote-system))
           (9 (send terminal-io ':clear-screen)(setq test "reset-remote-system")(reset-remote-system))
           (10 (send terminal-io ':clear-screen)(setq test "set-remote-mbc")(set-remote-mbc))
           (11 (send terminal-io ':clear-screen)(setq test "reset-remote-mbc")(reset-remote-mbc))
           (12 (send terminal-io ':clear-screen)(setq test "clear-map")(clear-map))
           (13 (send terminal-io ':clear-screen)(setq test "clear-remote-map")(clear-remote-map))

           (14 (send terminal-io ':clear-screen)(setq test "enable-timeout")(enable-timeout))
           (15 (send terminal-io ':clear-screen)(setq test "disable-timeout")(disable-timeout))
           (16 (send terminal-io ':clear-screen)(setq test "disable-serial-parity")(disable-serial-parity))
           (17 (send terminal-io ':clear-screen)(setq test "enable-serial-parity")(enable-serial-parity))
           (18 (send terminal-io ':clear-screen)(setq test "disable-try-again-later")(disable-try-again-later))
           (19 (send terminal-io ':clear-screen)(setq test "enable-try-again-later")(enable-try-again-later))
           (20 (send terminal-io ':clear-screen)(setq test "disable-remote-bus-address-space")
                                                            (disable-remote-bus-address-space))
           (21 (send terminal-io ':clear-screen)(setq test "enable-remote-bus-address-space")
                                                            (enable-remote-bus-address-space))
           (22 (send terminal-io ':clear-screen)(setq test "rba-map-ram-test")
                                                            (rba-map-ram-test))

           (23 (RETURN)))))
                                                ;
;;;;;;manual tests



;;;;;;

(DEFUN AUTOMATIC-DIAGNOSTIC ()
   (Progn () (SEND TERMINAL-IO ':CLEAR-SCREEN)(beeper-l)(setq test-L "AUTOMATIC DIAGNOSTIC TEST 1")     ;1
          (SEND TERMINAL-IO ':CLEAR-SCREEN)(test-local-mbc)(setq test-L "TEST LOCAL MBC")       ;2
          ))


;;;;;;;;;automatic tests

(defun check-for-user-input-L (user-stream)
  (send user-stream ':tyi-no-hang))
