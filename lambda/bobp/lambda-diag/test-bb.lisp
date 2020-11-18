;;;-*-Package:lambda; Base:8.;  Mode:Lisp-*-

test board independent of nu-debug board
  careful to not write garbage to control lines is nd is plugged into self
  drive data; read back
    walk bits
    all 0 or 1? cable reversed?
  drive control; read back

test board through nu-debug, assuming plugged into self
  what does reversed cable look like?
    nd in self vs. other box
  read, compare sysconf area with si:%nubus-read
  memtest user-shared area

(defun walk-bits (slot addr n-bits)
  "walk low n-bits at nu-addr"
  (dotimes (i n-bits)
    (let ((write-data (ash 1 i)))
      (si:%nubus-write slot addr write-data)
      (let ((read-data (si:%nubus-read slot addr)))
        (if (not (= write-data
                    (ldb (byte n-bits 0) read-data)))
            (format t "~&~x~x: wrote ~x read ~x" slot addr write-data read-data))))))

(defun test-bb ()
  (bb-hack-init)
  (setq sys-conf-nubus-adr (si:%processor-conf-sys-conf-ptr si:*my-proc-conf*))
  (read-compare-sysconf)
  )

(defun bb-hack-init ()
  (setq bb-address #x2ff00)                     ;"primary" burr-brown card
  (bb-nd-reset))

(defun read-compare-sysconf ()
  )

(defun hack-read-sysconf ()
  (dotimes (i 10)
    (let ((adr (+ #xfe1ffc00 (* 4 i))))
      (format t "~&~16r ~d" adr (bb-nd-bus-read adr)))))


;-----

;mode reg bits
;
;  0 hi or lo mode
;  1 reset
;  2 byte

;;      mode            address         read            write
;;      0               0               mode reg        mode reg
;;      0               1               nc              low data
;;      0               2               nc              high data
;;      0               3               nc              nc

;;      1               0               mode reg        mode reg
;;      1               1               start read      start write
;;      1               2               low data        low address
;;      1               3               high data       high address

;-----
;stuff for lambda with burr-brown to nu-debug  5/22/85  -pace

;2ff00 for first board (currently lam9)  - also for Ken Ks machine
;2fe00 for second board (currently exp1)
;this is set in the function SETUP
(defvar bb-address)


(defsubst bb-drive-data ()
  (si:%nubus-write-8 si:sdu-quad-slot (+ bb-address 2) 7))

(defsubst bb-dont-drive-data ()
  (si:%nubus-write-8 si:sdu-quad-slot (+ bb-address 2) 4))

(defun bb-read-csr ()
  (si:%nubus-read-8 si:sdu-quad-slot (+ bb-address 2)))

(defvar *delay-loops* 0)

(defmacro delay ()
  `(dotimes (x *delay-loops*)))

(defsubst bb-nd-read (reg)
  (delay)
  (bb-dont-drive-data)
  ;;send the address down
  (si:%nubus-write-8 si:sdu-quad-slot
                     (+ bb-address 6)
                     (+ 0                       ;not req.l
                        4
                        (logxor 3 reg)))
  (delay)
  ;;strobe it
  (si:%nubus-write-8 si:sdu-quad-slot
                     (+ bb-address 6)
                     (+ 8                       ;req.l
                        4
                        (logxor 3 reg)))
  (delay)
  (prog1
    ;;get data
    (ldb (byte 16. 0) (%nubus-read si:sdu-quad-slot (+ bb-address 4)))
    ;;turn off strobe
    (si:%nubus-write-8 si:sdu-quad-slot
                       (+ bb-address 6)
                       (+ 0
                          4
                          (logxor 3 reg)))
         ))

(defmacro bb-nd-write-strobe (reg)
  (cond ((numberp reg)
         `(progn
            (si:%nubus-write-8 #xff bb-address-plus-6 ,(+ 8 (logxor 3 reg)))
            (si:%nubus-write-8 #xff bb-address-plus-6 ,(logxor 3 reg))))
        (t
         `(let ((inv-reg (logxor 3 ,reg)))
            (si:%nubus-write-8 #xff bb-address-plus-6 (+ 8 inv-reg))    ;req.l
            (si:%nubus-write-8 #xff bb-address-plus-6 inv-reg)))))

(defmacro bb-nd-write-lo-data-wires (data)
  `(si:%nubus-write-8 #xff bb-address-plus-4 ,data))

(defmacro bb-nd-write-hi-data-wires (data)
  `(si:%nubus-write-8 #xff bb-address-plus-5 ,data))


(defsubst bb-nd-write (reg data)
  (let ((ireg (logxor 3 reg)))
    ;;send address
    (si:%nubus-write-8 si:sdu-quad-slot
                       (+ bb-address 6)
                       (+ 0
                          ireg))
    (si:%nubus-write-8 si:sdu-quad-slot (+ bb-address 4) data)
    (si:%nubus-write-8 si:sdu-quad-slot (+ bb-address 5) (ldb (byte 8 8) data))
    ;;assert data
    (bb-drive-data)

    (delay)

    (let ((bb-address-plus-6 (+ bb-address 6)))
      (bb-nd-write-strobe reg))
    ))


(defsubst bb-nd-write-data (data)
  (bb-nd-write 0 0)
  (bb-nd-write 1 (ldb (byte 16. 0) data))
  (bb-nd-write 2 (ldb (byte 16. 16.) data)))

(defsubst bb-nd-read-data ()
  (bb-nd-write 0 1)
  (dpb (bb-nd-read 3) (byte 16. 16.) (bb-nd-read 2)))

(defsubst bb-nd-write-adr (adr)
  (bb-nd-write 0 1)
  (bb-nd-write 2 (ldb (byte 16. 0) adr))
  (bb-nd-write 3 (ldb (byte 16. 16.) adr)))

(defsubst bb-nd-start-write ()
  (bb-nd-write 0 5)
  (bb-nd-write 1 0))

(defsubst bb-nd-start-write-byte ()
  (bb-nd-write 0 1)
  (bb-nd-write 1 0))

(defsubst bb-nd-start-read ()
  (bb-nd-write 0 5)
  (bb-nd-read 1))

(defsubst bb-nd-start-read-byte ()
  (bb-nd-write 0 1)
  (bb-nd-read 1))

(defun bb-nd-read-fastest-loop (adr)
  (bb-nd-reset)
  (bb-nd-write-adr adr)
  (bb-nd-write 0 5)
  (do-forever
    (bb-nd-read 1)
 ))

(defun bb-nd-bus-read (adr &optional ignore-bus-errors byte-mode
                       &aux loop-until-it-works (start-time (time)))
  (prog ()
   retry
      (bb-nd-write-adr adr)
      (if byte-mode
          (bb-nd-start-read-byte)
        (bb-nd-start-read))
      (dotimes (i 10.))
      (case (ldb (byte 2 8) (bb-nd-read 0))
        (0 ;try again later
         (cond ((and (null loop-until-it-works)
                     (time-lessp 30. (time-difference (time) start-time)))
                (signal-proceed-case
                  (() 'nubus-timeout
                      "nubus try-again-later too many times: adr #x~16r" adr 'try-again-later)
                  (:retry-bus-cycle
                   (setq start-time (time))
                   (go retry))
                  (:loop-until-it-works
                   (setq loop-until-it-works t)
                   (go retry))
                  (:ignore-bus-error-read (return -1))
                  ))
               (t
                (go retry))))
        (1                                      ;bus timeout
         (cond ((not (null loop-until-it-works))
                (go retry))
               ((and (not ignore-bus-errors) check-for-nubus-timeouts)
                (signal-proceed-case
                  (() 'nubus-timeout
                      "nubus timeout: adr = #x~16r" adr 'nubus-timeout)
                  (:retry-bus-cycle (go retry))
                  (:loop-until-it-works
                   (setq loop-until-it-works t)
                   (go retry))
                  (:ignore-bus-error-read (return -1))
                  ))
               (t
                (return -1))))
        (2                                      ;parity error
         (cond ((not (null loop-until-it-works))
                (go retry))
               ((and (not ignore-bus-errors) check-for-nubus-timeouts)
                (signal-proceed-case
                  (() 'nubus-timeout
                               "parity or other nubus error: adr = #x~x" adr 'parity-error)
                  (:retry-bus-cycle (go retry))
                  (:loop-until-it-works
                   (setq loop-until-it-works t)
                   (go retry))
                  (:ignore-bus-error-read (return -1))
                  ))
               (t
                (return -1))))
        (3                                      ;normal
         (return (bb-nd-read-data))))))

(defun bb-nd-bus-write (adr data &optional ignore-bus-errors byte-mode
                        &aux loop-until-it-works (start-time (time)))
  ignore-bus-errors
  (prog ()
     retry
        (bb-nd-write-adr adr)
        (bb-nd-write-data data)
        (if byte-mode
            (bb-nd-start-write-byte)
          (bb-nd-start-write))
        (dotimes (i 10.))
        (case (ldb (byte 2 8) (bb-nd-read 0))
          (0
           (cond ((and (null loop-until-it-works)
                       (time-lessp 30. (time-difference (time) start-time)))
                  (signal-proceed-case
                    (() 'nubus-timeout
                        "try-again-later too many times: adr=#x~x)"
                        adr 'try-again-later)
                    (:retry-bus-cycle
                     (setq start-time (time))
                     (go retry))
                    (:loop-until-it-works
                     (setq loop-until-it-works t)
                     (go retry))
                    (:ignore-bus-error (return -1))
                    ))
                 (t
                  (go retry))))
          (1                                    ; bus timeout
           (cond ((not (null loop-until-it-works))
                  (go retry))
                 (check-for-nubus-timeouts
                  (signal-proceed-case
                    (() 'nubus-timeout
                        "nubus timeout: adr = #x~x)"
                        adr 'nubus-timeout)
                    (:retry-bus-cycle (go retry))
                    (:loop-until-it-works
                     (setq loop-until-it-works t)
                     (go retry))
                    (:ignore-bus-error (return nil))))
                 (t
                  (return nil))))
          (2                                    ; other bus error - maybe parity
           (cond ((not (null loop-until-it-works))
                  (go retry))
                 (check-for-nubus-timeouts
                  (signal-proceed-case
                    (() 'nubus-timeout
                        "other nubus error (parity?): adr = #x~x)"
                        adr 'parity-error)
                    (:retry-bus-cycle (go retry))
                    (:loop-until-it-works
                     (setq loop-until-it-works t)
                     (go retry)
                     (:ignore-bus-error (return nil)))))
                 (t
                  (return nil))))
          (3                                    ; normal
           (return nil)))))


(defun bb-nd-reset ()
  (bb-nd-write 0 2) ;reset
  (bb-nd-write 0 0)
  (bb-nd-write 0 4)
  (bb-nd-read 0)
  )
