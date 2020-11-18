;;;-*- Mode:LISP; Package:K2; Base:10.; Readtable:CL -*-

;;; Definitions in "COMMON-DEFINITIONS"

(defmacro kbug-get-comm-word (location)
  "Read the LOCATION'th word of the KBUG communication area."
;; It is real hard to compile this now.
;  `(if (<= 0 ,location #.(1- #x400))
  `(progn (hw:vma-start-read-no-transport-vma-unboxed-md-unboxed (+ kbug-base-addr ,location))
          (hw:dpb (hw:read-md) vinc:%%fixnum-field 0)))
;     -1))

(defmacro kbug-set-comm-word (location value)
  "Change the value of the LOCATION'th word of the KBUG communication area to VALUE."
;  `(if (<= 0 ,location #.(1- #x400))
       `(progn (hw:write-md-unboxed ,value)
              (hw:vma-start-write-no-gc-trap-unboxed (+ ,location kbug-base-addr))
              (hw:nop)))
;     -1))

(defmacro kbug-read-command ()
  `(progn
     (hw:vma-start-read-no-transport-vma-unboxed-md-unboxed kbug-command-addr)
     (hw:dpb (hw:read-md) vinc:%%fixnum-field 0)))

(defmacro kbug-set-status (data)
  `(progn
     (hw:write-md-unboxed ,data)
     (hw:vma-start-write-no-gc-trap-unboxed kbug-status-addr)
     (hw:nop)))

(defmacro kbug-set-command (data)
  `(progn
     (hw:write-md-unboxed ,data)
     (hw:vma-start-write-no-gc-trap-unboxed kbug-command-addr)
     (hw:nop)))

(defmacro kbug-set-data (addr data)
  `(progn
     (hw:write-md-unboxed ,data)
     (hw:vma-start-write-no-gc-trap-unboxed (+ ,addr kbug-data-transfer-area-addr))
     (hw:nop)))

(defmacro kbug-read-data (addr)
  `(progn
     (hw:vma-start-read-no-transport-vma-unboxed-md-unboxed
       (+ ,addr kbug-data-transfer-area-addr))
     (hw:read-md)))

(defmacro kbug-read-data-and-set-boxed (addr)
  `(progn
     (hw:vma-start-read-no-transport-vma-unboxed-md-boxed
       (+ ,addr kbug-data-transfer-area-addr))
     (hw:read-md)))

(defun kbug-read-data-with-boxed-from-bit (bit n)
  (cond ((zerop (logand 1 bit))
         (kbug-read-data n))
        (t (kbug-read-data-and-set-boxed n))))

(defmacro kbug-read-parameter (n)
  `(progn
     (hw:vma-start-read-no-transport-vma-unboxed-md-unboxed (+ ,n kbug-parameter-addr))
     (hw:read-md)))

(defmacro kbug-read-parameter-and-set-boxed (n)
  `(progn
     (hw:vma-start-read-no-transport-vma-unboxed-md-boxed (+ ,n kbug-parameter-addr))
     (hw:read-md)))

(defun kbug-read-parameter-with-boxed-from-bit (bit n)
  (cond ((zerop (logand 1 bit))
         (kbug-read-parameter n))
        (t (kbug-read-parameter-and-set-boxed n))))

(defmacro kbug-vm-write (addr data)
  `(progn
     (hw:write-md-unboxed ,data)
     (hw:vma-start-write-no-gc-trap-unboxed ,addr)
     (hw:memory-wait)))

(defmacro kbug-vm-read (addr)
  `(progn
     (hw:vma-start-read-no-transport-vma-unboxed-md-unboxed ,addr)
     (hw:read-md)))

(defun set-boxed-bit (x)
  (hw:dpb-boxed x (byte 32. 0) nil))

(defsubst fixnum (x)
  (hw:dpb-boxed x vinc:%%fixnum-field 0))

(defun init-kbug ()
  (kbug-vm-write kbug-flag-addr
                 (hw:dpb (hw:unboxed-constant 0) (byte 2 2)
                         (kbug-vm-read kbug-flag-addr)))   ;; Keep kbug state for trace and entered.  --wkf
  (kbug-set-status kbug-status-busy)
  (kbug-set-command kbug-command-continue)

  (k2::kbug-stream-initialize
    kbug-k-output-stream
    (hw:dpb $$kbug-stream-flags-direction-from-k %%kbug-stream-flags-direction 0)
    kbug-output-stream-base
    (+ kbug-output-stream-base kbug-stream-buffer-size))
  (k2::kbug-stream-initialize
    kbug-k-input-fasl-stream
    (hw:dpb $$kbug-stream-flags-direction-to-k %%kbug-stream-flags-direction 0)
    kbug-input-fasl-stream-base
    (+ kbug-input-fasl-stream-base kbug-stream-buffer-size))
  (k2::kbug-stream-initialize
    kbug-k-input-character-stream
    (hw:dpb $$kbug-stream-flags-direction-to-k %%kbug-stream-flags-direction 0)
    kbug-input-character-stream-base
    (+ kbug-input-character-stream-base kbug-stream-buffer-size))
  (initialize-debug-root)
  )

;;; Need primop for reading and setting Q register
(defun kbug-trap-handler-1 ()
;  (trap:illop "Entering KBUG Trap Handler 1")
  (let ((save-pc      gr::*save-trap-pc*)
        (save-pc+     gr::*save-trap-pc+*)
        (save-oreg    gr::*save-oreg*)
        (save-jcond   gr::*save-jcond*)
        (save-status  gr::*save-status*)
        (save-left    gr::*save-left*)
        (save-right   gr::*save-right*)
        (save-q       (hw:read-q-register))
        (save-async   (vinc:disable-asynchronous-traps))
;       ;;  (save-dttrap  (vinc:disable-datatype-traps)))
        )
    (vinc:disable-single-step-trap)
    (when (= 1 (hw:ldb (hw:trap-off) (byte 1 0) 0))
        (trap:illop "Entered KBUG-TRAP-HANDLER-1 with traps on"))
    (let* (
          ;; Save memory status before fixing the memory system.
          (memstat      (hw:read-memory-status))
          (vma          (hw:read-vma))
          (md (progn
                (when (= hw:$$wmd-valid (hw:ldb memstat hw:%%memory-status-md-written-lately 0))
                  (hw:vma-start-write-no-gc-trap-unboxed trap:*magic-garbage-location*)
                  (hw:memory-wait)
                  (hw:md-start-write-no-gc-trap-unboxed (hw:read-md))
                  (hw:memory-wait))
                (trap:trap-on)
                (hw:read-md))))
      (nubus-stuff:acknowledge-debugger-trap)
      (hw:read-md)                              ;force ack
      (restore-asynchronous-traps save-async)
      (let ((answer
              (kbug-trap-handler-2 save-pc save-left save-right save-status vma md memstat)))
        (hw:read-md)
        (hw:trap-off)
        (vinc:restore-single-step-trap
          (hw:ldb (kbug-vm-read kbug-flag-addr) %%kbug-trace-flag 0))
;       (vinc:restore-datatype-traps save-dttrap)
        (setq gr::*save-right*    answer)
        (setq gr::*save-trap-pc*  (kbug-vm-read kbug-pc-addr))
        (setq gr::*save-trap-pc+* save-pc+)
        (setq gr::*save-oreg*     save-oreg)
        (setq gr::*save-jcond*    save-jcond)
        (setq gr::*save-status*   save-status)
        (hw:load-q-register save-q)
        (hw:write-md-unboxed md)
        (hw:vma-start-write-no-gc-trap-unboxed trap:*magic-garbage-location*)
        (vmem:write-md-generic md   (hw:ldb-not memstat hw:%%memory-status-md-not-boxed-bit  0))
        (vmem:write-vma-generic vma (hw:ldb-not memstat hw:%%memory-status-vma-not-boxed-bit 0))))))



(defun toggle-led (led)
  (vinc:modify-memory-control
    #'(lambda (memctl)
        (hw:dpb-xor 1. led memctl))))

(defconstant *debugger-led-count* 50000.)

;;;; This should work, but the compiler doesn't hack it.
;(defun get-command ()
;  (kbug-set-command kbug-command-idle)
;  (labels ((get-command-loop (n)
;            (let ((command (kbug-read-command)))
;              (if (= command kbug-command-idle)
;                  (maybe-do-leds n)
;                  command)))

;          (maybe-do-leds (n)
;            (if (> n *debugger-led-count*)
;                (progn (toggle-led hw:%%memory-control-led-1)
;                       (get-command-loop 0))
;                (get-command-loop (1+ n)))))
;    (let ((command (get-command-loop 0)))
;      (if (>= command kbug-command-last-command)
;         (progn
;           (kbug-set-status kbug-status-bad-command)
;           (get-command))
;         command))))


(defun get-command-loop (n)
  (let ((command (kbug-read-command)))
    (if (= command kbug-command-idle)
        (maybe-do-leds n)
        command)))

(defun maybe-do-leds (n)
  (if (> n *debugger-led-count*)
      (progn (toggle-led hw:%%memory-control-led-1)
             (get-command-loop 0))
      (get-command-loop (1+ n))))

(defun get-command ()
  (kbug-set-command kbug-command-idle)
  (let ((command (get-command-loop 0)))
    (if (or (>= command kbug-command-last-command)
            ;; if we are in LI:ERROR don't process fasload commands
            (and (not (zerop (hw:ldb (kbug-vm-read kbug-flag-addr) %%kbug-error-flag 0)))
                 (or (= command kbug-command-fasl-stream)
                     (= command kbug-command-load-fbin))))      ; $$$ Changed constant name <08-Nov-88 wkf>
        (progn
          (kbug-set-status kbug-status-bad-command)
          (get-command))
        (progn
          (kbug-set-status kbug-status-busy)
          command))))


(defun kbug-trap-handler-2 (save-pc save-left save-right save-alu-stat vma md memstat)
;  (trap:illop "*****In debugger*******")
;  (let ((flag-word (kbug-vm-read kbug-flag-addr)))
;    (when (zerop (hw:ldb flag-word %%kbug-entered-flag 0))

  ;; I'm putting this next line back in so that I have some way of knowing
  ;; in LI:ERROR, if we are already running inside a KBUG trap.
  ;; this happens when we get errors while fasloading a file ...
  ;; -pfc 8/3
  (kbug-vm-write kbug-flag-addr (hw:dpb-unboxed 1 %%kbug-entered-flag (kbug-vm-read kbug-flag-addr)))

  (kbug-vm-write kbug-flag-addr (hw:dpb 0 %%kbug-trace-flag (kbug-vm-read kbug-flag-addr)))
  (kbug-vm-write kbug-pc-addr save-pc)
  (kbug-vm-write kbug-left-addr save-left)
  (kbug-vm-write kbug-left-boxed-addr (hw::accumulate-box-bits 0 save-left))
  (kbug-vm-write kbug-right-addr save-right)
  (kbug-vm-write kbug-right-boxed-addr (hw::accumulate-box-bits 0 save-right))
  (kbug-vm-write kbug-alu-status-addr save-alu-stat)
  (kbug-vm-write kbug-vma-addr vma)
  (kbug-vm-write kbug-vma-boxed-addr
                 (hw::ldb-not memstat hw:%%memory-status-vma-not-boxed-bit 0))
  (kbug-vm-write kbug-md-addr md)
  (kbug-vm-write kbug-md-boxed-addr
                 (hw::ldb-not memstat hw:%%memory-status-md-not-boxed-bit 0))
  (kbug-vm-write kbug-mstat-addr memstat)
  (kbug-set-status kbug-status-done)
  (do ((command (get-command) (get-command)))
      ((= command kbug-command-continue))
    (prims::dispatch %%command-byte command
                     (kbug-command-read-register-frame (read-reg-frame))
                     (kbug-command-read-call-stack     (read-call-stack))
                     (kbug-command-read-map            (read-map))
                     (kbug-command-read-memory         (read-memory))
                     (kbug-command-write-memory        (write-memory))
                     (kbug-command-make-string         (c-make-string))
                     (kbug-command-intern              (c-intern))
                     (kbug-command-load-cold-info      (kbug-load-cold-info))   ;defined in "WARM-LOADER"
                     (kbug-command-fasl-stream         (kbug-fasl-stream))      ;defined in "WARM-LOADER"
                     (kbug-command-pc-to-function      (kbug-pc-to-function))
                     (kbug-command-read-misc           (kbug-command-read-misc))
                     (kbug-command-write-misc          (kbug-command-write-misc))
                     (kbug-command-write-register-frame (write-reg-frame))
                     (kbug-command-load-fbin           (li:k-fasload-internal)) ; $$$ Changed constant name <08-Nov-88 wkf>
                     (t (kbug-set-status kbug-status-bad-command))))
  (kbug-vm-write kbug-flag-addr
                 (hw:dpb-unboxed 0 %%kbug-entered-flag (kbug-vm-read kbug-flag-addr))))


(defun this-is-a-breakpoint? (pc)
  (let* ((addr (pc->addr pc))
         (insth (array:%vm-read32 addr 1))
         (instl (array:%vm-read32 addr 0)))
    (and (hw:32= (hw:32logand insth (hw:unboxed-constant #x3fffffff)) ;ignore trap & stat bits
                 (hw:unboxed-constant
                   #.(lisp:logand
                       (lisp:ash
                         (nc:assemble-inst
                           'K#:(JUMP 32. (NOP GR:*ZERO*) DT-RIGHT-ARRAY-AND-LEFT-STRUCTURE))
                         -32.)
                       #x3fffffff)))
         (hw:32= (hw:32logand instl (hw:unboxed-constant #xff000000)) ;ignore address
                 (hw:unboxed-constant
                   #.(lisp:logand
                       (nc:assemble-inst
                         'K#:(JUMP 32. (NOP GR:*ZERO*) DT-RIGHT-ARRAY-AND-LEFT-STRUCTURE))
                       #xff000000))))))


(defun boxed-bit (arg)   ;;;@@@ There is probably a way to do this in one instruction.  -wkf
  (let ((bit (hw::accumulate-box-bits 0 arg)))
    (fixnum bit)))

(defun read-call-stack ()
  (read-call-stack-1)
  nil)

(defun read-call-stack-1 ()
  (trap:without-traps
    #'(lambda ()
        (hw:nop)
        (hw:nop)
        (let* ((oar (hw:read-open-active-return))
               (hp-csp (hw:read-call-hp-sp))
               (csp-start (hw:ldb (hw:32-1- hp-csp) (byte 8. 0.) 0)))
          (kbug-set-data 0 csp-start)
          (do ((csp csp-start (1- csp))
               (index 1 (+ index 2)))
              ((minusp csp))
            (hw:write-call-hp-sp (hw:dpb csp (byte 8. 0.) hp-csp))
            (setq gr::*trap-temp1* oar)
            (hw:nop)
            (hw:nop)
            (hw:nop)
            (hw:nop)
            (hw:nop)
            (hw:nop)
            (kbug-set-data index (hw:read-return-pc-return-dest))
            (hw:ch-return)
            (hw:nop)
            (hw:nop)
            (hw:nop)
            (hw:nop)
            (hw:nop)
            (setq gr::*trap-temp2* (hw:read-open-active-return))
            (hw:write-open-active-return gr::*trap-temp1*)
            (hw:nop)
            (hw:nop)
            (hw:nop)
            (hw:nop)
            (hw:nop)
            (hw:nop)
            (kbug-set-data (hw:32-1+ index)
                           (vinc:dpb-multiple-unboxed
                             (hw:ldb gr::*trap-temp2* hw:%%ch-oar-active 0) (byte 8. 0.)
                             (hw:ldb gr::*trap-temp2* hw:%%ch-oar-open   0) (byte 8. 8.)
                             (hw:ldb (hw::read-processor-status) hw:%%processor-status-call-stack-immediate-bits 0)
                                (byte 4. 16.)   ;processor status bits 12-9 have return global frame number
                             0)))
          (hw:write-call-hp-sp hp-csp)
          (hw:nop)
          (hw:nop)
          (hw:nop)
          (hw:nop)
          (hw:nop)
          (hw:nop))
        (kbug-set-status kbug-status-done))))

(defun read-reg-frame ()
  (let ((frame (fixnum (kbug-read-parameter 0))))
    (trap:without-traps
      #'(lambda ()
          (hw:nop)
          (hw:nop)
          (let ((oar (hw:read-open-active-return))
                (boxbits 0))
            (hw:write-open-active-return (hw:dpb frame (byte 8. 16.) oar))
            (hw:nop)
            (hw:nop)
            (hw:nop)
            (hw:nop)
            (hw:nop)
            (hw:nop)
            (kbug-set-data 0. (hw:o0))
            (kbug-set-data 1. (hw:o1))
            (kbug-set-data 2. (hw:o2))
            (kbug-set-data 3. (hw:o3))
            (kbug-set-data 4. (hw:o4))
            (kbug-set-data 5. (hw:o5))
            (kbug-set-data 6. (hw:o6))
            (kbug-set-data 7. (hw:o7))
            (kbug-set-data 8. (hw:o8))
            (kbug-set-data 9. (hw:o9))
            (kbug-set-data 10. (hw:o10))
            (kbug-set-data 11. (hw:o11))
            (kbug-set-data 12. (hw:o12))
            (kbug-set-data 13. (hw:o13))
            (kbug-set-data 14. (hw:o14))
            (kbug-set-data 15. (hw:o15))
            (setq boxbits (hw:accumulate-box-bits boxbits (hw:o15)))
            (setq boxbits (hw:accumulate-box-bits boxbits (hw:o14)))
            (setq boxbits (hw:accumulate-box-bits boxbits (hw:o13)))
            (setq boxbits (hw:accumulate-box-bits boxbits (hw:o12)))
            (setq boxbits (hw:accumulate-box-bits boxbits (hw:o11)))
            (setq boxbits (hw:accumulate-box-bits boxbits (hw:o10)))
            (setq boxbits (hw:accumulate-box-bits boxbits (hw:o9)))
            (setq boxbits (hw:accumulate-box-bits boxbits (hw:o8)))
            (setq boxbits (hw:accumulate-box-bits boxbits (hw:o7)))
            (setq boxbits (hw:accumulate-box-bits boxbits (hw:o6)))
            (setq boxbits (hw:accumulate-box-bits boxbits (hw:o5)))
            (setq boxbits (hw:accumulate-box-bits boxbits (hw:o4)))
            (setq boxbits (hw:accumulate-box-bits boxbits (hw:o3)))
            (setq boxbits (hw:accumulate-box-bits boxbits (hw:o2)))
            (setq boxbits (hw:accumulate-box-bits boxbits (hw:o1)))
            (setq boxbits (hw:accumulate-box-bits boxbits (hw:o0)))
            (kbug-set-data 16. boxbits)
            (hw:write-open-active-return oar)))))
  (hw:nop)
  (hw:nop)
  (hw:nop)
  (hw:nop)
  (hw:nop)
  (hw:nop)
  (kbug-set-status kbug-status-done))


(defun write-reg-frame ()
  (let ((frame (fixnum (kbug-read-parameter 0))))
    (trap:without-traps
      #'(lambda ()
          (hw:nop)
          (hw:nop)
          (let ((oar (hw:read-open-active-return))
                (boxbits (kbug-read-data 16.)))
            (hw:write-open-active-return (hw:dpb frame (byte 8. 16.) oar))
            (hw:nop)
            (hw:nop)
            (hw:nop)
            (hw:nop)
            (hw:nop)
            (hw:nop)
    (setf (hw:o0) (kbug-read-data-with-boxed-from-bit 0 boxbits))
    (setf (hw:o1) (kbug-read-data-with-boxed-from-bit 1 (hw:ldb boxbits (byte 1 1) 0)))
    (setf (hw:o2) (kbug-read-data-with-boxed-from-bit 2 (hw:ldb boxbits (byte 1 2) 0)))
    (setf (hw:o3) (kbug-read-data-with-boxed-from-bit 3 (hw:ldb boxbits (byte 1 3) 0)))
    (setf (hw:o4) (kbug-read-data-with-boxed-from-bit 4 (hw:ldb boxbits (byte 1 4) 0)))
    (setf (hw:o5) (kbug-read-data-with-boxed-from-bit 5 (hw:ldb boxbits (byte 1 5) 0)))
    (setf (hw:o6) (kbug-read-data-with-boxed-from-bit 6 (hw:ldb boxbits (byte 1 6) 0)))
    (setf (hw:o7) (kbug-read-data-with-boxed-from-bit 7 (hw:ldb boxbits (byte 1 7) 0)))
    (setf (hw:o8) (kbug-read-data-with-boxed-from-bit 8 (hw:ldb boxbits (byte 1 8) 0)))
    (setf (hw:o9) (kbug-read-data-with-boxed-from-bit 9 (hw:ldb boxbits (byte 1 9) 0)))
    (setf (hw:o10) (kbug-read-data-with-boxed-from-bit 10 (hw:ldb boxbits (byte 1 10) 0)))
    (setf (hw:o11) (kbug-read-data-with-boxed-from-bit 11 (hw:ldb boxbits (byte 1 11) 0)))
    (setf (hw:o12) (kbug-read-data-with-boxed-from-bit 12 (hw:ldb boxbits (byte 1 12) 0)))
    (setf (hw:o13) (kbug-read-data-with-boxed-from-bit 13 (hw:ldb boxbits (byte 1 13) 0)))
    (setf (hw:o14) (kbug-read-data-with-boxed-from-bit 14 (hw:ldb boxbits (byte 1 14) 0)))
    (setf (hw:o15) (kbug-read-data-with-boxed-from-bit 15 (hw:ldb boxbits (byte 1 15) 0)))

            (hw:write-open-active-return oar)))))
  (hw:nop)
  (hw:nop)
  (hw:nop)
  (hw:nop)
  (hw:nop)
  (hw:nop)
  (kbug-set-status kbug-status-done))


(defun read-map ()
  (trap:without-traps
    #'(lambda ()
        (do ((count 0 (1+ count))
             (cluster (fixnum (kbug-read-parameter 0)) (1+ cluster)))
            ((= count 256.))
          (kbug-set-data count (map:read-map cluster)))))
        (kbug-set-status kbug-status-done))

(defun read-memory ()           ;this reads virtual memory.  If pages were swapped out, they would just
  (let ((base-addr (kbug-read-parameter 0))     ;get bad-address status, which might not be too much of a win.
        (temp nil)
        (max-count (fixnum (kbug-read-parameter 1))))
    (when (zerop max-count) (setq max-count 256.))
    (do ((count 0 (1+ count))
         (addr base-addr (hw:24+ 1 addr)))
        ((= count max-count))
      (unless (valid-address? addr)             ; $$$ this put back in.  Was commented out. <21-Nov-88 rg>
        (kbug-set-status kbug-status-bad-address)
        (return-from read-memory))
      (hw:vma-start-read-no-transport-vma-unboxed-md-unboxed addr)
      (kbug-set-data count (hw:read-md)))
    (kbug-set-status kbug-status-done)))

(defun write-memory ()
  (let ((status kbug-status-done))
    (map-fault:call-while-allowing-write-in-read-only
      #'(lambda ()
          (let ((base-addr (kbug-read-parameter 0))
                (temp nil)
                (max-count (fixnum (kbug-read-parameter 1))))
            (when (zerop max-count) (setq max-count 256.))
            (do ((count 0 (1+ count))
                 (addr base-addr (hw:24+ 1 addr)))
                ((= count max-count))
              (unless (valid-address? addr)
                (kbug-set-status kbug-status-bad-address)
                (return-from write-memory))
              (hw:write-md-unboxed (kbug-read-data count))
              (hw:vma-start-write-no-gc-trap-unboxed addr)
              (hw:read-md)))))
    (kbug-set-status status)))

(defun valid-address? (addr)
  (hw:read-md)
  (trap:without-traps
    #'(lambda ()
        (hw:write-vma-unboxed addr)
        (hw:nop)
        (hw:nop)
        (hw:nop)
        (hw:nop)
        (hw:32logbitp 0 (map:hw-read-map-safe)))))      ; $$$ Changed to call hw-read-map-safe <15-Nov-88 JIM>

(defun writeable-address? (addr)
  (hw:read-md)
  (trap:without-traps
    #'(lambda ()
        (hw:write-vma-unboxed addr)
        (hw:nop)
        (hw:nop)
        (hw:nop)
        (hw:nop)
        ;; $$$ Changed to call hw-read-map-safe <15-Nov-88 JIM>
        (hw:field= 3 (map:hw-read-map-safe) (byte 2 0)))))


(defun kbug-pc-to-function ()
  (let* ((pc (fixnum (kbug-read-parameter 0)))
         (addr (pc->addr pc))
         (function (and
                     (>= pc 64.)
                     (valid-address? addr)
                     (get-function-and-offset-from-pc pc))))
    (kbug-set-data 0 function)
    (kbug-set-data 1 (and function (%compiled-function-name function)))
    (kbug-set-status kbug-status-done)))

(defun kbug-command-read-misc ()
  (let ((sub-command (kbug-read-parameter 0))
        (arg (kbug-read-parameter 1)))          ;note arg is an UNBOXED 32 bit quantity.
    (prims:dispatch %%misc-command-byte sub-command
     (0 (kbug-set-data 0 (hw:32-1+ arg)))
     (1 (kbug-set-data 1 0))
     (2 (kbug-set-data 0 (transporter-ram:read-transporter-ram-address
                           (hw:ldb arg vinc:%%fixnum-field 0))))        ;read transporter ram.
     (3 (kbug-set-data 0 (gc-ram:read-gc-ram arg)))
     )
    (kbug-set-status kbug-status-done))
  )

(defun kbug-command-write-misc ()
  (let ((sub-command (kbug-read-parameter 0))
        (arg (kbug-read-parameter 1))           ;Note arg and data are UNBOXED 32 bit quantities.
        (data (kbug-read-parameter 2)))
    (prims:dispatch %%misc-command-byte sub-command
     (0 (kbug-set-data 0 (hw:32-1+ arg)))
     (1 (kbug-set-data 1 0))
     (2 (transporter-ram:write-transporter-ram-address
          (hw:ldb arg vinc:%%fixnum-field 0)
          (hw:ldb data vinc:%%fixnum-field 0)))
     (3 (gc-ram:write-gc-ram arg data))
     )
    (kbug-set-status kbug-status-done))  )

(defun c-intern ()
  ;;; Parameter[0] is symbol name as a string.
  ;;; Parameter[1] is package name as a string.
  ;;; Returns symbol in Data[0].
  (let ((name       (set-boxed-bit (kbug-read-parameter 0)))
        (package    (set-boxed-bit (kbug-read-parameter 1)))
        k-symbol)
    (setq k-symbol (warm-intern name package))
    (kbug-set-data 0 k-symbol)
    (kbug-set-status kbug-status-done)))

(defun c-make-string ()
  ;;; Parameter[0] is the length of the string.
  ;;; There are that many character objects (or maybe fixnums)
  ;;; in KBUG-DATA-TRANSFER-AREA-ADDR.
  ;;; Returns string in Data[0].
  (let* ((string-length (fixnum (kbug-read-parameter 0)))
         (string (li:make-string string-length)))
    (dotimes (i string-length)
             (array:aset-1
               (hw:dpb-boxed (kbug-read-data i) (byte 8. 0.) gr:*dtp-character*)
               string i))
    (kbug-set-data 0 string)
    (kbug-set-status kbug-status-done)))

;;; A simple test function for KBUG streams:
(defun kbug-stream-test-echo ()
  (loop
    (kbug-stream-write-character KBUG-K-OUTPUT-STREAM
      (kbug-stream-read-character KBUG-K-INPUT-CHARACTER-STREAM))))


(defun kbug-print-char (c)
  (kbug-stream-write-character KBUG-K-OUTPUT-STREAM c))

(defun kbug-print-string (s)
  (li::dotimes (i (array:length s))
    (kbug-print-char (array:svref s i))))

(defun kbug-print-return ()
  (kbug-print-char #\return))

(defun kbug-print-symbol (sym)
  (kbug-print-string (symbol:symbol-package sym))
  (kbug-print-char #\:)
  (kbug-print-string (symbol:symbol-name sym)))

(defun print-undefined-symbols ()
  (li::dolist (symbol gr:*warm-symbols*)
    (when (symbol:fboundp symbol)
      (let ((function (symbol:symbol-function symbol)))
        (when (%compiled-function-p function)
          (let* ((refs     (%compiled-function-refs function))
                 (ref-len  (array:length refs)))
            (do ((i 0 (+ i 3)))
                ((>= i ref-len))
              (let ((called-fcn-name (array:svref refs (+ i 1))))
                (unless (symbol:fboundp called-fcn-name)
                  (kbug-print-return)
                  (kbug-print-symbol called-fcn-name)
                  (kbug-print-string " is undefined in ")
                  (kbug-print-symbol symbol)))))))))
  (loop))


;header  - 40 words
;  proc status
;  proc control
;  mem status
;  mem control
;  trap register
;  stat counter
;  vma
;  md
;  bits:    md-boxed,   vma-boxed
;  hp-csp   (byte 8 8) is "heap pointer" (actually free list pointer)
;           (byte 8 0) is call stack pointer.
;  oar
;  q
;  pc
;-- add floating point stuff --

; registers  256x (17 word block)  <word-of-boxed-bits> <16 word-frame>
; call stack    (256 x )
; gc-transporter-ram  4Kx8 bits
; datatype ram 128kx1 bits
; map (16k words)
; opcs

(defmacro image-vm-write (base data offset)
  `(progn
     (hw:write-md-unboxed ,data)
     (hw:vma-start-write-no-gc-trap-unboxed (hw:32+ ,offset ,base))
     (hw:memory-wait)))

(defmacro image-vm-read (base offset)
  `(progn
     (hw:vma-start-read-no-transport-vma-unboxed-md-unboxed (hw:32+ ,offset ,base))
     (hw:read-md)))

;----------

(defvar *falcon-lowlevel-root-region*) ;region which "holds" stuff which "should" have been direct mapped, such as below.
(defvar *falcon-debug-root-cluster*)
(defvar *falcon-communication-root-cluster*)

(defun initialize-debug-root ()
  (if (not (symbol::boundp '*falcon-lowlevel-root-region*))
      (setq *falcon-lowlevel-root-region* (boot::read-boot-vector boot::*bv-lowlevel-root-region*)))
  (if (not (symbol::boundp '*falcon-debug-root-cluster*))
      (setq *falcon-debug-root-cluster* (boot::read-boot-vector boot::*bv-debug-root-cluster*)))
  (if (not (symbol::boundp '*falcon-communication-root-cluster*))
      (setq *falcon-communication-root-cluster* (boot::read-boot-vector boot::*bv-communication-root-cluster*)))
  )


(defun make-processor-image ()
  (array::zl-make-array 6000. :element-type '(li:unsigned-byte 32.))  ;may want a leader.
  )

(defun save-processor-image (base-image-address)
 ;base-image-address might be an array or it might just be raw memory.
 ; dont touch offset zero, since that might be the array header.
 ; store stuff starting at offset 1.

  ;micro save to temp frame
  ;save for debugging, we cant really restore this.
  (hw:trap-off)
  (let ((stat (hw:read-alu-status)))    ;this expands into a ref to GR:*ZERO*, so we cant set GR:*STATE-BITS* directly!!
    (setq gr:*state-processor-status* (hw:read-processor-status)
          gr:*state-bits* stat          ;must be after previous term to avoid compiler lossage!
          gr:*state-processor-control* (hw:read-processor-control)
          gr:*state-memory-status* (hw:read-memory-status)
          gr:*state-memory-control* (hw:read-memory-control)
          gr:*state-trap-register* (hw:read-trap-register)
          gr:*state-vma*        (hw:read-vma)))
  ;look at md-written-lately, etc.
  (WHEN (= HW:$$WMD-VALID (HW:LDB gr:*state-memory-status*
                                  HW:%%MEMORY-STATUS-MD-WRITTEN-LATELY 0))
  ;this not what was in TRAP-HANDLERS.   Check that.
    (HW:VMA-START-WRITE-NO-GC-TRAP-UNBOXED TRAP:*MAGIC-GARBAGE-LOCATION*)
    (HW:MEMORY-WAIT)
    (hw:vma-start-read-no-transport-vma-unboxed-md-unboxed trap:*magic-garbage-location*))
  (setq gr:*state-md*   (hw:read-md))

 ;these are in the same order as the list at the end of KBUG-GENERIC.
  (image-vm-write base-image-address gr:*state-processor-status* 1)
  (image-vm-write base-image-address gr:*state-memory-status* 2)
  (image-vm-write base-image-address gr:*state-trap-register* 3)
  (image-vm-write base-image-address (hw:read-microsecond-clock) 4)
  ;(image-vm-write base-image-address (hw:read-statistics-counter) 5)  ;temp
 ;begin main state.  This presumably can get restored.
  (image-vm-write base-image-address gr:*state-processor-control* 11.)
  (image-vm-write base-image-address gr:*state-memory-control* 12.)
  (image-vm-write base-image-address (hw:read-open-active-return) 13.)
  (image-vm-write base-image-address (hw:read-call-hp-sp) 14.)
  (image-vm-write base-image-address (hw:read-q-register) 15.)
  (image-vm-write base-image-address gr:*state-vma* 16.)
  (image-vm-write base-image-address (hw:ldb-not gr:*state-memory-status*
                                                 hw:%%memory-status-vma-not-boxed-bit
                                                 0) 17.)                ;vma boxed
  (image-vm-write base-image-address gr:*state-md* 18.)
  (image-vm-write base-image-address (hw:ldb-not gr:*state-memory-status*
                                                 hw:%%memory-status-md-not-boxed-bit
                                                 0) 19.)                ;md boxed
  ;call stack 512. words starting at 30.
  (let* ((oar (hw:read-open-active-return))
         (hp-csp (hw:read-call-hp-sp))
         (csp-start (hw:ldb (hw:32-1- hp-csp) (byte 8. 0.) 0)))
    (do ((csp csp-start (1- csp))
         (index 30. (+ index 2)))
        ((minusp csp))
      (hw:write-call-hp-sp (hw:dpb csp (byte 8. 0.) hp-csp))
      (setq gr::*trap-temp1* oar)
      (hw:nop)
      (hw:nop)
      (hw:nop)
      (hw:nop)
      (hw:nop)
      (hw:nop)
      (image-vm-write base-image-address (hw:read-return-pc-return-dest) index)
      (hw:ch-return)
      (hw:nop)
      (hw:nop)
      (hw:nop)
      (hw:nop)
      (hw:nop)
      (setq gr::*trap-temp2* (hw:read-open-active-return))
      (hw:write-open-active-return gr::*trap-temp1*)
      (hw:nop)
      (hw:nop)
      (hw:nop)
      (hw:nop)
      (hw:nop)
      (hw:nop)
      (image-vm-write base-image-address
                      (vinc:dpb-multiple-unboxed
                        (hw:ldb gr::*trap-temp2* hw:%%ch-oar-active 0) (byte 8. 0.)
                        (hw:ldb gr::*trap-temp2* hw:%%ch-oar-open   0) (byte 8. 8.)
                        (hw:ldb (hw::read-processor-status)
                                hw:%%processor-status-call-stack-immediate-bits 0)
                        (byte 4. 16.) ;processor status bits 12-9 have return global frame number
                        0)
                      (1+ index))
      (hw:write-call-hp-sp hp-csp)
      (hw:nop)
      (hw:nop)
      (hw:nop)
      (hw:nop)
      (hw:nop)
      (hw:nop)))


  ;frames 4352. words starting at 542.

  (do ((frame 0 (1+ frame))
       (index 542. (+ index 17.)))
      ((= frame 256.))
    (let ((oar (hw:read-open-active-return))
          (boxbits 0))
      (hw:write-open-active-return (hw:dpb frame (byte 8. 16.) oar))
      (hw:nop)
      (hw:nop)
      (hw:nop)
      (hw:nop)
      (hw:nop)
      (hw:nop)
      (setq boxbits (hw:accumulate-box-bits boxbits (hw:o15)))
      (setq boxbits (hw:accumulate-box-bits boxbits (hw:o14)))
      (setq boxbits (hw:accumulate-box-bits boxbits (hw:o13)))
      (setq boxbits (hw:accumulate-box-bits boxbits (hw:o12)))
      (setq boxbits (hw:accumulate-box-bits boxbits (hw:o11)))
      (setq boxbits (hw:accumulate-box-bits boxbits (hw:o10)))
      (setq boxbits (hw:accumulate-box-bits boxbits (hw:o9)))
      (setq boxbits (hw:accumulate-box-bits boxbits (hw:o8)))
      (setq boxbits (hw:accumulate-box-bits boxbits (hw:o7)))
      (setq boxbits (hw:accumulate-box-bits boxbits (hw:o6)))
      (setq boxbits (hw:accumulate-box-bits boxbits (hw:o5)))
      (setq boxbits (hw:accumulate-box-bits boxbits (hw:o4)))
      (setq boxbits (hw:accumulate-box-bits boxbits (hw:o3)))
      (setq boxbits (hw:accumulate-box-bits boxbits (hw:o2)))
      (setq boxbits (hw:accumulate-box-bits boxbits (hw:o1)))
      (setq boxbits (hw:accumulate-box-bits boxbits (hw:o0)))
      (image-vm-write base-image-address  boxbits (+ 16. index))
      (image-vm-write base-image-address  (hw:o0) index)
      (image-vm-write base-image-address  (hw:o1) (1+ index))
      (image-vm-write base-image-address  (hw:o2) (+ 2. index))
      (image-vm-write base-image-address  (hw:o3) (+ 3. index))
      (image-vm-write base-image-address  (hw:o4) (+ 4. index))
      (image-vm-write base-image-address  (hw:o5) (+ 5. index))
      (image-vm-write base-image-address  (hw:o6) (+ 6. index))
      (image-vm-write base-image-address  (hw:o7) (+ 7. index))
      (image-vm-write base-image-address  (hw:o8) (+ 8. index))
      (image-vm-write base-image-address  (hw:o9) (+ 9. index))
      (image-vm-write base-image-address  (hw:o10) (+ 10. index))
      (image-vm-write base-image-address  (hw:o11) (+ 11. index))
      (image-vm-write base-image-address  (hw:o12) (+ 12. index))
      (image-vm-write base-image-address  (hw:o13) (+ 13. index))
      (image-vm-write base-image-address  (hw:o14) (+ 14. index))
      (image-vm-write base-image-address  (hw:o15) (+ 15. index))
      (hw:write-open-active-return oar)
      (hw:nop)
      (hw:nop)
      (hw:nop)
      (hw:nop)
      (hw:nop)
      (hw:nop)))
  )

(defun restore-processor-image (base-image-address return-action)
  ;** this is not really the right thing its the save-processor-image code as a place holder.
  (hw:trap-off)
  (let ((stat  (hw:read-alu-status)))
    (setq gr:*state-processor-status* (hw:read-processor-status)
          gr:*state-bits* stat
          gr:*state-processor-control* (hw:read-processor-control)
          gr:*state-memory-status* (hw:read-memory-status)
          gr:*state-memory-control* (hw:read-memory-control)
          gr:*state-trap-register* (hw:read-trap-register)
          gr:*state-vma*        (hw:read-vma)))
    (WHEN (= HW:$$WMD-VALID (HW:LDB gr:*state-memory-status*
                                  HW:%%MEMORY-STATUS-MD-WRITTEN-LATELY 0))
  ;this not what was in TRAP-HANDLERS.   Check that.
    (HW:VMA-START-WRITE-NO-GC-TRAP-UNBOXED TRAP:*MAGIC-GARBAGE-LOCATION*)
    (HW:MEMORY-WAIT)
    (hw:vma-start-read-no-transport-vma-unboxed-md-unboxed trap:*magic-garbage-location*))
  (setq gr:*state-md*   (hw:read-md))

 ;these are in the same order as the list at the end of KBUG-GENERIC.
  (image-vm-write base-image-address (hw:read-processor-status) 1)
  (image-vm-write base-image-address (hw:read-memory-status) 2)
  (image-vm-write base-image-address (hw:read-trap-register) 3)
  (image-vm-write base-image-address (hw:read-microsecond-clock) 4)
  ;(image-vm-write base-image-address (hw:read-statistics-counter) 5)  ;temp
 ;begin main state.  This presumably can get restored.
  (image-vm-write base-image-address (hw:read-processor-control) 11.)
  (image-vm-write base-image-address (hw:read-memory-control) 12.)
  (image-vm-write base-image-address (hw:read-open-active-return) 13.)
  (image-vm-write base-image-address (hw:read-call-hp-sp) 14.)
  (image-vm-write base-image-address (hw:read-q-register) 15.)
  (image-vm-write base-image-address gr:*state-vma* 16.)
  (image-vm-write base-image-address (hw:ldb-not gr:*state-memory-status*
                                                 hw:%%memory-status-vma-not-boxed-bit
                                                 0) 17.)                ;vma boxed
  (image-vm-write base-image-address gr:*state-md* 18.)
  (image-vm-write base-image-address (hw:ldb-not gr:*state-memory-status*
                                                 hw:%%memory-status-md-not-boxed-bit
                                                 0) 19.)                ;md boxed
  ;call stack 512. words starting at 30.
  (let* ((oar (hw:read-open-active-return))
         (hp-csp (hw:read-call-hp-sp))
         (csp-start (hw:ldb (hw:32-1- hp-csp) (byte 8. 0.) 0)))
    (do ((csp csp-start (1- csp))
         (index 30. (+ index 2)))
        ((minusp csp))
      (hw:write-call-hp-sp (hw:dpb csp (byte 8. 0.) hp-csp))
      (setq gr::*trap-temp1* oar)
      (hw:nop)
      (hw:nop)
      (hw:nop)
      (hw:nop)
      (hw:nop)
      (hw:nop)
      (image-vm-write base-image-address (hw:read-return-pc-return-dest) index)
      (hw:ch-return)
      (hw:nop)
      (hw:nop)
      (hw:nop)
      (hw:nop)
      (hw:nop)
      (setq gr::*trap-temp2* (hw:read-open-active-return))
      (hw:write-open-active-return gr::*trap-temp1*)
      (hw:nop)
      (hw:nop)
      (hw:nop)
      (hw:nop)
      (hw:nop)
      (hw:nop)
      (image-vm-write base-image-address
                      (vinc:dpb-multiple-unboxed
                        (hw:ldb gr::*trap-temp2* hw:%%ch-oar-active 0) (byte 8. 0.)
                        (hw:ldb gr::*trap-temp2* hw:%%ch-oar-open   0) (byte 8. 8.)
                        (hw:ldb (hw::read-processor-status)
                                hw:%%processor-status-call-stack-immediate-bits 0)
                        (byte 4. 16.) ;processor status bits 12-9 have return global frame number
                        0)
                      (1+ index))
      (hw:write-call-hp-sp hp-csp)
      (hw:nop)
      (hw:nop)
      (hw:nop)
      (hw:nop)
      (hw:nop)
      (hw:nop)))
  ;frames 4352. words starting at 542.

  (do ((frame 0 (1+ frame))
       (index 542. (+ index 17.)))
      ((= frame 256.))
    (let ((oar (hw:read-open-active-return))
          (boxbits 0))
      (hw:write-open-active-return (hw:dpb frame (byte 8. 16.) oar))
      (hw:nop)
      (hw:nop)
      (hw:nop)
      (hw:nop)
      (hw:nop)
      (hw:nop)
      (setq boxbits (hw:accumulate-box-bits boxbits (hw:o15)))
      (setq boxbits (hw:accumulate-box-bits boxbits (hw:o14)))
      (setq boxbits (hw:accumulate-box-bits boxbits (hw:o13)))
      (setq boxbits (hw:accumulate-box-bits boxbits (hw:o12)))
      (setq boxbits (hw:accumulate-box-bits boxbits (hw:o11)))
      (setq boxbits (hw:accumulate-box-bits boxbits (hw:o10)))
      (setq boxbits (hw:accumulate-box-bits boxbits (hw:o9)))
      (setq boxbits (hw:accumulate-box-bits boxbits (hw:o8)))
      (setq boxbits (hw:accumulate-box-bits boxbits (hw:o7)))
      (setq boxbits (hw:accumulate-box-bits boxbits (hw:o6)))
      (setq boxbits (hw:accumulate-box-bits boxbits (hw:o5)))
      (setq boxbits (hw:accumulate-box-bits boxbits (hw:o4)))
      (setq boxbits (hw:accumulate-box-bits boxbits (hw:o3)))
      (setq boxbits (hw:accumulate-box-bits boxbits (hw:o2)))
      (setq boxbits (hw:accumulate-box-bits boxbits (hw:o1)))
      (setq boxbits (hw:accumulate-box-bits boxbits (hw:o0)))
      (image-vm-write base-image-address  boxbits (+ 16. index))
      (image-vm-write base-image-address  (hw:o0) index)
      (image-vm-write base-image-address  (hw:o1) (1+ index))
      (image-vm-write base-image-address  (hw:o2) (+ 2. index))
      (image-vm-write base-image-address  (hw:o3) (+ 3. index))
      (image-vm-write base-image-address  (hw:o4) (+ 4. index))
      (image-vm-write base-image-address  (hw:o5) (+ 5. index))
      (image-vm-write base-image-address  (hw:o6) (+ 6. index))
      (image-vm-write base-image-address  (hw:o7) (+ 7. index))
      (image-vm-write base-image-address  (hw:o8) (+ 8. index))
      (image-vm-write base-image-address  (hw:o9) (+ 9. index))
      (image-vm-write base-image-address  (hw:o10) (+ 10. index))
      (image-vm-write base-image-address  (hw:o11) (+ 11. index))
      (image-vm-write base-image-address  (hw:o12) (+ 12. index))
      (image-vm-write base-image-address  (hw:o13) (+ 13. index))
      (image-vm-write base-image-address  (hw:o14) (+ 14. index))
      (image-vm-write base-image-address  (hw:o15) (+ 15. index))
      (hw:write-open-active-return oar)
      (hw:nop)
      (hw:nop)
      (hw:nop)
      (hw:nop)
      (hw:nop)
      (hw:nop)))
  )
