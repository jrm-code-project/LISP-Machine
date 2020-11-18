;;; -*- Mode:LISP; Package:TIMERS; Base:10; Readtable:CL -*-


;;; Code for handling the 1024 and 16384 microsecond interrupts.

(defconstant %%sb-code-timer    (byte 1. 0.))
(defconstant %%sb-code-io       (byte 1. 1.))

(defconstant $$sequence-break    1.)
(defconstant $$no-sequence-break 0.)

(defsubst pulse-timer-interrupt-enable (interrupt-bit)
  ;; Pulsing acknowledges the interrupt.
  (let ((memctl (hw:read-memory-control)))
    (hw:write-memory-control
      (hw:dpb-unboxed hw:$$timer-interrupt-disable-reset
                      interrupt-bit
                      memctl))
    (hw:nop)
    (hw:write-memory-control memctl)
    (hw:nop)
    (hw:nop)
    nil
    ))

;;; We get called with from the trap handler with traps off.
;;; This is a quick routine, and we would waste time by
;;; dealing with turning traps on, adjusting the trap mask,
;;; etc.  Instead, we just blast away.

(defun handle-16384-microsecond-interrupt ()
  ;(trap::illop "Reached 16384 interrupt handler.")
  ;; Reset the interrupt
  (pulse-timer-interrupt-enable hw:%%memory-control-16384-interrupt)
  (update-time 16384.)
  nil)
;  (when (= $$sequence-break (hw:ldb gr::*sequence-break-code* %%sb-code-timer 0.))
;    (update-sequence-break-count))
;  (when (and gr::*request-sequence-break* gr::*allow-sequence-break*)
;    (trap::illop "What exactly is a sequence break?"))
;  )

(defun handle-1024-microsecond-interrupt ()
  ;; Just run one of these at once.
  ;; Reset the interrupt
  (pulse-timer-interrupt-enable hw:%%memory-control-1024-interrupt)
  (update-time 1024.)
  nil)

;;; Sequence break counter.

;;; This counter is set by the user to cause sequence breaks to occur.

(defun reset-sequence-break-count ()
  (setq gr::*ticks-till-next-sequence-break* gr::*ticks-between-sequence-break-requests*))

(defun update-sequence-break-count ()
  (if (< gr::*ticks-till-next-sequence-break* 0)
      (setq gr::*request-sequence-break* t)
      (decf gr::*ticks-till-next-sequence-break*)))

;;; We have a counter that counts down from a million.
;;; Every time we get a timer interrupt, we subtract the
;;; apropriate time in microseconds from the current counter.
;;; When the counter goes negative, about one second has elapsed.
;;; We then add one million to make it positive again.  This keeps
;;; the cumulative error at zero though at any one time, the error
;;; can be up to 1/60 second (or more if traps are off).

(defconstant one-million 1000000.)

(defun update-time (n)
  ;;; This routine is called with traps off (I hope).
;  (trap::illop "Update time.")
  (setq gr::*one-million-count* (- gr::*one-million-count* n))
  (when (minusp gr::*one-million-count*)
;    (trap::illop "Toggle time LED.")
    (setq gr::*one-million-count* (+ gr::*one-million-count* one-million))
    ;; Now increment global time, this will overflow at
    ;; 01:28:15 on February 7, 2036
    ;; I won't have to debug it...
    (setq gr::*elapsed-time-since-1900*
          (hw:32+ gr::*elapsed-time-since-1900* (hw:unboxed-constant 1.)))
    ;; **debugging** Toggle one of the LEDs
    (hw:write-memory-control
      (hw:dpb-xor 1. hw:%%memory-control-led-0 (hw:read-memory-control)))
    (hw:nop))
    nil)
