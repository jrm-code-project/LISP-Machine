;;;-*- Mode:LISP; Package:K2; Base:10; Readtable:CL; -*-

;--------------------- debug communication cluster layout -------------------
; 000     - command
; 001     - status
; 002-009 - command parameters 0-7
; 00A-0EF - internal debug storage
; 0F0-0F4 - K output stream
; 0F5-0F9 - K input fasl stream
; 0FA-0FE - K input character stream (one word per character though)
;   0FF   - unused
; 100-1FF - data transfer area
; 200-3FF - stream buffers

;---------------------- status codes ----------------------------------------
; 0 - busy
; 1 - done - ok
; 2 - unknown command
; 3 - bad parameters
; 4-F - TBD

;---------------------- command codes ----------------------------------------
; 00 - idle
; 01 - continue
; 02 - read register frame
; 03 - read call stack
; 04 - read map
;
; 08 - read virtual memory
; 09 - write virtual memory
; 0a - make string
; 0b - intern symbol
; 0A-0F spare

(defconstant kbug-base-addr #x000800)

;;; Use *magic-garbage-location*
;(defconstant new-math:magic-safe-memory-loc 41.)

(defconstant kbug-command-addr                  (+ kbug-base-addr #x000))
(defconstant kbug-status-addr                   (+ kbug-base-addr #x001))
(defconstant kbug-parameter-addr                (+ kbug-base-addr #x002))
(defconstant kbug-internal-storage-addr         (+ kbug-base-addr #x00A))
(defconstant kbug-pc-addr                       (+ kbug-base-addr #x00A))
(defconstant kbug-flag-addr                     (+ kbug-base-addr #x00B))
(defconstant kbug-left-addr                     (+ kbug-base-addr #x00C))
(defconstant kbug-right-addr                    (+ kbug-base-addr #x00D))
(defconstant kbug-alu-status-addr               (+ kbug-base-addr #x00E))
(defconstant kbug-left-boxed-addr               (+ kbug-base-addr #x00F))
(defconstant kbug-right-boxed-addr              (+ kbug-base-addr #x010))
(defconstant kbug-vma-addr                      (+ kbug-base-addr #x011))
(defconstant kbug-vma-boxed-addr                (+ kbug-base-addr #x012))
(defconstant kbug-md-addr                       (+ kbug-base-addr #x013))
(defconstant kbug-md-boxed-addr                 (+ kbug-base-addr #x014))
(defconstant kbug-mstat-addr                    (+ kbug-base-addr #x015))
(defconstant kbug-data-transfer-area-addr       (+ kbug-base-addr #x100))

;;; These are offsets in the KBUG communication area
(defconstant kbug-k-output-stream               #x0F0)
(defconstant kbug-k-input-fasl-stream           #x0F5)
(defconstant kbug-k-input-character-stream      #x0FA)

(defconstant kbug-stream-buffer-size          #xAA)   ;;;Was #x40, #xAA is maximum currently allocated.  --wkf
(defconstant kbug-output-stream-base          #x200)
(defconstant kbug-input-fasl-stream-base      (+ kbug-output-stream-base     kbug-stream-buffer-size))
(defconstant kbug-input-character-stream-base (+ kbug-input-fasl-stream-base kbug-stream-buffer-size))

;;; kbug-flag bit definitions
(defconstant %%kbug-trace-flag              (byte 1. 0.))
(defconstant %%kbug-entered-flag            (byte 1. 1.))
(defconstant %%kbug-error-flag              (byte 1. 2.))
(defconstant %%kbug-warm-boot-complete-flag (byte 1. 3.))

(defconstant %%command-byte             (byte 5. 0.))
(defconstant kbug-command-idle                  #x00)
(defconstant kbug-command-continue              #x01)
(defconstant kbug-command-read-register-frame   #x02)
(defconstant kbug-command-read-call-stack       #x03)
(defconstant kbug-command-read-map              #x04)
(defconstant kbug-command-read-pc               #x05) ;gone
(defconstant kbug-command-write-pc              #x06) ;gone
(defconstant kbug-command-write-step-mode       #x07) ;gone
(defconstant kbug-command-read-memory           #x08)
(defconstant kbug-command-write-memory          #x09)
(defconstant kbug-command-make-string           #x0A)   ; length in param 0, characters in data area
(defconstant kbug-command-intern                #x0B)   ; param0 package, param1 name
(defconstant kbug-command-load-cold-info        #x0C)   ; warm-download-cold-info
(defconstant kbug-command-fasl-stream           #x0D)   ; mini-fasl
(defconstant kbug-command-pc-to-function        #x0E)   ; get function & symbol from PC
(defconstant kbug-command-read-misc             #x0F)
(defconstant %%misc-command-byte         (byte 5. 0))
(defconstant kbug-command-write-misc            #x10)
(defconstant kbug-command-write-register-frame  #x11)
;;; $$$ Removed kbin which was obsolete and constant same as fbin. <08-Nov-88 wkf>
(defconstant kbug-command-load-fbin             #x12)   ; run k-fasload-internal to load a cross-compiled file

(defconstant kbug-command-last-command          #x13)   ;this MUST be the largest value kbug command
                                                        ;GET-COMMAND in K2.LISP depends on this
                                                        ;if these values change make sure these files
                                                        ;are recompiled: K2.LISP, KBUG2.LISP
                                                        ;(there may be others ...)
(defconstant kbug-status-busy                   #x00)
(defconstant kbug-status-done                   #x01)
(defconstant kbug-status-bad-command            #x02)
(defconstant kbug-status-bad-parameter          #x03)
(defconstant kbug-status-bad-address            #x04)


;;; Each kbug stream contains five words for book-keeping
;;; independent of their buffers.  The things that point to or into buffers are offsets
;;; from the base of the KBUG communication area.
(defconstant kbug-stream-flags          0)
(defconstant kbug-stream-base           1
  "Offset in communication area of the first word of this stream's buffer.")
(defconstant kbug-stream-end            2
  "One more than the offset in the communication area of the last word stream's buffer.")
(defconstant kbug-stream-in-pointer     3
  "Location in buffer where next character to be written to stream is put.")
(defconstant kbug-stream-out-pointer    4
  "Location in buffer from which next character from stream is read.")

;(defconstant kbug-stream-datum         3
;  "Next character to be written to the stream is written here before this is incremented.")
;(defconstant kbug-stream-semaphore     4
;  "Next character to be read from stream is here.  Read this then increment.")

(defconstant %%kbug-stream-flags-direction      (byte 1 0))
(defconstant $$kbug-stream-flags-direction-to-k   0)
(defconstant $$kbug-stream-flags-direction-from-k 1)


;;; A stream is represented by an offset into the KBUG communication area where the
;;; information about the stream lives.  This will be one of KBUG-K-OUTPUT-STREAM,
;;; KBUG-K-INPUT-FASL-STREAM or KBUG-K-INPUT-CHARACTER-STREAM.

;;; Since the LAMBDA can write into these streams, the appropriate thing to do here
;;; is read the word unboxed, and then stuff it into a boxed fixnum.

(defun kbug-get-comm-word (location)
  #+(target falcon) (if (and (<= 0 location)
                             (< location #x400))
                        (progn
                          (hw:vma-start-read-no-transport-vma-unboxed-md-unboxed (+ kbug-base-addr location))
                          (hw:dpb (hw:read-md) vinc:%%fixnum-field 0))
                      (li:error "location not in kbug communication area" location))
  #+(target lambda) (lisp:if (lisp:<= 0 location (lisp:1- #x400))
                             (lisp::ldb (byte 24. 0.)
                                        (lisp:let ((loc (lisp:ash (lisp:+ location kbug-base-addr)
                                                                  2)))
                                          (lam:k-mem-read loc)))
                      (user:ferror nil "location not in kbug communication area")))

(defun kbug-set-comm-word (location value)
  #+(target falcon) (if (and (<= 0 location)
                             (< location #x400))
                        (progn (hw:write-md-unboxed value)
                               (hw:vma-start-write-no-gc-trap-unboxed (+ location kbug-base-addr))
                               (hw:nop))
                      (li:error "location not in kbug communication area" location))
  #+(target lambda) (lisp:if (lisp:<= 0 location (lisp:1- #x400))
                             (lam:k-mem-write (lisp:ash (lisp:+ location kbug-base-addr) 2) value)
                      (user:ferror nil "location not in kbug communication area")))

;;; Accessing the KBUG communication area
;;; The actual data buffer for the stream is allocated at the end of the KBUG communication area.

(prims::defmacro define-kbug-stream-slot (accessor setter lisp:&optional (offset accessor))
  `(PROGN (prims:defmacro ,accessor (STREAM)             ;;;||| Changed into a macro 10/14/88 --wkf
            `(KBUG-GET-COMM-WORD (+ ,STREAM ,',offset)))
          (prims:defmacro ,setter (STREAM VALUE)
            `(KBUG-SET-COMM-WORD (+ ,STREAM ,',offset) ,VALUE))
          (DEFSETF ,accessor ,setter)))

(define-kbug-stream-slot kbug-stream-flags              set-kbug-stream-flags)
(define-kbug-stream-slot kbug-stream-base               set-kbug-stream-base)
(define-kbug-stream-slot kbug-stream-end                set-kbug-stream-end)
(define-kbug-stream-slot kbug-stream-in-pointer         set-kbug-stream-in-pointer)
(define-kbug-stream-slot kbug-stream-out-pointer        set-kbug-stream-out-pointer)
