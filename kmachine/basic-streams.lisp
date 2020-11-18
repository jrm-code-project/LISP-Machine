;;; -*- Mode:LISP; Package:LISP-INTERNALS; Readtable:CL; Base:10; Lowercase:T -*-
;;;
;;;
;;; BASIC-STREAMS.LISP


;;;----------------------------------------------------------------------------
;;; Representation
;;;----------------------------------------------------------------------------
;;;
;;; A stream is represented as a structure named LI::BASIC-STREAM with 12 slots:
;;;  General:
;;;    (1) name (for print function)
;;;    (2) direction (:NONE, :INPUT, :OUTPUT, or :BIDIRECTIONAL)
;;;    (3) unit-type (:NONE, :CHAR, or :BYTE)
;;;  Input operations:
;;;    (4) end-of-stream-pred
;;;    (5) listen-pred
;;;    (6) read-unit-proc
;;;    (7) unread-unit-proc
;;;    (8) read-line-proc
;;;  Output operations:
;;;    (9) start-of-line-pred
;;;    (10) write-unit-proc
;;;    (11) write-string-proc
;;;  Exceptional cases:
;;;    (12) other-operation-proc
;;;----------------------------------------------------------------------------

(defstruct basic-stream
  (name "Unnamed")
  (direction :NONE)
  (unit-type :NONE)
  (end-of-stream-pred   #'stream-loser-proc)
  (listen-pred          #'stream-loser-proc)
  (read-unit-proc       #'stream-loser-proc)
  (unread-unit-proc     #'stream-loser-proc)
  (read-line-proc       #'stream-loser-proc)
  (start-of-line-pred   #'stream-loser-proc)
  (write-unit-proc      #'stream-loser-proc)
  (write-string-proc    #'stream-loser-proc)
  (other-operation-proc #'stream-loser-proc))

(defun stream-loser-proc (&rest args)
  (error "Illegal stream operation performed on ~A" args))


;;;----------------------------------------------------------------------------
;;; Standard Streams
;;;----------------------------------------------------------------------------

(defvar *standard-inp* :NOTHING-YET)

(defun set-up-basic-streams ()
  (setq *standard-inp*
        (make-basic-stream
          :name                 "Standard Input"
          :direction            :INPUT
          :unit-type            :CHAR
          :end-of-stream-pred   #'(lambda () NIL)
          :listen-pred          #'stream-loser-proc
          :read-unit-proc       #'(lambda () (k2:kbug-stream-read-character
                                               k2:kbug-k-input-stream))
          :unread-unit-proc     #'stream-loser-proc
          :read-line-proc       #'stream-loser-proc
          :other-operation-proc #'stream-loser-proc)))
