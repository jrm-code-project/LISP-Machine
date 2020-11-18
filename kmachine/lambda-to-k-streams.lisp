;;;-*- Mode:LISP; Package:K2; Base:10; Readtable:CL; -*-

;;;****************************************************************
;;;
;;; Questions
;;;
;;;****************************************************************

;;; who direct maps the kbug communications area ??

;;; can we allocate a new stream in the kbug communications area ??

;;; see rick's memory map in KBUG-GENERIC.LISP

;;;****************************************************************
;;;
;;; Where is it ?
;;;
;;;****************************************************************

;;; COMMON-DEFINITIONS.LISP K-SYS;KBUG2;
;;;  loads on K and Lambda
;;;  defconstants for kbug communications area
;;;  kbug-get/set-comm-word
;;;  accessors for kbug stream slots

;;; STREAMS.LISP  K-SYS;KBUG2;
;;;  loaded on K and Lambda
;;;  kbug-stream-initialize
;;;  kbug-stream-peek-byte/character
;;;  kbug-stream-read/write-byte/character

;;; K2.LISP  K-SYS;KBUG2;
;;;   init-kbug, kbug-trap-handler-1
;;;   print-undefined-symbols  !!!
;;;     *warm-symbols* contains all the warm-intern'ed symbols
;;;     until warm-boot is run
;;;   kbug-stream-test-echo
;;;   save-image

;;; HIGH-LEVEL-STREAMS.LISP  K-SYS;K.LISP-IO;
;;;  commmon lisp streams for the K
;;;  this is the place to hook into lisp

;;; KBUG2.LISP
;;;  lambda stream flavor to do fasdump to K
;;;  defines the *kfasl-stream* which uses KBUG-K-INPUT-FASL-STREAM



;;; mini-lisp-listener/wimp-terminal
;;; fasdump
;;; print-undefined-symbols, etc.

;;; KBUG-K-INPUT-FASL-STREAM
;;;  reads from:   mini-fasl-read-byte mini-fasl-peek-byte  in WARM-LOADER
;;;  writes into:  *kfasl-stream* in KBUG2.LISP

;;; KBUG-K-INPUT-CHARACTER-STREAM
;;;  reads from:   kbug-stream-test-echo, initialize-terminal-input
;;;  writes into:  write-to-k-character-stream (in kbug2.lisp)

;;; KBUG-K-OUTPUT-STREAM
;;;  writes into:  kbug-stream-test-echo  kbug-print-char  (in k2.lisp) initialize-terminal-output
;;;  reads from:   read-from-k-stream

;;;****************************************************************
;;;
;;; OPEN
;;;
;;;****************************************************************

;;; ultimate goal is to define OPEN for the K which will read from the Lambda file system ...
