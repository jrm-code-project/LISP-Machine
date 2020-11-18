;;; -*- Mode:LISP; Package:LISP-IO; Readtable:CL; Base:10 -*-
;;;
;;; LISP-IO-IMPORTS.LISP
;;;
;;; This file contains code to shadowing-import symbols for the LISP-IO
;;; package.  This file should not be compiled into a QFASL file, because it
;;; won't work if loaded that way.


;;; Imports for READTABLE.LISP:

(shadowing-import '(
                    k-lisp:*readtable*
                    k-lisp:readtable
                    k-lisp:copy-readtable
                    k-lisp:readtablep
                    k-lisp:set-syntax-from-char
                    k-lisp:get-macro-character
                    k-lisp:set-macro-character
                    k-lisp:make-dispatch-macro-character
                    k-lisp:get-dispatch-macro-character
                    k-lisp:set-dispatch-macro-character))


;;; Imports for STREAMS.LISP:

(shadowing-import '(
                    k-lisp:read-line
                    k-lisp:read-char
                    k-lisp:unread-char
                    k-lisp:peek-char
                    k-lisp:listen
                    k-lisp:read-char-no-hang
                    k-lisp:clear-input
                    k-lisp:read-from-string
                    k-lisp:read-byte

                    k-lisp:write-char
                    k-lisp:write-string
                    k-lisp:write-line
                    k-lisp:terpri
                    k-lisp:fresh-line
                    k-lisp:finish-output
                    k-lisp:force-output
                    k-lisp:clear-output
                    k-lisp:write-byte
                    k-lisp:stream
                    k-lisp:streamp

                    k-lisp:make-synonym-stream
                    k-lisp:make-broadcast-stream
                    k-lisp:make-concatenated-stream
                    k-lisp:make-two-way-stream
                    k-lisp:make-echo-stream
                    k-lisp:make-string-input-stream
                    k-lisp:make-string-output-stream
                    k-lisp:get-output-stream-string
                    k-lisp:with-open-stream
                    k-lisp:with-input-from-string
                    k-lisp:with-output-to-string
                    k-lisp:input-stream-p
                    k-lisp:output-stream-p
                    k-lisp:stream-element-type
                    k-lisp:close

                    k-lisp:*standard-input*
                    k-lisp:*standard-output*
                    k-lisp:*error-output*
                    k-lisp:*trace-output*
                    ))


;;; Imports for READER.LISP:

(shadowing-import '(
                    k-lisp:*read-default-float-format*
                    k-lisp:read
                    k-lisp:read-preserving-whitespace
                    k-lisp:read-delimited-list
                    k-lisp:parse-integer))


;;; Imports for PRINTER.LISP:

(shadowing-import '(
                    k-lisp:write
                    k-lisp:prin1
                    k-lisp:print
                    k-lisp:pprint
                    k-lisp:princ
                    k-lisp:write-to-string
                    k-lisp:prin1-to-string
                    k-lisp:princ-to-string
                    k-lisp:*print-escape*
                    k-lisp:*print-pretty*
                    k-lisp:*print-circle*
                    k-lisp:*print-base*
                    k-lisp:*print-radix*
                    k-lisp:*print-case*
                    k-lisp:*print-gensym*
                    k-lisp:*print-level*
                    k-lisp:*print-length*
                    k-lisp:*print-array*
                    ))


;;; Imports for FORMAT.LISP:

(shadowing-import 'k-lisp:format)
