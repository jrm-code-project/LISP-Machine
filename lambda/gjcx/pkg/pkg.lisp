;;; -*- Mode:LISP; Package:USER; Base:10; Readtable:ZL -*-

;;; (C) Copyright 1987, LISP MACHINE INC
;;; See filename "Copyright.Text" for more information.
;;; *********************************************************
;;; *********************************************************
;;; *** NOTE: This is an EXAMPLE, not LMI supported code. ***
;;; *** information contained in this example is subject  ***
;;; *** to change without notice. The ways of doing       ***
;;; *** the things contained in the example may change    ***
;;; *** between system releases. Some techniques which    ***
;;; *** are mere examples in one release may become built ***
;;; *** in system features in the next release. Use good  ***
;;; *** judgement when copying these techniques. Most     ***
;;; *** examples have been motivated by specific customer ***
;;; *** requests, and may not be the best engineered      ***
;;; *** or most efficient solution for someone else.      ***
;;; *********************************************************
;;; *********************************************************


(defpackage xdr
  (:size 250)
  (:export "XDRSTDIO_CREATE"
           "XDRMEM_CREATE"
           "XDRREC_CREATE"
           "XDRREC_ENDOFRECORD"
           "XDRREC_SKIPRECORD"
           "EXDRREC_EOF"
           "XDR_TRANSMIT"
           "XDR_RECEIVE"

           "INT" "U_INT"
           "LONG" "U_LONG"
           "SHORT" "U_SHORT"
           "FLOAT" "DOUBLE"
           "ENUM"
           "BOOLE"
           "STRING" "BYTES" "ARRAY" "OPAQUE"
           "UNION" "REFERENCE"
           "XDR_GETPOS"
           "XDR_SETPOS"
           "XDR_DESTROY"

           "XDR_STRUCT"
           "XDR_ENUMERATION"
           "ENUMERATION"
           "XDR_DESCRIMINATED_UNION"

           "XDR_INLINE"

           "DESCRIMINATOR"
           "DESCRIMINATED-VALUE"

           "DEFAULT"


           "VOID"
           "XDR_GETPOS"

           "XDR_MAKE_STRUCTURE"
           "XDR_STRUCTURE"
           "XDR_STRUCTURE_SET"
           "XDR_STRUCTURE_REF"

           "XDR_MAKE_DESCRIMINATION"

           "XDR_TYPEDEF"

           "DESCRIMINATION"

           "XDRSIZE_CREATE"

           ))


(DEFPACKAGE rpc
  (:use xdr lisp global)
  (:export "MAKE-RPC-UDP-CALL")
  (:size 250))


(DEFPACKAGE SUN
  (:use xdr rpc lisp global)
  (:size 1000))
