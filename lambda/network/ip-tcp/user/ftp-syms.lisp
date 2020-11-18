;;; -*- Mode:LISP; Package:FTP; Readtable:CL; Base:10 -*-

#|

  Copyright LISP Machine, Inc. 1985, 1987
   See filename "Copyright.Text" for
  licensing and release information.


 @(#) ftp.h U. Texas 1.5 83/03/24 from 4.1 (Berkeley) 83/01/13

 Definitions for FTP
 See RFC-765

|#

;; some ascii codes

(defsym CR 13)
(defsym LF 10)

;; Reply codes.

(defsym PRELIM          1       "positive preliminary")
(defsym COMPLETE        2       "positive completion")
(defsym CONTINUE        3       "positive intermediate")
(defsym TRANSIENT       4       "transient negative completion")
(defsym ERROR           5       "permanent negative completion")


;; Type codes

(defsym TYPE_A          1       "ASCII")
(defsym TYPE_E          2       "EBCDIC")
(defsym TYPE_I          3       "image")
(defsym TYPE_L          4       "local byte size")


;; Form codes

(defsym FORM_N          1       "non-print")
(defsym FORM_T          2       "telnet format effectors")
(defsym FORM_C          3       "carriage control (ASA)")


;; Structure codes

(defsym STRU_F          1       "file (no record structure)")
(defsym STRU_R          2       "record structure")
(defsym STRU_P          3       "page structure")


;; Mode types

(defsym MODE_S          1       "stream")
(defsym MODE_B          2       "block")
(defsym MODE_C          3       "compressed")


;; Record Tokens

(defsym REC_ESC         #o377   "Record-mode Escape")
(defsym REC_EOR         #o001   "Record-mode End-of-Record")
(defsym REC_EOF         #o002   "Record-mode End-of-File")


;; Block Header

(defsym BLK_EOR         #x80    "Block is End-of-Record")
(defsym BLK_EOF         #x40    "Block is End-of-File")
(defsym BLK_ERRORS      #x20    "Block is suspected of containing errors")
(defsym BLK_RESTART     #x10    "Block is Restart Marker")

(defsym BLK_BYTECOUNT   2       "Bytes in this block")
