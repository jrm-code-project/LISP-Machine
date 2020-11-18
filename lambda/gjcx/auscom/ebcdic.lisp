;;; -*- Mode:LISP; Package:AUSCOM; Base:10; Readtable:CL -*-

;;;  6/19/86 09:15:02 the time has come, EBCDIC <=> LISPM character translation.
;;;
;;; - GEORGE CARRETTE


;; ?1 ?2 ?3 ?4 are funny characters I could not name.
;; CENT is the cent-sign which has no lisp equivalent in most LISPM fonts.
;; "" an empty string is an empty EBCDIC table entry.
;; Symbols are taken from the EBCDIC table.
;;

(defvar *ebcdic-table*
        '(( HEX    0    1    2    3    4    5    6    7    8    9    S    B    C    D    E    F)
          (   0  NUL  DLE   DS   ""   SP  "&"  "-"   ""   ""   ""   ""   ""  "{"  "}" "\\"  "0")
          (   1  SOH  DC1  SOS   ""  RSP   ""  "/"   ""  "a"  "j"  "~"   ""  "A"  "J"  NSP  "1")
          (   2  STX  DC2   FS  SYN   ""   ""   ""   ""  "b"  "k"  "s"   ""  "B"  "K"  "S"  "2")
          (   3  ETC  DC3  WUS   IR   ""   ""   ""   ""  "c"  "l"  "t"   ""  "C"  "L"  "T"  "3")
          (   4  SEL  ENP  INP   PP   ""   ""   ""   ""  "d"  "m"  "u"   ""  "D"  "M"  "U"  "4")
          (   5   HT   NL   LF   RS   ""   ""   ""   ""  "e"  "n"  "v"   ""  "E"  "N"  "V"  "5")
          (   6   ""   BS  ETB  NBS   ""   ""   ""   ""  "f"  "o"  "w"   ""  "F"  "O"  "W"  "6")
          (   7  DEL  POC  ESC  EOT   ""   ""   ""   ""  "g"  "p"  "x"   ""  "G"  "P"  "X"  "7")
          (   8   GE  CAN   ""  SBS   ""   ""   ""   ""  "h"  "q"  "y"   ""  "H"  "Q"  "Y"  "8")
          (   9  SPS   EM   ""   IT   ""   ""   ""   ""  "i"  "r"  "z"   ""  "I"  "R"  "Z"  "9")
          (   A  RPT  UBS   SM  RFF CENT  "!"   ?1  ":"   ""   ""   ""   ""  SHY   ""   ""   "")
          (   B   VT  CU1  FMT  CU3  "."  "$"  ","  "#"   ""   ""   ""   ""   ""   ""   ""   "")
          (   C   FF  IFS   ""   ""  "<"  "*"  "%"  "@"   ""   ""   ""   ""   ?2   ""   ?3   "")
          (   D   CR  IGS  ENQ  NAK  "("  ")"  "_"  "'"   ""   ""   ""   ""   ""   ""   ""   "")
          (   E   SO  IRS  ACK   ""  "+"  ";"  ">"  "="   ""   ""   ""   ""   ?4   ""   ""   "")
          (   F   SI  ITB  BEL  SUB  "|"  ?4   "?" "\""   ""   ""   ""   ""   ""   ""   ""   EO)))


(defvar *lispm->ebcdic* nil)


(defvar *ebcdic->lispm* nil)


(defun string-translate-in-place (table string &optional (start 0) (end (length string)))
  (do ((j start (1+ j)))
      ((= j end))
    (setf (aref string j) (or (aref table (aref string j)) 0))))

(defun process-ebcdic-table ()
  (setq *lispm->ebcdic* (make-array 256))
  (setq *ebcdic->lispm* (make-array 256))
  (dolist (row (cdr *ebcdic-table*))
    (do ((low-bits (if (numberp (car row)) (car row) (digit-char-p (aref (get-pname (car row)) 0) 16)))
         (hi-bits 0 (1+ hi-bits))
         (l (cdr row) (cdr l)))
        ((null l))
      (let ((ebcdic-code (dpb hi-bits (byte 4 4) low-bits))
            (lispm-char (cond ((symbolp (car l))
                               (get-symbol-lispm-char (car l)))
                              ((zerop (length (car l)))
                               ())
                              ('else
                               (aref (car l) 0)))))
        (when lispm-char
          (setf (aref *lispm->ebcdic* (char-code lispm-char)) ebcdic-code)
          (setf (aref *ebcdic->lispm* ebcdic-code) lispm-char))))))


(defun get-symbol-lispm-char (symbol)
  (let ((c (cdr (assq (intern (get-pname symbol) "") si:xr-special-character-names))))
    (when c
      (code-char c))))



(process-ebcdic-table)
