;;; -*- Mode:LISP; Package:FILE-SYSTEM; Base:8; Readtable:ZL; Fonts:(CPTFONT) -*-
;;
;; Copyright LISP Machine, Inc. 1986
;;   See filename "Copyright" for
;; licensing and release information.



(DEFFLAVOR EXP-PRINTER-HOST
         (DEVICE-NAME)
         (SI:BASIC-HOST)
  (:GETTABLE-INSTANCE-VARIABLES DEVICE-NAME)
  (:INITABLE-INSTANCE-VARIABLES DEVICE-NAME))

(DEFMETHOD (EXP-PRINTER-HOST :NAME) ()
  DEVICE-NAME)

(DEFMETHOD (EXP-PRINTER-HOST :NAME-AS-FILE-COMPUTER) ()
  DEVICE-NAME)

(DEFMETHOD (EXP-PRINTER-HOST :PATHNAME-HOST-NAMEP) (NAME)
  (STRING-EQUAL NAME DEVICE-NAME))

(DEFMETHOD (EXP-PRINTER-HOST :PATHNAME-FLAVOR) ()
  (VALUES 'EXP-PRINTER-PATHNAME NIL))

(DEFFLAVOR EXP-PRINTER-PATHNAME
         ()
         (PATHNAME))

(DEFMETHOD (EXP-PRINTER-PATHNAME :PARSE-NAMESTRING)
           (HOST-SPECIFIED STRING &OPTIONAL (START 0) END)
  HOST-SPECIFIED
  (VALUES :NO-INTERN (MAKE-INSTANCE 'EXP-PRINTER-FILEHANDLE
                                     :NAMESTRING (SUBSTRING STRING START END))))

;NAMESTRING is just for pseudo debugging purposes; Magtape files dont really have names.
(DEFFLAVOR EXP-PRINTER-FILEHANDLE
         (NAMESTRING
          (last-instance nil))
         ()
  (:INITABLE-INSTANCE-VARIABLES NAMESTRING))

(DEFMETHOD (EXP-PRINTER-FILEHANDLE :STRING-FOR-PRINTING) ()
  NAMESTRING)

(DEFMETHOD (EXP-PRINTER-FILEHANDLE :PRINT-SELF) (STREAM PRINDEPTH SLASHIFY-P) PRINDEPTH
  (COND (SLASHIFY-P
         (SEND STREAM :STRING-OUT "#<")
         (PRIN1 'EXP-PRINTER-FILEHANDLE STREAM)
         (FORMAT STREAM " ~S ~O>" NAMESTRING (%POINTER SELF)))
        (T
         (SEND STREAM :STRING-OUT NAMESTRING))))

;This is a kludge to make the copy-patch-files-of-system work.
(DEFMETHOD (EXP-PRINTER-FILEHANDLE :PATCH-FILE-PATHNAME) (&REST IGNORE)
  "EXP-PRINTER:")

(defmethod (EXP-PRINTER-FILEHANDLE :OPEN) (pathname &key flavor-and-init-options)
  (cond ((null flavor-and-init-options)
         (setq flavor-and-init-options default-flavor-and-init-options)))
  (let ((flavor (car flavor-and-init-options))
        (init-options (cdr flavor-and-init-options)))
    (cond ((null flavor-and-init-options)
           pathname)
          ((eq (car flavor-and-init-options) (type-of last-instance))
           last-instance)
          (t
           (setq last-instance (apply 'make-instance flavor init-options))))))

(DEFUN ADD-EXP-PRINTER-HOST (&OPTIONAL (NAME "EXP-PRINTER"))
  (COND ((NULL (GET-PATHNAME-HOST NAME T))
         (LET ((HOST (MAKE-INSTANCE 'EXP-PRINTER-HOST :DEVICE-NAME NAME)))
           (PUSH HOST *PATHNAME-HOST-LIST*)))))

(COMPILE-FLAVOR-METHODS EXP-PRINTER-HOST EXP-PRINTER-PATHNAME EXP-PRINTER-FILEHANDLE)

(ADD-EXP-PRINTER-HOST)
