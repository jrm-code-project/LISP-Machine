;;; -*- Mode:LISP; Package:MAC; Base:10; Readtable:ZL -*-


(defvar mac-host nil)

;;;======================
;;;======================
;;; FROM HOST

(DEFFLAVOR SI:HOST-MAC-MIXIN
      ()
      ()
  (:REQUIRED-FLAVORS SI:HOST))

(DEFPROP :KMAC SI:HOST-MAC-MIXIN SI:SYSTEM-TYPE-FLAVOR)

(DEFMETHOD (SI:HOST-MAC-MIXIN :PRIMARY-DEVICE) () NIL)

(DEFMETHOD (SI:HOST-MAC-MIXIN :HSNAME-PATHNAME) (STRING HOST)
  (FS:PARSE-PATHNAME STRING HOST))


;;;======================
;;; FROM ACCESS


(DEFFLAVOR fs:FILE-HOST-MAC-MIXIN
    ()
    (fs:FILE-HOST-MIXIN))

(DEFMETHOD (fs:FILE-HOST-MAC-MIXIN :PATHNAME-FLAVOR) () 'MAC-PATHNAME)



;(defvar *previous-local-host* si:local-host)

;(defun add-local-pathname-host ()
;  (setq *pathname-host-list* (delq *previous-local-host* *pathname-host-list*))
;  (setq *previous-local-host* si:local-host)
;  (fs:add-file-computer si:local-host))

;(add-initialization "Add Local Pathname Host" '(fs:add-local-pathname-host) '(:warm :now))
;(add-initialization "Add Local Pathname Host" '(fs:add-local-pathname-host) :site)


;;; A special host flavor for the local file system.  The local host used to be
;;; treated specially by the the :PATHNAME-HOST-P method, which compared SELF
;;; against SI:LOCAL-HOST to determine if "LM" was a valid host.  This causes
;;; problems when SI:LOCAL-HOST is not defined, as when changing the name of
;;; a machine and updating the site files.  DEFINE-LOCAL-FILE-SYSTEM-HOST defines
;;; a valid host, connected to the local file system, without concern for any
;;; of the site definitions.  KHS 1/9/85.
;;;
;;; This thing should be added only when there are site information problems;
;;; it causes lot of pain when things are otherwise OK.  We should advertise
;;; the function DEFINE-LOCAL-FILE-SYSTEM-HOST just in case the site information
;;; (or lack of it) requires it.

(DEFFLAVOR fs:MAC-HOST
    ()
    (SI:HOST-MAC-MIXIN fs:FILE-HOST-MAC-MIXIN SI:HOST))

(DEFPROP :KMAC fs:MAC-HOST SI:HOST-FLAVOR)


(DEFMETHOD (fs:MAC-HOST :AFTER :INIT) (&REST IGNORE)
  (SETQ fs:APPROPRIATE-ACCESS-FLAVORS '(MAC-FILE-ACCESS)))

(DEFMETHOD (FS:MAC-HOST :PRIMARY-DEVICE) () NIL)

(DEFMETHOD (fs:MAC-HOST :FILE-SYSTEM-TYPE) () :KMAC)

(DEFMETHOD (fs:MAC-HOST :DETERMINE-ACCESS) ()
  (SETQ fs:ACCESS (MAKE-INSTANCE 'MAC-FILE-ACCESS :HOST SELF)))

(DEFMETHOD (fs:MAC-HOST :GET-ACCESS) ()
  (OR fs:ACCESS (SEND SELF :DETERMINE-ACCESS)))

(DEFMETHOD (fs:MAC-HOST :NAME-AS-FILE-COMPUTER) () "KMAC")


(DEFMETHOD (fs:MAC-HOST :PATHNAME-HOST-NAMEP) (NAME)
  (STRING-EQUAL NAME (SEND SELF :NAME)))



;;(COMPILE-FLAVOR-METHODS fs:FILE-HOST-MAC-MIXIN si:HOST-MAC-MIXIN fs:MAC-HOST)
