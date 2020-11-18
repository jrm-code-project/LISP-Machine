;;; -*-Mode:Lisp;Package:User;Lowercase:T;Fonts:MEDFNB;Base:10;Readtable:T-*-
;;;
;;; System definition for the Lisp Machine mailer
;;;
;;; (c) 1985 Lisp Machine Incorporated
;;;

(defsystem mailer
  (:pathname-default "SYS: NETWORK; MAILER;")
  (:patchable t)
  (:name "Mailer")
  (:module main "MAIN")
  (:module chaos "CHAOS")
  (:compile-load main)
  (:compile-load chaos (:fasload main)))

(defpackage mailer
  (:nicknames "MAIL")
  (:export "*DEBUG-SERVER*" "*DELAY-DELIVERY*" "*MAIL-SERVER-P*"
           "READ-MAILING-LIST-FILE"
           "FORCE-DELIVERY" "INITIALIZE-MAILER" "DISABLE-MAIL-SERVER"
           "ENABLE-MAIL-SERVER" "UPDATE-MAILING-LIST-FILE"))
