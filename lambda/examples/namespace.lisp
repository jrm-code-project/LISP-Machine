;;; -*- Mode:LISP; Package:CHAOS; Base:10; Readtable:ZL -*-

;;; Copyright LISP Machine, Inc. 1986
;;;   See filename "Copyright.Text" for
;;; licensing and release information.
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

;;; The default value of SI:UNKNOWN-HOST-FUNCTION is CHAOS:CHAOS-UNKNOWN-HOST-FUNCTION.
;;; This creates a stream to the contact "HOSTAB" which receives the name of a host
;;; and then returns lines of info about the host.
;;; The function CHAOS:NAMESPACE-UNKNOWN-HOST-FUNCTION given here is similar,
;;; but it uses the contact "NAMESPACE" instead.
;;;
;;; To make this work you must set up a SITE option :NAMESPACE-SERVER-HOSTS.
;;; It should be an alist with elements of the form ("HOST-NAME" "namespace-name1" "namespace-name2" ...).
;;;
;;; This was done to make life easier for people using our software at MIT.
;;;    24-Apr-86 10:58:27 -George Carrette.

(SETQ SI:UNKNOWN-HOST-FUNCTION 'NAMESPACE-UNKNOWN-HOST-FUNCTION)

(DEFVAR *NAMESPACE-DEBUG* NIL)

(DEFUN WRITE-NAMESPACE-REQUEST (STREAM NAMESPACE CLASS NAME)
  ;; Somebody at MIT pointed this out, namely that it was extremely simple to send
  ;; a namespace request. And so it is. A very simple syntax.
  ;; The syntax of the resulting data from the namespace server was
  ;; so obvious that we didnt even bother to look up the Symbolics documentation
  ;; of it (if any). If you have any troubles with this you might want to look
  ;; at their documentation.
  (WHEN *NAMESPACE-DEBUG*
    (FORMAT T "~&SERVER ON ~S NAMESPACE ~S CLASS ~S NAME ~S"
            (SEND-IF-HANDLES STREAM :CONNECTION)
            NAMESPACE CLASS NAME))
  (format stream "NAMESPACE ~S~%" namespace)
  (format stream "CLASS ~S~%" CLASS)
  (format stream "NAME ~S~~2%" NAME)
  (SEND STREAM :EOF))


(DEFUN NAMESPACE-UNKNOWN-HOST-FUNCTION (NAME)
  (LET ((SI:UNKNOWN-HOST-FUNCTION #'(LAMBDA (X) (FERROR NIL "Unknown host function recursive on ~S" X))))
    (DOLIST (HOST-N (SI:GET-SITE-OPTION :NAMESPACE-SERVER-HOSTS))
      (LET ((HOST (CAR HOST-N)))
        (BLOCK NO-CONTACT
          (AND (SI:PARSE-HOST HOST T ())
               (DOLIST (NAMESPACE (CDR HOST-N))
                 (WITH-OPEN-STREAM (STREAM (OPEN-STREAM HOST "NAMESPACE" :ERROR NIL))
                   (IF (ERRORP STREAM) (RETURN-FROM NO-CONTACT))
                   (WRITE-NAMESPACE-REQUEST STREAM NAMESPACE "HOST" (STRING NAME))
                   (LET ((P (PARSE-NAMESPACE-REPLY STREAM)))
                     (WHEN P
                       (DEFINE-HOST-FROM-NAMESPACE-INFO P)
                       (RETURN-FROM NAMESPACE-UNKNOWN-HOST-FUNCTION T)))))))))))


(DEFUN PARSE-NAMESPACE-REPLY (i)
  (do ((line)
       (names)
       (addresses)
       (system-type)
       (machine-type)
       (temp)
       (token)
       (lmfs))
      ((null (setq line (readline i nil)))
       (if names
           (list :names names :addresses addresses :system-type system-type :machine-type machine-type
                 :lmfs lmfs)))
    (WHEN *NAMESPACE-DEBUG*
      (SEND STANDARD-OUTPUT :LINE-OUT LINE))
    (when (setq temp (string-search " " line))
      (setq token (intern (substring line 0 temp) ""))
      (selectq token
        (:host
         (setq names nil addresses nil system-type nil machine-type nil)
         (push (substring line (1+ temp)) names))
        (:nickname
         (when (not (string-search "|" line))
           (push (substring line (1+ temp)) names)))
        (:address
         (LET ((L (substring line (1+ temp))))
           (WHEN (AND (SETQ TEMP (STRING-SEARCH " " L))
                      (SETQ TOKEN (INTERN (SUBSTRING L 0 TEMP) ""))
                      (GET TOKEN 'NET:ADDRESS-PARSER))
             (PUSH (FUNCALL (GET TOKEN 'NET:ADDRESS-PARSER)
                            TOKEN
                            L (1+ TEMP) (LENGTH L))
                   (CDR (OR (ASSQ TOKEN ADDRESSES)
                            (CAR (PUSH (NCONS TOKEN) ADDRESSES))))))))
        (:machine-type
         (setq machine-type (substring line (1+ temp)))
         (when (string-equal machine-type "3600")
           (push (car names) lmfs)))
        (:system-type
         (setq system-type (substring line (1+ temp))))))))


(DEFUN DEFINE-HOST-FROM-NAMESPACE-INFO (L)
  (WHEN *NAMESPACE-DEBUG*
    (PPRINT L))
  (SI:DEFINE-HOST (CAR (GETF L :NAMES))
                  :HOST-names (getf l :names)
                  :MACHINE-TYPE (intern (canonical-machine-type (getf l :machine-type)) "")
                  :system-type (intern (canonical-system-type (getf l :system-type)) "")
                  :chaos (cdr (assq :chaos (getf l :addresses)))
                  :internet (cdr (assq :internet (getf l :addresses))))
  (when (mem #'string-equal
             (canonical-system-type (getf l :system-type))
             '("UNIX" "TOPS-20" "VMS" "ITS" "LISPM"))
    (fs:add-file-computer (if (getf l :lmfs)
                              (list (car (getf l :names)) :lmfs)
                            (car (getf l :names))))))

(defun canonical-system-type (system-type)
  (COND ((MEM #'string-equal system-type '("TI-LISPM"))
         "LISPM")
        ((MEM #'string-equal system-type '("VMS" "VMS4"))
         "VMS")
        ((MEM #'string-equal system-type '("UNIX" "UNIX42" "ULTRIX"))
         "UNIX")
        ('else
         system-type)))

(defun canonical-machine-type (machine-type)
  (cond ((mem #'string-equal machine-type '("3600" "CADR" "TI-EXPLORER"))
         "LISPM")
        ('else
         machine-type)))
