;;; -*- Mode:LISP; Package:MAIL; Base:10; Readtable:T; Lowercase:T -*-
;;;
;;; Lisp Machine Mailer
;;;
;;; (c) 1985 Lisp Machine Incorporated
;;;
;;; Changes:
;;;   Sometime, someone: written
;;;   RpK, 7 July 1983: Made to use NES, WITH-OPEN-FILE-CASE
;;;   Rpk, 8 July 1983: Works.  Delayed delivery implemented for more than
;;;      *DELAY-DELIVERY-THRESHOLD* (3 is the default).
;;;   RpK, 20 December 1984: Clean up for the modern world, customers.
;;;      Now it always queues...
;;;   RpK, 4 January 1985: Support Chaosnet. *delay-delivery* helps debugging
;;;   RpK, 8 January 1985: Be a real system.

(defvar *debug-server* ())

(defmacro ignoring-errors (&body body)
  `(condition-call-if (not *debug-server*) (.condition.) (progn ,@body t)
     ((and (condition-typep .condition. 'error)
           (not (or (send .condition. :debugging-condition-p)
                    (send .condition. :dangerous-condition-p)
                    (condition-typep .condition. 'cerror))))
      nil)))

(defvar *delay-delivery* () "If T, do not actually deliver")
(defvar *mailing-lists* nil)
(define-site-variable *mail-server-p* :mail-server "Am I a mail server ?")

(defmacro find-in-mailing-lists (string)
  #+explorer `(ass #'string-equal ,string *mailing-lists*)
  #-explorer `(cli:assoc ,string *mailing-lists* :test #'string-equal))

(defvar *mailing-list-file* "LM:MAIL;LIST.LISP")
(defvar *mailing-list-version-number* nil)

(defvar *unknown-addresses* ())
(defvar *mailer-initialized* ())
;;; For now only allow one file for all servers.
(defvar *mail-server-lock* nil)

(defvar *server-enabled* nil)
(defvar *server-disable-reason* "Mailer not initialized")

(defvar *history* nil)

(add-initialization "Reset Mailer History" '(setq *history* nil) '(:before-cold))

(defvar *notify-mode* nil "Can be NIL, meaning not to record anything, or:
 :NOTIFY   Do a window notification
 :OUTPUT   Print the notification on *TERMINAL-IO*
 :HISTORY  Push the note on the history.  Use PRINT-HISTORY")

(defun notify (format-string &rest format-args)
  (case *notify-mode*
    (:notify (tv:notify () format-string format-args))
    (:output
     (fresh-line *terminal-io*)
     (write-string "[Mailer: " *terminal-io*)
     (apply #'format *terminal-io* format-string format-args)
     (write-line "]" *terminal-io*))
    (:history
     (push (cons (get-universal-time) (apply #'format nil format-string format-args))
           *history*))))

(defun print-history () "Print out the activity of the Mailer"
  (dolist (h *history*)
    (fresh-line)
    (time:print-universal-time (car h))
    (write-string "   ")
    (write-string (cdr h))))

;;; The queue file format is depressingly simple.  The first part is a cons, whose CDR is
;;; plist and whose CAR is a list of recipients of the form (type . options).  Later on the
;;; CDR will have more interesting information in it (for use with GET).
;;; Standard mailer per-message properties:
;;;  :FAILURES, a number
;;;  :FIRST-FAILURE-DATE also a number, a universal time
(defvar *qfile-template* () "Set by initialising the mailer")

(defvar *qfile-name-counter* 0)
(defun qfile-name (&optional (name (format () "~16R-~D"
                                           (time:get-universal-time)
                                           (without-interrupts (incf *qfile-name-counter*)))))
  (send *qfile-template* :new-name (string name)))

;;; Currently we have just (:NAME . uname), (:FILE . filename) and
;;; (:FOREIGN address-for-host original-address host-or-class) (see below)
(defstruct (address (:type :list*) :conc-name)
  type options)

;;; A class symbol describes what kind of delivery to use.  No classes are currently implemented,
;;; but there will be some provided so that the Lisp Machine can act as a mail gateway.
(defstruct (foreign-address (:type :list) (:conc-name foreign-)
                            (:but-first address-options))
  (mailer-address () :documentation "what to use when contacting a mail server")
  (original-address ()
   :read-only t
   :documentation "what was received by the mailer, usually containing @")
  (class () :documentation "either a delivery class or a host to contact"))

(defvar *failed-address* (make-address :type :file :options "MAIL;FAILED-MAIL.TEXT"))

;;; Buffer resource
(defresource text-buffer (size)
  :constructor (make-array (or size 1000.) :fill-pointer 0 :element-type 'string-char)
  :initializer (setf (fill-pointer object) 0)
  :matcher (or (null size)
               (<= size (array-total-size object))))

(defmacro with-text-buffer ((buffer stream) &body body)
  "This is very useful with GET-MAIL-TEXT"
  `(using-resource (,buffer text-buffer (send ,stream :send-if-handles :length))
     ,@body))

(DEFUN GET-MAIL-TEXT (STREAM buffer)
  (WITH-OUTPUT-TO-STRING (SSTREAM buffer)
    (STREAM-COPY-UNTIL-EOF STREAM SSTREAM)
    (fresh-line sstream)))

;;; This ought to be globalized (net-ized ?), really
(defmacro with-open-network-stream ((stream form) &body body)
  "Like WITH-OPEN-STREAM, but ALWAYS uses abort mode."
  `(let ((,stream))
     (unwind-protect
         (progn
           (setq ,stream ,form)
           ,@body)
       (when ,stream (lisp:close ,stream :abort t)))))

(defmacro as-mail-server ((stream form) &body body)
  `(with-open-network-stream (,stream ,form)
     (cond (*server-enabled*
            (send ,stream :accept)
            ,@body)
           (t
            ;; Any :close after :reject should be harmless
            (send ,stream :reject *server-disable-reason*)))))

;;; There really ought to be a system-wide function do this.
(defun probe-directory (p)
  "Return non-NIL if the directory P exists.
Can get an error if the superior directory does not exist."
  (let ((dir (send (pathname p) :directory-pathname-as-file)))
    (probe-file dir)))

(defun terminal-warn-function (severity stream &rest format-args)
  (fresh-line stream)
  (write-string (symbol-name severity) stream)
  (write-string ": " stream)
  (apply #'format stream format-args)
  (fresh-line stream))

(defvar *warn-function* 'terminal-warn-function
  "Should take arguments of SEVERITY STREAM &REST FORMAT-ARGS")

(defun warn-stream (type stream &rest format-args)
  "Print a warning onto STREAM, which may be a server stream.
The FORMAT output should not output a newline.  TYPE is a keyword,
in the set {:DISK-FULL, :RANDOM-ERROR, :APPEND-ERROR}."
  (apply *warn-function* type stream format-args))

;;; This always appends new mail.
;;; Retuns either T (for success) or an error instance.  In the latter case, the
;;; negative or temporary negative message has already been sent.
;;; Does not rename the mail file at the present.  Too bad LMFS can't append.
;;; Locks out all other MAIL SERVER  FILE System activity.
(DEFUN FILE-DELIVER-MAIL (ADDRESS PATHNAME TEXT STREAM &OPTIONAL (INCLUDE-TAIL T))
  (WITH-LOCK (*MAIL-SERVER-LOCK*)
    (PROG ()
          (WITH-OPEN-FILE-CASE (INFILE PATHNAME :DIRECTION :INPUT)
            (FS:FILE-NOT-FOUND
             (RETURN (FILE-DELIVER-MAIL-INTERNAL ADDRESS () PATHNAME TEXT STREAM INCLUDE-TAIL)))
            (ERROR (warn-stream :append-error STREAM
                                "Error opening for append: ~A" (SEND INFILE :REPORT-STRING))
                   (RETURN INFILE))
            (:NO-ERROR
             (RETURN (FILE-DELIVER-MAIL-INTERNAL ADDRESS INFILE PATHNAME TEXT STREAM
                                                 INCLUDE-TAIL)))))))

(DEFUNP FILE-DELIVER-MAIL-INTERNAL (ADDRESS INFILE PATHNAME TEXT STREAM INCLUDE-TAIL)
  (WITH-OPEN-FILE-CASE (OUTFILE PATHNAME :DIRECTION :OUTPUT)
    (FS:NO-MORE-ROOM
     (warn-stream :disk-full stream "Disk full, please try later.")
     (RETURN OUTFILE))
    (ERROR
     (warn-stream :random-error STREAM "Unexpected error for ~A: ~A"
                  ADDRESS (SEND OUTFILE :REPORT-STRING))
     (FORCE-OUTPUT STREAM)
     (RETURN OUTFILE))
    (:NO-ERROR
     (IF INFILE (STREAM-COPY-UNTIL-EOF INFILE OUTFILE))
     (write-STRING TEXT OUTFILE)
     (IF INCLUDE-TAIL (write-LINE "" OUTFILE))
     (IF INFILE (SEND INFILE :DELETE))
     (FORCE-OUTPUT STREAM))
     (RETURN T)))

(defun write-queue-file (qfile recipients properties text stream)
  (let ((*print-level* ()) (*print-length* ()) (*print-base* 10.)
        (*package* si:pkg-user-package) (*print-array* t)
        (*READTABLE* SI:STANDARD-READTABLE))
    (file-deliver-mail "LIST" qfile
                       (format () "~S~A" (cons recipients properties) text)
                       stream
                       ())))

;;; This needs to be everytime the machine boots the mailer.
(defvar *file-defaults* :unbound)

(defun mail-file-for-address (address)
  (ecase (address-type address)
    (:name (make-pathname :host si:local-host
                          :directory (address-options address)
                          :name "MAIL"
                          :TYPE "TEXT"
                          :VERSION :NEWEST))
    (:FILE (merge-pathnames (address-options address) *file-defaults*))))

;;; Delivery
(defvar *delivery-methods* nil)

(defmacro define-delivery-method (type (address) filter (addresses plist text report-stream) &body delivery)
  `(progn
     (defun (:property ,type deliver-filter) (,address)
       ,filter)
     (defun (:property ,type deliver-driver) (,addresses ,plist ,text ,report-stream)
       ,@delivery)
     (pushnew ',type *delivery-methods*)))

(defun deliver-mail (addresses plist text report-stream &aux failed filter to-deliver)
  (dolist (method *delivery-methods*)
    (setq to-deliver nil)
    (setq filter (get method 'deliver-filter))
    (dolist (address addresses)
      (when (funcall filter address)
        (push address to-deliver)
        (setq addresses (delq address addresses))))
    (when to-deliver
      (setq failed (nconc failed
                          (funcall (get method 'deliver-driver) to-deliver plist text report-stream)))))
  (if addresses
      (format report-stream "Warning: These addresses appear unreachable:~%~S~%" addresses))
  (setq failed (nconc addresses failed)))

(define-delivery-method append-to-file (address)
                        (memq (address-type address) '(:name :file))
                        (addresses plist text report-stream)
  (declare (ignore plist))
  (let ((failed ()))
    (dolist (address addresses)
      (if (errorp (file-deliver-mail address (mail-file-for-address address)
                                     text report-stream))
          (push address failed)
       (format report-stream "~&OK: ~S (local)" address)))
    failed))

(defun mail-deliver-qfile (qfile &optional (report-stream #'si:null-stream)
                           &aux recipients failed-recipients properties)
  (format report-stream "~&Attempting delivery of ~A:~%" qfile)
  (with-open-file-case (msg qfile)
    (error ()) ; don't have to try anything
    (:no-error
     (with-text-buffer (text msg)
       (setq properties
             (let ((*read-base* 10.)
                   (*package* si:pkg-user-package)
                   (*readtable* si:standard-readtable))
               (read msg)))
       (setq recipients (first properties))
       (get-mail-text msg text)
       (setq failed-recipients (deliver-mail recipients (cdr properties) text report-stream))
       (send msg :delete)
       (when failed-recipients
         (when (= (incf (get properties :failures)) 1)
           (setf (get properties :first-failure-date) (get-universal-time)))
         (write-queue-file
           (send qfile :new-pathname
                 :name (let ((name (send qfile :NAME)))
                         (if (string-equal "FAILED_" name :end2 7)
                             name
                          (string-append "FAILED_" name)))
                 :type "-Q-"
                 :version :newest)
           failed-recipients (cdr properties) text report-stream)
         (format report-stream "~&  Requeueing file.~%"))))))

(defun reception-line (server-stream &optional (protocol "CHAOS-MAIL"))
  (explicit-reception-line (send (send server-stream :foreign-host) :name) protocol))

(defun explicit-reception-line (fromstring protocol)
  (multiple-value-bind (seconds minutes hours date month year dow dst-p) (time:get-time)
    (format () "Received: from ~A by ~A with ~A; ~A ~D-~A-~2,'0D ~2,'0D:~2,'0D:~2,'0D-~A"
            fromstring
            (send si:local-host :name)
            protocol
            (time:day-of-the-week-string dow :short)
            date
            (time:month-string month :short)
            year
            hours
            minutes
            seconds
            (time:timezone-string time:*timezone* dst-p))))

(defun deliver-to-sorted-hosts (delivery-name function addresses plist text report-stream)
  "Returned a list of failed addresses.
FUNCTION should take the arguments (HOST ADDRESSES PLIST TEXT REPORT-STREAM) and
return a list of failed addresses."
  (format report-stream "~&Attempting ~A delivery (~D addresses):"
          delivery-name (length addresses))
  (do ((addrs addresses (cdr addrs)) address host failed (to-deliver nil nil))
      ((null addrs) failed)
    (setq address (car addrs) host (foreign-class address))
    (cond ((null (setq host (si:parse-host host t)))
           (format report-stream "~&Unknown host ~A" (foreign-class address))
           (push address failed))
          (t
           (setq to-deliver (ncons address))
           (dolist (a (cdr addrs))
             (when (eq (si:parse-host (foreign-class a) t) host)
               (push a to-deliver)
               (setq addrs (delq a addrs))))
           (setq failed (nconc failed
                               (funcall function host to-deliver plist text report-stream)))))))

(defun address-existent-p (string)
  "Returns NIL, a string (a directory), or a list of addresses."
  (or (let ((dir (fs:lookup-directory string t)))
        (and dir
             (fs:directory-name dir)))
      (cdr (find-in-mailing-lists string))))

;; Deal with mailing lists.

(DEFUN EXPAND-ADDRESSES (ADDRESS-LIST &AUX RECIPIENTS LIST-ADDRESSES)
  (REMOVE-DUPLICATES
    (DOLIST (ADDRESS ADDRESS-LIST RECIPIENTS)
      (COND
        ((CONSP ADDRESS) ; should only get :FILE or :FOREIGN here
         (PUSH ADDRESS RECIPIENTS))
        ((FS:LOOKUP-DIRECTORY ADDRESS T)
         (PUSH (MAKE-ADDRESS :TYPE :NAME :OPTIONS ADDRESS) RECIPIENTS))
        ((AND (STRINGP ADDRESS)
              (SETQ LIST-ADDRESSES
                    (CDR (find-in-mailing-lists ADDRESS))))
         (SETQ RECIPIENTS (NCONC RECIPIENTS (EXPAND-ADDRESSES LIST-ADDRESSES))))
        ((STRING-SEARCH-CHAR #/; ADDRESS)
         (PUSH (MAKE-ADDRESS :TYPE :FILE :OPTIONS ADDRESS) RECIPIENTS))
        (T (PUSH ADDRESS *UNKNOWN-ADDRESSES*)
           (PUSH *FAILED-ADDRESS* RECIPIENTS))))
    :TEST #'EQUALP))

(DEFUN READ-MAILING-LIST-FILE (&optional output-p
                               &aux version NEW-LIST (count-errors 0) (count-lists 0) (count-names 0))
  (let ((mailing-list-pathname
          (probef (fs:parse-pathname *Mailing-List-FILE* si:local-host))))
    (when (typep mailing-list-pathname 'pathname)
      (setq version (pathname-version mailing-list-pathname))
      (if (eql version *Mailing-List-Version-Number*)
       (format (if output-p *terminal-io* nil) "Mailing list file already updated.")
       (with-lock (*mail-server-lock*)
         (fs:reading-from-file (list mailing-list-pathname)
           (incf count-lists)
           (do* ((l (cdr list) (cdr l))  ; munge it in place
                 (address (car l) (car l)))
                ((null l))
             (incf count-names)
             (unless (consp address)
               (setf (car l)
                     (multiple-value-bind (error-p result) (mailer-parse-address address)
                       (if (not error-p) result
                           (when output-p
                             (format *error-output* "~&Bad address ~A in ~A list: ~A~%"
                                     address (car list) result))
                           (decf count-names)
                           (incf count-errors)
                           *failed-address*)))))
           (PUSH LIST NEW-LIST))
         (setq *Mailing-List-Version-Number* version)
         (setq *mailing-lists* new-list))
       (format (if output-p *terminal-io* nil)
               "~&Finished: ~[No~;one~:;~:*~D~] list~:P, ~[no~;one~:;~:*~D~] name~:P, ~[no~;one~:;~:*~D~] error~:P."
               count-lists count-names count-errors)))))

(defun main-lm-mail-server-host ()
  (dolist (h (si:get-site-option :chaos-mail-server-hosts))
    (if (eq :lispm (send (setq h (si:parse-host h)) :system-type)) (return h))))

(defun lm-mail-server-p (host)
  (setq host (si:parse-host host))
  (dolist (h (si:get-site-option :chaos-mail-server-hosts))
    (if (eq host (si:parse-host h)) (return host))))

(DEFUN MESSAGE-HEADERS-AND-TEXT (TEXT)
  "Returns two values: a string with the headers, and a string with the message.
If no header was found, () is returned as the second value.
The header string will not end with a CR, or will the message string begin
with one (unless there are 2 blank lines at the start of the message text)."
  (LET ((IDX (STRING-SEARCH #.(FORMAT () "~C~C" #\CR #\CR) TEXT)))
    (IF (NULL IDX) TEXT
      (VALUES (SUBSTRING TEXT 0 IDX) (SUBSTRING TEXT (+ 2 IDX))))))

(DEFUN FROM-UNAME-FOR-DELIVERY (TEXT &AUX (UNAME "???") HTEXT EOF-P)
  (WITH-INPUT-FROM-STRING (S (MESSAGE-HEADERS-AND-TEXT TEXT))
    (DO () (EOF-P UNAME)
      (MULTIPLE-VALUE (HTEXT EOF-P) (read-line S))
      (WHEN
        (STRING-EQUAL "From:" (SUBSTRING HTEXT 0 5))
        (LET ((Z-UNAME (ZWEI:PARSE-ADDRESSES
                         (STRING-TRIM '(#\SP #\TAB)
                                      (SUBSTRING HTEXT 5
                                                 (OR (STRING-SEARCH-CHAR #\CR HTEXT)
                                                     (STRING-LENGTH HTEXT)))))))
          (typecase Z-UNAME
            (null (setq uname "unknown"))
            (string ; error
             (setq uname "Unknown"))
            (t
             (let ((z-address (first z-uname)))
               (SETQ UNAME (getf z-address :NAME))
               (LET ((HOST (getf z-address :host)))
                 (when HOST
                   (IF (LISTP HOST)
                       (DOLIST (H HOST)
                         (IF (NON-LOCAL-MAIL-HOST-P H)
                             (SETQ UNAME (STRING-APPEND UNAME #/@ H))))
                     (IF (NON-LOCAL-MAIL-HOST-P HOST)
                         (SETQ UNAME (STRING-APPEND UNAME #/@ HOST))))))))))))))


(DEFUN NON-LOCAL-MAIL-HOST-P (HOST-STRING)
  (NOT (OR (MEM #'STRING-EQUAL HOST-STRING (SEND SI:LOCAL-HOST :HOST-NAMES))
           (STRING-EQUAL HOST-STRING (SEND SI:LOCAL-HOST :NAME-AS-FILE-COMPUTER)))))

(DEFUN MAIL-HOST-NAME (STRING)
  (STRING (OR (SI:PARSE-HOST STRING T ())
              (FS:GET-PATHNAME-HOST STRING T)
              STRING)))

(DEFUN FORCE-DELIVERY (&OPTIONAL (STREAM STANDARD-OUTPUT))
  (LET ((DIR (FS:DIRECTORY-LIST (SEND *QFILE-TEMPLATE* :NEW-NAME :WILD))))
    (DOLIST (ELEM DIR)
      (if (FIRST ELEM)
        (MAIL-DELIVER-QFILE (FIRST ELEM) STREAM))))
  (FS:EXPUNGE-DIRECTORY *QFILE-TEMPLATE*))

(DEFUN INITIALIZE-MAILER ()
  (when *mail-server-p*
    (UNLESS *MAILER-INITIALIZED*
      (fresh-line *terminal-io*)
      (write-line "[Mailer: First initializations]" *terminal-io*)
      (setq *qfile-template*
            (make-pathname :host (fs:get-pathname-host si:local-host)
                           :directory '("MAIL" "QUEUE")
                           :type "-Q-"
                           :version :newest))
      (setq *file-defaults*
            (make-pathname :host si:local-host :directory '("MAIL")
                           :name "MAIL" :type "TEXT" :version :newest))
      (load "LM:MAIL.COM;BOOT" :if-does-not-exist nil :verbose nil
                               :package "MAIL" :set-default-pathname nil))
    (fresh-line *terminal-io*)
    (write-string "[Mailer: " *terminal-io*) (read-mailing-list-file t)
    (write-char #\] *terminal-io*)
    (format t "~%[Mailer: Checking for mail to deliver]~%")
    (force-delivery)
    (setq *mailer-initialized* t)
    (enable-mail-server)))

(defun disable-mail-server (&optional (why "Mailer disabled"))
  (setq *server-enabled* nil)
  (setq *server-disable-reason* why))

(defun enable-mail-server () (setq *server-enabled* t))

;;; I don't think the mailer should be initialized when it is being loaded.
;;; (add-initialization "Initialize Mailer" '(initialize-mailer))

(add-initialization "Reset Mailer State"
                    '(progn
                       (disable-mail-server "Mailer not intialized")
                       (SETQ *MAILER-INITIALIZED* ()))
                    ()
                    'si::before-cold-initialization-list)

(add-initialization "Start Mailer and Servers" '(initialize-mailer) '(:enable-services))
(add-initialization "Stop Mailer Servers" '(disable-mail-server) '(:disable-services))

;;; Here are typical address and the corresponding structures from
;;; (car (zwei:parse-addresses address)).  (getf * :interval) returns an interval
;;; so you can use the ZWEI:INTERVAL functions/accessors
;;;
;;; foo : (:NAME "foo" :HOST NIL
;;;        :INTERVAL (("foo" 0.) ("foo" 3.)))
;;; foo@cap : (:NAME "foo" :HOST ("cap")
;;;            :INTERVAL (("foo@cap" 0.) ("foo@cap" 7.)))
;;; "rpk@ccc"@cap : (:NAME "/"rpk@ccc/"" :HOST ("cap")
;;;                  :INTERVAL (("/"rpk@ccc/"@cap" 0.) ("/"rpk@ccc/"@cap" 13.)))
;;; rpk%mc@cap : (:NAME "rpk" :HOST ("mc" "cap")
;;;               :INTERVAL (("rpk%mc@cap" 0.) ("rpk%mc@cap" 10.)))
;;; mitccc!rpk@eddie : (:NAME "mitccc!rpk" :HOST ("eddie")
;;;                     :INTERVAL (("mitccc!rpk@eddie" 0.) ("mitccc!rpk@eddie" 16.)))
;;; rpk%ccc : (:NAME "rpk" :HOST ("ccc")
;;;            :INTERVAL (("rpk%ccc" 0.) ("rpk%ccc" 7.))))

;;; This forwarding scheme does not deal with multiple gateways or address-rewriting.
(defvar *simple-forwarding-alist* '()
  "An alist of hosts to which to forward, and what hosts are reachable through forwarding")

(defun set-simple-forwarding (host host-strings)
  (setq host (si:parse-host host))
  (assert (reachable-host-p host) (host) "Mail can't be forwarded to ~A" host)
  (let ((entry (lisp:assoc host *simple-forwarding-alist*)))
    (if entry
        (setf (cdr entry) host-strings)
      (push (cons host host-strings) *simple-forwarding-alist*)))
  host)

(defun load-simple-forwarding-data (host file)
  "Sets up the mailer so that the mail destined for hosts listed in FILE are sent to HOST.
FILE is simply names, one to a line."
  (set-simple-forwarding host (read-host-names file)))

(defun read-host-names (file)
  (with-open-file (in file)
    (do (line eofp names)
        (eofp (remove-duplicates names :test #'string-equal))
      (multiple-value-setq (line eofp) (read-line in nil))
      (when line
        (setq line (string-trim '(#\Space #\Tab) line))
        (unless (zerop (string-length line))
          (push line names))))))


(defun find-forwarding-host (hostname)
  (let ((entry (lisp:rassoc hostname *simple-forwarding-alist*
                            :test #'(lambda (name list)
                                      (lisp:member name list :test #'string-equal)))))
    (and entry (car entry))))

(defvar *direct-mail-connected-networks* '())

(defun add-direct-mail-connected-network (network)
  (pushnew network *direct-mail-connected-networks*))

(defun reachable-host-p (host)
  (dolist (network *direct-mail-connected-networks*)
    (when (send host :network-typep network)
      (return t))))

(defun host-name-on-network-p (name network-type)
  "Returns non-NIL if NAME names a host on NETWORK-TYPE."
  (let ((host (si:parse-host name t nil)))
    (when host
      (send host :network-typep network-type))))

(defun foreign-network-address-p (address network)
  (and (eq (address-type address) :foreign)
       (stringp (foreign-class address))
       (host-name-on-network-p (foreign-class address) network)))

;;; Returns two values: errorp and address (or error string).  The address returned
;;; is either a string (to be expanded into a mailing list, because it is actually
;;; local) or an address structure.  Note that :FOREIGN is canonicalised for taking
;;; out the local host, but initially the class slot is either a host or list of hosts.
;;; When the mailer gets more complex, this will get processed for a real class slot.
(defun mailer-parse-address (string)
  ;; Speed up the most common case
  (if (not (string-search-set "%@()[]<>" string))
      (values nil string)
   (condition-case (a) (zwei:parse-addresses string)
     (error (values t (format () "Parse error for ~A" string)))
     (:no-error
      (if (stringp a)
          (values t a) ; Error message
        (massage-parsed-address (car a) string))))))

(defun massage-parsed-address (a original)
  (let ((hs (getf a :host)))
    (if (null hs)
        (values nil (getf a :name))
     (let* ((last-host-string (car (last hs)))
            (last-host (si:parse-host last-host-string t)))
       (cond ((null last-host)
              (let ((fwd-host (find-forwarding-host last-host-string)))
                (if fwd-host
                    (values nil
                            (make-foreign-address :mailer-address original
                                                  :original-address original
                                                  :class (send fwd-host :name)))
                  (values t (format nil "Unknown host ~A" last-host-string)))))
             ((eq last-host si:local-host)
              (setf (getf a :host) (nbutlast hs))
              (massage-parsed-address a original))
             ((reachable-host-p last-host)
              (values nil
                      (make-address :type :foreign
                                    :options (make-foreign-address
                                               :mailer-address
                                               (mailer-address-for-other-host
                                                 (butlast hs) (getf a :name))
                                               :original-address original
                                               :class (send last-host :name)))))
             (t
              (values t (format () "Unreachable host ~A" last-host-string))))))))

(defun mailer-address-for-other-host (hosts name &aux (strings (ncons name)))
  (dolist (h hosts)
    (push "%" strings)
    (push h strings))
  (when (> (length strings) 2)
    (setf (cadr strings) "@"))
  (apply #'string-append (nreverse strings)))

(defun address-as-string (address)
  "Returns a fully-qualified address string (user@host)"
  (case (address-type address)
    (:name (format () "~A@~A" (address-options address) si:local-host))
    (:file (format () "/"~A/"@~A" (address-options address) si:local-host))
    (:foreign (foreign-original-address address))))

;;; Testing functions and the like
(defun _hack-it ()
  (setq *mail-server-p* t
        zwei:*mail-chaos-hosts* (delq si:local-host zwei:*mail-chaos-hosts*))
  (push si:local-host zwei:*mail-chaos-hosts*))

(defun _send-test-message (&rest addresses &aux s)
  (unwind-protect
      (progn
        (setq s (chaos:open-stream si:local-host "MAIL"))
        (dolist (a addresses)
          (write-line a s) (force-output s)
          (format t "~%  Response for ~A: ~A" a (send s :line-in)))
        (terpri s)
        (format s "From: ~A <~A@~A>~%To: Nobody in particular~%Subject: This is a test~2%"
                fs:user-personal-name-first-name-first (fs:uname-on-host fs:user-login-machine)
                fs:user-login-machine)
        (dotimes (i 10)
          (format s "Blah~%"))
        (format s "~%   --- F I N I S ---~%")
        (send s :eof)
        (format t "~%Response for text: ~A" (send s :line-in)))
    (when s (lisp:close s :abort t))))

(defun install-mailer ()
  (format t "~%Making directories...")
  (dolist (dirspec '("MAIL" ("MAIL" "COM") ("MAIL" "QUEUE")))
    (let ((directory (make-pathname :host si:local-host :directory dirspec)))
      (unless (probe-directory directory)
        (format t " ~A" directory)
        (fs:create-directory directory))))
  (format t "~%Looking for mailing list file...")
  (unless (probef *mailing-list-file*)
    (format t " installed ~A."
            (copy-file "SYS: NETWORK; MAILER; STARTER-LIST LISP >"
                       *mailing-list-file*
                       :report-stream nil)))
  (terpri)
  (cond
    ((get-site-option :mail-server)
     (write-line
       "Looks like you're ready to roll.  Save the mailer in a band if you haven't already.")
     (write-line
       (if (get-site-option :server-machine)
           "Since this is a server machine, make sure that services are enabled with the
function SI:ENABLE-SERVICES by some person, or an init file, after the machine boots."
         "This machine will start running the mailer as soon as it boots.")))
    (t
     (write-line "This machine will not be a mail server unless you give it a :MAIL-SERVER option
in LMLOCS, or a MAIL-SERVER user property using the Site Editor.")))
  t)
