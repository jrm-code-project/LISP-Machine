;;; -*- Mode:LISP; Package:UDP; Readtable:CL; Base:10 -*-

#|

  Copyright LISP Machine, Inc. 1987
   See filename "Copyright.Text" for
  licensing and release information.

|#

(defconstant *udp-namespace-server-port* 133.)

(tcpa:define-network-service *udp-namespace-service* :namespace :udp "LMI Namespace"
  :listen-port *udp-namespace-server-port*
  :toplevel-function 'udp-namespace-server-function
  :auto-enable? nil)

(defun udp-namespace-server-function (stream)
  (multiple-value-bind (packet length)
      (send stream :read-packet (make-string 128))
    (reply-to-namespace-request (find-port-identification (namespace-string packet length)
                                                          #'identity
                                                          #'(lambda () 0.))
                                stream)))

(defun namespace-string (buffer length)
  (let ((string (make-string length)))
    (do ((buffer-pointer 0                   (1+ buffer-pointer))
         (string-pointer 0                   (1+ string-pointer))
         (count length                       (1- count)))
        ((zerop count) string)
      (setf (char string string-pointer) (char-upcase (int-char (elt buffer buffer-pointer)))))))

(defun reply-to-namespace-request (port-identification stream)
  (let ((reply-buffer (make-array 2 :element-type '(unsigned-byte 8.))))
    (setf (elt reply-buffer 0) (logand #X00FF port-identification))
    (setf (elt reply-buffer 1) (ash (logand #xFF00 port-identification) -8.))
    (send stream :write-packet reply-buffer)))

(defparameter *udp-server-alist*
  `(
    ;; As per RFC 990
    ("RJE"          5.)
    ("ECHO"         7.)
    ("DISCARD"      9.)
    ("USERS"       11.)
    ("DAYTIME"     13.)
    ("NETSTAT"     15.)
    ("QUOTE"       17.)
    ("CHARGEN"     19.)
    ("FTP-DATA"    20.)
    ("FTP"         21.)
    ("TELNET"      23.)
    ("SMTP"        25.)
    ("NSW-FE"      27.)
    ("MSG-ICP"     29.)
    ("MSG-AUTH"    31.)
    ("DSP"         33.)
    ("TIME"        37.)
    ("RLP"         39.)
    ("GRAPHICS"    41.)
    ("NAMESERVER"  42.)
    ("NICNAME"     43.)
    ("MPM-FLAGS"   44.)
    ("MPM"         45.)
    ("MPM-SND"     46.)
    ("NI-FTP"      47.)
    ("LOGIN"       49.)
    ("LA-MAINT"    51.)
    ("DOMAIN"      53.)
    ("ISI-GL"      55.)
    ("NI-MAIL"     61.)
    ("VIA-FTP"     63.)
    ("TACACS-DS"   65.)
    ("BOOTPS"      67.)
    ("BOOTPC"      68.)
    ("TFTP"        69.)
    ("NETRJS-1"    71.)
    ("NETRJS-2"    72.)
    ("NETRJS-3"    73.)
    ("NETRJS-4"    74.)
    ("FINGER"      79.)
    ("HOSTS2-NS"   81.)
    ("MIT-ML-DEV"  83.)
    ;("MIT-ML-DEV"  85.)
    ("SU-MIT-TG"   89.)
    ("MIT-DOV"     91.)
    ("DCP"         93.)
    ("SUPDUP"      95.)
    ("SWIFT-RVF"   97.)
    ("TACNEWS"     98.)
    ("METAGRAM"    99.)
    ("HOSTNAME"   101.)
    ("ISO-TSAP"   102.)
    ("X400"       103.)
    ("X400-SND"   104.)
    ("CSNET-NS"   105.)
    ("RTELNET"    107.)
    ("POP-2"      109.)
    ("SUNRPC"     111.)
    ("AUTH"       113.)
    ("SFTP"       115.)
    ("UUCP-PATH"  117.)
    ("NNTP"       119.)
    ("ERPC"       121.)
    ("NTP"        123.)
    ("LOCUS-MAP"  125.)
    ("LOCUS-CON"  127.)
    ("PWDGEN"     129.)
    ("CISCO-FNA"  130.)
    ("CISCO-TNA"  131.)
    ("CISCO-SYS"  132.)
    ("SUR-MEAS"   243.)
    ("LINK"       245.)

    ;;; LMI indirection
    ("CONNAME"    133.)
    ))

(defun find-port-identification (string if-found if-not-found)
  (let ((result (assoc string *udp-server-alist* :test #'string-equal)))
    (if result
        (funcall if-found (second result))
        (funcall if-not-found))))

(defun ask-for-port-from-namespace-server (string host if-found if-unknown if-timeout if-bad-packet if-no-udp)
  (check-type string string)
  (let ((reply-packet
          (using-udp-socket host *udp-namespace-server-port* "Namespace Request"
            #'(lambda (stream ip-header)
                (send stream :receive)
                (labels ((wait-for-reply (count)
                           (send stream :write-packet string ip-header)
                           (cond ((zerop count) :timeout)
                                 ((process-wait-with-timeout "Namespace Reply" 60.
                                                             #'(lambda () (send stream :listen)))
                                  (send stream :read-packet))
                                 (t (wait-for-reply (1- count))))))
                  (wait-for-reply 5.)))
            #'(lambda ()
                :udp-down))))
    (cond ((eq reply-packet :timeout) (funcall if-timeout))
          ((eq reply-packet :udp-down) (funcall if-no-udp))
          ((not (eq (first reply-packet) :data))
           (funcall if-bad-packet (first reply-packet)))
          (t
           (let ((port-number
                   (+ (elt (third reply-packet) 0)
                      (ash (elt (third reply-packet) 1)
                           8.))))
             (if (zerop port-number)
                 (funcall if-unknown)
                 (funcall if-found port-number)))))))

(defun get-port-from-namespace-server (string host)
  (labels ((use-local-list (message)
             #'(lambda (&rest cruft)
                 (apply #'format t message host string cruft)
                 (find-port-identification (string-upcase string)
                   #'identity
                   #'(lambda ()
                       (cerror "Type in your guess."
                               "Could not find any information about contact ~A." string)
                       (let ((your-guess (si::prompt-and-read :read "What is your guess? ")))
                         (check-type your-guess (unsigned-byte 16))
                         your-guess))))))
  (ask-for-port-from-namespace-server string host
    #'identity
    (use-local-list "~%Namespace server at ~A ignorant of contact ~A, using local list.")
    (use-local-list "~%No response for namespace request at ~A, using local list.")
    (use-local-list "~%During namespace request of ~a for ~a, received a ~a, using local list.")
    (use-local-list "~%Could not open UDP stream, using local list."))))
