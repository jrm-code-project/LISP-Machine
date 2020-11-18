;;; -*- Mode:LISP; Package:SERIAL-IP; Readtable:CL; Base:10 -*-

#|

  Copyright GigaMos Systems, Inc. 1988
   See filename "Copyright.Text" for
  licensing and release information.

This implements serial I/O handling and remote site definition code, part of
the implementation of the "Serial IP" interface for the new network system.
The actual interface is in the file SERIAL.

On the Lambda, remote serial sites are defined in SYS:SITE;SITE, by the site options
:SERIAL-IP-SITES and :DEFAULT-SERIAL-IP-SITE.  The format of these options is:

(:SERIAL-IP-SITES
  `((<tag> <address> <network> <host-or-gateway> <type> <device> <baud-rate>
           (<open-args>) (<connect-args>))
    :
    :
    ))
(:DEFAULT-SERIAL-IP-SITE <tag>)

Each of these options defines the remote site from the viewpoint of the local
system; in other words, this is information to be used by a system when
opening its serial port and connecting to the particular remote site.  This
works with the Lambda site files, since any exceptions can be defined in
LMLOCS to alter a particular host's view of the remote sites.

The site specification elements are as follows:

<TAG> is a site name; it is used as an id for locating and describing sites.

<ADDRESS> can be the name of a defined Internet host, or an Internet address of
an undefined host; this host presumably answers to and otherwise supports this
serial interface.

<NETWORK> specifies the name or IP address of the Internet on which the serial
host is local.  A network name is compared to the local list of networks to
obtain the network number; an Internet address is parsed and used "as is".
<NETWORK> can be the same as the host address, or NIL, in which case the site
does not represent a gateway.  If <NETWORK> is T, then the site does act as a
gateway to the remote Internet.

Note that for Serial-IP to function properly, the local host must have an
Intenet address on the remote network it is talking to, and vice-versa.  If it
does not, the corresponding network number is used, with unpredictable
results.

<TYPE> is a keyword symbol specifying the manner of connection. Standard
connection types and methods are defined in the file SERIAL-PROTOCOL.

<DEVICE> is a description (probably a string) of the serial port device to be
used.  On the Lambdas, this is/should be limited to "SDU-SERIAL-B".

<BAUD-RATE> is a decimal number indicating the baud rate to be used for the
serial port device.

|#

;;;Miscellaneous utilities

;;;Error handling

(defparameter *sites-must-be-remote* nil
  "If non-NIL, it is an error to define a serial site that is
not on a remote Internet")

(defvar *throwing-serial-sites* nil)

(defmacro serial-site-error(fmt &rest args)
  `(progn
     (if *throwing-serial-sites*
         (progn
           (funcall #'warn
                    ,fmt ,@args)
           (throw *throwing-serial-sites* nil))
       (error ,fmt ,@args))))

(defmacro with-serial-site-error(&body body)
  `(let((*throwing-serial-sites* 'serial-site-error))
     (catch *throwing-serial-sites*
       ,@body)))

;;;Address manipulations

(defun network-number-from-name(name)
  (when (typep name '(or string symbol))
    (setq name (string name))
    (loop for spec in (si:get-site-option :network-names)
          as names = (first spec)
          when (member name names :test #'string-equal)
          as alist = (second spec)
          as pair = (assoc :internet alist :test #'eq)
          as num = (ip:parse-internet-address(second pair))
          when num return num)))

(defun find-network-number(name-or-number)
  (or (network-number-from-name name-or-number)
      (prog(address)
           (setq address (ip:parse-internet-address name-or-number))
           (if address (return (ip:ip-network-number-from-address address))))))

(defun find-address-on-network (addresses network &aux address)
  (and (setq network (find-network-number network))
       (setq address
             (find network addresses
                   :key #'(lambda(n) (ip:ip-network-number-from-address
                                       (ip:parse-internet-address n)))
                   :test #'eql))
       (ip:parse-internet-address address)))

(defun find-local-address-on-network(network)
  (or
    (find-address-on-network (send si:local-host :internet-addresses) network)
    (ip:ip-network-number-from-address network)))


;;;SERIAL-SITE structure:

(defstruct (serial-site (:print-function print-serial-site))
  (tag nil)                                     ;id tag
  (remote-address nil)                          ;site's address on remote net
  (gateway-to nil)                              ;site's a gateway to...
  (local-address nil)                           ;site's address on local net
  (is-remote-p nil)                             ;site is remote from local host
  (host-local-address nil)                      ;local host's local address
  (host-remote-address)                         ;local host's remote address
  (type nil)                                    ;open/connect type
  (device nil)                                  ;serial port device name
  (baud-rate 1200.)                             ;open at baud rate
  (open-args nil)                               ;arguments to open method
  (connect-args nil)                            ;arguments to connect method
  (stream nil)                                  ;current open stream
  )

;;;Print functions:

(defun print-serial-site-1(site &optional (stream *standard-output*))
  (let((*print-base* 10.)
       (tag (serial-site-tag site))
       (address (serial-site-remote-address site))
       (network (serial-site-gateway-to site))
       (type (serial-site-type site))
       (baud-rate (serial-site-baud-rate site)))
    (format stream "~a `~a' ~@[[gateway]~* ~]is ~a at ~a baud"
            tag
            (multiple-value-bind(address host)
                (ip:parse-internet-address address)
              (or host (net:unparse-address address :internet)))
            (not(eql address network))
            type baud-rate)))

(defun print-serial-site(site &optional (stream *standard-output*) &rest ignore)
  (si:printing-random-object(site stream :type)
    (print-serial-site-1 site stream))
  nil)

(defun print-serial-site-verbose(site &optional (stream *standard-output*))
  (flet((unp(addr) (and addr (net:unparse-address addr :internet))))
    (format stream "~%Site is ")
    (print-serial-site site stream)
    (format stream "~%Site remote address = ~s"
            (unp (serial-site-remote-address site)))
    (format stream "~%Site is gateway to  = ~s"
            (unp (serial-site-gateway-to site)))
    (format stream "~%Site local address  = ~s"
            (unp (serial-site-local-address site)))
    (format stream "~%Site is ~@[not ~]remote"
            (null (serial-site-is-remote-p site)))
    (format stream "~%Local host's local address   = ~s"
            (unp (serial-site-host-local-address site)))
    (format stream "~%Local host's remote address  = ~s"
            (unp (serial-site-host-remote-address site)))))

;;;Parsing site specifications

(defun parse-address-and-network(address network
                                 &aux site-remote-address site-gateway-to site-local-address
                                 address-list
                                 remote-network site-is-remote-p
                                 host-local-address host-remote-address)
  (declare(values site-remote-address site-gateway-to site-local-address
                  site-is-remote-p
                  host-local-address host-remote-address))
  (flet((barf (fmt &rest fmt-args)
              #+LMI (declare(eh:uninteresting-function))
              (serial-site-error
                "Parsing serial site - Address=~s Network=~s~%   ~a"
                address network
                (apply #'format nil fmt fmt-args))))
    ;;Get the local host and thus local Internet #
    (unless (setq host-local-address (send si:local-host :internet-address))
      (serial-site-error "Cannot parse serial sites: ~a ~a"
                         "No Internet address for local host"
                         si:local-host))
    ;;Get site host's address and host object if available
    (multiple-value-bind(try-address host)
        (ip:parse-internet-address address)
      ;;Address must be valid
      (unless try-address
        (barf "Address is neither a known host, nor an Internet address"))
      ;;Cons up a list of possible site addresses
      (unless (setq address-list
                    (if host (send host :internet-addresses)
                      (ncons try-address)))
        (barf "~s ~a"
              host
              "does not have an Internet address"))
      ;;Can't default network if host has multiple addresses
      (if (and host
               (member network '(t nil))
               (> (length address-list) 1))
          (barf "Host ~s has multiple Internet addresses"
                host))
      ;;Determine site gateway/network
      (unless
        (setq site-gateway-to
              (cond
                ((eq network nil)               ;Default network, not gateway
                 try-address)
                ((eq network t)                 ;Default network, is gateway
                 (find-network-number try-address))
                (t                              ;Else, parse NETWORK spec
                 (or (network-number-from-name network)
                     (ip:parse-internet-address network)))))
        (barf "Network not known by name, or not a valid Internet address"))
      (setq remote-network (find-network-number site-gateway-to))
      ;;Does site have an address on remote network?
      (unless (setq site-remote-address
                    (find-address-on-network address-list remote-network))
        (barf "Specified site/host ~a ~s"
              "does not have an address on network"
              (net:unparse-address remote-network :internet)))
      ;;Is local host on remote network?
      (if (eql (find-network-number host-local-address)
                 (find-network-number site-gateway-to))
          (if *sites-must-be-remote*
              (barf "Address for remote host is on local Internet")
            ;;If OK to define remote site on same Internet,
            ;;for heaven's sake don't make it a gateway!
            (setq site-gateway-to site-remote-address))
        (setq site-is-remote-p t))
      ;;Get site's address on local network, if any
      (setq site-local-address
            (if host
                (find-address-on-network
                  (send host :internet-addresses) host-local-address)))
      ;;Get local host's address on remote network, if any
      (setq host-remote-address
            (find-local-address-on-network site-gateway-to))
      ;;Finally, return values
      (values site-remote-address
              site-gateway-to
              site-local-address
              site-is-remote-p
              host-local-address
              host-remote-address))))

;;;Construct a SERIAL-SITE from raw specification:

(defun parse-serial-site (tag address network type device baud-rate
                          &optional open-args connect-args)
  (multiple-value-bind(remote-address gateway-to local-address
                       is-remote-p
                       host-local-address host-remote-address)
      (parse-address-and-network address network)
    (make-serial-site
      :tag tag
      :remote-address remote-address
      :gateway-to gateway-to
      :local-address local-address
      :is-remote-p is-remote-p
      :host-local-address host-local-address
      :host-remote-address host-remote-address
      :type type :device device :baud-rate baud-rate
      :open-args open-args :connect-args connect-args)))

;;;Site variables

(defun serial-site-device-name(site)
  #+LMI
  (string-right-trim ":" (serial-site-device site))
  #-LMI
  (string (serial-site-device site)))

#+LMI
;;;We get the Serial-IP-Sites from the site files:
(global:define-site-variable *serial-ip-sites*
                             :serial-ip-sites
  "Define a number of sites that support serial port Internet
functioning according to the Lambda protocol.  This option is a list
of site definitions, with elements as follows:

   (<TAG> <ADDRESS> <NETWORK> <TYPE> <DEVICE> <BAUD-RATE>
      (<OPEN-ARGS>)
      (<CONNECT-ARGS>))

See source of *SERIAL-IP-SITES* for further information.")

#-LMI
(defvar *serial-ip-sites* nil "Unparsed serial site specs")

(defvar *serial-sites* nil
  "List of actual SERIAL-SITE structures which correspond to those
defined in *SERIAL-IP-SITES*.")

#+LMI
;;;We also get the default Serial-IP site from the site files:
(global:define-site-variable *default-serial-ip-site*
                             :default-serial-ip-site
  "Specifies the TAG of a site (one of :SERIAL-IP-SITES) to be the default
  enabled site when Serial-IP is initialized.
See source of *DEFAULT-SERIAL-IP-SITE* for further information.")

#-LMI
(defvar *default-serial-ip-site* nil "Default serial site tag")

(defun serial-site-list-sites()
  *serial-sites*)

(defun serial-site-locate(tag-or-site)
  (if (typep tag-or-site 'serial-site)
      tag-or-site
    (car(member tag-or-site *serial-sites*
                :test #'string-equal
                :key #'serial-site-tag))))

(defun serial-site-locate-opened-site()
  (find-if #'serial-site-stream *serial-sites*))

(defun gather-serial-ip-sites()
  (prog1
    (setq *serial-sites*
          (loop for spec in *serial-ip-sites*
                as site = (with-serial-site-error
                            (apply #'parse-serial-site spec))
                when site
                collect site))
    (setq *default-serial-ip-site* (serial-site-locate *default-serial-ip-site*))))

#+LMI
(add-initialization
  "Define serial sites by parsing specs"
  `(gather-serial-ip-sites)
  '(:site-option))

#|
;;;Example/test code:

(defun span(address network)
  (with-serial-site-error
    (format t "~%*** Address=~s Network=~a" address network)
    (multiple-value-bind(site-remote site-network site-local remotep host-local host-remote)
        (parse-address-and-network address network)
      (unless remotep (format t "~%!!!! Site is not on remote Internet!!!!"))
      (format t "~%Site remote address:  ~a"
              (and site-remote  (net:unparse-address site-remote  :internet)))
      (format t "~%Site gateway address: ~a"
              (and site-network (net:unparse-address site-network :internet)))
      (format t "~%Site local address:   ~a"
              (and site-local   (net:unparse-address site-local   :internet)))
      (format t "~%Host local address:   ~a"
              (and host-local   (net:unparse-address host-local   :internet)))
      (format t "~%Host remote address:  ~a"
              (and host-remote  (net:unparse-address host-remote  :internet))))))

(defun serial-test()
  (setq *serial-ip-sites*
        `((:lowell     "opus"      "101.0.0.0" :hardwired "sdu-serial-b" 1200.)
          (:cambridge  "100.0.0.1" "100.0.0.0" :hardwired "sdu-serial-b" 1200.)
          (:test1      "125.0.0.1" "125.0.0.0" :hardwired "sdu-serial-b" 300.)
          (:test2      "125.0.0.1"   t         :hardwired "sdu-serial-b" 300.)
          (:test3      "125.0.0.1" "125.0.0.1" :hardwired "sdu-serial-b" 300.)
          (:test4      "103.0.0.3" "103.0.0.1" :hardwired "sdu-serial-b" 300.)))
  (loop for spec in *serial-ip-sites*
        as tag = (first spec)
        as add = (second spec)
        as net = (third spec)
        do (span add net))
  (gather-serial-ip-sites)
  (pprint *serial-sites*))

|#
