;;; -*- Mode:LISP; Package:SERIAL-IP; Readtable:CL; Base:10 -*-

#|

  Copyright GigaMos Systems, Inc. 1988
   See filename "Copyright.Text" for
  licensing and release information.


This is the implementation of the "Serial IP" interface driver and I/O
process for the new network system. The high-level serial I/O handling
and remote site code is in another file, SERIAL-SITES.  The actual
packet protocol and low-level serial I/O is in the file SERIAL-PROTOCOL.

|#

(export '(*serial-interface*
           *serial-io-process*
           setup-serial
           enable-serial-interface
           disable-serial-interface
           reenable-serial-interface))

(in-package "SERIAL-IP")

(use-package '("SERIAL-PROTO" "NETWORK" "LISP"))

;;;Interface globals

(defvar *serial-interface* nil "The (only, global) Serial network interface")
(defconstant *serial-max-packet-size*
             (floor *serial-proto-max-packet-size* 2))
(defvar *serial-free-list* (make-fifo) "List of serial-packets free for use")
(defvar *serial-send-list* (make-fifo) "List of serial-packets waiting to be received")
(defvar *serial-recv-list* (make-fifo) "List of serial-packets waiting to be sent")

;;;Process globals

(defvar *serial-io-process* nil "The (one, only) Serial I/O Handler process")
(defvar *serial-process-name* "Serial-IP I/O Process")
(defvar *serial-process-num* 0)                 ;How many serial processes run

;;;Define interface

(defstream serial-interface
           (network-interface)
           "SERIAL-INTERFACE-"
  (site nil)                                    ;Current serial-site
  (message                                      ;Latest message
    (make-string 100. :fill-pointer 0.))
  (process nil)                                 ;Current I/O process
  (startup-time nil)                            ;Process startup time
  (connected-p nil)                             ;Are we connected?
  )


(defop (serial-interface :peek-special-fields) (ni)
  (list (tv:scroll-parse-item
          `(:function serial-ip:describe-current-site (,ni) NIL ("~A")))
        (tv:scroll-parse-item
          `(:function serial-ip:serial-process-message (,ni) NIL ("~A")))
        (tv:scroll-parse-item
          `(:function serial-ip:copy-serial-interface-message (,ni) NIL ("~A"))))
  )

;;;Describe selected site

(defun describe-current-site(&optional (interface *serial-interface*)
                             stream
                             (max-length 100.)
                             &aux site)
  (unless stream
    (setq stream (make-string max-length :fill-pointer 0.)))
  (format stream "Current site: ")
  (cond
    ((null (setq site (serial-interface-site interface)))
     (format stream "not selected"))
    ((or (not (arrayp stream))
         (not(array-has-fill-pointer-p stream)))
     (format stream "~s" site))
    (t
      (prog(len)
           (setq len (fill-pointer stream))
           (format stream "~s" site)
           (when (> (fill-pointer stream) max-length)
             (setf (fill-pointer stream) len)
             (print-serial-site-1 site stream))
           (when (> (fill-pointer stream) max-length)
             (setf (fill-pointer stream) len)
             (format stream "~a" (serial-site-tag site))))))
  stream)

;;;Warning/status messages

(defvar *serial-message-max-length* 120.)

(defun blank-serial-message(&optional (interface *serial-interface*))
  (if (stringp (serial-interface-message interface))
    (setf (fill-pointer (serial-interface-message interface)) 0.)
    (setf (serial-interface-message interface)
          (make-string *serial-message-max-length* :fill-pointer 0.))))

(defun serial-message(interface prefix format-string &rest format-args)
  (blank-serial-message interface)
  (let((str (serial-interface-message interface))
       (max *serial-message-max-length*))
    (format str "~1(~a:~) ~a"
            prefix
            (apply #'format nil format-string format-args))
    (and max (> (string-length str) max) (setf (fill-pointer str) max))
    str))

(defun serial-warn(interface format-string &rest format-args)
  (apply #'serial-message interface "Warning" format-string format-args))

(defun serial-status(interface format-string &rest format-args)
  (apply #'serial-message interface "Status" format-string format-args))

(defun copy-serial-interface-message(interface)
  (subseq (or (serial-interface-message interface) "") 0 nil))


;;Address/gateway handling for protocols

(defun null-internet-alist()
  (list (ncons :internet)))

(defun serial-add-address-info(interface site)
  ;;Interface's address is just a label:
  (setf (serial-interface-address interface)
        (serial-site-tag site))
  (let*((remote-address (serial-site-remote-address site))
        (site-local-address (serial-site-local-address site))
        (host-remote-address (serial-site-host-remote-address site))
        (gateway-to (serial-site-gateway-to site)))
    (multiple-value-bind (remote-network mask)
        (ip:ip-network-number-and-mask remote-address)
      (cond
        ((null remote-address)
         (serial-warn interface "No remote Internet address for site"))
        (t
         ;;Associate interface with remote Internet network and address:
         (setf (serial-interface-address-alist interface)
               `((:internet ,host-remote-address)))
         (setf (serial-interface-network-alist interface)
               `((:internet ,remote-network)))
         ;;Associate remote site with interface
         (add-address-info site-local-address :internet interface site)
         (add-address-info remote-address :internet interface site)
         ;;Tell Network Software that remote network directly accessable
         (push (cons remote-network mask) ip:*default-network-numbers*)
         ;;Create gateway:
         (ip:add-gateway gateway-to remote-address interface))))))

(defun serial-remove-address-info(interface &aux site)
  (when (and (setq site (serial-interface-site interface))
             (eq *serial-io-process* (serial-interface-process interface)))
    (setq ip:*default-network-numbers*
          (delete (ip:ip-network-number-from-address (serial-site-remote-address site))
                  ip:*default-network-numbers*
                  :key #'car))
    (ip:remove-gateway (serial-site-gateway-to site))))

;  (setf (serial-interface-address interface) nil)
;  (setf (serial-interface-address-alist interface) (null-internet-alist))
;  (setf (serial-interface-network-alist interface) (null-internet-alist)))
;  (setf (serial-interface-address-translations interface) nil))

;;Disable interface

(defun serial-disable(interface &aux packets)
  (serial-status interface "Shutting down")
  (serial-remove-address-info interface)
  (kill-serial-process interface)
  (setq packets (append (fifo-as-list *serial-recv-list*)
                        (fifo-as-list *serial-send-list*)))
  (setq *serial-recv-list* (make-fifo))
  (setq *serial-send-list* (make-fifo))
  (dolist(spkt packets)
    (push-fifo spkt *serial-free-list*)))

;;Enable/start interface

(defun serial-start(interface site)
  "Assign SITE to INTERFACE with address info"
  (kill-serial-process *serial-interface*)
  (setf (serial-interface-site interface) site)
  (serial-add-address-info interface site)
  (setf (serial-interface-process interface)
        (setq *serial-io-process*
              (process-run-function
                (format nil "~a ~d"
                        *serial-process-name*
                        (incf *serial-process-num*))
                #'serial-process
                interface site))))

(defun serial-restart(interface &aux site)
  "Restart INTERFACE for current site"
  (setq site (serial-interface-site interface))
  (if site
      (serial-start interface site)
    (serial-warn interface "No site set, cannot restart.")))

(defun serial-enable(interface &optional site)
  ;;Network software calls this with no SITE arg
  "Call this manually to start serial interface on SITE (can be a tag id)"
  (serial-status interface "Enabling Serial-Interface")
  (flet((barf(fmt &rest args)
             (apply #'serial-warn interface fmt args)))
  (cond
    ((null site)                        ;restart on current site
     (serial-restart interface))
    ((serial-interface-enabled interface)       ;already enabled, punt
     (barf
       "Serial-Interface already enabled ~:[but not on any site?~;for site ~a~]"
       (serial-site-tag
         (serial-interface-site interface))))
    (t
     (setq site (serial-site-locate site))
     ;;
     ;;Enable on new site: locate first, error check
     ;;
     (cond
       ((null (serial-site-locate site))
        (if (null (serial-site-list-sites))
            (barf "No Serial-Interface sites are defined")
          (barf "Cannot locate Serial-Interface site ~s" site)))
       ((eq (setq site (serial-site-locate site))       ;???
            (serial-interface-site interface))  ;restart on current site
        (serial-restart interface))
       (t (serial-start interface site)))))))

;;Reset interface

(defun serial-reset (interface)
  (serial-disable interface)
  (serial-enable interface))


;;;Packet handling routines (internal)

(defstruct (serial-packet
             (:conc-name "SPKT-"))
  (pkt-16                               ;16-bit packet array
    (make-array
      *serial-max-packet-size*
      :element-type '(unsigned-byte 16)
      :fill-pointer 0))
  (pkt-8                                ;8-bit packet array
    (make-array
      (* 2 *serial-max-packet-size*)
      :element-type '(unsigned-byte 8)
      :fill-pointer 0
      :displaced-to pkt-16))
  (dest nil)                            ;packet destination
  )

(defun allocate-serial-packet()
  (or (pop-fifo *serial-free-list*)
      (make-serial-packet)))

(defun stuff-send-packet(int-pkt spkt &aux pkt-16 len-16)
  (setq pkt-16 (spkt-pkt-16 spkt))
  (setq len-16 (fill-pointer int-pkt))
  (copy-array-portion int-pkt 0 len-16
                      pkt-16  0 len-16)
  (setf (fill-pointer pkt-16) len-16)
  (setf (fill-pointer (spkt-pkt-8  spkt)) (* 2 len-16)))

(defun stuff-recv-pkt(spkt int-pkt &aux pkt-16 len-16)


  (setq pkt-16 (spkt-pkt-16 spkt))
  (setq len-16 (fill-pointer pkt-16))
  (copy-array-portion pkt-16  0 len-16
                      int-pkt 0 len-16)
  (setf (fill-pointer int-pkt) len-16))

(defun serial-send-int-pkt (int-pkt source dest type n-16-bit-words &aux spkt)
  (declare(ignore source type))
  (setf (fill-pointer int-pkt) n-16-bit-words)
  (when (setq spkt (allocate-serial-packet))
    (stuff-send-packet int-pkt spkt)
    (free-packet int-pkt)
    (setf (spkt-dest spkt) dest)        ;log dest - will need for multi-interface
    (push-fifo spkt *serial-send-list*))
  t)

;;;Listing packets (debug code)

(defun print-pkts()
  (loop for spkt in (fifo-as-list *serial-free-list*)
        with count = 0
        as pkt = (spkt-pkt-8 spkt)
        do
        (incf count)
        (format t "~%#~d: " count)
        (dotimes(i (fill-pointer pkt))
          (format t "~c" (aref pkt i)))))

;;;Packet routines for network receiver

(defun serial-packet-ready ()
  (not (fifo-empty-p *serial-recv-list*)))

(defvar *last-packet-returned* nil)

(defun serial-get-next-packet (&aux int-pkt)
  (declare (values packet type source destination broadcast-p))
  (let ((spkt (pop-fifo *serial-recv-list*)))
    (cond
      ((null spkt) nil)
      ((setq int-pkt (allocate-packet-for-receive *serial-interface*))
       (stuff-recv-pkt spkt int-pkt)            ;fill in int-pkt
       (values-list
         (setq *last-packet-returned*
             (list int-pkt                      ;the packet
                     ip-ethernet-type           ;packet is always ethernet type
                     (serial-interface-site     ;sender
                         *serial-interface*)
                     (serial-interface-address  ;receiver
                       *serial-interface*)
                     nil                        ;broadcast-p
                     ))))
      (t                                ;no free int-pkt
       (push-fifo-front spkt *serial-recv-list*)
       nil))))

;;;Main setup routines

(defun setup-serial(&optional site)
  (let((id "SERIAL")
       (site (serial-site-locate (or site *default-serial-ip-site*))))
    ;;
    ;;Close the previous interface down, if known:
    ;;
    (when *serial-interface*
      (send *serial-interface* :close))
    ;;
    ;;Instantiate Serial-Interface:
    ;;
    (setq *serial-interface*
          (make-serial-interface
            :interface :serial
            :keyword :serial
            :tag id
            :gauge-name id
            :point-to-point t
            :protocol-alist nil
            :network-alist nil
            :address-alist  nil
            :minimum-data-length 0
            :maximum-data-length 128.           ;41 <= x <= *serial-proto-max-packet-size*
            :sent-header-length 0
            :rcvd-header-length 0
            :sent-trailer-length 0
            :rcvd-trailer-length 0
            :disable-function 'serial-disable
            :enable-function 'serial-enable
            :reset-function 'serial-reset
            :packet-ready-function 'serial-packet-ready
            :get-next-packet-function 'serial-get-next-packet
            :send-function 'serial-send-int-pkt
            ))
    ;;
    ;;Open the interface
    ;;
    (funcall *serial-interface* :open)
    ;;
    ;;If there's a default site, enable the interface
    ;;
    (if (serial-site-p site)
        (funcall *serial-interface* :enable site)
      (serial-warn *serial-interface* "Cannot enable, no default site"))
    *serial-interface*))

(add-initialization "Setup Serial-IP" '(setup-serial) :network-driver)

(defun enable-serial-interface(&optional site)
  (funcall *serial-interface* :enable site))

(defun disable-serial-interface()
  (funcall *serial-interface* :disable))

(defun reenable-serial-interface(&optional site)
  (disable-serial-interface)
  (enable-serial-interface site))


;;;Serial-IP process

(defvar *serial-io-command* nil "Keyword indicating thing to process")

;;Debugging variables

(defvar *debugging* nil)
(defvar *spacket*)
(defvar *rpacket*)
(defvar *sdu* nil)
(defvar *sp* nil)

;;;Kill serial process:

(defun kill-serial-process(interface &aux process)
  (when (setq process (serial-interface-process interface))
    ;;Only forget global process if it belongs to this interface
    (if (eq *serial-io-process* process)
        (setq *serial-io-process* nil))
    ;;Forget local process
    (setf (serial-interface-process interface) nil)
    ;;Kill it, smack it dead
    (send process :kill)))

;;;Process-wait function for network receiver:

(defun serial-ready-p(fifo stream)
  (setq *serial-io-command*
        (cond
          ;;Packet?
          ((not(fifo-empty-p fifo)) :send)
          ;;Input on stream?
          ((serial-port-listen stream) :receive))))

;;;Start-up function:

(defun serial-process(interface site)
  (if *debugging*
      (serial-warn interface
                      "Debugging mode active (*debugging*=~s); to run, rebind and retry."
                      *debugging*)
    (with-serial-port-allocated (serial-site-device-name site)
      (serial-status interface
                        "Serial process has started")
      (unwind-protect
          (serial-process-top-level interface site)
        (serial-status interface
                        "Serial process has shut down")))))

;;;Top-level function (do this forever):

(defun serial-process-top-level(interface site &aux connected-p)
  (setf (serial-interface-startup-time interface)
        (time:get-universal-time))
  (let(send-spkt
       recv-spkt pkt-8 pkt-16
       status
       (type (serial-site-type site))
       (device (serial-site-device site))
       (baud-rate (serial-site-baud-rate site))
       (open-args (serial-site-open-args site))
       (connect-args (serial-site-connect-args site)))
    ;;
    ;;This is where everything comes together:
    ;;
    ;; Interface is up and running this process,
    ;; with an associated site and its data, which are
    ;; used to open serial port and run protocol.
    ;;
    (with-open-stream(slip (make-serial-proto-stream :type type))
      (setq *sp* slip)
      (apply slip :open device :baud-rate baud-rate open-args)
      (setq *sdu* (sproto:sp-stream *sp*))
      (loop
        (process-wait "Serial-IP Command"
                      #'serial-ready-p *serial-send-list* slip)
        (case *serial-io-command*
          ;;Send a packet?
          (:send
           (setq send-spkt (pop-fifo *serial-send-list*))
           (setq *spacket* send-spkt)
           (unless (equal (spkt-dest send-spkt) site)
             (serial-warn interface
                             "Serial-IP error: packet for ~s, not ~s"
                             (spkt-dest send-spkt) site)
             (funcall interface :disable)
             (return-from serial-process-top-level))
           (unless connected-p
             (setf (serial-interface-connected-p interface)
                   (setq connected-p
                         (apply slip :connect connect-args))))
           ;;Send packet
           (serial-proto-send slip (spkt-pkt-8 send-spkt))
           (push-fifo send-spkt *serial-free-list*))
          ;;Receive a packet?
          (:receive
           (unless recv-spkt
             (setq recv-spkt (allocate-serial-packet))
             (setq *rpacket* recv-spkt)
             (setq pkt-8 (spkt-pkt-8 recv-spkt))
             (setq pkt-16 (spkt-pkt-16 recv-spkt))
             (setf (fill-pointer pkt-8) 0.)
             (setf (fill-pointer pkt-16) 0.)
             ;;Associate pkt with proto-stream
             (send slip :new-packet pkt-8))
           (setq status (serial-proto-receive slip))
           (when status
             ;;Not doing anything about full packets!!??
             (unwind-protect
                 (progn
                   (setf (fill-pointer pkt-16)
                         (ceiling (fill-pointer pkt-8) 2.))
                   (push-fifo recv-spkt *serial-recv-list*))
               (setq recv-spkt nil)))))))))

(defun serial-process-message(&optional (interface *serial-interface*)
                              &aux process (no-state "<unknown>"))
    (cond
      ((null interface) "No interface.")
      ((null (setq process (serial-interface-process interface)))
       (if *serial-io-process*
           (format nil "No associated process~@[ but global process is in state ~*~a~]."
                   *serial-io-process*
                   (or (si:process-wait-whostate *serial-io-process*) no-state))
         (format nil "No associated process~@[, debugging mode active~]."
                         *debugging*)))
      ((not (typep process 'si:process))
       "Process bashed.")
      ((null(send process :active-p))
       (format nil "Process in state ~a - not active."
               (or (si:process-wait-whostate process) no-state)))
      (t
       (unless (eq process *serial-io-process*)
         (serial-warn interface "This is not the global process!"))
       (format nil "~a in state ~a~@[, last started ~a~],~@[ not~] connected."
                 (send process :name)
                 (or (si:process-wait-whostate process) no-state)
                 (let((time (serial-interface-startup-time interface)))
                    (and time
                         (time:print-universal-time time nil)))
                 (null (serial-interface-connected-p interface))))))
