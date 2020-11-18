;;; -*- Mode:LISP; Package:CHAOS; Readtable:CL; Base:10 -*-

#|

  Copyright LISP Machine, Inc. 1985, 1987
   See filename "Copyright.Text" for
  licensing and release information.


This is a USER as far as TCP is concerned and SERVER viewed by CHAOS.

The chaosnet TCP server, as documented on page 630 of the Orangeual.
The following comment was taken from the ITS PDP-10 implementation at MIT:

chaosnet to arpanet server, rfc has hostname and optional (octal) socket number

data is just forwarded from chaosnet connection to arpanet connection and vice versa,
except:
 data opcode 201 from the chaosnet means do an INS on the arpanet connection
  the rest of this packet is treated normally
 data opcode 210 means establish auxiliary connection (for gateway FTP), the
  the data portion of this packet contains 8 bytes of the (gensym'ed presumably)
  contact name for the chaos end of the auxiliary connection, the server will do
  a listen on that name and 2 words of the arpanet socket number to connect to.
  when a rfc is received for that contact name, a half-duplex arpanet connection
  is established and data then forwarded from/to it to/from the chaosnet connection
  in the same manner as the main connection.
 For TCP it is much worse, it starts listening on TCP and chaos and waits for a
 211 packet before checking that TCP is open and opening chaos.  This is needed
 with the screwy way TCP FTP works.
 Using 212 instead of 210 gensyms the local port and sends it back as
 a 300 packet on the main data connection.
 Made it send local internet host number in 300 packet, as four bytes
 following the local port.  (Users must check packet length since other
 servers might not have it) -GZ 10/2/84

 hacked a little in preparation for TCP.  -dcp  12/29/82
 logging feature installed -cstacy 11/7/84


------

Notes on the Lambda's TCP server implementation
The Lambda's TCP server is completely compatible with the documentation in the Orangeual
-- that is, it recognizes opcodes 201 and 210 (although 201 doesn't really do anything,
as explained in the Orangeual).

Opcode 212 is also supported -- this works like 210 except that a packet with opcode 300
containing the port number and local internet host number is sent by the server
over the Chaos connection.

Opcode 211 isn't supported yet.

Other opcodes may be added to this in the future.
|#

(add-initialization "TCP"
                    '(process-run-function "TCP SERVER" 'tcp-server-function)
                    nil
                    'server-alist)

(defvar *tcp-server-processes* nil "kept for use by kill-tcp-server-processes")

(defun kill-tcp-server-processes ()
  "Useful when things get wedged during debugging"
  (dolist (x *tcp-server-processes*)
    (when (member (si:process-name x)
                  '( "TCP server data-out" "TCP server data-in" "TCP aux server"
                    "TCP server aux data-in"
                    "TCP server aux data-out")
                  :test #'string-equal)
      (format t "~&Got one: ~A~%" x)
      (send x :kill)))
  (setq *tcp-server-processes* nil))

;;; to do -- trap errors
(defun tcp-server-function ()
  (condition-case ()
      (let (connection rfc-pkt-data host-name port-number tcp-stream p next-pkt chaos-stream
            parser-result)
        (setq connection (chaos:listen "TCP"))
        (if (unwanted-connection-rejected-p connection)
            (return-from tcp-server-function nil))
        (setq next-pkt nil)
        (unwind-protect
            (multiple-value (host-name port-number)
              (parse-tcp-rfc-packet
                (setq rfc-pkt-data
                      (chaos:pkt-string
                        (setq next-pkt (chaos:get-next-pkt connection))))))
          (and next-pkt (chaos:return-pkt next-pkt)))
        (cond ((not host-name)
               (chaos:reject connection (format nil "Can't parse RFC" host-name)))
              ((errorp (setq parser-result (condition-case (e)
                                               (tcp:get-internet-address host-name)
                                             (error e))))
               (chaos:reject connection (send parser-result :report-string)))
              ((not parser-result)
               (chaos:reject connection
                     (format nil "Internet address unknown for host ~A" host-name)))
              (t
               (if (not port-number) (setq port-number 1))
               (setq tcp-stream (tcp-connect host-name :internet port-number t nil nil nil))
               (cond ((errorp tcp-stream)
                      (chaos:reject connection
                                    (format nil "TCP Error: ~A"
                                            (send tcp-stream :report-string))))
                     (t
                      (chaos:accept connection)
                      (send tv:who-line-file-state-sheet :add-server connection "TCP")
                      (setq chaos-stream (make-stream connection))
                      (setq p (process-run-function "TCP server data-out"
                                                    #'tcp-server-data-out-function
                                                    chaos-stream
                                                    tcp-stream))
                      (pushnew p *tcp-server-processes*)
                      (pushnew (process-run-function "TCP server data-in"
                                                     #'tcp-server-data-in-function
                                                     host-name
                                                     connection
                                                     tcp-stream
                                                     p)
                               *tcp-server-processes*))))))
    (sys:network-error)))


(defun tcp-server-data-in-function (host-name connection tcp-stream rprocess &aux next-pkt opcode
                                    auxprocess)
  ;;; listen for packets from Chaos connection, relay them to the TCP host
  (unwind-protect
      (do-forever
        (process-wait "Net Input" #'(lambda (conn) (or (read-pkts conn)
                                                       (neq (state conn) 'open-state)))
                      connection)
        (and (neq (state connection) 'open-state) (return nil))
        (setq next-pkt (chaos:get-next-pkt connection))
        (setq opcode (chaos:pkt-opcode next-pkt))
        (cond ((or (= opcode chaos:eof-op) (= opcode chaos:cls-op)) (return nil))
              ((not (and (<= opcode #o277) (>= opcode #o200)))
               (ferror nil "Unhandled pkt opcode ~D" opcode))
              ((or (= opcode #o210) (= opcode #o212))
               (multiple-value-bind (rfc-name port-number)
                   (parse-tcp-aux-packet (chaos:pkt-string next-pkt))
                 (cond ((or (not rfc-name) (not port-number))
                        (ferror nil "Can't parse TCP auxiliary data connection request packet."))
                       (t
                        (setq auxprocess
                              (process-run-function "TCP aux server"
                                    #'tcp-aux-server-function
                                    rfc-name port-number host-name (= opcode #o212)))))))
              (t
               (send tcp-stream :string-out (chaos:pkt-string next-pkt))
               (send tcp-stream :force-output)))
        (without-interrupts (chaos:return-pkt next-pkt) (setq next-pkt nil)))
    (and next-pkt (chaos:return-pkt next-pkt))
    (send tcp-stream :force-output)
    (if auxprocess (send auxprocess :kill))
    (send rprocess :kill)
    (close-conn connection :abort)
    (send tcp-stream :close)))


(defun tcp-server-data-out-function (s tcp-stream)
  ;;; Copy input from TCP stream to Chaos stream
  (condition-case ()
      (do-forever
        (multiple-value-bind (array start end)
            (send tcp-stream :next-input-buffer)
          (unless array (return nil))
          (send s :string-out array start end)
          (send s :force-output)
          (send tcp-stream :discard-input-buffer array)))
    ((sys:network-error) nil)))


(defun parse-tcp-rfc-packet (pkt-string)
  (let (name socket-number idx start)
    (setq start (string-search-char #\Space pkt-string))
    (if start (setq start (string-search-not-char #\Space pkt-string start)))
    (if start
        (setq name (substring pkt-string start
                              (setq idx (string-search-char #\Space pkt-string start)))))
    (if (and name (zerop (string-length name))) (setq name nil))
    (if (and name idx)
        (setq socket-number (parse-integer pkt-string
                                           :start (1+ idx)
                                           :radix 8.
                                           :junk-allowed t)))
    (values name socket-number)))


(defun parse-tcp-aux-packet (pkt-string &aux name port-number)
  (cond ((>= (string-length pkt-string) 12.)
         (setq name (substring pkt-string 0 8.))
         (setq port-number (+ (* #x1000000 (aref pkt-string 8.))
                              (* #x10000 (aref pkt-string 9.))
                              (* #x100 (aref pkt-string 10.))
                              (aref pkt-string 11.)))))
  (values name port-number))


(defun tcp-aux-server-function (rfc-name port-number host-name send-local-port-pkt-p)
  (condition-case ()
      (let (connection tcp-stream p next-pkt chaos-stream)
        (setq connection (chaos:listen rfc-name))
        (unwind-protect
            (setq next-pkt (chaos:get-next-pkt connection))     ; get rfc
          (and next-pkt (chaos:return-pkt next-pkt)))
        (setq tcp-stream (tcp-connect host-name :internet port-number t nil nil nil))
        (cond ((errorp tcp-stream)
               (chaos:reject connection (format nil "TCP Error: ~A" (type-of tcp-stream))))
              (t
               (chaos:accept connection)
               (send tv:who-line-file-state-sheet :add-server connection "TCP")
               (setq chaos-stream (make-stream connection))
               (if send-local-port-pkt-p (send-tcp-local-port-pkt connection tcp-stream))
               (setq p (process-run-function "TCP server aux data-out"
                                             #'tcp-server-data-out-function
                                             chaos-stream
                                             tcp-stream))
               (pushnew p *tcp-server-processes*)
               (pushnew (process-run-function "TCP server aux data-in"
                                              #'tcp-server-data-in-function
                                              host-name
                                              connection
                                              chaos-stream
                                              tcp-stream
                                              p)
                        *tcp-server-processes*))))
    (sys:network-error)))

(defun send-tcp-local-port-pkt (connection tcp-stream)
  (condition-case ()
      (let ((pkt (chaos:get-pkt))
            (my-port (send tcp-stream :local-port))
            (my-address (send tcp-stream :local-address)))
        (setf (aref pkt chaos:first-data-word-in-pkt) my-port)
        (setf (aref pkt (1+ chaos:first-data-word-in-pkt)) (ldb (byte 16 16) my-address))
        (setf (aref pkt (+ 2 chaos:first-data-word-in-pkt)) (ldb (byte 16 0) my-address))
        (setf (chaos:pkt-nbytes pkt) 6)
        (setf (chaos:pkt-opcode pkt) #o300)
        (chaos:send-pkt connection pkt))
    (error nil)))


(defun tcp-connect (host net socket-number no-error characters ascii-translation timeout)
  (declare (ignore net))
  (declare (ignore characters))
  (declare (ignore timeout))
  (when ascii-translation
    (error "Ascii translation not yet supported."))
  (condition-case-if no-error (sig)
      (global:open (format nil "TCP-HOST:~A.~A" host socket-number))
    (error sig)))
