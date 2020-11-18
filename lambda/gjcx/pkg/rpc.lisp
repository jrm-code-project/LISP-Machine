;;; -*- Mode:LISP; Package:RPC; Readtable:CL; Base:10 -*-

;;;  (c) LISP Machine, Inc. 1986
;;;      All Rights Reserved
;;;  See filename "copyright.text" for
;;;  further information.


;;; (C) Copyright 1987, LISP MACHINE INC
;;; See filename "Copyright.Text" for more information.
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


;;; Implementation of RPC in Common Lisp.
;;; Following the "Remote Procedure Call Protocol Specification"
;;; Published by Sun Microsystems, Revision B of 17 February 1986.
;;;
;;; 17-Jul-86 09:30:33 -George Carrette


(xdr_enumeration 'msg_type
                 '((call 0)
                   (reply 1)))

(xdr_enumeration 'reply_stat
                 '((msg_accepted 0)
                   (msg_denied 1)))


(xdr_enumeration 'accept_stat
                 '((success 0)
                   (prog_unavail 1)
                   (prog_mismatch 2)
                   (proc_unavail 3)
                   (garbage_args 4)))

(xdr_enumeration 'reject_stat
                 '((rpc_mismatch 0)
                   (auth_error 1)))

(xdr_enumeration 'auth_stat
                 '((auth_badcred 1)
                   (auth_rejectedcred 2)
                   (auth_badverf 3)
                   (auth_rejectedverf 4)
                   (auth_tooweak 5)))

(xdr_descriminated_union 'rpc_msg_body
                         'msg_type
                         '((call call_body)
                           (reply reply_body)))

(xdr_structure 'rpc_msg
               '((xid u_int)
                 (body rpc_msg_body)))


(xdr_structure 'call_body
               '((rpcvers u_int)
                 (prog u_int)
                 (vers u_int)
                 (proc u_int)
                 (cred opaque_auth)
                 (verf opaque_auth)
                 (parameters void)))

;; we must send the parameters manually.

(xdr_descriminated_union 'reply_body
                         'reply_stat
                         '((msg_accepted accepted_reply)
                           (msg_denied rejected_reply)))

(xdr_structure 'accepted_reply_prog_mismatch
               '((low u_int)
                 (high u_int)))

(xdr_descriminated_union 'accepted_reply_stat
                         'accept_stat
                         '((success void) ;; pick up the results by hand later
                           (prog_mismatch accepted_reply_prog_mismatch)
                           (default void)))


(xdr_structure 'accepted_reply
               '((verf opaque_auth)
                 (result accepted_reply_stat)))


(xdr_structure 'rejected_reply_rpc_mismatch
               '((low u_int)
                 (high u_int)))


(xdr_descriminated_union 'rejected_reply
                         'reject_stat
                         '((rpc_mismatch rejected_reply_rpc_mismatch)
                           (auth_error auth_stat)))


(xdr_enumeration 'auth_flavor
                 '((auth_null 0)
                   (auth_unix 1)
                   (auth_short 2)))

;; the auth_unix structure has a kludge in that opaque_auth
;; is in fact a structure ((flavor auth_flavor) (data (string 400)))
;; with the data being descructured as auth_unix.

(xdr_descriminated_union 'opaque_auth
                         'auth_flavor
                         '((default (string 400))))

(xdr_structure 'auth_unix
               '((stamp u_int)
                 (machinename (string 255))
                 (uid u_int)
                 (gid u_int)
                 (gids (array u_int 10))))


;;;


(defun make-opaque-auth-unix (&key
                       (machinename (string-downcase (send si:local-host :name)))
                       (uid 102)
                       (gid 7)
                       (gids #(7)))
  (let* ((array (make-string 400))
         (xdrs (xdrmem_create :encode array)))
    (xdr_transmit xdrs
                  (xdr_make_structure
                    'auth_unix
                    'stamp (random (expt 2 31))
                    'machinename machinename
                    'uid uid
                    'gid gid
                    'gids (coerce gids 'array))
                  'auth_unix)
    (xdr_make_descrimination 'auth_unix
                             (substring array 0 (xdr_getpos xdrs)))))

(defun decode-auth-unix-string (x)
  (xdr_receive (xdrmem_create :decode x) 'auth_unix))

(defvar *null-auth* (xdr_make_descrimination 'auth_null ""))

(defun make-rpc-call-msg (prog vers proc &key cred verf (xid 0))
  (xdr_make_structure
    'rpc_msg
    'xid xid
    'body (xdr_make_descrimination 'call
                                   (xdr_make_structure
                                     'call_body
                                     'rpcvers 2
                                     'prog prog
                                     'vers vers
                                     'proc proc
                                     'cred (or cred *null-auth*)
                                     'verf (or verf *null-auth*)))))

;; note that is is very easy to interpret bad data as good data.
;; (xdr_receive (xdrmem_create :decode (make-string 100)) 'rpc_msg)
;; will return a very reasonable looking object.

(defun make-rpc-udp-call (host port prog vers proc arglist retlist
                          &key
                          (transmit-tries 3) (receive-tries 5)
                          (receive-wait 1)
                          (data-length tcp:bsize)
                          cred
                          verf)
  (let* ((length)
         (array (make-string data-length))
         (transmit-xdrs (xdrmem_create :encode array))
         (frame (make-rpc-call-msg prog vers proc :xid 17 :cred cred :verf verf)))
    (xdr_transmit transmit-xdrs frame 'rpc_msg)
    (dolist (arg arglist)
      (xdr_transmit transmit-xdrs (car arg) (cadr arg)))
    (setq length (udp-transmit-and-receive host port array (xdr_getpos transmit-xdrs)
                                           transmit-tries receive-tries receive-wait))
    (COND (length
           (let ((receive-xdrs (xdrmem_create :decode array 0 length)))
             (let ((reply (xdr_receive receive-xdrs 'rpc_msg)))
               (check-rpc-reply-msg reply)
               (mapcar #'(lambda (retval)
                           (xdr_receive receive-xdrs retval))
                       retlist))))
          ('else
           (error "failed to contact host ~S" host)))))


(defun check-rpc-reply-msg (obj)
  (let ((v (descriminated-value (xdr_structure_ref obj 'body))))
    (or (eq (descriminator v) 'msg_accepted)
        (error "Reply was ~A because ~A" (descriminator v)
               (descriminated-value v)))
    (let ((r (xdr_structure_ref (descriminated-value v) 'result)))
      (or (eq (descriminator r) 'success)
          (error "rpc reply not success: ~S" r)))))


#+LMI
(defun udp-transmit-and-receive (host port array length transmit-tries receive-tries receive-wait)
  (with-open-file (stream (format nil "TCP-HOST:~A#~D" (send (si:parse-host host) :name) port)
                            :for-udp t)
      (block got-a-reply
        (dotimes (j transmit-tries)
          (send stream :write-packet array 0 length)
          (dotimes (j receive-tries)
            ;; cannot use the :listen inside a process-wait because the :listen sends
            ;; the board a message and also waits on the reply. Need to hack a special
            ;; wait operation for this. (send stream :listen-wait <timeout> "whostate")
            (if (send stream :listen) (return-from got-a-reply))
            (process-sleep receive-wait "rpc reply")
            (if (send stream :listen) (return-from got-a-reply))))
        (setq length nil))
      (when length
        (fill array 0)
        (send stream :read-packet array))))





;; for servers:

#+LMI
(defun call-on-udp-socket (f &optional local-port &aux socket)
  (unwind-protect
      (progn (setq socket (tcp:open-socket :type 'tcp:sock-dgram
                                           :address (cond (local-port
                                                           (tcp:make-socket-address
                                                             :port local-port))
                                                          ('else
                                                           :unspecified))))
             (funcall f socket))
    (and socket (tcp:close-socket socket))))

(defun run-rpc-udp-server (program)
  (call-on-udp-socket #'(lambda (socket)
                          (rpc-udp-server-toplevel socket program))
                      (get program 'required-port)))

(defun rpc-udp-server-toplevel (socket program)
  (unwind-protect
      (let ((local-address (tcp:make-socket-address)))
        (tcp:get-address-of-socket socket local-address)
        (set-local-udp-rpc-mapping program (tcp:socket-address-port local-address))
        (let ((from (tcp:make-socket-address))
              (data (make-string tcp:bsize)))
          (do-forever
            (multiple-value-bind (reply-data reply-length)
                (rpc-server-toplevel-1 (get program 'procedure-alist)
                                       data
                                       (tcp:udp-socket-read socket data from)
                                       from)
              (when reply-length
                (tcp:udp-socket-write socket reply-data
                                      from
                                      reply-length))))))
    (unset-local-udp-rpc-mapping program)))


(defun rpc-server-toplevel-1 (procedures array length from)
  (rpc-server-toplevel-2 procedures array length
                         (tcp:socket-address-internet-address from)))

(defvar *call-msg* nil)

(defun apply-service-procedure (message f args)
  (let ((*call-msg* message))
    (apply f args)))

(defun call-msg-auth (&optional (type 'cred))
  (declare (values type value))
  (let ((auth (xdr_structure_ref (descriminated-value (xdr_structure_ref *call-msg* 'body))
                                 (ecase type
                                   (cred 'cred)
                                   (verf 'verf)))))
    (values (descriminator auth)
            (ecase (descriminator auth)
              ((auth_null auth_short) (descriminated-value auth))
              (auth_unix
               (decode-auth-unix-string (descriminated-value auth)))))))

(defvar *rpc-server-trace* nil)

(defun rpc-server-toplevel-2 (procedures array length actual-inet)
  actual-inet
  ;; perhaps check actual inet address with machinename field
  ;; of authorization.
  (let ((receive-xdrs (xdrmem_create :decode array 0 length))
        (transmit-xdrs (xdrmem_create :encode array)))
    (let ((message (xdr_receive receive-xdrs 'rpc_msg)))
      (let ((body (descriminated-value (xdr_structure_ref message 'body))))
        (let ((item (ass #'equal (list (xdr_structure_ref body 'proc)
                                       (xdr_structure_ref body 'vers))
                         procedures)))
          (when *rpc-server-trace*
            (format t "~&Request ~S = ~S"
                    (list (xdr_structure_ref body 'proc)
                          (xdr_structure_ref body 'vers))
                    (cadr item)))
          (cond ((and item (get (cadr item) 'service-procedure))
                 (let* ((args (mapcar #'(lambda (argtype)
                                                  (xdr_receive receive-xdrs argtype))
                                              (caddr item)))
                        (values (apply-service-procedure
                                  message
                                  (get (cadr item) 'service-procedure)
                                  args)))
                   (xdr_transmit transmit-xdrs
                                 (make-accepted-msg (xdr_structure_ref message 'xid)
                                                    'success)
                                 'rpc_msg)
                   (mapc #'(lambda (val type)
                            (xdr_transmit transmit-xdrs val type))
                         values
                         (cadddr item))
                   (when *rpc-server-trace*
                     (format t " done."))
                   (values array (xdr_getpos transmit-xdrs))))
                ('else
                 (xdr_transmit transmit-xdrs
                               (make-accepted-msg (xdr_structure_ref message 'xid)
                                                  'proc_unavail)
                               'rpc_msg)
                 (when *rpc-server-trace*
                   (format t " proc_unavail."))
                 (values array (xdr_getpos transmit-xdrs)))))))))


(defun make-accepted-msg (xid accept_stat)
  (xdr_make_structure
    'rpc_msg
    'xid xid
    'body (xdr_make_descrimination
                'reply
                (xdr_make_descrimination
                  'msg_accepted
                  (xdr_make_structure
                    'accepted_reply
                    'verf *null-auth*
                    'result
                    (xdr_make_descrimination
                      accept_stat
                      ()))))))
