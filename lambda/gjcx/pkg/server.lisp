;;; -*- Mode:LISP; Package:SUN; Base:10; Readtable:CL -*-


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


(defun on-udp-socket (f &optional local-port &aux socket)
  (unwind-protect
      (progn (setq socket (tcp:open-socket :type 'tcp:sock-dgram
                                           :address (cond (local-port
                                                           (tcp:make-socket-address
                                                             :port local-port))
                                                          ('else
                                                           :unspecified))))
             (funcall f socket))
    (and socket (tcp:close-socket socket))))



(defun run-port-mapper ()
  (on-udp-socket 'port-mapper-toplevel *port-mapper-port*))


(defun port-mapper-toplevel (socket)
  (let ((from (tcp:make-socket-address))
        (data (make-string tcp:bsize)))
    (do-forever
      (multiple-value-bind (reply-data reply-length)
          (port-mapper-1 data (tcp:udp-socket-read socket data from))
        (when reply-length
          (tcp:udp-socket-write socket reply-data
                                from
                                reply-length))))))


(defun port-mapper-1 (array length)
  (let ((receive-xdrs (xdrmem_create :decode array 0 length))
        (transmit-xdrs (xdrmem_create :encode array)))
    (let ((message (xdr_receive receive-xdrs 'rpc:rpc_msg)))
      (let ((body (descriminated-value (xdr_structure_ref message 'rpc:body))))
        (let ((item (ass #'equal (list (xdr_structure_ref body 'rpc:proc)
                                       (xdr_structure_ref body 'rpc:vers))
                         *port-mapper*)))
          (cond ((and item (get (cadr item) 'service-procedure))
                 (let* ((args (mapcar #'(lambda (argtype)
                                                  (xdr_receive receive-xdrs argtype))
                                              (caddr item)))
                        (values (apply (get (cadr item) 'service-procedure) args)))
                   ;; need to handle error conditions too.
                   (fill array 0)
                   (xdr_transmit transmit-xdrs
                                 (make-accepted-msg (xdr_structure_ref message 'rpc:xid)
                                                    'rpc:success)
                                 'rpc:rpc_msg)
                   (mapc #'(lambda (val type)
                            (xdr_transmit transmit-xdrs val type))
                         values
                         (cadddr item))
                   (values array (xdr_getpos transmit-xdrs))))
                ('else
                 (fill array 0)
                 (xdr_transmit transmit-xdrs
                               (make-accepted-msg (xdr_structure_ref message 'rpc:xid)
                                                  'rpc:proc_unavail)
                               'rpc:rpc_msg)
                 (values array (xdr_getpos transmit-xdrs)))))))))


(defun make-accepted-msg (xid accept_stat)
  (xdr_make_structure
    'rpc:rpc_msg
    'rpc:xid xid
    'rpc:body (xdr_make_descrimination
                'rpc:reply
                (xdr_make_descrimination
                  'rpc:msg_accepted
                  (xdr_make_structure
                    'rpc:accepted_reply
                    'rpc:verf rpc:*null-auth*
                    'rpc:result
                    (xdr_make_descrimination
                      accept_stat
                      ()))))))


(defvar *port-mapper*
        '(((0 2) pmapproc_null () ())
          ((1 2) pmapproc_set (u_int u_int protocol u_int) (boole))
          ((2 2) pmapproc_unset (u_int u_int protocol u_int) (boole))
          ((3 2) pmapproc_getport (u_int u_int protocol u_int) (u_int))
          ((4 2) pmapproc_dump () ((list port-mapping)))
          ((5 2) pmapproc_callit (u_int u_int u_int string) (u_int string))))

(defun (pmapproc_null service-procedure) ()
  ())

(defvar *port-mappings* nil)

(defun lookup-port-mapping (prog vers prot)
  (car (mem #'(lambda (ignore x)
                (and (eq (xdr_structure_ref x 'prog) prog)
                     (eq (xdr_structure_ref x 'vers) vers)
                     (eq (xdr_structure_ref x 'prot) prot)))
            ()
            *port-mappings*)))

(defun set-port-mapping (prog vers prot port)
  (with-lock ((get '*port-mappings* 'lock))
    (setq *port-mappings*
          (append (remq (lookup-port-mapping prog vers prot)
                        *port-mappings*)
                  (list (xdr_make_structure 'port-mapping
                                            'prog prog
                                            'vers vers
                                            'prot prot
                                            'port port))))))

(defun (pmapproc_set service-procedure) (prog vers prot port)
  (set-port-mapping prog vers prot port)
  '(t))



(defun (pmapproc_unset service-procedure) (prog vers ignore ignore)
  (with-lock ((get '*port-mappings* 'lock))
    (do ((item))
        ((not (setq item (car (mem #'(lambda (ignore x)
                                       (and (eq (xdr_structure_ref x 'prog) prog)
                                            (eq (xdr_structure_ref x 'vers) vers)))
                                   ()
                                   *port-mappings*)))))
      (setq *port-mappings* (remq item *port-mappings*))))
  '(t))

(defun (pmapproc_getport service-procedure) (prog vers prot ignore)
  (let ((map (lookup-port-mapping prog vers prot)))
    (cond ((not map)
           (list 0))
          ('else
           (list (xdr_structure_ref map 'port))))))


(defun (pmapproc_dump service-procedure) ()
  (list *port-mappings*))
