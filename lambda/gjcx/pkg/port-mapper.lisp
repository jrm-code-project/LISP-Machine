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


;;; Implementation of Port Mapper in Common Lisp.
;;; Following the "Port Mapper Program Protocol"
;;; Published by Sun Microsystems, Revision B of 17 February 1986.
;;;
;;; 17-Jul-86 09:30:33 -George Carrette

;;; the port mapper is the one program that is assigned a definite port
;;; for obvious reasons. All other programs might have different ports
;;; on different machines, so the port mapper must be invoked for each host
;;; we contact. Presumably it is legal to cache the results. (Although mappings
;;; can be done and undone).

(defvar *port-mapper-program* 100000)
(defvar *port-mapper-port* 111)

;; note: sun gives the PROTocol slot of the mapping structures
;; as a u_int, and has #define for ipproto_tcp and ipproto_udp.
;; but we might as well make that an enumeration.

(xdr_enumeration 'protocol
                 '((tcp 6)
                   (udp 17))
                 ;; make this relaxed just in case SUN implements a new transport.
                 ;; e.g. shared-memory
                 :relaxed t)

(defun pmapproc_null (host)
  (make-rpc-udp-call host
                     *port-mapper-port*
                     *port-mapper-program*
                     2
                     0
                     nil
                     nil))

(defun pmapproc_set (host prog vers prot port)
  (car (make-rpc-udp-call host
                          *port-mapper-port*
                          *port-mapper-program*
                          2
                          1
                          `((,prog u_int)
                            (,vers u_int)
                            (,prot protocol)
                            (,port u_int))
                          '(boole))))

(defun pmapproc_unset (host prog vers)
  (car (make-rpc-udp-call host
                          *port-mapper-port*
                          *port-mapper-program*
                          2
                          2
                          `((,prog u_int)
                            (,vers u_int)
                            (tcp protocol)      ; ignored
                            (0 u_int))          ; ignored
                          '(boole))))

(defun pmapproc_getport (host prog vers prot)
  (car (make-rpc-udp-call host
                          *port-mapper-port*
                          *port-mapper-program*
                          2
                          3
                          `((,prog u_int)
                            (,vers u_int)
                            (,prot protocol)
                            (0 u_int))          ; ignored
                          '(u_int))))


(xdr_structure 'port-mapping
               '((prog u_int)
                 (vers u_int)
                 (prot protocol)
                 (port u_int)))


(defun pmapproc_dump (host)
  (car (make-rpc-udp-call host
                          *port-mapper-port*
                          *port-mapper-program*
                          2
                          4
                          ()
                          '((list port-mapping)))))



(defun pmapproc_callit (host prog vers proc args)
  (make-rpc-udp-call host
                     *port-mapper-port*
                     *port-mapper-program*
                     2
                     5
                     `((,prog u_int)
                       (,vers u_int)
                       (,proc u_int)
                       (,args string))
                     '(u_int string)))



(defvar *programs* nil)

(defun record-program (name number version)
  (pushnew name *programs*)
  (setf (get name 'program-version) version)
  (setf (get name 'program-number) number)
  (setf (get name 'program-port-alist) nil)
  name)

(defun flush-program-port-cache ()
  (dolist (name *programs*)
    (setf (get name 'program-port-alist) ())))


#+lmi
(si:add-initialization "rpc port cache"
                       '(flush-program-port-cache)
                       '(:before-cold))


(defun describe-pmap-dump (list)
  (format t "~&   Program  Version  Transport  Port~%")
  (dolist (e list)
    (format t "~10D   ~2D       ~5D    ~5D~%"
            (or (car (mem #'(lambda (number symbol)
                              (eq number (get symbol 'program-number)))
                          (xdr_structure_ref e 'prog)
                          *programs*))
                (xdr_structure_ref e 'prog))
            (xdr_structure_ref e 'vers)
            (xdr_structure_ref e 'prot)
            (xdr_structure_ref e 'port))))


(defun describe-pmap-cache ()
  (format t "~&Program(Version) HOST(TRANSPORT)PORT ...~%")
  (dolist (name *programs*)
    (format t "~10A(~2D)~:{ ~A(~D)~A~}~%"
            name
            (get name 'program-version)
            (get name 'program-port-alist))))



(defun get-program-port (program host &optional (transport 'udp))
  (check-type program symbol)
  (check-type host si:host)
  (let ((number (or (get program 'program-number)
                    (error "unknown program name: ~S" program))))
    (or (caddar (mem #'(lambda (ignore entry)
                         (and (eq host (car entry))
                              (eq transport (cadr entry))))
                     ()
                     (get program 'program-port-alist)))
        (let ((port (pmapproc_getport host number (get program 'program-version) transport)))
          (cond ((or (null port) (zerop port))
                 port)
                ('else
                 (with-lock ((get program 'get-program-port))
                   ;; going to update a global data structure.
                   (push (list host transport port)
                         (get program 'program-port-alist)))
                 port))))))




(defun program-udp-call (ahost program-procedure-version arglist retlist &key cred verf)
  (let ((host (si:parse-host ahost))
        (program (car program-procedure-version))
        (procedure (cadr program-procedure-version))
        (version (caddr program-procedure-version)))
    (let ((port (get-program-port program host)))
      (cond ((null port)
             (error "host not responding: ~S" host))
            ((zerop port)
             (error "program ~S not available on host ~S" program host))
            ('else
             (values-list (make-rpc-udp-call host
                                             port
                                             (get program 'program-number)
                                             version
                                             procedure
                                             arglist
                                             retlist
                                             :cred cred
                                             :verf verf)))))))


;; now for the server side of things

(record-program 'port-mapper *port-mapper-program* 1)

(putprop 'port-mapper *port-mapper-port* 'rpc:required-port)

(defun run-port-mapper ()
  (rpc:run-rpc-udp-server 'port-mapper))

(defprop port-mapper
         (((0 2) pmapproc_null () ())
          ((1 2) pmapproc_set (u_int u_int protocol u_int) (boole))
          ((2 2) pmapproc_unset (u_int u_int protocol u_int) (boole))
          ((3 2) pmapproc_getport (u_int u_int protocol u_int) (u_int))
          ((4 2) pmapproc_dump () ((list port-mapping)))
          ((5 2) pmapproc_callit (u_int u_int u_int string) (u_int string)))
  rpc:procedure-alist)


(defvar *port-mappings* nil)


(defun rpc::set-local-udp-rpc-mapping (program-name port)
  (or (eq program-name 'port-mapper)
      (set-port-mapping (get program-name 'program-number)
                        (get program-name 'program-version)
                        'udp
                        port)))


(defun rpc::unset-local-udp-rpc-mapping (program-name)
  (unset-port-mapping (get program-name 'program-number)
                      (get program-name 'program-version)))

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


(defun unset-port-mapping (prog vers)
  (with-lock ((get '*port-mappings* 'lock))
    (do ((item))
        ((not (setq item (car (mem #'(lambda (ignore x)
                                       (and (eq (xdr_structure_ref x 'prog) prog)
                                            (eq (xdr_structure_ref x 'vers) vers)))
                                   ()
                                   *port-mappings*)))))
      (setq *port-mappings* (remq item *port-mappings*)))))


;; the procedures

(defun (pmapproc_null rpc:service-procedure) ()
  ())

(defun (pmapproc_set rpc:service-procedure) (prog vers prot port)
  (set-port-mapping prog vers prot port)
  '(t))


(defun (pmapproc_unset rpc:service-procedure) (prog vers ignore ignore)
  (unset-port-mapping prog vers)
  '(t))

(defun (pmapproc_getport rpc:service-procedure) (prog vers prot ignore)
  (let ((map (lookup-port-mapping prog vers prot)))
    (cond ((not map)
           (list 0))
          ('else
           (list (xdr_structure_ref map 'port))))))


(defun (pmapproc_dump rpc:service-procedure) ()
  (list *port-mappings*))
