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


(record-program 'mount 100005 1)

(defvar *mntpathlen* 1024)
(defvar *mntnamlen* 255)
(defvar *fhsize* 32)


(xdr_typedef 'fhandle (list 'opaque *fhsize*))

(xdr_descriminated_union 'fhstatus
                         'stat
                         '((nfs_ok fhandle)
                           (default void)))


(xdr_typedef 'dirpath (list 'string *mntpathlen*))


(xdr_typedef 'name (list 'string *mntnamlen*))

(defvar *default-auth* rpc:*null-auth*)

(defun mntproc_null (host)
  (program-udp-call host '(mount 0 1) () ()))

(defun mntproc_mnt (host directory)
  (program-udp-call host '(mount 1 1)
                    `((,directory dirpath))
                    '(fhstatus)
                    :cred *default-auth*
                    :verf *default-auth*))



(xdr_structure 'mountpair
               '((hostname name)
                 (directory dirpath)))


(defun mntproc_dump (host)
  (program-udp-call host '(mount 2 1)
                    ()
                    '((list mountpair))))

(defun mntproc_umnt (host directory)
  (program-udp-call host '(mount 3 1)
                    `((,directory dirpath))
                    ()))

(defun mntproc_umntall (host)
  (program-udp-call host '(mount 4 1) () ()))

;(xdr_structure 'export-item-1
;              '((filesys dirpath)
;                (groups (list name))))
;
;(xdr_structure 'export-item
;              '((a export-item-1)
;                (b (list mountpair))))
;
;; appears to be a bug in structure definition.
; this follows the text and makes more sense of data coming from sun:

(xdr_structure 'export-item
               '((filesys dirpath)
                 (groups (list name))))

(defun mntproc_export (host)
  (program-udp-call host '(mount 5 1)
                    ()
                    '((list export-item))))


;; the server side.
;; what should file handles be?
;; We dont have anything like the unix situation.
;; but we need something to uniquely identify a file over the
;; lifetime of the universe.
;;
;; Keep a counter, *fhandle-kounter*.'
;; Must keep this on disk. Say in a file opened in I/O mode.
;; On the plist of every file, keep a fhandle.
;; If fhandle is NIL then when asked for then
;; putprop incrementing the counter. That way only files
;; accessed via the NFS mechanism need the extra property.
;;
;; for now though: filename using string-for-host
;; "" means root directory.


(defun run-mount-server ()
  (rpc:run-rpc-udp-server 'mount))


(defprop mount
         (((0 1) mntproc_null () ())
          ((1 1) mntproc_mnt (dirpath) (fhstatus))
          ((2 1) mntproc_dump () ((list mountpair)))
          ((3 1) mntproc_umnt (dirpath) ())
          ((4 1) mntproc_umntall () ())
          ((5 1) mntproc_export () ((list export-item))))
  rpc:procedure-alist)


(defvar *mount-entries* nil)



(defun (mntproc_null rpc:service-procedure) ()
  ())

(defvar *root-file-handle* (make-string *fhsize*))

(defun (mntproc_mnt rpc:service-procedure) (directory)
  (multiple-value-bind (type auth)
      (rpc:call-msg-auth)
    (let ((machine (if (eq type 'rpc:auth_unix)
                       (xdr_structure_ref auth 'rpc:machinename)
                     "???")))
      (when rpc:*rpc-server-trace*
        (format t "~&~S asking to mount ~S" machine directory))
      (cond ((string-equal directory "/")
             (let ((e (xdr_make_structure 'mountpair
                                          'hostname machine
                                          'directory "/")))
               (push e *mount-entries*))
             (list (xdr_make_descrimination 'nfs_ok *root-file-handle*)))
            ('else
             (list (xdr_make_descrimination 'nfserr_noent
                                            nil)))))))


(defun (mntproc_dump rpc:service-procedure) ()
  (list *mount-entries*))


(defun (mntproc_export rpc:service-procedure) ()
  (list (list (xdr_make_structure 'export-item
                                  'filesys "/"
                                  'groups nil))))
