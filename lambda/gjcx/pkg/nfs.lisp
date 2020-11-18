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


(record-program 'nfs 100003 2)

(defvar *maxdata* 8192) ;; actually, for me now is less than 2048 tcp:bsize.

(defvar *maxpathlen* 1024)

(defvar *maxnamlen* 255)

(defvar *cookiesize* 4)

; defined in mount protocol file
;(defvar *fhsize* 32)

(xdr_enumeration 'stat
                 '((nfs_ok 0)
                   (nfserr_perm 1)
                   (nfserr_noent 2)
                   (nfserr_io 5)
                   (nfserr_nxio 6)
                   (nfserr_acces 13)
                   (nfserr_exist 17)
                   (nfserr_nodev 19)
                   (nfserr_notdir 20)
                   (nfserr_isdir 21)
                   (nfserr_fbig 27)
                   (nfserr_nospc 28)
                   (nfserr_rofs 30)
                   (nfserr_nametoolong 63)
                   (nfserr_notempty 66)
                   (nfserr_dquot 69)
                   (nfserr_stale 70)
                   (nfserr_wflush 99)))

(xdr_enumeration 'ftype
                 '((nfnon 0)
                   (nfreg 1)
                   (nfdir 2)
                   (nfblk 3)
                   (nfchr 4)
                   (nflnk 5)))

; defined in mount protocol file
;(xdr_typedef 'fhandle (list 'opaque *fhsize*))

(xdr_structure 'timeval
               ;; time since Jan 1, 1970 GMT.
               '((seconds u_int)
                 (useconds u_int)))

(xdr_structure 'fattr
               '((type ftype)
                 (mode u_int)
                 (nlink u_int)
                 (uid u_int)
                 (gid u_int)
                 (size u_int)
                 (blocksize u_int)
                 (rdev u_int)
                 (blocks u_int)
                 (fsid u_int)
                 (fileid u_int)
                 (atime timeval)
                 (mtime timeval)
                 (ctime timeval)))


(xdr_structure 'sattr
               '((mode u_int)
                 (uid u_int)
                 (gid u_int)
                 (size u_int)
                 (atime timeval)
                 (mtime timeval)))

(xdr_typedef 'filename `(string ,*maxnamlen*))

(xdr_typedef 'path `(string ,*maxpathlen*))

(xdr_descriminated_union 'attrstat
                         'stat
                         '((nfs_ok fattr)
                           (default void)))


(xdr_structure 'diropargs
               '((dir fhandle)
                 (name filename)))


(xdr_structure 'diropres_ok
               '((file fhandle)
                 (attributes fattr)))

(xdr_descriminated_union 'diropres
                         'stat
                         '((nfs_ok diropres_ok)
                           (default void)))


(defun nfsproc_null (host)
  (program-udp-call host '(nfs 0 2) () ()))

(defun nfsproc_getattr (host file)
  (program-udp-call host '(nfs 1 2)
                    `((,file fhandle))
                    '(attrstat)))



(defun nfsproc_setattr (host file attributes)
  (program-udp-call host '(nfs 2 2)
                    `((,file fhandle)
                      (,attributes sattr))
                    '(attrstat)))


;; obsolete: nfsproc_root '(nfs 3 2)

(defun nfsproc_lookup (host which)
  (program-udp-call host '(nfs 4 2)
                    `((,which diropargs))
                    '(diropres)))

(defun nfsproc_readlink (host file)
  (program-udp-call host '(nfs 5 2)
                    `((,file fhandle))
                    '((descrimination stat
                                      (nfs_ok path)
                                      (default void)))))

(xdr_structure 'read_reply
               `((attributes fattr)
                 (data (string ,*maxdata*))))


(defun nfsproc_read (host file offset count)
  (program-udp-call host '(nfs 6 2)
                    `((,file fhandle)
                      (,offset u_int)
                      (,count u_int)
                      (0 u_int))
                    '((descrimination stat
                                      (nfs_ok read_reply)
                                      (default void)))))

;; obsolete: nfsproc_writecache (nfs 7 2)

(defun nfsproc_write (host file offset data)
  (program-udp-call host '(nfs 8 2)
                    `((,file fhandle)
                      (0 u_int)
                      (,offset u_int)
                      (0 u_int)
                      (,data (string ,*maxdata*)))
                    '(attrstat)))




(defun nfsproc_create (host where attributes)
  (declare (values dir))
  (program-udp-call host '(nfs 9 2)
                    `((,where diropargs)
                      (,attributes sattr))
                    '(diropres)))
