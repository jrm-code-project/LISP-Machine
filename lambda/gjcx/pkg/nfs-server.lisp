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


;; the server.

;; this required port is documented as a bug in the protocol
(putprop 'nfs 2049 'rpc:required-port)



#|| a mount that works, notice the long timeout.
mount -v -t nfs -o ro,fg,retry=5,timeo=100,retrans=4,soft,rsize=1024,wsize=1024 lam15:/ /lmi/lam15
||#

(defun run-nfs-server ()
  (rpc:run-rpc-udp-server 'nfs))

(putprop 'nfs
         `(((0 2) nfsproc_null () ())
           ((1 2) nfsproc_getattr (fhandle) (attrstat))
           ((2 2) nfsproc_setattr (fhandle sattr) (attrstat))
           ((3 2) nfsproc_root () ())
           ((4 2) nfsproc_lookup (diropargs) (diropres))
           ((5 2) nfsproc_readlink (fhandle) ((descrimination stat
                                                              (nfs_ok path)
                                                              (default void))))
           ((6 2) nfsproc_read (fhandle u_int u_int u_int)
            ((descrimination stat
                             (nfs_ok read_reply)
                             (default void))))
           ((7 2) nfsproc_writecache () ())
           ((8 2) nfsproc_write (fhandle u_int u_int u_int (string ,*maxdata*))
            (attrstat))
           ((9 2) nfsproc_create (diropargs sattr) (diropres))
           ((10 2) nfsproc_remove (diropargs) (stat))
           ((11 2) nfsproc_rename (diropargs diropargs) (stat))
           ((12 2) nfsproc_link (fhandle diropargs) (stat))
           ((13 2) nfsproc_symlink (diropargs path sattr) (stat))
           ((14 2) nfsproc_mkdir (diropargs sattr) (diropres))
           ((15 2) nfsproc_rmdir (diropargs) (stat))
           ((16 2) nfsproc_readdir (fhandle readdir-cookie  u_int)
            ((descrimination stat
                             (nfs_ok readdir-value)
                             (default void))))
           ((17 2) nfsproc_statfs (fhandle) ((descrimination stat
                                                             (nfs_ok fsattr)
                                                             (default void)))))
         'rpc:procedure-alist)

(xdr_typedef 'readdir-cookie `(opaque ,*cookiesize*))

(xdr_structure 'readdir-value
               '((entries (list readdir-entry))
                 (eof boole)))

(xdr_structure 'readdir-entry
               '((fileid u_int)
                 (filename name)
                 (cookie readdir-cookie)))

(xdr_structure 'fsattr
               '((tsize u_int)
                 (bsize u_int)
                 (blocks u_int)
                 (bfree u_int)
                 (bavail u_int)))

;; With all the datatypes and procedure names defined the hair involved in
;; writing the server is reduced to that of mapping NFS
;; operations and values to the LMI file system.
;; Issues:
;;  * filenames, type and version number.
;;  * "open" file concept.
;;  * FATTR: FILEID, reference time, modify time support.
;;  * opaque file handle
;;
;; We dont want to make modifications to the SYS:FILE; at this time, so we
;; implement without that.
;;
;; Assign each file a FILEID, a number that is incremented each time a new FILEID
;; is needed.
;; Various ideas on file types and versions. Type is obvious. foo.lisp is
;; foo.lisp. For now give full file name type and version when asked.
;; and when opening foo.lisp open foo.lisp#>. Could be confusing for the
;; Unix that foo.lisp doesnt show up in a directory list, but that
;; foo.lisp can be opened. Could have foo.lisp linked to highest version
;; or to something like foo.lisp.newest. Then foo.lisp exists, and foo.lisp.newest
;; would have special meaning (even though it doesnt exist and would not show up
;; in directory listings).
;; ??directory files. if listing / gives, foo.directory, bar.directory, baz.directory
;; it is ugly. could just punt the .directory. Could be a problem with name conflicts
;; if there is also a file foo without a type. Also would be that if
;; we need to open "foo" we would need to look for foo.directory first.
;; Cant disallow files without types of course since these are most common
;; in unix. But all this translation is taken care of by the opaque file handle.
;; It is a matter of presentation of names. Being possible to create files using
;; lisp operations that conflict.

(defstruct filesys
  host
  fileid-table
  max-fileid
  root-pathname)

(defvar *filesys* nil)

(defun boot-filesys (&optional (host si:local-host))
  ;; this will recurse, listing every file in every directory in
  ;; the file system. The only required reason is to find the max-fileid.
  ;; But it will speed up all further directory operations.
  (let ((time (time))
        (root (make-pathname :host host
                             :directory :ROOT
                             :device :unspecific
                             :name :wild
                             :type :wild
                             :version :wild)))
    (boot-filesys-1 root
                    (setq *filesys* (make-filesys :host host :fileid-table (make-hash-table)
                                                  :max-fileid 0
                                                  :root-pathname root)))
    (format t "~&Took ~S minutes~%"
            (quotient (time-difference (time) time) (* 60.0 60.0)))))

(defun describe-filesys ()
  (format t "~&Filesys host = ~S~%Max-Fileid = ~S~%"
          (filesys-host *filesys*)
          (filesys-max-fileid *filesys*))
  (describe (filesys-fileid-table *filesys*)))

(defun boot-filesys-1 (path x)
  (format t "~&~A~%" path)
  (do ((l (fs:directory-list path) (cdr l))
       (dirs nil))
      ((null l)
       (dolist (d dirs)
         (boot-filesys-1 (send (send d :pathname-as-directory) :new-pathname
                               :name :wild :type :wild :version :wild)
                         x)))
    (when (caar l)
      (let ((id (get (car l) :nfs-fileid)))
        (when id
          (puthash id (caar l) (filesys-fileid-table x))
          (setf (filesys-max-fileid x) (max id (filesys-max-fileid x)))))
      (when (get (car l) :directory)
        (or (string-equal "DIRECTORY" (send (caar l) :type))
            (error "cant handle directory without type directory: ~S" (caar l)))
        (push (caar l) dirs)))))

(defun universal->unix-time (ut)
  (xdr_make_structure 'timeval
                      'seconds (- ut
                                  (quote #, (time:parse-universal-time "Midnight Jan 1,1970")))
                      'useconds 0))

(defun fileid->pathname (id)
  (cond ((zerop id)
         ;; there is no pathname for the root.
         :ROOT)
        ('else
         (gethash id (filesys-fileid-table *filesys*)))))

(defun fhandle->fileid (fhandle)
  ;; just take the first 4 bytes as an integer
  (do ((n 0 (+ (* n 256) (aref fhandle j)))
       (j 0 (1+ j)))
      ((= j 4) n)))

(defun fileid->fhandle (id)
  (do ((s (make-string *fhsize*))
       (j 0 (1+ j)))
      ((= j 4) s)
    (setf (aref s j) (ldb (byte 8 (- 24 (* j 8))) id))))

(defvar *directory-file-mode*     #o0040777)
(defvar *regular-file-mode*       #o0100777)
(defvar *symbolic-link-file-mode* #o0120777)

(defun (nfsproc_getattr rpc:service-procedure) (file &aux fileid pathname probe)
  (setq fileid (fhandle->fileid file))
  (setq pathname (fileid->pathname fileid))
  (when rpc:*rpc-server-trace*
    (format t " ~A" pathname))
  (cond ((eq pathname :root)
         (list (xdr_make_descrimination
                 'nfs_ok
                 (root-fattr))))
        ((null pathname)
         (list (xdr_make_descrimination 'nfserr_stale nil)))
        ((errorp (setq probe (open pathname :direction nil :error nil)))
         ;; depending on error could be different problems
         ;; nfserr_state or nfserr_noent.
         (list (xdr_make_descrimination 'nfserr_stale nil)))
        ('else
         (list (xdr_make_descrimination
                 'nfs_ok
                 (probe-stream->fattr probe fileid))))))

(defun probe-stream->fattr (probe &optional fileid)
  (when (not fileid)
    (or (setq fileid (get probe :nfs-fileid))
        (let ((n (1+ (filesys-max-fileid *filesys*))))
          (setf (filesys-max-fileid *filesys*) n)
          ;; this next operation can be amazingly slow.
          (fs:change-file-properties (send probe :truename) t :nfs-fileid n)
          (putprop probe n :nfs-fileid)
          (puthash n (send probe :truename) (filesys-fileid-table *filesys*))
          (setq fileid n))))
  (let ((date (universal->unix-time (get probe :creation-date))))
    (xdr_make_structure
      'fattr
      'type (if (get probe :directory) 'nfdir 'nfreg)
      'mode (if (get probe :directory) *directory-file-mode* *regular-file-mode*)
      'nlink 1
      'uid 0
      'gid 0
      'size (get probe :length-in-bytes)
      'blocksize 1024
      'rdev 0
      'blocks (or (get probe :length-in-blocks)
                  (ceiling (get probe :length-in-bytes) 1024))
      'fsid 0
      'fileid fileid
      'atime date
      'mtime date
      'ctime date)))

(defun root-fattr ()
  (xdr_make_structure
    'fattr
    'type 'nfdir
    'mode *directory-file-mode*
    'nlink 1
    'uid 0
    'gid 0
    'size (* 1024 (+ (aref fs:put-usage-array fs:put-used) (aref fs:put-usage-array fs:put-free)
                     (aref fs:put-usage-array fs:put-reserved)))
    'blocksize 1024
    'rdev 0                                     ; device number!
    'blocks (aref fs:put-usage-array fs:put-free)
    'fsid 0                                     ; file system ID
    'fileid 0
    'atime (universal->unix-time (time:get-universal-time))     ; reference date
    'mtime (universal->unix-time (time:get-universal-time))     ; modify date
    'ctime (universal->unix-time (time:get-universal-time))     ; status chng time
    ))



(defun (nfsproc_statfs rpc:service-procedure) (file &aux fileid pathname)
  (setq fileid (fhandle->fileid file))
  (setq pathname (fileid->pathname fileid))
  (when rpc:*rpc-server-trace*
    (format t " ~A" pathname))
  (list (xdr_make_descrimination
          'nfs_ok
          (xdr_make_structure
            'fsattr
            'tsize 1024
            'bsize 1024
            'blocks 10000
            'bfree  5000
            'bavail 1000))))

(defun (nfsproc_null rpc:service-procedure) ()
  ())


(defun integer->cookie (n)
  (do ((s (make-string *cookiesize*))
       (j 0 (1+ j)))
      ((= j 4) s)
    (setf (aref s j) (ldb (byte 8 (- 24 (* j 8))) n))))

(defun cookie->integer (cookie)
  ;; just take the first 4 bytes as an integer
  (do ((n 0 (+ (* n 256) (aref cookie j)))
       (j 0 (1+ j)))
      ((= j 4) n)))

;; exact semantics of cookie determined by experimentation.
;; seems that an ls -l on a dir had us being asked for the entries
;; in a dir given the cookie of the last file in the dir.
;; And the user side knew it was the last entry too.
;; So it seems the cookie means to point to the rest of the list exclusive
;; of the enter cooresponding to the cookie.

(defun (nfsproc_readdir rpc:service-procedure) (dir cookie count)
  ;; number of bytes taken by one entry is max
  ;; 4 + (4) + (4 + *maxnamlen*) + *cookiesize*
  ;; list_marker + u_int + name + readdir-cookie
  ;; here is a case where we would want to do our own xdr operations.
  ;; or using the xdrsize construct.
  ;; Plus overhead of status + eof = 8.
  ;; Let the cookie be the file-id of the next file in the sequence.
  (let* ((fileid (fhandle->fileid dir))
         (pathname (fileid->pathname fileid)))
    (when rpc:*rpc-server-trace*
      (format t " ~A after ~A" pathname
              (let ((i (cookie->integer cookie)))
                (if (zerop i) "begining" (fileid->pathname i)))))
    (cond ((null pathname)
           (list (xdr_make_descrimination 'nfserr_stale nil)))
          ('else
           (let ((probe (or (eq :root pathname)
                            (open pathname :direction nil :error nil)))
                 (index (cookie->integer cookie)))
             (cond ((errorp probe)
                    (list (xdr_make_descrimination 'nfserr_stale nil)))
                   ((not (or (eq probe t) (get probe :directory)))
                    (list (xdr_make_descrimination 'nfserr_notdir nil)))
                   ('else
                    (list (some-directory-entries pathname index count)))))))))

(defun some-directory-entries (pathname index count)
  (do ((l (directory-list (if (eq :root pathname)
                              (filesys-root-pathname *filesys*)
                            (send (send pathname :pathname-as-directory)
                                  :new-pathname :name :wild :type :wild
                                  :Version :wild))
                          (not (zerop index)))
          (cdr l))
       (size-tester (xdrsize_create))
       (entries nil)
       (looking (zerop index))
       (p))
      ((null l)
       (xdr_make_descrimination
         'nfs_ok
         (xdr_make_structure
           'readdir-value
           'entries (nreverse entries)
           'eof t)))
    (setq p (car l))
    (when (car p)
      (cond (looking
             (let ((entry (xdr_make_structure
                            'readdir-entry
                            'fileid (get p :nfs-fileid)
                            'filename (pathname->name (car p)
                                                      (get p :directory))
                            'cookie (integer->cookie (get p :nfs-fileid)))))
               (xdr_transmit size-tester entry 'readdir-entry)
               (cond ((> (+ (xdr_getpos size-tester) 4) count)
                      (return (xdr_make_descrimination
                                'nfs_ok
                                (xdr_make_structure
                                  'readdir-value
                                  'entries (nreverse entries)
                                  'eof nil))))
                     ('else
                      (push entry entries)))))
            ((= index (get (car l) :nfs-fileid))
             (setq looking t))))))


(defun parse-unix-name (string &aux i ii name type)
  ;; should actually be looking from the right hand side
  ;; for version number first. then type. then name is rest.
  (cond ((not (setq i (string-search "." string)))
         string)
        ((progn (setq name (substring string 0 i))
                (not (setq ii (string-search "." string (1+ i)))))
         (values name (substring string (1+ i))))
        ((progn (setq type (substring string (1+ i) ii))
                (cond ((string-equal string "newest" :start1 (1+ ii))
                       (setq i :newest))
                      ('else
                       (setq i (catch-error (parse-integer string :start (1+ ii)) nil)))))
         (values name type i))
        ('else
         string)))


(defun nfsproc_lookup-directory-pathname (pathname)
  (let ((pdir (send pathname :directory)))
    (cond ((eq pdir :root)
           (list (xdr_make_descrimination
                   'nfs_ok
                   (xdr_make_structure
                     'diropres_ok
                     'file (fileid->fhandle 0)
                     'attributes (root-fattr)))))
          ('else
           (let ((probe (open (send pathname :directory-pathname-as-file) :direction nil :error nil )))
             (cond ((errorp probe)
                    (list (xdr_make_descrimination 'nfserr_stale nil)))
                   ('else
                    (let ((fattr (probe-stream->fattr probe)))
                      (list (xdr_make_descrimination
                              'nfs_ok
                              (xdr_make_structure
                                'diropres_ok
                                'file (fileid->fhandle (xdr_structure_ref fattr 'fileid))
                                'attributes fattr)))))))))))


(defun (nfsproc_lookup rpc:service-procedure) (which)
  (let ((dir (xdr_structure_ref which 'dir))
        (name (xdr_structure_ref which 'name)))
    (let* ((fileid (fhandle->fileid dir))
           (pathname (fileid->pathname fileid)))
      (setq pathname (cond ((eq :root pathname)
                            (filesys-root-pathname *filesys*))
                           (pathname
                            (send pathname :pathname-as-directory))))
      (when rpc:*rpc-server-trace*
        (format t " ~S in directory ~A" name pathname))
      (cond ((null pathname)
             (list (xdr_make_descrimination 'nfserr_stale nil)))
            ((string-equal name ".")
             (nfsproc_lookup-directory-pathname pathname))
            ((string-equal name "..")
             (cond ((eq :root (send pathname :directory))
                    (list (xdr_make_descrimination 'nfserr_noent nil)))
                    ('else
                     (nfsproc_lookup-directory-pathname (send pathname :directory-pathname-as-file)))))
            ('else
             (multiple-value-bind (name type version)
                 (parse-unix-name name)
               (when (and (not type) (not version))
                 (let ((c (get pathname :directory-name-cache)))
                   (when c
                     (setq type (gethash name c)))))
               (let ((pfile (send pathname :new-pathname :name name :type type :Version version))
                     (probe))
                 (cond ((and (errorp (setq probe (open pfile :direction nil :error nil)))
                             (or type
                                 version
                                 (progn (setq probe (open (send pfile :new-type "DIRECTORY") :direction nil :error nil))
                                        (cond ((errorp probe)
                                               t)
                                              ('else
                                               (puthash name "DIRECTORY"
                                                        (or (get pathname :directory-name-cache)
                                                            (putprop pathname
                                                                     (make-hash-table :test #'string-equal)
                                                                     :directory-name-cache)))
                                               nil)))))
                        (list (xdr_make_descrimination
                                (cond ((typep probe 'fs:directory-not-found-error)
                                       'nfserr_stale)
                                      ('else 'nfserr_noent))
                                nil)))
                       ('else
                        (let ((fattr (probe-stream->fattr probe)))
                          (list (xdr_make_descrimination
                                  'nfs_ok
                                  (xdr_make_structure
                                    'diropres_ok
                                    'file (fileid->fhandle (xdr_structure_ref fattr 'fileid))
                                    'attributes fattr)))))))))))))


(defun pathname->name (pathname &optional dirp)
  (or (get pathname :nfs-name)
      (putprop pathname
               (if dirp
                   (string-downcase (send pathname :name))
                 (format nil "~A.~A.~D"
                         (string-downcase (send pathname :name))
                         (string-downcase (send pathname :type))
                         (send pathname :version)))
               :nfs-name)))


(defvar *dirlist-cache* (make-hash-table))

(defun directory-list (path cachep)
  (cond (cachep
         (or (gethash path *dirlist-cache*)
             (directory-list path nil)))
        ('else
         (let ((l (fs:directory-list path)))
           (setup-fileid-properties l)
           (puthash path l *dirlist-cache*)
           l))))


(defun setup-fileid-properties (dirlist)
  ;; make sure all files in directory list have :nfs-fileid properties.
  (do ((l dirlist (cdr l))
       (losers nil))
      ((null l)
       (when losers
         (fs:multiple-change-file-properties t losers))
       dirlist)
    (let ((p (car l)))
      (cond ((null (car p)))
            ((get p :nfs-fileid))
          ('else
           (let ((n (1+ (filesys-max-fileid *filesys*))))
             (setf (filesys-max-fileid *filesys*) n)
             (push (list (car p) :nfs-fileid n) losers)
             (putprop p n :nfs-fileid)
             (puthash n (car p) (filesys-fileid-table *filesys*))))))))


(defun (nfsproc_read rpc:service-procedure) (file offset count totalcount)
  totalcount
  (let* ((fileid (fhandle->fileid file))
         (pathname (fileid->pathname fileid)))
    (when rpc:*rpc-server-trace*
      (format t " ~A ~D ~D" pathname offset count))
    (cond ((not pathname)
           (list (xdr_make_descrimination 'nfserr_stale nil)))
          ((eq pathname :root)
           (list (xdr_make_descrimination 'nfserr_isdir nil)))
          ('else
           (let ((stream (open-input-stream pathname))
                 (length))
             (cond ((errorp stream)
                    (list (xdr_make_descrimination
                            (cond ((memq 'FS::INVALID-OPERATION-FOR-DIRECTORY (send stream :condition-names) )
                                   'nfserr_isdir)
                                  ('else 'nfserr_stale))
                            nil)))
                   ((< offset (setq length (get stream :length-in-bytes)))
                    (send stream :set-pointer offset)
                    (let ((string (make-string count :fill-pointer 0)))
                      (send stream :string-in nil string)
                      (string-translate-lispm->unix string)
                      (when (= (+ (fill-pointer string) offset) length)
                        ;; heuristic, flush the cache at the end of the file
                        (flush-open-input-stream-cache stream))
                      (list (xdr_make_descrimination
                              'nfs_ok
                              (xdr_make_structure 'read_reply
                                                  'attributes (probe-stream->fattr stream)
                                                  'data string)))))
                   ('else
                    ;; a unix artifact that we are asked to read off then end of
                    ;; the file.
                    (flush-open-input-stream-cache stream)
                    (list (xdr_make_descrimination
                            'nfs_ok
                            (xdr_make_structure 'read_reply
                                                'attributes (probe-stream->fattr stream)
                                                'data ""))))))))))

(defvar *open-input-stream-cache* nil)

(defun open-input-stream (pathname)
  (or (cdr (assq pathname *open-input-stream-cache*))
      (let ((stream (open pathname :direction :input :error nil)))
        (cond ((errorp stream)
               stream)
              ('else
               (let ((n (length *open-input-stream-cache*)))
                 (when (> n 3)
                   (flush-open-input-stream-cache (cdr (nth (random n) *open-input-stream-cache*)))))
               (push (cons pathname stream) *open-input-stream-cache*)
               stream)))))

(defun flush-open-input-stream-cache (&optional stream)
  (cond (stream
         (close stream)
         (setq *open-input-stream-cache* (delq (rassq stream *open-input-stream-cache*) *open-input-stream-cache*)))
        ('else
         (do ()
             ((not *open-input-stream-cache*))
           (close (cdr (pop *open-input-stream-cache*)))))))


(defun string-translate-lispm->unix (string)
  (do ((j 0 (1+ j))
       (n (length string)))
      ((= j n))
    (let ((c (aref string j)))
      (cond ((= c #\return)
             (aset 10 string j))
            ('else
             (aset (logand #o177 c) string j))))))


(defun (nfsproc_create rpc:service-procedure) (where attributes)
  attributes
  (let ((dir (xdr_structure_ref WHERE 'dir))
        (name (xdr_structure_ref WHERE 'name)))
    (let* ((fileid (fhandle->fileid dir))
           (pathname (fileid->pathname fileid)))
      (setq pathname (cond ((eq :root pathname)
                            (filesys-root-pathname *filesys*))
                           (pathname
                            (send pathname :pathname-as-directory))))
      (when rpc:*rpc-server-trace*
        (format t " ~S in directory ~A" name pathname))
      (cond ((null pathname)
             (list (xdr_make_descrimination 'nfserr_stale nil)))
            ('else
             (multiple-value-bind (name type version)
                 (parse-unix-name name)
               (or version (setq version :newest))
               (LET ((pfile (send pathname :new-pathname :name name :type type :Version version)))
                 (WITH-OPEN-FILE (STREAM PFILE :direction :OUTPUT :error nil)
                   (COND ((NOT (ERRORP STREAM))
                          (CLOSE STREAM) ;; must close
                          (let ((fattr (probe-stream->fattr (OPEN (SEND STREAM :TRUENAME) :DIRECTION NIL))))
                            (list (xdr_make_descrimination
                                    'nfs_ok
                                    (xdr_make_structure
                                      'diropres_ok
                                      'file (fileid->fhandle (xdr_structure_ref fattr 'fileid))
                                      'attributes fattr)))))
                         ((MEMQ 'FS:FILE-ALREADY-EXISTS (SEND STREAM :CONDITION-NAMES))
                          (list (xdr_make_descrimination 'nfserr_EXIST nil)))
                         ('ELSE
                          ;; DIRECTORY NOT FOUND OR SOMETHING.
                          (list (xdr_make_descrimination 'nfserr_stale nil))))))))))))


(defun (nfsproc_write rpc:service-procedure) (file beginoffset offset totalcount data)
  beginoffset
  totalcount
  (let* ((fileid (fhandle->fileid file))
         (pathname (fileid->pathname fileid))
         (index))
    (when rpc:*rpc-server-trace*
      (format t " ~A ~D ~D" pathname offset (length data)))
    (cond ((not pathname)
           (list (xdr_make_descrimination 'nfserr_stale nil)))
          ((eq pathname :root)
           (list (xdr_make_descrimination 'nfserr_isdir nil)))
          ('else
           ;; speed this up later with a cache of open streams.
           (with-open-file (stream pathname :direction :output :if-exists :append :error nil)
             (cond ((errorp stream)
                    (list (xdr_make_descrimination
                            (cond ((memq 'FS::INVALID-OPERATION-FOR-DIRECTORY (send stream :condition-names) )
                                   'nfserr_isdir)
                                  ('else 'nfserr_stale))
                            nil)))
                   ((> offset (setq index (send stream :read-pointer)))
                    ;; nfserr_io
                    ;; for now make this an error. although nfserr_io is too strong. only available
                    (list (xdr_make_descrimination 'nfserr_io nil)))
                   ('else
                    (or (= offset index) (send stream :set-pointer offset))
                    (send stream :string-out data))))))))
