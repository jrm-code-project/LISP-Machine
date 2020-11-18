;;; -*- Mode:LISP; Package:USER; Readtable:ZL; Base:10 -*-


;;; TAR Tape Archiver, after the Unix(TM) command of the same name.
;;; use READ-UID-ALIST first if you want correct file author (UID) handling.
;;; or setq *UID-ALIST*

;;; Copyright LISP Machine, Inc. 1986
;;;    See filename "Copyright.Text" for
;;; licensing and release information.
;;;
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
;;;
;;; There have been various implementations of this handy utility,
;;; but this aught to do it for good. 19-Dec-86 17:51:16 -gjc

;;; set this to NIL if you are going to be dealing with binary files.

(defvar *lispm-character-translate* t)


(defvar *tblock* 512)
(defvar *namsiz* 100)

(defvar *hblock* (let ((i 0))
                   (mapcar #'(lambda (b)
                               (append b (list i (incf i (cadr b)))))
                           `((name ,*namsiz* asciz-string)
                             (mode 8 octal-int)
                             (uid 8 octal-int)
                             (gid 8 octal-int)
                             (size 12 octal-int)
                             (mtime 12 octal-int)
                             (chksum 8 octal-int-checksum)
                             (linkflag 1 a-character)
                             (linkname ,*namsiz* asciz-string)))))

(defun parse-hblock (string)
  (cond ((zerop (aref string 0))
         ())
        ('else
         (let ((e (assq 'chksum *hblock*)))
           (funcall (nth 2 e) string (nth 3 e) (nth 4 e)))
         (let ((alist nil))
           (dolist (e *hblock*)
             (unless (eq (car e) 'chksum)
               (push (list (car e) (funcall (nth 2 e) string (nth 3 e) (nth 4 e)))
                     alist)))
           alist))))

(defun string-in (stream st)
  (values st
          (send stream :string-in nil st)))


(defun octal-int-checksum (string start end)
  (let ((chksum (octal-int string start end)))
    (copy-array-portion "        " 0 8
                        string start end)
    (do ((sum 0 (+ (aref string j) sum))
         (j 0 (1+ j))
         (n (length string)))
        ((= j n)
         (or (= sum chksum)
             (ferror nil "directory checksum error"))
         sum))))


(defun asciz-string (st start end)
  (substring st start
             (or (string-search-char 0 st start end) end)))

(defun octal-int (st start end)
  (parse-integer st
                 :radix 8
                 :start start
                 :end (or (string-search-char 0 st start end) end)))

(defun a-character (st start end)
  end
  (aref st start))


(defvar *verbose* nil)
(defvar *filter* nil)


(defun tar (&optional &key table verbose create (extract (and (not create) (not table))) file (blocksize 20)
            (character-translate t) no-rewind)
  "Like the unix(tm) tar command"
  (if (and extract create) (ferror nil "cant both extract and create at same time"))
  (let ((*verbose* (or table verbose))
        (*filter* extract)
        (*lispm-character-translate* character-translate))
    (cond ((and file (not (mem #'string-equal file '("MT" "MT:"))))
           (cond (create
                  (with-open-file (stream file :direction :output)
                    (tar-create stream create)))
                 ('else
                  (with-open-file (stream file)
                    (tar-extract stream)))))
          ('else
           (or no-rewind (fs:mt-rewind))
           (let (s)
                 (cond (create
                        (unwind-protect
                            (tar-create (setq s (fs:make-mt-stream :direction :output
                                                                   :record-size (* blocksize *tblock*)))
                                    create)
                          (and s (send s :close)))
                        (fs:mt-write-eof))
                       ('else
                        (unwind-protect
                            (tar-extract (setq s (fs:make-mt-stream :record-size (* blocksize *tblock*))))
                          ;; use close abort so that the driver wont seek to the EOF marker.
                          (and s (send s :close t))))))))))

(DEFVAR *UID-ALIST* NIL)

(defun tar-extract (stream)
  (let ((tbuf (make-string *tblock*)))
    (do ((dir)(file))
        ((not (setq dir (parse-hblock (string-in stream tbuf)))))
    (let ((name (cadr (assq 'name dir)))
          (size (cadr (assq 'size dir)))
          (linkp (cadr (assq 'linkflag dir))))
      (when *verbose*
        (case linkp
          (#\1
           (format t "~&~A linked to ~A~%" name (cadr (assq 'linkname dir))))
          (#\2
           (format t "~&~A symbolic link to ~A~%" name (cadr (assq 'linkname dir))))
          (t
           (format t "~&~A ~D bytes~%" name size)))
        (when (eq *verbose* :verbose)
          (dolist (d dir)
            (format t "~8A = ~S~%"
                    (car d)
                    (cond ((eq (car d) 'mtime)
                           (time:print-universal-time (unix-time->universal-time (cadr d)) nil))
                          ((and (eq (car d) 'uid)
                                (cadr (assq (cadr d) *uid-alist*))))
                          ('else
                           (cadr d)))))))
      (cond ((or (and (not (zerop (length name)))
                      (= #// (aref name (1- (length name)))))
                 (member linkp '(#\1 #\2))
                 (not (setq file (filter-filename name))))
             (cond ((= #\1 linkp))
                   ('else
                    (multiple-value-bind (nblocks left)
                        (floor size *tblock*)
                      (dotimes (j nblocks)
                        (string-in stream tbuf))
                      (or (zerop left)
                          (string-in stream tbuf))))))
            ('else
             (with-open-file (ostream file :direction :output)
               (SEND OSTREAM :CHANGE-PROPERTIES NIL
                     :CREATION-DATE (unix-time->universal-time (cadr (assq 'mtime dir)))
                     :AUTHOR (or (cadr (assoc (cadr (assq 'uid dir)) *uid-alist*))
                                 si:user-id))
               (multiple-value-bind (nblocks left)
                   (floor size *tblock*)
                 (dotimes (j nblocks)
                   (string-in stream tbuf)
                   (unix->lispm-translate tbuf)
                   (send ostream :string-out tbuf))
                 (unless (zerop left)
                   (string-in stream tbuf)
                   (unix->lispm-translate tbuf)
                   (send ostream :string-out tbuf 0 left))))))))))


(defvar dummy-unix-host (si:make-dummy-host :unix))

(defun filter-filename (name)
  (cond ((not *filter*)
         nil)
        ('else
         (let ((pathname (fs:parse-pathname (string-append "//" name) dummy-unix-host)))
           (cond ((eq *filter* t)
                  (send pathname :new-pathname :host si:local-host))
                 ((or (stringp *filter*) (typep *filter* 'si:host))
                  (send pathname :new-pathname :host *filter*))
                 ('else
                  (funcall *filter* pathname name)))))))


(defun tmp-query-filter (pathname name)
  name
  (let ((p (send pathname :new-pathname :host si:local-host :directory "TMP")))
    (when (y-or-n-p "~&Restore ~A ?" pathname)
      p)))

(defun unix->lispm-translate (s)
  (when *lispm-character-translate*
    (macrolet ((utrans (&rest l)
                       `(cond ,@(mapcan #'(lambda (c)
                                            (let (cfrom cto)
                                              (cond ((atom c)
                                                     (setq cfrom (- c #o200))
                                                     (setq cto c))
                                                    ('else
                                                     (setq cfrom (- (car c) #o200))
                                                     (setq cto (cadr c))))
                                              (list `((= c ,cfrom)
                                                      (setf (aref s j) ,cto))
                                                    `((= c ,cto)
                                                      (setf (aref s j) ,cfrom)))))
                                        l))))
      (dotimes (j (length s))
        (let ((c (aref s j)))
          (utrans (#\line #\return) #\tab #\form))))))


(defvar *unix-base-ut* (time:parse-universal-time "Midnight Jan 1,1970"))

(defun unix-time->universal-time (x)
  (+ x *unix-base-ut*))

(defun universal-time->unix-time (x)
  (- x *unix-base-ut*))


(defun read-uid-alist (from-host &optional (from-file "//etc//passwd"))
  ;; username:password:uid:gid:fullname:dir:shell
  (with-open-file (stream (fs:parse-pathname from-file from-host))
    (do ((line)
         (alist))
        ((null (setq line (readline stream nil)))
         (setq *uid-alist* (nreverse alist)))
      (cond ((zerop (length line)))
            ('else
             (let ((i (string-search-char #\: line))
                   (name)(uid))
               (setq name (substring line 0 i))
               (setq i (1+ (string-search-char #\: line (1+ i))))
               (setq uid (parse-integer line
                                        :start i
                                        :end (string-search-char #\: line i)))
               (when (and (not (= #\D (aref line 0)))
                          (not (assq uid alist)))
                 (push (list uid name ) alist))))))))

(defvar *filter-types* '("QFASL" "BIN" "XFASL" "FASL" "OFASL"))


(defun tar-create-getfiles (spec)
  (if (atom spec)
      (labels ((rec (x)
                    (do ((l (fs:directory-list x) (cdr l))
                         (r)(d))
                        ((null l)
                         (nreconc r (mapcan #'rec (nreverse d))))
                      (cond ((not (caar l)))
                            ((get (car l) :directory)
                             (push (send (send (caar l) :pathname-as-directory)
                                         :new-pathname
                                         :name (send x :name)
                                         :type (send x :type)
                                         :version (send x :version))
                                   d))
                            ((mem #'string-equal (send (caar l) :type) *filter-types*))
                            ('else
                             (push (caar l) r))))))
        (let ((p (fs:parse-pathname spec)))
          (rec (send p :new-pathname
                     :name (or (send p :name) :wild)
                     :type (or (send p :type) :wild)
                     :version :newest))))
    (apply #'append (mapcar #'tar-create-getfiles spec))))

(defun tar-create (stream spec)
  (let ((tbuf (make-string *tblock*))
        (dirs-made nil))
    (dolist (f (tar-create-getfiles spec))
      (with-open-file (istream f)
        (let* ((truename (send istream :truename))
               (dummy-name (send truename :new-pathname :host dummy-unix-host))
               (nsize (or (get istream :length) (get istream :length-in-bytes))))
          (when *verbose*
            (format t "~&~A ~D bytes~%" truename nsize))
          (let ((uid (format nil "~O"
                             (or (car (rass #'(lambda (i x) (string-equal i (car x)))
                                            (get istream :author)
                                            *uid-alist*))
                                 0)))
                (mtime (format nil "~O" (universal-time->unix-time (get istream :creation-date)))))
            (unless (mem #'equalp (send truename :directory) dirs-made)
              (push (send truename :directory) dirs-made)
              (encode-hblock tbuf
                             (send dummy-name :directory-string)
                             uid
                             "0"
                             mtime)
              (send stream :string-out tbuf))
            (encode-hblock tbuf
                           (send dummy-name :string-for-host)
                           uid
                           (format nil "~O" nsize)
                           mtime)
            (send stream :string-out tbuf)
            (multiple-value-bind (nblocks left)
                (floor nsize *tblock*)
              (dotimes (j nblocks)
                (string-in istream tbuf)
                (unix->lispm-translate tbuf)
                (send stream :string-out tbuf))
              (unless (zerop left)
                (fill tbuf 0)
                (send istream :string-in nil tbuf 0 left)
                (unix->lispm-translate tbuf)
                (send stream :string-out tbuf)))))))
    (fill tbuf 0)
    (send stream :string-out tbuf)))

(defvar *null-string-1* (make-string 1))
(setf (aref *null-string-1* 0) #\0)


(defun encode-hblock (string name uid size mtime)
  (fill string 0)
  (encode-hblock-item string 'name (if (and (not (zerop (length name)))
                                            (= (aref name 0) #//))
                                       (substring name 1)
                                     name))
  (encode-hblock-item string 'mode (if (and (not (zerop (length name)))
                                            (= (aref name (1- (length name))) #//))
                                       "777"
                                     "666"))
  (encode-hblock-item string 'uid uid)
  (encode-hblock-item string 'gid "0")
  (encode-hblock-item string 'size size)
  (encode-hblock-item string 'mtime mtime)
  (encode-hblock-item string 'chksum "        ")
  (encode-hblock-item string 'linkflag *null-string-1*)
  (encode-hblock-item string 'linkname "")
  (do ((sum 0 (+ sum (aref string j)))
       (j 0 (1+ j))
       (n (length string)))
      ((= j n)
       (encode-hblock-item string 'chksum (format nil "~O" sum)))))


(defun encode-hblock-item (into-string name from-string)
  (let ((c (assq name *hblock*)))
    (let ((start (nth 3 c))
          (end (nth 4 c))
          (n (length from-string)))
      ;; if from-string is too short then the rest is taken as 0
      ;; which is correct for unix.
      (copy-array-portion from-string 0 n
                          into-string start end))))
