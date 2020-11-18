;;; -*- Mode:LISP; Package:SDU; Base:10 -*-

; bobp
; unix-fs - read / write unix file system directly
;
; requires c-funcs.lisp
;
; does lots to figure out where the unix disk partitions are.
; files must already be allocated to be written; inodes aren't changed.
; works for sys5 too, since it doesn't modify inodes or the superblock.
;
; pathnames must have doubled '//'s.
;
; to use:
;     (unix-cat "//etc//passwd")
;     (unix-ls "//usr")
;
;     (setq buf (make-array nbytes :type :art-string))
;     (setq file (open-unix-file "//etc//passwd"))
;     (rw-file :read file buf nbytes)
;        returns number of bytes read (always > 0), or nil if starting at eof.
;     to seek:
;       (setf (file-seek file) new-offset)
;     to get size in 8-bit bytes:
;       (unix-file-size file)
;
; to-do:
;   normal "device" interface
;   better handling of disk rqbs

(defstruct (unix-file)
  "state per open unix file"
  (inode-number)
  (partition-offset)                            ;block offset for file system this file is on
  (inode-array (make-array 64. :type :art-8b))
  (file-seek 0)
  (disk-buffer (make-array 1024. :type :art-8b))        ;buffer for rw-file
  (indir-buffer (make-array 1024. :type :art-8b))       ;most recent, lowest level indir block
  (current-block-number nil)                    ;block number in disk-buffer if valid, else nil
  (current-indir-number nil))                   ;block number in indir-buffer if valid, else nil

(defun init-unix-file (&optional file)
  (if (null file)
      (setq file (make-unix-file)))
  (setf (partition-offset file) nil)
  (setf (file-seek file) 0)
  (setf (current-block-number file) nil)
  (setf (current-indir-number file) nil)
  file
  )

(defconst n-inodes-per-block 13.)
(defconst n-blocks-per-block (// 1024. 4))
(defconst unix-block-size 1024.)
(defconst unix-inode-size 64.)
(defconst unix-dir-size 16.)
(defconst disk-unit 0)

(defvar unix-rqb nil)

(defvar temp-buf nil)
(defvar temp-file nil)
(defvar temp-file-2 nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun unix-cat (name &optional (stream *standard-output*) &aux unix-rqb)
  (if (null temp-buf)
      (setq temp-buf (make-array 1024. :type :art-string)))
  (unwind-protect
      (progn
        (setq unix-rqb (si:get-disk-rqb)
              temp-file (init-unix-file temp-file))
        (do* ((file (open-unix-file name temp-file))
              nbytes)
             ((null (setq nbytes (rw-file :read file temp-buf 1024.))))
          (send stream :string-out (ascii-string temp-buf nil nbytes))))
    (si:return-disk-rqb unix-rqb)))

(defun unix-ls (name &aux unix-rqb)
  (if (null temp-buf)
      (setq temp-buf (make-array 1024. :type :art-string)))
  (setq temp-file (init-unix-file temp-file)
        temp-file-2 (init-unix-file temp-file-2))
  (unwind-protect
      (progn
        (setq unix-rqb (si:get-disk-rqb))
        (let ((file (open-unix-file name temp-file)))
          (cond ((not (inode-directory-type-p (inode-array file)))
                 (print-like-ls-l name (inode-array file)))
                (t
                 (do ()
                     ((not (eq unix-dir-size (rw-file :read file temp-buf unix-dir-size))))
                   (when (not (= 0 (dir-inode temp-buf)))
                     (open-inode (partition-offset file)
                                 (dir-inode temp-buf)
                                 temp-file-2)
                     (print-like-ls-l (dir-name temp-buf) (inode-array temp-file-2))))))))
    (si:return-disk-rqb unix-rqb)))

(defun print-like-ls-l (name inode)
  (format t "~&~4a ~4o ~3d ~4d ~10d  ~18a ~a" (di-type-string inode)
                                              (logand #o7777 (di-mode inode))
                                              (di-nlink inode)
                                              (di-uid inode)
                                              (di-size inode)
                                              (unix-time (di-mtime inode))
                                              name))

(defun open-unix-file (name &optional file)
  "open file by name and return unix-file as handle"
  (setq file (init-unix-file file))
  (multiple-value-bind (offs pathlist)
      (disk-partition-offset (parse-unix-pathname name))
    (if (null offs)
        (ferror nil "no partition for ~s" name))
    (setf (partition-offset file) offs)
    (get-inode-for-path file pathlist)
    (open-inode (partition-offset file)
                (inode-number file)
                file)))

(defun unix-file-size (file)
  (di-size (inode-array file)))

(defun get-inode-for-path (file pathlist)
  "find inode-number and read in inode-array for file pathlist"
  (open-inode (partition-offset file)
              2                                 ;start with "root" inode
              file)
  (do ((pl pathlist (cdr pl)))
      ((or (not (inode-directory-type-p (inode-array file)))
           (null pl)))
    (if (null (search-dir-for-name file (car pl)))
        (ferror nil "can't find file ~s" pathlist))
    (open-inode (partition-offset file)
                (inode-number file)
                file)))

(defvar dir-search-buf (make-array unix-dir-size :type :art-8b))

; file is already set up for reading
;
(defun search-dir-for-name (file name)
  "search file dir for name; set inode-number; return nil if not found"
  (do ((i 0 (+ i unix-dir-size)))
      ((>= i (di-size (inode-array file))))
    (if (null (rw-file :read file dir-search-buf unix-dir-size))
        (return nil))
    (let ((i-num (dir-inode dir-search-buf)))
      (if (and (not (= 0 i-num))
               (string-equal name (dir-name dir-search-buf)))
          (return (setf (inode-number file) i-num))))))

; set up to read an inode
(defun open-inode (part-offset i-num &optional file)
  "set up a file for reading i-num"
  (setq file (init-unix-file file))
  (setf (inode-number file) i-num)
  (setf (file-seek file) 0)
  (setf (partition-offset file) part-offset)
  (disk-read-bytes (inode-array file)
                   0
                   (+ (* 1024. (partition-offset file))
                      (inode-number-to-byte-offset (inode-number file)))
                   unix-inode-size)
  file)

; read or write a file as a byte stream
; disk-buffer caches last phys block
; indir-buffer caches most recent first-degree indir block
;
(defun rw-file (op file buf nbytes &optional (offs (file-seek file)))
  "read / write file; return nbytes transferred, or nil if starting at eof"
  (if (>= (file-seek file) (di-size (inode-array file)))
      nil
    (setq nbytes (min nbytes (- (di-size (inode-array file))
                                (file-seek file))))
    (do* ((total 0 (+ total xfer))
          (xfer (min nbytes (- 1024. (logand 1023. offs)))
                (min (- nbytes total) 1024.))
          bn)
         ((or (= total nbytes)
              (= 0 (setq bn (logical-to-physical-block-number file (// offs 1024.)))))
          total)
      (setq bn (+ bn (partition-offset file)))
      (when (not (eql bn (current-block-number file)))
        (disk-read-block (disk-buffer file) bn)
        (setf (current-block-number file) bn))
      (selectq op
        (:read
         (copy-array-portion (disk-buffer file)
                             (logand offs 1023.)
                             (+ (logand offs 1023.) xfer)
                             buf
                             total
                             (+ xfer total)))
        (:write
         (copy-array-portion buf
                             total
                             (+ xfer total)
                             (disk-buffer file)
                             (logand offs 1023.)
                             (+ (logand offs 1023.) xfer))
         (disk-write-block (disk-buffer file) bn)))
      (setf (file-seek file) (+ offs xfer))
      (setq offs (+ offs xfer)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; fs:parse-pathname ...
;    :directory returns:
;         :ROOT if name has just one component
;         a string for two components
;         a list of strings for more than two
;         :UP for //..//
;         :RELATIVE for ..//
;
(defun parse-unix-pathname (pathname)
  "parse pathname into list of component names"
  (let* ((path (fs:parse-pathname pathname "unix1"))    ;unix1 forces "unix mode"
         (return-list (list (fs:unix-filename (send path :name) (send path :type))))
         (dir (send path :directory)))
    (if (listp dir)
        (append dir return-list)
      (if (and dir (neq dir :root))
          (push dir return-list))
      (if (equal (car return-list) "")
          '(".")
        return-list))))

(defvar test-pathname '("//foo//bar//zot//bletch"
                        "//foo//bar//zot"
                        "//foo//bar"
                        "//foo"
                        "//"
                        ""))

(defun test-parse-pathname ()
  (do ((l test-pathname (cdr l)))
      ((null l))
    (format t "~&~26s: ~s" (car l) (parse-unix-pathname (car l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; low-level disk read stuff

; buf-array should be :art-8b
; can't cross block boundary
(defun disk-read-bytes (buf-array array-byte-offset disk-byte-offset size-in-bytes)
  "read disk relative to cyl 0"
  (if (null unix-rqb)
      (setq unix-rqb (si:get-disk-rqb)))
  (let ((block-number (// disk-byte-offset 1024.))
        (block-byte-offset (logand disk-byte-offset #x3ff)))
    (disk-read-hook unix-rqb disk-unit block-number)
    (copy-array-portion
      (si:rqb-8-bit-buffer unix-rqb)
      block-byte-offset
      (+ size-in-bytes block-byte-offset)
      buf-array
      array-byte-offset
      (+ size-in-bytes array-byte-offset))))

(defun disk-read-block (buf-array block-number)
  "read disk relative to cyl 0 into 8- or 16-b array"
  (if (null unix-rqb)
      (setq unix-rqb (si:get-disk-rqb)))
  (disk-read-hook unix-rqb disk-unit block-number)
  (copy-array-contents
    (if (eq (array-type buf-array) 'art-8b)
        (si:rqb-8-bit-buffer unix-rqb)
      (si:rqb-buffer unix-rqb))
    buf-array))

(defun disk-write-block (buf-array block-number)
  "write disk relative to cyl 0 into 8- or 16-b array"
  (if (null unix-rqb)
      (setq unix-rqb (si:get-disk-rqb)))
  (copy-array-contents
    buf-array
    (if (eq (array-type buf-array) 'art-8b)
        (si:rqb-8-bit-buffer unix-rqb)
      (si:rqb-buffer unix-rqb)))
  (disk-write-hook unix-rqb disk-unit block-number)
  )

(defconst max-unix-block-number (* 20. 25. 1000.))      ;heads * sectors * cylinders

(defun disk-read-hook (rqb unit block-number)
  "hook for si:disk-read-physical"
  (if (and (>= block-number 0)
           (< block-number max-unix-block-number))
      (si:disk-read-physical rqb unit block-number)
    (ferror nil "disk-read-hook called with ridiculous block-number ~d." block-number)))

(defun disk-write-hook (rqb unit block-number)
  "hook for si:disk-write-physical"
  (if (and (>= block-number 0)
           (< block-number max-unix-block-number))
      (si:disk-write-physical rqb unit block-number)
    (ferror nil "disk-write-hook called with ridiculous block-number ~d." block-number)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; unix fs indirect block stuff

; return the phys block number for a logical block number
; if n < 10
;    return (inode-array file) [n]
; n -= 10
; if n < 256
;    read indir block; return indir [n]
; n -= 256
; if n < 256 ^ 2
;    read second indir; 1st-indir = second-indir [n / 256]; return 1st-indir [n mod 256]
; n -= 256 ^ 2
; if n < 256 ^ 3
;    read third indir; 2nd-indir = third-indir [n / 256 / 256];
;        read 2nd-indir; 1st-indir = 2nd-indir [n / 256]; return 1st-indir [n mod 256]
; otherwise, its too big

(defun logical-to-physical-block-number (file logical-block-number)
  "do indir stuff to get block number rel to file-system; return 0 if block is not allocated"
  (cond ((< logical-block-number (- n-inodes-per-block 3))
         (di-addr (inode-array file) logical-block-number))     ;RETURN VALUE
        (t
         (decf logical-block-number 10)
         (cond ((< logical-block-number 256.)
                (disk-read-block (indir-buffer file)    ;read first-level indir block
                                 (+ (partition-offset file)
                                    (di-addr (inode-array file) (- n-inodes-per-block 3))))
                (di-indir-aref (indir-buffer file) logical-block-number))       ;RETURN VALUE
               (t
                (decf logical-block-number 256.)
                (cond ((< logical-block-number (* 256. 256.))
                       (disk-read-block (indir-buffer file)     ;read second-level indir block
                                        (+ (partition-offset file)
                                           (di-addr (inode-array file) (- n-inodes-per-block 2))))
                       (disk-read-block (indir-buffer file)     ;read first-level indir block
                                        (+ (partition-offset file)
                                           (di-indir-aref (indir-buffer file)
                                                          (// logical-block-number 256.))))
                       (di-indir-aref (indir-buffer file) (mod logical-block-number 256.)))
                      (t
                       (ferror nil "Attempt to read past second-level-indirect file block"))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; accessors for unix inode structure

(defun di-indir-aref (buf index)
  (get-68k-nbytes buf (* index 4) 4))

(defun di-mode (inode-array)
  (get-68k-nbytes inode-array 0 2))

(defun di-nlink (inode-array)
  (get-68k-nbytes inode-array 2 2))

(defun di-uid (inode-array)
  (get-68k-nbytes inode-array 4 2))

(defun di-gid (inode-array)
  (get-68k-nbytes inode-array 6 2))

(defun di-size (inode-array)
  (get-68k-nbytes inode-array 8 4))

(defun di-addr (inode-array l3-block-number)
  (get-68k-nbytes inode-array (+ 12. (* 3 l3-block-number)) 3))

(defun di-atime (inode-array)
  (get-68k-nbytes inode-array 52. 4))

(defun di-mtime (inode-array)
  (get-68k-nbytes inode-array 56. 4))

(defun di-ctime (inode-array)
  (get-68k-nbytes inode-array 60. 4))

(defun inode-directory-type-p (inode-array)
  "return t if inode-array is for a directory"
  (= #o40000 (logand (di-mode inode-array) #o170000)))

#|
#define IFMT    0170000         /* type of file */
#define         IFDIR   0040000 /* directory */
#define         IFCHR   0020000 /* character special */
#define         IFBLK   0060000 /* block special */
#define         IFREG   0100000 /* regular */
#define         IFMPC   0030000 /* multiplexed char special */
#define         IFMPB   0070000 /* multiplexed block special */
#define ISUID   04000           /* set user id on execution */
#define ISGID   02000           /* set group id on execution */
#define ISVTX   01000           /* save swapped text even after use */
#define IREAD   0400            /* read, write, execute permissions */
#define IWRITE  0200
#define IEXEC   0100
|#

(defun di-type-string (inode-array)
  "return string for file type"
  (let ((mode (ldb (byte 4 12.) (di-mode inode-array))))
    (selectq mode
      (#o2 "CHAR")
      (#o3 "MPC")
      (#o4 "DIR")
      (#o6 "BLK")
      (#o7 "MPB")
      (#o10 "FILE")
      (t "???"))))

(defun inode-number-to-byte-offset (inode-number)
  (+ (* unix-inode-size (1- inode-number))
     (* 2 unix-block-size)))

(defun print-inode (inode-array)
  (format t "~&mode    0~o" (di-mode inode-array))
  (format t "~&nlink   ~d." (di-nlink inode-array))
  (format t "~&uid     ~d." (di-uid inode-array))
  (format t "~&gid     ~d." (di-gid inode-array))
  (format t "~&size    ~d." (di-size inode-array))
  (format t "~&addr    ")
  (dotimes (i 13)
    (format t "~d. " (di-addr inode-array i)))
  (format t "~&atime   ~a" (unix-time (di-atime inode-array)))
  (format t "~&mtime   ~a" (unix-time (di-mtime inode-array)))
  (format t "~&ctime   ~a" (unix-time (di-ctime inode-array)))
  inode-array)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; accessors for unix directory structure

(defun dir-inode (dir-entry)
  (get-68k-nbytes dir-entry 0 2))

(defun dir-name (dir-entry)
  (c-str-copy dir-entry 2 14))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; unix and sdu names for standard (first eight) disk partitions
; index is minor device number
(defvar unix-file-system-names '("uroot" nil nil nil "usr" "lmi" "sdu" "src"))

; unix disk partition offsets for hard-wired root-image systems
; index is minor device number
(defvar disk-offsets '(1023. nil nil nil 10000. nil 23. nil))

; for a pathname (pathlist), figure out what partition it is in
; and where the partition is located.
; return the starting block number of the partition, and the
; rest of the path after the partition specifier.
(defun disk-partition-offset (pathlist)
  "return disk partition starting block and rest of path for unix file name"
  (let* ((part (car pathlist))
         (minor (position part unix-file-system-names :test 'string-equal)))
    (cond (minor                                ;//sdu, //lmi, //uroot
           (setq part (format nil "UNX~d" minor))
           (pop pathlist))
          ((string-equal part "disk")           ;//disk//# or //disk
           (pop pathlist)
           (setq minor (parse-integer (car pathlist) :junk-allowed t))
           (if minor
               (pop pathlist)                   ;//disk//#
             (setq minor 6))                    ;//disk w/o # defaults to /sdu
           (setq part (format nil "UNX~d" minor))))
    (let ((block-number (si:find-disk-partition part)))
      (cond (block-number                       ;there is such a partition.
             (if (equal (car pathlist) part)    ;pop it from pathlist if we haven't already.
                 (pop pathlist))
             (setq block-number (+ (get-label-block) block-number)))
            (t                                  ;no such partition
             (if (and (string-equal part "UNX" :end1 3) ;UNX# root-image offset from table
                      (setq minor (parse-integer (substring part 3))))
                 (setq block-number (nth minor disk-offsets))
               (if (setq block-number (si:find-disk-partition "UNX0"))  ;random ==> unix root
                   (setq block-number (+ (get-label-block)      ;from label
                                         block-number))
                 (setq block-number (car disk-offsets))))))     ;root-image unix root offset
      (values block-number pathlist))))

; disk-partition-offset should work for all of these
(defconst test-dpo-list '("//"
                          "//etc"
                          "//etc//rc"
                          "//sdu"
                          "//sdu//lambda"
                          "//unx0"
                          "//unx0//etc"
                          "//unx5"
                          "//s205"
                          "//s205//share"
                          "//disk"
                          "//disk//monitor"
                          "//disk//6"
                          "//disk//6//monitor"
                          "//disk//0"
                          "//disk//0//etc"))

(defun test-dpo ()
  (mapcar '(lambda (x)
             (format t "~&~24a" x)
             (multiple-value-bind (offs path)
                 (disk-partition-offset (parse-unix-pathname x))
               (format t "~a ~a" offs path)))
          test-dpo-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Accessors for block-10 mini-label

(defconst mini-label-qs '(
  ml-magic
  ml-size-in-bytes
  ml-label-block-number
  ml-backup-label-block-number
  ml-bad-track-number
  ml-spare-1
  ml-number-of-ok-tracks
  ml-disk-type
  ml-heads
  ml-sectors
  ml-cyls
  ml-gap1
  ml-gap2
  ml-interleave
  ml-skew
  ml-sector-size
  ml-default-bad-track-numer
  ml-default-backup-label-track-number
  ))
(assign-values mini-label-qs)
(si:define-accessors-for-structure mini-label-qs)

(defvar block-10-mini-label (make-array 1024.
                                        :type :art-16b
                                        :leader-length 3
                                        :named-structure-symbol 'mini-label))

(defselect ((mini-label named-structure-invoke))
  (:describe (struct)
    (format t "~&~S:" struct)
    (dolist (q mini-label-qs)
      (format t "~&~s:~40t~s~52t~:*~16r" q (funcall q struct)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar label-block-number nil)

(defun get-label-block ()
  (if label-block-number
      label-block-number
    (setq label-block-number (get-label-block-from-mini))))

(defun get-label-block-from-mini ()
  (disk-read-block block-10-mini-label 10.)
  (let ((mini (ml-magic block-10-mini-label)))
    (if (not (= mini (string-constant "MINI")))
        nil) ;(ferror nil "block-10 mini-label is not set up; need 2.1+ SDU software"))
    (ml-label-block-number block-10-mini-label)))
