;;; -*- Mode: Lisp; Package: File-System; Base: 10.; Readtable: T -*-


;;; Hooks into the guts of the filesystem (functions could be in fsguts)
;;; to make a faster NFS server after SUN microsystems.
;;; 8-Feb-87 18:14:08 -GJC


;;; problems:
;;; We must have a unique 32-byte string called a FILE-HANDLE
;;; for each file. In a Unix implementation this could be
;;; the disk device,inode-number,inode-generation-number.
;;; Even if the full filename could fit into 32-bytes this would
;;; not be a sufficient handle because it wouldnt cover the case
;;; of a file being created, deleted, then created again.
;;; This is considered a different file.
;;; We must be able to very quickly go from a FILE-HANDLE to an internal
;;; file object.
;;; The SERVER implementation does not depend then on lispmachine pathname
;;; manipulation. These functions server to bypass the usual pathname, access,
;;; and open and stream mechanisms.

;;; File Attributes:
;;; Directoryp, Protection/Mode, User-ID, Group-ID, SIZE, BLOCKSIZE, BLOCKS, FILEID,
;;; Access-Time, Modify-Time, Creation-Time.
;;; (what is FILEID used for?, a directory list consists of a filenames and fileids).
;;; Access-Time is not supported in FSGUTS. Group and User ID's could be generated
;;; from author by require using other SUN facilities such as yellow pages.
;;; Protection, Symbolic links, etc could me implemented using file properties.

;;; Hooks. Data returned should be lispmachine standard as possible.
;;; Dates universal time etc.

;;; (NFS-MOUNT-POINTS), returns a list of <FILE>'s. It will be the responsibility of
;;;  the MOUNT server to get the file handles and unix-style names of these.
;;;
;;; (NFS-FILEHANDLE->FILE <handle>) returns NIL or a FILE.
;;; (NFS-FILE->FILEHANDLE <FILE>) => a handle.
;;;
;;; (NFS-FILE-DIRECTORY-P <FILE>)
;;; (NFS-FILE-AUTHOR <FILE>)
;;; (NFS-FILE-SIZE <FILE>)
;;; (NFS-FILE-CREATION-DATE <FILE>)
;;; (NFS-FILE-PROPERTY <FILE> <KEY>)
;;; (NFS-FILE-NAME <FILE>)
;;; (NFS-FILE-TYPE <FILE>)
;;; (NFS-FILE-VERSION <FILE>)
;;; (NFS-FILE-DIRECTORY <FILE>)
;;;
;;;
;;; (NFS-FILE-SYSTEM-STATUS <FILE>) => info on blocks free, available. like LISTF prints out.
;;;
;;; (NFS-FILE-DIRECTORY-LIST <FILE>) => list of files.
;;;  Calling on <FILE> not a directory is an error. The server code
;;;  would check NFS-FILE-DIRECTORY-P first in order to signal an informative error to the client.
;;; It will be the responsibility of the server to map names, such as foo.directory to foo.
;;;
;;; Note: there is no (NFS-FILE-LOOKUP <FILE> <name>) because the server can do this
;;;       just about as well by looking through the (NFS-FILE-DIRECTORY-LIST <file>).
;;;       In any case if there were such a lookup it would take <NAME> <TYPE> <VERSION>.
;;;
;;; (NFS-FILE-STRING-READ  <FILE> <STRING> <START> <END>)
;;; (NFS-FILE-STRING-WRITE <FILE> <STRING> <START> <END>)
;;;
;;;
;;; (NFS-FILE-CREATE <FILE> "NAME" "TYPE" "VERSION") => (<FILE> <ALREADY-EXISTS-P>)
;;; Adds a file to a directory <FILE>.
;;; (NFS-DIRECTORY-CREATE <FILE> "NAME") => (<FILE> <ALREADY-EXISTS-P>)
;;;  Creates a directory "NAME"
;;;
;;; (NFS-FILE-DELETE <FILE>)


;;; Extra data structures involved:
;;; Must have a database which we could keep in a file.
;;; "<DCSN><FCSN>"
;;; Directory creation sequence number.
;;; File Creation Sequence number.
;;; Our database allows us to map the DCSN of a directory very quickly
;;; after reboot without having to map over the whole file system.
;;; Once we have the directory we can list the files and linear search.
;;; We use a cache of DCSN for directories that gives the <FILE> for
;;; the directory and also a hash table for further lookup.
;;; The all important NFS-FILEHANDLE->FILE operation is then in most cases but
;;; two GETHASH operations on two relatively small tables.
;;; * all file deletion needs to invalidate this cache.
;;; * all directory creation/deletion must update the cache and write the database to disk.
;;; Regular file creation by other lispmachine functions do not need to do anything
;;; special. The NFS-DIRECTORY-LIST (when reading :DISK) or NFS-FILE->FILEHANDLE
;;; can make sure a number is assigned. The FCSN can then be a number unique
;;; in the directory, not globally. The current max FCSN can then be easily
;;; stored in the directory-header. Keeping the FCSN as part of a new entry
;;; spec rather than a property has advantages of space and keeping it hidden
;;; (and unmodifiable) by the user :SET-PROPERTIES.
;;; Keeping the DCSN database hidden means we cannot keep it as a regular file in the root
;;; directory (for example). Is it the kind of thing one would want to backup?
;;; Hmmm. Lets keep it in a special directory NFS-DCSN;
;;; And since directory creation is infrequent enough we can afford
;;; to write it as NFS-DCSN;DATABASE.LISP#>. With sufficient warning about
;;; not editing by hand.
;;;
;;; (CREATE-NFS-DCSN-DATABASE) will map over the entire file system to create
;;; this database for the first time. Really it is just a way to flatten the
;;; file system into, (1 "GJC") (2 "GJC" "FOO") (3 "BAR") to make directory lookup
;;; fast.
;;;
;;; READ/WRITE.
;;; Being able to use existing stream access, need to keep open files.
;;; No indication of when to close them, so need a background process to do
;;; this.
;;; READ could do a direct disk read. But, could be inefficient to read
;;; small chunks like that. Same with write.
;;; Have a cache of disk block numbers and data, with background process
;;; to flush?

;;; need to patch defstruct of FILE, create-new-directory, create-new-file, read-directory-entry
;;;


(DEFVAR *NFS-DCSN-DATABASE-FILE* "LM:NFS-DCSN;DATEBASE.QFASL#>")
(DEFVAR *NFS-DCSN-DATABASE* NIL)
(DEFVAR *NFS-DCSN-DATABASE-LOCK* NIL)

(DEFUN GIVE-FILE-GENERATION-NUMBER (FILE)
  (COND ((DIRECTORY? FILE)
         (WHEN (NOT (EQ T *NFS-DCSN-DATABASE*))
           (WITH-LOCK (*NFS-DCSN-DATABASE-LOCK*)
             (WHEN (NULL *NFS-DCSN-DATABASE*)
               (CONDITION-CASE ()
                   (OPEN *NFS-DCSN-DATABASE-FILE* :DIRECTION NIL)
                 (FILE-NOT-FOUND
                  (RETURN-FROM GIVE-FILE-GENERATION-NUMBER NIL))
                 (DIRECTORY-NOT-FOUND
                  (RETURN-FROM GIVE-FILE-GENERATION-NUMBER NIL)))))))
        ('ELSE
         (LET ((DIRECTORY (FILE-DIRECTORY FILE)))
           (REQUIRE-LOCK (DIRECTORY-LOCK DIRECTORY))
           (OR (FILE-ATTRIBUTE DIRECTORY :HEADER-BLOCK)
               (WRITE-DIRECTORY-FILES DIRECTORY))
           (LET ((H (GETF (FILE-PLIST DIRECTORY) '%HEADER-INTERNAL)))
             (SETF (FILE-GENERATION-NUMBER FILE) (SETF (DIRHEADER-FLAG-GENERATION-COUNT H)

(1+ (DIRHEADER-FLAG-GENERATION-COUNT H)))))))))
