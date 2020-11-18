;;; -*- Mode:LISP; Package:ZWEI; Base:10; Readtable:CL -*-

;;;MFHOST2

;;;Like MFHOST, this file is for system-dependent ZMail extensions
;;;and support code.  Unlike MFHOST, this file is in CommonLISP, the
;;;syntax of "real" LISP programmers.

(defun make-unix-zmail-init-file (username unix-host login-dir &aux realhost)
  "This function makes mail initialization files for a ZMail user
 on a LISP machine who also uses a Unix mail server host.
\
For example, I am user GJC, my LISP home directory is \"LAM3:GJC;\", and I
 want to get my mail from the Unix host ANGEL; so I would execute:
\
  (make-unix-zmail-init-file 'gjc 'angel \"lam3:gjc;\")
\
This creates two files, one in the LISP home directory called 'ZMAIL.INIT',
 and one in the Unix home directory called '<user>.bb'.
\
Returns two values, the pathnames of the two ZMail initialization files."
  ;;
  (declare(values zmail-pathname mailfile-pathname))
  ;;
  (setq realhost (si:parse-host unix-host))
  (unless (eq (send realhost :system-type) :unix)
    (cerror "Proceed anyway, hoping for the best"
            "The host ~s is not type :UNIX - it is a ~s system"
            unix-host (send realhost :system-type)))
  (multiple-value-bind (real-username fullname home-directory-string error-msg)
      (lookup-user-etc-passwd username unix-host)
    (unless (and real-username fullname home-directory-string)
      (warn "Unable to make your Unix-ZMail-INIT-file~@[: ~a~]." error-msg)
      (return-from make-unix-zmail-init-file nil))
    (let* ((realhome (fs:parse-pathname (string-append home-directory-string "/")
                                        realhost))
           (mailfile-pathname (fs:merge-pathnames "mail.bb" realhome))
           (zmail-pathname (send (fs:parse-pathname login-dir)
                                 :new-pathname
                                 :name "ZMAIL"
                                 :type "INIT"
                                 :version :highest))
           (spool-dir (fs:parse-pathname "/usr/spool/mail/" realhost))
           (spool-file (fs:merge-pathnames
                         (fs:parse-pathname real-username realhost)
                         spool-dir)))
      (declare(ignore unix-p))
      ;;So long as there isn't one already...
      (and (or (null (probe-file zmail-pathname))
               ;;or the caller says it's ok...
               (yes-or-no-p "~%The ZMail initialization file ~s already exists...~
                             ~&  is it OK to overwrite it?"
                            zmail-pathname))
           ;;Write out ZMail .init file
           (with-open-file (stream zmail-pathname :direction :output)
             (format t "~%Writing out ~s..." zmail-pathname)
             (let ((*readtable* (si:find-readtable-named "Common-Lisp"))
                   (*package* (find-package "ZWEI"))
                   (*print-base* 10.)
                   (*read-base*  10.))
               (format stream ";;-*-Mode:LISP;Package:~A;Base:~10r;ReadTable:~A-*-~%"
                       (package-name *package*)
                       *read-base*
                       (si:rdtbl-short-name si:common-lisp-readtable))
               (format stream "~&~
                      ~%(login-setq *zmail-startup-file-name* ~S)~
                      ~2%(login-setq *from-user-id* ~S)~
                      ~2%(login-setq *from-host* (si:parse-host ~S))~
                      ~2%(login-setq fs:user-personal-name-first-name-first ~S)~
                      ~2%(login-setq zwei:*zmail-usual-mail-file-directory* ~S)~
                      ~2%(login-setq zwei:*zmail-homedir-real-new-mail-filename* ~S)~2%"
                       (send mailfile-pathname :string-for-printing)
                       real-username
                       (send realhost :name)
                       fullname
                       (send realhome :string-for-printing)
                       (send spool-file :string-for-printing)))))
      ;;So long as there isn't one already...
      (and (or (null (probe-file mailfile-pathname))
               ;;or the caller says it's ok...
               (yes-or-no-p "~%The mail file ~s already exists...~
                             ~&  is it OK to overwrite it?"
                            mailfile-pathname))
           ;;write Babyl file in user's Unix home directory.
           (with-open-file (stream mailfile-pathname :direction :output)
             (format t "~%Writing out ~s..." mailfile-pathname)
             (format stream "Babyl Options:~
                        ~%Append:1~
                        ~%Version:5~
                        ~%Mail: ~A~
                        ~%Owner:~A~
                        ~%Summary-window-format: T~
                        ~%"
                     (send spool-file :string-for-printing)
                     real-username)))
      ;;Return the pathnames we wrote out
      (values zmail-pathname mailfile-pathname))))

(defun lookup-user-etc-passwd (username host &optional (passwd-file "/etc/passwd"))
  "Look up password file entry for USERNAME on Unix HOST.
Returns 4 values:
  UNAME    - the actual username found.
  FULLNAME - the user's fullname entry.
  DIR      - the user's home directory entry.
  ERROR-STRING, if non-NIL, is a string indicating the lookup error."
  (declare(values uname fullname dir error-string))
  (ctypecase username
    (string)
    (symbol (setq username (string username))))
  (multiple-value-bind(uname fullname dir error-string)
      (with-open-file (stream (fs:parse-pathname passwd-file host))
        (do ((st))
            ((cond
               ((null (setq st (read-line stream nil)))
                (format t "~&End-of-file in Unix password file.")
                (return (values nil nil nil
                                (format nil  "Entry for ~s not found" username))))
               ((not (string-equal username
                                   (substring st 0 (string-search ":" st))))
                nil)
               ((y-or-n-p "~&Is this your Unix username entry? -->'~A'~% ...?" st)
                (return (parse-user-etc-passwd st)))))))
    (values uname fullname dir error-string)))

(defun parse-user-etc-passwd (st)
  ;; username:password:uic:gid:Full Name:directory:shell
  (let ((n (string-search ":" st))
        (uname)(fullname)(dir))
    (setq uname (substring st 0 n))
    (setq n (string-search ":" st (1+ n)))      ; pass
    (setq n (string-search ":" st (1+ n)))      ; uid
    (setq n (string-search ":" st (1+ n)))      ; gid
    (setq fullname (substring st (1+ n)
                              (setq n (string-search ":" st (1+ n)))))
    (if (string-search "," fullname)
        (setq fullname (substring fullname 0 (string-search "," fullname))))
    (setq dir (substring st (1+ n) (string-search ":" st (1+ n))))
    (values uname fullname dir)))
