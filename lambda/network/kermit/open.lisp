;;; -*- Mode:LISP; Package:KERMIT; Base:8; Readtable:ZL -*-


;;Copyright LISP Machine, Inc. 1984, 1985 ,1986
;;   See filename "Copyright" for
;;licensing and release information.


(declare (special interaction-pane *filnam* *filelist* *serial-stream* *terminal*))

;;;; G N X T F L
;moved here from file kermit-window; 6-21-84 --mhd

(DEFUN GNXTFL ()
  "Get next file in a file group.
   Set *FILNAM* to next file, and return rest of *FILELIST*."
  (AND *DEBUG* (DEBUGGER-TELL-USER ':GNXTFL *FILELIST*))
  (without-interrupts (setq *filnam* (car *filelist*))
                      (setq *filelist* (cdr *filelist*)))
  (cond ((consp *filnam*)
         (setq *as-filnam* (cadr *filnam*) *filnam* (car *filnam*))))
  *filnam*)






(defconst kermit-default-pathname :unbound)



(defun kermit-filelist (filename)
  (let ((pathname
          (fs:parse-pathname
            (fs:merge-pathname-defaults filename kermit-default-pathname))))
    ;; must be parsable pathname
    (cond
      ((eq (send pathname ':send-if-handles ':directory) ':unspecific)
       ;; some device or other random thing. just return what we got as a string.
       (list (string pathname)))
      (t
       ;; this is some other case; hopefully a string for the directory
       ;; such as "mhd", but who knows.  You know someone should straighten
       ;; the Lisp Machine file mess out some day....
       (loop for x in
             (fs:directory-list pathname)
             ; let user see error message; no files will be sent; reasonable for today.
             when (car x) collect (car x))))))


(defun string-for-kermit-infile (filename)
  (fs:merge-pathname-defaults filename kermit-default-pathname))


(defun string-for-kermit-outfile (filename)
  (fs:merge-pathname-defaults filename kermit-default-pathname))






(defun open-file-in-or-not (filename)
  (open filename ':in))

(defun open-file-out-or-not (filename)
  (open filename ':out))










(defvar *maxnamelength* 25)





(defvar *maxtypelength* 25)





;;; @@@ string-for-kermit

(defun string-for-kermit (filename &aux pathname dir name type version)
  "given a [lispm] pathname, GENERALLY returns /"name.type/"."
  (SETQ FILENAME (STRING FILENAME))
  (prog ()

        (setq pathname (fs:parse-pathname filename))

        (selectq *filnamcnv*
          (:generic
           (setq dir nil
                 name (maybe-handle-wildthing pathname ':name *filnamcnv*)
                 type (maybe-handle-wildthing pathname ':type *filnamcnv*)
                 version nil))
          (:raw (return filename))
          (otherwise
           (setq dir nil
                 name (maybe-handle-wildthing pathname ':name *filnamcnv*)
                 type (multiple-value-bind (thing winp)
                          (fs:decode-canonical-type (send pathname ':canonical-type) *filnamcnv*)
                        (if winp
                            thing
                          (maybe-handle-wildthing pathname ':type *filnamcnv*)))
                 version nil)))

        (return (string-append (if dir (string-append dir name) name)
                               "." (if version (string-append type version) type)))))

(defprop :vms 9. *maxnamelength*)
(defprop :vms 3. *maxtypelength*)

(defun (:vms ok-filename-char) (x)
  (or (<= #/a x #/z)
      (<= #/A x #/Z)
      (<= #/0 x #/9)
      (= #/* x)))

(defun maybe-handle-wildthing (pathname element system)
  (let ((s (cdr (assq element '((:name . *maxnamelength*)
                                (:type . *maxtypelength*))))))
    (let ((max-length (or (get system s) (symeval s))))
      (let ((e (send pathname element)))
        (if (eq e ':wild) (setq e "*"))
        (if (eq e ':unspecific) (setq e ""))
        (if (get system 'ok-filename-char)
            (setq e (with-output-to-string (y)
                      (do ((j 0 (1+ j)))
                          ((= j (string-length e)))
                        (if (funcall (get system 'ok-filename-char) (aref e j))
                            (send y ':tyo (aref e j)))))))
        (substring e 0 (min max-length (string-length e)))))))
