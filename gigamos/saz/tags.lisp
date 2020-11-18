;;; -*- Mode:LISP; Package:ZWEI; Base:10 -*-
From saz@GSI-CAM Fri Jul  8 13:53:34 1988
Received: from GSI-BRAHMS (lmi-fowlfood.ARPA) by gsi-cam.UUCP (4.12/4.7)  id AA04111; Fri, 8 Jul 88 13:53:27 edt
Date: Friday, 8 July 1988, 13:53-EDT
From: <saz@GSI-CAM>
Subject: Tag Tables now selectable by pathnames (with wildcards)
To: info-lispm@GSI-CAM
Message-Id: <[GSI-BRAHMS].8-Jul-88 13:53:13.saz>


;;;Feature: Reads in all files currently without zmacs buffers in the
;;;background after accepting tag table name as unique.  Alerts when
;;;done.
;;;
;;;Feature: Allows specification of identical sets of files for multiple
;;;tag tables (as long as they are given different names).
;;;
;;;Caveat (not specific to this code): Tag table names are not case sensi-
;;;tive!
;;;
;;;Warning: Full defaulting is in effect here, so all versions of a file are used
;;;if you do not specify otherwise!
;;;

(defcom com-select-files-as-tag-table "Prompts for and selects files to use as a tag table,
  reading in any specified files currently without ZMACS buffers in the background." ()
  (let ((file-list
          (mapcar #'car                         ;just the pathnames
            (cdr (fs:directory-list             ;cdr to strip off first entry (which lists the directory's attributes)
                   (READ-DIRECTORY-NAME "File or files to use as tag table:" (DEFAULT-PATHNAME))))))
        (tag-table-name (typein-line-readline "Name for this tag table:"))
        (all-files-with-associated-buffers (mapcar #'(lambda (buffer) (car (send buffer :file-id)))
                                                   *zmacs-buffer-list*)))
    (SELECT-FILE-LIST-AS-TAG-TABLE
      file-list
      (do ((used-name tag-table-name (typein-line-readline
                             (string-append
                               "/"" used-name "/""
                               " is already the name of a tag table.  Please enter another name for this new one:"))))
          (nil)
        (if (not (ass #'string-equal used-name *zmacs-tag-table-alist*))
            (return used-name))))
    (process-run-function (format nil "Load the files of ~A into ZMACS" tag-table-name)
                          (lambda (files window)
                            (dolist (file files) (load-file-into-zmacs file nil))
                            (tv:notify window "The files of ~A have been loaded into Zmacs and selected as current tag table."
                                       tag-table-name))
                          file-list             ;arguments to the above function
                          *window*))
  DIS-NONE)
