Date: Friday, 20 June 1986, 13:47-EDT
From: Roger Frye <ref@LMI-ANGEL>
Subject: FTP-ACCESS :multiple-file-plists
To: bug-network@LMI-ANGEL
Message-ID: <[LMI-AZATHOTH].20-Jun-86 13:47:48.ref>

George,
I've implemented (:method ftp-access :multiple-file-plists).  It works.
Please read it, and make sure I've got it right.  Then I'll put it in
Release 3 and in 2.0 TCP.

----- aza:ref;ftp-extend.lisp -----
;;;-*- Mode:LISP; Package:FS; Base:10; Fonts:(CPTFONTB) -*-


(defmethod (ftp-access :multiple-file-plists) (files options &aux error)
  "Return list of lists of files and properties in the form
((file property-pair ...) ...)
There are no currently meaningful options." options
  (handling-file-errors (error)
    (loop for file in files
          collect (cadr (directory-list file)))))
----- EOF -----

----- Commands used to test ftp-extend -----

(load "angel://lmi//ref//Sys//load.l")
(make-system 'fake :reload :compile)
(main)
; Returns 5251

----- End commands ----
-Roger

