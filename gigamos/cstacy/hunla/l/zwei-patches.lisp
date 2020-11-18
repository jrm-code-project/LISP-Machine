;;;-*- Mode:LISP; Package:ZWEI; Base:10; Readtable:CL; Patch-file: T  -*-



;;;; Dired
;
; [11/15/88 15:46]
; Function DIRECTORY-EDIT: Don't make this a read-only buffer, because we want to be able
; to edit the presentation of the directory using commands such as C-K and M-X Keep Lines.
; Function COM-DIRED-GOTO-END: added.
; Function COM-DIRED-GOTO-BEGINNING: added.
; Definition Dired Mode: Remove K and c-K bindings.  Add m-> and m-<

(DEFUN DIRECTORY-EDIT (PATHNAME &OPTIONAL (SELECTP T) directory-list-options)
  "Create a ZMACS buffer editing the directory PATHNAME, and select it unless inhibited.
The buffer is selected unless SELECTP is NIL."
  (LET* ((DIRNAME (SEND PATHNAME :STRING-FOR-DIRECTORY))
         (INTERVAL
           ;; We do not use :FIND-SPECIAL-BUFFER because we can be called
           ;; while not inside ZMACS, and there may not even be a good way to
           ;; pick which ZMACS window to call.
           (MAKE-INSTANCE 'ZMACS-BUFFER :NAME (LOOP FOR I FROM 1
                                                    AS BUFNAM = (FORMAT NIL "*Dired-~A-~D*"
                                                                        DIRNAME
                                                                        I)
                                                  UNLESS (FIND-BUFFER-NAMED BUFNAM)
                                                    RETURN BUFNAM))))
    (if directory-list-options
        (putprop interval directory-list-options 'directory-list-options))
    (SETF (NODE-SPECIAL-TYPE INTERVAL) :DIRED)
    (SETF (BUFFER-SAVED-MAJOR-MODE INTERVAL) 'DIRED-MODE)
    (SEND INTERVAL :ACTIVATE)
    (IF SELECTP (SEND INTERVAL :SELECT))
    (SETF (GET INTERVAL 'PATHNAME-LIST) (LIST PATHNAME))
    (LET ((*INTERVAL* NIL))
      (DIRECTORY-EDIT-REVERT INTERVAL))
    (IF SELECTP
        (SETQ *DIRED-PATHNAME-NAME* (SEND (BUFFER-PATHNAME INTERVAL) :STRING-FOR-PRINTING)))
    DIS-TEXT))


(DEFMAJOR COM-DIRED-MODE DIRED-MODE "Dired" "Setup for editing a directory" ()
  (PROGN (LET ((PATHNAME (SEND *INTERVAL* :PATHNAME)))
           (SETQ *DIRED-PATHNAME-NAME* (AND PATHNAME (STRING PATHNAME)))))
  (SET-COMTAB *MODE-COMTAB* '(#\SP COM-DOWN-REAL-LINE
                              #\! COM-DIRED-NEXT-UNDUMPED
                              #\@ COM-DIRED-COMPLEMENT-DONT-DELETE
                              #\# COM-DIRED-COMPLEMENT-DONT-SUPERSEDE
                              #\$ COM-DIRED-COMPLEMENT-NO-REAP-FLAG
                              #\. COM-DIRED-CHANGE-FILE-PROPERTIES
                              #\, COM-DIRED-PRINT-FILE-ATTRIBUTES
                              #\= COM-DIRED-SRCCOM
                              #\ COM-DIRED-SRCCOM-FILE
                              #\? COM-DIRED-DOCUMENTATION
                              #\HELP COM-DIRED-DOCUMENTATION
                              #\A COM-DIRED-APPLY-FUNCTION              #\a (0 #\A)
                              #\C COM-DIRED-COPY                        #\c (0 #\C)
                              #\D COM-DIRED-DELETE                      #\d (0 #\D)
                              #\C-D COM-DIRED-DELETE
                              #\E COM-DIRED-EDIT-FILE                   #\e (0 #\E)
                              #\C-SH-E COM-DIRED-EDIT-FILE-TWO-WINDOWS
                              #\F COM-DIRED-FIND-FILE                   #\f (0 #\F)
                              #\H COM-DIRED-AUTOMATIC                   #\h (0 #\H)
                              #\L COM-DIRED-LOAD-FILE                   #\l (0 #\L)
                              #\N COM-DIRED-NEXT-HOG                    #\n (0 #\N)
                              #\P COM-DIRED-PRINT-FILE                  #\p (0 #\P)
                              #\Q COM-DIRED-EXIT                        #\q (0 #\Q)
                              #\R COM-DIRED-RENAME                      #\r (0 #\R)
                              #\S COM-DIRED-SUBDIRECTORY                #\s (0 #\S)
                              #\U COM-DIRED-UNDELETE                    #\u (0 #\U)
                              #\V COM-DIRED-VIEW-FILE                   #\v (0 #\V)
                              #\X COM-DIRED-EXECUTE                     #\x (0 #\X)
                              #\m-< COM-DIRED-GOTO-BEGINNING
                              #\m-> COM-DIRED-GOTO-END
                              #\< COM-DIRED-EDIT-SUPERIOR-DIRECTORY
                              #\> COM-DIRED-GO-TO-MOST-RECENT-VERSION
                              #\RUBOUT COM-DIRED-REVERSE-UNDELETE
                              #\ABORT COM-DIRED-ABORT
                              #\END COM-DIRED-EXIT
                              #\MOUSE-3-1 COM-DIRED-MOUSE-MENU
                              #\1 COM-NUMBERS
                              #\2 COM-NUMBERS
                              #\3 COM-NUMBERS
                              #\4 COM-NUMBERS
                              #\5 COM-NUMBERS
                              #\6 COM-NUMBERS
                              #\7 COM-NUMBERS
                              #\8 COM-NUMBERS
                              #\9 COM-NUMBERS
                              #\0 COM-NUMBERS)
              '(("Automatic" . COM-DIRED-AUTOMATIC)
                ("Automatic All Files" . COM-DIRED-AUTOMATIC-ALL)
                ("Sort Increasing Reference Date"
                 . COM-DIRED-SORT-BY-INCREASING-REFERENCE-DATE)
                ("Sort Decreasing Reference Date"
                 . COM-DIRED-SORT-BY-DECREASING-REFERENCE-DATE)
                ("Sort Increasing Creation Date"
                 . COM-DIRED-SORT-BY-INCREASING-CREATION-DATE)
                ("Sort Decreasing Creation Date"
                 . COM-DIRED-SORT-BY-DECREASING-CREATION-DATE)
                ("Sort Increasing File Name"
                 . COM-DIRED-SORT-BY-INCREASING-FILE-NAME)
                ("Sort Decreasing File Name"
                 . COM-DIRED-SORT-BY-DECREASING-FILE-NAME)
                ("Sort Increasing Size"
                 . COM-DIRED-SORT-BY-INCREASING-SIZE)
                ("Sort Decreasing Size"
                 . COM-DIRED-SORT-BY-DECREASING-SIZE)))
  (SET-MODE-LINE-LIST (APPEND (MODE-LINE-LIST) '("  " *DIRED-PATHNAME-NAME*
                                                 "     (Q to exit)"))))

(DEFCOM COM-DIRED-GOTO-END "Move to the last fileline." ()
  (MOVE-BP (POINT) (INTERVAL-LAST-BP *INTERVAL*))
  (DOWN-REAL-LINE -1)
  DIS-BPS)

(DEFCOM COM-DIRED-GOTO-BEGINNING "Move to the first file line." ()
  (MOVE-BP (POINT) (INTERVAL-FIRST-BP *INTERVAL*))
  (DOWN-REAL-LINE 2)
  DIS-BPS)




;;;; Modes

;;; ZWEI used to implement modes using the mode-name's plist, but I have
;;; all this code written using a Symbolics where ZWEI:MODE was a flavor.
;;; Sigh, let's kludge things up for this to work....

(defflavor mode (mode-symbol)
           ()
  :initable-instance-variables
  :gettable-instance-variables
  (:default-handler mode-roundabout-flavor-handler))

;;; Buffers have a MAJOR-MODE, which looks on the mode-name found in the special
;;; variable *MAJOR-MODE* to get the mode object.

(defflavor major-mode () (mode))

(defun major-mode (interval)
  (assert (eq interval *interval*))
  (get *major-mode* 'mode-object))

;;; Once you have the object, you can send it messages to find out
;;; the attributes of the mode.

(defun mode-roundabout-flavor-handler (message-name &rest ignore)
  (getf (symbol-plist (send self :mode-symbol)) message-name))

;;; Finally, instantiate all the modes that we are interested in.
;;; (ZWEI was already loaded, so hacking DEFINE-MODE-MACRO wont get us anything.)

(EVAL-WHEN (LOAD EVAL)

(defflavor fundamental-mode () (major-mode))
(setf (get 'fundamental-mode 'mode-object)
      (make-instance 'fundamental-mode :mode-symbol 'fundamental-mode))

(defflavor text-mode () (major-mode))
(setf (get 'text-mode 'mode-object)
      (make-instance 'text-mode :mode-symbol 'text-mode))

(defflavor lisp-mode () (major-mode))
(setf (get 'lisp-mode 'mode-object)
      (make-instance 'lisp-mode :mode-symbol 'lisp-mode))

(defflavor lisp-syntax-mixin () (mode))
(setf (get 'lisp-mode 'mode-object) (get 'lisp-mode 'mode-object))

(compile-flavor-methods
  mode major-mode
  fundamental-mode text-mode
  lisp-mode lisp-syntax-mixin)

);EVAL-WHEN
