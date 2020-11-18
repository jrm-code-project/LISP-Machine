;;; -*- Mode:LISP; Package:ZWEI; Base:10; Readtable:CL -*-
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **
;;; This is the last file loaded in the editor, it should contain only flavors
;;; and initializations

;;; Compile the necessary flavors

(COMPILE-FLAVOR-METHODS INTERVAL NODE SECTION-NODE ZMACS-BUFFER
                        INTERVAL-WITH-SORT-KEY INTERVAL-WITH-SORT-INTERVAL)

(COMPILE-FLAVOR-METHODS EDITOR-TYPEOUT-WINDOW
                        ECHO-AREA-WINDOW TYPEIN-WINDOW
                        ZWEI-WITHOUT-TYPEOUT ZWEI-MINI-BUFFER
                        ZWEI-WINDOW ZWEI-WINDOW-PANE ZMACS-WINDOW ZMACS-WINDOW-PANE
                        MODE-LINE-WINDOW POP-UP-MODE-LINE-WINDOW
                        ZWEI-OVERLYING-WINDOW
                        ZWEI-FRAME ZMACS-FRAME MENU-COMMAND-MOMENTARY-MENU
                        STANDALONE-EDITOR-WINDOW
                        STANDALONE-EDITOR-FRAME POP-UP-STANDALONE-EDITOR-FRAME
;                       STANDALONE-MAIL-OR-DIRED-FRAME
                        TEMPORARY-MODE-LINE-WINDOW TEMPORARY-MODE-LINE-WINDOW-WITH-BORDERS
                        EDITOR-STREAM-WINDOW EDITOR-TOP-LEVEL
                        EDITOR-STREAM-FROM-WINDOW STREAM-IBEAM-BLINKER
                        ZTOP-STREAM-FROM-WINDOW
                        BACKGROUND-TYPEOUT-WINDOW)

;;; This is a little random, but I suppose that Tag Tables in general should be instances, so
;;; that we can define interesting ways of iterating over source files.
(compile-flavor-methods tag-table-dummy-file)

(DEFVAR *EDITOR-INITIALIZATION-LIST*)

;;;Initialize the editor the first time we are loaded up
(DEFUN INITIALIZE-ALL-OF-EDITOR (&AUX (PACKAGE PACKAGE))
  (LET ((ELEM (cl:assoc "INITIALIZE-ZMACS" *EDITOR-INITIALIZATION-LIST* :test #'equalp)))
    (AND ELEM (SETQ *EDITOR-INITIALIZATION-LIST*
                    (CONS ELEM (DELQ ELEM *EDITOR-INITIALIZATION-LIST*)))))
  (TV:WITHOUT-SCREEN-MANAGEMENT
    (INITIALIZATIONS '*EDITOR-INITIALIZATION-LIST*))
  ;; Make one of each
  (USING-RESOURCE (WINDOW TEMPORARY-MODE-LINE-WINDOW-WITH-BORDERS-RESOURCE))
  (USING-RESOURCE (WINDOW BACKGROUND-TYPEOUT-WINDOWS))
; (USING-RESOURCE (FOO STANDALONE-MAIL-OR-DIRED-FRAME) FOO)
  (USING-RESOURCE (FOO POP-UP-STANDALONE-EDITOR-FRAME) FOO)
  )

(ADD-INITIALIZATION "INITIALIZE-ALL-OF-EDITOR"
                    '(INITIALIZE-ALL-OF-EDITOR)
                    '(:ONCE))

(tv:add-system-key #\E 'ZWEI:ZMACS-FRAME "Editor" T)
