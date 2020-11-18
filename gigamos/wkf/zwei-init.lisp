;;; -*- Mode:LISP; Package:ZWEI; Base:10; Lowercase:T; Readtable:CL -*-

(setq *point-pdl-max* 30)
(setq *check-unbalanced-parentheses-when-saving* t)

;;; $$$ Temp. <15-Nov-88 wkf>
(set-comtab *zmacs-comtab* '(#\control-@ com-indent-for-enhancement-suggestive-cts))
(set-comtab *zmacs-comtab* '(#\control-+ com-indent-for-maintenance-suggestive-cts))
(set-comtab *zmacs-comtab* '(#\control-$ com-indent-for-local-modification-cts))
(set-comtab *zmacs-comtab* '(#\control-& com-indent-for-systematic-modification-cts))

;;; $$$ Written by SMH. <14-Nov-88 wkf>
(DEFCOM COM-SET-tvFONT "Quietly toggle the current buffer's font between TVFONT and CPTFONT." ()
  (LET* ((FONT (if (eq (TV:FONT-NAME (AREF (SEND (WINDOW-SHEET *WINDOW*) :FONT-MAP) 0))
                       'fonts:tvfont)
                   'fonts:cptfont 'fonts:tvfont)))

    (unless (BOUNDP FONT)
      (LOAD (FORMAT NIL "SYS: FONTS; ~A" FONT)
            :PACKAGE 'FONTS
            :SET-DEFAULT-PATHNAME NIL
            :IF-DOES-NOT-EXIST NIL)
      (OR (BOUNDP FONT) (BARF "~S is not a defined font" FONT)))
    (SEND *INTERVAL* :SET-ATTRIBUTE :FONTS (list FONT) nil)
    (REDEFINE-FONTS *WINDOW* (list (CONS (SYMBOL-NAME FONT) (SYMBOL-VALUE FONT))))
    (UPDATE-FONT-NAME))
  DIS-ALL)

(command-store 'com-set-tvfont #\super-t *standard-comtab*)

(set-comtab *zmacs-comtab* '(#\control-meta-y COM-REPEAT-MINI-BUFFER-COMMAND))

(defcom com-skip-to-next-defun "Searches for next form at left margin." ()
    (move-bp (point) (zwei-search (point) #.(format nil "~%(") nil t))
    (move-bp (point) (beg-line (point)))
    (com-reposition-window))
(set-comtab *zmacs-comtab* '(#\hand-right com-call-last-keyboard-macro))

(defcom com-pop-point-pdl "Pops the point pdl." ()
    (multiple-value-bind (bp pline)
        (point-pdl-pop *window*)
      (point-pdl-move bp pline))
    dis-bps)
(set-comtab *zmacs-comtab* '(#\hand-up com-pop-point-pdl))

(defcom com-revert-current-buffer "Reverts the current buffer." ()
    (revert-buffer  *INTERVAL*)
    (must-redisplay-buffer *INTERVAL* dis-text)
    dis-none)
(set-comtab *zmacs-comtab* '(#\hyper-r com-revert-current-buffer))

(set-comtab *zmacs-comtab* '(#\hyper-super-r com-refind-file))

#|||
(DEFUN ZMACS-MODE-LINE-RECALCULATE-FUNCTION (&AUX INT-TICK)
  (SETQ INT-TICK (NODE-TICK *INTERVAL*))
  (SETQ *BUFFER-MODIFIED-P* (COND ((BUFFER-READ-ONLY-P *INTERVAL*) "(RO) ")
                                  ((BUFFER-MODIFIED-P *INTERVAL*)
                                   (when (and *wkf-for-refind*
                                              (not (setq *wkf-for-refind*
                                                         (not (= 2 (length (UNDO-STATUS-UNDO-LIST
                                                                             (NODE-UNDO-STATUS-OR-NIL
                                                                               (NODE-TOP-LEVEL-NODE *INTERVAL*)))))))))
                                     (refind-file *interval*))
                                   "* ")
                                  (T (setf *wkf-for-refind* t) NIL))))
|||#

(set-comtab *zmacs-comtab* '(#\hyper-super-l com-reposition-window))  ;;same as control-meta-R

(defun com-install-last-macro-internal ()
    (assure-macro-stream :macro-previous-array)
    (let ((*package* si:pkg-keyword-package)
          (name (gensym))
          (mac (send *standard-input* :macro-previous-array))
          )
      (TYPEIN-LINE-READ
        "(Hit the ~@:C key):"
        #\Return)
      (putprop name mac 'macro-stream-macro)
      (install-command-internal (make-macro-command name nil) t)))
(defcom com-install-last-macro "Installs last defined macro on a keystroke you define" ()
    (com-install-last-macro-internal))
(set-comtab *zmacs-comtab* '(#\hyper-super-i com-install-last-macro))

(defcom com-start-keyboard-macro-brief "Begin defining a keyboard macro.
A numeric argument means append to the previous keyboard macro." (km)
     (assure-macro-stream :macro-push)
     (send *standard-input* :macro-push (+ 1 *numeric-arg-n-digits*)
           (and *numeric-arg-p* (send *standard-input* :macro-previous-array)))
     dis-none)

(defcom com-end-keyboard-macro-brief "Terminate the definition of a keyboard macro" ()
     (assure-macro-stream :macro-pop)
     (*catch 'macro-loop                                ;in case no macro running
       (send *standard-input* :macro-pop (+ 1 *numeric-arg-n-digits*)
             (and (not (zerop *numeric-arg*)) *numeric-arg*)))
     dis-none)
(set-comtab *zmacs-comtab* '(2097192 com-start-keyboard-macro-brief)) ;;control open  parentheses
(set-comtab *zmacs-comtab* '(2097193 com-end-keyboard-macro-brief))   ;;control close parentheses
(defcom com-install-end-macro "Ends keyboard macro and installs it" ()
     (assure-macro-stream :macro-pop)
     (*catch 'macro-loop                                ;in case no macro running
       (send *standard-input* :macro-pop (+ 1 *numeric-arg-n-digits*)
             (and (not (zerop *numeric-arg*)) *numeric-arg*)))
     (com-install-last-macro-internal))
(set-comtab *zmacs-comtab* '(6291497 com-install-end-macro)) ;;control-meta close parentheses

(defcom com-save-file-and-copy-bins "Save a file and copy the previous versions bins." ()
  (IF (NOT (SEND *INTERVAL* :MODIFIED-P))
      (FORMAT *QUERY-IO* "~&(No changes need to be written.)")
    (progn (SAVE-BUFFER *INTERVAL*)
           (MAYBE-DISPLAY-DIRECTORY :WRITE)
           (let ((si:user-id "COPY!!!"))
             (BUFFER-PATHNAME *interval*)
             (CONDITION-CASE (VALUE)
                 (COPY-FILE PATHNAME TO-SPEC
                            :COPY-CREATION-DATE nil
                            :COPY-AUTHOR nil
                            :CHARACTERS COPY-MODE
                            :REPORT-STREAM *QUERY-IO*)
               ((FS:FILE-ERROR SYS:REMOTE-NETWORK-ERROR)
                (BARF VALUE)))
           ) ;;From COM-SAVE-FILE
           )))
