;;; -*- Mode:LISP; Package:KERMIT; Base:8; Readtable:ZL -*-


;;Copyright LISP Machine, Inc. 1984, 1985, 1986
;;   See filename "Copyright" for
;;licensing and release information.


;;; This is the toplevel user interface for the kermit system.

(defvar kermit-frame :unbound
  "Frame for kermit")

(defvar status-pane :unbound
  "Status pane in kermit frame")

(defvar interaction-pane :unbound
  "Interaction pane in kermit frame")

(defvar command-pane :unbound
  "Pane for menu commands")

(defvar terminal-pane :unbound
  "Terminal emulation pane in kermit for connecting to remote host.
   The terminal emulated is a HEATH (or H19) type terminal.")


(declare (special kstate                        ;in calls.lisp
                  kterm-state                   ;in term.lisp
                  ))

(defconst *default-serial-stream-open-form*
          #+(OR LMI MIT TI)
          (select-processor
            (:cadr '(make-serial-stream))
            (:lambda '(sdu-serial-open))
            (:explorer '(make-serial-stream-perhaps)))
          #+3600
          '(open-device-something))

(defvar kermit-serial-stream :unbound
  "Special instance var of kermit-frame bound to serial stream or nil inside process.")

(defvar kermit-ready-for-commands? :unbound
  "Nil means data structures unitialized or invalid.")

(defvar kermit-connected-flag :unbound
  "Non-nil means locked into terminal CONNECTion.")

(defvar kermit-window-configurations '(default long-terminal))

(defflavor kermit-frame

           ((kermit-ready-for-commands? nil)
            (kermit-connected-flag nil)
            (kermit-serial-stream nil)
            (serial-stream-open-form *default-serial-stream-open-form*)
            kstate kterm-state
            )

           (tv:process-mixin
            tv:select-mixin                     ; just to get :set-process handler!
            tv:inferiors-not-in-select-menu-mixin
            tv:alias-for-inferiors-mixin
            tv:margin-choice-mixin tv:essential-mouse   ;for asynchronous mouse cmds
            tv:bordered-constraint-frame-with-shared-io-buffer)

  :SPECIAL-INSTANCE-VARIABLES
  :inittable-instance-variables
  :outside-accessible-instance-variables
  (:accessor-prefix "")

  (:documentation
    :special-purpose
    "kermit command and terminal frame for file transfer and remote terminal emulation")

  (:default-init-plist

    :margin-choices '((" Abort " nil async-abort 0 0)
                      (" Exit " nil async-exit 0 0)
                      (" Break " nil async-break 0 0)
                      (" Resume " nil async-resume 0 0))

    :borders 3                                  ; 3 on frame + 3 on each pane

    :expose-p t                                 ; expose w/o blink on instantiation
    :activate-p t                               ; activate on instantiation
    :save-bits :delayed                 ; make save bits array on deexposure
    :process '(run-kermit-process)

    :panes
    `((status-pane kermit-status-pane)
      (command-pane kermit-command-pane)
      (interaction-pane kermit-interaction-pane)
      (extra-pane kermit-status-pane)
      . ((terminal-pane kermit-terminal-pane)))

    ;;;Window configurations:
    :constraints

    '((default
        . ((top-strip terminal-pane interaction-pane)
           ((top-strip
              :horizontal (:ask-window command-pane :pane-size)
              . ((status-pane command-pane)
                 ((command-pane :ask :pane-size))
                 ((status-pane :even)))))
           ((terminal-pane 25. :lines))
           ((interaction-pane :even))))
      (long-terminal
        . ((top-strip terminal-pane interaction-pane)
           ((top-strip
              :horizontal (:ask-window command-pane :pane-size)
              . ((status-pane command-pane)
                 ((command-pane :ask :pane-size))
                 ((status-pane :even)))))
           ((interaction-pane 4. :lines))
           ((terminal-pane :even))
           )))
    ))

;;;; this is a very important thing to do unless
;;;; you like to live in the cold load stream:

(defmethod (kermit-frame :after :init) (ignore)
  (send self :set-selection-substitute
        (send self :get-pane 'interaction-pane)))

;;;User interface

(tv:add-system-key #\K 'kermit-frame "kermit" t)

;;;Inferiors
;;;

;;;Labels

(defmacro safe-font-spec (&rest fonts)
  `(loop for font-spec in '(,@fonts "CPTFONT")
         as font = (intern-soft font-spec "FONTS")
         when font (return font)))

(defmacro kermit-label(string &optional (mode :centered))
  `(list :string ,string :font ,label-font ,@(ncons mode)))

(defvar label-font (safe-font-spec "HL12BI"))

(defconst terminal-pane-label (kermit-label "Terminal Emulator Window"))

(defconst interaction-pane-label (kermit-label "Interaction Pane"))

(defconst command-pane-label (kermit-label "Commands"))

;;;This label gets overwritten by WITH-STATUS

(defconst status-pane-label (kermit-label  "Kermit Status"))

(defparameter status-pane-label-font (safe-font-spec))


;;;; scrolling mixin
;;; this should be part of the general system, but alot of people flame
;;; at the idea, so...

(defflavor scrolling-mixin
           ((scroll-p t)
            (smooth-scroll-p nil))
           ()
  (:required-flavors tv:minimum-window)
  (:init-keywords :scroll-p :smooth-scroll-p)
  :settable-instance-variables
  :gettable-instance-variables
  (:default-init-plist
    :scroll-p t
    :smooth-scroll-p nil))




(defmethod (scrolling-mixin :around :end-of-page-exception)
           (cont mt original-argument-list &rest args)
  original-argument-list
  (cond (scroll-p
         (let ((y (tv:sheet-cursor-y self))
               (old-x (tv:sheet-cursor-x self)))
           (setf (tv:sheet-cursor-x self)
                 (tv:sheet-inside-left self))
           (setf (tv:sheet-cursor-y self)
                 (tv:sheet-inside-top self))
           (cond (smooth-scroll-p
                  (send self ':smooth-delete-line))
                 (t (send self ':delete-line)))
           (setf (tv:sheet-cursor-x self) old-x)
           (setf (tv:sheet-cursor-y self) (- y tv:line-height))
           (setf (tv:sheet-end-page-flag self) 0)))
        (t (lexpr-funcall-with-mapping-table cont mt ':end-of-page-exception args))))


(defmethod (scrolling-mixin :smooth-delete-line) ()
  (let ((line-height (tv:sheet-line-height self)))
    (loop for i from 1 to line-height by 1
          do (tv:sheet-delete-line self 1 :pixel))))

; this should be doable per window, but isn't. therefore taken out.
;(tv:add-escape-key #/R
;                  'kbd-escape-scroll
;                  "terminal r -- toggle scrolling off, on, on-smooth
;terminal 0 r -- turn off scrolling
;terminal 1 r -- turn on scrolling
;terminal 2 r -- turn on smooth scrolling")

;(defun kbd-escape-scroll (arg)
;  (let ((window? tv:selected-window))
;    (and window?
;        (memq :set-scroll-p (send window? :which-operations))
;        (memq :set-smooth-scroll-p (send window? :which-operations))
;        (select arg
;          (nil (cond ((send window? :smooth-scroll-p)
;                      ;; go to no scroll
;                      (send window? :set-scroll-p nil)
;                      (send window? :set-smooth-scroll-p nil))
;                     ((send window? :scroll-p)
;                      ;; go to smooth-scroll
;                      (send window? :set-smooth-scroll-p t))
;                     (t
;                      ;; go to scroll
;                      (send window? :set-scroll-p t)
;                      (send window? :set-smooth-scroll-p nil))))
;          (0 (send window? :set-scroll-p nil)
;             (send window? :set-smooth-scroll-p nil))
;          (1 (send window? :set-scroll-p t)
;             (send window? :set-smooth-scroll-p nil))
;          (2 (send window? :set-scroll-p t)
;             (send window? :set-smooth-scroll-p t))))))



(defflavor kermit-interaction-pane ()

           (tv:notification-mixin
            tv:list-mouse-buttons-mixin
            scrolling-mixin
            tv:window)

  (:documentation
    :special-purpose
    "kermit interaction pane")

  (:default-init-plist
    :blinker-p t
    :borders 3                                  ; 3 on frame + 3 on each pane
    :vsp 3                                      ; 3 pixels between lines
    :reverse-video-p t
    :save-bits :delayed
    :more-p nil
    :label interaction-pane-label
    :deexposed-typeout-action :permit
    :font-map '(cptfont)
    :right-margin-character-flag 1))

(defflavor kermit-status-pane ()
           (tv:list-mouse-buttons-mixin
            tv:top-label-mixin
            tv:window)
  (:documentation
    :special-purpose
    "kermit status pane")
  (:default-init-plist
    :borders 3                                  ; 3 on frame + 3 on each pane
    :font-map (ncons status-pane-label-font)
    :vsp 3                                      ; 5 pixels between lines
    :more-p nil
    :deexposed-typeout-action :permit
    :save-bits :delayed
    :reverse-video-p t
    :label status-pane-label
    :blinker-p nil                              ; no blinker
    ))


(defflavor kermit-command-pane ()
           (tv:top-label-mixin
            tv:menu-highlighting-mixin
            tv:command-menu)
  (:documentation
    :special-purpose
    "kermit Command Pane")

  (:default-init-plist
    :borders 3                                  ; 3 on frame + 3 on each pane
    :label command-pane-label
    :columns 2
    :save-bits :delayed
    ;; :rows 6 ; if more items, they can be 'scrolled' to.  >>FOO ON THAT.  Can't even get the cursor.
    :reverse-video-p t
    :default-font fonts:hl12bi
    :item-list all-kermit-command-pane-items))

(defmethod (kermit-command-pane :around :execute)
           (cont mt original-argument-list item)
  original-argument-list
  (unwind-protect
      (progn (send self :add-highlighted-item item)
             (funcall-with-mapping-table cont mt :execute item))
    (send self :remove-highlighted-item item)))






;; code for terminal in "kermit; term.lisp".

(defflavor kermit-terminal-pane ()

           (tv:notification-mixin
            tv:box-label-mixin
            tv:list-mouse-buttons-mixin
            tv:window)


  (:documentation
    :special-purpose
    "A general Heath/Zenith terminal emulator for the Lisp Machine")

  (:default-init-plist
    :more-p nil
    :label-box-p t
    :border-margin-width 3
    :borders 3
    :label terminal-pane-label
    :font-map '(fonts:cptfont)
    :save-bits :delayed
    :deexposed-typeout-action :permit
    :vsp 1
    :character-height 26.                       ;1+ standard # of lines (25 for Heath/Zenith)
    ))





;;;; Asynchronous commands - these are activated by border margin choices
;;; in the bottom border of the frame.  As documented (in the Window Sys-
;;; tem manual), can not make any assumptions about the process/stack
;;; group we are in, but can assume that the variable self will be bound
;;; to instance of the particular frame we are in.  There is assumed to
;;; always be a process associated with this frame (the :process message
;;; returns it). These then simulate running in the frames own stack group
;;; by running their functions as "interrupts" to the process (via the :inter-
;;; rupt message on processes).

(defun async-abort (&rest ignore)
  (format (send self :get-pane 'interaction-pane) "~&[Abort]~%")
  (send (send self :process)
        :interrupt
        (function (lambda () (signal eh:abort-object)))))

;;; See if there's now a more standard way to "signal the abort condition"
;;; than the way done above.


(defun async-exit (&rest ignore)
  (async-abort)
  (send self :close-serial-stream)
  (send self :bury))


(defun async-break (&rest ignore)
  (send (send self :process) :interrupt #'break "kermit"))


(defun async-resume (&rest ignore)
  (let ((buf (send (send self :get-pane 'interaction-pane)
                   :io-buffer)))
    (tv:io-buffer-push buf #\resume)            ;this doesn't work in the rubout handler!
    ))

;;;; Menu


;;;; aux1-menu-alist is a hook for new user functions that want to use the kermit
;;;; interface in a somewhat hidden way. For one thing,

;;;; The format
;;;;      '(("oz-to-lmi connection"
;;;;         :funcall
;;;;            kermit-oz-to-lmi-connection
;;;;         :documentation
;;;;           "experimental modem & file transfer service between oz and lmi"
;;;;         )
;;;;         ...)


;(defconst aux1-menu-alist ())

;(defun aux1-commands ()
;  (if aux1-menu-alist
;      (tv:menu-choose aux1-menu-alist)
;    (format t "~&No Aux1 options available.~%")))


;;;; Window Menu Interface
;;; all items: (<string for menu> :funcall <name of function of no arguments>
;;;                               :documentation <string>)
;;; Note: all items beginning with the AUX1 item appear 'below' the menu--
;;; have to get to them via scroll-bar technology.


(defconst all-commands-requiring-kermit-serial-stream
          ;;;You may have to add to this list if you add to the one right below!
          '(make-connection close-connection
            send-files receive-files send-files-to-server receive-files-from-server
            finish-server bye-server
            be-a-kermit-server-only be-a-server
            )
  "Commands that require KERMIT-SERIAL-STREAM to be bound to the apropriate open stream.")

(defconst all-kermit-command-pane-items
      '(("Connect" :funcall make-connection
         :documentation "Establish a virtual terminal or Kermit connection with remote host.")

        ("Disconnect" :funcall close-connection
           :documentation "Interrupt the connection made by Connect.")

        ("Send files" :funcall send-files
           :documentation "Send files to a remote Kermit.")

        ("Receive files" :funcall receive-files
           :documentation "Receive files from a remote Kermit.")

        ;;Removed - redundant with Send Files
        ;;("Server//send" :funcall send-files
        ;;   :documentation "Send files to a remote Kermit (same as Send Files).")

        ("Server//receive" :funcall receive-files-from-server
           :documentation "Receive files from a remote Kermit that's in Server mode.")

        ("Server//finish" :buttons ;;Used to be :funcall finish-server
         ((nil :funcall finish-server
               :documentation "Finish with server; don't logout.")
          (nil :funcall bye-server
               :documentation "Finish with server and logout.")
          (nil :funcall finish-server-maybe-logout
               :documentation "Choose from menu how to finish with server: Just Finish or Finish and Logout"))
         :documentation "Finish with Kermit that's in Server mode.  L:Just Finish, M:Finish and Logout, R:Menu.")

        ;;Folded into Server Finish  as mouse-middle option
        ;;("Server//bye" :funcall bye-server
        ;; :documentation "Finish and be logged out by remote Kermit that's in Server mode.")

        ("Remote Login Server"
         :funcall be-a-server
         :documentation "Put Kermit in mode to process remote logins and file transfers.")

        ("Kermit Server"
         :funcall be-a-kermit-server-only
         :documentation "Put Kermit directly into Kermit SERVER mode (wait for Kermit commands).")

        ("Serial Port" :funcall set-baud-rate
           :documentation "Set parameters for serial port.")

;       ("Restart Program" :funcall restart-program
;          :documentation "Abandon everything  and start Kermit from scratch.")

        ("Review Parameters" :funcall review-parameters
           :documentation "Review, and maybe modify, global Kermit parameters.")


;       ("Refresh Windows" :funcall refresh-windows
;          :documentation "Refresh all the windows in this display.")

;       ("List directory" :funcall list-user-directory
;          :documentation "List the default directory in the interaction pane.")

        ("Configure Window" :funcall switch-configurations
         :documentation "Toggle between window configurations.")

        ("Help" :funcall kermit-interactive-help
         :documentation "Interactive Help for Kermit.")


        ))


;;;User interaction

(defmacro within-status (status-pane-format-string &rest format-args?)
  `(let ((maxlen (- (send status-pane :size-in-characters) 2))
         (str (format nil ,status-pane-format-string ,@format-args?)))
     (if (<= (string-length str) maxlen) str
       (string-append (substring str 0 (- maxlen 2)) " "))))

(defmacro with-status ((status-pane-format-string . format-args?) &body body)
  `(let ((*--old-label--*
           (send status-pane :label)))
     (unwind-protect
         (progn
           (send status-pane
                 :set-label                     ;which may be multi lines.
                 (list
                   :string (format nil
                                   ,status-pane-format-string
                                   . ,format-args?)
                   :font status-pane-label-font))
           . ,body)
       (send status-pane ':clear-screen)
       (send status-pane :set-label *--old-label--*))))


(defsubst beep-at-user (&optional message-format-string? &rest format-args?)
  (beep)
  (cond
    (message-format-string?
     (let ((old-mode (send status-pane ':reverse-video-p)))
       (unwind-protect
           (progn
             (send status-pane
                   ':set-reverse-video-p
                   (not old-mode))
             (with-status ((lexpr-funcall #'format nil message-format-string? format-args?))
               (process-sleep (* 60. 2)
                              "Beep!")))
         (send status-pane ':set-reverse-video-p old-mode))))))

;;; sleep 2 seconds -- maybe this should be made a user option!

;;;; Help (what?#@#$!!!)
(defun kermit-interactive-help ()
  "Get help interactively; just click on the command to document.
The documentation is then displayed in the interaction pane."
  (with-status ("~&Help with Commands.~A~A"
                (format nil "~%Please mouse any command")
                (format nil "~%to see its documentation.~%"))
    (let ((blip? (send terminal-io :any-tyi)))
      (cond ((and (not (atom blip?))
                  (eq (car blip?) :menu))
             (let* ((menu-item-name (car (cadr blip?)))
                    (menu-item-function
                      (get (cadr blip?) :funcall))
                    (documentation?
                      (or (documentation menu-item-function)    ;long doc?
                          (get (cadr blip?) :documentation))    ;short doc?
                      ))
               (cond (documentation?
                      (format interaction-pane "~&~A:~%  ~A~%"
                              menu-item-name
                              documentation?))
                     (t (format interaction-pane "~&Sorry, ~A is not documented.~%"
                                menu-item-name)))))
            (t (beep-at-user))))))

(defun switch-configurations ()
  "Switch between Kermit frame configurations."
  (with-status ("Switching configurations")
    (let* ((current (send kermit-frame :configuration))
           (new (first (remove current kermit-window-configurations))))
      (if new
          (progn
            (send kermit-frame :set-configuration new)
            (send kermit-frame :refresh))
        (beep)))))

;;;Server interaction

(defun receive-files-from-server ()
  (let* ((default-pathname (send kstate ':kermit-default-pathname))
         (filename                              ;don't merge with anything
           (prompt-and-read
             ':string-trim
             "~%Receive file:"))
         (as-filename
           (fs:merge-pathname-defaults
             (prompt-and-read
               ':string-trim
               "~%Merging with (default: ~A):"
               (fs:merge-pathname-defaults filename default-pathname))
             default-pathname)))
    (with-status ("Receive:~A ~A ~A"
                  kermit-serial-stream
                  (format nil "~%Transfer started: ~\time\"
                          (setq *kermit-beginning-time* (time:get-universal-time)))
                  (let ((baud-rate?
                          (lexpr-send
                            kermit-serial-stream
                            :send-if-handles
                            (select-processor
                              ((:lambda :explorer) (list :baud-rate))
                              (:cadr (list :get :baud))))))
                    (if baud-rate?
                        (format nil "~%Baud Rate: ~D." baud-rate?)
                      "")))
      (send kstate
            ':server-receive
            kermit-serial-stream
            filename
            as-filename))))

(defun receive-files ()
  (with-status ("Receive:~A ~A ~A"
                kermit-serial-stream
                (format nil "~%Transfer started: ~\time\"
                        (setq *kermit-beginning-time* (time:get-universal-time)))
                (let ((baud-rate?
                        (lexpr-send
                          kermit-serial-stream
                          :send-if-handles
                          (select-processor
                            ((:lambda :explorer) (list :baud-rate))
                            (:cadr (list :get :baud))))))
                  (if baud-rate?
                      (format nil "~%Baud Rate: ~D." baud-rate?)
                    "")))
    (send kstate
          ':simple-receive
          kermit-serial-stream)))


(defun send-files ()
  (let* ((default-pathname
           (send kstate ':kermit-default-pathname))
         (filename
           (prompt-and-read
             ':string-trim
             "~&Send file or filegroup (default: ~A):"
             (fs:merge-pathname-defaults "" default-pathname)))
         (filelist
           (send kstate
                 ':filelist
                 (fs:merge-pathname-defaults
                   filename
                   default-pathname)))
         (filelist-broken-down-into-from-and-to-filenames
           (loop for file? in filelist
                 with as-file?
                 with tem
                 nconcing
                 (progn
                   (format t "~&Send ~A as (default: ~A ):"
                           file? (send kstate
                                       ':string-for-kermit
                                       file?))
                   (setq as-file?
                         (if (zerop (string-length (setq tem (readline))))
                             (send kstate ':string-for-kermit file?)
                           tem))
                   (and (y-or-n-p
                          (format nil "~&Confirm sending ~A as ~A? "
                                  file? as-file?))
                        (if (string-equal file? as-file?)
                            (list file?)
                          (list (list file? as-file?))))))))
    (cond
      (filelist-broken-down-into-from-and-to-filenames
       (with-status ("Send:~A ~A ~A"
                     kermit-serial-stream
                     (format nil "~%Transfer started: ~\time\"
                             (setq *kermit-beginning-time* (time:get-universal-time)))
                     (let ((baud-rate?
                             (lexpr-send
                               kermit-serial-stream
                               :send-if-handles
                               (select-processor
                                 ((:lambda :explorer) (list :baud-rate))
                                 (:cadr (list :get :baud))))))
                       (if baud-rate?
                           (format nil "~%Baud Rate: ~D." baud-rate?)
                         "")))
         (send kstate
               ':simple-send
               kermit-serial-stream
               filelist-broken-down-into-from-and-to-filenames))))))


;;;; kermit Server (see the file SERVER for details).
(defun be-a-kermit-server-only ()
  (with-status ("Remote kermit Server~A~A~A"
                (format nil "~%Stream: ~A" kermit-serial-stream)
                (let ((current-baud-rate? (current-baud-rate)))
                  (if current-baud-rate?
                      (format nil
                              "~%Baud Rate: ~D.~%"
                              current-baud-rate?)
                    ""))
                (format nil "~%Use Control-abort key to quit locally."))
    (send kstate
          ':remote-server
          kermit-serial-stream)))


;;;; Login Server (see file S-TERM for the details).

(defun be-a-server ()
  (with-status ("Login Server ~%Stream: ~A ~A"
                kermit-serial-stream
                (let ((current-baud-rate? (current-baud-rate)))
                  (if current-baud-rate?
                      (format nil
                              "~%Baud Rate: ~D.~%"
                              current-baud-rate?)
                    "")))
    (let ((pst (make-instance 's-terminal:ps-terminal
                              :serial kermit-serial-stream
                              :peek-chars nil
                              :read-ahead-chars nil
                              :ttysync t)))
      (s-terminal:ps-kermit-login pst))))


;;;; Close connection.
;;; This shuts off the connection in the same way as the user would:
;;; by "typing in" the escape sequence (<network>c).

(defun close-connection ()
  (with-status ("Turning off Kermit//Login Connection.")
    (cond (kermit-connected-flag
           (send terminal-pane :force-kbd-input
                 #\network)
           (send terminal-pane :force-kbd-input
                 #\C)
           (setf kermit-connected-flag nil))
          (t (beep-at-user "You are not connected")))))

;;;; Make connection
;;; This is the call to the code in the TERMinal file for terminal emulation.
;;; Note that the terminal emulator will intercept and execute command menu mouse
;;; blips.

(defun make-connection ()
  (cond (kermit-connected-flag
         (beep-at-user "~&You are already connected: do <NETWORK>-C to disconnect"))
        (kermit-serial-stream
         (with-status ("Connection started: ~\time\~%~A~%~@[~A~]~%~A"
                       (setq *kermit-beginning-time* (time:get-universal-time))
                       (within-status "~A" kermit-serial-stream)
                       (let ((baud-rate?
                               (lexpr-send
                                 kermit-serial-stream
                                 :send-if-handles
                                 (select-processor
                                   ((:lambda :explorer) (list :baud-rate))
                                   (:cadr (list :get :baud))))))
                         (if baud-rate? (format nil "Baud Rate: ~D." baud-rate?)))
                       (format nil "Escape Character: ~:@C" #\network))
           (unwind-protect
               (progn (setf kermit-connected-flag t)
                      (tv:with-selection-substitute (terminal-pane kermit-frame)
                        (send kterm-state
                              ':make-connection
                              kermit-serial-stream
                              terminal-pane)))
             (setf kermit-connected-flag nil))))
        (t (ferror nil "kermit-serial-stream is NIL."))))


;;;Bye or Finish

(defun finish-server-maybe-logout (&optional logout)
  (setq logout
        (or logout
            (multiple-value-bind (choice item-chosen)
                (tv:menu-choose '(("Just Finish" :value nil)("Finish and Logout" :value T))
                                "Choose one")
              (if item-chosen choice
                (return-from finish-server-maybe-logout nil)))))
  (if logout
      (bye-server)
    (finish-server)))

;;;; Bye

(defun bye-server ()
  (with-status ("Bye Server")
    (send kstate
          ':bye-server
          kermit-serial-stream)))


;;;; Finish

(defun finish-server ()
  (with-status ("Finish Server")
    (send kstate
          ':finish-server
          kermit-serial-stream)))

;;;; Refresh

(defun refresh-windows ()
  (send kermit-frame :send-all-exposed-panes :clear-screen)
  (send (send kermit-frame :get-pane 'command-pane) :refresh))



(defconst all-baud-choices-items-alist
          '((" Extended " 0)
            (" 50.      " 50.)
            (" 75.      " 75.)
            (" 110.     " 110.)
            (" 134.     " 134.)
            (" 150.     " 150.)
            (" 300.     " 300.)
            (" 600.     " 600.)
            (" 1200.    " 1200.)
            (" 1800.    " 1800.)
            (" 2000.    " 2000.)
            (" 2400.    " 2400.)
            (" 3600.    " 3600.)
            (" 4800.    " 4800.)
            (" 7200.    " 7200.)
            (" 9600.    " 9600.)
            (" 19200.   " 19200.)))


(defun set-current-baud-rate (new-baud)
  (select-processor
    ((:lambda :explorer)
     (when (si:sdu-serial-stream-p kermit-serial-stream)
       (send kermit-serial-stream :set-baud-rate new-baud))
     (setq *sdu-serial-default-baud-rate* new-baud))
    (:cadr
     (send kermit-serial-stream
           :send-if-handles
           :put
           :baud
           new-baud))))

(defun current-baud-rate ()
  (if kermit-serial-stream
      (lexpr-send
        kermit-serial-stream
        :send-if-handles
        (select-processor
          ((:lambda :explorer) (list :baud-rate))
          (:cadr (list :get :baud))))
    ;;Return current default
    *sdu-serial-default-baud-rate*))

(defun set-baud-rate ()
  (let ((base 10.) (*nopoint nil))              ;just for printing
    (let ((old-baud (current-baud-rate)))
      (with-status ("Change Baud Rate or Extended Serial Port Parameters~%Current Baud Rate: ~S" old-baud)
        (let ((new-baud
                (tv:menu-choose
                  all-baud-choices-items-alist
                  "Choose the Baud Rate:"
                  '(:mouse)
                  nil
                  terminal-pane)))
          (cond ((and new-baud (zerop new-baud))
                 ;;If there's a real stream we can process more serial options
                 (if kermit-serial-stream
                     (extended-set-baud-rate)
                   (beep-at-user "Cannot set extended serial port parameters without an open stream")))
                ((and new-baud          ; nil if they move out of the window
                      (not (= old-baud new-baud)))      ;really have to change it
                 (set-current-baud-rate new-baud)
                 (format t "~&New Baud Rate: ~S~%" new-baud))))))))

(defun extended-set-baud-rate ()
  (select-processor
    (:lambda
      (when (si:sdu-serial-stream-p kermit-serial-stream)
        (let ((old-char-length (symeval-in-instance kermit-serial-stream 'si:char-length))
              (old-stop-bits (symeval-in-instance kermit-serial-stream 'si:stop-bits))
              (old-parity (symeval-in-instance kermit-serial-stream 'si:parity))
              (old-baud-rate (symeval-in-instance kermit-serial-stream 'si:baud-rate)))
          (let ((char-length old-char-length)
                (stop-bits old-stop-bits)
                (parity old-parity)
                (baud-rate old-baud-rate))
            (tv:choose-variable-values
              `((,(locf char-length) "Character Length" :choose (:5bits :6bits :7bits :8bits))
                (,(locf stop-bits)   "Stop Bits       " :choose (:1bit :1.5bits :2bits))
                (,(locf parity)      "Parity or None  " :choose (:even :odd NIL))
                (,(locf baud-rate)   "Baud Rate       " :number))
              :label "Extended Choice of Serial Characteristics")
            (or (equal old-char-length char-length)
                (send kermit-serial-stream :set-char-length char-length))
            (or (equal old-stop-bits stop-bits)
                (send kermit-serial-stream :set-stop-bits stop-bits))
            (or (equal old-parity parity)
                (send kermit-serial-stream :Set-parity parity))
            (or (equal old-baud-rate baud-rate)
                (set-current-baud-rate (fix baud-rate)))))
        t))))

;;;; Review parameters

(defun review-parameters ()
  (with-status ("Review Parameters")
    (send kstate :set-params)))

;;;;Ex-commands

;(defun list-user-directory ()
;  (with-status ("List Directory:~A"
;               (format nil "~%   ~A"
;                       (send kstate :kermit-default-pathname)))
;    (si:with-help-stream (stream :superior terminal-pane)
;      (listf (send kstate :kermit-default-pathname) stream))))

;(defun restart-program (&aux really?)
;  ;; do without status. maybe there's an emergency.
;  (setq really?
;       (y-or-n-p (format nil "~&Do you really want to restart and reinitialize kermit?")))
;  (cond (really?
;        (refresh-windows)
;        (setf kermit-ready-for-commands? nil)
;        (send command-pane :set-highlighted-items '())
;        (and kermit-serial-stream
;             (progn (send kermit-serial-stream :close :abort)))
;        (setf kermit-connected-flag nil)
;        (funcall command-pane :set-item-list all-kermit-command-pane-items)
;        (send status-pane :set-label status-pane-label)
;        (process-run-function "reset kermit"
;                              #'process-reset-and-enable current-process))))


;;;; Top-level

(defun run-kermit-process (kermit-frame-instance)
  (setq kermit-frame kermit-frame-instance)
  (unwind-protect
      (kermit-initial-function kermit-frame-instance)
    (send kermit-frame-instance :close-serial-stream)))

(defun kermit-initial-function (kermit-frame)
  (funcall kermit-frame :top-level kermit-frame))

(defmethod (kermit-frame :close-serial-stream) ()
  (when kermit-serial-stream
    (send kermit-serial-stream ':close ':abort)
    (setq kermit-serial-stream nil)))

(defvar *kermit-last-mouse-buttons* :unbound
  "Value from :menu blip of mouse button mask when Kermit command was pressed")

(defmethod (kermit-frame :top-level) (kermit-frame)
  (let ((status-pane (funcall kermit-frame :get-pane 'status-pane))
        (command-pane (funcall kermit-frame :get-pane 'command-pane))
        (interaction-pane (funcall kermit-frame :get-pane 'interaction-pane))
        (terminal-pane (funcall kermit-frame :get-pane 'terminal-pane))
        (terminal-io-syn-stream (make-syn-stream 'terminal-io))
        (*kermit-last-mouse-buttons* nil)
        (*sdu-serial-default-baud-rate* *sdu-serial-default-baud-rate*)
        (*sdu-serial-xon-xoff-p* *sdu-serial-xon-xoff-p*)
        (*sdu-serial-ascii-p* NIL)              ;For now, Kermit controls Ascii translation
        )
    (let ((terminal-io interaction-pane)
          (standard-input terminal-io-syn-stream)
          (standard-output terminal-io-syn-stream)
          (query-io terminal-io-syn-stream)
          (trace-output terminal-io-syn-stream)
          (error-output terminal-io-syn-stream)
          (debug-io terminal-io-syn-stream)
          (ibase 10.)
          (base 10.)
          )

      ;; if kermit is not yet ready to accept commands, either because it is
      ;; just being started up or because a reset or warm boot has been done
      ;; before it was ready for commands, do various initialization actions.

      (cond
        ((not kermit-ready-for-commands?)

         (setq kterm-state (make-instance 'kterm-state))
         (setq kstate                           ;have kstate bound to a kstate instance
               (progn
                 (fs:force-user-to-login)       ;default-pathname setup depends on user
                 (make-instance 'kstate)        ;  being logged in!
                 ))
         (setf kermit-ready-for-commands? t)))
      ;;;
      ;; this is kermit's top-level command execution loop.
      ;;
      (error-restart-loop (sys:abort "Restart kermit process")
        (loop as character = (funcall terminal-io :any-tyi)
              as command?
              = (when (and (not (atom character))
                           (eq (first character) :menu))
                  (setq *kermit-last-mouse-buttons* (third character))
                  (second character))
              doing
              (cond
                ((memq (get command? :funcall) all-commands-requiring-kermit-serial-stream)
                 (or kermit-serial-stream
                     (setq kermit-serial-stream (eval serial-stream-open-form)))
                 (if (eq (funcall command-pane :execute command?) :close)
                     (send self :close-serial-stream)))
                (command?
                 (funcall command-pane :execute command?))
                ((not (atom character))
                 (beep-at-user))
                ((= character #\hand-down)
                 (send kermit-frame ':set-configuration 'long-terminal)
                 (setq debug-io terminal-pane))
                ((= character #\hand-up)
                 (send kermit-frame ':set-configuration 'default)
                 (setq debug-io terminal-io-syn-stream)
                 (send kermit-frame :refresh))
                ('else
                 (handle-unanticipated-terminal-input character))))))))

(defvar *unanticipated-chars* nil
  "Stores unanticipated characters input to the kermit frame
   for later scientific analysis?")

(defun handle-unanticipated-terminal-input (char)
 (push char *unanticipated-chars*)
 (beep-at-user))

(compile-flavor-methods kermit-frame
                        kermit-status-pane
                        kermit-interaction-pane
                        kermit-command-pane
                        kermit-terminal-pane)
