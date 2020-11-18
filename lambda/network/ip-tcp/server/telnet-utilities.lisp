;;; -*- Mode:LISP; Package:NETWORK-USER; Base:10; Readtable:CL -*-

#|

  Copyright LISP Machine, Inc. 1986, 1987
   See filename "Copyright.Text" for
  licensing and release information.

First cut of utilities that may be useful to the user coming in via telnet
server.

|#

(defun help ()
  (typecase *terminal-io*
    (telnet:telnet-server
     (format t "~&You are logged into a Lisp Machine Telnet Server.

Interrupt Keys:
    Control-G        Abort Interrupt
    Control-T        Status Interrupt
    Control-S        Stop Output
    Control-Q        Resume Output
    Control-Z        Break Interrupt

Quote character:
    Control-V <x>    <x>
")
     (cond ((eq 'tv:alternate-rubout-handler (symeval-in-instance *terminal-io* 'tv:stream-rubout-handler))
            (format t "
Your terminal is smart enough to use the full Lisp Machine Rubout handler
for editing input.
"))
           (t
            (format t "
Your terminal is smart enough only to provide minimal rubout handling.

Keys to Edit Input:
    Rubout           Delete one character
    Meta-Rubout      Delete one word
    Control-U        Delete all input
    Control-L        Clear screen and refresh input
    Control-R        Fresh Line and refresh input
"))))
    (telnet:supdup-server
     (format t "~&You are logged into a Lisp Machine Supdup Server.~%
Your terminal is smart enough to use the full Lisp Machine Rubout handler
for editing input.~%"))
    (t
     (format t "~&You are logged into a Lisp Machine Console.~%
Your terminal is smart enough to use the full Lisp Machine Rubout handler
for editing input.~%")))
  (cond ((condition-case (foo)
             (telnet:termcap.extended-keyboard (symeval-in-instance *terminal-io* 'telnet:termcap))
           (error t))
         (format t "
Your terminal can send Control, Meta, Super, and Hyper bits with its characters.
"))
        (t
         (format t "
Ascii Keys for the rubout handler:
    Control-U        Delete all input (Clear-Input)
    Control-L        Clear screen and refresh input (Clear-Screen)

Prefix keys to modify characters:
    Escape <x>       Meta-<x>
    Control-\\ <x>    Super-<x>
")))
  (format t "
Useful programs in the NETWORK-USER package:
    (logout)         Close the Telnet connection
    (help)           Print this message
")
  )

(defun logout ()
  "Close the telnet connection"
  (throw 'telnet:telnet-server-logout nil))

(defun ed (&rest ignored)
  (no-window-calls))

(defun fed (&rest ignored)
  (no-window-calls))

(defun inspect (&rest ignored)
  (no-window-calls))

(defun kermit (&rest ignored)
  (no-window-calls))

(defun peek (&rest ignored)
  (no-window-calls))

(defun supdup (&rest ignored)
  (no-window-calls))

(defun telnet (&rest ignored)
  (no-window-calls))

(defun zmail (&rest ignored)
  (no-window-calls))

(defun dired (&rest ignored)
  (no-window-calls))

(defun mail (&optional user text call-editor-anyway)
  (declare (ignore call-editor-anyway))
  (if text
      (user:mail user text nil)
    (no-window-calls)))

(defun no-window-calls ()
  "You can't call a function that switches windows")

(defun status-interrupt (s)
  (format *terminal-io*
          "~&~A: ~A ~$ cpu, ~$ disk ~D faults, ~1$% ~A~%"
          (getf s :hostname)
          (getf s :state)
          (getf s :cpu-time)
          (getf s :disk-wait-time)
          (getf s :page-faults)
          (getf s :percent-utilization)
          (time:print-universal-time (getf s :current-time) ()))
  (send *terminal-io* :force-output))

(defun handle-abort (&rest ignore)
  (setq inhibit-scheduling-flag nil)            ;It was T in the IO-BUFFER-OUTPUT-FUNCTION
  (send *terminal-io* :clear-rest-of-line)
  (send *terminal-io* :string-out "[Abort]")
  (signal-condition eh:abort-object))

(defun handle-abort-all (&rest ignore)
  (setq inhibit-scheduling-flag nil)            ;It was T in the IO-BUFFER-OUTPUT-FUNCTION
  (send *terminal-io* :clear-rest-of-line)
  (send *terminal-io* :string-out "[Abort all]")
  (send current-process :reset :always))

(defun handle-break (&optional char &rest ignore)
  (setq inhibit-scheduling-flag nil)            ;It was T in the IO-BUFFER-OUTPUT-FUNCTION
  (break "BREAK")
  (values char t))

(defun handle-error-break (&optional char &rest ignore)
  (declare (dbg:error-reporter))
  (setq inhibit-scheduling-flag nil)            ;It was T in the IO-BUFFER-OUTPUT-FUNCTION
  (multiple-value-bind (buffer position)
      (send *standard-input* :send-if-handles :save-rubout-handler-buffer)
    (unwind-protect
        (signal-condition
          (make-condition 'break :format-string "Keyboard break.")
          '(:no-action) t)
      (if buffer (send *standard-input* :restore-rubout-handler-buffer buffer position))))
  (values char t))
