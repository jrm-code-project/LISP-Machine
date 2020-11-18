;;; -*- Mode:LISP; Package:S-TERMINAL; Base:10; Readtable:T -*-

;;; Copyright LISP Machine, Inc. 1984, 1985, 1986
;;;   See filename "Copyright" for
;;; licensing and release information.

;;; a stream based on an ascii serial stream which can be
;;; the value of terminal-io (perhaps).
;;; 5/15/84 10:39:58 -George Carrette

;;; the first thing to do is make a primitive terminal, enough to support
;;; a printing terminal, and determine what messages are really required.
;;; eventually implement enough options to support the STREAM-MIXIN rubout
;;; handler, except of course that some terminals cannot.

;;; the PS-TERMINAL does not handle asyncronous input (such as would signal
;;; SYS:ABORT and other conditions), as such it is suitable for
;;; simple command-interpreter applications but not ideally suited for
;;; use as a lisp-listener terminal-io. In order to handle asyncronous input
;;; one needs a ps-terminal-keyboard-process, and signal conditions
;;; in other processes.

;;; 2/03/85 16:05:21 The protocal for rubout handlers has gotten a bit
;;; hairier still for system 99. So this is about the right time to
;;; punt this code and write something with handles cursorpos, interrupt characters,
;; (e.g. hack <SYSTEM> etc by using ITS-style ^_)  and takes the STREAM-MIXIN.


(defflavor ps-terminal
           (serial
            peek-chars
            (cursor-x 0)
            (cursor-y 0)
            buffer
            read-ahead-chars
            ttysync
            status)
           ()
  :initable-instance-variables)

(defvar *punt-output-next* nil)

(defmacro with-puntable-output (&body body)
  `(*catch '*punt-output-next*
     (let ((*punt-output-next* t))
       ,@body)))

(defmethod (ps-terminal :raw-stream) ()
  serial)

(defun make-ps-terminal ()
  (make-instance 'ps-terminal
                 ':serial (if (fboundp 'kermit:sdu-serial-open)
                              (kermit:sdu-serial-open)
                            (open "sdu-serial-b:"))
                 ':peek-chars nil
                 ':read-ahead-chars nil
                 ':ttysync t))

(defmethod (ps-terminal :subtyi) ()
  (char-spy
    (let ((c (if read-ahead-chars
                 (pop read-ahead-chars)
               (send serial ':tyi))))
      (cond ((memq c '(#o10 #o11 #o12 #o14 #o15))
             (+ c #o200))
            ((< c #o40)
             (set-char-bit (logxor #o100 c) :control 1))
            ((= c #o177)
             #\rubout)
            ('else
             c)))))

(defvar *char-spy? nil "NIL, T, :CHAR or :BARE")

(defun char-spy (x)
  (cond ((null *char-spy?))
        ((eq *char-spy? :char)
         (print (code-char x)))
        ((eq *char-spy? :bare)
         (print x))
        ('else
         (if (<= 0 x 256)
             (tyo x)
           (print (code-char x)))))
  x)

(defmethod (ps-terminal :tyi) (&optional ignore)
  (send self ':any-tyi))

(defvar *rubout-handler-echo? t
  "If T (default) echo when rubout handling, otherwise dont. NIL is useful
when reading passwords or on a half-duplex line.")

(defmethod (ps-terminal :any-tyi) (&optional ignore)
  (cond (peek-chars
         (pop peek-chars))
        ((not tv:rubout-handler)
         (send self ':subtyi))
        ('else
         (when (memq status '(:restored :initial-entry))
           ;; this bizzare new "place" to put the prompting evidently related to
           ;; fixing a bug in preemptable read rubout handler redisplay.
           (setq status nil)
           ;;Prompt if desired
           (let ((prompt-option (assq :prompt tv:rubout-handler-options)))
             (when prompt-option
               (tv:rubout-handler-prompt (cadr prompt-option) self nil)))
           (setq tv:rubout-handler-starting-x cursor-x tv:rubout-handler-starting-y cursor-y))
         (do ((ch)(rubout? nil)
              (activation-handler (assq :activation tv:rubout-handler-options)))
             (nil)
           (setq ch (send self ':subtyi))
           (selectq ch
             (#\rubout
              (cond ((null buffer))
                    ('else
                     (and *rubout-handler-echo? (send self ':tyo (pop buffer)))
                     (setq rubout? t))))
             (#\form
              (send self ':clear-screen)
              (send self ':redisplay ch))
             (#\control-r
              (send self ':fresh-line)
              (send self ':redisplay ch))
             (#\control-u
              (cond ((null buffer))
                    ('else
                     (send self ':fresh-line)
                     (setq buffer nil)
                     (setq peek-chars nil)
                     (send self ':redisplay ch)
                     (*THROW 'tv:RUBOUT-HANDLER t))))
             ((or (not (graphic-char-p c))
                  (not (mem #'char= c '(#\return #\tab #\line #\control-g))))
              ;; if this is the case then we dont want to deal with it.
              ;; a safety feature for ignoring bogus input that can cause
              ;; race conditions when talking to /dev/ttyl?. We are being
              ;; over cautious here, since we could just reject known losers.
              ;; Instead, allow only known winners.
              )
             (t
              (push ch buffer)
              ;; why readers (case in point, READLINE) require this translation
              ;; to be handled by the rubout handler, that is, really why the readers
              ;; couldnt be modularized in some other way, well.
              (cond ((and activation-handler
                          (apply (cadr activation-handler) ch (cddr activation-handler)))
                     (setq ch `(:activation ,ch 1))
                     (setq tv:rubout-handler-activation-character ch))
                    (*rubout-handler-echo?
                     ;; having the reader actually do the echoing of the
                     ;; activation character harkens back to extremely dark
                     ;; ages indeed. Maybe the system is mature enough now
                     ;; to lose all semblance of modularity.
                     (send self ':tyo ch)))
              (cond (rubout?
                     (setq peek-chars (reverse buffer))
                     (*THROW 'tv:RUBOUT-HANDLER t))
                    ('else
                     (setq tv:rubout-handler-activation-character nil)
                     (return ch)))))))))

(defmethod (ps-terminal :redisplay) (ch &aux len)
  (AND (SETQ LEN (OR (ASSQ ':REPROMPT tv:RUBOUT-HANDLER-OPTIONS)
                     (ASSQ ':PROMPT tv:RUBOUT-HANDLER-OPTIONS)))
       (tv:RUBOUT-HANDLER-PROMPT (CADR LEN) SELF CH))
  (and *rubout-handler-echo?
       (dolist (c (reverse buffer))
         (send self ':tyo c)))
  (send self ':tyi))

(defmethod (ps-terminal :tyipeek) ()
  (cond ((null peek-chars)
         (setq peek-chars (list (send self ':tyi)))))
  (car peek-chars))

(defmethod (ps-terminal :untyi) (c)
  (push c peek-chars))

(defmethod (ps-terminal :tyo) (c)
  (cond ((char-equal #\return c)
         (send self ':terpri))
        ('else
         (if (eq ttysync ':all) (send self ':ttysync-action))
         (cond ((graphic-char-p c)
                (setq cursor-x (1+ cursor-x))
                (send serial ':tyo c))
               ('else
                (format self "#\~A" (char-name c)))))))


(defmethod (ps-terminal :terpri) ()
  ;; we must allow some kind of flow control since there is no
  ;; more processing. also at 9600 baud output is observed to be
  ;; far more than a timesharing system can take.
  (setq cursor-x 0)
  (setq cursor-y (1+ cursor-y))
  (send serial ':tyo #o15)
  (send serial ':tyo #o12)
  (if ttysync (send self ':ttysync-action)))

(defmethod (ps-terminal :ttysync-action) ()
  ;; crude but effective.
  (do ((c))
      ((null (send serial ':listen)))
    (setq c (send serial ':tyi))
    (cond ((= c #o23)                           ; ^S
           (return (do ()
                       (nil)
                     (setq c (send serial ':tyi))
                     (cond ((= c #o23))
                           ((= c #o17)          ; ^O
                            (if *punt-output-next* (*throw '*punt-output-next* nil)))
                           ((= c #o21)          ; ^Q
                            (return t))
                           ('else
                            (setq read-ahead-chars (nconc read-ahead-chars (list c))))))))
          ((= c #o17)          ; ^O
           (if *punt-output-next* (*throw '*punt-output-next* nil)))
          ((= c #o21))
          ('else
           (setq read-ahead-chars (nconc read-ahead-chars (list c)))))))

(defmethod (ps-terminal :clear-screen) ()
  (send self ':fresh-line)
  (setq cursor-x 0)
  (setq cursor-y 0))

(defmethod (ps-terminal :clear-eol) ()
  (send self ':fresh-line))

(DEFMETHOD (ps-terminal :STRING-OUT) (STRING &OPTIONAL (START 0) END)
  (or end (setq end (string-length string)))
  (do ((j start (1+ j)))
      ((= j end))
    (send self ':tyo (aref string j))))

(defmethod (ps-terminal :fresh-line) ()
  (or (zerop cursor-x) (send self ':tyo #\return)))

(defmethod (ps-terminal :clear-input) ()
  (setq peek-chars nil)
  (setq read-ahead-chars nil)
  (send serial ':clear-input))



;; this :read-cursopos caused lossage when it was defined and attempted to be
;; used from a lisp listener debugger.

#|

(defmethod (ps-terminal :read-cursorpos) (&optional (unit ':pixel))
  (selectq unit
    (:pixel (values (* cursor-x 10.) (* cursor-y 10.)))
    (:character (values cursor-x cursor-y))))

|#


#|
;; this is the old definition from system 94.

(DEFMETHOD (ps-terminal :RUBOUT-HANDLER) (tv:RUBOUT-HANDLER-OPTIONS FUNCTION &REST ARGS)
  (LET ((PROMPT-OPTION (ASSQ ':PROMPT tv:RUBOUT-HANDLER-OPTIONS)))
    (send self ':fresh-line)
    (AND PROMPT-OPTION
         (TV:RUBOUT-HANDLER-PROMPT (CADR PROMPT-OPTION) SELF NIL))
    (setq buffer nil)
    (DO ((tv:RUBOUT-HANDLER T))
        (NIL)
      (*CATCH 'tv:RUBOUT-HANDLER
        (CONDITION-CASE (ERROR)
            (RETURN (APPLY FUNCTION ARGS))
          (SYS:READ-ERROR
           (TERPRI SELF)
           (PRINC ">>ERROR: " SELF)
           (SEND ERROR ':REPORT SELF)
           (TERPRI SELF)
           (DO () (NIL) (FUNCALL-SELF ':TYI))))))))

|#

;; in system 99 we had to start again from the definition of (stream-mixin :rubout-handler)
;; and hack it up again. Given the STREAM-RUBOUT-HANDLER instance variable we could
;; mixin STREAM-MIXIN to out stream, and rewrite things to use what is provided
;; by (stream-mixin :rubout-handler). Because this is a hacked-to-work version
;; of another function some variables it binds/sets/references might not
;; actually be doing anything.

(defmethod (ps-terminal :rubout-handler) (options function &rest args)
  (declare (arglist rubout-handler-options function &rest args))
  (if (and (eq rubout-handler self) (not (cdr (assq :nonrecursive options))))
      (let ((tv:rubout-handler-options (append options tv:rubout-handler-options)))
        (apply function args))
    (let ((tv:rubout-handler-options options))
      (setq buffer nil status :initial-entry)
      (*catch 'return-from-rubout-handler
        (let (tv:prompt-starting-x tv:prompt-starting-y
              tv:rubout-handler-starting-x tv:rubout-handler-starting-y
              (tv:rubout-handler self)
              (tv:rubout-handler-inside self)
              (tv:rubout-handler-re-echo-flag nil)
              (tv:rubout-handler-activation-character nil))
          (setq tv:prompt-starting-x cursor-x
                tv:prompt-starting-y cursor-y)
          (setq tv:rubout-handler-starting-x tv:prompt-starting-x
                tv:rubout-handler-starting-y tv:prompt-starting-y)
          (do-forever
            (setq tv:rubout-handler-re-echo-flag nil)
            (*catch 'tv:rubout-handler                  ;Throw here when rubbing out
              (condition-case (error)
                  (return
                   (apply function args))
                (sys:parse-error
                 (send self :fresh-line)
                 (princ ">>ERROR: " self)
                 (send error :report self)
                 (send self :fresh-line)
                 (setq tv:rubout-handler-re-echo-flag t)
                 (do-forever (send self :tyi)))))       ;If error, force user to rub out
            ;;Maybe return when user rubs all the way back
            (and (null peek-chars)
                 (let ((full-rubout-option (assq :full-rubout tv:rubout-handler-options)))
                   (when full-rubout-option
                     ;; Get rid of the prompt, if any.
                     (send self :fresh-line)
                     (return (values nil (cadr full-rubout-option))))))))))))

(defun ps-terminal-echo-loop (x)
  (do ((c))(nil)
    (setq c (send x ':tyi))
    (send x ':tyo c)))

(defun ps-terminal-repl-test (x)
  (let ((error-output terminal-io)
        (debug-io terminal-io))
    (si:lisp-top-level1 x)))


;; The remote login and server loop.

(defun answer-call (x)
    (tyi x)
    (format x "~%~A" (send si:local-host :name)))

(defun ps-kermit-login (x)
  "This is the toplevel loop for login to get to the command interpreter"
  (prog (user-info)
     answer-call
     (answer-call x)
     validate-password
     (cond ((setq user-info (valid-password? x))
            (welcome-user x user-info)
            (kermit-remote-loop x)
            (hangup-call x)
            (go answer-call))
           ('else
            (format x "~&Bad username or password")
            (go validate-password)))))


(defconst *hangup-call* "+++" "a string for ascii modem control of hangup")

(defun hangup-call (x)
  "the +++ characters are for experimental use with the US Robotics modem"
  (format x "~&HANGUP at ~A~%~A"
          (time:print-current-date nil)
          *hangup-call*))

(defvar ps-kermit-default-pathname)

(defun welcome-user (x info)
  (setq ps-kermit-default-pathname (or (catch-error (fs:make-pathname ':host "LM"
                                                                      ':directory (car info)))
                                       (fs:make-pathname :host "LM"
                                                         :directory "TMP")))
  (format x "~&~A logged in at ~A~%" (car info) (time:print-current-date nil)))

(define-site-variable *ps-kermit-login-passwords* :kermit-login-accounts
  "Example use in DEFSITE:
 (:kermit-login-accounts (/"gjc/" /"mobyfoo/") (/"rg/" /"mobywin/"))")

(defun add-ps-terminal-account (username password)
  (check-arg username stringp "a string")
  (check-arg password stringp "a string")
  (push (list username password) *ps-kermit-login-passwords*)
  t)

(deff authorize 'add-ps-terminal-account)
(deff passwd 'add-ps-terminal-account)

(defvar *ps-kermit-login-fails* nil)

(defvar *ps-kermit-login-wins* nil)

(defun valid-password? (x)
  (let ((uname (prompt-and-read-s x :string-trim "~&Username: "))
        (password (let ((*rubout-handler-echo? nil))
                    (prompt-and-read-s x :string-trim "~&Password: ")))
        (data))
    (cond ((or (and (null *ps-kermit-login-passwords*)
                    (null (get-site-option ':kermit-server-passwords))
                    (progn (format x "~&;NULL password database. So ~S gets in free.~%"
                                   uname)
                           (setq data (list uname password))))
               (and (or (setq data (ass #'string-equal uname *ps-kermit-login-passwords*))
                        (setq data (ass #'string-equal uname
                                        (get-site-option ':kermit-server-passwords))))
                    (string-equal (cadr data) password)))
           (push (list (time:print-current-date nil) uname password)
                 *ps-kermit-login-wins*)
           data)
          ('else
           (push (list (time:print-current-date nil) uname password)
                 *ps-kermit-login-fails*)
           nil))))


(defmacro def-kermit-remote-loop-command (name argl documentation &body body)
  (or (symbolp name)
      (ferror nil "name of command not a symbol: ~S" name))
  `(progn 'compile
          (or (memq ',name *kermit-remote-loop-commands*)
              (push ',name *kermit-remote-loop-commands*))
          (defprop ,name ,documentation kermit-loop-documentation)
          (defun (:property ,name kermit-loop-command) ,argl ,@body)))

(defvar *kermit-remote-loop-commands* nil)

(defun prompt-and-read-s (query-io &rest prompt-and-read-arguments)
  "see prompt-and-read"
  (lexpr-funcall #'prompt-and-read prompt-and-read-arguments))

(defun kermit-remote-loop (stream)
  (*catch 'kermit-remote-loop
    (do ((command-line)(command)(index)(argument)(symbol))
        (nil)
      (do ()
          ((setq command-line (prompt-and-read-s stream :string-trim "~&Kermit-Q>"))))
      (cond ((setq index (string-search-set '(#\space #\tab) command-line))
             (setq command (substring command-line 0 index))
             (setq argument (substring command-line (1+ index))))
            ('else
             (setq command command-line)
             (setq argument nil)))
      (cond ((setq symbol (car (mem #'string-equal command *kermit-remote-loop-commands*)))
             (call-kermit-loop-command symbol stream argument))
            ('else
             (format stream "~&Unknown command: ~A~%" command))))))

(defvar call-kermit-loop-command ':no-debug)

(defvar *cl-arg* nil)

(defun call-kermit-loop-command (sym stream &optional *cl-arg*)
  (cond ((eq call-kermit-loop-command ':debug)
         (funcall (get sym 'kermit-loop-command) stream))
        ((catch-error (progn (funcall (get sym 'kermit-loop-command) stream) t)))
        (t (format stream "~%**FATAL ERROR IN COMMAND: ~S **~%" SYM)
           (*throw 'kermit-remote-loop stream))))

(def-kermit-remote-loop-command ? (stream) "Pointer to help"
  (format stream "~&Type HELP for help."))

(def-kermit-remote-loop-command HELP (stream) "prints this information"
  (do ((max-width (+ 4 (apply #'max (mapcar #'flatc *kermit-remote-loop-commands*))))
       (l (reverse *kermit-remote-loop-commands*) (cdr l)))
      ((null l))
    (format stream "~&~V,,,'.<~A ~;~> ~A~%" max-width (car l)
            (get (car l) 'kermit-loop-documentation))))

(def-kermit-remote-loop-command TIME (stream) "prints the current time"
  (time:print-current-date stream))

(def-kermit-remote-loop-command  LOGOUT (stream) "and hangup the call"
  (*throw 'kermit-remote-loop stream))

(def-kermit-remote-loop-command SERVER (x) "goes into kermit server mode."
  (declare (special kermit:kermit-default-pathname))
  (setq kermit:kermit-default-pathname
        (format nil "~A:~:[~A~;~{~A~^.~}~];"
                (send ps-kermit-default-pathname ':host)
                (listp (send ps-kermit-default-pathname ':directory))
                (send ps-kermit-default-pathname ':directory)))
  (if (eq (progw (mapcan #'(lambda (x) (if (not (boundp (car x))) (list x)))
                          '((kermit:interaction-pane terminal-io)
                            (kermit:status-pane terminal-io)))
            ;; bind these variables just in case and for debugging purposes.
            (format x "~& [ Now entering server mode. Now use your local escape sequence ]~
                       ~% [ to return to your local kermit command interpreter.          ]~%")
            (kermit:kermit-remote-server (send x ':raw-stream)
                                         ps-kermit-default-pathname))
          ':logout)
      (call-kermit-loop-command 'logout x)))

(def-kermit-remote-loop-command PWD (x) "print working directory"
  (format x "~&Default pathname: ~S~%" ps-kermit-default-pathname))


(defun get-cl-arg-pathname (x prompt)
  (let ((string (or *cl-arg*
                    (prompt-and-read-s x :string "~&~A" prompt))))
    (let ((*error-output* x))
      (catch-error (merge-pathnames string ps-kermit-default-pathname)))))

(def-kermit-remote-loop-command CD (x) "change directory"
  (let ((p (get-cl-arg-pathname x "Default Pathname> ")))
    (and p (setq ps-kermit-default-pathname p)))
  (call-kermit-loop-command 'pwd x))

(def-kermit-remote-loop-command DIR (x) "print a directory listing (of pwd)"
  (let ((dir (send ps-kermit-default-pathname ':new-pathname
                   ':name ':wild
                   ':type ':wild
                   ':version ':newest)))
    (Let ((dirstream (catch-error (fs:directory-list-stream dir))))
      (cond (dirstream
             (unwind-protect
                 (with-puntable-output
                   (let ((e (send dirstream ':entry)))
                     (format x "~&Directory: ~A~%~A~%"
                             (get e ':pathname)
                             (get e ':disk-space-description))
                     (do ((f))
                         ((null (setq f (send dirstream ':entry))))
                       (format x "~A ~D bytes~%"
                               (car f) (get f ':length-in-bytes)))))
               (send dirstream ':close)))
            ('else
             (format x "~&Bad default pathname: ~A~%" ps-kermit-default-pathname))))))


(def-kermit-remote-loop-command EVAL (x) "evaluate a single lisp form"
  (cond (*cl-arg*
         (let ((*error-output* x))
           (catch-error (prin1 (eval (read-from-string *cl-arg*)) x))))))

(def-kermit-remote-loop-command repl (x) "enter a read-eval-print loop"
  (format x "~%Entering READ//EVAL//PRINT. Say (*THROW ':REPL NIL) to exit~%~%")
  (*catch ':repl
    (LET ((STANDARD-INPUT (MAKE-SYNONYM-STREAM '*TERMINAL-IO*))
          (STANDARD-OUTPUT (MAKE-SYNONYM-STREAM '*TERMINAL-IO*))
          (QUERY-IO (MAKE-SYNONYM-STREAM '*TERMINAL-IO*))
          (TRACE-OUTPUT (MAKE-SYNONYM-STREAM '*TERMINAL-IO*))
          (ERROR-OUTPUT (MAKE-SYNONYM-STREAM '*TERMINAL-IO*))
          (DEBUG-IO (MAKE-SYNONYM-STREAM '*TERMINAL-IO*)))
      (si:lisp-top-level1 x))))

(def-kermit-remote-loop-command herald (x) "print software and site version information"
  (with-puntable-output (print-herald x)))


(def-kermit-remote-loop-command type (X) "print a file on the terminal"
  (let ((f (get-cl-arg-pathname x "filename to type> ")))
    (if (and f (probef f))
        (with-puntable-output
          (with-open-file (stream f)
            (stream-copy-until-eof stream x)))
      (format x "~&File does not exists: ~S~%" f))))


;; making and debugging.

(defflavor s-terminal-debug-window
           ((ps-terminal-stream nil)
            (baud-rate 1200.))
           (tv:notification-mixin tv:process-mixin tv:window)
  (:default-init-plist
    :save-bits t
    :process '(s-terminal-debug-process :SPECIAL-PDL-SIZE #o4000
                                        :REGULAR-PDL-SIZE #o10000))
  :initable-instance-variables
    )

(defmethod (s-terminal-debug-window :after :init)
           (ignore)
  (funcall-self ':activate)
  (funcall-self ':expose)
  (funcall-self ':select))

(defmethod (s-terminal-debug-window :clean-up-stuff) ()
  (close (send ps-terminal-stream ':raw-stream)))

(defun s-terminal-debug-process (terminal-io)
  (send terminal-io ':doit-loop))

(defmethod (s-terminal-debug-window :doit-loop) ()
  (send self ':set-more-p nil)
  (send self ':set-deexposed-typeout-action ':permit)
  (cond ((null ps-terminal-stream)
         (format t "Initializing Kermit remote login~%")
         (setq ps-terminal-stream (make-ps-terminal))
         (format t "Done. Now go back to your other window if you wish.~%")
         ;(tv:deselect-and-maybe-bury-window self nil)
         ))
  (send (send ps-terminal-stream ':raw-stream) ':set-baud-rate baud-rate)
  (ps-kermit-login ps-terminal-stream))

(defmethod (s-terminal-debug-window :primitive-loop) ()
  (ps-terminal-echo-loop ps-terminal-stream))

(defconst *ps-terminal-debug-window* nil)

(defun cleanup-s-terminal-debug-window ()
  (send *ps-terminal-debug-window* ':clean-up-stuff)
  (send *ps-terminal-debug-window* ':kill)
  (setq *ps-terminal-debug-window* nil))



;;;--------------------------------------------------------------------------------

;;; SETUP-S-TERMINAL-DEBUG-WINDOW (&rest options)

;;; This is it: TOP LEVEL
;;; Call this fn, but realize that it makes a new serial stream instance,
;;; so the regular Kermit frame should not be used unless you set it up
;;; with a new serial stream. This makes a window on which you can see
;;; trace output of your fave fns, and on which kermit prints out its
;;; usual messages.

(DEFUN SETUP-S-TERMINAL-DEBUG-WINDOW (&REST OPTIONS)
  (IF *PS-TERMINAL-DEBUG-WINDOW*
      (CLEANUP-S-TERMINAL-DEBUG-WINDOW))
  (SETQ *PS-TERMINAL-DEBUG-WINDOW*
        (LEXPR-FUNCALL #'MAKE-INSTANCE 'S-TERMINAL-DEBUG-WINDOW OPTIONS))
  (TV:AWAIT-WINDOW-EXPOSURE))

;;;--------------------------------------------------------------------------------

;; For some kind of interactive eval server from the unix/streams interface.

(defmethod (s-terminal-debug-window :unix-doit-loop) ()
  (format t "INITIALIZING...NOW GO TO OTHER WINDOW~%")
  (send self ':set-more-p nil)
  (send self ':set-deexposed-typeout-action ':permit)
  (ps-kermit-login ps-terminal-stream))

(defun unix-terminal-debug-process (terminal-io)
  (send terminal-io ':unix-doit-loop))

(defun setup-unix-terminal-debug-window ()
  (make-instance 's-terminal-debug-window
                 ':process '(unix-terminal-debug-process)
                 ':ps-terminal-stream (make-instance
                                        'ps-terminal
                                        ':serial (symeval (intern "*UNIX-PORT-1*" "UNIX"))
                                        ':peek-chars nil
                                        ':read-ahead-chars nil
                                        ':ttysync t))
  (TV:AWAIT-WINDOW-EXPOSURE))



;;;

(compile-flavor-methods ps-terminal)
