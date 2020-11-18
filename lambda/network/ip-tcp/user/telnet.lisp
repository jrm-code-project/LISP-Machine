;;; -*- Mode:LISP; Package:TELNET; Base:10; Readtable:CL -*-

#|

  Copyright LISP Machine, Inc. 1985, 1986, 1987
   See filename "Copyright.Text" for
  licensing and release information.

The Lambda has 4 User Telnet programs:

  1) System-T -- Telnet window
  2) System-K -- Kermit H19 emulator
  3) (telnet:telnet-glass-tty) -- this file.  crude, displays option negotiations without action.
  3) (telnet:telnet) -- this file.  crude but fast.  Ignores option negotiations.

For now, this is at the level of a crude hack.
There is no negotiation, no interpretation of TELNET_IAC.
What it provides is enough to say:

    (telnet-glass-tty "100.0.0.10")

As a quick but functional hack. Good for debugging the rest of
the TCP implemenation. 3/30/85 16:22:32 -George Carrette

|#

(defmacro without-more-processing (the-window &body body)
  "execute body with more processing disabled"
  (let ((more-p (gentemp "MOREP"))
        (window (gentemp "WINDOW")))
    `(let* ((,window ,the-window)
            (,more-p (send ,window :more-p)))
       (unwind-protect
           (progn (send ,window :set-more-p nil)
                  ,@body)
         (send ,window ':set-more-p ,more-p)))))


(eval-when (compile eval load)

(defun glass-tty-ascii-code (lispm-char)
  (cond ((char-bit lispm-char :control)
         (logxor #o100 (char-upcase (glass-tty-ascii-code (char-code lispm-char)))))
        ((not (zerop (char-bits lispm-char)))
         nil)
        ((member lispm-char '(#\Return #\Line #\Tab #\Form) :test #'eq)
         (- lispm-char #o200))
        ((= lispm-char #\Rubout)
         #o177)
        ((graphic-char-p lispm-char)
         (char-code lispm-char))))

(compiler:defoptimizer glass-tty-ascii-code-constant glass-tty-ascii-code (form)
  (global:destructuring-bind (nil arg) form
    (if (constantp arg) (eval form) form)))

)

(defvar *telsyms*
        '(
          (     255     iac             "interpret as command:")
          (     254     dont            "you are not to use option")
          (     253     do              "please, you use option")
          (     252     wont            "I won't use option")
          (     251     will            "I will use option")
          (     250     sb              "interpret as subnegotiation")
          (     249     ga              "you may reverse the line")
          (     248     el              "erase the current line")
          (     247     ec              "erase the current character")
          (     246     ayt             "are you there")
          (     245     ao              "abort output--but let prog finish")
          (     244     ip              "interrupt process--permanently")
          (     243     break           "break")
          (     242     dm              "data mark--for connect. cleaning")
          (     241     nop             "nop")
          (     240     se              "end sub negotiation")

          (     242     synch           "for telfunc calls")))

(dolist (x *telsyms*)
  (proclaim `(special ,(cadr x)))
  (set (cadr x) (car x))
  (setf (get (cadr x) 'telnet-sym) (car x)))

(defvar *telopts*
        '(
          (   0 telopt_binary   "8-bit data path")
          (   1 telopt_echo     "echo")
          (   2 telopt_rcp      "prepare to reconnect")
          (   3 telopt_sga      "suppress go ahead")
          (   4 telopt_nams     "approximate message size")
          (   5 telopt_status   "give status")
          (   6 telopt_tm       "timing mark")
          (   7 telopt_rcte     "remote controlled transmission and echo")
          (   8 telopt_naol     "negotiate about output line width")
          (   9 telopt_naop     "negotiate about output page size")
          (  10 telopt_naocrd   "negotiate about CR disposition")
          (  11 telopt_naohts   "negotiate about horizontal tabstops")
          (  12 telopt_naohtd   "negotiate about horizontal tab disposition")
          (  13 telopt_naoffd   "negotiate about formfeed disposition")
          (  14 telopt_naovts   "negotiate about vertical tab stops")
          (  15 telopt_naovtd   "negotiate about vertical tab disposition")
          (  16 telopt_naolfd   "negotiate about output LF disposition")
          (  17 telopt_xascii   "extended ascii character set")
          (  18 telopt_logout   "force logout")
          (  19 telopt_bm       "byte macro")
          (  20 telopt_det      "data entry terminal")
          (  21 telopt_supdup   "supdup protocol")
          (  22 telopt_supdup-output "supdup output allowed")
          ( 255 telopt_exopl    "extended-options-list")))

(dolist (x *telopts*)
  (proclaim `(special ,(cadr x)))
  (set (cadr x) (car x))
  (setf (get (cadr x) 'telnet-opt) (car x)))

(defun telnet-glass-tty (address &optional (port "TELNET") half-duplex)
  "This glass TTY works well enough for testing purposes"
  (check-type address string)
  (without-more-processing *terminal-io*
    (with-open-file (stream (string-append "TCP-HOST:" address "." port)
                            :keyword "Telnet Glass TTY"
                            :auto-force-output (not half-duplex))
      (catch 'eof
        (let (p)
          (unwind-protect
              (progn (setq p (process-run-function
                               "telnet input from remote"
                               #'telnet-glass-tty-characters-from-remote
                               stream
                               *terminal-io*
                               sys:current-process))
                     (if half-duplex
                         (do ((line))
                             (nil)
                           (setq line (global:prompt-and-read :string ""))
                           (with-input-from-string (s line)
                             (telnet-glass-tty-handle-keyboard s
                                                               stream)
                             (send stream :tyo (glass-tty-ascii-code #\return))
                             (send stream :tyo (glass-tty-ascii-code #\line))
                             (send stream :force-output)))
                       (telnet-glass-tty-handle-keyboard *terminal-io* stream)))
            (and p (send p :kill))))))))


(defun telnet-glass-tty-characters-from-remote (from to superior)
  (global:condition-case (x)
      (do ((c)
           (iac (get 'iac 'telnet-sym)))
          ((null (setq c (send from :tyi)))
           (format to "~&***CONNECTION CLOSED AT REMOTE END***~%"))
        (cond ((eq c iac)
               (glass-tty-telnet-iac from to))
              ((eq c #o12))
              ((member c '(#o10 #o11 #o14 #o15) )                     ;test #eq
               (send to :tyo (+ c #o200)))
              ('else
               (send to :tyo c))))
    (error
      (send x :report to)))
  (send superior :interrupt
        #'(lambda () (throw 'eof nil)))
  (si:process-wait-forever))


(defun telnet-glass-tty-handle-keyboard (from to)
  (do ((c))
      ((null (setq c (send from :tyi))))
    (cond ((= c #\return)
           (send to :tyo (glass-tty-ascii-code #\return))
           (send to :tyo (glass-tty-ascii-code #\line)))
          ('else
           (let ((cm (glass-tty-ascii-code c)))
             (when cm
               (send to :tyo cm)))))))

(defun glass-tty-telnet-iac (remote-stream terminal)
  (format terminal "~&***IAC ")
  (let ((cs (send remote-stream :tyi)))
    (case (cadr (assoc cs *telsyms* :test #'eq))
      (nil (format terminal " undefined in protocol: ~D ~%" cs))
      ((do dont will wont)
       (let ((oc (send remote-stream :tyi)))
         (format terminal "~A ~A~%"
                 (caddr (assoc cs *telsyms* :test #'eq))
                 (caddr (assoc oc *telopts* :test #'eq)))))
      (sb
       (format terminal "~A~%" (caddr (assoc cs *telsyms* :test #'eq)))
       (do ()
           ((eq (send remote-stream :tyi) (get 'se 'telnet-sym)))))
      (t
       (format terminal "~A~%" (caddr (assoc cs *telsyms* :test #'eq)))))))

;;;Here is a User Telnet that talks directly to a TCP socket.
;;It is fast, but primitive -- no option negotiation.

(defvar *telnet-logging* nil)
(defvar *telnet-data-log* nil)
(defvar *telnet-final-stats-block* nil)
(defvar *telnet-final-tcp-stats-block* nil)

(defun initialize-data-log ()
  (setq *telnet-data-log* nil))

(defun log-data (buffer)
  (when *telnet-logging*
    (push (string buffer) *telnet-data-log*)))

(defparameter *telnet-receive-buffers* 8)
(defparameter *telnet-receive-buffer-size* 256)

(defun telnet (host)
  (if (ip-header-p host)
      (setq host (ip:ih-dest-address host))
    (let ((original-host host))
      (assert (numberp (setq host (parse-internet-address (setq original-host host))))
              (host)
              "~S is not a valid Internet host specification"
              original-host)))
  (let ((socket (make-tcp-socket :keyword "User Telnet"))
        (opened nil)
        (open nil)
        (term-in-buffer nil)
        (term-out-buffer nil))
    (without-more-processing *terminal-io*
      (initialize-data-log)
      (unwind-protect
          (cond ((setq opened (send socket :open
                                    :remote-port 23
                                    :remote-address host
                                    :active t
                                    :auto-push t
                                    :optimistic t))
                 (setq term-in-buffer (make-array 256 :fill-pointer 0 :element-type '(unsigned-byte 8)))
                 (setq term-out-buffer (make-string 256))
                 (loop
                   (process-wait "Terminal or Network"
                                 #'(lambda (term)
                                     (or (send socket :listen)
                                         (and open (send term :listen))))
                                 *terminal-io*)
                   (cond ((send *terminal-io* :listen)
                          (get-terminal-data term-in-buffer)
                          (copy-data-to-net term-in-buffer socket))
                         ((send socket :listen)
                          (let ((item (send socket :read-data)))
                            (case (first item)
                              (:open
                               (dotimes (i *telnet-receive-buffers*)
                                 (send socket
                                       :receive
                                       (make-array *telnet-receive-buffer-size*
                                                   :fill-pointer 0
                                                   :element-type '(unsigned-byte 8))))
                               (setq open t))
                              (:data            ;Data from remote host
                               (log-data (second item))
                               (copy-data-to-terminal (second item) term-out-buffer)
                               (unless (eq (third item) :eof)
                                 (send socket :receive (second item))))
                              (:closing         ;Remote side has closed
                               (return "Closed by remote end"))
                              (:reset
                               (setq opened open)
                               (return (if open "Connection reset" "Connection refused")))
                              (:close           ;Socket closed out from under us
                               (setq opened nil)
                               (return "Closed locally"))
                              ((:network-unreachable :host-unreachable :protocol-unreachable :port-unreachable)
                               (return "Unreachable"))
                              (:timeout
                               (send socket :abort)
                               (setq opened nil)
                               (return "Timed out"))
                              (otherwise
                               ;;Ignore it
                               )))))))
                (t
                 "TCP not running"))
        (when opened
          (send socket :close))
        (setq *telnet-final-stats-block* (tcp:tcp-user-statistics-block socket))
        (setq *telnet-final-tcp-stats-block* (tcp:tcp-user-stats socket))
        (setq *telnet-data-log* (nreverse *telnet-data-log*))))))

(defun playback ()
  (let ((term-out-buffer (make-string 256)))
    (dolist (x *telnet-data-log*)
      (copy-data-to-terminal x term-out-buffer))))

(defun get-terminal-data (buffer)
  (do ((limit (array-length buffer))
       (index 0))
      ((or (not (send *terminal-io* :listen))
           (>= index limit))
       (setf (fill-pointer buffer) index)
       buffer)
    (let ((char (char-to-ascii (send *terminal-io* :tyi))))
      (when char
        ;;(send *terminal-io* :tyo char)
        (setf (aref buffer index) char)
        (incf index)
        (when (eq char #o15)
          (setf (aref buffer index) #o12)
          (incf index))))))

(defun copy-data-to-net (buffer socket)
  (when (plusp (length buffer))
    (send socket :write-data (string buffer))))

(defun copy-data-to-terminal (in-buffer out-buffer)
  (do ((index 0 (1+ index))
       (count 0)
       (limit (length in-buffer)))
      ((>= index limit)
       (when (plusp count)
         (send *terminal-io* :string-out out-buffer 0 count)))
    (let ((char (ascii-to-char (aref in-buffer index))))
      (cond ((eq char #o210)
             (when (plusp count)
               (send *terminal-io* :string-out out-buffer 0 count)
               (setq count 0))
             (send *terminal-io* :tyo char))
            (char
             (setf (char out-buffer count) char)
             (incf count))))))

(defun char-to-ascii (lispm-char)
  (cond ((char-bit lispm-char :control)
         (logxor #o100 (char-upcase (char-to-ascii (char-code lispm-char)))))
        ((not (zerop (char-bits lispm-char)))
         nil)
        ((member lispm-char `(,(char-int #\return) ,(char-int #\line) ,(char-int #\tab) ,(char-int #\form)))
         (- lispm-char #o200))
        ((= lispm-char (char-int #\Rubout))
         #o177)
        ((graphic-char-p lispm-char)
         (char-code lispm-char))))

(defun ascii-to-char (ascii-char)
  (cond ((> ascii-char #o177)
         ;;high bit set
         nil)
        ((= ascii-char #o15)
         ;;Carriage Return -- ignore it
         nil)
        ((= ascii-char #o12)
         ;;Line Feed -- convert to RETURN
         #\return)
        ((member ascii-char '(#o11 #o14 #o10))
         ;; Tab, Form Feed, Backspace
         (+ ascii-char #o200))
        ((< ascii-char #o40)
         ;;Other control character
         nil)
        ('else
         ;;Normal printing character
         ascii-char)))
