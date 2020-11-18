;;; -*- Mode:LISP; Package:USER; Base:10; Readtable:CL -*-

(defstruct (t-stream (:type :named-array))
  stream
  out-io-buffer
  in-io-buffer
  )

(defun t-stream-default-handler (operation object &rest args)
  object args
  (process-wait (format nil "oops ~s" operation) #'false))

(defselect-incremental (:property t-stream si:named-structure-invoke) t-stream-default-handler)

(defun (:select-method (:property t-stream si:named-structure-invoke) :print-self) (op x stream &rest ignore)
  op
  (si:printing-random-object (x stream :type)
    ))

(defun (:select-method (:property t-stream si:named-structure-invoke) :tyo) (op t-stream c &optional font)
  op font
  (cond ((< c #o40)
         (funcall t-stream :string-out (or (nth c chaos:*telnet-output-translation-table*) "??")))
        (t
         (tv:io-buffer-put (t-stream-out-io-buffer t-stream) c))))

(defun (:select-method (:property t-stream si:named-structure-invoke) :string-out)
       (op t-stream string &optional (start 0) stop)
  op
  (when (null stop)
    (setq stop (string-length string)))
  (do ((i start (1+ i)))
      ((= i stop))
    (funcall t-stream :tyo (aref string i))))

(defun (:select-method (:property t-stream si:named-structure-invoke) :tyi) (op t-stream &optional ignore)
  op
  (let ((c (tv:io-buffer-get (t-stream-in-io-buffer t-stream))))
    (setq c (logand c #o177))
    (cond ((< c #o40)
           (setq c (set-char-bit (logior #o100 c) :control 1)))
          ((= c #o177)
           (setq c #\rubout))
          (t
           nil))
    c))

(defun (:select-method (:property t-stream si:named-structure-invoke) :untyi) (op t-stream c)
  op
  (tv:io-buffer-unget (t-stream-in-io-buffer t-stream) c))

(defun (:select-method (:property t-stream si:named-structure-invoke) :read-cursorpos) (op t-stream &optional (unit :pixel))
  op t-stream unit
  (values 0 0))

(defun (:select-method (:property t-stream si:named-structure-invoke) :beep) (op t-stream &optional ignore)
  op
  (funcall t-stream :string-out "<BEEP>"))

(defun (:select-method (:property t-stream si:named-structure-invoke) :clear-screen) (op t-stream)
  op
  (funcall t-stream :string-out "<CLEAR-SCREEN>"))

(defun (:select-method (:property t-stream si:named-structure-invoke) :clear-window) (op t-stream)
  op
  (funcall t-stream :string-out "<CLEAR-SCREEN>"))

(defun (:select-method (:property t-stream si:named-structure-invoke) :fresh-line) (op t-stream)
  op
  (funcall t-stream :tyo #\return))

(defun (:select-method (:property t-stream si:named-structure-invoke) :clear-rest-of-line) (op t-stream)
  op
  t-stream
  nil)

(defun foo (form)
  (condition-bind ((() #'(lambda (&rest ignore)
                           (format t "***")
                           nil)))
    (eval form)))

(defun create-t-stream ()
  (make-t-stream :out-io-buffer (tv:make-io-buffer 100)
                 :in-io-buffer (tv:make-io-buffer 100)))

(defvar *t-stream*)
(defvar *proc*)
(defun start-internal (&aux (initial-terminal-io *terminal-io*))
  (setq *proc* current-process)
  (setq *t-stream* (make-t-stream :out-io-buffer (tv:make-io-buffer 100)
                                  :in-io-buffer (tv:make-io-buffer 100)))
  (condition-bind ((()
                    #'(lambda (&rest ignore)
                        (let ((*terminal-io* initial-terminal-io))
                          (ferror nil "foo")))))
    (si:lisp-top-level1 *t-stream*)))

(defun stop ()
  (mapcar #'(lambda (x)
               (when (string-equal (process-name x) "Lisp Proc")
                 (send x :kill)))
          si:all-processes))

(defun start ()
  (stop)
  (setq *t-stream* nil)
  (process-run-function "Lisp Proc" #'start-internal)
  (process-wait "setup" #'(lambda () *t-stream*))
  *t-stream*)

(defun soak (&optional no-hang)
  (let ((io-buffer (t-stream-out-io-buffer *t-stream*)))
    (do ((c (tv:io-buffer-get io-buffer no-hang)
            (tv:io-buffer-get io-buffer no-hang)))
        ((null c))
      (send standard-output :tyo c))))

(defun unsoak (string)
  (let ((io-buffer (t-stream-in-io-buffer *t-stream*)))
    (do ((i 0 (1+ i))
         (end (string-length string)))
        ((= i end))
      (tv:io-buffer-put io-buffer (aref string i)))))

(defun kill-supdup-servers ()
  (mapcar #'(lambda (x)
              (when (and (string-equal (process-name x) "SUPDUP Server")
                         (not (eq x current-process)))
                (send x :kill)))
          si:all-processes))

(defun supdup-server-function (&aux conn)
  (kill-supdup-servers)
  (setq conn (chaos:listen "SUPDUP"))
  (let ((lose (chaos:disallow-connection? "supdup" conn
                                          (list chaos:telnet-server-on :reject-unwanted))))
    (when lose
      (chaos:reject conn lose)
      (return-from supdup-server-function nil))
    (chaos:accept conn)
    (push conn chaos:eval-server-connections)
    (send tv:who-line-file-state-sheet :add-server conn "SUPDUP")
    (with-open-stream (net-stream (chaos:make-stream conn))
      (let ((lisp-stream (start)))
        (let ((to-lisp (t-stream-in-io-buffer lisp-stream))
              (to-net (t-stream-out-io-buffer lisp-stream)))
          (send net-stream :string-out "hello")
          (send net-stream :tyo #o210)
          (send net-stream :force-output)
          (read-supdup-parameters net-stream)
          (do-forever
            (send net-stream :force-output)
            (process-wait "chars"
                          #'(lambda ()
                              (or (and (not (tv:io-buffer-empty-p to-net))
                                       (chaos:may-transmit conn))
                                  (and (send net-stream :listen)
                                       (not (tv:io-buffer-full-p to-lisp))))))
            (move-bytes-to-net to-net net-stream)
            (move-bytes-to-lisp net-stream to-lisp)
            ))))))

(defun move-bytes-to-net (to-net net-stream)
  (when (not (tv:io-buffer-empty-p to-net))
    (let ((c (tv:io-buffer-get to-net)))
      (send net-stream :tyo c))))

(defun move-bytes-to-lisp (net-stream to-lisp)
  (when (send net-stream :listen)
    (let ((c (send net-stream :tyi)))
      (cond ((= c #o300)
             (process-supdup-escape net-stream)
             )
            (t
             (tv:io-buffer-put to-lisp c))))))

(defvar *supdup-server-location* nil)

(defun process-supdup-escape (stream)
  (let ((command (send stream :tyi)))
    (case command
      (#o302
       (setq *supdup-server-location*
             (with-output-to-string (s)
               (do ((c (send stream :tyi) (send stream :tyi)))
                   ((zerop c))
                 (send s :tyo c)))))
      )))

(defun read-18-bits (stream)
  (let* ((b0 (send stream :tyi))
         (b1 (send stream :tyi))
         (b2 (send stream :tyi)))
    (+ (ash b0 12.)
       (ash b1 6)
       b2)))

(defvar *supdup-server-tctyp*)
(defvar *supdup-server-ttyopt*)
(defvar *supdup-server-height*)
(defvar *supdup-server-width*)
(defvar *supdup-server-ttyrol*)
(defvar *supdup-server-ttysmt*)

(defun read-supdup-parameters (stream)
  (let ((nwords (dpb (read-18-bits stream) (byte 18. 0) -1)))
    (read-18-bits stream)
    (when (< nwords 0)
      (let* ((x (read-18-bits stream))
             (y (read-18-bits stream)))
        x
        (setq *supdup-server-tctyp* y))
      (incf nwords))
    (when (< nwords 0)
      (let* ((x (read-18-bits stream))
             (y (read-18-bits stream)))
        (setq *supdup-server-ttyopt* (dpb y (byte 18. 18.) x)))
      (incf nwords))
    (when (< nwords 0)
      (let* ((x (read-18-bits stream))
             (y (read-18-bits stream)))
        x
        (setq *supdup-server-height* y))
      (incf nwords))
    (when (< nwords 0)
      (let* ((x (read-18-bits stream))
             (y (read-18-bits stream)))
        x
        (setq *supdup-server-width* y))
      (incf nwords))
    (when (< nwords 0)
      (let* ((x (read-18-bits stream))
             (y (read-18-bits stream)))
        x
        (setq *supdup-server-ttyrol* y))
      (incf nwords))
    (when (< nwords 0)
      (let* ((x (read-18-bits stream))
             (y (read-18-bits stream)))
        (setq *supdup-server-ttysmt* (dpb y (byte 18. 18.) x)))
      (incf nwords))
    (do ()
        ((zerop nwords))
      (read-18-bits stream)
      (read-18-bits stream)
      (incf nwords))))


(add-initialization "SUPDUP"
                    '(process-run-function "SUPDUP Server" 'supdup-server-function)
                    NIL
                    'chaos:server-alist)
