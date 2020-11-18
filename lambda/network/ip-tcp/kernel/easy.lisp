;;; -*- Mode:LISP; Package:TCP-APPLICATION; Readtable:CL; Base:10 -*-

#|

  Copyright LISP Machine, Inc. 1987
   See filename "Copyright.Text" for
  licensing and release information.

|#

(export '(char-int-if-any
          int-char-if-any
          ))

;;;Easy TCP/UDP interface

;;;TCP-HOST and TCP-HOST-PATHNAME flavors

(defflavor tcp-host
           ()
           (si:basic-host))

(defmethod (tcp-host :name) ()
  "TCP-HOST")

(defmethod (tcp-host :system-type) ()
  :tcp-host)

(defmethod (tcp-host :name-as-file-computer) ()
  "TCP-HOST")

(defmethod (tcp-host :pathname-flavor) ()
  'tcp-host-pathname)

(defmethod (tcp-host :pathname-host-namep) (name)
  (or (typep name 'tcp-host)
      (string-equal name "TCP-HOST")))

(defflavor tcp-host-pathname
           ()
           (fs:pathname))

(defmethod (tcp-host-pathname :string-for-printing) ()
  (format nil "~A: ~D remote ~D local ~D"
          (send (send self :host) :name)
          (send self :name)
          (send self :type)
          (send self :version)))

(defmethod (tcp-host-pathname :string-for-editor) ()
  (send self :string-for-printing))

(defmethod (tcp-host-pathname :parse-namestring) (host-specified-p namestring
                                                  &optional (start 0) end)
  (declare (ignore host-specified-p))
  (declare (values device directory name type version))
  (tcp-host-parse-namestring namestring start end))

(defmethod (tcp-host-pathname :open) (&rest options)
  (apply #'open-easy-tcp-stream
         fs:name
         fs:type
         fs:version
         (cdr options)))

;;;UDP-HOST and UDP-HOST-PATHNAME flavors

(defflavor udp-host
           ()
           (si:basic-host))

(defmethod (udp-host :name) ()
  "UDP-HOST")

(defmethod (udp-host :system-type) ()
  :udp-host)

(defmethod (udp-host :name-as-file-computer) ()
  "UDP-HOST")

(defmethod (udp-host :pathname-flavor) ()
  'udp-host-pathname)

(defmethod (udp-host :pathname-host-namep) (name)
  (or (typep name 'udp-host)
      (string-equal name "UDP-HOST")))

(defflavor udp-host-pathname
           ()
           (fs:pathname))

(defmethod (udp-host-pathname :string-for-printing) ()
  (format nil "~A: ~D remote ~D local ~D"
          (send (send self :host) :name)
          (send self :name)
          (send self :type)
          (send self :version)))

(defmethod (udp-host-pathname :string-for-editor) ()
  (send self :string-for-printing))

(defmethod (udp-host-pathname :parse-namestring) (host-specified-p namestring
                                                  &optional (start 0) end)
  (declare (ignore host-specified-p))
  (declare (values device directory name type version))
  (tcp-host-parse-namestring namestring start end))

(defmethod (udp-host-pathname :open) (&rest options)
  (apply #'open-easy-udp-stream
         fs:name
         fs:type
         fs:version
         (cdr options)))

(defun tcp-host-parse-namestring (namestring &optional (start 0) (end (length namestring)))
  (flet ((skip-dotted-fields (string count test start end &aux (dot start) last-dot)
           (loop
             (when (null dot) (return nil))
             (and count (zerop count) (return dot))
             (unless (funcall test (char string (1+ dot)))
               (return dot))
             (when count (decf count))
             (setq last-dot dot)
             (setq dot (string-search "." string (1+ dot) end))
             (when (null dot)
               (return (cond ((null count) last-dot)    ;skipped as many as we could
                             ((zerop count) nil)        ;used up all dots
                             (t start)))))))            ;didn't find enough
    (let ((remote-address :wild)
          (remote-port nil)
          (local-port nil)
          (remote-port-start nil)
          (local-port-start nil)
          temp)
      (unless (= start end)
        (let ((first (char namestring start))
              (dot (string-search "." namestring start end))
              (pound (string-search "#" namestring start end))
              (name-end nil)
              (remote-port-end nil)
              (local-port-end nil))
          (when (and dot (alphanumericp first))
            (if (digit-char-p first)            ;skip dotted decimal address
                (setq dot (skip-dotted-fields namestring 3 #'digit-char-p dot (or pound end)))
              (setq dot (skip-dotted-fields namestring nil #'alpha-char-p dot (or pound end)))))
          (setq name-end (or dot pound end))
          (setq remote-address (substring namestring start name-end))
          (when (and pound dot)
            (setq local-port-start (1+ pound))
            (setq local-port-end end)
            (setq local-port (substring namestring local-port-start local-port-end)))
          (setq remote-port-start (cond (dot (1+ dot))
                                        (pound (1+ pound))))
          (when remote-port-start
            (setq remote-port-end (if local-port-start pound end))
            (setq remote-port (substring namestring remote-port-start remote-port-end)))))
      (cond ((null remote-port))
            ((setq temp (global:parse-number remote-port))
             (setq remote-port temp))
            ((and (setq temp (global:intern-soft (string-append "IPPORT-" (string-upcase remote-port))
                                                 "TCP-APPLICATION"))
                  (sym-boundp temp))
             (setq remote-port (sym-value temp)))
            (t
             (global:ferror :parse-pathname-error
                            "Bad REMOTE-PORT specification \"~A\" in: ~S"
                            remote-port
                            namestring)))
      (cond ((null local-port))
            ((setq temp (global:parse-number local-port))
             (setq local-port temp))
            ((and (setq temp (global:intern-soft (string-append "IPPORT-" (string-upcase local-port))
                                                 "TCP-APPLICATION"))
                  (sym-boundp temp))
             (setq local-port (sym-value temp)))
            (t
             (global:ferror :parse-pathname-error
                            "Bad LOCAL-PORT specification \"~A\" in: ~S"
                            local-port
                            namestring)))
      (values :unspecific
              :unspecific
              (or remote-address :wild)         ;name == remote address
              (or remote-port :wild)            ;type == remote port
              (or local-port :wild)))))         ;version == local port

(compile-flavor-methods tcp-host udp-host tcp-host-pathname udp-host-pathname)

(defvar *tcp-host* (make-instance 'tcp-host))
(pushnew *tcp-host* fs:*pathname-host-list*)

(defvar *udp-host* (make-instance 'udp-host))
(pushnew *udp-host* fs:*pathname-host-list*)

(defun open-easy-tcp-stream (remote-address remote-port local-port
                             &rest args &key
                             (keyword "Easy TCP stream")
                             (connect t)
                             (buffered t)
                             (auto-force-output nil)
                             (direction :both)
                             (input-buffers 4)
                             (output-buffers 4)
                             coroutine-input
                             for-udp
                             &allow-other-keys
                             )
  (declare (ignore coroutine-input))            ;Compatibility -- special option no longer required
  (when for-udp
    ;;This is a kludge -- should use UDP-HOST -- but is compatible with old system
    (return-from open-easy-tcp-stream
      (apply #'open-easy-udp-stream
             remote-address
             remote-port
             local-port
             (trim-keywords (copy-list args) '(:for-udp)))))
  (let ((flavor (cond ((not buffered) 'tcp-unbuffered-stream)
                      ((not auto-force-output) 'tcp-buffered-stream)
                      (t 'tcp-auto-buffered-stream)))
        (normalp nil)
        (stream nil)
        (trimmed-args (trim-keywords (copy-list args)
                                     '(:keyword :connect :buffered :auto-force-output :direction
                                       :input-buffers :output-buffers :coroutine-input :for-udp))))
    (unwind-protect
        (progn
          (when (eq remote-port :wild)          ;not specified
            (setq remote-address nil)           ;...so null out remote-address
            (setq remote-port nil)              ; and remote-port
            (setq connect nil))                 ; and make into a passive connection
          (when (eq local-port :wild)
            (setq local-port nil))
          (setq stream (if buffered
                           (make-instance flavor
                                          :input-buffer-limit (ecase direction
                                                                ((:input :both) input-buffers)
                                                                (:output 0))
                                          :output-buffer-limit (ecase direction
                                                                 ((:output :both) output-buffers)
                                                                 (:input 0))
                                          )
                         (make-instance flavor)))
          (setq normalp
                (lexpr-send stream
                            :open
                            keyword
                            :active connect
                            :remote-address remote-address
                            :remote-port remote-port
                            :local-port local-port
                            trimmed-args)))
      (unless normalp
        (and stream (close stream))))
    stream))

(defun open-easy-udp-stream (remote-address remote-port local-port
                             &rest args &key
                             (keyword "Easy UDP Stream")                ;For Peek
                             (raw t)                                    ;If raw UDP stream
                             (buffered t)                               ;If buffered or unbuffered
                             (receives-out 4)
                             &allow-other-keys
                             )
  (let ((normalp)
        (flavor (cond (raw 'udp:udp-stream)
                      (buffered 'udp:udp-buffered-stream)
                      (t 'udp:udp-unbuffered-stream)))
        (stream)
        (trimmed-args (trim-keywords (copy-list args) '(:keyword :raw :buffered :receives-out))))
    (unwind-protect
        (progn
          (when (eq remote-port :wild)          ;not specified
            (setq remote-address nil)           ;...so null out remote-address
            (setq remote-port nil))             ; and remote-port
          (when (eq local-port :wild)
            (setq local-port nil))
          (setq stream (make-instance flavor :receives-out receives-out))
          (setq normalp
                (lexpr-send stream
                            :open
                            keyword
                            :remote-address remote-address
                            :remote-port remote-port
                            :local-port local-port
                            trimmed-args)))
      (when (not normalp)
        (and stream (close stream))))
    stream))

(defun trim-keywords (arglist keywords)
  (dolist (x keywords)
    (remf arglist x))
  arglist)

;;;Compatibility

(defun tcp:get-internet-address (address)
  (ip:parse-internet-address address))

(defun char-int-if-any (x)
  ;; the :TYI message in zetalisp is defined to return a FIXNUM
  ;; for the most part a character will work, but not in
  ;; delimiters for READLINE etc.
  (if (characterp x)
      (char-int x)
    x))

(defun int-char-if-any (x)
  (and x (int-char x)))
