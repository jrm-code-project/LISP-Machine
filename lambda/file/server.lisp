;;;-*- Mode:LISP; Package:FS; Readtable:ZL; Base:8; Lowercase:T -*-

(defstruct (server-dataproc-comm (:type :list) :conc-name (:alterant nil))
  iotype control-proc data-proc conn sibling cell opening arg binp dinfo cconn tid)

;;; Toplevel and parsers

;;; This is separate to allow debugging by recompiling rfile-server.

(defun file-server (&rest ignore)
  (rfile-server))

(declare (special fs:*local-server-via-net*))

(defvar server-login-id "FileComputer")
(defvar server-login nil)
(defvar trace-server-enabled nil)
(defvar server-traces nil)
(defvar *lmfs-server-dont-answer-logins* nil)
(defvar server-protocol-version 0)
(defvar server-openings)
(defvar alldatas)                               ;all dataconn cells
(defvar tid)                                    ;running transaction id
(defvar conn-stream)                            ;connection stream
(defvar conn)                                   ;connection
(defvar server-instance)                        ;tag for props
(defvar uname)
(defvar lmfs-server-lossages nil)
(defvar lmfs-debug-server nil
  "T means file server processes do not handle most errors, so you can debug them.
Note that changing this may not take affect in existing servers.")
(defvar local-host-pathname)

(defvar *server-shutdown-message* nil)
(defvar *server-shutdown-time* 0)

;(defun cv-time (x) (time:print-universal-time x nil nil :mm//dd//yy))
;; this is different from the above since the :mm//dd//yy format will print
;;  5/10/77 instead of 05/10/77
(defun cv-time (ut)
  (multiple-value-bind (sec min hr day mon yr)
      (time:decode-universal-time ut)
    (format nil "~2,'0D//~2,'0D//~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            ;; note that this won't work beyond 1999.  Too bad!
            mon day (- yr 1900.)
            hr min sec)))


(defun trace-server (&optional (onoff t))
  (setq trace-server-enabled onoff))

(defvar trap-error nil)

(defmacro trap-lossage ((condition-names id) code &body errors)
  (declare (zwei:indentation 0 7 1 3 2 1))
  `(condition-case-if (not lmfs-debug-server) (trap-error)
       ,code
     (,condition-names
      (tv:notify nil "File server got an error.")
      (push (list (cv-time (time:get-universal-time))
                  ',id
                  (send trap-error :report-string)
                  (and (boundp 'conn)
                       conn
                       (si:get-host-from-address (chaos:foreign-address conn) :chaos)))
            lmfs-server-lossages)
      . ,errors)))

(defun rfile-server ()
  (if (and server-login
           (or (null user-id)
               (zerop (string-length user-id))))
      (trap-lossage (error "Server Login")
          (progn
            (login server-login-id si:local-host)
            (print-server-login-exegesis))))
  (let (tid conn-stream conn alldatas server-openings
        (server-instance (gensym))              ;bind em all local....
        (user-id server-login-id)
        (server-protocol-version server-protocol-version)
        (fs:*local-server-via-net* nil))
    (unwind-protect
        (trap-lossage (error "Server Top Level")
            (catch 'server-chaos-disappear
              (setq conn (chaos:listen "FILE"))
              (when (chaos:unwanted-connection-p conn)
                (chaos:unwanted-reject (prog1 conn (setq conn nil)))
                (ferror nil "unwanted connection tried to come in."))
              (when *lmfs-server-dont-answer-logins*
                (chaos:reject (prog1 conn (setq conn nil))
                              *lmfs-server-dont-answer-logins*)
                (ferror nil *lmfs-server-dont-answer-logins*))
              (let* ((pkt (chaos:read-pkts conn))       ;s/b rfc
                     (result (server-parse-rfc pkt)))
                (cond ((fixp result)
                       (setq server-protocol-version result))
                      (t (chaos:reject (prog1 conn (setq conn nil)) result)
                         (ferror nil result))))
              (chaos:accept conn)
              (send tv:who-line-file-state-sheet
                    :add-server conn "FILE" si:current-process
                    'lmfs-peek-server (process-stack-group si:current-process))
              (setq conn-stream (chaos:make-stream conn))
              (if *server-shutdown-message* (send-single-shutdown-message conn))
              (error-restart-loop ((sys:abort error) "Return to server command-reading loop.")
                (let (pkt op)
                  (setq pkt (trap-lossage (error "Server Reading packets")
                                (condition-bind
                                  ((sys:host-stopped-responding
                                     #'(lambda (&rest ignore)
                                         (throw 'server-chaos-disappear nil)))
                                   (sys:connection-lost
                                     #'(lambda (&rest ignore)
                                         (throw 'server-chaos-disappear nil))))
                                  (chaos:get-next-pkt conn))
                              (ferror 'server-control-conn-network-lossage
                                      "Control connection lost")))
                  (setq op (chaos:pkt-opcode pkt))
                  (cond ((or (= op chaos:eof-op)
                             (= op chaos:cls-op))
                         (send conn-stream :force-output)
                         (chaos:return-pkt pkt)
                         (throw 'server-chaos-disappear nil))
                        ((not (= op chaos:dat-op))
                         (ferror nil "Unrecognized packet opcode: ~S" op)))
                  (let* ((string (chaos:pkt-string pkt))
                         (strings (get-strings-from-pktstring string))) ;nl-delimited strings

                    (if trace-server-enabled
                        (without-interrupts (push (string-append string) server-traces)))
                    (destructuring-bind (tid fh cmd . rest) (parse-cmd-string string)
                        (if *lmfs-server-dont-answer-logins*
                            (format conn-stream "~A ~A ERROR HNA F Host not available - ~A "
                                    tid (or fh "")
                                    *lmfs-server-dont-answer-logins*)
                          (selectq cmd
                            (:login (setq user-id (file-server-login rest)))
                            (:open   (file-server-open fh rest (car strings)))
                            (:open-for-lispm
                             (apply 'file-server-open-for-lispm
                                    fh (car strings)
                                    (let ((*read-base* 10.) (*print-base* 10.)
                                          (*package* si:pkg-user-package)
                                          (*readtable* si:initial-readtable))
                                      (read-from-string
                                        string nil
                                        (+ (string-search-char #/cr string)
                                           (length (car strings))
                                           2)))))
                            (:extended-command
                             (apply 'file-server-extended-command
                                    fh (car rest) (car strings)
                                    (let ((*read-base* 10.) (*print-base* 10.)
                                          (*package* si:pkg-user-package)
                                          (*readtable* si:initial-readtable))
                                      (read-from-string
                                        string nil
                                        (+ (string-search-char #/cr string)
                                           (length (car strings))
                                           2)))))
                            (:data-connection (file-server-data-connection fh rest))
                            (:moby-connection (file-server-moby-connection fh rest))
                            (:undata-connection (file-server-undata-connection fh))
                            (:close (file-server-close-connection fh))
                            (:filepos (file-server-filepos fh rest))
                            (:delete (file-server-delete fh strings))
                            (:rename (file-server-rename fh strings))
                            (:expunge (file-server-expunge fh strings))
                            (:complete (file-server-complete fh rest strings))
                            (:continue (file-server-continue fh))
                            (:directory (file-server-directory fh rest strings))
                            (:change-properties (file-server-change-props fh strings))
                            (:create-directory (file-server-create-directory fh strings))
                            (:create-link (file-server-create-link fh strings))
                            (otherwise (format conn-stream "~A ~A ERROR UKC F Unknown command: ~A"
                                               tid (or fh "") cmd)))))
                    (send conn-stream :force-output)
                    (chaos:return-pkt pkt))))))
      (when conn
        (send tv:who-line-file-state-sheet :delete-server conn)
        (trap-lossage (error "Server Top Level close")
            (chaos:close-conn conn
                              (or *lmfs-server-dont-answer-logins*
                                  "Server error")))
        (chaos:remove-conn conn))
      (if server-openings
          (trap-lossage (error "Server finish closing remaining openings")
              (dolist (opening server-openings)
                (send opening :close :abort))))
      (trap-lossage (error "Closeout undata")
          (dolist (data alldatas)
            (rplaca (server-dataproc-comm-cell
                      (get (server-dataproc-comm-sibling data) server-instance))
                    'undata)
            (rplaca (server-dataproc-comm-cell data) 'undata)
            (chaos:remove-conn (server-dataproc-comm-conn data)))))))

(defun server-parse-rfc (pkt &aux s fx version)
  (setq s (chaos:pkt-string pkt))
  (setq fx (string-search "FILE" s))
  (cond ((null fx) "Unparseable RFC")
        (t (setq fx (string-search-not-char #/SP s (+ fx 4)))
           (cond ((null fx) 0)
                 ((null (setq version (parse-number s fx)))
                  "Unparseable version number in RFC")
                 ((or (= version 0) (= version 1)) version)
                 (t (format nil "Unsupported FILE protocol version: ~D" version))))))

(defun parse-cmd-string (string &aux answers)
  (let ((nlx (string-search-char #/CR string)))
    (do ((start 0) (lim (or nlx (string-length string))))
        (( start lim) (nreverse answers))
      (if (char-equal (aref string start) #/SP)
          (progn
            (push nil answers)
            (incf start))
          (let ((endx (or (string-search-char #/SP string start lim) lim)))
            (push (or (and (loop for x from start below endx finally (return t)
                                 unless (memq (aref string x)
                                              '(#/0 #/1 #/2 #/3 #/4 #/5 #/6 #/7 #/8 #/9))
                                 return nil)    ;parse number is a dead bear
                           (parse-number string start endx))
                      (si:intern1 (substring string start endx) si:pkg-keyword-package))
                  answers)
            (setq start (1+ endx)))))))

(defun get-strings-from-pktstring (string &aux answer)
  (do ((start (string-search-char #/CR string)))
      ((null start) nil)
    (let ((ix (string-search-char #/CR string (1+ start))))
      (if (null ix)
          (progn
            (if (not (= (1+ start) (string-length string)))
                (push (substring string (1+ start)) answer))
            (return (nreverse answer))))
      (push (substring string (1+ start) ix) answer)
      (setq start ix))))

(defun file-server-login (rest)
  (let ((uname (car rest)))
    (if (null uname)
        (format conn-stream "~A  ERROR LIP F Invalid Login syntax" tid)
        (progn
          (format conn-stream "~A  ~A ~A ~%~A~%" tid "LOGIN" uname
                  (send local-host-pathname
                        ':new-pathname :device :unspecific
                        ':directory (string uname)
                        ':name nil :type nil :version nil))
          (string uname)))))

;;; Opening files.

(defun file-server-open (fh rest filename &aux answer binp
                         (characters t) direction directionp
                         if-exists if-does-not-exist (byte-size :default)
                         deleted preserve-dates inhibit-links)
  (let ((losep
          (*catch 'open-opt-lost
            (progn
              (loop for olist on rest
                    do
                    (let ((opt (car olist)))
                      (selectq opt
                        (:binary (setq characters nil))
                        (:character (setq characters t))
                        (:default (setq characters :default))
                        (:read (setq direction :input directionp t))
                        (:write (setq direction :output directionp t))
                        (:probe (setq direction nil directionp t))
                        (:probe-directory
                         (setq direction :probe-directory directionp t))
                        (:probe-link
                         (setq direction nil directionp t inhibit-links t))
                        (:inhibit-links
                         (setq inhibit-links t))
                        ((:temporary :raw :super-image))
                        (:deleted (setq deleted t))
                        (:preserve-dates (setq preserve-dates t))
                        (:byte-size
                         (setq byte-size (cadr olist))
                         (pop olist))
                        (:if-exists (setq if-exists (cadr olist))
                                    (pop olist))
                        (:if-does-not-exist (setq if-does-not-exist (cadr olist))
                                            (pop olist))
                        (:ESTIMATED-SIZE (pop olist))   ;COMES FROM SYMBOLICS
                        (t (open-err "UOO F Unknown option: " opt)))))
              (if (null fh)
                  (if (memq direction '(:input :output))
                      (open-err "ICO F Inconsistent open options for probe opening"))
                ;; FHN given. must be real read or write.
                (let* ((comdata (get fh server-instance))
                       (type (selectq (server-dataproc-comm-iotype comdata)
                               (input :input)
                               (output :output))))
                    (if (null comdata)
                        (open-err "UFH F No open data channel for this file handle: " fh))
                    (if directionp
                        (unless (eq direction type)
                          (open-err "ICO F File handle type inconsistent with open mode."))
                      (setq direction type))))
              (let ((pathname (lmfs-parse-for-server filename)))
                (if (errorp pathname) (open-err "IPS F Bad filename syntax: " pathname))
                (let ((opening
                        (open pathname
                              :direction direction
                              :characters characters
                              :if-does-not-exist (or if-does-not-exist
                                                      (selectq direction
                                                        ((:input nil) :error)
                                                        (:output :create)))
                              :if-exists (or if-exists
                                              (if (memq (pathname-version pathname)
                                                        '(:unspecific :newest))
                                                  :new-version :supersede))
                              :error nil
                              :inhibit-links inhibit-links
                              :deleted deleted
                              :preserve-dates preserve-dates
                              :byte-size byte-size)))
                  (if (errorp opening) (*throw 'open-opt-lost (lmfs-error-string opening)))
                  (setq binp
                        (selectq characters
                          (:default (not (send opening :characters)))
                          (t (not characters))))
                  (setq answer
                        (selectq server-protocol-version
                          (0
                           (format nil
                                   "~D ~A ~D ~S~%~A~%"
                                   (send (send opening :truename) :version)
                                   (cv-time (send opening :creation-date))
                                   (send opening :length)
                                   (send opening :send-if-handles :qfaslp)
                                   (server-print-pathname (send opening :truename))))
                          (1
                           (format nil
                                   "~A ~D ~S ~S~%~A~%"
                                   (cv-time (send opening :creation-date))
                                   (send opening :length)
                                   binp         ;qfaslp, needed for compatibility
                                   (not binp)
                                   (server-print-pathname
                                     (send opening :truename))))))
                  (if (null direction)
                      (send opening :close)
                    (let ((servi (get fh server-instance)))
                      (push opening server-openings)
                      (setf (server-dataproc-comm-binp servi)
                            ;; BINP used to mean QFASLP, so people did not expect
                            ;; it to work for :BYTE-SIZE 8 files. But now it means binaryp
                            ;; but is doing the wrong thing for non-character binary files.
                            ;; So we arrange to set keep this BINP flag NIL here if our opening is
                            ;; an 8-bit file.
                            (COND ((NOT BINP) NIL)
                                  ((OR (EQ 8 (SEND-IF-HANDLES OPENING :BYTE-SIZE))
                                       (EQ 8 BYTE-SIZE))
                                   NIL)
                                  ('ELSE
                                   T)))
                      (setf (server-dataproc-comm-tid servi) tid)
                      (setf (server-dataproc-comm-opening servi) opening)
                      (rplaca (server-dataproc-comm-cell servi)
                              (if (eq direction :input) 'read 'write))))
                  nil))))))
    (if (null losep)
        (format conn-stream  "~A ~A OPEN ~A" tid (or fh "") answer)
        (format conn-stream  "~A ~A ERROR ~A" tid (or fh "") losep))))

(defun lmfs-error-string (error)
  (let* ((pn (send error :send-if-handles :pathname))
         (pnn (and pn (typep pn 'fs:pathname) (send pn :string-for-printing)))
         (en (send error :report-string)))
    ;; Drop a period off the end of the error message.
    (and (char-equal (aref en (1- (length en))) #/.)
         (setq en (substring en 0 (1- (length en)))))
    ;; Drop " for lm: foo" off the end.
    (and pnn
         (string-equal en pnn :start1 (- (length en) (length pnn)))
         (setq en (substring en 0 (- (length en) (length pnn) 5))))
    (string-append (dolist (cn (send error :condition-names))
                     (when (get cn 'file-error)
                       (return (get cn 'file-error))))
                   " F "
                   en)))

(defun open-err (&rest args)
  (*throw 'open-opt-lost (apply 'string-append args)))

(defvar local-host-pathname nil
  "A pathname whose host is SI:LOCAL-HOST.")

(defun init-local-host-pathname ()
  (setq local-host-pathname (fs:default-pathname nil si:local-host nil nil t)))

(add-initialization "Initialize local host pathname" '(init-local-host-pathname) :warm)
(add-initialization "Initialize local host pathname" '(init-local-host-pathname) :site)


(defvar *lmfs-parse-for-server-translations* nil)
;; if not NIL this is an alist of ("foreign-hostname" . t-alist)
;; where t-alist is ("from" "to")

(defun lmfs-parse-for-server (string)
  (condition-case (result)
      (lmfs-parse-for-server-translate string)
    (pathname-error result)))

(defun lmfs-parse-for-server-translate (pathname)
  (let* ((faddr (and (boundp 'conn) conn (chaos:foreign-address conn)))
         (fhost (and faddr (si:get-host-from-address faddr :chaos)))
         tr new)
    (setq pathname (cond ((and fhost
                               (eq :lmfs (send-if-handles fhost :file-system-type))
                               (or (get fhost 'wants-lmfs-pathnames-only)
                                   (string-search ">" pathname)))
                          (putprop fhost t 'wants-lmfs-pathnames-only)
                          (send (fs:merge-pathname-defaults
                                  pathname
                                  (fs:default-pathname nil fhost nil nil t)
                                  :unspecific :newest)
                                :new-pathname :host si:local-host))
                         ('else
                          (fs:merge-pathname-defaults
                            pathname
                            local-host-pathname
                            :unspecific :newest))))
    (cond ((not *lmfs-parse-for-server-translations*)
           pathname)
          ((setq tr (ass #'(lambda (addr host)
                             (let ((addrs (send (si:parse-host host) :network-addresses)))
                               (member addr (getf addrs :chaos))))
                         faddr
                         *lmfs-parse-for-server-translations*))
           (setq new (condition-case (x)
                         (lmfs-parse-for-server-translate-1
                          pathname (cdr tr))
                       (pathname-error x)))
           (cond ((errorp new)
                  (tv:notify nil "File serving ~S ~A" (car tr) (send new :report-string))
                  pathname)
                 ((eq new pathname)
                  pathname)
                 ('else
                  (tv:notify nil "File serving ~S translating ~A => ~A" (car tr) pathname new)
                  new)))
          ('else
           pathname))))

(defun lmfs-parse-for-server-translate-1 (path alist)
  (dolist (x alist path)
    (let ((pat (parse-pathname (car x) si:local-host)))
      (when (send pat :pathname-match path)
        (return (send pat :translate-wild-pathname
                      (parse-pathname (cadr x) si:local-host)
                      path))))))

(defun wants-lmfs-pathnames-only ()
  (let ((fhost (and (boundp 'conn)
                    conn
                    (si:get-host-from-address (chaos:foreign-address conn) :chaos))))
    (when (and fhost (get fhost 'wants-lmfs-pathnames-only))
      fhost)))

(defun other-guy-pathname (pathname fhost)
  (let ((p (make-pathname :host fhost
                          :device nil
                          :directory (send pathname :directory)
                          :name (send pathname :name)
                          :type (send pathname :type)
                          :version (send pathname :version))))
    (cond ((eq (send pathname :directory) :root)
           ;; kludge fix. bug work around
           (parse-pathname (string-append fhost
                                          ":>"
                                          (send pathname :name)
                                          "."
                                          (send pathname :type)
                                          "."
                                          (format nil "~D" (send pathname :Version)))
                           fhost))
          ('else
           p))))

(defun server-print-pathname (pathname)
  (let ((fhost (wants-lmfs-pathnames-only)))
    (cond ((not fhost)
           (if (eq (send pathname :host) si:local-host)
               (send pathname :string-for-host)
             (send pathname :string-for-printing)))
          ('else
           (send (other-guy-pathname pathname fhost)
                 :string-for-host)))))


;;;; Special open command that handles arbitrary open options.

(defconst unmentioned-standard-stream-ops
          '(:advance-input-buffer :break :characters :clear-input
            :clear-output :close :describe :direction
            :discard-current-input-buffer :discard-current-output-buffer
            :discard-input-buffer :discard-output-buffer :eof
            :eval-inside-yourself :finish :force-output :fresh-line
            :funcall-inside-yourself :get-handler-for :get-input-buffer
            :init :last-char-output :line-in :line-out :listen
            :new-output-buffer :next-input-buffer :operation-handled-p
            :plist :print-self :qfaslp :read-input-buffer :read-pointer
            :read-until-eof :rewind :send-current-output-buffer
            :send-if-handles :send-output-buffer :set-buffer-pointer
            :set-pointer :setup-new-output-buffer :setup-next-input-buffer
            :stream-input-buffer :stream-input-index :stream-input-limit
            :stream-output-buffer :stream-output-index
            :stream-output-limit :string-in :string-out :truename :tyi
            :tyi-no-hang :tyipeek :tyo :untyi :which-operations
            :who-line-information))

;;; These stream operations of the local file stream,
;;; plus all standard stream operations, do not get mentioned
;;; in the WHICH-OPERATIONS list we send to the remote system
;;; because they are either implemented specially over there
;;; or are not supposed to be available there.
(defconst unmentioned-stream-ops
          `(:init
            :get :getl :get-location :putprop :remprop :push-property
            :plist :property-list :set-property-list :setplist
            :info :pathname :generic-pathname :status
            :delete :rename :undelete :expunge :open :change-properties
            :all-directories :directory-list :directory-list-stream :peek-file-system
            :set-byte-size :byte-size :creation-date :length
            ;; Some LMFILE-only ones.
            :author :pdp10-format :must-explicitly-close
            :force-close :node :its-directory-stream
            ;; more local file operations added by Robert.
            :APPEND-OR-OVERWRITE
            :BYTES-BEFORE-CURRENT-BLOCK
            :BYTES-IN-BLOCK
            :CURRENT-LENGTH-FROM-MAP
            :KEEP-RESERVED-BLOCKS
            :LAST-BLOCK-P
            :MAP-NBLOCKS
            :MAX-BYTES-IN-BLOCK
            :MAYBE-UPDATE-MAP-BLOCK-SIZE
            :SET-BUFFER-POINTER-TO-END
            :SET-MAP-FOR-APPEND
            :SETUP-BUFFER-FOR-APPEND
            :SETUP-NEXT-OUTPUT-BUFFER
            :UPDATE-RQB-VALID-PAGES
            ;; ones that gjc thinks must be bogus
            :PROPERTY-LIST-LOCATION
            :PLIST-LOCATION
            :GET-LOCATION-OR-NIL
            ;; standard ones.
            ,@unmentioned-standard-stream-ops))

(defun file-server-open-for-lispm (fh filename &rest modes
                                   &key &optional (direction :input)
                                   moby-mapped
                                   &allow-other-keys)
  (let ((losep
          (*catch 'open-opt-lost
            (progn
              (if (null fh)
                  (unless (or (eq direction :probe) (null direction) moby-mapped)
                    ;; :direction missing or not nil.
                    (open-err "ICO F Inconsistent open options for probe opening"))

                ;; FHN given. must be real read or write.
                (let* ((comdata (get fh server-instance))
                       (type (server-dataproc-comm-iotype comdata)))
                    (if (null comdata)
                        (open-err "UFH F No open data channel for this file handle: " fh))
                    (unless (eq type
                                (selectq direction
                                  (:input 'input)
                                  (:output 'output)))
                      (open-err "ICO F File handle type inconsistent with open mode"))))
              (let ((pathname (lmfs-parse-for-server filename)))
                (if (errorp pathname) (open-err "IPS F Bad filename syntax: " pathname))
                (let ((opening
                        (if moby-mapped (apply 'open pathname :error nil :direction nil modes)
                          (apply 'open pathname :error nil modes))))
                  (if (errorp opening) (*throw 'open-opt-lost (lmfs-error-string opening)))
                  (format conn-stream "~A ~A OPEN " tid (or fh ""))
                  (format conn-stream
                          "~A ~D ~S ~S ~S ~D~%~A~%"
                          (cv-time (send opening :creation-date))
                          (send opening :length)
                          (send opening :send-if-handles :qfaslp)
                          (send opening :characters)
                          (send opening :get :author)
                          (send opening :byte-size)
                          (server-print-pathname
                            (send opening :truename)))
                  (let ((*read-base* 10.) (*print-base* 10.)
                        (*readtable* si:initial-readtable)
                        (*package* si:pkg-user-package))
                    (send conn-stream :tyo #/()
                    (dolist (op (send opening :which-operations))
                      (unless (memq op unmentioned-stream-ops)
                        (prin1 op conn-stream)
                        (send conn-stream :tyo #/sp)))
                    (send conn-stream :tyo #/))
                    (print (or (send opening :send-if-handles :file-contents-plist)
                               (send opening :send-if-handles :file-plist))
                           conn-stream))
                  (cond (moby-mapped
                         (let ((root (apply 'open pathname :error nil modes)))
                           (cond ((not (or (consp root) (arrayp root)))
                                  (open-err "ICO F MOBY-MAPPED open failed"))
                                 (t (moby-mapped-open-response-5th-line conn-stream root)))))
                        ((or (null direction) (eq direction :probe))
                         (send opening :close))
                        (t
                         (let ((servi (get fh server-instance)))
                           (push opening server-openings)
                           (setf (server-dataproc-comm-binp servi)
                                 (COND ((send opening :characters)
                                        NIL)
                                       ((EQ 8 (SEND-if-handles opening :byte-size))
                                        NIL)
                                       ('ELSE
                                        T)))
                           (setf (server-dataproc-comm-tid servi) tid)
                           (setf (server-dataproc-comm-opening servi) opening)
                           (rplaca (server-dataproc-comm-cell servi)
                                   (selectq direction
                                     (:input 'read)
                                     (:output 'write)
                                     (t (ferror "direction is not :input or :output")))))))
                  nil))))))
    (if losep
        (format conn-stream  "~A ~A ERROR ~A" tid (or fh "") losep))))


;;; Allow Lispm machine to do arbitrary stream and pathname operations
;;; with arbitrary Lisp data as arguments and as values.
(defun file-server-extended-command (fh command pathname &rest args &aux target)
  ;; Either FH or PATHNAME, but not both, should be non-nil.
  (unless
    ;; This returns t if pathname or stream is not suitable
    (cond ((null fh)
           (setq target (lmfs-parse-for-server pathname))
           (when (errorp pathname)
             (format conn-stream "~A  ERROR IPS F Syntax error in pathname" tid)
             t))
          (t
           (let* ((data (get fh server-instance))
                  (opening (server-dataproc-comm-opening data)))
             (setq target opening)
             (when (or (null data)
                       (null opening)
                       (symbolp opening))       ;yes, I know NIL is a symbol, thx
               (format conn-stream "~A ~A ERROR UFH F No opening for handle ~A"
                       tid fh fh)
               t))))
    (condition-case (results)
        (multiple-value-list
         (apply target command args))
      (error
       (format conn-stream "~A ~A ERROR ~A"
               tid (or fh "") (lmfs-error-string (car results))))
      (:no-error
       (format conn-stream "~A ~A ~A~%"
               tid (or fh "") command)
       (let ((*print-base* 10.) (*read-base* 10.)
             (*readtable* si:initial-readtable)
             (*package* si:pkg-user-package))
         (prin1 results conn-stream))))))

;;;; Data connection stuff.

(defun null-car (x) (null (car x)))

(defun send-sync-mark (dconn)
  (chaos:send-pkt dconn (chaos:get-pkt) fs:%qfile-synchronous-mark-opcode))

(defun file-server-data-connection (fh rest)
  (let ((ifh (car rest))
        (ofh (cadr rest))
        (default-cons-area working-storage-area))
    (if (not (and ifh ofh (symbolp ifh) (symbolp ofh)))
        (format conn-stream "~A ~A ERROR IRF F Ill-formed data-connection request"
                tid (or fh ""))
      (condition-case (dconn)
          (chaos:connect (chaos:foreign-address conn) (string ofh))
        (error
         (format conn-stream "~A ~A ERROR NWL F Reverse data connection (~A) lost:~%~A"
                 tid (or fh "") ofh dconn))
        (:no-error
         (putprop ifh
                  (make-server-dataproc-comm iotype 'input conn dconn cconn conn)
                  server-instance)
         (putprop ofh
                  (make-server-dataproc-comm iotype 'output conn dconn cconn conn)
                  server-instance)
         (let ((ocell (cons nil ofh))
               (icell (cons nil ifh))
               (idata (get ifh server-instance))
               (odata (get ofh server-instance)))
           (setf (server-dataproc-comm-data-proc idata)
                 (process-run-function
                   (string-append "File Server Data " ifh)
                   'file-server-data-top-level
                   server-instance icell ifh))
           (setf (server-dataproc-comm-data-proc odata)
                 (process-run-function
                   (string-append "File Server Data " ofh)
                   'file-server-data-top-level
                   server-instance ocell ofh))
           (setf (server-dataproc-comm-sibling idata) ofh)
           (setf (server-dataproc-comm-sibling odata) ifh)
           (push odata alldatas)                ;one side's good enough.
           (setf (server-dataproc-comm-cell idata) icell)
           (setf (server-dataproc-comm-cell odata) ocell)
           (format conn-stream "~A ~A DATA-CONNECTION" tid (or fh ""))))))))

;a moby-connection is similar to a data connection except that there is just one process,
; which receives a packet stream and replies with one.  It pays no attention to OPENING.
; The command cell of the input side is set immediately to moby-server, so the guy
;goes right to work.
(defun file-server-moby-connection (fh rest)
  (let ((ifh (car rest))
        (ofh (cadr rest))
        (default-cons-area working-storage-area))
    (if (not (and ifh ofh (symbolp ifh) (symbolp ofh)))
        (format conn-stream "~A ~A ERROR IRF F Ill-formed data-connection request"
                tid (or fh ""))
      (condition-case (dconn)
          (chaos:connect (chaos:foreign-address conn) (string ofh))
        (error
         (format conn-stream "~A ~A ERROR NWL F Reverse data connection (~A) lost:~%~A"
                 tid (or fh "") ofh dconn))
        (:no-error
         (putprop ifh
                  (make-server-dataproc-comm iotype 'input conn dconn cconn conn)
                  server-instance)
         (putprop ofh
                  (make-server-dataproc-comm iotype 'output conn dconn cconn conn)
                  server-instance)
         (let ((ocell (cons nil ofh))
               (icell (cons 'moby-server ifh))   ;go immediately into moby-server mode.
               (idata (get ifh server-instance))
               (odata (get ofh server-instance)))
           (let ((proc (process-run-function
                         (string-append "File Server for MOBY " ofh)
                         'file-server-data-top-level
                         server-instance icell ofh)))
             (setf (server-dataproc-comm-data-proc idata) proc)
             (setf (server-dataproc-comm-data-proc odata) proc)
             (setf (server-dataproc-comm-sibling idata) ofh)
             (setf (server-dataproc-comm-sibling odata) ifh)
             (push odata alldatas)              ;one side's good enough.
             (setf (server-dataproc-comm-cell idata) icell)
             (setf (server-dataproc-comm-cell odata) ocell)
             (format conn-stream "~A ~A MOBY-CONNECTION" tid (or fh "")))))))))

(defun file-server-undata-connection (fh)
  (let ((data (get fh server-instance)))
    (cond ((null data)
           (format conn-stream "~A ~A ERROR UFH F Unknown file handle: ~A" tid fh fh))
          (t (dolist (fh (list fh (server-dataproc-comm-sibling data)))
               (let* ((data (get fh server-instance))
                      (cell (server-dataproc-comm-cell data)))
                 (await-data-process cell 'undata)
                 ;; We can't predict which side we'll be told to undata, so make sure
                 ;; that all sides get removed from alldatas.
                 (setq alldatas (delq data alldatas)))
               (remprop fh server-instance))
             (chaos:remove-conn (server-dataproc-comm-conn data))
             (format conn-stream "~A ~A UNDATA-CONNECTION" tid fh)))))

(defun file-server-data-top-level (server-instance cell handle)
  (let ((fs:*local-server-via-net* nil))
    (declare (special fs:*local-server-via-net*))
    (trap-lossage (error "File Server Data Connection")
        (do-forever
          (process-wait "Data Conn Cmd" #'car cell)
          (let* ((data (get handle server-instance))
                 (celloc (locf (car cell)))
                 (opening (server-dataproc-comm-opening data))
                 (dconn (server-dataproc-comm-conn data))
                 (binp (server-dataproc-comm-binp data)))
            (selectq (car cell)
              (undata                           ;Gute Nacht, O Wesen.
               (rplaca cell nil)
               (return nil))

              ((fpsync wsync)
               (send-sync-mark dconn)
               (rplaca cell nil))

              (directory
               (server-dataproc-hack-directory data handle)
               (%store-conditional celloc 'directory nil))
              (write
               (if (null opening) (ferror nil "file-server-data-top-level - no opening"))
               (condition-bind ((no-more-room
                                  (let-closed ((server-instance server-instance)
                                               (cell1 cell)
                                               (handle1 handle))
                                    'server-disk-full-handler)))
                 (*catch 'async-abort
                   (do-forever
                     (if (not (eq (car cell) 'write)) (return nil))
                     (let* ((pkt (if (server-window-write-check cell dconn 'write)
                                     (chaos:get-next-pkt dconn)
                                   (return nil))))
                       (select (chaos:pkt-opcode pkt)
                         (chaos:eof-op
                          (chaos:return-pkt pkt)
                          (setq pkt (if (server-window-write-check cell dconn 'write)
                                        (chaos:get-next-pkt dconn)
                                      (return nil)))
                          (or (= (chaos:pkt-opcode pkt) fs:%qfile-synchronous-mark-opcode)
                              (ferror "Unrecognized Opcode in data server: ~O"
                                      (chaos:pkt-opcode pkt)))
                          (chaos:return-pkt pkt)
                          (%store-conditional celloc 'write nil)
                          (return nil))
                         (fs:%qfile-synchronous-mark-opcode
                          (chaos:return-pkt pkt)
                          (%store-conditional celloc 'write nil)
                          (return nil))
                         ((fs:%qfile-binary-opcode fs:%qfile-character-opcode)
                          (unwind-protect
                              (if binp
                                  (send opening :string-out pkt chaos:first-data-word-in-pkt
                                        (+ (truncate (chaos:pkt-nbytes pkt) 2)
                                           chaos:first-data-word-in-pkt))
                                (send opening :string-out (chaos:pkt-string pkt)
                                      0 (chaos:pkt-nbytes pkt)))
                            (chaos:return-pkt pkt)))
                         (otherwise (ferror nil "Unknown pkt opcode: ~O" (chaos:pkt-opcode pkt)))))))))
              (read
               (if (null opening) (ferror nil "file-server-data-top-level - no opening"))
               (do (last eofp) (())
                 (if (server-window-read-check cell dconn) (return nil))
                 (let ((pkt (chaos:get-pkt)))
                   (cond (binp
                          (multiple-value (last eofp)
                            (send opening :string-in nil pkt
                                  chaos:first-data-word-in-pkt chaos:max-data-words-per-pkt))
                          (setf (chaos:pkt-opcode pkt) fs:%qfile-binary-opcode)
                          (setf (chaos:pkt-nbytes pkt)
                                (* 2 (- last chaos:first-data-word-in-pkt))))
                         (t (multiple-value (last eofp)
                              (send opening :string-in nil (chaos:pkt-string pkt)
                                    0 chaos:max-data-bytes-per-pkt))
                            (setf (chaos:pkt-opcode pkt) fs:%qfile-character-opcode)
                            (setf (chaos:pkt-nbytes pkt) last)))
                   (if (plusp (chaos:pkt-nbytes pkt))
                       (chaos:send-pkt dconn pkt (chaos:pkt-opcode pkt))        ;don't let SEND dft it
                     (chaos:return-pkt pkt))
                   (cond (eofp
                          (if (server-window-read-check cell dconn) (return nil))
                          (chaos:send-pkt dconn (chaos:get-pkt) chaos:eof-op)
                          (%store-conditional celloc 'read nil)
                          (return nil))))))
              (moby-server
               (server-dataproc-hack-moby data handle dconn cell)
               (%store-conditional celloc 'moby-server nil))
              (t (ferror nil "Bogus com-cell value: ~S" (car cell)))))))))

(defun server-disk-full-handler (condition)
  (declare (special server-instance cell1 handle1)
           (unspecial tid))
  (let* ((data (get handle1 server-instance))
         (celloc (locf (car cell1)))
         (tid (server-dataproc-comm-tid data))
         (dconn (server-dataproc-comm-conn data))
         (cconn (server-dataproc-comm-cconn data)))
    (%store-conditional celloc 'write 'async-mark)
    ;; Send an async pkt on the control connection to advertise our woes.
    (let ((pkt (chaos:get-pkt)))
      (chaos:set-pkt-string
        pkt tid " " handle1 " ERROR NMR R " (send condition :report-string))
      (chaos:send-pkt cconn pkt %qfile-asynchronous-mark-opcode))
    ;; Now wait for the control connection to fix us.
    (process-wait "Disk Full"
                  #'(lambda (x) (neq (car x) 'async-mark))
                  cell1)
    (selectq (car cell1)
      (continue    (rplaca cell1 'write)
                   (values :retry-file-operation nil))
      (async-abort (loop for pkt = (chaos:get-next-pkt dconn)
                         as op = (chaos:pkt-opcode pkt)
                         do (chaos:return-pkt pkt)
                         when (= op fs:%qfile-synchronous-mark-opcode)
                         return nil)
                   (rplaca cell1 'nil)
                   (*throw 'async-abort nil))
      (otherwise  (ferror nil "Cell in odd state in async recover - ~S"
                          (car cell1))))))

(defun server-window-write-check (cell dconn val)
  (do-forever
    (if (neq (car cell) val) (return nil))
    (if (chaos:read-pkts dconn) (return t))
    (process-wait "Net In or Cmd" #'(lambda (cell dconn val)
                                 (or (neq (car cell) val)
                                     (chaos:read-pkts dconn)))
                  cell dconn val)))


(defun server-window-read-check (cell dconn &optional (cstate 'read))
  (do-forever
    (if (neq (car cell) cstate) (return t))
    (if (chaos:may-transmit dconn) (return nil))
    (process-wait "Net Out or Cmd" #'(lambda (cell dconn cstate)
                                 (or (neq (car cell) cstate)
                                     (chaos:may-transmit dconn)))
                  cell dconn cstate)))

(defun await-data-process (cell flag)
  (rplaca cell flag)
  (process-wait "Await Data Conn" #'null-car cell))

(defun file-server-continue (fh)
  (let ((data (get fh server-instance)))
    (if (null data)
        (format conn-stream "~A ~A ERROR UFH F Unknown file handle: ~A" tid fh fh)
        (let ((opening (server-dataproc-comm-opening data))
              (cell (server-dataproc-comm-cell data)))
          (cond ((null opening)
                 (format conn-stream "~A ~A ERROR UFH F No opening on handle ~A" tid fh fh))
                ((neq (car cell) 'async-mark)
                 (format conn-stream "~A ~A ERROR LOS F Channel not in async marked state" tid fh))
                (t (%store-conditional (locf (car cell)) 'async-mark 'continue)
                   (format conn-stream "~A ~A CONTINUE" tid fh)))))))

(defun file-server-close-connection (fh)
  (let ((data (get fh server-instance)))
    (if (null data)
        (format conn-stream "~A ~A ERROR UFH F Unknown file handle: ~A" tid fh fh)
        (let ((direction (server-dataproc-comm-iotype data))
              (opening (server-dataproc-comm-opening data))
              (cell (server-dataproc-comm-cell data)))
          (cond ((null opening)
                 (format conn-stream "~A ~A ERROR UFH F No opening on handle ~A" tid fh fh))
                (t
                 (if (eq direction 'input)
                     (rplaca cell 'wsync))
                 (%store-conditional (locf (car cell)) 'async-mark 'async-abort)
                 (cond ((eq opening 'directory)
                        (format conn-stream "~A ~A CLOSE" tid fh))
                       (t
                        (selectq server-protocol-version
                          (0
                           (format conn-stream "~A ~A CLOSE ~D ~A ~D~%~A~%"
                                   tid fh
                                   (send (send opening :truename) :version)
                                   (cv-time (send opening :creation-date))
                                   (send opening :length)
                                   (server-print-pathname (send opening :truename))))
                          (1
                           (format conn-stream "~A ~A CLOSE ~A ~D~%~A~%"
                                   tid fh
                                   (cv-time (send opening :creation-date))
                                   (send opening :length)
                                   (server-print-pathname (send opening :truename)))))))
                 (send conn-stream :force-output)
                 (if (eq direction 'input)
                     (process-wait "Read Finish" #'null-car cell)
                   (process-wait "Write Finish" #'null-car cell))       ;!!
                 (setf (server-dataproc-comm-opening data) nil)
                 (cond ((not (eq opening 'directory))
                        (send opening :close)
                        (setq server-openings (delq opening server-openings))))))))))

;;;; Random commands.

(defun file-server-filepos (fh rest)
  (let ((data (get fh server-instance)))
    (if (null data)
        (format conn-stream "~A ~A ERROR UFH F Unknown file handle: ~A" tid fh fh)
        (let ((direction (server-dataproc-comm-iotype data))
              (opening (server-dataproc-comm-opening data))
              (cell (server-dataproc-comm-cell data)))
          (format conn-stream "~A ~A FILEPOS" tid fh)
          (send conn-stream :force-output)
          (await-data-process cell 'fpsync)
          (send opening :set-pointer (car rest))
          (rplaca cell (if (eq direction 'input) 'read 'write))))))

(defun file-server-delete-multiple-files (fh strings)
  (if fh (format conn-stream "~A  ERROR IRF F Inconsistent command options" tid)
    (let (results)
      (dolist (s strings)
        (let ((path (lmfs-parse-for-server s)))
          (if (errorp path)
              (format conn-stream "~A  ERROR IPS F Syntax error in pathname" tid)
            (push (send path :delete nil) results))))
      (if (some results 'identity)
          (progn (format conn-stream "~A  DELETE-MULTIPLE-FILES~%" tid)
                 (dolist (r (reverse results))
                   (if (errorp r)
                       (format conn-stream "~A  ERROR ~A~%" tid (lmfs-error-string r))
                     (format conn-stream "~%"))))
        (format conn-stream "~A  DELETE-MULTIPLE-FILES" tid)))))

(defun file-server-undelete-multiple-files (fh strings)
  (if fh (format conn-stream "~A  ERROR IRF F Inconsistent command options" tid)
    (let (results)
      (dolist (s strings)
        (let ((path (lmfs-parse-for-server s)))
          (if (errorp path)
              (format conn-stream "~A  ERROR IPS F Syntax error in pathname" tid)
            (push (send path :undelete nil) results))))
      (if (some results 'identity)
          (progn (format conn-stream "~A  UNDELETE-MULTIPLE-FILES~%" tid)
                 (dolist (r (reverse results))
                   (if (errorp r)
                       (format conn-stream "~A  ERROR ~A~%" tid (lmfs-error-string r))
                     (format conn-stream "~%"))))
        (format conn-stream "~A  UNDELETE-MULTIPLE-FILES" tid)))))

(defun file-server-delete (fh strings)
  (cond ((null fh)                              ;must be string, delete random file
         (if (not (= (length strings) 1))
             (format conn-stream "~A  ERROR IRF F Inconsistent command options" tid)
           (let ((path (lmfs-parse-for-server (first strings))))
             (if (errorp path)
                 (format conn-stream "~A  ERROR IPS F Syntax error in pathname" tid)
               (let ((result (send path :delete nil)))
                 (if (errorp result)
                     (format conn-stream "~A  ERROR ~A" tid (lmfs-error-string result))
                   (format conn-stream "~A  DELETE" tid)))))))
        (t                                      ;delete while open
         (if strings
             (format conn-stream "~A ~A ERROR IRF F Inconsistent delete command options"
                     tid fh)
             (let ((data (get fh server-instance)))
               (if (or (null data) (null (server-dataproc-comm-opening data)))
                   (format conn-stream "~A ~A ERROR UFH F No opening for handle ~A"
                           tid fh fh)
                   (progn
                     (send (server-dataproc-comm-opening data) :delete)
                     (format conn-stream "~A ~A DELETE" tid fh))))))))


(defun file-server-directory (fh rest strings &aux data parsed)
  (cond ((or (null fh) (not (= (length strings) 1)))
         (format conn-stream "~A ~A ERROR IRF F Inconsistent DIRECTORY command" tid (or fh "")))
        ((or (null (setq data (get fh server-instance)))
             (not (eq (server-dataproc-comm-iotype data) 'input)))
         (format conn-stream "~A ~A ERROR UFH F Bad file handle for DIRECTORY command" tid fh fh))
;       ((car (server-dataproc-comm-cell data)) ;; your problem
;        (format conn-stream "~A ~A ERROR LCK F File handle ~A busy" tid fh fh))
        ((errorp (setq parsed (lmfs-parse-for-server (car strings))))
         (format conn-stream "~A ~A ERROR IPS F Bad pathname syntax - ~A" tid fh (car strings)))
        (t
         (setf (server-dataproc-comm-arg data) (cons parsed rest))
         (setf (server-dataproc-comm-tid data) tid)
         (setf (server-dataproc-comm-opening data) 'directory)  ;make close work
         (setf (server-dataproc-comm-dinfo data)
               (cons (format nil "~A ~A " tid fh) conn))
         ;;let dataproc do the answerage
         (setf (car (server-dataproc-comm-cell data)) 'directory))))

(defun server-dataproc-hack-directory
       (data handle &aux ok (conn (server-dataproc-comm-conn data))
                            (cell (server-dataproc-comm-cell data)))
  (trap-lossage (error "Directory lister toplevel")
     (let* ((conn-stream (chaos:make-stream conn))
            (arg (server-dataproc-comm-arg data))
            (path (car arg))
            (opts (cdr arg)))
       (let ((dirlist (send path :directory-list (cons :noerror opts)))
             (gopkt (chaos:get-pkt))
             (dinfo (server-dataproc-comm-dinfo data)))
         (cond ((errorp dirlist)
                (chaos:set-pkt-string gopkt (car dinfo)
                                      "ERROR " (lmfs-error-string dirlist)))
               (t
                (chaos:set-pkt-string gopkt (car dinfo) "DIRECTORY")))
         (chaos:send-pkt (cdr dinfo) gopkt)
         (cond ((not (errorp dirlist))
                (server-dirlist-single (cdar dirlist) nil conn-stream)
                (dolist (file (cdr dirlist))
                  (if (server-window-read-check cell conn 'directory) (return nil))
                  (server-dirlist-single (cdr file) (car file) conn-stream))
                (send conn-stream :tyo #/cr)
                (setq ok t))))
       (send conn-stream :force-output)
       (if ok (chaos:send-pkt conn (chaos:get-pkt) chaos:eof-op)))
    (send-data-async-lossage conn "System error during dir list processing" handle)))

(defun server-dataproc-hack-moby
       (data handle conn cell)  data
  (trap-lossage (error "Moby server toplevel")
   (moby-server-toplevel cell conn)     ;defined in moby stuff.
   (chaos:send-pkt conn (chaos:get-pkt) chaos:eof-op))
  (send-data-async-lossage conn "System error during moby serving" handle))

(defun send-data-async-lossage (conn msg handle)
  (let ((pkt (chaos:get-pkt)))
    (chaos:set-pkt-string pkt " " handle " ERROR LOS F " msg)
    (chaos:send-pkt conn pkt fs:%qfile-asynchronous-mark-opcode)))

(defun server-dirlist-single (props pn conn-stream &aux (*print-base* 10.) (*nopoint t))
  (format conn-stream "~%")
  (if pn (let ((fhost (wants-lmfs-pathnames-only)))
           (cond ((not fhost)
                  (format conn-stream "~A~%"
                          (send pn :string-for-host)))
                 ('else
                  (format conn-stream "~A~%"
                          (send (other-guy-pathname pn fhost)
                                :string-for-host))))))
  (tv:doplist (props prop ind)
    (format conn-stream "~A " ind)
    (if (eq ind :settable-properties)
        (loop for x on prop do (princ (car x) conn-stream) (if (cdr x) (tyo #/SP conn-stream)))
        (or (dolist (spec fs:*known-directory-properties*)
              (if (memq ind (cdr spec))
                  (progn
                    (funcall (or (cadar spec) #'princ) prop conn-stream)
                    (return t))))
            (princ prop conn-stream)))
    (format conn-stream "~%")))

(defun file-server-change-props (fh strings)
  (trap-lossage (error "Change properties toplevel")
      (cond ((null fh)
             (if (not (> (length strings) 0))
                 (format conn-stream "~A  ERROR IRF F No pathname given." tid)
               (let ((path (lmfs-parse-for-server (car strings))))
                 (if (errorp path)
                     (format conn-stream "~A  ERROR IPS F Syntax error in supplied path: ~A"
                             tid)
                   (change-props-1 path "" (cdr strings))))))
            (t (let ((data (get fh server-instance)))
                 (if (or (null data) (null (server-dataproc-comm-opening data)))
                     (format conn-stream "~A ~A ERROR UFH F No opening for handle ~A"
                             tid fh fh)
                   (change-props-1 (server-dataproc-comm-opening data) fh strings)))))
    (format conn-stream "~A ~A ERROR SYS F Internal error:~% ~A" tid (or fh "") trap-error)))

(defun change-props-1 (actor fh strings)
  (loop with sym
        for string in strings
        as spacex = (string-search-char #/sp string)
        unless spacex do
        (format conn-stream "~A ~A ERROR STX F Ill formated property spec: ~A" tid fh string)
        nconc (list* (setq sym (si:intern1 (substring string 0 spacex)
                                           si:pkg-keyword-package))
                     (server-convert-known-file-property string (1+ spacex) sym)
                     nil)
        into plist
        finally (trap-lossage (error "Change properties")
                      (let ((m (apply actor :change-properties nil plist)))
                        (if (errorp m)
                            (format conn-stream "~A ~A ERROR LOS F ~A" tid fh m)
                          (format conn-stream "~A ~A CHANGE-PROPERTIES" tid fh)))
                  (format conn-stream "~A ~A ERROR SYS F Internal error:~% ~A"
                          tid fh trap-error))))

(defun server-convert-known-file-property (string index ind)    ;not really general
  (loop for ((fcn) . propnames) in fs:*known-directory-properties*
        if (memq ind propnames)
        return (funcall fcn string index)
        finally (return (substring string index nil))))

(defun file-server-rename (fh strings)
  (cond ((null fh)                              ;must be string, delete random file
         (if (not (= (length strings) 2))
             (format conn-stream "~A  ERROR IRF F Inconsistent RENAME command options" tid)
           (let ((path1 (lmfs-parse-for-server (first strings)))
                 (path2 (lmfs-parse-for-server (second strings))))
             (if (errorp path1)
                 (format conn-stream "~A  ERROR IPS F Syntax error in pathname" tid)
               (if (errorp path2)
                   (format conn-stream "~A  ERROR IPS F Syntax error in rename pathname" tid)
                 (trap-lossage (error "Rename 2 args")
                       (progn
                         (if (null (send path1 :version))
                             (setq path1 (send path1 :new-version :newest)))
                         (if (null (send path2 :version))
                             (setq path2 (send path2 :new-version :newest)))
                         (let ((result (send path1 :rename path2 nil)))
                           (if (errorp result)
                               (format conn-stream "~A  ERROR ~A" tid (lmfs-error-string result))
                             (format conn-stream "~A  RENAME" tid))))
                       (format conn-stream "~A  ERROR SYS F System error renaming" tid)))))))
        (t                                      ;rename while open
         (if (not (= (length strings) 1))
             (format conn-stream "~A ~A ERROR IRF F Inconsistent rename command options"
                     tid fh)
           (let ((path (lmfs-parse-for-server (first strings))))
             (if (errorp path)
                 (format conn-stream "~A ~A ERROR IPS F Syntax error in pathname" tid fh)
               (let* ((data (get fh server-instance))
                      (opening (server-dataproc-comm-opening data)))
                 (if (or (null data)
                         (null opening)
                         (symbolp opening))     ;yes, I know NIL is a symbol, thx
                     (format conn-stream "~A ~A ERROR UFH F No opening for handle ~A"
                             tid fh fh)
                   (trap-lossage (error "Rename while open")
                         (progn
                           (if (null (send path :version))
                               (setq path (send path :new-version :newest)))
                           (let ((result (send opening :rename path nil)))
                             (if (errorp result)
                                 (format conn-stream "~A ~A ERROR ~A" tid fh
                                         (lmfs-error-string result))
                               (format conn-stream "~A ~A RENAME~%~A"
                                       tid fh
                                       (server-print-pathname (send opening :truename))))))
                       (format conn-stream "~A ~A ERROR SYS F System error while renaming"
                               tid fh))))))))))


(defun file-server-expunge (fh strings &aux path result)
  (cond (fh
         (format conn-stream "~A ~A ERROR IRF File handle given in EXPUNGE command." tid fh))
        ((null strings)
         (format conn-stream "~A  ERROR IRF F No pathname given to EXPUNGE command." tid))
        ((cdr strings)
         (format conn-stream "~A  ERROR IRF F Extra junk given to EXPUNGE command." tid))
        ((errorp (setq path (lmfs-parse-for-server (first strings))))
         (format conn-stream "~A  ERROR IPS F Syntax error in pathname" tid))
        ((errorp (setq result (send (send path :new-pathname
                                          :name :unspecific
                                          :type :unspecific
                                          :version :unspecific)
                                    :expunge :error nil)))
         (format conn-stream "~A  ERROR ~A" tid (lmfs-error-string result)))
        (t (format conn-stream "~A  EXPUNGE ~D" tid result))))

(defun file-server-create-directory (fh strings &aux path result)
  (cond (fh
         (format conn-stream "~A ~A ERROR IRF File handle given in CREATE-DIRECTORY command."
                 tid fh))
        ((null strings)
         (format conn-stream "~A  ERROR IRF F No pathname given to CREATE-DIRECTORY command." tid))
        ((cdr strings)
         (format conn-stream "~A  ERROR IRF F Extra junk given to CREATE-DIRECTORY command." tid))
        ((errorp (setq path (lmfs-parse-for-server (first strings))))
         (format conn-stream "~A  ERROR IPS F Syntax error in pathname" tid))
        ((errorp (setq result (send path :create-directory :error nil)))
         (format conn-stream "~A  ERROR ~A" tid (lmfs-error-string result)))
        (t (format conn-stream "~A  CREATE-DIRECTORY ~D" tid result))))

(defun file-server-create-link (fh strings &aux path path2 result)
  (cond (fh
         (format conn-stream "~A ~A ERROR IRF File handle given in CREATE-LINK command."
                 tid fh))
        ((null (second strings))
         (format conn-stream "~A  ERROR IRF F Insufficient arguments given to CREATE-LINK command."
                 tid))
        ((cddr strings)
         (format conn-stream "~A  ERROR IRF F Extra junk given to CREATE-LINK command." tid))
        ((errorp (setq path (lmfs-parse-for-server (first strings))))
         (format conn-stream "~A  ERROR IPS F Syntax error in pathname" tid))
        ((errorp (setq path2 (lmfs-parse-for-server (second strings))))
         (format conn-stream "~A  ERROR IPS F Syntax error in pathname" tid))
        ((errorp (setq result (send path :create-link path2 :error nil)))
         (format conn-stream "~A  ERROR ~A" tid (lmfs-error-string result)))
        (t (format conn-stream "~A  CREATE-LINK ~D" tid result))))

(defun file-server-complete (fh args strings &aux path result success)
  (cond (fh
         (format conn-stream "~A ~A ERROR IRF File handle given to COMPLETE command." tid fh))
        ((not (= (length strings) 2))
         (format conn-stream "~A  ERROR IRF Wrong number of strings given in COMPLETE command."
                 tid))
;       ((errorp (setq path (lmfs-parse-for-server (first strings))))
;        (format conn-stream "~A  ERROR IPS F Syntax error in pathname." tid))
        (t (setq path (lmfs-parse-for-server (first strings)))
           ;;string result means an error
           (if (errorp path)                    ;ZMACS will supply semibogus paths....!!
               (setq path (fs:user-homedir si:local-host)))
           (multiple-value (result success)
             (condition-case (result)
                 (send path :complete-string (second strings)
                       (list*
                         (if (memq :write args) :write :read)
                         (if (memq :new-ok args) :new-ok :old)
                         (if (memq :deleted args) '(:deleted))))
               (error
                (second strings))))
           (let ((x (and result (string-search-char #/: result))))      ;strip out host
             (if x (setq result (substring result (1+ x)))))
           (format conn-stream "~A  COMPLETE ~A~%~A~%" tid success result))))

(defun print-server-login-exegesis ()
  (tv:notify
    nil
    "This machine has been invoked as a remote file server, but is otherwise free."))


(defun print-server-lossages (&optional toggle-debug-switch?)
  "Print out server lossage info to *standard-output*.
If toggle-debug-switch? is non-nil, debugging is toggled on or off."
  (cond (toggle-debug-switch?
         (setq lmfs-debug-server (not lmfs-debug-server))
         (format t "~&Debugging of server turned ~A" (if lmfs-debug-server "ON" "OFF"))))
  (if (null lmfs-server-lossages)
      (format t "~&No lossages.")
    (loop for (time key err-msg-info host) in lmfs-server-lossages
          doing
          (format t "~&~A  ~A~@[  ~S~]~%~5T~S" time key host err-msg-info)
          (when (send *standard-input* :listen)
            (let ((c (send *standard-input* :tyi)))
              (or (= c #/sp) (send *standard-input* :untyi c)))
            (return nil)))))

(defun file-server-shutdown (message &optional (in-minutes 5))
  (setq *server-shutdown-message* message
        *server-shutdown-time* (+ (time:get-universal-time) (* in-minutes 60.)))
  (process-run-function "Server shutdown" 'file-server-shutdown-1
                                  message (time-increment (time) (* in-minutes 3600.))))

(defun file-server-cancel-shutdown (&optional (key :cancel))
  (let ((sdval *server-shutdown-message*))
    (or
      (null sdval)
      (%store-conditional (locf *server-shutdown-message*) sdval key))))

(defun file-server-reschedule-shutdown (message &optional (in-minutes 5))
  (file-server-cancel-shutdown :reschedule)
  (process-wait "Unshut" #'null-car (locf *server-shutdown-message*))
  (file-server-shutdown message in-minutes))

(defun file-server-shutdown-state ()
  (values *server-shutdown-message* *server-shutdown-time*))

(defvar *server-notify-times* '(60. 30. 15. 5. 2. 1. 0.))

(defun file-server-shutdown-1 (message at-time)
  (loop with time-to-go and notify-interval
        do (if (time-lessp at-time (time))      ;Already passed
               (setq time-to-go nil
                     notify-interval nil)
               (setq time-to-go (time-difference at-time (time))
                     notify-interval (loop for times on *server-notify-times*
                                           when ( (* (car times) 3600.) time-to-go)
                                           return times)))
        unless (eq message *server-shutdown-message*)
        do (cond ((eq  *server-shutdown-message* :reschedule)
                  (blast-message-to-file-servers "File Server shutdown rescheduled"))
                 (t (tv:notify nil "File Server shutdown cancelled.")
                    (blast-message-to-file-servers "File Server shutdown cancelled.")))
           (setq *server-shutdown-message* nil)
           (return :cancelled)
        unless (eq notify-interval *server-notify-times*)
        do (let* ((minutes-to-go (and time-to-go (round time-to-go 3600.)))
                  (current-message (format nil "File server shutting down~
                                                ~@[ in ~D minute~:P~] - ~A"
                                           minutes-to-go message)))
             (tv:notify nil "~A" current-message)
             (cond ((null minutes-to-go)
                    (setq chaos:chaos-servers-enabled nil)
                    (tv:close-all-servers current-message)
                    (setq *server-shutdown-message* nil)
                    (return t))
                   (t (blast-message-to-file-servers current-message))))
        do (process-wait "Shutdown Msg"
                         #'(lambda (until-time mval)
                             (or (neq *server-shutdown-message* mval)
                                 (time-lessp until-time (time))))
                         (time-increment at-time (* (car notify-interval) -3600.))
                         message)))

(defun send-single-shutdown-message (conn)
  (let ((pkt (chaos:get-pkt)))
    (chaos:set-pkt-string pkt (format nil "File Server shutting down in ~D minute~:P - ~A"
                                      (round (- *server-shutdown-time*
                                                (time:get-universal-time))
                                             60.)
                                      *server-shutdown-message*))
    (chaos:send-pkt conn pkt fs:%qfile-notification-opcode)))

(defun blast-message-to-file-servers (message)
  (loop for server in (send tv:who-line-file-state-sheet :servers)
        when (equalp (tv:server-desc-contact-name server) "FILE")
        do (let ((pkt (chaos:get-pkt)))
             (chaos:set-pkt-string pkt message)
             (chaos:send-pkt (tv:server-desc-connection server)
                             pkt fs:%qfile-notification-opcode))))

(defvar *chaos-file-server-notifier-sleep-time* 300. "Sleep time in seconds")

(defun chaos-file-server-notifier-top-level ()
  (let ((notifiers ()))
    (dolist (flavor (send si:local-host :appropriate-access-flavors))
      (let ((function (get flavor 'notification-for-server)))
        (when function (push function notifiers))))
    (loop
      (dolist (notifier notifiers)
        (let ((message (funcall notifier)))
          (when message
            (blast-message-to-file-servers message))))
      (sleep *chaos-file-server-notifier-sleep-time* "QFILE Server Notifier"))))

(add-initialization "QFILE Server Notifier"
                    '(process-run-function '(:name "QFILE Server Notifier" :restart-after-boot t
                                             :priority -2)
                                           'chaos-file-server-notifier-top-level)
                    '(:enable-services))

(defun lmfs-peek-server (sg)
  (let ((itag (symeval-in-stack-group 'server-instance sg)))
    (list '()
          (tv:scroll-parse-item
            "    User: "
            `(:function ,#'symeval-in-stack-group (user-id ,sg) 15.)
            "    Server Tag: "
            (string itag))
          (tv:scroll-maintain-list
            `(lambda () (symeval-in-stack-group 'alldatas ',sg))
            `(lambda (x) (lmfs-peek-data-process
                           (server-dataproc-comm-cell x) ',itag))))))

(defun lmfs-peek-data-process (cell itag)
  (let* ((handle (cdr cell))
         (data (get handle itag)))
    (cond ((null data)
           (tv:scroll-parse-item
                        (format nil "      Vanished process ~A, instance ~A." handle itag)))
          (t (let* ((sib (server-dataproc-comm-sibling data))
                    (sibdata (get sib itag)))
               (list ()
                     (lmfs-peek-data-process-half data)
                     (lmfs-peek-data-process-half sibdata)))))))

(defun lmfs-peek-data-process-half (data)
  (let ((process (server-dataproc-comm-data-proc data))
        (cell (server-dataproc-comm-cell data)))
    (list (list :pre-process-function 'lmfs-peek-server-preprocess
                'lmfs-cdata data 'cur-display (ncons nil))
          (tv:scroll-parse-item
            "      "
            `(:mouse
              (nil :eval (tv:peek-process-menu ',process)
                   :documentation
                   "Menu of useful things to do to this process.")
              :string
              ,(format nil "~A" (process-name process)))
            "    "
            `(:function ,#'tv:peek-whostate ,(ncons process))
            ", sibling "
            (string (server-dataproc-comm-sibling data))
            (format nil ", ~A" (server-dataproc-comm-iotype data))
            ", cmd: "
            `(:function ,#'(lambda (x) (or (car x) "(Idle)")) (,cell)))
          nil)))

(defun lmfs-peek-server-preprocess (list-item)
  (let* ((plist (locf (first list-item)))
         (data (get plist 'lmfs-cdata))
         (cell (get plist 'cur-display))
         (curdisp (car cell))
         (opening (server-dataproc-comm-opening data)))
    (cond ((eq curdisp opening))
          ((null opening) (setf (third list-item) nil))
          ((eq opening 'directory)
           (rplaca cell opening)
           (setf (third list-item) (tv:scroll-parse-item "         Directory state.")))
          ((setf (third list-item) (send opening :peek-file-system 9))
           (rplaca cell opening)))))

(add-initialization "FILE" '(PROCESS-RUN-FUNCTION "File Server" 'file-server)
                    nil 'chaos:server-alist)


;; if you want to go over to a server and use it interactively you might
;; want to (set-server-priority -1)

(defun set-server-priority (priority)
  (if (plusp priority) (ferror "You don't want positive priority."))
  (dolist (p si:all-processes)
    (when (string-search "File Server" (send p :name))
      (print p)
      (send p :set-priority priority))))

(defvar *disk-space-warner-interval* 10. "Interval for warning, in minutes")
(defvar *disk-space-warner-threshold* 800. "Warning level for disk space")
(defvar *disk-space-warner-process* () "The actual process doing the work")

(defun disk-space-warner-function ()
  (do-forever
    (process-wait "disk space watch"
                  #'(lambda ()
                      (< (aref put-usage-array put-free) *disk-space-warner-threshold*)))
    (let ((message (format () "Disk space low !  Only ~D blocks free."
                           (aref put-usage-array put-free))))
      (ignore-errors
        (tv:notify () "~A" message)
        (blast-message-to-file-servers message)))
    (process-sleep (* 60. 60. *disk-space-warner-interval*))))

(defun start-disk-space-warner-process ()
  (or *disk-space-warner-process*
      (setq *disk-space-warner-process* (make-process "Disk Space Watch" :priority -2)))
  (SEND *disk-space-warner-process* :PRESET 'DISK-SPACE-WARNER-FUNCTION)
  (PROCESS-RESET-AND-ENABLE *disk-space-warner-process*))
