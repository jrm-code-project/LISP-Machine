; -*- Mode:LISP; Package:STEVE; Readtable:CL; Base:10 -*-

;Copyright (c) May 1983 by Christopher Eliot
; and Massachusetts Institute of Technology.
;Permission to copy all or part of this material is granted, provided
; that the copies are not made or distributed for resale, the MIT
; copyright notice and reference to the source file and the software
; distribution version appear, and that notice is given that copying
; is by permission of Massachusetts Institute of Technology.

(bindmetax evaluate evaluate)

(defun evaluate (&optional (form nil) values)
  (with-no-passall
   (setq form (if (null form)
                  (mx-prompter #'read "Evaluate: ")
                  (with-input-from-string (stream form)
                    (read stream))))
   (setq values (multiple-value-list (eval form)))
   (setq *** ** ** *)
   (setq +++ ++ ++ +)
   (setq * (car values))
   (setq + form)
   (mapc #'(lambda (x) (print x echo-area-window)) values)
   '*))

(bindmetax print-into-buffer print-into-buffer)

(defun print-into-buffer (&optional (form nil) values)
  (with-no-passall
   (setq form (if (null form)
                  (mx-prompter #'read "Evaluate and Print: ")
                  (with-input-from-string (stream form)
                    (read stream))))
   (let ((stream (make-buffer-stream *editor-cursor*)))
     (let ((standard-output stream)
           (trace-output stream)
           (error-output stream))
       (setq values (multiple-value-list (eval form)))
       (setq *** ** ** *)
       (setq +++ ++ ++ +)
       (setq * (car values))
       (setq + form)
       (mapc #'(lambda (x) (print x stream)) values)
       (terpri stream)))
   '*))

(editor-defun-key '(#\control-x #\altmode) re-execute-minibuffer
  (let ((values))
    (setq values (multiple-value-list (eval +)))
    (mapc #'(lambda (x) (print x echo-area-window)) values)
    nil))

(defun zap-to-lisp ()
  (let ((mark (send *editor-cursor* :get-mark)))
    (find-beginning-of-defun *editor-cursor*)
    (with-no-passall
     (print (eval (read (make-buffer-stream *editor-cursor*)))
            echo-area-window))
    (send *editor-cursor* :goto-mark mark))
  nil)

(editor-bind-key #\meta-z zap-to-lisp)

(bindmetax insert-file insert-file)

(defun insert-file
       (&optional (file (mx-prompter #'read-file-name "Insert File: ")))
  (send terminal-io :send-if-handles :set-device-mode :passall nil)
  (setq file (editor-default file))
  (with-open-file (infile file :direction :in :noerror t)
    (when (stringp infile)
      (ed-lose infile))
    (auto-push-mark *editor-cursor*)
    (insert-stream infile)))

(bindmetax insert-buffer insert-buffer)

(defun insert-buffer (&optional (name (mx-prompter #'read-buffer-name
                                                        "Insert Buffer: ")))
  (send terminal-io :send-if-handles :set-device-mode :passall nil)
  (let ((buffer (buffer name :create nil)))
    (when (null buffer)
      (ed-lose "~a is not a buffer" name))
    (auto-push-mark *editor-cursor*)
    (insert-stream (make-buffer-stream
                    (make-bp buffer (buffer-content buffer) 0)))))

(defun insert-stream (stream)
  (send (bp-line *editor-cursor*) :break (bp-position *editor-cursor*))
  (send *editor-cursor* :to-end-of-previous-line)
  (loop with string = nil and eofp = nil and line = (bp-line *editor-cursor*)
        do (multiple-value (string eofp)
             (send stream :readline ""))
        until (not (null eofp))
        do (setq line (send line :insert-line string))
        finally (when (not (null string)) ;If file does not end with #\RETURN.
                  (setq line (send line :insert-line string)))
        finally (when (line-next line)
                  (send (line-next line) :delete-line-separator))))

(bindmetax compile-sexp compile-sexp)

(defun compile-sexp ()
  (with-notify-line
   (with-no-passall
    (format terminal-io "Compiling Defun...~%")
    (let ((mark (send *editor-cursor* :get-mark))
          (form))
      (find-beginning-of-defun *editor-cursor*)
      (setq form (read (make-buffer-stream *editor-cursor*)))
      (send *editor-cursor* :goto-mark mark)
      (let ((standard-output overwrite-stream)
            (error-output overwrite-stream)
            (trace-output overwrite-stream)
            (compiler-output overwrite-stream)
            (compile-function
             (or (and (buffer-file-name *editor-buffer*)
                      (cadr (assoc (send (buffer-file-name *editor-buffer*)
                                         :type)
                                   *incremental-compiling-function*)))
                 'compile)))
        (funcall compile-function form))
      (setq creamed-tty-lines-to (max& creamed-tty-lines-to
                                       (1+& (send terminal-io :linenum)))))))
  nil)

(editor-bind-key #\control-meta-c compile-sexp)

(bindmetax trace-current-defun trace-current-defun)

(defun trace-current-defun (&optional (function nil))
 (cond ((not (null function))
        (with-input-from-string (stream function)
         (eval (list 'trace (read stream)))))
       (t (find-beginning-of-defun *editor-cursor*)
          (let ((stream (make-buffer-stream *editor-cursor*)))
           (send stream :read-char)
           (read stream)
           (eval (list 'trace (read stream)))))))

(bindmetax save-all-files save-all-files)

(defun save-all-files (&aux (yes (not (argument?))))
  (overwrite-start)
  (loop for buffer in *all-buffers*
        if (when (buffer-modified? buffer)
             (format overwrite-stream "~&Save buffer ~a into file ~a? "
                     (buffer-name buffer)
                     (send (buffer-file-name buffer) :string-for-editor))
             (and (ed-y-or-n-p "Type Y to save buffer ~a N not to."
                               (buffer-name buffer))
                  (cond (yes t) (t (save-file buffer) nil))))
        collect buffer into save-em
        finally (when yes (loop for b in save-em do (save-file b)))))

(bindmetax kill-buffer kill-buffer)

(defun kill-buffer (&optional (name (mx-prompter #'read-buffer-name
                                                      "Kill Buffer: ")))
  (kill-one-buffer (if (string= name "")
                       *editor-buffer*
                       (or (buffer name :create nil)
                           (ed-lose "~a not a buffer" name))))
  nil)

(editor-bind-key '(#\control-x #\K) kill-buffer)

(defun %kill-buffer-primitive (buffer)
  ;;This does not guarantee much.
  (when (null (cdr *all-buffers*))      ; ** This is a hack **
    (point (string (gentemp "Kill-this-dummy-buffer-")) :create t))
  (setq *all-buffers* (delete buffer *all-buffers*))
  (loop for point in *all-edit-cursors*
        if (eq (edit-cursor-buffer point) buffer)
        do (setq *all-edit-cursors* (delete point *all-edit-cursors*)))
  ;; If the buffer is the current buffer, then select any other buffer.
  ;; This should be chosen on the basis of the history of which
  ;; buffers were selected, but instead we will hack it to
  ;; choose from the history of how buffers were created.
  (when (eq buffer *editor-buffer*)
    (select-point-in-current-window (car *all-edit-cursors*)))
  ;; The select-point function will put the buffer we are trying to kill
  ;; in only one place,(as we can verify by looking at its definition),
  ;; which is the variable *last-buffer-selected*.  This should be chosen
  ;; from the history list of selected buffers also, but that is not
  ;; available, so we will instead choose the handiest legal thing:
  (when (eq buffer *last-buffer-selected*)
    (setq *last-buffer-selected* *editor-buffer*))
  ;; At this point there should be no pointers to the killed buffer left.
  ;; Given these fixes this function is now less of a dangerous
  ;; primitive than it used to be.
  )


(defun kill-one-buffer (buffer)
  (when (null (cdr *all-buffers*)) (ed-lose "Can't Kill Only Buffer"))
  (when (buffer-modified? buffer)
    (with-query-line
     (format terminal-io "Save changes to ~a? " (buffer-name buffer))
     (when (ed-y-or-n-p "Type Y to save changes to ~a" (buffer-name buffer))
       (save-file buffer))))
  (when (eq buffer *editor-buffer*)
    (let ((b (if (eq (car *all-buffers*) buffer) (cadr *all-buffers*)
                 (car *all-buffers*))))
      (with-query-line
        (format terminal-io
                "Killing current buffer. Select which other buffer (~A): "
                (buffer-name b))
        (select-buffer-named (prescan #'read-buffer-name nil) b))))
  (%kill-buffer-primitive buffer))

(bindmetax kill-some-buffers kill-some-buffers)

(defun kill-some-buffers ()
  (overwrite-start)
  (loop for buffer in *all-buffers*
        do (if (buffer-modified? buffer)
               (format overwrite-stream
                       "~&Buffer ~a has been modified. Kill it? "
                       (buffer-name buffer))
               (format overwrite-stream
                       "~&Buffer ~a is unmodifed. Kill it? "
                       (buffer-name buffer)))
        if (ed-y-or-n-p "Type Y to Kill buffer ~a (~a)"
                        (buffer-name buffer)
                        (if (buffer-modified? buffer) "Modified"
                            "unmodified"))
        do (kill-one-buffer buffer)))

(defun compile-this-file (&optional (filename nil))
  (send terminal-io :set-device-mode :passall nil)
  (save-all-files)
  (with-notify-line
   (oustr "Compiling..." terminal-io)
   (setq filename (if (null filename)
                      (buffer-file-name *editor-buffer*)
                      (pathname filename)))
   (let ((compile-function (or (cadr (assoc (send filename :type)
                                            *compiling-function*))
                               #'compile-file)))
     (funcall compile-function filename)))
  (with-query-line
   (oustr "Load Compiled File? " terminal-io)
   (when (ed-y-or-n-p "Type Y to load compiled file")
     (load (send filename :new-type "VASL")))))

(bindmetax compile compile-this-file)

(bindmetax help-meta-x-commands help-meta-x-commands)

(defun help-meta-x-commands ()
  (overwrite-start)
  (loop for (command function)
        in (cdr (command-table-alist meta-x-command-table))
        do (format overwrite-stream "~&M-X ~a" command))
  (more-aborter "*done*"))

;Should passall mode be turned off?

(bindmetax query-replace query-replace)

(defun query-replace
       (&optional (search (mx-prompter #'readline "Search (Query Replace): "))
                  (replace (mx-prompter #'readline "Replace ~s by: " search)))
  (with-notify-line
   (oustr "Query Replace" terminal-io)
   (catch 'unwind-protect-is-not-finished
     (loop with chr = nil
           with string-length = (string-length search)
           with chr-read-as-if = nil
           with retry-current-position = nil
           for set-sgr = nil
           while (or retry-current-position
                     (catch 'search-fails (search-forward search *editor-cursor*)))
           do (setq retry-current-position nil)
           do (make-screen-image *editor-cursor*)
           if (null (type-ahead-p))
           do (setq set-sgr (not (eq :no (get :standout 'graphic-rendition-p))))
           do (unwind-protect
               (progn
                (when (not (null set-sgr))
                  (set-string-graphic-rendition
                   :standout t *editor-cursor* (-& string-length)))
                (send (edit-cursor-window *editor-cursor*) :make-cursor-visible)
                (or chr-read-as-if (setq chr (send terminal-io :read-char)))
                (case chr
                  (#\? (with-notify-line-remaining
                        (oustr "Implemented RUBOUT, COMMA, ALTMODE, PERIOD, !"
                               terminal-io))
                       (setq retry-current-position t))
                  (#\^R (setq retry-current-position t)
                        (unless (recursive-edit "Query Replace")
                          (ed-warn "Aborted Query Replace"))
                        (setup-mode-area))
                  (#\rubout nil)
                  (#\altmode (throw 'unwind-protect-is-not-finished t)
                             ;;(return t)
                             )
                  ((#\space #\. #\, #\!)
                   ;;The graphic rendition must be fixed before changing the
                   ;;buffer because otherwise the character counts may not be =.
                   (when (not (null set-sgr))
                     (set-string-graphic-rendition
                      :standout nil *editor-cursor* (-& string-length))
                     (setq set-sgr nil))
                   (send *editor-cursor* :delete-characters (-& string-length))
                   (send (bp-line *editor-cursor*)
                         :insert-string (bp-position *editor-cursor*) replace)
                   (case chr
                     (#\. (throw 'unwind-protect-is-not-finished t)
                          ;;(return t)
                          )
                     (#\, (make-screen-image *editor-cursor*)
                          (set-string-graphic-rendition
                           :standout t *editor-cursor*
                           (-& (string-length replace)))
                          (send terminal-io :read-char)
                          (set-string-graphic-rendition
                           :standout nil *editor-cursor*
                           (-& (string-length replace)))
                          nil)
                     (#\! (setq chr-read-as-if #\space) nil)))
                  (t (send terminal-io :unread-char chr)
                     (throw 'unwind-protect-is-not-finished t)
                     ;;(return t)
                     )))
               (when (not (null set-sgr))
                 (set-string-graphic-rendition
                  :standout nil *editor-cursor* (-& string-length))))))))

(defun set-string-graphic-rendition (rend value point length)
  (unless (eq :no (get rend 'graphic-rendition-p))
    (multiple-value-bind (x y) (real-x-y-position point)
      (let ((mark (send point :get-mark)))
        (send point :advance-pos-no-error length)
        (multiple-value-bind (x1 y1) (real-x-y-position point)
          (send point :goto-mark mark)
          (if (or (>& y y1) (and (=& y y1) (>=& x x1)))
              (set-graphic-rendition-between rend value x1 y1 x y)
              (set-graphic-rendition-between rend value x y x1 y1)))))))

(defun set-graphic-rendition-between (rend value x1 y1 x2 y2)
  (when (null x1) (setq x1 0 y1 0))
  (when (null x2) (setq x2 *tty-width* y2 *tty-height*))
  (setq x1 (min& *tty-width* (max& 0 x1))
        x2 (min& *tty-width* (max& 0 x2))
        y1 (min& *last-buffer-line* (max& 0 y1))
        y2 (min& *last-buffer-line* (max& 0 y2)))
  (cond ((send terminal-io :send-if-handles rend value)
         (loop for xs first x1 then 0
               for y from y1 to y2
               with xl = (1-& *tty-width*)
               if (=& y y2) do (setq xl x2)
               do (cursorpos y xs terminal-io)
               do (send terminal-io :raw-oustr (aref old-screen-image y)
                        xs (-& xl xs))
               do (send terminal-io :set-cursor-fried-flag t))
         (send terminal-io rend nil)) ;value is T.
        (t (putprop rend :no 'graphic-rendition-p) nil)))

(bindmetax underline-region underline-region)

(defun underline-region ()
  (multiple-value-bind (a b c) (count-between *editor-cursor* (get-mark))
    (when (null c) (setq b (-& b)))
    (set-string-graphic-rendition :underline t *editor-cursor* b)
    (when (char= (send terminal-io :peek-char) #\space)
      (send terminal-io :read-char))
    (set-string-graphic-rendition :underline nil *editor-cursor* b)))

(bindmetax replace replace-strings)

(defun replace-strings
  (&optional (search (mx-prompter #'readline "Search: "))
    (replace (mx-prompter #'readline "Replace ~s by: " search)))
  ;Should passall mode be turned off?
  (loop with string-length = (string-length search)
        while (catch 'search-fails (search-forward search *editor-cursor*))
        do (send *editor-cursor* :delete-characters (-& string-length))
        do (send (bp-line *editor-cursor*)
                 :insert-string (bp-position *editor-cursor*) replace)))

(bindmetax set-visited-filename set-visited-filename)

(defun set-visited-filename
  (&optional (name (mx-prompter #'read-file-name "New Visited Name: ")))
  (send *editor-buffer* :set-file-name (editor-default (pathname name)))
  (setq *last-major-mode* "No Mode") ;Force call to setup-mode-area.
  )

(bindmetax rename-buffer rename-buffer)

(defun rename-buffer (&optional (name (mx-prompter #'read-buffer-name
                                                        "New Buffer Name: ")))
  (when (buffer name :create nil)
    (ed-lose "A buffer by that name already exists"))
  (setf (buffer-name *editor-buffer*) name)
  (setq *last-major-mode* "No Mode"))




(editor-defun-key '(#\control-x #\control-d) directory-display
  (let ((path-def (buffer-file-name *editor-buffer*))
        (path))
    ;;Delete type so all will be selected.
    (setq path-def (send path-def :new-type :wild))
    (setq path path-def)
    (when (or argument-supplied? (not (=& 1 *argument*)))
      (with-query-line
       (oustr "Directory: " terminal-io)
       (setq path (pathname (read-file-name)))))
    (when (null (send path :device))
      (setq path (send path :new-device (send path-def :device))))
    (when (null (send path :directory))
      (setq path (send path :new-directory (send path-def :directory))))
    (when (null (send path :name))
      (setq path (send path :new-name (send path-def :name))))
    (overwrite-start)
    (princ (send (make-pathname :host (send path :host)
                                :device (send path :device)
                                :directory (send path :directory))
                 :string-for-printing)
           overwrite-stream)
    (terpri overwrite-stream)
    (*catch 'directory-display
      (let ((month-names (time:mode-language-fetch
                           :short time:*month-strings* "month name")))
        (declare (special month-names))
        (mapdirectory #'(lambda (dirinfo)
                          (oustr (directory-display-line-string
                                  dirinfo month-names)
                                 overwrite-stream)
                          (terpri overwrite-stream)
                          (when (listen terminal-io)
                            (*throw 'directory-display nil)))
                      path '(:creation-date :protection :author :blocks))
        (princ "Done" overwrite-stream)))
    (more-aborter "*done*")
    nil))


(defparameter *two-digit-integers*
   #.(loop with v = (make-vector 100)
           for i from 0 below 100
           do (setf (aref v i) (format nil "~2,'0d" i))
           finally (return v)))

(defun directory-display-line-string (dirinfo month-names)
  (multiple-value-bind (seconds minutes hours day month year)
    (decode-universal-time (get dirinfo :creation-date))
    (let ((name (send (car dirinfo) :file-namestring))
          (blks (let ((b (get dirinfo :blocks)))
                  (cond ((null b) "-")
                        ((<& b 10) (string (%digit-weight-to-char b)))
                        ((<& b 100) (svref *two-digit-integers* b))
                        (t (format nil "~D" b))))))
      (string-append
         "  " name
         ;9 chars name, 3 ext, 5 version, 2 punctuation = 19 max.
         (make-string
           (max& 1 (-& 28 (string-length name) (string-length blks)))
           :initial-element #\space)
         blks "  "
         (svref *two-digit-integers* day) "-" (svref month-names (1-& month))
         "-" (svref *two-digit-integers* (\\& year 100)) " "
         (svref *two-digit-integers* hours) ":"
         (svref *two-digit-integers* minutes) " "
         (get dirinfo :author) " "
         (fs:vms-directory-info-protection-string
           (get dirinfo :protection))))))


(bindmetax overwrite-mode overwrite-mode)

(defun overwrite-mode ()
  (bind-in-current-buffer '*overwrite-mode* (not *overwrite-mode*)))

(bindmetax set-key set-key)

(defun set-key (&optional (key nil) (binding nil))
  (cond ((null key)
         (with-query-line-remaining
          (oustr "Set Key: " terminal-io)
          (setq key (read-any-key))))
        (t (setq key (with-input-from-string (stream key)
                       (read stream)))))
  (when (cond ((consp key) (memq #\bell key))
              ((char= key #\bell)))
    (oustr "Really redefine the abort key (which may not work)? " terminal-io)
    (unless (ed-y-or-n-p "Type Y to Redefine the Abort key")
      (ed-warn "Aborted")))
  (setq binding
        (if (null binding)
            (mx-prompter #'read "Binding For ~s (~a): "
                         key (internal-editor-lookup-key key))
            (with-input-from-string (stream binding)
              (read stream))))
  (internal-bindery key binding)
  (with-notify-line-remaining
   (format terminal-io "Binding ~s to ~a" key binding)))

(bindmetax set-variable set-variable)

(defun set-variable (&optional (var nil) (value nil))
  (setq var (if (null var)
                (mx-prompter #'read-symbol "Variable: ")
                (with-input-from-string (stream var)
                  (read-symbol stream))))
  (setq value
        (if (null value)
            (mx-prompter #'read "Value for ~a (~a): " var
                         (cond ((boundp var) (symeval var))
                               (t "<Unbound>")))
            (with-input-from-string (stream value)
              (read stream))))
  (set var (eval value)))

(bindmetax kill-variable kill-variable)

(defun kill-variable (&optional (var nil))
  (setq var
        (if (null var)
            (mx-prompter #'read-symbol "Kill Variable: ")
            (with-input-from-string (stream var)
              (read-symbol stream))))
  (makunbound var))

(bindmetax view-buffer view-buffer)

(defun view-buffer (&optional (name (mx-prompter #'read-buffer-name
                                                      "View Buffer: ")))
  (let ((buffer (buffer name :create nil)))
    (when (null buffer) (ed-lose "No Such Buffer"))
    (overwrite-start)
    (loop for line first (buffer-content buffer) then (line-next line)
          until (null line)
          do (send terminal-io
                   :oustr (line-chars line) 0 (line-char-count line))
          do (overwrite-terpri))
    (overwrite-done)
    (more-aborter "*done*")))

(bindmetax view-file view-file)

(defun view-file (&optional (name (mx-prompter #'read-file-name
                                                    "View File: ")))
  (with-notify-line-remaining
   (oustr "Note: View File wastes memory" terminal-io))
  (let ((stream nil))
    (unwind-protect
      (progn
        (setq stream (send (editor-default name) :open-for-editor :read))
        (unless (streamp stream) (ed-lose stream))
        (overwrite-start)
        (loop as string = (send stream :next-record) while string
              do (oustr string terminal-io)
              do (overwrite-terpri))
        (send stream :close)
        (overwrite-done))
      (when (streamp stream) (send stream :close))))
  (more-aborter "*done*"))

(bindmetax write-region write-region)

(defun write-region (&optional (file (mx-prompter #'read-file-name
                                                       "File Name: "))
                          &aux last-line)
  (multiple-value-bind (bp1 bp2) (order-bps *editor-cursor* (get-mark))
   (setq last-line (bp-line bp2))
   (with-open-file (stream (editor-default file) :direction :out :noerror t)
     (when (stringp stream) (ed-lose stream))
     (oustr (line-chars (bp-line bp1))
            stream
            (bp-position bp1)
            (-& (line-char-count (bp-line bp1))
                (bp-position bp1)))
     (send stream :terpri)
     (unless (eq (bp-line bp1) last-line)
       (loop for line first (line-next (bp-line bp1))
             then (line-next line)
             until (eq line last-line)
             do (send stream :oustr (line-chars line) 0 (line-char-count line))
             do (send stream :terpri)))
     (oustr (line-chars last-line) stream 0 (bp-position bp2))
     (close stream))))

(bindmetax append-to-buffer append-to-buffer)

(defun append-to-buffer (&optional (name (mx-prompter
                                               #'read-buffer-name
                                               "Append Region to Buffer: "))
                               &aux last-line other-last-line)
  (setq name (buffer name :create T))
  (multiple-value-bind (bp1 bp2) (order-bps *editor-cursor* (get-mark))
    (setq other-last-line (last-line (buffer-content name)))
    (setq last-line (bp-line bp2))
    (setq other-last-line
          (send other-last-line :insert-line
                (string-subseq (line-chars (bp-line bp1))
                               (bp-position bp1)
                               (-& (line-char-count (bp-line bp1))
                                   (bp-position bp1)))))
    (unless (eq (bp-line bp1) last-line)
      (loop for line first (line-next (bp-line bp1))
            then (line-next line)
            until (eq line last-line)
            do (setq other-last-line
                     (send other-last-line :insert-line
                           (string-subseq (line-chars line)
                                          0
                                          (line-char-count line))))))
    (send other-last-line
          :insert-line
          (string-subseq (line-chars last-line) 0 (bp-position bp2))))
  nil)

(editor-bind-key '(#\control-x #\A) append-to-buffer)

(defun ed-delete-file (&optional (file (mx-prompter #'read-file-name
                                                      "Delete File: ")))
 (let ((path (editor-default file)))
  (with-query-line
   (format terminal-io "Delete File ~a" (send path :string-for-editor))
   (unless (ed-y-or-n-p "Type Y to delete ~a"
                        (send path :string-for-editor))
    (ed-warn "Aborted"))
   (when (delete-file path nil)
    (ed-warn "Error deleting file")))))

(bindmetax delete-file ed-delete-file)

(defun ed-rename-file (&optional (file1 (mx-prompter #'read-file-name
                                                      "Rename File: "))
                         (file2 (mx-prompter
                                 #'read-file-name
                                 "Rename ~a To File: "
                                 (send file1 :string-for-editor))))
 (let ((path1 (editor-default file1))
       (path2 (editor-default file2)))
  (when (rename-file path1 path2 nil)
   (ed-warn "Error renaming file."))))

(bindmetax rename-file ed-rename-file)

(defun bind-in-buffer (buffer var value)
 (let ((env (buffer-environment buffer)))
  (when (null env)
   (setf (buffer-environment buffer) (null-load-environment))
   (setq env (buffer-environment buffer)))
  (cond ((memq var (load-environment-binding-vars env))
         (loop for variable in (load-environment-binding-vars env)
               for tail on (load-environment-new-values env)
               if (eq var variable)
               return (setf (car tail) value)))
        (t (setf (load-environment-binding-vars env)
                 (append (load-environment-binding-vars env) (list var)))
           (setf (load-environment-new-values env)
                 (append (load-environment-new-values env) (list value))))))
 (setq *context-change-flag* t))

(defun bind-in-current-buffer (var value)
 (bind-in-buffer *editor-buffer* var value))

(bindmetax local-bind local-bind)

(defun local-bind (&optional (var (mx-prompter #'read-symbol
                                                    "Variable: "))
                                  (value))

  (when (null value)
    (with-notify-line
     (cond ((boundp var) (format terminal-io
                                 "Now: ~a New Value: " (symeval var)))
           (t (format terminal-io "Currently Unbound. Value: ")))
     (setq value (eval (prescan #'read)))))
  (bind-in-buffer *editor-buffer* var value))

(bindmetax make-local-variable make-local-variable)

(defun make-local-variable (&optional
                                  (var (mx-prompter #'read-symbol
                                                    "Make Local Variable: ")))
  (bind-in-current-buffer var (if (boundp var) (symeval var) nil)))

(bindmetax kill-local-variable kill-local-variable)

(defun kill-local-variable (&optional (var nil))
  (let ((env (buffer-environment *editor-buffer*))
        (found-flag nil))
    (when (null env) (ed-lose "No Local Variables"))
    (setq var (if (stringp var)
                  (symbol-from-string var)
                  (mx-prompter #'read-symbol "Kill Local Variable: ")))
    (loop for variable in (load-environment-binding-vars env)
          for value in (load-environment-new-values env)
          unless (eq var variable)
          collect variable into new-vars
          and collect value into new-values
          else do (setq found-flag (list value))
          finally (setf (load-environment-binding-vars env) new-vars
                        (load-environment-new-values env) new-values))
    (setq *context-change-flag* (not (null found-flag)))
    (with-notify-line-remaining
     (if (null found-flag)
         (format terminal-io "Var ~a Not local" var)
         (format terminal-io "Var ~a Was ~a" var (car found-flag))))))

(bindmetax view-variable view-variable)

(defun view-variable (&optional (var (mx-prompter #'read-symbol
                                                       "View Variable: ")))
  (with-notify-line-remaining
   (cond ((boundp var) (format terminal-io "~a = ~a" var (symeval var)))
         (t (format terminal-io "Variable ~a is unbound." var)))))


(bindmetax reparse-mode-line reparse-mode-line)

(defun reparse-mode-line (&optional (name nil))
 (let ((buffer (if (null name) *editor-buffer*
                   (buffer name :create nil))))
  (initialize-buffer-environment buffer)
  (with-notify-line-remaining
   (format terminal-io
           "Reparsed Modes For ~a (Replaces Old Environment)"
           buffer))))

(bindmetax copy-mode-line copy-mode-line)

(defun copy-mode-line (&optional (name nil))
 (let ((buffer (if (null name) *last-buffer-selected*
                   (buffer name :create nil))))
  (loop for line first (buffer-content buffer) then (line-next line)
        while (and line (empty-line? line))
        finally (when (null line) (return nil))
        finally (setf (buffer-content *editor-buffer*)
                      (make-line *editor-buffer*
                                 nil (buffer-content *editor-buffer*)
                                 (string-subseq (line-chars line)
                                                0 (line-char-count line))))
        finally (setf (edit-cursor-home-line *editor-cursor*)
                      (buffer-content *editor-buffer*)))))

(bindmetax what-page what-page)

(defun what-page ()
 (loop with this-line = (bp-line *editor-cursor*)
       with pages = 1
       for line first (buffer-content *editor-buffer*) then (line-next line)
       for number upfrom 1
       for page-line-number upfrom 1
       while (and line (not (eq line this-line)))
       if (string-search-for-substring (line-chars line) ""
                                             0 (line-char-count line))
       do (setq page-line-number 1
                pages (1+& pages))
       finally (with-notify-line-remaining
                (format terminal-io "Page ~a Line ~a (file line ~a)"
                        pages page-line-number number))))

;(editor-defun-key #\control-% replace-string
;  (new-minibuffer "\(replace \"\" \"\"" ))
;
;
;(editor-defun-key #\meta-% query-replace-string
;  (new-minibuffer "\(query-replace \"\" \"\""))
;


(bindmetax read-mail read-mail)

(defun read-mail ()
  (point-selected (pathname "sys$login:mail.mai")))

(bindmetax view-mail view-mail)

(defun view-mail ()
  (view-file "sys$login:mail.mai"))

;I wonder if this Kludge will work?

(bindmetax send-mail send-mail)

(defun send-mail (&optional (user (mx-prompter #'readline "To: ")))
  ;;send current buffer as mail.
  (when (buffer-modified? *editor-buffer*)
    (save-file))
  (setq creamed-tty-lines-to *tty-height*)
  (cursorpos *tty-height* 0 terminal-io)
  (valret (string-append "@nil$disk:[nil.steve]mail " user " "
                         (send (buffer-file-name *editor-buffer*)
                               :file-namestring))))

;7/19/83 1:21:12
;Tuesday the nineteenth of July, 1983; 1:21:53 am

(bindmetax Insert-date-time Insert-date-time)

(defun Insert-date-time () ;Tuesday the nineteenth of July, 1983; 1:24:41 am
  (send *editor-cursor* :insert-string (time:print-current-date nil)))

(bindmetax Insert-time Insert-time)

(defun Insert-time () ;7/19/83 1:24:31
  (send *editor-cursor* :insert-string (time:print-current-time nil)))
