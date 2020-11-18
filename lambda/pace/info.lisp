;;; -*- Mode:LISP; Package:ZWEI; Base:10; Readtable:CL -*-

(defvar *info-buffers* nil)

(defvar *info-history* nil)

(defvar *info-node-position-history* nil)

(defvar *info-node-name* nil)

(defvar *offer-to-save-info-buffers* t)

(defvar *current-info-pathname-default* "angel:/src/emacs/17/info/")

(defun last-position-in-node (node-name)
  (let ((item (assq *window* *info-node-position-history*)))
    (when item
      (let ((node (assoc node-name (cdr item) :test #'string-equal)))
        (when node
          (values (cadr node) (caddr node)))))))

(defun remember-last-position-in-node (node-name start-bp bp)
  (let ((item (assq *window* *info-node-position-history*)))
    (when (null item)
      (setq item (list *window*))
      (push item *info-node-position-history*))
    (let ((node (assoc node-name (cdr item) :test #'string-equal)))
      (when (null node)
        (setq node (list node-name nil nil))
        (push node (cdr item)))
      (setf (cadr node) start-bp)
      (setf (caddr node) bp))))

(defun reset-info ()
  (setq *info-buffers* nil)
  (setq *info-history* nil)
  (DOLIST (W *ALL-ZMACS-WINDOWS*)
    (LET ((HISTORY (SEND W :BUFFER-HISTORY)))
      (dolist (buffer (copylist (history-list history)))
        (when (eq (get buffer :special-type) :info)
          (delete-from-history buffer history)))))
  (dolist (buffer (copylist *zmacs-buffer-list*))
    (when (eq (get buffer :special-type) :info)
      (setq *zmacs-buffer-list* (remq buffer *zmacs-buffer-list*))))
  (dolist (pair (copylist *zmacs-buffer-name-alist*))
    (when (not (member (car pair) *zmacs-buffer-list*
                       :test #'(lambda (string buffer)
                                 (string-equal string (buffer-name buffer)))))
      (setq *zmacs-buffer-name-alist* (delq pair *zmacs-buffer-name-alist*))))
  )

(defun info-make-mode-line-list ()
  (let ((node-name (or (getf (get *interval* 'node-header) :node)
                       "(No current node)"))
        (pathname (or (send *interval* :pathname)
                      (get *interval* 'pathname)
                      (and (send *interval* :superior)
                           (get (send *interval* :superior) 'pathname))))
        (superior (or (send *interval* :superior) *interval*))
        )
    (cond ((null pathname)
           `("ZMACS " "(" *mode-name-list* ") "
             ,(if (buffer-modified-p superior) "* " nil)
             *more-above-below*
             ,node-name
             "   (Q to exit)"))
          (t
           `("INFO "
             ,(if (buffer-modified-p superior) "* " nil)
             *more-above-below*
             ,node-name
             "   File: "
             ,(send pathname :string-for-printing)
             "   (Q to exit)")))))


(DEFMAJOR COM-info-MODE info-MODE "INFO"
  ()
  (SET-CHAR-SYNTAX WORD-ALPHABETIC *MODE-WORD-SYNTAX-TABLE* #\_)
  (SET-CHAR-SYNTAX WORD-ALPHABETIC *MODE-WORD-SYNTAX-TABLE* #\')
  (SET-COMTAB *MODE-COMTAB* '(
                              #\space COM-NEXT-SCREEN
                              #\N com-info-next
                              #\n (0 #\N)
                              #\P com-info-previous
                              #\p (0 #\P)
                              #\U com-info-up
                              #\u (0 #\U)
                              #\M com-info-menu
                              #\m (0 #\M)
                              #\G com-info-goto
                              #\g (0 #\G)
                              #\E com-info-edit
                              #\e (0 #\E)
                              #\F com-info-follow-reference
                              #\f (0 #\F)
                              #\L com-info-last
                              #\l (0 #\L)
                              #\. com-info-beginning-of-node
                              #\B com-info-beginning-of-node
                              #\b (0 #\B)
                              #\1 com-info-menu-number
                              #\2 com-info-menu-number
                              #\3 com-info-menu-number
                              #\4 com-info-menu-number
                              #\5 com-info-menu-number
                              #\6 com-info-menu-number
                              #\7 com-info-menu-number
                              #\8 com-info-menu-number
                              #\9 com-info-menu-number
                              #\? com-info-short-help
                              #\H com-info-help
                              #\h (0 #\H)
                              #\D com-info-directory
                              #\d (0 #\D)
                              #\rubout com-previous-screen
                              #\overstrike com-previous-screen
                              #\S com-info-search
                              #\s (0 #\S)
                              #\help com-info-documentation
                              #\Q com-info-quit
                              #\q (0 #\Q)
                              )
              '(("Add New Node" . com-add-new-node)
                ("Info Reset" . com-info-reset)
                ("Uncache" . com-info-uncache)
                ))
  (set-comtab *zmacs-control-x-comtab*
              '(#\K com-info-kill-buffer
                ))
  (SET-MODE-LINE-LIST (info-make-mode-line-list))
  )

(defvar *info-recursive-edit-comtab*
        (let ((comtab (set-comtab 'info-recursive-edit-comtab
                                  '(#\C-\ COM-EXIT-CONTROL-R
                                    #\END COM-EXIT-CONTROL-R
                                    #\ABORT COM-EXIT-CONTROL-R)))
              (c-x-comtab (set-comtab 'info-recursive-edit-c-x-comtab
                                      '(#\c-\S com-info-save-and-exit)
                                      ))
              )
          (SET-COMTAB-INDIRECTION comtab *STANDARD-COMTAB*)
          (SET-COMTAB-CONTROL-INDIRECTION c-x-comtab)
          (SET-COMTAB comtab
                      (LIST #\C-\X (MAKE-EXTENDED-COMMAND c-x-comtab)))
          comtab))

(defcom com-info-reset "reset" ()
  (setq *info-buffers* nil)
  (setq *info-history* nil)
  (info-goto-node "(dir)")
  dis-text)

(DEFPROP INFO-MODE :TEXT EDITING-TYPE)

(defun info-find-buffer-named (name)
  (dolist (buffer *info-buffers*)
    (when (string-equal name (buffer-name buffer))
      (return buffer))))

(DEFUN info-read-file-for-node (info-buffer-name aux-info-buffer-name pathname)
  (when (null (probef pathname))
    (setq pathname (send pathname :new-raw-name (string-downcase (send pathname :name)))))

  (when (null (probef pathname))
    (barf "can't find file ~a" pathname))

  (let ((interval (info-find-buffer-named info-buffer-name))
        (aux-interval (info-find-buffer-named aux-info-buffer-name)))
    (when interval
      (kill-buffer interval))
    (setq interval (make-instance 'zmacs-buffer :name info-buffer-name))
    (when aux-interval
      (kill-buffer aux-interval))
    (setq aux-interval (make-instance 'zmacs-buffer :name aux-info-buffer-name))

    (MAKE-BUFFER-READ-ONLY INTERVAL)
    (make-buffer-read-only aux-interval)
    (SETF (NODE-SPECIAL-TYPE INTERVAL) :INFO)
    (setf (node-special-type aux-interval) :info)
    (SETF (BUFFER-SAVED-MAJOR-MODE INTERVAL) 'INFO-MODE)
    (setf (buffer-saved-major-mode aux-interval) 'info-mode)
    (SETF (GET INTERVAL 'pathname) PATHNAME)
    (send aux-interval :set-superior interval)
    (setf (get interval 'aux-interval) aux-interval)
    (LET ((*INTERVAL* NIL))
      (info-read-file-revert interval))
    (info-read-tags interval)

    (push interval *info-buffers*)
    interval))

(defun info-read-tags (interval &aux tags)
  (let ((end-bp (zwei-search (interval-last-bp interval)
                             "End Tag Table"
                             t
                             nil
                             500.
                             (interval-first-bp interval))))
    (when (null end-bp)
      (setf (get interval 'tags) nil)
      (return-from info-read-tags nil))
    (let ((start-bp (zwei-search end-bp
                                 "Tag Table:"
                                 t
                                 nil
                                 nil
                                 (interval-first-bp interval))))
      (when (or (null start-bp)
                (null (line-next (bp-line start-bp))))
        (setf (get interval 'tags) nil)
        (return-from info-read-tags nil))
      (move-bp start-bp (line-next (bp-line start-bp)) 0)
      (charmap-per-line (start-bp
                          end-bp
                          )
                        ((let ((line (charmap-line)))
                           (cond ((char-equal (aref line 0) #\)
                                  (return nil))
                                 (t
                                  (let ((node-end (string-search "Node:" line)))
                                    (when node-end
                                      (setq node-end (string-search-not-char #\space line (+ 5 node-end)))
                                      (when node-end
                                        (do ((i node-end (1+ i))
                                             (end (string-length line)))
                                            ((= i end))
                                          (when (ldb-test (byte 1 7) (aref line i))
                                            (let ((position (parse-integer (string-append (logand #o177 (aref line i))
                                                                                          (substring line (1+ i))))))
                                              (push (list (substring line node-end i) position) tags)
                                              (return nil))))))))))))))
  (setf (get interval 'tags) tags)
  tags)


(defun goto-character-position (pos interval)
  (do ((line (bp-line (interval-first-bp interval))
             (line-next line))
       (chars-before 0)
       )
      ((null line)
       nil)
    (cond ((>= chars-before pos)
           (return (forward-char (create-bp line 0) (- pos chars-before))))
          (t
           (incf chars-before (1+ (string-length line)))))))

(defun info-read-file-revert (interval)
  (let ((pathname (get interval 'pathname)))
    (with-open-file (stream pathname)
      (WITH-READ-ONLY-SUPPRESSED (interval)
        (stream-into-bp stream (interval-first-bp interval))
        (setf (buffer-tick interval) (tick)))
      (not-modified interval))))

(defcom com-info "" ()
  (when *numeric-arg-p*
    (reset-info))

  (let ((history (assq *window* *info-history*))
        place)
    (cond ((null (cdr history))
           (setq place (list "(dir)" *current-info-pathname-default*)))
          (t
           (setq place (pop (cdr history)))))
    (condition-case ()
        (progn
         (setq *current-info-pathname-default* (cadr place))
         (info-goto-node (car place)))
      (barf
       (format *query-io* "~&Error going to node ~s, trying (DIR) instead.")
       (info-goto-node "(dir)"))))
  dis-text)

(defun info-canonicalize-node-name (full-name &aux can-name file-name node-name pathname)
  (when (zerop (string-length full-name))
    (setq full-name "top"))
  (cond ((char-equal (aref full-name 0) #\( )
         (let ((close-paren (string-search-char #\) full-name)))
           (when (null close-paren)
             (barf "Bad node name ~s" full-name))
           (setq node-name (string-trim '(#\space #\tab) (substring full-name (1+ close-paren))))
           (when (zerop (string-length node-name))
             (setq node-name "top"))
           (setq file-name (string-trim '(#\space #\tab) (substring full-name 1 close-paren)))
           (setq pathname
                 (setq *current-info-pathname-default*
                       (fs:merge-pathname-defaults file-name *current-info-pathname-default*)))
           (setq can-name (format nil "(~a)~a" file-name node-name))))
        ((setq file-name (getf (get *interval* 'node-header) :file))
         (setq can-name (format nil "(~a)~a" file-name full-name))
         (setq node-name full-name)
         (setq pathname nil))
        (t
         (setq can-name full-name)
         (setq node-name full-name)
         (setq file-name full-name)
         (setq pathname nil)))
  (values can-name
          node-name
          pathname
          (format nil "*INFO-SUP-(~a)*" file-name)
          (format nil "*INFO-(~a)*" file-name)))

(defun info-remember-node-on-history (name)
  (let ((history (assq *window* *info-history*)))
    (cond ((null (cadr history))
           (push (list *window* (list name *current-info-pathname-default*)) *info-history*))
          ((not (string-equal name (caadr history)))
           (push (list name *current-info-pathname-default*) (cdr history))))))

(defun info-goto-node (full-name &aux buffer)
  (when (zerop (string-length full-name))
    (setq full-name "top"))
  (when (eq (get *interval* :special-type) :info)
    (let ((leaving-node (get *interval* 'canonical-node-name)))
      (when leaving-node
        (remember-last-position-in-node leaving-node (window-start-bp *window*) (copy-bp (point))))))
  (multiple-value-bind (canonical-name node-name pathname info-buffer-name aux-info-buffer-name)
      (info-canonicalize-node-name full-name)
    (cond ((not (char-equal (aref full-name 0) #\())
           (setq buffer (send *interval* :superior)))
          (t
           (setq buffer (info-find-buffer-named info-buffer-name))
           (when (null buffer)
             (setq buffer (info-read-file-for-node info-buffer-name aux-info-buffer-name pathname)))
           (let ((aux (get buffer 'aux-interval)))
             (setf (interval-first-bp aux) (interval-first-bp buffer))
             (setf (interval-last-bp aux) (interval-last-bp buffer))
             (make-buffer-current aux)
             )))

    (setf (get *interval* 'canonical-node-name) canonical-name)
    (cond ((string-equal node-name "*")
           (setf (get *interval* 'node-header) nil)
           (info-change-current-interval
             (interval-first-bp buffer)
             (interval-last-bp buffer)))
          (t
           (let ((bp (info-find-node-in-this-file node-name)))
             (when (null bp)
               (barf "can't find node ~s" full-name))
             (multiple-value-bind (begin end node-header header-items)
                 (info-compute-node-boundaries bp buffer)
               (setf (get *interval* 'node-header) node-header)
               (setf (get *interval* 'header-items) header-items)
               (info-change-current-interval begin end)
               ))))
    (when (eq *major-mode* 'info-mode)
      (setq *mode-line-list* (info-make-mode-line-list)))
    (info-remember-node-on-history canonical-name)
    (multiple-value-bind (start point)
        (last-position-in-node canonical-name)
      (when (and start point
                 (or (bp-= (interval-first-bp *interval*) point)
                     (bp-< (interval-first-bp *interval*) point))
                 (or (bp-= point (interval-last-bp *interval*))
                     (bp-< point (interval-last-bp *interval*))))
        (move-bp (window-start-bp *window*) start)
        (move-bp (window-point *window*) point)))
    )
  (info-get-notes)
  (info-get-menu t)
  (must-redisplay *window* dis-text)
  )

(defun find-good-starting-place (node-name)
  (let* ((interval (or (send *interval* :superior) *interval*))
         (entry (ass #'string-equal node-name (get interval 'tags))))
    (cond ((null entry)
           (interval-first-bp interval))
          (t
           (let ((*interval* interval))
             (forward-line (goto-character-position (cadr entry) interval) -50 t))))))

(defcom com-info-uncache "uncache" ()
  (let ((sup (or (send *interval* :superior) *interval*)))
    (setf (get sup 'node-cache) nil)
    (setf (get sup 'references) nil)
    (setf (get sup 'menu) nil))
  dis-none)

(defun info-cache-node-position (node-name bp)
  (let ((sup (or (send *interval* :superior) *interval*)))
    (let* ((node-cache (get sup 'node-cache))
           (node-tick (car node-cache))
           (nodes (cdr node-cache)))
      (when (and node-cache
                 (not (= node-tick (buffer-tick sup))))
        (setq nodes nil))
      (setq nodes (delq (assoc node-name nodes :test #'string-equal) nodes))
      (setf (get sup 'node-cache) (cons (buffer-tick sup)
                                        (cons (cons node-name bp)
                                              nodes))))))

(defun info-cached-node-position-if-valid (node-name)
  (let ((sup (or (send *interval* :superior) *interval*)))
    (let ((node-cache (get sup 'node-cache)))
      (when (and node-cache
                 (= (car node-cache) (buffer-tick sup)))
        (let ((item (assoc node-name (cdr node-cache) :test #'string-equal)))
          (cdr item))))))

(defun info-find-node-in-this-file (node-name &optional ok-if-not-found)
  (let ((bp (info-cached-node-position-if-valid node-name)))
    (when (null bp)
      (setq bp (info-find-node-in-this-file-hard node-name ok-if-not-found))
      (when bp
        (info-cache-node-position node-name bp)))
    bp))

(defun info-find-node-in-this-file-hard (node-name &optional ok-if-not-found)
  (let ((start-bp (find-good-starting-place node-name)))
    (do ((line (bp-line start-bp) (line-next line)))
        ((null line)
         (when (not ok-if-not-found)
           (barf "Can't find node ~s" node-name))
         nil)
      (when (and (> (string-length line) 0)
                 (char-equal (char-code (aref line 0)) #\or)
                 )
        (let ((header-start (create-bp (line-next line) 0))
              (header-end (end-of-line (line-next line))))
          (let ((bp (zwei-search header-start "Node:" nil nil nil header-end)))
            (when bp
              (setq bp (forward-over '(#\space #\tab) bp header-end))
              (let ((end-bp (search-set bp '(#\tab #\, #\.) nil t header-end)))
                (setq end-bp (backward-over '(#\tab #\, #\.) end-bp))
                (let ((this-node-name (string-interval bp end-bp t t)))
                  (when (string-equal this-node-name node-name)
                    (return bp)))))))))))

(defconst *info-node-separator* (format nil "~%"))

(defun info-compute-node-boundaries (bp interval &aux node-header header-items)
  "select node that point is in"
  (let ((begin (zwei-search bp
                            *info-node-separator*
                            t
                            nil
                            nil
                            (interval-first-bp interval)))
        (end (zwei-search bp
                          *info-node-separator*
                          nil
                          nil
                          nil
                          (interval-last-bp interval))))
    (when (null begin)
      (barf "Can't find node beginning marker"))
    (let ((*interval* interval))
      (setq begin (forward-line begin 2)))
    (when (null begin)
      (barf "Bad node: beginning marker, but no header line"))
    (multiple-value (node-header header-items)
      (info-parse-node-header begin))
    (cond ((null end)
           (setq end (interval-last-bp interval)))
          (t
           (setq end (create-bp (line-previous (bp-line end))
                                (string-length (line-previous (bp-line end)))))))
    (values begin end node-header header-items)))

(defun info-change-current-interval (start-bp end-bp)
  (move-bp (point) start-bp)
  (send *interval* :set-first-bp (copy-bp start-bp :normal))
  (send *interval* :set-last-bp (copy-bp end-bp :moves))
  (move-bp (window-point *window*) start-bp)
  (move-bp (window-start-bp *window*) start-bp)
  (must-redisplay *window* dis-all)
;  (REDISPLAY-POINT-ON-PLINE (point) *window* 0 nil)
  )

(defun info-parse-node-header (at-bp)
  (let ((bp (create-bp (bp-line at-bp) 0))
        (end-bp (if (line-next (bp-line at-bp))
                    (create-bp (line-next (bp-line at-bp)) 0)
                  (create-bp (bp-line at-bp) (string-length (bp-line at-bp)))))
        node-header header-items)
    (do (field-name-start
         field-name-end
         value-start
         value-end
         field value
         )
        (())
      (setq field-name-start (forward-over '(#\space #\tab) bp end-bp))
      (setq field-name-end (zwei-search field-name-start #\: nil nil nil end-bp))
      (when (or (null field-name-end)
                (bp-= field-name-start field-name-end)
                (bp-= field-name-end end-bp))
        (return nil))
      (setq value-start field-name-end)
      (setq field-name-end (forward-char field-name-end -1))
      (setq field (string-interval field-name-start field-name-end t t))
      (setq value-start (forward-over '(#\space #\tab) value-start end-bp))
      (setq value-end (search-set value-start
                                  (if (string-equal field "File")
                                      '(#\space #\tab #\,)
                                    '(#\tab #\,))
                                  nil
                                  t
                                  end-bp))
      (when (bp-= value-start value-end)
        (return nil))
      (setq bp value-end)
      (setq value-end (forward-char value-end -1))
      (setq value (string-interval value-start value-end t t))
      (setq node-header (append node-header
                                (list (intern (string-upcase field) pkg-keyword-package)
                                      value)))
      (push (list value value value-start value-end) header-items)
      )
    (values node-header header-items)
    ))

(defcom com-info-next "next" ()
  (let* ((node-header (get *interval* 'node-header))
         (next-node (getf node-header :next)))
    (when (null next-node)
      (barf "This node has no NEXT."))
    (info-goto-node next-node)
    dis-text))

(defcom com-info-up "up" ()
  (let* ((node-header (get *interval* 'node-header))
         (up-node (getf node-header :up)))
    (when (null up-node)
      (barf "This node has no UP."))
    (info-goto-node up-node)
    dis-text))

(defcom com-info-previous "previous" ()
  (let* ((node-header (get *interval* 'node-header))
         (prev-node (or (getf node-header :previous)
                        (getf node-header :prev))))
    (when (null prev-node)
      (barf "This node has no PREVIOUS."))
    (info-goto-node prev-node)
    dis-text))

(defcom com-info-goto "goto" ()
  (let ((node-name (completing-read-from-mini-buffer "Node name:" nil t)))
    (info-goto-node node-name)
    dis-all))

(defvar *info-enable-editing-p* nil)

(defcom com-info-edit "edit" ()
  (when (null *info-enable-editing-p*)
    (cond ((yes-or-no-p "Are you sure you want to edit this node? ")
           (setq *info-enable-editing-p* t))
          (t
           (barf "Aborted."))))

  (let ((superior (or (send *interval* :superior) *interval*)))
    (make-buffer-not-read-only *interval*)
    (make-buffer-not-read-only superior)
    (SEND *WINDOW* :EDIT NIL *info-recursive-edit-comtab* `("[" ,@*MODE-LINE-LIST* "]") NIL)
    (multiple-value-bind (node-header header-items)
        (info-parse-node-header (interval-first-bp *interval*))
      (setf (get *interval* 'node-header) node-header)
      (setf (get *interval* 'header-items) header-items))
    (when (eq *major-mode* 'info-mode)
      (setq *mode-line-list* (info-make-mode-line-list)))
    (make-buffer-read-only *interval*)
    (make-buffer-read-only superior)
    (setf (buffer-tick superior) (tick))
    dis-text))

;;;this is called when exiting info mode, so no telling what *interval* might be
(defvar *no-info-save* t)
(defun maybe-save-info-buffers (&optional no-query &aux saved-any-p)
  (when *offer-to-save-info-buffers*
    (dolist (buffer *info-buffers*)
      (let ((pathname (get buffer 'pathname)))
        (when (and (buffer-modified-p buffer)
                   pathname
                   (not *no-info-save*)
                   (or no-query (y-or-n-p "Save info file ~a? " pathname)))
          (with-open-file (stream pathname :direction :output)
            (stream-out-interval stream
                                 (interval-first-bp buffer)
                                 (interval-last-bp buffer)
                                 t
                                 t))
          (setq saved-any-p t)
          (not-modified buffer))))
    saved-any-p))

(defun info-next-menu-item (bp)
  (cond ((null (line-next (bp-line bp)))
         nil)
        ((eq (bp-line bp) (bp-line (interval-last-bp *interval*)))
         nil)
        (t
         (do ((line (line-next (bp-line bp)) (line-next line))
              (last-line (bp-line (interval-last-bp *interval*))))
             ((null line) nil)
           (when (and (> (string-length line) 1)
                      (char-equal (aref line 0) #\*)
                      (or (not (eq line last-line))
                          (> (bp-index (interval-last-bp *interval*)) 2)))
             (return (create-bp line 2)))
           (when (eq line last-line)
             (return nil))))))

(defun info-cache-menu (menu-items)
  (let ((sup (or (send *interval* :superior) *interval*))
        (node-name (get *interval* 'canonical-node-name)))
    (let ((node-menu (assoc node-name (get sup 'menu) :test #'string-equal)))
      (when (null node-menu)
        (setq node-menu (list node-name nil nil))
        (setf (get sup 'menu) (cons node-menu (get sup 'menu))))
      (setf (cadr node-menu) (buffer-tick sup))
      (setf (caddr node-menu) menu-items))))

(defun info-get-menu-cache-if-valid ()
  (let ((sup (or (send *interval* :superior) *interval*))
        (node-name (get *interval* 'canonical-node-name)))
    (let ((node-menu (assoc node-name (get sup 'menu) :test #'string-equal)))
      (when (and node-menu
                 (integerp (cadr node-menu))
                 (= (cadr node-menu) (buffer-tick sup)))
        (caddr node-menu)))))

(defun info-get-menu (&optional ok-if-none)
  (let ((menu-items (info-get-menu-cache-if-valid)))
    (when (null menu-items)
      (setq menu-items (info-get-menu-hard ok-if-none))
      (info-cache-menu menu-items))
    menu-items))

(defun extract-node-name (string start)
  (setq string (string-remove-fonts string))
  (let ((i start)
        real-start
        (end (string-length string)))
    (do ()
        ((or (= i end)
             (not (member (aref string i) '(#\space #\tab) :test #'char-equal))))
      (incf i))
    (setq real-start i)
    (when (char-equal (aref string i) #\()
      (setq i (string-search-char #\) string i))
      (when (null i)
        (barf "No close-paren in node name ~s" (substring string real-start)))
      )
    (setq i (string-search-set '(#\tab #\. #\,) string i))
    (string-trim '(#\space #\tab) (substring string real-start i))))

(defun info-get-menu-hard (&optional ok-if-none)
  (let ((menu-start (zwei-search (interval-first-bp *interval*)
                                 "* Menu:"))
        (menu-items nil)
        )
    (when (null menu-start)
      (when (null ok-if-none)
        (barf "No menu."))
      (return-from info-get-menu-hard nil))
    (do* ((bp (info-next-menu-item menu-start)
              (info-next-menu-item bp))
          end-bp
          selector-start selector-end node-start node-end
          )
        ((null bp))
      (setq end-bp (end-of-line (bp-line bp)))
      (setq bp (forward-over '(#\space #\tab) bp end-bp))
      (setq selector-start bp)
      (setq bp (zwei-search bp #\: nil nil nil end-bp))
      (setq selector-end bp)
      (when (bp-= selector-start selector-end)
        (return nil))
      (setq selector-end (forward-char selector-end -1))
      (setq selector-end (backward-over '(#\space #\tab) selector-end))
      (cond ((looking-at bp #\:)
             (setq node-start selector-start)
             (setq node-end selector-end)
             )
            (t
             (setq bp (forward-over '(#\space #\tab) bp))
             (setq node-start bp)
             (setq bp (search-set bp '(#\tab #\, #\.) nil nil end-bp))
             (cond ((null bp)
                    (setq bp end-bp)
                    (setq node-end end-bp))
                   (t
                    (when (bp-= node-start node-end)
                      (return nil))
                    (setq node-end (forward-char bp -1))))
             (setq node-end (backward-over '(#\space #\tab) node-end))))
        (push (list (string-interval selector-start selector-end t t)
                    (string-interval node-start node-end t t)
                    selector-start
                    selector-end)
              menu-items))
    (reverse menu-items)))

(defun info-item-filter (bp item-list)
  (dolist (item item-list)
    (let ((start (nth 2 item))
          (end (nth 3 item)))
      (when (and (or (bp-= start bp)
                     (bp-< start bp))
                 (bp-< bp end))
        (let ((chars-before (- (bp-index bp) (bp-index start))))
          (when (and (>= chars-before 0)
                     (< chars-before (string-length (car item))))
            (return (values (car item) chars-before))))))))

(defcom com-info-menu "menu" ()
  (let* ((menu-items (info-get-menu))
         (item (completing-read-from-mini-buffer
                 "Node from menu:" menu-items)))
    (cond ((consp item)
           (info-goto-node (cadr item)))
          ((stringp item)
           (let ((item-desc (assoc item menu-items :test #'string-equal)))
             (info-goto-node (cadr item-desc))))
          (t
           (barf "Unknown item ~s" item)))
    dis-text))

;(defcom com-info-menu "menu" ()
;  (let* ((menu-items (info-get-menu))
;        (item
;          (mouse-highlight #'(lambda ()
;                               (completing-read-from-mini-buffer
;                                 "Node from menu:" menu-items))
;                           #'(lambda (bp)
;                               (info-item-filter bp menu-items)))))
;    (cond ((consp item)
;          (info-goto-node (cadr item)))
;         ((stringp item)
;          (let ((item-desc (assoc item menu-items :test #'string-equal)))
;            (info-goto-node (cadr item-desc))))
;         (t
;          (barf "Unknown item ~s" item)))
;    dis-text))

(defun info-cache-references (references)
  (let ((sup (or (send *interval* :superior) *interval*))
        (node-name (get *interval* 'canonical-node-name)))
    (let ((node-references (assoc node-name (get sup 'references) :test #'string-equal)))
      (when (null node-references)
        (setq node-references (list node-name nil nil))
        (setf (get sup 'references) (cons node-references (get sup 'references))))
      (setf (cadr node-references) (buffer-tick sup))
      (setf (caddr node-references) references))))

(defun info-get-references-cache-if-valid ()
  (let ((sup (or (send *interval* :superior) *interval*))
        (node-name (get *interval* 'canonical-node-name)))
    (let ((node-references (assoc node-name (get sup 'references) :test #'string-equal)))
      (when (and node-references
                 (integerp (cadr node-references))
                 (= (cadr node-references) (buffer-tick sup)))
        (caddr node-references)))))

(defun info-get-notes ()
  (let ((references-items (info-get-references-cache-if-valid)))
    (when (null references-items)
      (setq references-items (info-get-references-hard))
      (info-cache-references references-items))
    references-items))

(defun info-get-references-hard (&aux notes)
  (do ((note-start (zwei-search (interval-first-bp *interval*) "*Note")
                   (zwei-search note-start "*Note")))
      ((null note-start)
       (reverse notes))
    (let ((colon (zwei-search note-start #\:))
          note-selector node-name)
      (when colon
        (setq colon (forward-char colon -1)))
      (when colon
        (let ((note-selector-start (forward-word colon -1)))
          (setq note-selector (string-trim '(#\space #\tab) (string-interval note-selector-start colon)))
          (when (null (string-search-char #\return note-selector))
            (let ((after-colon (forward-char colon)))
              (when after-colon
                (cond ((char-equal (bp-char after-colon) #\:)
                       (setq node-name note-selector))
                      (t
                       (let ((end-of-node-name (search-set after-colon '(#\tab #\, #\.))))
                         (when end-of-node-name
                           (setq end-of-node-name (forward-char end-of-node-name -1)))
                         (when end-of-node-name
                           (setq node-name (string-trim '(#\space #\tab) (string-interval after-colon end-of-node-name)))))))
                (when (null (string-search-char #\return node-name))
                  (push (list note-selector
                              node-name
                              note-selector-start
                              colon)
                        notes))))))))))

(defcom com-info-follow-reference "follow reference" ()
  (let ((notes (info-get-notes)))
    (when (null notes)
      (barf "No footnotes in this node."))
    (let ((item (completing-read-from-mini-buffer
                  "Footnote:" notes)))
      (cond ((consp item)
             (info-goto-node (cadr item)))
            ((stringp item)
             (let ((item-desc (assoc item notes :test #'string-equal)))
               (info-goto-node (cadr item-desc))))
            (t
             (barf "Unknown item ~s" item)))))
  dis-text)

;(defcom com-info-follow-reference "follow reference" ()
;  (let ((notes (info-get-notes)))
;    (when (null notes)
;      (barf "No footnotes in this node."))
;    (let ((item
;           (mouse-highlight #'(lambda ()
;                                (completing-read-from-mini-buffer
;                                 "Footnote:" notes))
;                           #'(lambda (bp)
;                               (info-item-filter bp notes)))))
;      (cond ((consp item)
;            (info-goto-node (cadr item)))
;           ((stringp item)
;            (let ((item-desc (assoc item notes :test #'string-equal)))
;              (info-goto-node (cadr item-desc))))
;           (t
;            (barf "Unknown item ~s" item)))))
;  dis-text)

(defcom com-info-last "last" ()
  (let ((history (assq *window* *info-history*)))
    (cond ((null (caddr history))
           (barf "No previous nodes in history for this window."))
          (t
           (pop (cdr history))
           (let ((place (pop (cdr history))))
             (setq *current-info-pathname-default* (cadr place))
             (info-goto-node (car place))))))
  dis-text)

(defcom com-add-new-node "Add a node to this nodes menu." ()
  (let ((name (string (string-trim '(#\space #\tab) (typein-line-readline "Name of new node:"))))
        (sup (send *interval* :superior))
        new-node-start)
    (when (string-search-set '(#\( #\)) name)
      (barf "Can't specify node in different file."))
    (when (info-find-node-in-this-file name t)
      (barf "A node named ~s already exists." name))
    (let ((last-menu-item (car (last (info-get-menu t)))))
      (with-read-only-suppressed (sup)
        (when (not (zerop (bp-index (interval-last-bp *interval*))))
          (insert-moving (interval-last-bp *interval*) (format nil "~%")))
        (when (null last-menu-item)
          (insert-moving (interval-last-bp *interval*) (format nil "* Menu:~%")))
        (insert-moving (interval-last-bp *interval*) (format nil "* ~a::~%" name))

        (setq new-node-start (interval-last-bp *interval*))
        (when last-menu-item
          (let ((bp (info-find-node-in-this-file (car last-menu-item))))
            (when (null bp)
              (barf "can't find node for last menu item: ~s" (car last-menu-item)))
            (multiple-value-bind (begin end)
                (info-compute-node-boundaries bp sup)
              (cond ((string-search "Next:" (bp-line begin))
                     (format *query-io* "Warning, the last menu item, ~s, already has a next node." (car last-menu-item)))
                    (t
                     (insert (create-bp (bp-line begin) (string-length (bp-line begin)))
                             (format nil ", Next: ~a" name))))
              (setq new-node-start end))))
        (let ((bp (copy-bp new-node-start))
              (node-header (get *interval* 'node-header)))
          (when (not (zerop (bp-index bp)))
            (insert-moving bp (format nil "~%")))
          (insert-moving bp (format nil "~%File: ~a, Node: ~a, Up: ~a, Previous: ~a"
                                    (getf node-header :file)
                                    name
                                    (getf node-header :node)
                                    (if last-menu-item
                                        (car last-menu-item)
                                      (getf node-header :node))))
          (insert-moving bp (format nil "~2%")))))
    (setf (buffer-tick sup) (tick))
    (info-goto-node name)
    (move-bp (point) (interval-last-bp *interval*))
    (com-info-edit))
  dis-text)

(setq *COM-DOCUMENTATION-ALIST*
      (cons '(#\I com-info)
            (delq (assq #\I *COM-DOCUMENTATION-ALIST*) *COM-DOCUMENTATION-ALIST*)))

(defprop com-info "Run INFO." documentation)

(defcom com-info-beginning-of-node "Beginning" ()
  (move-bp (point) (interval-first-bp *interval*))
  (move-bp (window-point *window*) (interval-first-bp *interval*))
  (move-bp (window-start-bp *window*) (interval-first-bp *interval*))
  dis-text)


(defcom com-info-menu-number "Nth menu item" ()
  (let ((menu-items (info-get-menu))
        (item-requested (- *LAST-COMMAND-CHAR* #\0)))
    (cond ((and (> item-requested 0)
                (<= item-requested 5)
                (>= (length menu-items) item-requested))
           (info-goto-node (cadr (nth (1- item-requested) menu-items)))
           dis-text)
          (t
           (barf)))))

(defun info-mouse-click (node-full-name)
  (cond ((null node-full-name)
         (barf))
        ((null *mini-buffer-command-in-progress* )
         (info-goto-node node-full-name))
        (t
         (throw 'return-from-command-loop (list nil node-full-name)))))

(defcom com-info-help "Help" ()
  (info-goto-node "(info)help")
  dis-text)

(defcom com-info-directory "directory" ()
  (info-goto-node "(dir)")
  dis-text)

(defvar *last-info-search-string* "")

(defcom com-info-search "search" ()
  (let ((interval (or (send *interval* :superior) *interval*))
        (string (completing-read-from-mini-buffer "Search string:" nil t))
        )
    (when (zerop (string-length string))
      (setq string *last-info-search-string*))
    (setq *last-info-search-string* string)
    (let ((bp (zwei-search (point) string
                           nil nil nil
                           (interval-last-bp interval))))
      (cond ((null bp)
             (barf))
            (t
             (multiple-value-bind (begin end node-header header-items)
                 (info-compute-node-boundaries bp interval)
               (setf (get *interval* 'node-header) node-header)
               (setf (get *interval* 'header-items) header-items)
               (info-change-current-interval begin end)
               (move-bp (point) bp)
               (let ((name (getf (get *interval* 'node-header) :node)))
                 (when name
                   (info-remember-node-on-history (info-canonicalize-node-name name))
                   (when (eq *major-mode* 'info-mode)
                     (cerror :no-action nil nil "foo")
                     (setq *mode-line-list* (info-make-mode-line-list)))
                   ))
               )))))
  dis-text)

(defcom com-info-documentation "doc" ()
  (let ((*com-documentation-alist*
          (cons '(#\M com-info-short-help)
                *com-documentation-alist*)))
    (com-documentation)))

(defcom com-info-short-help "short help" ()
  (format t "
Info mode provides commands for browsing through the Info documentation tree.
Documentation in Info is divided into \"nodes\", each of which
discusses one topic and contains references to other nodes
which discuss related topics.  Info has commands to follow
the references and show you other nodes.

h       Invoke the Info tutorial.

Selecting other nodes:
n       Move to the \"next\" node of this node.
p       Move to the \"previous\" node of this node.
u       Move \"up\" from this node.
m       Pick menu item specified by name (or abbreviation).
        Picking a menu item causes another node to be selected.
f       Follow a cross reference.  Reads name of reference.
l       Move to the last node you were at.

Moving within a node:
Space   scroll forward a page.     DEL  scroll backward.
b       Go to beginning of node.

Advanced commands:
q       Quit Info: reselect previously selected buffer.
e       Edit contents of selected node.
1       Pick first item in node's menu.
2, 3, 4, 5   Pick second ... fifth item in node's menu.
g       Move to node specified by name.
        You may include a filename as well, as (FILENAME)NODENAME.
s       Search through this Info file for specified regexp,
        and select the node in which the next occurrence is found.

\(Thanks to GNU-EMACS for this help message.)

Type space to flush:
" #\help)
  dis-text)


(defcom com-info-quit "quit" ()
  (info-hide-buffers t)
;  (let ((element (assq 'info-mode *mode-list*)))
;    (when element
;      (setf (cadr element) (delq (assq 'info-hide-buffers (cadr element)) (cadr element)))))

;  (MAKE-BUFFER-CURRENT (OR (CAR (MEM 'NEQ *interval* (HISTORY-LIST
;                                                      (send *window* :BUFFER-HISTORY))))
;                          (car *zmacs-buffer-list*)))
  (make-buffer-current (previous-buffer))
  DIS-BPS)

(defun info-hide-buffers (&optional bury-p)
  ;;this is called as the INFO major mode is being exited.  However,
  ;;it is called early enough that if the following function asks a
  ;;question, and the user aborts, then we just stay in info.
  (maybe-save-info-buffers)
  ;;from (:method zmacs-window :exit-special-buffer)
  (WITHOUT-INTERRUPTS
    (dolist (main-interval *info-buffers*)
      (dolist (interval (list main-interval (get main-interval 'aux-interval)))
        (when interval
          (DOLIST (W *ALL-ZMACS-WINDOWS*)
            (LET ((HISTORY (SEND W :BUFFER-HISTORY)))
              (delete-from-history interval HISTORY)))
          (SETQ *ZMACS-BUFFER-LIST* (REMQ interval *ZMACS-BUFFER-LIST*))
          (point-pdl-purge interval))))
    (dolist (pair (copylist *zmacs-buffer-name-alist*))
      (when (eq (get (cdr pair) :special-type) :info)
        (setq *zmacs-buffer-name-alist* (delq pair *zmacs-buffer-name-alist*))))
    (push (cons (buffer-name *interval*) *interval*) *zmacs-buffer-name-alist*)
    (cond ((null bury-p)
           (push *interval* *zmacs-buffer-list*)
           (dolist (w *all-zmacs-windows*)
             (let ((history (send w :buffer-history)))
               (push-on-history *interval* history))))
          (t
           (setq *zmacs-buffer-list* (append *zmacs-buffer-list* (list *interval*)))
           (dolist (w *all-zmacs-windows*)
             (let ((history (send w :buffer-history)))
               (append-remove-on-history *interval* history)))))))

(defun info-mode-hook ()
  (let ((element (assq 'info-mode *mode-list*)))
    (when element
      (push '(info-mouse-reset) (cadr element))
      (push '(info-hide-buffers) (cadr element))))
  (info-hide-buffers)
  (let ((sup (or (send *interval* :superior) *interval*)))
    (let ((aux (get sup 'aux-interval)))
      (when (and aux
                 (not (eq sup aux))
                 (or (bp-= (interval-first-bp sup) (interval-first-bp aux))
                     (bp-< (interval-first-bp sup) (interval-first-bp aux)))
                 (or (bp-= (interval-last-bp aux) (interval-last-bp sup))
                     (bp-< (interval-last-bp aux) (interval-last-bp sup))))
        (setq *interval* aux))))
  (mouse-highlight-always
    #'(lambda (window bp)
        (block top
          (labels ((check-item (item)
                               (let ((start (nth 2 item))
                                     (end (nth 3 item)))
                                 (when (and (or (bp-= start bp)
                                                (bp-< start bp))
                                            (bp-< bp end))
                                   (let ((chars-before (- (bp-index bp) (bp-index start))))
                                     (when (and (>= chars-before 0)
                                                (< chars-before (string-length (car item))))
                                       (return-from top
                                         (values (car item)
                                                 chars-before
                                                 (cadr item)
                                                 ))))))))
            (let ((interval (window-interval window)))
              (let ((sup (or (send interval :superior) interval))
                    (node-name (get interval 'canonical-node-name)))
                (let ((node-menu (assoc node-name (get sup 'menu) :test #'string-equal)))
                  (when (and node-menu
                             (integerp (cadr node-menu))
                             (= (cadr node-menu) (buffer-tick sup)))
                    (dolist (item (caddr node-menu))
                      (check-item item))))
                (let ((node-references (assoc node-name (get sup 'references) :test #'string-equal)))
                  (when (and node-references
                             (integerp (cadr node-references))
                             (= (cadr node-references) (buffer-tick sup)))
                    (dolist (item (caddr node-references))
                      (check-item item))))
                (let ((header-items (get interval 'header-items)))
                  (when header-items
                    (dolist (item header-items)
                      (check-item item))))
                )))))))

(defconst info-mode-hook 'info-mode-hook)

(defcom com-info-kill-buffer "kill buffer" ()
  (LET ((BUFFER (READ-BUFFER-NAME "Buffer to kill (Return to kill current buffer):"
                                  *INTERVAL*)))
    (when (and (typep buffer 'interval)
               (memq (or (send buffer :superior) buffer) *info-buffers*))
      (barf "Can't kill info buffers."))
    (KILL-BUFFER BUFFER))
  dis-bps)

(defcom com-info-save-and-exit "save and exit" ()
  (let ((*no-info-save* nil))
    (when (null (maybe-save-info-buffers t))
      (format *query-io* "~&(No buffers need saving.)")))
  (com-exit-control-r))


;;;


(defstruct (mouse-state (:type :named-array))
  global-mouse-char-blinker-handler
  global-mouse-char-blinker-documentation-string
  mouse-font-char
  mouse-x-offset
  mouse-y-offset
  )

(defun save-mouse-state ()
  (make-mouse-state
    :global-mouse-char-blinker-handler *global-mouse-char-blinker-handler*
    :global-mouse-char-blinker-documentation-string *global-mouse-char-blinker-documentation-string*
    :mouse-font-char *mouse-font-char*
    :mouse-x-offset *mouse-x-offset*
    :mouse-y-offset *mouse-y-offset*
    ))

(defun restore-mouse-state (ms)
  (check-type ms mouse-state)
  (setq *global-mouse-char-blinker-handler* (mouse-state-global-mouse-char-blinker-handler ms))
  (setq *global-mouse-char-blinker-documentation-string* (mouse-state-global-mouse-char-blinker-documentation-string ms))
  (setq *mouse-font-char* (mouse-state-mouse-font-char ms))
  (setq *mouse-x-offset* (mouse-state-mouse-x-offset ms))
  (setq *mouse-y-offset* (mouse-state-mouse-y-offset ms))
  )

(DEFUN mouse-highlight (read-function filter-function &aux (mouse-state (save-mouse-state)))
  (labels ((mouse-hook (window char ignore ignore)
             (when (char-equal char #\mouse-1-1)
               (multiple-value-bind (char x y line index)
                   (mouse-char window)
                 char x y
                 (when line
                   (let ((value (funcall filter-function (create-bp line index))))
                     (when value
                       (let ((interval (window-interval *mini-buffer-window*)))
                         (delete-interval interval)
                         (insert (interval-first-bp interval) value))
                       (throw 'return-from-command-loop value)))))))
           (command-hook (char-or-t)
             (cond ((or (eq char-or-t t)
                        (not (bp-= (interval-first-bp (window-interval *mini-buffer-window*))
                                   (interval-last-bp (window-interval *mini-buffer-window*)))))
                    (send *global-mouse-char-blinker* ':set-visibility nil)
                    (restore-mouse-state mouse-state)
                    (setq *mouse-hook* nil))
                   (t (setq *global-mouse-char-blinker-handler* #'blink
                            *global-mouse-char-blinker-documentation-string*
                            "Click left on highlighted name to select it."
                            *mouse-hook* #'mouse-hook
                            *mouse-font-char* 0 *mouse-x-offset* 4 *mouse-y-offset* 0)))
             (tv:mouse-set-blinker-definition :character *mouse-x-offset* *mouse-y-offset*
                                              :on :set-character *mouse-font-char*)
             (tv:mouse-wakeup))
           (blink (blinker window char x y line index)
                  (multiple-value-bind (string chars-before)
                      (funcall filter-function (create-bp line index))
                    (cond ((null string)
                           (tv:blinker-set-visibility blinker nil))
                          (t
                           (let ((sheet (window-sheet window)))
                             (tv:blinker-set-sheet blinker sheet)
                             (sheet-set-blinker-cursorpos
                               sheet blinker (- x (tv:sheet-string-length sheet string 0 chars-before)) y)
                             (tv:blinker-set-size blinker (tv:sheet-string-length sheet string)
                                                  (font-char-height (aref (tv:sheet-font-map sheet) (ldb %%ch-font char))))
                             (tv:blinker-set-visibility blinker t)))))))
  (let ((command-hook-name (gensym "MOUSE-READ-COMMAND-HOOK-" nil))
        (*post-command-hook* *post-command-hook*))
    (putprop command-hook-name 10000000 'command-hook-priority)
    (setf (symbol-function command-hook-name) #'command-hook)
    (setq *post-command-hook* (append *post-command-hook* (list command-hook-name)))
    (let ((*batch-undo-save* t))
      (delete-interval (window-interval *mini-buffer-window*)))
    (unwind-protect
        (progn (command-hook nil)
               (funcall read-function))
      (command-hook t)))))

(defun info-mouse-reset ()
  (send *global-mouse-char-blinker* ':set-visibility nil)
  (setq *global-mouse-char-blinker-handler* NIL
        *global-mouse-char-blinker-documentation-string* nil
        *mouse-font-char* 25.
        *mouse-x-offset* 8
        *mouse-y-offset* 0
        *mouse-hook* nil)
  (tv:mouse-set-blinker-definition :character *mouse-x-offset* *mouse-y-offset*
                                   :on :set-character *mouse-font-char*)
  (tv:mouse-wakeup)
  )

(DEFUN mouse-highlight-always (filter-function)
  (labels ((mouse-hook (window char ignore ignore)
             (when (char-equal char #\mouse-1-1)
               (multiple-value-bind (char x y line index)
                   (mouse-char window)
                 char x y
                 (when line
                   (multiple-value-bind (string nil node-full-name)
                       (funcall filter-function window (create-bp line index))
                     (when string
                       (send window :force-kbd-input `(:execute info-mouse-click ,node-full-name))))))))
           (blink (blinker window char x y line index)
                  (multiple-value-bind (string chars-before)
                      (funcall filter-function window (create-bp line index))
                    (cond ((null string)
                           (tv:blinker-set-visibility blinker nil))
                          (t
                           (let ((sheet (window-sheet window)))
                             (tv:blinker-set-sheet blinker sheet)
                             (sheet-set-blinker-cursorpos
                               sheet blinker (- x (tv:sheet-string-length sheet string 0 chars-before)) y)
                             (tv:blinker-set-size blinker (tv:sheet-string-length sheet string)
                                                  (font-char-height (aref (tv:sheet-font-map sheet) (ldb %%ch-font char))))
                             (tv:blinker-set-visibility blinker t)))))))
    (setq *global-mouse-char-blinker-handler* #'blink
          *mouse-hook* #'mouse-hook
          *mouse-font-char* 25.
          *mouse-x-offset* 8
          *mouse-y-offset* 0)
    (tv:mouse-set-blinker-definition :character *mouse-x-offset* *mouse-y-offset*
                                     :on :set-character *mouse-font-char*)
    (tv:mouse-wakeup)
    nil))

(defun any-atom-filter (bp)
  (let ((start (bp-index bp))
        (line (bp-line bp))
        end)
    (when (and (> (string-length line) 0)
               (< start (string-length line))
               (= (atom-word-syntax (char-code (aref line start))) word-alphabetic))
      (do ()
          ((< start 0)
           (incf start))
        (when (not (= (atom-word-syntax (char-code (aref line start))) word-alphabetic))
          (incf start)
          (return nil))
        (decf start))
      (setq end (bp-index bp))
      (do ((end-of-line (string-length line)))
          ((>= end end-of-line) (setq end end-of-line))
        (when (or (not (= (atom-word-syntax (char-code (aref line end))) word-alphabetic))
                  (= (char-code (aref line end)) #\:))
          (return nil))
        (incf end))
      (when (not (= start end))
        (values (substring line start end)
                (- (bp-index bp) start))))))


(defun test-mouse-highlight ()
  (mouse-highlight #'(lambda ()
                       (completing-read-from-mini-buffer "Foo:" '(("Bar") ("Xyz"))))
                   'any-atom-filter))
