;;; -*- Mode:LISP; Package:FILE-SYSTEM; Base:10; Readtable:ZL -*-
;;;
;;; hacks for fixing broken LMFS file systems.
;;;
;;;

(defconst *file-block-usage-map* nil)

(defun setup-file-block-usage-map ()
  (unless (and *file-block-usage-map*
               (= (array-length page-usage-table)
                  (array-length *file-block-usage-map*)))
    (setq *file-block-usage-map*
          (make-array (list (array-length page-usage-table) 2)
                      :type 'art-2b)))
  (fillarray *file-block-usage-map* nil))

(defun assess-put ()
  (let ((free 0)
        (reserved 0)
        (used 0)
        (dead 0))
    (do ((count 1 (add1 count)))
        ((= count (length page-usage-table))
         (format t "~%Free: ~D; Reserved: ~D; Used: ~D; Dead: ~D;"
                 free reserved used dead)
         (values free reserved used dead))
      (case (aref page-usage-table count)
        (0 (incf free))
        (1 (incf reserved))
        (2 (incf used))
        (3 (incf dead))))))

(defun record-page-usage-table-info (&optional (into *file-block-usage-map*))
  (do ((count 1 (add1 count))
       (len (length page-usage-table)))
      ((= count len))
    (setf (aref into count 0) (aref page-usage-table count))))

(defun record-current-pointers (&optional (file (dc-root-directory)))
  (let ((map (file-map file)))
    (dotimes (c (map-nblocks map))
      (setf (aref *file-block-usage-map* (map-block-location map c) 1) 3))
    (when (directory? file)
      (print file)
      (dolist (f (fs:read-directory-files file))
        (record-current-pointers f)))))

(defun number-of-valid-pointers (&optional (array *file-block-usage-map*))
  (let ((num 0))
    (do ((count 1 (add1 count)))
        ((= count (floor (array-length array) 2)) num)
      (when (= (aref array count 1) 3)
        (incf num)))))

(defun list-of-blocks-in-limbo (&optional (array *file-block-usage-map*))
  (let (list)
    (do ((count 1 (add1 count)))
        ((= count (floor (array-length array) 2)) (reverse list))
      (when (and (= (aref *file-block-usage-map* count 0) put-used)
                 (not (= (aref *file-block-usage-map* count 1) 3)))
        (push count list)))))


(defconst *review-documentation*
          "
<rubout>  : Next Block
<altmode> : Previous Block
<space>   : show whole block
<^c>      : change number of characters to display
<return>  : record this block as useful and go to next
<end>     : quit and return list of useful blocks
<help>    : print this info
")

(defun review-blocks (list &key
                      (unit 0)
                      (offset 0))
  (using-resource (rqb si:rqb 1 4)
    (let ((string (si:rqb-8-bit-buffer rqb))
          (chars-to-display 80)
          accepted-list)
      (do ((count 0 (add1 count))
           addr)
          ((= count (length list)) (reverse accepted-list))
        (setq addr (nth count list))
        (si:disk-read rqb unit (+ offset addr))
        (send *terminal-io* :clear-screen)
        (format t "[Relative block number: ~D]~2%" addr)
        (send *terminal-io* :string-out string 0 chars-to-display)
        (do (option
             finished?)
            (finished?)
          (setq option (tyi)
                finished?
                (case option
                  (#\rubout t)
                  (#\space (send *terminal-io* :clear-screen)
                           (format t "[Relative block number: ~D]~2%" addr)
                           (send *terminal-io* :string-out string))
                  (#\altmode (decf count (if (= count 0) 1 2)) t)
                  (#\control-c
                   (do ((num (prompt-and-read :number "~&Chars to display (1 to 1024)")
                             (prompt-and-read :number "~&Chars to display (1 to 1024)")))
                       ((and (< num 1025) (> num 0)) (setq chars-to-display num))))
                  (#\return (push addr accepted-list) t)
                  (#\end (return-from review-blocks (reverse accepted-list)))
                  (#\help (format t *review-documentation*))
                  (t (tv:beep) nil))))))))

;;; Directions for PW:
;;;
;;; (SETUP-FILE-BLOCK-USAGE-MAP)
;;; (RECORD-PAGE-USAGE-TABLE-INFO)
;;; (RECORD-CURRENT-POINTERS)
;;; (DEFCONST <some-var> (LIST-OF-BLOCKS-IN-LIMBO))
;;; (REVIEW-BLOCKS <some-var>)
;;;

(defun save-all-ascii (slist &optional (save-file "lam3:pw;blocks.save") (unit 0))
  (using-resource (rqb si:rqb 1 4)
    (let ((string (si:rqb-8-bit-buffer rqb)))
      (with-open-file (f save-file :direction :output)
        (do* ((l slist (cdr l))
              (addr (car l) (car l)))
             ((null addr))
          (print addr)
          (si:disk-read rqb unit (+ 162025 addr))
          (when (all-ascii string)
            (tyo #/!)
            (send f :string-out string)
            (send f :string-out "+---------------------------------------------------------+")))))))

(defun all-ascii (string)
  (dotimes (i 100. t)
    (let ((char (aref string i)))
      (unless (or (typep char '(integer 31 128))
                  (= char 141.))
        (return-from all-ascii nil)))))


(defmacro with-wired-block (var &body body)
  `(using-resource (,var si:rqb 1 4)
     (unwind-protect
         (progn
           (si:wire-disk-rqb ,var)
           ,@body)
       (when ,var
         (si:unwire-disk-rqb ,var)))))


(defun search-for-blocks-with-string (search-string &key
                                      block-status
                                      block-list
                                      at-position
                                      (search-offset 0)
                                      (partition-name "FILE")
                                      (disk-unit 0)
                                      &aux return-list)
  (check-type block-status (or null (integer 0 4)))
  (multiple-value-bind (start nil nil nil)
      (si:find-disk-partition partition-name nil disk-unit)
    (unless start
      (ferror nil "Invalid file partition name ~S on disk unit ~D"
              partition-name 0))
    (with-wired-block block
      (format t "~&Searching through pages: ")
      (let ((block-string (si:rqb-8-bit-buffer block))
            (xpos (car (cursorpos)))
            (ypos (cdr (cursorpos))))
        (if block-list
            (let ((length (length block-list))
                  (count 0))
              (dolist (page block-list)
                (incf count)
                (when (zerop (remainder count 100))
                  (cursorpos xpos ypos)
                  (cursorpos 'l)
                  (princ (- length count)))
                (si:disk-read block disk-unit (+ page start))
                (when (if at-position
                          (string-equal search-string block-string
                                        :start2 at-position
                                        :end2 (length search-string))
                        (string-search search-string block-string
                                       search-offset))
                  (format t "~&Found one: ~D" page)
                  (push page return-list))))
          (dotimes (page (length page-usage-table))
            (when (zerop (remainder page 1000))
              (cursorpos xpos ypos)
              (cursorpos 'l)
              (princ page))
            (when (or (null block-status)
                      (= (ar-1 page-usage-table page) block-status))
              (si:disk-read block 0 (+ page start))
              (when (if at-position
                        (string-equal search-string block-string
                                      :start2 at-position
                                      :end2 (length search-string))
                      (string-search search-string block-string
                                     search-offset))
                (format t "~&Found one: ~D" page)
                (push page return-list))))))))
  (tv:beep)
  (reverse return-list))


(defun dump-lm-blocks-to-file (filename page-list)
  (with-open-file (f filename
                     :direction :output
                     :characters t
                     :byte-size 8)
    (with-wired-block block
      (let ((block-string (si:rqb-8-bit-buffer block)))
        (dolist (page page-list)
          (si:disk-read block 0 (+ page lm-partition-base))
          (format f "~&#D~D~%~S~2%"
                  page
                  block-string)))))
  (tv:beep))

(defun look-at-directory-entries (directory)
  (let ((entries '()))
    (with-map-stream-in (stream (file-map (lookup-directory directory)))
      (loop
        (let ((entry (read-directory-entry stream directory)))
          (cond (entry
                 (format t "~&Got entry: ~S" entry)
                 (describe entry)
                 (push entry entries))
                (t
                 (format t "~%End of directory ~A." (directory-name directory))
                 (return (nreverse entries)))))))))

(defun read-directory-entries-from-file (file directory)
  (with-open-file (f file :direction :input)
    (do (entries
         (entry (read-directory-entry f directory)
                (read-directory-entry f directory)))
        ((null entry) (reverse entries))
      (push entry entries))))

(defun view-directory-map-stream (directory)
  (with-map-stream-in (stream (file-map (lookup-directory directory)))
    (stream-copy-until-eof stream *standard-output*)))

(defun remove-equal-blocks (block-list &key
                            (offset lm-partition-base)
                            (disk-unit 0))
  (with-wired-block block
    (let ((block-string (si:rqb-8-bit-buffer block))
          return-list
          string-list)
      (dolist (block-addr block-list)
        (si:disk-read block disk-unit (+ block-addr offset))
        (block search
          (dolist (str string-list
                       (let ((str (make-string 1024)))
                         (copy-array-contents block-string str)
                         (push str string-list)
                         (push block-addr return-list)))
            (when (string-equal str block-string)
              (format t "~& one equal block found")
              (return-from search nil))))))))

(defun revert-directory (directory)
  (setf (directory-files directory) :disk)
  (read-directory-files directory))

(defun check-directory-tree (&optional (directory (dc-root-directory)))
  (dolist (file (read-directory-files directory))
    (when (directory? file)
      (print file)
      (check-directory-tree file))))

(defun check-files-for-top-level-atoms (file-list)
  (let ((return-list))
    (dolist (file file-list return-list)
      (case (send (car file) :canonical-type)
        (:lisp
         (with-open-file (f (car file))
           (format t "~&Checking /"~A/" ... " (car file))
           (condition-case ()
               (do-forever
                 (unless (consp (read f))
                   (ferror nil "")))
             (fs:end-of-file (format t "[GOOD]"))
             (error
              (tv:beep)
              (format t "[BAD]")
              (push (car file) return-list)))))))))

;;;; filesystem excerciser

(defmacro with-abort-catch-and-notify (((format-string &rest format-args) &rest abort-forms) &body body)
  `(condition-case ()
       (progn . ,body)
     (sys:abort (format t ,format-string . ,format-args)
                . ,abort-forms)))

(defun get-random-directory ()
  (nth (random 4)
       '("lmfs-temp" ("lmfs-temp" "temp1") ("lmfs-temp" "temp2") ("lmfs-temp" "temp3"))))

(defun get-random-canonical-type ()
  (nth (random 3) '(:lisp :text :qfasl)))

(defun create-files-randomly (number-of-files existing-pathnames &optional all-new-pathnames)
  (using-resource (buf si:dma-buffer 200)
    (fill (si:dma-buffer-string buf) #/?)
    (dotimes (c number-of-files existing-pathnames)
      (let ((pathname (if (and (not all-new-pathnames)
                               (> (length existing-pathnames) 20)
                               (< (random 5) 3))
                          (let ((op (nth (random (length existing-pathnames)) existing-pathnames)))
                            (if (zerop (random 2))
                                op
                              (send op :new-version :newest)))
                        (fs:make-pathname :host si:local-host
                                          :directory (get-random-directory)
                                          :name (substring (gensym "tmpfil") 0)
                                          :canonical-type (get-random-canonical-type)))))
        (with-abort-catch-and-notify (("~&Aborted opening file ~A (during create-random-file)"
                                       pathname)
                                      (push (cons :open-for-output pathname) *lmfs-files-aborted*))
          (with-open-file (f pathname :direction :output)
            (with-abort-catch-and-notify (("~&Aborted writing data into ~A (in create-files-randomly)"
                                           pathname)
                                          (push (cons :write-data (send f :truename)) *lmfs-files-aborted*))
              (format t "~&Creating random file: ~A" (send f :truename))
              (pushnew (send f :truename) existing-pathnames)
              (let ((factor (// (send f :byte-size) 8)))
                (send f :string-out
                      (case factor
                        (1 (si:dma-buffer-string buf))
                        (2 (si:dma-buffer-16b buf)))
                      0
                      (random (// (* 200 1024.) factor)))))))))))

(defun delete-files-randomly (number-of-files-to-delete file-list)
  (dotimes (count number-of-files-to-delete file-list)
    (let ((file (nth (random (length file-list)) file-list)))
      (format t "~&Deleting random file: ~A" file)
      (with-abort-catch-and-notify (("~&Aborted deleting file ~A (delete-files-randomly)"
                                     file)
                                    (push (cons :delete file) *lmfs-files-aborted*)
                                    (setq file-list (delq file file-list)))
        (fs:delete-file file)
        (setq file-list (delq file file-list))))))

(defun open-files-randomly (number-of-files-to-open file-list)
  (dotimes (count number-of-files-to-open)
    (let ((file (nth (random (length file-list)) file-list)))
      (with-abort-catch-and-notify (("~&Aborted opening file ~A (open-files-randomly)" file)
                                    (push (cons :open file) *lmfs-files-aborted*))
        (with-open-file (f file)
          (with-abort-catch-and-notify (("~&Aborted with file open ~A (open-files-randomly)" file)
                                        (push (cons :file-open file) *lmfs-files-aborted*))
            (format t "~&Opening random file: ~A" (send f :truename))
            f))))))

(defun expunge-random-directories ()
  (dotimes (times (random 4))
    (let ((directory (get-random-directory)))
      (format t "~&Expunging random directory: ~A" directory)
      (with-abort-catch-and-notify (("~&Aborted during expunge of directory: ~A" directory)
                                    (push (cons :expunge-directory directory) *lmfs-files-aborted*))
        (fs:expunge-directory (fs:make-pathname :host si:local-host
                                                :directory directory
                                                :name :wild
                                                :type :wild
                                                :version :wild))))))

(defun exercise-file-system (&key run-abort-process times output-file
                             &aux existing-files)
  (with-open-stream (*standard-output* (if output-file
                                       (si:make-broadcast-stream
                                         (open output-file :direction :output)
                                         *standard-output*)
                                     *standard-output*))
    (let ((*lmfs-active-debug-modes* '(:check-directory-maps)))
      (when (yes-or-no-p
              "~&*** Warning: This function may crash your filesystem!!! Go ahead?")
        (fs:lm-salvage)
        (fs:create-directory "lm:lmfs-temp;")
        (fs:create-directory "lm:lmfs-temp.temp1;")
        (fs:create-directory "lm:lmfs-temp.temp2;")
        (fs:create-directory "lm:lmfs-temp.temp3;")
        (when run-abort-process
          (run-abort-process si:current-process))
        (dotimes (count (or times 1))
          (with-abort-catch-and-notify (("~&Aborted during excerciser loop"))
            (setq existing-files
                  (create-files-randomly (+ 20 (random 30)) existing-files))
            (open-files-randomly (floor (length existing-files) 2) existing-files)
            (setq existing-files
                  (delete-files-randomly (round (* (length existing-files) (// (random 6) 10.0)))
                                         existing-files))
            (open-files-randomly (floor (length existing-files) 2) existing-files)
            (expunge-random-directories)))
        (kill-abort-process)
        (format t "~&Deleting all files ...")
        (dolist (file existing-files)
          (fs:deletef file))
        (fs:expunge-directory "lm:lmfs-temp.temp3;*.*#*")
        (fs:expunge-directory "lm:lmfs-temp.temp2;*.*#*")
        (fs:expunge-directory "lm:lmfs-temp.temp1;*.*#*")
        (dolist (dir '(("lmfs-temp" "temp1") ("lmfs-temp" "temp2") ("lmfs-temp" "temp3")))
          (fs:lmfs-delete-empty-directory (fs:lookup-directory dir) nil))
        (fs:expunge-directory "lm:lmfs-temp;*.*#*")))))

(defconst *lmfs-exercise-abort-signalled* ())

(defconst *lmfs-files-aborted* ())

(defconst *abort-process-running* ())

(defconst *abort-process-probability* 500)

(defun signal-abort-p ()
  (and (null *lmfs-exercise-abort-signalled*)
       (zerop *abort-process-probability*)))

(defun abort-process (process)
  (do-forever
    (catch-error-restart (error "")
      (process-wait "Waiting to Abort" 'signal-abort-p)
      (send process :interrupt 'signal 'sys:abort :format-string "Randomly aborted..."))))

(defun run-abort-process (for-process)
  (kill-abort-process)
  (setq *abort-process-running*
        (process-run-function "Random Abort Generator"
                              'abort-process
                              for-process)))

(defun kill-abort-process ()
  (when *abort-process-running*
    (send *abort-process-running* :kill)))
