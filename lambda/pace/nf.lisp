;;; -*- Mode:LISP; Package:NF; Base:10; Readtable:CL -*-

(defvar *cluster-writes* 0)

(defmacro with-cluster ((cluster part cluster-number) &body body)
  `(let (,cluster)
     (unwind-protect
         (progn
           (setq ,cluster (get-cluster ,part ,cluster-number))
           ,@body)
       (when ,cluster
         (free-cluster ,cluster)))))

(defstruct (part (:type :named-array))
  name
  unit-name
  unit
  start-block
  last-block
  blocks-per-cluster
  n-clusters
  clusters
  clusters-scan-pointer
  cluster-usage-table
  string-table
  reverse-string-table
  cluster-usage-table-modified
  )

(defselect-incremental (:property part si:named-structure-invoke))

(defun (:select-method (:property part si:named-structure-invoke) :print-self) (ignore part stream ignore ignore)
  (si:printing-random-object (part stream :type)
    (format stream "Part ~s Unit ~s" (part-name part) (part-unit-name part))))

(defun create-part (name unit-name unit start-block last-block blocks-per-cluster)
  (make-part :name name
             :unit-name unit-name
             :unit unit
             :start-block start-block
             :last-block last-block
             :blocks-per-cluster blocks-per-cluster
             :n-clusters (floor (- last-block start-block) blocks-per-cluster)
             :clusters nil
             :clusters-scan-pointer nil
             :cluster-usage-table nil
             :string-table nil
             :reverse-string-table nil
             :cluster-usage-table-modified nil
             ))

(defvar *part-list* nil)

(defun reset ()
  (setq *cluster-writes* 0)
  (setq *part-list* nil))

(defstruct (cluster (:type :named-array))
  part
  number
  modified-p
  rqb
  age
  )

(defselect-incremental (:property cluster si:named-structure-invoke))

(defun (:select-method (:property cluster si:named-structure-invoke) :print-self) (ignore cluster stream ignore ignore)
  (si:printing-random-object (cluster stream :type)
    (format stream "Cluster-number ~d." (cluster-number cluster))
    (when (cluster-modified-p cluster)
      (format stream " *"))
    (format stream " Age ~d." (cluster-age cluster))))

(defun create-cluster (part)
  (make-cluster :part part
                :number nil
                :modified-p nil
                :rqb (si:get-disk-rqb (part-blocks-per-cluster part))
                :age 0
                ))

(defconst *clusters-per-part* 5.)

(defun find-part (name unit)
  (check-type name string)
  (check-type unit (or string integer))
  (labels ((same-unit-p (u1 u2)
                        (cond ((stringp u1)
                               (cond ((stringp u2)
                                      (string-equal u1 u2))
                                     (t
                                      nil)))
                              (t
                               (cond ((stringp u2)
                                      nil)
                                     (t
                                      (= u1 u2)))))))
    (dolist (part *part-list*)
      (when (and (string-equal (part-name part) name)
                 (same-unit-p (part-unit-name part) unit))
        (return part)))))

(defconst *default-blocks-per-cluster* 4)

(defun start ()
  (let ((part (establish-part "nfil" 0)))
    (build-cluster-usage-table part)
    (make-initial-string-table part)
    part))

(defun establish-part (name unit)
  (check-type name string)
  (check-type unit (or string integer))
  (when (find-part name unit)
    (ferror nil "already know about part ~s on unit ~s" name unit))
  (let ((u (si:decode-unit-argument unit "New File System" nil t)))
    (multiple-value-bind (first-block n-blocks)
        (si:find-disk-partition-for-read name nil u nil nil)
      (when (null first-block)
        (si:dispose-of-unit u)
        (setq u nil)
        (ferror nil "can't find part ~s on unit ~s" name unit))
      (let ((part (create-part name unit u first-block (+ first-block n-blocks) *default-blocks-per-cluster*)))
        (setf (part-clusters part)
              (loop for i from 0 below *clusters-per-part*
                    collect (create-cluster part)))
        (push part *part-list*)
        part))))

(defun validate-part-and-cluster-number (part cluster-number)
  (check-type part part)
  (when (null (memq part *part-list*))
    (ferror nil "obsolete part ~s" part))
  (when (or (< cluster-number 0)
            (>= cluster-number (part-n-clusters part)))
    (ferror nil "out of range cluster number ~d" cluster-number)))

(defun validate (part cluster cluster-number)
  (validate-part-and-cluster-number part cluster-number)
  (when (null (memq cluster (part-clusters part)))
    (ferror nil "stray cluster ~s" cluster))
  (when (not (eq (cluster-part cluster) part))
    (ferror nil "unconnected cluster ~s" cluster)))

(defun get-cluster (part cluster-number)
  (validate-part-and-cluster-number part cluster-number)
  (let (cluster first-empty oldest)
    (when (null (part-clusters-scan-pointer part))
      (setf (part-clusters-scan-pointer part) (part-clusters part)))
    (do ((clusters (part-clusters-scan-pointer part) (or (cdr clusters) (part-clusters part)))
         (first-time t nil))
        ((and (not first-time)
              (eq clusters (part-clusters-scan-pointer part))))
      (cond ((null (cluster-number (car clusters)))
             (setq first-empty clusters))
            ((= (cluster-number (car clusters)) cluster-number)
             (when (not (numberp (cluster-age (car clusters))))
               (ferror nil "alredy checked out"))
             (setq cluster (car clusters))
             (return nil))
            ((numberp (cluster-age (car clusters)))
             (incf (cluster-age (car clusters)))
             (cond ((null oldest)
                    (setq oldest clusters))
                   ((> (cluster-age (car clusters)) (cluster-age (car oldest)))
                    (setq oldest clusters))))))
    (when (null cluster)
      (cond ((null first-empty)
             (cond ((null oldest)
                    (ferror nil "no clusters"))
                   (t
                    (setq cluster (car oldest))
                    (setf (part-clusters-scan-pointer part) (cdr oldest))
                    (when (cluster-modified-p cluster)
                      (cluster-write part cluster))
                    (cluster-read part cluster cluster-number))))
            (t
             (setq cluster (car first-empty))
             (setf (part-clusters-scan-pointer part) (cdr first-empty))
             (cluster-read part cluster cluster-number))))
    (setf (cluster-age cluster) :user)
    cluster))

(defun free-cluster (cluster)
  (setf (cluster-age cluster) 0))

(defun cluster-read (part cluster cluster-number)
  (validate part cluster cluster-number)
  (setf (cluster-number cluster) nil)
  (si:disk-read (cluster-rqb cluster)
                (part-unit part)
                (+ (* cluster-number (part-blocks-per-cluster part))
                   (part-start-block part)))
  (setf (cluster-number cluster) cluster-number)
  (setf (cluster-modified-p cluster) nil)
  cluster)

(defun cluster-write (part cluster)
  (incf *cluster-writes*)
  (let ((cluster-number (cluster-number cluster)))
    (validate part cluster cluster-number)
    (let ((disk-address (+ (* cluster-number (part-blocks-per-cluster part))
                           (part-start-block part))))
      (when (or (< disk-address (part-start-block part))
                (> (+ disk-address (si:rqb-npages (cluster-rqb cluster))) (part-last-block part)))
        (ferror nil "out of bounds"))
      (si:disk-write (cluster-rqb cluster) (part-unit part) disk-address))
    (setf (cluster-modified-p cluster) nil)
    cluster))

(defun sync ()
  (dolist (part *part-list*)
    (dolist (cluster (part-clusters part))
      (when (cluster-modified-p cluster)
        (cluster-write part cluster)))))

(defun nfh-ref (array index)
  (let ((d (* index 2)))
    (without-interrupts
      (dpb (aref array (1+ d))
           (byte 16. 16.)
           (aref array d)))))

(defsetf nfh-ref (array index) (val)
  `(let ((d (* ,index 2)))
     (without-interrupts
       (aset (ldb (byte 16. 0) ,val) ,array d)
       (aset (ldb (byte 16. 16.) ,val) ,array (1+ d)))
     ,val))

(defconst %nfh-data-start 64.)

(defconst nfh-fields '(nfh-cluster
                        nfh-file-id
                        nfh-cluster-of-file
                        nfh-next-cluster
                        nfh-prev-cluster
                        nfh-file-size
                        nfh-file-original-author
                        nfh-file-author
                        nfh-creation-date
                        nfh-write-date
                        nfh-access-date
                        nfh-link-count
                        nfh-cluster-checksum

                        nfh-directory-id
                        nfh-directory-name
                        nfh-name
                        nfh-type

                        ))

(defmacro create-nfh-macros ()
  (when (> (length nfh-fields) %nfh-data-start)
    (ferror nil "oops"))
  (cons 'progn
        (do ((i 0 (1+ i))
             (fields nfh-fields (cdr fields))
             result)
            ((null fields)
             (reverse result))
          (push `(defmacro ,(car fields) (array) `(nfh-ref ,array ,,i)) result))))

(create-nfh-macros)

(defun cluster->nfh (cluster)
  (si:rqb-buffer (cluster-rqb cluster)))

(defun initialize-file-system (part)
  (dotimes (i (part-n-clusters part))
    (with-cluster (cluster part i)
      (let ((nfh (cluster->nfh cluster)))
        (setf (nfh-cluster nfh) i)
        (setf (nfh-file-id nfh) 0)
        (setf (nfh-cluster-checksum nfh) 0))
      (setf (cluster-modified-p cluster) t)))
  (sync))

(defconst %cut-bad 0)
(defconst %cut-free 1)
(defconst %cut-reserved 2)
(defconst %cut-used 3)

(defun build-cluster-usage-table (part)
  (let ((cut (make-array (part-n-clusters part) :type :art-2b :initial-element %cut-bad)))
    (dotimes (i (part-n-clusters part))
      (with-cluster (cluster part i)
        (let ((nfh (cluster->nfh cluster)))
          (cond ((zerop (nfh-file-id nfh))
                 (setf (aref cut i) %cut-free))
                (t
                 (setf (aref cut i) %cut-used))))))
    (setf (part-cluster-usage-table part) cut)
    (setf (part-cluster-usage-table-modified part) nil)))

(defun allocate-cluster (part status)
  (let ((cut (part-cluster-usage-table part)))
    (dotimes (i (array-length cut))
      (when (= (aref cut i) %cut-free)
        (setf (aref cut i) status)
        (setf (part-cluster-usage-table-modified part) t)
        (return i)))))

(defconst *initial-strings*
          '("ROOT"
            "TOP"
            "BACKUP-TOP"
            "STRING-TABLE"
            "BACKUP-STRING-TABLE"
            "CLUSTER-USAGE-TABLE"
            "BACKUP-CLUSTER-USAGE-TABLE"
            "DIRECTORY"
            ))

(defun make-initial-string-table (part)
  (let ((table (make-hash-table :test 'eq))
        (rtable (make-hash-table :test 'string=)))
    (do ((i 1 (1+ i))
         (strings *initial-strings* (cdr strings)))
        ((null strings))
      (puthash i (car strings) table)
      (puthash (car strings) i rtable))
    (setf (part-string-table part) table)
    (setf (part-reverse-string-table part) rtable)))

(defun string-number->string (part string-number)
  (let ((string (gethash string-number (part-string-table part))))
    (when (null string)
      (ferror nil "unknown string number ~d." string-number))
    string))

(defun string->string-number (part string create-p)
  (let ((n (gethash string (part-reverse-string-table part))))
    (cond ((null n)
           (when (null create-p)
             (ferror nil "can't find string ~s" string))
           (ferror nil "don't know how to create yet"))
          (t
           n))))

;file id 1 is the root directory, and it starts in block 0
;it is only allowed to contain the following files:
;        2 -> top directory
;        3 -> backup top directory
;        4 -> string table
;        5 -> backup string table
;        6 -> cluster usage table
;        7 -> backup cluster usage table
;        8 -> file-id to first cluster
;        9 -> backup file-id to first cluster

(defun create-root-directory (part)
  (setf (aref (part-cluster-usage-table part) 0) %cut-used)
  (let ((cluster-number 1))
    (setf (aref (part-cluster-usage-table part) cluster-number) %cut-used)
    (with-cluster (cluster part cluster-number)
      (let ((nfh (cluster->nfh cluster)))
        (array-initialize nfh 0)
        (setf (nfh-cluster nfh) 1)
        (setf (nfh-file-id nfh) 1)
        (setf (nfh-cluster-of-file nfh) 0)
        (setf (nfh-next-cluster nfh) 0)
        (setf (nfh-prev-cluster nfh) 0)
        (setf (nfh-file-size nfh) 0)
        (setf (nfh-file-original-author nfh) 0)
        (setf (nfh-file-author nfh) 0)
        (let ((time (get-universal-time)))
          (setf (nfh-creation-date nfh) time)
          (setf (nfh-write-date nfh) time)
          (setf (nfh-access-date nfh) time))
        (setf (nfh-link-count nfh) 1)
        (setf (nfh-cluster-checksum nfh) 0)

        (setf (nfh-directory-id nfh) 1)
        (setf (nfh-directory-name nfh) 0)
        (setf (nfh-name nfh) 0)
        (setf (nfh-type nfh) 0))
      (setf (cluster-modified-p cluster) t)))
  (sync))

(defun print-cluster (part cluster-number)
  (format t "~&Cluster ~d. has:" cluster-number)
  (with-cluster (cluster part cluster-number)
    (print-cluster-data cluster)))

(defun print-cluster-data (cluster)
  (let ((nfh (cluster->nfh cluster)))
    (format t "~&  cluster: ~s" (nfh-cluster nfh))
    (format t "~&  file-id: ~s" (nfh-file-id nfh))
    (format t "~&  next-cluster: ~s" (nfh-next-cluster nfh))
    (format t "~&  prev-cluster: ~s" (nfh-prev-cluster nfh))
    (format t "~&  file-size: ~s" (nfh-file-size nfh))
    (format t "~&  original-author: ~s" (nfh-file-original-author nfh))
    (format t "~&  author: ~s" (nfh-file-author nfh))
    (format t "~&  creation-date: ~\\time\\" (nfh-creation-date nfh))
    (format t "~&  write-date: ~\\time\\" (nfh-write-date nfh))
    (format t "~&  access-date: ~\\time\\" (nfh-access-date nfh))
    (format t "~&  link-count: ~s" (nfh-link-count nfh))
    (format t "~&  checksum: ~s" (nfh-cluster-checksum nfh))
    (format t "~&  directory-id: ~s" (nfh-directory-id nfh))
    (format t "~&  directory-name: ~s" (nfh-directory-name nfh))
    (format t "~&  name: ~s" (nfh-name nfh))
    (format t "~&  type: ~s" (nfh-type nfh))
    ))


;;;;


(defun file-offset->cluster-number (part file-cluster-number file-offset)
  (let ((cluster-number file-cluster-number)
        (prev-cluster-number nil))
    (dotimes (i file-offset)
      (with-cluster (cluster part cluster-number)
        (setq prev-cluster-number cluster-number)
        (setq cluster-number (nfh-next-cluster (cluster->nfh cluster))))
      (cond ((zerop cluster-number)
             (setq cluster-number nil)
             (return nil))
            (t
             (ferror nil "file doesn't have enough blocks"))))
    (values cluster-number file-cluster-number)))

(defflavor cluster-stream
         (part
          data-bytes-per-cluster
          file-cluster-number
          (file-size 0)
          (modified-p nil)
          (current-cluster nil)
          (file-offset 0)
          buffer
          (buffer-base 0)
          buffer-limit
          (buffer-pointer (* %nfh-data-start 4))
          )
         ()
  :settable-instance-variables)

(defun make-cluster-stream (part file-cluster-number)
  (make-instance 'cluster-stream
                 :part part
                 :file-cluster-number file-cluster-number))

(defmethod (cluster-stream :after :init) (ignore)
  (when (< file-cluster-number 1)
    (ferror nil "bad first-cluster-number"))
  (setq data-bytes-per-cluster (- (* 1024. (part-blocks-per-cluster part)) (* 4 %nfh-data-start)))
  (with-cluster (cluster part file-cluster-number)
    (setq file-size (nfh-file-size (cluster->nfh cluster))))
  (send self :setup-for-file-offset nil)
  )

(defmethod (cluster-stream :flush-cluster) ()
  (when current-cluster
    (when modified-p
      (cluster-write part current-cluster)
      (setq modified-p nil))
    (free-cluster (prog1 current-cluster (setq current-cluster nil)))))

(defmethod (cluster-stream :sync) ()
  (when (and current-cluster modified-p)
    (setf (cluster-modified-p current-cluster) t)
    (cluster-write part current-cluster)
    (setq modified-p nil)))

(defun cluster->user-data (cluster)
  (si:rqb-8-bit-buffer (cluster-rqb cluster)))

(defmethod (cluster-stream :setup-for-file-offset) (extend)
  (send self :sync)
  (when current-cluster
    (free-cluster (prog1 current-cluster (setq current-cluster nil))))
  (multiple-value-bind (offset byte-offset)
      (floor (+ buffer-base buffer-pointer (- (* 4 %nfh-data-start))) data-bytes-per-cluster)
    (setq file-offset offset)
    (setq buffer-base (* file-offset data-bytes-per-cluster))
    (setq buffer-pointer (+ (* 4 %nfh-data-start) byte-offset)))
  (multiple-value-bind (cluster-number prev-cluster-number)
      (file-offset->cluster-number part file-cluster-number file-offset)
    (when (null cluster-number)
      prev-cluster-number
      (when (null extend)
        (ferror nil "ran off end of file"))
      ;;allocate a new cluster, attach it to prev-cluster-number
      ;;and set cluster-number to it
      (ferror nil "foo"))
    (setq current-cluster (get-cluster part cluster-number))
    (setq buffer (cluster->user-data current-cluster))
    (setq buffer-limit (+ (* 4 %nfh-data-start)
                          (min data-bytes-per-cluster
                               (- file-size buffer-base))))))

(defmethod (cluster-stream :seek) (n)
  (when (> n file-size)
    (ferror nil "seek too far"))
  (send self :flush-cluster)
  (setq buffer-base n)
  (setq buffer-pointer (* 4 %nfh-data-start))
  (send self :setup-for-file-offset nil))

(defmethod (cluster-stream :tyi-32) ()
  (let* ((b0 (send self :tyi))
         (b1 (send self :tyi))
         (b2 (send self :tyi))
         (b3 (send self :tyi)))
    (+ b0
       (dpb b1 (byte 8 8) 0)
       (dpb b2 (byte 8 16.) 0)
       (dpb b3 (byte 8 24.) 0))))


(defmethod (cluster-stream :tyi) (&optional eof-action)
  (cond ((>= (+ buffer-base buffer-pointer (- (* 4 %nfh-data-start))) file-size)
         (when eof-action
           (ferror 'sys:end-of-file-1 "End of file on ~s." self))
         nil)
        (t
         (when (or (null current-cluster)
                   (>= buffer-pointer buffer-limit))
           (send self :setup-for-file-offset nil))
         (when (>= buffer-pointer buffer-limit)
           (ferror nil "didn't get any new data"))
         (aref buffer (prog1 buffer-pointer (incf buffer-pointer))))))

(defmethod (cluster-stream :listen) ()
  (< (+ buffer-base buffer-pointer) file-size))

(defmethod (cluster-stream :read-until-eof) ()
  (send self :seek file-size))

(defmethod (cluster-stream :clear-input) ()
  )

(defmethod (cluster-stream :close) (&optional abort)
  abort
  (send self :flush-cluster)
  (with-cluster (cluster part file-cluster-number)
    (let ((nfh (cluster->nfh cluster)))
      (let ((old-file-size (nfh-file-size nfh)))
        (cond ((< file-size old-file-size)
               (ferror nil "file-size shrunk??"))
              ((= file-size old-file-size))
              (t
               (setf (nfh-file-size nfh) file-size)
               (setf (cluster-modified-p cluster) t)))))))

(defmethod (cluster-stream :get-buffer) (&optional eof-action extend)
  (cond ((>= (+ buffer-base buffer-pointer (- (* 4 %nfh-data-start))) file-size)
         (when eof-action
           (ferror 'sys:end-of-file-1 "End of file on ~s." self))
         nil)
        (t
         (when (or (null current-cluster)
                   (>= buffer-pointer buffer-limit))
           (send self :setup-for-file-offset extend))
         (when (>= buffer-pointer buffer-limit)
           (ferror nil "didn't get any new data"))
         (values buffer buffer-pointer buffer-limit))))

(defmethod (cluster-stream :advance-pointer) (pointer)
  (when (or (< pointer buffer-pointer)
            (> pointer buffer-limit))
    (ferror nil "out of range"))
  (setq buffer-pointer pointer))

(defmethod (cluster-stream :read-pointer) ()
  (+ buffer-base buffer-pointer (- (* 4 %nfh-data-start))))

(defmethod (cluster-stream :return-buffer) (buf end modified)
  buf
  (when modified
    (setq modified-p t))
  (when (or (< end buffer-pointer)
            (> end buffer-limit))
    (ferror nil "out of range"))
  (setq buffer-pointer end))

(defmethod (cluster-stream :string-in) (eof string &optional (start 0) end)
  (when (null end)
    (setq end (array-length string)))
  (do ()
      (())
    (when (>= start end)
      (when (array-has-fill-pointer-p string)
        (setf (fill-pointer string) start))
      (return (values start (not (= start end)))))
    (cond ((>= (+ buffer-base buffer-pointer (- (* 4 %nfh-data-start))) file-size)
           (when eof
             (ferror 'sys:end-of-file-1 "End of file on ~s." self))
           (when (array-has-fill-pointer-p string)
             (setf (fill-pointer string) start))
           (return (values nil t)))
          (t
           (when (or (null current-cluster)
                     (>= buffer-pointer buffer-limit))
             (send self :setup-for-file-offset nil))
           (when (>= buffer-pointer buffer-limit)
             (ferror nil "didn't get any new data"))
           (let ((bytes (min (- end start)
                             (- buffer-limit buffer-pointer))))
             (copy-array-portion buffer buffer-pointer (setq buffer-pointer (+ buffer-pointer bytes))
                                 string start (setq start (+ start bytes))))))))

(defmethod (cluster-stream :tyo-32) (word)
  (send self :tyo (ldb (byte 8 0) word))
  (send self :tyo (ldb (byte 8 8) word))
  (send self :tyo (ldb (byte 8 16.) word))
  (send self :tyo (ldb (byte 8 24.) word))
  word)


(defmethod (cluster-stream :tyo) (c)
  (when (null current-cluster)
    (send self :setup-for-file-offset t))
  (when (>= buffer-pointer buffer-limit)
    (cond ((< buffer-limit (* 1024. (part-blocks-per-cluster part)))
           (incf buffer-limit))
          (t
           (ferror nil "didn't get any space")))
    (incf file-size))
  (when (>= buffer-pointer buffer-limit)
    (ferror nil "didn't get any room"))
  (setf (aref buffer (prog1 buffer-pointer (incf buffer-pointer))) c)
  (setq modified-p t)
  c)

(defmethod (cluster-stream :force-output) ()
  )

(defmethod (cluster-stream :string-out) (string &optional (start 0) end)
  (when (null end)
    (setq end (array-active-length string)))
  (when (null current-cluster)
    (send self :setup-for-file-offset t))
  (do ()
      ((< start end))
    (when (>= buffer-pointer buffer-limit)
      (cond ((< buffer-limit (* 1024. (part-blocks-per-cluster part)))
             (let ((new-limit (min (+ buffer-limit (- end start))
                                   (* 1024. (part-blocks-per-cluster part)))))
               (incf file-size (- new-limit buffer-limit))
               (setq buffer-limit new-limit)))
            (t
             (send self :setup-for-file-offset t)
             (incf file-size (min (- end start)
                                  (- buffer-limit buffer-pointer)))
             )))
    (when (>= buffer-pointer buffer-limit)
      (ferror nil "didn't get any room"))
    (let ((bytes (min (- end start)
                      (- buffer-limit buffer-pointer))))
      (copy-array-portion string start (setq start (+ start bytes))
                          buffer buffer-pointer (setq buffer-pointer (+ buffer-pointer bytes)))
      ))
  (setq modified-p t))

(defun get-32-bits-from-string (string index)
  (+ (aref string index)
     (dpb (aref string (+ index 1)) (byte 8 8) 0)
     (dpb (aref string (+ index 2)) (byte 8 16.) 0)
     (dpb (aref string (+ index 3)) (byte 8 24.) 0)))

(defsetf get-32-bits-from-string (string index) (val)
  (once-only (string index val)
    `(progn
       (aset (ldb (byte 8 0) ,val) ,string ,index)
       (aset (ldb (byte 8 8) ,val) ,string (+ ,index 1))
       (aset (ldb (byte 8 16.) ,val) ,string (+ ,index 2))
       (aset (ldb (byte 8 24.) ,val) ,string (+ ,index 3)))))

(defun primitive-lookup (op file-id part dir-cluster-number name type version)
  (when (and (eq version :next)
             (not (eq op :create)))
    (ferror nil "version :NEXT must only be used with :CREATE"))
  (with-open-stream (dir-stream (make-cluster-stream part dir-cluster-number))
    (when (not (zerop (remainder (send dir-stream :file-size) 16.)))
      (ferror nil "odd length directory"))
    (let (first-empty found-it (max-version 0) position)
      (do-named top
                ()
                (())
        (multiple-value-bind (buffer start end)
            (send dir-stream :get-buffer)
          (cond ((null buffer)
                 (return-from top nil))
                ((or (not (zerop (remainder start 16.)))
                     (not (zerop (remainder end 16.))))
                 (ferror nil "bad directory buffer"))
                (t
                 (do ((es start (+ es 16.)))
                     ((>= es end)
                      (send dir-stream :advance-pointer end))
                   (cond ((zerop (get-32-bits-from-string buffer es))
                          (when (null first-empty)
                            (send dir-stream :advance-pointer es)
                            (setq first-empty (send dir-stream :read-pointer))))
                         ((and (= (get-32-bits-from-string buffer (+ es 4)) name)
                               (= (get-32-bits-from-string buffer (+ es 8)) type))
                          (let ((this-version (get-32-bits-from-string buffer (+ es 12.))))
                            (cond ((memq version '(:newest :next))
                                   (when (> this-version max-version)
                                     (setq max-version this-version)
                                     (send dir-stream :advance-pointer es)
                                     (setq position (send dir-stream :read-pointer))))
                                  ((= this-version version)
                                   (setq found-it t)
                                   (send dir-stream :advance-pointer es)
                                   (setq position (send dir-stream :read-pointer))
                                   (return-from top nil)))))))))))

      (ecase op
        (:delete
         (when found-it
           (send dir-stream :seek position)
           (send dir-stream :tyo-32 0))
         found-it)
        (:find
         (when position
           (let (id version)
             (send dir-stream :seek position)
             (setq id (send dir-stream :tyi-32))
             (send dir-stream :seek (+ position 12.))
             (setq version (send dir-stream :tyi-32))
             (values id version))))
        (:create
         (cond ((not found-it)
                (cond ((null first-empty)
                       (send dir-stream :seek (send dir-stream :file-size)))
                      (t
                       (send dir-stream :seek first-empty)))
                (when (eq version :newest)
                  (setq version max-version))
                (when (eq version :next)
                  (when (> max-version 16000.)
                    (ferror nil "version number too big"))
                  (setq version (+ max-version 1)))
                (send dir-stream :tyo-32 file-id)
                (send dir-stream :tyo-32 name)
                (send dir-stream :tyo-32 type)
                (send dir-stream :tyo-32 version)
                (values file-id version))
               (t
                nil)))))))

(defun print-directory (part cluster-number)
  (with-open-stream (dir-stream (make-cluster-stream part cluster-number))
    (let* ((dirent (make-array 8 :type :art-16b))
           (dirent-8 (make-array 16. :type :art-8b :displaced-to dirent)))
      (do ()
          (())
        (multiple-value-bind (nil eof-p)
            (send dir-stream :string-in nil dirent-8)
          (when eof-p (return nil))
          (format t "~&~8d ~8d ~8d ~8d"
                  (nfh-ref dirent 0)
                  (nfh-ref dirent 1)
                  (nfh-ref dirent 2)
                  (nfh-ref dirent 3)))))))

(defun lookup-internal (part start-dir-number dirs name type version)
  (do ((cd start-dir-number)
       (files dirs (cdr files)))
      ((null files)
       )
    (let ((file-id (primitive-lookup :find nil part cd (car files) 0 0)))
