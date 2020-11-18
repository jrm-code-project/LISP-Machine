;;; -*- Mode: Lisp; Package: SI; Readtable: ZL; Base: 10. -*-
;;;
;;; Region hacking stuff
;;;
;;; -dg 5/11/86
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Region allocation recording utilities
;;;
;;; Region map record is 256 x 2 art-q entries.
;;; n = region number
;;; index[n,0] = area-number for the region
;;; index[n,1] = sorted list of quantum numbers
;;; index[0,0] = sorted list of free quantums
;;; index[0,1] = random
;;; array-leader[0] = free quantum list
;;; array-leader[1] = copy of address space map this all came from
;;; array-leader[2] = fixnum microsecond time when address space map copied
;;; array-leader[3] = universal time when address space map copied

;;; need to dump area info with records

(defun make-region-map-record (&optional
                               (record (make-array '(256 2) :type 'art-q :leader-length 4))
                               (addr-map-copy (make-array (length #'address-space-map) :type 'art-8b)))
  (let ((zero-region-kludge t))
    (without-interrupts
      (copy-array-contents #'address-space-map addr-map-copy)
      (setf (array-leader record 0) nil
            (array-leader record 1) addr-map-copy
            (array-leader record 2) (%microsecond-time)
            (array-leader record 3) (time:get-universal-time)))
    (dotimes (c 2048)
      (let ((region (aref addr-map-copy c)))
        (cond (zero-region-kludge
               (push c (aref record region 1))
               (unless (zerop region)
                 (setq zero-region-kludge nil)))
              ((zerop region)
               (push c (array-leader record 0)))
              (t (push c (aref record region 1))))))
    (setf (array-leader record 0) (sort (array-leader record 0) '<))
    (dotimes (idx 256)
      (when (setf (aref record idx 1) (sort (aref record idx 1) '<))
        (setf (aref record idx 0) (si:%region-area idx))))
    record))

(defun print-region-record-by-region (record)
  (dotimes (c 256)
    (when (aref record c 1)
      (format t "~&~D~5T~A~35T:~A"
              c
              (si:area-name (si:%region-area c))
              (aref record c 1)))))

(defconst *cached-real-area-list* ())

(defun get-real-current-area-list ()
  "This eliminated duplicate area numbers (synonyms)."
  (or *cached-real-area-list*
      (let ((collect))
        (dolist (area (current-area-list)
                      (setq *cached-real-area-list* (reverse collect)))
          (unless (mem #'(lambda (a b) (= (si:eval-special-ok a) (si:eval-special-ok b)))
                       area collect)
            (push area collect))))))

(defun find-area-regions-in-record (area-number record &aux return-list)
  (dotimes (c 256 (reverse return-list))
    (when (and (aref record c 1) (= (aref record c 0) area-number))
      (push c return-list))))

(defun print-region-record-by-area (record &aux temp)
  (dolist (c (get-real-current-area-list))
    (when (setq temp (find-area-regions-in-record
                       (si:eval-special-ok c)
                       record))
      (format t "~&~A:" c)
      (dolist (region temp)
        (when (aref record region 1)
          (format t "~&~10TRegion ~D~20T: ~A~%"
                  region
                  (aref record region 1)))))))

(defvar *region-map-record-list* ())

(defun snapshot-address-space-allocation ()
  (push (make-region-map-record) *region-map-record-list*))

(defconst *region-map-record-file-default* "dj:dg.region-records;")

(defun make-map-record-filename (&key (host si:local-host) (default *region-map-record-file-default*))
  (let ((default (fs:parse-pathname default)))
    (send default :new-pathname
          :name (string-append (send (si:parse-host host) :short-name)
                               "-region-map-record")
          :canonical-type :qfasl
          :version :newest)))

(defun dump-region-map-records (&key (host si:local-host)
                                (file (make-map-record-filename :host host)))
  (compiler:fasd-symbol-value file '*region-map-record-list*))

(defun restore-region-map-records (&key (host si:local-host)
                                   (file (make-map-record-filename :host host)))
  (load file))

(defun analyze-region-record-changes (&optional (list *region-map-record-list*))
  (format t "~&The First record start time: ~\time\" (array-leader (car (last list)) 3))
  (let* ((holes (find-holes-in-quantum-map (array-leader (car (last list)) 1)))
         (quantums (apply '+ (mapcar 'length holes))))
    (format t "~&~5THoles in quantum map: ~D holes; ~d quantums; ~D words."
            (length holes) quantums (* quantums 64 256)))
  (do* ((l (reverse list) (cdr l))
        (pos 2 (add1 pos))
        (record-1 (car l) (car l))
        (record-2 (cadr l) (cadr l)))
       ((null record-2) (format t "~&End of records."))
    (format t "~&~%The ~:R record start time: ~\time\" pos (array-leader record-2 3))
    (if (equal (listarray (array-leader record-1 1))
               (listarray (array-leader record-2 1)))
        (format t "~&~5TNo changes.")
      (format t "~&~5TArea changes:~%")
      (dolist (area (get-real-current-area-list))
        (let ((a1 (find-area-regions-in-record (si:eval-special-ok area) record-1))
              (a2 (find-area-regions-in-record (si:eval-special-ok area) record-2)))
          (unless (equal a1 a2)
            (let ((removed (set-difference a1 a2))
                  (added (set-difference a2 a1)))
              (when added
                (format t "~&~10TArea (~A) had regions added: ~A"
                        area added))
              (when removed
                (format t "~&~10TArea (~A) has regions removed: ~A"
                        area removed))))))
      (format t "~&~5TRegion changes:~%")
      (dotimes (region 256)
        (let ((r1 (aref record-1 region 1))
              (r2 (aref record-2 region 1)))
          (cond ((equal r1 r2))
                ((null r2)
                 (format t "~&~10TRegion ~D freed: ~A"
                         region r1))
                ((null r1)
                 (format t "~&~10TRegion ~D allocated: ~A"
                         region r2))
                (t
                 (format t "~&~10TRegion ~D modified:~%~
                            ~20TPrevious Quantums: ~A~%~20TCurrent Quantums: ~A"
                         region r1 r2)))))
      (let* ((holes (find-holes-in-quantum-map (array-leader record-2 1)))
             (quantums (apply '+ (mapcar 'length holes))))
        (format t "~&~5THoles in quantum map: ~D holes; ~d quantums; ~D words."
                (length holes) quantums (* quantums 64 256))))))

(defun nth-region-record (number &optional (list *region-map-record-list*))
  (car (nthcdr (- (length list) (add1 number)) list)))

(defun find-holes-in-quantum-map (quantum-map)
  (do ((count 31 (add1 count))
       counting
       return-list)
      ((= count 2048) (reverse return-list))
    (cond ((zerop (aref quantum-map count))
           (push count counting))
          (counting
           (push (reverse counting) return-list)
           (setq counting nil)))))

(defun analyze-holes-in-region-records (&key show-holes (list *region-map-record-list*))
  (do* ((l (reverse list) (cdr l))
        (record (car l) (car l))
        (count 1 (add1 count))
        holes
        quantums)
       ((null record) (format t "~&End of data.~%"))
    (setq holes (find-holes-in-quantum-map (array-leader record 1))
          quantums (apply '+ (mapcar 'length holes)))
    (format t "~&The ~:R record has ~D holes; ~D quantums; ~D words.~%"
            count (length holes) quantums (* quantums 64 256))
    (when show-holes
      (dolist (hole holes)
        (format t "~&~10T~A~%" hole)))))

(defun decode-volatility-constraint (constraint)
  (etypecase constraint
    ((member :any) 'last-gc)))
;;; this is more difficult to process-wait for... do it later.
;    ((integer 0 3) `(and last-gc (= last-gc ,constraint)))
;    (cons `((and last-gc
;                (,(case (car constraint)
;                    (:higher '<)
;                    (:lower '>)
;                    (:equal '=))
;                 last-gc
;                 ,(cadr constraint)))))))

(defvar *constraint-function-args* '(start-time gc-generation snapshot-count))

(defun decode-activation-constraint (constraint)
  (check-type constraint (or cons null))
  (if (null constraint)
      #'ignore
    (let ((*standard-output* 'si:null-stream))
      (compile-lambda
        `(lambda ,*constraint-function-args*
           ,(decode-activation-constraint-internal constraint))))))

(defun decode-activation-constraint-internal (constraint)
  (case (car constraint)
    (:funcall `(funcall ,(cadr constraint) ,(cddr constraint)))
    (:eval (cadr constraint))
    (:or `(or . ,(mapcar 'decode-activation-constraint-internal (cdr constraint))))
    (:and `(and . ,(mapcar 'decode-activation-constraint-internal (cdr constraint))))
;    (:elapsed-time `(> (time:get-universal-time) (+ start-time ,(cadr constraint))))
    (:after-gc `(//= gc-generation (symeval-globally 'gc:%gc-generation-number)))
    (:count `(= snapshot-count ,(cadr constraint)))))

(defvar *address-space-watch-process-control* ())
(defvar *address-space-watch-snapshot-notify* ())
(defvar *address-space-watch-process* ())

(defun address-space-watch-process (duration interval &rest args)
  (unwind-protect
      (apply 'address-space-watch duration interval args)
    (setq *address-space-watch-process* nil)))

(defun address-space-watch (duration interval &key
                            (wipe-out-current-list t)
                            (save-after-finish t)
                            save-after-each-snapshot
                            (save-filename (make-map-record-filename :host si:local-host)))
  (when wipe-out-current-list (setq *region-map-record-list* nil))
  (let ((*time-start* (time:get-universal-time))
        (*snapshot-count* 0)
        (*gc-generation-number* gc:%gc-generation-number)
        (finish-test (decode-activation-constraint duration))
        (snapshot-test (decode-activation-constraint interval))
        run-reason)
    (declare (special *time-start* *snapshot-count* *gc-generation-number*))
    (tv:notify nil "Address space watch process started ~\time\" *time-start*)
    (process-wait "1st snapshot wait" #'(lambda () (null gc:*gc-status*)))
    (snapshot-address-space-allocation)
    (tv:notify nil "Address space watch: Made initial snapshot.")
    (do-forever
      (process-wait
        "Wait for event"
        (closure '(*snapshot-count* *time-start* *gc-generation-number*)
                 #'(lambda ()
                     (setq run-reason (cond (*address-space-watch-process-control* :control)
                                            ((funcall finish-test
                                                      *time-start* *gc-generation-number* *snapshot-count*)
                                             :finish)
                                            ((funcall snapshot-test
                                                      *time-start* *gc-generation-number* *snapshot-count*)
                                             :snapshot))))))
      (selectq-every run-reason
        (:control
         (selectq-every *address-space-watch-process-control*
           (nil)
           (:wait (process-wait "Control wait"
                                (closure '(*address-space-watch-process-control*)
                                         #'(lambda ()
                                             (null *address-space-watch-process-control*)))))
           (:arrest
            (send si:current-process :arrest))
           ((:save :finish-and-save)
            (dump-region-map-records :file save-filename))
           ((:finish :finish-and-save)
            (return-from address-space-watch nil)))
         (setq *address-space-watch-process-control* nil))
        (:finish
         (when save-after-finish
           (dump-region-map-records :file save-filename))
         (return-from address-space-watch nil))
        (:snapshot
         (process-wait "Await flip done" #'(lambda () (null gc:*gc-status*)))
         (gc:without-flipping
           (when (si:eval-special-ok snapshot-test)     ;in case of flip when gc-type constraint
             (gc:without-scavenging
               (snapshot-address-space-allocation)
               (incf *snapshot-count*)
               (when *address-space-watch-snapshot-notify*
                 (tv:notify nil "Address space watch: Snapshot taken at ~\time\"
                            (time:get-universal-time)))
               (when save-after-each-snapshot
                 (dump-region-map-records :file save-filename)))
             (setq *gc-generation-number* gc:%gc-generation-number))))))))

(defun start-address-space-watch (duration interval &rest args)
  (setq *address-space-watch-process-control* nil)
  (if (null *address-space-watch-process*)
      (setq *address-space-watch-process*
            (apply 'process-run-function
                   "Address space watch"
                   'address-space-watch-process
                   duration interval args))
    (tv:beep)
    (format t "Address space watch process already exists: ~S"
            *address-space-watch-process*)))

(defun finish-address-space-watch (&optional (save t))
  (when *address-space-watch-process*
    (setq *address-space-watch-process-control*
          (if save
              :finish-and-save
            :finish))
    (process-wait "Wait for finish"
                  #'(lambda () (null *address-space-watch-process*)))))
