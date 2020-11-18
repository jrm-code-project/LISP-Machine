;;; -*- Mode:LISP; Package:LISP-IO; Readtable:CL; Base:10 -*-
;;;
;;; GRIND.LISP

;;; print-length, print-level, print-circle
;;; think some more about grind-array.
;;; also vectors.

;;;----------------------------------------------------------------------------
;;; GRINDER STREAMS
;;;----------------------------------------------------------------------------
;;;
;;; A grinder stream is a regular character output stream which also keeps
;;; track of two quantities:  HPOS, the number of characters written since the
;;; last newline (the "cursor column"), and WIDTH, a fixed quantity specified
;;; when the grinder stream is created.  The stream operations HPOS and WIDTH
;;; return these two quantities.
;;;----------------------------------------------------------------------------

(defun make-grinder (width stream)
  (let ((hpos 0))
    (let ((grinder
            (make-stream
              :input-stream-pred
                #'(lambda () NIL)
              :output-stream-pred
                #'(lambda () T)
              :output-element-type-proc
                #'(lambda () 'string-char)
              :write-char-proc
                #'(lambda (char)
                    (write-char char stream)
                    (if (char= char #\Newline)
                        (setf hpos 0)
                        (incf hpos)))
              :write-string-proc
                #'(lambda (string)
                    (dotimes (i (length string))
                      (write-char (char string i) stream)
                      (if (char= (char string i) #\Newline)
                          (setf hpos 0)
                          (incf hpos))))
              :unrecognized-output-operation-proc
                #'(lambda (operation &rest args)
                    (apply 'invoke-output-operation
                           stream
                           operation
                           args)))))
      (define-output-operation grinder :hpos
        #'(lambda ()
            hpos))
      (define-output-operation grinder :width
        #'(lambda ()
            width))
      grinder)))


(defun grinder-hpos (grinder)
  (invoke-output-operation grinder :HPOS))

(defun grinder-width (grinder)
  (invoke-output-operation grinder :WIDTH))

(defun width-remaining (grinder)
  (- (grinder-width grinder) (grinder-hpos grinder)))

(defun go-to-column (column grinder)
  (if (< column (grinder-hpos grinder))
      (error "Can't back up to column ~S." column)
      (progn
        (dotimes (foo (- column (grinder-hpos grinder)))
          (write-char #\Space grinder)))))


;;;----------------------------------------------------------------------------
;;; LIST GENERATORS
;;;----------------------------------------------------------------------------

(defun make-generator (list)
  (let ((pointer  list)
        (dot-sent NIL)
        (count    0))
    (flet ((next-item ()
             (cond ((and *print-length* (>= count *print-length*))
                    *three-dot-token*)
                   ((null pointer)  (error "No next item."))
                   ((consp pointer) (car pointer))
                   ((not dot-sent)  *dot-token*)
                   (t               pointer)))
           (advance-pointer ()
             (cond ((and *print-length* (>= count *print-length*))
                    (setq pointer nil))
                   ((null pointer)  (error "No next item."))
                   ((consp pointer) (setq pointer (cdr pointer))
                                    (incf count))
                   ((not dot-sent)  (setq dot-sent t))
                   (t               (setq pointer nil)
                                    (incf count)))))
      #'(lambda (message)
          (case message
            (:PEEK (next-item))
            (:NEXT (prog1 (next-item) (advance-pointer)))
            (:DONE (eq pointer nil)))))))


;;;----------------------------------------------------------------------------
;;; ELBOW ROOM
;;;----------------------------------------------------------------------------
;;;
;;; This is equivalent, but much less efficient:
;;;
;;; (defun will-it-fit? (thing width)
;;;   (let* ((thing-string (write-to-string thing))
;;;          (thing-width  (length thing-string)))
;;;     (if (<= thing-width width)
;;;         thing-string
;;;         NIL)))
;;;----------------------------------------------------------------------------

(defun will-it-fit? (thing width cycles)
  (unless (> width 0)
    (return-from will-it-fit? NIL))
  (let ((trial-stream-name (prog1 (gensym 'trial-stream-) (gensym 'g)))
        (old-cycle-entries NIL)
        (character-count   0)
        (inner-stream      (make-string-output-stream)))
    (maphash #'(lambda (key value) (push (cons key value) old-cycle-entries))
             cycles)
    (if (catch trial-stream-name
          (let ((*print-pretty* NIL)
                (*print-nicely* T))
            (print-object
              thing
              (make-stream
                :input-stream-pred
                  #'(lambda () NIL)
                :output-stream-pred
                  #'(lambda () T)
                :output-element-type-proc
                  #'(lambda () 'string-char)
                :write-char-proc
                  #'(lambda (char)
                      (incf character-count)
                      (if (>= character-count width)
                          (throw trial-stream-name NIL)
                          (write-char char inner-stream)))
                :write-string-proc
                  #'(lambda (string)
                      (setq character-count
                            (+ character-count (length string)))
                      (if (>= character-count width)
                          (throw trial-stream-name NIL)
                          (write-string string inner-stream))))
              cycles)))
        (get-output-stream-string inner-stream)
        (progn
          (clrhash cycles)
          (mapcar #'(lambda (pair)
                      (setf (gethash (car pair) cycles) (cdr pair)))
                  old-cycle-entries)
          NIL))))


;;;----------------------------------------------------------------------------
;;; GRINDING LISTS
;;;----------------------------------------------------------------------------
;;;
;;; GRIND-LIST is called only by the printer, and recursively if grinding a
;;; sublist with formatting information.
;;;----------------------------------------------------------------------------


(defvar *grinder-leeway* 10.
  "If a list will not fit on a line, but the space available is at least this
much, the list will be ground to fit in that space.  Otherwise, the list will
be put on a new line.")

(defun grind-list (list format grinder cycles)
  "Grind LIST, aligned at COLUMN, using FORMAT."
  (if (and *print-level* (zerop *print-level*))
      (print-object *sharp-token* grinder cycles)
      (let ((list-string (will-it-fit? list (width-remaining grinder) cycles)))
        (if list-string
            (write-string list-string grinder)
            (let ((car-column (1+ (grinder-hpos grinder))))
              (write-char #\( grinder)
              (let ((listgen (make-generator list)))
                (multiple-value-bind (n-on-top-line cadr-column)
                    (grind-top-line listgen car-column format grinder cycles)
                  (grind-rest-of-list listgen
                                      n-on-top-line
                                      grinder
                                      car-column
                                      cadr-column
                                      format
                                      cycles))
                (write-char #\) grinder)))))))

(defun grind-top-line (listgen car-column format grinder cycles)
  "Grind the top line of output of LIST.  Return two values:  how many elements
of LIST were printed, and what column the cadr of LIST was printed on (or
should be printed on.)"
  (let (number-printed cadr-column)
    (do ((n 0 (1+ n)))
        ((>= n (indent-info-top-line-forms format))
         (setq number-printed n))
      (let* ((element (funcall listgen :PEEK))
             ;;                ^^
             ;; presumably, there will be something there.  Otherwise the whole
             ;; list would have answered will-it-fit? with t.
             (element-string
               (let ((*print-level* (decrement *print-level*)))
                 (will-it-fit? element (width-remaining grinder) cycles))))
        (cond
          (element-string
           (when (= n 1)
             (setq cadr-column (grinder-hpos grinder)))
           (write-string element-string grinder)
           (write-char #\Space grinder)
           (funcall listgen :NEXT))
          ((or (> (width-remaining grinder) *grinder-leeway*) (= n 0))
           (when (= n 1)
             (setq cadr-column (grinder-hpos grinder)))
           (let ((*print-level* (decrement *print-level*)))
             (if (and (listp element)
                      (nth-element-indentation-format n format))
                 (grind-list element
                             (get-indentation
                               (nth-element-indentation-format n format))
                             grinder
                             cycles)
                 (print-object element grinder cycles)))
           (setq number-printed (1+ n))
           (funcall listgen :NEXT)
           (return))
          (t
           (setq number-printed n)
           (return)))))
    (unless cadr-column
      (setq cadr-column (+ car-column (nth-element-indentation 1 format))))
    (values number-printed cadr-column)))


(defun grind-rest-of-list (listgen n grinder car-column cadr-column format cycles)
  "Grind LIST, beginning with the Nth element, putting one element on each line
aligned according to the FORMAT information, CAR-COLUMN, and CADR-COLUMN."
  (loop
    (let ((element (if (funcall listgen :DONE) (return) (funcall listgen :NEXT))))
      (terpri grinder)
      (let* ((indent-relative-to (nth-element-indent-relative-to n format))
             (column             (+ (nth-element-indentation n format)
                                    (case indent-relative-to
                                      (:CAR  car-column)
                                      (:CADR cadr-column))))
             (element-format     (nth-element-indentation-format n format)))
        (go-to-column column grinder)
        (let ((*print-level* (decrement *print-level*)))
          (if (and (listp element) element-format)
              (grind-list element
                          (get-indentation element-format)
                          grinder
                          cycles)
              (print-object element grinder cycles))))
      (incf n))))


;;;----------------------------------------------------------------------------
;;; GRINDING VECTORS
;;;----------------------------------------------------------------------------

(defun grind-vector (vector grinder cycles)
  (let ((will-it-fit? (will-it-fit? vector (width-remaining grinder) cycles)))
    (cond
      (will-it-fit?
       (write-string will-it-fit? grinder))
      (t
       (write-char #\# grinder)
       (write-char #\( grinder)
       (let ((element-column (grinder-hpos grinder)))
         (dotimes (i (length vector))
           (go-to-column element-column grinder)
           (print-object (aref vector i) grinder cycles)
           (unless (= (1+ i) (length vector)) (terpri grinder))))
       (write-char #\) grinder)))))


;;;----------------------------------------------------------------------------
;;; GRINDING ARRAYS
;;;----------------------------------------------------------------------------

(defun grind-array (array grinder cycles)
  (cond
    ((not *print-array*)
     (write-string (write-to-string array) grinder))
    (t
     (write-char #\# grinder)
     (write-string (write-to-string (array-rank array)) grinder)
     (write-char #\A grinder)
     (grind-array-contents array grinder cycles))))

(defun grind-array-contents (array grinder cycles)
  (grind-list (array-to-list array)
              (get-indentation :array-indentation)
              grinder
              cycles))

;(defun grind-array-contents (array indices column grinder cycles)
;  (let* ((rank-of-subarray       (- (array-rank array) (length indices)))
;        (next-dimension         (length indices))
;        (length-of-subarray     (array-dimension array next-dimension)))
;    (go-to-column column grinder)
;    (write-char #\( grinder)
;    (dotimes (i length-of-subarray)
;      (cond ((and *print-length* (>= i *print-length*))
;            (write-string "..." grinder)
;            (return))
;           ((> rank-of-subarray 1)
;            (grind-array-contents array (append indices (list i)) (1+ column)
;                                  grinder cycles))
;           (t
;            (let ((*print-level* (decrement *print-level*)))
;              (grind (apply #'aref array (append indices (list i)))
;                     (1+ column) grinder cycles))))
;      (unless (= (1+ i) length-of-subarray)
;       (if (= rank-of-subarray 1)
;           (write-char #\Space grinder)
;           (terpri grinder))))
;    (write-char #\) grinder)))
