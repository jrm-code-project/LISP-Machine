;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Readtable:ZL; Base:10 -*-

(defun map-over-stream-lines (f from-stream)
  "Function F gets called on STRING START END for each line in the stream.
This is highly bummed, sends :read-input-buffer and :advance-input-buffer
messages, which is better than using :string-line-in because we only do
copying of data when a end-of-line boundary crosses an i-o buffer boundary."
  (declare (values total-lines easy-lines n-buffers n-chars))
  (prog (offset limit start end leftover leftoverp easy-lines hard-lines buf
         n-buffers n-chars)
        (setq easy-lines 0 hard-lines 0 n-buffers 0 n-chars 0)
     read-buffer
        (multiple-value (buf offset limit)
          (send from-stream :read-input-buffer))
        (or buf (return (values (+ easy-lines hard-lines) easy-lines n-buffers n-chars)))
        (incf n-buffers)
        (incf n-chars (- limit offset))
        (setq start offset)
     next-line
        (setq end (%string-search-char #\RETURN buf start limit))
        (cond ((null end)
               (when (or (not leftover)
                         (> (- limit start) (array-length leftover)))
                 (setq leftover (make-array (max 300 (fix (* 1.2 (- limit start))))
                                            :type 'art-string
                                            :fill-pointer 0)))
               (setf (fill-pointer leftover) (- limit start))
               (copy-array-portion buf start limit
                                   leftover 0 (- limit start))
               (send from-stream :advance-input-buffer)
               (setq leftoverp t)
               (go read-buffer))
              ((not leftoverp)
               (funcall f buf start end)
               (setq start (1+ end))
               (incf easy-lines)
               (go next-line))
              ('else
               (let* ((old-fp (fill-pointer leftover))
                      (new-fp (+ old-fp (- end start))))
                 (when (> new-fp (array-length leftover))
                   (let ((new-leftover (make-array (fix (* 1.2 new-fp))
                                            :type 'art-string
                                            :fill-pointer 0)))
                     (copy-array-portion leftover 0 old-fp
                                         new-leftover 0 old-fp)
                     (setq leftover new-leftover)))
                 (setf (fill-pointer leftover)  new-fp)
                 (copy-array-portion buf start end
                                     leftover old-fp new-fp)
                 (funcall f leftover 0 new-fp)
                 (setq leftoverp nil)
                 (setq start (1+ end))
                 (incf hard-lines)
                 (go next-line))))))


(defun type-file (filename &optional (stream standard-output) &aux tm)
  "Type the file to stream using the :line-out operation."
  (terpri)
  (with-open-file (input filename)
    (setq tm (time))
    (multiple-value-bind (nil nil n-buffers n-chars)
        (map-over-stream-lines #'(lambda (string start end)
                                   (send stream :line-out string start end))
                               input)
      (setq tm (quotient (time-difference (time) tm) 60.0))
      (list :time tm :buffers n-buffers :characters n-chars
            :characters-per-buffer (quotient n-chars (float n-buffers))
            :characters-per-second (quotient n-chars tm)))))


(defun file-line-search (filename keys &optional &key
                         (output standard-output)
                         (window 3)
                         (starlines t)
                         &aux
                         (line-number 0)
                         (display-next 0)
                         (found-alist nil)
                         (head))
  (if (atom keys) (setq keys (list keys)))
  (with-open-file (stream filename)
    (when starlines
      (format output "~&**** Searching ~S for keys: ~S~%"
              (setq head (send (send stream :truename) :string-for-editor)) keys))
    (map-over-stream-lines
      #'(lambda (string start end)
          (incf line-number)
          (LET ((FOUND (DO ((L KEYS (CDR L)))
                           ((NULL L) NIL)
                         (AND (string-search (CAR L) string start end)
                              (RETURN (CAR L))))))
            (cond (FOUND
                   (incf (cdr (or (assq found found-alist)
                                  (car (push (cons found 0) found-alist)))))
                   (when starlines
                     (format output "**** Line ~D of ~S contains ~S ****~%"
                             line-number
                             head
                             found)
                     (format output "~D " line-number))
                   (send output :string-out string start end)
                   (terpri output)
                   (setq display-next window))
                  ((> display-next 0)
                   (when starlines
                     (format output "~D " line-number))
                   (send output :string-out string start end)
                   (terpri output)
                   (decf display-next)
                   (when (and starlines (zerop display-next))
                     (format output "*************~%"))))))
      stream))
  (cond ((not starlines))
        ((null found-alist)
         (format output "*** NO KEYS FOUND IN ~S~%" HEAD))
        ('else
         (format output "*** SUMMARY OF KEYS FOUND IN ~S~%" HEAD)
         (dolist (k found-alist)
           (format output "~S found ~D time~p~%" (car k) (cdr k) (cdr k))))))

(defun system-source-search (system keys &optional &key output-filename (window 3)
                             (starlines t))
  (let ((files (delq nil (mapcar #'probe-file (si:system-source-files system))))
        (tm (time))
        (dw (read-meter 'si:%disk-wait-time)))
    (format t "~&~D files to search in ~S~%" (length files) system)
    (cond (output-filename
           (with-open-file (o output-filename :out)
             (do ((j 1 (1+ j))
                  (l files (cdr l)))
                 ((null l))
               (format t "~&~D of ~D~%" j (length files))
               (file-line-search (car l) keys :output o :window window :starlines starlines))
             (setq tm (quotient (time-difference (time) tm) 60.0))
             (setq dw (quotient (- (read-meter 'si:%disk-wait-time) dw) 1.0e6))
             (format o "~%%%%%%%% SEARCH OF ~D FILES TOOK ~A ~D seconds disk wait~%"
                     (length files)
                     (time:print-interval-or-never (fix tm) nil)
                     dw)))
          ('else
           (dolist (f files)
             (file-line-search f keys :window window :starlines starlines))
           (setq tm (quotient (time-difference (time) tm) 60.0))
             (setq dw (quotient (- (read-meter 'si:%disk-wait-time) dw) 1.0e6))
           (format t "~%%%%%%%% SEARCH OF ~S FILES TOOK ~A ~D seconds disk wait~%"
                   (length files)
                   (time:print-interval-or-never (fix tm) nil)
                   dw)))))
