;;; -*- Mode:LISP; Package:USER;BASE: 10 -*-


#||

Some unix commands have been set up on angel to support this:

/usr/local/bin/atigerimp <array-file-input-from-lispm> <output-impress-language-file>

/usr/local/bin/impress <impress-language-filename>
This deletes the file after it is printed.

||#


(defvar *atigerimp-command* "//usr//local//bin//atigerimp")
(defvar *impress-command*   "//usr//local//bin//impress")
(defvar *im-spool* "//usr//spool//im1//")


(defun spool-status ()
  (terpri)
  (princ (caar *spools*))
  (terpri)
  (princ (cadar *spools*))
  (terpri)
  (catch-error
    (let ((fn (string-append (send (get si:*default-bit-array-printer* :host) :name)
                             ":" *im-spool*)))
      (dolist (f '("status" "pstatus" "lock"))
        (viewf (string-append fn f)))
      (listf (string-append fn "*")))))

(defun setup-imagen-unix-spool (host &optional &key
                                (directory "//tmp")
                                (spool-to-printer t)
                                (filename nil)
                                (file-count 0)
                                (generate-spool-file-name 'generate-imagen-spool-file-name))
  (setq si:*default-bit-array-printer* (list :imagen-unix-spool
                                             :host (si:parse-host host)
                                             :directory directory
                                             :spool-to-printer spool-to-printer
                                             :filename filename
                                             :file-count file-count
                                             :generate-spool-file-name generate-spool-file-name)))

(defvar *spools* nil)

(defun (:imagen-unix-spool si:print-bit-array) (printer array left top right bottom &rest options)
  options
  (let ((pathname (funcall (get printer :generate-spool-file-name) printer)))
    (with-open-file (stream pathname :direction :output :characters nil
                            ;; might as well specify this since :byte-size 8
                            ;; doesnt work, appears to be 16 anyway.
                            :byte-size 16.)
      (let ((kludge (make-8b-16b-output-kludge stream)))
        (tiger:dump-pixel-array-to-stream kludge array left top right bottom)
        (send kludge :tyo 0)))
    (when (get printer :spool-to-printer)
      (let ((st (make-array 200 :type 'art-string :fill-pointer 0)))
        (push (list (time:print-current-time nil) st) *spools*)
        (process-run-function "handle imagen spooling"
                            #'handle-imagen-unix-array-spooling
                            pathname
                            (make-string-output-stream st))))))


(defun generate-imagen-spool-file-name (printer)
  (incf (get printer :file-count))
  (fs:parse-pathname (format nil "~A:~A//~A-~A.array"
                             (send (get printer :host) :name)
                             (get printer :directory)
                             (or (get printer :filename) (string-downcase si:user-id))
                             (get printer :file-count))))


(defun handle-imagen-unix-array-spooling (pathname &optional (stream standard-output))
  (let ((a-f (send pathname :string-for-host))
        (i-f (send (send pathname :new-type "IMPRESS") :string-for-host))
        (h (send (send pathname :host) :name)))
    (format stream "~&Processing on host ~S ~S into ~S~%" h a-f i-f)
    (simple-unix-eval h (format nil "~A ~a ~a" *atigerimp-command* a-f i-f) stream)
    (simple-unix-eval h (format nil "~A ~a" *impress-command* i-f) stream)
    (simple-unix-eval h (format nil "rm ~a" a-f) stream)))

(defun simple-unix-eval (host command stream)
  (with-open-stream (s (chaos:open-stream host
                                          (format nil "EVAL ~a" command)))
    (format stream "~&% ~A~%" command)
    (do ((c (send s ':tyi) (send s ':tyi)))
        ((null c))
      (send stream ':tyo
            (selectq c
              ((#o12 #o15) #\return)
              (#o11 #\tab)
              (t c))))))


(setup-imagen-unix-spool "angel")

(defun make-8b-16b-output-kludge (16b-stream &aux 8b-stream first)
  ;; I am finding that the file we get behaves as if byte size 16, not 8.
  ;; so we use this kludge.
  (setq 8b-stream #'(lambda (op &optional arg1 &rest args)
                      (si:selectq-with-which-operations op
                        (:tyo
                          (cond ((not first)
                                 (setq first arg1))
                                ('else
                                 (send 16b-stream :tyo (+ first (ash arg1 8)))
                                 (setq first nil))))
                        (:string-out
                          ;; this might look slow but it is ok.
                          (do ((j (or (car args) 0) (1+ j))
                               (c first)
                               (string arg1)
                               (end (or (cadr args) (length arg1))))
                              ((= j end)
                               (if c (setq first c)))
                            (cond ((not c)
                                   (setq c (aref string j)))
                                  ('else
                                   (send 16b-stream :tyo (+ c (ash (aref string j) 8)))
                                   (setq c nil)))))
                        (t
                          (stream-default-handler 8b-stream op arg1 args))))))



#||

(setup-imagen-unix-spool "angel" "//tmp" t)

(defun dbg ()
  (handle-imagen-unix-array-spooling  handle-imagen-unix-array-spooling))


(defun dbgx (&optional (f handle-imagen-unix-array-spooling))
  (simple-unix-eval
    "angel"
    (format nil
            "//usr//local//bin//ipr -Limpress -Pim1 -f screen -r -d ~A"
            (send (send f :new-type "I") :string-for-host))
  standard-output))



||#
