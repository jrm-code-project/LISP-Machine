;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:8; Lowercase:T; Readtable:ZL -*-

(defun dribble (&optional filename)
  "Starts writing all input and output to FILENAME.  With no arg, stops doing so.
Works by binding *STANDARD-OUTPUT* and *STANDARD-INPUT*.
See also DRIBBLE-ALL."
  (if filename (dribble-start filename) (dribble-end)))

;;; This binds *STANDARD-OUTPUT* and *STANDARD-INPUT* and enters a new read-eval-print
;;; loop.  SETQ'ing them would be global for all processes and would leave you
;;; totally shafted if the file connection broke.

(defun dribble-start (filename &optional editor-p)
  "Copy input and output to a file, or an editor buffer if second arg is T"
  (format t "~&Entering dribble read-eval-print loop.  Do ~S to exit." '(dribble))
  (let* ((*standard-input* (make-dribble-stream *terminal-io*
                                                (if (not editor-p)
                                                    (open filename :direction :output
                                                                   :characters t)
                                                  (zwei:make-file-buffer-stream filename))))
         (*standard-output* *standard-input*))
    (unwind-protect
        (catch 'dribble-end
          (lisp-top-level1 *terminal-io*))
      (send *standard-output* :dribble-end))))

(defun dribble-all (&optional filename &optional editor-p)
  "Copy input and output to a file, or an editor buffer if second arg is T.
With no argument, exits a previously entered DRIBBLE-ALL and closes the file.
This differs from DRIBBLE-START in that it binds *TERMINAL-IO*
instead of *STANDARD-OUTPUT*, so queries, break loops, etc. are included."
  (if (null filename)
      (dribble-end)
    (format t "~&Entering dribble read-eval-print loop.  Do ~S to exit." '(dribble))
    (let ((dribble (make-dribble-stream *terminal-io*
                                        (if (not editor-p)
                                            (open filename :direction :output
                                                           :characters t)
                                          (zwei:make-file-buffer-stream filename)))))
      (unwind-protect
          (catch 'dribble-end
            (lisp-top-level1 dribble))
        ;; Do this with old definition of *terminal-io*, in case it gets an error.
        (send dribble :dribble-end)))))

(defun dribble-end ()
  "Exit from the recursive read-eval-print loop entered by DRIBBLE-START or DRIBBLE-ALL."
  (condition-case ()
      (throw 'dribble-end nil)
    (eh:throw-tag-not-seen
     (format t "~&You do not appear to be in a dribble loop."))))

(defun make-dribble-stream (*tv-stream* *file-stream*)
  (declare (special *tv-stream* *file-stream*))
  (let ((*rubout-handler-buffer* (make-string 100. :fill-pointer 0))
        (dribble-stream))
    (declare (special *rubout-handler-buffer* dribble-stream))
    (setq dribble-stream
          (closure '(*tv-stream* *file-stream* *rubout-handler-buffer* dribble-stream)
                   'dribble-stream-io))))

(defun dribble-stream-io (op &rest args
                          &aux (old-tio *terminal-io*)
                               (*terminal-io*
                                 ;; Don't leave *terminal-io* as the dribble stream
                                 ;; so that errors inside here don't bomb out.
                                 (if (eq *terminal-io* dribble-stream)
                                     *tv-stream*
                                   *terminal-io*)))
  (declare (special *tv-stream* *file-stream* dribble-stream *rubout-handler-buffer*))
  (case op
    ((:tyo :string-out :line-out :fresh-line)
     (lexpr-send *tv-stream* op args)
     (lexpr-send *file-stream* op args))
    ((:any-tyi :tyi :tyi-no-hang :any-tyi-no-hang :read-char :read-char-no-hang)
     (prog ()
           (or rubout-handler (send-if-handles *file-stream* :force-output))
           (catch (if rubout-handler 'rubout-handler 'dummy-tag)
             (let ((ch (send *tv-stream* op)))
               (and ch rubout-handler
                    (vector-push-extend ;; If it's an activation blip, we want the character.
                                       (if (consp ch) (cadr ch) ch)
                                       *rubout-handler-buffer*))
               (return ch)))
           ;;get here if someone threw to rubout-handler
           ;;reset our buffer and continue the throw
           (setf (fill-pointer *rubout-handler-buffer*) 0)
           (throw 'rubout-handler nil)))
    ((:untyi :unread-char)
     (send *tv-stream* op (car args))
     (and rubout-handler
          (plusp (length *rubout-handler-buffer*))
          (decf (fill-pointer *rubout-handler-buffer*))))
    (:rubout-handler
     (setf (fill-pointer *rubout-handler-buffer*) 0)    ;reset the buffer
     (prog (vals)
       (cond ((and (operation-handled-p *file-stream* :safe-to-use-p)
                   (not (send *file-stream* :safe-to-use-p)))
              (format *tv-stream* "~&Dribble stream cannot accept output!")
              (dribble-end)))
       (setq vals (multiple-value-list
                    ;; Bind *terminal-io* back to the dribble stream if that's what it was
                    ;; in case the code run inside the rubout handler
                    ;; uses *terminal-io*.
                    (let ((*terminal-io* old-tio))
                      (lexpr-send *tv-stream* op args))))
       ;; If the stream is having troubles, don't echo to it.
       (cond ((and (operation-handled-p *file-stream* :safe-to-use-p)
                   (not (send *file-stream* :safe-to-use-p)))
              (format *tv-stream* "~&Dribble stream cannot accept output!")
              (dribble-end)))
       (send *file-stream* :string-out *rubout-handler-buffer*)
       (send-if-handles *file-stream* :force-output)
       (return-list vals)))
    (:dribble-end
     (format *tv-stream* "~&Closing dribble file.")
     (close *file-stream*)
     (send-if-handles *file-stream* :truename))
    (:notice
     (if (and (operation-handled-p *file-stream* :safe-to-use-p)
              (not (send *file-stream* :safe-to-use-p)))
         'tv:cold-load-stream
       (lexpr-send-if-handles *tv-stream* :notice args)))
    (:increment-cursorpos
     (cond ((eq (caddr args) ':character)
            (dotimes (y-increment (cadr args))
              (send *file-stream* :tyo #/return))
            (dotimes (x-increment (car args))
              (send *file-stream* :tyo #/sp))))
     (lexpr-send *tv-stream* op args))
    ((:finish :force-output)
     (lexpr-send-if-handles *file-stream* op args)
     (lexpr-send *tv-stream* op args))
    (:ITEM
     (LEXPR-FUNCALL #'FORMAT *FILE-STREAM* (CDDR ARGS))
     (LEXPR-SEND *TV-STREAM* OP ARGS))
    (otherwise
     (lexpr-send *tv-stream* op args))))
