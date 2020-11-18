;-*- Mode:LISP; Package:ZWEI; Readtable:ZL; Base:10; Lowercase:T -*-

;;; this file provides the interface to the rubout handler.

;>> editing-command not implemented

;; These two are temporary until the changes in sys:window;stream  are installed.

;(defmethod (tv:stream-mixin :displayer) ()
;  (if (variable-boundp tv::displayer) tv::displayer nil))
;(defmethod (tv:stream-mixin :set-displayer) (d)
;  (setq tv::displayer d))
;(defmethod (tv:stream-mixin :case :set :displayer) (d)
;  (setq tv::displayer d))


; tv:selected-window rather than *terminal-io* is the right thing in these
; turds, as consider when debuggin in the cold-load stream.
(defun erh-on (&optional globallyp)
  "GLOBALLYP non-NIL means make the erh the default rubout handler everywhere
-- NIL means just change it for TV:SELECTED-WINDOW"
;  (setq *trace-output* (setq *error-output* (setq *debug-io* si:cold-load-stream)))
  (if (not globallyp)
      (send tv:selected-window :set-stream-rubout-handler 'editor-rubout-handler)
    (setq tv::stream-mixin-default-rubout-handler 'editor-rubout-handler)
    (if (send tv:selected-window :stream-rubout-handler)
        ;; make it use the default one
        (send tv:selected-window :set-stream-rubout-handler nil)))
  'editor-rubout-handler)

(defun erh-off (&optional (window (or tv:selected-window *terminal-io*)))
  (setq tv::stream-mixin-default-rubout-handler 'tv::alternate-rubout-handler)
  (when window (send window :send-if-handles :set-stream-rubout-handler nil))
  'tv::alternate-rubout-handler)

(tv:add-escape-key #/hyper-help #'(lambda (ignore) (erh-off)) "Attempt to despazz the world")


;;; The :TYI operation on windows calls the function EDITOR-RUBOUT-HANDLER
;;; when this rubout handler is installed.
;;; It finds the rubout handler buffer in the instance variable
;;; TV::RUBOUT-HANDLER-BUFFER (of TV:STREAM-MIXIN)
;;; and binds ZWEI:*RUBOUT-HANDLER-BUFFER*, a special variable,
;;; so that the editor can get at the stuff.
;;; The value of either variable is a string which we update.

;;; The variable RUBOUT-HANDLER-OPTIONS will also be bound
;;; (to the alternating list of options and values supplied to the :RUBOUT-HANDLER operation).

;;; Each window has a single displayer of flavor EDITOR-RUBOUT-DISPLAYER
;;; used to edit for rubout handling on that window.
(defflavor editor-rubout-displayer () (external-stream-displayer)
  (:default-init-plist
    :editor-closure-variables editor-rubout-handler-editor-closure-variables
    :comtab *rubout-handler-comtab*))

(defconst editor-rubout-handler-editor-closure-variables
          `((*input-history* nil) . ,top-level-editor-closure-variables))

(defvar *input-history* :unbound
  "A history of previous input strings for Control-Meta-Y.
It is a closure var in editor-closures of displayers used for rubout handling.")

(defun window-editor-rubout-handler (window)
  "Return an EDITOR-RUBOUT-DISPLAYER displaying on WINDOW.
Creates one if none exists; from then on, the same one is returned each time."
  (or (send window :displayer)
      (let ((displayer (make-instance 'editor-rubout-displayer :window window)))
; done :after :init
;       (send displayer :set-window window)
        (send window :set-displayer displayer)
        displayer)))


(defmethod (editor-rubout-displayer :reprompt) (&optional char)
  (multiple-value-setq (tv::prompt-starting-x tv::prompt-starting-y)
    (send window :read-cursorpos))
  (let ((prompt (or (assq :reprompt rubout-handler-options)
                    (assq :prompt rubout-handler-options))))
    (and prompt
         (neq char :null)
         (tv::rubout-handler-prompt (cadr prompt) window char)))
  (multiple-value-setq (tv::rubout-handler-starting-x tv::rubout-handler-starting-y)
    (send window :read-cursorpos))
  (setq first-line-left tv::rubout-handler-starting-x
        starting-vpos tv::rubout-handler-starting-y))

;;;; Interface with TV::RUBOUT-HANDLER-BUFFER ivar of TV:STREAM-MIXIN

(defvar *rubout-handler-buffer* :unbound
  "The string that the window system expects RH input in.
Bound by ZWEI::EDITOR-RUBOUT-HANDLER.")

(defvar *rubout-handler-activation-character* :unbound
  "Used by ZWEI::EDITOR-RUBOUT-HANDLER.")

;;; First, the names of the slots in it.
;;; These are defined in io;stream, but the structure is in TV.
;;;   It's too tedious to constantly prefix the tv:, so we define these in zwei.

;;; The number of characters in it.
(defsubst rhb-fill-pointer (&optional (buffer *rubout-handler-buffer*))
  (tv::rhb-fill-pointer buffer))
;;; The index at which the caller is reading.
;;; We must set it to 0 to cause him to rescan.
(defsubst rhb-scan-pointer (&optional (buffer *rubout-handler-buffer*))
  (tv::rhb-scan-pointer buffer))
;;; This corresponds to our value of POINT.
(defsubst rhb-typein-pointer (&optional (buffer *rubout-handler-buffer*))
  (tv::rhb-typein-pointer buffer))
;;; T if this doesn't go onto the input ring
(defsubst rhb-dont-save-flag (&optional (buffer *rubout-handler-buffer*))
  (tv::rhb-dont-save-flag buffer))
;;; Not stored into by erh --- we use *input-history* which is an interval history instead
(defsubst rhb-input-history (&optional (buffer *rubout-handler-buffer*))
  (tv::rhb-input-history buffer))
;;; T is this is the first time the rh has been entered - meams we have to initialize
(defsubst rhb-status (&optional (buffer *rubout-handler-buffer*))
  (tv::rhb-status buffer))

;;; The elements of *RUBOUT-HANDLER-BUFFER* are the characters.  It is an ART-Q
;;;  with array-leader slots as above

;;; We do not actually edit the buffer array, of course.  We edit an interval.
;;; When returning to the rubout handler's caller, we must update *RUBOUT-HANDLER-BUFFER*
;;; for the changes that got made in the interval.

;;; We mark the buffer unmodified here.  That way, the buffer is modified only if
;;; it does not match what is in the rh buffer.
(defun update-rh-buffer (buffer interval point
                         &optional (update-start-bp (interval-first-bp interval)))
  "Updates the contents of BUFFER from the INTERVAL.
The RH-TYPEIN-POINTER is set according to the bp POINT.
Assumes that text in INTERVAL up to UPDATE-START-BP
 is already present and correct in BUFFER."
  (send *interval* :not-modified)
  (do ((line (bp-line (interval-first-bp interval)) (line-next line))
       (updating-yet nil)
       line-start-index
       (buffer-index 0)
       (stop-line (bp-line (interval-last-bp interval))))
      (())
    ;; If POINT falls in this line, set BUFFER's typein-pointer.
    (when (eq line (bp-line point))
      (setf (rhb-typein-pointer buffer)
            (+ buffer-index (bp-index point))))
    ;; Have we arrived yet at the line to start copying at?
    (if (eq line (bp-line update-start-bp))
        (setq updating-yet t
              line-start-index (bp-index update-start-bp))
      (setq line-start-index 0))
    ;; If copying started, copy this line.
    (when updating-yet
      (unless ( (array-length buffer) (+ buffer-index (length line)))
        (adjust-array-size buffer (* 2 (array-length buffer))))
      (copy-array-portion line line-start-index (length line)
                          buffer (+ buffer-index line-start-index)
                          (+ buffer-index (length line)))
      (setf (aref buffer (+ buffer-index (length line))) #/newline))
    (incf buffer-index (1+ (length line)))
    ;; Exit after processing last line.
    (when (eq line stop-line)
      (setf (rhb-fill-pointer buffer)
            (1- buffer-index))  ;-1 since final #\newline doesn't belong.
      (return nil))))

(defun old-buffer-contents-match-p (buffer interval)
  "Is BUFFER (an array) an initial segment of INTERVAL's contents?
Value is non-NIL if so; in that case, it is actually
a BP to the first character in INTERVAL not in BUFFER."
  (do ((line (bp-line (interval-first-bp interval))
             (line-next line))
       (buffer-index 0)
       (stop-line (bp-line (interval-last-bp interval))))
      (())
    (let ((compare-amount (min (length line)
                               (- (length buffer) buffer-index)))
;character lossage
          kludge)
      (dotimes (i compare-amount)
        (if (fixnump (setq kludge (cli:aref buffer (+ buffer-index i))))
            (setq kludge (int-char kludge)))
        (unless (eq kludge (char line i))
          ;; Mismatch within this line.
          (return nil)))
      (incf buffer-index (length line))
      ;; End of BUFFER reached => all of BUFFER matches.
      (when ( buffer-index (length buffer))
        (return (create-bp line compare-amount))))
    ;; End of INTERVAL means mismatch, since not end of BUFFER.
    (when (eq line stop-line)
      (return nil))
    ;; Mismatch with Return at end of line?
    (unless (char= (char buffer buffer-index) #/newline)
      (return nil))
    (incf buffer-index)
    ;; BUFFER ends with that Return => all matches.
    (when ( buffer-index (length buffer))
      (return (create-bp (line-next line) 0)))))

;;; The :SAVE-RUBOUT-HANDLER-BUFFER window op calls this function
;;; whose purpose is to make sure TV::RUBOUT-HANDLER-BUFFER is up to date to be saved.
(defun editor-rubout-handler-save-buffer (window)       ;window is just self
  (declare (:self-flavor tv:stream-mixin))
  (let ((displayer (window-editor-rubout-handler window)))
    (update-rh-buffer tv::rubout-handler-buffer
                      (window-interval displayer)
                      (window-point displayer))
    (setf (rhb-scan-pointer tv::rubout-handler-buffer)
          (length tv::rubout-handler-buffer))))

;; This property says what function to call to handle
;; a :SAVE-RUBOUT-HANDLER-BUFFER op on the window.
(defprop editor-rubout-handler
         editor-rubout-handler-save-buffer
         tv::save-rubout-handler-buffer)


;;; This function gets called whenever a :TYI message is sent to the stream and we are
;;; inside the rubout handler.  If a normal character is typed, it is echoed, put in the
;;; buffer, and returned.  If a rubout or any other editing character is typed, any number
;;; of editing commands are processed by modifying the buffer, then, when the first
;;; non-editing character is typed, a throw is done back to the top level of the read
;;; function and the buffered input is re-scanned.  The character must be typed at the end
;;; of the line in order for the throw to take place.

(defun editor-rubout-handler ()
  (declare (:self-flavor tv:stream-mixin))
  (let* ((*rubout-handler-buffer* tv::rubout-handler-buffer)
         (*rubout-handler-activation-character* nil)
         ;; Prevent calls to this function when the editor reads input!
         (rubout-handler nil)
         (displayer (window-editor-rubout-handler self)))
    ;; If this is the first entry to us within a given :RUBOUT-HANDLER operation,
    ;; initialize editor buffer from window system buffer.
    (if (memq (rhb-status) '(:initial-entry :restored))
        (editor-rubout-handler-initial-entry (rhb-status) displayer)
      (setf (rhb-status) nil))
    ;; Kludge #4.  After resuming a Break, the stream's cursorpos is wrong.
    ;; In fact, the cursor is at the end of the string in that case.
    (must-redisplay displayer dis-bps)
    ;; In case we had to return to the caller with a EDITING-COMMAND char
    ;; while editing, make things consistent again by causing a rescan now.
    (cond ((and (eq (rhb-status) ':rubout)
                (= (rhb-scan-pointer) most-positive-fixnum))
           (rh-rescan t))
          ;; Read characters.
          ;; A command hook may throw to RETURN-CHARACTER with a character
          ;; to be returned, if no old characters were changed.
          ;; Otherwise it will throw to RUBOUT-HANDLER,
          ;; whose CATCH is in the :RUBOUT-HANDLER method of our caller.
          ((prog1 *rubout-handler-activation-character*
                  (setq *rubout-handler-activation-character* nil)))
          (t
           (catch 'return-character
             (catch 'abort-out-of-rubout-handler
               (send displayer :edit))          ;Never returns.
             (signal eh:abort-object))))))

(defun editor-rubout-handler-initial-entry (initial-status displayer)
  (declare (:self-flavor tv:stream-mixin))
  (let* ((initial-input (cdr (assq :initial-input rubout-handler-options)))
         (length (length initial-input))
         (initial-input-pointer
           (if (eq initial-status ':restored)
               (rhb-typein-pointer)
               (max (min (or (cdr (assq ':initial-input-pointer rubout-handler-options))
                             length)
                         length)
                    0)))
         (prompt-option (assq ':prompt rubout-handler-options))
         (window self))
    (funcall (window-editor-closure displayer)
             #'(lambda ()
                 (unless *input-history*
                   (setq *input-history*
                         (zwei:make-history (format nil "~A input history" window)
                                            :element-string-function 'identity))
                   ;>>; ?? Is this really useful??
                   ;;Recover anything that was in the rh buffer's input history previously
                   (let ((history (rhb-input-history)))
                     (when (typep history 'history)
                       (dolist (x (history-list history))
                         (setq x (typecase x
                                   (node x)
                                   (array
                                    (let ((tem (make-string (length x))))
                                      (copy-array-portion x 0 (length x) tem 0 (length x))
                                      tem))))
                         (when x (push-on-history x *input-history*))))))
                 ;;No point in doing this since the histories used by the rh in window;rh
                 ;; doesn't grok nodes (and uses a different yank method in any case)
                 ;; The history from this rh is thus lost if luser switches to a different
                 ;; flavour of rh.
                 ;(setf (rhb-input-history) *input-history*)
                 (setf (rhb-status) nil)
                 (when (eq initial-status ':initial-entry)
                   (unless (bp-= (interval-first-bp (window-interval displayer))
                                 (interval-last-bp (window-interval displayer)))
                     (let ((typein-pointer (rhb-typein-pointer)))
                       (when (and (not (rhb-dont-save-flag))
                                  typein-pointer
                                  ;; only add the contents if different from last time
                                  ;; and if it is at least 2 characters long.
                                  (> typein-pointer 1)
                                  (mismatch *rubout-handler-buffer*
                                            (history-latest-element (rhb-input-history))
                                            :end1 typein-pointer))
                         (push-on-history (let ((tem (make-string typein-pointer)))
                                            (copy-array-portion
                                              *rubout-handler-buffer* 0 typein-pointer
                                              tem 0 typein-pointer)
                                            tem)
                                          *input-history*))))
                   (when ( length (array-length *rubout-handler-buffer*))
                     (adjust-array-size *rubout-handler-buffer* (+ length length)))
                   (when initial-input
                     (copy-array-portion initial-input 0 length
                                         *rubout-handler-buffer* 0 length)
                     (setf (rhb-status) ':rubout)))
                 (discard-undo-information (window-interval displayer))
                 (send displayer :initialize-contents *rubout-handler-buffer*
                       initial-input-pointer
                       tv::rubout-handler-starting-y
                       tv::rubout-handler-starting-x)
                 (setf (rhb-dont-save-flag)
                       (or (cadr (assq :dont-save rubout-handler-options))
                           (cadr (assq :no-input-save rubout-handler-options))))
                 (when prompt-option
                   ;; this kludgery is to make :reprompt go away, since the :reprompt
                   ;;  operation looks first for :reprompt and then for :prompt
                   (with-stack-list (foo :reprompt nil)
                     (with-stack-list* (rubout-handler-options foo rubout-handler-options)
                       (send window :reprompt))))
                 (must-redisplay displayer dis-text)))))


;;;; Command hooks to make editor interface with its caller the rubout handler.

(defmethod (editor-rubout-displayer :process-command-char) (ch)
  ;; make damn sure our hooks are run first!
  (with-stack-list* (*command-hook* 'rh-pre-command-hook *command-hook*)
    (with-stack-list* (*post-command-hook* 'rh-post-command-hook *post-command-hook*)
      (process-command-char ch))))

;;; Handle the TV::REDISPLAY-RUBOUT-HANDLER blip that indicates a notification
;;; or some such thing was output on the window.
(defmethod (editor-rubout-displayer :process-special-command) (subop &rest args)
  (case subop
    (tv::redisplay-rubout-handler
     (dotimes (p n-plines)
       (setf (pline-tick self p) -1))
     (send window :set-cursorpos left (second args))
     (send self :reprompt)
     (must-redisplay self dis-text))
    (t
     nil)))

;; This function is called before each editing command.

;massive character lossage throughout this entire function

(defun rh-pre-command-hook (char)
  (when t ;;**** have to have some test to check that we're in the right interval ****
    (let ((command-handler (cdr (assq ':command rubout-handler-options)))
          (do-not-echo (cdr (assq ':do-not-echo rubout-handler-options)))
          (activation-handler (cdr (assq ':activation rubout-handler-options)))
          (editing-command (cdr (assq ':editing-command rubout-handler-options)))
          (pass-through (cdr (assq ':pass-through rubout-handler-options))))
      (cond ((and (or (memq char pass-through)
                      (memq (char-int char) pass-through))
                  (zerop (char-bits char)))
             ;; Make redisplay finish now, since we will avoid doing so afterward.
             (redisplay *window* :point nil nil t)
             (insert-moving (point) char)
             (rh-continue-or-rescan t))
            ((and command-handler
                  (apply (car command-handler) char (cdr command-handler)))
             (move-bp (point) (interval-last-bp *interval*))
             (update-rh-buffer *rubout-handler-buffer* *interval* (point))
             (setf (rhb-scan-pointer) 0)
             (throw 'tv::return-from-rubout-handler
                     (values
                       `(:command ,(char-int char) ,(or *numeric-arg* 1))
                       :command)))
            ((or (and activation-handler
                      (apply (car activation-handler)
                             char (cdr activation-handler)))
                 (memq char do-not-echo)
                 (memq (char-int char) do-not-echo))
             (let ((tem (if (or (memq char do-not-echo) (memq (char-int char) do-not-echo))
                            (char-int char)
                          `(:activation ,(char-int char) ,(or *numeric-arg* 1)))))
               (move-bp (point) (interval-last-bp *interval*))
               (cond ((eq (rhb-status) ':rubout)
; (format *error-output* "~&>> Throwing to RUBOUT-HANDLER")
                      ;; Why isn't this done in the :RUBOUT-HANDLER method loop?
                      (setq *rubout-handler-activation-character* tem)
                      (rh-rescan t))
                     (t
; (format *error-output* "~&>> Throwing to RETURN-CHARACTER")
                      (update-rh-buffer *rubout-handler-buffer* *interval* (point))
                      (throw 'return-character tem)))))
;>> this is is utterly bogus
            ((or (memq char editing-command)
                 (memq (char-int char) editing-command)
                 (si:assq-careful char editing-command)
                 (si:assq-careful (char-int char) editing-command))
             (if (eq (rhb-status) ':rubout)
                 (setf (rhb-scan-pointer) most-positive-fixnum)))))))

;; Command hook. If at the end of the buffer after the command,
;;  send through the buffered input.
(defun rh-post-command-hook (char)
  (declare (ignore char))
  (cond ;; full-rubout
        ((and (cdr (assq :full-rubout rubout-handler-options))
              (bp-= (interval-first-bp *interval*)
                    (interval-last-bp *interval*)))
         (rh-rescan nil))
        ;; inserting at the end of the buffer
        ((and (eq *current-command-type* 'self-insert)
              (bp-= (point) (interval-last-bp *interval*)))
;>> changed this to inhibit-redisplay to make input faster
         (rh-continue-or-rescan t))
        ;; else just do bookkeeping
        (t
         (if (send *interval* :modified-p) (setf (rhb-status) ':rubout)))))

;;Call here if we have just inserted a character that should be given now
;;to the parser function.
;;If (rhb-status) is nil, what we have already given to the parser function
;;is still valid, so just give it the additional character.
;;Otherwise, (rhb-status) is :rubout, meaning something previously given
;;was edited.  In that case, the parser function must be restarted at the beginning.
;;>> inhibit-redisplay is always T now...
;;>> (else redisplay when typing quickly at end end of buffer is far too slow)
(defun rh-continue-or-rescan (inhibit-redisplay &aux old-match-p)
  (unless inhibit-redisplay
    (redisplay *window* :point nil nil t))      ;Force redisplay to completion.
  (if (not (rhb-status))
      ;; Previously given characters have not been changed.
      (progn
        (setq old-match-p (old-buffer-contents-match-p *rubout-handler-buffer* *interval*))
        (update-rh-buffer *rubout-handler-buffer* *interval* (point)
                          (or old-match-p (interval-first-bp *interval*)))
        (throw 'return-character
               (prog1 (aref *rubout-handler-buffer* (rhb-scan-pointer))
                      (incf (rhb-scan-pointer)))))
    ;; else rescan
    (rh-rescan t)))

;; Make the parser function scan the buffered input from the beginning.
;; This is necessary if any editing has happened other than insertion at end of buffer.
;; Also, if the user specified the :full-rubout option, he wants this to happen any time
;; the buffer becomes empty.

(defun rh-rescan (inhibit-redisplay)
  (unless inhibit-redisplay                             ;Force redisplay to completion.
    (redisplay *window* :point nil nil t))
  (update-rh-buffer *rubout-handler-buffer* *interval* (point))
  (setf (rhb-scan-pointer) 0)
  (throw 'rubout-handler t))


;;;; Special editing commands for rubout handling.

(defcom com-kill-all "Kill entire contents of buffer." ()
  (kill-interval (interval-first-bp *interval*) (interval-last-bp *interval*) t t)
  dis-text)

(defcom com-rh-break "Break key while in rubout handling editor." ()
  (let ((*terminal-io* (send *window* :window)))
    (com-break)))

(defcom com-rh-meta-break "Enter the debugger." ()
  (let ((*terminal-io* (send *window* :window)))
    (tv::kbd-intercept-error-break *last-command-char*)
    dis-none))

(defcom com-rh-abort "Abort running program.
This is on the Abort key in the rubout handler editor." ()
  (throw 'abort-out-of-rubout-handler t))

(defcom com-abort-all "Reset this process; abort all the way out." ()
  (tv::kbd-intercept-abort-all *last-command-char*))

(defcom com-rh-yank-input "Reinsert the text of a previous input.
The last entry on the input ring is reinserted.
A numeric arg specifies an input ring entry (1 is most recent)." ()
  (history-yank *input-history*)
  dis-text)

(defcom com-rh-quick-arglist "Print argument list of this function.
Uses the function which POINT is inside a call to." ()
  (quick-arglist *standard-output*)
  dis-none)

(defcom com-rh-quick-documentation "Print long documentation for this function.
Uses the function which POINT is inside a call to." ()
  (let ((name (relevant-function-name (point))))
    (let ((doc (documentation name 'function)))
      (cond ((null doc)
             (format t "~&Function ~S is not documented" name))
            (t (if (fdefinedp name)
                   (progn (send *standard-output* :fresh-line)
                          (print-arglist name *standard-output*))
                 (format t "~S:" name))
               (format t "~%~A" doc)))))
    dis-none)

(defcom com-rh-display-kill-history document-rh-history ()
  (rh-display-history *kill-history* nil))
(defcom com-rh-display-rest-of-kill-history document-rh-history ()
  (rh-display-history *kill-history* t))
(defcom com-rh-display-input-history document-rh-history ()
  (rh-display-history *input-history* nil))
(defcom com-rh-display-rest-of-input-history document-rh-history ()
  (rh-display-history *input-history* t))

(defun rh-display-history (history restp &aux start end)
  (when (eq *last-command-type* history)
    (setq *current-command-type* history))
  (if restp
      (setq start (if *numeric-arg-p* *numeric-arg* *history-menu-length*)
            end (history-length history))
      (setq start 0
            end (if *numeric-arg-p* *numeric-arg* *history-menu-length*)))
  (list-history-contents history *standard-output* start end)
  dis-none)

(defun document-rh-history (com ignore op &aux k r)
  (if (eq op ':name)
      (get com 'command-name)
    (case com
      (com-rh-display-input-history)
      (com-rh-display-rest-of-input-history (setq r t))
      (com-rh-display-kill-history (setq k t))
      (com-rh-display-rest-of-kill-history (setq k t r t)))
    (format t "Display summary of the~:[~; rest of the~] ~
                ~:[rubout handler input history for this window~;kill history.~]~%" r k)
    (when (eq op ':full)
      (unless r
        (format t "Length of history displayed is controlled by ~S (currently ~D.)~%"
                'zwei:*history-menu-length* zwei:*history-menu-length*)
        (format t "~@[~&Type ~A to see the rest of the history.~]"
                (key-for-command (if k 'com-rh-display-rest-of-kill-history
                                   'com-rh-display-rest-of-input-history)
                                 *comtab* nil nil
                                 (if k #/c-m-status #/m-status)))))))

;;; Comtabs are like standard-comtab but missing
;;; all things that use the minibuffer (for now...)
;;; or hack with fonts or with windows or files or buffers
;;; or do evaluation.

;>> is sparseness the right thing??
(defvar *rubout-handler-comtab* (create-sparse-comtab '*rubout-handler-comtab*))
(defvar *rubout-handler-control-x-comtab*
        (create-sparse-comtab '*rubout-handler-control-x-comtab*))

(defun init-rubout-handler-comtab ()
  (set-comtab *rubout-handler-comtab*
              '(#/c-sh-d com-rh-quick-documentation
                #/c-sh-a com-rh-quick-arglist
                #/status com-rh-display-input-history
                #/c-status com-rh-display-kill-history
                #/m-status com-rh-display-rest-of-input-history
                #/c-m-status com-rh-display-rest-of-kill-history
                #/break com-rh-break
                #/meta-break com-rh-meta-break
                #/abort com-rh-abort
                #/m-abort com-abort-all
                #/clear-input com-kill-all
                #/clear-screen com-recenter-window

                #/c-l com-recenter-window
                #/c-m-y com-rh-yank-input
                #/c-c com-rh-yank-input

                #/c-v :undefined
                #/m-v :undefined
                #/c-m-v :undefined
                #/m-r :undefined
                #/c-m-! :undefined
                #/m- :undefined
                #/c- :undefined
                #/c-h-c :undefined
                #/c-h-e :undefined
                #/m-h-e :undefined
                #/c-m-h-e :undefined
                #/c-m-? :undefined
                #/help :undefined
                #/c-help :undefined
                #/m-x :undefined
                #/c-m-x :undefined
                #/c-m-r :undefined
                #/c-z :undefined
                #/c-j :undefined
                #/m-j :undefined
                #/c-m-j :undefined
                #/m-# :undefined
                #/m-_ :undefined
                #/h-m-p :undefined
                #/mouse-1-1 :undefined
                #/mouse-1-2 :undefined
                #/mouse-2-1 :undefined
                #/mouse-2-2 :undefined
                #/mouse-3-1 :undefined
                ))
  (set-comtab-indirection *rubout-handler-comtab* *standard-comtab*)
  (set-comtab *rubout-handler-control-x-comtab*
              '(#/c-d :undefined
                #/c-q :undefined
                #/c-c :undefined
                #/c-j :undefined
                #/( :undefined
                #/) :undefined
                #/e :undefined
                #/q :undefined
                #/ :undefined
                #/# :undefined
                #/_ :undefined
                ))
  (set-comtab-indirection *rubout-handler-control-x-comtab* *standard-control-x-comtab*)
  (set-comtab-control-indirection *rubout-handler-control-x-comtab*)
  (set-comtab *rubout-handler-comtab*
              (list #/c-x (make-extended-command *rubout-handler-control-x-comtab*))))

(unless (comtab-indirect-to *rubout-handler-comtab*)
  (init-rubout-handler-comtab))


(compile-flavor-methods editor-rubout-displayer)
