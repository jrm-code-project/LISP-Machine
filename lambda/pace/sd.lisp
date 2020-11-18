;;; -*- Mode:LISP; Package:USER; Base:10; Readtable:CL -*-

(defvar *supdup-server-default-rubout-handler* nil)

;:CLEAR-BETWEEN-CURSORPOSES
;:INSERT-STRING (optional)
;:DELETE-STRING (optional)

(defconst %tdmov #o200 "move: oldv oldh newv newh")
(defconst %tdmv1 #o201 "move: newv newh")
(defconst %tdeof #o202 "clear to end of screen")
(defconst %tdeol #o203 "clear rest of line")
(defconst %tddlf #o204 "clear current char")
(defconst %tdcrl #o205 "fresh line")
(defconst %tdnop #o210)
(defconst %tdors #o214 "output reset")
(defconst %tdqot #o215 "quote one char")
(defconst %tdfs  #o216 "nondestructive forward space")
(defconst %tdmv0 #o217 "move: newv newh")
(defconst %tdclr #o220 "erase screen and home")
(defconst %tdbel #o221 "bell")
(defconst %tdilp #o223 "insert n lines")
(defconst %tddlp #o224 "delete n lines")
(defconst %tdicp #o225 "insert n chars")
(defconst %tddcp #o226 "delete n chars")
(defconst %tdbow #o227 "black chars on white screen")
(defconst %tdrst #o230 "reset bow and others")

(defconst %%tocid (byte 1 0))                   ;character insert/delete
(defconst %%tolid (byte 1 1))                   ;line insert/delete
(defconst %%tofci (byte 1 3))                   ;generates real control-meta
(defconst %%tolwr (byte 1 4))                   ;can send lowercase
(defconst %%torol (byte 1 6))                   ;scroll if possible
(defconst %%tomor (byte 1 7))                   ;do more processing
(defconst %%tomvu (byte 1 8))                   ;can move cursor up
(defconst %%toovr (byte 1 9))                   ;overstrikes
(defconst %%tosa1 (byte 1 10.))                 ;display sail characters
(defconst %%tosai (byte 1 11.))                 ;sail characters
(defconst %%tomvb (byte 1 12.))                 ;can backspace
(defconst %%toers (byte 1 14.))                 ;can erase
(defconst %%toclc (byte 1 15.))                 ;convert lower to upper case
(defconst %%toalt (byte 1 16.))                 ;convert #o175 and #o176 to #o33
(defconst %%tpcbs (byte 1 23.))                 ;intelligent terminal protocol "must be on"
(defconst %%tpors (byte 1 21.))                 ;do output resets

(defflavor t-stream
         (io-buffer
          input-process
          conn
          net-stream
          (tv:rubout-handler-buffer (tv:make-rubout-handler-buffer))
          (tv:old-typeahead nil)
          (cursor-x 0)
          (cursor-y 0)
          location
          tctyp
          ttyopt
          height
          width
          ttyrol
          ttysmt
          bucky-state
          bucky-bits
          (output-pending nil)
          (lock nil)
          )
         ()
  :settable-instance-variables
  (:default-handler t-stream-default-message-handler)
  )

(defflavor t-stream-insert-mixin
         ()
         ())

(defun t-stream-default-message-handler (operation &rest args)
  args
  (process-wait (format nil "oops ~s" operation) #'false))

(defun make-t-stream (conn net-stream input-process io-buffer)
  (make-instance 't-stream
                 :io-buffer io-buffer
                 :input-process input-process
                 :net-stream net-stream
                 :conn conn))


(defmethod (t-stream :send-move) ()
  (with-lock (lock)
    (send net-stream :tyo %tdmv0)
    (send net-stream :tyo cursor-y)
    (send net-stream :tyo cursor-x)
    (setq output-pending t)))

(defconst *lozenged-table* '((#\resume . "<RESUME>")
                             (#\break . "<BREAK>")
                             (#\abort . "<ABORT>")
                             ))

(defun fake-lozenged (c)
  (let ((entry (assq (int-char c) *lozenged-table*)))
    (cond ((null entry)
           (format nil "<~o>" (char-code c)))
          (t
           (cdr entry)))))

(defmethod (t-stream :tyo) (char &optional font)
  font
  (with-lock (lock)
    (let ((c (char-code char))
          (sail-p (ldb-test %%tosai ttyopt)))
      (cond ((and (not sail-p) (< c #o40))
             (send self :string-out (aref chaos:*telnet-output-translation-table* c)))
            ((and (not sail-p) (= c #o177))
             (send self :tyo #\^)
             (send self :tyo #\?))
            ((< c #o200)
             (send net-stream :tyo c)
             (incf cursor-x)
             (when (>= cursor-x width)
               (setq cursor-x 0)
               (incf cursor-y)
               (when (>= cursor-y height)
                 (setq cursor-y 0))
               (send self :send-move)
               (send net-stream :tyo %tdeol))
             (setq output-pending t))
            ((= c #\return)
             (setq cursor-x 0)
             (incf cursor-y)
             (when (>= cursor-y height)
               (setq cursor-y 0))
             (send self :send-move)
             (send net-stream :tyo %tdeol)
             (setq output-pending t))
            ((= c #\form)
             (send self :clear-screen))
            (t
             (send self :string-out (fake-lozenged c))))))
  char)

(defvar *sail-bitmap* (let ((x (make-array 256.
                                           :type :art-1b
                                           :initial-element 1)))
                        ;;this makes first 128. easy, rest hard
                        (dotimes (i 128.)
                          (aset 0 x i))
                        x))

(defvar *ascii-bitmap* (let ((x (make-array 256. :type :art-1b)))
                         (dotimes (i 256.)
                           (cond ((< i #o40)
                                  (aset 1 x i))
                                 ((< i 127.)
                                  (aset 0 x i))
                                 (t
                                  (aset 1 x i))))
                         x))

(defun string-search-bitmap-slow (bitmap string start end)
  (when (null end)
    (setq end (string-length string)))
  (do ((i start (1+ i)))
      ((= i end))
    (if (not (zerop (aref bitmap (aref string i))))
        (return i))))

(defconst *string-search-bitmap-function* 'string-search-bitmap-slow)

;this will turn on the microcode assist  -  it doesn't seem to really be needed though
;(when (and (find-package "MICRO")
;          (not (eq (find-package "MICRO") (find-package "UA"))))
;  (let ((func (intern "STRING-SEARCH-BITMAP" "MICRO")))
;    (when (fboundp func)
;      (setq *string-search-bitmap-function* (symbol-function func)))))



(defmethod (t-stream :string-out) (string &optional (init-start 0) end)
  (when (null end)
    (setq end (string-length string)))
  (with-lock (lock)
    (do ((start init-start)
         )
        ((>= start end))
      (let ((next-hard (min (or (funcall *string-search-bitmap-function*
                                         (if (ldb-test %%tosai ttyopt)
                                             *sail-bitmap*
                                           *ascii-bitmap*)
                                         string
                                         start
                                         nil)
                                end)
                            (+ start (- width cursor-x))
                            )))
        (cond ((> next-hard start)
               (send net-stream :string-out string start next-hard)
               (incf cursor-x (- next-hard start))
               (setq start next-hard))
              (t
               (send self :tyo (aref string start))
               (incf start)))))
    (setq output-pending t)))

(defmethod (t-stream :line-out) (string &optional (start 0) end)
  (send self :string-out string start end)
  (send self :tyo #\return)
  )

(defmethod (t-stream :any-tyi) ()
  (send self :tyi))

(defmethod (t-stream :tyi) (&optional eof-error-p &aux index)
  eof-error-p
  (cond ((> (tv:rhb-fill-pointer) (setq index (tv:rhb-scan-pointer)))
         (setf (tv:rhb-scan-pointer) (1+ index))
         (aref tv:rubout-handler-buffer index))
        ((not (eq rubout-handler self))
         (let ((char (tv:io-buffer-get io-buffer)))
           (cond ((and (eq tv:rubout-handler-inside self)
                       (eq tv:old-typeahead t)
                       (consp char)
                       (not (eq (car char) 'tv:redisplay-rubout-handler)))
                  (multiple-value-bind (string index)
                      (send self :save-rubout-handler-buffer)
                    (setq tv:old-typeahead (list string index))
                    (send self :untyi char)
                    (char-int #\clear-input)))
                 (t
                  (char-int char)))))
        (t
         (send self :rubout-handler-tyi))))

(defmethod (t-stream :untyi) (ch)
  (cond ((and (eq rubout-handler self)
              (<= (tv:rhb-scan-pointer) (tv:rhb-fill-pointer))
              (eq ch (aref tv:rubout-handler-buffer (1- (tv:rhb-scan-pointer)))))
         (decf (tv:rhb-scan-pointer)))
        (t
         (tv:io-buffer-unget io-buffer ch)))
  ch)

(defmethod (t-stream :clear-input) ()
  (setf (tv:rhb-fill-pointer) 0)
  (setf (tv:rhb-scan-pointer) 0)
  (tv:io-buffer-clear io-buffer))

(defmethod (t-stream :backward-char) (&optional char)
  char
  (when (> cursor-x 0)
    (setq cursor-x (1- cursor-x)))
  (send self :send-move))

(defvar *t-stream-prompt-starting-x*)
(defvar *t-stream-prompt-starting-y*)
(defvar *t-stream-activation-character*)

(defmethod (t-stream :rubout-handler-tyi) ()
  (block top
    (setf (tv:rhb-typein-pointer) nil)          ;mark that simple rubout handler is in use
    (when (= (tv:rhb-scan-pointer) most-positive-fixnum)
      (setf (tv:rhb-scan-pointer) 0)
      (throw 'tv:rubout-handler t))
    (let ((status (tv:rhb-status))
          (rubbed-out-some nil)
          (rubout-handler nil))
      (setf (tv:rhb-status) nil)
      (when (memq status '(:initial-entry :restored))
        (let ((prompt-option (assq :prompt rubout-handler-options)))
          (when prompt-option
            (tv::rubout-handler-prompt (cadr prompt-option) self nil)))
        (multiple-value (tv:rubout-handler-starting-x tv:rubout-handler-starting-y)
          (send self :read-cursorpos))
        (when (plusp (tv:rhb-fill-pointer))
          (send self :string-out tv:rubout-handler-buffer))
        (let ((initial-input (cadr (assq :initial-input rubout-handler-options))))
          (when initial-input
            (string-nconc tv:rubout-handler-buffer initial-input))))
      (or (prog1 tv:rubout-handler-activation-character
                 (setq tv:rubout-handler-activation-character nil))
          (do ((editing-command (cdr (assq :editing-command rubout-handler-options)))
               (do-not-echo (cdr (assq :do-not-echo rubout-handler-options)))
               (pass-through (cdr (assq :pass-through rubout-handler-options)))
               (command-handler (assq :command rubout-handler-options))
               (preemptable (assq :preemptable rubout-handler-options))
               (activation-handler (assq :activation rubout-handler-options))
               ch len)
              (nil)
            (setq ch (send self :any-tyi))
            (cond ((eq (car-safe ch) 'tv:redisplay-rubout-handler)
                   (send self :set-cursorpos tv:prompt-starting-x tv:prompt-starting-y)
                   (send self :clear-rest-of-line)
                   (and (setq len (or (assq :reprompt rubout-handler-options)
                                      (assq :prompt rubout-handler-options)))
                        (tv:rubout-handler-prompt (cadr len) self ch))
                   (multiple-value (tv:rubout-handler-starting-x tv:rubout-handler-starting-y)
                     (send self :read-cursorpos))
                   (send self :string-out tv:rubout-handler-buffer))
                  ((consp ch)
                   (when preemptable
                     (setf (tv:rhb-scan-pointer) 0)
                     (throw 'tv:return-from-rubout-handler
                            (values ch (cadr preemptable)))))
                  ((and command-handler
                        (apply (cadr command-handler) ch (cddr command-handler)))
                   (setf (tv:rhb-scan-pointer) 0)
                   (throw 'tv::return-from-rubout-handler
                          (values
                            `(:command ,ch 1)
                            :command)))
                  ;; Don't touch this character, just return it to caller.
                  ((or (memq ch editing-command)
                       (si:assq-careful ch editing-command))
                   ;; Cause rubout handler rescan next time the user does :TYI.
                   (if rubbed-out-some (setf (tv:rhb-scan-pointer) most-positive-fixnum))
                   (return ch))
                  ((and (not (or (memq ch do-not-echo)
                                 (memq ch pass-through)
                                 (and activation-handler
                                      (apply (cadr activation-handler) ch (cddr activation-handler)))))
                        (or (not (zerop (char-bits ch)))
                            (member ch '(#\Rubout #\Clear-input #\Clear-screen #\Delete) :test #'char-equal)))
                   (cond ((member ch '(#\Form #\Delete #\c-L) :test #'char-equal)       ;Retype buffered input
                          (if (not (char-equal ch #\Delete)) (send self :clear-window) (send self :tyo #\Return))
                          (multiple-value (tv:prompt-starting-x tv:prompt-starting-y)
                            (send self :read-cursorpos))
                          (let ((prompt (cadr (or (assq :reprompt rubout-handler-options)
                                                  (assq :prompt rubout-handler-options)))))
                            (when prompt
                              (tv::rubout-handler-prompt prompt self ch)))
                          (multiple-value (tv:rubout-handler-starting-x tv:rubout-handler-starting-y)
                            (send self :read-cursorpos))
                          (send self :string-out tv:rubout-handler-buffer))
                         ((member ch '(#\rubout #\clear-input #\c-U) :test #'char-equal)
                          (cond ((not (zerop (setq len (tv:rhb-fill-pointer))))
                                 (setf (tv:rhb-fill-pointer)
                                       (setq len (case (int-char ch)
                                                   (#\rubout (1- len))
                                                   ((#\clear-input #\c-U) 0))))
                                 (setf rubbed-out-some t
                                       (tv:rhb-status) :rubout)
                                 (multiple-value-bind (x y)
                                     (send self :compute-motion tv:rubout-handler-buffer 0 len
                                           tv:rubout-handler-starting-x tv:rubout-handler-starting-y)
                                   (if tv:rubout-handler-re-echo-flag
                                       (setq x tv:rubout-handler-starting-x y tv:rubout-handler-starting-y))
                                   (multiple-value-bind (cx cy) (send self :read-cursorpos)
                                     (send self :clear-between-cursorposes x y cx cy))
                                   (send self :set-cursorpos x y)
                                   (and tv:rubout-handler-re-echo-flag
                                        (send self :string-out tv:rubout-handler-buffer))))))
                         (t
                          (send self :tyo #\!)
                          (send self :beep)))
                   (cond ((and (zerop (tv:rhb-fill-pointer))
                               (assq :full-rubout rubout-handler-options))
                          (setf (tv:rhb-scan-pointer) 0)
                          (throw 'rubout-handler t))))
                  (t
                   ;; It's a self-inserting character.
                   ;; If this is first character typed in, re-get starting cursorpos since while
                   ;; waiting for input a notification may have been typed out.
                   (when (zerop (tv:rhb-fill-pointer))
                     (multiple-value (tv:rubout-handler-starting-x tv:rubout-handler-starting-y)
                       (send self :read-cursorpos)))
                   (cond ((memq ch do-not-echo)
                          (setq tv:rubout-handler-activation-character ch))
                         ((and activation-handler
                               (apply (cadr activation-handler) ch (cddr activation-handler)))
                          (setq ch `(:activation ,ch 1))
                          (setq tv:rubout-handler-activation-character ch))
                         ((not (zerop (char-bits ch)))
                          (send self :tyo #\=)
                          (send self :beep))
                         (t
                          (send self :tyo ch)
                          (array-push-extend tv:rubout-handler-buffer ch)))
                   (cond (rubbed-out-some
                          (setf (tv:rhb-scan-pointer) 0)
                          (throw 'rubout-handler t))
                         (t
                          (setf (tv:rhb-scan-pointer) (tv:rhb-fill-pointer))
                          (setq tv:rubout-handler-activation-character nil)
                          (return ch))))))))))

(defmethod (t-stream :rubout-handler) (options function &rest args)
  (declare (arglist rubout-handler-options function &rest args))
  (if (and (eq rubout-handler self) (not (cdr (assq :nonrecursive options))))
      (let ((rubout-handler-options (append options rubout-handler-options)))
        (apply function args))
    (let ((rubout-handler-options options))
      (if (<= (tv:rhb-fill-pointer) (tv:rhb-scan-pointer))
          (setf (tv:rhb-fill-pointer) 0)
        (copy-array-portion tv:rubout-handler-buffer (tv:rhb-scan-pointer) (tv:rhb-fill-pointer)
                            tv:rubout-handler-buffer 0 (array-length tv:rubout-handler-buffer))
        (if (numberp (tv:rhb-typein-pointer))
            (decf (tv:rhb-typein-pointer) (tv:rhb-scan-pointer)))
        (decf (tv:rhb-fill-pointer) (tv:rhb-scan-pointer)))
      (setf (tv:rhb-scan-pointer) 0 (tv:rhb-status) :initial-entry)
      (catch 'tv::return-from-rubout-handler
        (let (tv:prompt-starting-x tv:prompt-starting-y
              tv:rubout-handler-starting-x tv:rubout-handler-starting-y
              (tv:rubout-handler self)
              (tv:rubout-handler-inside self)
              (tv:rubout-handler-re-echo-flag nil)
              (tv:rubout-handler-activation-character nil))
          (multiple-value (tv:prompt-starting-x tv:prompt-starting-y) (send self :read-cursorpos))
          (setq tv:rubout-handler-starting-x tv:prompt-starting-x
                tv:rubout-handler-starting-y tv:prompt-starting-y)
          (do-forever
            (setq tv:rubout-handler-re-echo-flag nil)
            (catch 'tv:rubout-handler                   ;Throw here when rubbing out
              (condition-case (error)
                  (return
                   (multiple-value-prog1
                     (apply function args)      ;Call read type function
                     (setf (tv:rhb-fill-pointer) (tv:rhb-scan-pointer))
                     (and (tv:rhb-typein-pointer)
                          (> (tv:rhb-typein-pointer) (tv:rhb-fill-pointer))
                          (setf (tv:rhb-typein-pointer) (tv:rhb-fill-pointer)))))
                (si:parse-error
                 (send self :fresh-line)
                 (princ ">>ERROR: " self)
                 (send error :report self)
                 (send self :fresh-line)
                 (setq tv:rubout-handler-re-echo-flag t)
                 (do-forever (send self :tyi)))))               ;If error, force user to rub out
            ;;Maybe return when user rubs all the way back
            (and (zerop (tv:rhb-fill-pointer))
                 (let ((full-rubout-option (assq :full-rubout rubout-handler-options)))
                   (when full-rubout-option
                     ;; Get rid of the prompt, if any.
                     (multiple-value-bind (x y)
                         (send self :cursorpos)
                       (send self :clear-between-cursorposes tv:prompt-starting-x tv:prompt-starting-y x y))
                     (return (values nil (cadr full-rubout-option))))))))))))

(defmethod (t-stream :read-cursorpos) (&optional (units :pixel))
  (cond ((eq units :pixel)
         (values (* cursor-x 8) (* cursor-y 14.)))
        (t
         (values cursor-x cursor-y))))

(defmethod (t-stream :set-cursorpos) (x y &optional (units :pixel))
  (when (eq units :pixel)
    (setq x (floor x 8))
    (setq y (floor y 14.)))
  (when (>= x width)
    (setq x 0))
  (when (>= y height)
    (setq y 0))
  (setq cursor-x x)
  (setq cursor-y y)
  (send self :send-move))

(defmethod (t-stream :clear-between-cursorposes) (start-x start-y end-x end-y)
  (with-lock (lock)
    (setq start-x (floor start-x 8))
    (setq start-y (floor start-y 14.))
    (setq end-x (floor end-x 8))
    (setq end-y (floor end-y 14.))
    (when (and (< start-x width)
               (<= end-x width)
               (< start-y height)
               (<= end-y height))
      (let ((old-x cursor-x)
            (old-y cursor-y))
        (cond ((= start-y end-y)
               (when (< start-x end-x)
                 (setq cursor-x start-x)
                 (setq cursor-y start-y)
                 (send self :send-move)
                 (cond ((ldb-test %%toovr ttyopt)
                        (dotimes (i (- end-x start-x))
                          (send net-stream :tyo %tddlf)
                          (send net-stream :tyo #\space)))
                       (t
                        (dotimes (i (- end-x start-x))
                          (send net-stream :tyo #\space))))))
              (t
               (setq cursor-x start-x)
               (setq cursor-y start-y)
               (send self :send-move)
               (send net-stream :tyo %tdeol)
               (setq cursor-x 0)
               (incf cursor-y)
               (when (>= cursor-y height)
                 (setq cursor-y 0))
               (send self :send-move)
               (do ()
                   ((= cursor-y end-y))
                 (send self :send-move)
                 (send net-stream :tyo %tdeol)
                 (incf cursor-y)
                 (when (>= cursor-y height)
                   (setq cursor-y 0)))
               (send self :send-move)
               (cond ((ldb-test %%toovr ttyopt)
                      (dotimes (i end-x)
                        (send net-stream :tyo %tddlf)
                        (send net-stream :tyo #\space)))
                     (t
                      (dotimes (i end-x)
                        (send net-stream :tyo #\space))))))
        (setq cursor-x old-x)
        (setq cursor-y old-y)
        (send self :send-move)
        (setq output-pending t)))))

(defmethod (t-stream :compute-motion) (string &optional (start 0) (end nil) x y (cr-at-end-p nil) (stop-x 0) (stop-y nil)
                                       bottom-limit right-limit font (line-ht 14.) (tab-width (* 8 8))
                                       &aux (char-width 8) final-index max-x)
  (declare (values final-x final-y final-string-index maximum-x))
  font
  (when (null end) (setq end (string-length string)))
  (when (null x) (setq x (* cursor-x char-width)))
  (when (null y) (setq y (* cursor-y line-ht)))
  (when (null stop-y) (setq stop-y (* height line-ht)))
  (when (null bottom-limit) (setq bottom-limit (* (1- height) line-ht)))
  (when (null right-limit) (setq right-limit (* width char-width)))
  (setq max-x x)
  (let ((sail-p (ldb-test %%tosai ttyopt)))
    (catch 'done
        (labels ((do-string (str s e top-level)
                            (do ((index s (1+ index)))
                                ((>= index e))
                              (when top-level
                                (setq final-index index))
                              (let ((c (char-code (aref str index))))
                                (cond ((and (not sail-p)
                                            (< c #o40))
                                       (let ((str (aref chaos:*telnet-output-translation-table* c)))
                                         (do-string str 0 (string-length str) nil)))
                                      ((and (not sail-p)
                                            (= c #o177))
                                       (do-string "^?" 0 2 nil))
                                      ((< c #o200)
                                       (setq x (+ x char-width))
                                       (when (>= x right-limit)
                                         (setq x 0)
                                         (setq y (+ y line-ht))
                                         (when (>= y bottom-limit)
                                           (setq y 0)))
                                       (setq max-x (max x max-x))
                                       (when (and (> x stop-x) (> y stop-y))
                                         (throw 'done nil)))
                                      ((= c #\return)
                                       (setq x 0)
                                       (setq y (+ y line-ht))
                                       (when (>= y bottom-limit)
                                         (setq y 0))
                                       (when (> y stop-y)
                                         (throw 'done nil)))
                                      ((= c #\tab)
                                       (setq x (* (ceiling x tab-width) tab-width))
                                       (when (>= x right-limit)
                                         (setq x 0)
                                         (setq y (+ y line-ht))
                                         (when (>= y bottom-limit)
                                           (setq y 0)))
                                       (setq max-x (max x max-x))
                                       (when (and (> x stop-x) (> y stop-y))
                                         (throw 'done nil)))
                                      ((= c #\form)
                                       (setq x 0)
                                       (setq y 0))
                                      (t
                                       (let ((loz (fake-lozenged c)))
                                         (do-string loz 0 (string-length loz) nil))))))))
          (do-string string start end t)
          (when cr-at-end-p
            (setq final-index t)
            (do-string #.(format nil "~%") 0 1 nil))
          (setq final-index nil))
        ))
  (values x y final-index max-x))

(defmethod (t-stream :beep) (&optional beep-type)
  beep-type
  (with-lock (lock)
    (send net-stream :tyo %tdbel)
    (setq output-pending t)))

(defmethod (t-stream :clear-screen) ()
  (with-lock (lock)
    (send net-stream :tyo %tdclr)
    (setq cursor-x 0)
    (setq cursor-y 0)
    (setq output-pending t)))

(defmethod (t-stream :clear-window) ()
  (with-lock (lock)
    (send net-stream :tyo %tdclr)
    (setq cursor-x 0)
    (setq cursor-y 0)
    (setq output-pending t)))

(defmethod (t-stream :fresh-line) ()
  (when (not (zerop cursor-x))
    (send self :tyo #\return)))

(defmethod (t-stream :clear-rest-of-line) ()
  (with-lock (lock)
    (send net-stream :tyo %tdeol)
    (setq output-pending t)))

(defmethod (t-stream :quit) ()
  (send self :clear-screen)
  (format self "Bye.~%")
  (with-lock (lock)
    (send net-stream :finish)
    (send net-stream :close)))

(defun user:quit ()
  (send-if-handles terminal-io :quit))

;fancy rubout handler stuff
;------
(defmethod (t-stream :save-rubout-handler-buffer) ()
  (when (eq tv:rubout-handler-inside self)
    ;; Give rubout handler function a chance to put its internal data
    ;; into RUBOUT-HANDLER-BUFFER where we look for it.
    (values (copy-seq tv:rubout-handler-buffer) (tv:rhb-typein-pointer))))

(defmethod (t-stream :restore-rubout-handler-buffer) (string &optional pointer)
  (let ((length (array-active-length string)))
    (or ( (array-length tv:rubout-handler-buffer) length)
        (adjust-array-size tv:rubout-handler-buffer length))
    (copy-array-contents string tv:rubout-handler-buffer)
    (setf (tv:rhb-fill-pointer) length))
  (setf (tv:rhb-typein-pointer) pointer)
  (send self :refresh-rubout-handler)
  (setf (tv:rhb-scan-pointer) 0)
  ;(setf (rhb-status) :restored)
  (throw 'tv:rubout-handler t))

(defmethod (t-stream :refresh-rubout-handler) (&optional discard-last-character)
  (if discard-last-character
      (setf (tv:rhb-fill-pointer) (max 0 (1- (tv:rhb-fill-pointer)))))
  (if (tv:rhb-typein-pointer)
      (setf (tv:rhb-typein-pointer)
            (min (tv:rhb-typein-pointer) (tv:rhb-fill-pointer))))
  (send self :fresh-line)
  (let ((prompt (or (assq :reprompt rubout-handler-options)
                    (assq :prompt rubout-handler-options))))
    (when prompt (tv:rubout-handler-prompt (cadr prompt) self #\Delete)))
  (multiple-value (tv:rubout-handler-starting-x tv:rubout-handler-starting-y)
    (send self :read-cursorpos))
  (send self :string-out tv:rubout-handler-buffer))
;------

(defun supdup-lisp-top-level (stream)
  (si:lisp-top-level1 stream))

(defun supdup-server-function ()
  (let (conn)
    (setq conn (chaos:listen "SUPDUP"))
    (let ((lose (chaos:disallow-connection? "SUPDUP" conn
                                            (list chaos:telnet-server-on :reject-unwanted))))
      (when lose
        (chaos:reject conn lose)
        (return-from supdup-server-function nil))
      (chaos:accept conn)
      (push conn chaos:eval-server-connections)
      (send tv:who-line-file-state-sheet :add-server conn "SUPDUP")
      (with-open-stream (net-stream (chaos:make-stream conn))
        (send net-stream :string-out "Lisp Machine Supdup")
        (send net-stream :tyo #o15)
        (send net-stream :tyo #o12)
        (send net-stream :tyo %tdnop)
        (send net-stream :force-output)
        (let ((lisp-proc (make-process
                           "Lisp for SUPDUP"
                           :regular-pdl-size (array-length
                                               (si:sg-regular-pdl
                                                 (send si:initial-process :initial-stack-group)))))
              (io-buffer (tv:make-io-buffer 100.)))
          (let* ((t-stream (make-t-stream conn net-stream current-process io-buffer))
                 (lock-location (si:locate-in-instance t-stream 'lock))
                 error-p)
            (read-supdup-parameters net-stream t-stream)
            (send t-stream :clear-screen)
            (print-herald t-stream)
            (format t-stream "~2&")
            (send lisp-proc :preset #'supdup-lisp-top-level t-stream)
            (process-reset-and-enable lisp-proc)
            (send t-stream :set-bucky-state nil)
            (do-forever
              (let (start-time)
                (process-wait "Net Input"
                              #'(lambda ()
                                  (block top
                                    (when (send t-stream :output-pending)
                                      (cond ((null start-time)
                                             (setq start-time (time)))
                                            ((> (time-difference (time) start-time) 15.)
                                             (return-from top t))))
                                    (multiple-value-bind (input-p err-p)
                                        (ignore-errors
                                          (send net-stream :listen))
                                      (when err-p
                                        (setq error-p t))
                                      (or input-p err-p))))))
              (unwind-protect
                  (progn
                    (when (null (%store-conditional lock-location nil current-process))
                      (process-lock lock-location current-process "Lock" (* 60. 30.)))
                    (if (or error-p
                            (not (eq (chaos:state conn) 'chaos:open-state)))
                        (return nil))
                    (send net-stream :force-output)
                    (when (send net-stream :listen)
                      (let ((c (send net-stream :tyi)))
                        (cond ((= c #o300)
                               (process-supdup-escape net-stream t-stream))
                              (t
                               (process-net-input c io-buffer t-stream))))))
                (%store-conditional lock-location current-process nil)))
            (send lisp-proc :kill)
            (send net-stream :close t)))))))


(defun process-net-input (c io-buffer t-stream)
  (ecase (send t-stream :bucky-state)
    (:bits
     (cond ((= c #o34)
            (tv:io-buffer-put io-buffer #o34))
           (t
            (send t-stream :set-bucky-bits c)
            (send t-stream :set-bucky-state :char))))
    (:char
     (let* ((bits (send t-stream :bucky-bits))
            (control-p (ldb-test (byte 1 0) bits))
            (meta-p (ldb-test (byte 1 1) bits)))
       (when (or control-p meta-p)
         (setq c (char-flipcase c)))
       (when control-p
         (setq c (set-char-bit c :control t)))
       (when meta-p
         (setq c (set-char-bit c :meta t))))
     ;;bit (byte 1 4) is TOP, but I can't figure out how to use it
     (tv:io-buffer-put io-buffer c)
     (send t-stream :set-bucky-state nil))
    (:metaify
     (setq c (set-char-bit (char-flipcase c) :meta t))
     (tv:io-buffer-put io-buffer c)
     (send t-stream :set-bucky-state nil))
    (nil
     (cond ((= c #o34)
            (send t-stream :set-bucky-state :bits))
           ((= c #o15)
            (tv:io-buffer-put io-buffer #\return))
           ((= c #o177)
            (tv:io-buffer-put io-buffer #\rubout))
           ((zerop (ldb %%tofci (send t-stream :ttyopt)))
            ;;terminal can't generate real control/meta
            (setq c (logand c #o177))
            (cond ((= c #o33)
                   (send t-stream :set-bucky-state :metaify))
                  ((< c #o40)
                   (setq c (tv:set-char-bit (+ c #\@) :control t))
                   (tv:io-buffer-put io-buffer c))
                  (t
                   (tv:io-buffer-put io-buffer c))))
           (t
            (tv:io-buffer-put io-buffer c))))))

(defun process-supdup-escape (stream t-stream)
  (let ((command (send stream :tyi)))
    (case command
      (#o302
       (send t-stream :set-location
             (with-output-to-string (s)
               (do ((c (send stream :tyi) (send stream :tyi)))
                   ((zerop c))
                 (send s :tyo c)))))
      )))

(defun read-18-bits (stream)
  (let* ((b0 (send stream :tyi))
         (b1 (send stream :tyi))
         (b2 (send stream :tyi)))
    (+ (ash b0 12.)
       (ash b1 6)
       b2)))

(defun read-supdup-parameters (stream t-stream)
  (let ((nwords (dpb (read-18-bits stream) (byte 18. 0) -1)))
    (read-18-bits stream)
    (when (< nwords 0)
      (let* ((x (read-18-bits stream))
             (y (read-18-bits stream)))
        x
        (send t-stream :set-tctyp y))
      (incf nwords))
    (when (< nwords 0)
      (let* ((x (read-18-bits stream))
             (y (read-18-bits stream)))
        (send t-stream :set-ttyopt (dpb y (byte 18. 18.) x)))
      (incf nwords))
    (when (< nwords 0)
      (let* ((x (read-18-bits stream))
             (y (read-18-bits stream)))
        x
        (send t-stream :set-height y))
      (incf nwords))
    (when (< nwords 0)
      (let* ((x (read-18-bits stream))
             (y (read-18-bits stream)))
        x
        (send t-stream :set-width y))
      (incf nwords))
    (when (< nwords 0)
      (let* ((x (read-18-bits stream))
             (y (read-18-bits stream)))
        x
        (send t-stream :set-ttyrol y))
      (incf nwords))
    (when (< nwords 0)
      (let* ((x (read-18-bits stream))
             (y (read-18-bits stream)))
        (send t-stream :set-ttysmt (dpb y (byte 18. 18.) x)))
      (incf nwords))
    (do ()
        ((zerop nwords))
      (read-18-bits stream)
      (read-18-bits stream)
      (incf nwords))))


(add-initialization "SUPDUP"
                    '(process-run-function "SUPDUP Server" 'supdup-server-function)
                    NIL
                    'chaos:server-alist)
