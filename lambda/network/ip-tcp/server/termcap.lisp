;;; -*- Mode:LISP; Package:TELNET; Base:10; Readtable:CL -*-

#|

  Copyright LISP Machine, Inc. 1985, 1986, 1987
   See filename "Copyright.Text" for
  licensing and release information.

|#

;;;TERMCAP definition

(defstruct (termcap
             (:conc-name termcap.)
             (:print-function (lambda (termcap stream ignore)
                                (sys:printing-random-object
                                  (termcap stream :type :no-pointer)
                                  (format stream "of ~A"
                                          (termcap.name termcap))))))
  name
  nicknames
  documentation
  add-blank-line
  (back-space #o10)
  clear-to-end-of-display
  clear-to-end-of-line
  clear-to-beginning-of-line
  clear-screen
  cursor-motion
  (number-of-columns 256)
  (number-of-lines 1000000)
  (carriage-return '(#o15 0))
  change-scrolling-region
  cursor-horizontal-motion
  cursor-vertical-motion
  delete-character
  delete-line
  enter-delete-mode
  down-one-line
  end-delete-mode
  enter-insert-mode
  end-insert-mode
  (form-feed #o14)
  hardcopy-p
  home-cursor
  insert-character
  initialization-string
  cursor-right
  (line-feed #o12)
  (tab-stops 8)
  (tab #o11)
  cursor-up
  visible-bell
  (audible-bell #o7)
  (linewrap-indicator #\!)
  auto-new-line
  cursor-down
  cursor-left
  (cursor-motion-characters 0)
  clear-character
  multiple-character-insert
  multiple-character-delete
  multiple-line-insert
  multiple-line-delete
  fresh-line-and-clear
  (clear-screen-homes-cursor t)
  (selective-erase t)
  extended-keyboard
  overprint
  line-insert-delete
  character-insert-delete
  (new-line '(#o15 #o12))
  )

(defmacro define-termcap (name documentation &rest body)
  "Define a terminals capabilities in terms of how to do various operations"
  `(*define-termcap ',name ',documentation
                    (make-termcap ,@body)))

(defun *define-termcap (name documentation structure)
  (when (global:record-source-file-name name 'define-termcap)
    (setf (termcap.name structure) name)
    (setf (termcap.documentation structure) (or documentation name "NIL"))
    (setf (documentation name 'termcap) (termcap.documentation structure))
    (dolist (x (cons name (termcap.nicknames structure)))
      (let ((symbol (intern (string x) "")))
        (setf (get symbol 'termcap) structure)))
    structure))

(define-termcap default "the default terminal capabilities"
  )

(defvar *esc* #o33)

(define-termcap h19 "Now manufactured by Zenith Data Systems. For Zenith-Mode functions"
  :nicknames '(h-19 z29 z-29 heath zenith)
  :add-blank-line '(*esc* "L")
  :clear-to-end-of-display '(*esc* "J")
  :clear-to-end-of-line '(*esc* "K")
  :clear-to-beginning-of-line '(*esc* "l")
  :clear-screen '(*esc* "E")
  :cursor-motion '(*esc* "Y" (+ y 32) (+ x 32))
  :cursor-motion-characters 4
  :number-of-columns 80
  :number-of-lines 24
  :delete-character '(*esc* "N")
  :delete-line '(*esc* "M")
  :enter-insert-mode '(*esc* "@")
  :end-insert-mode '(*esc* "O")
  :home-cursor '(*esc* "H")
  :cursor-up '(*esc* "A")
  :cursor-down '(*esc* "B")
  :cursor-right '(*esc* "C")
  :cursor-left '(*esc* "D")
  :tab nil
  :auto-new-line t
  )

(define-termcap vt-100 "Another commonly used terminal"
  :nicknames '(vt100)
  :clear-to-end-of-display '(*esc* "[J")
  :clear-to-end-of-line '(*esc* "[K")
  :clear-screen '(*esc* "[;H" *esc* "[2J")
  :cursor-motion '(*esc* "[" y ";" x "H")
  :cursor-motion-characters 6
  :number-of-columns 80
  :number-of-lines 24
  :home-cursor '(*esc* "[H")
  )

;;;ASCII Stream Terminal that works as a value for *terminal-io*

(defflavor simple-ascii-stream-terminal
         (input
          output
          (output-lock nil)
          (need-force-output nil)
          (untyi-char nil)
          (term nil)
          (termcap nil))
         (si:stream tv:stream-mixin tv:sheet tv:essential-window)
  (:default-init-plist :blinker-p nil :deexposed-typeout-action :permit)
  :initable-instance-variables)

(defmethod (simple-ascii-stream-terminal :termcap) (type)
  (cond ((get type 'termcap)
         (setq term type)
         (setq termcap (get type 'termcap))
         (setq tv:line-height 1)
         (setq tv:char-width 1)
         (setq tv:width (termcap.number-of-columns termcap))
         (setq tv:height (termcap.number-of-lines termcap))
         (setf (tv:rhb-input-history) nil)
         (setf (tv:rhb-typein-pointer) nil)
         (setf (tv:rhb-scan-pointer) 0)
         (setf (tv:rhb-fill-pointer) 0)
         (setq tv:stream-rubout-handler
               (if (and (termcap.cursor-motion termcap)
                        (or (termcap.enter-insert-mode termcap)
                            (termcap.insert-character termcap))
                        (termcap.delete-character termcap))
                   'tv:alternate-rubout-handler
                 nil)))
        ('else
         (send self :termcap :default))))

(defmethod (simple-ascii-stream-terminal :after :init) (&rest ignored)
  (or termcap (send self :termcap term))
  (setq tv:superior nil)                        ;We are a screen
  (setq tv:line-height 1)
  (setq tv:char-width 1)
  (setq tv:width (termcap.number-of-columns termcap))
  (setq tv:height (termcap.number-of-lines termcap))
  (setf (tv:sheet-output-hold-flag self) 0)
  (send self :set-more-p t))

(defmethod (simple-ascii-stream-terminal :more-p) ()
  (not (null tv:more-vpos)))

(defmethod (simple-ascii-stream-terminal :set-more-p) (enable)
  (setq tv:more-vpos (and enable (- tv:height tv:line-height)))
  enable)

(defmethod (simple-ascii-stream-terminal :activate) ()
  t)

(defmethod (simple-ascii-stream-terminal :deactivate) ()
  t)

(defmethod (simple-ascii-stream-terminal :expose) ()
  t)

(defmethod (simple-ascii-stream-terminal :deexpose) ()
  t)

(defmethod (simple-ascii-stream-terminal :no-inferior-windows-p) ()
  t)

(defmethod (simple-ascii-stream-terminal :print-self) (stream ignore ignore)
  (if *print-escape*
      (si:printing-random-object (self stream :type :no-pointer)
        (format stream "~A: ~A" tv:name term))
    (send stream :string-out (string (or (send self :name-for-selection) tv:name)))))

(defmethod (simple-ascii-stream-terminal :notice) (event &rest args)
  (declare (ignore args))
  (case event
    ((:input :output)           ;Deexposed window needs some attention
     t)
    (:input-wait                ;Hanging up waiting for input.
     (setf (tv:sheet-more-flag self) 0)
     (cond ((null tv:more-vpos))                ;Unless MORE inhibited entirely
           ((< (* (- tv:height tv:cursor-y) 4)  ;More than 3/4 way down window?
               tv:height)
            ;; Wrap around and more just before the current line
            (setq tv:more-vpos (- tv:cursor-y tv:line-height)))
           (t ;; More at bottom
            (setq tv:more-vpos (- tv:height tv:line-height))))
     t)
    (:error                     ;Error in process using this window as its *TERMINAL-IO*.
     t)
    (otherwise nil)))

(defmethod (simple-ascii-stream-terminal :set-input-stream) (s)
  (setq input s))

(defmethod (simple-ascii-stream-terminal :initialize-terminal) ()
  (send self :output-control-sequence (termcap.initialization-string termcap))
  (send self :clear-window))

(defmethod (simple-ascii-stream-terminal :characters) ()
  t)

(defmethod (simple-ascii-stream-terminal :element-type) ()
  'character)

(defmethod (simple-ascii-stream-terminal :direction) ()
  :bidirectional)

(defsubst pixels-to-chars (x)
  (truncate x tv:char-width))

(defsubst chars-to-pixels (x)
  (* x tv:char-width))

(defsubst pixels-to-lines (y)
  (truncate y tv:line-height))

(defsubst lines-to-pixels (y)
  (* y tv:line-height))

;;;Output methods

(defmethod (simple-ascii-stream-terminal :tyo) (c)
  (with-lock (output-lock)
    (send self :tyo-unlocked c)))

(defmethod (simple-ascii-stream-terminal :string-out) (string &optional start end)
  (with-lock (output-lock)
    (do ((j (or start 0) (1+ j))
         (n (or end (length string))))
        ((= j n))
      (send self :tyo-unlocked (aref string j)))))

(defmethod (simple-ascii-stream-terminal :output-control-sequence) (s)
  (with-lock (output-lock)
    (setq need-force-output t)
    (send self :output-control-sequence-unlocked s)))

(defmethod (simple-ascii-stream-terminal :output-control-sequence-unlocked) (s)
  (cond ((null s))
        ((symbolp s)
         (cond ((boundp s)
                (send output :tyo (symbol-value s)))
               ((fboundp s)
                (funcall s output))))
        ((stringp s)
         (send output :string-out s))
        ((listp s)
         (dolist (z s)
           (send self :output-control-sequence-unlocked z)))
        ((integerp s)
         (send output :tyo s))
        ('else
         (funcall s output))))

(defmethod (simple-ascii-stream-terminal :force-output) ()
  (when output
    (with-lock (output-lock)
      (setq need-force-output nil)
      (send output :force-output))))

(defmethod (simple-ascii-stream-terminal :tyo-unlocked) (c)
  (when (plusp (tv:sheet-more-flag self))
    (send self :more-exception))
  (when (= tv:cursor-y tv:height)
    (send self :home-cursor)
    (send self :clear-rest-of-line))
  (cond ((null c))
        ((< c #o40))                            ;Ignore non-ASCII graphics
        ((= c #\Return)
         (send self :terpri))
        ((= c #\Tab)
         (send self :tab))
        ((= c #\Backspace)
         (when (plusp tv:cursor-x)
           (send self :output-control-sequence (termcap.back-space termcap))
           (decf tv:cursor-x tv:char-width)))
        ((graphic-char-p c)
         (cond ((= tv:cursor-x (- tv:width tv:char-width))
                (send self :end-of-line-exception c))
               (t
                (send output :tyo c)
                (incf tv:cursor-x tv:char-width)))
         (setq need-force-output t))
        ((and (zerop (char-bits c)) (> c #\Network)))
        (t))
  (and (zerop tv:cursor-x)
       (eql tv:cursor-y tv:more-vpos)
       (setf (tv:sheet-more-flag self) 1)))

(defmethod (simple-ascii-stream-terminal :more-exception) ()
  (when (plusp (tv:sheet-more-flag self))
    (setf (tv:sheet-more-flag self) 0)
    (let ((more-vpos tv:more-vpos))
      (unwind-protect
          (progn (princ "**MORE**" output)
                 (send self :clear-rest-of-line)
                 (force-output self)
                 (send self :clear-input)
                 (send self :any-tyi)
                 (setq tv:more-vpos more-vpos)
                 (send self :output-control-sequence (termcap.carriage-return termcap))
                 (send self :clear-rest-of-line))
        (cond ((= tv:cursor-y (- tv:height tv:line-height))
               ;;At bottom, wrap around.  Next MORE in same place
               (send self :home-cursor)
               (send self :clear-rest-of-line))
              ((null tv:more-vpos))             ;MORE inhibited
              ((< (* (- tv:height tv:cursor-y) 4)       ;More than 3/4 way down window?
                  tv:height)
               ;; Wrap around and MORE just before the current line
               (setq tv:more-vpos (- tv:cursor-y tv:line-height)))
              (t ;; More at bottom
               (setq tv:more-vpos (- tv:height tv:line-height))))))))

(defmethod (simple-ascii-stream-terminal :end-of-line-exception) (c)
  (let ((linewrap (termcap.linewrap-indicator termcap))
        (auto (termcap.auto-new-line termcap)))
    (cond (auto
           (cond ((= tv:cursor-y (- tv:height tv:line-height))
                  (send self :home-cursor)
                  (send self :clear-rest-of-line)
                  (send self :tyo c))
                 (linewrap
                  (send output :tyo linewrap)
                  (setq tv:cursor-x 0)
                  (incf tv:cursor-y tv:line-height)
                  (send self :clear-rest-of-line)
                  (when (eql tv:cursor-y tv:more-vpos)
                    (setf (tv:sheet-more-flag self) 1))
                  (send self :tyo c))
                 (t
                  (send output :tyo c)
                  (setq tv:cursor-x 0)
                  (incf tv:cursor-y tv:line-height)
                  (send self :clear-rest-of-line))))
          (linewrap
           (send output :tyo linewrap)
           (send self :terpri)
           (send self :tyo c))
          (t
           (send output :tyo c)
           (send self :terpri)))))

(defmethod (simple-ascii-stream-terminal :tab) ()
  (let ((stops (termcap.tab-stops termcap)))
    (cond ((null stops)
           (send self :string-out "        "))
          ((numberp stops)
           (let ((spaces (mod (pixels-to-chars tv:cursor-x) stops)))
             (dotimes (i (if (plusp spaces) spaces stops))
               (send self :tyo #\Space))))
          ('else
           ;; a list of tab stops. write this some other time
           nil
           ))))

(defmethod (simple-ascii-stream-terminal :terpri) (&aux fresh-line)
  (with-lock (output-lock)
    (setq tv:cursor-x 0)
    (incf tv:cursor-y tv:line-height)
    (cond ((= tv:cursor-y tv:height)
           (send self :home-cursor)
           (send self :clear-rest-of-line))
          ((setq fresh-line (termcap.fresh-line-and-clear termcap))
           (send self :output-control-sequence fresh-line))
          (t
           (send self :output-control-sequence (termcap.new-line termcap))
           (send self :clear-rest-of-line)))
    (when (eql tv:cursor-y tv:more-vpos)
      (setf (tv:sheet-more-flag self) 1))))

(defmethod (simple-ascii-stream-terminal :clear-rest-of-line) (&aux clear)
  (when (setq clear (termcap.clear-to-end-of-line termcap))
    (send self :output-control-sequence clear)))

(defmethod (simple-ascii-stream-terminal :clear-output) ()
  (with-lock (output-lock)
    (send output :clear-output)))

(defmethod (simple-ascii-stream-terminal :fresh-line) ()
  (or (zerop tv:cursor-x)
      (send self :terpri)))

(defmethod (simple-ascii-stream-terminal :beep) (&optional ignore)
  (let ((beep (termcap.audible-bell termcap)))
    (when beep
      (send self :output-control-sequence beep))))

(defmethod (simple-ascii-stream-terminal :clear-window) (&aux clear)
  (cond ((setq clear (termcap.clear-screen termcap))
         (send self :output-control-sequence clear)
         (cond ((termcap.clear-screen-homes-cursor termcap)
                (setq tv:cursor-x 0)
                (setq tv:cursor-y 0))
               (t
                (send self :home-cursor))))
        (t
         (send self :fresh-line)))
  t)

(defmethod (simple-ascii-stream-terminal :string-length) (string start end x)
  (do ((stops (termcap.tab-stops termcap))
       (index start (1+ index))
       (count 0)
       c)
      ((eql index end) count)
    (setq c (char string index))
    (when (null c)
      (return count))
    (cond ((< c #o40))
          ((= c #\Return))                      ;hmm
          ((= c #\Tab)
           (incf count (cond ((null stops) 8)
                             ((numberp stops)
                              (let ((x (mod (+ x count) stops)))
                                (if (zerop x) stops x)))
                             (t 0))))
          ((= c #\Backspace)
           (decf count))
          ((graphic-char-p c)
           (incf count))
          (t))))

(defmethod (simple-ascii-stream-terminal :compute-motion) (string &optional (start 0) (end (string-length string))
                                                           (x tv:cursor-x) (y tv:cursor-y))
  (declare (values end-x end-y))
  ;;Returns where cursor will be after buffer has been output.
  (setq x (pixels-to-chars x))
  (setq y (pixels-to-lines y))
  (do* ((end-x x)
        (end-y y)
        (stops (termcap.tab-stops termcap))
        (lines (truncate tv:height tv:line-height))
        (columns (- (truncate tv:width tv:char-width) (if (termcap.linewrap-indicator termcap) 1 0)))
        (index start (1+ index))
        c)
       ((eql index end)
        (values (chars-to-pixels end-x) (lines-to-pixels end-y)))
    (labels ((inc-y (dy)
               (incf end-y dy)
               (when (> end-y (- lines 2))
                 (setq end-y (mod end-y lines))))
             (inc-x (dx)
               (incf end-x dx)
               (when (> end-x columns)
                 (setq end-x (mod end-x columns))
                 (inc-y 1))))
      (setq c (char string index))
      (when (null c)
        (return (values (chars-to-pixels end-x) (lines-to-pixels end-y))))
      (cond ((< c #o40))
            ((= c #\Return)
             (setq end-x 0)
             (inc-y 1))
            ((= c #\Tab)
             (inc-x (cond ((null stops) 8)
                          ((numberp stops)
                           (let ((x (mod end-x stops)))
                             (if (zerop x) stops x)))
                          (t 0))))
            ((= c #\Backspace)
             (inc-x -1))
            ((graphic-char-p c)
             (inc-x 1))
            (t)))))

(defun eval-multiple-item (n item)
  (cond ((eq item 'n)
         n)
        ((and (symbolp item) (boundp item))
         (symbol-value item))
        ((atom item)
         item)
        ('else
         (apply (car item) (mapcar #'(lambda (z) (eval-multiple-item n z)) (cdr item))))))

(defmethod (simple-ascii-stream-terminal :insert-string) (string &optional (start 0) end (type-too t) &aux insert count)
  (when (null end)
    (setq end (string-length string)))
  (multiple-value-bind (x y)
      (send self :read-cursorpos)
    (setq count (send self :string-length string start end x))
    (cond ((setq insert (termcap.multiple-character-insert termcap))
           (dolist (elt insert)
             (send self :output-control-sequence (eval-multiple-item count elt)))
           (send self :string-out string start end))
          ((setq insert (termcap.enter-insert-mode termcap))
           (send self :output-control-sequence insert)
           (send self :string-out string start end)
           (send self :output-control-sequence (termcap.end-insert-mode termcap)))
          ((setq insert (termcap.insert-character termcap))
           (dotimes (i count)
             (send self :output-control-sequence insert))
           (send self :set-cursorpos x y)
           (send self :string-out string start end))
          (t                                    ;Sorry...
           ))
    (unless type-too
      (send self :set-cursorpos x y))))

(defmethod (simple-ascii-stream-terminal :delete-string) (string &optional (start 0) end &aux delete count)
  (when (null end)
    (setq end (string-length string)))
  (multiple-value-bind (x ignore)
      (send self :read-cursorpos)
    (setq count (send self :string-length string start end x))
    (cond ((setq delete (termcap.multiple-character-delete termcap))
           (dolist (elt delete)
             (send self :output-control-sequence (eval-multiple-item count elt))))
          ((setq delete (termcap.delete-character termcap))
           (dotimes (i count)
             (send self :output-control-sequence delete)))
          ((setq delete (termcap.enter-delete-mode termcap))
           ;;I don't known how to use this...
           )
          (t                                    ;Sorry....
           ))))


;;;Input methods

(defmethod (simple-ascii-stream-terminal :any-tyi) (&optional ignore &aux idx c)
  (char-int-if-any
    (cond ((> (tv:rhb-fill-pointer) (setq idx (tv:rhb-scan-pointer)))
           ;;untyi'd characters...
           (incf (tv:rhb-scan-pointer))
           (aref tv:rubout-handler-buffer idx))
          ((not (eq tv:rubout-handler self))
           ;;rubout handling not in effect...
           (when need-force-output
             (send self :force-output))
           (unless (or untyi-char (send input :listen))
             (send self :notice :input-wait))
           (setq c (if untyi-char
                       (prog1 untyi-char (setq untyi-char nil))
                     (send input :tyi)))
           (cond ((null c) nil)
                 ((eq c #\Control-U)
                  #\Clear-Input)
                 ((eq c #\Control-R)
                  #\Delete)
                 (t
                  c)))
          (t
           ;;Rubout handler (will call us for new characters)
           (funcall (or tv:stream-rubout-handler 'tv:default-rubout-handler))))))

(defmethod (simple-ascii-stream-terminal :untyi) (ch)
  (if (and (eq tv:rubout-handler self)
           ;; RUBOUT-HANDLER added as conjunct 6/1/83
           ;; to avoid lossage entering editor rubout handler
           ;; by typing (= 1 2) then stray ) while inside BREAK.
           (<= 1 (tv:rhb-scan-pointer) (tv:rhb-fill-pointer))
           (eq ch (aref tv:rubout-handler-buffer (1- (tv:rhb-scan-pointer)))))
      (decf (tv:rhb-scan-pointer))
    (setq untyi-char ch))
  ch)

(defmethod (simple-ascii-stream-terminal :clear-input) ()
  (setf (tv:rhb-fill-pointer) 0)
  (setf (tv:rhb-scan-pointer) 0)
  (setq untyi-char nil)
  (send input :clear-input))

(defmethod (simple-ascii-stream-terminal :listen) ()
  (or (not (null untyi-char))
      (> (tv:rhb-scan-pointer) (tv:rhb-fill-pointer))
      (send input :listen)))

(defmethod (simple-ascii-stream-terminal :tyi) (&optional ignore)
  (loop
    (let ((c (send self :any-tyi)))
      (when (or (null c) (numberp c))
        (return c)))))

(defmethod (simple-ascii-stream-terminal :tyi-no-hang) ()
  (and (send self :listen) (send self :tyi)))

(defmethod (simple-ascii-stream-terminal :any-tyi-no-hang) ()
  (and (send self :listen) (send self :any-tyi)))

(defmethod (simple-ascii-stream-terminal :read-char-no-hang) ()
  (and (send self :listen) (send self :read-char)))

(defmethod (simple-ascii-stream-terminal :any-read-char-no-hang) ()
  (and (send self :listen) (send self :any-read-char)))

;;;Cursor positioning

(defmethod (simple-ascii-stream-terminal :home-cursor) ()
  (let ((s (termcap.home-cursor termcap)))
    (cond ((null s)
           (send self :set-cursorpos 0 0))
          ('else
           (setq tv:cursor-x 0)
           (setq tv:cursor-y 0)
           (send self :output-control-sequence s)))))

(defmethod (simple-ascii-stream-terminal :read-cursorpos) (&optional (type :pixel))
  (ecase type
    (:pixel
     (values tv:cursor-x tv:cursor-y))
    (:character
     (values (pixels-to-chars tv:cursor-x) (pixels-to-lines tv:cursor-y)))))

(defmethod (simple-ascii-stream-terminal :set-cursorpos) (x y &optional (type :pixel))
  (ecase type
    (:pixel
     (let ((cursor-motion (termcap.cursor-motion termcap)))
       (cond (cursor-motion                     ;Terminal can move cursor
              (dolist (item cursor-motion)
                (send self :output-control-sequence
                      (eval-cursorpos-item (pixels-to-chars x) (pixels-to-lines y) item)))
              (setq tv:cursor-x x)
              (setq tv:cursor-y y))
             ((= tv:cursor-y y)                 ;Same line
              (if (> x tv:cursor-x)
                  (dotimes (i (pixels-to-chars (- x tv:cursor-x)))
                    ;;***Probably destructive....
                    (send self :tyo #\Space))
                (dotimes (i (pixels-to-chars (- tv:cursor-x x)))
                  (send self :tyo #\Backspace))))
             (t                                 ;Different line.  Sorry....
              ))))
    (:character
     (send self :set-cursorpos (chars-to-pixels x) (lines-to-pixels y)))))

(defmethod (simple-ascii-stream-terminal :increment-cursorpos) (dx dy &optional (type :pixel))
  (when (plusp (tv:sheet-more-flag self))
    (send self :more-exception))
  (multiple-value-bind (x y)
      (send self :read-cursorpos type)
    (send self :set-cursorpos (+ x dx) (+ y dy) type)))

(defmethod (simple-ascii-stream-terminal :clear-between-cursorposes) (x1 y1 x2 y2)
  (normal-clear-between-cursorposes x1 y1 x2 y2))

(defun normal-clear-between-cursorposes (x1 y1 x2 y2)
  (declare (:self-flavor simple-ascii-stream-terminal))
  (cond ((null (termcap.cursor-motion termcap))
         ;;No cursor motion
         (cond ((= y1 y2)                       ;clear in same line
                (erase-chars (pixels-to-chars (- x2 x1))))
               ((= y2 tv:cursor-y)              ;multiple lines, but end on current line
                (erase-chars (pixels-to-chars x2)))
               (t                               ;sorry...
                )))
        ((= y1 y2)                              ;Erase within line
         (let ((delete (pixels-to-chars (- x2 x1))))
           (cond ((= tv:cursor-x x1)
                  (dotimes (i delete)
                    (send self :tyo #\Space))
                  (send self :set-cursorpos x1 y1))
                 ((or (/= tv:cursor-x x2)
                      (< (+ (* 2 (termcap.cursor-motion-characters termcap)) delete)
                         (* 3 delete)))
                  (send self :set-cursorpos x1 y1)
                  (dotimes (i delete)
                    (send self :tyo #\Space))
                  (send self :set-cursorpos x1 y1))
                 (t
                  (erase-chars delete)))))
        (t
         (let ((clear-to-bol (termcap.clear-to-beginning-of-line termcap))
               (clear-to-eol (termcap.clear-to-end-of-line termcap)))
           (cond (clear-to-bol
                  (send self :set-cursorpos x2 y2)
                  (send self :output-control-sequence clear-to-bol))
                 (t
                  (send self :set-cursorpos 0 y2)
                  (dotimes (i (pixels-to-chars x2))
                    (send self :tyo #\Space))))
           (dotimes (i (- (pixels-to-lines (+ y2 (if (> y2 y1) 0 tv:height)))
                          (pixels-to-lines y1)
                          1))
             (send self :set-cursorpos 0 (mod (+ y1 (* (1+ i) tv:line-height)) tv:height))
             (send self :output-control-sequence clear-to-eol))
           (send self :set-cursorpos x1 y1)
           (send self :output-control-sequence clear-to-eol)))))

(defun erase-chars (x)
  (dotimes (i x)
    (send self :tyo #\Backspace)
    (send self :tyo #\Space)
    (send self :tyo #\Backspace)))

(defun eval-cursorpos-item (x y item)
  (cond ((eq item 'x)
         x)
        ((eq item 'y)
         y)
        ((and (symbolp item) (boundp item))
         (symbol-value item))
        ((atom item)
         item)
        ('else
         (apply (car item) (mapcar #'(lambda (z) (eval-cursorpos-item x y z)) (cdr item))))))

;;;Default methods

(defmethod (simple-ascii-stream-terminal :read-char) ()
  (global:stream-default-handler self :read-char nil nil))

(defmethod (simple-ascii-stream-terminal :any-read-char) ()
  (global:stream-default-handler self :any-read-char nil nil))

(defmethod (simple-ascii-stream-terminal :read-byte) ()
  (global:stream-default-handler self :read-byte nil nil))

(defmethod (simple-ascii-stream-terminal :unread-char) (c)
  (global:stream-default-handler self :unread-char c nil))

(defmethod (simple-ascii-stream-terminal :write-char) (c)
  (global:stream-default-handler self :write-char c nil))

(defmethod (simple-ascii-stream-terminal :write-byte) (c)
  (global:stream-default-handler self :write-byte c nil))

(defmethod (simple-ascii-stream-terminal :line-out) (&optional arg1 &rest args)
  (global:stream-default-handler self :line-out arg1 args))

(defmethod (simple-ascii-stream-terminal :line-in) (&optional arg1 &rest args)
  (global:stream-default-handler self :line-in arg1 args))

(defmethod (simple-ascii-stream-terminal :string-in) (&optional arg1 &rest args)
  (global:stream-default-handler self :string-in arg1 args))

(defmethod (simple-ascii-stream-terminal :string-line-in) (&optional arg1 &rest args)
  (global:stream-default-handler self :string-line-in arg1 args))

;;;SUPDUP Server

(defconstant %tdeof #o202 "Erase to end of screen")
(defconstant %tdeol #o203 "Erase to end of line")
(defconstant %tddlf #o204 "Clear character")
(defconstant %tdcrl #o207 "Go to next line and clear or scroll")
(defconstant %tdnop #o210 "NOP")
(defconstant %tdbs  #o211 "Back space")
(defconstant %tdlf  #o212 "Line Feed")
(defconstant %tdcr  #o213 "Carriage Return")
(defconstant %tdors #o214 "Output reset")
(defconstant %tdqot #o215 "Quote following character")
(defconstant %tdfs  #o216 "Non-destructive forward space")
(defconstant %tdmv0 #o217 "Move to <y><x>")
(defconstant %tdclr #o220 "Clear screen and home cursor")
(defconstant %tdbel #o221 "Create audible tone")
(defconstant %tdilp #o223 "Insert <n> lines")
(defconstant %tddlp #o224 "Delete <n> lines")
(defconstant %tdicp #o225 "Insert <n> characters")
(defconstant %tddcp #o226 "Delete <n> characters")
(defconstant %tdbow #o227 "Black on White mode")
(defconstant %tdrst #o230 "Reset")
(defconstant %tdgrf #o231 "Graphics")

(define-termcap supdup "Supdup"
  :add-blank-line '(%tdilp 1)
  :clear-character %tddlf
  :clear-to-end-of-display %tdeof
  :clear-to-end-of-line %tdeol
  :clear-screen %tdclr
  :cursor-right %tdfs
  :cursor-motion '(%tdmv0 y x)
  :delete-character '(%tddcp 1)
  :delete-line '(%tddlp 1)
  :insert-character '(%tdicp 1)
  :multiple-character-insert '(%tdicp n)
  :multiple-character-delete '(%tddcp n)
  :multiple-line-insert '(%tdilp n)
  :multiple-line-delete '(%tddlp n)
  :fresh-line-and-clear %tdcrl
  :back-space %tdbs
  :carriage-return %tdcr
  :line-feed %tdlf
  :new-line '(%tdcr %tdlf)
  :audible-bell %tdbel
  :tab nil
  )

(defflavor supdup-server
         (who-string)
         (simple-ascii-stream-terminal)
  (:settable-instance-variables who-string)
  :initable-instance-variables)

(defvar *ascii-supdup-translations*
        '((#o00 #\Null)
          (#o10 #\Backspace)
          (#o11 #\Tab)
          (#o12 #\Line)
          (#o14 #\FF)
          (#o15 #\Return)
          (#o32 #\Call)
          (#o33 #\Altmode)
          (#o177 #\Rubout)
          (#o4101 #\Escape)
          (#o4102 #\Break)
          (#o4103 #\Clear-Input)
          (#o4110 #\Help)
          (#o4111 #\Abort)                      ;Non-standard
          (#o4112 #\Resume)
          (#o4113 #\Hold-Output)
          (#o4114 #\End)
          (#o4115 #\Status)
          (#o4177 #\Integral)))

(defmethod (supdup-server :subtyi) (&aux c bits code)
  (loop
    (setq c (if untyi-char
                (prog1 untyi-char (setq untyi-char nil))
              (send input :tyi)))
    (cond ((null c)
           (return nil))
          ((characterp c)
           (return c))
          ((= c #o300)
           (case (setq c (send input :tyi))
             (#o301                             ;Logout
              (network-user:logout))
             (#o302
              (setq who-string (make-string 64 :fill-pointer 0))
              (loop
                (setq c (send input :tyi))
                (when (or (null c) (zerop c))
                  (return))
                (vector-push-extend c who-string)))))
          ((= c #o34)
           (setq bits (send input :tyi))
           (cond ((null bits)
                  (return nil))
                 ((= bits #o34)
                  (return c)))
           (setq bits (logand bits #o37))
           (setq code (send input :tyi))
           (when (null code)
             (return nil))
           (setq c (dpb (logand bits #o20) (byte 5 7) (logand #o177 code)))
           (return (make-char (cond ((cadr (assoc c *ascii-supdup-translations* :test #'eq)))
                                    ((= bits #o20) code)
                                    (t (global:char-flipcase code)))
                              (logand bits #o17))))
          (t
           (return (or (cadr (assoc c *ascii-supdup-translations* :test #'eq)) c))))))

(defmethod (supdup-server :any-tyi) (&optional ignore &aux idx)
  (char-int-if-any
    (loop
      (cond ((> (tv:rhb-fill-pointer) (setq idx (tv:rhb-scan-pointer)))
             ;;untyi'd characters...
             (incf (tv:rhb-scan-pointer))
             (return (aref tv:rubout-handler-buffer idx)))
            ((not (eq tv:rubout-handler self))
             ;;rubout handling not in effect...
             (when need-force-output
               (send self :force-output))
             (unless (or untyi-char (send input :listen))
               (send self :notice :input-wait))
             (return (send self :subtyi)))
            (t
             ;;Rubout handler (will call us for new characters)
             (return (funcall (or tv:stream-rubout-handler 'tv:default-rubout-handler))))))))

(defmethod (supdup-server :tyo-unlocked) (c)
  (when (plusp (tv:sheet-more-flag self))
    (send self :more-exception))
  (when (= tv:cursor-y tv:height)
    (send self :home-cursor)
    (send self :clear-rest-of-line))
  (cond ((null c))
        ((= c #\Return)
         (send self :terpri))
        ((= c #\Tab)
         (send self :tab))
        ((= c #\Backspace)
         (when (plusp tv:cursor-x)
           (send self :output-control-sequence (termcap.back-space termcap))
           (decf tv:cursor-x tv:char-width)))
        ((graphic-char-p c)
         (cond ((= tv:cursor-x (- tv:width tv:char-width))
                (send self :end-of-line-exception c))
               (t
                (send output :tyo c)
                (incf tv:cursor-x tv:char-width)))
         (setq need-force-output t))
        ((and (zerop (char-bits c)) (> c #\Network))
         ;; otherwise there will be a recursive call to this :TYO from FORMAT below.
         (format self "<~O>" c))
        ('else
         (format self "~:C" c)))
  (and (zerop tv:cursor-x)
       (eql tv:cursor-y tv:more-vpos)
       (setf (tv:sheet-more-flag self) 1)))

(defmethod (supdup-server :string-length) (string start end x)
  (do ((stops (termcap.tab-stops termcap))
       (index start (1+ index))
       (count 0)
       c)
      ((eql index end) count)
    (setq c (char string index))
    (when (null c)
      (return count))
    (cond ((= c #\Return))                      ;hmm
          ((= c #\Tab)
           (incf count (cond ((null stops) 8)
                             ((numberp stops)
                              (let ((x (mod (+ x count) stops)))
                                (if (zerop x) stops x)))
                             (t 0))))
          ((= c #\Backspace)
           (decf count))
          ((graphic-char-p c)
           (incf count))
          ((and (zerop (char-bits c)) (> c #\Network))
           (incf count (cond ((< c #o10) 3)
                             ((< c #o100) 4)
                             (t 5))))
          ('else
           (incf count (string-length (format nil "~:C" c)))))))

(defmethod (supdup-server :compute-motion) (string &optional (start 0) (end (string-length string))
                                            (x tv:cursor-x) (y tv:cursor-y))
  (declare (values end-x end-y))
  (setq x (pixels-to-chars x))
  (setq y (pixels-to-lines y))
  ;;Returns where cursor will be after buffer has been output.
  (do* ((end-x x)
        (end-y y)
        (stops (termcap.tab-stops termcap))
        (lines (truncate tv:height tv:line-height))
        (columns (- (truncate tv:width tv:char-width) (if (termcap.linewrap-indicator termcap) 1 0)))
        (index start (1+ index))
        c)
       ((eql index end)
        (values (chars-to-pixels end-x) (lines-to-pixels end-y)))
    (labels ((inc-y (dy)
               (incf end-y dy)
               (when (> end-y (- lines 2))
                 (setq end-y (mod end-y lines))))
             (inc-x (dx)
               (incf end-x dx)
               (when (> end-x columns)
                 (setq end-x (mod end-x columns))
                 (inc-y 1))))
      (setq c (char string index))
      (when (null c)
        (return (values (chars-to-pixels end-x) (lines-to-pixels end-y))))
      (cond ((= c #\Return)
             (setq end-x 0)
             (inc-y 1))
            ((= c #\Tab)
             (inc-x (cond ((null stops) 8)
                          ((numberp stops)
                           (let ((x (mod end-x stops)))
                             (if (zerop x) stops x)))
                          (t 0))))
            ((= c #\Backspace)
             (inc-x -1))
            ((graphic-char-p c)
             (inc-x 1))
            ((and (zerop (char-bits c)) (> c #\Network))
             (inc-x (cond ((< c #o10) 3)
                          ((< c #o100) 4)
                          (t 5))))
            ('else
             (inc-x (string-length (format nil "~:C" c))))))))

(defmethod (supdup-server :clear-between-cursorposes) (x1 y1 x2 y2)
  (supdup-clear-between-cursorposes x1 y1 x2 y2))

(defmethod (supdup-server :supdup-greeting) (format-string &rest args)
  (start-supdup-output :supdup)
  (apply #'format self format-string args)
  (send output :tyo %tdnop)
  (send self :terpri)
  (force-output output))

(compile-flavor-methods supdup-server)

(defun start-supdup-output (type)
  (declare (:self-flavor simple-ascii-stream-terminal))
  (let ((nwords (dpb (get-18-bits input) (byte 18. 0) -1))
        (ttyopt-left 0)
        (ttyopt-right 0)
        (ttyrol 0))
    (when (or (< nwords -16.) (> nwords 0))
      (return-from start-supdup-output nil))
    (get-18-bits input)
    (when (not (zerop nwords))
      (get-18-bits input)
      (unless (= (get-18-bits input) 7)
        (return-from start-supdup-output nil))
      (incf nwords))
    (send self :termcap type)
    (setq termcap (copy-termcap termcap))
    (when (not (zerop nwords))
      (setq ttyopt-left (get-18-bits input))
      (setf (termcap.selective-erase termcap) (not (zerop (logand ttyopt-left #o40000))))
      (setf (termcap.overprint termcap) (not (zerop (logand ttyopt-left #o1000))))
      (setf (termcap.extended-keyboard termcap) (not (zerop (logand ttyopt-left #o10))))
      (setf (termcap.line-insert-delete termcap) (not (zerop (logand ttyopt-left #o2))))
      (setf (termcap.character-insert-delete termcap) (not (zerop (logand ttyopt-left #o1))))
      (send self :set-more-p (not (zerop (logand ttyopt-left #o200))))
      (setq ttyopt-right (get-18-bits input))
      (incf nwords))
    (when (not (zerop nwords))
      (get-18-bits input)
      (setq tv:height (get-18-bits input))      ;height in lines
      (setf (termcap.number-of-lines termcap) tv:height)
      (incf nwords))
    (when (not (zerop nwords))
      (get-18-bits input)
      (setq tv:width (get-18-bits input))       ;width in characters
      (setf (termcap.number-of-columns termcap) tv:width)
      (incf nwords))
    (when (not (zerop nwords))
      ;;scroll glitch
      (get-18-bits input)
      (setq ttyrol (get-18-bits input))
      (incf nwords))
    (when (not (zerop nwords))
      ;;TTYSMT
      (let ((x (get-18-bits input)))
        (setq tv:line-height (ldb (byte 8 10) x))
        (setq tv:height (* tv:height tv:line-height))
        (setq tv:char-width (ldb (byte 4 6) x))
        (setq tv:width (* tv:width tv:char-width)))
      (get-18-bits input)
      (incf nwords))
    (do ()
        ((zerop nwords))
      (get-18-bits input)
      (get-18-bits input)
      (incf nwords))
    ))

(defun get-18-bits (stream)
  (let* ((b2 (send stream :tyi))
         (b1 (send stream :tyi))
         (b0 (send stream :tyi)))
    (dpb b2
         (byte 6 12.)
         (dpb b1
              (byte 6 6)
              b0))))

(defun supdup-clear-between-cursorposes (x1 y1 x2 y2)
  (declare (:self-flavor simple-ascii-stream-terminal))
  (with-lock (output-lock)
    (cond ((= y1 y2)                            ;Same line
           (let ((delete (pixels-to-chars (- x2 x1))))
             (when (eq term :supdup-output)
               (supdup-sb output)
               (send output :tyo (+ (if (= tv:cursor-x x1) 0 3)
                                    (if (= delete 1) 1 4))))
             (unless (= tv:cursor-x x1)
               (send output :tyo %tdmv0)
               (send output :tyo (pixels-to-lines y1))
               (send output :tyo (pixels-to-chars x1)))
             (cond ((= delete 1)
                    (send output :tyo %tddlf))
                   (t
                    (send output :tyo %tddcp)
                    (send output :tyo delete)
                    (send output :tyo %tdicp)
                    (send output :tyo delete)))
             (when (eq term :supdup-output)
               (supdup-se output))))
          (t
           (let ((nlines (- (pixels-to-lines (+ y2 (if (> y2 y1) 0 tv:height)))
                            (pixels-to-lines y1)
                            1)))
             (when (eq term :supdup-output)
               (supdup-sb output)
               (send output :tyo (+ (if (plusp x2) 7 0)
                                    (* nlines 4)
                                    4)))
             (when (plusp x2)
               (send output :tyo %tdmv0)
               (send output :tyo (pixels-to-lines y2))
               (send output :tyo 0)
               (send output :tyo %tddcp)
               (send output :tyo (pixels-to-chars x2))
               (send output :tyo %tdicp)
               (send output :tyo (pixels-to-chars x2)))
             (dotimes (i nlines)
               (send output :tyo %tdmv0)
               (send output :tyo (mod (+ (pixels-to-lines y1) i 1) tv:height))
               (send output :tyo 0)
               (send output :tyo %tdeol))
             (send output :tyo %tdmv0)
             (send output :tyo (pixels-to-lines y1))
             (send output :tyo (pixels-to-chars x1))
             (send output :tyo %tdeol)
             (when (eq term :supdup-output)
               (supdup-se output)))))
    (setq tv:cursor-x x1)
    (setq tv:cursor-y y1)
    (setq need-force-output t)))

;;;Telnet server

(defflavor telnet-server
         ((telnet-options-received nil)
          (telnet-options-sent nil)
          (flush-next-lf nil))
         (simple-ascii-stream-terminal)
  (:settable-instance-variables flush-next-lf)
  :initable-instance-variables)


(defun supdup-sb (stream)
  (send stream :tyo iac)
  (send stream :tyo sb)
  (send stream :tyo telopt_supdup-output)
  (send stream :tyo 2))

(defun supdup-se (stream)
  (declare (:self-flavor telnet-server))
  (send stream :tyo (pixels-to-chars tv:cursor-x))
  (send stream :tyo (pixels-to-lines tv:cursor-y))
  (send stream :tyo iac)
  (send stream :tyo se))

(define-termcap supdup-output "Telnet Supdup Output"
  :add-blank-line '(supdup-sb 2 %tdilp 1 supdup-se)
  :clear-character '(supdup-sb 1 %tddlf supdup-se)
  :clear-to-end-of-display '(supdup-sb 1 %tdeof supdup-se)
  :clear-to-end-of-line '(supdup-sb 1 %tdeol supdup-se)
  :clear-screen '(supdup-sb 1 %tdclr supdup-se)
  :cursor-right '(supdup-sb 1 %tdfs supdup-se)
  :cursor-motion '(supdup-sb 3 %tdmv0 y x supdup-se)
  :delete-character '(supdup-sb 2 %tddcp 1 supdup-se)
  :delete-line '(supdup-sb 2 %tddlp 1 supdup-se)
  :insert-character '(supdup-sb 2 %tdicp 1 supdup-se)
  :multiple-character-insert '(supdup-sb 2 %tdicp n supdup-se)
  :multiple-character-delete '(supdup-sb 2 %tddcp n supdup-se)
  :multiple-line-insert '(supdup-sb 2 %tdilp n supdup-se)
  :multiple-line-delete '(supdup-sb 2 %tddlp n supdup-se)
  :fresh-line-and-clear '(supdup-sb 1 %tdcrl supdup-se)
  :cursor-motion-characters 12
  )

(defvar *special-ascii-lispm-translations*
        '((#o10 #\Bs)
          (#o11 #\Tab)
          (#o12 #\Line)
          (#o14 #\FF)
          (#o15 #\Return)
          (#o33 #\Altmode)
          (#o177 #\Rubout)))

(defmethod (telnet-server :send-initial-telnet-frobs) ()
  (send-option 'will 'telopt_supdup-output)
  (send-option 'will 'telopt_echo)
  (send-option 'will 'telopt_sga)
  (send-option 'do 'telopt_sga)
  (do (c)
      ((eq term :supdup-output)         ;Quit when we've set up our termcap
       (send self :clear-window))
    (setq c (when (process-wait-with-timeout "Telnet Options"
                                             60
                                             #'(lambda (x)
                                                 (listen x))
                                             input)
              (send input :tyi)))
    (cond ((null c)                             ;Timeout on TCP stream
           (return))
          ((= c iac)                            ;Telnet command
           (receive-iac)
           (when (eq (cdr (find-option telnet-options-received 'telopt_supdup-output '(do dont))) 'dont)
             (return)))                         ;He said not to do supdup-output
          (t
           (send self :untyi c)
           (return)))))

(defun send-option (command option)
  (declare (:self-flavor telnet-server))
  (let ((item (find-option telnet-options-sent option (case command
                                                        ((do dont) '(do dont))
                                                        ((will wont) '(will wont))))))
    (cond ((null item)
           (unless (member command '(dont wont))
             (push (cons option command) telnet-options-sent)
             (send-iac output command option)))
          ((eq (cdr item) command))
          (t
           (setf (cdr item) command)
           (send-iac output command option)))))

(defun receive-option (command option)
  (declare (:self-flavor telnet-server))
  (let ((item (find-option telnet-options-received option (case command
                                                             ((do dont) '(do dont))
                                                             ((will wont) '(will wont))))))
    (unless (eq (cdr item) command)
      (if (null item)
          (push (cons option command) telnet-options-received)
        (setf (cdr item) command))
      (case command
        (do
         (send-option 'will option))
        (dont
         (send-option 'wont option))
        (will
         (send-option 'do option))
        (wont
         (send-option 'dont option))))))

(defun find-option (list option commands)
  (loop
    (setq list (member option list :key #'car :test #'eq))
    (cond ((null list)
           (return nil))
          ((member (cdar list) commands)
           (return (car list)))
          (t
           (setq list (cdr list))))))

(defun send-iac (stream &rest commands)
  (send stream :tyo iac)
  (dolist (char commands)
    (send stream :tyo (if (symbolp char)
                          (symbol-value char)
                        char)))
  (send stream :force-output))

(defmethod (telnet-server :notice) (event &rest args)
  (declare (ignore args))
  (case event
    ((:input :output)           ;Deexposed window needs some attention
     t)
    (:input-wait                ;Hanging up waiting for input.
     (setf (tv:sheet-more-flag self) 0)
     (cond ((null tv:more-vpos))                ;Unless MORE inhibited entirely
           ((< (* (- tv:height tv:cursor-y) 4)  ;More than 3/4 way down window?
               tv:height)
            ;; Wrap around and more just before the current line
            (setq tv:more-vpos (- tv:cursor-y tv:line-height)))
           (t ;; More at bottom
            (setq tv:more-vpos (- tv:height tv:line-height))))
     (unless (find-option telnet-options-received 'telopt_sga '(do))
       ;;Unless remote side said Do Suppress Go-aheads, send a Go-ahead
       (send-iac output 'ga))
     t)
    (:error                     ;Error in process using this window as its *TERMINAL-IO*.
     t)
    (otherwise nil)))

(defun translate-char (c)
  (setq c (or (cadr (assoc c *special-ascii-lispm-translations* :test #'eq)) c))
  (unless (characterp c)
    (when (< c #o40)
      (setq c (set-char-bit (logior c #o100) :control t))))
  (int-char c))

(defmethod (telnet-server :subtyi) ()
  (if need-force-output (send self :force-output))
  (flet ((getc (tcp stream)
           (unless (send tcp :listen)
             (send stream :notice :input-wait))
           (send tcp :tyi)))
    (do ((c) (quote)
         (extended (termcap.extended-keyboard termcap)))
        ((not (setq c (or untyi-char (getc input self))))
         nil)
      (cond (untyi-char
             (return (prog1 untyi-char (setq untyi-char nil))))
            ((characterp c)
             (return c))
            ((= c iac)
             (multiple-value-bind (done value)
                 (receive-iac)
               (when done
                 (return value))))
            ((and (not quote) (= c *telnet-ascii-quote-character*))
             (setq quote t))
            ((and (not extended) (not quote) (= c (glass-tty-ascii-code #\Altmode)))
             (setq c (send self :subtyi))
             (return (set-char-bit (make-char (if (plusp (char-bits c)) c (global:char-flipcase (char-code c)))
                                              (char-bits c))
                                   :meta t)))
            ((and (not quote) (= c (glass-tty-ascii-code #\Control-\\)))
             (cond (extended
                    (let ((bits (send input :tyi)))
                      (when (= bits #o034)
                        (return c))
                      (setq c (send input :tyi))
                      (return (make-char (global:char-flipcase c) (logand bits #o77)))))
                   (t
                    (setq c (send self :subtyi))
                    (return (set-char-bit (make-char (if (plusp (char-bits c)) c (global:char-flipcase (char-code c)))
                                                     (char-bits c))
                                          :super t)))))
            ((= (setq c (translate-char c)) #\Return)
             (setq flush-next-lf t)
             (return #\Return))
            ((and (= c #\Line) flush-next-lf)
             (setq flush-next-lf nil))
            ('else
             (setq flush-next-lf nil)
             (when quote
               (setq untyi-char c)
               (setq c *telnet-quote-character*))
             (return c))))))

(defun receive-iac (&aux c action option)
  (declare (:self-flavor telnet-server))
  (setq flush-next-lf nil)
  (setq c (send input :tyi))
  (case (setq action (cadr (assoc c *telsyms* :test #'eq)))
    (iac
     (values t c))
    (ec
     (values t #\Rubout))
    (el
     (values t #\Clear-input))
    (do
     (setq c (send input :tyi))
     (setq option (cadr (assoc c *telopts* :test #'eq)))
     (case option
       ((telopt_echo telopt_sga telopt_supdup-output)
        (receive-option action option))
       (telopt_logout
        (receive-option action option)
        (return-from receive-iac (values t nil)))
       (t
        (send-option 'wont option)))
     nil)
    (dont
     (setq c (send input :tyi))
     (setq option (cadr (assoc c *telopts* :test #'eq)))
     (case option
       ((telopt_echo telopt_sga telopt_supdup-output)
        (receive-option action option)))
     nil)
    (will
     (setq c (send input :tyi))
     (setq option (cadr (assoc c *telopts* :test #'eq)))
     (case option
       (telopt_sga
        (receive-option action option))
       (t
        (send-option 'dont option)))
     nil)
    (wont
     (setq c (send input :tyi))
     (setq option (cadr (assoc c *telopts* :test #'eq)))
     (case option
       (telopt_sga
        (receive-option action option)))
     nil)
    (sb
     (handle-subnegotiation)
     nil)
    (t
     nil)))

(defun handle-subnegotiation ()
  (declare (:self-flavor telnet-server))
  (let* ((c (send input :tyi))
         (option (cadr (assoc c *telopts* :test #'eq))))
    (case option
      (telopt_supdup-output
       (when (= (send input :tyi) 1)
         (start-supdup-output :supdup-output))))
    ;;Here to skip to end of subnegotiation
    (do ((iac-seen nil))
        ((and iac-seen (= (send input :tyi) se)))
      (setq iac-seen (= (send input :tyi) iac)))))


(defmethod (telnet-server :any-tyi) (&optional ignore)
  (let ((extended (termcap.extended-keyboard termcap))
        idx c quote)
    (char-int-if-any
      (loop
        (cond ((> (tv:rhb-fill-pointer) (setq idx (tv:rhb-scan-pointer)))
               (incf (tv:rhb-scan-pointer))
               (setq c (aref tv:rubout-handler-buffer idx))
               (cond ((and (not quote) (= c *telnet-quote-character*))
                      (setq quote t))
                     (extended
                      (return c))
                     (t
                      (return c))))
              ((not (eq tv:rubout-handler self))
               (setq c (send self :subtyi))
               (cond ((null c)
                      (return nil))
                     ((and (not quote) (= c *telnet-quote-character*))
                      (setq quote t))
                     (extended
                      (return c))
                     (quote
                      (return c))
                     ((eq c #\Control-U)
                      (return #\Clear-Input))
                     ((eq c #\Control-R)
                      (return #\Delete))
                     (t
                      (return c))))
              (t
               (return (funcall (or tv:stream-rubout-handler 'tv:default-rubout-handler)))))))))

(defvar *telnet-graphic-translations* #("." "v" "a" "b" "^" "~" "e" "p"
                                        "l" "g" "d" "^" "+-" "+" "infty" "d"
                                        "<" ">" "^" "v" "A" "E" "x" "<->"
                                        "<-" "->" "//=" "$" "<=" "=>" "=" "v")
  "Array of ASCII translations for Lisp Machine characters")

(defmethod (telnet-server :tyo-unlocked) (c)
  (when (plusp (tv:sheet-more-flag self))
    (send self :more-exception))
  (when (= tv:cursor-y tv:height)
    (send self :home-cursor)
    (send self :clear-rest-of-line))
  (cond ((null c))
        ((and (plusp c) (< c #o40))
         (send self :string-out (aref *telnet-graphic-translations* c)))
        ((= c #\Return)
         (send self :terpri))
        ((= c #\Tab)
         (send self :tab))
        ((= c #\Backspace)
         (when (plusp tv:cursor-x)
           (send self :output-control-sequence (termcap.back-space termcap))
           (decf tv:cursor-x tv:char-width)))
        ((graphic-char-p c)
         (cond ((= tv:cursor-x (- tv:width tv:char-width))
                (send self :end-of-line-exception c))
               (t
                (send output :tyo c)
                (incf tv:cursor-x tv:char-width)))
         (setq need-force-output t))
        ((and (zerop (char-bits c)) (> c #\Network))
         ;; otherwise there will be a recursive call to this :TYO from FORMAT below.
         (format self "<~O>" c))
        ('else
         (format self "~:C" c)))
  (and (zerop tv:cursor-x)
       (eql tv:cursor-y tv:more-vpos)
       (setf (tv:sheet-more-flag self) 1)))

(defmethod (telnet-server :string-length) (string start end x)
  (do ((stops (termcap.tab-stops termcap))
       (index start (1+ index))
       (count 0)
       c)
      ((eql index end)  count)
    (setq c (char string index))
    (when (null c)
      (return count))
    (cond ((< c #o40)
           (incf count (string-length (aref *telnet-graphic-translations* c))))
          ((= c #\Return))                      ;hmm
          ((= c #\Tab)
           (incf count (cond ((null stops) 8)
                        ((numberp stops)
                         (let ((x (mod (+ x count) stops)))
                           (if (zerop x) stops x)))
                        (t 0))))
          ((= c #\Backspace)
           (decf count))
          ((graphic-char-p c)
           (incf count))
          ((and (zerop (char-bits c)) (> c #\Network))
           (incf count (cond ((< c #o10) 3)
                        ((< c #o100) 4)
                        (t 5))))
          ('else
           (incf count (string-length (format nil "~:C" c)))))))

(defmethod (telnet-server :compute-motion) (string &optional (start 0) (end (string-length string))
                                                           (x tv:cursor-x) (y tv:cursor-y))
  (declare (values end-x end-y))
  (setq x (pixels-to-chars x))
  (setq y (pixels-to-lines y))
  ;;Returns where cursor will be after buffer has been output.
  (do* ((end-x x)
        (end-y y)
        (stops (termcap.tab-stops termcap))
        (lines (truncate tv:height tv:line-height))
        (columns (- (truncate tv:width tv:char-width) (if (termcap.linewrap-indicator termcap) 1 0)))
        (index start (1+ index))
        c)
       ((eql index end)
        (values (chars-to-pixels end-x) (lines-to-pixels end-y)))
    (labels ((inc-y (dy)
               (incf end-y dy)
               (when (> end-y (- lines 2))
                 (setq end-y (mod end-y lines))))
             (inc-x (dx)
               (incf end-x dx)
               (when (> end-x columns)
                 (setq end-x (mod end-x columns))
                 (inc-y 1))))
      (setq c (char string index))
      (when (null c)
        (return (values (chars-to-pixels end-x) (lines-to-pixels end-y))))
      (cond ((< c #o40)
             (inc-x (string-length (aref *telnet-graphic-translations* c))))
            ((= c #\Return)
             (setq end-x 0)
             (inc-y 1))
            ((= c #\Tab)
             (inc-x (cond ((null stops) 8)
                          ((numberp stops)
                           (let ((x (mod end-x stops)))
                             (if (zerop x) stops x)))
                          (t 0))))
            ((= c #\Backspace)
             (inc-x -1))
            ((graphic-char-p c)
             (inc-x 1))
            ((and (zerop (char-bits c)) (> c #\Network))
             (inc-x (cond ((< c #o10) 3)
                          ((< c #o100) 4)
                          (t 5))))
            ('else
             (inc-x (string-length (format nil "~:C" c))))))))

(defmethod (telnet-server :clear-between-cursorposes) (x1 y1 x2 y2)
  (if (eq term :supdup-output)
      (supdup-clear-between-cursorposes x1 y1 x2 y2)
    (normal-clear-between-cursorposes x1 y1 x2 y2)))

(compile-flavor-methods telnet-server)

;;;Utility functions

(defun get-raw-ascii-stream (&optional (s *terminal-io*))
  (check-type s simple-ascii-stream-terminal)
  (make-duplexing-stream (global:symeval-in-instance s 'input)
                         (global:symeval-in-instance s 'output)))


(defun make-duplexing-stream (input output &aux stream)
  (setq stream #'(lambda (op &optional arg1 &rest args)
                   (si:selectq-with-which-operations op
                     (:tyi (send input :tyi))
                     (:tyi-no-hang (send input :tyi-no-hang))
                     (:listen (send input :listen))
                     (:tyo (send output :tyo arg1))
                     (:string-out (global:lexpr-send output :string-out arg1 args))
                     (t
                       (global:stream-default-handler stream op arg1 args))))))
