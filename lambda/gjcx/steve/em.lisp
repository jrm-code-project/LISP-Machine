; -*- Mode:LISP; Package:STEVE; Readtable:CL; Base:10 -*-

;Copyright (c) May 1983 by Christopher Eliot
; and Massachusetts Institute of Technology.
;Permission to copy all or part of this material is granted, provided
; that the copies are not made or distributed for resale, the MIT
; copyright notice and reference to the source file and the software
; distribution version appear, and that notice is given that copying
; is by permission of Massachusetts Institute of Technology.

;Editor.
;
;*********
;

(defconstant line-not-modified 0)
(defconstant this-line-modified-bit 1)
(defconstant next-line-modified-bit 2)

;
;These "old and in the way" inconsistent definitions might have caused
;quite a chaotic situation. I hope nobody used them.
;YOW what a loss!
;(defconstant buffer-any-access 0)
;(defconstant buffer-read-only 1)
;(defconstant file-read-only 2)
;
(defconstant buffer-access-any 0)
(defconstant buffer-access-file-read-only 1)
(defconstant buffer-access-buffer-read-only 2)


(defvar *editor-redisplay-count*)

(defflavor buffer (name content modified? file-name
                   access environment mark-ring mark-ring-index truename
                   narrow-info)
  ()
  :ordered-instance-variables
  :initable-instance-variables
  ;;:gettable-instance-variables
  :outside-accessible-instance-variables)

(defmethod (buffer :print-self) (&optional (stream standard-output) ignore ignore)
  (format stream "#<~s ~a>" 'buffer name))

(defmethod (buffer :not-last-version?) ()
  (if (null file-name)
      nil
      (let ((true (probe-file file-name)))
        (if (null true)
            (not (null truename))
            (not (send true :equal truename))))))

(defmethod (buffer :set-file-name) (pathname)
 (when (not (of-type pathname 'pathname))
   (ed-lose "Set buffer file-name to non-pathname"))
 (setq file-name pathname)
 (setq truename (probe-file pathname))
 t)

(defflavor line (buffer buffer-pointers chars char-count previous next
                 bashed? graphic-length parens string-bits) ()
  :ordered-instance-variables
  :initable-instance-variables
  ;;:gettable-instance-variables
  :outside-accessible-instance-variables)

(defmacro bash-line (&optional (line nil))
  (cond ((null line) ;In a LINE defmethod only!
         `(progn (setq bashed? *editor-redisplay-count*)
                 (setf (buffer-modified? buffer) t)))
        (t `(setf (line-bashed? ,line) *editor-redisplay-count*
                  (buffer-modified? (line-buffer ,line)) t))))

(defmacro line-not-bashed? (line)
  `(<& (line-bashed? ,line) *editor-redisplay-count*))

(defsubst graphic-length (x)
  (if (line-not-bashed? x)
      (ed-lose "Internal Error In Graphic Length")
      (line-graphic-length x)))

(defflavor bp (buffer line position) ()
  :ordered-instance-variables
  :initable-instance-variables
  ;;:gettable-instance-variables
  :outside-accessible-instance-variables)

(defmacro preserving-point-on-error (&body corpus)
  (let ((home (gentemp))
        (lost-big-p (gentemp)))
    `(let ((,lost-big-p t))
       (with-bp (,home *editor-cursor*)
          (unwind-protect
             (prog1 (progn ,@corpus)
                    (setq ,lost-big-p nil))
           (when ,lost-big-p
             (move-bp *editor-cursor* ,home)))))))

(defun buffer-default-pathname ()
  (merge-pathname-defaults (user-workingdir-pathname) "foo.lsp"))

(defun make-line (buffer prev next &optional
                  (string (make-string 10 :initial-element #\space) str?))
  (let ((line (make-instance 'line
                             :buffer buffer
                             :buffer-pointers nil
                             :chars string
                             :char-count (if str? (string-length string) 0)
                             :previous prev
                             :next next
                             ;;:bashed? this-line-modified-bit
                             :graphic-length *:min-fixnum
                             :parens nil
                             :string-bits :reserved-for-future-use)))
    (bash-line line)
    (if prev (setf (line-next prev) line))
    (if next (setf (line-previous next) line))
    line))

(defun make-bp (buffer line pos)
  (let ((new-bp (make-instance 'bp :buffer buffer :line line :position pos)))
    (send line :add-bp new-bp)
    new-bp))

(defun replace-bp (bp1 bp2)
 (send (bp-line bp2) :remove-bp bp2)
 (setf (bp-buffer bp2) (bp-buffer bp1)
       (bp-line bp2) (bp-line bp1)
       (bp-position bp2) (bp-position bp1))
 (send (bp-line bp2) :add-bp bp2))

(defun copy-bp (bp &optional (drift? nil))
 (make-bp (bp-buffer bp) (bp-line bp) (bp-position bp)))

(defun bp-equal (bp1 bp2)
  (and (eq (bp-line bp1) (bp-line bp2))
       (=& (bp-position bp1) (bp-position bp2))))

;(defun insert-char (buffer line pos char)
;  (send line :insert-char pos char))

(defun insert-string (buffer line pos string)
 (send line :insert-string pos string))

(defun delete-char (buffer line pos)
  (send line :delete-char pos))

;(defun break-line (buffer line pos)
;  (send line :break pos))

;(defun get-char (buffer line pos)
;  (send line :get-char pos))

;(defun move-bp (buffer bp line pos)
;  (send bp :move line pos))



;
;Methods needed.
;

;This returns something which stroes the current BP position.
;It is invalid if anything is done to the buffer.
(defmethod (bp :get-mark) ()
  (list buffer line position))

(defmethod (bp :goto-mark) (mark)
  (if (equal buffer (car mark))
      (send self :move (cadr mark) (caddr mark))
      (ed-lose "Bp Mark For Wrong Buffer")))

;Special cases: beggining of line, end of line, empty line, full line.
(defmethod (line :insert-char) (pos char)
  (when (not (zerop& (logand& (buffer-access buffer)
                          buffer-access-buffer-read-only)))
    (ed-warn :read-only))
  (when (and (not (string-charp char))
             (null (setq char (coerse-to-string-charp char))))
    (ed-lose "Char Not String Charp"))
  (cond ((char= char #\return) (send self :break pos))
        (t (when (>& pos char-count) (ed-lose "Insert Past End Of Line"))
           (when (>=& char-count (string-length chars))
             (send self :grow-line 10))
           (without-interrupts
            (bash-line)
            (%string-replace chars chars (1+& pos) pos (-& char-count pos))
            (setf (char chars pos) (character char)) ;What else might it be???
            (setq char-count (1+& char-count))
            (loop for bp in buffer-pointers
                  if (>=& (bp-position bp) pos)
                  do (send bp :advance-pos 1))))))

(defmethod (line :insert-string) (pos string
                                      &optional (skip 0)
                                      (take (string-length string)))
  (unless (zerop& take)
    (when (not (zerop& (logand& (buffer-access buffer)
                            buffer-access-buffer-read-only)))
      (ed-warn :read-only))
    (let ((break-pos (%string-posq #\return string skip take)))
      (cond ((not (null break-pos))
             (send self :insert-primative-string
                   pos string skip (-& break-pos skip))
             (send self :break (+& pos (-& break-pos skip)))
             (cond ((>=& (1+& break-pos) (+& skip take)) nil)
                   ((char= (char string (+& break-pos 1)) #\line)
                    (send next :insert-string 0 string (+& break-pos 2)
                          (-& take (-& break-pos skip) 2)))
                   (t (send next :insert-string 0 string (+& break-pos 1)
                            (-& take (-& break-pos skip) 1)))))
            (t (send self :insert-primative-string pos string skip take))))))

(defmethod (line :insert-primative-string) (pos string
                                                &optional (skip 0)
                                                (take (string-length string)))
 (when (not (zerop& (logand& (buffer-access buffer)
                         buffer-access-buffer-read-only)))
  (ed-warn :read-only))
 (when (>& pos char-count) (ed-lose "Insert Past End Of Line"))
 (without-interrupts
  (let ((new-string (make-string (+& char-count take) :initial-element #\space)))
   (%string-replace new-string chars 0 0 pos) ;Copy first part.
   (%string-replace new-string chars (+& pos take) pos (-& char-count pos))
   ;Copy end before clobbering it with the new-middle.
   (%string-replace new-string string pos skip take) ;middle part.
   (setq chars new-string)
   (setq char-count (+& char-count take))
   (bash-line)
   (loop for bp in buffer-pointers
         if (>=& (bp-position bp) pos)
         do (send bp :advance-pos take)))))

(defmethod (bp :insert-char) (char &optional (repeat 1))
  (loop repeat repeat
        do (send line :insert-char position char)))

(defmethod (bp :insert-string) (string)
 (send line :insert-string position string))

(defmethod (line :replace-char) (pos char)
 (when (not (zerop& (logand& (buffer-access buffer)
                         buffer-access-buffer-read-only)))
  (ed-warn :read-only))
 (cond ((minusp& pos) (ed-lose "Replace Char Before Line"))
       ((<& pos char-count)
        (bash-line)
        (setf (char chars pos) char)
        (loop for bp in buffer-pointers
                  if (=& (bp-position bp) pos)
                  do (send bp :advance-pos 1)))
       (t (send self :delete-char pos)
          (send self :insert-char pos char))))

;Remember about beginning/end of line.
(defmethod (bp :advance-pos) (count)
  (let ((new-position (+& position count)))
    (cond ((and (not (minusp& new-position))
                (<=& new-position (line-char-count line)))
           (setq position new-position))
          ((minusp& new-position)
           (send self :to-end-of-previous-line)
           (send self :advance-pos (1+& new-position)))
          (t (setq new-position (-& new-position 1 (line-char-count line)))
             (when (null (line-next line)) (ed-warn :move-past-end))
             (send self :move (line-next line) 0)
             (send self :advance-pos new-position)))))

(defmethod (bp :advance-pos-no-error) (count)
  ;;Return count unused or NIL to signify success.
  (let ((new-position (+& position count)))
    (cond ((and (not (minusp& new-position))
                (<=& new-position (line-char-count line)))
           (setq position new-position)
           ;;Return NIL to signify no errors.
           nil)
          ((minusp& new-position)
           (cond ((null (line-previous line))
                  (setq position 0)
                  ;;Error value.
                  new-position)
                 (t (send self :to-end-of-previous-line)
                    (send self :advance-pos-no-error (1+& new-position)))))
          (t (setq new-position (-& new-position 1 (line-char-count line)))
             (cond ((null (line-next line))
                    (setq position (line-char-count line))
                    ;;Error value takes into account no <CR> at buffer end.
                    (1+& new-position))
                   (t (send self :move (line-next line) 0)
                      (send self :advance-pos-no-error new-position)))))))

;What if previous line is empty?
(defmethod (bp :to-end-of-previous-line) ()
  (if (null (line-previous line))
      (ed-warn :move-past-start)
      (send self :move (line-previous line)
            (line-char-count (line-previous line)))))

;What if previous line is empty?
(defmethod (bp :backward-over-crlf) ()
  (if (null (line-previous line))
      nil
      (send self :move (line-previous line)
            (line-char-count (line-previous line)))))

(defmethod (bp :to-beginning-of-next-line) ()
  (if (null (line-next line))
      (ed-warn :move-past-end)
      (send self :move (line-next line) 0)))

(defmethod (bp :forward-over-crlf) ()
  (if (null (line-next line))
      nil
      (send self :move (line-next line) 0)))

(defmethod (line :grow-line) (count)
  (let ((oldlen (simple-string-length chars)))
    (let ((new (make-string (+& count oldlen) :initial-element #\space)))
      (setq chars (%string-replace new chars 0 0 oldlen)))))

(defmethod (bp :delete-char) ()
  (send line :delete-char position))

;What about end of lines?
(defmethod (line :delete-char) (pos)
 (when (not (zerop& (logand& (buffer-access buffer)
                         buffer-access-buffer-read-only)))
  (ed-warn :read-only))
  (if (>& pos char-count) (ed-lose "Delete Past End Of Line"))
  (if (=& pos char-count)
      (if (null next)
          (ed-lose "Delete End Of Buffer")
          (send next :delete-line-separator))
      (without-interrupts
       (bash-line)
       (%string-replace chars chars pos (1+& pos) (-& char-count pos 1))
       (setq char-count (1-& char-count))
       (loop for bp in buffer-pointers
             if (>& (bp-position bp) pos)
             do (send bp :advance-pos -1)))))

(defmethod (bp :set-pos) (pos)
  (if (and (not (minusp& pos)) (<=& pos (line-char-count line)))
      (setq position pos)
      (send self :advance-pos (-& pos position))))

(defmethod (line :delete-line-separator) ()
 (when (not (zerop& (logand& (buffer-access buffer)
                         buffer-access-buffer-read-only)))
  (ed-warn :read-only))
  (if (null previous) (ed-warn :move-past-start))
  (without-interrupts
   (let* ((prev-linelen (line-char-count previous))
          (new-line-length (+& prev-linelen char-count))
          (prev-line (line-chars previous)))
     (when (<& (string-length prev-line) new-line-length)
      (send previous :grow-line (-& new-line-length (string-length prev-line)))
      (setq prev-line (line-chars previous))) ;Do it again, Sam.
     (%string-replace prev-line chars prev-linelen 0 char-count)
     (bash-line)
     (bash-line previous) ;Note that this is ineffiecient.
     (setf (line-char-count previous) (+& prev-linelen char-count)
           (line-next previous) next)
     (if next (setf (line-previous next) previous))
     (loop for bp in buffer-pointers
           do (send bp :move previous (+& (bp-position bp) prev-linelen))))))

(defmethod (line :set-next) (line)
 (when (not (zerop& (logand& (buffer-access buffer)
                         buffer-access-buffer-read-only)))
  (ed-warn :read-only))
  (when (eq line self)
        (ed-lose "Internal Error Attempt To Create Circularity In Buffer"))
  (do ((dead next (line-next dead))
       (n-chars char-count))
      ((or (null dead) (eq dead line)))
    (mapcar #'(lambda (old-bp) (send old-bp :move self n-chars))
            (line-buffer-pointers dead)))
  (setq next line)
  (unless (null line) (setf (line-previous line) self)))

(defmethod (line :get-char) (pos)
  (if (or (minusp& pos) (>=& pos char-count))
      (if (minusp& pos) (ed-lose "Get Char With Negative Position")
          (ed-lose "Get Char After Line")))
  (char chars pos))

(defmethod (bp :get-char) ()
  (if (>=& position (line-char-count line))
      (if (>& position (line-char-count line))
          (ed-lose "Get Char After Line")
          #\return)
      (char (line-chars line) position)))

(defmethod (bp :peek-char-backward) ()
 (send self :advance-pos -1)
 (send self :get-char-forward))

(defmethod (bp :get-char-forward) ()
  (cond ((>=& position (line-char-count line))
         (send self :to-beginning-of-next-line)
         #\return)
        (t (prog1 (schar (line-chars line) position)
                  (setq position (1+& position))))))

(defmethod (bp :get-char-backward) ()
  (cond ((plusp& position)
         (setq position (1-& position))
         (schar (line-chars line) position))
        (t (send self :to-end-of-previous-line)
           #\return)))

(defmacro bp-char-slashified? (bp)
  `(let ((start-index (1-& (bp-position ,bp))))
     (unless (zerop& start-index)
       (let ((line-chars (line-chars (bp-line ,bp))))
         (oddp (loop for index from start-index downto 0
                     while (of-syntax (schar line-chars index) character-quote)
                       count :each-character-quote))))))

(defmethod (bp :slashified?) ()
  (bp-char-slashified? self))

(defmethod (bp :move) (new-line new-pos)
  (if (and (eq new-line line)
           (not (minusp& new-pos))
           (<=& new-pos (line-char-count new-line)))
      (setq position new-pos)
      (progn (send line :remove-bp self)
             (setq line new-line
                   position new-pos)
             (send new-line :add-bp self))))

(defmethod (bp :expire) ()
  (send line :remove-bp self)
  (setq buffer "Expired BP")
  (setq line nil)
  (setq position nil))

(defmethod (bp :move-adjusting-pos) (new-line new-pos)
  (if (null new-line) (ed-lose "Move To Non-Line"))
  (send line :remove-bp self)
  (setq line new-line)
  (if (>& new-pos (line-char-count new-line))
      (setq new-pos (line-char-count new-line)))
  (setq position new-pos)
  (send new-line :add-bp self))

(defmethod (line :remove-bp) (bp)
  (setq buffer-pointers (delq bp buffer-pointers)))

(defmethod (line :add-bp) (bp)
  (if (minusp& (bp-position bp))
      (ed-lose "Bp Added With Negative Position"))
  (if (>& (bp-position bp) char-count)
      (ed-lose "Bp Added To Line Is Out Of Range"))
  (push bp buffer-pointers))

(defmethod (line :break) (pos)
  (when (not (zerop& (logand& (buffer-access buffer)
                          buffer-access-buffer-read-only)))
    (ed-warn :read-only))
  (let ((new-line (make-line buffer self next)))
    (bash-line)
    (loop for i upfrom 0
          for j from pos below char-count
          do (send new-line :insert-char i (schar chars j)))
    (loop for bp in buffer-pointers
          if (>=& (bp-position bp) pos) ;This has been changed!!!
          do (send bp :move new-line (-& (bp-position bp) pos ; 1
                                         )))
    (setq char-count pos)))

(defmethod (bp :beginning-of-line?) ()
  (zerop& position))

(defmethod (bp :end-of-line?) ()
  (=& position (line-char-count line)))

(defmethod (bp :end-of-buffer?) ()
  (null (line-next line)))

(defmethod (bp :beginning-of-buffer?) ()
  (null (line-previous line)))

;(defmethod (bp :delete-chars) () ;This is being phased out. don;t use it.
;  (send line :delete-chars-from-position position))

(defmethod (bp :delete-chars-from-position) ()
  (send line :delete-chars-from-position position))

(defmethod (bp :delete-characters) (n)
  ;;COding this so it is faster would speed up KILLING.
  (if (plusp& n)
      (do ((i 1 (1+& i)))
          ((>& i n) t)
        (send self :delete-char))
      (progn (send self :advance-pos n)
             (setq n (-& n))
             (do ((i 1 (1+& i)))
                 ((>& i n) t)
               (send self :delete-char)))))

;(defmethod (bp :delete-characters) (n)
;  ;;This won't work for negative N because line <= :delete-characters won't.
;  (send line :delete-characters position n))

(defmethod (line :delete-characters) (pos n)
  ;;Code this for negative N. (Not tested.)
  (unless (zerop& n)
    (when (not (zerop& (logand& (buffer-access buffer)
                            buffer-access-buffer-read-only)))
      (ed-warn :read-only))
    (unless (<=& pos char-count)
      (ed-lose "Reference Past End Of Line"))
    (cond ((>& n (-& char-count pos))
           (when (null next) (ed-lose "Delete End Of Buffer"))
           (send next :delete-line-separator)
           (send self :delete-characters pos (1-& n)))
          ;;Check for (and (minusp&  n) (<& pos (-& n))) here.
          ;;This clause is for negative N and new (i.e. partially tested.)
          ;;Monday the twenty-fifth of July, 1983; 6:15:45 pm
          ((minusp& (+& pos n))
           (when (null previous) (ed-lose "Delete beginning of buffer"))
           (send previous :delete-characters (line-char-count previous)
                 (+& n pos))
           (send self :delete-characters 0 pos))
          (t (%string-replace chars chars pos (+& pos n) (-& char-count pos n))
             (setq char-count (-& char-count n))
             (bash-line)
             (loop for bp in buffer-pointers
                   for bppos = (bp-position bp)
                   if (>& bppos pos)
                   do (send bp :set-pos (max& pos (-& bppos n))))))))

(defmethod (line :delete-chars-from-position) (pos)
  (when (not (zerop& (logand& (buffer-access buffer)
                          buffer-access-buffer-read-only)))
    (ed-warn :read-only))
  (unless (<=& char-count pos)
    (bash-line)
    (loop for bp in buffer-pointers
          if (>& (bp-position bp) pos)
          do (send bp :set-pos pos)) ;Put BPs on the end of the line.
                                     (setq char-count pos)))

(defmethod (line :match-string) (string pos)
  (and (<=& (string-length string)
            (-& char-count pos))
       (loop for i from 0 below (string-length string)
             for p from pos below char-count
             always (char-equal (char string i) (schar chars p)))))

;Create a new line following this one.
(defmethod (line :insert-line) (string)
  (when (not (zerop& (logand& (buffer-access buffer)
                          buffer-access-buffer-read-only)))
    (ed-warn :read-only))
  (bash-line)
  (setq next (make-line buffer self next string)))

(defmethod (line :translate) (function &optional (start 0) (count char-count))
  (bash-line)
  (loop for i from start below (min& count char-count)
        do (setf (schar chars i) (funcall function (schar chars i)))))


;
;More internal cruft.
;


;Buffer streams.
;These are used to read from a buffer directly into NIL, or vice versa.
;

(defflavor forward-buffer-stream (buffer line position)
  (bp input-stream output-stream)
  :ordered-instance-variables
  :initable-instance-variables
  :outside-accessible-instance-variables)

(defun make-buffer-stream (bp-start &aux (strm))
  (setq strm (make-instance 'forward-buffer-stream
                            :buffer (bp-buffer bp-start)
                            :line (bp-line bp-start)
                            :position (bp-position bp-start)))
  (send (bp-line strm) :add-bp strm)
  strm)

;Now for buffer streams.
(defmethod (forward-buffer-stream :tyi) ()
  (prog1 (char-code (send self :get-char))
         (send self :advance-pos 1)))

(defmethod (forward-buffer-stream :read-char) ()
  (prog1 (send self :get-char)
         (send self :advance-pos 1)))

(defmethod (forward-buffer-stream :peek-char) ()
  (send self :get-char))

(defmethod (forward-buffer-stream :untyi) (&optional char)
  (send self :advance-pos -1))

(defmethod (forward-buffer-stream :unread-char) (&optional char)
  (send self :advance-pos -1))

(defmethod (forward-buffer-stream :unread-n-chars) (n)
  (send self :advance-pos (-& n)))

;What is the eof-value for?
(defmethod (forward-buffer-stream :readline) (&optional eof-value)
  (let ((v1 (string-subseq (line-chars line) 0 (line-char-count line)))
        (v2 (null (line-next line))))
    (when (not v2)
      (send self :to-beginning-of-next-line))
    (values v1 v2)))

;These are used to implement lisp-listeners.

(defmethod (forward-buffer-stream :write-char) (char)
  (case char
    (#\return (send line :break position))
    (#\line nil)
    (t (send line :insert-char position char))))

(defmethod (forward-buffer-stream :terpri) ()
  (send line :break position))


;
;region functions.
;
;A region is defined by two BPs.
;We will want to be able to kill a region, move it, load it etc.
;
;This returns its arguments as two values, and guarantees that the
;first value is not after the second value.
(defun order-bps (bp-1 bp-2 &aux (bp-line-1 (bp-line bp-1))
                                 (bp-line-2 (bp-line bp-2)))
  (when (or (null bp-1) (null bp-2))
    (ed-lose "No mark. (Null buffer pointer.)"))
  (if (eq bp-line-1 bp-line-2)
      (if (<& (bp-position bp-1) (bp-position bp-2))
          (values bp-1 bp-2)
          (values bp-2 bp-1))
      (loop with bp-line-1 = (bp-line bp-1)
            with bp-line-2 = (bp-line bp-2)
            for line-1 first bp-line-1 then (line-next line-1)
            for line-2 first bp-line-2 then (line-next line-2)
            if (null line-1) return (values bp-2 bp-1)
            if (null line-2) return (values bp-1 bp-2)
            if (eq bp-line-1 line-2) return (values bp-2 bp-1)
            if (eq bp-line-2 line-1) return (values bp-1 bp-2))))

(defun order-lines (line-1 line-2)
  (if (eq line-1 line-2)
      (values line-1 line-2)
      (loop for line1 first line-1 then (line-next line1)
            for line2 first line-2 then (line-next line2)
            if (null line1) return (values line-2 line-1)
            if (null line2) return (values line-1 line-2)
            if (eq line1 line-2) return (values line-1 line-2)
            if (eq line2 line-1) return (values line-2 line-1))))

(defmethod (bp :forward-over-word) ()
  (let ((chrs (line-chars line))
        (max (line-char-count line)))
    (when (and (plusp& position)
               (=& 1 (get-char-syntax-bit syntax-table
                                          (schar chrs (1-& position))
                                          character-quote)))
      (setq position (1+& position)))
    (loop for i from position below max
          for chr = (schar chrs i)
          while (word-char? chr)
          if (=& 1 (get-char-syntax-bit syntax-table chr character-quote))
          do (setq i (1+& i))
          finally (setq position i))
    self))

(defmethod (bp :forward-over-not-word) ()
  (loop for chrs = (line-chars line)
        for max = (line-char-count line)
        until (loop for i from position below max
                    for chr = (schar chrs i)
                    if (word-char? chr)
                    do (progn (setq position i) (return t))
                    if (=& 1 (get-char-syntax-bit syntax-table chr
                                                  character-quote))
                    do (progn (setq position (1+& i)) (return t)))
        if (null (line-next line))
        do (progn (setq position max)
                  (return nil))
        do (send self :to-beginning-of-next-line))
  self)

(defmethod (bp :backward-over-word) ()
  (let ((chrs (line-chars line)))
    (do ((i (1-& position) (1-& i)))
        ((or (minusp& i)
             (and (not (word-char? (schar chrs i)))
                  (or (zerop& i)
                      (zerop& (get-char-syntax-bit syntax-table
                                               (schar chrs (1-& i))
                                               character-quote)))))
         (setq position (1+& i)))))
  self)

(defmethod (bp :backward-over-not-word) ()
  (loop for chrs = (line-chars line)
        until (do ((i (1-& position) (1-& i)))
                  ((minusp& i) nil)
                  (when (or (word-char? (schar chrs i))
                            (and (plusp& i)
                                 (=& 1 (get-char-syntax-bit
                                         syntax-table (schar chrs (1-& i))
                                         character-quote))))
                    (setq position i)
                    (send self :advance-pos 1)
                    (return t)))
        if (null (line-previous line))
        do (progn (setq position 0) (return nil))
        do (send self :to-end-of-previous-line))
  self)

(defmethod (bp :forward-over-string) ()
  (let ((delimiter (send self :get-char)))
    (when (and (of-syntax delimiter string-quote)
               (not (bp-char-slashified? self)))
      (send self :advance-pos 1)
      (loop do (send self :forward-to-char delimiter)
            until (not (bp-char-slashified? self))
            do (send self :advance-pos 1))
      (send self :advance-pos 1)))
  self)

(defmethod (bp :backward-over-string) ()
  (let ((delimiter (send self :get-char-backward)))
    (cond ((and (of-syntax delimiter string-quote)
                (not (bp-char-slashified? self)))
             (loop do (send self :backward-to-char delimiter)
                      (send self :advance-pos -1)
                   until (not (bp-char-slashified? self))))
          (:not-a-string
             (send self :advance-pos 1))))
  self)


(defmethod (bp :forward-over-atom) ()
  (let ((chrs (line-chars line))
        (max (line-char-count line)))
    (cond ((and (<& position max)
                (=& 1 (get-char-syntax-bit syntax-table
                                           (schar chrs position)
                                           string-quote)))
           (send self :forward-over-string))
          (t (do ((i position (1+& i)))
                 ((or (>=& i max)
                      (and (not (atom-char? (schar chrs i)))
                           (or (zerop& i)
                               (zerop& (get-char-syntax-bit
                                     syntax-table (schar chrs (1-& i))
                                     character-quote)))))
                  (setq position i))))))
  self)

(defmethod (bp :backward-over-atom) ()
  (let ((chrs (line-chars line)))
    (cond ((zerop& position))
          ((=& 1 (get-char-syntax-bit syntax-table
                                      (schar chrs (1-& position))
                                      string-quote))
           (send self :backward-over-string))
          (t (do ((i (1-& position) (1-& i)))
                 ((or (minusp& i)
                      (and (not (atom-char? (char chrs i)))
                           (or (zerop& i)
                               (zerop& (get-char-syntax-bit
                                     syntax-table (schar chrs (1-& i))
                                     character-quote)))))
                  (setq position (1+& i)))))))
  self)

(defmethod (bp :forward-over-horizontal-white-space) ()
  (let ((chrs (line-chars line))
        (max (line-char-count line)))
    (do ((i position (1+& i)))
        ((or (>=& i max) (not (white-space? (schar chrs i))))
         (setq position i))))
  self)

(defmethod (bp :backward-over-horizontal-white-space) ()
  (let ((chrs (line-chars line)))
    (do ((i (1-& position) (1-& i)))
        ((or (minusp& i) (not (white-space? (schar chrs i))))
         (setq position (1+& i)))))
  self)

(defmethod (bp :forward-over-white-space) ()
  (do ()
      ((do ((chrs (line-chars line))
            (max (line-char-count line))
            (i position (1+& i)))
           ((>=& i max) (setq position max) nil)
           (when (not (white-space? (schar chrs i)))
            (setq position i)
            (return t)))
       T)
      (if (line-next line)
          (send self :move (line-next line) 0) ;To-beginning-of-next-line.
          (return nil)))
 self)

(defmethod (bp :backward-over-white-space) ()
  (do ((prev))
      ((do ((chrs (line-chars line))
            (i (1-& position) (1-& i)))
           ((minusp& i) (setq position 0) nil)
           (when (not (white-space? (schar chrs i)))
            (setq position (1+& i))
            (return t)))
       T)
      (if (setq prev (line-previous line))
          (send self :move prev (line-char-count prev))
          (return nil)))
 self)

;The above two methods could be written like this:
;(defmethod (bp :forward-over-white-space) ()
;  (send self :forward-over-syntax white-space-mask))
;
;(defmethod (bp :backward-over-white-space) ()
;  (send self :backward-over-syntax white-space-mask))


(defmethod (bp :forward-over-syntax) (syntax-mask)
  (loop until
        (loop with chrs = (line-chars line)
              with max = (line-char-count line)
              until (>=& position max)
              thereis (zerop& (logand& (get-char-syntax syntax-table
                                                    (schar chrs position))
                                   syntax-mask))
              do (setq position (1+& position)))
        until (or (null (line-next line))
                  (zerop& (logand& (get-char-syntax syntax-table #\return)
                               syntax-mask)))
        do (send self :move (line-next line) 0))
  self)

(defmethod (bp :backward-over-syntax) (syntax-mask)
  (loop until
        (loop with chrs = (line-chars line)
              until (zerop& position)
              thereis (zerop& (logand& (get-char-syntax
                                     syntax-table (schar chrs (1-& position)))
                                   syntax-mask))
              do (setq position (1-& position)))
        until (or (null (line-previous line))
                  (zerop& (logand& (get-char-syntax syntax-table #\return)
                               syntax-mask)))
        do (send self :to-end-of-previous-line))
  self)

;;; These methods will move a buffer buffer to the beginning or end of
;;; the buffer rather than signalling an error if the character is not found.
(defmethod (bp :forward-to-char) (target-char)
  (loop for line* first line then (line-next line*)
        for pos first position then 0
        for i = (%string-posq target-char
                              (line-chars line*) pos (-& (line-char-count line*) pos))
        if (not (null i))
        return (send self :move line* i)
        if (null (line-next line*))
        return (send self :move line* (line-char-count line*)))
  self)

(defmethod (bp :backward-to-char) (target-char)
  (loop for line* first line then (line-previous line*)
        for pos first position then (line-char-count line*)
        for i = (and (plusp& pos)
                     (string-reverse-search-char target-char
                                                 (line-chars line*) pos 0))
        if (not (null i))
        return (send self :move line* i)
        if (null (line-previous line*))
        return (send self :move line* 0))
  (send self :advance-pos 1) ;leave BP after char.
  self)
