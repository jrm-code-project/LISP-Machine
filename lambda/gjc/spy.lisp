;;; -*- Mode:LISP; Package:(SPY GLOBAL); Fonts:(MEDFNB); Base:10 -*-


;;; Program to see what is going on in some other process.
;;; 10/12/84 23:26:38 -George Carrette

;;; Provided Functions:
;;; (SPY:PICK-A-PROCESS)
;;; (SPY:STACK-SPY <some-process>)
;;; (SPY:QUANTUM <process> &optional <new-quantum> <scheduler-rate>)

;;; Copyright LISP Machine, Inc. 1984
;;;   See filename "Copyright" for
;;; licensing and release information.


(defun pick-a-process ()
  "Puts up a menu to choose from the active proceeses"
  (let ((alist)(value))
    (dolist (p si:active-processes)
      (if (car p)
          (push (list (si:process-name (car p)) :value (car p)) alist)))
    (do ()
        ((setq value (tv:menu-choose alist "     pick a process"))
         value))))


(defun quantum (&optional process quantum scheduler-rate)
  "Reads or sets the quantum of the process P. Also sets the scheduler rate."
  (let ((p (get-process (or process current-process))))
    (cond ((null quantum)
           (list (format nil "For ~A in 60'ths of a second:"
                         (si:process-name p))
                 :quantum (si:process-quantum p)
                 :clock-rate (read-meter 'si:%tv-clock-rate)))
          ('else
           (send p :set-quantum quantum)
           (when scheduler-rate
             (write-meter 'si:%tv-clock-rate
                          (if (numberp scheduler-rate)
                              scheduler-rate
                            quantum)))
           (quantum p)))))

(defun get-process (x)
  (cond ((typep x 'si:process)
         x)
        ('else
         (or (send x :Send-if-handles :process)
             (ferror Nil "can't get a process from ~S" x)))))


(defun get-stack-group (x &aux temp)
  "Returns two values, a stack group and a process, if any"
  (cond ((typep x :stack-group)
         x)
        ((setq temp (send x :send-if-handles :process))
         (values (send temp :stack-group) temp))
        ((typep x 'si:process)
         (values (send x :stack-group) x))
        (t
         (or (send x :send-if-handles :stack-group)
             (ferror nil "Can't get a stack group from ~S" x)))))

(defvar *terse? t)

(defun sg-not-in-schedulerp (sg)
  (not (eq (si:rp-function-word (si:sg-regular-pdl sg)
                                (sys:SG-AP SG))
           sys:scheduler-stack-group)))

(defun wait-for-sg-not-in-scheduler (sg)
  (process-wait "spy" #'sg-not-in-schedulerp sg))

(defun wait-for-process-run (proc lt)
  (process-wait "*spy*"
                #'(lambda (p x)
                    (not (eq (si:process-last-time-run p)
                             x)))
                proc
                lt))

(defun stack-spy (x &optional &key (terse t) sb-hack)
  "Spy on stack group or process X. Prints a line of info
everytime it is seen that the state of X changes. SB-Hack
controls use of extra sequence breaks."
  (let ((SG)(proc)(*terse? terse)(old-sb)
        (stream (make-char-punting-stream standard-output)))
    (multiple-value (sg proc) (get-stack-group x))
    (unwind-protect
        (progn (and sb-hack (setq old-sb (si:sb-on)))
               (si:sb-on (if (eq sb-hack t)
                             '(:call :unibus :chaos :clock)
                           (union '(:clock) sb-hack)))
               (cond ((null proc)
                      (do ()
                          (nil)
                        (wait-for-sg-not-in-scheduler sg)
                        (stack-print-1 sg stream)
                        (process-sleep 1. "spy sleep")))
                     ('else
                      (do ((lt))
                          (nil)
                        (setq lt (si:process-last-time-run proc))
                        (stack-print-1 sg stream)
                        (wait-for-process-run proc lt)))))
      (and sb-hack (si:sb-on old-sb)))))


(defvar *wholine* nil)
(defvar *background-spy* nil)

(defun start-background-spy ()
  (or *wholine* (setq *wholine* (make-char-punting-stream tv:who-line-documentation-window)))
  (or *background-spy* (setq *background-spy*
                            (make-process "background spy")))
  (process-disable *background-spy*))

(defun stop-background-spy ()
  (if *background-spy* (process-disable *background-spy*)))

(defun background-spy-case-sg (sg)
  (do ()
      (nil)
    (wait-for-sg-not-in-scheduler sg)
    (send *wholine* :clear-screen)
    (stack-print-1 sg *wholine*)
    (process-sleep 1. "spy sleep")))


(defun background-spy-case-proc (sg proc)
  (do ((lt))
      (nil)
    (setq lt (si:process-last-time-run proc))
    (when (or (not *terse?) (sg-not-in-schedulerp sg))
      (send *wholine* :clear-screen)
      (stack-print-1 sg *wholine*))
    (wait-for-process-run proc lt)))


(defun background-spy (x)
  "Spy on stack group or process X. Prints a line of info
everytime it is seen that the state of X changes."
  (start-background-spy)
  (multiple-value-bind (sg proc)
      (get-stack-group x)
    (cond ((null proc)
           (send *background-spy* :preset 'background-spy-case-sg sg))
          ('else
           (send *background-spy* :preset 'background-spy-case-proc sg proc)))
    (process-enable *background-spy*)))

(defvar *print-methods* :operation)

(defun stack-print-1 (sg punting-stream &aux
                      eh:EH-ERROR
                      eh:CURRENT-FRAME eh:ERROR-LOCUS-FRAME
                      eh:INNERMOST-VISIBLE-FRAME
                      eh:INNERMOST-FRAME-IS-INTERESTING)
  "Print one line of information about the stack group to the punting-stream"
  ;; &aux variables are for simulating the environment inside
  ;; the error handler, which is usually the only thing to look at
  ;; stacks. Many useful fuction in EH can be called.
  (SETQ eh:INNERMOST-VISIBLE-FRAME (sys:SG-AP SG))
  (setq eh:current-frame eh:innermost-visible-frame)
  (send punting-stream :fresh-line)
  (call-on-char-punting-stream
    0
    punting-stream
    #'(lambda (stream sg)
        (map-n-frames
          #'(lambda (number sg frame stream) number
                    (let* ((f (si:rp-function-word
                                                 (si:sg-regular-pdl sg)
                                                 frame))
                           (name (function-name f)))
                      (cond ((atom name)
                             (princ name stream))
                            ('else
                             (selectq *print-methods*
                               (:operation
                                (princ (caddr name) stream))
                               (t
                                (princ name stream)))))
                      (princ " " stream)
                      ;; kludge
                      (or (null name)
                          (and *terse? (eq f sys:scheduler-stack-group)))))

          sg
          eh:current-frame
          ()
          stream))
      punting-stream
      sg))

(defvar *map-n-frames-active-only? t)

(defun next-frame (stack-group frame)
  (if *map-n-frames-active-only?
      (eh:sg-next-active stack-group frame)
    (eh:sg-next-open stack-group frame)))

(defun map-n-frames (f stack-group start-frame n &rest l)
  "Call F on n frames in the stack group on arguments:
frame-number, stack-group, frame, &rest l.
Return if F returns T."
  (do ((j 0 (1+ j))
       (f-val)
       ;; using EH:SG-NEXT-OPEN gives an error on about the last frame.
       ;; but in use, the FRAME-to-STOP-ON might save us.
       (frame start-frame (next-frame stack-group frame)))
      ((or (null frame) (and n (> j n))))
    (if (setq f-val (lexpr-funcall f j stack-group frame l))
        (return f-val))))

(defvar *char-punting-limit*)
(defvar *char-punting-count*)
(defvar *char-punting-output*)

(defun char-punting-stream-closure-function (operation
                                             &optional arg1 &Rest args)
  (selectq operation
    (:which-operations
     '(:tyo :reset :count :clear-screen))
    ((:tyo)
     (cond ((> (setq *char-punting-count* (1+ *char-punting-count*))
               *char-punting-limit*)
            (princ " etc ..." *char-punting-output*)
            (*throw 'char-punt *char-punting-limit*))
           ('else
            (send *char-punting-output* operation arg1))))
    ((:reset)
     (setq *char-punting-count* (or arg1 0)))
    ((:count)
     *char-punting-count*)
    ((:fresh-line)
     (setq *char-punting-count* 0)
     (send *char-punting-output* :fresh-line))
    ((:clear-screen)
     (char-punting-stream-closure-function :reset)
     (send *char-punting-output* :clear-screen))
    (t
     (stream-default-handler 'char-punting-stream-closure-function
                             operation arg1 args))))

(defun make-char-punting-stream (*char-punting-output*
                                 &optional (*char-punting-limit*
                                             (- (send *char-punting-output*
                                                      :size-in-characters)
                                                10))
                                 &aux (*char-punting-count* 0))
  (closure '(*char-punting-output* *char-punting-limit* *char-punting-count*)
           'char-punting-stream-closure-function))

(defun call-on-char-punting-stream (sofar stream f &rest l)
  (send stream :reset sofar)
  (*catch 'char-punt (lexpr-funcall f l))
  (send stream :count))

(defun prin1-puntable (sofar object stream)
  (call-on-char-punting-stream sofar stream #'prin1 object stream))

(defun princ-puntable (sofar object stream)
  (call-on-char-punting-stream sofar stream #'princ object stream))

(defun terpri-puntable (stream)
  (call-on-char-punting-stream 0 stream #'terpri stream))

(defun format-puntable (stream string &rest l)
  (lexpr-funcall #'call-on-char-punting-stream 0
                 stream #'format stream string l))
