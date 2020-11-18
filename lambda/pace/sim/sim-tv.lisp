;;; -*- Mode:LISP; Package:SIM; Readtable:CL; Base:10 -*-

(defflavor sim-frame
           ()
           (tv:bordered-constraint-frame-with-shared-io-buffer)
           (:default-init-plist :panes
                                '((lisp lam:sensitive-lisp-listener
                                        :save-bits t)
                                  (display tv:window
                                           :blinker-deselected-visibility :off
                                           :save-bits t))
                                :constraints
                                '((sim-frame (display lisp)
                                             ((display 6 :lines))
                                             ((lisp :even)))))
           )


(defmethod (sim-frame :after :init) (&rest ignore)
  (send self :set-selection-substitute (send self :get-pane 'lisp)))

(tv:add-system-key #\1 'sim-frame "Simulator")

(defun display ()
  (let ((sup (send terminal-io :superior)))
    (cond ((typep sup 'sim-frame)
           (send sup :get-pane 'display))
          (t
           terminal-io))))

(defun update-display ()
  (labels ((register-contents (inst base-code offset)
                              (ecase base-code
                                (0
                                 (send *proc* :read-open offset))
                                (1
                                 (send *proc* :read-active offset))
                                (2
                                 (send *proc* :read-return offset))
                                (3
                                 (send *proc* :read-frames (+ (ash (ldb %%i-immediate inst) 4)
                                                              offset)))
                                (4
                                 (send *proc* :read-functional offset))
                                )))
    (catch-error-restart ((sys:abort error) "Give up trying to UPDATE-DISPLAY")
      (let* ((w (display))
             (pc (send *proc* :read-next-pc))
             (inst (send *proc* :read-main-memory (send *proc* :read-pc))))
        (send w :clear-screen)
        (format w "PC=#o~o " pc)
        (print-symbolic-adr pc w)
        (format w "~40t")
        (format w "~s" (disassemble-inst inst))
        (format w "~&DEST=~o~20tSRC1=~o~40tSRC2=~o"
                (register-contents inst (ldb %%i-dest-base inst) (ldb %%i-dest-offset inst))
                (register-contents inst (ldb %%i-src-1-base inst) (ldb %%i-src-1-offset inst))
                (register-contents inst (ldb %%i-src-2-base inst) (ldb %%i-src-2-offset inst)))
        (format w "~&O=~o~20tA=~o~40tR=~o"
                (send *proc* :read-open-frame)
                (send *proc* :read-active-frame)
                (send *proc* :read-return-frame))

        (when (not (zerop (send *proc* :read-noop-next-bit)))
          (format w "~&NOOP"))
        )
      )))

(defvar *get-next-command-unget* nil)

(defun unget-command (blip)
  (setq *get-next-command-unget* (append *get-next-command-unget* (list blip))))

(defconst *activation-list* '(#\/ #\tab #\space #\: #\altmode
                              #\@ #\linefeed #\^ #\= #\page #\:
                              #\. #\_ #\(
                              ))
(defun get-next-command ()
  (cond ((null *get-next-command-unget*)
         (labels ((act-fun (char)
                           (or (ldb-test %%kbd-super char)
                               (member char *activation-list*
                                       :test 'char-equal))))
           (do (command blip error-p)
               (())
             (multiple-value (command blip)
               (with-input-editing (terminal-io
                                     `((:activation ,#'act-fun)
                                       (:preemptable)
                                       (:full-rubout :full-rubout)
                                       )
                                     )
                 (do ((out-string (make-string 10. :fill-pointer 0))
                      (char (send terminal-io :any-tyi)
                            (send terminal-io :any-tyi)))
                     ((or (not (integerp char)
                               (characterp char)))
                      (values out-string char))
                   (array-push-extend out-string char))))
             (cond ((eq blip :full-rubout)
                    (return :full-rubout))
                   ((consp command)
                    (case (car command)
                      (:typeout-execute
                       (case (cadr command)
                         (lam:force-object-kbd-input
                          (funcall (cadr command) (caddr command) terminal-io))))))
                   (t
                    (multiple-value (*command-prefix* error-p)
                      (ignore-errors (read-from-string command nil)))
                    (cond (error-p
                           (format t "?? error reading "))
                          (t
                           (when (not (member (cadr blip) '(#\linefeed #\( ) :test 'char-equal))
                             (format t "~c" (cadr blip)))
                           (return blip))))))))
        (t
         (pop *get-next-command-unget*))))


(defvar *accumulator*)
(defvar *command-prefix*)
(defvar *last-command*)
(defvar *update-display*)
(defvar *current-address*)

(defun sim ()
  (let ((*accumulator* nil)
        (*command-prefix* nil)
        (*update-display* t)
        (*read-base* 8)
        (*print-base* 8)
        (*last-command* nil)
        (*current-address* 0)
        )
    (command-loop)
    ))


(defsignal bad-reg-adr error ())
(defsignal sim error ())

(defun command-loop ()
  (format t "~&")
  (catch 'exit
    (do-forever
      (catch-error-restart ((sys:abort error) "Return to top level in SIM")
        (condition-case (error)
            (do-forever
             (when *update-display*
               (update-display)
               (setq *update-display* nil))
             (let ((blip (get-next-command)))
               (cond ((eq blip :full-rubout)
                      (format t " ?? "))
                     (t
                      (let ((val (sim-eval *command-prefix*)))
                        (when (numberp val)
                          (if (null *accumulator*) (setq *accumulator* 0))
                          (incf *accumulator* val)))
                      (when (consp blip)
                        (case (car blip)
                          (:activation
                           (let* ((fun-name (format nil "SIM-~@:(~:C~)" (cadr blip)))
                                  (fun (intern-soft fun-name 'sim)))
                             (cond ((fboundp fun)
                                    (funcall fun)
                                    (setq *last-command* fun)
                                    (when (not (memq fun '(sim-@ sim-space sim-.)))
                                      (setq *accumulator* nil)))
                                   (t
                                    (format t "?? ~s " fun-name)))))))))))
          ((bad-reg-adr sim)
           (format t "~&Error: ")
           (send error :report terminal-io)
           (format t "~&")
           (setq *accumulator* nil))))
      (format t "~&Back to SIM top level.~&"))))

(defun sim-space ()
  )

(defun sim-altmode ()
  (throw 'exit (>convert-to-pointer *last-value*)))

(defun sim-page ()
  (setq *update-display* t))

(defun sim-super-n ()
  (send *proc* :single-step)
  (setq *update-display* t))

(defun sim-@ ()
  (let ((blip (get-next-command)))
    (unget-command blip)
    (case *command-prefix*
      (G
       (when (null *accumulator*)
         (ferror 'sim "no value available"))
       (send *proc* :write-next-pc *accumulator*)
       (send *proc* :write-noop-next-bit 1)
       (format t "~&")
       (setq *update-display* t))
      (t
       (let ((off (ra-command-to-address *command-prefix*)))
         (cond ((null off)
                (format t "?? unknown base"))
               ((null *accumulator*)
                (ferror 'sim "@ requires a prefix value"))
               (t
                (incf *accumulator* off))))))))

(defun sim-/ (&aux val)
  (format t " ")
  (when (null *accumulator*)
    (ferror 'sim "no value accumulated"))
  (setq *current-address* *accumulator*)
  (multiple-value-bind (sym-base offset adr-mode value-type)
      (symbolic-adr *current-address*)
    sym-base offset adr-mode
    (cond ((< *current-address* 0)
           (setq val (ra-read *current-address*)))
          (t
           (setq val (send *proc* :read-main-memory *current-address*))))
    (incf *current-address* (sim-print-val val value-type *current-address*))))

(defvar *last-value* 0)

(defun sim-= ()
  (cond ((null *accumulator*)
         (format t "  ~o  " *last-value*))
        (t
         (format t "  ~o  " *accumulator*)
         (setq *last-value* *accumulator*))))


(defun sim-print-val (val type &optional adr)
  (setq *last-value* val)
  (ecase type
    (:inst (let ((sym-inst (disassemble-inst val)))
             (cond ((and (eq (car sym-inst) 'store-immediate)
                         adr
                         (>= adr 0))
                    (format t "~s " sym-inst)
                    (format t "~o " (send *proc* :read-main-memory (+ adr 1)))
                    1)
                   (t
                    (format t "~s " sym-inst)
                    0))))
    (:opc  (when (ldb-test (byte 1 31.) val)
             (format t "  {N}  "))
           (print-symbolic-adr (ldb-big (byte 31. 0) val))
           (format t "  ")
           0)
    (nil (format t " ~o " val)
         0)))

(defun symbolic-adr (adr &aux info)
  (declare (values base offset adr-mode value-type))
  (setq info (ra-address-info adr))
  (cond ((< adr 0)
         (when (null info)
           (ferror 'bad-reg-adr "unknown reg-adr ~o" adr))
         (values (cadr info)
                 (- adr (car info))
                 :at-sign
                 (cadddr (cdr info))))
        (t
         (cond ((null info)
                (values nil adr :number (cadddr (cdr info))))
               ((= (car info) adr)
                (values (cadr info) 0 :symbol (cadddr (cdr info))))
               (t
                (values (cadr info) (- adr (car info)) :list (cadddr (cdr info))))))))

(defun print-symbolic-adr (adr &optional (stream t))
  (declare (values sym-base offset adr-mode value-type))
  (multiple-value-bind (sym-base offset adr-mode value-type)
      (symbolic-adr adr)
    (ecase adr-mode
      (:at-sign
       (format stream "~o@~s" offset sym-base))
      (:number
       (format stream "~o" offset))
      (:symbol
       (format stream "~s" sym-base))
      (:list
       (format stream "(~s ~o)" sym-base offset)))
    (values sym-base offset adr-mode value-type)))


(defun print-new-adr-and-contents (&aux val)
  (format t "~&")
  (multiple-value-bind (sym-base offset adr-mode value-type)
      (print-symbolic-adr *current-address*)
    sym-base offset adr-mode
    (cond ((< *current-address* 0)
           (setq val (ra-read *current-address*)))
          (t
           (setq val (send *proc* :read-main-memory *current-address*))))
    (format t "/ ")
    (incf *current-address* (sim-print-val val value-type *current-address*))))

(defun sim-tab ()
  (when (< *last-value* 0)
    (ferror 'sim "current address must be main memory"))
  (let ((ptr (>convert-to-pointer *last-value*)))
    (setq *current-address* (>pointer-as-integer ptr)))
  (print-new-adr-and-contents))

(defun sim-line ()
  (incf *current-address*)
  (print-new-adr-and-contents))

(defun sim-^ ()
  (decf *current-address*)
  (print-new-adr-and-contents))

(defun sim-super-p ()
  (format t "--RUN--")
  (send *proc* :run t)
  (format t "STOP~&")
  (setq *update-display* t))

(defun sim-super-t ()
  (let ((start-time (time))
        end-time)
    (format t "--RUN--")
    (send *proc* :run)
    (setq end-time (time))
    (format t "STOP~&")
    (format t "~&Real time ~\\time-interval\\" (round (time-difference end-time start-time) 60.))
    (let ((n (read-register 'micro:a-sim-inst-counter)))
      (format t "~&~:d instructions" n)
      (let ((clock 72.))
        (format t "~&Simulated time (~d ns/clk) = ~8f sec" clock (* n clock 1e-9))))
    (format t "~&Memory cost = ~d." (read-register 'micro:a-sim-memory-cost))
    (format t "~&")
    (setq *update-display* t)))

(defun sim-colon-memory-cost ()
  (let ((cost (read-with-full-rubout terminal-io)))
    (when (y-or-n-p "~&New memory cost = ~d., OK? " cost)
      (setq *memory-cost* cost)
      (write-register 'micro:a-sim-memory-cost cost)))
  (format t "~&"))

(defun sim-. ()
  (when (null *accumulator*)
    (setq *accumulator* 0))
  (incf *accumulator* *current-address*))

(defun read-with-full-rubout (stream)
  (with-input-editing (terminal-io
                        '((:full-rubout :full-rubout)))
    (read stream)))

(defun sim-colon-try ()
  (let ((form (read-with-full-rubout terminal-io)))
    (cold-reset)
    (sim-test (car form) (cdr form))
    (setq *update-display* t)
    (format t "~&")
    ))

(defun |SIM-:| ()
  (let ((command (read-with-full-rubout terminal-io)))
    (cond ((null command)
           (format t " ?? "))
          (t
           (when (not (symbolp command))
             (ferror nil "colon commands must be symbols"))
           (let ((func (intern-soft (string-append "SIM-COLON-" command) 'sim)))
             (cond ((and func
                         (fboundp func))
                    (funcall func))
                   (t
                    (format t "~&SIM-COLON-~a is not implemented~&" command))))))))

(defvar *breakpoint-list* nil)

(defun adr-to-breakpoint-name (adr)
  (multiple-value-bind (sym-base offset)
      (symbolic-adr adr)
    (cond ((null sym-base)
           nil)
          ((zerop offset)
           sym-base)
          (t
           (list sym-base offset)))))

(defun breakpoint-name-to-adr (name)
  (let ((base-name (if (consp name) (car name) name))
        (offset (if (consp name) (cadr name) 0)))
    (let ((base-adr (symbol-lookup base-name)))
      (cond ((null base-adr)
             (ferror nil "unknown name ~s" name))
            (t
             (+ base-adr offset))))))

(defun sim-colon-b (&optional (adr *accumulator*))
  (when (null adr)
    (ferror nil "no value accumulated"))
  (when (< adr 0)
    (ferror nil "breakpoints only work in main memory"))
  (let ((breakpoint-name (adr-to-breakpoint-name adr)))
    (cond ((null breakpoint-name)
           (ferror nil "~&no symbol at adr ~s~&" adr))
          (t
           (when (not (member breakpoint-name *breakpoint-list* :test 'equal))
             (push breakpoint-name *breakpoint-list*))
           (let ((inst (send *proc* :read-main-memory adr)))
             (cond ((ldb-test %%i-halt inst)
                    (format t "~&~s already has the halt bit set~&" breakpoint-name))
                   (t
                    (send *proc* :write-main-memory adr (dpb 1 %%i-halt inst))
                    (format t "~&Breakpoint set at ~s~&" breakpoint-name))))))))

(defun sim-colon-ub (&aux x)
  (when (null *accumulator*)
    (ferror nil "no value accumulated"))
  (when (< *accumulator* 0)
    (ferror nil "breakpoints only work in main memory"))
  (let ((inst (send *proc* :read-main-memory *accumulator*)))
    (cond ((ldb-test %%i-halt inst)
           (send *proc* :write-main-memory *accumulator* (dpb 0 %%i-halt inst)))
          (t
           (format t "~&halt bit not set~&"))))
  (let ((breakpoint-name (adr-to-breakpoint-name *accumulator*)))
    (cond ((null breakpoint-name)
           (format t "~&No name for location ~s" *accumulator*))
          ((setq x (member breakpoint-name *breakpoint-list*))
           (setq *breakpoint-list* (delq (car x) *breakpoint-list*))
           (format t "~&Breakpoint at ~s removed.~&" breakpoint-name))
          (t
           (format t "~&~s is not on the breakpoint list~&" breakpoint-name)))))

(defun sim-colon-uab (&optional dont-print-newline)
  (dolist (breakpoint-name *breakpoint-list*)
    (let ((adr (breakpoint-name-to-adr breakpoint-name)))
      (cond ((null adr)
             (format t "~&Can't find address for ~s" breakpoint-name))
            (t
             (send *proc* :write-main-memory adr
                   (dpb 0 %%i-halt (send *proc* :read-main-memory adr)))))))
  (setq *breakpoint-list* nil)
  (when (not dont-print-newline)
    (format t "~&")))

(defun sim-colon-rb ()
  (dolist (breakpoint-name (copylist *breakpoint-list*))
    (let ((adr (breakpoint-name-to-adr breakpoint-name)))
      (cond ((null adr)
             (format t "~&Can't find address for ~s" breakpoint-name)
             (setq *breakpoint-list* (delq breakpoint-name *breakpoint-list*)))
            (t
             (send *proc* :write-main-memory adr
                   (dpb 1 %%i-halt (send *proc* :read-main-memory adr)))
             (format t "~&Breakpoint ~s replaced.~&" breakpoint-name))))))

(defun sim-colon-reload ()
  (sim-reload)
  (sim-colon-rb)
  (setq *update-display* t))

(defun print-function-and-offset (adr)
  (let ((info (main-mem-symbol-info adr)))
    (cond ((null info)
           (format t "unknown-function-~o" adr))
          (t
           (format t "~S[#o~O]" (cadr info) (- adr (car info)))))))

(defun sim-colon-trace ()
  (do ((prev-frame (send *proc* :previous-active (send *proc* :read-active-frame))
                   (send *proc* :previous-active prev-frame))
       (frame (send *proc* :read-active-frame)
              (send *proc* :previous-active frame))
       (pc (send *proc* :read-next-pc)
           (send *proc* :frame-saved-pc frame)))
      (())
    (format t "~&~4o: " frame)
    (print-function-and-offset pc)
    (when (zerop prev-frame)
      (return nil)))
  (format t "~&"))

(defun sim-colon-return-break ()
  (let ((pc (send *proc* :frame-saved-pc (send *proc* :read-active-frame))))
    (sim-colon-b pc)))


(defun sim-_ ()
  (let ((underline-command (send standard-input :tyi)))
    (send standard-output :tyo underline-command)
    (let* ((fun-name (format nil "SIM-UNDERLINE-~@:(~:C~)" underline-command))
           (fun (intern-soft fun-name 'sim)))
      (cond ((or (null fun)
                 (not (fboundp fun)))
             (format t "~&~s is not implemented~&" fun-name))
            (t
             (funcall fun))))))

(defun sim-underline-q ()
  (let ((q (>convert-to-pointer *last-value*)))
    (format t "  ")
    (>print-random-object q)
    (format t "  ")))

(defun sim-underline-s ()
  (let ((q (>convert-to-pointer *last-value*)))
    (format t " ")
    (>prin1 q)
    (format t " ")))

(defun sim-underline-t ()
  (format t "  ")
  (send standard-output :tyo (ldb (byte 8 0) *last-value*))
  (send standard-output :tyo (ldb (byte 8 8) *last-value*))
  (send standard-output :tyo (ldb (byte 8 16.) *last-value*))
  (send standard-output :tyo (ldb (byte 8 24.) *last-value*))
  (format t "  "))

(defun |SIM-(| ()
  (format t "~&")
  (let ((form (with-input-editing (terminal-io
                                    `((:full-rubout :full-rubout)
                                      (:initial-input "(")
                                      (:initial-input-pointer 1)
                                      (:prompt "Eval: ")))
                (read))))
    (let ((val (eval form)))
      (print val)
      (format t "~&")
      (typecase val
        (integer (setq *last-value* val))
        (sim-pointer (setq *last-value* (>pointer-as-integer val)))))))
