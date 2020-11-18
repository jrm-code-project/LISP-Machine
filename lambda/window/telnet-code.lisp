;;; -*- Mode:LISP; Package:CHAOS; Base:8; Fonts:(CPTFONT HL12I CPTFONTB) -*-


si:
(progn 'compile
(defun my-hack (char)
  (let (char-list)
    (loop for i from 0 to 3 do
          (push (logand #xff (lsh char (* -8. i))) char-list))
    char-list))

(defun my-reverse-hack (list)
  (let ((char 0))
    (loop for item in list
          and i first 3 then (- i 1) do
          (setq char (logior char (lsh item (* 8. i)))))
    char))

(defflavor lm-char-stream ((*communication* nil)) (chaos:binary-stream))
(defflavor test-stream () (chaos:binary-stream))

(defwrapper (lm-char-stream :untyi) ((char) . body)
  `(prog (return (setq *communication* char))))


(defwrapper (lm-char-stream :tyo) ((char) . body)
  `(lm-char-tyo-hack #'(lambda (&rest .daemon-caller-args.
                               &aux (.daemon-mapping-table. self-mapping-table))
                        .daemon-mapping-table.
                        . ,body)
                    char))

(defun lm-char-tyo-hack (stream char)
  (loop for item in (my-hack char) do
        (send stream ':tyo item)))

(defwrapper (lm-char-stream :tyi) (ignore . body)
  `(cond ((equal *communication* ())
          (let (char)
            (loop for i from 1 to 4 do
                  (push (progn . ,body) char))
            (my-reverse-hack (reverse char)))
          (setq *communication* ()))
         (t *communication*)))

(compile-flavor-methods lm-char-stream))



(defun chaos:make-stream (connection &key &optional (direction ':bidirectional)
                                              (characters nil)
                                              (ascii-translation nil)
                                              (lm-char-stream nil))
  "Return a stream that does I//O to an already established chaos connection.
:ASCII-TRANSLATION - if non-NIL, assume the data on the connection is in ASCII
 and translate to and from the Lisp machine character set as appropriate.
:DIRECTION, :CHARACTERS - as in OPEN.  :DIRECTION defaults to ':BIDIRECTIONAL."
  (make-instance (selectq direction
                   (:input
                    (cond (ascii-translation 'ascii-translating-input-character-stream)
                          (characters 'input-character-stream)
                          (t 'input-binary-stream)))
                   (:output
                    (cond (ascii-translation 'ascii-translating-output-character-stream)
                          (characters 'output-character-stream)
                          (t 'output-binary-stream)))
                   (:bidirectional
                    (cond (ascii-translation 'ascii-translating-character-stream)
                          (characters 'character-stream)
                          (lm-char-stream 'si:lm-char-stream)
                          (t 'binary-stream))))
                 ':connection connection))

(defunp chaos:telnet-server-function (&aux conn)
  ; we only chaned the flavor of stream to implement----- to lm-char-stream
  (setq conn (listen "TELNET"))
  (let ((lose
          (disallow-connection? "TELNET" conn (list telnet-server-on ':reject-symbolics))))
    (when lose
      (reject conn lose)
      (return nil))
    (accept conn)
    (push conn eval-server-connections)
    (send tv:who-line-file-state-sheet ':add-server conn "TELNET")
    (condition-case ()
        (let ((untyi-char nil))
          (declare (special untyi-char))
          (with-open-stream (stream (make-stream conn ':lm-char-stream t))
            (declare (special stream))
            (print-herald stream)
            (format stream "~&Telnet server here~2%")
            (send stream ':force-output)
            ;; Flush any number of telnet negotiations.
            ;; (We only understand the simplest kind).
            (do-forever
              (let ((ch (tyi stream)))
                (if (= ch #o377)
                    (progn (tyi stream) (tyi stream))
                  (return (send stream ':untyi ch)))))
            (si:lisp-top-level1 (closure '(stream untyi-char) 'echoing-stream))))
      (sys:remote-network-error nil))))


(add-initialization "TELNET"
                    '(process-run-function "TELNET Server" 'chaos:telnet-server-function)
                    nil
                    'server-alist)

supdup:
(progn 'compile

(defflavor lm-telnet () (telnet))

(defmethod (lm-telnet :set-connection) (new-connection)
  (send typein-process ':reset)
  (send typeout-process ':reset)
  (setq stream (chaos:make-stream new-connection ':lm-char-stream t))
  (send self ':gobble-greeting)
;; Typeout process initially waits to see CONNECTION non-NIL.
  (setq connection new-connection)
  (setq black-on-white nil))

(defmethod (lm-telnet :net-output-translated) (char)
  (send stream ':tyo char))

(defmethod (lm-telnet :net-output) (char)
  (send stream ':tyo char))

(recompile-flavor 'lm-telnet)

(defun supdup:telnet-separate (&optional path &aux sw)
  "Switch to a non-connected TELNET window and connect it to machine PATH.
If PATH is NIL, a connected TELNET window will be selected if there is one."
  (cond ((and (null path) (setq sw (find-selectable-telnet t nil)))
         (send sw ':select)
         nil)
        (t
         (setq sw (or (find-selectable-telnet nil) (tv:make-window 'lm-telnet)))
         (send sw ':set-connect-to (or path supdup-default-path
                                          si:associated-machine))
         (send sw ':expose nil ':clean) ;Don't come up with old garbage
         (send sw ':select)
         t))))
