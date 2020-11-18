;;; -*- Mode:LISP; Package:USER; Base:10; Readtable:ZL -*-

(defun glass-tty (&optional (device "SDU-SERIAL-B:") &rest options)
  (with-open-stream (serial-stream (open device))
    (do ((l options (cddr l)))
        ((null l))
      (send serial-stream (car l) (cadr l)))
    (terpri)
    (glass-tty-loop serial-stream terminal-io)))


(defun glass-tty-loop (serial-stream terminal)
  (do ((ch)(tch))
      (nil)
    (process-wait "tyi or sdu"
                  #'(lambda (term sdu)
                      (or (send term ':listen)
                          (send sdu ':listen)))
                  terminal
                  serial-stream)
    (cond ((setq ch (send serial-stream ':tyi-no-hang))
           (if (numberp ch)
               (setq ch (ldb #o0007 ch)))
           ;; this is enough for a primitive but effective
           ;; terminal simulator.
           (selectq ch
             (#o10                              ; back-space
              (cursorpos 'x terminal))
             (#o12)                                     ; line-feed
             (#o11                                      ; tab
              (send terminal ':tyo #\tab))
             (#o14
              (send terminal ':clear-screen))
             (#o7
              (send terminal ':beep))
             (#o15
              (send terminal ':fresh-line))
             (otherwise
              (send terminal ':tyo ch)))))
    (cond ((setq ch (send terminal ':tyi-no-hang))
           (cond ((setq tch (simple-control-char-p ch))
                  (send serial-stream ':tyo tch))
                 ('else
                   (selectq ch
                     (#\network
                      (format terminal "~&Network>")
                      (selectq (prog1 (tyi terminal) (terpri terminal))
                        ((#/? #\help #\h #\H)
                         (format terminal
                                 "~&<Network><Network> send a CONTROL-^~
                                  ~%<Network>S Status~
                                  ~%<Network>E or Q EXIT.~%"))
                        ((#/s #/S)
                         (send-if-handles serial-stream :describe-status))
                        (#\network
                         (send serial-stream ':tyo (simple-control-char-p #\control-^)))
                        ((#\e #\E #\q #\Q)
                         (return nil))
                        (t
                         (send terminal ':beep))))
                     (#\altmode (send serial-stream ':tyo #o33))
                     (#\return (send serial-stream ':tyo #o15))
                     (#\line (send serial-stream ':tyo #o12))
                     ((#\rubout #\delete) (send serial-stream ':tyo #o177))
                     (#\OVERSTRIKE
                      (SEND SERIAL-STREAM ':tyo #o10))
                     (t
                      (send serial-stream ':tyo (logand #o177 ch))))))))))


(defun simple-control-char-p (ch)
  (if (and (= 1 (ldb si:%%kbd-control ch))
           (= 0 (ldb si:%%kbd-meta ch))
           (= 0 (ldb si:%%kbd-super ch))
           (= 0 (ldb si:%%kbd-hyper ch)))
      (- ch #o500)))
