;;; -*- Mode:LISP; Package:USER; Base:10 -*-


(defun up-lispm-hosts ()
  (let ((up-hosts nil))
    (CHAOS:POLL-HOSTS (subset #'(lambda (x) (eq (send x :system-type) :lispm))
                        (mapcar 'cadr si:host-alist))
                "STATUS"
                *STANDARD-OUTPUT*
                #'(lambda (stream) stream)
                #'(lambda (stream address pkt) stream pkt
                          (push (si:get-host-from-address address :chaos) up-hosts))
                :WHOSTATE "Hostat Reply"
                :ignore-states '(CHAOS:RFC-SENT-STATE))
    up-hosts))

(defun chaos-eval (string host)
  (CONDITION-CASE ()
      (with-open-stream (s (chaos:open-stream host "EVAL" :ascii-translation t))
        (send s :tyo #o377)
        (send s :tyo 0)
        (send s :tyo 0)
        (princ string s)
        (send s :tyo 13)
        (send s :tyo 10)
        (send s :force-output)
        (readline s))
    (SYS:REMOTE-NETWORK-ERROR NIL)))



(defun set-all-lispm-times (&optional (time (read-a-time)) (hosts (up-lispm-hosts)))
  (time:initialize-timebase time)
  (dolist (host hosts)
    (let ((st (format nil "(TIME:INITIALIZE-TIMEBASE #o~O)" (time:get-universal-time))))
      (format t "~&~A ~A" host st)
      (format t " ~A~%" (chaos-eval st host)))))


(defun read-a-time ()
  (do ((time))
      ((y-or-n-p "~&Is ~A correct?" (time:print-universal-time
                                      (setq time (prompt-and-read :date "~&Input the time>"))
                                      nil))
       time)))
