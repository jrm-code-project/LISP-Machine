;;; -*- Mode:LISP; Package:USER; Base:10 -*-


(defun enable-tcp-and-chaosnet (&optional hosts &aux l)
  (tcp:test-enable-excelan-chaos)
  (format t "~&Chaosnet Only Enabled, polling hosts...")
  (CHAOS:POLL-HOSTS HOSTS "STATUS"
                    terminal-io
                    #'(lambda (&rest ignore) nil)
                    #'(lambda (&rest ignore) nil)
                    :WHOSTATE "Hostat Reply"
                    :ignore-states '(CHAOS:RFC-SENT-STATE)
                    :timeout 1)
  (setq l ethernet:ether-address-translations)
  (format t "~&~D ethernet translations established, turning on tcp and chaosnet"
          (length ethernet:ether-address-translations))
  (tcp:test-enable-excelan-chaos :tcp-also t)
  (setq ethernet:ether-address-translations l)
  t)
