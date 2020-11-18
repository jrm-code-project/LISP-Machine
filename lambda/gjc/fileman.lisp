;;; -*- Mode:LISP; Package:USER; Base:10; Readtable:ZL -*-

(when (fboundp 'fs:start-disk-space-warner-process)
  (format t "~&;Starting disk space warner process, ~D blocks now free~%"
          (aref fs:put-usage-array fs:put-free))
  (format t "; Warnings happen every ~D minutes if space gets under ~D blocks~%"
          fs:*disk-space-warner-interval*
          fs:*disk-space-warner-threshold*)
  (fs:start-disk-space-warner-process))

(login-setq gc:*report-volatility* 3)
