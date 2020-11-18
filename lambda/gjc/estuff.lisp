;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:8; Readtable:ZL -*-


(defun enable-keyboard-ints ()
  (without-interrupts
    (initialize-wired-kbd-buffer)
    ;;write vector
    (%nubus-write #xf5 #xf00018 #xf6e00034)
    ;;initialize 8251
    (%nubus-write #xf5 #xFC0000 0)
    (%nubus-write #xf5 #xFC0000 0)
    (%nubus-write #xf5 #xFC0000 0)
    (%nubus-write #xf5 #xFC0000 #x40)
    (%nubus-write #xf5 #xFC0000 #x+7F)
    (%nubus-write #xf5 #xFC0000 #x+14)
    ;;enable keyboard ints
    ;; (apparently controlled by 8251 initialization above)
    ;;enable SI bus master
    (%nubus-write #xf5 #xf00040 2)
    (setq use-keyboard-interrupts t)))


(defun FOO-enable-keyboard-ints ()
  (without-interrupts
    (initialize-wired-kbd-buffer)
    ;;write vector
    (%nubus-write #xf5 #xf00018 #xf6e00034)
    ;;initialize 8251
    (%nubus-write #xf5 #xFC0000 0)
    (%nubus-write #xf5 #xFC0000 0)
    (%nubus-write #xf5 #xFC0000 0)
    (%nubus-write #xf5 #xFC0000 #x40)
    (%nubus-write #xf5 #xFC0000 #x7F)
    (dotimes (i 100))
    (%nubus-write #xf5 #xFC0000 #x14)           ;was #x14
    (dotimes (i 100))
    ;;enable keyboard ints
    ;; (apparently controlled by 8251 initialization above)
    ;;enable SI bus master
    (%nubus-write #xf5 #xf00040 2)
    (setq use-keyboard-interrupts t)))
