;;; -*- Mode:LISP; Package:TV; Base:10; Readtable:ZL -*-
;;; Pops up a window with a user-supplied prompt in it, returning the character the user
;;; types in to make it disappear...Do not use the mouse to deexpose this, however...
(defun pop-up-prompt (prompt &optional (sup tv:mouse-sheet) (pop-up-near '(:mouse)))
  (let ((message (string-append prompt " ")))
    (let ((pop-up-message-window (make-instance 'tv:pop-up-text-window :superior sup)))
      (send pop-up-message-window :set-label nil)
      (send pop-up-message-window :set-size-in-characters message message)
      (send pop-up-message-window :clear-input)
      (tv:expose-window-near pop-up-message-window pop-up-near nil)
      (tv:window-call (pop-up-message-window :deactivate)
        (send pop-up-message-window :string-out message)
        ;(send pop-up-message-window :tyo #\space)
        ;; Back up the cursor by one.  This is easier than trying to make the window
        ;; come out wider, because of the interface to :set-size-in-characters.
        (multiple-value-bind (x-pos y-pos)
            (send pop-up-message-window :read-cursorpos :character)
        (send pop-up-message-window :set-cursorpos (1- x-pos) y-pos :character))
        (let ((response (send pop-up-message-window :any-tyi)))
          (typecase response
                 (list response)
                 ((or number character)
                  (send pop-up-message-window :tyo response)
                  (tv:blinker-set-visibility (first (last (send pop-up-message-window :blinker-list))) nil)
                  (sleep 0.5)
                  (character response))
                 (t (beep))))))))
