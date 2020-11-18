;;; -*- Mode:LISP; Fonts:(5X5) -*-
(defun drf (win)
  (send win ':clear-screen)
  (loop for i from 1 to 100
        as wid = (send win ':width)
        as hit = (send win ':height)
        as rad = (// (min wid hit) 15)
        do (send win ':draw-circle
                 (random wid)
                 (random hit)
                 (random rad))
        ))

(defun cruft-up-frame
       (&optional
        (frame
          (send terminal-io
                ':superior)))
  (loop for pane in (send
                      frame
                      ':exposed-inferiors)

        when (send pane
                   ':operation-handled-p
                   ':draw-circle)
        do (drf pane)
        ))

(defun clear-up-frame
       (&optional
        (frame
          (send terminal-io
                ':superior)))
  (loop for pane in (send
                      frame
                      ':exposed-inferiors)

        when (send pane
                   ':operation-handled-p
                   ':clear-screen)
        do (send pane ':clear-screen)
        do (send pane ':refresh)
        ))
