;;; -*- Mode:LISP; Package:HACKS; Lowercase:T; Base:8; Readtable:ZL -*-

(defflavor rdm-window ()
           (tv:borders-mixin tv:bottom-box-label-mixin tv:window)
  (:default-init-plist :blinker-p nil :more-p nil :label ""))

(defmethod (rdm-window :before :select) (&rest ignore)
  (funcall-self ':clear-input)
  (funcall-self ':set-label ""))

(compile-flavor-methods rdm-window)

(load "sys:demo;elp-array")

(defvar *rdm-window* nil)

(defun rdm-window nil   ;reduce size of image containing this, make window if needed.
  (cond (*rdm-window*)
        (t (setq *rdm-window
                 (tv:window-create 'rdm-window
                                   ':position '(63. 63.)
                                   ':inside-size '(513. 513.))))))

(defmacro multiple-of-32 (number)
  `(* (ceiling ,number 32.) 32.))


(defun fast-fredkin (&OPTIONAL (middle-toggle nil) (window (rdm-window))(pattern elp-array))
  (tv:window-call (window :deactivate)
    (multiple-value-bind (beginning-x beginning-y ending-x ending-y)
        (send window ':inside-edges)
      (let* ((width (- ending-x beginning-x))
             (height (- ending-y beginning-y))
             (first-alu (if middle-toggle tv:alu-seta tv:alu-setz))
             (copy-array (make-pixel-array (multiple-of-32 (1+ width))
                                           (multiple-of-32 (1+ height))
                                           ':type ':art-1b))
             (screen-array (send window ':screen-array)))
        (send window :set-label "FREDKIN")
        (send window :clear-window)
        (send window :bitblt
              tv:alu-seta (array-dimension pattern 1) (array-dimension pattern 0) pattern 0 0 0 0)
        (process-sleep 40.)
        (do () ((send window :tyi-no-hang))
          (bitblt first-alu width height screen-array beginning-x beginning-y copy-array 1 1)
          (bitblt tv:alu-xor width height screen-array beginning-x beginning-y copy-array 2 1)
          (bitblt tv:alu-xor width height screen-array beginning-x beginning-y copy-array 0 1)
          (bitblt tv:alu-xor width height screen-array beginning-x beginning-y copy-array 1 0)
          (bitblt tv:alu-xor width height screen-array beginning-x beginning-y copy-array 1 2)
          (bitblt tv:alu-seta width height copy-array 1 1 screen-array beginning-x beginning-y))))))

(defdemo "Fredkin" "This implements the famous FREDKIN algorithm, which, for each pixel on a screen, XOR's the pixel
to the right, the pixel to the left, the pixel above, the pixel below, an optionally the pixel itself all together to
obtain the new pixel value" (fast-fredkin t))
