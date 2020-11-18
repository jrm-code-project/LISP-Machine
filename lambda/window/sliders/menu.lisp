;;; -*- Mode:LISP; Package:OBIE; Base:10; Readtable:CL -*-

; A menu is just a collection of items (icons or whatever)
(defobclass menu (window-rect)
  items)

(defobclass menu-item (mouse-highlighting-rect)
  value)

(defobclass text-menu-item (text-icon menu-item))

(defobfun (mouse-click menu-item) (char x y)
  (and (point-in-region (- x (tv:sheet-left-margin-size window)) (- y (tv:sheet-top-margin-size window)))
       value))

(defobclass char-menu-item (menu-item char-icon))      ;flush this?

(defobfun (draw menu) ()
  (draw-border)
  (dolist (item items)
    (ask item (draw))))

(defobfun menu-test ()
  (setq window (make-instance 'obie-window :edges-from :mouse :expose-p t :borders 10))
  (setq char-menu (oneof menu 'x 0 'y 0 'height 100 'width 100 'window window))
  (dotimes (char-index 26)
    (setq item (oneof char-menu-item 'x (rem (* char-index 15) 100) 'y (* 15 (floor (* char-index 15) 100)) 'window window
                      'image-char (aref "ABCDEFGHIJKLMNOPQRSTUVWXYZ" char-index) 'font fonts:cptfont))
    (ask char-menu
      (push item items))
    (send window :add-object item t))
  (ask char-menu (draw)))
