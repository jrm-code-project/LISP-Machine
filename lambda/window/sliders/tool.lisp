;;; -*- Mode:LISP; Package:OBIE; Base:10 -*-
;;; Tools

#|
A tool is a metaphor for a mode.  A user picks a tool from a toolbox menu and
uses it to perform some operation on display (usually on a specific objects).
Picking up a tool changes the mouse icon and the mouse sensitivity: only objects
that the tool can act on are sensitive.

To make the interface more flexible, the user can mouse on an object and get a
pop-up menu of tools.  This corresponds to a noun-verb grammer rather than the
verb-noun grammer of modes.

Implementation:  a tool is an object...
|#

(defobclass tool ()
  manager                                              ;The owner of this tool
  (active? nil)
  object-class                                         ;The class of objects this tool can act on.
  icon-class                                           ;The general icon class for the tool
;  mouse-icon                                          ;An instance for mouse blinker
  mouse-char                                           ;Temp until icons can be mouse blinkers
  (mouse-font fonts:mouse)
  (mouse-hot-x 0)
  (mouse-hot-y 0)
  menu-icon)                                           ;An instance for a toolbox menu


(defobclass tool-menu-item (menu-item)
  tool
  (value '(frobnitz)))

; This returns an icon appropriate for a menu
(defobfun (toolbox-menu-icon tool) (&rest args)
  (apply #'oneof (list tool-menu-item icon-class) 'tool obj:*object args))

(defobfun (tool-mouse-icon tool) (&rest args)
  (apply #'oneof (list dynamic-icon icon-class) args))

(defobfun (mouse-click tool-menu-item) (char x y)
  (ask tool (pick-up)))

(defobfun (pick-up tool) ()
  (ask-funcall manager 'picked-up (current-obj))
;  (unless mouse-icon                                  ;init mouse-icon
;    (setq mouse-icon (tool-mouse-icon 'window (ask manager tv-window) 'x (ask menu-icon x) 'y (ask menu-icon y))))
;  (ask mouse-icon (be-the-mouse-blinker))
  (tv:mouse-set-blinker-definition :character mouse-hot-x mouse-hot-y :on :set-character mouse-char mouse-font)
  ;; set mouse-sensitivity list
  ;; highlight the menu icon
  )

(defobfun (put-down tool) ()
  )

; This is the default handler for mouse-clicks when the tool is picked up.
(defobfun (mouse-click tool) (char object)
  (ask-funcall object 'mouse-click char class-name))



; An application-level window can have this mixed into itself.
(defobclass tool-manager (object-managing-window)
  (tools nil)
  (held-tool nil))

(defobfun (picked-up tool-manager) (tool)
  (unless (eq tool held-tool)
    (when held-tool (ask held-tool (put-down)))
    (setq held-tool tool)
    (setq mouse-char (ask tool mouse-char)
          mouse-font (ask tool mouse-font)
          mouse-hot-x (ask tool mouse-hot-x)
          mouse-hot-y (ask tool mouse-hot-y))))

; If a tool is held, pass call to it, else let window handle it
(defobfun (mouse-click tool-manager) (char x y)
  (or (and held-tool
           (dolist (obj mouseable-objects)
             (when (and (obj-classp (ask held-tool object-class) obj)  ;+++ take ask out of loop
                        (ask-funcall obj 'point-in-region x y))
               (ask-funcall held-tool 'mouse-click char obj)
               (return t))))
      (shadowed-mouse-click char x y)))
