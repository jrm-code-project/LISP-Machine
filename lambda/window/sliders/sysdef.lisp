;;; -*- Mode:LISP; Package:USER; Base:10 -*-

(defpackage obie
  (:use OBJ GLOBAL)
  ;; All these have to be documented
  (:export  ;; PRIMS
    "DEFOBCLASS" "MAPCAN-ASK" "PULL" "DISPLAY-OBJECT" "POINT" "RECT" "WINDOW-POINT"
    "WINDOW-RECT" "MOUSE-HIGHLIGHTING-RECT"
    "X" "Y" "HEIGHT" "WIDTH" "WINDOW" "OBJ-UNDER-POINT" "OBJ-UNDER-MOUSE"
    "MOVE" "POINT-IN-REGION" "DRAW" "OBJ-CLASSP" "ERASE" "DISAPPEAR"
    ;; ICON
    "ICON" "TEXT-ICON" "ARRAY-ICON" "DYNAMIC-ARRAY-ICON" "DYNAMIC-TEXT-ICON"
    "IMAGE-CHAR" "BORDERED-P"  "TEXT"           ; "FONT"
    "PORTED-ICON" "EXPOSE-PORTS" "HIDE-PORTS" "PORTS"
    ;; LINE
    "LINE" "PORT0" "PORT1" "LINES" "ADD-LINE" "REMOVE-LINE" "OTHER-PORT"
    ;; MENU, MOUSE, WINDOW
    "MENU-ITEM" "TEXT-MENU-ITEM" "VALUE" "MOUSE-CLICK" "WINDOW" "MOUSEABLE-P"
    "ADD-OBJECT" "DRAG" "OBIE-WINDOW" "GHOST-WINDOW" "TV-WINDOW" "OBJECTS" "PORT" "OWNER"
    ;; TOOL
    "TOOL" "TOOL-MANAGER" "TOOLBOX-MENU-ICON" "ICON-CLASS"
    "MOUSE-CHAR" "MOUSE-FONT" "MOUSE-HOT-X" "MOUSE-HOT-Y"       ;these are temp
    "MANAGER" "PICK-UP" "PUT-DOWN"))

(defsystem obie
  (:pathname-default "sys:window;sliders;")
  (:package "OBIE")
  (:module prims "PRIMS")
  (:module icon "ICON")
  (:module mouse "MOUSE")
  (:module window "window")
  (:module menu "menu")
  (:module line "line")
  (:module tool "tool")
  (:module slider "SLIDER")
  (:compile-load prims)
  (:compile-load icon (:fasload prims))
  (:compile-load line (:fasload prims))
  (:compile-load window (:fasload prims))
  (:compile-load mouse (:fasload prims window icon))
  (:compile-load menu (:fasload prims mouse window  icon))    ;Try it this way
  (:compile-load tool (:fasload prims icon window menu)))

; Sliders
(defsystem sliders
  (:pathname-default "sys:window;sliders;")
  (:package "OBIE")
  (:module prims "PRIMS")
  (:module slider "SLIDER")
  (:compile-load prims)
  (:compile-load slider (:fasload prims)))

(defun load-sliders-silently ()
  (let ((fs:inhibit-fdefine-warnings t))
    (make-system 'sliders :compile :noconfirm)))
