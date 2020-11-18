;;; -*- Mode:LISP; Package:TV; Base:8; Readtable:ZL -*-
;;;     ** (c) Copyright 1980 Massachusetts Institute of Technology **

;This file is loaded after the kernel of the window system and
;before any windows are instantiated.  It contains the combined
;methods and such.

;WINDOW because those methods will get shared then, and
;because it may even be instantiated itself.  Plus all the
;types of windows used in code loaded up til now.
(COMPILE-FLAVOR-METHODS WHO-LINE-SCREEN WHO-LINE-SHEET WHO-LINE-FILE-SHEET WHO-LINE-WINDOW
                        WINDOW LISP-LISTENER LISP-INTERACTOR BACKGROUND-LISP-INTERACTOR
                        POP-UP-TEXT-WINDOW POP-UP-NOTIFICATION-WINDOW
                        TRUNCATING-POP-UP-TEXT-WINDOW
                        TRUNCATING-POP-UP-TEXT-WINDOW-WITH-RESET)

(add-system-key #/L 'LISTENER-MIXIN "Lisp" 'LISP-LISTENER)

(ADD-INITIALIZATION "Sheet" '(INITIALIZE) '(:ONCE))
(ADD-INITIALIZATION "Mouse" '(MOUSE-INITIALIZE) '(:WARM :FIRST))

;Later modules generally have their own COMPILE-FLAVOR-METHODS at the
;end of their own file.  This file exists for bootstrapping reasons.

;Mustn't create any windows (including doing any DEFWINDOW-RESOURCEs) until
;here, because flavors aren't compiled yet and TV:DEFAULT-SCREEN isn't set up yet.

(DEFRESOURCE BACKGROUND-LISP-INTERACTORS ()
  :CONSTRUCTOR (MAKE-WINDOW 'BACKGROUND-LISP-INTERACTOR
                            ':PROCESS CURRENT-PROCESS   ;will be set later
                            ':SUPERIOR DEFAULT-SCREEN   ;always on this screen
                            ':HEIGHT (TRUNCATE (SHEET-HEIGHT DEFAULT-SCREEN) 3)))

; It is now time to initialize the window system, which will create and expose
; the initial-lisp-listener and turn on blinkers.
(ADD-INITIALIZATION "WINDOW" '(WINDOW-INITIALIZE) '(:SYSTEM))

;Resource of general-purpose momentary menus
(DEFWINDOW-RESOURCE MOMENTARY-MENU ()
        :MAKE-WINDOW (MOMENTARY-MENU)
        :REUSABLE-WHEN :DEEXPOSED)

;Windows gotten from here are not initialized as to their size or position.
; before trying to expose them, you had better give that attention.
; WINDOW-CALL and WINDOW-MOUSE-CALL will resize the window enuf to avoid an error, at least.
(DEFWINDOW-RESOURCE POP-UP-FINGER-WINDOW ()
  :MAKE-WINDOW (TRUNCATING-POP-UP-TEXT-WINDOW-WITH-RESET)
  :REUSABLE-WHEN :DEACTIVATED
;  :initializer (when (or ( (send object :width) (send tv:main-screen :inside-width))
;                        ( (send object :height) (send tv:main-screen :inside-height))
;                        ( (send object :x-offset) 0)
;                        ( (send object :y-offset) 0))
;                (send object :change-of-size-or-margins
;                      :left 0
;                      :top 0
;                      :width (send tv:main-screen :inside-width)
;                      :height (send tv:main-screen :inside-height)))
                 )

(DEFWINDOW-RESOURCE POP-UP-NOTIFICATION-WINDOW ()
  :MAKE-WINDOW (POP-UP-NOTIFICATION-WINDOW)
  :REUSABLE-WHEN :DEACTIVATED)
