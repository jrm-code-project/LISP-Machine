;;; -*- Mode:LISP; Package:ZWEI; Fonts:(CPTFONT CPTFONTB); Base:10; Readtable:ZL -*-

;;; Copyright (C) Lisp Machine, Inc. 1984, 1985, 1986
;;;   See filename "Copyright" for
;;; licensing and release information.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                      ;;;
;;;                                                                      ;;;
;;;                           GLOBAL VARIABLES                           ;;;
;;;                                                                      ;;;
;;;                                                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Controls the width of the shadow borders around the DIRED frame panes.
(defvar *D-SHADOW-WIDTH* 10)

;;; A DIRED command whose name was given literally in a menu item.
(defvar *DIRED-COMMAND* nil)

;;; 'LANDSCAPE on a Landscape terminal; 'PORTRAIT on a Portrait.
(defvar *DIRED-TERMINAL-TYPE* nil)

;;; The window on display before entry into DIRED.
(defvar *PREVIOUS-WINDOW* nil)

;;; Flag variable to let DIRED know to abort/exit a DIRED frame, not just
;;; a DIRED buffer.
(defvar *MENU-DRIVEN-DIRED* t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                      ;;;
;;;                                                                      ;;;
;;;                    CONSTRAINT-FRAME SHADOW BORDERS                   ;;;
;;;                                                                      ;;;
;;;                                                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; These routines draw the various shadow borders used by the panes in
;;; the DIRED constraint frame.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; D-DRAW-LEFT-SHADOW-BORDER
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defprop D-DRAW-LEFT-SHADOW-BORDER 10  tv:default-border-size)

(defun D-DRAW-LEFT-SHADOW-BORDER  (window alu left top right bottom)
  (let ((width (- right left))
        (height (- bottom top))
        )
    (tv:%draw-rectangle 1                       ;width
                        (- height width)        ;height
                        (- width 1)             ;x
                        0                       ;y
                        alu
                        window)
    (d-draw-gray-rectangle (- width 5)          ;width
                         (- height width)       ;height
                         5
                         (- width 5)
                         tv:alu-ior
                         window
                         :gray 50)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; D-DRAW-LEFT-SHADOW-BORDER-PLUS
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defprop D-DRAW-LEFT-SHADOW-BORDER-PLUS 10  tv:default-border-size)

(defun D-DRAW-LEFT-SHADOW-BORDER-PLUS  (window alu left top right bottom)
  (let ((width (- right left))
        (height (- bottom top))
        )
    (tv:%draw-rectangle 1                       ;width
                        (- height width 5)      ;height
                        (- width 1)             ;x
                        5                       ;y
                        alu
                        window)
    (d-draw-gray-rectangle (- width 5)          ;width
                         (- height width 5)     ;height
                         5
                         width
                         tv:alu-ior
                         window
                         :gray 50)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; D-DRAW-RIGHT-SHADOW-BORDER
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defprop D-DRAW-RIGHT-SHADOW-BORDER 1 tv:default-border-size)

(defun D-DRAW-RIGHT-SHADOW-BORDER  (window alu left top right bottom)
  (let ((width (- right left))
        (height (- bottom top))
        )
    (tv:%draw-rectangle width                   ;width
                        (- height *d-shadow-width*)     ;height
                        left                    ;x
                        top                     ;y
                        alu
                        window)
    ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; D-DRAW-RIGHT-SHADOW-BORDER-PLUS
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defprop D-DRAW-RIGHT-SHADOW-BORDER-PLUS 6 tv:default-border-size)

(defun D-DRAW-RIGHT-SHADOW-BORDER-PLUS  (window alu left top ignore bottom)
  (let ((height (- bottom top))
        )
    (tv:%draw-rectangle 1                       ;width
                        (- height *d-shadow-width*)     ;height
                        left                    ;x
                        top                     ;y
                        alu
                        window)
    ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; D-DRAW-TOP-SHADOW-BORDER
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defprop D-DRAW-TOP-SHADOW-BORDER 1  tv:default-border-size)

(defun D-DRAW-TOP-SHADOW-BORDER  (window alu left top right ignore)
  (let ((width (- right left)))                 ;
    (tv:%draw-rectangle (- width 5)             ;width
                        1                       ;height
                        left                    ;x
                        top                     ;y
                        alu
                        window)
    ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; D-DRAW-TOP-SHADOW-BORDER-PLUS
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defprop D-DRAW-TOP-SHADOW-BORDER-PLUS 6  tv:default-border-size)

(defun D-DRAW-TOP-SHADOW-BORDER-PLUS  (window alu left ignore right bottom)
  (let ((width (- right left)))                 ;
    (tv:%draw-rectangle (- width 5)             ;width
                        1                       ;height
                        left                    ;x
                        (- bottom 1)            ;y
                        alu
                        window)
    ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; D-DRAW-BOTTOM-SHADOW-BORDER
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defprop D-DRAW-BOTTOM-SHADOW-BORDER 10  tv:default-border-size)

(defun D-DRAW-BOTTOM-SHADOW-BORDER  (window alu left top right bottom)
  (let ((width (- right left))                  ;
        (height (- bottom top))
        )

    (tv:%draw-rectangle width                   ;width
                        1                       ;height
                        left                    ;x
                        top                     ;y
                        alu
                        window)
    (d-draw-gray-rectangle (- width  height -5) ;width
                         (- height 5)           ;height
                         left                   ;x
                         top                    ;y
                         tv:alu-ior
                         window
                         :gray 50)
    ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; D-DRAW-50%-GRAY-RECTANGULAR-BORDER
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defprop D-DRAW-50%-GRAY-RECTANGULAR-BORDER 10 tv:default-border-size)

(defun D-DRAW-50%-GRAY-RECTANGULAR-BORDER (window ignore left top right bottom)
  (d-draw-gray-rectangle (- right left)         ;width
                       (- bottom top)           ;height
                       left
                       top
                       tv:alu-ior
                       window
                       :gray 50))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; D-DRAW-GRAY-RECTANGLE
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun D-DRAW-GRAY-RECTANGLE (width height x y alu window
                            &optional &key
                            (gray 50))
  (let ((gray-array (selectq gray
                      (12 tv:12%-gray)
                      (25 tv:25%-gray)
                      (33 tv:33%-gray)
                      (50 tv:50%-gray)
                      (75 tv:75%-gray)
                      (t tv:50%-gray))))

    (bitblt                                     ;operation
      alu                                       ;alu
      width                                     ;width
      height                                    ;height
      gray-array                                ;from-array
      0 0                                       ;from-x, from-y
      (send window :screen-array)               ;to-array
      x y)))                                    ;to-x , to-y



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                      ;;;
;;;                                                                      ;;;
;;;                           CONSTRAINT FRAMES                          ;;;
;;;                                                                      ;;;
;;;                                                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; GLOBALS FOR REFERRING TO CONSTRAINT FRAME PANES
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar *DIRED-CONSTRAINT-FRAME* nil      "Frame for menu-driven DIRED")
(defvar *DIRED-COMMAND-PANE* nil          "Displays the DIRED Command Menu")
(defvar *DIRED-HERALD-PANE* nil           "Displays the current herald")
(defvar *DIRED-PATHNAME-PANE* nil         "Displays the name of the current directory")

(defvar *DIRED-DISPLAY-FRAME* nil         "Subframe for DIRED or DEBUG display")
(defvar *DIRED-DISPLAY-PANE* nil          "Displays a DIRED buffer")
(defvar *DIRED-DATA-PANE* nil             "Displays data on current file descriptor")
(defvar *DIRED-DELTA-PANE* nil            "Menu for changing current file descriptor")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; DIRED-COMMAND-MENU-MIXIN
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A dummy mixin used to include the (DIRED-COMMAND-MENU-MIXIN :AFTER :MOUSE-BUTTONS)
;;; method in DIRED-COMMAND-MENU.

(defflavor DIRED-COMMAND-MENU-MIXIN (io-buffer) ()
  (:required-flavors tv:basic-menu)
  (:settable-instance-variables io-buffer))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; (DIRED-COMMAND-MENU-MIXIN :AFTER :MOUSE-BUTTONS)
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This method provides the link between the mouse process that creates blips when
;;; a DIRED Command Menu item is chosen, and the Zmacs process (running as part of DIRED)
;;; that executes them. It takes the blip produced by the mouse process, extracts the
;;; DIRED command it contains (the car of its third), and tells Zmacs what to do by
;;; putting the command into the Zmacs io buffer. Without this method, Zmacs would get
;;; the whole blip, which it could not interpret. A special case occurs when the command
;;; will cause a popup to be displayed from which a second command will be selected. In
;;; this case, the routine that will display the popup is given in the cdr of the blip's
;;; third which gets saved; the car of the third is a command directing the routine in
;;; the saved location to be funcalled.

(defmethod (DIRED-COMMAND-MENU-MIXIN :AFTER :MOUSE-BUTTONS)
           (&rest ignore)
  (let ((blip tv:chosen-item))
    (when (typep (third blip) 'cons)
      (setq *dired-command* (cdr (third tv:chosen-item)))
      (tv:io-buffer-put
        (send *dired-display-pane* :io-buffer)
        (car (third tv:chosen-item))))
    (setq tv:chosen-item nil)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; DIRED-COMMAND-MENU
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; TV:MENU with the mixin that provides the
;;; (DIRED-COMMAND-MENU-MIXIN :AFTER :MOUSE-BUTTONS)
;;; method in place of the (TV:MENU :AFTER :MOUSE-BUTTONS) method.

(defflavor DIRED-COMMAND-MENU ()
           (dired-command-menu-mixin tv:menu))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; STACKED-CHOOSE-VARIABLE-VALUES-PANE
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; TV:CHOOSE-VARIABLE-VALUES-PANE with an :after :init that lets you
;;; initialize its stack frame (via an :after :init). You're supposed to
;;; be able to give a :stack-frame init keyword, but that doesn't seem
;;; to work.


(defflavor STACKED-CHOOSE-VARIABLE-VALUES-PANE ()
           (tv:choose-variable-values-pane))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; (STACKED-CHOOSE-VARIABLE-VALUES-PANE :AFTER :INIT)
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Method to give STACKED-CHOOSE-VARIABLE-VALUES-PANE a stack frame.

(defmethod (STACKED-CHOOSE-VARIABLE-VALUES-PANE :AFTER :INIT) (&rest ignore)
           (setq stack-group tv:%current-stack-group))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; (STACKED-CHOOSE-VARIABLE-VALUES-PANE :UPDATE)
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; A more modularly accessible version of (TV:BASIC-CHOOSE-VARIABLE-VALUES :SETUP)

(defmethod (STACKED-CHOOSE-VARIABLE-VALUES-PANE :UPDATE)
           (&key ((:elems new-elems)) ((:label new-label)) ((:function new-function))
            ((:margin-choices new-margin-choices)) ((:width new-width))
            ((:extra-width new-extra-width)))
  (cond-every
    (new-function (setq function new-function))
    (t (setq stack-group tv:%current-stack-group))
    (t (setf (tv:io-buffer-last-output-process tv:io-buffer) current-process))
    (new-label (send self :set-label new-label))
    (new-margin-choices (send self :set-margin-choices new-margin-choices))
    (new-elems (send self :set-variables new-elems nil new-width new-extra-width))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; SCROLLABLE-DISPLAY-PANE
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defflavor SCROLLABLE-DISPLAY-PANE
         ()
         (tv:borders-mixin
          tv:scroll-stuff-on-off-mixin
          tv:scroll-window
          ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; DIRED-DISPLAY-FRAME
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defflavor DIRED-DISPLAY-FRAME ()
         (tv:constraint-frame)
         (:default-init-plist
          :panes
          '(
            (display-pane zwei:zmacs-frame
                          :borders
                          (d-draw-left-shadow-border-plus
                            d-draw-top-shadow-border-plus
                            d-draw-right-shadow-border-plus
                            d-draw-bottom-shadow-border
                            )
                          :blinker-deselected-visibility
                          :blink
                          :blinker-flavor
                          tv:rectangular-blinker
                          :blinker-p
                          nil
                          :deexposed-typein-action
                          :normal
                          :deexposed-typeout-action
                          :permit
                          :save-bits
                          t)
            (data-pane scrollable-display-pane
                        :borders
                        (d-draw-left-shadow-border-plus
                          d-draw-top-shadow-border-plus
                          d-draw-right-shadow-border-plus
                          d-draw-bottom-shadow-border
                          )
                        :deexposed-typein-action
                        :normal
                        :deexposed-typeout-action
                        :permit
                        :label
                        nil
                        :save-bits
                        t)
            (delta-pane stacked-choose-variable-values-pane
                        :borders
                        (d-draw-left-shadow-border-plus
                          d-draw-top-shadow-border-plus
                          d-draw-right-shadow-border-plus
                          d-draw-bottom-shadow-border
                          )
                        :margin-choices  (("Do It" nil fsdebug-do-it nil nil)
                                          ("Reset" nil fsdebug-reset nil nil))
                        :deexposed-typein-action
                        :normal
                        :deexposed-typeout-action
                        :permit
                        :label
                         nil
;                        :item-list
;                        (("" :no-select nil))
                        :save-bits
                        t)
            )
           :constraints
           '(
             (dired-configuration (:whole)
                    ((:whole :horizontal
                                         (:even)
                                         (display-pane)
                                         ((display-pane :even)))))
             (debug-configuration (display-pane dummy-name4)
                                  ((display-pane 0.76789s0))
                                  ((dummy-name4 :horizontal
                                                (:even)
                                                (delta-pane data-pane)
                                                ((delta-pane 0.5s0))
                                                ((data-pane :even)))))
             ))
  :gettable-instance-variables
  :settable-instance-variables
  :inittable-instance-variables)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (DIRED-DISPLAY-FRAME :AFTER :INIT)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Initializes a newly instantiated DIRED DISPLAY Frame.

(defmethod (DIRED-DISPLAY-FRAME :AFTER :INIT)
           (&rest ignore)
  (setq
    *dired-display-frame* self
    *dired-display-pane* (send self :get-pane 'display-pane)
    *dired-data-pane* (send self :get-pane 'data-pane)
    *dired-delta-pane* (send self :get-pane 'delta-pane)
    *dired-terminal-type* (if (> (send tv:main-screen :size) 900) 'landscape 'portrait))

  (send self :set-selection-substitute *dired-display-pane*)
  (send *dired-data-pane* :set-selection-substitute *dired-display-pane*))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; DIRED-CONSTRAINT-FRAME
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defflavor DIRED-CONSTRAINT-FRAME ()
           (tv:constraint-frame)
  (:default-init-plist
    :panes
    '(
      (display-frame dired-display-frame)
      (command-pane dired-command-menu
                    :borders
                    (d-draw-left-shadow-border-plus
                      d-draw-top-shadow-border-plus
                      d-draw-right-shadow-border-plus
                      d-draw-bottom-shadow-border
                      )
                    :deexposed-typein-action
                    :normal
                    :deexposed-typeout-action
                    :permit
                    :label
                    nil
                    :save-bits
                    t
                    :item-list
                    (("" :no-select nil)))
      (herald-pane dired-command-menu
                   :borders
                   (d-draw-left-shadow-border-plus
                     d-draw-top-shadow-border-plus
                     d-draw-right-shadow-border-plus
                     d-draw-bottom-shadow-border
                     )
                   :deexposed-typein-action
                   :normal
                   :deexposed-typeout-action
                   :permit
                   :label
                   nil
                   :item-list
                   (("" :no-select nil))
                   :save-bits
                   t)
      (pathname-pane dired-command-menu
                     :borders
                     (d-draw-left-shadow-border-plus
                       d-draw-top-shadow-border-plus
                       d-draw-right-shadow-border-plus
                       d-draw-bottom-shadow-border
                       )
                     :deexposed-typein-action
                     :normal
                     :deexposed-typeout-action
                     :permit
                     :label
                     nil
                     :item-list
                     (("" :no-select nil))
                     :save-bits
                     t)
      )
    :constraints
    '(
      (four-pane-configuration
        (:whole)
        ((:whole :horizontal
                             (:even)
                             (dummy-name4 dummy-name7)
                             ((dummy-name4 :vertical
                                           (0.10s0)
                                           (herald-pane command-pane)
                                           ((herald-pane 0.059s0))
                                           ((command-pane :even))))
                             ((dummy-name7 :vertical
                                           (:even)
                                           (pathname-pane display-frame)
                                           ((pathname-pane 0.059s0))
                                           ((display-frame :even)))))))

      (standard-configuration (:whole)
                              ((:whole :horizontal
                                                   (:even)
                                                   (display-frame)
                                                   ((display-frame :even)))))

      ))
  :gettable-instance-variables
  :settable-instance-variables
  :inittable-instance-variables)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (DIRED-CONSTRAINT-FRAME :AFTER :INIT)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Initializes a newly instantiated DIRED Constraint Frame.

(defmethod (DIRED-CONSTRAINT-FRAME :AFTER :INIT)
           (&rest ignore)
  (setq
    *dired-constraint-frame* self
    *dired-command-pane* (send self :get-pane 'command-pane)
    *dired-herald-pane* (send self :get-pane 'herald-pane)
    *dired-pathname-pane* (send self :get-pane 'pathname-pane)
    *dired-command* nil)

  (send self :set-selection-substitute *dired-display-pane*)
  (send *dired-command-pane* :set-io-buffer (tv:make-default-io-buffer))
  (send *dired-herald-pane* :set-io-buffer (tv:make-default-io-buffer))
  (send *dired-pathname-pane* :set-io-buffer (tv:make-default-io-buffer))
  (send *dired-herald-pane* :set-item-list (dired-herald))
  (send *dired-command-pane* :set-item-list (dired-menu))

  (insure-fsdebug-process-ok))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                      ;;;
;;;                                                                      ;;;
;;;                               HERALDS                                ;;;
;;;                                                                      ;;;
;;;                                                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; These are displayed in the Herald Pane to tell the user what program and
;;; mode are active.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; DISPLAY-HERALD
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The herald which says that DIRED is active.

(defun DIRED-HERALD ()
  (list
    (list
       "DIRED"
       :no-select nil
      :font
      fonts:METSI)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                      ;;;
;;;                                                                      ;;;
;;;                          COMMAND MENU ITEMS                          ;;;
;;;                                                                      ;;;
;;;                                                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; These are the labels and items that make up the various DIRED command menus.


(defun BLANK-LINE ()
  (list
    " "
    :no-select
    nil
    :font
    'cptfontb))


(defun DEBUG-THE-CURRENT-FILE-ITEM (&optional short)
  (list
    (if short
        "Debug  "
      " Debug:   Debug the Current File         [] ")
    (if short :value :funcall)
    (if short (ncons #/)
      (cdr (assq '#/ (comtab-keyboard-array *zmacs-comtab*))))
    :font
    (if short 'cptfontb 'cptfont)
    :documentation
    " DEBUG THE CURRENT FILE."))


(defun LEAVE-DEBUG-MODE-ITEM (&optional short)
  (list
    (if short
        "Nodebug"
      " Nodebug: Leave DEBUG Mode               [] ")
    (if short :value :funcall)
    (if short (ncons #/)
      (cdr (assq '#/ (comtab-keyboard-array *zmacs-comtab*))))
    :font
    (if short 'cptfontb 'cptfont)
    :documentation
    " LEAVE DEBUG MODE; RETURN TO ORDINARY DIRED."))


(defun MOVE-TO-NEXT-FILE-ITEM (&optional short)
  (list
    (if short
        "File  "
      " File:   Move to Next File             [sp] ")
    (if short :value :funcall)
    (if short (ncons #/SP)
      (cdr (assq '#/SP (comtab-keyboard-array *mode-comtab*))))
    :font
    (if short 'cptfontb 'cptfont)
    :documentation
    " MOVE TO THE NEXT FILE."))


(defun  MOVE-TO-NEXT-UNDUMPED-FILE-ITEM (&optional short)
  (list
    (if short
        "Noback"
      " Noback: Move to Next Undumped File     [!] ")
    (if short :value :funcall)
    (if short (ncons #/!)
      (cdr (assq '#/! (comtab-keyboard-array *mode-comtab*))))
    :font
    (if short 'cptfontb 'cptfont)
    :documentation
    " MOVE TO THE NEXT FILE THAT HAS NOT BEEN BACKED UP ON TAPE."))


(defun TOGGLE-DONT-DELETE-FILE-ITEM (&optional short)
  (list
    (if short
        "D-Flag"
      " D-Flag: Toggle Don't-Delete-File       [@] ")
    (if short :value :funcall)
    (if short (ncons #/@)
      (cdr (assq '#/@ (comtab-keyboard-array *mode-comtab*))))
    :font
    (if short 'cptfontb 'cptfont)
    :documentation
    " MARK THE CURRENT FILE AS NOT DELETABLE (IF IT IS) OR DELETABLE (IF IT IS NOT)."))


(defun TOGGLE-DONT-SUPERSEDE-FILE-ITEM (&optional short)
  (list
    (if short
        "S-Flag"
      " S-Flag: Toggle Don't-Supersede-File    [#] ")
    (if short :value :funcall)
    (if short (ncons #/#)
      (cdr (assq '#/# (comtab-keyboard-array *mode-comtab*))))
    :font
    (if short 'cptfontb 'cptfont)
    :documentation
    " MARK THE CURRENT FILE AS NOT SUPERSEDABLE (IF IT IS) OR SUPERSEDABLE (IF IT IS NOT)."))


(defun TOGGLE-DONT-REAP-FILE-ITEM (&optional short)
  (list
    (if short
        "R-Flag"
      " R-Flag: Toggle Don't-Reap-File         [$] ")
    (if short :value :funcall)
    (if short (ncons #/$)
      (cdr (assq '#/$ (comtab-keyboard-array *mode-comtab*))))
    :font
    (if short 'cptfontb 'cptfont)
    :documentation
    " MARK THE CURRENT FILE AS NOT REAPABLE (IF IT IS) OR REAPABLE (IF IT IS NOT)."))


(defun CHANGE-FILE-PROPERTIES-ITEM (&optional short)
  (list
    (if short
        "Props  "
      " Props:   Change File Properties         [.] ")
    (if short :value :funcall)
    (if short (ncons #/.)
      (cdr (assq '#/. (comtab-keyboard-array *mode-comtab*))))
    :font
    (if short 'cptfontb 'cptfont)
    :documentation
    " CHANGE THE PROPERITES OF THE CURRENT FILE."))


(defun DISPLAY-ATTRIBUTES-LINE-ITEM (&optional short)
  (list
    (if short
        "Attribs"
      " Attribs: Display Attributes Line        [,] ")
    (if short :value :funcall)
    (if short (ncons #/,)
      (cdr (assq '#/, (comtab-keyboard-array *mode-comtab*))))
    :font
    (if short 'cptfontb 'cptfont)
    :documentation
    " DISPLAY THE ATTRIBUTES LINE OF THE CURRENT FILE."))


(defun DEFAULT-SOURCE-COMPARE-ITEM (&optional short)
  (list
    (if short
        "Compare"
      " Compare: Default Source Compare         [=] ")
    (if short :value :funcall)
    (if short (ncons #/=)
      (cdr (assq '#/= (comtab-keyboard-array *mode-comtab*))))
    :font
    (if short 'cptfontb 'cptfont)
    :documentation
    " SOURCE-COMPARE THE CURRENT FILE WITH THE ITS MOST RECENT VERSION."))


(defun HELP-WITH-DIRED-ITEM (&optional short)
  (list
    (if short
        "Help   "
      " Help:    Help With DIRED                [?] ")
    (if short :value :funcall)
    (if short (ncons #/?)
      (cdr (assq '#/? (comtab-keyboard-array *mode-comtab*))))
    :font
    (if short 'cptfontb 'cptfont)
    :documentation
    " GIVE HELP WITH THE DIRECTORY EDITOR."))


(defun APPLY-FUNCTION-TO-FILE-ITEM (&optional short)
  (list
    (if short
        "Apply  "
      " Apply:   Apply Function to File         [A] ")
    (if short :value :funcall)
    (if short (ncons #/A)
      (cdr (assq '#/A (comtab-keyboard-array *mode-comtab*))))
    :font
    (if short 'cptfontb 'cptfont)
    :documentation
    " MARK THE CURRENT FILE TO HAVE A FUNCTION APPLIED TO IT ON QUIT (Q) OR EXECUTE (E)."))


(defun COPY-FILE-ITEM (&optional short)
  (list
    (if short
        "Copy   "
      " Copy:    Copy File                      [C] ")
    (if short :value :funcall)
    (if short (ncons #/C)
      (cdr (assq '#/C (comtab-keyboard-array *mode-comtab*))))
    :font
    (if short 'cptfontb 'cptfont)
    :documentation
    " COPY THE CURRENT FILE TO A LOCATION READ FROM THE MINI-BUFFER."))


(defun DELETE-FILE-ITEM (&optional short)
  (list
    (if short
        "Delete "
      " Delete:  Delete File                    [D] ")
    (if short :value :funcall)
    (if short (ncons #/D)
      (cdr (assq '#/D (comtab-keyboard-array *mode-comtab*))))
    :font
    (if short 'cptfontb 'cptfont)
    :documentation
    " MARK THE CURRENT FILE TO BE DELETED ON QUIT (Q) OR EXECUTE (E)."))


(defun EDIT-FILE-ITEM (&optional short)
  (list
    (if short
        "Edit   "
      " Edit:    Edit File                      [E] ")
    (if short :value :funcall)
    (if short (ncons #/E)
      (cdr (assq '#/E (comtab-keyboard-array *mode-comtab*))))
    :font
    (if short 'cptfontb 'cptfont)
    :documentation
    " EDIT THE CURRENT FILE."))


(defun EDIT-IN-OTHER-WINDOW-ITEM (&optional short)
  (list
    (if short
        "Edit 2 "
      " Edit 2:  Edit in Other Window      [c-sh-E] ")
    (if short :value :funcall)
    (if short (ncons #/C-SH-E)
      (cdr (assq '#/C-SH-E (comtab-keyboard-array *mode-comtab*))))
    :font
    (if short 'cptfontb 'cptfont)
    :documentation
    " EDIT THE CURRENT FILE IN A SECOND WINDOW, WHICH WILL BE CREATED IF NECESSARY."))


(defun FIND-FILE-ITEM (&optional short)
  (list
    (if short
        "Find   "
      " Find:    Find File                      [F] ")
    (if short :value :funcall)
    (if short (ncons #/F)
      (cdr (assq '#/F (comtab-keyboard-array *mode-comtab*))))
    :font
    (if short 'cptfontb 'cptfont)
    :documentation
    " MARK THE CURRENT FILE TO BE FOUND ON QUIT (Q) OR EXECUTE (E)."))


(defun DELETE-EXTRA-VERSIONS-ITEM (&optional short)
  (list
    (if short
        "Delhog "
      " Delhog:  Delete Extra Versions          [H] ")
    (if short :value :funcall)
    (if short (ncons #/H)
      (cdr (assq '#/H (comtab-keyboard-array *mode-comtab*))))
    :font
    (if short 'cptfontb 'cptfont)
    :documentation
    " MARK SUPERFLUOUS VERSIONS OF THE CURRENT FILE FOR DELETION ON QUIT (Q) OR EXECUTE (E)."))


(defun LOAD-FILE-ITEM (&optional short)
  (list
    (if short
        "Load   "
      " Load:    Load File                      [L] ")
    (if short :value :funcall)
    (if short (ncons #/L)
      (cdr (assq '#/L (comtab-keyboard-array *mode-comtab*))))
    :font
    (if short 'cptfontb 'cptfont)
    :documentation
    " LOAD THE CURRENT FILE."))


(defun MOVE-TO-NEXT-FILE-WITH-EXTRAS-ITEM (&optional short)
  (list
    (if short
        "Hog   "
      " Hog:    Move to Next File with Extras  [N] ")
    (if short :value :funcall)
    (if short (ncons #/N)
      (cdr (assq '#/N (comtab-keyboard-array *mode-comtab*))))
    :font
    (if short 'cptfontb 'cptfont)
    :documentation
    " MOVE TO THE NEXT FILE WITH SUPERFLUOUS VERSIONS."))


(defun PRINT-FILE-ITEM (&optional short)
  (list
    (if short
        "Print  "
      " Print:   Print File                     [P] ")
    (if short :value :funcall)
    (if short (ncons #/P)
      (cdr (assq '#/P (comtab-keyboard-array *mode-comtab*))))
    :font
    (if short 'cptfontb 'cptfont)
    :documentation
    " MARK THE CURRENT FILE TO BE PRINTED ON QUIT (Q) OR EXECUTE (E)."))


(defun QUIT-ITEM (&optional short)
  (list
    (if short
        "Quit   "
      " Quit:    Quit DIRED                     [Q] ")
    (if short :value :funcall)
    (if short (ncons #/Q)
      (cdr (assq '#/Q (comtab-keyboard-array *mode-comtab*))))
    :font
    (if short 'cptfontb 'cptfont)
    :documentation
    " EXECUTE COMMANDS SPECIFIED BY MARKING FILES, THEN EXIT DIRED."))


(defun RENAME-FILE-ITEM (&optional short)
  (list
    (if short
        "Rename "
      " Rename:  Rename File                    [R] ")
    (if short :value :funcall)
    (if short (ncons #/R)
      (cdr (assq '#/R (comtab-keyboard-array *mode-comtab*))))
    :font
    (if short 'cptfontb 'cptfont)
    :documentation
    " RENAME THE CURRENT FILE TO A NAME READ IN FROM THE MINI-BUFFER."))


(defun INSERT-OR-REMOVE-SUBDIRECTORY-ITEM (&optional short)
  (list
    (if short
        "Subdir"
      " Subdir: Insert or Remove Subdirectory  [S] ")
    (if short :value :funcall)
    (if short (ncons #/S)
      (cdr (assq '#/S (comtab-keyboard-array *mode-comtab*))))
    :font
    (if short 'cptfontb 'cptfont)
    :documentation
    " INSERT CURRENT SUBDIRECTORY CONTENTS (IF ABSENT) OR REMOVE THEM (IF PRESENT)."))


(defun UNMARK-FILE-ITEM (&optional short)
  (list
    (if short
        "Unmark "
      " Unmark:  Unmark File                    [U] ")
    (if short :value :funcall)
    (if short (ncons #/U)
      (cdr (assq '#/U (comtab-keyboard-array *mode-comtab*))))
    :font
    (if short 'cptfontb 'cptfont)
    :documentation
    " MARK THE CURRENT FILE FOR UNDELETION (IF IT HAS BEEN DELETED BUT NOT EXPUNGED)."))


(defun VIEW-FILE-ITEM (&optional short)
  (list
    (if short
        "View   "
      " View:    View File                      [V] ")
    (if short :value :funcall)
    (if short (ncons #/V)
      (cdr (assq '#/V (comtab-keyboard-array *mode-comtab*))))
    :font
    (if short 'cptfontb 'cptfont)
    :documentation
    " VIEW THE CURRENT FILE."))


(defun EXECUTE-COMMANDS-ITEM (&optional short)
  (list
    (if short
        "Execute"
      " Execute: Execute Commands               [X] ")
    (if short :value :funcall)
    (if short (ncons #/X)
      (cdr (assq '#/X (comtab-keyboard-array *mode-comtab*))))
    :font
    (if short 'cptfontb 'cptfont)
    :documentation
    " EXECUTE ANY COMMANDS THAT HAVE BEEN SPECIFIED BY MARKING FILES."))


(defun EDIT-SUPERIOR-DIRECTORY-ITEM (&optional short)
  (list
    (if short
        "Supdir"
      " Supdir: Edit Superior Directory        [<] ")
    (if short :value :funcall)
    (if short (ncons #/<)
      (cdr (assq '#/< (comtab-keyboard-array *mode-comtab*))))
    :font
    (if short 'cptfontb 'cptfont)
    :documentation
    " EDIT THE DIRECTORY SUPERIOR TO THE ONE NOW BEING EDITED."))


(defun  SORT-INCREASING-REFERENCE-DATE-ITEM (&optional short)
  (list
    (if short
        ">Ref   "
      " >Ref:    Sort Increasing Reference Date  [Meta-X Command] ")
    (if short :value :funcall)
    (if short '(#/h-s-m-c-d . com-dired-sort-by-increasing-reference-date)
      'com-dired-sort-by-increasing-reference-date)
    :font
    (if short 'cptfontb 'cptfont)
    :documentation
    " SORT FILES IN ORDER FROM LEAST RECENTLY TO MOST RECENTLY REFERENCED."))


(defun  SORT-DECREASING-REFERENCE-DATE-ITEM (&optional short)
  (list
    (if short
        "<Ref   "
      " <Ref:    Sort Decreasing Reference Date  [Meta-X Command] ")
    (if short :value :funcall)
    (if short '(#/h-s-m-c-d . com-dired-sort-by-decreasing-reference-date)
      'com-dired-sort-by-decreasing-reference-date)
    :font
    (if short 'cptfontb 'cptfont)
    :documentation
    " SORT FILES IN ORDER FROM MOST RECENTLY TO LEAST RECENTLY REFERENCED."))


(defun  SORT-INCREASING-CREATION-DATE-ITEM (&optional short)
  (list
    (if short
        ">Date  "
      " >Date:   Sort Increasing Creation Date   [Meta-X Command] ")
    (if short :value :funcall)
    (if short '(#/h-s-m-c-d . com-dired-sort-by-increasing-creation-date)
      'com-dired-sort-by-increasing-creation-date)
    :font
    (if short 'cptfontb 'cptfont)
    :documentation
    " SORT FILES IN ORDER FROM LEAST RECENTLY TO MOST RECENTLY CREATED."))


(defun  SORT-DECREASING-CREATION-DATE-ITEM (&optional short)
  (list
    (if short
        "<Date  "
      " <Date:   Sort Decreasing Creation Date   [Meta-X Command] ")
    (if short :value :funcall)
    (if short '(#/h-s-m-c-d . com-dired-sort-by-decreasing-creation-date)
      'com-dired-sort-by-decreasing-creation-date)
    :font
    (if short 'cptfontb 'cptfont)
    :documentation
    " SORT FILES IN ORDER FROM MOST RECENTLY TO LEAST RECENTLY CREATED."))


(defun SORT-INCREASING-FILE-NAME-ITEM (&optional short)
  (list
    (if short
        ">Name  "
      " >Name:   Sort Increasing File Name       [Meta-X Command] ")
    (if short :value :funcall)
    (if short '(#/h-s-m-c-d . com-dired-sort-by-increasing-file-name)
      'com-dired-sort-by-increasing-file-name)
    :font
    (if short 'cptfontb 'cptfont)
    :documentation
    " SORT FILES IN ALPHABETICAL ORDER BY NAME."))


(defun SORT-DECREASING-FILE-NAME-ITEM (&optional short)
  (list
    (if short
        "<Name  "
      " <Name:   Sort Decreasing File Name       [Meta-X Command] ")
    (if short :value :funcall)
    (if short '(#/h-s-m-c-d . com-dired-sort-by-decreasing-file-name)
      'com-dired-sort-by-decreasing-file-name)
    :font
    (if short 'cptfontb 'cptfont)
    :documentation
    " SORT FILES IN REVERSE ALPHABETICAL ORDER BY NAME."))


(defun SORT-INCREASING-SIZE-ITEM (&optional short)
  (list
    (if short
        ">Size  "
      " >Size:   Sort Increasing Size            [Meta-X Command] ")
    (if short :value :funcall)
    (if short '(#/h-s-m-c-d . com-dired-sort-by-increasing-size)
      'com-dired-sort-by-increasing-size)
    :font
    (if short 'cptfontb 'cptfont)
    :documentation
    " SORT FILES IN ORDER BY SIZE, FROM SMALLEST TO LARGEST."))


(defun SORT-DECREASING-SIZE-ITEM (&optional short)
  (list
    (if short
        "<Size  "
      " <Size:   Sort Decreasing Size            [Meta-X Command] ")
    (if short :value :funcall)
    (if short '(#/h-s-m-c-d . com-dired-sort-by-decreasing-size)
      'com-dired-sort-by-decreasing-size)
    :font
    (if short 'cptfontb 'cptfont)
    :documentation
    " SORT FILES IN ORDER BY SIZE, FROM LARGEST TO SMALLEST."))


(defun EXPUNGE-DIRECTORY-ITEM (&optional short)
  (list
    (if short
        "Expunge"
      " Expunge: Expunge Directory [Meta-X Command] ")
    (if short :value :funcall)
    (if short '(#/h-s-m-c-d . com-expunge-directory)
      'com-expunge-directory)
    :font
    (if short 'cptfontb 'cptfont)
    :documentation
    " EXPUNGE DELETED FILES FROM THE DIRECTORY."))


(defun MOVE-COMMANDS-LABEL (&optional short)
  (list
    (if short
        "MOVE     "
      "MOVE COMMANDS")
    (if short :value :no-select)
    (if short '(#/h-s-m-c-d . com-dired-pop-up-move-commands-menu) nil)
    :font
    'cptfontb
    :documentation
    " DISPLAY A MENU GIVING FULL NAMES AND THE KEYSTROKES FOR COMMANDS IN THIS CATEGORY."))


(defun FILE-MARK-COMMANDS-LABEL (&optional short)
  (list
    (if short
        "MARK     "
      "FILE MARK COMMANDS")
    (if short :value :no-select)
    (if short '(#/h-s-m-c-d . com-dired-pop-up-file-mark-commands-menu) nil)
    :font
    'cptfontb
    :documentation
    " DISPLAY A MENU GIVING FULL NAMES AND THE KEYSTROKES FOR COMMANDS IN THIS CATEGORY."))


(defun FILE-ATTRIBUTE-COMMANDS-LABEL (&optional short)
  (list
    (if short
        "META     "
      "FILE ATTRIBUTE COMMANDS")
    (if short :value :no-select)
    (if short '(#/h-s-m-c-d . com-dired-pop-up-file-attribute-commands-menu) nil)
    :font
    'cptfontb
    :documentation
    " DISPLAY A MENU GIVING FULL NAMES AND THE KEYSTROKES FOR COMMANDS IN THIS CATEGORY."))


(defun FILE-PROCESSING-COMMANDS-LABEL (&optional short)
  (list
    (if short
        "PROCESS  "
      "FILE PROCESSING COMMANDS")
    (if short :value :no-select)
    (if short '(#/h-s-m-c-d . com-dired-pop-up-file-processing-commands-menu) nil)
    :font
    'cptfontb
    :documentation
    " DISPLAY A MENU GIVING FULL NAMES AND THE KEYSTROKES FOR COMMANDS IN THIS CATEGORY."))


(defun SORT-COMMANDS-LABEL (&optional short)
  (list
    (if short
        "SORT     "
      "SORT-DIRECTORY COMMANDS")
    (if short :value :no-select)
    (if short '(#/h-s-m-c-d . com-dired-pop-up-sort-commands-menu) nil)
    :font
    'cptfontb
    :documentation
    " DISPLAY A MENU GIVING FULL NAMES AND THE KEYSTROKES FOR COMMANDS IN THIS CATEGORY."))


(defun MISCELLANEOUS-COMMANDS-LABEL (&optional short)
  (list
    (if short
        "MISC     "
      "MISCELLANEOUS COMMANDS")
    (if short :value :no-select)
    (if short '(#/h-s-m-c-d . com-dired-pop-up-miscellaneous-commands-menu) nil)
    :font
    'cptfontb
    :documentation
    " DISPLAY A MENU GIVING FULL NAMES AND THE KEYSTROKES FOR COMMANDS IN THIS CATEGORY."))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                      ;;;
;;;                                                                      ;;;
;;;                          STATIC COMMAND MENUS                        ;;;
;;;                                                                      ;;;
;;;                                                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; The static menu(s) used by DIRED. See also POPUP COMMAND MENUS, below.

(defun DIRED-MENU ()
   (list

     (blank-line)
     (file-processing-commands-label t)
     (edit-file-item t)
     (load-file-item t)
     (copy-file-item t)
     (rename-file-item t)
     (edit-in-other-window-item t)
     (view-file-item t)

     (blank-line)
     (file-mark-commands-label t)
     (delete-file-item t)
     (delete-extra-versions-item t)
     (find-file-item t)
     (print-file-item t)
     (apply-function-to-file-item t)
     (unmark-file-item t)

     (blank-line)
     (file-attribute-commands-label t)
     (display-attributes-line-item t)
     (change-file-properties-item t)
     (debug-the-current-file-item t)
     (leave-debug-mode-item t)
     (toggle-dont-delete-file-item t)
     (toggle-dont-supersede-file-item t)
     (toggle-dont-reap-file-item t)

     (blank-line)
     (move-commands-label t)
     (move-to-next-file-item t)
     (move-to-next-undumped-file-item t)
     (move-to-next-file-with-extras-item t)

     (blank-line)
     (sort-commands-label t)
     (sort-increasing-file-name-item t)
     (sort-increasing-creation-date-item t)
     (sort-increasing-size-item t)
     (sort-increasing-reference-date-item t)
     (sort-decreasing-file-name-item t)
     (sort-decreasing-creation-date-item t)
     (sort-decreasing-size-item t)
     (sort-decreasing-reference-date-item t)

     (blank-line)
     (miscellaneous-commands-label t)
     (insert-or-remove-subdirectory-item t)
     (edit-superior-directory-item t)
     (execute-commands-item t)
     (expunge-directory-item t)
     (quit-item t)
     (help-with-dired-item t)
     ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                      ;;;
;;;                                                                      ;;;
;;;                          POPUP COMMAND MENUS                         ;;;
;;;                                                                      ;;;
;;;                                                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; These are the popups that are used on the Portrait terminal. They have no
;;; adapting to do, because all the items adapt themselves individually.


(defun COM-DIRED-POP-UP-FILE-PROCESSING-COMMANDS-MENU ()
  (tv:menu-choose
    (list
     (blank-line)
     (file-processing-commands-label)
     (blank-line)
     (edit-file-item)
     (load-file-item)
     (copy-file-item)
     (rename-file-item)
     (edit-in-other-window-item)
     (view-file-item)
     (blank-line)))
  dis-none)


(defun COM-DIRED-POP-UP-FILE-MARK-COMMANDS-MENU ()
  (tv:menu-choose
    (list
     (blank-line)
     (file-mark-commands-label)
     (blank-line)
     (delete-file-item)
     (delete-extra-versions-item)
     (find-file-item)
     (print-file-item)
     (apply-function-to-file-item)
     (unmark-file-item)
     (blank-line)))
  dis-none)


(defun COM-DIRED-POP-UP-FILE-ATTRIBUTE-COMMANDS-MENU ()
  (tv:menu-choose
    (list
     (blank-line)
     (file-attribute-commands-label)
     (blank-line)
     (display-attributes-line-item)
     (change-file-properties-item)
     (debug-the-current-file-item)
     (leave-debug-mode-item)
     (toggle-dont-delete-file-item)
     (toggle-dont-supersede-file-item)
     (toggle-dont-reap-file-item)
     (blank-line)))
  dis-none)


(defun COM-DIRED-POP-UP-MOVE-COMMANDS-MENU ()
  (tv:menu-choose
    (list
     (blank-line)
     (move-commands-label)
     (blank-line)
     (move-to-next-file-item)
     (move-to-next-undumped-file-item)
     (move-to-next-file-with-extras-item)
     (blank-line)))
  dis-none)


(defun COM-DIRED-POP-UP-SORT-COMMANDS-MENU ()
  (tv:menu-choose
    (list
     (blank-line)
     (sort-commands-label)
     (blank-line)
     (sort-increasing-file-name-item)
     (sort-increasing-creation-date-item)
     (sort-increasing-size-item)
     (sort-increasing-reference-date-item)
     (sort-decreasing-file-name-item)
     (sort-decreasing-creation-date-item)
     (sort-decreasing-size-item)
     (sort-decreasing-reference-date-item)
     (blank-line)))
  dis-none)


(defun COM-DIRED-POP-UP-MISCELLANEOUS-COMMANDS-MENU ()
  (tv:menu-choose
    (list
     (blank-line)
     (miscellaneous-commands-label)
     (blank-line)
     (insert-or-remove-subdirectory-item)
     (edit-superior-directory-item)
     (execute-commands-item)
     (expunge-directory-item)
     (quit-item)
     (help-with-dired-item)
     (blank-line)))
  dis-none)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                      ;;;
;;;                                                                      ;;;
;;;                                 DEFCOMS                              ;;;
;;;                                                                      ;;;
;;;                                                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; These are the commands that are part of the new DIRED user interface.
;;; The DIRED commands themselves live in DIRED file.


(defcom COM-ENTER-DIRED
        "Invoke DIRED to do a new edit." ()
  (enter-or-re-enter-dired t)
  dis-none)


(defcom COM-RE-ENTER-DIRED
        "Invoke DIRED continue an edit." ()
  (enter-or-re-enter-dired nil)
  dis-none)


(defun ENTER-OR-RE-ENTER-DIRED (get-new-dired-p)
  (unless (dolist (screen (send tv:main-screen :inferiors))
            (when (eq (type-of screen) 'dired-constraint-frame)
              (return screen)))
    (make-instance 'dired-constraint-frame))
  (let ((current-window (car (send tv:main-screen :exposed-inferiors))))
    (unless (eq (type-of current-window) 'dired-constraint-frame)
      (setq *previous-window* current-window)))
  (send *dired-constraint-frame* :mouse-select)
  (send *dired-display-pane* :mouse-select)
  (setq *dired-command* 'com-dired)
  (when get-new-dired-p
    (send *dired-display-pane* :force-kbd-input #/h-s-m-c-d)))


(defcom COM-MENU-DIRED-ABORT "Temporary hack." ()
  (if (null *previous-window*)
      (SEND *WINDOW* :EXIT-SPECIAL-BUFFER)
    (send *previous-window* :mouse-select)
    (setq *previous-window* nil))
  dis-none)


(defcom COM-MENU-DIRED-EXIT "Temporary hack" ()
  (when (dired-process-files)
    (if (null *previous-window*)
        (SEND *WINDOW* :EXIT-SPECIAL-BUFFER NIL *INTERVAL*)
      (send *previous-window* :mouse-select)
      (setq *previous-window* nil)))
  dis-none)


(defcom COM-DIRED-EXECUTE-COMMAND
        "Execute a DIRED command whose name was given literally in a menu-item's value." ()
  (funcall *dired-command*)
  dis-all)


(defcom COM-DIRED-MOUSE-MOVE-POINT
        "MOUSE-LEFT and MOUSE-CENTER handling when DIRED active." ()
  (let ((line (nth-value 3 (mouse-char *window*))))
    (when line
      (move-bp (point) line (min (length line) 1))
      (must-redisplay *window* dis-bps)
      (send *window* :redisplay :point nil nil nil)))
  dis-none)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                      ;;;
;;;                                                                      ;;;
;;;                     ESTABLISH STARTUP CONDITIONS                     ;;;
;;;                                                                      ;;;
;;;                                                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(eval-when (load)
  (set-comtab zwei:*zmacs-comtab*
              '(#/meta-greek-d com-enter-dired
                #/control-greek-d com-re-enter-dired
                #/ com-enter-fsdebug-mode
                #/ com-leave-fsdebug-mode
                #/ com-standard-configuration
                #/ com-four-pane-configuration
                #/h-s-m-c-d com-dired-execute-command)
              '(("dired" . com-enter-dired)))
  (make-instance 'zwei:dired-constraint-frame :activate-p t))
