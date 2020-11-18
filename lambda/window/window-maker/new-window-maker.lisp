;;;-*- Mode:LISP; Package:WINDOW-MAKER; Fonts:(CPTFONT); Base:8; Readtable:ZL -*-
;;; Copyright C LISP MACHINE INC., 1985.
;;

(defflavor graphic-window
           ((in-slicing-procedures NIL))
           (tv:basic-mouse-sensitive-items tv:window)
  :inittable-instance-variables
  :gettable-instance-variables
  :settable-instance-variables)

(defmethod (graphic-window :before :update-typeout-list) ()
  (tv:typeout-item-window-remove-items))


(defmethod (graphic-window :update-typeout-list) ()
  (loop for (type object left top right bottom) in *mouse-sensitive-items-of-window*
        do
        (funcall self :primitive-item type object left top right bottom)))

(defmethod (graphic-window :mouse-moves) (x y &aux item)
  (if (not in-slicing-procedures)
      (COND ((AND (SETQ ITEM (SEND SELF ':MOUSE-SENSITIVE-ITEM X Y))
                  (ASSQ (tv:TYPEOUT-ITEM-TYPE ITEM) tv:ITEM-TYPE-ALIST))
             (LET ((LEFT (tv:TYPEOUT-ITEM-LEFT ITEM))
                   (TOP (tv:TYPEOUT-ITEM-TOP ITEM))
                   (RIGHT (tv:TYPEOUT-ITEM-RIGHT ITEM))
                   (BOTTOM (tv:TYPEOUT-ITEM-BOTTOM ITEM))
                   BWIDTH BHEIGHT)
               (SETQ BWIDTH (- RIGHT LEFT)
                     BHEIGHT (- BOTTOM TOP))
               (tv:BLINKER-SET-CURSORPOS tv:ITEM-BLINKER (- LEFT (tv:SHEET-INSIDE-LEFT))
                                         (- TOP (tv:SHEET-INSIDE-TOP)))
               (tv:BLINKER-SET-SIZE tv:ITEM-BLINKER BWIDTH BHEIGHT)
               (tv:BLINKER-SET-VISIBILITY tv:ITEM-BLINKER T)))
            (T (tv:BLINKER-SET-VISIBILITY tv:ITEM-BLINKER NIL)))
    ;; otherwise we are in slicing procedures and should
    ;; first make sure that the mouse blinker stays in the window
    ;; second return a blip of the form (:mouse-move x y) where x and y are the
    ;; coordinates of the slicing point.
    (funcall-self :force-kbd-input (list ':my-mouse-move x y))))


(DEFMETHOD (graphic-window :WHO-LINE-DOCUMENTATION-STRING) (&AUX ITEM ITEM-TYPE
                                                                         X Y)
  (if in-slicing-procedures
      in-slicing-procedures
;      "click left to take slicing point, middle or right to abort operation"
    (MULTIPLE-VALUE (X Y)
      (tv:SHEET-CALCULATE-OFFSETS SELF tv:MOUSE-SHEET))
    (SETQ X (- tv:MOUSE-X X)
          Y (- tv:MOUSE-Y Y))
    (AND (SETQ ITEM (SEND SELF ':MOUSE-SENSITIVE-ITEM X Y))
         (SETQ ITEM-TYPE (tv:TYPEOUT-ITEM-TYPE ITEM))
         (SETQ ITEM-TYPE (ASSQ ITEM-TYPE tv:ITEM-TYPE-ALIST))
         (COND ((STRINGP (THIRD ITEM-TYPE)) (THIRD ITEM-TYPE))
               ((CONSP (THIRD ITEM-TYPE))
                (FUNCALL (CAR (THIRD ITEM-TYPE)) ITEM))))))

;;; Mouse-left selects the blinking item, mouse-right pops up a menu near it
(DEFMETHOD (graphic-window :MOUSE-CLICK) (BUTTON X Y &AUX ITEM)
  (if in-slicing-procedures
      (funcall-self :force-kbd-input (list ':my-mouse-click button x y))
    (SETQ ITEM (SEND SELF ':MOUSE-SENSITIVE-ITEM X Y))
    (OR (WHEN ITEM
          (LET ((ITEM-TYPE (ASSQ (tv:TYPEOUT-ITEM-TYPE ITEM) tv:ITEM-TYPE-ALIST)))
            (WHEN ITEM-TYPE
              (SELECTQ BUTTON
                (#/MOUSE-1-1
                 (SEND SELF ':FORCE-KBD-INPUT
                       (LIST ':TYPEOUT-EXECUTE (CADR ITEM-TYPE)
                             (tv:TYPEOUT-ITEM-ITEM ITEM)))
                 T)
                (#/MOUSE-3-1
                 (PROCESS-RUN-FUNCTION "Menu Choose" #'tv:TYPEOUT-MENU-CHOOSE
                                       tv:MENU (CDDDR ITEM-TYPE) ITEM SELF
                                       ;; Compute a label for the menu.
                                       (OR (AND (CONSP (THIRD ITEM-TYPE))
                                                (CADR (THIRD ITEM-TYPE))
                                                (FUNCALL (CADR (THIRD ITEM-TYPE))
                                                         ITEM))
                                           (AND (TYPEP (SECOND ITEM) 'INSTANCE)
                                                (OR (SEND (SECOND ITEM) ':SEND-IF-HANDLES
                                                          ':STRING-FOR-PRINTING)
                                                    (SEND (SECOND ITEM) ':SEND-IF-HANDLES
                                                          ':NAME)))))
                 T)))))
        ;; Return T unless this is double-right, to inhibit the blip made by default.
        (NEQ BUTTON #/MOUSE-R-2))))

(defmethod (graphic-window :REDISPLAY) ()
  (when (typep *frame* 'frame)
    (multiple-value-bind (list-of-panes-to-draw list-of-lines)
        (funcall *frame* :get-all-inferiors-and-lines)
      (funcall-self :clear-screen)
      (setq *mouse-sensitive-items-of-window* nil)
      (loop for pane in (cons *frame* list-of-panes-to-draw)
            do
            (update-list *mouse-sensitive-items-of-window* (funcall pane :set-mouse-region))
            (multiple-value-bind (x y z s) (funcall pane :get-slots)
              (draw-box x y z s)))
      (loop for line in list-of-lines
            DO
            (update-list *mouse-sensitive-items-of-window* (funcall line :set-mouse-region)))
      (funcall-self :update-typeout-list))))

(DEFFLAVOR WINDOW-MAKER-frame
           ((*frame* nil))
           (
            tv:select-mixin
            tv:inferiors-not-in-select-menu-mixin
            tv:bordered-constraint-frame-with-shared-io-buffer
            tv:window tv:process-mixin
            )
  (:default-init-plist
    :label nil
    :save-bits T
    :panes
    '((title-pane tv:window
                  :label nil :blinker-p nil :save-bits t
                  :reverse-video-p t
                  :deexposed-typeout-action :permit)
      (menu-pane tv:command-menu :item-list ("menu"))
      (instrument-pane tv:window :label nil :blinker-p nil :save-bits t)
      (documentation-pane tv:window :label nil :blinker-p nil :save-bits t)
      (graphics-pane graphic-window :label nil :blinker-p nil
                     :save-bits t :deexposed-typeout-action :permit))
    :constraints
    '((main . ((title-and-instrument-and-menu-pane graphics-pane)
               ((title-and-instrument-and-menu-pane :horizontal (0.1)
                                                    (title-pane instrument-pane menu-pane)
                                                    ((title-pane 65. :characters))
                                                    ((menu-pane .55))
                                                    ((instrument-pane :even))))
                ((graphics-pane :even))))))

  :INITTABLE-INSTANCE-VARIABLES
  :GETTABLE-INSTANCE-VARIABLES
  :SETTABLE-INSTANCE-VARIABLES
  (:special-instance-variables *frame*))

(defmethod (window-maker-frame :after :init) (&rest ignore)
  (funcall self ':set-process (process-run-restartable-function "window editor" 'process-function self)))

(defun reset-window-maker ()
  (funcall *graphic-window-area* ':clear-screen)
  (funcall *frame* ':set-direction-of-slice nil)
  (funcall *frame* ':set-list-of-panes-or-frames nil)
  (funcall *frame* ':set-name-of-frame 'WHOLE)
  (setq *mouse-sensitive-items-of-window* nil)
  (multiple-value-bind (x y z s) (funcall *frame* ':get-slots)
    (draw-box x y z s))
  (setq *list-of-existing-and-used-names* nil
        *list-of-existing-and-not-yet-used-names* nil
        *names-accumulated-so-far* nil
        *configuration-accumulated-so-far* nil
        *what-to-save* nil
        *constraints-to-choose-from* nil
        *configuration-to-edit* nil
        *frame-to-edit* nil)
  (update-list *mouse-sensitive-items-of-window* (funcall *frame* ':set-mouse-region))
  (funcall *graphic-window-area* ':update-typeout-list))

(DEFUN PROCESS-FUNCTION (window)
  (let* ((*window-maker* window)
         (*menu-pane* (funcall window :get-pane 'menu-pane))
         (*graphic-window-area* (funcall window :get-pane 'graphics-pane))
         (*documentation-pane* (funcall window :get-pane 'documentation-pane))
         (*instrument-pane* (funcall window :get-pane 'instrument-pane))
         (*title-pane* (funcall window :get-pane 'title-pane))
         (*frame* (make-instance 'frame
                                 :left 2 :top 2
                                 :right (- (funcall *graphic-window-area* :width) 10)
                                 :bottom (- (funcall *graphic-window-area* :height) 10)
                                 :keyword :EVEN :name-of-frame 'WHOLE))
         (*mouse-sensitive-items-of-window* NIL)
         (*list-of-existing-and-used-names* NIL)
         (*list-of-existing-and-not-yet-used-names* NIL))
    (setq *latest-window-maker* window)
    (funcall window :set-*frame* *frame*)
    (funcall *menu-pane* :set-item-list *item-list-for-permanent-menu*)
    (funcall *graphic-window-area* :set-item-type-alist *item-type-alist*)
    (draw-wm-title *title-pane*)
    (multiple-value-bind (x y z s) (funcall *frame* :get-slots)
      (draw-box x y z s))
    (update-list *mouse-sensitive-items-of-window* (funcall *frame* :set-mouse-region))
    (funcall *graphic-window-area* :update-typeout-list)
    (window-editor)))

(defun process-function-internal ()
  (WINDOW-EDITOR))

(defvar wm-frame-title-font fonts:40vshd)

(defun draw-wm-title(pane)
  (funcall pane :clear-screen)
  (funcall pane
           :string-out-centered-explicit
           "WINDOW MAKER"
           0 (// (funcall *title-pane* :height) 4)
           (funcall pane :width) (funcall pane :height)
           wm-frame-title-font tv:alu-xor))

(defmethod (window-maker-frame :after :refresh) (&optional type)
  (let* ((*window-maker* self)
         (*menu-pane* (funcall self :get-pane 'menu-pane))
         (*graphic-window-area* (funcall self :get-pane 'graphics-pane))
         (*documentation-pane* (funcall self :get-pane 'documentation-pane))
         (*instrument-pane* (funcall self :get-pane 'instrument-pane))
         (*title-pane* (funcall self :get-pane 'title-pane)))
    (draw-wm-title *title-pane*)
    (funcall *graphic-window-area* :redisplay)
    (funcall *menu-pane* :set-item-list *item-list-for-permanent-menu*)
    (funcall *instrument-pane* :refresh type)
    t))

;;;Main loop

(defun window-editor ()
  (error-restart-loop
    ((sys:abort error) "aborting computation")
    (loop as blip = (funcall *documentation-pane* :list-tyi)
          as object = (third blip)
          do
          (funcall *instrument-pane* :clear-screen)
          (selectq (car blip)
            (:menu
             (eval (list (get (cadr blip) :funcall))))
            (:typeout-execute
             (selectq (second blip)
               (:kill (funcall object ':kill))
               (:vertical-split (funcall object :slice :vertical))
               (:help (tv:menu-choose help-message '(:string "WINDOWMAKER HELP" :font fonts:metsi :centered)))
               (:horizontal-split (funcall object :slice :horizontal))
               (:Insert-new-pane (funcall object :insert-new-pane))
               ;;Not implemented (:drag ())
               ))))))

(compile-flavor-methods window-maker-frame)

(tv:add-system-key #/w 'window-maker:window-maker-frame "window maker for constraint frames" t)
