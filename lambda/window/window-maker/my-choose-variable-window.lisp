;;;-*-Mode:Lisp;Base:8;package:window-maker-*-
;;; Copyright C LISP MACHINE INC., 1985.
;;

(defflavor my-choose-variable-window
  (init-option-list done)
  (tv:function-text-scroll-window
   tv:mouse-sensitive-text-scroll-window
   tv:text-scroll-window
   tv:borders-mixin
   tv:top-label-mixin
   tv:basic-scroll-bar
   tv:flashy-scrolling-mixin
   tv:margin-scroll-mixin
   tv:margin-region-mixin
   tv:margin-choice-mixin
   tv:scroll-stuff-on-off-mixin
   tv:dont-select-with-mouse-mixin
   tv:window)
  (:default-init-plist
    :label '(:string "Specify window attributes" :centered :font fonts:metsi)
    :blinker-p t
    :blinker-deselected-visibility ':off
    :blinker-flavor 'tv:rectangular-blinker
    :deexposed-typeout-action ':permit
    :save-bits t
    :margin-choices '(("Do It" nil CHOICE-DONE nil nil)
                      ("Abort" NIL CHOICE-ABORT NIL NIL))
    :flashy-scrolling-region '((20 0.30 0.70) (20 0.30 0.70))
    :margin-scroll-regions '((:top) (:bottom))
    :scroll-bar-always-displayed t
    :font-map (list fonts:cptfont fonts:cptfontb fonts:tr12 fonts:tr10 fonts:tr10b fonts:tr12b)
    :print-function 'output-line)
  :settable-instance-variables
  :inittable-instance-variables
  :gettable-instance-variables)

(defun choice-done (&rest ignore)
  (DECLARE (:SELF-FLAVOR my-choose-variable-window))
  (SETQ init-option-list
        (loop for i from 0 to (ARRAY-ACTIVE-LENGTH tv:ITEMS)
              with return-list = NIL
              as item = (aref tv:items i)
              do
              (selectq item
                (*name* (update-list return-list (if (and (boundp '*name*) *name*)
                                                     *name* (gentemp 'unspecified-pane-name))))
                (*label* (if (boundp '*label*) (nconc return-list (list ':label *label*))))
                (*type* (update-list return-list (or *type* 'tv:window)))
                (*blinker-p* (nconc return-list (list ':blinker-p *blinker-p*)))
                (*save-bits* (nconc return-list (list ':save-bits *save-bits*)))
                (*blinker-flavor* (nconc return-list (list ':blinker-flavor *blinker-flavor*)))
                (*blinker-deselected-visibility*
                  (nconc return-list (list ':blinker-deselected-visibility *blinker-deselected-visibility*)))
                (*deexposed-typeout-action* (nconc return-list (list ':deexposed-typeout-action *deexposed-typeout-action*)))
                (*deexposed-typein-action* (nconc return-list (list ':deexposed-typein-action *deexposed-typein-action*))))
              finally (return (if (eq *type* 'tv:command-menu)
                                  (nconc return-list (list ':item-list '("you" "have" "to" "change" "me")))
                                (if (eq *type* 'b+w-display-window)
                                    (nconc return-list (list ':name-of-pane *name*))
                                return-list)))))
  (funcall self :force-kbd-input (list ':choice-box 'do-it)))



(DEFUN CHOICE-ABORT (&REST IGNORE)
  (DECLARE (:SELF-FLAVOR my-choose-variable-window))
  (setq init-option-list (list (gentemp 'unspecified-pane-name) 'tv:window))
  (funcall-self :force-kbd-input (list ':choice-box 'abort)))

(defmethod (tv:scroll-stuff-on-off-mixin :adjustable-size-p) ()
  nil)

(defmethod (my-choose-variable-window :enable-scrolling-p) ()
  tv:scroll-bar-always-displayed)



(defun find-line (variable-to-bind &optional (reset-value? nil))
  (loop for line in *lines-on-the-window*
        when (equal (variable-to-bind line) variable-to-bind)
        do
        (and reset-value? (set (variable-to-bind line) nil))
        (return line)
        finally (return nil)))

(defun update-line-for-name ()
  (let ((line (find-line '*name*))
        items)
    (setq items (sensitive-items line))
    (if *list-of-existing-and-not-yet-used-names*
        (and (= (length items) 1)
             (rplacd items '((:name "Name from menu"))))
      (and (= (length items) 2)
           (rplacd items nil)))))

(defun create-line (text variable-to-bind default-value sensitive-items selected-element font &optional (reset-value? nil))
  (let ((line (find-line text reset-value?)))
    (when (null line)
      (setq line (make-line-to-output
                   :text-to-display text
                   :variable-to-bind variable-to-bind
                   :sensitive-items sensitive-items
                   :selected-element selected-element
                   :font font))
      (set variable-to-bind default-value)
      (update-list *lines-on-the-window* line))
    line))

(defmethod (my-choose-variable-window :add-line-to-be-displayed) (line)
  (funcall-self ':append-item (variable-to-bind line)))

(defmethod (my-choose-variable-window :delete-all-elements) (&optional (index 0))
  (loop with index-of-item-to-flush = index
        when (= (array-leader tv:items 0) index)
        do (return nil)
        else
        do
        (funcall-self ':delete-item index-of-item-to-flush)))

(defmethod (my-choose-variable-window :output-string-to-window) (text &optional (line-no nil))
  "method to output the string which is the choice for the line. if line-no is unspecified then we supposed that
the cursor is already on the line"
  (if (not line-no) nil
    (if (zerop tv:top-item) nil (setq line-no (- line-no tv:top-item)))
    (SEND self ':SET-CURSORPOS 0 (* LINE-NO tv:line-height)));(tv:SHEET-LINE-HEIGHT self))))
  (setq tv:cursor-x 0)
  (format self "~60T")
  (multiple-value-bind (x y) (funcall-self ':read-cursorpos)
    (funcall self ':bitblt-within-sheet tv:alu-xor
             (- tv:width x)
             tv:line-height
             x y x y)
    (let ((allowed-width (// (- (tv:sheet-inside-width self) x) tv:char-width))
          (length-of-text (string-length text)))
      (format self "~A" (if (<= length-of-text allowed-width) text (substring text 0 allowed-width))))))

(defmethod (my-choose-variable-window :end-of-line-exception) ()
  (multiple-value-bind (NIL cursor-y) (funcall-self ':read-cursorpos)
    (funcall-self ':set-cursorpos 0 cursor-y)
    (format self "~60T")
    (multiple-value-bind (x y) (funcall-self ':read-cursorpos)
        (funcall self ':bitblt-within-sheet tv:alu-xor
                 (- (funcall self ':width) x)
                 (funcall-self ':line-height)
                 x y x y))))

(defun output-line (item arg window &rest ignore)
  arg
  (let ((line (find-line item)))
    (when line
      (funcall window ':Set-current-font fonts:tr12)
      (format window "~A   : " (text-to-display line))
      (format window "~40T")
      ;; output now mouse sensitive items
      (funcall window ':set-current-font fonts:tr10)
      (loop for (type text) in (sensitive-items line)
            with selected-element = (selected-element line)
            when (and selected-element (string-equal text selected-element))
            do
            (funcall window ':set-current-font (font line))
            (funcall window ':item1 text type #'princ)
            (funcall window ':set-current-font fonts:tr10)
            (format window "   ")
            else
            do
            (funcall window ':item1 text type #'princ)
            (format window "   ")
            finally
            (progn
              (funcall window ':set-current-font fonts:cptfont)
              (format window "~60T")
              ;; first erase all of that left over region.
              (multiple-value-bind (x y) (funcall window ':read-cursorpos)
                (funcall self ':bitblt-within-sheet tv:alu-xor
                         (- (funcall window ':width) x)
                         (funcall window ':line-height)
                         x y x y))
              (and (not selected-element)
                   (boundp (variable-to-bind line))
                   (eval (variable-to-bind line))
                   (funcall window ':output-string-to-window
                            (format nil "~S" (eval (variable-to-bind line))))))))))

(defmethod (my-choose-variable-window :after :read-from-the-window) (item line-no &rest ignore)
  "just checking to see if we have any thing to redisplay. Making sure that all lines are still
displayed"
  item
  (let (line-clobbered item1)
    (setq line-clobbered (+ tv:top-item(TRUNCATE (- (tv:SHEET-CURSOR-Y self) (tv:SHEET-INSIDE-TOP self))
                                                  (tv:SHEET-LINE-HEIGHT self)))
          item1 (funcall-self :item-of-number line-clobbered))
    (if (not item1) NIL
      (funcall-self :delete-item line-clobbered)
      (funcall-self :insert-item line-clobbered item1)
      (setq tv:cursor-y (* (if (zerop tv:top-item) LINE-NO (- line-no tv:top-item)) (tv:SHEET-LINE-HEIGHT self))))))


(defmethod (my-choose-variable-window :read-from-the-window) (item line-no &optional (rf 'read)
                                                              &aux var str newval no-change redis oldval)
  (setq  line-no (if (zerop tv:top-item) line-no (- line-no tv:top-item)))
  (COND ((STRINGP item)
         (SETQ STR ITEM))       ;Can't happen
        ((SYMBOLP ITEM)
         (SETQ VAR ITEM STR (GET-PNAME VAR))))
  (tv:SHEET-SET-FONT self (AREF (tv:SHEET-FONT-MAP self) 0))
  (LET ((BL (tv:SHEET-FOLLOWING-BLINKER self))
        (WS (SEND self ':STATUS)))
    (UNWIND-PROTECT
        (PROGN (SEND self ':SELECT)
               ;; Next line makes the mouse highlight go away
               (SEND BL :SET-VISIBILITY ':BLINK)
               (SEND self ':SET-CURSORPOS
                     (IF (NULL STR) 0
                       (+ (tv:SHEET-STRING-LENGTH self (STRING STR))
                          (tv:SHEET-CHAR-WIDTH self)))
                     (* line-no (tv:SHEET-LINE-HEIGHT self)))
               (format self "~60T")
               ;; clears the area of the window where the typeout is going to be done
               (multiple-value-bind (x y) (funcall-self ':read-cursorpos)
                 (funcall self ':bitblt-within-sheet tv:alu-xor
                          (- (funcall self ':width) x)
                          (funcall-self ':line-height)
                          x y x y))
               (SEND self ':CLEAR-EOL)
               ;; clear io buffer from any pending characters
               (tv:io-buffer-clear tv:io-buffer)
               ;; Hair for over-rubout => save old value
               (DO ((CH) (FULL-RUBOUT T) (REDISPLAY-FLAG NIL)
                    (FIRST-TIME T NIL)
                    (*TERMINAL-IO* self))       ;Should be ERROR-OUTPUT
                   ((NOT FULL-RUBOUT))
                 (DECLARE (SPECIAL REDISPLAY-FLAG))
                 (UNLESS FIRST-TIME
                   (AND (CHAR= (SETQ CH (SEND self ':TYI)) #/RUBOUT)
                        (RETURN (SETQ NO-CHANGE T)))
                   (SEND self ':UNTYI CH))
                 (MULTIPLE-VALUE (NEWVAL FULL-RUBOUT)
                   (SEND self ':RUBOUT-HANDLER '((:full-rubout t))
                         #'(LAMBDA (rf STREAM &AUX WIN)
                             (UNWIND-PROTECT
                                 (PROG1
                                   (FUNCALL RF STREAM)
                                   (SETQ WIN T))
                               (UNLESS WIN
                                 (SETQ REDISPLAY-FLAG T))))
                         RF self))
                 ;; If we got a read error, try to avoid garbage in the display
                 ;; This is really a kludge, is there a better way?
                 (SETQ REDIS REDISPLAY-FLAG)))
      (SEND BL :SET-VISIBILITY NIL)
      (OR (EQ WS ':SELECTED) (SEND self ':SET-STATUS WS)))      ;)
    (SETQ OLDVAL (IF (SYMBOLP VAR)
                     (SYMEVAL VAR)
                   (CAR VAR)))
    (AND NO-CHANGE (SETQ NEWVAL OLDVAL))
    (IF (SYMBOLP VAR)
        (SET VAR NEWVAL)
      (RPLACA VAR NEWVAL))))

(defun add-keyword (keyword text variable-to-bind default-value sensitive-items selected-element font)
  (if (memq keyword *list-of-known-keywords*) nil
    (putprop keyword text 'text)
    (putprop keyword variable-to-bind 'variable-to-bind)
    (putprop keyword default-value 'default-value)
    (putprop keyword sensitive-items 'sensitive-items)
    (putprop keyword selected-element 'selected-element)
    (putprop keyword font 'font)
    (update-list *list-of-known-keywords* keyword)))

(add-keyword ':label "Specify a label" '*label* nil '((:label "NIL") (:label "Default") (:label "From keyboard"))
             "nil" fonts:tr10b)

(add-keyword ':deexposed-typeout-action "Deexposed typeout action" '*deexposed-typeout-action* ':NORMAL
             '((:deexposed-typeout-action "PERMIT") (:deexposed-typeout-action "NORMAL")
               (:deexposed-typeout-action  "EXPOSE") (:deexposed-typeout-action "NOTIFY")
               (:deexposed-typeout-action "ERROR")) "NORMAL" fonts:tr10b)

;(add-keyword ':item-list "Specify item list" '*item-list* '("item" "list")
;            '((:item-list "Default") (:item-list "From keyboard")) nil nil)

(add-keyword ':save-bits "Save bit array" '*save-bits* T '((:save-bits "Yes") (:save-bits "No")) "Yes" fonts:tr10b)

(add-keyword ':blinker-p "Blinker for window" '*blinker-p* T '((:blinker-p "Yes") (:blinker-p "No")) "Yes" fonts:tr10b)

(add-keyword ':blinker-flavor "Specify blinker flavor" '*blinker-flavor* 'tv:rectangular-blinker
             '((:blinker-flavor "From keyboard") (:blinker-flavor "From menu")) nil nil)

(add-keyword ':blinker-deselected-visibility "Visibility of blinker" '*blinker-deselected-visibility* ':ON
             '((:blinker-deselected-visibility "On") (:blinker-deselected-visibility "Off")
               (:blinker-deselected-visibility  "Blink") (:blinker-deselected-visibility "T")
               (:blinker-deselected-visibility "NIL")) "On" fonts:tr10b)

(add-keyword ':deexposed-typein-action "Deexposed typein action" '*deexposed-typein-action* ':NORMAL
             '((:deexposed-typein-action "NORMAL") (:deexposed-typein-action "NOTIFY")) "NORMAL" fonts:tr10b)

(defun get-all-the-other-init-keywords-for-option-list (window type)
  (let ((init-keyword-list
          (condition-case (error)
              (si:flavor-all-allowed-init-keywords type)
            (error NIL))))
    (funcall window ':delete-all-elements 2)
    (loop for keyword in init-keyword-list
          when (memq keyword *list-of-known-keywords*)
          do
          (funcall window ':add-line-to-be-displayed
                   (create-line (get keyword 'text)
                                (get keyword 'variable-to-bind)
                                (get keyword 'default-value)
                                (get keyword 'sensitive-items)
                                (get keyword 'selected-element)
                                (get keyword 'font) T)))))

(defun create-all-line-except-for-name (option-list &aux line)
  (setq line (create-line "Window flavor" '*type* *type* '((:type "From keyboard")(:type "From menu")) nil nil))
  (loop for index from 0 to (- (length option-list) 2) by 2
        as keyword = (nth index option-list)
        when (memq keyword *list-of-known-keywords*)
        do
        (create-line (get keyword 'text)
                     (get keyword 'variable-to-bind)
                     (get keyword 'default-value)
                     (get keyword 'sensitive-items)
                     (get keyword 'selected-element)
                     (get keyword 'font)))
  line)
;;
;;
;;   The name of the panes should be unique to avoid any loss when trying to
;; instantiate the frame.
;;
;;

(defun regenerate-lines-for-window (window &aux option-list line)
  ;; first flush all elements already in the window except for
  ;; name
  (funcall window :delete-all-elements 1)
  (setq *type* (get *name* 'flavor))
  (setq option-list (copylist (get *name* 'option-list)))
  (setq line (create-all-line-except-for-name option-list))
  (funcall window :add-line-to-be-displayed line) ;(find-line *type*))
  (and (member ':item-list option-list)
       (progn
         (delete-element-from-list option-list ':item-list)
         (delete-element-from-list option-list "you have to change me")))
  (loop for index from 0 to (- (length option-list) 2) by 2
        as keyword = (nth index option-list)
        as value = (nth (1+ index) option-list)
        when (memq keyword *list-of-known-keywords*)
        do
        (funcall window :add-line-to-be-displayed
                 (selectq keyword
                   (:label (setq *label* value) (find-line '*label*))
                   (:save-bits (setq *save-bits* value) (find-line '*save-bits*))
                   (:deexposed-typeout-action (setq *deexposed-typeout-action* value)
                                              (find-line '*deexposed-typeout-action*))
                   (:blinker-p (setq *blinker-p* value) (find-line '*blinker-p*))
                   (:deexposed-typein-action (setq *deexposed-typein-action* value)
                                             (find-line '*deexposed-typein-action*))
                   (:blinker-flavor (setq *blinker-flavor* value) (find-line '*blinker-flavor*))
                   (:blinker-deselected-visibility (setq *blinker-deselected-visibility* value)
                                                   (find-line '*blinker-deselected-visibility*)))))
  ;; Now the trick is to turn off the mouse sensitivity of every thing except for name.
  ;; This ought to do it.
  (funcall window :set-sensitive-item-types '(:name)))

;;; This function get-the-name-of-pane when called will always check to see if the name
;;; entered from the keyboard has been already entered previously. If it has then it gets
;;; all the data associated with that name. A check is done to see if the name just
;;; entered from the keyboard has not been already in use. If yes then window maker refuses
;;; to take it.

(defun get-the-name-of-pane (window element)
  (funcall window ':set-current-font fonts:cptfont)
  (let ((line-no (funcall window ':number-of-item '*name*)))
    (loop do
          (if (string-equal element "From keyboard")
              (funcall window ':read-from-the-window '*name* line-no)
            (setq *name* (tv:menu-choose *list-of-existing-and-not-yet-used-names*
                                         '(:string "Known names" :font fonts:cptfontb)))
            (SEND window ':SET-CURSORPOS 0 (* LINE-NO (tv:SHEET-LINE-HEIGHT window)))
            (format window "~60T")
            (multiple-value-bind (x y) (funcall window ':read-cursorpos)
              (funcall window ':bitblt-within-sheet tv:alu-xor
                       (- (funcall window ':width) x)
                       (funcall window ':line-height)
                       x y x y)))
          (if (not (member *name* *list-of-existing-and-used-names*)) (return nil)))
    (funcall window ':output-string-to-window (format nil "~S" *name*) line-no))
  (if (member *name* *list-of-existing-and-not-yet-used-names*)
      (regenerate-lines-for-window window)
    (funcall window :set-sensitive-item-types T)))


(defun get-the-type-of-flavor-window (window element)
  (funcall window ':set-current-font fonts:cptfont)
  (let ((line-no (funcall window ':number-of-item '*type*)))
    (if (string-equal element "From keyboard")
        (funcall window ':read-from-the-window '*type* line-no)
      (setq *type* (or (tv:menu-choose *list-of-known-flavor-types*
                                       '(:string "flavor types" :font fonts:cptfontb)
                                       '(:MOUSE) "tv:window")
                       'tv:window)))
    (funcall window ':output-string-to-window (format nil "~S" *type*) line-no)
    ;;add a function which get the new items to display on the window for init option list.
    (get-all-the-other-init-keywords-for-option-list window *type*)))

(defun get-the-save-bits (window element)
  (let ((line (find-line '*save-bits*))
        (line-no (funcall window ':number-of-item '*save-bits*)))
  (setf (selected-element line) element)
  (funcall window ':delete-item line-no)
  (funcall window ':insert-item line-no '*save-bits*))
  (setq *save-bits* (cond ((string-equal element "Yes") t)
                          ((string-equal element "No") nil))))


(defun get-the-item-list (window element)
  window element)

(defun get-the-blinker-flavor (window element)
  (funcall window ':set-current-font fonts:cptfont)
  (let ((line-no (funcall window ':number-of-item '*blinker-flavor*)))
    (if (string-equal element "From keyboard")
        (funcall window ':read-from-the-window '*blinker-flavor* line-no)
      (setq *blinker-flavor* (or (tv:menu-choose  *list-of-blinker-flavor*
                                                  '(:string "blinker types" :font fonts:cptfontb)
                                                  '(:MOUSE) 'TV:RECTANGULAR-BLINKER)
                                 'TV:RECTANGULAR-BLINKER)))
    (funcall window ':output-string-to-window (format nil "~S" *blinker-flavor*) line-no)))


(defun get-the-blinker-deselected-visibility (window element)
  (let ((line (find-line '*blinker-deselected-visibility*))
        (line-no (funcall window ':number-of-item '*blinker-deselected-visibility*)))
    (setf (selected-element line) element)
    (funcall window ':delete-item line-no)
    (funcall window ':insert-item line-no '*blinker-deselected-visibility*)
    ;; now update the variable.
    (setq *blinker-deselected-visibility*
          (cond ((string-equal element "on") ':on)
                ((string-equal element "blink") ':blink)
                ((string-equal element "off") ':off)
                ((string-equal element "T") T)
                ((string-equal element "NIL") nil)))))

(defun get-the-blinker (window element)
  (let ((line (find-line '*blinker-p*))
        (line-no (funcall window ':number-of-item '*blinker-p*)))
    (setf (selected-element line) element)
  (funcall window ':delete-item line-no)
  (funcall window ':insert-item line-no '*blinker-p*))
  (setq *blinker-p* (cond ((string-equal element "Yes") t)
                          ((string-equal element "No") nil))))

(defun get-the-label-for-window (window element)
  "this will ask the user to specify a label for the window. If no label is needed then select nil
if want to use default then window will have same label as *name*. if keyboard is entered then a string is read
from the keyboard."
  (funcall window ':set-current-font fonts:cptfont)
  (let ((line (find-line '*label*))
        (line-no (funcall window ':number-of-item '*label*)))
    (setf (selected-element line) (if (string-equal element "NIL") "NIL" nil))
    ;; another way of cleaning the line from previous text.
    (setq *label* nil)
    (funcall window ':delete-item line-no)
    (funcall window ':insert-item line-no '*label*)
    (if (string-equal element "NIL")
        (setq *label* nil)
      (setq *label* *name*)
      (if (string-equal element "Default")
          (funcall window ':output-string-to-window (format nil "~A" *label*) line-no)
        (funcall window ':read-from-the-window '*label* line-no 'readline)
        (funcall window ':output-string-to-window *label* line-no)
        ))))


(defun get-deexposed-typeout-action (window element)
  (let ((line-no (funcall window ':number-of-item '*deexposed-typeout-action*))
        (line (find-line '*deexposed-typeout-action*)))
    (setf (selected-element line) element)

    (funcall window ':delete-item line-no)
    (funcall window ':insert-item line-no '*deexposed-typeout-action*)
    (setq *deexposed-typeout-action*
          (cond ((string-equal element "PERMIT") ':PERMIT)
                ((string-equal element "NORMAL") ':NORMAL)
                ((string-equal element "ERROR") ':ERROR)
                ((string-equal element "NOTIFY") ':NOTIFY)
                ((string-equal element "EXPOSE") ':EXPOSE)))))

(defun get-the-deexposed-typein-action (window element)
  (let ((line-no (funcall window ':number-of-item '*deexposed-typein-action*))
        (line (find-line '*deexposed-typein-action*)))
    (setf (selected-element line) element)
    (funcall window ':delete-item line-no)
    (funcall window ':insert-item line-no '*deexposed-typein-action*)
    (setq *deexposed-typein-action*
          (cond ((string-equal element "PERMIT") ':PERMIT)
                ((string-equal element "NORMAL") ':NORMAL)))))
;;
;;
;;     There is a problem when the layout of the screen has changed. this should be taken care of
;;  each time that i want to print something on the side.
;;
;;

(defflavor temporary-my-choose-variable-window () (my-choose-variable-window))

(defwindow-resource temporary-my-choose-variable-window ()
  :make-window (temporary-my-choose-variable-window :height 300 :width 1200)
  :reusable-when :deactivated
  :initial-copies 1)

(defun get-name-and-type (left top right bottom superior &aux line (name-and-option nil) list-to-return)
  (using-resource (*choose-window* temporary-my-choose-variable-window superior)
    ;; Make-sure that the window does not have any thing from before.
    (makunbound '*name*)
    (funcall *choose-window* :set-done nil)
    (funcall *choose-window* :delete-all-elements)
    (funcall *choose-window* :clear-screen)
    (setq line (create-line "Name must be unique" '*name* nil (list '(:name "From keyboard")) nil nil T))
    (update-line-for-name)
    (funcall *choose-window* :add-line-to-be-displayed line)
    (setq line (create-line "Window flavor" '*type* nil '((:type "From keyboard")(:type "From menu")) nil nil))
    (funcall *choose-window* :add-line-to-be-displayed line)
    (funcall *choose-window* :set-sensitive-item-types T)
    (funcall *choose-window* :set-scroll-bar-always-displayed t)
    (funcall *choose-window* :expose-near (list ':rectangle left top right bottom))
    (unwind-protect
        (setq list-to-return
              (loop with tv:mouse-sheet = *choose-window*
                    as blip = (funcall *choose-window* ':list-tyi)
                    as type = (first blip)
                    as element = (second blip)
                    do
                    (funcall *choose-window* :set-scroll-bar-always-displayed nil)
                    (selectq type
                      (:type
                       (get-the-type-of-flavor-window *choose-window* element))
                      (:name
                       (get-the-name-of-pane *choose-window* element))
                      (:label
                       (get-the-label-for-window *choose-window* element))
                      (:save-bits
                       (get-the-save-bits *choose-window* element))
                      (:deexposed-typeout-action
                       (get-deexposed-typeout-action *choose-window* element))
                      (:item-list
                       (get-the-item-list *choose-window* element))
                      (:blinker-p
                       (get-the-blinker *choose-window* element))
                      (:blinker-flavor
                       (get-the-blinker-flavor *choose-window* element))
                      (:blinker-deselected-visibility
                       (get-the-blinker-deselected-visibility *choose-window* element))
                      (:deexposed-typein-action
                       (get-the-deexposed-typein-action *choose-window* element))
                      (:choice-box
                       (selectq element
                         (do-it (setq name-and-option (funcall *choose-window* :init-option-list))
                                (rplacd (cdr name-and-option) (ncons (cddr name-and-option)))
                                (delete-element-from-list *list-of-existing-and-not-yet-used-names* *name*)
                                (and *name* (not (string-search "unspecified-pane-name" (string *name*)))
                                     (progn (update-list *list-of-existing-and-used-names* *name*)
                                            (putprop *name* *type* 'flavor)
                                            (putprop *name* (third name-and-option) 'option-list)))
                                (return (list name-and-option element)))
                         (Abort (return (list nil element))))))
                    (funcall *choose-window* :set-scroll-bar-always-displayed t)))
      ;; we want something which recognize the control-abort.
      (or name-and-option (setq list-to-return (list NIL 'ABORT)))
      (funcall *choose-window* :set-scroll-bar-always-displayed t)
      (funcall *choose-window* :deactivate))
    (apply #'values list-to-return)))


(compile-flavor-methods my-choose-variable-window)

(compile-flavor-methods temporary-my-choose-variable-window)
