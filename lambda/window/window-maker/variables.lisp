;;;-*- Mode:LISP; Package:WINDOW-MAKER; Base:8; Fonts:(CPTFONT); Readtable:ZL -*-
;;; Copyright C LISP MACHINE INC., 1985.
;;

(defvar *window-maker* nil)
(defvar *latest-window-maker* nil)

(defvar *item-type-alist* nil)

(defvar *documentation-pane* nil)

(defvar *graphic-window-area* nil)

(defvar *menu-pane* :unbound)

(defvar *instrument-pane* :unbound)

(defvar *frame* :unbound)

(defvar *item-list-for-permanent-menu* '(("Generate code" :funcall generate-code-to-use :font fonts:cptfontb
                                          :documentation "Generate code for present configuration")
;                                        ("Edit frame" :funcall frame-editor :font fonts:cptfontb
;                                         :documentation "Edit an old frame. Frame has to be already in environment")
                                         ("Reset" :funcall reset-window-maker :font fonts:cptfontb
                                          :documentation "reset window maker process")))

(defvar *title-pane* :unbound)

(defvar *lines-bordering-frames-to-update* :unbound)

(defvar *mouse-sensitive-items-of-window* nil)



(defvar *menu-1* '(("Equal areas" :eval ':EVEN :font fonts:cptfontb
                    :documentation "divide present area into two equally sized panes")
                   ("Proportional areas" :eval ':PERCENTWISE :font fonts:cptfontb
                    :documentation "divide present area into two proportional panes")
                   ("Areas of absolute size" :eval ':ABSOLUTE :font fonts:cptfontb
                    :documentation "specify size of two panes absolutely")))

(defvar *menu-2* '(("LINES" :eval ':LINES :font fonts:cptfontb :documentation "specify size in number of lines")
                   ("PIXELS" :eval ':PIXELS :font fonts:cptfontb :documentation "specify size in number of pixels")
                   ("Or let it take any remaining space" :eval ':EVEN :font fonts:cptfontb
                    :documentation "pane size is whatever area is left over")))

(defvar *menu-3* '(("CHARACTERS" :eval ':CHARACTERS :font fonts:cptfontb
                    :documentation "specify size in number of characters")
                   ("PIXELS" :eval ':PIXELS :font fonts:cptfontb :documentation "specify size in number of pixels")
                   ("Or let it take any remaining space" :eval ':EVEN :font fonts:cptfontb
                    :documentation "pane size is whatever area is left over")))


;(defvar *stream* (zwei:open-editor-stream ':buffer-name "debug trace" ':create-p t))

(defvar *list-of-known-keywords* nil)

(defvar *lines-on-the-window* nil)

(defvar *list-of-known-flavor-types*
        '(("tv:lisp-listener" :eval 'tv:lisp-listener :font fonts:tr10b)
          ("zwei:zmacs-frame" :eval 'zwei:zmacs-frame :font fonts:tr10b)
          ("tv:command-menu" :eval 'tv:command-menu :font fonts:tr10b)
          ("tv:window" :eval 'tv:window :font fonts:tr10b)
          ("tv:inspect-frame" :eval 'tv:inspect-frame :font fonts:tr10b)
          ("tv:peek-frame" :eval 'tv:peek-frame :font fonts:tr10b)
          ("b-w-obl-display-window" :eval 'b-w-obl-display-window :font fonts:tr10b)))

(defvar *list-of-blinker-flavor*
        '(("tv:rectangular-blinker" :eval 'tv:rectangular-blinker :font fonts:tr10b)
;         ("tv:hollow-rectangular-blinker" :eval 'tv:hollow-rectangular-blinker :font fonts:tr10b)
;         ("tv:box-blinker" :eval 'tv:box-blinker :font fonts:tr10b)
;         ("tv:ibeam-blinker" :eval 'tv:ibeam-blinker :font fonts:tr10b)
;         ("tv:character-blinker" :eval 'tv:character-blinker :font fonts:tr10b)
          ))

(defstruct (line-to-output
             :named-array
             (:print-function (lambda (object stream)
                                (sys:printing-random-object (object stream :type)
                                  (format stream "~A" (text-to-display object))))))
  (Text-to-display "" :documentation "this is the text to output on the window")
  (variable-to-bind nil :documentation "this is the variable to bind the result to")
  (sensitive-items nil :documentation "this slot will hold the mouse sensitive items for this line")
  (selected-element nil)
  (font nil))

(defvar *window-for-code-generation* nil)

(defvar *type-of-constraint-frames* NIL)

(defvar *menu-of-different-constraint-type*
        '(("tv:constraint-frame" :eval 'tv:constraint-frame :font fonts:tr12b
           :documentation "basic constraint frame no borders")
          ("tv:bordered-constraint-frame" :eval 'tv:bordered-constraint-frame :font fonts:tr12b
           :documentation "basic constraint frame with borders")
          ("tv:constraint-frame-with-shared-io-buffer" :eval 'tv:constraint-frame-with-shared-io-buffer :font fonts:tr12b
           :documentation "no borders, every pane has same io buffer")
          ("tv:bordered-constraint-frame-with-shared-io-buffer" :eval 'tv:bordered-constraint-frame-with-shared-io-buffer
           :font fonts:tr12b :documentation "borders, every pane has same io buffer")))

(defvar *selection-substitute* NIL)

(defvar *type-of-code* 'MY-FLAVOR)

(defvar *buffer-name* NIL)

(defvar *list-of-lines* nil)

(defvar *name* nil)

(defvar *type* 'tv:window)

(defvar *label* *name*)

(defvar *save-bits* nil)

(defvar *deexposed-typeout-action* ':normal)

(defvar *blinker-p* nil)

(defvar *deexposed-typein-action* ':notify)

(defvar *choose-window* nil)

(defvar *list-of-existing-and-not-yet-used-names* nil)

(defvar *list-of-existing-and-used-names* nil)

(defvar *item-list* nil)

(defvar *blinker-flavor* nil)

(defvar *blinker-deselected-visibility* nil)

;;; variables used in edit functions.

(defvar *what-to-edit* 'tv:peek-frame)

(defvar *frame-to-edit* (make-instance 'tv:peek-frame))

(defvar *configuration-to-edit* NIL)

(defvar *constraints-to-choose-from* nil)

(defvar *what-to-save* NIL)


(defvar help-message
        '(("Copyright C LISP MACHINE INC., 1985." :no-select nil :font fonts:cptfontb)
          ("Dividing the workspace" :no-select nil :font fonts:metsi)
          ("To divide any pane in the workspace, move the mouse over it and click right.   "
           :no-select nil :font fonts:cptfontb)
          ("WindowMaker will guide you through each division with menus. Panes are divided "
           :no-select nil :font fonts:cptfontb)
          ("either horizontally or vertically. For each division, you will be asked to     "
           :no-select nil :font fonts:cptfontb)
          ("choose a method to specify the sizes of the new panes. For proportional        "
           :no-select nil :font fonts:cptfontb)
          ("or absolute specification of pane sizes, you will be prompted with a cursor    "
           :no-select nil :font fonts:cptfontb)
          ("tied to the mouse. Move the cursor to the chosen dividing point, then click    "
           :no-select nil :font fonts:cptfontb)
          ("left to select that point. Click middle to abort.                              "
           :no-select nil :font fonts:cptfontb)
          ("Generating the code" :no-select nil :font fonts:metsi)
          ("Select GENERATE CODE. For each pane visible on the workspace, WindowMaker will "
           :no-select nil :font fonts:cptfontb)
          ("ask you to specify the name, flavor type and and init plist for that pane.     "
           :no-select nil :font fonts:cptfontb)
          ("To go to the next pane, select the DO-IT box. If no name has been specified for"
           :no-select nil :font fonts:cptfontb)
          ("the current pane, WindowMaker generates a dummy name and defaults the type to  "
           :no-select nil :font fonts:cptfontb)
          ("tv:window. If the abort box is selected, WindowMaker generates a dummy name and"
           :no-select nil :font fonts:cptfontb)
          ("defaults the type to tv:window regardless of what has been entered previously. "
           :no-select nil :font fonts:cptfontb)
          ("After specifying the attributes of the last pane, WindowMaker prompts you with "
           :no-select nil :font fonts:cptfontb)
          ("a menu which asks for information about the whole frame. It also allows you to "
           :no-select nil :font fonts:cptfontb)
          ("choose a buffer into which the code should be written.                         "
           :no-select nil :font fonts:cptfontb)
          ("Resetting WindowMaker" :no-select nil :font fonts:metsi)
          ("To reset WindowMaker, select RESET. This will cause all data accumulated by    "
           :no-select nil :font fonts:cptfontb)
          ("WindowMaker to be lost.                                                        "
           :no-select nil :font fonts:cptfontb)))


;;; variables used for different configurations.

(defvar *configuration-accumulated-so-far* NIL)

(defvar *names-accumulated-so-far* NIL)

(defvar *configuration-name* NIL "name of configuration")

(defvar *known-configuration-names* NIL "used to prevent name conflicts")

(defvar *code-or-configuration* NIL "values for this variable are :code for generate code
after storing this configuration and :configuation for store as next configuration")
