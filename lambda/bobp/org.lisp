
;-*- Mode:LISP; Package:(ORG :USE (USER GLOBAL SYSTEM)); Fonts:CPTFONT; Base:8 -*-

;; Copyright LISP Machine, Inc. 1984
;;   See filename "Copyright.text" for
;; licensing and release information.

;;; ORG: Organization Chart Program

;;; REVISION HISTORY
;;;
;;; E. Smith and L. Wilde       2/85       Revise to work with Release 2.0
;;; S. Strassman                7/84       Added additional features
;;; J. Hendler                  6/84       Modify nodes to use "FLAVORS" object class system
;;; L. Hawkinson                Unknown    Original Version

;------------------------------------------------------------------------------
; To use this code:
;
; Just load this file up and type SYSTEM-O.
;
;------------------------------------------------------------------------------
; Set up our global variables.
;
; Note the convention of beginning each one with an asterisk.
; Also, every one has a documentation string.
;

(defvar *chart-frame :unbound
  "frame for org process")

(defvar *chart-pane :unbound
  "org chart pane in org chart frame")

(defvar *command-pane :unbound
  "command pane in org chart frame")

(defvar *prompt-pane :unbound
  "prompt pane in org chart frame")

(defvar *default-attributes
        '(("Name" "anonymous")
          ("Title" *needs-filling-in)
          ("Location" *needs-filling-in))
  "The default list of attributes a node could have.")

(defconst *needs-filling-in "*****"
  "value for descriptor not (yet) filled in")

(defvar *closed-width 15.
  "The width of a closed node")

(defvar *closed-height 8.
  "The height of a closed node")

(defconst *alu tv:alu-ior "TV algorithm for drawing characters")

(defconst *scroll-amount 100. "Number of pixels to scroll on the scroll command")

(defvar *file (fs:merge-pathname-defaults "foo.chart")
 "The filename to store the current chart under.")

(defvar *default-h-separation 10)
(defvar *default-v-separation 10)

;------------------------------------------------------------------------------
; Nodes

; A node has the following instance variables, which keep track of the state
; of that node:
;
; opened?:                A closed node exists, but is invisible. An opened node is visible.
; attributes:             A list of sublists (i.e. an "alist"). Each sublist is a list of
;                         two strings: a slot (eg: "Name"), and its value (eg: "Melvin").
;                         A special value is the variable *NEEDS-FILLING-IN.
; justification           One of three keywords: :CENTER, :LEFT, or :RIGHT.
; extra-width:            How much whitespace to put inside the box around the text
;                         (horizontally)
; vertical-separation:    How much whitespace to put beneath this node and its inferiors.
; horizontal-separation:  How much whitespace to put between each inferior.
; above:                  The node above this one
; below:                  A list of the nodes below this one
; pane:                   The window this node draws itself on
; height:                 The height of this node's box in pixels
; width:                  The width of this node's box in pixels
; sub-tree-height:        The height of the sub-tree including this node in pixels
; sub-tree-width:         The width of the sub-tree including this node in pixels
; left:                   The left edge of this node in screen coordinates
; top:                    The top edge of this node in screen coordinates

(defflavor node
        ((opened? t)                            ; are you visible?
         (attributes *default-attributes)       ; an alist of print values
         (justification ':center)               ; how to print the values
         (extra-width 10.)                      ; extra margin in pixels
         (vertical-separation *default-v-separation)
         (horizontal-separation *default-v-separation)
         (above nil)                            ; the node above
         (below nil)                            ; the nodes below
         pane                                   ; the window to draw-self on
         height                                 ; in pixels
         width                                  ; in pixels
         sub-tree-width                         ; visible sub-tree
         sub-tree-height                        ; visible sub-tree
         left                                   ; x coordinate of left edge
         top)                                   ; y coordinate of top edge
        ()
  :gettable-instance-variables
  :settable-instance-variables
  :initable-instance-variables)

(defmethod (node :after :init) (&rest ignore)
  (send self ':compute-dimensions))

;------------------------------------------------------------------------------
;
; Node methods I:
; How to draw nodes; including geometry computations
;

; This computes the height and width of a node, based on what strings go
; inside it. Note that this is independent of where on the screen the node is.
;
(defmethod (node :compute-dimensions) (&rest ignore)
  (cond ((not opened?)
         (setq width *closed-width)
         (setq height *closed-height)
         (setq sub-tree-width *closed-width)
         (setq sub-tree-height *closed-height))
        (t (setq width
                 (+ extra-width
                    (loop for attribute in attributes
                          if (send self ':ought-to-print-attribute? attribute)
                          maximize (send pane ':string-length (eval (second attribute))))))
           (setq height
                 (+ (send pane ':vsp)
                    (loop for attribute in attributes
                          if (send self ':ought-to-print-attribute? attribute)
                          sum (+ (send pane ':vsp)
                                 (tv:font-char-height (send pane ':current-font)))))))))



; This is a recursive procedure. You call it on the topmost node
; which you're interested in, and all of its underlings will be
; called with the same :COMPUTE-TOTAL-DIMENSIONS message. Thus, this
; returns the total width and total height (in pixels) of the sub-tree
; beginning with any node. Unopened cells, of course, take up no room.
;
; First, the node's own height and width is computed.
; Then, all the space occupied below is computed (depth-first).
; The value returned is a list of the entire width and the entire height.
; Note that the width is the maximum of this node's width and
; the width of its underlings (max width total-width)
; The height is the sum of three things: this node's height, the vertical
; separation between layers, and the max of the underlings' heights.

; This method makes sure everyone's HEIGHT and WIDTH are up to date.
; The next method, :reposition-underlings, makes sure everyone's
; TOP and LEFT are up to date.
;
(defmethod (node :compute-total-dimensions) (&rest ignore)
  (send self ':compute-dimensions)
  (cond ((not opened?) (list width height))
        ((null below)
         (setq sub-tree-width width)
         (setq sub-tree-height height)
         (list width height))
        (t
         (loop for underling in below
               for under-dim = (send underling ':compute-total-dimensions)
               sum (+ horizontal-separation
                      (first under-dim)) into under-width
               maximize (second under-dim) into under-height
               finally
               (return
                 (progn
                   (setq sub-tree-width (max width (- under-width horizontal-separation)))
                   (setq sub-tree-height (+ height      ; my height
                                            vertical-separation ; height directly under me
                                            under-height))      ; my underlings' height
                   (list sub-tree-width sub-tree-height)))))))


; This is also a recursive procedure which makes sure everyone's
; TOP and LEFT are ok. It does this by finding out where its underlings
; are, and then positioning itself on top of them, in the center.
; Note that this really has to call :COMPUTE-TOTAL-DIMENSIONS first,
; but :COMPUTE-TOTAL-DIMENSIONS doesn't really need to call
; :REPOSITION-UNDERLINGS in order to work. For extra credit: Why is this so?
;
(defmethod (node :reposition-underlings) ()
  (if (not opened?) nil
      (let* ((under-width                               ; the width of all my underlings
               (- (loop for underling in below
                        summing (+ (send underling ':sub-tree-width)
                                   horizontal-separation))
                  horizontal-separation))
             (under-left (- (+ left (* .5 width))       ; middle of this node
                            (* .5 under-width))))       ; left edge of sub-tree
        (loop for underling in below
              for dx first under-left then (+ dx under-sub-tree-width
                                              horizontal-separation)
              for under-sub-tree-width = (send underling ':sub-tree-width)
              do (send underling ':set-top (+ top height vertical-separation))
              (send underling ':set-left
                    (- (+ dx (* .5 under-sub-tree-width))       ; center of sub-tree
                       (* .5 (send underling ':width))))        ; offset of underling's width
              (send underling ':reposition-underlings)))))

; Drawing stuff.
;
; This is the procedure you call on the top node to get the whole tree
; drawn. It draws itself, and then each of its underlings (which, of course
; head subtrees themselves) get drawn the same way. If you have the special
; case that a node is "closed", you give it to the special :DRAW-CLOSED method.
;

(defmethod (node :draw-sub-tree) ()
  (if (not opened?) (send self ':draw-closed)
      (send self ':draw)
      (cond ((> (length below) 1)
             (send self ':draw-under-line)
             (loop for underling in below
                   do (send self ':draw-line-to underling)
                   do (send underling ':draw-sub-tree)))
            ((= (length below) 1)
             (send pane ':draw-line (fix (+ left (* .5 width)))
                                    (fix (+ top height))
                                    (fix (+ left (* .5 width)))
                                    (fix (+ top height vertical-separation -1)))
             (send (car below) ':draw-sub-tree)))))

(defmethod (node :draw-under-line) ()
  (let* ((left-node (first below))
         (right-node (car (last below)))
         (top-x (fix (+ left (* .5 width))))
         (top-y (fix (+ top height)))
         (left-x (fix (+ (send left-node ':left)
                          (* .5 (send left-node ':width)))))
         (right-x (fix (+ (send right-node ':left)
                          (* .5 (send right-node ':width)))))
         (bottom-y (fix (+ top-y (* .5 vertical-separation)))))
    (send pane ':draw-line top-x top-y top-x (1- bottom-y) *alu)
    (send pane ':draw-line left-x bottom-y right-x bottom-y *alu)))

; This draws the line from superior to underling.
; The FIXR operations convert any kind of number into an integer.
; FIX does this by truncation, FIXR rounds off the argument.
; This is because graphics commands can't handle floating point
; for screen coordinates.
;
(defmethod (node :draw-line-to) (underling)
  (let* ((top-x (fix (+ (send underling ':left)
                        (* .5 (send underling ':width)))))
         (top-y (fix (+ top height (* .5 vertical-separation))))
         (bottom-y (fix (- (send underling ':top) 1))))
    (send pane ':draw-line top-x top-y top-x bottom-y *alu)))


; This is called by :DRAW-SUB-TREE. It draws just the box and contents for
; itself, but it doesn't draw connecting lines or any of its underlings.
; If an attribute shouldn't be printed, it doesn't get printed.
; Notice the abstractions here. Such decisions as "How should the edges
; be drawn?" "Should this attribute be printed?" "How should it look?"
; are not decided here. Why did I write these as separate functions?
;
(defmethod (node :draw) (&aux (char-height (tv:font-char-height (send pane ':current-font))))
  (send self ':draw-edges)
  (loop for attribute in attributes
        for print? = (send self ':ought-to-print-attribute? attribute)
        for y first (+ top (send pane ':vsp))
              then (if print? (+ y char-height (send pane ':vsp))
                       y)
        do (if print?
               (send self ':print-attribute attribute y))))

; The box around the node
;
(defmethod (node :draw-edges) ()
  (send pane ':draw-lines *alu
        (fix left)(fix top)
        (fix (+ left width)) (fix top)
        (fix (+ left width)) (fix (+ top height))
        (fix left)(fix (+ top height))
        (fix left)(fix top)))

; This method defines how closed boxes draw themselves. You draw
; only its edges (which were set to be really small in :COMPUTE-DIMENSIONS
; because this node is closed, and then color it gray.

(defmethod (node :draw-closed) ()
  (send self ':draw-edges)
  (send pane ':bitblt *alu (fix (1- width)) (fix (1- height))
        tv:33%-gray 0 0 (fix (1+ left)) (fix (1+ top))))


; Notice the way this implementation allows you to use "*****"
; to represent the notion of needing filling-in, yet you are not
; prohibited from including your Martian employees ("*****" is
; a common Martian family name, you know) on this chart.
; A professional programmer never has "magic" values that might
; cause an innnocent user some grief some day.
;
(defmethod (node :ought-to-print-attribute?) (attribute)
  (and (cdr attribute)
       (not (eq (second attribute) '*needs-filling-in))))


; Notice the idiom (* .5 foo). This is used to figure out half
; of a region, so things get centered nicely.
;
(defmethod (node :print-attribute) (attribute y)
  (let* ((string (eval (second attribute)))
         (free-space (- width (send pane ':string-length string))))
    (selectq justification
      (:center (label pane (+ left (* .5 free-space)) y string))
      (:left (label pane (+ (* .5 extra-width) left) y string))
      (:right (label pane (+ left free-space (* -.5 extra-width)) y string)))))


; The following is a short-hand notation for the corrected :STRING-OUT message.
;
(defun label (window left top string)
  (if  (or (< top 0) (< left 0)) nil
    (send window ':set-cursorpos (fix left) (correct window top))
    (send window ':string-out string)))

(defun correct (window top)
  (fix (+ top
          (tv:font-baseline (send window ':current-font))
          (- (tv:font-baseline (aref (send window ':font-map)
                                        (send window ':largest-font)))))))

; The string-out message is very useful, since it's a very explicit
; way of getting typout on a screen. Some other favorite ways of printing
; include (this is by no means an exhaustive list):
;
; (send <window> ':draw-char <font> <char> <x> <y>)
; (format <window> <format-string> . <args>)
; (print <string> <window>)
; (princ <string> <window>)
; (prin1 <string> <window>)
; (send <window> ':tyo <char>)

;------------------------------------------------------------------------------
;
; Node methods II:
; Maintaining the contents
;
; There's several ways you may want to edit the attributes, and
; rather than bother the user with one big menu, this is an exercise
; in menu hacking.
;
; In creating a menu, you must specify everything the computer cannot
; figure out for itself. For example, things you must specify include:
;
; What the items to choose from are; what to do when you select one of them,
; what to do when you DON'T select one of them, what the mouse documentation
; who-line should say when you're pointing at the item, etc.
;
; Every menu has an <item-list> which, for each item, specifies
; just what kind of actions and items you have. For more information
; on the different kinds of menus, consult the Window System Manual.
;
(defmethod (node :edit) ()
         (send pane ':set-current-node self)
         (tv:menu-choose '(("Modify Contents" :eval (send (send *chart-pane
                                                                ':current-node)
                                                          ':fill-in-self)
                            :documentation "Modify the contents of this node")
                           ("Select Attributes" :eval (send (send *chart-pane
                                                                  ':current-node)
                                                            ':select-attributes)
                            :documentation "Remove one or more attributes of this node")
                           ("Change Geometry" :eval (send (send *chart-pane
                                                                ':current-node)
                                                          ':change-geometry)
                            :documentation "Change the display features of this node")))
         (send pane ':refresh))

; Choose-variable-values only modifies the values of variables.
; I wanted to modify the elements of a list, so I needed to GENSYM some new
; variables. GENSYM creates a symbol that is guaranteed brand-new,
; and I generate as many as I need to hold all the attributes.
; Then I call up a CHOOSE-VARIABLE-VALUES on these variables, and
; put everything back when I'm finished.
;
(defmethod (node :fill-in-self) ()
  (let ((vars (loop for attribute in attributes
                    for var = (gensym 'attribute)
                    do (setf (plist var)
                             (append (plist var)
                                     '(si:documentation-property (("variable"))
                                       special t)))
                       (set var (second attribute))
                    collect var))
        (base 10.)
        (ibase 10.))
    (tv:choose-variable-values
      (loop for var in vars
            for attribute in attributes
            collecting (list var
                             (first attribute)
                             ':string))
      ':label "Fill in as appropriate:"
      ':near-mode '(:mouse)
      ':margin-choices '("Mouse here when done"))
    (setq attributes
          (loop for var in vars
            for attribute in attributes
            collecting (list (first attribute) (eval var))))))


; Multiple-choose menus provide columns and rows of boxes.
; Each box can be on or off, and turning it on or off may cause other boxes
; to turn on or off. Consult the Window System Manual for more info.
;
; The items are labeled with strings like "Name: Mary" if a value is found,
; and a string like "Name" if not found. Notice the use of back-quoted lists
; and commas in order to control creation of the item list.
;
(defmethod (node :select-attributes) ()
  (let ((selections
          (tv:multiple-choose
            "This node's attributes"
            (loop for default-attribute in *default-attributes
                  for found = (assoc (first default-attribute) attributes)
                  collecting
                  (if found
                      (copytree (list found (string-append (first found) ":  " (second found))
                            '((:keep t t nil nil) (:remove nil t nil nil))))
                      ; if not found
                    (copytree (list default-attribute (first default-attribute)
                               '((:keep nil t nil nil) (:remove t t nil nil))))))
            (copytree '((:keep " Keep " nil t t nil)
              (:remove " Remove " nil t t nil))))))
    (if selections
        (setq attributes (loop for item in selections
                               if (eq (second item) ':keep)
                               collect (first item))))))

; This allow a user to modify the appearance of a box, or the separation
; of a box's underlings. This is in a different menu from the other two,
; because the nature of the information is rather different.
;
(defmethod (node :change-geometry) ()
  (let* ((base 10.)
         (ibase 10.)
         (properties '(justification extra-width
                       vertical-separation horizontal-separation))
         (vars (loop for property in properties
                     for var = (gensym 'prop)
                     do (setf (plist property)
                              (append (plist property)
                                      '(si:documentation-property (("variable"))
                                        special t)))
                     (setf (plist var)
                              (append (plist var)
                                      '(si:documentation-property (("variable"))
                                        special t)))
                        (set var (send self ':eval-inside-yourself property))
                     collect var)))
    (tv:choose-variable-values
      (loop for var in vars
            for doc in '(("Justification" :choose (:center :left :right))
                         ("Extra box width (10 is good)" :number)
                         ("Vertical distance to underlings" :number)
                         ("Separation between underlings" :number))
            collecting (cons var doc))
      ':label "All numbers are in pixels:"
      ':near-mode '(:mouse)
      ':margin-choices '("Mouse here when done"))
    (loop for property in properties
          for var in vars
          do (send self ':eval-inside-yourself `(setq ,property ,var)))))

;------------------------------------------------------------------------------
;
; Node methods III:
; Adding and deleting nodes


; Yes Virginia, FIND-POSITION-IN-LIST, FIRSTN, and NTHCDR are lisp primitives!
; Aren't you glad some nut wrote them for us?
;
(defmethod (node :left-siblings-of) (node)
  (if (not (memq node below)) nil
      (firstn (find-position-in-list node below) below)))

(defmethod (node :right-siblings-of) (node)
  (if (not (memq node below)) nil
      (cdr (nthcdr (find-position-in-list node below) below))))

; Create a new underling
;
(defmethod (node :add-below) ()
  (let ((new (make-instance 'node ':pane pane
                            ':horizontal-separation *default-h-separation
                            ':vertical-separation *default-v-separation)))
    (setq below (append below (list new)))
    (send new ':set-above self)
    (send new ':fill-in-self)
    (send pane ':refresh)
    (send pane ':set-current-node new)))

; Create a new superior. Don't forget to inform the superior's superior
; about this, and if you've made a brand-new top-node for the chart,
; inform the chart-pane too, while you're at it.
;
(defmethod (node :add-above) ()
  (let ((new (make-instance 'node ':pane pane
                            ':horizontal-separation *default-h-separation
                            ':vertical-separation *default-v-separation)))
    (send new ':set-above above)
    (send new ':set-below (list self))
    (setq above new)
    (cond ((eq self (send pane ':top-node))     ; If a new top is created
           (send pane ':set-top-node new)
           (send new ':fill-in-self)
           (send pane ':go-to-top))
          ((send new ':above)                   ; Else we must inform it of a new inferior
           (send (send new ':above) ':set-below
                 (append (send (send new ':above) ':left-siblings-of self)
                         (list new)
                         (send (send new ':above) ':right-siblings-of self)))
           (send new ':fill-in-self)
           (send pane ':refresh)
           (send pane ':set-current-node new)))))

; A new sibling
;
(defmethod (node :add-right) ()
  (if (null above)
      (beep-on-terminal)
      (let ((new (make-instance 'node ':pane pane
                            ':horizontal-separation *default-h-separation
                            ':vertical-separation *default-v-separation)))
        (send new ':set-above above)
        (send above ':set-below
              (append (send above ':left-siblings-of self)
                      (list self new)
                      (send above ':right-siblings-of self)))
        (send new ':fill-in-self)
        (send pane ':refresh)
        (send pane ':set-current-node new))))


; A new sibling
;
(defmethod (node :add-left) ()
  (if (null above)
      (beep-on-terminal)
      (let ((new (make-instance 'node ':pane pane
                            ':horizontal-separation *default-h-separation
                            ':vertical-separation *default-v-separation)))
        (send new ':set-above above)
        (send above ':set-below
              (append (send above ':left-siblings-of self)
                      (list new self)
                      (send above ':right-siblings-of self)))
        (send new ':fill-in-self)
        (send pane ':refresh)
        (send pane ':set-current-node new))))

(defmethod (node :remove-self) ()
  (cond ((null below)
         (if (null above) (beep-on-terminal)
             (send above ':set-below (remq self (send above ':below)))
             (send pane ':set-current-node above)
             (send pane ':refresh)))
        ((null above)
         (if (< 1 (length below)) (beep-on-terminal)
             (send (car below) ':set-above nil)
             (send pane ':set-top-node (car below))
             (send pane ':go-to-top)))
        (t (send above ':set-below
                 (append (send above ':left-siblings-of self)
                         below
                         (send above ':right-siblings-of self)))
           (loop for orphan in below
                 do (send orphan ':set-above above))
           (send pane ':set-current-node above)
           (send pane ':refresh))))

; This keeps a node, and eliminates all subtrees
;
(defmethod (node :remove-below) ()
  (if (null below) (beep-on-terminal)
      (setq below nil)
      (send pane ':refresh)))

;------------------------------------------------------------------------------
;
; User queries

; This prompts the user on the prompt pane for a yes or no answer.
; It cleans up the prompt pane when finished by sending a :REFRESH message.
;
(defun let-user-confirm (query-string)
  (let ((answer (fquery () query-string)))
    (send *prompt-pane ':refresh)
    answer))

; This beeps on (and maybe flashes) the user's terminal.
;
(defun beep-on-terminal ()
  (send terminal-io ':beep))

;------------------------------------------------------------------------------
; The pane with the organization chart
;

(defflavor chart-pane
        ((top-node nil)
         (current-node)
         (font 3)
         (largest-font 4)
         (smallest-font 0))
        (tv:list-mouse-buttons-mixin tv:truncating-window)
  (:documentation
   "ORG Chart pane")
  (:default-init-plist
    :font-map '(fonts:5x5 fonts:tr8 fonts:tr10 fonts:tr12 fonts:mets)
    :save-bits t
    :blinker-flavor 'tv:box-blinker
    :label nil)                                 ; no window label
  :gettable-instance-variables
  :settable-instance-variables
  :initable-instance-variables)

(defmethod (chart-pane :after :init) (&rest ignore)
  (setq top-node (make-instance 'node ':pane self
                            ':horizontal-separation *default-h-separation
                            ':vertical-separation *default-v-separation))
  (setq current-node top-node)
  (send current-node ':set-top (* .1 (send self ':height)))
  (send current-node ':set-left
        (- (* .5 (send self ':width))
           (* .5 (send current-node ':width)))))


(defmethod (chart-pane :after :refresh) (&rest ignore)
  (send self ':set-current-font font)
  (send top-node ':compute-total-dimensions)
  (send top-node ':reposition-underlings)
  (send top-node ':draw-sub-tree)               ; This actually draws the whole graph
  (send self ':update-blinker))

(defmethod (chart-pane :after :set-current-node) (ignore)
  (send self ':scroll-if-necessary)
  (send self ':update-blinker))

(defmethod (chart-pane :update-blinker) ()
  (let ((blinker (car (send self ':blinker-list))))
    (send blinker ':set-size
          (+ 5 (send current-node ':width))
          (+ 5 (send current-node ':height)))
    (send blinker ':set-cursorpos
          (- (send current-node ':left) 2)
          (- (send current-node ':top) 2))
    (send blinker ':set-visibility t)))

;------------------------------------------------------------------------------
;
; The command pane's functions
;

; File storage stuff
;
(defmethod (chart-pane :save-as-file) ()
  (with-open-file
    (file (send self ':get-file-name-from-user "Save this graph into which file?") ':write)
    (format file ";-*- Mode:LISP; Package:(ORG :USE (USER GLOBAL SYSTEM));  Base:10. -*- ~3%")
    (format file "; This file was generated by the ORG program. It contains the ~%")
    (format file "; information necessary to reconstruct a chart from scratch. ~%")
    (format file "; ~%")
    (format file "; Modify this file only if you really know what you're doing.~%")
    (format file "; It's much better to run ORG, then load this file, modify it,~%")
    (format file "; and save out the preferred version.~2%")
    (format file "; This is the top node:")
    (dump-world t file (send top-node ':dump-subtree))
    (format file "~3%; That's all, folks!"))
  (format t "Done.~%"))

(defun dump-world (top-node? stream string-tree)
  (if (null string-tree) nil
      (let ((node-string (first string-tree))
            (symbol (second string-tree))
            (kids (third string-tree)))
        (format stream "~2%(defconst ~A ~A)" symbol node-string)
        (if (not top-node?) nil
            (format stream "~2%(send *chart-pane ':set-top-node ~A)" symbol)
            (format stream "~%(send ~A ':set-pane *chart-pane)" symbol))
        (loop for kid in kids
              do (dump-world nil stream kid)
              do (format stream "~%(send ~A ':greet-new-inferior ~A)"
                      symbol (second kid))))))

(defmethod (node :greet-new-inferior) (node)
  (setq below (append below (list node)))
  (send node ':set-above self))


(defmethod (node :dump-subtree) ()
  (let ((nodename (gensym 'node)))
       (list (send self ':dump-string)
        nodename
        (loop for kid in below
              collect (send kid ':dump-subtree)))))


(defmethod (node :dump-string) ()
  (let ((base 10.))
    (string-append
      (format nil "~% (make-instance 'node ~%")
      (format nil   "      ':pane *chart-pane ~%")
      (format nil   "      ':opened? ~S ~%" opened?)
      (format nil   "      ':attributes~%")
      (format nil   "          '~S ~%" attributes)
      (format nil   "      ':justification ':~S ~%" justification)
      (format nil   "      ':extra-width ~S ~%" extra-width)
      (format nil   "      ':vertical-separation ~S ~%" vertical-separation)
      (format nil   "      ':horizontal-separation ~S)" horizontal-separation))))


(defmethod (chart-pane :get-file-name-from-user) (prompt-string)
  (let ((fn (prompt-and-read `(:pathname :defaults ,*file)
                             "~%~A (default: ~A) " prompt-string *file)))
    (setq *file (fs:merge-pathname-defaults fn))))

(defmethod (chart-pane :restore-from-file) ()
  (cond ((fquery ()
          "Are you sure you want to wipe out everything and load a brand-new chart? ")
         (load (send self ':get-file-name-from-user "File to load"))
         (send self ':go-to-top)
         (format t "Done.~%"))))

;------------------------------------------------------------------------------
;
; Font nonsense
;
(defmethod (chart-pane :make-bigger) ()
  (if (equal font largest-font)
      (beep-on-terminal)
      (setq font (1+ font))
      (send self ':set-current-font font))
  (send self ':refresh))

(defmethod (chart-pane :make-smaller) ()
  (if (equal font smallest-font)
      (beep-on-terminal)
      (setq font (1- font))
      (send self ':set-current-font font))
  (send self ':refresh))

(defmethod (chart-pane :change-parameters) ()
  (let ((base 10.)
        (ibase 10.))
    (tv:choose-variable-values
      `((*closed-width "Width of a closed box (15 is good)" :number)
        (*closed-height "Height of a closed box (8 is good)" :number)
        (*default-v-separation "Default space under a node (10 is good)" :number)
        (*default-h-separation "Default space between nodes (10 is good)" :number)
        (*scroll-amount "How big a jump to make when scrolling (100 is good)" :number)
        (*file "The pathname of the current file" :pathname)))))

(defmethod (chart-pane :go-to-top) ()
  (setq current-node top-node)
  (send current-node ':set-top (* .1 (send self ':height)))
  (send current-node ':set-left
        (- (* .5 (send self ':width))
           (* .5 (send current-node ':width))))
  (send self ':refresh))

(defmethod (chart-pane :go-up) ()
  (let ((boss (send current-node ':above)))
    (if (null boss)
        (beep-on-terminal)
        (send self ':set-current-node boss))))

(defmethod (chart-pane :go-down) ()
  (let ((underlings (send current-node ':below)))
    (if (or (not (send current-node ':opened?)) (null underlings))
        (beep-on-terminal)
        (send self ':set-current-node (car underlings)))))

(defmethod (chart-pane :go-left) ()
  (let ((boss (send current-node ':above)))
    (if (null boss)
        (beep-on-terminal)
        (let ((left-kid (car (last (send boss ':left-siblings-of current-node)))))
         (cond ((null left-kid)
                (send self ':go-up))
               (t
                (send self ':set-current-node left-kid)))))))


(defmethod (chart-pane :go-right) ()
  (let ((boss (send current-node ':above)))
    (if (null boss)
        (send self ':go-down)
        (let ((right-kid (car (send boss ':right-siblings-of current-node))))
          (cond ((and (null right-kid) (null (send current-node ':below)))
                 (beep-on-terminal))
                ((null right-kid)
                 (send self ':go-down))
                (t
                  (send self ':set-current-node right-kid)))))))

(defmethod (chart-pane :far-right) ()
  (let ((boss (send current-node ':above)))
    (if (null boss)
        (beep-on-terminal)
        (let ((right-kid (car (last (send boss ':right-siblings-of current-node)))))
          (cond ((null right-kid)
                 (beep-on-terminal))
                (t
                  (send self ':set-current-node right-kid)))))))

(defmethod (chart-pane :far-left) ()
  (let ((boss (send current-node ':above)))
    (if (null boss)
        (beep-on-terminal)
        (let ((left-kid (car (send boss ':left-siblings-of current-node))))
          (cond ((null left-kid)
                 (beep-on-terminal))
                (t
                  (send self ':set-current-node left-kid)))))))

; The following three methods are needed in case you :GO-DOWN or :GO-RIGHT,
; etc. to a node which lies off the screen.
;
(defmethod (chart-pane :scroll-if-necessary) ()
  (if (or (send self ':fix-horizontal?)
          (send self ':fix-vertical?))
      (send self ':refresh)
      (send self ':update-blinker)))

; If the node is on screen, this returns nil and nothing happens.
; If the node lies off-screen, the top-node is moved to compensate,
; and T is returned. Thus, :SCROLL-IF-NECESSARY will only refresh once,
; even if we need to scroll both horizontally and vertically.
;
; For a discussion on the existance of the 2's and the 7's in the code, see the
; documentation for the scrolling methods (e.g. :SCROLL-UP, :SCROLL-LEFT, etc.)
;
(defmethod (chart-pane :fix-horizontal?) ()
  (let ((new-left (send current-node ':left))
        (origin-left (send top-node ':left))
        (right-boundary (- (send self ':width)
                           7 (send current-node ':width))))
    (cond ((and (>= new-left 2) (<= new-left right-boundary))
           nil)                                 ; OK, return nil
          ((< new-left 2)
           (send top-node ':set-left (+ origin-left (- 2 new-left)))
           t)                                   ; Was bad, return t
          (t
           (send top-node ':set-left (- origin-left (- new-left right-boundary)))
           t))))                                ; Was bad, return t

(defmethod (chart-pane :fix-vertical?) ()
  (let ((new-top (send current-node ':top))
        (origin-top (send top-node ':top))
        (bottom-boundary (- (send self ':height)
                           7 (send current-node ':height))))
    (cond ((and (>= new-top 2) (<= new-top bottom-boundary))
           nil)                                 ; OK, return nil
          ((< new-top 2)
           (send top-node ':set-top (+ origin-top (- 2 new-top)))
           t)                                   ; Was bad, return t
          (t
           (send top-node ':set-top (- origin-top (- new-top bottom-boundary)))
           t))))                                ; Was bad, return t

(defmethod (chart-pane :add-below) ()
  (send current-node ':add-below))

(defmethod (chart-pane :add-right) ()
  (send current-node ':add-right))

(defmethod (chart-pane :add-left) ()
  (send current-node ':add-left))

(defmethod (chart-pane :add-above) ()
  (send current-node ':add-above))

(defmethod (chart-pane :open) ()
  (send current-node ':set-opened? t)
  (send self ':refresh))

(defmethod (chart-pane :open-below) ()
  (loop for kid in (send current-node ':below)
        do (send kid ':set-opened? t))
  (send self ':refresh))

(defmethod (chart-pane :close) ()
  (send current-node ':set-opened? nil)
  (send self ':refresh))

(defmethod (chart-pane :close-below) ()
  (loop for kid in (send current-node ':below)
        do (send kid ':set-opened? nil))
  (send self ':refresh))

(defmethod (chart-pane :edit) ()
  (send current-node ':edit))

(defmethod (chart-pane :make-this-top) ()
  (cond ((let-user-confirm "Are you sure you want to wipe out everything above this node? ")
         (setq top-node current-node)
         (send self ':go-to-top))
        (t nil)))

(defmethod (chart-pane :remove-current) ()
  (cond ((eq current-node top-node)
         (beep-on-terminal))
        ((let-user-confirm "Are you sure you want to wipe out this node? ")
         (send current-node ':remove-self))
        (t nil)))

(defmethod (chart-pane :remove-below) ()
  (cond ((let-user-confirm "Are you sure you want to wipe out everybody under this node? ")
         (send current-node ':remove-below))
        (t nil)))

; First, compute DY (the amount to move up), and HERE (where you are now).
; By taking the MIN of the default *SCROLL-AMOUNT and our current-node's
; position, you ensure you never move in a step large enough to take
; your current node off the screen.
;
; "Why do you subtract 2?" you may ask... This ensures that you never
; bring the current-node to a y-coordinate of less than +2, not zero.
; Leaving it at +2 gives it a few aesthetic pixels of upper border.
; Remember, your craftsmanship and attention to aesthetic detail
; really shows in applications programs like this.
;
(defmethod (chart-pane :scroll-up) ()
  (let ((dy (min *scroll-amount (- (send current-node ':top) 2)))
        (here (send top-node ':top)))
    (send top-node ':set-top (- here dy))
    (if (not (zerop dy)) (send self ':refresh))))

; The 7 here is like the 2 above. It includes compensation for the border drawn
; around the current-node, plus the screen's blinker, which is a blinker of
; TV:BOX-BLINKER flavor and thickness 2.
;
(defmethod (chart-pane :scroll-down) ()
  (let ((dy (min *scroll-amount
                 (- (send self ':height) 7
                    (send current-node ':top) (send current-node ':height))))
        (here (send top-node ':top)))
    (send top-node ':set-top (+ here dy))
    (if (not (zerop dy)) (send self ':refresh))))

(defmethod (chart-pane :scroll-left) ()
  (let ((dx (min *scroll-amount (- (send current-node ':left) 2)))
        (here (send top-node ':left)))
    (send top-node ':set-left (- here dx))
    (if (not (zerop dx)) (send self ':refresh))))

(defmethod (chart-pane :scroll-right) ()
  (let ((dx (min *scroll-amount
                 (- (send self ':width) 7
                    (send current-node ':left) (send current-node ':width))))
        (here (send top-node ':left)))
    (send top-node ':set-left (+ here dx))
    (if (not (zerop dx)) (send self ':refresh))))


;------------------------------------------------------------------------------
; The command pane
;

(defflavor org-command-pane ()
           (tv:command-menu)
  :gettable-instance-variables
  :settable-instance-variables
  :initable-instance-variables
  (:documentation
   "ORG Command Pane")
  (:default-init-plist
    :font-map '(fonts:hl12bi)
    :columns 4
    :item-list *command-list))

(defvar *column-1-commands
  '(("Save as File (c-S)" :save-as-file
                "Save current organization chart in a file")
               ("Read from File (c-R)" :restore-from-file
                "Restore organization chart previously saved in a file")
               ()
               ()
               ("Far Left (c-A)" :far-left
                "Go to the furthest left on this level")
               ("Far Right (c-E)" :far-right
                "Go to the furthest right on this level")
               ("Parameters (m-X)" :change-parameters
                "Change the parameters for the whole chart")
               ("Edit This Node (c-X)" :edit
                "Edit this individual")))

(defvar *column-2-commands
  '(("Add Below (h-N)" :add-below
                "Add a new inferior")
               ("Add Left (h-B)" :add-left
                "Add a new sibling to the left")
               ("Add Right (h-F)" :add-right
                "Add a new sibling to the right")
               ("Add Above (h-P)" :add-above
                "Insert a new node above this one")
               ("Go Up (c-P)" :go-up
                "Move the current node upwards")
               ("Go Down (c-N)" :go-down
                "Move the current node downwards")
               ("Go Left (c-B)" :go-left
                "Move the current node to the left")
               ("Go Right (c-F)" :go-right
                "Move the current node upwards")))

(defvar *column-3-commands
  '(("Delete (c-D)" :remove-current
                "Delete exactly this node. (Keep its underlings, if any)")
               ("Delete Below (m-D)" :remove-below
                "Delete everything under this node")
               ("Go to the Top (m-<)" :go-to-top
                "Go to the top of the chart")
               ()
               ("Scroll Up (m-P)" :scroll-up
                "Move the display upwards")
               ("Scroll Down (m-N)" :scroll-down
                "Move the display downwards")
               ("Scroll Left (m-B)" :scroll-left
                "Move the display to the left")
               ("Scroll Right (m-F)" :scroll-right
                "Move the display to the right")))

(defvar *column-4-commands
  '(("Open (c-O)" :open
                "Open up the current node")
               ("Open Below (m-O)" :open-below
                "Open up the underlings")
               ("Close (c-C)" :close
                "Close the current node")
               ("Close Below (m-C)" :close-below
                "Close the underlings")
               ()
               ()
               ("Make Bigger (c->)" :make-bigger
                "Expand this chart")
               ("Make Smaller (c-<)" :make-smaller
                "Shrink this chart")))

(defun select-command-items (command-column)
  (if (null command-column)
       '("" :no-select nil)
       `(,(first command-column)
         :eval (send *chart-pane ',(second command-column))
         :documentation ,(third command-column))))

(defvar *command-list
    (loop for command-column-1 in *column-1-commands
          for command-column-2 in *column-2-commands
          for command-column-3 in *column-3-commands
          for command-column-4 in *column-4-commands
          collecting (select-command-items command-column-1)
          collecting (select-command-items command-column-2)
          collecting (select-command-items command-column-3)
          collecting (select-command-items command-column-4)))

;;; Note that, in item-list values, ("" :no-select nil) can be used for dummy
;;; (blank) menu items.


;------------------------------------------------------------------------------
;
; The pane at the bottom of the screen
;
(defflavor prompt-pane
        ()
        (tv:pane-mixin tv:window))

(defmethod (prompt-pane :after :refresh)(&rest ignore)
  (send self ':home-cursor))

;------------------------------------------------------------------------------
; Org Chart Frame: The master window with many panes
;

(defflavor chart-frame
  ()
  (tv:process-mixin
   tv:bordered-constraint-frame-with-shared-io-buffer)
  :gettable-instance-variables
  :settable-instance-variables
  :initable-instance-variables
  (:documentation
   "Organization Chart Program Frame")
  (:default-init-plist
   :expose-p t                                          ; expose w/o blink on instantiation
   :activate-p t                                        ; activate on instantiation
   :save-bits ':delayed                                 ; make save bits array on deexposure
   :process '(org-initial-function)
   :panes
   `((command-pane org-command-pane)
     (chart-pane chart-pane)
     (prompt-pane prompt-pane))
   :constraints
   '((standard-configuration
       (command-pane chart-pane prompt-pane)            ; top, middle, bottom
       ((command-pane :ask :pane-size))                 ; As big as necessary
       ((prompt-pane 8 :lines))                         ; 8 lines tall
       ((chart-pane :even))))))                         ; Whatever's left over

; Look at the :PROCESS init-keyword above (in the defflavor).
; This function gets run as the "initial form" of that process.
; It gets an infinite loop running which maintains the window.
;
(defun org-initial-function (window)
  (send window ':loop))

; This is org's top level loop.
; It does all the work of getting input from the user, and doing
; something intelligent with it.
;
(defmethod (chart-frame :loop) ()
  (let* ((io (send self ':get-pane 'prompt-pane))
         (chart (send self ':get-pane 'chart-pane))
         (terminal-io io)
         (query-io io)
         (error-output io))
    (loop for input = (send io ':any-tyi)
          do (cond ((atom input)
                    (selectq input
                      ((#\c-L
                        #+LMI #\clear-screen)
                       (send chart ':refresh)
                       (send io ':refresh))
                      (#\c-F (send chart ':go-right))
                      (#\c-B (send chart ':go-left))
                      (#\c-P (send chart ':go-up))
                      (#\c-N (send chart ':go-down))
                      (#\c-A (send chart ':far-left))
                      (#\c-E (send chart ':far-right))
                      (#\m-F (send chart ':scroll-right))
                      (#\m-B (send chart ':scroll-left))
                      (#\m-P (send chart ':scroll-up))
                      (#\m-N (send chart ':scroll-down))
                      (#\h-F (send chart ':add-right))
                      (#\h-B (send chart ':add-left))
                      (#\h-P (send chart ':add-above))
                      (#\h-N (send chart ':add-below))
                      (#\c-D (send chart ':remove-current))
                      (#\m-D (send chart ':remove-below))
                      (#\c-O (send chart ':open))
                      (#\m-O (send chart ':open-below))
                      (#\c-C (send chart ':close))
                      (#\m-C (send chart ':close-below))
                      (#\c-S (send chart ':save-as-file))
                      (#\c-R (send chart ':restore-from-file))
                      (#\c-> (send chart ':make-bigger))
                      (#\c-< (send chart ':make-smaller))
                      (#\m-< (send chart ':go-to-top))
                      (#\c-X (send chart ':edit))
                      (#\end (send self ':bury))
                      (#\m-X (send chart ':change-parameters))))
                   ((listp input)
                    (selectq (car input)
                      (:menu
                       (send (fourth input) ':execute (second input)))
                      (t (beep))))))))

;------------------------------------------------------------------------------
;
; This is how you get ORG up and running in the first place
; Notice that this function always returns the same window,
; namely, *CHART-PANE.  When you type System-O, it executes
; the function ORG, and selects whatever window is returned
; be ORG.

(defun org ()
  (if (and (boundp '*chart-frame)
           (not (eq ':unbound *chart-frame))
           *chart-pane)
      *chart-pane
          ;else
      (setq *chart-frame (tv:make-window 'chart-frame))
      (setq *chart-pane (send *chart-frame ':get-pane 'chart-pane))
      (setq *command-pane (send *chart-frame ':get-pane 'command-pane))
      (setq *prompt-pane (send *chart-frame ':get-pane 'prompt-pane))
      *chart-pane))

; This means you never have to type (org). Just type SYSTEM-O and
; it will find the chart-frame and display it for you.
;
(tv:add-system-key #\O '(org) "Organization Chart")

; This will get printed whenever you load this file

(format t "~%Org loaded. Type SYSTEM-O to begin.~2%")
