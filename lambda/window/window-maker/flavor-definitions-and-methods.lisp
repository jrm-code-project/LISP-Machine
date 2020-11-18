;;;-*- Mode:LISP; Package:WINDOW-MAKER; Base:8; Readtable:ZL -*-
;;; Copyright C LISP MACHINE INC., 1985.
;;;

;;;       A frame will be taken as a dummy window having a list of panes


(defflavor frame
           ((owner nil)
            (list-of-border-lines nil)
            left right top bottom
            (list-of-panes-or-frames nil)
            (direction-of-slice nil)
            (relative-size 1.0)
            keyword
            (number nil) name-of-frame)
           ()
  :inittable-instance-variables
  :gettable-instance-variables
  :settable-instance-variables)

(tv:add-typeout-item-type *item-type-alist* frame "help" :help t "help" fonts:cptfontb)
(tv:add-typeout-item-type *item-type-alist* frame "horizontal split" :horizontal-split nil "horizontal split" fonts:cptfontb)
(tv:add-typeout-item-type *item-type-alist* frame "vertical split"  :vertical-split nil "vertical split" fonts:cptfontb)
(tv:add-typeout-item-type *item-type-alist* frame "kill" :kill nil "kill frame" fonts:cptfontb)

(defflavor pane
           ((owner nil)
            (relative-size 1.0)
            (type 'tv:window)
            (init-options nil)
            left right top bottom
            keyword name-of-pane
            (number nil)
            (list-of-border-lines nil))
           ()
  :inittable-instance-variables
  :gettable-instance-variables
  :settable-instance-variables)

(tv:Add-typeout-item-type *item-type-alist* pane "help" :help t "help" fonts:cptfontb)
(tv:add-typeout-item-type *item-type-alist* pane "kill" :kill nil "kill pane" fonts:cptfontb)
(tv:add-typeout-item-type *item-type-alist* pane "horizontal split"
                          :horizontal-split nil "split horizontally pane" fonts:cptfontb)
(tv:add-typeout-item-type *item-type-alist* pane "Insert new pane" :insert-new-pane nil "Insert new pane" fonts:cptfontb)
(tv:add-typeout-item-type *item-type-alist* pane "vertical split" :vertical-split nil "split vertically pane" fonts:cptfontb)


(defflavor line
           (x-position y-position owner1 owner2 direction)
           ()
  :inittable-instance-variables
  :settable-instance-variables
  :gettable-instance-variables)

;;Not really implemented
;;(tv:add-typeout-item-type *item-type-alist* line "drag it with mouse" :drag)
;; (setq *item-type-alist* (remove 'line *item-type-alist* :key #'car))

(defmethod (line :replace-element) (old new)
  (if (eq owner1 old)
      (setq owner1 new)
    (setq owner2 new)))

(defmethod (line :get-other-window) (pane-or-frame)
  (if (eq owner1 pane-or-frame)
      owner2 owner1))

(defmethod (line :is-it-it?) (point-of-slicing)
  (selectq direction
    (:horizontal (> y-position point-of-slicing))
    (:vertical (> x-position point-of-slicing))))

(defmethod (line :set-mouse-region) ()
  (nconc (list 'line self)
         (selectq direction
           (:horizontal (list x-position (1- y-position) (funcall owner1 ':right) (1+ y-position)))
           (:vertical (list (1- x-position) y-position  (1+ x-position) (funcall owner1 ':bottom))))))

(defmethod (pane :set-mouse-region) ()
  (list 'pane self (+ left 4) (+ top 4) (- right 2) (- bottom 2)))

(defmethod (frame :set-mouse-region) ()
  (list 'frame self (+ left 4) (+ top 4) (- right 2) (- bottom 2)))

(defun get-element-to-delete (*mouse-sensitive-items-of-window* pane)
  (loop for item in *mouse-sensitive-items-of-window*
        as object = (second item)
        when (eq object pane) do (return item)))

(defun get-line-to-delete-from-list (pane1 point-of-slicing)
  (loop for line in (funcall pane1 ':list-of-border-lines)
        when (funcall line ':is-it-it? point-of-slicing)
        do (return line)))

(defmethod (frame :get-both-panes) (point-of-slicing key1 key2)
  (let* ((pane1 (loop for pane in list-of-panes-or-frames
                      as left-of-pane = (funcall pane ':left)
                      as right-of-pane = (funcall pane ':right)
                      as top-of-pane = (funcall pane ':top)
                      as bottom-of-pane = (funcall pane ':bottom)
                      when (and (> point-of-slicing (if (equal direction-of-slice ':horizontal)
                                                        top-of-pane left-of-pane))
                                (< point-of-slicing (if (equal direction-of-slice ':horizontal)
                                                        bottom-of-pane right-of-pane)))
                      do (return pane)))
         pane2 left1 top1 right1 bottom1
         (line (get-line-to-delete-from-list pane1 point-of-slicing)))
    (multiple-value (left1 top1 right1 bottom1) (funcall pane1 ':get-slots))
    (delete-element-from-list list-of-panes-or-frames pane1)
    (draw-box left1 top1 right1 bottom1)
    (selectq direction-of-slice
      (:horizontal
       (setq pane2 (make-instance 'pane :left left :right right
                                  :top (1+ point-of-slicing) :bottom bottom1 :owner self :keyword key2))
       (funcall pane1 ':set-bottom (1- point-of-slicing)))
      (:vertical
       (setq pane2 (make-instance 'pane :left (1+ point-of-slicing) :right right1
                                  :top top :bottom bottom :owner self :keyword key2))
       (funcall pane1 ':set-right (1- point-of-slicing))))
    (funcall pane1 ':delete-line-from-border-list line)
    (funcall pane1 ':set-keyword key1)
    (values pane1 pane2 line)))

(defmethod (pane :delete-line-from-border-list) (line)
  (delete-element-from-list list-of-border-lines line))

(defmethod (pane :update-list-of-border-lines) (new-line)
  (update-list list-of-border-lines new-line))

(defmethod (frame :delete-line-from-border-list) (line)
  (delete-element-from-list list-of-border-lines line))

(defmethod (frame :update-list-of-border-lines) (new-line)
  (update-list list-of-border-lines new-line))

(defmethod (frame :get-slots) ()
  (values left top right bottom owner keyword number list-of-border-lines))


(defun replace-element-in-list-by-new-one (list old-element new-element operation)
  (loop for item in list
        for index from 0 to (1- (length list))
        when (eq (if operation (funcall operation item) item) old-element)
        do (if (not operation)
               (progn (rplaca (nthcdr index list) new-element) (return t))
             (rplacd (cdr item) new-element))))

(defmethod (pane :get-slots) ()
  (values left top right bottom owner keyword number list-of-border-lines))

(defmethod (line :update-owner) (old new)
  (if (eq owner1 old) (setq owner1 new)
    (setq owner2 new)))

(defun create-new-frame (pane direction)
  (let (new-frame top right left bottom owner key)
    (setq key (funcall pane ':keyword))
    (setq key (if (member key '(:LINES :CHARACTERS :PIXELS)) ':PERCENTWISE key))
    (multiple-value (left top right bottom owner) (funcall pane ':get-slots))
    (setq new-frame (make-instance 'frame :owner owner :left left :right right :top top :bottom bottom
                                   :list-of-panes-or-frames (ncons pane) :direction-of-slice direction
                                   :list-of-border-lines (funcall pane ':list-of-border-lines)
                                   :keyword key :number (funcall pane ':number)))
    (funcall pane ':set-owner new-frame)
    (funcall pane ':set-list-of-border-lines nil)
    (replace-element-in-list-by-new-one (funcall owner ':list-of-panes-or-frames) pane new-frame nil)
    (funcall new-frame ':update-list-of-lines pane)
    (funcall new-frame ':slice direction)))

(defmethod (frame :update-list-of-lines) (pane)
  (loop for line in list-of-border-lines
        do
        (funcall line ':update-owner pane self)))

(defun draw-box (left top right bottom)
  (funcall *graphic-window-area* ':draw-line
           left top left bottom tv:alu-xor)
  (funcall *graphic-window-area* ':draw-line
           left top right top tv:alu-xor)
  (funcall *graphic-window-area* ':draw-line
           right top right bottom tv:alu-xor)
  (funcall *graphic-window-area* ':draw-line
           left bottom right bottom tv:alu-xor))

(defmethod (line :drag) ()
  nil)

(defmethod (pane :get-value-of-slots-for-pane) (&aux name-and-option-list element)
  (funcall *graphic-window-area* ':draw-rectangle (- right left 10.) (- bottom top 10.) (+ left 5) (+ top 5) tv:alu-xor)
  (multiple-value (name-and-option-list element) (get-name-and-type left top right bottom *window-maker*))
  (multiple-value (name-of-pane type init-options) (apply #'values name-and-option-list))
  (funcall *graphic-window-area* ':draw-rectangle (- right left 10.) (- bottom top 10.) (+ left 5) (+ top 5) tv:alu-xor)
  (if (equal element 'ABORT)
      (progn
        (funcall *instrument-pane* :string-out-explicit
                      "Aborting code generation" 5 18. nil nil fonts:cptfontb tv:alu-ior)
        (*throw 'ABORT-CODE-GENERATION T))))

(defmethod (frame :get-panes-of-inferiors) (&optional (inhibit-getting-parameters t))
  (loop for item in list-of-panes-or-frames
        with list-of-panes = nil
        when (typep item 'pane)
        do
        (push item list-of-panes)
        (and inhibit-getting-parameters (funcall item ':get-value-of-slots-for-pane))
        else
        do
        (if list-of-panes (nconc list-of-panes (funcall item ':get-panes-of-inferiors))
          (setq list-of-panes (funcall item ':get-panes-of-inferiors)))
        finally (return list-of-panes)))

(defmethod (frame :get-all-inferiors-and-lines) ()
  (apply #'VALUES
         (LOOP FOR item IN list-of-panes-or-frames
               WITH list-of-lines = nil
               WITH display-list = nil
               WHEN (TYPEP item 'frame)
               DO
               (loop for line in (funcall item :list-of-border-lines)
                     when (not (member line list-of-lines))
                     DO
                     (update-list list-of-lines line))
               (multiple-value-bind (new-list-of-inferiors new-list-of-lines)
                   (funcall item :get-all-inferiors-and-lines)
                 (if list-of-lines (nconc list-of-lines new-list-of-lines)
                   (setq list-of-lines new-list-of-lines))
                 (if display-list (nconc  display-list new-list-of-inferiors)
                   (setq display-list new-list-of-inferiors)))
               ELSE
               DO
               (update-list display-list item)
               (loop for line in (funcall item :list-of-border-lines)
                     when (not (member line list-of-lines))
                     DO
                     (update-list list-of-lines line))
               FINALLY (return (list display-list list-of-lines)))))

(defmethod (frame :make-a-pane-to-use-for-code-generation) ()
  (update-list list-of-panes-or-frames (make-instance 'pane :left left :right right
                                                      :top top :bottom bottom :keyword ':even :owner self)))

(defun get-position-it-should-be-in (item new-list operation)
  (loop for element in new-list
        as coord1 = (funcall element operation)
        with coord2 = (funcall item operation)
        when (< coord2 coord1) do
        (return (find-position-in-list element new-list))))


(defun sort-panes-and-frames-in-frame (list-of-panes-or-frames direction)
  (loop for item in list-of-panes-or-frames
        with operation = (if (equal direction ':horizontal) ':top ':left)
        with new-list = nil
        with index
        do
        (if (not new-list) (setq new-list (ncons item))
          (setq index (get-position-it-should-be-in item new-list operation))
          (if (not index) (nconc new-list (ncons item))
            (if (zerop index) (push item new-list)
              (rplacd (nthcdr (1- index) new-list) (nconc (ncons item) (nthcdr index new-list))))))
        finally (return new-list)))

(defmethod (frame :area)()
  (* (- right left) (- bottom top)))

(defmethod (frame :compute-relative-size) ()
  (setq relative-size (// (small-float (* (- right left) (- bottom top)))
                          (small-float (funcall owner ':area)))))

(defmethod (pane :compute-relative-size) ()
  (setq relative-size (// (small-float (* (- right left) (- bottom top)))
                          (small-float (funcall owner ':area)))))

(defmethod (frame :get-size-keyword) ()
  (selectq keyword
    (:even '(:EVEN))
    (:percentwise (ncons (funcall self ':compute-relative-size)))))


(defmethod (pane :get-size-keyword) ()
  (selectq keyword
    (:even '(:EVEN))
    (:percentwise (ncons (funcall self ':compute-relative-size)))
    (:lines (list number ':lines))
    (:pixels (ncons number))
    (:characters (list number ':characters))))


(defmethod (frame :update-list-of-panes-or-frames) (list-of-new-panes-or-frames)
  (setq list-of-panes-or-frames (nconc list-of-panes-or-frames list-of-new-panes-or-frames)))

(defmethod (frame :delete-element-from-its-list) (element)
  (delete-element-from-list list-of-panes-or-frames element))


(defmethod (frame :do-i-have-a-pane-or-frame-with-an-even?) ()
  (loop for pane-or-frame in list-of-panes-or-frames
        WHEN (equal (funcall pane-or-frame :keyword) ':EVEN)
        do (return T)
        finally (return NIL)))

;;
;;
;;    Method which slices a given frame between the limit lower-limit and upper-limit.
;;
;;

(defmethod (frame :slice) (&optional direction lower-limit upper-limit
                           &aux point-of-slicing1 point-of-slicing2 point-of-slicing
                           pane1 pane2 pane3 line1 line2 key key1 key2 number1 number2
                           left1 left2 top1 top2 right1 right2 bottom1 bottom2 line)
  ;;
  ;;  Should prevent any kind of break from here on. First the menu should return was they suppose to return
  ;; otherwise an error condition is signaled. Second intercepted keys should be considered.
  ;;
  (and direction-of-slice (or direction (setq direction direction-of-slice)))
  (or lower-limit (setq lower-limit (if (equal direction ':horizontal) top left)))
  (or upper-limit (setq upper-limit (if (equal direction ':horizontal) bottom right)))
  (setq key (tv:menu-choose *menu-1* '(:string "Divide the pane into:" :font fonts:metsi)))
  (if (equal direction ':horizontal)
      (setq left1 left left2 left top1 lower-limit top2 lower-limit
            right1 right right2 right bottom1 upper-limit bottom2 upper-limit)
    (setq left1 lower-limit left2 lower-limit top1 top top2 top
          right1 upper-limit right2 upper-limit bottom1 bottom bottom2 bottom))
  (selectq key
    (:PERCENTWISE (setq point-of-slicing (get-point-of-slicing direction top1 left1 lower-limit upper-limit self)
                        key1 key key2 key))
    (:EVEN (setq point-of-slicing (+ lower-limit (// (- upper-limit lower-limit) 2))
                 key1 key key2 key))
    (:ABSOLUTE (multiple-value
                 (point-of-slicing1 number1 point-of-slicing2 number2
                  key1 key2 left1 left2 right1 right2 bottom1 bottom2 top1 top2 pane3)
                 (get-arguments-for-slicing-for-absolute-size direction lower-limit upper-limit self))
               (setq point-of-slicing point-of-slicing1)))
  ;;
  ;;    Now we are ready to slice if every thing is ok.
  ;;
  (if (and key key1 key2 point-of-slicing (if (member key2 '(:PIXELS :LINES :CHARACTERS)) point-of-slicing2 T))
      (progn
        ;; First see if you have to change the keywords to :PERCENTWISE in the case that the new keyword
        ;; is :EVEN and that this one already exists with the frame.
        (and (funcall-self :do-i-have-a-pane-or-frame-with-an-even?)
             (progn
               (AND (equal key ':EVEN) (setq key ':PERCENTWISE))
               (AND (equal key1 ':EVEN) (setq key1 ':PERCENTWISE))
               (AND (equal key2 ':EVEN) (setq key2 ':PERCENTWISE))))
        (if (not direction-of-slice)
            (progn
              (draw-box left top right bottom)  ; erase old drawing of frame
              (selectq direction
                (:horizontal                    ; then point-of-slicing is a y coordinate
                 (setq pane1 (make-instance
                               'pane :left left1 :right right1 :top top1
                               :bottom (1- point-of-slicing) :owner self
                               :keyword key1 :number number1)
                       pane2 (make-instance
                               'pane :left left2 :right right2 :top (1+ point-of-slicing)
                               :bottom bottom2 :owner self :keyword key2 :number number2)
                       direction-of-slice direction))
                (:vertical
                 (setq pane1 (make-instance
                               'pane :left left1 :right (1- point-of-slicing) :top top1
                               :bottom bottom1 :owner self :keyword key1 :number number1)
                       pane2 (make-instance
                               'pane :left (1+ point-of-slicing) :right right2 :top top2
                               :bottom bottom2 :owner self :keyword key2 :number number2)
                       direction-of-slice direction))))
          ;;
          ;;  Then the action will be to slice a pane into two section and consider them as new.
          ;;
          (multiple-value (pane1 pane2 line)
            (funcall self ':get-both-panes point-of-slicing key1 key2))
          (delete-element-from-list *mouse-sensitive-items-of-window*
                                    (get-element-to-delete *mouse-sensitive-items-of-window* pane1))
          (funcall pane1 ':set-number number1)
          (funcall pane2 ':set-number number2))
        ;;
        ;;  Update list of panes in frames
        ;;
        (update-list list-of-panes-or-frames pane1)
        (update-list list-of-panes-or-frames pane2)
        (and pane3 (update-list list-of-panes-or-frames pane3))
        ;;
        ;;  make an object line separating the two panes.
        ;;
        (setq line1 (make-instance 'line :direction direction
                                   :x-position (if (equal direction ':horizontal) left point-of-slicing)
                                   :y-position (if (equal direction ':horizontal) point-of-slicing top)
                                   :owner1 pane1 :owner2 pane2))
        (if pane3 (progn
                    (setq line2 (make-instance 'line :direction direction
                                               :x-position (if (equal direction ':horizontal) left point-of-slicing2)
                                               :y-position (if (equal direction ':horizontal) point-of-slicing2 top)
                                               :owner1 pane2 :owner2 pane3))
                    (and line
                         (if (eq (funcall line ':owner1) pane1)
                             (funcall line ':set-owner1 pane3)
                           (funcall line ':set-owner2 pane3))
                         (funcall pane3 ':update-list-of-border-lines line)))
          (and line (if (eq (funcall line ':owner1) pane1)
                        (funcall line ':set-owner1 pane2)
                      (funcall line ':set-owner2 pane2))
               (funcall pane2 ':update-list-of-border-lines line)))
        ;;
        ;;  Both new panes are separated by the line in the frame
        ;;
        (funcall pane1 ':update-list-of-border-lines  line1)
        (funcall pane2 ':update-list-of-border-lines  line1)
        (and line2
             (progn (funcall pane2 ':update-list-of-border-lines line2)
                    (funcall pane3 ':update-list-of-border-lines line2)
                    (funcall pane2 ':set-left left2)
                    (funcall pane2 ':set-right right2)
                    (funcall pane2 ':set-bottom bottom2)
                    (funcall pane2 ':set-top top2)))
        ;;
        ;;  Make line separating new panes mouse sensitives
        ;;
        (update-list *mouse-sensitive-items-of-window* (funcall line1 ':set-mouse-region))
        (and line2 (update-list *mouse-sensitive-items-of-window* (funcall line2 ':set-mouse-region)))
        ;;
        ;;  Make the new panes mouse sensitive
        ;;
        (update-list *mouse-sensitive-items-of-window* (funcall pane1 ':set-mouse-region))
        (update-list *mouse-sensitive-items-of-window* (funcall pane2 ':set-mouse-region))
        (and pane3 (update-list *mouse-sensitive-items-of-window* (funcall pane3 ':set-mouse-region)))
        ;;
        ;;  Now delete mouse sensitive area of frame from list
        ;;
        (delete-element-from-list *mouse-sensitive-items-of-window*
                                  (get-element-to-delete *mouse-sensitive-items-of-window* self))
        ;;
        ;;  update graphic windows now to reflect the new change
        ;;
        (let (x y z s)
          (multiple-value (x y z s) (funcall pane1 ':get-slots))
          (draw-box x y z s)
          (multiple-value (x y z s) (funcall pane2 ':get-slots))
          (draw-box x y z s)
          (and pane3 (progn (multiple-value (x y z s) (funcall pane3 ':get-slots))
                            (draw-box x y z s))))
        (funcall *graphic-window-area* ':update-typeout-list))
    (funcall self ':signal-error)))
;;
;;
;;    Method to slice a pane into two or three pieces depending on the size of the two windows constructed
;; the third will be the remaining space if any is left.
;; If the direction of the slice is the same as the one of the owner then a new pane is added to the frame
;; (possibly two new panes are added). If the direction of slice is different then a new frame is constructed
;; with the pane mouse on and then sliced.
;;
;;
(defmethod (pane :slice) (direction)
  (let ((direction-of-slicing-of-frame-owning-pane (funcall owner ':direction-of-slice)))
    (if (not (equal direction direction-of-slicing-of-frame-owning-pane))
        (create-new-frame self direction)
      (funcall owner ':slice direction
               (if (equal direction ':horizontal) top left)
               (if (equal direction ':horizontal) bottom right)))))

(defvar *added-pane* nil)

(defmethod (frame :insert-new-pane) (lower-limit upper-limit frame-to-change
                                     &aux (number-of-char-or-lines-or-pixels nil)
                                     location key point-of-slicing)
  ;; first ask for the location where the user wants the new pane to be inserted.
  ;; default will be left for vertical split, top for horizontal split.
  (setq location (or (tv:menu-choose (if (equal direction-of-slice :horizontal)
                                         '(("At the top" :eval ':top :font fonts:cptfontb
                                            :documentation "Insert new pane at the top")
                                           ("At the bottom" :eval ':bottom :font fonts:cptfontb
                                            :documentation "Insert new pane at the bottom"))
                                       '(("At the left" :eval ':left :font fonts:cptfontb
                                          :documentation "Insert new pane at the left")
                                         ("At the right" :eval ':right :font fonts:cptfontb
                                          :documentation "Insert new pane at the right")))
                                     '(:string "Where do you want to insert new pane ?" :font fonts:metsi))
                     (if (equal direction-of-slice ':horizontal) ':top ':left)))
  ;;
  ;; Now here we can ask the user about the size of the new pane.
  ;; first ask him for the way to slice. Even is excluded from here.
  ;; IF the user did not select any thing then the default is :percentwise.
  ;;
  (setq key (or (tv:menu-choose '(("Proportional area" :eval ':PERCENTWISE :font fonts:cptfontb)
                                  ("Specifying absolute size" :eval ':ABSOLUTE :font fonts:cptfontb))
                                '(:string "How do you want to Specify size of new pane?" :font fonts:metsi :centered))
                ':PERCENTWISE))
  (selectq key
    (:PERCENTWISE (setq point-of-slicing
                        (get-point-of-slicing direction-of-slice (funcall frame-to-change :top)
                                              (funcall frame-to-change :left) lower-limit upper-limit self
                                              (member location '(:bottom :right)))))
    (:ABSOLUTE
     (setq key (or (tv:menu-choose (if (equal direction-of-slice ':horizontal)
                                       '(("Specify number of pixels" :eval ':PIXELS :font fonts:cptfontb)
                                         ("specify number of lines" :eval ':LINES :font fonts:cptfontb))
                                     '(("Specify number of pixels" :eval ':PIXELS :font fonts:cptfontb)
                                       ("Specify number of characters" :eval ':CHARACTERS :font fonts:cptfontb)))
                                   '(:string "Choose method of measuring size" :font fonts:metsi))
                   ':PIXELS))
     (multiple-value (number-of-char-or-lines-or-pixels point-of-slicing)
       (get-argument-for-other-keyword key direction-of-slice frame-to-change
                                       lower-limit upper-limit (not (member location '(:bottom :right)))))))
  ;; Now here we ought to check to see if every thing is ok.
  (if (not (and point-of-slicing (if (member key '(:pixels :characters :lines)) number-of-char-or-lines-or-pixels T)))
      nil
    ;; otherwise everything seems to be ok.
    ;; Create a pane then.
    (let* ((direction (equal direction-of-slice ':horizontal))
           (top-position-of-pane (member location '(:top :left)))
           (left-pane (if direction left (if top-position-of-pane lower-limit (1+ point-of-slicing))))
           (top-pane (if direction (if top-position-of-pane lower-limit (1+ point-of-slicing)) top))
           (right-pane (if direction right (if top-position-of-pane (1- point-of-slicing) upper-limit)))
           (bottom-pane (if direction (if top-position-of-pane (1- point-of-slicing) upper-limit) bottom))

           (pane (make-instance 'pane :owner self :left left-pane :right right-pane :top top-pane :bottom bottom-pane
                                :keyword key :number number-of-char-or-lines-or-pixels))
           (line (make-instance 'line
                                :x-position (if direction left point-of-slicing)
                                :y-position (if direction point-of-slicing top)
                                :owner1 pane
                                :direction direction-of-slice))
           (value-to-pass (if top-position-of-pane
                              (list (1+ point-of-slicing)
                                    (if direction (funcall frame-to-change :bottom)
                                      (funcall frame-to-change :right)))
                            (list (if direction (funcall frame-to-change :top)
                                    (funcall frame-to-change :left))
                                  (1- point-of-slicing))))
           (*lines-bordering-frames-to-update* NIL))
      (setq *added-pane* pane)
      (funcall pane :update-list-of-border-lines line)
      ;; A new line is entered to reflect the change. The panes around it should be updated to
      ;; reflect whose line belongs to who.
      (update-owner-ship-of-lines-of-frame-to-change pane line frame-to-change location)
      (update-list list-of-panes-or-frames pane)
      (update-list *mouse-sensitive-items-of-window* (funcall pane :set-mouse-region))
      (update-list *mouse-sensitive-items-of-window* (funcall line :set-mouse-region))
      ;; Now the real work of updating the graphics and the structure to reflect the change.
      ;; The frame to change is frame to change. The proportions of the member of the frame to change
      ;; will be kept.
      (funcall frame-to-change :update-inferiors value-to-pass direction-of-slice)
      (loop for line in *lines-bordering-frames-to-update*
            do
            (funcall line :update-line-position))
      ;; now draw the new pane
      (multiple-value-bind (x y z s) (funcall pane :get-slots)
        (draw-box x y z s))
      (funcall *graphic-window-area* :update-typeout-list))))


(defmethod (frame :update-inferiors) (new-values direction)
  (let (old-left old-right old-top old-bottom factor
        (direction-flag (equal direction ':horizontal))
        (update-second-side? (equal direction direction-of-slice))
        (sorted-list-of-panes-or-frames-by-stacking
          (sort-panes-and-frames-in-frame list-of-panes-or-frames direction-of-slice)))
    ;; compute relative change in the frame.
    ;; remember your old measurements.
    (setq old-left left old-top top old-right right old-bottom bottom)
    (if direction-flag
        ;; the top frame has a direction of slice equal to horizontal
        ;; the new pane is either inserted at the top or bottom.
        ;; compute the relative change in the length.
        (setq factor (// (small-float (abs (apply #'- new-values))) (small-float (- bottom top)))
              top (first new-values)
              bottom (second new-values))
      ;; the top frame has a direction of slice equal to vertical.
      ;; the new pane is inserted at the left or right.
      ;; compute the relative changes in the length
      (setq factor (// (small-float (abs (apply #'- new-values))) (small-float (- right left)))
              left (first new-values)
              right (second new-values)))
      ;; Now to update the element of the this frame to reflect the change.
      (LOOP WITH first-time = T
            WITH new-value1 AND new-value2
            FOR pane-or-frame IN (butlast sorted-list-of-panes-or-frames-by-stacking)
            WITH last-pane-or-frame = (nth (1- (length sorted-list-of-panes-or-frames-by-stacking))
                                           sorted-list-of-panes-or-frames-by-stacking)
            WHEN (typep pane-or-frame 'frame)
            DO
            (setq new-value1 (if first-time (if direction-flag top left)
                               (if update-second-side?
                                   (if direction-flag
                                       (+ (fix (* factor (- (funcall pane-or-frame :top) old-top))) top)
                                     (+ (fix (* factor (- (funcall pane-or-frame :left) old-left))) left))
                                 (if direction-flag top left))))
            (setq new-value2 (if update-second-side?
                                 (if direction-flag (+ (fix (* factor (- (funcall pane-or-frame :bottom)) old-top)) top)
                                   (+ (fix (* factor (- (funcall pane-or-frame :right) old-left))) left))
                               (if direction-flag bottom right)))
            (funcall pane-or-frame :update-inferiors (list new-value1 new-value2) direction)
            (and first-time (setq first-time nil))
            ELSE
            DO
            ;; First update the keywords for cases where it does not make sens to
            ;; have the absolute size after this operation.
            (if direction-flag
                (and (member (funcall pane-or-frame :keyword) '(:PIXELS :LINES))
                     (funcall pane-or-frame :set-keyword ':PERCENTWISE))
              (and (member (funcall pane-or-frame :keyword) '(:PIXELS :CHARACTERS))
                   (funcall pane-or-frame :set-keyword ':PERCENTWISE)))
            ;;
            ;; erase all drawing first.
            (multiple-value-bind (x y z s) (funcall pane-or-frame :get-slots)
              (draw-box x y z s))
            ;;
            ;; Now update slot of pane-or-frame.
            (if direction-flag
                ;; update top and bottom if allowed.
                (if update-second-side?
                    ;; we have to update both bottom and top.
                    ;; all values have to be computed in this case.
                    (progn
                      (funcall pane-or-frame :set-top
                               (if first-time top
                                 (+ (fix (* factor (- (funcall pane-or-frame :top) old-top))) top)))
                      (funcall pane-or-frame :set-bottom
                               (+ (fix (* factor (- (funcall pane-or-frame :bottom) old-top))) top)))
                  ;; otherwise update only both side of the pane. and it is all the time the same value.
                  (funcall pane-or-frame :set-top top)
                  (funcall pane-or-frame :set-bottom bottom))
              ;; update left and right of pane-or-frame.
              (if update-second-side?
                  ;; we have to update both left and right.
                  ;; all values have to be computed in this case.
                  (progn
                    (funcall pane-or-frame :set-left
                             (if first-time left
                               (+ (fix (* factor (- (funcall pane-or-frame :left) old-left))) left)))
                    (funcall pane-or-frame :set-right
                             (+ (fix (* factor (- (funcall pane-or-frame :right) old-left))) left)))
                ;; otherwise update both side of the pane. and it is all the time the same value.
                (funcall pane-or-frame :set-left left)
                (funcall pane-or-frame :set-right right)))
            ;; now at this point of time the structure for the current pane is all happy again.
            ;; so draw it back on the window.
            (multiple-value-bind (x y z s) (funcall pane-or-frame :get-slots)
              (draw-box x y z s))
            (and first-time (setq first-time nil))
            FINALLY
            ;; now the last of the panes which has to take the second value to be.
            (if (typep last-pane-or-frame 'frame)
                (progn
                  (if update-second-side?
                      ;; that is if we have to compute new values for sides.
                      (setq new-value1 (if direction-flag
                                           (+ (fix (* factor (- (funcall last-pane-or-frame :top) old-top))) top)
                                         (+ (fix (* factor (- (funcall last-pane-or-frame :left) old-left))) left))
                            new-value2 (if direction-flag bottom right))
                    (setq new-value1 (if direction-flag top left)
                          new-value2 (if direction-flag bottom right)))
                  (funcall last-pane-or-frame :update-inferiors (list new-value1 new-value2) direction))
              ;; First update the keywords for cases where it does not make sens to
              ;; have the absolute size after this operation.
              (if direction-flag
                  (and (member (funcall last-pane-or-frame :keyword) '(:PIXELS :LINES))
                       (funcall last-pane-or-frame :set-keyword ':PERCENTWISE))
                (and (member (funcall last-pane-or-frame :keyword) '(:PIXELS :CHARACTERS))
                     (funcall last-pane-or-frame :set-keyword ':PERCENTWISE)))
              ;;
              ;; erase all drawing first.
              (multiple-value-bind (x y z s) (funcall last-pane-or-frame :get-slots)
                (draw-box x y z s))
              ;;
              ;; Now update slot of pane-or-frame.
              (if direction-flag
                  ;; update top and bottom if allowed.
                  (if update-second-side?
                      ;; we have to update both bottom and top.
                      ;; all values have to be computed in this case.
                      (progn
                        (funcall last-pane-or-frame :set-top
                                 (+ (fix (* factor (- (funcall last-pane-or-frame :top) old-top))) top))
                        (funcall last-pane-or-frame :set-bottom bottom))
                    ;; otherwise update both side of the pane. and it is all the time the same value.
                    (funcall last-pane-or-frame :set-top top)
                    (funcall last-pane-or-frame :set-bottom bottom))
                ;; update left and right of pane-or-frame.
                (if update-second-side?
                    ;; we have to update both left and right.
                    ;; all values have to be computed in this case.
                    (progn
                      (funcall last-pane-or-frame :set-left
                               (+ (fix (* factor (- (funcall last-pane-or-frame :left) old-left))) left))
                      (funcall last-pane-or-frame :set-right right))
                  ;; otherwise update only both side of the pane. and it is all the time the same value.
                  (funcall last-pane-or-frame :set-left left)
                  (funcall last-pane-or-frame :set-right right)))
              ;; now at this point of time the structure for the current pane is all happy again.
              ;; so draw it back on the window.
              (multiple-value-bind (x y z s) (funcall last-pane-or-frame :get-slots)
                (draw-box x y z s))))))

(defmethod (frame :after :update-inferiors) (&rest ignore)
  ;; This here will update all the mouse-sensitive-items affected by the insert operation
  ;; this will include panes and lines bordering the panes or frames.
  ;; update the lines in the frame itself.
  (fix-all-new-coordinates self list-of-panes-or-frames)
  (LOOP FOR pane-or-frame IN list-of-panes-or-frames
        WITH line-to-update = NIL
        WHEN (typep pane-or-frame 'pane)
        do
        ;; update mouse sensitive region for pane first.
        (replace-element-in-list-by-new-one *mouse-sensitive-items-of-window* pane-or-frame
                                            (cddr (funcall pane-or-frame :set-mouse-region)) 'second)
        (loop for line in (funcall pane-or-frame :list-of-border-lines)
              when (not (member line line-to-update))
              do
              (update-list line-to-update line))
        FINALLY
        (progn
          ;; update lines structures.
          (loop for line in line-to-update
                do
                  (funcall line :update-line-position))
          (loop for line in list-of-border-lines
                when (not (member line *lines-bordering-frames-to-update*))
                do
                (update-list *lines-bordering-frames-to-update* line)))))

(defmethod (frame :after :insert-new-pane) (&rest ignore)
  ;;
  ;;  Check here for consistancy of keyword in panes. We do not want to have two or more panes with :EVEN
  ;; and rest of them with other keyword.
  ;;
  (loop for pane-or-frame in list-of-panes-or-frames
        with number-of-even = 0
        with list-of-items-with-even = nil
        with number-of-other-keywords = 0
        when (equal (funcall pane-or-frame ':keyword) ':EVEN)
        do (setq number-of-even (1+ number-of-even))
        (push pane-or-frame list-of-items-with-even)
        else do (setq number-of-other-keywords (1+ number-of-other-keywords))
        finally
        (and (> number-of-even 1)
             (> number-of-other-keywords 0)
                ;;
                ;; Change all :even keywords to :percentwise except for one
                ;;
                (loop for item in (butlast list-of-items-with-even)
                      do
                      (funcall item ':set-keyword ':PERCENTWISE)))))

(defmethod (pane :insert-new-pane) ()
  ;; May be to avoid confusion where the pane is to be inserted, I should hilight the frame
  ;; that will free some of its space.
  (let ((parent-of-owner (and owner (funcall owner :owner)))
        (direction (funcall owner :direction-of-slice)))
    ;;
    ;; parent-of-owner if it exists will be the frame to add the new pane to.
    ;; if parent-of-owner does not exit then we should create a frame to hold the existing
    ;; panes and insert a pane to the parent of this frame.
    ;; a pane to the parent.
    ;;
    (if (not parent-of-owner)
        (funcall owner :slice direction (if (equal direction ':horizontal) top left)
                 (if (equal direction :horizontal) bottom right))
      ;; If the parent does exist, then insert a new pane to it, and update all the
      ;; inferior to reflect the change.
      (setq direction (funcall parent-of-owner :direction-of-slice))
      (funcall parent-of-owner :insert-new-pane (if (equal direction ':horizontal) top left)
               (if (equal direction ':horizontal) bottom right) owner))))


(defmethod (line :update-line-position) (&aux l1 t1 r1 b1 l2 r2 t2 b2)
  (multiple-value (l1 t1 r1 b1) (funcall owner1 :get-slots))
  (multiple-value (l2 t2 r2 b2) (funcall owner2 :get-slots))
  (setq x-position (if (equal direction ':horizontal) l1 (1+ (min r1 r2)))
        y-position (if (equal direction ':horizontal) (1+ (min b1 b2)) t1))
  (replace-element-in-list-by-new-one *mouse-sensitive-items-of-window* self
                                                    (cddr (funcall-self :set-mouse-region)) 'second))

(defmethod (frame :signal-error) ()
  (if (= (length list-of-panes-or-frames) 1)
      ;; that means that we tried to slice a pane which has been transformed into a frame.
      (funcall (first list-of-panes-or-frames) ':kill)))
;;
;;
;;  Method to compute the relative size of the new window in the frame
;;
(defmethod (frame :compute-percent) (lower-limit cursor-position direction)
  (fix (* (// (small-float (if (equal direction ':horizontal)
                               (* (- right left) (- cursor-position lower-limit))
                             (* (- cursor-position lower-limit) (- bottom top))))
              (small-float (* (- right left) (- bottom top)))) 100.)))

(defun get-windows-bordered-by-lines (lines pane-or-frame direction)
   (loop for line in lines
         as window = (funcall line ':get-other-window pane-or-frame)
         as left-or-top = (funcall window (if (equal direction ':horizontal) ':top ':left))
         with old-left-or-top = nil
         with list-to-return = nil
         do
         (if old-left-or-top
             (if (< old-left-or-top left-or-top)
                 (update-list list-to-return window)
               (setq old-left-or-top left-or-top)
               (push window list-to-return))
           (setq old-left-or-top left-or-top
                 list-to-return (ncons window)))
         finally (return list-to-return)))

;;
;;  Method which is called if a pane has been killed in a frame
;;
(defmethod (frame :fix-yourself) (pane-or-frame-killed &aux l1 t1 r1 b1 key1 numb lis-of-bord own)
  (multiple-value (l1 t1 r1 b1 own key1 numb lis-of-bord) (funcall pane-or-frame-killed ':get-slots))
  (if (typep pane-or-frame-killed 'frame)
      (let ((new-pane (make-instance 'pane :left l1 :top t1 :right r1 :bottom b1
                                     :keyword key1 :number numb :list-of-border-lines lis-of-bord :owner self)))
        (replace-element-in-list-by-new-one list-of-panes-or-frames pane-or-frame-killed new-pane nil)
        (update-list *mouse-sensitive-items-of-window* (funcall new-pane ':set-mouse-region))
        ;; update owner of lines passed to it
        (loop for line in (funcall new-pane ':list-of-border-lines)
              do
              (funcall line ':replace-element pane-or-frame-killed new-pane))
        (draw-box l1 t1 r1 b1)
        (funcall *graphic-window-area* ':update-typeout-list))
    (delete-element-from-list list-of-panes-or-frames pane-or-frame-killed)
    (let ((number-of-elements (length list-of-panes-or-frames))
          (lines-bordering-it (funcall pane-or-frame-killed ':list-of-border-lines))
          window-to-expand)
      (cond ((zerop number-of-elements) (if owner (funcall owner ':fix-yourself self)
                                          (setq direction-of-slice nil)))
            ((= number-of-elements 1)
             ;; if the remaining thing is a frame then put every body in the list of its owner
             (if (typep (first list-of-panes-or-frames) 'pane)
                 ;; then kill the frame
                 (if owner (funcall self ':kill)
                   (funcall (first list-of-panes-or-frames) ':expand pane-or-frame-killed))
               ;; otherwise the element is a frame that should not exits any more (no use for it).
               ;; so replace it by its list of panes or frames. Nothing to update after this
               (let ((frame-to-add (first list-of-panes-or-frames)))
                 (setq list-of-panes-or-frames (funcall frame-to-add ':list-of-panes-or-frames))
                 ;; every body in this list should have its parent updated.
                 (loop for pane-or-frame in list-of-panes-or-frames
                       do
                       (funcall pane-or-frame ':set-owner self))
                 (funcall frame-to-add ':expand pane-or-frame-killed)
                 ;; now update the direction of the current frame.
                 (setq direction-of-slice (funcall frame-to-add ':direction-of-slice)))))
            ;;
            ;;  Here it means that we have other things in the frame
            ;; and we can expand one of the existing pane to occupy the rest of the free space
            ;;
            (t (if (= (length lines-bordering-it) 1)
                   ;;
                   ;; Only one window was adjacent to the killed pane
                   ;;
                   (setq window-to-expand (funcall (first lines-bordering-it) ':get-other-window pane-or-frame-killed))
                 (setq window-to-expand (funcall
                                          (or (tv:menu-choose
                                                (if (equal direction-of-slice ':horizontal)
                                                    '(("top window" :eval 'first :font fonts:cptfontb
                                                       :documentation "Expand top pane or frame")
                                                      ("bottom window" :eval 'second :font fonts:cptfontb
                                                       :documentation "Expand bottom pane or frame"))
                                                  '(("left window" :eval 'first :font fonts:cptfontb
                                                     :documentation "Expand left pane or frame")
                                                    ("right window" :eval 'second :font fonts:cptfontb
                                                     :documentation "Expand right pane or frame")))
                                                '(:string "which window to expand" :font fonts:cptfontb))
                                              'first)
                                          (get-windows-bordered-by-lines
                                            lines-bordering-it
                                            pane-or-frame-killed direction-of-slice))))
               ;;
               ;;  We have the window to expand. It could be a pane or a frame. Let us get on with it.
               ;;
               (funcall window-to-expand ':expand pane-or-frame-killed))))))
;;
;;
;;     Method to expand a pane to occupy the free region.
;;
(defmethod (pane :expand) (pane-or-frame-killed &aux direction l1 t1 r1 b1)
  (setq direction (funcall owner ':direction-of-slice))
  (multiple-value (l1 t1 r1 b1) (funcall pane-or-frame-killed ':get-slots))
  (draw-box left top right bottom)
  (if (equal direction ':horizontal)
      (if (> top t1) (setq top t1)
        (setq bottom b1))
    (if (> left l1) (setq left l1)
      (setq right r1)))
  (draw-box left top right bottom)
  (or (equal keyword ':EVEN) (setq keyword ':PERCENTWISE)))
;;
;;
(defmethod (pane :after :expand) (pane-or-frame-killed)
  ;;
  ;;  This will be only to update the mouse sensitive items of the window
  ;; The general case will be if the expanded pane has two borders itself
  ;; that is one of its sides is not part of the frame border.
  ;;
  (let* ((lines-in-killed-pane-or-frame (funcall pane-or-frame-killed ':list-of-border-lines))
         (common-line (loop for line in list-of-border-lines
                           when (member line lines-in-killed-pane-or-frame) (return line)))
         line)
    (delete-element-from-list *mouse-sensitive-items-of-window*
                              (get-element-to-delete *mouse-sensitive-items-of-window* common-line))
    (delete-element-from-list lines-in-killed-pane-or-frame common-line)
    (delete-element-from-list list-of-border-lines common-line)
    ;;
    ;;  If there is a remaining line in lines-in-killed-pane-or-frame, it would be the border
    ;; between pane and other part of the world.
    ;;
    (if (not lines-in-killed-pane-or-frame)
        ;;
        ;;  There is no lines left in the killed pane which was one with a side as a part of the frame border.
        ;; Then do nothing.
        ;;
        NIL
      ;;
      ;; There is one line left there and now it should be a border to the expanded pane.
      ;;
      (update-list list-of-border-lines (setq line (first lines-in-killed-pane-or-frame)))
      (funcall line ':replace-element pane-or-frame-killed self))
    (replace-element-in-list-by-new-one
      *mouse-sensitive-items-of-window* self (cddr (funcall self ':set-mouse-region)) 'second)
    (funcall *graphic-window-area* ':update-typeout-list)))

;;
;;
;;      Method to expand a frame to occupy the free region.
;;
(defmethod (frame :expand) (pane-or-frame-killed &aux l1 t1 r1 b1 operation
                            new-value operation-for-line killed-owner-direction)
  (multiple-value (l1 t1 r1 b1) (funcall pane-or-frame-killed ':get-slots))
  (setq killed-owner-direction (funcall owner ':direction-of-slice))
  ;;
  ;;  Update the size of the frame to occupy all the of space.
  ;;
  (if (equal direction-of-slice ':horizontal)
      (if (> left l1) (setq left l1 operation ':set-left new-value l1 operation-for-line ':set-x-position)
      (setq right r1 operation ':set-right new-value r1 operation-for-line nil))
    (if (> top t1) (setq top t1 operation ':set-top new-value t1 operation-for-line ':set-y-position)
      (setq bottom b1 operation ':set-bottom new-value b1 operation-for-line nil)))
  ;;
  ;;  Update all the components of the frame.
  ;;
  (loop for pane-or-frame in list-of-panes-or-frames
        when (typep pane-or-frame 'pane)
        do
        (multiple-value-bind (x y z s) (funcall pane-or-frame ':get-slots)
          (draw-box x y z s))
        (funcall pane-or-frame operation new-value)
        (multiple-value-bind (x y z s) (funcall pane-or-frame ':get-slots)
          (draw-box x y z s))
        ;; update mouse sensitivity to include the new area if any.
        (replace-element-in-list-by-new-one *mouse-sensitive-items-of-window* pane-or-frame
                                            (cddr (funcall pane-or-frame ':set-mouse-region)) 'second)
        ;; update the length of the mouse sensitivity of the line to reflect the new area.
        (loop for line in (funcall pane-or-frame ':list-of-border-lines)
              do
              (and operation-for-line (funcall line operation-for-line new-value))
              (replace-element-in-list-by-new-one *mouse-sensitive-items-of-window* line
                                                  (cddr (funcall line ':set-mouse-region)) 'second))
        (or (equal (funcall pane-or-frame ':keyword) ':EVEN) (funcall pane-or-frame ':set-keyword ':PERCENTWISE))
        else
        do
        (funcall pane-or-frame operation new-value)
        ;; update mouse sensitivity of lines bordering the frame.
        (loop for line in (funcall pane-or-frame ':list-of-border-lines)
              do
              (and operation-for-line (funcall line operation-for-line new-value))
              (replace-element-in-list-by-new-one *mouse-sensitive-items-of-window* line
                                                  (cddr (funcall line ':set-mouse-region)) 'second))
        (funcall pane-or-frame ':update-element nil killed-owner-direction operation new-value)))
;;
;;
(defmethod (frame :after :expand) (pane-or-frame-killed)
  (let* ((lines-in-killed-pane-or-frame (funcall pane-or-frame-killed ':list-of-border-lines))
         (common-line (loop for line in list-of-border-lines
                           when (member line lines-in-killed-pane-or-frame) (return line)))
         line)
    (delete-element-from-list *mouse-sensitive-items-of-window*
                              (get-element-to-delete *mouse-sensitive-items-of-window* common-line))
    (delete-element-from-list lines-in-killed-pane-or-frame common-line)
    (delete-element-from-list list-of-border-lines common-line)
    ;;
    ;;  If there is a remaining line in lines-in-killed-pane-or-frame, it would be the border
    ;; between pane and other part of the world.
    ;;
    (if (not lines-in-killed-pane-or-frame)
        ;;
        ;;  There is no lines left in the killed pane which was one with a side as a part of the frame border.
        ;; Then do nothing.
        ;;
        NIL
      ;;
      ;; There is one line left there and now it should be a border to the expanded pane.
      ;;
      (update-list list-of-border-lines (setq line (first lines-in-killed-pane-or-frame)))
      (funcall line ':replace-element pane-or-frame-killed self))
    (funcall *graphic-window-area* ':update-typeout-list)))
;;
;;
;;
(defmethod (frame :update-element) (&optional (top-level t) direction-of-slice-of-owner operation new-value)
  ;;
  ;; only one dimension has changed at this point of time. so reflect all the element to include the change.
  ;;
  (if top-level
      (loop for pane-or-frame in list-of-panes-or-frames
            with operation-for-line
            when (typep pane-or-frame 'pane)
            do
            ;; erase old box
            (multiple-value-bind (x y z s) (funcall pane-or-frame ':get-slots)
              (draw-box x y z s))
            ;; update size of box
            (funcall pane-or-frame operation new-value)
            (setq operation-for-line
                  (selectq operation
                    (:set-top ':set-y-position)
                    (:set-left ':set-x-position)
                    (otherwise nil)))
            ;; draw new box
            (multiple-value-bind (x y z s) (funcall pane-or-frame ':get-slots)
              (draw-box x y z s))
            ;; update mouse sensitivity to include the new area if any.
            (replace-element-in-list-by-new-one *mouse-sensitive-items-of-window* pane-or-frame
                                                (cddr (funcall pane-or-frame ':set-mouse-region)) 'second)
            ;; update the length of the mouse sensitivity of the line to reflect the new area.
            (loop for line in (funcall pane-or-frame ':list-of-border-lines)
                  do
                  (and operation-for-line (funcall line operation-for-line new-value))
                  (replace-element-in-list-by-new-one *mouse-sensitive-items-of-window* line
                                                      (cddr (funcall line ':set-mouse-region)) 'second))
            (or (equal (funcall pane-or-frame ':keyword) ':EVEN) (funcall pane-or-frame ':set-keyword ':PERCENTWISE))


            else
            do
            ;; update size of frame
            (funcall pane-or-frame operation new-value)
            (setq operation-for-line
                  (selectq operation
                    (:set-top ':set-y-position)
                    (:set-left ':set-x-position)
                    (otherwise nil)))
            ;; update mouse sensitivity of border lines
            (loop for line in (funcall pane-or-frame ':list-of-border-lines)
                  do
                  (and operation-for-line (funcall line operation-for-line new-value))
                  (replace-element-in-list-by-new-one *mouse-sensitive-items-of-window* line
                                                      (cddr (funcall line ':set-mouse-region)) 'second))
            (funcall pane-or-frame ':update-element nil direction-of-slice-of-owner operation new-value))
        ;;  We are at another level of the hierarchy. Only the panes which see their border changing is updated
    (let ((sorted-list-of-panes-or-frames (sort-panes-and-frames-in-frame list-of-panes-or-frames direction-of-slice))
          candidate)
      (if (equal direction-of-slice direction-of-slice-of-owner)
          ;; only one candidate is to be updated to occupy the space
          ;; and the candidate is either the first or last of the list.
          (let ((candidate1 (first sorted-list-of-panes-or-frames))
                (candidate2 (first (last sorted-list-of-panes-or-frames))))
            (if (equal direction-of-slice ':horizontal)
                ;; only top window or bottom window is concerned here.
                (if (> (funcall candidate1 ':top) top)
                    (multiple-value-bind (x y z s) (funcall candidate1 ':get-slots)
                      (if (typep candidate1 'frame) nil (draw-box x y z s) (draw-box x top z s))
                      (funcall (setq candidate candidate1) ':set-top top))
                  (and (typep candidate2 'pane)
                       (multiple-value-bind (x y z s) (funcall candidate2 ':get-slots)
                         (draw-box x y z s) (draw-box x y z bottom)))
                  (funcall (setq candidate candidate2) ':set-bottom bottom))
              (if (> (funcall candidate1 ':left) left)
                  (multiple-value-bind (x y z s) (funcall candidate1 ':get-slots)
                      (if (typep candidate1 'frame) nil (draw-box x y z s) (draw-box left y z s))
                      (funcall (setq candidate candidate1) ':set-left left))
                (and (typep candidate2 'pane)
                     (multiple-value-bind (x y z s) (funcall candidate2 ':get-slots)
                         (draw-box x y z s) (draw-box x y right s)))
                (funcall (setq candidate candidate2) ':set-right right)))
            (if (typep candidate 'frame)
                (funcall candidate ':update-element nil direction-of-slice-of-owner operation new-value)
              (replace-element-in-list-by-new-one *mouse-sensitive-items-of-window* candidate
                                                (cddr (funcall candidate ':set-mouse-region)) 'second)))
        ;;  Then here we should update all the panes and frames of this frame.
        (funcall self ':update-element T direction-of-slice-of-owner operation new-value)))))

;;
;;
;;  Method to kill frame
;;
;;
(defmethod (frame :kill) (&aux copy-of-list-of-panes-or-frames)
  (loop for pane-or-frame in (setq copy-of-list-of-panes-or-frames (copy-list list-of-panes-or-frames))
        when (typep pane-or-frame 'pane)
        do
        (delete-element-from-list *mouse-sensitive-items-of-window*
                                  (get-element-to-delete *mouse-sensitive-items-of-window* pane-or-frame))
        (multiple-value-bind (x y z s) (funcall pane-or-frame ':get-slots)
          (draw-box x y z s))
        (delete-element-from-list list-of-panes-or-frames pane-or-frame)
        else do
        (funcall pane-or-frame ':kill)
        finally
        (progn (if owner
                   (funcall owner ':fix-yourself self)
                 ;; it is the top level frame
                 (setq direction-of-slice nil)
                 ;; If the box for the frame was not there in the first place then draw it.
                 (and copy-of-list-of-panes-or-frames (draw-box left top right bottom))))))

;;
;;  method to kill a pane
;;
(defmethod (pane :kill) ()
  (delete-element-from-list *mouse-sensitive-items-of-window*
                            (get-element-to-delete *mouse-sensitive-items-of-window* self))
  (draw-box left top right bottom)
  (funcall owner ':fix-yourself self))


(compile-flavor-methods frame)

(compile-flavor-methods pane)

(compile-flavor-methods line)
