;;;-*-Mode:Lisp;Base:8;Package:window-maker-*-
;;; Copyright C LISP MACHINE INC., 1985.
;;



(defun get-constraint-specification-for-pane-or-frame (name constraint-descriptions)
  "searches all the subgroups untill it finds the constraints for the pane or frame whose
name is name."
  (LOOP FOR subgroup IN constraint-descriptions
        WITH result
        WHEN (setq result (assoc name subgroup))
        DO (return result)))

;;;; for :ask or :ask-window
;;;; we should may be just go and do a search for the
;;;; instance of the frame and get the size and return as number of lines or characters or pixels.
;;;; this is a quick and dirty hack of the system. May be what I should do is add a property to
;;;; the pane name if we get this. and then at the time of code generation we should force the code
;;;; generator to replace whatever it has figured out for it by the same thing as was before.

(defun get-parameter-for-ask-or-ask-window (name constraint)
  ;; *Frame-to-edit* must still have the pointer to the instance of the flavor being edited.
  ;; we should make sure that we have the configuration selected as the current one, and ask
  ;; the pane its actual size.
  (putprop name constraint 'old-constraint)
  (funcall *Frame-to-edit* :set-configuration *configuration-name*)
  (let ((window (funcall *frame-to-edit* :get-pane name)))
    (list (funcall window :height) (funcall window :width))))


(defun parse-constraint (name constraint available-space)
  ;; return the size of one of the sides
  (let (keyword units window size (number-of-char-or-lines-or-pixels nil))
    (multiple-value (keyword units window) (apply #'values constraint))
    (cond ((and (fixnump keyword) units)
           (setq number-of-char-or-lines-or-pixels keyword
                 size (* keyword (funcall *graphic-window-area* (if (equal units ':LINES) ':LINE-HEIGHT ':CHAR-WIDTH)))
                 keyword units))
          ((fixnump keyword)
           (setq number-of-char-or-lines-or-pixels size
                 size keyword
                 keyword ':PIXELS))
          ((or (flonump keyword)
               (small-floatp keyword))
           (setq size (* available-space keyword)
                 keyword ':PERCENTWISE))
          ;; otherwise it is :EVEN.
          ((equal ':EVEN keyword)  (setq size nil))
          ((equal ':ASK keyword)  (setq size (get-parameter-for-ask-or-ask-window name constraint)))
          ((equal ':ASK-WINDOW keyword) (setq size (get-parameter-for-ask-or-ask-window name constraint))))
    (values keyword size number-of-char-or-lines-or-pixels)))

(defun parse-and-compute-coordinates-of-left-right-top-bottom (name side1 side2 constraints area)
  ;; the goal here is to parse the constraints list and to compute the coordinate of the new pane
  ;; or frame which allows the drawing of the representation of the panes.
  ;; constraints is a list.
  ;; If length of list is 1 then it is either a fixnum or :EVEN or a float. If it is a fixnum
  ;; the parser assumes it is the size of a pane in pixels, if it is :EVEN then the parser assumes
  ;; it is doing the last subgroup of some level of the hierarchy. If the length of the list is
  ;; greater than one then, the object that we are trying to parse can either specify the size
  ;; of a frame or of a pane.
  (let (keyword
        (size NIL)
        (number1 NIL)
        (limit-flag NIL))
    (cond ((member ':LIMIT constraints)
           (setq limit-flag T)
           (multiple-value (keyword size number1) (parse-constraint name (nthcdr 2 constraints) area)))
          (t (multiple-value (keyword size number1) (parse-constraint name constraints area))))
    ;; Hopefully we have the size in size and the keyword in keyword.
    (if (not (equal keyword ':PERCENTWISE)) NIL
      (setq size (fix (// size (- side2 side1))))
      (and limit-flag
           (let ((limit (second constraints))
                 width-or-height number-of-chars-or-lines)
             (and (third limit)
                  (setq width-or-height
                        (funcall *graphic-window-area* (if (equal (third limit) ':LINES) ':LINE-HEIGHT ':CHAR-WIDTH))))
             (selectq (length limit)
               (2 (multiple-value-bind (lower upper) (apply #'values limit)
                    (and lower (< size lower) (setq size lower))
                    (and upper (> size upper) (setq size upper))))
               (otherwise (multiple-value-bind (lower upper) (apply #'values limit)
                            (setq number-of-chars-or-lines (fix (// size width-or-height)))
                            (and lower (< number-of-chars-or-lines lower)
                                 (setq size (* lower width-or-height)))
                            (and upper (> number-of-chars-or-lines upper)
                                 (setq size (* upper width-or-height)))))))))
    (values keyword size number1)))



(defun parse-constraints (configuration-description &optional (frame-parent *frame*) &aux list-of-sized-panes-and-frames)
  ;;  The problem is to regenerate the data structure used by WindowMaker. We are presented
  ;; with a list of two elements of which the first one is a list of ordered pane or frame
  ;; names and a constraint description for these.
  ;; The list of names is ordered by stacking, that means that we either start at the top or
  ;; at the left depending on the stacking. The list of constraints description is ordered by
  ;; keyword that is that the last group is or could be :EVEN.
  ;; The algorithm is then to compute the size of each pane or frame and at the end allocate
  ;; the structure representing each by stacking.
  (setq list-of-sized-panes-and-frames
        (LOOP WITH waiting-list = NIL
              WITH frame-parent-surface = (funcall frame-parent :area)
              WITH list-of-ordered-names = (first configuration-description)
              WITH constraint-description = (cdr configuration-description)
              WITH current-left = (funcall frame-parent :left)
              with current-top = (funcall frame-parent :top)
              WITH current-right = (funcall frame-parent :right)
              WITH current-bottom = (funcall frame-parent :bottom)
              WITH parent-direction-of-slice = (funcall frame-parent :direction-of-slice)
              WITH direction = (equal ':HORIZONTAL parent-direction-of-slice)
              WITH list-of-names-done = NIL
              WITH size AND number1 AND keyword AND constraints AND direction-of-slice
              WITH side1 = (if direction current-left current-top)
              with side2 = (if direction current-right current-bottom)
              FOR pane-or-frame-name IN list-of-ordered-names
              AS constraint-specification = (get-constraint-specification-for-pane-or-frame
                                              pane-or-frame-name constraint-description)
              WHEN (or (member ':HORIZONTAL constraint-specification)
                       (member ':VERTICAL constraint-specification)
                       (member ':BLANK constraint-specification))
              DO
              ;; it is a frame. it should be build if possible and then expanded. otherwise it is put on a waiting list.
              (multiple-value (direction-of-slice constraints) (apply #'values (cdr constraint-specification)))
              (setq direction-of-slice
                    (selectq direction-of-slice
                      (:VERTICAL ':HORIZONTAL)
                      (:HORIZONTAL ':VERTICAL)
                      (:BLANK nil)))
              (multiple-value (keyword size number1)
                (parse-and-compute-coordinates-of-left-right-top-bottom pane-or-frame-name
                  side1 side2 constraints frame-parent-surface))
              (if (member keyword '(:ASK :ASK-WINDOW))
                  ;; size is a list of the width and the height transform this to
                  ;; number of lines or characters according to the direction-of-slice of the frame-parent
                  (if direction (setq size (fix (first size)) keyword ':PERCENTIWISE)
                    (setq size (fix (second size)) keyword ':PERCENTWISE)))
              (if (equal :EVEN keyword)
                  (update-list waiting-list (list pane-or-frame-name (if direction-of-slice 'frame 'pane) direction-of-slice))
                (update-list list-of-names-done (list pane-or-frame-name (if direction-of-slice 'frame 'pane)
                                                      direction-of-slice keyword size number1)))
              ELSE
              DO
              (multiple-value (keyword size number1)
                (parse-and-compute-coordinates-of-left-right-top-bottom pane-or-frame-name
                  side1 side2 (cdr constraint-specification) frame-parent-surface))
              (if (member keyword '(:ASK :ASK-WINDOW))
                  ;; size is a list of the width and the height transform this to
                  ;; number of lines or characters according to the direction-of-slice of the frame-parent
                  (if direction (setq number1 (fix (// (first size) (funcall *graphic-window-area* :LINE-HEIGHT)))
                                      size (first size)
                                      keyword ':CHARACTERS)
                    (setq number1 (fix (// (second size) (funcall *graphic-window-area* :CHAR-WIDTH)))
                          size (second size)
                          keyword ':LINES)))
              (if (equal keyword ':EVEN)
                  (update-list waiting-list (list pane-or-frame-name 'pane NIL))
                (update-list list-of-names-done (list pane-or-frame-name 'pane NIL keyword size number1)))
              FINALLY
              (if (not waiting-list) (return list-of-names-done)
                ;; we have :EVEN waiting to be assigned a size.
                ;; first compute the remaining space taken by all the panes and frames of known sizes
                ;; the value of size is expected to be in pixels. Since all the component are in the
                ;; same frame, size is the measure of the same side, thus can be added up to see how
                ;; much space we have taken. The difference between the length of the appropriate side
                ;; of the frame parent and of the sum of all the known sizes of the known is the length
                ;; to be shared by the names in the waiting-list.
                (return (LOOP FOR (name type direction-of-slice keyword size number) IN list-of-names-done
                              WITH total-length-taken = 0
                              do
                              (setq total-length-taken (+ total-length-taken size))
                              FINALLY
                              (return (LOOP FOR item IN waiting-list
                                            WITH size-of-object = (fix (// (- (if direction (- current-bottom current-top)
                                                                                (- current-right current-left))
                                                                              total-length-taken)
                                                                           (length waiting-list)))
                                            do
                                            (nconc item (list ':EVEN size-of-object NIL))
                                            FINALLY (return (append list-of-names-done waiting-list)))))))))
  ;; now to generate the structure and expand the frames.
  (LOOP WITH expand-list = NIL
        WITH list-of-panes-or-frames = NIL
        WITH total-number-of-names = (length (first configuration-description))
        WITH left = (funcall frame-parent :left)
        with top = (funcall frame-parent :top)
        WITH right = (funcall frame-parent :right)
        WITH bottom = (funcall frame-parent :bottom)
        WITH name1 AND size AND keyword AND number1 AND direction-of-slice AND type
        WITH parent-direction-of-slice = (funcall frame-parent :direction-of-slice)
        WITH direction = (equal ':HORIZONTAL parent-direction-of-slice)
        WITH new-line AND old-line AND new-pane-or-frame
        FOR name in (first configuration-description)
        FOR index from 1 to total-number-of-names
        AS item = (assoc name list-of-sized-panes-and-frames)
        AS last-name-p = (equal index total-number-of-names)
        DO
        (multiple-value (name1 type direction-of-slice keyword size number1) (apply #'values item))
        (if direction (setq bottom (+ top size)) (setq right (+ left size)))
        (or last-name-p (setq new-line (make-instance 'line :x-position (if direction left (1+ right))
                                                      :y-position (if direction (1+ bottom) top)
                                                      :direction parent-direction-of-slice)))
        (update-list list-of-panes-or-frames
                     (setq new-pane-or-frame
                           (selectq type
                             (frame (let ((frame (make-instance 'frame
                                                                :owner frame-parent
                                                                :left left
                                                                :right (if last-name-p (funcall frame-parent :right) right)
                                                                :top top
                                                                :bottom (if last-name-p (funcall frame-parent :bottom) bottom)
                                                                :direction-of-slice direction-of-slice
                                                                :keyword keyword
                                                                :number NIL
                                                                :list-of-border-lines (if (and old-line new-line)
                                                                                          (list old-line new-line)
                                                                                        (if new-line (ncons new-line)
                                                                                          (ncons old-line))))))
                                      (update-list expand-list (list name1 frame))
                                      frame))
                             (pane (make-instance 'pane
                                                  :owner frame-parent
                                                  :left left
                                                  :right (if last-name-p (funcall frame-parent :right) right)
                                                  :top top
                                                  :bottom (if last-name-p (funcall frame-parent :bottom) bottom)
                                                  :keyword keyword
                                                  :number number1
                                                  :list-of-border-lines (if (and old-line new-line)
                                                                             (list old-line new-line)
                                                                           (if new-line (ncons new-line)
                                                                             (ncons old-line))))))))
        ;; update owner1 of new-line if any and owner2 of old-line if any
        (and old-line (funcall old-line :set-owner2 new-pane-or-frame))
        (and new-line (funcall new-line :set-owner1 new-pane-or-frame))
        (setq old-line new-line)
        ;; update now current-left or current-top accordingly.
        (if direction (setq top (+ 2 bottom))
          (setq left (+ 2 right)))
        FINALLY
        (progn
          (funcall frame-parent :set-list-of-panes-or-frames list-of-panes-or-frames)
          ;; now every body is updated in this frame expand any other frames to expand.
          (LOOP FOR (name frame) IN expand-list
                WITH constraint-description = (cdr configuration-description)
                AS constraint-specification = (cdddr (get-constraint-specification-for-pane-or-frame
                                                       name constraint-description))
                DO
                (parse-constraints constraint-specification frame)))))

(defvar frame1 nil)

(defun parse-configuration (configuration)
  (setq frame1 *frame*)
  "the goal is to regenerate the data structure as known by the display functions
of window maker from the constraints specifications. configuration is a list
of a configuration name and a configuration description. we should check to see
if we have more than one pane in the first list of panes."
  (let ((configuration-name (first configuration))
        (configuration-description (cdr configuration)))
    configuration-name
    ;; may add thing later for if we want to replace the present configuration by
    ;; a new one or start a new flavor.
    (funcall *frame* :set-direction-of-slice ':HORIZONTAL)
    (if (> (length (first configuration-description)) 1)
        (parse-constraints configuration-description)
      (funcall *frame* :set-direction-of-slice ':VERTICAL)
      (parse-constraints (cdddr (car (second configuration-description)))))))

(defun get-name-of-configuration-to-edit (window)
  (setq *configuration-name*
        (or (tv:menu-choose
              (loop for configuration in *constraints-to-choose-from*
                    collect (first configuration))) *configuration-name*)
        *configuration-to-edit* (assoc *configuration-name* *constraints-to-choose-from*))
  (funcall window :output-string-to-window (format nil "~S" *configuration-name*)))

(defun get-input-type (object window)
  (let ((line (find-line '*what-to-edit*))
        (line-no (funcall window :number-of-item '*what-to-edit*))
        list-of-frames)
    (if (string-equal object "Instance of flavor")
        (loop do
              ;; loop until the user input the name of an instance of a frame.
              (condition-case (error)
                  (progn
                   (funcall window :read-from-the-window '*what-to-edit* line-no)
                   (funcall (eval *what-to-edit*) :constraints)
                   (return T))
                (error ())))
      (if (equal (or (tv:menu-choose '(("From menu" :eval 'menu :font fonts:tr12b)
                                       ("From keyboard" :eval 'keyboard :font fonts:tr12b))
                                     '(:string "From where?" :font fonts:cptfontb :centered)) 'keyboard)
                 'menu)
          ;; get flavor from menu provided by searching through all previously seclected windows in the
          ;; system.
          (setq *what-to-edit*
                (or (tv:menu-choose (setq list-of-frames
                                          (loop for i from 0 to (1- (array-length tv:previously-selected-windows))
                                                as window-or-frame = (aref tv:previously-selected-windows i)
                                                with list-of-frames = nil
                                                with flavor-type
                                                do
                                                (condition-case (error)
                                                    (progn
                                                     (funcall window-or-frame :constraints)
                                                     (or (member (setq flavor-type (typep window-or-frame)) list-of-frames)
                                                         (update-list list-of-frames flavor-type)))
                                                  (error ()))
                                                finally (return list-of-frames)))
                                '(:string "Known frames in the system" :font fonts:cptfontb :centered))
                    (first list-of-frames)))
        ;; the user is inputting the flavor name from the keyboard. So make sure we have a frame, otherwise
        ;; sit there untill it gets it.
        (loop do
              ;; loop until the user input the name of a flavor definition of a frame.
              (condition-case (error)
                  (let (instance-of-frame)
                   (funcall window :read-from-the-window '*what-to-edit* line-no)
                   (setq instance-of-frame (make-instance *what-to-edit*))
                   (funcall instance-of-frame :constraints)
                   (return T))
                (error ())))))
    ;; now *what-to-edit* has either a flavor frame definition or an instance of a flavor.
    (if (string-equal object "Instance of flavor")
        (setq *frame-to-edit* *what-to-edit*
              *what-to-edit* (typep *what-to-edit*))
      (setq *frame-to-edit*
            (loop for i from 0 to (1- (array-length tv:previously-selected-windows))
                  as frame = (aref tv:previously-selected-windows i)
                  when (typep frame *what-to-edit*)
                  do (return frame)
                  finally (make-instance *what-to-edit*))))
    (setf (selected-element line) object)
    (funcall window :delete-item line-no)
    (funcall window :insert-item line-no '*what-to-edit*)
    (funcall window :output-string-to-window (format nil "~S" *what-to-edit*))
    ;; update the configuration and configuration name.
    ;; each time that we get here.
    (setq *constraints-to-choose-from* (funcall *frame-to-edit* :constraints)
          *configuration-to-edit* (first *constraints-to-choose-from*)
          *configuration-name* (first *configuration-to-edit*))
    ;; update mouse sensitivity of line configuration name.
    (if (> (length *constraints-to-choose-from*) 1)
        (or (member ':configuration-name (funcall window :sensitive-item-types))
            (nconc  (funcall window :sensitive-item-types) (ncons ':configuration-name)))
      (delq ':configuration-name (funcall window :sensitive-item-types)))
    ;; update two lines of the window.
    (setq line-no (funcall window :number-of-item '*configuration-name*))
    (funcall window :delete-item line-no)
    (funcall window :insert-item line-no '*configuration-name*)))

(defun get-what-to-save-for-frame (object window)
  (let ((line (find-line '*what-to-save*))
        (line-no (funcall window :number-of-item '*what-to-save*)))
    (setf (selected-element line) object)
    (funcall window :delete-item line-no)
    (funcall window :insert-item line-no '*what-to-save*)
    (setq *what-to-save*
          (cond ((string-equal "none" object) nil)
                ((string-equal "names" object) 'names)
                ((string-equal "everything" object) 'all)
                ((string-equal "all but edited configuration" object) 'but)))))

(defun get-save-things ()
  (setq *list-of-existing-and-used-names* nil
        *list-of-existing-and-not-yet-used-names* nil
         *known-configuration-names* nil)
  (loop for pane in (funcall *frame-to-edit* :panes)
        as name = (first pane)
        as flavor = (second pane)
        as option-list = (cddr pane)
        do
        (putprop name flavor 'flavor)
        (putprop name option-list 'option-list)
        (update-list *list-of-existing-and-not-yet-used-names* name))
  (selectq *what-to-save*
    (names nil)
    (all
     ;; update used configuration names
     (setq *configuration-accumulated-so-far* (copy-list *constraints-to-choose-from*)
           *known-configuration-names* (loop for configuration in *configuration-accumulated-so-far*
                                             collect (first configuration))))
    (but
     ;; delete the edited one from the list
     (setq *configuration-accumulated-so-far* (copy-list *constraints-to-choose-from*))
     (delete-element-from-list *configuration-accumulated-so-far* *configuration-to-edit*)
     (setq *known-configuration-names* (loop for configuration in *configuration-accumulated-so-far*
                                             collect (first configuration))))))



(defun frame-editor ()
  "An already existing frame can be edited by the user. The input can be either the
flavor name of the frame or an instance of that flavor. The user must specify which
one he chose. This might be changed latter."
  (using-resource (*window-for-code-generation* temporary-window-code-generation *window-maker*)
    ;; we should remember the present label of the window.
    (let ((old-label (funcall *window-for-code-generation* :label))
          (*what-to-save* nil)
          (*frame-to-edit* *frame-to-edit*)
          *configuration-name*
          *configuration-to-edit*
          *constraints-to-choose-from*
          list-of-sensitive-items
          line)
      ;; clear every thing in window
      (funcall *window-for-code-generation* :delete-all-elements)
      (setq *configuration-name*
            (caar (setq *constraints-to-choose-from*
                        (if *frame-to-edit* (funcall *frame-to-edit* :constraints)
                          (funcall (setq *frame-to-edit*
                                         (make-instance 'tv:peek-frame)) :constraints)))))
      (setq *configuration-to-edit* (first *constraints-to-choose-from*))
      ;; set the label to window editor frame.
      (funcall *window-for-code-generation* :set-label '(:string "Edit frame configuration" :font fonts:metsi :centered))
      (setq *what-to-edit* 'tv:inspect-frame)
      (setq line (create-line "Input is" '*what-to-edit* 'tv:peek-frame
                              '((:input-type "Flavor") (:input-type "Instance of flavor")) "Flavor" fonts:tr12b))
      (funcall *window-for-code-generation* :add-line-to-be-displayed line)
      (funcall *window-for-code-generation* :output-string-to-window (format nil "~S" 'tv:inspect-frame))
      (setq line (create-line "Configuration to edit" '*configuration-name* *configuration-name*
                              '((:configuration-name "Get new configuration name")) nil nil))
      (funcall *window-for-code-generation* :add-line-to-be-displayed line)
      (update-list list-of-sensitive-items ':input-type)
      (update-list list-of-sensitive-items ':what-to-save)
      (and (> (length *constraints-to-choose-from*) 1) (update-list list-of-sensitive-items ':configuration-name))
      (funcall *window-for-code-generation* :set-sensitive-item-types list-of-sensitive-items)
      (setq line (create-line "What do you want to save" '*what-to-save* nil
                              '((:what-to-save "None") (:what-to-save "Names")
                                (:what-to-save "everything")
                                (:what-to-save "all but edited configuration")) "None" fonts:tr12b))
      (setf (selected-element line) "None")
      (funcall *window-for-code-generation* :add-line-to-be-displayed line)
      (tv:io-buffer-clear (funcall *window-for-code-generation* :io-buffer))
      ;; now everything is set up to ask the user to do his part.
      (funcall *window-for-code-generation* :expose-near (list ':point (fix (// (funcall *graphic-window-area* :width) 2))
                   (fix (// (funcall *graphic-window-area* :height) 2))))
      (unwind-protect
          (loop as blip = (funcall *window-for-code-generation* :list-tyi)
                as type = (first blip)
                as object = (second blip)
                do
                (selectq type
                  (:input-type (get-input-type object *window-for-code-generation*))
                  (:configuration-name
                   (get-name-of-configuration-to-edit *window-for-code-generation*))
                  (:what-to-save (get-what-to-save-for-frame object *window-for-code-generation*))
                  (:choice-box
                   (if (equal object 'Abort) (return T)
                     (and *what-to-save* (get-save-things))
                     (parse-configuration *configuration-to-edit*)
                     (funcall *graphic-window-area* :redisplay)
                     (return t)))))
        (funcall *window-for-code-generation* :deexpose)
        (funcall *window-for-code-generation* :set-label old-label)
        (funcall *window-for-code-generation* :deactivate)))))
