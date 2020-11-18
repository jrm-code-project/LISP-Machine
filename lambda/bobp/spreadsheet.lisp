;;; -*- Mode:LISP; Package:USER; Base:10; Readtable:ZL -*-
;;
;; spreadsheet hack
;; bobp

;; change ref to gridwin in cell methods to be what?

;;what is meaning of null, "", or :blank value or expr?

;;save mouse position after clicking on mouse-sensitive item,
;; restore when that cmd completes.

;; (fs:copy-file "dj:bobp;spreadsheet.lisp" "lad:bobp;spreadsheet.lisp")

;;flush type-ahead after select

;;which pane should be selectable?
;;  make a "break" typeout window that covers whole frame.
;;make trace window optional

;;redisplay: do :home instead of :clear-screen for simple redisplay
;;  also, only redisplay cell if cell tick is newer than window tick.

;;;;;;;;;;;;;;;;

; spreadsheet-window -> grid-window -> win -> sheet -> array -> cell
; cell -> sheet

;; handy copy of most recently selected spreadsheet-window
(defvar w nil)

;;;;;;;;;;;;;;;;

;; Special instance variables of spreadsheet-window

;; panes from initial instance of current spreadsheet-window
(defvar gridwin)
(defvar edwin)
(defvar tracewin)
(defvar statwin)

;; inferior grid-windows of this spreadsheet-window
(defvar all-gridwins nil)

;; sheets belonging to this spreadsheet-window
(defvar sheet-list nil)

;;;;;;;;;;;;;;;;

;; Special instance variables of grid-window

(defvar win)

;;;;;;;;;;;;;;;;

;; mouse-sensitive things that are printed on grid windows
(defvar *gridwin-alist* nil)

(defvar command-alist)

;; incremented whenever anything changes.
;; ???
(defvar timestamp t)

;; default width of a column
(defvar dfl-width 10.)

;; default format strings for printing out cells
(defvar value-format "~s")      ;;printing values
(defvar format-format "~s")     ;;printing format strings
(defvar expr-format "~s")       ;;printing expressions

;; cell
(defflavor cell
         (expr          ;;cell expression
          value         ;;value computed from expression
          sheet         ;;sheet (array) this cell is in
          row           ;;index of this cell in its array
          col
          value-valid-p ;;value is valid if not null
          display-valid-p ;;display is valid if disp-time matches global timestamp
          names         ;;list of names defined here (?)
          format        ;;display format string (one-shot value, or ptr to a shared value)
          dependents    ;;list of cells that reference this one
          )
         ()
  :settable-instance-variables)


;; display parameters for a grid-window
(defstruct (win :conc-name
                (:type :named-array))
  sheet         ;;window displays from this array
  (start-col 0) ;;top-left corner starts displaying here
  (start-row 0) ;;
  (mode :value) ;;display mode; :value, :expr or :format
  width         ;;optional array of column widths
  )

;; spreadsheet data array and parameters
(defstruct (sheet :conc-name
                  (:type :named-array)
                  (:constructor make-empty-sheet))
  name          ;;name of this sheet
  array         ;;2-dim array for this sheet
  width         ;;array of column widths
  )

;;;;;;;;;;;;;;;;

;; the following are bound inside eval-cell and edit-cell

(defvar this-cell)
(defvar this-array)
(defvar this-row)
(defvar this-col)

(defun cell-ref (ar row col)
  (eval-cell (aref ar row col)))

(defmacro with-cell-bound
          ((cell)
           &body body)
  `(let ((this-cell ,cell)
         (this-array (sheet-array (cell-sheet ,cell)))
         (this-row (cell-row ,cell))
         (this-col (cell-col ,cell)))
     . ,body))

;;;;;;;;;;;;;;;;

;;later, add fancy mouse scrolling ...
;; how is window filled?
(defflavor grid-window
         ((win nil))
         (tv:basic-mouse-sensitive-items
          tv:window)
  :gettable-instance-variables
  :settable-instance-variables
  :inittable-instance-variables
  :special-instance-variables)

(defflavor cell-edit-window
         ()
         (tv:box-label-mixin
          tv:window)
  :gettable-instance-variables
  :settable-instance-variables
  :inittable-instance-variables)

;; may want to make cell-edit-pane be tv:lisp-listener
(defflavor spreadsheet-window
         (gridwin
          edwin
          tracewin
          statwin
          (all-gridwins nil)
          (sheet-list nil))
         (tv:alias-for-inferiors-mixin
          tv:inferiors-not-in-select-menu-mixin
          tv:process-mixin
          tv:bordered-constraint-frame-with-shared-io-buffer)
  (:default-init-plist
    :process t
    :panes `((grid-pane grid-window
                        :blinker-p nil
                        :label nil
                        :more-p nil
                        :save-bits t)
             (command-pane tv:command-menu
                           :save-bits t
                           :item-list nil)
             (cell-edit-pane cell-edit-window
                             :blinker-deselected-visibility :off
                             :blinker-flavor tv:rectangular-blinker
                             :blinker-p t
                             :more-p nil
                             :label ""
                             :save-bits t)
             (status-pane tv:window
                          :blinker-p nil
                          :more-p nil
                          :label nil
                          :save-bits t)
             (trace-pane tv:window
                         :blinker-deselected-visibility :on
                         :blinker-flavor tv:rectangular-blinker
                         :blinker-p t
                         :more-p nil
                         :label nil
                         :save-bits t)
             (zmacs-pane zwei:zmacs-frame
                         :save-bits t)
             )
    :constraints '((default (grid-pane command-pane cell-edit-pane trace-pane status-pane)
                            ((command-pane 2 :lines)
                             (cell-edit-pane 7 :lines)
                             (trace-pane 15 :lines)
                             (status-pane 1 :lines))
                     ((grid-pane :even)))
                   (editor (zmacs-pane grid-pane command-pane cell-edit-pane
                                       trace-pane status-pane)
                           ((zmacs-pane 30 :lines)
                            (command-pane 2 :lines)
                            (cell-edit-pane 7 :lines)
                            (trace-pane 10 :lines)
                            (status-pane 1 :lines))
                           ((grid-pane :even)))
                   )
    )
  :gettable-instance-variables
  :settable-instance-variables
  :inittable-instance-variables
  :special-instance-variables)

(defmethod (spreadsheet-window :after :init) (&rest ignore)
  (funcall-self :set-selection-substitute (funcall-self :get-pane 'trace-pane))) ;;'cell-edit-pane

(tv:add-system-key #/$ 'spreadsheet-window "Spreadsheet")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;to-do:
;;  remove explicit refs to 'gridwin
;;
(defmethod (spreadsheet-window :process-top-level) ()
  (setq w self)

  ;;get initial panes of frame
  ;;(send self :set-gridwin (setq grid-win (send self :get-pane 'grid-pane)))
  ;;(send self :set-edwin (setq ed-win (send self :get-pane 'cell-edit-pane)))
  ;;(send self :set-tracewin (setq trace-win (send self :get-pane 'trace-pane)))
  ;;(send self :set-statwin (setq stat-win (send self :get-pane 'status-pane)))
  (setq gridwin (send self :get-pane 'grid-pane))
  (setq edwin (send self :get-pane 'cell-edit-pane))
  (setq tracewin (send self :get-pane 'trace-pane))
  (setq statwin (send self :get-pane 'status-pane))

  (let ((cmdwin (send self :get-pane 'command-pane)))
    (send cmdwin :set-item-list command-alist))

  (when (null sheet-list)
    (push (make-sheet "Default" 20. 20.) sheet-list))

  (send gridwin :add-self (car sheet-list))
  ;;(add-gridwin gridwin (car sheet-list))

  (let ((*terminal-io* tracewin))
    (top-level-loop)))

;; what args should be passed to :MENU commands?
;; is raw item-list really what is passed to menu?
;;how does :typeout-execute blip indicate which window was clicked?
;;
;; add grid-window arg to :menu list ..
;;
(defun top-level-loop ()
  (do-forever
    (dolist (gw all-gridwins)
      (send gw :redisplay))
      ;;(redisplay gw))

    (let ((blip (send edwin :any-tyi)))
      (typecase blip
        (list
         (selectq (car blip)
           (:typeout-execute
            (handle-typeout-execute blip))
            ;;(apply (cadr blip) (caddr blip)))
           (:menu  ;;(:menu item-list mouse-mask command-pane)
            (send gridwin (caddr (cadr blip))))
            ;;(funcall (caddr (cadr blip)) grid-win))
           (t
            (format t "blip was ~s" blip))
           ))
        (t
         (format t "blip was ~s" blip))
        ))
    ))

;;(defun handle-typeout-execute (blip)
;;  (let ((fcn (cadr blip))
;;      (args (caddr blip)))
;;    ))
            ;; cell:
            ;;   (cell-func gridwin cell)
            ;;     (send gridwin :send-to-cell cell-func cell)
            ;;  or (let ((gridwin gridwin)) (send cell func))
            ;; row-header:
            ;;   (row-func gridwin row)
            ;;     (send gridwin row-func row)
            ;; col-header:
            ;;   (col-func gridwin col)
            ;;     (send gridwin col-func col)
            ;; empty-cell:
            ;;   (empty-cell-func gridwin row col)
            ;;     (send gridwin

;;add and initialize a grid-window
(defmethod (grid-window :add-self) (sheet)
  (when (null win)
    (setq win (make-win)))
  (setf (win-sheet win) sheet)
  (send self :set-item-type-alist *gridwin-alist*)
  (pushnew self all-gridwins))

(defmethod (grid-window :remove-self) ()
  (setf all-gridwins (delq self all-gridwins)))

#|
(defun add-gridwin (gw sheet)
  (let ((win (or (send gw :win) (make-win))))
    (setf (win-sheet win) sheet)
    (send gw :set-item-type-alist *gridwin-alist*)
    (send gw :set-win win)
    (pushnew gw all-gridwins)))

;;remove a grid-window from the world
(defun remove-gridwin (gw)
  (setf all-gridwins (delq gw all-gridwins)))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;evaluate a cell
;; 1. catch errors in eval; store garbage-marker if error
;; 2. don't update timestamp if error
(defun eval-cell (cell)
  (cond
    ((cell-value-valid-p cell)                  ;value is already valid
     (cell-value cell))
    (t
     (with-cell-bound (cell)                    ;recompute value
       (let ((val (eval (cell-expr cell))))
         (setf (cell-value cell) val)
         (setf (cell-value-valid-p cell) t)
         (setf (cell-display-valid-p cell) nil)
         val)))
    ))

(defun invalidate-cell (cell)
  ;;follow reference chain of cell and invalidate users of this cell
  (setf (cell-value-valid-p cell) nil)
  (setf (cell-display-valid-p cell) nil))

;;;;;;;;;;;;;;;;

(defun aref-safe (ar &rest indices)
  (let ((in indices))
    (when (null ar)
      (return-from aref-safe nil))
    (dolist (ad (array-dimensions ar))
      (let ((i (pop in)))
        (when (>= i ad)
          (return-from aref-safe nil))))
    (apply #'aref ar indices)))

(defun width-to-use (win col)
  ;;1. try win-width array for win
  ;;2. try sheet-width array for sheet
  ;;3. use dfl-width
  (or (aref-safe (win-width win) col)
      (aref (sheet-width (win-sheet win)) col)
      dfl-width))

(defun format-to-use (cell)
  (or (cell-format cell)
      value-format))

;;compute number of rows and cols that fit
(defun compute-max-row-col (gw)
  (let* ((win (send gw :win))
         (sheet (win-sheet win))
         max-row max-col)
    (multiple-value-bind (width height)
        (send gw :size-in-characters)
      (setq max-row (min (+ (win-start-row win) (1- height))
                         (array-dimension (sheet-array sheet) 0)))
      (do* ((col (win-start-col win) (1+ col))
            (tot 0 (+ tot (width-to-use win col))))
           ((or (<= (array-length (sheet-width sheet)) (1+ col))
                (> tot width)))
        (setq max-col col)))
    (values max-row max-col)))

;;display a window into a sheet
;;  arg is the grid-window to refresh
;;  it has an instance-var that says what "window" to use ...
;;(defun redisplay (gw &optional (new-p nil))
;;  (let* ((win (send gw :win))
;;       (ar (sheet-array (win-sheet win))))

(defmethod (grid-window :redisplay) (&optional (erase-p nil))
  (let ((ar (sheet-array (win-sheet win))))
    (multiple-value-bind (max-row max-col)
        (compute-max-row-col self)
      (send self :clear-screen)

      ;;display column headers
      (format self "~&    ")
      (loop for col from (win-start-col win) below max-col
            do (send self :item 'column-header (list self col)
                     "~Vd" (width-to-use win col) col)
            )

      ;;display each row
      (loop for row from (win-start-row win) below max-row
            do
            (send self :item 'row-header (list self row)
                  "~&~3d " row)

            ;;display each column
            (loop for col from (win-start-col win) below max-col
                  do
                  (let ((cell (aref ar row col)))
                    (cond
                      (cell
                       (send cell :eval)
                       (send self :item 'cell (list self cell)
                             "~a" (cell-string cell win)))
                      (t
                       (send self :item 'empty-cell (list self row col)
                             "~V@T" (width-to-use win col)))
                      )
                    ))
            ))))

;;return the actual string to print, already adjusted to the correct width.
;;the window indicates how long the string should be.
(defun cell-string (cell win)
  (let ((max-len (width-to-use win (cell-col cell))))
    (cond
      ((null cell)
       (format nil "~V@t" max-len))
      (t
       (let ((output-string
               (selectq (win-mode win)
                 (:value
                  (format nil (format-to-use cell) (or (cell-value cell) "")))
                 (:expr
                  (format nil expr-format (or (cell-expr cell) "")))
                 (:format
                  (format nil format-format (or (cell-format cell) "")))
                 (t
                  (format nil "win-mode ~s" (win-mode win))))
               ))
         (let ((len (length output-string)))
           (cond
             ((> len max-len)
              (substring output-string 0 max-len))
             (t
              (format nil "~V@t~a" (- max-len len) output-string)))
           ))))
    ))

;;;;;;;;;;;;;;;;

(defun make-sheet (name nrows ncols)
  (let ((sheet (make-empty-sheet)))
    (setf (sheet-name sheet) name)
    (setf (sheet-array sheet) (make-array `(,nrows ,ncols)))
    (setf (sheet-width sheet) (make-array ncols))
    sheet
    ))

(defun change-sheet-dimensions (sheet nrows ncols)
  (let ((new-array (make-array nrows ncols))
        (new-width (make-array ncols)))
    (dotimes (c (min ncols (array-dimension (sheet-array sheet) 1)))
      (dotimes (r (min nrows (array-dimension (sheet-array sheet) 0)))
        (setf (aref new-array r c) (aref (sheet-array sheet) r c)))
      (setf (aref new-width c) (aref (sheet-width sheet) c))
      )
    (setf (sheet-array sheet) new-array
          (sheet-width sheet) new-width)
    sheet))

;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;

(setq command-alist
      (sortcar '(("Values" :value :display-values       ;cmd-disp-values
                  :documentation "Display cell values")
                 ("Expressions" :value :display-expressions     ;cmd-disp-expr
                  :documentation "Display cell expressions")
                 ("Formats" :value :display-formats     ;cmd-disp-format
                  :documentation "Display cell format strings")
                 ("Recomp Off" :value :recompute-off    ;cmd-recomp-off
                  :documentation "Don't recompute")
                 ("Recomp On" :value :recompute-on      ;cmd-recomp-on
                  :documentation "Recompute after every change")
                 ("Recompute" :value :recompute-once    ;cmd-recomp
                  :documentation "Recompute just once")
                 ("Home" :value :home-window    ;cmd-home
                  :documentation "Reposition this window to (0,0)")
                 ("Page Left" :value :page-left ;cmd-page-left
                  :documentation "Reposition window one screen-width left")
                 ("Page Up" :value :page-up     ;cmd-page-up
                  :documentation "Reposition window one screen-height up")
                 )
               #'string-lessp))

;;display mode: value, expr, format
;;recompute on, off; recompute
;;home; page left, right, up, down

(defmethod (grid-window :display-values) ()
  (setf (win-mode win) :value))

(defmethod (grid-window :display-expressions) ()
  (setf (win-mode win) :expr))

(defmethod (grid-window :display-formats) ()
  (setf (win-mode win) :format))

(defmethod (grid-window :recompute-on) ()
  )

(defmethod (grid-window :recompute-off) ()
  )

(defmethod (grid-window :recompute-once) ()
  )

(defmethod (grid-window :page-left) ()
  )

(defmethod (grid-window :page-up) ()
  )

(defmethod (grid-window :home-window) ()
  (move-window self 0 0))

#|
(defun cmd-disp-values (gw)
  (let ((win (send gw :win)))
    (setf (win-mode win) :value)))

(defun cmd-disp-expr (gw)
  (let ((win (send gw :win)))
    (setf (win-mode win) :expr)))

(defun cmd-disp-format (gw)
  (let ((win (send gw :win)))
    (setf (win-mode win) :format)))

(defun cmd-home (gw)
  (move-window gw 0 0))
|#

;;;;;;;;;;;;;;;;

;;selecting existing cell
;;args: (grid-window cell)

(tv:add-typeout-item-type *gridwin-alist* cell "Edit Cell" :edit t
                          "Edit this cell")

(defmethod (cell :edit) ()
  (edit-cell gridwin self))

(tv:add-typeout-item-type *gridwin-alist* cell "Recompute Cell" :eval nil
                          "Compute value of just this cell")

(tv:add-typeout-item-type *gridwin-alist* cell "Edit Format" :edit-format nil
                          "Edit format string for this cell")

(defmethod (cell :edit-format) ()
  (multiple-value-bind (new abort-p)
      (edit-sexpr (send self :format))
    (when (not abort-p)
      (send self :set-format new)
      ;;tick sheet and cell
      gridwin
      )
    ))

(tv:add-typeout-item-type *gridwin-alist* cell "Move Window" :move-window nil
                          "Move window to have this cell in top-left")

(defmethod (cell :move-window) ()
  (move-window gridwin (send self :row) (send self :col))
  gridwin)
  ;;tick sheet

(tv:add-typeout-item-type *gridwin-alist* cell "Blank Cell" :blank nil
                          "Blank this cell")

(defmethod (cell :blank) ()
  ;;follow refs to do protocol

  ;;setf this cell's array slot to nil
  (setf (aref (sheet-array (send self :sheet)) (send self :row) (send self :col)) nil)
  ;;tick sheet and cell
  gridwin
  )

;;;;;;;;;;;;;;;;

;;selecting column header
;;args: (grid-window col)

(tv:add-typeout-item-type *gridwin-alist* column-header "Change Width" col-cmd-width t
                          "Change width of this column")

(tv:add-typeout-item-type *gridwin-alist* column-header "Insert Column" col-cmd-insert nil
                          "Insert a new column here")

(tv:add-typeout-item-type *gridwin-alist* column-header "Delete Column" col-cmd-delete nil
                          "Delete this column")

;;;;;;;;;;;;;;;;

;;selecting row header
;;args: (grid-window row)

(tv:add-typeout-item-type *gridwin-alist* row-header "Row Header" row-cmd-nop t
                          "Do Nothing to this Row")

(defun row-cmd-nop (gw row)
  (format t "~&row ~d" row)
  gw)

;;;;;;;;;;;;;;;;

;;selecting empty cell
;;args: (grid-window row col)

;; create new cell and edit it

(tv:add-typeout-item-type *gridwin-alist* empty-cell "Enter New Cell" ec-cmd-empty-cell t
                          "Enter new cell")

(defun ec-cmd-empty-cell (gw row col)
  (let* ((win (send gw :win))
         (ar (sheet-array (win-sheet win)))
         (cell (make-cell (win-sheet win) row col)))
    (setf (aref ar row col) cell)
    (edit-cell gw cell)))
    ;;tick sheet and cell

(tv:add-typeout-item-type *gridwin-alist* empty-cell "Move Window" ec-cmd-move-window nil
                          "Move window to have this cell in top-left")

(defun ec-cmd-move-window (gw row col)
  (move-window gw row col))

;;;;;;;;;;;;;;;;

;;stuff called by commands

;; first value is new sexpr string, second is non-null if aborted.
(defun edit-sexpr (l)
  (unwind-protect
      (let ((starting-str (if l (format nil "~s" l) "")))
        (do-forever
          (send edwin :select)                  ;turn on cursor
          (send edwin :clear-screen)
          (send edwin :force-kbd-input starting-str)

          (multiple-value-bind (str eof-p ignore)
              (read-delimited-string #/end edwin)
            (cond
              (eof-p
               ;;got error instead of <end>
               (set-edwin-error "end-of-file reading expr")
               (return-from edit-sexpr (values nil t)))
              ((= (length str) 0)
               (return-from edit-sexpr (values :blank nil)))
              (t
               (multiple-value-bind (expr error-p)
                   (ignore-errors
                     (multiple-value-bind (sexpr end-char-pos)
                         (read-from-string str)
                       (if (> (length str) end-char-pos)
                           :extra
                         sexpr)))
                 (cond
                   ((eq expr :extra)
                    (set-edwin-error "warning: extra characters at end of expression"))
                   (error-p
                    (set-edwin-error "error reading expr"))
                   (t
                    (return-from edit-sexpr (values expr nil)))))
               ))
            ;;got error, try again
            (setq starting-str str))
          ))
    (send (send w :gridwin) :select)            ;turn off cursor
    (send edwin :set-label nil)
    ))

(defvar edwin-label)

(defun set-edwin-label-string (gw cell)
  (setq edwin-label (format nil "~a: sheet=~a row=~d col=~d"
                            (send gw :name)
                            (sheet-name (cell-sheet cell))
                            (cell-row cell)
                            (cell-col cell)))
  (send edwin :set-label edwin-label))

(defun set-edwin-error (fmt &rest fmt-args)
  (send edwin :set-label (format nil "<<< ~? >>>~&~a" fmt fmt-args edwin-label)))

(defun edit-cell (gw cell)
  (set-edwin-label-string gw cell)
  (multiple-value-bind (new abort-p)
      (edit-sexpr (cell-expr cell))
    (when (null abort-p)
      (setf (cell-expr cell) new)
      (invalidate-cell cell)      ;;tick sheeet and cell
      )))

(defun move-window (gw row col)
  (let ((win (send gw :win)))
    (when row
      (setf (win-start-row win) row))
    (when col
      (setf (win-start-col win) col))
    ))

;;;;;;;;;;;;;;;;
