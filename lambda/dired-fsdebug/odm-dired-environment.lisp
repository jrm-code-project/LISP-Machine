;;; -*- Mode:LISP; Package:Zwei; Fonts:(CPTFONT); Base:10 -*-

;;; Copyright (C) Lisp Machine, Inc. 1984, 1985, 1986
;;;   See filename "Copyright" for
;;; licensing and release information.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                      ;;;
;;;                                                                      ;;;
;;;                    ENVIRONMENT INTERFACE FUNCTIONS                   ;;;
;;;                                                                      ;;;
;;;                                                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; These routines adjust the state of the environment in which ODM or DIRED
;;; operates, turning on and off various conditions depending on whether
;;; ODM/DIRED is the current program, and whether ODM/DIRED is being used in
;;; its own right or is being used to edit an ordinary file via its Zmacs
;;; pane. When neither ODM nor DIRED is operating, default conditions are
;;; restored. Note that the success of this strategy depends on no-one else
;;; playing similar games without reference to what we do here.

;;; The :AFTER methods that drive the environment tailorings, and all the
;;; routines they call, are potentially dangerous because they have global
;;; effects on the system. Changes to them must be made with great caution!!


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (TV:ESSENTIAL-WINDOW :AFTER :MOUSE-SELECT)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Takes a look at each window that is displayed (via the misnamed :mouse-select
;;; operation) and sets the environment to support ODM, DIRED, or anything else.


(defmethod (TV:ESSENTIAL-WINDOW :AFTER :MOUSE-SELECT) (&rest ignore)
  (cond ((and (typep self 'zwei:zmacs-frame)
              (typep (send self :selection-substitute) 'zwei:zmacs-window-pane)
              (boundp 'zwei:*dired-constraint-frame*)
              (typep (send self :superior) 'zwei:dired-constraint-frame))
         (zwei:mouse-select-dired self))
        ((and (boundp 'zwei:*dired-constraint-frame*)      ;;; Prevents action on mini-buffer selection
              (typep self 'zwei:dired-constraint-frame)))
        ((typep self 'zwei:gateway-constraint-frame)
         (zwei:mouse-select-gateway self))
        (t (zwei:mouse-select-standard self))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (ZMACS-WINDOW :AFTER :SET-INTERVAL-INTERNAL)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Takes a look at each buffer that is displayed and sets the environment to
;;; support ODM, DIRED, or standard Zwei.


(defmethod (ZMACS-WINDOW :AFTER :SET-INTERVAL-INTERNAL) (&rest ignore)
  (cond ((and (boundp 'zwei:*dired-display-pane*)
              (eq (send self :superior) zwei:*dired-display-pane*))
         (set-interval-internal-dired self))
        ((and (boundp 'zwei:*display-pane-1*)
              (eq (send self :superior) zwei:*display-pane-1*))
         (set-interval-internal-gateway self))
        (t (set-interval-internal-standard self))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                      ;;;
;;;                                                                      ;;;
;;;                           ODM ENVIRONMENT                            ;;;
;;;                                                                      ;;;
;;;                                                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun MOUSE-SELECT-GATEWAY (ignore)
  ;;; Extract from closure to global space the values that will be needed by
  ;;; processes operating outside the closure. This allows all ODM routines
  ;;; to think of things the same way no matter what process they are running
  ;;; under.
  (setq *editor-closure*
        (dolist (pane (send self :inferiors))
          (when (typep pane 'zwei:zmacs-frame)
            (return (send pane :editor-closure)))))
  (when (and *editor-closure*
             (boundp-in-closure *editor-closure* '*gateway-constraint-frame*))
    (setq *display-pane-1*
          (symeval-in-closure *editor-closure* '*display-pane-1*))
    (setq *selected-command*
          (symeval-in-closure *editor-closure* '*selected-command*))
    (setq *script-display-buffer*
          (symeval-in-closure *editor-closure* '*script-display-buffer*))
    (setq *current-script-frame*
          (symeval-in-closure *editor-closure* '*current-script-frame*))
    (setq *current-script-frame*
          (symeval-in-closure *editor-closure* '*current-script-frame*))
    (setq *current-gateway-buffer*
          (symeval-in-closure *editor-closure* '*current-gateway-buffer*)))
  (setq tv:scroll-bar-max-speed 5)
  (set-gateway-mouse-handling))


(defun SET-INTERVAL-INTERNAL-GATEWAY (sheet)
  (display-standard-blinkers sheet)
  (set-gateway-mouse-handling)
  (set-gateway-zwei-commands))


(defun SET-GATEWAY-MOUSE-HANDLING (&aux buffer node)
  (send *global-mouse-char-blinker* :set-visibility nil)
  (setq buffer (send (send zwei:*display-pane-1* :selection-substitute) :interval))
  (cond ((and (eq buffer *script-display-buffer*)
              *current-script-frame*
              (setq node (get-node (script-frame-node-name *current-script-frame*) 'quiet 'continue))
              (gnode-script node))
         (without-interrupts
           (setq *global-mouse-char-blinker-handler* 'blink-line
                 *global-mouse-char-blinker-documentation-string*
                 "Click left on a node name to display that node."
                 *gateway-mouse-handling-is-on* 'blink-line)))
        ((and (eq buffer *current-gateway-buffer*)
              (display-mode-p))
         (without-interrupts
           (setq *global-mouse-char-blinker-handler* 'blink-ref
                 *global-mouse-char-blinker-documentation-string*
                 "Click left on node name to visit it; or on a term to see its documentation."
                 *gateway-mouse-handling-is-on* 'blink-ref)))
        (t (turn-gateway-mouse-handling-off)))
  (tv:mouse-wakeup))


(defun TURN-GATEWAY-MOUSE-HANDLING-OFF ()
  (without-interrupts
    (send *global-mouse-char-blinker* :set-visibility nil)
    (setq *global-mouse-char-blinker-handler* nil
          *global-mouse-char-blinker-documentation-string* nil
          *gateway-mouse-handling-is-on* nil)))


(defun SET-GATEWAY-ZWEI-COMMANDS ()
  (set-comtab *mode-comtab* '(#/mouse-1-1 com-mouse-1-1
                              #/mouse-2-1 com-mouse-mark-thing)))


(defun MOUSE-1-1 ()
  (selectq *gateway-mouse-handling-is-on*
    (blink-line (com-select-from-script))
    (blink-ref (select-via-reference))
    (nil (com-mouse-mark-region))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                      ;;;
;;;                                                                      ;;;
;;;                          DIRED ENVIRONMENT                           ;;;
;;;                                                                      ;;;
;;;                                                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun MOUSE-SELECT-DIRED (ignore)
  (setq tv:scroll-bar-max-speed 2.5)
  (if (eq (send (send (send self :selection-substitute) :interval) :saved-major-mode) 'zwei:dired-mode)
      (set-dired-mouse-handling)
    (set-standard-mouse-handling)))


(defun SET-INTERVAL-INTERNAL-DIRED (sheet &aux pathname)
  (setq pathname (send (send sheet :interval) :pathname))
  (if pathname
      (set-interval-internal-dired-1 sheet pathname *dired-pathname-pane*)
    (process-run-function  "GET-PATHNAME" 'get-pathname sheet *dired-pathname-pane*)))


(defun GET-PATHNAME (sheet pathname-pane &aux pathname)
  (process-wait
    "Waiting for Pathname"
    #'(lambda () (setq pathname (send (send sheet :interval) :pathname))))
  (set-interval-internal-dired-1 sheet pathname pathname-pane))


(defun SET-INTERVAL-INTERNAL-DIRED-1 (sheet pathname pathname-pane)
  (send pathname-pane
        :set-item-list
        (list
          (list
            (send pathname :string-for-printing)
            :no-select nil
            :font 'mets)))
  (if (neq (send (send sheet :interval) :saved-major-mode) 'zwei:dired-mode)
      (set-interval-internal-standard sheet)
    (display-dired-blinkers sheet)
    (set-dired-mouse-handling)
    (set-dired-zwei-commands sheet)))


(defun SET-DIRED-MOUSE-HANDLING ()
  (send *global-mouse-char-blinker* :set-visibility nil)
  (without-interrupts
    (setq *global-mouse-char-blinker-handler* 'blink-dired-line
          *global-mouse-char-blinker-documentation-string*
          "Click left to make the outlined file the current file.  R:Menu.  R2:System menu."
          *gateway-mouse-handling-is-on* nil))
    (tv:mouse-wakeup))


(defun SET-DIRED-ZWEI-COMMANDS (sheet &aux mode-comtab)
  (setq mode-comtab (symeval-in-closure (send sheet :editor-closure) 'zwei:*mode-comtab*))
  (set-comtab mode-comtab '(#/mouse-1-1 com-dired-mouse-move-point
                            #/mouse-2-1 com-dired-mouse-move-point)))


(defmethod (ZWEI:ZMACS-WINDOW-PANE :SET-POINT-BLINKER) (blinker)
  (setq point-blinker blinker))


(defun DISPLAY-DIRED-BLINKERS (sheet &aux dired-blinker dummy-blinker current-point-blinker)
  (setq inhibit-scheduling-flag t)
  (tv:sheet-open-all-blinkers sheet)
  (when (location-boundp (locate-in-instance sheet 'point-blinker))
    (setq current-point-blinker (send sheet :point-blinker))
    (unless (eq (type-of current-point-blinker) 'tv:unblinking-reverse-video-line-blinker)
      (unless (setq dired-blinker (get-dired-line-blinker-for-sheet sheet))
        (setq dired-blinker
              (tv:make-blinker
                sheet
                'tv:unblinking-reverse-video-line-blinker
                :x-pos (send current-point-blinker :x-pos)
                :y-pos (send current-point-blinker :y-pos)
                :half-period 60
                :visibility :ON
                :deselected-visibility :OFF
                :follow-p nil)))
      (unless (setq dummy-blinker (get-dummy-blinker-for-sheet sheet))
        (setq dummy-blinker
              (tv:make-blinker
                sheet
                'tv:dummy-rectangular-blinker
                :x-pos (send current-point-blinker :x-pos)
                :y-pos (send current-point-blinker :y-pos)
                :visibility :OFF
                :deselected-visibility :OFF
                :follow-p (send current-point-blinker :follow-p))))
      (setf (tv:sheet-blinker-list sheet) (delq current-point-blinker (tv:sheet-blinker-list sheet)))
      (setf (tv:sheet-blinker-list sheet) (delq dummy-blinker (tv:sheet-blinker-list sheet)))
      (setf (tv:sheet-blinker-list sheet)
            (append
              (tv:sheet-blinker-list sheet)
              (list dummy-blinker)))
      (send dired-blinker :set-visibility :ON)
      (send sheet :set-point-blinker dired-blinker))
    (setq inhibit-scheduling-flag nil)))


(defun GET-DIRED-LINE-BLINKER-FOR-SHEET (sheet)
  (dolist (blinker (tv:sheet-blinker-list sheet))
    (when (typep blinker 'tv:unblinking-reverse-video-line-blinker)
      (return blinker))))


(defun GET-DUMMY-BLINKER-FOR-SHEET (sheet)
  (dolist (blinker (tv:sheet-blinker-list sheet))
    (when (typep blinker 'tv:dummy-rectangular-blinker)
      (return blinker))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                      ;;;
;;;                                                                      ;;;
;;;                         STANDARD-ENVIRONMENT                         ;;;
;;;                                                                      ;;;
;;;                                                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun MOUSE-SELECT-STANDARD (ignore)
  (setq tv:scroll-bar-max-speed 5)
  (set-standard-mouse-handling))


(defun SET-INTERVAL-INTERNAL-STANDARD (sheet)
  (display-standard-blinkers sheet)
  (set-standard-mouse-handling)
  (set-standard-zwei-commands sheet))


(defun SET-STANDARD-MOUSE-HANDLING ()
  (send *global-mouse-char-blinker* :set-visibility nil)
  (without-interrupts
    (send *global-mouse-char-blinker* :set-visibility nil)
    (setq *global-mouse-char-blinker-handler* nil
          *global-mouse-char-blinker-documentation-string* nil
          *gateway-mouse-handling-is-on* nil))
    (tv:mouse-wakeup))


(defun SET-STANDARD-ZWEI-COMMANDS (sheet &aux mode-comtab)
  (setq mode-comtab (symeval-in-closure (send sheet :editor-closure) 'zwei:*mode-comtab*))
  (set-comtab mode-comtab '(#/mouse-1-1 com-mouse-mark-region
                            #/mouse-2-1 com-mouse-mark-thing)))


(defun DISPLAY-STANDARD-BLINKERS (sheet &aux current-point-blinker standard-blinker)
  (when (location-boundp (locate-in-instance sheet 'point-blinker))
    (setq current-point-blinker (send sheet :point-blinker))
    (unless (eq (type-of current-point-blinker) 'tv:rectangular-blinker)
      (setq inhibit-scheduling-flag t)
      (tv:sheet-open-all-blinkers sheet)
      (send current-point-blinker :set-visibility nil)
      (setq standard-blinker
            (tv:make-blinker
              sheet
              'tv:rectangular-blinker
              :x-pos (send current-point-blinker :x-pos)
              :y-pos (send current-point-blinker :y-pos)
              :visibility :OFF
              :deselected-visibility :OFF
              :follow-p (send current-point-blinker :follow-p)))
      (setf (tv:sheet-blinker-list sheet) (delq standard-blinker (tv:sheet-blinker-list sheet)))
      (setf (tv:sheet-blinker-list sheet)
            (append
              (tv:sheet-blinker-list sheet)
              (list standard-blinker)))
      (send standard-blinker :set-visibility :BLINK)
      (send sheet :set-point-blinker standard-blinker)
      (setq inhibit-scheduling-flag nil))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                      ;;;
;;;                                                                      ;;;
;;;                         BLINKER HANDLERS                             ;;;
;;;                                                                      ;;;
;;;                                                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; BLINK-DIRED-LINE
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Draws a blinker that blinks a line. This blinker is used to highlight
;;; DIRED file entries. BLINK-DIRED-LINE is actually ZWEI:BLINK-FUNCTION with
;;; most of its brains removed.

(defun BLINK-DIRED-LINE (blinker window char x y line index &aux beg end sheet); string-length)
  (if (or (not (typep (tv:window-under-mouse) 'zmacs-window-pane))
          (null (line-previous line))
          (null (line-previous (line-previous line)))
          (null (line-next line)))
      (tv:blinker-set-visibility blinker nil)
    (setq beg 0)
    (setq end (array-active-length line))
    (setq sheet (window-sheet window))
    (tv:blinker-set-sheet blinker sheet)
    (sheet-set-blinker-cursorpos
      sheet
      blinker
      (- x (tv:sheet-string-length sheet line beg index))
      y)
    (tv:blinker-set-size
      blinker
      (plus (tv:sheet-string-length sheet line beg end) 2)
      (plus (font-char-height (aref (tv:sheet-font-map sheet) (ldb %%ch-font char))) 1))
    (tv:blinker-set-visibility blinker t)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; BLINK-LINE
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Handles a blinker that blinks a line. This blinker is used to highlight
;;; ODM script entries. BLINK-LINE is actually ZWEI:BLINK-FUNCTION with
;;; most of its brains removed.

(defun BLINK-LINE (blinker window char x y line index &aux beg end sheet)
  (setq beg 0)
  (setq end (array-active-length line))
  (if
    (or
      (equal char #/return)
      (equal end 0)
      (not (string-search-not-set *whitespace-chars* line))
      (not (typep (tv:window-under-mouse) 'zmacs-window-pane)))
    (tv:blinker-set-visibility blinker nil)
    (setq sheet (window-sheet window))
    (tv:blinker-set-sheet blinker sheet)
    (sheet-set-blinker-cursorpos
      sheet
      blinker
      (- x (tv:sheet-string-length sheet line beg index))
      y)
    (tv:blinker-set-size
      blinker
      (tv:sheet-string-length sheet line beg end)
      (font-char-height (aref (tv:sheet-font-map sheet) (ldb %%ch-font char))))
    (tv:blinker-set-visibility blinker t)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; BLINK-REF
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Handles a blinker that blinks a reference (cross-reference, or function
;;; for super-point). BLINK-REF is a minor variant of ZWEI:BLINK-FUNCTION.


(defun BLINK-REF (blinker window char x y line index &aux beg end sheet)
  (cond ((> tv:mouse-speed *blinking-function-maximum-mouse-speed*)
         (tv:blinker-set-visibility blinker nil))       ;moving too fast, forget it
        (t
         (multiple-value (nil nil beg end)
           (ref-under-mouse window char x y line index))
         (cond ((not (null beg))
                (setq sheet (window-sheet window))
                (tv:blinker-set-sheet blinker sheet)
                (sheet-set-blinker-cursorpos sheet blinker
                                             (- x (tv:sheet-string-length sheet line beg
                                                                          index))
                                             y)
                (tv:blinker-set-size blinker
                                     (+ 2 (tv:sheet-string-length sheet line beg end)) ;+1 for bold, +1 for space
                                     (1+ (font-char-height (aref (tv:sheet-font-map sheet)
                                                                (ldb %%ch-font char)))))
                (tv:blinker-set-visibility blinker t))
               (t
                (tv:blinker-set-visibility blinker nil))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; REF-UNDER-MOUSE
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Returns a reference (cross-reference, or function for super-point) if there
;;; is one under the mouse. If font is bold or tty, use atom rules.
;;; If font is reference font, reference is the entire substring in that font

(defconst BOLD-FONT-NUM 2)  ;Font C
(defconst TTY-FONT-NUM 3)   ;Font D
(defconst REF-FONT-NUM 4)   ;Font E

(defconst *SYM-CONSTITUENT-CHARS*
          '(#\- #\_ #\: #\% #\* #\& #\= #\< #\> #\+ #\$ #\/ #\\ #\^ #\ #\ #\))

(defun REF-UNDER-MOUSE (window &optional char x y line index &aux symbol font start end)
  "Returns the reference which the mouse is pointing at in WINDOW. NIL if not pointing at
one. The values are the reference pointed at, whether the reference is a nodename or a
symbol, and the start and end indices of the reference as a substring in that line.
All values are NIL if the position is not on a valid reference."

  (declare (values symbol line start end type))
  (or char (multiple-value (char x y line index)
             (mouse-char window)))
  (and char
       ( char #/CR)
       (setq font (char-font char))
       (select font
         (ref-font-num
          (do ((i index (1- i)))
              ((or (minusp i)
                   ( (char-font (aref line i)) ref-font-num))
               (setq start (1+ i))))
          (do ((i index (1+ i))
               (len (string-length line)))
              ((or (>= i len)
                   ( (char-font (aref line i)) ref-font-num))
               (setq end i)))
          (values (substring line start end) :node start end))
         ((bold-font-num tty-font-num)
          (when (or (alphanumericp char)
                    (cli:member (char-code char) *sym-constituent-chars* :test #'char=)
                    )
            (setq start
                (do* ((i index (1- i))
                      (c (aref line index) (if (minusp i) #\space (aref line i)))
                      (c-code (char-code c) (char-code c))
                      (c-font (char-font c) (char-font c)))
                     ((or (minusp i)
                          (not (or (alphanumericp c-code)
                                   (cli:member c-code *sym-constituent-chars* :test #'char=)))
                          (not (member c-font `(,bold-font-num ,tty-font-num))))
                      (1+ i)))
                end
                (do* ((i start (1+ i))
                      (c (aref line start) (aref line i))
                      (c-code (char-code c) (char-code c))
                      (c-font (char-font c) (char-font c)))
                     ((or (= i (string-length line))
                          (not (or (alphanumericp c-code)
                                   (cli:member c-code *sym-constituent-chars* :test #'char=)))
                          (not (member c-font `(,bold-font-num ,tty-font-num))))
                      i))
                symbol (catch-error (doc-symbol line :start start :end end)))
          (setq end (min (array-active-length line) end))
          (and (symbolp symbol)
               (get symbol :odm-node)
               (values (substring line start end) :symbol start end))))
         )))



