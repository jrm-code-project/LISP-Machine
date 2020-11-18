;;; -*- Mode:LISP; Package:(PAINT GLOBAL); Base:10; Readtable:CL -*-
;;
;;  Paint  A perq painting program (similar to MacPaint)
;;
;;  Semi-finalized version put together 4/21/85
;;
;;  Robert Rose
;;

;; hacked up from the Spice Lisp version to run in ZetaLisp-Plus
;; because it is interesting to see how portable supposedly
;; highly system-dependant code really is.

;(load "press.sfasl")

;(in-package "PAINT" :use '("LISP" "VIEWPTUSER" "SAPPHUSER" "SAPPHDEFSDEFS"
;                          "ACCINTUSER" "ACCINTDEFS"))


;
;  Definitions for the PAINT drawing program
;
;  'paintdef.slisp'
;


(defvar *paintstuff*) ; global holder of paint information

;  Some strings that are used in a lot of places
(defvar *selcorner* "(L or R)  --  Select A Corner    M -- Back to Menu")  ;; const

(defvar *blank-line* "                                                          ")


; other variables

(defvar *menu-pos-x* 600)
(defvar *menu-other-pos-y* 500)

(defvar *top-patt*)

(defvar *patt-spacer* 74)
(defvar *brush-w*)
(defvar *brush-h*)
(defvar *brush-x*)
(defvar *brush-y*)

;brush positions
(defvar *brush-64x*)
(defvar *brush-32x*)
(defvar *brush-16x*)
(defvar *brush-8x*)
(defvar *brush-64y*)
(defvar *brush-32y*)
(defvar *brush-16y*)
(defvar *brush-8y*)

(defvar *last-grid-x*)
(defvar *last-grid-y*)

(defvar *pattern-x*)
(defvar *pattern-y*)

(defvar *current-draw-fun*)
(defvar *move-area-rop*)

(defconstant LEFTDOWN 83)
(defconstant LEFTUP 75)
(defconstant RIGHTDOWN 84)
(defconstant MIDDLEDOWN 82)
(defconstant NOTHING 85)



(defstruct paintstruct
  "This structure holds pertinant PAINT information"
  window    ;the main window
  drawport  ;drawings to here
  image     ;the image on leaving
  off-view  ;offscreen viewport
  off-whole ;offscreen viewport
  rot-view  ;offscreen viewport
  font      ;the current font (set to system font at first)
  pixels-x  ;pixels across for image area
  pixels-y  ;pixels down for image area
  screen-y  ;pixels down for whole screen
  screen-x  ;pixels across for whole screen
  grid-mod  ;grid mod value
  )




;
;  'paint.slisp'
;

;
;  Asks user for confirmation
;
(defun confirm (message)
  (makewinlistener *userwindow*)
  (setwindowtitle *userwindow* message )
  (let ((answer (yes-or-no-p)))
    (setwindowtitle  *userwindow* *blank-line*)
    answer))

;
;   Flash-area turns on then off a rectangular area bound by x1 y1, x2 y2.
;
(defun half-flash (x1 y1 x2 y2 dp)
     (vpline dp :xorline x1  y1  x1  y2)
     (vpline dp :xorline x1  y1  x2  y1)
     (vpline dp :xorline x2  y1  x2  y2)
     (vpline dp :xorline x1  y2  x2  y2))


(defun flash-area (x1 y1 x2 y2 dp)
  (progn
    (half-flash x1 y1 x2 y2 dp)
    (half-flash x1 y1 x2 y2 dp)))

;
;  Get the drawing viewport ready
;
(defun prepare-drawport (title)
  (setwindowtitle  (paintstruct-window *paintstuff*) title)
  (makewinlistener (paintstruct-window *paintstuff*))
  (setlistener (paintstruct-drawport *paintstuff*))
  (setcursorpos (paintstruct-drawport *paintstuff*)
                *last-grid-x* *last-grid-y*))

;;
;;   Allows the user to select a rectangle in the window and
;; highlights it while he is choosing  returns LeftX,UpperY,Wid,Hgt,
;;

(defun SelectWindowRegion (inwindow port)
  (let ((oldx 0)
        (oldy 0))
    (setwindowtitle inwindow *Selcorner*)
    (multiple-value-bind (cm char y x)
                         (get-points nil port)
      (declare (ignore char))
      (setq oldx x oldy y)
      (cond ((= cm MIDDLEDOWN)
             nil)
            ((left-or-right cm)
             (setwindowtitle inwindow *Selcorner* )
             (loop
              (multiple-value-bind (cm* char y* x*)
                                   (get-points t port)
                (declare (ignore char))
                (when (or (/= oldx x*)
                          (/= oldy y*))
                  (half-flash x y oldx oldy port)
                  (setq oldx x* oldy y*)
                  (half-flash x y x* y* port))
                (cond ((left-or-right cm*)
                       (half-flash x y x* y* port)
                       (return (values (min x x*) (min y* y)
                                       (1+ (- (max x x*) (min x x*)))
                                       (1+ (- (max y y*) (min y y*))))))
                      ((= cm* MIDDLEDOWN)
                       (half-flash x y x* y* port)
                       (return nil))))))))))

;
;  If the left or right key was pressed; or a keyboard character.
;
(defun left-or-right (key)
  (or (=  key LEFTDOWN)
      (=  key RIGHTDOWN)))


;
;   Get-points returns where the puck was clicked
;   If return-p is true, it returns after one scan regardless of what
;   is pressed or not.  If return-p is nil, get-points waits until you
;   press something.
;
;   Optional parameters are the x1, y1 x2,y2 broders you want to stay within;
;   default is the whole screen (to the left of the menu stuff)
;
(defmacro constrain (num modu)
  `(* ,modu (floor ,num ,modu)))

(defun get-points (return-p vp
                            &optional
                            (xleft 0) (ytop 0)
                            (xright (paintstruct-pixels-x *paintstuff*))
                            (ybot   (paintstruct-pixels-y *paintstuff*)))
  (let ((gmod (paintstruct-grid-mod *paintstuff*))
        (a nil)
        (b nil)
        (reg nil)
        (c nil)
        (d nil))
    (loop (multiple-value-bind (a b reg c d) (getevent vp :keywaitdiffpos)
            (when (and (= 1 reg)
                       (> d xleft)
                       (> c ytop)
                       (< d xright)
                       (< c ybot))
              (when (or (left-or-right a)
                        (= a 0)
                        (= a MIDDLEDOWN))
                (return (values a b (constrain c gmod) (constrain d gmod))))
              (when return-p
                (return (values a b (constrain c gmod) (constrain d gmod)))))))))


(defun get-integer (message low high)
  (setwindowtitle  *userwindow* message)
  (loop
   (let ((new (parse-integer (read-line) :junk-allowed t)))
     (when (and (not (null new))
                (<= low new high ))
       (return new))
     (princ "Illegal value, try again:")
     (force-output))))
;;
;  Let the user set the mouse jump value
;;
(defun jump-choose ()
  (makewinlistener *userwindow*)
  (let ((new (get-integer
              "Select mouse jump value; Greater than 0; Less than 100." 1 99)))
    (setf (paintstruct-grid-mod *paintstuff*) new)
    (setwindowtitle  *userwindow* *blank-line*)))

;
;  Output file error message
;

(defun file-error ()
  (princ "Illegal or nonexistent file.")
  (force-output))

;
;   Select-file asks the user for a file name in the lisp window.
;
(defun select-file (message)
  (makewinlistener *userwindow*)
  (setwindowtitle *userwindow* message)
  (terpri)
  (let ((new-input (read-line)))
    (setwindowtitle *userwindow* *blank-line*)
    (if (= 0 (length new-input))
        nil
        new-input)))

;;
;  Let the user select a new KST font
;;
(defun new-font (dp)
  (makewinlistener *userwindow*)
  (let ((file (select-file "Enter name of valid KST font file:")))
    (if (null file)
        (file-error)
        (let ((f (loadfont dp file)))
          (if (= 0 f)
              (file-error)
              (setf (paintstruct-font *paintstuff*) f))))))



;
;   Primitive save and load functions
;
;   Written by Skef
;
(defun load-raw-bitmap (filename &optional
                                 (file-x 0) (file-y 0)
                                 (file-width (paintstruct-pixels-x *paintstuff*))
                                 (x 0) (y 0)
                                 (width (paintstruct-pixels-x *paintstuff*))
                                 (height (paintstruct-pixels-y *paintstuff*)))
  (multiple-value-bind (gr sap size)
                       (sesameuser:subreadfile *sesport* filename)
    (cond ((= gr success)
           (putviewportrectangle
            (paintstruct-drawport *paintstuff*)
            *move-area-rop*
            x y width height
            (alien-sap sap)
            (ash size -1)
            (logand (ash (+ file-width 63) -4) (lognot 3))
            file-x
            file-y)
           (invalidatememory kernelport (lisp::%sp-make-fixnum sap) size))
          (t (format t "Couldn't read ~A." filename)
             (force-output)
             nil))))

(defun save-raw-bitmap (filename &optional
                                 (x 0) (y 0)
                                 (width (paintstruct-pixels-x *paintstuff*))
                                 (height (paintstruct-pixels-y *paintstuff*)))
  (multiple-value-bind (available sap size)
                       (getviewportrectangle
                        (paintstruct-drawport *paintstuff*)
                        x y width height
                        0 0)
    (cond ((and available
                (not (null size)))
           (sesameuser:subwritefile *sesport* filename (alien-sap sap)
                                    (ash size 1) 0)
           (invalidatememory kernelport (lisp::%sp-make-fixnum sap) (ash size 1)))
          (t (format t "Couldn't write the bits!")
             (force-output)
             nil))))

;
;  Load-file loads in a picture image
;

(defun load-file ()
  (let ((file (select-file "Enter name of PAINT image to load:")))
    (if (null file)
        (file-error)
        (load-raw-bitmap (namestring (truename file))))))

;
;  Save-file saves out the whole screen
;
(defun save-file ()
  (let ((file (select-file "Enter name of PAINT image to save to:")))
    (if (null file)
        (file-error)
        (save-raw-bitmap file))))

;
; load-arb-file  lets user choose the width
;
(defun load-arb-file ()
  (let ((file (select-file "Enter name of file to load from:")))
    (if (null file)
        (file-error)
         (let ((x (get-integer "How many pixels across to start (x) :" 0 1024))
               (y (get-integer "How many pixels down to start (y) :" 0 1024))
               (width (get-integer "How many pixels on a line (width) :" 0 1024)))
           (load-raw-bitmap (namestring (truename file)) x y width)
           (setwindowtitle  *userwindow* *blank-line*)))))


;
;   Save-arb-file lets the user select the width and where to store
(defun save-arb-file (dp)
  (let ((file (select-file "Enter name of file to save to:")))
    (if (null file)
        (file-error)
        (let ((x (get-integer
                  "How many pixels across to start (x) (-1 to use mouse):"
                  -1 1024)))
          (cond ((= x -1)
                 (prepare-drawport *selcorner*)
                 (multiple-value-bind (x y width)
                                      (selectwindowregion
                                       (paintstruct-window *paintstuff*) dp)
                   (save-raw-bitmap file x y width)))
                (t (let ((y (get-integer "How many pixels down to start (y) :"
                                         0 1024))
                         (width (get-integer "How many pixels on a line (width) :"
                                             0 1024)))
                     (save-raw-bitmap file x y width))))))))

;
;  Press-file  Makes a press file from the paint screen
;
(defun press-file ()
  (let ((file (select-file "Enter name of press file to save image to:")))
    (if (null file)
        (file-error)
        (press:press-window file "paint" 0 0
                            (paintstruct-pixels-x *paintstuff*)
                            (paintstruct-pixels-y *paintstuff*)))))
;
;  Destroy-all-vp gets rid of all the viewports when we are done.
;
(defun destroy-all-vp ()
  (progn (destroyviewport (paintstruct-drawport *paintstuff*))
         (destroyviewport (paintstruct-off-view *paintstuff*))
         (destroyviewport (paintstruct-off-whole *paintstuff*))
         (destroyviewport (paintstruct-rot-view *paintstuff*))
         (deletewindow    (paintstruct-window *paintstuff*))
         (makunbound '*paintstuff*)))

;
;   Do-functions gets the users choice and branches away.
;
(defun do-functions ()
  (let ((dp (paintstruct-drawport *paintstuff*))
        (rop-x (+ 5 *menu-pos-x*))
        (rop-y 680))
    (loop
     (setwindowtitle (paintstruct-window *paintstuff*)
                     "Select Option From Menu")
     (let* ((msjump (format nil "~a" (paintstruct-grid-mod *paintstuff*)))
            (fvp (getsysfont (windowviewport *userwindow*)))
            (duy (vpputcharray dp fvp :rrpl rop-x rop-y
                               (concatenate 'string "Mouse Jump:"
                                            msjump " ")
                               (+ 12 (length msjump))
                               0 (+ 11 (length msjump))))
            (duy (vpputcharray dp fvp :rrpl rop-x (+ 15 rop-y)
                               (concatenate 'string "Draw ROP:"
                                            (case *current-draw-fun*
                                              (:DRAWLINE "Black ")
                                              (:ERASELINE "White ")
                                              (:XORLINE "Invert")))
                               15 0 14))
            (duy (vpputcharray dp fvp :rrpl rop-x (+ 30 rop-y)
                               (concatenate 'string
                                            "Move/CHR ROP:"
                                            (case *move-area-rop*
                                              (:RRPL "Replc")
                                              (:RNOT "Not  ")
                                              (:RAND "And  ")
                                              (:RANDNOT "AndNt")
                                              (:ROR "Or   ")
                                              (:ORNOT "OrNot")
                                              (:RXOR "XOr  ")
                                              (:RXORNOT "XNor ")))
                               18 0 17))
            (user-choice (if (pasted-menu-p "OPTIONS:")
                             (pasted-menu-choose "OPTIONS:")
                             (put-up-menu))))
       (declare (ignore duy))
       (case user-choice
         (1 (load-file))
         (2 (save-file))
         (24 (load-arb-file))
         (25 (save-arb-file dp))
         (26 (press-file))
         (3 (prepare-drawport
             "(L or R) Turn On Painting Mode  M --  Back to Menu")
            (painting dp))
         (4 (prepare-drawport
             "(L or R) -- Select New Brush    M -- Back to Menu")
            (select-brush dp))
         (5 (prepare-drawport
             "(L or R) -- Select New Pattern   M -- Back to Menu")
            (select-default dp))
         (6 (prepare-drawport
             "(L or R) -- Turn On Drawing Mode    M -- Back to Menu")
            (freehand dp))
         (7 (prepare-drawport "(L or R) -- Start Line    M -- Drawing Menu ")
            (make-line dp))
         (8 (prepare-drawport "(L or R) -- Center of Circle  M -- Back to Menu  ")
            (make-circle dp))
         (9 (prepare-drawport *selcorner*)
            (make-rect nil dp))
         (10 (prepare-drawport *selcorner*)
             (make-rect t dp))
         (11 (setq *current-draw-fun* (change-rop-menu)))
         (12 (prepare-drawport
              "(L or R)  --  Top corner of SQUARE to rotate    M -- Back to Menu")
             (rotate-window dp))
         (13 (prepare-drawport *selcorner*)
             (move-area t dp))
         (14 (prepare-drawport *selcorner*)
             (move-area nil dp))
         (15 (setq *move-area-rop* (change-move-rop)))
         (16 (prepare-drawport
              "(L or R) --  Move Entering Cursor    M -- Back to Menu")
             (enter-text dp))
         (19 (prepare-drawport " ")
             (mirror nil dp))
         (20 (prepare-drawport " ")
             (mirror t dp))
         (21 (prepare-drawport "Give function to apply to viewport")
             (makewinlistener *userwindow*)
             (let ((maybe (read)))
               (if t
                   (funcall (eval maybe) dp)
                   (setwindowtitle (paintstruct-window *paintstuff*)
                                   "Unknown Function")))
             (makewinlistener (paintstruct-window *paintstuff*)))
         (17 (prepare-drawport
              "(L or R) -- Select 64 x 64 area to edit   M -- Back to Menu")
             (fat-bits dp))
         (22 (jump-choose))
         (23 (new-font dp))
         (18 (when (confirm "Clear all these points (Yes or no)?")
               (blank)))
         (0  (vpputcharray dp fvp :rrpl rop-x rop-y
                           "              "
                           (+ 12 (length msjump))
                           0 (+ 11 (length msjump)))
             (vpputcharray dp fvp :rrpl rop-x (+ 15 rop-y)
                           "               "
                           15 0 14)
             (vpputcharray dp fvp :rrpl rop-x (+ 30 rop-y)
                           "                  " 18 0 17)
             (return nil))
         (-1 (cond ((confirm "Exit PAINT (and lose your image) (Yes or No)?")
                    (vpputcharray dp fvp :rrpl rop-x rop-y
                                  "              "
                                  (+ 12 (length msjump))
                                  0 (+ 11 (length msjump)))
                    (vpputcharray dp fvp :rrpl rop-x (+ 15 rop-y)
                                  "               "
                                  15 0 14)
                    (vpputcharray dp fvp :rrpl rop-x (+ 30 rop-y)
                                  "                  " 18 0 17)
                    (destroy-all-vp)
                    (return nil))
                   (t (makewinlistener (paintstruct-window *paintstuff*))))))))))

;   Make-win sets up the PAINT windows; this is only called ONCE per lisp!
;   The window is saved when you get out of PAINT.
;
(defun make-win ()
  (let* ((wind (createwindow (getfullwindow *userwindow*)
                             nil 0 0 nil
                             (paintstruct-screen-x *paintstuff*)
                             (paintstruct-screen-y *paintstuff*)
                             t t
                             "PerqPaint "
                             "PAINT" t))
         (vp (windowviewport wind)))
    (setf (paintstruct-window *paintstuff*) wind)
    (setf (paintstruct-drawport *paintstuff*) vp) ;actual drawing area
    (setf (paintstruct-off-view *paintstuff*)  ;used for work space
          (makeviewport vp -32002 -32002
                        (paintstruct-pixels-x *paintstuff*)
                        (paintstruct-pixels-y *paintstuff*)
                        0 t nil t))
    (setf (paintstruct-off-whole *paintstuff*)  ;used for work space
          (makeviewport vp -32002 -32002
                        (paintstruct-pixels-x *paintstuff*)
                        (paintstruct-pixels-y *paintstuff*)
                        0 t nil t))
    (setf (paintstruct-rot-view *paintstuff*)  ;used for rotation
          (makeviewport vp -32002 -32002
                        512 512
                        0 t nil t))
    (let ((big (make-array (* 64 64) :element-type '(mod 2)))
          (bot-patt (+ *patt-spacer* *top-patt*)))
      (show-pattern (patt1) big 3 *top-patt*)
      (show-pattern (bh) big 3 bot-patt)
      (show-pattern (half-tone) big *patt-spacer* *top-patt*)
      (show-pattern (patt2) big *patt-spacer* bot-patt)
      (show-pattern (patt3) big (* 2 *patt-spacer*) *top-patt*)
      (show-pattern (patt4) big (* 2 *patt-spacer*) bot-patt)
      (show-pattern (patt5) big (* 3 *patt-spacer*) *top-patt*)
      (show-pattern (patt6) big (* 3 *patt-spacer*) bot-patt)
      (show-pattern (patt7) big (* 4 *patt-spacer*) *top-patt*)
      (show-pattern (patt8 ) big (* 4 *patt-spacer*) bot-patt)
      (show-pattern (patt9 ) big (* 5 *patt-spacer*) *top-patt*)
      (show-pattern (patt10) big (* 5 *patt-spacer*) bot-patt)
      (show-pattern (patt11) big (* 6 *patt-spacer*) *top-patt*)
      (show-pattern (patt12) big (* 6 *patt-spacer*) bot-patt)
      (show-pattern (patt13) big (* 7 *patt-spacer*) *top-patt*)
      (show-pattern (patt14) big (* 7 *patt-spacer*) bot-patt)
      ; Draw current pattern
      (draw-current vp (* 6 *patt-spacer*) *top-patt* t))
    ;
    ; Set up brush parameters
    ;
    (let* ((zedx (paintstruct-pixels-x *paintstuff*))
           (zedy (paintstruct-pixels-y *paintstuff*))
           (spacer 5)
           (*brushes-x1* (+ zedx 5)))


      (setq *brush-64y* (+ zedy spacer))
      (setq *brush-32y* (+ zedy (* 2 spacer) 64))
      (setq *brush-16y* (+ zedy (* 3 spacer) 96))
      (setq *brush-8y* (+ zedy (* 4 spacer) 112))

      (setq *brush-64x* *brushes-x1*)
      (setq *brush-32x* (+ *brushes-x1* 16))
      (setq *brush-16x* (+ *brushes-x1* 24))
      (setq *brush-8x*  (+ *brushes-x1* 28)))))


;
;  Put patterns and brushes up
;
(defun init-screen (vp)
  ;draw patterns
  ;draw a borderline (vertical)
  (vpline vp :drawline
          (paintstruct-pixels-x *paintstuff*) 0
          (paintstruct-pixels-x *paintstuff*)
          (paintstruct-screen-y *paintstuff*))
  ;draw a borderline (horizontal)
  (vpline  vp :drawline
           0 (paintstruct-pixels-y *paintstuff*)
           (paintstruct-screen-x *paintstuff*)
           (paintstruct-pixels-y *paintstuff*))
  ;draw a line to seperate menus from displayed RASTOP info
  (vpline  vp :drawline
           (1+ (paintstruct-pixels-x *paintstuff*))
           660
           (paintstruct-screen-x *paintstuff*)
           660)
  ; put up brushes
  (show-brushes vp))

;;
;;    Top level call for PAINT
;;


(export 'paint)

(defun paint ()
  (let ((v1 0)
        (v2 0)
        (v3 0)
        (v4 0))
    (unwind-protect
      (progn
       (cond ((boundp '*paintstuff*)                           ;already made
              (restorewindow
               (paintstruct-window *paintstuff*))               ;bring it back
              (modifywindow     (paintstruct-window *paintstuff*) 0 0
                                (paintstruct-screen-x *paintstuff*)
                                (paintstruct-screen-y *paintstuff*) 0)
              (putviewportrectangle (paintstruct-drawport *paintstuff*)
                                    :RRPL 0 0
                                    (paintstruct-pixels-x *paintstuff*)
                                    (paintstruct-screen-y *paintstuff*)
                                    (paintstruct-image *paintstuff*)
                                    (* (paintstruct-pixels-x *paintstuff*)
                                       (paintstruct-screen-y *paintstuff*))
                                    40 0 0))
             (t
              ;fresh run of paint
              (setq *paintstuff* (make-paintstruct
                                  :font (getsysfont (windowviewport *userwindow*))
                                  :pixels-x 592
                                  :pixels-y 720
                                  :screen-x (+ 592 175)
                                  :screen-y (+ 720 176)
                                  :grid-mod 1))
              (setq *current-draw-fun* :drawline)
              (setq *move-area-rop* :rrpl)
              (setq *top-patt* (+ 4 (paintstruct-pixels-y *paintstuff*)))

              (make-win)
                      ;default brushe sizes

              (setq *brush-w* 32)
              (setq *brush-h* 32)

              ; default brush
              (setq *brush-x* (+ 80 *brush-32x*)  *brush-y* *brush-32y*)))

       (setwindowtitle (paintstruct-window *paintstuff*)
                       "PerqPaint ... a new design in painting")
       (multiple-value-setq (v1 v2 v3 v4) (fullwindowstate *userwindow*))
       (modifywindow *userwindow* 0 896 765 55 0)
       (setwindowtitle *userwindow* *blank-line*)
       (init-screen (paintstruct-drawport *paintstuff*))
       (enablewinlistener (paintstruct-window *paintstuff*) accint:nullport
                          (namestring (truename "lisp.keytran")) 0)
       (enableinput (paintstruct-drawport *paintstuff*)
                    (namestring (truename "lisp.keytran")) 0)
       (setq *last-grid-x* 0 *last-grid-y* 0)
       (do-functions))
      (unpaste-menu "OPTIONS:")
      (setwindowtitle *userwindow* (concatenate 'string (lisp-implementation-type)
                                                " " (lisp-implementation-version)))
      (when (boundp '*paintstuff*)
        (multiple-value-bind (available sap)
                             (getviewportrectangle
                              (paintstruct-drawport *paintstuff*)
                              0 0
                              (paintstruct-pixels-x *paintstuff*)
                              (paintstruct-screen-y *paintstuff*)
                              0 0)
          (when (not available)
            (format t "WOOPS!  Image not saved, sorry! ~%~%"))
          (setf (paintstruct-image *paintstuff*) (alien-sap sap)))
        (removewindow (paintstruct-window *paintstuff*)))
      (modifywindow *userwindow* v1 v2 v3 v4 0)
      (makewinlistener *userwindow*)
      ;return t
      t)))


;;
;;  Low level; and other picture making stuff.
;;
;;  'paintpic.slisp'
;;


;
;  Blanks the screen,  paint  viewport unless you specify otherwise.
;
(defun blank (&optional (port (paintstruct-drawport *paintstuff*)))
  (vpcolorrect port :rectwhite 0 0
                   (paintstruct-pixels-x *paintstuff*)
                   (paintstruct-pixels-y *paintstuff*)))


;
; for lines
;
(defun make-line (dp)
  (loop
   (setwindowtitle  (paintstruct-window *paintstuff*)
                    "(L or R) -- Start Line    M -- Drawing Menu ")
   (multiple-value-bind (a char c d) (get-points nil dp)
     (declare (ignore char))
     (cond ((= a MIDDLEDOWN)
            (setq *last-grid-x* d *last-grid-y* c)
            (return nil))
           ((left-or-right a)
            (setwindowtitle  (paintstruct-window *paintstuff*)
                             "(L or R) -- End Line     M -- Abort Line")
            (let ((firstx d)
                  (firsty c)
                  (lastx d)
                  (lasty c))
              (vpline dp :xorline firstx firsty firstx firsty)
              (loop
               (multiple-value-bind (a char c d) (get-points t dp)
                 (declare (ignore char))
                 (when (or (/= lastx d)
                           (/= lasty c))
                   (vpline dp :xorline firstx firsty lastx lasty)
                   (vpline dp :xorline firstx firsty d c)
                   (setq lastx d lasty c))
                 (cond ((= a MIDDLEDOWN)
                        (vpline dp :xorline firstx firsty d c)
                        (return nil))
                       ((left-or-right a)
                        (vpline dp :xorline firstx firsty d c)
                        (vpline dp *current-draw-fun* firstx firsty d c)
                        (return nil)))))))))))


;
;   Freehand lets user draw 'pen' style
;

(defun freehand (dp)
  (let ((fresh t)
        (prevx 0)
        (prevy 0))
    (loop
     (setwindowtitle (paintstruct-window *paintstuff*)
                     "(L or R) -- Turn On Drawing Mode    M -- Back to Menu")
     (multiple-value-bind (a char c d) (get-points nil dp)
       (declare (ignore char))
       (cond ((= a MIDDLEDOWN)
              (setq *last-grid-x* d *last-grid-y* c)
              (return nil))
             ((left-or-right a)
              (setwindowtitle (paintstruct-window *paintstuff*)
                              "M --  Turn Off Drawing Mode")
              (loop
               (multiple-value-bind (a char c d) (get-points t dp)
                 (declare (ignore char))
                 (cond ((= a MIDDLEDOWN)
                        (setq fresh t)
                        (setq *last-grid-x* d *last-grid-y* c)
                        (return nil))
                       (t
                        (if fresh
                            (vpline dp *current-draw-fun* d c d c)
                            (vpline dp *current-draw-fun* prevx prevy d c))
                        (setq prevx d prevy c)
                        (setq fresh nil))
                       (t (setq fresh t)))))))))))


;
;   Make-rect lets the user make rectangles
;

(defun make-rect (fill-p dp)  ;filled rectangle or not?
  (loop
   (multiple-value-bind (xtop ytop wid hgt)
                        (selectwindowregion (paintstruct-window *paintstuff*) dp)
     (when (null xtop)
       (return nil))
     (cond (fill-p
            (vpcolorrect dp (case *current-draw-fun*
                                (:drawline :rectblack)
                                (:eraseline :rectwhite)
                                (:xorline :rectinvert))
                           xtop ytop wid hgt))
           (t
            (vpline dp *current-draw-fun* xtop ytop (+ -1 xtop wid) ytop)
            (vpline dp *current-draw-fun* xtop ytop xtop (+ -1 ytop hgt))
            (vpline dp *current-draw-fun* xtop (+ -1 ytop hgt) (+ -1 xtop wid)
                      (+ -1 ytop hgt))
            (vpline dp *current-draw-fun* (+ -1 xtop wid) ytop (+ -1 xtop wid)
                      (+ -1 ytop hgt))
            (setq *last-grid-x* xtop *last-grid-y* ytop))))))

(defmacro sqr (num)
  `(* ,num ,num))
;
;   Make-circle lets the user make circles
;

(defun make-circle (dp)
  (let ((rightmax (paintstruct-pixels-x *paintstuff*))
        (bottmax  (paintstruct-pixels-y *paintstuff*)))
    (loop
     (setwindowtitle (paintstruct-window *paintstuff*)
                     "(L or R) -- Center of Circle  M -- Back to Menu  ")
     (multiple-value-bind (a char c d) (get-points nil dp)
       (declare (ignore char))
       (cond ((= a MIDDLEDOWN)
              (setq *last-grid-x* d *last-grid-y* c)
              (return nil))
             ((left-or-right a)
              (vpline dp :xorline d c d c)
              (setwindowtitle  (paintstruct-window *paintstuff*)
                               "(L or R) -- Radius       M -- Abort Circle")
              (let ((firstx d)
                    (firsty c))
                (loop
                 (multiple-value-bind (a char c d) (get-points t dp)
                   (declare (ignore char))
                   (let ((rad (round (sqrt (+ (sqr (- d firstx))
                                              (sqr (- c firsty)))))))
                     (cond ((= a MIDDLEDOWN)
                            (vpline dp :xorline firstx firsty firstx firsty)
                            (return nil))
                           ((left-or-right a)
                            (draw-circle firstx firsty
                                         rad
                                         (case *current-draw-fun*
                                           (:drawline :rectblack)
                                           (:eraseline :rectwhite)
                                           (:xorline :rectinvert)) dp)
                            (vpline dp :xorline firstx firsty firstx firsty)
                            (return nil))
                           (t (vpline dp :xorline
                                      (max 0 (- firstx rad )) firsty
                                      (min rightmax (+ firstx rad))
                                      firsty)
                              (vpline dp :xorline
                                      (max 0 (- firstx rad )) firsty
                                      (min rightmax (+ firstx rad))
                                      firsty)
                              (vpline dp :xorline
                                      firstx (max 0 (- firsty rad))
                                      firstx
                                      (min bottmax (+ firsty rad)))
                              (vpline dp :xorline
                                      firstx (max 0 (- firsty rad))
                                      firstx (min bottmax (+ firsty rad))
                                      )))))))))))))



;
;   Move-area is for moves and copies.  If delete-p is true, then it is
;   moving (i.e. delete what WAS there).
;
(defun move-area (delete-p dp)
  (let ((win (paintstruct-window *paintstuff*))
        (off (paintstruct-off-view *paintstuff*))
        (whole (paintstruct-off-whole *paintstuff*))
        (sizx (paintstruct-pixels-x *paintstuff*))
        (sizy (paintstruct-pixels-y *paintstuff*)))
    (loop
     (multiple-value-bind (xtop ytop width height) (selectwindowregion win dp)
       (when (null xtop)
         (return))
       (setcursorpos dp xtop ytop)
       (setwindowtitle win "(L or R) -- Select NEW Top Left  M -- Drawing Menu")
       (vprop off :rrpl 0 0 width                ;move area to move off screen
                 height dp xtop ytop)
       (when delete-p                              ;blank region if moving
         (vpcolorrect dp :rectwhite xtop ytop width height))
       (vprop whole :rrpl 0 0                    ;move whole picture off screen
                 sizx sizy
                 dp 0 0)
       (let ((movex 0)
             (movey 0)
             (ab 0))
         (loop
          (multiple-value-bind (a2 char c2 d2) (get-points t dp)
            (declare (ignore char))
            (let* ((maxw (- sizx d2))
                   (maxh (- sizy c2))
                   (mov-w (min maxw width))        ;border checks
                   (mov-h (min maxh height)))
              (cond  ((or (left-or-right a2)
                          (= a2 MIDDLEDOWN))
                      (multiple-value-setq (ab movex movey)
                        (values a2 d2 c2))
                      (return))
                     (t (vprop dp :rrpl 0 0 sizx sizy whole
                                  0 0)
                        (vprop dp *move-area-rop*
                                  d2 c2 mov-w mov-h off 0 0))))))
         (when (= ab MIDDLEDOWN)
           (setq *last-grid-x* movex *last-grid-y* movey)
           (return ab)))))))


;;
;;  Uses view-put-ch-array to put characters on the screen from the keybaord
;;
(defun enter-text (dp)
  (let* ((*paintfont* (paintstruct-font *paintstuff*)))
    (multiple-value-bind (ignore ignore ignore ignore wid hgt)
                         (fontsize *paintfont*)
      (declare (ignore ignore))
      (let ((maxright (- (paintstruct-pixels-x *paintstuff*) wid))
            (maxbott (- (paintstruct-pixels-y *paintstuff*) 1)))
        (multiple-value-bind (a b dy dx) (get-points t dp)
          (declare (ignore a b))
          (vpcolorrect dp :rectinvert dx (- dy hgt) wid hgt)
          (loop
           (multiple-value-bind (a b c d) (get-points t dp 0 hgt
                                        (- (paintstruct-pixels-x *paintstuff*) wid)
                                        (- (paintstruct-pixels-y *paintstuff*) 4))

             (cond ((= a MIDDLEDOWN)
                    (setq *last-grid-x* dx *last-grid-y* dy)
                    (vpcolorrect dp :rectinvert dx (- dy hgt) wid hgt)
                    (return nil))
                   ((left-or-right a)
                    (vpcolorrect dp :rectinvert dx (- dy hgt) wid hgt)
                    (setq dx d dy c)
                    (vpcolorrect dp :rectinvert dx (- dy hgt) wid hgt))
                   ((= a 0)
                    (let ((ch (string b)))
                      (vpcolorrect dp :rectinvert dx (- dy hgt) wid hgt)
                      (vpputcharray dp *paintfont* *move-area-rop*
                                      dx dy ch (length ch) 0 0)
                      (setq dx (+ dx (fontstringwidthvector *paintfont* ch 1 1)))
                      (when (>= dx maxright)
                        (setq dx 0)
                        (setq dy (+ dy hgt)))
                      (when (>= dy maxbott)
                        (setq dy hgt)
                        (setq dx 0))
                      (vpcolorrect dp :rectinvert  dx (- dy hgt) wid hgt)))))))))))


;;
;;
;;   FAT BITS STUFF
;;
;;


;  Draw-grid now does dots instead of lines.
(defun draw-grid (strtx strty wid hgt siz
                        &optional (port (paintstruct-drawport *paintstuff*)))
  (let ((*off-whole* (paintstruct-off-view *paintstuff*)))
    (blank *off-whole*)
    (dotimes (i (1+ wid))
      (vpcolorrect *off-whole* :rectblack
                       (* siz i) 0 1 1))
    (dotimes (i (1+ hgt))
      (vprop port :rrpl (* siz strtx) (* siz (+ i strty)) (* siz (1+ wid)) 1
                *off-whole* 0 0))))



(defun fat-bits (dp)
  (let* ((win (paintstruct-window *paintstuff*))
         (off (paintstruct-off-whole *paintstuff*))
         (off2 (paintstruct-off-view *paintstuff*))
         (maxx (paintstruct-pixels-x *paintstuff*))
         (maxy (paintstruct-pixels-y *paintstuff*))
         (bit-size 9)
         (move-size 64)
         (fat-size (* bit-size move-size))
         (bot-size (- maxy fat-size))
         (oldd -10000)
         (oldc -10000)
         (hold (paintstruct-grid-mod *paintstuff*)));horrible hack to save gridmod
    (setf (paintstruct-grid-mod *paintstuff*) 1)    ; make grid mod 1
    (loop
     (multiple-value-bind (a char c d) (get-points t dp
                                                0 0
                                                (- maxx move-size 1)
                                                (- maxy move-size 1))
       (declare (ignore char))
       (when (= a MIDDLEDOWN)
         (half-flash (1- oldd)  (1- oldc)
                     (+ oldd move-size) (+ oldc move-size) dp)
         (setf (paintstruct-grid-mod *paintstuff*) hold)    ; set it back
         (return nil))
       (when (or (/= d oldd)
                 (/= c oldc))
         (half-flash (1- oldd)  (1- oldc)
                     (+ oldd move-size) (+ oldc move-size) dp)
         (setq oldd d oldc c)
         (half-flash (1- d)  (1- c) (+ d move-size) (+ c move-size) dp))

       (when (left-or-right a)

         ;save where cursor is
         (setq *last-grid-x* d *last-grid-y* c)

         (setwindowtitle win "L  --  White    M -- Select New Region   R -- Black")

         ;store picture
         (vprop off :rrpl 0 0 maxx maxy dp 0 0)

         ;box area
         (half-flash (1- d)  (1- c) (+ move-size d) (+ move-size c) dp)

         (let* ((origx d)
                (origy c)
                (coffs (min (- maxy bot-size)
                            (max 0 (- c (floor bot-size 2)))))
                (newy (+ fat-size (- c coffs))))

           ;put area at bottom
           (vprop dp :rrpl 0 fat-size maxx bot-size dp 0 coffs)

           ;clear top part
           (vpcolorrect dp :rectwhite 0 0 maxx fat-size)

           ;put bits up
           (magnify bit-size move-size 0 0 origx newy dp off2)
           (draw-grid 0 0 (1- move-size) (1- move-size)  bit-size)
           (vpline dp :drawline 0 fat-size (- fat-size bit-size) fat-size)
           (vpline dp :drawline (- fat-size bit-size) 0 (- fat-size bit-size) fat-size)

           (setcursorpos dp 100 100)
           (loop
            (multiple-value-bind (a char c d) (get-points nil dp
                                                       0 0 (- fat-size bit-size)
                                                       fat-size)
              (declare (ignore char))
              (when (= a MIDDLEDOWN)
                (setcursorpos dp *last-grid-x* *last-grid-y*)
                (setwindowtitle win
                                "(L or R) -- Select 64 x 64 area to edit   M -- Back to Menu")
                (return nil))

              (when (left-or-right a)
                (let ((point (if (= a LEFTDOWN)
                                 :rectwhite :rectblack)))
                  (vpcolorrect dp point
                                 (1+ (constrain d bit-size))
                                 (1+ (constrain c bit-size))
                                 (- bit-size 2) (- bit-size 2))
                  (vpcolorrect dp point
                                 (+ origx (floor d bit-size))
                                 (+ newy (floor c bit-size))
                                 1 1)))))
           ;put back picture
           (vprop off :rrpl 0 (1+ coffs) maxx (1- bot-size) dp 0 (1+ fat-size))
           (vprop dp :rrpl  0 0 maxx maxy off 0 0)
           (half-flash (1- origx) (1- origy)
                       (+ move-size origx) (+ move-size origy) dp)))))))

(defun magnify (size move-size newx newy x y port temp)
  (blank temp)
  (dotimes (i move-size)
    (vprop temp :rrpl (1+ (* size i)) 0 1 move-size port (+ i x) y))
  (dotimes (i (- size 3))
    (vprop temp :ror (1+ i) 0
              (* move-size size)
              move-size temp
              i 0))
  (dotimes (i move-size)
    (vprop port :rrpl newx (+ 1 newy (* size i)) (* (1- move-size) size) 1 temp
              newx (+ i newy)))
  (dotimes (i (- size 3))
    (vprop port :ror newx (+ newy 1 i)
              (* (1- move-size) size)
              (* (1- move-size) size)
              port newx (+ newy i))))

;;   Mirroring and rotation

;
;
;  Mirror flips an image about the vertical or horizontal axis
(defun mirror (v-or-h dp)  ;nil=v t=h
  (let ((off (paintstruct-off-view *paintstuff*)))
    (blank off)
    (loop
     (multiple-value-bind (xtop ytop wid hgt)
                          (selectwindowregion (paintstruct-window *paintstuff*) dp)
       (when (null xtop)
         (return nil))
       (cond (v-or-h
              (dotimes (i hgt)
                (vprop off :rrpl 0 (- hgt i) wid 1
                         dp (1+ xtop) (+ -1 i ytop)))
              (vprop dp *move-area-rop* xtop ytop wid hgt off 0 1))
             (t
              (dotimes (i wid)
                (vprop off :rrpl (- wid i) 0 1 hgt
                         dp (+ -1 i xtop) (1+ ytop)))
              (vprop dp *move-area-rop* xtop ytop wid hgt off 1 0)))))))

;
;   Rotate-window gets a region and does a 90 degree flip (counterclockwise).
;

(defun rotate-window (dp)
  (let ((win (paintstruct-window *paintstuff*))
        (sizx (paintstruct-pixels-x *paintstuff*))
        (sizy (paintstruct-pixels-y *paintstuff*)))
    (loop
     (setwindowtitle win
              "(L or R) - Top corner of SQUARE to rotate    M -- Back to Menu")
     (multiple-value-bind (a char ytop xtop ) (get-points nil dp)
       (declare (ignore char))
       (when (= a MIDDLEDOWN)
         (return nil))
       (when (left-or-right a)
         (setwindowtitle win
                         "(L or R) - Bottom of SQUARE to rotate     M -- Abort Square")
         (loop
          (multiple-value-bind (a char ybot xbot) (get-points t dp)
            (declare (ignore char))
            (cond ((< xbot xtop)
                   (setq xbot xtop))
                  ((< ybot ytop)
                   (setq ybot ytop)))
            (let* ((maxw (- sizx xtop))
                   (maxh (- sizy ytop))
                   (wid (- xbot xtop))
                   (hgt (- ybot ytop))
                   (sq  (min 512 (min maxh maxw (max wid hgt))))  ;max siz 512x512
                   (botx (+ xtop sq))
                   (boty (+ ytop sq)))
              (flash-area xtop ytop botx boty dp)
              (when (= a MIDDLEDOWN)
                (return nil))
              (when (left-or-right a)
                (rotate-wind xtop ytop sq)
                (return nil))))))))))



(defmacro copy-all-to (from xoff yoff to func)
  `(vprop ,to ,func ,xoff ,yoff
             (- size-of ,xoff) (- size-of ,yoff)
             ,from 0 0))

(defmacro copy-all-from (to xoff yoff from func)
  `(vprop ,to ,func 0 0 (- size-of ,xoff) (- size-of ,yoff)
             ,from ,xoff ,yoff))

(defun next-2-power (number)
  (do ((pow 1 (* 2 pow)))
      ((<= number pow) pow)))

(defun rotate-wind (x y width)
  (let* ((dp (paintstruct-drawport *paintstuff*))
         (mask (paintstruct-off-view *paintstuff*))
         (temp (paintstruct-off-whole *paintstuff*))
         (rot-port (paintstruct-rot-view *paintstuff*))
         (size-of (next-2-power width)))
    (blank mask)
    (blank temp) ;clear the offscreen viewports
    (vpcolorrect mask :rrpl 0 0
                   (/ size-of 2) (/ size-of 2)) ;make the first mask

    ;move picture to temp
    ;temp will be worked on; dp will be
    ;for storage
    (vprop temp :rrpl 0 0 width width dp x y)

    (do ((part (floor size-of 2) (floor part 2)))
        ((< part (paintstruct-grid-mod *paintstuff*)))
      (copy-all-to mask 0 0 rot-port :rrpl)             ;0
      (copy-all-to mask 0 part rot-port :ror)           ;4
      (copy-all-to temp 0 0 rot-port :rand)             ;2
      (copy-all-to rot-port 0 0 temp :rxor)             ;6
      (copy-all-from rot-port part 0 temp :rxor)        ;6
      (copy-all-from temp part 0 temp :ror)             ;4
      (copy-all-to rot-port part 0 temp :rxor)          ;6
      (copy-all-to temp 0 0 rot-port :rrpl)             ;0
      (copy-all-from rot-port part part temp :rxor)     ;6
      (copy-all-to mask 0 0 rot-port :rand)             ;2
      (copy-all-to rot-port 0 0 temp :rxor)             ;6
      (copy-all-to rot-port part part temp :rxor)       ;6
      (copy-all-from mask (floor part 2) (floor part 2) mask :rand)  ;2
      (copy-all-to mask part 0 mask :ror)                            ;4
      (copy-all-to mask 0 part mask :ror)                            ;4
      (vprop dp :rrpl x y width width temp (- size-of width) 0))))
    ;put the picture back


;;; Circle drawing stuff.
;;; Written by Skef Wholey.
;;;


;;; Draw-Circle is actually slower than calling Draw-Filled-In-Circle twice,
;;; so you might not want to use it at all.
;
; but: calling draw-circle twice whites out the center of the circle;
; not necessarily desirable!
;

(defmacro draw-point (vp function x y)
  `(when (and (< ,x maxr)
              (< ,y maxl))
     (vpcolorrect ,vp ,function ,x ,y 1 1)))

(defun draw-circle (center-x center-y radius
                             &optional (function :rectblack)
                             (vp (sapphuser:windowviewport *userwindow*)))
  (do ((y 0)
       (f 0)
       (x radius)
       (maxr (paintstruct-pixels-x *paintstuff*))
       (maxl (paintstruct-pixels-y *paintstuff*)))
      (())
    (draw-point vp function (+ center-x x) (- center-y y))
    (draw-point vp function (- center-x x) (+ center-y y))
    (draw-point vp function (+ center-x y) (+ center-y x))
    (draw-point vp function (- center-x y) (- center-y x))
    (setq f (+ f y y 1) y (1+ y))
    (if (>= f x) (setq f (- f x x -1) x (- x 1)))
    (if (> y x) (return nil))
    (draw-point vp function (+ center-x x) (+ center-y y))
    (draw-point vp function (- center-x x) (- center-y y))
    (draw-point vp function (+ center-x y) (- center-y x))
    (draw-point vp function (- center-x y) (+ center-y x))
    (if (= y x) (return nil))))

(defmacro draw-horizontal-line (vp function x y width)
  `(vpcolorrect ,vp ,function ,x ,y ,width 1 ))

(defun draw-filled-in-circle (center-x center-y radius
                              &optional (function :rectblack)
                                   (vp (sapphuser:windowviewport *userwindow*)))

  (do ((y 0)
       (f 0)
       (x radius)
       (last-top-x 0)
       (last-bottom-x 0))
      (())
    (draw-horizontal-line vp function (- center-x x) (+ center-y y) (+ x x))
    (when (/= x last-top-x)
      (draw-horizontal-line vp function (- center-x y) (- center-y x) (+ y y))
      (setq last-top-x x))
    (setq f (+ f y y 1) y (1+ y))
    (if (>= f x) (setq f (- f x x -1) x (- x 1)))
    (if (> y x) (return nil))
    (draw-horizontal-line vp function (- center-x x) (- center-y y) (+ x x))
    (when (/= x last-bottom-x)
      (draw-horizontal-line vp function (- center-x y) (+ center-y x) (+ y y))
      (setq last-bottom-x x))
    (if (= y x) (return nil))))



;;
;;   The painting with patterns stuff for PAINT
;;
;
;    'paintpatt.slisp'
;

;  Here are 14 patterns to start the user off with:
;





(defun patt1 ()
  (make-array (* 8 8)
              :element-type '(mod 2)
              :initial-contents
              '(1 1 1 1 1 0 0 0
                0 1 1 1 0 1 0 0
                0 0 1 0 0 0 1 0
                0 1 0 0 0 1 1 1
                1 0 0 0 1 1 1 1
                0 0 0 1 0 1 1 1
                0 0 1 0 0 0 1 0
                0 1 1 1 0 0 0 1)))

(defun patt2 ()
  (make-array (* 8 8)
              :element-type '(mod 2)
              :initial-contents
              '(0 0 0 0 1 0 0 0
                0 0 0 1 1 1 0 0
                0 0 1 0 0 0 1 0
                1 1 0 0 0 0 0 1
                1 0 0 0 0 0 0 0
                0 0 0 0 0 0 0 1
                0 0 0 0 0 0 1 0
                0 0 0 0 0 1 0 0)))

(defun patt3 ()
  (make-array (* 8 8)
              :element-type '(mod 2)
              :initial-contents
              '(0 1 0 1 0 1 0 1
                0 1 0 1 0 1 0 1
                0 1 0 1 0 1 0 1
                0 1 0 1 0 1 0 1
                0 1 0 1 0 1 0 1
                0 1 0 1 0 1 0 1
                0 1 0 1 0 1 0 1
                0 1 0 1 0 1 0 1)))


(defun patt4 ()
  (make-array (* 8 8)
              :element-type '(mod 2)
              :initial-contents
              '(1 1 1 1 1 1 1 1
                0 0 0 0 0 0 0 0
                1 1 1 1 1 1 1 1
                0 0 0 0 0 0 0 0
                1 1 1 1 1 1 1 1
                0 0 0 0 0 0 0 0
                1 1 1 1 1 1 1 1
                0 0 0 0 0 0 0 0)))

(defun patt5 ()
  (make-array (* 8 8)
              :element-type '(mod 2)
              :initial-contents
              '(1 1 1 0 1 1 1 0
                1 1 0 1 1 1 0 1
                1 0 1 1 1 0 1 1
                0 1 1 1 0 1 1 1
                1 1 1 0 1 1 1 0
                1 1 0 1 1 1 0 1
                1 0 1 1 1 0 1 1
                0 1 1 1 0 1 1 1)))

(defun patt6 ()
  (make-array (* 8 8)
              :element-type '(mod 2)
              :initial-contents
              '(0 0 0 1 0 0 0 1
                0 0 1 0 0 0 1 0
                0 1 0 0 0 1 0 0
                1 0 0 0 1 0 0 0
                0 0 0 1 0 0 0 1
                0 0 1 0 0 0 1 0
                0 1 0 0 0 1 0 0
                1 0 0 0 1 0 0 0)))


(defun patt7 ()
  (make-array (* 8 8)
              :element-type '(mod 2)
              :initial-contents
              '(0 1 1 1 0 1 1 1
                1 0 1 1 1 0 1 1
                1 1 0 1 1 1 0 1
                1 1 1 0 1 1 1 0
                0 1 1 1 0 1 1 1
                1 0 1 1 1 0 1 1
                1 1 0 1 1 1 0 1
                1 1 1 0 1 1 1 0)))
(defun patt8 ()
  (make-array (* 8 8)
              :element-type '(mod 2)
              :initial-contents
              '(1 0 0 0 1 0 0 0
                0 1 0 0 0 1 0 0
                0 0 1 0 0 0 1 0
                0 0 0 1 0 0 0 1
                1 0 0 0 1 0 0 0
                0 1 0 0 0 1 0 0
                0 0 1 0 0 0 1 0
                0 0 0 1 0 0 0 1)))

(defun patt9 ()
  (make-array (* 8 8)
              :element-type '(mod 2)
              :initial-contents
              '(1 1 1 1 1 1 1 0
                1 1 1 1 1 1 0 1
                1 1 1 1 1 0 1 1
                1 1 1 1 0 1 1 1
                1 1 1 0 1 1 1 1
                1 1 0 1 1 1 1 1
                1 0 1 1 1 1 1 1
                0 1 1 1 1 1 1 1)))

(defun patt10 ()
  (make-array (* 8 8)
              :element-type '(mod 2)
              :initial-contents
              '(0 0 0 0 0 0 0 1
                0 0 0 0 0 0 1 0
                0 0 0 0 0 1 0 0
                0 0 0 0 1 0 0 0
                0 0 0 1 0 0 0 0
                0 0 1 0 0 0 0 0
                0 1 0 0 0 0 0 0
                1 0 0 0 0 0 0 0)))
(defun patt11 ()
  (make-array (* 8 8)
              :element-type '(mod 2)
              :initial-contents
              '(0 1 1 1 1 1 1 1
                1 0 1 1 1 1 1 1
                1 1 0 1 1 1 1 1
                1 1 1 0 1 1 1 1
                1 1 1 1 0 1 1 1
                1 1 1 1 1 0 1 1
                1 1 1 1 1 1 0 1
                1 1 1 1 1 1 1 0)))
(defun patt12 ()
  (make-array (* 8 8)
              :element-type '(mod 2)
              :initial-contents
              '(1 0 0 0 0 0 0 0
                0 1 0 0 0 0 0 0
                0 0 1 0 0 0 0 0
                0 0 0 1 0 0 0 0
                0 0 0 0 1 0 0 0
                0 0 0 0 0 1 0 0
                0 0 0 0 0 0 1 0
                0 0 0 0 0 0 0 1)))

;;
;;   here are some extra patterns
(defun brick ()
  (make-array (* 8 8)
              :element-type '(mod 2)
              :initial-contents
              '(1 1 1 1 1 1 1 1
                0 0 0 0 0 0 0 1
                0 0 0 0 0 0 0 1
                1 1 1 1 1 1 1 1
                0 0 0 1 0 0 0 0
                0 0 0 1 0 0 0 0
                0 0 0 1 0 0 0 0
                0 0 0 1 0 0 0 0)))
(defun half-tone ()
  (make-array (* 8 8)
              :element-type '(mod 2)
              :initial-contents
              '(1 0 1 0 1 0 1 0
                0 1 0 1 0 1 0 1
                1 0 1 0 1 0 1 0
                0 1 0 1 0 1 0 1
                1 0 1 0 1 0 1 0
                0 1 0 1 0 1 0 1
                1 0 1 0 1 0 1 0
                0 1 0 1 0 1 0 1)))
(defun bh ()
  (make-array (* 8 8)
              :element-type '(mod 2)
              :initial-contents
              '(0 0 0 0 0 0 0 0
                0 0 0 1 1 0 0 0
                0 1 0 1 1 0 0 0
                0 0 1 1 1 0 0 0
                0 0 0 1 1 0 0 0
                0 0 0 1 1 0 0 0
                0 0 1 1 1 1 0 0
                0 0 0 0 0 0 0 0)))



(defun patt13 ()
  (make-array (* 8 8)
              :element-type '(mod 2)
              :initial-element 1))

(defun patt14 ()
  (make-array (* 8 8)
              :element-type '(mod 2)
              :initial-element 0))


(defun move-pattern (sa da xd yd)
  (dotimes (i 8)
    (dotimes (j 8)
      (setf (bit da (+ (+ i xd)
                       (* 64 (+ j yd))))
            (bit sa (+ i (* 8 j)))))))



(defun show-pattern (aray big x y)
  (let ((vp (paintstruct-drawport *paintstuff*)))
    (dotimes (i 8)
      (dotimes (j 8)
        (move-pattern aray big (* i 8) (* j 8))))
    (vpcolorrect vp :rectblack (1- x) (1- y) 66 66)
    (vpcolorrect vp :rectwhite x y 64 64)
    (putviewportrectangle
     vp :RRPL x y 64 64 big (* 64 64) 4 0 0)))

(defun draw-current (dp x y first-call-p)
  (progn
   (unless first-call-p
     ; unhighlight old pattern
     (vpcolorrect dp :rectinvert (- *pattern-x* 4) (- *pattern-y* 4) 72 72)
     (vpcolorrect dp :rectinvert *pattern-x* *pattern-y* 64 64))
   ; highlight new pattern
   (vpcolorrect dp :rectinvert (- x 4) (- y 4) 72 72)
   (vpcolorrect dp :rectinvert x y 64 64)
   (setq *pattern-x* x)
   (setq *pattern-y* y)))

(defun select-default (dp)
  (setcursorpos dp *pattern-x* *pattern-y*)
  (let ((hold (paintstruct-grid-mod *paintstuff*)));horrible hack to save gridmod
    (setf (paintstruct-grid-mod *paintstuff*) 1)   ; make grid mod 1
    (loop
     (multiple-value-bind (a char c d)
                          (get-points nil dp
                                      0 (paintstruct-pixels-y *paintstuff*)
                                      (paintstruct-pixels-x *paintstuff*)
                                      (paintstruct-screen-y *paintstuff*))
       (declare (ignore a char))
       (cond ((= a MIDDLEDOWN)
              (setf (paintstruct-grid-mod *paintstuff*) hold) ;restore grid mod
              ;set up offscreen area
              (return nil))
             ((and (> c (paintstruct-pixels-y *paintstuff*))

                   (< d (paintstruct-pixels-x *paintstuff*)))
              (let ((y (if (< c (+ *patt-spacer* *top-patt*))
                           *top-patt*
                           (+ *patt-spacer* *top-patt*)))
                    (x (max 3 (* *patt-spacer* (floor d *patt-spacer*)))))
                (draw-current dp x y nil))))))))


;
;   Picture stuff for PAINT
;


(defun painting (dp)
  (let ((off (paintstruct-off-view *paintstuff*)))
  ;set up offscreen area; must be done here because we use off-view for other stuff
   (vprop off :RRPL 0  0  64 64 dp *pattern-x* *pattern-y*)
   (vprop off :RRPL 0  64 64 64 dp *pattern-x* *pattern-y*)
   (vprop off :RRPL 64 0  64 64 dp *pattern-x* *pattern-y*)
   (vprop off :RRPL 64 64 64 64 dp *pattern-x* *pattern-y*)
    (loop
     (multiple-value-bind (a char c d) (get-points t dp)
       (declare (ignore char))
       (cond ((= a MIDDLEDOWN)
              (setq *last-grid-x* d *last-grid-y* c)
              (return nil))
             ((left-or-right a)
              (setwindowtitle (paintstruct-window *paintstuff*)
                              "M --  Turn Off Painting Mode")
              (loop
               (multiple-value-bind (a char c d) (get-points t dp)
                 (declare (ignore char))
                 (cond ((= a MIDDLEDOWN)
                        (setwindowtitle (paintstruct-window *paintstuff*)
                                        "(L or R) Turn On Painting Mode  M --  Back to Menu")
                        (return nil))
                       (t
                        (let ((width (min
                                      *brush-w*
                                      (- (paintstruct-pixels-x *paintstuff*) d 1)))
                              (height (min
                                       *brush-h*
                                       (- (paintstruct-pixels-y *paintstuff*)
                                          c 1))))
                          (when (< c (1- (paintstruct-pixels-y *paintstuff*)))
                            ;
                            ;
                            ; If you don't mind the extra slowness,
                            ; the next two lines show where
                            ;  the brush is on the screen.
                            ;
                            ;(vprop dp :rxor d c width height dp *brush-x* *brush-y*)
                            ;(vprop dp :rxor d c width height dp *brush-x* *brush-y*)

                            ; First cut a 'hole' in screen if replacing, replace-noting
                            (when (or (eq *move-area-rop* :RRPL)
                                      (eq *move-area-rop* :RNOT))
                              (vprop dp :RANDNOT d c width height dp *brush-x* *brush-y*))

                            ; Put the pattern (or it's inverse) offscreen
                            (if (or (eq *move-area-rop* :RRPL)
                                    (eq *move-area-rop* :RAND)
                                    (eq *move-area-rop* :ROR)
                                    (eq *move-area-rop* :RXOR))
                                (vprop off :RRPL 0 130 width height off
                                       (mod d 64)
                                       (mod c 64))
                                (vprop off :RNOT 0 130 width height off
                                       (mod d 64)
                                       (mod c 64)))

                            ; Cut out a border around the offscreen pattern; like the brush
                            (vprop off :RAND 0 130 width height dp *brush-x* *brush-y*)

             ;for anding, put a black area around pattern; since "1 AND x=x"
             (when (or (eq *move-area-rop* :RAND)
                       (eq *move-area-rop* :RANDNOT))
               (vpcolorrect off :rectblack 0 195 64 64)
               (vprop off :RANDNOT 0 195 width height dp *brush-x* *brush-y*)
               (vprop off :ROR 0 130 width height off 0 195))

             ;  The final step; put it back together

             (when (or (eq *move-area-rop* :RRPL)
                       (eq *move-area-rop* :RNOT)
                       (eq *move-area-rop* :ROR)
                       (eq *move-area-rop* :RORNOT))
               (vprop dp :ROR d c width height off 0 130))

             (when (or (eq *move-area-rop* :RXOR)
                       (eq *move-area-rop* :RXOR))
               (vprop dp :RXOR d c width height off 0 130))

             (when (or (eq *move-area-rop* :RAND)
                       (eq *move-area-rop* :RANDNOT))
               (vprop dp :RAND d c width height off 0 130))))))))))))))



(defun show-brushes (dp)
  ;four square brushes:
  (vpcolorrect dp :rectblack *brush-64x* *brush-64y* 64 64)
  (vpcolorrect dp :rectblack *brush-32x* *brush-32y* 32 32)
  (vpcolorrect dp :rectblack *brush-16x* *brush-16y* 16 16)
  (vpcolorrect dp :rectblack *brush-8x* *brush-8y* 8 8)
  ;four round brushes

  (draw-filled-in-circle (+ 80 *brush-64x* 32)
                         (+ *brush-64y* 32) 32 :rectblack dp)
  (draw-filled-in-circle (+ 80 *brush-32x* 16)
                         (+ *brush-32y* 16) 16 :rectblack dp)
  (draw-filled-in-circle (+ 80 *brush-16x* 8) (+ *brush-16y* 8) 8 :rectblack dp)
  (draw-filled-in-circle (+ 80 *brush-8x* 4) (+ *brush-8y* 4) 4 :rectblack dp)

  ; Highlight brush
  (vpcolorrect dp :rectinvert (- *brush-x* 3) (- *brush-y* 3)
               (+ *brush-w* 6) (+ *brush-h* 6))
  (vpcolorrect dp :rectinvert (1- *brush-x*) (1- *brush-y*)
               (+ 2 *brush-w*)  (+ 2 *brush-h*)))


(defun select-brush (dp)
  (let ((topx (paintstruct-pixels-x *paintstuff*))
        (topy (paintstruct-pixels-y *paintstuff*))
        (botx (paintstruct-screen-x *paintstuff*))
        (boty (paintstruct-screen-y *paintstuff*))
        (hold (paintstruct-grid-mod *paintstuff*)))
    (setf (paintstruct-grid-mod *paintstuff*) 1)
    (setcursorpos (paintstruct-drawport *paintstuff*) (+ topx 5) (+ *brush-y* 5))
    (loop
     (multiple-value-bind (a char c d)
                          (get-points nil dp
                                      topx topy botx boty)
       (declare (ignore char))
       (cond ((= a MIDDLEDOWN)
              (setf (paintstruct-grid-mod *paintstuff*) hold)
              (return nil))
             (t
              ;erase old lines
              (vpcolorrect dp :rectinvert (- *brush-x* 3) (- *brush-y* 3)
                               (+ *brush-w* 6) (+ *brush-h* 6))
              (vpcolorrect dp :rectinvert (1- *brush-x*) (1- *brush-y*)
                               (+ *brush-w* 2)  (+ *brush-h* 2))
              (cond ((< c *brush-32y*)
                     (setq *brush-h* 64 *brush-w* 64
                           *brush-x* *brush-64x* *brush-y* *brush-64y*))
                    ((< c *brush-16y*)
                     (setq *brush-h* 32 *brush-w* 32
                           *brush-x* *brush-32x* *brush-y* *brush-32y*))
                    ((< c *brush-8y*)
                     (setq *brush-h* 16 *brush-w* 16
                           *brush-x* *brush-16x* *brush-y* *brush-16y*))
                    (t
                     (setq *brush-h* 8 *brush-w* 8
                           *brush-x* *brush-8x* *brush-y* *brush-8y*)))
              (if (> d (+ 80 *brush-64x*))
                  (setq *brush-x* (+ 80 *brush-x*)))
              ;show new brush
              (vpcolorrect dp :rectinvert (- *brush-x* 3) (- *brush-y* 3)
                               (+ *brush-w* 6) (+ *brush-h* 6))
              (vpcolorrect dp :rectinvert (1- *brush-x*) (1- *brush-y*)
                               (+ *brush-w* 2) (+ *brush-h* 2))))))))


;
;  Menu stuff for PAINT
;
;  'paintmenu.slisp'
;

(defun put-up-menu ()
  (menu-choose :items
               (("Load Screen" :value 1
                 :help "Select file PAINT picture is stored in.")
                ("Save Screen" :value 2
                 :help "Select file to store picture PAINT in.")
                ("Load Bitmap" :value 24
                 :help "Load any type of bitmap")
                ("Save Bitmap" :value 25
                 :help "Select area to store.")
                ("Make Pressfile" :value 26
                 :help "Send screen to pressfile for dover")
                ("   " :no-select)
                ("Paint" :value 3 :help "Paint with brush.")
                ("Change Brush  " :value 4 :help "Select new brush to paint with.")
                ("Change Pattern" :value 5
                 :help "Select new pattern to paint with.")
                ("  " :no-select)
                ("Draw Freehand" :value 6 :help "Move a pen and draw.")
                ("Draw Line"     :value 7 :help "Connect two dots together.")
                ("Draw Circle"   :value 8 :help "Choose center and radius.")
                ("Lined Rectgle" :value 9
                 :help "Choose opposite corners; connects with lines.")
                ("Solid Rectgle" :value 10
                 :help "Choose opposite corners; region filled in.")
                ("  " :no-select)
                ("Move Area" :value 13
                 :help "Mark an area, then move it.  Original goes away.")
                ("Copy Area" :value 14
                 :help "Mark an area; then move it.  Original is unchanged")
                ("Characters" :value 16 :help "Enter characters from keyboard.")
                ("Char Font" :value 23
                 :help "Select a new KST font for characters.")
                ("Rotate Area" :value 12
                 :help "Rotate an area clockwise by 90 degrees.")
                ("Mirror Vert" :value 19 :help "Mirror a region vertically")
                ("Mirror Horz" :value 20
                 :help "Mirror a region horizontally")
                ("  " :no-select)
                ("Pick TRI ROP" :value 11
                 :help "Select RastOP for anything that draws, erases, or inverts.")
                ("Pick OCTO ROP" :value 15
                 :help "Pick the RastOP for anything that can do all eight possible raster ops.")
                ("  " :no-select)
                ("Clear Screen" :value 18 :help "Clear entire screen.")
                ("Fat Bits" :value 17 :help "Point edit a 64X64 area.")
                ("Mouse Jump" :value 22 :help "Make mouse jump mod <N>")
                ("Apply" :value 21 :help "Apply a function to the viewport")
                ("  " :no-select)
                ("LISP" :value 0 :help "Go back to lisp; everything saved.")
                ("Exit Paint" :value -1 :help "Leave PAINT and DESTROY all information, and viewports."))
               :position-x *menu-pos-x*
               :position-y 0
               :title "OPTIONS:"
               :in-window (paintstruct-window *paintstuff*)
               :default-item "Paint"
               :paste-up :and-choose ))

(defun change-rop-menu ()
  (menu-choose :items
               (("White " :value :eraseline :help "White lines, circles, regions.")
                ("Black "   :value :drawline
                 :help "Black lines, circles, regions.")
                ("Invert" :value :xorline
                 :help "Inverse lines, circles, regions."))
                :position-x *menu-pos-x*
                :position-y *menu-other-pos-y*
                :in-window (paintstruct-window *paintstuff*)
                :title "Draw Rastop:"))


(defun change-move-rop ()
  (menu-choose :items
               (("Repl" :value :rrpl :help "Replace current area with move area.")
                ("Not"  :value :rnot
                 :help "Replace current area with invert of move area.")
                ("And"  :value :rand
                 :help "Replace current area with AND of current area and move area")
                ("And Not" :value :randnot
                 :help "Replace current area with AND of current area and invert of move area")
                ("Or" :value :ror
                 :help "Replace current area with OR of current area and move area")
                ("Or Not" :value :rornot
                 :help "Replace current area with OR of current area and invert of move area")
                ("XOr" :value :rxor
                 :help "Replace current area with XOR of current area and move area")
                ("XNor" :value :rxnor
                 :help "Replace current area with XOR of current area and invert of move area"))
               :position-x *menu-pos-x*
               :position-y *menu-other-pos-y*
               :title "Mov/Char ROP:"
               :default-item "Repl"
               :in-window (paintstruct-window *paintstuff*)))
