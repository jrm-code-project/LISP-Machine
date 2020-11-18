;;; -*- Mode:LISP; Package:USER; Base:10; Readtable:ZL -*-

#|

Here are examples of several of the most useful user interface (window
system) routines. Evaluate the forms in sequence (or M-X Eval Buffer) to get
an idea of what each menu type looks like.

|#

(defmacro pt-example(does &body body)
  (let((bodyl (caar body)))
  `(print
     (progn
       (format t "~%;;; ~a example: illustrates ~a~%"
               ',bodyl
               ,does)
       (sleep 2)
       . ,body))))

;;; TV:CHOOSE-USER-OPTIONS
;;; A poor man's popup window for global variables

(tv:define-user-option-alist myoptions)

(tv:defvar-user-option option1 "Testing"
  "Test variable" myoptions)
(tv:defvar-site-user-option option2 :sys-host
  "Defaults to sys host site variable value" myoptions)


(pt-example "pop-up window for setting global and site variables"
  (tv:choose-user-options myoptions :label "Set these global variables"))
(print option1)
(print option2)

;;; TV:MENU-CHOOSE
;;; Returns the choice, e.g., LISP. Second value returned is the full choice list.

(pt-example "pop-up window for returning one choice"
  (tv:menu-choose '(("C" c)
                    ("CommonLisp" lisp)
                    ("Prolog" prolog)
                    ("Fortran" f77))
                  "Pick your favorite language"
                  '(:mouse)
                  '("CommonLisp" lisp)))

;;; TV:MULTIPLE-MENU-CHOOSE

;; Compile these -- first time through can be v-e-r-y s-l-o-w...

;;Highlight/defaulting works in simple cases

(pt-example "simple multiple choice"
  (tv:multiple-menu-choose
    '(lisp
       prolog
       C
       f77)
    "Click on any"
    '(:point 100 100)
    '(lisp)))

;But when using an item label, be sure the default choice is EQ:

(let((choice '(" ZetaLISP" lisp)))
  (pt-example "multiple choice with item labels and default"
    (tv:multiple-menu-choose
      `(,choice
        (" Prolog" prolog)
        (" C compiler" C)
        (" F77 Fortran" f77))
      "Click on any"
      '(:mouse)
      (ncons choice))))

;;; TV:CHOOSE-VARIABLE-VALUES

;;This example illustrates a simple function for implementing
;;a constraint on a variable

(let ((dont-touch-this 'this)
      (lisp t)
      prolog f77 c other favorite)
  ;;Only special variables work with CHOOSE-VARIABLE-VALUES
  (declare(special dont-touch-this
                   lisp prolog f77 c other favorite))
  (pt-example "setting local//special variable values"
    (tv:choose-variable-values
      '("This is just a label."
        (dont-touch-this "Don't touch" :sexp)
        (lisp "Lisp" :CHOOSE (T nil))
        (prolog "LM-PROLOG":CHOOSE (T nil))
        (f77    "FORTRAN" :CHOOSE (T nil))
        (c      "C" :CHOOSE (T nil))
        (other "Other (enter one)" :string)
        (favorite :CHOOSE (lisp prolog f77 c)))
      :function #'(lambda(window var old new)
                    (if(equal var 'dont-touch-this)
                       (progn (set var old)
                              (send window :refresh))
                      nil)))
    `((lisp ,lisp)
      (prolog ,prolog)
      (f77 ,f77)
      (c ,c)
      (other ,other)
      (favorite ,favorite))))

;;; TV:MULTIPLE-CHOOSE

;;; This first example illustrates accumulation of non-exclusive
;;; choices. Note also the display of redundant choices -- choosing to
;;; read about a topic for "All" highlights all keywords; turning this off
;;; turns off all the individual choices.

(pt-example "complex multiple choice with choice interactions"
  (tv:multiple-choose "Document Language Features"
                      '((arithmetic "Full Arithmetic" (lisp prolog c pascal f77 all))
                        (lists "List Processing" (prolog lisp all))
                        (unix "Unix Libraries" (c f77 all))
                        (iteration "Iteration Features" (lisp c pascal f77 all))
                        (datatypeing "Strong Data-Typeing" (c pascal f77 all)))
                      '((lisp "Lisp" nil nil nil (all))
                        (prolog "LM-Prolog" nil nil nil (all))
                        (c "C"  nil nil nil (all))
                        (pascal "Pascal" nil nil nil (all))
                        (f77 "Fortran"  nil nil nil (all))
                        (all "All" t nil nil t))))

;;; Here the use of exclusivity implication (nil t nil nil)
;;; forces the user to "choose one only"...

(pt-example "complex multiple choice with choose-one-only constraint"
  (tv:multiple-choose "Vote For Favorite Language Features"
                      '((arithmetic "Full arithmetic" (lisp prolog c pascal f77))
                        (lists "List processing" (prolog (lisp t)))
                        (unix "Streams I//O" ((c t) lisp))
                        (iteration "Iteration features" (lisp (c t) pascal f77))
                        (datatyping "Strong Data-Typeing" (c f77 (pascal t))))
                      '((lisp "Lisp" nil t nil nil)
                        (prolog "LM-Prolog" nil t nil nil)
                        (c "C" nil t nil nil)
                        (pascal "Pascal" nil t nil nil)
                        (f77 "Fortran" nil t nil nil))))

;;; TV:BASIC-MOUSE-SENSITIVE-ITEMS

;;; Thanks, Loren

;;;This suggests a way to do mouse-sensitive-items with some capability
;;;of redisplaying the items after a clear-screen.

(defflavor item-window
         ((refresh-function #'(lambda()(send self :clear-window))))
         (tv:basic-mouse-sensitive-items
          tv:borders-mixin
          tv:top-box-label-mixin
          tv:window)
  (:inittable-instance-variables))

(defmethod (item-window :after :refresh) (&rest ignore)
  (funcall refresh-function))

(compile-flavor-methods item-window)

(defun item-windows()
  (let(itemlist mywindow)
    ;;Set up function for left click
    (tv:add-typeout-item-type itemlist
                              :toggle
                              "reverse video"
                              tv:complement-bow-mode t
                              "L: Reverse Video")
    ;;Set up function for right click
    (tv:add-typeout-item-type itemlist
                              :toggle
                              "don't reverse"
                              ignore nil        ;no-arg function
                              "R: Reverse video")
    (setq mywindow
          (make-instance 'item-window
                         :edges '(100 100 500 500)
                         :expose-p t
                         :save-bits t
                         :item-type-alist itemlist
                         :refresh-function
                         #'(lambda()
                             (send self :clear-window)
                             (format self "~%Click on <toggle> to complement the screen.")
                             (format self "~&Press ~\lozenged-char\ to exit~2%" #\end)
                             (send self :item :toggle "toggle")
                             (send self :primitive-item :toggle "toggle" 200 200 270 270)
                             (send self :fresh-line))
                         :label "Mouse Sensitive Items. Use <Clear-Screen> to redisplay."))
    (loop for input = (send mywindow :any-tyi)
          do
          (cond ((listp input)
                 (selectq (first input)
                   (:typeout-execute (funcall (second input)))))
                ((equal input #\end)
                 (format mywindow "~%Item type alist is now:~2%")
                 (grind-top-level itemlist 80 mywindow)
                 (send mywindow :bury)
                 (return))
                ((equal input #\page)
                 (send mywindow :refresh))
                ((typep input '(or character fixnum))
                 (format mywindow "~c" (coerce input 'character)))
                (t (print input mywindow))
                ))
    itemlist))

(pt-example "flavor for mouse-sensitive-items with refresh"
  (item-windows))

;;; POP-UP EDIT WINDOWS

;;; Returns edited string, or NIL if user aborts.

(pt-example "pop-up editing windows (returns a string)"
  (zwei:pop-up-edstring
    (string-append                              ;initial text
      "This is a test of the pop up edit string"
      #/return)
    '(:mouse)                                   ;near-mode
    nil
    500                                         ;minimum width (pixels)
    500                                         ;mimimum height (pixels)
    '("Edit and press <end> to exit")           ;string(s) for bottom msg.
    ))
