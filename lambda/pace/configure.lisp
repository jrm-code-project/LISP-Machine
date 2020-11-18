;;; -*- Mode:LISP; Package:TV; Base:10 -*-
;;; Try to adapt to the proliferation of incompatible video boards.

(DEFUN SET-CONSOLE-SIZE (WIDTH HEIGHT)
  (DELAYING-SCREEN-MANAGEMENT
    (WITH-MOUSE-USURPED
      (LOCK-SHEET (MAIN-SCREEN)
        (LOCK-SHEET (WHO-LINE-SCREEN)
          (WITHOUT-INTERRUPTS
            (LET ((MS MOUSE-SHEET) (SW SELECTED-WINDOW))
              (WHEN (SHEET-ME-OR-MY-KID-P MS MAIN-SCREEN) (SETQ MOUSE-SHEET NIL))
              (SEND WHO-LINE-SCREEN :DEEXPOSE)
              (SEND MAIN-SCREEN :DEEXPOSE)
              (SI:CLEAR-SCREEN-BUFFER SYS:IO-SPACE-VIRTUAL-ADDRESS)
              (SETQ MAIN-SCREEN-HEIGHT HEIGHT)
              (SEND WHO-LINE-SCREEN :CHANGE-OF-SIZE-OR-MARGINS
                                    :WIDTH WIDTH
                                    :TOP (- HEIGHT (SHEET-HEIGHT WHO-LINE-SCREEN)))
              (SEND MAIN-SCREEN :CHANGE-OF-SIZE-OR-MARGINS
                                :WIDTH WIDTH
                                :HEIGHT (- HEIGHT (SHEET-HEIGHT WHO-LINE-SCREEN)))
              (COND ((> WIDTH 800.)
                     (SEND WHO-LINE-RUN-STATE-SHEET :CHANGE-OF-SIZE-OR-MARGINS :LEFT 328. :RIGHT 520.)
                     (SEND WHO-LINE-FILE-STATE-SHEET :CHANGE-OF-SIZE-OR-MARGINS :LEFT 520. :RIGHT 1024.)
                     (SEND WHO-LINE-DOCUMENTATION-WINDOW :CHANGE-OF-SIZE-OR-MARGINS :WIDTH 1024.))
                    (T
                     (SEND WHO-LINE-RUN-STATE-SHEET :CHANGE-OF-SIZE-OR-MARGINS :LEFT 328. :RIGHT 480.)
                     (SEND WHO-LINE-FILE-STATE-SHEET :CHANGE-OF-SIZE-OR-MARGINS :LEFT 480. :RIGHT 800.)
                     (SEND WHO-LINE-DOCUMENTATION-WINDOW :CHANGE-OF-SIZE-OR-MARGINS :WIDTH 800.)))
              (INITIALIZE-RUN-LIGHT-LOCATIONS)
              (SEND WHO-LINE-SCREEN :EXPOSE)
              (SEND MAIN-SCREEN :EXPOSE)
              (MOUSE-SET-SHEET MAIN-SCREEN)
              (WHEN SW (SEND SW :SELECT)))))))
    (DOLIST (RESOURCE-NAME WINDOW-RESOURCE-NAMES)
      (SI:MAP-RESOURCE
        #'(LAMBDA (WINDOW &REST IGNORE)
            (OR (TYPEP WINDOW 'INSTANCE) (FERROR NIL "LOSSAGE"))
            (IF (TYPEP WINDOW 'TV:BASIC-MENU)
                (LET ((GEO (SEND WINDOW :GEOMETRY)))
                  (DO ((L GEO (CDR L))) ((NULL L))
                    (SETF (CAR L) NIL)))
              (LET* ((SUPERIOR (SEND WINDOW :SUPERIOR))
                     (BOTTOM (SEND WINDOW :HEIGHT))
                     (SUPHEIGHT (OR (SEND SUPERIOR :SEND-IF-HANDLES :INSIDE-HEIGHT)
                                    (SEND SUPERIOR :HEIGHT))))
                (IF (> BOTTOM SUPHEIGHT)
                    (SEND WINDOW :SET-SIZE (SEND WINDOW :WIDTH) SUPHEIGHT)))))
        RESOURCE-NAME)))
  T)

(DEFUN CONFIGURE-CONSOLE (&optional TYPE)
  "Configure windows on primary console for :PORTRAIT, :LANDSCAPE, or :EXPLORER monitors."
  (when (null type)
    (select-processor
      (:explorer
        (setq type :explorer))
      ((:lambda :cadr)
       (ferror nil "must give type explicitly"))))
  (CASE TYPE
    (:PORTRAIT
     (SET-CONSOLE-SIZE 800. 1020.))
    (:LANDSCAPE
     (SET-CONSOLE-SIZE 1024. 796.))
    (:EXPLORER
     (SET-CONSOLE-SIZE 1024. 804.))
    (OTHERWISE
     (FERROR "Unrecognized console type."))))

(DEFUN LANDSCAPE ()
  "Configure all existing (primary) screens and windows for landscape monitor."
  (CONFIGURE-CONSOLE :LANDSCAPE))

(DEFUN PORTRAIT ()
  "Configure all existing (primary) screens and windows for portrait monitor."
  (CONFIGURE-CONSOLE :PORTRAIT))
