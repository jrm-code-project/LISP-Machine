;;; -*- Mode:LISP; Package:TV; Base:10; Readtable:ZL -*-


(DEFUN WHO-LINE-SETUP ()
  (WHEN (NULL WHO-LINE-SCREEN)
    (SETQ WHO-LINE-SCREEN
          (DEFINE-SCREEN 'WHO-LINE-SCREEN "Who Line Screen"
            :AREA WHO-LINE-AREA
            :DEFAULT-FONT FONTS:CPTFONT ;not *DEFAULT-FONT*
            :BUFFER MAIN-SCREEN-BUFFER-ADDRESS
            :CONTROL-ADDRESS #o377760
            :PROPERTY-LIST '(:VIDEO :BLACK-AND-WHITE
                                    :CONTROLLER :SIMPLE
                                    :WHO-LINE T)
            :WIDTH MAIN-SCREEN-WIDTH
            :LOCATIONS-PER-LINE MAIN-SCREEN-LOCATIONS-PER-LINE
            :CHARACTER-HEIGHT 2
            :VSP 0
            :Y NIL                      ;Force this to be calculated
            :BOTTOM MAIN-SCREEN-HEIGHT))
    ;; 18 characters of the date and time
    (SETQ NWATCH-WHO-LINE-SHEET
          (WHO-LINE-FIELD :FLAVOR 'WHO-LINE-SHEET
                          :WHO-LINE-UPDATE-FUNCTION 'NWATCH-WHO-FUNCTION
                          :HEIGHT (SHEET-LINE-HEIGHT WHO-LINE-SCREEN)
                          :LEFT 0 :RIGHT 144. :BOTTOM (SHEET-HEIGHT WHO-LINE-SCREEN)))
    ;; 24 characters of user id or process
    (WHO-LINE-FIELD :FLAVOR 'WHO-LINE-SHEET
                    :WHO-LINE-UPDATE-FUNCTION 'WHO-LINE-USER-OR-PROCESS
                    :HEIGHT (SHEET-LINE-HEIGHT WHO-LINE-SCREEN)
                    :LEFT 152. :RIGHT 344. :BOTTOM (SHEET-HEIGHT WHO-LINE-SCREEN))
    ;; 2 characters of readtable
    (WHO-LINE-FIELD :FLAVOR 'WHO-LINE-SHEET
                    :WHO-LINE-UPDATE-FUNCTION 'WHO-LINE-READTABLE
                    :HEIGHT (SHEET-LINE-HEIGHT WHO-LINE-SCREEN)
                    :LEFT 352. :RIGHT 368. :BOTTOM (SHEET-HEIGHT WHO-LINE-SCREEN))
    ;; 10 characters of package
    (WHO-LINE-FIELD :FLAVOR 'WHO-LINE-SHEET
                    :WHO-LINE-UPDATE-FUNCTION 'WHO-LINE-PACKAGE
                    :HEIGHT (SHEET-LINE-HEIGHT WHO-LINE-SCREEN)
                    :LEFT 372. :RIGHT 452. :BOTTOM (SHEET-HEIGHT WHO-LINE-SCREEN))
    ;; 19 characters of process state
    (SETQ WHO-LINE-RUN-STATE-SHEET
          (WHO-LINE-FIELD :FLAVOR 'WHO-LINE-SHEET
                          :WHO-LINE-UPDATE-FUNCTION 'WHO-LINE-RUN-STATE
                          :HEIGHT (SHEET-LINE-HEIGHT WHO-LINE-SCREEN)
                          :BOTTOM (SHEET-HEIGHT WHO-LINE-SCREEN)
                          :LEFT 460.
                          :RIGHT (SELECT-PROCESSOR ((:LAMBDA :EXPLORER) 612.) (:CADR 480.))))
    ;; The remaining 36 characters go to the file/idle/boot state
    (SETQ WHO-LINE-FILE-STATE-SHEET
          (WHO-LINE-FIELD :FLAVOR 'WHO-LINE-FILE-SHEET
                          :LEFT (SELECT-PROCESSOR (:CADR 480.) ((:LAMBDA :EXPLORER) 620.))
                          :RIGHT (SELECT-PROCESSOR (:CADR 768.) ((:LAMBDA :EXPLORER) 1024.))
                          :HEIGHT (SHEET-LINE-HEIGHT WHO-LINE-SCREEN)
                          :BOTTOM (SHEET-HEIGHT WHO-LINE-SCREEN)))
    ;; Above those windows is a full line of mouse button documentation
    (SETQ WHO-LINE-DOCUMENTATION-WINDOW
          (WHO-LINE-FIELD :FLAVOR 'WHO-LINE-SHEET
                          :WHO-LINE-UPDATE-FUNCTION 'WHO-LINE-DOCUMENTATION-FUNCTION
                          :HEIGHT (SHEET-LINE-HEIGHT WHO-LINE-SCREEN)
                          :TOP 0 :REVERSE-VIDEO-P T))))


(DEFUN WHO-LINE-READTABLE (WHO-SHEET)
  (DECLARE (:SELF-FLAVOR WHO-LINE-SHEET))
  (WHEN (AND *SHOW-READTABLE-IN-WHO-LINE*
             LAST-WHO-LINE-PROCESS)
    (LET* ((SG (PROCESS-STACK-GROUP LAST-WHO-LINE-PROCESS))
           (RDTBL (COND ((EQ SG %CURRENT-STACK-GROUP) *READTABLE*)
                        ((TYPEP SG 'STACK-GROUP) (SYMEVAL-IN-STACK-GROUP '*READTABLE* SG))
                        (T READTABLE))))
      (WHEN (AND RDTBL
                 (NEQ WHO-LINE-EXTRA-STATE RDTBL))
        (SETQ RDTBL (HL:RDTBL-SHORTEST-NAME RDTBL))
        (PREPARE-SHEET (WHO-SHEET)
          (SHEET-CLEAR WHO-SHEET)
          (SHEET-STRING-OUT WHO-SHEET "Hi" 0 (MIN (STRING-LENGTH RDTBL) 2)))))))
