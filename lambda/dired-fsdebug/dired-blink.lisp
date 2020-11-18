;;; -*- Mode:LISP; Package:TV; Base:10 -*-

;;; Copyright (C) Lisp Machine, Inc. 1984, 1985, 1986
;;;   See filename "Copyright" for
;;; licensing and release information.


;;; Flavors, methods, and functions that are part of DIRED, but are executed
;;; in the TV package by the mouse process.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; UNBLINKING-REVERSE-VIDEO-LINE-BLINKER
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defflavor UNBLINKING-REVERSE-VIDEO-LINE-BLINKER
         ()
        (rectangular-blinker)
  (:documentation :combination
   "A rectangular-blinker that highlights a whole line."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (UNBLINKING-REVERSE-VIDEO-LINE-BLINKER :SET-VISIBILITY)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod (UNBLINKING-REVERSE-VIDEO-LINE-BLINKER :SET-VISIBILITY)
           (new-visibility &aux (inhibit-scheduling-flag t))
  "Defy the devil in Zmacs by making the cursor blinker stay on."
  (check-type new-visibility (member t nil :blink :on :off) "a valid blinker visibility type.")
  (when (eq new-visibility :blink) (setq new-visibility :on))
  (cond ((eq visibility new-visibility))
        ((eq phase new-visibility)
         (setq visibility new-visibility))
        (t
         (do () ((not (sheet-output-held-p sheet)))
             (setq inhibit-scheduling-flag nil)
             (send sheet :output-hold-exception)
             (setq inhibit-scheduling-flag t))
         (or new-visibility (open-blinker self))
         (setq visibility new-visibility)
         ;; Blinker clock will fix the screen
         (setq time-until-blink 0))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (UNBLINKING-REVERSE-VIDEO-LINE-BLINKER :BLINK)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod (UNBLINKING-REVERSE-VIDEO-LINE-BLINKER :BLINK) (&aux string-length)

  (when (eq (send self :visibility) :blink)
    (send self :set-visibility :on))

  (setq string-length
        (sheet-string-length
          sheet
          (or (nth-value 3 (zwei:mouse-char sheet t x-pos y-pos)) "")))

  (when (> string-length 0) (setq string-length (plus 1 string-length)))
  (%draw-rectangle-clipped
    string-length
    height
    3
    (max (truncate height -2)
         (min (- (sheet-height sheet) (truncate height 2)) y-pos))
    alu-xor sheet))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DUMMY-RECTANGULAR-BLINKER
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defflavor DUMMY-RECTANGULAR-BLINKER
         ()
        (rectangular-blinker)
  (:documentation :combination
   "A rectangular-blinker that remains invisible at all times."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (DUMMY-RECTANGULAR-BLINKER :BLINK)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod (DUMMY-RECTANGULAR-BLINKER :BLINK) ()
  ())



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; COMPILE-TIME DIRECTIVES
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(compile-flavor-methods unblinking-reverse-video-line-blinker)
(compile-flavor-methods dummy-rectangular-blinker)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MOVE-POINT-VIA-MOUSE-PROCESS
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;(defun MOVE-POINT-VIA-MOUSE-PROCESS (sheet line)
;  (zwei:move-bp (send sheet :point) line (min (length line) 1))
;  (zwei:must-redisplay sheet zwei:dis-bps)
;  (send zwei:*dired-display-pane* :force-kbd-input #/h-s-m-c-r))



;   (setq point-line (car (send sheet :point)))
;   (when (and (stringp point-line) (fixnump (send sheet :cursor-x)) (fixnump (send sheet :cursor-y)))
;     (cond ((null (zwei:line-previous point-line))
;           (move-point-via-mouse-process sheet (zwei:line-next (zwei:line-next point-line)))
;            (send self :set-cursorpos (send sheet :cursor-x) (send sheet :cursor-y)))
;;           (with-blinker-ready t (setq x-pos (send sheet :cursor-x) y-pos (send sheet :cursor-y))))
;           ((null (zwei:line-previous (zwei:line-previous point-line)))
;            (move-point-via-mouse-process sheet (zwei:line-next point-line))
;            (send self :set-cursorpos (send sheet :cursor-x) (send sheet :cursor-y)))
;;           (with-blinker-ready t (setq x-pos (send sheet :cursor-x) y-pos (send sheet :cursor-y))))
;           ((null (zwei:line-next point-line))
;            (move-point-via-mouse-process sheet (zwei:line-previous point-line))
;            (send self :set-cursorpos (send sheet :cursor-x) (send sheet :cursor-y)))))
;;           (with-blinker-ready t (setq x-pos (send sheet :cursor-x) y-pos (send sheet :cursor-y))))))

;  (send zwei:*dired-pathname-pane*
;       :set-item-list
;       (list
;         (list
;           (format nil "~A    ~A" zwei:*dired-blinker-on* zwei:*dired-blinker-count*)
;           :no-select nil
;           :font 'mets)))

;       (send zwei:*dired-pathname-pane*
;            :set-item-list
;            (list
;              (list
;                (format nil "Yowzah!")
;                :no-select nil
;                :font 'mets)))

;  (incf zwei:*dired-blinker-count*)

;(defvar zwei:*event-list* (list nil))
;(defun zwei:event (thing)
;  (nconc zwei:*event-list* (list thing)))
;(defun zwei:event (ignore)
;  nil)
;(defun zwei:iel ()
;  (setq zwei:*event-list* (list nil)))

;(defmethod  (blinker :snap) ()
;  (list
;    (format nil "X-POS: ~A" x-pos)
;    (format nil "Y-POS: ~A" y-pos)
;    (format nil "VISIB: ~A" visibility)
;    (format nil "D-VIS: ~A" deselected-visibility)
;    (format nil "H-PER: ~A" half-period)
;    (format nil "PHASE: ~A" phase)
;    (format nil "T-U-B: ~A" time-until-blink)
;    (format nil "FOL-P: ~A" follow-p)))

;  (and phase
;       (eq (send zwei:*dired-display-frame* :configuration) 'zwei:debug-configuration)
;       (zwei:funcall-in-zmacs 'zwei:com-fsdebug-current-file-if-needed zwei:*dired-display-pane*)))
