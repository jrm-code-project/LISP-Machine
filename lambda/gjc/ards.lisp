
;;; ARDS graphics interpreter for the Gould spooling system.
;;;Writes vectors, lines, etc. into the page buffer.
;;; Window for gould plotting is  [8. 1015., 5. 790.]
(Comment ARDS and TEK Interpreter for the Gould Spooling System)

(declare (special current-height current-width current-baseline current-rot
                  current-font xorg yorg last-x last-y in-file pb-char-width2
                  pb-char-height2 eoffn pb-size-x pb-size-y clear-page-buffer
                  tek-hix tek-hiy tek-loy output
                  grf-thick grf-scale grf-xorg grf-yorg
                  grf-rot grf-defaults grf-botmar grf-lftmar)
         (fixnum current-height current-width current-baseline current-rot
                 xorg yorg last-x last-y pb-char-width2 pb-char-height2
                 pb-size-x pb-size-y x y x1 y1 x2 y2 i j start grf-thick
                 grf-xorg grf-yorg grf-rot hix hiy loy tek-hix tek-hiy
                 tek-loy grf-botmar grf-lftmar)
         (flonum grf-scale)
         (fixnum (grf-x fixnum fixnum) (grf-y fixnum fixnum)
                 (grf-char fixnum) (ards-to-pb-setpoint)
                 (ards-to-pb-long) (ards-to-pb-short) (tek-to-pb-vector))
         (notype (grf-line fixnum fixnum fixnum fixnum) (gtyo fixnum)
                 (pb-linen fixnum fixnum fixnum fixnum fixnum)
                 (font fixnum notype fixnum) (switch-font fixnum)
                 (output-page-buffer) (clear-page-buffer) (font-init))
         (*expr font font-init switch-font gtyo pb-linen set-file-info
                output-page-buffer clear-page-buffer))

(setq grf-defaults '((grf-scale  . 1.75)
                      (grf-thick  . 2.)
                      (grf-botmar . 150.)
                      (grf-lftmar . 150.)))

(defun process-ards-file (commands) (process-grf-file commands t))

(defun process-tek-file  (commands) (process-grf-file commands nil))

(defun process-grf-file (commands ards-flag)
       (setq in-file (open in-file '(in dsk ascii)))
       (eoffn in-file (function (lambda (file flag) -1.)))
       (set-file-info)
       (clear-page-buffer)
       (font-init)
       (process-grf-commands commands)
       (desetq (grf-xorg grf-yorg)
               (caseq grf-rot
                      (0 `(,grf-lftmar
                           ,(- pb-size-x grf-botmar)))
                      (1 `(,(- (lsh pb-size-y 5.) grf-botmar)
                           ,(- pb-size-x grf-lftmar)))
                      (2 `(,(- (lsh pb-size-y 5.) grf-lftmar)
                           ,grf-botmar))
                      (t `(,grf-botmar
                           ,grf-lftmar))))
       (font 31. '((dsk fonts) 25fr kst) grf-rot)
       (switch-font 31.)
       (setq last-x 0. last-y 0. tek-hix 0. tek-hiy 0. tek-loy 0.
             pb-char-width2
             (1+ (fix (//$ (float current-width) grf-scale)))
             pb-char-height2
             (1+ (fix (//$ (float current-height) grf-scale))))
       (do ((i (tyi in-file)))
           ((< i 0.))
           (setq i
                 (caseq i
                        (12. (output-page-buffer) (clear-page-buffer) (tyi in-file))
                        (29. (if ards-flag (ards-to-pb-setpoint)
                                 (tek-to-pb-vector)))
                        (30. (if ards-flag (ards-to-pb-long)
                                 (grf-char i ards-flag)))
                        (31. (if ards-flag (ards-to-pb-short)
                                 (grf-char i ards-flag)))
                        (t (grf-char i ards-flag)))))
       (if (not clear-page-buffer) (output-page-buffer)))

(defun process-grf-commands (commands)
   (mapcar (function (lambda (l) (set (car l) (cdr l)))) grf-defaults)
   (setq grf-rot (if (eq 'xgp output) 3 0))             ;default rotations
   (do ((l commands (cdr l)) (command) (val))
       ((null l))
       (desetq (command . val) (car l))
       (caseq command
              ((thick botmar lftmar rotate)
               (if (fixp val) (set (cdr (assoc command
                                               '((thick . grf-thick)
                                                 (botmar . grf-botmar)
                                                 (lftmar . grf-lftmar)
                                                 (rotate . grf-rot))))
                                   val)))
              (scale
               (if (numberp val) (setq grf-scale (*$ 1.75 (float val))))))))

(defun grf-char (i ards-flag)
   (do ((i i (tyi in-file)))
       ((or (< i 0.) (= i 12.) (= i 29.)
            (and ards-flag (or (= i 30.) (= i 31.))))
        i)
       (caseq i
              (8. (setq last-x (- last-x pb-char-width2)))
              (9. (setq last-x (* 8. pb-char-width2
                                  (1+ (// last-x pb-char-width2 8.)))))
              (10. (setq last-y (- last-y pb-char-height2)))
              (11. (setq last-y (+ last-y pb-char-height2)))
              (13. (setq last-x 0.))
              (32. (setq last-x (+ last-x pb-char-width2)))
              ((22.  27. 31.)                    ;ignore ,,  in tek mode
               (if ards-flag (grf-tyo i)))       ;otherwise print them
              ((127. 28.))                       ;ignore rubout and ^\
              (T (grf-tyo i)))))                 ;typeout the anything else

(defun grf-tyo (ch)
   (gtyo ch (grf-y last-x last-y) (grf-x last-x last-y))
   (setq last-x (+ last-x pb-char-width2)))

;;;; everything from here on should probably get rewritten in MIDAS

(defun ards-to-pb-setpoint nil
   (do ((i (tyi in-file) (tyi in-file))
        (x1) (x2) (y1) (y2) (j 0. (\ (1+ j) 4.)))
       ((< i 64.) i)
       (caseq j
              (0. (setq x1 i))
              (1. (setq x2 i))
              (2. (setq y1 i))
              (3. (setq y2 (+ (lsh (boole 1. 31. i) 5.)
                              (boole 1. 31. (lsh y1 -1.))))
                  (if (= (boole 1. 1. y1) 1.) (setq y2 (- y2)))
                  (setq x2 (+ (lsh (boole 1. 31. x2) 5.)
                              (boole 1. 31. (lsh x1 -1.))))
                  (if (= (boole 1. 1. x1) 1.) (setq x2 (- x2)))
                  (setq last-x (+ 512. x2)
                        last-y (+ 512. y2))))))

(defun ards-to-pb-long nil
   ; dotted vectors not implemented
   (do ((i (tyi in-file) (tyi in-file))
        (x1) (x2) (y1) (y2) (j 0. (\ (1+ j) 4.)))
       ((< i 64.) i)
       (caseq j
              (0. (setq x1 i))
              (1. (setq x2 i))
              (2. (setq y1 i))
              (3. (setq y2 (+ (lsh (boole 1. 31. i) 5.)
                              (boole 1. 31. (lsh y1 -1.))))
                  (if (= (boole 1. 1. y1) 1.) (setq y2 (- y2)))
                  (setq i (boole 1. 1. (lsh x2 -5.)))
                  (setq x2 (+ (lsh (boole 1. 31. x2) 5.)
                              (boole 1. 31. (lsh x1 -1.))))
                  (if (= (boole 1. 1. x1) 1.) (setq x2 (- x2)))
                  (if (= i 0.) (grf-line last-x last-y
                                          (+ last-x x2) (+ last-y y2)))
                  (setq last-x (+ last-x x2)
                        last-y (+ last-y y2))))))

(defun ards-to-pb-short nil
   (do ((i (tyi in-file) (tyi in-file)) (x1) (y1) (j 0. (\ (1+ j) 2.)))
       ((< i 64.) i)
       (caseq j
              (0. (setq x1 i))
              (1. (setq y1 i)
                  (setq i (boole 1. 1. y1)
                        y1 (boole 1. 31. (lsh y1 -1.)))
                  (if (= i 1.) (setq y1 (- y1)))
                  (setq i (boole 1. 1. x1)
                        x1 (boole 1. 31. (lsh x1 -1.)))
                  (if (= i 1.) (setq x1 (- x1)))
                  (grf-line last-x last-y (+ last-x x1) (+ last-y y1))
                  (setq last-x (+ last-x x1) last-y (+ last-y y1))))))

(defun tek-to-pb-vector nil
   (do ((i (tyi in-file) (tyi in-file))
        (esc-flag) (dark-flag t) (loy-flag)
        (hiy tek-hiy) (hix tek-hix) (loy tek-loy) (x 0.) (y 0.))
       ((or (= i 31.) (= i 13.) (< i 0.) (and esc-flag (= i 12.)))
        (setq tek-hix hix tek-hiy hiy tek-loy loy)
        i)
       (setq esc-flag (= i 27.))
       (cond ((= i 29.) (setq dark-flag t))
             ((< i 32.))                         ;ignore other controls
             ((< i 64.) (if loy-flag (setq hix i)
                            (setq hiy i)))
             ((< i 96.)
              (setq x (boole 7. (lsh (boole 1. 31. hix) 5.)(boole 1. 31. i))
                    y (boole 7. (lsh (boole 1. 31. hiy) 5.)(boole 1. 31. loy)))
              (if dark-flag (setq dark-flag nil)
                  (grf-line last-x last-y x y))
              (setq last-x x last-y y loy-flag nil))
             (t (setq loy i loy-flag t)))))

(comment Co-ordinate transformation)

;Transform ards x and y coordinates into pb ones s.t. [0,0] ==> [grf-xorg,grf-yorg]
;grf-rot specifies rotation, 0 means
; ards x axis ==> pb y axis, ards y axis ==> pb -x axis
;rotation is measured anti-clockwise in units of 90 degrees

(defun grf-x (x y)
   (+ grf-xorg
      (if (oddp grf-rot) (* (- grf-rot 2.) (fix (*$ (float y) grf-scale)))
          (* (- 1. grf-rot) (fix (*$ (float x) grf-scale))))))

(defun grf-y (x y)
   (+ grf-yorg
      (if (oddp grf-rot) (* (- grf-rot 2.) (fix (*$ (float x) grf-scale)))
          (* (1- grf-rot) (fix (*$ (float y) grf-scale))))))

(defun grf-line (x y x1 y1)
    (pb-linen (grf-y x y) (grf-x x y) (grf-y x1 y1) (grf-x x1 y1)
              grf-thick))

;; Local Modes:
;; Mode: LISP
;; Comment Col: 40
;; END:
