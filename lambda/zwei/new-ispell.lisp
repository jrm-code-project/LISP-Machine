;;; -*- Mode:LISP; Package:ZWEI; Base:8; Readtable:ZL -*-

;;; (C) Enchancments Copyright LISP Machine, Inc. 1984
;;;   See filename "Copyright" for
;;; licensing and release information.

;;;These functions depend on the ISPELL system being loaded.  Note that
;;;the current ISPELL is not the old ISPELL server, but rather a local
;;;host resident spelling checker.  It isn't currently loaded by default
;;;with ZWEI, it must be loaded by the user.  The interface functions
;;;check this by looking to see if the ZWEI:SPELL-WORD function is
;;;FBOUNDP.  -Keith 8/1/88

(defun assure-spell-system-loaded(&optional error-p)
  "Try to make sure the spelling checker is loaded.
Won't signal an error unless ERROR-P is non-NIL."
  (unless (fboundp 'spell-word)
    (if (not (si:find-system-named :ISPELL (not error-p)))
        (barf "The ISPELL system is not accessible")
      (if (not (y-or-n-p-with-timeout (* 2. 60. 60.) T "Load the ISPELL (spelling checker) system?"))
          (barf "ISPELL is not loaded.")
        (progn
          (unless (find-package "SPELL")
            (make-package "SPELL" :use "GLOBAL"))
          (make-system :ISPELL :noconfirm)
          (unless (fboundp 'spell-word)
            (barf "Error: The spelling checker is not defined.")))))))

(defcom com-correct-word-spelling
        "Check the spelling of the word before point.
The status of the word will be reported in the echo area."
  ()
  (assure-spell-system-loaded)
  (let* ((bp1 (forward-word (forward-word (forward-char (point) -1 t) 1 t) -1 t))
         (bp2 (forward-word bp1 1 t))
         (word (string-interval bp1 bp2)))
    (format *query-io* "Checking the spelling of ~s --" word)
    (zwei-correct-spelling bp1 bp2)
    )
  dis-none)

(defun spell-replace-word (new-word bp1 bp2)
  (with-undo-save ("Correct spelling of word" bp1 bp2 t)
    (let ((bp3 (copy-bp bp2 :moves))
          (move-point (and (eq (bp-line bp1) (bp-line (point)))
                           (eq (bp-line bp2) (bp-line (point)))
                           (or (bp-= bp1 (point))
                               (bp-< bp1 (point)))
                           (bp-< (point) bp2))))
      (delete-interval bp1 bp2 t)
      (insert bp1 new-word)
      (when move-point
        (move-bp (point) bp3))
      (flush-bp bp3)))
  (cond ((string-search-char #/return new-word)
         (must-redisplay *window* dis-text))
        (t
         (must-redisplay *window* dis-line (bp-line bp1) (bp-index bp1)))))

(defun zwei-correct-spelling (bp1 bp2 &optional initial-choices whole-buffer-p &aux word)
  (do-forever
    (setq word (string-interval bp1 bp2 t t))
    (let ((result (cond ((null initial-choices)
                         (inhibit-style-warnings (spell-word word)))
                        (t
                         (prog1 initial-choices
                                (setq initial-choices nil))))))
      (cond ((eq word result)
             (when (null whole-buffer-p)
               (format *query-io* " found it."))
             (return nil))
            ((stringp result)
             (when (null whole-buffer-p)
               (format *query-io* " found it because of ~s." result))
             (return nil))
            ((null result)
             (when (null whole-buffer-p)
               (format *query-io* " not found."))
             (return nil))
            ((and (null whole-buffer-p)
                  (= (length result) 1))
             (format *query-io* " near miss for ~s ... replacing." (car result))
             (spell-replace-word (car result) bp1 bp2)
             (return nil))
            (t
             ;;result is a list of possible replacements
             (let ((choice (select-word word result)))
               (cond ((null choice)
                      (return nil))
                     ((and (integerp choice)
                           (>= choice 0)
                           (< choice (length result)))
                      (spell-replace-word (nth choice result) bp1 bp2)
                      (return nil))
                     ((stringp choice)
                      (setq bp1 (copy-bp bp1 :normal))
                      (setq bp2 (copy-bp bp2 :moves))
                      (spell-replace-word choice bp1 bp2)
                      (flush-bp bp1)
                      (flush-bp bp2)
                      ;;and loop around
                      ))))))))

(defun select-word (bad-word word-list)
  (prompt-line "~s not found.  Select a replacement:" bad-word)
  (let* ((width (send *query-io* :size-in-characters))
         (tab1 (round width 4))
         )
    (send *query-io* :clear-screen)
    (do ((i 0 (1+ i))
         (words word-list (cdr words)))
        ((or (null words)
             (= i 9))
         (send *query-io* :set-cursorpos (* tab1 (ceiling i 3)) 0 :character)
         (format *query-io* "R      Replace")
         (send *query-io* :set-cursorpos (* tab1 (ceiling i 3)) 1 :character)
         (format *query-io* "Space  Accept")
         )
      (send *query-io* :set-cursorpos
            (* tab1 (floor i 3))
            (remainder i 3)
            :character)
      (format *query-io* "~d ~a" i (car words))
      )
    (do ((char (char-upcase (char-code (read-char)))
               (char-upcase (char-code (read-char)))))
        (())
      (cond ((<= #/0 char (digit-char (length word-list) 10.))
             (send *query-io* :clear-screen)
             (return (- char #/0)))
            ((char= char #/R)
             (redisplay-mode-line)
             (send *query-io* :clear-screen)
             (format *query-io* "Type replacement word: ")
             (return (values (readline-trim *query-io*))))
            ((char= char #/space)
             (send *query-io* :clear-screen)
             (return nil))
            (t
             (send standard-output :beep))))))

(defcom com-correct-spelling
        "Run the spelling checker on the buffer or the region.
Each word not found in the dictionary will be displayed, and
your choices for proceeding will be listed in the echo area."
        ()
  (assure-spell-system-loaded)
  (let ((bp1 (interval-first-bp *interval*))
        (bp2 (interval-last-bp *interval*)))
    (when (window-mark-p *window*)
      (setq bp1 (copy-bp (mark)) bp2 (copy-bp (point)))
      (order-bps bp1 bp2))
    (setf (window-mark-p *window*) nil)
    (must-redisplay *window* dis-mark-goes)
    (setq bp1 (copy-bp bp1))
    (setq bp2 (copy-bp bp2 :moves))
    (do-named for-each-line
              (end-of-line-bp
                line bp3
                )
              (())
      (setq line (bp-line bp1))
      (setq end-of-line-bp (create-bp line
                                      (if (eq line (bp-line bp2))
                                          (bp-index bp2)
                                        (string-length line))
                                      :moves))
      (do-named for-a-line
                ()
                (())
        ;;skip to first alphabetic character
        (do-named find-next-word
                  ()
                  (())
          (when (bp-= bp1 end-of-line-bp)
            (return-from find-next-word nil))
          (let ((char (char-code (aref line (bp-index bp1)))))
            (when (or (<= #/A char #/Z)
                      (<= #/a char #/z))
              (return-from find-next-word nil))
            (incf (bp-index bp1))))
        (setq bp3 (copy-bp bp1))
        ;;find end of alphabetic string
        (do-named find-last-letter-of-word
                  ()
                  (())
          (when (bp-= bp3 end-of-line-bp)
            (return-from find-last-letter-of-word nil))
          (let ((char (char-code (aref line (bp-index bp3)))))
            (when (not (or (<= #/A char #/Z)
                           (<= #/a char #/z)))
              (return-from find-last-letter-of-word nil)))
          (incf (bp-index bp3)))
        (when (= (bp-index bp1) (bp-index bp3))
          (return-from for-a-line nil))
        ;;now check it
        (let ((choices (inhibit-style-warnings (spell-word line (bp-index bp1) (bp-index bp3)))))
          (when (or (null choices)
                    (consp choices))
            (setq bp3 (copy-bp bp3 :moves))
            (fix-word bp1 bp3 choices)
            (flush-bp bp3)))
        (move-bp bp1 bp3))
      ;;now move bp1 to next line
      (when (eq line (bp-line bp2))
        (return-from for-each-line nil))
      (move-bp bp1 (line-next line) 0)
      (flush-bp end-of-line-bp)
      )
    (flush-bp bp2)
    )
  dis-none)

(defun fix-word (bp1 bp2 choices)
  (POINT-PDL-PUSH (point) *WINDOW* nil nil)
  (move-bp (point) bp1)
  (must-redisplay *window* dis-bps)
  (REDISPLAY *WINDOW* :POINT)
  (REDISPLAY-MODE-LINE)
  (zwei-correct-spelling bp1 bp2 choices t))
