; -*- Mode:LISP; Package:STEVE; Readtable:CL; Base:10 -*-

;Copyright (c) May 1983 by Christopher Eliot
; and Massachusetts Institute of Technology.
;Permission to copy all or part of this material is granted, provided
; that the copies are not made or distributed for resale, the MIT
; copyright notice and reference to the source file and the software
; distribution version appear, and that notice is given that copying
; is by permission of Massachusetts Institute of Technology.

;Newer version of lisp-indentor

(defparameter *lisp-indent-alist*
  '((lambda 1 1) (let 1 1) (let* 1 1)
    (flet 1 1) (macrolet 1 1) (compiler-let 1 1)
    (do 2 1) (do* 2 1) (do-named 3 1) (do*-named 3 1)
    (dotimes 1 1) (dolist 1 1)
    (progv 2 1) (progw 1 1)
    (labels 1 1) (block 1 1) (return-from 1 1)
    (when 1 1) (unless 1 1) (eval-when 1 1)
    (defun 2 1) (defmacro 2 1)
    (defparameter 1 1) (defvar 1 1)
    (defstruct 1 1) (defflavor 1 12 3 1)
    (unwind-protect 0 3 1 1) (catch 1 1) (eval-when 1 1)
    (if 2 1) (cond-every 0 1) (typecase 1 1)
    (select 1 1) (selectq 1 1) (selectq-every 1 1)
    (case 1 1) (typecase 1 1)
    (with-output-to-string 1 1) (with-input-from-string 1 1)
    (with-open-file 1 1)
    (multiple-value 1 1) (multiple-value-bind 0 3 1 5 1 1)
    (:top-level . rigid-indentor)
    (:non-atomic . rigid-indentor)
    ))

(defun indentation-type (bp)
  (send bp :set-pos 0)
  (loop with count = -1                 ;don't count opening-atom
        do (send bp :backward-over-white-space)
        until (send bp :beginning-of-line?)
        do (setq count (1+& count))
        do (setq bp (bp-backward-over-sexp bp))
        until (or (send bp :beginning-of-line?)
                  (char= left-paren (send bp :peek-char-backward)))
        finally (return (values (if (0p (bp-position bp))
                                    :top-level
                                  (let ((type (get-atom-for-indentor-only bp)))
                                    (if (equal type "")
                                        :non-atomic
                                      type)))
                                count
                                (position-really
                                  (bp-line bp) (bp-position bp)
                                  (window-x-size (edit-cursor-window
                                                   *editor-cursor*)))))))

(defun get-atom-for-indentor-only (bp)
  ;;assume in one line.
  (let ((mark (copy-bp bp)))
    (send mark :forward-over-atom)
    (prog1
      (cond ((not (eq (bp-line bp) (bp-line mark))) nil)
            (t (let ((start (bp-position bp)) (end (bp-position mark)))
                 (let ((cnt (-& end start)))
                   (let ((new (make-string cnt :initial-element #\space)))
                     (%string-translate new (line-chars (bp-line bp))
                                        *:character-upcase-table 0 start cnt)
                     (or (intern-soft new) new))))))
      (send mark :expire))))

(defun lisp-indentor (&optional (bp *editor-cursor*) &aux indent-info)
  (let ((mark (send bp :get-mark)))
    (indent-line-to-column
      (bp-line bp)
      (unwind-protect
          (multiple-value-bind
              (type count start-pos) (indentation-type bp)
            (send bp :goto-mark mark)
            (cond ((and (symbolp type)
                        (setq indent-info
                              (cdr (assoc type *lisp-indent-alist*))))
                   (if (symbolp indent-info)
                       (funcall indent-info bp type count start-pos)
                     (generic-lisp-indentor bp type count
                                            start-pos indent-info)))
                  ((let ((type-pname (typecase type
                                       (symbol (get-pname type))
                                       (string type)
                                       (otherwise ""))))
                     (and (>=& (string-length type-pname) 3)
                          (string-equal type-pname "DEF" 0 0 3 3)))
                   (def-hack-indentor bp type count start-pos))
                  (:else
                   (default-indentor bp type count start-pos))))
        (send bp :goto-mark mark)))))


(defun generic-lisp-indentor (bp type count start-pos offsets-list)
  (loop for (skip offset . rest) first offsets-list then rest
        summing skip into scount
        if (= scount count)
           return (+& start-pos offset)
        if (or (>& scount count) (null rest))
           return (default-indentor bp type count start-pos)))

(defun rigid-indentor (ignore ignore ignore start-pos)
  start-pos)

(defun prog-indentor (bp type count start-pos)
  ;;~~manana~~
  )

(defun def-hack-indentor (bp type count start-pos)
  (send bp :to-end-of-previous-line)
  (bp-backward-over-sexp bp)
  (loop with line = (bp-line bp)
        with x-size = (window-x-size (edit-cursor-window *editor-cursor*))
        for pos = (bp-position bp)
        do (send bp :backward-over-horizontal-white-space)
        if (char= (send bp :peek-char-backward) left-paren)
        return (1+& (position-really (bp-line bp) (bp-position bp) x-size))
        do (bp-backward-over-sexp bp)
        if (not (eq (bp-line bp) line))
        return (position-really line pos x-size)))

(defun default-indentor (bp type count start-pos)
  (send bp :to-end-of-previous-line)
  (bp-backward-over-sexp bp)
  (loop with line = (bp-line bp)
        with x-size = (window-x-size (edit-cursor-window *editor-cursor*))
        for i from 1 below count
        for pos = (bp-position bp)
        do (bp-backward-over-sexp bp)
        if (not (eq (bp-line bp) line))
        do (return (position-really line pos x-size))
        finally (return (+& (position-really (bp-line bp)
                                             (bp-position bp) x-size)
                            (if (char= #\( (send bp :peek-char-backward))
                                1
                              0)))))



(editor-bind-key #\tab indent-for-lisp lisp)

(defun indent-for-lisp ()
  (if (=& *argument* 1)
      (lisp-indentor)
      (loop for i from 1 to *argument*
            do (lisp-indentor)
            do (send *editor-cursor* :to-beginning-of-next-line)))
  nil)

(defun indent-line-to-column (line column)
  (with-bp (mark *editor-cursor*)
    (send *editor-cursor* :move line 0)
    (back-to-indentation)
    (let ((current-indentation (position-really (bp-line *editor-cursor*)
                                                (bp-position *editor-cursor*)
                                                259))) ; Very wide screen...
      (unless (= column current-indentation)
        (send *editor-cursor* :set-pos 0)
        (delete-horizontal-space *editor-cursor*)
        (do ((i 1 (1+& i))
              (whole (/& column 8)))
            ((>& i whole))
          (send *editor-cursor* :insert-char #\tab))
        (do ((i 1 (1+& i))
              (part (\\& column 8)))
            ((>& i part))
          (send *editor-cursor* :insert-char #\space))))
    (when (> (bp-position mark) (bp-position *editor-cursor*))
      (move-bp *editor-cursor* mark))))

(editor-bind-key #\control-meta-\\ indent-region)

(defun indent-region ()
  (cond ((-p *argument*) (ed-lose "Indent To Negative Column?"))
        ((or (>& *argument* 1) (not (null argument-supplied?)))
         ;;Indent to column.
         (process-region-as-lines #'indent-line-to-column *argument*))
        ;;Apply TAB to each line.
        (t (process-region-as-lines-with-point
            #'(lambda (line)
                (send *editor-cursor* :set-pos 0)
                (funcall-key #\tab)))))
  nil)

(editor-bind-key '(#\control-x #\tab) indent-rigidly)

(defun indent-rigidly ()
  (process-region-as-lines-with-point
   #'(lambda (line)
       (send *editor-cursor* :set-pos 0)
       (send *editor-cursor* :forward-over-horizontal-white-space)
       (indent-line-to-column
        line (max& 0 (+& (edit-cursor-position-really *editor-cursor*)
                         *argument*))))))



(editor-bind-key #\meta-\\ delete-horizontal-space)

(defun delete-horizontal-space (&optional (bp *editor-cursor*))
  (unless (or argument-supplied? (not (=& *argument* 1)))
    (do ((chr))
        ((send bp :beginning-of-line?))
      (setq chr (send bp :get-char-backward))
      (unless (horizontal-white-space? chr)
        (send bp :advance-pos 1)
        (return t))))
  (do ((chr (send bp :get-char)
            (send bp :get-char)))
      ((not (horizontal-white-space? chr)))
    (send bp :delete-characters 1))
  nil)

(defun empty-line? (line)
  (loop with chars = (line-chars line)
        for i from 0 below (line-char-count line)
        for chr = (schar chars i)
        always (horizontal-white-space? chr)))

(editor-bind-key '(#\control-x #\control-o) delete-blank-lines)

(defun delete-blank-lines ()
  (let ((line (bp-line *editor-cursor*)) (q))
    (cond ((and (=& (line-char-count line) 0)   ;(empty-line? line)
                (or (not (setq q (line-previous line)))
                    (not (empty-line? q)))
                (setq q (line-next line))
                (not (empty-line? q)))
             ;Single blank line
             (send line :delete-chars-from-position 0)
             (send q :delete-line-separator))
          (t (loop while (and (line-next line) (empty-line? (line-next line)))
                   do (send (line-next line) :delete-chars-from-position 0)
                      (send (line-next line) :delete-line-separator))
             (loop for line = (bp-line *editor-cursor*)
                   while (and (empty-line? line)
                              (line-previous line)
                              (empty-line? (line-previous line)))
                   do (send (line-previous line) :delete-chars-from-position 0)
                   do (send line :delete-line-separator))
             (delete-horizontal-space))))
  nil)

(defun delete-blank-space (&optional (bp *editor-cursor*))
  (send bp :backward-over-white-space)
  (loop until (or (send bp :end-of-buffer?)
                  (not (white-space? (send bp :get-char))))
        do (send bp :delete-characters 1))
  (unless (or (send bp :beginning-of-buffer?)
              (send bp :end-of-buffer?)
              (char= right-paren (send bp :get-char))
              (char= left-paren (send bp :peek-char-backward)))
    (send bp :insert-char #\space))
  nil)

;;; In GSB's emacs we bind meta-slash with this command:
;(editor-bind-key #\meta-/ delete-blank-space)
;
;Also, it might be better to define control-x control-o
;to the more consistent function:
;(editor-defun-key '(#\control-x #\control-o) delete-blank-lines-proposal
; (delete-blank-space)
; (linefeed)
; nil)
;

(editor-bind-key #\meta-\) move-over-right-paren)

(defun move-over-right-paren ()
  (loop while
        (not (char= right-paren (send *editor-cursor* :get-char-forward))))
  (send *editor-cursor* :advance-pos -1)
  (delete-blank-space *editor-cursor*)
  (send *editor-cursor* :advance-pos 1)
  (linefeed)
  nil)
