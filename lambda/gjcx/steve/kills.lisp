;-*- Mode:LISP; Package:STEVE; Readtable:CL; Base:10 -*-

;Copyright (c) May 1983 by Christopher Eliot
; and Massachusetts Institute of Technology.
;Permission to copy all or part of this material is granted, provided
; that the copies are not made or distributed for resale, the MIT
; copyright notice and reference to the source file and the software
; distribution version appear, and that notice is given that copying
; is by permission of Massachusetts Institute of Technology.

;
;
;Killing commands.
;
;The kill ring is a vector of vectors.
;Each entry is a vector of strings.
;

(defparameter kill-ring-size 8)

(defparameter kill-ring
  (make-vector
    kill-ring-size
    :initial-element (make-vector 1 :initial-element (make-string 0))))

(defvar kill-ring-index 0)

(defun push-kill (carcass)
  (setq kill-ring-index (1+& kill-ring-index))
  (if (>=& kill-ring-index kill-ring-size)
      (setq kill-ring-index 0))
  (setf (aref kill-ring kill-ring-index) carcass))

(defun pop-kill ()
  (prog1 (or (svref kill-ring kill-ring-index) (ed-lose "No kill to pop"))
         (loop do (setq kill-ring-index (1-& kill-ring-index))
               do (when (-p kill-ring-index)
                    (setq kill-ring-index (1-& kill-ring-size)))
               until (svref kill-ring kill-ring-index))))

(defun get-kill (&optional (offset 0))
  (svref kill-ring (mod& (-& kill-ring-index offset) kill-ring-size)))

(defsubst string-copy (string length)
  (%string-replace (make-string length) string 0 0 length))

(defun count-lines-between (bp1 bp2)
  (count-lines (bp-line bp1) (bp-line bp2)))

(defun count-lines (line target)
  (do ((line line (line-next line))
        (count 1 (1+& count)))
      ((eq line target) count)))

(defvar *last-kill-command-count* -1000)

(defvar *last-un-kill-command-count* -1000)

(defun push-or-append-kill-after (carcass)
  (if (=& (1+& *last-kill-command-count*) *editor-command-count*)
      (push-kill (append-carcasses (pop-kill) carcass))
    (push-kill carcass))
  (setq *last-kill-command-count* *editor-command-count*))

(defun push-or-append-kill-before (carcass)
  (if (=& (1+& *last-kill-command-count*) *editor-command-count*)
      (push-kill (append-carcasses carcass (pop-kill)))
    (push-kill carcass))
  (setq *last-kill-command-count* *editor-command-count*))

(defun kill-between-marks (bp1 bp2 &aux lines)
  (multiple-value (bp1 bp2) (order-bps bp1 bp2))
  (setq lines (count-lines-between bp1 bp2))
  (let ((carcass (make-vector lines :initial-element nil)))
    (if (=& lines 1)
        (setf (aref carcass 0)
               (string-subseq (line-chars (bp-line bp1))
                              (bp-position bp1)
                              (-& (bp-position bp2) (bp-position bp1))))
      (setf (aref carcass 0) ;First line.
            (string-subseq (line-chars (bp-line bp1))
                           (bp-position bp1)
                           (-& (line-char-count (bp-line bp1))
                               (bp-position bp1)))
            (aref carcass (1-& lines)) ;last line.
            (string-subseq (line-chars (bp-line bp2)) 0 (bp-position bp2))))
    ;Now get complete lines between.
    (loop for line first (line-next (bp-line bp1)) then (line-next line)
          for i from 1 below (1-& lines)
          until (null line)
          do (setf (aref carcass i)
                    (string-subseq
                     (line-chars line) 0 (line-char-count line))))
    (if (eq *editor-cursor* bp1)
        (push-or-append-kill-after carcass)
      (push-or-append-kill-before carcass))))

(defun delete-between-marks (bp1 bp2)
  (multiple-value (bp1 bp2) (order-bps bp1 bp2))
  (if (eq (bp-line bp1) (bp-line bp2))
      (send bp1 :delete-characters (-& (bp-position bp2) (bp-position bp1)))
      (progn (send bp1 :delete-chars-from-position)
             (send bp2 :delete-characters (-& (bp-position bp2)))
             ;;Clobber link from deleted lines so it can't get us.
             (setf (line-next (line-previous (bp-line bp2))) nil)
             (send (bp-line bp1) :set-next (bp-line bp2))
             (send (bp-line bp2) :delete-line-separator))))

(defun kill-lines-from-bp (bp1 lines) ;Forward only.
  (let ((carcass (make-vector (1+& lines) :initial-element nil)))
    (setf (aref carcass 0) (string-subseq (line-chars (bp-line bp1))
                                          (bp-position bp1)
                                          (line-char-count (bp-line bp1)))
          (aref carcass lines) "")
    ;Now get complete lines.
    (loop for i from 1 below lines
          for line first (line-next (bp-line bp1)) then (line-next line)
          until (null line)
          do (setf (aref carcass i) (string-copy (line-chars line)
                                                 (line-char-count line))))
    (push-or-append-kill-after carcass)))

(defun backward-kill-lines-from-bp (bp1 lines) ;Backward only.
  (setq lines (abs& lines))
  (let ((carcass (make-vector (1+& lines) :initial-element nil)))
    (setf (aref carcass lines) (string-subseq (line-chars (bp-line bp1))
                                              0 (bp-position bp1))
          (aref carcass 0) "")
    ;Now get complete lines.
    (loop for i from (1-& lines) above 0
          for line first (line-previous (bp-line bp1))
          then (line-previous line)
          until (null line)
          do (setf (aref carcass i) (string-copy (line-chars line)
                                                 (line-char-count line))))
    (push-or-append-kill-before carcass)))

(defun kill-one-line-from-bp (bp1) ;Forward only.
  (let ((carcass (make-vector 1)))
    (setf (aref carcass 0)
          (string-subseq (line-chars (bp-line bp1))
                         (bp-position bp1)
                         (-& (line-char-count (bp-line bp1))
                             (bp-position bp1))))
    (push-or-append-kill-after carcass)))

(defun backward-kill-one-line-from-bp (bp1) ;backward only.
  (let ((carcass (make-vector 1)))
    (setf (aref carcass 0)
          (string-subseq (line-chars (bp-line bp1))
                         0 (bp-position bp1)))
    (push-or-append-kill-before carcass)))

(defun kill-characters-from-bp (bp chars)
  (let ((bp2 (copy-bp bp)))
    (send bp2 :advance-pos chars) ;works for negative chars.
    (kill-between-marks bp bp2)))

(defun kill-and-delete-from-bp (bp chars)
  (let ((bp2 (copy-bp bp)))
    (send bp2 :advance-pos chars)
    (kill-between-marks bp bp2)
    (delete-between-marks bp bp2)
    (send (bp-line bp2) :remove-bp bp2)))

(defun append-carcasses (c1 c2)
  (let* ((len1 (vector-length c1))
          (len2 (vector-length c2))
          (result (make-vector (+& len1 len2 -1))))
    (loop for i from 0 below (1-& len1)
          do (setf (aref result i) (aref c1 i)))
    (setf (aref result (1-& len1))
          (string-append (aref c1 (1-& len1)) (aref c2 0)))
    (loop for i from 1 below len2
          for j upfrom len1
          do (setf (aref result j) (aref c2 i)))
    result))



;
;Editor Kill commands.
;

(editor-bind-key #\meta-w copy-region)

(defun copy-region ()
  (kill-between-marks (get-mark) *editor-cursor*)
  nil)

(editor-bind-key #\control-w kill-region)

(defun kill-region ()
  (kill-between-marks (get-mark) *editor-cursor*)
  (delete-between-marks (pop-mark) *editor-cursor*)
  nil)

(editor-bind-key '(#\control-x #\X) put-q-reg)

(defun put-q-reg (&optional (var nil))
  (setq var (if (null var)
                (mx-prompter #'read "Put to Variable (Q reg): ")
              (intern (string-upcase var))))
  (kill-between-marks (pop-mark) *editor-cursor*)
  (set var (pop-kill))
  nil)

(editor-bind-key '(#\control-x #\G) get-q-reg)

(defun get-q-reg (&optional (var nil))
  (setq var (if (null var)
                (mx-prompter #'read  "Get from Variable (Q reg): ")
              (intern (string-upcase var))))
  (cond ((stringp (symeval var))
          (send *editor-cursor* :insert-string (symeval var)))
        ((vectorp (symeval var))
          (un-kill (symeval var)))
        (t (ed-lose "Variable contents illegal as Q register")))
  nil)

(editor-bind-key #\control-k kill-lines)

;;; This should go in EM, but...

(defun rest-of-line-empty? (point)
  (with-bp (bp point)
    (send bp :forward-over-horizontal-white-space)
    (send bp :end-of-line?)))

(defun kill-lines ()
  (cond ((null (argument?))
          (not-buffer-end)
          (cond ((send *editor-cursor* :end-of-line?)
                 (let ((carcass (make-vector 2)))
                   (setf (aref carcass 0) "")
                   (setf (aref carcass 1) "")
                   (push-or-append-kill-after carcass)
                   (send *editor-cursor* :delete-characters 1)))
                ((rest-of-line-empty? *editor-cursor*)
                 (let ((argument-supplied? t)
                       (*argument* 1))
                   (kill-lines)))
                (:else
                  (kill-one-line-from-bp *editor-cursor*)
                  (send *editor-cursor* :delete-chars-from-position))))
        ((+p *argument*)
          (not-buffer-end)
          (kill-lines-from-bp *editor-cursor* *argument*)
          (loop for i from 1 to *argument* ;Now delete the lines.
                do (send *editor-cursor* :delete-chars-from-position)
                until (buffer-end?)
                do (send *editor-cursor* :delete-char)
                until (buffer-end?)))
        ((0p *argument*)
          (not-buffer-begin)
          (backward-kill-one-line-from-bp *editor-cursor*)
          (send *editor-cursor* :delete-characters
                (-& (bp-position *editor-cursor*))))
        (t (not-buffer-begin)
           (backward-kill-lines-from-bp *editor-cursor* *argument*)
           (loop for i from 1 to (-& *argument*)
                 do (send *editor-cursor* :delete-characters
                          (-& (bp-position *editor-cursor*)))
                 until (buffer-begin?)
                 do (send (bp-line *editor-cursor*) :delete-line-separator)
                 until (buffer-begin?))))
  nil)

(editor-bind-key #\control-y un-kill)

(defun un-kill (&optional (carcass nil arg?))
  (when (null arg?)
    (setq carcass (if (real-arg-sup?)
                      (get-kill (1-& *argument*)) ;1-origined
                      (get-kill))))
  (unless (null carcass)
    (let ((mark (send *editor-cursor* :get-mark)))
      (setq *last-un-kill-command-count* *editor-command-count*)
      (loop with max-index = (1-& (vector-length carcass))
            for i from 0 to max-index
            do (unless (null (aref carcass i))
               (send (edit-cursor-line *editor-cursor*)
                     :insert-string (edit-cursor-position *editor-cursor*)
                     (aref carcass i))
               (when (<& i max-index)
                 (send (edit-cursor-line *editor-cursor*)
                       :break (edit-cursor-position *editor-cursor*)))))
      (push-mark *editor-cursor*)
      (send *editor-cursor* :goto-mark mark)
      (unless (and (null arg?) (C-U-only?))
        (exchange-point-and-mark))))
  nil)

(editor-bind-key #\meta-y un-kill-pop)

(defun un-kill-pop ()
  (when (=& (1+& *last-un-kill-command-count*) *editor-command-count*)
    (delete-between-marks (pop-mark) *editor-cursor*))
  (pop-kill)
  (un-kill))

(editor-bind-key #\control-meta-w append-next-kill)

(defun append-next-kill ()
  (setq *last-kill-command-count* *editor-command-count*)
  nil)

(defun do-not-append-next-kill ()
  (setq *last-kill-command-count* -100)
  nil)

(editor-bind-key #\meta-d kill-word)

(defun kill-word (&optional (how-many *argument*))
  (if (-p how-many)
      (backward-kill-word (-& how-many))
    (let ((bp2 (copy-bp *editor-cursor*)))
      (do ((i 1 (1+& i)))
          ((>& i how-many))
        (send bp2 :forward-over-not-word)
        (send bp2 :forward-over-word))
      (kill-between-marks *editor-cursor* bp2)
      (delete-between-marks *editor-cursor* bp2)
      nil)))

(editor-bind-key #\meta-rubout backward-kill-word)

(defun backward-kill-word (&optional (how-many *argument*))
  (if (-p how-many)
      (kill-word (-& how-many))
    (let ((bp2 (copy-bp *editor-cursor*)))
      (do ((i 1 (1+& i)))
          ((>& i how-many))
        (send bp2 :backward-over-not-word)
        (send bp2 :backward-over-word))
      (kill-between-marks *editor-cursor* bp2)
      (delete-between-marks *editor-cursor* bp2)
      nil)))

(editor-bind-key #\control-meta-k kill-sexp)

(defun kill-sexp (&optional (how-many *argument*))
  (cond ((-p how-many) (backward-kill-sexp (-& how-many)))
        (t (let ((bp2 (copy-bp *editor-cursor*)))
             (loop for i from 1 to how-many
                   do (bp-forward-over-sexp bp2))
             (kill-between-marks *editor-cursor* bp2)
             (delete-between-marks *editor-cursor* bp2))))
  nil)

(editor-bind-key #\control-meta-rubout backward-kill-sexp)

(defun backward-kill-sexp (&optional (how-many *argument*))
  (cond ((-p how-many) (kill-sexp (-& how-many)))
        (t (let ((bp2 (copy-bp *editor-cursor*)))
             (loop for i from 1 to how-many
                   do (bp-backward-over-sexp bp2))
             (kill-between-marks *editor-cursor* bp2)
             (Delete-between-marks *editor-cursor* bp2))))
  nil)

;;; The following functions shouldn't be in this file but will
;;; ramain here until a better home can be found

(defun process-words-as-strings (function)
  (setq *last-kill-command-count* -1000) ;ickypoo hack.
  (cond ((-p *argument*)
          (setq *argument* (-& *argument*))
          (preserving-point
            (loop repeat *argument*
                  do (send *editor-cursor* :backward-over-not-word)
                  (send *editor-cursor* :backward-over-word))
            (process-words-as-strings-1 function)))
        (t
          (process-words-as-strings-1 function))))

(defun process-words-as-strings-1 (function)
  (kill-word *argument*)
  (loop with carcass = (get-kill)
        for i from 0 below (vector-length carcass)
        do (setf (aref carcass i) (funcall function (aref carcass i)))
        finally (un-kill carcass)
        finally (pop-kill))
  (pop-mark)
  nil)

(defun process-region-as-chars (function)
  (multiple-value-bind (bp1 bp2) (order-bps *editor-cursor* (get-mark))
    (cond ((eq (bp-line bp1) (bp-line bp2))
            (send (bp-line bp1) :translate function
                  (bp-position bp1) (bp-position bp2)))
          (t (let ((line))
               (setq line (bp-line bp1))
               (send line :translate function
                     (bp-position bp1) (line-char-count line))
               (setq line (bp-line bp2))
               (send line :translate function 0 (bp-position bp2))
               (loop for nline first (line-next (bp-line bp1))
                     then (line-next nline)
                     until (eq nline line)
                     do (send nline :translate function
                              0 (line-char-count nline)))))))
 nil)

;;;This is not quite the same as the (unimplemented) CommonLisp
;;;function STRING-CAPITALIZE.
(defun string-capcase (string &optional (skip 0))
  (loop for i from skip below (string-length string)
        for char = (char string i)
        if (word-char? char)
        return
        (progn (setf (char string i) (char-upcase char))
               (loop for j from (1+& i) below (string-length string)
                     if (not (word-char? (char string j)))
                     return (string-capcase string j)
                     do (setf (char string j)
                              (char-downcase (Char String j))))))
  string)

(editor-bind-key #\meta-c uppercase-initial)

(defun uppercase-initial ()
  (process-words-as-strings #'string-capcase)
  nil)

(editor-bind-key #\meta-u uppercase-word)

(defun uppercase-word ()
  (process-words-as-strings #'string-upcase)
  nil)

(editor-bind-key #\meta-l lowercase-word)

(defun lowercase-word ()
  (process-words-as-strings #'string-downcase)
  nil)

(editor-bind-key '(#\control-x #\control-u) uppercase-region)

(defun uppercase-region ()
  (process-region-as-chars #'char-upcase)
  nil)

(editor-bind-key '(#\control-x #\page) lowercase-region)

(defun lowercase-region ()
  (process-region-as-chars #'char-downcase)
  nil)
