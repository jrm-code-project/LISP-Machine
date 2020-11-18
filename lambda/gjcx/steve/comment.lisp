; -*- Mode:LISP; Package:STEVE; Readtable:CL; Base:10 -*-

;Tuesday the nineteenth of July, 1983; 10:50:58 pm
;Copyright (c) July 1983 by Christopher Eliot
; and Massachusetts Institute of Technology.
;Permission to copy all or part of this material is granted, provided
; that the copies are not made or distributed for resale, the MIT
; copyright notice and reference to the source file and the software
; distribution version appear, and that notice is given that copying
; is by permission of Massachusetts Institute of Technology.

;
;Comment manipulation functions.
;

(defvar comment-start)
(defvar comment-begin)
(defvar comment-end) ;Null string means end of line.
(defvar comment-column)
(defvar comment-multi-line)

(defun find-comment-in-line (line)
  (string-search-for-substring (line-chars line) comment-start
                               0 (line-char-count line)))

(defun bp-to-comment (bp)
  (let ((column (find-comment-in-line (bp-line bp))))
    (cond ((null column) nil)
          (t (send bp :set-pos column) t))))


(editor-bind-key #\meta-\; indent-for-comment)
(editor-bind-key #\control-\; indent-for-comment)

(defun indent-for-comment (&aux (bp *editor-cursor*) (line (bp-line bp)))
  (let ((column (find-comment-in-line line)))
    (cond ((null column)
           (send bp :set-pos (line-char-count (bp-line bp)))
           (delete-horizontal-space bp)
           (pad-to-column-with-tabs bp comment-column)
           (when (and (not (buffer-begin?))
                      (not (white-space? (send bp :peek-char-backward))))
             (send bp :insert-char #\tab))
           (send bp :insert-string comment-begin)
           (send bp :insert-string comment-end)
           (send bp :advance-pos (-& (string-length comment-end))))
          ((and (=& (string-length comment-start) 1)
                (<& (1+& column) (line-char-count line))
                (char= (char (line-chars line) (1+& column))
                       (char comment-start 0)))
           ;; Repeated comment.
           (cond ((and (<& (+& 2 column) (line-char-count line))
                       (char= (char (line-chars line) (+& 2 column))
                              (char comment-start 0)))
                  (send bp :set-pos (+& 3 column)))
                 (t (send bp :set-pos column)
                    (send bp :backward-over-horizontal-white-space)
                    (cond ((0p (bp-position bp)) ;Bp must be *editor-cursor*
                           (funcall (internal-editor-lookup-key #\tab))
                           (send bp :advance-pos 2))
                          (t (send bp :set-pos (+& 2 column)))))))
          ((0p column)
           (send bp :set-pos (+& column (string-length comment-start))))
          (t (send bp :set-pos column)
             (delete-horizontal-space bp)
             (pad-to-column-with-tabs bp comment-column)
             (when (and (not (buffer-begin?))
                        (not (white-space? (send bp :peek-char-backward))))
               (send bp :insert-char #\tab))
             (send bp :advance-pos (string-length comment-start))))
    nil))

(defun pad-to-column-with-tabs (point column)
  (let ((hpos (edit-cursor-position-really point)))
    (cond ((>& hpos column) (send point :insert-char #\tab))
          (t (let ((tabs (-& (/& column 8) (/& hpos 8))))
               (loop repeat tabs do (send point :insert-char #\tab))
               (loop repeat (if (+p tabs) (-& column (*& (/& column 8) 8))
                                (-& column hpos))
                     do (send point :insert-char #\space)))))))

(editor-bind-key #\control-meta-\; kill-comment)

(defun kill-comment ()
  (when (bp-to-comment *editor-cursor*)
    (send *editor-cursor* :backward-over-horizontal-white-space)
    (kill-one-line-from-bp *editor-cursor*)
    (send *editor-cursor* :delete-chars-from-position))
  nil)

(editor-bind-key '(#\control-x #\;) set-comment-column)

(defun set-comment-column ()
  (setq comment-column (edit-cursor-position-really *editor-cursor*))
  (with-notify-line-remaining
   (format terminal-io "Comment Column is ~a" comment-column))
  nil)

(defun null-comment-this-line ()        ;bar
  (and (bp-to-comment *editor-cursor*)  ;foo
       (let ((line (bp-line *editor-cursor*))
             (position (bp-position *editor-cursor*)))
         (or (and (=& (line-char-count line) (+& (string-length comment-begin)
                                                 (string-length comment-end)
                                                 position))
                  (send line :match-string comment-begin position)
                  (send line :match-string comment-end
                        (+& position (string-length comment-begin))))
             (and (=& (line-char-count line) (+& (string-length comment-start)
                                                 (string-length comment-end)
                                                 position))
                  (send line :match-string comment-start position)
                  (send line :match-string comment-end
                        (+& position (string-length comment-start))))))))

(editor-bind-key #\meta-n down-comment-line)

(defun down-comment-line ()
  (when (null-comment-this-line) (kill-comment))
  (when (null (line-next (bp-line *editor-cursor*)))
    (send (bp-line *editor-cursor*) :insert-line ""))
  (send *editor-cursor* :to-beginning-of-next-line)
  (indent-for-comment)
  nil)

(editor-bind-key #\meta-p up-comment-line)

(defun up-comment-line ()
  (when (null-comment-this-line) (kill-comment))
  (when (null (line-previous (bp-line *editor-cursor*)))
    (ed-warn :at-start))
  (send *editor-cursor* :to-end-of-previous-line)
  (indent-for-comment)
  nil)

(editor-bind-key #\meta-j indent-new-comment-line)

(defun indent-new-comment-line ()
  (cond ((0p comment-multi-line)
         (or (send (bp-line *editor-cursor*)
                   :match-string comment-end (bp-position *editor-cursor*))
             (send *editor-cursor* :insert-string comment-end))
         (crlf)
         (indent-for-comment))
        (t (linefeed)))
  nil)

(editor-bind-key #\meta-linefeed indent-new-comment-line)
(editor-bind-key #\control-meta-linefeed indent-new-comment-line)
(editor-bind-key #\control-meta-j indent-new-comment-line)

;Add a prefix to every line in a region.

(defun prefix-region-lines (prefix)
  (multiple-value-bind (bp1 bp2) (order-bps *editor-cursor* (get-mark))
    (loop with limit-line = (line-next (bp-line bp2))
          for line first (bp-line bp1) then (line-next line)
          until (or (null line) (eq line limit-line))
          do (send line :insert-string 0 prefix))))

(defun unprefix-region-lines (prefix)
  (multiple-value-bind (bp1 bp2) (order-bps *editor-cursor* (get-mark))
    (loop with limit-line = (line-next (bp-line bp2))
          with length = (string-length prefix)
          for line first (bp-line bp1) then (line-next line)
          until (or (null line) (eq line limit-line))
          if (send line :match-string prefix 0)
          do (send line :delete-characters 0 length))))

(bindmetax comment-region comment-region)

(defun comment-region ()
  (prefix-region-lines comment-start))

(bindmetax uncomment-region uncomment-region)

(defun uncomment-region ()
  (unprefix-region-lines comment-start))



;
;Underlining and font changing.
;

(editor-bind-key #\meta-# change-font-word)

(defun change-font-word ()
 (let ((bp (copy-bp *editor-cursor*)))
  (cond ((null (argument?))
         ;;Move last font change 4forward one* word.
         (send bp :backward-to-char #\^F)
         (let ((change (send bp :get-char)))
          (send bp :advance-pos -1)
          (send bp :delete-characters 2)
          (send bp :forward-over-not-word)
          (send bp :forward-over-word)
          (send bp :insert-char #\control-f)
          (send bp :insert-char change)))
        ((+p *argument*)
         ;;Change font of last word.
         (send bp :backward-over-not-word)
         (send bp :backward-over-word)
         (send bp :insert-char #\control-f)
         (send bp :insert-char (%digit-weight-to-char *argument*))
         (send bp :forward-over-word)
         (send bp :insert-string "*"))
        (t ;;Move last font change backward one word.
           (send bp :backward-to-char #\^F)
           (let ((change (send bp :get-char)))
            (send bp :advance-pos -1)
            (send bp :delete-characters 2)
            (send bp :backward-over-not-word)
            (send bp :backward-over-word)
            (cond ((char= (send bp :peek-char-backward) #\^F)
                   (cond ((eq (char= (send bp :get-char) #\*)
                              (char= change #\*))
                          (send (bp-line bp) :replace-char
                                (bp-position bp) change))
                         (t (send bp :advance-pos -1)
                            (send bp :delete-characters 2))))
                  (t (send bp :insert-char #\control-f)
                     (send bp :insert-char change))))))
  (send bp :send-if-handles :expire)
  nil))

;Now is the time for all good men to come to the aid
;of their party.

(editor-bind-key '(#\control-x #\#) change-font-region)

(defun change-font-region ()
 (multiple-value-bind (bp1 bp2) (order-bps *editor-cursor* (get-mark))

  (cond ((eq (bp-line bp1) (bp-line bp2))
         (delete-fonts-in-line (bp-line bp1)
                               (bp-position bp1) (bp-position bp2)))
        (t (let ((line))
            (delete-fonts-in-line (bp-line bp1) (bp-position bp1))
            (setq line (bp-line bp2))
            (delete-fonts-in-line line 0 (bp-position bp2))
            (loop for nline first (line-next (bp-line bp1))
                  then (line-next nline)
                  until (eq nline line)
                  do (delete-fonts-in-line nline)))))
  (unless (-p *argument*)
   (send bp1 :insert-char #\^F)
   (send bp1 :insert-char (%digit-weight-to-char *argument*))
   (send bp1 :advance-pos -2) ;so repeated use deletes this.
   (send bp2 :insert-string "*")))
 nil)

(defun delete-fonts-in-line (line &optional (p1 0) (p2 (line-char-count line)))
 (loop for offset upfrom 0 by 2
       for pos = (%string-posq #\^F (line-chars line) p1 (-& p2 p1 offset))
       while pos
       do (send line :delete-char pos)
       do (send line :delete-char pos)))
