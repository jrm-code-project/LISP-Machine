; -*- package:steve; readtable:cl; mode:lisp; -*-

;Copyright (c) May 1983 by Christopher Eliot
; and Massachusetts Institute of Technology.
;Permission to copy all or part of this material is granted, provided
; that the copies are not made or distributed for resale, the MIT
; copyright notice and reference to the source file and the software
; distribution version appear, and that notice is given that copying
; is by permission of Massachusetts Institute of Technology.

;Search functions.
;

(defvar *last-search-string* "")

(defvar saved-search-start-mark nil)

(editor-bind-key #\control-s incremental-search)

(defun incremental-search ()
  (setq saved-search-start-mark (copy-bp *editor-cursor*))
  (with-query-line
   (catch 'search-done
     (i-search-internal "" :forward nil 0)))
  (send saved-search-start-mark :expire)
  nil)

(editor-bind-key #\control-r reverse-search)

(defun reverse-search ()
  (setq saved-search-start-mark (copy-bp *editor-cursor*))
  (with-query-line
   (catch 'search-done
     (i-search-internal "" :backward nil 0)))
  (send saved-search-start-mark :expire)
  nil)

(defun i-search-internal (string dir failed? chars-just-read)
  (unless (or (eq dir :backward) failed?)
    (send *editor-cursor* :advance-pos-no-error
          (-& chars-just-read (string-length string))))
  (loop with status = (and (null failed?)
                           (search-buffer-for-string string
                                                     *editor-cursor* dir))
        and mark = (send *editor-cursor* :get-mark)
        and stand = nil
        initially (when (and (null failed?) (null status) (eq dir :forward))
                    ;;Fix adjustment in case of failure.
                    (send *editor-cursor* :advance-pos-no-error
                          (-& (string-length string) chars-just-read)))
        do (when (and (null status) (null failed?))
             (send terminal-io :write-char #\bell))
           (unless (type-ahead-p)
             (with-query-line-remaining
               (when (null status)
                 (oustr "Failing " terminal-io))
               (if (eq dir :backward)
                   (oustr "Reverse I-Search: " terminal-io)
                 (oustr "I-Search: " terminal-io))
               (oustr string terminal-io)
               (make-screen-image)))
           (unless (type-ahead-p)
             (if (null status)
                 (send (edit-cursor-window *editor-cursor*)
                       :make-cursor-visible)
               (setq stand (cond ((null status) 1)
                                 ((eq dir :backward) (string-length string))
                                 (t (-& (string-length string)))))
               (set-string-graphic-rendition :standout t *editor-cursor* stand)
               (send (edit-cursor-window *editor-cursor*) :make-cursor-visible)
               (peek-char&save)
               (set-string-graphic-rendition :standout nil
                                             *editor-cursor* stand)))
        thereis (null (i-search-parser string dir (not status)))
        ;;Rubout.
        do (send *editor-cursor* :goto-mark mark)))

(defun i-search-parser (string dir failed?)
  (let ((chr (read-char&save terminal-io)))
    (case chr
      (#\rubout nil)
      (#\^S (cond ((and (string-equal "" string) (eq dir :forward))
                   (setq string *last-search-string*)
                   (i-search-internal
                    string :forward (and failed? (eq dir :forward))
                    (string-length string)))
                  (t (unless (and failed? (eq dir :forward))
                       (send *editor-cursor* :advance-pos-no-error 1))
                     (i-search-internal
                      string :forward (and failed? (eq dir :forward)) 0))))
      (#\^R (cond ((and (string-equal "" string) (eq dir :backward))
                   (setq string *last-search-string*)
                   (i-search-internal
                    string :backward (and failed? (eq dir :backward))
                    (string-length string)))
                  (t (unless (and failed? (eq dir :backward))
                       (send *editor-cursor* :advance-pos-no-error -1))
                     (i-search-internal
                      string :backward (and failed? (eq dir :backward)) 0))))
      (#\^Q (i-search-internal
             (string-append string (string (read-char&save terminal-io)))
             dir failed? 1))
      (#\^G (goto-mark saved-search-start-mark)
            (setq *last-search-string* string)
            (ed-warn "Aborted Search"))
      (t (if (or (graphic-charp chr)
                 (char= chr #\tab)
                 (char= chr #\return))
             (i-search-internal
              (string-append string (string chr)) dir failed? 1)
             (progn (or (char= chr #\altmode)
                        (unread-char&save terminal-io))
                    (setq *last-search-string* string)
                    (throw 'search-done t)))))))


;
;Now the actual search primatives.
;

;This actually changes the POINT it receives as an argument.

(defun search-buffer-for-string (string point dir)
  (if (eq dir :backward)
      (search-back string point)
      (search-forward string point)))

(defun search-forward (string point &aux p1 p2 chars length line-length
                       (string-len (string-length string)))
  (cond ((0p string-len))
        ((%string-posq #\return string 0 string-len)
         (multi-line-search-forward (string-lineify string) point))
        (t (block search-forward
            (do ((line (bp-line point) (line-next line))
                 (pos (bp-position point) 0)
                 (char1 (char-upcase (char string 0)))
                 (char2 (char-downcase (char string 0))))
                ((null line) nil)
               (setq chars (line-chars line)
                     line-length (line-char-count line))
               (do ()
                   (nil)
                  (setq p1 (%string-posq char1 chars pos (-& line-length pos))
                        p2 (%string-posq char2 chars pos (-& line-length pos)))
                  (when (not (or p1 p2)) (return nil))
                  (setq pos (min& (or p1 p2) (or p2 p1)))
                  (when (<& (-& line-length pos) string-len) (return nil))
                  (when (string-equal chars string (1+& pos) 1
                                      (+& string-len pos) string-len)
                    (send point :move line pos)
                    (send point :advance-pos-no-error string-len)
                    (return-from search-forward t))
                  (setq pos (1+& pos))))))))

(defun string-lineify (string)
  (loop with len = (string-length string)
        for p1 first 0 then (1+& p2)
        while (<& p1 len)
        for p2 = (or (%string-posq #\return string p1 (-& len p1))
                     len)
        collect (substring string p1 p2)))

;This is a real hairy function.
;It is very similar to multi-line-search-back (below), (In fact this was
;used as a template.) The inversion is complicated also. (See comments
;before it.) Note line " for p1 first (bp-position point) then 0" is
;changed and the logical test "(>= (- ...) ...)" changes also.
;That test is used to handle the partial line that we start with.
(defun multi-line-search-forward (s-list point)
  (loop with s1 = (car s-list)
        initially (setq s-list (cdr s-list))
        with s1len = (string-length s1)
        for line first (bp-line point) then (line-next line)
        for p1 first (bp-position point) then 0
        while line
        thereis
        (and (>=& (-& (line-char-count line) p1) s1len)
             (string-equal (line-chars line) s1
                           (-& (line-char-count line) s1len)
                           0 (line-char-count line) s1len)
             (loop for (s . tail) on s-list
                   for l first (line-next line) then (line-next l)
                   while l
                   for llen = (line-char-count l)
                   for slen = (string-length s)
                   always
                   (if (not (null tail)) ;Last string is a separate case.
                       (and (=& llen slen)
                            (string-equal (line-chars l) s 0 0 llen slen))
                       (and (>=& llen slen)
                            (string-equal (line-chars l) s 0 0 slen slen)))
                   finally (cond ((null l)
                                  (when (null (line-next line)) (return nil))
                                  (send point :move (line-next line) 0))
                                 (t (send point :move l slen)))))))

;This seems to assume that the point is the last possible place to
;match the first character in the string. I guess this is to avoid having
;to back up by the length of the string before matching.

(defun search-back (string point &aux (string-len (string-length string)))
  (cond ((0p string-len))
        ((%string-posq #\return string 0 string-len)
         (multi-line-search-back (string-lineify string) point))
        (t (block search-tag
             (loop with char1 = (char string 0)
                   for line first (bp-line point) then (line-previous line)
                   while line
                   for pos first (min& (1+& (bp-position point))
                                       (line-char-count line))
                   then (line-char-count line)
                   for chars = (line-chars line)
                   for line-length = (line-char-count line)
                   do (do ()
                          ((-p pos) nil)
                          (setq pos (string-reverse-search-char
                                     char1 chars pos))
                          (when (null pos) (return nil))
                          (when (and (>=& (-& line-length pos) string-len)
                                     (loop for i from 1 below string-len
                                           for p from (1+& pos)
                                           always (char-equal (char chars p)
                                                              (char string i)))
                                     (send point :move line pos)
                                     (return-from search-tag t)))))))))

;This is super hairy.
;the s-list is a list of strings. There is an implicit CR between
;each string. (Often the first string sill be "".)
;The first clause in the AND in the THEREIS does NOT check to see
;if there are enough characters in the line. It checks to see if
;point is close enough to the end so that when the string S1 is
;matched to the suffix of the line, the beginning of the match is on or
;before point.
;The STRING-EQUAL checks that the suffixes match.
;
;There seems to be a bogus double bug. This function can't do single line
;matches, because the string-equal is placed so it assumes there
;is a CR after the first string in s-list, even though the definition above
;says that CRs are between strings. The bug is that there is an assumed CR
;after a s-list of length one, and the function above returns such s-lists.
;all is cool though as long as this function multi-line-search-forward
;don't get changed.
(defun multi-line-search-back (s-list point)
  (loop with s1 = (car s-list)
        initially (setq s-list (cdr s-list))
        with s1len = (string-length s1)
        for line first  (bp-line point) then (line-previous line)
        while line
        for p1 first (bp-position point) then (line-char-count line)
        thereis
        (and (<=& (-& (line-char-count line) p1) s1len)
             (string-equal (line-chars line) s1
                           (-& (line-char-count line) s1len)
                           0 (line-char-count line) s1len)
             (loop for (s . tail) on s-list
                   for l first (line-next line) then (line-next l)
                   while l
                   for llen = (line-char-count l)
                   for slen = (string-length s)
                   always
                   (if (not (null tail)) ;Last string is a separate case.
                       (and (=& llen slen)
                            (string-equal (line-chars l) s 0 0 llen slen))
                       (and (>=& llen slen)
                            (string-equal (line-chars l) s 0 0 slen slen)))
                   finally (send point :move line (-& (line-char-count line)
                                                      s1len))))))
