;;; -*- Mode:LISP; Package:USER; Base:10; Readtable:ZL -*-

#||

Copyright LISP Machine, Inc. 1986
   See filename "Copyright.Text" for
licensing and release information.

*********************************************************
*********************************************************
*** NOTE: This is an EXAMPLE, not LMI supported code. ***
*** information contained in this example is subject  ***
*** to change without notice. The ways of doing       ***
*** the things contained in the example may change    ***
*** between system releases. Some techniques which    ***
*** are mere examples in one release may become built ***
*** in system features in the next release. Use good  ***
*** judgement when copying these techniques. Most     ***
*** examples have been motivated by specific customer ***
*** requests, and may not be the best engineered      ***
*** or most efficient solution for someone else.      ***
*********************************************************
*********************************************************


Search for a set of strings in a source file.
(SOMETIME CONSIDERABLY BEFORE) 27-May-86 19:03:13 -GJC

||#

(defun search-files (specification strings &optional &key output-file (window 3)
                     (version :newest) (type :lisp))
  (if output-file
      (with-open-file (stream output-file :direction :output)
        (search-files-1 specification
                        (if (atom strings) (list strings) strings)
                        stream
                        window
                        version
                        type))
    (search-files-1 specification
                    (if (atom strings) (list strings) strings)
                    standard-output
                    window
                    version
                    type)))

(defun search-files-1 (specification strings output-stream window version type)
  (OR (FBOUNDP'MAP-ALL-FILES) (LOAD "SYS:EXAMPLES;FILE-UTILS" :SET-DEFAULT-PATHNAME NIL))
  (COND ((SYMBOLP SPECIFICATION)
         (DOLIST (FILE (SI:SYSTEM-SOURCE-FILES SPECIFICATION))
           (SEARCH-ONE-FILE (SEND (OPEN FILE :DIRECTION NIL)
                                  :TRUENAME)
                            STRINGS OUTPUT-STREAM WINDOW)))
        ('ELSE
         (MAP-ALL-FILES #'(LAMBDA (FILE)
                            (SEARCH-ONE-FILE FILE STRINGS OUTPUT-STREAM WINDOW))
                        specification
                        :version version
                        :type type))))


(defun multiple-string-search (strings string start end)
  (dolist (s strings)
    (if (string-search s string start end)
        (return-from multiple-string-search t))))

(DEFUN SEARCH-ONE-FILE (FILE STRINGS OUTPUT-STREAM WINDOW)
  (FORMAT OUTPUT-STREAM "~&In ~A~%" FILE)
  (LET ((LINE-NUMBER 0)
        (LAST-FOUND-LINE))
    (MAP-OVER-FILE-LINES
      #'(LAMBDA (STRING START END)
          (INCF LINE-NUMBER)
          (COND ((MULTIPLE-STRING-SEARCH STRINGS STRING START END)
                 (SETQ LAST-FOUND-LINE LINE-NUMBER)
                 (FORMAT OUTPUT-STREAM "~4D* " LINE-NUMBER)
                 (SEND OUTPUT-STREAM :LINE-OUT STRING START END))
                ((AND LAST-FOUND-LINE
                      (< (- LINE-NUMBER LAST-FOUND-LINE) WINDOW))
                 (FORMAT OUTPUT-STREAM "~4D> " LINE-NUMBER)
                 (SEND OUTPUT-STREAM :LINE-OUT STRING START END))))
      FILE)))


(defun map-over-file-lines (f filename)
  "Function F gets called on STRING START END for each line in the file"
  ;; This is maximally bummed. Better than using :string-line-in
  ;; because it doesnt copy unless on boundaries.
  (with-open-file (s filename)
    (cond ((equal 8 (get s :byte-size))
           (map-over-stream-lines f s))
          ('else
           (format t "Byte size # 8, skipping")))))

(defun map-over-stream-lines (f from-stream)
  (prog (offset limit start end leftover leftoverp easy-lines hard-lines buf)
        (setq easy-lines 0 hard-lines 0)
     read-buffer
        (multiple-value (buf offset limit)
          (send from-stream :read-input-buffer))
        (UNLESS BUF
          (WHEN LEFTOVERP
            (FUNCALL F LEFTOVER 0 (FILL-POINTER LEFTOVER))
            (INCF HARD-LINES))
          (return (values easy-lines hard-lines)))
        (setq start offset)
     next-line
        (setq end (%string-search-char #\RETURN buf start limit))
        (cond ((null end)
               (or leftover
                   (setq leftover (make-array 300
                                              :element-type 'string-char
                                              :fill-pointer 0)))
               (UNLESS (ZEROP (setf (fill-pointer leftover) (- limit start)))
                 (copy-array-portion buf start limit
                                     leftover 0 (- limit start)))
               (send from-stream :advance-input-buffer)
               (setq leftoverp t)
               (go read-buffer))
              ((not leftoverp)
               (funcall f buf start end)
               (setq start (1+ end))
               (incf easy-lines)
               (go next-line))
              ('else
               (let* ((old-fp (fill-pointer leftover))
                      (new-fp (+ old-fp (- end start))))
                 (setf (fill-pointer leftover)  new-fp)
                 (copy-array-portion buf start end
                                     leftover old-fp new-fp)
                 (funcall f leftover 0 new-fp)
                 (setq leftoverp nil)
                 (setq start (1+ end))
                 (incf hard-lines)
                 (go next-line))))))

#||
;; reference version.

(defun map-over-stream-lines (f from-stream)
  (do ((line)(eof))
      ((progn (multiple-value-setq (line eof)
                (send from-stream :line-in))
              eof))
    (funcall f line 0 (length line))))

#||
