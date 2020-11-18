;;; -*- Mode:LISP; Package:POSTSCRIPT; Base:10; Readtable:CL -*-

; %  comment
; [] beginning and end of array
; {} beginning and end of procedure
; /  quotes a symbol
; () string delimeters
; \  introduces octal in string
; <> string with hex numbers for characters

(defun isspace (c)
  (member c '(#\space #\tab #\newline #o12 #o15 #o11)))

(defun isdelimeter (c)
  (member c '(#\% #\[ #\] #\{ #\} #\/ #\( #\) #\\ #\< #\>)))

(defun skip-spaces (stream)
  (do ((c (read-char stream nil)
          (read-char stream nil)))
      ((null c) nil)
    (when (not (isspace c))
      (unread-char c stream)
      (return nil))))


(defun read-token (stream)
  (skip-spaces stream)
  (let ((c (read-char stream nil)))
    (cond ((null c) nil)
          ((isdelimeter c)
           c)
          (t
           (with-output-to-string (str)
             (write-char c str)
             (do ((c (read-char stream nil)
                     (read-char stream nil)))
                 ((null c)
                  nil)
               (cond ((isdelimeter c)
                      (unread-char c stream)
                      (return nil))
                     ((isspace c)
                      (return nil))
                     (t
                      (write-char c str)))))))))

(defun test-tokens ()
  (do-forever (print (read-token *standard-input*))))
