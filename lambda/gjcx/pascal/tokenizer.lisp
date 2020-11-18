;;; -*- Mode:LISP; Package:PASCAL; Base:10; Fonts:(CPTFONTB); Readtable:CL -*-


(defvar %reserved-tokens%
        '(and
          array
          begin
          case
          const
          div
          do
          downto
          else
          end
          file
          for
          forward
          function
          goto
          if
          in
          label
          mod
          nil
          not
          of
          or
          packed
          procedure
          program
          record
          repeat
          set
          string
          then
          to
          type
          until
          var
          while
          with
          +
          -
          *
          /
          <
          <=
          =
          <>
          >
          >=
          \:=
          \,
          \;
          \:
          \'
          \.
          \.\.
          ^
          \(
          \)
          [
          ]))

(defvar *ptol-readtable* (copy-readtable nil))

(defun initialize-ptol-readtable ()
  (let ((*readtable* *ptol-readtable*))
    (set-syntax-macro-char #\; #'(lambda (ignore ignore) '\;))
    (set-syntax-macro-char #\: #'colon-reader)
    (set-syntax-macro-char #\= #'(lambda (ignore ignore) '=))
    (set-syntax-macro-char #\[ #'(lambda (ignore ignore) '[))
    (set-syntax-macro-char #\] #'(lambda (ignore ignore) ']))
    (set-syntax-macro-char #\{ #'comment-reader)
    (set-syntax-macro-char #\< #'lt-reader)
    (set-syntax-macro-char #\> #'gt-reader)
    (set-syntax-macro-char #\. #'(lambda (ignore ignore) '\.))
    (set-syntax-macro-char #\, #'(lambda (ignore ignore) '\,))
    (set-syntax-macro-char #\+ #'(lambda (ignore ignore) '+))
    (set-syntax-macro-char #\- #'(lambda (ignore ignore) '-))
    (set-syntax-macro-char #\* #'(lambda (ignore ignore) '*))
    (set-syntax-macro-char #\/ #'(lambda (ignore ignore) '\/))
    (set-syntax-macro-char #\( #'open-paren-or-comment-reader)
    (set-syntax-macro-char #\) #'(lambda (ignore ignore) '\)))
    (set-syntax-macro-char #\# #'(lambda (ignore ignore) '\#))
    (set-syntax-macro-char #\" #'(lambda (ignore ignore) '\"))
    (set-syntax-macro-char #\^ #'(lambda (ignore ignore) '\^))
    (set-syntax-macro-char #\' #'string-reader)
    (set-syntax-macro-char #\. #'dot-reader)))


(defun open-paren-or-comment-reader (ignore stream)
  (cond ((char-equal #\* (peek-char nil stream))
         (do ()
             ((and (char-equal (read-char stream) #\*)
                   (char-equal (peek-char nil stream) #\)))
              (read-char stream)
              '**comment**)))
        ('else
         '\()))

(defun comment-reader (ignore stream)
  (do-forever
    (if (char-equal (send stream :tyi) #\})
        (return)))
  '**comment**)


(defun gt-reader (ignore stream)
  (cond ((char-equal (peek-char nil stream) #\=)
         (read-char stream)
         '>=)
        (t '>)))

(defun lt-reader (ignore stream)
  (let ((char (peek-char nil stream)))
    (cond ((char-equal char #\=)
           (read-char stream)
           '<=)
          ((char-equal char #\>)
           (read-char stream)
           '<>)
          (t '<))))

(defun colon-reader (ignore stream)
  (cond ((char-equal (peek-char nil stream) #\=)
         (read-char stream)
         '\:=)
        (t '\:)))

(defun dot-reader (ignore stream)
  (cond ((char-equal (peek-char nil stream) #\.)
         (read-char stream)
         '\.\.)
        (t '\.)))

(defun string-reader (ignore stream)
  (do ((string (make-string 10 :fill-pointer 0))
       (char (read-char stream) (read-char stream)))
      (nil)
    (cond ((char-equal char #\')
           (cond ((char-equal (peek-char nil stream) #\')
                  (read-char stream)
                  (array-push-extend string char))
                 (t (return string))))
          (t (array-push-extend string char)))))

(defun ptol-read (stream &optional (eof-errorp t) eof-flag)
  (let ((*readtable* *ptol-readtable*))
    (read stream eof-errorp eof-flag)))


(defun tokenize-stream (stream)
  (do (token tokens)
      ((eq token '**eof**) (nreverse tokens))
    (setq token (ptol-read stream nil '**eof**))
    (if (and (not (eq token '**comment**))
             (not (eq token '**eof**)))
        (push token tokens))))


(defun tokenize-file (filename)
  (with-open-file (file filename)
    (tokenize-stream file)))

(defun tokenize-string (string)
  (with-input-from-string (str string)
    (tokenize-stream str)))


(defun categorize-token (token)
  (cond ((memq token %reserved-tokens%) token)
        ((numberp token) '<number>)
        ((stringp token) '<string>)
        (t '<identifier>)))


(defun parse-string (string &optional target-category)
  (with-input-from-string (str string)
    (parse (nodify-tokens (tokenize-stream str))
           target-category)))


(defun parse-tokens (tokens &optional target-category)
  (parse (nodify-tokens tokens)
         target-category))


(defun nodify-tokens (tokens)
  (mapcar #'(lambda (x) (make-node :category (categorize-token x)
                                   :translation x))
          tokens))


(initialize-ptol-readtable)
