;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:10; Readtable:ZL -*-

(defconst *spelling-dwim-is-loaded?* t "Variable is boundp if dwim file exists.")

(defvar *enable-spelling-dwim?* nil "T to turn on spelling checker.")

(defun find-candidate-for-poor-spelling (symbol package definition-type)
  ;; This code does a lot of unnecessary stuff (e.g. conses up a
  ;; possibilities list)   However, it works fairly well.
  (let ((possibilities
          (make-possibilities (string-upcase (string symbol))
                              (make-tryer
                                #'(lambda (word if-good)
                                    (let ((symbol (intern-soft word *package*)))
                                      (if (and symbol
                                               (funcall definition-type symbol))
                                          (funcall if-good symbol))))))))
    (and (= (length possibilities) 1)
         (first possibilities))))

(defconst *number-of-possibilities-to-check* 2.)

(defvar *possibilities*)

(defun look-in-package (symbol-or-print-name &optional (package *package*))
  (make-possibilities (string-upcase (string symbol-or-print-name))
                      (make-tryer
                        #'(lambda (word)
                            (intern-soft word package)))))

(defun make-possibilities (word checker)
  (setq *possibilities* nil)
  (catch 'enough
    (wrong-letter word checker)
    (extra-letter word checker)
    (missing-letter word checker)
    (transposed-letter word checker)
    (asterisks word checker))
  (remove word (remove-duplicates *possibilities* :test #'equal)))

(defun make-tryer (test)
  #'(lambda (word)
      (funcall test word #'insert)))

(defun insert (word)
  (push word *possibilities*)
  (if (> (length *possibilities*) *number-of-possibilities-to-check*)
      (*throw 'enough nil)))

(defun wrong-letter (word try)
  (let ((word-copy (copy-seq word)))
    (dotimes (char-number (string-length word))
      (do ((letter (char-int #/A) (1+ letter)))
          ((> letter (char-int #/Z)))
        (setf (elt word-copy char-number) letter)
        (funcall try word-copy)
        (setf (elt word-copy char-number) (elt word char-number))))))

(defun extra-letter (word try)
  (let* ((nchars (length word))
         (test-word (make-string (1- (length word)))))
    (dotimes (char-number nchars)
      (do ((from 0 (1+ from))
           (to 0))
          ((= from nchars) (funcall try test-word))
        (unless (= from char-number)
          (setf (elt test-word to) (elt word from))
          (incf to))))))

(defun missing-letter (word try)
  (let* ((nchars (length word))
         (test-word (make-string (1+ nchars)))
         (char-number 0))
    (tagbody
        loop
           (do ((letter (char-int #/A) (1+ letter)))
               ((> letter (char-int #/Z)))
             (setf (elt test-word char-number) letter)
             (do ((x char-number (1+ x)))
                 ((= x nchars) (funcall try test-word))
               (setf (elt test-word (1+ x)) (elt word x))))
           (unless (= char-number nchars)
             (setf (elt test-word char-number) (elt word char-number))
             (incf char-number)
             (go loop)))))

(defun transposed-letter (word try)
  (let* ((nchars (length word))
         (test-word '()))
    (dotimes (char-number (1- nchars))
      (setq test-word (copy-seq word))
      (let ((temp (elt test-word char-number)))
        (setf (elt test-word char-number) (elt test-word (1+ char-number)))
        (setf (elt test-word (1+ char-number)) temp))
      (funcall try test-word))))

(defun asterisks (word try)
  (funcall try (string-append "*" word    ))
  (funcall try (string-append     word "*"))
  (funcall try (string-append "*" word "*")))
