;;-*- Mode:LISP; Package:MICRO-ASSEMBLER; Lowercase:T; Base:8 -*-

(defun read-ucode (file)
  (with-open-file (stream file '(in block))
    (eval (fread stream))))

;; Fast, simple reader for reading in ucode

(defvar *stream* :unbound
  "Stream that was passed to FREAD, for reference from within FREAD.")

(defvar *line-in* :unbound
  "Within FREAD, buffers one line read from the stream passed to FREAD.")

(defvar *line-in-index* :unbound
  "Within FREAD, index for scanning within *LINE-IN*")

(defvar *line-in-length* :unbound
  "Within FREAD, length of valid data in *LINE-IN*")

(defvar *unrchf* :unbound
  "Used within FREAD for backing up one character.")

(defvar *char-type-table* :unbound
  "Syntax table used by FREAD.  Indexed by an input character.
Each element is:
 NIL  -> break char
 T -> whitespace
 negative number, -n  -> number, ascii code n
 positive number, +n  -> non-number. n is code to store
  (n is identical to the character used to index, except for upcasing).")

(defvar *fread-string* :unbound)

(defmacro fread-eat-whitespace nil
  `(prog nil
    l    (cond ((eq (setq char-type (ar-1 *char-type-table* (setq char (fread-tyi))))
                    t)
                (go l)))
         (return char)))

(defmacro fread-tyi (&optional preserve-crs)
  `(prog nil
        (cond (*unrchf*
               (return (prog1 *unrchf* (setq *unrchf* nil)))))
     l  (cond ,@(if preserve-crs '(((= *line-in-index* *line-in-length*)
                                    (setq *line-in-index* (1+ *line-in-index*))
                                    (return #\return))))
              ((>= *line-in-index* *line-in-length*)
               (return-array *line-in*)
               (multiple-value (*line-in* eof-flag)
                            (funcall *stream* ':line-in))
               (and eof-flag
                    (equal *line-in* "")
                    (ferror nil "Premature EOF in fast reader"))
               (setq *line-in-length* (array-active-length *line-in*))
               (setq *line-in-index* 0)
               (go l)))
       (return (prog1 (ar-1 *line-in* *line-in-index*)
                      (setq *line-in-index* (1+ *line-in-index*))))))

(defun fread (&rest read-args &aux *stream* eofval)
  "Like READ, but faster and accepting only a very limited syntax.
Called just like READ."
   (declare (arglist stream eof-option))
   (multiple-value (*stream* eofval)
                   (si:decode-read-args read-args))
   (cond ((not (variable-boundp *char-type-table*))
          (setq *char-type-table* (make-array 216 ':type 'art-q))
          (do ch 0 (1+ ch) (= ch 216)                          ;initialize to self
              (as-1 ch *char-type-table* ch))
          (do ch #/0 (1+ ch) (> ch #/9)                        ;numbers
              (as-1 (minus ch) *char-type-table* ch))
          (dolist (ch '(#/( #/) #/. #// #/; #/' #/_ #/#))      ;breaks
            (as-1 nil *char-type-table* ch))
          (dolist (ch '(#\sp #\tab #\lf #\vt #\ff #\return))  ;white-space
            (as-1 t *char-type-table* ch))
          (do ch #/a (1+ ch) (> ch #/z)
              (as-1 (- ch 40) *char-type-table* ch))
          (setq *fread-string*
                (make-array 200 ':type 'art-string ':leader-list '(0)))))
   (setq *line-in-length*
           (array-active-length (setq *line-in* (funcall *stream* ':line-in))))
   (setq *line-in-index* 0)
   (setq *unrchf* nil)
   (unwind-protect (fread-1)
        (return-array *line-in*)
        (setq *line-in* nil)))


(defun fread-1 nil
 (prog (idx ob char char-type number-possible dec oct acc number-finished sign
        digit-seen eof-flag)
       (setq idx -1 number-possible t dec 0 oct 0 sign 1)
  l00  (setq char (fread-eat-whitespace))
  l0   (cond ((not (numberp char-type))         ;predicate true if not symbol constit.
              (cond ((= char #/.)                       ;dot
                     (cond ((and number-possible (not (= idx -1)))
                            (setq oct dec)
                            (setq char-type #/.)                ;can be symbol const
                            (setq number-finished t))   ;error check
                           (t (break "dot-context-error"))))    ;legit dots read at read-list
                    ((= char #/_)                       ;underline (old leftarrow)
                     (setq oct (ash oct (fread-1)))
                     (go x))
                    ((= char #// )                      ;slash
                     (setq number-possible nil)
                     (setq char-type (fread-tyi))
                     (go s))
                    ((> idx -1)
                     (setq *unrchf* char)               ;in middle of somthing,
                     (go x))
                    ((= char #/()
                     (go read-list-start))
                    ((= char #/')
                     (return (list 'quote (fread-1))))
                    ((= char #/;)
                     (return-array *line-in*)
                     (setq *line-in-length*
                           (array-active-length
                            (setq *line-in*
                                  (funcall *stream* ':line-in))))
                     (setq *line-in-index* 0)
                     (go l00))
                    ((= char #/))
                     (ferror nil "unexpected close"))   ;()
                    ((= char #/#)
                     (let ((values
                             (si:invoke-reader-macro
                               (cdr (assq char (dont-optimize (si:rdtbl-macro-alist readtable))))
                               'fread-stream)))
                       (if values (return (car values))
                         (*throw ':top-level-splicing t))))
                    (t (go l00))))   ;flush it.
            (number-possible
             (cond ((> char-type 0)             ;true if not digit
                    (cond ((and (= char #/+) (null digit-seen)))
                          ((and (= char #/-) (null digit-seen))
                           (setq sign (minus sign)))
                          (t (setq number-possible nil))))
                   (t
                    (cond (number-finished (break "no-floating-point")))
                    (setq digit-seen t)
                    (setq dec (+ (* 10. dec) (setq ob (- char #/0)))
                          oct (+ (* 10 oct) ob))))))
  s   (aset (abs char-type) *fread-string* (setq idx (1+ idx)))
      (setq char-type (ar-1 *char-type-table* (setq char (fread-tyi t))))
      (go l0)
  read-list-start
      (setq char (fread-eat-whitespace))
      (cond ((= char #/))               ;close
             (return nil))
            ((= char #/;)                       ;Semi-colon
             (return-array *line-in*)
             (setq *line-in-length*
                   (array-active-length
                    (setq *line-in*
                          (funcall *stream* ':line-in))))
             (setq *line-in-index* 0)
             (go read-list-start)))
      (setq *unrchf* char)
  read-list
      (*catch ':top-level-splicing
        (push (fread-1) acc))
  read-list0
      (setq char (fread-eat-whitespace))
      (cond ((eq char #/))                      ;close
             (return (nreverse acc)))
            ((= char #/;)                       ;Semi-colon
             (return-array *line-in*)
             (setq *line-in-length*
                   (array-active-length
                    (setq *line-in*
                          (funcall *stream* ':line-in))))
             (setq *line-in-index* 0)
             (go read-list0))
            ((eq char #/.)
             (setq ob (cons (car acc) (fread-1)))
             (cond ((not (= (setq char (fread-eat-whitespace))
                            #/)))
                    (break "dot-closing-error")))
             (return ob)))
      (setq *unrchf* char)
      (go read-list)
   x  (cond ((and number-possible digit-seen)             ; return it.
                                                        ;digit seen so it wins on +, -
             (return (* sign oct)))
            (t (cond ((not (= idx -1))
                      (store-array-leader (1+ idx) *fread-string* 0)
                      (multiple-value (ob oct)
                                (intern-soft *fread-string*))
                      (cond ((null oct)
                             (setq ob
                                   (intern
                                     (let ((s (make-array (1+ idx)
                                                          ':type 'art-string)))
                                                 (copy-array-contents *fread-string*
                                                                       s)
                                                 s)))))))
               (return ob)))))


(defprop fread-stream t io-stream-p)

(defun fread-stream (operation &optional arg1 &rest rest &aux eof-flag)
  (COND ((EQ OPERATION ':TYI)
         (fread-tyi t))         ;preserve cr's so comments terminate!
        ((EQ OPERATION ':UNTYI)
         (setq *unrchf* arg1))
        ((EQ OPERATION ':WHICH-OPERATIONS)
         '(:TYI :UNTYI))
        (T (STREAM-DEFAULT-HANDLER 'READ-FROM-STRING-STREAM OPERATION ARG1 REST))))
