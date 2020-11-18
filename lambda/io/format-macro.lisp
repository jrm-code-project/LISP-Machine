;; -*- Mode:Lisp; Package:FORMAT; Base:8 -*-
;; Function for printing or creating nicely formatted strings.
;; written by Andrew L. Ressler on September 8, 1982
;; copyright LISP MACHINE INC.
;; permission granted to anyone to use this or modify it.

;; attempt to turn format into a macro facility
;; if it can't do it easily it just makes it call format instead.

(defvar format-results NIL)
(defvar final-format-results NIL)
(defvar inside-conditional NIL)
(defvar optimize-lets T)

(defmacro format-macro (stream ctl-string &REST args)
  (prog ()
        (setq final-format-results NIL)
        (let ((FORMAT-ARGLIST ARGS)
              (LOOP-ARGLIST NIL)(value))
          (setq value
                (*Catch 'IMPOSSIBLE
                  (*CATCH 'FORMAT-/:^-POINT
                    (*CATCH 'FORMAT-^-POINT
                      (COND ((STRINGP CTL-STRING)
                             (FORMAT-CTL-STRING-macro ARGS CTL-STRING))
                            (T (return `(format ,stream ,ctl-string . ,args))))))))
          (if (eq value 'IMPOSSIBLE)
              (return `(format ,stream ,ctl-string . ,args))))
        (return
          (let ((result
                  (if (null stream)
                      `(progn
                         ;;; Only bind FORMAT-STRING if STREAM is NIL.  This avoids lossage if
                         ;;; FORMAT with a first arg of NIL calls FORMAT recursively (e.g. if
                         ;;; printing a named structure).
                         (bind (value-cell-location 'FORMAT-STRING)
                               (make-array 200
                                           ':AREA FORMAT-TEMPORARY-AREA
                                           ':TYPE 'ART-STRING
                                           ':LEADER-LIST '(0)))
                         (let ((standard-output 'FORMAT-STRING-STREAM))
                           ,@(nreverse final-format-results))
                         (prog1 (substring format-string 0)
                                (return-array format-string)))
                    `(let ((standard-output
                             ,(cond ((eq stream T) 'standard-output)
                                    (T stream))))
                       . ,(nreverse final-format-results)))))
            (if optimize-lets
                (setq result (elim-lets result))
              result)))))

(defun format-ctl-string-macro (ARGS CTL-STRING &AUX (FORMAT-PARAMS NIL))
  (UNWIND-PROTECT
      (DO ((CTL-INDEX 0)
           (CTL-LENGTH (ARRAY-ACTIVE-LENGTH CTL-STRING))
           (TEM))
          ((>= CTL-INDEX CTL-LENGTH))
        (SETQ TEM (%STRING-SEARCH-CHAR #/~ CTL-STRING CTL-INDEX CTL-LENGTH))
        (COND ((NEQ TEM CTL-INDEX)                      ;Put out some literal string
               (push `(funcall standard-output
                               ':STRING-OUT
                               ,(substring CTL-STRING CTL-INDEX
                                           ;If you really supply the fourth arg,
                                           ;it had better be a string index or some
                                           ;streams will bomb.
                                           (IF (NULL TEM)
                                               (ARRAY-ACTIVE-LENGTH CTL-STRING)
                                             tem)))
                     final-format-results)
               (IF (NULL TEM) (RETURN))
               (SETQ CTL-INDEX TEM)))
        ;; (AREF CTL-STRING CTL-INDEX) is a tilde.
        (LET ((ATSIGN-FLAG NIL)
              (COLON-FLAG NIL)
              (format-results NIL)
              (flush-let NIL))
          (IF (NULL FORMAT-PARAMS)
              (SETQ FORMAT-PARAMS (ALLOCATE-RESOURCE 'FORMAT-PARAMS)))
          (STORE-ARRAY-LEADER 0 FORMAT-PARAMS 0)
          (MULTIPLE-VALUE (TEM ARGS) (FORMAT-PARSE-COMMAND ARGS T))
          (multiple-value (ARGS flush-let)
            (FORMAT-CTL-OP-macro TEM ARGS (G-L-P FORMAT-PARAMS)))
          (if flush-let
              (push (cons 'PROGN (nreverse format-results)) final-format-results)
            (push `(let ((atsign-flag ,ATSIGN-FLAG)
                         (colon-flag ,colon-flag))
                     . ,(nreverse format-results))
                  final-format-results))))
    (AND FORMAT-PARAMS (DEALLOCATE-RESOURCE 'FORMAT-PARAMS FORMAT-PARAMS)))
  ARGS)


;Perform a single formatted output operation on specified args.
;Return the remaining args not used up by the operation.
(defun format-ctl-op-macro (op args params &AUX tem immediate)
    (cond ((null op) (format-error "Undefined FORMAT command.") args) ;e.g. not interned
          ((setq tem (get op 'FORMAT-CTL-ONE-ARG))
           (if (setq immediate (get op 'EVAL-IMMEDIATE))
               (progn
                 (funcall immediate (car args) params)
                 (values (cdr args) T))
             (push `(funcall ',tem ,(copytree (first args)) ',(copytree params))
                   format-results)
             (cdr args)))
          ((setq tem (get op 'FORMAT-CTL-NO-ARG))
           (if (setq immediate (get op 'eval-immediate))
               (progn
                 (funcall immediate params)
                 (values args T))
             (push `(funcall ',TEM ',(copytree params)) format-results)
             args))
          ((setq tem (get op 'FORMAT-CTL-MULTI-ARG))
           (if (setq immediate (get op 'eval-immediate))
               (values (funcall immediate args params) T)
             (push `(funcall ',TEM ,(copytree args) ,(copytree params)) format-results)))
          ((setq tem (get op 'FORMAT-CTL-REPEAT-CHAR))
           (push `(format-ctl-repeat-char ,(copytree (or (first params) 1)) ,tem)
                 format-results)
           (values args T))
          (T (FORMAT-ERROR "/"~S/" is not defined as a FORMAT command." OP)
             args)))

(defprop * format-ctl-ignore-macro eval-immediate)
(defun format-ctl-ignore-macro (args params &AUX (count (or (car params) 1)))
  (if colon-flag
      (do ((a format-arglist (cdr a))
           (b (nthcdr count format-arglist) (cdr b)))
          ((null a) (format-error "Can't back up properly for a ~:*"))
        (and (eq b args) (return a)))
    (nthcdr count args)))

(defprop crlf crlf-macro eval-immediate)
(defun crlf-macro (ignore)
  (and atsign-flag
       (push '(funcall standard-output ':TYO #\CR) format-results)))

(defprop % format-ctl-newlines-macro eval-immediate)
(defun format-ctl-newlines-macro (params &AUX (count (or (car params) 1)))
  (dotimes (i count)
    (push '(funcall standard-output ':TYO #\CR) format-results)))


(defprop & format-ctl-fresh-line-macro eval-immediate)
(defun format-ctl-fresh-line-macro (params &AUX (count (or (car params) 1)))
  (push '(funcall standard-output ':FRESH-LINE) format-results)
  (do i (1- count) (1- i) (= i 0)
      (push '(FUNCALL STANDARD-OUTPUT ':TYO #\CR) format-results)))


(defprop d format-ctl-decimal-macro eval-immediate)
(defun format-ctl-decimal-macro (arg params &OPTIONAL (base 10.)        ;Also called for octal
                                 &AUX
                                 (width (first params))
                                 (padchar (second params))
                                 (commachar (third params))
                                 (gen-arg (gensym)))
  (setq padchar (cond ((null padchar) #\SP)
                      ((numberp padchar) padchar)
                      (T (aref (string padchar) 0)))
        commachar (cond ((null commachar) #/,)
                        ((numberp commachar) commachar)
                        (T (aref (string commachar) 0))))
  (push
    `(let ((base ,base)(*nopoint T)(,gen-arg ,arg))
       ,@(if width
             `((format-ctl-justify
                 ,width
                 (+ (if (typep ,gen-arg ':FIXNUM)
                        (+ (LOOP FOR x = (abs ,gen-arg) THEN (// x base)
                                 COUNT T
                                 UNTIL (< x base))
                           (if (minusp ,gen-arg) 1 0))
                      (flatc ,gen-arg))
                    ,(if atsign-flag
                         `(if (and (numberp ,gen-arg)(not (minusp ,gen-arg))) 1 0)
                       0)
                    ,(if colon-flag
                         `(if (fixp ,gen-arg) (// (1- (flatc (abs ,gen-arg))) 3) 0) 0))
                 ,padchar)) NIL)
       ,@(if atsign-flag
             `((if (and (numberp ,gen-arg)(not (minusp ,gen-arg)))
                   (funcall standard-output ':TYO #/+)))
           NIL)
       ,(if colon-flag
            `(cond ((fixp ,gen-arg)
                    ;; Random hair with commas.  I'm not going to bother not consing.
                    (cond ((minusp ,gen-arg)
                           (funcall standard-output ':TYO #/-) (setq ,gen-arg (- ,gen-arg))))
                    (setq ,gen-arg (nreverse (inhibit-style-warnings    ;Give up!
                                           (exploden ,gen-arg))))
                    (do ((l ,gen-arg (cdr l))
                         (i 2 (1- i)))
                        ((null (cdr l)))
                      (cond ((zerop i)
                             (rplacd l (cons ,commachar (cdr l)))
                             (setq i 3 l (cdr l)))))
                    (dolist (ch (nreverse ,gen-arg))
                      (funcall standard-output ':TYO ch)))
                   ((typep ,gen-arg ':FIXNUM) (si:print-fixnum ,gen-arg standard-output))
                   ;; This is PRINC rather than PRIN1
                   ;; so you can have a string instead of a number
                   (T (princ ,gen-arg)))
          `(cond ((typep ,gen-arg ':FIXNUM) (si:print-fixnum ,gen-arg standard-output))
                 ;; This is PRINC rather than PRIN1
                 ;; so you can have a string instead of a number
                 (T (princ ,gen-arg)))))
    format-results))

(defprop o format-ctl-octal-macro eval-immediate)
(defun format-ctl-octal-macro (arg params)
  (format-ctl-decimal-macro arg params 8))

(defprop f format-ctl-f-format-macro eval-immediate)
(defun format-ctl-f-format-macro (arg params)
  (push
    `(let ((arg ,arg))
       (and (numberp arg) (not (floatp arg)) (setq arg (float arg)))
       (if (not (floatp arg))
           ,(let ((format-results NIL))
              (format-ctl-decimal-macro 'ARG NIL)
              format-results)
         (si:print-flonum arg standard-output NIL (small-floatp arg)
                          ,(first params) NIL)))
    format-results))

(defprop e format-ctl-e-format-macro eval-immediate)
(defun format-ctl-e-format-macro (arg params)
  (push
    `(let ((arg ,arg))
       (and (numberp arg) (not (floatp arg)) (setq arg (float arg)))
       (if (not (floatp arg))
           ,(let ((format-results NIL))
              (format-ctl-decimal-macro 'ARG NIL)
              format-results)
         (si:print-flonum arg standard-output NIL (small-floatp arg)
                          ,(first params) T)))
    format-results))


(defprop A format-ctl-ascii-macro eval-immediate)
(defun format-ctl-ascii-macro (arg params &OPTIONAL prin1p)
  (let ((edge (car params))
        (period (cadr params))
        (min (caddr params))
        (padchar (cadddr params)))
    (cond ((null padchar)
           (setq padchar #\SP))
          ((not (numberp padchar))
           (setq padchar (character padchar))))
    (cond (atsign-flag)                         ;~@5nA right justifies
          (colon-flag
           (if prin1p
               (push `(cond ((null ,arg)
                             (funcall standard-output ':STRING-OUT "()"))
                            (T (prin1 ,ARG))) format-results)
             (push `(cond ((null ,arg)
                           (funcall standard-output ':STRING-OUT "()"))
                          ((stringp ,arg) (funcall standard-output ':STRING-OUT ,arg))
                          (T (princ ,arg))) format-results)))
          (prin1p (push `(prin1 ,ARG) format-results))
          (T (push `(if (stringp ,ARG)(funcall standard-output ':STRING-OUT ,ARG)
                      (princ ,ARG)) format-results)))
    (cond ((not (null edge))
           (push
             `(let ((width ,(if prin1p
                                `(funcall #'FLATSIZE ,ARG)
                              `(cond ((stringp ,ARG) #'ARRAY-ACTIVE-LENGTH)
                                     (T #'FLATC)))))
                ,@(cond (min
                         `((progn
                             (format-ctl-repeat-char ,min ,padchar)
                             (setq width (+ width ,min))))))
                ,(cond (period
                         `(progn
                            (format-ctl-repeat-char
                              (- (+ ,edge (* (// (+ (- (max ,edge width) ,edge 1)
                                                    ,period)
                                                 ,period)
                                             ,period))
                                 width)
                              ,padchar)))
                        (T (progn `(format-ctl-justify edge width ,padchar)))))
             format-results)))
    (cond ((null atsign-flag))
          (colon-flag
           (if prin1p
               (push `(cond ((null ,arg)
                             (funcall standard-output ':STRING-OUT "()"))
                            (T (prin1 ,ARG))) format-results)
             (push `(cond ((null ,arg)
                           (funcall standard-output ':STRING-OUT "()"))
                          ((stringp ,arg) (funcall standard-output ':STRING-OUT ,arg))
                          (T (princ ,arg))) format-results)))
          (prin1p (push `(prin1 ,ARG) format-results))
          (T (push `(if (stringp ,ARG)(funcall standard-output ':STRING-OUT ,ARG)
                      (princ ,ARG)) format-results)))))

(defprop s format-ctl-sexp-macro eval-immediate)
(defun format-ctl-sexp-macro (arg params)
    (format-ctl-ascii-macro arg params T))

(defprop g format-ctl-goto-macro eval-immediate)
(defun format-ctl-goto-macro (ignore params &AUX (count (or (car params) 1)))
  (nthcdr count format-arglist))

(defprop p format-ctl-plural-macro eval-immediate)
(defun format-ctl-plural-macro (args ignore)
  (and colon-flag (setq args (format-ctl-ignore-macro args NIL))) ;crock: COLON-FLAG is set
  (if atsign-flag
      (push `(if (equal ,(car args) 1)
                 (funcall standard-output ':TYO #/y)
               (funcall standard-output ':STRING-OUT "ies"))
            format-results)
    (push `(or (equal,(car args) 1) (funcall standard-output ':TYO #/s)) format-results))
  (cdr args))

(defprop q format-ctl-apply-macro eval-immediate)
(defun format-ctl-apply-macro (arg params)
  (push `(apply ,arg ,params) format-results))

(defprop t format-ctl-tab-macro eval-immediate)
(defun format-ctl-tab-macro (params &AUX (dest (or (first params) 1))
                             (extra (or (second params) 1)))
  (push
    `(let ((ops (funcall standard-output ':WHICH-OPERATIONS))(incr-ok))
       (cond ((or (setq incr-ok (memq ':INCREMENT-CURSORPOS ops))
                  (memq ':SET-CURSORPOS ops))
              (multiple-value-bind (x y) (funcall standard-output ':READ-CURSORPOS
                                                  ,(if colon-flag '':PIXEL '':CHARACTER))
                (let ((new-x              ;next multiple of EXTRA after X
                        (if (< x ,dest) ,dest
                          ,(if (eq extra 1)
                               `(1+ x)
                             `(* (1+ (// x ,extra)) ,extra)))))
                  (cond (incr-ok
                         ;; Use :INCREMENT-CURSORPOS preferentially
                         ;; because it will do a **MORE** if we need one.
                         (funcall standard-output ':INCREMENT-CURSORPOS
                                  (- new-x x) 0  ,(if colon-flag '':PIXEL '':CHARACTER)))
                        (T
                         (funcall standard-output ':SET-CURSORPOS
                                  new-x y ,(if colon-flag '':PIXEL '':CHARACTER)))))))
             (T (funcall standard-output ':STRING-OUT "   "))))
    format-results))

(defprop [ format-ctl-start-case-macro eval-immediate)
(defun format-ctl-start-case-macro (args params &AUX (arg (car args)))
  (let ((inside-conditional T))
    (let ((clauses (format-parse-clauses '] T))
          (remaining-args 'NO-ARGS))
      (cond (colon-flag
              (cond (atsign-flag (format-error "~~:@[ is not a defined FORMAT command"))
                    (T (pop args))))
            (atsign-flag (*THROW 'IMPOSSIBLE 'IMPOSSIBLE))
            (T (pop args)))
      (push
        `(let ((arg
                 ,(COND (COLON-FLAG
                         (COND (ATSIGN-FLAG (FORMAT-ERROR
                                              "~~:@[ is not a defined FORMAT command"))
                               (T `(if ,ARG 1 0))))
                        (ATSIGN-FLAG
                         `(if ,ARG 0 -1))
                        ((CAR PARAMS) (CAR PARAMS))
                        (T arg))))
           ,(cons 'cond
                  (LOOP FOR clause ON (g-l-p clauses) BY #'CDDDR
                        FOR clause-number FROM 0
                        AS string = (first clause)
                        AS code = (let* ((final-format-results NIL)
                                         (arguments (format-ctl-string-macro args string)))
                                    (if (or (eq remaining-args 'NO-ARGS)
                                            (equal remaining-args arguments))
                                        (setq remaining-args arguments)
                                      (*Throw 'IMPOSSIBLE 'IMPOSSIBLE))
                                    (nreverse final-format-results))
                        COLLECT `((= ,clause-number arg)
                                  . ,code))))
       format-results)
      remaining-args)))

(defprop ] format-ctl-end-case-macro eval-immediate)
(defun format-ctl-end-case-macro (ignore)
  (format-error "Stray ~~] in FORMAT control string"))

(defun elim-lets (tree)
  (cond ((null tree) NIL)
        ((atom tree) tree)
        ((listp tree)
         (setq tree (eliminate-lets tree))
         (elim-lets (first tree))
         (elim-lets (cdr tree))
         tree)))

(defun eliminate-lets (tree)
  (if (and (listp tree)
           (listp (first tree))
           (listp (second tree)))
      (if (and (eq 'LET (first (first tree)))
               (eq 'LET (first (second tree))))
          ;; then maybe we can eliminate something
          (if (equal (second (first tree))(second (second tree)))
              ;; then we can eliminate the lets probably.
              (progn
                (setf (second tree)
                      `(let
                         ,(second (first tree))
                         ,(third (first tree))
                         ,(third (second tree))))
                (setf (first tree) '(progn)))
            tree)
        tree)
    tree))

(defprop /| format-ctl-forms-macro eval-immediate)
(defun format-ctl-forms-macro (params)
  (if colon-flag
      (push
        `(if (memq ':CLEAR-SCREEN (funcall standard-output ':WHICH-OPERATIONS))
             (funcall standard-output ':CLEAR-SCREEN)
           (format-ctl-repeat-char ,(or (first params) 1) #\FORM))
        format-results)
    (push `(format-ctl-repeat-char ,(or (first params) 1) #\FORM)
          format-results)))




(defprop { format-iterate-over-list-maco eval-immediate)
(defun format-iterate-over-list-maco (&REST ignore)
  (*Throw 'IMPOSSIBLE 'IMPOSSIBLE))

(defprop ^ format-ctl-terminate-macro eval-immediate)
(defun format-ctl-terminate-macro (&REST ignore)
  (*Throw 'IMPOSSIBLE 'IMPOSSIBLE))

;This is not so hairy as to work with ~T, tabs, crs.  I really don't see how to do that.
;It makes a list of strings, then decides how much spacing to put in,
;then goes back and outputs.
(defprop < format-hairy-justification-macro eval-immediate)
(defun format-hairy-justification-macro (&REST ignore)
  (*Throw 'IMPOSSIBLE 'IMPOSSIBLE))

(comment
(defun format-hairy-justification-macro (args params)
  (let ((mincol (or (first params) 0))
        (colinc (or (second params) 1))
        (minpad (or (third params) 0))
        (padchar (or (fourth params) #\SP))(temp-results NIL))
    `(let ((newline NIL)
           (extra 0)
           (linewidth NIL)
           (strings NIL)
           (string-ncol 0)
           (clauses)
           (n-padding-points -1)
           (total-padding)
           (n-pads)
           (n-extra-pads))
    (push '((W-O (FUNCALL STANDARD-OUTPUT ':WHICH-OPERATIONS))) temp-results)
    (and colon-flag (setq n-padding-points (1+ n-padding-points)))
    (and atsign-flag (setq n-padding-points (1+ n-padding-points)))
    (*catch 'FORMAT-^-POINT
        (progn (setq clauses (format-parse-clauses '> T))
               (do ((specs (g-l-p clauses) (cdddr specs)) (str))
                   ((null specs))
                 (multiple-value (args str-code)
                   (format-ctl-string-to-string args (car specs)))
                 (push `(setq str ,str-code) temp-results)
                 (push
                   `(progn
                      (setq string-ncol (+ (string-length str) string-ncol))
                      (setq n-padding-points (1+ n-padding-points))
                      (setq strings (cons-in-area str strings format-temporary-area)))
                   temp-results))))
    (push `(setq strings (nreverse strings)) temp-results)
    (cond ((and (g-l-p clauses) (oddp (cadr (g-l-p clauses))))
           (push `(progn
                    (setq newline (pop strings))
                    (and ,(caddr (g-l-p clauses))
                         (setq extra ,(or (car (g-l-p (caddr (g-l-p clauses)))) 0)
                               linewidth ,(cadr (g-l-p (caddr (g-l-p clauses))))))
                    (setq string-ncol (- string-ncol (string-length newline)))
                    (setq n-padding-points (1- n-padding-points)))
                 temp-results)))
    (push
      `(progn
         (and (zerop n-padding-points)          ;With no options and no ~; right-justify
              (setq colon-flag T n-padding-points 1))
         ;; Get the amount of space needed to print the strings and MINPAD padding
         (setq total-padding (+ (* n-padding-points minpad) string-ncol))
         ;; Now bring in the MINCOL and COLINC constraint, i.e. the total width is
         ;; at least MINCOL and exceeds MINCOL by a multiple of COLINC, and
         ;; get the total amount of padding to be divided among the padding points
         (setq total-padding (- (+ mincol (* colinc (// (+ (max (- total-padding mincol) 0)
                                                           (1- colinc))
                                                        colinc)))
                                string-ncol))
         ;; Figure out whether a newline is called for or not.
         (cond ((and newline
                     (memq ':READ-CURSORPOS w-o)
                     (> (+ (funcall standard-output ':READ-CURSORPOS ':CHARACTER)
                           string-ncol total-padding extra)
                        (or linewidth
                            (and (memq ':SIZE-IN-CHARACTERS w-o)
                                 (funcall standard-output ':SIZE-IN-CHARACTERS))
                            95.)))
                (funcall standard-output ':STRING-OUT newline)))
         ;; Decide how many pads at each padding point + how many of the leftmost
         ;; padding points need one extra pad.
         (setq n-pads (// total-padding n-padding-points)
               n-extra-pads (\ total-padding n-padding-points))
         (or (zerop n-extra-pads) (setq n-pads (1+ n-pads)))
         ;; Output the stuff
         (do ((strings strings (cdr strings))
              (pad-before-p colon-flag t))
             ((null strings))
           (cond (pad-before-p
                  (format-ctl-repeat-char n-pads ,padchar)
                  (and (zerop (setq n-extra-pads (1- n-extra-pads))) (setq n-pads (1- n-pads)))))
           (funcall standard-output ':STRING-OUT (first strings)))
         ;; Finally spacing at the right
         ,@(and atsign-flag `((format-ctl-repeat-char n-pads ,padchar)))
         ;; Reclamation
         (dolist (str (nreverse strings))
           (return-array str))
         (and newline (return-array newline))
         (format-reclaim-clauses clauses)) temp-results)
    (push
      (cons 'LET
            (nreverse temp-results)) format-results)
    args))))

(defprop > format-ctl-end-hairy-justification-macro eval-immediate)
(defun format-ctl-end-hairy-justification-macro (ignore)
  (format-error "Stray ~~> in FORMAT control string"))


;;; This function is like FORMAT-CTL-STRING except that instead of sending to
;;; STANDARD-OUTPUT it sends to a string and returns that as its second value.
;;; The returned string is in the temporary area.
(defun format-ctl-string-to-string-macro (args str)
  (let* ((format-results)
         (args-result (format-ctl-string args str)))
    (values args-result
            `(let ((format-string (make-array 200 ':AREA format-temporary-area
                                              ':TYPE 'ART-STRING
                                              ':LEADER-LIST '(0)))
                   (standard-output 'FORMAT-STRING-STREAM))
               ,@(nreverse format-results)
               (adjust-array-size format-string (array-active-length format-string))))))
