;;; -*- Mode:LISP; Package:USER; Base:10; Readtable:CL -*-

(defvar @-package)

(defvar *loaded-fonts*)
(defvar *dvi-stream*)

(defvar *h*)
(defvar *v*)
(defvar *w*)
(defvar *x*)
(defvar *y*)
(defvar *z*)
(defvar *f*)
(defvar *f-tfm*)

(defstruct (dvi-pos (:type :named-array))
  h v w x y z)

(defvar *dvi-pos-stack* nil)

(defun dvi-push-pos ()
  (let ((pos (make-dvi-pos)))
    (setf (dvi-pos-h pos) *h*)
    (setf (dvi-pos-v pos) *v*)
    (setf (dvi-pos-w pos) *w*)
    (setf (dvi-pos-x pos) *x*)
    (setf (dvi-pos-y pos) *y*)
    (setf (dvi-pos-z pos) *z*)
    (push pos *dvi-pos-stack*)))

(defun dvi-pop-pos ()
  (let ((pos (pop *dvi-pos-stack*)))
    (setq *h* (dvi-pos-h pos))
    (setq *v* (dvi-pos-v pos))
    (setq *w* (dvi-pos-w pos))
    (setq *x* (dvi-pos-x pos))
    (setq *y* (dvi-pos-y pos))
    (setq *z* (dvi-pos-z pos))))

(defun dvi-stream-position ()
  (cond ((eq *dvi-stream* 'si:null-stream) 0)
        (t
         (send *dvi-stream* :read-pointer))))

(defun dvi-put-8b (val)
  (setq val (round val))
  (send *dvi-stream* :tyo (ldb (byte 8 0) val)))

(defun dvi-put-16b (val)
  (setq val (round val))
  (send *dvi-stream* :tyo (ldb (byte 8 8) val))
  (send *dvi-stream* :tyo (ldb (byte 8 0) val)))

(defun dvi-put-24b (val)
  (setq val (round val))
  (send *dvi-stream* :tyo (ldb (byte 8 16.) val))
  (send *dvi-stream* :tyo (ldb (byte 8 8) val))
  (send *dvi-stream* :tyo (ldb (byte 8 0) val)))

(defun dvi-put-32b (val)
  (setq val (round val))
  (send *dvi-stream* :tyo (ldb (byte 8 24.) val))
  (send *dvi-stream* :tyo (ldb (byte 8 16.) val))
  (send *dvi-stream* :tyo (ldb (byte 8 8) val))
  (send *dvi-stream* :tyo (ldb (byte 8 0) val)))

(defun dvi-set-char (char)
  "Typeset CHAR, then move right by its width."
  (let ((int (char-int char)))
    (when (>= int 256.) (ferror nil "not supported"))
    (when (>= int 128.)
      (dvi-put-8b 128.))
    (dvi-put-8b (ldb (byte 7 0) int)))
  (incf *h* (dvi-char-width char)))

(defun dvi-set-rule (height width)
  "Typeset black box of HEIGHT, WIDTH, then move right by WIDTH."
  (dvi-put-8b 132.)
  (dvi-put-32b height)
  (dvi-put-32b width)
  (incf *h* width))

(defun dvi-put-char (char)
  "Typeset CHAR, but don't move."
  (put-integer 133. char))

(defun dvi-put-rule (height width)
  "Typeset black box of HEIGHT, WIDTH, but don't move."
  (dvi-put-8b 137.)
  (dvi-put-32b height)
  (dvi-put-32b width))

(defun dvi-nop ()
  (dvi-put-8b 138.))

(defvar *last-bop*)
(defvar *number-of-pages*)

;;;prev should be -1 first time, then file offset of previous bop
(defun dvi-bop (c0 c1 c2 c3 c4 c5 c6 c7 c8 c9)
  (setq *h* 0)
  (setq *v* 0)
  (setq *w* 0)
  (setq *x* 0)
  (setq *y* 0)
  (setq *z* 0)
  (setq *f* nil)
  (setq *dvi-pos-stack* nil)
  (let ((this-bop (dvi-stream-position)))
    (dvi-put-8b 139.)
    (dvi-put-32b c0)
    (dvi-put-32b c1)
    (dvi-put-32b c2)
    (dvi-put-32b c3)
    (dvi-put-32b c4)
    (dvi-put-32b c5)
    (dvi-put-32b c6)
    (dvi-put-32b c7)
    (dvi-put-32b c8)
    (dvi-put-32b c9)
    (dvi-put-32b *last-bop*)
    (setq *last-bop* this-bop))
  (incf *number-of-pages*)
  (dvi-push)
  )

(defun dvi-eop ()
  (dvi-pop)
  (when (not (null *dvi-pos-stack*))
    (ferror nil "pos stack not empty"))
  (dvi-put-8b 140.))

(defun dvi-push ()
  (dvi-push-pos)
  (dvi-put-8b 141.))

(defun dvi-pop ()
  (dvi-pop-pos)
  (dvi-put-8b 142.))

(defun put-integer (command-base val)
  (setq val (round val))
  (cond ((and (<= -128. val) (< val 128.))
         (dvi-put-8b command-base)
         (dvi-put-8b val))
        ((and (<= -32768. val) (< val 32768.))
         (dvi-put-8b (+ command-base 1))
         (dvi-put-16b val))
        ((and (<= (- (ash 1 23.)) val) (< val (ash 1 23.)))
         (dvi-put-8b (+ command-base 2))
         (dvi-put-24b val))
        ((and (<= (- (ash 1 31.)) val) (< val (ash 1 31.)))
         (dvi-put-8b (+ command-base 3))
         (dvi-put-32b val))
        (t
         (ferror nil "too big"))))

(defun dvi-right (distance &optional set-pos-register)
  (cond ((= distance *w*)
         (dvi-put-8b 147.))
        ((= distance *x*)
         (dvi-put-8b 152.))
        (t
         (put-integer (ecase set-pos-register
                        (w (setq *w* distance) 148.)
                        (x (setq *x* distance) 153.)
                        (nil 143.))
                      distance)))
  (incf *h* distance))

(defun dvi-down (distance &optional set-pos-register)
  (cond ((= distance *y*)
         (dvi-put-8b 161.))
        ((= distance *z*)
         (dvi-put-8b 166.))
        (t
         (put-integer (ecase set-pos-register
                        (y (setq *y* distance) 162.)
                        (z (setq *z* distance) 167.)
                        (nil 157.))
                      distance)))
  (incf *v* distance))

(defun dvi-font (font-number &aux font-info)
  (dolist (f *loaded-fonts*
             (ferror nil "font ~d not found" font-number))
    (when (= (cadr f) font-number)
      (setq font-info f)
      (return nil)))
  (cond ((< font-number 64.)
         (dvi-put-8b (+ 171. font-number)))
        (t
         (put-integer 235. font-number)))
  (setq *f* font-number)
  (setq *f-tfm* (caddr font-info)))

(defun dvi-special (string)
  (put-integer 239. (string-length string))
  (dotimes (i (string-length string))
    (dvi-put-8b (aref string i))))

(defun dvi-define-font (font-number checksum scale-factor design-size font-directory font-name)
  (put-integer 243. font-number)
  (dvi-put-32b checksum)
  (dvi-put-32b scale-factor)
  (dvi-put-32b design-size)
  (when (>= (string-length font-directory) 256.)
    (ferror nil "font-directory is too long"))
  (dvi-put-8b (string-length font-directory))
  (when (>= (string-length font-name) 256.)
    (ferror nil "font-name is too long"))
  (dvi-put-8b (string-length font-name))
  (dotimes (i (string-length font-directory))
    (dvi-put-8b (aref font-directory i)))
  (dotimes (i (string-length font-name))
    (dvi-put-8b (aref font-name i))))

;beginning of preamble
(defun dvi-pre (comment)
  (dvi-put-8b 247.)
  (dvi-put-8b 2)                                ;dvi format version
  (dvi-put-32b 25400000.)                       ;size scale, same as TEX
  (dvi-put-32b 473628672.)
  (dvi-put-32b 1000.)                           ;magnification * 1000.
  (when (>= (string-length comment) 256.)
    (ferror nil "comment too long"))
  (dvi-put-8b (string-length comment))
  (dotimes (i (string-length comment))
    (dvi-put-8b (aref comment i))))

(defvar *post-position*)

;beginning of postamble
(defun dvi-post ()
  (setq *post-position* (dvi-stream-position))
  (dvi-put-8b 248.)
  (dvi-put-32b *last-bop*)
  (dvi-put-32b 25400000.)                       ;size scale, same as TEX
  (dvi-put-32b 473628672.)
  (dvi-put-32b 1000.)                           ;magnification * 1000.
  (dvi-put-32b 0)                               ;height of largest page
  (dvi-put-32b 0)                               ;width of largest page
  (dvi-put-16b 5)                               ;maximum stack depth
  (dvi-put-16b *number-of-pages*)
  ;;then put out font definitions again
  )

;end of postamble
(defun dvi-post-post ()
  (dvi-put-8b 249.)
  (dvi-put-32b *post-position*)
  (dvi-put-8b 2)
  (dotimes (i 4)
    (dvi-put-8b 223.))
  (do ()
      ((zerop (ldb (byte 2 0) (dvi-stream-position))))
    (dvi-put-8b 223.)))




(defun read-bigendian-8b-unsigned (stream)
  (send stream :tyi))

(defun read-bigendian-16b-unsigned (stream)
  (let* ((b1 (send stream :tyi))
         (b0 (send stream :tyi)))
    (dpb b1 (byte 8 8) b0)))

(defun read-bigendian-24b-unsigned (stream)
  (let* ((b2 (send stream :tyi))
         (b1 (send stream :tyi))
         (b0 (send stream :tyi)))
    (dpb b2 (byte 8 16.)
         (dpb b1 (byte 8 8) b0))))

(defun read-bigendian-32b-unsigned (stream)
  (let* ((b3 (send stream :tyi))
         (b2 (send stream :tyi))
         (b1 (send stream :tyi))
         (b0 (send stream :tyi)))
    (dpb b3 (byte 8 24.)
         (dpb b2 (byte 8 16.)
              (dpb b1 (byte 8 8) b0)))))

(defstruct (tfm (:type :named-array))
  file-length                                   ;in words
  header-length                                 ;in words
  smallest-char
  largest-char
  width-table-length
  height-table-length
  depth-table-length
  italic-table-length
  lig-table-length
  kern-table-length
  extend-table-length
  param-table-length
  checksum
  design-size
  coding-scheme
  font-family
  seven-bit-safe-flag
  width
  height
  depth
  italic
  )

(defun fix-word-to-small-float (num)
  (let ((factor 1))
    (when (ldb-test (byte 1 31.) num)
      (setq factor -1)
      (setq num (logand #o37777777777 (- num))))
    (* factor (+ (small-float (ldb (byte 12. 20.) num))
                 (/ (small-float (ldb (byte 20. 0) num))
                    (^ 2 20.))))))

(defun fix-word-to-ratio (num)
  (let ((factor 1))
    (when (ldb-test (byte 1 31.) num)
      (setq factor -1)
      (setq num (logand #o37777777777 (- num))))
    (* factor (+ (ldb (byte 12. 20.) num)
                 (/ (ldb (byte 20. 0) num) (ash 1 20.))))))

(defun read-tfm-file (name)
  (let ((tfm (make-tfm)))
    (with-open-file (stream name
                            :direction :input
                            :byte-size 8
                            :raw t)

      (setf (tfm-file-length tfm) (read-bigendian-16b-unsigned stream))
      (setf (tfm-header-length tfm) (read-bigendian-16b-unsigned stream))
      (setf (tfm-smallest-char tfm) (read-bigendian-16b-unsigned stream))
      (setf (tfm-largest-char tfm) (read-bigendian-16b-unsigned stream))
      (setf (tfm-width-table-length tfm) (read-bigendian-16b-unsigned stream))
      (setf (tfm-height-table-length tfm) (read-bigendian-16b-unsigned stream))
      (setf (tfm-depth-table-length tfm) (read-bigendian-16b-unsigned stream))
      (setf (tfm-italic-table-length tfm) (read-bigendian-16b-unsigned stream))
      (setf (tfm-lig-table-length tfm) (read-bigendian-16b-unsigned stream))
      (setf (tfm-kern-table-length tfm) (read-bigendian-16b-unsigned stream))
      (setf (tfm-extend-table-length tfm) (read-bigendian-16b-unsigned stream))
      (setf (tfm-param-table-length tfm) (read-bigendian-16b-unsigned stream))

      (send stream :set-pointer 24.)
      (when (< (tfm-header-length tfm) 2)
        (ferror nil "bad tfm file"))
      (setf (tfm-checksum tfm) (read-bigendian-32b-unsigned stream))
      (setf (tfm-design-size tfm) (fix-word-to-ratio (read-bigendian-32b-unsigned stream)))
      (cond ((> (tfm-header-length tfm) 2)
             (when (< (tfm-header-length tfm) 12.)
               (ferror nil "bad tfm file"))
             (let ((length (read-bigendian-8b-unsigned stream)))
               (when (or (< length 0) (> length 39.))
                 (ferror nil "bad tfm file"))
               (let ((string (make-string length)))
                 (dotimes (i 39.)
                   (let ((c (read-bigendian-8b-unsigned stream)))
                     (when (< i length)
                       (setf (aref string i) c))))
                 (setf (tfm-coding-scheme tfm) string)))))
      (cond ((> (tfm-header-length tfm) 12.)
             (when (< (tfm-header-length tfm) 17.)
               (ferror nil "bad tfm file"))
             (let ((length (read-bigendian-8b-unsigned stream)))
               (when (or (< length 0) (> length 19.))
                 (ferror nil "bad tfm file"))
               (let ((string (make-string length)))
                 (dotimes (i 19.)
                   (let ((c (read-bigendian-8b-unsigned stream)))
                     (when (< i length)
                       (setf (aref string i) c))))
                 (setf (tfm-font-family tfm) string)))))
      (when (>= (tfm-header-length tfm) 17.)
        (setf (tfm-seven-bit-safe-flag tfm) (read-bigendian-8b-unsigned stream))
        (read-bigendian-8b-unsigned stream)
        (read-bigendian-8b-unsigned stream)
        (read-bigendian-8b-unsigned stream)     ;face code - don't appear to be really in any tfm files
        )
      ;;other header words are ignored

      ;;char-info
      (let ((char-info (make-array (1+ (- (tfm-largest-char tfm) (tfm-smallest-char tfm)))))
            (width (make-array (tfm-width-table-length tfm)))
            (height (make-array (tfm-height-table-length tfm)))
            (depth (make-array (tfm-depth-table-length tfm)))
            (italic (make-array (tfm-italic-table-length tfm)))
            (lig (make-array (tfm-lig-table-length tfm)))
            (kern (make-array (tfm-kern-table-length tfm)))
            (extend (make-array (tfm-extend-table-length tfm)))
            (param (make-array (tfm-param-table-length tfm))))
        (send stream :set-pointer (+ 24. (* 4 (tfm-header-length tfm))))
        (dotimes (i (array-length char-info))
          (setf (aref char-info i) (read-bigendian-32b-unsigned stream)))

        ;;width
        (dotimes (i (array-length width))
          (setf (aref width i) (read-bigendian-32b-unsigned stream)))

        ;;height
        (dotimes (i (array-length height))
          (setf (aref height i) (read-bigendian-32b-unsigned stream)))

        ;;depth
        (dotimes (i (array-length depth))
          (setf (aref depth i) (read-bigendian-32b-unsigned stream)))

        ;;italic
        (dotimes (i (array-length italic))
          (setf (aref italic i) (read-bigendian-32b-unsigned stream)))

        ;;lig
        (dotimes (i (array-length lig))
          (setf (aref lig i) (read-bigendian-32b-unsigned stream)))

        ;;kern
        (dotimes (i (array-length kern))
          (setf (aref kern i) (read-bigendian-32b-unsigned stream)))

        ;;extend
        (dotimes (i (array-length extend))
          (setf (aref extend i) (read-bigendian-32b-unsigned stream)))

        ;;param
        (dotimes (i (array-length param))
          (setf (aref param i) (read-bigendian-32b-unsigned stream)))

        (let ((n-chars (1+ (- (tfm-largest-char tfm) (tfm-smallest-char tfm)))))
          (setf (tfm-width tfm) (make-array n-chars))
          (setf (tfm-height tfm) (make-array n-chars))
          (setf (tfm-depth tfm) (make-array n-chars))
          (setf (tfm-italic tfm) (make-array n-chars)))
        (do ((char (tfm-smallest-char tfm) (1+ char))
             (end (tfm-largest-char tfm)))
            ((> char end))
          (let* ((char-info (aref char-info char))
                 (width-index (ldb (byte 8 24.) char-info))
                 (height-index (ldb (byte 4 20.) char-info))
                 (depth-index (ldb (byte 4 16.) char-info))
                 (italic-index (ldb (byte 6 10.) char-info))
                 (tag (ldb (byte 2 8) char-info))
                 (remainder (ldb (byte 8 0) char-info)))
            tag remainder
            (setf (aref (tfm-width tfm) char) (fix-word-to-ratio (aref width width-index)))
            (setf (aref (tfm-height tfm) char) (fix-word-to-ratio (aref height height-index)))
            (setf (aref (tfm-depth tfm) char) (fix-word-to-ratio (aref depth depth-index)))
            (setf (aref (tfm-italic tfm) char) (fix-word-to-ratio (aref italic italic-index)))
            )))

      )
    tfm))


(defconst *font-directory* "angel:/usr/lib/tex/fonts/")

(defun output-font-definition (font-info)
  (let ((name (car font-info))
        (number (cadr font-info))
        (tfm (caddr font-info)))
    (dvi-define-font number
                     (tfm-checksum tfm)
                     (* (^ 2 16.) 10.)
                     (* (^ 2 16.) (tfm-design-size tfm))
                     ""
                     name)))

(defun load-font (font-number name)
  (let ((tfm (read-tfm-file (send (fs:merge-pathname-defaults name *font-directory*)
                                  :new-raw-type "tfm"))))
    (let ((font-info (list name font-number tfm)))
      (setq *loaded-fonts* (append *loaded-fonts* (list font-info)))
      (output-font-definition font-info))))

(defun dvi-char-width (char)
  (* (tfm-design-size *f-tfm*)
     (aref (tfm-width *f-tfm*) char)
     (^ 2 16.)))

(defun dvi-char-height (char)
  (* (tfm-design-size *f-tfm*)
     (aref (tfm-height *f-tfm*) char)
     (^ 2 16.)))

(defconst *page-width-in-points* (* 7 72))      ;7 inches
(defconst *normal-page-width-in-points* (* (+ 6 1/2) 72.))
(defconst *page-height-in-points* (* 10 72))    ;10 inches
(defconst *normal-page-height-in-points* (* (+ 9 1/2) 72))
(defconst *lines-per-inch* 6)
(defconst *points-per-line* (/ 72. 6))

(defun dvi-next-line ()
  (let ((v *v*))
    ;;reset to top of page
    (dvi-pop)
    (dvi-push)
    (dvi-down (+ v (* (^ 2 16.) (/ 72. *lines-per-inch*))) nil)))

(defun dvi-beginning-of-line ()
  (let ((v *v*))
    ;;reset to top of page
    (dvi-pop)
    (dvi-push)
    (dvi-down v nil)))

(defun points->scaled-points (points)
  (* (^ 2 16.) points))

(defun scaled-points->points (scaled-points)
  (/ scaled-points (^ 2 16.)))

(defun dvi-set-line (string)
  (when (string-search-char #\tab string)
    (setq string (with-output-to-string (s)
                   (do ((col 0)
                        (i 0 (1+ i))
                        (end (string-length string)))
                       ((= i end))
                     (cond ((char= (aref string i) #\tab)
                            (send s :tyo #\space)
                            (incf col)
                            (loop until (zerop (ldb (byte 3 0) col))
                                  do (progn (send s :tyo #\space)
                                            (incf col)))
                            )
                           (t
                            (send s :tyo (aref string i))
                            (incf col)))))))
  (let ((end (* *normal-page-width-in-points* (^ 2 16.)))
        (semi (string-search-char #\; string)))
    (labels ((put-char-and-check-for-end (c)
                                         (cond ((char= c #\space)
                                                (dvi-right (dvi-char-width #\X) 'w))
                                               (t
                                                (dvi-set-char c)))
                                         (when (>= *h* end)
                                           (dvi-put-rule (dvi-char-height #\X)
                                                         (dvi-char-width #\X))
                                           (return-from dvi-set-line nil))))
      (dvi-font 0)
      (dotimes (i (or semi (string-length string)))
        (put-char-and-check-for-end (aref string i)))
      (when semi
        (dvi-font 1)
        (do ((i semi (1+ i)))
            ((= i (string-length string)))
          (put-char-and-check-for-end (aref string i)))))))

(defmacro with-dvi-environment ((output-file-name) &body body)
  `(let ((*last-bop* -1)
         (*number-of-pages* 0)
         (*loaded-fonts* nil)
         (*f* nil)
         (*f-tfm* nil)
         *h* *v* *w* *x* *y* *z*)
     (with-open-stream (*dvi-stream*
                         (cond ((null ,output-file-name)
                                'si:null-stream)
                               (t
                                (open ,output-file-name
                                      :direction :output
                                      :byte-size 8
                                      :raw t))))
       (dvi-pre "LISPM DVI")
       ,@body
       (dvi-post)
       (output-fonts-in-decending-order)
       (dvi-post-post))))

(defun output-fonts-in-decending-order ()
  (dolist (font-info (sort *loaded-fonts*
                           #'(lambda (f1 f2)
                               (> (cadr f1) (cadr f2)))))
    (output-font-definition font-info)))


(defun dvi-test-2 ()
  (with-dvi-environment ("angel:/lmi/pace/dvi/dvi2.dvi")
    (load-font 0 "amtt10")
    (dvi-new-page-with-header-and-line
      (fs:parse-pathname "angel:/lmi3/pace/t/comp/top/top.t") 5)
    (dvi-set-line "This is a test.")
    (dvi-next-line)
    (dvi-set-line (make-string 100 :initial-element #\x))
    (dvi-eop)))

(defun dvi-new-page-with-header-and-line (page-info page-number)
  (dvi-bop page-number 0 0 0 0 0 0 0 0 0)
  (dvi-font 0)
  (dvi-set-line
    (cond ((stringp page-info)
           (dvi-make-simple-header page-info page-number))
          (t
           (dvi-make-file-header (car page-info) (caddr page-info) page-number))))
  (dvi-pop)
  (dvi-push)
  (dvi-down (points->scaled-points (/ *points-per-line* 2)))
  (dvi-put-rule (points->scaled-points 3/4)
                (points->scaled-points *normal-page-width-in-points*))
  (dvi-down (points->scaled-points *points-per-line*))
  )

(defun dvi-make-simple-header (center-string page-number)
  (let ((n-chars (floor *normal-page-width-in-points*
                        (scaled-points->points (dvi-char-width #\0))))
        (time (time:get-universal-time)))
    (let ((header (make-string n-chars :initial-element #\space)))
      (let ((start (- (floor n-chars 2) (floor (string-length center-string) 2))))
        (copy-array-portion center-string 0 (string-length center-string)
                            header start (+ start (string-length center-string))))
      (let ((page-string (format nil "Page ~d" page-number))
            (date-string (time:print-universal-time time nil nil :|DD MMM YY|)))
        (cond ((evenp page-number)
               (copy-array-portion page-string 0 (string-length page-string)
                                   header 0 (string-length page-string))
               (copy-array-portion date-string 0 (string-length date-string)
                                   header (- n-chars (string-length date-string)) n-chars))
              (t
               (copy-array-portion date-string 0 (string-length date-string)
                                   header 0 (string-length date-string))
               (copy-array-portion page-string 0 (string-length page-string)
                                   header (- n-chars (string-length page-string)) n-chars))))
      header)))


(defun dvi-make-file-header (pathname creation-date page-number)
  (let ((n-chars (floor *normal-page-width-in-points*
                        (scaled-points->points (dvi-char-width #\0))))
        file-name
        )
    (let ((dir (send pathname :directory)))
      (when (consp dir)
        (setq dir (car (last dir))))
      (setq file-name (send (send pathname :new-directory (list :relative dir))
                            :string-for-host)))
    (let ((header (make-string n-chars :initial-element #\space)))
      (let ((start (- (floor n-chars 2) (floor (string-length file-name) 2))))
        (copy-array-portion file-name 0 (string-length file-name)
                            header start (+ start (string-length file-name))))
      (let ((page-string (format nil "Page ~d" page-number))
            (date-string (time:print-universal-time creation-date nil nil :|DD MMM YY|)))
        (cond ((evenp page-number)
               (copy-array-portion page-string 0 (string-length page-string)
                                   header 0 (string-length page-string))
               (copy-array-portion date-string 0 (string-length date-string)
                                   header (- n-chars (string-length date-string)) n-chars))
              (t
               (copy-array-portion date-string 0 (string-length date-string)
                                   header 0 (string-length date-string))
               (copy-array-portion page-string 0 (string-length page-string)
                                   header (- n-chars (string-length page-string)) n-chars))))
      header)))

(defun dvi-test ()
  (let ((*last-bop* -1)
        (*number-of-pages* 0)
        (*loaded-fonts* nil)
        (*f* nil)
        (*f-tfm* nil)
        )
    (with-open-file (*dvi-stream* "angel:/lmi/pace/dvi/dvi-test.dvi"
                                  :direction :output
                                  :byte-size 8
                                  :raw t
                                  )
      (dvi-pre "Testing")
      (load-font 0 "amtt10")
      (dvi-bop 1 0 0 0 0 0 0 0 0 0)
      (dvi-font 0)
      (dotimes (i 15.)
        (dvi-put-char (+ #\0 (mod i 10.)))
        (dvi-right (* 10. (dvi-char-width #\0)) nil))
      (dvi-next-line)
      (dotimes (i 150.)
        (dvi-set-char (+ #\0 (mod i 10.))))
      (dotimes (i 90.)
        (dvi-next-line)
        (dvi-put-char (+ #\0 (mod (+ i 2) 10.))))
      (dvi-eop)
      (dvi-post)
      (dolist (font-info *loaded-fonts*)
        (output-font-definition font-info))
      (dvi-post-post)
      )))

(defvar *page-list* nil)

(defun read-file (file-name &aux line-list creation-date)
  (setq file-name (fs:parse-pathname file-name))
  (with-open-file (f file-name)
    (setq creation-date (send f :creation-date))
    (do (last-line
         line eof-p
         )
        (())
      (labels ((substring-with-leader (string start &optional end)
                 (setq end (or end (string-length string)))
                 (let ((new-string (make-array (- end start)
                                               :type :art-string
                                               :leader-length 3)))
                   (copy-array-portion string start end
                                       new-string 0 (- end start))
                   new-string))
               (add-line (x)
                 (cond ((null line-list)
                        (setq line-list x)
                        (setq last-line x))
                       (t
                        (setf (array-leader last-line 1) x)
                        (setq last-line x)))))
        (multiple-value-setq (line eof-p)
          (send f :line-in 3))
        (when eof-p
          (return nil))
        (do-forever
          (let ((page-char (string-search-char #\page line)))
            (cond ((null page-char)
                   (add-line line)
                   (return nil))
                  (t
                   (when (not (= page-char 0))
                     (add-line (substring-with-leader line 0 page-char)))
                   (add-line (substring-with-leader (string #\page) 0))
                   (when (= (1+ page-char) (string-length line))
                     (return nil))
                   (setq line (substring-with-leader line (1+ page-char))))))))))
  (values line-list creation-date))

(defun break-long-lines (lines max-chars)
  (do ((line lines (array-leader line 1)))
      ((null line)
       lines)
    (labels ((substring-with-leader (string start &optional end)
                                    (setq end (or end (string-length string)))
                                    (let ((new-string (make-array (- end start)
                                                                  :type :art-string
                                                                  :leader-length 3)))
                                      (copy-array-portion string start end
                                                          new-string 0 (- end start))
                                      new-string)))
      (do (rest)
          ((<= (string-length line) max-chars))
        (setq rest (substring-with-leader line max-chars))
        (setf (array-leader rest 1) (array-leader line 1))
        (setf (array-leader line 1) rest)
        (setf (fill-pointer line) (+ max-chars 1))
        (aset #\! line max-chars)
        (setq line rest)))))


(defun print-lines (line)
  (do ((l line (array-leader l 1)))
      ((null l))
    (print l)))

(defun print-page (line-list)
  (dolist (line line-list)
    (print-lines line)
    (format t "~&*************************************************")))

(defun line-has-code-p (line)
  (let ((first-non-blank (string-search-not-set '(#\space #\tab) line)))
    (and first-non-blank
         (not (char= (aref line first-non-blank) #\;)))))

;;;well, not quite - if there are excess closes, this doesn't care
(defun line-contains-balanced-parens-p (line)
  (let ((stripped-line (with-output-to-string (s)
                         (dotimes (i (array-length line))
                           (cond ((char= (aref line i) #\() (send s :tyo #\())
                                 ((char= (aref line i) #\)) (send s :tyo #\))))))))
    (not (nth-value 1 (ignore-errors (read-from-string stripped-line))))))

(defun end-of-this-defun (line-list &aux start last-non-blank)
  (cond ((and (> (string-length line-list) 0)
              (char= (aref line-list 0) #\())
         (cond ((line-contains-balanced-parens-p line-list)
                (return-from end-of-this-defun line-list))
               (t
                (setq start (array-leader line-list 1))
                (setq last-non-blank line-list))))
        (t
         (setq start line-list)
         (setq last-non-blank line-list)))
  (do ((line (array-leader line-list 1) (array-leader line 1)))
      ((or (null line)
           (and (> (string-length line) 0)(char= (aref line 0) #\( )))
       last-non-blank)
    (when (line-has-code-p line)
      (setq last-non-blank line))))

(defun next-nonblank-line (from-line)
  (do ((line from-line (array-leader line 1)))
      ((null line)
       nil)
    (when (string-search-not-set '(#\space #\tab) line)
      (return line))))

(defun count-lines-inclusive (from to)
  (do ((count 1 (1+ count))
       (line from (array-leader line 1)))
      ((null line)
       (ferror nil "couldn't find ~s" to))
    (when (eq line to)
      (return count))))

(defun beginning-of-next-defun (from-line)
  (do ((line from-line (array-leader line 1)))
      ((null line)
       nil)
    (when (and (> (string-length line) 0) (char= (aref line 0) #\())
      (return line))))

(defun break-file-into-pages (line-list)
  (let ((lines-per-page (/ *normal-page-height-in-points*
                           *points-per-line*))
        new-line-list (line-number 0) aux-line page-list
        beginning-of-next-defun end-of-this-defun lines-in-this-defun)
    (labels ((maybe-add-line (line)
                             (when (or (not (eq (car new-line-list) :page)) (string-search-not-set '(#\space #\tab) line))
                               (push line new-line-list)
                               (incf line-number)
                               (when (>= line-number lines-per-page)
                                 (push :page new-line-list)
                                 (setq line-number 0))))
             (done ()
                   (loop until (not (eq :page (car new-line-list)))
                         do (pop new-line-list))
                   (setf (array-leader (car new-line-list) 1) nil)
                   (do ((line-list (cdr new-line-list))
                        (line (car new-line-list)))
                       (())
                     (cond ((null line-list)
                            (push line page-list)
                            (return nil))
                           ((eq (car line-list) :page)
                            (push line page-list)
                            (loop until (not (eq :page (car line-list)))
                                  do (pop line-list))
                            (when (null line-list)
                              (return nil))
                            (setf (array-leader (car line-list) 1) nil)
                            (setq line (car line-list))
                            (pop line-list))
                           (t
                            (setf (array-leader (car line-list) 1) line)
                            (setq line (car line-list))
                            (pop line-list))))
                   (return-from break-file-into-pages  page-list))
             (dump-interval (from-line to-line)
               (do ((line from-line (array-leader line 1)))
                   ((null line)
                    (when (not (null to-line))
                      (ferror nil "couldn't find ~s" to-line)))
                 (maybe-add-line line)
                 (when (eq line to-line)
                   (return nil))))
             )
      ;;dump everything to end of first defun
      (setq aux-line (beginning-of-next-defun line-list))
      (when aux-line
        (setq aux-line (end-of-this-defun aux-line)))
      (dump-interval line-list aux-line)

      (do ((line aux-line))
          ((null line)
           (done))
        (setq line (array-leader line 1))
        (setq beginning-of-next-defun (beginning-of-next-defun line))

        (when (null beginning-of-next-defun)
          (dump-interval line nil)
          (done))

        (setq end-of-this-defun (end-of-this-defun beginning-of-next-defun))
        (setq lines-in-this-defun (count-lines-inclusive line end-of-this-defun))
        (when (and (> (+ line-number lines-in-this-defun) lines-per-page)
                   (or (< lines-in-this-defun (* 3/4 lines-per-page))
                       (> line-number (* 1/2 lines-per-page))))
          (push :page new-line-list)
          (setq line-number 0))
        (dump-interval line end-of-this-defun)
        (setq line end-of-this-defun)
        ))))

(defvar *chars-per-line*)

(defun read-file-set (set)
  (setq *page-list* nil)
  (setq *chars-per-line* (compute-chars-per-line))
  (dolist (file set)
    (let ((pathname (fs:parse-pathname file)))
      (multiple-value-bind (lines creation-date)
          (read-file file)
        (let ((page-list (break-file-into-pages
                           (break-long-lines lines *chars-per-line*))))
          (dolist (page page-list)
            (setq *page-list* (nconc *page-list*
                                     (cons (list pathname page creation-date) nil)))))))))

(defvar *small-orbit-set*
        '("angel:/lmi3/pace/t/comp/front_end/param.t"
          "angel:/lmi3/pace/t/comp/front_end/front.t"))

(defvar *font-for-code* "amtt10")

(defun compute-chars-per-line ()
  (with-dvi-environment (nil)
    (load-font 0 *font-for-code*)
    (dvi-bop 1 0 0 0 0 0 0 0 0 0)
    (dvi-font 0)
    (return-from compute-chars-per-line
      (values (round (/ *normal-page-width-in-points*
                        (scaled-points->points (dvi-char-width #\0))))))))


(defun print-page-list ()
  (with-dvi-environment ("angel:/lmi/pace/dvi/dvi2.dvi")
    (load-font 0 "amtt10")
    (load-font 1 "amsltt10")
    (do ((pages *page-list* (cdr pages))
         (page-number 1 (1+ page-number)))
        ((null pages))
      (dvi-new-page-with-header-and-line (car pages) page-number)
      (do ((line (cadr (car pages)) (array-leader line 1)))
          ((null line))
        (dvi-set-line line)
        (dvi-next-line))
      (dvi-eop))))



(defun dvi-test-3 ()
  (with-dvi-environment ("angel:/lmi/pace/dvi/dvi3.dvi")
    (load-font 0 "amtt10")
    (load-font 1 "amsltt10")
    (dvi-bop 1 0 0 0 0 0 0 0 0 0)
    (dvi-font 0)
    (dvi-set-line "foobar")
    (dvi-eop)
    ))

(defun find-symbols-on-line (line &aux symbols)
  (labels ((put-symbol (string start end)
                       (do ((i start (1+ i)))
                           ((= i end)
                            (return-from put-symbol nil))
                         (when (not (digit-char-p (aref string i)))
                           (return nil)))
                       (push (intern (string-upcase (substring string start end)) @-package) symbols)))

    (do ((i 0 (1+ i))
         start-of-symbol
         (end (or (string-search-char #\; line)
                  (string-length line))))
        ((= i end)
         (when start-of-symbol
           (put-symbol line start-of-symbol end)))
      (cond ((null start-of-symbol)
             (when (not (memq (aref line i)
                              '(#\( #\) #\# #\\ #\` #\, #\' #\space #\tab)))
               (setq start-of-symbol i)))
            ((memq (aref line i)
                   '(#\( #\) #\# #\\ #\` #\, #\' #\space #\tab))
             (put-symbol line start-of-symbol i)
             (setq start-of-symbol nil))))
    symbols))

(defvar *xpage-list*)

(defmacro line-page-number (page)
  `(getf (array-leader ,page 2) :page-number))

(defmacro line-references-from-page (page)
  `(getf (array-leader ,page 2) :references))

(defun assign-page-numbers (&aux new-list)
  (dolist (page-info *page-list*)
    (push nil new-list)
    (push page-info new-list))
  (setq *xpage-list* (cons nil (reverse new-list)))
  ;;First NIL is cover
  ;;Then a NIL for refs on the fisrt page of code
  ;;Then the first page of code (page 3)
  (do ((page-number 1 (1+ page-number))
       (page *xpage-list* (cdr page)))
      ((null page))
    (when (car page)
      (do ((line (cadar page) (array-leader line 1)))
          ((null line))
        (setf (line-page-number line) page-number))))
  )

(defun find-references ()
  (dolist (page-info *xpage-list*)
    (when page-info
      (let ((page-header (cadr page-info)))
        (do ((line page-header (array-leader line 1)))
            ((null line))
          (let ((symbols (find-symbols-on-line line)))
            (dolist (sym symbols)
              (pushnew sym (line-references-from-page page-header))
              (pushnew page-header (getf (symbol-plist sym) :referenced-by)))))))))

(defun find-definitions ()
  (dolist (page-info *xpage-list*)
    (when page-info
      (let ((page-header (cadr page-info)))
        (do ((line page-header (array-leader line 1)))
            ((null line))
          (when (string-equal line "(define" :end1 7)
            (let (func)
              (let ((*package* @-package)
                    (*read-base* 10.)
                    (*readtable* (si:find-readtable-named "CL")))
                (setq func (ignore-errors (read-from-string (substring line 7)))))
              (loop until (not (consp func))
                    do (setq func (car func)))
              (when func
                (pushnew page-header (getf (symbol-plist func) :defined-on))))))))))

(defvar ignore-list)

(defun prepare-even-page (odd-page &aux even-page last-line)
  (labels ((add-line (line bold-p)
                     (let ((new-line (make-array (string-length line)
                                                 :type :art-string
                                                 :leader-length 3)))
                       (copy-array-contents line new-line)
                       (setq line new-line))
                     (when bold-p
                       (setf (getf (array-leader line 2) :bold) t))
                     (cond ((null even-page)
                            (setq even-page line)
                            (setq last-line line))
                           (t
                            (setf (array-leader last-line 1) line)
                            (setq last-line line)))))
  (let ((references (sort (line-references-from-page odd-page)
                          #'(lambda (a b) (string< (string a) (string b))))))
    (dolist (ref references)
      (when (and (not (memq ref ignore-list))
                 (getf (symbol-plist ref) :defined-on))
        (let ((first (format nil "~a:~{~8d~}" ref (remove-duplicates (sort (mapcar #'(lambda (x)
                                                                                       (line-page-number x))
                                                                                   (getf (symbol-plist ref) :defined-on))
                                                                           #'<)))))
          (add-line first t))
        (do ((also-refs (remove-duplicates (sort (mapcar #'(lambda (x)
                                                             (line-page-number x))
                                                         (getf (symbol-plist ref) :referenced-by))
                                                 #'<))))
            ((null also-refs))
          (add-line (with-output-to-string (s)
                      (do ((i 0 (1+ i)))
                          ((or (= i 8)
                               (null (nthcdr i also-refs))))
                        (format s "~8d" (nth i also-refs))))
                    nil)
          (setq also-refs (nthcdr 8 also-refs)))
        (add-line "" nil))))
  even-page))

(defun create-even-pages ()
  (do ((pages (cdr *xpage-list*) (cddr pages)))
      ((null pages))
    (setf (car pages) (list (car (cadr pages))
                            (prepare-even-page (cadr (cadr pages)))
                            (caddr (cadr pages))))))

(defun print-xpage-list (&optional page-filter-function)
  (with-dvi-environment ("angel:/lmi/pace/dvi/dvi2.dvi")
    (load-font 0 "amtt10")
    (load-font 1 "amsltt10")
    (do ((pages *xpage-list* (cdr pages))
         (page-number 1 (1+ page-number)))
        ((null pages))
      (cond ((null (car pages))
             (dvi-bop page-number 0 0 0 0 0 0 0 0 0)
             (dvi-font 0)
             (dvi-down (points->scaled-points (* 4 72.)))
             (dvi-right (points->scaled-points (/ *normal-page-width-in-points* 2)))
             (dvi-set-line "@")
             (dvi-eop))
            ((or (null page-filter-function)
                 (funcall page-filter-function page-number))
             (format t "[~d" page-number)
             (dvi-new-page-with-header-and-line (car pages) page-number)
             (do ((line (cadr (car pages)) (array-leader line 1)))
                 ((null line))
               (dvi-set-line line)
               (when (getf (array-leader line 2) :bold)
                 (dvi-beginning-of-line)
                 (dvi-right (points->scaled-points 1/2))
                 (dvi-set-line line))
               (dvi-next-line))
             (dvi-eop)
             (format t "] "))))))

(defun print-sublist (from to)
  (print-xpage-list #'(lambda (page-number) (and (<= from page-number) (<= page-number to)))))

(defun run-cmd (host command)
  (with-open-stream (s (chaos:open-stream
                         host
                         command
                         :direction :input
                         :ascii-translation t))
    (do ((c (send s :tyi) (send s :tyi)))
        ((null c) t)
      (send standard-output :tyo c))))

(defun spool-it ()
  (let ((temp-file (format nil "/tmp/i~d" (time:get-universal-time))))
    (run-cmd "angel" (format nil "EVAL /usr/local/bin/imagen1 /lmi/pace/dvi/dvi2.dvi > ~a || rm ~:*~a" temp-file))
    (when (probef (fs:parse-pathname temp-file "angel"))
      (run-cmd "angel" (format nil "EVAL /usr/local/bin/ipr ~a ; rm ~:*~a" temp-file)))))

(defun show-xpage ()
  (dolist (page-info *xpage-list*)
    (print-lines (cadr page-info))
    (format t "~&***************************************************************")))

(defun atsign (files)
  (when (find-package "@ Utility")
    (kill-package "@ Utility"))
  (setq @-package (make-package "@ Utility" :use '()))
  (read-file-set files)
  (assign-page-numbers)
  (find-references)
  (find-definitions)
  (create-even-pages)
  (print-xpage-list))

(defconst *orbit-files*
          '("front_end/expanding_vector"
            "front_end/free_stuff"
            "top/sets"
            "top/defs"
            "top/util"
            "top/oprimops"
            "top/new_syntax"
            "top/top"

            "front_end/type"
            "front_end/envs"
            "front_end/nodestuff"
            "front_end/alpha"
            "front_end/declare"
            "front_end/compilators"
            "front_end/node"
            "front_end/assign"
            "front_end/simplify"
            "front_end/simplify_call"
            "front_end/simplify_let"
            "front_end/param"
            "front_end/simplifiers"
            "front_end/simplify_y"
            "front_end/support"
            "front_end/gen_interface"
            "front_end/fixup"
            "front_end/user_error"
            "front_end/module"
            "front_end/analyze"
            "front_end/front"

            "back_end/strategy"
            "back_end/live"
            "back_end/closure"
            "back_end/bookkeep"
            "back_end/generate"
            "back_end/parassign"
            "back_end/reg"

            "back_end/vaxemit"
            "back_end/vaxbookkeep"
            "back_end/vaxgen"
            "back_end/unvaxgen"
            "back_end/vaxlocgen"
            "back_end/vaxarithgen"
            "back_end/vaxrep"
            "primops/vaxconstants"


            "back_end/m68emit"
            "back_end/m68bookkeep"
            "back_end/m68gen"
            "back_end/aem68gen"
            "back_end/m68locgen"
            "back_end/m68arithgen"
            "back_end/m68rep"


            "assembler/as_open"
            "assembler/as_utils"
            "assembler/as"

            "assembler/fg"
            "assembler/ib"
            "assembler/count"
            "assembler/mark"
            "assembler/mini"
            "assembler/bits"
            "assembler/listing"

            "assembler/lap"

            "assembler/as_vax"
            "assembler/vmodes"
            "assembler/vaxis"
            "assembler/vaxi"
            "assembler/vaxam"


            "assembler/as_m68"
            "assembler/m68am"
            "assembler/m68is1"
            "assembler/m68is2"
            ))

(defun orbit-file-to-real-name (file)
  (format nil "angel:/lmi3/pace/t/comp/~a.t" file))

(defun atsign-orbit ()
  (atsign (mapcar #'orbit-file-to-real-name *orbit-files*)))

(defun check-for-files (files)
  (dolist (f files)
    (when (null (probef f))
      (format t "~&~a not found" f))))


(defvar *index-list*)

(defun make-index ()
  (let (defined-symbols output)
    (mapatoms #'(lambda (sym)
                  (when (getf (symbol-plist sym) :defined-on)
                    (push sym defined-symbols)))
              @-package)
    (setq defined-symbols (sort defined-symbols
                                #'(lambda (x y)
                                    (string< (string x) (string y)))))
    (dolist (defined-sym defined-symbols)
      (let ((first-line (format nil "~a:~{~8d~}"
                                defined-sym
                                (remove-duplicates
                                  (sort (mapcar #'(lambda (x)
                                                    (line-page-number x))
                                                (getf (symbol-plist defined-sym) :defined-on))
                                        #'<))))
            rest)
        (do ((also-refs (remove-duplicates
                          (sort
                            (mapcar #'(lambda (x)
                                        (line-page-number x))
                                    (getf (symbol-plist defined-sym) :referenced-by))
                            #'<))))
            ((null also-refs))
          (push (with-output-to-string (s)
                  (do ((i 0 (1+ i)))
                      ((or (= i 8)
                           (null (nthcdr i also-refs))))
                    (format s "~8d" (nth i also-refs))))
                rest)
          (setq also-refs (nthcdr 8 also-refs)))
        (push (cons first-line (reverse rest)) output)))
    (setq *index-list* (reverse output))))


(defun print-index (starting-page-number)
  (let ((page-number starting-page-number)
        (line-number 0)
        (refs *index-list*)
        (lines-per-page (/ *normal-page-height-in-points*
                           *points-per-line*)))
    (labels ((eject ()
               (dvi-eop)
               (dvi-new-page-with-header-and-line "INDEX" page-number)
               (incf page-number)
               (setq line-number 0)))
      (with-dvi-environment ("angel:/lmi/pace/dvi/index.dvi")
        (load-font 0 "amtt10")
        (load-font 1 "amsltt10")
        (dvi-new-page-with-header-and-line "INDEX" page-number)
        (do ()
            ((null refs))
          (when (> (+ line-number (length (car refs))) lines-per-page)
            (eject))
          (dolist (line (car refs))
            (dvi-set-line line)
            (when (not (char= (aref line 0) #\space))
              (dvi-beginning-of-line)
              (dvi-right (points->scaled-points 1/2))
              (dvi-set-line line))
            (dvi-next-line)
            (incf line-number))
          (pop refs))
        (dvi-eop)))))

(defun print-table-of-contents ()
  (with-dvi-environment ("angel:/lmi/pace/dvi/toc.dvi")
    (load-font 0 "amtt10")
    (load-font 1 "amsltt10")
    (dvi-new-page-with-header-and-line "Table of Contents" 1)
    (dvi-font 0)
    (dvi-next-line)
    (do ((last nil)
         (pages *xpage-list* (cdr pages)))
        ((null pages))
      (when (car pages)
        (when (not (eq (car (car pages)) last))
          (let ((line (cadr (car pages))))
            (when (null line)
              (setq line (cadr (cadr pages))))
            (when (null line)
              (ferror nil "can't find page"))
            (dvi-set-line (format nil "~V<~a~;~d~>"
                                  *chars-per-line*
                                  (car (car pages))
                                  (line-page-number line)))
            (setq last (car (car pages))))
          (dvi-next-line))))
    (dvi-eop)))

(defvar *files-to-starts* nil)

(defun find-file-starts ()
  (setq *files-to-starts* nil)
  (do ((last nil)
       (pages (cddr *xpage-list*) (cddr pages)))
      ((null pages))
    (when (not (eq last (car (car pages))))
      (push (cons (car (car pages)) (line-page-number (cadr (car pages))))
            *files-to-starts*)
      (setq last (car (car pages)))))
  (setq *files-to-starts* (reverse *files-to-starts*)))

(defun print-table-of-contents-2 ()
  (labels ((short-file-name (name)
                            (let ((dir (send name :directory)))
                              (when (consp dir)
                                (setq dir (car (last dir))))
                              (send (send name :new-directory (list :relative dir))
                                    :string-for-host))))

    (let ((lines-per-page (/ *normal-page-height-in-points*
                             *points-per-line*))
          )
      (with-dvi-environment ("angel:/lmi/pace/dvi/toc.dvi")
        (load-font 0 "amtt10")
        (load-font 1 "amsltt10")
        (dvi-new-page-with-header-and-line "Table of Contents" 1)
        (dvi-font 0)
        (dvi-next-line)
        (do ((pages1 *files-to-starts* (cdr pages1))
             (pages2 (nthcdr lines-per-page *files-to-starts*) (cdr pages2))
             (count 0 (1+ count)))
            ((= count lines-per-page))
          (cond ((null pages2)
                 (dvi-set-line (format nil "~V<~a~;~d~>"
                                       (- (round (/ *chars-per-line* 2)) 10)
                                       (short-file-name (car (car pages1)))
                                       (cdr (car pages1)))))
                (t
                 (dvi-set-line (format nil "~V<~a~;~d~>        ~V<~a~;~d~>"
                                       (- (round (/ *chars-per-line* 2)) 10)
                                       (short-file-name (car (car pages1)))
                                       (cdr (car pages1))
                                       (round (/ *chars-per-line* 2))
                                       (short-file-name (car (car pages2)))
                                       (cdr (car pages2))))))
          (dvi-next-line))
        (dvi-eop)
        ))))
