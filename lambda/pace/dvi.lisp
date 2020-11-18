;;; -*- Mode:LISP; Package:USER; Base:10; Readtable:CL -*-

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

(defvar *dvi-pos-stack*)

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

(defmacro with-dvi-environment ((output-file-name) &body body)
  `(let ((*last-bop* -1)
         (*number-of-pages* 0)
         (*loaded-fonts* nil)
         (*f* nil)
         (*f-tfm* nil)
         (*dvi-pos-stack* nil)
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

(defconst *page-width* (points->scaled-points *page-width-in-points*))
(defconst *page-height* (points->scaled-points *page-height-in-points*))

(defflavor dvi-output-stream
         ((*dvi-stream* nil)
          (output-buffer (make-array 1024.))
          (*last-bop* -1)
          (*number-of-pages* 0)
          (*f* nil)
          (*f-tfm* nil)
          (*dvi-pos-stack* nil)
          (*h* nil)
          (*v* nil)
          (*w* nil)
          (*x* nil)
          (*y* nil)
          (*z* nil)
          (*loaded-fonts* nil))
         (si:buffered-output-stream)
  :settable-instance-variables
  (:special-instance-variables
    *last-bop* *number-of-pages* *f* *f-tfm* *dvi-pos-stack*
    *h* *v* *w* *x* *y* *z*
    *dvi-stream* *loaded-fonts*
    )
  )

(defmethod (dvi-output-stream :after :init) (ignore)
  (when (null *dvi-stream*)
    (ferror nil "must specify a *dvi-stream*"))
  (dvi-pre "LISPM DVI")
  (load-font 0 "amtt10")
  )

(defmethod (dvi-output-stream :after :close) (&optional abort-p)
  (when (null abort-p)
    (when *dvi-pos-stack*
      (dvi-eop))
    (dvi-post)
    (output-fonts-in-decending-order)
    (dvi-post-post))
  (send *dvi-stream* :close abort-p)
  )

(defmethod (dvi-output-stream :new-output-buffer) ()
  (values output-buffer 0 (array-length output-buffer)))

(defmethod (dvi-output-stream :send-output-buffer) (ignore end)
  (dotimes (i end)
    (let ((char (aref output-buffer i)))
      (when (null *dvi-pos-stack*)
        (dvi-bop *number-of-pages* 0 0 0 0 0 0 0 0 0)
        )
      (when (null *f*)
        (dvi-font 0))
      (when (or (> *h* *page-width*)
                (char-equal char #\return))
        (dvi-set-rule (points->scaled-points 10)
                      (points->scaled-points 5))
        (dvi-next-line))
      (when (> *v* *page-height*)
        (dvi-eop)
        (dvi-bop *number-of-pages* 0 0 0 0 0 0 0 0 0))

      (cond ((char-equal char #\return))
            ((< char 128.)
             (dvi-set-char char))
            (t
             (ferror nil "Unknown character ~@c" char))
            ))))

(defmethod (dvi-output-stream :discard-output-buffer) (ignore)
  )

(defun open-dvi-file ()
  (make-instance 'dvi-output-stream
                 :*dvi-stream* (open "angel:/lmi/pace/mf/test.dvi"
                                      :direction :output
                                      :byte-size 8
                                      :raw t)))

(defun test ()
  (with-open-stream (s (open-dvi-file))
    (send s :string-out "Hello")))

(defun test1 ()
  (with-open-stream (s (open-dvi-file))
    (with-open-file (i "angel:/etc/passwd")
      (stream-copy-until-eof i s))))


;;;;;



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



(defvar *chars-per-line*)





(defvar *font-for-code* "amtt10")

(defun compute-chars-per-line ()
  (with-dvi-environment (nil)
    (load-font 0 *font-for-code*)
    (dvi-bop 1 0 0 0 0 0 0 0 0 0)
    (dvi-font 0)
    (return-from compute-chars-per-line
      (values (round (/ *normal-page-width-in-points*
                        (scaled-points->points (dvi-char-width #\0))))))))






(defun dvi-test-3 ()
  (with-dvi-environment ("angel:/lmi/pace/dvi/dvi3.dvi")
    (load-font 0 "amtt10")
    (load-font 1 "amsltt10")
    (dvi-bop 1 0 0 0 0 0 0 0 0 0)
    (dvi-font 0)
    (dvi-set-line "foobar")
    (dvi-eop)
    ))





















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
