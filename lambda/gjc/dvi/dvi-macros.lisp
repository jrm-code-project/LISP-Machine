;;; -*- Mode: LISP; Syntax: Zetalisp; Package: DVI; Base: 10 -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer I/O routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-when (eval compile load)

(defmacro bpeek-byte (buff)
  `(aref ,buff (fill-pointer ,buff)))

(defmacro bget-byte (buff) ;; return the next 8 bits from the input
  ;; stream
  `(progn (incf (fill-pointer ,buff))
          (aref ,buff (1- (fill-pointer ,buff)))))

(defmacro bget-signed-byte (buff)
  `(let ((b (bget-byte ,buff)))
     (if (< b 128) b (- b 256))))

(defmacro bget-2-bytes (buff)
  `(let* ((a (aref ,buff (fill-pointer ,buff)))
          (b (aref ,buff (1+ (fill-pointer ,buff)))))
     (incf (fill-pointer ,buff) 2)
     (logior (lsh a 8) b))) ;; a * 256 + b

(defmacro bsigned-pair (buff)
  `(let* ((a (aref ,buff (fill-pointer ,buff)))
          (b (aref ,buff (1+ (fill-pointer ,buff)))))
     (incf (fill-pointer ,buff) 2)
    (if (< a 128)
        (logior (lsh a 8) b)
        (logior (lsh (- a 256) 8) b))))

(defmacro bget-3-bytes (buff)
  `(let* ((a (aref ,buff (fill-pointer ,buff)))
          (b (aref ,buff (1+ (fill-pointer ,buff))))
          (c (aref ,buff (+ 2 (fill-pointer ,buff)))))
     (incf (fill-pointer ,buff) 3)
     (logior (lsh a 16) (lsh b 8) c)))

(defmacro bsigned-trio (buff)
  `(let* ((a (aref ,buff (fill-pointer ,buff)))
          (b (aref ,buff (1+ (fill-pointer ,buff))))
          (c (aref ,buff (+ 2 (fill-pointer ,buff)))))
     (incf (fill-pointer ,buff) 3)
     (if (< a 128)
         (logior (lsh a 16) (lsh b 8) c)
         (logior (lsh (- a 256) 16)(lsh b 8) c))))

(defmacro bsigned-quad (buff)
  `(let* ((a (aref ,buff (fill-pointer ,buff)))
          (b (aref ,buff (1+ (fill-pointer ,buff))))
          (c (aref ,buff (+ 2 (fill-pointer ,buff))))
          (d (aref ,buff (+ 3 (fill-pointer ,buff)))))
     (incf (fill-pointer ,buff) 4)
     (if (< a 128)
         (logior (ash a 24)(lsh b 16)(lsh c 8) d)
         (logior (ash (- a 256) 24)(lsh b 16)(lsh c 8) d))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Binary file I/O routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro peek-byte (istr)
  `(send ,istr :tyipeek))

(defmacro get-byte (istr) ;; return the next 8 bits from the input
  ;; stream
  `(send ,istr :tyi))  ;; handler for eof?

(defmacro get-signed-byte (istr)
  `(let ((b (send ,istr :tyi)))
    (if (< b 128) b (- b 256))))

(defmacro get-2-bytes (istr)
  `(let* ((a (send ,istr :tyi))
         (b (send ,istr :tyi)))
    (logior (lsh a 8) b))) ;; a * 256 + b

(defmacro signed-pair (istr)
  `(let* ((a (send ,istr :tyi))
         (b (send ,istr :tyi)))
    (if (< a 128)
        (logior (lsh a 8) b)
        (logior (lsh (- a 256) 8) b))))

(defmacro get-3-bytes (istr)
  `(let* ((a (send ,istr :tyi))
         (b (send ,istr :tyi))
         (c (send ,istr :tyi)))
    (logior (lsh a 16) (lsh b 8) c)))

(defmacro signed-trio (istr)
  `(let* ((a (send ,istr :tyi))
         (b (send ,istr :tyi))
         (c (send ,istr :tyi)))
    (if (< a 128)
        (logior (lsh a 16) (lsh b 8) c)
        (logior (lsh (- a 256) 16)(lsh b 8) c))))
;;i.e.((a-256) * 256 + b) * 256 + c

(defmacro signed-quad (istr)
  `(let* ((a (send ,istr :tyi))
         (b (send ,istr :tyi))
         (c (send ,istr :tyi))
         (d (send ,istr :tyi)))
     (if (< a 128)
         (logior (ash a 24)(lsh b 16)(lsh c 8) d)
         (logior (ash (- a 256) 24)(lsh b 16)(lsh c 8) d))))
)

(defmacro write-byte (ostr byte)
  `(send ,ostr :tyo ,byte))

(defmacro write-2-bytes (ostr bytes)
  `(progn
    (send ,ostr :tyo (ldb (byte 8 8) ,bytes))
    (send ,ostr :tyo (ldb (byte 8 0) ,bytes))))

(defmacro write-buffer (buff byte)
  `(array-push-extend ,buff ,byte))

(defmacro write2-buffer (buff bytes)
  `(progn
     (array-push-extend ,buff (ldb (byte 8 8) ,bytes))
     (array-push-extend ,buff (ldb (byte 8 0) ,bytes))))

;; go to the end of the file.
(defmacro go-eof (istr)
  `(send ,istr :set-pointer (1- (file-stream-length ,istr))))

(defmacro skip-bytes (istr n)
  ;;go forward n bytes, n can be negative
  `(send ,istr :set-pointer (+ (send ,istr :read-pointer) ,n)))

(defsubst move-back (file-buffer n)
  ;;move back the dvi file by n pages
  (let (prev-page-ptr)
    (dotimes (i n)
      (if ( (bget-byte file-buffer) bop)(bad-dvi "Missing bop"))
      (incf (fill-pointer file-buffer) 40) ;get rid of c0 to c9 params
      (setq prev-page-ptr (bsigned-quad file-buffer))
      (if (> prev-page-ptr 0)
          (setf (fill-pointer file-buffer) prev-page-ptr)
          (decf (fill-pointer file-buffer) 44)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; subroutines for unit conversions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar conv)
(defvar dvis-per-fix)
(defvar dvi2mica)
;;
;; compute the number of pixels in the height or width of a rule.
;;
(defsubst rule-pixels (x)
  (ceiling (* conv x)))

;;convert from dvi units to pixels
(defsubst pixel-round (x)
  (round (* conv x)))

(defsubst dvi-round (x)
  (// x (float conv)))
;;
(defsubst fix2dvi (x)
  (* dvis-per-fix x))

;;

(defsignal dvi-error error ())

(defun bad-dvi (reason &rest args)
  (if args
      (lexpr-funcall 'ferror 'dvi-error reason args)
    (ferror 'dvi-error "Bad dvi: ~S" reason)))

(deff bad-pxl 'bad-dvi)

(defmacro get-fntnum (texfntnum array)
  `(loop for i from 0 below (fill-pointer ,array)
         do
     (if (= (aref ,array i) ,texfntnum)
         (return i))))

(defmacro file-length (fbuffer)
  `(array-leader ,fbuffer 1))

(defmacro store-file-length (fbuffer length)
  `(store-array-leader ,length ,fbuffer 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;The following macros are for handling PRESS files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Puts out command to show characters on the DL.
;;; Uses Show-characters-short if possible.
(defmacro put-pending-characters ()
  `(when (plusp pending-characters)
     (loop until (< pending-characters 256.) do
           (el-byte <Show-Characters>)
           (el-byte 255.)
           (decf pending-characters 255.)
           finally (cond ((> pending-characters 32.)
                          (el-byte <Show-Characters>)
                          (el-byte pending-characters))
                         ((plusp pending-characters)
                          (el-byte (1- (+ <Show-Characters-Short>
                                          pending-characters))))))
     (setq pending-characters 0)))

;;; Insert a byte into the EL.
(defmacro el-byte (byte)
  `(progn (array-push-extend page-entity-buffer ,byte entity-buffer-extension-size)
          (incf entity-list-length)))

;;; Insert a word into the EL.
(defmacro el-word (word)
  (once-only (word)
    `(progn (el-byte (ldb #o1010 ,word))
            (el-byte (ldb #o0010 ,word)))))

;;; Insert a 32-bit word into the EL.
(defmacro el-32word (word)
  (once-only (word)
    `(progn (el-byte (ldb #o3010 ,word))
            (el-byte (ldb #o2010 ,word))
            (el-byte (ldb #o1010 ,word))
            (el-byte (ldb #o0010 ,word)))))

;;; Insert a byte into the DL.
(defmacro dl-byte (byte)
  `(progn (send output-stream :tyo ,byte)
          (incf data-list-length)))

;;; Insert a word into the DL.
(defmacro dl-word (word)
  (once-only (word)
    `(progn (dl-byte (ldb #o1010 ,word))
            (dl-byte (ldb #o0010 ,word)))))

;;; Insert a 32-bit word into the DL.
(defmacro dl-32word (word)
  (once-only (word)
    `(progn (dl-byte (ldb #o3010 ,word))
            (dl-byte (ldb #o2010 ,word))
            (dl-byte (ldb #o1010 ,word))
            (dl-byte (ldb #o0010 ,word)))))

;;; File I/O.

(defmacro byte-out (byte)
  `(send output-stream :tyo ,byte))

(defmacro word-out (word)
  (once-only (word)
    `(progn (byte-out (ldb #o1010 ,word))
            (byte-out (ldb #o0010 ,word)))))

;;; Output a BCPL string.
(defmacro bcpl-out (string max-length)
  `(let ((string-end (min (1+ (string-length ,string)) ,max-length)))
     (byte-out (1- string-end))
     (send output-stream :string-out ,string 0 (1- string-end))
     (pad-bytes-out (- ,max-length string-end))))

(defmacro pad-bytes-out (number)
  `(loop repeat ,number do (byte-out 0)))


(defun open-8b-input (filename)
  ;; using :characters NIL can confuse some servers into 16 bit mode.
  ;; (open filename :direction :input :characters nil :byte-size 8)
  ;; :raw T is quaint but works.
  (let ((pathname (send (fs:parse-pathname filename) :translated-pathname)))
    (cond ((eq :lispm (send pathname :system-type))
           (open filename :direction :input))
          ('else
           (open filename :direction :input :raw t)))))


(defun open-8b-output (filename)
  (let ((pathname (send (fs:parse-pathname filename) :translated-pathname)))
    (cond ((eq :lispm (send pathname :system-type))
           (open filename :direction :output))
          ('else
           (open filename :direction :output :raw t)))))

(defun file-stream-length (file-stream)
  ;; if we used :characters NIL and :byte-size 8
  ;; then some unix servers were give the wrong length.
  ;; but since we dont, we win.
  (send file-stream :length))


(defvar *pxl-filename-prepend* "tex: TeXfonts;")
(defvar *tfm-filename-prepend* "tex: TeXfonts;")
