;;; -*- Mode: LISP; Syntax: Zetalisp; Package: DVI; Base: 10; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file contains the methods for translation of DVI commands
;; to ImPress format.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The following methods are defined.
;;
;;     :set-char
;;     :set-rule
;;     :put-char
;;     :put-rule
;;     :nop
;;     :bop
;;     :eop
;;     :push  - handled by document :push-stack method
;;     :pop   - handled by document :pop-stack method
;;     :right
;;     :w
;;     :x
;;     :down
;;     :y
;;     :z
;;     :set-fnt
;;     :xxx
;;     :start-output
;;     :end-output
;;     :do-pages
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defflavor imagen-dvi-document
         ((extra-pixel-h-offset 0)
          (extra-pixel-v-offset 0))
         (document)
  :settable-instance-variables)


;;FOR THE IMAGEN PRINTER
(defmethod (imagen-dvi-document :initialize) ()
  (send self :set-printer-name "IMAGEN")
  (send self :set-printer-resolution 300)
  (send self :set-printer-max-fonts 64)
  (send self :set-fontmap (make-array 64 :fill-pointer 0))
  (send self :set-buffer (make-array (* 5 (file-length file-buffer))
                                     :type art-8b
                                     :fill-pointer 0
                                     :area *temp-buffer*))
  (send self :set-fontnums (make-array 64 :fill-pointer 0))
  (maphash #'(lambda (ignore table)
               (fill (fd-char-status table) 0))
           *font-definitions*))


(defmethod (imagen-dvi-document :normal-ending) ()
  ())

(defmethod (imagen-dvi-document :abnormal-ending) ()
  ())

;; ImPress commands used.
(defconstant bgly 199)
(defconstant mplus 131)
(defconstant mminus 132)
(defconstant mmove 133)
(defconstant brule 193)
(defconstant page 213)
(defconstant endpage 219)
(defconstant sp 128)
(defconstant set-sp 210)
(defconstant set-abs-v 137)
(defconstant set-abs-h 135)
(defconstant set-family 207)
(defconstant create-path 230)
(defconstant set-pen 232)
(defconstant draw-path 234)


(defconst set-magnification 236)
(defconst bitmap 235)
(defconst opaque 3)
(defconst eof 255)

(defconst impress-set-push-mask 214)
(defconst impress-push 211)
(defconst impress-pop 212)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (imagen-dvi-document :start-output)()
  (format t "~&   Processing pages.... ")
  )

(defvar *im-debug* nil)

(defmethod (imagen-dvi-document :end-output)()
  ;download needed fonts
  (format t "~& Writing output ... ")
  (with-open-stream (outstream (open-8b-output outfile))
    (setq outfile (send outstream :truename))
    ;for each needed font
    (dotimes (fntnum (fill-pointer fontnums))
      (let* ((key (aref fontmap fntnum))
             (fontdef (send *font-definitions* :get-hash key))
             (fontdims (fd-font-directory fontdef))
             (rasters (fd-raster-info fontdef))
             (char-status (fd-char-status fontdef)))
        ;for each needed char
        (dotimes (charcode chars-per-font)
          (cond ((not (zerop (aref char-status charcode)))
                 (let ((chardims (aref fontdims charcode)))
                   (write-byte outstream bgly)
                   (write-2-bytes
                     outstream (logior (lsh fntnum 7) charcode))
                   (write-2-bytes outstream (char-pxl-width chardims))
                   ;; units to avoid roundoff errors.
                   (write-2-bytes outstream (char-width chardims))
                   (write-2-bytes outstream (char-x-offset chardims))
                   (write-2-bytes outstream (char-height chardims))
                   (write-2-bytes outstream (char-y-offset chardims)))
                 (let ((raster (aref rasters charcode)))
                   (dotimes (i (array-length raster))
                     (write-byte outstream (aref raster i)))))))))

    ;output textual commands from buffer
    (send outstream :string-out buffer)
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  This is the main loop that processes the pages
;;  once the preamble and postamble have been
;;  processed.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod (imagen-dvi-document :do-pages)()
  (move-back file-buffer (1- total-pages))
  (let ((current-page 1)
        (bop-params nil))
    (loop for command = (bget-byte file-buffer)
          do
      (if ( command bop)(bad-dvi "bop command missing"))
      (setq bop-params nil)
      (dotimes (i 11) ; collect  the c0 to c9 params and prev page ptr
        (setq bop-params (cons (bsigned-quad file-buffer) bop-params)))
      (setq bop-params (nreverse bop-params)) ; put them in right order
      (format t "[~D" current-page)
      (cond ((or (not *do-page-p*) (funcall *do-page-p* current-page (car bop-params)))
             (send self :bop bop-params)
             ;;do the page
             (loop for cmnd = (bget-byte file-buffer)
                   do
;       (if ( cmnd pre)(bad-dvi "invalid DVI command between bop and eop"))
                   (cond (*im-debug*
                          (let ((message (get-message-name cmnd))
                                (args (get-parameters cmnd file-buffer)))
                            (format t "~%~A ~S " message args)
                            (cond ((eq message :push-stack)
                                   (format t "~D ~D ~D ~D ~D" h v w x y z))
                                  ((memq message '(:set-char :put-char))
                                   (tyo args)))
                            (send self message args)
                            (cond ((eq message :pop-stack)
                                   (format t "~D ~D ~D ~D ~D" h v w x y z)))))
                         ('else
                          (send self (get-message-name cmnd) (get-parameters cmnd file-buffer))))

                   (if (= cmnd eop)(return (values)))))
            ('else
             (format t " (~D)skipping" (car bop-params))
             (loop for cmnd = (bget-byte file-buffer)
                   do
                   (get-parameters cmnd file-buffer)
                   (if (= cmnd eop) (return (values))))))

      (incf current-page)
      ;; set pointer to the next bop and loop back
      (if ( current-page total-pages)
          (loop for byte = (bget-byte file-buffer)
                until (= byte bop) do
                (cond (( byte nop) ; i.e., font def, skip the params
                       (incf (fill-pointer file-buffer)
                             (+ (1+ (- byte fntdef1)) 12))
                       (let ((bytes (+ (bget-byte file-buffer)
                                       (bget-byte file-buffer))))
                         (incf (fill-pointer file-buffer) bytes))))
                finally (decf (fill-pointer file-buffer)))
          (return (format t "]")))
      (format t "] ")
      )))

(defmacro correct()
  `(let ((diff (- pixel-h (pixel-round h))))
    (cond ((< diff 0)
           (dotimes (i (minus diff))
             (write-buffer buffer mplus)))
          ((> diff 0)
           (dotimes (i diff)
             (write-buffer buffer mminus))))
    (setq pixel-h (pixel-round h))))

(defmethod (imagen-dvi-document :set-char)(charcode)
  ;;params - the char code of the char to be set
;  (if (> char-code chars-per-font)(bad-dvi "Undefined font member"))
  (write-buffer buffer charcode) ;;set the char
  (let* ((fntdef (send *font-definitions* :get-hash current-font))
         (char-dims (aref (fd-font-directory fntdef)charcode)))
    ;;set char status to used.
    (aset 1 (fd-char-status fntdef) charcode)
    ;;advance internal h and correct for cumulative errors.
    (incf h (char-dvi-width char-dims))
    (incf pixel-h (char-pxl-width char-dims))
    (correct)))

(defmethod (imagen-dvi-document :set-rule)(params)
  ;;(car params) - height of rectangle
  ;;(cdr params) - width of rectangle
  (let ((ht (car params))
        (wd (cdr params)))
    (cond ((and (> ht 0)(> wd 0))
           (correct)
           ;;draw the rule on the imagen
           (write-buffer buffer brule)
           (write2-buffer buffer (rule-pixels wd))
           (write2-buffer buffer (rule-pixels ht))
           (write2-buffer buffer (- 1 (pixel-round ht))) ;offset
           ;;advance the internal h
           (incf h wd)
           ;;advance the pen on the imagen
           (write-buffer buffer mmove)
           (write2-buffer buffer (- (rule-pixels h) pixel-h))
           (setq pixel-h (rule-pixels h))))))

(defmethod (imagen-dvi-document :put-char)(charcode)
  ;;params - the char code of the char to be written
;  (if (> charcode chars-per-font)(bad-dvi "Undefined font member"))
  (write-buffer buffer charcode) ;;write the char
  (let* ((fntdef (send *font-definitions* :get-hash current-font))
         (char-dims (aref (fd-font-directory fntdef)charcode)))
    (write-buffer buffer mmove)
    (write2-buffer buffer (minus (char-width char-dims))))
  (aset 1 (fd-char-status (send *font-definitions* :get-hash current-font))
        charcode)
  )

(defmethod (imagen-dvi-document :put-rule )(params)
  ;;(car params) - height of rectangle
  ;;(cdr params) - width of rectangle
  (let ((ht (car params))
        (wd (cdr params)))
    (cond ((and (> ht 0)(> wd 0))
           (correct)
           ;;draw the rule on the imagen
           (write-buffer buffer brule)
           (write2-buffer buffer (rule-pixels wd))
           (write2-buffer buffer (rule-pixels ht))
           (write2-buffer buffer (- 1 (pixel-round ht)))))))

(defvar *extra-pixel-h-offset* nil)
(defvar *extra-pixel-v-offset* nil)

(defun set-picture-page (h-inches v-inches &optional (left-page-offset 0))
  (list (setq *EXTRA-PIXEL-V-OFFSET* (round (* v-inches 300)))
        (setq *EXTRA-PIXEL-H-OFFSET*
              (cond ((zerop left-page-offset)
                     (round (* h-inches 300)))
                    ('else
                     #'(lambda (pnum)
                         (cond ((oddp pnum)
                                (round (* h-inches 300)))
                               ('else
                                (round (* (- h-inches left-page-offset) 300))))))))))


(defmethod (imagen-dvi-document :bop) (list)
  (format t "(~D)" (car list))
  (setq extra-pixel-h-offset (cond ((not *extra-pixel-h-offset*)
                                    0)
                                   ((numberp *extra-pixel-h-offset*)
                                    *extra-pixel-h-offset*)
                                   ('else
                                    (funcall *extra-pixel-h-offset* (car list)))))
  (setq extra-pixel-v-offset (cond ((not *extra-pixel-v-offset*)
                                    0)
                                   ((numberp *extra-pixel-v-offset*)
                                    *extra-pixel-v-offset*)
                                   ('else
                                    (funcall *extra-pixel-v-offset* (car list)))))
  (setq h (dvi-round (+ xpage-offset extra-pixel-h-offset))
        v (dvi-round (+ ypage-offset extra-pixel-v-offset))
        w 0 x 0 y 0 z 0
        pixel-h (+ xpage-offset extra-pixel-h-offset)
        pixel-v (+ ypage-offset extra-pixel-v-offset))
  (send self :empty-stack)
  (setq current-font "Undefined")
  (write-buffer buffer page)
  (write-buffer buffer set-abs-h)
  (write2-buffer buffer pixel-h)
  (write-buffer buffer set-abs-v)
  (write2-buffer buffer pixel-v)
  )

(defmethod (imagen-dvi-document :eop)(ignore)
  ;send the  page for printing
  (write-buffer buffer endpage)
;  (if ( 0 (fill-pointer hstack))
;     (print "Warning - Stack not empty at EOP"))
  )


(defmethod (imagen-dvi-document :after :pop-stack)(ignore)
  (write-buffer buffer set-abs-h)
  (write2-buffer buffer pixel-h)
  (write-buffer buffer set-abs-v)
  (write2-buffer buffer pixel-v))

(defmethod (imagen-dvi-document :right)(delta-h)
  ;params - amount in dvi units to move h in the right direction
  (incf h delta-h)
  (let ((diff (pixel-round h)))
    (write-buffer buffer mmove)
    (write2-buffer buffer (- diff pixel-h))
    (setq pixel-h diff)))

(defmethod (imagen-dvi-document :w)(new-w)
  ;params  - = nil if current value of w is to be used
  ;            = gives new value of w if not nil
  (if (numberp new-w)(setq w new-w))
  (incf h w)
  (let ((diff (pixel-round h)))
    (write-buffer buffer mmove)
    (write2-buffer buffer (- diff pixel-h))
    (setq pixel-h diff)))

(defmethod (imagen-dvi-document :x)(new-x)
  ;params  - = nil if current value of w is to be used
  ;            = gives new value of w if not nil
  ; this makes use of the set space feature of ImPress,
  ; which presumably is faster than explicitly setting
  ; the x position.
  (cond ((numberp new-x)
         (setq x new-x)
         (write-buffer buffer set-sp)
         (write2-buffer buffer (pixel-round x))))
  (write-buffer buffer sp)
  (incf h x)
  (incf pixel-h (pixel-round x))
  (correct)
  )

(defmethod (imagen-dvi-document :down)(delta-v)
  ;params - amount to move pen down in dvi units
  (incf v delta-v)
  (setq pixel-v (pixel-round v))
  (write-buffer buffer set-abs-v)
  (write2-buffer buffer pixel-v))

(defmethod (imagen-dvi-document :y)(new-y)
  (if (numberp new-y)(setq y new-y))
  (incf v y)
  (setq pixel-v (pixel-round v))
  (write-buffer buffer set-abs-v)
  (write2-buffer buffer pixel-v))

(defmethod (imagen-dvi-document :z)(new-z)
  (if (numberp new-z)(setq z new-z))
  (incf v z)
  (setq pixel-v (pixel-round v))
  (write-buffer buffer set-abs-v)
  (write2-buffer buffer pixel-v))

(defmethod (imagen-dvi-document :set-fnt)(texfntnum)
  ;params - number of font to set to (TeX number)
  (let ((fntnum (get-fntnum texfntnum fontnums)))
    (setq current-font (aref fontmap fntnum))
    (if (null current-font)(bad-dvi "Undefined font"))
    (write-buffer buffer set-family)
    (write-buffer buffer fntnum)))

(defvar *ignore-specials* nil)

(defmethod (imagen-dvi-document :xxx) (bytes)
  (let ((string (make-string bytes))
        (offset (fill-pointer file-buffer)))
    (incf (fill-pointer file-buffer) bytes)
    (copy-array-portion file-buffer offset (fill-pointer file-buffer)
                        string 0 bytes)
    (let ((command (parse-xxx-string string)))
      (cond (*ignore-specials*)
            ((and (consp command)
                  (symbolp (car command))
                  (get (intern (string-upcase (car command)) "")
                       'imagen-dvi-xxx))
             (funcall (get (intern (string-upcase (car command)) "")
                       'imagen-dvi-xxx)
                      self
                      command))
            ('else
             (bad-dvi "unknown \special command: ~A" (car command)))))))


(defun (:impress-bitmap imagen-dvi-xxx) (document command)
  (let ((buffer (send document :buffer)))
    (format t "~&Reading impress bitmap from ~S ~{~S ~S~^, ~}~%" (cadr command) (cddr command))
    (cond ((not (probe-file (cadr command)))
           (format t "File does not exist. Continuing...~%"))
          ('else
           (write-buffer buffer impress-set-push-mask)
           (write-buffer buffer 255)
           (write-buffer buffer 255)
           (write-buffer buffer impress-push)
           (with-open-stream (stream (open-8b-input (cadr command)))
             (let ((defaults))
               (when (setq defaults (probe-defaults stream (cadr command)))
                 (format t "~&Getting defaults from ~A~%" defaults)
                 (setq defaults (car (forms-from-file defaults))))
               (let ((hsize (send stream :tyi))
                     (vsize (send stream :tyi)))
                 (read-32-le stream)
                 (read-32-le stream)
                 (write-buffer buffer set-magnification)
                 (write-buffer buffer (get (cdr command) :magnification (getf defaults :magnification 0)))
                 (write-buffer buffer set-abs-h)
                 (write2-buffer buffer (round (evaluate-offset (get (cdr command) :h-offset (getf defaults :h-offset 0))
                                                               document)))
                 (write-buffer buffer set-abs-v)
                 (write2-buffer buffer (round (evaluate-offset (get (cdr command) :v-offset (getf defaults :v-offset 0))
                                                               document)))
                 (write-buffer buffer bitmap)
                 (write-buffer buffer opaque)
                 (write-buffer buffer hsize)
                 (write-buffer buffer vsize)
                 (let ((s-stream))
                   (setq s-stream #'(lambda (op &optional arg1 &rest args)
                                      (si:selectq-with-which-operations op
                                        (:tyo
                                          (write-buffer buffer arg1))
                                        (:string-out
                                          (do ((j (or (car args) 0) (1+ j))
                                               (end (or (cadr args) (length arg1)))
                                               (to buffer))
                                              ((= j end))
                                            (write-buffer to (aref arg1 j))))
                                        (#+(OR LMI SYMBOLICS) t #+TI OTHERWISE
                                          (stream-default-handler s-stream op arg1 args)))))
                   (stream-copy-until-eof stream s-stream))
                 (write-buffer buffer impress-pop))))))))


(defun evaluate-offset (x document)
  ;; return an offset in imagen-sized pixels, i.e. 300 per inch.
  ;; common input is (+ :pixel-v 150), for 1/2 inch under the last text.
  (etypecase x
    (number x)
    (symbol
     (* (quotient 300.0 (send document :printer-resolution))
        (ecase x
          (:pixel-h (send document :pixel-h))
          (:pixel-v (send document :pixel-v))
          (:ypage-offset (send document :ypage-offset))
          (:xpage-offset (send document :xpage-offset))
          (:extra-pixel-h-offset
           (or (send document ':send-if-handles :extra-pixel-h-offset) 0))
          (:extra-pixel-v-offset
           (or (send document ':send-if-handles :extra-pixel-v-offset) 0)))))
    (cons
     (apply (car x) (mapcar #'(lambda (a) (evaluate-offset a document)) (cdr x))))))


;;; the following code is for converting bitmaps as arrays into
;;; impress BITMAP data bytes.

(defvar *imagen-data-bytes* nil)
(defvar *imagen-data-size* nil)
(defvar *bitrev-byte-table* nil)
(defvar *32^32-chunk* nil)
(defvar *128-byte-chunk* nil)

(defun setup-imagen-data-bytes (array x y dx dy)
  (when (null *bitrev-byte-table*)
    (setq *bitrev-byte-table* (make-array 256))
    (dotimes (j 256)
      (setf (aref *bitrev-byte-table* j)
            (do ((value 0 (dpb (ldb (byte 1 k) j) (byte 1 (- 7 k)) value))
                 (k 0 (1+ k)))
                ((= k 8) value)))))
  (when (or (null *32^32-chunk*)
            (null *128-byte-chunk*))
    (setq *32^32-chunk* (make-array '(32 32) :type 'art-1b))
    (setq *128-byte-chunk* (make-array 128 :type 'art-8b
                                       :displaced-to *32^32-chunk*)))
  (multiple-value-bind (easy-y remainder-y)
      (floor dy 32)
    (multiple-value-bind (easy-x remainder-x)
        (floor dx 32)
      (let ((hsize (+ easy-x (if (zerop remainder-x) 0 1)))
            (vsize (+ easy-y (if (zerop remainder-y) 0 1))))
        (setq *imagen-data-size* (list hsize vsize dx dy))
        (do ((ymap 0 (1+ ymap))
             (data-bytes *imagen-data-bytes*)
             (inc 0)
             (bitrev *bitrev-byte-table*)
             (32^32-chunk *32^32-chunk*)
             (128-byte-chunk *128-byte-chunk*))
            ((= ymap vsize))
          (do ((xmap 0 (1+ xmap)))
              ((= xmap hsize))
            (let ((xbase (ash xmap 5))
                  (ybase (ash ymap 5)))
              (let ((x-want (min (- dx xbase) 32))
                    (y-want (min (- dy ybase) 32)))
                (when (or (= x-want remainder-x)
                          (= y-want remainder-y))
                  (fill 128-byte-chunk 0))
                (bitblt tv:alu-seta
                        x-want
                        y-want
                        array
                        (+ xbase x)
                        (+ ybase y)
                        32^32-chunk
                        0
                        0)))
              (copy-array-portion-translated 128-byte-chunk
                                             0
                                             128
                                             data-bytes
                                             inc
                                             (incf inc 128)
                                             bitrev)))))))


(defun copy-array-portion-translated (from-array from-start from-end to-array to-start
                                      to-end translation-table)
  ;; a canditate for microcompilation.
  (copy-array-portion from-array
                      from-start
                      from-end
                      to-array
                      to-start
                      to-end)
  ;; A typical screen array takes 4.2 seconds to process.
  ;; 1.0 seconds with the following code commented out,
  ;; and 0.33 seconds with this entire function a no-op.
  (do ((j to-start (1+ j)))
      ((= j to-end))
    ;; by having only one real array reference here
    ;; we win in the array cache.
    (setf (aref to-array j)
          ;; 3.4 seconds without the %p-contents-offset.
          (%p-contents-offset translation-table (1+ (aref to-array j))))))



;;; the following code is enabled via (setq si:*default-bit-array-pinter* :bitmap)
;;; in the LMI system. It is a way to create the bitmap files to be used
;;; by the \special{impress-bitmap ...} command.


;;; this will save a file with the minimal information
;;; about the bitmap. <HSIZE><VSIZE><WIDTH-32-LE><HEIGHT-32-LE><128*HSIZE*VSIZE bytes of data>
;;;

#+(OR LMI TI)

(progn 'compile

(defvar *bitmap-pathname-defaults* nil)

(add-initialization "bitmap pathname defaults"
                    '(progn (setq *bitmap-pathname-defaults* (fs:make-pathname-defaults))
                            (fs:merge-and-set-pathname-defaults "FOO.IBITS" *bitmap-pathname-defaults*))
                    '(:now warm))


(defun open-printer-bitmap-file (printer)
  (let (pathname)
    (cond ((atom printer)
           (setq pathname (prompt-and-read `(:pathname :defaults ,*bitmap-pathname-defaults*)
                                           "~&Filename for bitmap, default /"~A/"> "
                                           (fs:merge-pathname-defaults "" *bitmap-pathname-defaults*)))
           (format *query-io* "~&Writing to /"~A/"~%" pathname))
          ('else
           (setq pathname (fs:parse-pathname (cadr printer)))))
    (fs:merge-and-set-pathname-defaults pathname *bitmap-pathname-defaults*)
    (cond ((eq :lispm (send pathname :system-type))
           (open pathname :direction :output))
          ('else
           (open pathname :direction :output :raw t)))))


(defun (:impress-bitmap-file si:print-bit-array) (PRINTER ARRAY LEFT TOP RIGHT BOTTOM &rest ignore)
  (when (null *imagen-data-bytes*)
    (setq *imagen-data-bytes* (make-array (// (* 1024 1024) 8)
                                          :type 'art-string)))
  (setup-imagen-data-bytes array left top (- right left) (- bottom top))
  (with-open-stream (stream (open-printer-bitmap-file printer))
    (send stream :tyo (nth 0 *imagen-data-size*))
    (send stream :tyo (nth 1 *imagen-data-size*))
    (write-32-le (nth 2 *imagen-data-size*) stream)
    (write-32-le (nth 3 *imagen-data-size*) stream)
    (send stream :string-out *imagen-data-bytes*
          0 (* 128 (nth 0 *imagen-data-size*) (nth 1 *imagen-data-size*)))
    (setq *imagen-data-size* nil)))

)



(defun (:saved-paint-image imagen-dvi-xxx) (document command)
  (let (FILE DEFAULTS ARRAY WIDTH HEIGHT MAGNIFICATION H-OFFSET V-OFFSET HSIZE VSIZE BUFFER
        array-h-offset array-v-offset array-width array-height)
    (setq buffer (send document :buffer))
    (format t "~&Loading paint file ~S" (cadr command))
    (multiple-value-setq (array file) (load-paint-array (cadr command)))
    (when (setq defaults (probe-file (send file :new-pathname :type "DEFAULTS" :VERSION :NEWEST)))
      (format t "~&Getting defaults from ~A" defaults)
      (SETQ DEFAULTS (CAR (FORMS-FROM-FILE DEFAULTS))))
    (SETQ WIDTH (PIXEL-ARRAY-WIDTH ARRAY))
    (SETQ HEIGHT (PIXEL-ARRAY-HEIGHT ARRAY))
    (setq magnification (get (cdr command) :magnification (getf defaults :magnification 0)))
    (setq h-offset (evaluate-offset (get (cdr command) :h-offset (getf defaults :h-offset 0))
                                    document))
    (setq v-offset (evaluate-offset (get (cdr command) :v-offset (getf defaults :v-offset 0))
                                    document))
    (when (null *imagen-data-bytes*)
      (setq *imagen-data-bytes* (make-array (// (* 1024 1024) 8)
                                            :type 'art-string)))
    (format t " processing ...")
    (setq array-h-offset (eval (getf defaults :array-h-offset 0)))
    (setq array-v-offset (eval (getf defaults :array-v-offset 0)))
    (setq array-width (eval (getf defaults :array-width (- width array-h-offset))))
    (setq array-height (eval (getf defaults :array-height (- height array-v-offset))))
    (setup-imagen-data-bytes array
                             array-h-offset
                             array-v-offset
                             (min array-width (- width array-h-offset))
                             (min array-height (- height array-v-offset)))
    (setq hsize (nth 0 *imagen-data-size*))
    (setq vsize (nth 1 *imagen-data-size*))
    (write-buffer buffer impress-set-push-mask)
    (write-buffer buffer 255)
    (write-buffer buffer 255)
    (write-buffer buffer impress-push)
    (write-buffer buffer set-magnification)
    (write-buffer buffer magnification)
    (write-buffer buffer set-abs-h)
    (write2-buffer buffer (round h-offset))
    (write-buffer buffer set-abs-v)
    (write2-buffer buffer (round v-offset))
    (write-buffer buffer bitmap)
    (write-buffer buffer opaque)
    (write-buffer buffer hsize)
    (write-buffer buffer vsize)
    (do ((data *imagen-data-bytes*)
         (j 0 (1+ j))
         (n (* 128 hsize vsize)))
        ((= j n))
      (write-buffer buffer (aref data j)))
    (write-buffer buffer impress-pop)))
