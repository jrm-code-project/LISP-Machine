;;; -*- Mode: LISP; Syntax: Zetalisp; Package: DVI; Base: 10; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file contains the methods for translation of DVI commands
;; to display on the screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The following methods are defined.
;;
;;     :set-char
;;     :set-rule
;;     :put-char
;;     :put-rule
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

(defflavor screen-dvi-document
         (image-width
          image-height
          (origin-x 5)
          (origin-y 5)
          previous-page-ptr)
         (document))

(defvar *screen-resolution* 160)

(defresource sheet-bit-array (w h)
  :constructor (tv:make-sheet-bit-array tv:main-screen w h))

(defmethod (screen-dvi-document :initialize) ()
  (setq image-width (fix (* *screen-resolution* 8.5)))
  (setq image-height (* *screen-resolution* 11))
  (send self :set-printer-name "SCREEN")
  (send self :set-printer-resolution *screen-resolution*)
  (send self :set-printer-max-fonts 64)
  (send self :set-fontmap (make-array 64 :fill-pointer 0))
  (setq buffer (allocate-resource 'sheet-bit-array image-width image-height))
  ;clear the sheet
  (tv:%draw-rectangle image-width image-height 0 0 tv:alu-setz buffer)
  ;draw the border of the page
  (let ((ux 0) (uy 0)
        (lx (1- image-width)) (ly (1- image-height)))
    (tv:%draw-line ux uy lx uy tv:alu-ior t buffer)
    (tv:%draw-line lx uy lx ly tv:alu-ior t buffer)
    (tv:%draw-line lx ly ux ly tv:alu-ior t buffer)
    (tv:%draw-line ux ly ux uy tv:alu-ior t buffer))
  (send self :set-buffer buffer)
  (send self :set-fontnums (make-array 64 :fill-pointer 0)))


(defmethod (screen-dvi-document :start-output)()
  (format t "~&   Computing page 1 ... ")
  )

(defmethod (screen-dvi-document :end-output)()
  nil
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  This is the main loop that processes the pages
;;  once the preamble and postamble have been
;;  processed. File pointer points to bop of first-page
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (screen-dvi-document :do-pages) ()
  (move-back file-buffer (1- total-pages));move to first page
  (let ((current-page 1)
        (desired-page 1)
        (numeric-arg 0))
    (setq previous-page-ptr -1)
    (send terminal-io :clear-window)
    (send self :do-screen-page)
    (show-image self)
    (loop for input = (send terminal-io :any-tyi)
          when (nlistp input)
            do
        (selectq (char-upcase input)
          ((#/0 #/1 #/2 #/3 #/4 #/5 #/6 #/7 #/8 #/9)
           (setq numeric-arg (+ (* numeric-arg 10.) (- input #/0))))
          (#/G
           (cond ((not (zerop numeric-arg))
                  (setq desired-page (min total-pages (max 0 numeric-arg)))
                  (view-page self current-page desired-page)
                  (setq current-page desired-page))
                 (:else (beep)))
           (setq numeric-arg 0))
          ((#/control-v #/N)
           (incf desired-page (if (zerop numeric-arg) 1 numeric-arg))
           (setq desired-page (min total-pages (max 1 desired-page)))
           (view-page self current-page desired-page)
           (setq current-page desired-page)
           (setq numeric-arg 0))
          ((#/meta-v #/P)
           (decf desired-page (if (zerop numeric-arg) 1 numeric-arg))
           (setq desired-page (min total-pages (max 1 desired-page)))
           (view-page self current-page desired-page)
           (setq current-page desired-page)
           (setq numeric-arg 0))
          (#/H
           (setq origin-x 5
                 origin-y 5)
           (show-image self))
          (#/return
           (setq origin-x 5)
           (show-image self))
          ((#\/ #/control-n)
           (decf origin-y 80.)
           (show-image self))
          ((#/control-p #\\)
           (incf origin-y 80.)
           (show-image self))
          ((#\? #\Help)
           (give-help)
           (show-image self))
          (#\Q
           (send terminal-io :clear-window)
           (describe-settings self)
           (return (values)))
          (#\S
           (describe-settings self))
          (otherwise
           (beep)
           (show-image self))))))

;;Functions that should be added.
;;- show page number
;;- set page number
;;- set magnification
;;- center page

(defvar intro
"                        This is the DVI File display program
                    with user interface stolen from the DPRESS program
")

(defun give-help ()
  (send terminal-io :clear-window)
  (format t "~A

               nG       GOTO page n
               nN       NEXT page(s)
               nP       PREVIOUS page(s)
               //       Move page down
               \        Move page up
               H        Home window
               Return   Goto left end of paper
               Q        QUIT
               S        Describe settings
               ?        Print this wonderfully useful information

               You can also use the mouse to move the page around.  Just hold down any
               mouse button and move.  The page will follow until you release the button.
               Bugs to bhlim@vx.

Press space to refresh: " intro)
  (loop for input = (send terminal-io :any-tyi) do
        (if (listp input) (beep)
            (if (= input #\space) (return (values))
                (return (send terminal-io :untyi input))))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The document drawing subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (screen-dvi-document :set-char)(charcode)
  ;;params - the char code of the char to be set
;  (if (> charcode chars-per-font)(bad-dvi "Undefined font member"))

  (let* ((fntdef (send *font-definitions* :get-hash current-font))
         (char-dims (aref (fd-font-directory fntdef) charcode))
         (char-height (char-height char-dims))
         (char-width (char-width char-dims))
         (char-x-offset (char-x-offset char-dims))
         (char-y-offset (char-y-offset char-dims))
         (bitmap (aref (fd-bitmaps fntdef) charcode)))
    (cond ((null bitmap)
           (setq bitmap (glyph2bitmap (aref (fd-raster-info fntdef)charcode)
                                      char-width char-height))
           (aset bitmap (fd-bitmaps fntdef) charcode)))
    ;;draw the char on the buffer
    (if #+SYMBOLICS   (and (plusp pixel-h)(< pixel-h (- (array-dimension-n 1 buffer) 32))
                           (plusp pixel-v)(< pixel-v (- (array-dimension-n 2 buffer) 32)))
        #+(OR LMI TI) (and (plusp pixel-h)(< pixel-h (- (array-dimension buffer 1) 32))
                           (plusp pixel-v)(< pixel-v (- (array-dimension buffer 0) 32)))
        (bitblt tv:alu-ior
                (min char-width (- #+SYMBOLICS   (array-dimension-n 1 buffer)
                                   #+(OR LMI TI) (array-dimension buffer 1)
                                   pixel-h))
                char-height
                bitmap 0 0 buffer
                (- pixel-h char-x-offset)
                (- pixel-v char-y-offset)))
    ;;draw the char on the screen.
    #+SYMBOLICS
    (send terminal-io :bitblt tv:alu-ior
          char-width
          char-height
          bitmap 0 0
          (+ origin-x (- pixel-h char-x-offset))
          (+ origin-y (- pixel-v char-y-offset)))

    ;;advance internal h and correct for cumulative errors.
    (incf h (char-dvi-width char-dims))
    (setq pixel-h (pixel-round h))))

(defmethod (screen-dvi-document :set-rule)(params)
  ;;(car params) - height of rectangle
  ;;(cdr params) - width of rectangle
  (let* ((ht (car params))
         (wd (cdr params))
         (pixel-ht (rule-pixels ht))
         (pixel-wd (rule-pixels wd)))
  (cond ((and (> ht 0)(> wd 0))
          (setq pixel-h (pixel-round h))
          ;;draw the rule on the buffer
          (sys:%draw-rectangle
            (min (- #+SYMBOLICS   (array-dimension-n 1 buffer)
                    #+(OR LMI TI) (array-dimension buffer 1)
                    pixel-h
                    1)
                 pixel-wd)
            pixel-ht
            pixel-h
            (- pixel-v (pixel-round ht))
            tv:alu-ior
            buffer)
          (send terminal-io :draw-rectangle
                pixel-wd
                pixel-ht
                (+ origin-x pixel-h)
                (+ origin-y (- pixel-v (pixel-round ht)))
                tv:alu-ior)
          ;;advance the internal h
          (incf h wd)
          (setq pixel-h (rule-pixels h))))))

(defmethod (screen-dvi-document :put-char)(charcode)
  ;;params - the char code of the char to be written
;  (if (> charcode chars-per-font)(bad-dvi "Undefined font member"))
  (let* ((fntdef (send *font-definitions* :get-hash current-font))
         (char-dims (aref (fd-font-directory fntdef) charcode))
         (char-height (char-height char-dims))
         (char-width (char-width char-dims))
         (char-x-offset (char-x-offset char-dims))
         (char-y-offset (char-y-offset char-dims))
         (bitmap (aref (fd-bitmaps fntdef)charcode)))
    (cond ((null bitmap)
           (setq bitmap (glyph2bitmap (aref (fd-raster-info fntdef) charcode)
                                      char-width char-height))
           (aset bitmap (fd-bitmaps fntdef) charcode)))
    ;;draw the char on the buffer
    (if (and (plusp pixel-h)(< pixel-h (- (array-dimension buffer 0) 32))
             (plusp pixel-v)(< pixel-v (- (array-dimension buffer 1) 32)))
        (bitblt tv:alu-ior
                (min char-width (- (array-dimension buffer 0) pixel-h))
                char-height bitmap 0 0 buffer
                (- pixel-h char-x-offset)
                (- pixel-v char-y-offset)))
    ;;draw the char on the screen
    (send terminal-io :bitblt tv:alu-ior
          char-width
          char-height
          bitmap 0 0
          (+ origin-x (- pixel-h char-x-offset))
          (+ origin-y (- pixel-v char-y-offset)))
    ))

(defmethod (screen-dvi-document :put-rule)(params)
  ;;(car params) - height of rectangle
  ;;(cdr params) - width of rectangle
  (let ((ht (car params))
        (wd (cdr params)))
    (cond ((and (> ht 0)(> wd 0))
           (setq pixel-h (pixel-round h))
           (sys:%draw-rectangle
             (min (- (array-dimension buffer 0) pixel-h 1)
                  (rule-pixels wd))
             (rule-pixels ht)
             pixel-h
             (- pixel-v (pixel-round ht))
             tv:alu-ior
             buffer)
          (send terminal-io :draw-rectangle
                (rule-pixels wd)
                (rule-pixels ht)
                (+ origin-x pixel-h)
                (+ origin-y (- pixel-v (pixel-round ht)))
                tv:alu-ior)))))



(defmethod (screen-dvi-document :bop)(ignore)
  ;params - nil
  (setq h (dvi-round (+ (// *screen-resolution* 16) xpage-offset))
        v (dvi-round ypage-offset)
        w 0 x 0 y 0 z 0
        pixel-h (+ (// *screen-resolution* 16) xpage-offset)
        pixel-v ypage-offset)
  (send self :empty-stack)
  (setq current-font "Undefined")
  )

(defmethod (screen-dvi-document :eop)(ignore)
  nil
  ;do nothing
  )


(defmethod (screen-dvi-document :right)(delta-h)
  ;params - amount in dvi units to move h in the right direction
  (incf h delta-h)
  (setq pixel-h (pixel-round h)))

(defmethod (screen-dvi-document :w)(new-w)
  ;params  - = nil if current value of w is to be used
  ;            = gives new value of w if not nil
  (if (numberp new-w)(setq w new-w))
  (incf h w)
  (setq pixel-h (pixel-round h)))

(defmethod (screen-dvi-document :x)(new-x)
  ;params  - = nil if current value of w is to be used
  ;            = gives new value of w if not nil
  (if (numberp new-x)(setq x new-x))
  (incf h x)
  (setq pixel-h (pixel-round h))
  )

(defmethod (screen-dvi-document :down)(delta-v)
  ;params - amount to move pen down in dvi units
  (incf v delta-v)
  (setq pixel-v (pixel-round v))
  )

(defmethod (screen-dvi-document :y)(new-y)
  (if (numberp new-y)(setq y new-y))
  (incf v y)
  (setq pixel-v (pixel-round v))
  )

(defmethod (screen-dvi-document :z)(new-z)
  (if (numberp new-z)(setq z new-z))
  (incf v z)
  (setq pixel-v (pixel-round v))
  )

(defmethod (screen-dvi-document :set-fnt)(font-number)
  ;params - number of font to set to (TeX number)
  (setq current-font (aref fontmap (get-fntnum font-number fontnums)))
  (if (null current-font)(bad-dvi "Undefined font"))
  (if (null (fd-bitmaps (send *font-definitions* :get-hash current-font)))
      (alter-fontdef (send *font-definitions* :get-hash current-font)
                     bitmaps
                     (make-array chars-per-font)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun glyph2bitmap (glyph char-width char-height)
  (let* ((width (* (1+ (// char-width 32)) 32))
         (height (* (1+ (// char-height 32)) 32))
         (bitmap (make-array #+SYMBOLICS   `(,width ,height)
                             #+(OR LMI TI) `(,height ,width)
                             :type 'art-1b))
         (bytes-per-row (ceiling (// (float char-width) 8))))
    (dotimes (row char-height)
      (let ((.row*bytes-per-row. (* row bytes-per-row)))

        (dotimes (byte bytes-per-row)
          (let ((bits (aref glyph (+ .row*bytes-per-row. byte)))
                (.8*byte. (* 8 byte)))

            (dotimes (i 8)
              (if ( (+ .8*byte. i) char-width)
                  #+SYMBOLICS
                  (aset (load-byte bits (- 7 i) 1)
                        bitmap
                        (+ .8*byte. i) row)
                  #+(OR LMI TI)
                  (aset (load-byte bits (- 7 i) 1)
                        bitmap
                        row (+ .8*byte. i))))))))
    bitmap))

(defun refresh-image (doc)
  (send doc :refresh-image))

(defmethod (screen-dvi-document :refresh-image) ()
  (tv:prepare-sheet (terminal-io)
    (let ((alu tv:alu-seta)
          (width image-width)
          (height image-height)
          (from-array buffer)
          (from-x 0)
          (from-y 0)
          (to-array (tv:sheet-screen-array terminal-io))
          (to-x origin-x)
          (to-y origin-y))
      (let ((sxmin (tv:sheet-inside-left terminal-io))
            (symin (tv:sheet-inside-top terminal-io))
            (sxmax (tv:sheet-inside-right terminal-io))
            (symax (tv:sheet-inside-bottom terminal-io)))
        (let (left top
              (right+1 (min sxmax (+ to-x (abs width))))
              (bottom+1 (min symax (+ to-y (abs height)))))
          (if (< to-x sxmin)
              (setq left sxmin
                    from-x (+ from-x (- sxmin to-x)))
              (setq left to-x))
          (if (< to-y symin)
              (setq top symin
                    from-y (+ from-y (- symin to-y)))
              (setq top to-y))
          (let ((new-width (- right+1 left))
                (new-height (- bottom+1 top)))
            (when (and (> new-width 0) (> new-height 0))
              (when (< width 0) (setq new-width (- new-width)))
              (when (< height 0) (setq new-height (- new-height)))
              (destructuring-bind (a-width a-height) (array-dimensions from-array)
                (cond (( from-x a-width) (setq from-x (\ from-x a-width)))
                      ((< from-x 0)
                       (when (minusp (setq from-x (\ from-x a-width)))
                         (incf from-x a-width))))
                (cond (( from-y a-height) (setq from-y (\ from-y a-height)))
                      ((< from-y 0)
                       (when (minusp (setq from-y (\ from-y a-height)))
                         (incf from-y a-height)))))
              (bitblt alu new-width new-height from-array
                      from-x from-y to-array left top))))))))

(defun handle-mouse-input (document)
  (unwind-protect
    (progn
      (tv:mouse-set-blinker-definition :character 0 0 :on
                                       :set-character 12. 'fonts:mouse)
      (loop for blip = (send terminal-io :any-tyi) do
            (when (nlistp blip) (return (send terminal-io :untyi blip)))
            (when (eq (car blip) :mouse-button)
              (destructuring-bind (encoded-click window x y) (cdr blip)
                encoded-click
                (when (eq window terminal-io)
                  (loop until (zerop #+SYMBOLICS tv:mouse-buttons #+(OR LMI TI) tv:mouse-last-buttons) do
                        (tv:mouse-set-blinker-definition :character 0 0 :on
                                                         :set-character 12. 'fonts:mouse)
                        (process-wait "Move Mouse" #'(lambda (old-x old-y)
                                                       (or (zerop #+SYMBOLICS   tv:mouse-buttons
                                                                  #+(OR LMI TI) tv:mouse-last-buttons)
                                                           (not (and (= old-x tv:mouse-x)
                                                                     (= old-y tv:mouse-y)))))
                                      x y)
                        (let ((dx (- x (setq x tv:mouse-x)))
                              (dy (- y (setq y tv:mouse-y))))
                          (move-image document dx dy)
                          (refresh-image document))))))))
    (tv:mouse-standard-blinker)))


(defun move-image (document dx dy)
  (send document :move-image dx dy))


(defmethod (screen-dvi-document :move-image) (dx dy)
  (decf origin-x dx)
  (decf origin-y dy)
  (let ((inside-left (tv:sheet-inside-left terminal-io))
        (inside-top (tv:sheet-inside-top terminal-io)))
    (tv:prepare-sheet (terminal-io)
      (setq dx (- dx)
            dy (- dy))
      (if (minusp dx)
          (tv:draw-rectangle-inside-clipped
            (- dx) (+ image-height (* 2 (abs dy)))
            (- (+ origin-x image-width) inside-left)
            (- (- origin-y (abs dy)) inside-top) tv:alu-setz terminal-io)
          (tv:draw-rectangle-inside-clipped
            dx (+ image-height (* 2 (abs dy)))
            (- (- origin-x dx) inside-left)
            (- (- origin-y (abs dy)) inside-top) tv:alu-setz terminal-io))
      (if (minusp dy)
          (tv:draw-rectangle-inside-clipped
            (+ image-width (* 2 (abs dx))) (- dy)
            (- (- origin-x (abs dx)) inside-left)
            (- (+ origin-y image-height) inside-top) tv:alu-setz
            terminal-io)
          (tv:draw-rectangle-inside-clipped
            (+ image-width (* 2 (abs dx))) dy
            (- (- origin-x (abs dx)) inside-left)
            (- (- origin-y dy) inside-top) tv:alu-setz terminal-io)))))

(defun show-image (doc)
  (send terminal-io :clear-window)
  (send doc :refresh-image)
  (handle-mouse-input doc))

(defun view-page (document current-page desired-page)
  (send document 'view-page current-page desired-page))

(defmethod (screen-dvi-document view-page) (current-page desired-page)
  ;;requires that 0 < desired-page  total pages
  (format t "~:|Computing Page ~D ... " desired-page)
  (cond ((> current-page desired-page)
         (setf (fill-pointer file-buffer) previous-page-ptr)
         (move-back file-buffer (- current-page desired-page 1))
         (do-page self))
        ((< current-page desired-page)
         (cond ((and (= desired-page (1+ current-page))
                     (> (- total-pages desired-page) 2))        ;efficiency hack.
                (loop for byte = (bget-byte file-buffer)
                      until (= byte bop)
                      do
                      (cond (( byte nop)       ;fntdef command skip the params
                             (incf (fill-pointer file-buffer)
                                   (+ (1+ (- byte fntdef1)) 12))
                             (let ((bytes (+ (bget-byte file-buffer)
                                             (bget-byte file-buffer))))
                               (incf (fill-pointer file-buffer) bytes))))
                      finally (decf (fill-pointer file-buffer))))
               (:else
                (find-the-postamble file-buffer)
                (setf (fill-pointer file-buffer)(bsigned-quad file-buffer))
                (move-back file-buffer (- total-pages desired-page))))
         (do-page self)))
  (show-image self))

(defun do-page (document)
  (send document :do-screen-page))

(defmethod (screen-dvi-document :do-screen-page) ()
  ;clear contents of buffer
  (tv:%draw-rectangle image-width image-height 0 0 tv:alu-setz buffer)
  (let ((ux 0) (uy 0)
        (lx (1- image-width)) (ly (1- image-height)))
    (tv:%draw-line ux uy lx uy tv:alu-ior t buffer)
    (tv:%draw-line lx uy lx ly tv:alu-ior t buffer)
    (tv:%draw-line lx ly ux ly tv:alu-ior t buffer)
    (tv:%draw-line ux ly ux uy tv:alu-ior t buffer))

    (if ( (bget-byte file-buffer) bop)(bad-dvi "Missing bop - do page"))
    (incf (fill-pointer file-buffer) 40);skip the c0 to c9 params
    (setq previous-page-ptr (bsigned-quad file-buffer))
    (send self :bop nil)

    ;;do the page
    (loop for cmnd = (bget-byte file-buffer)
          until (= cmnd eop)
          do
;     (if ( cmnd pre)(bad-dvi "invalid DVI command between bop and eop"))
      (send self
            (get-message-name cmnd)
            (get-parameters cmnd file-buffer))))

;      (if (= cmnd eop)(return (values))))


(defmethod (screen-dvi-document :normal-ending) ()
  (deallocate-resource 'sheet-bit-array buffer))

(defmethod (screen-dvi-document :abnormal-ending) ()
  (deallocate-resource 'sheet-bit-array buffer))

(defmethod (screen-dvi-document :xxx) (bytes)
  (let ((string (make-string bytes))
        (offset (fill-pointer file-buffer)))
    (incf (fill-pointer file-buffer) bytes)
    (copy-array-portion file-buffer offset (fill-pointer file-buffer)
                        string 0 bytes)
    (let ((command (parse-xxx-string string)))
      (cond ((and (consp command)
                  (symbolp (car command))
                  (get (intern (string-upcase (car command)) "")
                       'screen-dvi-xxx))
             (funcall (get (intern (string-upcase (car command)) "")
                       'screen-dvi-xxx)
                      self
                      command))
            ('else
             (bad-dvi "unknown \special command: ~A" (car command)))))))

;; a bitmap file is <4-bytes-X-dim><4-bytes-y-dim><...data...>

(defun read-32-le (stream)
  (do ((j 8 (+ 8 j))
       (n (send stream :tyi) (dpb (send stream :tyi) (byte 8 j) n)))
      ((= j 32) n)))

(defun write-32-le (n stream)
  (do ((j 0 (+ 8 j)))
      ((= j 32) n)
    (send stream :tyo (ldb (byte 8 j) n))))

(defun read-bitmap-file (filename)
  (with-open-stream (stream (open-8b-input filename))
    (let* ((dim0 (read-32-le stream))
           (dim1 (read-32-le stream))
           (bitmap (make-array (list dim0 dim1) :type 'art-1b)))
      (send stream :string-in nil (make-array (floor (* dim0 dim1) 8) :displaced-to bitmap :type 'art-string))
      bitmap)))

(defun view-bitmap-file (filename)
  (let ((array (read-bitmap-file filename)))
    (send terminal-io :clear-screen)
    (send terminal-io :bitblt tv:alu-xor
          (array-dimension array 1)
          (array-dimension array 0)
          array 0 0 0 0)
    (send terminal-io :tyi)))

(defun (:bitmap screen-dvi-xxx) (document command)
  ;; Since we are mostly interested in bitmap Placement, and to
  ;; avoid having to do moby conversions on the bitmap, vis a vi screen resolution
  ;; we just draw the BOX that would contain the bits.
  (let (width height)
    (with-open-stream (stream (open-8b-input (cadr command)))
      (setq height (read-32-le stream))
      (setq width  (read-32-le stream)))
    document
    ()))


(defun probe-defaults (stream file &aux (lpath (fs:parse-pathname file)))
  (let ((probe (probe-file (send (send lpath :back-translated-pathname (send stream :truename))
                                 :new-pathname :type "DEFAULTS" :version :newest))))
    (and probe (send lpath :back-translated-pathname probe))))

(defun (:impress-bitmap screen-dvi-xxx) (document command)
  (let (defaults hsize vsize magnification h-offset v-offset width height)
    (when (probe-file (cadr command))
      (with-open-stream (stream (open-8b-input (cadr command)))
        (WHEN (setq defaults (probe-defaults stream (cadr command)))
          (SETQ DEFAULTS (CAR (FORMS-FROM-FILE DEFAULTS))))
        (setq hsize (send stream :tyi))
        (setq vsize (send stream :tyi))
        (setq width (read-32-le stream))
        (setq height (read-32-le stream)))
      (setq magnification (get (cdr command) :magnification (getf defaults :magnification 0)))
      (setq h-offset (evaluate-offset (get (cdr command) :h-offset (getf defaults :h-offset 0))
                                      document))
      (setq v-offset (evaluate-offset (get (cdr command) :v-offset (getf defaults :v-offset 0))
                                      document))
      ;; now, simulate the impress BITMAP command
      ;; just draw a box
      ;; Imagen would be 300 pixels per inch.
      ;; We are hsize*32*2^mag
      (let ((divisor (quotient 300.0 (send document :printer-resolution))))
        (tv:%draw-rectangle (round (* width (expt 2 magnification)) divisor)
                            (round (* height (expt 2 magnification)) divisor)
                            (round h-offset divisor)
                            (round v-offset divisor)
                            tv:alu-ior
                            (send document :buffer))))))

;; The special command may contain quote marks, such as in a logical pathname
;; "foo:bar;baz". These get converted to {\tt \char '042} by TEX.

(defun parse-xxx-string (string)
  (do ((offset)
       (bad "{\tt \char '042}"))
      ((not (setq offset (string-search bad string))))
    (setq string (string-append (substring string 0 offset)
                                "/""
                                (substring string (+ offset (length bad))))))
  (condition-case (x)
      (let ((*package* (find-package "USER"))
            #+LMI (*readtable* (si:find-readtable-named "CL")))
        (read-from-string (string-append "(" string ")")))
    (error
     (bad-dvi "Error in parsing \special command: ~S~%~A"
              string
              (send x :report-string)))))



;;; the following code is enabled via (setq si:*default-bit-array-pinter* :bitmap)
;;; in the LMI system. It is a way to create the bitmap files to be used
;;; by the \special{bitmap ...} command.

#+LMI
(defun (:bitmap si:print-bit-array) (printer sarray left top right bottom &rest ignored)
  ;; store the bitmap into an array that is in 32x32.
  (let* ((array (make-array (list (* (ceiling (- bottom top) 32) 32)
                                 (* (ceiling (- right left) 32) 32))
                           :type 'art-1b))
         (string (make-array (floor (* (array-dimension array 0) (array-dimension array 1)) 8) :type 'art-string
                             :displaced-to array)))
    (bitblt tv:alu-seta (- right left) (- bottom top)
            sarray left top
            array 0 0)
    (with-open-stream (stream (open-printer-bitmap-file printer))
      (write-32-le (array-dimension array 0) stream)
      (write-32-le (array-dimension array 1) stream)
      (send stream :string-out string))))


;; the paint program saves an array as (setq symbol <array>)
;; make this work for any fasd-symbol-value of an array.

(defun load-paint-array (filename)
  (declare (values array truename))
  (or (find-package "PAINT") (make-package "PAINT"))
  (LET ((SYMBOL (INTERN ".DRAWING." "PAINT"))
        ARRAY FILE)
    (or (get symbol 'special) (putprop symbol t 'special))
    (PROGV (LIST SYMBOL) (LIST NIL)
      (SETQ FILE (LOAD filename :set-default-pathname nil :VERBOSE NIL))
      (setq array (symeval symbol)))
    (cond ((and (typep ARRAY 'array)
                (equal (array-type ARRAY) 'art-1b)
                (equal (array-rank ARRAY) 2))
           (values array file))
          ((and (typep array 'array)
                (equal (array-type array) 'art-8b)
                (equal (array-rank array) 2))
           (bad-dvi "cant handle grey scale arrays"))
          ('ELSE
           (BAD-DVI "File ~S doesnt contain a paint saved image" file)))))

(defun (:saved-paint-image screen-dvi-xxx) (document command)
  (let (FILE DEFAULTS ARRAY WIDTH HEIGHT MAGNIFICATION H-OFFSET V-OFFSET)
    (multiple-value-setq (array file) (load-paint-array (cadr command)))
    (when (setq defaults (probe-file (send file :new-pathname :type "DEFAULTS" :VERSION :NEWEST)))
      (SETQ DEFAULTS (CAR (FORMS-FROM-FILE DEFAULTS))))
    (SETQ WIDTH (PIXEL-ARRAY-WIDTH ARRAY))
    (SETQ HEIGHT (PIXEL-ARRAY-HEIGHT ARRAY))
    (setq magnification (get (cdr command) :magnification (getf defaults :magnification 0)))
    (setq h-offset (evaluate-offset (get (cdr command) :h-offset (getf defaults :h-offset 0))
                                    document))
    (setq v-offset (evaluate-offset (get (cdr command) :v-offset (getf defaults :v-offset 0))
                                    document))
    ;; now, simulate the impress BITMAP command
    ;; just draw a box
    ;; Imagen would be 300 pixels per inch.
    (let ((divisor (quotient 300.0 (send document :printer-resolution))))
      (tv:%draw-rectangle (round (* width (expt 2 magnification)) divisor)
                          (round (* height (expt 2 magnification)) divisor)
                          (round h-offset divisor)
                          (round v-offset divisor)
                          tv:alu-ior
                          (send document :buffer)))))







(defun forms-from-file (filename)
  (with-open-file (stream filename)
    (let ((gp (send (send stream :pathname) :generic-pathname)))
      (FS:READ-ATTRIBUTE-LIST GP STREAM)
    ;; Enter appropriate environment for the file
    (MULTIPLE-VALUE-BIND (VARS VALS)
        (FS:FILE-ATTRIBUTE-BINDINGS GP)
      (PROGV VARS VALS
        (DO ((EOF (LIST NIL))
             (FORM)
             (FORMS NIL (CONS FORM FORMS)))
            ((EQ (SETQ FORM (READ STREAM EOF)) EOF)
             (NREVERSE FORMS))))))))
