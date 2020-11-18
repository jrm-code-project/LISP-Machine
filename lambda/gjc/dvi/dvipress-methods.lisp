;;; -*- Mode: LISP; Syntax: Zetalisp; Package: DVI; Base: 10; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file contains the methods for translation of DVI commands
;; to Press format.
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
;;     :start-output
;;     :end-output
;;     :do-pages
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defflavor dover-dvi-document
         ()
         (document))


(defmethod (dover-dvi-document :initialize) ()
  (send self :set-printer-name "DOVER")
  (send self :set-printer-resolution 384)
  (send self :set-printer-max-fonts 64)
  (send self :set-fontmap (make-array 64 :fill-pointer 0))
  (send self :set-buffer (make-instance 'press-stream))
  (send (send self :buffer) :set-output-stream
        (setq outfile (open-8b-output outfile)))
  (send (send self :buffer) :set-press-pathname outfile)
  (send self :set-fontnums (make-array 64 :fill-pointer 0)))

(defmethod (dover-dvi-document :normal-ending) ()
  (close (send (send self :buffer) :output-stream)))

(defmethod (dover-dvi-document :abnormal-ending) ()
  (close (send (send self :buffer) :output-stream) t))


(defmethod (dover-dvi-document :start-output)()
  (send buffer :add-fonts fontmap)
  (format t "~&   Processing pages.... ")
  )

(defmethod (dover-dvi-document :end-output)()
  (format t "~& Finishing output ... ")
  (send buffer :output-font-directory-part)
  (send buffer :output-part-directory)
  (send buffer :output-document-directory))

(defmethod (dover-dvi-document :do-pages)()
  (move-back file-buffer (1- total-pages))
  (let ((current-page 1)
        bop-params)
    (loop for command = (bget-byte file-buffer)
          do
      (if ( command bop)(bad-dvi "bop command missing"))
      (dotimes (i 11) ; collect  the c0 to c9 params and prev page ptr
        (setq bop-params (cons (bsigned-quad file-buffer) bop-params)))
      (send self :dover-bop (reverse bop-params))
      (format t "~D " current-page)

      ;;do the page
      (loop for cmnd = (bget-byte file-buffer)
            do
;       (if ( cmnd pre)(bad-dvi "invalid DVI command between bop and eop"))
        (send self
              (get-message-name cmnd)
              (get-parameters cmnd file-buffer))
        (if (= cmnd eop)(return (values))))

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
          (return (values)));else stop
      )))

(defmethod (dover-dvi-document :set-char)(charcode)
  (send buffer :maybe-new-entity)
  (send buffer :press-show-char charcode)
    ;;advance internal h and correct for cumulative errors.
  (let ((tfmfntdef (send *font-definitions* :get-hash current-font)))
    (incf h (aref (tfd-dvi-widths tfmfntdef) charcode))))


(defmethod (dover-dvi-document :put-char)(charcode)
  (send buffer :maybe-new-entity)
  (send buffer :press-put-char charcode (* dvi2mica h)))

(defmethod (dover-dvi-document :set-rule)(params)
  (send buffer :maybe-new-entity)
  (send buffer :set-graphic-x (* dvi2mica h))
  (let ((ht (car params))
        (wd (cdr params)))
    (cond ((and (plusp ht)(plusp wd))
           (send buffer :press-set-rectangle (* dvi2mica wd)(* dvi2mica ht))
           (incf h wd)))))

(defmethod (dover-dvi-document :put-rule)(params)
  (send buffer :maybe-new-entity)
  (let ((ht (car params))
        (wd (cdr params)))
    (and (and (plusp ht)(plusp wd))
         (send buffer :press-draw-rectangle (* dvi2mica wd)(* dvi2mica ht)))))

(defmethod (dover-dvi-document :nop)(ignore)
  ;do nothing
  )

(defmethod (dover-dvi-document :right)(delta-h)
  (send buffer :maybe-new-entity)
  ;params - amount in dvi units to move h in the right direction
  (incf h delta-h)
  (let ((graphic-x (* dvi2mica h)))
    (send buffer :graph-set-x graphic-x)))

(defmethod (dover-dvi-document :w)(new-w)
  (send buffer :maybe-new-entity)
  ;params  - = nil if current value of w is to be used
  ;            = gives new value of w if not nil
  (if (numberp new-w)(setq w new-w))
  (incf h w)
  (let ((graphic-x (* dvi2mica h)))
    (send buffer :graph-set-x graphic-x)))

(defmethod (dover-dvi-document :x)(new-x)
  (send buffer :maybe-new-entity)
  ;params  - = nil if current value of x is to be used
  ;            = gives new value of x if not nil
  (if (numberp new-x)(setq x new-x))
  (incf h x)
  (let ((graphic-x (* dvi2mica h)))
    (send buffer :graph-set-x graphic-x)))

(defmethod (dover-dvi-document :down)(delta-v)
  (send buffer :maybe-new-entity)
  ;params - amount to move pen down in dvi units
  (incf v delta-v)
  (let ((graphic-y (- (send buffer :height)(* dvi2mica v))))
    (send buffer :graph-set-y graphic-y)))

(defmethod (dover-dvi-document :y)(new-y)
  (send buffer :maybe-new-entity)
  (if (numberp new-y)(setq y new-y))
  (incf v y)
  (let ((graphic-y (- (send buffer :height)(* dvi2mica v))))
    (send buffer :graph-set-y graphic-y)))

(defmethod (dover-dvi-document :z)(new-z)
  (send buffer :maybe-new-entity)
  (if (numberp new-z)(setq z new-z))
  (incf v z)
  (let ((graphic-y (- (send buffer :height)(* dvi2mica v))))
    (send buffer :graph-set-y graphic-y)))


(defmethod (dover-dvi-document :bop)(ignore)
  ;params - the list of the c0 to c9 params and the previous page pointer
  (setq h xpage-offset
        v ypage-offset
        w 0 x 0 y 0 z 0)
  (send buffer :set-graphic-x (* xpage-offset dvi2mica))
  (send buffer :set-graphic-y (- (send buffer :height)(* dvi2mica ypage-offset)))
  (send buffer :begin-new-page)
  (send self :empty-stack)
  )


(defmethod (dover-dvi-document :eop)(ignore)
  (send buffer :output-part)
  )

(defmethod (dover-dvi-document :after :pop-stack) (ignore)
  (send buffer :graph-set-x (* dvi2mica h))
  (send buffer :graph-set-y (- (send buffer :height)(* dvi2mica v))))

(defmethod (dover-dvi-document :set-fnt)(texfntnum)
  ;params - number of font to set to (TeX number)
  (let ((fntnum (get-fntnum texfntnum fontnums)))
    (setq current-font (aref fontmap fntnum))
    (if (null current-font)(bad-dvi "Undefined font"))
    (send buffer :set-graphic-x (* h dvi2mica))
    (send buffer :set-graphic-y (- (send buffer :height)(* dvi2mica v)))
    (send buffer :set-font current-font)))
