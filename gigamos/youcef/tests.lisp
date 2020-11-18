;;; -*- Mode:LISP; Package:li; Base:10; Readtable:CL -*-


(defun test-rect-1 (x y z u v w &optional (screen-device *screen-address*))
  (clear-vcmem)
  (do ((i 0 (+ i 32.)))
      ((>= i 700.))
    (do ((j 0 (+ j 72.)))
        ((>= j 660.))
      (draw-rectangle 30. 70. i  j 7. nil)))
  (loop)
  )

(defun test-rect-2 (width height &optional (screen-device *screen-address*))
  (clear-vcmem)
  (setq width 33.)
  (setq height 33.)
  (do* ((inc-h (1+ width))
        (i 16. (+ i inc-h)))
       ((>= i 700.))
    (do* ((inc-v (1+ height))
          (j 16. (+ j inc-v)))
         ((>= j 660.))
      (draw-rectangle width height i j 7. nil))
;    (error "Done with first column"))
    )
  (loop)
  )

(defun test-rect-3 (x y z &optional (screen-device *screen-address*))
  (clear-vcmem)
  (draw-rectangle 31. 100 32. 32. 7. nil)
  (draw-rectangle 100 16. 64. 32. 7. nil)
  (draw-rectangle 31. 200. 165. 32. 7. nil)
  (draw-rectangle 16. 200. 196. 32. 7. nil)
  (loop)
  )

(defun test-draw-line-1 (x y z)
  (clear-vcmem)
  (error "Done Clearing screen")
  (draw-line 200 200 600 600 7. t nil)
  (loop)
  )

(defun test-draw-line-2 (x)
  (clear-vcmem)
  (draw-line 200 200 600 200 7. t nil)
  (draw-line 600 200 600 600 7. t nil)
  (draw-line 600 600 200 600 7. t nil)
  (draw-line 200 600 200 200 7. t nil)
  (draw-line 200 200 600 600 7. t nil)
  (draw-line 600 200 200 600 7. t nil)
  (loop)
  )

(defun haulong (n)
  (if (zerop n)
      0
    (hw:24- 0 (hw:24- (hw:24-prioritize n) 24.))
    )
  )

(defun isqrt (n)
  (cond ((not (integerp n))
         (error "ISQRT called with ~S, which is not an integer" n))
        ((= n 0) 0)                             ;otherwise, it would loop
        (t (do ((guess (ash 1 (hw:ldb-boxed (1- (haulong n)) (byte 23 1) 0))
                   (hw:24+ epsilon guess))
                (epsilon))
               ((zerop (setq epsilon (new-math:truncate (- n (new-math:multiply-fixnum guess guess))
                                                        (hw:dpb-boxed guess (byte 23. 1) 0))))
                ;; We are now within 1, but might be too high
                (if (> (new-math:multiply-fixnum guess guess) n)
                    (hw:24- guess 1)
                  guess))))))

(defun compute-other-dim (radius x &aux y)
  (setq y (hw:24- radius (new-math:multiply-fixnum x x)))
  (isqrt y)
  )

(defun draw-line-3 (w)
  (clear-vcmem)
  (let ((center-x 200)
        (center-y 200)
        (radius 100.)
        (radius-square 10000.)
        (step 5.)
        (x 0.)
        (y 0)
        )
    (loop
      (setq x 0)
      (setq y 0)
      (dotimes (i 20.)
        (setq y (compute-other-dim radius-square x))
        (draw-line center-x center-y (+ center-x x) (+ center-y y) 7 t nil)
        (draw-line center-x center-y (- center-x x) (+ center-y y) 7 t nil)
        (draw-line center-x center-y (+ center-x x) (- center-y y) 7 t nil)
        (draw-line center-x center-y (- center-x x) (- center-y y) 7 t nil)
        (setq x (hw:24+ x step))
        )
      )
    )
  )


(defun test-sqrt (x1 x2 x3)
  (setq x1 (isqrt 4))
  (setq x2 (isqrt 6))
  (setq x3 (isqrt 81.))
  (loop)
  )

(defun test-draw-char-1 (v)
;  (clear-vcmem)
  (draw-rectangle 400 400 200 200 6. nil)
  (draw-line 390 390 410 390 6. t nil)
  (draw-line 410 390 410 410 6. t nil)
  (draw-line 410 410 390 410 6. t nil)
  (draw-line 390 410 390 390 6. t nil)
  (draw-line 390 390 200 200 6. t nil)
  (draw-line 390 410 200 600 6. t nil)
  (draw-line 410 390 600 200 6. t nil)
  (draw-line 410 410 600 600 6. t nil)
  (let ((font (array:make-array 3 :element-type '(array:unsigned-byte 32.)))
        (width 8)
        (height 9)
        tv-screen-buffer-address
        tv-screen-buffer-end-address
        tv-bit-offset
        tv-screen-locations-per-line
        tv-screen-buffer-pixel-size-mrot
        font-word-address x y z)
    (setq v font)
;    (error "foo")
    (array:aset-n #x42242418 font 0)
    (array:aset-n #x8181817e font 1)
    (array:aset-n #x81 font 2)
    (multiple-value-setq (x y z font-word-address) (array:decode-array font))
    (multiple-value-setq (tv-screen-locations-per-line
                          tv-bit-offset
                          tv-screen-buffer-address
                          tv-screen-buffer-end-address
                          tv-screen-buffer-pixel-size-mrot)
      (select-sheet nil))
    (multiple-value-setq (tv-screen-buffer-address tv-bit-offset)
      (tv-xy-address 396 396
                     tv-screen-locations-per-line
                     tv-bit-offset
                     tv-screen-buffer-address
                     tv-screen-buffer-pixel-size-mrot))
;   (error "About to go draw the sucker")
    (draw-char-no-check width
                        height
                        tv-screen-buffer-address
                        tv-bit-offset
                        tv-screen-locations-per-line
                        font-word-address
                        6))
  (loop)
  )

(defun make-squares-on-screen (width height)
  (clear-vcmem)
  (setq width 33.)
  (setq height 33.)
  (do* ((inc-h (1+ width))
        (i 16. (+ i inc-h)))
       ((>= i 700.))
    (do* ((inc-v (1+ height))
          (j 16. (+ j inc-v)))
         ((>= j 660.))
      (draw-rectangle width height i j 7. nil))
    )
  (loop)
  )

(defun move-char-horizontal (font-word-address start-x start-y inc already-there
                             tv-screen-locations-per-line tv-bit-offset tv-screen-buffer-address
                             tv-screen-buffer-pixel-size-mrot)
  (multiple-value-setq (tv-screen-buffer-address tv-bit-offset)
    (tv-xy-address  start-x start-y
                    tv-screen-locations-per-line
                    tv-bit-offset
                    tv-screen-buffer-address
                    tv-screen-buffer-pixel-size-mrot))
  (dotimes (i 19.)
    (when already-there
      (draw-char-no-check 8 10. tv-screen-buffer-address tv-bit-offset tv-screen-locations-per-line font-word-address 6.))
    (setq tv-screen-buffer-address (hw:24+ inc tv-screen-buffer-address))
    (draw-char-no-check 8 10. tv-screen-buffer-address tv-bit-offset tv-screen-locations-per-line font-word-address 6.)
    (dotimes (j 500000.)
      (setq j j))
    (setq already-there t)
    )
  )


(defun test-draw-char-2 ()
  (clear-vcmem)
  (make-squares-on-screen nil nil)
  (draw-rectangle 400 400 200 200 6. nil)
  (draw-line 390 390 410 390 6. t nil)
  (draw-line 410 390 410 410 6. t nil)
  (draw-line 410 410 390 410 6. t nil)
  (draw-line 390 410 390 390 6. t nil)
  (draw-line 390 390 200 200 6. t nil)
  (draw-line 390 410 200 600 6. t nil)
  (draw-line 410 390 600 200 6. t nil)
  (draw-line 410 410 600 600 6. t nil)
  (let ((font (array:make-array 9 :element-type '(array:unsigned-byte 32.)))
        char-b-address char-y-address
        tv-screen-buffer-address
        tv-screen-buffer-end-address
        tv-bit-offset
        tv-screen-locations-per-line
        tv-screen-buffer-pixel-size-mrot
        font-word-address x)
    ;; char A
    (array:aset-n #x42242418 font 0)
    (array:aset-n #x8181817e font 1)
    (array:aset-n #x81 font 2)
    ;; char B
    (array:aset-n #x63637f3f font 3)
    (array:aset-n #xc3c3737f font 4)
    (array:aset-n #x7fff font 5)
    ;; char y
    (array:aset-n #x66e3e3e3 font 6)
    (array:aset-n #x1818183e font 7)
    (array:aset-n #x1818 font 8)
    (multiple-value-setq (char-b-address char-y-address x font-word-address) (array:decode-array font))
    (multiple-value-setq (tv-screen-locations-per-line
                          tv-bit-offset
                          tv-screen-buffer-address
                          tv-screen-buffer-end-address
                          tv-screen-buffer-pixel-size-mrot)
      (select-sheet nil))
    (multiple-value-setq (tv-screen-buffer-address tv-bit-offset)
      (tv-xy-address 396 396
                     tv-screen-locations-per-line
                     tv-bit-offset
                     tv-screen-buffer-address
                     tv-screen-buffer-pixel-size-mrot))
    (draw-char-no-check 8 10.
                        tv-screen-buffer-address
                        tv-bit-offset
                        tv-screen-locations-per-line
                        font-word-address
                        6)
    (setq char-b-address (hw:24+ 3 font-word-address))
    (setq char-y-address (hw:24+ 6 font-word-address))
    (multiple-value-setq (tv-screen-locations-per-line
                          tv-bit-offset
                          tv-screen-buffer-address
                          tv-screen-buffer-end-address
                          tv-screen-buffer-pixel-size-mrot)
      (select-sheet nil))
    (loop
      (move-char-horizontal char-b-address 32 32 1. nil
                             tv-screen-locations-per-line tv-bit-offset tv-screen-buffer-address
                             tv-screen-buffer-pixel-size-mrot)
;      (move-char-horizontal char-b-address 32. 32 -1. t
;                            tv-screen-locations-per-line tv-bit-offset tv-screen-buffer-address
;                            tv-screen-buffer-pixel-size-mrot)
      (move-char-horizontal char-y-address 32 642. 1. nil
                             tv-screen-locations-per-line tv-bit-offset tv-screen-buffer-address
                             tv-screen-buffer-pixel-size-mrot)
;      (move-char-horizontal char-y-address 576. 628. -1. t
;                            tv-screen-locations-per-line tv-bit-offset tv-screen-buffer-address
;                            tv-screen-buffer-pixel-size-mrot)
      )
    )
  )


;;; test function

(defvar *source-array* nil)

(defvar *dest-array* nil)

(defun init-arrays-for-bitblt ()
  (setq *source-array* (array:make-array '(16. 1) :element-type '(array:unsigned-byte 32.)))
  (setq *dest-array* (array:make-array '(1024. 1024.) :element-type '(array:unsigned-byte 1)
                                       :displaced-to *screen-address*))
  (array:aset-n #xf047f047 *source-array* 0 0)
  (array:aset-n #x08a408a4 *source-array* 1 0)
  (array:aset-n #x05120512 *source-array* 2 0)
  (array:aset-n #x02190219 *source-array* 3 0)
  (array:aset-n #x78127812 *source-array* 4 0)
  (array:aset-n #x10241024 *source-array* 5 0)
  (array:aset-n #x27c227c2 *source-array* 6 0)
  (array:aset-n #x47c147c1 *source-array* 7 0)
  (array:aset-n #x47ff47ff *source-array* 8 0)
  (array:aset-n #x47c047c0 *source-array* 9 0)
  (array:aset-n #x47c047c0 *source-array* 10 0)
  (array:aset-n #x48204820 *source-array* 11 0)
  (array:aset-n #x50105010 *source-array* 12 0)
  (array:aset-n #x20082008 *source-array* 13 0)
  (array:aset-n #x00040004 *source-array* 14 0)
  (array:aset-n #x00020002 *source-array* 15 0)
  (loop)
  )


(defun test-bitblt (x0 y0)
  (let ((ar *source-array*)
        (ar2 *dest-array*))
    (setq x0 ar)
    (setq y0 ar2)
    (clear-vcmem)
    (bitblt 7. 2. 16. ar 0 0 ar2 200. 100.)
    (bitblt 7. 5. 60. ar 0 0 ar2 0. 100.)
    (loop)
    )
  )

(defun test-bitblt-1 (x0 y0)
  (let ((ar *source-array*)
        (ar2 *dest-array*)
        pos)
    (setq x0 ar)
    (setq y0 ar2)
    (clear-vcmem)
;    (bitblt 6. 96. 64. ar 0 0 ar2 0. 400.)
;    (bitblt 6. 96. -64. ar 0 0 ar2 97. 400.)
    (bitblt 6 96. 64. ar 0 0 ar2 31. 400.)
    (bitblt 6 -96. 64. ar 0 0 ar2 31. 600.)
    (bitblt 6. -96. 64. ar 0 0 ar2 32. 600.)
    (bitblt 6 -96. 64. ar 0 0 ar2 31. 600.)
    (bitblt 6. -96. -64. ar 0 0 ar2 32. 600.)
;    (bitblt 6. 96. 64. ar 0 0 ar2 33. 600.)
    (loop)
    )
  )

(defun test-bitblt-2 (x0 y0)
  (let ((ar *source-array*)
        (ar2 *dest-array*)
        pos)
    (setq x0 ar)
    (setq y0 ar2)
    (clear-vcmem)
    (loop
      (bitblt 6. 96. 64. ar 0 0 ar2 0. 400.)
;      (bitblt 6. 96. 64. ar 0 0 ar2 31. 400.)
;      (bitblt 6. 96. 64. ar 0 0 ar2 32. 600.)
      (do ((pos 0))
          ((>= pos 650.)
           (bitblt 6. 96. 64. ar 0 0 ar2 pos 400.))
        (dotimes (j 100000.)
          (setq x0 x0))
        (bitblt 6. -96. 64. ar 0 0 ar2 (1+ pos) 400.)
        (bitblt 6. 96. -64. ar 0 0 ar2 pos 400.)
        (setq pos (1+ pos)))
     )
    )
  )

(defun test-bitblt-3 (x y)
  (let ((ar *source-array*)
        (ar2 *dest-array*))
    (clear-vcmem)
    (bitblt 6 -96. 64. ar 0 0 ar2 0. 400.)
    (loop)
    )
  )

(defun test-bitblt-4 (x)
  (let ((ar *source-array*)
        (ar2 *dest-array*) x-pos y-pos)
    (clear-vcmem)
    (loop
      (bitblt 6 96. 64. ar 0 0 ar2 (setq x-pos 0) (setq y-pos 0))
      (dotimes (i 850.)
        (bitblt 6 -96. 64. ar 0 0 ar2 x-pos y-pos)
;       (dotimes (j 100000.)
;         (setq x 2))
        (bitblt 6 -96. -64. ar 0 0 ar2 x-pos (setq y-pos (1+ y-pos)))
        (dotimes (j 10000.)
          (setq x 2))
        )
      (dotimes (i 700.)
        (bitblt 6 96. -64 ar 0 0 ar2 x-pos y-pos)
;       (dotimes (j 100000.)
;         (setq x 2))
        (bitblt 6 -96. 64. ar 0 0 ar2 (setq x-pos (1+ x-pos)) y-pos)
        (dotimes (j 10000.)
          (setq x 2))
        )
      (dotimes (i 850.)
        (bitblt 6 -96. 64. ar 0 0 ar2 x-pos y-pos)
;       (dotimes (j 5000.)
;         (setq x 2))
        (bitblt 6 96. -64. ar 0 0 ar2 x-pos (setq y-pos (1- y-pos)))
        (dotimes (j 10000.)
          (setq x 2))
        )
      (dotimes (i 700. (bitblt 6 -96. 64 ar 0 0 ar2 x-pos y-pos))
        (bitblt 6 -96. -64 ar 0 0 ar2 x-pos y-pos)
;       (dotimes (j 100000.)
;         (setq x 2))
        (bitblt 6 96. -64. ar 0 0 ar2 (setq x-pos (1- x-pos)) y-pos)
        (dotimes (j 10000.)
          (setq x 2))
        )
      )
    )
  )

(defun test-make-array (x y z)
  (setq z (list 0 1 2 3 123 45 67))
  (setq y (array:make-array 40 :element-type 'li:(unsigned-byte 16.)))
  (setq x (array:zl-make-array 30.
                               :element-type 'li:(unsigned-byte 16.)
                               :named-structure-symbol 'sys-conf
                               :displaced-index-offset 0
                               :displaced-to y
                               :leader-length 3)
          )
  (array:store-array-leader 10. x 2)
  (setq z (list (array:array-leader x 2) z y x))
  (loop)
  )


(defun init (x y z)
  (init-config-structure)
  (li:error "Inited config structure vars")
  (init-config-structure-props)
  (li:error "Inited config structure props")
  (find-processor-configuration-structure)
;  (li:error "built configuration structure")
  (setq x *sys-conf*)
  (setq y *my-proc-conf*)
  (setq z *other-processors*)
  (loop)
  )

(defun read-config (x y z u t v w l p k)
  (setq x (%system-configuration-processor-block-size *sys-conf*)
        y (%system-configuration-number-of-processors *sys-conf*))
  (loop))
;;;
;;; compile on lambda before compiling the config-defs file.
;;;
(defun init-config-structure ()
  (init-config-variables)
;  (MAPC #'(LAMBDA (x) (setf (get x 'system-constant) t)) system-configuration-qs)
  (si:assign-values system-configuration-qs)
;  (MAPC #'(LAMBDA (x) (setf (get x 'system-constant) t)) processor-configuration-qs)
  (si:assign-values processor-configuration-qs)
;  (MAPC #'(LAMBDA (x) (IF (SYMBOLP x) (setf (get x 'special) t))) lambda-processor-switches-bits)
  (si:assign-alternate lambda-processor-switches-bits)
  (setq lambda-processor-switches-bits-symbols (si:get-alternate lambda-processor-switches-bits))
;  (MAPC #'(LAMBDA (x) (IF (SYMBOLP x) (setf (get x 'special) t))) proc-conf-boot-commands)
  (si:assign-alternate proc-conf-boot-commands)
  (setq proc-conf-boot-commands-symbols (si:get-alternate proc-conf-boot-commands))
;  (MAPC #'(LAMBDA (x) (setf (get x 'system-constant) t)) chaos-share-dev-qs)
  (si:assign-values chaos-share-dev-qs)
;  (MAPC #'(LAMBDA (x) (IF (SYMBOLP x) (setf (get x 'special) t))) chaos-share-dev-csr-bits)
  (si:assign-alternate chaos-share-dev-csr-bits)
  (setq chaos-share-dev-csr-bits-symbols (si:get-alternate chaos-share-dev-csr-bits))
;  (MAPC #'(LAMBDA (x) (setf (get x 'system-constant) t)) share-tty-qs)
  (si:assign-values share-tty-qs)
;  (MAPC #'(LAMBDA (x) (IF (SYMBOLP x) (setf (get x 'special) t))) share-tty-csr-bits)
  (si:assign-alternate share-tty-csr-bits)
  (setq share-tty-csr-bits-symbols (si:get-alternate share-tty-csr-bits))
;  (MAPC #'(LAMBDA (x) (setf (get x 'system-constant) t)) share-struct-qs)
  (si:assign-values share-struct-qs)
;  (MAPC #'(LAMBDA (x) (setf (get x 'system-constant) t)) intmap-qs)
  (si:assign-values intmap-qs)
  (setq intmap-types-symbols (si:get-alternate intmap-types))
;  (MAPC #'(LAMBDA (x) (IF (SYMBOLP x) (get x 'special) t)) intmap-types)
  (si:assign-alternate intmap-types)
;  (MAPC #'(LAMBDA (x) (setf (get x 'system-constant) t)) sdu-interrupt-numbers)
  (si:assign-values sdu-interrupt-numbers)
  )

(eval-when (compile load eval)
(defun make-forms-for-sym (sym)
  (let ((set-func (intern (si:format nil "SET-~A" sym) (si:symbol-package sym))))
    `((declare (special ,sym))
      (defun ,sym (array-16b)
        (dpb (array:aref array-16b (1+ (* ,sym 2)))
                (byte 16. 16.)
                (array:aref array-16b (* ,sym 2))))
      (defun ,set-func (array-16b val)
        (setf (array:aref array-16b (* ,sym 2)) (ldb (byte 16. 0) val))
        (setf (array:aref array-16b (1+ (* ,sym 2))) (ldb (byte 16. 16.) val))
        val)
      (defsetf ,sym ,set-func))))
)

(defun sys-conf-make (x y)
  (setq *my-proc-conf* nil)
  (setq *other-processors* nil)
  (setq rg-quad-slot
        (select-processor
          (:lambda
            (%lambda-rg-quad-slot))
          (:k (k-slot))))
  (setq sdu-quad-slot
        (select-processor
          (:lambda (%lambda-sdu-quad-slot))
          (:k #XFF)
          )
        )
  (setq *sys-conf* (select-processor
                      (:lambda (make-sys-conf-virtual (%lambda-sys-conf-virtual-adr)))
                      (:k
                         (multiple-value-bind (slot offset)
                             (get-config-structure-pointer)
                           (k-make-sys-conf-virtual slot offset)))))
 ;   (setq x (%system-configuration-processor-block-size *sys-conf*))
 ;   (setq y (%system-configuration-number-of-processors *sys-conf*))
  (loop)
  )


(defun testfoo (x y z u v w n l m)
  (setq ;x (array:aref *sys-conf* 0)
        ;y  (array:aref *sys-conf* 1)
        ;z (array:aref *sys-conf* 2)
        ;u (array:aref *sys-conf* 3)
        v (array:aref *sys-conf* 4)
        w (array:aref *sys-conf* 5)
        )
  (loop)

  )

(defun test-ar-foo (x y z u v w n l m p o)
  (setq x (array:zl-make-array 200 :element-type '(li:unsigned-byte 16.)))
  (setq o (array:zl-make-array 100 :element-type '(li:unsigned-byte 16.) :displaced-to x :displaced-index-offset 0))
  (setf (array:aref x 0) 0)
  (setf (array:aref x 1) 1)
  (setf (array:aref x 2) 2)
  (setf (array:aref x 3) 3)
  (setf (array:aref x 4) 4)
  (setf (array:aref x 5) 5)
  (setf (array:aref x 6) 6)
  (setf (array:aref x 7) 7)
  (setf (array:aref x 8) 8)
  (setq y  (array:aref o 0)
        z (array:aref o 1)
        u (array:aref o 2)
        v (array:aref o 3)
        w (array:aref o 4)
        n (array:aref o 5)
        l (array:aref o 6)
        m (array:aref o 7)
        p (array:aref o 8)
        )
  (loop)

  )

(defun foo (x y)
  3)

(defun bar (x)
  3)

(defun boo (x)
  3)

(defun test-stack ()
  (foo (bar (boo 0)) (bar (boo 1)))
  (loop)
  )



(defun test-x (x y z u v w)
  (setq u (hw:dpb vinc:$$dtp-unboxed-locative vinc::%%data-type #xd0000))
  (hw:vma-start-read-vma-boxed-md-boxed u)
  (setq x (hw:read-md))
  (hw:vma-start-read-vma-boxed-md-unboxed (hw:24+ 1 u))
  (setq y (hw:read-md))
  (hw:vma-start-read-vma-boxed-md-unboxed (hw:24+ 2 u))
  (setq z (hw:read-md))
  (loop)
  )


;; the disk stuff

(warm-compile-and-load-file "jb:youcef.k;share-iopb-1")
(warm-compile-and-load-file "jb:youcef.k;iopb-stuff-1")

(defun init-disk-stuff ()
  (allocate-iopb-and-buffer-memory)
  (li:error "Done initting the disk")
  (loop)
  )

(defun read-label ()
  (lam-read-mini-label nil)
  (loop)
  )


(defun test-ldb ()
  (ldb (byte 8 16.) 977568.))
  (loop)
  )

(defun foo ()
  (ldb (byte 8. 8.) 0)
  (ldb (byte 8. 16.) 0)
  (loop)
  )


;;; some bignum tests


;;; 11 factorial is the first factorial to return a bignum ..

(defun fact-up (x count-down)
  (cond ((= count-down 1) x)
        (t (fact-up (* x count-down) (1- count-down)))))

(defun fact-down (top count-down)
  (cond ((= count-down 1) top)
        (t (fact-down (cl:/ top count-down) (1- count-down)))))

(defun fact-up-and-down (x)
  (fact-down (fact-up 1 x) x))

(defun factorial-test ()
  (when (not (= 1 (fact-up-and-down 11)))
    (li:error "Failed the factorial-test"))
  (li:error "Passes the factorial-test")
  (li:error "What's going on here ??")
  (loop))


;;; can we do 1+ largest fixnum

(defun add-one (x)
  (+ x 1))

(defun test-smallest-bignum ()
  (let ((x (add-one 8388607)))
    (when  (= x 8388608)
      (li:error "Yes equal"))
    (li:error "No")
    (li:error "foo")
    (loop)))

(defun test-logxor ()
  (let ((x (add-one 8388607)))
    (when (null (logxor x 8388608))
      (li:error "Yes zero"))
    (li:error "No")
    (li:error "foo")
    (loop)))



(defun test-ldb-bignum ()
  (let ((x (ldb (byte 8. 16.) -1)))
    (when (= x 255)
      (li:error "Yes, passed !!!"))
    (li:error "No go ...")
    (li:error "foo")
    (loop)))


;;until a new band is built remember to load these files !!


(progn
  (warm-compile-and-load-file "jb:k.math;bignum")
  (warm-compile-and-load-file "jb:k.math;rational")
  )


;; debugging bignums
;; some functions to trace

(progn
  (set-breakpoint 'trap:dt-and-ovf-trap-handler-2)
  (set-breakpoint 'trap:dt-and-ovf-trap-handler-2 #x18)
  (set-breakpoint 'trap:dt-and-ovf-trap-handler-2 #x94)
  (set-breakpoint 'new-math:allocate-bignum)
  (set-breakpoint 'new-math:equal-generic)
  (set-breakpoint 'new-math:compare-generic)
  (set-breakpoint 'new-math:compare-bignum)
  (set-breakpoint 'new-math:generic-math-type-coercer)
  (set-breakpoint 'li:test-smallest-bignum 3)
  (set-breakpoint 'li:test-smallest-bignum 4)
  )
