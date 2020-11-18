;;; -*- Mode:LISP; Package:(ktv USE (K prims)); Readtable:CL; Base:10 -*-

(import '(nc:defafun nc:ndisassemble nc:link))


#||||

Load this

ktv::(write-vcmem read-vcmem
      munch draw-point draw-hline draw-vline draw-rectangle draw-test
      close-enuf draw-line draw-line-1)

||||#

(defun munch ()
  (do ((time  (hw:unboxed-constant 0))
       (x     (hw:unboxed-constant 0)  (hw:32logand (hw:32-1+ x) (hw:unboxed-constant #x1FF)))
       (xword (hw:unboxed-constant 0))
       (xbit  (hw:unboxed-constant 0)  (hw:32logical-shift-up xbit 1.)))
      (())
    (when (hw:32zerop xbit)
      (setq xbit (hw:unboxed-constant 1))
      (when (hw:32zerop (setq xword (hw:32logand (hw:32-1+ xword)
                                                 (hw:unboxed-constant #x1F))))
        (setq time (hw:32+ time (hw:unboxed-constant #o52525)))))
    ;; use xor mode
    (write-vcmem (hw:32+ (hw:32logical-shift-up (hw:32logand (hw:unboxed-constant #x1FF)
                                                             (hw:32logxor x time))
                                                5.)
                         xword)
                 xbit)))


(defconstant screen-virtual-address (hw:unboxed-constant #.kbug::screen-virtual-address))

(defun write-vcmem (addr data)
  (hw:write-vma-unboxed (hw:32+ addr screen-virtual-address))
  (hw:md-start-write-unboxed data)
  data)

(defun read-vcmem (addr)
  (hw:vma-start-read-vma-unboxed-md-unboxed (hw:32+ addr screen-virtual-address))
  (hw:read-md))


(defmacro xy-addr (x y)
  `(let ((temp-x ,x))
     (values (hw:dpb-unboxed ,y (byte 10. 5.) (hw:ldb temp-x (byte 5. 5.) (hw:unboxed-constant 0)))
             (hw:32logand temp-x (hw:unboxed-constant #x1F)))))

(defun draw-point (x y)
  (multiple-value-bind (addr bit)
      (xy-addr x y)
    (write-vcmem addr
                 (hw:32set-bit bit (read-vcmem addr)))))

(defun draw-hline (y from to)
  (do ((x from (1+ x)))
      ((> x to))
    (draw-point x y)))

(defun draw-vline (x from to)
  (do ((y from (1+ y)))
      ((> y to))
    (draw-point x y)))


(defun draw-rectangle (top left bottom right)
  (draw-vline right top bottom)
  (draw-hline top left right)
  (draw-hline bottom left right)
  (draw-vline left top bottom))


(defun draw-test ()
  (lisp:compiler-let ((nc::*allow-setq-of-global-constant-register* t))
    (setq gr::*all-zero* (hw:unboxed-constant 0))
    (setq gr::*one* 1)
    (setq gr::*minus-one* -1))
  (do ((y 20 (+ y 20)))
      ((> y 600))
    (do ((x 20 (+ x 20)))
        ((> x 600))
      (draw-line 300 300 x y)))
;  (draw-rectangle 20 20 500 500)
;  (draw-rectangle 40 40 480 480)
;  (draw-rectangle 60 60 460 460)
;  (draw-rectangle 80 80 440 440)
  (loop))


(defun draw-line (x1 y1 x2 y2)
  (draw-point x1 y1)
  (draw-point x2 y2)
  (draw-line-1 x1 y1 x2 y2))

(defun draw-line-1 (x1 y1 x2 y2)
;  (print (list x1 y1 x2 y2))
    (let ((x-mid (ash (+ x1 x2) -1))
          (y-mid (ash (+ y1 y2) -1)))
      (draw-point x-mid y-mid)
      (when (not (and (close-enuf x1 x2) (close-enuf y1 y2)))
        (draw-line-1 x1 y1 x-mid y-mid)
        (draw-line-1 x-mid y-mid x2 y2))))

(defun close-enuf (p1 p2)
  (if (or (= p1 p2)
          (let ((d (- p1 p2)))
            (or (= d -1)
                (= d 1))))
      1 nil))

(defun draw-point (x y)
  (zl:send tv:selected-window :draw-point x y))
