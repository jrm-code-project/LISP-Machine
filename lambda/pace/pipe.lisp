;;; -*- Mode:LISP; Package:USER; Base:10; Readtable:CL -*-


(defconst *n-inst* 8.)
(defconst *level-width* 64.)
(defconst *tick-width* (round (/ *level-width* 8)))

(defun draw-one-level (x-pos tick-mark)
  (multiple-value-bind (width height)
      (send standard-output :inside-size)
    width
    (let ((start (round (* height .02)))
          (end (round (* height .98))))
      (let ((len (round (/ (- end start) (float *n-inst*)))))
        (send standard-output :draw-line x-pos start x-pos (+ start (* len *n-inst*)))
        (send standard-output :draw-line
              (+ x-pos *level-width*)
              start
              (+ x-pos *level-width*)
              (+ start (* len *n-inst*)))
        (dotimes (i (1+ *n-inst*))
          (send standard-output :draw-line
                x-pos
                (+ start (* i len))
                (+ x-pos *level-width*)
                (+ start (* i len)))
          (when (and tick-mark
                     (< i *n-inst*))
            (send standard-output :draw-line
                  x-pos
                  (+ start (* i len) (round len 2))
                  (+ x-pos *tick-width*)
                  (+ start (* i len) (round len 2)))
            (send standard-output :draw-line
                  (- (+ x-pos *level-width*) *tick-width*)
                  (+ start (* i len) (round len 2))
                  (+ x-pos *level-width*)
                  (+ start (* i len) (round len 2)))))))))

(defun draw-it ()
  (send standard-output :clear-screen)
  (let ((offset 200.))
    (dotimes (i 5)
      (draw-one-level (+ offset (* i (round (* 1.5 *level-width*))))
                      (= i 1)
                      ))))
