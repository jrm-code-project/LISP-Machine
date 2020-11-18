;;; -*- Mode:LISP; Package:HACKS; Lowercase:T; Base:8; Readtable:ZL -*-

(defun qix (&optional (length 100) (stream *terminal-io*) (times NIL))
 (with-real-time
   (let* ((list (make-list (1+ length)))
          (history (nthcdr (1- length) list)))
     (%p-store-cdr-code (cdr history) cdr-error)
     (%p-store-cdr-code history cdr-normal)
     (rplacd history list)
     (send stream :clear-window)
     (loop repeat length
           for h = history then (cdr h)
           do (setf (car h) (make-list 4)))
     (multiple-value-bind (xlim ylim)
         (send stream :inside-size)
       (loop with x1 = 0
             and y1 = (1- ylim)
             and x2 = 0
             and y2 = (1- ylim)
             and dx1 = 5
             and dy1 = 12
             and dx2 = 12
             and dy2 = 5
             with tem
             until (or (send stream :tyi-no-hang)
                       (if times (= (setq times (1- times)) 0) NIL))
             when (caar history)
             do (send stream :draw-line
                      (first (car history))
                      (second (car history))
                      (third (car history))
                      (fourth (car history))
                      tv:alu-xor)
             do (setf (first (car history)) x1)
             (setf (second (car history)) y1)
             (setf (third (car history)) x2)
             (setf (fourth (car history)) y2)
             (setq history (cdr history))
             (send stream :draw-line x1 y1 x2 y2 tv:alu-xor)
             (setq dx1 (1- (+ dx1 (random 3)))
                   dy1 (1- (+ dy1 (random 3)))
                   dx2 (1- (+ dx2 (random 3)))
                   dy2 (1- (+ dy2 (random 3))))
             (cond ((> dx1 12) (setq dx1 12))
                   ((< dx1 -12) (setq dx1 -12)))
             (cond ((> dy1 12) (setq dy1 12))
                   ((< dy1 -12) (setq dy1 -12)))
             (cond ((> dx2 12) (setq dx2 12))
                   ((< dx2 -12) (setq dx2 -12)))
             (cond ((> dy2 12) (setq dy2 12))
                   ((< dy2 -12) (setq dy2 -12)))
             (cond ((or ( (setq tem (+ x1 dx1)) xlim)
                        (minusp tem))
                    (setq dx1 (- dx1))))
             (cond ((or ( (setq tem (+ x2 dx2)) xlim)
                        (minusp tem))
                    (setq dx2 (- dx2))))
             (cond ((or ( (setq tem (+ y1 dy1)) ylim)
                        (minusp tem))
                    (setq dy1 (- dy1))))
             (cond ((or ( (setq tem (+ y2 dy2)) ylim)
                        (minusp tem))
                    (setq dy2 (- dy2))))
             (setq x1 (+ x1 dx1)
                     y1 (+ y1 dy1)
                     x2 (+ x2 dx2)
                     y2 (+ y2 dy2))
          finally (loop repeat length
                        when (caar history)
                         do (send stream :draw-line
                                  (first (car history))
                                  (second (car history))
                                  (third (car history))
                                  (fourth (car history))
                                  tv:alu-xor)
                        do (setq history (cdr history))))))))

;(defdemo "Qix" "Not the arcade game" (qix))

;; lets try that again, less hackishly coded, and faster. -gjc

(defun nqix (&optional (length 100) (stream terminal-io) (times (lsh -1 -1)) &aux xlim ylim)
  (multiple-value (xlim ylim) (send stream :inside-size))
  (send stream :clear-window)
  (send stream :clear-input)
  (with-real-time
    (tv:prepare-sheet (stream)
      (nqix-run (make-array (* length 4)) stream times xlim ylim))))


(defun nqix-run (history stream times xlim ylim)
  (declare (fixnum length times xlim ylim))
  (do ((j 0 (1+ j))
       (index 0)
       (length (// (length history) 4))
       (x1 0)
       (y1 (1- ylim))
       (x2 0)
       (y2 (1- ylim))
       (dx1 5)
       (dy1 12)
       (dx2 12)
       (dy2  5)
       (tem 0)
       (qtem))
      ((or (= j times) (sys:kbd-hardware-char-available))
       (dotimes (j length)
         (setq index (* j 4))
         (if (setq qtem (aref history index))
             (sys:%draw-line qtem
                             (aref history (+ index 1))
                             (aref history (+ index 2))
                             (aref history (+ index 3))
                             tv:alu-xor
                             t
                             stream))))
    (declare (fixnum j index  x1 y1 x2 y2 dx1 dy1 dx2 dy2 tem))
    (setq index (* (\ j length) 4))
    (if (setq qtem (aref history index))
        (sys:%draw-line qtem
                        (aref history (+ index 1))
                        (aref history (+ index 2))
                        (aref history (+ index 3))
                        tv:alu-xor
                        t
                        stream))
    (setf (aref history (+ index 0)) x1)
    (setf (aref history (+ index 1)) y1)
    (setf (aref history (+ index 2)) x2)
    (setf (aref history (+ index 3)) y2)
    (sys:%draw-line x1 y1 x2 y2 tv:alu-xor t stream)
    (setq dx1 (1- (+ dx1 (random 3)))
          dy1 (1- (+ dy1 (random 3)))
          dx2 (1- (+ dx2 (random 3)))
          dy2 (1- (+ dy2 (random 3))))
    (cond ((> dx1 12) (setq dx1 12))
          ((< dx1 -12) (setq dx1 -12)))
    (cond ((> dy1 12) (setq dy1 12))
          ((< dy1 -12) (setq dy1 -12)))
    (cond ((> dx2 12) (setq dx2 12))
          ((< dx2 -12) (setq dx2 -12)))
    (cond ((> dy2 12) (setq dy2 12))
          ((< dy2 -12) (setq dy2 -12)))
    (cond ((or ( (setq tem (+ x1 dx1)) xlim)
               (< tem 0))
           (setq dx1 (- dx1))))
    (cond ((or ( (setq tem (+ x2 dx2)) xlim)
               (< tem 0))
           (setq dx2 (- dx2))))
    (cond ((or ( (setq tem (+ y1 dy1)) ylim)
               (< tem 0))
           (setq dy1 (- dy1))))
    (cond ((or ( (setq tem (+ y2 dy2)) ylim)

               (< tem 0))
           (setq dy2 (- dy2))))
    (setq x1 (+ x1 dx1)
          y1 (+ y1 dy1)
          x2 (+ x2 dx2)
          y2 (+ y2 dy2))))

(defdemo "Qix" "Not the arcade game" (nqix))
