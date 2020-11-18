;;; -*- Mode:LISP; Package:USER; Base:10.; Readtable: ZetaLisp -*-
;  Gears2.Lisp
;  DDBII


(defun shell-sort-number-list (number-list)             ; standard shell sort algorithm
  (let ((len (length number-list))
        (temp-num))
    (do ((gap (// len 2) (// gap 2))) ((<= gap 0))
      (do ((i gap (1+ i))) ((>= i len))
        (do ((j (- i gap) (- j gap))) ((minusp j))
          (when (> (elt number-list j) (elt number-list (+ j gap)))
            (setf temp-num (elt number-list j))
            (setf (elt number-list j) (elt number-list (+ j gap)))
            (setf (elt number-list (+ j gap)) temp-num)))))
    (return-from shell-sort-number-list number-list)))


(defstruct gear-form                            ; gear structure
  (ratio  0.0)                                  ; ratio (float) of front/rear
  (front  0)                                    ; number of front cogs
  (rear   0)                                    ; number of rear cogs
  (distance  0.0)                               ; distance traveled in feet
  )

(defun gears ()
  (let ((gear)
        (number-of-front-cogs)  (front-list)
        (number-of-rear-cogs)   (rear-list)
        (total-number-of-gears)
        (pointer-list () )      (ratio-list)    (sort-list)
        (diameter)
        (p 0)
        )
    (format t "~%Enter list of front cog teeth:  ")  (setq number-of-front-cogs (length (setq front-list (read))))
    (format t "~%Enter list of rear cog teeth:   ")  (setq number-of-rear-cogs (length (setq rear-list (read))))
    (format t "~%Diameter(inches) of rear wheel: ")  (setq diameter (read))
    (setq total-number-of-gears (* number-of-front-cogs number-of-rear-cogs))
    (setq ratio-list (make-list total-number-of-gears :initial-element 0.0))
    (setq gear (make-array (list total-number-of-gears)))
    (do ((f 0 (1+ f))) ((>= f number-of-front-cogs))
      (do ((r 0 (1+ r))) ((>= r number-of-rear-cogs))
        (setf (elt ratio-list p) (// (float (elt front-list f)) (elt rear-list r)))
        (setf (aref gear p) (make-gear-form
                              :front (elt front-list f)
                              :rear  (elt rear-list r)
                              :ratio (elt ratio-list p)
                              :distance (// (* (elt ratio-list p) pi diameter) 12.0)))
        (setq p (1+ p))))
    (setq sort-list (shell-sort-number-list (copy-list ratio-list)))
    (do ((i 0 (1+ i))) ((>= i total-number-of-gears))
      (setq pointer-list
            (append pointer-list
                    (list (position (elt sort-list i) ratio-list :test #'equal)))))
    (format t "~2%Order  Ratio  front  rear   distance (feet)~2%")
    (do ((i 0 (1+ i))) ((>= i total-number-of-gears))
      (format t " ~2d    ~5,2f   ~2d     ~2d    ~7,2f~%"
              (1+ i)
              (ratio (aref gear (elt pointer-list i)))
              (front (aref gear (elt pointer-list i)))
              (rear (aref gear (elt pointer-list i)))
              (distance (aref gear (elt pointer-list i)))))
    (values)
    ))
