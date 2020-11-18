; Gears.Lisp
; functions to display sorted bicycle gear ratio breakdown

(defun shell-sort-numbers (number-list)         ; standard shell sort algorithm
  (let* ((len (length number-list))
         (temp-num))
    (do ((gap (// len 2) (// gap 2))) ((<= gap 0))
      (do ((i gap (1+ i))) ((>= i len))
        (do ((j (- i gap) (- j gap))) ((minusp j))
          (when (> (elt number-list j) (elt number-list (+ j gap)))
            (setf temp-num (elt number-list j))
            (setf (elt number-list j) (elt number-list (+ j gap)))
            (setf (elt number-list (+ j gap)) temp-num)))))
    (return-from shell-sort-numbers number-list)))

(defun gears ()                                 ; main procedure
  (let ((front) (num-front) (rear) (num-rear) (num) (num-gears)
        (diameter) (gear-list) (ratio-list) (pointer-list))
    (format t "~%Enter number of teeth on each front cog in list format:~%")
    (setf front (read))
    (format t "~%Enter number of teeth on each rear cog in list format:~%")
    (setf rear (read))
    (format t "~%Enter the diameter of drive wheel (inches): ")
    (setf diameter (read))
    (setf num-gears (* (setf num-front (length front)) (setf num-rear (length rear))))
    (do ((i 0 (1+ i))) ((>= i num-front))
      (do ((j 0 (1+ j))) ((>= j num-rear))
        (setf num (// (float (elt front i)) (elt rear j)))
        (setf ratio-list (append ratio-list (list num)))
        (setf gear-list (append gear-list
                               (list (list num i j
                                           (// (* num pi diameter) 12.0)))))))
    (setf ratio-list (shell-sort-numbers ratio-list))   ; sort ratios
    (do ((i 0 (1+ i))) ((>= i num-gears))       ; make pointer list
      (setf pointer-list (append pointer-list
             (list (position (elt ratio-list i) gear-list :key 'car :test 'equal)))))
    (format t "~2%Order  Ratio  front  rear   distance (feet)~2%")
    (do ((i 0 (1+ i))) ((>= i num-gears))
      (let ((record (elt gear-list (elt pointer-list i))))
        (format t " ~2d    ~5,2f   ~2d     ~2d    ~7,2f~%" (1+ i) (first record)
                (elt front (second record)) (elt rear (third record)) (fourth record)))))
  (values))
