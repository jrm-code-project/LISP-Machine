;;; -*- Mode:LISP; Package:USER; Readtable:CL; Base:10 -*-


(defun make-garbage (mega-words &optional (sleep-amount 0.10))
  "Returns total realtime minus sleeping time"
  (let ((realtime)
        (mega-word/100 (floor (expt 2 20) 100))
        (loops (round (* mega-words 100)))
        (sleep-time 0))
    (format t "~&; Going to make ~D mega-word~p of garbage, in ~D loops, sleeping about ~S seconds."
            mega-words mega-words loops
            (* loops sleep-amount))
    (setq realtime (get-internal-real-time))
    (dotimes (j loops)
      ;; make 1/100'th of a megaword of potential garbage, assuming that the overead word for
      ;; an array or negligable.
      (make-array mega-word/100)
      (let ((off (/ (float sleep-amount) 6))
            (x (get-internal-real-time)))
        (sleep (+ sleep-amount (- off) (random (* 2 off))))
        (incf sleep-time (- (get-internal-real-time) x))))
    (setq realtime (- (get-internal-real-time) realtime))
    (values (/ (float (- realtime sleep-time)) internal-time-units-per-second)
            (/ (float sleep-time) internal-time-units-per-second))))


#+LISPM
(defun batch-process (expression)
  (let ((l (list :expression expression
                 :values nil
                 :output (make-string 250 :fill-pointer 0))))
    (process-run-function '(:name "batch process" :priority -1)
                          (closure '(*package* *readtable*)
                                   #'(lambda (l)
                                       (with-output-to-string (standard-output (getf l :output))
                                         (format t ";; Batch Evaluating:")
                                         (pprint (getf l :expression))
                                         (format t "~%;; =>~%")
                                         (setf (getf l :values) (multiple-value-list (eval (getf l :expression))))
                                         (format t "~&~S~%" (car (getf l :values))))))

                          l)
    l))

#+lispm
(defun make-garbage-batch (total-mega-words &optional (processes 10) (sleep-amount 0.3))
  (do ((j 0 (1+ j))
       (l nil (cons (batch-process `(make-garbage  ,(/ total-mega-words processes) ,sleep-amount))
                    l)))
      ((= j processes) l)))


#+lispm
(defun result-garbage-batch (list)
  "Wait for a batch of garbage making processes to complete and describe the result"
  (let ((looking list))
    (process-wait "crunching"
                  #'(lambda ()
                      (cond ((null looking)
                             t)
                            ((not (getf (car looking) :values))
                             nil)
                            ('else
                             (null (setq looking (cdr looking))))))))
  (result-garbage-batch-report list)
  list)

#+lispm
(defun result-garbage-batch-report (list)
  (format t "~&Summary for ~D processes, each consing ~D megawords, ~D megawords total.~%"
          (length list)
          (cadr (getf (car list) :expression))
          (* (cadr (getf (car list) :expression)) (length list)))
  (result-garbage-report (mapcar #'(lambda (x) (cadr (getf x :values))) list)
                         (mapcar #'(lambda (x) (car (getf x :values))) list)))


(defun result-garbage-report (sleep-times elapsed-times)
  (let ((sleep-stats (statistics sleep-times))
        (elapsed-stats (statistics elapsed-times)))
      (format t "Elapsed (non-sleep) time:~%Average: ~S~% Median: ~S~%Minimum: ~S~%Maximum: ~S~%Sum: ~S~%"
              (getf elapsed-stats :average)
              (getf elapsed-stats :median)
              (getf elapsed-stats :min)
              (getf elapsed-stats :max)
              (getf elapsed-stats :sum))
      (format t "Sleep time:~%Average: ~S~% Median: ~S~%Minimum: ~S~%Maximum: ~S~%Sum: ~S~%"
              (getf sleep-stats :average)
              (getf sleep-stats :median)
              (getf sleep-stats :min)
              (getf sleep-stats :max)
              (getf sleep-stats :sum))))



(defun statistics (list)
  (let ((sum (apply #'+ list))
        (sorted (sort (copy-list list) #'<))
        (length (length list)))
    (cond ((= length 0)
           ())
          ('else
           (list :sum sum
                 :average (/ sum length)
                 :median (/ (+ (nth (floor length 2) sorted)
                               (nth (floor (1- length) 2) sorted))
                            2)
                 :min (car sorted)
                 :max (car (last sorted)))))))
