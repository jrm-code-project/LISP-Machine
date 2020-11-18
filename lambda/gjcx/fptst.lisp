;;; -*- Mode:LISP; Package:USER; Readtable:CL; Base:10 -*-

;;; to produce the same results as fptst.c

(defmacro time-operation (loops &body operations)
  "returns time in milliseconds for doing operation loops times"
  `(let ((time (get-internal-real-time)))
     (dotimes (j ,loops)
       ,@operations)
     (* 1000 (/ (- (get-internal-real-time) time) (float internal-time-units-per-second)))))


;; + - * / log exp sqrt expt sin cos tan

(defun fptst (&key (value 127.125) (value2 2.77777) (loop 1000) (nloop 10000))
  (let (ttime result +time -time *time /time logtime exptime sqrttime expttime sintime costime tantime nlooptime)
    (setq ttime (time-operation 1
                  (setq +time (time-operation loop (setq result (+ value value2))))
                  (setq -time (time-operation loop (setq result (- value value2))))
                  (setq *time (time-operation loop (setq result (* value value2))))
                  (setq /time (time-operation loop (setq result (/ value value2))))
                  (setq logtime (time-operation loop (setq result (log value))))
                  (setq exptime (time-operation loop (setq result (exp value))))
                  (setq sqrttime (time-operation loop (setq result (sqrt value))))
                  (setq expttime (time-operation loop (setq result (expt value value2))))
                  (setq sintime (time-operation loop (setq result (sin value))))
                  (setq costime (time-operation loop (setq result (cos value))))
                  (setq tantime (time-operation loop (setq result (tan value))))
                  (setq nlooptime (time-operation nloop (setq result value)))))
    ;; normalize
    (format t "~&Entire test took ~5$ seconds.~%" (/ ttime 1000))
    (format t "Times in milliseconds per loop, minus ~5$ loop overhead~%"
            (/ nlooptime nloop))
    (format t "Add: ~5$ Sub: ~5$ Mult: ~5$ Div: ~5$~%"
            (- (/ +time loop) (/ nlooptime nloop))
            (- (/ -time loop) (/ nlooptime nloop))
            (- (/ *time loop) (/ nlooptime nloop))
            (- (/ /time loop) (/ nlooptime nloop)))
    (format t "log: ~5$ exp: ~5$ sqrt: ~5$ expt: ~5$~%"
            (- (/ logtime loop) (/ nlooptime nloop))
            (- (/ exptime loop) (/ nlooptime nloop))
            (- (/ sqrttime loop) (/ nlooptime nloop))
            (- (/ expttime loop) (/ nlooptime nloop)))
    (format t "sin: ~5$ cos: ~5$ tan: ~5$~%"
            (- (/ sintime loop) (/ nlooptime nloop))
            (- (/ costime loop) (/ nlooptime nloop))
            (- (/ tantime loop) (/ nlooptime nloop)))))
