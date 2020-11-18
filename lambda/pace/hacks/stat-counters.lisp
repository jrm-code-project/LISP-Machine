;;; -*- Mode:LISP; Package:STAT-COUNTERS; Base:8; Readtable:ZL -*-

(defun disable-microsecond-clock ()
  (cond ((not (zerop (ldb si:%%processor-switch-use-stat2-for-usec-clock (si:%processor-switches nil))))
         (si:%processor-switches
           (dpb 0 si:%%processor-switch-use-stat2-for-usec-clock (si:%processor-switches nil)))
         (time:initialize-timebase))))

(defconst %stat-counter-read-main 0)
(defconst %stat-counter-write-main 1)
(defconst %stat-counter-read-aux 2)
(defconst %stat-counter-write-aux 3)
(defconst %stat-counter-read-rg-mode 4)
(defconst %stat-counter-write-control 5)

(defconst %%aux-stat-count-control (byte 3 0))
(defconst %%aux-stat-clock-control (byte 1 4))
(defconst %%main-stat-count-control (byte 3 20.))
(defconst %%main-stat-clock-control (byte 1 24.))

(defconst stat-clock-values '(SM-CLOCK UINST-CLOCK))

(defprop SM-CLOCK 0 stat-clock-select)
(defprop UINST-CLOCK 1 stat-clock-select)

(defconst stat-count-values '(VALID-STATISTICS-BIT
                                 MEMORY-START-NEXT-CYCLE
                                 CSM-STATISTICS-BIT
                                 INCREMENT-LC
                                 T-HOLD
                                 T-STATISTICS-BIT
                                 NOT-CONNECTED
                                 HI))

(defconst aux-stat-count-values '(VALID-STATISTICS-BIT
                                   MEMORY-START-NEXT-CYCLE
                                   CSM-STATISTICS-BIT
                                   INCREMENT-LC
                                   T-HOLD
                                   T-STATISTICS-BIT
                                   1-MHZ-CLOCK
                                   HI))

(defprop VALID-STATISTICS-BIT 0 stat-count-select)
(defprop MEMORY-START-NEXT-CYCLE 1 stat-count-select)
(defprop CSM-STATISTICS-BIT 2 stat-count-select)
(defprop INCREMENT-LC 3 stat-count-select)
(defprop T-HOLD 4 stat-count-select)
(defprop T-STATISTICS-BIT 5 stat-count-select)
(defprop NOT-CONNECTED 6 stat-count-select)
(defprop 1-MHZ-CLOCK 6 stat-count-select)
(defprop HI 7 stat-count-select)

(defsubst read-rg-mode ()
  (logand 37777777777 (si:%stat-counter %stat-counter-read-rg-mode 0)))

(defsubst read-main-stat-counter ()
  (logand 37777777777 (si:%stat-counter %stat-counter-read-main 0)))

(defsubst write-main-stat-counter (val)
  (si:%stat-counter %stat-counter-write-main val))

(defsubst write-main-stat-control (clock count)
  (without-interrupts
    (let ((rg-mode (si:%stat-counter %stat-counter-read-rg-mode 0)))
      (setq rg-mode (dpb clock %%main-stat-clock-control rg-mode))
      (setq rg-mode (dpb count %%main-stat-count-control rg-mode))
      (si:%stat-counter %stat-counter-write-control rg-mode))))

(defsubst read-aux-stat-counter ()
  (logand 37777777777 (si:%stat-counter %stat-counter-read-aux 0)))

(defsubst write-aux-stat-counter (val)
  (si:%stat-counter %stat-counter-write-aux val))

(defsubst write-aux-stat-control (clock count)
  (without-interrupts
    (let ((rg-mode (si:%stat-counter %stat-counter-read-rg-mode 0)))
      (setq rg-mode (dpb clock %%aux-stat-clock-control rg-mode))
      (setq rg-mode (dpb count %%aux-stat-count-control rg-mode))
      (si:%stat-counter %stat-counter-write-control rg-mode))))

(defun describe-stat-counters ()
  (let (rg-mode main aux)
    (without-interrupts
      (setq rg-mode (si:%stat-counter %stat-counter-read-rg-mode 0))
      (setq main (read-main-stat-counter))
      (setq aux (read-aux-stat-counter)))
    (format t "~&Main counts ~25a on ~12a and currently is ~15:d."
            (nth (ldb %%main-stat-count-control rg-mode) stat-count-values)
            (nth (ldb %%main-stat-clock-control rg-mode) stat-clock-values)
            main)
    (format t "~&Aux  counts ~25a on ~12a and currently is ~15:d."
            (nth (ldb %%aux-stat-count-control rg-mode) aux-stat-count-values)
            (nth (ldb %%aux-stat-clock-control rg-mode) stat-clock-values)
            aux)
    (format t "~2&")
    (format t "~&~15:d MAIN - AUX" (- main aux))
    (if (not (zerop aux))
        (format t "~&~15d MAIN // AUX" (// (float main) aux)))
    (if (not (zerop main))
        (format t "~&~15d AUX // MAIN" (// (float aux) main)))
    ))

(defun reset-stat-counters ()
  (disable-microsecond-clock)
  (let* ((rg-mode (si:%stat-counter %stat-counter-read-rg-mode 0))
         (main-stat-clock (ldb %%main-stat-clock-control rg-mode))
         (main-stat-count (ldb %%main-stat-count-control rg-mode))
         (aux-stat-clock (ldb %%aux-stat-clock-control rg-mode))
         (aux-stat-count (ldb %%aux-stat-count-control rg-mode)))
    (micro:reset-and-enable-stat-counters
      main-stat-clock main-stat-count
      aux-stat-clock aux-stat-count)))

(defun print-stat-results (main aux)
  (format t "~2&")
  (format t "~&~15:d MAIN" main)
  (format t "~&~15:d AUX" aux)
  (format t "~&~15:d MAIN - AUX" (- main aux))
  (if (not (zerop aux))
      (format t "~&~15d MAIN // AUX" (// (float main) aux)))
  (if (not (zerop main))
      (format t "~&~15d AUX // MAIN" (// (float aux) main))))

(defun read-and-print-stat-results ()
  (let ((results (micro:stop-and-read-stat-counters)))
    (print-stat-results (car results) (cdr results))))

(defvar main-stat-clock)
(defvar main-stat-count)
(defvar aux-stat-clock)
(defvar aux-stat-count)

(defun make-atom-list-into-menu-alist (atom-list)
  (loop for a in atom-list
        collect (list (string a) a)))

(defun menu-set-stat-counters ()
  (disable-microsecond-clock)
  (let ((rg-mode (si:%stat-counter %stat-counter-read-rg-mode 0)))
    (let ((main-stat-clock (nth (ldb %%main-stat-clock-control rg-mode)
                                stat-clock-values))
          (main-stat-count (nth (ldb %%main-stat-count-control rg-mode)
                                stat-count-values))
          (aux-stat-clock (nth (ldb %%aux-stat-clock-control rg-mode)
                               stat-clock-values))
          (aux-stat-count (nth (ldb %%aux-stat-count-control rg-mode)
                               aux-stat-count-values)))
      (tv:choose-variable-values
        `((main-stat-clock
            "Main stat clock"
            :menu-alist ,(make-atom-list-into-menu-alist stat-clock-values))
          (main-stat-count
            "Main stat count control"
            :menu-alist ,(make-atom-list-into-menu-alist stat-count-values))
          (aux-stat-clock
            "Aux stat clock"
            :menu-alist ,(make-atom-list-into-menu-alist stat-clock-values))
          (aux-stat-count
            "Aux stat count control"
            :menu-alist ,(make-atom-list-into-menu-alist aux-stat-count-values))
          "foo"
          ))
      (micro:reset-and-enable-stat-counters
        (get main-stat-clock 'stat-clock-select)
        (get main-stat-count 'stat-count-select)
        (get aux-stat-clock 'stat-clock-select)
        (get aux-stat-count 'stat-count-select)))))

(defvar stat-sg (make-stack-group "STAT SG"))

(defun stat-function (fun args)
  (without-interrupts
    (set-stat-clocks :main-clock 'uinst-clock
                     :main-count 'valid-statistics-bit
                     :aux-clock 'uinst-clock
                     :aux-count 'hi)
    (reset-stat-counters)
    (apply fun args)
    (micro:stop-and-read-stat-counters)))

(defun set-stat-clocks (&key
                        (main-clock 'uinst-clock)
                        (main-count 'hi)
                        (aux-clock 'uinst-clock)
                        (aux-count 'valid-statistics-bit))
  (if (or (null (get main-clock 'stat-clock-select))
          (null (get main-count 'stat-count-select))
          (null (get aux-clock 'stat-clock-select))
          (null (get aux-count 'stat-count-select)))
      (ferror nil "bad stat control args"))

  (micro:reset-and-enable-stat-counters
    (get main-clock 'stat-clock-select)
    (get main-count 'stat-count-select)
    (get aux-clock 'stat-clock-select)
    (get aux-count 'stat-count-select)))

(defun execute-stat-test (fun args)
  (stack-group-preset stat-sg 'stat-function fun args)
  (funcall stat-sg))

(defun run-stat-test (fun args)
  (let ((results (execute-stat-test fun args)))
    (print-stat-results (car results) (cdr results)))
  t)

(defun write-stat-bits-in-range (from to val)
  (let ((from-adr (micro:i-mem-lookup from))
        (to-adr (micro:i-mem-lookup to)))
    (loop for adr from from-adr below to-adr
          do (micro:write-stat-bit adr val))))

(defun write-function-call-stat-bits (val)
  (write-stat-bits-in-range 'p3adi '(pn-1 2) val)
  (write-stat-bits-in-range 'qical0-fef 'qimove-ignore-fef-0 val))

(defun write-page-fault-stat-bits (val)
  (write-stat-bits-in-range 'pgf-r-sb-save-vma-in-t '(p-r-pf 8) val))

(defun measure-function-call-time (fun args)
  (micro:set-macro-single-step t)
  (write-function-call-stat-bits 1)
  (run-stat-test fun args)
  (write-function-call-stat-bits 0))

(defun count-macro-instructions (fun args)
  (micro:set-macro-single-step t)
  (micro:write-stat-bit 'qmlp 1)
  (run-stat-test fun args)
  (micro:set-macro-single-step nil)
  (micro:write-stat-bit 'qmlp 0)
  t)

(defun count-calls-to-instructions (inst-list fun args)
  (micro:set-macro-single-step t)
  (dolist (i inst-list)
    (micro:write-stat-bit i 1))
  (run-stat-test fun args)
  (micro:set-macro-single-step nil)
  (dolist (i inst-list)
    (micro:write-stat-bit i 0))
  t)

(defun count-calls-to-push-1-minus-arg (fun args)
  (set-stat-clocks)
  (count-calls-to-instructions '(q-one-minus-arg) fun args))

(defun count-calls-to-qmrcl (fun args)
  (set-stat-clocks)
  (count-calls-to-instructions '(qmrcl) fun args))

(defun tak (x y z)
  (if (not (< y x))
      z
    (tak (tak (1- x) y z)
         (tak (1- y) z x)
         (tak (1- z) x y))))

(defun tak-top-level ()
  (tak 18. 12. 6))

(defvar *basic-blocks*)

(defun load-basic-blocks ()
  (declare (special user:*basic-block-info*))
  (load "dj:jrm;basic-blocks.qfasl#>")
  (when (not (= si:%microcode-version-number (car user:*basic-block-info*)))
    (ferror nil "wrong basic block list"))
  (setq *basic-blocks* (cond ((zerop (caadr user:*basic-block-info*))
                              (cadr user:*basic-block-info*))
                             (t
                              (reverse (cadr user:*basic-block-info*)))))
  nil)
