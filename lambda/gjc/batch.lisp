;;; -*- Mode:LISP; Package:USER; Fonts:(CPTFONTB); Base:10 -*-


(defflavor window-with-process
           ()
           (tv:notification-mixin tv:process-mixin tv:window))


(DEFFLAVOR Batch-Control-Frame
           ((pending-jobs-cell (list nil))
            (workers-available-cell (list nil)))
           (TV:BORDERED-CONSTRAINT-FRAME)
  (:DEFAULT-INIT-PLIST
    :PANES
  '((BATCH-JOB-2 WINDOW-WITH-PROCESS
                 :BLINKER-DESELECTED-VISIBILITY :ON
                 :BLINKER-FLAVOR TV:RECTANGULAR-BLINKER
                 :BLINKER-P T
                 :DEEXPOSED-TYPEIN-ACTION :NORMAL
                 :DEEXPOSED-TYPEOUT-ACTION :NORMAL
                 :LABEL  NIL
                 :SAVE-BITS T
                 :process '(wait-for-a-job)
                 )
    (BATCH-JOB-1 WINDOW-WITH-PROCESS
                 :BLINKER-DESELECTED-VISIBILITY
                 :ON
                 :BLINKER-FLAVOR
                 TV:RECTANGULAR-BLINKER
                 :BLINKER-P
                 T
                 :DEEXPOSED-TYPEIN-ACTION
                 :NORMAL
                 :DEEXPOSED-TYPEOUT-ACTION
                 :NORMAL
                 :LABEL
                 NIL
                 :SAVE-BITS T
                 :process '(wait-for-a-job))
    (BATCH-CONTROL WINDOW-WITH-PROCESS
                   :BLINKER-DESELECTED-VISIBILITY
                   :ON
                   :BLINKER-FLAVOR
                   TV:RECTANGULAR-BLINKER
                   :BLINKER-P
                   T
                   :DEEXPOSED-TYPEIN-ACTION
                   :NORMAL
                   :DEEXPOSED-TYPEOUT-ACTION
                   :PERMIT
                   :LABEL
                   NIL
                   :SAVE-BITS
                   T
                   :process '(batch-job-control))
    (BATCH-JOB-5 WINDOW-WITH-PROCESS
                 :BLINKER-DESELECTED-VISIBILITY
                 :ON
                 :BLINKER-FLAVOR
                 TV:RECTANGULAR-BLINKER
                 :BLINKER-P
                 T
                 :DEEXPOSED-TYPEIN-ACTION
                 :NORMAL
                 :DEEXPOSED-TYPEOUT-ACTION
                 :NORMAL
                 :LABEL
                 NIL
                 :SAVE-BITS
                 T
                 :process '(wait-for-a-job))
    (BATCH-JOB-4 WINDOW-WITH-PROCESS
                 :BLINKER-DESELECTED-VISIBILITY
                 :ON
                 :BLINKER-FLAVOR
                 TV:RECTANGULAR-BLINKER
                 :BLINKER-P
                 T
                 :DEEXPOSED-TYPEIN-ACTION
                 :NORMAL
                 :DEEXPOSED-TYPEOUT-ACTION
                 :NORMAL
                 :LABEL
                 NIL
                 :SAVE-BITS
                 T
                 :process '(wait-for-a-job))
    (BATCH-JOB-3 WINDOW-WITH-PROCESS
                 :BLINKER-DESELECTED-VISIBILITY
                 :ON
                 :BLINKER-FLAVOR
                 TV:RECTANGULAR-BLINKER
                 :BLINKER-P
                 T
                 :DEEXPOSED-TYPEIN-ACTION
                 :NORMAL
                 :DEEXPOSED-TYPEOUT-ACTION
                 :NORMAL
                 :LABEL
                 NIL
                 :SAVE-BITS
                 T
                 :process '(wait-for-a-job)))
  :CONSTRAINTS
  (QUOTE
   ((NIL
     (WINDOW-MAKER:WHOLE)
     ((WINDOW-MAKER:WHOLE
       :HORIZONTAL
       (:EVEN)
       (DUMMY-NAME5 DUMMY-NAME6)
       ((DUMMY-NAME5 :VERTICAL
                     (:EVEN)
                     (BATCH-CONTROL BATCH-JOB-1 BATCH-JOB-2)
                     ((BATCH-CONTROL 0.335766s0) (BATCH-JOB-1 0.335766s0))
                     ((BATCH-JOB-2 :EVEN)))
        (DUMMY-NAME6 :VERTICAL
                     (:EVEN)
                     (BATCH-JOB-3 BATCH-JOB-4 BATCH-JOB-5)
                     ((BATCH-JOB-3 0.331387s0) (BATCH-JOB-4 0.337227s0))
                     ((BATCH-JOB-5 :EVEN))))))))))
 :GETTABLE-INSTANCE-VARIABLES
 :SETTABLE-INSTANCE-VARIABLES
 :INITTABLE-INSTANCE-VARIABLES)


(DEFMETHOD (WINDOW-MAKER:MY-FLAVOR :AFTER :INIT) (&REST IGNORE)
  (FUNCALL-SELF :SET-SELECTION-SUBSTITUTE (FUNCALL-SELF :GET-PANE 'BATCH-CONTROL)))


(defmethod (Batch-Control-Frame :submit) (job)
  (let ((l (ncons job)))
    (without-interrupts
      (setf (car pending-jobs-cell) (nconc (car pending-jobs-cell) l)))))

(defun wait-for-a-job (terminal-io)
  (let ((l (send (send terminal-io :superior) :workers-available-cell)))
    (without-interrupts
      (or (mem #'memq current-process (car l))
          (push (list t current-process terminal-io) (car l)))))
  (process-wait "initial wait for a job" #'false))

(defun process-a-job (terminal-io job cell)
  (unwind-protect
      (progn (format t "~&Processing...")
             (funcall job)
             (format t "done~%"))
    (rplaca cell t)))

(defun batch-job-control (terminal-io)
  (let ((pending-jobs-cell (send (send terminal-io :superior) :pending-jobs-cell))
        (workers-available-cell (send (send terminal-io :superior) :workers-available-cell)))
    (do-forever
      (process-wait "wait for a job" #'car pending-jobs-cell)
      (let ((job (with-interrupts (pop (car pending-jobs-cell)))))
        (format t "~&Got a job to run: ~S" job)
        (process-wait "for worker"
                      #'(lambda (cell)
                          (assq t (car cell)))
                      workers-available-cell)
        (let ((worker (assq t workers-available-cell)))
          (rplaca worker nil)
          (process-preset (cadr worker)
                          #'process-a-job
                          (caddr worker)
                          job
                          worker)
          (process-enable (cadr worker)))))))



(compile-flavor-methods window-with-process
                        Batch-Control-Frame)
