;;; -*- Mode:LISP; Package:NETWORK; Readtable:CL; Base:10 -*-

#|

  Copyright LISP Machine, Inc. 1987
   See filename "Copyright.Text" for
  licensing and release information.

|#

(export '( make-fifo
           fifo-empty-p
           fifo-as-list
           last-element-in-fifo
           push-fifo
           pushnew-fifo
           push-fifo-front
           pop-fifo
           rotate-fifo
           remove-from-fifo
           move-to-end-of-fifo
           priority-insert-fifo
           make-key-list
           original-array
           nth-value
           byte-array-or-string-p
           milliseconds-since-midnight-gmt
           find-network-address-for-other-processor
           set-gauges
           add-gauges
           delete-gauges
           with-tcp-favored
           with-chaos-favored
           ))

(defsubst make-fifo ()
  "Make a FIFO queue"
  (cons nil nil))

(defsubst fifo-empty-p (f)
  (null (car f)))

(defsubst fifo-as-list (f)
  (car f))

(defsubst last-element-in-fifo (f)
  (cadr f))

(defun push-fifo (elem f)
  "Push an element onto the tail of a FIFO queue"
  (without-interrupts
    (let ((new (ncons elem))
          (tail (cdr f)))
      (if tail
          (setf (cdr tail) new)
        (setf (car f) new))
      (setf (cdr f) new)))
  elem)

(defun pushnew-fifo (elem f)
  "Add an element to the end of a FIFO queue unless it already appears in the queue"
  (without-interrupts
    (do ((this (car f) (cdr this)))
        ((null this)
         ;;If element not found, simply add to end
         (let ((new (ncons elem))
               (tail (cdr f)))
           (if tail
               (setf (cdr tail) new)
             (setf (car f) new))
           (setf (cdr f) new))
         elem)
      (when (eql elem (car this))
        (return elem)))))

(defun push-fifo-front (elem f)
  "Push an element onto the front of a FIFO queue"
  (without-interrupts
    (let ((new (ncons elem)))
      (setf (car f) (nconc new (car f)))
      (unless (cdr f)
        (setf (cdr f) new))))
  elem)

(defun pop-fifo (f)
  "Remove the first element from a FIFO queue and return it"
  (without-interrupts
    (let ((l (car f)))
      (prog1 (car l)
             (or (setf (car f) (cdr l))
                 (setf (cdr f) nil))))))

(defun rotate-fifo (f)
  "Like (push-fifo (pop-fifo f) f)"
  (without-interrupts
    (let ((new (car f)))
      (if (setf (car f) (cdr new))
          (setf (cdr new) nil)
        (setf (cdr f) nil))
      (if (cdr f)
          (setf (cddr f) new)
        (setf (car f) new))
      (setf (cdr f) new)))
  (car f))

(defun remove-from-fifo (elem f)
  "Remove all copies of a specific element from a FIFO queue"
  (without-interrupts
    (do ((this (car f) (cdr this))
         (prev nil this))
        ((null this))
      (when (eql elem (car this))
        (if prev
            (setf (cdr prev) (cdr this))
          (setf (car f) (cdr this)))
        (if (eql elem (cadr f))
            (setf (cdr f) prev)))))
  elem)

(defun move-to-end-of-fifo (elem f)
  "Move the first copy of a specific element to the end of a FIFO queue"
  (without-interrupts
    (do ((this (car f) (cdr this))
         (prev nil this))
        ((null this)
         ;;If element not found, simply add to end
         (let ((new (ncons elem))
               (tail (cdr f)))
           (if tail
               (setf (cdr tail) new)
             (setf (car f) new))
           (setf (cdr f) new))
         elem)
      (when (eql elem (car this))
        (unless (null (cdr this))
          ;;Remove element from fifo
          (if prev
              (setf (cdr prev) (cdr this))
            (setf (car f) (cdr this)))
          (if (eql elem (cadr f))
              (setf (cdr f) prev))
          ;;Stick on end of fifo
          (setf (cdr this) nil)
          (if (cdr f)
              (setf (cddr f) this)
            (setf (car f) this))
          (setf (cdr f) this))
        (return elem)))))

(defun priority-insert-fifo (elem func f)
  "Inserts an element into a fifo at the end of like-priority items, before lower-priority items.
FUNC is a function that returns a number rating the priority of an element"
  (without-interrupts
    (do ((this (car f) (cdr this))
         (prev nil this)
         (priority (funcall func elem))
         (new (ncons elem)))
        ((null this)                            ;If lower-priority element not found, add to end
         (if (cdr f)
             (setf (cddr f) new)
           (setf (car f) new))
         (setf (cdr f) new)
         elem)
      (when (> priority (funcall func (car this)))
        (cond (prev                             ;Adding between two elements
               (setf (cdr new) this)
               (setf (cdr prev) new))
              (t                                ;Adding at front
               (setf (cdr new) (car f))
               (setf (car f) new)))
        (return elem)))))

(defun nrotate (list)
  "Given a list, destructively rotate the elements towards the front.  I.e. the first
element becomes the last, the second the first, the third the second, etc."
  (do ((save (car list))
       (rest list (cdr rest)))
      ((null (cdr rest))
       (rplaca rest save)
       list)
    (rplaca rest (cadr rest))))

;;;utility routine: set up (check-type key (member ...)) by doing (check-type key #.(make-key-list alist))
(defun make-key-list (alist &aux result)
  (push 'member result)
  (dolist (elt alist)
    (push (car elt) result))
  (nreverse result))

(defun original-array (array)
  "Follows array displacements to find the original array"
  (do ()
      ((not (si:array-indirect-p array)) array)
    (setq array (si:array-indirect-to array))))

(defmacro nth-value (n exp)
  `(nth ,n (multiple-value-list ,exp)))

(defun byte-array-or-string-p (array)
  "Returns T if the argument is a string or an art-8b vector"
  #-lambda (typep array '(or (vector (unsigned-byte 8)) string))
  #+lambda (and (eq (sys:%data-type array) sys:dtp-array-pointer)
                (= (array-rank array) 1)
                (let ((type (nth (sys:%p-ldb-offset sys:%%array-type-field array 0) sys:array-types)))
                  (or (eq type 'sys:art-8b) (eq type 'sys:art-string))))
  )

(defun milliseconds-since-midnight-gmt ()
  "Number of milliseconds since midnight GMT; suitable for timestamp option"
  (multiple-value-bind
    (seconds minutes hours ignore ignore ignore ignore dst-p time-zone)
      (get-decoded-time)
    (* 1000
       (+ seconds
          (* 60 minutes)
          (* 3600 (rem (+ hours
                          time-zone
                          (if dst-p 1 0))
                       24))))))

(defun find-host-for-other-processor (op)
  (let* ((host-name (nth (si:op-proc-number op) (multiple-value-list (si:get-pack-name))))
         (host (si:parse-host host-name t nil)))
    host))

(defun find-network-address-for-other-processor (op domain)
  (let ((host (find-host-for-other-processor op)))
    (cond (host
           (send host :network-address domain))
          ((eq domain :chaos)
           ;;***Kludge -- system either isn't in pack name or isn't in site files.
           ;;***Get the chaos address the old way
           (let* ((conf (si:op-proc-conf op))
                  (address (and conf (si:%processor-conf-chaos-address conf))))
             (cond ((null address)              ;No configuration structure or no address
                    nil)
                   ((zerop address)             ;Bad address -- not set up yet
                    nil)
                   (t                           ;Good address
                    address))))
          (t nil))))

;;;NETWORK CLOCK FUNCTION
;;;This function is called by the scheduler every N 60ths of a second.  It maintains statistics for all
;;;network-statistics blocks on the list net:*network-statistics-blocks*

(defparameter *averaging-constant* 0.95s0)
(defparameter *minimum-60ths* 60)
(defvar *accumulated-60ths* 0)
(defvar *network-clock-enabled* t)

(defun network-clock (n)
  (when *network-clock-enabled*
    (when (>= (incf *accumulated-60ths* n) *minimum-60ths*)
      (without-floating-underflow-traps
        (let* ((alpha *averaging-constant*)
               (1-alpha (- 1s0 alpha))
               (interval (short-float *accumulated-60ths*)))
          (dolist (array *network-statistics-blocks*)
            (dotimes (stat 4)
              (let* ((current (aref array stat STAT-CURR))
                     (last (aref array stat STAT-LAST))
                     (instant (if (= current last)
                                  0s0
                                (/ (short-float (- current (aref array stat STAT-LAST)))
                                   (/ interval 60s0)))))
                (setf (aref array stat STAT-INST) instant)
                (setf (aref array stat STAT-AVRG)
                      (+ (* alpha (aref array stat STAT-AVRG))
                         (* 1-alpha instant)))
                (if (> instant (aref array stat STAT-MAX))
                    (setf (aref array stat STAT-MAX) instant))
                (setf (aref array stat STAT-LAST) current)))))
        (setf *accumulated-60ths* 0)))))

(defun time-network-clock (n)
  (let* ((block (make-statistics-block))
         (net:*network-statistics-blocks* (ncons block))
         (net:*accumulated-60ths* 0))
    (declare (special net:*network-statistics-blocks*
                      net:*accumulated-60ths*))
    (dotimes (i n)
      (dotimes (stat 4)
        (incf (aref block stat STAT-CURR)))
      (network-clock *minimum-60ths*))
    block))

;;;Network Gauge functions

(defun pps->gauge (pps array stat bytes?)
  "Convert a Packets-Per-Second figure in range of 0 - RANGE into range -1 to 1
Auto-scaling -- statistics array maximum value used to determine RANGE"
  (without-floating-underflow-traps
    (let* ((max (aref array stat STAT-MAX))
           (limit (if (zerop max)
                      1.0
                    (/ (if bytes? (/ (* 8.0 max) 1000.0) max) 2.0))))
      (min 1.0
           (max -1.0
                (- (/ pps limit) 1.0))))))

(defun pps-in-label (last-value value keyword)
  (declare (values new-label changed?))
  (if (= value last-value)
      (values nil nil)
    (values (format nil "~A:~D" keyword value) t)))

(defun make-network-gauge (array keyword name inactive-gauges)
  (declare (special tv:*control-panel*))
  (let ((g (assoc keyword (cdr inactive-gauges))))
    (when g
      (setf (cdr inactive-gauges) (delete g (cdr inactive-gauges)))
      (return-from make-network-gauge (cdr g))))
  (let* ((stat (case keyword
                 ((:ipr :apr) STAT-PR)
                 ((:ips :aps) STAT-PS)
                 ((:ibr :abr) STAT-BR)
                 ((:ibs :abs) STAT-BS)))
         (inst-or-avrg (case keyword
                         ((:ipr :ips :ibr :ibs) STAT-INST)
                         ((:apr :aps :abr :abs) STAT-AVRG)))
         (bytes? (member keyword '(:ibr :abr :ibs :abs))))
    (make-instance 'tv:probe-map-gauge-with-value-and-name
                   :superior         tv:*control-panel*
                   :mapping-function #'(lambda (pps)
                                         (pps->gauge pps array stat bytes?))
                   :label            `(:string ,(format nil "~A:0" keyword))
                   :label-function   (let ((last-value 0))
                                       #'(lambda (value)
                                           (pps-in-label (prog1 last-value (setq last-value value))
                                                         value keyword)))
                   :margin-name      name
                   :probe-function   (if bytes?
                                         #'(lambda ()
                                             (round (/ (* 8.0 (aref array stat inst-or-avrg)) 1000.0)))
                                       #'(lambda ()
                                           (round (aref array stat inst-or-avrg)))))))

(defun set-gauges (array active-gauges inactive-gauges gauge-name gauge-list)
  (declare (special tv:fancy-landscape))
  (when (and (boundp 'tv:fancy-landscape) tv:fancy-landscape)
    (let* ((current-gauges (mapcar 'car (cdr active-gauges)))
           (new-gauges (set-difference gauge-list current-gauges))
           (killed-gauges (set-difference current-gauges gauge-list))
           (windows nil))
      (dolist (keyword killed-gauges)
        (let ((g (assoc keyword (cdr active-gauges))))
          (push g (cdr inactive-gauges))
          (push (cdr g) windows)
          (setf (cdr active-gauges) (delete g (cdr active-gauges)))))
      (apply 'tv:delete-network-gauges windows)
      (setq windows nil)
      (dolist (keyword new-gauges)
        (let ((g (make-network-gauge array keyword gauge-name inactive-gauges)))
          (push (cons keyword g) (cdr active-gauges))
          (push g windows)))
      (apply 'tv:add-network-gauges windows)
      t)))

(defun add-gauges (array active-gauges inactive-gauges gauge-name gauge-list)
  (declare (special tv:fancy-landscape))
  (when (and (boundp 'tv:fancy-landscape) tv:fancy-landscape)
    (let* ((current-gauges (mapcar 'car (cdr active-gauges)))
           (new-gauges (set-difference gauge-list current-gauges))
           (windows nil))
      (dolist (keyword new-gauges)
        (let ((g (make-network-gauge array keyword gauge-name inactive-gauges)))
          (push (cons keyword g) (cdr active-gauges))
          (push g windows)))
      (apply 'tv:add-network-gauges windows)
      t)))

(defun delete-gauges (active-gauges inactive-gauges gauge-list)
  (declare (special tv:fancy-landscape))
  (when (and (boundp 'tv:fancy-landscape) tv:fancy-landscape)
    (let* ((current-gauges (mapcar 'car (cdr active-gauges)))
           (killed-gauges (intersection gauge-list current-gauges))
           (windows nil))
      (dolist (keyword killed-gauges)
        (let ((g (assoc keyword (cdr active-gauges))))
          (push g (cdr inactive-gauges))
          (push (cdr g) windows)
          (setf (cdr active-gauges) (delete g (cdr active-gauges)))))
      (apply 'tv:delete-network-gauges windows))))

(defmacro with-tcp-favored (&body body)
  `(let ((*network-protocols* (cons :internet (remove :internet *network-protocols*))))
     ,@body))

(defmacro with-chaos-favored (&body body)
  `(let ((*network-protocols* (cons :chaos (remove :chaos *network-protocols*))))
     ,@body))
