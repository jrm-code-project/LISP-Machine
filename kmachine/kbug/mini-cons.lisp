;;; -*- Mode:LISP; Package:CONS; Base:10; Readtable:CL -*-

(defafun cons (car cdr)
  ;; Turn off sequence breaks to avoid other processes consing after we
  ;; move the free pointer.
  ;; Check for a region in the cache.
  ;; Write the cdr at one beyond the free pointer.
 try-again
  (alu r+1 gr::*allow-sequence-break* ignore gr::*allow-sequence-break*)
  (move md a1 boxed-md) ; maybe pass cdr in md
  (alu r+1 vma-start-write ignore gr::*cons-cache-free* boxed-vma)

  ;; Bump the free pointer by 2.
  (alu r+2 gr::*cons-cache-free* ignore gr::*cons-cache-free*)

  ;; Scavenge, and check for region end at the end of each cluster.
  (alu-field field-pass nop gr::*cons-cache-free* ignore
	     #.(byte (byte-size %%offset-in-cluster)
		     (- (byte-position %%offset-in-cluster))))


  ;; Write the car, jump to cluster full check.
  (alu r-2 vma ignore gr::*cons-cache-free* br-zero boxed-vma)

  (branch cluster-full (move md-start-write a0 boxed-md))

  ;; Sequence breaks come back on, and we return the cons cell apropriately typed.
  (alu r-1 gr::*allow-sequence-break* ignore gr::*allow-sequence-break*)
  (alu-field aligned-field-pass-left return gr::*dtp-cons* vma %%data-type ch-return next-pc-return)

 cluster-full
  ;; Make the cons cell and call "cons new cluster"
  (alu-field aligned-field-pass-left a2 gr::*dtp-cons* vma %%data-type )

  (open-call (cons-new-cluster 1) r0 (o0 gr::*cons-cache-area*))
  ;; Sequence breaks come back on.
  (alu r-1 gr::*allow-sequence-break* ignore gr::*allow-sequence-break*)
  (return a2))

(defun cons-new-cluster (area)
;  (unless (zerop gr::*scavenge-work-while-consing*)
;    (gc:scavenge-while-consing))
  (when (>= gr::*cons-cache-free* (region-data:region-end gr::*cons-cache-region*))
    (trap::illop "Ran off the end of the region.")))

(defun no-region-in-cache ()
  (trap::illop "The cons cache was empty."))

;;;; CAR and CDR

(eval-when (compile)

USER::(defun li::make-c.n.r (length &optional (suffix "") (guts 'list))
	(if (= length 0)
	    `(PRIMS::DEFUN ,(intern (format nil "C~aR" suffix) (find-package "CONS")) (list)
	       ,guts)
	    `(PRIMS:PROGN ,(li::make-c.n.r (1- length) (concatenate 'string "A" suffix)
				       `(CONS::%CAR ,guts))
			  ,(li::make-c.n.r (1- length) (concatenate 'string "D" suffix)
				       `(CONS::%CDR ,guts)))))

USER::(prims::defmacro LI::def-c...r (number)
  (let ((forms '()))
    (dotimes (i number)
      (push (li::make-c.n.r (1+ i))
		 forms))
    `(PRIMS:PROGN ,@(nreverse forms))))

)

;;; They're all in here:
(li::def-c...r 4)
