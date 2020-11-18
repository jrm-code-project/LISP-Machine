;;; -*- Mode:LISP; Package:KBUG; Base:10; Readtable:CL -*-


(defun kbug-generic-read-memory (addr)
  (if (k-halted-p)
      (k-read-virtual-memory addr)
    (if (= -1 (kbug-cmd kbug-command-read-memory addr 1.))
	(progn (cerror "Halt Machine"
		       "The machine is running,~@
                        but the debugger does not seem to be responding.~@
                        Type ~c to halt the machine." #\resume)
	       (k-stop)
	       (k-read-virtual-memory addr))
      (kbug-data 0))))

(defun kbug-generic-write-memory (addr data)
  (if (k-halted-p)
      (k-write-virtual-memory addr data)
    (progn
      (kbug-set-data 0 data)
      (if (= -1 (kbug-cmd kbug-command-write-memory addr 1.))
	  (progn (cerror "Halt Machine"
			 "The machine is running,~@
                          but the debugger does not seem to be responding.~@
                          Type ~c to halt the machine." #\resume)
		 (k-stop)
		 (k-write-virtual-memory addr data)))))
  data)


(defun show-memory (addr &optional (how-many 32.))
  (dotimes (i how-many)
    (let ((d (kbug-generic-read-memory (+ addr i))))
      (format t "~&~7,'0x: ~8,'0x  ~c~c~c~c ~40t~7,'0x ~a"
	      (+ addr i) d
	      (ldb (byte 8 0) d)
	      (ldb (byte 8 8) d)
	      (ldb (byte 8 16) d)
	      (ldb (byte 8 24) d)
	      (dpb (ldb (byte 6 20.) d)
		   (byte 6 20.)
		   (ldb (byte 20. 0) d))
	      (let* ((type (ldb (byte 6 26.) d))
		     (type-entry (rassoc type vinc:k-data-type-names-alist :key #'car)))
		(if type-entry (car type-entry) type))))))
  
  
