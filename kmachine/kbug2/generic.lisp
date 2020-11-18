;;; -*- Mode:LISP; Package:KBUG; Base:10; Readtable:CL -*-

;;;; Generic Interface

;;; Functions which can access memory
;;; under either debugger if the machine is 
;;; running or not

(defmacro def-k-generic (name lambda-list kbug-body kbug2-body)
  `(DEFUN ,name ,lambda-list
     (DO () (())
       (BLOCK .TRY.AGAIN.
	 (IF (K-HALTED-P)
	     (RETURN-FROM ,name ,kbug-body)
	   (RETURN-FROM ,name ,kbug2-body))))))

(defmacro kbug-cmd-cerror (cmd &rest params)
  `(WHEN (= -1 (KBUG-CMD ,cmd . ,params))
     (CERROR "Halt Machine"
	     "The machine is running,~@
              but the debugger does not seem to be responding.~@
              Type ~c to halt the machine." #\resume)
     (K-STOP)
     (RETURN-FROM .TRY.AGAIN.)))
     

(def-k-generic kbug-generic-read-memory (addr)
  (k-read-virtual-memory addr)
  ;---------------------------
  (progn (kbug-cmd-cerror kbug-command-read-memory addr 1.)
	 (kbug-data 0)))

(def-k-generic kbug-generic-write-memory (addr data)
  (progn (k-write-virtual-memory addr data)
	 data)
  ;---------------------------------
  (progn (kbug-set-data 0 data)
         (kbug-cmd-cerror kbug-command-write-memory addr 1.)
	 data))



;;;; Generic HW memory functions

(defun saving-current-k-pc (thunk)
  (let ((current-pc (k-read-spy-pc)))
    (unwind-protect
      (funcall thunk)
      (k-set-pc current-pc))))

(defvar *kbug2-md*)
(defvar *kbug2-vma*)

(def-k-generic hw::read-md ()
  (saving-current-k-pc
    #'(lambda () (r-md)))
  ;----------------------
  *kbug2-md*)


(def-k-generic hw::read-vma ()
  (saving-current-k-pc
    #'(lambda () (r-vma)))
  ;-----------------------
  *kbug2-vma*)

(def-k-generic hw::vma-start-read-vma-boxed-md-boxed (location)
  (saving-current-k-pc
    #'(lambda ()
	(k-read-virtual-memory location)))
  ;------------------------
  (progn (kbug-cmd-cerror kbug-command-read-memory
			  (setq *kbug2-vma* location) 1.)
	 (setq *kbug2-md* (kbug-data 0))))


(defun hw::vma-start-read-vma-unboxed-md-boxed (location)
  (hw::vma-start-read-vma-boxed-md-boxed location))

(def-k-generic hw::read-md ()
  (saving-current-k-pc
    #'(lambda () (r-md)))
  ;----------------------
  *kbug2-md*)





  










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
	      (k-data-type-name d)))))



(defun k-data-type (pointer)
  (hw:ldb pointer vinc:%%data-type 0))

(defun k-data-type-name (pointer)
  (let* ((type (k-data-type pointer))
	 (type-entry (rassoc type vinc:k-data-type-names-alist :key #'car)))
    (if type-entry
	(car type-entry)
      type)))
      
      
(defun read-string (pointer)
  (if (/= (k-data-type pointer) vinc:$$dtp-array)
      (cerror "OK" "This isn't a string, it is a ~s" (k-data-type pointer))) 
  (let ((header (kbug-generic-read-memory pointer)))
    (if (/= (k-data-type header) vinc:$$dtp-array-header-single)
	(cerror "OK" "This isn't an array header, it's a ~s" (k-data-type header)))
    (let* ((length (hw:ldb header array:%%bounds 0))
	   (string (make-string length)))
      (dotimes (i length)
	(setf (svref string i)
	      (int-char (hw:ldb (kbug-generic-read-memory (+ pointer (ash i -2) 1))
				(byte 8 (* 8 (ldb (byte 2 0) i)))
				0))))
      string)))
	

(defun show-symbol (pointer)
  (if (/= (hw:ldb pointer vinc:%%data-type 0)
	  vinc:$$dtp-symbol)
      (cerror "OK" "This isn't a symbol, it is a ~s" (k-data-type pointer)))
  (let ((header (kbug-generic-read-memory pointer)))
    (if (/= (hw:ldb header vinc:%%data-type 0)
	    vinc:$$dtp-symbol-header)
	(cerror "OK" "This isn't a symbol header, it's a ~s" (k-data-type header)))
    (read-string (hw:dpb vinc:$$dtp-array vinc:%%data-type header))))
  


  
  
