;;; -*- Mode:LISP; Package:K-KBUG; Base:10; Readtable:CL; compile-in-roots:("K-GLOBAL") -*-

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

(defvar *kbug-cmd-cerror-message* "The machine is running,~@
              				but the debugger does not seem to be responding.~@
              				Type ~c to halt the machine.")

(defmacro kbug-cmd-cerror (cmd &rest params)
  `(WHEN (= -1 (KBUG-CMD ,cmd . ,params))
     (CERROR "Halt Machine"
	     *kbug-cmd-cerror-message* #\resume)
     (K-STOP)
     (RETURN-FROM .TRY.AGAIN.)))
     

(def-k-generic kbug-generic-read-memory (addr)
  (k-read-virtual-memory addr)
  ;---------------------------
  (progn (let ((*kbug-cmd-cerror-message* "You have probably read memory who's quantum is not in any region,~@
						The machine is looping in error, running,~@
              					but the debugger does not seem to be responding.~@
              					If this is true do NOT Type ~c to halt the machine."))
	   (kbug-cmd-cerror k2:kbug-command-read-memory addr 1.))
	 (kbug-data 0)))

(def-k-generic kbug-generic-write-memory (addr data &optional inhibit-read-only)
  (progn (k-write-virtual-memory addr data inhibit-read-only)
	 data)
  ;---------------------------------
  (progn (kbug-set-data 0 data)
         (kbug-cmd-cerror k2:kbug-command-write-memory addr 1.)	;should hack inhibit-read-only?
	 data))


(def-k-generic kbug-generic-read-instruction (doubleword-address)
  (lam:read-inst-physical-with-offset doubleword-address)	;from physical memory offset of *code-start*
  (kbug-read-inst doubleword-address))	;this reads virtual memory and supplies the PC-space bit.


;;;; Generic HW memory functions


(defvar *kbug2-md*)
(defvar *kbug2-vma*)

(def-k-generic hw::read-md ()
  (r-md)
 ;(saving-current-k-pc
 ;  #'(lambda () (r-md)))
  ;----------------------
  *kbug2-md*)


(def-k-generic hw::read-vma ()
  (r-vma)
 ;(saving-current-k-pc
 ;  #'(lambda () (r-vma)))
  ;-----------------------
  *kbug2-vma*)

(def-k-generic hw::vma-start-read-vma-boxed-md-boxed (location)
  (k-read-virtual-memory location)
;  (saving-current-k-pc
;    #'(lambda ()
;	(k-read-virtual-memory location)))
  ;------------------------
  (progn (kbug-cmd-cerror k2:kbug-command-read-memory
			  (setq *kbug2-vma* location) 1.)
	 (setq *kbug2-md* (kbug-data 0))))


(def-k-generic kbug-read-map-cluster (cluster)
  (k-read-memory-map cluster)			;was (map::read-map cluster)  <17-Nov-88 rg>
  (kbug2-read-map-cluster cluster))

(defun hw::vma-start-read-vma-unboxed-md-boxed (location)
  (hw::vma-start-read-vma-boxed-md-boxed location))

(defun hw::vma-start-read-no-transport (location boxed-vma boxed-md)
  boxed-vma boxed-md
  (hw:vma-start-read-vma-boxed-md-boxed location))


(defun show-memory (addr &optional (how-many 32.))
  (cond ((not (numberp addr))
	 (setq addr (kbug2-get-starting-address addr))))
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

(defun show-physical-memory (addr &optional (how-many 32.))
  (cond ((not (numberp addr))
	 (format t "~%Remember, this is physical memory")	;guy is probaby losing.
	 (setq addr (kbug2-get-starting-address addr))))
  (dotimes (i how-many)
    (let ((d (lam:k-mem-read-word-address (+ addr i))))
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

(defun show-instruction-memory-as-data (pc &optional (how-much #x100))
  (dotimes (i how-much)
    (let ((d0 (kbug-generic-read-memory (logior #X2000000 (ash (+ i pc) 1.))))
	  (d1 (kbug-generic-read-memory (logior #x2000001 (ash (+ i pc) 1.)))))
      (format t "~&lh:~7,'0x: ~8,'0x  ~c~c~c~c ~40t~7,'0x ~a"
	    (+ pc i) d0
	    (ldb (byte 8 0) d0)
	    (ldb (byte 8 8) d0)
	    (ldb (byte 8 16) d0)
	    (ldb (byte 8 24) d0)
	    (dpb (ldb (byte 6 20.) d0)
		 (byte 6 20.)
		 (ldb (byte 20. 0) d0))
	    (k-data-type-name d0))
      (format t "~&hh:~7,'0x: ~8,'0x  ~c~c~c~c ~40t~7,'0x ~a"
	    (+ pc i) d1
	    (ldb (byte 8 0) d1)
	    (ldb (byte 8 8) d1)
	    (ldb (byte 8 16) d1)
	    (ldb (byte 8 24) d1)
	    (dpb (ldb (byte 6 20.) d1)
		 (byte 6 20.)
		 (ldb (byte 20. 0) d1))
	    (k-data-type-name d1)))))

(defun show-stuff (addr count)
  (do ((i 1 (+ 3 i)))
      ((>= i count))
    (let ((csp (kbug-generic-read-memory (+ addr i 0)))
	  (oar (kbug-generic-read-memory (+ addr i 1)))
	  (type (kbug-generic-read-memory (+ addr i 2))))
      (format t "~&~3d:  o:~2x  a:~2x  r:~2x  ~a"
	      (ldb (byte 8 0) csp)
	      (ldb (byte 8 16) oar)
	      (ldb (byte 8 8) oar)
	      (ldb (byte 8 0) oar)
	      (case (ldb (byte 4 0) type)
		(1 'open-call)
		(2 'open)
		(3 'open-call-topen)
		(t (ldb (byte 4 0) type)))))))

(defun show-current-control-pdl ()
  (let ((cp (read-global-register 'gr:*control-pdl*))
	(cpp (read-global-register 'gr:*control-pdl-pointer*))
	(cpl (read-global-register 'gr:*control-pdl-limit*)))
    (format t "pdl: #x~x    pointer: #x~x    limit: #x~x" cp cpp cpl)
    (when (> (ldb (byte 24 0) cpp) (ldb (byte 24 0) cpl))	;fudge fudge
      (format t "Pointer beyond limit in current control pdl"))
    (show-control-pdl (ldb (byte 24 0) cp)
		      (ceiling (- (ldb (byte 24 0) cpp)
				  (ldb (byte 24 0) cp) 3) 18.))))		      

(defun show-control-pdl (address &optional frame-count)
  (unless (= (ldb (byte 5. 21.) (kbug-generic-read-memory address))
	     28.)				;ART-CONTROL-PDL
    (error nil "not a control-pdl"))
  (let ((stack-group (+ address 2))
	(pointer (kbug-generic-read-memory (+ address 2))))
    (format t "~&Control pdl: #x~x   Stack group: #x~x    pointer: #x~x" address stack-group pointer)
    (do* ((addr (+ address 3) (+ 18 addr))
	  (first-word (kbug-generic-read-memory addr)
		      (kbug-generic-read-memory addr))
	  (pc (kbug-generic-read-memory (1+ addr))
	      (kbug-generic-read-memory (1+ addr)))
	  (box-bits (ldb (byte 16 16) first-word)
		    (ldb (byte 16 16) first-word))
	  (i 0 (1+ i)))
	 ((if frame-count
	      (>= i frame-count)
	    (>= addr (ldb (byte 24 0) pointer))))
      (macrolet ((bit (n) `(ldb (byte 1 ,n) box-bits))
		 (reg (n) `(kbug-generic-read-memory (+ addr 2 ,n))))
	(format t "~&~[Open    ~;OpenCall~;T-Open  ~;P-O-C   ~] ~8x ~4x ~6x ~~@{~^~<~%~1:; ~[ ~;*~]~8,48x~;~>~}~"
		(ldb (byte 2 0) first-word)	;type code
		pc
		(ldb (byte 7 2) first-word)	;return destination
		(ldb (byte 4 9) first-word)	;global frame
		(bit 0)  (reg 0)   (bit 1)  (reg 1)
		(bit 2)  (reg 2)   (bit 3)  (reg 3)
		(bit 4)  (reg 4)   (bit 5)  (reg 5)
		(bit 6)  (reg 6)   (bit 7)  (reg 7)
		(bit 8)  (reg 8)   (bit 9)  (reg 9)
		(bit 10) (reg 10)  (bit 11) (reg 11)
		(bit 12) (reg 12)  (bit 13) (reg 13)
		(bit 14) (reg 14)  (bit 15) (reg 15))))))


(defun k-data-type (pointer)
  (hw:ldb pointer vinc:%%data-type 0))

(defun k-data-type-name (pointer)
  (let* ((type (k-data-type pointer))
	 (type-entry (rassoc type vinc:k-data-type-names-alist :key #'car)))
    (if type-entry
	(car type-entry)
      type)))
      
(defun show-character (pointer &optional (stream t))
  (format stream "#/~c" (int-char (hw:ldb pointer (byte 8. 0.) ;;li:%%ch-char
					0))))
      

;; added support for printing the whole string if *show-object-max-length* is nil  ||| 27sept88 pfc
      
(defun read-string (pointer &optional (max-print-length 70))
  (if (/= (k-data-type pointer) vinc:$$dtp-array)
      (cerror "OK" "This isn't a string, it is a ~s" (k-data-type pointer))) 
  (let* ((header   (kbug-generic-read-memory pointer))
	 (multiple (= (k-data-type header) vinc:$$dtp-array-header-multiple)))
    (if (/= (k-data-type header) vinc:$$dtp-array-header-single)
	(if (not multiple)
	    (cerror "OK" "This isn't an array header, it's type is ~s" (k-data-type header))
	  (let* ((length  (hw:ldb header array:%%bounds 0))
		 (header2 (kbug-generic-read-memory (1- pointer)))
		 (fillp   (hw:ldb header2 array:%%fill-pointer-p 0))
		 (ldr-off (hw:ldb header2 array:%%leader-offset  0)) ;;offset to Number-of-Leader-Words word.
		 (fill-pt (hw:ldb (kbug-generic-read-memory (- pointer ldr-off 1)) vinc:%%fixnum-field 0))
		 (short  (if *show-object-max-length*
			     (min fill-pt max-print-length)
			   fill-pt))
		 (string (make-string short)))
	    (dotimes (i short)
	      (setf (svref string i)
		    (int-char (hw:ldb (kbug-generic-read-memory (+ pointer (ash i -2) 1))
				      (byte 8 (* 8 (ldb (byte 2 0) i)))
				      0))))
	    (if fillp
		(format nil "~A..MULTIPLE length = #x~x  fill-pointer = #x~x" string length fill-pt)
	      (format nil "~A..MULTIPLE length = #x~x" string length))))
      (let* ((length (hw:ldb header array:%%bounds 0))
	     (short  (if *show-object-max-length*
			 (min length max-print-length)
		       length))
	     (string (make-string short)))
	(dotimes (i short)
	  (setf (svref string i)
		(int-char (hw:ldb (kbug-generic-read-memory (+ pointer (ash i -2) 1))
				  (byte 8 (* 8 (ldb (byte 2 0) i)))
				  0))))
	(if (= short length)
	    string
	  (format nil "~A... length = #x~X" string length))))))
	

(defun kbug-local-symbol (pointer)
  (if (/= (hw:ldb pointer vinc:%%data-type 0)
	  vinc:$$dtp-symbol)
      (global:ferror "This isn't a symbol, it is a ~s" (k-data-type pointer)))
  (let ((header (kbug-generic-read-memory pointer)))
    (if (/= (hw:ldb header vinc:%%data-type 0)
	    vinc:$$dtp-symbol-header)
	(global:ferror "This symbol's header is clobbered, it's a ~s" (k-data-type header)))
    (intern (read-string (hw:dpb vinc:$$dtp-array vinc:%%data-type header)) *package*)))

(defun read-symbol-name (pointer &optional (expect-symbol-datatype t))
  (when (and expect-symbol-datatype
	     (/= (hw:ldb pointer vinc:%%data-type 0)
		 vinc:$$dtp-symbol))
    (cerror "OK" "This isn't a symbol, it is a ~s (~s)"
	    (k-data-type pointer) (car (ass2q (k-data-type pointer) vinc:k-data-type-names-alist))))
  (let* ((header      (kbug-generic-read-memory pointer))
	 (pkg         (kbug-generic-read-memory (+ symbol:*symbol-package* pointer)))
	 (symbol-name (read-string (hw:dpb vinc:$$dtp-array vinc:%%data-type header))))
    (if (= (hw:ldb pkg vinc:%%data-type 0) vinc:$$dtp-structure)
	(format nil "~a:~a" (find-package-name pkg) symbol-name)
      (if (= (hw:ldb pkg vinc:%%data-type 0) vinc:$$dtp-array)
	  (format nil "<~a>:~a" (read-string pkg) symbol-name)
	(format nil "[cold]:~a" symbol-name)))))

(defun find-package-name (package-structure)
  (let* ((pkg-nick    (kbug-generic-read-memory (+ package-structure 7))) ;;;+++ This constant is dangerous.
	 (first-nick  (kbug-generic-read-memory pkg-nick))
	 (pkg-name    (if (= first-nick 0) ;;If no nicknames use the name.
			  (kbug-generic-read-memory (+ package-structure 6))
			first-nick)))
    (format nil "~a" (read-string pkg-name))))
  
(defun ass2q (x l)
  (cond ((null l) nil)
	((eq x (cadr (car l))) (car l))
	(t (ass2q x (cdr l)))))

(defun local-corresponding-symbol (pointer)
  (if (/= (hw:ldb pointer vinc:%%data-type 0)
	  vinc:$$dtp-symbol)
      (cerror "OK" "This isn't a symbol, it is a ~s" (k-data-type pointer)))
  (let ((header (kbug-generic-read-memory pointer)))
    (if (/= (hw:ldb header vinc:%%data-type 0)
	    vinc:$$dtp-symbol-header)
	(cerror "OK" "This isn't a symbol header, it's a ~s" (k-data-type header)))
    (let* ((symbol-pname-string (read-string (hw:dpb vinc:$$dtp-array vinc:%%data-type header)))
	   (package-cell (kbug-generic-read-memory (+ pointer symbol:*symbol-package*)))
	   (package-cell-datatype (ldb vinc:%%data-type package-cell)))
      (cond ((not (= package-cell-datatype vinc:$$dtp-structure))
	     (global:fsignal "Unable to decode package cell contents"))
	    (t (let ((possible-structure-symbol  (kbug-generic-read-memory (1+ package-cell)))
		     temp)
		 (cond ((or (not (= (ldb vinc:%%data-type possible-structure-symbol)
				    vinc:$$dtp-symbol))
			    (not (string-equal (setq temp (read-symbol-name possible-structure-symbol))
					       "LI:PACKAGE")))
			(global:fsignal "Bad structure in package cell"))
		       (t (let ((package-name-component
				  (kbug-generic-read-memory (+ package-cell 2))))
			    (show-object package-name-component)
			    #+why-is-this-here(global:fsignal "foo")))))))
      ;for now, just return the string
      symbol-pname-string
      )))
      
(defun show-symbol (pointer &optional (stream t) (expect-symbol-datatype t) (print-package nil))
  (zl:condition-case (.error.)
      (format stream "~a" (read-symbol-name pointer expect-symbol-datatype))
   (zl:error (format stream "Can't print pointer #x~X: ~A" pointer .error.))))

(defun show-string (pointer &optional (stream t))
  (prin1 (read-string pointer) stream))

(defun show-list (pointer)
  (do ()
      ((zerop pointer))
    (format t "~&~x" (kbug-generic-read-memory pointer))
    (setq pointer (kbug-generic-read-memory (1+ pointer)))))

(defun show-compiled-function (pointer &optional (stream t))
  (zl:condition-case (.error.)
  (format stream "#'~a" (read-symbol-name (kbug-generic-read-memory (1+ pointer))))
    (zl:error (format stream "Can't print pointer #x~X: ~A" pointer .error.))))

(defun show-structure  (pointer &optional (stream t) (package-name-only t))
  ;below lossage necessary until named structures get fixed.
  (let ((possible-structure-symbol  (kbug-generic-read-memory (1+ pointer))))
    (global:select (ldb vinc:%%data-type possible-structure-symbol)
      (vinc:$$dtp-symbol
	   ;assume it is
	(let ((symbol-name (read-symbol-name possible-structure-symbol)))
	  (if (and package-name-only
		   ;; added flag to turn off abbreviated printing of package structures ||| 27sept88 pfc
		   (or (string-equal symbol-name "li:package")
		       (string-equal symbol-name "<li>:package")))
	      (format stream "[Package :~a]" (find-package-name pointer))
	    (let* ((elements+1 (kbug-generic-read-memory pointer))
		   (n          (ldb vinc:%%fixnum-field elements+1))
		   (m          (if *show-object-max-length*
				   (min n (1+ *show-object-max-length*))
				 n)))
	      (format stream "#S[~a " symbol-name)
	      (do ((i 2 (1+ i)))
		  ((> i m) (unless (= n m) (format stream " ...")))
		(show-object (kbug-generic-read-memory (+ pointer i)) stream i 1)
		(format stream " "))
	      (format stream "]")))))
      (vinc:$$dtp-array
	(show-array pointer stream))
      (t (format stream "Unable to decode #x~X as structure" pointer)))))

(defun array-type (array-code)
  (cond
    ((= array:art-q             array-code) 'art-q)
    ((= array:art-1b		array-code) 'art-1b)
    ((= array:art-2b		array-code) 'art-2b)
    ((= array:art-4b		array-code) 'art-4b)
    ((= array:art-8b		array-code) 'art-8b)
    ((= array:art-16b		array-code) 'art-16b)
    ((= array:art-32b		array-code) 'art-32b)
    ((= array:art-2bs		array-code) 'art-2bs)
    ((= array:art-4bs		array-code) 'art-4bs)
    ((= array:art-8bs		array-code) 'art-8bs)
    ((= array:art-16bs		array-code) 'art-16bs)
    ((= array:art-32bs		array-code) 'art-32bs)
    ((= array:art-string	array-code) 'art-string)
    ((= array:art-fat-string  	array-code) 'art-fat-string)
    ((= array:art-single-float	array-code) 'art-single-float)
    ((= array:art-double-float	array-code) 'art-double-float)
    (t (format nil "Unknown type #x~X" array-code))))

(defun show-array (pointer &optional (stream t))
  (let* ((header    (kbug-generic-read-memory pointer))
	 (header-dt (k-data-type header))
	 (acode     (ldb array:%%sv-art header))
	 (bounds    (ldb array:%%bounds header)))
    (cond ((= header-dt vinc:$$dtp-array-header-single)
	   (if (= acode array:art-string)
	       (show-string pointer stream)
	     (format stream " Array (single header) acode: ~x bounds #x~x" (array-type acode) bounds)))
	  ((= header-dt vinc:$$dtp-array-header-multiple)
	   (let* ((header2 (kbug-generic-read-memory (1- pointer)))
		  (acode2 (ldb array::%%array-type header2)))
	     (if (= acode2 array:art-string)
		 (show-string pointer stream)
	       (format stream " Array (double header) acode: ~x 1-dim bounds #x~x" (array-type acode2) bounds))))
	  (t
	   (format stream "Array has bad header, its data-type is ~s" (k-data-type header))))))

(defvar *show-object-max-depth* 3)
(defvar *show-object-max-length* 5)

(defun show-data-type-and-object (pointer &optional (stream t))
  (format stream "~19a" (k-data-type-symbol (ldb vinc:%%data-type pointer)))
  (show-object pointer stream))


(defun show-full-object (pointer)
  (let ((*show-object-max-depth*  nil)
	(*show-object-max-length* nil))
    (show-object pointer)))

(defun show-object-to-string (pointer)
  (let ((*show-object-max-depth*  nil)
	(*show-object-max-length* nil))
    (with-output-to-string (str) (show-object pointer str))))

(defun show-object (pointer &optional (stream t) (current-length 0) (current-depth 0) show-pointer)
  (cond ((and *show-object-max-depth*
	      (>= current-depth *show-object-max-depth*))
	 (format stream " ### "))
	((and *show-object-max-length*
	      (>= current-length *show-object-max-length*))
	 (format stream " ... "))
	(t
	 (when show-pointer (format stream "#x~x" pointer))
	 (global:select (ldb vinc:%%data-type pointer)
	   (vinc:$$dtp-nil          (format stream "NIL"))
	   (vinc:$$dtp-fixnum       (show-fixnum       pointer stream))
	   (vinc:$$dtp-bignum       (show-bignum       pointer stream))
	   (vinc:$$dtp-rational	    (show-rational     pointer stream))
	   (vinc:$$dtp-complex      (show-complex      pointer stream))
	   (vinc:$$dtp-short-float  (show-short-float  pointer stream))
	   (vinc:$$dtp-single-float (show-single-float pointer stream))
	   (vinc:$$dtp-double-float (show-double-float pointer stream))
	   (vinc:$$dtp-symbol       (show-symbol       pointer stream))
	   (vinc:$$dtp-compiled-function (show-compiled-function pointer stream))
	   (vinc:$$dtp-array        (show-array        pointer stream))
	   (vinc:$$dtp-character    (show-character    pointer stream))
	   (vinc:$$dtp-structure    (show-structure    pointer stream))
	   (vinc:$$dtp-code         (format stream "~A" (get-warm-symbolic-address pointer)))
	   (vinc:$$dtp-unbound      (format stream "Unbound: ")
				    (show-symbol       pointer stream nil))
	   (vinc:$$dtp-cons         (show-cons         pointer stream current-length current-depth show-pointer))
	   (vinc:$$dtp-unboxed-header (format stream "  # of words = #x~x" (ldb vinc::%%fixnum-field pointer)))
	   (t (format stream " Datatype is ~s, hex:  " (ldb vinc:%%data-type pointer))
	      (hex32 pointer stream))))))

(defun show-fixnum (pointer &optional (stream t))
  (cond ((zerop (ldb vinc:%%fixnum-sign-bit pointer))
	 (format stream "#x+~x" (ldb vinc:%%fixnum-field pointer)))
	(t
	 (format stream "#x-~x" (1+ (logxor #o37777777 (logand #o37777777 pointer)))))))

(defun show-bignum (pointer &optional (stream t))
  (let* ((size (zl:ldb (byte 19. 0) (kbug-generic-read-memory pointer)))
	 (high-word (kbug-generic-read-memory (+ pointer size))))
    (cond
      ((zerop size) (format stream "Illegal Bignum of zero length!"))
      ((and *show-object-max-length*
	    (> size *show-object-max-length*))
       (let ((low  (kbug-generic-read-memory (+ pointer 1)))
	     (high (kbug-generic-read-memory (+ pointer size))))
	 (format stream "#x bignum of #x~X words  high word: #x~8,'0x  low word: #x~8,'0x" size high low)))
      ((zerop (ldb vinc:%%bignum-sign-high-word high-word))
       (format stream "#x+~8,'0x" high-word)
       (do ((n (1- size) (1- n)))
	   ((zerop n))
	 (format stream ",~8,'0x" (kbug-generic-read-memory (+ pointer n)))))
      (t (let ((non-zero-word (do ((n 1 (1+ n)))
				  ((plusp (kbug-generic-read-memory (+ pointer n))) n))))
	   (format stream "#x-")
	   (do ((n size (1- n)))
	       ((= n non-zero-word))
	     (unless (= n size) (format stream ","))
	     (format stream "~8,'0x" (logxor #xFFFFFFFF (kbug-generic-read-memory (+ pointer n)))))
	   (do ((n non-zero-word (1- n)))
	       ((zerop n))
	     (unless (= n size) (format stream ","))
	     (format stream "~8,'0x" (logand #xFFFFFFFF (1+ (logxor #xFFFFFFFF
								    (kbug-generic-read-memory (+ pointer n))))))))))))

(defun show-rational (pointer &optional (stream t))
  (let* ((numerator   (kbug-generic-read-memory pointer))
	 (denominator (kbug-generic-read-memory (1+ pointer))))
    (show-object numerator stream)
    (format stream " \\ ")
    (show-object denominator stream)))

(defun show-complex (pointer &optional (stream t))
  (let* ((real-part      (kbug-generic-read-memory pointer))
	 (imaginary-part (kbug-generic-read-memory (1+ pointer))))
    (show-object real-part stream)
    (format stream " + ")
    (show-object imaginary-part stream)
    (format stream "i")))

(defun show-short-float (pointer &optional (stream t))
  (let* ((sign-bit  (nlisp:ldb hw#:%%short-float-sign pointer))
	 (sign-char (if (zerop sign-bit) '+ '-))
	 (sign      (if (zerop sign-bit) 1 -1))
	 (exp-actu  (nlisp:ldb hw#:%%short-float-exponent pointer))
	 (exp       (if (zerop exp-actu) exp-actu (- exp-actu hw#:$$short-float-exponent-excess)))
	 (mant-size (prims#:byte-size hw#:%%short-float-mantissa))
	 (scale-exp (- exp mant-size))
	 (mant      (nlisp:ldb hw#:%%short-float-mantissa pointer)))
    (cond ((zerop exp-actu)
	   (let ((val (* sign mant (expt 2 scale-exp))))
	     (format stream "~s0.~17,'0b*2^0  ~d = ~d"
		     sign-char mant val (float val))))
	  ((= hw#:$$short-float-exponent-not-a-number exp-actu)
	   (format stream "Short Float Not a Number."))
	  (t
	   (let ((val (* sign (+ mant (expt 2 mant-size)) (expt 2 scale-exp))))
	     (format stream "~s1.~17,'0b*2^~d  ~d = ~d" sign-char mant exp val (float val)))))))

(defun show-single-float (pointer &optional (stream t))
  (if (= (kbug-generic-read-memory pointer)
	 #b10000000000000000000000000000001)
      (show-single (kbug-generic-read-memory (1+ pointer)) stream)
    (format stream "Illegal single-float which does not point to an unboxed header of length 1.")))

(defun show-single (single &optional (stream t))
  (let* ((sign-bit  (nlisp:ldb hw#:%%single-float-sign single))
	 (sign-char (if (zerop sign-bit) '+ '-))
	 (sign      (if (zerop sign-bit) 1 -1))
	 (exp-actu  (nlisp:ldb hw#:%%single-float-exponent single))
	 (exp       (if (zerop exp-actu) exp-actu (- exp-actu hw#:$$single-float-exponent-excess)))
	 (mant-size (prims#:byte-size hw#:%%single-float-mantissa))
	 (scale-exp (- exp mant-size))
	 (mant      (nlisp:ldb hw#:%%single-float-mantissa single)))
    (cond ((zerop exp-actu)
	   (let ((val (* sign mant (expt 2 scale-exp))))
	     (format stream "~s0.~23,'0b*2^0  ~d = ~d" sign-char mant val (float val))))
	  ((= hw#:$$single-float-exponent-not-a-number exp-actu)
	   (format stream "Single Float Not a Number."))
	  (t
	   (let ((val (* sign (+ mant (expt 2 mant-size)) (expt 2 scale-exp))))
	     (format stream "~s1.~23,'0b*2^~d  ~d = ~d" sign-char mant exp val (float val)))))))

(defun show-double-float (pointer &optional (stream t))
  (if (= (kbug-generic-read-memory pointer)
	 #b10000000000000000000000000000010)
      (show-double (kbug-generic-read-memory (1+ pointer)) (kbug-generic-read-memory (+ 2 pointer)) stream)
    (format stream "Illegal double-float which does not point to an unboxed header of length 2.")))

(defun show-double (double-lo double-hi &optional (stream t))
  (let* ((sign-bit  (nlisp:ldb hw#:%%double-float-sign-word2 double-hi))
	 (sign-char (if (zerop sign-bit) '+ '-))
	 (sign      (if (zerop sign-bit) 1 -1))
	 (exp-actu  (nlisp:ldb hw#:%%double-float-exponent-word2 double-hi))
	 (exp       (if (zerop exp-actu) exp-actu (- exp-actu hw#:$$double-float-exponent-excess)))
	 (mant-size (+ (prims#:byte-size hw#:%%double-float-mantissa-word1)
		       (prims#:byte-size hw#:%%double-float-mantissa-word2)))
	 (scale-exp (- exp mant-size))
	 (mant-lo   double-lo) ;;(nlisp:ldb hw#:%%double-float-mantissa-word1 double-lo)
	 (mant      (+ (ash (nlisp:ldb hw#:%%double-float-mantissa-word2 double-hi)
			    (prims#:byte-size hw#:%%double-float-mantissa-word1))
		       mant-lo)))
    (cond ((zerop exp-actu)
	   (let ((val (* sign mant (expt 2 scale-exp))))
	     (format stream "~s0.~52,'0b*2^0 = ~d" sign-char mant #+not-room-on-screen val (float val))))
	  ((= hw#:$$double-float-exponent-not-a-number exp-actu)
	   (format stream "Single Float Not a Number."))
	  (t
	   (let ((val (* sign (+ mant (expt 2 mant-size)) (expt 2 scale-exp))))
	     (format stream "~s1.~52,'0b*2^~d = ~d" sign-char mant exp #+not-room-on-screen val (float val)))))))

(defun show-cons (pointer stream current-length current-depth show-pointer)
  (format stream "(")
  (prog (car cdr)
     next
	(when (and *show-object-max-length*
		   (>= current-length *show-object-max-length*))
	  (format stream " ... ")
	  (return))
	(setq car (kbug-generic-read-memory pointer))
	(show-object car stream 0 (1+ current-depth) show-pointer)
	(setq cdr (kbug-generic-read-memory (1+ pointer)))
	(let ((typ (ldb vinc:%%data-type cdr)))
	  (when (= typ vinc:$$dtp-cons)
	    (format stream " ")
	    (incf current-length)
	    (setq pointer cdr)
	    (go next))
	  (unless (= typ vinc::$$dtp-nil)
	    (format stream " . ")
	    (show-object cdr stream (1+ current-length) current-depth show-pointer))))
  (format stream ")"))

(defun read-symbol-value (symbol)
  (kbug-generic-read-memory
    (+ symbol:*symbol-value*
       (kbug-intern (symbol-name symbol) (si:package-primary-name (symbol-package symbol))))))

(defun show-symbol-value (symbol)
  (show-object (read-symbol-value symbol)))
    
(defun describe-symbol (symbol &optional package-string)
  (if (null package-string)
      (setq package-string (si:package-primary-name (symbol-package symbol))))
  (let ((symbol-location (kbug-intern (symbol-name symbol) package-string)))
    (format t "~%Description of Symbol ~s in package ~a:" symbol package-string)
    (format t "~%Value cell: ")
    (show-object (kbug-generic-read-memory (+ symbol:*symbol-value* symbol-location)))
    (format t "~%Function cell: ")
    (show-object (kbug-generic-read-memory (+ symbol:*symbol-function* symbol-location)))
    (format t "~%Package cell: ")
    (show-object (kbug-generic-read-memory (+ symbol:*symbol-package* symbol-location)))
    (format t "~%Plist cell: ")
    (show-object (kbug-generic-read-memory (+ symbol:*symbol-plist* symbol-location)))))

(defun show-frame-without-datatypes (frame) "This function is decommited, was just show-frame"
  (do ((r 0 (1+ r))
       (rlist (kbug-generic-read-frame-as-list frame) (cdr rlist)))
      ((= r 16.))
    (format t "~% ~2d   ~:[ ~;*~]~8,'0X" r (caar rlist) (cdar rlist))))

(defun show-frame (frame &optional (stream t))
  (show-frame-with-datatypes frame stream))

(defun show-all-frames (&optional (stream t))
  (dotimes (n (length *global-frame-table*))
    (show-frame-with-datatypes n stream)))

(defun show-frame-with-datatypes (frame &optional (stream t))
  (let ((frame-register-list (cdr (nth frame *global-frame-table*))))
    (flet ((show-datatypes (frame-reg-list rlist reg stream)
		(if frame-register-list
		    (show-register-with-datatypes frame-reg-list rlist reg stream)
		  (show-rlist-with-datatypes rlist reg stream))))
      (do ((r 0 (1+ r))
	   (rlist (kbug-generic-read-frame-as-list frame) (cdr rlist)))
	  ((= r 16.))
	(format stream "~%")
	(show-datatypes frame-register-list rlist r stream)))))

(defun show-register-with-datatypes (frame-register-list rlist reg &optional (stream t))
  (let* ((boxed   (caar rlist))
	 (pointer (cdar rlist))
	 (star    (if boxed "*" " ")))
    (format stream "~12,,a ~2,,2d ~a~8,'0X," (nth reg frame-register-list) reg star pointer)
    (when boxed
      (show-data-type-and-object pointer stream))))

(defun show-rlist-with-datatypes (rlist reg &optional (stream t))
  (let* ((boxed   (caar rlist))
	 (pointer (cdar rlist))
	 (star    (if boxed "*" " ")))
    (format stream " ~2,,2d ~a~8,'0X," reg star pointer)
    (when boxed
      (show-data-type-and-object pointer stream))))

;CONVENTION:  KBG-xxx are low level routines for when machine is completely stopped.
;   they generally cause loss of inner processor state.
  
  
(defun kbg-read-reg (reg &optional check-oar)
 "Register read for when machine is stopped.  Totally mungs state. Uses OAR path."
  (let ((frame (ldb (byte 8. 4.) reg))
	(offset (ldb (byte 4 0) reg)))
    (saving-oar
      #'(lambda (ignore)
	  (lam:k-write-oar (dpb frame k-hw:%%ch-oar-open 0))
	  (if check-oar (let ((oar (lam:k-read-oar)))
			  (if (not (= frame (ldb k-hw:%%ch-oar-open oar)))
			      (global:fsignal "OAR failed to load, is #x~x should be #x~x"
				       (ldb k-hw:%%ch-oar-open oar) frame))))
	  (kbg-read-open offset)))))

(defun kbg-read-reg-via-left (reg &optional check-oar)
 "Register read for when machine is stopped.  Totally mungs state. Uses OAR path. Uses left source."
  (let ((frame (ldb (byte 8. 4.) reg))
	(offset (ldb (byte 4 0) reg)))
    (saving-oar
      #'(lambda (ignore)
	  (lam:k-write-oar (dpb frame k-hw:%%ch-oar-open 0))
	  (if check-oar (let ((oar (lam:k-read-oar)))
			  (if (not (= frame (ldb k-hw:%%ch-oar-open oar)))
			      (global:fsignal "OAR failed to load, is #x~x should be #x~x"
				       (ldb k-hw:%%ch-oar-open oar) frame))))
	  (kbg-read-open-via-left offset)))))

(defun kbg-read-reg-with-boxed (reg)
 "Register read (incl boxed bit) for when machine is stopped.  Uses OAR path."
  (let ((frame (ldb (byte 8. 4.) reg))
	(offset (ldb (byte 4 0) reg)))
    (saving-oar
      #'(lambda (ignore)
	  (lam:k-write-oar (dpb frame k-hw:%%ch-oar-open 0))
	  (cons (= 1 (kbg-read-open-boxed offset)) (kbg-read-open offset))))))

(defun kbg-read-open (offset)
  (lam:k-execute3 lam:KIH-ALU-NOP (dpb offset (byte 4. 25.) lam:KIL-READ-O0))
  (k-read-spy-mmfio))

(defun kbg-read-open-via-left (offset)	;read from open frame via left source.
  (lam:k-execute3 lam:KIH-ALU-NOP (dpb offset (byte 4. 19.) lam:KIL-READL-O0))
  (k-read-spy-mmfio))

(defun kbg-read-active (offset)
  (lam:k-execute3 lam:KIH-ALU-NOP (dpb offset (byte 4. 25.) lam:KIL-READ-A0))
  (k-read-spy-mmfio))

(defun kbg-read-open-boxed (offset)
  (lam:k-execute lam:KIH-ALU-G1FBR		;clobbers global register 1F
		  (dpb offset (byte 4. 25.) lam:KIL-READ-O0))
  (lam:k-execute3 lam:kih-alu-nop lam:kil-read-pstat)
  (ldb (byte 1. 18.) (k-read-spy-mmfio)))

(defun kbg-find-reg-crosstalk (&optional (reg 0) (data nil))   ;uses right stuff.
  (if data (kbg-write-reg-via-reg reg data))
  (dotimes (frame 256.)
    (let* ((other-reg (dpb frame (byte 8 4) reg))
	   (read-data (kbg-read-reg other-reg)))
      (cond ((= data read-data)
	     (format t "~%Data written at ~x found at ~x"  reg other-reg))))))

(defun kbg-read-reg-crosschecking (reg &optional inhibit-printouts)
 "Register read for when machine is stopped.  Totally mungs state.  Uses global register path
  so only wins for frames to #xf"
 (let ((right-data nil) (left-data nil) (right-frame-data nil) (left-frame-data nil))
  (setq right-frame-data (kbg-read-reg reg t))
  (setq left-frame-data (kbg-read-reg-via-left reg t))
  (setq right-data (kbg-read-global-reg reg))
  (setq left-data (kbg-read-global-reg-via-left reg))
  (cond ((and (null inhibit-printouts)
	      (or (not (= left-data right-data))
		  (not (= right-data left-frame-data))
		  (not (= left-frame-data right-frame-data))))
	 (global:format t "~%Paths differ, register #x~x, left-frame #x~x right-frame #x~x left #x~x, right #x~x"
			 reg left-frame-data right-frame-data left-data right-data)))
  right-data))

(defun kbg-read-global-reg (reg)
  (let ((frame (ldb (byte 4. 4.) reg)))
    (lam:k-execute4 (dpb frame (byte 4. 5.) lam:KIH-ALU-NOP)
		    (dpb reg (byte 4. 25.) lam:KIL-READR-G0))
    (lam:k-read-spy-mmfio)))

(defun kbg-read-global-reg-via-left (reg)
  (let ((frame (ldb (byte 4. 4.) reg)))
    (lam:k-execute4 (dpb frame (byte 4. 5.) lam:KIH-ALU-NOP)
		    (dpb reg (byte 4. 19.) lam:KIL-READL-G0))
    (lam:k-read-spy-mmfio)))


(defun kbg-read-reg-boxed-bit-via-reg (reg)
 "Read boxed bit of register. only wins for frames to #xf"
 (let ((frame (ldb (byte 4. 4.) reg)))
  (lam:k-execute (dpb frame (byte 4. 5.) lam:KIH-ALU-G0)
		 (dpb reg (byte 4. 25.) lam:KIL-READR-G0))
  lam:(k-execute3 kih-alu-nop kil-read-pstat)
  (ldb (byte 1 18.) (lam:k-read-spy-mmfio))))

  
(defun kbg-read-reg-with-boxed-crosschecking (reg)
  (let ((data (kbg-read-reg-crosschecking reg))
	(boxed (kbg-read-reg-boxed-bit-via-reg reg)))
    (cons (= 1 boxed) data)))
				     
(defun kbg-write-reg (reg data &optional (boxed-bit 0))
  "Register write for when machine is stopped. Totally mungs state. Uses OAR path."
  (let ((frame (ldb (byte 8. 4.) reg))
	(offset (ldb (byte 4 0) reg)))
    (saving-oar
      #'(lambda (ignore)
	  (lam:k-write-oar (dpb frame hw:%%ch-oar-open 0))
	  (kbg-write-open offset data boxed-bit)))))

(defun kbg-write-open (offset data boxed-bit)
  (setq boxed-bit (+ boxed-bit 2))	;make boxed code 2 or 3
  (lam:k-execute (dpb boxed-bit (byte 2 (- 54. 32.))
		      (dpb offset (byte 4 (- 41. 32.)) lam:kih-load-o0))
		 data)
  lam:(k-execute3 KIH-NOP 0))
  
(defun kbg-write-reg-via-reg (reg data)
 "Register write for when machine is stopped.  Totally mungs state. Uses global register path
  so only wins for frames to #xf"
 (let ((frame (ldb (byte 4. 4.) reg)))
  ;   lam:(k-execute3 KIH-JUMP #x100)   ;presumably not necessary
   (lam:k-execute (dpb reg (byte 4. 9.) (dpb frame (byte 4. 5) lam:KIH-LOAD-G0)) data)
   lam:(k-execute3 KIH-NOP 0)))



(defun kbg-read-frame-as-list (frame)
  (do* ((r 15. (1- r))
	(rlist))
       ((minusp r) rlist)
    (setq rlist (cons (kbg-read-reg-with-boxed (dpb frame (byte 8 4) r)) rlist))))

(defun kbg-read-all-frames-crosschecking ()
  (dotimes (f 16.)
    (kbg-read-frame-as-list-crosschecking f)))

(defun kbg-read-frame-as-list-crosschecking (frame)
  "Uses register path, max frame #xF"
  (do* ((r 15. (1- r))
	(rlist))
       ((minusp r) rlist)
    (setq rlist (cons (kbg-read-reg-with-boxed-crosschecking (dpb frame (byte 8 4) r)) rlist))))


(def-k-generic kbug-generic-read-register-with-boxed (reg)
  (kbg-read-reg-with-boxed reg)
  (let ((frame (ldb (byte 8 4) reg))
	(offset (ldb (byte 4 0) reg)))
    (nth offset (read-frame-as-list frame))))

(def-k-generic kbug-generic-read-frame-as-list (frame)
  (kbg-read-frame-as-list frame)
  (read-frame-as-list frame))

(defun kbg-read-call-stack-as-list ()
  (let ((saved-pc (lam:k-read-spy-pc))
	(saved-hp-csp (lam:k-read-hp-sp))
	(saved-oar (lam:k-read-oar)))
    (do ((depth (ldb (byte 8 0) saved-hp-csp) (1- depth))
	 (ans (list (list saved-pc (kbg-convert-oar saved-oar)))))	;top level status, sort of valid, maybe.
	((zerop depth)
	 (lam:k-write-hp-csp saved-hp-csp)	;restore csp first.
	 (lam:k-write-oar saved-oar)
	 (lam:k-set-pc saved-pc)
	 (reverse ans))
      (k-write-hp-csp (dpb depth (byte 8. 0.) 0.))
      (let ((retpc-rdest (k-read-retpc-rdest)))
  ;return first.  The toplevel OAR goes with the PC, which we tried to save above.
	(lam:k-execute lam:KIH-RETURN 0.)		;to cause OAR to restore from stack.
	(let ((oar (k-read-oar)))
	  (push (list retpc-rdest (kbg-convert-oar oar)) ans))))))

(defun kbg-read-frame-free-list ()
  (let ((saved-pc (lam:k-read-spy-pc))
	(saved-hp-csp (lam:k-read-hp-sp))
	(saved-oar (lam:k-read-oar)))
    (do ((ans))
	((not (= (global:tyi) #\space ))
	 (lam:k-write-hp-csp saved-hp-csp)	;restore csp first.
	 (lam:k-write-oar saved-oar)
	 (lam:k-set-pc saved-pc)
	 (reverse ans)
	 )
      (lam:k-execute lam:kih-topen #x100)
      (let ((oar (k-read-oar))
	    (hp-csp (lam:k-read-hp-sp)))
	(push (ldb (byte 8 16.) oar) ans)
	(format t "frame:~X hp:~X" (ldb (byte 8 16.) oar) (ldb (byte 8 8) hp-csp))))))

;for some losing reason, the KBUG2 thing (in K2) repacks things.  So simulate that.
(defun kbg-convert-oar (oar)
  (let ((o (ldb (byte 8 16.) oar))
	(a (ldb (byte 8 8) oar)))
    (dpb o (byte 8 8) a)))

(def-k-generic kbug-generic-read-call-stack-as-list ()
  (kbg-read-call-stack-as-list)
  (read-call-stack-as-list))


(defun show-globals (&optional (stream t))	;call update-globals-from-remote-machine before this.
  (do ((ft *global-frame-table* (cdr ft))
       (fn 0 (1+ fn)))
      ((null ft))
    (do ((rn 0 (1+ rn))
	 (gl (cdr (car ft)) (cdr gl)))
	((null gl))
      (multiple-value-bind (contents boxed)
	  (read-register fn rn)
	(format stream "~%~a  " (car gl))
	(let ((special-printer (get (car gl) 'kbug-printer)))
	  (cond (special-printer (funcall special-printer stream contents boxed))
		(boxed (show-object contents stream))
		(t (format stream "Unboxed. #x=~x, #D=~d, #O=~o" contents contents contents))))))))

(defun (:property gr:*save-trap* kbug-printer) (stream contents boxed)
  (if boxed (format t "~%Trap register is #x~x, and boxed!!" contents)
    (k-print-trap-bits stream contents)))

(defun (:property gr:*trap-mask* kbug-printer) (stream contents boxed)
  (if boxed (format t "~%Trap mask is #x~x, and boxed!!" contents)
    (k-print-trap-bits stream contents)))

(defun k-print-trap-bits (stream trap-word)
  (format stream " ~o, bits: " trap-word)
  (lam:print-bits trap-word)
  (do ((bit-no 31. (1- bit-no))
       (p '(reset trace icache-parity icache-nubus-err
		  icache-nubus-timeout icache-page-fault proc-mread-parity proc-mread-nubus-err
		  proc-mread-nubus-timeout proc-mread-page-fault proc-mread-transporter proc-mwrite-nubus-err
		  proc-mwrite-nubus-timeout proc-mwrite-page-fault proc-mwrite-gc floating-point
		  heap-empty instruction-bit datatype overflow
		  spare11 interrupt7 interrupt6 interrupt5
		  interrupt4 interrupt3 interrupt2 interrupt1
		  interrupt0 timer-1024 timer-16384 spurious) (cdr p)))
      ((null p))
    (if (global:bit-test (ash 1 bit-no) trap-word)
	(format stream " ~A " (car p)))))

(defun update-globals-from-remote-machine ()
  "This function has been replaced by UPDATE-GLOBALS"
  "This function has been replaced by UPDATE-GLOBALS")

(defun update-globals ()
  (labels ((update-frame (framelist count)
	     (when framelist
	       (update-globals-internal (cdr (car framelist)) count 0)
	       (update-frame (cdr framelist) (1+ count))))
	   (update-globals-internal (globals frame count)
	     (when globals
	       (let ((this-global (first globals)))
		 (multiple-value-bind (contents boxed) (read-register frame count)
		   (let ((decoded-value
			   (if boxed
			       (let ((datatype (ldb vinc::%%data-type contents)))
				 (cond ((= datatype vinc:$$dtp-fixnum)
					(+ (* (- (expt 2. 24.)) (ldb vinc::%%fixnum-sign-bit contents))
					   (ldb (byte (1- (byte-position vinc::%%fixnum-sign-bit))
						      0)
						contents)))
				       ((= datatype vinc:$$dtp-nil) nil)
				       ((= datatype vinc:$$dtp-code) 'code-pointer)
				       ((= datatype vinc:$$dtp-symbol)
					(cond ((= (logand contents #x3FFFFFF)	;pointer field
						  5.)
					       'T)
					      ((string-equal (read-symbol-name contents)
							     "KEYWORD-GARBAGE")
						;package structure of this doesnt seem well formed.
					       'keyword-garbage)
					      (t
					       (local-corresponding-symbol contents))))
				       (t (format t "~%Found something weird in ~s." this-global)
					  `(weird-thing ,contents))))
			     (progn (format t "~%~s is unboxed." this-global)
				    contents))))
		     (set this-global decoded-value)
		     (update-globals-internal (rest globals) frame (1+ count))))))))
    (update-frame *global-frame-table* 0)))

(defun kbg-read-register (frame offset)		;reads it via OAR. Actually reads right physical memory.
 "Register read for when machine is stopped.  Totally mungs state. Uses OAR path."
    (saving-oar
      #'(lambda (ignore)
	  (lam:k-write-oar (dpb frame hw:%%ch-oar-open 0))
	  (values (kbg-read-open offset)
		  (= 1 (kbg-read-open-boxed offset))))))

(def-k-generic read-register (frame offset)
  (kbg-read-register frame offset)
  (kbug2-read-register frame offset))

(defun number-of-free-clusters ()
  (let ((register (get 'gr#:*physical-cluster-free-clusters* :register)))
    (dpb 0 vinc:%%data-type (read-register (second register) (third register)))))


(def-k-generic read-transporter-ram (address)
  (kbg-read-transporter-ram address)
  (kbug2-read-transporter-ram address))

(defun kbug2-read-transporter-ram (address)
  (kbug-cmd-confirm k2:kbug-command-read-misc 2 address)
  (let ((response (kbug-data 0))) 
    (cond ((not (numberp response))
	   (global:fsignal "bad reponse" response))
	  ((not (= (ldb vinc:%%data-type response) vinc:$$dtp-fixnum))
	   (global:fsignal "response not fixnum" response)))
    (logand #xf response)))

(def-k-generic write-transporter-ram (address data)
  (kbg-write-transporter-ram address data)
  (kbug2-write-transporter-ram address data))

(defun kbug2-write-transporter-ram (address data)
  (kbug-cmd-confirm k2:kbug-command-write-misc 2 address data)
  data)

(def-k-generic read-gc-ram (address)
  (k-read-gc-ram address)
  (kbug2-read-gc-ram address))

(defun kbug2-read-gc-ram (address)
  (kbug-cmd-confirm k2:kbug-command-read-misc 3 address)
  (let ((response (kbug-data 0))) 
    (cond ((not (numberp response))
	   (global:fsignal "bad reponse" response))
	  ((not (= (ldb vinc:%%data-type response) vinc:$$dtp-fixnum))
	   (global:fsignal "response not fixnum" response)))
    (logand #xf response)))

(def-k-generic write-gc-ram (address data)
  (k-write-gc-ram address data)
  (kbug2-write-gc-ram address data))

(defun kbug2-write-gc-ram (address data)
  (kbug-cmd-confirm k2:kbug-command-write-misc 3 address data)
  data)

;;;;;;;;;;;;;;
;;; Area data
;;;;;;;;;;;;;;

(defun show-all-areas (&optional (stream t))
  (format stream "~2%All areas:")
  (dotimes (area memory-management::*number-of-areas*)
    (let* ((area-data (area-data::area-region-data area))
	   (status    (area-data::area-data-status area-data)))
      (cond ((= status area-data::$$area-free)
	     (do ((first-area area)
		  (a0 (1+ area) (1+ a0)))
		 ((or (= a0 memory-management::*number-of-areas*)
		      (not (= (area-data::area-data-status
				(area-data::area-region-data a0))
			      area-data::$$area-free)))
		  (format stream "~%Areas ~D up to ~D free." first-area a0))
	       (incf area)))
	    (t (show-area area stream))))))
  

;use (update-globals-from-remote-machine) before calling this.
(defun show-area (area &optional (stream t))
  (format stream "~2%Area ~3d.  " area)
  (let* ((area-data (area-data::area-region-data area))
	 (status    (area-data::area-data-status area-data))
	 (thread    (area-data::area-data-region-thread area-data)))
    (format stream "~[FREE~;Allocated (no regions)~;Allocated~;Fixed~]" status)
    (unless (area-data::area-free? area-data)
      (let ((area-bits (area-data::area-region-bits area)))
	(format stream "~%New regions are Volatility ~d, ~d quant~:*~[a~;um~:;a~] in size."
		(ldb area-data::%%area-region-bits-volatility area-bits)
		(logand #x03FFFFFF (area-data::area-region-size area)))
	(format stream "~%Default-region-bits: ")
	(decode-region-bits (ldb area-data::%%area-region-bits-the-bits area-bits) stream)))
    (when (area-data::area-has-regions? area-data)
	(labels ((show-regions-in-area (region)
		   (show-region region stream)
		   (let* ((thread (area-data::region-list-thread region))
			  (next   (logand thread #x03FFFFFF))
			  (flag   (ldb area-data::%%region-list-thread-end-flag thread)))
		     (cond ((= flag area-data::$$thread-ends)
			    (when (not (= next area))
			      (format stream "~%THREAD DOESN'T LINK ~d ~d!!" area next)))
			   ((= next region)
			    (format stream "~%Infinite loop in thread at region ~d" region))
			   (t
			    (show-regions-in-area next))))))
	  (show-regions-in-area thread)))))

;terminology summary

;  pointer: (data)  			(byte 26. 0)
;  cluster: (granularity of map)	(byte 16. 10.)		64K clusters total.		
;     %%unmapped-vma-byte (unmapped vma low bits)  (byte 10. 0)
;  quantum: (granularity of GC ram)	(byte 12. 14.)		4096 quantums total.
;   region-size must be a multiple of a quantum, therefore max regions = 4096.
;   thus, there are 2**4 = 16. clusters in a quantum.


;important stuff to be able to print on lambda:

;memory data
;   area data	*number-of-areas* = 256.
;     area-region-data
;     area-region-bits
;     area-region-size
;     area-data-status
;     area-data-region-thread
;   region data  all allocated based on max possible 4096. regions.
;     region-bits	(origin: cluster 8.)
;     unsafe-region-free-pointer
;     region-list-thread
;     region-end
;     region-gc-pointer

;   quantum-map  one Q per virtual quantum.  4096 total.
;        at *quantum-map-physical-location*  (cluster 4)
;     %%quantum-map-dqin          (byte 12.  0.)	;Any device can map all quanta
;	where room for this quantum allocated on the paging device.
;     %%quantum-map-device        (byte  4. 12.)	;Up to 16 I/O or paging devices
;     %%quantum-map-region-origin (byte 12. 16.)        ;Same as region number
;     %%quantum-map-status        (byte  2. 28.)
;     %%quantum-map-valid-bit     (byte  1. 28.)
;     %%quantum-map-mapped-bit    (byte  1. 29.)
;     unused (byte 2. 30.)

;   pcd-table one Q per physical cluster.  size *physical-memory-max-clusters*=8192.
;    located in quantum 1, bottom half, ie 8 clusters.
;     unused                      (byte  7.  0.)
;     %%pcd-read-mar-bit          (byte  1.  7.)
;; This field subsumes the next three fields for the purpose of fast dispatching
;; on write faults.
;     %%pcd-write-bits            (byte  3.  8.)
;     %%pcd-write-mar-bit         (byte  1.  8.)
;     %%pcd-read-only-bit         (byte  1.  9.)
;     %%pcd-clean-bit             (byte  1. 10.)
;     %%pcd-virtual-cluster-number (byte (byte-size vinc:%%cluster-number) 11.)
;     unused                      (byte  2. 27.)
;     %%pcd-status                (byte  3. 29.)

;from BOOT (package BOOT:)
;   *initial-physical-cluster-data-physical-location* (30000 octal) 12288.
;commented out *initial-gc-ram-data-physical-location*  (cluster->address 13.)	;1 cluster
;commented out *initial-transporter-ram-data-physical-location*   14.
;;; Virtual addresses of nifty things.
;;; Quantum 0.
;;; Cluster 1.
;   *temporary-map-entry-location*   (lisp::expt 2 (byte-position vinc::%%cluster-number))
;;; Quantum 1.
;;; Clusters 0. 7.
;   *physical-cluster-table-location* (lisp::* 1 (lisp::expt 2.
;							(byte-position vinc::%%quantum-number)))
;;; Clusters 8. 11.
;   *quantum-map-virtual-location*    (+ *physical-cluster-table-location*
;					  (lisp::expt 2.
;						   (1- (byte-position vinc::%%quantum-number))))
;;; Clusters 12. 15.
;   *region-bits-virtual-location*    (+ *quantum-map-virtual-location*
;					  (lisp::expt 2.
;					    (- (byte-position vinc::%%quantum-number) 2)))

;from KOLD-LOADER (package K-COLD:)
;commented out *initial-map-data-physical-location*    (cluster->address 2.)	;64 clusters
;   *quantum-map-physical-location*       4.
;   *quantum-map-clusters*                4.
;   *region-bits-physical-location*       8.
;   *region-bits-clusters*                4.
;   *initial-physical-cluster-data-physical-location* (cluster->address 12.) ;1 cluster
;	 =12288.(30000)
;   *initial-gc-ram-data-physical-location*  (cluster->address 13.) ;1 cluster =13312.(32000)
;   *initial-transporter-ram-data-physical-location*  14.
;;; Virtual addresses of nifty things.
;;; Quantum 0.
;;; Cluster 1.
;   *temporary-map-entry-location*            (ash 1. (byte-position vinc::%%cluster-number))
;;; Quantum 1.
;;; Clusters 0. 7.
;   *physical-cluster-table-location*     (* 1 (ash 1 (byte-position vinc::%%quantum-number)))
;;; Clusters 8. 11.
;   *quantum-map-virtual-location*        (+ *physical-cluster-table-location*
;					   (ash 1. (1- (byte-position vinc::%%quantum-number))))
;;; Clusters 12. 15.
;   *region-bits-virtual-location*   (+ *quantum-map-virtual-location*
;					  (ash 1 (- (byte-position vinc::%%quantum-number) 2)))


;processor data
;   map
;   gc-ram
;   datatype-ram


;boot-vector
;  Intention is whatever boots writes this.  Currently written by pseudo-boot.
;  this would have been read in by prom from disk.  -- also, we are using this to
;  hold a bit of info generated before setq of special-variable can win.

;from KBUG-GENERIC  boot::**boot-vector-origin** is 42.  =#x2A			    observed value 6/20/88
;  boot::**bootprom-version**  (0)						#x2A	#x0
;  boot::**initial-code-physical-location-bv-offset** (1)  *cold-data-size*	#x2B	#x3C00
;  boot::**initial-code-size-in-clusters-bv-offset** (2)			#x2C	#x39
;	      (1+ (ldb vinc::%%cluster-number (ash *cold-code-pointer* 2)))
;  boot::**initial-code-entry-point-bv-offset** (3)				#x2D	#x327B
;      (nc::ncompiled-function-starting-address (find-named-function initial-function))	normally boot:cold-boot-function
;  boot::**physical-memory-block-map** (4.)  Megabyte-per-bit bit-map.		#x2E    #xFF
;-- end "hard" defined locations, begin "soft" --  Intention was, I think, that hard wired would
;    be depended on by the BOOT PROM.
;  boot::*initial-gc-ram-data-physical-location* (5)				#x2F	#x3400  --decommitted
;	      *initial-gc-ram-data-physical-location*
;  boot::*initial-transporter-ram-data-physical-location* (6)			#x30	#x3800
;	      (cluster->address *initial-transporter-ram-data-physical-location*)
;  boot::*cold-load-flag* (7) 1							#x31	#x1
;  boot::*bv-lowlevel-root-region*	these written by MAP-IN-DEBUG-AND-COMMUNCIATION-ROOTS #x32
;  boot::*bv-debug-root-cluster*						#x33
;  boot::*bv-communication-root-cluster*					#x34
;  boot::*bv-all-packages*		points to symbol *all-packages*		#x35

;(trap) function located at location <pc> 0
;(non-modifying-exit)  at location 12.
;(modifying-exit) at location 20.
;(diagnostic-trap-exit) at location 28.
;(trap-vector-table) at location 32.  dispatch table at 32. - 63.

;instructions start at PC address #x100.
;(logior #x02000000 (ash PC 1)) converts PC address to data address.

;kbug-base-addr #x800	from kbug2;common-definitions.
;  k-side in kbug2;k2  lambda-side in kbug2;streams.
; 000     - command
; 001     - status
; 002-009 - command parameters 0-7
; 00A-0EF - internal debug storage
; 0F0-0F4 - K output stream
; 0F5-0F9 - K input fasl stream
; 0FA-0FE - K input character stream (one word per character though)
;   0FF   - unused
; 100-1FF - data transfer area
; 200-3FF - stream buffers

;Access physical memory on the K board:
;  (lam:k-mem-read-word-address <board-physical-address>) (lam:k-mem-write-word-address <b-p-adr> <data>) 
;Access virtual memory:
;  (kbug-generic-read-memory <adr>) (kbug-generic-write-memory <adr> <data>)
;    these work in one of two ways:  If the machine is stopped, instructions
;    are mungded in to the processor and clocked.  This will result in loss of
;    processor state.  If the machine is running, it is assumed the "wedge" is
;    running and it is passed a command.

;low physical and virtual memory usage summary:
;		     <physical>       <virtual>
; quantum 0	lisp-structured
;   cluster 0 #x0    NIL and T, boot-vector #x2A(42.) thru #x31.   self-mapped
;   cluster 1 #x400  boot:*temporary-map-entry-location*  also vmem:*temporary-map-entry*
;		used as scratch map location, very temporary.  see vmem:associate-temporary.
;   cluster 2 #x800  kbug communication area			   self-mapped
;   cluster 3 #xc00  unused	        nubus-stuff:*k-io-regs-cluster*
;					mapped to nubus F<slot>FFF
;				   see (nubus-stuff:cause-nubus-interrupt n) and (nubus-stuff:acknowledge-nubus-interrupt n)
;   cluster 4 #x1000 v.cluster #x18     nubus-stuff:*bus-access-cluster*  
;      and so on thru virtual cluster #x1f mapped to physical #x2C00 (physical cluster #xB)
;   physical clusters C, D, E unused in great eventuality.
;	during initialization,  C is *initial-physical-cluster-data-physical-location*, length: term by entry for cluster 0.
;				   ea entry: <phys-cluster> <virt-cluster> <map-status> <cluster-count>
;				   manually setup in k-cold:get-initial-physical-cluster-data
;				D is *initial-gc-ram-data-physical-location*  length:4096. entries **too long**  **decomitted
;				   ea entry: GC-RAM contents as fixnum.
;				   manually setup in k-cold:load-boot-gc-ram-data.   **decomitted**
;				E is *initial-transporter-ram-data-physical-location*
;				   ea entry 4 bits, packed 8 to a word. length:4096. entries, 512 words.
;				  initialized to zero as part of *cold-data*
;				  modified by k-cold:modify-boot-transporter-ram-data.
;				  initial data comes from k-kbug:*transporter-ram-initial-data*.
;   physical cluster F holds first code page, virtual cluster #x8000.
; quantum 1
;   clusters 0-7  #x4000	*physical-cluster-table-location*
;	  for each physical-cluster (i.e. page), a word.
;	  gives virtual-cluster, clean bit, read-only-bit, write-mar-bit, and read-mar bit.
;   clusters 8-B  #x6000      	*quantum-map-virtual-location*
;         for each virtual quantum (i.e. 2**4 pages), a word.
;	  gives device, device-quanta, region-origin (equivantly region-number) mapped, valid, status
;   clusters C-F  #x7000	*region-bits-virtual-location*

;boot-allocate-physical-clusters allocates via gr:*physical-cluster-free-pointer*.  active only in bootstrapping.
;  callers:  only (!) pcd:create-physical-cluster-data-table 

;new scheme 7/11/88:  we will use the low physical clusters wasted (i.e. left unused) by the current stuff.
;  cluster 1 will be used for debug stuff
;  cluster 3 will be used for MAC-FALCON-LAMBDA communication.
;Note that neither virtual cluster 1 nor 3 is available, the Falcon will be responsible for setting
;  up other virtual clusters and using them, etc.

;stuff to put in cluster 1	*falcon-debug-root-cluster*
;  header area
;    "main" state vector
;
;  IOPB disk stuff if we have that.

;stuff to put in cluster 3      *falcon-communication-root-cluster*
;  ... talk to RWK, etc.

;new low memory allocations:
;  reasons to put things in low memory:
;     communication areas
;       with MAC
;       with "debugger"
;       with lambda disk
;     key pointers to entry points, etc
;       save-processor-image function   (saves all state information held in processor as a vector.)
;       restore-processor-image function (restores it from there)
;	pcd table origin
;	quantum map table origin
;       disk-command-block
;       keyboard comm area
;       mouse comm area
;       network comm area

;ideas for what all to put there:

;  full-saved-machine-image
;  area-name
;  small disk buffer for booting


;saved-machine-image layout:  SAVE-PROCESSOR-IMAGE and RESTORE-PROCESSOR-IMAGE are in K2

;header:

;formatting info:
; -inner processor state.  mostly saved for debugging.
;  
;	processor status
;	memory status
;	trap-register
;	microsecond clock
;	statistics counter
;	nb: trap state is saved as global-frame 0.
; offset  5,   GAP
; offset 11, 
; -main state
;   misc registers
;	processor control
;	memory control
;	OAR
;	HP-CSP
;	Q
;	VMA
;	VMA-BOXED
;	MD
;	MD-BOXED
; offset 19.  GAP
; offset 30.
;
;   call-stack  512.   256x2  (return code, return offset, return pc word)
;		      (return global-frame, saved o, saved a)
; offset 542.
;   frames:     4352.  256. frames x 17. words per frame.  (16 registers plus 1 word of boxed-bits)
; -background state    69.25K allow 70K words total.
;	map		    64Kx32 bits 64K words
;	datatype ram	    128Kx1	packed 32 per word, 4K words
;	transporter ram	    4Kx4	packed 8 per word, 512. words
;	gc ram		    4Kx4	packed 8 per word, 512. words
;	free-frame-list	    256x8	256 words



;random gotcha's documented here because its better than none at all, which was the previous state.
;		BRANCH is a conditional branch, UNCONDITIONAL-BRANCH is the other thing.
;however,	JUMP is an unconditional jump, JUMP-CONDITIONAL is the other thing!

;the boxed bits:
;	Inside the machine, there is no easy way to alter the boxed bit without affecting the quantity
; (in the long run, this is probably just as well).  Or more accurately, there is in the hardware, but
; its not made conveniently available.  Instead, the "interface" to the boxed bits is pretty much at the
; MD/VMA level.  On reads and writes to the VMA and MD, the boxed bits can be specified.


(defun show-state (adr &optional (stream t))
  (let ((value-alist nil))
    (do ((sym '(gr:*state-processor-status* gr:*state-memory-status* gr:*state-trap-register*
		microsecond-clock statistics-counter
		11. gr:*state-processor-control* gr:state-memory-control* oar hp-sp q gr:*state-vma*
		vma-boxed gr:*state-md* md-boxed)
	      (cdr sym))
	 (offset 1 (1+ offset)))
	((null sym))
      (cond ((numberp sym)
	     (setq offset (1- sym)))
	    (t
	     (format stream "~%~S: " (car sym))
	     (let ((value (kbug-generic-read-memory (+ adr offset))))
	       (push (list sym value) value-alist)
	       (show-object value stream)))))
    (let* ((hp-sp (cadr (global:assq 'hp-sp value-alist)))
	   (csp-start (ldb (byte 8 0) hp-sp))
	   (cs nil))
      (do ((csp csp-start (1- csp))
	   (index 30. (+ index 2)))
	  ((minusp csp))
	(let ((d0 (kbug-generic-read-memory (+ adr index)))
	      (d1 (kbug-generic-read-memory (+ adr index 1))))
	  (push (list d0 d1) cs)))
      (show-cs-list cs nil :with-frames nil :stream stream))
    value-alist
    ))

