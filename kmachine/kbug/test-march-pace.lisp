;;; -*- Mode:LISP; Package:LISP-INTERNALS; Base:10; Readtable:CL -*-

;; User provides a piece of code which is in two parts, each of which
;; resides on one page (probably using up less than half a page)
;; This code will be written using defafuns which can jump between
;; each other, finally jumping to a constant location, the copy routine.

;; The copy routine will work from an image of the code in low memory
;; and copy it to two pages at offsets which are different and change
;; each time the routine is called.  All the other virtual pages should
;; map to a page of all halt instructions.  The unused instructions on
;; the copied pages are halt instuctions.

(defvar *march-download-end*)
(defvar *march-parameter-block*) ;physical address

(defun march-defaults ()
  (let ((phys1 (* inst-block-size-in-bytes 16.))
	(phys2 (* inst-block-size-in-bytes 17.)))
    (list phys1
	  phys2
	  (+ (ash 1 25.) phys1)
	  (+ (ash 1 25.) phys2))))

(defun march-store-parameter-block (func1 func2 &optional location)
  (if (null location)
      (setq location (march-defaults)))
  (setq *march-parameter-block* (* *march-download-end* 8))
  (let ((x (lisp:/ *march-parameter-block* 4)))
    (lam:k-mem-write-word-address (+ x Virtual-address-original-user1)
				  (* 2 (nc:ncompiled-function-starting-address (nc:nsymbol-function func1))))
    (lam:k-mem-write-word-address (+ x Virtual-address-original-user2)
				  (* 2 (nc:ncompiled-function-starting-address (nc:nsymbol-function func2))))
    (lam:k-mem-write-word-address (+ x Length-user1)
				  (nc:ncompiled-function-length (nc:nsymbol-function func1)))
    (lam:k-mem-write-word-address (+ x Length-user2)
				  (nc:ncompiled-function-length (nc:nsymbol-function func2)))
    (lam:k-mem-write-word-address (+ x min-physical-address)
				  (* inst-block-size-in-bytes 16.))
    (lam:k-mem-write-word-address (+ x max-physical-address)
				  (* inst-block-size-in-bytes 100.))	;*** set to memory size
    (lam:k-mem-write-word-address (+ x march-pass-counter) 0)
    (lam:k-mem-write-word-address (+ x halt-page-vadr)
				  (lisp:/ (* inst-block-size-in-bytes 15.) 4))
    
    (lam:k-mem-write-word-address (+ x physical-address-1) (lisp:nth 0 location))
    (lam:k-mem-write-word-address (+ x physical-address-2) (lisp:nth 1 location))
    
    (lam:k-mem-write-word-address (+ x Copy-location-user1) (lisp:nth 2 location))
    (lam:k-mem-write-word-address (+ x Copy-location-user2) (lisp:nth 3 location))
    ))

(defun print-block-fast (&optional (adr *march-parameter-block*))
    (let* ((x (lisp:/ adr 4))
	   (aa (+ x march-pass-counter))
	   (bb (+ x  physical-address-1))
	   (cc (+ x  physical-address-2))
	   (dd (+ x Copy-location-user1))
	   (ee (+ x Copy-location-user2)))

    (do ((a (lam:k-mem-read-word-address aa) (lam:k-mem-read-word-address aa))
	 (b (lam:k-mem-read-word-address bb) (lam:k-mem-read-word-address bb))
	 (c (lam:k-mem-read-word-address cc) (lam:k-mem-read-word-address cc))
	 (d (lam:k-mem-read-word-address dd) (lam:k-mem-read-word-address dd))
	 (e (lam:k-mem-read-word-address ee) (lam:k-mem-read-word-address ee))
	 (ao -1 a)
	 (bo nil b)
	 (co nil c)
	 (do nil d)
	 (eo nil e))
	(())
      (when (= ao a)
	(do ((new-a  (lam:k-mem-read-word-address aa) (lam:k-mem-read-word-address aa)))
	    ((not (= ao new-a)) (zl:format t "Count Changed to: ~x~%" new-a))))
      (when (= e #xffffffff)
	(zl:format t "   OLD~%count: ~x~%Phys-add-1: ~x~%Phys-add-2: ~x~%Copy-loc1: ~x~%Copy-loc2: ~X~%" ao bo co do eo)
	(zl:format t "   NEW~%count: ~x~%Phys-add-1: ~x~%Phys-add-2: ~x~%Copy-loc1: ~x~%Copy-loc2: ~X~%" a b c d e)
	(do ()
	    ((not (= #xffffffff (lam:k-mem-read-word-address aa))))))
      (zl:format t "count: ~x~%Phys-add-1: ~x~%Phys-add-2: ~x~%Copy-loc1: ~x~%Copy-loc2: ~X~%" a b c d e))))

(defun march-print-parameter-block (&optional (adr *march-parameter-block*))
    (let ((x (lisp:/ adr 4)))
      (lisp:format t "~&~2d Virtual-address-original-user1 ~x"
		   Virtual-address-original-user1
		   (lam:k-mem-read-word-address (+ x Virtual-address-original-user1)))
      (lisp:format t "~&~2d Virtual-address-original-user2 ~x"
		   Virtual-address-original-user2
		   (lam:k-mem-read-word-address (+ x Virtual-address-original-user2)))
      (lisp:format t "~&~2d Length-user1 ~x (* 64 bits)"
		   Length-user1
		   (lam:k-mem-read-word-address (+ x Length-user1)))
      (lisp:format t "~&~2d Length-user2 ~x"
		   Length-user2
		   (lam:k-mem-read-word-address (+ x Length-user2)))
      (lisp:format t "~&~2d min-physical-address ~x"
		   min-physical-address
		   (lam:k-mem-read-word-address (+ x min-physical-address)))
      (lisp:format t "~&~2d max-physical-address ~x"
		   max-physical-address
		   (lam:k-mem-read-word-address (+ x max-physical-address)))
      (lisp:format t "~&~2d march-pass-counter ~x"
		   march-pass-counter
		   (lam:k-mem-read-word-address (+ x march-pass-counter)))
      (lisp:format t "~&~2d halt-page-vadr ~x"
		   halt-page-vadr
		   (lam:k-mem-read-word-address (+ x halt-page-vadr)))
      
      (lisp:format t "~&~2d physical-address-1 ~x (word adr: ~x)"
		   physical-address-1
		   (lam:k-mem-read-word-address (+ x physical-address-1))
		   (ash (lam:k-mem-read-word-address (+ x physical-address-1)) -2))
      (lisp:format t "~&~2d physical-address-2 ~x (word adr: ~x)"
		   physical-address-2
		   (lam:k-mem-read-word-address (+ x physical-address-2))
		   (ash (lam:k-mem-read-word-address (+ x physical-address-2)) -2))
      
      (lisp:format t "~&~2d Copy-location-user1 ~x"
		   Copy-location-user1
		   (lam:k-mem-read-word-address (+ x Copy-location-user1)))
      (lisp:format t "~&~2d Copy-location-user2 ~x"
		   Copy-location-user2
		   (lam:k-mem-read-word-address (+ x Copy-location-user2)))
      (lisp:format t "~&vadr delta ~x" (- (lam:k-mem-read-word-address (+ x Copy-location-user2))
					  (lam:k-mem-read-word-address (+ x Copy-location-user1))))
      ))

(defun read-march-pass ()
  (lam:k-mem-read (+ *march-parameter-block* (* 4 march-pass-counter))))

;;; @@@ for now run this by hand once, later, do in K itself  <14-Nov-88 wkf>
(defun march-init-map ()
  (let* ((adr (lisp:/ (* 15. map-page-size-in-bytes) 4))
	 (map-entry (+ #x8f (logand (lognot (- map-page-size-in-bytes 1)) adr))))
    (do ((map-index #x8000 (+ map-index 1)))
	((= map-index #x10000))
      (lam:k-write-memory-map map-index map-entry))))

(defun march-store-halts ()
  (let ((adr (lam:k-mem-read (+ *march-parameter-block* (* 4 halt-page-vadr)))))
    (dotimes (i 1024.)
      (lam:k-mem-write-word-address (+ adr i) -1))))

(defvar march-last-location)
(defvar march-user-1)
(defvar march-user-2)

(defun march-restart ()
  (vc-clear-k)
  (march-store-parameter-block march-user-1 march-user-2 march-last-location)
  (lam:falcon-set-pc  (vc-test-entry 'march-driver)
			     :do-init t
			     :do-initialize-call-hardware t
			     :do-init-virtual-memory nil
			     :memory-control-register #x40000)  
  (lam:falcon-stop-clearing-spy-mode))

;;stolen from vc-test-driver
(defun run-march (user-func-1 user-func-2 &optional location)
  (setq march-user-1 user-func-1)
  (setq march-user-2 user-func-2)
  (setq march-last-location location)
  (setq k-kbug:*loaded-functions* nil)
  (unless (nc:nsymbol-function user-func-1)
    (lisp:error "~&~s is has not been assembled. (Are you looking in the li: package?)" user-func-1))
  (unless (nc:nsymbol-function user-func-2)
    (lisp:error "~&~s is has not been assembled. (Are you looking in the li: package?)" user-func-2))
  (vc-clear-k)
  (setq k-kbug:*code-start* 0)		;read-inst-physical-with-offset (in turn disassemble) looks at this.
  (vc-load-global-constant-frame)
  (vc-download-trap-handlers)
  (march-link-and-download-test-code user-func-1 user-func-2)
  (lam:write-inst k-test-entry-loc
		  (nc:assemble-inst `(k:jump , (vc-test-entry 'march-driver) nil)))
  (lam:write-inst k-test-pass-exit-loc (nc:assemble-inst `(k:jump ,k-test-pass-halt-loc nil)))
  (lam:write-inst k-test-fail-exit-loc (nc:assemble-inst `(k:jump ,k-test-fail-halt-loc nil)))

  (march-initialize-memory-map)


  (march-store-parameter-block user-func-1 user-func-2 location)
  (march-store-halts)
  (lam:k-mem-write-word-address k-test-word-arg0-loc (lisp:/ *march-parameter-block* 4))

  (lam:falcon-set-pc  (vc-test-entry 'march-driver)
			     :do-init t
			     :do-initialize-call-hardware t
			     :do-init-virtual-memory nil
			     :memory-control-register #x40000)  ;disable prom and traps off.
  (lam:falcon-stop-clearing-spy-mode)
;  (do ()
;      ((k-kbug:k-halted-p)
;       (lisp:format t "~&halted"))
;    (lisp:format t "~&pass ~d; " (lam:k-mem-read (+ *march-parameter-block* (* 4 march-pass-counter))))
;    (lisp:sleep 1))

  (k-kbug:kbug)
  )

(defun march-roundup (n)
  (* (ceiling n (lisp:/ inst-block-size-in-bytes 8)) (lisp:/ inst-block-size-in-bytes 8)))

(defun march-link-and-download-test-code (test1 test2)
  (let ((starting-address 0))
  ;link test code
    (setq starting-address (vc-link-and-increment 'k-test-setup starting-address))	;must be first.
    (setq starting-address (vc-link-and-increment 'march-driver starting-address))
    (dolist (fctn (lisp:get 'march-driver 'vc-support-functions))
      (when (null (nc:nsymbol-function fctn))
	(lisp:error "Test ~s requires support function ~s." 'march-driver fctn))
      (setq starting-address (vc-link-and-increment fctn starting-address)))
    (dolist (fctn (lisp:get test1 'vc-support-functions))
      (when (null (nc:nsymbol-function fctn))
	(lisp:error "Test ~s requires support function ~s." test1 fctn))
      (setq starting-address (vc-link-and-increment fctn starting-address)))
    (setq starting-address (march-roundup starting-address))
    (setq starting-address (vc-link-and-increment test1 starting-address))
    (setq starting-address (march-roundup starting-address))
    (setq starting-address (vc-link-and-increment test2 starting-address))
    (setq *march-download-end* starting-address)
    )
  (let ((starting-address 0))
  ;link again and download
    (setq starting-address (vc-link-and-increment-and-download 'k-test-setup starting-address))
    (setq starting-address (vc-link-and-increment-and-download 'march-driver starting-address))
    (dolist (fctn (lisp:get 'march-driver 'vc-support-functions))
      (setq starting-address (vc-link-and-increment-and-download fctn starting-address)))
    (dolist (fctn (lisp:get test1 'vc-support-functions))
      (setq starting-address (vc-link-and-increment-and-download fctn starting-address)))
    (setq starting-address (march-roundup starting-address))
    (setq starting-address (vc-link-and-increment-and-download test1 starting-address))
    (setq starting-address (march-roundup starting-address))
    (setq starting-address (vc-link-and-increment-and-download test2 starting-address)))
  (setq *last-test-downloaded* test1))

;initialize data and instruction map for 16 instruction blocks (distance covered by jump inst) worth of space
(defun march-initialize-memory-map ()
  (let ((npages (* 4 16.)))
    (dotimes (i npages)
      (lam:falcon-write-map-and-check i (+ (* i map-page-size-in-bytes) #x8f))
      (lam:falcon-write-map-and-check (+ #x8000 i) (+ (* i map-page-size-in-bytes) #x8f)))))


(defafun mtest1 ()
;  (move a1 gr:*trap-temp2*)
;  (movei a2 1)
;  (alu l+r a3 a1 a2)
;  (move gr:*trap-temp2* a3)
  (return a0))

(defafun mtest2 ()
;  (move a1 gr:*trap-temp2*)
;  (movei a2 2)
;  (alu l-r a3 a1 a2)
;  (move gr:*trap-temp2* a3)
  (return a0))
