;;;-*- Mode:LISP; Package:K2; Base:10; Readtable:CL -*-
;;; This is a wimp repl loop, with READ, EVAL, and PRINT.
;;; 2-Jul-87 13:31:40 -george carrette

;;; This needs only
;;;  kbug-stream-read-character 
;;;  kbug-stream-write-character
;;;  WARM-INTERN, MAKE-STRING, SVREF

;;; No state is kept in global variables, this makes it a lot easier to debug
;;; the lisp implementation by setting breakpoints and looking at argument and local
;;; registers only.


(defun wimp-repl ()
  (do ((input-form)
       (result)
       (took-time))
      (nil)
    (wimp-prompt)
    (setq input-form (wimp-read))
    (wimp-write-char #\return)
    (HW:WRITE-MICROSECOND-CLOCK (HW:UNBOXED-CONSTANT 0))
    (setq result (wimp-eval input-form))
    (SETQ TOOK-TIME (HW:LDB (HW:READ-MICROSECOND-CLOCK) %%FIXNUM-FIELD 0))
    (WIMP-WRITE-STRING ";Evaluation took ")
    (wimp-print took-time)
    (wimp-write-string " microseconds.")
    (wimp-write-char #\return)
    (wimp-print result)))

(defun wimp-write-char (c)
  (select-processor
    (:lambda (write-char c))
    (:k
      (k2::kbug-stream-write-character KBUG-K-OUTPUT-STREAM
						 c))))

(defun wimp-read-char ()
  (let ((c (select-processor
	     (:lambda
	       (read-char))
	     (:k
	       (k2::kbug-stream-read-character KBUG-K-INPUT-CHARACTER-STREAM)))))
    (wimp-write-char c)
    c))

(defun wimp-prompt ()
  (wimp-write-char #\return)
  (wimp-write-char #\>))

(defun wimp-read ()
  ;; (peek-stack-pointer stack1 stack2 token-buffer)
  (wimp-read-1 (cons:cons 0
			  (cons:cons nil
				     (cons:cons nil
						(cons:cons (array:make-string 8)
							   nil))))))
(defun wimp-read-1 (state)
  (let ((token (wimp-read-token state)))
    (cond ((eq token #\()
	   (wimp-read-paren state))
	  ((eq token #\))
	   (wimp-read-error "The special token GLOBAL:CLOSE was read in at top level."))
	  ((eq token #\.)
	   (wimp-read-error "The special token SI:CONSING-DOT was read in at top level."))
	  ((eq token #\')
	   (cons:cons 'quote (cons:cons (wimp-read-1 state) nil)))
	  ('else
	   token))))

(defun wimp-read-paren (state)
  (let ((token (wimp-read-token state)))
    (cond ((eq token #\))
	   nil)
	  ((eq token #\.)
	   (wimp-read-error "A dot was read before any list was accumulated."))
	  ('else
	   (wimp-unread-token token state)
	   (cons:cons (wimp-read-1 state)
		 (wimp-read-paren-rest state))))))

(defun wimp-read-paren-rest (state)
  (let ((token (wimp-read-token state)))
    (cond ((eq token #\))
	   nil)
	  ((eq token #\.)
	   (setq token (wimp-read-1 state))
	   (or (eq (wimp-read-token state) #\))
	       (wimp-read-error "Missing close paren after dot."))
	   token)
	  ('else
	   (wimp-unread-token token state)
	   (cons:cons (wimp-read-1 state)
		 (wimp-read-paren-rest state))))))
  
(defun wimp-read-error (mess)
  (wimp-write-char #\return)
  (wimp-write-string ">>ERROR: ")
  (wimp-write-string mess)
  (wimp-write-char #\return)
  mess)

(defun wimp-unread-token (token state)
  (cond ((eq (cons:car state) 0)
	 (cons:rplaca state 1)
	 (cons:rplaca (cons:cdr state) token))
	((eq (cons:car state) 1)
	 (cons:rplaca state 2)
	 (cons:rplaca (cons:cddr state) token))))

(defun wimp-read-token (state)
  (cond ((eq (cons:car state) 2)
	 (cons:rplaca state 1)
	 (cons:caddr state))
	((eq (cons:car state) 1)
	 (cons:rplaca state 0)
	 (cons:cadr state))
	('else
	 (do ((c))
	     ((not (setq c (wimp-read-char)))
	      'eof)
	   (cond ((wimp-whitespacep c))
		 ((wimp-macrop c)
		  (return c))
		 ('else
		  (return (wimp-token-loop state c))))))))

(defun wimp-token-loop (state c)
  (let ((buffer (cons:cadddr state))
	(n (array:length (cons:cadddr state)))
	(len 1))
    (setf (array:svref buffer 0) c)
    (do ()
	((not (setq c (wimp-read-char)))
	 (wimp-intern-c buffer len 'eof state))
      (cond ((wimp-macrop c)
	     (return (wimp-intern-c buffer len c state)))
	    ((wimp-whitespacep c)
	     (return (wimp-intern buffer len)))
	    ('else
	     (cond ((= len n)
		    (setq n (+ n n))
		    (setq buffer (wimp-copy-buffer buffer n))
		    (cons:rplaca (cons:cdddr state) buffer)))
	     (setf (array:svref buffer len) c)
	     (incf len))))))


(defun wimp-copy-buffer (old n)
  (do ((m (array:length old))
       (new (array:make-string n))
       (j 0 (1+ j)))
      ((= j m)
       new)
    (setf (array:svref new j) (array:svref old j))))

	       

(defun wimp-whitespacep (c)
  (or (eq c #\return)
      (eq c #\space)
      (eq c #\tab)
      (eq c #\form)))

(defun wimp-macrop (c)
  (or (eq c #\')
      (eq c #\()
      (eq c #\))))


(defun wimp-intern-c (st n token state)
  (wimp-unread-token token state)
  (wimp-intern st n))


(defun wimp-intern (st n)
  (cond ((and (= n 1)
	      (eq (array:svref st 0) #\.))
	 #\.)
	('else
	 ;; check for all numeric later.
	 (do ((j 0 (1+ j)))
	     ((= j n)
	      (warm-intern (wimp-substring st 0 n)
			   "K2"))
	   (when (eq #\: (array:svref st j))
	     (return (warm-intern (wimp-substring st (1+ j) n)
				  (wimp-substring st 0 j))))))))
	 
(defun wimp-substring (old start end)
  ;; since I'm being stingy I should really have a resource of these
  ;; strings. but more usual is to have your low-level INTERN
  ;; function take a length argument.
  (do ((new (array:make-string (- end start)))
       (j start (1+ j))
       (k 0 (1+ k)))
      ((= j end)
       new)
    (setf (array:svref new k) (array:svref old j))))

    
(defun wimp-print (x)
  (cond ;((characterp x)
	; (wimp-write-char #\#)
	; (wimp-write-char #\\)
	; (wimp-write-char x))
	((symbol:symbolp x)
	 (wimp-write-string (symbol:symbol-name x)))
	((wimp-fixnump x)
	 (cond ((< x 0)
		(wimp-write-char #\-)
		(wimp-print-pos-fixnum (- x)))
	       ('else
		(wimp-print-pos-fixnum x))))
	((wimp-atom x)
	 (wimp-write-string "#<UNKNOWN ATOM>"))
	('else
	 (wimp-write-char #\()
	 (wimp-print (cons:car x))
	 (wimp-print-tail (cons:cdr x)))))

(defun wimp-print-pos-fixnum (x)
  (multiple-value-bind (quo rem)
      (wimp-div10 x)
    (or (= quo 0)
	(wimp-print-pos-fixnum quo))
    (wimp-write-char (array:svref "0123456789" rem))))
    
(defun wimp-div10 (x)
  (do ((rem x (- rem 10))
       (quo 0 (1+ quo)))
      ((< rem 0)
       (values (1- quo) (+ rem 10)))))


(defun wimp-print-tail (x)
  (cond ((null x)
	 (wimp-write-char #\)))
	((wimp-atom x)
	 (wimp-write-string " . ")
	 (wimp-print x)
	 (wimp-write-char #\)))
	('else
	 (wimp-write-char #\space)
	 (wimp-print (cons:car x))
	 (wimp-print-tail (cons:cdr x)))))

(defun wimp-write-string (x)
  (do ((j 0 (1+ j))
       (n (array:length x)))
      ((= j n))
    (wimp-write-char (array:svref x j))))


(defun wimp-eval (x)
  (cond ((symbol:symbolp x)
	 (cond ((symbol:boundp x)
		(symbol:symbol-value x))
	       ('else
		x)))
	((wimp-atom x)
	 x)
	((eq (cons:car x) 'K2::cons)
	 (cons:cons (wimp-eval (cons:cadr x))
		    (wimp-eval (cons:caddr x))))
	((eq (cons:car x) 'K2::quote)
	 (cons:cadr x))
	((eq (cons:car x) 'K2::car)
	 (wimp-car (wimp-eval (cons:cadr x))))
	((eq (cons:car x) 'K2::cdr)
	 (wimp-cdr (wimp-eval (cons:cadr x))))
	((eq (cons:car x) 'K2::list)
	 (wimp-eval-args (cons:cdr x)))
	((eq (cons:car x) 'K2::setq)
	 (symbol:set (cons:cadr x) (wimp-eval (cons:caddr x))))
	((EQ (CONS:CAR X) 'K2::+)
	 (WIMP-PLUS (WIMP-EVAL-ARGS (CONS:CDR X))))
	((EQ (CONS:CAR X) 'K2::-)
	 (WIMP-DIFFERENCE (WIMP-EVAL-ARGS (CONS:CDR X))))
	('else
	 (cons:cons (cons:car x) (wimp-eval-args (cons:cdr x))))))

(defun wimp-car (x)
  (if (wimp-atom x) (cons:cons 'K2::car (cons:cons x nil)) (cons:car x)))

(defun wimp-cdr (x)
  (if (wimp-atom x) (cons:cons 'K2::cdr (cons:cons x nil)) (cons:cdr x)))
  

(defun wimp-eval-args (l)
  (cond ((wimp-atom l)
	 l)
	('else
	 (cons:cons (wimp-eval (cons:car l))
		    (wimp-eval-args (cons:cdr l))))))


(defun wimp-atom (x)
  (not (hw:field= gr:*dtp-cons* x vinc:%%data-type)))

(defun wimp-fixnump (x)
  (hw:field= gr:*zero* x vinc:%%data-type))

(DEFUN WIMP-PLUS (L)
  (DO ((ACC 0)
       (K NIL)
       (V L (cons:CDR L)))
      ((NULL V)
       (COND ((NULL K)
	      ACC)
	     ('ELSE
	      (CONS:CONS 'K2::+
			 (CONS:CONS ACC K)))))
    (COND ((WIMP-FIXNUMP (CONS:CAR V))
	   (SETQ ACC (+ (CONS:CAR V) ACC)))
	  ('ELSE
	   (SETQ K (CONS:CONS (CONS:CAR V) K))))))


(DEFUN WIMP-DIFFERENCE (L)
  (DO ((ACC 0)
       (K NIL)
       (V L (cons:CDR L)))
      ((NULL V)
       (COND ((NULL K)
	      ACC)
	     ('ELSE
	      (CONS:CONS 'K2::-
			 (CONS:CONS ACC K)))))
    (COND ((WIMP-FIXNUMP (CONS:CAR V))
	   (SETQ ACC (- (CONS:CAR V) ACC)))
	  ('ELSE
	   (SETQ K (CONS:CONS (CONS:CAR V) K))))))