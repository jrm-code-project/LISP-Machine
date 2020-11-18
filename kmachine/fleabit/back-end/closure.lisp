;;; -*- Mode:LISP; Package:NC; Base:10; Readtable:CL -*-

;;; Copyright (c) 1985 Yale University
;;;     Authors: N Adams, R Kelsey, D Kranz, J Philbin, J Rees.
;;; This material was developed by the T Project at the Yale University Computer
;;; Science Department.  Permission to copy this software, to redistribute it,
;;; and to use it for any purpose is granted, subject to the following restric-
;;; tions and understandings.
;;; 1. Any copy made of this software must include this copyright notice in full.
;;; 2. Users of this software agree to make their best efforts (a) to return
;;;    to the T Project at Yale any improvements or extensions that they make,
;;;    so that these may be included in future releases; and (b) to inform
;;;    the T Project of noteworthy uses of this software.
;;; 3. All materials developed as a consequence of the use of this software
;;;    shall duly acknowledge such use, in accordance with the usual standards
;;;    of acknowledging credit in academic research.
;;; 4. Yale has made no warrantee or representation that the operation of
;;;    this software will be error-free, and Yale is under no obligation to
;;;    provide any services, by way of maintenance, update, or otherwise.
;;; 5. In conjunction with products arising from the use of this material,
;;;    there shall be no use of the name of the Yale University nor of any
;;;    adaptation thereof in any advertising, promotional, or sales literature
;;;    without prior written consent from Yale in each case.
;;;

;;; Copyright (c) 1985 David Kranz



;;; Closure analysis.
;;;=========================================================================


;;; Environment structure is the lambda-env slot of each lambda which is
;;; strategy/stack or strategy/heap. The variables are sorted by size.
;;; (For stack closures) a continuation is represented as offset -1 in the
;;;  a-list.


(defstruct (environment (:print-function (lambda (struct stream depth)
                                           depth
                                           (format stream "#{Environment_~D in Closure_~D}"
                                                   (object-hash struct)
                                                   (object-hash (environment-closure struct))))))
  closure    ; the closure this environment is a member of
  cic-offset ; offset of this environment's descriptor in the closure
  )

(defstruct (closure (:print-function (lambda (struct stream depth)
                                       depth
                                       (format stream "#{Closure with ~D vars, cics ~S ~D}"
                                               (length (closure-env struct))
                                               (mapcar #'variable-unique-name
                                                       (closure-members struct))
                                               (object-hash struct))))
                    (:constructor make-closure ()))
  members     ; list of closure-internal-closures (variables)
  vframe-lambdas
  env         ; a-list of variables and offsets in the closure (in bytes)
  pointer     ; number of pointer slots
  scratch     ; number of scratch slots
  size        ; total size of closure (in bytes)
  cit-offset  ; offset of first
  link        ; superior closure
  )





(defun close-analyze-top (node variables)
    (setq *unit-closures* nil)
    (setq *unit-templates* nil)
    (let* ((l (call-arg-n 1 (lambda-body node)))
           (env (list (lambda-self-var node)))
           (via (lambda-self-var l)))
      (close-analyze-body (lambda-body l) env via env via)
      (setq *unit* (create-unit))
      (create-environment l *unit* 16)
      (values (cdr (closure-env *unit*)) *unit-templates* l))) ; skip the
                                                               ; *environment*

(defun close-analyze-body (node  senv svia henv hvia)
  (cond ((and (primop-node? (call-proc node))
              (eq (primop-value (call-proc node)) primop/Y))
         (really-close-analyze-body
                       (call-args (lambda-body (call-arg-n 1 node)))
                       senv svia henv hvia))
        (t
         (really-close-analyze-body (call-proc+args node)
                                    senv svia henv hvia))))


(defun really-close-analyze-body (nodes senv svia henv hvia)
  (multiple-value-bind (live cics vframe)
      (accumulate-environment nodes senv svia henv hvia)
    (if cics (close-analyze-heap cics live henv hvia))
    (if vframe (close-analyze-vframe vframe hvia svia henv senv))))


(defun close-analyze-heap (cics live henv hvia)
  (let* ((cic-vars (mapcar #'lambda-self-var cics))
         (live (set-difference live cic-vars))
         (link (if (or (intersection live henv)
                       (member hvia live :test #'eq)
                       (some #'(lambda (node)
                                 (eq (lambda-env node) 'unit-internal-closure))
                             cics))
                       hvia
                       nil))
         (delta (set-difference (delete hvia live :test #'eq) henv)))
    (ecase (lambda-strategy (variable-binder hvia))
      (STRATEGY/HEAP
       (create-closure link cic-vars delta nil 'heap))
      (STRATEGY/VFRAME
       (create-closure nil cic-vars (delete hvia live :test #'eq) nil 'heap)))
    (dolist (cic cics)
      (cond ((object-lambda? cic)
             (destructure (((nil proc nil . methods)
                            (call-args (lambda-body cic))))
               (dolist (method (cons proc methods))
                 (setf (lambda-env method)
                       (copy-structure (lambda-env cic)))    ;copy-environment (put :copier in defstruct)
                 (close-analyze-body (lambda-body method)
                                     live
                                     (lambda-self-var cic)
                                     live
                                     (lambda-self-var cic)))))
            (t
             (close-analyze-body (lambda-body cic)
                                 live
                                 (lambda-self-var cic)
                                 live
                                 (lambda-self-var cic)))))))



(defun close-analyze-vframe (vframe hvia svia henv senv)
  (let* ((live (do ((vframe vframe (cdr vframe))
                    (live '() (union live (lambda-live (car vframe)))))
                   ((null vframe) (delete hvia live :test #'eq))))
         (link (if (or (intersection live senv)
                       (some #'(lambda (node)
                                 (eq (lambda-env node) 'needs-link))
                             vframe))
                   svia
                   nil))
         (via (if (and link (eq svia hvia)) hvia nil)))
    (ecase (lambda-strategy (car vframe))
      (STRATEGY/EZCLOSE
       (create-closure via nil (set-difference live senv) vframe 'ezclose)
       (dolist (cic vframe)
         (close-analyze-body (lambda-body cic)
                             live
                             (lambda-self-var cic)
                             henv
                             hvia)))
      (STRATEGY/VFRAME
       (let* ((contour (lambda-self-var (node-parent
                          (node-parent (car vframe)))))
              (xlive (delete contour live :test #'eq)))
         (if (or link xlive)
             (create-closure via nil (set-difference xlive senv) vframe 'vframe)
             (dolist (cic vframe) (setf (lambda-env cic) nil)))
         (dolist (cic vframe)
           (close-analyze-body (lambda-body cic)
                               xlive
                               contour
                               xlive
                               contour)))))))



;;; (proc+handler k object-proc method-names . methods)
;;; Must hack this by not returning the proc as a cic.  The parent lambda will
;;; masquerade as the proc until code generation

(defun accumulate-environment (nodes senv svia henv hvia)
  (let  ((live '()) (cics '()) (vframe '()))
    (dolist (node nodes)
      (when (lambda-node? node)
        (ecase (lambda-strategy node)
          (STRATEGY/HEAP
           (cond ((object-lambda? node)
                  (let* ((args (cdddr (call-args (lambda-body node))))
                         (new-cics (close-analyze-object node args)))
                    (setq live (union (lambda-live node) live))
                    (setq cics (union new-cics cics))))
                 ((eq (lambda-env node) 'unit-internal-closure)
                  (push node *unit-closures*)
                  (let ((env (lambda-live node))
                        (via (lambda-self-var node)))
                    (close-analyze-body (lambda-body node)
                                        env via env via)
                    (setq live (union env live))))
                 (t
                  (setq live (union (lambda-live node) live))
                  (setq cics (adjoin node cics)))))
             (STRATEGY/OPEN
              (close-analyze-body (lambda-body node) senv svia henv hvia))
             (STRATEGY/LABEL
              (close-analyze-label node senv svia henv hvia))
             (STRATEGY/STACK
              (close-analyze-stack node senv svia henv hvia))
             (STRATEGY/EZCLOSE
              (push node vframe))
             (STRATEGY/VFRAME
              (push node vframe)))))
    (values live cics vframe)))


#|||||||
(define (close-analyze-object obj methods)
  (cond ((null? (lambda-live obj))
         (let ((proc (cadr (call-args (lambda-body obj)))))
           (push *unit-closures* obj)
           (let ((env (lambda-live obj))
                 (via (lambda-self-var obj)))
             (close-analyze-body (lambda-body proc) env via env via)
             (walk (lambda (node)
                     (push *unit-closures* node)
                     (let ((env (lambda-live node))
                           (via (lambda-self-var (node-parent (node-parent node)))))
                       (close-analyze-body (lambda-body node) env via env via)))
                    methods)))
           '())
        (else
         (list obj))))
||||||||#



(defun close-analyze-stack (node stackenv stackvia heapenv heapvia)
  (let* ((live (lambda-live node))
         (h (variable-binder heapvia))
         (link-set (if (or (intersection live stackenv)
                           (and (member heapvia live :test #'eq)
                                (or (not (eq (lambda-strategy h) strategy/vframe))
                                    (lambda-env h)))    ; hack
                           (eq (lambda-env node) 'needs-link))
                       `(,stackvia)
                       '()))
         ;; the closure env is all the live vars that are not already on the stack
         ;; really it can remove the live vars that are in a regs???
         ;; maybe all the live vars are in a regs? and argument forms don't munge a regs right??
         ;; and if we don't clear a regs we won't munge them??? so what if we just put nil here???
         ;; ie there isn't anything in the env (ie there aren't really stack closures...)
         ;; fetch-from-heap looks in closure-env for vars... but it should be in reg so access-value
         ;; should get it first, but conditionals and some other things kill regs....
         ;; efh
         (closure-env nil)) ;(delete heapvia (set-difference live stackenv) :test #'eq)))
;    (format t "~%close-analyze-stack stackenv: ~s  stackvia: ~s" stackenv stackvia)
    (create-closure ;;P reg, gets into closure-ev too
                    (if (and link-set (eq stackvia heapvia)) heapvia nil)
                    (list (lambda-self-var node))
                    closure-env
                    nil
                    'stack)
    (close-analyze-body (lambda-body node)
                        nil   ;live   ;things on stack
                        (lambda-self-var node)
                        heapenv heapvia)))






(defun close-analyze-label (node stackenv stackvia heapenv heapvia)
  (let* ((live (lambda-live node))
         (h (variable-binder heapvia))
         (delta (delete heapvia (set-difference live heapenv) :test #'eq))
         (need-contour? (or (intersection live heapenv)
                            (and (member heapvia live :test #'eq)
                                 (or (not (eq (lambda-strategy h) strategy/vframe))
                                     (lambda-env h)))          ; hack
                            (eq (lambda-env node) 'needs-link))))
    (setf (lambda-env node)
          (create-join-point delta
                             heapvia
                             need-contour?))
    (orbit-debug "join-point (~D) ~a env = ~S~%"
           (object-hash node)
           (lambda-name node)
           (mapcar #'variable-name delta))
    (close-analyze-body (lambda-body node)
                        heapenv
                        heapvia
                        heapenv heapvia)))





(defstruct join-point
  env                  ;;; free variables
  arg-specs            ;;; list of numbers for argument-positions
  global-registers     ;;; list of (register . variable)
  contour              ;;; nearest superior template
  contour-needed?
  )

(defun create-join-point (env contour needed?)
  (let ((j (make-join-point)))
    (setf (join-point-env j) env)
    (setf (join-point-arg-specs j) nil)
    (setf (join-point-global-registers j) 'not-yet-determined)
    (setf (join-point-contour-needed? j) needed?)
    (setf (join-point-contour j) contour)
    j))


(defstruct loc-list        ;;; appears in the unit
  var
  )


(defun create-loc-list (var)
  (let ((l (make-loc-list)))
    (setf (loc-list-var l) var)
    l))


(defun create-unit ()
 (let ((unit (make-closure)))
   (multiple-value-bind (a-list count) (do-unit-variables unit)
     (do ((lits *unit-literals* (cdr lits))
          (count count (+ count CELL))
          (a-list a-list `((,(car lits) . ,count) ,@a-list)))
       ((null lits)
        (do ((closures *unit-closures* (cdr closures))
             (count count (+ count CELL))
             (a-list a-list `((,(car closures) . ,count) ,@a-list)))
            ((null closures)
             (do ((templates *unit-templates* (cdr templates))
                  (count count (+ count (* CELL 3)))
                  (a-list a-list `((,(car templates) . ,(+ count CELL)) ,@a-list)))
                 ((null templates)
                  (setf (closure-pointer unit) (- (/ count CELL) 1))
                  (setf (closure-scratch unit) 0)
                  (setf (closure-env unit)  (nreverse a-list))
                  (setf (closure-cit-offset unit) nil)
                  unit)
               (setf (closure-cit-offset (car templates)) (+ count CELL))))
          (create-environment (car closures) unit count)))))))

(defvar *the-environment* (create-variable '*the-environment*))


(defun do-unit-variables (unit)
; header 0
; id 4
; filename 8
; env 12
; thing 16
  (let ((a-list `((,*the-environment* . 12)) )
        (count 20))
    (dolist (var (delete *the-environment* *unit-variables* :test #'eq))
      (cond ((unit-var-needs-vcell? var)
             (setq a-list `(,(cons var (+ count cell))
                            ,(cons (create-loc-list var) count)
                            ,@a-list))
             (incf count (* CELL 2)))
            (t
             (setq a-list `(,(cons var count) ,@a-list))
             (incf count CELL))))
    (values a-list count)))



(defun unit-var-needs-vcell? (var)
  (if (defined-variable? var)
      t
    (let ((variant (defined-variable-variant var)))
      (if variant
          (member variant '(set lset))))))



(defun create-env-a-list (pointer scratch)
  (do ((vars `(,@pointer . ,(sort scratch #'scratch-compare)) (cdr vars))
       (count 0 (+ count (rep-size (variable-rep (car vars)))))
       (a-list '() `((,(car vars) . ,count) . ,a-list)))
      ((null vars)
       (nreverse a-list))))

(defconstant *dummy-var* (create-variable '*dummy-var*))

(defun create-closure (link cics vars vframe-lambdas strategy)
  (let ((closure (make-closure)))
    (if (eq strategy 'heap)
        (mapc #'cell-collapse vars)
        (mapc #'(lambda (var) (setf (variable-support var) 'many)) vars))
    (multiple-value-bind (pointer scratch) (sort-vars vars)
      (let* ((scratch-slots (compute-scratch-slots scratch))
             (pvars (if (null (cdr cics))
                        (if link (cons link pointer) pointer)
                        (case (length pointer)
                            (0
                             (if link
                                 (list link *dummy-var*)
                                 (list *dummy-var* *dummy-var*)))
                            (1
                             (if link
                                 (list link (car pointer))
                                 (list *dummy-var* (car pointer))))
                            (t
                             (if link (cons link pointer) pointer)))))
             (pointer-slots (+ (length pvars)
                                 (if cics (length cics) 1)))
             (var-a-list (create-env-a-list
                           (if cics
                               `(,(car cics) ,@pvars ,@(cdr cics))
                               `(,*dummy-var* ,@pvars))
                           scratch)))
          (setf (closure-link closure) link)
          (setf (closure-members closure) cics)
          (setf (closure-vframe-lambdas closure) vframe-lambdas)
          (setf (closure-cit-offset closure) nil)
          (setf (closure-env        closure) var-a-list)
          (setf (closure-scratch    closure) scratch-slots)
          (setf (closure-pointer    closure) (1- pointer-slots))
          (setf (closure-size       closure)
               (* (+ scratch-slots pointer-slots) CELL))
          (if (null vframe-lambdas)
              (create-environments var-a-list closure cics)
              (create-vframe-environments closure vframe-lambdas))
          closure))))

(defun cell-collapse (var)
  (cond ((null (variable-support var))
         (setf (variable-support var) 'one))
        ((eq (variable-support var) 'one)
         (setf (variable-support var) 'many))))




(defun compute-scratch-slots (scratch)
  (do ((vars scratch (cdr vars))
       (count 0 (+ count (rep-size (variable-rep (car vars))))))
      ((null vars)
       count)))

; rep-size returns # of q's in lm
;       (ash (+ count 3) -2))))           ; bytes->longwords



(defun create-environments (var-a-list closure cics)
  (create-environment (variable-binder (car cics)) closure 0)
  (orbit-debug "~a (~d) ~s env = ~a~%" (lambda-strategy (variable-binder (car cics)))
          (object-hash (variable-binder (car cics)))
          (variable-name (car cics))
          (mapcar #'(lambda (var) (variable-name (car var)))
               (closure-env closure)))
  (mapc #'(lambda (cic)
            (create-environment (variable-binder cic)
                                closure
                                (cdr (assoc cic var-a-list :test #'eq))))
        (cdr cics)))

(defun create-vframe-environments (closure vframe-lambdas)
  (dolist (cic vframe-lambdas)
    (setf (lambda-env cic) nil))
  (orbit-debug "~a (~d) ~s env = ~a~%" (lambda-strategy (car vframe-lambdas))
          (object-hash (car vframe-lambdas))
          (variable-name (lambda-self-var (car vframe-lambdas)))
          (mapcar #'(lambda (var) (variable-name (car var)))
                  (closure-env closure)))
  (create-environment (node-parent (node-parent (car vframe-lambdas)))
                      closure 0))


(defun create-environment (node closure offset)
  (let ((env (make-environment)))
    (setf (environment-closure    env) closure)
    (setf (environment-cic-offset env) offset)
    (if (and (eq 'unit-internal-template (lambda-env node))
             (not (eq closure (car *unit-templates*))))
        (push closure *unit-templates*))
    (setf (lambda-env node) env)))

(defun sort-vars (vars)
  (let ((pointer '()) (scratch '()))
    (dolist (var vars)
      (if (eq (variable-rep (car vars)) 'rep/pointer)
          (push var pointer)
        (push var scratch)))
    (values pointer scratch)))

(defun bound-to-continuation? (var)
  (and (variable-binder var)
       (some #'(lambda (ref)
                 (let ((exits (call-exits (node-parent ref))))
                   (and (< exits 2)
                        (= (call-arg-number (node-role ref)) exits))))
             (variable-refs var))))


(defun continuation?  (node)
  (let ((lv (lambda-variables node)))
    (or (null lv)
        (null (car lv))
        (not (bound-to-continuation? (car lv))))))


(defun scratch-compare (var1 var2)
  (> (rep-size (variable-rep var1)) (rep-size (variable-rep var2))))
