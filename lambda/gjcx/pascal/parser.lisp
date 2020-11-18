;;; -*- Mode:LISP; Package:PASCAL; Readtable:CL; Fonts:(CPTFONTB); Base:10 -*-


(defvar *debug* nil)
(defvar *indent-spaces* 5)

(defvar clone-ctr 0)
(defvar cycle-ctr 0)
(defvar cycle-ctr-limit 1000000)

(defvar max-processed-states 0)
(defvar max-processed-states-limit 128)

(defvar *left-corners*     nil)
(defvar *dominated-nodes*  nil)
(defvar *processed-states* nil)
(defvar *completed-states* nil)

(defvar max-tree-depth 64)
(defvar *collapse-single-branches* t)
(defvar *debug2* t)

(defsubst rule-mother-category (rule) (first rule))

(defsubst rule-left-corner (rule) (second rule))

(defsubst find-rules-with-left-corner (lc)
  (get lc :lc-rules))

(defstruct node
  (category  nil)
  (daughters nil)
  (translation nil))

(defsubst nodes-which-dominate (n)
  (get n :dominating-nodes))

(defun princ-at-level (obj level &optional (stream standard-output))
  (terpri stream)
  (dotimes (i (* level *indent-spaces*))
    (tyo #\space stream))
  (princ obj stream))


(defun print-tree (node level)
  (when (< level max-tree-depth)
    (princ-at-level (node-category node) level)
    (when (and *collapse-single-branches*
               (= (length (node-daughters node)) 1))
      (do () ((not (= (length (node-daughters node)) 1)))
        (setq node (car (node-daughters node))))
      (format t "... ~S" (node-category node)))
    (if (and (node-translation node)
             (not (eq (node-translation node)
                      (node-category node))))
        (format t " = ~A" (node-translation node)))
    (dolist (d (reverse (node-daughters node)))
      (print-tree d (1+ level)))))


(defstruct state
  (phrase-stack nil)
  (rule-stack   nil)
  (tree-stack   nil)
  (trans-stack  nil))

(defsubst current-phrase (state) (first (state-phrase-stack state)))
(defsubst pop-phrase (state) (pop (state-phrase-stack state)))
(defsubst push-phrase (phrase state) (push phrase (state-phrase-stack state)))

(defsubst current-tree (state) (first (state-tree-stack state)))
(defsubst pop-tree (state) (pop (state-tree-stack state)))

(defsubst push-tree (mother left-corner state)
  (push
   (make-node :category  mother
              :daughters (list left-corner))
    (state-tree-stack state)))


(defsubst right-adjoin-phrase-to-tree (phrase tree)
  (push phrase (node-daughters tree)))


(defsubst current-rule-category (state)  (first (first (state-rule-stack state))))
(defsubst pop-rule (state) (pop (state-rule-stack state)))
(defsubst push-rule (rule state) (push (cddr rule) (state-rule-stack state)))
(defsubst advance-rule (state)  (pop (first (state-rule-stack state))))

(defsubst current-rule-complete? (state)
  (and (consp (state-rule-stack state))
       (null (first (state-rule-stack state)))))

(defsubst completely-parsed? (state)
  (and (null (state-tree-stack state))
       (= (length (state-phrase-stack state)) 1)
       (or (null (state-rule-stack state))
           (and (= (length (state-phrase-stack state)) 1)
                (eq (current-rule-category state)
                    (node-category (car (state-phrase-stack state))))))))

(defsubst clone-state (state)
  (incf clone-ctr)
  (make-state :phrase-stack (state-phrase-stack state)
              :rule-stack   (copy-list (state-rule-stack state))
              :tree-stack   (mapcar #'(lambda (x)
                                        (make-node :category  (node-category  x)
                                                   :daughters (node-daughters x)))
                                    (state-tree-stack state))))


(defsubst store-processed-state (state)
  (push state *processed-states*))


(defsubst possible-left-daughters (rule)
  (let ((l (car rule)))
    (if (not (consp l))
        (list l)
        (possible-left-daughters-1 rule))))


(defun possible-left-daughters-1 (rule)
  (cond ((null rule) nil)
        ((not (consp (car rule)))
         (if (not (eq (car rule) 'k*))
             (list (car rule))
             (list (cadr rule))))
        (t (append (possible-left-daughters-1 (car rule))
                   (possible-left-daughters-1 (cdr rule))))))



(defun add-a-rule (rule)
  (dolist (lc (possible-left-daughters (cdr rule)))
    (cond ((not (memq lc *left-corners*))
           (remprop lc :lc-rules)
           (putprop lc (list rule) :lc-rules)
           (push lc *left-corners*))
          (t (let ((entry  (get lc :lc-rules)))
               (if entry
                   (push rule (cdr entry))
                   (putprop lc (list rule) :lc-rules)))))))


(defun add-rule (rule-with-options)
  (let ((mother-node (car rule-with-options)))
    (dolist (daughters (rule-list-from-options (cdr rule-with-options)))
      (add-a-rule (cons mother-node daughters)))))


(defun rule-list-from-options (rule-with-options) (list rule-with-options))

;(defun rule-list-from-options (rule-with-options)
;  (let ((expanded-list       (expand-options rule-with-options nil))
;       (*flattened-options* nil))
;    (declare (special *flattened-options*))
;    (flatten-options expanded-list)
;    *flattened-options*))


(defun expand-options (input output)
  (declare (special *flattened-options*))
  (cond ((null input) (reverse output))
        ((consp (car input))
         (list (expand-options (cdr input) (append (reverse (car input)) output))
               (expand-options (cdr input) output)))
        (t (expand-options (cdr input) (cons (car input) output)))))


(defun flatten-options (list)
  (declare (special *flattened-options*))
  (cond ((null list) nil)
        ((and (consp list) (not (consp (car list))))
         (push list *flattened-options*))
        (t (flatten-options (car list))
           (flatten-options (cdr list)))))




(defsubst rule-1-applicable? (state)
  (and (current-phrase state)
       (not (current-rule-complete? state))))

(defun rule-1 (lc lc-rules state)
  (do ((rules     lc-rules (cdr rules))
       (rule      nil)
       (new-state nil))
      ((null rules))
    (setq rule (car rules))
    (if (consp (cadr rule))
        (setq rule (cons (car rule) (lc-expanded-options (cdr rule) lc))))
    (cond ((check-domination (car rule) state)
           (if *debug* (format t "~%Successfully applying rule 1 for ~S ..." rule))
           (setq new-state (clone-state state))
           (push-rule rule new-state)
           (push-tree (rule-mother-category rule)
                      (pop-phrase new-state)
                      new-state)
           (expand-rule-options new-state))
          (t (if *debug* (format t "~%UNsuccessfully applying rule 1 for ~S ..." rule))))))



(defun lc-expanded-options (options lc)
  (cond ((eq (car options) lc) options)
        ((consp (car options))
         (or (lc-expanded-options (append (car options) (cdr options)) lc)
             (lc-expanded-options (cdr options) lc)))))

(defsubst rule-2-applicable? (state)
  (and (current-phrase state)
       (current-tree state)
       (not (current-rule-complete? state))
       (eq (node-category (current-phrase state))
           (current-rule-category state))))

(defun rule-2 (state)
  (if *debug* (format t "~%Applying rule 2 to ~S..." (car (state-rule-stack state))))
  (advance-rule state)
  (right-adjoin-phrase-to-tree (pop-phrase state) (current-tree state))
  (expand-rule-options state)
  t)


(defun expand-rule-options (state)
  (do ((states-to-expand (list state)))
      ((null states-to-expand))
    (let ((st (pop states-to-expand)))
      (cond ((not (consp (current-rule-category st)))
             (store-processed-state st))
            (t (let ((option (current-rule-category st)))
                 (advance-rule st)
                 (push (clone-state st) states-to-expand)
                 (if (eq (car option) 'k*)
                     (setf (state-rule-stack st)
                           (cons (append (cdr option) (cons option (car (state-rule-stack st))))
                                 (cdr (state-rule-stack st))))
                     (setf (state-rule-stack st)
                           (cons (append option (car (state-rule-stack st)))
                                 (cdr (state-rule-stack st)))))
                 (push st states-to-expand)))))))


(defsubst rule-3-applicable? (state)
  (current-rule-complete? state))


(defun rule-3 (state)
    (if *debug* (print "Applying rule 3 ..."))
    (pop-rule state)
    (push-phrase (pop-tree state) state)
    (store-processed-state state)
    t)



(defun parse (node-list &optional target-category)
  (setq *processed-states*
        (list (make-state :phrase-stack node-list
                          :rule-stack   (if target-category
                                            (list (list target-category))
                                          nil))))
  (setq *completed-states* nil)
  (setq clone-ctr 0)
  (setq max-processed-states 0)
  (setq cycle-ctr 0)
  (setq new-node-ctr 0
        new-state-ctr 0)
  (parse-1))


(defun parse-1 (&aux temp)

  (do () ((null *processed-states*))

    (let ((states *processed-states*)
          (lc     nil)
          (rules  nil)
          (r1?    nil)
          (r2?    nil)
          (r3?    nil))
      (setq *processed-states* nil)
      (dolist (state states)
        (setq temp (current-phrase state))

        (setq lc    (if (not temp) nil (node-category temp))
              rules (find-rules-with-left-corner lc))

        (setq r1? (and rules (rule-1-applicable? state))
              r2? (rule-2-applicable? state)
              r3? (rule-3-applicable? state))

        (cond ((not (or r1? r2? r3?))
               )
              (t (if r1?
                     (rule-1 lc rules (if r2? (clone-state state) state)))
                 (if r2? (rule-2 state))
                 (if r3? (rule-3 state)))))


      (incf cycle-ctr)
      (if (or (>= cycle-ctr cycle-ctr-limit)
              (>= (length *processed-states*) max-processed-states-limit))
          (setq *debug* t))

      (when *debug*
        (when *debug2* (dolist (state *processed-states*)
                         (format t "~%Rulestack = ~S" (state-rule-stack state))
                         (dolist (tree (state-tree-stack state))
                           (print-tree tree 0))))
        (read-char)
        (send standard-output :clear-window))

      (dolist (state *processed-states*)
        (if (completely-parsed? state)
            (push state *completed-states*)))

      (setq max-processed-states
            (max max-processed-states
                 (length *processed-states*)))))

;  (dolist (state *completed-states*)
;     (print-tree (car (state-phrase-stack state)) 0))
  )



; for each node X, add the nodes which can immediately dominate it to
; the entry for X and the entries of nodes which X can dominate

(defun calculate-domination ()

  (setq *dominated-nodes* nil)

  (dolist (lc *left-corners*)
    (dolist (rule (get lc :lc-rules))
      (dolist (node (possible-left-daughters (cdr rule)))
        (make-dominated (first rule) node))))

  (let ((*calculated-nodes* nil)
        (*nodes-under-consideration* nil))
    (declare (special *calculated-nodes* *nodes-under-consideration*))
    (dolist (dnode *dominated-nodes*)
      (calculate-full-domination-for dnode)))

  )

(defun calculate-full-domination-for (node)
  (declare (special *calculated-nodes* *nodes-under-consideration*))
  (let ((d-nodes (get node :dominating-nodes)))
    (dolist (n d-nodes)
      (when (and (not (member n *calculated-nodes*))
                 (not (member n *nodes-under-consideration*)))
        (let ((*nodes-under-consideration* (cons n *nodes-under-consideration*)))
          (declare (special *nodes-under-consideration*))
          (calculate-full-domination-for n)
          (push n *calculated-nodes*)))
      (dolist (n (nodes-which-dominate n))
        (make-dominated n node)))))


(defun make-dominated (d n)
  (if (not (eq d n))
      (cond ((not (memq n *dominated-nodes*))
             (remprop n :dominating-nodes)
             (putprop n (list d) :dominating-nodes)
             (push n *dominated-nodes*))
            (t (let ((domination-entry (get n :dominating-nodes)))
                 (if domination-entry
                     (if (not (member d domination-entry))
                         (push d (cdr domination-entry)))
                     (putprop n (list d) :dominating-nodes)))))))


(defun check-domination (lc state)
  (let ((nodes (nodes-which-dominate lc)))
    (let ((rule (car (state-rule-stack state))))
      (if (and rule (not (dolist (ld (possible-left-daughters rule) nil)
                           (if (or (eq ld lc) (memq ld nodes))
                               (return t)))))
          nil
          t))))



(defmacro define-grammar (name documentation &body rules)
  `(*define-grammar ',name ,documentation ',rules))


(defun *define-grammar (name documentation rules)
  name
  documentation
  (setq *left-corners* nil)
  (dolist (rule rules)
    (add-rule rule))
  (calculate-domination))
