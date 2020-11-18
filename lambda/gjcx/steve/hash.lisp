; -*-mode:lisp; package:steve; readtable:cl; -*-

;Copyright (c) July 1983 by Christopher Eliot
; and Massachusetts Institute of Technology.
;Permission to copy all or part of this material is granted, provided
; that the copies are not made or distributed for resale, the MIT
; copyright notice and reference to the source file and the software
; distribution version appear, and that notice is given that copying
; is by permission of Massachusetts Institute of Technology.

;New binding scheme.
;There is still one issue to deal with:
; How to store Control-X commands so they may be bound in specific modes,
; and still be properly inherited from superior modes.
;

;;list of all binding tables, used for invalidating them.
(defvar *all-binding-tables* nil)

;;When non-nil any reference to a binding  table will cause them all to
;;be invalidated.
(defvar invalidate-binding-tables-on-reference nil)

(defflavor binding-table (cache bit-vector bit-vector-valid? alist
                          superior prefix-alist buffer-local?)
  ()
  ;;Cache is a vector referenced by:
  ;; (sgvref cache (coerse-to-9bit-charp char)) => the binding.
  ;;Bit-vector is 1 for every self-inserting character. It should
  ;; be changed to LOGAND self-insertion and graphic-charness.
  ;; Its use is for ECHOIN.
  ;;Bit-vector-valid? is used internally.
  ;;Alist is used internally. It is the alist of local binding definitions.
  ;; These are cached with the inherited definitions in CACHE.
  ;;Superior is the superior binding-table, or NIL.
  ;;Prefix-alist is an alist of Gensym atoms and binding tables.
  ;; A prefix-character binding contains a reference to a gensym atom,
  ;; and this is used to get to the binding table in the alist.
  ;; If the binding-table is not found in the alist the superiors are searched.
  ;; If it is found in a superior then that is the superior of the new
  ;; binding-table. Otherwise the new binding-table has no superior.
  ;; This is complicated, but I think the simplest robust scheme for
  ;; extensible and inheritable prefix-character bindings.
  ;; Notice in particular that
  ;; (editor-bind-key FOO (internal-editor-lookup-key #\control-x))
  ;; will work, and be inheritable. Also the binding of control-x could
  ;; be deleted or changed (even to be a new prefix-character).
  :ordered-instance-variables
  :initable-instance-variables
  )

(defmethod (binding-table :invalidate) ()
  (loop for i from 0 below 256
        do (setf (sgvref cache i) nil)
        do (setf (sgvref cache (+& i 256)) nil) ;9bit character set.
        do (setf (bit bit-vector i) 0))
  (setq bit-vector-valid? nil))

(defmethod (binding-table :define-binding) (key binding)
  ;;By binding something to nil you may cause it to be unbound
  ;;even when it is still bound in a superior binding table.
  ;;Note that in all cases it is less efficieint to lookup an
  ;;unbound key than a bound one.
  (setq invalidate-binding-tables-on-reference t)
  ;;Shadow superior binding definitions.
  (cond ((assq key alist) (setf (cadr (assq key alist)) binding))
        (t (push (list key binding) alist))))

(defmethod (binding-table :undefine-binding) (key)
  (setq invalidate-binding-tables-on-reference t)
  (delq (assq key alist) alist))

(defmethod (binding-table :lookup-primitive) (key)
  (or (assq key alist)
      (and (not (null superior))
           (send superior :lookup-primitive key))))

(defmacro check-binding-validity ()
  '(when invalidate-binding-tables-on-reference
     (invalidate-all-binding-tables)))

(defun invalidate-all-binding-tables ()
  (loop for table in *all-binding-tables*
        do (send table :invalidate))
  (setq invalidate-binding-tables-on-reference nil))

(defun coerse-to-9bit-charp (character)
  ;;Return an integer < 512 to represent the character.
  (unless (and (<& (char-code character) 128)
               (<& (char-bits character) 4)
               (0p (char-font character)))
    (ed-lose "Invalid character for keybinding ~a" character))
  (+& (char-code character) (*& (char-bits character) 128)))

(defmethod (binding-table :lookup) (key)
  (unless (characterp key)
    (ed-lose "Key not character ~a" key))
  (check-binding-validity)
  (let ((s-key (coerse-to-9bit-charp key)))
    (or (sgvref cache s-key)
        (let ((binding (send self :lookup-primitive key)))
          (if (not (null binding))
              (progn (setf (sgvref cache s-key) (cadr binding))
                     (cadr binding)))))))

(defmethod (binding-table :get-echoin-bit-vector) ()
  (unless bit-vector-valid?
    (send self :calculate-bit-vector))
  bit-vector)

(defmethod (binding-table :calculate-bit-vector) ()
  (loop for i from 0 below 256
        for char = (int-char i)
        do (setf (bit bit-vector i)
                 (if (and (graphic-charp char)
                          (not (of-syntax char paren-close))
                          (eq (or (send self :lookup char)
                                  (send self :lookup (char-upcase char)))
                              'self-insert))
                     1 0)))
  (setq bit-vector-valid? t))

(defun make-binding-table (&optional (superior nil))
  (let ((table (make-instance 'binding-table
                              :cache (make-vector 512 :initial-element nil)
                              :bit-vector (make-bits 256)
                              :alist nil
                              :superior superior
                              :bit-vector-valid? nil
                              :prefix-alist nil
                              :buffer-local? nil)))
    (push table *all-binding-tables*)
    table))

;Two compatibility functions.
(defun make-empty-key-binding-table ()
  (make-binding-table))

(defun derive-bindings (superior-bindings)
  (make-binding-table superior-bindings))

;
;Key binding.
;

(defmethod (binding-table :get-prefix-table) (char)
  ;;This use of :lookup-primitive is slower at run time, but
  ;;using :lookup would force every prefix-character sub-command binding
  ;;to invalidate all of the binding-tables. This would be very very
  ;;slow. linking the editor would be painfull.
  ;;There could be a message for :get-prefix-table-for-binding
  ;;or a flag argument. It probably is not worth the trouble either way though,
  ;;since control-x commands are several orders of magnitude less important
  ;;than self-insert. In fact it is guaranteed that terminal i/o will be
  ;;done immediately after this lookup (to get the subcommand dispatch char)
  ;;so we have all the time in the world to find the table.
  (let ((binding (cadr (send self :lookup-primitive char))))
    (and (consp binding)
         (eq (car binding) 'prefix-character)
         (or ;;Table already there?
             (cadr (assq (caddr binding) prefix-alist))
             ;;Find it and inherit from intermeditate superiors.
             (send self :get-prefix-table-primitive (caddr binding))
             (let ((new (make-binding-table)))
               (push (list (caddr binding) new) prefix-alist)
               new)))))

(defmethod (binding-table :get-prefix-table-primitive) (tag &aux table)
  (cond ((cadr (assq tag prefix-alist)))
        ((null superior) nil)
        ((setq table (send superior :get-prefix-table-primitive tag))
         (setq table (make-binding-table table))
         (push (list tag table) prefix-alist)
         table)
        (t nil)))

;Why clutter up the m-X namespace with all these???

(defun internal-bindery (key-seq binding &optional (mode nil))
; (when (symbolp binding)
;   (defcommand (string binding) meta-x-command-table binding))
  (%internal-bindery key-seq binding
                     (if (null mode)
                         *editor-bindings*
                         (mode-keys (lookup-mode mode)))))

(defun %internal-bindery (key-seq binding table)
  (cond ((consp key-seq)
         (let ((x-table (send table :get-prefix-table (car key-seq))))
           (when (null x-table)
             (ed-lose "Bad prefix character: ~a" key-seq))
           (send x-table :define-binding (cadr key-seq) binding)))
        (t (send table :define-binding key-seq binding)))
  binding)

(defun lookup-key-in-superior-tables (key table)
  (send table :lookup key))

(defun %internal-lookup (key-seq table)
  (if (consp key-seq)
      (let ((x-table (send table :get-prefix-table (car key-seq))))
        (when (null x-table)
          (ed-lose "Bad prefix-character in ~a" key-seq))
        (lookup-key-in-table (cadr key-seq) x-table))
      (lookup-key-in-table key-seq table)))

(defun internal-editor-lookup-key (key-seq)
  (if (consp key-seq)
      (let ((x-table (send *editor-bindings* :get-prefix-table (car key-seq))))
        (when (null x-table)
          (ed-lose "Bad prefix-character in ~a" key-seq))
        (lookup-key-in-table (cadr key-seq) x-table))
      (lookup-key-in-table key-seq *editor-bindings*)))

;The prefix-character scheme is rather complex.
; It includes many levels of indirection to implement two principle features.
; (1) The binding definition of a prefix-character can be bound to any
;     other key and it will be the same prefix-character (using the same
;     subtable.) With some restriction this can be done even if the new
;     binding is done in a different mode. (The restriction is a scoping
;     restriction. The new binding must be in the original binding-table, or
;     one of its inferiors.) With this restriction it is unambiguous how
;     the subtable should inherit: it always inherits from some superior,
;     just as the original binding-table would.
; (2) Redefinining the binding of a two character command is always
;     scoped right. An inferior mode definition will shadow a superior
;     definition and a superior definition will be inherited by an inferior
;     binding-table unless it is shadowed. Again this is exactly the behavior
;     of single character binding definitions.
;
;Implementation.
;       The binding definition of a prefix character includes a gensym atom.
;       This identifies the original sub-table for that prefix-character.
;       Using this as a tag the sub-table is found by searching the
;       current binding-table and its superiors. The instance-variable
;       prefix-alist is used to store the sub-table. If the prefix-table
;       is found in a superior binding-table, then a new sub-table is
;       generated and added to the prefix-alist of the current binding-table.
;       The newly generated sub-table will have the original one as its
;       ultimate superior, so inheritance and shadowing can occur.
;
;      Binding a sub-command is done by finding or creating the correct
;       sub-table (as described above) and then binding the secondary
;       dispatch character in the sub-table.
;
;      Lookup is done by finding or creating the correct sub-table and
;       looking up the secondary dispatch character in the sub-table.
;

(defun define-prefix-char (char &optional (prompt-string ""))
  (internal-bindery char `(prefix-character
                           ,prompt-string
                           ,(gensym))))

(defun prefix-character (command-char echo-string tag)
 (let ((char)
       (x-table (send *editor-bindings* :get-prefix-table command-char)))
  (or (null *display-prefixes*)
      (type-ahead-p)
      (null echo-string)
      (with-prefix-echo-line
       (send terminal-io :oustr echo-string 0 (string-length echo-string))
       (peek-char&save terminal-io)))
  (setq char (canonicalize-control-characters (read-char&save terminal-io)))
  (values (list command-char char)
          (or (send x-table :lookup char)
              (send x-table :lookup (char-upcase char))))))


;
;Local binding
;


(defmethod (binding-table :buffer-local?) (&optional (value buffer-local?))
  (setq buffer-local? value)
  buffer-local?)

(defun bind-key-in-buffer (buffer key-seq binding)
  (let ((binds (buffer-key-bindings buffer)))
    (when (not (send binds :buffer-local?))
      (setq binds (make-binding-table binds))
      (send binds :buffer-local? t)
      (bind-in-buffer buffer '*binding-table* binds))
    (%internal-bindery key-seq binding binds))
  t)

(defun buffer-local-value (buffer variable)
  (let ((env (buffer-environment buffer)))
    (if (null env)
        (values nil nil)
        (loop for var in (load-environment-binding-vars env)
              for val in (load-environment-new-values env)
              if (eq var variable) return (values val t)
              finally (return (values nil nil))))))

(defun buffer-default-major-mode (buffer)
  (let ((temp (buffer-file-name buffer)))
    (or (and temp
             (setq temp (send temp :type))
             (default-major-mode temp))
        "Fundamental")))

(defun buffer-key-bindings (buffer)
  (or (buffer-local-value buffer '*editor-bindings*)
      (mode-keys (lookup-mode (buffer-default-major-mode buffer)))))

;Kludging key bindings.

(defmacro with-key-binding (key binding &body forms)
  (let ((table (gensym))
        (old-binding (gensym)))
    `(let ((,table *editor-bindings*))
       (let ((,old-binding (%internal-lookup ,key ,table)))
         (unwind-protect
          (progn (%internal-bindery ,key ,binding ,table)
                 ,@forms)
          (%internal-bindery ,key ,old-binding ,table))))))

(defmacro with-many-key-bindings (alist &body forms)
  (let ((old-bindings (gensym))
        (table (gensym))
        (x (gensym))
        (y (gensym))
        (v-alist (gensym)))
    `(let ((,table *editor-bindings*)
           (,v-alist ,alist))
       (when (not (listp (car ,v-alist)))
         (setq ,table (car ,v-alist))
         (setq ,v-alist (cdr ,v-alist)))
       (let ((,old-bindings
              (mapcar #'(lambda (,x) (%internal-lookup (car ,x) ,table))
                      ,v-alist)))
         (unwind-protect
          (progn (mapc #'(lambda (,x)
                           (%internal-bindery (car ,x) (cadr ,x) ,table))
                       ,v-alist)
                 ,@forms)
          (mapc #'(lambda (,x ,y)
                    (%internal-bindery (car ,x) ,y ,table))
                ,v-alist ,old-bindings))))))

(defmacro with-many-key-bindings-named ((var alist) &body forms)
  ;;This form is very powerfull. It binds several keys temporarilly,
  ;;inside an unwind-protect so they are certain to be rebound on exit.
  ;;However, the VARIABLE given as the first parameter is used to store
  ;;the old key-bindings. They could be changed this way, but this is not
  ;;reccommended and is considered to be incorrect. It can be used as
  ;;an argument to this form so one may temorarrily revert to the old
  ;;bindings. It has been hacked so it also stores the binding-table to
  ;;use there.
  (let ((old-bindings var)
        (table (gensym))
        (x (gensym))
        (v-alist (gensym)))
    `(let ((,table *editor-bindings*)
           (,v-alist ,alist))
       (when (not (listp (car ,v-alist)))
         (setq ,table (car ,v-alist))
         (setq ,v-alist (cdr ,v-alist)))
       (let ((,old-bindings
              (cons ,table
                    (mapcar #'(lambda (,x) (list (car ,x)
                                                 (%internal-lookup
                                                  (car ,x) ,table)))
                            ,v-alist))))
         (unwind-protect
          (progn (mapc #'(lambda (,x)
                           (%internal-bindery (car ,x) (cadr ,x) ,table))
                       ,v-alist)
                 ,@forms)
          (mapc #'(lambda (,x)
                    (%internal-bindery (car ,x) (cadr ,x) ,table))
                (cdr ,old-bindings)))))))



;
;NEW binding functions.
;These are now the correct way to do these things.
;They provide for proper separation between editor names and lisp names
;and also require documentation strings. (I am facist, I don't make
;documentation optional...But I know the sub-primitives to avoid this, ha ha.)
;

;(define-binding key-seq binding mode-list documentation args)

;(define-extended name binding documentation args)

(defvar function-documentation nil)

(defun %define-extended (name binding documentation args)
  (defcommand (string name) meta-x-command-table binding)
  (setq function-documentation
        (nconc function-documentation
               (delq (assq (intern name 'steve) function-documentation)
                     function-documentation)
               (list (list* binding documentation args))))
  name)

(defun %define-binding (key-seq binding m-x-name modes documentation args)
  (mapc #'(lambda (mode)
            (%internal-bindery key-seq binding
                               (mode-keys (lookup-mode mode))))
        modes)
  (when m-x-name
    (%define-extended m-x-name binding documentation args))
  (or m-x-name binding key-seq))

;User level suger.

(defmacro define-extended (name binding documentation &rest doc-args)
  `(%define-extended name ',binding documentation ',doc-args))

(defmacro define-binding (key-seq binding m-x-name modes documentation
                                  &rest args)
  `(%define-binding ,key-seq ',binding ,m-x-name
                    ,(if modes
                         (list 'quote modes)
                         '("Fundamental"))
                    ',documentation ',args))
