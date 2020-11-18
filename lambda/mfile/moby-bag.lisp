;;; -*- Mode: Lisp; Base: 8; Package: moby-file-system; Readtable: T -*-


;  COPYRIGHT LISP MACHINE INC 1985

; A "bag" is
;some statements

;each statement is:
;  optionally, a name.
;  a form.
;  optionally, a enable condition.

(defschema compile-load (file)
  (meta-progn
    (compile (qc-file-if-necessary file))
    (load    (load-if-necessary  file) (after compile))))

(defnewsystem foobar
 (foo (compile-load  "foo"))
 (ble (compile-load))   ;if name ommitted, default from event name.
 (bar (compile-load  "lose-the-bar") (after (load foo)))
 (hack (compile-load) (after (compile hack) (compile bar)))
 )

;a metamacro node has:
; pointer to previous node.
; variable a-list
; list of possible expressions to expand.
; alist of expression expanded and following node.

;write-once style of programming:
; variable may be unassigned or set once.  Once set, it can never
;change.  However, new bindings can be created.

; (take-from-bag (<variable assigned to selected item>
;                 <variable assigned to remaining items>
;                 <bag>) <body>)

(defmetamacro defnewsystem (options files-and-constraints)
  (meta-let (files-so-far nil)
   (take-from-bag (file-and-constraint files-and-constraints files-and-constraints)
      (meta-if (check-constraint (second file-and-constraint) files-so-far)

; (meta-happen event-name <lisp-code>)
;   event-name recorded for use in constraint checking..
;   <lisp-code> is "outputted" (if success eventually occurs along this path).


; produce list of events that want to happen.
;  start with a list of events (associated contraints tag along)
;  expand schemas to produce list of atomic events.
; find an acceptable order for them.
;  explore tree.
