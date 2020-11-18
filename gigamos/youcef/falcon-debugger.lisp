;;; -*- Mode:LISP; Package:USER; Readtable:ZL; Base:10 -*-


(defvar *FALCON-DEBUGGER-FRAME* nil "The frame of the FALCON debugger")

;;;********************************************************************************
;;;
;;;         Flashy scroll window display
;;;

(defflavor flashy-display-flavor  ()
  (tv:function-text-scroll-window
   tv:mouse-sensitive-text-scroll-window
   tv:text-scroll-window
   tv:borders-mixin
   tv:top-label-mixin
   tv:basic-scroll-bar
   tv:flashy-scrolling-mixin
   tv:margin-scroll-mixin
   tv:margin-region-mixin
   tv:margin-choice-mixin
   tv:scroll-stuff-on-off-mixin
   tv:dont-select-with-mouse-mixin
   tv:pane-mixin
   tv:window)
  (:default-init-plist
    :label '(:string "Flashy scroll window" :centered :font fonts:tr12bi)
    :blinker-p nil
    :deexposed-typeout-action ':permit
    :save-bits t
    :flashy-scrolling-region '((20 0.30 0.70) (20 0.30 0.70))
    :margin-scroll-regions '((:top) (:bottom))
    :scroll-bar-always-displayed t
    :font-map (list fonts:cptfont fonts:cptfontb fonts:tr12 fonts:tr10 fonts:tr10b fonts:tr12b)
    :print-function 'output-line)
  :settable-instance-variables
  :inittable-instance-variables
  :gettable-instance-variables)

(defmethod (flashy-display-flavor :adjustable-size-p) ()
  nil)

(defmethod (flashy-display-flavor :enable-scrolling-p) ()
  tv:scroll-bar-always-displayed)


;;;********************************************************************************
;;;
;;;     Frame-registers window
;;;
;;;

(defvar *item-type-alist* nil)

(defvar *falcon-debugger-frame* nil)
(defvar *call-stack* nil)
(defvar *frame-registers* nil)
(defvar *memory-display* nil)
(defvar *lisp-object-display* nil)

(tv:add-typeout-item-type *item-type-alist* :Register "Show Data Type" show-data-type t "Show Data Type" fonts:tr12bi)
(tv:add-typeout-item-type *item-type-alist* :Register "Show as lisp Object" show-lisp-object nil
                          "Show As Lisp Object" fonts:tr12bi)
(tv:add-typeout-item-type *item-type-alist* :Register "Start Show Memory"
                          start-show-falcon-memory nil "Start Show Memory" fonts:tr12bi)
(tv:add-typeout-item-type *item-type-alist* :Register "Show Memory" show-falcon-memory nil "Show Memory" fonts:tr12bi)

(defflavor frame-registers
         (alu-left
          alu-right
          Open-frame
          Active-frame
          Return-frame
          (O-registers-list nil)
          (A-registers-list nil)
          (R-registers-list nil))
         (tv:basic-mouse-sensitive-items tv:window)
  (:default-init-plist
    :font-map (list fonts:cptfont)
    :label '(:string "Current Stack Frame Registers" :font fonts:tr12bi :centered :top)
    :save-bits t
    :blinker-p nil
    :deexposed-typeout-action :permit)
  :settable-instance-variables
  :inittable-instance-variables
  :gettable-instance-variables)

(defmethod (frame-registers :after :init) (&rest ignore)
  (funcall-self :set-item-type-alist *item-type-alist*)
  (funcall-self :compute-position-and-sizes)
  )

(defmethod (frame-registers :compute-position-and-sizes) ()
  ;; Registers are stored as a list (boxed-bit value left top right bottom)
  ;; where left top right bottom define the region in the window where the boxed-bit and register value
  ;; are output.
  ;; First the header should have the Left source and right source.
  (let* (left-field top-field right-field
         (line-h (tv:sheet-line-height self))
         (reg-width (funcall-self :string-length "*XXXXXXXX"))
         (O-left-field (funcall-self :string-length "  O99"))
         (O-right-field (+ O-left-field reg-width))
         (A-left-field (+ O-right-field (funcall-self :string-length "  A99")))
         (A-right-field (+ A-left-field reg-width))
         (R-left-field (+ A-right-field (funcall-self :string-length "  R99")))
         (R-right-field (+ R-left-field reg-width))
         )
    (setq O-registers-list nil
          A-registers-list nil
          R-registers-list nil)
    (funcall-self :clear-screen)
    (format self "~%")
    (multiple-value-setq (left-field top-field) (funcall-self :read-cursorpos))
    ;; first the position of the alu left side.
    (setq left-field (+ left-field (funcall-self :string-length "  ALU:     Left "))
          right-field (+ left-field reg-width)
          alu-left (list nil nil left-field top-field right-field (+ top-field line-h)))
    ;; now the right side of the alu
    (setq left-field (+ right-field (funcall-self :string-length " Right "))
          right-field (+ left-field reg-width)
          alu-right (list nil nil left-field top-field right-field (+ top-field line-h)))
    ;; go to next line and compute rects for header for the registers.
    (format self "~%")
    (multiple-value-setq (left-field top-field)
      (funcall-self :read-cursorpos))
    ;; Open register field number
    (setq left-field (funcall-self :string-length "  Open #x")
          right-field (+ left-field (funcall-self :string-length "XX"))
          open-frame ( list 0 left-field top-field right-field (+ top-field line-h)))
    ;; Active Register field number
    (setq left-field (+ right-field (funcall-self :string-length "     Active #x"))
          right-field (+ left-field (funcall-self :string-length "XX"))
          Active-frame (list 0 left-field top-field right-field (+ top-field line-h)))
    ;; Return Register field number
    (setq left-field (+ right-field (funcall-self :string-length "   Return #x"))
          right-field (+ left-field (funcall-self :string-length "XX"))
          Return-frame (list 0 left-field top-field right-field (+ top-field line-h)))
    ;; Now compute the position of each register line.
    ;; first leave one blank line and start from there.
    (format self "~%")
    (dotimes (reg 16)
      (format self "~%")
      (multiple-value-setq (left-field top-field)
        (funcall-self :read-cursorpos))
      (setq O-registers-list (nconc
                               O-registers-list
                               (ncons (list reg t 0 O-left-field top-field O-right-field (+ top-field line-h))))
            A-registers-list (nconc
                               A-registers-list
                               (ncons (list reg t 0 A-left-field top-field A-right-field (+ top-field line-h))))
            R-registers-list (nconc
                               R-registers-list
                               (ncons (list reg t 0 R-left-field top-field R-right-field (+ top-field line-h)))))
      )
    )
  )

(defmethod (frame-registers :output-regs) ()
  ;; called each time that we have to completly redraw the contents of the window.
  (funcall-self :clear-screen)
  (format self "~%")
  (format self "  ALU:     Left ")
  (if (first alu-left)
      (funcall-self :item :Register `(left ,(second alu-left) ,(third alu-left)) "*~8,'0x" (second alu-left))
    (format self " ~8,'0x" (second alu-left)))
  (format self " Right ")
  (if (first alu-right)
      (funcall-self :item :Register `(right ,(second alu-right) ,(third alu-right)) "*~8,'0x" (second alu-right))
    (format self " ~8,'0x" (second alu-right)))
  (format self "~%")
  (format self "  Open #x~2,'0x     Active #x~2,'0x   Return #x~2,'0x"
          (car Open-frame)
          (car Active-frame)
          (car Return-frame))
  (format self "~%")
  (do ((O-reg O-registers-list (cdr O-reg))
       (A-reg A-registers-list (cdr A-reg))
       (R-reg R-registers-list (cdr R-reg))
       o-f a-f r-f)
      ((null O-reg))
    (setq o-f (car O-reg)
          a-f (car A-reg)
          r-f (car R-reg))
    (format self "~%  O~2D" (first O-f))
    (if (second O-f)
        (funcall-self :item :Register `(O ,(first O-f) ,(third O-f)) "*~8,'0x" (third O-f))
      (format self " ~8,'0x" (third O-f)))
    (format self "  A~2D" (first A-f))
    (if (second A-f)
        (funcall-self :item :Register `(A ,(first A-f) ,(third A-f)) "*~8,'0x" (third A-f))
      (format self " ~8,'0x" (third A-f)))
    (format self "  R~2D" (first R-f))
    (if (second A-f)
        (funcall-self :item :Register `(R ,(first R-f) ,(third R-f)) "*~8,'0x" (third R-f))
      (format self " ~8,'0x" (third R-f)))
    )
  (funcall *call-stack* :update-call-stack)
  )

(defmethod (frame-registers :redisplay) ()
  (k-kbug:kbug-cmd-confirm k2:kbug-command-read-call-stack)
  (let* ((user-open-frame
           (ldb (byte 8. 8.) (k-kbug:kbug-data (+ 2 (* 2 k-kbug:depth-to-user-OA-frame)))))
         (user-active-frame
           (ldb (byte 8. 0.) (k-kbug:kbug-data (+ 2 (* 2 k-kbug:depth-to-user-OA-frame)))))
         (user-return-frame
           (ldb (byte 8. 0.) (k-kbug:kbug-data (+ 2 (* 2 k-kbug:depth-to-user-R-frame)))))
         (o-list (k-kbug:read-frame-as-list user-open-frame))
         (a-list (k-kbug:read-frame-as-list user-active-frame))
         (r-list (k-kbug:read-frame-as-list user-return-frame))
         (a-left (k-kbug:kbug-left))
         (a-right (k-kbug:kbug-right))
         (left-boxed (k-kbug:kbug-left-boxed))
         (right-boxed (k-kbug:kbug-right-boxed)))
    (do ((A-reg A-registers-list (cdr A-reg))
         (O-reg O-registers-list (cdr O-reg))
         (R-reg R-registers-list (cdr R-reg))
         (O-reg-new o-list (cdr o-reg-new))
         (A-reg-new a-list (cdr a-reg-new))
         (R-reg-new r-list (cdr r-reg-new))
         o a r)
        ((null A-reg)
         (progn
           ;; update frame numbers.
           (setf (car active-frame) user-active-frame)
           (setf (car open-frame) user-open-frame)
           (setf (car return-frame) user-return-frame)
           ;; left and right alu
           (setf (car alu-left) (ldb-test (byte 1 0) left-boxed))
           (setf (car alu-right) (ldb-test (byte 1 0) right-boxed))
           (setf (cadr alu-left) a-left)
           (setf (cadr alu-right) a-right)
           (funcall-self :output-regs)
           )
         )
      (setq o (car o-reg)
            a (car a-reg)
            r (car r-reg))
      (setf (cadr o) (caar o-reg-new))
      (setf (caddr o) (cdar o-reg-new))
      (setf (cadr a) (caar a-reg-new))
      (setf (caddr a) (cdar a-reg-new))
      (setf (cadr r) (caar r-reg-new))
      (setf (caddr r) (cdar r-reg-new))
      )
    )
  )


(defmethod (frame-registers :update-regs) ()
  (k-kbug:kbug-cmd-confirm k2:kbug-command-read-call-stack)
  (let* ((user-open-frame
           (ldb (byte 8. 8.) (k-kbug:kbug-data (+ 2 (* 2 k-kbug:depth-to-user-OA-frame)))))
         (user-active-frame
           (ldb (byte 8. 0.) (k-kbug:kbug-data (+ 2 (* 2 k-kbug:depth-to-user-OA-frame)))))
         (user-return-frame
           (ldb (byte 8. 0.) (k-kbug:kbug-data (+ 2 (* 2 k-kbug:depth-to-user-R-frame)))))
         (o-list (k-kbug:read-frame-as-list user-open-frame))
         (a-list (k-kbug:read-frame-as-list user-active-frame))
         (r-list (k-kbug:read-frame-as-list user-return-frame))
         (a-left (k-kbug:kbug-left))
         (a-right (k-kbug:kbug-right))
         (left-boxed (k-kbug:kbug-left-boxed))
         (right-boxed (k-kbug:kbug-right-boxed))
         (changed-regs nil))
    ;; if any of the frame number has changed then go and redraw the whole window
    (unless (= user-active-frame (car active-frame))
        ;; active frame has changed. Must have entered a new function.
        (funcall *call-stack* :update-call-stack))
      ;; collect changed register in chaged-regs
      (do ((A-reg A-registers-list (cdr A-reg))
           (O-reg O-registers-list (cdr O-reg))
           (R-reg R-registers-list (cdr R-reg))
           (O-reg-new o-list (cdr o-reg-new))
           (A-reg-new a-list (cdr a-reg-new))
           (R-reg-new r-list (cdr r-reg-new))
           o a r)
          ((null A-reg)
           (progn
             ;; update frame numbers.
             (unless (= (car active-frame) user-active-frame)
               (setf (car active-frame) user-active-frame)
               (push `(a ,active-frame) changed-regs)
               )
             (unless (= (car open-frame) user-open-frame)
               (setf (car open-frame) user-open-frame)
               (push `(o ,open-frame) changed-regs)
               )
             (unless (= (car return-frame) user-return-frame)
               (setf (car return-frame) user-return-frame)
               (push `(r ,return-frame) changed-regs)
               )
             ;; left and right alu
             (setf (car alu-left) (ldb-test (byte 1 0) left-boxed))
             (setf (car alu-right) (ldb-test (byte 1 0) right-boxed))
             (setf (cadr alu-left) a-left)
             (setf (cadr alu-right) a-right)
             (push `((left ,(car alu-left) ,(cadr alu-left)) ,alu-left) changed-regs)
             (push `((right ,(car alu-right) ,(cadr alu-right)) ,alu-right) changed-regs)
             (funcall-self :update-contents changed-regs)
             )
           )
        (setq o (car o-reg)
              a (car a-reg)
              r (car r-reg))
        (when (not (and (equal (cadr o) (caar o-reg-new))
                        (equal (caddr o) (cdar o-reg-new))))
          (setf (cadr o) (caar o-reg-new))
          (setf (caddr o) (cdar o-reg-new))
          (push `((O ,(car o) ,(caddr o)) ,(cdr o)) changed-regs))
        (when (not (and (equal (cadr a) (caar a-reg-new))
                        (equal (caddr a) (cdar a-reg-new))))
          (setf (cadr a) (caar a-reg-new))
          (setf (caddr a) (cdar a-reg-new))
          (push `((A ,(car a) ,(caddr a)) ,(cdr a)) changed-regs))
        (when (not (and (equal (cadr r) (caar r-reg-new))
                        (equal (caddr r) (cdar r-reg-new))))
          (setf (cadr r) (caar r-reg-new))
          (setf (caddr r) (cdar r-reg-new))
          (push `((R ,(car r) ,(caddr r)) ,(cdr r)) changed-regs))
      )
    )
  )

(defmethod (frame-registers :erase-rect) (rect &aux item)
  (let* ((l-rect (first rect))
         (T-rect (second rect))
         (r-rect (third rect))
         (b-rect (fourth rect))
         (width-rect (- r-rect l-rect))
         (height-rect (- b-rect t-rect)))
    ;; if this designate something mouse sensitive, then unsensitive it.
    ;; for the mouse sensitive message compute the position of the item with margin sizes.
    (when (setq item (cddr (funcall-self :mouse-sensitive-item
                                         (+ (tv:sheet-left-margin-size self)
                                            l-rect (// width-rect 2))
                                         (+ (tv:sheet-top-margin-size self)
                                            t-rect (// height-rect 2)))))
      (dolist (mouse-sensitive tv:item-list)
        (when (equal (cddr mouse-sensitive) item)
          (setq tv:item-list (delq mouse-sensitive tv:item-list))
          (return))))
    (funcall-self :bitblt-within-sheet
                  tv:alu-xor
                  width-rect
                  height-rect
                  l-rect t-rect
                  l-rect t-rect)
    )
  )

(defmethod (frame-registers :output-reg-number) (reg)
  (let ((rect (cdr reg)))
    (funcall-self :erase-rect rect)
    (funcall-self :set-cursorpos (first rect) (second rect))
    (format self "~2,'0x" (first reg))
    )
  )

(defmethod (frame-registers :output-register) (reg)
  (let* ((item (car reg))
         (reg-desc (second reg))
         (boxed-bit (car reg-desc))
         (value (cadr reg-desc))
         (rect (cddr reg-desc)))
    (funcall-self :erase-rect rect)
    (funcall-self :set-cursorpos (first rect) (second rect))
    (if boxed-bit
        ;; boxed value
        ;; mouse-sensitive item in window.
        (funcall-self :item :Register item "*~8,'0x" value)
      (format self " ~8,'0x" value))
    )
  )

(defmethod (frame-registers :update-contents) (regs)
  (dolist (reg regs)
    (case (car reg)
      ((a o r)
       (funcall-self :output-reg-number (cadr reg)))
      (otherwise
       (funcall-self :output-register reg))
      )
    )
  )

;;;********************************************************************************************************
;;;
;;;
;;;      Call Stack Window
;;;
;;;


(defflavor call-stack-flavor ((stack-info nil))
  (flashy-display-flavor)
  (:default-init-plist
    :label '(:string "Call Stack Dump" :centered :font fonts:metsi)
    :blinker-p nil
    :deexposed-typeout-action ':permit
    :save-bits t
    :flashy-scrolling-region '((20 0.30 0.70) (20 0.30 0.70))
    :margin-scroll-regions '((:top) (:bottom))
    :scroll-bar-always-displayed t
    :font-map (list fonts:cptfont fonts:cptfontb fonts:tr12 fonts:tr10 fonts:tr10b fonts:tr12b)
    :print-function 'output-line)
  :settable-instance-variables
  :inittable-instance-variables
  :gettable-instance-variables)

(defun output-line (text arg window &rest ignore)
  arg
  (if (cdr text)
      (progn
        (format window "  ")
        (funcall window
                 :item text
                 :call-stack
                 "Ret-Dest ~[ O~; A~; R~; G~;NO~;NO~;NT~;NT~]~2,'0D  Open ~2,'0X  Active ~2,'0X  rpc ~6,'0X ~a"
                 (third text)
                 (fourth text)
                 (fifth text)
                 (sixth text)
                 (second text)
                 (first text)))
    (format window "~A" (car text))
    )
  )

(defmethod (call-stack-flavor :show-call-stack) ()
  (funcall-self :flush-contents)
  (funcall-self :append-item `(,(format nil "CALL Stack, Maching is ~:[running~;halted~]"
                                        (k-kbug:k-halted-p)) . nil))
  (let ((cs (k-kbug:kbug-generic-read-call-stack-as-list)))
    (dolist (cse cs)
      (let* ((d0 (car cse))
             (d1 (cadr cse))
             (rpc (ldb (byte 24. 0.) d0))
             (rdf (ldb (byte 3. 28.) d0))
             (rdr (ldb (byte 4. 24.) d0))
             (o   (ldb (byte 8. 8.) d1))
             (a   (ldb (byte 8. 0.) d1))
             (sym-adr (k-kbug:kbug-symbolic-address rpc))
             line)
        (funcall-self :append-item (setq line `(,sym-adr . (,rpc ,rdf ,rdr ,o ,a))))
        (push line stack-info)
        )
      )
    )
  )

(defmethod (call-stack-flavor :flush-contents) ()
  (setq stack-info nil)
  (funcall-self :clear-screen)
  (store-array-leader 0 tv:items 0)
  (setq tv:top-item 0)
  (funcall-self :update-call-stack)
  )

(defmethod (call-stack-flavor :update-call-stack) ()
  (let* (rpc rdf rdr o a sym-adr line)
    (multiple-value-setq (sym-adr rpc rdf rdr o a)
      (k-kbug:read-call-stack-entry 7))
    ;; look for an entry with same symbolic address, not including offset.
    (setq line (dolist (l stack-info)
                 (when (string-equal (car sym-adr) (caar l))
                   (return l))))
      (if line
          ;; delete all lines that are between this line and line 0.
          (let ((item-number (funcall-self :number-of-item line)))
            (dotimes (index item-number)
              (funcall-self :delete-item 0)
              (setq stack-info (cdr stack-info)))
            )
        (progn
          (funcall-self :insert-item 0 (setq line `(,sym-adr . (,rpc ,rdf ,rdr ,o ,a))))
          (push line stack-info)))
      )
  )


;;;********************************************************************************************************
;;;
;;;
;;;

(defflavor memory-pane-flavor  ()
           (flashy-display-flavor tv:process-mixin)
  (:default-init-plist
    :label '(:string "Memory Contents" :centered :font fonts:tr12bi)
    :blinker-p nil
    :deexposed-typeout-action ':permit
    :save-bits t
    :margin-scroll-regions '((:top) (:bottom))
    :scroll-bar-always-displayed t
    :process '(display-process)
    :font-map (list fonts:cptfont fonts:cptfontb fonts:tr12 fonts:tr10 fonts:tr10b fonts:tr12b)
    :print-function 'memory-output-line)
  :settable-instance-variables
  :inittable-instance-variables
  :gettable-instance-variables)

(defun memory-output-line (addr arg window &rest ignore)
  arg
  (if (numberp addr)
      (let ((d (k-kbug:kbug-generic-read-memory addr)))
        (format window "~7,'0x: ~8,'0x  ~c~c~c~c ~40t~7,'0x ~a"
                (logand #xfffffff addr) d
                (ldb (byte 8 0) d)
                (ldb (byte 8 8) d)
                (ldb (byte 8 16) d)
                (ldb (byte 8 24) d)
                (dpb (ldb (byte 6 20.) d)
                     (byte 6 20.)
                     (ldb (byte 20. 0) d))
                (k-kbug:k-data-type-name d)))
    (format window (make-string 82. :initial-element #\-)))
  )

(defmethod (memory-pane-flavor :show-memory) (addr &optional (clear-screen t) &aux max-number-of-words object)
  (if clear-screen
      (progn
        (funcall-self :clear-screen)
        (setq tv:top-item 0)
        (store-array-leader 0 tv:items 0))
    (funcall-self :append-item t))
  ;; should look at data type of the thing to display and adjust the address accordingly.
  ;; for example we would like to see arrays including all information stored in headers....
  (cond ((not (numberp addr))
         (setq addr (k-kbug:kbug2-get-starting-address addr))))
  (setq object (k-kbug:kbug2-get-object-description addr)
        addr (car object) max-number-of-words (cadr object))
  (dotimes (i max-number-of-words)
    (funcall-self :append-item (+ addr i)))
  )

(defun display-process-internal ()
  (do ((blip (funcall *frame-registers* :any-tyi)
             (funcall *frame-registers* :any-tyi)))
      (())
;    (format (send *falcon-debugger-frame* :get-pane 'debug-pane) "~%Blip is ~S" blip)
    (case (car blip)
      (:typeout-execute
       (case (second blip)
         (show-falcon-memory
          (let ((last-item (funcall *memory-display* :number-of-items))
                (top-item (send *memory-display* :top-item)))
            (send *memory-display* :show-memory (third (third blip)) nil)
            (send *memory-display* :scroll-redisplay last-item (- last-item top-item))))
         (start-show-falcon-memory
          (send *memory-display* :show-memory (third (third blip))))
         (show-lisp-object
          (send *lisp-object-display* :show-object (third (third blip))))
         )
       )
      (:object
       (case (fourth blip)
         (#\mouse-1-1
          (funcall (third blip) :expand-object (second blip)))
         (#\mouse-1-2
          (funcall (third blip) :contract-object (second blip)))
         (#\mouse-2-1
          (funcall (third blip) :delete-object (second blip)))))
      (:menu
       (send (fourth blip) :execute (second blip))
       )
      )
    )
  )

(defun display-process (&rest ignore)
  (let ((*frame-registers* (send *falcon-debugger-frame* :get-pane 'frame-pane))
        (*call-stack* (send *falcon-debugger-frame* :get-pane 'call-stack-pane))
        (*memory-display* (send *falcon-debugger-frame* :get-pane 'memory-pane))
        (*lisp-object-display* (send *falcon-debugger-frame* :get-pane 'lisp-object-pane)))
    (display-process-internal)
    )
  )

;;;********************************************************************************************************
;;;
;;;        Object lisp display.
;;;

(defstruct (display-lisp-object
             (:conc-name lo-)
             )
  (print-name nil)                              ; the print name of the object.
  (addr nil)                                    ; virtual address in k-mem
  (display-length nil)                          ; the number of words displayed in memory contents pane.
  (object-description nil)                      ; the description of the object.
  (can-be-expanded? nil)                        ; t if the object can be opened.
  (is-expanded nil)                             ; t if the object is expanded in display window
  (number-of-lines 0)                           ; the number of lines describing the internals of the object.
  )

(defresource lisp-object ()
  "Resource for describing lisp object from the falcon processor"
  :constructor (make-display-lisp-object)
  :initial-copies 20.)


(defflavor object-lisp-display-pane ((displayed-objects nil))
           (flashy-display-flavor)
    (:default-init-plist
    :label '(:string "Lisp Object" :centered :font fonts:tr12bi)
    :blinker-p nil
    :deexposed-typeout-action ':permit
    :save-bits t
    :margin-scroll-regions '((:top) (:bottom))
    :scroll-bar-always-displayed t
    :font-map (list fonts:cptfont fonts:cptfontb fonts:tr12 fonts:tr10 fonts:tr10b fonts:tr12b)
    :print-function 'object-display-output-line)
  :settable-instance-variables
  :inittable-instance-variables
  :gettable-instance-variables)

(defun object-display-output-line (line arg window &rest ignore)
  arg
  (format window " ")
  (if (stringp line)
      (format window "~A" line)
    (funcall window :item :object line "~A" (lo-print-name line)))
  )

(defmethod (object-lisp-display-pane :expand-object) (item &aux (number-of-generated-lines 0))
  (when (and (lo-can-be-expanded? item) (not (lo-is-expanded item)))
    (let ((item-number (funcall-self :number-of-item item)))
      ;; we will insert new lines after this item. Indent them
      (do* ((tail (lo-object-description item) (cddr tail))
            (key (car tail) (car tail))
            (value (cadr tail) (cadr tail))
            )
           ((null tail))
        (when value
          (funcall-self :insert-item (setq item-number (1+ item-number)) (format nil "    ~A ~A" key value))
          (incf number-of-generated-lines)))
      (unless (zerop number-of-generated-lines)
        (setf (lo-number-of-lines item) number-of-generated-lines)
        (setf (lo-is-expanded item) t))
      )
    )
  )

(defmethod (object-lisp-display-pane :contract-object) (item)
  (when (lo-is-expanded item)
    (let ((item-number (1+ (funcall-self :number-of-item item)))
          (number-of-lines (lo-number-of-lines item)))
      (dotimes (i number-of-lines)
        (funcall-self :delete-item item-number)
        )
      )
    (setf (lo-is-expanded item) nil)
    (setf (lo-number-of-lines item) 0)
    )
  )

(defmethod (object-lisp-display-pane :delete-object) (item)
  (setq displayed-objects (delq item displayed-objects))
  ;; now delete them from display window.
  (let ((index (funcall-self :number-of-item item))
        (number-of-lines-to-delete
          (1+ (lo-number-of-lines item))))
    (dotimes (i number-of-lines-to-delete)
      (funcall-self :delete-item index))
    (deallocate-resource 'lisp-object item)
    )
  )


(defmethod (object-lisp-display-pane :show-object) (adr)
  (let ((object (k-kbug:kbug2-get-object-description adr))
        (object-line (allocate-resource 'lisp-object)))
    (setf (lo-print-name object-line) (third object))
    (setf (lo-addr object-line) (car object))
    (setf (lo-display-length object-line) (cadr object))
    (setf (lo-object-description object-line) (fourth object))
    (setf (lo-can-be-expanded? object-line) (fifth object))
    (push object-line displayed-objects)
    (funcall-self :append-item object-line)
    )
  )

;;;********************************************************************************************************
;;;
;;;
;;;



(defflavor kbug2-pane ()
           (tv:interaction-pane tv:process-mixin)
  (:default-init-plist
    :blinker-deselected-visibility :on
    :blinker-flavor 'tv:rectangular-blinker
    :blinker-p t
    :deexposed-typein-action :normal
    :deexposed-typeout-action :normal
    :label "KBUG2"
    :Save-bits t
    :process '(kbug2-process))
  :gettable-instance-variables
  :settable-instance-variables
  :inittable-instance-variables)

(defun kbug2-process (&rest ignore)
  (let* ((*terminal-io* (send *FALCON-DEbugger-frame* :get-pane 'DEBUG-PANE))
         (*standard-input* *terminal-io*)
         (*frame-registers* (send *falcon-debugger-frame* :get-pane 'frame-pane))
         (*call-stack* (send *falcon-debugger-frame* :get-pane 'call-stack-pane))
         (*memory-display* (send *falcon-debugger-frame* :get-pane 'memory-pane))
         (*lisp-object-display* (send *falcon-debugger-frame* :get-pane 'lisp-object-pane)))
    (funcall *frame-registers* :redisplay)
    (k-kbug:window-kbug2)
    ))

(defflavor wimp-pane ()
           (tv:interaction-pane tv:process-mixin)
  (:default-init-plist
    :blinker-deselected-visibility :on
    :blinker-flavor 'tv:rectangular-blinker
    :blinker-p t
    :deexposed-typein-action :normal
    :deexposed-typeout-action :normal
    :label "WIMP"
    :save-bits t
    :process '(wimp-process))
  :gettable-instance-variables
                      :settable-instance-variables
  :inittable-instance-variables)

(defun wimp-process (&rest ignore)
  (let* ((*terminal-io* (send *FALCON-DEbugger-frame* :get-pane 'WIMP-TERMINAL)))
    ;(k-kbug:wimp t)
    ))


;;;*******************************************************************************************
;;;
;;;      Falcon debugger frame.
;;;;

(defvar *command-menu-item-list*
        '(
          ("Flush Call Stack" :eval (clean-up)
           :font fonts:tr12bi
           :documentation "Clears the Call Stack Hardware")
          ("Show Call Stack" :eval (show-machine-state)
           :font fonts:tr12bi
           :documentation "Show Call Stack")
          ("Cold Boot Falcon" :eval (k-kbug:mega-boot) :font fonts:tr12bi
           :documentation "Builds a cold load and load the processor")
          ("Warm Boot Falcon" :eval (k-kbug:mega-boot t) :font fonts:tr12bi
           :documentation "Reloads the Falcon Processor")
          ("Run Listener" :eval (Run-listener)
           :font fonts:tr12bi
           :documentation "Runs the Read Eval Print loop of the Falcon")
          ("Run REP" :eval (Run-Rep)
           :font fonts:tr12bi
           :documentation "Runs the Read Eval Print loop of the Falcon")
           ))

(DEFFLAVOR FALCON-DEBUGGER-FRAME
         NIL
         (TV:BORDERED-CONSTRAINT-FRAME tv:top-box-label-mixin)
  (:DEFAULT-INIT-PLIST
    :label '(:string "FALCON DEBUGGER" :font fonts:metsi :centered :top)
  :PANES
  `((DEBUG-PANE KBUG2-PANE
                :deexposed-typeout-action :permit
                :LABEL (:string "Falcon Debugger" :font ,fonts:tr12bi :top :centered)
                :SAVE-BITS T)
    (WIMP-TERMINAL WIMP-PANE
                   :BLINKER-P T
                   :deexposed-typeout-action :permit
                   :LABEL (:string "Falcon Read Eval Print Process" :font ,fonts:tr12bi :centered :top)
                   :SAVE-BITS T)
    (MENU-PANE TV:COMMAND-MENU
               :LABEL NIL
               :SAVE-BITS T
               :ITEM-LiST ,*command-menu-item-list*
               :reverse-video-p t)
    (FRAME-PANE frame-registers
                :BLINKER-P nil
                :LABEL (:string "Current Stack Frame" :font ,fonts:tr12bi :centered :top)
                :SAVE-BITS T)
    (MEMORY-PANE memory-pane-flavor
                 :LABEL (:string "Memory Contents" :font ,fonts:tr12bi :centered :top)
                 :SAVE-BITS t)
    (LISP-OBJECT-PANE object-lisp-display-pane
                      :LABEL (:string "Lisp Object" :font ,fonts:tr12bi :centered :top)
                      :SAVE-BITS T)
    (CALL-STACK-PANE call-stack-flavor
                     :label (:string "Call Stack" :font ,fonts:tr12bi :centered :top)
                     :sAVE-BITS T))
  :CONSTRAINTS
  (QUOTE
   ((CONFIG (MENU-PANE DUMMY-NAME7 DUMMY-NAME12 DEBUG-PANE)
            ((MENU-PANE 2 :LINES)
             (DUMMY-NAME7 :HORIZONTAL (21 :lines frame-pane)
                          (FRAME-PANE DUMMY-NAME11)
                          ((FRAME-PANE 44 :CHARACTERS))
                          ((DUMMY-NAME11 :VERTICAL (:EVEN)
                                         (MEMORY-PANE wimp-terminal)
                                         ((wimp-terminal 5 :lines))
                                         ((MEMORY-PANE :even)))))
             (DUMMY-NAME12 :HORIZONTAL (0.203512s0)
                           (CALL-STACK-PANE LISP-OBJECT-PANE)
                           ((CALL-STACK-PANE 75 :CHARACTERS))
                           ((LISP-OBJECT-PANE :EVEN))))
            ((DEBUG-PANE :EVEN)))
    (MAIN
     (MENU-PANE DUMMY-NAME54 WIMP-TERMINAL DEBUG-PANE)
     ((MENU-PANE 2 :LINES)
      (DUMMY-NAME54 :HORIZONTAL (21 :lines frame-pane)
       (FRAME-PANE DUMMY-NAME70)
       ((FRAME-PANE 44 :CHARACTERS))
       ((DUMMY-NAME70 :VERTICAL (:EVEN)
                      (MEMORY-PANE DUMMY-NAME74)
                      ((MEMORY-PANE 8 :lines))
                      ((DUMMY-NAME74 :HORIZONTAL
                                     (:EVEN)
                                     (CALL-STACK-PANE LISP-OBJECT-PANE)
                                     ((CALL-STACK-PANE :EVEN) (LISP-OBJECT-PANE :EVEN)))
                       ))
        ))
      (WIMP-TERMINAL 5 :LINES))
     ((DEBUG-PANE :EVEN)))
    )))
 :GETTABLE-INSTANCE-VARIABLES
 :SETTABLE-INSTANCE-VARIABLES
 :INITTABLE-INSTANCE-VARIABLES)

(DEFMETHOD (FALCON-DEBUGGER-FRAME :AFTER :INIT) (&REST IGNORE)
  (let ((io-buffer-1 (tv:make-io-buffer 1000.)))
    (send (funcall-self :get-pane 'menu-pane) :set-io-buffer io-buffer-1)
    (send (funcall-self :get-pane 'call-stack-pane) :set-io-buffer io-buffer-1)
    (send (funcall-self :get-pane 'frame-pane) :set-io-buffer io-buffer-1)
    (send (funcall-self :get-pane 'memory-pane) :set-io-buffer io-buffer-1)
    (send (funcall-self :get-pane 'lisp-object-pane) :set-io-buffer io-buffer-1))
  (FUNCALL-SELF :SET-SELECTION-SUBSTITUTE (FUNCALL-SELF :GET-PANE 'DEBUG-PANE)))

(defmethod (FALCON-DEBUGGER-FRAME :selectable-windows) ()
  (list (list tv:name self)))


(defmethod (FALCON-DEBUGGER-FRAME :before :kill) (&rest ignore)
  (setq *FALCON-DEbugger-frame* nil))


(defun wbug (&optional (select-p t) starting-address)
  (when (or (not (boundp '*FALCON-DEbugger-frame*))
            (null *FALCON-DEbugger-frame*)
            (not (typep *FALCON-DEbugger-frame* 'FALCON-DEbugger-frame))
            (member (send *FALCON-DEbugger-frame* :status) '(:deactivated)))
    (setq *FALCON-DEbugger-frame*
          (make-instance 'FALCON-DEBUGGER-FRAME :activate-p t)))
  (when select-p
    (send *FALCON-DEbugger-frame* :select))
  *FALCON-DEbugger-frame*)

(defun update-regs ()
  (funcall *frame-registers* :update-regs)
  )

(defun show-call-stack ()
  (funcall *call-stack* :flush-contents)
  (funcall *call-stack* :update-call-stack))



(compile-flavor-methods wimp-pane kbug2-pane CALL-STACK-FLAVOR FALCON-DEBUGGER-FRAME frame-registers)

(tv:add-system-key #\D '(wbug nil) "FALCON Debugger" nil)
