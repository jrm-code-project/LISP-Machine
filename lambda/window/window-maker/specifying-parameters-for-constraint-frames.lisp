;;;-*-Mode:Lisp;Base:8;package:window-maker-*-
;;; Copyright C LISP MACHINE INC., 1985.
;;;


(defflavor window-code-generation
           ()
           (tv:margin-scroll-region-on-and-off-with-scroll-bar-mixin my-choose-variable-window)
  (:default-init-plist
    :margin-scroll-regions nil
    :scroll-bar nil
    :scroll-bar-always-displayed nil
    :print-function 'output-line
    :label '(:string "Specify attributes for frame" :font fonts:metsi :centered)
    :margin-choices '(("Do It" nil frame-CHOICE-DONE nil nil)
                      ("Abort" NIL frame-CHOICE-ABORT NIL NIL)))
  :gettable-instance-variables
  :settable-instance-variables
  :inittable-instance-variables)

(defun frame-choice-done (&rest ignore)
  (DECLARE (:SELF-FLAVOR window-code-generation))
  (funcall self :force-kbd-input (list ':choice-box 'do-it)))

(DEFUN frame-CHOICE-ABORT (&REST IGNORE)
  (DECLARE (:SELF-FLAVOR window-code-generation))
  (funcall-self :force-kbd-input (list ':choice-box 'abort)))

(defun build-lines ()
  (setq *list-of-lines* NIL)

  (push (create-line "Input buffer name for code output" '*buffer-name* "your-code-is-in-this-buffer"
                     '((:buffer-name "BUFFER NAME")) nil nil)
        *list-of-lines*)

  (push (create-line "Which pane to select when frame exposed" '*selection-substitute*
                     (first *list-of-existing-and-used-names*)
                     '((:selection-substitute "Choose name")) NIL NIL)
        *list-of-lines*)

  (push (create-line "choose type of frame flavor" '*type-of-constraint-frames* 'tv:constraint-frame
                     '((:type "Choose from menu")) NIL NIL)
        *list-of-lines*)

  (push (create-line "Define a flavor or function" '*type-of-code* 'my-flavor
                     '((:code "Flavor") (:code "Function")) "flavor" fonts:tr12b)
        *list-of-lines*)

  (create-line "Store configuration or generate code" '*code-or-configuration* ':configuration
               '((:action "Generate code") (:action "Store configuration"))
               "Store as next configuration" fonts:tr12b)

  (create-line "Input name for present configuration" '*configuration-name* nil
               '((:configuration-name "New name")) NIL NIL)
  )

(build-lines)

(defflavor temporary-window-code-generation () (window-code-generation))

(defwindow-resource temporary-window-code-generation ()
  :make-window (temporary-window-code-generation :height 300 :width 1200)
  :reusable-when :deactivated
  :initial-copies 1)

(defun get-the-configuration-name (window)
  "get configuration name. makes sure that the name is unique."
  (funcall window ':set-current-font fonts:cptfont)
  (let ((line-no (funcall window ':number-of-item '*configuration-name*)))
    (loop do
          (funcall window ':read-from-the-window '*configuration-name* line-no)
          (if (not (member *configuration-name* *known-configuration-names*)) (return nil)))
    (funcall window ':output-string-to-window (format nil "~A" *configuration-name*) line-no)))

(defun get-code-or-configuration (window element)
  "if code is chosen then generate the other lines as well."
  (setf (selected-element (find-line '*code-or-configuration*)) element)
  (setq *code-or-configuration*
        (cond ((string-equal element "Store configuration") ':configuration)
              ((string-equal element "Generate code") ':code)))
  (multiple-value-bind (line-no) (funcall window :number-of-item '*code-or-configuration*)
    (funcall window :delete-item line-no)
    (funcall window :insert-item line-no '*code-or-configuration*))
  ;; update display on window now. first erase all lines
  (funcall window :delete-all-elements 2)
  (and (string-equal element "Generate code")
       (loop for line in *list-of-lines*
             do
             (funcall *window-for-code-generation* :add-line-to-be-displayed line)
             finally (return t))
       (let ((line-no (funcall *window-for-code-generation* :number-of-item '*type-of-code*)))
         (funcall *window-for-code-generation* ':output-string-to-window (format nil "~A" *type-of-code*)
                  line-no))))

(defun get-frame-flavor (window)
  (setq *type-of-constraint-frames*
        (or (tv:menu-choose *menu-of-different-constraint-type*
                            '(:string "frame flavors" :font fonts:cptfontb :centered)) 'tv:constraint-frame))
  (funcall window ':output-string-to-window (format nil "~A" *type-of-constraint-frames*)
           (funcall window :number-of-item '*type-of-constraint-frames*)))

(defun get-selection-substitute (window)
  (setq *selection-substitute* (or (tv:menu-choose *list-of-existing-and-used-names*
                                                   '(:string "list of known names" :font fonts:cptfontb :centered))
                                   (first *list-of-existing-and-used-names*)))
  (funcall window ':output-string-to-window (format nil "~A" *selection-substitute*)
           (funcall window :number-of-item '*selection-substitute*)))

(defun get-code-type (element window)
  (setf (selected-element (find-line '*type-of-code*)) element)
  (putprop '*type-of-code* element ':type)
  (multiple-value-bind (line-no) (funcall window :number-of-item '*type-of-code*)
    (funcall window :delete-item line-no)
    (funcall window :insert-item line-no '*type-of-code*)
    (funcall window :read-from-the-window '*type-of-code* line-no)
    (funcall window ':output-string-to-window (format nil "~A" *type-of-code*)
           line-no)))

(defun get-buffer-name (element window)
  element
  (funcall window :read-from-the-window '*buffer-name* (funcall window :number-of-item '*buffer-name*))
  (funcall window ':output-string-to-window (format nil "~A" *buffer-name*)
           (funcall window :number-of-item '*buffer-name*)))


(defun build-flavor-definition-for-frame (list-of-panes constraints stream)
  (format stream "~%~%;;; This is a flavor definition generated by the window maker.~%~%")
  (with-output-to-string (string-output)
    (format string-output "(defflavor ~S~&()~&(~S)~&" *type-of-code* *type-of-constraint-frames*)
    (format string-output "(:default-init-plist ~&:panes ~&'(")
    (loop for pane in list-of-panes
          do
          (format string-output "~S~&" pane)
          finally (format string-output ")~&"))
    (format string-output ":constraints~&'~S)~&" constraints)
    (format string-output ":gettable-instance-variables~&:settable-instance-variables~&:inittable-instance-variables)")
    (format string-output "~%~%~%(defmethod (~S :after :init) (&rest ignore)" *type-of-code*)
    (format string-output "~&(funcall-self :set-selection-substitute (funcall-self :get-pane '~S)))" *selection-substitute*)))

(defun build-function-definition-to-instantiate-frame (list-of-panes constraints stream)
  (format stream "~%~%;;; This the function definition generated by the window maker.~%~%")
  (with-output-to-string (string-output)
    (format string-output "(defun ~S () ~& (make-instance '~S~&" *type-of-code* *type-of-constraint-frames*)
    (format string-output "':panes ~&'(")
    (loop for pane in list-of-panes
          do
          (format string-output "~S~&" pane)
          finally (format string-output ")~&"))
    (format string-output "':constraints~& '~S))~&" constraints)))

(defun command-loop-for-code-generation (&aux return-blip)
  (using-resource (*window-for-code-generation* temporary-window-code-generation *window-maker*)
    (tv:io-buffer-clear (funcall *window-for-code-generation* :io-buffer))
    (funcall *window-for-code-generation* :delete-all-elements)
    (funcall *window-for-code-generation* :set-sensitive-item-types T)
    (funcall *window-for-code-generation* :add-line-to-be-displayed (find-line '*configuration-name*))
    (funcall *window-for-code-generation* :add-line-to-be-displayed (find-line '*code-or-configuration*))
    (funcall *window-for-code-generation* :expose-near
             (list ':point (fix (// (funcall *graphic-window-area* :width) 2))
                   (fix (// (funcall *graphic-window-area* :height) 2))))
    (loop as blip = (funcall *window-for-code-generation* :list-tyi)
          as type = (first blip)
          as element = (second blip)
          do
          (selectq type
            (:type (get-frame-flavor *window-for-code-generation*))
            (:selection-substitute (get-selection-substitute *window-for-code-generation*))
            (:code (get-code-type element *window-for-code-generation*))
            (:buffer-name (get-buffer-name element *window-for-code-generation*))
            (:action (get-code-or-configuration *window-for-code-generation* element))
            (:configuration-name (get-the-configuration-name *window-for-code-generation*))
            (:choice-box (setq return-blip blip) (return nil))))
    ;; clean up now
    (funcall *window-for-code-generation* :deactivate))
  return-blip)

;;    (funcall *window-for-code-generation* :deexpose))
;  (if (equal (second return-blip) 'Abort)
;      (progn (funcall *instrument-pane* :string-out-explicit
;                     "Aborting code generation" 5 18. nil nil fonts:cptfontb tv:alu-ior))
;            (*throw 'ABORT-CODE-GENERATION T))))
;;  return-blip)


(defun generate-code-to-use (&aux (list-of-old-names (copy-list *list-of-existing-and-not-yet-used-names*))
                             flag)
  "Generate the code or the configuration. Configurations are stored until the code is generated.
When the code is generated then every thing is cleaned up, that is WindowMaker will erase all
the configuration and name information from its data base"
  (setq flag
        (*catch 'abort-code-generation
          (let ((list-of-panes (generate-pane-specifier-section))
                (constraints (generate-constraint-list-for-frame))
                (*type-of-constraint-frames* 'TV:CONSTRAINT-FRAME)
                (*type-of-code* 'MY-FLAVOR)
                (*selection-substitute* (first *list-of-existing-and-used-names*))
                (*buffer-name* 'YOUR-CODE-IS-IN-THIS-BUFFER)
                (*configuration-name* NIL)
                (*code-or-configuration* ':code)
                doit-or-abort)
            (putprop '*type-of-code* "Flavor" ':type)
            ;; get now parameters
            (setq doit-or-abort (command-loop-for-code-generation))
            (if (equal (second doit-or-abort) 'abort)
                (progn
                  (funcall *instrument-pane* :clear-screen)
                  (funcall *instrument-pane* :string-out-explicit
                           "Aborting code generation" 5 18. nil nil fonts:cptfontb tv:alu-ior)
                  (makunbound '*configuration-name*)
                  (*throw 'abort-code-generation t))
              ;; first update the two variables to include present configuration.
              (update-list *configuration-accumulated-so-far* (cons (if (boundp '*configuration-name*)
                                                                        *configuration-name*
                                                                      (setq *configuration-name* (gentemp 'config)))
                                                                    constraints))
              (update-list *known-configuration-names* *configuration-name*)
              (loop for name in list-of-panes
                    when (not (member name *names-accumulated-so-far*))
                    do (update-list *names-accumulated-so-far* name))
              (if (equal *code-or-configuration* ':CODE)
                  ;; last configuration entered and generate code.
                  ;; otherwise return since the structure is already updated.
                  (let* ((stream (zwei:open-editor-stream :buffer-name (format nil "~S" *buffer-name*) :create-p T))
                         (output-string (if (string-equal (get '*type-of-code* ':type)
                                                          "flavor")
                                            (build-flavor-definition-for-frame
                                              *names-accumulated-so-far*
                                              *configuration-accumulated-so-far*
                                              stream)
                                          (build-function-definition-to-instantiate-frame
                                            *names-accumulated-so-far*
                                            *configuration-accumulated-so-far*
                                            stream))))
                    (funcall *instrument-pane* :clear-screen)
                    (funcall *instrument-pane* :string-out-explicit
                             "Generating code now" 5 18. nil nil fonts:cptfontb tv:alu-ior)
                    (loop with index = 0
                          with max-index = (string-length output-string)
                          with expression-to-output
                          when (>= (1+ index) max-index)
                          do (return nil)
                          else do
                          (multiple-value (expression-to-output index)
                            (read-from-string output-string 'si:no-eof-option index))
                          (grind-top-level expression-to-output nil stream nil 'displaced nil)
                          (format stream "~%~%~%"))
                    (setq *names-accumulated-so-far* NIL
                          *configuration-accumulated-so-far* NIL
                          *list-of-existing-and-not-yet-used-names* NIL
                          *list-of-existing-and-used-names* NIL
                          *known-configuration-names* NIL))
                (if *list-of-existing-and-not-yet-used-names* (nconc *list-of-existing-and-not-yet-used-names*
                                                                     *list-of-existing-and-used-names*)
                  (setq *list-of-existing-and-not-yet-used-names* *list-of-existing-and-used-names*)))
              (funcall *instrument-pane* :clear-screen)
              (funcall *instrument-pane* :string-out-explicit "      Done." 0 36. nil nil fonts:cptfontb tv:alu-ior)))
          NIL))
  (funcall *instrument-pane* :select)
  (beep)
  (setq *list-of-existing-and-used-names* NIL)
  (and (= (length (funcall *frame* :list-of-panes-or-frames)) 1) (reset-window-maker))
  ;; We are all finished. just do the cleanup.
  (when flag
    (setq *list-of-existing-and-not-yet-used-names* list-of-old-names))
  )


(compile-flavor-methods window-code-generation)

(compile-flavor-methods temporary-window-code-generation)
