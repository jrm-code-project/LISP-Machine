;;; -*- Mode:LISP; Package:TV; Base:10; Readtable:ZL -*-

;; *********************************************************
;; *********************************************************
;; *** NOTE: This is an EXAMPLE, not LMI supported code. ***
;; *** information contained in this example is subject  ***
;; *** to change without notice. The ways of doing       ***
;; *** the things contained in the example may change    ***
;; *** between system releases. Some techniques which    ***
;; *** are mere examples in one release may become built ***
;; *** in system features in the next release. Use good  ***
;; *** judgement when copying these techniques. Most     ***
;; *** examples have been motivated by specific customer ***
;; *** requests, and may not be the best engineered      ***
;; *** or most efficient solution for someone else.      ***
;; *********************************************************
;; *********************************************************


;;;----------------------------------------------------------------------------------------------------
;;;
;;;          CONTROL FOR STOPPING :CONFIGURATIONS INIT KEYWORD AND :CONFIGURATIONS OPTIONS
;;;
;;;----------------------------------------------------------------------------------------------------


(DEFFLAVOR Symbolics-Frame-configuration-Compatibility-Mixin (configurations)
           ()
  :settable-instance-variables
  :inittable-instance-variables
  (:required-flavors tv:basic-constraint-frame))

(DEFMETHOD (Symbolics-Frame-configuration-Compatibility-Mixin :after :init) (init-plist)
  (COND ((NOT (NULL (GET init-plist :configurations)))
         (Setq constraints (convert-configuration-spec (GET init-plist :configurations)
                                                       (GET init-plist :panes))))))



(defun convert-configuration-spec (configurations panes)
  (LOOP for description in configurations
        collect (rebuild-configuration description panes)))

;;;----------------------------------------------------------------------------------------------------
;;;
;;;                              Top Level
;;;
;;;----------------------------------------------------------------------------------------------------

(DEFVAR *spec-instances* ())

(DEFUN rebuild-configuration (init-list &optional (pane-list ()))
  (LET ((config-instance (MAKE-INSTANCE 'pane-config
                                        ':name (FIRST init-list)
                                        ':real-panes (LOOP for element in pane-list
                                                           collect (CAR element)))))
    (progn (SEND config-instance :build-convertable-instances init-list)
           (SEND config-instance :rebuild-pane-constraints))))


(DEFUN find-instance-named (real-name instance-list)
  "Returns an associative list of (<flavor> <instance>) pairs."
  (LOOP for instance in instance-list
        when (EQ real-name (SEND instance :name))
        collect (LIST (TYPE-OF instance) instance)))

;;;----------------------------------------------------------------------------------------------------
;;;
;;;                                    CONVERT CONFIGURATIONS
;;;
;;;----------------------------------------------------------------------------------------------------

(DEFFLAVOR pane-config
            ((layout ())                                ;the (REST <Symbolics :layout keyed list>)
            (sizes ()))                         ;the (REST <Symbolics :sizes keyed list>)
           (pane-dummy)
  :gettable-instance-variables)


;;;----------------------------------------------------------------------------------------------------




(DEFMETHOD (pane-config :build-convertable-instances)
           (init-list
            &aux (init-name (FIRST init-list)) (keyed-desc1 (SECOND init-list)) (keyed-desc2 (THIRD init-list)))
  (PROGN (COND ((NOT (SYMBOLP init-name)) (FERROR "The configuration name ~s is not a symbol." init-name))
               ((AND (EQ (FIRST keyed-desc1) ':layout) (EQ (FIRST keyed-desc2) ':sizes))
                (SETQ layout (REST keyed-desc1))
                (SETQ sizes (REST keyed-desc2)))
               ((AND (EQ (CAR keyed-desc2) ':layout) (EQ (FIRST keyed-desc1) ':sizes))
                (SETQ layout (REST keyed-desc2))
                (SETQ sizes (REST keyed-desc1)))
               (t (FERROR "Layout and Sizes are not recognizable in the configuration description group of ~s")))
         (SEND self :build-description-group-instances)))


(DEFMETHOD (pane-config :build-description-group-instances) ()
  (PROGN (LOOP for spec in layout
               do (SEND self :instantiate-layout-spec spec))
         (LOOP for spec in sizes
               do (SEND self :instantiate-size-spec spec))))


(DEFMETHOD (pane-config :instantiate-layout-spec) (spec)
  (LET ((found-list (find-instance-named (FIRST spec) *spec-instances*))
        (spec-option (SECOND spec)))
    (COND ((NULL found-list)
           (SELECTQ spec-option
             (:column (MAKE-INSTANCE 'pane-dummy :name (FIRST spec)
                                      :keyword ':vertical
                                      :option-list (REST2 spec)
                                      :real-panes real-panes))
             (:row (MAKE-INSTANCE 'pane-dummy :name (FIRST spec)
                                   :keyword ':horizontal
                                   :option-list (REST2 spec)
                                   :real-panes real-panes))
             (:fill (MAKE-INSTANCE 'pane-dummy ':name (FIRST spec)
                                    :keyword ':blank
                                    :fill-options (REST2 spec)
                                    :real-panes real-panes))
             (t (FERROR "~s is not a known layout specification option" spec-option))))
          ((= 1 (LENGTH found-list))
           (LET ((pane-pair (FIRST found-list)))
             (SELECTQ spec-option
               (:column (SEND (SECOND pane-pair) :set-keyword ':vertical)
                        (SEND (SECOND pane-pair) :set-option-list (REST2 spec)))
               (:row (SEND (SECOND pane-pair) :set-keyword ':horizontal)
                     (SEND (SECOND pane-pair) :set-option-list (REST2 spec)))
               (:fill (SEND (SECOND pane-pair) :set-keyword ':fill)
                      (SEND (SECOND pane-pair) :set-fill-options (REST2 spec)))
               (t (FERROR "~s is not a known layout specification option" spec-option)))))
          (t (FERROR "More than one instance named, ~s, was created." (FIRST spec))))))


(DEFMETHOD (pane-config :instantiate-size-spec) (spec)
  (LET ((found-list (find-instance-named (FIRST spec) *spec-instances*)))
    (COND ((NULL found-list)
           (COND ((MEMQ (FIRST spec) (send self :real-panes))
                  (MAKE-INSTANCE 'pane-real :name (FIRST spec) :size-constraint (REST spec)))
                 (t (LET ((dummy (MAKE-INSTANCE 'pane-dummy :name (FIRST spec) :real-panes (send self :real-panes))))
                             (SEND dummy :build-instance-stack (REST spec))))))
          ((= 1 (LENGTH found-list))
           (LET ((pane-pair (FIRST found-list)))
             (COND ((EQ (FIRST pane-pair) '(tv:pane-real pane-real))
                    (SEND (SECOND pane-pair) :set-size-constraints (REST spec)))
                   ((MEMQ (FIRST pane-pair) '(pane-dummy pane-config tv:pane-config tv:pane-dummy))
                    (SEND (SECOND pane-pair) :build-instance-stack (REST spec)))
                   (t (FERROR "An instance of type, ~s, was created and placed in spec-instance slot."
                              (FIRST pane-pair))))))
          (t (FERROR "More than one instance named, ~s, was created." (FIRST spec))))))


;;;----------------------------------------------------------------------------------------------------


(DEFMETHOD (pane-config :rebuild-pane-constraints) ()   ;should selectq on option for correct form of pane constraints
  (SELECTQ (send self :keyword)
    (:vertical (CONS name (APPEND (LIST (send self :option-list)) (SEND self :stack))))
    (:horizontal (CONS name (LIST '(whole-thing)
                                  (LIST (APPEND `(whole-thing :horizontal (:even) ,option-list)
                                                (SEND self :stack))))))
    (:blank (FERROR "The :blank option may not be part of a top level configuration."))
    (t (FERROR "~s is not a known layout specification option." (send self :keyword)))))

(DEFMETHOD (pane-config :after :init) (IGNORE)
  (SETQ *spec-instances* ())
  (PUSH self *spec-instances*))

(DEFMETHOD (pane-config :before :stack) ()
  (COND ((NULL stack) (SEND self :set-stack (SEND self :rebuild-stack)))        ;add error check on instance-stack
        (t ())))


;;;----------------------------------------------------------------------------------------------------
;;;
;;;                                           PANE-DUMMY
;;;
;;;----------------------------------------------------------------------------------------------------

(DEFFLAVOR pane-dummy ((keyword ())             ;:row, :column, or :fill
                       (stack ())
                       (instance-stack ())
                       (option-list ())
                       (fill-options ())
                       (real-panes ())) ;a list of :fill options,or a list of real or dummy panes

           (pane-real)
  :settable-instance-variables)




;;;----------------------------------------------------------------------------------------------------

(DEFMETHOD (pane-dummy :build-instance-stack) (spec-list)
  (SEND self :build-instance-group (REVERSE spec-list)))

(DEFMETHOD (pane-dummy :build-instance-group) (spec-list)
  (COND ((NULL spec-list) instance-stack)
        ((EQ ':then  spec-list) (PUSH () instance-stack))
        ((SYMBOLP (FIRST spec-list))
         (SEND self :push-to-first-instance-stack
               (LET ((found-list (find-instance-named (FIRST spec-list) *spec-instances*)))
                 (COND ((NULL found-list)
                        (COND ((MEMQ (FIRST spec-list) (send self :real-panes))
                               (MAKE-INSTANCE 'pane-real
                                              ':name (FIRST spec-list)
                                              ':size-constraint (REST spec-list)))
                              (t (MAKE-INSTANCE 'pane-dummy :name (FIRST spec-list) :real-panes real-panes))))
                       ((= 1 (LENGTH found-list))
                        (COND ((MEMQ (FIRST (FIRST found-list))
                                     '(tv:pane-config tv:pane-dummy tv:pane-real))
                               (SEND (SECOND (FIRST found-list)) :set-size-constraint (REST spec-list))
                               (SECOND (FIRST found-list)))
                              (t (FERROR
                                   "An instance of type, ~s, was created and placed in spec-instance slot."
                                   (FIRST (FIRST found-list))))))
                       (t (FERROR "More than one instance named, ~s, was created." (FIRST spec-list)))))))
        ((LISTP spec-list)
         (LOOP for element in spec-list
               do (SEND self :build-instance-group element)))
        (t (FERROR "Size constraint list contains an illegal stack option or symbol, ~s" spec-list))))


(DEFMETHOD (pane-dummy :push-to-first-instance-stack) (item
                                                       &aux
                                                       (first-list (FIRST instance-stack))
                                                       (rest-list (REST instance-stack)))
  (PROGN (PUSH item first-list)
         (SETQ instance-stack (APPEND (LIST first-list) rest-list))
         ()))

;;;----------------------------------------------------------------------------------------------------




(DEFMETHOD (pane-dummy :rebuild-stack) (&optional (real-stack instance-stack))
  (COND ((NULL real-stack) ())
        ((LISTP real-stack) (APPEND (LIST (SEND self :rebuild-stack (FIRST real-stack)))
                                    (SEND self :rebuild-stack (REST real-stack))))
        ((INSTANCEP real-stack) (SEND real-stack :rebuild-pane-constraints))
        (t (FERROR "Illegal object encountered while rebuilding stack."))))


(DEFMETHOD (pane-dummy :rebuild-pane-constraints) ()
        (COND ((EQ keyword ':blank)
               (APPEND (LIST (SEND self :name)
                             (send self :keyword))
                       (send self :fill-options)
                       (SEND self :size-constraint)))
              ((MEMQ keyword '(:horizontal :vertical))
               (APPEND (LIST (SEND self :name)
                             (send self :keyword)
                             (SEND self :size-constraint)
                             (send self :option-list))
                       (SEND self :stack)))
              (t (FERROR "Unknown entity keyword, ~s" keyword))))

(DEFMETHOD (pane-dummy :before :stack) ()
  (COND ((NULL stack) (send self :set-stack (SEND self :rebuild-stack)))
        (t ())))


;;;----------------------------------------------------------------------------------------------------
;;;
;;;                                    PANE REAL
;;;
;;;----------------------------------------------------------------------------------------------------


(DEFFLAVOR pane-real
           (name                                ;a symbol
            (size-constraint()))                        ;a list of size-constraints
           ()
  :inittable-instance-variables
  :gettable-instance-variables
  :settable-instance-variables)


(DEFMETHOD (pane-real :after :init) (IGNORE)
  (PUSH self *spec-instances*))


(DEFMETHOD (pane-real :rebuild-pane-constraints) ()
  (APPEND (LIST name) size-constraint))
