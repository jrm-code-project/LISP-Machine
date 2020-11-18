;;;-*-Mode:lisp;package:window-maker;base:8; Readtable: ZL-*-
;;; Copyright C LISP MACHINE INC., 1985.
;;;
;;;
;;;  At this point of time all the structure should be under the very first frame
;;; which is pointed to by the variable *frame*
;;; The goal is to construct the list of panes and the constraints on the organization
;;; of these panes to reflect exactly what the user has done in the graphical section
;;; of the window editor.
;;;
;;;

(defun generate-pane-specifier-section ()
  ;; this will be only for checking the case where there is no panes at the top level.
  ;; force the top level frame to have one pane.
  (or (funcall *frame* :list-of-panes-or-frames)
      (funcall *frame* :make-a-pane-to-use-for-code-generation))
  (loop for pane in (funcall *frame* ':get-panes-of-inferiors)
        as pane-specifier = (nconc (list (funcall pane ':name-of-pane)
                                         (funcall pane ':type))
                                   (funcall pane ':init-options))
        with pane-specifier-list = nil
        do (update-list pane-specifier-list pane-specifier)
        finally (return pane-specifier-list)))

(defun make-every-body-even (list-of-items)
  (loop for item in list-of-items
        do
        (funcall item ':set-keyword ':EVEN))
  list-of-items)

(defun sort-panes-and-frames-by-keyword (list-of-panes-or-frames)
  ;
  ;     This will consist only of searching down the list and making sure
  ;  that if a pane has the keyword :EVEN, it ends up as the last
  ;  element of the list
  ;
  (loop with list-of-items = (copylist list-of-panes-or-frames)
        for pane-or-frame in list-of-panes-or-frames
        as keyword = (funcall pane-or-frame ':keyword)
        with number-of-even = 0
        when (equal keyword ':EVEN)
        do
        (delete-element-from-list list-of-items pane-or-frame)
        (update-list list-of-items pane-or-frame)
        (setq number-of-even (1+ number-of-even))
        finally (return (if (> number-of-even 1) (make-every-body-even list-of-items)
                          (if (zerop number-of-even) (funcall (first (last list-of-items)) ':Set-keyword ':EVEN))
                          list-of-items))))


(defun group-the-items-by-subgroup (list-of-items)
  (loop for item in list-of-items
        with list-of-subgroup = nil
        with flag = (every (mapcar #'(lambda (x) (send x :keyword)) list-of-items)
                           #'(lambda (x) (equal x :even)))
        with last-item = (first (last list-of-items))
        with subgroup-of-panes = nil
        do
        (if flag (return (ncons list-of-items))
            (if (eq item last-item)
                (if (equal (funcall item ':keyword) ':even)
                    (if (not subgroup-of-panes) (update-list list-of-subgroup (ncons item))
                      (update-list list-of-subgroup subgroup-of-panes)
                      (update-list list-of-subgroup (ncons item)))
                  (update-list subgroup-of-panes item)
                  (update-list list-of-subgroup subgroup-of-panes))
              (update-list subgroup-of-panes item)))
        finally (return list-of-subgroup)))


(defun build-constraint-for-subgroup (subgroup)
  (loop for item in subgroup
        with constraint-list = nil
        with old-constraint
        when (typep item 'frame)
        do
        ;;
        ;;   There is only one item and it is a frame. So supply the name and return its constraint list.
        ;;
        (funcall item ':set-name-of-frame (gentemp 'dummy-name))
        (update-list constraint-list (generate-constraint-list-for-frame item))
        else do
        (setq old-constraint (get (funcall item :name-of-pane) 'old-constraint))
        ;;
        ;;   There are only panes in this subgroup
        ;;
        (update-list constraint-list (append (ncons (funcall item ':name-of-pane))
                                             (or old-constraint (funcall item ':get-size-keyword))))
        finally (return constraint-list)))

(defun generate-constraint-list-for-frame (&optional (frame *frame*))
  (loop with list-of-panes-or-frames-unsorted = (funcall frame ':list-of-panes-or-frames)
        with direction-of-slice = (funcall frame ':direction-of-slice)
        with direction-of-stacking = (if (equal direction-of-slice ':horizontal) ':vertical ':horizontal)
        with sorted-list-by-stacking = (sort-panes-and-frames-in-frame list-of-panes-or-frames-unsorted direction-of-slice)
        with sorted-list-by-keyword = (sort-panes-and-frames-by-keyword list-of-panes-or-frames-unsorted)
        with owner = (funcall frame ':owner)
        with old-stacking-direction = (and owner (if (equal (funcall owner ':direction-of-slice) ':horizontal)
                                                   ':vertical ':horizontal))
        with direction-of-owner-stacking = (or old-stacking-direction ':vertical)
        with list-of-subgroup = (group-the-items-by-subgroup sorted-list-by-keyword)
        with constraints = nil
        for subgroup in list-of-subgroup
        do
        (update-list constraints (build-constraint-for-subgroup subgroup))
        finally (return
                  (let ((list-of-names (loop with list-of-names = nil
                                            for item in sorted-list-by-stacking
                                            as operation = (if (typep item 'frame)
                                                               ':name-of-frame ':name-of-pane)
                                            do
                                            (update-list list-of-names (funcall item operation))
                                            finally (return list-of-names)))
                        constraint)
                    (setq constraint (nconc (if (equal (funcall frame ':name-of-frame) 'whole)
                                               (ncons list-of-names)
                                             (list (funcall frame ':name-of-frame) direction-of-stacking
                                                   (funcall frame ':get-size-keyword)
                                                   list-of-names))
                                           constraints))
                    (if (equal (funcall frame ':name-of-frame) 'whole)
                        (if (equal direction-of-owner-stacking direction-of-stacking)
                            constraint
                          (list (ncons (funcall frame ':name-of-frame))
                                (list (append (list (funcall frame ':name-of-frame)
                                                    direction-of-stacking '(:EVEN))  constraint))))
                      constraint)))))
