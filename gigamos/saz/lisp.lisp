(DEFUN UPDATE-COMMAND-WHO-LINE-DOCUMENTATION (COMMAND &OPTIONAL (TELL-WHO-LINE T) RECURSIVE
                                                      &AUX STRING FUNCTION)
  (IF (SETQ STRING (GET COMMAND :WHO-LINE-DOCUMENTATION))
      (condition-case ()
          (SETF (ARRAY-LEADER STRING 0) 0)
        (array-has-no-leader
         (setq string
               (copy-array-contents
                 string
                 (make-array (string-length string)
                             :element-type 'string-char
                             :fill-pointer t
                             :leader-length 1)))
         (setf (array-leader string 0) 0)))
      (SETQ STRING (MAKE-EMPTY-STRING 95.))
      (PUTPROP COMMAND STRING :WHO-LINE-DOCUMENTATION))
  (SETQ FUNCTION (GET COMMAND 'WHO-LINE-DOCUMENTATION-UPDATER))
  (IF RECURSIVE
      (FUNCALL FUNCTION STRING T)
    (FUNCALL FUNCTION STRING))
  (AND TELL-WHO-LINE
       (SEND TV:WHO-LINE-DOCUMENTATION-WINDOW :SET-WHO-LINE-ITEM-STATE NIL)))
