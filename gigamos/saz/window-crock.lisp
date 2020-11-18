
(defmethod (zmail-window :around :process-special-command)
           (cont mt args &rest ignore)  ;used to have TYPE arg after ARGS.
  (let ((string
          (copy-array-contents-and-leader
            "Click the mouse on a message line to save editing changes and jump to this message."
            (make-string 95.))))
    (setf (get 'ZMAIL-SUMMARY-MOUSE :WHO-LINE-DOCUMENTATION)
          string)
  (let ((item (cdr args)))
    (selectq (car-safe item)
      (zwei:summary-mouse
       ;; We really can't handle menu commands when we're composing mail or replying, because
       ;; many of the commands refer to the current messsage, and so on.
       (progn (zmail-select-msg (msg-displayed-index (cadadr item)))
              (com-quit-zmail-edit)))
      (:menu            ; blip type
       (if (fquery '(:type :tyi) "To execute the command /"~A/", ~
                                  you must first return from editing this message.~
                                ~%Return from editing this message now? " (car (second item)))
           (progn (cleanup-message-window)
                  (format *query-io* "Click again to execute the /"~A/" command" (car (second item)))
                  (com-quit-zmail-edit))
         (format *query-io* "Type ~\lozenged-character\ to abort, or ~\lozenged-character\ to save ~
                             and return from editing this message."
                 #/Abort #/End)))
      (read-background-response-queue
       ;;Gross crock!  This command manages to filter through here, and ZWEI doesnt have
       ;; a handler, so it bombs.  Right thing is to ignore it here so it filters
       ;; thru the main loop.  --rg 12/08/86
       nil)
      (otherwise (lexpr-funcall-with-mapping-table cont mt args))))))
