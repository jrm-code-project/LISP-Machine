;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:10; Readtable:CL -*-

#|
This file is the last one loaded into the "System" system.
|#

(defun maybe-set-systems-released (&optional (new-status :released))
  ;;Status of patchable systems
  (let ((systems-to-release nil) answer)
    (dolist (s patch-systems-list)
      (and (neq (patch-status s) new-status)
           (push s systems-to-release)))
    (when (setq answer
                (and systems-to-release
                     (fquery `(:type :tyi :choices (((:all "All")   #\A)
                                                    ((:some "Some") #\S)
                                                    ((nil "None")   #\N))
                                     :clear-input t
                                     :timeout ,(* 5 60 60)
                                     :default-value nil)
                             "Mark patchable systems as ~A? " new-status)))
      (dolist (s (nreverse systems-to-release))
        (when (or (eq answer :all)
                  (let ((proceed
                          (fquery `(:type :tyi :choices (((t "Yes")      #\Y)
                                                         ((nil "No")     #\N)
                                                         ((:quit "Quit") #\Q)
                                                         ((:proceed "Proceed") #\P))
                                          :clear-input t
                                          :timeout ,(* 2 60 60)
                                          :default-value nil)
                                  "~A ~A (version ~D.~D)~&  Mark it ~A? "
                                  (patch-status s)
                                  (patch-name s)
                                  (patch-version s)
                                  (version-number (first (patch-version-list s)))
                                  new-status)))
                    (case proceed
                      (:quit (return))
                      (:proceed (setq answer :all)))
                    proceed))
          (set-system-status (patch-name s) new-status)
          (format t "~&~A is ~A" (patch-name s) new-status)))))
  ;;Status keyword
  (when (yes-or-no-p-with-timeout (* 5 60 60) nil
                                  "~2&Current release status is ~S~@[ ~A~].  Change it ?"
                                  si:*release-status*
                                  (release-status))
    (let* ((statuslist release-status-keywords)
           (choices (loop for status in statuslist
                          as str = (format nil "~1(~A~)" status)
                          collect (list
                                    (list status str)
                                    (char str 0)))))
      (format t "~%The release status should be one of the following:~%~{~%~3T- ~1(~A~)~}" statuslist)
      (setq si:*release-status*
            (fquery `(:type :tyi
                      :choices ,choices
                      :clear-input t
                      :timeout ,(* 2 60 60)
                      :default-value :released)
                    "~%Set release status - specify first character: "))))
  (format t "~2&")
  ;;Show results
  (print-herald)
  nil)

(add-initialization "Maybe Set Systems Released" '(maybe-set-systems-released) '(:gc-system-release :head-of-list))
