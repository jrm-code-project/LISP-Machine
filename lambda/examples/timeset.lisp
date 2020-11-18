;;; -*- Mode:LISP; Package:CHAOS; Base:10; Readtable:ZL -*-

#||

 Copyright LISP Machine, Inc. 1985, 1986
   See filename "Copyright.Text" for
 licensing and release information.

*********************************************************
*********************************************************
*** NOTE: This is an EXAMPLE, not LMI supported code. ***
*** information contained in this example is subject  ***
*** to change without notice. The ways of doing       ***
*** the things contained in the example may change    ***
*** between system releases. Some techniques which    ***
*** are mere examples in one release may become built ***
*** in system features in the next release. Use good  ***
*** judgement when copying these techniques. Most     ***
*** examples have been motivated by specific customer ***
*** requests, and may not be the best engineered      ***
*** or most efficient solution for someone else.      ***
*********************************************************
*********************************************************

||#


(defun remote-set-time (host &key (time (time:get-universal-time)) (timeout 4))
  (with-open-stream (x (chaos:open-stream host "EVAL" :timeout (* timeout 60)
                                          :error nil))
    (cond ((errorp x)
           x)
          ('else
           (condition-case (y)
               (progn (format x " (TIME:SET-LOCAL-TIME #o~O)   CHAOS:QUIT "
                              time)
                      (send x :force-output)
                      (do ((c))
                          ((null (setq c (send x :tyi)))))
                      )
             (si:remote-network-error y))))))


(defun set-all-lispm-times (&optional set-local-time?)
  (when set-local-time?
    (do ((ut))
        (nil)
      (setq ut (prompt-and-read :date "~&New time: "))
      (if (y-or-n-p "~&Is ~A correct?" (time:print-universal-time ut nil))
          (return (time:set-local-time ut)))))
  (let ((hosts (mapcan #'(lambda (x)
                           (if (and (not (eq (car x) si:local-host))
                                    (eq :lispm (send (car x) :system-type)))
                               (list (car x))))
                       (chaos:CREATE-HOSTAT-CONNECTION-LIST nil))))
    (format t "~&; Possible lispm hosts: ~D, checking for up hosts..." (length hosts))
    (let ((up (chaos:up-hosts hosts)))
      (format t "~&; Hosts found up: ~D, setting times..~%" (length up))
      (dolist (host up)
        (let ((old (chaos:host-time (list host)))
              (new (1+ (time:get-universal-time))))
          (format t "~A had time: ~A, setting to ~A~%"
                  (send host :name)
                  (time:print-universal-time old nil)
                  (time:print-universal-time new nil))
          (let ((result (remote-set-time host :time new)))
            (if (errorp result)
                (format t "; Error in setting ~A~%"
                        (send result :report-string)))))))))
