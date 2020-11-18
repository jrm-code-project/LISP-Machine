;;; -*- Mode:LISP; Package:USER; Readtable:ZL; Base:10 -*-

;;;SIMPLE-UNIX-EVAL
;;;
;;;Defines a LISP command that executes commands on a Unix Chaosnet
;;;host.  You have to be careful; the commands are executed in the
;;;Bourne shell, within the EVAL server running on the Unix host, with
;;;the current directory set to a Chaosnet file directory.  It is good
;;;practice to change the working directory on the command line.  Also,
;;;for some Unix Chaosnet hosts, you must establish the Unix login
;;;'anonymous' under which the EVAL server runs.
;;;
;;;For example, if LURCH is a Unix Chaosnet host:
;;;
;;;(simple-unix-eval "lurch" "ls //etc")

(defun simple-unix-eval (host command)
  (with-open-stream
    (s (chaos:open-stream host (string-append "EVAL " command)))
    (format t "~&# ~A~%" command)
    (do ((c))
        ((null (setq c (send s ':tyi))))
        (send standard-output
              :tyo
              (selectq c
                ;;LISP to UNIX character
                ;;set translation.
                ((#o12 #o15) #\return)
                (#o11 #\tab)
                (t c))))))

;;;This is the example in the manual.  This function writes out a simple
;;;C program, then goes on to compile and execute it.

(defun test-simple-unix-eval()
  (with-open-file (s "lurch://tmp//foo.c" ':out)
    (format s "#include <stdio.h>~
               ~%main()~
               ~%  {printf(/"Hello world.\n/");}~%"))
  (simple-unix-eval "lurch" "cd //tmp;cc foo.c")
  ;;default dir is /etc/chaos
  ;;cd /tmp fixes problem.
  (simple-unix-eval "lurch" "cd //tmp;a.out"))
