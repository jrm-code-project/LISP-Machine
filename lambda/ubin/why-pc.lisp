;;; -*- Mode:LISP; Package:(WHY-PC GLOBAL); Base:10; Readtable:CL -*-

;;; Copyright LISP Machine, Inc. 1985, 1986
;;;   See filename "Copyright.Text" for
;;; licensing and release information.

;;; A simple function to call to learn the meaning of the output of
;;; the :WHY program. To use: Call the function WHY-PC:HALTS
;;; 1/24/85 13:16:40 -George Carrette

;;; this is a small program to be independant of any other diagnostics systems.

(defvar *i-mem-symbol-table* nil "An alist of (<symbol> . pc) sorted by pc")

(defvar *i-mem-symbol-tables* nil)

(defpackage i-mem-symbols
  ;; have an independent package of the correct size, so as to make reading faster
  (:nicknames "I")
  (:prefix-name "I")
  (:size 4000.)
  (:use))

(defun halts (&optional previous-info &aux version)
  "This function decodes the output of the why program on the SDU
Call this to find out what microcode procedures were being executed at the
time a machine is halted."
  (format t "~&Please enter the version number of the microcode the machine was running.~%")
  (load-i-mem-symbols-for-ucode-version
    (setq version (or (prompt-and-read '(:number :input-radix 10. :or-nil t)
                                       "UCODE version (default ~D)> "
                                       si:%microcode-version-number)
                      si:%microcode-version-number)))
  (format t "Please enter the PC's printed by the 'why' program.
Entering -1 exits this program, <~:C> does a reprint.~%" #\Return)
  (do ((pc) (meanings previous-info) (symbol))
      ((and (setq pc (prompt-and-read '(:number :input-radix 8. :or-nil t)
                                       "PC>"))
            (minusp pc))
       (cond ((y-or-n-p "Do you want to save the results in a file?")
              (let ((comment (prompt-and-read
                               '(:delimited-string :delimiter #\end)
                               "~&Input a comment about the crash, ending with ~C~%"
                               #\end))
                    (fname (prompt-and-read :pathname "~&Filename to save results to: ")))
                (with-open-file (stream fname :direction :output)
                  (format stream "Running Microcode ~D~%~A~%~{~A~%~}"
                          version comment meanings)
                  (values meanings (truename stream)))))
             ('else
              meanings)))
    (cond ((null pc)
           (format t "~&For Ucode version ~D~%~{~A~%~}" version meanings))
          ('else
           (setq symbol (lookup-i-mem-value pc *i-mem-symbol-table*))
           (let ((st (format nil "At PC=#o~6,'0O machine is at ~O" pc symbol)))
             (format t "~&~A~%" st)
             (setq meanings (append meanings (list st))))))))

(defvar *lmc-sym-file* "SYS:UBIN;ULAMBDA LMC-SYM")

(defun load-i-mem-symbols-for-ucode-version (n &aux temp)
  (cond ((setq temp (assoc n *i-mem-symbol-tables*))
         (setq *i-mem-symbol-table* (cdr temp)))
        ('else
         (format t "~&;Loading symbol table for ucode ~D ..." n)
         (let ((time (time)))
           (with-open-file (stream (send (pathname *lmc-sym-file*) :new-version n))
             (let ((*package* (find-package "I-MEM-SYMBOLS"))
                   (*read-base* 8)
                   (*readtable* (si:find-readtable-named "T")))
               (do ()
                   ((eq -2 (read stream))))
               (setq *i-mem-symbol-table* nil)
               (do ((symbol) (type) (value) (want-type (intern "I-MEM" package)))
                   ((eq -1 (setq symbol (read stream))))
                 (setq type (read stream))
                 (setq value (read stream))
                 (cond ((eq type want-type)
                        (push (cons symbol value) *i-mem-symbol-table*))))))
           (setq *i-mem-symbol-table*
                 (sort *i-mem-symbol-table* #'< :key #'cdr))
           (push (cons n *i-mem-symbol-table*) *i-mem-symbol-tables*)
           (describe-package 'i-mem-symbols)
           (format t "~&;Took ~D seconds realtime~%" (/ (time-difference (time) time) 60.0)))))
  (format t "~&Using symbols for microcode version ~D.~%" n))

(defun lookup-i-mem-value (pc table)
  (do ((l (cdr table) (cdr l))
       (previous (car table) (car l)))
      ((or (= pc (cdr previous))
           (null l)
           (< (cdr previous) pc (cdr (car l))))
       (if (= pc (cdr previous))
           (car previous)
         `(,(car previous) + ,(- pc (cdr previous)))))))


(defun iapropos (string)
  (subset #'(lambda (x)
              (string-search string (car x)))
          *i-mem-symbol-table*))

(defun active-ucode-table ()
  (car (rassoc *i-mem-symbol-table* *i-mem-symbol-tables*)))

(defun i-mem-dump-filename (version)
  (format nil "SYS:UBIN;ULAMBDA PC-LISTING ~D" version))

(defun i-mem-dump (&optional filename)
  (if filename
      (with-open-file (stream (i-mem-dump-filename (or (active-ucode-table)
                                                       (ferror nil "no active ucode table")))
                              :out)
        (format stream "Directions: Look up the PC's of the WHY program in this table~
                        ~%Copyright 1985, 1986 Lisp Machine Inc. For internal use only!~%")
        (i-mem-dump-s stream))
    (i-mem-dump-s standard-output)))

(defun i-mem-dump-s (stream)
  (format stream "~&For microcode version ~D~%" (active-ucode-table))
  (dolist (elem *i-mem-symbol-table*)
    (format stream "~6,'0o = ~A~%"
            (cdr elem) (car elem))))

(defun i-mem-dumpl (&rest versions)
  (mapcar #'(lambda (v)
              (let ((fname (i-mem-dump-filename v)))
                (cond ((probe-file fname)
                       (format t "~&; Already exists: ~S~%" fname))
                      ('else
                       (load-i-mem-symbols-for-ucode-version v)
                       (format t "~&;Writing ~S~%" fname)
                       (i-mem-dump t)))
                fname))
          versions))


(defun cpcap (l dir)
  (dolist (x l)
    (with-open-file (in x)
      (with-open-file (out (merge-pathnames (format nil "UCODE-~D.t"
                                                    (send (fs:parse-pathname x) :version))
                                            dir)
                           :out)
        (format t "~&; ~A ==> ~A~%"
                (send in :truename)
                (send out :truename))
        (stream-copy-until-eof in out)))))
