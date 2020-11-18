;;; -*- Mode: LISP; Syntax: Zetalisp; Package: DVI; Base: 10; -*-

;;get the output pathname from the input pathname
#+SYMBOLICS
(defun outfile (pname printer)
  (selector  printer string-equal
    ("IMAGEN" (send pname :new-type :impress))
    ("SCREEN" nil)
    ("DOVER" (send pname :new-type :press))
    ;;other printers come here
    (otherwise (bad-dvi "Unknown printer ~S" printer))
    ))

#+SYMBOLICS
(define-cp-command (com-process-dvi :comtab "Global")
                   ((dvi-file :pathname
                              :documentation "Name of DVI File"
                              :allow-multiple t
                              :default `(,(fs:default-pathname nil nil :dvi :newest))
                              :prompt (format nil "file [default ~A]" (first =default=)))
                    (printer (:enumeration :imagen :screen :dover)
                             :documentation "Type of printer"
                             :prompt "Type of printer"
                             :default :screen)
                    &key
                    (output-file :pathname
                                 :documentation "Output file"
                                 :default (outfile (first dvi-file) printer)
                                 )
                    (center  :boolean
                             :documentation "No to turn off centering of text"
                             :default t)
                    (margin  :number
                             :documentation "width of left margin in inches"
                             :prompt "left margin width"
                             :default 1)
                    (magnification (:enumeration :realsize :twice :two-and-a-half)
                                   :documentation "Size of font to use - screen display"
                                   :default :twice)
                                                ;(debug :boolean)
                                                ;(dump :boolean)
                    )
  (setq center-text center)
  (fs:merge-pathnames-and-set-defaults (first dvi-file))        ;for sticky file defaulting
  (let ((*screen-resolution*
          (selectq magnification
            (:realsize 80)
            (:twice  160)
            (otherwise  200))))
    (condition-case (x)
      (progn (format t "Processing ~S .... " (send (first dvi-file) :string-for-printing))
             (run-dvi (fs:merge-pathnames (first dvi-file))
                      output-file (string-upcase printer)
                      :margin margin)
             (if (not (string-equal printer "SCREEN"))
                 (format t "Done.   ~& Output is in file ~S."
                         (send output-file :string-for-printing)))
             (values))
      (dvi-error
       (send x :report error-output)))))

(defvar *do-page-p* nil)

(defun query-page-p (n1 n2)
  (let ((a (FQUERY '(:CHOICES (((T "Yes.") #/y #/t #\SP #\HAND-UP)
                               ((() "No.") #/n #\RUBOUT #\HAND-DOWN)
                               ((:PROCEED "Proceed.") #/p #\RESUME)
                               ((:QUIT    "Quit.") #/q #\end)))
                   "~&Print page ~D(~D)~%" n1 n2)))
    (cond ((eq a :proceed)
           (setq *do-page-p* nil)
           t)
          ((eq a :quit)
           (setq *do-page-p* #'(lambda (a b) a b nil))
           nil)
          (a))))


#+(OR LMI TI)
(defun process-dvi (dvi-file &optional (printer :screen)
                             &key (output-file nil)
                                  (center t)
                                  (margin 1)
                                  (magnification :twice)
                                  select-pages)
  (setq center-text center)
  (setq dvi-file (fs:merge-pathnames dvi-file))
  (setq output-file (fs:merge-pathnames (or output-file
                                            (send dvi-file :new-type printer))))
  (cond ((eq select-pages t)
         (setq *do-page-p* 'query-page-p))
        ((numberp select-pages)
         (setq *do-page-p* #'(lambda (p q)
                               q
                               (= p select-pages))))
        ((consp select-pages)
         (setq *do-page-p* #'(lambda (p q)
                               q
                               (mem #'= p select-pages))))
        ('else
         (setq *do-page-p* select-pages)))
  (let ((*screen-resolution*
          (if (integerp magnification)
              magnification
            (ecase magnification
              (:realsize 80)
              (:twice  160)))))
    (condition-case (x)
        (run-dvi (fs:merge-pathnames  dvi-file)
                 output-file
                 (string-upcase printer)
                 :margin margin)
      (dvi-error
       (send x :report error-output)))))


#+SYMBOLICS
(define-cp-command (com-show-dvi :comtab "Global")
                   ((dvi-file :pathname
                              :documentation "Name of DVI File"
                              :allow-multiple t
                              :default `(,(fs:default-pathname nil nil :dvi :newest))
                              :prompt (format nil "file [default ~A]" (first =default=)))
                    (magnification (:enumeration :realsize :twice :two-and-a-half)
                                   :documentation "Size of font to use"
                                   :default :twice))
  (fs:merge-pathnames-and-set-defaults (first dvi-file))        ;for sticky file defaulting
  (let ((*screen-resolution*
          (selectq magnification
            (:realsize 80)
            (:twice  160)
            (otherwise  200))))
    (format t "Processing ~S .... " (send (first dvi-file) :string-for-printing))
    (run-dvi (fs:merge-pathnames (first dvi-file)) nil "SCREEN")
    (values)))

#+(OR LMI TI)
(defun show-dvi (dvi-file &optional (magnification :twice) flush-font-cache)
  (when flush-font-cache
    (clrhash *font-definitions*))
  (process-dvi dvi-file "SCREEN" :magnification magnification))

#+SYMBOLICS (deff user:run-dvi #'run-dvi)

#+TI (globalize 'show-dvi)
#+TI (globalize 'process-dvi)
