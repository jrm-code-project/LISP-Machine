;;; -*- Syntax: Zetalisp; Package: DVI; Base: 10; Mode: LISP -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Sending a file to be spooled on the imagen.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#||
;; These should already be defined.  Certainly Press is defined by
;; system.  Impress and DVI are defined in impress and dvi stream
;; packages.

(fs:define-canonical-type :impress "IMPRESS"
  ((:vms :vms4 :tops-20 :tenex :unix42 :unix) "IMP" "IMPRESS"))

(fs:define-canonical-type :dvi "DVI"
  ((:vms :tops-20 :unix42 :unix) "DVI"))

(fs:define-canonical-type :press "PRESS"
  ((:tops-20 :tenex :unix42 :unix) "PRESS")
  (:fep "PRES")
  ((:vms :vms4 :msdos) "PRS"))
||#

(defvar *hostname* "zermatt")
(defvar *contact-string* "IMAGEN-SPOOL")
(defvar *no-of-copies* 1)
(defvar *reverse* nil)
(defvar *collate* nil)
(defvar *paper-type* nil)
(defvar *printer* nil)

#+SYMBOLICS
(define-cp-command (com-imagen :comtab "Global")
                   ((file :pathname
                          :allow-multiple t
                          :documentation "Name of file(s) to be sent to the Imagen"
                          :default `(,(fs:default-pathname nil nil :impress :newest))
                          :prompt (format nil "file [default ~A]" (first =default=)))
                    &key
                    (copies :number
                            :documentation "Number of copies to be printed"
                            :default 1)
                    (reverse :boolean
                             :documentation "Set page reversal"
                             :default nil)
                    (collate :boolean
                             :documentation "Turn on collation"
                             :default nil)
                    (paper-type :string
                                :documentation "Type of paper to be used for printing"
                                :prompt "paper type"
                                :default nil)
                    (printer :string
                             :documentation "Name of printer to be used"
                             :prompt "name of printer"
                             :default nil))
  (setq *no-of-copies* copies)
  (setq *reverse* reverse)
  (setq *collate* collate)
  (setq *paper-type* paper-type)
  (setq *printer* printer)
  (loop for f in file do
        (imagen f))
  (fs:merge-pathnames-and-set-defaults (first file));for sticky file defaulting
  (send standard-output :fresh-line)
  (send standard-output :tyo #\newline)
  (values))

(defmacro get-person()
  `(string-append
     (send (send si:*user* :name) :string)
     "@"
     (send net:local-host :name)))

(defmacro gen-tmpname (pname)
  `(string-append "z:>tex>spool>"
                  (string user-id)
                  (string (gensym))
                  (send ,pname :name) ".tmp"))

(defun imagen (filename) ;filename is of type pathname
  (let* ((ch-conn (chaos:connect *hostname* *contact-string*))
         (tmpname (gen-tmpname filename)))
    (format t "~&  Sending file ~A to 4th Floor Imagen via Zermatt ... " filename)
    (with-open-stream (ch-stream (chaos:make-stream ch-conn))
      (icopyf filename tmpname)
      (send ch-stream :line-out (format nil "file ~A" tmpname))
      (send ch-stream :line-out (format nil "name ~A" (send filename :string-for-printing)))
      (send ch-stream :line-out (format nil "owner ~S" (get-person)))
      (if ( *no-of-copies* 1)
          (send ch-stream :line-out (format nil "copies ~D" *no-of-copies*)))
      (if *reverse*
          (send ch-stream :line-out (format nil "pagereversal t")))
      (if *collate*
          (send ch-stream :line-out (format nil "pagecollation t")))
      (if *paper-type*
          (send ch-stream :line-out (format nil "paper ~A" *paper-type*)))
      (if *printer*
          (send ch-stream :line-out (format nil "printer ~A" *printer*)))
      (send ch-stream :force-output))
    (format t "Done")
    (chaos:remove-conn ch-conn)))

(deff user:imagen #'imagen)


;;this function is for taking into account character set translations
;;for printing text files because the imagen spooler recognizes only
;;the standard ASCII character set encoding which is used by the VAX.

(defun icopyf (inpath outfile)
  (with-open-file (istr inpath :direction :input :raw t :characters nil :byte-size 8)
    (if (> (send istr :tyipeek) #o215)   ;probably impress
        (copyf inpath outfile :characters nil :byte-size 8) ;simply copy the file.
        (with-open-file (ostr outfile :direction :output :if-exists :new-version
                              :if-does-not-exist :create)
          (loop for byte = (send istr :tyi)
                until (null byte)
                do
            (if ( byte #o215) ;because #\return turns into ASCII #o012 on vax
                (send ostr :tyo (logand byte #o177)) ;get rid of 8th bit
                (send ostr :tyo #o012)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;This sends press files to the dover via MC.  Does no checking on whether
;;file is a press file or not, and no bounds check on the number of
;;copies to be made.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+SYMBOLICS
(define-cp-command (com-dover :comtab "Global")
                   ((file :pathname
                          :allow-multiple t
                          :documentation "Name of file(s) to be sent to the Dover"
                          :default `(,(fs:default-pathname nil nil :press :newest))
                          :prompt (format nil "file [default ~A]" (first =default=)))
                    &key
                    (copies :number
                            :documentation "Number of copies to be printed"
                            :default 1))
  (setq *no-of-copies* copies)
  (loop for f in file do
        (dotimes (i *no-of-copies*)(dover f)))
  (fs:merge-pathnames-and-set-defaults (first file));for sticky file defaulting
  (send standard-output :fresh-line)
  (send standard-output :tyo #\newline)
  (values))

(defmacro gen-MC-tmpname (pname)
  `(string-append "MC:.dovr.;"
                  (substring (string user-id) 0 2)
                  (substring (string (gensym)) 1)
                  (send ,pname :name) " tmp"))


(defun dover (filename);filename is of type pathname
  "Send a file to the dover via MC"
  (with-open-file (instr filename :direction :input :byte-size 8 :characters nil)
    (format t "~&   Sending file ~A to Dover via MC ... " filename)
    (with-open-file (outstr (gen-mc-tmpname filename) :direction :output
                             :characters nil :byte-size 8)
      (if (plusp (\ (file-stream-length instr) 512.))
          (format t "~&  Error: Press file lenght not multiple of 512")
          (stream-copy-until-eof instr outstr)
          (format t "Done.")))))
