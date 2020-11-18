;;; -*- Mode:LISP; Package:USER; Base:8; Readtable:ZL -*-

; Simple stuff for printing text files on angel (default).
; Instructions for use:
;   type (load "dj:bobp;unix-print.qfasl")
;
; Then use the normal zmacs (or other) text-printing commands:
;   <meta>-X print buffer
;   <meta>-X print region
;   <meta>-X print file
; To print in landscape mode, print without page headings,
; or select a different printer, type:
;   (set-unix-print-options)

; This was printed by marking this region and typing <meta>-X print region

;to-do:
;  n-copies
;  no imagen header page

(DEFUN (:PROPERTY :unix-printer si:PRINT-FILE) (PRINTER FILE-NAME
                                                  &OPTIONAL &KEY (FORMAT ':TEXT)
                                                  &ALLOW-OTHER-KEYS)
  (IF (EQ FORMAT ':TEXT)
      (WITH-OPEN-FILE-CASE (FILE-STREAM FILE-NAME ':DIRECTION ':INPUT)
        (FS:FILE-ERROR
         (SEND ERROR-OUTPUT ':FRESH-LINE)
         (SEND ERROR-OUTPUT ':STRING-OUT "unix printer error in opening file: ")
         (SEND FILE-STREAM ':REPORT ERROR-OUTPUT))
        (:NO-ERROR (unix-print-stream PRINTER FILE-STREAM)))
    (FORMAT ERROR-OUTPUT "~&unix systems only know about the TEXT format, not ~S." FORMAT)))

(DEFUN (:PROPERTY :unix-printer si:PRINT-STREAM) (PRINTER STREAM
                                      &OPTIONAL &KEY (FORMAT ':TEXT)
                                      file-name
                                      &ALLOW-OTHER-KEYS)
  (IF (EQ FORMAT ':TEXT)
      (unix-print-stream PRINTER STREAM :file-name file-name)
    (FORMAT ERROR-OUTPUT "~&unix systems only now about the TEXT format, not ~S." FORMAT)))

(defvar unix-print-landscape nil)               ;use to be :unknown
(defvar unix-print-header t)
(defvar unix-print-host-name "angel")
(defvar unix-print-printer-name "im1")

;(add-initialization "unix-print-landscape" '(setq unix-print-landscape :unknown))

(defun set-unix-print-options (&optional &key
                               (landscape unix-print-landscape l-supplied)
                               (header unix-print-header he-supplied)
                               (host unix-print-host-name ho-supplied)
                               (printer unix-print-printer-name p-supplied))
  (if (or l-supplied he-supplied ho-supplied p-supplied)
      (setq unix-print-landscape landscape
            unix-print-header header
            unix-print-host-name host
            unix-print-printer-name printer)
    (tv:choose-variable-values '((unix-print-landscape "Landscape mode" :boolean)
                                 (unix-print-header "Include page headings" :boolean)
                                 (unix-print-host-name "Host name" :string)
                                 (unix-print-printer-name "Printer name" :string))
                               :label "Unix IMPRINT parameters"
                               ))
  nil)

;; By this point, the FORMAT had darn well better be :TEXT...
(DEFUN unix-print-stream (PRINTER FROM-STREAM &key file-name)
  (if (eq unix-print-landscape :unknown)
      (set-unix-print-options))

  (let ((host (if (listp printer)
                  (SI:PARSE-HOST (SECOND PRINTER))
                unix-print-host-name))

        (contact-name
          (format nil
                  "EVAL imprint ~:[~;-P~:*~a~] ~
                   ~:[~*~;~a~] ~
                   ~:[~*~;~a~] ~
                   ~:[~*~;-I '/"-Dfor ~:@(~a~)/"'~] ~
                   ~:[~*~;-I '/"-Dserver ~:@(~a~)/"'~] ~
                   ~:[~*~;-I '/"-Ddirectory ~:@(~a~)/"'~] ~
                   ~:[~*~;-h '~a'~] ~
                   ~:[~*~;-I '/"-Dversion ~:@(~a~)/"'~]"
                  unix-print-printer-name
                  unix-print-landscape "-L"
                  (null unix-print-header) "-n"         ;use "cat" instead of "pr"
                  user-id user-id
                  file-name (extract-server-name file-name)
                  file-name (extract-directory-name file-name)
                  (and unix-print-header file-name)
                    (extract-file-name file-name)       ;can only give this to "pr"
                  file-name (extract-version-name file-name)
                  ))
        )
    (with-open-stream-case (stream
                            (chaos:open-stream
                              host
                              contact-name
;                             (substring contact-name 0 (min 125. (length contact-name)))
                              :direction :output))
      (FS:FILE-ERROR
       (SEND ERROR-OUTPUT ':FRESH-LINE)
       (SEND ERROR-OUTPUT ':STRING-OUT "unix printer error in opening file: ")
       (SEND STREAM ':REPORT ERROR-OUTPUT))
      (:NO-ERROR
       (do ((c (send from-stream ':tyi) (send from-stream ':tyi)))
           ((null c))
         (selectq c
           (#\epsilon (send from-stream ':tyi))
           (#\newline (send stream ':tyo 12))
           (#\tab (send stream ':tyo 11))
           (#\overstrike (send stream ':tyo 10))
           (#\page (send stream ':tyo 14))
           (t (send stream ':tyo c))))))))

(defun extract-server-name (name)
  (let* ((colon-position (position #/: name))
         (preceding-space-position (string-reverse-search-char #/space name colon-position))
         (start-position (1+ preceding-space-position)))
    (lobotomize-string (substring name start-position colon-position))))

(defun extract-directory-name (name)
  (let ((colon-position (position #/: name))
        (semicolon-position (position #/; name))
        (first-slash-position (position #// name))
        (last-slash-position (position #// name :from-end t)))
    (lobotomize-string
      (cli:remove
        #/space
        (cond
          (first-slash-position
           (substring name first-slash-position last-slash-position))
          ((and colon-position semicolon-position)
           (substring name (1+ colon-position) semicolon-position))
          (t
           "???"))))))

(defun extract-file-name (name)
  (let* ((semicolon-position (string-search-char #/; name))
         (last-slash-position (position #// name :from-end t))
         (sharp-position (position #/# name)))
    (lobotomize-string
      (cli:remove
        #/space
        (cond
          (semicolon-position
           (substring name (1+ semicolon-position) sharp-position))
          (last-slash-position
           (substring name (1+ last-slash-position) sharp-position))
          (t
           "???"))))))

(defun extract-version-name (name)
  (let* ((sharp-position       (position #/# name))
         (open-paren-position  (position #/( name :start (if sharp-position sharp-position 0)))
         (close-paren-position (position #/) name :start (if open-paren-position open-paren-position 0))))
    (cond
      (open-paren-position
       (substring name (1+ open-paren-position) close-paren-position))
      (t
       ""))))


(defun lobotomize-string (string)
  (substring string (max 0 (- (length string) 25.))))

(setq si:*default-printer* ':unix-printer)
