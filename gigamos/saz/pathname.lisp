;;; -*- Mode:LISP; Package:FILE-SYSTEM; Readtable:CL; Base:10 -*-

(DEFMETHOD (PATHNAME :device-directory-string) ()
  ;;;ensure we get just the device and directory
  ;; :string-for-printing usually gives the whole thing, but
  ;; could change
  (let* ((whole-name (SEND SELF :STRING-FOR-PRINTING))
         (first-semi (string-search-char #\; whole-name)))
    ;; commented-out code is smart about
    ;; defaulting in the absence of either a device or
    ;; a directory specification.
    (setq first-semi
          (if first-semi (1+ first-semi)
            (1+ (string-search-char
                  #\;
                        (setq whole-name (send (merge-pathnames
                                        self (default-pathname))
                                        :string-for-printing))))))
    (if (= (length whole-name)
           ;; nil must never match
           (or first-semi -1))
        ;; we started out with the right thing.
        whole-name
      ;;process and return the string up to and including the last
      ;;field ending with a semi-colon.
      (do* ((first-field  ""         (substring rest-of-name
                                                0 first-semi))
            (rest-of-name whole-name (substring rest-of-name
                                                first-semi))
            (out-string   ""         (string-append first-field out-string)))
           ((or (null rest-of-name)
                (not (string-search-char #\; rest-of-name)))
            out-string)))))
