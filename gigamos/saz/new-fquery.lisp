;;; -*- Mode:LISP; Package:ZWEI; Base:10; Readtable:ZL -*-

(defun new-fquery-unless-arg (return-keywords echo-strings activators string-for-user &rest string-args)
  ;;  This function returns a special symbol if *numeric-arg* has been
  ;; provided to its caller; if not, it calls FQUERY with the lists provided.
  ;; A surrounding SELECTQ can catch :GO-AHEAD or any of the <RETURN-KEYWORDS>
  ;; provided.
  ;;  Caveat: This does no checking to see if args 1-3 are of equal length, though
  ;; they must be for proper functioning to occur
    (if *numeric-arg-p*
        :GO-AHEAD
      (loop for each-key in return-keywords
            and for echo-string in echo-strings
            and for each-char in activators
            with choice-list initially nil
            do
            (let* ((this-choice (list (list each-key echo-string) each-char)))
              (setq choice-list (append choice-list (list this-choice))))
            finally
            (return
              (apply
                #'completing-read-from-mini-buffer
                string-for-user
                (pairlis return-keywords)
                nil nil
                (append
                  (list (list :type :tyi :choices choice-list :stream *standard-output*))
                  (list fquery-string-for-user)
                  string-args))))))

(setq *fquery-help-stream* t)

(new-fquery-unless-arg '(:foo :bar) '("boo" "far") '(#/f #/b) "pick one! ")

(fquery '(:type :mini-buffer-or-readline :choices (((:foo "foo") "foo") ((:boo "boo") "boo" #/Space))) "pick one! ")

(y-or-n-p "foo")

(setq foo '(("apple" APPLE "a red fruit") ("dog" DOG "a nice pet") ("kitty" KITTY "a clean pet")))

(cdr (completing-read-from-mini-buffer "sodfn" foo nil nil "hi mike" ))



(DEFMACRO DEFINE-COMMAND-DOCUMENTATION (COMMAND &BODY BODY)
  "Define a documentation function for a ZWEI command.
There are three arguments provided for you:
    COM, the command symbol itself.
    CHAR, the character typed to get this command, or NIL.  If the second
       argument is NIL, that means that the caller does not have any particular
       character in mind (e.g. in List Commands).  The documentation-function
       should be prepared to deal with this case.
    OP, an operation which tells the function what to do.  They are:
       :NAME  Return your name as a string, (e.g. /"Self Insert/")
       :FULL  Type out full documentation to *STANDARD-OUTPUT*.
       :SHORT Type out short documentation to *STANDARD-OUTPUT*."
  `(DEFUN (:PROPERTY ,COMMAND DOCUMENTATION-FUNCTION) (COM CHAR OP)
     COM CHAR OP ; in case not used in body
     ,@BODY))
