;;; -*- Mode:LISP; Package:USER; Base:10 -*-



(DEFUN FIND-ILL-FILES ()
  (DOLIST (H (UP-LISPM-FILE-HOSTS))
    (CATCH-ERROR (FIND-ILL-FILES-ON-HOST H))))

(DEFUN FIND-ILL-FILES-ON-HOST (H)
  (DOLIST (D (CDR (FS:DIRECTORY-LIST (FORMAT NIL "~A:~~;*.DIRECTORY#>" H))))
    (WHEN (GET D :DIRECTORY)
      (LISTF (FORMAT NIL "~A:~A;*.ILL#>" H (SEND (CAR D) :NAME))))))


(DEFUN UP-LISPM-FILE-HOSTS ()
  (MAPCAR #'(LAMBDA (H) (SEND H :NAME))
          (CHAOS:UP-HOSTS (SUBSET #'(LAMBDA (H) (AND H (EQ :LISPM (SEND H :SYSTEM-TYPE))))
                                  (MAPCAR 'CADR SI:HOST-ALIST))
                          NIL
                          (* 60 20))))


