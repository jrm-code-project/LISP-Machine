;;; -*- Mode:LISP; Package:USER; Readtable:ZL; Base:10 -*-
; DDBII
; Lisp program to validate a note name and compute its frequency.


(defvar middle-C 523.2411306)                   ; frequency of middle C in cycles per second.


(defconstant enharmonic '
  ((("C"   "C#"  "D"   "D#"  "E"   "F"   "F#"  "G"   "G#" "A"   "A#"  "B")      ; all enharmonic notes.
    ("B#"  "B##" "C##" "D#"  "D##" "E#"  "E##" "F##" "G#" "G##" "A#"  "A##"))
   (("C"   "Db"  "D"   "Eb"  "E"   "F"   "Gb"  "G"   "Ab" "A"   "Bb"  "B")
    ("Dbb" "Db"  "Ebb" "Fbb" "Fb"  "Gbb" "Gb"  "Abb" "Ab" "Bbb" "Cbb" "Cb"))))


(defun valid-note (note-name)                   ; returns list of indices to enharmonic, or nil if not found.
  (do ((x 0 (+ x 1))) ((= x 2))
    (do ((y 0 (+ y 1))) ((= y 2))
      (do ((z 0 (+ z 1))) ((= z 12))
        (if (equal (elt (elt (elt enharmonic x) y) z) note-name)
          (return-from valid-note (list x y z)))))))


(defun compute-frequency ()                                   ; main procedure loop...
  (format t "~%Frequency calculation procedure 2.2~%")        ; invoke by (compute-frequency) after
  (loop                                                       ; (load "note.lisp").
    (princ "Enter note name (in quotes): ")
    (let* ((note (read))
           (position (valid-note note)))
      (cond (position
             (loop
               (format t "~%Enter displacement from middle-c: ")
               (let* ((displacement (read)))
                 (cond ((numberp displacement)
                        (format t "~%frequency of ~A ~D octaves from middle-C is ~G cps~%"
                                note
                                displacement
                                (* middle-c (^ 2.0 (+ (// (third position) 12.0) displacement))))
                        (return))
                       (t
                        (format t "~%...~A is not a number..." displacement))))))       ; the frequency
            ((equalp note 'quit)
             (format t "~%see ya soon...~%")
             (return-from compute-frequency))
            ((null position)
             (format t "~%valid note names are:")
             (print enharmonic))))))
