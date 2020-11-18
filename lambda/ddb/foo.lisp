;;; -*- Mode:LISP; Package:FOO; Base:10; Syntax: Common-Lisp -*-


#|

c-A   Begining of Line
c-B   Back one character
c-D   Delete next character  m-D  Delete next word
c-E   End of Line
c-F   Next Character  m-F Next Word c-m-F Next Expression
c-G   Quit an editor command
c-K   Kill rest of line c-m-K Kill next Expression
c-L   Redisplay & recenter window.
c-N   Next Line
c-O   Open next line  c-m-O Open next line, moving vertically
c-P   Previous Line
c-R   Reverse Search
c-S   Forward Search
c-T   Exchange Characters m-T Exchange Words c-m-T Exchange Expressions
c-U   Argument prefix (x 4)
c-V   Next Screen m-V Previous Screen
c-W   Kill Region m-W Copy into kill ring without deleting.
c-X   Prefix
   c-D  Show Directory (of current buffer, c-U c-X c-D to get to specify directory)
   c-S  Save File
   c-F  Find File
c-Y   Yank (inverse of Kill).  m-Y get previous thing killed.
c-Z   Dump you in the Lisp listener wondering what happened.

c-sh-U  Undo
c-sh-R  Redo


Tab   Indent for Lisp
Line  Next line & indent for Lisp (Return followed by Tab)
Rubout Delete previous character
m-Rubout Kill previous word
c-m-Rubout Kill previous Expression

C-Space  Start region

|#




(defun fact (x)
  (if (< x 2)
      x
    (* x (fact (1- x)))))

