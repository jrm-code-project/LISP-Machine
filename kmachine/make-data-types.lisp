;;; -*- Mode:LISP; Package:USER; Readtable:CL; Base:10 -*-


;;;;; Master list of all the datatypes.
;;;;;     Codes 0 through 31 are the visible datatypes.
(defconstant $$datatype-assoc-list
             '(( 0. . $$dtp-nil)
               ( 1. . $$dtp-fixnum)
               ( 2. . $$dtp-cons)
               ( 3. . $$dtp-symbol)
               ( 4. . $$dtp-bignum)
               ( 5. . $$dtp-short-float)
               ( 6. . $$dtp-float)
               ( 7. . $$dtp-double-float)
               ( 8. . $$dtp-rational)
               ( 9. . $$dtp-complex)
               (10. . $$dtp-locative)
               (11. . $$dtp-unboxed-locative)
               (12. . $$dtp-compiled-function)
               (13. . $$dtp-code)
               (14. . $$dtp-array)
               (15. . $$dtp-stack-group)
               (16. . $$dtp-instance)
               (17. . $$dtp-lexical-closure)
               (18. . $$dtp-interpreter-closure)
               (19. . $$dtp-lexical-environment)
             ; 20 spare - was once dtp-string
               (21. . $$dtp-character)
               (22. . $$dtp-extend)
               (23. . $$dtp-encapsulation)
             ; 24 spare
             ; 25 spare
             ; 26 spare
             ; 27 spare
             ; 28 spare
             ; 29 spare
             ; 30 spare
             ; 31 spare

;;;;;      Codes 32 through 63 are the magic datatypes. (invisible, headers, unbound, etc
               (32. . $$dtp-unboxed-header)
               (33. . $$dtp-symbol-header)
               (34. . $$dtp-array-header-single)
               (35. . $$dtp-array-header-multiple)
               (36. . $$dtp-array-header-extension)
               (37. . $$dtp-external-value-cell-pointer)
               (38. . $$dtp-gc-forward)
               (39. . $$dtp-one-q-forward)
               (40. . $$dtp-indexed-forward)
               (41. . $$dtp-instance-header)
               (42. . $$dtp-array-leader-header)
               (43. . $$dtp-unbound)
               (44. . $$dtp-header-forward)
               (45. . $$dtp-body-forward)
             ; 46 spare
             ; 47 spare
             ; 48 spare
             ; 49 spare
             ; 50 spare
             ; 51 spare
             ; 52 spare
             ; 53 spare
             ; 54 spare
             ; 55 spare
             ; 56 spare
             ; 57 spare
             ; 58 spare
             ; 59 spare
             ; 60 spare
             ; 61 spare
             ; 62 spare
             ; 63 spare
 ))


;;; Generate defconstants for all the datatypes.
(defun generate-data-type-file ()
  (with-open-file (file "jb:k;data-types.lisp" :write)
    (format file ";;; -*- Mode:LISP; Package:Vinculum; Readtable:CL; Base:10 -*-")
    (format file "~%~%;;; Note - this file is generated automatically by jb:k;make-data-types.lisp")
    (format file "~%~%(export '(")
    (dolist (dt $$datatype-assoc-list)
      (format file "~%          ~(~A~)" (cdr dt)))
    (format file "~%            ))~%")
    (dolist (dt $$datatype-assoc-list)
      (format file "~%(defconstant ~(~A~) ~D)" (cdr dt) (car dt)))))
