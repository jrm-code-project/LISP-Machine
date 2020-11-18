;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:10; Readtable:ZL -*-


;;;
;;; (string-compare "foo19" "foo20")
;;; ==> -4
;;; (string-compare "foo19" "foo2")
;;; ==> -4
;;; (string-compare "foo20" "foo19")
;;; ==> 4
;;; (string-compare "foo2" "foo19")
;;; ==> 4
;;;

(DEFUN MY-STRING-LESSP (STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2 &aux num1 num2) ;CL compatible
  "True if STRING1 (or substring) is less than STRING2 (or substring) in dictionary order.
The value is actually the index of the first difference between the strings.
Keyword arg :START1 is where to start comparing, in STRING1, and :END1 where to stop.
Similar for :START2 and :END2.  Case is ignored when comparing letters."
  (LET ((vv (STRING-COMPARE STRING1 STRING2 START1 START2 END1 END2 NIL)))
    (if (minusp vv)                             ;string-compare indicates string1<string2 --
                                                ; only chance it could be wrong is if string1
                                                ; ends in a fixnum greater than string2.
        (if (and (fixp (setq num1 (read-from-string (substring string1 (1- (abs vv))))))
                 (fixp (setq num2 (read-from-string (substring string2 (1- (abs vv)))))))
            (if (< num1 num2)           ;only if string-compare happened to be right do we return
                (1- (ABS vv)))))))      ;this


(DEFUN MY-STRING-GREATERP (STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2) ;CL compatible
  "True if STRING1 (or substring) is greater than STRING2 (or substring) in dictionary order.
The value is actually the index of the first difference between the strings.
Keyword arg :START1 is where to start comparing, in STRING1, and :END1 where to stop.
Similar for :START2 and :END2.  Case is ignored when comparing letters."
  (LET* ((V (STRING-COMPARE STRING1 STRING2 START1 START2 END1 END2 NIL))
         (num1 (read-from-string (substring string1 (1- (abs v)))))
         (num2 (read-from-string (substring string2 (1- (abs v))))))
    (if (and (fixp num1)
             (fixp num2))
        (cond ((MINUSP V)                       ;string-compare indicates string1>string2 --
                                                ;only chance it could be wrong is if string1
                                                ;ends in a fixnum less than string2's trailing
                                                ;fixnum
               (if (> num1 num2)                ;only if string-compare happened to be right do we return
                   (1- (ABS V))))               ;this
              ((plusp V)
               (if (> num1 num2)

;; Copied from LAD: RELEASE-3.SYS2; STRING.LISP#161 on 2-Oct-86 04:36:56
(DEFUN STRING-NOT-GREATERP (STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2) ;CL compatible
  "True if STRING1 (or substring) is  to STRING2 (or substring) in dictionary order.
The value is actually the index of the first difference between the strings,
or their length if they match.
Keyword arg :START1 is where to start comparing, in STRING1, and :END1 where to stop.
Similar for :START2 and :END2.  Case is ignored when comparing letters."
  (LET ((V (STRING-COMPARE STRING1 STRING2 START1 START2 END1 END2 NIL)))
    (COND ((MINUSP V) (- (ABS V) 1))
          ((ZEROP V) (- (OR END1 (LENGTH STRING1)) START1)))))

(DEFUN STRING-NOT-LESSP (STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2) ;CL compatible
  "True if STRING1 (or substring) is  to STRING2 (or substring) in dictionary order.
The value is actually the index of the first difference between the strings,
or their length if they match.
Keyword arg :START1 is where to start comparing, in STRING1, and :END1 where to stop.
Similar for :START2 and :END2.  Case is ignored when comparing letters."
  (LET ((V (STRING-COMPARE STRING1 STRING2 START1 START2 END1 END2 NIL)))
    (COND ((PLUSP V) (1- V))
          ((ZEROP V) (- (OR END1 (LENGTH STRING1)) START1)))))
