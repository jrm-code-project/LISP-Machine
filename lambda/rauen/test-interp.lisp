
;;; Declarations
(defun test-declarations ()
  (map 'list
       #'(lambda (test-form)
           (print (examine-declarations test-form)))
       test-declarations-forms)
  "Done")

(defvar test-declarations-forms
  (list
    '((declare (special a b))
      (declare (other a c))
      "doc string"
      (declare)
      (declare (foo bar) (special e) (special b))
      (+ 1 2)
      (declare (special f)))
    '((declare (special a) (special a))
      (declare (special a b) (special b)))
    ))

(first-pass-eval
  '(let ((a 2)
         (b 3))
     (+ a b)))

(+ 1 2)

'(let ((a 2) (b a)) (+ a b))
'(let* ((a 2) (b a)) (+ a b))

