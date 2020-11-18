


(defvar kbugstream t)
(defun mini-fasl-read-byte ()
  (lisp:read-byte kbugstream))

(defun hw:dpb (v bs w)
  (nlisp:dpb v bs w))


(defun cons:make-pointer (type data)
  (nlisp:dpb type vinc:%%data-type data))


(defun array:make-string (length)
  (lisp:make-string length))

(defun array:aset-1 (val array index)
  (lisp:setf (aref array index) val))
