;;; -*- Mode:LISP; Package:(NC LISP); Base:10; Readtable:CL -*-

;;;; KBIN Sleazy File Loader

;;; This loads files compiled with ncompile-file.
;;; You have to link and load the functions into the actual machine
;;; using kbug:load-fcns


(zl:defsubst kbin-read (stream)
  (read stream))

(zl:defsubst kbin-read-byte (stream)
  (read-byte stream))

(defun kbin-read-instruction (stream)
  (let ((b0 (kbin-read-byte stream))
        (b1 (kbin-read-byte stream))
        (b2 (kbin-read-byte stream))
        (b3 (kbin-read-byte stream))
        (b4 (kbin-read-byte stream))
        (b5 (kbin-read-byte stream))
        (b6 (kbin-read-byte stream))
        (b7 (kbin-read-byte stream)))
    (dpb b7 (byte 8. 56.)
         (dpb b6 (byte 8. 48.)
              (dpb b5 (byte 8. 40.)
                   (dpb b4 (byte 8. 32.)
                        (dpb b3 (byte 8. 24.)
                             (dpb b2 (byte 8. 16.)
                                  (dpb b1 (byte 8. 8.) b0)))))))))

(defun kbin-read-code (stream)
  (let ((code '()))
    (dotimes (n (kbin-read stream))
      (push (kbin-read-instruction stream) code))
    (nreverse code)))

(defvar *fasl-op-handler-table* (make-array n-fasl-ops))

(defmacro define-fasl-op-handler (name lambda-list &body body)
  `(progn
     (defun ,name ,lambda-list
       ,@body)
     (setf (aref *fasl-op-handler-table* ,name) #',name)))


(defun nfasload (filename &key (package *package*))
  (let ((*package* package))
    (let ((path (fs:merge-pathname-defaults filename nil "KBIN")))
      (with-open-file (stream path :direction :input)
        (do ((op (read-byte stream nil 'done)(read-byte stream nil 'done)))
            ((eq op 'done))
          (funcall (aref *fasl-op-handler-table* op)
                   stream)))
      path)))

(define-fasl-op-handler FASL-OP/DEFUN (stream)
  (let ((name        (kbin-read stream))
        (lambda-list (kbin-read stream))
        (local-refs  (kbin-read stream))
        (refs        (kbin-read stream))
        (immediates  (kbin-read stream))
        (code        (kbin-read-code stream)))
    (create-ncompiled-function name code local-refs refs immediates)))

;;; This doesn't install the macro
(define-fasl-op-handler FASL-OP/MACRO (stream)
  (let ((name        (kbin-read stream))
        (lambda-list (kbin-read stream))
        (local-refs  (kbin-read stream))
        (refs        (kbin-read stream))
        (immediates  (kbin-read stream))
        (code        (kbin-read-code stream)))
    (create-ncompiled-function name code local-refs refs immediates)))

;;; More here too
(define-fasl-op-handler FASL-OP/SUBST (stream)
  (let ((name        (kbin-read stream))
        (lambda-list (kbin-read stream))
        (body        (kbin-read stream))
        (local-refs  (kbin-read stream))
        (refs        (kbin-read stream))
        (immediates  (kbin-read stream))
        (code        (kbin-read-code stream)))
    (create-ncompiled-function name code local-refs refs immediates)))



(define-fasl-op-handler FASL-OP/EVAL (stream)
  (eval (kbin-read stream)))
