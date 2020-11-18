;;;-*- Mode:LISP; Package:K2; Readtable:CL; Base:10;  compile-in-roots:("K-GLOBAL") -*-

;;; KBUG streams

(export '(

          kbug-stream-initialize
          kbug-stream-peek-byte
          kbug-stream-peek-character
          kbug-stream-read-byte
          kbug-stream-read-character
          kbug-stream-write-byte
          kbug-stream-write-character
          ))

;;;||| Reworked file to remove (select-processor (:k ...) (:lambda ...))  10/14/88 --wkf

;;; These two functions are used to determine if the debug master
;;; side or the K machine side of a KBUG stream is being used in
;;; the correct direction.

(defun ldb (byte source)
  #+(target falcon) (if (vinc:fixnump byte)
                        (hw:ldb source byte 0.)
                      (etypecase byte
                        (byte-spec (ldb-hard-case byte source))))
  #+(target lambda) (lisp:ldb byte source))

(defun kbug-input-stream? (kbug-stream)
  "T if this processor is allowed to read from this stream."
  (=
    #+(target lambda) $$kbug-stream-flags-direction-FROM-k
    #+(target falcon) $$kbug-stream-flags-direction-TO-k
     (ldb %%kbug-stream-flags-direction
          (kbug-stream-flags kbug-stream))))

(defun kbug-output-stream? (kbug-stream)
  "T if this processor is allowed to write to this stream."
  (=
    #+(target lambda) $$kbug-stream-flags-direction-TO-k
    #+(target falcon) $$kbug-stream-flags-direction-FROM-k
     (ldb %%kbug-stream-flags-direction
          (kbug-stream-flags kbug-stream))))

(defun kbug-stream-pointer-1+ (kbug-stream pointer)
  "Given a KBUG stream and the value of one of its pointers, return the next location
that the pointer will point to.  That is, increment it but take into account
any necessary wrapping."
  (let ((new-pointer (1+ pointer)))
    (if (>= new-pointer (kbug-stream-end kbug-stream))
        (kbug-stream-base kbug-stream)
      new-pointer)))

(defun kbug-stream-advance-in-pointer (kbug-stream)
  "Increment the in-pointer of KBUG-STREAM."
  (setf (kbug-stream-in-pointer kbug-stream)
        (kbug-stream-pointer-1+ kbug-stream (kbug-stream-in-pointer kbug-stream))))

(defun kbug-stream-advance-out-pointer (kbug-stream)
  (setf (kbug-stream-out-pointer kbug-stream)
        (kbug-stream-pointer-1+ kbug-stream (kbug-stream-out-pointer kbug-stream))))

;;; Why read byte?  Because we would have to deal with the boxedness issue if we
;;; read a Q.

(defun initialize-kbug-stream (kbug-stream direction base end)
  (setf (kbug-stream-base  kbug-stream) base)
  (setf (kbug-stream-end   kbug-stream) end)
  (setf (kbug-stream-flags kbug-stream)
        (hw:dpb direction %%kbug-stream-flags-direction
                (kbug-stream-flags kbug-stream))))

(defun kbug-stream-needs-input (kbug-stream out-ptr)
  (do ()
      ((not (= (kbug-stream-in-pointer kbug-stream)  ;;wait for byte to exist in stream.
               out-ptr)))))

(defun kbug-stream-needs-output (kbug-stream in-ptr+1)
  (do ()
      ((not (= (kbug-stream-out-pointer kbug-stream)
               in-ptr+1)))))

;;; open code things and eliminate gratuitous memory references
(defun kbug-stream-peek-byte (kbug-stream)
  (unless (kbug-input-stream? kbug-stream)
    #+(target falcon) (trap::illop "Input requested from write-only KBUG stream")
    #+(target lambda) (global:ferror nil "Input requested from a write-only KBUG stream"))
  (%kbug-stream-peek-byte kbug-stream (kbug-stream-out-pointer kbug-stream)))

(defun %kbug-stream-peek-byte (kbug-stream out-ptr)  ;;optimized by --wkf
  #+(target lambda) (progn (when (= (kbug-stream-in-pointer kbug-stream) out-ptr)
                              (zl:process-wait "Readch" (kbug::make-fancy-wait-function
                                                          #'zl:false
                                                          #'(lambda ()
                                                              (not (= (kbug-stream-in-pointer kbug-stream)
                                                                      out-ptr))))))
                            (hw:dpb (ldb (byte 8 0) (kbug-get-comm-word out-ptr))
                                    (byte 8 0) 0))
   #+(target falcon) (progn (kbug-stream-needs-input kbug-stream out-ptr)
                            (hw:vma-start-read-no-transport-vma-unboxed-md-unboxed (+ kbug-base-addr out-ptr))
                            (hw:dpb (hw:read-md) (byte 8 0) 0)))

(defun kbug-stream-peek-character (kbug-stream)
  (cons:make-pointer vinc:$$dtp-character (kbug-stream-peek-byte kbug-stream)))

(defun kbug-stream-read-byte (kbug-stream)
  (prog1 (kbug-stream-peek-byte kbug-stream)
         (kbug-stream-advance-out-pointer kbug-stream)))

(defun kbug-stream-read-character (kbug-stream)
  (unless (kbug-input-stream? kbug-stream)
    #+(target falcon) (trap::illop    "Input attempted from write-only KBUG stream")
    #+(target lambda) (zl::ferror nil "Input attempted from write-only KBUG stream"))
  (let* ()
    #+(target falcon) (cons:make-pointer vinc:$$dtp-character (kbug-stream-read-byte kbug-stream))
    #+(target lambda) (lisp:code-char (kbug-stream-read-byte kbug-stream))))

;;; open code things and eliminate gratuitous memory references
(defun kbug-stream-write-character (kbug-stream character)
  (unless (kbug-output-stream? kbug-stream)
    #+(target falcon) (trap::illop "Output attempted to read-only KBUG stream")
    #+(target lambda) (zl::ferror nil "Output attempted to read-only KBUG stream"))
  (let* ((in-ptr   (kbug-stream-in-pointer kbug-stream))
         (in-ptr+1 (kbug-stream-pointer-1+ kbug-stream in-ptr)))
    #+(target lambda) (when (= (kbug-stream-out-pointer kbug-stream) in-ptr+1)
                         (zl:process-wait "Writech"
                                          (kbug::make-fancy-wait-function
                                            #'zl:false
                                            #'(lambda ()
                                                (not (= (kbug-stream-out-pointer kbug-stream)
                                                        in-ptr+1))))))
    #+(target falcon) (kbug-stream-needs-output kbug-stream in-ptr+1)
    (kbug-set-comm-word in-ptr character)
    (setf (kbug-stream-in-pointer kbug-stream)
          (kbug-stream-pointer-1+ kbug-stream in-ptr))))

(defun kbug-stream-write-byte (kbug-stream byte)
  (unless (kbug-output-stream? kbug-stream)
    #+(target falcon) (trap::illop "Output attempted to read-only KBUG stream")
    #+(target lambda) (zl::ferror nil "Output attempted to read-only KBUG stream"))
  (let* ((in-ptr   (kbug-stream-in-pointer kbug-stream))
         (in-ptr+1 (kbug-stream-pointer-1+ kbug-stream in-ptr)))
    #+(target lambda) (when (= (kbug-stream-out-pointer kbug-stream) in-ptr+1)
                         (zl:process-wait "Writech"
                                          (kbug::make-fancy-wait-function
                                            #'zl:false
                                            #'(lambda ()
                                                (not (= (kbug-stream-out-pointer kbug-stream)
                                                        in-ptr+1))))))
    #+(target falcon) (kbug-stream-needs-output kbug-stream in-ptr+1)
    (kbug-set-comm-word in-ptr byte)
    (setf (kbug-stream-in-pointer kbug-stream)
          (kbug-stream-pointer-1+ kbug-stream in-ptr))))


#+(target lambda)
(defun kbug-stream-string-out (kbug-stream string)
  (unless (kbug-output-stream? kbug-stream)
    #+(target falcon) (trap::illop "Output attempted to read-only KBUG stream")
    #+(target lambda) (zl:ferror nil "Output attempted to read-only KBUG stream"))
  (let ((base (kbug-stream-base kbug-stream))
        (end  (kbug-stream-end  kbug-stream))
        (chars-to-send (zl:length string)))
  (labels ((wait-for-space (string-index)
             (let* ((in-pointer  (kbug-stream-in-pointer  kbug-stream))
                    (in+1        (kbug-stream-pointer-1+  kbug-stream in-pointer)))
               #+(target lambda) (when (= (kbug-stream-out-pointer kbug-stream) in+1)
                                    (zl:process-wait "Writest"
                                                     (kbug::make-fancy-wait-function
                                                       #'zl:false
                                                       #'(lambda ()
                                                           (not (= (kbug-stream-out-pointer kbug-stream)
                                                                   in+1))))))
               #+(target falcon) (kbug-stream-needs-output kbug-stream in+1)
               ;; Now we have space, find out how much
               (let* ((out-pointer (kbug-stream-out-pointer kbug-stream))
                      (space (if (>= in-pointer out-pointer)
                                 (1- (+ (- end in-pointer)
                                        (- out-pointer base)))
                               (- (- out-pointer in-pointer) 2))))
                 (send-string string-index in-pointer space))))

           (increment-stream-pointer (pointer)
             (let ((new (1+ pointer)))
               (if (>= new end)
                   base
                   new)))

           (send-string (string-index buffer-index space)
             (cond ((= string-index chars-to-send)
                    (setf (kbug-stream-in-pointer kbug-stream) buffer-index))
                   ((lisp:zerop space)
                    (setf (kbug-stream-in-pointer kbug-stream) buffer-index)
                    (wait-for-space string-index))
                   (t
                    (kbug-set-comm-word buffer-index (lisp::char string string-index))
                      (send-string (1+ string-index)
                                   (increment-stream-pointer buffer-index)
                                   (1- space))))))
    (wait-for-space 0))))

(defun kbug-stream-initialize (kbug-stream flags begin end)
  (setf (kbug-stream-base        kbug-stream) begin)
  (setf (kbug-stream-end         kbug-stream) end)
  (setf (kbug-stream-in-pointer  kbug-stream) begin)
  (setf (kbug-stream-out-pointer kbug-stream) begin)
  (setf (kbug-stream-flags       kbug-stream) flags))
