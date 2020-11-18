;;; -*- Mode:Lisp; Package:(NC LISP); Readtable:CL; Base:10 -*-

;;;; Hardware primitives

(defmacro def-functional-source (name
                                 &optional
                                 (fs-name
                                   (intern (symbol-name name)
                                           'K))
                                 (doc
                                   (format nil "Read the ~a functional source" fs-name)))
  `(ndefmacro ,name ()
     ,doc
     `(read-functional-source ',',fs-name)))

(def-functional-source hw:read-processor-status      K::PROCESSOR-STATUS)
(def-functional-source hw:read-processor-control     K::PROCESSOR-CONTROL)
(def-functional-source hw:read-open-active-return    K::OPEN-ACTIVE-RETURN)
(def-functional-source hw:read-return-pc-return-dest K::RETURN-PC-RETURN-DEST)

(def-functional-source hw:read-call-hp-sp            K::CALL-HP-SP)     ;new name. HP bits 15-8, SP bits 7-0.
(def-functional-source hw:read-call-sp-hp            K::CALL-SP-HP)     ;old name
(def-functional-source hw:read-gc-ram                K::GC-RAM)
(def-functional-source hw:read-map                   K::MEMORY-MAP)
(def-functional-source hw:read-memory-control        K::MEMORY-CONTROL)
(def-functional-source hw:read-memory-status         K::MEMORY-STATUS)
(def-functional-source hw:read-microsecond-clock     K::MICROSECOND-CLOCK)

(def-functional-source hw:read-trap-pc               K::TRAP-PC)
(def-functional-source hw:read-trap-pc+              K::TRAP-PC+)
(def-functional-source hw:read-vma                   K::VMA)
(def-functional-source hw:read-md                    K::MD)
(def-functional-source hw:read-trap-register         K::TRAP-REGISTER)
(def-functional-source hw:read-statistics-counter    K:STATISTICS-COUNTER)
(def-functional-source hw:trap-off)

;;; Functional destinations

(defmacro def-functional-dest (name
                               &optional
                               (fdest-name
                                 (intern (symbol-name name)
                                         'K))
                               (doc
                                 (format nil "Write the ~a functional destination" fdest-name)))
  `(progn
     (setf (get ',fdest-name 'functional-dest-p) t)
     (ndefmacro ,name (value)
       ,doc
       `(write-functional-dest ',',fdest-name ,value))))


(def-functional-dest hw:datatype-ram-write-pulse)
(def-functional-dest hw:write-processor-control          K::PROCESSOR-CONTROL)
(def-functional-dest hw:write-open-active-return         K::OPEN-ACTIVE-RETURN)
(def-functional-dest hw:write-return-pc-return-dest      K::RETURN-PC-RETURN-DEST)
(def-functional-dest hw:write-call-hp-sp                 K::CALL-HP-SP)         ;new name
(def-functional-dest hw:write-call-sp-hp                 K::CALL-SP-HP)         ;old name

(def-functional-dest hw:write-memory-control             K::MEMORY-CONTROL)
(def-functional-dest hw:write-microsecond-clock          K::MICROSECOND-CLOCK)
(def-functional-dest hw:write-statistics-counter         K::STATISTICS-COUNTER)

(defmacro def-vma-functional-dest (name
                               &optional
                               (fdest-name
                                 (intern (symbol-name name)
                                         'K))
                               (doc
                                 (format nil "Write the ~a functional destination" fdest-name)))
  `(progn
     (setf (get ',fdest-name 'functional-dest-p) t)
     (ndefmacro ,name (value &optional boxed)
       ,doc
       `(write-functional-dest ',(case boxed
                                   (:boxed   ',(list fdest-name 'K::BOXED-VMA))
                                   (:unboxed ',(list fdest-name 'K::UNBOXED-VMA))
                                   (t ',fdest-name))
                               ,value))
     (ndefmacro ,(intern (format nil "~a-UNBOXED" name)
                         (symbol-package name))
                (value)
       ,doc
       `(,',name ,value :unboxed))
     (ndefmacro ,(intern (format nil "~a-BOXED" name)
                         (symbol-package name))
                (value)
       ,doc
       `(,',name ,value :boxed))))



(def-vma-functional-dest hw:write-vma                        K::VMA)
(def-vma-functional-dest hw:vma-start-write-no-gc-trap)
(def-vma-functional-dest hw:vma-start-write)


(defmacro def-md-functional-dest (name
                               &optional
                               (fdest-name
                                 (intern (symbol-name name)
                                         'K))
                               (doc
                                 (format nil "Write the ~a functional destination" fdest-name)))
  `(progn
     (setf (get ',fdest-name 'functional-dest-p) t)
     (ndefmacro ,name (value &optional boxed)
       ,doc
       `(write-functional-dest ',(case boxed
                                   (:boxed   ',(list fdest-name 'K::BOXED-MD))
                                   (:unboxed ',(list fdest-name 'K::UNBOXED-MD))
                                   (t ',fdest-name))
                               ,value))
     (ndefmacro ,(intern (format nil "~a-UNBOXED" name)
                         (symbol-package name))
                (value)
       ,doc
       `(,',name ,value :unboxed))
     (ndefmacro ,(intern (format nil "~a-BOXED" name)
                         (symbol-package name))
                (value)
       ,doc
       `(,',name ,value :boxed))))

(def-md-functional-dest hw:write-md                         K::MD)
(def-md-functional-dest hw:md-start-write-no-gc-trap)
(def-md-functional-dest hw:md-start-write)

(defmacro def-start-read-functional-dest (name
                                          &optional
                                          (fdest-name
                                            (intern (symbol-name name)
                                                    'K))
                                          (doc
                                            (format nil
                                                    "Write the ~a functional destination"
                                                    fdest-name)))
  `(progn
     (setf (get ',fdest-name 'functional-dest-p) t)
     (ndefmacro ,name (value &optional (vma-boxed-p :unboxed) (md-boxed-p :unboxed) (ticks-to-wait 1))
       ,doc
       `(progn (write-functional-dest ',(list ',fdest-name
                                              (case vma-boxed-p
                                                ((:boxed t)     'K::BOXED-VMA)
                                                ((:unboxed nil) 'K::UNBOXED-VMA))
                                              (case md-boxed-p
                                                ((:boxed t)     'K::BOXED-MD)
                                                ((:unboxed nil) 'K::UNBOXED-MD)))
                                      ,value)
               . ,(make-list ticks-to-wait :initial-element '(hw:memory-wait))))
     ;; this is ridiculous
     (ndefmacro ,(intern (format nil "~a-VMA-UNBOXED-MD-UNBOXED" name)
                         (symbol-package name))
                (value &optional (ticks-to-wait 1))
       ,doc
       `(,',name ,value :unboxed :unboxed ,ticks-to-wait))
     (ndefmacro ,(intern (format nil "~a-VMA-BOXED-MD-UNBOXED" name)
                         (symbol-package name))
                (value &optional (ticks-to-wait 1))
       ,doc
       `(,',name ,value :boxed :unboxed ,ticks-to-wait))
     (ndefmacro ,(intern (format nil "~a-VMA-UNBOXED-MD-BOXED" name)
                         (symbol-package name))
                (value &optional (ticks-to-wait 1))
       ,doc
       `(,',name ,value :unboxed :boxed ,ticks-to-wait))
     (ndefmacro ,(intern (format nil "~a-VMA-BOXED-MD-BOXED" name)
                         (symbol-package name))
                (value &optional (ticks-to-wait 1))
       ,doc
       `(,',name ,value :boxed :boxed ,ticks-to-wait))))


(def-start-read-functional-dest hw:vma-start-read-no-transport)
(def-start-read-functional-dest hw:vma-start-read)
(def-start-read-functional-dest hw:vma-start-read-visible-evcp)
(def-start-read-functional-dest hw:vma-start-read-will-write)
(def-start-read-functional-dest hw:vma-start-read-cdr-no-transport)
(def-start-read-functional-dest hw:vma-start-read-cdr)
(def-start-read-functional-dest hw:vma-start-read-cdr-visible-evcp)
(def-start-read-functional-dest hw:vma-start-read-cdr-will-write)

(def-start-read-functional-dest hw:vma-start-read-early-no-transport)
(def-start-read-functional-dest hw:vma-start-read-early)
(def-start-read-functional-dest hw:vma-start-read-early-visible-evcp)
(def-start-read-functional-dest hw:vma-start-read-early-will-write)
(def-start-read-functional-dest hw:vma-start-read-early-cdr-no-transport)
(def-start-read-functional-dest hw:vma-start-read-early-cdr)
(def-start-read-functional-dest hw:vma-start-read-early-cdr-visible-evcp)
(def-start-read-functional-dest hw:vma-start-read-early-cdr-will-write)




;;; Low level Register Access
;;; kluged up as functional sources and destinations

(defmacro def-reg-accessor (name reg)
  (let ((kname (intern (symbol-name name) 'K)))
    `(progn
       (def-functional-source ,name ,reg
         ,(format nil "Read the ~a register" name))
       (setf:defsetf ,name () (value)
         `(write-functional-dest ,',reg ,value)))))

(defmacro def-frame-accessors (name reg)
  (setq reg (eval reg))
  (do ((n 0 (1+ n))
       (forms '()))
      ((>= n hw:frame-size)
       `(progn . ,forms))
    (push `(def-reg-accessor ,(intern (format nil "~a~d" name n)
                                      (symbol-package name))
                             ,(+ reg n))
          forms)))


(def-frame-accessors hw::O O0)
(def-frame-accessors hw::A A0)
(def-frame-accessors hw::R R0)
