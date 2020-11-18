;;; -*- Mode:LISP; Package:SIM; Readtable:CL; Base:10 -*-

;;;functional sources and destinations are stored at the end of the frame memory

(defconst %func-vma 0)
(defconst %func-vma-start-write 1)
(defconst %func-vma-start-read 2)
(defconst %func-md 3)
(defconst %func-return 4)
(defconst %func-instruction-counter 5)

(defun %p-contents-as-bignum (adr)
  (check-type adr integer)
  (dpb (%p-ldb (byte 16. 16.) adr)
       (byte 16. 16.)
       (%p-ldb (byte 16. 0) adr)))

(defun %p-store-contents-as-bignum (adr val)
  (check-type adr integer)
  (%p-dpb (ldb (byte 16. 0) val) (byte 16. 0) adr)
  (%p-dpb (ldb (byte 16. 16.) val) (byte 16. 16.) adr)
  val)

(defsetf %p-contents-as-bignum %p-store-contents-as-bignum)

(eh:def-ucode-format-error eh:sim-error "Simulator Microcode Error")

(defsubst read-register (name)
  (micro:%read-a-mem (get name 'micro:a-mem-adr)))

(defsubst read-register-as-pointer (name)
  (si:%make-pointer-unsigned (micro:%read-a-mem (get name 'micro:a-mem-adr))))

(defsubst write-register (name val)
  (let ((adr (get name 'micro:a-mem-adr)))
    (when (not (fixp adr))
      (ferror nil "bad address ~s" name))
    (micro:%write-a-mem adr val)))

(defsetf read-register write-register)

(defconst *main-memory-pages* 2048.)
(defconst *number-of-frames* 256.)
(defconst *pages-of-frames* 16.)
(defconst *pages-of-opcs* 16.)

(defvar *main-memory-array* (si:make-wireable-array (* 4 *main-memory-pages*) 'art-32b nil))
(defvar *frames-array* (si:make-wireable-array (+ *pages-of-frames* 1) 'art-32b nil))
(defvar *free-list-array* (si:make-wireable-array (ceiling *number-of-frames* si:page-size) 'art-32b nil))
(defvar *hram-array* (si:make-wireable-array (ceiling (* 2 *number-of-frames*) si:page-size) 'art-32b nil))
(defvar *opc-array* (si:make-wireable-array *pages-of-opcs* 'art-32b nil))

(defun main-memory-size ()
  (floor (array-length *main-memory-array*) 4))

(defconst *memory-cost* 3)

;;; this should only reset the processor registers - not the memory system
(defun reset-registers ()
  (write-register 'micro:a-sim-main-memory (+ (si:%pointer-unsigned (%pointer *main-memory-array*))
                                              (si:array-data-offset *main-memory-array*)))
  (write-register 'micro:a-sim-main-memory-size (main-memory-size))
  (write-register 'micro:a-sim-frames (+ (si:%pointer-unsigned (%pointer *frames-array*))
                                         (si:array-data-offset *frames-array*)))
  (write-register 'micro:a-sim-free-list (+ (si:%pointer-unsigned (%pointer *free-list-array*))
                                            (si:array-data-offset *free-list-array*)))
  (write-register 'micro:a-sim-hram (+ (si:%pointer-unsigned (%pointer *hram-array*))
                                       (si:array-data-offset *hram-array*)))
  (write-register 'micro:a-sim-opc-ram (+ (si:%pointer-unsigned (%pointer *opc-array*))
                                          (si:array-data-offset *opc-array*)))
  (write-register 'micro:a-sim-opc-ram-size (array-length *opc-array*))
  (write-register 'micro:a-sim-opc-ram-ptr 0)

  (write-register 'micro:m-sim-src-1-ptr 0)
  (write-register 'micro:m-sim-src-2-ptr 0)
  (write-register 'micro:m-sim-dest-ptr 0)
  (write-register 'micro:m-sim-last-src-1 0)
  (write-register 'micro:m-sim-last-aluf 0)
  (write-register 'micro:m-sim-inst-0 0)
  (write-register 'micro:m-sim-inst-1 0)
  (write-register 'micro:m-sim-inst-2 0)
  (write-register 'micro:m-sim-open-frame 0)
  (write-register 'micro:m-sim-active-frame 0)
  (write-register 'micro:m-sim-return-frame 0)

  (write-register 'micro:a-sim-pc 0)
  (write-register 'micro:a-sim-next-pc 0)
  (write-register 'micro:a-sim-noop-next-bit 0)
  (write-register 'micro:a-sim-last-src-2 0)
  (write-register 'micro:a-sim-last-result 0)

  (write-register 'micro:a-sim-src-ptrs-valid 0)

  (write-register 'micro:a-sim-inst-counter 0)
  (write-register 'micro:a-sim-last-memory-op 0)
  (write-register 'micro:a-sim-src-is-func 0)
  (write-register 'micro:a-sim-memory-cost *memory-cost*)

  (write-register 'micro:a-sim-free-list-ptr 0)

  (let ((free-list-base (read-register-as-pointer 'micro:a-sim-free-list)))
    (dotimes (i *number-of-frames*)
      (setf (%p-contents-as-bignum (%pointer-plus free-list-base i)) i)))

  (let ((first-real-frame (+ *constants-frame-base*
                             (ceiling (array-length *constants*) 16.))))
    (write-register 'micro:m-sim-return-frame first-real-frame)
    (write-register 'micro:m-sim-active-frame (+ first-real-frame 1))
    (write-register 'micro:m-sim-open-frame (+ first-real-frame 1))
    (write-register 'micro:a-sim-free-list-ptr (+ first-real-frame 2)))
  )



(defflavor ucode-sim
         (
          )
         ()
  :settable-instance-variables)

(defmethod (ucode-sim :after :init) (ignore)
  (send self :reset))

(defmethod (ucode-sim :reset) ()
  (reset-registers))

(defun check-array (array data-ptr)
  (when (not (eq data-ptr
                 (%pointer-plus (%pointer array)
                                (si:array-data-offset array))))
    (ferror nil "not the same array")))

(defmethod (ucode-sim :reset-and-clear) ()
  (reset-registers)
  (let ((main-memory-base (read-register-as-pointer 'micro:a-sim-main-memory)))
    (check-array *main-memory-array* main-memory-base)
    (setf (%p-contents-as-bignum main-memory-base) 0)
    (%blt main-memory-base (%pointer-plus main-memory-base 1) (array-length *main-memory-array*) 1))

  (let ((frames-base (read-register-as-pointer 'micro:a-sim-frames)))
    (check-array *frames-array* frames-base)
    (setf (%p-contents-as-bignum frames-base) 0)
    (%blt frames-base (%pointer-plus frames-base 1) (array-length *frames-array*) 1))

  (let ((free-list-base (read-register-as-pointer 'micro:a-sim-free-list)))
    (check-array *free-list-array* free-list-base)
    (setf (%p-contents-as-bignum free-list-base) 0)
    (%blt free-list-base (%pointer-plus free-list-base 1) (array-length *free-list-array*) 1))

  (let ((hram-base (read-register-as-pointer 'micro:a-sim-hram)))
    (check-array *hram-array* hram-base)
    (setf (%p-contents-as-bignum hram-base) 0)
    (%blt hram-base (%pointer-plus hram-base 1) (array-length *hram-array*) 1))

  (send *proc* :write-main-memory *free-pointer* #o1000)

  )


(defmethod (ucode-sim :read-main-memory) (adr)
  (when (or (< adr 0)
            (>= adr (floor (array-length *main-memory-array*) 4)))
    (ferror nil "bad adr"))
  (let* ((real-adr (si:%make-pointer-unsigned (+ (* 4 adr) (read-register 'micro:a-sim-main-memory))))
         (real-adr+1 (+ real-adr 1))
         (real-adr+2 (+ real-adr 2)))
    (+ (%p-ldb (byte 16. 0) real-adr)
       (dpb (%p-ldb (byte 16. 16.) real-adr) (byte 16. 16.) 0)
       (dpb (%p-ldb (byte 16. 0) real-adr+1) (byte 16. 32.) 0)
       (dpb (%p-ldb (byte 16. 16.) real-adr+1) (byte 16. 48.) 0)
       (dpb (%p-ldb (byte 16. 0) real-adr+2) (byte 16. 64.) 0)
       (dpb (%p-ldb (byte 16. 16.) real-adr+2) (byte 16. 80.) 0))))

(defmethod (ucode-sim :write-main-memory) (adr val)
  (when (or (< adr 0)
            (>= adr (floor (array-length *main-memory-array*) 4)))
    (ferror nil "bad adr"))
  (let* ((real-adr (si:%make-pointer-unsigned (+ (* 4 adr) (read-register 'micro:a-sim-main-memory))))
         (real-adr+1 (+ real-adr 1))
         (real-adr+2 (+ real-adr 2)))
    (%p-dpb (ldb (byte 16. 0) val) (byte 16. 0) real-adr)
    (%p-dpb (ldb (byte 16. 16.) val) (byte 16. 16.) real-adr)
    (%p-dpb (ldb (byte 16. 32.) val) (byte 16. 0) real-adr+1)
    (%p-dpb (ldb (byte 16. 48.) val) (byte 16. 16.) real-adr+1)
    (%p-dpb (ldb (byte 16. 64.) val) (byte 16. 0) real-adr+2)
    (%p-dpb (ldb (byte 16. 80.) val) (byte 16. 16.) real-adr+2))
  val)

(defmethod (ucode-sim :read-raw-hram-low) (index)
  (when (or (< index 0)
            (> index 256.))
    (ferror nil "index out of range ~s" index))
  (let ((hram-base (read-register-as-pointer 'micro:a-sim-hram)))
    (%p-contents-as-bignum (%pointer-plus hram-base (* 2 index)))))

(defmethod (ucode-sim :read-raw-hram-hi) (index)
  (when (or (< index 0)
            (> index 256.))
    (ferror nil "index out of range ~s" index))
  (let ((hram-base (read-register-as-pointer 'micro:a-sim-hram)))
    (%p-contents-as-bignum (%pointer-plus hram-base (1+ (* 2 index))))))


(defmethod (ucode-sim :read-h-active) (index)
  (ldb (byte 8 8) (send self :read-raw-hram-low index)))

(defmethod (ucode-sim :read-h-open) (index)
  (ldb (byte 8 0) (send self :read-raw-hram-low index)))

(defmethod (ucode-sim :read-h-pc) (index)
  (send self :read-raw-hram-hi index))

(defmethod (ucode-sim :previous-active) (frame)
  (send self :read-h-active frame))

(defmethod (ucode-sim :previous-open) (frame)
  (send self :read-h-open frame))

(defmethod (ucode-sim :frame-saved-pc) (frame)
  (send self :read-h-pc frame))



(defmethod (ucode-sim :read-frames) (adr)
  (when (or (< adr 0)
            (>= adr (* 256. 16.)))
    (ferror nil "bad adr"))
  (let ((frame-base (read-register-as-pointer 'micro:a-sim-frames)))
    (%p-contents-as-bignum (%pointer-plus frame-base adr))))

(defmethod (ucode-sim :write-frames) (adr val)
  (when (or (< adr 0)
            (>= adr (* 256. 16.)))
    (ferror nil "bad adr"))
  (let ((frame-base (read-register-as-pointer 'micro:a-sim-frames)))
    (setf (%p-contents-as-bignum (%pointer-plus frame-base adr)) val))
  val)

(defmethod (ucode-sim :read-open-frame) ()
  (read-register 'micro:m-sim-open-frame))

(defmethod (ucode-sim :write-open-frame) (val)
  (write-register 'micro:m-sim-open-frame val))

(defmethod (ucode-sim :read-open) (adr)
  (when (or (< adr 0)
            (>= adr 16.))
    (ferror nil "bad adr"))
  (send self :read-frames (dpb (send self :read-open-frame) (byte 8 4) adr)))

(defmethod (ucode-sim :write-open) (adr val)
  (when (or (< adr 0)
            (>= adr 16.))
    (ferror nil "bad adr"))
  (send self :write-frames (dpb (send self :read-open-frame) (byte 8 4) adr) val))

(defmethod (ucode-sim :read-active-frame) ()
  (read-register 'micro:m-sim-active-frame))

(defmethod (ucode-sim :write-active-frame) (val)
  (write-register 'micro:m-sim-active-frame val))

(defmethod (ucode-sim :read-active) (adr)
  (when (or (< adr 0)
            (>= adr 16.))
    (ferror nil "bad adr"))
  (send self :read-frames (dpb (send self :read-active-frame) (byte 8 4) adr)))

(defmethod (ucode-sim :write-active) (adr val)
  (when (or (< adr 0)
            (>= adr 16.))
    (ferror nil "bad adr"))
  (send self :write-frames (dpb (send self :read-active-frame) (byte 8 4) adr) val))

(defmethod (ucode-sim :read-return-frame) ()
  (read-register 'micro:m-sim-return-frame))

(defmethod (ucode-sim :write-return-frame) (val)
  (write-register 'micro:m-sim-return-frame val))

(defmethod (ucode-sim :read-return) (adr)
  (when (or (< adr 0)
            (>= adr 16.))
    (ferror nil "bad adr"))
  (send self :read-frames (dpb (send self :read-return-frame) (byte 8 4) adr)))

(defmethod (ucode-sim :write-return) (adr val)
  (when (or (< adr 0)
            (>= adr 16.))
    (ferror nil "bad adr"))
  (send self :write-frames (dpb (send self :read-return-frame) (byte 8 4) adr) val))

(defmethod (ucode-sim :read-pc) ()
  (read-register 'micro:a-sim-pc))

(defmethod (ucode-sim :write-pc) (val)
  (setf (read-register 'micro:a-sim-pc) val))

(defmethod (ucode-sim :read-next-pc) ()
  (read-register 'micro:a-sim-next-pc))

(defmethod (ucode-sim :write-next-pc) (val)
  (setf (read-register 'micro:a-sim-next-pc) val))

(defmethod (ucode-sim :read-noop-next-bit) ()
  (read-register 'micro:a-sim-noop-next-bit))

(defmethod (ucode-sim :write-noop-next-bit) (val)
  (setf (read-register 'micro:a-sim-noop-next-bit) val))

(defmethod (ucode-sim :read-free-list-ptr) (ignore)
  (read-register 'micro:a-sim-free-list-ptr))

(defmethod (ucode-sim :read-free-list) (adr)
  (let ((base (read-register-as-pointer 'micro:a-sim-free-list)))
    (%p-contents-as-bignum (%pointer-plus base adr))))

(defmethod (ucode-sim :read-functional) (adr)
  (when (or (< adr 0)
            (> adr 256.))
    (ferror nil "out of range"))
  (let ((frame-base (read-register-as-pointer 'micro:a-sim-frames)))
    (%p-contents-as-bignum (%pointer-plus frame-base (+ adr 4096.)))))


(defmethod (ucode-sim :read-vma) (ignore)
  (let ((frame-base (read-register-as-pointer 'micro:a-sim-frames)))
    (%p-contents-as-bignum (%pointer-plus frame-base (+ 4096. %func-vma)))))

(defmethod (ucode-sim :read-md) (ignore)
  (let ((frame-base (read-register-as-pointer 'micro:a-sim-frames)))
    (%p-contents-as-bignum (%pointer-plus frame-base (+ 4096. %func-md)))))

(defmethod (ucode-sim :read-opc) (adr)
  (let ((opc-base (read-register-as-pointer 'micro:a-sim-opc-ram))
        (opc-pointer (read-register 'micro:a-sim-opc-ram-ptr))
        (opc-size (read-register 'micro:a-sim-opc-ram-size)))
    (let ((offset (mod (+ opc-size opc-pointer (- adr)) opc-size)))
      (when (or (< offset 0)
                (>= offset opc-size))
        (ferror nil "bad opc pointer"))
      (%p-contents-as-bignum (%pointer-plus opc-base offset)))))

(defmethod (ucode-sim :check) ()
  (check-array *main-memory-array* (read-register-as-pointer 'micro:a-sim-main-memory))
  (when (not (= (read-register 'micro:a-sim-main-memory-size)
                (main-memory-size)))
    (ferror nil "main memory size bad"))
  (check-array *frames-array* (read-register-as-pointer 'micro:a-sim-frames))
  (check-array *free-list-array* (read-register-as-pointer 'micro:a-sim-free-list))
  (check-array *hram-array* (read-register-as-pointer 'micro:a-sim-hram))
  (check-array *opc-array* (read-register-as-pointer 'micro:a-sim-opc-ram))

  (mapcar #'(lambda (reg)
              (let ((val (read-register (car reg))))
                (when (or (< val (eval (cadr reg)))
                          (>= val (eval (caddr reg))))
                  (ferror nil "reg out of range ~s" reg))))
          '((micro:m-sim-src-1-ptr 0 (+ 256. 4096.))
            (micro:m-sim-src-2-ptr 0 (+ 256. 4096.))
            (micro:m-sim-dest-ptr 0 (+ 256. 4096.))
            (micro:m-sim-open-frame 0 256.)
            (micro:m-sim-active-frame 0 256.)
            (micro:m-sim-return-frame 0 256.)

            (micro:a-sim-pc 0 (array-length *main-memory-array*))
            (micro:a-sim-next-pc 0 (array-length *main-memory-array*))

            (micro:a-sim-opc-ram-ptr 0 (array-length *opc-array*))
            ))
  t)

(defmethod (ucode-sim :single-step) ()
  (send self :check)
  (write-register 'micro:a-sim-inst-count 1)
  (micro:%sim-step)
  )

(defmethod (ucode-sim :run) (&optional check-for-kbd-input)
  (send self :check)
  (write-register 'micro:a-sim-inst-count -1)
  (do-forever
    (let ((val (micro:%sim-step)))
      (cond ((eq val t)
             (when (and check-for-kbd-input
                      (read-char-no-hang))
             (return nil)))
            (t
             (ecase val
               (0 (return nil))                 ;saw halt bit
               ))))))

(defvar *last-sim-load-functions* nil)
(defvar *last-sim-load-start* nil)
(defvar *last-sim-load-args* nil)
(defun sim-reload ()
  (format t "~&(SIM-LOAD ~:FUNCTIONS ~s~&~
                           :START ~s~&~
                           :ARGS ~s)~~&"
          *last-sim-load-functions*
          *last-sim-load-start*
          *last-sim-load-args*)
  (sim-load :functions *last-sim-load-functions*
            :start *last-sim-load-start*
            :args *last-sim-load-args*))


(defun sim-test (func args &aux all-funcs jump-desc)
  (labels ((collector (function &aux x)
             (push function all-funcs)
             (cond ((setq x (get function 'sim-program))
                    (dolist (inst x)
                      (cond ((setq jump-desc (getf (cdr inst) '%%i-jump-adr))
                             ;; jump-desc is of the form (jump-target foobar)
                             (ecase (car jump-desc)
                               (local-jump-target)
                               (jump-target
                                (when (not (memq (cadr jump-desc) all-funcs))
                                  (collector (cadr jump-desc))))))
                            ((setq jump-desc (getf (cdr inst) 'immediate-data))
                             (when (and (eq (car jump-desc) 'function)
                                        (not (memq (cadr jump-desc) all-funcs)))
                               (collector (cadr jump-desc))))
                            )))
                   (t
                    (ferror nil "~s has no program" function)))))
    (collector func)
    (sim-load :functions all-funcs :start func :args args)))

(defun cold-reset ()
  (send *proc* :reset)
  (>invalidate-cons-cache)
  (send *proc* :write-main-memory *free-pointer* #o1000))

(defun sim-load (&key functions start args)
  (setq *last-sim-load-functions* functions)
  (setq *last-sim-load-start* start)
  (setq *last-sim-load-args* args)
  (send *proc* :reset)
  (clear-symbol-cache)
  (sim-colon-uab t)
  (clear-symbols)
  (>invalidate-cons-cache)
  (let ((*dont-cons* t))
    (let ((adr (send *proc* :read-main-memory *free-pointer*)))
      (dolist (f functions)
        (let ((n-inst (count-instructions f)))
          (when (> n-inst 256.)
            (ferror nil "don't know how to load big functions yet"))
          (let ((words-to-go (- 256. (remainder adr 256.))))
            (when (< words-to-go n-inst)
              (dotimes (i words-to-go)
                (send *proc* :write-main-memory (+ adr i) 0))
              (incf adr words-to-go)))
          (add-symbol f adr n-inst :inst)
          (incf adr n-inst)))
      (send *proc* :write-main-memory *free-pointer* adr)
      ))
  (dolist (f functions)
    (store-function-into-main-memory f (symbol-lookup f)))
  (install-constants)
  (let ((pc (symbol-lookup start)))
    (send *proc* :write-pc pc)
    (send *proc* :write-next-pc pc))
  (send *proc* :write-noop-next-bit 1)
  (do ((arg-number 0 (1+ arg-number))
       (arg args (cdr arg)))
      ((null arg))
    (send *proc* :write-active arg-number (car arg)))
  (write-register 'micro:a-sim-inst-counter 0)

  )

(defun read-inst-from-registers ()
  (+ (read-register 'micro:m-sim-inst-0)
     (ash (read-register 'micro:m-sim-inst-1) 32.)
     (ash (read-register 'micro:m-sim-inst-2) 64.)))
