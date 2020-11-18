;;; -*- Mode:LISP; Package:PRIMITIVE-ARITHMETIC; Readtable:CL; Base:10 -*-

(export '(
          %32*
          ))

(defun %32* (x y)
  ;; Multiply 2 unboxed numbers, illop if overflow.  Used before
  ;; bignums are loaded.
  (multiple-value-bind (low-half high-half)
      (signed-multiply-32 x y)
    ;; We win if the high half is all zero or all ones,
    ;; and if the top bit of the low half is the same.
    (if (zerop (hw:ldb low-half (byte 1. 31.) 0))
        (if (hw:32= high-half (hw:unboxed-constant 0.))
            low-half
            (trap::illop "%32* overflow."))
        (if (hw:32= high-half (hw:unboxed-constant -1.))
            low-half
            (trap::illop "%32* overflow.")))))

(defun unsigned-multiply-32 (x y)
  (let ((accumulate 0)
        (q-temporary))
    (trap::without-traps
      #'(lambda ()
          (hw:load-q-register x)
          (setq accumulate (hw:unsigned-multiply-first-step y accumulate))

          (setq accumulate (hw:unsigned-multiply-step y accumulate))
          (setq accumulate (hw:unsigned-multiply-step y accumulate))
          (setq accumulate (hw:unsigned-multiply-step y accumulate))
          (setq accumulate (hw:unsigned-multiply-step y accumulate))

          (setq accumulate (hw:unsigned-multiply-step y accumulate))
          (setq accumulate (hw:unsigned-multiply-step y accumulate))
          (setq accumulate (hw:unsigned-multiply-step y accumulate))
          (setq accumulate (hw:unsigned-multiply-step y accumulate))

          (setq accumulate (hw:unsigned-multiply-step y accumulate))
          (setq accumulate (hw:unsigned-multiply-step y accumulate))
          (setq accumulate (hw:unsigned-multiply-step y accumulate))
          (setq accumulate (hw:unsigned-multiply-step y accumulate))

          (setq accumulate (hw:unsigned-multiply-step y accumulate))
          (setq accumulate (hw:unsigned-multiply-step y accumulate))
          (setq accumulate (hw:unsigned-multiply-step y accumulate))

          (setq accumulate (hw:unsigned-multiply-last-step y accumulate))

          (setq q-temporary (hw:read-q-register))))
    (values q-temporary accumulate)))

(defun signed-signed-multiply-32 (x y)
  (let ((accumulate 0)
        (q-temporary))
    (trap::without-traps
      #'(lambda ()
          (hw:load-q-register x)
          (setq accumulate (hw:signed-multiply-first-step y accumulate))

          (setq accumulate (hw:signed-multiply-step y accumulate))
          (setq accumulate (hw:signed-multiply-step y accumulate))
          (setq accumulate (hw:signed-multiply-step y accumulate))
          (setq accumulate (hw:signed-multiply-step y accumulate))

          (setq accumulate (hw:signed-multiply-step y accumulate))
          (setq accumulate (hw:signed-multiply-step y accumulate))
          (setq accumulate (hw:signed-multiply-step y accumulate))
          (setq accumulate (hw:signed-multiply-step y accumulate))

          (setq accumulate (hw:signed-multiply-step y accumulate))
          (setq accumulate (hw:signed-multiply-step y accumulate))
          (setq accumulate (hw:signed-multiply-step y accumulate))
          (setq accumulate (hw:signed-multiply-step y accumulate))

          (setq accumulate (hw:signed-multiply-step y accumulate))
          (setq accumulate (hw:signed-multiply-step y accumulate))
          (setq accumulate (hw:signed-multiply-step y accumulate))

          (setq q-temporary (hw:read-q-register))))
    (values q-temporary accumulate)))
