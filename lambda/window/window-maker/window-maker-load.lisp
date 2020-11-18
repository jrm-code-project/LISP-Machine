;;;-*-Mode:lisp;Base:8;Package:user-*-
;;; Copyright C LISP MACHINE INC., 1985.
;;;

(fs:add-logical-pathname-host
  "window-maker"
  (let ((host (send fs:fdefine-file-pathname ':host)))
    (if (typep host 'fs:logical-host)
        (send host ':host)
      host))
  '(("SOURCE" "window-maker;")
    ("PATCH" "window-maker;")))


(SI:SET-SYSTEM-SOURCE-FILE "window-maker" "window-maker:SOURCE;window-maker-system")

(LOAD "window-maker:SOURCE;window-maker-system")

(make-system 'window-maker :compile :noconfirm)
