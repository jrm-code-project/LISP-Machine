;;; -*- Mode:LISP; Package:CLSCH; Readtable:CL; Base:10 -*-

(defvar *t-source-directory* "angel:/lmi3/pace/t")

(defpackage "T" :use ())

(defun find-definitions-in-t-file (file-name)
  (labels ((most-car (x)
             (do ()
                 ((not (consp x)) x)
               (setq x (car x)))))
    (with-open-file (s file-name)
      (let ((*package* (find-package "T"))
            (*readtable* scheme-readtable)
            (si:fdefine-file-pathname (send s :truename))
            (*eof* (ncons nil)))
        (do ((exp (read s nil *eof*)
                  (read s nil *eof*)))
            ((eq exp *eof*))
          (when (and (consp exp)
                     (symbolp (car exp))
                     (string-equal "DEFINE" (string (car exp)) :end2 6))
            (let ((defines (most-car (cdr exp))))
              (si:record-source-file-name defines))))))))

(defun scan-t-source-files ()
  (labels ((scan-t-directory (directory-name)
             (let ((files (fs:directory-list directory-name)))
               (dolist (file (cdr files))
                 (cond ((getf (cdr file) :directory)
                        (scan-t-directory (send (send (car file) :pathname-as-directory)
                                                :new-pathname
                                                :name :wild
                                                :type :wild
                                                :version :newest)))
                       ((string-equal (string (send (car file) :type)) "T")
                        (format t "~&Reading ~a" (car file))
                        (find-definitions-in-t-file (car file))
                        ))))))
    (scan-t-directory (send (send (fs:parse-pathname *t-source-directory*)
                                  :pathname-as-directory)
                            :new-pathname
                            :name :wild
                            :type :wild
                            :version :newest))))


(defvar *t-source-file-properties*)

(defun make-saveable-form ()
  (setq *t-source-file-properties* nil)
  (mapatoms #'(zl:lambda (x)
                (when (get x :source-file-name)
                  (push `(putprop ',x ',(get x :source-file-name) :source-file-name)
                        *t-source-file-properties*)))
            (find-package "T"))
  nil)

(defun save-t-source-properties ()
  (compiler:dump-forms-to-file
    "dj:pace;t-source-file-properties.qfasl"
    *t-source-file-properties*))

(defun load-t-source-properties ()
  (putprop (intern "QUOTE" "T")
           (get 'quote 'si:interpreter-special-form)
           'si:interpreter-special-form)
  (setf (symbol-function (intern "PUTPROP" "T"))
        #'putprop)
  (load "dj:pace;t-source-file-properties.qfasl" "T")
  (when (null (assq :t fs:*file-type-mode-alist*))
    (push (cons :t :t) fs:*file-type-mode-alist*)))

(advise fs:extract-attribute-list :around "Check for T files" nil
  (let ((stream (car arglist))
        pathname)
    (cond ((and (memq :pathname (send stream :which-operations))
                (setq pathname (send stream :pathname))
                (eq :t (send pathname :canonical-type)))
           (multiple-value-bind (plist error-p)
               :do-it
             (let ((possible-package (getf plist :package)))
               (when (not possible-package)
                 (setf (getf plist :package) :T))
               (values plist error-p))))
          (t
           :do-it))))

(si:compile-encapsulations 'fs:extract-attribute-list)

(defun t-source-files ()
  (let (pathnames)
    (mapatoms #'(zl:lambda (sym)
                  (labels ((map-leaves (tree func)
                                       (cond ((null tree))
                                             ((consp tree)
                                              (map-leaves (car tree) func)
                                              (map-leaves (cdr tree) func))
                                             (t
                                              (funcall func tree)))))
                    (map-leaves (symbol-plist sym)
                                #'(zl:lambda (x)
                                    (when (typep x 'fs:pathname)
                                      (pushnew x pathnames))))))
              (find-package "T"))
    pathnames))

(defun select-orbit ()
  (zwei:SELECT-FILE-LIST-AS-TAG-TABLE
    (mapcar #'(zl:lambda (x)
                (fs:merge-pathname-defaults x "angel:/lmi3/pace/t/comp/"))
            '("build/oload.t"
              "build/patches.t"
              "build/tree-table.t"
              "front_end/expanding_vector.t"
              "front_end/free_stuff.t"
              "front_end/type.t"
              "front_end/envs.t"
              "front_end/nodestuff.t"
              "front_end/alpha.t"
              "front_end/declare.t"
              "front_end/compilators.t"
              "front_end/node.t"
              "front_end/assign.t"
              "front_end/simplify.t"
              "front_end/simplify_call.t"
              "front_end/simplify_let.t"
              "front_end/param.t"
              "front_end/simplifiers.t"
              "front_end/simplify_y.t"
              "front_end/support.t"
              "front_end/gen_interface.t"
              "front_end/fixup.t"
              "front_end/user_error.t"
              "front_end/module.t"
              "front_end/analyze.t"
              "front_end/front.t"

              "top/sets.t"
              "top/defs.t"
              "top/util.t"
              "top/oprimops.t"
              "top/new_syntax.t"
              "top/top.t"


              "back_end/comex.t"
              "back_end/strategy.t"
              "back_end/live.t"
              "back_end/closure.t"
              "back_end/bookkeep.t"
              "back_end/generate.t"
              "back_end/parassign.t"
              "back_end/reg.t"

              "back_end/vaxemit.t"
              "back_end/vaxbookkeep.t"
              "back_end/vaxgen.t"
              "back_end/unvaxgen.t"
              "back_end/vaxlocgen.t"
              "back_end/vaxarithgen.t"
              "back_end/vaxrep.t"
              "primops/vaxconstants.t"


              "back_end/m68emit.t"
              "back_end/m68bookkeep.t"
              "back_end/m68gen.t"
              "back_end/aem68gen.t"
              "back_end/m68locgen.t"
              "back_end/m68arithgen.t"
              "back_end/m68rep.t"



              "assembler/as_open.t"
              "assembler/as_utils.t"
              "assembler/as.t"

              "assembler/fg.t"
              "assembler/ib.t"
              "assembler/count.t"
              "assembler/mark.t"
              "assembler/mini.t"
              "assembler/bits.t"
              "assembler/listing.t"

              "assembler/lap.t"

              "assembler/as_vax.t"
              "assembler/vmodes.t"
              "assembler/vaxis.t"
              "assembler/vaxi.t"
              "assembler/vaxam.t"


              "assembler/as_m68.t"
              "assembler/m68am.t"
              "assembler/m68is1.t"
              "assembler/m68is2.t"
              ))
    "ORBIT-ON-VAX"))



(when (format:y-or-n-p-with-timeout (* 60. 60.) t "Load T source pointers? ")
  (load-t-source-properties))
