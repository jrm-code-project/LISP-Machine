; -*- Mode:LISP; Package:STEVE; Readtable:CL; Base:10 -*-

;Copyright (c) May 1983 by Christopher Eliot
; and Massachusetts Institute of Technology.  All rights reserved.
;Permission to copy all or part of this material is granted, provided
; that the copies are not made or distributed for resale, the MIT
; copyright notice and reference to the source file and the software
; distribution version appear, and that notice is given that copying
; is by permission of Massachusetts Institute of Technology.


;;; Hacked up version of process-in-load-environment for the editor.

(defstruct (load-environment (:type :list) (:conc-name load-environment-))
 pathname binding-vars new-values init-fns file-exit-functions)

(defun null-load-environment ()
 (make-load-environment :pathname nil
                        :binding-vars nil
                        :new-values nil
                        :init-fns nil
                        :file-exit-functions nil))

;This function is what may be used to properly perform some action within
; a file-plist binding.  It, by convention, defines the semantics of
; file-attribute-bindings.  Note the pathname should be a real pathname.
; Note that this function does not deal with how we get the plist, just how
; we use it.
;Each property name on the plist is examined for a
; fs:file-attribute-bindings property.  If such a property is present,
; it is funcalled on three arguments:  the pathname, the property
; name, and the property value.  It should return four values [the
; last two may be defaulted]:  a list of variables to bind, a list of
; values to bind them to, a list of initialization functions to call,
; and a list of cleanup functions to call.  For the duration of the
; "loading", the variables will be bound (dynamically) to the given
; values.  Inside that binding environment, the initialization
; functions will be called to perform any other non-binding or
; asynchronous setups that may be required.  (The calculation of the
; values happens outside of the unwind-protected scoping, but the
; calling of the init-functions does not.)  The exit-functions are
; treated identically to those functions in file-exit-functions.  They
; also are called within the binding scope.
;At this time it is not guaranteed that multiple values are passed
; back correctly.
;
(defun calculate-buffer-environment (plist pathname)
  (let ((file-exit-functions file-exit-functions-default)
        (binding-vars ())
        (new-values ())
        (init-fns ()))
    (loop for (propname propval . crap) on plist by #'cddr
          as f = (or (get propname 'fs:file-attribute-bindings)
                     (get propname 'fs:file-property-bindings))
          when f
            do (or (errset
                    (multiple-value-bind (vars vals inits exits)
                      (funcall f pathname propname propval)
                      (if (not (=& (length vars) (length vals)))
                          (cerror t () ()
                                  "Fileplist fn ~S returned different # vars ~S as vals ~S"
                                  f vars vals)
                          (loop for var in vars for val in vals
                                do (push var binding-vars)
                                (push val new-values))
                          (setq init-fns (append inits init-fns)
                                exits (append exits file-exit-functions)))))
                   (with-error-line-remaining
                    (format terminal-io "Error in file property list"))))
    (make-load-environment :pathname pathname
                           :binding-vars binding-vars
                           :new-values new-values
                           :init-fns (nreverse init-fns)
                           :file-exit-functions file-exit-functions)))

;NOTE: this will accept NIL as an environment.
(defun process-in-saved-buffer-environment (environment funct args)
 (if (null environment)
     (lexpr-funcall funct args)
     (let ((pathname (load-environment-pathname environment))
           (binding-vars (load-environment-binding-vars environment))
           (new-values (load-environment-new-values environment))
           (init-fns (load-environment-init-fns environment))
           (file-exit-functions
            (load-environment-file-exit-functions environment)))
      (progv binding-vars new-values
        (mapc #'(lambda (f) (funcall f pathname)) init-fns)
        (unwind-protect (prog1 (lexpr-funcall funct pathname args)
                               (setq pathname ()))
         (mapc #'(lambda (f) (funcall f pathname)) file-exit-functions))))))


;; Local Modes:
;; Mode:LISP
;; Comment Column:40
;; Lisp WHEN Indent:1
;; Lisp UNLESS Indent:1
;; Lisp WITH-OUTPUT-TO-STRING Indent:1
;; END:
