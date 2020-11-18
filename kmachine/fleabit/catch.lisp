;;; -*- Mode:LISP; Package:LISP-INTERNALS; Readtable:CL; Base:10 -*-

;;;; CATCH, THROW and UNWIND-PROTECT

(in-package 'lisp-internals)

(export '(CATCH THROW UNWIND-PROTECT) 'nlisp)

;;; Catch
;;;
;;; (catch <tag> <body>)
;;;
;;; (let* ((.tag. <tag>)
;;;        (*sg-established-tags* (cons .tag. *sg-established-tags*)))
;;;   (catch-open
;;;     'si:unwind-marker
;;;     .tag.
;;;     *special-pdl*
;;;     *stack-pointer*
;;;     <pc>
;;;     <body>))
;;;
(defmacro catch (tag &body body)
  ` ;(let* ((.tag. <tag>)
    ;       (*sg-established-tags* (cons .tag. *sg-established-tags*)))
     (catch-continue
       ;; this is so arg order isn't rearranged
       (setf (hw:O0) 'li:unwind-marker)
       (setf (hw:O1) ,tag)
       (setf (hw:O2) gr:*special-pdl*)
       (setf (hw:O3) gr:*stack-pointer*)
       (%catch-cont)
       (nc:%values (progn ,@body))
       (%catch-label)))

(defafun catch-continue (marker tag spdl sptr pc body-value ignore)
  (return-tail a5))


;;; (unwind-protect
;;;     <form>
;;;   <cleanup>)
;;;
;;; (unwind-protect-continue
;;;      'si:unwind-marker
;;;      'si:unwind-protect-tag
;;;      *special-pdl*
;;;      *stack-pointer*
;;;      #'(lambda ()
;;;          <cleanup>)
;;;      <form>)
;;;
(defmacro unwind-protect (protected-form &body cleanup-forms)
  `(unwind-protect-continue
     'si:unwind-marker
     'si:unwind-protect-tag
     *special-pdl*
     *stack-pointer*
     #'(lambda ()
         ,@cleanup-forms)
     ,protected-form))


(defun unwind-protect-continue (marker tag spdl sptr cleanup-closure form-value)
  (funcall cleanup-closure)
  form-value)



(defmacro hw:return-code-mv-p ()
  `(hw:32logbitp
     (byte-position hw:%%processor-status-return-code)
     (hw:read-processor-status)))

(defun throw (tag value-1)
  (if (hw:return-code-mv-p)
      (throw-mv tag value-1)
    (throw-sv tag value-1)))

;;; Throw
;;;
;;; A catch frame contains:
;;; ----------------------
;;; O0: li:unwind-marker
;;; O1: <tag> li:unwind-protect-tag for unwind protect
;;; O2: *special-pdl*
;;; O3: *stack-pointer*
;;; O4: <pc> if throw <cleanup closure> if unwind protect
;;;
;;; Temporarily used:
;;; O5: <value> of catch or unwind body
;;;
;;; gr:*arg-1*    is throw tag
;;; gr:*arg-2*    is throw value
;;;
(defun throw-sv (tag value)
;  (if (memq tag *sg-established-tags*)
    (progn
      (setq gr:*arg-1* tag)
      (setq gr:*arg-2* value)
      (do-forever
        ;; or until we hopefully hit an
        ;; unwind protect at top of stack
        ;; ** scroll stack when hit bottom
        (if (= (hw:ldb hw:%%ch-oar-active (hw:read-open-active-return) 0)
               (hw:ldb hw:%%ch-oar-open (hw:read-open-active-return) 0))
            ;; was a call, pop it
            (hw:ch-return)
          ;; was open or topen, check it
          (when (eq (hw:O0) 'LI:UNWIND-MARKER)
            (unbind-to (hw:O2))
            (setq gr:*stack-pointer* (hw:O3))
            (if (eq (hw:O1) gr:*arg-1*)
                (progn
                  ;; this calls flush-catch rather than ch-call ch-return
                  ;; so that the mv-return bit gets cleared
                  ;; the value comes back in O5
                  (setf (hw:O5) (single-value gr:*arg-2*))
                  (hw:dispatch (hw:R4)))
              (when (eq (hw:O1) 'LI:UNWIND-PROTECT-TAG)
                (setf (hw:O0) NIL)               ;don't lose if cleanup throws
                (setf (hw:O1) gr:*arg-1*)        ;save tag and value
                (setf (hw:O5) gr:*arg-2*)
                (funcall (hw:O4))                ;execute cleanup forms
                (setq gr:*arg-2* (hw:O5))        ;and restore tag and value
                (setq gr:*arg-1*   (hw:O1)))
            (if (= (hw:ldb hw:%%ch-oar-active (hw:read-open-active-return) 0)
                   (get-CS-A-O))
                (hw:ch-call)
              (hw:ch-tcall))
            (hw:ch-return)))))
;   (error "There was no pending CATCH for the tag ~s" tag)
      ))

(defun single-value (v)
  v)

(defun flush-catch ()
  gr:*arg-2*)


(defun throw-mv (tag value1)
;  (if (memq tag *sg-established-tags*)
    (progn
      (setq gr:**arg-1* tag)
      (setq gr:*arg-2* value1)
      (do-forever
        ;; or until we hopefully hit an
        ;; unwind protect at top of stack
        ;; ** scroll stack when hit bottom
        (if (= (hw:ldb hw:%%ch-oar-active (hw:read-open-active-return) 0)
               (hw:ldb hw:%%ch-oar-open (hw:read-open-active-return) 0))
            ;; was a call, pop it
            (hw:ch-return)
          ;; was open or topen, check it
          (when (eq (hw:O0) 'LI:UNWIND-MARKER)
            (unbind-to (hw:O2))
            (setq gr:*stack-pointer* (hw:O3))
            (if (eq (hw:O1) gr:*arg-1*)
                (progn
                  ;; this calls flush-catch-mv so that the mv-return bit gets set
                  ;; the value comes back in R0
                  (setf (hw:O5) (multiple-values gr:*arg-2*))
                  (hw:dispatch (hw:R4)))
              (when (eq (hw:O1) 'LI:UNWIND-PROTECT-TAG)
                (setf (hw:O0) nil)               ;don't lose if cleanup throws
                (setf (hw:O1) gr:*arg-1*)        ;save tag and value
                (setf (hw:O5) gr:*arg-2*)
                ;; *** save rest of values
                (funcall (hw:O4))                ;execute cleanup forms
                ;; *** restore rest of values
                (setq gr:*arg-2* (hw:O5))        ;and restore tag and value
                (setq gr:*arg-1*   (hw:O1)))
            (if (= (hw:ldb hw:%%ch-oar-active (hw:read-open-active-return) 0)
                   (hw:ldb hw:%%ch-oar-active (get-CS-OA) 0))
                (hw:ch-call)
              (hw:ch-tcall))
            (hw:ch-return)))))
;   (error "There was no pending CATCH for the tag ~s" tag)
      ))


(defafun flush-catch-mv ()
  (return-mv gr:*arg-2*))

(defafun multiple-values (v1)
  (return-mv v1))

(defun get-CS-OA ()
  "Return the open and active frame numbers
from the top of the call stack."
  (setf (hw:A0) (hw:read-open-active-return))
  (setf (hw:A2) (hw:read-call-sp-hp))
  (hw:ch-return)
  (setf (hw:R1) (hw:read-open-active-return))   ;save active on stack
  (hw:write-open-active-return (hw:R0))         ;put back saved oar
  (hw:write-call-sp-hp (hw:A2))
  (hw:A1))
