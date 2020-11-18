;;;-*- Mode:LISP; Package:HACKS; Base:8; Lowercase:YES; Readtable:ZL -*-

;;; Shared definitions for the hacks.

(defmacro with-real-time body
  `(let ((old-sb-state (si:sb-on)))
     (unwind-protect
       (progn
         (si:sb-on '(:keyboard))
         . ,body)
       (si:sb-on old-sb-state))))

;;; System for menu of demos

(defvar *demo-alist*
        nil
  "Menu item list.  Elements are (name :VALUE <value> :DOCUMENTATION <string>).
   <value>s are either forms to evaluate or lists of shape (MENU name . elements),
   where elements are recursively the same thing.")


(defmacro defdemo (name documentation &rest args)
  "For a simple demo, (DEFDEMO <name> <documentation> <form>).
   For a sub-menu, (DEFDEMO <name> <documentation> <sub-menu title> . <elements>)
   where each <element> is a list that looks like the cdr of a defdemo form."
  `(setq *demo-alist* (add-or-update-demo *demo-alist* ',name ',documentation ',args)))

(defstruct (demo-list-element (:type :list))
  demo-name
  (demo-value-symbol ':value)
  demo-value
  (demo-documentation-symbol ':documentation)
  demo-documentation)

;;; Given a demo list, add the new demo, or update the old demo of the same
;;; name, and return the updated demo list.
(defun add-or-update-demo (demo-list name documentation args)
  (let ((element (or (ass 'equalp name demo-list)
                     (car (push (make-demo-list-element demo-name name) demo-list)))))
    (setf (demo-documentation element) documentation)
    (setf (demo-value element)
          (if (= (length args) 1)
              ;; This is the simple form.
              (first args)
              ;; This is the hairy form.
              `(menu ,(first args) . ,(let ((list (cddr (demo-value element))))
                                        (dolist (x (rest1 args))
                                          (setq list
                                                (add-or-update-demo
                                                  list
                                                  (first x)
                                                  (second x)
                                                  (rest2 x))))
                                        list)))))
  demo-list)

(DEFDEMO "Quit" "Exit from these demos." NIL)

(defun demo (&optional
             (alist *demo-alist*) (name "Click the mouse to select the boxed demo."))
  "Provide the user with a menu of demos to select from."
  (setq alist (sortcar (copylist alist) #'string-lessp))
  (loop as choice = (tv:menu-choose alist name)
        until (or (null choice)
                  (and (stringp (car choice))
                       (string-equal (car choice) "Quit")))
        do
             (if (eq (car choice) 'menu)
                 (demo (cddr choice) (cadr choice))
               (catch-error-restart ((sys:abort error) "Terminate this demo.")
                 (eval choice)))))

(defflavor hof-window ()
           (tv:borders-mixin tv:bottom-box-label-mixin tv:window)
  (:default-init-plist :blinker-p nil :more-p nil :label ""))

(defmethod (hof-window :before :select) (&rest ignore)
  (funcall-self ':clear-input)
  (funcall-self ':set-label ""))

(compile-flavor-methods hof-window)

(defvar *hof-window* nil)

(defun hof-window nil
  (cond (*hof-window*)
        (t (setq *hof-window* (tv:make-window 'hof-window
                                                  ':position '(77 77)
                                                  ':inside-size '(1001 1001))))))
(defvar *little-hof-window* nil)

(defun little-hof-window nil
  (cond (*little-hof-window*)
        (t (setq *little-hof-window*
                 (tv:make-window 'hof-window
                          ':position '(500 500)
                          ':inside-size '(200 200))))))
