;;; -*- Mode:LISP; Package:NIL; Base:10; Readtable:CL -*-
;;;
;;;
;;; VANILLA-TABLES.LISP


(defun make-table ()
  (si::make-array-into-named-structure
    (vector :TABLE :A-LIST NIL)))

(defun tablep (thing)
  (and (vectorp thing)
       (= (length thing) 3)
       (eq (aref thing 0) :TABLE)))

(defun table-type (table)
  (aref table 1))

(defun table-elements (table)
  (aref table 2))

(defun set-table-elements (table elements)
  (aset elements table 2))

(defun put-in-table (key value table)
  (let ((elements (table-elements table))
        (type     (table-type     table)))
    (case type
      (:A-LIST (let ((pair (assq key elements)))
                 (if pair
                     (rplacd pair value)
                     (set-table-elements table (cons (cons key value) elements)))))
      (:HASH   (puthash key value elements)))))

(defun get-from-table (key table &optional default)
  (let ((elements (table-elements table))
        (type     (table-type     table)))
    (case type
      (:A-LIST (let ((pair (assq key elements)))
                 (if pair (values (cdr pair) T) (values default NIL))))
      (:HASH   (gethash key elements default)))))
