;;; -*- Mode:LISP; Package:NETWORK; Readtable:CL; Base:10 -*-

#|

  Copyright LISP Machine, Inc. 1987
   See filename "Copyright.Text" for
  licensing and release information.

|#

;;;***The following are not necessary when this is loaded into clean system
(eval-when (compile load eval)
  (unintern 'arp-ethernet-hardware 'ethernet)
  (unintern 'ip-ethernet-type 'ethernet)
  (unintern 'chaos-ethernet-type 'ethernet))

(export '( arp-ethernet-hardware
           arp-experimental-ethernet-hardware
           arp-pronet-hardware
           arp-chaos-hardware
           xns-ethernet-type
           ip-ethernet-type
           chaos-ethernet-type
           x.25-ethernet-type
           arp-ethernet-type
           defstream
           defop
           delete-from-alist
           swap-two-bytes
           make-statistics-block
           STAT-PS
           STAT-BS
           STAT-PR
           STAT-BR
           STAT-CURR
           STAT-LAST
           STAT-INST
           STAT-AVRG
           STAT-MAX
           add-network-statistics-block
           delete-network-statistics-block))

;;; Hardware types for the Address Resolution Protocol -- from RFC 960
(defconstant arp-ethernet-hardware 1 "ARP hardware type for the Ethernet")
(defconstant arp-experimental-ethernet-hardware 2 "ARP hardware type for the Experimental Ethernet")
(defconstant arp-pronet-hardware 4 "ARP hardware type for the Proteon ProNET Token Ring")
(defconstant arp-chaos-hardware 5 "ARP hardware type for the Chaosnet")

;;; Ethernet types -- from RFC 960
(defconstant xns-ethernet-type #x600 "Ethernet type code for XNS IDB protocol")
(defconstant ip-ethernet-type #x800 "Ethernet type code for DoD IP protocol")
(defconstant chaos-ethernet-type #x804 "Ethernet type code for chaos protocol")
(defconstant x.25-ethernet-type #x805 "Ethernet type code for X.25 Level 3")
(defconstant arp-ethernet-type #x806 "Ethernet type for address resolution protocol")

(defconstant ethernet:*ethernet-broadcast-address* #xffffffffffff)

;;; *** DEFSTREAM and DEFOP are an attempt to provide object oriented programming without assuming
;;;     that Flavors are available.  On the Lambda, they are available, and are used.  Otherwise,
;;;     We use defselect-incremental and the named-structure-invoke property of a defstruct.
;;; *** What I'd REALLY like is to be able to use #+flavors

#|  **** not for now.....

(defmacro defstream (struct-name components conc-name &rest instance-variables)
  (declare (zwei:indentation 3 1))
  `(progn
     (defflavor ,struct-name
              ,instance-variables
              ,(cond ((null components)
                      `())
                     ((consp (car components))
                      `(,(caar components)))
                     (t
                      `(,(car components))))
       :inittable-instance-variables
       (:default-init-plist ,@(if (cdr components) (keywordify-init-plist (cadr components))))
       :outside-accessible-instance-variables
       (:accessor-prefix ,conc-name)
       )
     (defmacro ,(intern (string-append "MAKE-" (string-upcase struct-name))) (&rest args)
       `(make-instance ',',struct-name ,@args))
     ))

(defun keywordify-init-plist (list)
  (do ((result nil)
       (args list (cddr args)))
      ((null args)
       (nreverse result))
    (push (intern (first args) 'keyword) result)
    (push (second args) result)))

(defmacro defop ((struct operation) &optional arguments &body body)
  `(defmethod (,struct ,operation) ,arguments
     ,@body))
|#

(defmacro defstream (struct-name components conc-name &rest instance-variables &aux (default-handler 'ignore))
  (declare (zwei:indentation 3 1))
  (when (consp struct-name)
    (setq default-handler (second struct-name))
    (setq struct-name (first struct-name)))
  `(progn
     ,(if components
          `(defstruct (,struct-name
                       (:include ,@components)
                       (:conc-name ,conc-name))
             ,@instance-variables)
        `(defstruct (,struct-name
                     (:conc-name ,conc-name))
           ,@instance-variables))
     (defselect-incremental (:property ,struct-name named-structure-invoke) ,default-handler)
     (net:create-included-methods ',struct-name)))

(defmacro defop ((struct operation) &optional arguments &body body)
  `(defun (:select-method (:property ,struct named-structure-invoke) ,operation)
          (operation self ,@arguments)
     (declare (ignore operation))
     ,@body))

(defun create-included-methods (struct-name)
  (let* ((description (get struct-name 'si:defstruct-description))
         (include (si:defstruct-description-include description))
         (included-select-method (and include (get (car include) 'si:named-structure-invoke)))
         (operation-list (and included-select-method (funcall included-select-method :which-operations)))
         )
    (dolist (op operation-list)
      (case op
        ((:which-operations :send-if-handles :get-handler-for :operation-handled-p))
        (otherwise (zl:fdefine `(:select-method (:property ,struct-name si:named-structure-invoke) ,op)
                               (zl:get-handler-for included-select-method op)))))))

(defmacro delete-from-alist (key list)
  `(setf ,list (delete (assoc ,key ,list) ,list)))

(defmacro swap-two-bytes (w)
  `(dpb (ldb (byte 8 0) ,w) (byte 8 8) (ldb (byte 8 8) ,w)))

;;;STATISTICS BLOCKS
;;;  Network statistics are stored in a 4 by 5 array
;;;  One column for packets sent, packets received, bytes sent, bytes received
;;;  One row for current value, last value, instantaneous rate, average rate, and maximum rate
;;;Only the first row is maintained by the actual network entity; the rest are maintained by
;;;a clock function called by the scheduler

(defmacro make-statistics-block ()
  '(make-array '(4 5) :initial-element 0))

(defconstant STAT-PS 0 "Packets Sent")
(defconstant STAT-BS 1 "Bytes Sent")
(defconstant STAT-PR 2 "Packets Received")
(defconstant STAT-BR 3 "Bytes Received")

(defconstant STAT-CURR 0 "Current value of statistic")
(defconstant STAT-LAST 1 "Last value of statistic")
(defconstant STAT-INST 2 "Instantaneous rate")
(defconstant STAT-AVRG 3 "Averaged rate")
(defconstant STAT-MAX  4 "Maximum rate")

(defvar *network-statistics-blocks* nil "List of statistics block maintained by clock function")

(defmacro add-network-statistics-block (array)
  `(without-interrupts
     (dotimes (i 4)
       (setf (aref ,array i STAT-LAST) (aref ,array i STAT-CURR)))
     (pushnew ,array *network-statistics-blocks*)))

(defmacro delete-network-statistics-block (array)
  `(without-interrupts
     (setq *network-statistics-blocks* (delete ,array *network-statistics-blocks*))))
