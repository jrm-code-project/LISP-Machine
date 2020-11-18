;;; -*- Mode:LISP; Package:USER; Base:10 -*-



(defun internet-addresses ()
  (let ((l (sort (subset #'(lambda (x) (send x :network-address :internet))
                         (mapcar #'cadr si:host-alist))
                 #'(lambda (x y) (< (send x :network-address :internet)
                                    (send y :network-address :internet))))))
    (when l
      (format t "~&Min INET address = ~D.~D.~D.~D, Max INET address = ~D.~D.~D.~D~%"
              (ldb (byte 8 24) (send (car l) :network-address :internet))
              (ldb (byte 8 16) (send (car l) :network-address :internet))
              (ldb (byte 8 08) (send (car l) :network-address :internet))
              (ldb (byte 8 00) (send (car l) :network-address :internet))
              (ldb (byte 8 24) (send (car (last l)) :network-address :internet))
              (ldb (byte 8 16) (send (car (last l)) :network-address :internet))
              (ldb (byte 8 08) (send (car (last l)) :network-address :internet))
              (ldb (byte 8 00) (send (car (last l)) :network-address :internet)))
      (dolist (e l)
        (format t "~20A ~D.~D.~D.~D~%"
                e
              (ldb (byte 8 24) (send e :network-address :internet))
              (ldb (byte 8 16) (send e :network-address :internet))
              (ldb (byte 8 08) (send e :network-address :internet))
              (ldb (byte 8 00) (send e :network-address :internet)))))))
