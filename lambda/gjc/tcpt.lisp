;;; -*- Mode:LISP; Package:TCP; Base:10; Readtable:ZL -*-



(defun accept-one (remote-address remote-port)
  (let (socket normalp)
    (unwind-protect
        (let ((sa (tcp:make-socket-address :port 0 :internet-address *my-internet-address*)))
          (setq socket (tcp:open-socket :address sa
                                        :keepalive t
                                        :listen-p t))
          (let ((data_addr (tcp:make-socket-address)))
            (tcp:get-address-of-socket socket data_addr)
            (let ((a (tcp:socket-address-internet-address data_addr))
                  (b (tcp:socket-address-port data_addr)))
              (when (zerop a)
                (setq a tcp:*my-internet-address*))
              (format t "PORT ~d,~d,~d,~d,~d,~d (~D)"
                      (ldb (byte 8 24) a)
                      (ldb (byte 8 16) a)
                      (ldb (byte 8 8) a)
                      (ldb (byte 8 0) a)
                      (ldb (byte 8 8) b)
                      (ldb (byte 8 0) b)
                      b)))
          (tcp:SOCKET-ACCEPT socket
                             (tcp:make-socket-address
                               :port remote-port
                               :internet-address
                               (get-internet-address remote-address)))
          (setq normalp t)
          socket)
      (and socket (not normalp)
           (close-socket socket)))))
