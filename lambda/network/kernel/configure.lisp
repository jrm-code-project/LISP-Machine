;;; -*- Mode:LISP; Package:NETWORK; Base:10; Readtable:CL -*-

#|

  Copyright LISP Machine, Inc. 1987
   See filename "Copyright.Text" for
  licensing and release information.

|#

(export '( configure
           deconfigure
           enable
           disable
           setup-network-gauges
           ))

(defvar *processor-forwarding-alist* nil
  "Alist matching processors with flags indicating whether we are their gateway")

(defvar *protocol-configuration*
        `((:ethernet  (:internet ,ip-ethernet-type (:address :subnet) :address)
                      (:chaos ,chaos-ethernet-type :subnet :address)
                      (:arp ,arp-ethernet-type nil nil))
          (:loopback  (:internet ,ip-ethernet-type (#x7f000000 #xff000000) #x7f000000)
                      (:chaos ,chaos-ethernet-type nil :address))
          (:backplane (:internet ,ip-ethernet-type nil :address)
                      (:chaos 0 nil :address))
          (:serial    (:internet ,ip-ethernet-type nil :address))
          )
  "List of (hardware-type protocol...) for network interfaces")

(defun configure ()
  ;; If we are in the cold load, do a mini-configuration
  (when si:*in-cold-load-p*
    (cold-load-configure)
    (return-from configure t))

  ;; Clear out current network configuration
  (deconfigure)

  ;; Reset the int-pkts
  (initialize-network-buffers)

  ;; Put the network clock function onto the clock list
  (without-interrupts
    (pushnew 'net:network-clock sys:clock-function-list))

  ;; Start the device drivers
  (initializations 'network-driver-initialization-list t)

  ;; Start the protocol modules
  (when (eq si:*my-op* si:*ethernet-hardware-controller*)
    (arp:setup-arp))
  (chaos:setup-chaos)

  (let ((ip-address-p (ip:setup-ip)))
    (cond (ip-address-p
           (icmp:setup-icmp)
           (udp:setup-udp)
           (tcp:setup-tcp))
          (t
           (tv:careful-notify nil t "This processor has no Internet Address")))

    (initialize-share-interface)                ;Set up address translations and forwarding
    (initialize-loopback-interface)             ;Set up address translations

    (when ip-address-p
      ;;Start the TCP/UDP server process and enable network services
      (tcp-application:initialize-tcp-server-process)
      (tcp-application:enable-all-network-services)
      (tcp-application:initialize-udp-rwho-server-process)))

  ;;Start the network receiver process
  (start-receiver)

  (setup-network-gauges)                        ;If Fancy-Landscape enabled, populate screen with gauges

  t)

(defun initialize-share-interface ()
  (when si:*other-processors*
    ;;KLUDGE: share interface must be set up before chaos, as chaos enables itself on share device --
    ;;but share interface sets %processor-conf-chaos-address, so must reset it after chaos
    ;;has set its address
    (send si:share-interface :reset)
    ;; for all of our other processors, set up ARP translation for share interface
    (setq *processor-forwarding-alist* nil)
    (flet ((forward-p (op)
             (when (eq ethernet:*3com-owner* si:*my-op*)
               ;;We forward for other processer IFF we own 3com and they can't take an Excelan
               (let* ((device (fs:parse-pathname "EXCELAN-NETWORK-INTERFACE:"))
                      (host (and device (send device :host)))
                      (current-owner (and host (send host :owner))))
                 (cond ((eq current-owner :not-on-bus)  ;no Excelan
                        t)
                       ((> (si:op-proc-number op) 1)
                        ;;There is an Excelan and the other processor is the 3rd Lisp Machine on a 3X3, or is the
                        ;;Unix procssor of a 2X2+.  We forward in either case...
                        t)
                       ((typep (find-host-for-other-processor op) '(or null fs:unix-host))
                        ;;There is an Excelan, the other processor is the 2nd on the bus, and is either Unix or
                        ;;is unknown.  Assume we forward for it.
                        t)
                       (t
                        ;;There is an Excelan and the other processor is the 2nd Lisp Machine.
                        ;;Wait until it asks us to forward for it...
                        nil))))))
      (dolist (op si:*other-processors*)
        (push (cons op (forward-p op)) *processor-forwarding-alist*)
        (dolist (domain '(:chaos :internet))
          (let ((address (find-network-address-for-other-processor op domain)))
            (when address
              (add-address-info address domain si:share-interface op))))))
    ))

(defun initialize-loopback-interface ()
  (dolist (x (ni-address-alist *loopback-interface*))
    ;;For all protocols enabled on the loopback interface, set up address translations
    (when (second x)
      (add-address-info (second x) (first x) *loopback-interface* nil))))

(defun cold-load-configure ()
  ;; Reset the int-pkts
  (initialize-network-buffers)

  ;; Start the device drivers
  (si:set-processor-owning-ethernet :give-up)   ;Just find out who owns the various ethernet boards...
  (setup-loopback "LOOPBACK")
  (si:setup-share-interface "SHARE")

  ;; Start the protocol modules
  (chaos:setup-chaos)

  (initialize-share-interface)                  ;Set up address translations and forwarding
  (initialize-loopback-interface)               ;Set up address translations

  ;;Start the network receiver process
  (start-receiver)

  t)

(defun deconfigure ()
  (when *receiver*
    (send *receiver* :flush))
  (when tcp-application:*tcp-server-process*
    (send tcp-application:*tcp-server-process* :flush))
  (when tcp-application:*udp-rwho-server-process*
    (send tcp-application:*udp-rwho-server-process* :flush))
  (when tcp:*tcp-background-process*
    (send tcp:*tcp-background-process* :flush))
  (when ip:*ip-background-process*
    (send ip:*ip-background-process* :flush))
  (tcp-application:disable-all-network-services)
  (dolist (np *network-protocol-streams*)
    (send np :close))
  (dolist (ni *network-interfaces*)
    (send ni :close))
  (without-interrupts
    (setq sys:clock-function-list (delete 'net:network-clock sys:clock-function-list)))
  nil)

(defun disable ()
  (dolist (np *network-protocol-streams*)
    (send np :disable))
  (dolist (ni *network-interfaces*)
    (send ni :disable)))

(defun enable ()
  (dolist (np *network-protocol-streams*)
    (send np :enable))
  (dolist (ni *network-interfaces*)
    (send ni :enable)))

(defun setup-network-gauges ()
  (declare (special tv:fancy-landscape))
  (when (and (boundp 'tv:fancy-landscape) tv:fancy-landscape)
    (dolist (n *network-interfaces*)
      (when (equal (ni-tag n) "ONE")
        (send n :make-gauges)))))

(defun kill-network-gauges ()
  (declare (special tv:fancy-landscape))
  (when (and (boundp 'tv:fancy-landscape) tv:fancy-landscape)
    (dolist (n *network-interfaces*)
      (send n :kill-gauges))
    (dolist (np *network-protocol-streams*)
      (send np :kill-gauges))))

(defvar si:*ethernet-hardware-controller* :unbound
  "The processor description of the processor owning the Ethernet controller.")

(defvar ethernet:*3com-owner* nil "Owner of the 3com board")
(defvar ethernet:*excelan-owner* nil "Owner of the Excelan board")

(defun si:set-processor-owning-ethernet (&optional (operation :find) (board :all))
  "Change the processor controlling the ethernet boards.
If there is no ethernet board, make sure this machine knows not to use it.
If the argument is :FIND (the default) figure out who owns the board so we can
send packets to him.  If no one currently owns it, we allocate it.
If the argument is :TAKE or T, then steal it from the current owner.
If the argument is :GIVE-UP or NIL, then deallocate it so some one else can have it."
  (let ((results nil))
    (when (member board '(:3com :all))
      (push (si:set-3com-owner operation) results))
    (when (member board '(:excelan :all))
      (push (si:set-excelan-owner operation) results))
    ;;If I own either the 3com or excelan, note that I have an ethernet hardware device.
    ;;If I own neither, send through 3com, or excelan if no 3com.
    (let ((owner ethernet:*excelan-owner*))
      (cond ((eq owner si:*my-op*))             ;I own excelan?  good...
            ((null ethernet:*3com-owner*))      ;Nobody owns 3com?  Send packets through excelan owner.
            (t                                  ;Else, send packets through 3com owner.
             (setq owner ethernet:*3com-owner*)))
      (setq si:*ethernet-hardware-controller* owner))
    results))

(defvar si:dont-use-3com nil
  "leave 3com alone, even if free or already owned by me,
   so that unix / rtime can use it for raw-ethernet")

(defun si:set-3com-owner (operation)
  (let* ((index (* 2 si:%system-configuration-ethernet-owner))
         (lo (aref si:*sys-conf* index))
         (hi (aref si:*sys-conf* (1+ index)))
         (my-slot (si:get-slot-index si:rg-quad-slot))
         (result nil))
    (flet ((write-ethernet-slot (n)
                                (setf (aref si:*sys-conf* index) (ldb (byte 16. 0) n))
                                (setf (aref si:*sys-conf* (1+ index)) (ldb (byte 16. 16.) n))))
      (if (and (= lo #o177777) (= hi #o177777))
          (setq ethernet:*3com-owner* nil result "No 3com hardware.")
        (let ((current-owner (if (ldb-test (byte 1 15.) hi) (si:get-slot-index lo) nil)))
          (cond ((not (fboundp 'ethernet:setup-3com))
                 (setq ethernet:*3com-owner* nil)
                 (setq result "No 3com driver"))
                (si:dont-use-3com
                 (when (and current-owner (= current-owner my-slot))
                   (write-ethernet-slot 0))     ;if I own it, give it up
                 (setq ethernet:*3com-owner* nil)
                 (setq result "3com hardware reserved for raw ethernet"))
                ((and (or (null current-owner) (= current-owner my-slot))
                      (or (eq operation :GIVE-UP) (eq operation nil)))
                 (write-ethernet-slot 0)
                 (setq ethernet:*3com-owner* nil)
                 (setq result "No one owns the 3com."))
                ((or (null current-owner) (eq operation :TAKE) (eq operation t))
                 (write-ethernet-slot (dpb 1 (byte 1 31.) my-slot))
                 (setq ethernet:*3com-owner* si:*my-op*)
                 (when current-owner
                   (format t
       "~&WARNING: the 3com was owned by the processor in slot ~D but this processor has~
        ~%forcibly taken it.  If the other processor is dead, all is well, but if it is~
        ~%alive, both processors will get incorrect data until it does (net:configure)."
                           current-owner))
                 (setq result "I own the 3com."))
                (t                              ;:find, or don't own, but :give-up
                 (dolist (op (cons si:*my-op* si:*other-processors*)
                             (si:config-ferror nil "slot ~d (the ethernet owner) not found in sys-conf"
                                               current-owner))
                   (when (= current-owner (si:%processor-conf-slot-number (si:op-proc-conf op)))
                     (setq ethernet:*3com-owner* op)
                     (setq result (if (= current-owner my-slot)
                                      "I own the 3com."
                                    (format nil "Processor in slot ~d owns the 3com." current-owner)))
                     (return)))))))
      (unless (eq ethernet:*3com-owner* si:*my-op*)
        (when (fboundp 'ethernet:disable-lambda-ucode)
          (ethernet:disable-lambda-ucode)))
      result)))

(defvar si:dont-use-excelan nil
  "leave excelan alone, even if free or already owned by me")

(defun si:set-excelan-owner (operation)
  (let* ((device (fs:parse-pathname "EXCELAN-NETWORK-INTERFACE:"))
         (host (and device (send device :host)))
         (current-owner (and host (send host :owner)))
         (my-slot (si:get-slot-index si:rg-quad-slot))
         (result nil))
    (cond ((or (null host) (eq current-owner :not-on-bus))
           (setq ethernet:*excelan-owner* nil)
           (setq result "No Excelan hardware"))
          ((not (fboundp 'ethernet:setup-excelan))
           (setq ethernet:*excelan-owner* nil)
           (setq result "No excelan driver"))
          (si:dont-use-excelan
           (when (and current-owner (= current-owner my-slot))
             (send host :deallocate))
           (setq ethernet:*excelan-owner* nil)
           (setq result "Excelan hardware reserved for raw ethernet"))
          ((and (or (null current-owner) (= current-owner my-slot))
                (or (eq operation :GIVE-UP) (eq operation nil)))
           (send host :deallocate)
           (setq ethernet:*excelan-owner* nil)
           (setq result "No one owns the Excelan."))
          ((or (null current-owner) (eq operation :TAKE) (eq operation t))
           (send host :allocate)
           (setq ethernet:*excelan-owner* si:*my-op*)
           (setq result "I own the Excelan."))
          (t                                    ;:find, or don't own, but :give-up
           (dolist (op (cons si:*my-op* si:*other-processors*)
                       (si:config-ferror nil "slot ~d (the Excelan owner) not found in sys-conf"
                                         current-owner))
             (when (= current-owner (si:%processor-conf-slot-number (si:op-proc-conf op)))
               (setq ethernet:*excelan-owner* op)
               (setq result (if (= current-owner my-slot)
                                "I own the Excelan."
                              (format nil "Processor in slot ~d owns the Excelan." current-owner)))
               (return)))))
    result))

(defun setup-lambda-ethernet (tag)
  (when (eq si:processor-type-code si:lambda-type-code)
    (si:set-processor-owning-ethernet :find :3com)
    (cond ((eq ethernet:*3com-owner* si:*my-op*)
           (ethernet:setup-3com TAG)
           (si:set-processor-owning-ethernet :give-up :excelan))
          (t
           (si:set-processor-owning-ethernet :find :excelan)
           (if (eq ethernet:*excelan-owner* si:*my-op*)
               (ethernet:setup-excelan TAG))))))

(add-initialization "Start Lambda Ethernet interface" '(setup-lambda-ethernet "ONE") :network-driver)

(add-initialization "Configure Network System" '(configure) '(:normal :system))

(add-initialization "Deconfigure Network System"
                    '(progn (deconfigure)
                            (initialize-network-buffers)
                            (cleanup))
                    '(:before-cold))

(defun move-initialization (initialization-list first-name second-name)
  (do* ((first nil)
        (list initialization-list (cdr list))
        (next (second list) (second list)))
       ((null list))
    (cond ((string-equal first-name (car next))
           (setq first list))
          ((string-equal second-name (car next))
           (when first
             (setf (cdr list) (cddr list))      ;Splice out second initialization
             (setf (cdr first) (nconc (ncons next) (cdr first))))
           (return))
          (t))))

(add-initialization "Move network configuration initialization"
                    '(move-initialization si:system-initialization-list "Forget old file access" "Configure Network System")
                    '(:once))

(defun maybe-deconfigure-network ()
  (when (and (boundp '*loopback-interface*)
             *loopback-interface*
             (ni-enabled *loopback-interface*))
    (when (global:with-timeout ((* 30 60.) (progn (write-string "Yes (after timeout)" *query-io*) t))
            (yes-or-no-p "      Deconfigure network (Yes after thirty seconds) ? "))
      (net:deconfigure)))
  (cleanup))

(defun cleanup ()
  (unless (and (boundp '*loopback-interface*)
               *loopback-interface*
               (ni-enabled *loopback-interface*))
    ;;Only do this if network is deconfigured
    (chaos:reset nil)
    (setq chaos:made-pkts nil)
    (setq chaos:pkts-made 0)
    (setq chaos:free-pkts nil)
    (setq chaos:los-pkts nil)
    (setq chaos:current-los-pkt-count 0)
    (setq net:*network-interfaces* nil)
    (setq net:*network-protocol-streams* nil)
    ;;;Processor-specific interfaces:
    (select-processor
      (:lambda
        (setq ethernet:*3com-ethernet-interface* nil)
        (setq ethernet:*excelan-ethernet-interface* nil)
        (setq si:share-interface nil))
      (:explorer (setq ethernet:explorer-ethernet-interface nil))
      (:cadr (setq chaos:cadr-network-interface nil)))
    (setq net:*loopback-interface* nil)
    (setq arp:*arp-stream* nil)
    (setq chaos:*chaos-stream* nil)
    (setq ip:*ip-stream* nil)
    (setq ip:*free-ip-headers* nil)
    (setq ip:*route-table* nil)
    (setq ip:*default-network-numbers* nil)
    (setq ip:*default-gateway* nil)
    (setq ip:*default-interface* nil)
    (setq ip:*network-list* nil)
    (setq ip:*queued-packet-fifo* (make-fifo))
    (setq icmp:*icmp-stream* nil)
    (setq icmp:*free-icmp-messages* nil)
    (setq udp:*udp-stream* nil)
    (setq udp:*free-udp-buffers* nil)
    (setq udp:*free-udp-headers* nil)
    (setq tcp:*tcp-stream* nil)
    (setq tcp:*free-tcp-buffers* nil)
    (setq tcp:*free-tcp-headers* nil)
    (setq tcp:*send-blocked-sockets* (make-fifo))
    (setq tcp:*send-blocked-control-packets* (make-fifo))
    (setq tcp:*global-timeouts* nil)
    (setq tcp:*tcp-log* (make-fifo))
    (setq tcpa:*udp-rwho-server-packets* nil)
    (si:clear-resource 'tcp:simple-art-8b-buffer)
    ))

(add-initialization "Maybe Deconfigure Network" '(maybe-deconfigure-network) '(:gc-system-release))
