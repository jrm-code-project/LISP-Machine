;;; -*- Mode:LISP; Package:NETWORK; Base:10; Readtable:CL -*-

#|

  Copyright LISP Machine, Inc. 1987
   See filename "Copyright.Text" for
  licensing and release information.

|#

(tv:define-peek-mode peek-network #\N "Network status"
                     NIL "Display status of various network system entities")

(defun peek-network (ignore)
  "Displays state of all network interfaces and network protocols"
  (LIST NIL
        (TV:SCROLL-PARSE-ITEM "Network Protocols")
        (TV:SCROLL-PARSE-ITEM "")
        (TV:SCROLL-MAINTAIN-LIST #'(LAMBDA () *network-protocol-streams*)
                                 'peek-network-protocol)
        (TV:SCROLL-PARSE-ITEM "Network Interfaces")
        (TV:SCROLL-PARSE-ITEM "")
        (TV:SCROLL-MAINTAIN-LIST #'(LAMBDA () *network-interfaces*)
                                 'peek-network-interface)
        ))

(defvar gauge-items '(("Average Packets Sent" :value :aps)
                      ("Average Packets Received" :value :apr)
                      ("Average Bytes Sent" :value :abs)
                      ("Average Bytes Received" :value :abr)
                      ("Instantaneous Packets Sent" :value :ips)
                      ("Instantaneous Packets Received" :value :ipr)
                      ("Instantaneous Bytes Sent" :value :ibs)
                      ("Instantaneous Bytes Received" :value :ibr))
  "Item List for tv:multiple-menu-choose to select types of gauges")

(defun select-gauge-items (gauge-list)
  (mapcar #'(lambda (gauge)
              (dolist (g gauge-items)
                (when (eq (third g) (car gauge))
                  (return g)))) gauge-list))

(defun gauge-menu (&rest args)
  (apply 'process-run-function "Peek Network Gauge Menu" 'gauge-menu-internal args))

(defun gauge-menu-internal (object gauge-list)
  (let ((items (select-gauge-items gauge-list)))
    (multiple-value-bind (selected-gauges chosen?)
        (tv:multiple-menu-choose gauge-items
                                 "Which gauges"
                                 '(:mouse)
                                 items)
      (if chosen?
          (funcall object :set-gauges selected-gauges)))))

(defun peek-network-interface (ni)
  "Format is:

Network interface <tag> is a <interface> interface to a <keyword> network on address <address>.
<enabled state>, protocols <protocol list>
Packets: <n> sent <n> rcvd <n> sdisc <n> rdisc <n> looped
Bytes: <n> sent <n> rcvd <n> sdisc <n> rdisc <n> looped
"
  (list `( :PRE-PROCESS-FUNCTION peek-network-insert-ni-special-fields
           :interface ,ni)
        (tv:scroll-parse-item
          `(:MOUSE-ITEM
             (NIL :MENU-CHOOSE
                  ("Network Interface Operations"
                   ("Close" :EVAL  (when (tv:mouse-y-or-n-p "Close this network interface")
                                     (funcall ni :close))
                    :DOCUMENTATION
                    "Click left to close this network interface.")
                   ("Reset" :EVAL  (when (tv:mouse-y-or-n-p "Reset this network interface")
                                     (funcall ni :reset))
                    :DOCUMENTATION
                    "Click left to reset this network interface.")
                   ("Inspect" :EVAL (send tv:selected-window :force-kbd-input `(inspect ,ni))
                    :DOCUMENTATION
                    "Click left to INSPECT this network interface.")
                  ("Describe" :EVAL (send tv:selected-window :force-kbd-input `(describe ,ni))
                    :DOCUMENTATION
                    "Click left to DESCRIBE this network interface.")
                  ("Addr-Stat" :EVAL (send tv:selected-window :force-kbd-input `(:eval (arp:addr-stat ,ni)))
                    :DOCUMENTATION
                    "Click left to list address translation table for this interface"))
                  :DOCUMENTATION
                  "Menu of things to do to this network interface."
                  :BINDINGS
                  ((ni ',ni)))
             :FUNCTION ni-tag (,ni) NIL ("Network Interface ~S:"))
          `(:function ni-interface (,ni) NIL (" ~A interface"))
          `(:function ni-keyword (,ni) NIL (" to ~A network"))
          `(:function ni-address (,ni) NIL (" on address ~X")))
        (tv:scroll-parse-item
          `(:function ni-sent-header-length (,ni) NIL ("Header (S/R): ~D"))
          `(:function ni-rcvd-header-length (,ni) NIL ("/~D"))
          `(:function ni-sent-trailer-length (,ni) NIL (" trailer (S/R): ~D"))
          `(:function ni-rcvd-trailer-length (,ni) NIL ("/~D"))
          `(:function ni-minimum-data-length (,ni) NIL (" data length: ~D"))
          `(:function ni-maximum-data-length (,ni) NIL ("-~D, "))
          `(:mouse-item
             (nil :buttons
                  ,(make-list 3 :initial-element
                              `(nil :eval (setf (ni-loopback ni) (not (ni-loopback ni)))
                                    :bindings ((ni ',ni))))
                  :DOCUMENTATION
                  "Click to toggle software loopback"
                  :BINDINGS
                  ((ni ',ni)))
             :function ni-loopback (,ni) NIL ("loopback: ~A"))
          `(:function ni-allocation-failures (,ni) NIL (", alloc failed ~D")))
        (tv:scroll-parse-item
          `(:mouse-item
             (nil :buttons
                  ,(make-list 3 :initial-element
                              `(nil :eval (funcall ni (if (ni-enabled ni) :disable :enable))
                                    :bindings ((ni ',ni))))
                  :DOCUMENTATION
                  "Click to toggle state of interface"
                  :BINDINGS
                  ((ni ',ni)))
             :function ni-enabled (,ni) NIL ("~:[Dis~;En~]abled"))
          `(:function ,#'(lambda (ni)
                          (loop for elt in (ni-network-alist ni)
                                collect (car elt)))
                      (,ni) NIL (" on protocols: ~A"))
          `(:function ni-protocols-not-understood (,ni) NIL (", not understood: ~A,  "))
          `(:mouse-item
             (nil :buttons
                  ((nil :eval (funcall ni (if (ni-active-gauges ni) :kill-gauges :make-gauges))
                        :bindings ((ni ',ni)))
                   (nil :eval (ignore))
                   (nil :eval (gauge-menu ni (ni-active-gauges ni))
                        :bindings ((ni ',ni))))
                  :documentation
                  "Click left for default, right for menu"
                  ((ni ',ni)))
             :function ,#'(lambda (n) (mapcar 'car (ni-active-gauges n))) (,ni) NIL ("Gauges: ~A")))
        (sent-statistics (ni-statistics-block ni) "")
        (rcvd-statistics (ni-statistics-block ni) "")
        (TV:SCROLL-PARSE-ITEM
          `(:function ni-packets-sent-discarded (,ni) NIL ("Packets sent/disc ~D"))
          `(:function ni-packets-received-discarded (,ni) NIL (" rcvd/disc ~D"))
          `(:function ni-bytes-sent-discarded (,ni) NIL (" Bytes sent/disc ~D"))
          `(:function ni-bytes-received-discarded (,ni) NIL (" rcvd/disc ~D")))
        (TV:SCROLL-PARSE-ITEM
          :leader 1                             ;to indicate whether special fields inserted
          "")
        ))

(DEFUN peek-network-insert-ni-special-fields (ITEM &aux new-lines ni)
  "A pre-process function to insert network interface specific fields in the display"
  (unless (ARRAY-LEADER (car (last (TV:SCROLL-ITEM-component-items ITEM))) TV:SCROLL-ITEM-LEADER-OFFSET)
    (setq ni (getf (tv:scroll-item-plist item) :interface))
    (when (setq new-lines (send ni :peek-special-fields ni))
      (setf (tv:scroll-item-component-items item)
            (nconc (butlast (tv:scroll-item-component-items item))
                   new-lines
                   (last (tv:scroll-item-component-items item)))))
    (SETF (ARRAY-LEADER (car (last (TV:SCROLL-ITEM-component-items ITEM))) TV:SCROLL-ITEM-LEADER-OFFSET) t)))


(defun peek-network-protocol (np)
  "Format is:

Network Protocol <keyword> is <enabled state>
Packets: <n> sent <n> rcvd <n> sdisc <n> rdisc <n> looped
Bytes: <n> sent <n> rcvd <n> sdisc <n> rdisc <n> looped
"
  (list `( :PRE-PROCESS-FUNCTION peek-network-insert-np-special-fields
          :stream ,np)
        (tv:scroll-parse-item
          `(:MOUSE-ITEM
             (NIL :MENU-CHOOSE
                  ("Network protocol Operations"
                   ("Close" :EVAL   (when (tv:mouse-y-or-n-p "Close this network protocol")
                                      (funcall np :close))
                    :DOCUMENTATION
                    "Click left to close this network protocol.")
                   ("Reset" :EVAL   (when (tv:mouse-y-or-n-p "Reset this network protocol")
                                      (funcall np :reset))
                    :DOCUMENTATION
                    "Click left to reset this network protocol.")
                   ("Inspect" :EVAL (send tv:selected-window :force-kbd-input `(inspect ,np))
                    :DOCUMENTATION
                    "Click left to INSPECT this network protocol.")
                   ("Describe" :EVAL (send tv:selected-window :force-kbd-input `(describe ,np))
                    :DOCUMENTATION
                    "Click left to DESCRIBE this network protocol."))
                  :DOCUMENTATION
                  "Menu of things to do to this network protocol."
                  :BINDINGS
                  ((np ',np)))
             :FUNCTION np-keyword (,np) NIL ("Network Protocol ~A"))
          " is "
          `(:mouse-item
             (nil :buttons
                  ,(make-list 3 :initial-element
                              `(nil :eval (funcall np (if (np-enabled np) :disable :enable))
                                    :bindings ((np ',np))))
                  :DOCUMENTATION
                  "Click to toggle state of network protocol"
                  :BINDINGS
                  ((np ',np)))
             :function np-enabled (,np) NIL ("~:[dis~;en~]abled"))
          `(:function network-protocol-address-list (,np) NIL (" on addresses ~A")))
        (sent-statistics (np-statistics-block np) "")
        (rcvd-statistics (np-statistics-block np) "")
        (TV:SCROLL-PARSE-ITEM
          `(:function np-packets-sent-discarded (,np) NIL ("Packets sent/disc ~D"))
          `(:function np-packets-received-discarded (,np) NIL (" rcvd/disc ~D"))
          `(:function np-bytes-sent-discarded (,np) NIL (" Bytes sent/disc ~D"))
          `(:function np-bytes-received-discarded (,np) NIL (" rcvd/disc ~D")))
        (tv:scroll-parse-item
          `(:function network-protocol-list (,np) NIL ("Protocols understood: ~A"))
          `(:function np-protocols-not-understood (,np) NIL (", not understood: ~A,  "))
          `(:mouse-item
             (nil :buttons
                  ((nil :eval (funcall np (if (np-active-gauges np) :kill-gauges :make-gauges))
                        :bindings ((np ',np)))
                   (nil :eval (ignore))
                   (nil :eval (gauge-menu np (np-active-gauges np))
                        :bindings ((np ',np))))
                  :documentation
                  "Click left for default, right for menu"
                  ((np ',np)))
             :function ,#'(lambda (n) (mapcar 'car (np-active-gauges n))) (,np) NIL ("Gauges: ~A")))
        (TV:SCROLL-PARSE-ITEM
          :leader 1
          ""))
  )

(defun network-protocol-list (np &aux result)
  (dolist (elt (np-protocols np))
    (push (tp-keyword (cdr elt)) result))
  (nreverse result))

(defun network-protocol-address-list (np)
  (loop for a in (np-addresses np)
        collect (if (np-address-printer np)
                    (funcall (np-address-printer np) a)
                  (format nil "~O" a))))

(DEFUN peek-network-insert-np-special-fields (ITEM &aux new-lines np)
  "A pre-process function to insert network protocol specific fields in the display"
  (unless (ARRAY-LEADER (car (last (TV:SCROLL-ITEM-component-items ITEM))) TV:SCROLL-ITEM-LEADER-OFFSET)
    (setq np (getf (tv:scroll-item-plist item) :stream))
    (when (setq new-lines (send np :peek-special-fields np))
      (setf (tv:scroll-item-component-items item)
            (nconc (butlast (tv:scroll-item-component-items item))
                   new-lines
                   (last (tv:scroll-item-component-items item)))))
    (SETF (ARRAY-LEADER (car (last (TV:SCROLL-ITEM-component-items ITEM))) TV:SCROLL-ITEM-LEADER-OFFSET) t)))

(defun peek-transport-protocol (protocol &aux (tp (cdr protocol)))
  (list `( :PRE-PROCESS-FUNCTION peek-network-insert-tp-special-fields
          :stream ,tp)
        (tv:scroll-parse-item
          :leader 2
          `(:MOUSE-ITEM
             (NIL :MENU-CHOOSE
                  ("Transport protocol Operations"
                   ("Close" :EVAL (when (tv:mouse-y-or-n-p "Close this transport protocol")
                                    (funcall tp :close))
                    :DOCUMENTATION
                    "Click left to close this transport protocol.")
                   ("Reset" :EVAL (when (tv:mouse-y-or-n-p "Reset this transport protocol")
                                    (funcall tp :reset))
                    :DOCUMENTATION
                    "Click left to reset this transport protocol.")
                   ("Inspect" :EVAL (send tv:selected-window :force-kbd-input `(inspect ,tp))
                    :DOCUMENTATION
                    "Click left to INSPECT this transport protocol.")
                   ("Describe" :EVAL (send tv:selected-window :force-kbd-input `(describe ,tp))
                    :DOCUMENTATION
                    "Click left to DESCRIBE this transport protocol.")
                   ("Normal" :eval (transport-protocol-level tv:item :normal)
                    :documentation
                    "Click left to choose normal display for this transport protocol.")
                   ("Verbose" :eval (transport-protocol-level tv:item :verbose)
                    :documentation
                    "Click left to choose verbose display for this transport protocol."))
                  :DOCUMENTATION
                  "Menu of things to do to this transport protocol."
                  :BINDINGS
                  ((tp ',tp)))
             :FUNCTION tp-keyword (,tp) NIL (" Transport protocol ~A"))
          `(:function tp-type (,tp) NIL (" (~D) is "))
          `(:mouse-item
             (nil :buttons
                  ,(make-list 3 :initial-element
                              `(nil :eval (funcall tp (if (tp-enabled tp) :disable :enable))
                                    :bindings ((tp ',tp))))
                  :DOCUMENTATION
                  "Click to toggle state of transport protocol"
                  :BINDINGS
                  ((tp ',tp)))
             :function tp-enabled (,tp) NIL ("~:[dis~;en~]abled"))
          `(:function peek-check-tp-next-identification (,tp) NIL ("~@[ with next packet identifier ~D~]"))
          ",  "
          `(:mouse-item
             (nil :buttons
                  ((nil :eval (funcall tp (if (tp-active-gauges tp) :kill-gauges :make-gauges))
                        :bindings ((tp ',tp)))
                   (nil :eval (ignore))
                   (nil :eval (gauge-menu tp (tp-active-gauges tp))
                        :bindings ((tp ',tp))))
                  :documentation
                  "Click left for default, right for menu"
                  ((tp ',tp)))
             :function ,#'(lambda (n) (mapcar 'car (tp-active-gauges n))) (,tp) NIL ("Gauges: ~A")))))

(defun transport-protocol-level (item level)
  (when (null (array-leader item (1+ tv:scroll-item-leader-offset)))
    (setf (array-leader item (1+ tv:scroll-item-leader-offset)) :normal))
  (unless (eq level (array-leader item (1+ tv:scroll-item-leader-offset)))
    (setf (array-leader item tv:scroll-item-leader-offset) nil)
    (setf (array-leader item (1+ tv:scroll-item-leader-offset)) level)))

(defun peek-check-tp-next-identification (tp)
  (case (named-structure-p tp)
    ((ip:ip-transport-protocol icmp:icmp-ip-transport-protocol udp:udp-ip-transport-protocol
                               tcp:tcp-ip-transport-protocol)
     (ip:tp-next-identification tp))
    (otherwise
     nil)))

(DEFUN peek-network-insert-tp-special-fields (ITEM &aux level tp)
  "A pre-process function to insert transport protocol specific fields in the display"
  (let ((first-item (first (tv:scroll-item-component-items item))))
    (unless (array-leader first-item tv:scroll-item-leader-offset)
      (setq level (array-leader first-item (1+ tv:scroll-item-leader-offset)))
      (setq tp (getf (tv:scroll-item-plist item) :stream))
      (setf (tv:scroll-item-component-items item)
            (nconc (list (first (tv:scroll-item-component-items item)))
                   (ncons (sent-statistics (tp-statistics-block tp) "  "))
                   (ncons (rcvd-statistics (tp-statistics-block tp) "  "))
                   (ncons (TV:SCROLL-PARSE-ITEM
                            `(:function tp-packets-sent-discarded (,tp) NIL ("  Packets sent/disc ~D"))
                            `(:function tp-packets-received-discarded (,tp) NIL (" rcvd/disc ~D"))
                            `(:function tp-bytes-sent-discarded (,tp) NIL (" Bytes sent/disc ~D"))
                            `(:function tp-bytes-received-discarded (,tp) NIL (" rcvd/disc ~D"))
                            `(:function tp-broadcast-packets-sent (,tp) NIL (" Broadcast sent ~D"))
                            `(:function tp-broadcast-packets-received (,tp) NIL (" rcvd ~D"))))
                   (send tp :peek-normal-fields tp)
                   (if (eq level :verbose) (send tp :peek-verbose-fields tp))
                   (send tp :peek-final-fields tp)))
      (setf (array-leader first-item tv:scroll-item-leader-offset) t))))

(defun sent-statistics (block offset)
  (TV:SCROLL-PARSE-ITEM
    offset
    `(:function ,#'(lambda () (aref block STAT-PS STAT-CURR)) nil NIL ("Sent Pkts tot ~6D"))
    `(:function ,#'(lambda () (aref block STAT-PS STAT-INST)) nil NIL (" inst ~6F"))
    `(:function ,#'(lambda () (aref block STAT-PS STAT-AVRG)) nil NIL (" avrg ~6F"))
    `(:function ,#'(lambda () (aref block STAT-PS STAT-MAX)) nil NIL (" max ~6F"))
    `(:function ,#'(lambda () (aref block STAT-BS STAT-CURR)) nil NIL (" Bytes tot ~6D"))
    `(:function ,#'(lambda () (aref block STAT-BS STAT-INST)) nil NIL (" inst ~6F"))
    `(:function ,#'(lambda () (aref block STAT-BS STAT-AVRG)) nil NIL (" avrg ~6F"))
    `(:function ,#'(lambda () (aref block STAT-BS STAT-MAX)) nil NIL (" max ~6F"))))

(defun rcvd-statistics (block offset)
  (TV:SCROLL-PARSE-ITEM
    offset
    `(:function ,#'(lambda () (aref block STAT-PR STAT-CURR)) nil NIL ("Rcvd Pkts tot ~6D"))
    `(:function ,#'(lambda () (aref block STAT-PR STAT-INST)) nil NIL (" inst ~6F"))
    `(:function ,#'(lambda () (aref block STAT-PR STAT-AVRG)) nil NIL (" avrg ~6F"))
    `(:function ,#'(lambda () (aref block STAT-PR STAT-MAX)) nil NIL (" max ~6F"))
    `(:function ,#'(lambda () (aref block STAT-BR STAT-CURR)) nil NIL (" Bytes tot ~6D"))
    `(:function ,#'(lambda () (aref block STAT-BR STAT-INST)) nil NIL (" inst ~6F"))
    `(:function ,#'(lambda () (aref block STAT-BR STAT-AVRG)) nil NIL (" avrg ~6F"))
    `(:function ,#'(lambda () (aref block STAT-BR STAT-MAX)) nil NIL (" max ~6F"))))
