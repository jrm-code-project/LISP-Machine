;;; -*- Mode:LISP; Package:NETWORK; Readtable:CL; Base:10 -*-

#||

  Copyright LISP Machine, Inc. 1986, 1987
   See filename "Copyright.Text" for
  licensing and release information.

Generalizations to keep transparent user interface across different
network protocols. For now we have two protocol classes to contend
with, :CHAOS, and :INTERNET.

||#

(export '(define-network-function))

(defun network-path-available (type host)
  "Very simply, is it reasonable to use protocols of TYPE (e.g. :CHAOS, :INTERNET)
in connecting to this HOST?"
  (and (send host :network-typep type)
       (funcall (get type 'network-path-available 'default-network-path-available)
                host)))

(defun default-network-path-available (host)
  (declare (ignore host))
  t)


;;; Since chaos and system configuration are always loaded we dont feel too bad
;;; about including these functions here for now. In the future device driver
;;; dependant information must be factored out some way.
;;; THIS FUNCTIONS REQUIRE THAT ALL LMI MADE HOSTS HAVE CHAOSNET ADDRESSES.
;;; OTHER SYSTEM SOFTWARE WILL LOOK FUNNY IF THIS IS NOT THE CASE ANYWAY.

(defun local-hostp (to-host)
  (equal (or (send to-host :network-address :chaos)
             (return-from local-hostp nil))
         (send si:local-host :network-address :chaos)))

(defun other-bus-local-hostp (host)
  ;; as it is now all processors, ah, hosts, on the nubus have a chaosnet
  ;; address. so thats how we drive this function.
  (select-processor
    (:lambda
      (do ((addr (send host :network-address :chaos))
           (l si:*other-processors* (cdr l)))
          ((or (null l) (null addr)) nil)
        (if (equal (si:%processor-conf-chaos-address (si:op-proc-conf (car l))) addr)
            (return t))))
    (:cadr nil)
    (:explorer
      ;; we dont put these together like this yet
      nil)))

(defun (:property :chaos network-path-available) (to-host)
  (cond ((local-hostp to-host))
        ((other-bus-local-hostp to-host))
        ('else
         (select-processor
           (:lambda
             (cond ((not si:*ethernet-hardware-controller*)
                    ;; this variable means functions available to
                    ;; send raw ethernet packets. Which means 3COM,
                    ;; or link-level Excelan. If NIL it could also mean
                    ;; that the unix processor has taken the 3com
                    ;; for use in special protocols.
                    ())
                   ((get to-host :ethernet-chaos-protocols-ignored)
                    ;; if some conscientious site manager has put this
                    ;; property on a particular host, then obey it.
                    ())
                   ('else
                    ;; Still might not be true if there is no gateway.
                    t)))
           ((:cadr :explorer)
            t)))))

;;; NEXT. file access was handled specially, and works fine so we dont
;;; want to change that just now. but we need to have SI:DECODE-UNIT-ARGUMENT,
;;; HOSTAT, CONVERSE, etc do the right thing.
;;; Mail is also handled another way and mail is already messy because of the
;;; many different ways to sending it, two or three ways in chaosnet
;;; and at least to in TCP (FTP, rare but true, and SMTP).


(defvar *network-functions* nil)

(defmacro define-network-function (name argument-list &body body)
  (if (atom name)
      `(progn (defun ,name (host &rest args)
                (declare (arglist ,@argument-list))
                ,(car body)
                (invoke-correct-network-function
                  ',name ,(car argument-list)  args))
              (*define-network-function ',name))
    `(defun (:property ,(car name) ,(cadr name)) ,argument-list
       ,@body)))

(defun *define-network-function (name)
  (pushnew name *network-functions*)
  name)

(define-network-function get-remote-disk-unit (host unit usage &optional initp writep)
  "return an object that responds to the :DISK-READ, :DISK-WRITE, etc messages")

(define-network-function host-status (host &rest similar-hosts)
  "Do a host status on that host and similar hosts")

(define-network-function send-terminal-message (host user message-generator)
  "Send the message to the terminal of the user on the given host, MESSAGE-GENERATOR gets called on stream")

(define-network-function finger-host (host user output-stream style)
  "prints the finger information for user on host, or all users if NIL")

(define-network-function open-remote-tape-server (host)
  "opens a stream to a server running lmi's remote tape protocol")

(define-network-function get-host-time (host)
  "returns the universal time on HOST, or NIL if cannot")

(defvar *network-protocols* '(:chaos :internet))

(defun invoke-correct-network-function (name host args)
  (dolist (p *network-protocols*)
    (if (and (network-path-available p host)
             (get name p))
        (return-from invoke-correct-network-function
          (apply (get name p) host args))))
  (error "No network available to ~S implements the ~S utility function" host name))


;; Copied from LAD: RELEASE-3.NETWORK; TRANSPARENT.LISP#13 on 2-Oct-86 05:15:41
;; this variable is only looked at by time:initialize-timebase.
(setq time:*network-time-function* 'more-winning-network-time-function)

;; Copied from LAD: RELEASE-3.NETWORK; TRANSPARENT.LISP#13 on 2-Oct-86 05:15:41
(defun more-winning-network-time-function ()
  ;; this function is here to be a reminder of what
  ;; needs to be worked out to have this sort of thing
  ;; work out more naturally.
  ;; Not modular, just working, and in just one file...
  ;; First see if we can expect the chaos server hosts
  ;; to work.
  (dolist (h CHAOS:TIME-SERVER-HOSTS nil)
    (let ((p (si:parse-host h t nil)))
      (when (and p (network-path-available :chaos p))
        (return-from more-winning-network-time-function (chaos:host-time)))))
  ;; at this point assume some dumb generic thing
  (dolist (h (si:get-site-option :time-server-hosts))
    ;; return the first time we can get.
    (let ((p (si:parse-host h t nil)))
      (and p
           (not (eq p si:local-host))
           (let ((tm (get-host-time p)))
             (and tm
                  (return (values tm p))))))))
