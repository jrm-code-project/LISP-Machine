;;; -*- Mode:LISP; Package:ETHERNET; Base:10; Readtable:CL -*-

#|

  Copyright LISP Machine, Inc. 1987
   See filename "Copyright.Text" for
  licensing and release information.

|#

(export '( setup-excelan
           *excelan-ethernet-interface*
           *excelan-owner*
           exos-stats))

(defstream excelan-interface
           (network-interface)
           "EXC-"
  (exintr-called-by-user 0)                     ;statistic: interrupts handled by user
  (exintr-called-by-system 0)                   ;statistic: interrupts handled by network receiver
  (exintr-called-by-alloc 0)                    ;statistic: interrupts handeled by allocate-dmabuf
  (exintr-calls 0)                              ;statistic: user requests that wanted to handle own exintr
  (exintr-wins 0)                               ;statistic: user requests that succeeded in that
  (exintr-busy-loops 39)                        ;how many times to loop waiting for exintr
  )


(defop (excelan-interface :peek-special-fields) (ni)
  (list (tv:scroll-parse-item
          `(:function exc-exintr-called-by-user (,ni) NIL ("Exintr calls: user ~D"))
          `(:function exc-exintr-called-by-system (,ni) NIL (" system: ~D"))
          `(:function exc-exintr-called-by-alloc (,ni) NIL (" alloc: ~D"))
          `(:function exc-exintr-calls (,ni) NIL (" User requests: ~D"))
          `(:function exc-exintr-wins (,ni) NIL (" wins: ~D"))
          `(:function exc-exintr-busy-loops (,ni) NIL (" Loop Constant = ~D")))))

(defsubst i-own-excelan ()
  (eq *excelan-owner* si:*my-op*))

(defvar *excelan-ethernet-interface* nil "The Lambda Excelan Ethernet interface")
(defvar *excelan-initialized-p* nil "T if the Excelan is ready to send packets")
(defvar *exintr-in-progress* nil "NIL, :user, :system, or :alloc")

(defvar *excelan-link-recv-count* 4 "Number of pending receives to keep out")
(defvar *excelan-link-recv-out* 0 "Number of outstanding receives")

(defvar *exos-polling-p* nil "T if network receiver polls Excelan queues")

(defmacro ex-do-poll ()
  '(setq *exos-polling-p* t))

(defmacro ex-stop-poll ()
  '(setq *exos-polling-p* nil))

(defstruct (excelan-reply
             (:type :list)
             (:conc-name "RP-"))
  command
  reply
  buf
  data)

(defvar *excelan-reply-list* nil "list of (command reply buf data) received from Excelan")

;;; R/W the exos board's two CSR's

(defparameter porta #x100280)                   ;set dipswitches on 101 for port A0
(defparameter portb #x100284)

;;; bits in port b
(defconstant pb-error #x01)                     ; fatal error when 0
(defconstant pb-int  #x02)                      ; exos has interrupted when 1
(defconstant pb-ready #x08)                     ; exos is ready when 0

(defun inb (port-num) (logand #xff (si:%nubus-read-8 si:sdu-quad-slot port-num)))
(defun outb (port-num val) (si:%nubus-write-8 si:sdu-quad-slot port-num val))

;;; ROUTINES TO READ INTEGERS FROM BYTE ARRAYS

(defun write-uchar (char array baseoff offset)
  (unless (= (get offset 'size-in-bytes) 1)
    (error "~A is not a valid character data offset.~%" offset))
  (setf (aref array (+ baseoff (symbol-value offset))) char))

(defun read-uchar (array baseoff offset)
  (unless (= (get offset 'size-in-bytes) 1)
    (error "~A is not a valid character data offset.~%" offset))
  (aref array (+ baseoff (symbol-value offset))))

(defun write-short (shortnum array baseoff offset)
  (unless (= (get offset 'size-in-bytes) 2)
    (error "~A is not a valid shortnum data offset.~%" offset))
  (setf (aref array (+ baseoff (symbol-value offset))) (ldb (byte 8 8) shortnum))
  (setf (aref array (+ 1 baseoff (symbol-value offset))) (ldb (byte 8 0) shortnum)))

(defun read-short (array baseoff offset)
  (unless (= (get offset 'size-in-bytes) 2)
    (error "~A is not a valid shortnum data offset.~%" offset))
  (+ (aref array (+ 1 baseoff (symbol-value offset)))
     (ash (aref array (+ baseoff (symbol-value offset))) 8.)))

(defun write-long (longnum array baseoff offset)
  (unless (= (get offset 'size-in-bytes) 4)
    (error "~A is not a valid longnum data offset.~%" offset))
  (setf (aref array (+ baseoff (symbol-value offset))) (ldb (byte 8 24) longnum))
  (setf (aref array (+ 1 baseoff (symbol-value offset))) (ldb (byte 8 16) longnum))
  (setf (aref array (+ 2 baseoff (symbol-value offset))) (ldb (byte 8 8) longnum))
  (setf (aref array (+ 3 baseoff (symbol-value offset))) (ldb (byte 8 0) longnum)))

(defun read-long (array baseoff offset)
  (unless (= (get offset 'size-in-bytes) 4)
    (error "~A is not a valid longnum data offset.~%" offset))
  (+ (aref array (+ 3 baseoff (symbol-value offset)))
     (ash (aref array (+ 2 baseoff (symbol-value offset))) 8.)
     (ash (aref array (+ 1 baseoff (symbol-value offset))) 16.)
     (ash (aref array (+ baseoff (symbol-value offset))) 24.)))

(defun write-48bits (longnum array baseoff offset)      ; for ethernet addresses
  (unless (= (get offset 'size-in-bytes) 6)
    (error "~A is not a valid 48bit data offset.~%" offset))
  (setf (aref array (+ baseoff (symbol-value offset))) (ldb (byte 8 40.) longnum))
  (setf (aref array (+ 1 baseoff (symbol-value offset))) (ldb (byte 8 32.) longnum))
  (setf (aref array (+ 2 baseoff (symbol-value offset))) (ldb (byte 8 24.) longnum))
  (setf (aref array (+ 3 baseoff (symbol-value offset))) (ldb (byte 8 16.) longnum))
  (setf (aref array (+ 4 baseoff (symbol-value offset))) (ldb (byte 8 8) longnum))
  (setf (aref array (+ 5 baseoff (symbol-value offset))) (ldb (byte 8 0) longnum)))

(defun read-48bits (array baseoff offset)
  (unless (= (get offset 'size-in-bytes) 6)
    (error "~A is not a valid 48bit data offset.~%" offset))
  (+ (aref array (+ 5 baseoff (symbol-value offset)))
     (ash (aref array (+ 4 baseoff (symbol-value offset))) 8.)
     (ash (aref array (+ 3 baseoff (symbol-value offset))) 16.)
     (ash (aref array (+ 2 baseoff (symbol-value offset))) 24.)
     (ash (aref array (+ 1 baseoff (symbol-value offset))) 32.)
     (ash (aref array (+ baseoff (symbol-value offset))) 40.)))

;;; Set up DMA between Lambda and EXOS board

(defparameter bsize 1518.)                      ; buffer size

(defvar *dma-buffer* nil "a structure of type DMA-BUFFER")
(defvar *dma* nil "art-8b array, page-aligned, wired, and multibus mapped")
(defvar *dma-multibus-address* nil "address in multibus of first byte of *DMA*")
(defvar *dma16* "same as *DMA* but art-16b")

;;; Offsets into *DMA*: first 1024 bytes for message queues, rest for dma buffers.

(defparameter h2xq-offset 0)                         ; first buf is for the host-to-exos message queue
(defparameter wqueue-pointer h2xq-offset)
(setf (get 'wqueue-pointer 'size-in-bytes) 2)

(defparameter x2hq-offset 512.)                      ; second buf is for the exos-to-host message queue
(defparameter rqueue-pointer x2hq-offset)
(setf (get 'rqueue-pointer 'size-in-bytes) 2)

(defparameter dmabuf-area-offset 1024.)

;;; administration of free pool
(defvar *n-dmabufs*)
(defvar *dmabuf-record*)                        ; is it free or in use?
(defvar *dmabuf-commands*)                      ; who allocated it?
(defvar *dma-bufs*)                             ; the buffers
(defvar *dmabuf-alist*)

(defun find-dmabuf (command &aux n)
  "find free dmabuf, return NIL if none available"
  (without-interrupts
    (when (setq n (sys:%string-search-char 0 *dmabuf-record* 0 *n-dmabufs*))
      (setf (aref *dmabuf-record* n) #\*)
      (setf (aref *dmabuf-commands* n) command)
      (aref *dma-bufs* n))))

(defun free-dmabuf (b)
  (dotimes (j *n-dmabufs*)
    (when (eq b (aref *dma-bufs* j))
      (if (= 0 (aref *dmabuf-record* j))
          (cerror "Ignore" "DMABUF ~A already freed" b)
        (without-interrupts
          (setf (aref *dmabuf-commands* j) nil)
          (setf (aref *dmabuf-record* j) 0)))
      (return-from free-dmabuf nil)))
  (error "Not a result of FIND-DMABUF: ~S" b))

(defun allocate-dmabuf (&optional (command :unspecified))
  "Returns a DMA buffer, process waits if needed"
  (do ((buf (find-dmabuf command)))
      (buf buf)
    (if (and (exintr-message-readyp)
             (sys:%store-conditional (locf *exintr-in-progress*) nil :alloc))
        (progn (incf (exc-exintr-called-by-alloc *excelan-ethernet-interface*))
               (exintr)
               (setq buf (find-dmabuf command))
               (setf *exintr-in-progress* nil))
      (process-wait "Exos buf lock"
                    #'(lambda ()
                        (or buf
                            (setq buf (find-dmabuf command))
                            (and (null *exintr-in-progress*)
                                 (exintr-message-readyp))))))))

(defun reset-dmabuf-record nil
  (array-initialize *dmabuf-record* 0)
  (array-initialize *dmabuf-commands* nil)
  (and *dma* (array-initialize *dma* 0))
  t)

(defun setup-mapping-register (multibus-page nubus-page)
  (si:write-multibus-mapping-register
    multibus-page
    (logior (ash 1 23.) nubus-page)))

(defun get-dmabuf-16 (dmabuf-8)
  (get (assoc dmabuf-8 *dmabuf-alist* :test #'eq) :art-16b))

(defvar *dma-initialized-p* nil)

(defun initialize-dma ()
  (unless *dma-initialized-p*
    (setup-dma (si:%system-configuration-excelan-base-multibus-map-block si:*sys-conf*)
               (si:%system-configuration-excelan-multibus-map-size si:*sys-conf*))
    (setq *dma-initialized-p* t)))

(defun free-dma-resources ()
  (when *dma-buffer*
    (si:free-dma-buffer *dma-buffer*)
    ;; SET EVERYTHING TO NIL JUST FOR SAFETY.
    (dolist (sym '(*dma-buffer* *dma* *dma16* *dma-bufs* *dmabuf-alist*))
      (set sym nil)))
  (setq *dma-initialized-p* nil)
  (setq *excelan-initialized-p* nil))

(defun setup-dma (multibus-start-page number-of-pages)
  (setq *dma-buffer* (si:get-dma-buffer number-of-pages))
  (si:%wire-structure *dma-buffer*)
  (dotimes (j number-of-pages)
    (setup-mapping-register (+ multibus-start-page j)
                            (truncate (si:vadr-to-nubus-phys (+ (si:dma-buffer-data-vadr *dma-buffer*)
                                                                (* j si:page-size)))
                                      1024)))
  (setq *dma-multibus-address* (* multibus-start-page 1024))
  (setq *dma* (si:dma-buffer-8b *dma-buffer*))
  (setq *dma16* (si:dma-buffer-16b *dma-buffer*))
  (setq *n-dmabufs* (floor (- (length *dma*) dmabuf-area-offset) bsize))
  (setq *dmabuf-record* (make-array *n-dmabufs* :element-type 'string-char))
  (setq *dmabuf-commands* (make-array *n-dmabufs*))
  (setq *dma-bufs* (make-array *n-dmabufs*))
  (setq *dmabuf-alist* nil)
  (do ((j 0 (1+ j))
       (offset dmabuf-area-offset (+ offset bsize)))
      ((= j *n-dmabufs*))
    (push (list (make-array bsize
                            :element-type '(unsigned-byte 8)
                            :displaced-to *dma*
                            :displaced-index-offset offset)
                :index j
                :multibus-address (+ *dma-multibus-address* offset)
                :art-16b (make-array (ceiling bsize 2)
                                     :element-type '(unsigned-byte 16)
                                     :displaced-to *dma16*
                                     :displaced-index-offset (ceiling offset 2)))
          *dmabuf-alist*))
  (setq *dmabuf-alist* (nreverse *dmabuf-alist*))
  (dolist (x *dmabuf-alist*)
    (setf (aref *dma-bufs* (get x :index)) (car x))))

;;; Given an offset into *dma*, return the corresponding multibus-address.

(defun dmabuf-multibus-address (buf)
  (get (assoc buf *dmabuf-alist* :test #'eq) :multibus-address))

(defun offset-to-multibus-address (offset-in-dma)
  (+ *dma-multibus-address* offset-in-dma))

(defmacro with-dmabuf ((var &optional command) &body body &aux (temp (gentemp var)))
  "Execute the body with var bound to an ART-8B array mapped DMA to the multibus"
  `(let (,temp)
     (unwind-protect
         (let ((,var (setq ,temp (allocate-dmabuf ,command))))
           ,@body)
       (and ,temp (free-dmabuf ,temp)))))

(defun display-dma-bufs (&optional (nchars 60))
  (format t "~& N State First ~D characters~%" nchars)
  (dotimes (i *n-dmabufs*)
    (format t "~2D ~S ~:[ FREE~;TAKEN~] "
            i
            (aref *dma-bufs* i)
            (not (zerop (aref *dmabuf-record* i))))
    (do ((j 0 (1+ j))
         (b (aref *dma-bufs* i))
         (c))
        ((or (= j nchars) (= #\return (setq c (aref b j)))))
      (write-char c))
    (terpri)))

;;;
;;; Format of a standard "exos to host" or "host to exos" message:
;;;     - this is what is linked together into a queue which both the
;;;       host and exos manipulate while talking to each other
;;;     - a message contains:
;;;             - a header describing the state of the message and its size
;;;             - an actual network message
;;;             - (For the host:
;;;             - a link for the host to use to maintain and follow the
;;;               message queue with
;;;             - a pointer to a kludge buffer, which is used to hold
;;;               the reply from a message)
;;;

;;; Queue headers
(defconstant headers-link 0)                    ; exos link address
(setf (get 'headers-link 'size-in-bytes) 2)

(defconstant headers-reserved 2)                ; not used; must be 0
(setf (get 'headers-reserved 'size-in-bytes) 1)

(defconstant headers-status 3)                  ; status of queue element
(setf (get 'headers-status 'size-in-bytes) 1)

(defconstant headers-length 4)                  ; length of data in queue element
(setf (get 'headers-length 'size-in-bytes) 2)

(defconstant sizeof-headers 6)                  ; size of structure in bytes

;;; Generic queue messages
(defconstant messages-link 0)                   ; exos link address
(setf (get 'messages-link 'size-in-bytes) 2)

(defconstant messages-reserved 2)               ; not used; must be 0
(setf (get 'messages-reserved 'size-in-bytes) 1)

(defconstant messages-status 3)                 ; status of queue element
(setf (get 'messages-status 'size-in-bytes) 1)

(defconstant messages-length 4)                 ; length of data in queue element
(setf (get 'messages-length 'size-in-bytes) 2)

(defconstant messages-soid 6)                   ; reserved for exos, or, socket id
(setf (get 'messages-soid 'size-in-bytes) 2)

(defconstant messages-userid 8)                 ; sequence # to attach to msg
(setf (get 'messages-userid 'size-in-bytes) 4)

(defconstant messages-request 12)               ; command
(setf (get 'messages-request 'size-in-bytes) 1)

(defconstant messages-reply 13)                 ; result
(setf (get 'messages-reply 'size-in-bytes) 1)

(defconstant sizeof-messages 14)                ; size of structure in bytes

;;; mq_status values
(defconstant mq-exos #x01)                      ; exos owns queue element
(defconstant mq-done #x02)                      ; exos is done with queue element
(defconstant mq-overflow #x04)                  ; data area too big

;;; link level message definitions
;;; fields in bytes 0 to 13 are the same as any of the above
;;; see excelan hardware manual for details on what these messages do

(defconstant *link-ethernet-slot* 253.)         ; raw ethernet address slot -- see hardware manual
(defconstant *link-universal-slot* 254.)        ; promiscuous receive address slot -- see hardware manual
(defconstant *link-broadcast-slot* 255.)        ; broadcast address slot -- see hardware manual

;;; read or set receive mode
(defconstant link-nrecv-reqmask 14)
(setf (get 'link-nrecv-reqmask 'size-in-bytes) 1)

(defconstant link-nrecv-slot 15)
(setf (get 'link-nrecv-slot 'size-in-bytes) 1)

(defconstant sizeof-link-nrecv 16)

;;; read ethernet address (can also write address for multicast slots)
(defconstant link-naddress-reqmask 14)
(setf (get 'link-naddress-reqmask 'size-in-bytes) 1)

(defconstant reqmask-write 1)
(defconstant reqmask-read 2)
(defconstant reqmask-rw 3)
(defconstant reqmask-enable 4)

(defconstant link-naddress-slot 15)
(setf (get 'link-naddress-slot 'size-in-bytes) 1)

(defconstant link-naddress-addr 16)
(setf (get 'link-naddress-addr 'size-in-bytes) 6)

(defconstant sizeof-link-naddress 22)

;;; read or set controller net mode
(defconstant link-nmode-reqmask 14)
(setf (get 'link-nmode-reqmask 'size-in-bytes) 1)

(defconstant link-nmode-optmask 15)
(setf (get 'link-nmode-optmask 'size-in-bytes) 1)

(defconstant optmask-ignore-alignment-errors #x10)
(defconstant optmask-ignore-crc-errors #x20)
(defconstant optmask-net-disable #x80)

(defconstant link-nmode-mode 16)
(setf (get 'link-nmode-mode 'size-in-bytes) 1)

(defconstant nmode-disconnect 0)
(defconstant nmode-connect-hardware-filter 1)
(defconstant nmode-connect-perfect-filter 2)
(defconstant nmode-connect-promiscuous 3)
(eval-when (load compile eval)
  (defconstant all-link-net-modes '(nmode-disconnect nmode-connect-hardware-filter
                                                     nmode-connect-perfect-filter
                                                     nmode-connect-promiscuous)))

(defconstant sizeof-link-nmode 17)

;;; Transmit or receive ethernet packet
(defconstant link-tr-slot 14)
(setf (get 'link-tr-slot 'size-in-bytes) 1)

(defconstant link-tr-nblocks 15)
(setf (get 'link-tr-nblocks 'size-in-bytes) 1)

(defconstant link-tr-length 16)
(setf (get 'link-tr-length 'size-in-bytes) 2)

(defconstant link-tr-address 18)
(setf (get 'link-tr-address 'size-in-bytes) 4)

(defconstant sizeof-link-tr 22)

;;; link-level board statistics
(defconstant link-nststcs-reqmask 14)
(setf (get 'link-nststcs-reqmask 'size-in-bytes) 1)

(defconstant link-nststcs-reserved 15)
(setf (get 'link-nststcs-reserved 'size-in-bytes) 1)

(defconstant link-nststcs-nobjects 16)
(setf (get 'link-nststcs-nobjects 'size-in-bytes) 2)

(defconstant link-nststcs-index 18)
(setf (get 'link-nststcs-index 'size-in-bytes) 2)

(defconstant link-nststcs-address 20)
(setf (get 'link-nststcs-address 'size-in-bytes) 4)

(defconstant sizeof-link-nststcs 24)

;;; The following constant determines the max size of a message in the queue
(defconstant sizeof-exos-msg
          (max sizeof-headers sizeof-messages sizeof-link-tr sizeof-link-nrecv
               sizeof-link-naddress sizeof-link-nmode sizeof-link-nststcs))

;;; EXOS-HOST MESSAGE QUEUES

;;; To run this board, a static data area is allocated at boot time which
;;; will contain the linked list of queues used by the exos.

;;; The [rw]msg_area structure is used to contain the working queues which
;;; both the host and the exos use.  It is declared in this fashion so as to
;;; allow it to be allocated at run time in the multibus address space.

;;; NET_RBUFS and NET_WBUFS are defined so that each fits in a 512-byte buffer

;;; the array element type for rmsg-area-msgs and wmsg-area-msgs
(defstruct (msg (:print-function (lambda (struct stream ignore)
                                   (sys:printing-random-object
                                     (struct stream :type :no-pointer)
                                     (format stream "offset ~D" (msg-offset struct))))))
  (link nil)                                         ; host link to next msg
  (kludge nil)                                       ; pointer to kludge buffer
  (offset 0))                                        ; offset of message in dma array

;;; How the host sees the rmsg-area
(defconstant rmsg-area-rlink 0)                      ; exos link to ma_rmsgs
(setf (get 'rmsg-area-rlink 'size-in-bytes) 2)

(defconstant net-rbufs (truncate 510. sizeof-exos-msg))

(defvar rmsgarea nil)                                ; read msg queues, as seen by host
(defvar rqueue-offset-alist nil
  "alist of EXOS message offsets and Lispm MSG structures for the Host-to-Exos message queue.")

(defstruct (rmsg-area (:print-function (lambda (struct stream ignore)
                                         (sys:printing-random-object
                                           (struct stream :type :no-pointer)
                                           (format stream "offset ~D" (rmsg-area-offset struct))))))
  (offset 0)                                         ; start offset into DMA array
  (msgs (make-array net-rbufs :element-type 'msg))   ; message descriptors
  (lastr nil))                                       ; last examined read-msg


;;; How the host sees the wmsg-area

(defconstant wmsg-area-wlink 0)                      ; exos link to ma-rmsgs
(setf (get 'wmsg-area-wlink 'size-in-bytes) 2)

(defconstant net-wbufs (truncate 510. sizeof-exos-msg))

(defvar wmsgarea nil)                                ; write msg queues, as seen by host
(defvar wqueue-offset-alist nil
  "alist of EXOS message offsets and Lispm MSG structures for the Host-to-Exos message queue.")

(defstruct (wmsg-area (:print-function (lambda (struct stream ignore)
                                         (sys:printing-random-object
                                           (struct stream :type :no-pointer)
                                           (format stream "offset ~D" (wmsg-area-offset struct))))))
  (offset 0)                                         ; start offset into DMA array
  (msgs (make-array net-wbufs :element-type 'msg))   ; message descriptors
  (lastw nil))                                       ; last examined write-msg

;;; Build up the RMSGAREA and WMSGAREA structures
(defun initialize-msgareas nil
  (setq rmsgarea (make-rmsg-area))
  (setq wmsgarea (make-wmsg-area))
  (dotimes (i net-rbufs)
    (setf (aref (rmsg-area-msgs rmsgarea) i) (make-msg)))
  (dotimes (i net-wbufs)
    (setf (aref (wmsg-area-msgs wmsgarea) i) (make-msg))))

;;; debugging functions
(defun display-rmsgarea nil
  (format t " Exos-to-host message queue:~%")
  (dotimes (i net-rbufs)
    (display-msg (aref (rmsg-area-msgs rmsgarea) i) (rmsg-area-lastr rmsgarea)))
  (format t "~%"))

(defun display-wmsgarea nil
  (format t " Host-to-exos message queue:~%")
  (dotimes (i net-wbufs)
    (display-msg (aref (wmsg-area-msgs wmsgarea) i) (wmsg-area-lastw wmsgarea)))
  (format t "~%"))

(defun display-msg (msg current-msg)
  (format t "~&")
  (if (eq msg current-msg) (format t "* ") (format t "  "))
  (display-msg-in-dma (msg-offset msg)))

(defun display-msg-in-dma (offset &aux request)
  (let ((st (read-uchar *dma* offset 'messages-status)))
    (format t "<~4D> OWNER=~A,DONE=~A,OVFL=~A,LENGTH=~D,SOID=~D,USERID=~D,RQST=~D,REPLY=~D~%"
            offset
            (if (zerop (logand st mq-exos)) 'host 'exos)
            (if (zerop (logand st mq-done)) 'no 'yes)
            (if (zerop (logand st mq-overflow)) 'no 'yes)
            (read-short *dma* offset 'messages-length)
            (read-short *dma* offset 'messages-soid)
            (read-long *dma* offset 'messages-userid)
            (setq request (read-uchar *dma* offset 'messages-request))
            (read-uchar *dma* offset 'messages-reply))))

(defun print-qs ()
  (format t "~%")
  (display-wmsgarea)
  (display-rmsgarea))

;;; This structure defines the reply data from a message sent to the
;;; board.  This is needed because of a problem with the message queues
;;; on the board - they are not searched.  Once a message is sent to the
;;; board, it EXPECTS the next sent message to be immediately following
;;; (in the circular queue sense) the previous sent message.  Since we
;;; need to save the write message until the sender can use it to find
;;; the reply message, a deadlock situation arises.  To solve this problem
;;; the following kludge is used.  Whenever we allocate a message to send
;;; to the board, we also allocate a kludge buffer.  The ex_send() routine
;;; will sleep on the address of the kludge buffer (which will be unique
;;; across any current sleepers).  Once the reply comes in, the interrupt
;;; routine will put the result of the sent message into the kludge
;;; buffer.

(defparameter net-kludges 50)

(defstruct (kludge (:print-function (lambda (struct stream ignore)
                                      (sys:printing-random-object
                                        (struct stream :type :no-pointer)
                                        (format stream "offset ~D" (kludge-id struct))))))
  (id 0)                                             ; id num for this kludge
  (state 0)                                          ; state of buffer (0 if free)
  (reply 0)                                          ; reply from exos
  (buf 0)                                            ; dmabuf associated with this
  (data nil))                                        ; data (a 32-bit number)

; kl_state's
(defconstant kl-free #x00)                      ; pseudo flag
(defconstant kl-busy #x01)                      ; buffer in use
(defconstant kl-waiting #x02)                   ; somebody wants an interrupt
(defconstant kl-non-blocking #x04)              ; non-blocking request

(defmacro set-kludge-state-bits (kp bits)
  `(without-interrupts
     (setf (kludge-state ,kp) (logior (kludge-state ,kp) ,bits))))

(defmacro clear-kludge-state-bits (kp bits)
  `(without-interrupts
     (setf (kludge-state ,kp) (logand (kludge-state ,kp) (lognot ,bits)))))

(defvar next-kludge 0)
(defvar kludge (make-array net-kludges))

;;; (KLUDGE-ID KP) = N+100., where N is what element it is in the KLUDGE array.
(defun initialize-kludge nil
  (dotimes (i net-kludges)
    (let ((k (make-kludge)))
      (setf (kludge-id k) (+ i 100.))
      (setf (aref kludge i) k))))

(defun find-kludge (id)
  (aref kludge (decode-kludge-id id)))

(defun decode-kludge-id (id)
  (when (< id 100.)
    (error "bad message id ~D. -- it must be > 100.~%" id))
  (- id 100.))

;;; reset all the kludge buffers
(defun reset-kludge ()
  (setq next-kludge 0)
  (dotimes (i net-kludges)
    (reset-kludge-buffer (aref kludge i))))

(defun reset-kludge-buffer (kp)
  (setf (kludge-state kp) kl-free)
  (setf (kludge-reply kp) nil)
  (setf (kludge-buf kp) nil)
  (setf (kludge-data kp) nil))

(defun describe-kludge (&aux kp)
  (dotimes (j net-kludges)
    (setq kp (aref kludge j))
    (and kp (describe kp))))

;;; EXINTR - Excelan interrupt handler

(defvar *exintr-request-table* (make-array 15))
(defvar *exintr-request-table-length* 15)

(defun exintr (&aux kp request-code reply-code handler)
  (loop
    (let ((read-mp (rmsg-area-lastr rmsgarea)))
      (unless (zerop (logand (read-uchar *dma* (msg-offset read-mp) 'messages-status) mq-exos))
        (return nil))
      (without-interrupts
        (setf (rmsg-area-lastr rmsgarea) (msg-link read-mp)))
      (setq request-code (read-uchar *dma* (msg-offset read-mp) 'messages-request))
      (setq reply-code (read-uchar *dma* (msg-offset read-mp) 'messages-reply))
      (setq kp (find-kludge (read-long *dma* (msg-offset read-mp) 'messages-userid)))
      (if (or (< request-code 0)
              (not (< request-code *exintr-request-table-length*))
              (not (setq handler (aref *exintr-request-table* request-code))))
          (error "unknown message type #x~X.~%" request-code)
        (funcall handler read-mp request-code kp reply-code))
      (without-interrupts
        (write-uchar (logior mq-exos mq-done) *dma* (msg-offset read-mp) 'messages-status))
      (when (zerop (logand (inb portb) 4))
        (outb portb 1)))))

;;; link level commands
(defconstant link-net-mode 8)                   ; set mode
(defconstant link-net-addrs 9)                  ; get slot address
(defconstant link-net-recv 10)                  ; set receive
(defconstant link-net-ststcs 11)                ; get statistics
(defconstant link-xmit 12)                      ; transmit a packet
(defconstant link-recv 13)                      ; receive a packet
(defconstant link-xmit-self-recv 14)            ; transmit with self-receive

(defmacro defexintr (name other-codes &body body)
  (let ((bname (intern (format nil "EXINTR_~A" name))))
    `(progn (defun ,bname (read-mp request-code kp reply-code)
              read-mp request-code kp reply-code
              ,@body)
            (setup-exintr ',bname ',(cons name other-codes)))))


(defun setup-exintr (fname codes)
  (dolist (code codes)
    (when (or (not (atom code)) (global:record-source-file-name (car codes) 'defexintr))
      (let ((n (if (atom code) (symbol-value code) (eval code))))
        (when (or (not *exintr-request-table*)
                  (not (< n *exintr-request-table-length*)))
          (let ((new (make-array (setq *exintr-request-table-length* (1+ n)))))
            (replace new *exintr-request-table*)
            (setq *exintr-request-table* new)))
        (setf (aref *exintr-request-table* n) (symbol-function fname))))))

(defexintr link-net-mode nil
  (setf (kludge-data kp)
        (cons (read-uchar *dma* (msg-offset read-mp) 'link-nmode-optmask)
              (read-uchar *dma* (msg-offset read-mp) 'link-nmode-mode)))
  (when (kludge-buf kp)
    (error "NET-MODE with a buffer!"))
  (finish-kludge kp reply-code))

(defexintr link-net-addrs nil
  (setf (kludge-data kp) (read-48bits *dma* (msg-offset read-mp) 'link-naddress-addr))
  (when (kludge-buf kp)
    (error "NET-ADDRS with a buffer!"))
  (finish-kludge kp reply-code))

(defexintr link-net-recv nil
  (setf (kludge-data kp) (read-uchar *dma* (msg-offset read-mp) 'link-nrecv-reqmask))
  (when (kludge-buf kp)
    (error "NET-RECV with a buffer!"))
  (finish-kludge kp reply-code))

(defexintr link-net-ststcs nil
  (setf (kludge-data kp)
        (read-short *dma* (msg-offset read-mp) 'link-nststcs-nobjects))
  (when (kludge-buf kp)
    (error "NET-STSTCS with a buffer!"))
  (finish-kludge kp reply-code))

(defexintr link-recv nil
  (setf (kludge-data kp) (read-short *dma* (msg-offset read-mp) 'link-tr-length))
  (decf *excelan-link-recv-out*)
  (unless (zerop (logand (kludge-state kp) kl-non-blocking))
    (if *exos-polling-p*                                ;if polling process on, good
        (let ((rp (make-excelan-reply)))
          (setf (rp-command rp) link-recv)
          (setf (rp-reply rp) reply-code)
          (setf (rp-buf rp) (kludge-buf kp))
          (setf (rp-data rp) (kludge-data kp))
          (push-fifo rp *excelan-reply-list*))
      (free-dmabuf (kludge-buf kp))))           ;otherwise, free the buffer
  (finish-kludge kp reply-code))

(defexintr link-xmit (link-xmit-self-recv)
  (cond ((plusp (logand (kludge-state kp) kl-non-blocking))
         (if (kludge-buf kp)
             (free-dmabuf (kludge-buf kp))
           (error "Non-blocking XMIT reply but no buffer to return!~&")))
        ((kludge-buf kp)
         (error "Blocking XMIT with buffer!~&")))
  (finish-kludge kp reply-code))

(defun finish-kludge (kp reply)
  (setf (kludge-reply kp) reply)
  (clear-kludge-state-bits kp (logior kl-waiting kl-non-blocking)))

;;; STATUS FUNCTIONS

;;; On the board, the EXBDSTATS structure is a byte-array of 8 longnums

(defconstant sizeof-exbdstats 32.)

(defconstant exbdstats-xmt 0)
(setf (get 'exbdstats-xmt 'size-in-bytes) 4)

(defconstant exbdstats-excess-coll 4)
(setf (get 'exbdstats-excess-coll 'size-in-bytes) 4)

(defconstant exbdstats-late-coll 8)
(setf (get 'exbdstats-late-coll 'size-in-bytes) 4)

(defconstant exbdstats-tdr 12)
(setf (get 'exbdstats-tdr 'size-in-bytes) 4)

(defconstant exbdstats-rcv 16)
(setf (get 'exbdstats-rcv 'size-in-bytes) 4)

(defconstant exbdstats-align-err 20)
(setf (get 'exbdstats-align-err 'size-in-bytes) 4)

(defconstant exbdstats-crc-err 24)
(setf (get 'exbdstats-crc-err 'size-in-bytes) 4)

(defconstant exbdstats-lost-err 28)
(setf (get 'exbdstats-lost-err 'size-in-bytes) 4)

(defvar stats-array (make-array sizeof-exbdstats :element-type '(unsigned-byte 8)))
(defvar stats-array-lock nil)

(defun exos-stats (&key (format-stream t) (reset-p nil) (print-p t) force-p)
  "Print exos board statistics."
  (cond ((null *excelan-owner*)
         (format format-stream "~&This backplane doesn't have an Excelan board."))
        ((not (i-own-excelan))
         (format format-stream "~&This processor doesn't own the Excelan board."))
        ((null *excelan-ethernet-interface*)
         (format format-stream "~&The Excelan board is not configured."))
        ((and (not force-p) (not (exc-enabled *excelan-ethernet-interface*)))
         (format format-stream "~&The Excelan board is not enabled."))
        (t
         (with-lock (stats-array-lock)
           (array-initialize stats-array 0)
           (link-net-ststcs stats-array reset-p)

           (when print-p
             (format format-stream "~
~%*******************************************************~
~% frames transmitted:                     ~12d.~
~%-------------------------------------------------------~
~% frames aborted due to excess collisions:~12d.~
~%-------------------------------------------------------~
~% time domain reflectometer:              ~12d.~
~%-------------------------------------------------------~
~% frames received:                        ~12d.~
~%-------------------------------------------------------~
~% frames received with alignment errors:  ~12d.~
~%-------------------------------------------------------~
~% frames received with crc errors:        ~12d.~
~%-------------------------------------------------------~
~% frames lost (no receive buffers):       ~12d.~
~%*******************************************************~
~%"
                     (read-long stats-array 0 'exbdstats-xmt)
                     (read-long stats-array 0 'exbdstats-excess-coll)
                     (read-long stats-array 0 'exbdstats-tdr)
                     (read-long stats-array 0 'exbdstats-rcv)
                     (read-long stats-array 0 'exbdstats-align-err)
                     (read-long stats-array 0 'exbdstats-crc-err)
                     (read-long stats-array 0 'exbdstats-lost-err)))))))

;;; EXOS link-level controller, for sending/receiving raw ethernet packets

;;; EX-FINDMSG
;      - first find a kludge buffer to hold result of message
;      - find next available message on "host to exos" message queue
;      - sleep if necessary, to get one

(defun ex-findmsg (&aux kp j)
  (dotimes (i (1+ net-kludges))                 ; get a kludge buffer
    (setq j (mod (+ i next-kludge) net-kludges))
    (when (eq (kludge-state (aref kludge j)) kl-free)
      (setq kp (aref kludge j))
      (reset-kludge-buffer kp)                  ; initialize its fields to NIL or 0
      (setf (kludge-state kp) kl-busy)          ; make it busy
      (setq next-kludge (1+ j))
      (return)))

  (unless kp (error "EXOS: couldn't get kl-buffer!~%"))

  (do (mp)
      (nil)
    (setq mp (cdr (assoc (read-short *dma* 0 'wqueue-pointer) wqueue-offset-alist)))
    (unless mp (error "Exos board queue pointer is bad ~D.~%"
                      (read-short *dma* 0 'wqueue-pointer)))

    (unless (zerop (read-uchar *dma* (msg-offset mp) 'headers-status))
      (process-wait '"Exos msg wait"
                    #'(lambda () (zerop (read-uchar *dma* (msg-offset mp) 'headers-status)))))

    (when (zerop (read-uchar *dma* (msg-offset mp) 'headers-status))
      (setf (msg-kludge mp) kp)
      (write-uchar mq-done *dma* (msg-offset mp) 'headers-status)
      (write-uchar 0 *dma* (msg-offset mp) 'headers-reserved)
      (array-initialize *dma* 0                 ; clear out the message
                        (+ (msg-offset mp) messages-soid)
                        (+ (msg-offset mp) sizeof-exos-msg))
      (return-from ex-findmsg mp))))

; ex_send:
;    - send a network message via the message queues to the exos
;    - set up standard header
;    - bump board to tell it to go

;;; returns a kludge packet, or NIL if aborted.
(defun ex-send (command mp &aux kp)
  (setq kp (msg-kludge mp))
  (write-long (kludge-id kp) *dma* (msg-offset mp) 'messages-userid)
  (write-uchar command *dma* (msg-offset mp) 'messages-request)
  (set-kludge-state-bits kp kl-waiting)
  (write-uchar (logior mq-exos (read-uchar *dma* (msg-offset mp) 'headers-status))
               *dma* (msg-offset mp) 'headers-status)
  (outb portb 0)                                     ; bump the board
  kp)

(defun kp-readyp (kp)
  (zerop (logand (kludge-state kp) kl-waiting)))

(defun exintr-message-readyp ()
  (zerop (logand (read-uchar *dma* (msg-offset (rmsg-area-lastr rmsgarea)) 'messages-status) mq-exos)))

;; EX-SEND-WAIT is called to wait for a reply to a blocking Excelan request.
;;
;; When the network receiver is handling the excelan, all sends and receives through the usual
;; network-interface mechanisms are non-blocking and the receiver will do the excelan polling.
;;
;; When the receiver is not polling the Excelan board, all sends and receives ARE blocking and
;; this routine must do the polling.
;;
;; George Carrette did some nice optimization for this case: The routine first checks to see if
;; the board has already responded to the user's blocking request.  For example, if the user has
;; asked to receive data already on the board, or requested transmission into the board's large
;; buffer when its window is not used up.  In that case we don't want to bother going through the
;; overhead of OUR-PROC=>SCHEDULER=>(other process)=>SCHEDULER=>OUR-PROC, so we service the "interrupt"
;; right here and now.
;;
;; Some timing tests for blocking and nonblocking transmissions generated the following figures:
;;
;; BUFFER SIZE   FUNCTION             BLOCKING?    BUSY-LOOPS        1000 packets in   Packets/sec   Bits/sec
;; -----------   --------             ---------    ----------        ---------------   -----------   --------
;; 60 bytes      link-xmit             Yes          14 (100% hits)    8.2 seconds       122.0          58560
;; 60 bytes      link-xmit             Yes          39 (100% hits)    8.2 seconds       122.0          58560
;; 1500 bytes    link-xmit             Yes          14 (3% hits)      27.3 seconds       36.6         439200
;; 1500 bytes    link-xmit             Yes          39 (100% hits)    13.1 seconds       76.3         915600
;;
;; 60 bytes      excelan-send-int-pkt  Yes          39 (100% hits)    13.3 seconds       75.2          36096
;; 60 bytes      excelan-send-int-pkt  No           n.a.              13.6 seconds       73.5          35280
;; 1500 bytes    excelan-send-int-pkt  Yes          14 (2% hits)      47.0 seconds       21.3         255600
;; 1500 bytes    excelan-send-int-pkt  Yes          39 (100% hits)    24.3 seconds       41.1         493200
;; 1500 bytes    excelan-send-int-pkt  No           n.a.              20.0 seconds       50.0         600000
;;
;; Explanations and inferences:
;;
;;   Access to this driver via link-xmit is currently possible only when the network receiver process does not
;;   service it.  Hence, non-blocking access is not currently provided.  The calling program allocated a
;;   dmabuf and repeatedly called link-xmit blockingly without deallocating it between calls.
;;
;;   Access to the driver via excelan-send-int-pkt is the usual route for a network-level protocol.  In this
;;   test, the calling program allocated an int-pkt and then called excelan-send-int-pkt which allocated a
;;   dmabuf, copied the data into it, and freed the int-pkt.  excelan-send-int-pkt then called link-xmit
;;   either blocking or non-blocking.  In the former case, excelan-send-int-pkt deallocated the dmabuf.
;;   In the latter, exintr deallocated the int-pkt.
;;
;;   For the non-blocking case, one half of the dmabufs (10) were permanently reserved for non-blocking
;;   receives.  Thus, up to 10 non-blocking sends would be out at once.
;;
;;   Some conclusions we can draw:
;;    - A busy-loop count of 14 will serve for the minimal size packet -- but only catches 2 or 3% of maximum
;;      size transmissions.  A busy-loop count of 39 will catch maximum size packets as well (with no
;;      additional overhead for the minimum size case -- the loop simply exits sooner for them).  THUS, we set
;;      our busy-loop count to 39 to maximize throughput for blocking sends.
;;    - blocking sends with access through link-xmit is quite fast; 50% faster than the fastest access through
;;      excelan-send-int-pkt.  Considering the additional allocations (int-pkt and dmabuf), buffer copy,
;;      and deallocations (int-pkt and dmabuf), this is not surprising.
;;    - blocking sends through excelan-send-int-pkt are marginally faster for small packets, but significantly
;;      slower for large packets.  THUS, we make non-blocking the default for excelan-send-int-pkt.

(defun ex-send-wait (kp)
  ;;Called by user.  If busy loops enabled and no-one else currently handling interrupts, try it ourselves
  (when (and (plusp (exc-exintr-busy-loops *excelan-ethernet-interface*))
             (sys:%store-conditional (locf *exintr-in-progress*) nil :user))
    (incf (exc-exintr-calls *excelan-ethernet-interface*))
    (do ((i 1 (1+ i)))
        ((or (kp-readyp kp)
             (and (> i (exc-exintr-busy-loops *excelan-ethernet-interface*))
                  (not (exintr-message-readyp)))))
      (when (exintr-message-readyp)
        (incf (exc-exintr-called-by-user *excelan-ethernet-interface*))
        (exintr)))
    (setf *exintr-in-progress* nil)
    (when (kp-readyp kp)
      (incf (exc-exintr-wins *excelan-ethernet-interface*))
      (return-from ex-send-wait t)))

  (when *exos-polling-p*                                ;let network receiver handles interrupts
    (process-wait "Exos reply" #'kp-readyp kp)
    (return-from ex-send-wait t))

  (do ()                                        ;loop until my kludge-buf is ready
      ((kp-readyp kp) t)                        ; and return t
    (cond ((and (exintr-message-readyp)         ;if excelan is ready
                (sys:%store-conditional (locf *exintr-in-progress*) nil :user))
           (incf (exc-exintr-called-by-user *excelan-ethernet-interface*))
           (exintr)                             ;service the interrupt
           (setf *exintr-in-progress* nil))
          (t
           (process-wait "Exos Reply" #'(lambda () (and (null *exintr-in-progress*)
                                                        (exintr-message-readyp))))))))

(defun exintr-stats (&optional resetp)
  (format t "~&~D calls, ~D wins, ~D percent, BUSY LOOP COUNT = ~D~%"
          (exc-exintr-calls *excelan-ethernet-interface*)
          (exc-exintr-wins *excelan-ethernet-interface*)
          (round (* (exc-exintr-wins *excelan-ethernet-interface*) 100)
                 (if (zerop (exc-exintr-calls *excelan-ethernet-interface*)) 1
                   (exc-exintr-calls *excelan-ethernet-interface*)))
          (exc-exintr-busy-loops *excelan-ethernet-interface*) )
  (when resetp
    (setf (exc-exintr-wins *excelan-ethernet-interface*) 0)
    (setf (exc-exintr-calls *excelan-ethernet-interface*) 0)
    (when (integerp resetp)
      (setf (exc-exintr-busy-loops *excelan-ethernet-interface*) resetp))))

;;;LINK-LEVEL-COMMAND
(defvar sendmsg-lock nil)                       ; lock used for sending messages

(defun link-level-command (cmd no-hang-p dmabuf &rest alternating-fields-and-values &aux mp kp)
  (declare (values reply data))
  (unless *excelan-initialized-p*
    (error "EXOS board has not been initialized yet.~%"))
  (unwind-protect
      (progn
        (with-lock (sendmsg-lock :timeout 600)
                   ;;We are inside a with-lock.  Nobody else calls ex-findmsg to allocate a kludge buffer
                   ;; and message queue element, nor ex-send to pass the message to the board.  THUS,
                   ;; "without-interrupt" constructs should be unnecessary here and in ex-findmsg and ex-send.
                   (setq kp (msg-kludge (setq mp (ex-findmsg))))
                   (when no-hang-p
                     (setf (kludge-buf kp) dmabuf)
                     (set-kludge-state-bits kp kl-non-blocking))
                   (setf (wmsg-area-lastw wmsgarea) (msg-link mp))
                   (zl:loop for x on alternating-fields-and-values by #'cddr
                            do (case (get (car x) 'size-in-bytes)
                                 (1 (write-uchar (cadr x) *dma* (msg-offset mp) (car x)))
                                 (2 (write-short (cadr x) *dma* (msg-offset mp) (car x)))
                                 (4 (write-long (cadr x) *dma* (msg-offset mp) (car x)))
                                 (6 (write-48bits (cadr x) *dma* (msg-offset mp) (car x)))
                                 (otherwise (error "~A unknown link-level offset~%" (car x)))))
                   (unless (eq kp (ex-send (symbol-value cmd) mp))
                     (error "ex-send returned unexpected kp")))
        (unless no-hang-p
          (ex-send-wait kp)
          (return-from link-level-command (values (kludge-reply kp) (kludge-data kp)))))
    (and kp (clear-kludge-state-bits kp kl-busy)))
  (values 0 0))

(defun exos-error (reply-code alist)
  (cond ((cdr (assoc reply-code alist)))
        (t (format nil "[Unknown error code ~X.]" reply-code))))

(defvar exos-link-net-mode-error-code-alist
        '((#x00 . "Successful completion")
          (#xa1 . "EXOS 201 not in controller mode")))

(defun link-net-mode (&key (read-p t) write-p ignore-alignment-errors-p ignore-crc-errors-p
                      net-disable-p (mode 'nmode-connect-hardware-filter))
  (check-type mode #.(append (ncons 'member) all-link-net-modes))
  (multiple-value-bind (reply-code other-data)
      (link-level-command 'link-net-mode (not read-p) nil
                          'link-nmode-reqmask (logior (if read-p reqmask-read 0)
                                                      (if write-p reqmask-write 0))
                          'link-nmode-optmask (logior
                                                (if ignore-alignment-errors-p
                                                    optmask-ignore-alignment-errors 0)
                                                (if ignore-crc-errors-p
                                                    optmask-ignore-crc-errors 0)
                                                (if net-disable-p
                                                    optmask-net-disable 0))
                          'link-nmode-mode (symbol-value mode))
    (when read-p
      (unless (zerop reply-code)
        (error "Exos link-level NET-MODE failed: ~A.~%"
               (exos-error reply-code exos-link-net-mode-error-code-alist)))
      (values (car other-data) (cdr other-data)))))

(defvar exos-link-net-recv-error-code-alist
        '((#x00 . "Successful completion")
          (#xd1 . "slot does not exist or access not permitted")
          (#xd2 . "slot is empty")
          (#xa1 . "EXOS 201 not in controller mode")))

(defun link-net-recv (&key (read-p t) write-p enable-p (slot *link-ethernet-slot*))
  (check-type slot (unsigned-byte 8))
  (multiple-value-bind (reply-code other-data)
      (link-level-command 'link-net-recv (not read-p) nil
                          'link-nrecv-reqmask (logior (if read-p reqmask-read 0)
                                                      (if write-p reqmask-write 0)
                                                      (if enable-p reqmask-enable 0))
                          'link-nrecv-slot slot)
    (when read-p
      (unless (zerop reply-code)
        (error "Exos link-level NET-RECV failed: ~A.~%"
               (exos-error reply-code exos-link-net-recv-error-code-alist)))
      (logand other-data (logior reqmask-read reqmask-write reqmask-enable)))))

(defvar exos-link-net-ststcs-error-code-alist
        '((#x00 . "Successful completion")
          (#xa1 . "EXOS 201 not in controller mode")))

(defconstant *net-ststcs-max-objects* 14.)
(defun link-net-ststcs (outbuf &optional (reset-p nil))
  (with-dmabuf (buf :net-ststcs)
    (multiple-value-bind (reply-code other-data)
        (link-level-command 'link-net-ststcs nil nil
                            'link-nststcs-reqmask (logior reqmask-read
                                                          (if reset-p reqmask-write 0))
                            'link-nststcs-reserved 0
                            'link-nststcs-nobjects *net-ststcs-max-objects*
                            'link-nststcs-index 0
                            'link-nststcs-address (dmabuf-multibus-address buf))
      (unless (zerop reply-code)
        (error "Exos link-level NET-STSTCS failed: ~A.~%"
               (exos-error reply-code exos-link-net-ststcs-error-code-alist)))
      (copy-array-portion buf 0 sizeof-exbdstats outbuf 0 sizeof-exbdstats)
      other-data)))

(defvar exos-link-net-addrs-error-code-alist
        '((#x00 . "Successful completion")
          (#xd1 . "slot does not exist or access not permitted")
          (#xd3 . "improper address for slot")
          (#xa1 . "EXOS 201 not in controller mode")))

(defun link-net-addrs (&key (slot *link-ethernet-slot*) (read-p t) (write-p nil) (multicast-address 0))
  (check-type slot (unsigned-byte 8))
  (check-type multicast-address (unsigned-byte 48))
  (multiple-value-bind (reply-code address)
      (link-level-command 'link-net-addrs (not read-p) nil
                          'link-naddress-reqmask (logior (if read-p reqmask-read 0)
                                                         (if write-p reqmask-write 0))
                          'link-naddress-slot slot
                          'link-naddress-addr multicast-address)
    (when read-p
      (unless (zerop reply-code)
        (error "Couldn't get Exos ethernet address: ~A.~%"
               (exos-error reply-code exos-link-net-addrs-error-code-alist)))
      address)))

(defvar exos-recv-error-code-alist
        '((#x00 . "Successful completion")
          (#x04 . "Packet truncated")
          (#x10 . "Alignment error")
          (#x20 . "CRC error")
          (#x40 . "Supplied buffer less than 64 bytes")
          (#xa1 . "EXOS 201 not in controller mode")))

(defun link-recv (dmabuf &optional no-hang-p)
  (incf *excelan-link-recv-out*)
  (multiple-value-bind (reply-code nbytes)
      (link-level-command 'link-recv no-hang-p (if no-hang-p dmabuf)
                          'link-tr-nblocks 1
                          'link-tr-length bsize
                          'link-tr-address (dmabuf-multibus-address dmabuf))
    (unless (zerop reply-code)
      (error "Link-level RECV failed: ~A.~%"
             (exos-error reply-code exos-recv-error-code-alist)))
    nbytes))

(defvar exos-xmit-error-code-alist
        '((#x00 . "Successful completion")
          (#x01 . "Successful transmission, 1 retry")
          (#x02 . "Successful transmission, more than 1 retry")
          (#x10 . "Excessive collisions")
          (#x20 . "no Carrier Sense")
          (#x40 . "Transmit length not in range")
          (#xa1 . "EXOS 201 not in controller mode")))

(defun link-xmit (dmabuf nbytes &optional no-hang-p)
  (check-type nbytes (integer 60 1514))
  (multiple-value-bind (reply-code ignore)
      (link-level-command (if (net:ni-loopback *excelan-ethernet-interface*) 'link-xmit 'link-xmit-self-recv)
                          no-hang-p (if no-hang-p dmabuf)
                          'link-tr-nblocks 1
                          'link-tr-length nbytes
                          'link-tr-address (dmabuf-multibus-address dmabuf))
    (unless (member reply-code '(#x00 #x01 #x02))
      (error "Link-level XMIT failed: ~A.~%"
             (exos-error reply-code exos-xmit-error-code-alist))))
  t)

;;;INITIALIZATION

;;; some of the dummy entries are due to byte swapping
(defconstant init-msg-newstyle 0)               ; new style init msg?
(setf (get 'init-msg-newstyle 'size-in-bytes) 2)

(defconstant init-msg-version0 2)               ; version of the hardware
(setf (get 'init-msg-version0 'size-in-bytes) 1)
(defconstant init-msg-version1 3)
(setf (get 'init-msg-version1 'size-in-bytes) 1)
(defconstant init-msg-version2 4)
(setf (get 'init-msg-version2 'size-in-bytes) 1)
(defconstant init-msg-version3 5)
(setf (get 'init-msg-version3 'size-in-bytes) 1)

(defconstant init-msg-result 6)                 ; completion code
(setf (get 'init-msg-result 'size-in-bytes) 1)

(defconstant init-msg-mode 7)                   ; operation mode
(setf (get 'init-msg-mode 'size-in-bytes) 1)

(defconstant init-msg-hdfo0 8)                  ; host data format option
(setf (get 'init-msg-hdfo0 'size-in-bytes) 1)
(defconstant init-msg-hdfo1 9)
(setf (get 'init-msg-hdfo1 'size-in-bytes) 1)

(defconstant init-msg-junk0 10)
(setf (get 'init-msg-junk0 'size-in-bytes) 1)
(defconstant init-msg-junk1 11)
(setf (get 'init-msg-junk1 'size-in-bytes) 1)
(defconstant init-msg-junk2 12)
(setf (get 'init-msg-junk2 'size-in-bytes) 1)

(defconstant init-msg-haddrmode 13)             ; host address mode
(setf (get 'init-msg-haddrmode 'size-in-bytes) 1)

(defconstant init-msg-dummy2 14)
(setf (get 'init-msg-dummy2 'size-in-bytes) 1)

(defconstant init-msg-mmsize 15)                ; memory map size (returned)
(setf (get 'init-msg-mmsize 'size-in-bytes) 1)

(defconstant init-msg-byteptn0 16)              ; data order byte pattern
(setf (get 'init-msg-byteptn0 'size-in-bytes) 1)
(defconstant init-msg-byteptn1 17)
(setf (get 'init-msg-byteptn1 'size-in-bytes) 1)
(defconstant init-msg-byteptn2 18)
(setf (get 'init-msg-byteptn2 'size-in-bytes) 1)
(defconstant init-msg-byteptn3 19)
(setf (get 'init-msg-byteptn3 'size-in-bytes) 1)

(defconstant init-msg-wordptn0 20)              ; data order word pattern
(setf (get 'init-msg-wordptn0 'size-in-bytes) 2)
(defconstant init-msg-wordptn1 22)
(setf (get 'init-msg-wordptn1 'size-in-bytes) 2)

(defconstant init-msg-longptn 24)               ; data order long pattern
(setf (get 'init-msg-longptn 'size-in-bytes) 4)

(defmacro init-msg-mmap (n)                     ; rest of memory map returned
  `(+ 24 ,n))
(defconstant sizeof-init-msg-mmap 20.)

(defconstant init-msg-101off 48)                ; movable block offset
(setf (get 'init-msg-101off 'size-in-bytes) 2)

(defconstant init-msg-101seg 50)                ; movable block segment
(setf (get 'init-msg-101seg 'size-in-bytes) 2)

(defconstant init-msg-nproc 52)                 ; number of exos 101 processes
(setf (get 'init-msg-nproc 'size-in-bytes) 1)

(defconstant init-msg-nmb 53)                   ; number of exos 101 mailboxes
(setf (get 'init-msg-nmb 'size-in-bytes) 1)

(defconstant init-msg-nslots 54)                ; number of address slots
(setf (get 'init-msg-nslots 'size-in-bytes) 1)

(defconstant init-msg-nhosts 55)                ; number of hosts = 1
(setf (get 'init-msg-nhosts 'size-in-bytes) 1)

;;; host to exos stuff
(defconstant init-msg-h2exqaddr 56)             ; host to exos msg q address
(setf (get 'init-msg-h2exqaddr 'size-in-bytes) 4)

(defconstant init-msg-h2exoff 60)               ; offset from base of actual q
(setf (get 'init-msg-h2exoff 'size-in-bytes) 2)

(defconstant init-msg-h2extype 62)              ; interrupt type for h2ex msg q
(setf (get 'init-msg-h2extype 'size-in-bytes) 1)

(defconstant init-msg-h2exvalue 63)             ; interupt value
(setf (get 'init-msg-h2exvalue 'size-in-bytes) 1)

(defconstant init-msg-h2exaddr 64)              ; interrupt address
(setf (get 'init-msg-h2exaddr 'size-in-bytes) 4)

;;; "exos to host" stuff
(defconstant init-msg-ex2hqaddr 68)             ; exos to host msg q address
(setf (get 'init-msg-ex2hqaddr 'size-in-bytes) 4)

(defconstant init-msg-ex2hoff 72)               ; offset from base of actual q
(setf (get 'init-msg-ex2hoff 'size-in-bytes) 2)

(defconstant init-msg-ex2htype 74)              ; interrupt type for ex2h msg q
(setf (get 'init-msg-ex2htype 'size-in-bytes) 1)

(defconstant init-msg-ex2hvalue 75)             ; interupt value
(setf (get 'init-msg-ex2hvalue 'size-in-bytes) 1)

(defconstant init-msg-ex2haddr 76)              ; interupt address
(setf (get 'init-msg-ex2haddr 'size-in-bytes) 4)

(defconstant sizeof-init-msg 80)

; im-mode
(defconstant exos-linkmode 0)
(defconstant exos-hostload 1)
(defconstant exos-netload 2)

(defun initialize-sw nil                        ; should only be called once, at load time.
  (ex-stop-poll)                                ; but this makes this function safe to call after net started
  (initialize-dma)
  (initialize-kludge)
  (initialize-msgareas))

(defun reset-sw nil
  (ex-stop-poll)                                     ; turn off the polling process
  (reset-kludge)
  (reset-dmabuf-record)
  (setq *exintr-in-progress* nil)
  (setq sendmsg-lock nil)
  (setq *excelan-link-recv-out* 0)
  (setq *excelan-reply-list* (make-fifo)))                   ; no buffers waiting

(defvar exsetup-verbose nil)

;;; error codes returned from initialization message reply
;;; from Exos/101 reference manual, page 23
(defvar exos-config-error-code-alist
        '((#x00 . "Successful completion")
          (#xa4 . "Invalid operation mode")
          (#xa5 . "Invalid host data format test pattern")
          (#xa7 . "Invalid configuration message format")
          (#xa8 . "Invalid movable block address")
          (#xa9 . "Invalid number of processes")
          (#xaa . "Invalid number of mailboxes")
          (#xab . "Invalid number of address slots")
          (#xae . "Insufficient memory for movable data block")
          (#xaf . "Net boot failed")))

(defun exsetup (&aux magic current current-offset next next-offset timeout addr tmp)
  (with-dmabuf (im :initialize)
    (setq magic (make-array 8
                            :element-type '(unsigned-byte 8)
                            :initial-contents '(#xff #xff 0 0 0 0 0 0)))

    ;; link together the read ("exos to host") message queue
    (write-short 2 *dma* x2hq-offset 'rmsg-area-rlink)
    (setf (rmsg-area-offset rmsgarea) (+ x2hq-offset rmsg-area-rlink))
    (setf (rmsg-area-lastr rmsgarea) (aref (rmsg-area-msgs rmsgarea) 0))

    (setq current (aref (rmsg-area-msgs rmsgarea) (1- net-rbufs)))
    (setq current-offset (+ 2 (* sizeof-exos-msg (1- net-rbufs))))
    (dotimes (i net-rbufs)
      (setq next (aref (rmsg-area-msgs rmsgarea) i))
      (setq next-offset (+ 2 (* sizeof-exos-msg i)))    ; offset of message from beginning of buffer
      (setf (msg-offset next) (+ x2hq-offset next-offset))
      (write-short next-offset *dma*
                   (+ x2hq-offset current-offset) 'headers-link)
      (write-short (- sizeof-exos-msg sizeof-headers) *dma*
                   (+ x2hq-offset current-offset) 'headers-length)
      (write-uchar 3 *dma*
                   (+ x2hq-offset current-offset) 'headers-status)
      (setf (msg-link current) next)
      (setq current next current-offset next-offset))

    (setq rqueue-offset-alist
          (zl:loop for i from 0 below net-rbufs
                   collect (cons (msg-offset (aref (rmsg-area-msgs rmsgarea) i))
                                 (aref (rmsg-area-msgs rmsgarea) i))))

    ;; link together the write ("host to exos") message queue
    (write-short 2 *dma* h2xq-offset 'wmsg-area-wlink)
    (setf (wmsg-area-offset wmsgarea) (+ h2xq-offset wmsg-area-wlink))
    (setf (wmsg-area-lastw wmsgarea) (aref (wmsg-area-msgs wmsgarea) 0))

    (setq current (aref (wmsg-area-msgs wmsgarea) (1- net-wbufs)))
    (setq current-offset (+ 2 (* sizeof-exos-msg (1- net-wbufs))))
    (dotimes (i net-wbufs)
      (setq next (aref (wmsg-area-msgs wmsgarea) i))
      (setq next-offset (+ 2 (* sizeof-exos-msg i)))    ; offset of message from beginning of buffer
      (setf (msg-offset next) (+ h2xq-offset next-offset))
      (write-short next-offset *dma*
                   (+ h2xq-offset current-offset) 'headers-link)
      (write-short (- sizeof-exos-msg sizeof-headers) *dma*
                   (+ h2xq-offset current-offset) 'headers-length)
      (write-uchar 0 *dma*
                   (+ h2xq-offset current-offset) 'headers-status)
      (setf (msg-link current) next)
      (setq current next current-offset next-offset))

    (setq wqueue-offset-alist
          (zl:loop for i from 0 below net-rbufs
                   collect (cons (msg-offset (aref (wmsg-area-msgs wmsgarea) i))
                                 (aref (wmsg-area-msgs wmsgarea) i))))

    ;; setup init-msg data structure
    (array-initialize im 0 0 sizeof-init-msg)
    (write-short 1 im 0 'init-msg-newstyle)     ; new-style init msg
    (write-uchar exos-linkmode im 0 'init-msg-mode)     ; download mode
    (write-uchar 1 im 0 'init-msg-hdfo0)        ; auto-byte/word swapping
    (write-uchar 1 im 0 'init-msg-hdfo1)
    (write-uchar 1 im 0 'init-msg-junk0)
    (write-uchar 3 im 0 'init-msg-haddrmode)    ; absolute address mode

    ;; data order test patterns
    (write-uchar 1 im 0 'init-msg-byteptn0)
    (write-uchar 3 im 0 'init-msg-byteptn1)
    (write-uchar 7 im 0 'init-msg-byteptn2)
    (write-uchar #xf im 0 'init-msg-byteptn3)
    (write-short #x103 im 0 'init-msg-wordptn0)
    (write-short #x70f im 0 'init-msg-wordptn1)
    (write-long #x103070f im 0 'init-msg-longptn)

    (write-short #xffff im 0 'init-msg-101off)
    (write-short #xffff im 0 'init-msg-101seg)
    (write-uchar 1 im 0 'init-msg-nhosts)
    (write-uchar #xff im 0 'init-msg-result)
    (write-uchar #xff im 0 'init-msg-nmb)
    (write-uchar #xff im 0 'init-msg-nproc)
    (write-uchar #xff im 0 'init-msg-nslots)

    ;; setup pointer to host-to-exos message queue
    (write-long (offset-to-multibus-address h2xq-offset) im 0 'init-msg-h2exqaddr)
    (write-short 0 im 0 'init-msg-h2exoff)
    (write-uchar 0 im 0 'init-msg-h2extype)

    ;; setup pointer to exos-to-host message queue
    (write-long (offset-to-multibus-address x2hq-offset) im 0 'init-msg-ex2hqaddr)
    (write-short 0 im 0 'init-msg-ex2hoff)
    (write-uchar 0 im 0 'init-msg-ex2htype)

    ;; give init-msg to exos
    (inb porta)
    (setq timeout 1000000)
    (loop
      (if (or (not (zerop (logand (inb portb) pb-error))) (zerop (decf timeout)))
          (return)))
    (if exsetup-verbose (format t "~&EXOS DIAGS: ~D reads of portb~%" (- 1000000 timeout)))
    (if (zerop timeout)
        (error "EXOS board failed diagnostics,~%"))

    ;;; output addr of init msg in absolute format:
    ;;; #xffff0000 followed by the address backwards, bytewise

    ;; get addr into array
    (setq addr (dmabuf-multibus-address im))
    (dotimes (i 4)
      (setf (aref magic (+ i 4)) (ldb (byte 8 0) addr))
      (setq addr (ash addr -8)))

    ;; output the bytes
    (dotimes (i 8)
      (setq timeout 100000)
      (loop
        (if (or (zerop (logand (inb portb) pb-ready)) (zerop (decf timeout)))
            (return)))
      (if exsetup-verbose (format t "~&INIT ADDR: ~D reads of portb~%" (- 100000 timeout)))
      (if (zerop timeout)
          (error "EXOS board hung while sending init addr.~%"))
      (outb portb (aref magic i)))

    (setq timeout 1000000)
    (do ()
        ((or (zerop (decf timeout)) (/= #xff (read-uchar im 0 'init-msg-result)))))
    (if exsetup-verbose (format t "~&EXOS CONFIG: ~D busy loops~%" (- 1000000 timeout)))
    (if (zerop timeout)
        (error "EXOS: board hung while waiting for init to complete.~%"))

    (setq tmp (read-uchar im 0 'init-msg-result))
    (save-init-msg im)
    (if exsetup-verbose (describe-init-msg im))
    (unless (zerop tmp)
      (error "EXOS board initialization error: ~A.~%"
             (exos-error tmp exos-config-error-code-alist)))

    (outb portb 0)
    (setq *excelan-initialized-p* t)
    t))

(defvar *last-init-msg* nil "setq'd or rplaca'd by SAVE-INIT-MSG")

(defun save-init-msg (array)
  (or *last-init-msg* (setq *last-init-msg* (make-array sizeof-init-msg :element-type '(unsigned-byte 8))))
  (copy-array-portion array 0 sizeof-init-msg *last-init-msg* 0 sizeof-init-msg))

(defvar *lp* nil "Use as a pointer-to-long data in READ-LONG, bound to offsets at will")
(setf (get '*lp* 'size-in-bytes) 4)

(defun describe-init-msg (array)
  (format t "~&NX release ~c.~c~
             ~%Exos Release ~c.~c~%"
          (read-uchar array 0 'init-msg-version0)
          (read-uchar array 0 'init-msg-version1)
          (read-uchar array 0 'init-msg-version2)
          (read-uchar array 0 'init-msg-version3))
  (format t "Completion code ~16R~%"
          (read-uchar array 0 'init-msg-result))
  (format t "Context = ~D~%" (read-uchar array 0 'init-msg-junk1))
  (format t "Port B = ~X~%" (read-uchar array 0 'init-msg-dummy2))
  (format t "Memory Map size = ~D~%" (read-uchar array 0 'init-msg-mmsize))
  (let ((*lp* init-msg-byteptn0))
    (dotimes (j (min 4 (read-uchar array 0 'init-msg-mmsize)))
      (format t "Memory map segment: ~X ~X~%"
              (read-long array 0 '*lp*)
              (read-long array 4 '*lp*))
      (incf *lp* 8)))
  (format t "Number of Processes:       ~D~%~
             Number of Mailboxes:       ~D~%~
             Number of Multicast Slots: ~D~%~
             Number of Hosts:           ~D~%"
          (read-uchar array 0 'init-msg-nproc)
          (read-uchar array 0 'init-msg-nmb)
          (read-uchar array 0 'init-msg-nslots)
          (read-uchar array 0 'init-msg-nhosts))
  ;; this never seems to indicate failure. Comment in netload.c about
  ;; what the xmem driver does with port b may be looked at.
  (format t "Transceiver loop-back test ~:[ok~;failed~]~%"
          (and (not (zerop (read-uchar array 0 'init-msg-junk1)))
               (not (zerop (logand #x40 (read-uchar array 0 'init-msg-dummy2)))))))

;;; ethernet packet
(defconstant ether-dhost 0)
(setf (get 'ether-dhost 'size-in-bytes) 6)

(defconstant ether-shost 6)
(setf (get 'ether-shost 'size-in-bytes) 6)

(defconstant ether-type 12.)
(setf (get 'ether-type 'size-in-bytes) 2)

(defconstant ether-data-start 14.)
(defconstant sizeof-ether-data (- 1514. 14.))

(defconstant ether-data-start-16 7.)
(defconstant sizeof-ether-data-16 (- 756. 7.))

;;; THE EXCELAN NETWORK-INTERFACE

(defun excelan-disable (stream &optional (reset-p t))
  (declare (ignore stream))
  (ex-stop-poll)
  (link-net-mode :read-p nil :write-p t :mode 'nmode-disconnect)
  (link-net-recv :read-p nil :write-p t :enable-p nil :slot *link-ethernet-slot*)
  (link-net-recv :read-p nil :write-p t :enable-p nil :slot *link-broadcast-slot*)
  (do ((rp (pop-fifo *excelan-reply-list*)
           (pop-fifo *excelan-reply-list*)))
      ((null rp))
    (when (rp-buf rp)
      (free-dmabuf (rp-buf rp))))
  (when reset-p
    (free-dma-resources)))

(defun excelan-reset (stream)
  (declare (ignore stream))
  (when (i-own-excelan)
    (inb porta)                                 ;reset the board
    (or *dma-initialized-p* (initialize-sw))
    (reset-sw)
    (exsetup)))

(defun excelan-enable (stream &optional (link-recv-count (ceiling *n-dmabufs* 2)))
  ;;default usage of dmabufs: half for non-blocking receives, half for non-blocking sends
  (when (i-own-excelan)
    (unless *excelan-initialized-p*
      (excelan-reset stream))
    (setq *excelan-link-recv-count* link-recv-count)
    (setf (net:ni-address *excelan-ethernet-interface*) (link-net-addrs))
    (link-net-mode :read-p nil :write-p t :mode 'nmode-connect-hardware-filter)
    (link-net-recv :read-p nil :write-p t :enable-p t :slot *link-ethernet-slot*)
    (link-net-recv :read-p nil :write-p t :enable-p t :slot *link-broadcast-slot*)
    (dotimes (i (- *excelan-link-recv-count* *excelan-link-recv-out*))
      (let ((buffer (allocate-dmabuf :recv)))
        (link-recv buffer t)))
    (ex-do-poll)                                ; restart the polling process
    t))

(defun excelan-send-int-pkt (int-pkt e-source e-dest e-type n-16-bit-words &optional (no-hang-p t)
                             &aux buffer buf16 tmp sent)
  (net:assure-safe-int-pkt int-pkt)
  (unwind-protect
      (progn
        (setq buffer (allocate-dmabuf :xmit))
        (write-48bits e-dest buffer 0 'ether-dhost)
        (write-48bits e-source buffer 0 'ether-shost)
        (write-short e-type buffer 0 'ether-type)
        (setq buf16 (get-dmabuf-16 buffer))
        (setq tmp (+ ether-data-start-16 n-16-bit-words))
        (copy-array-portion int-pkt 0 n-16-bit-words buf16 ether-data-start-16 tmp)
        ;;max 60: dont send runt packets.
        (setq sent (link-xmit buffer (min 1514 (max 60 (* tmp 2))) no-hang-p))
        ;;Fall out, return "sent"
        )
    (unless (and sent no-hang-p)
      (and buffer (free-dmabuf buffer)))
    (free-packet int-pkt)))

(defun excelan-packet-ready ()
  (or (and *exos-polling-p*
           (exintr-message-readyp)
           (sys:%store-conditional (locf *exintr-in-progress*) nil :system))
      (not (fifo-empty-p *excelan-reply-list*))))

(defun excelan-get-next-packet (&aux nwords int-pkt buffer)
  (declare (values packet type source destination broadcast-p))

  (when (and *exos-polling-p*                   ; If this process does excelan polling..
             (eq *exintr-in-progress* :system)) ;  and it is currently ours to do
    (do ()
        ((not (exintr-message-readyp)))         ; until no interrupt pending,
      (incf (exc-exintr-called-by-system *excelan-ethernet-interface*))
      (exintr))                                 ; service the interrupt
    (setf *exintr-in-progress* nil))

  (unless (fifo-empty-p *excelan-reply-list*)
    (when (setq int-pkt (allocate-packet-for-receive *excelan-ethernet-interface*))
      (do ((item (pop-fifo *excelan-reply-list*)
                 (pop-fifo *excelan-reply-list*)))
          ((null item))
        (when (eql (rp-command item) link-recv)
          (setq buffer (rp-buf item))           ; retrieve dmabuf
          (unwind-protect
              (when (zerop (rp-reply item))
                ;; (rp-data item) is number of bytes in packet, including 4 byte FCS at end
                (setq nwords (ceiling (- (rp-data item) 18) 2))
                (copy-array-portion (get-dmabuf-16 buffer)
                                    ether-data-start-16
                                    (+ nwords ether-data-start-16)
                                    int-pkt 0 nwords)
                (setf (fill-pointer int-pkt) nwords)
                (return-from excelan-get-next-packet
                  (let ((dest (read-48bits buffer 0 'ether-dhost)))
                    (values int-pkt
                            (read-short buffer 0 'ether-type)
                            (read-48bits buffer 0 'ether-shost)
                            dest
                            (= dest *ethernet-broadcast-address*)))))
            (if (and *exos-polling-p*
                     (< *excelan-link-recv-out* *excelan-link-recv-count*))
                (link-recv buffer t)            ; else, generate another non-blocking receive
              (free-dmabuf buffer)))))
      (free-packet int-pkt)
      nil)))

(defun setup-excelan (tag &optional (enable-p t) &aux alist)
  (when (and (eq si:processor-type-code si:lambda-type-code)
             (i-own-excelan))

    (when *excelan-ethernet-interface*
      (setq alist (net:ni-protocol-alist *excelan-ethernet-interface*))
      (funcall *excelan-ethernet-interface* :close))

    (setq *excelan-ethernet-interface*
          (make-excelan-interface :tag tag
                                  :interface :excelan
                                  :keyword :ethernet
                                  :hardware-type net:arp-ethernet-hardware
                                  :address-length 6
                                  :address 0
                                  :broadcast-address *ethernet-broadcast-address*
                                  :minimum-data-length 46.
                                  :maximum-data-length 1500.
                                  :sent-header-length 0
                                  :rcvd-header-length 0
                                  :sent-trailer-length 0
                                  :rcvd-trailer-length 0
                                  :loopback t   ;will operate properly either way
                                  :protocol-alist alist
                                  :reset-function 'excelan-reset
                                  :enable-function 'excelan-enable
                                  :disable-function 'excelan-disable
                                  :packet-ready-function 'excelan-packet-ready
                                  :get-next-packet-function 'excelan-get-next-packet
                                  :send-function 'excelan-send-int-pkt
                                  :gauge-name "Excelan"
                                  ))

    (funcall *excelan-ethernet-interface* :open)
    (when enable-p
      (funcall *excelan-ethernet-interface* :enable))))
