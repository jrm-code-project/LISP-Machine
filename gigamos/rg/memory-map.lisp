;;; -*- Mode:LISP; Package:MAP; Base:10.; Readtable:CL -*-

(in-package 'map)

(export '(
          $$map-status-read-mar
          $$map-status-read-only
          $$map-status-read-mar-aged
          $$map-status-direct-mapped
          $$map-status-read-only-aged
          $$map-status-swapped-out
          $$map-status-normal-aged
          $$map-status-normal

          associate-local-memory
          associate-nubus-memory
          direct-map
          dump-map
          extract-map-status
          flush-direct-map
          free-virtual-cluster
          fresh-cluster?
          inject-map-status
          illop-if-not-lisp-map
          lisp-map
          load-map
          map-cluster-volatility
          map-local-memory?
          map-on-board-address
          read-cluster-volatility
          read-map
          read-map-status
          write-cluster-volatility
          write-map
          write-map-status
          ))

;;; Map modes are governed by the memory control register

(vinc::defflag-extractor map-lisp-mode? hw:%%memory-control-l-c-map-select hw:$$lisp-map-bits)

(defun illop-if-not-lisp-map ()
  (when (not (map-lisp-mode? (hw:read-memory-control)))
    (trap::illop "Memory control is in C mode.")))

(define-control-register-modifier modify-l-c-map-select modify-memory-control
  hw:%%memory-control-l-c-map-select)

(defun lisp-map ()
  (modify-l-c-map-select hw:$$lisp-map-bits))

(defconstant *number-of-map-entries* (vinc:field-maximum hw:%%mapped-vma-byte))

;;; The 4 software definable bits.
(defconstant %%map-status-bits   (byte 2.  8.))
(defconstant %%map-fresh-cluster (byte 1. 10.))
;; unused                        (byte 1. 11.)

(defconstant $$cluster-not-fresh 0)
(defconstant $$cluster-fresh     1)

(vinc::defextractor      map-lisp-trap-bits        hw:%%map-lisp-trap-bits)
(vinc::defextractor      map-lisp-valid-bit        hw:%%map-lisp-valid-bit)
(vinc::defextractor      map-lisp-write-enable-bit hw:%%map-lisp-write-enable-bit)
(vinc::defextractor      map-local-memory-bit      hw:%%map-local-memory-bit)
(vinc::defflag-extractor map-local-memory?         hw:%%map-local-memory-bit hw:$$map-local)
(vinc::defextractor      map-on-board-address      hw:%%map-on-board-address)
(vinc::defextractor      map-off-board-address     hw:%%map-off-board-address)
(vinc::defextractor      map-cluster-volatility    hw:%%map-volatility)
(vinc::defextractor      map-cluster-status-bits      %%map-status-bits)
(vinc::defextractor      map-cluster-fresh-bit        %%map-fresh-cluster)
(vinc::defflag-extractor cluster-is-fresh?            %%map-fresh-cluster $$cluster-fresh)

;;; The map status is a field of 4 bits, two of which are the
;;; lisp trap bits, the other two being the software defined
;;; status bits.

(defconstant %%map-status-v-we-bits (byte 2. 0.))
(defconstant %%map-status-s-bits    (byte 2. 2.))

(vinc::defextractor status-s-bits    %%map-status-s-bits)
(vinc::defextractor status-v-we-bits %%map-status-v-we-bits)

;(defconstant $$map-status-read-mar          #b0000)
;(defconstant $$map-status-read-only         #b0001)
;(defconstant $$map-status-aged-to-read-mar  #b0010)
;(defconstant $$map-status-direct-mapped     #b0011)
;;read mar    #b0100
;;read only   #b0101
;(defconstant $$map-status-aged-to-read-only #b0110)
;;normal      #b0111
;(defconstant $$map-status-swapped-out       #b1000)
;;read only   #b1001
;;unused      #b1010
;;normal      #b1011
;;swapped out #b1100
;;read only   #b1101
;(defconstant $$map-status-aged-to-normal    #b1110)
;(defconstant $$map-status-normal            #b1111)

;;; Map status for ageable clusters is the same
;;; in the s bits and v-we bits for the unaged version
;;; so to unage an entry just copy the s bits to the v-we.
;;; Aged versions are the same as unaged versions with 10
;;; in the v-we bits.
(defconstant $$map-status-read-mar       #b0000)
; 0001
(defconstant $$map-status-read-mar-aged  #b0010)
; 0011
; 0100
(defconstant $$map-status-read-only      #b0101)
(defconstant $$map-status-read-only-aged #b0110)
; 0111
(defconstant $$map-status-swapped-out    #b1000)
; 1001
; 1010
; 1011
; 1100
(defconstant $$map-status-direct-mapped  #b1101)        ;not used after booting
(defconstant $$map-status-normal-aged    #b1110)
(defconstant $$map-status-normal         #b1111)

(defconstant $$map-aged-trap-bits #b10)


(defsubst extract-map-status (map-bits)
  (hw:dpb (map-cluster-status-bits map-bits)
          %%map-status-s-bits
          (map-lisp-trap-bits map-bits)))

(defsubst inject-map-status (map-bits status)
  (vinc::dpb-multiple-unboxed
    (status-s-bits    status)    %%map-status-bits      ;software bits.
    (status-v-we-bits status) hw:%%map-lisp-trap-bits   ;valid-and-write-enable bits
    map-bits))

;;; This is stupid.
;(defsetf extract-map-status inject-map-status)

(defsubst address-map (virtual-cluster)
  (hw:write-vma-unboxed (cluster->address virtual-cluster))
  ;; Wait for the VMA to load.
  (hw:nop)
  (hw:nop)
  (hw:nop)
  nil)

(defun read-map (virtual-cluster)
  (address-map virtual-cluster)
  (hw:read-map))

(defun write-map (virtual-cluster value)
  ;; HW:WRITE-MAP must be called with traps off to ensure
  ;; that the maps get written with the right value.
  (trap::without-traps
    #'(lambda ()
        (address-map virtual-cluster)
        (hw:write-map value)
        ;; Avoid mmfio collisions.
        (hw:nop)
        (hw:nop)
        nil)))

(defmacro modify-map (virtual-cluster modifier)
  ;; Atomically modifies the map by ensuring traps are off.
  ;; Use this function if you are not blasting the map data.
  ;; NOTE:  We don't re-address the map after funcalling the
  ;; modifier.  This can lose if the modifier touches the VMA.
  ;; I don't think we need to "do it right" yet.  (I just know
  ;; I'm going to screw someone with this...)
  `(LET ((VIRTUAL-CLUSTER ,virtual-cluster)
         (MODIFIER        ,modifier))
     (trap::without-traps
       #'(LAMBDA ()
           (ADDRESS-MAP VIRTUAL-CLUSTER)
           (LET ((NEW-VALUE (FUNCALL MODIFIER (HW:READ-MAP))))
             (HW:WRITE-MAP NEW-VALUE)
             (HW:NOP)
             (HW:NOP)
             nil)))))

(defun read-map-status (virtual-cluster)
  (extract-map-status (read-map virtual-cluster)))

(defun write-map-status (virtual-cluster new-status)
  (modify-map virtual-cluster
    #'(lambda (map-value)
        (inject-map-status map-value new-status))))

;;; If the map-status definitions change,
;;; these two may need to change also
(defun age-virtual-cluster (virtual-cluster)
  (modify-map virtual-cluster
    #'(lambda (map-value)
        (hw:dpb $$map-aged-trap-bits hw:%%map-lisp-trap-bits map-value))))

;;; This is called with the address in the vma
;;; and the original map value
;;; - And with traps off I hope - JRM
(defun touch-aged (map-value)
  (hw:write-map
    (hw:dpb (map-cluster-status-bits map-value)
            hw:%%map-lisp-trap-bits
            map-value)))

;;; This will win better, though it is slower.
;(defun touch-aged (virtual-cluster)
;  (modify-map virtual-cluster
;    #'(lambda (map-value)
;       (hw:dpb (map-cluster-status-bits map-value)
;               hw:%%map-lisp-trap-bits
;               map-value))))

(defun read-cluster-volatility (virtual-cluster)
  (map-cluster-volatility (read-map virtual-cluster)))

(defun verify-map-status (status new-status)
  (unless (= new-status $$map-status-direct-mapped)
    (when (not (or (= status $$map-status-swapped-out)  ;ok to bonk these.
                   (= status $$map-status-direct-mapped)))
      (trap::illop "Illegal attempt to change the map status of a cluster in use."))))

(defun write-cluster-volatility (virtual-cluster new-volatility)
  (modify-map virtual-cluster
    #'(lambda (map-value)
        (hw:dpb new-volatility hw:%%map-volatility
                map-value))))

(defun free-swapped-out-virtual-cluster (virtual-cluster)
  (write-map virtual-cluster
             (inject-map-status
               (vinc::dpb-multiple-unboxed
                 ;; Local to avoid trashing bus if we crash.
                 hw:$$map-local         hw:%%map-local-memory-bit
                 0.                     hw:%%map-volatility
                 $$cluster-not-fresh    %%map-fresh-cluster
                 (hw:unboxed-constant 0))
               $$map-status-swapped-out)))

(defun free-virtual-cluster (virtual-cluster)
  (write-map virtual-cluster
             (inject-map-status
               (vinc::dpb-multiple-unboxed
                 ;; Local to avoid trashing bus if we crash.
                 hw:$$map-local  hw:%%map-local-memory-bit
                 0.              hw:%%map-volatility
                 $$cluster-fresh    %%map-fresh-cluster
                 (hw:unboxed-constant 0))
               $$map-status-swapped-out)))

(defconstant %%map-read-status-we-bit (byte 1. 0.))
(defconstant %%map-read-status-s-bits (byte 2. 1.))

(defconstant $$map-read-status-read-mar-1       #b000)
(defconstant $$map-read-status-aged-to-read-mar #b001)
(defconstant $$map-read-status-read-mar-2       #b010)
(defconstant $$map-read-status-aged-to-readable #b011)
(defconstant $$map-read-status-swapped-out-1    #b100)
(defconstant $$map-read-status-unused           #b101)
(defconstant $$map-read-status-swapped-out-2    #b110)
(defconstant $$map-read-status-aged-to-normal   #b111)

(defun associate-cluster (local-bit physical-cluster virtual-cluster status)
  ;note carefully this is not the same function as PCD-TABLE:ASSOCIATE-CLUSTER.  Both hack the map tho.
;  (trap::illop "Associate cluster.")
  (modify-map virtual-cluster
    #'(lambda (map-entry)
        (verify-map-status (extract-map-status map-entry) status)
        (inject-map-status
          (vinc::dpb-multiple-unboxed
            local-bit           hw:%%map-local-memory-bit
            physical-cluster    hw:%%map-off-board-address
            $$cluster-not-fresh    %%map-fresh-cluster
            map-entry)
          status))))

(defun associate-local-memory (physical-cluster virtual-cluster status)
;  (trap::illop "associate local memory")
  (associate-cluster hw:$$map-local     physical-cluster virtual-cluster status))

(defun associate-nubus-memory (nubus-cluster virtual-cluster status)
  (associate-cluster hw:$$map-non-local nubus-cluster    virtual-cluster status))

;;; This probably shouldn't be here.
(defsubst physical-block-exists? (block physical-memory-layout)
  (hw:32logbitp block physical-memory-layout))

(defun direct-map (physical-memory-layout)
  ;; First, we blow away the entire map with $$map-status-direct-mapped
  ;; this won't hurt because it is read-only and the instructions are read
  ;; only.  Then, we point the bottom parts of virtual memory at the blocks
  ;; of physical memory to make them appear contiguous.  Later on, when we
  ;; flush this direct map, anything marked as initial code will be thrown
  ;; away, and anything marked as direct map will be placed on the freelist.
  (labels (
           (zap-map (entry)
             (if (= entry *number-of-map-entries*)
                 '()
                 (progn (write-map-status entry $$map-status-direct-mapped)
                        (zap-map (1+ entry)))))

           (associate-block (virtual physical)
             (dotimes (cluster-in-block vinc:*clusters-in-physical-block*)
;              (trap::illop "Calling associate local memory.")
               (associate-local-memory
                 (hw:dpb-unboxed physical hw:%%cluster-physical-address-block cluster-in-block)
                 (hw:dpb-unboxed virtual  hw:%%cluster-physical-address-block cluster-in-block)
                 $$map-status-direct-mapped)
;              (trap::illop "Returned from associate local memory.")
               ))

           (associate-memory (virtual physical)
;            (trap::illop "associate memory")
             (cond ((= physical vinc:*blocks-of-physical-memory*) nil)
                   ((physical-block-exists? physical physical-memory-layout)
                    (associate-block virtual physical)
                    (associate-memory (1+ virtual) (1+ physical)))
                   (t (associate-memory virtual (1+ physical))))))

;    (trap::illop "Entered direct map.")
    (zap-map 0)
    (associate-memory 0 0)))

(defun flush-direct-map ()
  (dotimes (entry *number-of-map-entries*)
    (let ((status (read-map-status entry)))
      (when (= status $$map-status-direct-mapped)
        (free-virtual-cluster entry)))))

(defun fresh-cluster? (cluster)
  (cluster-is-fresh? (read-map cluster)))

(defun dump-map (address)
  (dotimes (i *number-of-map-entries*)
    (hw:write-md-unboxed (read-map i))
    (hw:vma-start-write-no-gc-trap-unboxed (+ address i))))

(defun load-map (address)
  ;; It is actually only interesting to load up the volatilities
  ;; and the freshness because the other parts of the map entries
  ;; will be changed later.
  (dotimes (i *number-of-map-entries*)
    (hw:vma-start-read-no-transport-vma-unboxed-md-unboxed (+ address i))
    (let ((initial-data (hw:read-md)))
      (modify-map i
        #'(lambda (map-data)
            (vinc::dpb-multiple-unboxed
              (map-cluster-volatility initial-data) hw:%%map-volatility
              (map-cluster-fresh-bit  initial-data) %%map-fresh-cluster
              map-data))))))
