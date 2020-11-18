;;; -*- Mode:LISP; Package:NUBUS-STUFF; Readtable:CL; Base:10 -*-

(export
  '(
    acknowledge-debugger-trap
    cause-debugger-trap
;    make-external-bus-device ;; $$$ Removed definition <17-Nov-88 wkf>
    %bus-read-24
    %bus-read
    %bus-write
    ))

(defconstant *k-io-regs-cluster* 3.)

(defconstant %%nubus-cluster-slot-field (byte 8. 12.))

(defun slot-and-address->nubus-cluster (slot address)
  (hw:dpb slot %%nubus-cluster-slot-field (cluster-number (hw:ldb address (byte 24. 2.) 0))))

(defun map-in-k-io-cluster ()
  (map:associate-nubus-memory
    (slot-and-address->nubus-cluster
      (hw:ldb (hw:read-memory-status) hw:%%memory-status-nubus-slot-id #xF0)    ;slot space
      hw:*slot-offset-nubus-interrupts*)
    *k-io-regs-cluster*
    map:$$map-status-normal))

;(defun setup-temporary-nubus-map (slot address)
;  (map:associate-nubus-memory (slot-and-address->nubus-cluster slot address) *temporary-map-entry*
;                             map:$$map-status-direct-mapped))

;(defun nubus-read-32 (slot address)
;  (trap::without-traps
;    #'(lambda ()
;       (setup-temporary-nubus-map slot address)
;       (hw:vma-start-read-no-transport-vma-unboxed-md-unboxed
;         (hw:ldb address (byte 10. 2.) (cluster->address *temporary-map-entry*)))))
;    (hw:read-md))

;(defun nubus-write-32 (slot address data)
;  (trap::without-traps
;    #'(lambda ()
;       (setup-temporary-nubus-map slot address)
;       (hw:write-md-unboxed data)
;       (hw:vma-start-write-no-gc-trap-unboxed
;         (hw:ldb address (byte 10. 2.) (cluster->address *temporary-map-entry*)))))
;  (hw:read-md))

(defsubst cause-nubus-interrupt (n)
  (hw:write-md-unboxed 1)
  (hw:vma-start-write-no-gc-trap-unboxed (+ (cluster->address *k-io-regs-cluster*) n)))

(defsubst acknowledge-nubus-interrupt (n)
  (hw:write-md-unboxed 0)
  (hw:vma-start-write-no-gc-trap-unboxed (+ (cluster->address *k-io-regs-cluster*) n)))

(defmacro define-nubus-interrupt (n cause acknowledge)
  `(PROGN (DEFSUBST ,cause       () (CAUSE-NUBUS-INTERRUPT ,n))
          (DEFSUBST ,acknowledge () (ACKNOWLEDGE-NUBUS-INTERRUPT ,n))))

(define-nubus-interrupt hw:*cluster-offset-nubus-interrupt-7* cause-debugger-trap acknowledge-debugger-trap)

;;; $$$ Removed since not used. <17-Nov-88 wkf>
;;; @@@ This function is currently never called <17-Nov-88 wkf>
#+Removed
 (defun make-external-bus-device (external-bus-byte-addr size-in-bytes)
  (let* ((size-in-words          (ash (+ size-in-bytes 3) -2))
         (reg-size               (area-data:poor-mans-ceiling size-in-words vinc:*qs-in-quantum*))
         (external-bus-byte-addr (unboxed-32 external-bus-byte-addr))
         reg)
    (setq reg (region-data:make-region
                reg-size
                (region-bits:encode-region-bits
                  region-bits:$$region-fixed
                  region-bits:$$region-new-space
                  region-bits:$$region-space-unboxed
                  region-bits:$$region-read-write
                  region-bits:$$scavenge-disabled
                  region-bits:$$region-external-bus
                  0.)
                0.))
    ;; Setup the map in such a way that no physical memory is ever allocated to the region.
    (do ((virtual-cluster (vinc:quantum->cluster reg) (1+ virtual-cluster))
         (external-bus-cluster (hw:ldb external-bus-byte-addr (byte 20. 12.) 0) (1+ external-bus-cluster))
         (i 0 (1+ i))
         ;; $$$ Changed *qs-in-cluster* to vinc:*qs-in-cluster* <17-Nov-88 wkf>
         (max-number-of-clusters (area-data:poor-mans-ceiling size-in-words vinc:*qs-in-cluster*)))
        ((= i max-number-of-clusters))
      (map:associate-cluster hw:$$map-non-local external-bus-cluster virtual-cluster map:$$map-status-normal))
    ;; Now advance region free pointer.
    (region-data:advance-free-pointer reg size-in-words)
    ;; Now make a displaced array to return.
    (array:make-array size-in-words
                      :element-type '(li:unsigned-byte 32.)
                      :displaced-to (hw:dpb-boxed reg (byte 12. 14.)
                                            (hw:unboxed-constant #.(lisp:ash vinc:$$dtp-unboxed-locative 26.))))
    )
  )

(defun get-virtual-array-size (element-type size-in-words size-in-bytes)
  (case (array:array-type-from-element-type element-type)
    (0 size-in-words)
    (1 (new-math:multiply-fixnum size-in-bytes 8))
    ((2 3) (new-math:multiply-fixnum size-in-bytes 4))
    ((4 5) (new-math:multiply-fixnum size-in-bytes 2))
    ((6 7 12) size-in-bytes)
    ((8 9) (ash (1+ size-in-bytes) -1))
    ((10. 11.) size-in-words)
    )
  )

(defun make-external-structure (external-bus-byte-addr
                                size-in-bytes
                                element-type
                                named-structure-symbol
                                leader-length
                                )
  (let* ((size-in-words (ash (+ size-in-bytes 3) -2))
         (reg-size (area-data:poor-mans-ceiling size-in-words vinc:*qs-in-quantum*))
         (external-bus-byte-addr (unboxed-32 external-bus-byte-addr))
         (array-size (get-virtual-array-size element-type size-in-words size-in-bytes))
         reg)
    (setq reg (region-data:make-region
                reg-size
                (region-bits:encode-region-bits
                  region-bits:$$region-fixed
                  region-bits:$$region-new-space
                  region-bits:$$region-space-unboxed
                  region-bits:$$region-read-write
                  region-bits:$$scavenge-disabled
                  region-bits:$$region-external-bus
                  0.)
                0.))
    ;; Setup the map in such a way that no physical memory is ever allocated to the region.
    (do ((virtual-cluster (vinc:quantum->cluster reg) (1+ virtual-cluster))
         (external-bus-cluster (hw:ldb external-bus-byte-addr (byte 20. 12.) 0) (1+ external-bus-cluster))
         (i 0 (1+ i))
         (max-number-of-clusters (area-data:poor-mans-ceiling size-in-words vinc:*qs-in-cluster*)))
        ((= i max-number-of-clusters))
      ;; map only those clusters that we are going to use. Otherwise physical address might end up in
      ;; a different board.
      (map:associate-cluster hw:$$map-non-local external-bus-cluster virtual-cluster map:$$map-status-normal))
    ;; Now advance region free pointer.
    (region-data:advance-free-pointer reg size-in-words)
    ;; Now make a displaced array to return.
    (let ((loc (hw:dpb-boxed reg (byte 12. 14.)
                             (hw:unboxed-constant #.(lisp:ash vinc:$$dtp-unboxed-locative 26.)))))
      (values (array:zl-make-array array-size
                             :element-type element-type
                             :leader-length leader-length
                             :named-structure-symbol named-structure-symbol
                             :displaced-to loc)
              loc))
    )
  )

;;; $$$ Removed since not used. <17-Nov-88 wkf>
;;; @@@ This function is currently never called <17-Nov-88 wkf>
#+Removed
 (defun make-screen-bit-array (external-bus-byte-addr size-in-bytes &optional (screen-dims '(1024. 1024.)))
  (let* ((size-in-words (ash (+ size-in-bytes 3) -2))
         (reg-size (area-data:poor-mans-ceiling size-in-words vinc:*qs-in-quantum*))
         (external-bus-byte-addr (unboxed-32 external-bus-byte-addr))
         reg)
    (setq reg (region-data:make-region
                reg-size
                (region-bits:encode-region-bits
                  region-bits:$$region-fixed
                  region-bits:$$region-new-space
                  region-bits:$$region-space-unboxed
                  region-bits:$$region-read-write
                  region-bits:$$scavenge-disabled
                  region-bits:$$region-external-bus
                  0.)
                0.))
    ;; Setup the map in such a way that no physical memory is ever allocated to the region.
    (do ((virtual-cluster (vinc:quantum->cluster reg) (1+ virtual-cluster))
         (external-bus-cluster (hw:ldb external-bus-byte-addr (byte 20. 12.) 0) (1+ external-bus-cluster))
         (i 0 (1+ i))
         ;; $$$ Changed *qs-in-cluster* to vinc:*qs-in-cluster* <17-Nov-88 wkf>
         (max-number-of-clusters (area-data:poor-mans-ceiling size-in-words vinc:*qs-in-cluster*)))
        ((= i max-number-of-clusters))
      (map:associate-cluster hw:$$map-non-local external-bus-cluster virtual-cluster map:$$map-status-normal))
    ;; Now advance region free pointer.
    (region-data:advance-free-pointer reg size-in-words)
    ;; Now make a displaced array to return.
    (array:make-array (hw:dpb size-in-words (byte 21. 5.) 0)
                      :element-type '(li:unsigned-byte 1.)
                      :displaced-to (hw:dpb-boxed reg (byte 12. 14.)
                                            (hw:unboxed-constant #.(lisp:ash vinc:$$dtp-unboxed-locative 26.))))
    )
  )


(defun make-disk-io-buffer (clusters)
  ;; makes and return an area where disk transfers will go when reading.
  (let* ((size-in-words (hw:dpb clusters vinc:%%cluster-number 0.))
         (reg-size (area-data:poor-mans-ceiling size-in-words vinc:*qs-in-quantum*))
         (reg (region-data:make-region
                reg-size
                (region-bits:encode-region-bits
                  region-bits:$$region-fixed
                  region-bits:$$region-new-space
                  region-bits:$$region-space-unboxed
                  region-bits:$$region-read-write
                  region-bits:$$scavenge-disabled
                  region-bits:$$region-internal-memory
                  0.)
                0.))
         (ptr (hw:dpb-boxed reg (byte 12. 14.) (hw:unboxed-constant #.(lisp:ash vinc:$$dtp-unboxed-locative 26.)))))
    ;; Now advance region free pointer.
    (region-data:advance-free-pointer reg size-in-words)
    ;; Now make a displaced array to return.
    (dotimes (i clusters)
      (map-fault:swapin-cluster-and-wire-it (+ reg i)))
    (values (array:make-array size-in-words
                      :element-type '(li:unsigned-byte 32.)
                      :displaced-to ptr) ptr)
    )
  )

(defconstant *bus-access-cluster* 4)
;;; cluster 4. is reserved for special external bus access routines.

(defconstant *bus-access-address* #.(lisp:ash *bus-access-cluster* 10.))


(defun unboxed-32 (data)
  (cond ((not (hw:32logbitp 0 (k2:boxed-bit data)))
         data)
        ((vinc:%fixnump data)
         (hw:32-sign-extend data))
        ((vinc:%bignump data)
         (hw:vma-start-read-no-transport-vma-unboxed-md-unboxed (hw:32-1+ data))
         (hw:read-md))
        (t (li:error "non integer Data"))
        )
  )


(defun %bus-write (unboxed-byte-address unboxed-data &optional ignore-bus-errors)
  ignore-bus-errors
  (hw:read-md)
  (trap:without-traps
    #'(lambda ()
        (hw:write-vma-unboxed *bus-access-address*)
        (hw:nop)
        (hw:nop)
        (hw:nop)
        (hw:write-map (hw:dpb 15. (byte 12. 0.) unboxed-byte-address))
        (hw:write-md-unboxed unboxed-data)
        (hw:vma-start-write-no-gc-trap-unboxed (hw:ldb unboxed-byte-address (byte 10. 2) *bus-access-address*))
        (hw:nop)
        (let ((result (hw:read-md)))
          (hw:nop)
          (hw:write-map (hw:unboxed-constant 0))
          unboxed-data))))

(defun %bus-read (unboxed-byte-address &optional ignore-bus-errors)
  ignore-bus-errors
  (hw:read-md)
  (trap:without-traps
    #'(lambda ()
        (hw:write-vma-unboxed *bus-access-address*)
        (hw:nop)
        (hw:nop)
        (hw:nop)
        (hw:write-map (hw:dpb 15. (byte 12. 0.) unboxed-byte-address))
        (hw:vma-start-read-no-transport-vma-unboxed-md-unboxed
          (hw:ldb unboxed-byte-address (byte 10. 2) *bus-access-address*))
        (let ((result (hw:read-md)))
          (hw:nop)
          (hw:write-map (hw:unboxed-constant 0))
          result))))

(defun %local-memory-read (unboxed-byte-address)
  "Read our internal memory."
  (hw:read-md)
  (trap:without-traps
    #'(lambda ()
        (hw:write-vma-unboxed *bus-access-address*)
        (hw:nop)
        (hw:nop)
        (hw:nop)
        ;; set up local memory with everything enabled.
        (hw:write-map (hw:dpb #X8f (byte 12. 0.) unboxed-byte-address))
        (hw:vma-start-read-no-transport-vma-unboxed-md-unboxed
          (hw:ldb unboxed-byte-address (byte 10. 2) *bus-access-address*))
        (let ((result (hw:read-md)))
          (hw:nop)
          (hw:write-map (hw:unboxed-constant 0))
          result))))

(defun %local-memory-write (unboxed-byte-address unboxed-data &optional ignore-bus-errors)
  ignore-bus-errors
  (hw:read-md)
  (trap:without-traps
    #'(lambda ()
        (hw:write-vma-unboxed *bus-access-address*)
        (hw:nop)
        (hw:nop)
        (hw:nop)
        (hw:write-map (hw:dpb #x8f (byte 12. 0.) unboxed-byte-address))
        (hw:write-md-unboxed unboxed-data)
        (hw:vma-start-write-no-gc-trap-unboxed (hw:ldb unboxed-byte-address (byte 10. 2) *bus-access-address*))
        (hw:nop)
        (let ((result (hw:read-md)))
          (hw:nop)
          (hw:write-map (hw:unboxed-constant 0))
          unboxed-data))))

(defun %cluster-copy (from-unboxed-byte-address from-local-mem? to-unboxed-byte-address to-local-mem?)
  (let ((from-map (hw:dpb-unboxed (if from-local-mem? #x8f #x0f) (byte 12. 0) from-unboxed-byte-address))
        (to-map   (hw:dpb-unboxed (if to-local-mem?   #x8f #x0f) (byte 12. 0) to-unboxed-byte-address)))
    (hw:read-md)
    (trap:without-traps
      #'(lambda ()
          (hw:write-vma-unboxed *bus-access-address*)
          (dotimes (i 1024.)
            (hw:read-md)
            (hw:nop)
            (hw:nop)
            (hw:write-map from-map)
            (hw:vma-start-read-no-transport-vma-unboxed-md-unboxed
              (hw:dpb-unboxed i (byte 10. 0) *bus-access-address*))
            (hw:write-md-unboxed (hw:read-md))
            (hw:nop)
            (hw:nop)
            (hw:write-map to-map)
            (hw:nop)
            (hw:vma-start-write-no-gc-trap-unboxed (hw:read-vma)))
          (hw:read-md)
          (hw:nop)
          (hw:nop)
          (hw:write-map (hw:unboxed-constant 0))
          nil))))

(defun %cluster-fill (unboxed-byte-address local-mem? unboxed-fill-value)
  (hw:read-md)
  (trap:without-traps
    #'(lambda ()
        (hw:write-vma-unboxed *bus-access-address*)
        (hw:write-md-unboxed unboxed-fill-value)
        (hw:nop)
        (hw:nop)
        (hw:write-map (hw:dpb-unboxed (if local-mem? #x8f #x0f) (byte 12. 0) unboxed-byte-address))
        (dotimes (i 1024.)
          (hw:vma-start-write-no-gc-trap-unboxed
            (hw:dpb-unboxed i (byte 10. 0) (hw:read-vma))))
        (hw:read-md)
        (hw:nop)
        (hw:nop)
        (hw:write-map (hw:unboxed-constant 0))
        nil)))


(defun %bus-byte-read (unboxed-byte-address &optional ignore-bus-errors)
  ignore-bus-errors
  (hw:read-md)
  (trap:without-traps
    #'(lambda ()
        (hw:write-vma-unboxed *bus-access-address*)
        (hw:write-memory-control (vinc:dpb-multiple-unboxed
                                   unboxed-byte-address (byte 2 9.)
                                   1 (byte 1 8.)
                                   (hw:read-memory-control)))
        (hw:nop)
        (hw:nop)
        (hw:write-map (hw:dpb 15. (byte 12. 0.) unboxed-byte-address))
        (hw:vma-start-read-no-transport-vma-unboxed-md-unboxed
          (hw:ldb unboxed-byte-address (byte 10. 2) *bus-access-address*))
        (let ((result (hw:ldb (hw:read-md) (byte 8 (hw:dpb unboxed-byte-address (byte 2 3) 0)) 0)))
          (hw:nop)
          (hw:nop)
          (hw:write-map (hw:unboxed-constant 0))
          (hw:write-memory-control (hw:dpb-unboxed 0 (byte 3 8.)
                                                   (hw:read-memory-control)))
          result))))

(defun %bus-byte-write (unboxed-byte-address unboxed-data &optional ignore-bus-errors)
  ignore-bus-errors
  (hw:read-md)
  (trap:without-traps
    #'(lambda ()
        (hw:write-vma-unboxed *bus-access-address*)
        (hw:write-memory-control (vinc:dpb-multiple-unboxed
                                   unboxed-byte-address (byte 2 9.)
                                   1 (byte 1 8.)
                                   (hw:read-memory-control)))
        (hw:nop)
        (hw:nop)
        (hw:write-map (hw:dpb 15. (byte 12. 0.) unboxed-byte-address))
        (hw:write-md-unboxed (hw:dpb unboxed-data (byte 8. (hw:dpb unboxed-byte-address (byte 2 3) 0))
                                     (hw:unboxed-constant 0)))
        (hw:vma-start-write-no-gc-trap-unboxed (hw:ldb unboxed-byte-address (byte 10. 2) *bus-access-address*))
        (hw:nop)
        (let ((result (hw:read-md)))
          (hw:nop)
          (hw:write-map (hw:unboxed-constant 0))
          (hw:write-memory-control (hw:dpb-unboxed 0 (byte 3 8.)
                                                   (hw:read-memory-control)))
          unboxed-data))))

(defun %bus-read-16 (unboxed-byte-address &optional ignore-bus-errors)
  ignore-bus-errors
  (hw:read-md)
  (trap:without-traps
    #'(lambda ()
        (hw:write-vma-unboxed *bus-access-address*)
        (hw:write-memory-control (vinc:dpb-multiple-unboxed
                                   (hw:ldb unboxed-byte-address (byte 1 1) 0) (byte 1 9.)
                                   1 (byte 1 10.)
                                   0 (byte 1 8.)
                                   (hw:read-memory-control)))
        (hw:nop)
        (hw:nop)
        (hw:write-map (hw:dpb 15. (byte 12. 0.) unboxed-byte-address))
        (hw:vma-start-read-no-transport-vma-unboxed-md-unboxed
          (hw:ldb unboxed-byte-address (byte 10. 2) *bus-access-address*))
        (let ((result (hw:ldb (hw:read-md) (byte 16. (hw:dpb (hw:ldb unboxed-byte-address (byte 1 1) 0) (byte 1 4) 0)) 0)))
          (hw:nop)
          (hw:write-map (hw:unboxed-constant 0))
          (hw:write-memory-control (hw:dpb-unboxed 0 (byte 3 8.)
                                                   (hw:read-memory-control)))
          result))))

(defun %bus-write-16 (unboxed-byte-address unboxed-data &optional ignore-bus-errors)
  ignore-bus-errors
  (hw:read-md)
  (trap:without-traps
    #'(lambda ()
        (hw:write-vma-unboxed *bus-access-address*)
        (hw:write-memory-control (vinc:dpb-multiple-unboxed
                                   (hw:ldb unboxed-byte-address (byte 1 1) 0) (byte 1 9.)
                                   0 (byte 1 8.)
                                   1 (byte 1 10.)
                                   (hw:read-memory-control)))
        (hw:nop)
        (hw:nop)
        (hw:write-map (hw:dpb 15. (byte 12. 0.) unboxed-byte-address))
        (hw:write-md-unboxed (hw:dpb unboxed-data
                                     (byte 16. (hw:dpb (hw:ldb unboxed-byte-address (byte 1 1) 0) (byte 1 4) 0))
                                     (hw:unboxed-constant 0)))
        (hw:vma-start-write-no-gc-trap-unboxed (hw:ldb unboxed-byte-address (byte 10. 2) *bus-access-address*))
        (hw:nop)
        (let ((result (hw:read-md)))
          (hw:nop)
          (hw:write-map (hw:unboxed-constant 0))
          (hw:write-memory-control (hw:dpb-unboxed 0 (byte 3 8.)
                                                   (hw:read-memory-control)))
          unboxed-data))))

;;;
;;; Reading and writing to a slot with an offset.
;;; In the next 4 functions slot is of the form #xF<slot number>
;;;

(defun %slot-bus-read (slot address)
  (%bus-read (hw:dpb slot (byte 8. 24.) (unboxed-32 address)))
  )

(defun %slot-bus-byte-write (slot address data)
  (%bus-write (hw:dpb slot (byte 8. 24.) (unboxed-32 address)) (unboxed-32 data))
  )

(defun %slot-bus-byte-read (slot address)
  (%bus-byte-read (hw:dpb slot (byte 8. 24.) (unboxed-32 address)))
  )

(defun %slot-bus-write (slot address data)
  (%bus-byte-write (hw:dpb slot (byte 8. 24.) (unboxed-32 address)) (unboxed-32 data))
  )

(defconstant *sdu-slot* #xff)

(defconstant *byte-memory-offset-in-sdu-mode* #o20000)

(defun multibus-real-address (multibus-address)
  (+ multibus-address *byte-memory-offset-in-sdu-mode*))

(defun %multibus-byte-write (address unboxed-data &optional (offset-p nil) ignore-bus-errors)
  (%bus-byte-write (hw:dpb-unboxed *sdu-slot* (byte 8. 24.)
                                   (hw:24+ (unboxed-32 address)
                                           (if offset-p
                                               *byte-memory-offset-in-sdu-mode*
                                             0)))
                   (unboxed-32 unboxed-data) ignore-bus-errors)
  )

(defun %multibus-byte-read (address &optional (offset-p nil) ignore-bus-errors)
  (%bus-byte-read (hw:dpb-unboxed *sdu-slot* (byte 8. 24.)
                                   (hw:24+ (unboxed-32 address)
                                           (if offset-p *byte-memory-offset-in-sdu-mode*
                                             0))))
  )

(defun %multibus-IO-read-8 (&optional (port-addr 0) ignore-bus-errors)
  ignore-bus-errors
  (%bus-byte-read (hw:dpb-unboxed *sdu-slot* (byte 8. 24.)
                                   (hw:24+ #o4000000 (unboxed-32 (ash port-addr 2)))))
  )

(defun %multibus-IO-write-8 (&optional (port-addr 0) (data 0) ignore-bus-errors)
  (%bus-byte-write (hw:dpb-unboxed *sdu-slot* (byte 8. 24.)
                                   (hw:24+ #o4000000 (unboxed-32 (ash port-addr 2))))
                   (unboxed-32 data))
  )

(defun %multibus-write-32 (Address unboxed-data)
  (%bus-write (hw:dpb-unboxed *sdu-slot* (byte 8. 24.) address) (unboxed-32 unboxed-data))
  )

(defun %multibus-read-32 (address)
  (%bus-read (hw:dpb-unboxed *sdu-slot* (byte 8. 24.) (unboxed-32 address)))
  )

;;;
;;; Unsafe bus read and write. frobs with -unsafe tagged on end bypass slot safety test in 2x2 mode.
;;;  only a few of these exist now.
;;;

(defun %bus-read-unsafe (byte-address &optional ignore-bus-errors)
  (setq byte-address (unboxed-32 byte-address))
  (%bus-quad-slot-read-unsafe
        (hw:ldb byte-address (byte 8 24.) 0)
        (hw:ldb byte-address (byte 24. 0) (hw:unboxed-constant 0))
        ignore-bus-errors)
  )

(defun %bus-read-byte-unsafe (byte-address &optional ignore-bus-errors)
  (%bus-byte-read (unboxed-32 byte-address) ignore-bus-errors)
  )

(defun %bus-quad-slot-read-unsafe (quad-slot byte-address &optional ignore-bus-errors)  ; byte-mode
  (%slot-bus-read quad-slot byte-address)       ; byte-mode ignore-bus-errors
  )

(defun %bus-write-unsafe (byte-address data &optional ignore-bus-errors)
  (setq byte-address (unboxed-32 byte-address))
  (%bus-quad-slot-write-unsafe
        (hw:ldb byte-address (byte 8 24.) 0)
        (hw:ldb byte-address (byte 24. 0) (hw:unboxed-constant 0))
        (unboxed-32 data) ignore-bus-errors)
  )

(defun %bus-write-byte-unsafe (byte-address data &optional ignore-bus-errors)
  (%bus-byte-write (unboxed-32 byte-address) (unboxed-32 data) ignore-bus-errors)
  )

(defun %bus-quad-slot-write-unsafe (quad-slot byte-address data &optional ignore-bus-errors byte-mode)
  (%slot-bus-write quad-slot byte-address data) ;ignore-bus-errors byte-mode
  )


;;;
;;;    Mapping registers
;;;

(defun map-multibus-address (nubus-address)
  "return nubus-address, unless it points to the multibus, and is mapped to the nubus.
in that case, follow the mapping, and return that address"
  (cond ((not (= (hw:ldb (unboxed-32 nubus-address) (byte 8 24.) 0) #xff))
         nubus-address)
        (t
         (let* ((unboxed-nubus-address (unboxed-32 nubus-address))
                (map-to (read-multibus-mapping-register (hw:ldb unboxed-nubus-address (byte 10. 10.) 0))))
           (cond ((li:ldb-test (byte 1 23.) map-to)     ; check valid bit
                  (hw:dpb (hw:ldb map-to (byte 22. 0) 0)
                          (byte 22. 10.)
                          (hw:ldb unboxed-nubus-address (byte 10. 0) 0)))
                 (t
                  nubus-address)))))
  )


(defun read-multibus-mapping-register (page-number
                                       &aux (multibus-mapping-reg-adr (hw:24+ (unboxed-32 (ash page-number 2)) #x18000)))
  (unless (< page-number #o2000)
    (li:error "~S is too big for a page number" page-number))
  (hw:dpb-multiple
    (%multibus-byte-read multibus-mapping-reg-adr) (byte 8. 0)
    (%multibus-byte-read (1+ multibus-mapping-reg-adr)) (byte 8. 8.)
    (%multibus-byte-read (+ multibus-mapping-reg-adr 2)) (byte 8. 16.)
    0)
  )

(defun write-multibus-mapping-register (page-number data
                                        &aux (multibus-mapping-reg-adr (hw:24+ (unboxed-32 (ash page-number 2)) #x18000)))
  (unless (< page-number #o2000)
    (li:error "~S is too big for a page number" page-number))
  (setq data (unboxed-32 data))
  (%multibus-byte-write multibus-mapping-reg-adr (hw:ldb data (byte 8. 0) 0))
  (%multibus-byte-write (1+ multibus-mapping-reg-adr) (hw:ldb data (byte 8. 8.) 0))
  (%multibus-byte-write (+ multibus-mapping-reg-adr 2) (hw:ldb data (byte 8. 16.) 0))
  data
  )

(defun print-multibus-mapping-register (page-number)
  (cond ((< page-number #o2000)
         (let ((data (READ-MULTIBUS-MAPPING-REGISTER PAGE-NUMBER)))
           (li:FORMAT T "~%MAPPING REGISTER ~S, ~S, Valid ~s, quad-slot ~s, page number ~s, adr #x~16r"
                   page-number
                   data
                   (hw:ldb data (byte 1 23.) 0)
                   (hw:ldb data (byte 8 14.) 0)
                   (hw:ldb data (byte 14. 0) 0)
                   (ash (hw:ldb data (byte 22. 0) 0) 10.))))
        (t (li:format t "~%Mapping register ~s too big" page-number))))

;(defun print-lambda-mapping-registers ()
;  (li:format t "~%INTMAP-MULTIBUS-MAP:")
;  (print-multibus-mapping-register (send *proc* :intmap-multibus-map))
;  (li:format t "~%Mapping register block:")
;  (do ((reg (send *proc* :base-multibus-mapping-register) (1+ reg))
;       (lim (send *proc* :number-of-multibus-maps))
;       (c 0 (1+ c)))
;      ((= c lim))
;    (print-multibus-mapping-register reg)))

#||||
(defun mapping-registers-used (&optional (from #o700))
  (do ((to from (1+ to)))
      ((zerop (%multibus-byte-read (+ #x18000 (ash to 2))))
;       (li:format t "~&~o-~o (= ~o~:* ~d.)" from to (- to from))
       )))


(defun unmap-multibus-mapping-registers-pointing-to-slot (slot)
  (dotimes (reg 1024.)
    (let ((mapped-to (read-multibus-mapping-register reg)))
      (cond ((= (ash mapped-to -14.) (hw:dpb slot (byte 4. 0.) #x2f0))
;            (li:format t "~&reg ~o mapped to #x~16r" reg (ash mapped-to 10.))
             (write-multibus-mapping-register reg 0))))))
||||#
