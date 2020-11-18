; -*- Mode:LISP; Package:COLD; Base:8; Lowercase:T; Readtable:ZL -*-
;       ** (c) Copyright 1980 Massachusetts Institute of Technology **

; Utilities for cold-load generator

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; To compile this:                           ;;;
;;;   (1) Load the old QFASL of it             ;;;
;;;   (2) Run (LOAD-PARAMETERS)                ;;;
;;;   (3) Now you may compile it               ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Little variables that have to do with the word format
(defvar big-fixnum)
(defvar little-fixnum)
(defvar q-typed-pointer-mask)   ;Due to deficiencies in LDB and DPB
(defvar q-pointer-mask)

;;; Disk unit number

(defvar *unit-number*)

;;; The virtual memory

(defvar n-vmem-pages 16.)

;(i,0) is virtual page number, (i,1) is rqb
;Both slots are nil if unused
(defvar vmem-pages (make-array (list n-vmem-pages 2)))

(defvar vmem-page-reuse-pointer)

(defvar vmem-part-base)
(defvar vmem-part-size)
(defvar vmem-highest-address nil)

(defun vmem-initialize (part-name)
  (setq vmem-page-reuse-pointer 0)
  (multiple-value (vmem-part-base vmem-part-size) (sys:find-disk-partition part-name))
  (or vmem-part-base (ferror "~S partition not found on disk unit 0" part-name))
  (dotimes (i n-vmem-pages)
    (aset nil vmem-pages i 0)
    (aset nil vmem-pages i 1)))

;Write out all the buffered pages and return the rqb's
(defun vmem-finish (&aux rqb)
  (dotimes (i n-vmem-pages)
    (cond ((setq rqb (aref vmem-pages i 1))
           (vmem-disk-io rqb (aref vmem-pages i 0) t)
           (sys:return-disk-rqb rqb)
           (aset nil vmem-pages i 1)))))

(defun vmem-disk-io (rqb vpn writep)
  (and (or (minusp vpn) ( vpn vmem-part-size))
       (ferror "Disk I//O outside of partition"))
  (funcall (if writep #'sys:disk-write #'sys:disk-read) rqb *unit-number* (+ vpn vmem-part-base)))

;Given address returns art-16b array containing that page.  With second arg of nil
;initializes to dtp-trap instead of reading in from disk.
(defun vmem-find-page (address &optional (get-from-disk-p t))
  (if (> (logand q-pointer-mask address) vmem-highest-address)
      (ferror "vmem-highest-address exceeded"))
  (do ((i 0 (1+ i))
       (vpn (truncate ;(ldb sym:%%q-pointer address)
                      (logand q-pointer-mask address)
                      sym:page-size))
       (rqb) (buf) (tem))
      (( i n-vmem-pages)
       (setq i vmem-page-reuse-pointer)
       (setq vmem-page-reuse-pointer (\ (1+ i) n-vmem-pages))
       (cond ((setq rqb (aref vmem-pages i 1))
              (vmem-disk-io rqb (aref vmem-pages i 0) t))       ;Swap this guy out
             (t (setq rqb (sys:get-disk-rqb))
                (aset rqb vmem-pages i 1)))
       (aset vpn vmem-pages i 0)
       (setq buf (sys:rqb-buffer rqb))
       (cond (get-from-disk-p
               (vmem-disk-io rqb vpn nil))
             (t (setq tem (dpb sym:dtp-trap sym:%%q-data-type (* vpn sym:page-size)))
                (do ((j 0 (1+ j))
                     (high (ldb #o2020 tem))
                     (low (ldb #o0020 tem)))
                    (( j sym:page-size))
                  (aset (+ low j) buf (+ j j))
                  (aset high buf (+ j j 1)))))
       buf)
    (cond ((eq (aref vmem-pages i 0) vpn)       ;Already swapped in
           (and (= vmem-page-reuse-pointer i)
                (setq vmem-page-reuse-pointer (\ (1+ i) n-vmem-pages)))
           (return (sys:rqb-buffer (aref vmem-pages i 1)))))))

(defun print-vmem-status ()
  (dotimes (i n-vmem-pages)
    (format t "~%Buffer index ~s holds virtual page ~s, rqb ~s"
            i
            (aref vmem-pages i 0)
            (aref vmem-pages i 1))))

(defun vread (address)
  (let ((buf (vmem-find-page address))
        (i (* 2 (\ address sym:page-size))))
    (dpb (aref buf (1+ i)) #o2020 (aref buf i))))

(defun vwrite (address value)
  (let ((buf (vmem-find-page address))
        (i (* 2 (\ address sym:page-size))))
    (aset (ldb #o0020 value) buf i)
    (aset (ldb #o2020 value) buf (1+ i))))

(defun vwrite-low (address value)
  (let ((buf (vmem-find-page address))
        (i (* 2 (\ address sym:page-size))))
    (aset value buf i)))

(defun vwrite-high (address value)
  (let ((buf (vmem-find-page address))
        (i (* 2 (\ address sym:page-size))))
    (aset value buf (1+ i))))

(defun vcontents (address)
  (logand q-typed-pointer-mask (vread address)))

(defun vcdr-code (address)
  (ldb sym:%%q-cdr-code (vread address)))

(defun vstore-contents (address value)
  (let ((buf (vmem-find-page address))
        (i (* 2 (\ address sym:page-size))))
    (aset (ldb #o0020 value) buf i)
    (aset (deposit-field (aref buf (1+ i))
                         (- sym:%%q-all-but-typed-pointer #o2000)
                         (ldb #o2020 value))
          buf (1+ i))))

(defun vstore-cdr-code (address value)
  (let ((buf (vmem-find-page address))
        (i (* 2 (\ address sym:page-size))))
    (aset (dpb value (- sym:%%q-cdr-code #o2000) (aref buf (1+ i))) buf (1+ i))))

(defun vwrite-cdr (address cdr-code value)
  (vwrite address (dpb cdr-code sym:%%q-cdr-code value)))

(defsubst vmake-pointer (data-type address)
  (dpb data-type sym:%%q-all-but-pointer address))

(defsubst vpointer (value)
  (logand q-pointer-mask value))

(defsubst vdata-type (value)
  (ldb sym:%%q-data-type value))

(defsubst vfix (value)
  (vmake-pointer sym:dtp-fix value))

(defun vlist (area &rest elements)
  (if (null elements)
      qnil
    (let ((value (vmake-pointer sym:dtp-list
                                (store-cdr-q area sym:cdr-next (car elements)))))
      (dolist (element (cdr elements))
        (store-cdr-q area sym:cdr-next element))
      (vstore-cdr-code (+ value (length elements) -1) sym:cdr-nil)
      value)))

(defun vlist* (area &rest elements)
  (cond ((null elements) (ferror "Too few arguments to VLIST*"))
        ((null (cdr elements)) (car elements))
        (t
         (let ((value (vmake-pointer sym:dtp-list
                                     (store-cdr-q area sym:cdr-next (car elements)))))
           (dolist (element (cdr elements))
             (store-cdr-q area sym:cdr-next element))
           (vstore-cdr-code (+ value (length elements) -1) sym:cdr-error)
           (vstore-cdr-code (+ value (length elements) -2) sym:cdr-normal)
           value))))

(defun vcar (location)
  (vcontents location))

(defun vcdr (location)
  (let ((cdr-code (vcdr-code location)))
    (select cdr-code
      (sym:cdr-nil qnil)
      (sym:cdr-next (1+ location))
      (sym:cdr-normal
       (vcontents (1+ location)))
      (sym:cdr-error (ferror "Location ~O contains CDR-ERROR." (vpointer location))))))

;If no property, returns a NIL in this machine.
;If property found, returns other-machine pointer to cell whose car is the property value.
(defun vget-location-or-nil (location property)
  (do ((cell (vcontents location) (vcdr (vcdr cell))))
      ((= cell qnil) qnil)
    (if (= (vcontents cell) property)
        (return (vcdr cell)))))

;;;; a bit of stuff for debugging

(defun vprint-q (q)
  (format t "#<~a ~a ~a>"
          (nth (ldb sym:%%q-cdr-code q) sym:q-cdr-codes)
          (nth (vdata-type q) sym:q-data-types)
          (vpointer q)))

(defvar vprinlength #o200)
(defvar vprinlevel  #o20)
(defvar vmax-stringlength #o200)

(defun vprint (typed-pointer &optional (vprinlevel vprinlevel))
  (let ((prinlength-count 0)
        (data-type (vdata-type typed-pointer))
        (pointer (vpointer typed-pointer)))
    (cond ((vatom? typed-pointer)
           (cond ((= data-type sym:dtp-symbol)
                  (vprint-string (vcontents pointer)))
                 ((= data-type sym:dtp-fix)
                  (prin1 pointer))
                 (t (vprint-bomb typed-pointer))))
          ((= data-type sym:dtp-array-pointer)
           (let ((header (vcontents pointer)))
             (cond ((= (mask-field-from-fixnum sym:%%array-type-field header)
                       sym:art-string)
                    (princ "/"")
                    (vprint-string typed-pointer)
                    (princ "/""))
                   (t (vprint-bomb typed-pointer)))))
          ((= data-type sym:dtp-list)
           (cond ((= vprinlevel 0)
                  (princ "#"))
                 (t
                  (princ "(")
                  (prog ((l typed-pointer)
                         (first-time t))
                     l  (cond ((> (setq prinlength-count (1+ prinlength-count))
                                  vprinlength)
                               (princ "...")
                               (return nil))
                              ((vatom? l)
                               (cond ((vnull? l)
                                      (princ ")")
                                      (return nil))
                                     (t
                                      (princ " . ")
                                      (vprint l (1- vprinlevel))
                                      (princ ")")
                                      (return nil)))))
                        (if (null first-time)
                            (princ " "))
                        (vprint (vcar l) (1- vprinlevel))
                        (setq l (vcdr l))
                        (setq first-time nil)
                        (go l)))))
          (t (vprint-bomb typed-pointer)))))

(defun vprint-bomb (typed-pointer)
  (vprint-q typed-pointer))

(defun vprint-string (string)
  (let* ((pointer (vpointer string))
         (header (vcontents pointer))
         (long-flag (ldb sym:%%array-long-length-flag header))
         (len (min vmax-stringlength
                   (if (zerop long-flag)
                       (ldb sym:%%array-index-length-if-short header)
                     (vpointer (1+ (vcontents pointer)))))))
    (dotimes (c len)
      (let ((wd (vread (+ pointer 1 long-flag (lsh c -2)))))
        (tyo (logand 377 (ash wd (minus (* 8 (logand c 3)))))))))
  )

(defun vatom? (typed-pointer)
  (let ((data-type (vdata-type typed-pointer)))
    (cond ((or (= data-type sym:dtp-symbol)
               (= data-type sym:dtp-fix)
               (= data-type sym:dtp-extended-number))
           t))))

(defun vnull? (typed-pointer)
  (= typed-pointer qnil))

(defun mask-field-from-fixnum (ppss word)
   (logand word (dpb -1 ppss 0)))

(defvar sym-package (pkg-find-package "COLD-SYMBOLS"))
(defvar misc-function-list)
(defvar misc-instruction-list)

;;; Set up the sym: package by loading the appropriate files
(defun load-parameters ()
  (load "SYS: COLD; QCOM LISP >" sym-package)
  (load "SYS: COLD; QDEFS LISP >" sym-package)
  (LOAD "SYS:COLD; SYSCONF LISP >" sym-package)
  (setq misc-function-list nil)
  (setq misc-instruction-list nil)
  (load "SYS: COLD; DEFMIC LISP >" sym-package)
  (dolist (l sym:system-constant-lists) ;Make declarations so can compile self
    (dolist (s (symeval l))
      (putprop s t 'special)))
  (setq big-fixnum (1- (ash 1 (1- sym:%%q-pointer)))
        little-fixnum (1- (- big-fixnum))
        q-typed-pointer-mask (1- (ash 1 sym:%%q-typed-pointer))
        q-pointer-mask (1- (ash 1 sym:%%q-pointer))))

;;; These have to be explicitly declared special because they only exist in
;;; the cold-load generator, and are not sent over.
(proclaim '(special sym:cold-load-area-sizes sym:cold-load-region-sizes
                    sym:scratch-pad-pointers sym:scratch-pad-parameters
                    sym:scratch-pad-parameter-offset sym:q-corresponding-variable-lists
                    sym:support-vector-contents sym:constants-page
                    sym:read-only-area-list sym:wired-area-list sym:pdl-buffer-area-list
                    sym:list-structured-areas sym:static-areas sym:unstructured-areas
                   ;sym:a-memory-array-locations
                    sym:new-array-index-order
                    sym:prin1 sym:base sym:ibase sym:*nopoint sym:for-cadr
                    sym:*print-base* sym:*read-base* sym:*print-radix*
                    sym:lambda-list-keywords))

;Put on QLVAL and QINTCMP properties
;Creates MISC-FUNCTION-LIST for STORE-MISC-LINK  (CALLED FROM STORE-MISC-U-ENTRY-LINKS)
; and MISC-INSTRUCTION-LIST for STORE-MICRO-CODE-SYMBOL-NAMES
(defun defmic (&quote name opcode arglist lisp-function-p &optional no-qintcmp)
  (prog (function-name instruction-name)
    (unless (variable-boundp sym:lambda-list-keywords)
      (setq sym:lambda-list-keywords lambda-list-keywords))
    (cond ((atom name)
           (setq function-name name instruction-name name))
          ((setq function-name (car name) instruction-name (cdr name))))
    (cond ((not no-qintcmp)
           (loop for x in arglist
                 when (memq x sym:lambda-list-keywords)
                   do (ferror "~S has ~S in its arglist which is not allowed"
                              name x))
           (putprop instruction-name (length arglist) 'sym:qintcmp)
           (or (eq function-name instruction-name)
               (putprop function-name (length arglist) 'sym:qintcmp)))
          (t ;The number of arguments is needed anyway for the cold-load generator
           (let ((nargs (length arglist))
                 (restarg (memq 'sym:&rest arglist)))
             (loop for x in arglist
                   when (memq x sym:lambda-list-keywords)
                     when (neq x 'sym:&rest)            ;&rest allowed if no-qintcmp
                       do (ferror "~S has ~S in its arglist which is not allowed"
                                  name x))
             ;; Note that if it says &rest, for a microcode function we don't really
             ;; want to get a list of args, we want to see the args on the stack, so
             ;; we translate this into the maximum possible number of optional arguments.
             ;; EVAL doesn't check the rest-arg bits for microcode entries anyway.
             (cond (restarg
                    (or (not lisp-function-p)
                        (= (length restarg) 2)
                        (ferror "~S has garbage ~S in its arglist" name restarg))
                    (setq nargs (cons (- nargs 2) #o77))))      ;(min . max)
             (putprop instruction-name nargs 'defmic-nargs-info)
             (or (eq function-name instruction-name)
                 (putprop function-name nargs 'defmic-nargs-info)))))
    (putprop instruction-name opcode 'sym:qlval)
    (setq misc-instruction-list (cons instruction-name misc-instruction-list))
    (and lisp-function-p
         (setq misc-function-list (cons name misc-function-list)))))

;;;; Basic area-processing and data-storing stuff

;;; Note that area names are always symbols in the sym: package

(defvar symbol-creation-trace-list nil)
(defvar qnil)
(defvar qtruth)
(defvar area-origins (make-array #o400))
(defvar area-alloc-pointers (make-array #o400))
(defvar area-alloc-bounds (make-array #o400))

;;; Region areas correspond with area areas in that there are the same number of regions
;;; as there are areas.  If this tenuous connection is broken, this will have to change.
(defvar area-corresponding-arrays
        'sym:(area-name region-origin region-length region-free-pointer
              region-gc-pointer region-bits area-region-list area-region-bits
              area-region-size
              region-area-map
              region-list-thread))

(defvar micro-code-entry-corresponding-arrays
        'sym:(micro-code-entry-area
              micro-code-entry-name-area
              micro-code-entry-args-info-area
              micro-code-entry-arglist-area
              micro-code-entry-max-pdl-usage))

(defvar areas-with-fill-pointers
        (append '(sym:micro-code-symbol-area
                   sym:micro-code-symbol-name-area
                   sym:support-entry-vector
                   sym:constants-area
                   sym:area-name)
                micro-code-entry-corresponding-arrays))

;;; areas in this list get art-q-list
(defvar list-referenced-areas areas-with-fill-pointers)

;;; areas in this list get art-q, all other areas get art-inum
(defvar array-referenced-areas 'sym:(system-communication-area
                                     region-moby-bits-array
                                     region-namespace-origin
                                     region-spare))

(defun create-areas (&aux high-loc the-region-bits)
  (do ((l sym:cold-load-area-sizes (cddr l)))   ;Area sizes in pages
      ((null l))
    (putprop (car l) (cadr l) 'area-size))
  (fillarray area-origins '(nil))
  ;; Set up the area origin and allocation tables
  (loop with quantum = sym:page-size
        for area in sym:initial-area-list
        for area-number from 0 by 1
        for loc = 0 then (+ loc size)
        as size = (* (ceiling (* (get-area-size area) sym:page-size) quantum)
                     quantum)
        when (eq area 'sym:init-list-area)      ;Last fixed area
          do (setq quantum sym:%address-space-quantum-size)
             (let ((foo (\ (+ loc size) quantum)))      ;Start next area on quantum boundary
               (or (zerop foo) (setq size (+ (- size foo) quantum))))
        do (aset loc area-origins area-number)
        when (eq area 'sym:extra-pdl-area)
          do (if (not (zerop (\ (+ loc size) 1_13.)))
                 (ferror "EXTRA-PDL-AREA does not end on a lvl-1 map boundary"))
        finally (setq high-loc loc))
  (copy-array-contents area-origins area-alloc-pointers)
  (copy-array-portion area-origins 1 400 area-alloc-bounds 0 400)
  (aset high-loc area-alloc-bounds (1- (length sym:initial-area-list)))
  (setq vmem-highest-address high-loc)
  ;; Fill various areas with default stuff
  (init-area-contents 'sym:area-region-size 40000)      ;these are ART-INUM now.
  (init-area-contents 'sym:region-allocation-status big-fixnum)
  (init-area-contents 'sym:region-area-map (vpointer -1))
  (init-area-contents 'sym:region-origin 0)  ;so good type in free region#'s
  (init-area-contents 'sym:region-length 0)  ;..
  (init-area-contents 'sym:region-free-pointer 0)
  (init-area-contents 'sym:region-gc-pointer 0)
  (init-area-contents 'sym:region-bits 0)  ;Suitable for free region
  (init-area-contents 'sym:area-region-bits 0)
  (init-area-contents 'sym:region-moby-bits-array (vfix 0))
  (init-area-contents 'sym:region-namespace-origin (vfix 0))
  (init-area-contents 'sym:region-spare (vfix 0))
  ;; Every page thinks it has volatile pointers.  This makes the first scavenge slow,
  ;; but guarantees consistency without any awkward initializations.
  (init-area-contents 'sym:virtual-page-volatility -1 3)
  (init-area-contents 'sym:virtual-page-data 0 0) ;at least for reproducability.
  ;; Crank up region size for certain big areas
  (do l sym:cold-load-region-sizes (cddr l) (null l)
      (vwrite (+ (get-area-origin 'sym:area-region-size) (get-area-number (car l)))
              (cadr l)))
  ;; Set up contents of certain initial areas
  (do ((i 0 (1+ i))
       (al sym:initial-area-list (cdr al))
       (fixed-p t))
      ((null al))
    (and (eq (car al) 'sym:working-storage-area) (setq fixed-p nil))
    (vwrite (+ (get-area-origin 'sym:area-region-list) i) i)
    (vwrite (+ (get-area-origin 'sym:region-list-thread) i) (+ i little-fixnum))
    (vwrite (+ (get-area-origin 'sym:region-bits) i)
            (setq the-region-bits
                  (+ (dpb (cond ((memq (car al) sym:read-only-area-list) #o1200)        ;ro
                                      ((memq (car al) sym:wired-area-list) #o1400)      ;rw
                                      ((memq (car al) sym:pdl-buffer-area-list)
                                       #o500)                   ;may be in pdl-buffer, no access.
                                      (t #o1300))                       ;rwf
                                sym:%%region-map-bits
                                0)
                     (dpb 1 sym:%%region-oldspace-meta-bit 0)
                     (dpb (if (eq (car al) 'sym:extra-pdl-area) 0 1)
                          sym:%%region-extra-pdl-meta-bit 0)
                     (dpb (if (memq (car al) sym:unstructured-areas)
                              sym:%region-representation-type-unstructured
                            sym:%region-representation-type-lisp)
                          sym:%%region-representation-type 0)
                     (dpb (cond ((eq (car al) 'sym:extra-pdl-area) sym:%region-space-extra-pdl)
                                (fixed-p sym:%region-space-fixed)
                                ((memq (car al) sym:static-areas) sym:%region-space-static)
                                (t sym:%region-space-new))
                          sym:%%region-space-type 0)
                     (dpb (let ((v (memq (car al) sym:cold-load-area-volatilities)))
                            (if v (cadr v) 0))
                          sym:%%region-volatility
                          0)
                     ;; Set up the scavenge enable.  Note!  The extra-pdl does not follow the
                     ;; prescribed protocol for header/body forward, and gets randomly reset.
                     ;; Fortunately it never points at anything.
                     (dpb (if fixed-p
                              (if (memq (car al) sym:unstructured-areas) 0 1)
                            (if (memq (car al) sym:static-areas) 1 0))
                          sym:%%region-scavenge-enable
                          0)
                     (dpb (if (memq (car al) '(sym:pdl-area sym:special-pdl-area)) 1 0)
                          sym:%%region-scavenge-carefully
                          0))))
    (vwrite (+ (get-area-origin 'sym:region-area-map) i) i)
    (vwrite (+ (get-area-origin 'sym:area-region-bits) i)
            the-region-bits)
    (vwrite (+ (get-area-origin 'sym:region-origin) i)
            (aref area-origins i))
    (vwrite (+ (get-area-origin 'sym:region-length) i)
            (- (aref area-alloc-bounds i) (aref area-origins i)))))

(defun get-area-number (area)
  (cond ((numberp area) area)
        ((find-position-in-list area sym:initial-area-list))    ;No symeval, might have changed.
        ((ferror "~S bad area-name" area))))

(defun get-area-origin (area)
  (aref area-origins (get-area-number area)))

(defun get-area-bound (area)
  (aref area-alloc-bounds (get-area-number area)))

(defun get-area-free-pointer (area)
  (aref area-alloc-pointers (get-area-number area)))

(defun allocate-block (area size &aux address high)
  (setq area (get-area-number area))
  (setq address (aref area-alloc-pointers area))
  (setq high (+ address size))
  (and (> high (aref area-alloc-bounds area))
       (ferror "~A area overflow" (nth area sym:initial-area-list)))
  (aset high area-alloc-pointers area)
  ;Page in all the fresh pages without really paging them in, thus initializing them
  (do ((vpn (ceiling address sym:page-size) (1+ vpn))
       (hpn (ceiling high sym:page-size)))
      (( vpn hpn))
    (vmem-find-page (* vpn sym:page-size) nil))
  address)

;;; In pages
(defun get-area-size (area)
  (check-arg area (memq area sym:initial-area-list) "an area-name")
  (cond ((get area 'area-size))
        (t 1)))

;;; Doesn't advance allocation pointer, i.e. sets it back to origin when done
(defun init-area-contents (area contents &optional (main-cdr-code sym:cdr-next))
  (let ((count (* sym:page-size (get-area-size area))))
    (setq area (get-area-number area))
    (do ((adr (allocate-block area count) (1+ adr))
         (n count (1- n)))
        ((zerop n)
         (store-nxtnil-cdr-code area)
         (aset (aref area-origins area) area-alloc-pointers area))
  ;this main-cdr-code kludge allows us to store -1 (all bits) for virtual-page-volatility.
      (vwrite-cdr adr main-cdr-code contents))))

(defvar store-halfwords-address)
(defvar store-halfwords-count)
(defvar store-halfwords-buffer)

(defun begin-store-halfwords (name-of-area n-words)
  (let* ((area-number (get-area-number name-of-area))
         (address (allocate-block area-number n-words)))
    (setq store-halfwords-address address
          store-halfwords-count (* 2 n-words))
    address))

(defun store-halfword (hwd)
  (if (oddp (setq store-halfwords-count (1- store-halfwords-count)))
      (setq store-halfwords-buffer hwd)
      (vwrite store-halfwords-address (dpb hwd #o2020 store-halfwords-buffer))
      (setq store-halfwords-address (1+ store-halfwords-address))))

(defun end-store-halfwords ()
  (or (zerop store-halfwords-count)
      (ferror "store-halfword called wrong number of times")))

;;; Given an object in our world, construct a matching one in the cold load world
;;; and return a cold-load pointer to it.
(defun make-q-list (area s-exp &aux bsize value)
  (cond ((numberp s-exp)
         (cond ((small-floatp s-exp) (make-small-flonum s-exp))
               ((floatp s-exp) (store-flonum 'sym:working-storage-area s-exp))
               ((complexp s-exp) (store-complex 'sym:working-storage-area s-exp))
               ((integerp s-exp)
                (cond ((and ( s-exp big-fixnum) ( s-exp little-fixnum)) (vfix s-exp))
                      (t (store-bignum 'sym:working-storage-area s-exp))))
               ((rationalp s-exp)
                (store-rational 'sym:working-storage-area s-exp))
               (t (ferror "unknown number type ~s" s-exp))))
        ((characterp s-exp)
         (vmake-pointer sym:dtp-character (char-int s-exp)))
        ((symbolp s-exp) (qintern s-exp))
        ((stringp s-exp) (store-string 'sym:p-n-string s-exp))
        ((atom s-exp) (ferror "~S unknown type" s-exp))
        (t
;          (or (memq area sym:list-structured-areas)
;              (ferror "make-q-list in non-list-structured area ~S" area))
           (setq bsize (length s-exp))
           (cond ((cdr (last s-exp)) (setq bsize (1+ bsize))))  ;ends in dotted pair
           (setq value (vmake-pointer sym:dtp-list (allocate-block area bsize)))
           (do ((s-exp s-exp (cdr s-exp))
                (adr (logand q-pointer-mask value) (1+ adr))
                (c-code))
               ((atom s-exp)
                (or (null s-exp)
                    (vwrite-cdr adr sym:cdr-error (make-q-list area s-exp))))
             (setq c-code (cond ((null (cdr s-exp)) sym:cdr-nil)
                                ((atom (cdr s-exp)) sym:cdr-normal)
                                (t sym:cdr-next)))
             (vwrite-cdr adr c-code (make-q-list area (car s-exp))))
           value)))

(defun make-small-flonum (s-exp)  ;I hope the format doesn't change!
  (let ((as-fixnum (%pointer s-exp)))
;; The following line should be removed once we are running in system 99 or above.
;   (setq as-fixnum (%pointer-plus as-fixnum #o40000000))
    (vmake-pointer sym:dtp-small-flonum as-fixnum)))

(defun magic-aref (a i n)
  (if (< i n) (aref a i) #o200))

(defun store-string (area string)
;   (and (memq area sym:list-structured-areas)
;       (ferror "store-string in list-structured area"))
   (let* ((n-chars (string-length string))
          (n-words (+ 1 (ceiling n-chars 4)))
          long-flag
          adr)
     (and (> n-chars sym:%array-max-short-index-length)
          (setq long-flag t
                n-words (1+ n-words)))
     (setq adr (allocate-block area n-words))
     (vwrite adr (vmake-pointer sym:dtp-array-header
                                (+ sym:array-dim-mult   ;1-dim
                                   sym:art-string
                                   (if long-flag
                                       (dpb 1 sym:%%array-long-length-flag 0)
                                     n-chars))))
     (when long-flag
       (vwrite (1+ adr) n-chars))
     (do ((i (if long-flag 2 1) (1+ i))
          (j 0 (+ j 4)))
         ((= i n-words))
       (vwrite (+ adr i)
               (+ (magic-aref string j n-chars)
                  (ash (magic-aref string (1+ j) n-chars) 8)
                  (ash (magic-aref string (+ j 2) n-chars) 16.)
                  (ash (magic-aref string (+ j 3) n-chars) 24.))))
     (vmake-pointer sym:dtp-array-pointer adr)))

(defun store-symbol-vector (atom-name area)
;  (and (memq area sym:list-structured-areas)
;       (ferror "store-symbol-vector in list-structured area ~S" area))
  (and (eq atom-name '**screw**)
       (ferror "you've probably encountered a bug in COLDLD" atom-name))
  (prog (adr sym path real-atom-name package-name pname keyword-p)
     (cond ((setq path (get atom-name 'package-path))
            (or (= (length path) 2)
                (ferror "package path ~S not 2 long - code not hairy enough"))
            (setq keyword-p (eq (car path) 'keyword))
            (setq package-name (qintern (car path))
                  real-atom-name (car (last path))))
           (t (setq package-name qnil real-atom-name atom-name)))
     (cond ((and (>= (array-length (string atom-name)) 1)
                 (= (aref (string atom-name) 0)
                    #/:))
            (setq keyword-p t)))
     (when symbol-creation-trace-list   ;debugging tool to track down appears twice in
       (do ((l symbol-creation-trace-list (cdr l)))     ;cold load messages.
           ((null l))
         (cond ((string= real-atom-name (car l))
                (format t "
A-flavor-of ~S being-created, atom-name ~S, path ~S, package-name ~S, keyword-p ~s"
                        real-atom-name atom-name path  package-name keyword-p)))))
     (setq pname (store-string 'sym:p-n-string (string real-atom-name)))
     (setq adr (allocate-block area sym:length-of-atom-head))
     (setq sym (vmake-pointer sym:dtp-symbol adr))
     (vwrite-cdr adr sym:cdr-next (vmake-pointer sym:dtp-symbol-header pname))
     (vwrite-cdr (+ adr 1) sym:cdr-next (if keyword-p
                                            sym         ;keywords self evaluate.
                                          (vmake-pointer sym:dtp-null adr)))
     (vwrite-cdr (+ adr 2) sym:cdr-next (vmake-pointer sym:dtp-null adr))
     (vwrite-cdr (+ adr 3) sym:cdr-next qnil)
     (vwrite-cdr (+ adr 4) sym:cdr-nil package-name)
     (putprop atom-name sym 'q-atom-head)
     (return sym)))

(deff store-bignum 'store-extended-number)
(deff store-flonum 'store-extended-number)

(defun store-extended-number (area number)
;  (and (memq area sym:list-structured-areas)
;       (ferror "extended-number in list-structured area ~S" area))
  (let* ((size (%structure-total-size number))
         (adr (allocate-block area size)))
    (loop for i from 0 below size
          do (vwrite-low (+ adr i) (%p-ldb-offset #o0020 number i))
             (vwrite-high (+ adr i) (%p-ldb-offset #o2020 number i)))
    (vmake-pointer sym:dtp-extended-number adr)))

(deff store-complex 'store-complex-or-rational)
(deff store-rational 'store-complex-or-rational)

;someday, this can be combined back with store-extended-number, and
;copy over first %structure-boxed-size Qs, then the rest of %structure-total-size
;words
(defun store-complex-or-rational (area number)
  (if (not (or (typep number 'rational)
               (typep number 'complex)))
      (ferror "not complex or rational"))
;  (and (memq area sym:list-structured-areas)
;       (ferror "extended-number in list-structured area ~S" area))
  (let ((adr (allocate-block area 3)))
    ;copy over the header
    (vwrite-low adr (%p-ldb #o0020 number))
    (vwrite-high adr (%p-ldb #o2020 number))
    (vwrite-cdr (+ adr 1) sym:cdr-next (make-q-list area (%p-contents-offset number 1)))
    (vwrite-cdr (+ adr 2) sym:cdr-nil (make-q-list area (%p-contents-offset number 2)))
    (vmake-pointer sym:dtp-extended-number adr)))


;;; New version of qintern.  Machine builds obarray when it first comes up (easy enough).
(defun qintern (atom-name)
  (check-type atom-name symbol)
  (cond ((not (eq (car (package-cell-location atom-name)) sym-package))
         (let* ((sym-atom-name (intern (string atom-name) sym-package))
                (current-package-path-property (get sym-atom-name 'package-path))
                (new-package-path-property
                  (list (intern (package-name (car (package-cell-location atom-name)))
                                'cold)
                        (intern (string atom-name) 'cold))))
           (if (not (or (equal current-package-path-property
                               new-package-path-property)
                        ;for these, we either dont really want it there, or it will get there.
                        (memq (car new-package-path-property)
                              '(global cold cold-symbols system-internals))))
               (cond ((null current-package-path-property)
                      (format t "~%Adding package path property ~s for ~s at qintern"
                              new-package-path-property
                              atom-name)
                      (putprop sym-atom-name new-package-path-property 'package-path))
                     (t
                      ;cold load can only deal with each string being in one package.
                      (ferror "Need to add package-path property ~S for symbol ~s, but ~s is already on sym: symbol"
                              new-package-path-property
                              atom-name
                              current-package-path-property))))
           (setq atom-name sym-atom-name))))
  (or (get atom-name 'q-atom-head)
      (store-symbol-vector atom-name 'sym:nr-sym)))

(defun q-atom-head-reset (&optional (pkg sym-package))
  (mapatoms #'(lambda (x) (remprop x 'q-atom-head)) pkg nil))

(defun print-q-symbols (&optional (pkg sym-package))
  (mapatoms #'(lambda (x)
                (let ((q-atom (get x 'q-atom-head)))
                  (if q-atom
                      (format t "~%Symbol ~s, q-atom-head ~s" x q-atom))))
            pkg
            nil))

(defun store-nxtnil-cdr-code (area)
  (vstore-cdr-code (1- (aref area-alloc-pointers (get-area-number area))) sym:cdr-nil))

(defun store-list-of-atoms (area loa)
  (let ((adr (allocate-block area (length loa))))
    (do ((loa loa (cdr loa))
         (adr adr (1+ adr)))
        ((null loa))
      (vwrite-cdr adr (if (null (cdr loa)) sym:cdr-nil sym:cdr-next)
                      (q-convert-atom (car loa))))
    adr))

(defun q-convert-atom (atm)
  (if (numberp atm) (make-q-list nil atm) (qintern atm)))

(defun store-list (area lst)
  (let ((adr (allocate-block area (length lst))))
    (do ((lst lst (cdr lst))
         (adr adr (1+ adr)))
        ((null lst))
      (vwrite-cdr adr (if (null (cdr lst)) sym:cdr-nil sym:cdr-next)
                      (make-q-list
                        'sym:init-list-area
                        (car lst))))
    adr))

(defun store-nils (area number)
  (let ((adr (allocate-block area number)))
    (do ((number number (1- number))
         (adr adr (1+ adr)))
        ((zerop number))
      (vwrite-cdr adr (if (= number 1) sym:cdr-nil sym:cdr-next) qnil))
    adr))

(defun storeq (area data)
  (let ((adr (allocate-block area 1)))
    (vwrite adr data)
    adr))

(defun store-cdr-q (area cdr-code data)
  (let ((adr (allocate-block area 1)))
    (vwrite-cdr adr cdr-code data)
    adr))

;;;; Hair for making arrays

(defun init-q-array (area name offset type dimlist displaced-p leader)
  (init-q-array-named-str area name offset type dimlist displaced-p leader nil))

;NOTE!! LEADER IS STOREQ ED DIRECTLY SO IT MUST ALREADY BE MAKE-Q-LIST IFIED
(defun init-q-array-named-str (area name offset type dimlist displaced-p leader named-str)
        ;  leader is contents of array leader, if desired.  it is in "storage order"
        ;which is reversed from index order.
        ;  if leader is numeric, it means make leader consisting of that many q's
        ;initialized to nil.
        ;  if name -> nil, return (list <array-adr> <data-length>) and dont try
        ;to store in function or value cell.
        ;offset 1 for storing pointer to array in value cell, 2 for function cell
;  (and (memq area sym:list-structured-areas)
;       (ferror "init-q-array in list-structured area"))
  (prog (tem ndims index-length data-length tem1 leader-length header-q long-array-flag adr)
        (and (numberp dimlist) (setq dimlist (list dimlist)))
        (setq ndims (length dimlist))
        (when sym:new-array-index-order
          (setq dimlist (reverse dimlist)))
        ;; The rest of this is correct for column-major order.
        (setq index-length (sys:list-product dimlist))
        (cond ((and (> index-length sym:%array-max-short-index-length)
                    (null displaced-p))
               (setq long-array-flag t)))
        (setq leader-length (cond ((null leader) 0)
                                  ((numberp leader) (+ 2 leader))
                                  (t (+ 2 (length leader)))))
        (cond ((null (setq tem (assq type sym:array-elements-per-q)))
               (ferror "~S bad array type" type)))
        (setq tem (cdr tem))
        (cond ((not (null leader))
               (setq adr (allocate-block area leader-length))
               (vwrite adr (vmake-pointer sym:dtp-header
                                          (dpb sym:%header-type-array-leader
                                               sym:%%header-type-field
                                               leader-length)))
               (cond ((numberp leader)
                      (dotimes (i leader)
                        (vwrite (+ adr i 1) qnil))
                      (and named-str (vwrite (+ adr leader -1)  ;(array-leader x 1)
                                             (qintern named-str))))
                     (t (do ((l leader (cdr l))
                             (i 1 (1+ i)))
                            ((null l))
                          (vwrite (+ adr i) (car l)))))
               (vwrite (+ adr leader-length -1) (vfix (- leader-length 2)))))
        (setq data-length (ceiling index-length tem))
        (setq header-q (vmake-pointer sym:dtp-array-header
                                      (+ (* sym:array-dim-mult ndims)
                                         (symeval type))))
        (and leader (setq header-q (+ header-q sym:array-leader-bit)))
        (and named-str (setq header-q (+ header-q sym:array-named-structure-flag)))
        (cond (displaced-p   ;note, no index-offset arrays in cold-load
                (setq tem 1 header-q (+ header-q sym:array-displaced-bit 2)))
              ((null long-array-flag)
                (setq tem 1 header-q (+ header-q index-length)))
              (t (setq tem 2 header-q (+ header-q sym:array-long-length-flag))))
        (setq tem1 (setq adr (allocate-block area (+ tem ndims -1))))
        (vwrite adr header-q)
        (and (= tem 2) (vwrite (setq adr (1+ adr)) (vfix index-length)))
        ;Store all dimensions except for last
        (do l dimlist (cdr l) (null (cdr l))
          (vwrite (setq adr (1+ adr)) (vfix (car dimlist))))
        (cond ((null name) (return (list tem1 data-length))))
        (vstore-contents (+ (qintern name) offset)
                         (vmake-pointer sym:dtp-array-pointer tem1))
        (return data-length)))

(defun store-q-array-leader (arrayp idx data)
  (vwrite (- arrayp (+ 2 idx))                  ;1 for array header, 1 for ldr len
          data))

;;;; Setting up various magic data structures,
;;;;  mostly having to do with the microcode and the fixed-areas

(defun store-support-vector (item)
  (let ((adr (allocate-block 'sym:support-entry-vector 1)))
    (vwrite-cdr adr sym:cdr-next
                (cond ((eq (car item) 'sym:function)
                       (get-q-fctn-cell (cadr item)))
                      ((memq (car item) '(quote sym:quote))
                       (make-q-list
                         'sym:init-list-area
                         (cadr item)))
                      (t (ferror "bad-support-code: ~S" item))))
    adr))

(defun get-q-fctn-cell (fctn &aux tem)
  (and (setq tem (get fctn 'q-atom-head))
       (vcontents (+ tem 2))))

(defun store-displaced-array-pointer (area)
 (prog (fillp area-array-type data-length adr)
    (setq fillp (memq area areas-with-fill-pointers))
    (setq area-array-type
          (cond ((eq area 'sym:address-space-map) 'sym:art-16b) ;%address-space-map-byte-size
                ((memq area list-referenced-areas) 'sym:art-q-list)
                ((memq area array-referenced-areas) 'sym:art-q)
                ((eq area 'sym:virtual-page-volatility) 'sym:art-2b)
                (t 'sym:art-inum)))
    (init-q-array 'sym:control-tables
                  area
                  2
                  area-array-type
                  (setq data-length     ;In entries, not Qs!
                        (case area
                          (sym:virtual-page-volatility
                           (^ 2 17.))
                          (sym:address-space-map
                           (truncate (1+ q-pointer-mask) sym:%address-space-quantum-size))
                          (sym:system-communication-area
                           (length sym:system-communication-area-qs))
                          (otherwise
                           (* sym:page-size (get-area-size area)))))
                  t
                  (and fillp
                       (list (vfix (cond ((memq area area-corresponding-arrays)
                                          (length sym:initial-area-list))
                                         ((memq area
                                                micro-code-entry-corresponding-arrays)
                                          (length micro-code-entry-vector))
                                         ((eq area 'sym:address-space-map)
                                          (truncate (1+ q-pointer-mask)
                                                    sym:%address-space-quantum-size))
                                         (t
                                          (* sym:page-size (get-area-size area))))))))
    (setq adr (allocate-block 'sym:control-tables 2))
    (vwrite adr (vfix (get-area-origin area)))
    (vwrite (1+ adr) (vfix data-length))))

;;; x is a symbol or cons function-name instruction-name
(defun store-misc-link (x)
  (cond ((atom x)
         (misc-store-micro-entry x x))
        ((misc-store-micro-entry (car x) (cdr x)))))

;;; special kludge which filters out *catch
(defun store-misc-link-1 (x)
  (or (eq x 'sym:*catch)
      (store-misc-link x)))

;;; This creates an indirect through the MICRO-CODE-SYMBOL-AREA by using
;;; DTP-FIX and #o200 less than the misc function index.  This makes
;;; the core image independent of the microcode version.
(defun misc-store-micro-entry (name me-name)
  (prog (misc-index u-entry-prop u-entry-index)
        (cond ((null (setq misc-index (get me-name 'sym:qlval)))
               (ferror "No QLVAL property: ~S" me-name)))
        (setq u-entry-prop (vfix (- misc-index #o200)))
        (setq u-entry-index (get-u-entry-index name))
        (vstore-contents (+ (qintern name) 2)   ;function cell
                         (vmake-pointer sym:dtp-u-entry u-entry-index))
        (vstore-contents (+ (get-area-origin 'sym:micro-code-entry-area) u-entry-index)
                         u-entry-prop)
        (vstore-contents (+ (get-area-origin 'sym:micro-code-entry-args-info-area)
                            u-entry-index)
                         (make-q-list
                           'sym:init-list-area
                           (get-q-args-prop name)))))

;;; This abbreviated version of the stuff in UTIL2 should be enough to get us off the ground
(defun get-q-args-prop (fctn &aux tem)
  (cond ((setq tem (get fctn 'sym:argdesc))
         (get-q-args-prop-from-argdesc-prop tem))
        ((setq tem (get fctn 'sym:qintcmp))
         (+ (lsh tem 6) tem))
        ;; You may think this is a kludge, but in the Maclisp cold-load generator
        ;; it gets the number of arguments out of the Maclisp subr of the same name!
        ((setq tem (get fctn 'defmic-nargs-info))
         (if (consp tem) (+ (lsh (car tem) 6) (cdr tem))
             (+ (lsh tem 6) tem)))
        (t (ferror "Cannot find arg desc for ~S" fctn))))

(defun get-q-args-prop-from-argdesc-prop (arg-desc)
  (prog (prop min-args max-args count item)
        (setq prop 0 min-args 0 max-args 0)
     l  (cond ((null arg-desc) (return (+ prop (lsh min-args 6) max-args))))
        (setq count (caar arg-desc))
        (setq item (cadar arg-desc)) ;list of arg syntax, quote type, other attributes
        (setq arg-desc (cdr arg-desc))
     l1 (cond ((= 0 count) (go l))
              ((memq 'sym:fef-arg-rest item)
               (setq prop (logior prop (if (or (memq 'sym:fef-qt-eval item)
                                               (memq 'sym:fef-qt-dontcare item))
                                           sym:%arg-desc-evaled-rest
                                           sym:%arg-desc-quoted-rest)))
               (go l))
              ((memq 'sym:fef-arg-req item)
               (setq min-args (1+ min-args)))
              ((memq 'sym:fef-arg-opt item))
              (t (go l)))
        (setq max-args (1+ max-args))
        (or (memq 'sym:fef-qt-eval item)
            (memq 'sym:fef-qt-dontcare item)
            (setq prop (logior prop sym:%arg-desc-fef-quote-hair)))
        (setq count (1- count))
        (go l1)))

(defvar micro-code-entry-vector nil)

(defun get-u-entry-index (fctn)
  (prog (tem)
        (cond ((setq tem (find-position-in-list fctn micro-code-entry-vector))
               (return tem)))
        (setq tem (length micro-code-entry-vector))
        (store-cdr-q 'sym:micro-code-entry-area sym:cdr-next qnil)  ;will be changed
        (store-cdr-q 'sym:micro-code-entry-name-area sym:cdr-next (qintern fctn))
        (store-cdr-q 'sym:micro-code-entry-args-info-area sym:cdr-next qnil)  ;will be chngd
        (store-cdr-q 'sym:micro-code-entry-arglist-area sym:cdr-next qnil) ;set on startup
        (setq micro-code-entry-vector (nconc micro-code-entry-vector
                                             (list fctn)))
        (return tem)))

(defun store-micro-code-symbol-name (name)
  (let ((opcode (get name 'sym:qlval)))
    (or opcode (ferror "no qlval property in store-micro-code-symbol-name: ~S" name))
    (vstore-contents (+ (get-area-origin 'sym:micro-code-symbol-name-area) (- opcode #o200))
                     (qintern name))))

(defun store-lisp-value-list (x)
  (mapc #'store-lisp-value (symeval x)))

(defun store-lisp-value (sym)
  (storein-q-value-cell sym (make-q-list
                              'sym:init-list-area
                              (symeval sym))))

;;; Store cdr-coded list of #o1000 (or however many) NIL's.
(defun init-micro-code-symbol-name-area ()
  (store-nils 'sym:micro-code-symbol-name-area
              (* sym:page-size
                 (getf sym:cold-load-area-sizes 'sym:micro-code-symbol-name-area))))

(defun cold-load-time-set (sym value)
;  (cond ((or (numberp value)
;            (stringp value)
;            (memq value '(sym:t sym:nil))))
;       ((quotep value)
;        (setq value (cadr value)))
;       (t (ferror "(setq ~S ~S) no can do" sym value)))
  (storein-q-value-cell sym (make-q-list
                              'sym:init-list-area
                              value)))

(defun storein-q-value-cell (sym data)
  (vstore-contents (1+ (qintern sym)) data))

(defun store-constant (c)
  (vwrite-cdr (allocate-block 'sym:constants-area 1)
              sym:cdr-next
              (make-q-list
                'sym:init-list-area
                c)))

(defun init-scratch-pad-area ()
  (init-area-contents 'sym:scratch-pad-init-area (vfix 0))
  (aset (+ (aref area-origins (get-area-number 'sym:scratch-pad-init-area))
           sym:scratch-pad-parameter-offset
           (length sym:scratch-pad-parameters))
        area-alloc-pointers
        (get-area-number 'sym:scratch-pad-init-area))
  (scratch-store-q 'sym:initial-top-level-function
                   (vmake-pointer sym:dtp-locative
                                  (+ (qintern 'sym:lisp-top-level) 2)))
  ;trap-handler (not used)
  (let ((initial-stack-group-pointer (make-initial-stack-group-structure)))
    (scratch-store-q 'sym:current-stack-group initial-stack-group-pointer)
    (scratch-store-q 'sym:initial-stack-group initial-stack-group-pointer))
  (scratch-store-q 'sym:error-handler-stack-group qnil)  ;initialized at run time
  (scratch-store-q 'sym:default-cons-area (vfix (get-area-number 'sym:working-storage-area))))

(defun scratch-store-q (symbolic-name data)
   (prog (tem origin)
         (setq origin (get-area-origin 'sym:scratch-pad-init-area))
         (cond ((setq tem (find-position-in-list symbolic-name sym:scratch-pad-pointers))
                (vstore-contents (+ origin tem) data))
               ((setq tem (find-position-in-list symbolic-name sym:scratch-pad-parameters))
                (vstore-contents (+ origin sym:scratch-pad-parameter-offset tem) data))
               (t (ferror "unknown-scratch-quantity: ~S" symbolic-name)))))

(defun store-a-mem-location-names ()
    (do ((name sym:a-memory-location-names (cdr name))
         (locn (+ sym:size-of-hardware-m-memory sym:a-memory-virtual-address) (1+ locn)))
        ((null name))
     (store-mem-location (car name) locn))
    (do name sym:m-memory-location-names (cdr name) (null name)
     (store-mem-location (car name) (get (car name) 'sym:forwarding-virtual-address)))
    (store-mem-location 'sym:%gc-generation-number
                        (+ #o400 sym:%sys-com-gc-generation-number))
    )

(defun store-mem-location (name locn)
  (storein-q-value-cell name (vmake-pointer sym:dtp-one-q-forward locn)))

(defun make-ordered-array-list (assoc-list)
  (mapcar #'(lambda (x) (cdr (assq x assoc-list)))
          sym:array-types))

;;;The order store-misc-link is called determines the final micro-code-entry
;;; numbers that are assigned.  however, except for 0 which must be *catch,
;;; micro-code-entry numbers are unconstrained and independant from everything
;;; else.  So the other entries below may be in any order.
(defun store-misc-u-entry-links ()
  (store-misc-link 'sym:*catch)         ;must be first
  (mapc #'store-misc-link-1 misc-function-list)
  ;; now set up the first #o600 locations of micro-code-symbol-name-area
  (init-micro-code-symbol-name-area)
  (mapc #'store-micro-code-symbol-name misc-instruction-list))

(defun make-initial-stack-group-structure ()
  (make-stack-group-structure 'sym:main-stack-group 'sym:control-tables
                              'sym:pdl-area 'sym:special-pdl-area
                              sym:sg-state-active))

(defun make-stack-group-structure (name sg-area linear-area l-b-p-area initial-state)
  (prog (sg pdl-array l-b-p-array reg-len spec-len)
        (setq sg (car (init-q-array sg-area nil nil 'sym:art-stack-group-head '(0)
                                    nil (length sym:stack-group-head-leader-qs))))
        ;; 4  leader-header + leader-length-q + array-header-q + long-length-q
        (setq reg-len (- 40000 (+ (length sym:reg-pdl-leader-qs) 4)))
        (setq spec-len (- 4000 (+ (length sym:special-pdl-leader-qs) 4)))
        (setq pdl-array
              (car (init-q-array linear-area nil nil 'sym:art-reg-pdl (list reg-len)
                                 nil (length sym:reg-pdl-leader-qs))))
        (allocate-block linear-area reg-len)    ;advance free pointer
        (setq l-b-p-array
              (car (init-q-array l-b-p-area nil nil 'sym:art-special-pdl (list spec-len)
                                 nil (length sym:special-pdl-leader-qs))))
        (allocate-block l-b-p-area spec-len)    ;advance free pointer
        (stack-group-linkup sg pdl-array l-b-p-array)
        (store-q-array-leader sg sym:sg-state (vfix initial-state))
        (store-q-array-leader sg sym:sg-name (make-q-list
                                               'sym:init-list-area
                                               name))
        (store-q-array-leader sg sym:sg-regular-pdl-limit
                              (make-q-list
                                'sym:init-list-area
                                (- reg-len 100)))
        (store-q-array-leader sg sym:sg-special-pdl-limit
                              (make-q-list
                                'sym:init-list-area
                                (- spec-len 100)))
        (return (vmake-pointer sym:dtp-stack-group sg))))

(defun stack-group-linkup (sg pdl-arrayp l-b-p-arrayp)
  (store-q-array-leader l-b-p-arrayp sym:special-pdl-sg-head-pointer
                        (vmake-pointer sym:dtp-stack-group sg))
  (store-q-array-leader pdl-arrayp sym:reg-pdl-sg-head-pointer
                        (vmake-pointer sym:dtp-stack-group sg))
  (store-q-array-leader sg sym:sg-special-pdl
                        (vmake-pointer sym:dtp-array-pointer l-b-p-arrayp))
  (store-q-array-leader sg sym:sg-regular-pdl
                        (vmake-pointer sym:dtp-array-pointer pdl-arrayp))
  (store-q-array-leader sg sym:sg-initial-function-index (vfix 3)))

;This better agree with the order of the list of qs in QCOM
(defun init-system-communication-area (&aux (nqs 30.) adr)
  (setq adr (allocate-block 'sym:system-communication-area nqs))
  (vwrite (+ adr sym:%sys-com-area-origin-pntr)
          (vmake-pointer sym:dtp-fix (get-area-origin 'sym:region-origin)))
  (vwrite (+ adr sym:%sys-com-valid-size) (vfix 0))     ;fixed later
  (vwrite (+ adr sym:%sys-com-page-table-pntr)
          (vmake-pointer sym:dtp-fix (get-area-origin 'sym:page-table-area)))
  (vwrite (+ adr sym:%sys-com-page-table-size)  ;Real value put in by microcode
          (vfix (* (get-area-size 'sym:page-table-area) sym:page-size)))
  (vwrite (+ adr sym:%sys-com-obarray-pntr) (qintern 'sym:obarray))
  (vwrite (+ adr sym:%sys-com-ether-free-list) qnil)
  (vwrite (+ adr sym:%sys-com-ether-transmit-list) qnil)
  (vwrite (+ adr sym:%sys-com-ether-receive-list) qnil)
  (vwrite (+ adr sym:%sys-com-band-format) (vfix 0))    ;not compressed format
  (vwrite (+ adr sym:%sys-com-gc-generation-number) (vfix 0))
  (vwrite (+ adr sym:%sys-com-unibus-interrupt-list) (vfix 0))
  (vwrite (+ adr sym:%sys-com-temporary) (vfix 0))
  (vwrite (+ adr sym:%sys-com-free-area/#-list) 0)      ;fixed later
  (vwrite (+ adr sym:%sys-com-free-region/#-list) 0)    ;fixed later
  (vwrite (+ adr sym:%sys-com-memory-size) (vfix #o100000))     ;assume 32K, fixed later
  (vwrite (+ adr sym:%sys-com-wired-size)  ;region-moby-bits-array is the first pageable area
          (vfix (get-area-origin 'sym:region-moby-bits-array)))
  (vwrite (+ adr sym:%sys-com-chaos-free-list) qnil)
  (vwrite (+ adr sym:%sys-com-chaos-transmit-list) qnil)
  (vwrite (+ adr sym:%sys-com-chaos-receive-list) qnil)
  (vwrite (+ adr sym:%sys-com-debugger-requests) (vfix 0))
  (vwrite (+ adr sym:%sys-com-debugger-keep-alive) (vfix 0))
  (vwrite (+ adr sym:%sys-com-debugger-data-1) (vfix 0))
  (vwrite (+ adr sym:%sys-com-debugger-data-2) (vfix 0))
  ;(vwrite (+ adr sym:%sys-com-major-version) qnil)     ;I.e. fresh cold-load
  (vwrite (+ adr  sym:%sys-com-major-version)
          (vfix (fs:reading-from-file (form "SYS:PATCH; SYSTEM PATCH-DIRECTORY >")
                  (return (cadr form)))))
  (vwrite (+ adr sym:%sys-com-desired-microcode-version) qnil)  ;Set by system initialization
  (vwrite (+ adr sym:%sys-com-highest-virtual-address)
          (vfix 0)) ;used only if compressed band.
  (vwrite (+ adr sym:%sys-com-pointer-width) (vfix sym:%%q-pointer))
  (vwrite (+ adr sym:%sys-com-number-regions) (vfix sym:number-of-regions))
  (vwrite (+ adr sym:%sys-com-number-areas) (vfix sym:number-of-areas))
  (vwrite (+ adr sym:%sys-com-band-crc) (vfix 0))
  (or (= nqs (length sym:system-communication-area-qs))
      (ferror "QCOM and COLDUT disagree about system-communication-area")))

(defun q-storage-finalize ()
  (mapc #'store-support-vector sym:support-vector-contents)
  (store-nxtnil-cdr-code 'sym:support-entry-vector)
  (mapc #'store-displaced-array-pointer sym:initial-area-list)
  (scratch-store-q 'sym:active-micro-code-entries (vfix (length micro-code-entry-vector)))
  ;; Transfer over free pointers
  (do ((area-number 0 (1+ area-number))
       (a-l sym:initial-area-list (cdr a-l))
       (rfp (get-area-origin 'sym:region-free-pointer)))
      ((null a-l))
    (vwrite (+ rfp area-number)
            (- (aref area-alloc-pointers area-number) (aref area-origins area-number)))
    )

  (let ((high-loc (aref area-alloc-bounds (1- (length sym:initial-area-list)))))
    (vwrite (+ (get-area-origin 'sym:system-communication-area) sym:%sys-com-valid-size)
            (vfix high-loc)))
  ;; Set up the area# and region# free lists
  (vwrite (+ (get-area-origin 'sym:system-communication-area) sym:%sys-com-free-area/#-list)
          (vfix (length sym:initial-area-list)))
  (vwrite (+ (get-area-origin 'sym:system-communication-area) sym:%sys-com-free-region/#-list)
          (vfix (length sym:initial-area-list)))
  (do i (length sym:initial-area-list) (1+ i) (= i (1- sym:number-of-areas))    ;all but the last
    (vwrite (+ (get-area-origin 'sym:region-list-thread) i) (1+ i))
    (vwrite (+ (get-area-origin 'sym:area-region-list) i) (1+ i)))
  (vwrite (+ (get-area-origin 'sym:region-list-thread) (1- sym:number-of-areas)) 0)
  (vwrite (+ (get-area-origin 'sym:area-region-list) (1- sym:number-of-areas)) 0)
  ;; Make certain areas look full
  (dolist (area 'sym:(region-origin region-length region-free-pointer region-gc-pointer
                      region-bits region-list-thread area-name area-region-list
                      region-moby-bits-array region-namespace-origin region-spare
                      area-region-bits area-region-size
                      region-area-map
                      virtual-page-volatility virtual-page-data
;                     linear-pdl-area linear-bind-pdl-area
                      ))
    (vwrite (+ (get-area-origin 'sym:region-free-pointer) (get-area-number area))
            (* (get-area-size area) sym:page-size)))
  ;; Initialize unused portions of the disk
  (initialize-unused-pages)
  (init-address-space-map)
  ;; Don't bother setting up the PHT and PPD, the microcode will take care of it
  ;; Cold-booting into this band will then do the right thing with it
  (init-area-contents 'sym:page-table-area (vfix 0))
  ;; Terminate areas which have overlying lists
  (store-nxtnil-cdr-code 'sym:constants-area)
  (store-nxtnil-cdr-code 'sym:scratch-pad-init-area)
  (store-nxtnil-cdr-code 'sym:area-name)
  (store-nxtnil-cdr-code 'sym:micro-code-entry-area)
  (store-nxtnil-cdr-code 'sym:micro-code-entry-name-area)
  (store-nxtnil-cdr-code 'sym:micro-code-entry-args-info-area)
  (store-nxtnil-cdr-code 'sym:micro-code-entry-arglist-area))

(defun initialize-unused-pages (&aux area address high)
  (dolist (name-of-area (memq 'sym:extra-pdl-area
                              sym:initial-area-list)) ;no trash low fixed areas
    (setq area (get-area-number name-of-area)
          address (aref area-alloc-pointers area)
          high (aref area-alloc-bounds area))
    ;Page in all the fresh pages without really paging them in, thus initializing them
    (do ((vpn (ceiling address sym:page-size) (1+ vpn))
         (hpn (ceiling high sym:page-size)))
        (( vpn hpn))
      (vmem-find-page (* vpn sym:page-size) nil))))

(defun init-address-space-map ()
  (or (= sym:%address-space-map-byte-size 16.)
      (ferror "This code only works for %address-space-map-byte-size = 16."))
  (let ((map (make-array #o4000 :type 'art-16b)) ;Initializes to 0
        (asm (get-area-origin 'sym:address-space-map))
        (asqs sym:%address-space-quantum-size))
    (if (not (zerop (logand 1 (// asm sym:page-size))))
        (ferror "ADDRESS-SPACE-MAP must allocated on an even page number"))
      ;Reason is, Ucode wants to do LDB to compute address instead of add.
    ;For each non-fixed area, find all the address space quanta in the area's initial
    ;region and store them into the map
    (loop for area from (1+ (get-area-number 'sym:init-list-area))
                   below (length sym:initial-area-list)
          unless (and (zerop (\ (aref area-origins area) asqs))
                      (zerop (\ (aref area-alloc-bounds area) asqs)))
            do (ferror "Area ~A is not an integral number of address space quanta"
                       (nth area sym:initial-area-list))
          do (loop for q from (truncate (aref area-origins area) asqs)
                         below (truncate (aref area-alloc-bounds area) asqs)
                   do (aset area map q)))
    ;Now dump this into the cold load
    (loop for i from 0 below #o2000 for j from 0 by 2
          do (vwrite (+ asm i)
                     (dpb (aref map (+ j 1)) #o2020
                          (aref map j))))
    ;cause address-space-map region to appear full so it gets dumped by band dumper.
    (vwrite (+ (get-area-origin 'sym:region-free-pointer)
               (get-area-number 'sym:address-space-map))
            (* (get-area-size 'sym:address-space-map) sym:page-size))))

(defun make-sorted-region-list ()
  (sort (do ((i 0 (1+ i))
             (al sym:initial-area-list (cdr al))
             (l nil))
            ((null al)
             (nreverse l))
          (push (cons (aref area-origins i) i) l))
        #'(lambda (x y)
            (cond ((= (car x) (car y))          ;if one is zero length, it -must- go first
                   (cond
                     ((= (aref area-origins (cdr x)) (aref area-alloc-bounds (cdr x))) t)
                     ((= (aref area-origins (cdr y)) (aref area-alloc-bounds (cdr y))) nil)
                     (t (ferror "2 non-zero-length areas at same address"))))
                  ((< (car x) (car y)))))))

;;;; Driver

(defvar cold-list-area 'sym:init-list-area
  "Where FROID (COLDLD) puts lists (usually).")
(defvar evals-to-be-sent-over)
(defvar *target-processor*)

;;; User calls this to build a cold-load onto a band

(defun make-cold (part-name &optional (unit-number 0) (processor-type si:processor-type-code)  (query t))
  (if (numberp part-name) (print (setq part-name (format nil "LOD~d" part-name))))
  (setq *unit-number* unit-number)
  (setq *target-processor* processor-type)
  (when (if query (si:find-disk-partition-for-write part-name nil *unit-number*)
          (si:find-disk-partition part-name nil *unit-number*))
    (si:update-partition-comment part-name "cold-incomplete" unit-number)
    (SI:REPORT-ELAPSED-TIME T 0 "COLD-LOAD"
      #'(LAMBDA ()
          (si:report-elapsed-time t 1 "flushing of old state"
            #'(lambda ()
                (or (boundp 'big-fixnum) (load-parameters))
                ;; Flush old state
                (q-atom-head-reset)
                (q-atom-head-reset (pkg-find-package "GLOBAL"))
                (makunbound '*cold-loaded-file-property-lists*)
                (makunbound 'cold-loaded-function-property-lists)
                (setq evals-to-be-sent-over nil)))
          (unwind-protect (progn (si:report-elapsed-time t 1 "vmem initialize"
                                   'vmem-initialize part-name)
                                 (si:report-elapsed-time t 1 "cold loading of files"
                                   #'make-cold-1
                                   (select processor-type
                                     (si:lambda-type-code
                                      si:lambda-cold-load-file-list)
                                     (si:explorer-type-code
                                      si:lambda-cold-load-file-list)
                                     (si:cadr-type-code
                                      si:cold-load-file-list)))
                                 (format nil "Boot off the ~A partition on unit ~a to test it."
                                         part-name unit-number))
            (vmem-finish))
          (si:update-partition-comment
            part-name
            (format nil "cold ~A"
                    (let ((date-and-time
                            (with-output-to-string (str)
                              (time:print-current-time str))))
                      (substring date-and-time 0 (string-search-char #/space date-and-time))))
            unit-number)))))

;;;Easier default calling sequence:
(defun make-cold-load-band (part-name &key (unit 0) (query t))
  (make-cold part-name unit si:processor-type-code query))

(defun make-cold-1 (file-list)
  ;; Divide up virtual memory into areas and initialize tables
  (SI:REPORT-ELAPSED-TIME T 3 'ASSIGN-VALUES #'assign-values sym:initial-area-list 0)
  (si:report-elapsed-time t 3 'create-areas #'create-areas)
  (make-t-and-nil)
  ;; Initialize various fixed areas and really random data tables
  (si:report-elapsed-time t 3 'init-area-contents #'init-area-contents 'sym:area-name qnil)
  (si:report-elapsed-time t 3 'store-list-of-atoms
    #'store-list-of-atoms 'sym:area-name sym:initial-area-list)
  (si:report-elapsed-time t 3 "set up constants page"
    #'(lambda () (mapc #'store-constant sym:constants-page)))
  (storein-q-value-cell 'sym:constants-page
                        (vmake-pointer sym:dtp-list (get-area-origin 'sym:constants-area)))
  (si:report-elapsed-time t 3 'init-scratch-pad-area 'init-scratch-pad-area)
  (si:report-elapsed-time t 3 'init-system-communication-area 'init-system-communication-area)
  (si:report-elapsed-time t 3 'initialize-certain-variables 'initialize-certain-variables)
  (si:report-elapsed-time t 3 "init more variables"
    #'(lambda ()
        (mapc #'store-lisp-value-list sym:q-corresponding-variable-lists)
        (mapc #'store-lisp-value-list sym:sysconf-constant-lists)))
  (si:report-elapsed-time t 3 'init-random-variables 'init-random-variables)
  (si:report-elapsed-time t 3 'store-a-mem-location-names 'store-a-mem-location-names)
  (setq micro-code-entry-vector nil)
  (store-misc-u-entry-links)
  ;A copy of AREA-LIST was previously sent over.  Change it to share with AREA-NAME.
  (storein-q-value-cell 'sym:initial-area-list
                        (vmake-pointer sym:dtp-list (get-area-origin 'sym:area-name)))
  ;;Load up all those QFASL files
  (si:report-elapsed-time t 3 "COLD-FASLOAD"
    #'(LAMBDA ()
        (mapc #'cold-fasload file-list)
        ;;Don't let list-structure portion of the readtable end up in a read-only area
        (let ((cold-list-area 'sym:property-list-area))  ;Random list-structured area
          (cold-fasload "SYS: IO; RDTBL QFASL >")
          (cold-fasload "SYS: IO; CRDTBL QFASL >"))))
  ;;Translate all pathnames needed before logical pathnames work
  ;;***not needed now that (si:qld) loads sys:sys;inner-system-file-alist which sets these variables.
;  (dolist (sym si:mini-file-alist-list)
;    (cold-load-time-set sym
;      (loop for (file pack) in (symeval sym)
;        collect (list (cold-translate-pathname file)
;                      pack
;                      (equalp (send (fs:parse-pathname file) :type) "QFASL")))))
 (setq evals-to-be-sent-over (nreverse evals-to-be-sent-over)) ;do in order specified
 (cold-load-time-set 'sym:lisp-crash-list evals-to-be-sent-over)
; (storein-q-value-cell 'sym:lisp-crash-list
;                      ;; This MAKE-Q-LIST must not use the FASL-TEMP-AREA,
;                      ;; because the list structure being created includes
;                      ;; definitions of important macros.
;                      (make-q-list
;                        'sym:init-list-area
;                        evals-to-be-sent-over))
 (setf (aref area-alloc-pointers (get-area-number 'sym:region-allocation-status))
       (aref area-alloc-bounds (get-area-number 'sym:region-allocation-status)))
 (si:report-elapsed-time t 3 'q-storage-finalize 'q-storage-finalize))

;(defun cold-translate-pathname (file)
;  #-ti
;  (send (send (fs:merge-pathname-defaults file)
;             :translated-pathname)
;       :string-for-mini)
;  #+ti
;  (let ((first-semi (string-search-char (char-int #/;) file)))
;    (cond ((and (not (null first-semi))
;               (string-search-char (char-int #/;) file (1+ first-semi)))
;          (format t "~&What should ~s translate to? (e.g. L.IO.FILE; ACCESS.QFASL#>) ? " file)
;          (readline))
;         (t
;          (send (send (fs:merge-pathname-defaults file)
;                      :translated-pathname)
;                :string-for-mini)))))


;nil and t must be stored manually since qnil and qtruth would not be bound when needed
(defun make-t-and-nil ()
  (setq qnil (vmake-pointer sym:dtp-symbol
                      (allocate-block 'sym:resident-symbol-area sym:length-of-atom-head)))
  (vwrite-cdr qnil sym:cdr-next (vmake-pointer sym:dtp-symbol-header
                                               (store-string 'sym:p-n-string "NIL")))
  (vwrite-cdr (+ qnil 1) sym:cdr-next qnil)
  (vwrite-cdr (+ qnil 2) sym:cdr-next (vmake-pointer sym:dtp-null qnil))
  (vwrite-cdr (+ qnil 3) sym:cdr-next qnil)
  (vwrite-cdr (+ qnil 4) sym:cdr-next qnil)
  (putprop 'sym:nil qnil 'q-atom-head)
  (setq qtruth (vmake-pointer sym:dtp-symbol
                   (allocate-block 'sym:resident-symbol-area sym:length-of-atom-head)))
  (vwrite-cdr qtruth sym:cdr-next (vmake-pointer sym:dtp-symbol-header
                                                 (store-string 'sym:p-n-string "T")))
  (vwrite-cdr (+ qtruth 1) sym:cdr-next qtruth)
  (vwrite-cdr (+ qtruth 2) sym:cdr-next (vmake-pointer sym:dtp-null qtruth))
  (vwrite-cdr (+ qtruth 3) sym:cdr-next qnil)
  (vwrite-cdr (+ qtruth 4) sym:cdr-next qnil)
  (putprop 'sym:t qtruth 'q-atom-head))

;Fix the values of certain variables before they are sent over
(defun initialize-certain-variables ()
; These are no longer needed since si::defvar-1 of constants is specially recognized in coldld
;  (cold-load-time-set 'sym:prin1 nil)
;  (cold-load-time-set 'sym:*read-base* 10.)
;  (cold-load-time-set 'sym:*print-base* 10.)
  (cold-load-time-set 'sym:most-positive-fixnum big-fixnum)
  (cold-load-time-set 'sym:most-negative-fixnum little-fixnum)
  )

;;; Initializations of all sorts of random variables.  Must follow the map
;;; over q-corresponding-variable-lists, because previous initializations are stored over.
(defun init-random-variables ()
  ;;set up array-types symbol (both value and function cells).
  ;;  the function cell is an array which gives maps numeric array type to symbolic name.
  ;;  the value cell is a list pointer into the above array, so is an ordered list
  ;;   of the array types.
  (init-q-array 'sym:control-tables 'sym:array-types 2 'sym:art-q-list '(32.) nil nil)
  (store-list-of-atoms 'sym:control-tables sym:array-types)
  (store-nils 'sym:control-tables (- 32. (length sym:array-types)))
  (storein-q-value-cell 'sym:array-types
    (vmake-pointer sym:dtp-list (- (aref area-alloc-pointers
                                         (get-area-number 'sym:control-tables))
                                   32.)))
  ;;set up the array-elements-per-q array.
  (init-q-array 'sym:control-tables 'sym:array-elements-per-q 2 ;fcn
                'sym:art-q-list '(32.) nil nil)
  (store-list-of-atoms 'sym:control-tables (make-ordered-array-list sym:array-elements-per-q))
  (store-nils 'sym:control-tables (- 32. (length sym:array-types)))
  ;;value cell of array-elements-per-q has assq list, is not same as array.
  ;;set up the array-bits-per-element array, similar
  (init-q-array 'sym:control-tables 'sym:array-bits-per-element 2 ;fcn
                'sym:art-q-list '(32.) nil nil)
  (store-list-of-atoms 'sym:control-tables
                       (make-ordered-array-list sym:array-bits-per-element))
  (store-nils 'sym:control-tables (- 32. (length sym:array-types)))
  ;; Set up ARRAY-BOXED-WORDS-PER-ELEMENT.
  (init-q-array 'sym:control-tables 'sym:array-boxed-words-per-element 2 ;fcn
                'sym:art-q-list '(32.) nil nil)
  (store-list-of-atoms 'sym:control-tables
                       (make-ordered-array-list sym:array-boxed-words-per-element))
  (store-nils 'sym:control-tables (- 32. (length sym:array-types)))
  ;;set up q-data-types
  (init-q-array 'sym:control-tables 'sym:q-data-types 2 'sym:art-q-list '(32.) nil
                (list (make-q-list
                        'sym:init-list-area
                        (length sym:q-data-types))))
  (store-list-of-atoms 'sym:control-tables sym:q-data-types)
  (store-nils 'sym:control-tables (- 32. (length sym:q-data-types)))
  (storein-q-value-cell 'sym:q-data-types
    (vmake-pointer sym:dtp-list (- (aref area-alloc-pointers
                                         (get-area-number 'sym:control-tables))
                                   32.)))
;  ;;Make the arrays which are mapped into A-memory
;  (init-q-array 'sym:control-tables 'sym:mouse-cursor-pattern 1
;               'sym:art-1b '(32. 32.) t nil)
;  (let ((adr (allocate-block 'sym:control-tables 2)))
;    (vwrite adr (vfix (+ (cadr (memq 'sym:mouse-cursor-pattern sym:a-memory-array-locations))
;                        sym:a-memory-virtual-address)))
;    (vwrite (1+ adr) (vfix 1024.)))
;  (init-q-array 'sym:control-tables 'sym:mouse-buttons-buffer 1
;               'sym:art-q '(32.) t nil)
;  (let ((adr (allocate-block 'sym:control-tables 2)))
;    (vwrite adr (vfix (+ (cadr (memq 'sym:mouse-buttons-buffer sym:a-memory-array-locations))
;                        sym:a-memory-virtual-address)))
;    (vwrite (1+ adr) (vfix 32.)))
;  (init-q-array 'sym:control-tables 'sym:mouse-x-scale-array 1
;               'sym:art-q '(16.) t nil)
;  (let ((adr (allocate-block 'sym:control-tables 2)))
;    (vwrite adr (vfix (+ (cadr (memq 'sym:mouse-x-scale-array sym:a-memory-array-locations))
;                        sym:a-memory-virtual-address)))
;    (vwrite (1+ adr) (vfix 16.)))
;  (init-q-array 'sym:control-tables 'sym:mouse-y-scale-array 1
;               'sym:art-q '(16.) t nil)
;  (let ((adr (allocate-block 'sym:control-tables 2)))
;    (vwrite adr (vfix (+ (cadr (memq 'sym:mouse-y-scale-array sym:a-memory-array-locations))
;                        sym:a-memory-virtual-address)))
;    (vwrite (1+ adr) (vfix 16.)))
  )
