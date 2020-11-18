;;; -*- Mode: Lisp; Base: 8; Package: moby-file-system; Readtable: ZL -*-

(defun moby-bug (msg localp)
  (let* ((area (%area-number localp))
         (region (%region-number localp))
         (region-relative-address (%pointer-difference localp (si:%region-origin region)))
         (region-moby-bits-array (aref #'sys:region-moby-bits-array region))
         (moby-bits (aref region-moby-bits-array region-relative-address)))
    (format t "~%Moby bug!: ~a, localp ~s, area ~s, region ~s"
            msg localp (aref #'sys:area-name area) region)
    (format t "~% Moby-bits ~o, starts-object ~s, boxed ~s, writable ~s"
            (ldb %%%moby-starts-object moby-bits)
            (ldb %%%moby-boxed moby-bits)
            (ldb %%%moby-writable moby-bits))
    (local-print-handles localp)
    (ferror nil "moby bug")))

(defun moby-ddt (&optional moby-address local-address region)
  (prog (ch stack mode print-region)
        (if region
            (if (fquery nil "Do you really want to hack region ~s which is part of area ~s"
                        region (aref #'sys:area-name (aref #'sys:region-area-map region)))
                (setq local-address (si:%region-origin region))))
        (if local-address
            (setq local-address (%pointer local-address)))  ;ignore any funny data type.
        (setq mode 'both)       ;or 'local or 'moby
    l0  (setq print-region t)
    l   (cond ((and moby-address local-address))
              (moby-address
               (setq local-address
                     (moby-to-local-correspondance moby-address))
               (cond ((not (= moby-address
                              (local-to-moby-correspondance local-address)))
                      (format t "~% moby <-> local correspondances not inverses!"))))
              (local-address
               (setq moby-address
                     (local-to-moby-correspondance local-address))
               (cond ((null moby-address)
                      (format t "~%There is no moby association for this local address!"))
                     ((not (= local-address
                              (moby-to-local-correspondance moby-address)))
                      (format t "~% moby <-> local correspondances not inverses!"))))
              (t (setq moby-address 1_24.)
                 (go l)))
    l1  (cond ((and moby-address (memq mode '(both moby)))
               (moby-print-data (moby-to-local-correspondance moby-address) 1)))
        (cond ((memq mode '(both local))
               (cond (local-address
                      (if print-region
                          (describe-area (%area-number local-address)))))
               (local-print-data local-address 1)
               (setq print-region nil))
              (t
               (format t "~%No local association")))
    l2 (setq ch (funcall *standard-input* :tyi))
       (cond ((< ch 200)
              (funcall *standard-output* :tyo ch)))  ;echo only printing characters.
       (select ch
         (#\end (return (values moby-address local-address)))
         (#\line (setq local-address (%pointer-plus local-address 1)
                       moby-address nil)
                 (go l))
         (#/+    (let ((incr (read)))
                   (cond ((fixp incr)
                          (setq local-address (%pointer-plus local-address incr)
                                moby-address nil)
                          (go l0))
                         (t (format t "???")
                            (go l)))))
         (#\meta-+
          (let* ((region (%region-number local-address))
                 (limit (%pointer-plus (si:%region-origin region)
                                       (moby-region-free-pointer region))))
            (cond ((= (%p-ldb-offset %%q-data-type 0 local-address)
                      dtp-unreconciled)
                   (do ((off local-address (%pointer-plus off 1)))
                       ((or (not (si:%pointer-lessp off limit))
                            (not (= (%p-ldb-offset %%q-data-type 0 off)
                                dtp-unreconciled)))
                        (setq local-address off
                              moby-address nil)
                        (go l0))))
                  (t
                   (let* ((base (%find-structure-leader local-address))
                          (size (%structure-total-size base)))
                     (setq local-address (%pointer-plus base size)
                           moby-address nil)
                     (go l0))))))
         (#\super-+
          (let* ((region (%region-number local-address))
                 (region-relative-address
                   (%pointer-difference local-address
                                        (si:%region-origin region)))
                 (region-moby-bits-array (aref #'sys:region-moby-bits-array region))
                 (limit (moby-region-free-pointer region)))
            (do ((offset (1+ region-relative-address) (1+ offset))
                 (moby-bits))
                ((>= offset limit)
                 (ferror nil "Reached free pointer"))
              (setq moby-bits (aref region-moby-bits-array offset))
              (cond ((zerop (logand %moby-region-valid moby-bits))
                     (ferror nil "Moby bits not valid"))
                    ((not (zerop (ldb %%%moby-starts-object moby-bits)))
                     (setq local-address (%pointer-plus (si:%region-origin region)
                                                        offset)
                           moby-address nil)
                     (go l0)))
              )))
         (#\tab  (push (list moby-address local-address) stack)
                 (setq local-address (%p-pointer local-address)
                       moby-address nil)
                 (go l0))
         (#/^    (setq local-address (%pointer-difference local-address 1)
                       moby-address nil)
                 (go l))
         (#\control-line (setq moby-address (1+ moby-address)
                               local-address nil)
                         (go l))
         (#\control-tab (push (list moby-address local-address) stack)
                        (multiple-value-bind (ignore ignore ignore moby-pointer)
                            (moby-fetch-all local-address)
                          (setq moby-address moby-pointer
                                local-address nil)
                          (go l0)))
         (#\control-^ (setq moby-address (1- moby-address)
                            local-address nil)
                      (go l))
         (#\altmode
          (cond ((null stack)
                 (return (values moby-address local-address)))
                (t
                 (setq moby-address (first (car stack))
                       local-address (second (car stack))
                       stack (cdr stack))
                 (go l1))))
         (#\:
          (setq ch (tyi))
          (selectq ch
            ((#\b #\B)
             (setq mode 'both))
            ((#\d #\D)
;            (cond ((and local-address moby-address)
;                   (format t "~%Dereconciling local address ~s and moby address ~s")
;                   (moby-dereconcile-single-q local-address moby-address)))
             )
            ((#\h #\H)
             (let ((h (moby-structure-handle-of-local-address local-address)))
               (Format t "~%Moby-structure-handle is ~s, (initial boxed ~s, first header ~s)"
                       h (ldb si:%%virtual-page-initial-qs h)
                         (ldb si:%%virtual-page-first-header h)))
             (let ((h (ldb si:%%virtual-page-structure-handle
                           (aref #'system:virtual-page-data (lsh local-address -8)))))
               (format t "~%Local-structure-handle is ~s, (initial boxed ~s, first header ~s)"
                       h (ldb si:%%virtual-page-initial-qs h)
                         (ldb si:%%virtual-page-first-header h)))
             (format t "~%Clean bit ~s, moby bits complete ~s, handle-and-bits-processed ~s,
 moby-bits from moby ~s, swapout-pointer ~s~%"
                     (ldb si:%%virtual-page-clean
                          (aref #'system:virtual-page-data (lsh local-address -8)))
                     (ldb si:%%virtual-page-moby-bits-complete
                          (aref #'system:virtual-page-data (lsh local-address -8)))
                     (ldb si:%%virtual-page-moby-format-handle-and-bits-processed
                          (aref #'system:virtual-page-data (lsh local-address -8)))
                     (ldb si:%%virtual-page-moby-bits-from-moby
                          (aref #'system:virtual-page-data (lsh local-address -8)))
                     (ldb si:%%virtual-page-swapout-pointer
                          (aref #'system:virtual-page-data (lsh local-address -8)))
                     ))
            ((#\m #\M)
             (setq mode 'moby))
            ((#\l #\L)
             (setq mode 'local))
            ((#\p #\P)
             (let ((local-data-type (%p-ldb-offset %%q-data-type 0 local-address)))
               (cond ((= local-data-type #.dtp-array-header)
                      (print (%make-pointer dtp-array-pointer local-address)))
                     (t
                      (cond ((memq local-data-type
                                   '(#.dtp-symbol #.dtp-list #.dtp-array-pointer
                                     #.dtp-fix #.dtp-extended-number
                                     #.dtp-character
                                     #.dtp-small-flonum #.dtp-instance))
                             (format t "~%local data = ")
                             (print (%p-contents-offset 0 local-address)))
                            (t (format t "~%local data unsafe to print!")))))))
            ((#\r #\R)
             (cond ((and local-address moby-address)
                    (format t "~%Reconciling local address ~s and moby address ~s"
                            local-address moby-address)
                    (moby-reconcile-q local-address 'unknown))))
            ((#\s #\S)
             (format t "~%Section ~s"
                     (btree-lookup *moby-page-association* (ash moby-address -8))))
            ((#\u #\U)
             (cond ((and local-address
                         (fquery nil "~%Smash unreconciled on local address ~s?"
                                 local-address))
                    (%p-dpb-offset dtp-unreconciled 0 local-address)))))))
       (go l2)))

(defun moby-print-data (local-address &optional (qs 400) (leading-spaces 0))
  (dotimes (c qs)
    (let* ((local-a (%pointer-plus local-address c))
           (region (si:%region-number local-a))
           (region-relative (%pointer-difference local-a (si:%region-origin region)))
           (msa (aref *region-to-msa* region)))
      (cond ((msa-primary-host-object msa)
             (format t "~%Remote on host ~s" (msa-primary-host-object msa)))
            (t
             (multiple-value-bind (full-value moby-bits cdr-code-and-data-type moby-pointer)
                 (moby-fetch-all local-a)
               (cond ((null full-value)
                      (format t "~%No moby space assigned"))
                     (t
                      (let ((moby-data-type (logand cdr-code-and-data-type 37)))
                        (format t "~%~V@T~o:    ~o,~o(starts-object ~o, boxed ~o, writable ~o), ~o(~s), ~o"
                                leading-spaces
                                (+ (ash (aref #'sys:region-namespace-origin region) 8)
                                   region-relative)
                                full-value
                                moby-bits
                                (ldb %%%moby-starts-object moby-bits)
                                (ldb %%%moby-boxed moby-bits)
                                (ldb %%%moby-writable moby-bits)
                                cdr-code-and-data-type
                                (nth moby-data-type q-data-types)
                                moby-pointer)
                        (cond ((= moby-data-type dtp-array-header)
                               (moby-print-array-header moby-pointer))
                              ((= moby-data-type dtp-header)
                               (moby-print-header moby-pointer))))))))))))

(defun local-print-data (local-address &optional (qs 400))
  (dotimes (c qs)
    (let* ((local-p (%pointer-plus local-address c))
           (local-data (dpb (%p-ldb-offset 2020 0 local-p) 2020 (%p-ldb-offset 0020 0 local-p)))
           (local-data-type (ldb %%q-data-type local-data))
           (local-pointer (ldb-big %%q-pointer local-data))
           (region (%region-number local-p))
           (region-relative-address (%pointer-difference local-p (si:%region-origin region)))
           (region-moby-bits-array (aref #'sys:region-moby-bits-array region)))
      (format t "~%~o: ~o, (cdr ~o, data-type ~o (~s), pointer ~o "
              (%pointer-plus local-address c)
              local-data
              (ldb %%q-cdr-code local-data)
              local-data-type
              (nth local-data-type q-data-types)
              local-pointer)
      (cond ((= local-data-type dtp-array-header)
             (moby-print-array-header local-pointer))
            ((= local-data-type dtp-header)
             (moby-print-header local-pointer)))
      (if region-moby-bits-array
          (let ((moby-bits (aref region-moby-bits-array region-relative-address)))
            (format t " Local moby-bits: ~o(starts-object ~o, boxed ~o, writable ~o)"
                    moby-bits
                    (ldb %%%moby-starts-object moby-bits)
                    (ldb %%%moby-boxed moby-bits)
                    (ldb %%%moby-writable moby-bits)
                    ))))))

(defun local-print-handles (local-address &optional (pages 1))
  (do ((local-page (lsh local-address -8) (1+ local-page))
       (c 0 (1+ c)))
      ((= c pages))
    (format t "~%page ~s, initial-qs ~s, next-object ~s, vp-status-bits ~s"
            local-page
            (ldb si:%%virtual-page-initial-qs
                 (aref #'system:virtual-page-data local-page))
            (ldb si:%%virtual-page-first-header
                 (aref #'system:virtual-page-data local-page))
            (ldb si:%%irtual-page-moby-status-bits
                 (aref #'system:virtual-page-data local-page)))))

(defun moby-print-array-header (h)
  (format t "~%  array header: type ~s, leader ~s, flag ~s, dimensions ~d, long-length ~s, index-length-if-short ~s"
          (nth (ldb si:%%array-type-field h) si:array-types)
          (ldb si:%%array-leader-bit h)
          (ldb si:%%array-flag-bit h)
          (ldb si:%%array-number-dimensions h)
          (ldb si:%%array-long-length-flag h)
          (ldb si:%%array-index-length-if-short h)))

(defun moby-print-header (h)
  (format t "~% header: type ~s"
          (nth (ldb si:%%header-type-field h) si:q-header-types)))


(defun moby-print-page (&optional (moby-page 1_16.))
  (moby-print-data (moby-to-local-correspondance (ash moby-page 8)) 400))

(defun moby-room (&optional describe)
  (let ((moby-areas nil))
    (dolist (msa *moby-section-area-and-region-associations*)
      (push (msa-area msa) moby-areas))
    (if describe
        (dolist (area moby-areas)
          (let ((msa (aref *area-to-msa* area)))
            (if (msa-map msa)
                (describe-moby-section-map (msa-map msa))))
          (describe-area area)))
    (dolist (a moby-areas)
      (format t "~% Area ~S: " a)
      (si:room-print-area a))
    ))

(defun describe-moby-section-map (section-map)
  (format t "~%Map (handle ~s) has ~s sections, moby-options ~s"
          (local-to-moby-correspondance section-map)
          (fill-pointer section-map)
          (sm-area-moby-options section-map))
  (dotimes (c (fill-pointer section-map))
    (describe-moby-region-map (aref section-map c)))
  )

(defun describe-moby-region-map (region-map)
  (format t "~%   Region map has ~d pages, namespace page origin ~o, fp ~o"
          (array-length region-map)
          (rm-namespace-page-origin region-map)
          (rm-free-pointer region-map)))

;(defun verify-moby-structure-handles ()
;  (dolist (msa *moby-section-area-and-region-associations*)
;    (si:for-every-region-in-area (region (msa-area msa))
;      (si:verify-structure-handles-for-region region))))


(defflavor foobar (a b c) ()
  :initable-instance-variables
  :gettable-instance-variables
  :settable-instance-variables)

(defmethod (foobar :hack) () (list a b c))

(defun write-list-test (&optional (phn *moby-local-package-host*))
  (let* ((root
           (open (string-append phn ":bar;list-test.mby")
                 :direction :output :moby-mapped t
                 :if-exists :append :if-does-not-exist :create
                 :create-directories t))
         (root-area (%area-number root))
         (loss (make-instance-in-area root-area 'foobar :a 'this :b 'is :c 'a-hack)))
    (rplacd root (list-in-area root-area 'foo 'fs:bar 'bletch 'si:hack loss))
    ;(moby-writeout-area root-area)
    (let ((ph (partition-host-of-partition-host-name phn)))
      (moby-finish ph))))

(defun read-list-test (&optional (phn *moby-local-package-host*))
  (open (string-append phn ":bar;list-test.mby")
        :direction :output :moby-mapped t
        :if-exists :append :if-does-not-exist :create))

(defun probe-list-test (&optional (phn *moby-local-package-host*))
  (open (string-append phn ":bar;list-test.mby")
        :direction :probe :moby-mapped t
        :if-exists :append :if-does-not-exist :create))

(defun test-direct-mapped-stream (&optional (phn *moby-local-package-host*))
  (open (string-append phn ":bar;moby-bag.lisp")
        :direction :output :moby-mapped t
        :if-exists :append :if-does-not-exist :create))

(defun write-foo (&optional (phn *moby-local-package-host*))
  (let* ((root
           (open (string-append phn ":bar;foo.mby")
                 :direction :output :moby-mapped t
                 :if-exists :append :if-does-not-exist :create
                 :create-directories t))
         (root-area (%area-number root)))
    (let ((default-cons-area root-area))
      (rplacd root (string-append "f" "o" "o")))
    (moby-writeout-area root-area)
    (let ((ph (partition-host-of-partition-host-name phn)))
      (moby-finish ph))))

(defun read-foo (&optional (phn *moby-local-package-host*))
  (open (string-append phn ":bar;foo.mby")
        :direction :output :moby-mapped t
        :if-exists :append :if-does-not-exist :create))

(defun moby-finish (&optional (phn *moby-local-package-host*))
  (let ((ph (partition-host-of-partition-host-name phn)))
    (moby-writeout-all)
    (moby-writeout-root ph)))

(DEFUN MAKE-INSTANCE-IN-AREA (AREA FLAVOR-NAME &REST INIT-OPTIONS)
  "Create and return an instance of flavor FLAVOR-NAME.
INIT-OPTIONS is an alternating list of init keywords and their values.
The new instance is sent an :INIT message."
  (SI:INSTANTIATE-FLAVOR FLAVOR-NAME (LOCF INIT-OPTIONS) T NIL AREA))


;;; Load RSG's debugging code:

(unless (fboundp 'USER:AREA-TEST)
  (load "DJ:RSG;UTILS.QFASL"))


(defvar *demapped-area-counter* 0)

(defun moby-smash-all ()
 "Clobber all moby areas.  Simulate rebooting as far as moby concerned."
 (format t "~%Turnning of GC.")
 (gc:gc-off)
  (cond (*remember-objects-with-cached-plists*
         (dolist (obj *all-objects-with-cached-plists*)
           (do ((p (plist obj) (cddr p)))
               ((null p))
             (if (numberp (car p))
                 (remprop obj (car p)))))
         (setq *all-objects-with-cached-plists* nil)))
  (let ((moby-areas nil))
    (dolist (msa *moby-section-area-and-region-associations*)
      (push (msa-area msa) moby-areas))
    (dolist (area moby-areas)
      (moby-demap-area area)))
  (makunbound '*moby-page-association*)
  (makunbound '*vmem-page-reuse-pointer*)
  (makunbound '*symbol-to-moby-address*)
  (setq *moby-package-root* nil
        *moby-local-package-host* nil)
  (setq *vmem-moby-partition-alist* nil
        *vmem-moby-dataspace-offset-allocator* 0)
  (setq *moby-command-seq* 0)
  (dotimes (i (array-length *moby-host-index*))
    (let ((h (aref *moby-host-index* i)))
      (remprop h 'moby-package-root-host-name)
      (SETQ FS:*PATHNAME-HOST-LIST* (delq h FS:*PATHNAME-HOST-LIST*)))
    (setf (aref *moby-host-index* i) nil))
  )

;disconnect area from moby system and wipe it as much as possible.
;wipes entire area to NIL, and converts it back to normal area so it
;can be garbage collected.  When there are 0 active regions, area number itself
;can be recycled.
(defun moby-demap-area (area-number)
  (let ((a-n (aref sys:#'area-name area-number)))
    (si:rename-area a-n
                    (intern (string-append "DEMAPPED-" a-n "-"
                                             (format nil "~D" *demapped-area-counter*))))
    (incf *demapped-area-counter*)
    (moby-deinitialize-area area-number)    ;clobber data in this area as much as possible.
    (moby-unhook-area area-number)          ;cleanse moby related structures.
    (moby-smash-area-to-non-moby area-number)  ;%region-space-type of regions, %area-region-bits.
    ))


(defun moby-deinitialize-area (area-number)
  (si:for-every-region-in-area (region area-number)
    (moby-deinitialize-region region)))

;can only deinitialize area with moby bits.
;  if moby bits not valid, compute them if past fresh cons boundary, else smash frob.
(defun moby-deinitialize-region (region)
  (let ((region-origin (si:%region-origin region))
        (moby-bits-array (aref #'sys:region-moby-bits-array region))
        (region-free-pointer (si:%region-free-pointer region))
        (region-relative-address 0))
    (prog (moby-bits localp ldt clobber-count skip-count)
        (if (null moby-bits-array)
            (return nil))
     l  (cond ((not (< region-relative-address region-free-pointer))
               (return t)))
        (setq moby-bits (aref moby-bits-array region-relative-address)
              localp (%pointer-plus region-origin region-relative-address))
        (cond ((zerop (ldb %%%moby-region-valid moby-bits))
               (go e1))
              ((zerop (ldb %%%moby-starts-object moby-bits))
                ;(ferror nil "STARTS-OBJECT not set where expected")
               )
              )
        (setq ldt (%p-ldb-offset %%q-data-type 0 localp))
   ;dont clobber "skeleton", clobber everything else.
   ;skeleton consists of:
   ;  header
   ;   array-leader-header:  preserve delta before array-header
   ;  array-header:
   ;   long-length q
   ;   array-indexes

        (select ldt
          (dtp-header
           (selectq (%p-ldb-offset si:%%header-type-field 0 localp)
             (si:%header-type-array-leader
              (setq clobber-count (%p-ldb-offset si:%%array-leader-length 0 localp)
                    skip-count 1)
              (go clobber-loop))
             (si:%header-type-list
              (go x1))
             (si:%header-type-flonum
              (go skip-to-next))
             (si:%header-type-complex
              (%p-store-contents-offset 0 1 localp)
              (%p-store-contents-offset 0 2 localp)
              (incf region-relative-address 3)
              (go l))
             (si:%header-type-bignum
              (go skip-to-next))
             (si:%header-type-rational
              (%p-store-contents-offset 0 1 localp)
              (%p-store-contents-offset 1 1 localp)
              (incf region-relative-address 3))
             ))
          (dtp-array-header
           (let ((long-length (%p-ldb-offset si:%%array-long-length-flag 0 localp))
                 (displaced (%p-ldb-offset si:%%array-displaced-bit 0 localp))
                 (n-dims (%p-ldb-offset si:%%array-number-dimensions 0 localp))
                 ;(type (%p-ldb-offset si:%%array-type-field 0 localp))
                 )
             (cond ((not (zerop displaced))
                    (ferror nil "Displaced array in moby space")))
             (incf region-relative-address (+ long-length (max 0 (1- n-dims))))
;            (cond ((memq type
;                         '(#.(find-position-in-list 'art-q array-types)
;                           #.(find-position-in-list 'art-q-list array-types)))
;                   (go clobber-to-next))
;                  (t (go skip-to-next)))
             (go clobber-to-next)))
          (dtp-instance-header
           (go clobber-to-next))
  ;        (let* ((lp (%p-ldb-offset %%q-pointer 0 localp))
  ;               (size (%p-ldb-offset %%q-pointer lp 1)))
  ;            )
          (dtp-header-forward )
          (dtp-unreconciled ))
   e1   (%p-store-contents-offset nil 0 localp)
   x1   (incf region-relative-address)
        (go l)
   clobber-loop
        (cond ((zerop clobber-count)
               (incf region-relative-address skip-count)
               (go l)))
        (%p-store-contents-offset nil 0
         (%pointer-plus region-origin region-relative-address))
        (setq clobber-count (1- clobber-count))
        (incf region-relative-address)
        (go clobber-loop)
   skip-to-next         ;skip to next frob with starts-object set.
        (incf region-relative-address)
        (cond ((not (< region-relative-address region-free-pointer))
               (return t)))
        (setq moby-bits (aref moby-bits-array region-relative-address))
        (cond ((or (zerop (ldb %%%moby-region-valid moby-bits))
                   (not (zerop (ldb %%%moby-starts-object moby-bits))))
               (go l)))
        (go skip-to-next)
   clobber-to-next              ;clobber everything to next frob with starts-object set.
        (incf region-relative-address)
        (cond ((not (< region-relative-address region-free-pointer))
               (return t)))
        (setq moby-bits (aref moby-bits-array region-relative-address)
              localp (%pointer-plus region-origin region-relative-address))
        (cond ((or (zerop (ldb %%%moby-region-valid moby-bits))
                   (not (zerop (ldb %%%moby-starts-object moby-bits))))
               (go l)))
        (%p-store-contents-offset nil 0
         (%pointer-plus region-origin region-relative-address))
        (go clobber-to-next)
        )))

(defun moby-unhook-area (area-number)
  (si:for-every-region-in-area (region area-number)
    (moby-unhook-region region))
  (let ((msa (aref *area-to-msa* area-number)))
    (setf (aref *area-to-msa* area-number) nil)
    (setf *moby-section-area-and-region-associations*
          (delq msa *moby-section-area-and-region-associations*)))
  )

(defun moby-unhook-region (region)
  (setf (aref *region-to-msa* region) nil)
  (setf (aref *region-to-region-map* region) nil)
  (setf (aref *region-fresh-cons-boundary* region) nil)
  (setf (aref #'sys:region-moby-bits-array region) nil)
  (setf (aref #'sys:region-namespace-origin region) nil)
  )

(defun moby-smash-area-to-non-moby (area-number)
  (without-interrupts
    (gc:without-scavenging
      (gc:without-flipping
        (si:for-every-region-in-area (region area-number)
          (setf (ldb si:%%region-space-type (aref #'si:region-bits region)) si:%region-space-new))
        (setf (ldb si:%%region-space-type (aref #'si:area-region-bits area-number)) si:%region-space-new)
        ))))



(defun moby-select-base-namespace-page ()
  (selector si:local-host-name equal
    ("LMI-GUINEA-PIG"  1_32.)
    ("LMI-LAMBDA-2"  1_16.)     ;OLD DEFAULT THING
    ("LMI-ALEX" (* 3 1_32.))
    ("LMI-HARPO" (* 4 1_32.))
    ("LMI-MOE" (* 5 1_32.))
    ("LMI-CURLEY" (* 6 1_32.))
    ("LMI-LARRY" (* 7 1_32))
    (T  (* 2 1_32.))))

(defun moby-select-partition-host-name ()
  (selector si:local-host-name equal
   ("LMI-GUINEA-PIG" "PIGMBY")
   ("LMI-LAMBDA-2"   "MOBY")    ;old default thing
   ("LMI-ALEX" "M-ALEX")
   ("LMI-HARPO" "M-HARPO")
   ("LMI-MOE" "M-MOE")
   ("LMI-CURLEY" "M-CURLEY")
   ("LMI-LARRY" "M-LARRY")
   (t   "XXXMBY")))

;; End.
