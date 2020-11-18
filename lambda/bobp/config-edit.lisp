;;; -*- Mode:LISP; Package:SDU; Base:10; Readtable:ZL -*-

;; define accessors and names when flavor is instantiated.

;; add ":check" funcs to accessors, in particular for string values.
;;   parts: up-case, 4 chars, "xxxx" ok.

;; more-breaks are random now; mouse is ignored until more is finished.
;; fix more-p, including redisplay randomness.  How to do second screen?
;; don't let it reach bottom; reset redisplay list and clear screen for next page?
;; catch more-break and reset redisplay list.
;; catch more-break and keep redisplay list for each line instead of each top-level func.

;; print sysconf and procconf stuff nicer:
;;   tab over just far enough so value column is even
;; only draw boxes around actual values
;; print some kind of header for sys-conf, procconf: file vs. core, slot number etc.
;;   in-core procconf 0, lmi-lambda in slot 11.

;; spread out option struct printout

;; "exit" :deselect should make it most-recently-selected.

;; warn that some stuff e.g. memory size really shouldn't be changed,
;; or that 1. requires reboot, 2. requires config/reboot, 3. add code to DTRT.

;; keep functions to print config file components
;; so they still can be called without instantiating
;; a config-edit-window.

;; print-from-list vs. in-line for cf-header , cf-slot, option stuff...

;; uggh ... slot-stuff and options is only useful if changes
;; are propagated into sysconf, both in config file image and
;; in core.

;; one possibility is for accessor to have property to update
;; file and/or in-core sysconf; if only for certain entries.

;;; Copyright LISP Machine, Inc. 1986
;;;   See filename "Copyright.Text" for
;;; licensing and release information.

; Bobp
; read and print sdu config file
; edit config file and in-core config structures
;
; requires unix-fs.lisp and c-funcs.lisp (now part of lambda-diag)
;
; (edit-config-file)
;    sets up mouse-sensitive display of config file
;
; (print-config-file)
;    prints most useful info from config file.
;
; (get-list-of-boards)
;    returns a list of the per-slot structures for all nubus slots.
;    use the per-slot defstruct to access them.
;
; (all-disabled-memory-boards)
;    returns a list of the disabled memory boards
;       each element of list is a list of (slot-number board-type)
;    see board-type-qs for the board types.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; per-slot structure in "resource map" section of config file
;; see /usr/86include/sys/lmi-config.h

;; art-8b image of the file
(defvar config-image nil)

(defvar n-slots 16.)

(defvar *config-alist* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile load eval)
(defun cf-assign-values (qs)
  (loop for q in qs
        for i from 0
        when q do
          (set q i)
          (putprop q t 'special)))
 )

; board names: position in list is board-type value
(defconst board-type-qs
          '(unknown none lmi-lambda mc68000 sdu vcmem half-meg-memory
             ti-two-meg-memory medium-res-color bus-coupler
             ti-eight-meg-memory four-meg-memory sixteen-meg-memory
             quad-video nil eight-meg-memory nil twelve-meg-memory))
(cf-assign-values board-type-qs)

(defprop half-meg-memory 512. :memory-size)
(defprop two-meg-memory 2048. :memory-size)
(defprop ti-eight-meg-memory 8192. :memory-size)
(defprop lmi-four-meg-memory 4096. :memory-size)
(defprop lmi-sixteen-meg-memory 16380. :memory-size)
(defprop lmi-eight-meg-memory 8192. :memory-size)
(defprop lmi-twelve-meg-memory 12288. :memory-size)

(defun mem-board-p (ar)
  "return size in 1k pages if memory board, nil if not"
  (get (cf-slot-board-type ar) :memory-size))

(defconst mem-boards
          (loop for l in board-type-qs
                when (get l :memory-size)
                   collect l and do (putprop l 'print-memory-options :option-func)))

(defprop mc68000 print-68000-options :option-func)
(defprop lmi-lambda print-lambda-options :option-func)
(defprop vcmem print-vcmem-options :option-func)
(defprop quad-video print-quad-options :option-func)
(defprop sdu print-sdu-options :option-func)

(defprop lmi-lambda t :is-processor)
(defprop mc68000 t :is-processor)
(defprop sdu t :is-processor)

;;;;;;;;;;;;;;;;

(defconst console-type-qs '(sdu-serial-port-a
                             vcmem-screen
                             quad-video-screen
                             share-tty
                             sdu-serial-port-b
                             quad-video-four-screen-color
                             quad-video-serial-port))
(cf-assign-values console-type-qs)

(defprop vcmem 1 :number-of-screens)
(putprop 'vcmem vcmem-screen :console-type)
(defprop quad-video 4 :number-of-screens)
(putprop 'quad-video quad-video-screen :console-type)

(defvar y-n-qs '(no yes))
(cf-assign-values y-n-qs)

;;;;;;;;;;;;;;;;

(defvar ac-offset)

(eval-when (compile load eval)
(defun make-byte-accessor-forms (sym-def)
  (let* ((sym (car sym-def))
         (set-sym (intern (string-append "SET-" sym) (symbol-package sym)))
         (size 4)
         (offs ac-offset)
         (mult (or (get sym-def :repeat) size))
         args)
    (when (or (null (cadr sym-def)) (numberp (cadr sym-def)))
      (setq offs (or (cadr sym-def) offs)
            size (or (caddr sym-def) size)))
    (setq ac-offset (+ offs size))

    (do ((p (cdr sym-def) (cddr p)))
        ((null p))
      (cond
        ((and (symbolp (car p)) (car p))
         (putprop sym (cadr p) (car p)))))

    (cond
      ((get sym-def :repeat)
       (setq offs `(+ ,offs (* i ,mult)))
       (setq args `(i))))

    (selectq (get sym-def :type)
      (:string
       `((declare (special ,sym))
         (defun ,sym (ar ,@args)
           (get-string ar ,offs ,size))
         (defun ,set-sym (ar ,@args val)
           (set-string ar ,offs ,size val))
         (defsetf ,sym ,set-sym)))
      (:choice
       `((declare (special ,sym))
         (defun ,sym (ar ,@args)
           (get-choice ar ,offs ,size (get ',sym :choice-list)))
         (defun ,set-sym (ar ,@args val)
           (set-bytes ar ,offs ,size val))
         (defsetf ,sym ,set-sym)))
      (t
       `((declare (special ,sym))
         (defun ,sym (ar ,@args)
           (get-bytes ar ,offs ,size))
         (defun ,set-sym (ar ,@args val)
           (set-bytes ar ,offs ,size val))
         (defsetf ,sym ,set-sym))))))
)

(defun get-choice (ar offs size choices)
  (let ((n (get-bytes ar offs size)))
    (or (nth n (symeval choices)) n)))

(defun get-bit-list (ar accessor)
  (let ((w (funcall accessor ar))
        (bits (symeval (get accessor :bits))))
    (loop for b in bits
          when (plusp (ldb (symeval b) w))
          collect (or (get b :name) b))))

(defun get-bytes (ar offs size)
  (loop for i from 0 below size
        for b from 0 by 8
        sum (ash (aref ar (+ i offs)) b)))

(defun set-bytes (ar offs size new)
  (loop for i from 0 below size
        for b from 0 by 8
        do (setf (aref ar (+ i offs)) (ldb (byte 8 b) new)))
  new)

(defun get-string (ar offs ignore)
  (ascii-string (c-str-copy ar offs)))  ;; size

(defun set-string (ar offs size new)
  (copy-array-portion new 0 (string-length new) ar offs (+ offs size)))

(defun set-up-names (qs &optional (f #'(lambda (x) x)))
  (let ((base-len
          (loop for q in qs
                for diff = (abs (string-compare (funcall f q) (funcall f (car qs))))
                when (plusp diff) minimize diff)))
    (loop for q in qs
          do (putprop (funcall f q)
                      (string-subst-char #/space #/- (substring (funcall f q) (1- base-len)))
                      :name))))

(defmacro make-byte-accessors (qs)
  `(progn
     (set-up-names ,qs #'car)
     ,@(let ((ac-offset 0))
         (loop for q in (eval qs)
               append (make-byte-accessor-forms q)))))

(defun get-radix (q)
  (or (get q :radix)
      (cond
        ((get q :decimal)
         10.)
        ((get q :nubus-physical-adr)
         16.)
        ((get q 'si:system-constant)
         8)
        (t
         10.))))

;;;;;;;;;;;;;;;;

;; byte-fields for console type descriptor word in sysconfig and option structures
(defconst vcs-slot-number (byte 8 0))           ;slot number of board
(defconst vcs-type (byte 8 8))                  ;vcmem, quad, serial etc.
(defconst vcs-screen-number (byte 8 16))        ;screen number, for quad
(defconst vcs-hi-byte (byte 8 24))              ;#xff if no board, 0=port, 1=land

(defun vcs-present-p (vcm-slot)
  (not (or (zerop vcm-slot)
           (= #xff (ldb vcs-hi-byte vcm-slot)))))

;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;

; radix: default base to print and edit in
; verbose: only print in verbose mode
; type: string or screen; default is number
;   string: data is converted to/from a string array
;   screen: data is a number, but print and edit as a vcm_slot structure

; accessors for config file structures

;; config-file header
;; starts at 0 in file
(defconst cf-header-qs
          `((cf-header-version 0 4)
             (cf-header-bootable 4 4)
             (cf-header-whole-shared-area-address 8 4 :radix 16)
             (cf-header-whole-shared-area-size 12 4 :type :memory-size)
             (cf-header-sys-config-address 16 4 :radix 16)
             (cf-header-sys-config-size 20 4 :type :memory-size)
             (cf-header-slot-array-file-offset 24 4)
             (cf-header-slot-array-per-slot-size 28 4)
             (cf-header-sys-config-file-offset 32 4)
             (cf-header-slot-map-file-offset 36 4)
             (cf-header-slot-map-size 40 4)))
(make-byte-accessors cf-header-qs)

;; per-slot structure in config file
;; starts at (cf-header-slot-array-file-offset)
(defconst cf-slot-qs
          `((cf-slot-board-type 0 2 :type :choice :choice-list board-type-qs)
             (cf-slot-disabled 2 2 :type :choice :choice-list y-n-qs)
             (cf-slot-slot-number 4 2)
             (cf-slot-assigned-memory-size 12 4 :type :memory-size)
             (cf-slot-option-struct-file-offset 16 4)
             (cf-slot-option-size 20 4)
             (cf-slot-major-version 24 2)
             (cf-slot-minor-version 28 2)))
(make-byte-accessors cf-slot-qs)

;; mc68000 option struct
(defconst unix-opt-qs
          `((unix-opt-screen 0 4 :type :screen)
            (unix-opt-devmap-size 4 4)
;;          (unix-opt-bootcons-size 8 4)
;;          (unix-opt-devmap 12 748)
;;          (unix-opt-boot-console 760 20 :type :string)
            (unix-opt-number-of-sharettys 780 2)
            (unix-opt-console-type 782 2 :type :choice :choice-list console-type-qs)
            (unix-opt-multibus-map-size 784 2)
            (unix-opt-console-baud-rate 786 2)))
(make-byte-accessors unix-opt-qs)

;; lambda parity enable bits in procconf and lambda options
(defconst lam-parity-qs '(mi-parity cm-parity dp-parity mid-parity treg-parity))
(set-up-names lam-parity-qs)
(set-up-names si:lambda-processor-switches-bits-symbols)

(assign-alternate `(mi-parity ,(byte 1 0)
                     cm-parity ,(byte 1 1)
                     dp-parity ,(byte 1 2)
                     mid-parity ,(byte 1 3)
                     treg-parity ,(byte 1 4)))

;; lambda option struct
(defconst cf-lambda-qs
          `((lam-opt-source-cycles 0 2)
            (lam-opt-exec-cycles 4 2)
            (lam-opt-screen 8 4 :type :screen)
            (lam-opt-processor-switches 12 4 :radix 8 :type :switch
                                        :bits si:lambda-processor-switches-bits-symbols)
            (lam-opt-timing-ram-file 20 60 :type :string)
            (lam-opt-microcode-partition 80 6 :type :string :check check-part-string)
            (lam-opt-load-partition 86 6 :type :string :check check-part-string)
            (lam-opt-page-partition 92 6 :type :string :check check-part-string)
            (lam-opt-file-partition 98 6 :type :string :check check-part-string)
            (lam-opt-base-multibus-map-reg 104 4)
            (lam-opt-parity-enables 108 4 :radix 8 :type :switch :bits lam-parity-qs)
            (lam-opt-multibus-map-size 112 2)
            (lam-opt-scan-line-size 114 2)))
(make-byte-accessors cf-lambda-qs)

;; vcmem and quad-video option struct
(defconst cf-vcmem-qs
          `((vcm-opt-size nil 4)
            (vcm-opt-location nil 80 :repeat 80 :type :string)))
(make-byte-accessors cf-vcmem-qs)

;; SDU option structure in config file
(defconst cf-sdu-qs
  `((sdu-opt-newboot-code-size 0 4 :type :memory-size)
    (sdu-opt-user-area-size 4 4 :type :memory-size)
    (sdu-opt-user-map-size 8 4)))
(make-byte-accessors cf-sdu-qs)

;; memory board option struct
(defconst cf-bad-mem-qs
  `((mem-opt-list-size 0 2)
    (mem-opt-number-of-bad-sections 2 2)
    (mem-opt-bad-mem-addr 12 4 :repeat 8 :radix 16)
    (mem-opt-bad-mem-size 16 4 :repeat 8 :type :memory-size)))
(make-byte-accessors cf-bad-mem-qs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defconst cf-command-alist
          '(("Read File" :value :read-config-file
             :documentation "Reread config file from disk")
            ("Write File" :value :write-config-file
             :documentation "Write current state to config file")
            ("Exit" :value :exit
             :documentation "Exit")
            ("Print All" :value :print-all
             :documentation "Print contents of config file")
            ("Edit Header" :value :edit-header
             :documentation "Edit config-file header")
            ("Edit Slot" :value :edit-slot
             :documentation "Select a slot and edit its contents")
            ("Edit File Sysconf" :value :edit-file-sysconf
             :documentation "Edit sys-conf image in config-file")
            ("Edit In-Core Sysconf" :value :edit-in-core-sysconf
             :documentation "Edit in-core sys-conf image")
            ("Edit File Procconf" :value :edit-file-procconf
             :documentation "Select and edit a proc-conf image in config-file")
            ("Edit In-Core Procconf" :value :edit-in-core-procconf
             :documentation "Select and edit an in-core proc-conf image")
            ))

;;;;;;;;;;;;;;;;

(defvar w nil)
(defvar cfwin nil)
(defvar tracewin nil)

(defvar redisplay nil)

(defflavor cf-window
         ()
         (tv:basic-mouse-sensitive-items
          tv:window)
  :gettable-instance-variables
  :settable-instance-variables
  :inittable-instance-variables
  :special-instance-variables)

(defflavor config-edit-window
         (cmdwin
          cfwin
          tracewin)
         (tv:alias-for-inferiors-mixin
          tv:inferiors-not-in-select-menu-mixin
          tv:process-mixin
          tv:bordered-constraint-frame-with-shared-io-buffer)
  (:default-init-plist
    :process t
    :panes `((command-pane tv:command-menu
                           :save-bits t
                           :item-list ,cf-command-alist)
             (edit-pane cf-window
                        :blinker-p nil
                        :label nil ;;"Config File Editor"
                        :more-p t
                        :save-bits t)
             (title-pane tv:window
                         :blinker-p nil
                         :label (:string "Configuration Structure Editor"
                                 :font fonts:bigfnt
                                 :centered)
                         :save-bits t)
             )
    :constraints '((default (title-pane command-pane edit-pane) ;; trace-pane)
                            ((title-pane 1 :lines))
                            ((command-pane :ask :pane-size))
                            ;;(trace-pane 30 :lines))
                            ((edit-pane :even))))
    :name "Configuration Structure Editor"
    )
  :settable-instance-variables
  :special-instance-variables)

(defmethod (config-edit-window :after :init) (&rest ignore)
  (funcall-self :set-selection-substitute (funcall-self :get-pane 'edit-pane)))

(tv:add-system-key #/lambda 'config-edit-window "System-Configuration Editor")

;;;;;;;;;;;;;;;;

(defmethod (config-edit-window :process-top-level) ()
  (let ((*package* (find-package "SDU")))
    (setq w self)
    (setq cfwin (send self :get-pane 'edit-pane))
    (setq tracewin (send self :get-pane 'trace-pane))

;;  (let ((cmdwin (send self :get-pane 'command-pane)))
;;    (send cmdwin :set-item-list cf-command-alist))
    (send cfwin :set-item-type-alist *config-alist*)

    (unless config-image
      (read-config-file))
    (let ((*terminal-io* cfwin))
      (cf-loop cfwin))))

(defun cf-loop (w)
  (do-forever
    (let ((blip (send w :any-tyi)))
      ;;(format w "~&~s" blip)
      (typecase blip
        (list
         (selectq (car blip)
           (:typeout-execute                    ;;(:typeout-execute func args)
            (when (apply (cadr blip) (caddr blip))
              (loop for r in (nreverse redisplay)
                (apply w r))))
           (:menu                               ;;(:menu item-list mouse-mask command-pane)
            (send w (get (cadr blip) :value)))
           ))
        ))
    ))

;;;;;;;;;;;;;;;;

(defmethod (cf-window :after :clear-screen) ()
  (setq redisplay nil))


(defmethod (cf-window :read-config-file) ()
  (read-config-file)
  (send self :print-all))

(defmethod (cf-window :write-config-file) ()
  (when (tv:mouse-y-or-n-p "Write config file")
    (write-config-file)))

(defmethod (cf-window :exit) ()
  (send self :deselect))

(defmethod (cf-window :print-all) ()
  (send self :clear-screen)
  (print-config-header self)
  (format self "~2&")
  (loop for ar in (make-slot-list)
        do (print-one-slot self ar))
  (push `(:print-all) redisplay))

(defmethod (cf-window :edit-header) ()
  (send self :clear-screen)
  (print-config-header self)
  (push `(:edit-header) redisplay))

(defmethod (cf-window :edit-slot) ()
  (let ((ar (tv:menu-choose (make-slot-alist))))
    (when ar
      (send self :print-slot ar))))

(defmethod (cf-window :print-slot) (ar)
  (send self :clear-screen)
  (print-one-slot self ar)
  (push `(:print-slot ,ar) redisplay))


;; print sysconf from file
(defmethod (cf-window :edit-file-sysconf) ()
  (send self :print-sys-conf (sys-conf-array-16b)))

;; print in-core sysconf
(defmethod (cf-window :edit-in-core-sysconf) ()
  (send self :print-sys-conf si:*sys-conf*))

(defmethod (cf-window :print-sys-conf) (ar)
  (send self :clear-screen)
  (loop for q in si:system-configuration-qs
        do (send self :print-sysconf-entry q ar))
  (push `(:print-sys-conf ,ar) redisplay))


(defmethod (cf-window :edit-file-procconf) ()
  (let ((pa (tv:menu-choose (make-procconf-alist (sys-conf-array-16b))
                            "Choose In-File Procconf")))
    (when pa
      (send self :print-proc-conf pa))))

(defmethod (cf-window :edit-in-core-procconf) ()
  (let ((pa (tv:menu-choose (make-procconf-alist si:*sys-conf*)
                            "Choose In-Core Procconf")))
    (when pa
      (send self :print-proc-conf pa))))



(defmethod (cf-window :print-proc-conf) (ar)
  (send self :clear-screen)
  (loop for q in si:processor-configuration-qs
        do (send self :print-sysconf-entry q ar))
  (push `(:print-proc-conf ,ar) redisplay))

(defun make-slot-list ()
  (loop for i from 0 below n-slots
        collect (slot-array i)))

(defun make-slot-alist ()
  (loop for ar in (make-slot-list)
        collect `(,(format nil "Slot ~d: ~@(~a~)"
                           (cf-slot-slot-number ar)
                           (cf-slot-board-type ar))
                  ,ar)))

;;;;;;;;;;;;;;;;

;(defprop %processor-conf-sys-conf-ptr t :nubus-physical-adr)
;(defprop %processor-conf-vcmem-slot t :screen-device)
;(defprop %processor-conf-micro-band  t :four-byte-ascii)
;(defprop %system-configuration-ethernet-owner t :device-owner)
;(defprop %system-configuration-sdu-rom-version-number t :decimal)

(set-up-names si:system-configuration-qs)
(set-up-names si:processor-configuration-qs)

;; set up so 'choose-value works here
(defprop si:%processor-conf-console console-type-qs :choice-list)

;; set up so 'switches works here
(defprop si:%processor-conf-starting-processor-switches :switch :type)
(defprop si:%processor-conf-starting-processor-switches
         si:lambda-processor-switches-bits-symbols :bits)

(defprop si:%processor-conf-parity-enables :switch :type)
(defprop si:%processor-conf-parity-enables
         lam-parity-qs :bits)

(defmethod (cf-window :print-sysconf-entry) (q ar)
  (format self "~&~@(~a~)~36t" (or (get q :name) q))
  (let ((v (funcall q ar)))
    (cond-every
      ((get q :four-byte-ascii)
       (send self :item 'string (list ar q)
             "~s " (word-to-string v)))
      ((get q :device-owner)
       (send self :item 'device-owner (list ar q)
             "~@(~a~) " (device-owner-string v)))
      ((eq (get q :type) :switch)
       (send self :item 'switches (list ar q)
             "~a  " (fancy-print-in-base v (get-radix q))))
      (otherwise
       (send self :item 'number (list ar q)
             "~a  " (fancy-print-in-base v (get-radix q)))))
    (cond
      ((get q :choice-list)
       (send self :item 'choose-value (list ar q)
             "/"~@(~s~)/" " (nth (ldb vcs-slot-number v) console-type-qs)))
      ((get q :screen-device)
       (send self :item 'screen (list ar q)
             "~s " (screen-slot-string v)))
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-config-file ()
  "read and print contents of sdu config file"
  (read-config-file)
  (print-config-header *standard-output*)
  (format t "~2&")
  (loop for ar in (make-slot-list)
        do (print-one-slot *standard-output* ar)))

(defun print-config-header (w)
  (print-from-list w cf-header-qs config-image))

(defun get-config-file (op)
  (let ((file (open-unix-file "//sdu//lambda//shr-config.1")))
    (when (null config-image)
      (if (neq op :read)
          (ferror nil "Attemp to write config file with garbage"))
      (setq config-image (make-array (unix-file-size file) :type :art-8b)))
    (rw-file op file config-image (unix-file-size file)))
  config-image)

(defun read-config-file ()
  (get-config-file :read))

(defun write-config-file ()
  (get-config-file :write))

;;;;;;;;;;;;;;;;

;; make indir array for a slot array
(defun slot-array (slot)
  "given a slot number (0..15), returns an array for that slot structure"
  (let ((offs (cf-header-slot-array-file-offset config-image))
        (size (cf-header-slot-array-per-slot-size config-image)))
    (make-array size
                :type :art-8b
                :displaced-to config-image
                :displaced-index-offset (+ offs (* slot size)))))

;; make indir array for the option structure for a slot array
(defun option-image (ar)
  "given a slot array, returns that slot's option array"
  (make-array (cf-slot-option-size ar)
              :type :art-8b
              :displaced-to config-image
              :displaced-index-offset (cf-slot-option-struct-file-offset ar)))

;;;;;;;;;;;;;;;;

;; make indir array-16b for whole sys-conf / procconf image in config file
(defun sys-conf-image-16b ()
  (let ((offs (ash (cf-header-sys-config-file-offset config-image) -1))
        (size (cf-header-sys-config-size config-image)))
    (make-array size
                :type :art-16b
                :displaced-to config-image
                :displaced-index-offset offs)))

;; make indir array-16b just for sys-conf image in config-file
(defun sys-conf-array-16b ()
  (let ((offs (ash (cf-header-sys-config-file-offset config-image) -1))
        (size (ash (si:%system-configuration-size (sys-conf-image-16b)) 1)))
    (make-array size
                :type :art-16b
                :displaced-to config-image
                :displaced-index-offset offs)))

;; make indir array-16b for one proc-conf image in config-file
(defun proc-conf-array-16b (index)
  (let* ((size (ash (si:%system-configuration-processor-block-size (sys-conf-image-16b)) 1))
         (offs (+ (ash (cf-header-sys-config-file-offset config-image) -1)      ;sysconf offs
                  (ash (si:%system-configuration-size (sys-conf-image-16b)) 1)  ;sysconf size
                  (* index size))))     ;procconf size
    (make-array size
                :type :art-16b
                :displaced-to config-image
                :displaced-index-offset offs)))

;; choose a processor and return a procconf array-16b
;; loop over procconfs and print menu item "Slot ~d, procconf ~d: ~a"
;; arg is sys-conf array-16b
(defun make-procconf-alist (sy)
  (loop for i from 0 below (si:%system-configuration-number-of-processors sy)
        for pa = (proc-conf-array-16b i)
        for slot = (si:%processor-conf-slot-number pa)
        collect `(,(format nil "Slot ~d, procconf ~d: ~@(~a~)"
                           slot
                           i
                           (cf-slot-board-type (slot-array slot)))
                  :value
                  ,pa)))

;;;;;;;;;;;;;;;;

(defun get-list-of-boards ()
  "return a list of slot arrays for slots that contain boards"
  (read-config-file)
  (loop for slot from 0 below n-slots
        for ar = (slot-array slot)
        when (neq (cf-slot-board-type ar) 'none)
          collect ar))

; top level function for memory diagnostic

(defun all-disabled-mem-boards ()
  "return list of lists of car slot-number, cdr board-type symbol, for disabled memory boards"
  (read-config-file)
  (loop for slot from 0 below n-slots
        for ar = (slot-array slot)
        when (and (eq 'yes (cf-slot-disabled ar))
                  (mem-board-p ar))
        collect (cons (cf-slot-slot-number ar) (cf-slot-board-type ar))))

;;;;;;;;;;;;;;;;

(defun print-from-list (w qs ar)
  (let ((len (loop for q in qs
                   maximize (string-length (get (car q) :name)))))
    (dolist (q qs)
      (format w "~&")
      (send w :item 'number (list ar (car q))
            "~@(~Va~) ~a"
            (+ 2 len)
            (get (car q) :name)
            (fancy-print-in-base (funcall (car q) ar) (get-radix q))
            ))))

;; maybe try printing multiple times if b is a list of radices.
(defun fancy-print-in-base (v b)
  (selectq b
    (16.
     (format nil "#x~x" v))
    (10.
     (format nil "~d." v))
    (8
     (format nil "#o~o" v))
    (t
     (format nil "#~dr~Vr" b b v))
    ))

;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;

(defun mem-size-to-print (n)
  (if (= #xffffffff n)
      nil
    (format nil "~d KB" (ash n -10))))

(defun print-one-slot (w ar)
  (let ((board-type (cf-slot-board-type ar)))
    (format w "~&Slot ~d: " (cf-slot-slot-number ar))
    (send w :item 'choose-value (list ar 'cf-slot-board-type)
          "~@(~s~), " (cf-slot-board-type ar))
    (when (neq board-type 'none)
      (send w :item 'choose-value (list ar 'cf-slot-disabled)
            "Disabled: ~@(~a~), " (cf-slot-disabled ar))
      (send w :item 'number (list ar 'cf-slot-major-version)
            "Version: ~d" (cf-slot-major-version ar))
      (send w :item 'number (list ar 'cf-slot-minor-version)
            ".~d, " (cf-slot-minor-version ar))

      (format w "~&")
      (when (get board-type :is-processor)
        (send w :item 'memory-size (list ar 'cf-slot-assigned-memory-size)
              "~8tMemory size: ~a, " (mem-size-to-print (cf-slot-assigned-memory-size ar))))

      (when (or (get board-type :is-processor)
                (not (zerop (cf-slot-option-struct-file-offset ar))))
        (send w :item 'number (list ar 'cf-slot-option-struct-file-offset)
              "Option offset: ~d., " (cf-slot-option-struct-file-offset ar))
        (when (not (zerop (cf-slot-option-struct-file-offset ar)))
          (send w :item 'number (list ar 'cf-slot-option-size)
                "size: ~d., " (cf-slot-option-size ar))))

      (format w "~&")
      (let ((opt-func (get board-type :option-func)))
        (when (and opt-func
                   (not (zerop (cf-slot-option-struct-file-offset ar))))
          (funcall opt-func w ar)))
    )))

;;;;;;;;;;;;;;;;

(defun print-68000-options (w ar)
  (let ((oa (option-image ar)))
    (format w "~&~16t")
    (send w :item 'screen (list oa 'unix-opt-screen)
          "Screen: ~a, " (screen-slot-string (unix-opt-screen oa)))
    (send w :item 'number (list oa 'unix-opt-devmap-size)
          "Devmap size: ~d., " (unix-opt-devmap-size oa))
    (format w "~&~16t")
    (send w :item 'number (list oa 'unix-opt-multibus-map-size)
          "Multibus map size: ~d., " (unix-opt-multibus-map-size oa))
    (send w :item 'choose-value (list oa 'unix-opt-console-type)
          "Console: ~@(~a~), " (unix-opt-console-type oa))))

;;;;;;;;;;;;;;;;

(defun print-lambda-options (w ar)
  (let ((op (option-image ar)))
    (format w "~&~16t")
    (send w :item 'string (list op 'lam-opt-microcode-partition)
          "Microload: ~s, " (lam-opt-microcode-partition op))
    (send w :item 'string (list op 'lam-opt-load-partition)
          "Band: ~s, " (lam-opt-load-partition op))
    (send w :item 'string (list op 'lam-opt-page-partition)
          "Page: ~s, " (lam-opt-page-partition op))
    (send w :item 'string (list op 'lam-opt-file-partition)
          "File: ~s, " (lam-opt-file-partition op))
    (format w "~&~16t")
    (send w :item 'screen (list op 'lam-opt-screen)
          "Screen: ~a, " (screen-slot-string (lam-opt-screen op)))
    (send w :item 'number (list op 'lam-opt-source-cycles)
          "Speed: ~d-" (lam-opt-source-cycles op))
    (send w :item 'number (list op 'lam-opt-exec-cycles)
          "~d, " (lam-opt-exec-cycles op))
    (format w "~&~16t")
    (send w :item 'string (list op 'lam-opt-timing-ram-file)
          "Timing-ram file: /"~a/", " (lam-opt-timing-ram-file op))
    (format w "~&~16t")
    (send w :item 'number (list op 'lam-opt-base-multibus-map-reg)
          "Base map reg: ~d., " (lam-opt-base-multibus-map-reg op))
    (send w :item 'number (list op 'lam-opt-multibus-map-size)
          "Map size: ~d., " (lam-opt-multibus-map-size op))
    (send w :item 'number (list op 'lam-opt-scan-line-size)
          "Scan line size: ~d., " (lam-opt-scan-line-size op))
    (format w "~&~16t")
    (send w :item 'switches (list op 'lam-opt-processor-switches)
          "Switches: #o~o:" (lam-opt-processor-switches op))
    (format w "~{~&~24t~@(~a~)~}" (get-bit-list op 'lam-opt-processor-switches))
    (format w "~&~16t")
    (send w :item 'switches (list op 'lam-opt-parity-enables)
          "Parity enables: #o~o:" (lam-opt-parity-enables op))
    (format w "~{~&~24t~@(~a~)~}" (get-bit-list op 'lam-opt-parity-enables))
    ))

;;;;;;;;;;;;;;;;

(defun screen-slot-string (vcm-slot)
    (cond ((vcs-present-p vcm-slot)
           (select (ldb vcs-type vcm-slot)
             (vcmem-screen ;;1
              (format nil "Vcmem in slot ~d" (ldb vcs-slot-number vcm-slot)))
             (quad-video-screen ;;2
              (format nil "Screen ~d of quad-video in slot ~d"
                      (ldb vcs-screen-number vcm-slot)
                      (ldb vcs-slot-number vcm-slot)))
             ((0 3 4)
              (format nil "~@(~a~)" (nth (ldb vcs-type vcm-slot) console-type-qs)))
             ))
          (t
           "<none assigned>")))

(defun make-vcm-slot (type slot screen)
  (dpb type vcs-type
       (dpb slot vcs-slot-number
            (dpb screen vcs-screen-number 0))))

;;;;;;;;;;;;;;;;

(defun print-vcmem-options (w ar)
  (let ((oa (option-image ar)))
    (format w "~&~16t")
    (send w :item 'string (list oa 'vcm-opt-location)
          "Location: ~a" (vcm-opt-location oa 0))))

(defun print-quad-options (w ar)
  (let ((oa (option-image ar)))
    (dotimes (i 4)
      (format w "~&~16t")
      (send w :item 'string (list oa 'vcm-opt-location i)
            "Screen ~d location: ~a" i (vcm-opt-location oa i)))))

;;;;;;;;;;;;;;;;

(defun print-sdu-options (w ar)
  (let ((oa (option-image ar)))
    (format w "~&~16t")
    (send w :item 'memory-size (list oa 'sdu-opt-newboot-code-size)
          "Nubus code size: ~a, " (mem-size-to-print (sdu-opt-newboot-code-size oa)))
    (send w :item 'memory-size (list oa 'sdu-opt-user-area-size)
          "User-def area size: ~a, " (mem-size-to-print (sdu-opt-user-area-size oa)))
    (send w :item 'number (list oa 'sdu-opt-user-map-size)
          "User-def map pages: ~d." (sdu-opt-user-map-size oa))))

;;;;;;;;;;;;;;;;

(defun print-memory-options (w ar)
  (let ((op (option-image ar)))
    (format w "~&~16t")
    (send w :item 'number (list op 'mem-opt-list-size)
          "Bad list size: ~d., " (mem-opt-list-size op))
    (send w :item 'number (list op 'mem-opt-number-of-bad-sections)
          "Number of sections: ~d., " (mem-opt-number-of-bad-sections op))
    (dotimes (i (mem-opt-number-of-bad-sections op))
      (format w "~&~24t")
      (send w :item 'number (list op 'mem-opt-bad-mem-addr i)
            "addr=#x~x  " (mem-opt-bad-mem-addr op i))
      (send w :item 'memory-size (list op 'mem-opt-bad-mem-size i)
            "size=~a, " (mem-size-to-print (mem-opt-bad-mem-size op i))))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; item-types:
;;   number, screen, switches, string, console, memory-size, choose-value

(defconst edstring-info "Type  to complete; type  to abort")

(tv:add-typeout-item-type *config-alist* number "Set Value" cf-change-number-dflt t
                          "Change this number")

(tv:add-typeout-item-type *config-alist* number "Hex" cf-change-number-16 nil
                          "Change this number, printed in hex")

(tv:add-typeout-item-type *config-alist* number "Decimal" cf-change-number-10 nil
                          "Change this number, printed in decimal")

(tv:add-typeout-item-type *config-alist* number "Octal" cf-change-number-8 nil
                          "Change this number, printed in octal")

(defun cf-change-number-dflt (ar accessor &rest args)
  (apply 'cf-change-number ar accessor (get-radix accessor) args))

(defun cf-change-number-16 (ar accessor &rest args)
  (apply 'cf-change-number ar accessor 16 args))

(defun cf-change-number-10 (ar accessor &rest args)
  (apply 'cf-change-number ar accessor 10 args))

(defun cf-change-number-8 (ar accessor &rest args)
  (apply 'cf-change-number ar accessor 8 args))

(defun cf-change-number (ar accessor radix &rest args &aux msg)
  (when (eq (get accessor :type) :memory-size)
    (setq msg (or (mem-size-to-print (apply accessor ar args)) "")))
  (do-forever
    (let ((new-string
            (zwei:pop-up-edstring
              (fancy-print-in-base (apply accessor ar args) radix)
              '(:mouse)
              `("Type a number; " ,edstring-info)
              500
              nil
              msg)))
      (cond
        (new-string
         (let ((new (eval (read-from-string new-string nil))))
           (when (numberp new)
             (funcall (get accessor 'si:setf-method) ar new)
             (return-from cf-change-number t))))
        (t
         (return-from cf-change-number nil)))
      (setq msg "Not a number!"))))

;;;;;;;;;;;;;;;;

(tv:add-typeout-item-type *config-alist* memory-size "Change Memory Size" cf-change-number t
                          "Change assigned memory size")

;;;;;;;;;;;;;;;;

(tv:add-typeout-item-type *config-alist* screen "Change screen" cf-change-screen t
                          "Change the selected screen")

(tv:add-typeout-item-type *config-alist* screen "Set Value" cf-change-number nil
                          "Set word to a new value")

(defun cf-change-screen (ar accessor)
  (cf-choose
    ar
    accessor
    (make-screen-alist)))

(defun make-screen-alist ()
  (loop for ar in (make-slot-list)
        append (make-screen-alist-one-board ar)))

(defun make-screen-alist-one-board (ar)
  (let ((board-type (cf-slot-board-type ar)))
    (loop for screen from 0 below (or (get board-type :number-of-screens) 0)
          collect (let ((vcs (make-vcm-slot (get board-type :console-type)
                                            (cf-slot-slot-number ar)
                                            screen)))
                    `(,(screen-slot-string vcs) ,vcs)))))

;;;;;;;;;;;;;;;;

(tv:add-typeout-item-type *config-alist* switches "Change Bits" cf-hack-switch t
                          "Change bits in the word")

(tv:add-typeout-item-type *config-alist* switches "Set Value" cf-change-number nil
                          "Set word to a new value")

(defun cf-hack-switch (ar accessor)
  (catch 'abort
    (let* ((bits (symeval (get accessor :bits)))
           (ca (setup-cvv-array bits (funcall accessor ar))))
      (tv:choose-variable-values
        (make-cvv-alist ca bits)
        :label (format nil "Edit bit-field values for ~:(~a~)" (or (get accessor :name) accessor))
        :margin-choices `("Done" ("Abort" (,#'(lambda () (throw 'abort nil))))))
      (funcall (get accessor 'si:setf-method)
               ar
               (revert-cvv-array ca bits (funcall accessor ar)))
      t)))

(defun setup-cvv-array (bits w)
  (let ((ar (make-array (length bits))))
    (loop for b in bits
          for i from 0
          do (setf (aref ar i) (ldb (symeval b) w)))
    ar))

(defun revert-cvv-array (ar bits w)
  (loop for b in bits
        for i from 0
        do (setq w (dpb (aref ar i) (symeval b) w)))
  w)

(defun make-cvv-alist (ar bits)
  (loop for b in bits
        for i from 0
        collect `(,(locf (aref ar i))
                  ,(format nil "~@(~a~) (~o)" (or (get b :name) b) (symeval b))
                  :number)))

;;;;;;;;;;;;;;;;

(tv:add-typeout-item-type *config-alist* string "Change String" cf-change-string t
                          "Change this string")

(defun cf-change-string (ar accessor &rest args &aux cvt-p)
  (let ((old (apply accessor ar args)))
    (when (numberp old)
      (setq old (word-to-string old)
            cvt-p t))
    (let ((new (zwei:pop-up-edstring
                 old
                 '(:mouse)
                 `(,(format nil "Edit value for ~:(~a~)." (or (get accessor :name) accessor)))
                 500
                 nil
                 edstring-info)))
      (when new
        (when cvt-p
          (setq new (string-to-word new)))
        (apply (get accessor 'si:setf-method) ar (append args `(,new)))
        t))))

;;;;;;;;;;;;;;;;

(tv:add-typeout-item-type *config-alist* choose-value "Choose Value" cf-choose-value t
                          "Choose value from a list")

(tv:add-typeout-item-type *config-alist* choose-value "Set Value" cf-change-number nil
                          "Set value to a number you type")

(defun cf-choose-value (ar accessor)
  (cf-choose
    ar
    accessor
    (make-cv-list (symeval (get accessor :choice-list)))))

(defun make-cv-list (qs)
  (sortcar
    (loop for q in qs
          when q collect `(,(format nil "~@(~a~)" (or (get q :name) q)) ,(symeval q)))
    'string-lessp))

(defun cf-choose (ar accessor choices &optional (cmp-func 'cf-cmp-func))
  (let ((new (tv:menu-choose
               choices
               (format nil "Choose value for ~:(~a~):" (or (get accessor :name) accessor))
               '(:mouse)
               (rass cmp-func (funcall accessor ar) choices))))
    (when new
      (funcall (get accessor 'si:setf-method) ar new))))

(defun cf-cmp-func (a b)
  (when (listp a)
    (setq a (car a)))
  (when (listp b)
    (setq b (car b)))
  (when (and (symbolp a) (boundp a))
    (setq a (symeval a)))
  (when (and (symbolp b) (boundp b))
    (setq b (symeval b)))
  (equal a b))

;;;;;;;;;;;;;;;;

(tv:add-typeout-item-type *config-alist* device-owner "Set Device Owner"
                          cf-set-device-owner t
                          "Change the owner of this device")

(defun cf-set-device-owner (ar accessor)
  (cf-choose
    ar
    accessor
    (make-device-owner-alist)
    'compare-owner))

(defconst device-free 0)
(defconst device-not-present #xffffffff)

(defun compare-owner (a b)
  (when (listp b)
    (setq b (car b)))
  (and (= (ldb (byte 8 24) a)
          (ldb (byte 8 24) b))
       (= (ldb (byte 4 0) a)
          (ldb (byte 4 0) b))))

(defun make-device-owner-alist ()
  (append
    (loop for (name ar) in (make-slot-alist)
          when (get (cf-slot-board-type ar) :is-processor)
            collect `(,name ,(make-device-owner-word (cf-slot-slot-number ar))))
    `((,(device-owner-string device-free) ,device-free))
    `((,(device-owner-string device-not-present) ,device-not-present))))

(defun make-device-owner-word (slot)
  (dpb 1 (byte 1 31) (logand slot #xf)))

(defun device-owner-string (w)
  (select w
    (device-not-present
     "<not present>")
    (device-free
     "<free>")
    (t
     (let ((slot (logand w #xf)))
       (format nil "Owned by ~@(~a~) in slot ~d"
               (cf-slot-board-type (slot-array slot))
               slot)))))

;;;;;;;;;;;;;;;;

(defun string-to-word (s)
  (loop for i from 0 below (string-length s)
        sum (dpb (aref s i) (byte 8 (ash i 3)) 0)))

(defun word-to-string (w)
  (let* ((len (ceiling (haulong w) 8))
         (ar (make-array len :type :art-string)))
    (loop for i from 0 below len
          do (setf (aref ar i) (ldb (byte 8 (ash i 3)) w)))
    ar))

;;;;;;;;;;;;;;;;
