;;; -*- Mode:LISP; Package:SITE-DATA-EDIT; Base:10; Readtable: CL -*-
;;; Copyright (c) Lisp Machine Inc., 1986.

;;; The usual comment goes here...
(defmacro do-plist ((name value plist &optional result) &body body)
  `(do ((.plist. ,plist (cdr .plist.)))
       ((null .plist.) ,result)
     (setq ,name (car .plist.) ,value (cadr .plist.))
     ,@body))

(defun evaluate-safely (form)
  (condition-case (result) (eval form t)
    (error
     (format *error-output* "~&Error while evaluating ~S: ~A" form result))))

(defmethod (attribute :value-as-site-attribute) ()
  (send self :value))

(defmethod (attribute :attribute-symbol) ()
  (or key (type-of self)))

;;; Some random site-data holders

(defvar *site-option-alist* :unbound
  "What will be contained in si:site-option-alist")

(defvar *machine-location-alist* :unbound
  "What will be contained in si:machine-location-alist")

(defvar *site-name* :unbound
  "What will be contained in si:site-name")

;;; This only gets site-wide options.  Per host things are handled in a different manner.
(defun get-site-wide-option (option &optional (alist *site-option-alist*))
  "Get a site-wide option."
  (cdr (assoc option alist)))

(defun set-site-wide-option (option newvalue)
  (let ((entry (assoc option *site-option-alist*)))
    (if entry
        (setf (cdr entry) newvalue)
      (push (cons option newvalue) *site-option-alist*)))
  newvalue)

(defsetf get-site-wide-option set-site-wide-option)

(defflavor object-choosing-mixin (class (predicate nil) (predicate-args nil)
                                  (any-predicate nil) (any-predicate-args nil)) ()
  (:required-init-keywords :class)
  :inittable-instance-variables
  (:method-combination (:append :base-flavor-last :extra-choices))
  (:required-flavors attribute)
  :abstract-flavor)

(defmethod (object-choosing-mixin :append :copy-init-plist) ()
  (list :class class :predicate predicate :predicate-args predicate-args))

(defmethod (object-choosing-mixin :choose) (&key prompt)
  (let ((prompt (or prompt (send self :prompt-string))))
    (choose-object prompt class (send self :extra-choices prompt)
                   predicate predicate-args)))

(defun make-any-object-choice-item (prompt class predicate predicate-args)
  (flet ((choose-any-object ()
           (choose-object prompt class '() predicate predicate-args)))
    (list (format () "Any ~A" (class-pretty-name class))
          :funcall #'choose-any-object ; the wonders of lexical closures !
          :font :menu-standout)))

;;; The :EXTRA choices are lists of ``extended'' menu items which can also have the following
;;; options:
;;;  KEYBOARD-CHOOSE-FROM-MENU-CHOOSE, value non-NIL means to use the CDR of the value of
;;;   the :MENU-CHOOSE keyword.
;;;  KEYWOARD-CHOOSE-DEFAULT, value is non-NIL if NIL can be returned.
(defmethod (object-choosing-mixin :extra-choices) (prompt)
  (declare (ignore prompt))
  '())

(defmethod (object-choosing-mixin :append :extra-choices) (prompt)
  (list (make-any-object-choice-item prompt class any-predicate any-predicate-args)))

(defun-method verify-object object-choosing-mixin (object)
  (or (null predicate)
      (apply predicate object predicate-args)))

;;; Object reference: a name which refers an object of a certain class; an atomic name.
(defflavor object-reference () (object-choosing-mixin value-variable-mixin attribute))

(defmethod (object-reference :before :init) (plist)
  (declare (ignore plist))
  (when (stringp value)
    (setq value (find-object-of-class class value))))

(defmethod (object-reference :cue-string) ()
  (or (get class 'class-pretty-name) "Object"))

(defmethod (object-reference :append :extra-choices) (prompt)
  (declare (ignore prompt))
  (when (send self :ok-if-void-p)
    (list (cons (send self :default-string)
                '(:value () :font :menu-standout :documentation "The default value"
                         keyboard-choose-default t)))))

;;; Notice that this writes out the full name of the object referred to !
(defmethod (object-reference :output-value) (window)
  (send window :item1 self 'object-reference
        #'(lambda (ignore w -name-) (send w :string-out -name-))
        (send value :name)))

(defmethod (object-reference :and :verify) ()
  (when value (verify-object value)))

(defmethod (object-reference :case :edit object-reference) ()
  (setq value (send self :choose)))

(defmethod (object-reference :value-as-site-attribute) ()
  (and value ; can be NIL, if not referring to any object
       (send value :name)))

(defflavor object-list (elements) (object-choosing-mixin value-method-mixin attribute))

(defmethod (object-list :value-as-site-attribute) ()
  (mapcar #'(lambda (x) (send x :name)) (send self :value)))

(defmethod (object-list :void-p) () (null elements))

(defmethod (object-list :and :verify) ()
  (or (null elements)
      (every #'(lambda (e) (verify-object (second e))) elements)))

(defmethod (object-list :set-value) (list)
  (setq elements (mapcar #'(lambda (object)
                             (list self (if (stringp object)
                                            (find-object-of-class class object)
                                          object)))
                         list)))

(defmethod (object-list :append :copy-init-plist) ()
  (list :class class :predicate predicate :predicate-args predicate-args))

(defmethod (object-list :cue-string) () "List")

(defmethod (object-list :print-value) (stream escape-p)
  (declare (ignore escape-p))
  (cond (elements
         (write-char #\{ stream)
         (do* ((vv elements (cdr vv))
               (v (car vv) (car vv)))
              ((null vv))
           (when (second v)
             (unless (eq vv elements)
               (write-string ", " stream))
             (write-string (send (second v) :name) stream)))
         (write-char #\} stream))
        (t (write-string "empty" stream))))

(defmethod (object-list :output-value) (window)
  (send window :item1 self 'object-list
        #'(lambda (x w)
            (write-char #\{ w) ; you need something to help the user mouse on the whole list
            (send x :output-elements w)
            (write-char #\} w))))

(defmethod (object-list :output-elements) (window)
  (do* ((vv elements (cdr vv))
        (v (car vv) (car vv)))
       ((null vv))
    (when (second v)
      (unless (eq vv elements)
        (send window :tyo #\Space))
      (send window :item1 v 'object-list-element
            #'(lambda (ignore w)
                (send w :string-out (send (second v) :short-name)))))))

(defmethod (object-list :case :documentation-string object-list) ()
  "Click to add to the list")

(defmethod (object-list :case :edit object-list) ()
  (send self :adjoin (send self :choose :prompt "New Element")))

(defmethod (object-list :case :documentation-string object-list-element) ()
  "Click: L: to drag this to the front; M: delete; M2: replace; R: drag to rear")

(defmethod (object-list :case :edit object-list-element) (object)
  (mousecase
    (:l (send self :drag-object-to-front object))
    ((:m 1) (send self :delete-object object))
    (:m (send self :replace-object object (send self :choose :prompt "Replace with")))
    (:r (send self :drag-object-to-rear object))))

(defmacro saving-list ((list) &body body)
  "Save a copy of LIST, setting LIST to value of BODY unless something tries to abort"
  ; Access to .completed. variable needed ??
  `(let ((.completed. nil) (.old-list. (copy-list ,list)))
     (unwind-protect (setq ,list (prog1 (progn ,@body) (setq .completed. t)))
       (unless .completed.
         (setq ,list .old-list.)))))

(defun-method object-list-element object-list (object)
  (rassoc object elements :test #'(lambda (item elt) (eq (car elt) item))))

(defmethod (object-list :drag-object-to-front) (object)
  (let ((e (object-list-element object)))
    (unless (eq (car elements) e)
      (setq elements (cons e (remq e elements))))))

(defmethod (object-list :drag-object-to-rear) (object)
  (let ((tail (last elements))
        (e (object-list-element object)))
    (unless (eq e (car tail))
      (saving-list (elements)
        (setf (cdr tail) (cons e nil)) ; add to tail, which still must be linked
        (delq e elements 1)))))        ; delete from list (just once !)

;;; This is a little dwimoid, since we're supposed to be maintaining an set of elements.
;;; If the new element is in the set, we delete it from where it was.
(defmethod (object-list :replace-object) (old new)
  (unless (eq old new)
    (let ((old-member (send self :member old))
          (new-elt (object-list-element new)))
      (if new-elt
          (saving-list (elements)
            (setf (car old-member) new-elt) ; stick element here
            (delq new-elt elements 1))      ; delete from list (just once !)
        (setf (second (car old-member)) new)))))

;;; These don't do checking
(defmethod (object-list :delete-object) (object)
  (setq elements (delete object elements :key #'second)))

(defmethod (object-list :add-object) (object)
  (unless (member object elements :key #'second)
    (push (list self object) elements)))

(defmethod (object-list :n-objects) ()
  (length elements))

(defmethod (object-list :objects) ()
  (mapcar #'second elements))

(defmethod (object-list :value) ()
  (send self :objects))

;;; This really is supposed to return the tail of elements, so be careful.
(defmethod (object-list :member) (object)
  (member object elements :key #'second))

(defmethod (object-list :adjoin) (object)
  (unless (member object elements :key #'second)
    (push (list self object) elements)))

(compile-flavor-methods object-list object-reference)

;;; Name for self; a name which refers to OBJECT within CLASS and no other.

(defflavor self-name (class object) (atomic-name)
  :inittable-instance-variables)

(defmethod (self-name :append :copy-init-plist) ()
  (list :class class :object object))

(defmethod (self-name :after :init) (ignore)
  (assert (and object class) (object class)))

(defmethod (self-name :pass-on :verify-string) (string)
  (let ((thing (find-object-of-class class string)))
    (if (or (null thing) (eq thing object))
        string
      (progn
        (complain "~A is already a name for ~A." string thing)
        (abort-edit)))))

(defmethod (self-name :and :verify) ()
  (let ((thing (find-object-of-class class value)))
    (or (eq object thing)
        (ferror "Another object is called ~A" value))))

(defflavor nickname () (self-name)
  (:default-init-plist :name "Nickname" :ok-if-void-p t))

(defmethod (nickname :multiple-p) () t)

(defmethod (nickname :case :edit nickname) ()
  (send self :edit 'name))

(defflavor file-system-type () (restricted-keyword)
  (:default-init-plist :restriction 'file-system-type :ok-if-void-p t :name "File System Type"
                       :value nil))

(defmethod (file-system-type :case :edit file-system-type) ()
  (send self :edit 'choice-or-any))

(defmethod (file-system-type :choices) ()
  (append '(("Same as system type" :value nil :font :menu-standout) ("" :no-select t))
          (get-restriction 'file-system-type)))

(defmethod (file-system-type :default-string) () "Like system type")

(add-restrictions 'file-system-type :lmfile :lmfs)

(compile-flavor-methods self-name nickname file-system-type)

;;; Network address: a pair of a network type and an address
(defflavor network-address (addressing-domain address) (value-method-mixin attribute)
  (:default-init-plist :name "Address" :ok-if-void-p t)
  (:init-keywords :value))

(defmethod (network-address :set-value) (list)
  (setf `(,addressing-domain ,address) list))

(defmethod (network-address :print-value) (stream escape-p)
  (write-char #\{ stream)
  (write addressing-domain :stream stream :escape escape-p)
  (when address
    (write-string ", " stream)
    (write-string (net::unparse-address address addressing-domain) stream))
  (write-char #\} stream))

(defmethod (network-address :value) ()
  (list addressing-domain address))

(defmethod (network-address :multiple-p) () t)

(defmethod (network-address :void-p) ()
  (null addressing-domain))

(defmethod (network-address :cue-string) () "Pair")

;;; What could clicking on the whole address do ?
(defmethod (network-address :output-value) (window)
  (let ((x self) (-addressing-domain- addressing-domain) (-address- address))
    (send window :set-current-font cue-font)
    (write-string "Addressing domain " window)
    (send window :set-current-font default-font)
    (send window :item1 x 'network-address-addressing-domain
          #'(lambda (ignore window)
              (send window :string-out (symbol-name -addressing-domain-))))
    (send window :tyo #\Space)
    (send window :set-current-font cue-font)
    (write-string "Address " window)
    (send window :set-current-font default-font)
    (send window :item1 x 'network-address-address
          #'(lambda (ignore window)
              (send window :string-out
                    (if -address- (net::unparse-address -address- -addressing-domain-) "Fill in"))))))

(defun (:property :chaos valid-address-p) (x)
  (typep x '(fixnum #16r100 #16rFFFF)))

(defun (:property :internet valid-address-p) (x)
  (and (typep x '(integer #16r1000000 #16rFFFFFFFF))
       (cond ((= (ldb (byte 1 31) x) 0)
              (not (zerop (ldb (byte 24 0) x))))
             ((= (ldb (byte 2 30) x) 2)
              (not (zerop (ldb (byte 16 0) x))))
             ((= (ldb (byte 3 29) x) 6)
              (not (zerop (ldb (byte 8 0) x))))
             (t nil))))

(defun valid-address-for-addressing-domain-p (addressing-domain address)
  (funcall (or (get addressing-domain 'valid-address-p) #'identity) address))

(defun valid-address-p (address)
  (valid-address-for-addressing-domain-p (car address) (cadr address)))

(defmethod (network-address :case :edit network-address-addressing-domain) ()
  (let ((new (choose-or-read (get-restriction 'addressing-domain) #'string-to-keyword "Addressing Domain")))
    (when address
      (check-shared-addresses new address)
      (unless (valid-address-for-addressing-domain-p new address)
        (format *query-io* "~A is not a valid address for the ~A addressing domain.~%"
                (net::unparse-address address addressing-domain) new)
        (if (fquery '(:beep t :type :tyi :list-choices t :fresh-line t
                            :choices (((nil "Keep addressing domain") #\K) ((t "Erase address") #\E)))
                    "Keep the old addressing-domain (~A) or Erase the current address ? " addressing-domain)
            (setq address nil)
          (abort-edit))))
    (setq addressing-domain new)))

;;; This can be called after clicking on a void address
(defmethod (network-address :case :edit network-address) ()
  (send self :edit 'network-address-addressing-domain)
  (send self :edit 'network-address-address))

(defun query-read-line (prompt)
  (readline *query-io* () `((:prompt ,prompt))))

(defmethod (network-address :case :edit network-address-address) ()
  (if addressing-domain
      (let ((new
              (funcall (if (member addressing-domain (get-restriction 'addressing-domain))
                           (let ((-addressing-domain- addressing-domain)) ; grumble....
                             #'(lambda (string) (net:parse-address string -addressing-domain-)))
                         #'identity)
                       (query-read-line "Address: "))))
        (check-shared-addresses addressing-domain new)
        (setq address new))
    (progn
      (complain "You need to specify a addressing domain first.")
      (abort-edit))))

(defun check-shared-addresses (addressing-domain address)
  (unless (valid-address-for-addressing-domain-p addressing-domain address)
    (format *query-io* "~A is not a valid address for the ~A addressing domain.~%"
            (net::unparse-address address addressing-domain) addressing-domain)
    (abort-edit))
  (let* ((current-host (send *edit-pane* :object))
         (other-hosts (remq current-host (multiple-value-list (get-host-from-address (list addressing-domain address))))))
    (when other-hosts
      (complain "~A already has address of ~A."
                (car other-hosts)
                (net::unparse-address address addressing-domain))
      (abort-edit)))
  (unless (find-network-by-address address addressing-domain)
    (complain "~A address ~A is on network ~A, which is unknown"
              addressing-domain
              (net::unparse-address address addressing-domain)
              (net::unparse-address (funcall (get addressing-domain 'network-number-from-address #'identity) address)
                                    addressing-domain))
    (abort-edit)))

(defun (:property :chaos network-number-from-address) (address)
  (ldb (byte 8 8) address))

(defun (:property :internet network-number-from-address) (address)
  (cond ((= (ldb (byte 1 31) address) 0)
         (dpb (ldb (byte 8 24) address) (byte 8 24) 0))
        ((= (ldb (byte 2 30) address) 2)
         (dpb (ldb (byte 16 16) address) (byte 16 16) 0))
        ((= (ldb (byte 3 29) address) 6)
         (dpb (ldb (byte 24 8) address) (byte 24 8) 0))
        (t address)))

(compile-flavor-methods network-address)

(add-restrictions 'addressing-domain :chaos :internet)

;;; Network-spec: (addressing-domain network-number [subnet-mask])
(defflavor network-spec (addressing-domain network-number subnet-mask) (value-method-mixin attribute)
  (:default-init-plist :name "Network Spec" :ok-if-void-p t)
  (:init-keywords :value))

(defmethod (network-spec :set-value) (list)
  (let ((domain (first list))
        (number (second list))
        (mask (third list)))
    (when (eq domain :internet)
      (setq number (ip:parse-internet-address number))
      (setq mask (ip:parse-internet-address mask)))
    (setf `(,addressing-domain ,network-number ,subnet-mask) (list domain number mask))))

(defmethod (network-spec :print-value) (stream escape-p)
  (write-char #\( stream)
  (write addressing-domain :stream stream :escape escape-p)
  (write-char #\Space stream)
  (write-string (net::unparse-address network-number addressing-domain) stream)
  (when subnet-mask
    (write-char #\Space stream)
    (write-string (net::unparse-address subnet-mask addressing-domain) stream))
  (write-char #\) stream))

(defmethod (network-spec :value) ()
  (if subnet-mask
      (list addressing-domain
            (net::unparse-address network-number addressing-domain)
            (net::unparse-address subnet-mask addressing-domain))
    (list addressing-domain (net::unparse-address network-number addressing-domain))))

(defmethod (network-spec :multiple-p) () t)

(defmethod (network-spec :void-p) ()
  (null addressing-domain))

(defmethod (network-spec :cue-string) () "")

(defmethod (network-spec :output-value) (window)
  (let ((x self)
        (-addressing-domain- addressing-domain)
        (-network-number- network-number)
        (-subnet-mask- subnet-mask))
    (send window :set-current-font cue-font)
    (write-string "Addressing Domain " window)
    (send window :set-current-font default-font)
    (send window :item1 x 'network-spec-addressing-domain
          #'(lambda (ignore window)
              (send window :string-out (symbol-name -addressing-domain-))))
    (send window :tyo #\Space)
    (send window :set-current-font cue-font)
    (write-string "Network Number " window)
    (send window :set-current-font default-font)
    (send window :item1 x 'network-spec-network-number
          #'(lambda (ignore window)
              (send window :string-out (and -network-number-
                                            (net::unparse-address -network-number- -addressing-domain-)))))
    (send window :tyo #\Space)
    (send window :set-current-font cue-font)
    (write-string "Subnet Mask " window)
    (send window :set-current-font default-font)
    (send window :item1 x 'network-spec-subnet-mask
          #'(lambda (ignore window)
              (send window :string-out (and -subnet-mask-
                                            (net::unparse-address -subnet-mask- -addressing-domain-)))))))

(defmethod (network-spec :case :edit network-spec-addressing-domain) ()
  (let ((new (choose-or-read (get-restriction 'addressing-domain) #'string-to-keyword "Addressing Domain")))
    (setq addressing-domain new)))

(defmethod (network-spec :case :edit network-spec-network-number) ()
  (let ((new (funcall (let ((-addressing-domain- addressing-domain))    ; grumble....
                        #'(lambda (string) (net:parse-address string -addressing-domain-)))
                      (query-read-line "Network Number: "))))
    (unless (valid-network-for-addressing-domain-p addressing-domain new)
      (format *query-io* "~A is not a valid network for the ~A addressing domain.~%"
              (net::unparse-address new addressing-domain) addressing-domain)
      (abort-edit))
    (let ((network (find-network-by-address new addressing-domain)))
      (when network
        (format *query-io* "~A already has ~A network number ~A.~%"
                (send network :name)
                addressing-domain
                (net::unparse-address new addressing-domain))
        (abort-edit)))
    (setq network-number new)))

(defun (:property :chaos valid-network-p) (x)
  (and x (not (minusp x)) (< x 256)))

(defun (:property :internet valid-network-p) (x)
  (and x
       (not (minusp x))
       (or (and (= (ldb (byte 1 31) x) 0)       ;Class A
                (not (zerop (ldb (byte 8 24) x))))
           (= (ldb (byte 2 30) x) 2)            ;Class B
           (= (ldb (byte 3 29) x) 6))))         ;Class C

(defun valid-network-for-addressing-domain-p (addressing-domain network)
  (funcall (or (get addressing-domain 'valid-network-p) #'identity) network))

(defmethod (network-spec :case :edit network-spec-subnet-mask) ()
  (when (addressing-domain-needs-subnet-mask addressing-domain)
    (let* ((string (query-read-line "Subnet Mask: "))
           (new (and (plusp (length string))
                     (let ((-addressing-domain- addressing-domain))     ; grumble....
                       (net:parse-address string -addressing-domain-)))))
      (unless (valid-subnet-mask-for-addressing-domain-and-network-number-p addressing-domain network-number new)
        (format *query-io* "~A is not a valid subnet mask for network ~A in the ~A addressing domain.~%"
                (net::unparse-address new addressing-domain)
                (net::unparse-address network-number addressing-domain)
                addressing-domain)
        (abort-edit))
      (setq subnet-mask new))))

(defun (:property :chaos valid-subnet-mask-p) (network-number subnet-mask)
  (declare (ignore network-number))
  (null subnet-mask))

(defun (:property :internet valid-subnet-mask-p) (network-number subnet-mask)
  (or (null subnet-mask)                        ;Not having a subnet mask is allowed
      (and (zerop (logandc2 network-number subnet-mask))        ;But if have one, must cover all network bits
           (cond ((= (ldb (byte 1 31) network-number) 0)        ;...Including all of Class A
                  (= (ldb (byte 8 24) subnet-mask) #xff))
                 ((= (ldb (byte 2 30) network-number) 2)        ; or Class B
                  (= (ldb (byte 16 16) subnet-mask) #xffff))
                 ((= (ldb (byte 3 29) network-number) 6)        ; or Class C network number
                  (= (ldb (byte 24 8) subnet-mask) #xffffff))))))

(defun valid-subnet-mask-for-addressing-domain-and-network-number-p (addressing-domain network subnet-mask)
  (let ((func (get addressing-domain 'valid-subnet-mask-p)))
    (if func
        (funcall func network subnet-mask)
      (null subnet-mask))))

(setf (get :chaos 'subnet-mask-p) nil)
(setf (get :internet 'subnet-mask-p) t)

(defun addressing-domain-needs-subnet-mask (addressing-domain)
  (get addressing-domain 'subnet-mask-p))

;;; This can be called after clicking on a void network-spec
(defmethod (network-spec :case :edit network-spec) ()
  (send self :edit 'network-spec-addressing-domain)
  (send self :edit 'network-spec-network-number)
  (if (addressing-domain-needs-subnet-mask addressing-domain)
      (send self :edit 'network-spec-subnet-mask)
    (setq subnet-mask nil)))

(compile-flavor-methods network-spec)

;;; Server/Medium entry.
(add-restrictions 'protocol
                  :chaos-mail :qfile :smtp :time-simple :telnet :supdup :3600-login
                  :tcp-ftp :tcp-gateway :name :hostab :spell :ispell)

(add-restrictions 'medium :chaos :tcp :udp)

;;; This is used just for commonly used/confusing medium/protocol combinations
(defvar *service-pretty-names* ())

(defun add-medium-service-name (medium service pretty-name documentation)
  (let ((entry (assoc pretty-name *service-pretty-names* :test 'string-equal)))
    (if entry
        (setf (cdr entry) (list :value (list medium service) :documentation documentation))
      (push (cons pretty-name (list :value (list medium service) :documentation documentation))
            *service-pretty-names*))))

(add-medium-service-name :chaos :name "Chaos Finger" "Chaosnet way of showing users")
(add-medium-service-name :chaos :qfile "Chaos File"
                         "The standard Chaosnet file access protocol")
(add-medium-service-name :tcp :tcp-ftp "TCP FTP" "The standard Internet File Transfer Protcol")
(add-medium-service-name :chaos :eval "Eval" "Evaluation protocol for Unix and Lisp Machines")
(add-medium-service-name :chaos :time-simple "Chaos Time" "The standard Chaosnet Time protocol")
(add-medium-service-name :chaos :lispm-finger "LispM Finger" "Used by Terminal-1-F")
(add-medium-service-name :tcp :ascii-name "TCP Finger" "TCP way of showing users")
(add-medium-service-name :tcp :smtp "Standard SMTP mail" "Standard mail protocol for TCP")
(add-medium-service-name :chaos :send "Chaos Send" "Interactive message protocol")
(add-medium-service-name :chaos :hostab "Chaos HOSTAB" "Chaosnet host information protocol")
(add-medium-service-name :chaos :chaos-spell
                         "Standard Chaos Spell" "Chaosnet SPELL offered by Twenex")
(add-medium-service-name :chaos :chaos-ispell
                         "Unix Chaos Spell" "MIT/LMI Unix Chaos spell protocol")

(defvar *service-protocol-alist* '())

(defun add-abstract-service (service protocol)
  (let ((entry (assoc service *service-protocol-alist*)))
    (if entry
        (pushnew protocol (cdr entry))
      (push (list service protocol) *service-protocol-alist*))))

;;; Just do the relevant ones for now.
(add-abstract-service :store-and-forward-mail :chaos-mail)
(add-abstract-service :store-and-forward-mail :smtp)

(add-abstract-service :file :qfile)
(add-abstract-service :file :tcp-ftp)

(add-abstract-service :time :time-simple)

(defflavor service () (choice-or-any)
  (:default-init-plist :ok-if-void-p t :name "Service" :parse-function 'read-from-string
                       :prompt-string "Medium/Protocol entry"))

(defmethod (service :multiple-p) () t)

(defmethod (service :after :init) (ignore)
  (unless value
    (setq value (list () ()))))

(defmethod (service :void-p) ()
  (not (and (first value) (second value))))

(defmethod (service :cue-string) () "Pair")

(defmethod (service :output-choice) (window)
  (send window :set-current-font cue-font)
  (write-string "Medium " window)
  (send window :set-current-font default-font)
  (send window :item1 self 'service-medium
        #'(lambda (ignore window -medium-) (write-string (symbol-name -medium-) window))
        (first value))
  (write-char #\Space window)
  (send window :set-current-font cue-font)
  (write-string "Protocol " window)
  (send window :set-current-font default-font)
  (send window :item1 self 'service-protocol
        #'(lambda (ignore window -medium-) (write-string (symbol-name -medium-) window))
        (second value)))

(defmethod (service :case :documentation-string service) ()
  "Click to select a usual medium/protocol pair")

(defmethod (service :output-item-type) () 'service)

(defmethod (service :case :edit service) ()
  (send self :edit 'choice-or-any)
  (setq value (copy-list value))) ; don't use actual menu item !

(defmethod (service :choices) () *service-pretty-names*)

(defmethod (service :case :documentation-string service-medium) ()
  "Click to change the medium")

(defmethod (service :case :edit service-medium) ()
  (setf (first value) (choose-or-read (get-restriction 'medium) 'string-to-keyword
                                      "Medium for this protocol")))

(defmethod (service :case :documentation-string service-protocol) ()
  "Click to change the protocol")

(defmethod (service :case :edit service-protocol) ()
  (setf (second value) (choose-or-read (get-restriction 'protocol) 'string-to-keyword
                                       "Protocol offered by host")))

(compile-flavor-methods service)

;;; Printer spec parameters (an S-expression).  Printer NAMES are what other things
;;; point to.
(defflavor printer-parameter-list () (value-variable-mixin attribute)
  (:default-init-plist :name "Parameters" :prompt-string "Printer Parameters (usually a list)"))

(defmethod (printer-parameter-list :cue-string) () "Parameters")

(defmethod (printer-parameter-list :output-value) (window)
  (send window :item1 self 'printer-parameter-list
        #'(lambda (ignore window -spec-) (prin1 -spec- window))
        value))

(defmethod (printer-parameter-list :case :edit printer-parameter-list) ()
  (setq value (read-lisp-object (send self :prompt-string) :type 'cons :default value)))

;;; Timezone
(defvar *bogus-timezones* '(2 12 -1 -2 -3 -4 -5 -6 -7 -8 -9)
  "A somewhat geocentric view of what timezones people are unlikely to choose")

(defun make-timezone-choices ()
  (let ((choices '()))
    (dolist (elt time::*timezones*)
      (let ((offset (car elt)))
        (unless (member offset *bogus-timezones*)
          (unless (rassoc offset choices)
            (push (cons (or (second elt)
                            (format () "~A/~D" (fourth elt) offset))
                        offset)
                  choices)))))
      (reverse choices)))

(defvar *timezone-choices* (make-timezone-choices))

(defflavor timezone () (choice)
  (:default-init-plist :name "Timezone" :choices *timezone-choices* :value time:*timezone*))

(defmethod (timezone :cue-string) () "Timezone")

(compile-flavor-methods timezone printer-parameter-list)

;;; Machine type: built on name, but easier to edit
;;; System type: built on name, but easier to edit
;;; Location: pair of building name and floor
;;; Pathname
;;; List of tokens
;;; Terminal-F arguments

;;; OBJECTS that are actually edited
(defflavor object (name (nicknames ())) (si:property-list-mixin)
  :inittable-instance-variables
  :gettable-instance-variables
  :abstract-flavor
  (:method-combination (:append :base-flavor-first :editor-attributes)
                       (:case :base-flavor-last :update-property :delete-property))
  (:required-methods :class))

(defmacro getp (property)
  "Shorthand for (GET SELF ...)"
  `(getf si:property-list ,property))

(defmethod (object :update-property) (property newvalue oldvalue)
  (declare (ignore oldvalue))
  (setf (getp property) newvalue))

;;; VALUE has to be passed in to differentiate between various instances of the same kind
;;; of attribute.
(defmethod (object :delete-property) (property oldvalue)
  (declare (ignore oldvalue))
  (remf si:property-list property))

;;; This is called after all the property change messages are sent.  :AFTER daemons should
;;; be put on this.
(defmethod (object :consolidate-changes) ()
  (when (getp 'short-name)
    (pushnew (getp 'short-name) nicknames :test #'string-equal))
  (setq nicknames (delete name nicknames :test #'string-equal)))

(defmethod (object :make-copy) ()
  (complain "You can't make of copy of this.")
  ())

(defmethod (object :class-pretty-name) ()
  (or (get (send self :class) 'class-pretty-name) "Object"))

(defmethod (object :name-for-editor) ()
  (string-append (send self :class-pretty-name) " " (send self :name)))

;;; Needed for now.
(defmethod (object :site-name) ()
  (symbol-name *site-name*))

(defmethod (object :deletable-p) ()
  nil)

(defvar *store-alist* '())

(defstruct (store (:type list))
  class
  hash-table ; for fast name searching
  list)

(defsubst class-store (class) (assq class *store-alist*))

(defun find-store (class)
  (or (class-store class)
      (ferror "No store for class ~S" class)))

(defun find-or-create-store (class)
  (or (class-store class)
      (first (push (make-store :class class
                               :hash-table (make-hash-table :test 'string-equal))
                   *store-alist*))))

(defun all-class-objects (class)
  (store-list (find-store class)))

;;; The CLEAR-foo functions don't nuke the hash tables, so they can be reused.
(defun clear-class-store (class)
  (let ((store (find-store class)))
    (when store (clear-store store))))

(defun clear-store (store)
  (clrhash (store-hash-table store))
  (setf (store-list store) '()))

(defun clear-all-stores ()
  (dolist (s *store-alist*) (clear-store s)))

(defmacro with-class-hash-table ((table class) &body body)
  `(let ((,table (store-hash-table (class-store ,class))))
     ,@body))

;;; Doesn't check for hash-table collisions of nicknames -- for the initial entry of the object.
(defmethod (object :hash) ()
  (let ((table (store-hash-table (find-or-create-store (send self :class)))))
    (setf (gethash name table) self)
    (dolist (n nicknames)
      (setf (gethash n table) self))))

(defmethod (object :unhash) ()
  (with-class-hash-table (table (send self :class))
    (remhash name table)
    (dolist (n nicknames)
      (remhash n table))))

(defmethod (object :append :editor-attributes) ()
  (cons (make-instance 'self-name :class (send self :class) :object self :value name
                       :key :name)
        (make-list-of-attributes 'nickname nicknames
                                 :class (send self :class) :object self)))

;;; Daemons should be placed on this
(defmethod (object :add) ()
  (send self :hash)
  (pushnew self (store-list (find-store (send self :class)))))

;;; Daemons should be placed on this
(defmethod (object :remove) ()
  (send self :unhash)
  (let ((store (find-store (send self :class))))
    (setf (store-list store) (delq self (store-list store)))))

(defmethod (object :string-for-printing) ()
  (or name ">> No Name<<"))

(defmethod (object :print-self) (stream print-depth escape-p)
  (declare (ignore print-depth))
  (if escape-p
      (format stream "#<Site Editor ~S \"~A\">" (send self :class) name)
    (send stream :string-out name)))

(defmethod (object :editor-attributes) () '())

(defmethod (object :name-p) (string)
  (eq self (gethash string (store-hash-table (class-store (send self :class))))))

(defun object-name-p (object name) (send object :name-p name))

(defmethod (object :short-name) () name)

(defun find-object-of-class (class name)
  (gethash name (store-hash-table (find-store class))))

(defmethod (object :case :update-property nickname) (newvalue oldvalue)
  (with-class-hash-table (table (send self :class))
    (remhash oldvalue table)
    (setf (gethash newvalue table) self))
  (if (null oldvalue)
      (push newvalue nicknames)
    (setf (car (member oldvalue nicknames :test #'string-equal)) newvalue)))

(defmethod (object :case :delete-property nickname) (oldvalue)
  (with-class-hash-table (table (send self :class))
    (remhash oldvalue table))
  (setq nicknames (delete oldvalue nicknames :test #'string-equal)))

(defmethod (object :case :update-property :name) (newvalue oldvalue)
  (with-class-hash-table (table (send self :class))
    (remhash oldvalue table)
    (setf (gethash newvalue table) self))
  (setq name newvalue))

;;; This doesn't do any consistency checks.
(defun assign-name-for-object (name object)
  (setf (gethash name (store-hash-table (find-store (send object :class)))) object))

(defun generate-name (prefix class)
  (labels ((try (n)
             (let ((name (string-append prefix "-" (symbol-name class) "-" (format () "~D" n))))
               (if (find-object-of-class class name)
                   (try (+ 1 n))
                 name))))
    (try (length (store-list (find-store class))))))

;;; OK, YOU go ahead and write the LOOP iteration path...
(defmacro do-objects ((object class) &body body)
  `(dolist (,object (store-list (find-store ,class)))
     ,@body))

(defmacro do-classes ((class) &body body)
  `(dolist (,class *object-classes*)
     ,@body))

(defmacro do-all-objects ((object) &body body)
  `(do-classes (.class.)
     (do-objects (,object .class.)
       ,@body)))

(defun find-objects-meeting-predicate (class predicate predicate-args)
  (if predicate
      (subset #'(lambda (x) (apply predicate x predicate-args))
              (store-list (find-store class)))
    (all-class-objects class)))

(defvar *keyboard-choose-threshold* 15.
  "If there are more than this many choices, choosing an object uses the keyboard.")

(defvar *choose-any-character* #\Control-Return
  "This character allows less restricted choices when using the keyboard.")

(defun query-command-char-p (char allowed-p)
  (or (and allowed-p (char= char *choose-any-character*))
      (char-equal #\? char)))

(defun keyboard-choose-object (prompt class primary-choices &key default-allowed any-choices)
  (labels
    ((choose-from-set (choices any-allowed)
       ;; This returns out of KEYBOARD-CHOOSE-OBJECT if a choice is made
       ;; It returns non-NIL to the caller if the user wants to choose any object.
       (let ((thing
               (with-input-editing (*query-io* `((:command query-command-char-p ,any-allowed)))
                 (prompt-and-read :string-or-nil "~&~A ~:[~;(~:C for default)~]: "
                                  prompt default-allowed #\Return))))
         (etypecase thing
           (string
            (let ((object (car (member thing choices :test #'(lambda (n o) (send o :name-p n))))))
              (if object
                  (return-from keyboard-choose-object object)
                (progn
                  (format *query-io* "~&\"~A\" is not a name for a ~(~A~) to choose." thing
                          (class-pretty-name class))
                  nil))))
           (cons
            (when (eq (car thing) :command)
              (cond ((char= (second thing) *choose-any-character*)
                     (clear-input *query-io*)
                     t)
                    (t
                     (format *query-io*
                             "~2%Here are the objects to choose; nicknames are also accepted.~2%")
                     (format:print-list *query-io* "~A" choices)
                     (terpri *query-io*))))) ; TERPRI returns NIL.
           (null
            (when default-allowed
              (return-from keyboard-choose-object nil)))))))
    (when any-choices
      (format *query-io* "~&You can type ~:C to select from a wider range of ~(~A~)."
              *choose-any-character* (string-pluralize (class-pretty-name class))))
    (loop (when (choose-from-set primary-choices any-choices) (return)))
    (format *query-io* "~&Careful, since the choice is less restricted now.")
    (loop (choose-from-set any-choices nil))))

(defun choose-object (menu-label class extra-choices &optional predicate predicate-args)
  (let ((choices (find-objects-meeting-predicate class predicate predicate-args)))
    (if (> (length choices) *keyboard-choose-threshold*)
        (flet ((first-item-with-keyword (keyword)
                 (car (member keyword extra-choices
                              :test #'(lambda (keyword choice) (get choice keyword))))))
          (keyboard-choose-object menu-label class choices
                                  :default-allowed
                                  (get (first-item-with-keyword 'keyboard-choose-default)
                                       'keyboard-choose-default)
                                  :any-choices
                                  (let ((item (first-item-with-keyword
                                                'keyboard-choose-from-menu-choose)))
                                    (and item (cdr (get item :menu-choose))))))
      (multiple-value-bind (choice menu-item)
          (tv:menu-choose
            (append extra-choices
                    (find-objects-meeting-predicate class predicate predicate-args))
            menu-label)
        (if menu-item
            choice
          (abort-edit))))))

;;; The REFERENCE-FINDERS property of a class is an alist of the form (property . function).
;;; The function should return a string which is the name of a potential object in that class,
;;; or a list of names as such.  If the function is NIL, the identity function is assumed.
(defun add-interobject-reference (property class &optional function)
  (let ((entry (assoc property (get class 'reference-finders))))
    (if entry
        (setf (cdr entry) function)
      (push (cons property function) (get class 'reference-finders)))))

(defmacro do-reference-strings ((string reference-finders plist) &body body)
  `(dolist (.rf. ,reference-finders)
     (flet ((.do-reference-string. (,string) (declare (sys:downward-funarg)) ,@body))
       (let ((.value. (funcall (or (cdr .rf.) 'identity) (getf ,plist (car .rf.)))))
         (cond ((stringp .value.)
                (.do-reference-string. .value.))
               ((consp .value.)
                (dolist (.s. .value.) (.do-reference-string. .s.))))))))

(defmethod (object :object-reference-present-p) (referee)
  "Return non-NIL if SELF refers to REFEREE"
  (with-class-hash-table (table (send referee :class))
    (do-reference-strings (string (get (send referee :class) 'reference-finders) si:property-list)
      (when (eq (gethash string table) referee)
        (return t)))))

(defmethod (object :class-object-references) (class)
  (let ((refs '()))
    (do-reference-strings (string (get class 'reference-finders) si:property-list)
      (let ((o (find-object-of-class class string)))
        (when o (pushnew o refs))))
    refs))

(defmethod (object :all-object-referrers) ()
  (let ((refs '()))
    (do-all-objects (o)
      (when (send o :object-reference-present-p self)
        (unless (eq o self)
          (push o refs))))
    refs))

(defun add-defaulted-attribute (attribute &rest classes)
  (dolist (class classes) (pushnew attribute (get class 'defaulted-attributes))))

(defmethod (object :set-defaulted-attributes) ()
  (dolist (a (get (send self :class) 'defaulted-attributes))
    (when (eq '.not-there. (getf si:property-list a '.not-there.))
      (setf (getf si:property-list a) nil))))

;;; Old Site option -> site option transformation (default is to keep as this)
;;; At this point, things which really refer to hosts should point to the object.
;;; which is done by having the :VALUE keyword for the attribute flavor do the conversion.
;;; But the actual properties are still names, so that you can delete one object and let
;;; another assume its name.
(defmacro define-site-attribute (keyword (attribute-type &key for-classes) &rest options)
  `(progn
     (setf (get ,keyword 'attribute-descriptor)
           '(,attribute-type ,@(if (getf options :name)
                                   options
                                 (list* :name (string-capitalize-words keyword)
                                        options))))
     ,(when for-classes
        `(add-defaulted-attribute ,keyword ,@for-classes))))


(defmacro define-referring-site-attribute (keyword (attribute-type class
                                                                   &key function for-classes)
                                           &rest options)
  `(progn
     (define-site-attribute ,keyword (,attribute-type :for-classes ,for-classes)
       :class ,class ,@options)
     (add-interobject-reference ,keyword ,class ,function)))

(defun define-ignored-site-attribute (keyword)
  (setf (get keyword 'attribute-descriptor) 'ignore))

;;; This is called also by the host stuff to take care of overriding site options
(defun make-attribute-from-option (property value)
  (let ((desc (get property 'attribute-descriptor)))
    (cond ((null desc)
           (make-instance 'user-property :key property :value value))
          ((eq desc 'ignore) nil)
          ((consp desc)
           (apply #'make-instance (first desc) :value value :key property (rest desc)))
          (t
           (ferror "Funny attribute descriptor ~S for ~S" desc property)))))

(defun canonicalize-attribute (keyword value)
  (funcall (or (get keyword 'attribute-canonicalizer) 'identity) value))

;;; Site options
(define-site-attribute :sys-login-name (ascii-string :for-classes (:site))
  :name "SYS Host Login Name")

(define-site-attribute :sys-login-password (ascii-string :for-classes (:site))
  :name "SYS Host Login Password" :ok-if-void-p t)

(define-site-attribute :standalone (boolean :for-classes (:site)))

(define-referring-site-attribute :default-associated-machine
                                 (object-reference :host :for-classes (:site))
  :predicate file-server-p)

(define-site-attribute :timezone (timezone :for-classes (:site)))

(define-referring-site-attribute :host-for-bug-reports
                                 (object-reference :host :for-classes (:site))
  :predicate mail-server-p)

(define-referring-site-attribute :local-mail-hosts
                                 (object-list :host :for-classes (:site))
  :predicate mail-receiver-p)

(define-site-attribute :long-site-name (ascii-string :for-classes (:site))
  :name "Long Pretty Name" :ok-if-void-p t)

(define-site-attribute :short-site-name (ascii-string :for-classes (:site))
  :name "Short Pretty Name" :ok-if-void-p t)

(define-site-attribute :site-specific-system (atomic-name :for-classes (:site)))

(defvar *mail-receiving-systems* '(:tops20 :its :unix :vms))

(defun mail-receiver-p (host)
  (memq (send host :system-type) *mail-receiving-systems*))

;;; :LOCAL-MAIL-HOSTS is called other-sites-ignored-in-zmail-summary by Brand S

(add-restrictions 'mail-mode :chaos :smtp)
(define-site-attribute :default-mail-mode (restricted-keyword :for-classes (:site))
  :restriction mail-mode)

(define-referring-site-attribute :default-printer
                                 (object-reference :printer :for-classes (:site)))

;;; Called default-bit-map-printer by Brand S
(define-referring-site-attribute :default-bit-array-printer
                                 (object-reference :printer :for-classes (:site))
  :ok-if-void-p t)

;;; :ESC-F-ARGLIST.  (Yuck !).  This is called TERMINAL-F-ARGUMENT in Brand S.
;;; triple: [{nil, number} {:local-lisp-machines, :all-lisp-machines :host} {list of hosts}]

;;; These two are also on *lispm-options*.
(define-site-attribute :verify-lm-dumps (boolean :for-classes (:site))
  :name "Verify dumps made on LMI Lambdas")

(define-referring-site-attribute :backup-host (object-reference :host :for-classes (:site))
  :name "Default host for storage backup logs" :ok-if-void-p t
  :predicate lisp-machine-p)

(define-referring-site-attribute :time-server-hosts (object-list :host :for-classes (:site)))

;;; Ignore the following:
(define-ignored-site-attribute :sys-host)
(define-ignored-site-attribute :arpa-contact-name)
(define-ignored-site-attribute :dover)
(define-ignored-site-attribute :printer-names)
(define-ignored-site-attribute :host-default-device-alist)
(define-ignored-site-attribute :special-file-hosts)

(defvar *media-network-implementations* '()
  "Alist of media and the networks that can implement them")

(defun add-network-for-medium (network medium)
  (let ((entry (assoc medium *media-network-implementations*)))
    (if entry
        (pushnew entry (cdr entry))
      (push (list medium network) *media-network-implementations*))))

(add-network-for-medium :chaos :chaos)
(add-network-for-medium :chaos :chaos-simple)
(add-network-for-medium :internet :tcp)
(add-network-for-medium :internet :udp)

(defun host-supports-medium-p (host medium)
  (and medium
       (host-some-network-typep host (cdr (assoc medium *media-network-implementations*)))))

(defvar *site-option-service-alist* '()
  "An alist of site options that are host lists and medium/protocol combinations.")

(defmacro define-service-list (site-option mp-entry &rest options)
  `(progn
     (define-referring-site-attribute ,site-option (object-list :host :for-classes (:site))
       :predicate host-supports-medium-protocol-pair-p :predicate-args (,mp-entry)
       :any-predicate host-supports-medium-p :any-predicate-args (,(car mp-entry))
       ,@options)
     (set-site-option-service ,site-option ',mp-entry)))

(defun set-site-option-service (option entry)
  (let ((aentry (assoc option *site-option-service-alist*)))
    (if aentry
        (setf (cdr aentry) entry)
      (push (cons option entry) *site-option-service-alist*))))

(define-service-list :chaos-time-server-hosts (:chaos :time-simple))
(define-service-list :chaos-mail-server-hosts (:chaos :chaos-mail))
(define-service-list :spell-server-hosts (:chaos :chaos-spell))
(define-service-list :ispell-server-hosts (:chaos :chaos-ispell))
(define-service-list :arpa-gateways (:chaos :tcp-gateway)
  :name "Chaosnet to TCP/IP ``Arpa'' Gateways")
(define-service-list :smtp-mail-server-hosts (:tcp :smtp))
(define-service-list :chaos-host-table-server-hosts (:chaos :hostab))

(defun internet-lispm-p (host)
  (and (eq (get host :system-type) :lispm)
       (send host :network-typep :internet)))

(define-referring-site-attribute :front-end-tcp-chaos-server
                                 (object-reference :host :for-classes (:site))
  :predicate internet-lispm-p)

;;; Attributes turn back into options by sending the :VALUE-AS-SITE-OPTION message
;;; which defaults to the same as :VALUE.
(defmethod (attribute :value-as-site-option) ()
  (send self :value))

(defun attribute-site-key (attribute)
  (or (send-if-handles attribute :property)
      (send attribute :key)
      (ferror "No site editor key for ~S" attribute)))

(defvar *object-classes* ())

(defsubst class-pretty-name (class) (get class 'class-pretty-name))

(defun assure-class-stores ()
  (dolist (c *object-classes*)
    (find-or-create-store c)))

(defun choose-object-class ()
  (tv:menu-choose (mapcar #'(lambda (s) (list (class-pretty-name s)
                                              :value s
                                              :documentation (get s 'class-documentation)))
                          *object-classes*)
                  "Class"))

(defun choose-any-object (&key (use ""))
  (let ((class (choose-object-class)))
    (when class
      (choose-object (string-append "Choose a "
                                    (string-downcase (get class 'class-pretty-name "Object")) " "
                                    use)
                     class
                     ()))))

(defmacro defclass (flavor (&key (class (intern flavor 'keyword))
                                 (pretty-name (string-capitalize-words class))
                                 (documentation pretty-name))
                    flavor-ivars component-flavors &rest flavor-options)
  `(progn
     (defflavor ,flavor ,flavor-ivars ,(if (memq 'object component-flavors)
                                           component-flavors
                                         (append component-flavors (ncons 'object)))
       ,@flavor-options)
     (defmethod (,flavor :class) () ,class)
     (setf (get ,class 'class-flavor) ',flavor)
     (setf (get ,class 'class-pretty-name) ,pretty-name)
     (setf (get ,class 'class-documentation) ,documentation)
     (pushnew ,class *object-classes*)))

(defmacro compile-object-classes ()
  (cons 'compile-flavor-methods (mapcar #'(lambda (c) (get c 'class-flavor)) *object-classes*)))

(defclass site (:documentation "Represent the attributes of the site")
  ((object-list-attributes nil)) ())

(defmethod (site :get-option) (option)
  (getp option))

(defmethod (site :case :set :get-option) (option newvalue)
  (setf (getp option) newvalue))

(defun make-site (name alist)
  (let ((s (make-instance 'site :name (string name))))
    (send s :snarf-alist alist)
    (send s :set-defaulted-attributes)
    (send s :add)
    s))

(defmethod (site :snarf-alist) (alist)
  (dolist (entry alist)
    (let ((prop (car entry)))
      (unless (eq (get prop 'attribute-descriptor) 'ignore)
        (setf (getp prop) (canonicalize-attribute prop (cdr entry)))))))

(defmethod (site :append :editor-attributes) ()
  (let ((attributes '()))
    (doplist (si:property-list value symbol)
      (let ((a (make-attribute-from-option symbol value)))
        (when a
          (push a attributes))))
    attributes))

(defmethod (site :dump-to-site-info) ()
  (doplist (si:property-list value symbol)
    (setf (get-site-wide-option symbol) value)))

(defclass host (:documentation "Represents the attributes of a machine")
  ((real-host nil) addresses (services nil))
  ()
  :inittable-instance-variables
  (:gettable-instance-variables services addresses))

(defmethod (host :deletable-p) () t)

(add-restrictions 'system-type :unix :its :lispm :tops-20 :tops-10 :vms :s1)
(add-restrictions 'machine-type
                  :vax :vax-11/780 :vax-11/750 :nu :pdp10 :pdp20 :lispm :symbolics-3600 :explorer)
(add-restrictions 'file-system-type :lmfs :lmfile)

(defmethod (host :short-name) ()
  (or (getp 'short-name)
      (car nicknames)
      name))

;;; Not exactly the most efficient thing the world...
(defmethod (host :host-names) ()
  (append nicknames (ncons name)))

(defmethod (host :append :editor-attributes) ()
  (let ((attributes
          (list*
            (make-instance 'restricted-keyword :name "System Type" :restriction 'system-type
                           :key :system-type :value (getp :system-type))
            (make-instance 'restricted-keyword :name "Machine Type" :restriction 'machine-type
                           :key :machine-type :value (getp :machine-type))
            (make-instance 'file-system-type :key :file-system-type
                           :value (getp :file-system-type))
            (nconc (make-list-of-attributes 'network-address addresses)
                   (make-list-of-attributes 'service services)))))
    (when (getp 'short-name)
      (push (make-instance 'self-name :name "Short Name" :key 'short-name
                           :value (getp 'short-name) :object self :class :host)
            attributes))
    (doplist (si:property-list value symbol)
      (when (and (keywordp symbol) (not (memq symbol
                                              '(:file-system-type :system-type :machine-type))))
        (push (make-attribute-from-option symbol value) attributes)))
    attributes
    ))

(defun plist-addresses (plist)
  (do* ((l plist (cddr l))
        (network (car l) (car l))
        (address-list (cadr l) (cadr l))
        addresses)
       ((null l) addresses)
    (dolist (a address-list) (setq addresses (nconc addresses (ncons (list network a)))))))

(defun make-address-plist (addresses)
  (let ((plist nil))
    (mapc #'(lambda (address) (push (cadr address) (getf plist (car address)))) addresses)
    plist))

;;; Options from the host object
(defun make-host-from-real-host (rhost)
  (let* ((name (send rhost :name))
         (host
           (make-instance 'host :real-host rhost
                          :name name :nicknames (remove name (send rhost :host-names)
                                                        :test #'string-equal)
                          :addresses (plist-addresses (send rhost :network-addresses)))))
    (when (send rhost :send-if-handles :file-host-p)
      (setf (get host 'short-name) (send rhost :name-as-file-computer)))
    (setf (get host :system-type) (send rhost :system-type))
    (setf (get host :machine-type) (send rhost :machine-type))
    host))

(defmethod (host :case :update-property network-address) (newval oldval)
  (if oldval
      (setf (car (member oldval addresses :test #'equal)) newval)
    (push newval addresses)))

(defmethod (host :case :delete-property network-address) (oldval)
  (setq addresses (delete oldval addresses :test #'equal)))

(defmethod (host :case :update-property service) (newval oldval)
  (if oldval
      (setf (car (member oldval services :test #'equal)) newval)
    (push newval services)))

(defmethod (host :case :delete-property service) (oldval)
  (setq services (delete oldval services :test #'equal)))

(defmacro do-all-internal-hosts ((host) &body body)
  `(let ((,host nil))
     (dolist (.elem. si:host-alist)
       (setq ,host (si::get-alist-elem-host .elem.))
       ,@body)))

(defvar *init-host-mode* 'make-hosts-from-hosts.text)

(defun make-hosts-from-internal-list ()
  (do-all-internal-hosts (h)
    (let ((host (make-host-from-real-host h)))
      (send host :add))))

(defvar *lisp-host-table-file* "SYS: SITE; HSTTBL LISP >")

(defun make-host-from-define-host (name options)
  (let ((machine-type :default)
        (system-type :default)
        (addresses nil)
        (nicknames nil))
    (doplist (options value symbol)
      (case symbol
        (:machine-type (setq machine-type value))
        (:system-type (setq system-type value))
        (:host-names (setq nicknames (butlast value)))
        (otherwise ; it's a network address spec.
         (setq addresses (nconc addresses (mapcar #'(lambda (n) (list symbol n)) value))))))
    (let ((h (make-instance 'host :name name :nicknames nicknames :addresses addresses)))
      (when (get system-type 'si:host-flavor)
        (setf (get h 'short-name) (or (car nicknames) name)))
      (setf (get h :system-type) system-type)
      (setf (get h :machine-type) machine-type)
      h)))

(defun unquotify (thing)
  (if (and (consp thing) (eq (car thing) 'quote))
      (cadr thing)
    thing))

(defun make-hosts-from-hsttbl.lisp ()
  (fs:reading-from-file (x *lisp-host-table-file*)
    (if (eq (car x) 'si::define-host)
        (send (make-host-from-define-host (cadr x) (mapcar #'unquotify (cddr x))) :add)
      (format *error-output* "~&Strange thing [~S] found in Lisp host table." x))))

(defun init-hosts ()
  (funcall *init-host-mode*)
  (do-objects (o :host) (send o :set-defaulted-attributes)))

(defvar *hosts2-table-file* "SYS: CHAOS; HOSTS TEXT >")

(defun make-hosts-from-hosts.text ()
  (dolist (elem (NET:READ-HOSTS2-TABLE *hosts2-table-file*))
    (send (make-host-from-define-host (car elem) (cdr elem)) :add)))

(defun hosts2-address-lessp (x y)
  (if (send x :network-typep :chaos)
      (if (send y :network-typep :chaos)
          (< (send x :network-address :chaos) (send y :network-address :chaos))
        nil)
    (if (send y :network-typep :chaos)
        t
      (string< (send x :name) (send y :name)))))

(defun write-hosts2-table (&optional (file *hosts2-table-file*))
  (with-open-file (o file :direction :output)
    (format o ";;; -*- Mode: Fundamental -*- HOSTS2 table ~A" (truename o))
    (flet ((write-address (a)
             (let ((type (car a)))
               (write-string (symbol-name type) o)
               (write-char #\Space o)
               (write-string (net::unparse-address (cadr a) type) o))))
      (si::write-responsibility-comment o)
      (net::write-hosts2-preamble o)
      (dolist (h (sort (copy-list (store-list (class-store :host))) #'hosts2-address-lessp))
        (format o "HOST ~A,~C" (send h :name) #\Tab)
        (let ((addresses (send h :addresses)))
          (cond ((> (length addresses) 1)
                 (write-char #\[ o)
                 (do ((addrs addresses (cdr addrs))
                      a)
                     ((null addrs))
                   (setq a (car addrs))
                   (write-address a)
                   (write-string (if (null (cdr addrs)) "]," ", ") o)))
                (t
                 (write-address (car addresses))
                 (write-char #\, o))))
        (format o "USER,~A,~A,[" (get h :system-type) (get h :machine-type))
        (net::write-other-host-names h o "," "~A")
        (write-line "]" o)))))

(defun make-hosts-complete ()
  (do-objects (o :host)
    (send o :snarf-from-site-alist) ; get random and LISPM properties
    (send o :snarf-extra-information)))

(defmethod (host :network-address) (network)
  (cadr (assoc network addresses)))

(defmethod (host :system-type) () (getp :system-type))
(defmethod (host :machine-type) () (getp :machine-type))

(defmethod (host :supports-service-p) (service)
  (some #'(lambda (m-p) (sys:member-equal m-p (cdr (assoc service *service-protocol-alist*))))
        services))

(defun host-supports-medium-protocol-pair-p (host mp)
  (send host :supports-medium-protocol-pair-p mp))

(defmethod (host :supports-medium-protocol-pair-p) (mp)
  (zl:member mp services))

(defmethod (host :add-medium-protocol-entry) (m-p)
  (pushnew m-p services :test #'equal))

(defmethod (host :network-typep) (network)
  (assoc network addresses))

(defun host-network-typep (host network)
  (send host :network-typep network))

(defun host-some-network-typep (host networks)
  (some #'(lambda (network) (host-network-typep host network)) networks))

;;; This is just like :default-printer, except it can be empty.
(define-referring-site-attribute :printer (object-reference :printer)
  :ok-if-void-p t)

(define-site-attribute :pretty-name (ascii-string))
(define-site-attribute :finger-location (ascii-string))
(define-site-attribute :server-machine (boolean))

(define-referring-site-attribute :associated-machine (object-reference :host)
  :predicate file-server-p)

(defun lisp-machine-p (host)
  (lisp-machine-system-p (send host :system-type)))

(defun lisp-machine-system-p (os)
  (eq os :lispm))

(defun file-server-p (host)
  (send host :file-server-p))

(defmethod (host :file-server-p) ()
  (or (send self :supports-service-p :file)
      (if (and real-host (eq (getp :system-type) (send real-host :system-type)))
          (send real-host :send-if-handles :file-host-p)
        (get (getp :system-type) 'si::host-flavor)))) ; very reasonable heuristic/kludge

(defun mail-server-p (host)
  (send host :supports-service-p :store-and-forward-mail))

(defun host-supports-service-p (host service)
  (send host :supports-service-p service))

(defmethod (host :supports-service-p) (service)
  (let ((service-protocols (cdr (assoc service *service-protocol-alist*))))
    (when service-protocols
      (dolist (m-p-entry services nil)
        (when (memq (second m-p-entry) service-protocols)
          (return t))))))

(defmethod (host :snarf-from-site-alist) ()
  (dolist (elt *site-option-alist*)
    (let ((key (car elt)) (value (cdr elt)))
      (case key
        (:host-default-device-alist
         (let ((entry (assoc self value :test #'object-name-p)))
           (when entry
             (setf (getp :default-device) (cdr entry)))))
        (:special-file-hosts
         (let ((entry (rassoc self value :test #'(lambda (h list)
                                                       (member h list :test #'object-name-p)))))
           (when entry
             (setf (getp :file-system-type) (car entry)))))
        (otherwise
         (let ((service-entry (assoc key *site-option-service-alist*)))
           (when service-entry
             (when (member self value :test #'object-name-p)
               (send self :add-medium-protocol-entry (copy-list (cdr service-entry)))))))))))

;;; Things that Lisp Machines care about: (* means it is a meaningful site option)
;;; Console location
;;; * Default printer
;;; * Default bit-array printer
;;; Pretty name
;;; Finger location
;;; Console location (building, floor)
;;; Associated machine
;;; Random user properties

;;; There's no &^@#% DEFSTRUCT for this in the right place !
(defstruct (machine-entry (:type list)
                          (:conc-name me-))
  name
  pretty-name
  finger-location
  location
  associated-machine
  options)

;;; Put these in reverse order of presentation
(defvar *lispm-options* '(:server-machine :default-bit-array-printer :printer
                          :backup-host :verify-lm-dumps
                          :location :finger-location :associated-machine :pretty-name))


;;; Used to translate between certain Brand S properties and old site options.
(defvar *site-option-property-translations* '((:default-printer . :printer)))

;;; This is called after all hosts have been made, but not completed.
(defmethod (host :snarf-extra-information) ()
  (when (lisp-machine-system-p (getp :system-type))
    (let ((me (assoc self *machine-location-alist* :test #'object-name-p)))
      (setf (getp :pretty-name) (me-pretty-name me))
      (setf (getp :finger-location) (me-finger-location me))
      (setf (getp :location) (me-location me))
      (setf (getp :associated-machine) (me-associated-machine me))
      (dolist (opentry (me-options me))
        (let* ((symbol (car opentry))
               (value (canonicalize-attribute symbol (evaluate-safely (cadr opentry)))))
          (setf (getp (or (cdr (assq symbol *site-option-property-translations*)) symbol))
                      value)))
      (dolist (op *lispm-options*)
        (unless (getp op)
          (setf (getp op) nil))))))

(defmethod (host :network-address-p) (address)
  (zl:member address addresses))

(defun get-host-from-address (address)
  (values-list (subset #'(lambda (h) (send h :network-address-p address))
                       (all-class-objects :host))))

;;; >Assuming numeric addresses here, not hacking subnets
(defun generate-new-address (address)
  (let ((test-address (copy-list address)))
    (loop
      (if (get-host-from-address test-address)
          (incf (second test-address))
        (return test-address)))))

(defun make-like-addresses (addresses)
  (mapcar #'generate-new-address addresses))

(defmethod (host :make-copy) ()
  (let* ((new-name (generate-name (string-append (send self :site-name) "-" (getp :machine-type))
                                  :host))
         (h
           (make-instance 'host :name new-name :nicknames ()
                          :property-list (copy-tree si:property-list)
                          :services (copy-tree services)
                          :addresses (make-like-addresses addresses)
                          :real-host ())))
    (when (lisp-machine-system-p (getp :system-type))
      (setf (get h 'short-name) new-name)
      (setf (get h :finger-location) "Somewhere")
      (setf (get h :pretty-name) new-name))
    (send h :add)
    h))

;;; This translation should not be applied to random properties
(defun object-for-site-file (thing)
  (etypecase thing
    ((or number string keyword null) thing)
    (symbol (intern (symbol-name thing) 'keyword))
    (object (send thing :name))
    (list (mapcar #'object-for-site-file thing))))

(defmethod (host :dump-to-site-info) ()
  (let ((entry (and (lisp-machine-system-p (getp :system-type))
                    (make-machine-entry :name name :pretty-name name :finger-location "??"
                                        :location '(:? 0) :associated-machine name))))
    (doplist (si:property-list value symbol)
      (when (keywordp symbol) ; only do external ones
        (case symbol
          (:file-system-type
           (when value
             (let ((alist (get-site-wide-option :special-file-hosts)))
               (if alist
                   (let ((entry (assoc value alist)))
                     (if entry
                         (push name (cdr entry))
                       (setq alist (nconc alist (list (list value name))))))
                 (setf (get-site-wide-option :special-file-hosts) (list (list value name)))))))
          (:default-device
            (when value
              (push (cons name value) (get-site-wide-option :host-default-device-alist))))
          (otherwise
           (when entry ; for Lisp Machines
             (case symbol
               (:location (setf (me-location entry) value))
               (:pretty-name (setf (me-pretty-name entry) value))
               (:finger-location (setf (me-finger-location entry) value))
               (:associated-machine (setf (me-associated-machine entry) value))
               ((:system-type :machine-type))
               (otherwise ; a random property
                (when value (push (list (or (car (rassoc symbol
                                                         *site-option-property-translations*))
                                            symbol)
                                        value) (me-options entry))))))))))
    (when entry
      (push entry *machine-location-alist*))
    entry))

;;; Printers
(defclass printer (:documentation "Represents a printer")
  (type (parameters ()) (parameters-p t))
  ()
  :inittable-instance-variables
  (:init-keywords :spec)
  (:required-init-keywords :spec))

(defmethod (printer :deletable-p) () t)

(defmethod (printer :before :init) (plist)
  (let ((spec (get plist :spec)))
    (if (keywordp spec)
        (setq type spec parameters-p ())
      (setq type (car spec) parameters (cdr spec)))))

(defmethod (printer :spec) ()
  (if (or parameters-p parameters)
      (cons type parameters)
    type))

(defmethod (printer :make-copy) ()
  (let ((p
          (make-instance 'printer
                         :name (generate-name (send self :site-name) :printer)
                         :nicknames ()
                         :spec (send self :spec))))
    (send p :add)
    p))

;;; This dumps the object out to the current way of doing things.
(defmethod (printer :dump-to-site-info) ()
  (push (list (cons name nicknames) (send self :spec)) (get-site-wide-option :printer-names)))

(defun make-printers ()
  (map ()
    #'(lambda (entry)
        (send
          (make-instance 'printer :name (first (first entry)) :nicknames (rest (first entry))
                         :spec (second entry))
          :add))
    (get-site-wide-option :printer-names)))

(add-restrictions 'printer-type :imagen :laser2 :ti855 :laser1)

(defmethod (printer :append :editor-attributes) ()
  (list
    (make-instance 'restricted-keyword :value type :restriction 'printer-type :key :printer-type
                   :name "Type")
    (make-instance 'printer-parameter-list :value parameters)))

(defmethod (printer :case :update-property :printer-type) (newvalue oldvalue)
  (declare (ignore oldvalue))
  (setq type newvalue))

(defmethod (printer :case :update-property printer-parameter-list) (newvalue oldvalue)
  (declare (ignore oldvalue))
  (setq parameters newvalue))

;;; This stuff is done to make the switchover to named printer specs easy.  The user is asked
;;; to name the printers during the BIG-INIT function.
(defvar *hacked-printer-specs* '() "An alist of names and printer specs")

(defun ask-user-for-printer-name (spec)
  (format *query-io* "~&A printer with the specification ~S was found in the site information."
          spec)
  (unless *hacked-printer-specs* ; Don't give the spiel if we've told 'em.
    (format *query-io* "~2%In Release 3.0 and after, printers should be referred to by name,
as a string, so that they can be edited as objects in their own right.  The site option that
currently maps names to printer specs is called :PRINTER-NAMES; see the Release 3.0 Notes for
details.~2%"))
  (flet ((ask ()
           (let ((string (prompt-and-read :string-or-nil "~&Name for ~S: " spec)))
             (and string (string-upcase string)))))
    (do ((string (ask) (ask)))
        (nil)
      (cond ((null string)
             (format *query-io* "~&Please supply a name."))
            ((or (find-object-of-class :printer string)
                 ;; Actually, the following line isn't absolutely neccessary right now, but
                 ;; might be later if the order of the initialization of the classes changes.
                 (assoc string *hacked-printer-specs* :test #'string-equal))
             (format *query-io* "~&That name has already been given to another printer."))
            ((verify-string string atomic-name-bit-vector)
             (push (cons string spec) *hacked-printer-specs*)
             (send (make-instance 'printer :name string :nicknames () :spec spec) :add)
             (return string))
            (t
             (format
               *query-io*
               "~&The name should have only type of characters that a host name would have."))))))

(defun find-printer-name-of-spec (thing)
  (or (do-objects (p :printer)
        (when (equal (send p :spec) thing)
          (return (send p :name))))
      (car (rassoc thing *hacked-printer-specs* :test #'equal))))

(defun canonicalize-printer-reference (thing)
  (typecase thing
    ((or null string) thing) ; printer name or NIL
    (keyword ; internal printer spec
     (or (find-printer-name-of-spec thing)
         (ask-user-for-printer-name thing)))
    (cons ; internal printer spec
      ;; Uppercase the string, just to be canonical.  This won't catch different names for the
      ;; same hosts, though.
      (let ((spec (mapcar #'(lambda (x) (if (stringp x) (string-upcase x) x)) thing)))
        (or (find-printer-name-of-spec spec)
            (ask-user-for-printer-name spec))))
    (t
     (format *error-output* "~&A strange printer spec [~S] was found in the site files." thing)
     nil)))

(defprop :printer canonicalize-printer-reference attribute-canonicalizer)
(defprop :default-printer canonicalize-printer-reference attribute-canonicalizer)
(defprop :default-bit-array-printer canonicalize-printer-reference attribute-canonicalizer)

(compile-flavor-methods host printer site)

;;;Network class
(defclass network (:documentation "Represents a network at this site")
  ((network-specs nil))
  ()
  :inittable-instance-variables
  (:init-keywords :spec)
  (:required-init-keywords :spec)
  :gettable-instance-variables)

(defmethod (network :deletable-p) () t)

(defmethod (network :short-name) ()
  (or (getp 'short-name)
      (car nicknames)
      name))

(defmethod (network :before :init) (plist)
  (setq network-specs (get plist :spec)))

(defmethod (network :after :init) (&rest ignore)
  (dolist (x network-specs)
    (when (and (second x) (not (numberp (second x))))
      (setf (second x) (net:parse-address (second x) (first x))))
    (when (and (third x) (not (numberp (third x))))
      (setf (third x) (net:parse-address (third x) (first x))))))

(defmethod (network :spec) ()
  network-specs)

;;; Not exactly the most efficient thing the world...
(defmethod (network :network-names) ()
  (append nicknames (ncons name)))

(defmethod (network :append :editor-attributes) ()
  (let ((attributes (make-list-of-attributes 'network-spec network-specs)))
    (when (getp 'short-name)
      (push (make-instance 'self-name :name "Short Name" :key 'short-name
                           :value (getp 'short-name) :object self :class :network)
            attributes))
    attributes
    ))

(defmethod (network :case :update-property network-spec) (newval oldval)
  (when (and (second newval) (not (numberp (second newval))))
    (setf (second newval) (net:parse-address (second newval) (first newval))))
  (when (and (third newval) (not (numberp (third newval))))
    (setf (third newval) (net:parse-address (third newval) (first newval))))
  (if oldval
      (setf (car (member (car oldval) network-specs :test #'eq :key #'car)) newval)
    (push newval network-specs)))

(defmethod (network :case :delete-property network-spec) (oldval)
  (setq network-specs (delete oldval network-specs :test #'equal)))

(define-ignored-site-attribute :network-names)

(defmethod (network :make-copy) ()
  (let ((n (make-instance 'network
                          :name (generate-name (send self :site-name) :network)
                          :nicknames ()
                          :spec ())))
    (send n :add)
    n))

(defmethod (network :dump-to-site-info) ()
  (push (list (cons name nicknames)
              (do* ((list (send self :spec) (cdr list))
                    (spec (car list) (car list))
                    (domain (first spec) (first spec))
                    (result nil))
                   ((null spec)
                    (nreverse result))
                (push (append (ncons domain)
                              (when (second spec)
                                (ncons (human-readable-network-address (second spec) domain)))
                              (when (third spec)
                                (ncons (human-readable-network-address (third spec) domain))))
                      result)))
        (get-site-wide-option :network-names)))

(defun human-readable-network-address (address domain)
  (case domain
    (:chaos address)
    (:internet (net:unparse-address address domain))))

(defun make-networks ()
  (map ()
    #'(lambda (entry)
        (send (make-instance 'network
                             :name (first (first entry))
                             :nicknames (rest (first entry))
                             :spec (second entry))
              :add))
    (get-site-wide-option :network-names))
  (unless (all-class-objects :network)
    (format *query-io* "No Networks are defined in the site files.  Please name one.")
    (ask-user-for-network-name)))

(defun find-network-by-address (address domain)
  (let ((func (get domain 'network-number-from-address #'identity)))
    (dolist (x (all-class-objects :network))
      (dolist (network-spec (send x :spec))
        (and (eq domain (first network-spec))
             (if (third network-spec)
                 (= (logand (third network-spec) address) (second network-spec))
               (= (funcall func address) (funcall func (second network-spec))))
             (return-from find-network-by-address x))))))

(compile-flavor-methods network)

(defun ask-user-for-network-name ()
  (flet ((ask ()
           (let ((string (prompt-and-read :string-or-nil "~&Network Name: ")))
             (and string (string-upcase string)))))
    (do ((string (ask) (ask)))
        (nil)
      (cond ((null string)
             (format *query-io* "~&Please supply a name."))
            ((find-object-of-class :network string)
             (format *query-io* "~&That name has already been given to another network."))
            ((verify-string string atomic-name-bit-vector)
             (send (make-instance 'network :name string :nicknames () :spec ()) :add)
             (return string))
            (t
             (format *query-io*
                     "~&The name should have only type of characters that a host name would have."))))))
