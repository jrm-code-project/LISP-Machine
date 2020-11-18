;-*- Mode:LISP; Package:QFASL-REL; Base:8; Lowercase:T; Readtable:T -*-

;Temporary area for data structures used in dumping.
(defvar dump-temp-area)

;Section number of next section to be created.
(defvar next-dump-section-number)

;This is how big to create sections.
(defvar dump-section-array-size #o3000)

;Close off a section when it gets bigger than this size.
(defvar dump-section-max-size #o2000)

;This vector, indexed by section number, gives the
;art-32b array used to hold the data for the section.
(defvar dump-section-array-table)

;This vector, indexed by section number, gives the area code number for the section.
(defvar dump-section-area-code)

;This vector, indexed by section number,
;is T if the section contains structures (not lists).
(defvar dump-section-structure-p)

;This art-q array holds the symbols which make up section 0.
(defvar dump-section-0-array)

;This alist maps area numbers into section numbers of structure sections.
(defvar area-structure-section-alist)

;This alist maps area numbers into section numbers of list sections.
(defvar area-list-section-alist)

;In the dump file, areas get mapped into the index of the area in this list.
;When loading, a corresponding list (area-code-list) is used to
;map the indices back into areas.  The two lists are similar but not identical.
(defvar dump-area-code-list '(dump-temp-area macro-compiled-program nr-sym p-n-string
                              working-storage-area permanent-storage-area
                              fasl-constants-area fasd-temporary-area
                              debug-info-area))

;This is the index within the array used to hold a section's data
;of the first word which we store the contents in.
(defvar dump-section-contents-offset 2)

;Stream we are writing the dump file on
(defvar dump-stream)

;Save odd bytes to be put together with following bytes
;to make halfwords to be written in the file.
(defvar previous-byte)

;If this is non-nil, it is an area which the data
;being dumped should be loaded into when loaded,
;overriding the area the data is actually in.
;This is used for dumping temporary lists.
(defvar force-dump-area)

;; The remaining variables are DEFVAR'd in RELLD, but used here as well.

;The high ten bits of a relocatable pointer are the section number.
(proclaim '(special section-number-bp))

;The low 14 bits of a relocatable pointer are the index within the section.
(proclaim '(special address-within-section-bp))

;Vector indexed by data type of Q, T if Q points at something.
(proclaim '(special data-type-pointer-p))

;Increment this when the format of data storage changes
;such that old relocatable files will no longer be valid.
(proclaim '(special current-storage-format-version))

;List relating names of opcodes in relocatable files to their numeric values.
;See WRITE-OPCODE.
(proclaim '(special op-list))

;Dumping involves these steps:
;(dump-start) to initialize the tables,
;one or more calls to (dump-form), etc., to store data in the tables,
;(write-dump-file filename) to store the tables in a file (this doesn't change the tables).

(defun dump-start ()
  "Initialize output to a REL file already open."
  (or (boundp 'dump-temp-area)
      (setq dump-temp-area (make-area :name 'dump-temp-area)))
  ;(reset-temporary-area dump-temp-area)
  (setq next-dump-section-number 1
        dump-section-array-table (make-array #o100
                                             :area dump-temp-area
                                             :leader-length 1)
        dump-section-area-code (make-array #o100
                                           :area dump-temp-area
                                           :leader-length 1)
        dump-section-structure-p (make-array #o100
                                             :area dump-temp-area
                                             :leader-length 1)
        dump-section-0-array (make-array dump-section-array-size
                                         :area dump-temp-area
                                         :leader-length 1)
        area-structure-section-alist nil
        area-list-section-alist nil
        force-dump-area nil)

  ;; Mark section 0 as existing and empty.
  (setf (array-leader dump-section-array-table 0) 1)
  (setf (array-leader dump-section-area-code 0) 1)
  (setf (array-leader dump-section-structure-p 0) 1)
  (setf (array-leader dump-section-0-array 0) 0)

  ;;Create section 1.  Give it an area code of 0
  ;;but don't leave it marked as THE section to use by default
  ;;for that area.
  (create-section dump-temp-area nil)
  (setq area-list-section-alist nil)

  (init-data-type-tables))

(defun dump-file-property-list (generic-pathname plist)
  "Output a file property list to the open REL file.
GENERIC-PATHNAME says which file, and PLIST is the property list value to dump."
  (dump-form `(set-generic-pathname-property-list
                ;; Cannot fasd instances yet.
                ,(send generic-pathname :string-for-printing)
                ,plist)))

(defun set-generic-pathname-property-list (generic-pathname-string plist &aux pathname)
  (setq pathname (send (fs:merge-pathname-defaults generic-pathname-string)
                       :generic-pathname))
  (do ((l plist (cddr l))) ((null l))
    (send pathname :putprop (cadr l) (car l))))

;Right now this doesn't really obey storage conventions
;in that it can return values with pointer data types
;whose addresses are relocatable pointers (containing section numbers)
;which aren't really meaningful as virtual addresses.
;However, this happens only if called from dump-fset-evalled,
;whose uses are commented out below.
(defun dump-form (form &optional optimize &aux function)
  "Put something to execute FORM into the REL file being written.
If OPTIMIZE is true, many common types of forms are handled specially,
including SETQ, DEFF, DEFUN, etc.  In particular, (DEFUN FOO)
is processed by dumping FOO's current function definition."
  (cond ((or (memq form '(t nil))
             (stringp form)
             (numberp form))
         (%make-pointer (%data-type form)
                        (dump-value (%data-type form) (%pointer form))))
        ((atom form) (dump-random-form form))
        ((not (symbolp (car form))) (dump-random-form form))
        ((eq (setq function (car form))
             'quote)
         (%make-pointer (%data-type (cadr form))
                        (dump-value (%data-type (cadr form)) (%pointer (cadr form)))))
        ((not optimize)
         (dump-random-form form))
;       ((eq function #'deff)
;        (dump-fset-evalled (cadr form) (caddr form)))
;       ((and (eq (car form) 'fset-carefully)
;             (consp (cadr form))
;             (eq (caadr form) 'quote))
;        (dump-fset-evalled (cadadr form) (caddr form)))
        ((eq function 'setq)
         (dump-setq-form form))
        ((eq function 'deff)
         (dump-deff-form form))
        ((eq function 'fset)
         (dump-fset-form form))
        ((eq function 'defun)
         (dump-function (cadr form)))
         ;; Does this happen?  It does not, for compilation of top-level DECLAREs.
         ;; Let's see if anyone misses it.  QFASD has something similar.
;       ((eq function 'declare)
;        (dolist (dcl (cdr form))
;          (and (memq (car dcl) '(special unspecial :special :unspecial))
;               (dump-form dcl))))
        (t (dump-random-form form))))

(defun dump-setq-form (form)
  (cond ((and (consp (third form))
              (eq (car (third form)) 'quote))
         (dump-set (second form) (second (third form))))
        (t (dump-random-form form))))

(defun dump-deff-form (form)
  (cond ((and (consp (third form))
              (eq (car (third form)) 'quote))
         (dump-lap-fset (second form) (second (third form))))
        (t (dump-random-form form))))

(defun dump-fset-form (form)
  (cond ((and (consp (second form))
              (eq (car (second form)) 'quote)
              (consp (third form))
              (eq (car (third form)) 'quote))
         (dump-set (second (second form)) (second (third form))))
        (t (dump-random-form form))))

;Dump out a command for a form to be evaluated at load time.
;Returns the relocatable pointer into section 1
;in case we want to store the value somewhere.
(defun dump-random-form (form &aux index pointer)
  "Output instructions to open REL file to evaluate FORM at load time.
This is a low level function and does not check for constants or quoted data."
  (setq index (ldb address-within-section-bp
                   (setq pointer (dump-value-1 (list form) 1))))
  (%p-dpb-offset 0 %%q-cdr-code (aref dump-section-array-table 1)
                 (+ index dump-section-contents-offset))
  pointer)

(defun dump-set (symbol value &aux index)
  "Dump a commend to the open REL file to set SYMBOL to VALUE at load time."
  ;; Dump two words, one pointing at symbol, another pointing at value.
  (setq index (ldb address-within-section-bp (dump-value-1 (list symbol value) 1)))
  ;; Change cdr code of first word to 1, which means "symbol, store value".
  (%p-dpb-offset 1 %%q-cdr-code (aref dump-section-array-table 1)
                 (+ index dump-section-contents-offset)))

(defun dump-lap-fset (symbol definition)
  (dump-fset symbol definition compiler:qc-file-file-group-symbol))

;Dump out a command for an fset to be done at load time.
(defun dump-fset (symbol definition &optional file-symbol &aux index)
  ;; Dump three words, one pointing at symbol, another pointing at definition,
  ;; another pointing at the file symbol saying where the symbol is defined.
  (setq index (ldb address-within-section-bp
                   (dump-value-1 (list symbol definition file-symbol) 1)))
  ;; Change cdr code of first word to 2, which means "symbol, store function cell".
  (%p-dpb-offset 2 %%q-cdr-code (aref dump-section-array-table 1)
                 (+ index dump-section-contents-offset)))

(defun dump-variable (symbol)
  "Dump a command to the open REL file to set SYMBOL to the same value it has now."
  (dump-set symbol (symeval symbol)))

(defun dump-function (symbol)
  "Dump a command to the open REL file to define SYMBOL as it is defined now."
  (dump-fset symbol (fsymeval symbol) (get symbol ':source-file-name)))

(defun dump-value (data-type pointer &aux (str-pointer (%find-structure-header pointer)))
  "Dump a pointer to the beginning or the middle of an object.
Returns a relocatable pointer (a fixnum) to that object as dumped.
Non-pointers are also accepted.  They are returned unchanged.
Symbols are handled specially so that they will be interned at load time."
  (cond ((not (aref data-type-pointer-p data-type))
         pointer)
        (t (+ (%pointer-difference pointer str-pointer)
              (cond ((symbolp str-pointer)
                     (dump-symbol str-pointer))
                    (t (dump-value-1 str-pointer)))))))

(defun dump-symbol (symbol)
  "Dump a reference to a symbol.
Returns a relocatable pointer (a fixnum) to the symbol.
Enters the symbol in section 0."
  (do ((i 0 (1+ i))
       (end (array-active-length dump-section-0-array)))
      ((= i end)
       (array-push-extend dump-section-0-array symbol)
       (* 4 i))
    (and (eq (aref dump-section-0-array i) symbol)
         (return (* 4 i)))))

(defun dump-value-1 (object &optional section-number)
  "Dump a pointer to an object not a symbol.
Puts the contents of the object in the appropriate section
and returns a relocatable pointer to it.
We assume that object is a value returned by %find-structure-header
and therefore its data type must be dtp-list if it lives in a list region."
  (let ((secnum (or section-number
                    (area-section (or force-dump-area (%area-number object))
                                  (atom object)))))
    (or secnum (ferror nil "object in unhandled area"))
    (let ((total-len (%structure-total-size object))
          (boxed-len (%structure-boxed-size object))
          (start-offset (%pointer-difference (%find-structure-leader object)
                                             object)))
      (let ((index (allocate-section-space secnum total-len))
            (array (aref dump-section-array-table secnum)))
        ;; Copy in all the data of the object, assuming not relocatable data.
        ;; Don't use %BLT-TYPED since we are copying into an array that won't exist for long.
        (%blt (%make-pointer-offset dtp-fix object start-offset)
              (%make-pointer-offset dtp-fix array (+ index dump-section-contents-offset))
              total-len 1)
        ;; Now, for all Qs which contain boxed data,
        ;; find pointers to storage, dump the storage,
        ;; and replace the pointers by relocatable ones.
        (dotimes (i boxed-len)
          (and (%p-pointerp-offset object (+ i start-offset))
               (%p-dpb-offset (dump-value (%p-ldb-offset %%q-data-type
                                                         object (+ i start-offset))
                                          (%p-contents-as-locative-offset
                                            object (+ i start-offset)))
                              %%q-pointer array (+ index i dump-section-contents-offset))))
        (dpb secnum section-number-bp (dpb index address-within-section-bp 0))))))

;If the section reaches the maximum desired space as a result of this,
;we remove the section from the area-section alists
;so any further dumping from this section's area
;will create a new section.
(defun allocate-section-space (secnum n-words)
  "Allocate N-WORDS words in section SECNUM, returning the index in that section."
  (prog (array index)
        (setq array (aref dump-section-array-table secnum))
        (setq index (array-leader array 0))
        (and (> (+ index n-words) (array-length array))
             (adjust-array-size array (+ index n-words)))
        (setf (array-leader array 0) (+ index n-words))
        (and (> (+ index n-words) dump-section-max-size)
             (prog (area)
                   (cond ((aref dump-section-structure-p secnum)
                          (dolist (l area-structure-section-alist)
                            (and (= (cdr l) secnum)
                                 (return (setq area (car l)
                                               area-structure-section-alist
                                               (delq l area-structure-section-alist))))))
                         (t
                          (dolist (l area-list-section-alist)
                            (and (= (cdr l) secnum)
                                 (return (setq area (car l)
                                               area-list-section-alist
                                               (delq l area-list-section-alist)))))))))
        (return index)))

(defun area-section (area structure-p)
  "Return the section number to use for dumping data from area AREA.
STRUCTURE-P says whether the data to be dumped is structures (as opposed to lists)
since different sections are used for those two."
  (prog (secnum)
        (setq secnum (cdr (assq area (if structure-p area-structure-section-alist
                                         area-list-section-alist))))
        (and secnum (return secnum))
        (or (dolist (a dump-area-code-list)
              (and (= area
                      (cond ((eq a 'temp-area) dump-temp-area)
                            (t (symeval a))))
                   (return t)))
            (ferror nil "Area ~S can't be dumped" area))
        (return (create-section area structure-p))))

;Create a section to hold data in the specified area.
;You must also specify whether the section is for structure data
;as opposed to lists.
;We assume that there is no section for the desired area and type of data.
;The new section is put on area-list-section-alist or area-structure-section-alist
;so that area-section can find it and keep using it.
(defun create-section (area structure-p)
  (prog (secnum array)
        (setq secnum next-dump-section-number)
        (setq next-dump-section-number (1+ next-dump-section-number))
        (setq array (make-array dump-section-array-size
                                :type 'art-inum
                                :area dump-temp-area
                                :leader-length 1))
        (setf (array-leader array 0) 0)
        (array-push-extend dump-section-array-table array)
        (array-push-extend dump-section-area-code
                           (do ((i 0 (1+ i))
                                (l dump-area-code-list (cdr l)))
                               ((null l))
                             (and (= area
                                     (cond ((zerop i) dump-temp-area)
                                           (t (symeval (car l)))))
                                  (return i))))
        (array-push-extend dump-section-structure-p structure-p)
        (cond (structure-p
               (push (cons area secnum) area-structure-section-alist))
              (t (push (cons area secnum) area-list-section-alist)))
        (return secnum)))

(defun write-rel-file (filename &optional (package package))
  "Write out the contents of the tables to make the REL file."
  (prog (dump-stream previous-byte)
    (unwind-protect
      (progn
        (setq dump-stream (open filename '(write fixnum)))
        (write-file-beginning)
        (write-storage-format-version)
        (write-package-name)
        (write-symbols)
        (write-section-dictionary)
        (write-section-contents)
        (write-halfword 0)
        (write-halfword (+ %fasl-group-check fasl-op-end-of-file)))
      (and dump-stream (close dump-stream)))))

;Functions for writing the major components of a rel file.

(defun write-file-beginning ()
  (write-halfword #o143150)
  (write-halfword #o71660)
  (write-halfword (+ %fasl-group-check fasl-op-rel-file)))

(proclaim '(special current-storage-format-version))

(defun write-storage-format-version ()
  (write-opcode 'read-storage-format-version)
  (write-halfword current-storage-format-version))

(defun write-package-name ()
  (write-opcode 'read-package-name)
  (write-string (package-name package)))

(defun write-symbols ()
  (write-opcode 'read-symbols)
  (write-halfword (array-active-length dump-section-0-array))
  (dotimes (i (array-active-length dump-section-0-array))
    (write-string (aref dump-section-0-array i))
    (si:pkg-prefix (aref dump-section-0-array i)
                   #'(lambda (refname cnt)
                       cnt (write-string refname)))
    (write-halfword 0)))

(defun write-section-dictionary ()
  (write-opcode 'read-data)
  (write-halfword next-dump-section-number)
  (dotimes (i next-dump-section-number)
    (cond ((zerop i)
           (write-halfword (* 4 (array-active-length dump-section-0-array)))
           (write-halfword 0))
          (t
            (write-halfword (array-active-length (aref dump-section-array-table i)))
            (write-halfword (+ (aref dump-section-area-code i)
                               (if (aref dump-section-structure-p i)
                                   #o10000 0)))))))

(defun write-section-contents ()
  (do ((i 1 (1+ i))) ((= i next-dump-section-number))
    (let ((array (aref dump-section-array-table i))
          (section-length (array-active-length (aref dump-section-array-table i))))
      (dotimes (j section-length)
        (write-halfword (%p-ldb-offset %%q-low-half array
                                       (+ j dump-section-contents-offset)))
        (write-halfword (%p-ldb-offset %%q-high-half array
                                       (+ j dump-section-contents-offset)))))))

;Subroutines to write out part of a main data group.

(defun write-string (str &aux (length (string-length str)))
  (setq str (string str))
  (setq previous-byte nil)
  (write-halfword (1+ length))
  (dotimes (i length)
    (write-byte (aref str i)))
  (and (oddp length)
       (write-byte 0)))

(defun write-opcode (op-name)
  (write-halfword (find-position-in-list op-name op-list)))

(defun write-byte (byte)
  (cond (previous-byte (write-halfword (dpb byte 1010 previous-byte))
                       (setq previous-byte nil))
        (t (setq previous-byte byte))))

(defun write-halfword (halfword)
  (send dump-stream :tyo halfword))
