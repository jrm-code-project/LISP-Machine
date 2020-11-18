;;; -*- Mode:LISP; Package:USER; Base:10; Readtable:CL; compile-in-roots: ("GLOBAL") -*-

;;;**************************************************************************************************
;;;*****   This file should be identical to k-macros(-for-k-debugger) at all times WKF 5/5/88   *****
;;;**************************************************************************************************

;;; These duplicate versions are needed to fool make-system since COMPILER-FOR-K and K-DEBUGGER
;;;  both use this file and k-debugger loads it in the k-user hierarchy.

;;; The macros in this file are used by:
;;        CONVERSIONS.LISP, MEMORY-MAP.LISP, QUANTUM-MAP.LISP, PCD-TABLE.LISP, REGION-BITS.LISP
;;        AREA-DATA.LISP,
;;        GC-RAM.LISP, TRANSPORTER-RAM.LISP, NUBUS-INTERRUPTS.LISP, ARRAY.LISP,
;;        KOLD-LOADER.QFASL, ARRAY2.KFASL, CONTROL-PDL.KFASL, MAP-FAULT.KFASL, TYPE-PREDICATES.KFASL,
;;        K2.KFASL, WARM-LOADER.KFASL

;;WKF 5/5/88 removed : from front of compile-in-roots above.

;compile-in-roots: ("COMPILER-PACKAGE-HIERARCHY")
; LOAD-QFASL-IN-ROOTS: ("COMPILER-PACKAGE-HIERARCHY" "SIMULATION-PACKAGE-HIERARCHY") -*-

;is to be loaded in both hierarchies, but compiled only in GLOBAL.  For now,
; we will wind up READFILEing in the K-GLOBAL hierarchy until the load-qfasl-in-roots frob really
; exists.

;these moved to VINC and export eliminated.  Reference package explicitly as needed.
;(export '(
;         prims::defextractor
;         prims::defflag-extractor
;         prims::dpb-multiple-boxed
;         prims::dpb-multiple-unboxed
;         )
;       (find-package "PRIMITIVES" *package*))

;;;;;;;;;;;;;;;;;
;;; DEFEXTRACTOR
;;;;;;;;;;;;;;;;;

(prims:defmacro vinc::defextractor (name field)
  `(progn
     (PRIMS::DEFSUBST ,name (PRIMS::Q)
       (HW::LDB PRIMS::Q ,field 0.))
     (PRIMS::DEFSETF ,name (PRIMS::Q) (PRIMS::VALUE)
       `(HW:DPB ,prims::value ,',field ,prims::q))))

(prims:defmacro vinc::defflag-extractor (name field value)
  `(PRIMS::DEFSUBST ,name (PRIMS::Q)
     (HW::32= (HW::LDB PRIMS::Q ,field 0.) ,value)))

;;;;;;;;;;;;;;;;;
;;; DPB Multiple
;;;;;;;;;;;;;;;;;

(prims:defmacro vinc::dpb-multiple-boxed (&rest fields)
  (dpb-multiple-expander t fields))

(prims:defmacro vinc::dpb-multiple-unboxed (&rest fields)
  (dpb-multiple-expander nil fields))

(defun dpb-multiple-expander (boxed-result? fields)
  (cond ((null fields) (ferror nil "Even number of arguments to ~s" 'dpb-multiple))
        ((null (rest fields)) (first fields))
        (t `(,(if boxed-result?
                  (intern "DPB-BOXED" (find-package "HW" *package*))
                  (intern "DPB-UNBOXED" (find-package "HW" *package*)))
             ,(first fields) ,(second fields)
             ,(dpb-multiple-expander nil (rest (rest fields)))))))


;;;;;;;;;;;;;;;;;
;;; Bytespec operations
;;;;;;;;;;;;;;;;;

;;; These are tools for manipulating byte specifiers.  The idea is that you
;;; wouldn't need as big a vocabulary of byte specifiers if a certain minimal
;;; set could be combined in certain ways.
;;;   BYTE-UNION        Takes several byte specifiers and tries to produce a single
;;;                     byte specifier which describes all the fields together.
;;;                     This can only work if the argument fields form a contiguous
;;;                     field.  (byte-union (byte 4 3) (byte 2 7)) ==> (byte 6 3)
;;;   BYTE-COMPOSITION  This is useful if you want to do a byte operation on a datum
;;;                     which is embedded in a larger datum.  The second bytespec
;;;                     describes the field in the big datum where the little datum
;;;                     is stored.  The first bytespec is the field (as it appears
;;;                     in the little datum) that you want to manipulate.  The result
;;;                     is a bytespec describing where in the big datum the subfield
;;;                     that you want to operate on from the little datum is.
;;;                     (byte-composition (byte 2 4) (byte 10 3)) ==> (byte 2 7)
;;;   BYTE-INTERSECTION For the byte specifiers provided as arguments, byte-intersection
;;;                     produces a single byte specifier which describes a field of those
;;;                     bits which are in common to all of the argument bytespecs.
;;;                     (byte-intersection (byte 10 5) (byte 5 10)) ==> (byte 5 5)
;;;   BYTE-IN-BYTE      Given two byte specifiers which both describe fields in
;;;                     a datum, this computes a byte specifier which locates the field
;;;                     described by the first bytespec in the field described by the second.
;;;                     (byte-in-byte (byte 4 18) (byte 16 16)) ==> (byte 4 2)

;;; union
(defun byte-union1 (&rest byte-specs)
  "Given a set of byte specifiers which describe a contiguous field,
return a single byte specifier for the entire field."
  (setq byte-specs (sort (copy-list Byte-specs) '< :key 'byte-position))
  ;;; make sure they are contiguous
  (do ((specs byte-specs (cdr specs)))
      ((null (cdr specs)) t)
    (unless (>= (+ (byte-size (car specs))
                   (byte-position (car specs)))
                (byte-position (cadr specs)))
      (error nil "byte specs are not contiguous (BYTE ~d. ~d.) (BYTE ~d. ~d.)"
              (byte-size (car specs)) (byte-position (car specs))
              (byte-size (cadr specs)) (byte-position (cadr specs)))))
  ;;; position is position of first one
  ;;; size is position of last plus size of last minus position of first
  (let ((last (car (last byte-specs))))
    (byte  (+ (- (byte-position last)
                 (byte-position (first byte-specs)))
              (byte-size last))
           (byte-position (first byte-specs)))))

(defun prims:byte-union (&rest byte-specs)
  (if (and byte-specs (every byte-specs #'byte-macro-foldable))
      (apply #'byte-union1 (mapcar #'eval byte-specs))
    (error nil "BYTE-UNION can't cope with its arguments")))

;;; composition
(defun byte-composition1 (subspec of-spec)
  "Of-spec is a byte specifier describing a field foo of bar.  subspec is a byte specifier
describing a field baz of foo.  Returns a byte-spec describing field baz of bar."
  ;;; a validity check
  (unless (<= (+ (byte-position subspec)
                 (byte-size subspec))
              (byte-size of-spec))
    (error nil "(BYTE ~d. ~d.) does not fit in (BYTE ~d. ~d.)"
            (byte-size subspec)
            (byte-position subspec)
            (byte-size of-spec)
            (byte-position of-spec)))
  (byte (byte-size subspec)
        (+ (byte-position of-spec)
           (byte-position subspec))))

;;This macro is not used any more WKF.  5/12/88
;(defmacro prims:byte-composition (subspec of-spec)
;  (if (and (byte-macro-foldable subspec)
;          (byte-macro-foldable of-spec))
;      (byte-composition1 (eval subspec) (eval of-spec))
;     `(byte (byte-size ,subspec)
;           (+ (byte-position ,of-spec)
;              (byte-position ,subspec)))))

(defun prims:byte-intersection (&rest bytes)
  "Make a byte specifier describing those bits which are in all of BYTES"
  (flet ((2-byte-intersect (byte1 byte2)
           (let* ((start1 (byte-position byte1))
                  (start2 (byte-position byte2))
                  (start (max start1 start2))
                  (end (min (+ start1 (byte-size byte1))
                            (+ start2 (byte-size byte2)))))
             (byte (- end start) start))))
    (do* ((byte-spec (car bytes)
                     (2-byte-intersect byte-spec (car byte-specs)))
          (byte-specs (cdr bytes) (cdr byte-specs)))
         ((null byte-specs)
          byte-spec))))

;;; subtraction???
(defun byte-in-byte1 (byte in-byte &optional (safe nil))
  "Given two byte specifiers BYTE and IN-BYTE into a word, return
a byte specifier describing the field BYTE as it appears in IN-BYTE."
  (when (and safe
             (< (byte-position byte)
                (byte-position in-byte))
             (> (+ (byte-position byte)
                   (byte-size byte))
                (+ (byte-position in-byte)
                   (byte-size in-byte))))
    (error nil "byte spec (BYTE ~d. ~d.) is not contained within (BYTE ~d. ~d.)"
            (byte-size byte) (byte-position byte)
            (byte-size in-byte) (byte-position in-byte)))
  (let ((bi (byte-intersection byte in-byte)))
    (byte (byte-size bi)
          (- (byte-position bi) (byte-position in-byte)))))

;;This macro is not being used any more wkf.  5/12/88
;(defmacro prims:byte-in-byte (byte in-byte)
;  (if (and (byte-macro-foldable byte)
;          (byte-macro-foldable in-byte))
;      (byte-in-byte1 (eval byte) (eval in-byte))
;    (error nil "BYTE-IN-BYTE can't cope with its arguments")))

(defun byte-macro-foldable thing
  (or (constantp thing)
      (and (listp thing)
           (eq (first thing) 'prims:byte)
           (constantp (second thing))
           (constantp (third thing)))))
