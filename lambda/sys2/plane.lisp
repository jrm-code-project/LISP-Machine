;;;-*- Mode:LISP; Package:SI; Base:8; Readtable:ZL -*-

;;; A PLANE is an array whose bounds, in each dimension,
;;; are plus-infinity and minus-infinity.  All integers are legal as indices.
;;; Planes are distinguished not by size and shape, but by number of dimensions alone.
;;; When a plane is created, a "default value" must be specified.
;;; At that moment, every component of the plane has that value.
;;; As you can't ever change more than a finite number of components,
;;; only a finite region of the plane need actually be stored.

;;; You can use MAKE-PLANE to create a plane,
;;; PLANE-REF or PLANE-AR-N to get the value of a component,
;;; PLANE-STORE or PLANE-AS-N to store into a component.
;;; ARRAY-RANK will work on a plane.

;;; A plane is actually stored as an array with a leader.
;;; The array corrsponds to a rectangular, aligned region of the plane,
;;; containing all the components in which a PLANE-STORE has been done
;;; (and others, in general, whcih have never been altered).
;;; The lowest-co-ordinate corner of that rectangular region is
;;; given by the PLANE-ORIGIN in the array leader.
;;; The highest co-ordinate corner can be found by adding the PLANE-ORIGIN
;;; to the ARRAY-DIMENSIONS of the array.
;;; The PLANE-DEFAULT is the contents of all the
;;; elements of the plane which are not actually stored in the array.
;;; The PLANE-EXTENSION is the amount to extend a plane by in any direction
;;; when the plane needs to be extended.  The default is 32.

(DEFSUBST PLANE-ORIGIN (PLANE)
  "Return the list of lowest-possible indices in each dimension."
  (ARRAY-LEADER PLANE 0))

(DEFSUBST PLANE-DEFAULT (PLANE)
  "Return the default element value for a plane.
This is the element which we pretend is stored
at all the locations which no storage is allocated for."
  (ARRAY-LEADER PLANE 1))

(DEFSUBST PLANE-EXTENSION (PLANE)
  "Return the distance to extend this plane by.
When it is necessary to make the plane's storage larger,
allocate this much extra in each dimension that needs to be extended."
  (ARRAY-LEADER PLANE 2))

(DEFUN PLANE-AREF (PLANE &REST POINT)
  "Return the value stored in PLANE for indices in POINT."
  (PLANE-REF PLANE POINT))

(DEFUN PLANE-ASET (DATUM PLANE &REST POINT)
  "Store DATUM into PLANE at indices POINT."
  (PLANE-STORE DATUM PLANE POINT))

(DEFSETF PLANE-AREF SET-PLANE-AREF)

(DEFMACRO SET-PLANE-AREF (PLANE &REST L)
  (LET ((INDS (BUTLAST L))
        (VALUE (CAR (LAST L))))
    `(,(OR (CADR (ASSQ (LENGTH INDS) '((1 SET-PLANE-AREF-1)
                                       (2 SET-PLANE-AREF-2)
                                       (3 SET-PLANE-AREF-3)
                                       (4 SET-PLANE-AREF-4))))
           (FERROR NIL "UNHANDLED DIMENSIONS, USE PLANE-ASET INSTEAD"))
      ,PLANE
      ,@INDS
      ,VALUE)))

(DEFUN SET-PLANE-AREF-1 (PLANE X VALUE)
  (WITH-STACK-LIST (POINT X)
    (PLANE-STORE VALUE PLANE POINT)))

(DEFUN SET-PLANE-AREF-2 (PLANE X Y VALUE)
  (WITH-STACK-LIST (POINT X Y)
    (PLANE-STORE VALUE PLANE POINT)))

(DEFUN SET-PLANE-AREF-3 (PLANE X Y Z VALUE)
  (WITH-STACK-LIST (POINT X Y Z)
    (PLANE-STORE VALUE PLANE POINT)))

(DEFUN SET-PLANE-AREF-4 (PLANE X Y Z S VALUE)
  (WITH-STACK-LIST (POINT X Y Z S)
    (PLANE-STORE VALUE PLANE POINT)))

(DEFF PLANE-AR-N 'PLANE-AREF)
(DEFF PLANE-AS-N 'PLANE-ASET)
(compiler:make-obsolete plane-ar-n "use PLANE-AREF")
(compiler:make-obsolete plane-as-n "use SETF of PLANE-AREF")

;;; Access the element of PLANE at co-ordinates POINT.
;;; Absolutely any point is legal.
(DEFUN PLANE-REF (PLANE POINT)
  "Return the value stored in PLANE at the indices in the list POINT."
  (DO ((PT POINT (CDR PT))
       (PO (PLANE-ORIGIN PLANE) (CDR PO)))
      ((NULL PT))
    (DECF (CAR PT) (CAR PO)))
  (IF (APPLY #'ARRAY-IN-BOUNDS-P PLANE POINT)
      (APPLY #'AREF PLANE POINT)
    (PLANE-DEFAULT PLANE)))

;;; Store DATUM in PLANE at co-ordinates POINT.
;;; PLANE is extended if necessary.
(DEFUN PLANE-STORE (DATUM PLANE POINT &AUX POINT1)
  "Store DATUM into PLANE at the indices in the list POINT."
  (SETQ POINT1 (MAPCAR #'- POINT (PLANE-ORIGIN PLANE)))
  (COND ((NOT (APPLY 'ARRAY-IN-BOUNDS-P PLANE POINT1))
         (PLANE-EXTEND PLANE POINT)
         (APPLY #'ASET DATUM PLANE (MAPCAR #'- POINT (PLANE-ORIGIN PLANE))))
        (T (APPLY #'ASET DATUM PLANE POINT1))))

(DEFUN PLANE-EXTEND (PLANE POINT
                     &AUX TOP-EXTEND BOTTOM-EXTEND NEW-PLANE
                     TEM OLD-DIMS (MIN (PLANE-EXTENSION PLANE)))
  "Make PLANE's storage larger so that storage is allocated for POINT.
POINT is a list of numerical indices."
  (SETQ OLD-DIMS (ARRAY-DIMENSIONS PLANE))
  (SETQ BOTTOM-EXTEND
        (MAPCAR #'(LAMBDA (PT OLD-BOT)
                    (SETQ TEM (- OLD-BOT PT))
                    (IF ( TEM 0) 0
                      (MAX TEM MIN)))
                POINT
                (PLANE-ORIGIN PLANE)))
  (SETQ TOP-EXTEND
        (MAPCAR #'(LAMBDA (PT OLD-BOT OLD-LEN)
                    (SETQ TEM (1+ (- PT OLD-BOT OLD-LEN)))
                    (IF ( TEM 0) 0
                      (MAX TEM MIN)))
                POINT
                (PLANE-ORIGIN PLANE)
                OLD-DIMS))
  (COND ((AND (ZEROP (APPLY #'+ BOTTOM-EXTEND))
              (ZEROP (APPLY #'+ TOP-EXTEND))))
        (T
          (SETQ NEW-PLANE (MAKE-PLANE-INTERNAL
                            (ARRAY-TYPE PLANE)
                            (MAPCAR #'+
                                    OLD-DIMS
                                    BOTTOM-EXTEND
                                    TOP-EXTEND)
                            (MAPCAR #'-
                                    (PLANE-ORIGIN PLANE)
                                    BOTTOM-EXTEND)
                            (PLANE-DEFAULT PLANE)
                            T
                            (PLANE-EXTENSION PLANE)))
          (PLANE-COPY PLANE NEW-PLANE)
          (STRUCTURE-FORWARD PLANE NEW-PLANE)))
  PLANE)

;;; Make a new plane, for the user.  Specify the number of dimensions,
;;; and optionally the array type, default value, and extension.
(DEFUN MAKE-PLANE (RANK &KEY (TYPE 'ART-Q) (ELEMENT-TYPE T)
                             (DEFAULT-VALUE NIL DEFAULT-P) (EXTENSION 32.)
                             INITIAL-DIMENSIONS INITIAL-ORIGINS)
  "Create an infinite plane of RANK dimensions, all elements containing DEFAULT-VALUE.
/(You can only alter finitely many of the elements, of course).
If you do not specify DEFAULT-VALUE, it defaults according to the type of array.
The type of the specified by the common-lisp ELEMENT-TYPE type-specifier or by
 is either TYPE (whose value should be an /"ART-foo/" symbol.)
EXTENSION is the minimum amount to extend
the allocated storage by, in any dimension, when it needs to be extended.
Use PLANE-AREF and PLANE-ASET to access the plane.

You can use the arguments INITIAL-ORIGINS and INITIAL-DIMENSIONS to specify
which part of the plane storage should initially be allocated for.
Each element of INITIAL-ORIGINS is the first index, in one dimension,
for which space is allocated.  The corresponding element of INITIAL-DIMENSIONS
specifies how much space is allocated in that dimension.
The origins default to a list of RANK zeros, and the dimensions to
a list of RANK ones."
  (CHECK-TYPE RANK FIXNUM)
  (if element-type (setq type (array-type-from-element-type element-type)))
  (MAKE-PLANE-INTERNAL TYPE
                       ;; SIZE is a list of 1's, as many as there are dimensions.
                       (OR INITIAL-DIMENSIONS (MAKE-LIST RANK :INITIAL-VALUE 1))
                       ;; ORIGIN gets a similar list of zeroes.
                       (OR INITIAL-ORIGINS (MAKE-LIST RANK :INITIAL-VALUE 0))
                       DEFAULT-VALUE
                       DEFAULT-P
                       EXTENSION))

;;; Create a new plane of specified type (an array type) and default value,
;;; with a specified region in actual existence.
(DEFUN MAKE-PLANE-INTERNAL (TYPE SIZE ORIGIN DEFAULT DEFAULT-P EXTENSION &AUX PLANE)
  (IF DEFAULT-P
      (SETQ PLANE (MAKE-ARRAY SIZE :TYPE TYPE :LEADER-LENGTH 3 :INITIAL-VALUE DEFAULT))
      (SETQ PLANE (MAKE-ARRAY SIZE :TYPE TYPE :LEADER-LENGTH 3)))
  (SETQ DEFAULT (AR-1-FORCE PLANE 0))
  (SETF (PLANE-DEFAULT PLANE) DEFAULT)
  (SETF (PLANE-ORIGIN PLANE) ORIGIN)
  (SETF (PLANE-EXTENSION PLANE) EXTENSION)
  PLANE)

(DEFUN PLANE-COPY (OLD NEW &AUX OLD-ORIGIN NEW-ORIGIN OLD-DIMS
                                OLD-INDICES NEW-INDICES)
  "Copy all the allocated contents of the plane OLD into the plane NEW.
Assumes that storage is already allocated in NEW
for the range of indices that correspond to OLD."
  (PROG ()
        ;; OLD-ORIGIN and NEW-ORIGIN are the origins (lowest corners) of the planes.
        ;; OLD-DIMS is the list of actual dimensions of the old plane.
        (SETQ OLD-ORIGIN (PLANE-ORIGIN OLD))
        (SETQ NEW-ORIGIN (PLANE-ORIGIN NEW))
        (SETQ OLD-DIMS (ARRAY-DIMENSIONS OLD))
        (AND (ZEROP (APPLY #'+ OLD-DIMS)) (RETURN NEW))
        ;; OLD-INDICES has the real indices in the old plane of a point.
        ;; NEW-INDICES has the corresponding indices in the new plane.
        ;; We update both lists simultaneously by RPLACA to avoid consing.
        (SETQ OLD-INDICES (MAPCAR #'- OLD-ORIGIN OLD-ORIGIN))
        (SETQ NEW-INDICES (MAPCAR #'- OLD-ORIGIN NEW-ORIGIN))
     LOOP
        (APPLY #'ASET (APPLY #'AREF OLD OLD-INDICES) NEW NEW-INDICES)
        (OR (DO ((OI OLD-INDICES (CDR OI))
                 (NI NEW-INDICES (CDR NI))
                 (DIMS OLD-DIMS (CDR DIMS))
                 (NEW-ORIGIN NEW-ORIGIN (CDR NEW-ORIGIN))
                 (OLD-ORIGIN OLD-ORIGIN (CDR OLD-ORIGIN)))
                ((NULL OI))
              (INCF (CAR OI))
              (OR (< (CAR OI) (CAR DIMS))
                  (RPLACA OI 0))
              (SETF (CAR NI) (+ (- (CAR OI) (CAR NEW-ORIGIN)) (CAR OLD-ORIGIN)))
              (OR (ZEROP (CAR OI))
                  (RETURN T)))
            (RETURN NEW))
        (GO LOOP)))
