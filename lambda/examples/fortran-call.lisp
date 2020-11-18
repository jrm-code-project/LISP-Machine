#| -*- Mode:LISP; Package:(FCALL global); Base:10 -*-

 Copyright LISP Machine, Inc. 1985, 1986
   See filename "Copyright" for
 licensing and release information.

A self-contained example of how to build an interprocessor
FORTRAN-CALL library mechanism. 5/18/85 13:16:28 -George Carrette

*********************************************************
*********************************************************
*** NOTE: This is an EXAMPLE, not LMI supported code. ***
*** information contained in this example is subject  ***
*** to change without notice. The ways of doing       ***
*** the things contained in the example may change    ***
*** between system releases. Some techniques which    ***
*** are mere examples in one release may become built ***
*** in system features in the next release. Use good  ***
*** judgement when copying these techniques. Most     ***
*** examples have been motivated by specific customer ***
*** requests, and may not be the best engineered      ***
*** or most efficient solution for someone else.      ***
*********************************************************
*********************************************************

The goal would be to have a system such that if,

We define:

(DEFINE-FORTRAN-LIBRARY <NAME> "documentation"
  :executable-image "/usr/flib/streamlib"

  :subroutine (FFT (X :REAL-ARRAY)
                   (Y :REAL-ARRAY)
                   (N :INTEGER))

  :subroutine (IFT (X :REAL-ARRAY)
                   (Y :REAL-ARRAY)
                   (N :INTEGER)))


Then:
  (GENERATE-MAIN-PROGRAM '<NAME>) ==> causes a main program in "C" to be generated.
  (GENERATE-EXECUTABLE '<NAME>)   ==> compiles the "C" program and calls the linker.

Before a subroutine may be called there must be an executable program started
up on the connected shared processor, to do this we would instantiated a
job, duty or processing thread, whatever you want to call it.

Prequisite to run:

  (SETQ *ABLE* (INSTANTIATE-FORTRAN-LIBRARY '<NAME>))


To run:

  (SETQ X (MAKE-ARRAY 16 :ELEMENT-TYPE 'FLOAT))
  (SETQ Y (MAKE-ARRAY 16 :ELEMENT-TYPE 'FLOAT))

  (SEND *ABLE* :CALL "FFT" X Y 16)

A simple :CALL such as the above would not return until the computation
on the coprocessor had completed. However, that need not be the case,
we could say instead:

  (SEND *ABLE* :CO-CALL "FFT" X Y 16)

  Which would return immediately. To found out when the computation is
  complete and the values in the arrays X and Y are consistent we send
  a message:

  (EQ (SEND *ABLE* :STATE) :DONE)  => true if the computation is complete.

An example of doing two things at the same time would be:

 (LET ((DONE NIL))
   (PROCESS-RUN-FUNCTION "symbolic way"
                         #'(lambda ()
                            (find-the-answer-using-AI-techniques X Y Z)
                            (setq done t)))
   (SEND *ABLE* :CO-CALL "SOLVE9" X Y Z)
   (PROCESS-WAIT "thinking" #'(lambda () (or done (eq :done (send *able* :STATE)))))
   (if done
       (format t "The answer was found first using AI techniques")
     (format t "The answer was found first using the old FORTRAN numerical way")))




Implementation:
  fort.h   A file of declarations shared by all files.
  fmain.c  The main subroutine, which opens the syncronizing device,
           initializes pointers to shared memory, and calls fexec
           for each subroutine call request.
  fexec.c  This gets an opcode from shared memory, and calls fload to
           get a link (dynamically or statically) to a FORTRAN subroutine.
           It then sets up an argument vector by normalizing the arglist
           in shared memory by the base of shared memory in the
           user process. The linked FORTRAN subroutine is then called,
           and a shared memory location is set to zero to indicate
           completion of the subroutine.
  fload.c  In our example this is a static linking of an array of
           pointers to FORTRAN subroutines.
  ftable.c The dispatch table of FORTRAN subroutines.
  flib.f   The fortran subroutines.

Note: In the example we do not address the issue of interlocking
      of shared memory resources such as would be needed with
      multiple instantiations of fortran libraries in or across
      lambda processors.

|#


(defvar *unix-version* :sys5 "can be :v7 or :sys5")


(defmacro DEFINE-FORTRAN-LIBRARY (name documentation &rest body)
  `(*define-fortran-library ',name ,documentation ',body))

(defun *define-fortran-library (name documentation arguments)
  (when (record-source-file-name name 'define-fortran-library)
    (setf (function-documentation name 'fortran-library) documentation)
    (do ((l arguments (cddr l))
         (plist))
        ((null l)
         (setf (get name 'fortran-library) plist))
      (ecase (car l)
        ((:unix-filename-append :executable-image :link-table :files)
         (setf (getf plist (car l)) (cadr l)))
        (:subroutine
         (let ((subroutine (if (atom (cadr l)) (list (cadr l)) (cadr l))))
           (push subroutine (getf plist :subroutines))))))))

(defflavor fortran-image
           ((name nil) sync-stream (processor nil)
            eval-server-process
            (eval-server-output "")
            command
            return-values
            being-called
            (flushed nil)
            opcode-pointer)
           ()
  :initable-instance-variables
  :settable-instance-variables)


(defmethod (fortran-image :print-self) (stream &rest ignored)
  (format stream "#<FORTRAN-IMAGE ~A running on ~A ~A>"
          name processor
          eval-server-output))

(defun instantiate-fortran-library (name &optional (port-number (ecase *unix-version*
                                                                  (:sys5 4)
                                                                  (:v7 5))))
  ;; in fact the port number should be auto incremented
  (check-arg name (fortran-libraryp name) "a fortran library")
  (let ((port (open (format nil "UNIX-STREAM-~D:" port-number)))
        (proc (attached-unix-host))
        (os (make-array 200 :fill-pointer 0 :element-type 'string-char))
        (command (format nil "~a //dev//ttyl~D"
                         (send (fortran-library-pathname name :executable-image)
                               :string-for-host)
                         port-number)))
    (let ((obj (make-instance 'fortran-image
                              :name name
                              :sync-stream port
                              :processor proc
                              :eval-server-output os
                              :command command)))
      (send obj :start-process)
      (process-wait "unix startup"
                    #'(lambda (x)
                        (> (length (send x :eval-server-output))
                           (length (send x :command))))
                    obj)
      obj)))


(defmethod (fortran-image :start-process) ()
  (setq eval-server-process
        (process-run-function
          (format nil "fortran library ~A" name)
          #'fortran-image-simple-unix-eval
          processor
          command
          (make-string-output-stream eval-server-output)
          self)))


(defun fortran-image-simple-unix-eval (processor command output done)
  (simple-unix-eval processor command output)
  (send done :set-flushed t))


(defvar *shared-memory-type-array* ())

(defvar *value-free-pointer* 0)

(defun allocate-value-space ()
  ;; this is where interlocking between processors might happen.
  (or (and *shared-memory-type-array*
           (= (length *shared-memory-type-array*)
              (quotient (length si:*global-shared-memory-16*) 2)))
      (setq *shared-memory-type-array*
            (make-array (quotient (length si:*global-shared-memory-16*) 2))))
  (setq *value-free-pointer* 0))

(defsetf read-value write-value)

(defun read-value (location type)
  "Read a value and do representation conversion"
  (let ((n (dpb (aref si:*global-shared-memory-16* (1+ (ash location 1)))
                #o2020
                (aref si:*global-shared-memory-16* (ash location 1)))))
    (ecase type
      (:real
       (ecase *unix-version*
         (:v7
          (68000-float->lispm n))
         (:sys5
          (ieee-32b n))))
      (:integer
       (signed-68000->lispm n))
      (nil
       n))))

(defun write-value (location type value)
  "Write a value of a given type into the shared location, doing representation
conversion for types of :REAL and :INTEGER."
  (let ((n (ecase type
             (:real
              (ecase *unix-version*
                (:v7
                 (lispm-float->68000 value))
                (:sys5
                 (ieee-32b-bits value))))
             (:integer
              (signed-lispm->68000 value)))))
    (setf (aref si:*global-shared-memory-16* (ash location 1))
          (ldb #o0020 n))
    (setf (aref si:*global-shared-memory-16* (1+ (ash location 1)))
          (ldb #o2020 n))
    (setf (aref *Shared-memory-type-array* location) type)))

(defun push-value (value type)
  (setf (read-value *value-free-pointer* type) value)
  (incf *value-free-pointer*))

(defun read-known-value (location)
  (read-value location (aref *shared-memory-type-array* location)))


(defun dump-value-space (&optional (end (length *shared-memory-type-array*)))
  (format t "~&Location Type   Value~%")
  (dotimes (j end)
    (let ((type (aref *shared-memory-type-array* j)))
      (format t "~4D     ~7A ~S~%" J TYPE (read-value j type)))))


(defmethod (fortran-image :co-call) (subroutine &rest arguments)
  (let* ((subroutines (getf (get name 'fortran-library) :subroutines))
         (desc (ass #'string-equal subroutine subroutines)))
    (or desc (ferror nil "No subroutine called ~A in fortran library ~A"
                     subroutine name))
    (setq being-called desc)
    (let ((nargs (length arguments))
          (opcode (1+ (find-position-in-list desc subroutines)))
          (argspointer))
      (or (= nargs
             (length (cdr desc)))
          (ferror nil "Fortran subroutine ~A expects ~D argument~p, got ~D"
                  subroutine
                  (length (cdr desc))
                  (length (cdr desc))
                  nargs))
      (allocate-value-space)
      (setq opcode-pointer *value-free-pointer*)
      (push-value opcode :integer)
      (push-value nargs :integer)
      (setq argspointer *value-free-pointer*)
      (incf *value-free-pointer* nargs)
      (setq return-values nil)
      (do ((actuals arguments (cdr actuals))
           (formals (cdr desc) (cdr formals))
           (j 0 (1+ j)))
          ((null actuals))
        (let ((pointer *value-free-pointer*)
              (datatype (cadr (car formals)))
              (data (car actuals)))
          (if (typep data 'sequence)
              (push (list data datatype pointer) return-values))
          (setf (read-value (+ argspointer j) :integer) pointer)
          (ecase datatype
            ((:real :integer)
             (push-value (if (typep data 'sequence)
                             (elt data 0)
                           data)
                         datatype))
            ((:real-array :integer-array)
             (let ((element-type (cadr (assq datatype '((:real-array :real)
                                                        (:integer-array :integer))))))
               (dotimes (k (length data))
                 (push-value (elt data k) element-type)))))))
      (send sync-stream :tyo #/X)
      (send sync-stream :tyo 13)
      (send sync-stream :force-output)
      )))


(defmethod (fortran-image :stop) ()
  (cond ((not flushed)
         (send sync-stream :tyo #/S)
         (send sync-stream :tyo 13)
         (send sync-stream :force-output)
         (process-wait "unix to stop"
                       #'(lambda (x) (send x :flushed))
                       self)
         (close sync-stream)
         (setq sync-stream nil)
         (setq eval-server-process nil)))
  self)

(defmethod (fortran-image :state) ()
  (if (zerop (read-value opcode-pointer :integer)) :done :computing))


(defmethod (fortran-image :call) (subroutine &rest arguments)
  (lexpr-send self :co-call subroutine arguments)
  (process-wait "computing" #'(lambda (x) (eq :done (send x :state))) self)
  (send self :get-return-values))

(defmethod (fortran-image :get-return-values) ()
  (dolist (item return-values)
    (let ((sequence (car item))
          (pointer (caddr item)))
      (dotimes (j (length sequence))
        (setf (elt sequence j) (read-known-value (+ pointer j)))))))


(compile-flavor-methods fortran-image)


(defun simple-unix-eval (host command &optional (stream standard-output))
  (with-open-stream (s (chaos:open-stream host
                                          (format nil "EVAL ~a" command)))
    (format stream "~&% ~A~%" command)
    (do ((c (send s ':tyi) (send s ':tyi)))
        ((null c))
      (send stream ':tyo
            (selectq c
              ((#o12 #o15) #\return)
              (#o11 #\tab)
              (t c))))))

(defun attached-unix-host ()
  "Returns host object for attached unix-host if it exits otherwise NIL"
;; Relevant variable:
;; si:*other-processors* list of structures of type SI:OTHER-PROCESSOR
  (dolist (op si:*other-processors*)
    (let ((host (SI:GET-HOST-FROM-ADDRESS
                  (si:%processor-conf-chaos-address (si:op-proc-conf op))
                  ':CHAOS)))
      (if (typep host 'fs:unix-host)
          (return host)))))

;; data conversion:
;; In the lisp environment all 68000 data is manipulated as 32 bit integers.

(defun 68000-byteswap (integer)
  (dpb (ldb #o0010 integer)
       #o3010
       (dpb (ldb #o1010 integer)
            #o2010
            (dpb (ldb #o2010 integer)
                 #o1010
                 (ldb #o3010 integer)))))

(defun signed-lispm->68000 (integer)
  (68000-byteswap integer))

(defun signed-68000->lispm (integer)
  (let ((n (68000-byteswap integer)))
    (if (bit-test 1_31 n)
        (- n 1_32)
      n)))

;; "68000" is the V7 Unix format


(defun 68000-float->lispm (x)
  "Take 32bits, a 68000 float, and return a lisp float object"
  (// (* (expt -1 (ldb #o3701 x))
         (expt 2.0 (- (ldb #o3007 x) #o100))
         (+ (ldb #o2010 x)
            (ash (+ (ldb #o1010 x)
                    (ash (ldb #o0010 x)
                         8))
                 8)))
      #o100000000))


(defun lispm-float->68000 (x &aux sign exp frac)
  "Take a lispmachine floating point number and return 32 bits suitable for the 68000"
  (cond ((zerop x)
         (return-from lispm-float->68000 0))
        ((< x 0.0)
         (setq sign 1)
         (setq x (- x)))
        ('else
         (setq sign 0)))
  (etypecase x
    (short-float
     (setq exp (+ (- (si:%short-float-exponent x) #o200) #o100))
     (setq frac (ash (si:%short-float-mantissa x)
                     (- 23 16))))
    (single-float
     (setq exp (+ (- (si:%single-float-exponent x) #o2000) #o100))
     (setq frac (ash (si:%single-float-mantissa x)
                     (- 23 30)))))
  (if (or (< exp 0)
          (> exp #o177))
      (ferror nil "Exponent too big to represent in 68000: ~S" x))
  (dpb sign #o3701
       (dpb exp #o3007
            (dpb (ldb #o0010 frac) #o2010
                 (dpb (ldb #o1010 frac) #o1010
                      (ldb #o2010 frac))))))

;; system V uses ieee representation.
;; These functions are good enough for demonstration.

(defun ieee-32b-value (sign exp frac)
  "This is from the definition of IEEE format"
  (if (zerop exp)
      0.0
    (* (expt -1 sign)
       (expt 2.0 (- exp 127))
       (1+ (quotient (float frac) (expt 2 23))))))

(defun ieee-32b-components (x &aux sign exp frac)
  "Returns 3 fixnum values: sign-bit exponent-field fraction-field"
  (etypecase x
    (short-float
     (cond ((zerop x)
            (return-from ieee-32b-components (values 0 0 0)))
           ((minusp x)
            (setq sign 1)
            (setq x (- x)))
           (t
            (setq sign 0)))
     (setq exp (+ (- (si:%short-float-exponent x) #o201) 127))
     (setq frac (ash (- (si:%short-float-mantissa x) (expt 2 16)) (- 23 16))))
    (single-float
     (cond ((zerop x)
            (return-from ieee-32b-components (values 0 0 0)))
           ((minusp x)
            (setq sign 1)
            (setq x (- x)))
           (t
            (setq sign 0)))
     (setq exp (+ (- (si:%single-float-exponent x) #o2001) 127))
     (setq frac (ash (- (si:%single-float-mantissa x) (expt 2 30)) (- 23 30)))))
  (or (<= 1 exp 254)
      (ferror nil "Exponent won't fit in 32b ieee format: ~S" x))
  (values sign exp frac))

(defun ieee-32b-bits (x)
  "returns the IEEE bit representation as a bignum"
  (multiple-value-bind (sign exp frac)
      (ieee-32b-components x)
    (68000-byteswap (dpb sign (byte 1 31)
                         (dpb exp (byte 8 23)
                              (dpb frac (byte 23 0) 0))))))

(defun ieee-32b (bits)
  "takes the IEEE bit representation as a bignum, returns a flonum"
  (setq bits (68000-byteswap bits))
  (ieee-32b-value (ldb (byte 1 31) bits)
                  (ldb (byte 8 23) bits)
                  (ldb (byte 23 0) bits)))


(defun ieee-32b-test (x &optional noprint)
  (multiple-value-bind (a b c)
      (ieee-32b-components x)
    (let ((new-x (ieee-32b-value a b c)))
      (or noprint (format t "~&~S => ~S (~D ~D ~D)~%" x new-x a b c)))
    (let ((n (ieee-32b-bits x)))
      (let ((new-x (ieee-32b n)))
        (or noprint (format t "~S => ~S (#10R~D #8R~8R #16r~16R)" x new-x n n n))
        (values new-x
                (abs (quotient (- x new-x) x)))))))




;; generation of code and linking.


(defun fortran-libraryp (name)
  (get name 'fortran-library))

(defun fortran-library-pathname (name type &optional (to-host (attached-unix-host)))
  (let ((p (fortran-libraryp name)))
    (let ((l (getf p type))
          (a (getf p :unix-filename-append)))
      (if (and (symbolp a) (boundp a))
          (setq a (symeval a)))
      (cond ((atom l)
             (fs:parse-pathname (if a (string-append a l) l) to-host))
            ('else
             (mapcar #'(lambda (x) (fs:parse-pathname (if a (string-append a x) x)
                                                      to-host))
                     l))))))

(defun generate-main-program (name &aux plist subroutines)
  "generates the link table for fortran library NAME"
  (check-arg name (setq plist (fortran-libraryp name)) "a fortran library")
  (setq subroutines (mapcar #'(lambda (x)
                                (if (> (length (cdr x)) 9)
                                    (ferror nil
                                            "Too many arguments in subroutine: ~S"
                                            x))
                                (string-downcase (car x)))
                            (getf plist :subroutines)))
  (ecase *unix-version*
    (:sys5
     (setq subroutines (mapcar #'(lambda (x) (string-append x "_")) subroutines)))
    (:v7))
  (with-open-file (s (fortran-library-pathname name :link-table)
                     :out)
    (format s "~
              ~70,,,'*<//~;~>~
              ~%~70< * Automatically generated C code for the FORTRAN link ~;*~>~
              ~%~70< * Compiled from ~S by ~S ~;*~>~
              ~%~70< * ~A ~;*~>~
              ~%~71,,,'*< *~;//~>~
              ~%~%#include /"fort.h/"~2%~
              ~%~{int ~a();~%~}~
              ~%int (*ftable[])() = {noop,~%~{ ~a~^,~%~}};~%"
              name
              si:user-id
              (time:print-current-date nil)
              subroutines
              subroutines
              )))



(defun generate-executable (name &optional (host (attached-unix-host)))
  (check-arg name (fortran-libraryp name) "a fortran library")
  (flet ((hosty (x) (send x :string-for-host)))
    (simple-unix-eval host
                      (format nil "cd ~A; make; mv mainprog ~A" *udir*
                              (hosty (fortran-library-pathname name :executable-image))))))

;; the example

(defvar *udir* "//usr//gjc//fort//")

(define-fortran-library example "an example"
  :subroutine (VADD (X :real-array)
                    (y :real-array)
                    (z :real-array)
                    (n :integer))
  :subroutine (vmult (X :real-array)
                     (y :real-array)
                     (z :real-array)
                     (n :integer))
  :subroutine (vprint (x :real-array)
                      (n :integer))
  :unix-filename-append *udir*
  :executable-image "example"
  :link-table "ftable.c"
  :files ("fmain.o" "fload.o" "fexec.o" "flib.o"))


(defun setup-unix-source-files (&optional (h (string-append
                                               (send (attached-unix-host)
                                                     :name)
                                               ":")))
  (let (ofile)
    (unwind-protect
        (with-open-file (s (send (si:get-source-file-name 'setup-unix-source-files )
                                 :new-pathname :type "LISP" :version :NEWEST))
          (do ((line))
              ((null (setq line (readline s nil))))
            (cond ((string-equal "Filename:" line
                                 :end2 (length "Filename:"))
                   (when ofile
                     (close ofile)
                     (setq ofile nil))
                   (setq line (string-append h *udir*
                                             (string-trim
                                               '(#\space)
                                               (substring line
                                                          (length "Filename:")))))
                   (print line)
                   (setq ofile (open line :direction :Output)))
                  ((string-equal "||#" line :end2 (length "||#"))
                   (when ofile
                     (close ofile)
                     (setq ofile nil)))
                  (ofile
                   (princ line ofile)
                   (terpri ofile)
                   (princ ".")))))
      (and ofile (close ofile)))))

#||

To run the example, create a directory /usr/gjc/fort/
and put the Unix sources in it with the lisp command:

  (setup-unix-source-files)

Then generate ftable.c with the command:

  (generate-main-program 'example)

Then compile and link everything with the command:

  (generate-executable 'example)

Not instantiate a runnable image of the library:

  (setq lib (instantiate-fortran-library 'example))

  (setq x '(1.0 2.0 3.0))
  (setq y '(1.0 2.0 3.0))
  (setq z '(0.0 0.0 0.0))


  (send lib :call "VMULT" x y z 3)


The C and fortran code follows:
< (setup-unix-source-files) automatically unpacks these>

Filename: fort.h

/**************************************
 * Copyright LISP Machine, Inc. 1985  *
 *  See filename "Copyright" for      *
 * licensing and release information. *
 **************************************/

int noop();

int (*(fload()))();

int (*ftable[])();

Filename: fmain.c

/**************************************
 * Copyright LISP Machine, Inc. 1985  *
 *  See filename "Copyright" for      *
 * licensing and release information. *
 **************************************/

#include <stdio.h>
#include <sys/types.h>
#include <sys/termio.h>
#include <share.h>

main(argc,argv)
  int argc; char **argv;
{
  int n, chan;
  char c;
  struct termio t;

  if (argc != 2) {
    printf("\n%s <syncronizing-device>\n",argv[0]);
    exit(1);
  }
  chan = open(argv[1],2);
  ioctl(chan, TCGETA, &t);
  t.c_lflag &= ~(ECHO|ECHOE|ECHOK);
  ioctl(chan, TCSETA, &t);

  if (chan < 0) {
    printf("\ncouldnt open syncronizing device: %s\n",argv[1]);
    exit(1);
  }
  if (share_setup() < 0) {
    printf("share_setup failed");
    exit(1);
  }
  while(1) {
    n = read(chan,&c,1);
    if (n != 1) {
      printf("\nsyncronizing device read failed\n");
      exit(1);
    }
    if ( c == 'S') {
      printf("\nbeen told to stop\n");
      exit(1);
    }
    if ( c == 'X')
       fexec(sharebase);
  }
}



Filename: fexec.c

/**************************************
 * Copyright LISP Machine, Inc. 1985  *
 *  See filename "Copyright" for      *
 * licensing and release information. *
 **************************************/

/* execute a subroutine call out of arguments passed in the heap.
   All arguments are call-by-reference, which is FORTRAN style.

   Heap format:

     0    [Opcode]
     1    [Number-Of-Arguments]
     2    [ARG1]
     3    [ARG2]
     ...
     9    [ARG9]
     10   begining of actual argument value storage heap

*/

#include "fort.h"
#include <stdio.h>

fexec(heap) int *heap;
 { int (*func)(), v[9];
   func = fload(heap[0]);
   setargs(heap,&heap[2],v,heap[1]);

   switch (heap[1])
   { case 0: (*func)(); break;
     case 1: (*func)(v[0]); break;
     case 2: (*func)(v[0],v[1]); break;
     case 3: (*func)(v[0],v[1],v[2]); break;
     case 4: (*func)(v[0],v[1],v[2],v[3]); break;
     case 5: (*func)(v[0],v[1],v[2],v[3],v[4]); break;
     case 6: (*func)(v[0],v[1],v[2],v[3],v[4],v[5]); break;
     case 7: (*func)(v[0],v[1],v[2],v[3],v[4],v[5],v[6]); break;
     case 8: (*func)(v[0],v[1],v[2],v[3],v[4],v[5],v[6],v[7]); break;
     case 9: (*func)(v[0],v[1],v[2],v[3],v[4],v[5],v[6],v[7],v[8]); break;
     default: break;}
   heap[0] = 0;}


setargs(absbase,reloffsets,v,n) int *absbase, *reloffsets, **v,n;
 { int j;
   for (j = 0;
        j<n;
        j++)
    {v[j] = reloffsets[j] + absbase;}}


Filename: fload.c

/**************************************
 * Copyright LISP Machine, Inc. 1985  *
 *  See filename "Copyright" for      *
 * licensing and release information. *
 **************************************/

/* The fload module could provide either dynamic or static linking.
   Let us assume for now that it is static linking */


#include "fort.h"

int noop()
{return(0);}



int (*(fload(opcode)))() int opcode;
{ return(ftable[opcode]);}



Filename: ftable.c

/*********************************************************************
 * Automatically generated C code for the FORTRAN link               *
 * Compiled from EXAMPLE by "gjc"                                    *
 * Saturday the twenty-fifth of May, 1985; 8:05:22 pm                *
 *********************************************************************/

#include "fort.h"


int vprint();
int vmult();
int vadd();

int (*ftable[])() = {noop,
 vprint,
 vmult,
 vadd};


Filename: flib.f

C**************************************
C* Copyright LISP Machine, Inc. 1985  *
C*  See filename "Copyright" for      *
C* licensing and release information. *
C**************************************
       SUBROUTINE VADD(X,Y,Z,N)
       REAL X(N),Y(N),Z(N)
       INTEGER N
       DO 100 J=1,N
100    Z(J) = X(J) + Y(J)
       RETURN
       END
       SUBROUTINE VMULT(X,Y,Z,N)
       REAL X(N),Y(N),Z(N)
       INTEGER N
       DO 100 J=1,N
100    Z(J) = X(J) * Y(J)
       RETURN
       END
       SUBROUTINE VPRINT(X,N)
       REAL X(N)
       INTEGER N
       DO 100 J=1,N
100    WRITE(6,200) J,X(J)
200    FORMAT(' X(',I3,')=',E13.3)
       RETURN
       END


Filename: sdump.c

/**************************************
 * Copyright LISP Machine, Inc. 1985  *
 *  See filename "Copyright" for      *
 * licensing and release information. *
 **************************************/

/* Interprocessor Lisp->Fortran call mechanism
   prototype 5/26/85 15:23:17 -George Carrette */

#include <stdio.h>

#include <share.h>

int aint(a,j) int j,a[];
{ return(a[j]);}

float afloat(a,j) int j; float a[];
{ return(a[j]);}

main(argc,argv) int argc; char **argv;
{ int j,start,end;
  if (share_setup() < 0) {printf("share_setup failed");exit(1);}
  if ( argc > 1) sscanf(argv[1],"%d",&start); else start = 0;
  if ( argc > 2) sscanf(argv[2],"%d",&end); else end = start + 10;
  printf("Shared memory Dump of words %d to %d\n",start,end);
  printf(" Word     as int         as float\n");
  for (j=start; j<end; j++)
   printf("%3d = %12d    %12e\n",j,aint(sharebase,j),afloat(sharebase,j));
}


Filename: putint.c

/**************************************
 * Copyright LISP Machine, Inc. 1985  *
 *  See filename "Copyright" for      *
 * licensing and release information. *
 **************************************/

/* Interprocessor Lisp->Fortran call mechanism
   prototype 5/26/85 15:23:17 -George Carrette */

#include <stdio.h>

#include <share.h>

pfloat(a,j,v) int j; float a[],v;
{a[j]=v;}

main(argc,argv) int argc; char **argv;
{ int loc; float val;
  if (share_setup() < 0) {printf("share_setup failed");exit(1);}
  if (argc != 3) {printf("%s <location> <integer-value>\n",argv[0]);exit(1);}
  sscanf(argv[1],"%d",&loc);
  sscanf(argv[2],"%e",&val);
  pfloat(sharebase,loc,val);
}

Filename: putfloat.c

/**************************************
 * Copyright LISP Machine, Inc. 1985  *
 *  See filename "Copyright" for      *
 * licensing and release information. *
 **************************************/

/* Interprocessor Lisp->Fortran call mechanism
   prototype 5/26/85 15:23:17 -George Carrette */

#include <stdio.h>

#include <share.h>

pfloat(a,j,v) int j; float a[],v;
{a[j]=v;}

main(argc,argv) int argc; char **argv;
{ int loc; float val;
  if (share_setup() < 0) {printf("share_setup failed");exit(1);}
  if (argc != 3) {printf("%s <location> <integer-value>\n",argv[0]);exit(1);}
  sscanf(argv[1],"%d",&loc);
  sscanf(argv[2],"%e",&val);
  pfloat(sharebase,loc,val);
}


Filename: makefile

OFILES= fmain.o fload.o fexec.o flib.o ftable.o

.SUFFIXES: .o .c .f

.f.o:
        f77 -c $*.f

all: sdump putfloat putint mainprog

sdump:
        cc -o sdump sdump.c -lshare

putfloat:
        cc -o putfloat putfloat.c -lshare

putint:
        cc -o putint putint.c -lshare

mainprog: $(OFILES)
        cc -o mainprog $(OFILES) -lshare -lF77 -lI77

||#
