;;; -*- Mode:LISP; Package:USER;BASE: 10 -*-

;;; Copyright LISP Machine, Inc. 1984
;;;  See filename "Copyright.Text" for
;;; licensing and release information.

;;; This code implements ARRAY printing <TERMINAL-Q> by writing an array-dump file
;;; to a Unix host, and then using the EVAL server to call a command file that
;;; converts that file to Imagen's IMPRESS format and then calls the IPR command
;;; to queue the file to the laserprinter.
;;;
;;; The Unix command to convert an array dump file X.ARRAY to impress X.IMPRESS
;;; is the following:
;;; /lmi/gjc/bin/bitmap -300 -newdraw X.ARRAY > X.IMPRESS
;;;
;;; The Unix command to queue (and delete) the file X.IMPRESS is:
;;; /usr/local/bin/ipr -Limpress -P im1 -f X.IMPRESS -r -d X.IMPRESS

;;; Some unix commands must be set up to support the EVAL server call:
;;; /usr/local/bin/atigerimp <array-file-input-from-lispm> <output-impress-language-file>
;;; /usr/local/bin/impress <impress-language-filename>

;;; It is expected that most sites will need to modify these procedures slightly
;;; to get maximum benefit.


(defun setup-imagen-unix-spool (host &optional &key
                                (directory "//tmp")
                                (spool-to-printer t)
                                (filename nil)
                                (file-count 0)
                                (generate-spool-file-name 'generate-imagen-spool-file-name))
  (setq si:*default-bit-array-printer* (list :imagen-unix-spool
                                             :host (si:parse-host host)
                                             :directory directory
                                             :spool-to-printer spool-to-printer
                                             :filename filename
                                             :file-count file-count
                                             :generate-spool-file-name generate-spool-file-name)))

(defun (:imagen-unix-spool si:print-bit-array) (printer array left top right bottom &rest options)
  options
  (let ((pathname (funcall (get printer :generate-spool-file-name) printer)))
    (with-open-file (stream pathname :direction :output :characters nil
                            :byte-size 16.)
      (let ((translator (make-8b-16b-output-translator stream)))
        (tiger:dump-pixel-array-to-stream translator array left top right bottom)
        (send translator :tyo 0)))
    (when (get printer :spool-to-printer)
      (process-run-function "handle imagen spooling"
                            #'handle-imagen-unix-array-spooling
                            pathname
                            'si:null-stream))))


(defun generate-imagen-spool-file-name (printer)
  (incf (get printer :file-count))
  (fs:parse-pathname (format nil "~A:~A//~A-~A.array"
                             (send (get printer :host) :name)
                             (get printer :directory)
                             (or (get printer :filename) (string-downcase si:user-id))
                             (get printer :file-count))))


(defun handle-imagen-unix-array-spooling (pathname &optional (stream standard-output))
  (let ((a-f (send pathname :string-for-host))
        (i-f (send (send pathname :new-type "IMPRESS") :string-for-host))
        (h (send (send pathname :host) :name)))
    (format stream "~&Processing on host ~S ~S into ~S~%" h a-f i-f)
    (simple-unix-eval h (format nil "//usr//local//bin//pdrawfile ~A ~A"
                                a-f i-f)
                      stream)))


(defun simple-unix-eval (host command stream)
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

(defun make-8b-16b-output-translator (16b-stream &aux 8b-stream first)
  (setq 8b-stream #'(lambda (op &optional arg1 &rest args)
                      (si:selectq-with-which-operations op
                        (:tyo
                          (cond ((not first)
                                 (setq first arg1))
                                ('else
                                 ;; this assumes transmission to a VAX
                                 ;; a 68000 would want (+ (ash first 8) arg1)
                                 (send 16b-stream :tyo (+ first (ash arg1 8)))
                                 (setq first nil))))
                        (:string-out
                          ;; this might look slow but it is ok.
                          (do ((j (or (car args) 0) (1+ j))
                               (c first)
                               (string arg1)
                               (end (or (cadr args) (length arg1))))
                              ((= j end)
                               (if c (setq first c)))
                            (cond ((not c)
                                   (setq c (aref string j)))
                                  ('else
                                   (send 16b-stream :tyo (+ c (ash (aref string j) 8)))
                                   (setq c nil)))))
                        (t
                          (stream-default-handler 8b-stream op arg1 args))))))



;;; The code BITMAP.C follows:
#||


/*
;;; Copyright LISP Machine, Inc. 1984
;;;  See filename "Copyright.Text" for
;;; licensing and release information.

Converts an array dump file to an IMPRESS language file.

*/

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>

#define tstream stdout

int flag;
int width, height, left, top, right, bottom;
unsigned char *bitarray;
FILE *fp;
int mode = {'p'};
int DPI = {300};        /* laser printer dots per inch */
int mag = {1};                /* magnification = 4 for 480 DPI */
int setup_flag = {0};        /* setup data not sent yet */
int label_flag = {0};

void setup_graphics() {
        int x,y;

        /* fputs("@document(language imPRESS)",tstream); */

        if(mode == 'p') {                /* portrait */
                if(DPI == 480) {
                        x = DPI;
                        y = DPI;
                }
                else {
                        x = DPI * 2;
                        y = DPI * 2;
                }

                fputc(135,tstream);                /* h offset */
                fputc(x/256,tstream);
                fputc(x%256,tstream);

                fputc(137,tstream);                /* v offset */
                fputc(y/256,tstream);
                fputc(y%256,tstream);
        }
        else {                                /* landscape / newdraw */
                if(DPI == 480) {
                        x = DPI;
                        y = DPI * 10;
                }
                else {
                        x = DPI * 2;
                        y = DPI * 9;
                }

                fputc(135,tstream);                /* h offset */
                fputc(x/256,tstream);
                fputc(x%256,tstream);

                fputc(137,tstream);                /* v offset */
                fputc(y/256,tstream);
                fputc(y%256,tstream);

                fputc(205,tstream);        /* rotate hv system 270 degrees */
                fputc(007,tstream);
        }

        fputc(236,tstream);                /* mag = 2x or 4x */
        fputc(mag,tstream);

        fputc(214,tstream);                /* set push mask */
        fputc(1,tstream);
        fputc(255,tstream);
}

int read_word()
{        register unsigned char c1, c2, c3, c4;
        c1 = getc(fp);
        c2 = getc(fp);
        c3 = getc(fp);
        c4 = getc(fp);
        return(c1 | (c2 << 8) | (c3 << 16) | (c4 << 24));
}

char flip_byte(c)
char c;
{
        register int i;
        register char cc;
        cc = 0;
        for (i = 1; i < 256; i <<= 1) {
                if(c & i)
                        cc += cc + 1;
                else
                        cc += cc;
        }
        return(cc);
}

int chekfile() {
        char c1,c2,c3,c4;
        c1 = getc(fp);
        c2 = getc(fp);
        c3 = getc(fp);
        c4 = getc(fp);
        if((c1 == 'A') && (c2 == 'R') && (c3 == 'A') && (c4 == 'Y'))
                return(1);
        else
                return(0);
}

void print_1(filename)
char *filename;
{        int version, file_bytes, total_bytes;
        int row, col, row_incr ,y, x;
        register unsigned char *ind, *bitend, *realend, *idx;
        int ch1, ch2, ch3, ch4;
        register int ch, pos, byte_width, delta_x;

        fp = fopen(filename, "r");
        if(fp == NULL) {
                fprintf(stderr,"Can't open file %s\n",filename);
                return;
        }

        if(!chekfile()) {
                for(x = 0; x < 05774; x++) getc(fp);
                if(!chekfile()) {
                        for(x = 0; x < 01774; x++) getc(fp);
                        if(!chekfile()) {
                                fprintf(stderr, "%s is not an array file. %c\n",
                                       filename,getc(fp));
                                return;
                        }
                }
        }

        /* The version number of the file must be 1 */
        version = read_word(fp);
        if (version != 1) {
                fprintf(stderr, "The version number of %s is %d, not 1.\n",
                                 filename, version);
                return;
        }

        /* Read the other numbers from the file */
        width = read_word();
        height = read_word();
        left = read_word();
        top = read_word();
        right = read_word();
        bottom = read_word();

        /* Adjust bottom so the number of rows is a multiple of 32 */
        byte_width = width / 8;
        file_bytes = byte_width * height;
        bottom = height = (bottom + 31) / 32 * 32;
        total_bytes = byte_width * height;
        row_incr = byte_width * 32;

        /* Put the contents of the file into the bitarray */
        bitarray = (unsigned char *) malloc(total_bytes);
        bitend = &bitarray[file_bytes];
        realend = &bitarray[total_bytes];
        for (ind = bitarray; ind != bitend; ind++) {
                *ind = getc(fp);
        }

        /* Add zeros at the end to fill any created rows */
        for (ind = bitend; ind != realend; ind++) {
                *ind = 0;
        }

        /* Now it is time to start printing */
        ind = bitarray;

        fputc(211,tstream);                /* push graphics state */

        fputc(235,tstream);                /* bitmap command */
        fputc(3,tstream);
        fputc((byte_width + 3)/4, tstream);
        fputc(height/32, tstream);

        for (row = 0; row < bottom; row += 32) {
                for(col = 0; col < byte_width; col += 4) {
                        for(y = row; y < row+32; y++) {
                                ind = &bitarray[y*byte_width + col];
                                for(x = col; x < col+4; x++) {
                                        if(x < byte_width)
                                            fputc(flip_byte(*ind++),tstream);
                                        else
                                                fputc(0,tstream);
                                }
                        }
                }
        }

        if(label_flag) {
                x = y = 0;
                fputc(135,tstream);                /* h offset */
                fputc(x/256,tstream);
                fputc(x%256,tstream);

                fputc(137,tstream);                /* v offset */
                fputc(y/256,tstream);
                fputc(y%256,tstream);
                fprintf(tstream,"%s",filename);
        }
        fputc(219,tstream);                /* endpage */
        fputc(212,tstream);                /* pop state */
        free(bitarray);
}

main(argc, argv)
int argc;
char *argv[];
{        int n, of;

        /* Print the files */
        for (n = 1; n < argc; n++) {
                /* ignore anything that looks like a switch */
                if (argv[n][0] != '-') {
                        if(!setup_flag) {
                                setup_graphics();
                                setup_flag = 1;
                        }
                        print_1 (argv[n]);
                }
                else {
                        flag = argv[n][1];
                        switch (flag) {
                                case '3':
                                         DPI = 300;
                                         mag = 1;        /* 2X */
                                         break;
                                case '4':
                                         DPI = 480;
                                         mag = 2;        /* 4X */
                                         break;
                                case 'n': case 'N': mode = 'n'; break;
                                case 'l': case 'L': mode = 'l'; break;
                                case 'p': case 'P': mode = 'p'; break;
                                case 'x': case 'X': label_flag = 1; break;
                        }
                }
        }

        /* All done -- clean up */
        fclose(tstream);
}


||#
