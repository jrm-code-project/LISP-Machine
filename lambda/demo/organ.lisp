;; -*- Mode:LISP; Package:HACKS; Base:8; Readtable:ZL -*-

;; Originally written by ACW, modified by CWH
;; Modified again by DLA.

(defvar *scale*)
(defvar *key* 0)
(defvar *initial-speed* 400000)                 ;Normal speed
(defvar *speed* *initial-speed*)                ;Current speed
(defvar *organ-initial-speed*)                  ;Speed when ORGAN was last called

(defvar *organ-input-buffer* (make-string #o300 :fill-pointer 0))

(defvar *organ-speed-stack* (make-array #o300 :fill-pointer 0))

(defvar *organ-initial-right*)                  ;Variables for rubout handling
(defvar *organ-initial-down*)

(defun piano (n)
  (fix (* .5878590257 (piano-1 (- n *key*)))))  ;A = 440 4 Jan 1981

(defun piano-1 (n)
  (if (zerop n)
      1.0
      (let ((s (piano-1 (lsh n -1))))
        (* s s (if (oddp n)
                   1.059463095
                   1.0)))))

(defvar *scale*
  (let ((arr (make-array #o177 :type 'art-16b)))
    (aset (piano 230) arr #/z)
    (aset (piano 227) arr #/Z)
    (aset (piano 226) arr #/x)
    (aset (piano 225) arr #/X)
    (aset (piano 224) arr #/c)
    (aset (piano 223) arr #/C)
    (aset (piano 223) arr #/v)
    (aset (piano 222) arr #/V)
    (aset (piano 221) arr #/b)
    (aset (piano 220) arr #/B)
    (aset (piano 217) arr #/n)
    (aset (piano 216) arr #/N)
    (aset (piano 215) arr #/m)
    (aset (piano 214) arr #/M)
    (aset (piano 214) arr #/a)
    (aset (piano 213) arr #/A)
    (aset (piano 212) arr #/s)
    (aset (piano 211) arr #/S)
    (aset (piano 210) arr #/d)
    (aset (piano 207) arr #/D)
    (aset (piano 207) arr #/f)
    (aset (piano 206) arr #/F)
    (aset (piano 205) arr #/g)
    (aset (piano 204) arr #/G)
    (aset (piano 203) arr #/h)
    (aset (piano 202) arr #/H)
    (aset (piano 201) arr #/j)
    (aset (piano 200) arr #/J)
    (aset (piano 200) arr #/q)
    (aset (piano 177) arr #/Q)
    (aset (piano 176) arr #/w)
    (aset (piano 175) arr #/W)
    (aset (piano 174) arr #/e)
    (aset (piano 173) arr #/E)
    (aset (piano 173) arr #/r)
    (aset (piano 172) arr #/R)
    (aset (piano 171) arr #/t)
    (aset (piano 170) arr #/T)
    (aset (piano 167) arr #/y)
    (aset (piano 166) arr #/Y)
    (aset (piano 165) arr #/u)
    (aset (piano 164) arr #/U)
    (aset (piano 164) arr #/k)
    (aset (piano 163) arr #/K)
    (aset (piano 162) arr #/l)
    (aset (piano 161) arr #/L)
    (aset (piano 160) arr #/i)
    (aset (piano 157) arr #/I)
    (aset (piano 157) arr #/o)
    (aset (piano 156) arr #/O)
    (aset (piano 155) arr #/p)
    (aset (piano 154) arr #/P) arr))

;; We need the entire processor here, so turn off :CLOCK and :CHAOS interrupts.
;; Change when new version of PROCES installed.


(defun play-string (str)
  (with-real-time
    (prog (where char ii (repeat (= (aref str 0) #/:)))
       R (setq where -1)
       L (setq where (1+ where))
          (if (= where (string-length str))
              (if (and repeat
                       (not (send *terminal-io* :tyi-no-hang)))
                  (go R)
                  (return nil)))
          (setq char (aref str where))
          (and (> char 177) (go L))
          (selectq char
            (#/')
            (#//)
            (#/ )
            (#/:)
            (#/CR)
            (#/@ (setq *speed* *initial-speed*))
            (#/< (setq *speed* (truncate *speed* 3)))
            (#/> (setq *speed* (* *speed* 3)))
            (#/[ (setq *speed* (lsh *speed* -1)))
            (#/] (setq *speed* (lsh *speed* 1)))
            (#/- (si:%beep -1 *speed*))         ;rest
            (t (go ON)))
          (go L)
       ON (setq ii (do ((i where (1+ i)))
                       ((or (= i (string-length str))
                            (not (= (aref str i) char))) i)))
          (si:%beep (aref *scale* char) (* *speed* (- ii where)))
          (setq where (1- ii))
          (go L))))

(defun play (thing)
  (cond ((stringp thing) (play-string thing))
        ((symbolp thing) (play (symeval thing)))
        ((consp thing) (mapc #'play thing))
        ((integerp thing) (si:%beep (aref *scale* thing) *speed*))))

(defun organ (&aux (buffer *organ-input-buffer*)
                   (speed-stack *organ-speed-stack*)
                   (stream standard-input)
                   (temp-array (make-array 1 ':type 'art-string)))
  (organ-note-initial-cursorpos stream)
  (setf (fill-pointer buffer) 0)                ;Flush buffer contents
  (setf (fill-pointer speed-stack) 0)
  (do ((char (send stream :tyi) (send stream :tyi)))
      ((= char #/.)
       (string-append buffer))
    (selectq char
      (#/rubout (organ-do-rubout buffer speed-stack stream))
      ((#/return #/tab)
       (send stream :tyo char)
       (vector-push-extend char buffer))
      ((#/form 554 514)
       (send stream :clear-window)
       (organ-note-initial-cursorpos stream)
       (princ buffer))
      ((#/? #/help)
       (send stream :clear-window)
       (princ "
Welcome to the ORGAN.  The keyboard is now an organ.  Most of the keys play
notes, but the following have special meanings.  The most notable of these are
the following:

:       If this is the first character in the string, the string will
        repeat when played.
@       Resets speed to the initial speed.
<       Speeds you up by a factor of 3.
>       Slows you down by a factor of 3.
[       Speeds you up by a factor of 2.
]       Slows you down by a factor of 2.
-       Plays a rest.
RUBOUT  Allows you to erase your mistakes.
.       Stops.  ORGAN returns a string which is your tune.  This tune can
        be played with the PLAY function.

")
           (organ-note-initial-cursorpos stream)
           (princ buffer))
      (otherwise
       (cond ((< char #o200)
              (send stream :tyo char)
              (and (memq char '(#/< #/> #/[ #/] #/@))
                   (vector-push-extend *speed* speed-stack))
              (aset char temp-array 0)
              (OR (CHAR-EQUAL CHAR #/:)         ;This would play forever...
                  (play-string temp-array))
              (vector-push-extend char buffer))
             (t (tv:beep)))))))


(defun organ-do-rubout (buffer speed-stack stream &aux r d char)
  (cond ((plusp (array-leader buffer 0))
         (setq char (vector-pop buffer))
         (multiple-value (r d)
           (send stream :read-cursorpos))       ;in PIXEL!!
         (cond ((or (zerop r)
                    (= char #/tab))
                (send stream :set-cursorpos *organ-initial-right* *organ-initial-down*)
                (send stream :string-out buffer))
               (t (send stream :set-cursorpos
                               (- r (send stream :character-width char))
                               d)
                  (send stream :clear-rest-of-line)))
         (and (memq char '(#/< #/> #/[ #/] #/@))
              (setq *speed* (vector-pop speed-stack))))
        (t (tv:beep))))                         ;Is this the right thing??

(defun organ-note-initial-speed ()
  (setq *organ-initial-speed* *speed*))

(defun organ-figure-out-speed (buffer temp-array)
  (setq *speed* *organ-initial-speed*)
  (dotimes (x (string-length buffer))
    (and (mem #'= (aref buffer x) '(#/< #/> #/[ #/] #/@))
         (progn (aset (aref buffer x) temp-array 0)
                (play-string temp-array)))))

(defun organ-note-initial-cursorpos (stream)
  (multiple-value (*organ-initial-right* *organ-initial-down*)
    (send stream :read-cursorpos)))


(DEFUN INS (SEXP)
    (PRIN1 SEXP ZWEI:(INTERVAL-STREAM (POINT) (POINT) T)))

(zwei:defcom com-play-region "Plays the region with the ORGAN program." ()
   (zwei:region (a b)
      (play (zwei:string-interval a b)))
   zwei:dis-none)

;Thank you, Khyai Udin Mas
(defvar pelog-scale
  (let ((arr (make-array 177 ':type 'art-16b)))
    (aset 2340 arr #/a)
    (aset 2340 arr #/A)
    (aset 2200 arr #/s)
    (aset 2200 arr #/S)
    (aset 2030 arr #/d)
    (aset 2030 arr #/D)
    (aset 1540 arr #/f)
    (aset 1540 arr #/F)
    (aset 1444 arr #/g)
    (aset 1444 arr #/G)
    (aset 1377 arr #/h)
    (aset 1377 arr #/H)
    (aset 1273 arr #/j)
    (aset 1273 arr #/J)
    arr))

(defun gamelan ()
  (let ((*scale* pelog-scale))
    (organ)))

(defun play-gamelan (thing)
  (let ((*scale* pelog-scale))
    (play thing)))
