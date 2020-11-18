;              -*- Mode:Lisp;  Package:Hacks;  Base: 10.    -*-
;****************************************************************************************
;  Beeps . module  6/28/82

(defunp BEEPS ()
   "Module that contains a lot of routines to make strange noises on the Lisp machines.
    Run this program for more documentation"

  (format t "  Beeps.module

      This module contains several routines that create sequences of sound on the
  Lisp machines.  ( The newer ones only, from what I've been told.)
  All these routines use the sys:%beep function to make things work.

  The routines in this module, all in package HACKS, are:
     BOOP       -  makes a sound with a randomish rising frequency.
     POOB       -  makes a similar sound to boop, but the frequency drops.
     OOPB       -  boop and poob at the same time with varying duty cycles.
     BROOP      -  alternating tones one up, one down.
     SPOOB      -  one tone blending into another by varying duty cycles.
     SPOOP      -  rapidly alternating tone and nothing. Very sputtery.
     BREEP      -  makes a wired sound with five alternaing frequencies.
     DREEP      -  like breep, but the sound goes up in frequency.
     BEEP-RISE  -  like breep, but the sound goes up in frequency.
     FREEP      -  fractal noise, sounds a bit a cat walking on an organ keyboard.
     GREEP      -  like freep, but uses a gaussian noise function so it sounds more musical.
     NREEP      -  four sets of greep going off at the same time. Sounds like computer noise.
     SRIEEP     -  siren-like sound.  Sinusiodal variation of frequencies between two freqs.
     FSRIEEP    -  like srieep, but the endpoint frequencies are changing exponentially
     PLAY-SONG  -  plays a song - Scale and speed are sort of random.
                   Current ones written include:
                      'SWARS  - theme from star wars
                      'ET     - theme from movie E.T.
                      'ZOWIE  - plays the zowie tune
                      'TZONE  - theme from twilight zone.
    RND-BEEP   - plays a random beep from the above selection

    BEEP-INSTALL - redefines system function BEEP
                   to play a random beep from the above selection.

  I got the initial set of routines from AGRE, who says he got them from someone else.

  Most of the routines have optional arguments, many of which are randomly determined.
  I picked reasonable values or ranges for the default values, but they are by no means
  perfect.  Experiment to see which values you like.

  For your added enlightenment, the SYS:%BEEP function which is used to produce the basic
  noises made by my routine is of the form (SYS:%BEEP period length), where:

      period  -  number of clockcycles in half period of tone.  100 produces a really high
                 pitch tone, while 5000 produces about the lowest tone possible.  500 is
                 the default beep tone on most of the lisp machines (I think).
      length  -  Total number of clockcycles that tone will last for.  A value of 100000
                 last a little less than (really roughly) a 10th of a second.

  J.Bradstreet  10-26-82
----------------------------------------------------------------------------------------"))

(defun rnd-beep ()
  (selectq (RANDOM 16)
    ((0 1 3) (poob))
    ((4 5) (boop))
    ((6 7 8 9 10) (oopb))
    (11 (spoop))
    ((12 13 14) (selectq (RANDOM 9)
                  ((0 1 2) (play-song 'zowie))
                  ((3 4 5) (play-song 'tzone))
                  (6 (play-song 'swars))
                  (7 (play-song 'bat5))
                  (8 (play-song 'races))))
    (15 (selectq (RANDOM 11)
          (0 (breep))
          (1 (dreep))
          (2 (play-song 'cscale-both))
          (3 (beep-rise))
          (4 (nreep 25 5 5))
          (5 (nreep 25 100 5))
          (6 (srieep))
          (7 (play-song 'cscale-up))
          (8 (fsrieep))
          (9 (play-song 'cscale-up))
          (10 (play-song 'cscale-down))
          ))))

(defun beep-install ()
  "Redefine system function BEEP to play a randomly-chosen funny beep."
  (login-eval
    (prog1 `(fset 'tv:beep ',(function tv:beep))
           (fset 'tv:beep 'rnd-beep))))


(defvar boop-increment 100)
(defvar boop-min 100)
(defvar boop-max 2500)
(defun boop (&optional
             (incr boop-increment)
             (max-val (FIXR (* boop-max (+ 1 (* .75 (RANDOM-U2))))))
             (min-val (FIXR (* boop-min (+ 1 (* .75 (RANDOM-U2)))))))
  (without-interrupts
    (do ((a1 min-val (+ a1 (random 5) incr))) ((> a1 max-val)) (sys:%beep a1 10000))))

(defun poob (&optional
             (incr boop-increment)
             (max-val (FIXR (* boop-max (+ 1 (* .75 (RANDOM-U2))))))
             (min-val (FIXR (* boop-min (+ 1 (* .75 (RANDOM-U2)))))))
  (without-interrupts
    (do ((a1 max-val (- a1 (+ (random 5) incr)))) ((< a1 min-val)) (sys:%beep a1 10000))))

(defun oopb (&optional
             (incr boop-increment)
             (max-val (FIXR (* boop-max (+ 1 (* .75 (RANDOM-U2))))))
             (min-val (FIXR (* boop-min (+ 1 (* .75 (RANDOM-U2))))))
             (dir-1 (RANDOM 20000))
             (dir-2 (RANDOM 20000)))

  (without-interrupts
    (do ((a1 max-val (- a1 (+ (random 5) incr))))
        ((< a1 min-val))
      (sys:%beep a1 dir-1)
      (sys:%beep (+ min-val (- max-val a1)) dir-2)  )))


(defun broop (&optional
             (incr boop-increment)
             (max-val boop-max)
             (min-val boop-min)
             (dir-1 (RANDOM 20000))
             (dir-2 (RANDOM 20000))
             (tone-1 (RANDOM 2))
             (tone-2 (IF (zerop tone-1) 1 0)))
  (without-interrupts
    (do ((a1 max-val (- a1 (+ (random 5) incr))))
        ((< a1 min-val))
      (sys:%beep (* a1 tone-1) dir-1)
      (sys:%beep (* (+ min-val (- max-val a1)) tone-2) dir-2)  )))

(defun spoop (&optional
             (incr boop-increment)
             (val (+ boop-min (RANDOM (- boop-max boop-min))))
             (max-val boop-max)
             (min-val boop-min)
             (dir-1 10000)
             (dir-2 20000))
  (without-interrupts
    (do ((a1 max-val (- a1 (+ (random 5) incr))))
        ((< a1 min-val))
      (sys:%beep val dir-1)
      (sys:%beep dir-2 dir-2)  )))

(defun spoob (&optional
             (val-1 (+ boop-min (RANDOM (- boop-max boop-min))))
             (val-2 (+ boop-min (RANDOM (- boop-max boop-min))))
             (dir-max 10000))
  (without-interrupts
    (sys:%beep val-1 100000)
    (do ((dir 0 (+ dir (random 800))))
        ((> dir dir-max))
      (sys:%beep val-2 dir)
      (sys:%beep val-1 (- dir-max dir))  )
    (sys:%beep val-2 20000)))


(defun breep (&optional (incr 50) (tone-start 500))
  (without-interrupts
    (do ((i 0 (1+ i))
         (a1 tone-start (if (> i 10) (+ a1 incr) a1))
         (a2 tone-start (if (> i 20) (+ a2 incr) a2))
         (a3 tone-start (if (> i 30) (+ a3 incr) a3))
         (a4 tone-start (if (> i 40) (+ a4 incr) a4))
         (a5 tone-start (if (> i 50) (+ a5 incr) a5)))
        ((OR (> i 250) (> a5 2500)))
      (sys:%beep a1 10000)
      (sys:%beep a2 10000)
      (sys:%beep a3 10000)
      (sys:%beep a4 10000)
      (sys:%beep a5 10000))))

(defun dreep (&optional (incr 75) (tone-start 2500) (seperation 10))
  (without-interrupts
    (do ((i 0 (1+ i))
         (a1 tone-start (if (> i (* 0 seperation)) (- a1 incr) a1))
         (a2 tone-start (if (> i (* 1 seperation)) (- a2 incr) a2))
         (a3 tone-start (if (> i (* 2 seperation)) (- a3 incr) a3))
         (a4 tone-start (if (> i (* 3 seperation)) (- a4 incr) a4))
         (a5 tone-start (if (> i (* 4 seperation)) (- a5 incr) a5)))
        ((OR (> i 250) (< a5 100)))
      (sys:%beep a5 10000)
      (sys:%beep a4 10000)
      (sys:%beep a3 10000)
      (sys:%beep a2 10000)
      (sys:%beep a1 10000))))

(defun beep-rise ()
  (without-interrupts
    (do ((i 0 (1+ i))
         (a1 500 (if (= (\ i 40) 0) 500 (+ a1 10)))
         (a2 500 (if (= (\ i 40) 10) 500 (+ a2 10)))
         (a3 500 (if (= (\ i 40) 20) 500 (+ a3 10)))
         (a4 500 (if (= (\ i 40) 30) 500 (+ a4 10))))
        ((> i 75))
      (sys:%beep a1 10000)
      (sys:%beep a2 10000)
      (sys:%beep a3 10000)
      (sys:%beep a4 10000))))

(defun freep (&optional (sstep 25) (nstep 5) (stime 50) (fmin 400) (fmax 2500)  )
  (without-interrupts
    (do ((i 0 (1+ i))
         (fi (+ fmin (RANDOM (- fmax fmin)))
             (MAX fmin (MIN fmax (+ fi (* sstep (- nstep (RANDOM (+ 1 nstep nstep)))))))))
        ((> i (truncate 1000 stime)))
      (IF (= fi fmin) (setq fi (+ fmin (* sstep (RANDOM 5)))))
      (sys:%beep fi (* (1+ (RANDOM stime)) 10000) ))))

(defun greep (&optional (sstep 25) (nstep 5) (stime 25) (fmin 400) (fmax 2500)  )
  (without-interrupts
   (do ((i 0 (1+ i))
        (fi (+ fmin (RANDOM (- fmax fmin)))
            (MAX fmin (MIN fmax (+ fi (* sstep (FIXR (* nstep (RANDOM-U2)))))))))
       ((> i (truncate 1000 stime)))
     (IF (= fi fmin) (setq fi (+ fmin (* sstep (RANDOM 5)))))
     (sys:%beep fi (* (1+ (RANDOM stime)) 10000) ))))

(defun nreep (&optional (sstep 10) (nstep 25) (stime 10) (fmin 400) (fmax 2500)  )
  (without-interrupts
   (do ((i 0 (1+ i))
        (fi1 (+ fmin (RANDOM (- fmax fmin)))
             (MAX fmin (MIN fmax (+ fi1 (* sstep (FIXR (* nstep (RANDOM-U2))))))))
        (fi2 (+ fmin (RANDOM (- fmax fmin)))
             (MAX fmin (MIN fmax (+ fi2 (* sstep (FIXR (* nstep (RANDOM-U2))))))))
        (fi3 (+ fmin (RANDOM (- fmax fmin)))
             (MAX fmin (MIN fmax (+ fi3 (* sstep (FIXR (* nstep (RANDOM-U2))))))))
        (fi4 (+ fmin (RANDOM (- fmax fmin)))
             (MAX fmin (MIN fmax (+ fi4 (* sstep (FIXR (* nstep (RANDOM-U2)))))))))
       ((> i (truncate 75 stime)))
     (sys:%beep fi1 (* (1+ (RANDOM stime)) 10000) )
     (sys:%beep fi2 (* (1+ (RANDOM stime)) 10000) )
     (sys:%beep fi3 (* (1+ (RANDOM stime)) 10000) )
     (sys:%beep fi4 (* (1+ (RANDOM stime)) 10000) ))))


(defun srieep (&optional
               (top-f (+ 100 (RANDOM 500)))
               (bot-f (+ top-f 100 (RANDOM 4000)))
               (num-cycles (+ 5 (RANDOM 15)))
               (period (+ 50000 (RANDOM 200000)))
               (beep-time (+ 2000 (RANDOM 8000)))
               &aux ff w-increment)
  (setq w-increment (// (* 2.0 3.14159265 beep-time) period))
  (without-interrupts
    (do ((c 0 (1+ c)))
        ((= c num-cycles))
      (do ((k 0 (+ k beep-time))
           (w 0 (+ w w-increment)))
          (( k period))
        (setq ff (+ top-f (FIXR (* .5 (- bot-f top-f) (+ 1 (COS w))))))
        (sys:%beep ff (*  (truncate beep-time ff) ff))))))

(defun fsrieep (&optional
               (top-f (+ 100 (RANDOM 2000)))
               (bot-f (+ top-f 100 (RANDOM 2000)))
               (decay-factor (+ .8 (* .4 (// (RANDOM 100) 100.0))))
               (num-cycles (+ 5 (RANDOM 15)))
               (period (+ 100000 (RANDOM 600000)))
               (beep-time (+ 2000 (RANDOM 8000)))
               &aux ff w-increment)

  (setq w-increment (// (* 2.0 3.14159265 beep-time)  period))

  (without-interrupts
    (do ((c 1 (1+ c))
         (tf top-f (* tf decay-factor))
         (bf bot-f (* bf decay-factor)))
        (( c num-cycles))
      (do ((k 0 (+ k beep-time))
           (w 0 (+ w w-increment)))
          (( k period))
        (setq ff (FIXR (+ tf (* .5 (- bf tf) (+ 1 (COS w))))))
        (sys:%beep ff (*  (truncate beep-time ff) ff))))))


(defvar beeps*notes
;              c    c#   d    eb   e    f    f#   g    ab   a    bb   b
;              0    1    2    3    4    5    6    7    8    9    10   11
        (list 200. 189. 178. 168. 159. 150. 141. 133. 126. 119. 112. 106.
              100. 94.  89.  84.  79.  75.  71.  66.  63.  59.  56.  53.))
;              12  13   14   15   16   17   18   19   20   21   22   23

(defvar beeps*cscale-up
        '((0 1 2 3 4 5 6 7 8 9 10 11 12)
          (1 1 1 1 1 1 1 1 1 1 1 1 1 2)))

(defvar beeps*cscale-down
        '((12 11 10 9 8 7 6 5 4 3 2 1 0)
          (1   1  1 1 1 1 1 1 1 1 1 1 2)))

(defvar beeps*cscale-both
        '((0 1 2 3 4 5 6 7 8 9 10 11 12 12 11 10 9 8 7 6 5 4 3 2 1 0)
          (1 1 1 1 1 1 1 1 1 1  1  1  2  1  1  1 1 1 1 1 1 1 1 1 1 2)))

(defvar beeps*joy
        '((12 11 9 7 -1 5 4 2 0 -1 7 9 -1 9 11 -1 11 12)
          (3  2  1 3  1 1 2 2 4  2 2 4  1 2  4  1  2  4)))

(defvar beeps*nuts
        '(( 6 18 17 15 13 11 10 10 10 -1  3 15 13 11 10  8  6)
          ( 2  3  1  1  1  1  1  1  4  2  2  3  1  1  1  1  2)))

(defvar beeps*kiss
        '((0 2 4 7 9 12 11 7 4 -1)
          (2 1 1 1 1  1  2 1 2  2)))


(defvar beeps*tree
        '((0 5 5 5 -1 7 9 9 9 -1 9 7 9 10 4 7 5 5)
          (2 1 1 2  0 2 1 1 2  2 2 1 1 2  2 2 1 3)))

(defvar beeps*deck
        '((14 12 11 9 7 9 11 7  9 11 12 9 11 9 7 6 7)
          ( 2  1  2 2 2 2  2 3  1 1   1 1 3  1 2 2 4)))

(defvar beeps*jingle
        '((4 4 4 -1 4 4 4 -1 4 7 0 2 4 -1 5 5 5 5 5 4 4 4 7 7 5 2 0)
          (2 2 2  2 2 2 2  2 2 2 3 1 4  2 2 2 3 1 2 2 3 1 2 2 2 2 4)))

(defvar beeps*zowie
         '((5  5  5  4  5  7  5  7  9  7  5  4  2  4  5)
           (1  1  2  1  1  2  1  1  2  1  1  2  1  1  2)))

(defvar beeps*races
        '((0 5 9 12 12 12 -1 9 9 9 -1 5 9 5 0 -1 0 5 9 12 12 12 -1 9 9 9 -1 0 0 0 5)
          (2 2 2 1  1  1   1 1 1 1  1 1 1 1 3  2 2 2 2 1  1  1   1 1 1 1  1 1 1 1 5)))

(defvar beeps*swars
        '((0  7 -1  5  4  2 12  7 -1  5  4  3 12  7 -1  5  4  4  2)
          (2  2  1  1  1  1  2  2  1  1  1  1  2  2  1  1  1  1  4)))

(defvar beeps*et
        '((12 19 17 16 14 16 12 7 -1  14 21 19 18 16 18 14)
          ( 2  6  2  2  2  2  4 6  2   2  6  2  2  2  2  6)))

(defvar beeps*tzone
        '((4 5 4 1 4 5 4 1 4 5 4 1 -1)
          (1 1 1 1 1 1 1 1 1 1 1 1 2)))

(defvar beeps*bat5
        '((6 6 6 3 -1 5 5 5 2)
          (1 1 1 3 1  1 1 1 3)))


(defun bsong (song)                             ;Test proceedure
              (play-song song 5 200000))

(defun play-song (song
                  &optional
                  (tone-scale (+ 1 (RANDOM 10)))
                  (master-duration (+ 40000 (RANDOM 60000)))
                  &aux song-vars notes Lengths)

  (setq song-vars
        (selector song string-equal
          ("zowie" beeps*zowie)
          ("races" beeps*races)
          ("swars" beeps*swars)
          ("et"    beeps*et)
          ("tzone" beeps*tzone)
          ("bat5"  beeps*bat5)
          ("deck"  beeps*deck)
          ("jingle" beeps*jingle)
          ("tree"   beeps*tree)
          ("kiss"   beeps*kiss)
          ("nuts"   beeps*nuts)
          ("joy"    beeps*joy)
          ("cscale-up" beeps*cscale-up)
          ("cscale-down" beeps*cscale-down)
          ("cscale-both" beeps*cscale-both)
          (otherwise beeps*cscale-up)))


  (setq Notes (first song-vars)
        Lengths (second song-vars))

  (play-notes notes lengths tone-scale master-duration))

(defun ZOWIE ()
  (play-song 'zowie))

(defun RACES ()
  (play-song 'races))

(defun SWARS ()
  (play-song 'swars))

(defun TZONE ()
  (play-song 'tzone))

(defun BAT5 ()
  (play-song 'bat5))


(defun PLAY-notes (notes lengths tone-scale master-duration &aux not len)
 (with-real-time
   (DO ((i 0 (1+ i)))
       ((= i (LENGTH notes)))
     (SETQ not (Nth i notes)
           Len (* (Nth i lengths) master-duration))
     (play-note -1  (truncate master-duration 10) tone-scale)   ;pause a bit
     (play-note not len tone-scale))))

(DEFUN play-note (note-number &optional (duration 50000) (tone-scale 32)
                  &aux tone)
  (Cond ((minusp note-number)
         (sys:%beep duration duration))
        (T
         (setq tone (* (NTH (REMAINDER note-number 12) beeps*notes) tone-scale))
         (setq tone (truncate tone (1+ (truncate note-number 12))))
         (sys:%beep tone duration))))

;----------------------------------------------------------------------------------------

(DEFUN BUZZ (WAVELENGTH DURATION &OPTIONAL (WAVELENGTH2 WAVELENGTH)
             &AUX (LOC 764110))
  (AND (MINUSP WAVELENGTH)
       (SETQ WAVELENGTH (MINUS WAVELENGTH)))
  (AND (MINUSP WAVELENGTH2)
       (SETQ WAVELENGTH2 (MINUS WAVELENGTH2)))
  (WITHOUT-INTERRUPTS
    (DO I 0 (1+ I) (= I DURATION)
        (DO J 0 (1+ J) (= J WAVELENGTH))
        (%UNIBUS-READ LOC)
        (DO J 0 (1+ J) (= J WAVELENGTH2))
        (%UNIBUS-READ LOC))))

(defun vbeep (tone volume
              &optional (duration 40000) (max-volume 20)
              &aux non-volume two-volume)
  "This routine attempts to create a beep with non-typical volume by imposing a very
   high frequency vibration to the on portion of the duty cycle of the base tone.
   The volume is then controlled by varying the duty cycle of this high frequency tone."

  (Without-interrupts
    (setq two-volume (+ volume volume)
          non-volume (- max-volume two-volume))
    (Do ((jj 0 (+ tone jj)))
        ((> jj (truncate duration 2)))
      (DO ((i 0 (+ i max-volume)))
          ((> i tone))
        (sys:%beep volume two-volume)
        (sys:%beep non-volume non-volume)
      (sys:%beep tone tone)))))

;----------------------------------------------------------------------------------------

(declare (special random-u2-random-array))
(setq random-u2-random-array NIL)

(defun RANDOM-U2 (&optional (rand-seed nil)
                 &aux (fineness 32768.))

" This routine will generate a uniformly distributed random number between -1 and 1.

  See the documentation Random-N for more complete documentation.

    The two differences between that routine and this one is that one generates
    normally distributed random numbers and this one generates linearly distributed
    random numbers, and that routine uses random-n-random-array as it's random array,
    while this routine uses random-u2-random-array for the same purpose.

........................................................................................"

  (COND ((NOT (NULL rand-seed))                 ; If rand-seed is non-zero, generate array
         (setq random-u2-random-array (si:random-create-array 71 35 rand-seed))))

  (COND ((NOT (arrayp random-u2-random-array))   ; random array not set up, use default
         (1- (// (FLOAT (RANDOM (* 2 fineness))) fineness)))
        (t
         (1- (// (FLOAT (RANDOM (* 2 fineness) random-u2-random-array)) fineness)))))

;----------------------------------------------------------------------------------------

(defdemo "Beep Hacks" "Hacks to make strange noises out of the lisp machine"
  "Beeps"
 ("- Documentation -" "More Documentation on these routines" (beeps))
 ("boop"      "Makes a sound with a randomish rising frequency." (boop))
 ("poob"      "Makes a similar sound to boop, but the frequency drops." (poob))
 ("oopb"      "Boop and poob at the same time with varying duty cycles." (oopb))
 ("spoob"     "One tone blending into another by varying duty cycles." (spoob))
 ("spoop"     "Rapidly alternating tone and nothing. Very sputtery." (spoop))
 ("broop"     "Alternating tones one up, one down." (broop))
 ("breep"     "Makes a wired sound with five alternaing frequencies." (breep))
 ("dreep"     "Like breep, but the sound goes up in frequency." (dreep))
 ("beep-rise" "Like breep, but the sound goes up in frequency." (beep-rise))
 ("freep" "Fractal noise, sounds a bit a cat walking on an organ keyboard." (freep))
 ("greep" "Like freep, but uses a gaussian noise function to sounds more musical." (greep))
 ("nreep"  "Four sets of greep going off at same time. Sounds like computer noise." (nreep))
 ("srieep" "Siren-like sound.  Sinusiodal variation of freq. between two freqs." (srieep))
 ("fsrieep" "Like srieep, but the endpoint freq. are changing exponentially" (fsrieep))
 ("cscale-up"   "Upward chromatic scale" (play-song 'cscale-up))
 ("cscale-down" "Downward chromatic scale" (play-song 'cscale-down))
 ("cscale"    "Chromatic scale" (play-song 'cscale-both))
 ("swars"     "Star Wars theme" (play-song 'swars))
 ("et"        "E.T. theme" (play-song 'et))
 ("zowie"     "Zowie phone ring" (play-song 'zowie))
 ("tzone"     "Twilight zone theme" (play-song 'tzone))
 ("rnd-beep"  "Plays a random beep out of this selection" (rnd-beep)))
