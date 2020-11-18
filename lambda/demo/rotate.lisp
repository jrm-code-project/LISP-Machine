;;; -*- Mode:LISP; Package:HACKS; Base:10; Lowercase:T; Readtable:ZL -*-
;;; Created 11/24/81 09:57:32 by CMB
;;; Modified, moved to POINTER, and installed by DLW, 1/9/82

;;; This rotate function was translated from Smalltalk.  It appeared in the August 1981
;;; issue of Byte magazine.  The array must be square and a power of two bits on a side.
;;; The direction of rotation will be clockwise.  To rotate a 512x512 bit array takes
;;; about 5 seconds of solid bitblt time.  Rotate takes 2 + 15*log(N) bitblts.

(defconst .STOR tv:alu-seta)
(defconst .IOR tv:alu-ior)
(defconst .AND tv:alu-and)
(defconst .XOR tv:alu-xor)
(defconst .CLEAR 0)
(defconst .SET 15)
(defconst .NAND 2)

(defmacro copy-all-to (from xoffset yoffset to alu)
  `(bitblt ,alu (- width ,xoffset) (- width ,yoffset) ,from 0 0 ,to ,xoffset ,yoffset))

(defmacro copy-all-from (to xoffset yoffset from alu)
  `(bitblt ,alu (- width ,xoffset) (- width ,yoffset) ,from ,xoffset ,yoffset ,to 0 0))

(defun rotate (myself w)
  (let* ((width (array-dimension myself 0))
         (mask (make-array (list width width) :element-type 'bit))
         (temp (make-array (list width width) :element-type 'bit)))
    (copy-all-to mask 0 0 mask .CLEAR)
    (copy-all-from mask (truncate width 2) (truncate width 2) mask .SET)
    (do ((quad (truncate width 2) (truncate quad 2)))
        ((< quad 1))
      (copy-all-to mask 0 0 temp .STOR)         ; 1
      (copy-all-to mask 0 quad temp .IOR)               ; 2
      (copy-all-to myself 0 0 temp .AND)                ; 3
      (copy-all-to temp 0 0 myself .XOR)                ; 4
      (copy-all-from temp quad 0 myself .XOR)   ; 5
      (copy-all-from myself quad 0 myself .IOR) ; 6
      (copy-all-to temp quad 0 myself .XOR)     ; 7
      (copy-all-to myself 0 0 temp .STOR)       ; 8
      (copy-all-from temp quad quad myself .XOR)        ; 9
      (copy-all-to mask 0 0 temp .AND)          ; 10
      (copy-all-to temp 0 0 myself .XOR)                ; 11
      (copy-all-to temp quad quad myself .XOR)  ; 12
      (copy-all-from mask (truncate quad 2) (truncate quad 2) mask .AND)        ; 13
      (copy-all-to mask quad 0 mask .IOR)               ; 14
      (copy-all-to mask 0 quad mask .IOR)               ; 15
      (send w :bitblt tv:alu-seta width width myself 0 0 0 0)))
  myself)

(defvar *rotate-source* nil)
(defvar *rotate-size* 512)

(defun run-rotate ()
  (hof-window)
  (tv:window-call (*hof-window* :deactivate)
    (with-real-time
      (send *hof-window* :set-label "Life Window")
      (if (null *rotate-source*)
          (setq *rotate-source* (make-array (list *rotate-size* *rotate-size*)
                                            :element-type 'bit)))
      (send *hof-window* :clear-window)
      (bitblt tv:alu-xor *rotate-size* *rotate-size* *rotate-source* 0 0 *rotate-source* 0 0)
      ;; random text
      (princ (documentation 'format 'function) *hof-window*)
      (send *hof-window* :bitblt-from-sheet
                         tv:alu-seta *rotate-size* *rotate-size* 0 0 *rotate-source* 0 0)
      (rotate *rotate-source* *hof-window*)
      (send *hof-window* :tyi))))

(defdemo "Rotate"
         "A demonstration of an interesting algorithm for rotating a bit array."
  (run-rotate))

;;; This life function was translated from Smalltalk.  It appeared in the
;;; August 1981 issue of Byte magazine.  The array may be any size at all,
;;; as long as it fits on the screen.  Each generation of life takes 65
;;; bitblts.  If the loop was unrolled, some of the initial bitblts could
;;; be deleted since it is unnecessary to calculate the carrys and high order
;;; sums.

(defun life (window)
  (multiple-value-bind (w h)
      (send window :inside-size)
    (let* ((h2 (+ h 2))
           (w2 (+ w 2))
           (w32 (* (ceiling w2 32) 32))
           (myself (make-pixel-array w32 h :element-type 'bit))
           (nbr1   (make-pixel-array w32 h2 :element-type 'bit))
           (nbr2   (make-pixel-array w32 h2 :element-type 'bit))
           (nbr4   (make-pixel-array w32 h2 :element-type 'bit))
           (carry2 (make-pixel-array w32 h2 :element-type 'bit))
           (carry4 (make-pixel-array w32 h2 :element-type 'bit)))
      (send window :bitblt-from-sheet tv:alu-seta w h 0 0 myself 0 0)
      (dotimes (generation 100000)
        (bitblt .XOR w2 h2 nbr1   0 0 nbr1   0 0)
        (bitblt .XOR w2 h2 nbr2   0 0 nbr2   0 0)
        (bitblt .XOR w2 h2 nbr4   0 0 nbr4   0 0)
        (bitblt .XOR w2 h2 carry2 0 0 carry2 0 0)
        (bitblt .XOR w2 h2 carry4 0 0 carry4 0 0)
        (dolist (l '((0 0) (0 1) (0 2) (1 0) (1 2) (2 0) (2 1) (2 2)))
          (bitblt .STOR w2 h2 nbr1   0 0 carry2 0       0       )

          ;; carry2 = nbr1 .AND carry2
          (bitblt .AND  w  h  myself   0 0 carry2 (car l) (cadr l))

          ;; nbr1   = nbr1 .XOR myself
          (bitblt .XOR  w  h  myself   0 0 nbr1   (car l) (cadr l))
          (bitblt .STOR w2 h2 nbr2   0 0 carry4 0       0       )

          ;; carry4 = nbr2 .AND carry4
          (bitblt .AND  w2 h2 carry2 0 0 carry4 0       0       )

          ;; nbr2   = nbr2 .XOR carry2
          (bitblt .XOR  w2 h2 carry2 0 0 nbr2   0       0       )

          ;; nbr4   = nbr4 .XOR carry4
          (bitblt .XOR  w2 h2 carry4 0 0 nbr4   0       0       ))

        ;; myself = myself .AND nbr2
        (bitblt .AND    w  h  nbr2 1 1 myself 0 0)

        ;; nbr1 = nbr1 .AND nbr2
        (bitblt .AND    w2 h2 nbr2 0 0 nbr1 0 0)

        ;; myself = (myself .AND nbr2) .IOR (nbr1 .AND nbr2)
        (bitblt .IOR     w  h  nbr1 1 1 myself 0 0)

        ;; myself = (NOT nbr4) .AND ((myself .AND nbr2) .IOR (nbr1 .AND nbr2))
        (bitblt .NAND w  h  nbr4 1 1 myself 0 0)
        (send window :bitblt tv:alu-seta w h myself 0 0 0 0)
        (send window :home-cursor)
        (format window "~D" generation)
        (if (send window :tyi-no-hang) (return-from life t))
        )))
  window)

(defun run-life ()
  (little-hof-window)
  (tv:window-call (*little-hof-window* :deactivate)
    (with-real-time
      (send *little-hof-window* :set-label "Life Window")
      (multiple-value-bind (width height) (send *little-hof-window* :inside-size)
        (send *little-hof-window* :clear-window)
        (send *little-hof-window*
              :draw-line 100 (truncate height 2)
                         (- width 100) (truncate height 2)))
      (life *little-hof-window*))))

(defdemo "Life"
         "Conway's game of /"Life/", a cellular automaton demonstration.  By CMB."
  (run-life))
