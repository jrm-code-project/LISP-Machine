;;; -*- Mode:LISP; Package:FED; Base:10; Fonts:(CPTFONTB); Readtable:ZL -*-

;;
;; Copyright (C) Lisp Machine, Inc. 1985
;;   See filename "Copyright" for
;; licensing and release information.

(defvar *scratch* nil
  "a scratch array, used by image tools in the grey package")

(defun make-*scratch* ()
  (or *scratch*
      (setq *scratch* (make-array '(1024. 1024.) ':type 'art-8b))))

(defconst *scratch-ctr-x* 512.S0)
(defconst *scratch-ctr-y* 512.S0)

(defun rolltst ()
  (loop for x from 75.S0 to 500.S0 by 50.S0
        for i from 0S0 by .5S0
        do (image-convex-region-rotate-scale-and-translate-copy
             (tv:sheet-screen-array color:color-screen)
             400.S0 300.S0
             '(((350.S0 275.S0) (450.S0 275.S0))
               ((350.S0 275.S0) (350.S0 325.S0))
               ((450.S0 275.S0) (450.S0 325.S0))
               ((350.S0 325.S0) (450.S0 325.S0)))
             i
             1S0
             (tv:sheet-screen-array color:color-screen)
             x 100.S0)))

(defun tst (x &optional p)
  (image-convex-region-rotate-scale-and-translate-copy
    (tv:sheet-screen-array color:color-screen)
    400.S0 300.S0
    '(((350.S0 275.S0) (450.S0 275.S0))
      ((350.S0 275.S0) (350.S0 325.S0))
      ((450.S0 275.S0) (450.S0 325.S0))
      ((350.S0 325.S0) (450.S0 325.S0)))
    x
    1
    (tv:sheet-screen-array color:color-screen)
    200.S0 100.S0 p))

(defun image-convex-region-rotate-scale-and-translate-copy (s-arr
                                                       s-ctr-x
                                                       s-ctr-y
                                                       s-envelope-linelist
                                                       rot-cw-radians
                                                       scale-factor
                                                       d-arr
                                                       d-ctr-x
                                                       d-ctr-y
                                                       &optional
                                                       flood-not-feather-p
                                                       extend-envelope-p
                                                       &aux
                                                       n-quarter-turns-cw
                                                       fine-signed-radians)

  "Rotate and zoom raster image within convex polygonal envelope about s-ctr in s-arr, result to d-arr
with s-ctr mapped to d-ctr.

All numeric arguments (and components thereof) may be real.

For speed, all floats should be short-floats: <sgn><foo>.<bar>S<sgn><exp>

Linelists are lists of lists of lists (X Y) of the endpoints of the polygon's sides.  Each side
must be specified, so each point will appear twice.

Envelopes with any thickness of less than a few pixels may be unpalatable.  I don't know.

For more info, see function distort-lines-or-columns-within-convex-polygonal-envelope."

  (multiple-value (n-quarter-turns-cw fine-signed-radians)
    (round (mod rot-cw-radians (* pi 2.0)) (short-float (// pi 2.0))))

  (setq n-quarter-turns-cw (nth-value 1 (floor n-quarter-turns-cw 4)))

  (distort-lines-or-columns-within-convex-polygonal-envelope
    (make-*scratch*)
    t
    *scratch-ctr-y*
    *scratch-ctr-x*
    (distort-lines-or-columns-within-convex-polygonal-envelope
      s-arr
      nil
      s-ctr-x
      s-ctr-y
      s-envelope-linelist
      (tan fine-signed-radians)
      (* scale-factor
         (cos fine-signed-radians))
      0
      t
      *scratch*
      *scratch-ctr-x*
      *scratch-ctr-y*
      t
      (if extend-envelope-p
          'double
        t))
    (// (sin fine-signed-radians) scale-factor -1S0)
    (// scale-factor
        (cos fine-signed-radians))
    n-quarter-turns-cw
    nil
    d-arr
    d-ctr-y
    d-ctr-x
    flood-not-feather-p))

(defun shear-scale-n-quarter-rot-cw-and-translate-coordinate (s-coord
                                                              index-or-nil ;nil if s-coord is index
                                                              s-ctr-index
                                                              s-ctr-distort
                                                              index-turned-cw-points-same-way-as-distort-p
                                                              s-shear-ratio
                                                              scale-factor
                                                              n-quarter-turns-cw
                                                              d-ctr-index  ;pretty bogus. x or y, same as s.
                                                              d-ctr-distort)

  "Intended primarily for use by distort-lines-or-columns-within-convex-polygonal-envelope.
User is expected to determine whether the returned value is a line or a column."

  (selectq n-quarter-turns-cw
    (0 (if index-or-nil
           (+ d-ctr-distort
              (* scale-factor
                 (+ (* (- index-or-nil
                          s-ctr-index)
                       s-shear-ratio)
                    (- s-coord
                       s-ctr-distort))))
         (+ d-ctr-index
            (- s-coord s-ctr-index))))
    (1 (if index-or-nil
           (+ d-ctr-index
              (* (if index-turned-cw-points-same-way-as-distort-p
                     -1
                   1)
                 (* scale-factor
                    (+ (* (- index-or-nil
                             s-ctr-index)
                          s-shear-ratio)
                       (- s-coord
                          s-ctr-distort)))))
         (+ d-ctr-distort
            (* (if index-turned-cw-points-same-way-as-distort-p
                   1
                 -1)
               (- s-coord s-ctr-index)))))
    (2 (if index-or-nil

           (- d-ctr-distort
              (* scale-factor
                 (+ (* (- index-or-nil
                          s-ctr-index)
                       s-shear-ratio)
                    (- s-coord
                       s-ctr-distort))))
         (- d-ctr-index
            (- s-coord s-ctr-index))))
    (3 (if index-or-nil
           (- d-ctr-index
              (* (if index-turned-cw-points-same-way-as-distort-p
                     -1
                   1)
                 (* scale-factor
                    (+ (* (- index-or-nil
                             s-ctr-index)
                          s-shear-ratio)
                       (- s-coord
                          s-ctr-distort)))))
         (- d-ctr-distort
            (* (if index-turned-cw-points-same-way-as-distort-p
                   1
                 -1)
               (- s-coord s-ctr-index)))))))

(defun distort-lines-or-columns-within-convex-polygonal-envelope (s-arr
                                                                  distort-s-lines-not-columns-p
                                                                  s-ctr-index
                                                                  s-ctr-distort
                                                                  s-envelope-linelist
                                                                  s-shear-ratio
                                                                  scale-factor
                                                                  n-quarter-turns-cw
                                                                  index-turned-cw-points-same-way-as-distort-p

                                                                  d-arr
                                                                  d-ctr-index
                                                                  d-ctr-distort

                                                                  &OPTIONAL
                                                                  flood-not-feather-p
                                                                  extend-envelope-p

                                                                  &AUX
                                                                  lesser-distort-limit-line
                                                                  greater-distort-limit-line
                                                                  head-list
                                                                  interior-list
                                                                  tail-list
                                                                  temp-list
                                                                  return-linelist
                                                                  terminal-index
                                                                  terminal-index-distorts
                                                                  crossing-lines
                                                                  center-distort
                                                                  lesser-back-crossing-distort
                                                                  lesser-front-crossing-distort
                                                                  greater-back-crossing-distort
                                                                  greater-front-crossing-distort
                                                                  max-t-i-d
                                                                  min-t-i-d
                                                                  greater-distort-area
                                                                  lesser-distort-area)

  "Shear and scale raster image within convex polygonal envelope about s-ctr in s-arr, result to d-arr
with s-ctr mapped to d-ctr.

Everything is real (haw haw) except quarter-turns (int 0 - 3).

This function was created primarily to be used by a rotate-and-zoom function, which would call it
once on lines and once on columns.

Fractional dest pixels can be flooded (for an intermediate dest) or feathered (for anti-aliasing).
\(Flooded pixels may have aberrant values when corresponding only slightly to source within the envelope,
however by the same token they will be all but ignored when used as source enveloped by the returned
linelist, so the final, anti-aliased result of a sequence of applications should be OK.)

Extending the envelope is also advisable for intermediate results.  All points are moved 4 pixels
out from the nominal center.

Linelists are lists of lists of lists (INDEX DISTORT) of the endpoints of the polygon's sides,
where INDEX is the dimension being left alone and DISTORT is the dimension being frobbed.

When feathering, weighting will taper up at each end of a strip from outer to inner intersections
of limit line with edges of strip.  Pointy features within a strip will thus be portrayed as flat;
corners as though bevelled.  Remember, this is just sub-pixel detail.

Enveloped region must have some chord longer than one pixel lying at any given angle.

For the first and last strips, the length of the non-bevelled sections above and below the nominal center
will be such as to provide the same area as the actual outline specified (or 0 length), in conjunction
with a 0 - 1 weighting of the entire strip depending on the exact values of greatest and least INDEX.

If n-quarter-turns-cw is 0, a linelist for the dest region is returned (for feeding to a subsequent
application as the source envelope with the opposite value of arg distort-s-lines-not-columns-p, etc.)

For speed, all floats should be short-floats: <sgn><foo>.<bar>S<sgn><exp>"


  ; PREP:
  ; Divide lines into head strip, interior, and tail strip.

 (if (equal 'double extend-envelope-p)     ;special for fed
     (setq s-envelope-linelist
           (loop for line in s-envelope-linelist
                 with radius-addend = (// 4S0 scale-factor)
                 with center = `(,s-ctr-index ,s-ctr-distort)
                 collect `(,(increase-radius (car line) center radius-addend)
                           ,(increase-radius (cadr line) center radius-addend)))))

 (setq return-linelist (copylist s-envelope-linelist))

 (if extend-envelope-p                     ;Make a safe intermediate result.
     (setq s-envelope-linelist
           (loop for line in s-envelope-linelist
                 with radius-addend = (// 4S0 scale-factor)
                 with center = `(,s-ctr-index ,s-ctr-distort)
                 collect `(,(increase-radius (car line) center radius-addend)
                           ,(increase-radius (cadr line) center radius-addend)))))

 (loop for line in s-envelope-linelist
       do (if (//= (caar line)             ;Throw out those dreadful infinite-slope lines.
                   (caadr line))
              (push (sortcar line #'<)
                    temp-list)))

 (setq s-envelope-linelist (sortcar temp-list #'(lambda (a b) (< (car a) (car b)))))

 (loop do (setq head-list (append head-list (list (pop s-envelope-linelist))))
       while (and s-envelope-linelist
                  (= (truncate (caaar s-envelope-linelist))
                     (truncate (caaar head-list)))))

 (loop initially (setq temp-list nil)
       for line in s-envelope-linelist
       do (push line temp-list)) ;reverse rest to get tail.

 (if temp-list
     (loop with end-int = (truncate (caadar temp-list))
           while (and temp-list
                      (= (truncate (caaar temp-list))
                         end-int))
           do (setq tail-list (append tail-list (list (pop temp-list))))))

 (loop for line in temp-list
       (push line interior-list)) ;unreverse

  ; HEAD STRIP:

 (setq terminal-index (caaar head-list))

 ; We know head terminal-index.  We will now find center-distort and border intersections.
 ; We will list DISTORTs of all points with INDEX terminal-index; center will be (max + min) / 2.
 ; Yow!  We can identify lines that cross the border 'cause they'll be half outside!

 (loop with int
       initially (setq crossing-lines nil
                       terminal-index-distorts nil
                       int (truncate terminal-index))
       for line in head-list
       do (if (//= (truncate (caadr line)) int)
              (push line crossing-lines))
       do (if (= (caar line) terminal-index)
              (push (cadar line) terminal-index-distorts)))
;(comment Zwei-lossage)

 (if (not (cdr crossing-lines))
     (ferror nil "Less than 2 lines leave the head strip -- region too thin somewhere or bogus linelist."))

 (if (> (+ (cadar (car crossing-lines))    ;compare midpoints (2 x midpoint distort, quick & effective)
           (cadadr (car crossing-lines)))
        (+ (cadar (cadr crossing-lines))
           (cadadr (cadr crossing-lines))))
     (psetq crossing-lines
            (list (cadr crossing-lines)
                  (car crossing-lines))))

 (let ((border-i (+ 1 (truncate terminal-index)))
       (a1i (caar (car crossing-lines)))
       (a1d (cadar (car crossing-lines)))
       (a2i (caadr (car crossing-lines)))
       (a2d (cadadr (car crossing-lines)))
       (b1i (caar (cadr crossing-lines)))
       (b1d (cadar (cadr crossing-lines)))
       (b2i (caadr (cadr crossing-lines)))
       (b2d (cadadr (cadr crossing-lines))))
   (setq lesser-front-crossing-distort (+ a1d (// (* (- a2d a1d) (- border-i a1i)) 1S0
                                                  (- a2i a1i)))
         greater-front-crossing-distort (+ b1d (// (* (- b2d b1d) (- border-i b1i)) 1S0
                                                   (- b2i b1i)))))

 ; center-distort will be restricted to lie between the border intersections.

 (setq center-distort (min greater-front-crossing-distort
                           (max lesser-front-crossing-distort
                                (// (+ (setq max-t-i-d (apply 'max terminal-index-distorts))
                                       (setq min-t-i-d (apply 'min terminal-index-distorts)))
                                    2S0))))

 ; Then we'll find greater-distort-area and lesser-distort-area.

 (loop initially (setq greater-distort-area 0      ;Follow linkages and sum areas.
                       lesser-distort-area 0
                       greater-distort-limit-line nil
                       lesser-distort-limit-line nil)

       for line in head-list

       do (if (= (caar line) terminal-index)       ;is this one of the 2 most terminal lines?
              (progn
                (if lesser-distort-limit-line
                    (if (< (cadar line)
                           (cadar lesser-distort-limit-line))
                        (setq lesser-distort-limit-line line)
                      (if (and (= (cadar line)
                                  (cadar lesser-distort-limit-line))
                               (< (// (- (cadadr line)
                                         (cadar line))
                                      (- (caadr line)
                                         (caar line))
                                      1.0)
                                  (// (- (cadadr lesser-distort-limit-line)
                                         (cadar lesser-distort-limit-line))
                                      (- (caadr lesser-distort-limit-line)
                                         (caar lesser-distort-limit-line))
                                      1.0)))
                          (setq lesser-distort-limit-line line)))
                  (setq lesser-distort-limit-line line))
                (if greater-distort-limit-line
                    (if (> (cadar line)
                           (cadar greater-distort-limit-line))
                        (setq greater-distort-limit-line line)
                      (if (and (= (cadar line)
                                  (cadar greater-distort-limit-line))
                               (> (// (- (cadadr line)
                                         (cadar line))
                                      (- (caadr line)
                                         (caar line))
                                      1.0)
                                  (// (- (cadadr greater-distort-limit-line)
                                         (cadar greater-distort-limit-line))
                                      (- (caadr greater-distort-limit-line)
                                         (caar greater-distort-limit-line))
                                      1.0)))
                          (setq greater-distort-limit-line line)))
                  (setq greater-distort-limit-line line)))
            (if (and lesser-distort-limit-line greater-distort-limit-line)
                (progn
                  (if (equalp (car line)
                              (cadr lesser-distort-limit-line))
                      (setq lesser-distort-area (+ lesser-distort-area
                                                   (* (- (caadr lesser-distort-limit-line)
                                                         (caar lesser-distort-limit-line))
                                                      (- center-distort
                                                         (// (+ (cadar lesser-distort-limit-line)
                                                                (cadadr lesser-distort-limit-line))
                                                             2S0))))
                            lesser-distort-limit-line line))
                  (if (equalp (car line)
                              (cadr greater-distort-limit-line))
                      (setq greater-distort-area (+ greater-distort-area
                                                    (* (- (caadr greater-distort-limit-line)
                                                          (caar greater-distort-limit-line))
                                                       (- (// (+ (cadar greater-distort-limit-line)
                                                                 (cadadr greater-distort-limit-line))
                                                              2S0)
                                                          center-distort)))
                            greater-distort-limit-line line)))
              (ferror nil "Croak! Nastiness during head area computation.")))

       finally (setq lesser-distort-area (+ lesser-distort-area
                                            (* (- (+ 1 (truncate terminal-index))
                                                  (caar lesser-distort-limit-line))
                                               (- center-distort
                                                  (// (+ (cadar lesser-distort-limit-line)
                                                         lesser-front-crossing-distort)
                                                      2S0))))
                     greater-distort-area (+ greater-distort-area
                                                (* (- (+ 1 (truncate terminal-index))
                                                      (caar greater-distort-limit-line))
                                                   (- (// (+ (cadar greater-distort-limit-line)
                                                             greater-front-crossing-distort)
                                                          2S0)
                                                      center-distort)))))

 ; Then we'll cook up some args for distort-line-or-column.

 (distort-line-or-column s-arr
                         (if distort-s-lines-not-columns-p ;s-line-or-column
                             'line
                           'column)
                         (truncate terminal-index) ;s-index
                         lesser-front-crossing-distort     ;s-start
                         greater-front-crossing-distort    ;s-end
                         d-arr
                         (if (zerop (logand n-quarter-turns-cw 1)) ;d-line-or-column
                             (if distort-s-lines-not-columns-p
                                 'line
                               'column)
                           (if distort-s-lines-not-columns-p
                               'column
                             'line))
                         (truncate (shear-scale-n-quarter-rot-cw-and-translate-coordinate
                                     (truncate terminal-index)
                                     nil
                                     s-ctr-index
                                     s-ctr-distort
                                     index-turned-cw-points-same-way-as-distort-p
                                     s-shear-ratio
                                     scale-factor
                                     n-quarter-turns-cw
                                     d-ctr-index
                                     d-ctr-distort))
                         (shear-scale-n-quarter-rot-cw-and-translate-coordinate
                           lesser-front-crossing-distort
                           (+ (truncate terminal-index) .5S0)
                           s-ctr-index
                           s-ctr-distort
                           index-turned-cw-points-same-way-as-distort-p
                           s-shear-ratio
                           scale-factor
                           n-quarter-turns-cw
                           d-ctr-index
                           d-ctr-distort)
                         (shear-scale-n-quarter-rot-cw-and-translate-coordinate
                           greater-front-crossing-distort
                           (+ (truncate terminal-index) .5S0)
                           s-ctr-index
                           s-ctr-distort
                           index-turned-cw-points-same-way-as-distort-p
                           s-shear-ratio
                           scale-factor
                           n-quarter-turns-cw
                           d-ctr-index
                           d-ctr-distort)
                         (if (not flood-not-feather-p)
                             (max 0 (* -2S0  ;s-begin-bevel-length
                                       (- (// lesser-distort-area
                                              (- 1S0 (nth-value 1 (truncate terminal-index))))
                                          (- center-distort lesser-front-crossing-distort)))))
                         (if (not flood-not-feather-p)
                             (max 0 (* -2S0  ;s-end-bevel-length
                                       (- (// greater-distort-area
                                              (- 1S0 (nth-value 1 (truncate terminal-index))))
                                          (- greater-front-crossing-distort center-distort)))))
                         (if (not flood-not-feather-p)
                             (- 1S0 (nth-value 1 (truncate terminal-index)))))     ;weight

  ; INTERIOR:

  ; Conveniently, limit lines have already been initialized.

 (loop for index from (+ 1S0 (truncate terminal-index))
       while (or (and interior-list
                      (or (cdr interior-list)
                          (< index (truncate (caadar interior-list)))))
                 (< index (truncate (caadr lesser-distort-limit-line))))   ;both dist-lim-lines end @ same index

       do (let ((a1i (caar lesser-distort-limit-line))
                (a1d (cadar lesser-distort-limit-line))
                (a2i (caadr lesser-distort-limit-line))
                (a2d (cadadr lesser-distort-limit-line))
                (b1i (caar greater-distort-limit-line))
                (b1d (cadar greater-distort-limit-line))
                (b2i (caadr greater-distort-limit-line))
                (b2d (cadadr greater-distort-limit-line)))
            (setq lesser-back-crossing-distort (+ a1d (// (* (- a2d a1d) (- index a1i)) 1.0S0
                                                          (- a2i a1i)))
                  greater-back-crossing-distort (+ b1d (// (* (- b2d b1d) (- index b1i)) 1.0S0
                                                           (- b2i b1i)))))
          (if (and (equalp index (truncate (caadr lesser-distort-limit-line)))
                   (equalp (cadr lesser-distort-limit-line)        ;process any vertices
                           (caar interior-list)))
              (setq lesser-distort-limit-line (pop interior-list)))
          (if (and (equalp index (truncate (caadr greater-distort-limit-line)))
                   (equalp (cadr greater-distort-limit-line)
                           (caar interior-list)))
              (setq greater-distort-limit-line (pop interior-list)))
          (let ((border-i (+ 1S0 index))
                (a1i (caar lesser-distort-limit-line))
                (a1d (cadar lesser-distort-limit-line))
                (a2i (caadr lesser-distort-limit-line))
                (a2d (cadadr lesser-distort-limit-line))
                (b1i (caar greater-distort-limit-line))
                (b1d (cadar greater-distort-limit-line))
                (b2i (caadr greater-distort-limit-line))
                (b2d (cadadr greater-distort-limit-line)))
            (setq lesser-front-crossing-distort (+ a1d (// (* (- a2d a1d) (- border-i a1i)) 1.0S0
                                                           (- a2i a1i)))
                  greater-front-crossing-distort (+ b1d (// (* (- b2d b1d) (- border-i b1i)) 1.0S0
                                                            (- b2i b1i)))))

          (distort-line-or-column s-arr
                                  (if distort-s-lines-not-columns-p ;s-line-or-column
                                      'line
                                    'column)
                                  index
                                  (min lesser-back-crossing-distort
                                       lesser-front-crossing-distort)
                                  (max greater-back-crossing-distort
                                       greater-front-crossing-distort)
                                  d-arr
                                  (if (zerop (logand n-quarter-turns-cw 1)) ;d-line-or-column
                                      (if distort-s-lines-not-columns-p
                                          'line
                                        'column)
                                    (if distort-s-lines-not-columns-p
                                        'column
                                      'line))
                                  (truncate (shear-scale-n-quarter-rot-cw-and-translate-coordinate
                                              index
                                              nil
                                              s-ctr-index
                                              s-ctr-distort
                                              index-turned-cw-points-same-way-as-distort-p
                                              s-shear-ratio
                                              scale-factor
                                              n-quarter-turns-cw
                                              d-ctr-index
                                              d-ctr-distort))
                                  (shear-scale-n-quarter-rot-cw-and-translate-coordinate
                                    (min lesser-back-crossing-distort
                                         lesser-front-crossing-distort)
                                    (+ index .5S0)
                                    s-ctr-index
                                    s-ctr-distort
                                    index-turned-cw-points-same-way-as-distort-p
                                    s-shear-ratio
                                    scale-factor
                                    n-quarter-turns-cw
                                    d-ctr-index
                                    d-ctr-distort)
                                  (shear-scale-n-quarter-rot-cw-and-translate-coordinate
                                    (max greater-back-crossing-distort
                                         greater-front-crossing-distort)
                                    (+ index .5S0)
                                    s-ctr-index
                                    s-ctr-distort
                                    index-turned-cw-points-same-way-as-distort-p
                                    s-shear-ratio
                                    scale-factor
                                    n-quarter-turns-cw
                                    d-ctr-index
                                    d-ctr-distort)
                                  (if (not flood-not-feather-p)
                                      (abs (- lesser-back-crossing-distort         ;begin bevel length
                                              lesser-front-crossing-distort)))
                                  (if (not flood-not-feather-p)
                                      (abs (- greater-back-crossing-distort        ;end bevel length
                                              greater-front-crossing-distort)))))

 ; TAIL STRIP:
 ; This will bear a remarkable resemblance to the head strip.

 (setq tail-list (append tail-list (list lesser-distort-limit-line greater-distort-limit-line)))

 (if (not (zerop (nth-value 1 (truncate (setq terminal-index (caadar tail-list))))))
     (progn

 ; We know tail terminal-index.  We will now find center-distort and border intersections.
 ; We will list DISTORTs of all points with INDEX terminal-index; center will be (max + min) / 2.
 ; We can identify lines that cross the border 'cause they'll be half outside.

       (loop with int = (truncate terminal-index)
             for line in tail-list
             initially (setq crossing-lines nil
                             terminal-index-distorts nil)
             do (if (//= (truncate (caar line)) int)
                    (push line crossing-lines))
             do (if (= (caadr line) terminal-index)
                    (push (cadadr line) terminal-index-distorts)))

       (if (not (cdr crossing-lines))
           (ferror nil "Less than 2 lines leave the tail strip -- region too thin somewhere or bogus linelist."))

       (if (> (+ (cadadr (car crossing-lines)) (cadar (car crossing-lines)))
              (+ (cadadr (cadr crossing-lines)) (cadar (cadr crossing-lines))))
           (psetq crossing-lines
                  (list (cadr crossing-lines)
                        (car crossing-lines))))

       (let ((border-i (truncate terminal-index))
             (a1i (caar (car crossing-lines)))
             (a1d (cadar (car crossing-lines)))
             (a2i (caadr (car crossing-lines)))
             (a2d (cadadr (car crossing-lines)))
             (b1i (caar (cadr crossing-lines)))
             (b1d (cadar (cadr crossing-lines)))
             (b2i (caadr (cadr crossing-lines)))
             (b2d (cadadr (cadr crossing-lines))))
         (setq lesser-back-crossing-distort (+ a1d (// (* (- a2d a1d) (- border-i a1i)) 1.0S0
                                                       (- a2i a1i)))
               greater-back-crossing-distort (+ b1d (// (* (- b2d b1d) (- border-i b1i)) 1.0S0
                                                        (- b2i b1i)))))

 ; center-distort will be restricted to lie between the border intersections.

       (setq center-distort (min greater-back-crossing-distort
                                 (max lesser-back-crossing-distort
                                      (// (+ (setq max-t-i-d (apply 'max terminal-index-distorts))
                                             (setq min-t-i-d (apply 'min terminal-index-distorts)))
                                          2.S0))))

 ; Then we'll find greater-distort-area and lesser-distort-area.

       (loop initially (setq greater-distort-area 0S0      ;Follow linkages and sum areas.
                             lesser-distort-area 0S0
                             greater-distort-limit-line nil
                             lesser-distort-limit-line nil)

             for line in tail-list

             do (if (= (caadr line) terminal-index)
                    (progn
                      (if lesser-distort-limit-line
                          (if (< (cadadr line)
                                 (cadadr lesser-distort-limit-line))
                              (setq lesser-distort-limit-line line)
                            (if (and (= (cadadr line)
                                        (cadadr lesser-distort-limit-line))
                                     (> (// (- (cadadr line)       ;we're interested in minus INDEX direction
                                               (cadar line))       ;so slope comparison is opposite of head's
                                            (- (caadr line)
                                               (caar line))
                                            1.0)
                                        (// (- (cadadr lesser-distort-limit-line)
                                               (cadar lesser-distort-limit-line))
                                            (- (caadr lesser-distort-limit-line)
                                               (caar lesser-distort-limit-line))
                                            1.0)))
                                (setq lesser-distort-limit-line line)))
                        (setq lesser-distort-limit-line line))
                      (if greater-distort-limit-line
                          (if (> (cadadr line)
                                 (cadadr greater-distort-limit-line))
                              (setq greater-distort-limit-line line)
                            (if (and (= (cadadr line)
                                        (cadadr greater-distort-limit-line))
                                     (> (// (- (cadadr line)
                                               (cadar line))
                                            (- (caadr line)
                                               (caar line))
                                            1.0)
                                        (// (- (cadadr greater-distort-limit-line)
                                               (cadar greater-distort-limit-line))
                                            (- (caadr greater-distort-limit-line)
                                               (caar greater-distort-limit-line))
                                            1.0)))
                                (setq greater-distort-limit-line line)))
                        (setq greater-distort-limit-line line)))
                  (if (and lesser-distort-limit-line greater-distort-limit-line)
                      (progn
                        (if (equalp (cadr line)
                                    (car lesser-distort-limit-line))
                            (setq lesser-distort-area (+ lesser-distort-area
                                                         (* (- (caadr lesser-distort-limit-line)
                                                               (caar lesser-distort-limit-line))
                                                            (- center-distort
                                                               (// (+ (cadar lesser-distort-limit-line)
                                                                      (cadadr lesser-distort-limit-line))
                                                                   2.S0))))
                                  lesser-distort-limit-line line))
                        (if (equalp (cadr line)
                                    (car greater-distort-limit-line))
                            (setq greater-distort-area (+ greater-distort-area
                                                          (* (- (caadr greater-distort-limit-line)
                                                                (caar greater-distort-limit-line))
                                                             (- (// (+ (cadar greater-distort-limit-line)
                                                                       (cadadr greater-distort-limit-line))
                                                                    2.S0)
                                                                center-distort)))
                                  greater-distort-limit-line line)))
                    (ferror nil "Croak! Nastiness during tail area computation.")))

             finally (setq lesser-distort-area (+ lesser-distort-area
                                                  (* (- (caadr lesser-distort-limit-line)
                                                        (truncate terminal-index))
                                                     (- center-distort
                                                        (// (+ (cadadr lesser-distort-limit-line)
                                                               lesser-back-crossing-distort)
                                                            2.S0))))
                           greater-distort-area (+ greater-distort-area
                                                      (* (- (caadr greater-distort-limit-line)
                                                            (truncate terminal-index))
                                                         (- (// (+ (cadadr greater-distort-limit-line)
                                                                   greater-back-crossing-distort)
                                                                2.S0)
                                                            center-distort)))))



 ; Then we'll cook up some args for distort-line-or-column.

       (distort-line-or-column s-arr
                               (if distort-s-lines-not-columns-p ;s-line-or-column
                                   'line
                                 'column)
                               (truncate terminal-index) ;s-index
                               lesser-back-crossing-distort        ;s-start
                               greater-back-crossing-distort       ;s-end
                               d-arr
                               (if (zerop (logand n-quarter-turns-cw 1)) ;d-line-or-column
                                   (if distort-s-lines-not-columns-p
                                       'line
                                     'column)
                                 (if distort-s-lines-not-columns-p
                                     'column
                                   'line))
                               (truncate (shear-scale-n-quarter-rot-cw-and-translate-coordinate
                                           (truncate terminal-index)
                                           nil
                                           s-ctr-index
                                           s-ctr-distort
                                           index-turned-cw-points-same-way-as-distort-p
                                           s-shear-ratio
                                           scale-factor
                                           n-quarter-turns-cw
                                           d-ctr-index
                                           d-ctr-distort))
                               (shear-scale-n-quarter-rot-cw-and-translate-coordinate
                                 lesser-back-crossing-distort
                                 (+ (truncate terminal-index) .5S0)
                                 s-ctr-index
                                 s-ctr-distort
                                 index-turned-cw-points-same-way-as-distort-p
                                 s-shear-ratio
                                 scale-factor
                                 n-quarter-turns-cw
                                 d-ctr-index
                                 d-ctr-distort)
                               (shear-scale-n-quarter-rot-cw-and-translate-coordinate
                                 greater-back-crossing-distort
                                 (+ (truncate terminal-index) .5S0)
                                 s-ctr-index
                                 s-ctr-distort
                                 index-turned-cw-points-same-way-as-distort-p
                                 s-shear-ratio
                                 scale-factor
                                 n-quarter-turns-cw
                                 d-ctr-index
                                 d-ctr-distort)
                               (if (not flood-not-feather-p)
                                   (max 0 (* -2S0  ;s-begin-bevel-length
                                             (- (// lesser-distort-area
                                                    (nth-value 1 (truncate terminal-index)))
                                                (- center-distort lesser-back-crossing-distort)))))
                               (if (not flood-not-feather-p)
                                   (max 0 (* -2S0  ;s-end-bevel-length
                                             (- (// greater-distort-area
                                                    (nth-value 1 (truncate terminal-index)))
                                                (- greater-back-crossing-distort center-distort)))))
                               (if (not flood-not-feather-p)
                                   (nth-value 1 (truncate terminal-index))))))

 ;RETURN VALUE

 (if (zerop n-quarter-turns-cw) ;I don't need anything more than this and it's a damned sight easier.
     (loop for line in return-linelist
           collect `((,(shear-scale-n-quarter-rot-cw-and-translate-coordinate
                                 (cadar line)
                                 (caar line)
                                 s-ctr-index
                                 s-ctr-distort
                                 index-turned-cw-points-same-way-as-distort-p
                                 s-shear-ratio
                                 scale-factor
                                 0
                                 d-ctr-index
                                 d-ctr-distort)
                      ,(shear-scale-n-quarter-rot-cw-and-translate-coordinate
                                 (caar line)
                                 nil
                                 s-ctr-index
                                 s-ctr-distort
                                 index-turned-cw-points-same-way-as-distort-p
                                 s-shear-ratio
                                 scale-factor
                                 0
                                 d-ctr-index
                                 d-ctr-distort))
                     (,(shear-scale-n-quarter-rot-cw-and-translate-coordinate
                                 (cadadr line)
                                 (caadr line)
                                 s-ctr-index
                                 s-ctr-distort
                                 index-turned-cw-points-same-way-as-distort-p
                                 s-shear-ratio
                                 scale-factor
                                 0
                                 d-ctr-index
                                 d-ctr-distort)
                      ,(shear-scale-n-quarter-rot-cw-and-translate-coordinate
                                 (caadr line)
                                 nil
                                 s-ctr-index
                                 s-ctr-distort
                                 index-turned-cw-points-same-way-as-distort-p
                                 s-shear-ratio
                                 scale-factor
                                 0
                                 d-ctr-index
                                 d-ctr-distort))))))

; 10/10/86 Why not used TAN as defined in SYS: SYS2; NUMER?
;(defun tan (ang)
;  (// (sin ang) (cos ang)))

; In SYS: SYS2; NUMER
;(defun tan (x)
;  "Tangent of an angle measured in radians.
;Small flonum arg gets small flonum value."
;  (float (// (sin x 0.0) (sin (+ x 1.570796326) 0.0)) x))


(defun distort-line-or-column (source-array
                               s-line-or-column    ;really, 'LINE or 'COLUMN
                               s-index     ;which line or column
                               s-start
                               s-end
                               dest-array
                               d-line-or-column
                               d-index
                               d-start
                               d-end
                               &optional
                               (s-begin-bevel-length nil)          ;NEW MEANING OF THESE FIELDS!!!
                               (s-end-bevel-length nil)
                               (weight 1)
                               &aux        ;LOCAL VARS
                               s-npixels
                               d-npixels
                               s-over-d-npixels    ;ratios
                               d-over-s-npixels
                               orig-s-start
                               s-start-int ;the starts will change
                               s-start-frac
                               d-start-int
                               d-start-frac
                               d-end-int
                               d-end-frac
                               s-line
                               s-column
                               d-line
                               d-column
                               gulp-in-d   ;gulp size in units of d-pixels
                               pixel
                               terminal-frac
                               invertp)
  "Fundamental Weiman raster routine.  Source start must be less than end, dest need not be.
If s-begin-bevel-length is nil, all fractional dest pixels will be flooded.
Weight and bevels are for good feathering-in, i.e. anti-aliasing.  (Jaggie prevention.)

For speed, all floats should be short-floats: <sgn><foo>.<bar>S<sgn><exp>"

  (if (not (zerop (nth-value 1 (truncate s-index))))
      (ferror nil "s-index, ~d, has a fractional part." s-index))

  (if (not (zerop (nth-value 1 (truncate d-index))))
      (ferror nil "d-index, ~d, has a fractional part." d-index))

  (if (and (> s-end s-start)
           (//= d-start d-end))
      (progn

        (if s-begin-bevel-length
            (setq s-begin-bevel-length (+ s-begin-bevel-length 1)          ;0-indexed makes more sense
                  s-end-bevel-length (+ s-end-bevel-length 1)))    ; but 1-indexed works better for this.

        (setq s-npixels (abs (- s-end s-start))
              d-npixels (abs (- d-end d-start))
              invertp (< d-end d-start))

        (setq s-over-d-npixels (// s-npixels 1.0S0 d-npixels)      ;1.0S0 forces flonum
              d-over-s-npixels (// d-npixels 1.0S0 s-npixels)
              s-index (truncate s-index)
              d-index (truncate d-index))

        (selectq d-line-or-column
          (column (setq d-column d-index))
          (line (setq d-line d-index)))

        (selectq s-line-or-column
          (column (setq s-column s-index))
          (line (setq s-line s-index)))

        (setq orig-s-start s-start)        ;used for feather-in

      ;ragged dest. ends are averaged with present contents
      ; of that pixel (background), weighting ratio is
      ; based on overlap (of course).

        (if (not (zerop (progn
                          (multiple-value (d-start-int d-start-frac)
                            (truncate d-start))
                          d-start-frac)))
            (progn
              (setq pixel 0
                    terminal-frac (if invertp d-start-frac (- 1 d-start-frac)))
              (loop until (or (zerop (progn
                                       (multiple-value (d-start-int d-start-frac)
                                         (truncate d-start))
                                       d-start-frac))
                              (= d-start d-end)
                              (>= s-start s-end))
                    do (progn
                         (multiple-value (s-start-int s-start-frac)
                           (truncate s-start))
                         (setq gulp-in-d (min (if invertp (if (zerop d-start-frac)
                                                              1
                                                             d-start-frac)
                                                (- 1 d-start-frac))
                                              (* d-over-s-npixels (- 1 s-start-frac))
                                              d-npixels))
                         (selectq s-line-or-column
                           (column (setq s-line s-start-int))
                           (line (setq s-column s-start-int)))
                         (setq pixel (+ pixel
                                        (* gulp-in-d
                                           (aref source-array s-line s-column)))
                               s-start (+ s-start (* s-over-d-npixels gulp-in-d))
                               d-start (if invertp (- d-start gulp-in-d) (+ d-start gulp-in-d))))
                    if (= (+ d-start (if invertp
                                         (- gulp-in-d)
                                       gulp-in-d)) d-start) (loop-finish))  ;x + {y|y!=0} == x sometimes!!!

              (selectq d-line-or-column
                (column (setq d-line (if invertp d-start-int (- d-start-int 1))))
                (line (setq d-column (if invertp d-start-int (- d-start-int 1)))))
              (aset (max 0 (min 255. (round (if (not s-begin-bevel-length)
                                                 (// pixel 1S0 terminal-frac)
                                               (+ (* weight
                                                     (max 0 (min 1 (// (- s-start orig-s-start) 1.0S0
                                                                       s-begin-bevel-length)))
                                                     pixel)
                                                  (* (- 1 (* weight
                                                             (max 0 (min 1 (// (- s-start orig-s-start) 1.0S0
                                                                               s-begin-bevel-length)))
                                                             terminal-frac))
                                                     (aref dest-array d-line d-column)))))))
                    dest-array
                    d-line
                    d-column)))

      ;now the center

        (multiple-value (d-start-int d-start-frac)
          (truncate d-start))

        (multiple-value (d-end-int d-end-frac)
          (truncate d-end))

        (setq pixel 0)

        (if (> (abs (- d-end d-start)) 1)
            (loop with d-start-at-last-aset = 999999999
                  while (//= d-start d-end-int)  ;save ragged end for special treatment
                  do (progn
                       (multiple-value (s-start-int s-start-frac)
                         (truncate s-start))
                       (setq gulp-in-d (min (if invertp (if (zerop d-start-frac)
                                                            1
                                                          d-start-frac)
                                              (- 1 d-start-frac))
                                                   (* d-over-s-npixels (- 1 s-start-frac))))
                       (selectq s-line-or-column
                         (column (setq s-line s-start-int))
                         (line (setq s-column s-start-int)))
                       (setq pixel (+ pixel (* gulp-in-d
                                               (aref source-array s-line s-column)))
                             s-start (+ s-start (* s-over-d-npixels gulp-in-d))
                             d-start (if invertp (- d-start gulp-in-d) (+ d-start gulp-in-d)))
                       (multiple-value (d-start-int d-start-frac)
                         (truncate d-start))
                       (if (and (zerop d-start-frac)
                                (< .5S0 (abs (- d-start d-start-at-last-aset))))
                                ;(//= d-start (+ d-start gulp-in-d)))   ;x + {y|y!=0} == x sometimes!!!
                           (progn
                             (selectq d-line-or-column
                               (column (setq d-line (if invertp d-start-int (- d-start-int 1))))
                               (line (setq d-column (if invertp d-start-int (- d-start-int 1)))))
                             (aset (max 0 (min 255. (round (if (not s-begin-bevel-length)
                                                                pixel
                                                              (+ (* weight
                                                                    (max 0 (min 1
                                                                                (// (- s-start orig-s-start)
                                                                                    1.0S0
                                                                                    s-begin-bevel-length)
                                                                                (// (- s-end s-start) 1.0S0
                                                                                    s-end-bevel-length)))
                                                                    pixel)
                                                                 (* (- 1
                                                                       (* weight
                                                                          (max 0
                                                                               (min 1
                                                                                    (// (- s-start orig-s-start)
                                                                                        1.0S0
                                                                                        s-begin-bevel-length)
                                                                                    (// (- s-end s-start) 1.0S0
                                                                                        s-end-bevel-length)))))
                                                                    (aref dest-array d-line d-column)))))))
                                   dest-array
                                   d-line
                                   d-column)
                             (setq pixel 0
                                   d-start-at-last-aset d-start))))))

      ;now the ragged end, if non-zero size, is averaged into the background.

        (if (not (zerop d-end-frac))
            (progn
              (setq terminal-frac 0
                    pixel 0)
              (loop until (or (= d-start d-end)
                              (>= s-start s-end)) ;used to hang sometimes without this s-check.
                    do (progn
                         (multiple-value (s-start-int s-start-frac)
                           (truncate s-start))
                         (setq gulp-in-d (min (if invertp (- 1 d-end-frac) d-end-frac)
                                              (* d-over-s-npixels (- 1 s-start-frac))
                                              (* d-over-s-npixels (abs (- s-end s-start)))))
                         (selectq s-line-or-column
                           (column (setq s-line s-start-int))
                           (line (setq s-column s-start-int)))
                         (setq pixel (+ pixel
                                        (* gulp-in-d
                                           (aref source-array s-line s-column)))
                               s-start (+ s-start (* s-over-d-npixels gulp-in-d))
                               d-start (if invertp (- d-start gulp-in-d) (+ d-start gulp-in-d))
                               terminal-frac (+ terminal-frac gulp-in-d)))
                    if (= (+ d-start gulp-in-d) d-start) (loop-finish))   ;x + {y|y!=0} == x sometimes!!!
              (selectq d-line-or-column
                (column (setq d-line d-end-int))
                (line (setq d-column d-end-int)))
              (if (not (zerop terminal-frac))
                  (aset (max 0 (min 255. (round (if (not s-begin-bevel-length)
                                                     (// pixel 1.0S0 terminal-frac)
                                                   (+ (* weight
                                                         (max 0 (min 1 (// (- s-end s-start) 1.0S0
                                                                           s-end-bevel-length)))
                                                         pixel)
                                                      (* (- 1 (* terminal-frac
                                                                 weight
                                                                 (max 0 (min 1 (// (- s-end s-start) 1.0S0
                                                                                   s-end-bevel-length)))))
                                                         (aref dest-array d-line d-column)))))))
                        dest-array
                        d-line
                        d-column)))))

    (if (< s-end s-start)
        (ferror nil "Asked to traverse source backwards; not supported."))))

(defun increase-radius (point
                        center
                        radius-addend)
  (let* ((delta-i (- (car point)
                     (car center)))
         (delta-d (- (cadr point)
                     (cadr center)))
         (theta (atan delta-d delta-i))
         (r (+ radius-addend (sqrt (+ (* delta-i delta-i)
                                      (* delta-d delta-d))))))
    `(,(+ (car center) (* r (cos theta)))
      ,(+ (cadr center) (* r (sin theta))))))
