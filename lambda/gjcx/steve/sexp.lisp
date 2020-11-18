; -*- Mode:LISP; Package:STEVE; Readtable:CL; Base:10 -*-

;Copyright (c) May 1983 by Christopher Eliot
; and Massachusetts Institute of Technology.
;Permission to copy all or part of this material is granted, provided
; that the copies are not made or distributed for resale, the MIT
; copyright notice and reference to the source file and the software
; distribution version appear, and that notice is given that copying
; is by permission of Massachusetts Institute of Technology.

;

(defun bp-forward-over-sexp (bp)
 (send bp :forward-over-syntax (logior& prefix-mask white-space-mask))
 (send bp :forward-over-sexp)
 bp)

(defmethod (bp :forward-over-sexp) (&aux (max (line-char-count line))
                                    (chrs (line-chars line)))
 (cond ((>=& position max))
       ((of-syntax (char chrs position) paren-open)
        (loop with recursive-level = 0
              with chr
              do (cond ((=& position max)
                        (send self :to-beginning-of-next-line)
                        (setq max (line-char-count line)
                              chrs (line-chars line)))
                       (t (setq chr (char chrs position))
                          (setq position (1+& position))
                          (cond ((of-syntax chr string-quote)
                                 (setq position (1-& position))
                                 (send self :forward-over-string)
                                 (setq max (line-char-count line)
                                       chrs (line-chars line)))
                                ((of-syntax chr paren-open)
                                 (setq recursive-level (1+& recursive-level)))
                                ((of-syntax chr paren-close)
                                 (setq recursive-level (1-& recursive-level)))
                                ((of-syntax chr character-quote)
                                 (setq position (1+& position))))))
              while (+p recursive-level)))
       ((atom-char? (char chrs position))
        (send self :forward-over-atom))
       ((of-syntax (char chrs position) string-quote)
        (send self :forward-over-string))
       (t (send self :advance-pos 1)))
 self)

(defun bp-backward-over-sexp (bp)
 (send bp :backward-over-syntax white-space-mask)
 (send bp :backward-over-sexp)
 (send bp :backward-over-syntax prefix-mask)
 bp)

(defmethod (bp :backward-over-sexp) (&aux (chrs (line-chars line)))
  (send self :advance-pos -1)
  (cond ((-p position))
        ((of-syntax (char chrs position) paren-close)
         (loop with recursive-level = 0
               with chr
               do (cond ((bp-char-slashified? self)
                         (setq position (-& position 2)))
                        ((of-syntax (char chrs position) string-quote)
                         (setq position (1+& position))
                         (send self :backward-over-string)
                         (send self :advance-pos -1)
                         (setq chrs (line-chars line)))
                        (t (setq chr (char chrs position))
                           (setq position (1-& position))
                           (cond ((of-syntax chr paren-close)
                                  (setq recursive-level (1+& recursive-level)))
                                 ((of-syntax chr paren-open)
                                  (setq recursive-level
                                        (1-& recursive-level))))))
               if (<& position 0)
               do (loop while (and (<& position 0) ;In case of null lines.
                                   (line-previous line))
                        do (send self :to-end-of-previous-line)
                        do (when (+p recursive-level) ;Skip RETURN unless done.
                             (setq position (1-& position))) ;I Can hack SELF.
                        do (setq chrs (line-chars line))
                        finally (when (-p position)
                                  (unless (and (=& position -1)
                                               (null (line-previous line))
                                               (0p recursive-level))
                                    ;;Hack case of very beginning of buffer.
                                    (setq position 0)
                                    (ed-warn :unbalanced-parens))))
               while (+p recursive-level))
         (send self :advance-pos 1))
        ((or (atom-char? (char chrs position))
             (bp-char-slashified? self))
         (send self :backward-over-atom))
        ((of-syntax (char chrs position) string-quote)
         (setq position (1+& position)) ;Back to normal.
         (send self :backward-over-string)))
  self)
