;;; -*- Mode:LISP; Package:HEARTS; Readtable:CL; Base:10 -*-
(defun CARD-VALUE-LESS (card1 card2)
  (< (find-position-in-list (card-value card1) *card-values*)
     (find-position-in-list (card-value card2) *card-values*)))

(defun CARD-VALUE-GREATER (card1 card2)
  (> (find-position-in-list (card-value card1) *card-values*)
     (find-position-in-list (card-value card2) *card-values*)))

(defun suit-strength (hand suit)
  (let* ((suit-fun
           (selectq suit
             (spades 'hand-spade-cards)
             (hearts 'hand-heart-cards)
             (clubs 'hand-club-cards)
             (diamonds 'hand-diamond-cards)))
         (suit-cards
           (mapcar 'card-value (apply suit-fun (list hand)))))
    (+ (* 5 (count 'a suit-cards))
       (* 4 (count 'k suit-cards))
       (* 3 (count 'q suit-cards))
       (* 2 (count 'j suit-cards))
       (* 1 (count 10 suit-cards)))))
