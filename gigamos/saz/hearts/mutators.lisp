;;; -*- Mode:LISP; Package:HEARTS; Readtable:CL; Base:10 -*-

(defun CLEAR-ALL-HANDS ()
  (mapcar #'(lambda (player) (setf (player-hand player) (make-hand))) *the-table*))

(defun DEAL-HAND ()
  (clear-all-hands)
  (let ((the-deck *THE-DECK*))
    (dotimes (i (length the-deck))
      (let* ((player (nth (remainder i (length *the-table*)) *the-table*))
             (card (nth (random (- 52 i)) the-deck)))
        (selectq (card-suit card)
          (spades (give-spade card player))
          (hearts (give-heart card player))
          (clubs (give-club card player))
          (diamonds (give-diamond card player)))
        (setq the-deck (remq card the-deck))))))

(defun give-spade (card player)
  (setf (hand-spade-cards (player-hand player))
        (merge 'list (list card) (hand-spade-cards (player-hand player))
               'card-value-greater)))

(defun give-heart (card player)
  (setf (hand-heart-cards (player-hand player))
        (merge 'list (list card) (hand-heart-cards (player-hand player))
               'card-value-greater)))

(defun give-club (card player)
  (setf (hand-club-cards (player-hand player))
        (merge 'list (list card) (hand-club-cards (player-hand player))
               'card-value-greater)))

(defun give-diamond (card player)
  (setf (hand-diamond-cards (player-hand player))
        (merge 'list (list card) (hand-diamond-cards (player-hand player))
               'card-value-greater)))

;(defun pass-cards (player1 player2)
;  (let ((p1hand (player-hand player1))
;       (p2hand (player-hand player2)))
;    (let* ((number-of-spades (length (hand-spade-cards p1hand)))
;          (winning-spades (subset #'(lambda (card)
;                                      (card-value-greater
;                                        card
;                                        (make-card :value 'J)))
;                                  number-of-spades))
;          (heart-strength
