;;; -*- Mode:LISP; Package:HEARTS; Readtable:CL; Base:10 -*-

(defconst *CARD-VALUES* '(2. 3. 4. 5. 6. 7. 8. 9. 10. J Q K A))

(defconst *ALL-SUITS* '(SPADES DIAMONDS HEARTS CLUBS))

(defconst THE-SPADES-SUIT
  (make-suit :symbol 'spades
             :cards (mapcar #'(lambda (value) (make-spade-card :value value))
                            *CARD-VALUES*)))

(defconst THE-HEARTS-SUIT
  (make-suit :symbol 'hearts
             :cards (mapcar #'(lambda (value) (make-heart-card :value value))
                            *CARD-VALUES*)))

(defconst THE-CLUBS-SUIT
  (make-suit :symbol 'clubs
             :cards (mapcar #'(lambda (value)  (make-club-card :value value))
                            *CARD-VALUES*)))

(defconst THE-DIAMONDS-SUIT
  (make-suit :symbol 'diamonds
             :cards (mapcar #'(lambda (value)  (make-diamond-card :value value))
                            *CARD-VALUES*)))

(defconst *THE-DECK* (append (suit-cards the-spades-suit)
                         (suit-cards the-hearts-suit)
                         (suit-cards the-clubs-suit)
                         (suit-cards the-diamonds-suit)))


(defconst NORTH (make-player :name 'north))
(defconst SOUTH (make-player :name 'south))
(defconst EAST (make-player :name 'east))
(defconst WEST (make-player :name 'west))

(defconst *THE-TABLE* (list NORTH EAST SOUTH WEST))
