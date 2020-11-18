;;; -*- Mode:LISP; Package:HEARTS; Base:10; Readtable:CL -*-

(defstruct CARD
  "A thing with a suit and a (pip) value"
  suit
  value)

(defstruct SUIT
  "A collection of cards with an identifying symbol (one of
spades, hearts, clubs, or diamonds."
  symbol
  cards)

(defstruct (SPADE-CARD (:include card
                                 (suit 'spades))
                       (:conc-name spade-)))

(defstruct (HEART-CARD (:include card
                                 (suit 'hearts))
                       (:conc-name heart-)))

(defstruct (CLUB-CARD (:include card
                                (suit 'clubs))
                        (:conc-name club-)))

(defstruct (DIAMOND-CARD (:include card
                                   (suit 'diamonds))
                        (:conc-name diamond-)))

(defstruct HAND
  "At most thirteen cards, divided up into suits."
  spade-cards
  heart-cards
  club-cards
  diamond-cards)

(defstruct PLAYER
  "A side of the table and its hand (its cards)"
  name
  (hand (make-hand)))
