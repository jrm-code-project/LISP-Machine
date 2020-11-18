;;; -*- Mode:LISP; Package:HEARTS; Base:10; Readtable:CL -*-

(defun DISPLAY-HANDS ()
  (let ((hand (player-hand north))
        (west-hand (player-hand west))
        (east-hand (player-hand east))
        (south-hand (player-hand south)))
    (format t "~%~%~25TH: ~A~%~25TS: ~A~%~25TC: ~A~%~25TD: ~A~%"
            (mapcar 'card-value (hand-heart-cards hand))
            (mapcar 'card-value (hand-spade-cards hand))
            (mapcar 'card-value (hand-club-cards hand))
            (mapcar 'card-value (hand-diamond-cards hand)))
    (format t "~%~%S: ~A~50TS: ~A"
            (mapcar 'card-value (hand-spade-cards west-hand))
            (mapcar 'card-value (hand-spade-cards east-hand)))
    (format t "~%H: ~A~50TH: ~A"
            (mapcar 'card-value (hand-heart-cards west-hand))
            (mapcar 'card-value (hand-heart-cards east-hand)))
    (format t "~%C: ~A~50TC: ~A"
            (mapcar 'card-value (hand-club-cards west-hand))
            (mapcar 'card-value (hand-club-cards east-hand)))
    (format t "~%D: ~A~50TD: ~A"
            (mapcar 'card-value (hand-diamond-cards west-hand))
            (mapcar 'card-value (hand-diamond-cards east-hand)))
    (format t "~%~%~25TH: ~A~%~25TS: ~A~%~25TC: ~A~%~25TD: ~A~%"
            (mapcar 'card-value (hand-heart-cards south-hand))
            (mapcar 'card-value (hand-spade-cards south-hand))
            (mapcar 'card-value (hand-club-cards south-hand))
            (mapcar 'card-value (hand-diamond-cards south-hand)))))

