;;; -*- Mode:LISP; Package:ZWEI; Base:10 -*-

(defvar *zmw* nil)
(defvar *zw* nil)
(defvar *mw* nil)
(defun gw (&aux zmw zw mw)
  (setq *zmw* nil *zw* nil *mw* nil)
  (dolist
    (wind (setq zmw (send (car (send tv:main-screen :exposed-inferiors)) :exposed-inferiors)))
    (when (typep wind 'zmacs-window-pane)
      (setq zw wind))
    (when (typep wind 'mode-line-window)
      (setq mw wind)))
  (when (and zmw zw mw)
    (setq *zmw* zmw *zw* zw *mw* mw)))


(defun dz (&optional (delta 0) &aux zll ztt zrr zbb mll mtt mrr mbb)
  (without-interrupts
    (with-height-changes-allowed
      (multiple-value-setq (zll ztt zrr zbb) (send *zw* :edges))
      (multiple-value-setq (mll mtt mrr mbb) (send *mw* :edges))
      (cond ((and (> delta 0)
                  (send *mw* :set-edges mll (+ mtt delta) mrr mbb :verify)
                  (send *zw* :set-edges zll ztt zrr (+ zbb delta) :verify))
             (send *mw* :set-edges mll (+ mtt delta) mrr mbb)
             (send *zw* :set-edges zll ztt zrr (+ zbb delta)))
            ((and (send *zw* :set-edges zll ztt zrr (+ zbb delta) :verify)
                  (send *mw* :set-edges mll (+ mtt delta) mrr mbb :verify))
             (send *zw* :set-edges zll ztt zrr (+ zbb delta))
             (send *mw* :set-edges mll (+ mtt delta) mrr mbb))
            (t "ILLEGAL HEIGHT CHANGE")))))


(defmacro with-height-changes-allowed (&body body)
  "Execute BODY, allowing height changes in fixed-height windows."
  `(let ((*allow-changing-height* t))
     . ,body))
