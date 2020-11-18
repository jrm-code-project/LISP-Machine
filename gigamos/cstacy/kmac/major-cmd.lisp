;;; -*- Mode:LISP; Package:MAC; Base:10; Readtable:ZL -*-
;;;
;;;
;;;



(defun ready-to-boot ()
  (lam:clear-map)
  (setq *BC-memory-available* BC-base-adr)
  (allocate-communication-root)
  (init-device-free-list)
  (init-pathname-free-list)
  (init-info-free-list)
  (allocate-buffers)
  (process-wait "BC" #'(lambda () (//= (mac-protocol-version  Header-Table) 0)))
  (setf (falcon-protocol-version Header-Table) Falcon-version-number)
  t)



;;;
;;;  Major K commands (to the Mac)
;;;



(defmacro with-idle-state-secured (&body body)
  `(let ((struct (Major-CMD-blk Header-Table)))
     (process-wait "BC" #'(lambda () (let ((state (Command-state struct)))
                                       (or (= state ST-IDLE)
                                           (= state ST-DONE)
                                           (= state ST-ERROR)
                                           (= state ST-FAULT)))))
     (setf (Command-state struct) ST-FILLING)
     (unwind-protect
         (prog1
           ,@body
           (setf   (Command-state struct) ST-FILLED)
           (process-wait "BC" #'(lambda () (let ((state (Command-state struct)))
                                             (or (= state ST-DONE)
                                                 (= state ST-ERROR)
                                                 (= state ST-FAULT))))))
       (setf (Command-state struct) ST-IDLE))))


(defun major-k-quit ()
  (with-idle-state-secured
    (setf (Command-command struct)  CMD-QUIT)
    (setf (Command-arg-count struct)0)))


(defun major-k-reboot ()
  (with-idle-state-secured
    (setf (Command-command struct)  CMD-REBOOT)
    (setf (Command-arg-count struct)0)))


(defun major-k-shutdown ()
  (with-idle-state-secured
    (setf (Command-command struct)  CMD-SHUTDOWN)
    (setf (Command-arg-count struct)0)))



(defun major-k-create-device (type)
  (with-idle-state-secured
    (let ((dev-blk (new-device-blk)))
      (setf (Command-command struct)  CMD-CREATE-DEVICE)
      (setf (Command-arg-count struct) 2)
      (setf (Command-args struct) type)
      (setf (Command-args struct 4) dev-blk)
      dev-blk)))


(defun major-k-delete-device (type dev-blk)
  (with-idle-state-secured
    (setf (Command-command struct)  CMD-DELETE-DEVICE)
    (setf (Command-arg-count struct) 2)
    (setf (Command-args struct) type)
    (setf (Command-args struct 4) dev-blk)))



(defun major-k-pagein  ())
(defun major-k-pageout ())





;;;
;;;  Major Mac commands (to the K)
;;;


(defvar CMD-COLD-BOOT)
(defvar CMD-WARM-BOOT)
(defvar CMD-K-HOW-ARE-YOU)


(setq CMD-COLD-BOOT     0
      CMD-WARM-BOOT     1
      CMD-K-HOW-ARE-YOU 2)



(defun major-mac-cold-boot ())
(defun major-mac-warm-boot ())
(defun major-mac-k-how-are-you ())
