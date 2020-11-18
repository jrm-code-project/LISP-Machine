;;;-*- Mode:LISP; Package:LAMBDA; Base:16 -*-

(defconst mode-reg-offset #xfff7fc)
(defconst config-prom-offset #xfff800)
(defconst 4meg-slot #xfa)
(defconst 4meg-addr (ash 4meg-slot 24.))
(defconst mem-size #x1000)
(defconst mem-error-count 0)
(defconst mem-start 0)
(defconst 4meg-v2 nil)

;***********************************************************************************************
;;;; this routine will run diagnostics on the specified memory board - be sure it is disabled!!

(defun test-memory-board (slot)
  (setq 4meg-slot (logior #xf0 (logand #x0f slot)))
  (4test))
;***********************************************************************************************

(defsubst 4rm ()
  (%nubus-read 4meg-slot mode-reg-offset))



(defsubst 4wm (data)
  (%nubus-write 4meg-slot mode-reg-offset data))



(defsubst 4r (addr)
  (%nubus-read 4meg-slot addr))



(defsubst 4w (addr data)
  (%nubus-write 4meg-slot addr data))


(defsubst 4rb (addr)
  (sys:%nubus-read-8 4meg-slot addr))


(defsubst 4wb (addr data)
  (sys:%nubus-write-8 4meg-slot addr data))

(defun 4rloop ()
  (do-forever
    (4r mem-start)))


(defun 4rrloop ()
  (do-forever
    (4wm 0)
    (dotimes (i mem-size)
      (4r (ash i 2)))
    (mem-final-check)))

(defun 4wloop ()
  (do-forever
    (4w mem-start #Xffffffff)))


(defun 4wbloop ()
  (do-forever
    (4wb 3 #xff)))



(defun 4atest (&aux addr data temp)
  (4wm 0)

  (format t "Memory address test.~%")
  (setq addr mem-start data mem-start)
  (dotimes (i mem-size)
    (4w addr data)
    (setq addr (+ addr 4) data (+ data 4)))

  (setq addr mem-start data mem-start)
  (dotimes (i mem-size)
    (and
      ( (setq temp (4r addr)) data)
      (mem-error "address" addr data temp))
    (setq addr (+ addr 4) data (+ data 4)))

  (format t "Inverted memory address test.~%")
  (setq addr mem-start data (logxor #xffffffff mem-start))
  (dotimes (i mem-size)
    (4w addr data)
    (setq addr (+ addr 4) data (- data 4)))

  (setq addr mem-start data (logxor #xffffffff mem-start))
  (dotimes (i mem-size)
    (and
      ( (setq temp (4r addr)) data)
      (mem-error "inverted address" addr data temp))
    (setq addr (+ addr 4) data (- data 4)))
  (mem-final-check))


(defun 4aatest (&aux temp mask addr)
  (4wm 0)
  (dotimes (bank 16.)
    (format t "Ram address bit test on bank ~X~%" bank)
    (setq addr (ash bank 20.))
    (dotimes (i #x3c000)
      (4w addr 0)
      (setq addr (+ addr 4)))
    (dotimes (abit 18.)
      (setq addr (logior (ash bank 20.) (ash 1 (+ abit 2))))
      (4w addr -1)
      (dotimes (mbit 18.)
        (setq mask (ash 1 (+ mbit 2)))
        (when (not (equal 0 (setq temp (4r (logxor addr mask)))))
          (mem-error "ram address-1" (logxor addr mask) 0 temp))
        (when (and
                (neq mbit abit)
                (not (equal 0 (setq temp (4r (logxor addr mask (ash 1 (+ abit 2))))))))
          (mem-error "ram address-2" (logxor addr mask (ash 1 (+ abit 2))) 0 temp)))
      (4w addr 0)
      (mem-final-check))))

(defun 4a2test (&aux temp size)
  (setq size mem-size)
  (4wm 0)
  (format t "Memory address test 2~%")
  (dotimes (i size)
    (4w (ash i 2) -1))
  (dotimes (i 20.)
    (format t "     Testing address bit ~D~%" (+ i 2))
    (4w (ash 1 (+ i 2)) 0)
    (dotimes (j size)
      (if ( (setq temp (4r (ash j 2))) #xffffffff)
          (progn
            (and ( (* j 4) (ash 1 (+ i 2)))
                 (mem-error "address 2" (ash j 2) #xffffffff temp)))))
    (4w (ash 1 (+ i 2)) -1)))



(defun 4dtest (&aux addr data datalist temp)
  (setq datalist (copylist '(#x01010101 #x02020202 #x04040404 #x08080808
                   #x10101010 #x20202020 #x40404040 #x80808080)))
  (rplacd (last datalist) datalist)

  (format t "Memory rotating data bit test.~%")
  (dotimes (j 8.)
    (4wm 0)
    (format t "      Pass ~A of 8.~%" (1+ j))
    (setq addr mem-start data (nthcdr j datalist))
    (dotimes (i mem-size)
      (4w addr (car data))
      (setq addr (+ addr 4) data (cdr data)))

    (setq addr mem-start data (nthcdr j datalist))
    (dotimes (i mem-size)
      (cond (( (setq temp (4r addr)) (car data))
             (mem-error "rotating bit" addr (car data) temp)))
      (setq addr (+ addr 4) data (cdr data)))
    (mem-final-check)))


(defun 4d2test (&aux addr temp)
  (format t "Check for shorted data bits test~%")
  (4wm 0)
  (setq mem-start 0)
  (dotimes (bank 16.)
    (setq addr (* bank #x100000))
    (dotimes (bit 32.)
      (4w addr (ash 1 bit))
      (when (not (equal (ash 1 bit) (setq temp (4r addr))))
        (mem-error "data bit short" addr (ash 1 bit) temp)))))


(defun 4btest (&aux addr temp shift)
  (format t "Byte access test.~%")
  (setq mem-start 0)
  (4wm 0)
  (dotimes (i 4)
    (format t "     Writing byte ~A~%" i)
    (setq addr (+ i mem-start))
    (dotimes (j mem-size)
      (4wb addr (logand #x00ff (+ i j)))
      (setq addr (+ addr 4))))
  (dotimes (i 4)
    (format t "     Reading byte ~A~%" i)
    (setq addr (+ i mem-start) shift (byte 8 (* i 8)))
    (dotimes (j mem-size)
      (setq temp (logand #xff (ldb shift (4r addr))))
      (cond
        ((  (logand #xff (+ i j)) temp)
         (print temp)
         (mem-error "byte access" addr (logand #xff (+ i j)) temp)))
      (setq addr (+ addr 4))))
  (mem-final-check))


(defun 4brtest (&aux addr temp shift)
  (4wm 0)
  (dotimes (i 4)
    (format t "     Reading byte ~A~%" i)
    (setq addr (+ i mem-start) shift (byte 8 (* i 8)))
    (dotimes (j mem-size)
      (setq temp (ldb shift (4r addr)))
      (cond
        ((  (logand #xff (+ i j)) temp)
         (mem-error "byte access" addr (logand #xff (+ i j)) temp)))
      (setq addr (+ addr 4)))))



(defun mem-error (test addr expect got)
  (format t "Error in ~A test at address ~X (bank ~X) expected ~X got ~X~%"
          test addr (bank addr) expect got)
  (setq mem-error-count (1+ mem-error-count))
  )


(defun pcheck ()
  (cond ((perror-p)
         (setq mem-error-count (1+ mem-error-count))
         (format t "Parity error occured somtime during this test.~%")
         (tyi))))



(defun 4iotest (&aux temp)
  (format t "Mode register test.~%")
  (setq mem-start 0)
  (4wm 0)
  (setq temp (logand #x1f (4rm)))
  (and
    (neq temp 0)
    (mem-error "mode register reset" mode-reg-offset 0 temp))

  (loop for bit in '(2 3 4)
        do (progn
             (4w mode-reg-offset (ash 1 bit))
             (setq temp (logand #x9f (4r mode-reg-offset)))
             (and
               (neq (ash 1 bit) temp)
               (mem-error "mode register bit" mode-reg-offset (ash 1 bit) temp))))

  (4wm 0)

  (format t "Config prom test.~%")
  (setq temp (4rprom 32.))
  (cond
    ((and
       (string= (substring temp 0 26.) "LMI 4-MEGABYTE MEMORY V1.0")
       (eq (logand (4rm) #x20) #x20))
     (setq 4meg-v2 nil)
     (setq mem-size #x100000)
     4.)
    ((and
       (string= (substring temp 0 27.) "LMI 16-MEGABYTE MEMORY V1.0")
       (eq (logand (4rm) #x20) 0))
     (setq 4meg-v2 nil)
     (setq mem-size #x3ffc00)
     16.)
    ((and
       (string= (substring temp 0 26.) "LMI 4-MEGABYTE MEMORY V2.0")
       (eq (logand (4rm) #x60) #x00))
     (setq 4meg-v2 t)
     (setq mem-size #x100000)
     4.)
    ((and
       (string= (substring temp 0 26.) "LMI 8-MEGABYTE MEMORY V2.0")
       (eq (logand (4rm) #x60) #x20))
     (setq 4meg-v2 t)
     (setq mem-size #x200000)
     8.)
    ((and
       (string= (substring temp 0 27.) "LMI 12-MEGABYTE MEMORY V2.0")
       (eq (logand (4rm) #x60) #x40))
     (setq 4meg-v2 t)
     (setq mem-size #x300000)
     12.)
    ((and
       (string= (substring temp 0 27.) "LMI 16-MEGABYTE MEMORY V2.0")
       (eq (logand (4rm) #x60) #x60))
     (setq 4meg-v2 t)
     (setq mem-size #x3ffc00)
     16.)
    (t
     (format t "Config prom failure - ~S~%" temp)
     nil)))

(defun 4rprom (n &aux temp)
  (setq temp "")
  (dotimes (i n)
    (setq temp (string-append temp (ascii (logand #xff (4r (+ config-prom-offset (* i 4))))))))
  temp)

(defun perror-p ()
  (equal 4 (logand 4 (4rm))))

(defun perror-info (&aux mr)
  (setq mr (4rm))
  (format t "    Parity error register info -- Bank ~X -  Byte(s) " (logand #x0f (ash mr -8.)))
  (dotimes (i 4)
    (when (eq 1 (logand 1 (ash mr (- 0 (+ 12. i)))))
      (format t " ~X" i)))
  (format t "~%"))


(defun 4ptest ()
  (format t "Parity logic test.~%")
  (4wm 0)
  (4w mem-start 0)
  (4r mem-start)
  (and (perror-p) (format t "Unable to write good parity to memory loc 0.~%"))

  (dotimes (j 4.)
    (dotimes (i 256.)
      (4wm 0)
      (4w mem-start 0)
      (4wm 16.)
      (4wb (+ j mem-start) i)
      (4rb (+ j mem-start))
      (when (not (perror-p))
        (format t "Parity logic error at byte ~X with data pattern ~X~%" (+ j mem-start) i))

      (4wm 0)
      (4wb (+ j mem-start) i)
      (4rb (+ j mem-start))
      (when (perror-p)
        (format t "Parity logic error at byte ~X with data pattern ~X~%" (+ j mem-start) i))))
  (when 4meg-v2
    (dotimes (bank (* 4 (ash (+ mem-size #x400) -20.)))
      (setq mem-start (* bank #x100000))
      (dotimes (i 4)
        (4wm 0)
        (4w mem-start 0)
        (4wm 16.)
        (4wb (+ mem-start i) 0)
        (4r mem-start)
        (when (neq bank (ldb (byte 4. 8.) (4rm)))
          (mem-error "Parity register bank" mode-reg-offset bank (ldb (byte 4. 8.) (4rm))))
        (when (neq (ldb (byte 4. 12.) (4rm)) (ash 1 i))
          (mem-error "Parity register byte"
                     mode-reg-offset (ash 1 i) (ldb (byte 4. 12.) (4rm))))))))

(defun 4rtest (&aux addr temp)
  (format t "Refresh test.~%")
  (4wm 0)
  (setq addr mem-start)
  (dotimes (i mem-size)
    (4w addr 0)
    (setq addr (+ addr 4)))

  (sleep 15.)

  (setq addr mem-start)
  (dotimes (i mem-size)
    (setq temp (4r addr))
    (and
      (not (equal temp 0))
      (mem-error "refresh" addr 0 temp))
    (setq addr (+ addr 4)))
  (mem-final-check))



(defun mem-final-check ()
  (when (perror-p)
    (format t "Warning - parity error occured during test!!!~%")
    (perror-info)))


(defun 4test ()
  (format t "~%Starting 4//8//12//16 Meg memory diagnostics.~%")
  (4wm 0)
  (setq mem-error-count 0 mem-start 0)
  (when (4iotest)
     (4atest)
     (4dtest)
     (4ptest)
     (4btest)
     (4rtest))
  (format t "4//16 Meg memory diagnostics complete.~%"))


(defun 4xtest (&aux addr temp x)
  (format t "~%Bashing the bits ....~%")
  (do-forever
    (setq addr mem-start x (ash mem-start 4))
    (dotimes (i mem-size)
      (4wm 0)
      (4w addr x)
      (cond (( (setq temp (4r addr)) x)
             (4wm 4)
             (mem-error "XTEST" addr x temp)))
      (setq addr (+ 4 addr) x (+ x 64.)))))


(defun 4ztest (&aux addr)
  (4wm 0)
  (setq addr mem-start)
  (dotimes (i mem-size)
    (4w addr -1)
    (setq addr (+ addr 4)))
  (4w mem-start 0)
  (do
    while ( (4r mem-start) 0))
  (4wm 4))


(defun 4dptest (&aux addr datalist dataloop temp)
  (setq dataloop (copylist '(#x01010101 #x03030303 #x07070707 #x0f0f0f0f
                             #x1f1f1f1f #x3f3f3f3f #x7f7f7f7f #xffffffff
                             #xfefefefe #xfcfcfcfc #xf8f8f8f8 #xf0f0f0f0
                             #xe0e0e0e0 #xc0c0c0c0 #x80808080 #x55555555 #xaaaaaaaa)))
  (rplacd (last dataloop) dataloop)

  (format t "Memory immediate rotating data bit test with parity check.~%")
  (dotimes (j 16.)
    (4wm 0)
    (format t "      Pass ~D. of 16.~%" (1+ j))
    (setq addr mem-start datalist (nthcdr j dataloop))
    (dotimes (i mem-size)
      (4w addr (car datalist))
      (setq addr (+ addr 4) datalist (cdr datalist)))
    (setq addr mem-start datalist (nthcdr j dataloop))
    (dotimes (i mem-size)
      (cond (( (setq temp (4r addr)) (car datalist))
             (mem-error "rotating bit" addr (car datalist) temp))
            ((eq (logand (4rm) 4) 4)
             (mem-error "rotating bit parity" addr (car datalist) temp)
             (dotimes (j 4)
               (4wm 0)
               (4rb (+ addr j))
               (and
                 (eq (logand (4rm) 4) 4)
                 (format t "     Parity error is in byte ~d.~%" j)))
             (4wm 0))))
      (setq addr (+ addr 4) datalist (cdr datalist))))


(defun 4qtest (&optional (first-bank 0) &aux max inc megs)
  (format t "~%Starting quick 4//16 Meg memory diagnostics.~%")
  (4wm 0)
  (setq megs (4iotest))
  (cond
    (4meg-v2
     (setq inc #x100000 max megs))
    (t
     (setq inc #x40000 max 16.)))
  (setq mem-error-count 0 mem-start 0 mem-size #x1000)
  (setq max (- max first-bank))
  (dotimes (i max)
     (setq mem-start (* (+ i first-bank) inc))
     (format t "~%***************** Testing RAM bank ~A **************************~%"
             (+ i first-bank))
     (4atest)
     (4dtest)
     (4ptest)))



(defun 4scan-OLD (&aux addr)
  (4wm 0)
  (setq mem-size #x3ff000)
  (setq addr mem-start)
  (dotimes (i mem-size)
    (4r addr)
    (cond ((eq (logand (4rm) 4) 4)
           (format t "Memory parity error at location ~X which is bank ~X~%" addr (bank addr))
           (dotimes (j 4)
             (4wm 0)
             (4wb (+ addr j) (4rb (+ addr j)))
             (4wm 0)
             (4r (+ addr j))
             (when (eq (logand (4rm) 4) 0)
               (format t "     Parity error is in byte ~d.~%" j)
               (return j)))
           (4wm 0)))
    (setq addr (+ addr 4))))


(defun bank (addr)
  (cond
    (4meg-v2
     (logand #x0f (ash addr -20.)))
    (t
     (logand #x0f (ash addr -18.)))))

(DEFUN 4SCAN (&AUX ADDR)
  (when (equal (logand (4rm) 4) 4)
    (format t "Initial parity error info in reg:")
    (perror-info)
    (format t "~%"))
  (4IOTEST)
  (format t "Starting scan ------~%")
  (4WM 0)
  (SETQ ADDR MEM-START)
  (DOTIMES (I MEM-SIZE)
    (4R ADDR)
    (WHEN (EQUAL (LOGAND (4RM) 4) 4)
      (FORMAT T "Memory parity error at location ~X~%" addr)
      (perror-info)
      (4wm 0))
    (setq addr (+ addr 4))))
