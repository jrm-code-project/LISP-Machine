;;; -*- Mode:LISP; Package:USER; Base:10 -*-
;;
;; make a doe-macsyma tape for RELEASE-2 for LPH
;; files ordered for convenient loading. required files first.

(defconst *doe-macsyma-dirs* '(

      DOE-MACSYMA.LISPMENVIR;   2   DIRECTORY!   04/14/86 15:06:18        FileMan
      DOE-MACSYMA.REL2-OBJECTS;  10   DIRECTORY!   04/14/86 16:14:43      FileMan
      DOE-MACSYMA.REL2-PATCHES;   3   DIRECTORY!   04/14/86 16:48:52      FileMan
      DOE-MACSYMA.LPH;    2   DIRECTORY!   04/14/86 15:11:58              FileMan
      DOE-MACSYMA.EXAMPLE;   6   DIRECTORY!   04/14/86 14:37:54           FileMan
      DOE-MACSYMA.DEMO;   4   DIRECTORY!   04/14/86 14:18:46              FileMan

      DOE-MACSYMA.ODE;    5   DIRECTORY!   04/14/86 15:49:30              FileMan
      DOE-MACSYMA.SHARE;   6   DIRECTORY!   04/14/86 16:58:13             FileMan
      DOE-MACSYMA.SHARE1;   6   DIRECTORY!   04/14/86 17:20:30            FileMan
      DOE-MACSYMA.SHARE2;   5   DIRECTORY!   04/14/86 17:44:54            FileMan
      DOE-MACSYMA.SHAREM;   2   DIRECTORY!   04/14/86 18:05:48            FileMan
      DOE-MACSYMA.TENSOR;   4   DIRECTORY!   04/14/86 18:12:27            FileMan




      DOE-MACSYMA.AAREADME;   1   DIRECTORY!   04/14/86 14:16:52          FileMan
      DOE-MACSYMA.CFFK;   1   DIRECTORY!   04/14/86 14:18:25              FileMan
      DOE-MACSYMA.DAS;    1   DIRECTORY!   04/14/86 14:18:37              FileMan
      DOE-MACSYMA.DUMP;   1   DIRECTORY!   04/14/86 14:33:35              FileMan
      DOE-MACSYMA.ELLEN;   1   DIRECTORY!   04/14/86 14:35:22             FileMan
      DOE-MACSYMA.EMAXIM;   1   DIRECTORY!   04/14/86 14:36:05            FileMan
      DOE-MACSYMA.GRAPHICS;   1   DIRECTORY!   04/14/86 15:01:17          FileMan
      DOE-MACSYMA.JIM;    1   DIRECTORY!   04/14/86 15:01:34              FileMan
      DOE-MACSYMA.JM;     1   DIRECTORY!   04/14/86 15:02:05              FileMan
      DOE-MACSYMA.JPG;    1   DIRECTORY!   04/14/86 15:02:59              FileMan
      DOE-MACSYMA.LIBMAX;   1   DIRECTORY!   04/14/86 15:04:33            FileMan

      DOE-MACSYMA.LMMAX;   2   DIRECTORY!   04/14/86 15:08:40             FileMan

      DOE-MACSYMA.MACDOC;   3   DIRECTORY!   04/14/86 15:15:48            FileMan
      DOE-MACSYMA.MACRAK;   1   DIRECTORY!   04/14/86 15:20:56            FileMan
      DOE-MACSYMA.MACSYM;   1   DIRECTORY!   04/14/86 15:21:43            FileMan
      DOE-MACSYMA.MAXDOC;   6   DIRECTORY!   04/14/86 15:22:03            FileMan
      DOE-MACSYMA.MAXII;   1   DIRECTORY!   04/14/86 15:35:15             FileMan
      DOE-MACSYMA.MAXSRC;   2   DIRECTORY!   04/14/86 15:35:36            FileMan
      DOE-MACSYMA.MRG;    1   DIRECTORY!   04/14/86 15:41:35              FileMan
      DOE-MACSYMA.NILENVIR;   2   DIRECTORY!   04/14/86 15:43:20          FileMan
      DOE-MACSYMA.PAULW;   2   DIRECTORY!   04/14/86 16:04:36             FileMan
      DOE-MACSYMA.PLOT;   1   DIRECTORY!   05/19/86 13:41:54              dexter
      DOE-MACSYMA.RAT;    3   DIRECTORY!   04/14/86 16:07:31              FileMan
      DOE-MACSYMA.REH;    1   DIRECTORY!   04/14/86 16:14:25              FileMan
;      DOE-MACSYMA.REL3-OBJECTS;  16   DIRECTORY!   05/19/86 15:21:35      gjc
;      DOE-MACSYMA.REL3-PATCHES;   1   DIRECTORY!   05/19/86 15:22:03      gjc
      DOE-MACSYMA.RZ;     1   DIRECTORY!   04/14/86 16:57:04              FileMan
      DOE-MACSYMA.TRANSL;   1   DIRECTORY!   04/14/86 18:27:23            FileMan
;      DOE-MACSYMA.UNIXENVIR;   1   DIRECTORY!   04/14/86 18:30:36         FileMan
;      DOE-MACSYMA.VMSLINK;   2   DIRECTORY!   04/14/86 18:32:43           FileMan
      DOE-MACSYMA.WGD;    1   DIRECTORY!   04/14/86 18:37:54              FileMan
      ))


(defun make-macsyma-tape ()
  (fs:mt-rewind)
  (dolist (dir *doe-macsyma-dirs*)
    (let ((path (format nil "lm:~A;*.*#>")))
      (format t "~&;~A~%" path)
      (fs:copy-directory path "mt:")))
  (fs:mt-write-eof)
  (fs:mt-write-eof)
  (fs:mt-unload))
