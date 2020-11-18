;;; -*- Mode:LISP; Base:10; Readtable:ZL -*-

(defun INITIAL-SUBLIST-P (list1 list2)
  ;;does list2 begin with list1's elements, in order?
  (dotimes (i (length list2))
        (if (equal list1
                   (firstn i list2))
            (return t))))

(defun dired-subdirectory-p (dir-spec subdir-spec)
  (let* ((parsed-dir (fs:parse-pathname dir-spec))
         (parsed-subdir (fs:parse-pathname subdir-spec))
         (dirlist (send parsed-dir :raw-directory))
         (subdirlist (send parsed-subdir :raw-directory)))
    (when (listp subdirlist)                    ;otherwise it's a toplevel dir
      (if (not (listp dirlist))
          (setq dirlist
                (list dirlist)))
      (initial-sublist-p dirlist subdirlist))))
