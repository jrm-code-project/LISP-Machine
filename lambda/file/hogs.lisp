;-*-Mode:Lisp; Package:FS; Base:8 -*-
(DEFUN FIND-HOGS (&optional (dir (dc-root-directory)) (prefix "")
                  &aux (blocks 0) (files 0) (blocks-in-sub-directories 0))
  (LOOP FOR file IN (read-directory-files dir)
        DO (cond ((directory? file)
                  (setq blocks-in-sub-directories
                        (+ blocks-in-sub-directories
                           (find-hogs file (string-append (directory-name dir) ".")))))
                 (t
                  (setq blocks (+ blocks (file-npages file))
                        files (1+ files)))))
  (setq blocks (+ (map-npages (directory-map dir)) blocks))
  (FORMAT T "~%~A:~15T~5D pages~30T~3D files~45T~3D blocks in subdirs"
          (string-append prefix (DIRECTORY-NAME dir))
          blocks
          files
          blocks-in-sub-directories)
  (+ blocks-in-sub-directories blocks))
