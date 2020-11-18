; -*- Mode:LISP; Package:STEVE; Readtable:CL; Base:10 -*-
;
;C-X M
;
;;; modified by an indignant user, 1/4/84
(defconstant send-mail-text-delimiter "--Text follows this line--")

(defconstant send-mail-header
             (list "To: " send-mail-text-delimiter ""))

;;; Invoke with a numeric argument to continue or re-edit.
(editor-bind-key '(#\control-x #\M) send-mail)

(defun send-mail (&optional (continue (argument?)))
  (unless (si:trnlog "SYS$MAIL") (ed-lose "This machine has no mail daemon?"))
  (let ((buffer) (point))
    (when (null continue)
      (setq buffer (buffer "*Mail*" :create nil))
      (unless (null buffer)
        (%kill-buffer-primitive buffer)))
    (unwind-protect
     (progn
      (setq buffer (buffer "*Mail*" :create t))
      (unless continue
        (loop for line first (buffer-content buffer)
              then (make-line buffer line nil (copy-seq string))
              for string in send-mail-header)
        (setf (buffer-content buffer) (line-next (buffer-content buffer))))
      (setq point (point buffer))
      (unless continue
        (send point :set-pos (line-char-count (bp-line point))))
      (unless (null (recursive-editor "Send Mail" :point point))
        (parse-send-mail-buffer buffer)
        (output-buffer-to-file (pathname "sys$mail:foo.new")
                               buffer
                               t)))
     (setf (buffer-modified? buffer) nil)))
  nil)


(defun parse-send-mail-buffer (buffer)
  (loop for line first (buffer-content buffer) then (line-next line)
        if (null line)
        do (ed-lose "The mail buffer is mal-formed")
        until (line-match line send-mail-text-delimiter)
        if (line-match line "To:")
        nconc (extract-recipients-from-line line 3)
        into recip-list
        if (line-match line "CC:")
        nconc (extract-recipients-from-line line 3)
        into recip-list
        finally (setf (line-char-count line) 0) ;Clear the delimiter line
        finally (setf (buffer-content buffer)
                      (make-line buffer nil (buffer-content buffer)
                                 (string-append
                                  (if (loop for l first line
                                            then (line-previous l)
                                            until (null l)
                                            thereis (line-match l "From:"))
                                      "Sent-By: "
                                      "From: ")
                                  (status uname)
                                  " <" (status userid) " @ "
                                  (status site) ">")))
        finally (setf (buffer-content buffer)
                      (make-line
                        buffer nil (buffer-content buffer)
                        (string-append
                          "Date: " (time:print-current-mail-format-date nil))))
        finally (loop for name in (cons "" recip-list)
                      do (setf (buffer-content buffer)
                               (make-line
                                buffer nil (buffer-content buffer) name)))))

(defun extract-recipients-from-line (line skip)
  (loop with i = skip
        with name = nil
        while (<& i (line-char-count line))
        do (multiple-value (name i) (extract-one-recipient line i))
        if name collect name))

(defun extract-one-recipient (line i)
  (loop with limit = (line-char-count line)
        with string = (line-chars line)
        with start = nil
        for j upfrom i
        if (and (not (null start))
                (or (=& j limit)
                    (memq (char string j) '(#\, #\space #\tab))))
        return (values
                (substring string start j) j)

        if (and (null start)
                (not (memq (char string j) '(#\, #\space #\tab))))
        do (setq start j)))

(defun line-match (line string)
  (let ((length (string-length string)))
    (and (>=& (line-char-count line) length)
         (string-equal (line-chars line) string 0 0 length))))
