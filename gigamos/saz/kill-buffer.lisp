
(DEFUN KILL-BUFFER (BUFFER &OPTIONAL NO-SAVE-P)
  "Kill BUFFER; remove it from the list which can be selected.
Offers to save it if it is a modified file buffer, unless NO-SAVE-P."
  ;; If the buffer is associated with a file and contains changes, offer to write it out.
  (AND (NOT NO-SAVE-P)
       (BUFFER-NEEDS-SAVING-P BUFFER)
       (OR (CONSP (BUFFER-FILE-ID BUFFER))
           (NOT (BP-= (INTERVAL-FIRST-BP BUFFER) (INTERVAL-LAST-BP BUFFER))))
       (FQUERY '(:BEEP T
                 :TYPE :READLINE
                 :CHOICES #,FORMAT:YES-OR-NO-P-CHOICES)
               "Buffer ~A has been modified, save it first? "
               (BUFFER-NAME BUFFER))
       (SAVE-BUFFER BUFFER))
  ;; If buffer is current, select something else before killing.
  (DO () ((NOT (EQ BUFFER *INTERVAL*)))
    (MUST-REDISPLAY *WINDOW*
                    (SELECT-BUFFER
                      "Killing the current buffer, select which other buffer?"
                      'MAYBE)))
  ;; Anybody who refers to this buffer should be redirected.
  (SEND BUFFER :KILL)
  T)


(DEFUN KILL-BUFFER (BUFFER &OPTIONAL NO-SAVE-P)
  "Kill BUFFER; remove it from the list which can be selected.
Offers to save it if it is a modified file buffer, unless NO-SAVE-P."
  ;; If the buffer is associated with a file and contains changes, offer to write it out.
  (AND (NOT NO-SAVE-P)
       (BUFFER-NEEDS-SAVING-P BUFFER)
       (OR (CONSP (BUFFER-FILE-ID BUFFER))
           (NOT (BP-= (INTERVAL-FIRST-BP BUFFER) (INTERVAL-LAST-BP BUFFER))))
       (FQUERY '(:BEEP T
                 :TYPE :READLINE
                 :CHOICES #,FORMAT:YES-OR-NO-P-CHOICES)
               "Buffer ~A has been modified, save it first? "
               (BUFFER-NAME BUFFER))
       (SAVE-BUFFER BUFFER))
  ;; If buffer is current, select something else before killing.
  (when (EQ BUFFER *INTERVAL*)
    ;; Cannot select, or be prompted with, the name of the buffer being killed
    (let* ((*zmacs-buffer-name-alist*
            (lisp:remove buffer *zmacs-buffer-name-alist* :key 'cdr))
           (new-buffer
              (SELECT-BUFFER
                "Killing the current buffer, select which other buffer?"
                'MAYBE)))
      (MUST-REDISPLAY *WINDOW* new-buffer)))
  ;; Anybody who refers to this buffer should be redirected.
  (SEND BUFFER :KILL)
  T)
