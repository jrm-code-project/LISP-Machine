;;; -*- Mode:LISP; Package:USER -*-
;; assure system patches through #205 are compiled

(defun system-patch-pathname (arg)
  (fs:make-pathname :host "SYS"
                    :directory "PATCH"
                    :name (format nil "SYSTEM-110-~d" arg)
                    :canonical-type :qfasl
                    :version :newest))


(defun load-or-compile-patches (&optional (start 1))
  (do* ((list patches-to-consider (cdr list))
        (count (car list) (car list))
        system-version
        pathname
        lisp-pathname)
       ((null count))
    (setq system-version (nth-value 1 (si:get-system-version :system))
          pathname (system-patch-pathname count)
          lisp-pathname (send pathname :new-canonical-type :lisp))
    (unless (< count system-version)
      (format t "~&Checking patch version ~D ..." count)
      (when (and (not (probef pathname))
                 (probef lisp-pathname))
        (format t "~&Compiling file: ~A" lisp-pathname)
        (compile-file lisp-pathname))
      (load pathname))))

(defconst patches-to-consider '(74
                                  75
                                  76
                                  77
                                  78
                                  79
                                  80
                                  81
                                  82
                                  83
                                  84
                                  85
                                  86
                                  87
                                  88
                                  89
                                  90
                                  91
                                  92
                                  93
                                  94
                                  95
                                  96
                                  97
                                  98
                                  99
                                  100
                                  101
                                  102
                                  103
                                  104
                                  105
                                  106
                                  107
                                  108
                                  109
                                  110
                                  111
                                  112
                                  113
                                  114
                                  115
                                  116
                                  117
                                  118
                                  119
                                  120
                                  121
                                  122
                                  123
                                  124
                                  125
                                  126
                                  127
                                  128
                                  129
                                  130
                                  131
                                  132
                                  133
                                  134
                                  135
                                  136
                                  137
                                  138
                                  139
                                  140
                                  141
                                  142
                                  143
                                  144
                                  145
                                  146
                                  147
                                  148
                                  149
                                  150
                                  151
                                  152
                                  154
                                  155
                                  156
                                  157
                                  158
                                  159
                                  160
                                  161
                                  162
                                  163
                                  164
                                  165
                                  166
                                  167
                                  168
                                  169
                                  170
                                  171
                                  172
                                  173
                                  174
                                  175
                                  176
                                  178
                                  179
                                  180
                                  181
                                  182
                                  183
                                  184
                                  185
                                  186
                                  187
                                  188
                                  189
                                  190
                                  191
                                  192
                                  193
                                  194
                                  195
                                  196
                                  197
                                  198
                                  199
                                  200
                                  201
                                  202
                                  204
                                  205))
