(defun ruby-string->symbol ()
  "Change string at point into a symbol"
  (interactive)
  (when (ruby-string-at-point-p)
    (save-excursion
      (sp-beginning-of-sexp)
      (sp-splice-sexp)
      (insert ":"))))
