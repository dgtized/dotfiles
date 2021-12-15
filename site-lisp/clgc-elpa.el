(defun regen-autoloads (autoload-dir autoload-file)
  "Regenerate the autoload definitions file and load it."
  (interactive "P")
  (let (emacs-lisp-mode-hook)
    (make-directory-autoloads autoload-dir autoload-file))
  (load autoload-file))

(provide 'clgc-elpa)
