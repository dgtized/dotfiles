(require 'package)

(defvar clgc-packages
  (list 'haml-mode 'yaml-mode 'js2-mode 'sass-mode 'css-mode))

(defun clgc-elpa-install ()
  "Install all starter-kit packages that aren't installed."
  (interactive)
  (dolist (package clgc-packages)
    (unless (or (member package package-activated-list)
                (functionp package))
      (message "Installing %s" (symbol-name package))
      (package-install package))))

(defun regen-autoloads (&optional force-regen)
  "Regenerate the autoload definitions file if necessary and load it."
  (interactive "P")
  (let ((autoload-dir site-lisp)
        (generated-autoload-file autoload-file))
    (when (or force-regen
              (not (file-exists-p autoload-file))
              (some (lambda (f) (file-newer-than-file-p f autoload-file))
                    (directory-files autoload-dir t "\\.el$")))
      (message "Updating autoloads...")
      (let (emacs-lisp-mode-hook)
        (update-directory-autoloads autoload-dir))))
  (load autoload-file))

(provide 'clgc-elpa)
