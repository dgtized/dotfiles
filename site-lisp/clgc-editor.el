;; Autocorrect
(setq default-abbrev-mode t
      save-abbrevs t
      abbrev-file-name (expand-file-name "abbrev_defs.el" dotc-elisp))

(global-undo-tree-mode)

(defun clgc-term-mode ()
  (setq yas-dont-activate t))
(add-hook 'term-mode-hook 'clgc-term-mode)
(add-hook 'comint-mode-hook 'clgc-term-mode)

;; Company
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0.5)

(require 'yasnippet)
(add-to-list 'yas-snippet-dirs (expand-file-name "snippets" dotc-elisp))
(yas-global-mode t)
(add-hook 'prog-mode-hook #'yas-minor-mode)

(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
(global-flycheck-mode)

(global-magit-file-mode)

(require 'avy)
(setq avy-background t)

(provide 'clgc-editor)
