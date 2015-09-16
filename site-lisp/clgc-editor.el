;; On startup this is run in after-init-hook by default

(require 'clgc-functions)
(require 'clgc-javascript)
(require 'clgc-lisp)
(require 'clgc-find-files)
(require 'starter-kit-eshell)
(require 'clgc-ruby)
(require 'clgc-major-modes)
(require 'clgc-org)
(require 'clgc-shell)
(require 'clgc-sql)

;; make highlight standard /etc files
(require 'generic)
(require 'generic-x)
(require 'page-ext)
;; enable C-x C-j
(require 'dired-x)

;; Enable font lock (colours) for all modes that support it:
(require 'font-lock)
(setq-default font-lock-maximum-decoration t)
(global-font-lock-mode t)

;; (require 'pretty-mode-plus)
;; (global-pretty-mode 1)

;; smart pairing for all
(require 'smartparens-config)
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol t)
(sp-use-smartparens-bindings)

(add-to-list 'sp-ignore-modes-list 'org-mode)

(show-smartparens-global-mode +1)
(smartparens-global-mode)

;; Uniquify buffer
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets
      uniquify-after-kill-buffer-p t)

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

;; Stop demanding confirmation to go over 50 characters on first line
(remove-hook 'git-commit-finish-query-functions
             'git-commit-check-style-conventions)

(add-hook 'occur-mode-hook 'occur-context-resize-mode)

(require 'avy)
(setq avy-background t)

(require 'clgc-keybindings)
(winner-mode)

(load-secrets)

(provide 'clgc-editor)
