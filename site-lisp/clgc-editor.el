;; On startup this is run in after-init-hook by default

(require 'clgc-functions)
(require 'clgc-javascript)
(require 'clgc-lisp)
(require 'clgc-find-files)
;; (require 'starter-kit-eshell)
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
(require 'command-log-mode)

;; Enable font lock (colours) for all modes that support it:
(require 'font-lock)
(setq-default font-lock-maximum-decoration t)
(global-font-lock-mode t)

;; smart pairing for all
(require 'smartparens-config)
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol t)
(sp-use-smartparens-bindings)

(add-to-list 'sp-ignore-modes-list 'org-mode)

(show-smartparens-global-mode +1)
(smartparens-global-strict-mode)

;; Uniquify buffer
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets
      uniquify-after-kill-buffer-p t)

;; Autocorrect
(setq default-abbrev-mode t
      save-abbrevs 'silently
      abbrev-file-name (expand-file-name "abbrev_defs.el" dotc-elisp))

(global-undo-tree-mode)

(defun clgc-term-mode ()
  (setq yas-dont-activate t))
(add-hook 'term-mode-hook 'clgc-term-mode)
(add-hook 'comint-mode-hook 'clgc-term-mode)

;; Company
(global-company-mode)
(setq company-idle-delay 0.5
      company-transformers '(company-sort-by-occurrence))

(require 'yasnippet)
(add-to-list 'yas-snippet-dirs (expand-file-name "snippets" dotc-elisp))
(yas-global-mode t)
(add-hook 'prog-mode-hook #'yas-minor-mode)

;; Add yasnippet support for all company backends
;; https://github.com/syl20bnr/spacemacs/pull/179
(defvar company-mode/enable-yas t "Enable yasnippet for all backends.")

(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend)    (member 'company-yasnippet backend)))
  backend
(append (if (consp backend) backend (list backend))
        '(:with company-yasnippet))))

(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
(global-flycheck-mode)

;; Stop demanding confirmation to go over 50 characters on first line
(remove-hook 'git-commit-finish-query-functions
             'git-commit-check-style-conventions)

(add-hook 'occur-mode-hook 'occur-context-resize-mode)

(require 'avy)
(setq avy-background t
      avy-style 'at-full)

(require 'atomic-chrome)
(atomic-chrome-start-server)

(require 'clgc-keybindings)
(winner-mode)

(load-secrets)

(provide 'clgc-editor)
