;;; init.el --- Summary
;;; Commentary:
;;; Code:
(makunbound 'dotc-dir)
(defvar dotc-dir
  (expand-file-name (getenv "DOTC_DIR"))
  "Directory root containing shell configuration.")

(require 'cask (expand-file-name "cask/cask.el" dotc-dir))
(cask-initialize)

(setq site-lisp (concat dotc-dir "/site-lisp/"))
(setq autoload-file (concat site-lisp "loaddefs.el"))
(setq custom-file (concat dotc-dir "/custom.el"))

(add-to-list 'load-path site-lisp)

(setenv "PAGER" (executable-find "cat")) ;; disable pager

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

(require 'cl)

(require 'clgc-elpa)
(regen-autoloads)

(require 'clgc-functions)
(require 'clgc-javascript)
(require 'clgc-lisp)
(require 'clgc-find-files)
(require 'starter-kit-eshell)
(require 'clgc-ruby)
(require 'clgc-major-modes)

(setq gc-cons-threshold (expt 2 24) ;; 16mb instead of 800k
      inhibit-startup-screen t
      line-number-mode t
      column-number-mode t
      auto-fill-default t
      visible-bell t
      require-final-newline t
      debug-on-error nil
      apropos-do-all t
      sentence-end-double-space nil)

;; Clipboard and Selection
(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      mouse-yank-at-point t)

(setq-default indent-tabs-mode nil
              tab-width 2
              c-basic-offset 2
              sgml-basic-offset 4
              nxml-child-indent 2)

(defun clgc-prog-mode-hook ()
  (setq show-trailing-whitespace t
        default-indicate-empty-lines t))

(add-hook 'prog-mode-hook 'clgc-prog-mode-hook)

(fset 'yes-or-no-p 'y-or-n-p)

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(menu-bar-mode -1)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; remove vc peskiness while editing .emacs file
(setq vc-follow-symlinks nil)

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(setq tramp-default-method "ssh")

;; smart pairing for all
(require 'smartparens-config)
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol t)
(sp-use-smartparens-bindings)

(show-smartparens-global-mode +1)
(smartparens-global-mode)

;; Uniquify buffer
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Enable font lock (colours) for all modes that support it:
(require 'font-lock)
(setq-default font-lock-maximum-decoration t)
(global-font-lock-mode t)

;; (require 'pretty-mode-plus)
;; (global-pretty-mode 1)

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(setq delete-by-moving-to-trash t)

(setq compilation-ask-about-save nil)
(setq compilation-read-command t)
(setq compilation-window-height 12)

;; make highlight standard /etc files
(require 'generic)
(require 'generic-x)
(require 'page-ext)

(require 'dired-x)

;; http://repose.cx/conf/.elisp/
;; Or enable more if you wish
(setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
                  global-semanticdb-minor-mode
                  global-semantic-idle-summary-mode
                  global-semantic-mru-bookmark-mode))
(semantic-mode 1)

(require 'yasnippet)
(add-to-list 'yas-snippet-dirs (concat site-lisp "snippets"))
(yas-global-mode)

(global-undo-tree-mode)

(defun clgc-term-mode ()
  (setq yas-dont-activate t))
(add-hook 'term-mode-hook 'clgc-term-mode)
(add-hook 'comint-mode-hook 'clgc-term-mode)

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (concat site-lisp "ac-dict"))
(ac-config-default)

(setq-default ac-sources
              '(ac-source-yasnippet
                ac-source-abbrev
                ac-source-dictionary
                ac-source-words-in-same-mode-buffers))

(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
(add-hook 'after-init-hook #'global-flycheck-mode)

(autoload 'org-tree-slide-mode "org-tree-slide" t)

;; setup audo modes
(dolist (mode '(("\\.C$"          . c++-mode)
                ("\\.cc$"         . c++-mode)
                ("\\.[ch]xx|pp$"  . c++-mode)
                ("\\.hh$"         . c++-mode)
                ;; C Bindings
                ("\\.c$"          . c-mode)
                ("\\.h$"          . c++-mode)

                ;; Ruby Bindings
                ("\\.ruby$"       . ruby-mode)
                ("\\.rabl$"       . ruby-mode)

                ("\\.awk"         . awk-mode)
                ("\.gradle$"      . groovy-mode)
                ("\.gsp$"         . nxml-mode)

                ("\\.css"         . css-mode)
                ("\\.ya?ml$"      . yaml-mode)
                ("\\.sass$"       . sass-mode)
                ("\\.ha?ml$"      . haml-mode)
                ("\\.md$"         . markdown-mode)

                ("\\.js$"         . js2-mode)
                ("Cask"           . emacs-lisp-mode)
                ("gitconfig"      . conf-mode)))
  (add-to-list 'auto-mode-alist mode))

;; FIXME: frequent problem with menubars without this required because
;; of void-variable senator-kill-ring
(require 'semantic/senator)

(require 'clgc-keybindings)
(winner-mode)

(require 'server)
(unless (server-running-p)
  (server-start))

(require 'diminish)
(eval-after-load "auto-complete" '(diminish 'auto-complete-mode " A"))
(eval-after-load "yasnippet" '(diminish 'yas-minor-mode "Y"))
(eval-after-load "eldoc" '(diminish 'eldoc-mode))
(eval-after-load "elisp-slime-nav" '(diminish 'elisp-slime-nav-mode))
(eval-after-load "undo-tree" '(diminish 'undo-tree-mode "U"))
(eval-after-load "projectile" '(diminish 'projectile-mode "P"))

(load custom-file 'noerror)
(load-theme 'zenburn t)

;;; init.el ends here
