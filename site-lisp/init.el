;;; init.el --- Summary
;;; Commentary:
;;; Code:
(makunbound 'dotc-dir)
(defvar dotc-dir
  (expand-file-name (getenv "DOTC_DIR"))
  "Directory root containing shell configuration.")

(defvar dotc-elisp (concat dotc-dir "/site-lisp/")
  "Directory root for dotc managed elisp files.")

(require 'cask (expand-file-name "cask/cask.el" dotc-dir))
(cask-initialize)

(setq autoload-file (expand-file-name "loaddefs.el" dotc-elisp))
(setq custom-file (expand-file-name "custom.el" dotc-elisp))
(setq eshell-aliases-file (expand-file-name "aliases" dotc-elisp))

(add-to-list 'load-path dotc-elisp)

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
(require 'clgc-org)
(require 'clgc-shell)

(setq gc-cons-threshold (expt 2 24) ;; 16mb instead of 800k
      inhibit-startup-screen t
      line-number-mode t
      column-number-mode t
      visible-bell t
      require-final-newline t
      debug-on-error nil
      apropos-do-all t
      confirm-nonexistent-file-or-buffer nil
      sentence-end-double-space nil)

;; Clipboard and Selection
(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      mouse-yank-at-point t)

(setq-default indent-tabs-mode nil
              tab-width 2
              c-basic-offset 2
              sgml-basic-offset 2
              nxml-child-indent 2
              fill-column 80)

(fset 'yes-or-no-p 'y-or-n-p)
;; Stop confirming process kill
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
         kill-buffer-query-functions))

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))
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

(add-to-list 'sp-ignore-modes-list 'org-mode)

(show-smartparens-global-mode +1)
(smartparens-global-mode)

;; Uniquify buffer
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets
      uniquify-after-kill-buffer-p t)

;; make the backup gods obey ME! no more ~ sprinkles all over the place
(let ((backup-dir (expand-file-name "backups" user-emacs-directory)))
  (setq version-control nil
        delete-by-moving-to-trash t
        backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,backup-dir t))))

;; Enable font lock (colours) for all modes that support it:
(require 'font-lock)
(setq-default font-lock-maximum-decoration t)
(global-font-lock-mode t)

;; (require 'pretty-mode-plus)
;; (global-pretty-mode 1)

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(setq compilation-ask-about-save nil
      compilation-read-command t
      compilation-window-height 12)

;; make highlight standard /etc files
(require 'generic)
(require 'generic-x)
(require 'page-ext)
;; enable C-x C-j
(require 'dired-x)

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
(add-hook 'after-init-hook 'yas-global-mode t)

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
                ("\\Berksfile$"   . ruby-mode)

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

(require 'clgc-keybindings)
(winner-mode)

(require 'server)
(unless (server-running-p)
  (server-start))

(require 'diminish)
(eval-after-load "abbrev" '(diminish 'abbrev-mode))
(eval-after-load "eldoc" '(diminish 'eldoc-mode))
(eval-after-load "elisp-slime-nav" '(diminish 'elisp-slime-nav-mode))
(eval-after-load "magit" '(diminish 'magit-auto-revert-mode))
(eval-after-load "projectile" '(diminish 'projectile-mode))

(eval-after-load "company" '(diminish 'company-mode))
(eval-after-load "undo-tree" '(diminish 'undo-tree-mode))
(eval-after-load "yasnippet" '(diminish 'yas-minor-mode "Υ"))

(load custom-file 'noerror)
(load-theme 'zenburn t)

(if (eq window-system 'x)
    (set-face-attribute 'default nil
                    :family "Inconsolata"
                    :height 110
                    :weight 'normal
                    :width 'normal))

(when (functionp 'set-fontset-font)
  (set-fontset-font "fontset-default"
                    'unicode
                    (font-spec :family "DejaVu Sans Mono"
                               :width 'normal
                               :size 10.0
                               :weight 'normal)))

;;; init.el ends here
