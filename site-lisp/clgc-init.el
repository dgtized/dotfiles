;;; clgc-init.el --- Summary
;;; Commentary:
;;; Code:

;; Set this first to speed up startup 5.5s -> 2.5s
(setq gc-cons-threshold (expt 2 25)) ;; 32mb instead of 800k

;; Enable native compilation if available
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (setq package-native-compile t))

(makunbound 'dotc-dir)
(defvar dotc-dir
  (expand-file-name (getenv "DOTC_DIR"))
  "Directory root containing shell configuration.")

(defvar dotc-elisp (concat dotc-dir "/site-lisp/")
  "Directory root for dotc managed elisp files.")

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; https://emacs.stackexchange.com/questions/61386/package-refresh-hangs
(setq gnutls-algorithm-priority "normal:-vers-tls1.3"
      ;; otherwise x would toggle package install
      package-menu-use-current-if-no-marks nil)

(setq custom-file (expand-file-name "custom.el" dotc-elisp))
(setq eshell-aliases-file (expand-file-name "aliases" dotc-elisp))

(add-to-list 'load-path dotc-elisp)

(setenv "PAGER" (executable-find "cat")) ;; disable pager

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

(require 'cl-lib)

(require 'clgc-elpa)
(regen-autoloads dotc-elisp (expand-file-name "loaddefs.el" dotc-elisp))

(setq inhibit-startup-screen t
      line-number-mode t
      column-number-mode t
      visible-bell t
      require-final-newline t
      debug-on-error nil
      apropos-do-all t
      confirm-nonexistent-file-or-buffer nil
      sentence-end-double-space nil
      split-height-threshold 140
      idle-update-delay 0.25)

;; Clipboard and Selection
(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      mouse-yank-at-point t)

(setq-default indent-tabs-mode nil
              tab-width 2
              c-basic-offset 2
              standard-indent 2
              sgml-basic-offset 2
              nxml-child-indent 2
              smie-indent-basic 2
              web-mode-markup-indent-offset 2
              fill-column 80)

(fset 'yes-or-no-p 'y-or-n-p)
(setq use-short-answers t) ;; see kill-buffer--possibly-save
;; Stop confirming process kill
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))
(menu-bar-mode -1)

(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'list-timers 'disabled nil)

;; remove vc peskiness while editing .emacs file
(setq vc-follow-symlinks nil)

(setq tramp-default-method "ssh"
      tramp-ssh-controlmaster-options t)

;; make the backup gods obey ME! no more ~ sprinkles all over the place
(let ((backup-dir (expand-file-name "backups" user-emacs-directory))
      (undo-dir (expand-file-name "undo" user-emacs-directory)))
  (setq version-control nil
        delete-by-moving-to-trash t
        backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,backup-dir t))
        undo-tree-history-directory-alist `(("." . ,undo-dir))))

(setq initial-scratch-message ";; What are you doing in there?\n\n")

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

                ("\\.clje$"       . clojure-mode)

                ("\\.rkt$"        . racket-mode)

                ("\\.wasm$"       . hexl-mode)

                ("\\.js$"         . js2-mode)
                ("Cask"           . emacs-lisp-mode)
                ("gitconfig"      . conf-mode)))
  (add-to-list 'auto-mode-alist mode))

(require 'server)
(unless (server-running-p)
  (server-start))

(load custom-file 'noerror)

(condition-case nil
    (progn
      (require 'diminish)
      (with-eval-after-load "abbrev" (diminish 'abbrev-mode))
      (with-eval-after-load "eldoc" (diminish 'eldoc-mode))
      (with-eval-after-load "projectile" (diminish 'projectile-mode))
      (with-eval-after-load "company" (diminish 'company-mode))
      (with-eval-after-load "undo-tree" (diminish 'undo-tree-mode))
      (with-eval-after-load "yasnippet" (diminish 'yas-minor-mode "ys"))
      (with-eval-after-load "counsel" (diminish 'counsel-mode "Co")))
  (message "warning: diminish not installed"))

;; theme setup
(setq custom-theme-directory user-emacs-directory) ;; fix error in custom-theme--load-path
(condition-case nil
    (load-theme 'zenburn t)  ;; use disable-theme / load-theme to switch
  (message "warning: theme package not installed"))

;; Experiment with Cascadia from https://github.com/microsoft/cascadia-code/releases
;; sudo apt install fonts-cascadia-code

;; https://idiocy.org/emacs-fonts-and-fontsets.html
;; (list-fontsets t)
;; (list-fonts (font-spec :size 16))

;; (list-fonts (font-spec :font "Cascadia"))
(defun clgc-set-font-size (size)
  "Change font size uniformly & on the fly"
  (interactive "nFont size in points: ")
  (set-face-attribute 'default nil
                      :font "Cascadia Code" ;; "Inconsolata-16"
                      :height (truncate (* size 10)))
  (when (functionp 'set-fontset-font)
    (set-fontset-font "fontset-default" 'symbol "Noto Color Emoji")
    (set-fontset-font "fontset-default" 'symbol "Symbola" nil 'append)
    (set-fontset-font t nil
                      (font-spec :family "DejaVu Sans Mono"
                                 :size (float size))
                      nil 'append)))

;; Just force the font size manually
;; (set-face-attribute 'default nil :height 160)

(defun clgc-after-init-hook ()
  (message "emacs loaded after %s with %d GC passes."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done)

  (let ((before-package-time (current-time)))
    (require 'clgc-editor)
    (toggle-frame-maximized)
    (when (display-graphic-p)
      (clgc-set-font-size
       (pcase (system-name)
         ("reason" 14.0)
         ("nocturnal" 14.0)
         ("anathem" 16.0)
         (_ 16.0))))

    (message "emacs packages loaded after %s with %d GC passes."
             (format "%.2f seconds"
                     (float-time
                      (time-subtract (current-time) before-package-time)))
             gcs-done)))

(add-hook 'after-init-hook #'clgc-after-init-hook)
;;; clgc-init.el ends here
