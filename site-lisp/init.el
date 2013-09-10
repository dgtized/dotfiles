
(defconst dotc-dir
  (expand-file-name (getenv "DOTC_DIR"))
  "shell config directory")
(defconst dotc-name
  (expand-file-name (getenv "DOTC_NAME"))
  "shell config name")

(setq site-lisp (concat dotc-dir "/site-lisp/"))
(setq autoload-file (concat site-lisp "loaddefs.el"))
(setq package-user-dir (concat site-lisp "elpa/"))
(setq custom-file (concat dotc-dir "/custom.el"))

(add-to-list 'load-path site-lisp)
(dolist (path '("vendor/" "vendor/groovy" "vendor/js2-mode"
                "vendor/javert"
                "malabar-1.5-SNAPSHOT/lisp"
                "emacs-eclim" "emacs-eclim/vendor"))
  (add-to-list 'load-path (concat site-lisp path)))

(setenv "PAGER" "/bin/cat") ;; disable pager

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

(setq inhibit-startup-screen t
      line-number-mode t
      column-number-mode t
      auto-fill-default t
      c-basic-offset 4
      sgml-basic-offset 4
      nxml-child-indent 2
      visible-bell t
      require-final-newline t
      debug-on-error t
      sentence-end-double-space nil)

;; Clipboard and Selection
(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      mouse-yank-at-point t)

(setq-default indent-tabs-mode nil
              tab-width 4
              show-trailing-whitespace t
              default-indicate-empty-lines t)
(fset 'yes-or-no-p 'y-or-n-p)

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; remove vc peskiness while editing .emacs file
(setq vc-follow-symlinks nil)

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(setq tramp-default-method "ssh")

;Enable opposite bracket/paranthesis highlighting
(require 'paren)
(show-paren-mode t)
(setq blink-matching-paren nil)

;; Uniquify buffer
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Enable font lock (colours) for all modes that support it:
(require 'font-lock)
(setq-default font-lock-maximum-decoration t)
(global-font-lock-mode t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(setq delete-by-moving-to-trash t)

(setq compilation-ask-about-save nil)
(setq compilation-read-command t)
(setq compilation-window-height 12)

;;; make Groovy mode electric by default.
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))
(add-hook 'groovy-mode-hook
          '(lambda ()
             (setq c-basic-offset 4)
             (require 'groovy-electric)
             (groovy-electric-mode)))

;; Perl Stuff (for the horrible times when I can't use ruby)
(defalias 'perl-mode 'cperl-mode)
(defun my-cperl-mode-hook ()
  (setq cperl-hairy t)
  ;(setq cperl-auto-newline t)
  (setq cperl-electric-keywords nil)
  ;(define-key cperl-mode-map "\C-cp" 'cperl-perldoc)
  ;(make-variable-buffer-local 'compile-command)
  ;(setq compile-command (concat "perl -w " (buffer-file-name) " "))
 )

(add-hook 'cperl-mode-hook 'my-cperl-mode-hook)

;;
;; set c/c++ indent width and compile modes
;;
(defun my-c-mode-hook ()
  (c-set-style "K&R")
  (local-set-key "\C-cc" 'compile)
)
(add-hook 'c-mode-hook 'my-c-mode-hook)

(defun my-text-mode-hook ()
  (flyspell-mode)
  (auto-fill-mode))
(add-hook 'text-mode 'my-text-mode-hook)

;; isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
           (regexp-quote isearch-string))))))


(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
                         (if (equal 'fullboth current-value)
                             (if (boundp 'old-fullscreen) old-fullscreen nil)
                           (progn (setq old-fullscreen current-value)
                                  'fullboth)))))

;; make highlight standard /etc files
(require 'generic)
(require 'generic-x)
(require 'page-ext)

(add-to-list 'magic-mode-alist '("^>\\|ID\\|LOCUS\\|DNA" . dna-mode))
(add-to-list
 'auto-mode-alist
 '("\\.\\(fasta\\|fa\\|exp\\|ace\\|gb\\)\\'" . dna-mode))
(add-hook 'dna-mode-hook 'turn-on-font-lock)

(defun auctex nil
  (interactive)
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/auctex/")
  (load "auctex.el" nil t t)
  (load "preview-latex.el" nil t t)
  (setq-default TeX-master nil)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t))

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

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (concat site-lisp "ac-dict"))
(ac-config-default)

(setq-default ac-sources
              '(ac-source-yasnippet
                ac-source-abbrev
                ac-source-dictionary
                ac-source-words-in-same-mode-buffers))

(add-hook 'after-init-hook
      (lambda nil
        (message "clgc-after-init-hook")
        (server-start)
        ;; (normal-erase-is-backspace-mode)
        ))

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
                ("\\.rb$"         . ruby-mode)
                ("\\.ruby$"       . ruby-mode)
                ("\\.rake$"       . ruby-mode)
                ("[Rr]akefile$"   . ruby-mode)
                ("\\.gem$"        . ruby-mode)
                ("\\.gemspec$"    . ruby-mode)
                ("\\Gemfile$"     . ruby-mode)
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
                ("\\.cljs$"       . clojure-mode)))
  (add-to-list 'auto-mode-alist mode))

;; FIXME: frequent problem with menubars without this required because
;; of void-variable senator-kill-ring
(require 'semantic/senator)

(require 'clgc-keybindings)

(load custom-file 'noerror)
(load-theme 'zenburn t)
