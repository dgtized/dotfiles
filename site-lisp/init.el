 
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
(add-to-list 'load-path (concat site-lisp "vendor/"))
(add-to-list 'load-path (concat site-lisp "vendor/groovy"))
(add-to-list 'load-path (concat site-lisp "vendor/js2-mode"))
(add-to-list 'load-path (concat site-lisp "malabar-1.5-SNAPSHOT/lisp"))
(add-to-list 'load-path (concat site-lisp "emacs-eclim"))
(add-to-list 'load-path (concat site-lisp "emacs-eclim/vendor"))

(setenv "PAGER" "/bin/cat") ;; disable pager

(require 'cl)

(require 'clgc-elpa)
(regen-autoloads)
(require 'clgc-functions)
(require 'clgc-javascript)
(require 'clgc-lisp)
(require 'clgc-find-files)

(setq inhibit-startup-screen t
      line-number-mode t
      column-number-mode t
      auto-fill-default t
      c-basic-offset 4
      sgml-basic-offset 4
      nxml-child-indent 4
      visible-bell t
      require-final-newline t
      debug-on-error t)

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

;; Enable font lock (colours) for all modes that support it:
(require 'font-lock)
(setq-default font-lock-maximum-decoration t)
(global-font-lock-mode t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(require 'mode-compile)
(autoload 'mode-compile "mode-compile"
  "Command to compile current buffer file based on the major mode" t)
(autoload 'mode-compile-kill "mode-compile"
  "Command to kill a compilation launched by `mode-compile'" t)
(global-set-key "\C-cc" 'mode-compile)
(global-set-key "\C-ck" 'mode-compile-kill)
(setq emacs-lisp-sources-regexp "\\.el$\\|\\.emacs$")
;(global-set-key [f5] 'smart-compile)


;(require 'compile)
;(require 'smart-compile)
(setq compilation-ask-about-save nil)
(setq compilation-read-command t)
(setq compilation-window-height 12)

;;;
;;; Scheme Mode
;;;
(autoload 'scheme-mode "quack" "Quack scheme editing mode" t)
(autoload 'run-scheme "quack" "Quack scheme editing mode" t)

;;;
;;; css mode
;;;
(setq css-indent-level 4)

;;;
;;; ruby mode
;;;
(defun ruby-eval-buffer () (interactive)
   "Evaluate the buffer with ruby."
   (shell-command-on-region (point-min) (point-max) "ruby -w "))

(defun my-ruby-mode-hook ()
  ;; (make-variable-buffer-local 'compilation-error-regexp-alist)
  ;; (add-to-list 'compilation-error-regexp-alist
  ;;       '("test[a-zA-Z0-9_]*([A-Z][a-zA-Z0-9_]*) \\[\\(.*\\):\\([0-9]+\\)\\]:" 1 2))
  ;; (add-to-list 'compilation-error-regexp-alist
  ;;       '("\\(.*?\\)\\([0-9A-Za-z_./\:-]+\\.rb\\):\\([0-9]+\\)" 2 3))
  ;;(make-variable-buffer-local 'compile-command)
  ;;(setq compile-command (concat "ruby -w " (buffer-file-name) " "))
  (local-set-key "\C-cr" 'ruby-eval-buffer)

  (c-add-style
   "ruby"
   '("bsd"
     (c-basic-offset . 4)
     (c-offsets-alist
      (case-label . 2)
      (label . 2)
      (statement-case-intro . 2)
      )))
  ;; from: http://shylock.uw.hu/Emacs/ruby-electric.el
  ;(require 'ruby-electric)
  ;(ruby-electric-mode)
  ;(abbrev-mode 1)
  (define-key ruby-mode-map "\C-m" 'reindent-then-newline-and-indent)
  (define-key ruby-mode-map "\C-j" 'newline)
  (require 'nxml-mode)
  ;(require 'rhtml-mode)
  ;(require 'rails)
)

(add-hook 'ruby-mode-hook 'my-ruby-mode-hook)

(autoload 'ruby-mode "ruby-mode" nil t)
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode) t)
(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby" "Set local key defs for inf-ruby in ruby-mode")

;;; use groovy-mode when file ends in .groovy or has #!/bin/groovy at start
(autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'auto-mode-alist '("\.gradle$" . groovy-mode))
(add-to-list 'auto-mode-alist '("\.gsp$" . nxml-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))

;;; make Groovy mode electric by default.
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
  (outline-minor-mode)
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
(global-set-key [f11] 'toggle-fullscreen)

(global-set-key "\C-xO" (lambda () (interactive) (other-window -1)))
(global-set-key "\C-xp" (lambda () (interactive) (other-window -1)))

(global-set-key "\C-xE" 'apply-macro-to-region-lines)

;;so now Control-c 7 prompts for a Unicode hex code, will then insert the glyph
(global-set-key "\C-c7" 'ucs-insert)

(autoload 'svn-status "psvn" "Load subversion SCM commands." t)

(autoload 'magit-status "magit" "Load git SCM commands." t)
(global-set-key "\C-xg" 'magit-status)
(global-set-key [f6] 'magit-status)

(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

(global-set-key "\C-cw" 'whitespace-cleanup)
(global-set-key "\C-c;" 'comment-region)
(global-set-key "\C-c:" 'uncomment-region)

(global-set-key [remap find-tag] 'ido-find-tag)
(global-set-key (kbd "C-.") 'find-file-in-project)
(global-set-key (kbd "C-,") 'find-grep-in-project)

;; Ediff
;(eval-after-load 'ediff
;  (setq ediff-split-window-function 'split-window-horizontally))

;; from http://www.ntu.edu.sg/home5/pg04878518/EmacsTools.html

;; (require 'session)
;; (add-hook 'after-init-hook 'session-initialize)

;; make highlight standard /etc files
(require 'generic)
(require 'generic-x)

(require 'page-ext)
(require 'recentf)

(autoload 'dna-mode "dna-mode" "Major mode for dna" t)
(add-to-list 'magic-mode-alist '("^>\\|ID\\|LOCUS\\|DNA" . dna-mode))
(add-to-list
 'auto-mode-alist
 '("\\.\\(fasta\\|fa\\|exp\\|ace\\|gb\\)\\'" . dna-mode))
(add-hook 'dna-mode-hook 'turn-on-font-lock)


(defun graphviz
  (interactive)
  (require 'graphviz-dot-mode)
  (eval-after-load 'graphviz-dot-mode
    (setq graphviz-dot-indent-width 2)
    (setq graphviz-dot-auto-indent-on-semi nil)))

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

(autoload 'malabar-mode "malabar-mode" "load java/malabar mode" t)
(setq malabar-groovy-lib-dir (concat site-lisp "/malabar-1.5-SNAPSHOT/lib"))

(add-hook 'malabar-mode-hook
          (lambda ()
            (setq c-basic-offset 4)
            (add-hook 'after-save-hook 'malabar-compile-file-silently nil t)))

;(require 'auto-complete-config)
;(add-to-list 'ac-dictionary-directories (concat site-lisp "/ac-dict"))
;(ac-config-default)

(defun eclim nil
  (interactive)
  (require 'eclim)
  (setq eclim-auto-save t)
  (global-eclim-mode))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (require 'bytecomp)
            (add-hook 'after-save-hook
                      (lambda ()
                        (byte-compile-file (buffer-file-name) t))))) 

(add-hook 'after-init-hook
      (lambda nil
        (message "clgc-after-init-hook")
        (server-start)
        ;; (normal-erase-is-backspace-mode)
        ))

;; setup audo modes
(setq auto-modes
 '(("\\.C$"          . c++-mode)
   ("\\.cc$"         . c++-mode)
   ("\\.[ch]xx|pp$"  . c++-mode)
   ("\\.hh$"         . c++-mode)
   ;; C Bindings
   ("\\.c$"          . c-mode)
   ("\\.h$"          . c++-mode)

   ("\\.awk"         . awk-mode)
   ("\\.css"         . css-mode)

   ;; Ruby Bindings
   ("\\.rb$"         . ruby-mode)
   ("\\.ruby$"       . ruby-mode)
   ("\\.rake$"       . ruby-mode)
   ("[Rr]akefile$"   . ruby-mode)
   ("\\.gem$"        . ruby-mode)
   ("\\.gemspec$"    . ruby-mode)

   ("\\.ya?ml$"      . yaml-mode)
   ("\\.sass$"       . sass-mode)
   ("\\.ha?ml$"      . haml-mode)

   ;;("\\.java$"       . malabar-mode)
   ("\\.js$"         . js2-mode)))
(dolist (mode auto-modes) (add-to-list 'auto-mode-alist mode))

(load custom-file 'noerror)
