;; Please be -*- emacs-lisp -*-

(if (file-exists-p "~/.site-config")
    (save-excursion
      (let ((site-config-buf (find-file-noselect "~/.site-config")))
        (switch-to-buffer site-config-buf)
        (goto-line 0)
        (while (re-search-forward "export \\(.*\\)=\\(.*\\)" nil t)
          (setenv (match-string 1) (match-string 2)))
        (kill-buffer site-config-buf)
        )))

(defconst dotc-dir (getenv "DOTC_DIR") "shell config directory")
(defconst dotc-name (getenv "DOTC_NAME") "shell config name")

(add-to-list 'load-path (concat dotc-dir "/site-lisp"))
(add-to-list 'load-path (concat dotc-dir "/site-lisp/ruby"))
;(add-to-list 'load-path (concat dotc-dir "/site-lisp/rhtml"))
;(add-to-list 'load-path (concat dotc-dir "/site-lisp/rails"))

(if (string-equal dotc-name "gentoo")
    (require 'clgc-site-gentoo))
(require 'clgc-functions)

(setq inhibit-startup-message t)
(setq default-buffer-file-coding-system 'utf-8
      file-name-coding-system 'utf-8
      locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-selection-coding-system 'compound-text-with-extensions)

(setq line-number-mode t
      column-number-mode t
      auto-fill-default t
      c-basic-offset 2
      visible-bell t
      require-final-newline t
      debug-on-error t)
(setq-default indent-tabs-mode t) ; default save as tabs mode
(fset 'yes-or-no-p 'y-or-n-p)
;(resize-minibuffer-mode)           ; Size the minibuffer according to contents.

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

;; Ido
(require 'ido)
(ido-mode t)

(defvar crs-hated-buffers
  '("KILL" "*Compile-Log*"))
; might as well use this for both
;(setq ido-ignore-buffers (append '("^ " "*Buffer") crs-hated-buffers))

(setq completion-ignored-extensions
      '("~" ".aux" ".a" ".bbl" ".blg" ".dvi" ".elc" ".class"
        ".hc" ".hi" ".log" ".mlc" ".o" ".so" ".toc"))

;; make the backup gods obey ME! no more ~ sprinkles all over the place
(setq version-control nil)
(add-to-list 'backup-directory-alist
             (cons "." "~/.emacs.d/backups/"))

(require 'mode-compile)
(autoload 'mode-compile "mode-compile"
  "Command to compile current buffer file based on the major mode" t)
(autoload 'mode-compile-kill "mode-compile"
  "Command to kill a compilation launched by `mode-compile'" t)
(global-set-key "\C-cc" 'mode-compile)
(global-set-key "\C-ck" 'mode-compile-kill)
(setq emacs-lisp-sources-regexp "\\.el$|\\.emacs$")
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
(autoload 'css-mode "css-mode")
(setq cssm-indent-function #'cssm-c-style-indenter)
(setq cssm-indent-level '2)

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
  (define-key ruby-mode-map "\C-m" 'ruby-reindent-then-newline-and-indent)
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

(if (or (string-equal dotc-name "gentoo")
        (string-equal dotc-name "debian")
        (and (string-equal dotc-name "bio")
             (>= emacs-major-version 22)))
    (progn
      ;; eRuby
      (require 'mmm-mode)
      (require 'mmm-auto)
      (setq mmm-global-mode 'maybe)

      (setq mmm-submode-decoration-level 2)
      (set-face-background 'mmm-output-submode-face "white")
      (set-face-background 'mmm-code-submode-face "white")
      (set-face-background 'mmm-comment-submode-face "white")
      (set-face-foreground 'mmm-output-submode-face "DarkGreen")
      (set-face-foreground 'mmm-code-submode-face "DarkGreen")
      (set-face-foreground 'mmm-comment-submode-face "Red")
      (set-face-foreground 'mmm-delimiter-face "Yellow")
      (make-face-italic 'mmm-delimiter-face)
      (make-face-italic 'mmm-comment-submode-face)

      (mmm-add-classes
       '((eruby-code
          :submode ruby-mode
          :match-face (("<%#" . mmm-comment-submode-face)
                       ("<%=" . mmm-output-submode-face)
                       ("<%"  . mmm-code-submode-face))
          :front "<%[#=]?"
          :back "-?%>"
          :insert ((?% erb-code       nil @ "<%"  @ " " _ " " @ "%>" @)
                   (?# erb-comment    nil @ "<%#" @ " " _ " " @ "%>" @)
                   (?= erb-expression nil @ "<%=" @ " " _ " " @ "%>" @)))

         (html-css-attribute
          :submode css-mode
          :face mmm-declaration-submode-face
          :front "style=\""
          :back "\"")))

      ;; (add-hook 'html-mode-hook (lambda ()
      ;;                                   (setq mmm-classes '(eruby-code))
      ;;                                   (mmm-mode-on)))
      ;; (add-to-list 'auto-mode-alist '("\\.rhtml$" . rhtml-mode))
      ))

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

;;;
;;; VHDL mode
;;;
;(autoload 'vhdl-mode "vhdl-mode" "VHDL Mode" t)

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

(setq auto-mode-alist
      (append
       '(
         ("\\.C$"          . c++-mode)
         ("\\.cc$"         . c++-mode)
         ("\\.[ch]xx|pp$"  . c++-mode)
         ;;("\\.h$"      . c++-mode)
         ("\\.hh$"         . c++-mode)
         ;; C Bindings
         ("\\.c$"          . c-mode)
         ("\\.h$"          . c++-mode)

         ("\\.awk"         . awk-mode)
         ("\\.css"         . css-mode)

         ("\\.vhdl?\\'" . vhdl-mode)

         ;; Ruby Bindings
         ("\\.rb$"         . ruby-mode)
         ("\\.ruby$"       . ruby-mode)
         ("\\.rake$"       . ruby-mode)
         ("[Rr]akefile$"   . ruby-mode)
         ("\\.gem$"        . ruby-mode)
         ("\\.gemspec$"    . ruby-mode)
         ) auto-mode-alist))

(defun fix-display nil
  "fix display problems"
  (interactive)
  (redraw-display)
  (font-lock-fontify-buffer))

;; KEYBINDINGS
(global-set-key [f11] 'fix-display)

(global-set-key "\C-xO" (lambda () (interactive) (other-window -1)))
(global-set-key "\C-xp" (lambda () (interactive) (other-window -1)))

(global-set-key "\C-xE" 'apply-macro-to-region-lines)

;;so now Control-c 7 prompts for a Unicode hex code, will then insert the glyph
(global-set-key "\C-c7" 'ucs-insert)

(require 'psvn)
(global-set-key "\C-c1" 'svn-status)
(global-set-key [f6] 'svn-status)

(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)
(global-set-key "\C-cg" 'goto-line)

(global-set-key "\C-cw" 'whitespace-cleanup)
(global-set-key "\C-c;" 'comment-region)
(global-set-key "\C-c:" 'uncomment-region)

;; Ediff
;(eval-after-load 'ediff
;  (setq ediff-split-window-function 'split-window-horizontally))

; (server-start)

;; from http://www.ntu.edu.sg/home5/pg04878518/EmacsTools.html

;; (require 'session)
;; (add-hook 'after-init-hook 'session-initialize)

;; make highlight standard /etc files
(require 'generic)
(require 'generic-x)

(require 'page-ext)
(require 'recentf)

;; it eats up some screen space but let's play with it for a while
;(require 'tabbar)
;(tabbar-mode)

(defun graphviz
  (interactive)
  (require 'graphviz-dot-mode)
  (eval-after-load 'graphviz-dot-mode
    (setq graphviz-dot-indent-width 2)
    (setq graphviz-dot-auto-indent-on-semi nil)))

(defun auctex nil
  (interactive)
  (load "auctex.el" nil t t)
  (load "preview-latex.el" nil t t)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t))

;; http://repose.cx/conf/.elisp/

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(pr-ps-name (quote default))
 '(ps-inter-column 42)
 '(ps-landscape-mode t)
 '(ps-left-margin 42)
 '(ps-number-of-columns 2)
 '(ps-right-margin 42)
 '(ps-spool-duplex t)
 '(quack-pretty-lambda-p nil)
 '(quack-programs (quote ("mzscheme" "mzscheme -M errortrace" "mzscheme -M eopl" "mred -z" "guile" "mit-scheme" "scheme" "scheme48")))
 '(quack-tabs-are-evil-p t)
 '(rng-nxml-auto-validate-flag nil))

(defun autocompile nil
  "compile itself if ~/.emacs"
  (interactive)
  (require 'bytecomp)
  (if (string= (buffer-file-name)
               (expand-file-name "~/.emacs"))
      (byte-compile-file (buffer-file-name))))
;(add-hook 'after-save-hook 'autocompile)


(add-hook 'after-init-hook
	  (lambda nil
	    (server-start)
	    (normal-erase-is-backspace-mode)
	    (if (fboundp 'tool-bar-mode)
		(tool-bar-mode -1))))

(add-hook 'after-make-frame-functions 
	  (lambda (frame)
	    (if (window-system frame)
		(progn
		  (let ((c-height (floor (* (display-pixel-height) 0.80)))
			(c-width (floor (* (display-pixel-width) 0.80))))
		    (message "Setting width: %d, height: %d" c-width c-height)
		    (set-frame-position frame 5 30)
		    (set-frame-width frame c-width)
		    (set-frame-height frame c-height)
		    )
		  (set-frame-font 'fixed)
		  ;; Turn off Emacs 21 toolbar		  
		  (if (load "mwheel" t)
		      (mwheel-install)))
	      ;; if not in window system	      
	      )))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
