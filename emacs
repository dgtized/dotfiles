;; Please be -*- emacs-lisp -*-

(save-excursion
  (let ((site-config-buf (find-file-noselect "~/.site-config")))
    (switch-to-buffer site-config-buf)
    (goto-line 0)
    (while (re-search-forward "export \\(.*\\)=\\(.*\\)" nil t)
      (setenv (match-string 1) (match-string 2)))
    (kill-buffer site-config-buf)
    ))

(defvar dotc-dir (getenv "DOTC_DIR") "shell config directory")
(defvar dotc-name (getenv "DOTC_NAME") "shell config name")

(add-to-list 'load-path (concat dotc-dir "/site-lisp"))

(if (string-equal dotc-name "gentoo")
    (require 'clgc-site-gentoo))
(require 'clgc-functions)

(if window-system
    (progn
      (set-frame-width (selected-frame) 140)
      (set-frame-height (selected-frame) 70)
      (set-frame-font "fixed")
      ;; Turn off Emacs 21 toolbar
      (if (fboundp 'tool-bar-mode)
	  (tool-bar-mode -1))
      (if (load "mwheel" t)
	  (mwheel-install)))
  ;; if we are in text we don't need no stinkin menu's
  (menu-bar-mode 0)
  )
 
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

; (setq iswitchb-case nil) ; completions are case sensitive.
(if (fboundp 'iswitchb-default-keybindings)
    (iswitchb-default-keybindings)
  (iswitchb-mode))

;; make the backup gods obey ME! no more ~ sprinkles all over the place
(setq version-control nil)
(add-to-list 'backup-directory-alist
	     (cons "." "~/.emacs.d/backups/"))

(require 'compile)
(setq compilation-ask-about-save nil)
(setq compilation-read-command t)
(setq compilation-window-height 10)

;
; Scheme Mode
;
(autoload 'scheme-mode "quack" "Quack scheme editing mode" t)
(autoload 'run-scheme "quack" "Quack scheme editing mode" t)

;
; ruby mode
; 

(defun ruby-eval-buffer () (interactive)
   "Evaluate the buffer with ruby."
   (shell-command-on-region (point-min) (point-max) "ruby -w "))

(defun my-ruby-mode-hook ()
  (make-variable-buffer-local 'compilation-error-regexp-alist)
  (add-to-list 'compilation-error-regexp-alist
	       '("test[a-zA-Z0-9_]*([A-Z][a-zA-Z0-9_]*) \\[\\(.*\\):\\([0-9]+\\)\\]:" 1 2))
  (add-to-list 'compilation-error-regexp-alist
	       '("\\(.*?\\)\\([0-9A-Za-z_./\:-]+\\.rb\\):\\([0-9]+\\)" 2 3))
  (make-variable-buffer-local 'compile-command)
  (setq compile-command (concat "ruby -w " (buffer-file-name) " "))
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
  (inf-ruby-keys)
  ;; from: http://shylock.uw.hu/Emacs/ruby-electric.el
  (require 'ruby-electric)
  (ruby-electric-mode)
  (abbrev-mode 1)
  (define-key ruby-mode-map "\C-m" 'ruby-reindent-then-newline-and-indent)
  (define-key ruby-mode-map "\C-j" 'newline)
)

(add-hook 'ruby-mode-hook 'my-ruby-mode-hook)

(autoload 'ruby-mode "ruby-mode" nil t)
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode) t)
(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby" "Set local key defs for inf-ruby in ruby-mode")

(if (string-equal dotc-name "gentoo") 
    (progn
      ;; eRuby
      (require 'mmm-mode)
      (require 'mmm-auto)
      (setq mmm-global-mode 'maybe)
      ;;(setq mmm-submode-decoration-level 0)
      ;;(set-face-background 'mmm-output-submode-face  "LightGrey")
      ;;(set-face-background 'mmm-code-submode-face    "MediumSlateBlue")
      ;;(set-face-background 'mmm-comment-submode-face "DarkOliveGreen")
      (mmm-add-classes
      '((eruby
	 :submode ruby-mode
	 :match-face (("<%#" . mmm-comment-submode-face)
		      ("<%=" . mmm-output-submode-face)
		      ("<%"  . mmm-code-submode-face))
	 :front "<%[#=]?"
	 :back  "-?%>"
	 :insert ((?% erb-code       nil @ "<%"  @ " " _ " " @ "%>" @)
		  (?# erb-comment    nil @ "<%#" @ " " _ " " @ "%>" @)
		  (?= erb-expression nil @ "<%=" @ " " _ " " @ "%>" @))
	 )))
      (add-hook 'html-mode-hook
      (lambda ()
	(setq mmm-classes '(erb-code))
	(mmm-mode-on)))
      (add-to-list 'auto-mode-alist '("\\.rhtml$" . html-mode))
      (add-to-list 'auto-mode-alist '("\\.rhtml$" . html-mode))
      ))

(autoload 'css-mode "css-mode")

;; Perl Stuff (for the horrible times when I can't use ruby)
(defalias 'perl-mode 'cperl-mode)
(defun my-cperl-mode-hook ()      
  (setq cperl-hairy t)
  ;(setq cperl-auto-newline t)
  (outline-minor-mode)
  ;(define-key cperl-mode-map "\C-cp" 'cperl-perldoc)
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

;; ISWITCH

; this is the list of buffers I never want to see
(defvar crs-hated-buffers
  '("KILL" "*Compile-Log*"))

; might as well use this for both
(setq iswitchb-buffer-ignore (append '("^ " "*Buffer") crs-hated-buffers))

(setq completion-ignored-extensions
      '("~" ".aux" ".a" ".bbl" ".blg" ".dvi" ".elc" ".class"
        ".hc" ".hi" ".log" ".mlc" ".o" ".so" ".toc"))

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
         
         ;; Ruby Bindings
	 ("\\.rb$"         . ruby-mode)
	 ("\\.ruby$"       . ruby-mode)
	 ("\\[Rr]akefile$" . ruby-mode)
	 ("\\.gem$"        . ruby-mode)
	 ("\\.gemspec$"    . ruby-mode)
	 ) auto-mode-alist))

;; KEYBINDINGS

(global-set-key [f11] 'redraw-display)

(global-set-key [(control tab)] 'crs-bury-buffer)
(global-set-key [(control shift tab)]  (lambda () (interactive) (crs-bury-buffer -1)))
(global-set-key "\C-cc" 'compile)	
(global-set-key [f5] 'compile)		
;;(global-set-key "\C-cm" 		
;;                (lambda () (interactive) 
;;                  (switch-to-buffer "Makefile") 
;;                  (compile "make -k")))	
(global-set-key "\C-xp" (lambda () (interactive) (other-window -1))) 
(global-set-key "\C-xE" 'apply-macro-to-region-lines)

;;so now Control-c 7 prompts for a Unicode hex code, will then insert the glyph
(global-set-key "\C-c7" 'ucs-insert)

;; (global-set-key [f6] 'svn-status)
(require 'psvn)
(global-set-key "\C-c1" 'svn-status)

(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)
(global-set-key "\C-cg" 'goto-line)

(global-set-key "\C-cw" 'whitespace-cleanup)
(global-set-key "\C-c;" 'comment-region)
(global-set-key "\C-c:" 'uncomment-region)

; (server-start)

;; from http://www.ntu.edu.sg/home5/pg04878518/EmacsTools.html

;; (require 'session)
;; (add-hook 'after-init-hook 'session-initialize)

;; (require 'ido)
;; (ido-mode t)

;; make highlight standard /etc files
(require 'generic)
(require 'generic-x)

(require 'page-ext)

;; it eats up some screen space but let's play with it for a while
;(require 'tabbar)
;(tabbar-mode)

;; http://repose.cx/conf/.elisp/

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(quack-pretty-lambda-p nil)
 '(quack-programs (quote ("mzscheme" "mzscheme -M errortrace" "mzscheme -M eopl" "mred -z" "guile" "mit-scheme" "scheme" "scheme48")))
 '(quack-tabs-are-evil-p t))

(defun autocompile nil
  "compile itself if ~/.emacs"
  (interactive)
  (require 'bytecomp)
  (if (string= (buffer-file-name) 
	       (expand-file-name "~/.emacs"))	  
      (byte-compile-file (buffer-file-name))))
 
(add-hook 'after-save-hook 'autocompile)
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
