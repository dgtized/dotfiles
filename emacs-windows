;; Please be -*- emacs-lisp -*-

(defmacro when-emacs-version (vers &rest body)
  `(when (equal emacs-major-version ,vers)
    ,@body))

(defun call-if-fbound (function &rest args)
  (when (fboundp function)
	(apply function args)))

(defmacro unbound-defvar (name value)
  `(unless (boundp (quote ,name))
     (progn (defvar ,name ,value))))

(defun indent-or-complete ()
  "Complete if point is at end of line, and indent line."
  (interactive)
  (if (looking-at "$")
      (hippie-expand nil))
  (indent-for-tab-command))

(setq line-number-mode t)
(setq column-number-mode t)
(setq auto-fill-default t)
(setq-default indent-tabs-mode t) ; default save as tabs mode
(setq visible-bell t)
(setq require-final-newline t)
(setq next-line-extends-end-of-buffer nil)
(setq debug-on-error t)
(fset 'yes-or-no-p 'y-or-n-p)
(resize-minibuffer-mode)            ; Size the minibuffer according to contents.

(defvar home-ntemacs t)
(unbound-defvar home-ntemacs nil)

(defvar platform-home-path
  (concat (if home-ntemacs 
	      "c:"
	    (getenv "HOME"))))

(defvar site-path   (concat platform-home-path ".emacs.d/site-lisp/"))
(defvar cedet-path  (concat site-path "cedet/"))
(defvar backup-path (concat platform-home-path "/.emacs.d/backups/"))

(setq load-path (cons site-path load-path))

 (setq load-path 
       (append 
        (list site-path)
        (mapcar (lambda (x) (concat site-path x))
                (list 
                 "jde/lisp"		  
                 "elib"
                 "ecb"
                 "semantic"
                 "cc"
                 ))
        load-path))

;;(load (concat site-path "subdirs.el"))
(load (concat cedet-path "subdirs.el"))

;; Load CEDET
(setq semantic-load-turn-useful-things-on t)
(load-file (concat cedet-path "common/cedet.el"))
;(semantic-load-enable-code-helpers)
;;(require 'overlay-fix)

;; make the backup gods obey ME! no more ~ sprinkles all over the place
(setq version-control nil)
(add-to-list 'backup-directory-alist
	     (cons "." backup-path))

;Enable opposite bracket/paranthesis highlighting
(require 'paren)
(show-paren-mode t)
(setq blink-matching-paren nil)

;; Enable font lock (colours) for all modes that support it:
(require 'font-lock)
(setq-default font-lock-maximum-decoration t)
(global-font-lock-mode t)

(setq completion-ignored-extensions
      '("~" ".aux" ".a" ".bbl" ".blg" ".dvi" ".elc" ".class"
        ".hc" ".hi" ".log" ".mlc" ".o" ".so" ".toc"))

;;
;; misc. tab/whitespace stuff
;;
(setq-default indent-tabs-mode nil)

(defun turn-on-nuke-trailing-whitespace ()
  (setq nuke-trailing-whitespace-p t))
(add-hook 'c-mode-hook 'turn-on-nuke-trailing-whitespace)
(add-hook 'c++-mode-hook 'turn-on-nuke-trailing-whitespace)
(add-hook 'makefile-mode-hook 'turn-on-nuke-trailing-whitespace)

;; goto-line
(global-set-key [(meta g)] 'goto-line) 

;;
;; set c/c++ indent width and compile modes
;;
(add-hook 'c-mode-hook 
          (function (lambda ()
                      (c-set-style "K&R")
                      (setq c-basic-offset 2)
                      (setq indent-tabs-mode nil)
                      (local-set-key "\C-cc" 'compile))))

;;
;; put compiler in a new frame
;;
;(setq special-display-buffer-names
;      (cons "*compilation*" special-display-buffer-names))

; necessary support function for buffer burial
(defun crs-delete-these (delete-these from-this-list)
  "Delete DELETE-THESE FROM-THIS-LIST."
  (cond
   ((car delete-these)
    (if (member (car delete-these) from-this-list)
	(crs-delete-these (cdr delete-these) (delete (car delete-these)
                                                     from-this-list))
      (crs-delete-these (cdr delete-these) from-this-list)))
   (t from-this-list)))

(defun crs-hated-buffers ()
  "List of buffers I never want to see, converted from names to buffers."
  (delete nil
	  (append
	   (mapcar 'get-buffer crs-hated-buffers)
	   (mapcar (lambda (this-buffer)
		     (if (string-match "^ " (buffer-name this-buffer))
			 this-buffer))
		   (buffer-list)))))

; I'm sick of switching buffers only to find KILL right in front of me
(defun crs-bury-buffer (&optional n)
  (interactive)
  (unless n
    (setq n 1))
  (let ((my-buffer-list (crs-delete-these (crs-hated-buffers)
					  (buffer-list (selected-frame)))))
    (switch-to-buffer
     (if (< n 0)
	 (nth (+ (length my-buffer-list) n)
	      my-buffer-list)
       (bury-buffer)
       (nth n my-buffer-list)))))

(global-set-key [(control tab)] 'crs-bury-buffer)
(global-set-key [(control shift tab)]  (lambda () (interactive) (crs-bury-buffer -1)))
(global-set-key "\C-cc" 'compile)
(global-set-key "\C-cm" 
                (lambda () (interactive) 
                  (switch-to-buffer "Makefile") 
                  (compile)))
(global-set-key "\C-xp" (lambda () (interactive) (other-window -1)))

(setq iswitchb-case nil) ; completions are case sensitive.
(iswitchb-default-keybindings)

; this is the list of buffers I never want to see
(defvar crs-hated-buffers
  '("KILL" "*Compile-Log*"))

; might as well use this for both
(setq iswitchb-buffer-ignore (append '("^ " "*Buffer") crs-hated-buffers))

; dunno what this does so don't use it yet
;(global-set-key "\C-cy" 'do-smart-yank)
;(fset 'do-smart-yank "\C-y\C-c\C-q")
                  
(defun up-slightly () (interactive) (scroll-up 5))
(defun down-slightly () (interactive) (scroll-down 5))
(global-set-key [mouse-4] 'down-slightly)
(global-set-key [mouse-5] 'up-slightly)
                  
(defun up-one () (interactive) (scroll-up 1))
(defun down-one () (interactive) (scroll-down 1))
(global-set-key [S-mouse-4] 'down-one)
(global-set-key [S-mouse-5] 'up-one)
                  
(defun up-a-lot () (interactive) (scroll-up))
(defun down-a-lot () (interactive) (scroll-down))
(global-set-key [C-mouse-4] 'down-a-lot)
(global-set-key [C-mouse-5] 'up-a-lot)

;(load "desktop")
;(desktop-load-default)
;(desktop-read)

;; Make the % key jump to the matching {}[]() if on another, like VI

;(bounce-toggle t)

(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))
;(defun bounce-toggle (arg)
;  (interactive)
;  (if arg
      (global-set-key "%" 'match-paren)
;    (global-unset-key "%")))

(require 'compile)
;
; ruby mode
; 

(autoload 'ruby-mode "ruby-mode" nil t)
(add-hook 'ruby-mode-hook
            '(lambda () (abbrev-mode 1)
                    (define-key ruby-mode-map "\C-m" 'ruby-reindent-then-newline-and-indent)
                         (define-key ruby-mode-map "\C-j" 'newline)))

(defun ruby-eval-buffer () (interactive)
   "Evaluate the buffer with ruby."
   (shell-command-on-region (point-min) (point-max) "ruby -w "))

(defun my-ruby-mode-hook ()
  (make-variable-buffer-local 'compilation-error-regexp-alist)
   (setq compilation-error-regexp-alist 
        (append compilation-error-regexp-alist 
                (list (list  
                       (concat "\\(.*?\\)\\([0-9A-Za-z_./\:-]+\\.rb\\):\\([0-9]+\\)") 2 3))))
  (make-variable-buffer-local 'compile-command)
  (setq compile-command (concat "ruby -w " (buffer-file-name) " "))
  (local-set-key "\C-cr" 'ruby-eval-buffer)
  (inf-ruby-keys)
  (require 'ruby-electric)
  (ruby-electric-mode)
)

(add-hook 'ruby-mode-hook 'my-ruby-mode-hook)

(setq interpreter-mode-alist (append '(("ruby" . ruby-mode))
    				     interpreter-mode-alist))
(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby" "Set local key defs for inf-ruby in ruby-mode")

(c-add-style
 "ruby"
 '("bsd"
   (c-basic-offset . 4)
   (c-offsets-alist
    (case-label . 2)
    (label . 2)
    (statement-case-intro . 2)
    )))

(add-hook 'after-save-hook
          '(lambda ()
             (progn
               (and (save-excursion
                      (save-restriction
                        (widen)
                        (goto-char (point-min))
                        (save-match-data
                          (looking-at "^#!"))))
                    (shell-command (concat "chmod u+x " buffer-file-name))
                    (message (concat "Saved as script: " buffer-file-name))
                    ))))

;
; html helper
;
(autoload 'html-helper-mode "html-helper-mode" "HTML Helper Mode" t)

(setq html-helper-do-write-file-hooks t)
(setq html-helper-build-new-buffer t)
(defvar html-author-website "http://userfs.cec.wustl.edu/~cc1")
(defvar html-author-name "Charles Comstock")
(defvar html-author-email "cc1@cec.wustl.edu")
(setq html-helper-address-string 
  (concat 
   (if (not (null html-author-website)) (concat "<a href=\"" html-author-website "\">"))
	    html-author-name 
	    "</a>  &lt;<a href=\"mailto:" html-author-email "\">" 
	    html-author-email "</a>&gt;"))

;(setq-default compilation-error-regexp-alist
; (append '(
; ; Microsoft JVC:
; ;sample.java(6,1) : error J0020: Expected 'class' or 'interface'
; ("\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)[,]\\([0-9]+\\)) : \\(error\\|warning\\) J[0-9]+:" 1 3 4)
;
; ; Microsoft C/C++:
; ;  keyboard.c(537) : warning C4005: 'min' : macro redefinition
; ;  d:\tmp\test.c(23) : error C2143: syntax error : missing ';' before 'if'
; ;VC EEi
; ;e:\projects\myce40\tok.h(85) : error C2236: unexpected 'class' '$S1'
; ;myc.cpp(14) : error C3149: 'class System::String' : illegal use of managed type 'String'; did you forget a '*'?
;    ("\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) \
;: \\(error\\|warning\\) C[0-9]+:" 1 3)
; ) compilation-error-regex-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              C# Mode support
;;;

(autoload 'csharp-mode "cc-mode")

(c-add-style "myC#Style"
   '("C#"
   (c-basic-offset . 2)
   (c-comment-only-line-offset . (0 . 0))
   (c-offsets-alist . (
     (c                     . c-lineup-C-comments)
     (inclass		    . 0)
     (namespace-open	    . +)
     (namespace-close	    . +)
     (innamespace	    . 0)
     (class-open	    . +)
     (class-close	    . +)
     (inclass		    . 0)
     (defun-open	    . +)
     (defun-block-intro     . 0)
     (inline-open	    . +)
     (inline-close	    . 0)
     (statement-block-intro . 0)
     (statement-cont	    . +)
     (brace-list-intro      . +)
     (topmost-intro-cont    . 0)
     (block-open	    . +)
     (block-close	    . 0)
     (arglist-intro	    . +)
;     (arglist-cont	    . 0)
     (arglist-close	    . 0)
     ))
   ))

(add-hook 'csharp-mode-hook 
          (lambda ()
            (cond (window-system
                   (turn-on-font-lock)
                   (c-set-style "myC#Style")))))



(append '(          
          ;;C# Compiler
          ;;t.cs(6,18): error SC1006: Name of constructor must match name of class
          ;;(setq compilation-error-regexp-alist
          ("\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)[,]\\([0-9]+\\)): \\(error\\|warning\\) CS[0-9]+:" 1 3 4)
          )
        compilation-error-regexp-alist)

(autoload 'jde-mode "jde" "JDE mode." t)
(add-hook 'jde-mode-hook (lambda () 
                           (setq c-basic-offset 2)
                           (require 'ecb-autoloads)))

(autoload 'speedbar-frame-mode "speedbar" "Popup a speedbar frame" t)
(autoload 'speedbar-get-focus "speedbar" "Jump to speedbar frame" t)

;; Include the following only if you want to run
;; Bash as your shell.

;; Setup Emacs to run bash as its primary shell.
(setq shell-file-name "bash")
(setq shell-command-switch "-c")
(setq explicit-shell-file-name shell-file-name)
(setenv "SHELL" shell-file-name)
(setq explicit-sh-args '("-login" "-i"))
(if (boundp 'w32-quote-process-args)
    (setq w32-quote-process-args ?\")) ;; Include only for MS Windows.

(when-emacs-version 21
                    ;; Handle ANSI color sequences nicely.
                    (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
                    (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on))

(setq auto-mode-alist
      (append 
       '(
         ;; 
	 ("\\.C$"         . c++-mode)
	 ("\\.cc$"        . c++-mode)
	 ("\\.cpp$"       . c++-mode)
	 ("\\.cxx$"       . c++-mode)
	 ("\\.hxx$"       . c++-mode)
	 ;;("\\.h$"     . c++-mode)
	 ("\\.hh$"        . c++-mode)
	 ("\\.idl$"       . c++-mode)
	 ("\\.ipp$"       . c++-mode)
         ;; C Bindings
	 ("\\.c$"         . c-mode)
	 ("\\.h$"         . c-mode)
         ;; Perl Bindings
	 ("\\.pl$"        . perl-mode)
	 ("\\.pm$"        . perl-mode)
         
         ("\\.awk"        . awk-mode)
         
         ;; Ruby Bindings
	 ("\\.rb$"        . ruby-mode)
	 ("\\.ruby$"      . ruby-mode)
	 ("\\.rhtml$"     . ruby-mode)
	 ("\\[Rr]akefile$" . ruby-mode)
	 ("\\.gem$"       . ruby-mode)
	 ("\\.gemspec$"   . ruby-mode)
         
	 ("\\.java$"      . jde-mode)

	 ("\\.txt$"       . text-mode)

	 ("\\.s?html?\\'" . html-helper-mode)
	 ("\\.asp$"       . html-helper-mode)
	 ("\\.aspx$"      . html-helper-mode)
	 ("\\.ascx$"      . html-helper-mode)
	 ("\\.html$"      . html-helper-mode)
	 ("\\.htm$"       . html-helper-mode)

	 ("\\.cs$"        . csharp-mode)

         ("\\.tex\\'"     . LaTeX-mode)
         ("\\.sty\\'"     . LaTeX-mode)
         ("\\.bbl\\'"     . LaTeX-mode)
         ("\\.bib\\'"     . bibtex-mode)
         ("\\.texi"       . texinfo-mode)
	 ) auto-mode-alist))

(add-hook 'font-lock-mode-hook
	  (lambda ()
	    ;; This is cool...
	    ;;(setq font-lock-keywords
            ;;	   (append font-lock-keywords
            ;;	  '(("\\(FIX\\([ -_]?ME\\)?\\|FUCK\\|HELP\\([_- ]?ME\\)?\\|TODO\\|HACK\\)" 
            ;;     (1 'font-lock-warning-face t)))))
	    ;; Lazy font lock is good. 5000 line C++ files get on my nerves
	    ;; without it. But smaller files should be fontified on open so
	    ;; they can be browsed quickly. This is the solution:
	    (if (> (buffer-size) 5000) (call-if-fbound 'turn-on-lazy-lock))))

(autoload 'LaTeX-mode "tex-site")
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'text-mode-hook (lambda () (auto-fill-mode 1)))
(setq tex-dvi-view-command "xdvi")

; Turn off Emacs 21 toolbar
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))

(autoload 'matlab-mode "matlab" "Enter Matlab mode." t)
(setq auto-mode-alist (cons '("\\.m$" . matlab-mode) auto-mode-alist))
(setq matlab-mode-hook 
      (lambda ()
        (setq matlab-indent-function t) ; if you want function bodies indented
        (setq fill-column 76)                 ; where auto-fill should wrap
        (matlab-mode-hilit)
        (turn-on-auto-fill)))
(autoload 'matlab-shell "matlab" "Interactive Matlab mode." t)

;; Quack is enhanced scheme mode -- it is good
(require 'quack)

;; I want server support to work but does it even work under console?
;;(if (not (equal (getenv "OSNAME") "winblows") )
;;  (server-start)
;;  )
;; Stuff to avoid when reloading ~/.emacs, but do when starting the app.
(if (not (boundp 'emacs-loaded))
	(progn
	  (defvar emacs-loaded 1)
	  ;;      (gnuserv-start)))
	  ;;	  ;; We do this here because RMS tells us to
	  ;;	  (desktop-load-default)
	  ;;	  (desktop-read))
))

(add-to-list 'auto-mode-alist '("\\.rsp\\'" . rsp-mode))
(autoload 'rsp-mode "RSP" "Major mode for editing RSP code" t)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;(set-foreground-color "light grey")
;(set-background-color "black")
;(set-cursor-color "light blue")
;(set-mouse-color "light blue")
;(set-border-color "light grey")

(set-frame-height (selected-frame) 58)
(set-frame-width  (selected-frame) 120)

(custom-set-variables
  ;; custom-set-variables was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 '(compilation-ask-about-save nil)
 '(compilation-read-command t)
 '(compilation-window-height 10)
 '(quack-default-program "mzscheme")
 '(quack-pretty-lambda-p nil)
 '(quack-programs (quote ("mzscheme -M eopl" "bigloo" "csi" "csi -hygienic" "gosh" "gsi" "guile" "kawa" "mit-scheme" "mred -z" "mzscheme" "mzscheme -M errortrace" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi")))
 '(quack-tabs-are-evil-p t))
(custom-set-faces
  ;; custom-set-faces was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 )
