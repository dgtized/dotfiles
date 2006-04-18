
;(set-foreground-color "light grey")
;(set-background-color "black")
;(set-cursor-color "light blue")
;(set-mouse-color "light blue")
;(set-border-color "light grey")

;; (add-to-list 'auto-mode-alist '("\\.rsp\\'" . rsp-mode))
;; (autoload 'rsp-mode "RSP" "Major mode for editing RSP code" t)
;; (require 'rsp-mode) 


;; I want server support to work but does it even work under console?
;;(if (not (equal (getenv "OSNAME") "winblows") )
;;  (server-start)
;;  )
;; Stuff to avoid when reloading ~/.emacs, but do when starting the app.
(unless (boundp 'emacs-loaded)
	(progn
	  (defvar emacs-loaded 1)
	  ;;      (gnuserv-start)))
	  ;;	  ;; We do this here because RMS tells us to
	  ;;	  (desktop-load-default)
	  ;;	  (desktop-read))
)


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


(autoload 'speedbar-frame-mode "speedbar" "Popup a speedbar frame" t)
(autoload 'speedbar-get-focus "speedbar" "Jump to speedbar frame" t)


(when-emacs-version 21
                    ;; Handle ANSI color sequences nicely.
                    (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
                    (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on))

(autoload 'jde-mode "jde" "JDE mode." t)
(add-hook 'jde-mode-hook (lambda () 
                           (setq c-basic-offset 2)
                           (require 'ecb-autoloads)))

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
	    html-author-email "</a>&gt;")
)


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
;    (global-unset-key "%"))
)


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

;;
;; put compiler in a new frame
;;
;(setq special-display-buffer-names
;      (cons "*compilation*" special-display-buffer-names))

;;
;; misc. tab/whitespace stuff
;;
(setq-default indent-tabs-mode nil)

(defun turn-on-nuke-trailing-whitespace ()
  (setq nuke-trailing-whitespace-p t))
(add-hook 'c-mode-hook 'turn-on-nuke-trailing-whitespace)
(add-hook 'c++-mode-hook 'turn-on-nuke-trailing-whitespace)
(add-hook 'makefile-mode-hook 'turn-on-nuke-trailing-whitespace)


;;(when (memq system-type '(windows-nt)) do-stuff)

(if (not (boundp 'cec-ntemacs))
    (progn
      (defvar cec-ntemacs nil)))

(if (not (boundp 'home-ntemacs))
    (progn
      (defvar home-ntemacs nil)))

(defvar platform-home-path
  (concat (if cec-ntemacs "h:"
            (if home-ntemacs "/cygwin/home/comstocl"
              (getenv "HOME")))))

(defvar site-path   (concat platform-home-path "/.emacs.d/site-lisp/"))
(defvar cedet-path  (concat site-path "cedet/"))
(defvar backup-path (concat platform-home-path "/.emacs.d/backups/"))

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
       (mapcar (lambda (x) (concat cedet-path x))
               (list
                "auctex"
                "speedbar"
                "eieio"
                "cogre"
                "common"
                "contrib"))
       load-path))
;;(load (concat site-path "subdirs.el"))
;;(load (concat cedet-path "subdirs.el"))

;; Load CEDET
(setq semantic-load-turn-useful-things-on t)
(load-file (concat cedet-path "common/cedet.el"))
;(semantic-load-enable-code-helpers)
;;(require 'overlay-fix)

;;;
;;; Backup files in one spot
;;;
(setq backup-directory-alist nil)
(setq backup-directory-alist
      (cons (cons "\\.*$" backup-path)
            backup-directory-alist))

; all backups (*~ files) are in the hidden directory /.emacs.d/.emacs-backup
;(require 'backup-dir)
;(setq bkup-backup-directory-info
;      '((t backup-path full-path prepend-name search-upward)))
