(defun clgc-prog-mode-hook ()
  (when (fboundp 'prettify-symbols-mode)
    (add-to-list 'prettify-symbols-alist '("<=" . ?≤))
    (add-to-list 'prettify-symbols-alist '(">=" . ?≥))
    (add-to-list 'prettify-symbols-alist '("!=" . ?≠))
    (prettify-symbols-mode))

  (setq show-trailing-whitespace t
        indicate-empty-lines t))

(add-hook 'prog-mode-hook 'clgc-prog-mode-hook)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Elm
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-elm-setup))

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

(defun clgc-text-mode-hook ()
  (flyspell-mode)
  (visual-line-mode))
(add-hook 'text-mode-hook 'clgc-text-mode-hook)
(add-hook 'markdown-mode 'clgc-text-mode-hook)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-hook 'web-mode-hook #'(lambda () (yas-activate-extra-mode 'html-mode)))

(setq css-indent-offset 2)

;; SQLi Mode
(defun clgc-sql-interactive-hook () (toggle-truncate-lines t))
(add-hook 'sql-interactive-mode-hook 'clgc-sql-interactive-hook)

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

(add-hook 'occur-mode-hook 'occur-context-resize-mode)

;; Stop demanding confirmation to go over 50 characters on first line
(remove-hook 'git-commit-finish-query-functions
             'git-commit-check-style-conventions)

(provide 'clgc-major-modes)
