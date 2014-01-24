(defun clgc-prog-mode-hook ()
  (setq show-trailing-whitespace t
        indicate-empty-lines t))

(add-hook 'prog-mode-hook 'clgc-prog-mode-hook)

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

(defun my-text-mode-hook ()
  (flyspell-mode)
  (auto-fill-mode))
(add-hook 'text-mode 'my-text-mode-hook)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

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

(provide 'clgc-major-modes)
