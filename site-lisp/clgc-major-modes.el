(defun clgc-prog-mode-hook ()
  (add-to-list 'prettify-symbols-alist '("<=" . ?≤))
  (add-to-list 'prettify-symbols-alist '(">=" . ?≥))
  (add-to-list 'prettify-symbols-alist '("!=" . ?≠))

  (setq show-trailing-whitespace t
        indicate-empty-lines t))

;; Reveals the original string when point is on the symbol
(setq prettify-symbols-unprettify-at-point 'right-edge)
(global-prettify-symbols-mode)

(add-hook 'prog-mode-hook 'clgc-prog-mode-hook)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
;; FIXME: flyspell-mode-map is overriding C-. and C-, bindings
;; (add-hook 'prog-mode-hook 'flyspell-prog-mode)

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(setq compilation-ask-about-save nil
      compilation-read-command t
      compilation-window-height 12)

;; Elm
;; (with-eval-after-load 'flycheck
;;   (add-hook 'flycheck-mode-hook #'flycheck-elm-setup))
(eval-when-compile (require 'elm-mode))
(with-eval-after-load 'elm-mode
  (progn
    ;; (add-to-list 'company-backends 'company-elm)
    (setq elm-indent-offset 4
          elm-format-on-save t)))

;; Standard ML
(with-eval-after-load 'sml-mode
  (progn
    (setq sml-indent-level 2)))

;; Haskell

(defun clgc-haskell-mode-hook ()
  (set (make-local-variable 'company-backends)
       (append '((company-capf company-dabbrev-code))
               company-backends))
  (require 'haskell-interactive-mode)
  (require 'haskell-process)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode))

(add-hook 'haskell-mode-hook 'clgc-haskell-mode-hook)

;; Elixir
(defun my-elixir-do-end-close-action (id action context)
  (when (eq action 'insert)
    (newline-and-indent)
    (forward-line -1)
    (indent-according-to-mode)))

(add-hook 'elixir-mode-hook 'alchemist-mode)
(with-eval-after-load 'alchemist-mode
  (progn
    (setq alchemist-goto-elixir-source-dir (expand-file-name "~/code/elixir/elixir")
          alchemist-goto-erlang-source-dir (expand-file-name "~/code/erlang-otp-src")
          alchemist-hooks-compile-on-save t
          alchemist-hooks-test-on-save t)
    (require 'smartparens)
    (sp-with-modes '(elixir-mode)
      (sp-local-pair "->" "end"
                     :when '(("SPC" "RET" "<evil-ret>"))
                     :post-handlers '(:add my-elixir-do-end-close-action)
                     :actions '(insert)))))

;; auto-format on save
(add-hook 'elixir-mode-hook
          (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))

(with-eval-after-load 'flycheck
  (flycheck-credo-setup)
  (flycheck-dialyxir-setup))

(setq-default prolog-system 'swi
              prolog-program-switches '((swi ("-G128M" "-T128M" "-L128M" "-O"))
                                        (t nil))
              prolog-electric-if-then-else-flag t)

;; Gnuplot
(with-eval-after-load 'gnuplot-mode
  (add-hook 'gnuplot-mode-hook
            (lambda ()
              (flyspell-prog-mode)
              (add-hook 'before-save-hook
                        'whitespace-cleanup nil t))))

;;; make Groovy mode electric by default.
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))
(add-to-list 'auto-mode-alist '("\\Jenkinsfile" . groovy-mode))

;; Perl Stuff (for the horrible times when I can't use ruby)
(defalias 'perl-mode 'cperl-mode)
(defun my-cperl-mode-hook ()
  (setq cperl-hairy t)
  (setq cperl-electric-keywords nil)
  ;; (keymap-set cperl-mode-map "C-c p" 'cperl-perldoc)
  ;; (make-variable-buffer-local 'compile-command)
  ;; (setq compile-command (concat "perl -w " (buffer-file-name) " "))
  )

(add-hook 'cperl-mode-hook 'my-cperl-mode-hook)

(require 'rust-mode)
(setq rust-format-on-save t)
(add-hook 'rust-mode-hook
          (lambda () (prettify-symbols-mode)))

;;
;; set c/c++ indent width and compile modes
;;
(defun my-c-mode-hook ()
  (c-set-style "gnu"))
(add-hook 'c-mode-hook 'my-c-mode-hook)

(defun clgc-text-mode-hook ()
  (flyspell-mode)
  (visual-line-mode))
(add-hook 'text-mode-hook 'clgc-text-mode-hook)
(add-hook 'markdown-mode 'clgc-text-mode-hook)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.eex\\'" . web-mode))
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

(add-to-list 'magic-mode-alist '("^>\\|ID\\|LOCUS\\|DNA" . dna-mode))
(add-to-list
 'auto-mode-alist
 '("\\.\\(fasta\\|fa\\|exp\\|ace\\|gb\\)\\'" . dna-mode))
(add-hook 'dna-mode-hook 'turn-on-font-lock)

(defun auctex ()
  (interactive)
  (load "auctex.el" nil t t)
  (load "preview.el" nil t t)
  (setq-default TeX-master nil)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t))

;; Github pages refuses to serve .vert or .frag as anything but
;; application/octet-stream, which breaks fetch when loading shaders from a url.
;; As a hack, just add the .c suffix so it has mime-type for text/x-c which
;; fetch can read, and then remap the automode selection for Emacs to match.
(add-to-list 'auto-mode-alist '("\\.\\(vert\\|frag\\).c\\'" . glsl-mode))

(provide 'clgc-major-modes)
