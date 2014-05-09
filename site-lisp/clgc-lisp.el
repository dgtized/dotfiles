(progn
  (require 'cider) ;; until cider autoloads are stable
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook 'auto-recompile-el-buffer)

  (defun auto-recompile-el-buffer ()
    "Recompile elisp buffers on save if it has been compiled before"
    (make-local-variable 'after-save-hook)
    (add-hook 'after-save-hook
              (lambda ()
                (when (file-exists-p (byte-compile-dest-file buffer-file-name))
                  (byte-compile-file buffer-file-name)))))

  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook 'turn-on-elisp-slime-nav-mode))

  ;; Enhance Lisp Modes
  (dolist (mode '(scheme emacs-lisp inferior-emacs-lisp lisp clojure
                         inferior-lisp slime slime-repl cider-repl))
    (let ((mode-hook (intern (concat (symbol-name mode) "-mode-hook"))))
      (progn
        (add-hook mode-hook 'rainbow-delimiters-mode)
        (add-hook mode-hook 'smartparens-strict-mode))))

  ;; Clojure Specific
  ;; (add-hook 'cider-interaction-mode-hook (lambda () (require 'nrepl-ritz)))
  (defun clgc-clojure-mode-hook ()
    (subword-mode +1)
    (clojure-test-mode +1)
    (require 'clj-refactor)
    (clj-refactor-mode t)
    (cljr-add-keybindings-with-prefix "C-c r"))

  (defun clgc-cider-repl-mode-hook ()
    (subword-mode +1)
    (cider-turn-on-eldoc-mode))

  (defun clgc-cider-mode-hook ()
    (cider-turn-on-eldoc-mode))

  (setq cider-popup-stacktraces nil)

  (define-key clojure-mode-map (kbd "C-c h") 'clojure-cheatsheet)

  (add-hook 'clojure-mode-hook 'clgc-clojure-mode-hook)
  (add-hook 'cider-repl-mode-hook 'clgc-cider-repl-mode-hook)
  (add-hook 'cider-mode-hook 'clgc-cider-mode-hook)

  (eval-after-load 'company '(add-to-list 'company-backends 'company-cider))

  (eval-after-load "cider"
    '(when (require 'nrepl-inspect nil 'noerror)
       (define-key cider-repl-mode-map (kbd "C-c C-i") 'nrepl-inspect)
       (define-key cider-interaction-mode-map (kbd "C-c C-i") 'nrepl-inspect))))

(provide 'clgc-lisp)
