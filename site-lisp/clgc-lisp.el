(progn
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

  ;; Enhance Lisp Modes
  (dolist (mode '(scheme emacs-lisp inferior-emacs-lisp lisp clojure
                         inferior-lisp slime slime-repl cider-repl))
    (let ((mode-hook (intern (concat (symbol-name mode) "-mode-hook"))))
      (progn
        (add-hook mode-hook 'smartparens-strict-mode))))

  (defadvice cider-eval-last-sexp (after cider-flash-last activate)
    (flash-region (save-excursion (backward-sexp) (point)) (point)))
  (defadvice cider-eval-defun-at-point (after cider-flash-at activate)
    (apply #'flash-region (cider--region-for-defun-at-point)))

  ;; Clojure Specific
  (defun clgc-clojure-mode-hook ()
    (subword-mode +1)
    (require 'clj-refactor)
    (clj-refactor-mode t)
    (cljr-add-keybindings-with-prefix "C-c r"))

  (defun clgc-cider-repl-mode-hook ()
    (subword-mode +1)
    (eldoc-mode))

  (defun clgc-cider-mode-hook ()
    (eldoc-mode))

  (setq cider-show-error-buffer nil)

  (add-hook 'clojure-mode-hook 'clgc-clojure-mode-hook)
  (add-hook 'cider-repl-mode-hook 'clgc-cider-repl-mode-hook)
  (add-hook 'cider-mode-hook 'clgc-cider-mode-hook)
  ;;(eval-after-load 'clojure-mode '(flycheck-clojure-setup))
  )

(provide 'clgc-lisp)
