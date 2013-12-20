(progn
  (require 'cider) ;; until cider autoloads are stable
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook 'auto-recompile-el-buffer)

  (defun auto-recompile-el-buffer ()
    "Recompile elisp buffers on save if it has been compiled before"
    (make-local-variable 'after-save-hook)
    (add-hook 'after-save-hook
              (lambda ()
                (when (file-exists-p (byte-compile-dest-file buffer-file-name))
                  (byte-compile-file buffer-file-name)))))

  (define-key emacs-lisp-mode-map (kbd "M-.") 'find-function-at-point)
  (define-key emacs-lisp-mode-map (kbd "C-c v") 'eval-buffer)

  (define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)
  (define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)

  ;; Enhance Lisp Modes
  (dolist (mode '(scheme emacs-lisp lisp clojure
                         inferior-lisp slime slime-repl cider-repl))
    (let ((mode-hook (intern (concat (symbol-name mode) "-mode-hook"))))
      (progn
        (add-hook mode-hook 'rainbow-delimiters-mode))))

  ;; Clojure Specific
  ;; (add-hook 'cider-interaction-mode-hook (lambda () (require 'nrepl-ritz)))
  (defun clgc-clojure-mode-hook ()
    (subword-mode +1)
    (clojure-test-mode +1))

  (defun clgc-cider-repl-mode-hook ()
    (subword-mode +1)
    (ac-nrepl-setup)
    (ensure-yasnippet-is-first-ac-source)
    (cider-turn-on-eldoc-mode))

  (defun clgc-cider-mode-hook ()
    (ac-nrepl-setup)
    (ensure-yasnippet-is-first-ac-source)
    (cider-turn-on-eldoc-mode))

  (setq cider-popup-stacktraces nil)

  (define-key cider-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc)
  (define-key clojure-mode-map (kbd "C-c C-g") 'clojure-cheatsheet)

  (add-hook 'clojure-mode-hook 'clgc-clojure-mode-hook)
  (add-hook 'cider-repl-mode-hook 'clgc-cider-repl-mode-hook)
  (add-hook 'cider-mode-hook 'clgc-cider-mode-hook)

  (autoload 'ac-nrepl-setup "ac-nrepl" "AC nRepl Mode" t)
  (eval-after-load "auto-complete"
    '(add-to-list 'ac-modes 'cider-repl-mode))

  (eval-after-load "cider"
    '(when (require 'nrepl-inspect nil 'noerror)
       (define-key cider-repl-mode-map (kbd "C-c C-i") 'nrepl-inspect)
       (define-key cider-interaction-mode-map (kbd "C-c C-i") 'nrepl-inspect))))

(provide 'clgc-lisp)
