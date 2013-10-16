(progn
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

  (eval-after-load 'paredit
    ;; need a binding that works in the terminal
    '(define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp))

  ;; Enhance Lisp Modes
  (dolist (mode '(scheme emacs-lisp lisp clojure
                         inferior-lisp slime slime-repl nrepl-repl))
    (let ((mode-hook (intern (concat (symbol-name mode) "-mode-hook"))))
      (progn
        (add-hook mode-hook 'paredit-mode)
        (add-hook mode-hook 'rainbow-delimiters-mode))))

  ;; Clojure Specific
  ;; (add-hook 'nrepl-interaction-mode-hook (lambda () (require 'nrepl-ritz)))
  (add-hook 'nrepl-repl-mode-hook 'subword-mode)

  (autoload 'ac-nrepl-setup "ac-nrepl" "AC nRepl Mode" t)
  (add-hook 'nrepl-repl-mode-hook 'ac-nrepl-setup)
  (add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
  (eval-after-load "auto-complete"
    '(add-to-list 'ac-modes 'nrepl-repl-mode))

  (add-hook 'nrepl-interaction-mode-hook
            'nrepl-turn-on-eldoc-mode)

  (eval-after-load "nrepl"
    '(when (require 'nrepl-inspect nil 'noerror)
       (define-key nrepl-repl-mode-map (kbd "C-c C-i") 'nrepl-inspect)
       (define-key nrepl-interaction-mode-map (kbd "C-c C-i") 'nrepl-inspect))))

(provide 'clgc-lisp)
