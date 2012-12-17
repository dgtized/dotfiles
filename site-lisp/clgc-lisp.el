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

  (dolist (mode '(scheme emacs-lisp lisp clojure
                         inferior-lisp slime slime-repl nrepl))
    (add-hook (intern (concat (symbol-name mode) "-mode-hook"))
              'paredit-mode)) ;;; Enhance Lisp Modes

  ; (add-hook 'nrepl-interaction-mode-hook (lambda () (require 'nrepl-ritz)))

  ;; Clojure Specific
  (add-to-list 'load-path "~/code/4clj-el")
  (autoload '4clojure-problem "four-clj" "4Clojure-Mode" t)

  (autoload 'ac-nrepl-setup "ac-nrepl" "AC nRepl Mode" t)
  (add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
  (add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
  (eval-after-load "auto-complete"
    '(add-to-list 'ac-modes 'nrepl-mode))

  (add-hook 'nrepl-interaction-mode-hook
            'nrepl-turn-on-eldoc-mode)

  ;; (defun set-auto-complete-as-completion-at-point-function ()
  ;;   (setq completion-at-point-functions '(auto-complete)))
  ;; (add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

  ;; (add-hook 'nrepl-mode-hook 'set-auto-complete-as-completion-at-point-function)
  ;; (add-hook 'nrepl-interaction-mode-hook 'set-auto-complete-as-completion-at-point-function)

  
  )


(provide 'clgc-lisp)
