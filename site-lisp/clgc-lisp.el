

(progn
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook 'esk-remove-elc-on-save)

  (defun esk-remove-elc-on-save ()
    "If you're saving an elisp file, likely the .elc is no longer valid."
    (make-local-variable 'after-save-hook)
    (add-hook 'after-save-hook
              (lambda ()
                (if (file-exists-p (concat buffer-file-name "c"))
                    (delete-file (concat buffer-file-name "c"))))))

  (define-key emacs-lisp-mode-map (kbd "M-.") 'find-function-at-point)
  (define-key emacs-lisp-mode-map (kbd "C-c v") 'eval-buffer)

;;; Enhance Lisp Modes

  (define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)
  (define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)

  (eval-after-load 'paredit
    ;; need a binding that works in the terminal
    '(define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp))

  (dolist (mode '(scheme emacs-lisp lisp clojure))
    (add-hook (intern (concat (symbol-name mode) "-mode-hook")) (lambda () (paredit-mode t))))

  (eval-after-load 'clojure-mode
    '(font-lock-add-keywords
      'clojure-mode `(("(\\(fn\\>\\)"
                       (0 (progn (compose-region (match-beginning 1)
                                                 (match-end 1) "ƒ")
                                 nil))))))
  (eval-after-load 'clojure-mode
    (add-hook 'slime-repl-mode-hook 'clojure-mode-font-lock-setup)))

(provide 'clgc-lisp)
