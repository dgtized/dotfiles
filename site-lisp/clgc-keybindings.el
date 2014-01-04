;;; clgc-keybindings -- keybindings for clgc
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1)))

(global-set-key (kbd "C-x E") 'apply-macro-to-region-lines)

;;so now Control-c 7 prompts for a Unicode hex code, will then insert the glyph
(global-set-key (kbd "C-c 7") 'insert-char)

(global-unset-key (kbd "C-z")) ;; use C-x C-z instead

(global-unset-key (kbd "M-x")) ;; use one below
(global-set-key (kbd "C-c M-x") 'execute-extended-command)
(global-set-key (kbd "C-x C-m") 'smex)
(global-set-key (kbd "C-c C-m") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-/") 'hippie-expand)

(global-set-key (kbd "C-x \\") 'align-regexp)
(global-set-key (kbd "C-c k") 'kill-whole-line)
(global-set-key (kbd "C-c j") 'join-line)
(global-set-key (kbd "C-c w") 'whitespace-cleanup)
(global-set-key (kbd "C-c q") 'comment-dwim)

(global-set-key (kbd "C-c R") 'rename-file-and-buffer)

(global-set-key (kbd "C-c G") 'github-browse)
(global-set-key (kbd "C-x g") 'magit-status)

(define-key goto-map (kbd "t") 'visit-term-buffer)
(define-key goto-map (kbd "e") 'eshell)
(define-key goto-map (kbd "f") 'helm-find-files)
(define-key goto-map (kbd "b") 'magit-blame-mode)
(define-key goto-map (kbd "l") 'magit-file-log)
(define-key goto-map (kbd "j") 'ace-jump-mode)
(define-key goto-map (kbd "M-j") 'ace-jump-mode)
(define-key goto-map (kbd "i") 'imenu)
(define-key goto-map (kbd ".") 'find-file-in-project)
(define-key goto-map (kbd ",") 'find-grep-in-project)

(let ((map smartparens-mode-map))
  (define-key map (kbd "C-(") 'sp-backward-slurp-sexp)
  (define-key map (kbd "C-)") 'sp-forward-slurp-sexp)
  (define-key map (kbd "C-{") 'sp-backward-barf-sexp)
  (define-key map (kbd "C-}") 'sp-forward-barf-sexp)
  (define-key map (kbd "C-M-t") 'sp-transpose-sexp)
  (define-key map (kbd "C-M-j") 'sp-split-sexp)
  (define-key map (kbd "M-J") 'sp-join-sexp)
  (define-key map (kbd "M-?") 'sp-convolute-sexp))

(global-set-key (kbd "<f9>") 'org-tree-slide-mode)
(global-set-key (kbd "M-<f9>") 'org-tree-slide-mode)

(global-set-key (kbd "C-=") 'er/expand-region)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(define-key ctl-x-4-map (kbd "t") 'transpose-windows)
(define-key ctl-x-4-map (kbd "e") 'ediff-other-window)
(define-key ctl-x-4-map (kbd "s") 'isearch-other-window)

(windmove-default-keybindings)

;; isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
           (regexp-quote isearch-string))))))

(provide 'clgc-keybindings)
